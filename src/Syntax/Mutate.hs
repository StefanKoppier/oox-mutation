module Syntax.Mutate where

import           Polysemy
import           Polysemy.State
import           System.Random
import           Data.Maybe
import qualified Data.Map       as M
import           Control.Lens hiding (withIndex)
import           Syntax.Syntax
import           Syntax.DSL

withIndex :: [a] -> [(a, Int)]
withIndex xs = zip xs [0..]

concatM :: Sem r [[a]] -> Sem r [a]
concatM xs = concat <$> xs

--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

type Environment = M.Map Identifier' NonVoidType'

emptyEnvironment :: Environment
emptyEnvironment = M.empty

local :: Members [Embed IO, State Environment] r
    => Sem r a -> Sem r a
local f = do
    state <- get
    result <- f
    put state
    return result
    
--------------------------------------------------------------------------------
-- Mutation Operator
--------------------------------------------------------------------------------

data MutationOperator r
    = StatementMutation  (Members [Embed IO, State Environment] r => Statement'  -> Sem r [Statement'])
    | BinOpMutation      (Members [Embed IO, State Environment] r => BinOp       -> Sem r [BinOp])
    | UnOpMutation       (Members [Embed IO, State Environment] r => UnOp        -> Sem r (Maybe [UnOp]))
    | LitMutation        (Members [Embed IO, State Environment] r => Lit'        -> Sem r [Lit'])
    | VarMutation        (Members [Embed IO, State Environment] r => Identifier' -> Sem r [Identifier'])

applyStat :: Members [Embed IO, State Environment] r => 
    MutationOperator r -> Statement' -> Sem r [Statement']
applyStat (StatementMutation f) = f
applyStat _                     = return . const []

applyBinOp :: Members [Embed IO, State Environment] r => 
    MutationOperator r -> BinOp -> Sem r [BinOp]
applyBinOp (BinOpMutation f) = f
applyBinOp _                 = return . const []

applyUnOp :: Members [Embed IO, State Environment] r => 
    MutationOperator r -> UnOp -> Sem r (Maybe [UnOp])
applyUnOp (UnOpMutation f) = f
applyUnOp _                = return . const (Just [])

applyLit :: Members [Embed IO, State Environment] r => 
    MutationOperator r -> Lit' -> Sem r [Lit']
applyLit (LitMutation f) = f
applyLit _               = return . const []

applyVar :: Members [Embed IO, State Environment] r => 
    MutationOperator r -> Identifier' -> Sem r [Identifier']
applyVar (VarMutation f) = f
applyVar _               = return . const []

operators :: Members [Embed IO, State Environment] r => [(String, MutationOperator r)]
operators = 
    [ ("DEL"
        , StatementMutation (\ stat -> return [skip | deletableStat stat]))

    , ("FLOW"
        , StatementMutation (\case (Continue label info) -> return [Break label info]
                                   (Break label info)    -> return [Continue label info]
                                   _                     -> return []))

    , ("FORK"
        , StatementMutation (\case (Fork invocation label info) -> return [Call invocation label info]
                                   _                            -> return []))

    , ("LOCK"
        , StatementMutation (\case (Lock originalVar body label info) -> do
                                        state <- get
                                        let options  = map fst . filter (\ (var, ty) -> originalVar /= var && case ty of 
                                                                                            ReferenceType{} -> True
                                                                                            ArrayType{} -> True 
                                                                                            _ -> False) . M.toList $ state
                                        return $ map (\ option -> Lock option body label info) options
                                               ++ [Block body label info]
                                   _ -> return []))

    , ("VAR"
        , VarMutation (\ originalVar -> do
            state <- get
            let varTy = state M.! originalVar
            let options = map fst . filter (\ (var, ty) -> varTy `sameType` ty && var /= originalVar) . M.toList $ state
            return options))

    , ("LIT"
        , LitMutation (\case (IntLit value info)  -> return [ IntLit (value - 1) info
                                                            , IntLit (value + 1) info ]
                             (BoolLit value info) -> return [ BoolLit (not value) info ]
                             _                    -> return []))

    , ("EQ"
        , BinOpMutation (\case Equal    -> return [NotEqual]
                               NotEqual -> return [Equal]
                               _        -> return []))

    , ("CMP"
        , BinOpMutation (\case LessThan         -> return [LessThanEqual, GreaterThan, GreaterThanEqual]
                               LessThanEqual    -> return [LessThan, GreaterThan, GreaterThanEqual]
                               GreaterThan      -> return [LessThan, LessThanEqual, GreaterThanEqual]
                               GreaterThanEqual -> return [LessThan, LessThanEqual, GreaterThan]
                               _                -> return []))
    
    , ("ARITH"
        , BinOpMutation (\case Plus     -> return [Minus, Multiply, Divide, Modulo]
                               Minus    -> return [Plus, Multiply, Divide, Modulo]
                               Multiply -> return [Plus, Minus, Divide, Modulo]
                               Divide   -> return [Plus, Minus, Multiply, Modulo]
                               Modulo   -> return [Plus, Minus, Multiply, Divide]
                               _        -> return []))
    
    , ("BOOL"
        , BinOpMutation (\case And     -> return [Or, Implies]
                               Or      -> return [And, Implies]
                               Implies -> return [And, Or]
                               _       -> return []))

    , ("UN"
        , UnOpMutation (\ op -> return Nothing))
    ]

--------------------------------------------------------------------------------
-- Mutation Operator Application
--------------------------------------------------------------------------------

mutateCompilationUnit :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> CompilationUnit'-> Sem r [CompilationUnit']
mutateCompilationUnit mutation (CompilationUnit declarations info) = do
    declarations' <- mutateDeclarations mutation declarations
    return $ map (\ declarations'' -> CompilationUnit declarations'' info) declarations'

mutateDeclarations :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> [Declaration'] -> Sem r [[Declaration']]
mutateDeclarations mutation declarations =
    concatM <$> mapM (\ (declaration, index) -> do
        declaration' <- mutateDeclaration mutation declaration
        return $ map (\ declaration'' -> declarations & element index .~ declaration'') declaration'
        ) $ withIndex declarations

mutateDeclaration :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> Declaration' -> Sem r [Declaration']
mutateDeclaration mutation (Class name members info) = do
    members' <- mutateMembers mutation members
    return $ map (\ members'' -> Class name members'' info) members'

mutateMembers :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> [Member'] -> Sem r [[Member']]
mutateMembers mutation members =
    concatM <$> mapM (\ (member, index) -> do
        member' <- mutateMember mutation member
        return $ map (\ member'' -> members & element index .~ member'') member'
        ) $ withIndex members

mutateMember :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> Member' -> Sem r [Member']
mutateMember mutation (Constructor name params specification body labels info) = do
    body' <- local (do
        mapM_ (\ (Parameter ty name _) -> modify (M.insert name ty)) params
        mutateStatement mutation body)
    return $ map (\ body'' -> Constructor name params specification body'' labels info) body'
mutateMember mutation (Method isStatic returnTy name params specification body labels info) = do
    body' <- local (do
        mapM_ (\ (Parameter ty name _) -> modify (M.insert name ty)) params
        mutateStatement mutation body)
    return $ map (\ body'' -> Method isStatic returnTy name params specification body'' labels info) body'
mutateMember _ field = return [field]

mutateStatement :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> Statement' -> Sem r [Statement']
mutateStatement mutation stat@(Declare ty var label info) = do
    modify (M.insert var ty)
    applyStat mutation stat
mutateStatement mutation stat@(Assign lhs rhs label info) = do
    lhs'  <- mutateLhs mutation lhs
    rhs'  <- mutateRhs mutation rhs
    stat' <- applyStat mutation stat
    return $ stat' 
           ++ map (\ lhs'' -> Assign lhs'' rhs label info) lhs'
           ++ map (\ rhs'' -> Assign lhs rhs'' label info) rhs'
mutateStatement mutation stat@(Call invocation label info) = do
    invocation' <- mutateInvocation mutation invocation
    stat'       <- applyStat mutation stat
    return $ stat'
           ++ map (\ invocation'' -> Call invocation'' label info) invocation'
mutateStatement mutation stat@(Skip label info) = 
    applyStat mutation stat
mutateStatement mutation stat@(Assert assertion label info) =
    applyStat mutation stat
mutateStatement mutation stat@(Assume assumption label info) =
    applyStat mutation stat
mutateStatement mutation stat@(While guard body label info) = do
    body'  <- mutateStatement mutation body
    guard' <- mutateExpression mutation guard
    stat'  <- applyStat mutation stat
    return $ stat' 
           ++ map (\ body'' -> While guard body'' label info) body'
           ++ map (\ guard'' -> While guard'' body label info) guard'
mutateStatement mutation stat@(Ite guard trueBody falseBody label info) = do
    trueBody'  <- mutateStatement mutation trueBody
    falseBody' <- mutateStatement mutation falseBody
    guard'     <- mutateExpression mutation guard
    stat'      <- applyStat mutation stat
    return $ stat' 
           ++ map (\ trueBody''  -> Ite guard trueBody'' falseBody label info) trueBody'
           ++ map (\ falseBody'' -> Ite guard trueBody falseBody'' label info) falseBody'
           ++ map (\ guard''     -> Ite guard'' trueBody falseBody label info) guard'
mutateStatement mutation stat@(Continue label info) =
    applyStat mutation stat
mutateStatement mutation stat@(Break label info) = 
    applyStat mutation stat
mutateStatement mutation stat@(Return expression label info) = do
    expression' <- mutateMExpression mutation expression
    stat'       <- applyStat mutation stat
    return $ stat'
           ++ map (\ expression'' -> Return expression'' label info) expression'
mutateStatement mutation stat@(Throw message label info) =
    applyStat mutation stat
mutateStatement mutation stat@(Try tryBody catchBody label label2 label3 label4 info) = do
    tryBody'   <- mutateStatement mutation tryBody
    catchBody' <- mutateStatement mutation catchBody
    stat'      <- mutateStatement mutation stat
    return $ stat' 
           ++ map (\ tryBody''   -> Try tryBody'' catchBody label label2 label3 label4 info) tryBody'
           ++ map (\ catchBody'' -> Try tryBody catchBody'' label label2 label3 label4 info) catchBody'
mutateStatement mutation (Block body label info) = do
    body' <- local (mutateStatement mutation body)
    return $ map (\ body'' -> Block body'' label info) body'
mutateStatement mutation stat@(Lock var body label info) = do
    stat' <- applyStat mutation stat
    var'  <- applyVar mutation var
    body' <- mutateStatement mutation body
    return $ stat'
           ++ map (\ var''  -> Lock var'' body label info) var'
           ++ map (\ body'' -> Lock var body'' label info) body'
mutateStatement mutation stat@(Fork invocation label info) = do
    invocation' <- mutateInvocation mutation invocation
    stat' <- applyStat mutation stat
    return $ stat'
           ++ map (\ invocation'' -> Fork invocation'' label info) invocation'
mutateStatement mutation stat@(Join label info) =
    applyStat mutation stat
mutateStatement mutation (Seq stat1 stat2 label info) = do
    stat1' <- mutateStatement mutation stat1
    stat2' <- mutateStatement mutation stat2
    return $ map (\ stat1'' -> Seq stat1'' stat2 label info) stat1' ++ map (\ stat2'' -> Seq stat1 stat2'' label info) stat2'

mutateInvocation :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> Invocation' -> Sem r [Invocation']
mutateInvocation mutation (InvokeMethod lhs rhs arguments resolved label info) = do
    arguments' <- mutateExpressions mutation arguments
    return $ map (\ arguments'' -> InvokeMethod lhs rhs arguments'' resolved label info) arguments'
mutateInvocation mutation (InvokeConstructor className arguments resolved label info) = do
    arguments' <- mutateExpressions mutation arguments
    return $ map (\ arguments'' -> InvokeConstructor className arguments'' resolved label info) arguments'

mutateLhs :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> Lhs' -> Sem r [Lhs']
mutateLhs mutation (LhsVar var ty info) = do
    var' <- applyVar mutation var
    return $ map (\ var'' -> LhsVar var'' ty info) var'
mutateLhs mutation (LhsField var varTy field ty info) = do
    var' <- applyVar mutation var
    return $ map (\ var'' -> LhsField var'' varTy field ty info) var'
mutateLhs mutation (LhsElem var index ty info) = do
    var'   <- applyVar mutation var
    index' <- mutateExpression mutation index
    return $ map (\ var'' -> LhsElem var'' index ty info) var'
           ++ map (\ index'' -> LhsElem var index'' ty info) index'

mutateRhs :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> Rhs' -> Sem r [Rhs']
mutateRhs mutation (RhsExpression value ty info) = do
    value' <- mutateExpression mutation value
    return $ map (\ value'' -> RhsExpression value'' ty info) value'
mutateRhs mutation (RhsField var field ty info) = do
    var' <- mutateExpression mutation var
    return $ map (\ var'' -> RhsField var'' field ty info) var'
mutateRhs mutation (RhsElem var index ty info) = do
    var'   <- mutateExpression mutation var
    index' <- mutateExpression mutation index
    return $ map (\ var'' -> RhsElem var'' index ty info) var'
           ++ map (\ index'' -> RhsElem var index'' ty info) index'
mutateRhs mutation (RhsCall invocation ty info) = do
    invocation' <- mutateInvocation mutation invocation
    return $ map (\ invocation'' -> RhsCall invocation'' ty info) invocation'
mutateRhs mutation (RhsArray arrayTy sizes ty info) = do
    sizes' <- mutateExpressions mutation sizes
    return $ map (\ sizes'' -> RhsArray arrayTy sizes'' ty info) sizes'

mutateMExpression :: Members [Embed IO, State Environment] r 
    => MutationOperator r -> Maybe Expression' -> Sem r [Maybe Expression']
mutateMExpression _        Nothing  = return []
mutateMExpression mutation (Just e) = map Just <$> mutateExpression mutation e

mutateExpressions :: Members [Embed IO, State Environment] r
    => MutationOperator r -> [Expression'] -> Sem r [[Expression']]
mutateExpressions mutation expressions = 
    concatM <$> mapM (\ (expression, index) -> do
            expression' <- mutateExpression mutation expression
            return $ map (\ expression'' -> expressions & element index .~ expression'') expression'
            ) $ withIndex expressions

mutateExpression :: Members [Embed IO, State Environment] r
    => MutationOperator r -> Expression' -> Sem r [Expression']
mutateExpression _ Forall{} = return []
mutateExpression _ Exists{} = return []
mutateExpression mutation (BinOp binOp lhs rhs ty info) = do
    binOp' <- applyBinOp mutation binOp
    lhs'   <- mutateExpression mutation lhs
    rhs'   <- mutateExpression mutation rhs
    return $ map (\ binOp'' -> BinOp binOp'' lhs rhs ty info) binOp'
           ++ map (\ lhs'' -> BinOp binOp lhs'' rhs ty info) lhs'
           ++ map (\ rhs'' -> BinOp binOp lhs rhs'' ty info) rhs'
mutateExpression mutation (UnOp unOp value ty info) = do
    unOp'  <- applyUnOp mutation unOp
    value' <- mutateExpression mutation value
    return $ [value | isNothing unOp']
           ++ map (\ unOp'' -> UnOp unOp'' value ty info) [unOp'' | isJust unOp', unOp'' <- fromJust unOp']
           ++ map (\ value'' -> UnOp unOp value'' ty info) value'
mutateExpression mutation (Var var ty info) = do
    var' <- applyVar mutation var
    return $ map (\ var'' -> Var var'' ty info) var'
mutateExpression mutation (Parens value ty info) = do
    value' <- mutateExpression mutation value
    return $ map (\ value'' -> Parens value'' ty info) value'
mutateExpression mutation (Lit lit ty info) = do
    lit' <- applyLit mutation lit
    return $ map (\ lit'' -> Lit lit'' ty info) lit'
mutateExpression mutation (SizeOf var ty info) = do
    var' <- applyVar mutation var
    return $ map (\ var'' -> SizeOf var'' ty info) var'

sameType :: NonVoidType' -> NonVoidType' -> Bool
sameType UIntType{}      UIntType{}      = True
sameType IntType{}       IntType{}       = True
sameType FloatType{}     FloatType{}     = True
sameType BoolType{}      BoolType{}      = True
sameType StringType{}    StringType{}    = True
sameType CharType{}      CharType{}      = True
sameType ReferenceType{} ReferenceType{} = True -- ty
sameType ArrayType{}     ArrayType{}     = True -- inner ty
sameType _               _               = False

deletableStat :: Statement' -> Bool
deletableStat Declare{}  = False
deletableStat Assign{}   = True 
deletableStat Call{}     = True 
deletableStat Skip{}     = False 
deletableStat Assert{}   = False 
deletableStat Assume{}   = False 
deletableStat While{}    = False 
deletableStat Ite{}      = False 
deletableStat Continue{} = True 
deletableStat Break{}    = True 
deletableStat Return{}   = False
deletableStat Throw{}    = True
deletableStat Try{}      = False
deletableStat Block{}    = False
deletableStat Lock{}     = False
deletableStat Fork{}     = True
deletableStat Join{}     = True
deletableStat Seq{}      = False
