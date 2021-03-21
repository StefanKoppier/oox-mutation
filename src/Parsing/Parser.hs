module Parsing.Parser where

import           Control.Monad     (when)
import           Text.Parsec       hiding (Empty)
import           Text.Parsec.Error
import           Data.Maybe
import           Text.Pretty
import           Parsing.Lexer
import           Syntax.Syntax
import           Data.Positioned

type P = Parsec [Positioned Token] ()

unknownLabel :: Label
unknownLabel = (-1)

parser :: FilePath -> [Positioned Token] -> CompilationUnit'
parser fileName tokens =
    case parse pCompilationUnit fileName tokens of
        Left err     -> error "failed to parse"
        Right result -> result

getErrorMessage :: ParseError -> String
getErrorMessage = unwords . filter (not . null) . map messageString . errorMessages

pCompilationUnit :: P CompilationUnit'
pCompilationUnit = do
    pos <- getPosition
    declarations <- many pClassDeclaration <* eof
    return $ CompilationUnit declarations pos

--------------------------------------------------------------------------------
-- Class parsing
--------------------------------------------------------------------------------

pClassDeclaration :: P Declaration'
pClassDeclaration = do
    pos <- getPosition
    pToken TClass
    name <- pIdentifier
    members <- pBetweenCurly (many (pMember name))
    return $ Class name members pos
    
pMember :: Identifier' -> P Member'
pMember className = choice [ try (pMethod className) <?> "a method"
                           , try (pConstructor className) <?> "a constructor"
                           , pField <?> "a field" ]

pConstructor :: Identifier' -> P Member'
pConstructor className = do
    pos <- getPosition
    name <- pIdentifier
    when (name /= className)
        (parserFail "Constructor identifier does not match class identifier")
    params <- pBetweenParens pParameters
    specification <- pSpecification
    body <- pBetweenCurly pStatements
    let body' = Seq body (Return Nothing unknownLabel unknownPos) unknownLabel unknownPos
    return $ Constructor name params specification body' (unknownLabel, unknownLabel) pos

pMethod :: Identifier' -> P Member'
pMethod className = do
    pos <- getPosition
    isStatic <- option False (True <$ pToken TStatic)
    ty <- pType
    name <- pIdentifier
    when (name == className)
        (parserFail "Method name can not match class name")
    params <- pBetweenParens pParameters
    specification <- pSpecification
    body <- pBetweenCurly pStatements
    return $ Method isStatic ty name params specification body (unknownLabel, unknownLabel) pos

pField :: P Member'
pField = do
    pos <- getPosition
    ty <- pNonVoidType
    name <- pIdentifier
    pSemicolon
    return $ Field ty name pos

pParameters :: P [Parameter']
pParameters = pParameter `sepBy` pComma

pParameter :: P Parameter'
pParameter = do
    pos  <- getPosition
    ty   <- pNonVoidType
    name <- pIdentifier
    return $ Parameter ty name pos

pSpecification :: P Specification'
pSpecification = do
    pos <- getPosition
    requires    <- optionMaybe (pToken TRequires    *> pBetweenParens pVerificationExpression)
    ensures     <- optionMaybe (pToken TEnsures     *> pBetweenParens pVerificationExpression)
    exceptional <- optionMaybe (pToken TExceptional *> pBetweenParens pVerificationExpression)
    return $ Specification requires ensures exceptional pos

--------------------------------------------------------------------------------
-- Statement parsing
--------------------------------------------------------------------------------

pStatements :: P Statement'
pStatements = do
    stat1 <- pStatement
    pos   <- getPosition
    stat2 <- optionMaybe pStatements
    return $ case stat2 of
        Just stat2' -> 
            Seq stat1 stat2' unknownLabel pos
        Nothing -> 
            case stat1 of --TODO: this can probably be removed
                Assign{_rhs = RhsCall{}} -> 
                    Seq stat1 (Skip unknownLabel pos) unknownLabel pos
                Seq{_stat2 = Assign{_rhs = RhsCall{}}} ->
                    Seq stat1 (Skip unknownLabel pos) unknownLabel pos
                _ -> 
                    stat1

pStatement :: P Statement'
pStatement = choice [ 
      try pDeclare, try pAssign, pCall
    , pSkip       , pAssert    , pAssume
    , pWhile      , pIte       , pContinue
    , pBreak      , pReturn    , pThrow
    , pTry        , pBlock     , pLock
    , pJoin       , pFork ] <?> "a statement"

pDeclare :: P Statement'
pDeclare = do
    pos <- getPosition
    ty <- pNonVoidType
    name <- pIdentifier
    rhs <- optionMaybe (pToken TAssign *> pRhs)
    pSemicolon
    let declaration = Declare ty name unknownLabel pos
    case rhs of
        Just rhs' -> do 
            let assign    = Assign (LhsVar name UnknownRuntimeType pos) rhs' unknownLabel pos
            let statement = Seq declaration assign unknownLabel pos
            return statement
        Nothing   -> return declaration

pAssign :: P Statement'
pAssign = do
    pos <- getPosition
    lhs <- pLhs
    pToken TAssign
    rhs <- pRhs
    pSemicolon
    return $ Assign lhs rhs unknownLabel pos
    
pLhs :: P Lhs'
pLhs = do
    pos <- getPosition
    lhs <- choice [ try $ LhsElem  <$> pIdentifier <*> pBetweenSquare pExpression
                  , try $ LhsField <$> pIdentifier <*> pure UnknownRuntimeType <* pDot <*> pIdentifier
                  ,       LhsVar   <$> pIdentifier] <?> "a left-hand side"
    return $ lhs UnknownRuntimeType pos

pRhs :: P Rhs'
pRhs = do
    pos <- getPosition
    rhs <- choice [ try pRhsCall     , try pRhsField  
                  , try pRhsNewObject, try pRhsArray
                  , try pRhsElem     , pRhsExpression ] <?> "a right-hand side"
    return $ rhs UnknownRuntimeType pos
    where
        pRhsCall       = RhsCall       <$> pInvocation
        pRhsField      = RhsField      <$> pVar <*  pDot <*> pIdentifier
        pRhsNewObject  = (\ pos name args -> RhsCall (InvokeConstructor name args Nothing unknownLabel pos)) <$> getPosition <* pToken TNew <*> pIdentifier  <*> pBetweenParens (pExpression `sepBy` pComma)
        pRhsArray      = RhsArray      <$  pToken TNew <*> pNonArrayType <*> many1 (pBetweenSquare pExpression)
        pRhsElem       = RhsElem       <$> pVar <*> pBetweenSquare pExpression
        pRhsExpression = RhsExpression <$> pExpression

pCall :: P Statement'
pCall =  do
    pos <- getPosition
    invocation <- pInvocation
    return $ Call invocation unknownLabel pos

pSkip :: P Statement'
pSkip = do
    pos <- getPosition
    pSemicolon
    return $ Skip unknownLabel pos

pAssert :: P Statement'
pAssert = do
    pos <- getPosition
    expression <- pToken TAssert *> pVerificationExpression <* pSemicolon
    return $ Assert expression unknownLabel pos

pAssume :: P Statement'
pAssume = do
    pos <- getPosition
    expression <- pToken TAssume *> pVerificationExpression <* pSemicolon
    return $ Assume expression unknownLabel pos

pWhile :: P Statement'
pWhile = do
    pos <- getPosition
    guard <- pToken TWhile *> pBetweenParens pExpression
    body  <- pStatement
    return $ While guard body unknownLabel pos

pIte :: P Statement'
pIte = do
    startPos <- getPosition
    pToken TIf
    guard <- pBetweenParens pExpression
    trueStat <- pStatement
    elsePos <- getPosition
    falseStat <- option (Skip unknownLabel elsePos) (pToken TElse *> pStatement)
    return $ Ite guard trueStat falseStat unknownLabel startPos 

pContinue :: P Statement'
pContinue = do
    pos <- getPosition
    pToken TContinue <* pSemicolon
    return $ Continue unknownLabel pos

pBreak :: P Statement'
pBreak = do
    pos <- getPosition
    pToken TBreak <* pSemicolon
    return $ Break unknownLabel pos

pReturn :: P Statement'
pReturn = do
    pos <- getPosition
    expression <- pToken TReturn *> optionMaybe pExpression <* pSemicolon
    return $ Return expression unknownLabel pos

pThrow :: P Statement'
pThrow = do
    pos <- getPosition
    pToken TThrow
    (StringLit message _) <- pStringLit
    return $ Throw message unknownLabel pos

pTry :: P Statement'
pTry = do
    pos <- getPosition
    tryBody <- pToken TTry *> pBetweenCurly pStatements
    catchBody <- pToken TCatch *> pBetweenCurly pStatements
    return $ Try tryBody catchBody unknownLabel unknownLabel unknownLabel unknownLabel pos

pBlock :: P Statement'
pBlock = do
    pos <- getPosition
    body <- pBetweenCurly pStatements
    return $ Block body unknownLabel pos

pLock :: P Statement'
pLock = do
    lockPos <- getPosition
    pToken TLock
    name <- pBetweenParens pIdentifier
    body <- pBetweenCurly pStatements
    return $ Lock name body unknownLabel lockPos

pJoin :: P Statement'
pJoin = do
    pos <- getPosition
    pToken TJoin <* pSemicolon
    return $ Join unknownLabel pos
    
pFork :: P Statement'
pFork = do
    pos <- getPosition
    invocation <- pToken TFork *> pInvocation <* pSemicolon
    return $ Fork invocation unknownLabel pos

pInvocation :: P Invocation'
pInvocation = do
    pos <- getPosition
    lhs <- pIdentifier
    pDot
    method <- pIdentifier
    arguments <- pBetweenParens (pExpression `sepBy` pComma)
    return $ InvokeMethod lhs method arguments Nothing unknownLabel pos

--------------------------------------------------------------------------------
-- Expression parsing
--------------------------------------------------------------------------------

pExpression :: P Expression'
pExpression = pExpression8 NormalParsing

pVerificationExpression :: P Expression'
pVerificationExpression = pExpression9 VerificationParsing

data ExpressionParsingMode = VerificationParsing | NormalParsing

pExpression9 :: ExpressionParsingMode -> P Expression'
pExpression9 mode = pQuantifier <|> pExpression8 mode
    where
        pQuantifier = do
            pos <- getPosition
            quantifier <- choice [Forall <$ pToken TForall, Exists <$ pToken TExists]
            variable   <- pIdentifier <* pComma
            index      <- pIdentifier <* pColon
            domain     <- pIdentifier <* pColon
            expression <- pExpression9 mode
            return $ quantifier variable index domain expression UnknownRuntimeType pos
    
pExpression8 :: ExpressionParsingMode -> P Expression'
pExpression8 mode = pExpression7 mode `chainl1` pOperators
    where
        pOperators = do
            pos <- getPosition
            (\ e1 e2 -> BinOp Implies e1 e2 UnknownRuntimeType pos) <$ pToken TImplies

pExpression7 :: ExpressionParsingMode -> P Expression'
pExpression7 mode = pExpression6 mode `chainl1` pOperators
    where
        pOperators = do
            pos <- getPosition
            op <- choice [ BinOp And <$ pToken TAnd
                         , BinOp Or  <$ pToken TOr ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression6 :: ExpressionParsingMode -> P Expression'
pExpression6 mode = pExpression5 mode `chainl1` pOperators
    where
        pOperators = do
            pos <- getPosition
            op <- choice [ BinOp Equal    <$ pToken TEqual    
                         , BinOp NotEqual <$ pToken TNotEqual ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression5 :: ExpressionParsingMode -> P Expression'
pExpression5 mode = pExpression4 mode `chainl1` pOperators
    where
        pOperators =  do
            pos <- getPosition
            op <- choice [ BinOp LessThan         <$ pToken TLessThan        
                         , BinOp LessThanEqual    <$ pToken TLessThanEqual   
                         , BinOp GreaterThan      <$ pToken TGreaterThan     
                         , BinOp GreaterThanEqual <$ pToken TGreaterThanEqual ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression4 :: ExpressionParsingMode -> P Expression'
pExpression4 mode = pExpression3 mode `chainl1` pOperators
    where
        pOperators = do
            pos <- getPosition
            op <- choice [ BinOp Plus  <$ pToken TPlus 
                         , BinOp Minus <$ pToken TMinus ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression3 :: ExpressionParsingMode -> P Expression'
pExpression3 mode = pExpression2 mode `chainl1` pOperators
    where
        pOperators =  do
            pos <- getPosition
            op <- choice [ BinOp Multiply <$ pToken TMultiply
                         , BinOp Divide   <$ pToken TDivide   
                         , BinOp Modulo   <$ pToken TModulo ]
            return (\ e1 e2 -> op e1 e2 UnknownRuntimeType pos)

pExpression2 :: ExpressionParsingMode -> P Expression'
pExpression2 mode = pExpression1 mode `chainlUnary1` pOperators
    where
        pOperators = do
            pos <- getPosition
            op  <- choice [ UnOp Negative <$ pToken TMinus
                          , UnOp Negate   <$ pToken TNot ]
            return (\ e -> op e UnknownRuntimeType pos)

pExpression1 :: ExpressionParsingMode -> P Expression'
pExpression1 mode = do
    pos <- getPosition
    choice [ pVar
           , Lit    <$> pLit <*> pure UnknownRuntimeType <*> pure pos
           , SizeOf <$  pToken TSizeOf <*> pIdentifier <*> pure UnknownRuntimeType <*> pure pos
           , Parens <$> pBetweenParens pVerificationExpression <*> pure UnknownRuntimeType <*> pure pos]

pVar :: P Expression'
pVar = do 
    pos <- getPosition
    var <- pIdentifier
    return $ Var var UnknownRuntimeType pos

pLit :: P Lit'
pLit = choice
    [ pBoolLit  , pIntLit , pRealLit
    , pStringLit, pCharLit, pNullLit ] <?> "a literal"

pBoolLit :: P Lit'
pBoolLit = do
    pos <- getPosition
    choice [ BoolLit True  pos <$ pToken TTrue
           , BoolLit False pos <$ pToken TFalse ] <?> "a boolean literal"

pIntLit :: P Lit'
pIntLit = do
    sourceName <- getSourceName
    token showToken (posFromToken sourceName) (matchToken sourceName)
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken name (Positioned pos (TIntLiteral value)) = intLiteral value (setSourceName pos name)
        matchToken _    _                                    = Nothing
        intLiteral value pos = Just $ IntLit value pos
          --  | value >= 0       && value < (2^32) = Just $ UIntLit value
          --  | value >= -(2^31) && value < (2^31) = Just $ IntLit  value
          --  | otherwise                          = Nothing

pRealLit :: P Lit'
pRealLit = do
    sourceName <- getSourceName
    token showToken (posFromToken sourceName) matchToken
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned pos (TRealLiteral value)) = Just $ FloatLit value pos
        matchToken _                                     = Nothing

pStringLit :: P Lit'
pStringLit = do
    sourceName <- getSourceName
    token showToken (posFromToken sourceName) matchToken
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned pos (TStringLiteral value)) = Just $ StringLit value pos
        matchToken _                                       = Nothing

pCharLit :: P Lit'
pCharLit = do
    sourceName <- getSourceName
    token showToken (posFromToken sourceName) matchToken
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned pos (TCharLiteral value)) = Just $ CharLit value pos
        matchToken _                                     = Nothing

pNullLit :: P Lit'
pNullLit = do
    pos <- getPosition
    NullLit <$ pToken TNull <*> pure pos <?> "null"

--------------------------------------------------------------------------------
-- Type parsing
--------------------------------------------------------------------------------

pType :: P Type'
pType = do
    pos <- getPosition    
    ty <- choice [ Nothing <$  pToken TVoid
                 , Just    <$> pNonVoidType ] <?> "a type or void"
    return $ Type ty pos

pNonArrayType :: P NonVoidType'
pNonArrayType = do
    pos <- getPosition
    choice [ UIntType      <$  pToken TUint , IntType    <$ pToken TInt
           , FloatType     <$  pToken TFloat, StringType <$ pToken TString
           , BoolType      <$  pToken TBool , CharType   <$ pToken TChar
           , ReferenceType <$> pIdentifier  ] <*> pure pos <?> "a type"

pNonVoidType :: P NonVoidType'
pNonVoidType = do
    pos <- getPosition
    baseTy <- choice 
        [ UIntType      <$  pToken TUint , IntType    <$ pToken TInt
        , FloatType     <$  pToken TFloat, StringType <$ pToken TString
        , BoolType      <$  pToken TBool , CharType   <$ pToken TChar
        , ReferenceType <$> pIdentifier  ] <?> "a type"
    ty <- pArrayRanks 
    return $ ty (baseTy pos)

pArrayRanks :: P (NonVoidType' -> NonVoidType')
pArrayRanks = do
    pos <- getPosition
    isArrayTy <- optionMaybe (ArrayType <$ pSOpen <* pSClose)
    case isArrayTy of
        Just arrayTy -> do 
            nextArrayTy <- pArrayRanks
            return (\ innerTy -> arrayTy (nextArrayTy innerTy) pos)
        Nothing      -> return id

--------------------------------------------------------------------------------
-- Token parsing
--------------------------------------------------------------------------------

pToken :: Token -> P ()
pToken tok = do
    name <- getSourceName
    token showToken (posFromToken name) matchToken <?> toString tok
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned _ tok') = if tok == tok' then Just () else Nothing
        posFromToken name (Positioned pos _) = setSourceName pos name

pSemicolon :: P ()
pSemicolon = pToken TSemicolon

pColon :: P ()
pColon = pToken TColon

pComma :: P ()
pComma = pToken TComma

pDot :: P ()
pDot = pToken TDot

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

pIdentifier :: P Identifier'
pIdentifier = do
    pos <- getPosition
    name <- getSourceName
    Identifier <$> token showToken (posFromToken name) matchToken <*> pure pos
    where
        showToken (Positioned _ tok') = toString tok'
        matchToken (Positioned _ (TIdentifier s)) = Just s
        matchToken _                              = Nothing

pBetweenCurly :: P a -> P a
pBetweenCurly = between (pToken TCOpen) (pToken TCClose)

pBetweenParens :: P a -> P a
pBetweenParens = between (pToken TPOpen) (pToken TPClose)

pSOpen, pSClose :: P ()
pSOpen  = pToken TSOpen
pSClose = pToken TSClose

pBetweenSquare :: P a -> P a
pBetweenSquare = between pSOpen pSClose

posFromToken :: SourceName -> Positioned a -> SourcePos
posFromToken name (Positioned pos _) = setSourceName pos name

getSourceName :: P SourceName
getSourceName = sourceName <$> getPosition

chainlUnary1 :: P a -> P (a -> a) -> P a
chainlUnary1 p op = do
    fs <- many op
    x  <- p
    return $ foldr id x fs