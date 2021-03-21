{-# LANGUAGE DeriveGeneric #-}

module Syntax.Syntax(
      CompilationUnit', CompilationUnit(..)
    , Declaration', Declaration(..)
    , Member', Member(..)
    , Parameter', Parameter(..)
    , Specification', Specification(..)
    , Statement', Statement(..)
    , Invocation', Invocation(..)
    , Lhs', Lhs(..)
    , Rhs', Rhs(..)
    , Expression', Expression(..)
    , BinOp(..), UnOp(..)
    , Lit', Lit(..)
    , Type', Type(..)
    , NonVoidType', NonVoidType(..)
    , RuntimeType(..), Label
    , Identifier', Identifier(..)
    , Reference
) where

import GHC.Generics         (Generic)
import Data.Positioned
import Data.Hashable

--------------------------------------------------------------------------------
-- Top-level definitions
--------------------------------------------------------------------------------

type CompilationUnit' = CompilationUnit SourcePos
data CompilationUnit a
    = CompilationUnit { _members :: [Declaration a], _info :: a }
    deriving (Show)

type Declaration' = Declaration SourcePos
data Declaration a
    = Class { _name :: Identifier a, _members :: [Member a], _info :: a }
    deriving (Show)

instance Eq Declaration' where
    (Class _ _ posX) == (Class _ _ posY) = posX == posY
    
instance Ord Declaration' where
    (Class _ _ posX) <= (Class _ _ posY) = posX <= posY

type Member' = Member SourcePos
data Member a
    = Constructor { _name          :: Identifier a   , _params :: [Parameter a]
                  , _specification :: Specification a, _body   :: Statement a
                  , _labels        :: (Label, Label) , _info   :: a }
    | Method { _isStatic      :: Bool           , _returnTy :: Type a
             , _name          :: Identifier a   , _params   :: [Parameter a]
             , _specification :: Specification a, _body     :: Statement a
             , _labels        :: (Label, Label) , _info     :: a }
    | Field  { _ty :: NonVoidType a, _name :: Identifier a, _info :: a }
    deriving (Show)

instance Eq Member' where
    a == b = _info (a :: Member') == _info (b :: Member')
        
instance Ord Member' where
    a <= b = _info (a :: Member') <= _info (b :: Member')

instance WithPos Member' where
    getPos = _info

type Parameter' = Parameter SourcePos
data Parameter a
    = Parameter { _ty :: NonVoidType a, _name :: Identifier a, _info :: a }
    deriving (Show)

type Specification' = Specification SourcePos
data Specification a
    = Specification { _requires    :: Maybe (Expression a)
                    , _ensures     :: Maybe (Expression a)
                    , _exceptional :: Maybe (Expression a)
                    , _info    :: a }
    deriving (Show)

--------------------------------------------------------------------------------
-- Statement definitions
--------------------------------------------------------------------------------

type Statement' = Statement SourcePos
data Statement a
    = Declare { _ty :: NonVoidType a, _var :: Identifier a, _label :: Label, _info :: a }
    | Assign { _lhs :: Lhs a, _rhs :: Rhs a, _label :: Label, _info :: a }
    | Call { _invocation :: Invocation a, _label :: Label, _info :: a }
    | Skip { _label :: Label, _info :: a }
    | Assert { _assertion :: Expression a, _label :: Label, _info :: a }
    | Assume { _assumption :: Expression a, _label :: Label, _info :: a }
    | While { _guard :: Expression a, _body :: Statement a, _label :: Label, _info :: a }
    | Ite { _guard :: Expression a, _trueBody :: Statement a, _falseBody :: Statement a, _label :: Label, _info :: a }
    | Continue { _label :: Label, _info :: a }
    | Break { _label :: Label, _info :: a }
    | Return { _expression :: Maybe (Expression a), _label :: Label, _info :: a }
    | Throw { _message :: String, _label :: Label, _info :: a }
    | Try { _tryBody :: Statement a, _catchBody :: Statement a, _label :: Label, _label2 :: Label, _label3 :: Label, _label4 :: Label, _info :: a }
    | Block { _body :: Statement a, _label :: Label, _info :: a }
    | Lock { _var :: Identifier a, _body :: Statement a, _label :: Label, _info :: a }
    | Fork { _invocation :: Invocation a, _label :: Label, _info :: a }
    | Join { _label :: Label, _info :: a }
    | Seq { _stat1 :: Statement a, _stat2 :: Statement a, _label :: Label, _info :: a }
    deriving (Show)

instance Eq (Statement a) where
    a == b = (_label :: Statement a -> Label) a == (_label :: Statement a -> Label) b

instance WithPos Statement' where
    getPos = _info

type Invocation' = Invocation SourcePos
data Invocation a
    = InvokeMethod { _lhs :: Identifier a, _rhs :: Identifier a, _arguments :: [Expression a], _resolved :: Maybe (Declaration a, Member a), _label :: Label, _info :: a }
    | InvokeConstructor { _className :: Identifier a, _arguments :: [Expression a], _resolved :: Maybe (Declaration a, Member a), _label :: Label, _info :: a }
    deriving (Show)
 
instance Eq (Invocation a) where
    a == b = (_label :: Invocation a -> Label) a == (_label :: Invocation a -> Label) b

instance WithPos Invocation' where
    getPos = _info

type Lhs' = Lhs SourcePos
data Lhs a
    = LhsVar   { _var :: Identifier a, _ty :: RuntimeType, _info :: a }
    | LhsField { _var :: Identifier a, _varTy :: RuntimeType, _field :: Identifier a, _ty :: RuntimeType, _info :: a }
    | LhsElem  { _var :: Identifier a, _index :: Expression a, _ty :: RuntimeType, _info :: a }
    deriving (Eq, Show)

type Rhs' = Rhs SourcePos
data Rhs a
    = RhsExpression { _value :: Expression a, _ty :: RuntimeType, _info :: a }
    | RhsField { _var :: Expression a, _field :: Identifier a, _ty :: RuntimeType, _info :: a }
    | RhsElem { _var :: Expression a, _index :: Expression a, _ty :: RuntimeType, _info :: a }
    | RhsCall { _invocation :: Invocation a, _ty :: RuntimeType, _info :: a }
    | RhsArray { _arrayTy :: NonVoidType a, _sizes :: [Expression a], _ty :: RuntimeType, _info :: a }
    deriving (Eq, Show)

instance WithPos Rhs' where
    getPos = _info

--------------------------------------------------------------------------------
-- Expression definitions
--------------------------------------------------------------------------------

type Reference = Int

type Expression' = Expression SourcePos
data Expression a
    = Forall      { _elem :: Identifier a, _range :: Identifier a, _domain :: Identifier a, _formula :: Expression a, _ty :: RuntimeType, _info :: a }
    | Exists      { _elem :: Identifier a, _range :: Identifier a, _domain :: Identifier a, _formula :: Expression a, _ty :: RuntimeType, _info :: a }
    | BinOp       { _binOp :: BinOp, _lhs :: Expression a, _rhs :: Expression a, _ty :: RuntimeType, _info :: a }
    | UnOp        { _unOp :: UnOp, _value :: Expression a, _ty :: RuntimeType, _info :: a }
    | Var         { _var :: Identifier a, _ty :: RuntimeType, _info :: a }
    | SymbolicVar { _var :: Identifier a, _ty :: RuntimeType, _info :: a }
    | Parens      { _value :: Expression a, _ty :: RuntimeType, _info :: a }
    | Lit         { _lit :: Lit a, _ty :: RuntimeType, _info :: a }
    | SizeOf      { _var :: Identifier a, _ty :: RuntimeType, _info :: a }
    | Ref         { _ref :: Reference, _ty :: RuntimeType, _info :: a }
    | SymbolicRef { _var :: Identifier a, _ty :: RuntimeType, _info :: a }
    | IteE        { _guard :: Expression a, _true :: Expression a, _false :: Expression a, _ty :: RuntimeType, _info :: a }
    deriving (Show)

instance Eq (Expression a) where
    (Forall elemA rangeA domainA formulaA _ _) == (Forall elemB rangeB domainB formulaB _ _) 
        = elemA == elemB && rangeA == rangeB && domainA == domainB && formulaA == formulaB
    (Exists elemA rangeA domainA formulaA _ _) == (Forall elemB rangeB domainB formulaB _ _) 
        = elemA == elemB && rangeA == rangeB && domainA == domainB && formulaA == formulaB
    (BinOp opA lhsA rhsA _ _) == (BinOp opB lhsB rhsB _ _)
        = opA == opB && lhsA == lhsB && rhsA == rhsB
    (UnOp opA valueA _ _) == (UnOp opB valueB _ _)
        = opA == opB && valueA == valueB
    (Var varA _ _) == (Var varB _ _)
        = varA == varB
    (SymbolicVar varA _ _) == (SymbolicVar varB _ _)
        = varA == varB
    (Parens valueA _ _) == (Parens valueB _ _)
        = valueA == valueB
    (Lit litA _ _) == (Lit litB _ _)
        = litA == litB    
    (SizeOf varA _ _) == (SizeOf varB _ _)
        = varA == varB
    (Ref refA _ _) == (Ref refB _ _)
        = refA == refB
    (SymbolicRef varA _ _) == (SymbolicRef varB _ _)
        = varA == varB
    (IteE guardA trueA falseA _ _) == (IteE guardB trueB falseB _ _)
        = guardA == guardB && trueA == trueB && falseA == falseB
    _ == _ = False

instance Ord (Expression a) where
    (Forall elemA rangeA domainA formulaA _ _) <= (Forall elemB rangeB domainB formulaB _ _) 
        = elemA <= elemB && rangeA <= rangeB && domainA <= domainB && formulaA <= formulaB
    (Exists elemA rangeA domainA formulaA _ _) <= (Forall elemB rangeB domainB formulaB _ _) 
        = elemA <= elemB && rangeA <= rangeB && domainA <= domainB && formulaA <= formulaB
    (BinOp opA lhsA rhsA _ _) <= (BinOp opB lhsB rhsB _ _)
        = opA <= opB && lhsA <= lhsB && rhsA <= rhsB
    (UnOp opA valueA _ _) <= (UnOp opB valueB _ _)
        = opA <= opB && valueA <= valueB
    (Var varA _ _) <= (Var varB _ _)
        = varA <= varB
    (SymbolicVar varA _ _) <= (SymbolicVar varB _ _)
        = varA <= varB
    (Parens valueA _ _) <= (Parens valueB _ _)
        = valueA <= valueB
    (Lit litA _ _) <= (Lit litB _ _)
        = litA <= litB    
    (SizeOf varA _ _) <= (SizeOf varB _ _)
        = varA <= varB
    (Ref refA _ _) <= (Ref refB _ _)
        = refA <= refB
    (SymbolicRef varA _ _) <= (SymbolicRef varB _ _)
        = varA <= varB
    (IteE guardA trueA falseA _ _) <= (IteE guardB trueB falseB _ _)
        = guardA <= guardB && trueA <= trueB && falseA <= falseB
    _ <= _ = False

instance Hashable (Expression a) where
    hashWithSalt salt (Forall elem range domain formula _ _)
        = salt `hashWithSalt` elem `hashWithSalt` range `hashWithSalt` domain `hashWithSalt` formula `hashWithSalt` (1 :: Int)
    hashWithSalt salt (Exists elem range domain formula _ _)
        = salt `hashWithSalt` elem `hashWithSalt` range `hashWithSalt` domain `hashWithSalt` formula `hashWithSalt` (2 :: Int)
    hashWithSalt salt (BinOp binOp lhs rhs _ _)
        = salt `hashWithSalt` binOp `hashWithSalt` lhs `hashWithSalt` rhs `hashWithSalt` (3 :: Int)
    hashWithSalt salt (UnOp unOp value _ _)
        = salt `hashWithSalt` unOp `hashWithSalt` value `hashWithSalt` (4 :: Int)
    hashWithSalt salt (Var var _ _)
        = hashWithSalt salt var `hashWithSalt` (5 :: Int)
    hashWithSalt salt (SymbolicVar var _ _)
        = hashWithSalt salt var `hashWithSalt` (6 :: Int)
    hashWithSalt salt (Parens value _ _)
        = hashWithSalt salt value `hashWithSalt` (7 :: Int)
    hashWithSalt salt (Lit lit _ _)
        = hashWithSalt salt lit `hashWithSalt` (8 :: Int)
    hashWithSalt salt (SizeOf var _ _)
        = hashWithSalt salt var `hashWithSalt` (9 :: Int)
    hashWithSalt salt (Ref ref _ _)
        = hashWithSalt salt ref `hashWithSalt` (10 :: Int)
    hashWithSalt salt (SymbolicRef var _ _)
        = hashWithSalt salt var `hashWithSalt` (11 :: Int)
    hashWithSalt salt (IteE guard true false _ _)
        = salt `hashWithSalt` guard `hashWithSalt` true `hashWithSalt` false `hashWithSalt` (12 :: Int)

instance WithPos Expression' where
    getPos = _info

data BinOp 
    = Implies  | And           | Or          | Equal            | NotEqual 
    | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual | Plus 
    | Minus    | Multiply      | Divide      | Modulo
    deriving (Show, Eq, Ord, Generic)

instance Hashable BinOp

data UnOp
    = Negative | Negate
    deriving (Show, Eq, Ord, Generic)
    
instance Hashable UnOp

type Lit' = Lit SourcePos
data Lit a
    = BoolLit   { _boolValue   :: Bool  , _info :: a }
    | UIntLit   { _uintValue   :: Int   , _info :: a }
    | IntLit    { _intValue    :: Int   , _info :: a }
    | FloatLit  { _floatValue  :: Float , _info :: a }
    | StringLit { _stringValue :: String, _info :: a }
    | CharLit   { _charValue   :: String, _info :: a }
    | NullLit   { _info        :: a                  }
    deriving (Show)

instance Eq (Lit a) where
    (BoolLit a _)   == (BoolLit b _)   = a == b
    (UIntLit a _)   == (UIntLit b _)   = a == b
    (IntLit a _)    == (IntLit b _)    = a == b
    (FloatLit a _)  == (FloatLit b _)  = a == b
    (StringLit a _) == (StringLit b _) = a == b
    (CharLit a _)   == (CharLit b _)   = a == b
    (NullLit _)     == (NullLit _)     = True
    _               == _               = False

instance Ord (Lit a) where
    (BoolLit a _)   <= (BoolLit b _)   = a <= b
    (UIntLit a _)   <= (UIntLit b _)   = a <= b
    (IntLit a _)    <= (IntLit b _)    = a <= b
    (FloatLit a _)  <= (FloatLit b _)  = a <= b
    (StringLit a _) <= (StringLit b _) = a <= b
    (CharLit a _)   <= (CharLit b _)   = a <= b
    (NullLit _)     <= (NullLit _)     = True
    _               <= _               = False

instance Hashable (Lit a) where
    hashWithSalt salt (BoolLit value _)   = hashWithSalt salt value `hashWithSalt` (1 :: Int)
    hashWithSalt salt (UIntLit value _)   = hashWithSalt salt value `hashWithSalt` (2 :: Int)
    hashWithSalt salt (IntLit value _)    = hashWithSalt salt value `hashWithSalt` (3 :: Int)
    hashWithSalt salt (FloatLit value _)  = hashWithSalt salt value `hashWithSalt` (4 :: Int)
    hashWithSalt salt (StringLit value _) = hashWithSalt salt value `hashWithSalt` (5 :: Int)
    hashWithSalt salt (CharLit value _)   = hashWithSalt salt value `hashWithSalt` (6 :: Int)
    hashWithSalt salt (NullLit _)         = hashWithSalt salt (7 :: Int)

instance WithPos Lit' where
    getPos = _info

--------------------------------------------------------------------------------
-- Type definitions
--------------------------------------------------------------------------------

type Type' = Type SourcePos
data Type a 
    = Type { _ty :: Maybe (NonVoidType a), _info :: a }
    deriving (Show, Eq)

type NonVoidType' = NonVoidType SourcePos
data NonVoidType a
    = UIntType      { _info :: a }
    | IntType       { _info :: a }
    | FloatType     { _info :: a }
    | BoolType      { _info :: a }
    | StringType    { _info :: a }
    | CharType      { _info :: a }
    | ReferenceType { _ty :: Identifier a, _info :: a }
    | ArrayType     { _innerTy :: NonVoidType a, _info :: a }
    deriving (Show, Eq)

data RuntimeType
    = UnknownRuntimeType
    | VoidRuntimeType
    | UIntRuntimeType
    | IntRuntimeType
    | FloatRuntimeType
    | BoolRuntimeType
    | StringRuntimeType
    | CharRuntimeType
    | ReferenceRuntimeType { _ty :: Identifier' }
    | ArrayRuntimeType { _innerTy :: RuntimeType }
    | ANYRuntimeType
    | NUMRuntimeType
    | REFRuntimeType
    | ARRAYRuntimeType
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Auxiliary definitions
--------------------------------------------------------------------------------

type Identifier' = Identifier SourcePos
data Identifier a
    = Identifier { _name :: String, _info :: a }
    deriving (Show)

instance Eq (Identifier a) where
    (Identifier x _) == (Identifier y _) = x == y

instance Ord (Identifier a) where
    (Identifier x _) <= (Identifier y _) = x <= y

instance Hashable (Identifier a) where
    hashWithSalt salt (Identifier name _)
        = hashWithSalt salt name

instance WithPos Identifier' where
    getPos = _info

type Label = Int