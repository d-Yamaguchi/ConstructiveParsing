module TypesTupleVer where

data FlatTypes =
  TNumber -- Bs Types
  | TInteger
  | TFloat
  | TBoolean
  | TString
  | TName
  | TText -- end Bs Types
  | TObject
  | TTagObject
  | TKeyValueBase
  | TKeyValue
  | TIterationDelim -- Bd types
  | TKeyValueDelim
  | TClose
  | TOpen
  | TTOpen
  | TTClose
  | TTEnd
  | TWS -- end Bd Types
  | TOpenTag
  | TCloseTag
  | TV FlatTypes --TS
  | TList FlatTypes
  | TSequence [FlatTypes]-- end TS
  | TEmpty
  | TUnionC FlatTypes FlatTypes
  deriving (Show,Eq,Ord)

data Value =
  Token String
  | Link Value
  | Struct Value
  | ValueList [TypedValue]
  deriving (Show)

type TypedValue = (FlatTypes, Value)

tyOf :: TypedValue -> FlatTypes
tyOf = fst
