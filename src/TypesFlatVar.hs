module TypesFlatVar where

import Syntax

data StructuredTypes =
  TV FlatTypes
  | TPair StructuredTypes StructuredTypes
  | TList StructuredTypes
  deriving (Show,Eq)

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
  | TS StructuredTypes
  | TUnionC FlatTypes FlatTypes
  | TEmpty
  deriving (Show,Eq)


typeOf :: Term -> Maybe FlatTypes
typeOf Unit = Just TEmpty -- empty
typeOf (Concat (x:nil)) = typeOf x -- empty2
typeOf (OrderedStruct (x:nil)) = typeOf x -- struct
typeOf (SubNode x) = typeOf x -- Label
typeOf (Concat (SubNode v1:SubNode v2:SubNode v3:nil)) = case (typeOf v1,typeOf v2,typeOf v3) of
  (Just TOpenTag, Just (TS _), Just TCloseTag) -> Just TTagObject -- tagobject1
  (Just TOpenTag, Just TEmpty, Just TCloseTag) -> Just TTagObject -- tagobject2
typeOf (Concat (v1:SubNode v2:v3:nil)) = case (typeOf v1,typeOf v2,typeOf v3) of
  (Just TOpen, Just (TS _), Just TClose) -> Just TObject -- Object1
  (Just TOpen, Just TEmpty, Just TClose) -> Just TObject -- Onject2
  (Just TTOpen, Just (TS _), Just TTEnd) -> Just TOpenTag -- Opentag
  (Just TTClose, Just (TS _), Just TTEnd) -> Just TCloseTag -- Closetag
typeOf (Concat (v1:v2:nil)) = case (typeOf v1, typeOf v2) of
  (Just TKeyValueDelim, Just TNumber) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TInteger) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TFloat) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TBoolean) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TString) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TName) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TText) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TObject) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TTagObject) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TKeyValueBase) -> Just TKeyValueBase
  (Just TKeyValueDelim, Just TKeyValue) -> Just TKeyValueBase
  (Just TWS, Just t) -> Just t -- WS
  (Just TIterationDelim, Just TNumber) -> Just TNumber
  (Just TIterationDelim, Just TInteger) -> Just TInteger
  (Just TIterationDelim, Just TFloat) -> Just TFloat
  (Just TIterationDelim, Just TBoolean) -> Just TBoolean
  (Just TIterationDelim, Just TString) -> Just TKeyValueBase
  (Just TIterationDelim, Just TName) -> Just TName
  (Just TIterationDelim, Just TText) -> Just TText
  (Just TIterationDelim, Just TObject) -> Just TObject
  (Just TIterationDelim, Just TTagObject) -> Just TTagObject
  (Just TIterationDelim, Just TKeyValueBase) -> Just TKeyValueBase
  (Just TIterationDelim, Just TKeyValue) -> Just TKeyValue
typeOf (Concat (SubNode v1:SubNode v2:nil)) = case (typeOf v1, typeOf v2) of
  (Just (TS a), Just (TS b)) -> Just (TS (TPair a b))-- tsequence
typeOf _ = Nothing
