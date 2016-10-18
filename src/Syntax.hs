module Syntax where

data Term =
 Token String
 | OrderedStruct [Term]
 | Unit
 | Concat [Term] -- 簡単のためpairをリストで扱う
 | SubNode Term
 | UnionT Term Term
 | Repititon Term
 deriving (Show)
