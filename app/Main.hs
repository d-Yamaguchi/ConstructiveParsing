module Main where

--import Types
import TypesTupleVer
import Construction

main :: IO ()
main = print $ constructor [(TOpen, Token "{"),(TString, Token "name"),(TKeyValueDelim,Token ":"),(TString, Token "Taro"),(TClose,Token "}")]
