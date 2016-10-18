module Construction where

import TypesTupleVer

constructor :: [TypedValue] -> [TypedValue]
constructor [] = []
constructor (v1:xs)
  | tyOf v1 == TOpen = case constructor xs of
    v2:vs -> if tyOf v2 == TEmpty || TCloseTag < tyOf v2 && tyOf v2 < TEmpty
      then case constructor vs of
        v3:ws -> if tyOf v3 == TClose
          then (TObject,ValueList [v1,v2,v3]): ws
          else v1:xs --stub
        [] -> v1:xs -- stub
      else v1:xs -- stub
    [] -> v1:xs -- stub
  | tyOf v1 == TOpenTag = case constructor xs of
    v2:vs -> if tyOf v2 == TEmpty || TCloseTag < tyOf v2 && tyOf v2 < TEmpty
      then case constructor vs of
        v3:ws -> if tyOf v3 == TCloseTag
          then (TTagObject,ValueList [v1,v2,v3]): ws
          else v1:xs --stub
        [] -> v1:xs -- stub
      else v1:xs -- stub
    [] -> v1:xs -- stub
  | tyOf v1 == TTOpen = case constructor xs of
    v2:vs -> if TCloseTag < tyOf v2 && tyOf v2 < TEmpty
      then case constructor vs of
        v3:ws -> if tyOf v3 == TTEnd
          then (TOpenTag, ValueList [v1,v2,v3]): ws
          else v1:xs -- stub
        [] -> v1:xs -- stub
      else v1:xs -- stub
    [] -> v1:xs -- stub
  | tyOf v1 == TTClose = case constructor xs of
    v2:vs -> if TCloseTag < tyOf v2 && tyOf v2 < TEmpty
      then case constructor vs of
        v3:ws -> if tyOf v3 == TTEnd
          then (TTClose, ValueList [v1,v2,v3]): ws
          else v1:xs -- stub
        [] -> v1:xs -- stub
      else v1:xs -- stub
    [] -> v1:xs -- stub
  | tyOf v1 == TKeyValueDelim = case constructor xs of
    v2:vs -> if TNumber <= tyOf v2 && tyOf v2 <= TKeyValue
      then (TKeyValueBase, ValueList [v1,v2]):vs
      else v1:xs -- stub
    [] -> v1:xs -- stub
  | tyOf v1 == TWS = case constructor xs of
    v2:vs -> (tyOf v2,ValueList [v1,v2]):vs
    [] -> v1:xs -- stub
  | tyOf v1 == TIterationDelim = case constructor xs of
    v2:vs -> if TNumber <= tyOf v2 && tyOf v2 <= TKeyValue
      then (tyOf v2,ValueList [v1,v2]):vs
      else v1:xs -- stub
    [] -> v1:xs -- stub
  | TCloseTag < tyOf v1 && tyOf v1 < TSequence = case constructor xs of
    v2:vs -> if TCloseTag < tyOf v2 && tyOf v2 < TEmpty
      then (TSequence [tyOf v1,tyOf v2], ValueList [v1,v2]):vs
      else v1:xs
    [] -> v1:xs
  | otherwise = v1:xs
