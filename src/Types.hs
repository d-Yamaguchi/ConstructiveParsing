module Types where

data Term =
 Token String
 | Ordered_struct Term
 | Labeled String Term
 | Concat Term Term
 | Union Term Term
 | Repititon Term


-- value : logical inductive
-- with constructors : val

data Basic_sType =
 Bs_Empty
 | Bs_Number
 | Bs_Integer
 | Bs_Float
 | Bs_Boolean
 | Bs_String
 | Bs_Name
 | Bs_Text


data Struct_Type =
 S_Basic Basic_sType
 | S_Object
 | S_TagObject
 | S_KeyValue
 | S_Array Struct_Type
 | S_Sequence
 | S_Union Struct_Type Struct_Type


data Basic_dType =
 Bd_Delim
 | Bd_KeyValueDelim
 | Bd_Open
 | Bd_Close
 | Bd_TOpen
 | Bd_TClose
 | Bd_TEnd
 | Bd_WS


data Delim_Type =
 D_Basic Basic_dType
 | D_OpenTag
 | D_CloseTag


data Ty =
 Ty_Struct Struct_Type
 | Ty_Delim Delim_Type

--basicType :: Basic_sType -> Ty
--Basic_sType = Ty_Struct S_Basic
