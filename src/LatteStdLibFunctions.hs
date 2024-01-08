module LatteStdLibFunctions where

import qualified Data.Map

import QuadrupleCode

stdFunctionsResultTypes :: Data.Map.Map String TypeQ
stdFunctionsResultTypes = Data.Map.fromList
  [ ("printInt", VoidQ)
  , ("printString", VoidQ)
  , ("error", VoidQ)
  , ("readInt", IntQ)
  -- , ("readString", StringQ)
  ]

stdFunctionsDeclarations :: String
stdFunctionsDeclarations = unlines [
    "declare void @printInt(i32)"
    -- ,"declare void @printString(i8*)",
    -- ,"declare void @error()",
    -- ,"declare i32 @readInt()",
    -- ,"declare i8* @readString()",
    -- ,"declare i8* @concat(i8*, i8*)"
  ]
