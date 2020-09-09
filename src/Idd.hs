{-# LANGUAGE OverloadedStrings #-}
module Idd where

import qualified Data.Text as T
import Data.Maybe

data IddObject = IddObject {
         memo :: String,
         unique :: Bool,
         minFields :: Int,
         iddFields :: [IddField] }

data IddField = IddField { name :: String, iddFieldType :: IddFieldType }

data IddFieldType = Integer | Real | Alpha | Choice [String] | ObjectList | ExternalList | Node

data IddAttribute = IddAttribute {
        iddAttributeLineNum :: Int,
        iddAttribute :: String,
        iddAttributeValue :: String
    }

iddFieldNote :: [IddAttribute] -> String
iddFieldNote attrs = unwords $ iddAttrFilter "note" attrs

isNote :: IddAttribute -> Bool
isNote attr = iddAttribute attr == "note"

iddAttrFilter :: String -> [IddAttribute] -> [String]
iddAttrFilter commentType attrs = fmap iddAttributeValue
                                    (filter (\attr -> iddAttribute attr == commentType) attrs)

readAttribute :: Int -> T.Text -> Maybe IddAttribute
readAttribute lineNum line
    | T.length attr == 0 = Nothing
    | otherwise = Just (IddAttribute {
                          iddAttributeLineNum = lineNum,
                          iddAttribute        = T.unpack $ attrType attr,
                          iddAttributeValue   = T.unpack $ attrValue attr
                        })
    where attr = extractAttr line

extractAttr :: T.Text -> T.Text
extractAttr = T.dropWhile (/= '\\')

-- Break apart the attribute at the first space.
splitAttr :: T.Text -> (T.Text, T.Text)
splitAttr = T.break (== ' ')

attrType :: T.Text -> T.Text
attrType attr = T.tail $ fst $ splitAttr attr

attrValue :: T.Text -> T.Text
attrValue attr = snd $ splitAttr attr

parseAttributes :: T.Text -> [IddAttribute]
parseAttributes iddFile =
         mapMaybe readAttrs $ zip [1..] (T.lines iddFile)
         where readAttrs (lineNum, lineText) = readAttribute lineNum lineText

