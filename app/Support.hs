module Support where

import Control.Lens
import Data.Aeson.Lens
import Data.Monoid ((<>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text
import Debug.Trace
import Data.List (intercalate)

(!!) :: (AsValue s) => s -> T.Text -> T.Text
j !! k = case (j ^? (key k) . _String) of
  Nothing -> error $ "key not found: " <> show k
  Just x -> x

(!!!) :: (AsValue s) => s -> [T.Text] -> T.Text
_ !!! [] = error "at least 1 key is required"
j !!! a@(k:ks) = case (j ^?  key k . (foldr (.) _String $ map key ks)) of
  Nothing -> error $ "key not found: " <> show a
  Just x -> x

localeParser :: Parser String
localeParser = (many $ try dirLayer) >> manyTill anyChar yamlSuffix
  where
    dirLayer :: Parser String
    dirLayer = manyTill anyChar (char '/')
    yamlSuffix :: Parser String
    yamlSuffix = string ".yaml" <|> string ".yml"

parseLocale :: FilePath -> Either (ParseError Char Dec) String
parseLocale filename = parse localeParser "" $ T.pack filename

postParser :: String -> Parser String
postParser prefix = manyTill anyChar (string prefix) >> many anyChar

parsePost :: FilePath -> FilePath -> Either (ParseError Char Dec) String
parsePost prefix filename = parse (postParser prefix) "" $ T.pack filename

filenameParser :: Parser String
filenameParser = (many $ try dirLayer) >> (intercalate "." <$> (many $ try nameLayer))
  where
    dirLayer :: Parser String
    dirLayer = manyTill anyChar (char '/')
    nameLayer :: Parser String
    nameLayer = manyTill anyChar (char '.')

parseFilename :: FilePath -> Either (ParseError Char Dec) String
parseFilename filename = parse filenameParser "" $ T.pack filename

commentParser :: Parser String
commentParser = many (char ' ') >> string "--"

isComment :: T.Text -> Bool
isComment line = case parse commentParser "" line of
  Left _ -> False
  Right _ -> True
