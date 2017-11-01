module Support where

import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Data.Text as T

dirLayer :: Parser String
dirLayer = manyTill anyChar (char '/')

localeParser :: Parser String
localeParser = (many $ try dirLayer) >> manyTill anyChar yamlSuffix
  where
    yamlSuffix :: Parser String
    yamlSuffix = string ".yaml" <|> string ".yml"

parseLocale :: FilePath -> Either (ParseError Char Dec) String
parseLocale filename = parse localeParser "" $ T.pack filename

-- filenameParser :: Parser String
-- filenameParser = (many $ try dirLayer) >> many anyChar
-- 
-- parseFilename :: FilePath -> Either (ParseError Char Dec) String
-- parseFilename filename = parse filenameParser "" $ T.pack filename

postParser :: String -> Parser String
postParser prefix = manyTill anyChar (string prefix) >> many anyChar

parsePost :: FilePath -> FilePath -> Either (ParseError Char Dec) String
parsePost prefix filename = parse (postParser prefix) "" $ T.pack filename
