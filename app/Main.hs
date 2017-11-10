{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Support
import Text.Heterocephalus
import Text.Blaze.Renderer.Text (renderMarkup)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Yaml as Yaml
import Data.Either (either)
import Prelude hiding ((!!), FilePath)
import Data.Monoid ((<>))
import System.FilePath.Glob (glob)
import Control.Monad (forM, forM_)
import Language.Haskell.TH
import Turtle
import Data.String (fromString)

do
  sources <- runIO $ do
    cFilenames <- glob "src/A_Tour_of_Go/Concurrency/*.hs"
    let escape x = case x of
          '"' -> "\\\""
          '\n' -> "\\n"
          a -> LT.singleton a
    let isNotComment = not . isComment . LT.toStrict
    let removeComment = LT.unlines . reverse . tail . reverse . filter isNotComment . LT.lines
    contents <- map (LT.concatMap escape . removeComment) <$> mapM LT.readFile cFilenames
    let names = map (either (error . show) id . parseFilename) cFilenames
    return $ zip names contents
  ret <- forM sources $ \(name, content) -> do
    let name' = mkName $ "code" <> name
    let contentStr = LT.unpack content
    let v = ValD (VarP name') (NormalB (LitE (StringL contentStr))) []
    strTMaybe <- lookupTypeName "String"
    let strT = case strTMaybe of
          Just a -> a
          Nothing -> error ""
    let ann = SigD name' (ConT strT)
    return [ann, v]
  return $ concat ret

main :: IO ()
main = do
  generateHtmlFiles
  copyFiles

generateHtmlFiles :: IO ()
generateHtmlFiles = do
  mktree "target"
  iFilenames <- glob "static/i18n/*.yaml"
  forM_ iFilenames $ \iFilename -> do
    iMaybe <- Yaml.decodeFile iFilename :: IO (Maybe Yaml.Value)
    let i = case iMaybe of
          Nothing -> error $ "failed to decode: " <> iFilename
          Just x -> x
    let localeEither = parseLocale iFilename
    case localeEither of
      Left e -> print e
      Right locale -> do
        let localeDir = "target/" <> locale <> "/"
        mktree $ fromString $ localeDir <> "concurrency/"
        let compiledDict =
              $(do
                   hFilenames <- runIO $ glob "static/heterocephalus/**/*.html"
                   let names = map (either (error . show) id . parsePost "static/heterocephalus/") hFilenames
                   namesE <- mapM (\x -> [|x|]) names
                   compiledFilesE <- mapM compileTextFile hFilenames
                   return $ ListE $ zipWith (\x y -> TupE [x,y]) namesE compiledFilesE
               )
        forM_ compiledDict $ \(name, compiled) -> do
          LT.writeFile (localeDir <> name) $ renderMarkup compiled

copyFiles :: IO ()
copyFiles = do
  mktree "target"
  mktree "target/css"
  cptree "static/css" "target/css"
  mktree "target/images"
  cptree "static/images" "target/images"
