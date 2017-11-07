{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Support ((!!), (!!!), parseLocale, parsePost)
import Text.Heterocephalus
import Text.Blaze.Renderer.Text (renderMarkup)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Directory (createDirectoryIfMissing)
import qualified Data.Yaml as Yaml
import Data.Aeson.Lens
import Control.Lens hiding (noneOf)
import Data.Maybe (fromJust, maybe)
import Data.Either (either)
import Prelude hiding ((!!))
import Data.Monoid ((<>))
import System.FilePath.Glob (glob)
import Control.Monad (forM_)
import Language.Haskell.TH
-- import Text.Megaparsec
-- import Text.Megaparsec.Text
-- import qualified Data.HashMap.Lazy as LHM

-- do
--   iMaybe <- runIO $ Yaml.decodeFile "static/i18n/ja_JP.yaml" :: Q (Maybe Yaml.Value)
--   let i = case iMaybe of
--             Nothing -> error "failed to parse yaml file"
--             Just x -> x
--   let f v name = case v of
--         Yaml.Object v -> concat $ map (\(n, v') -> f v' (name <> "_" <> n)) $ LHM.toList v
--         Yaml.String t -> [(name, t)]
--   let x = f i ""
--   runIO $ print x
--   return $ []

main :: IO ()
main = do
  generateHtmlFiles

generateHtmlFiles :: IO ()
generateHtmlFiles = do
  createDirectoryIfMissing True "target"
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
        let dirname = "target/" <> locale <> "/"
        createDirectoryIfMissing True dirname
        let compiledDict =
              $(do
                   hFilenames <- runIO $ glob "static/heterocephalus/*.html"
                   let names = map (either (error . show) id . parsePost "static/heterocephalus/") hFilenames
                   namesE <- mapM (\x -> [|x|]) names
                   compiledFilesE <- mapM compileHtmlFile hFilenames
                   return $ ListE $ zipWith (\x y -> TupE [x,y]) namesE compiledFilesE
               )
        forM_ compiledDict $ \(name, compiled) -> do
          LT.writeFile (dirname <> name) $ renderMarkup compiled

