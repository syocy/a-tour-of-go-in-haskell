{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.Heterocephalus
import Text.Blaze.Renderer.Text (renderMarkup)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Directory (createDirectoryIfMissing)
import qualified Data.Yaml as Yaml
import Data.Aeson.Lens
import Control.Lens
import Data.Maybe (fromJust)
import Prelude hiding ((!!))
import Data.Monoid ((<>))

(!!) :: (AsValue s) => s -> T.Text -> T.Text
j !! k = fromJust (j ^? (key k) . _String)

(!!!) :: (AsValue s) => s -> [T.Text] -> T.Text
j !!! (k:ks) = fromJust (j ^?  key k . (foldl (\r x->x.r) _String . map key $ reverse ks))

main :: IO ()
main = do
  let name = "Foo" :: LT.Text

  -- i18n <- Yaml.decodeFile "static/i18n/en_US.yaml" :: 
  -- let x = i18n ^. key "a"
  let vars = "{\"aaa\": \"the aaa\", \"bbb\": {\"ccc\": \"the ccc\"}}" :: LT.Text
  let num = 1 :: Int
  -- print $ vars ^. key "aaa" . _Number
  print $ vars ^? key "bbb" . key "ccc" . _String
  print $ vars !!! ["bbb", "ccc"]

  let rendered = renderMarkup $(compileHtmlFile "static/heterocephalus/test.html")
  createDirectoryIfMissing True "target"
  LT.writeFile "target/test.html" rendered
