{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Support
import Text.Heterocephalus
import Text.Blaze.Renderer.Text (renderMarkup)
import qualified Data.Text as T
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
import Data.Extensible
import Control.Lens hiding ((:>))
import Lucid
import Data.List (transpose, tails)

mkField "localeF dictF"
type I18N = Record
  [ "localeF" :> T.Text
  , "dictF" :> Yaml.Value
  ]

loadI18NFiles :: IO [I18N]
loadI18NFiles = do
  paths <- glob "static/i18n/*.yaml"
  forM paths $ \path -> do
    let locale = case parseLocale path of
          Left e -> error $ show e
          Right x -> T.pack x
    iMaybe <- Yaml.decodeFile path :: IO (Maybe Yaml.Value)
    let i = case iMaybe of
          Nothing -> error $ "failed to decode: " <> path
          Just x -> x
    return $
      localeF @= locale
      <: dictF @= i
      <: nil

mkField "titleF srcF targetF genArticleF"
type Page = Record
  [ "titleF" :> T.Text
  , "srcF" :> T.Text
  , "targetF" :> T.Text
  , "genArticleF" :> (I18N -> Html ())
  ]

toPage :: (T.Text, T.Text, T.Text, I18N -> Html ()) -> Page
toPage (name, src, target, genArticle) =
  titleF @= name
  <: srcF @= src
  <: targetF @= target
  <: genArticleF @= genArticle
  <: nil

pages :: [Page]
pages = map toPage
  [ ( "Goroutines", "A_Tour_of_Go/Concurrency/Goroutines.hs", "concurrency/goroutines.html", \i -> do
        let at = at1 i "goroutines"
        p_ $ at "first"
        pre_ $ code_ "async $ f x y z"
        p_ $ at "inter"
        pre_ $ code_ "f x y z"
        p_ ( at "overviewPreAsync"
             <> a_ [href_ "https://hackage.haskell.org/package/async"] "async"
             <> at "overviewPostAsync"
           )
        p_ ( at "detailPreBase"
             <> a_ [href_ "https://hackage.haskell.org/package/base"] "base"
             <> at "detailPostBase"
           )
    )
  , ( "Channels", "A_Tour_of_Go/Concurrency/Channels.hs", "concurrency/channels.html", \i -> do
        let at = at1 i "channels"
        let at' = at1' i "channels"
        p_ $ at "first"
        pre_ $ code_ $ toHtmlRaw $ T.unlines
          [ "writeChan ch v // " <> at' "writeChan"
          , "readChan ch    // " <> at' "readChan"
          ]
        p_ $ at "postRW"
        p_ $ at "preNewChan"
        pre_ $ code_ $ toHtmlRaw $ T.unlines
          [ "newChan"
          ]
        p_ $ at "postNewChan"
        p_ $ at "example"
        p_ $ stmAnchor <> at "stm"
    )
  , ( "Buffered Channels", "A_Tour_of_Go/Concurrency/BufferedChannels.hs", "concurrency/buffered-channels.html", \i -> do
        let at = at1 i "bufferedChannels"
        p_ ( a_ [href_ "https://hackage.haskell.org/package/BoundedChan"] "BoundedChan"
             <> at "first"
           )
        pre_ $ code_ $ toHtmlRaw $ T.unlines
          [ "newBoundedChan 100"
          ]
        p_ $ at "post"
        p_ $ stmAnchor <> at "stm"
    )
  , ( "Range and Close", "A_Tour_of_Go/Concurrency/RangeAndClose.hs", "concurrency/range-and-close.html", \i -> do
        let at = at1 i "rangeAndClose"
        p_ $ at "first"
        p_ $ at "range"
        p_ $ at "close"
        p_ $ at "list"
    )
  , ( "Select", "A_Tour_of_Go/Concurrency/Select.hs", "concurrency/select.html", \i -> do
        let at = at1 i "select"
        p_ $ at "first"
        p_ $ at "preStm"  <> stmAnchor <> at "postStm"
        p_ $ at "preMsum"
        pre_ $ code_ $ toHtmlRaw $ T.unlines
          [ "// another implimentation of select"
          , "import Control.Monad (msum)"
          , "select = atomically . msum"
          ]
        p_ $ at "postMsum"
    )
  , ( "Default Selection", "A_Tour_of_Go/Concurrency/DefaultSelection.hs", "concurrency/default-selection.html", \i -> do
        let at = at1 i "defaultSelection"
        p_ $ at "first"
        p_ $ at "preCode"
        pre_ $ code_ $ toHtmlRaw $ T.unlines
          [ "select [ readTQueue c >>= (\\i -> return $ do"
          , "           -- use i"
          , "           )"
          , "       , return $ do"
          , "           -- if `readTQueue c` block"
          , "       ]"
          ]
        p_ $ at "tickAfter"
    )
  ]
  where
    at1' i key subKey = (i ^. dictF) !!! [key, subKey]
    at1 i key subKey = toHtmlRaw $ at1' i key subKey
    stmAnchor = a_ [href_ "https://hackage.haskell.org/package/stm"] "stm"

pagesWindow :: [Page] -> [(Maybe Page, Page, Maybe Page)]
pagesWindow [] = []
pagesWindow (x:[]) = [(Nothing, x, Nothing)]
pagesWindow (x:y:[]) = [(Nothing, x, Just y), (Just x, y, Nothing)]
pagesWindow a@(x:y:z:xs) = (Nothing, x, Just y) : (pagesWindow' a <> [lastWindow a])
  where
    windows n xs = take (length xs - n + 1) $ transpose (take n (tails xs))
    pagesWindow' = map (\[x',y',z'] -> (Just x', y', Just z')) . windows 3
    lastWindow = (\(x':y':_) -> (Just y', x', Nothing)) . reverse

getLastPiece :: Page -> T.Text
getLastPiece = ("./"<>) . snd . T.breakOnEnd "/" . (^.targetF)

clenseCode :: LT.Text -> LT.Text
clenseCode = LT.concatMap escape . removeComment
  where
    escape x = case x of
      '"' -> "\\\""
      '\n' -> "\\n"
      '\\' -> "\\\\"
      c -> LT.singleton c
    isNotComment = not . isComment . LT.toStrict
    removeComment = LT.unlines . filter isNotComment . LT.lines

generateHtmlFiles :: IO ()
generateHtmlFiles = do
  i18ns <- loadI18NFiles
  let lastPageNumber = length pages
  forM_ (zip (pagesWindow pages) ([1..]::[Int])) $ \((prevPageMaybe, page, nextPageMaybe), pageNumber) -> do
    let prevPagePathMaybe = getLastPiece `fmap` prevPageMaybe
    let nextPagePathMaybe = getLastPiece `fmap` nextPageMaybe
    rawCode <- LT.readFile $ T.unpack $ "src/" <> (page ^. srcF)
    let code = clenseCode rawCode
    forM_ i18ns $ \i18n -> do
      let title = page ^. titleF
      let article = renderText $ (page ^. genArticleF) i18n
      let anotherLanguage = "../../" <> (if (i18n^.localeF) == "en_US" then "ja_JP" else "en_US") <> "/" <> (page^.targetF)
      let compiled = $(compileTextFile "static/heterocephalus/template.html")
      let targetPath = "target/" <> (i18n^.localeF) <> "/" <> (page^.targetF)
      let targetDirPath = fst $ T.breakOnEnd "/" targetPath
      mktree $ fromString $ T.unpack targetDirPath
      LT.writeFile (T.unpack targetPath) $ renderMarkup compiled

copyFiles :: IO ()
copyFiles = do
  mktree "target"
  mktree "target/css"
  cptree "static/css" "target/css"
  mktree "target/images"
  cptree "static/images" "target/images"

main = do
  generateHtmlFiles
  copyFiles
