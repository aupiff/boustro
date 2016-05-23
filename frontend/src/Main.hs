{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.FileEmbed
import           Data.JSString.Text
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           GHCJS.DOM (webViewGetDomDocument)
import           GHCJS.DOM.EventM (on, preventDefault)
import           GHCJS.DOM.Element (keyDown)
import           GHCJS.DOM.Document (getBody)
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import qualified Reflex as R
import           Reflex.Dom.Class
import qualified Reflex.Dom as RD

import           Typography

data PageEvent = NextPage | PrevPage | Start deriving Show


wordsWithWidths :: [String] -> IO [Item JQ.JQuery Double]
wordsWithWidths inputWords = do

     ws <- mapM (toItem . T.pack) inputWords

     -- creating a temporary div specifically to measure the width of every element
     scratchArea <- JQ.empty =<< JQ.select "#scratch-area"
     mapM_ (`JQ.appendJQuery` scratchArea) $ fmap itemElement ws
     reverse <$> foldM func [] ws

     where func (Penalty w _ flag a : ls) i = do
                width <- JQ.getInnerWidth (itemElement i)
                return $ setItemWidth width i : Penalty w width flag a : ls
           func p i = do n <- (\w -> return $ setItemWidth w i) =<< JQ.getInnerWidth (itemElement i)
                         return $ n : p


arrangeBoustro :: [Item JQ.JQuery Double] -> IO ()
arrangeBoustro boxes = do
    ls <- mapM renderLine (removeSpacesFromEnds <$> foldr accumLines [[]] boxes)
    -- Should I be applying this style every time? Definitely on window change
    -- dim, so maybe it's not so bad.
    textArea <- JQ.select "#boustro" >>= (JQ.empty >=> widthCss)
    mapM_ (`JQ.appendJQuery` textArea) =<< boustro ls
    where widthCss = JQ.setCss "width" (textToJSString . T.pack $ show textWidth)


pagingD :: MonadWidget t m => m (RD.Dynamic t PageEvent)
pagingD = do
     wv <- askWebView
     Just doc <- liftIO $ webViewGetDomDocument wv
     Just body <- liftIO $ getBody doc
     kp <- RD.wrapDomEvent body (`on` keyDown) $ do
       i <- RD.getKeyEvent
       preventDefault
       return i
     RD.foldDynMaybe toPageEvent Start kp
  where toPageEvent 37 _ = Just PrevPage
        toPageEvent 39 _ = Just NextPage
        toPageEvent _  _ = Nothing


main :: IO ()
main = JQ.ready $ RD.mainWidgetWithCss $(embedStringFile "app/Boustro.css") $

    RD.elAttr "div" (Map.singleton "id" "content") $ do
        pagingEvent <- R.updated <$> pagingD

        _ <- RD.workflow (titlePage pagingEvent)

        -- TODO git rid of this if possible
        RD.elAttr "div" (Map.singleton "id" "scratch-area") RD.blank


-- TODO if I want these pages to both have access to pagingEvent, I could throw
-- them in a reader monad, couldn't I?
titlePage :: forall t (m :: * -> *).  MonadWidget t m
          => RD.Event t PageEvent -> RD.Workflow t m String
titlePage pagingEvent = RD.Workflow . RD.el "div" $ do
    RD.el "div" $ RD.text "This is a boustrophedon reading application"
    showTextView <- RD.button "Start reading \"Middlemarch\" by George Elliot"
    return ("Page 1", textView pagingEvent <$ showTextView)


textView :: forall (m :: * -> *) t.  MonadWidget t m
         => RD.Event t PageEvent -> RD.Workflow t m String
textView pagingEvent = RD.Workflow . RD.el "div" $ do

    RD.elAttr "div" (Map.singleton "id" "boustro") RD.blank
    pb <- RD.getPostBuild
    let textRefresh = RD.leftmost [pagingEvent, fmap (const Start) pb]
    currentPage <- R.updated <$> RD.foldDyn pagingFunction 0 textRefresh

    RD.performEvent_ $ RD.ffor currentPage (\p -> do
            wordBoxes <- liftIO . wordsWithWidths . take 500 . drop (500 * p) $ processedWords
            liftIO $ arrangeBoustro wordBoxes
            )

    home <- RD.button "back home"

    return ("Page 2", titlePage pagingEvent <$ home)

    where pagingFunction NextPage currentPage = currentPage + 1
          pagingFunction PrevPage currentPage = max 0 $ currentPage - 1
          pagingFunction _ _ = 0
