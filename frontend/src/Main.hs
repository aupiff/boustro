{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad.IO.Class
import           Data.FileEmbed
import qualified Data.Map.Strict as Map
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
  where toPageEvent keyCode _
           | keyCode == leftArrow  = Just PrevPage
           | keyCode == rightArrow = Just NextPage
           | otherwise             = Nothing
        rightArrow               = 39 :: Int
        leftArrow                = 37 :: Int


main :: IO ()
main = JQ.ready $ RD.mainWidgetWithCss $(embedStringFile "app/Boustro.css") $

    RD.elAttr "div" (Map.singleton "id" "content") $ do
        pagingEvent <- R.updated <$> pagingD

        _ <- RD.workflow (titlePage pagingEvent)
        return ()

-- TODO if I want these pages to both have access to pagingEvent, I could throw
-- them in a reader monad, couldn't I?
titlePage :: forall t (m :: * -> *).  MonadWidget t m
          => RD.Event t PageEvent -> RD.Workflow t m String
titlePage pagingEvent = RD.Workflow . RD.el "div" $ do
    RD.el "div" $ RD.text "This is a boustrophedon reading application. Use left and right arrows to turn pages."
    showTextView <-
        RD.button "Reading \"Tess of the D'Urbervilles\" by Thomas Hardy"
    return ("Page 1", textView pagingEvent <$ showTextView)


textView :: forall (m :: * -> *) t.  MonadWidget t m
         => RD.Event t PageEvent -> RD.Workflow t m String
textView pagingEvent = RD.Workflow . RD.el "div" $ do

    RD.elAttr "div" (Map.singleton "id" "scratch-area") RD.blank

    RD.elAttr "div" (Map.singleton "id" "boustro") RD.blank
    pb <- RD.getPostBuild
    let textRefresh = RD.leftmost [pagingEvent, fmap (const Start) pb]

    currentWord <- R.updated <$> RD.foldDyn pagingFunction 0 textRefresh

    lastWord <- RD.performEvent $ RD.ffor currentWord (liftIO . typesetPage)

    home <- RD.button "back home"

    return ("Page 2", titlePage pagingEvent <$ home)

    where pagingFunction NextPage currentPage = currentPage + 1
          pagingFunction PrevPage currentPage = max 0 $ currentPage - 1
          pagingFunction _ _ = 0
