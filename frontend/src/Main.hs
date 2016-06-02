{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main where


import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Maybe

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


pagingEvent :: MonadWidget t m => m (RD.Event t PageEvent)
pagingEvent = do
     wv <- askWebView
     Just doc <- liftIO $ webViewGetDomDocument wv
     Just body <- liftIO $ getBody doc
     kp <- RD.wrapDomEvent body (`on` keyDown) $ do
       i <- RD.getKeyEvent
       preventDefault
       return i
     return $ RD.fmapMaybe toPageEvent kp

  where toPageEvent keyCode
           | keyCode == leftArrow  = Just PrevPage
           | keyCode == rightArrow = Just NextPage
           | otherwise             = Nothing
        rightArrow               = 39 :: Int
        leftArrow                = 37 :: Int


main :: IO ()
main = JQ.ready $ RD.mainWidget $

    -- RD.mainWidgetWithCss $(embedStringFile "app/Boustro.css") $
    void . RD.workflow . titlePage =<< pagingEvent


-- TODO if I want these pages to both have access to pagingEvent, I could throw
-- them in a reader monad, couldn't I?
titlePage :: forall t (m :: * -> *).  MonadWidget t m
          => RD.Event t PageEvent -> RD.Workflow t m String
titlePage pagingEvent = RD.Workflow . RD.el "div" $ do

    RD.elAttr "div" (Map.singleton "id" "content") $

          do RD.el "h1" $ RD.text "Boustrophedon"

             RD.el "p" $

                RD.text "An ancient, efficient, yet \
                       \ unfortunately forgotten style of typsetting."

             RD.el "p" $ RD.text "In the reader view, use left and \
                                \ right arrows to turn pages."

    RD.elAttr "div" (Map.singleton "id" "menu") $ do

        showTextView <-

            RD.button "Read \"Tess of the D'Urbervilles\" by Thomas Hardy"

        return ("Page 1", textView pagingEvent <$ showTextView)


-- Now we have some Reflex code to wrap our demo in a minimal web page
pageEventResponse :: MonadWidget t m => RD.Event t PageEvent -> RD.Dynamic t (Int, Int) -> m (RD.Event t (Int, Int))
pageEventResponse pagingEvent currentWord = RD.performEvent $

        (liftIO . typesetPage) <$> RD.attachDyn currentWord pagingEvent


textView :: forall (m :: * -> *) t.  MonadWidget t m
         => RD.Event t PageEvent -> RD.Workflow t m String
textView pagingEvent = RD.Workflow . RD.el "div" $ do

    -- `scratch-area` is a hidden div where words widths are measured
    RD.elAttr "div" (Map.singleton "id" "scratch-area") $ RD.blank

    RD.elAttr "div" (Map.singleton "id" "reader-view") $ do

        posString <- RD.elAttr "div" (Map.singleton "id" "boustro") $ do

            pb <- RD.getPostBuild

            rec wordDelta  <- pageEventResponse (RD.leftmost [fmap (const Start) pb, pagingEvent]) wordDeltaD
                wordDeltaD <- RD.holdDyn (0,0) wordDelta
                posString' <- RD.mapDyn (show . (flip (,) (length processedWords)) . fst) wordDeltaD

            return posString'

        RD.dynText posString

        home <- RD.button "back home"

        return ("Page 2", titlePage pagingEvent <$ home)
