{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main where


import           Control.Monad
import           Control.Monad.IO.Class
import           Data.FileEmbed
import qualified Data.Map.Strict as Map
import           GHCJS.DOM (webViewGetDomDocument)
import           GHCJS.DOM.EventM (on, preventDefault)
import           GHCJS.DOM.Element (keyDown)
import           GHCJS.DOM.Document (getBody)
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import           Reflex
import           Reflex.Dom.Class
import qualified Reflex.Dom as RD

import           Typography


main :: IO ()
main = JQ.ready $ RD.mainWidget $ void . RD.workflow $ titlePage


titlePage :: forall t (m :: * -> *).  MonadWidget t m => RD.Workflow t m String
titlePage = RD.Workflow . RD.el "div" $ do

    RD.elAttr "div" (Map.singleton "id" "content") $

          do RD.el "h1" $ RD.text "βουστροφηδόν"

             RD.el "p" $

                RD.text "An ancient, efficient, yet \
                       \ unfortunately forgotten style of typsetting."

             RD.el "p" $ RD.text "In the reader view, use left and \
                                \ right arrows to turn pages."

    RD.elAttr "div" (Map.singleton "id" "menu") $ do

        showTextView <-

            RD.button "Read \"Tess of the D'Urbervilles\" by Thomas Hardy"

        return ("Page 1", textView <$ showTextView)


textView :: forall (m :: * -> *) t.  MonadWidget t m => RD.Workflow t m String
textView = RD.Workflow . RD.el "div" $ do

    -- `scratch-area` is a hidden div where words widths are measured
    RD.elAttr "div" (Map.singleton "id" "scratch-area") RD.blank

    pagingE <- pagingEvent

    RD.elAttr "div" (Map.singleton "id" "reader-view") $ do

        (boustroEl, posString) <- RD.elAttr' "div" (Map.singleton "id" "boustro") $ do

            pb <- RD.getPostBuild

            let buildAndPagingEvent = RD.leftmost [ fmap (const Start) pb
                                                  , pagingE ]

            rec wordDelta  <- pageEventResponse buildAndPagingEvent wordDeltaD
                wordDeltaD <- RD.holdDyn (0,0) wordDelta
                posString' <- RD.mapDyn renderProgressString wordDeltaD

            return posString'

        let svgClick = RD.domEvent RD.Mouseup boustroEl

        RD.dynText posString
        clickInfo <- RD.holdDyn "" $ fmap show svgClick
        RD.dynText clickInfo

        home <- RD.button "back home"

        return ("Page 2", titlePage <$ home)

    where renderProgressString = show . flip (,) (length processedWords) . fst


pageEventResponse :: MonadWidget t m
                  => RD.Event t PageEvent -> RD.Dynamic t (Int, Int)
                  -> m (RD.Event t (Int, Int))
pageEventResponse pageEvent currentWord = RD.performEvent $

        (liftIO . typesetPage) <$> RD.attachDyn currentWord pageEvent


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
