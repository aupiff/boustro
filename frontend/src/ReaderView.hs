{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module ReaderView where

import           Control.Monad
import           Data.Monoid ((<>))
import           Control.Monad.IO.Class
import           Data.FileEmbed
import qualified Data.Map.Strict as Map
import           GHCJS.DOM (webViewGetDomDocument)
import           GHCJS.DOM.Window ( getInnerHeight, getInnerWidth
                                  , getWindow, resize)
import           GHCJS.DOM.EventM (on, preventDefault)
import           GHCJS.DOM.Element (keyDown)
import           GHCJS.DOM.Document (getBody)
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import           Reflex
import           Reflex.Dom.Class
import qualified Reflex.Dom as RD

import Style
import Typography


titlePage :: forall t (m :: * -> *).  MonadWidget t m => RD.Workflow t m String
titlePage = RD.Workflow . RD.el "div" $ do

    (w, h) <- windowDimensions

    let contentWidth = min 650 $ w - 40

    RD.elAttr "div" ("id" =: "content" <> style [("width", show contentWidth)]) $

          do RD.el "h1" $ RD.text "βουστροφηδόν"

             RD.el "p" $

                RD.text "An ancient, efficient, yet \
                       \ unfortunately forgotten style of typsetting."

             RD.el "p" $ RD.text "In the reader view, use left and \
                                \ right arrows to turn pages on PCs.\
                                \ On mobile phones, single click \
                                \ left and right sides of the screen."

    RD.elAttr "div" (Map.singleton "id" "menu") $ do

        showTextView <-

            RD.button "Read \"Tess of the D'Urbervilles\" by Thomas Hardy"

        dims <- RD.performEvent $ liftIO . const (viewDims w h) <$> showTextView

        return ("Page 1", textView <$> dims)

    where viewDims w h = do lineHeight <- measureLineHeight
                            let w' = min 650 $ fromIntegral w - 40
                                h' = fromIntegral h - 20
                            return $ ViewDimensions w w' h' lineHeight


windowDimensionsE :: MonadWidget t m => m (RD.Event t (Int, Int))
windowDimensionsE = do
     wv <- askWebView
     Just window <- liftIO $ getWindow wv
     RD.wrapDomEvent window (`on` resize) $ do
       w <- getInnerWidth window
       h <- getInnerHeight window
       preventDefault
       return (w, h)


windowDimensions :: MonadWidget t m => m (Int, Int)
windowDimensions = do
     wv <- askWebView
     Just window <- liftIO $ getWindow wv
     w <- liftIO $ getInnerWidth window
     h <- liftIO $ getInnerHeight window
     return (w, h)


textView :: forall (m :: * -> *) t.  MonadWidget t m
         => ViewDimensions -> RD.Workflow t m String
textView vd@(ViewDimensions fullWidth textWidth textHeight lineHeight) =

    RD.Workflow . RD.el "div" $ do

        pagingE <- pagingEvent

        pb <- RD.getPostBuild

        RD.elAttr "div" ("id" =: "content" <> style [("width", show textWidth)]) $ do

            (boustroEl, _) <- RD.elAttr' "div" (Map.singleton "id" "boustro") $ return ()

            let textClick = RD.domEvent RD.Mouseup boustroEl
                textTransform = (\x -> if x > div fullWidth 2 then NextPage else PrevPage) . fst
                buildAndPagingEvent = RD.leftmost [ fmap (const Start) pb
                                                  , pagingE
                                                  , fmap textTransform textClick
                                                  ]

            rec wordDelta  <- pageEventResponse buildAndPagingEvent wordDeltaD vd
                wordDeltaD <- RD.holdDyn (0,0) wordDelta
                posString <- RD.mapDyn renderProgressString wordDeltaD

            home <- RD.button "back home"

            return ("Page 2", titlePage <$ home)

    where renderProgressString = show . flip (,) (length processedWords) . fst


pageEventResponse :: MonadWidget t m
                  => RD.Event t PageEvent -> RD.Dynamic t (Int, Int)
                  -> ViewDimensions -> m (RD.Event t (Int, Int))
pageEventResponse pageEvent currentWord vd = RD.performEvent $

        (liftIO . typesetPage vd) <$> currentWord `RD.attachDyn` pageEvent


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
