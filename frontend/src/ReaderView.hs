{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}

module ReaderView where

import           Data.Monoid ((<>))
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import           GHCJS.DOM (webViewGetDomDocument)
import           GHCJS.DOM.Window ( getInnerHeight, getInnerWidth
                                  , getWindow, resize, load)
import           GHCJS.DOM.EventM (on, preventDefault)
import           GHCJS.DOM.Element (keyDown)
import           GHCJS.DOM.Document (getBody)
import           Reflex.Dom.Class
import qualified Reflex.Dom as RD

import Style
import Typography


titlePage :: forall t (m :: * -> *). MonadWidget t m
          => RD.Workflow t m ()
titlePage = RD.Workflow $ do

    viewDimsD <- viewDimensions

    contentStyleMap <- RD.mapDyn dynamicStyle viewDimsD

    RD.elDynAttr "div" contentStyleMap $

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

                      RD.button "Read \"Inside the Whale\" by George Orwell"

                return ((), textView <$ showTextView)


textView :: forall (m :: * -> *) t. MonadWidget t m
         => RD.Workflow t m ()
textView = RD.Workflow . RD.el "div" $ do

        viewDimsD <- viewDimensions
        contentStyleMap <- RD.mapDyn dynamicStyle viewDimsD
        pagingE <- pagingEvent

        RD.elDynAttr "div" contentStyleMap $ do

          RD.elAttr "div" ("id" =: "b" <> style [("width", "100%"), ("height", "90%")]) $ do

            (boustroEl, _) <- RD.elAttr' "div" (Map.singleton "id" "boustro") $ return ()

            pb <- RD.getPostBuild

            let textClick = RD.domEvent RD.Mouseup boustroEl
                textClick' = RD.attachDynWith textTransform viewDimsD textClick
                textTransform vd (x, _) | x > div (fullWidth vd) 2 = NextPage
                                        | otherwise                = PrevPage

                resizeE = RD.updated viewDimsD

                buildAndPagingEvent = RD.leftmost [ const Start <$> pb
                                                  , pagingE
                                                  , textClick'
                                                  , const Resize <$> resizeE
                                                  ]

            rec wordDelta  <- pageEventResponse buildAndPagingEvent wordDeltaD viewDimsD
                wordDeltaD <- RD.holdDyn (0,0) wordDelta

            return ()

          RD.elAttr "div" ("id" =: "back" <> style [("width", "100%"), ("height", "10%")]) $ do

            home <- RD.button "<="

            return ((), titlePage <$ home)


viewDimensions :: forall (m :: * -> *) t. MonadWidget t m
               => m (RD.Dynamic t ViewDimensions)
viewDimensions = do
    (w, h) <- windowDimensions
    lineHeight <- liftIO measureLineHeight
    wds <- windowDimensionsE
    RD.holdDyn (makeDims lineHeight w h) $ uncurry (makeDims lineHeight) <$> wds
    where makeDims lineHeight w h = let w' = min 700 $ fromIntegral w - 40
                                        h' = fromIntegral h - 8
                                    in ViewDimensions w w' h' lineHeight


dynamicStyle (ViewDimensions fullWidth textWidth fullHeight lineHeight) =
    "id" =: "content" <> style [ ("width", show textWidth) , ("height", show fullHeight) ]


pageEventResponse :: MonadWidget t m
                  => RD.Event t PageEvent -> RD.Dynamic t (Int, Int)
                  -> RD.Dynamic t ViewDimensions -> m (RD.Event t (Int, Int))
pageEventResponse pageEvent currentWord vd = RD.performEvent $

        (liftIO . typesetPage) <$> vd `RD.attachDyn` (currentWord `RD.attachDyn` pageEvent)


pagingEvent :: MonadWidget t m => m (RD.Event t PageEvent)
pagingEvent = do

     wv <- askWebView
     bodyM <- liftIO $ webViewGetDomDocument wv >>= maybe (return Nothing) getBody

     case bodyM of

         Just body -> do

            kp <- RD.wrapDomEvent body (`on` keyDown) $ do
              i <- RD.getKeyEvent
              preventDefault
              return i
            return $ RD.fmapMaybe toPageEvent kp

         Nothing -> return RD.never

  where toPageEvent keyCode
           | keyCode == leftArrow  = Just PrevPage
           | keyCode == rightArrow = Just NextPage
           | otherwise             = Nothing
        rightArrow               = 39 :: Int
        leftArrow                = 37 :: Int


windowDimensionsE :: MonadWidget t m => m (RD.Event t (Int, Int))
windowDimensionsE = do
     wv <- askWebView
     windowM <- liftIO $ getWindow wv

     case windowM of

        Just window ->

            RD.wrapDomEvent window (`on` resize) $ do
              w <- liftIO $ getInnerWidth window
              h <- liftIO $ getInnerHeight window
              preventDefault
              return (w, h)

        Nothing -> return RD.never


windowDimensions :: MonadWidget t m => m (Int, Int)
windowDimensions = do
     wv <- askWebView
     windowM <- liftIO $ getWindow wv

     case windowM of

        (Just window) -> do

          w <- liftIO $ getInnerWidth window
          h <- liftIO $ getInnerHeight window
          return (w, h)

        Nothing -> return (100, 100)
