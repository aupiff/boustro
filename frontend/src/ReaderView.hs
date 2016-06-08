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


titlePage :: forall t (m :: * -> *).  MonadWidget t m => RD.Workflow t m ()
titlePage = RD.Workflow $ do

    (w, h) <- windowDimensions
    lineHeight <- liftIO measureLineHeight
    let vd = viewDims lineHeight w h

    RD.elAttr "div" ("id" =: "content" <> style [ ("width", show $ viewWidth vd)
                                                , ("height", show $ viewHeight vd)]) $

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

                return $ ((), textView <$ showTextView)


textView :: forall (m :: * -> *) t.  MonadWidget t m
         => RD.Workflow t m ()
textView = RD.Workflow . RD.el "div" $ do

        dims@(w, h) <- windowDimensions
        lineHeight <- liftIO measureLineHeight
        wds <- windowDimensionsE
        viewDimsB <- RD.hold (viewDims lineHeight w h) $ uncurry (viewDims lineHeight) <$> wds

        pagingE <- pagingEvent

        vd@(ViewDimensions fullWidth textWidth fullHeight lineHeight) <- RD.sample viewDimsB

        RD.elAttr "div" ("id" =: "content" <> style [ ("width", show textWidth)
                                                    , ("height", show fullHeight)]) $ do

          RD.elAttr "div" ("id" =: "b" <> style [("width", show textWidth), ("height", show $ 0.9 * fullHeight)]) $ do

            (boustroEl, _) <- RD.elAttr' "div" (Map.singleton "id" "boustro") $ return ()

            pb <- RD.getPostBuild
            let textClick = RD.domEvent RD.Mouseup boustroEl
                textTransform = (\x -> if x > div fullWidth 2 then NextPage else PrevPage) . fst
                buildAndPagingEvent = RD.leftmost [ fmap (const Start) pb
                                                  , pagingE
                                                  , fmap textTransform textClick
                                                  , fmap (const Resize) wds
                                                  ]

            rec wordDelta  <- pageEventResponse buildAndPagingEvent wordDeltaD viewDimsB
                wordDeltaD <- RD.holdDyn (0,0) wordDelta

            return ()

          RD.elAttr "div" ("id" =: "back" <> style [("width", show textWidth), ("height", show $ 0.1 * fullHeight)]) $ do

            home <- RD.button "<="

            return $ ((), titlePage <$ home)


pageEventResponse :: MonadWidget t m
                  => RD.Event t PageEvent -> RD.Dynamic t (Int, Int)
                  -> RD.Behavior t ViewDimensions -> m (RD.Event t (Int, Int))
pageEventResponse pageEvent currentWord vd = RD.performEvent $

        (liftIO . typesetPage) <$> vd `RD.attach` (currentWord `RD.attachDyn` pageEvent)


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


viewDims lineHeight w h = let w' = min 700 $ fromIntegral w - 40
                              h' = fromIntegral h
                          in ViewDimensions w w' h' lineHeight


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
