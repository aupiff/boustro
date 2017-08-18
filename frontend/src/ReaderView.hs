{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}

module ReaderView where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import           GHCJS.DOM (currentDocumentUnchecked) -- (askWebView)  -- (webViewGetDomDocument)
import           GHCJS.DOM.Window ( getInnerHeight, getInnerWidth )
--                                  , getWindow, resize)
import           GHCJS.DOM.EventM (on, preventDefault, uiView, uiKeyCode)
-- import           GHCJS.DOM.Element (keyDown)
import           GHCJS.DOM.Document (getBody)
import           Reflex.Dom.Class
import qualified Reflex.Dom as RD

import Style
import Typography


titlePage :: forall t (m :: * -> *). RD.MonadWidget t m
          => RD.Workflow t m ()
titlePage = RD.Workflow $ do

    viewDimsD <- viewDimensions

    contentStyleMap <- RD.mapDyn viewDimsToStyleMap viewDimsD

    pb <- RD.getPostBuild

    RD.elDynAttr "div" contentStyleMap $ do

          RD.el "h1" $ RD.text "βουστροφηδόν"

          RD.elAttr "div" (Map.singleton "id" "instruction") $ do

             RD.el "p" $

                RD.text "It is a strange fact that many ancient civilizations read not left-to-right or right-to-left but alternated between the two. The text below demonstrates the advantages of this ancient style of typesetting called Boustrophedon. To read the following paragraph, read the first line from left to right, then read the next line's mirrored text from right to left, and repeat this alternation of reading direction until you finish the text."

             RD.elAttr "div" (Map.singleton "id" "demo") $

                loadBoustro pb viewDimsD

             RD.elAttr "div" (Map.singleton "id" "menu") $ do

                readButton <- RD.button "READ MORE"

                return ((), selectionPage <$ readButton)

loadBoustro :: RD.MonadWidget t m
            => RD.Event t () -> RD.Dynamic t ViewDimensions
            -> m (RD.Event t ())
loadBoustro pageEvent viewDimsD = RD.performEvent $
        (liftIO . typesetParagraph) <$> viewDimsD `RD.attachDyn` pageEvent

selectionPage :: forall t (m :: * -> *). RD.MonadWidget t m
              => RD.Workflow t m ()
selectionPage = RD.Workflow $ do

    viewDimsD <- viewDimensions

    contentStyleMap <- RD.mapDyn viewDimsToStyleMap viewDimsD

    RD.elDynAttr "div" contentStyleMap $

      RD.elAttr "div" (Map.singleton "id" "instruction") $ do

               RD.el "p" $

                    RD.text "Select one of the works below to read. Works labelled \"ALL-CAPS\" are recommended for beginners."

               x <- RD.elAttr "div" (Map.singleton "id" "menu") $ do

                  one <- RD.button "\"The Velveteen Rabbit\" -- Margery Williams  (ALL-CAPS)"
                  twain <- RD.button "\"The The Celebrated Jumping Frog of Calaveras County\" -- Mark Twain (ALL-CAPS)"
                  two <- RD.button "\"Walden, Chapter III\" -- Henry David Thoreau"
                  three <- RD.button "\"The Man of the Crowd\" -- Edgar Allan Poe"
                  four <- RD.button "\"Eveline\" -- James Joyce"

                  let selectionEvent = RD.leftmost [ const 0 <$> one
                                                   , const 1 <$> twain
                                                   , const 2 <$> two
                                                   , const 3 <$> three
                                                   , const 4 <$> four
                                                   ]

                  return ((), textView <$> selectionEvent)

               RD.el "p" $ RD.text "To turn pages in the reader view, \
                                  \ use the left and right the arrows on \
                                  \ PCs or, if using a mobile device, single \
                                  \ click the left and right sides of the \
                                  \ screen. Additionally, paging and home \
                                  \ buttons are displayed at the bottom of \
                                  \ the screen."

               return x

textView :: forall (m :: * -> *) t. RD.MonadWidget t m
         => Int -> RD.Workflow t m ()
textView textIndex = RD.Workflow . RD.el "div" $ do

        viewDimsD <- viewDimensions
        contentStyleMap <- RD.mapDyn viewDimsToStyleMap viewDimsD
        pagingE <- pagingEvent

        RD.elDynAttr "div" contentStyleMap $ mdo

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
                                                  , const PrevPage <$> prev
                                                  , const NextPage <$> next
                                                  , textClick'
                                                  , const Resize <$> resizeE
                                                  ]

            rec wordDelta  <- pageEventResponse textIndex buildAndPagingEvent wordDeltaD viewDimsD
                wordDeltaD <- RD.holdDyn (0,0) wordDelta

            return ()

          (a, b, prev, next) <- RD.elAttr "div" ("id" =: "readerNav" <> style [("width", "100%"), ("height", "10%")]) $ RD.elAttr "div" ("id" =: "readerNavInner") $ do

            prev <- RD.button "<="
            home <- RD.button "o"
            next <- RD.button "=>"

            return ((), titlePage <$ home, prev, next)

          return (a, b)


viewDimensions :: forall (m :: * -> *) t. RD.MonadWidget t m
               => m (RD.Dynamic t ViewDimensions)
viewDimensions = do
    (w, h) <- windowDimensions
    lineH <- liftIO measureLineHeight
    wds <- windowDimensionsE
    RD.holdDyn (makeDims lineH w h) $ uncurry (makeDims lineH) <$> wds
    where makeDims lineH w h = let w' = min 700 $ fromIntegral w - 40
                                   h' = fromIntegral h - 8
                               in ViewDimensions w w' h' lineH


viewDimsToStyleMap :: ViewDimensions -> Map.Map Text Text
viewDimsToStyleMap (ViewDimensions _ textWidth fullHeight _) =
    "id" =: "content" <> style [ ("width", T.pack $ show textWidth)
                               , ("height", T.pack $ show fullHeight) ]


pageEventResponse :: RD.MonadWidget t m
                  => Int -> RD.Event t PageEvent -> RD.Dynamic t (Int, Int)
                  -> RD.Dynamic t ViewDimensions -> m (RD.Event t (Int, Int))
pageEventResponse textIndex pageEvent currentWord vd = RD.performEvent $

        (liftIO . typesetPage textIndex) <$> vd `RD.attachDyn` (currentWord `RD.attachDyn` pageEvent)


pagingEvent :: RD.MonadWidget t m => m (RD.Event t PageEvent)
pagingEvent = do

    --return RD.never

     -- wv <- askWebView
    -- bodyM <- liftIO $ currentDocumentUnchecked >>= maybe (return Nothing) getBody

    --case bodyM of

    --    Just body -> do
           kp <- uiKeyCode

           -- kp <- RD.wrapDomEvent body (`on` keyDown) $ do
           --   i <- RD.getKeyEvent
           --   preventDefault
           --   return i
           return $ RD.fmapMaybe toPageEvent kp
           -- return RD.never

    --     Nothing -> return RD.never

  where toPageEvent keyCode
           | keyCode == leftArrow  = Just PrevPage
           | keyCode == rightArrow = Just NextPage
           | otherwise             = Nothing
        rightArrow               = 39 :: Int
        leftArrow                = 37 :: Int


windowDimensionsE :: RD.MonadWidget t m => m (RD.Event t (Int, Int))
windowDimensionsE = do
     return RD.never
     -- wv <- askWebView
     -- windowM <- liftIO $ getWindow wv

     -- case windowM of

     --    Just window ->

     --        RD.wrapDomEvent window (`on` resize) $ do
     --          w <- liftIO $ getInnerWidth window
     --          h <- liftIO $ getInnerHeight window
     --          preventDefault
     --          return (w, h)

     --    Nothing -> return RD.never


windowDimensions :: RD.MonadWidget t m => m (Int, Int)
windowDimensions = do  return (100, 100)
     -- wv <- askWebView
     -- windowM <- liftIO $ getWindow wv

     -- case windowM of

     --    (Just window) -> do

     --      w <- liftIO $ getInnerWidth window
     --      h <- liftIO $ getInnerHeight window
     --      return (w, h)

     --    Nothing -> return (100, 100)
