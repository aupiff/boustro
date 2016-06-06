{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Data.Monoid ((<>))
import           Control.Monad.IO.Class
import           Data.FileEmbed
import qualified Data.Map.Strict as Map
import           GHCJS.DOM (webViewGetDomDocument)
import           GHCJS.DOM.Window ( getOuterHeight, getOuterWidth
                                  , getWindow, resize)
import           GHCJS.DOM.EventM (on, preventDefault)
import           GHCJS.DOM.Element (keyDown)
import           GHCJS.DOM.Document (getBody)
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import           Reflex
import qualified Reflex.Dom as RD

import           ReaderView

main :: IO ()
main = JQ.ready $ RD.mainWidgetWithCss $(embedStringFile "app/Boustro.css") $ do

    -- `scratch-area` is a hidden div where words widths are measured
    RD.elAttr "div" (Map.singleton "id" "scratch-area") $ return ()

    void . RD.workflow $ titlePage
