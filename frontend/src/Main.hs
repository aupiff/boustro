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
import           Reflex
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import qualified Reflex.Dom as RD

import           ReaderView
import           Style

main :: IO ()
main = JQ.ready $ RD.mainWidgetWithCss $(embedStringFile "app/Boustro.css") $ do

    (_, _, r) <- windowDimensions

    RD.elAttr "div" (style [("font-size", show (r * fontSize) ++ "px")]) $ do

        -- `scratch-area` is a hidden div where words widths are measured
        RD.elAttr "div" (Map.singleton "id" "scratch-area") $ return ()

        void . RD.workflow $ titlePage
