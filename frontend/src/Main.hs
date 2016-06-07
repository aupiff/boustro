{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Data.FileEmbed
import qualified Data.Map.Strict as Map
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import qualified Reflex.Dom as RD

import           ReaderView

main :: IO ()
main = JQ.ready $ RD.mainWidgetWithCss $(embedStringFile "app/Boustro.css") $ do

    -- `scratch-area` is a hidden div where words widths are measured
    RD.elAttr "div" (Map.singleton "id" "scratch-area") $ return ()

    void . RD.workflow $ titlePage
