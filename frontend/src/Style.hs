{-# LANGUAGE OverloadedStrings #-}

module Style where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           Data.Map.Strict
import           Reflex.Dom ((=:))

type Style = Map Text Text

style :: [(Text, Text)] -> Style
style ls = "style" =: styleValues
  where styleValues = T.concat $ (\(x, y) -> T.concat [x , ": " , y , ";"]) <$> ls
