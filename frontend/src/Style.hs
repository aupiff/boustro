module Style where

import           Data.Monoid ((<>))
import           Data.Map.Strict
import           Reflex.Dom ((=:))

type Style = Map String String

style :: [(String, String)] -> Style
style ls = "style" =: styleValues
  where styleValues = concatMap (\(x, y) -> x ++ ": " ++ y ++ ";") ls

fontSize :: Double
fontSize = 22
