{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Typography where

import           Control.Monad
import           Data.FileEmbed
import           Data.JSString.Text
import           Data.List (intersperse)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Text.Hyphenation
import qualified JavaScript.JQuery as JQ hiding (filter, not)

toItem :: T.Text -> IO (Item JQ.JQuery Double)
toItem "-" = hyphen 0 <$> JQ.select "<span>-</span>"
toItem " " = space spaceWidth <$> (styleSpace spaceWidth =<< JQ.select "<span>&nbsp;</span>")
toItem str = Box 0 <$> JQ.select ("<span>" <> textToJSString str <> "</span>")

-- can this be a monoid?
data Item a b = Box b a             -- Box w_i
              | Spring b b b a      -- Spring w_i y_i z_i
              | Penalty b b Bool a  -- Penalty w_i p_i f_i

-- all hypens are flagged penality items because we don't want two hyphens in
-- a row

itemIsSpring :: Item a b -> Bool
itemIsSpring Spring{} = True
itemIsSpring _ = False

itemWidth :: Item a b -> b
itemWidth (Box w _) = w
itemWidth (Spring w _ _ _) = w
itemWidth (Penalty w _ _ _) = w

setItemWidth :: b -> Item a b -> Item a b
setItemWidth w (Box _ a) = Box w a
setItemWidth w (Spring _ a b c) = Spring w a b c
setItemWidth w (Penalty _ a b c) = Penalty w a b c

itemElement :: Item a b -> a
itemElement (Box _ e) = e
itemElement (Spring _ _ _ e) = e
itemElement (Penalty _ _ _ e) = e

spaceWidth :: Double
spaceWidth = 5

space :: Double -> JQ.JQuery -> Item JQ.JQuery Double
space w = Spring w 3 2

styleSpace :: Double -> JQ.JQuery -> IO JQ.JQuery
styleSpace width = JQ.setCss "display" "inline-block" <=< JQ.setCss "width" (textToJSString . T.pack $ show width)

hyphen :: Double -> JQ.JQuery -> Item JQ.JQuery Double
hyphen hyphenWidth = Penalty hyphenWidth 2 True

textWidth :: Int
textWidth = 600 -- TODO this will eventually have to be dynamic


removeSpacesFromEnds :: forall a b. [Item a b] -> [Item a b]
removeSpacesFromEnds [] = []
removeSpacesFromEnds (h : t)
    | itemIsSpring h = removeLastP t
    | otherwise    = h : removeLastP t
    where removeLastP r
               | not (null r) && itemIsSpring (Prelude.last r) = init r
               | otherwise             = r

boustro :: [JQ.JQuery] -> IO [JQ.JQuery]
boustro [] = return []
boustro [l] = return [l]
boustro (l:l2:ls) = do ho <- reverseLine l2
                       fi <- boustro ls
                       return (l : ho : fi)


accumLines :: (Ord b, Num b) => Item a b -> [[Item a b]] -> [[Item a b]]
accumLines i@(Penalty _ nextWidth _ _) p@(l:_)
    | (lineLength l + nextWidth) > fromIntegral textWidth = [ i ] : p
    | otherwise                                           = p
  where lineLength = sum . fmap itemWidth
accumLines item p@(l:ls)
    | (lineLength l + itemWidth item) > fromIntegral textWidth = [ item ] : p
    | otherwise                                                = (item : l) : ls
  where lineLength = sum . fmap itemWidth
accumLines _ [] = []


renderLine :: [Item JQ.JQuery Double] -> IO JQ.JQuery
renderLine ls = do lineDiv <- JQ.select "<div></div>" >>= JQ.setCss "width" (textToJSString . T.pack $ show textWidth)
                                                   >>= JQ.setCss "white-space" "nowrap"
                   nls <- mapM convertSpace ls
                   mapM_ (\i -> (`JQ.appendJQuery` lineDiv) <=< styleSpace (itemWidth i) $ itemElement i) nls
                   return lineDiv
    where
      filteredLs = filter (not . itemIsSpring) ls
      totalLength = sum . fmap itemWidth $ filteredLs
      spaceSize :: Double
      spaceSize = realToFrac $ (fromIntegral textWidth - totalLength) / (fromIntegral . length $ filter itemIsSpring ls)
      convertSpace :: Item JQ.JQuery Double -> IO (Item JQ.JQuery Double)
      convertSpace e
        | itemIsSpring e = space spaceSize <$> JQ.select "<span>&nbsp;</span>"
        | otherwise      = return e


reverseLine :: JQ.JQuery -> IO JQ.JQuery
reverseLine = JQ.setCss "-moz-transform" "scaleX(-1)" <=<
              JQ.setCss "-o-transform"  "scaleX(-1)" <=<
              JQ.setCss "-webkit-transform" "scaleX(-1)" <=<
              JQ.setCss "transform" "scaleX(-1)" <=<
              JQ.setCss "filter" "FlipH" <=<
              JQ.setCss "-ms-filter" "\"FlipH\""

hyphenString :: String
hyphenString = "-"

nonBreakingHypenString :: String
nonBreakingHypenString = "‑"

preprocess :: String -> [String]
preprocess = prepareText
  where
    insertHyphens = concatMap ((++ [" "]) . intersperse hyphenString . hyphenate english_US)
    insertPilcrows = concatMap (\x -> if x == '\n' then " ¶ " else [x])
    prepareText = insertHyphens . words . replace . insertPilcrows
    replace = concatMap (\x -> if x == '-' then nonBreakingHypenString else [x])

processedWords :: [String]
processedWords = preprocess contextText

contextText :: String
contextText = $(embedStringFile "texts/middlemarch.txt")
