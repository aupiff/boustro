{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Default
import           Data.IORef
import           Data.JSString.Text
import           Data.List (intersperse)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.JSString as S
import           Data.JSString.Text as S (textToJSString, textFromJSString)
import           JavaScript.JQuery hiding (filter, not)
import           Text.Hyphenation

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

spaceWidth = 5

space :: Double -> JQuery -> Item JQuery Double
space w = Spring w 3 2

styleSpace :: Double -> JQuery -> IO JQuery
styleSpace width = setCss "display" "inline-block" <=< setCss "width" (textToJSString . T.pack $ show width)

hyphen :: Double -> JQuery -> Item JQuery Double
hyphen hyphenWidth = Penalty hyphenWidth 2 True

textWidth :: Int
textWidth = 600

main :: IO ()
main = ready $ do

          let processedWords = preprocess text
              processedText = textToJSString $ T.unwords processedWords
              toItem :: T.Text -> IO (Item JQuery Double)
              toItem "-" = hyphen 0 <$> select "<span>-</span>"
              toItem " " = space spaceWidth <$> (styleSpace spaceWidth =<< select "<span>&nbsp;</span>")
              toItem str = Box 0 <$> select ("<span>" <> textToJSString str <> "</span>")

          words <- mapM toItem processedWords
          scratchArea <- select "#scratch-area" >>= setCss "width" (textToJSString . T.pack $ show 0)
          mapM_ (`appendJQuery` scratchArea) $ fmap itemElement words

          boxes <- reverse <$> foldM func [] words
          lines <- mapM renderLine (removeSpacesFromEnds <$> foldr accumLines [[]] boxes)
          textArea <- select "#text-area" >>= setCss "width" (textToJSString . T.pack $ show textWidth)
          mapM_ (`appendJQuery` textArea) =<< boustro lines
          return ()
    where func p@(Penalty w _ flag a : ls) i = do
               width <- getInnerWidth (itemElement i)
               return $ setItemWidth width i : Penalty w width flag a : ls
          func p i = do n <- (\w -> return $ setItemWidth w i) =<< getInnerWidth (itemElement i)
                        return $ n : p

-- removeSpacesFromEnds :: [Item _ _] -> [Item _ _]
removeSpacesFromEnds (h : t)
    | itemIsSpring h = removeLastP t
    | otherwise    = h : removeLastP t
    where removeLastP r
               | itemIsSpring (Prelude.last r) = init r
               | otherwise             = r

boustro :: [JQuery] -> IO [JQuery]
boustro [] = return []
boustro [l] = return [l]
boustro (l:l2:ls) = do ho <- reverseLine l2
                       fi <- boustro ls
                       return (l : ho : fi)


accumLines :: (Ord b, Num b) => Item a b -> [[Item a b]] -> [[Item a b]]
accumLines i@(Penalty w nextWidth flag _) p@(l:_)
    | (lineLength l + nextWidth) > fromIntegral textWidth = [ i ] : p
    | otherwise                                           = p
  where lineLength = sum . fmap itemWidth
accumLines item p@(l:ls)
    | (lineLength l + itemWidth item) > fromIntegral textWidth = [ item ] : p
    | otherwise                                                = (item : l) : ls
  where lineLength = sum . fmap itemWidth


renderLine :: [Item JQuery Double] -> IO JQuery
renderLine ls = do lineDiv <- select "<div></div>" >>= setCss "width" (textToJSString . T.pack $ show textWidth)
                                                   >>= setCss "white-space" "nowrap"
                   nls <- mapM convertSpace ls
                   mapM_ (\i -> (`appendJQuery` lineDiv) <=< styleSpace (itemWidth i) $ itemElement i) nls
                   return lineDiv
    where
      filteredLs = filter (not . itemIsSpring) ls
      totalLength = sum . fmap itemWidth $ filteredLs
      spaceSize :: Double
      spaceSize = realToFrac $ (fromIntegral textWidth - totalLength) / (fromIntegral . length $ filter itemIsSpring ls)
      convertSpace :: Item JQuery Double -> IO (Item JQuery Double)
      convertSpace e
        | itemIsSpring e = space spaceSize <$> (styleSpace spaceSize =<< select "<span>&nbsp;</span>")
        | otherwise      = return e


reverseLine = setCss "-moz-transform" "scaleX(-1)" <=<
              setCss "-o-transform"  "scaleX(-1)" <=<
              setCss "-webkit-transform" "scaleX(-1)" <=<
              setCss "transform" "scaleX(-1)" <=<
              setCss "filter" "FlipH" <=<
              setCss "-ms-filter" "\"FlipH\""

hyphenString :: String
hyphenString = "-"

preprocess :: S.JSString -> [T.Text]
preprocess = prepareText . textFromJSString
  where
    insertHyphens = concatMap ((++ [" "]) . intersperse hyphenString . hyphenate english_US)
    insertPilcrows = concatMap (\x -> if x == '\n' then " ¶ " else [x])
    prepareText = fmap T.pack . insertHyphens . words . insertPilcrows . T.unpack


text :: S.JSString
text = "The Roots of Honour\nJohn Ruskin\nAmong the delusions which at different periods have possessed themselves of the minds of large masses of the human race, perhaps the most curious -- certainly the least creditable -- is the modern soi-disant science of political economy, based on the idea that an advantageous code of social action may be determined irrespectively of the influence of social affection.\nOf course, as in the instances of alchemy, astrology, witchcraft, and other such popular creeds, political economy, has a plausible idea at the root of it. \"The social affections,\" says the economist, \"are accidental and disturbing elements in human nature; but avarice and the desire of progress are constant elements. Let us eliminate the inconstants, and, considering the human being merely as a covetous machine, examine by what laws of labour, purchase, and sale, the greatest accumulative result in wealth is obtainable. Those laws once determined, it will be for each individual afterwards to introduce as much of the disturbing affectionate element as he chooses, and to determine for himself the result on the new conditions supposed.\""
