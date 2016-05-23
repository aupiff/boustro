{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Typography where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.FileEmbed
import           Data.JSString.Text
import           Data.List (intersperse)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import           Prelude hiding (Word)
import           Text.Hyphenation

type Word = Item JQ.JQuery Double
type Txt = [Word]
type Line = [Word]
type Paragraph = [Line]


fold1 :: forall a b. (a -> b -> b) -> (a -> b) -> [a] -> b
fold1 _ g [x] = g x
fold1 f g (x:xs) = f x (fold1 f g xs)


minWith :: Ord b => (a -> b) -> [a] -> a
minWith f = fold1 choice id
    where choice a b
             | f a < f b = a
             | otherwise = b


textWidth :: Double
textWidth = 600 -- TODO this will eventually have to be dynamic


optTextWidth :: Double
optTextWidth = 550


-- TODO Should I keep this 'Int'?
width :: Line -> Double
width = sum . map itemWidth

lineWaste :: Line -> Double
lineWaste l = numSpaces * (spaceSize / numSpaces - spaceWidth) ^ 2
    where spaceSize = textWidth - width l
          numSpaces = fromIntegral $ length l - 1


par1' :: Txt -> Paragraph
par1' = parLines . minWith waste . fold1 step start
    where
        step w ps = filter fitH (new w (minWith waste ps) : map (glue w) ps)
        start w   = filter fitH [([[w]], itemWidth w, 0)]
        new w ([l], _, 0)  = ([w]:[l], itemWidth w, 0)
        new w p@(ls, _, _) = ([w]:ls, itemWidth w, waste p)
        glue w (l:ls, n, m) = ((w:l):ls, itemWidth w + n, m) -- TODO what is this 1 about? space width? change this
        parLines (ls, _, _) = ls
        widthHead (_, n, _) = n
        wasteTail (_, _, m) = m
        linwHead p@(l:ls, _, _) = lineWaste l ^ (2 :: Int)
        waste ([_], _, _) = 0
        waste p = linwHead p + wasteTail p
        fitH p = widthHead p <= textWidth


typesetPage :: forall (m :: * -> *). MonadIO m => Int -> m ()
typesetPage p = do
            wordBoxes <- liftIO . wordsWithWidths . take 500 . drop (500 * p) $ processedWords
            liftIO $ arrangeBoustro wordBoxes


wordsWithWidths :: [String] -> IO [Item JQ.JQuery Double]
wordsWithWidths inputWords = do

     ws <- mapM (toItem . T.pack) inputWords

     -- creating a temporary div specifically to measure the width of every element
     scratchArea <- JQ.empty =<< JQ.select "#scratch-area"
     mapM_ (`JQ.appendJQuery` scratchArea) $ fmap itemElement ws
     reverse <$> foldM func [] ws

     where func (Penalty w _ flag a : ls) i = do
                iWidth <- JQ.getInnerWidth (itemElement i)
                return $ setItemWidth iWidth i : Penalty w iWidth flag a : ls
           func p i = (:) <$> ((`setItemWidth` i) <$> JQ.getInnerWidth (itemElement i))
                          <*> return p


arrangeBoustro :: [Item JQ.JQuery Double] -> IO ()
arrangeBoustro boxes = do
    ls <- mapM renderLine $ par1' boxes
    -- Should I be applying this style every time? Definitely on window change
    -- dim, so maybe it's not so bad.
    textArea <- JQ.select "#boustro" >>= (JQ.empty >=> widthCss)
    mapM_ (`JQ.appendJQuery` textArea) =<< boustro ls
    where widthCss = JQ.setCss "width" (textToJSString . T.pack $ show textWidth)


boustro :: [JQ.JQuery] -> IO [JQ.JQuery]
boustro [] = return []
boustro [l] = return [l]
boustro (l:l2:ls) = do ho <- reverseLine l2
                       fi <- boustro ls
                       return (l : ho : fi)


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

--TODO rename this
styleSpace :: Double -> JQ.JQuery -> IO JQ.JQuery
styleSpace txtWidth =
        JQ.setCss "display" "inline-block"
    <=< JQ.setCss "width" (textToJSString . T.pack $ show txtWidth)


hyphen :: Double -> JQ.JQuery -> Item JQ.JQuery Double
hyphen hyphenWidth = Penalty hyphenWidth 2 True


renderLine :: [Item JQ.JQuery Double] -> IO JQ.JQuery
renderLine ls = do lineDiv <- JQ.select "<div></div>" >>= JQ.setCss "width" (textToJSString . T.pack $ show textWidth)
                                                      >>= JQ.setCss "white-space" "nowrap"
                   nls <- sequence . intersperse (space spaceSize <$> JQ.select "<span>&nbsp;</span>") $ map return ls
                   mapM_ (\i -> (`JQ.appendJQuery` lineDiv) <=< styleSpace (itemWidth i) $ itemElement i) nls
                   return lineDiv
    where
      spaceSize :: Double
      spaceSize = realToFrac $ (textWidth - width ls) / fromIntegral (length ls - 1)

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
    insertHyphens = concatMap (intersperse hyphenString . hyphenate english_US)
    insertPilcrows = concatMap (\x -> if x == '\n' then " ¶ " else [x])
    prepareText = insertHyphens . words . replace . insertPilcrows
    replace = concatMap (\x -> if x == '-' then nonBreakingHypenString else [x])


processedWords :: [String]
processedWords = preprocess contextText


contextText :: String
contextText = $(embedStringFile "texts/middlemarch.txt")
