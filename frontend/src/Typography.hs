{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Typography
    ( typesetPage
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.FileEmbed
import           Data.JSString.Text
import           Data.List (intersperse)
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import           Prelude hiding (Word)
import           Text.Hyphenation

import           Debug.Trace

type Word = Item JQ.JQuery Double
type Txt = [Word]
type Line = [Word]
type Paragraph = [Line]


fold1 :: forall a b. (a -> b -> b) -> (a -> b) -> [a] -> Maybe b
fold1 _ _ [] = Nothing
fold1 _ g [x] = Just $ g x
fold1 f g (x:xs) = f x <$> fold1 f g xs


minWith :: Ord b => (a -> b) -> [a] -> Maybe a
minWith f = fold1 choice id
    where choice a b
             | f a < f b = a
             | otherwise = b


textWidth :: Double
textWidth = 600 -- TODO this will eventually have to be dynamic


-- TODO Should I keep this 'Int'?
width :: Line -> Double
width = sum . map itemWidth

lineWaste :: Line -> Double
lineWaste l = numSpaces * (spaceSize / numSpaces - spaceWidth) ^ (2 :: Int)
    where spaceSize = textWidth - width l
          numSpaces = fromIntegral $ length l - 1


par1' :: Txt -> Paragraph
par1' = parLines . fromMaybe (error "par1 minWith") . minWith waste . fromMaybe (error "par1' fold1") . fold1 step start
    where
        step :: Word -> [(Paragraph, Double, Double)] -> [(Paragraph, Double, Double)]
        step w ps = let origin = (new w (fromMaybe (error $ "par1' step" ++ show ps) $ minWith waste ps) : map (glue w) ps)
                        result = filter fitH origin
                    in if null result then traceShow origin [fromMaybe (head origin) (minWith waste origin)] else result

        start :: Word -> [(Paragraph, Double, Double)]
        start w = [([[w]], itemWidth w, 0.0)]

        new w ([l], _, 0)  = ([w]:[l], itemWidth w, 0.0)
        new w p@(ls, _, _) = ([w]:ls, itemWidth w, waste p)
        glue w (l:ls, n, m) = ((w:l):ls, itemWidth w + n, m) -- TODO what is this 1 about? space width? change this
        parLines (ls, _, _) = ls
        widthHead (_, n, _) = n
        wasteTail (_, _, m) = m
        linwHead = lineWaste . head . parLines
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
     mapM (\i -> (`setItemWidth` i) <$> JQ.getInnerWidth (itemElement i)) ws


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

instance Show b => Show (Item a b) where
    show (Box w _) = "Box " ++ show w
    show (Spring w _ _ _) = "Spring " ++ show w
    show (Penalty w _ _ _) = "Penalty " ++ show w

-- all hypens are flagged penality items because we don't want two hyphens in
-- a row


itemWidth :: Num b => Item a b -> b
itemWidth (Box w _) = w
itemWidth (Spring w _ _ _) = w
itemWidth (Penalty w _ True _) = w
itemWidth (Penalty _ _ False _) = 0


itemWidth' :: Num b => Item a b -> b
itemWidth' (Box w _) = w
itemWidth' (Spring w _ _ _) = w
itemWidth' (Penalty w _ _ _) = w


setItemWidth :: b -> Item a b -> Item a b
setItemWidth w (Box _ a) = Box w a
setItemWidth w (Spring _ a b c) = Spring w a b c
setItemWidth w (Penalty _ a b c) = Penalty w a b c


itemElement :: Item a b -> a
itemElement (Box _ e) = e
itemElement (Spring _ _ _ e) = e
itemElement (Penalty _ _ _ e) = e


itemIsSpace Spring{} = True
itemIsSpace _ = False


spaceWidth :: Double
spaceWidth = 4


space :: Double -> JQ.JQuery -> Item JQ.JQuery Double
space w = Spring w 3 2


--TODO rename this
styleSpace :: Double -> JQ.JQuery -> IO JQ.JQuery
styleSpace txtWidth =
        JQ.setCss "display" "inline-block"
    <=< JQ.setCss "width" (textToJSString . T.pack $ show txtWidth)


hyphen :: Double -> JQ.JQuery -> Item JQ.JQuery Double
hyphen hyphenWidth = Penalty hyphenWidth penaltyValue False
    where penaltyValue = undefined -- TODO I may not end up using this


renderLine :: [Word] -> IO JQ.JQuery
renderLine ls = do lineDiv <- JQ.select "<div></div>" >>= JQ.setCss "width" (textToJSString . T.pack $ show textWidth)
                                                      >>= JQ.setCss "white-space" "nowrap"
                   nls <- fromMaybe (error "renderLine fold1") $ fold1 dehyphen (\x -> return [x]) ls
                   let spaceSize = realToFrac $ (textWidth - sum (fmap itemWidth' nls)) / fromIntegral (length $ filter itemIsSpace nls)
                       nls' = map (\x -> case x of
                                            (Spring _ a b e) -> Spring spaceSize a b e
                                            _ -> x) nls
                   mapM_ (\i -> (`JQ.appendJQuery` lineDiv) <=< styleSpace (itemWidth' i) $ itemElement i) nls'
                   return lineDiv
    where
      dehyphen :: Word -> IO [Word] -> IO [Word]
      dehyphen n@(Box{}) p = do
                        p' <- p
                        sp <- space 0 <$> JQ.select "<span>&nbsp;</span>"
                        case head p' of
                           Box{} -> return $ n : sp : p'
                           Penalty{} -> case tail p' of
                                            (Box{}:_) -> return $ n : tail p'
                                            _         -> return $ n : p'
      dehyphen (Penalty a b _ d) p = (Penalty a b True d :) <$>  p
      dehyphen _ p = p



reverseLine :: JQ.JQuery -> IO JQ.JQuery
reverseLine = JQ.setCss "-moz-transform" "scaleX(-1)" <=<
              JQ.setCss "-o-transform"  "scaleX(-1)" <=<
              JQ.setCss "-webkit-transform" "scaleX(-1)" <=<
              JQ.setCss "transform" "scaleX(-1)" <=<
              JQ.setCss "filter" "FlipH" <=<
              JQ.setCss "-ms-filter" "\"FlipH\""


hyphenString :: String
hyphenString = "-"


nonBreakingHypenString :: Char
nonBreakingHypenString = '‑'

emdash :: Char
emdash = '—'

preprocess :: String -> [String]
preprocess = prepareText
  where
    insertHyphens x
       | nonBreakingHypenString `elem` x = [x]
       | otherwise = intersperse hyphenString $ hyphenate english_US x
    insertPilcrows = concatMap (\x -> if x == '\n' then " ¶ " else [x])
    prepareText = concatMap insertHyphens . words . replace . insertPilcrows
    replace = map (\x -> if x == '-' || x == emdash then nonBreakingHypenString else x)


processedWords :: [String]
processedWords = preprocess contextText


contextText :: String
contextText = $(embedStringFile "texts/middlemarch.txt")
