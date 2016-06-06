{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Typography
    ( measureLineHeight
    , PageEvent(..)
    , ViewDimensions(..)
    , processedWords
    , typesetPage
    ) where

import           Control.Monad
import           Data.JSString.Text
import           Data.List (intersperse)
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import           Prelude hiding (Word)
import           Text.Hyphenation

import           Server

import           Debug.Trace

data PageEvent = NextPage | PrevPage | Start deriving Show

data ViewDimensions = ViewDimensions { fullWidth  :: Int
                                     , viewWidth  :: Double
                                     , veiwHeight :: Double
                                     , lineHeight :: Double
                                     }

type Word = Item T.Text Double
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


width :: Line -> Double
width l = sum (map itemWidth l)
-- bring this back in case of issues
--   where hyphenWidth = case last l of
--                            (Penalty w _ _ _) -> w
--                            _                 -> 0


lineWaste :: Double -> Line -> Double
lineWaste textWidth l = numSpaces * weighting (spaceSize / numSpaces - spaceWidth) + hyphenPenalty + hyphenHeadPenalty
    where spaceSize = textWidth - width l
          numSpaces :: Double
          numSpaces = fromIntegral . length $ filter itemIsBox l
          -- penalize placing hyphen at end of line
          hyphenPenalty = if itemIsPenalty (last l) then 10 else 0 -- what number should I use here?
                                                                   -- I just need to see what wastes look like generally
          -- disallow hyphens first
          hyphenHeadPenalty = if itemIsPenalty (head l) then 1000000 else 0
          weighting x -- too close is worse than too far apart : TODO This seems backwards to me
            | x < 0 = x ^ (2 :: Int) -- spaces larger than optimal width
            | otherwise =  3 * x ^ (2 :: Int) -- spaces smaller than optimal width
    -- Write quickcheck properties for this


par1' :: Double -> Txt -> Paragraph
par1' textWidth = parLines . fromMaybe (trace "par1 minWith" ([], 0, 0)) . minWith waste
                           . fromMaybe (trace "par1' fold1" []) . fold1 step start
    where
        step :: Word -> [(Paragraph, Double, Double)] -> [(Paragraph, Double, Double)]
        step w ps = let origin = (new w (fromMaybe (error $ "par1' step" ++ show ps) $ minWith waste ps) : map (glue w) ps)
                        result = filter fitH origin
                    in if null result then traceShow origin [fromMaybe (head origin) (minWith waste origin)] else result

        start :: Word -> [(Paragraph, Double, Double)]
        start w = [([[w]], itemWidth w, 0.0)]

        new w ([l], _, 0)  = ([w]:[l], itemWidth w, 0.0)
        new w p@(ls, _, _) = ([w]:ls, itemWidth w, waste p)
        -- TODO what is this 1/n about? space width? change this
        glue w (l:ls, n, m) = ((w:l):ls, itemWidth w + n, m)
        parLines (ls, _, _) = ls
        widthHead (_, n, _) = n
        wasteTail (_, _, m) = m
        linwHead = lineWaste textWidth . head . parLines
        waste ([_], _, _) = 0
        waste p = linwHead p + wasteTail p
        fitH p = widthHead p <= textWidth


typesetPage :: ViewDimensions -> ((Int, Int), PageEvent) -> IO (Int, Int)
typesetPage (ViewDimensions _ textWidth textHeight lineH) ((wordNumber, wordsOnPage), pageEvent) = do

    let linesPerPage = round $ textHeight / (lineH + 6) - 1
        numWords = round $ 30 * (textWidth / 700) * fromIntegral linesPerPage

    wordNumber' <- case pageEvent of

        NextPage -> return $ min (wordNumber + wordsOnPage)
                                 (length processedWords - 2)
        Start    -> return 0
        PrevPage -> do let boxify = wordsWithWidths . take numWords . reverse
                       boxesMeasure <- boxify $ take wordNumber processedWords
                       let parMeasure = take linesPerPage $ par1' textWidth boxesMeasure
                           wordsOnPageMeasure = sum $ map length parMeasure
                       return $ max 0 $ wordNumber - wordsOnPageMeasure

    boxes <- wordsWithWidths . take numWords . drop wordNumber' $ processedWords
    let par = take linesPerPage $ par1' textWidth boxes
        wordsOnPage' = sum $ map length par
    ls <- mapM (renderLine lineH textWidth) par

    boustroLines <- boustro ls
    -- Should I be applying this style every time? Definitely on window change
    -- dim, so maybe it's not so bad.
    textArea <- (JQ.empty >=> widthCss) =<< JQ.select "#boustro"
    mapM_ (`JQ.appendJQuery` textArea) boustroLines
    return (wordNumber', wordsOnPage')

      where widthCss = JQ.setCss "width" (textToJSString . T.pack $ show textWidth)


wordsWithWidths :: [String] -> IO [Word]
wordsWithWidths inputWords = do

     -- creating a temporary div specifically to measure the width of every element
     scratchArea <- JQ.empty =<< JQ.select "#scratch-area"

     mapM (\x -> do let t = T.pack x
                    jq <- toItem t
                    jq `JQ.appendJQuery` scratchArea
                    jqWidth <- JQ.getInnerWidth jq
                    return $ toItem' t jqWidth
                ) inputWords

  where toItem' :: T.Text -> Double -> Word
        toItem' "-" w = hyphen w "-"
        toItem' " " _ = space spaceWidth
        toItem' str w = Box w str

boustro :: [JQ.JQuery] -> IO [JQ.JQuery]
boustro [] = return []
boustro [l] = return [l]
boustro (l:l2:ls) = do ho <- reverseLine l2
                       fi <- boustro ls
                       return (l : ho : fi)


measureLineHeight :: IO Double
measureLineHeight = do
     scratchArea <- JQ.empty =<< JQ.select "#scratch-area"
     quux <- JQ.select ("<span>" <> textToJSString "Hail qQuuXX!" <> "</span>")
     _ <- quux `JQ.appendJQuery` scratchArea
     JQ.getInnerHeight quux


-- can this be a monoid?
data Item a b = Box b a             -- Box w_i
              | Spring b b b        -- Spring w_i y_i z_i
              | Penalty b b Bool a  -- Penalty w_i p_i f_i

instance Show b => Show (Item a b) where
    show (Box w _) = "Box " ++ show w
    show (Spring w _ _) = "Spring " ++ show w
    show (Penalty w _ _ _) = "Penalty " ++ show w

-- all hypens are flagged penality items because we don't want two hyphens in
-- a row
itemWidth :: Num b => Item a b -> b
itemWidth (Box w _) = w
itemWidth (Spring w _ _) = w
-- itemWidth (Penalty w _ _ _) = w
itemWidth Penalty{} = 0


itemWidth' :: Num b => Item a b -> b
itemWidth' (Box w _) = w
itemWidth' (Spring w _ _) = w
itemWidth' (Penalty w _ _ _) = w


toItem :: T.Text -> IO JQ.JQuery
toItem "-" = JQ.select "<span>-</span>"
toItem " " = assignCssWidth spaceWidth =<< JQ.select "<span>&nbsp;</span>"
toItem str = JQ.select ("<span>" <> textToJSString str <> "</span>")


itemElement :: Word -> IO JQ.JQuery
itemElement (Box _ e) = JQ.select ("<span>" <> textToJSString e <> "</span>")
itemElement Spring{} = JQ.select "<span>&nbsp;</span>"
itemElement Penalty{} = JQ.select "<span>-</span>"

itemIsSpace :: forall t s . Item t s -> Bool
itemIsSpace Spring{} = True
itemIsSpace _ = False


itemIsBox :: forall t s . Item t s -> Bool
itemIsBox Box{} = True
itemIsBox _ = False


itemIsPenalty :: forall t s . Item t s -> Bool
itemIsPenalty Penalty{} = True
itemIsPenalty _ = False

spaceWidth :: Double
spaceWidth = 6


space :: Double -> Word
space w = Spring w 3 2


--TODO rename this
assignCssWidth :: Double -> JQ.JQuery -> IO JQ.JQuery
assignCssWidth txtWidth =
        JQ.setCss "display" "inline-block"
    <=< JQ.setCss "width" (textToJSString . T.pack $ show txtWidth)


hyphen :: Double -> T.Text -> Word
hyphen hyphenWidth = Penalty hyphenWidth penaltyValue False
    where penaltyValue = undefined -- TODO I may not end up using this


renderLine :: Double -> Double -> [Word] -> IO JQ.JQuery
renderLine lineH textW ls = do
    lineDiv <- JQ.select "<div class='line'></div>"
                 >>= JQ.setCss "width" (textToJSString . T.pack $ show textW)
                 >>= JQ.setCss "height" (textToJSString . T.pack $ show lineH)
                 >>= JQ.setCss "white-space" "nowrap"
    let nls = foldr dehyphen [] ls
        numSpaces = fromIntegral (length $ filter itemIsSpace nls)
        spaceSize = realToFrac $ (textW - sum (fmap itemWidth' nls)) / numSpaces
        nls' = map (\x -> case x of
                             (Spring _ a b) -> Spring spaceSize a b
                             _ -> x) nls
    mapM_ ((`JQ.appendJQuery` lineDiv) <=< toJQueryWithWidth) nls'
    return lineDiv

    where
      toJQueryWithWidth i = assignCssWidth (itemWidth' i) =<< itemElement i
      dehyphen :: Word -> [Word] -> [Word]
      dehyphen n [] = [n]
      dehyphen n@(Box{}) p =
                        let sp = space 0
                        in case head p of
                            Box{} -> n : sp : p
                            Penalty{} -> case tail p of
                                             (Box{}:_) -> n : tail p
                                             _         -> n : p
      dehyphen n@(Penalty{}) p = n : p
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
