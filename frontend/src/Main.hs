{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import           Data.FileEmbed

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.JSString.Text
import           Data.String
import           Data.List (intersperse)
import           Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified JavaScript.JQuery as JQ hiding (filter, not)
import           Text.Hyphenation
import           GHCJS.DOM.EventM (on, preventDefault)
import           GHCJS.DOM.Element (keyDown)

import GHCJS.DOM (webViewGetDomDocument)
import GHCJS.DOM.Document (getBody)

import Reflex.Dom.Class

import qualified Reflex as R
import qualified Reflex.Dom as RD

wordsWithWidths :: [String] -> IO [Item JQ.JQuery Double]
wordsWithWidths inputWords = do

     ws <- mapM (toItem . T.pack) inputWords

     -- creating a temporary div specifically to measure the width of every element
     scratchArea <- JQ.select "#scratch-area"
     mapM_ (`JQ.appendJQuery` scratchArea) $ fmap itemElement ws
     reverse <$> foldM func [] ws

     where toItem :: T.Text -> IO (Item JQ.JQuery Double)
           toItem "-" = hyphen 0 <$> JQ.select "<span>-</span>"
           toItem " " = space spaceWidth <$> (styleSpace spaceWidth =<< JQ.select "<span>&nbsp;</span>")
           toItem str = Box 0 <$> JQ.select ("<span>" <> textToJSString str <> "</span>")

           func (Penalty w _ flag a : ls) i = do
                width <- JQ.getInnerWidth (itemElement i)
                return $ setItemWidth width i : Penalty w width flag a : ls
           func p i = do n <- (\w -> return $ setItemWidth w i) =<< JQ.getInnerWidth (itemElement i)
                         return $ n : p

arrangeBoustro :: [Item JQ.JQuery Double] -> IO ()
arrangeBoustro boxes = do
    ls <- mapM renderLine (removeSpacesFromEnds <$> foldr accumLines [[]] boxes)
    textArea <- JQ.select "#boustro" >>= JQ.setCss "width" (textToJSString . T.pack $ show textWidth)
    mapM_ (`JQ.appendJQuery` textArea) =<< boustro ls

data PageEvent = NextPage | PrevPage | Start deriving Show

data TextViewModel = TextViewModel { fullText :: String
                                   , wordIndex :: Int
                                   , pageWordCount :: Int
                                   }

pagingD :: MonadWidget t m => m (RD.Dynamic t PageEvent)
pagingD = do
     wv <- askWebView
     Just doc <- liftIO $ webViewGetDomDocument wv
     Just body <- liftIO $ getBody doc
     kp <- RD.wrapDomEvent body (`on` keyDown) $ do
       i <- RD.getKeyEvent
       preventDefault
       return i
     RD.foldDynMaybe toPageEvent Start kp
  where toPageEvent 37 _ = Just PrevPage
        toPageEvent 39 _ = Just NextPage
        toPageEvent _  _ = Nothing

main :: IO ()
main = JQ.ready $ RD.mainWidgetWithCss $(embedStringFile "app/Boustro.css") $

    RD.el "div" $ do
        pagingDyn <- R.updated <$> pagingD

        _ <- RD.workflow (titlePage pagingDyn)

        -- TODO git rid of this if possible
        RD.elAttr "div" (Map.singleton "id" "scratch-area") RD.blank


-- TODO if I want these pages to both have access to pagingDyn, I could throw
-- them in a reader monad, couldn't I?
titlePage pagingDyn = RD.Workflow . RD.el "div" $ do
  RD.el "div" $ RD.text "This is a boustrophedon reading application"
  pg2 <- RD.button "Start reading \"Middlemarch\" by George Elliot"
  return ("Page 1", textView pagingDyn <$ pg2)


textView :: forall a (m :: * -> *) a1 t. (Data.String.IsString a1, MonadWidget t m)
         => RD.Event t PageEvent -> RD.Workflow t m a1
textView pagingDyn = RD.Workflow . RD.el "div" $ do

    RD.elAttr "div" (Map.singleton "id" "boustro") RD.blank

    currentPage <- R.updated <$> RD.foldDyn pagingFunction 0 pagingDyn

    RD.performEvent_ $ RD.ffor currentPage (\p -> do
            wordBoxes <- liftIO . wordsWithWidths . take 200 . drop (200 * p) $ processedWords
            liftIO $ arrangeBoustro wordBoxes
            )

    home <- RD.button "back home"
    return ("Page 2", titlePage pagingDyn <$ home)

    where pagingFunction NextPage currentPage = currentPage + 1
          pagingFunction PrevPage currentPage = min 0 $ currentPage - 1
          pagingFunction _ currentPage = 0


initialModel :: TextViewModel
initialModel = TextViewModel text 0 0


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
               | itemIsSpring (Prelude.last r) = init r
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
processedWords = preprocess text

text :: String
text = "Chapter 1\nSince I can do no good because a woman,\nReach constantly at something that is near it.\nThe Maid's Tragedy: BEAUMONT AND FLETCHER.\nMiss Brooke had that kind of beauty which seems to be thrown into relief by poor dress. Her hand and wrist were so finely formed that she could wear sleeves not less bare of style than those in which the Blessed Virgin appeared to Italian painters; and her profile as well as her stature and bearing seemed to gain the more dignity from her plain garments, which by the side of provincial fashion gave her the impressiveness of a fine quotation from the Bible, - or from one of our elder poets, - in a paragraph of to-day's newspaper. She was usually spoken of as being remarkably clever, but with the addition that her sister Celia had more common-sense. Nevertheless, Celia wore scarcely more trimmings; and it was only to close observers that her dress differed from her sister's, and had a shade of coquetry in its arrangements; for Miss Brooke's plain dressing was due to mixed conditions, in most of which her sister shared. The pride of being ladies had something to do with it: the Brooke connections, though not exactly aristocratic, were unquestionably \"good:\" if you inquired backward for a generation or two, you would not find any yard-measuring or parcel-tying forefathers - anything lower than an admiral or a clergyman; and there was even an ancestor discernible as a Puritan gentleman who served under Cromwell, but afterwards conformed, and managed to come out of all political troubles as the proprietor of a respectable family estate. Young women of such birth, living in a quiet country-house, and attending a village church hardly larger than a parlour, naturally regarded frippery as the ambition of a huckster's daughter. Then there was well-bred economy, which in those days made show in dress the first item to be deducted from, when any margin was required for expenses more distinctive of rank. Such reasons would have been enough to account for plain dress, quite apart from religious feeling; but in Miss Brooke's case, religion alone would have determined it; and Celia mildly acquiesced in all her sister's sentiments, only infusing them with that common-sense which is able to accept momentous doctrines without any eccentric agitation. Dorothea knew many passages of Pascal's Pens\233es and of Jeremy Taylor by heart; and to her the destinies of mankind, seen by the light of Christianity, made the solicitudes of feminine fashion appear an occupation for Bedlam. She could not reconcile the anxieties of a spiritual life involving eternal consequences, with a keen interest in gimp and artificial protrusions of drapery. Her mind was theoretic, and yearned by its nature after some lofty conception of the world which might frankly include the parish of Tipton and her own rule of conduct there; she was enamoured of intensity and greatness, and rash in embracing whatever seemed to her to have those aspects; likely to seek martyrdom, to make retractations, and then to incur martyrdom after all in a quarter where she had not sought it. Certainly such elements in the character of a marriageable girl tended to interfere with her lot, and hinder it from being decided according to custom, by good looks, vanity, and merely canine affection. With all this, she, the elder of the sisters, was not yet twenty, and they had both been educated, since they were about twelve years old and had lost their parents, on plans at once narrow and promiscuous, first in an English family and afterwards in a Swiss family at Lausanne, their bachelor uncle and guardian trying in this way to remedy the disadvantages of their orphaned condition.\nIt was hardly a year since they had come to live at Tipton Grange with their uncle, a man nearly sixty, of acquiescent temper, miscellaneous opinions, and uncertain vote. He had travelled in his younger years, and was held in this part of the county to have contracted a too rambling habit of mind. Mr Brooke's conclusions were as difficult to predict as the weather: it was only safe to say that he would act with benevolent intentions, and that he would spend as little money as possible in carrying them out. For the most glutinously indefinite minds enclose some hard grains of habit; and a man has been seen lax about all his own interests except the retention of his snuff-box, concerning which he was watchful, suspicious, and greedy of clutch.\nIn Mr Brooke the hereditary strain of Puritan energy was clearly in abeyance; but in his niece Dorothea it glowed alike through faults and virtues, turning sometimes into impatience of her uncle's talk or his way of \"letting things be\" on his estate, and making her long all the more for the time when she would be of age and have some command of money for generous schemes. She was regarded as an heiress; for not only had the sisters seven hundred a-year each from their parents, but if Dorothea married and had a son, that son would inherit Mr Brooke's estate, presumably worth about three thousand a-year - a rental which seemed wealth to provincial families, still discussing Mr Peel's late conduct on the Catholic question, innocent of future gold-fields, and of that gorgeous plutocracy which has so nobly exalted the necessities of genteel life.\nAnd how should Dorothea not marry? - a girl so handsome and with such prospects? Nothing could hinder it but her love of extremes, and her insistence on regulating life according to notions which might cause a wary man to hesitate before he made her an offer, or even might lead her at last to refuse all offers. A young lady of some birth and fortune, who knelt suddenly down on a brick floor by the side of a sick laborer and prayed fervidly as if she thought herself living in the time of the Apostles - who had strange whims of fasting like a Papist, and of sitting up at night to read old theological books! Such a wife might awaken you some fine morning with a new scheme for the application of her income which would interfere with political economy and the keeping of saddle-horses: a man would naturally think twice before he risked himself in such fellowship. Women were expected to have weak opinions; but the great safeguard of society and of domestic life was, that opinions were not acted on. Sane people did what their neighbours did, so that if any lunatics were at large, one might know and avoid them.\nThe rural opinion about the new young ladies, even among the cottagers, was generally in favor of Celia, as being so amiable and innocent-looking, while Miss Brooke's large eyes seemed, like her religion, too unusual and striking. Poor Dorothea! compared with her, the innocent-looking Celia was knowing and worldly-wise; so much subtler is a human mind than the outside tissues which make a sort of blazonry or clock-face for it.\nYet those who approached Dorothea, though prejudiced against her by this alarming hearsay, found that she had a charm unaccountably reconcilable with it. Most men thought her bewitching when she was on horseback. She loved the fresh air and the various aspects of the country, and when her eyes and cheeks glowed with mingled pleasure she looked very little like a devotee. Riding was an indulgence which she allowed herself in spite of conscientious qualms; she felt that she enjoyed it in a pagan sensuous way, and always looked forward to renouncing it.\nShe was open, ardent, and not in the least self-admiring; indeed, it was pretty to see how her imagination adorned her sister Celia with attractions altogether superior to her own, and if any gentleman appeared to come to the Grange from some other motive than that of seeing Mr Brooke, she concluded that he must be in love with Celia: Sir James Chettam, for example, whom she constantly considered from Celia's point of view, inwardly debating whether it would be good for Celia to accept him. That he should be regarded as a suitor to herself would have seemed to her a ridiculous irrelevance. Dorothea, with all her eagerness to know the truths of life, retained very childlike ideas about marriage. She felt sure that she would have accepted the judicious Hooker, if she had been born in time to save him from that wretched mistake he made in matrimony; or John Milton when his blindness had come on; or any of the other great men whose odd habits it would have been glorious piety to endure; but an amiable handsome baronet, who said \"Exactly\" to her remarks even when she expressed uncertainty, - how could he affect her as a lover? The really delightful marriage must be that where your husband was a sort of father, and could teach you even Hebrew, if you wished it.\nThese peculiarities of Dorothea's character caused Mr Brooke to be all the more blamed in neighboring families for not securing some middle-aged lady as guide and companion to his nieces. But he himself dreaded so much the sort of superior woman likely to be available for such a position, that he allowed himself to be dissuaded by Dorothea's objections, and was in this case brave enough to defy the world - that is to say, Mrs. Cadwallader the Rector's wife, and the small group of gentry with whom he visited in the northeast corner of Loamshire. So Miss Brooke presided in her uncle's household, and did not at all dislike her new authority, with the homage that belonged to it.\nSir James Chettam was going to dine at the Grange to-day with another gentleman whom the girls had never seen, and about whom Dorothea felt some venerating expectation. This was the Reverend Edward Casaubon, noted in the county as a man of profound learning, understood for many years to be engaged on a great work concerning religious history; also as a man of wealth enough to give lustre to his piety, and having views of his own which were to be more clearly ascertained on the publication of his book. His very name carried an impressiveness hardly to be measured without a precise chronology of scholarship.\nEarly in the day Dorothea had returned from the infant school which she had set going in the village, and was taking her usual place in the pretty sitting-room which divided the bedrooms of the sisters, bent on finishing a plan for some buildings (a kind of work which she delighted in), when Celia, who had been watching her with a hesitating desire to propose something, said -\n\"Dorothea, dear, if you don't mind - if you are not very busy - suppose we looked at mamma's jewels to-day, and divided them? It is exactly six months to-day since uncle gave them to you, and you have not looked at them yet.\"\nCelia's face had the shadow of a pouting expression in it, the full presence of the pout being kept back by an habitual awe of Dorothea and principle; two associated facts which might show a mysterious electricity if you touched them incautiously. To her relief, Dorothea's eyes were full of laughter as she looked up.\n\"What a wonderful little almanac you are, Celia! Is it six calendar or six lunar months?\"\n\"It is the last day of September now, and it was the first of April when uncle gave them to you. You know, he said that he had forgotten them till then. I believe you have never thought of them since you locked them up in the cabinet here.\"\n\"Well, dear, we should never wear them, you know.\" Dorothea spoke in a full cordial tone, half caressing, half explanatory. She had her pencil in her hand, and was making tiny side-plans on a margin.\nCelia coloured, and looked very grave. \"I think, dear, we are wanting in respect to mamma's memory, to put them by and take no notice of them. And,\" she added, after hesitating a little, with a rising sob of mortification, \"necklaces are quite usual now; and Madame Poincon, who was stricter in some things even than you are, used to wear ornaments. And Christians generally - surely there are women in heaven now who wore jewels.\" Celia was conscious of some mental strength when she really applied herself to argument.\n\"You would like to wear them?\" exclaimed Dorothea, an air of astonished discovery animating her whole person with a dramatic action which she had caught from that very Madame Poincon who wore the ornaments. \"Of course, then, let us have them out. Why did you not tell me before? But the keys, the keys!\" She pressed her hands against the sides of her head and seemed to despair of her memory.\n\"They are here,\" said Celia, with whom this explanation had been long meditated and prearranged.\n\"Pray open the large drawer of the cabinet and get out the jewel-box.\"\nThe casket was soon open before them, and the various jewels spread out, making a bright parterre on the table. It was no great collection, but a few of the ornaments were really of remarkable beauty, the finest that was obvious at first being a necklace of purple amethysts set in exquisite gold work, and a pearl cross with five brilliants in it. Dorothea immediately took up the necklace and fastened it round her sister's neck, where it fitted almost as closely as a bracelet; but the circle suited the Henrietta-Maria style of Celia's head and neck, and she could see that it did, in the pier-glass opposite.\n\"There, Celia! you can wear that with your Indian muslin. But this cross you must wear with your dark dresses.\"\nCelia was trying not to smile with pleasure. \"O Dodo, you must keep the cross yourself.\"\n\"No, no, dear, no,\" said Dorothea, putting up her hand with careless deprecation.\n\"Yes, indeed you must; it would suit you - in your black dress, now,\" said Celia, insistingly. \"You MIGHT wear that.\"\n\"Not for the world, not for the world. A cross is the last thing I would wear as a trinket.\" Dorothea shuddered slightly.\n\"Then you will think it wicked in me to wear it,\" said Celia, uneasily.\n\"No, dear, no,\" said Dorothea, stroking her sister's cheek. \"Souls have complexions too: what will suit one will not suit another.\"\n\"But you might like to keep it for mamma's sake.\"\n\"No, I have other things of mamma's - her sandal-wood box which I am so fond of - plenty of things. In fact, they are all yours, dear. We need discuss them no longer. There - take away your property.\"\nCelia felt a little hurt. There was a strong assumption of superiority in this Puritanic toleration, hardly less trying to the blond flesh of an unenthusiastic sister than a Puritanic persecution.\n\"But how can I wear ornaments if you, who are the elder sister, will never wear them?\"\n\"Nay, Celia, that is too much to ask, that I should wear trinkets to keep you in countenance. If I were to put on such a necklace as that, I should feel as if I had been pirouetting. The world would go round with me, and I should not know how to walk.\"\nCelia had unclasped the necklace and drawn it off. \"It would be a little tight for your neck; something to lie down and hang would suit you better,\" she said, with some satisfaction. The complete unfitness of the necklace from all points of view for Dorothea, made Celia happier in taking it. She was opening some ring-boxes, which disclosed a fine emerald with diamonds, and just then the sun passing beyond a cloud sent a bright gleam over the table.\n\"How very beautiful these gems are!\" said Dorothea, under a new current of feeling, as sudden as the gleam. \"It is strange how deeply colors seem to penetrate one, like scent I suppose that is the reason why gems are used as spiritual emblems in the Revelation of St. John. They look like fragments of heaven. I think that emerald is more beautiful than any of them.\"\n\"And there is a bracelet to match it,\" said Celia. \"We did not notice this at first.\"\n\"They are lovely,\" said Dorothea, slipping the ring and bracelet on her finely turned finger and wrist, and holding them towards the window on a level with her eyes. All the while her thought was trying to justify her delight in the colors by merging them in her mystic religious joy.\n\"You WOULD like those, Dorothea,\" said Celia, rather falteringly, beginning to think with wonder that her sister showed some weakness, and also that emeralds would suit her own complexion even better than purple amethysts. \"You must keep that ring and bracelet - if nothing else. But see, these agates are very pretty and quiet.\"\n\"Yes! I will keep these - this ring and bracelet,\" said Dorothea. Then, letting her hand fall on the table, she said in another tone - \"Yet what miserable men find such things, and work at them, and sell them!\" She paused again, and Celia thought that her sister was going to renounce the ornaments, as in consistency she ought to do.\n\"Yes, dear, I will keep these,\" said Dorothea, decidedly. \"But take all the rest away, and the casket.\"\nShe took up her pencil without removing the jewels, and still looking at them. She thought of often having them by her, to feed her eye at these little fountains of pure colour.\n\"Shall you wear them in company?\" said Celia, who was watching her with real curiosity as to what she would do.\nDorothea glanced quickly at her sister. Across all her imaginative adornment of those whom she loved, there darted now and then a keen discernment, which was not without a scorching quality. If Miss Brooke ever attained perfect meekness, it would not be for lack of inward fire.\n\"Perhaps,\" she said, rather haughtily. \"I cannot tell to what level I may sink.\"\nCelia blushed, and was unhappy: she saw that she had offended her sister, and dared not say even anything pretty about the gift of the ornaments which she put back into the box and carried away. Dorothea too was unhappy, as she went on with her plan-drawing, questioning the purity of her own feeling and speech in the scene which had ended with that little explosion.\nCelia's consciousness told her that she had not been at all in the wrong: it was quite natural and justifiable that she should have asked that question, and she repeated to herself that Dorothea was inconsistent: either she should have taken her full share of the jewels, or, after what she had said, she should have renounced them altogether.\n\"I am sure - at least, I trust,\" thought Celia, \"that the wearing of a necklace will not interfere with my prayers. And I do not see that I should be bound by Dorothea's opinions now we are going into society, though of course she herself ought to be bound by them. But Dorothea is not always consistent.\"\nThus Celia, mutely bending over her tapestry, until she heard her sister calling her.\n\"Here, Kitty, come and look at my plan; I shall think I am a great architect, if I have not got incompatible stairs and fireplaces.\"\nAs Celia bent over the paper, Dorothea put her cheek against her sister's arm caressingly. Celia understood the action. Dorothea saw that she had been in the wrong, and Celia pardoned her. Since they could remember, there had been a mixture of criticism and awe in the attitude of Celia's mind towards her elder sister. The younger had always worn a yoke; but is there any yoked creature without its private opinions?"
