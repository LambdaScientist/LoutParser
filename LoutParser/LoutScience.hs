




module LoutScience where 








import Prelude hiding (Word)

import Control.Arrow ((***), first)

import Data.List (foldl', intersperse)

import Data.List.Split (chunksOf)


-- Code harvested and modified from
-- import Text.PrettyPrint.Boxes

-- | The basic data type.  A box has a specified size and some sort of
--   contents.
data Box = Box { rows    :: Int
               , cols    :: Int
               , content :: Content
               }
  -- deriving (Show)
-- This is temporary
instance Show Box where
  show = render

-- | Contents of a box.
data Content = Blank        -- ^ No content.
             | Text String  -- ^ A raw string.
             | Row [Box]    -- ^ A row of sub-boxes.
             | Col [Box]    -- ^ A column of sub-boxes.
             | SubBox Alignment Alignment Box
                            -- ^ A sub-box with a specified alignment.
  deriving (Show)

-- | Data type for specifying the alignment of boxes.
data Alignment = AlignFirst    -- ^ Align at the top/left.
               | AlignCenter1  -- ^ Centered, biased to the top/left.
               | AlignCenter2  -- ^ Centered, biased to the bottom/right.
               | AlignLast     -- ^ Align at the bottom/right.
  deriving (Eq, Read, Show)


-- | The null box, which has no content and no size.  It is quite
--   useless.
nullBox :: Box
nullBox = emptyBox 0 0

-- | @emptyBox r c@ is an empty box with @r@ rows and @c@ columns.
--   Useful for effecting more fine-grained positioning of other
--   boxes, by inserting empty boxes of the desired size in between
--   them.
emptyBox :: Int -> Int -> Box
emptyBox r c = Box r c Blank

-- | A @1x1@ box containing a single character.
char :: Char -> Box
char c = Box 1 1 (Text [c])

-- | A (@1 x len@) box containing a string of length @len@.
text :: String -> Box
text t = Box 1 (length t) (Text t)


-- | Glue a list of boxes together horizontally, with the given alignment.
hcat :: Alignment -> [Box] -> Box
hcat a bs = Box h w (Row $ map (alignVert a h) bs)
  where h = maximum . (0:) . map rows $ bs
        w = sum . map cols $ bs

-- | @alignVert algn n bx@ creates a box of height @n@, with the
--   contents and width of @bx@, vertically aligned according to
--   @algn@.
alignVert :: Alignment -> Int -> Box -> Box
alignVert a r b = Box r (cols b) $ SubBox AlignFirst a b

-- | Glue a list of boxes together vertically, with the given alignment.
vcat :: Alignment -> [Box] -> Box
vcat a bs = Box h w (Col $ map (alignHoriz a w) bs)
  where h = sum . map rows $ bs
        w = maximum . (0:) . map cols $ bs

-- | @alignHoriz algn n bx@ creates a box of width @n@, with the
--   contents and height of @bx@, horizontally aligned according to
--   @algn@.
alignHoriz :: Alignment -> Int -> Box -> Box
alignHoriz a c b = Box (rows b) c $ SubBox a AlignFirst b

-- | Align boxes along their tops.
top :: Alignment
top = AlignFirst

-- | Align boxes along their bottoms.
bottom :: Alignment
bottom = AlignLast

-- | Align boxes to the left.
left :: Alignment
left = AlignFirst

-- | Align boxes to the right.
right :: Alignment
right = AlignLast

-- | Align boxes centered, but biased to the left/top in case of
--   unequal parities.
center1 :: Alignment
center1 = AlignCenter1

-- | Align boxes centered, but biased to the right/bottom in case of
--   unequal parities.
center2 :: Alignment
center2 = AlignCenter2


-- | Render a box as a list of lines.
renderBox :: Box -> [String]

renderBox (Box r c Blank)            = resizeBox r c [""]
renderBox (Box r c (Text t))         = resizeBox r c [t]
renderBox (Box r c (Row bs))         = resizeBox r c
                                       . merge
                                       . map (renderBoxWithRows r)
                                       $ bs
                           where merge = foldr (zipWith (++)) (repeat [])

renderBox (Box r c (Col bs))         = resizeBox r c
                                       . concatMap (renderBoxWithCols c)
                                       $ bs

renderBox (Box r c (SubBox ha va b)) = resizeBoxAligned r c ha va
                                       . renderBox
                                       $ b

-- | Render a box as a list of lines, using a given number of rows.
renderBoxWithRows :: Int -> Box -> [String]
renderBoxWithRows r b = renderBox (b{rows = r})

-- | Render a box as a list of lines, using a given number of columns.
renderBoxWithCols :: Int -> Box -> [String]
renderBoxWithCols c b = renderBox (b{cols = c})

-- | Resize a rendered list of lines.
resizeBox :: Int -> Int -> [String] -> [String]
resizeBox r c = takeP (blanks c) r . map (takeP ' ' c)

-- | Resize a rendered list of lines, using given alignments.
resizeBoxAligned :: Int -> Int -> Alignment -> Alignment -> [String] -> [String]
resizeBoxAligned r c ha va = takePA va (blanks c) r . map (takePA ha ' ' c)

-- | A convenience function for rendering a box to stdout.
printBox :: Box -> IO ()
printBox = putStr . render

-- | Render a 'Box' as a String, suitable for writing to the screen or
--   a file.
render :: Box -> String
render = unlines . renderBox


-- | \"Padded take\": @takeP a n xs@ is the same as @take n xs@, if @n
--   <= length xs@; otherwise it is @xs@ followed by enough copies of
--   @a@ to make the length equal to @n@.
takeP :: a -> Int -> [a] -> [a]
takeP _ n _      | n <= 0 = []
takeP b n []              = replicate n b
takeP b n (x:xs)          = x : takeP b (n-1) xs

-- | @takePA @ is like 'takeP', but with alignment.  That is, we
--   imagine a copy of @xs@ extended infinitely on both sides with
--   copies of @a@, and a window of size @n@ placed so that @xs@ has
--   the specified alignment within the window; @takePA algn a n xs@
--   returns the contents of this window.
takePA :: Alignment -> a -> Int -> [a] -> [a]
takePA c b n = glue . (takeP b (numRev c n) *** takeP b (numFwd c n)) . split
  where split t = first reverse . splitAt (numRev c (length t)) $ t
        glue    = uncurry (++) . first reverse
        numFwd AlignFirst    n = n
        numFwd AlignLast     _ = 0
        numFwd AlignCenter1  n = n `div` 2
        numFwd AlignCenter2  n = (n+1) `div` 2
        numRev AlignFirst    _ = 0
        numRev AlignLast     n = n
        numRev AlignCenter1  n = (n+1) `div` 2
        numRev AlignCenter2  n = n `div` 2

-- | Generate a string of spaces.
blanks :: Int -> String
blanks = flip replicate ' '

-- | @punctuateH a p bs@ horizontally lays out the boxes @bs@ with a
--   copy of @p@ interspersed between each.
punctuateH :: Alignment -> Box -> [Box] -> Box
punctuateH a p bs = hcat a (intersperse p bs)

-- | A vertical version of 'punctuateH'.
punctuateV :: Alignment -> Box -> [Box] -> Box
punctuateV a p bs = vcat a (intersperse p bs)



-- | Pretty Stuff
encloseSymbols :: Box -> Box -> Box -> Box 
encloseSymbols rowSym colSym = addColBarSym colSym . addRowBarSym rowSym

encloseSymbols' :: String -> String -> Box -> Box 
encloseSymbols' rowSym colSym = encloseSymbols (text rowSym) (text colSym)

encloseBoxQuick :: String -> Box -> Box
encloseBoxQuick s = encloseSymbols' s s 

encloseBoxDef :: Box -> Box
encloseBoxDef = encloseSymbols' "|" "-"

addRowBarSym :: Box -> Box -> Box
addRowBarSym sym input = hcat top [row,input,row]
  where
    ri = rows input
    row = mkRowSym sym ri
addColBarSym :: Box -> Box -> Box
addColBarSym sym input = vcat left [col,input,col] 
  where
    ci = cols input
    col = mkColSym sym ci 

mkColSym :: Box -> Int -> Box
mkColSym b l = hcat top $ replicate l b
mkRowSym :: Box -> Int -> Box
mkRowSym b l = vcat top $ replicate l b



--------------------------------------------------------------------------------
--  Paragraph flowing  ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | @para algn w t@ is a box of width @w@, containing text @t@,
--   aligned according to @algn@, flowed to fit within the given
--   width.
para :: Alignment -> Int -> String -> Box
para a n t = (\ss -> mkParaBox a (length ss) ss) $ flow n t

-- | @columns w h t@ is a list of boxes, each of width @w@ and height
--   at most @h@, containing text @t@ flowed into as many columns as
--   necessary.
columns :: Alignment -> Int -> Int -> String -> [Box]
columns a w h t = map (mkParaBox a h) . chunksOf h $ flow w t

-- | @mkParaBox a n s@ makes a box of height @n@ with the text @s@
--   aligned according to @a@.
mkParaBox :: Alignment -> Int -> [String] -> Box
mkParaBox a n = alignVert top n . vcat a . map text
-- | Flow the given text into the given width.
flow :: Int -> String -> [String]
flow n t = map (take n)
         . getLines
         $ foldl' addWordP (emptyPara n) (map mkWord . words $ t)

data Para = Para { paraWidth   :: Int
                 , paraContent :: ParaContent
                 }
data ParaContent = Block { fullLines :: [Line]
                         , lastLine  :: Line
                         }

emptyPara :: Int -> Para
emptyPara pw = Para pw (Block [] (Line 0 []))

getLines :: Para -> [String]
getLines (Para _ (Block ls l))
  | lLen l == 0 = process ls
  | otherwise   = process (l:ls)
  where process = map (unwords . reverse . map getWord . getWords) . reverse

data Line = Line { lLen :: Int, getWords :: [Word] }

mkLine :: [Word] -> Line
mkLine ws = Line (sum (map wLen ws) + length ws - 1) ws

startLine :: Word -> Line
startLine = mkLine . (:[])

data Word = Word { wLen :: Int, getWord  :: String }

mkWord :: String -> Word
mkWord w = Word (length w) w

addWordP :: Para -> Word -> Para
addWordP (Para pw (Block fl l)) w
  | wordFits pw w l = Para pw (Block fl (addWordL w l))
  | otherwise       = Para pw (Block (l:fl) (startLine w))

addWordL :: Word -> Line -> Line
addWordL w (Line len ws) = Line (len + wLen w + 1) (w:ws)

wordFits :: Int -> Word -> Line -> Bool
wordFits pw w l = lLen l == 0 || lLen l + wLen w + 1 <= pw

-- | Lout like 
infixl 6 -/
infixl 6 -//
infixl 7 -|
infixl 7 -||
infixl 8 -&
-- object / gap object               Vertical concatenation with mark alignment
(-/) lbox rbox  = vcat left [lbox,rbox]
-- object // gap object              Vertical concatenation with left justification
(-//) lbox rbox = vcat center1 [lbox,rbox]
-- object | gap object               Horizontal concatenation with mark alignment
(-|) lbox  rbox = hcat left [lbox,rbox]
-- object || gap object              Horizontal concatenation with top-justification
(-||) lbox rbox = hcat top [lbox,rbox]
-- object & gap object               Horizontal concatenation within paragraphs
(-&) lbox  rbox = hcat bottom [lbox,rbox]


-- | Paste two boxes together vertically with a single intervening row
--   of space, using a default (left) alignment.
(/+/) :: Box -> Box -> Box
t /+/ b = vcat left [t, emptyBox 1 0, b]

-- | Paste two boxes together horizontally with a single intervening
--   column of space, using a default (top) alignment.
(<+>) :: Box -> Box -> Box
l <+> r = hcat top [l, emptyBox 0 3, r]

-- Examples

pgLout' :: Alignment -> Int -> Int -> [Box]
pgLout' a w h = columns a w h testStr
pgLout :: Int -> Int -> [Box]
pgLout = pgLout' left
-- ex' :: [Box]


ex' w h = sequence_ $ map (printBox.encloseBoxDef) (pgLout w h)
ex = ex' 20 15

lout :: Box
lout = ( a -// c -| d ) -| ( b -/ e ) -/ ( f -/ i ) -| ( g -| h -// j )
a = encloseBoxDef $ text "a" --  
b = encloseBoxDef $ text "b" --  
c = encloseBoxDef $ text "c" --  
d = encloseBoxDef $ text "d" --  
e = encloseBoxDef $ text "e" --  
f = encloseBoxDef $ text "f" --  
g = encloseBoxDef $ text "g" --  
h = encloseBoxDef $ text "h" --  
i = encloseBoxDef $ text "i" --  
j = encloseBoxDef $ text "j" --  
showOff = printBox lout 
showOff2x = printBox (lout -| lout)

ghv = g -| h 

runExample = 
  [ ( "\n" ++ str1 ++ " " ++ catName ++ " " ++ show align ++ " " ++ str2
    , catFun align [box1, box2]
    ) | (str1, box1) <- [("j",j),("ghv",ghv)]
      , (str2, box2) <- [("j",j),("ghv",ghv)]
      , (catName, catFun) <- allCats
      , align <- allAlign
  ]
runEx = sequence_ [putStrLn str >> printBox bx | (str,bx) <- runExample ]


testStr = " \
        \ In the world of music\
        \ England is supposed\
        \ to be a mere province.\
        \ If she produces an\
        \ indifferent\
        \ composer\
        \ or performer, that is\
        \ regarded elsewhere as\
        \ perfectly normal and\
        \ natural; but if foreign\
        \ students of musical\
        \ history\
        \ have\
        \ to\
        \ acknowledge a British\
        \ musical genius, he is\
        \ considered a freak.\
        \ Such a freak is\
        \ Henry Purcell. Yet if we\
        \ make a choice of fifteen\
        \ of the world’s musical\
        \ classics, as here, we find\
        \ that we cannot omit this\
        \ English master."



foo0 = text "foo0"
foo1 = text "buzz"
foo2 = text "foo2"
foo3 = text "foo3"
foo4 = text "foo4"
foo5 = text "foo5"

allAlign = [top, bottom, left, right, center1, center2]
allCats = [("vcat", vcat),("hcat", hcat)]
fooEx1 = 
  [ ("foo1", foo1)
  -- , ("foo1", foo1)
  -- , ("foo2", foo2)
  -- , ("foo3", foo3)
  -- , ("foo4", foo4)
  -- , ("foo5", foo5)
  ]
fooEx2 = 
  [ ("foo0", foo0)
  -- , ("foo0", foo1)
  -- , ("foo2", foo2)
  -- , ("foo3", foo3)
  -- , ("foo4", foo4)
  -- , ("foo5", foo5)
  ]
-- fooNames = fst <$> fooEx
-- fooBoxes = snd <$> fooEx

allExample = 
  [ ( "\n" ++ str1 ++ " " ++ catName ++ " " ++ show align ++ " " ++ str2
    , catFun align [box1, box2]
    ) | (str1, box1) <- fooEx1
      , (str2, box2) <- fooEx2
      , (catName, catFun) <- allCats
      , align <- allAlign
  ]
main = sequence_ [putStrLn str >> printBox bx | (str,bx) <- allExample ]
