{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
import Prelude 
import Text.Parsec
import Text.Parsec.String

import Control.Monad

-- import qualified GalleyCode as GC

-------------------------------------------------------------------
-- General
parseInt :: Parser String
parseInt = plus <|> minus <|> number
  where plus   = char '+' *> number
        minus  = (:) <$> char '-' <*> number
        number = many1 digit

parseDouble :: Parser String
parseDouble = (++) <$> decimal <*> parseInt
  where decimal = ((++) <$> parseInt <*> string "." ) 
              <|> return ""
-------------------------------------------------------------------

------------------------------------------------------------------------------
-- The 23 primitive operators of Lout, in order of increasing precedence.
------------------------------------------------------------------------------
-- object / gap object               Vertical concatenation with mark alignment
-- object // gap object              Vertical concatenation with left justification
-- object | gap object               Horizontal concatenation with mark alignment
-- object || gap object              Horizontal concatenation with top-justification
-- object & gap object               Horizontal concatenation within paragraphs
-- @OneCol object                    Hide all but one column mark of object
-- @OneRow object                    Hide all but one row mark of object
-- font @Font object                 Render object in nominated font
-- breakstyle @Break object          Break paragraphs of object in nominated style
-- spacestyle @Space object          Render spaces between words in nominated style
-- length @Wide object               Render object to width length
-- length @High object               Render object to height length
-- @HExpand object                   Expand horizontal gaps to fill available space
-- @VExpand object                   Expand vertical gaps to fill available space
-- @HScale object                    Horizontal geometrical scaling to fill available space
-- @VScale object                    Vertical geometrical scaling to fill available space
-- angle @Rotate object              Rotate object by angle
-- PostScript @Graphic object        Escape to graphics language
-- @Next object                      Add 1 to an object denoting a number
-- object @Case alternatives         Select from a set of alternative objects
-- identifier && object              Cross reference
-- cross-reference @Open object      Retrieve value from cross reference
-- cross-reference @Tagged object    Attach cross referencing tag to object

data LoutPrim = LP Gap
  deriving (Show)

type Component = String -- [String]


data ItemW = W { wContents :: [String]
               }
data ItemH = H { hContents :: [ItemW]
               }


-- |0.2ie produces a 0.2 inches measured from edge to edge; 
-- |0.3ix produces a 0.3 inch gap measured from mark to mark 
--        MAYBE widened to prevent overstriking
-- |2.5it places its right parameter 2.5 inches from the current left margin, 
--        irrespective of the position of the left parameter. There is also a 
--        choice of eleven units of measurement (inches, centimetres, multiples 
--        of the current font size, etc.)
-- | r unit: one r is the column width minus the width of the following object, so that 
-- * |1rt produces sufficient space to right justify the following object
-- * |0.5rt to center it. 
data Unit = I | IE | IX | IT | R | RT
  deriving (Show)
data GapArg = GA Double Unit | DefUnit Unit | DefaultValue
  deriving (Show)

-- object / gap object               Vertical concatenation with mark alignment
-- object // gap object              Vertical concatenation with left justification
-- object | gap object               Horizontal concatenation with mark alignment
-- object || gap object              Horizontal concatenation with top-justification
-- object & gap object               Horizontal concatenation within paragraphs
data Gap = VMark GapArg -- Object Object
         | VLeft GapArg -- Object Object
         | HMark GapArg -- Object Object
         | HTop  GapArg -- Object Object
         | HPara GapArg -- Object Object
         deriving (Show)

parseGapArg :: Parser GapArg
parseGapArg = GA <$> (rd <$> parseDouble) <*> parseUnit
          <|> DefUnit <$> parseUnitR
          <|> return DefaultValue
  where rd = read :: String -> Double

parseUnitR :: Parser Unit
parseUnitR = string "r" *> return R

parseUnit :: Parser Unit
parseUnit = choice 
          [ string "i"  *> return I
          , string "ie" *> return IE
          , string "ix" *> return IX
          , string "it" *> return IT
          , parseUnitR 
          , string "rt" *> return RT
          ]

-- object / gap object               Vertical concatenation with mark alignment
-- object // gap object              Vertical concatenation with left justification
-- object | gap object               Horizontal concatenation with mark alignment
-- object || gap object              Horizontal concatenation with top-justification
-- object & gap object               Horizontal concatenation within paragraphs
parseGapOp :: Parser Gap -- (Object -> Object -> Gap)
-- This fuction take advantage of partial succeeding parsers ==> Order matters on like symbols
parseGapOp = choice 
           [ 
             char '&' *> (HPara <$> parseGapArg)
           , char '|' *> (char '|' *> (HMark <$> parseGapArg)
                     <|> (HTop <$> parseGapArg))
           , char '/' *> (char '/' *> (VLeft <$> parseGapArg)
                     <|> (VMark <$> parseGapArg))
           ]

parseObject :: Parser Object
parseObject = pSeqObj

--Not finished
parseObject' :: Parser Object
parseObject' = choice 
             [ --loutExpression,
               pNestObj
             , (PosLit <$> loutIdent)
             ] >>= loutInfixHalfExpression 

pNestObj :: Parser Object
pNestObj = char '{' *> pSeqObj <* char '}'

pSeqObj :: Parser Object
pSeqObj = SeqObj <$> many1 (parseObject' <* spaces)
      <|> return EmptyObj

loutIdent :: Parser TextInput
loutIdent = loutIdentCharSeq <|> loutIdentString

data TextInput = Unanalyzed String
               | Identifier String
               | LiteralWord String 
               | Symbol String
               deriving (Show)

type Operator = Gap
-- object
-- → object infixop object 
-- → prefixop object
-- → object postfixop 
-- → noparsop
-- → literalword
-- → { object }
-- → object object 
-- →
data Object = 
  -- where infixop, prefixop, postfixop, and noparsop are identifiers naming 
  -- operators which take 0, 1 or 2 parameters, as shown, and literalword is 
  -- a sequence of non-space characters, or an arbitrary sequence of characters 
  -- enclosed in double quotes.
    InfixOp Object Operator Object 
  | PrefixOp Operator Object 
  | PostfixIp Object Operator
  | Noparsop Operator
  -- a sequence of non-space characters, or an arbitrary sequence of characters enclosed in double quotes.
  | PosLit TextInput
  --sequences of arbitrary objects separated by white space, called paragraphs.
  | SeqObj [Object] 
  -- allows a meaning for expressions such as {}, in which an object is missing. The value of 
  -- this empty object is a rectangle of size 0 by 0, with one column mark and one row mark, that prints as nothing.
  | EmptyObj
  deriving (Show)



loutExpression :: Parser Object
loutExpression = parseObject' -- >>= loutInfixHalfExpression --add other expression types
loutInfixHalfExpression :: Object -> Parser Object
loutInfixHalfExpression obj = do spaces
                                 gapFun <- parseGapOp 
                                 spaces
                                 rightObj <- parseObject'
                                 return $ InfixOp obj gapFun rightObj
                          <|> return obj



loutInfixExpression :: Parser Object
loutInfixExpression = do leftObj <- parseObject'
                         spaces
                         gapFun <- parseGapOp 
                         spaces
                         rightObj <- parseObject'
                         return $ InfixOp leftObj gapFun rightObj
loutPostExpression :: Parser Object
loutPostExpression = undefined
loutPrefixExpression :: Parser Object
loutPrefixExpression = undefined





testFile = "/home/burhopja/Documents/burhopSrc/Joy/joy-hs/LoutParser/ex.txt"
-- foo = do result <- parseFromFile parseObject testFile 
foo = do result <- parseFromFile loutExpression testFile 
         putStrLn $ "Reading from: " ++ testFile
         putStrLn $ "\nResult: \n"
         print result







escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0rvbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\r\v\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

-- A sequence of characters which is neither a white space,
-- an identifier, nor a delimiter, is by default a literal 
-- word, which means that it will pass through Lout unchanged. 
-- An arbitrary sequence of characters enclosed in double quotes, 
-- for example "{ }", is also a literal word.
-- Space characters may be included, but not tabs or newlines.
loutIdentString = do
    s <- char '"'
    strings <- many character
    end <- char '"'
    return.Unanalyzed $ s:(concat strings)++[end]
                    
loutIdentCharSeq = many1 (letter <|> char '@') 
               >>= return . Unanalyzed

pSymbol = many1 (oneOf "!@$%&’*+,-./:;<=>?^‘|~") 
      >>= return . Symbol

pTextInput = (try pSymbol <|> loutIdent ) 
pPosLit = PosLit <$> pTextInput
-- comment :: Parsec String String String
comment = do char '#'
             manyTill anyChar endOfLine 
             return ()

-- pLoutFile = many $ (many comment *> pObject)

-- pObject = try pInfix
--       <|> pPosLit
--       <|> return EmptyObj 

-- pInfix :: Parsec Object
-- pInfix = do left <- pObject 
--             infixop <- pTextInput
--             right <- pObject
--             return $ InfixOp left infixop right

-- letter
-- white space quote escape comment other
-- @ab-zAB-Z_
-- space formfeed tab newline
-- "
-- \
-- # !$%&’()*+,-./0123456789:;<=>?[]^‘{|}~

-- When the comment character # is encountered, everything from that point to the end of the line is ignored.


-- If objects are to be constructed like mathematical expressions, the natural 
-- notation is a func- tional language based on operators, as in Eqn. The 
-- grammar of Lout objects is accordingly

-- object
-- → object infixop object 
-- → prefixop object
-- → object postfixop 
-- → noparsop
-- → literalword
-- → { object }
-- → object object 
-- →
-- where infixop, prefixop, postfixop, and noparsop are identifiers naming 
-- operators which take 0, 1 or 2 parameters, as shown, and literalword is 
-- a sequence of non-space characters, or an arbitrary
-- sequence of characters enclosed in double quotes. Ambiguities are resolved by precedence and associativity.
-- The last production allows a meaning for expressions such as {}, in which an object is missing. The value of 
-- this empty object is a rectangle of size 0 by 0, with one column mark and one row mark, that prints as nothing.
-- The second-last production generates sequences of arbitrary objects separated by white space, called 
-- paragraphs. Ignoring paragraph breaking for now, the natural meaning is that the two objects should appear 
-- side by side, and Lout’s parser accordingly interpolates an infix horizontalconcatenationoperator(seebelow)
-- betweenthem. Thisoperatorisassociative,sothe grammatical ambiguity does no harm. However, the Algol-60 rule 
-- that white space should be significant only as a separator is necessarily broken by Lout in just this one place.
-- Algol-like languages distinguish literal strings from identifiers by enclosing them in quotes, but literals are 
-- far too frequent in document formatting for this to be viable. The conventional solution is to begin identifiers
--  with a special character, and Lout follows Scribe [7] in using ‘@’ rather than the ‘\’ of troff [8] and TEX [9].
-- However, Lout takes the unusual step of making an initial ‘@’ optional. The designers of Eqn apparently 
-- considered such characters disfiguring in fine-grained input like equations, and this author agrees. The 
-- implementation is straightforward: ‘@’ is classed as just another letter, and every word is searched for 
-- in the symbol table. If it is found, it is an identifier, otherwise it is a literal. A warning message 
-- is printed when a literal beginning with ‘@’ is found, since it is probably a mis-spelt identifier. 
-- No such safety net is possible for identifiers without ‘@’.
-- Equation formatting also demands symbols made from punctuation characters, such as + and <=. It is traditional
--  to allow such symbols to be juxtaposed, which means that the input
-- <=++
-- for example must be interpreted within the lexical analyser by searching the symbol table for its prefixes in the 
-- order <=++, <=+, <=. Although this takes quadratic time, in practice such sequences are too short to make a 
-- more sophisticated linear method like tries worthwhile.




-- A delimiter is a sequence of one or more ‘other’ characters which is the name of a symbol. 
-- For example, { and // are delimiters. 
-- When defining a delimiter, the name must be enclosed in quotes:
-- def "^" { {} ^& {} }

-- but quotes are not used when the delimiter is invoked. 
-- A delimiter may have delimiters and any other characters adjacent, 
-- whereas identifiers may not be adjacent to letters or other identifiers. 
-- The complete list of predefined delimiters is
-- / | & && // || ^& { ^/^| } ^// ^||

-- A longer delimiter like <= will be recognised in preference to a shorter one like <.


-- Some characters are symbols that produce special effects – for example, { and } produce
-- grouping – and to turn off these effects the characters must be enclosed in double quotes: "{"
-- produces {. The complete set of these special characters is
-- / | & { } # @ ^ ~ \ "

-- An identifier is a sequence of one or more letters which is the name of a symbol. 
-- It is conventional but not essential to begin identifiers with @; Basser Lout will 
-- print a warning message if it finds an unquoted literal word (see below) beginning 
-- with @, since such words are usually misspelt identifiers. The ten digits are not 
-- letters and may not appear in identifiers; and although the underscore character 
-- is a letter and may be used in identifiers, it is not conventional to do so. 
-- The complete list of predefined identifiers is
-- @BackEnd @Background @Begin @BeginHeaderComponent @Break @Case @ClearHeaderComponent @Common @Char @CurrFace
-- @CurrFamily @CurrLang @CurrYUnit @CurrZUnit @Database @End @EndHeaderComponent @Enclose @Filter @FilterErr
-- @FilterIn @FilterOut @Font @ForceGalley @Galley @Graphic @HAdjust @HContract @HCover @HExpand @High @HLimited
-- @HScale @HShift @HSpan @Include @IncludeGraphic @Insert @KernShrink @Key @Language @LClos @LEnv @LInput
-- @LVis @LUse @LinkSource @LinkDest @Meld @Merge @Minus @Moment @Next @NotRevealed @Null
-- @OneCol @OneOf @OneRow @Open @Optimize @Outline @PAdjust @PageLabel @PlainGraphic @Plus @PrependGraphic
-- @RawVerbatim @Rotate @Rump @Scale @SetColor @SetColour @SetHeaderComponent @Space @StartHSpan @StartHVSpan 
-- @StartVSpan @SysDatabase @SysInclude @SysIncludeGraphic @SysPrependGraphic @Tag @Tagged @Target @Underline @Use 
-- @VAdjust @VContract @VCover @Verbatim @VExpand @VLimited @VScale @VShift @VSpan @Wide @Yield @YUnit @ZUnit
-- plus the names of the parameters of @Moment. 
-- The symbols @LClos, @LEnv, @LInput, @LVis and @LUse appear 
-- in cross reference databases generated by Lout and are not for use elsewhere.



-- Macros provide a means of defining symbols which stand for a sequence of textual units rather than an object. 
-- For example, the macro definition macro @PP { //1.3vx 2.0f @Wide &0i } makes Lout replace the symbol @PP by 
-- the given textual units before assembling its input into objects. A similar macro to this one is used to 
-- separate the paragraphs of the present document. The enclosing braces and any spaces adjacent to them are dropped, 
-- which can be a problem: @PP2i has result //1.3vx 2.0f @Wide &0i2i which is erroneous.

-- When @Chapter is invoked, its named parameters are given
-- 16 Chapter 2. Details values in the following way:
-- @Chapter
-- @Tag { intro }
-- @Title { Introduction }
-- {
-- ...
-- }

-- def @Strange
-- named @Format right @Val { [@Val] } right x
-- {
-- @Format x
-- }


-- def @NineSquare right x
-- {
-- def @Three { x |0.2i x |0.2i x }
-- @Three /0.2i @Three /0.2i @Three }
