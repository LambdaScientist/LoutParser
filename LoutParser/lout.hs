{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
import Prelude
import Text.Parsec
import Text.Parsec.String
-- letter
-- white space quote escape comment other
-- @ab-zAB-Z_
-- space formfeed tab newline
-- "
-- \
-- # !$%&’()*+,-./0123456789:;<=>?[]^‘{|}~

-- When the comment character # is encountered, everything from that point to the end of the line is ignored.

-- object
-- → object infixop object 
-- → prefixop object
-- → object postfixop 
-- → noparsop
-- → literalword
-- → { object }
-- → object object 
-- →

data TextInput = Unanalyzed String
                 | Identifier String
                 | LiteralWord String 
                 | Symbol String
                 deriving (Show)

type Operator = TextInput
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
  | SequencesOp [Object]
  -- allows a meaning for expressions such as {}, in which an object is missing. The value of 
  -- this empty object is a rectangle of size 0 by 0, with one column mark and one row mark, that prints as nothing.
  | EmptyOp
  deriving (Show)

-- loutIdent ::Parsec String String Identity
loutIdent = many1 (letter <|> char '@') >>= return . Unanalyzed

pSymbol = many1 (oneOf "!@$%&’*+,-./:;<=>?^‘|~") >>= return . Symbol

pTextInput = (try pSymbol <|> loutIdent ) 
pPosLit = PosLit <$> pTextInput
-- comment :: Parsec String String String
comment = do char '#'
             manyTill anyChar endOfLine 
             return ()

pLoutFile = many $ (many comment *> pObject)

pObject = pInfix
      <|> pPosLit
      <|> return EmptyOp 

-- pInfix :: Parsec Object
pInfix = do left <- pObject 
            infixop <- pTextInput
            right <- pObject
            return $ InfixOp left infixop right

testFile = "/home/burhopja/Documents/burhopSrc/Joy/joy-hs/LoutParser/ex.txt"
foo = do result <- parseFromFile pPosLit testFile 
         putStrLn $ "Reading from: " ++ testFile
         putStrLn $ "\nResult: \n"
         print result

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
