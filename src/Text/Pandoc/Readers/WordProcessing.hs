{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RelaxedPolyRec      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Readers.WordProcessing
   Copyright   : 
   License     : GNU GPL, version 2 or above

   Maintainer  : 
   Stability   : 
   Portability : 

Conversion of wordprocessing-formatted text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.WordProcessing (readWordProcessing ) where

import Prelude
import Data.Text (Text, pack, groupBy, head)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
-- import Data.Text.IO (readFile, putStrLn)
-- import System.Environment (getArgs)
-- import Data.List (intercalate)
-- import System.Exit (exitWith, ExitCode(..))
-- import System.IO (stderr, hPutStrLn)

--import Control.Monad
import Control.Monad.Except (throwError)
-- import Data.Char (isAlphaNum, isPunctuation, isSpace)
-- import Data.List (sortBy, transpose, elemIndex)
-- import qualified Data.Map as M
-- import Data.Maybe
-- import Data.Ord (comparing)
import qualified Data.Set as S
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.ByteString.Lazy as BL
-- import System.FilePath (addExtension, takeExtension)
-- import Text.HTML.TagSoup
-- import Text.Pandoc.Builder (Blocks, Inlines)
-- import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad (..)) --, report)
import Text.Pandoc.Definition
-- import Text.Pandoc.Emoji (emojiToInline)
import Text.Pandoc.Error
-- import Text.Pandoc.Logging
import Text.Pandoc.Options
-- import Text.Pandoc.Parsing hiding (tableWith)
-- import Text.Pandoc.Readers.HTML (htmlInBalanced, htmlTag, isBlockTag,
--                                  isCommentTag, isInlineTag, isTextTag)
-- import Text.Pandoc.Readers.LaTeX (applyMacros, rawLaTeXBlock, rawLaTeXInline)
-- import Text.Pandoc.Shared
-- import qualified Text.Pandoc.UTF8 as UTF8
-- import Text.Pandoc.XML (fromEntities)
-- import Text.Pandoc.Readers.Metadata (yamlBsToMeta)
 
-- Turn wordprocessing document into Pandoc document.
readWordProcessing :: PandocMonad m
                   => ReaderOptions
                   -> Text
                   -> m Pandoc
readWordProcessing _ s = do
    let result = parseWP s
    case result of
        Left _ -> throwError (PandocParseError "bork")
        Right root ->
            case read_dtd root of
                Just root' -> as_toplevel root'
                Nothing -> throwError (PandocParseError "dtd;pandoc -attribute missing, is this a pandoc wp file?")

-- DTD recognition
read_dtd :: Element -> Maybe Element
read_dtd (Attr "dtd" ["pandoc"]:eleme) = Just eleme
read_dtd (Text t:Attr "dtd" ["pandoc"]:eleme) = Just (Text t:eleme)
read_dtd _ = Nothing

data ToplevelDef = MetaDef Text
                 | BlockDef BlockDef

data BlockDef = ParDef
              | PlainDef 
              | HeaderDef Int (Text, [Text])
              | CodeBlockDef (Text, [Text])
              | DivDef (Text, [Text])
              -- | LineBlockDef
              -- | RawBlockDef Format
              -- | BlockQuoteDef 
              -- | OrderedListDef Int ListNumberStyle ListNumberDelim
              -- | BulletListDef
              -- | TableDef

        -- | LineBlock [[Inline]]  -- ^ Multiple non-breaking lines
        -- | RawBlock Format Text -- ^ Raw block ?not sure if I use it here.
        -- | BlockQuote [Block]    -- ^ Block quote (list of blocks)
        -- | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
        --                         -- and a list of items, each a list of blocks)
        --type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)
        -- 
        -- -- | Style of list numbers.
        -- data ListNumberStyle = DefaultStyle
        --                      | Example
        --                      | Decimal
        --                      | LowerRoman
        --                      | UpperRoman
        --                      | LowerAlpha
        --                      | UpperAlpha deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
        -- 
        -- -- | Delimiter of list numbers.
        -- data ListNumberDelim = DefaultDelim
        --                      | Period
        --                      | OneParen
        --                      | TwoParens deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
        --
        -- | BulletList [[Block]]  -- ^ Bullet list (list of items, each
        --                         -- a list of blocks)
        -- | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
        --                         -- Each list item is a pair consisting of a
        --                         -- term (a list of inlines) and one or more
        --                         -- definitions (each a list of blocks)
        -- | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table,
        --                         -- with caption, column alignments (required),
        --                         -- relative column widths (0 = default),
        --                         -- column headers (each a list of blocks), and
        --                         -- rows (each a list of lists of blocks)


data MetaDef = MetaMapDef
             | MetaListDef
             | MetaTrueDef
             | MetaFalseDef
             | MetaStringDef
             | MetaInlinesDef
             | MetaBlocksDef

as_toplevel :: PandocMonad m => Element -> m (Pandoc)
as_toplevel xs = (mapM toplevel_map xs) >>= return . (foldl mappend mempty)

as_blocks :: PandocMonad m => Element -> m [Block] 
as_blocks = concatMapM block_map

as_meta_value :: PandocMonad m => Element -> m MetaValue
as_meta_value xs = do
    md <- identify_it identify_meta (return MetaInlinesDef) xs
    build_meta_value md xs

toplevel_map :: PandocMonad m => Node -> m Pandoc
toplevel_map (Text _) = return (Pandoc (Meta { unMeta = M.empty }) [])
toplevel_map (Attr "meta" [key, str]) = do
    return (Pandoc (Meta { unMeta = M.singleton key (MetaString str) }) [])
toplevel_map (Attr "hr" []) = return (Pandoc (Meta { unMeta = M.empty }) [HorizontalRule])
toplevel_map (Attr "null" []) = return (Pandoc (Meta { unMeta = M.empty }) [Null])
toplevel_map (Elem xs) = do
    bd <- identify_it identify_toplevel (return (BlockDef ParDef)) xs
    build_toplevel_elem bd xs
toplevel_map _ = return (Pandoc (Meta { unMeta = M.empty }) [])

block_map :: PandocMonad m => Node -> m [Block]
block_map (Text _) = return []
block_map (Attr "hr" []) = return [HorizontalRule]
block_map (Attr "null" []) = return [Null]
block_map (Elem xs) = do
    bd <- identify_it identify_block (return ParDef) xs
    build_block_elem bd xs
block_map _ = return []

identify_it :: PandocMonad m
            => (Node -> m (Maybe a)) -- Selector
            -> m a                   -- Empty element
            -> Element -> m a
identify_it fn e node = mapMaybeM fn node >>= select
    where
    select (_:_:_) = throwError (PandocParseError "ambiguous attributes in an element")
    select [a] = return a
    select [] = e

identify_toplevel :: PandocMonad m => Node -> m (Maybe ToplevelDef)
identify_toplevel (Attr "meta" [name]) = (return . Just) (MetaDef name)
identify_toplevel a = do
    bd <- identify_block a
    return (bd >>= Just . BlockDef)

identify_block :: PandocMonad m => Node -> m (Maybe BlockDef)
identify_block (Attr "plain" []) = (return . Just) (PlainDef)
identify_block (Attr "header" [depth]) = case readMaybe (T.unpack depth) of
    Nothing -> throwError (PandocParseError "non-integer header argument")
    Just i -> (return . Just) (HeaderDef i ("", []))
identify_block (Attr "header" [depth, sel]) = case readMaybe (T.unpack depth) of
    Nothing -> throwError (PandocParseError "non-integer header argument")
    Just i -> case identify_selector sel of
        Nothing -> throwError (PandocParseError "malformed selector string in header")
        Just k -> (return . Just) (HeaderDef i k)
identify_block (Attr "code" []) = (return . Just) (CodeBlockDef ("", []))
identify_block (Attr "code" [sel]) = case identify_selector sel of
    Nothing -> throwError (PandocParseError "malformed selector string in code block")
    Just k -> (return . Just) (CodeBlockDef k)
identify_block (Attr sel []) = case identify_selector sel of
    Nothing -> return Nothing
    Just k -> (return . Just) (DivDef k)
identify_block _ = return Nothing

identify_meta :: PandocMonad m => Node -> m (Maybe MetaDef)
identify_meta (Attr "map" []) = (return . Just) (MetaMapDef)
identify_meta (Attr "list" []) = (return . Just) (MetaListDef)
identify_meta (Attr "true" []) = (return . Just) (MetaTrueDef)
identify_meta (Attr "false" []) = (return . Just) (MetaFalseDef)
identify_meta (Attr "str" []) = (return . Just) (MetaStringDef)
identify_meta (Attr "^" []) = (return . Just) (MetaBlocksDef)
identify_meta _ = return Nothing

identify_key :: PandocMonad m => Node -> m (Maybe Text)
identify_key (Attr "key" [key]) = (return . Just) key
identify_key _ = return Nothing

build_toplevel_elem :: PandocMonad m => ToplevelDef -> Element -> m Pandoc
build_toplevel_elem (MetaDef name) xs = do
    new_value <- as_meta_value xs
    return (Pandoc (Meta { unMeta = M.singleton name new_value }) [])
build_toplevel_elem (BlockDef bd) xs = do
    build_block_elem bd xs >>= return . (Pandoc (Meta { unMeta = M.empty }))

build_block_elem :: PandocMonad m => BlockDef -> Element -> m [Block]
build_block_elem (ParDef) xs = do
    return (map (Para . as_inline) (break_paragraphs xs))
build_block_elem (PlainDef) xs = do
    return [Plain (as_inline xs)]
build_block_elem (HeaderDef i k) xs = do
    return [Header i (as_attrs k xs) (as_inline xs)]
build_block_elem (CodeBlockDef k) xs = do
    return [CodeBlock (as_attrs k xs) (as_plain_text xs)]
    -- TODO: Add possibility to wrap code-part into element.
build_block_elem (DivDef k) xs =
    -- TODO: Add possibility to provide actual block div.
    return [Div (as_attrs k xs) [Plain (as_inline xs)]]

build_meta_value :: PandocMonad m => MetaDef -> Element -> m MetaValue
build_meta_value MetaMapDef xs =
    let each ys = do
            key <- identify_it identify_key
                               (throwError (PandocParseError "key missing in a map field"))
                               ys
            mv <- as_meta_value ys
            return (key, mv)
    in mapM each (sub_elements xs) >>= return . MetaMap . M.fromList
build_meta_value MetaListDef xs =
    mapM as_meta_value (sub_elements xs) >>= return . MetaList
build_meta_value MetaTrueDef _ = return (MetaBool True)
build_meta_value MetaFalseDef _ = return (MetaBool False)
build_meta_value MetaStringDef xs = return (MetaString (as_plain_text xs))
build_meta_value MetaInlinesDef xs = return (MetaInlines (as_inline xs))
build_meta_value MetaBlocksDef xs = do
    as_blocks xs >>= return . MetaBlocks

as_inline :: Element -> [Inline]
as_inline = foldr make_inline []

make_inline :: Node -> [Inline] -> [Inline]
make_inline (Text txt) z = Str txt : z
make_inline (Elem xs) z = Span nullAttr (foldr make_inline [] xs) : z
make_inline (Attr "_" _) z = Space : z -- Inter-word space
make_inline (Attr "-" _) z = SoftBreak : z -- Soft line break
make_inline (Attr "br" _) z = LineBreak : z -- Hard line break
make_inline (Attr _ _) z = z

-- (_) as 'inter word space'
-- (-) as 'soft break'
-- (br) as 'hard break'

-- Identify the role of the element.
-- Create pipe to decorate it.
--        | Emph [Inline]         -- ^ Emphasized text (list of inlines)
--        | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
--        | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
--        | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
--        | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
--        | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
--        | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
--          (SingleQuote | DoubleQuote)
--
--        These may 'alternate'
--
--        | Link Attr [Inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
--          (href;src)
--          (href;src;#ident.class)
--        | Span Attr [Inline]    -- ^ Generic inline container with attributes
--          (span)
--          (span;#ident.class)
--        | Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
--          (img;src) should be allowed on block-level as well.
--          (img;src;#ident.class)
--        | Code Attr Text      -- ^ Inline code (literal)
--          (code)
--          (code;#ident.class)
--
-- Handle ins-attrs.

-- (texmath)
--    data Inline
--        | Math MathType Text  -- ^ TeX math (literal)
--          (math)
--        | Note [Block]          -- ^ Footnote or endnote
--          (note) (^) -trick here as well, down to 'Plain' if not such item.
--
--        | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
--        Not sure how to handle the complex structure.
--
--        | RawInline Format Text -- ^ Raw inline
--          Not sure I want to use it.

-- use AlignDefault on default
identify_align :: PandocMonad m => Node -> m (Maybe Alignment)
identify_align (Attr "left" []) = (return . Just) AlignLeft
identify_align (Attr "right" []) = (return . Just) AlignRight
identify_align (Attr "center" []) = (return . Just) AlignCenter
identify_align _ = return Nothing

        -- TableCell is just [Block], can do the (^) -trick to handle it.
        --
        -- could look up all #xxx.yy.z -attributes and act on them.
        --else if S.member ("div",1) sh then
        -- | Div Attr [Block]      -- ^ Generic block container with attributes
        --   Use the (^) -trick here as well.
        --   Demote down to 'Plain' if it's not a full block.
        
        -- Consider making these short attributes.
        -- --more likely they don't interfere with ordinary use if done that way.


-- Used for node transformation into blocks and inline elements.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = (mapM f xs >>= return . concat)

-- Used for node identification
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = (mapM f xs >>= return . catMaybes)

-- Attribute handling.
as_attrs :: (Text, [Text]) -> Element -> Attr
as_attrs (ident, classes) xs = (ident, classes, mapMaybe identify_attrib xs)

identify_attrib :: Node -> Maybe (Text, Text)
identify_attrib (Attr "\"" [name, value]) = Just (name, value)
identify_attrib _ = Nothing

-- Treat whole element as plain text
as_plain_text :: Element -> Text
as_plain_text (Text t:xs) = T.concat [t, as_plain_text xs]
as_plain_text (Elem e:xs) = T.concat [as_plain_text e, as_plain_text xs]
as_plain_text (_:xs) = as_plain_text xs
as_plain_text [] = ""

-- Take sub-elements out of an element.
sub_elements :: Element -> [Element]
sub_elements (Elem e:xs) = e : sub_elements xs
sub_elements (_:xs) = sub_elements xs
sub_elements [] = []

-- Identifies selectors (#|.)text(.text)*
identify_selector :: Text -> Maybe (Text, [Text])
identify_selector t | (T.take 1 t == "#") = case T.splitOn "." (T.drop 1 t) of
    (x:xs) -> Just (x, xs)
    _ -> Just ("", [])
identify_selector t | (T.take 1 t == ".") = Just ("", T.splitOn "." (T.drop 1 t))
identify_selector _ = Nothing

-- Target: (URL, title) -- 'title' as different attribute.

-- Break text nodes into paragraphs while otherwise preserving structure.
break_paragraphs :: [Node] -> [[Node]]
break_paragraphs [] = [[]]
break_paragraphs (Text txt:zs) = case break_paragraphs zs of
    (y:ys) -> splice y ys (text_to_paragraphs txt)
    _ -> undefined
    where
    splice :: [Node] -> [[Node]] -> [Text] -> [[Node]]
    splice y ys [t] = (Text t:y):ys
    splice y ys (t:ts) = [Text t]:splice y ys ts
    splice _ _ _ = undefined
    text_to_paragraphs :: Text -> [Text]
    text_to_paragraphs a = (tk_xs (T.foldr sepfn ("",[],Any) a))
    tk_xs (cs,xs,_) = cs:xs
    sepfn :: Char -> (Text,[Text],ParaBreaker) -> (Text,[Text],ParaBreaker)
    sepfn char (cs,xs,pr) = let
        pr' = step_brk pr char
        in case (pr,pr') of
            (PRNL, Any) -> ("", T.stripStart cs:xs, Any)
            (_, _) -> (T.cons char cs, xs, pr')

break_paragraphs (other:xs) = case break_paragraphs xs of
    (y:ys) -> ((other:y):ys)
    _ -> undefined

data ParaBreaker = Any | NLSP | PRNL

step_brk :: ParaBreaker -> Char -> ParaBreaker
step_brk Any '\n'  = NLSP
step_brk Any  _    = Any
step_brk NLSP '\n' = PRNL
step_brk NLSP '\t' = NLSP
step_brk NLSP ' '  = NLSP
step_brk NLSP _    = Any
step_brk PRNL '\n' = PRNL
step_brk PRNL '\t' = PRNL
step_brk PRNL ' '  = PRNL
step_brk PRNL _    = Any


-- This is a generated LR0 grammar
-- python parsergen4.py ../wordprocessing/grammar.txt
--        -p -o ../wordprocessing/wp_parser2_tables.py --lr0
--symtab :: [String]
--symtab = ["LB", "LP", "RB", "RP", "SEP", "TEXT", "element", "block", "attribute"]

rtab :: [(Int, [Bool])]
rtab = [ (9, [True]),
         (6, [True]),
         (6, [True, False]),
         (6, [True, False]),
         (7, [False]),
         (7, [True, True]),
         (8, [False]),
         (8, [False, True]),
         (8, [True, False]),
         (8, [True, False, True])]

state :: [[Int]]
state = [ [1, 2, 0, 0, 0, -2, -1, 0, 0],
          [1, 2, -5, 0, 0, -2, 3, -3, 0],
          [0, 0, 0, -7, 5, 4, 0, 0, -4],
          [1, 2, -5, 0, 0, -2, 3, -6, 0],
          [0, 0, 0, -8, 6, 0, 0, 0, 0],
          [0, 0, 0, -7, 5, 4, 0, 0, -9],
          [0, 0, 0, -7, 5, 4, 0, 0, -10]]

-- Control character table
ctrtab :: Char -> Int
ctrtab '\x10' = 1 -- ctrl-P, DLE, 'LP'
ctrtab '\x11' = 3 -- ctrl-Q, DC1, 'RP'
ctrtab '\x12' = 0 -- ctrl-R, DC2, 'LB'
ctrtab '\x13' = 2 -- ctrl-S, DC3, 'RB'
ctrtab '\x16' = 4 -- ctrl-V, SYN, 'SEP'
ctrtab _    = 5   -- 'TEXT'

-- Output from the parser
type Element = [Node]
type ParseError = ()

data Node = Text Text | Elem Element | Attr Text [Text]
    deriving (Show)

-- Tokenizing faces no error conditions.
tokenize :: Text -> [(Int, Text)]
tokenize = map nameIt . groupText
    where
    nameIt x = (ctrtab (Data.Text.head x), x)

groupText :: Text -> [Text]
groupText = groupBy rule
    where
    rule x y = (ctrtab x == 5) && (ctrtab y == 5)

-- The parsing routine 
data Stack = Initial | Cell Stack Int Node
data Parse = Failure | OK [Node] Stack

parseWP :: Text -> Either ParseError Element
parseWP text = finish (foldr f (\x -> x) (tokenize text) (OK [] Initial))
    where
    f (g,txt) remaining = remaining . scan (g, Text txt)

scan :: (Int, Node) -> Parse -> Parse
scan _ Failure = Failure
scan (g,x) (OK eleme s) =
    case get_action s g of
        Shift k -> (OK eleme (Cell s k x))
        Reduce n -> let
            (g',r) = rtab !! n
            (s',z) = peel x s r
            in if n == 0
                then (OK (reverse z ++ eleme) s')
                else scan (g',interp n (reverse z)) (OK eleme s')
        Error -> Failure

finish :: Parse -> Either ParseError Element
finish Failure = Left ()
finish (OK eleme Initial) = Right (reverse eleme)
finish (OK _ _) = Left ()

peel :: Node -> Stack -> [Bool] -> (Stack, [Node])
peel n a [True] = (a, [n])
peel _ a [False] = (a, [])
peel n (Cell p _ m) (True:q) = let (p',ns) = peel m p q in (p', n:ns)
peel _ (Cell p _ m) (False:q) = let (p',ns) = peel m p q in (p', ns)
peel _ _ _ = error "Text.Pandoc.Readers.WordProcessing peel bug"

get_action :: Stack -> Int -> Action
get_action s g =
    let k = get_state s
        a = (state !! k) !! g
    in if a < 0
        then Reduce (-1 - a)
        else if a == 0
            then Error
            else Shift a

get_state :: Stack -> Int
get_state (Initial)    = 0
get_state (Cell _ k _) = k

data Action = Shift Int | Reduce Int | Error

interp :: Int -> [Node] -> Node
interp 0 [a] = a
interp 1 [text] = text
interp 2 [block] = block
interp 3 [attr] = attr
interp 4 [] = Elem []
interp 5 [item, Elem block] = Elem (item : block)
interp 6 [] = Attr (pack "") []
interp 7 [Text a] = Attr a []
interp 8 [Attr b blk] = Attr (pack "") (b:blk)
interp 9 [Text a, Attr b blk] = Attr a (b:blk)
interp a b = (error (show (a,b)))
