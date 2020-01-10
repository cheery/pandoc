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
import qualified Data.Text as T
import qualified Data.Map as M
-- import Data.Text.IO (readFile, putStrLn)
-- import System.Environment (getArgs)
-- import Data.List (intercalate)
-- import System.Exit (exitWith, ExitCode(..))
-- import System.IO (stderr, hPutStrLn)

import Control.Monad
import Control.Monad.Except (throwError)
-- import Data.Char (isAlphaNum, isPunctuation, isSpace)
-- import Data.List (sortBy, transpose, elemIndex)
-- import qualified Data.Map as M
-- import Data.Maybe
-- import Data.Ord (comparing)
-- import qualified Data.Set as Set
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
readWordProcessing opts s = do
    let result = parseWP s
    case result of
        Left _ -> throwError (PandocParseError "bork")
        Right root ->
            return (just_paragraphs root)

just_paragraphs :: Element -> Pandoc
just_paragraphs elem =
    Pandoc (Meta { unMeta = M.empty }) (foldr g [] elem)
    where
    g :: Node -> [Block] -> [Block]
    g (Elem xs) z = foldr f z (map Elem (break_paragraphs xs))
    g _ z = z
    f :: Node -> [Block] -> [Block]
    f (Elem xs) z = Para (foldr make_inline [] xs) : z
    f _ z = z
    make_inline :: Node -> [Inline] -> [Inline]
    make_inline (Text txt) z = Str txt : z
    make_inline (Elem xs) z = Span nullAttr (foldr make_inline [] xs) : z
    make_inline (Attr _ _) z = z

-- Break text nodes into paragraphs while otherwise preserving structure.
break_paragraphs :: [Node] -> [[Node]]
break_paragraphs [] = [[]]
break_paragraphs (Text a:xs) = let
    (y:ys) = break_paragraphs xs
    in splice y ys (text_to_paragraphs a)
    where
    splice :: [Node] -> [[Node]] -> [Text] -> [[Node]]
    splice y ys [t] = (Text t:y):ys
    splice y ys (t:ts) = [Text t]:splice y ys ts
    text_to_paragraphs :: Text -> [Text]
    text_to_paragraphs a = (tk_xs (T.foldr sepfn ("",[],Any) a))
    tk_xs (cs,xs,_) = cs:xs
    sepfn :: Char -> (Text,[Text],ParaBreaker) -> (Text,[Text],ParaBreaker)
    sepfn char (cs,xs,pr) = let
        pr' = step_brk pr char
        in case (pr,pr') of
            (PRNL, Any) -> ("", T.stripStart cs:xs, Any)
            (_, _) -> (T.cons char cs, xs, pr')

break_paragraphs (other:xs) = let
    (y:ys) = break_paragraphs xs
    in ((other:y):ys)

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
