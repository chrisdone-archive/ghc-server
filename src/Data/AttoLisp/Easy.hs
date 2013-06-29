-- | A module to contain the magnitude of s-expression parsing.

module Data.AttoLisp.Easy
  (fromLispString
  ,module L)
   where

import qualified Data.AttoLisp              as L
import qualified Data.Attoparsec            as P
import qualified Data.ByteString as B

-- | Parse a single s-expr followed by optional whitespace and end of
--   file.
parseLispOnly :: B.ByteString -> Either String L.Lisp
parseLispOnly b =
    case P.parseOnly lisp b of
      Left err -> Left ("Bad s-expression: " ++ err)
      Right ok -> Right ok

  where

    lisp = L.lisp

-- | Parse a single s-expr.
fromLispString :: L.FromLisp a => B.ByteString -> Either String a
fromLispString str = L.parseEither L.parseLisp =<< parseLispOnly str
