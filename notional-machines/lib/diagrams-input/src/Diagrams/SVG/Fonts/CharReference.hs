module Diagrams.SVG.Fonts.CharReference (charsFromFullName, characterStrings) where
import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.List (sortBy)

charRef :: Parser Int
charRef
    = do
      _ <- try (string (T.pack "&#x"))
      d <- hexadecimal
      _ <- char ';'
      return d
 <|>  do
      _ <- try (string (T.pack "&#"))
      d <- decimal
      _ <- char ';'
      return d
 <|>  do
      c <- anyChar
      return (fromEnum c)
      <?> "character reference"

charRefs :: Parser [Int]
charRefs = do l <- many1 charRef
              return l

fromCharRefs :: T.Text -> [Int]
fromCharRefs str
  = case (parseOnly charRefs str) of
           Right x -> x
           Left _ -> []

-- | Parsing of xml character references.
--
--   I.e. \"\&\#x2e\;\&\#x2e\;\&\#x2e\;\" is converted into a list of three Chars.
--
--        \"ffb\" is also parsed and converted into three Chars (not changing it).
charsFromFullName :: String -> String
charsFromFullName str = map toEnum ( fromCharRefs (T.pack str) )


-- | A string represents a glyph, i.e. the ligature \"ffi\" is a string that represents the ligature glyph ffi
characterStrings :: String -> [String] -> [T.Text]
characterStrings str ligs | null ligs = map ((T.pack).(\x->[x])) str
                          | otherwise = case parseOnly myParser (T.pack str)
                                           of Right x -> x
                                              Left  _ -> []
  where myParser = many (try ligatures <|> charToText)
        ligatures = buildChain $ sortBy -- sort so that the longest ligatures come first, i.e. "ffi", "ff", ..
                                 (\x y -> compare (length y) (length x)) $ ligs
        buildChain []     = string (T.pack "") -- will never be called, just to get rid of the warning message
        buildChain [x]    = parseLigature x -- try to parse with the first parsers in the chain first
        buildChain (x:xs) = try (parseLigature x) <|> buildChain xs
        parseLigature x = string (T.pack x)
        charToText = do c <- anyChar -- or accept a single char
                        return (T.singleton c)
