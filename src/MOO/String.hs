
-- | Abstract MOO string type
module MOO.String (
    MOOString

  -- * Creation and elimination
  , fromText
  , fromBinary
  , fromString
  , toText
  , toCaseFold
  , toBinary
  , toString
  , toRegexp
  , singleton
  , empty

  -- * Basic interface
  , append
  , tail
  , null
  , length
  , compareLength
  , storageBytes
  , equal

  -- * Transformations
  , intercalate

  -- * Special folds
  , concat
  , concatMap

  -- * Substrings
  -- ** Breaking strings
  , take
  , drop
  , splitAt
  , breakOn
  , breakOnEnd
  , break
  -- ** Breaking into many substrings
  , splitOn
  -- ** Breaking into lines and words
  , words
  , unwords

  -- * Predicates
  , validChar
  , isPrefixOf

  -- * Indexing
  , index
  ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (isAscii, isPrint, isHexDigit, digitToInt, intToDigit)
import Data.Function (on)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Monoid (Monoid(mempty, mappend, mconcat))
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.Storable (sizeOf)
import Prelude hiding (tail, null, length, concat, concatMap, take, drop,
                       splitAt, break, words, unwords)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Prelude

import MOO.Builtins.Match (Regexp, newRegexp)

type CompiledRegexp = Either String Regexp

data CachedReps = CachedReps {
    binaryData            :: Maybe ByteString
  , caseInsensitiveRegexp :: CompiledRegexp
  , caseSensitiveRegexp   :: CompiledRegexp
  }

mkCachedReps :: Text -> CachedReps
mkCachedReps text = CachedReps {
    binaryData            = decodeBinary text
  , caseInsensitiveRegexp = newRegexp text False
  , caseSensitiveRegexp   = newRegexp text True
  }

data MOOString = MOOString {
    toText     :: Text
  , toCaseFold :: Text
  , length     :: Int
  , cachedReps :: CachedReps
  }

instance IsString MOOString where
  fromString = fromText . T.pack

instance Eq MOOString where
  (==) = (==) `on` toCaseFold

instance Ord MOOString where
  compare = compare `on` toCaseFold

instance Hashable MOOString where
  hashWithSalt salt = hashWithSalt salt . toCaseFold

instance Monoid MOOString where
  mempty  = empty
  mappend = append
  mconcat = concat

instance Show MOOString where
  show = show . toText

fromText :: Text -> MOOString
fromText text = MOOString {
    toText     = text
  , toCaseFold = caseFold text
  , length     = T.length text
  , cachedReps = mkCachedReps text
  }

fromBinary :: ByteString -> MOOString
fromBinary bytes =
  let str = fromText (encodeBinary bytes)
  in str { cachedReps = (cachedReps str) { binaryData = Just bytes } }

toString :: MOOString -> String
toString = T.unpack . toText

toBinary :: MOOString -> Maybe ByteString
toBinary = binaryData . cachedReps

toRegexp :: Bool  -- ^ case-matters
         -> MOOString -> CompiledRegexp
toRegexp False = caseInsensitiveRegexp . cachedReps
toRegexp True  = caseSensitiveRegexp   . cachedReps

-- | Case-fold the argument, returning the same argument if the result is
-- unchanged to avoid wasting memory.
caseFold :: Text -> Text
caseFold text
  | text == folded = text
  | otherwise      = folded
  where folded = T.toCaseFold text

-- | Encode a MOO /binary string/.
encodeBinary :: ByteString -> Text
encodeBinary = TE.decodeLatin1 . BSL.toStrict . BSB.toLazyByteString . BP.primMapByteStringBounded encode where
  encode = BP.condB (\b -> b < 128 && let c = w2c b in isPrint c && c /= '~')
    (BP.liftFixedToBounded BP.word8)
    (BP.liftFixedToBounded $ (,) '~' BP.>$< BP.char8 BP.>*< BP.word8HexFixed)

-- | Decode a MOO /binary string/ or return 'Nothing' if the string is
-- improperly formatted.
decodeBinary :: Text -> Maybe ByteString
decodeBinary = fmap BS.pack . decode . T.unpack
  where decode :: String -> Maybe [Word8]
        decode ('~':q:r:rest) = do
          q' <- fromHex q
          r' <- fromHex r
          let b = 16 * q' + r'
          (b :) <$> decode rest
        decode ('~':_) = Nothing
        decode (c:rest)
          | b < 128 && isPrint c = (b :) <$> decode rest
          | otherwise            = Nothing
          where b = c2w c
        decode [] = Just []

        fromHex :: Char -> Maybe Word8
        fromHex c
          | isHexDigit c = Just b
          | otherwise    = Nothing
          where b = fromIntegral (digitToInt c)

-- | May the given character appear in a MOO string?
validChar :: Char -> Bool
validChar c = isAscii c && (isPrint c || c == '\t')

singleton :: Char -> MOOString
singleton = fromText . T.singleton

storageBytes :: MOOString -> Int
storageBytes str = sizeOf 'x' * (length str + 1) +
                   sizeOf (undefined :: Int) * 4

-- | Test two strings for indistinguishable (case-sensitive) equality.
equal :: MOOString -> MOOString -> Bool
equal = (==) `on` toText

empty :: MOOString
empty = fromText T.empty

tail :: MOOString -> MOOString
tail = fromText . T.tail . toText

append :: MOOString -> MOOString -> MOOString
append str1 str2 = fromText $ toText str1 `T.append` toText str2

null :: MOOString -> Bool
null = T.null . toText

compareLength :: MOOString -> Int -> Ordering
compareLength str = T.compareLength (toText str)

intercalate :: MOOString -> [MOOString] -> MOOString
intercalate sep = fromText . T.intercalate (toText sep) . map toText

concat :: [MOOString] -> MOOString
concat = fromText . T.concat . map toText

concatMap :: (Char -> MOOString) -> MOOString -> MOOString
concatMap f = fromText . T.concatMap (toText . f) . toText

take :: Int -> MOOString -> MOOString
take len = fromText . T.take len . toText

drop :: Int -> MOOString -> MOOString
drop len = fromText . T.drop len . toText

splitAt :: Int -> MOOString -> (MOOString, MOOString)
splitAt n str = (fromText prefix, fromText remainder)
  where (prefix, remainder) = T.splitAt n (toText str)

-- XXX Need caseless versions...

breakOn :: MOOString -> MOOString -> (MOOString, MOOString)
breakOn sep str = (fromText before, fromText match)
  where (before, match) = T.breakOn (toText sep) (toText str)

breakOnEnd :: MOOString -> MOOString -> (MOOString, MOOString)
breakOnEnd sep str = (fromText before, fromText match)
  where (before, match) = T.breakOnEnd (toText sep) (toText str)

break :: (Char -> Bool) -> MOOString -> (MOOString, MOOString)
break p str = (fromText prefix, fromText remainder)
  where (prefix, remainder) = T.break p (toText str)

splitOn :: MOOString -> MOOString -> [MOOString]
splitOn sep = map fromText . T.splitOn (toText sep) . toText

--

words :: MOOString -> [MOOString]
words = map fromText . T.words . toText

unwords :: [MOOString] -> MOOString
unwords = fromText . T.unwords . map toText

isPrefixOf :: MOOString -> MOOString -> Bool
isPrefixOf = T.isPrefixOf `on` toCaseFold

index :: MOOString -> Int -> Char
index str =  T.index (toText str)
