module Ch24.IPAddress where

import Control.Applicative
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (digitToInt)
import Data.Functor
import Data.List (intercalate, intersperse)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric (showHex)
import Text.Parser.Combinators
import Text.Trifecta

data IPv4 = IPv4 Word32
  deriving (Eq, Ord)

instance Show IPv4 where
  show (IPv4 w) = intercalate "." (map show octets)
    where
      octets = splitBitBlocks 4 8 w

data IPv6 = IPv6 Word64 Word64
  deriving (Eq, Ord)

instance Show IPv6 where
  show (IPv6 w1 w2) = (foldr ($) "" . intersperse (":" ++) . map showHex) hs
    where
      hs = (splitBitBlocks 4 16 w1) ++ (splitBitBlocks 4 16 w2)

---

createIPv4 :: [Word8] -> IPv4
createIPv4 = IPv4 . fromIntegral . combineBitBlocks 8 . take 4

createIPv6 :: [Word16] -> IPv6
createIPv6 hs = IPv6 block1 block2
  where
    (h, t) = splitAt 4 (take 8 (hs ++ repeat 0))
    block1 = fromIntegral $ combineBitBlocks 16 h
    block2 = fromIntegral $ combineBitBlocks 16 t

combineBitBlocks :: Integral a => Int -> [a] -> Integer
combineBitBlocks b = foldl (\acc d -> fromIntegral d + shiftL acc b) 0

splitBitBlocks :: Integral a => Int -> Int -> a -> [Integer]
splitBitBlocks n b dec = (reverse . map fst . take n . drop 1) bls
  where
    bls = iterate (\(_, d) -> (d .&. (2 ^ b - 1), shiftR d b)) (0, int)
    int = fromIntegral dec

convertIPv4ToIPv6 :: IPv4 -> IPv6
convertIPv4ToIPv6 (IPv4 w32) = IPv6 0 (mask .|. (fromIntegral w32))
  where
    mask = shiftL (2 ^ 16 - 1) 32

---

hexToInteger :: [Char] -> Integer
hexToInteger = foldl (\acc d -> 16 * acc + fromIntegral (digitToInt d)) 0

zeroUpTo :: Int -> Parser a -> Parser [a]
zeroUpTo n p
  | n > 0 = (:) <$> try p <*> zeroUpTo (n - 1) p <|> return []
  | otherwise = return []

oneUpTo :: Int -> Parser a -> Parser [a]
oneUpTo n p
  | n > 0 = (:) <$> p <*> zeroUpTo (n - 1) p
  | otherwise = return []

---

octet :: Parser Word8
octet = do
  oct <- natural
  if oct > 255
    then unexpected "invalid octect"
    else return $ fromIntegral oct

ipV4 :: Parser IPv4
ipV4 = createIPv4 <$> octets
  where
    octets = (:) <$> octet <*> count 3 (char '.' *> octet)

hextet :: Parser Word16
hextet = do
  hex <- oneUpTo 4 hexDigit
  let val = hexToInteger hex
  if val > 65535
    then unexpected "invalid hextet"
    else return $ fromIntegral val

data BlockOrSkip
  = Block Word16
  | Skip
  deriving (Show)

isSkip :: BlockOrSkip -> Bool
isSkip Skip = True
isSkip (Block _) = False

ipV6 :: Parser IPv6
ipV6 =
  fmap createIPv6 hextets
  where
    skip = (char ':' >> notFollowedBy block) $> Skip
    block = fmap Block hextet
    skipOrBlock = try (skip) <|> (char ':' >> block)
    blocks = do
      x <- block
      xs <- oneUpTo 7 skipOrBlock
      return (x : xs)
    hextets = do
      blocks <- blocks
      let skipCount = length . filter (isSkip) $ blocks
          blockLength = length blocks
          expandElement x = case x of
            Skip -> replicate (9 - blockLength) 0
            (Block w) -> [w]
      _ <- if (skipCount > 1) then fail "only one skip allowed" else return ()
      return $ foldr (\x acc -> (expandElement x) ++ acc) [] blocks

---

ipV6Examples :: [String]
ipV6Examples =
  [ "0:0:0:0:0:ffff:ac10:fe01",
    "0:0:0:0:0:ffff:cc78:f",
    "FE80:0000:0000:0000:0202:B3FF:FE1E:8329",
    "2001:DB8::8:800:200C:417A",
    "FE80::0202:B3FF:FE1E:8329",
    "2001:DB8:8:800:200C:417A",
    "001:DB8::8:800::417A" -- invalid
  ]
