module System.USB.HID.Parsers (-- * Top Level Parsers
                               -- All "Data.Attoparsec" parsers
                               parseHIDDesc
                              ,parsePhysDescSet
                              ,parseHIDReportDesc
                               -- ** Second Level Parsers
                              ,parseInt
                              ,convertEnum) where

import Data.Bits ((.&.),rotate,rotateR,testBit)
import Data.Attoparsec.ByteString (Parser,anyWord8,peekWord8',peekWord8,choice,(<?>),many')
import qualified Data.Attoparsec.ByteString as AB (take)
import System.USB.HID.Descriptor
import Data.Word (Word8)
import Data.ByteString (ByteString, unpack,pack)
import qualified Data.ByteString as B (zip)
import Control.Applicative ((<$>))
import Control.Monad (when, unless)

-- | Parse a 'HIDDescriptor'
parseHIDDesc :: Parser HIDDescriptor
parseHIDDesc  = do 
  hidDesc1 <- anyWord8
  hidcd <- parseVersion
  hidCC <- anyWord8
  hidND <- anyWord8
  hidDesc2 <- anyWord8
  hidDescL <- anyWord8
  hidDescT' <- peekWord8
  hidDescL' <- peekWord8
  return (HIDDescriptor hidDesc1 
                        hidcd 
                        hidCC
                        hidND
                        hidDesc2
                        (fromEnum hidDescL) 
                        hidDescT' 
                        hidDescL')

-- | Parse a 'HIDReportDesc'
parseHIDReportDesc :: Parser HIDReportDesc
parseHIDReportDesc = HIDReport <$> many' parseReportItem 

parsePhysicalDesc :: Parser HIDPhysicalDescriptor
parsePhysicalDesc = do 
  desig <- convertEnum <$> anyWord8 
  bq <- anyWord8 
  return (PD desig (convertEnum . sig3 $ bq) (convertEnum . lsig5 $ bq))

-- | Parse a 'HIDPhysDescSet' 
parsePhysDescSet :: Parser HIDPhysDescSet
parsePhysDescSet = do 
  pref <- anyWord8 
  sets <- many' parsePhysicalDesc
  return (PDS (convertEnum . sig3 $ pref) (fromEnum . lsig5 $ pref) sets)

parseVersion :: Parser Version
parseVersion = do 
  ma <- anyWord8 
  mi <- anyWord8
  return (V (binDec ma) (binDec mi))


binDec :: Word8 -> Int 
binDec w = (t * 10) + s
    where beS = 0xF0
          s = fromEnum $ w .&. beS
          t = fromEnum $ (rotate w 4) .&. beS

parseReportItem :: Parser HIDReportItem
parseReportItem = choice [HIDReportS <$> parseShortItem
                         ,HIDReportL <$> parseLongItem 
                         ]

parseShortItem :: Parser ShortItem
parseShortItem = choice [Main <$> parseMain
                        ,Global <$> parseGlobal 
                        ,Local <$> parseLocal 
                        ]

parseTop1 :: (Word8 -> Bool) -> String -> Parser a -> Parser a
parseTop1 test err parser = do 
  prefix <- peekWord8' 
  unless (test prefix) $ 
         fail err
  parser

parseLongItem :: Parser LongItem
parseLongItem = parseTop1 isLong "Not a Long Item" (return $ Long ())

parseMain :: Parser HIDMainTag
parseMain = parseTop1 isMain "Not main Type" $ choice [parseMainInput,parseCollection,parseEndCollection]

parseGlobal :: Parser HIDGlobalTag 
parseGlobal = parseTop1 isGlobal "Not Global" $ choice [parseUsagePage, parseGlobalRest]

parseLocal :: Parser HIDLocalTag
parseLocal = parseTop1 isLocal "Not Local Tag" $ choice [parseUsage,parseDesignatorI,parseDelim,parseLocalRest]

parseTop2 :: (Word8 -> Bool) -> String -> (Word8 -> Parser a) 
          -> Parser a
parseTop2 test err parseCons = do 
  prefix <- anyWord8
  unless (test . preTag $ prefix) $ 
         fail err
  parseCons (preDataL $ prefix)
  

parseDesignatorI :: Parser HIDLocalTag
parseDesignatorI = parseTop2 (== 3) "Not DesignatorI" ((DesignatorIndex <$>) . parseDesIData . fromEnum)

parseDelim :: Parser HIDLocalTag
parseDelim = parseTop2 (== 9) "Not Delimiter" ((Delimiter <$>) . parseDelimData . fromEnum)
  
parseDelimData :: Int -> Parser HIDDelimeter
parseDelimData n = toEnum <$> parseInt n 

parseDesIData :: Int -> Parser HIDDesignator
parseDesIData n = toEnum <$> parseInt n 

parseLocalRest :: Parser HIDLocalTag
parseLocalRest = do 
  prefix <- anyWord8
  let int = parseInt (fromEnum . preDataL $ prefix)
  case preTag prefix of
    1 -> UsageMinimum <$> int
    2 ->  UsageMaximum <$> int
    4 ->  DesignatorMinimum <$> int
    5 ->  DesignatorMaximum <$> int
    7 ->  StringIndex <$> int
    8 ->  StringMinimum <$> int
    9 -> StringMaximum <$> int
    _ -> fail "Not a data Int Local"

parseUsage :: Parser HIDLocalTag
parseUsage = parseTop2 (== 0) "Not Usage Tag" ((Usage <$>) . parseUsageData . fromEnum)

parseUsageData :: Int -> Parser HIDUsage
parseUsageData n = U <$> parseInt n 
   
parseUsagePage :: Parser HIDGlobalTag
parseUsagePage = parseTop2 isUsagePage "Not UsagePage" ((UsagePage <$>) . parseUsagePageData . fromEnum)

parseUsagePageData :: Int -> Parser HIDUsagePage
parseUsagePageData n = UP <$> parseInt n 

-- | @parseInt n@ parses an @Int@ of length encoded in @n@ 'Word8''s
parseInt :: Int -> Parser Int
parseInt n = byteStringToInt <$> AB.take n

parseGlobalRest :: Parser HIDGlobalTag
parseGlobalRest = do
  prefix <- anyWord8
  let int = parseInt (fromEnum . preDataL $ prefix)
  case preTag prefix of
      1  -> LogicalMinimum  <$> int
      2  -> LogicalMaximum  <$> int
      3  -> PhysicalMinimum <$> int
      4  -> PhysicalMaximum <$> int
      5  -> UnitExponent    <$> int
      6  -> Unit            <$> int
      7  -> ReportSize      <$> int
      8  -> ReportID        <$> int
      9  -> ReportCount     <$> int
      10 -> Push            <$> int
      11 -> Pop             <$> int
      _  -> fail "Not a data Int global"

byteStringToInt :: ByteString -> Int
byteStringToInt = foldl f 0 . B.zip (pack [0..])
    where f a (b,c) = 2^(8* (fromEnum b)) * (fromEnum c) + a  

parseMainInput :: Parser HIDMainTag 
parseMainInput = do
  prefix <- anyWord8
  let mainData = parseMainData (preDataL prefix)
  case preTag prefix of 
    8 -> Input <$> mainData
    9 -> Output <$> mainData
    11 -> Feature <$> mainData
    _ -> fail "Incorrect Tag"

parseCollection :: Parser HIDMainTag
parseCollection = parseTop2 isCollection "Not Collection" ((Collection <$>) . parseCollectionData)

parseEndCollection :: Parser HIDMainTag
parseEndCollection = parseTop2 isEndCollection "Not End Collection" (const (return EndCollection))
  
parseCollectionData :: Word8 -> Parser HIDCollectionData
parseCollectionData w = do
  tdata <- convertEnum <$> anyWord8
  AB.take (fromEnum (w - 1))
  return tdata

parseMainData :: Word8 -> Parser HIDMainData
parseMainData n = do 
  let a = fromEnum (if n <= 2 
                   then n
                   else 2)
  as <- AB.take a
  let bs = concatMap takingBits (unpack as) ++ (repeat False)
  return (constructHIDMainD (map fromEnum (takeBits bs)))

takeBits :: [Bool] -> [Bool]
takeBits xs = take 7 xs ++ [xs !! 9]

isMain :: Word8 -> Bool
isMain w = preType w == 0

isGlobal :: Word8 -> Bool
isGlobal w = preType w == 1

isLocal :: Word8 -> Bool
isLocal w = preType w == 2

isLong :: Word8 -> Bool 
isLong w = preTag w == 15

isUsagePage :: Word8 -> Bool
isUsagePage w = preTag w == 0

isCollection :: Word8 -> Bool
isCollection w = preTag w == 10

isEndCollection :: Word8 -> Bool
isEndCollection w = preTag w == 12

preType :: Word8 -> Word8                       
preType p = rotateR (p .&. typeM) 2
    where typeM = 0x0C

preTag  ::  Word8 -> Word8                       
preTag p = rotateR (p .&. tagM) 4
    where tagM = 0xF0

preDataL :: Word8 -> Word8           
preDataL p = p .&. dataLengthM
    where dataLengthM = 0x03

sig3 :: Word8 -> Word8 
sig3 w = rotate (w .&. m) 3
    where m = 0xE0

lsig5 :: Word8 -> Word8 
lsig5 w = w .&. m 
    where m = 0x1F

convertEnum :: (Enum a, Enum b) => a -> b
convertEnum = toEnum . fromEnum

takingBits :: Word8 -> [Bool]
takingBits x = map (testBit x) [0..7]
