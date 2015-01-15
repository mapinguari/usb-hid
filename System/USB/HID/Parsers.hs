module System.USB.HID.Parsers (-- * Top Level Parsers
                               -- All "Data.Attoparsec" parsers
                               parseHIDDesc
                              ,parsePhysDescSet
                              ,parseHIDReportDesc
                               -- ** Second Level Parsers
                              ,parseInt) where

import Data.Bits ((.&.),rotate,rotateR)
import Data.Bits.Bitwise (fromListBE,fromListLE,toListLE)
import Data.Attoparsec.ByteString (Parser,anyWord8,peekWord8',peekWord8,choice,(<?>),many')
import qualified Data.Attoparsec.ByteString as AB (take)
import System.USB.HID.Descriptor
import Data.Word (Word8)
import Data.ByteString (ByteString, unpack,pack)


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
  return (HIDDescriptor hidDesc1 hidcd hidCC hidND hidDesc2 (fromEnum hidDescL) hidDescT' hidDescL')

-- | Parse a 'HIDReportDesc'
parseHIDReportDesc :: Parser HIDReportDesc
parseHIDReportDesc = many' parseReportItem >>= return . HIDReport

parsePhysicalDesc :: Parser HIDPhysicalDescriptor
parsePhysicalDesc = do 
  desig <- anyWord8 >>= return . toEnum . fromEnum
  bq <- anyWord8 
  return (PD desig (toEnum . fromEnum . sig3 $ bq) (toEnum . fromEnum . lsig5 $ bq))

-- | Parse a 'HIDPhysDescSet' 
parsePhysDescSet :: Parser HIDPhysDescSet
parsePhysDescSet = do 
  pref <- anyWord8 
  sets <- many' parsePhysicalDesc
  return (PDS (toEnum . fromEnum . sig3 $ pref) (fromEnum . lsig5 $ pref) sets)

parseVersion :: Parser Version
parseVersion = do 
  ma <- anyWord8 
  mi <- anyWord8
  return (V (binDec ma) (binDec mi))


binDec :: Word8 -> Int 
binDec w = (t * 10) + s
    where beS = fromListLE [True,True,True,True,False,False,False,False]
          s = fromEnum $ w .&. beS
          t = fromEnum $ (rotate w 4) .&. beS

parseReportItem :: Parser HIDReportItem
parseReportItem = choice [parseShortItem >>= return . HIDReportS,
                          parseLongItem >>= return . HIDReportL]

parseShortItem :: Parser ShortItem
parseShortItem = choice [parseMain >>= return . Main,parseGlobal >>= return . Global,parseLocal >>= return . Local]

parseLongItem :: Parser LongItem
parseLongItem = do 
  prefix <- peekWord8' 
  if isLong prefix 
  then return (Long ())
  else fail "Not a Long Item"


parseMain :: Parser HIDMainTag
parseMain = do
  prefix <- peekWord8'
  if not.isMain $ prefix
  then fail "Not main type"
  else choice [parseMainInput,parseCollection,parseEndCollection] <?> "Could not parse a main tag"

parseGlobal :: Parser HIDGlobalTag 
parseGlobal = do
  prefix <- peekWord8'
  if isGlobal prefix
  then choice [parseUsagePage, parseGlobalRest]
  else fail "Not Global"

parseLocal :: Parser HIDLocalTag
parseLocal = do 
  prefix <- peekWord8'
  if isLocal prefix
  then choice [parseUsage,parseDesignatorI,parseDelim,parseLocalRest]
  else fail "Not Local Tag"

parseDesignatorI :: Parser HIDLocalTag
parseDesignatorI = do 
  prefix <- anyWord8 
  tdata <- if preTag prefix == 3
           then parseDesIData (fromEnum (preDataL prefix))
           else fail "Not DesignatorI"
  return . DesignatorIndex $ tdata

parseDelim :: Parser HIDLocalTag
parseDelim = do 
  prefix <- anyWord8 
  tdata <- if preTag prefix == 9
           then parseDelimData (fromEnum (preDataL prefix))
           else fail "Not Delimiter"
  return . Delimiter $ tdata
  
parseDelimData :: Int -> Parser HIDDelimeter
parseDelimData n = parseInt n >>= return . toEnum

parseDesIData :: Int -> Parser HIDDesignator
parseDesIData n = parseInt n >>= return . toEnum

parseLocalRest :: Parser HIDLocalTag
parseLocalRest = do 
  prefix <- anyWord8
  tdata <- if let t = preTag prefix in 
              t < 10 && not (t `elem` [0,3,9])
           then parseInt (fromEnum . preDataL $ prefix)
           else fail "Not a data Int Local"
  return (intToLocalTag (fromEnum . preTag $ prefix) tdata)
  

parseUsage :: Parser HIDLocalTag
parseUsage = do 
  prefix <- anyWord8
  tdata <- if preTag prefix == 0
           then parseUsageData (fromEnum . preDataL $ prefix)
           else fail "Not Usage Tag"
  return . Usage $ tdata

parseUsageData :: Int -> Parser HIDUsage
parseUsageData n = parseInt n >>= return . U
   

parseUsagePage :: Parser HIDGlobalTag
parseUsagePage = do
  prefix <- anyWord8 
  tdata <- if isUsagePage prefix 
           then parseUsagePageData (fromEnum (preDataL prefix))
           else fail "Not UsagePage"
  return $ UsagePage tdata

parseUsagePageData :: Int -> Parser HIDUsagePage
parseUsagePageData n = parseInt n >>= return . UP

-- | @parseInt n@ parses an @Int@ of length encoded in @n@ 'Word8''s
parseInt :: Int -> Parser Int
parseInt n = do
  bs <- AB.take n
  return (byteStringToInt bs)

parseGlobalRest :: Parser HIDGlobalTag
parseGlobalRest = do
  prefix <- anyWord8
  tdata <- if preTag prefix `elem` [1..11]
           then parseInt (fromEnum . preDataL $ prefix)
           else fail "Not a data Int global"
  return (intToGlobalTag (fromEnum . preTag $ prefix) tdata)
  
intToGlobalTag :: Int -> Int -> HIDGlobalTag
intToGlobalTag i 
 | i == 1 = LogicalMinimum 
 | i == 2 = LogicalMaximum
 | i == 3 = PhysicalMinimum 
 | i == 4 = PhysicalMaximum 
 | i == 5 = UnitExponent 
 | i == 6 = Unit 
 | i == 7 = ReportSize 
 | i == 8 = ReportID 
 | i == 9 = ReportCount 
 | i == 10 = Push 
 | i == 11 = Pop  

intToLocalTag :: Int -> Int -> HIDLocalTag
intToLocalTag i 
    | i == 1 = UsageMinimum
    | i == 2 = UsageMaximum
    | i == 4 = DesignatorMinimum 
    | i == 5 = DesignatorMaximum 
    | i == 7 = StringIndex 
    | i == 8 = StringMinimum
    | i == 9 = StringMaximum 

byteStringToInt :: ByteString -> Int
byteStringToInt = fromEnum . sum . zipWith f [0..] . unpack
    where f a b = 2^(8*a) * b 

parseMainInput :: Parser HIDMainTag 
parseMainInput = do
  prefix <- anyWord8
  constr <- chooseMainTag (preTag prefix)
  tdata <- parseMainData (preDataL prefix)
  return (constr tdata)

chooseMainTag :: Word8 -> Parser (HIDMainData -> HIDMainTag)
chooseMainTag n 
    | n == 8 = return Input
    | n == 9 = return Output
    | n == 11 = return Feature
    | otherwise = fail "Incorrect tag"

parseCollection :: Parser HIDMainTag
parseCollection = do 
  prefix <- anyWord8
  tdata <- if isCollection prefix
           then parseCollectionData (preDataL prefix)
           else fail "Not Collection"
  return $ Collection tdata

parseEndCollection :: Parser HIDMainTag
parseEndCollection = do
  prefix <- anyWord8
  if isEndCollection prefix
  then return EndCollection
  else fail "Not End Collection"
  
parseCollectionData :: Word8 -> Parser HIDCollectionData
parseCollectionData w = do
  tdata <- anyWord8
  let collData = toEnum . fromEnum $ tdata
  AB.take (fromEnum (w - 1))
  return collData

parseMainData :: Word8 -> Parser HIDMainData
parseMainData n = do 
  let a = fromEnum (if n <= 2 
                   then n
                   else 2)
  as <- AB.take a
  let bs = concatMap toListLE (unpack as) ++ (repeat False)
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
    where typeM = fromListBE ([False,False,False,False] ++ [True,True] ++ [False,False]) 

preTag  ::  Word8 -> Word8                       
preTag p = rotateR (p .&. tagM) 4
    where tagM = fromListLE (take 4 (repeat False) ++ take 4 (repeat True)) 

preDataL :: Word8 -> Word8           
preDataL p = p .&. dataLengthM
    where dataLengthM = fromListLE ([True,True] ++ (take 6 (repeat False)))

sig3 :: Word8 -> Word8 
sig3 w = rotate (w .&. m) 3
    where m = fromListBE (take 3 (repeat True) ++ take 5 (repeat False))

lsig5 :: Word8 -> Word8 
lsig5 w = w .&. m 
    where m = fromListBE (take 3 (repeat False) ++ take 5 (repeat True))

