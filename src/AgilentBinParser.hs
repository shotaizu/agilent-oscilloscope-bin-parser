module AgilentBinParser
    ( WFDHeader
    , WFType
    , WFUnit
    , WFFNumWaveform
    , parseAgilentBinFormat
    , getWFFHeader
    , getWFData
    , convWFDataType
    , convWFDataTypeToFloat
    , toFloatFromW8
    , convOSCGraph
    , getOSCGraph
    ) where

import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import GHC.Float
import qualified WaveformAnalysis as WFA


data WFType = WTUnknown | WTNormal | WTOthers deriving (Show)
toWFType 0 = WTUnknown
toWFType 1 = WTNormal
toWFType _ = WTOthers

data WFUnit = WUUnknown | WUVolts | WUSeconds | WUOthers deriving (Show)
toWFUnit 0 = WUUnknown
toWFUnit 1 = WUVolts
toWFUnit 2 = WUSeconds
toWFUnit _ = WUOthers


type WFFNumWaveform = Int
type WFFSize = Int
type WFFCookie = [Word8]
type WFFVersion = [Word8]
data WFFHeader = WFFHeader WFFCookie WFFVersion WFFSize WFFNumWaveform deriving (Show)

type WFHSize = Int
type WFNumWFBuffer = Int
type WFNumPoint = Int
type WFNumCount = Int
type WFXOrigin = Double
type WFXIncrement = Double
type WFXUnit = WFUnit
type WFYUnit = WFUnit

data WFHeader = WFHeader WFHSize WFType WFNumWFBuffer WFNumPoint WFNumCount WFXOrigin WFXIncrement WFXUnit WFYUnit deriving (Show)
getWFXOrigin (WFHeader _ _ _ _ _ o _ _ _ ) = o
getWFXIncrement (WFHeader _ _ _ _ _ _ i _ _ ) = i

data WForm = WForm WFHeader [WFData] deriving (Show)

type WFDHSize = Int
data WFDBufferType = WFDBTUnknown | WFDBT32Float | WFDBTMaxFloat | WFDBTMinFloat | WFDBTOthers deriving (Show)
type WFDBytesPerPoint = Int
type WFDBufferSize = Int
toWFDBType :: Int -> WFDBufferType
toWFDBType 0 = WFDBTUnknown
toWFDBType 1 = WFDBT32Float
toWFDBType 2 = WFDBTMaxFloat
toWFDBType 3 = WFDBTMinFloat
toWFDBType _ = WFDBTOthers

data WFDHeader = WFDHeader WFDHSize WFDBufferType WFDBytesPerPoint WFDBufferSize deriving (Show)

data WFData = WFData WFDHeader [Word8] 
instance Show WFData where
    show ( WFData h d ) = (show h) ++ " len = " ++ ((show . length) d) ++ " " ++ ((show . take 10)d)

data WFDataConv a = WFDataConv WFDHeader [a] deriving (Show)


data AgilentBinFormat = AgilentBinFormat WFFHeader WForm deriving (Show)

getWFFHeader :: AgilentBinFormat -> WFFHeader
getWFFHeader (AgilentBinFormat header _) = header


fromChar :: [Word8] -> Int
fromChar = foldr (\c a -> a * 0x100 + (fromEnum c)) 0

parseWFFHeader :: [Word8] -> WFFHeader
parseWFFHeader c = WFFHeader cookie ver num numWF
    where
        cookie = take 2 c
        ver = (drop 2 . take 4) c
        num = fromChar $ (drop 4 . take 8) c
        numWF = fromChar $ drop 8 c

parseWFHeader :: [Word8] -> WFHeader
parseWFHeader c = WFHeader hs fwt fwbn fwpn fwcn xo xi wfutx wfuty
    where 
        hs = fromChar $ take 4 c
        fwt = (toWFType . fromChar . take 4 . drop 4) c
        fwbn = (fromChar . take 4 . drop 8) c
        fwpn = (fromChar . take 4 . drop 12) c
        fwcn = (fromChar . take 4 . drop 16) c
        xi = (toDoubleFromW8 . take 8 . drop 32) c
        xo = (toDoubleFromW8 . take 8 . drop 40) c
        wfutx = (toWFUnit . fromChar . take 4 . drop 48) c
        wfuty = (toWFUnit . fromChar . take 4 . drop 52) c

parseWFDHeader :: [Word8] -> WFDHeader
parseWFDHeader c = WFDHeader nhs dbt dbpp dbs
    where
        nhs = (fromChar . take 4) c
        dbt = (toWFDBType . fromChar . take 2 . drop 4) c
        dbpp = (fromChar . take 2 . drop 4) c
        dbs = (fromChar . take 4 . drop 4) c

parseWFData :: [Word8] -> WFData
parseWFData c = WFData header d
    where
        h = take 12 c
        dbs = drop 12 c
        header = parseWFDHeader h
        d = dbs

parseWForm :: [Word8] -> WForm
parseWForm c = WForm wfh [wfd]
    where
        wfh = (parseWFHeader . take 140) c
        wfd = (parseWFData . drop 140) c



toFloatFromW8 = castWord32ToFloat . toW32FromW8
    where
        toW32FromW8 :: [Word8] -> Word32
        toW32FromW8 = foldr accum 0
        accum o a = (a `shiftL` 8) .|. fromIntegral o

toDoubleFromW8 = castWord64ToDouble . toW64FromW8
    where
        toW64FromW8 :: [Word8] -> Word64
        toW64FromW8 = foldr accum 0
        accum o a = (a `shiftL` 8) .|. fromIntegral o

convWFDataType :: ([Word8] -> a) -> WFData -> WFDataConv a
convWFDataType f (WFData hraw draw) = WFDataConv hraw d
    where
        d = map f $ split4 draw
        split4 l  = if ( length l ) < 4 
                       then []
                       else ((take 4 l):(split4 (drop 4 l)))

convWFDataTypeToFloat = convWFDataType toFloatFromW8
convWFDataTypeToDouble = convWFDataType (realToFrac . toFloatFromW8)





parseAgilentBinFormat :: BS.ByteString -> AgilentBinFormat
parseAgilentBinFormat bs = AgilentBinFormat fheader dlist
    where
        fheader = parseWFFHeader $ (BS.unpack . (BS.take 12)) bs
        dlist = parseWForm $ ((BS.unpack . BS.drop 12)) bs

getWFData :: AgilentBinFormat -> WFData
getWFData a = head ds
    where
        AgilentBinFormat _ dl = a
        WForm _ ds = dl

convOSCGraph :: (Num a) => a -> a -> WFDataConv a -> [(a,a)]
convOSCGraph orig inc (WFDataConv _ dr) = zip [orig + inc * (fromIntegral i) | i <- [0..]] dr

getOSCGraph :: AgilentBinFormat -> [(Double,Double)]
getOSCGraph (AgilentBinFormat _ wf) = convOSCGraph xo xi dc
    where
        WForm wfh (d:_) = wf
        xo = realToFrac $ getWFXOrigin wfh
        xi = realToFrac $ getWFXIncrement wfh
        dc = convWFDataTypeToDouble d
        -- dc = convWFDataTypeToFloat d


