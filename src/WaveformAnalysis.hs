module WaveformAnalysis
    ( WFPoint
    , Waveform
    , showCSVFormat
    , fromPairs
    ) where

type WFPoint a = (a,a)
data Waveform a = Waveform [(WFPoint a)] deriving (Show)

concat :: Waveform a -> Waveform a -> Waveform a
concat (Waveform f) (Waveform s) = Waveform (f ++ s)

cons :: WFPoint a -> Waveform a -> Waveform a
cons p (Waveform l) = Waveform (p:l)



showCSVFormat :: (Show a) => Char -> Waveform a -> String
showCSVFormat _ (Waveform []) = "\n"
showCSVFormat c (Waveform ((x,y):wf)) = (show x) ++ [c] ++ (show y) ++ "\n" ++ showCSVFormat c (Waveform wf)

fromPairs :: [(a,a)] -> Waveform a
fromPairs [] = Waveform []
fromPairs (p:left) = cons p $ fromPairs left

