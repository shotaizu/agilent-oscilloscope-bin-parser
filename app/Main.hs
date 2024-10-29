module Main (main) where

import AgilentBinParser as ABP
import WaveformAnalysis as WFA
import qualified Data.ByteString as BS
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.IO

data Options = Options { optNumPoint :: Int
                       }

defaultOptions = Options { optNumPoint = -1
                         }
options = 
    [ Option "n" ["npoint"] (ReqArg (\arg opt -> return opt { optNumPoint = read arg }) "NUM") "Number of waveform point to be output"
    , Option "h" ["help"] (NoArg (\_ -> do
        prg <- getProgName
        hPutStrLn stderr (usageInfo prg options)
        exitWith ExitSuccess))
        "Show this help"
    ]


main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optNumPoint = np } = opts
    bs <- BS.readFile $ head nonOptions
    -- "./test/bmr_0v85d_2.bin"
    let a = ABP.parseAgilentBinFormat bs
    -- putStrLn $ show a
    putStrLn $ ((WFA.showCSVFormat ' ') . WFA.fromPairs . take np . ABP.getOSCGraph) a
