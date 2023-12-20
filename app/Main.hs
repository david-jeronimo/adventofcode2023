import Aoc20
import Lib (Part(..), Text)
import qualified Data.Text.IO as TIO
import Control.Applicative (liftA2)
import System.TimeIt (timeItT)
import Text.Printf
import Control.Monad.IO.Class (MonadIO(liftIO))

main :: IO ()
main = timeItNamed' "Total" $ runDay parseInput solution "20"

runDay::(Show a, Show b) => (Text -> a) -> (Part -> a -> b)  -> String -> IO()
runDay parse sol dayStr = sequence_ $ liftA2 (run parse sol) (fileName <$> ["sample","input"]) [PartOne,PartTwo]
  where fileName suffix = "files/aoc" <> dayStr <> "." <> suffix <> ".txt"

readInput:: (Show a) => (Text -> a) -> String -> IO a
readInput parse file = do
  l <- TIO.readFile file
  let input = parse l
  print input
  return input

run::(Show a, Show b) => (Text -> a) -> (Part -> a -> b) -> FilePath -> Part -> IO()
run parse sol file part = timeItNamed' "CPU time" $ do
    putStrLn $ "\n" <> show part <> "  " <> file
    input <- readInput parse file
    print $ sol part input

timeItNamed' :: MonadIO m => String -> m a -> m a
timeItNamed' name ioa = do
    (t, a) <- timeItT ioa
    liftIO $ printf (name ++ ": %6.3fs\n") t
    return a

-- 3274