import           Control.Monad (unless)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import           System.Exit (exitFailure)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
    -- TODO Add tests here
      ]

  unless (and results) $
    exitFailure
