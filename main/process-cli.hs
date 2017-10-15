import           Data.Semigroup ((<>))

import           Options.Applicative

import           Process

import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Hardcoded list of nodes.
  let remotes = [ ("localhost", "8001")
                , ("localhost", "8002")
                ]

  execParser (info (commandParser <**> helper) idm)  >>= \x -> case x of
    RunProcess h p s w seed -> do
      startProcess h p s w seed remotes

data Command
  = RunProcess Host Port SendFor WaitFor WithSeed

commandParser :: Parser Command
commandParser = subparser $
  command' "node" "Runs the node."
    (RunProcess <$> hostP <*> portP <*> sendForP <*> waitForP <*> withSeedP)

command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser =
  command label (info (parser <**> helper) (progDesc description))

sendForP :: Parser SendFor
sendForP = (fmap SendFor) $ option auto $
     long "send-for"
  <> metavar "SEND_FOR"
  <> help "How many seconds does the system sends messages for."

waitForP :: Parser WaitFor
waitForP = (fmap WaitFor) $ option auto $
     long "wait-for"
  <> metavar "WAIT_FOR"
  <> help "The length of the grace period in seconds."

withSeedP :: Parser WithSeed
withSeedP = (fmap WithSeed) $ option auto $
     long "with-seed"
  <> metavar "WITH_SEED"
  <> help "Seed value for the RNG"

portP :: Parser Port
portP = (fmap Port) $ strOption $
     long "port"
  <> metavar "PORT"
  <> help "Port to listen on"

hostP :: Parser Host
hostP = (fmap Host) $ strOption $
     long "host"
  <> metavar "HOST"
  <> help "Hostname"
