module Main (main) where

import Options.Applicative

-- | Top-level CLI entry point (to be expanded in future issues)
main :: IO ()
main = do
  cmd <- execParser opts
  runCommand cmd
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "ZStradr — Haskell trading framework CLI"
     <> header "zstradr - a quantitative trading framework" )

-- ---------------------------------------------------------------------------
-- Commands

data Command
  = CmdVersion
  deriving (Show)

commandParser :: Parser Command
commandParser = subparser
  ( command "version"
      (info (pure CmdVersion) (progDesc "Print version information"))
  )

runCommand :: Command -> IO ()
runCommand CmdVersion = putStrLn "zstradr 0.1.0"
