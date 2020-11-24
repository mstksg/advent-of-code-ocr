{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

import           Advent.OCR
import           Options.Applicative
import           System.Exit
import           System.IO
import qualified Data.Set            as S

data Opts = Opts
    { oChars :: S.Set Char
    }
  deriving Show

parseOpts :: Parser Opts
parseOpts = Opts <$>
    option (S.fromList <$> str)
      ( long "chars"
     <> short 'c'
     <> help "characters to treat as \"on\"/included"
     <> value (S.singleton '#')
     <> showDefaultWith S.toList
     <> metavar "CHARS"
      )

main :: IO ()
main = do
    Opts{..} <- execParser $
      info (parseOpts <**> helper)
        ( fullDesc
       <> progDesc "Parse an ASCII image into its letters."
       <> header "advent-of-code-orc - Advent of Code ASCII parser"
        )
    inp <- getContents
    case asciiMapToLetters oChars defaultLetterMap inp of
      Nothing  -> do
        hPutStrLn stderr "No valid parse"
        exitFailure
      Just res -> putStrLn res
