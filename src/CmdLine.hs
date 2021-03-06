
module CmdLine where

import qualified Data.Text as T
import Data.Char (toUpper)
import Data.Colour
import Data.Colour.Names
import Options.Applicative
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import System.FilePath

import Types

pCmdLine :: Parser CmdLine
pCmdLine =
    CmdLine
      <$> strOption
            ( long "output"
              <> short 'o'
              <> metavar "OUTPUT.png"
              <> value "chart.png"
              <> help "write output to OUTPUT.png. Default is chart.png."
            )
      <*> pChart
      <*> pParseOpts
      <*> (optional $ strOption
            ( long "title"
              <> short 't'
              <> metavar "TITLE"
              <> help "set chart title to TITLE"
            )
          )
      <*> option auto
            ( long "width"
              <> short 'w'
              <> value 800
              <> metavar "WIDTH"
              <> showDefault
              <> help "specify chart width, in pixels"
            )
      <*> option auto
            ( long "height"
              <> short 'h'
              <> value 600
              <> metavar "HEIGHT"
              <> showDefault
              <> help "specify chart height, in pixels"
            )
      <*> option colour
            ( long "background"
              <> short 'b'
              <> value white
              <> metavar "COLOR"
              <> help "specify background color name (see SVG 1.1 spec)"
            )
      <*> option colour
            ( long "foreground"
              <> short 'f'
              <> value black
              <> metavar "COLOR"
              <> help "specify foreround color name (see SVG 1.1 spec)"
            )
      <*> option bool
            ( long "legend"
              <> short 'L'
              <> value True
              <> metavar "ON|OFF"
              <> showDefault
              <> help "enable or disable the legend"
            )
      <*> (optional $ strArgument (
              metavar "INPUT.txt"
              <> help "Input data file. If not provided, standard input will be used."
            ))

colour :: ReadM (Colour Double)
colour = maybeReader readColourName

bool :: ReadM Bool
bool = maybeReader $ \str -> 
  case map toUpper str of
    "TRUE" -> Just True
    "ON" -> Just True
    "YES" -> Just True
    "Y" -> Just True
    "FALSE" -> Just False
    "OFF" -> Just False
    "NO" -> Just False
    "N" -> Just False
    _ -> Nothing

lineEnding :: ReadM LineEnding
lineEnding = eitherReader $ \str ->
  case map toUpper str of
    "LF" -> Right LF
    "UNIX" -> Right LF
    "CR" -> Right CR
    "MACOS" -> Right CR
    "CRLF" -> Right CRLF
    "WINDOWS" -> Right CRLF
    "WIN" -> Right CRLF
    "DOS" -> Right CRLF
    _ -> Left "Unknown line ending specified; supported are: LF, UNIX; CR, MACOS, CRLF, WINDOWS, WIN, DOS."
          
pChart :: Parser ChartConfig
pChart =
    hsubparser (
           command "line" (info (pure Line) (progDesc "Make a line chart"))
        <> command "area" (info (pure Area) (progDesc "Make an area chart"))
        <> command "points" (info (pure Points) (progDesc "Make a points chart"))
        <> command "bar" (info pBar (progDesc "Make a bar chart"))
      )
  <|> pure Line

pBar :: Parser ChartConfig
pBar =
  Bar
    <$> (    flag' BarsStacked
                (long "stack"
                  <> help "stack bars of different columns on top of each other (by default)")
         <|> flag' BarsClustered
                (long "cluster"
                  <> help "place bars of different columns near each other")
         <|> pure BarsStacked
        )

pParseOpts :: Parser ParseOptions
pParseOpts =
  ParseOptions
    <$> switch
          ( long "header"
            <> short '1'
            <> help "first line contains column headers"
          )
    <*> option lineEnding
          ( long "line-ending"
            <> showDefault
            <> value LF
            <> metavar "UNIX|DOS|MACOS"
            <> help "specify line endings variant to be used"
          )
    <*> (T.pack <$> strOption
          ( long "delimiter"
            <> short 'd'
            <> metavar "CHAR"
            <> value "\t"
            <> help "specify fields delimiter ('\\t' by default)"
          )
        )
    <*> switch
          ( long "index"
            <> short 'i'
            <> showDefault
            <> help "if enabled, treat input data as if there was an additional first column, containing line numbers, starting from 1"
          )

parseCmdLine :: IO CmdLine
parseCmdLine = execParser opts
  where
    opts = info (pCmdLine <**> helper)
             ( fullDesc
               <> progDesc "Make a chart"
               <> header "chart - plot charts from input data"
             )

detectFormat :: FilePath -> FileFormat
detectFormat path =
  case map toUpper (takeExtension path) of
    ".PNG" -> PNG
    ".SVG" -> SVG
    ".PS" -> PS
    ".PDF" -> PDF
    ext -> error $ "unsupported output file format: " ++ ext

