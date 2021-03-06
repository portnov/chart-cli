{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Types where

import qualified Data.Text as T
import Data.Dates
import Data.Time
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Colour
import Graphics.Rendering.Chart

data LineEnding =
    LF
  | CR
  | CRLF
  deriving (Eq, Show)

data ParseOptions = ParseOptions {
      poHeader :: Bool
    , poLineEnding :: LineEnding
    , poSeparator :: T.Text
    , poIndex :: Bool
  }
  deriving (Eq, Show)

data Value =
    Number Double
  | Index PlotIndex
  | Date { toTime :: LocalTime }
  deriving (Eq, Show, Ord)

toDouble :: Value -> Double
toDouble (Number x) = x
toDouble (Index i) = toValue i
toDouble (Date dt) = error $ "can't convert date/time to double"

toIndex :: Value -> PlotIndex
toIndex (Number x) = PlotIndex $ round x
toIndex (Index i) = i
toINdex (Date dt) = error $ "can't convert date/time to index"

data Column =
    NumberColumn T.Text
  | DateColumn T.Text
  deriving (Eq, Show)

data ChartData = ChartData {
      chtTitle :: T.Text
    , chtBackground :: Colour Double
    , chtForeground :: Colour Double
    , chtLegend :: Bool
    , chtColumns :: [Column]
    , chtValues :: [[Value]]
  }
  deriving (Eq, Show)

deriving instance Eq PlotBarsStyle

data ChartConfig =
    Line
  | Bar { barStyle :: PlotBarsStyle }
  | Area
  | Points
  deriving (Eq, Show)

data AnyChart =
    LineChart [PlotLines Value Double]
  | BarChart (PlotBars Value Double)
  | AreaChart [PlotFillBetween Value Double]
  | PointsChart [PlotPoints Value Double]

toLocalTime :: DateTime -> LocalTime
toLocalTime dt =
  let localDay = fromGregorian (fromIntegral $ year dt) (month dt) (day dt)
      time = TimeOfDay (hour dt) (minute dt) (fromIntegral $ second dt)
  in  LocalTime localDay time

mapAxisData :: (x -> y) -> (y -> x) -> AxisData x -> AxisData y
mapAxisData to from axis =
  axis {
      _axis_viewport = \range x -> _axis_viewport axis range (from x)
    , _axis_tropweiv = \range d -> to (_axis_tropweiv axis range d)
    , _axis_ticks = [(to x, d) | (x, d) <- _axis_ticks axis]
    , _axis_labels = [[(to x, s) | (x, s) <- pairs] | pairs <- _axis_labels axis]
    , _axis_grid = map to (_axis_grid axis)
  }

instance PlotValue Value where
  toValue (Number x) = x
  toValue (Date dt) = toValue dt
  toValue (Index i) = toValue i

  fromValue x = Number x
  
  autoAxis = \list ->
    case list of
      [] -> mapAxisData Number toDouble $ autoAxis $ map toDouble list
      (Number x : _) -> mapAxisData Number toDouble $ autoAxis $ map toDouble list
      (Date dt : _) -> mapAxisData Date toTime $ autoAxis $ map toTime list
      (Index i : _) -> mapAxisData Index toIndex $ autoAxis $ map toIndex list

data CmdLine = CmdLine {
      cmdOutput :: FilePath
    , cmdChart :: ChartConfig
    , cmdParse :: ParseOptions
    , cmdTitle :: Maybe String
    , cmdWidth :: Int
    , cmdHeight :: Int
    , cmdBackground :: Colour Double
    , cmdForeground :: Colour Double
    , cmdLegend :: Bool
    , cmdInput :: Maybe FilePath
  }
  deriving (Show)

