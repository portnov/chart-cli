{-# LANGUAGE ExistentialQuantification #-}
module Types where

import qualified Data.Text as T
import Data.Dates
import Data.Time
import Data.Time.Calendar
import Data.Time.LocalTime
import Graphics.Rendering.Chart

data ParseOptions = ParseOptions {
      poHeader :: Bool
    , poSeparator :: T.Text
  }

data Value =
    Number { toDouble :: Double  }
  | Index { toIndex :: PlotIndex }
  | Date { toTime :: LocalTime }
  deriving (Eq, Show, Ord)

data Column =
    NumberColumn T.Text
  | DateColumn T.Text
  deriving (Eq, Show)

data ChartData = ChartData {
      chtTitle :: T.Text
    , chtColumns :: [Column]
    , chtValues :: [[Value]]
  }
  deriving (Eq, Show)

data ChartType =
    Line
  | Bar
  | Area
  | Histogram
  | Points
  deriving (Eq, Show)

data AnyChart =
    LineChart [PlotLines Value Double]
  | BarChart (PlotBars Value Double)
  | AreaChart [PlotFillBetween Value Double]
  | HistogramChart (PlotHist Value Double)
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
      (Number x : _) -> mapAxisData Number toDouble $ autoAxis $ map toDouble list
      (Date dt : _) -> mapAxisData Date toTime $ autoAxis $ map toTime list
      (Index i : _) -> mapAxisData Index toIndex $ autoAxis $ map toIndex list

