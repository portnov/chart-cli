{-# LANGUAGE RecordWildCards #-}

module Chart where

import Control.Lens
import qualified Data.Text as T
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSL
import Data.Word
import Data.Hashable
import Data.List
import Data.Default.Class
import Graphics.Rendering.Chart

import Types

nameColor :: T.Text -> Colour Double
nameColor name =
  let h = fromIntegral (hash name `mod` 255) :: Word8
      hue = fromIntegral (hash name `mod` 360)
      r = (fromIntegral h / 255)
      v = (1 - r) * 0.5 + r*0.9

      hslColor = hsl hue 0.5 v
  in  sRGB (channelRed hslColor) (channelGreen hslColor) (channelBlue hslColor)

toPairs :: [[Value]] -> [[(Value, Double)]]
toPairs = transpose . zipWith go [1..]
  where
    go i [y] = [(Number (fromIntegral i), toDouble y)]
    go _ [x, y] = [(x, toDouble y)]
    go _ (x : ys) = [(x, toDouble y) | y <- ys]

toSeries :: [[Value]] -> [(Value, [Double])]
toSeries = zipWith go [1..]
  where
    go i ys = (Index (PlotIndex i), map toDouble ys)

toAreas :: [[Value]] -> [[(Value, (Double, Double))]]
toAreas = transpose . go 1
  where
    go :: Int -> [[Value]] -> [[(Value, (Double, Double))]]
    go _ [] = []
    go i ([y] : rest) = [(Number (fromIntegral i), (0, toDouble y))] : go (i+1) rest
    go i ((x : ys) : rest) =
        let runningSums = tail $ scanl (+) 0 $ map toDouble ys
            pairs = zip (0 : runningSums) runningSums
        in  [(x, pair) | pair <- pairs] : go (i+1) rest

mkLineStyle :: T.Text -> LineStyle
mkLineStyle name =
  line_color .~ opaque (darken 0.2 $ nameColor name) $ def

mkFillStyle :: T.Text -> FillStyle
mkFillStyle name =
  fill_color .~ opaque (nameColor name) $ def

mkPointStyle :: T.Text -> PointStyle
mkPointStyle name =
    point_color .~ opaque (nameColor name)
  $ point_border_color .~ opaque (darken 0.2 $ nameColor name)
  $ point_border_width .~ 1
  $ point_radius .~ 4
  $ def

convertChart :: ChartConfig -> ChartData -> AnyChart
convertChart Line cht =
  LineChart [
        plot_lines_title .~ T.unpack title
      $ plot_lines_style .~ mkLineStyle title
      $ plot_lines_values .~ [pairs]
      $ def
    | (pairs, NumberColumn title) <- zip (toPairs (chtValues cht)) (tail $ chtColumns cht)
  ]
convertChart (Bar {..}) cht =
  BarChart $
      plot_bars_titles .~ [T.unpack title | NumberColumn title <- chtColumns cht]
    $ plot_bars_item_styles .~ [(mkFillStyle title, Just (mkLineStyle title)) | NumberColumn title <- chtColumns cht]
    $ plot_bars_style .~ barStyle
    $ plot_bars_values .~ toSeries (chtValues cht)
    $ def
convertChart Area cht =
  AreaChart [
        plot_fillbetween_title .~ T.unpack title
      $ plot_fillbetween_style .~ mkFillStyle title
      $ plot_fillbetween_values .~ area
      $ def
    | (area, NumberColumn title) <- zip (toAreas (chtValues cht)) (tail $ chtColumns cht)
  ]
convertChart Points cht =
  PointsChart [
        plot_points_title .~ T.unpack title
      $ plot_points_style .~ mkPointStyle title
      $ plot_points_values .~ pairs
      $ def
    | (pairs, NumberColumn title) <- zip (toPairs (chtValues cht)) (tail $ chtColumns cht)
  ]

chartToPlots :: AnyChart -> [Plot Value Double]
chartToPlots (LineChart plots) = map toPlot plots
chartToPlots (BarChart plot) = [plotBars plot]
chartToPlots (AreaChart plots) = map toPlot plots
chartToPlots (PointsChart plots) = map toPlot plots

makeChart :: ChartConfig -> ChartData -> Layout Value Double
makeChart chtype cht =
  let title = chtTitle cht
      foreground = chtForeground cht
      background = chtBackground cht
      setFontStyle = font_color .~ opaque foreground
      setLineColor = line_color .~ opaque foreground
      gridStyle = dashedLine 1 [5, 5] $ dissolve 0.5 $ opaque foreground
      setAxisColor =
          laxis_style %~ (axis_label_style %~ setFontStyle) .
                         (axis_line_style %~ setLineColor) .
                         (axis_grid_style .~ gridStyle)
      
      setLegendFont = legend_label_style %~ setFontStyle
      setBackground = fill_color .~ opaque background

      yAxis = setAxisColor def
      xAxis = setAxisColor def

      legend = if chtLegend cht
                 then Just $ setLegendFont $ legend_orientation .~ LORows 4 $ def
                 else Nothing

  in  layout_title .~ T.unpack title
        $ layout_background %~ setBackground
        $ layout_title_style %~ setFontStyle
        $ layout_x_axis .~ xAxis
        $ layout_y_axis .~ yAxis
        $ layout_legend .~ legend
        $ layout_plots .~ chartToPlots (convertChart chtype cht)
        $ def

