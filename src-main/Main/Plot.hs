module Main.Plot where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

plotScore :: FilePath -> Int -> [Double] -> IO ()
plotScore f r ls = toFile def f $ do
    --layout_title .= "Amplitude Modulation"
    plot $ line "Score" [zip [r, 2 * r..] ls]
