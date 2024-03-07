module Main where
import Brick
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import Brick.Widgets.Center

ui :: Widget()
ui = str "Hello, World"

main :: IO ()
main = simpleMain ( joinBorders $ withBorderStyle unicode $ borderWithLabel (str "Hello!")  (center (str "Left") <+> vBorder <+> center (str "Right")) :: Widget())
