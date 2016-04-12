import Control.Lens

main :: IO ()
main = print $ ("hello","world")^._2
