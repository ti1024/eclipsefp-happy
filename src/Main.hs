import Calc (runCalc)

main :: IO ()
main = do
    cs <- getContents
    print $ runCalc cs
