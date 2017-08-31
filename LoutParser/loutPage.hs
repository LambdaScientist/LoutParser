import Text.PrettyPrint.Boxes
import qualified GalleyCode as G


longestString :: [[String]] -> Int
longestString = maximum . (map length) . concat 


mkBoxFromList :: [String] -> Box
mkBoxFromList = vcat right . map text 

mkPgBox :: [[String]] -> [Box]
mkPgBox = map mkBoxFromList 


foo = mkPgBox G.bodycs

bar = printBox (foo !! 1)

renderComp compList = render $ vcat left docCompList
  where  
    docCompList = map (encloseBox.prettyComponent) compList


prettyComponent :: G.Component -> Box
prettyComponent cmp = vcat top $ map text cmp

encloseBox = addColBar.addRowBar
addRowBar input = hcat top [row,input,row]
  where
    ri = rows input
    row = mkRow ri
addColBar input = vcat left [col,input,col] 
  where
    ci = cols input
    col = mkCol ci


mkCol = hcat top . map text . flip replicate "-"
mkRow = vcat top . map text . flip replicate "|"

main = do
  -- putStrLn ""
  -- let res = G.force G.nexample
  -- putStrLn "nexample"
  -- putStrLn $ renderComp res
  -- putStrLn "example"
  -- putStrLn $ renderComp $ G.force G.example
  -- putStrLn "h2a"
  -- putStrLn $ renderComp $ G.force G.h2a
  -- putStrLn "h3a"
  -- putStrLn $ renderComp $ G.force G.h3a
  -- putStrLn "bp"
  -- putStrLn $ renderComp $ G.force G.bp
  -- putStrLn "bh"
  -- putStrLn $ renderComp $ G.force G.bh
  -- putStrLn "vfill"
  -- putStrLn $ renderComp $ G.force G.vfill
  -- putStrLn "f2"
  -- putStrLn $ renderComp $ G.force G.f2
  -- putStrLn "f2a"
  -- putStrLn $ renderComp $ G.force G.f2a
  -- putStrLn "fn2"
  -- putStrLn $ renderComp $ G.force G.fn2
  -- putStrLn "fn2'"
  -- putStrLn $ renderComp $ G.force G.fn2'
  -- putStrLn "fns"
  -- putStrLn $ renderComp $ G.force G.fns
  -- putStrLn "fns'"
  -- putStrLn $ renderComp $ G.force G.fns'
  -- putStrLn "ps"
  -- putStrLn $ renderComp $ G.force G.ps
  -- putStrLn "ps2"
  -- putStrLn $ renderComp $ G.force G.ps2
  -- putStrLn "bp'"
  -- putStrLn $ renderComp $ G.force G.bp'
  -- putStrLn "npageList"
  putStrLn "Double Bound Example 23 23:"
  putStrLn $ renderComp $ G.force (G.doubleBoundEx'' 23 23)
  putStrLn "Double Bound Example  23 15:"
  putStrLn $ renderComp $ G.force (G.doubleBoundEx'' 23 15)
  putStrLn "Double Bound Example  15 18:"
  putStrLn $ renderComp $ G.force (G.doubleBoundEx'' 15 18)
  putStrLn "Double Bound Example  18 23:"
  putStrLn $ renderComp $ G.force (G.doubleBoundEx'' 18 23)
  putStrLn "Double Bound Example  20 15:"
  putStrLn $ renderComp $ G.force (G.doubleBoundEx'' 20 15)
