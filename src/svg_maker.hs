{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
import System.Exit
import Text.Printf
import Text.Blaze
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

-- usage and stuff
usage = putStrLn "Usage: svg_maker <output.svg>"
exit = exitWith ExitSuccess

-- main functions
main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 1 then
      usage >> exit
  else
      let a = renderSvg svgDoc
          name = head args
      in withFile name WriteMode
             (\handle -> hPutStr handle a)

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "300" ! A.height "300" ! A.viewbox "0 0 30 30" $ do
           S.g ! A.transform makeTransform $ do
             makeCDSs lst
             connectCDSs clst

-- example contigs and lists
c1 = (1, 5, 0, True)
c2 = (1, 5, 10, False)
lst = [c1, c2]
clst = [(c1, c2, False)]

-- helper functions for tuple
first4 (x, _, _, _) = x
second4 (_, y, _, _) = y
third4 (_, _, z, _) = z
forth4 (_, _, _, a) = a

first3 (x, _, _) = x
second3 (_, y, _) = y
third3 (_, _, z) = z

-- example function to transform the whole image
makeTransform :: S.AttributeValue
makeTransform = rotate 0

-- functions to draw contigs
makeCDSs :: [(Int,Int,Int,Bool)] -> S.Svg
makeCDSs [x] = makeCDS (first4 x) (second4 x) (third4 x) (forth4 x)
makeCDSs (x:xs) = do makeCDS (first4 x) (second4 x) (third4 x) (forth4 x)
                     makeCDSs xs

makeCDS :: Int -> Int -> Int -> Bool -> S.Svg
makeCDS y l s sense = S.polygon !
                      A.fill "#008d46" !
                      A.stroke "#000000" !
                      A.strokeWidth "0.2" !
                      (A.points $ toValue $ if sense then formatRPolyP y l s else formatLPolyP y l s)

formatLPolyP :: Int -> Int -> Int -> String
formatLPolyP y l s = printf "%d,%d %d,%d %d,%d %d,%d %d,%d" s (y + 1) (s + 1) y (s + l + 1) y (s + l + 1) (y + 2) (s + 1) (y + 2)

formatRPolyP :: Int -> Int -> Int -> String
formatRPolyP y l s = printf "%d,%d %d,%d %d,%d %d,%d %d,%d" s y (s + l) y (s + l + 1) (y + 1) (s + l) (y + 2) s (y + 2)

-- functions to connect contigs
makePath :: Int -> Int -> Int -> Int -> S.AttributeValue
makePath x1 y1 x2 y2 = mkPath $ do
  m x1 y1
  l x2 y2

connectCDS :: Int -> Int -> Int -> Int -> Bool -> S.Svg
connectCDS x1 y1 x2 y2 direct = S.path ! (A.d $ makePath x1 y1 x2 y2) ! A.stroke "#000000" ! A.strokeWidth "0.2" ! A.strokeDasharray (if direct then "0,0" else "0.5,0.5")

connectCDSfromContigs :: ((Int, Int, Int, Bool), (Int, Int, Int, Bool), Bool) -> S.Svg
connectCDSfromContigs x = let
                             c1 = first3 x
                             c2 = second3 x
                             dir = third3 x
                          in connectCDS ((third4 c1) + 1 + (second4 c1)) ((first4 c1) + 1) (third4 c2) ((first4 c2) + 1) dir

connectCDSs :: [((Int, Int, Int, Bool), (Int, Int, Int, Bool), Bool)] -> S.Svg
connectCDSs [x] = connectCDSfromContigs x
connectCDSs (x:xs) = do
                       connectCDSfromContigs x
                       connectCDSs xs
