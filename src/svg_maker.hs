{-# LANGUAGE OverloadedStrings #-}
import Text.Printf
import Text.Blaze
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

main :: IO ()
main = do
  let a = renderSvg svgDoc
  putStrLn a

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "300" ! A.height "300" ! A.viewbox "0 0 30 30" $ do
  S.g ! A.transform makeTransform $ do
  makeCDS 1 5 0 True
  makeCDS 1 5 10 False
  connectCDS 6 2 10 2 False

makePath :: Int -> Int -> Int -> Int -> S.AttributeValue
makePath x1 y1 x2 y2 = mkPath $ do
  m x1 y1
  l x2 y2

connectCDS :: Int -> Int -> Int -> Int -> Bool -> S.Svg
connectCDS x1 y1 x2 y2 direct = S.path ! (A.d $ makePath x1 y1 x2 y2) ! A.stroke "#000000" ! A.strokeWidth "0.2" ! A.strokeDasharray (if direct then "0,0" else "0.5,0.5")


makeTransform :: S.AttributeValue
makeTransform = rotate 0

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
