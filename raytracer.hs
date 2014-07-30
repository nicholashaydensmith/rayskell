import Data.List
import System.Environment
import Debug.Trace
import qualified Data.Vector.Storable as V
import Codec.Picture

t3 :: [a] -> (a, a, a)
t3 [a, b, c] = (a, b, c)

type Vertex = (Float, Float, Float)
type Face = (Vertex, Vertex, Vertex)
type Id = String
type FrameBuffer = Image PixelRGBA8
type ImgData = V.Vector (PixelBaseComponent PixelRGBA8)
data Obj = Geometry Id [Vertex] [Face]
      deriving (Show)

main :: IO ()
main = getArgs >>= mapM readFile >>= render img . geometry
      where
            img      = Image 500 500 (V.replicate (500 * 500) (PixelRGBA8 0 0 0 0) :: ImgData) --(V.fromList [] :: ImgData)
            geometry = map (\f -> parseObj (Geometry "" [] []) (lines f))

render :: FrameBuffer -> [Obj] -> IO ()
render fb []                     = writePng "./renders/test.png" fb
render fb ((Geometry _ _ fs):gs) = render (renderGeometry fb fs) gs

renderGeometry :: FrameBuffer -> [Face] -> FrameBuffer
renderGeometry fb fs = fb

parseObj :: Obj -> [String] -> Obj
parseObj g []                         = g
parseObj g@(Geometry i vs fs) ("":ss) = parseObj g ss
parseObj g@(Geometry i vs fs) (s:ss)  = case t of 
                                          "g" -> parseObj (Geometry (head vals) vs fs) ss
                                          "v" -> parseObj (Geometry i (vs ++ [(t3 (map read vals::[Float]))]) fs) ss
                                          "f" -> parseObj (Geometry i vs (fs ++ [(t3 (map (vs !!) (map (\i -> (read i::Int) - 1) vals)::[Vertex]))])) ss
                                          _   -> parseObj g ss
                                          where line = words s
                                                t    = head line
                                                vals = tail line
