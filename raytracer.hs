import Data.List
import Foreign ( newArray )
import System.Environment
import Debug.Trace
import Graphics.UI.GLUT

t3 :: [a] -> (a, a, a)
t3 [a, b, c] = (a, b, c)

type OBJVertex = (GLfloat, GLfloat, GLfloat)
type OBJFace = (OBJVertex, OBJVertex, OBJVertex)
type Id = String
type FrameBuffer = PixelData (Color4 GLubyte)
data Obj = Geometry Id [OBJVertex] [OBJFace]
      deriving (Show)

data Vector a = Vec3 a a a
      deriving (Show)

data Ray = Ray (Color4 GLubyte) (Vector GLfloat) (Vector GLfloat)
      deriving (Show)

main :: IO ()
main = do
      (_progName, _args) <- getArgsAndInitialize
      _window <- createWindow "Rayskell"
      --clearColor $= Color4 0 0 0 0
      --rowAlignment Unpack $= 1
      objs <- mapM readFile _args
      fb <- render (replicate (64 * 64) (Color4 0 0 255 255)) (map (\f -> parseObj (Geometry "" [] []) (lines f)) objs)
      displayCallback $= display fb
      mainLoop

display :: FrameBuffer -> DisplayCallback
display fb = clear [ ColorBuffer ] >>= \_-> drawPixels (Size 64 64) fb >>= \_-> flush

render :: [Color4 GLubyte] -> [Obj] -> IO FrameBuffer
render fb gs = fmap (PixelData RGBA UnsignedByte) (newArray (map (\(p, i) -> renderPixel (Ray p (Vec3 (fromIntegral (i `mod` 64 :: Int) :: GLfloat) (fromIntegral (i `quot` 64 :: Int) :: GLfloat) (-1.0)) (Vec3 0.0 0.0 1.0)) fs) (zip fb [0..])))
                  where fs = concatMap (\(Geometry _ _ fs) -> fs) gs

renderPixel :: Ray -> [OBJFace] -> Color4 GLubyte
renderPixel r@(Ray c _ _) fs = foldl (\c0 c1 -> c0 + c1) c $ map (\f -> accumulate r f) fs

accumulate :: Ray -> OBJFace -> Color4 GLubyte
accumulate r@(Ray c _ _) f = if r `intersects` f
                                    then c + (Color4 255 0 0 0)
                                    else c

intersects :: Ray -> OBJFace -> Bool
intersects (Ray _ orig dir) ((x0, y0, z0), (x1, y1, z1), (x2, y2, z2)) = if a > (-0.00001) && a < 0.00001
                                                                             then False
                                                                             else if u < 0.0 || u > 1.0
                                                                                  then False
                                                                                  else if v < 0.0 || (u + v) > 1.0
                                                                                       then False
                                                                                       else if t > 0.00001
                                                                                            then True
                                                                                            else False
                                                                              where
                                                                                    v0 = Vec3 x0 y0 z0
                                                                                    v1 = Vec3 x1 y1 z1
                                                                                    v2 = Vec3 x2 y2 z2
                                                                                    e1 = v1 - v0
                                                                                    e2 = v2 - v0
                                                                                    h  = dir `cross` e2
                                                                                    a  = e1 `dot` h
                                                                                    f  = 1 / a
                                                                                    s  = orig - v0
                                                                                    u  = f * (s `dot` h)
                                                                                    q  = s `cross` e1
                                                                                    v  = f * (dir `dot` q)
                                                                                    t  = f * (e2 `dot` q)


parseObj :: Obj -> [String] -> Obj
parseObj g []                         = g
parseObj g@(Geometry i vs fs) ("":ss) = parseObj g ss
parseObj g@(Geometry i vs fs) (s:ss)  = case t of 
                                          "g" -> parseObj (Geometry (head vals) vs fs) ss
                                          "v" -> parseObj (Geometry i (vs ++ [(t3 (map read vals::[GLfloat]))]) fs) ss
                                          "f" -> parseObj (Geometry i vs (fs ++ [(t3 (map (vs !!) (map (\i -> (read i::Int) - 1) vals)::[OBJVertex]))])) ss
                                          _   -> parseObj g ss
                                          where line = words s
                                                t    = head line
                                                vals = tail line

cross :: Vector GLfloat -> Vector GLfloat -> Vector GLfloat
cross (Vec3 x y z) (Vec3 u v w) = (Vec3 (y * w) (z * u) (x * v)) - (Vec3 (z * v) (x * w) (y * u))

dot :: Vector GLfloat -> Vector GLfloat -> GLfloat
dot (Vec3 x y z) (Vec3 u v w) = (x * u) + (y * v) + (z * w)

instance (Num a) => Num (Color4 a) where
      (Color4 a b c d) + (Color4 e f g h) = Color4 (a + e) (b + f) (c + g) (d + h)
      (Color4 a b c d) - (Color4 e f g h) = Color4 (a - e) (b - f) (c - g) (d - h)
      (Color4 a b c d) * (Color4 e f g h) = Color4 (a * e) (b * f) (c * g) (d * h)

instance (Num a) => Num (Vector a) where
      (Vec3 a b c) + (Vec3 d e f) = Vec3 (a + d) (b + e) (c + d)
      (Vec3 a b c) - (Vec3 d e f) = Vec3 (a - d) (b - e) (c - d)
      (Vec3 a b c) * (Vec3 d e f) = Vec3 (a * d) (b * e) (c * d)
