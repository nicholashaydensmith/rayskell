import Data.List
import Data.Bits ( (.&.) )
import Data.IORef ( IORef, newIORef )
import Foreign ( newArray )
import System.Environment
import Debug.Trace
import Graphics.UI.GLUT

t3 :: [a] -> (a, a, a)
t3 [a, b, c] = (a, b, c)

type OBJVertex = (GLfloat, GLfloat, GLfloat)
type OBJFace = (OBJVertex, OBJVertex, OBJVertex)
type Id = String
type FrameBuffer = PixelData (Color3 GLubyte)
data Obj = Geometry Id [OBJVertex] [OBJFace]
      deriving (Show)

main :: IO ()
main = do
      (_progName, _args) <- getArgsAndInitialize
      _window <- createWindow "Rayskell"
      img <- myInit
      displayCallback $= display img
      mainLoop

display :: FrameBuffer -> DisplayCallback
display fb = do
      clear [ ColorBuffer ]
      drawPixels checkImageSize fb
      flush

checkImageSize :: Size
checkImageSize = Size 64 64

makeCheckImage :: Size -> GLsizei -> (GLubyte -> (Color3 GLubyte)) -> IO FrameBuffer
makeCheckImage (Size w h) n f =
                              fmap (PixelData RGB UnsignedByte) $
                                    newArray [ f c |
                                          i <- [ 0 .. w - 1 ],
                                          j <- [ 0 .. h - 1 ],
                                          let c | (i .&. n) == (j .&. n) = 0
                                                | otherwise              = 255 ]

myInit :: IO FrameBuffer
myInit = do
      clearColor $= Color4 0 0 0 0
      rowAlignment Unpack $= 1
      makeCheckImage checkImageSize 0x8 (\c -> Color3 c c c)

render :: FrameBuffer -> [Obj] -> IO ()
render fb []                     = putStr $ show fb
render fb ((Geometry _ _ fs):gs) = render fb gs

--renderGeometry :: ImgData -> [OBJFace] -> ImgData
--renderGeometry v fs = map (\(PixelRGBA8 r g b a) -> PixelRGBA8 255 g b 255) v

--renderPixel :: PixelBaseComponent PixelRGBA8 -> PixelBaseComponent PixelRGBA8
--renderPixel a = a + 10

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
