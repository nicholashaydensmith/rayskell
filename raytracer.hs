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
type FrameBuffer = PixelData (Color4 GLubyte)
data Obj = Geometry Id [OBJVertex] [OBJFace]
      deriving (Show)

--main :: IO ()
--main = do
--      (_progName, _args) <- getArgsAndInitialize
--      _window <- createWindow "Rayskell"
--      clearColor $= Color4 0 0 0 0
--      rowAlignment Unpack $= 1
--      fb <- render (replicate (64 * 64) (Color4 0 0 255 0))
--      displayCallback $= display fb
--      mainLoop
main :: IO ()
main = getArgsAndInitialize >>= \_ -> createWindow "Rayskell" >>= \_ -> render (replicate (64 * 64) (Color4 255 0 0 255)) >>= \fb -> displayCallback $= display fb >>= \_ -> mainLoop

display :: FrameBuffer -> DisplayCallback
display fb = do
      clear [ ColorBuffer ]
      drawPixels (Size 64 64) fb
      flush

render :: [Color4 GLubyte] -> IO FrameBuffer
render fb = fmap (PixelData RGBA UnsignedByte) (newArray fb)

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
