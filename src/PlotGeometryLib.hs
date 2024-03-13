{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE CPP #-}

module PlotGeometryLib where

-- import qualified Graphics.Rendering.FTGL as FTGL

-- import Graphics.GL.Types

-- Saturday, 07 October 2023 11:46 PDT
-- Complex number,
-- c = 1 :+ 2

-- Hidden modules
-- import Graphics.Rendering.OpenGL.GL.Capability
-- import Graphics.Rendering.OpenGL.GL.Exception
-- import Graphics.Rendering.OpenGL.GL.MatrixComponent
-- import Graphics.Rendering.OpenGL.GL.PeekPoke
-- import Graphics.Rendering.OpenGL.GL.QueryUtils
-- import Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit
-- import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
-- import Graphics.GL

-- BEG
-- KEY: ansi color, console color
-- import Rainbow
-- import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
-- https://hackage.haskell.org/package/ansi-terminal-0.10.3/docs/System-Console-ANSI.html
-- import System.IO (hFlush, stdout)
-- import qualified System.Console.ANSI as AN
-- END

-- import Graphics.UI.GLUT
-- import Graphics.UI.GLUT.Callbacks.Global
import AronGraphic
import AronModule
import AronAlias
import AronHtml2
import AronToken
import AronOpenGL
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Lens
    ( Field1(_1), Field2(_2), Field3(_3), Field4(_4), (<&>), (^.) )
-- import Control.Monad
import Control.Monad (unless, when, join)
import qualified Control.Monad.State as CMS
-- import AronDevLib

import Data.Array.IO
import qualified Data.Array.IO as DAO
import Data.Complex
import Data.IORef
    ( modifyIORef, writeIORef, readIORef, newIORef, IORef )
import Data.Int
import qualified Data.List as DL
import qualified Data.Map as DM
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.StateVar
-- import Data.Typeable
import Data.Typeable (typeOf)
import qualified Text.Read as DT
import qualified Data.Vector as VU
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Float.RealFracMethods
import Graphics.Rendering.OpenGL
    ( viewport,
      perspective,
      loadIdentity,
      matrixMode,
      preservingMatrix,
      renderPrimitive,
      ComparisonFunction(Lequal),
      ClearBuffer(DepthBuffer, ColorBuffer),
      Size(Size),
      Position(Position),
      MatrixMode(Modelview, Projection),
      Matrix(getMatrixComponents, newMatrix),
      MatrixOrder(RowMajor, ColumnMajor),
      GLmatrix,
      MatrixComponent(rotate, translate),
      Vector3(..),
      Vertex(vertex),
      Vertex4(Vertex4),
      Normal(normal),
      Normal3(..),
      Color(color),
      PrimitiveMode(Triangles, TriangleStrip, LineLoop, Quads, TriangleFan, Points),
      GLdouble,
      Color3(..),
      Vertex3(..),
      Capability(Enabled),
      GLfloat )
import Graphics.Rendering.OpenGL as GL
  ( GLdouble,
    MatrixComponent (scale),
    clear,
    cullFace,
    depthFunc,
    lookAt,
    matrix,
    Capability(Enabled),
    blend,
    multMatrix,
  )
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GLU.Matrix as GM
import qualified Graphics.UI.GLFW as G
import qualified Graphics.UI.GLUT as GLUT
import Language.Haskell.Interpreter
import System.Posix.Unistd
import System.Directory
import System.Process
import System.Environment
import System.Exit
import System.IO
import qualified Text.Printf as PR
import System.IO.Silently

-- |
--
--   | --------------------------------------------------------------------------------
--   | compile: run.sh
--   | ghc -i/Users/cat/myfile/bitbucket/haskelllib -o file file.hs
--   |
--   | KEY: keyboard example, keypress example, modifyIORef example,
--   |
--   | Tuesday, 09 November 2021 11:56 PST
--   |
--   | TODO: Combine Cam{..} and Step{..} in one function with Keyboard input
--   |     Current issue: try to implement orthogonal projective with key one press
--   |                    but Step{..} and Cam{..} are different type class.
--   |
--   | mainLoop w refCam refStep refCount lssVex
--   | keyboardRot refCam refStep (fromIntegral width) (fromIntegral height)
--
--   @
--   data Cam = Cam{alpha::Double, beta::Double, gramma::Double, dist::Double} deriving(Show)
--
--   data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double} deriving(Show)
--   initCam = Cam{alpha=0.0, beta=0.0, gramma=0.0, dist = 0.0}
--   initStep = Step{xx=0.0, yy=0.0, zz=0.0, ww = 0.01}
--
--   SEE: Check the following code why the cube does not draw properly
--   NOTE: /Users/aaa/myfile/bitbucket/opengl/KeyBoardRotate/cube.c
--   @
tmpfile = "/tmp/tmpfile.txt"

{--
mc :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
mc (a, b) (a', b') = (a * a' - b * b', a * b' + a' * b)
--}

convexPts :: IO [Vertex3 GLfloat]
convexPts = return cx
  where
    cx =
      [ Vertex3 0.1 0.1 0,
        Vertex3 0.2 0.6 0,
        Vertex3 0.88 0.9 0,
        Vertex3 0.25 0.34 0,
        Vertex3 0.12 0.8 0,
        Vertex3 1.3 0.12 0
      ]
  

  
drawCylinderX :: [[Vertex3 GLfloat]] -> IO ()
drawCylinderX = drawSurfaceFromList

helpme :: IO ()
helpme = do
  let (+) = (++)
  b <- en "b"
  -- AronModule.clear
  let ls =
        [ "file => " + b + "/tmp/draw.x",
          "PlotGeometry -h => help     ",
          "                            ",
          "point                       ",
          "0.1 0.1 0.1                 ",
          "endpoint                    ",
          "                            ",
          "Support primitives:         ",
          "point, segment and triangle "
        ]
  printBox 4 ls

--   #if (1==0)

errorString :: InterpreterError -> String
errorString (WontCompile es) = DL.intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

say :: String -> Interpreter ()
say = liftIO . putStrLn

emptyLine :: Interpreter ()
emptyLine = say ""

colorls =
  VU.fromList
    [ Color3 0.1 0.1 0.1,
      Color3 0.3 0.1 0.9,
      Color3 0.3 0.7 0.9,
      Color3 0.4 0.1 0.1,
      Color3 0.3 0.6 0.9,
      Color3 0.4 0.1 0.3,
      Color3 0.3 0.4 0.9,
      Color3 0.6 0.1 0.9,
      Color3 0.3 0.7 0.9,
      Color3 0.4 0.1 0.5,
      Color3 0.3 0.8 0.9,
      Color3 0.1 0.1 0.4
    ]

-- |
--   KEY: random color, get color
randomColor :: IO (Color3 GLdouble)
-- randomColor = randomInt 0 (len ls - 1) >>= \x -> return $ ls !! x
randomColor = randomInt 0 (len lt - 1) >>= \x -> return $ lt !! x
  where
    lt =
      [ green,
        yellow,
        gray,
        blue,
        cyan,
        white
      ]

    ls =
      [ Color3 0.8 0.2 0.1,
        Color3 0.3 0.1 0.9,
        Color3 0.3 0.7 0.9,
        Color3 0.4 0.1 0.1,
        Color3 0.3 0.6 0.9,
        Color3 0.4 0.1 0.3,
        Color3 0.3 0.4 0.9,
        Color3 0.6 0.1 0.9,
        Color3 0.3 0.7 0.9,
        Color3 0.4 0.1 0.5,
        Color3 0.3 0.8 0.9,
        Color3 0.1 0.6 0.4
      ]

-- (a -> b -> c) => (a -> b -> a)
randomColorList :: Int -> IO [Color3 GLdouble]
randomColorList n = mapM (const randomColor) [1..n]
  
grid :: [[[Vertex3 GLfloat]]]
grid = [[[Vertex3 a b (a * a - b * b) | a <- aa] | b <- bb] | c <- cc]
  where
    n = 10
    fa = 1 / n
    aa = map (\x -> fa * x) [1 .. n]
    bb = map (\x -> fa * x) [1 .. n]
    cc = map (\x -> fa * x) [1 .. n]

-- f z = z*z
--
-- a    => x
-- b    => y
-- re c => z
-- im c => color
grid4 :: [[(Vertex3 GLfloat, GLfloat)]]
grid4 = [[let c = (C a b) * (C a b) in (Vertex3 a b (re c), im c) | a <- aa] | b <- bb]
  where
    ne = [[let c = sqrtC' (C a b) in (Vertex3 a b (re c), im c) | a <- aa] | b <- bb]
    n = 20
    fa = 1 / (1.5 * n)
    aa = map (\x -> fa * x) [- n .. n]
    bb = map (\x -> fa * x) [- n .. n]

grid4' = (map . map) (\x -> fst x) grid4

trig s1 s2 = map (\x -> foldr (++) [] x) $ (zipWith . zipWith) (\x y -> [x, y]) (init s1) (tail s2)

segment1 = zipWith (\x y -> [Vertex3 (-1) 0 0, y]) [1 ..] test_circle

test_circle :: [Vertex3 GLfloat]
test_circle =
  [ let x = (1 - t * t) / (1 + t * t)
        y = (2 * t) / (1 + t * t)
     in Vertex3 x y 0
    | t <- aa
  ]
  where
    n = 50
    fa = 1 / (0.1 * n)
    aa = map (\x -> fa * x) [- n .. n]

splitPt :: Int -> [(GLfloat, GLfloat, GLfloat)] -> [[(GLfloat, GLfloat, GLfloat)]]
splitPt _ [] = []
splitPt n xs = take n xs : (splitPt n $ drop n xs)

mergeChunk :: Int -> [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
mergeChunk n c = mergeList (take n c) (take n $ drop n c)

bigChunk :: Int -> [(GLfloat, GLfloat, GLfloat)] -> [[(GLfloat, GLfloat, GLfloat)]]
bigChunk n xs = splitPt n xs

renderSurface :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderSurface xs = do
  iterateList
    (bigChunk 80 xs)
    ( \chunk -> do
        let len = length chunk
        let n = div len 2
        let c = mergeChunk n chunk
        let cc = zipWith (\x y -> (x, y)) c [1 ..]
        renderPrimitive TriangleStrip $
          mapM_
            ( \((x, y, z), n) -> do
                case mod n 3 of
                  0 -> do
                    color (Color3 0.8 1 0 :: Color3 GLdouble)
                  1 -> do
                    color (Color3 0 0.5 1 :: Color3 GLdouble)
                  _ -> do
                    color (Color3 1 0 0.7 :: Color3 GLdouble)
                normal $ (Normal3 x y z :: Normal3 GLfloat)
                vertex $ Vertex4 x y z 0.8
            )
            cc
    )

-- |
--
--    translate rotate drawRect drawCircle
type Vex3 = Vertex3 GLfloat

type V3d = Vertex3 GLdouble

drawTri :: [(Vertex3 GLfloat)] -> IO ()
drawTri cx = do
  drawPrimitiveVex LineLoop green cx

-- |
--   === Move object alone X-Axis
--
--   1. move drawRect2d to the right in x
--
--   @
--   moveToX drawRect2d (w, h) x    -- move to the right
--   moveToX drawRect2d (w, h) (-x) -- move to the left
--   @
--
--       drawRect2d w h
--
--           ↑ y
--           |
--         ⌜---⌝
--  ← -x   |   |  → x
--         | + |–--->      X-Axis
--         |   |
--         ⌞---⌟
moveToX :: ((GLfloat, GLfloat) -> IO ()) -> (GLfloat, GLfloat) -> GLdouble -> IO () -- moveToX drawRect2d (w, h) x  -- move to the (x) right, (-1) left
moveToX f (w, h) x = do
  preservingMatrix $ do
    translate (Vector3 x 0 0 :: Vector3 GLdouble)
    f (w, h)

moveToY :: ((GLfloat, GLfloat) -> IO ()) -> (GLfloat, GLfloat) -> GLdouble -> IO ()
moveToY f (w, h) y = do
  preservingMatrix $ do
    translate (Vector3 0 y 0 :: Vector3 GLdouble)
    f (w, h)

moveToZ :: ((GLfloat, GLfloat) -> IO ()) -> (GLfloat, GLfloat) -> GLdouble -> IO ()
moveToZ f (w, h) z = do
  preservingMatrix $ do
    translate (Vector3 0 0 z :: Vector3 GLdouble)
    f (w, h)

-- KEY: string width, string height, font width, font height
-- strWidth <- GLUT.stringWidth GLUT.Roman str
-- strHeight <- GLUT.stringHeight GLUT.Roman str

-- |
--    === cylinder xz-plane, perpendicular to xz-plane
cylinderX :: GLfloat -> IO ()
cylinderX r = drawSurfaceFromList cylinderPt

cylinderPt :: [[Vertex3 GLfloat]]
cylinderPt = cm
  where
    cm =
      let n = 40 :: Int
          δ = (2 * pi) / (rf (n -1)) :: Float
          r = 0.1
          br = 0.2
          σ = 1 / rf (n -1)

          fx :: Int -> Int -> GLfloat
          fx i j =
            let i' = rf i
                j' = rf j
             in (1 / rf 40) * j'

          fy :: Int -> Int -> GLfloat
          fy i j =
            let i' = rf i
                j' = rf j
                n = 3
             in r * cos (δ * i')

          fz :: Int -> Int -> GLfloat
          fz i j =
            let i' = rf i
                j' = rf j
             in r * sin (δ * i')
       in [[Vertex3 (fx j j) (fy i i) (fz i i) | i <- [1 .. n]] | j <- [1 .. n]]

lsfun =
  [ "\\x -> x*x",
    "\\x -> 2*x*x",
    "\\x -> 3*x*x",
    "\\x -> 4*x*x",
    "\\x -> 5*x*x",
    "\\x -> 6*x*x",
    "\\x -> 7*x*x",
    "\\x -> 8*x*x",
    "\\x -> 9*x*x"
  ]

-- |
--    KEY: convert 'String' to 'Vector3' 'GLdouble'
--
--    @
--     Input:
--     vector3
--     0.3 0.4 0.0
--     0.2 0.3 0.0
--     endvector3
--
--     => [Vector3 0.3 0.4 0.0, Vector3 0.2 0.3 0.0]
--    @
takeVector3 :: [String] -> [Vector3 GLdouble]
takeVector3 [] = []
takeVector3 cx = cs
  where
    beg = "vector3"
    end = "endvector3"
    ss = filter (\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    cs = map (\x -> strToVector3 x) ss

-- |
--    KEY: convert 'String' to 'Vertex3' 'GLfloat'
--
--    @
--     Input:
--     triangle
--     0.3 0.4 0.0
--     0.2 0.3 0.0
--     0.1 0.2 0.0
--     endtriangle
--
--     => [Vertex3 0.3 0.4 0.0, Vertex3 0.2 0.3 0.0, Vertex3 0.1 0.2 0.0]
--    @
takeTriangleVex :: [String] -> [Vertex3 GLfloat]
takeTriangleVex [] = []
takeTriangleVex cx = xs
  where
    beg = "triangle"
    end = "endtriangle"
    ss = filter (\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    xs = map (\x -> strToVertex3 x) ss

-- |
--    === KEY: vertex to tuple
vertex3Triple :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)]
vertex3Triple cx = ts
  where
    ls = let s = partList 3 cx in if (len . last) s == 3 then s else init s
    ts = map (\(a : b : c : _) -> (a, b, c)) ls

{-|  
--    === KEY: Convert a list vertices to tuple3 vertices
--
--    @
--     [Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2, Vertex3 0.3 0.3 0.3, Vertex3 0.4 0.4 0.4]
--
--     => [(Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2, Vertex3 0.3 0.3 03)]
--    @
-}
listToTuple3Vex :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)]
listToTuple3Vex cx =
  let lss = partList 3 cx
      lst =
        if len lss > 0
          then
            ( let s = last lss
               in len s == 3 ? lss $ init lss
            )
          else lss
   in map
        ( \x ->
            let a = x ! 0
                b = x ! 1
                c = x ! 2
                (!) = (!!)
             in (a, b, c)
        )
        lst ::
        [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)]

listToTuple3VexD :: [Vertex3 GLdouble] -> [(Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble)]
listToTuple3VexD cx =
  let lss = partList 3 cx
      lst =
        if len lss > 0
          then (let s = last lss in len s == 3 ? lss $ init lss)
          else lss
   in map
        ( \x ->
            let a = x ! 0
                b = x ! 1
                c = x ! 2
                (!) = (!!)
             in (a, b, c)
        )
        lst ::
        [(Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble)]

-- |
--    === KEY: Convert a list vertices to tuple2 vertices
--
--    @
--     [Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2, Vertex3 0.3 0.3 0.3, Vertex3 0.4 0.4 0.4]
--
--     => [(Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2),
--         (Vertex3 0.3 0.3 0.3,  Vertex3 0.4 0.4 0.4)
--        ]
--    @
listToTuple2Vex :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
listToTuple2Vex cx =
  let lss = partList num cx
      lst =
        if ρ lss > 0
          then
            ( let s = last lss
               in ρ s == num ? lss $ init lss
            )
          else lss
   in map
        ( \x ->
            let a = x ! 0
                b = x ! 1
                (!) = (!!)
             in (a, b)
        )
        lst ::
        [(Vertex3 GLfloat, Vertex3 GLfloat)]
  where
    num = 2

maybeX' :: (Maybe a1, Maybe a2) -> b -> ((a1, a2) -> b) -> b
maybeX' (m1, m2) b f = case m1 of
  Nothing -> error "e1"
  Just x1 -> case m2 of
             Nothing -> error "e2"
             Just x2 -> f (x1, x2)
  

-- |
--    KEY: convert 'String' to 'Vertex3' 'GLfloat'
--
--    @
--     Input:
--     segment
--     0.3 0.4 0.0
--     0.2 0.3 0.0
--     0.1 0.2 0.0
--     endsegment
--
--     => [Vertex3 0.3 0.4 0.0, Vertex3 0.2 0.3 0.0, Vertex3 0.1 0.2 0.0]
--    @
takeSegment :: [String] -> [Vertex3 GLfloat]
takeSegment [] = []
takeSegment cx = cs
  where
    beg = "segment"
    end = "endsegment"
    ss = filter (\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    cs = map strToVertex3 ss

-- |
--    KEY: convert 'String' to 'Vertex3' 'GLfloat'
--
--    @
--     Input:
--     point
--     0.3 0.4 0.0
--     0.2 0.3 0.0
--     endpoint
--
--     => [Vertex3 0.3 0.4 0.0, Vertex3 0.2 0.3 0.0]
--    @
takePoint :: [String] -> [Vertex3 GLfloat]
takePoint [] = []
takePoint cx = cs
  where
    beg = "point"
    end = "endpoint"
    ss = filter (\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    cs = map strToVertex3 ss

circleNArc' :: Vertex3 GLfloat -> Double -> Integer -> (GLfloat, GLfloat) -> [Vertex3 GLfloat]
circleNArc' (Vertex3 x₀ y₀ z₀) r n (r₀, r₁) =
  [ let δ = (r₁ - r₀) / (rf n); r' = rf r
     in Vertex3 (r' * cos (r₀ + (rf x) * δ) + x₀) (r' * sin (r₀ + (rf x) * δ) + y₀) z₀
    | x <- [0 .. n]
  ]

-- |
--   [0, 1, 2]
--   x0 -- x1 -- x2
--
--   curvePtK::(GLfloat -> GLfloat)->(GLfloat,    GLfloat) -> Integer ->[Vertex3 GLfloat]
--                                     ↑             ↑          ↑
--                                     +-  interval  +          |
--                                                              + - n steps
--                                     |             |
--                                     x_0           x_1
--
--
--                                      (x_1 - x_0) / n
--                    f(x) = x^2
--
--
--    curvePtK f (0, 1.0) 10
--     f(x) = x^2
--    |-----+------|
--    |   x |  x^2 |
--    | 0.1 | 0.01 |
--    | 0.2 | 0.04 |
--    | 0.3 | 0.09 |
--    | 0.4 | 0.16 |
--    | 0.5 | 0.25 |
--    | 0.6 | 0.36 |
--    | 0.7 | 0.49 |
--    | 0.8 | 0.64 |
--    | 0.9 | 0.81 |
--    | 1.0 |  1.0 |
--    |-----+------|
curvePtK :: (GLfloat -> GLfloat) -> (GLfloat, GLfloat) -> Integer -> [Vertex3 GLfloat]
curvePtK f (x₀, x₁) n = [Vertex3 x (f x) 0 | x <- let δ = (x₁ - x₀) / (rf n) in map (\x -> x₀ + (rf x) * δ) [0 .. n]]

-- |
--   === KEY: use multMatrix, multiply matrix, multiply matrix with vector
--
--   https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html
--
--  @
--      (cos α  + i sin α) ( cos β + i sin β)
--    = cos α  * cos β + sin α * cos β + i cos α * cos β + i cos α * sin β
--    = (cos α  * cos β + sin α * cos β) + i (cos α * cos β + cos α * sin β)
--    = cos (α + β) + i sin (α + β)
--
--     m 1    => 0
--       0       1
--
--     m 0    => -1
--       1       0
--
--    1  0      0 -1
--    0  1      1  0
--
--                     →  X -  \
--                              ↓
--                     |       Y
--                     \      /
--                       Z  ←
--
--                Right-hand Rule
--
--                      y
--                      ^
--                      |
--                      + - ->   x
--                     /
--                    /
--                   z
--
--                 x (x) y => Z
--             rotate around Z-axis
--             cos x   -sin x  0
--             sin x    cos x  0
--             0        0      1
--                   ↓
--             Swap row(2, 3) => M = -M
--                   ↓
--
--             rotate around X-axis
--             cos x   sin x  0
--             0        0      1
--             -sin x    cos x  0
--
--                    ↓
--             Swap row (1, 2) => M = -M
--                    ↓
--
--             rotate around Y-axis
--             0        0      1
--             cos x   -sin x  0
--             sin x    cos x  0
--  @
testmultMatrix :: IO ()
testmultMatrix = do
  let x = 1.0 :: GLfloat
  let y = 0.0 :: GLfloat
  let z = 0.0 :: GLfloat
  mat <-
    newMatrix
      RowMajor
      [ 1, 0, 0, x,
        0, 1, 0, y,
        0, 0, 1, z,
        0, 0, 0, 1
      ] ::
      IO (GLmatrix GLfloat)

  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html
  -- multMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
  GL.multMatrix mat

  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
  -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
  ls <- getMatrixComponents RowMajor mat -- [GLfloat]
  -- pre ls
  writeFileList "/tmp/m.x" $ map show ls
  
{-|
   KEY: rotate around Z-Axis
-}  
multiRotateZ :: GLfloat -> IO [GLfloat]
multiRotateZ x = do
  mat <-
    newMatrix
      RowMajor
      [ cos x,  (negate . sin) x, 0, 0,
        sin x,  cos x,            0, 0,
        0,      0,                1, 0,
        0,      0,                0, 1
      ] :: IO (GLmatrix GLfloat)
  GL.multMatrix mat
  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
  -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
  ls <- getMatrixComponents RowMajor mat -- [GLfloat]
  -- pre ls
  writeFileList "/tmp/mz.x" $ map show ls
  return ls

testMeZ :: GLfloat -> IO()
testMeZ rad = do
  preservingMatrix $ do
    loadIdentity
    rotate (radianToDegree rad) (Vector3 0 0 1 :: Vector3 GLfloat)
    ls <- getModelviewMatrixRow
    let f s = map (\x -> abs x < 0.00001 ? 0 $ x) s
    let lx = partList 4 $ f ls
    tx <- (cap . printMat) lx
    logFileG ["testMeZ 90 Vector3 0 0 1"]
    logFileG [tx]
  
testMeX :: GLfloat -> IO()
testMeX rad = do
  preservingMatrix $ do
    loadIdentity
    rotate (radianToDegree rad) $ (Vector3 1 0 0 :: Vector3 GLfloat)
    ls <- getModelviewMatrixRow
    let f s = map (\x -> abs x < 0.00001 ? 0 $ x) s
    let lx = partList 4 $ f ls
    tx <- (cap . printMat) lx
    logFileG ["testMeX 90 Vector3 1 0 0"]
    logFileG [tx]
  
testMeY :: GLfloat -> IO()
testMeY rad = do
  preservingMatrix $ do
    loadIdentity
    rotate (radianToDegree rad) (Vector3 0 1 0 :: Vector3 GLfloat)
    ls <- getModelviewMatrixRow
    let f s = map (\x -> abs x < 0.00001 ? 0 $ x) s
    let lx = partList 4 $ f ls
    tx <- (cap . printMat) lx
    logFileG ["testMeY 90 Vector3 0 1 0"]
    logFileG [tx]
  
{-|
   KEY: rotate around Y-Axis
-}
multiRotateY :: GLfloat -> IO [GLfloat]
multiRotateY x = do
  mat <-
    newMatrix
      RowMajor
      [ cos x,             0, sin x, 0,
        0,                 1, 0,     0,
        (negate . sin) x,  0, cos x, 0,
        0,                 0, 0,     1
      ] :: IO (GLmatrix GLfloat)
  GL.multMatrix mat
  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
  -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
  ls <- getMatrixComponents RowMajor mat -- [GLfloat]
  -- pre ls
  writeFileList "/tmp/my.x" $ map show ls
  return ls

{-|
   KEY: rotate around X-Axis
-}
multiRotateX :: GLfloat -> IO [GLfloat]
multiRotateX x = do
  mat <-
    newMatrix
      RowMajor
      [
        1, 0,                 0,                0,
        0, cos x,             (negate . sin) x, 0,
        0, sin x,             cos x,            0,          
        0, 0,                 0,                1
      ] :: IO (GLmatrix GLfloat)
  GL.multMatrix mat
  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
  -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
  ls <- getMatrixComponents RowMajor mat -- [GLfloat]
  -- pre ls
  writeFileList "/tmp/mx.x" $ map show ls
  return ls

multiModelScale ::(GLfloat, GLfloat, GLfloat) -> IO [GLfloat]
multiModelScale (x, y, z) = do
  mat <-
    newMatrix
      RowMajor
      [ x,0,0,0,
        0,y,0,0,
        0,0,z,0,
        0,0,0,1
      ] :: IO (GLmatrix GLfloat)

  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html
  -- multMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
  GL.multMatrix mat
  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
  -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
  ls <- getMatrixComponents RowMajor mat -- [GLfloat]
  writeFileList "/tmp/m4.x" $ map show ls
  return ls
  
multiModelviewVec :: Vector3 GLfloat -> IO [GLfloat]
multiModelviewVec (Vector3 x y z) = do
  mat <-
    newMatrix
      RowMajor
      [ 1,0,0,x,
        0,1,0,y,
        0,0,1,z,
        0,0,0,1
      ] :: IO (GLmatrix GLfloat)
  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html
  -- multMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
  GL.multMatrix mat
  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
  -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
  getMatrixComponents RowMajor mat -- [GLfloat]
  -- pre ls
  -- writeFileList "/tmp/m.x" $ map show ls
  
multiModelviewMat :: [GLfloat] -> IO [GLfloat]
multiModelviewMat ls = do
  mat <- newMatrix RowMajor ls :: IO (GLmatrix GLfloat)
  GL.multMatrix mat
  getMatrixComponents RowMajor mat -- [GLfloat]

  
getMatrixTest :: IO ()
getMatrixTest = do
  -- let pnameMatrix = getMatrix $ matrixModeToGetMatrix Projection
  -- matrixModeToGetMatrix :: MatrixMode -> GetPName
  -- let m = matrixModeToGetMatrix Projection

  -- matrixModeToGetMatrix is from hidden module
  -- Use src/Graphics
  -- let m = matrixModeToGetMatrix $ Modelview 16
  -- getMatrixf :: p -> Ptr GLfloat -> IO ()
  --                 let ls = [ 0, 0, 0, 0,
  --                            0, 0, 0, 0,
  --                            0, 0, 0, 0,
  --                            0, 0, 0, 0
  --                          ]::[GLfloat]
  --
  --                 getMatrixf m (Ptr ls)
  fl
  fl
  -- let m2 = getMatrix m
  return ()

{--
     x
       y
   z

   x * y = z
   y * z = x
--}
  
getModelviewMatrixRow :: IO [GLfloat]
getModelviewMatrixRow = do
  preservingMatrix $ do
    let stateVar = GL.matrix (Just $ Modelview 16) :: StateVar (GLmatrix GLfloat)
    m1 <- Data.StateVar.get stateVar
    pre m1
    ls <- getMatrixComponents RowMajor m1  -- [GLfloat]
    -- ls <- getMatrixComponents ColumnMajor m1 -- [GLfloat]
    pre ls
    writeFileList "/tmp/m1.x" $ map show ls
    return ls


rotateN :: Int -> [[a]] -> [[a]]
rotateN n = foldl (\f g -> f . g) id $ take (abs n) $ repeat $ n >= 0 ? (reverse . DL.transpose) $ (DL.transpose . reverse)

--                                     shape
--                       color            |
--          tetris type     |             |
--     Global ID  |         |             |
--           |    |         |             |
mkTetris1 :: Int -> Int -> (Int, Int, Color3 GLdouble, [[Int]])
mkTetris1 bt bid = (bt, bid, white, bk)
  where
    bk =
      [ [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 1, 1, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0]
      ]

initXYZAxis :: XYZAxis
initXYZAxis = XYZAxis {xa = False, ya = False, za = False}

initGlobal :: GlobalRef
initGlobal =
  GlobalRef
    { str_ = "",
      cursor_ = (0.0, 0.0),
      xyzAxis_ = initXYZAxis,
      mousePressed_ = (False, (0.0, 0.0)),
      drawRectX_ = (Vertex3 (-0.2) (-0.2) (0.0 :: GLfloat), Vertex3 0.2 0.2 (0.0 :: GLfloat)),
      tranDrawRectX_ = Vector3 0.0 0.0 (0.0 :: GLdouble),
      fovDegree_ = 100.0,
      drawPts_ = [[Vertex3 0.0 0.0 0.0]],
      randomWalk_ = [Vertex3 0.0 0.0 0.0, Vertex3 0.1 0.1 0.1],
      boardMap_ = DM.empty,
      boardMap1_ = DM.empty,
      moveX_ = 0,
      moveY_ = 0,
      block1_ =
        [ ((0 - 2, 0 + 8), gray),
          ((1 - 2, 0 + 8), gray),
          ((2 - 2, 0 + 8), gray),
          ((3 - 2, 0 + 8), gray),
          ((4 - 2, 0 + 8), gray)
        ],
      rectGrid_ = initRectGrid,
      centerBrick_ = map (\y -> map (\x -> (x - 2, y - 2)) [0 .. len (head $ bk1_ initGlobal) - 1]) $ reverse [0 .. len (bk1_ initGlobal) - 1],
      bk1_ =
        [ [0, 0, 0, 0, 0],
          [0, 0, 1, 0, 0],
          [0, 1, 1, 1, 0],
          [0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0]
        ],
      bk2_ =
        [ [(0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green)],
          [(0, 0, green), (0, 0, green), (1, 0, green), (0, 0, green), (0, 0, green)],
          [(0, 0, green), (1, 0, green), (1, 0, green), (1, 0, green), (0, 0, green)],
          [(0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green)],
          [(0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green)]
        ],
      rot_ = False,
      rotDeg_ = 0,
      time1_ = 0,
      count1_ = 10000000,
      rotN_ = 0,
      blockCount_ = 1,
      tetrisCount_ = 1,
      tetris1_ = mkTetris1 (blockCount_ initGlobal) 0,
      tetris1X_ = (BlockAttr {isFilled_ = True, typeId_ = 1, tetrisNum_ = (blockCount_ initGlobal), color_ = blue}, bk1_ initGlobal),
      isPaused_ = False
    }

{--

```
data MyRec = {a_ :: Int, b_ :: Int} deriving (Show)

let r = MyRec = {a_ = 3, b_ = a_ + 4}
```

  ---------------------------------------m1---------------------------------------
  bk1_ =
  0 0 0 0 0
  0 0 0 0 0
  1 1 1 1 1
  0 0 0 0 0
  0 0 0 0 0
  ---------------------------------------sk---------------------------------------
  centerBrick_ =
  ((-2,2),0)  ((-1,2),0)  ((0,2),0)  ((1,2),0)  ((2,2),0)
  ((-2,1),0)  ((-1,1),0)  ((0,1),0)  ((1,1),0)  ((2,1),0)
  ((-2,0),1)  ((-1,0),1)  ((0,0),1)  ((1,0),1)  ((2,0),1)
  ((-2,-1),0) ((-1,-1),0) ((0,-1),0) ((1,-1),0) ((2,-1),0)
  ((-2,-2),0) ((-1,-2),0) ((0,-2),0) ((1,-2),0) ((2,-2),0)
--}

-- DONOTDELETE: /Users/aaa/myfile/github/haskell-opengl-tetris/src/keyBoardCallBack3d-2024-01-19-11-19-06.x
keyBoardCallBack3d :: IORef CameraRot -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
keyBoardCallBack3d refCamRot refGlobalRef ioArray window key scanCode keyState modKeys = do
  pp "keyBoardCallBack in $b/haskelllib/AronOpenGL.hs"
  putStrLn $ "inside =>" ++ show keyState ++ " " ++ show key
  globalRef <- readIORef refGlobalRef
  cam <- readIORef refCamRot
  let axisOld = xyzAxis_ globalRef
  let fovOld = fovDegree_ globalRef
  logFileG ["fovOld=" ++ show fovOld]
  rr <- readIORef refGlobalRef <&> rectGrid_
  case keyState of
    ks
      | ks `elem` [G.KeyState'Pressed, G.KeyState'Repeating] -> do
        -- G.KeyState'Pressed -> do
        -- write Step{...} to ref
        case key of
          k

            | k == G.Key'Right -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 -> do
                         modifyIORef refCamRot (\s -> s{alpha_ = alpha_ s + _STEP})
                     | v == 2 -> do
                         modifyIORef refCamRot (\s -> s{beta_ = beta_ s + _STEP})
                     | v == 3 -> do
                         modifyIORef refCamRot (\s -> s{gramma_ = gramma_ s + _STEP})
                     | otherwise -> error "Invalid currXYZ_ value, Key Right"
  
            | k == G.Key'Left -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 -> do
                         modifyIORef refCamRot (\s -> s{alpha_ = alpha_ s - _STEP})
                     | v == 2 -> do
                         modifyIORef refCamRot (\s -> s{beta_ = beta_ s - _STEP})
                     | v == 3 -> do
                         modifyIORef refCamRot (\s -> s{gramma_ = gramma_ s - _STEP})
                     | otherwise -> error "Invalid currXYZ_ value, Key Left"
  
            --  | k == G.Key'Left -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = - _STEP, yy = 0, zz = 0}})
            | k == G.Key'Up -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = _STEP, zz = 0}})
            | k == G.Key'Down -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = - _STEP, zz = 0}})
            | k == G.Key'9 -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = 0, zz = _STEP}})
            | k == G.Key'0 -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = 0, zz = - _STEP}})
            | k == G.Key'8 -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = 0, zz = 0, ww = _STEP}})
            | k == G.Key'7 -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = 0, zz = 0, ww = - _STEP}})
            | k == G.Key'X -> do
                modifyIORef refCamRot (\s -> let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 1.0 0}})
                -- modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) xAxis})
            --                                  ↑
            --                                  + -> Update Coord to YZ-plane

            --  | k == G.Key'Y -> modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) yAxis})
            | k == G.Key'1 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 1, vecAxisY_ = vecY_ s, vecAxisZ_ = vecZ_ s})
            | k == G.Key'2 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 2, vecAxisX_ = vecX_ s, vecAxisZ_ = vecZ_ s})
            | k == G.Key'3 -> do
                vec <- readIORef refCamRot <&> vecRotZ_
                modifyIORef refCamRot (\s -> s{currXYZ_ = 3})
  
            | k == G.Key'Y ->
                modifyIORef refCamRot (\s -> let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 1.0 0, up_ = Vector3 1.0 0 0}})
            | k == G.Key'T ->
                modifyIORef refCamRot (\s -> let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 (-1.0) 0, up_ = Vector3 1.0 0 0}})

            --                                  ↑
            --                                  + -> Update Coord to XZ-plane

            | k == G.Key'Z -> do
                modifyIORef refCamRot (\s -> let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 0 1.0}})
  
            | k == G.Key'E -> do
                modifyIORef refCamRot (\s -> let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 0 (-1.0)}})
                -- modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) zAxis})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane

            -- zoom out
            --  | k == G.Key'O -> modifyIORef refGlobalRef (\s -> s {fovDegree_ = fovDegree_ s + 5.0})
            | k == G.Key'O -> do
                modifyIORef refCamRot (\s -> let p = persp_ s in s{persp_ = p{fov_ = fov_ p + 5.0}})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane
                logFileG ["CameraRot_O"]
                readIORef refCamRot >>= \x -> logFileG [show x]
            -- zoom in
            | k == G.Key'I -> do
                modifyIORef refCamRot (\s -> let p = persp_ s in s{persp_ = p{fov_ = fov_ p - 5.0}})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane
                logFileG ["CameraRot_I"]
                readIORef refCamRot >>= \x -> logFileG [show x]

            -- TODO: In orthogonal projective status,
            | k == G.Key'Space -> modifyIORef refCamRot (\s -> s {delta_ = initStep})
            --  | k == G.Key'O -> modifyIORef refCamRot (\s -> s {fovDeg_ = fovDeg_ s + 5.0})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane

            | k == G.Key'W -> do
              nowTime <- timeNowMilli
              modifyIORef refGlobalRef (\s -> s {count1_ = 0})
              modifyIORef refGlobalRef (\s -> s {time1_ = nowTime})
              modifyIORef refGlobalRef (\s -> s {rot_ = True})
              fw "Rotate Block"
              pp "rotate me"
            | k == G.Key'P -> do
              pp "Pause"
            | k == G.Key'A -> do
              pp "rotateN 1"
            | k == G.Key'L || k == G.Key'R -> do
              print "kk"
            | k == G.Key'U -> do
              print "kk"
            | k == G.Key'D -> do
              print "kk"
            | otherwise -> pp $ "Unknown Key Press" ++ show key
      | ks == G.KeyState'Released -> do
        -- G.KeyState'Released -> do
        if key == G.Key'Right then pp "Release Key => Right" else pp "Press No Right"
        if key == G.Key'Left then pp "Release Key => left" else pp "Press No Right"
        if key == G.Key'Up then pp "Release Key => up" else pp "Release No Up"
        if key == G.Key'Down then pp "Release Key => Down" else pp "Release No Down"
      -- if key == G.Key'R  then modifyIORef refGlobalRef (\x -> x{moveX_ = 0}) else pp "Release No Down"
      -- if key == G.Key'L  then modifyIORef refGlobalRef (\x -> x{moveY_ = 0}) else pp "Release No Down"
      | otherwise -> pp "Unknow keyState"
  when
    (key == G.Key'Escape && keyState == G.KeyState'Pressed)
    (G.setWindowShouldClose window True)

keyBoardCallBack2d :: IORef CameraRot -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
keyBoardCallBack2d refCamRot refGlobalRef ioArray window key scanCode keyState modKeys = do
  pp "keyBoardCallBack in $b/haskelllib/AronOpenGL.hs"
  putStrLn $ "inside =>" ++ show keyState ++ " " ++ show key
  globalRef <- readIORef refGlobalRef
  cam <- readIORef refCamRot
  let axisOld = xyzAxis_ globalRef
  let fovOld = fovDegree_ globalRef
  logFileG ["fovOld=" ++ show fovOld]
  rr <- readIORef refGlobalRef <&> rectGrid_
  coordFrame <- readIORef refCamRot <&> coordFrame_
  case keyState of
    ks
      | ks `elem` [G.KeyState'Pressed, G.KeyState'Repeating] -> do
        -- G.KeyState'Pressed -> do
        -- write Step{...} to ref
        case key of
          k
            | k == G.Key'Right -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 -> do
                         modifyIORef refCamRot (\s -> s{alpha_ = alpha_ s + _STEP})
                         alpha <- readIORef refCamRot <&> alpha_
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecXCF (rf $ (pi/180) * _STEP)
                         -- multiModelviewMat ls
                         let lsY = vecToList3 vecYCF ++ [0]
                         let lsZ = vecToList3 vecZCF ++ [0]
                         let vy3 = (listToVec . join) $ mm `multiVec` lsY
                         let vz3 = (listToVec . join) $ mm `multiVec` lsZ

                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vecXCF, vy3, vz3)})
                         mm' <- (cap . printMat) mm
                         logFileG ["mm_matrix"]
                         logFileG [mm']

                         logFileG ["alpha=" ++ show alpha]
                         logFileG ["vecXCF"]
                         logFileG [show vecXCF]  
                         logFileG ["n=1 vecvy3"]
                         logFileG [show vy3]
                         logFileG ["n=1 vecvz3"]
                         logFileG [show vz3]
                         logFileG ["lenn lsY lsZ"]
                         logFileG $ map show lsY
                         logFileG $ map show lsZ
  
                     | v == 2 -> do
                         modifyIORef refCamRot (\s -> s{beta_ = beta_ s + _STEP})
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecYCF (rf $ (pi/180) * _STEP)
                         -- multiModelviewMat ls
                         let lsX = vecToList3 vecXCF ++ [0]
                         let lsZ = vecToList3 vecZCF ++ [0]
                         let vx3 = (listToVec . join) $ mm `multiVec` lsX
                         let vz3 = (listToVec . join) $ mm `multiVec` lsZ
                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vx3, vecYCF, vz3)})
                         mm' <- (cap . printMat) mm
                         logFileG ["mm_matrix"]
                         logFileG [mm']
  
                     | v == 3 -> do
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecZCF (rf $ (pi/180) * _STEP)
                         let lsX = vecToList3 vecXCF ++ [0]
                         let lsY = vecToList3 vecYCF ++ [0]
                         let vx3 = (listToVec . join) $ mm `multiVec` lsX
                         let vy3 = (listToVec . join) $ mm `multiVec` lsY
                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vx3, vy3, vecZCF)})
                     | v == 4 -> do
                         persp <- readIORef refCamRot <&> persp_
                         modifyIORef refCamRot (\s -> s{persp_ = persp{zn_ = zn_ persp + 0.1}})
                         persp' <- readIORef refCamRot <&> persp_
                         logFileGT "persp_1" [show persp']
  
                     | otherwise -> error "Invalid currXYZ_ value, Key Right"
  
            | k == G.Key'Left -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 -> do
                         modifyIORef refCamRot (\s -> s{alpha_ = alpha_ s - _STEP})
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecXCF (rf $ (pi/180) * negate _STEP)
                         let lsY = vecToList3 vecYCF ++ [0]
                         let lsZ = vecToList3 vecZCF ++ [0]
                         let vy3 = (listToVec . join) $ mm `multiVec` lsY
                         let vz3 = (listToVec . join) $ mm `multiVec` lsZ

                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vecXCF, vy3, vz3)})

                     | v == 2 -> do
                         modifyIORef refCamRot (\s -> s{beta_ = beta_ s - _STEP})
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecYCF (rf $ (pi/180) * negate _STEP)
                         let lsX = vecToList3 vecXCF ++ [0]
                         let lsZ = vecToList3 vecZCF ++ [0]
                         let vx3 = (listToVec . join) $ mm `multiVec` lsX
                         let vz3 = (listToVec . join) $ mm `multiVec` lsZ

                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vx3, vecYCF, vz3)})
  
                     | v == 3 -> do
                         let vecXCF = coordFrame ^._1
                         let vecYCF = coordFrame ^._2
                         let vecZCF = coordFrame ^._3
                         let (mm, ls) = rotMat4Tup vecZCF (rf $ (pi/180) * negate _STEP)
                         let lsX = vecToList3 vecXCF ++ [0]
                         let lsY = vecToList3 vecYCF ++ [0]
                         let vx3 = (listToVec . join) $ mm `multiVec` lsX
                         let vy3 = (listToVec . join) $ mm `multiVec` lsY
                         modifyIORef refCamRot (\s -> s{coordFrameMat_ = let m = coordFrameMat_ s in mm `multiMat` m})
                         modifyIORef refCamRot (\s -> s{coordFrame_ = (vx3, vy3, vecZCF)})
                     | v == 4 -> do
                         persp <- readIORef refCamRot <&> persp_
                         modifyIORef refCamRot (\s -> s{persp_ = persp{zn_ = zn_ persp - 0.1}})                             
                     | otherwise -> error "Invalid currXYZ_ value, Key Left"

            | k == G.Key'Up -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = _STEP, zz = 0}})
            | k == G.Key'Down -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = - _STEP, zz = 0}})
            | k == G.Key'X -> do
                modifyIORef refCamRot (\s -> let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 1.0 0}})
            | k == G.Key'1 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 1, vecAxisY_ = vecY_ s, vecAxisZ_ = vecZ_ s})
            | k == G.Key'2 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 2, vecAxisX_ = vecX_ s, vecAxisZ_ = vecZ_ s})
            | k == G.Key'3 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 3})
            | k == G.Key'4 -> do
                modifyIORef refCamRot (\s -> s{currXYZ_ = 4})
  
            | k == G.Key'Z -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 -> do
                         modifyIORef refCamRot(\s ->let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 0 1.0}})
                     | v == 2 -> do
                         -- modifyIORef refCamRot(\s ->let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 1.0 0, up_ = Vector3 1.0 0 0}})
                         pp ""
                     | v == 3 -> do
                         modifyIORef refCamRot(\s ->let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 1.0 0 0}})
                     | otherwise -> do
                         pp "unknown number"  
  
            --  | k == G.Key'O -> modifyIORef refGlobalRef (\s -> s {fovDegree_ = fovDegree_ s + 5.0})
            | k == G.Key'O -> do
                modifyIORef refCamRot (\s -> let p = persp_ s in s{persp_ = p{fov_ = fov_ p + 5.0}})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane
                logFileG ["CameraRot_O"]
                readIORef refCamRot >>= \x -> logFileG [show x]
            -- zoom in
            | k == G.Key'I -> do
                modifyIORef refCamRot (\s -> let p = persp_ s in s{persp_ = p{fov_ = fov_ p - 5.0}})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane
                logFileG ["CameraRot_I"]
                readIORef refCamRot >>= \x -> logFileG [show x]

            -- TODO: In orthogonal projective status,
            | k == G.Key'Space -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 || v == 2 || v == 3 || v == 4 -> do
                         modifyIORef refCamRot (\s -> s {coordFrame_ = (Vector3 1 0 0, Vector3 0 1 0, Vector3 0 0 1)})
                         modifyIORef refCamRot (\s -> s {coordFrameMat_ = matId 4})                           
                     | otherwise -> return () 
  
            | otherwise -> pp $ "Unknown Key Press" ++ show key
      | ks == G.KeyState'Released -> do
        if key == G.Key'Right then do
          pp "Release Key => Right"
          else pp "Press No Right"
        if key == G.Key'Left then do
          pp "Release Key => left"
          else pp "Press No Right"
        if key == G.Key'Up then do
          pp "Release Key => up"
          else pp "Release No Up"
        if key == G.Key'Down then do
          pp "Release Key => Down"
          else pp "Release No Down"
      | otherwise -> pp "Unknow keyState"
  when
    (key == G.Key'Escape && keyState == G.KeyState'Pressed)
    (G.setWindowShouldClose window True)



-- |
--    KEY:
--    NOTE: USED
keyBoardCallBackX :: IORef Step -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
keyBoardCallBackX refStep refGlobalRef ioArray window key scanCode keyState modKeys = do
  pp "keyBoardCallBack in $b/haskelllib/AronOpenGL.hs"
  putStrLn $ "inside =>" ++ show keyState ++ " " ++ show key
  globalRef <- readIORef refGlobalRef
  let axisOld = xyzAxis_ globalRef
  let fovOld = fovDegree_ globalRef
  logFileG ["fovOld=" ++ show fovOld]
  rr <- readIORef refGlobalRef <&> rectGrid_
  case keyState of
    ks
      | ks `elem` [G.KeyState'Pressed, G.KeyState'Repeating] -> do
        -- G.KeyState'Pressed -> do
        -- write Step{...} to ref
        case key of
          k
            | k == G.Key'Right -> modifyIORef refStep (\s -> s {xx = _STEP, yy = 0, zz = 0})
            | k == G.Key'Left -> modifyIORef refStep (\s -> s {xx = - _STEP, yy = 0, zz = 0})
            | k == G.Key'Up -> modifyIORef refStep (\s -> s {xx = 0, yy = _STEP, zz = 0})
            | k == G.Key'Down -> modifyIORef refStep (\s -> s {xx = 0, yy = - _STEP, zz = 0})
            | k == G.Key'9 -> modifyIORef refStep (\s -> s {xx = 0, yy = 0, zz = _STEP})
            | k == G.Key'0 -> modifyIORef refStep (\s -> s {xx = 0, yy = 0, zz = - _STEP})
            | k == G.Key'8 -> modifyIORef refStep (\s -> s {ww = _STEP})
            | k == G.Key'7 -> modifyIORef refStep (\s -> s {ww = - _STEP})
            | k == G.Key'X -> modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) xAxis})
            --                                  ↑
            --                                  + -> Update Coord to YZ-plane

            | k == G.Key'Y -> modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) yAxis})
            --                                  ↑
            --                                  + -> Update Coord to XZ-plane

            | k == G.Key'Z -> modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) zAxis})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane

            -- zoom out
            | k == G.Key'O -> modifyIORef refGlobalRef (\s -> s {fovDegree_ = fovDegree_ s + 5.0})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane
            -- zoom in
            | k == G.Key'I -> modifyIORef refGlobalRef (\s -> s {fovDegree_ = fovDegree_ s - 5.0})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane

            -- TODO: In orthogonal projective status,
            | k == G.Key'Space -> do
                writeIORef refStep initStep

            | k == G.Key'W -> do
              nowTime <- timeNowMilli
              modifyIORef refGlobalRef (\s -> s {count1_ = 0})
              modifyIORef refGlobalRef (\s -> s {time1_ = nowTime})
              modifyIORef refGlobalRef (\s -> s {rot_ = True})
              fw "Rotate Block"
              pp "rotate me"
            | k == G.Key'P -> do
              pp "Pause"
            | k == G.Key'A -> do
              pp "rotateN 1"
            | k == G.Key'L || k == G.Key'R -> do
              print "kk"
            | k == G.Key'U -> do
              print "kk"
            | k == G.Key'D -> do
              print "kk"
            | otherwise -> pp $ "Unknown Key Press" ++ show key
      | ks == G.KeyState'Released -> do
        -- G.KeyState'Released -> do
        if key == G.Key'Right then pp "Release Key => Right" else pp "Press No Right"
        if key == G.Key'Left then pp "Release Key => left" else pp "Press No Right"
        if key == G.Key'Up then pp "Release Key => up" else pp "Release No Up"
        if key == G.Key'Down then pp "Release Key => Down" else pp "Release No Down"
      -- if key == G.Key'R  then modifyIORef refGlobalRef (\x -> x{moveX_ = 0}) else pp "Release No Down"
      -- if key == G.Key'L  then modifyIORef refGlobalRef (\x -> x{moveY_ = 0}) else pp "Release No Down"
      | otherwise -> pp "Unknow keyState"
  when
    (key == G.Key'Escape && keyState == G.KeyState'Pressed)
    (G.setWindowShouldClose window True)


vecdTovecf :: Vector3 GLdouble -> Vector3 GLfloat
vecdTovecf (Vector3 x y z) = Vector3 x' y' z'
  where
    x' = rf x
    y' = rf y
    z' = rf z

-- |
--   === KEY: points set for sphere
--   The function from AronGraphic.hs spherePts
spherePtsX :: [[Vertex3 GLfloat]]
spherePtsX = geneParamSurface fx fy fz n
  where
    n = 20 :: Int
    δ = (2 * pi) / (rf (n -1)) :: Float
    r = 0.1
    br = 0.2
    σ = 1 / rf (n -1)

    fx :: Int -> Int -> GLfloat
    fx i j =
      let i' = rf i
          j' = rf j
          α = δ * i'
          β = δ * j'
       in r * cos (α) * cos (β)
    fy :: Int -> Int -> GLfloat
    fy i j =
      let i' = rf i
          j' = rf j
          α = δ * i'
          β = δ * j'
       in r * cos (α) * sin (β)

    fz :: Int -> Int -> GLfloat
    fz i j =
      let i' = rf i
          j' = rf j
          α = δ * i'
          β = δ * j'
       in r * sin α

drawRectX2 :: G.Window -> IORef GlobalRef -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat) -> IO ()
drawRectX2 w ioGlobalRef (p0@(Vertex3 x0 y0 z0), p1@(Vertex3 x1 y1 z1)) c@(x, y) = do
  preservingMatrix $ do
    tvec <- getTranVecDrawRectX ioGlobalRef
    (isPre, _) <- getMousePressed ioGlobalRef
    drawRectMouse w tvec (p0, p1) (x, y, 0.0) isPre

drawRectMouse :: G.Window -> (Vector3 GLdouble) -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat, GLfloat) -> Bool -> IO ()
drawRectMouse w (Vector3 vx vy vz) (p0, p1) (mx, my, mz) isPre = do
  let tvec = Vector3 vx vy vz
  logFileG [show tvec]
  translate tvec
  --
  --      tvec                                          tvec'
  --        ↓                                             ↓
  --   mouse movement coord-system
  --     ↓                                      ^    translate, coord-system
  --  (0, 0)                                    |
  --    + - - - ->                              | (0,0)
  --    |                 =>         ---------- + - - - - - ->
  --    |                                       |
  --    |                                       |
  --    v                                       |
  --                                            |
  --
  --    x-axis is the same direction
  --    y-axis is opposive direction
  --
  --  =>  (x, y, z) => (x, -y, z)
  --
  let tvec' = Vector3 vx (- vy) vz
  (winWidth, winHeight) <- G.getWindowSize w
  let (winW, winH) = (rf winWidth, rf winHeight)
  isIn <- isPtInsideRect w (p0 +: (d2f tvec'), p1 +: (d2f tvec')) (mx, my)
  drawRectColor (isPre && isIn ? green $ red) (p0, p1)

d2f :: Vector3 GLdouble -> Vector3 GLfloat
d2f (Vector3 x y z) = Vector3 x' y' z'
  where
    x' = rf x
    y' = rf y
    z' = rf z

-- |
--    === KEY: check whether point (x, y) is inside the rectangle
--
--    @
--                    | upLeftY
--         upLeftX -> v +----------+
--                                 |
--                                 |
--                      +          +
--                                 ^ <-  downRightX
--                                 |
--                              downRightY
--                                                                             +- - -> translate Vector3
--                                 + -> upLeft                                 |
--                                 |                                           |
--                                 |                     + -> downRight        |
--                                 |                     |                     |                    +-> cursor pos
--                                 ↓                     ↓                     ↓                    ↓
--    @
isPtInsideRectTran :: G.Window -> (Vertex3 GLfloat, Vertex3 GLfloat) -> Vector3 GLdouble -> IO Bool
isPtInsideRectTran w (p0, p1) (Vector3 a b c) = do
  cursorPos <- getCursorPosf w -- IO (GLfloat, GLfloat)
  let tvec = Vector3 a (- b) c
  isPtInsideRect w (p0 +: (d2f tvec), p1 +: (d2f tvec)) cursorPos

isPtInsideRect :: G.Window -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat) -> IO Bool
isPtInsideRect w (Vertex3 x0 y0 z0, Vertex3 x1 y1 z1) (x, y) = do
  let ndcWidth = 4.0
  (winWidth, winHeight) <- G.getWindowSize w
  let (winW, winH) = (rf winWidth, rf winHeight)
  -- let (w, h) = (rf width, rf height)
  --
  --                 + -> Shift to the Right in half window
  --                 ↓
  let upLeftX = winW / 2 + x0 * (winW / ndcWidth)
  let upLeftY = winH / 2 + y0 * (winH / ndcWidth)
  let downRightX = winW / 2 + x1 * (winW / ndcWidth)
  let downRightY = winH / 2 + y1 * (winH / ndcWidth)
  if upLeftX <= x && x <= downRightX && upLeftY <= y && y <= downRightY
    then return True
    else return False

-- |
--    KEY: getter for fovDegree_
--    NOTE: zoom in, zoom out
getFOVDegree :: IORef GlobalRef -> IO GLdouble
getFOVDegree ioGlobalRef = readIORef ioGlobalRef <&> fovDegree_

-- |
--    KEY: getter for str_
getStr :: IORef GlobalRef -> IO String
getStr ioGlobalRef = readIORef ioGlobalRef <&> str_

-- |
--    KEY: getter for xyzAxis_
getXYZAxis :: IORef GlobalRef -> IO XYZAxis
getXYZAxis ioGlobalRef = readIORef ioGlobalRef <&> xyzAxis_

-- |
--    KEY: getter for drawPts_
getDrawPts :: IORef GlobalRef -> IO [[Vertex3 GLfloat]]
getDrawPts ioGlobalRef = readIORef ioGlobalRef <&> drawPts_

getRandomPts :: IORef GlobalRef -> IO [Vertex3 GLfloat]
getRandomPts ioGlobalRef = readIORef ioGlobalRef <&> randomPts_

-- |
--    KEY:
getCursorPosf :: G.Window -> IO (GLfloat, GLfloat)
getCursorPosf w = G.getCursorPos w >>= \(x, y) -> return (rf x, rf y) :: IO (GLfloat, GLfloat)

-- |
--    KEY:
getMousePressed :: IORef GlobalRef -> IO (Bool, (GLfloat, GLfloat))
getMousePressed ioGlobalRef = readIORef ioGlobalRef <&> mousePressed_

-- |
--    KEY:
getCursor :: IORef GlobalRef -> IO (GLfloat, GLfloat)
getCursor ioGlobalRef = readIORef ioGlobalRef <&> cursor_

-- |
--    KEY:
getDrawRectX :: IORef GlobalRef -> IO (Vertex3 GLfloat, Vertex3 GLfloat)
getDrawRectX ioGlobalRef = readIORef ioGlobalRef <&> drawRectX_

getTranVecDrawRectX :: IORef GlobalRef -> IO (Vector3 GLdouble)
getTranVecDrawRectX ioGlobalRef = readIORef ioGlobalRef <&> tranDrawRectX_

-- |
--    KEY:
--
--    t0 pressed
--       (x0, y0)
--
--    t1 released
--       (x1, y1)
mouseCallbackX :: IORef GlobalRef -> G.MouseButtonCallback
mouseCallbackX globalRef window but butState mk = do
  case butState of
    G.MouseButtonState'Pressed -> do
      case but of
        v
          | v == G.MouseButton'1 -> do
            (fbw, fbh) <- G.getFramebufferSize window
            pos <- G.getCursorPos window >>= \(x, y) -> return (rf x, rf y) :: IO (GLfloat, GLfloat)
            ws <- G.getWindowSize window
            let str = PR.printf "cx=%.2f cy=%.2f wx=%d wy=%d bx=%d by=%d" (fst pos) (snd pos) (fst ws) (snd ws) fbw fbh :: String

            gRef <- readIORef globalRef
            -- ↑
            -- +---------------------------+
            --                             ↓
            -- writeIORef globalRef $ setStr gRef str
            modifyIORef globalRef (\x -> x {str_ = str})

            -- newGlobalRef <- readIORef globalRef >>= return . setCursor pos
            -- readIORef globalRef >>= return . setCursor pos >>= \x -> writeIORef globalRef $ setMousePressed (True, pos) x
            -- readIORef globalRef >>= return . setCursor pos >>= \x -> writeIORef globalRef $ setMousePressed (True, pos) x
            modifyIORef globalRef (\x -> x {cursor_ = pos})
            modifyIORef globalRef (\x -> x {mousePressed_ = (True, pos)})

            --  ↑
            --  +--------------------------------------------------+
            --                                                     ↓
            -- writeIORef globalRef $ setMousePressed (True, pos) newGlobalRef

            pp str
          | otherwise -> pp "No button pressed"
    G.MouseButtonState'Released -> do
      -- pos <- G.getCursorPos window >>= \(x, y) -> return $ (rf x, rf y) :: IO (GLfloat, GLfloat)
      let pos = (0.0, 0.0)
      -- readIORef globalRef >>= \x -> writeIORef globalRef $ setMousePressed (False, pos) x
      modifyIORef globalRef (\x -> x {mousePressed_ = (False, pos)})

      pp "Button Released"


-- |
--    === KEY: screen coordinates-system to graphic coordinates-system
--
--    @
--        (x, y) <- getCursorPosf w = G.getCursorPos w >>= \(x, y) -> return $ (rf x, rf y)
--        screen Coordinates-System
--
--        [winW = 1000, winH = 1000]
--        topLeft
--         ↓ (0,0)
--         + - - - ->  winW
--         |
--         |
--         |
--         v winH
--
--        Scale XY-axes to [0, 1.0]
--        (winW, winH) <- G.getWindowSize w >>= \(u, v) -> return (rf u, rf v)
--
--        (x/winW, y/winH)
--
--         topLeft
--         ↓ (0,0)
--         + - - - ->  1.0
--         |
--         |
--         |
--         v 1.0
--
--
--         Move (0,0) => (0, 0.5)
--         (x/winW, y/winH - 0.5)
--         topLeft
--         ↓
--         + -     ->  1.0
--         |
-- (0,0)   + - - - ->
--         |
--         v 1.0
--
--         Flip Y-Axis
--         (x/winW, (0.5 - y/winH))
--
--         ^
--         |
--         |
--         + - - - ->
--         |
--         |
--
--         ↑
--         bottomLeft
--
--         Move (0,0) to (0, 0.5), 0.5 on X-axis
--         (0.5, 0.5) => (0.0, 0.0)
--
--         (x/winW - 0.5,  0.5 - y/winH)
--
--                 ^
--                 |
--                 |
--                 | (0,0)
--          -------+------->
--                 |
--                 |
--                 |
--
--
--         Test 1, x = winW/2, y=winH/2 => (0, 0)       ✓
--         Test 2, x = winW,   y=winH   => (0.5, -0.5)  ✓
--         Test 3, x = (1/4)winW, y= (1/4) winH => (0.25 - 0.5, 0.5 - 0.25) = (-0.25, 0.25)  ✓
--
--    @
screenCSToGraphicCS :: G.Window -> (GLfloat, GLfloat) -> IO (Vertex3 GLfloat)
screenCSToGraphicCS ws (wpx, hpx) = do
  let ndcWidth = 4.0
  (winW, winH) <- G.getWindowSize ws >>= \(u, v) -> return (rf u, rf v)
  let cenx = ndcWidth / 2.0
  let ceny = ndcWidth / 2.0
  -- [0, ndcWidth] - (ndcWidth/2)
  -- x: [0, 2] - 1 => [-1, 1], move (0, 0) => (1, 0)
  -- y: [0, 2] - 1 => [-1, 1], flip Y-axis => -1*[-1, 1] => [1, -1]
  let x' = ndcWidth / winW
  let y' = ndcWidth / winH
  return $ Vertex3 (x' * wpx - cenx) (negate (y' * hpx - ceny)) 0.0

normDist :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> GLfloat
normDist (x0, y0) (x1, y1) = (x1 - x0) ^ 2 + (y1 - y0) ^ 2

drawConvexHull :: [Vertex3 GLfloat] -> IO ()
drawConvexHull pts = do
  let n = len pts
  let ls = convexHull n pts
  mapM_ drawDot pts
  mapM_ (drawSegmentWithEndPt red) ls

-- listTupeToList::[(Vertex3 GLfloat, Vertex3 GLfloat)] -> [[Vertex3 GLfloat]]
-- listTupeToList cx = map (\x -> ((fromJust . head) cx, (fromJust . last) cx) ) cx

seg :: Vertex3 GLfloat -> Vertex3 GLfloat -> [Vertex3 GLfloat]
seg v0 v1 = cmpVex v0 v1 ? [v0, v1] $ [v1, v0]

findAllChildren :: Vertex3 GLfloat -> [(Vertex3 GLfloat, Vertex3 GLfloat)] -> [Vertex3 GLfloat]
findAllChildren v cx = unique $ map fromJust $ filter (/= Nothing) $ map (\(v0, v1) -> v == v0 ? Just v1 $ (v == v1 ? Just v0 $ Nothing)) cx

moveNext :: Vertex3 GLfloat -> IO (Vertex3 GLfloat)
moveNext (Vertex3 x y z) = do
  n <- randomInt 1 4
  let b = 0.8 :: GLfloat
  case n of
    1 -> if x < b then let v = Vertex3 (x + 0.1) y z in return v else moveNext (Vertex3 x y z)
    2 -> if x > - b then let v = Vertex3 (x - 0.1) y z in return v else moveNext (Vertex3 x y z)
    3 -> if y < b then let v = Vertex3 x (y + 0.1) z in return v else moveNext (Vertex3 x y z)
    4 -> if y > - b then let v = Vertex3 x (y - 0.1) z in return v else moveNext (Vertex3 x y z)
    _ -> error "ERROR randomInt"

{--
             1
        -1   +(0 0)    1
            -1

--}
randomVexList :: Vertex3 GLfloat -> [Int] -> [Vertex3 GLfloat]
randomVexList _ [] = []
randomVexList v@(Vertex3 x y z) (c : cx) = case c of
  1 -> x < 1.0 ? (let u = Vertex3 (x + 0.1) y z in u : randomVexList u cx) $ randomVexList v cx
  2 -> x > -1.0 ? (let u = Vertex3 (x - 0.1) y z in u : randomVexList u cx) $ randomVexList v cx
  3 -> y < 1.0 ? (let u = Vertex3 x (y + 0.1) z in u : randomVexList u cx) $ randomVexList v cx
  4 -> y > -1.0 ? (let u = Vertex3 x (y - 0.1) z in u : randomVexList u cx) $ randomVexList v cx
  _ -> error "ERROR randomInt"

randomVexListInt :: (Int, Int) -> [Int] -> [(Int, Int)]
randomVexListInt _ [] = []
randomVexListInt v@(x, y) (c : cx) = case c of
  1 -> x < mx ? (let u = (x + 1, y) in u : randomVexListInt u cx) $ randomVexListInt v cx
  2 -> x > - mx ? (let u = (x - 1, y) in u : randomVexListInt u cx) $ randomVexListInt v cx
  3 -> y < my ? (let u = (x, y + 1) in u : randomVexListInt u cx) $ randomVexListInt v cx
  4 -> y > - my ? (let u = (x, y - 1) in u : randomVexListInt u cx) $ randomVexListInt v cx
  _ -> error "ERROR randomInt"
  where
    mx = 10
    my = 10

-- |
--                (x0, y0)
--
--                    +--
--                    |
--                         |
--                        -+ (-x0, -y0)
--
--
--                    boundary    init pt     random list   next move list
--                       |            |            |             |
randomVexListX :: (Int, Int) -> (Int, Int) -> [Int] -> [(Int, Int)]
randomVexListX _ _ [] = []
randomVexListX ix@(x0, y0) v@(x, y) (c : cx) = case c of
  1 -> x > x0 ? (let u = (x - 1, y) in u : randomVexListX ix u cx) $ randomVexListX ix v cx
  2 -> x < - x0 ? (let u = (x + 1, y) in u : randomVexListX ix u cx) $ randomVexListX ix v cx
  3 -> y < y0 ? (let u = (x, y + 1) in u : randomVexListX ix u cx) $ randomVexListX ix v cx
  4 -> y > - y0 ? (let u = (x, y - 1) in u : randomVexListX ix u cx) $ randomVexListX ix v cx
  _ -> error "ERROR randomInt"

getShape :: [[(Int, Int)]] -> [[Int]] -> [(Int, Int)]
getShape centerBrick bk = map fst $ join $ (map . filter) (\(_, n) -> n > 0) $ (zipWith . zipWith) (,) centerBrick bk

getShape3 :: [[[(Int, Int, Int)]]] -> [[[Int]]] -> [(Int, Int, Int)]
getShape3 centerBrick bk = map fst $ join . join $ (map . map . filter) (\(_, n) -> n > 0) $ (zipWith . zipWith . zipWith) (,) centerBrick bk

innerBrick :: (Int, Int) -> [[(Int, Int)]] -> [[Int]] -> [(Int, Int)]
innerBrick (moveX, moveY) centerBrick bk1 = currBr
  where
    f x = map fst $ join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (,) centerBrick x
    r0 = bk1
    ((x0, x1), (y0, y1)) =
      let r1 = rotateN 1 r0
          r2 = rotateN 1 r1
          r3 = rotateN 1 r2
          r4 = rotateN 1 r3
          a1 = f r0
          a2 = f r1
          a3 = f r2
          a4 = f r3
          minmax x = (minimum x, maximum x)
          ls = a1 ++ a2 ++ a3 ++ a4
          tx = minmax $ map fst ls
          ty = minmax $ map snd ls
       in (tx, ty)
    cb0 = (map . filter) (\(x, _) -> x0 <= x && x <= x1) centerBrick
    cb1 = (map . filter) (\(_, y) -> y0 <= y && y <= y1) cb0
    currBr = join $ (map . map) (\(x, y) -> (x + moveX, y + moveY)) cb1

-- |
--
--               minY_
--                |
--        minX_ - +  - -> maxX_
--                |
--               maxY_
--
--        |<-    xCount_    ->|
--                20
initRectGrid :: RectGrid
initRectGrid =
  RectGrid
    { minX_ = -1.0,
      maxX_ = 1.0,
      minY_ = -1.0,
      maxY_ = 1.0,
      xCount_ = 20,
      yCount_ = 20,
      xEdge_ = 0.1,
      yEdge_ = 0.01,
      rotStep = 20
    }

drawRectGridX :: RectGrid -> IO ()
drawRectGridX r = do
  let xc = [0 .. xCount_ r - 1]
  let yc = [0 .. yCount_ r - 1]
  mapM_ (\ny -> mapM_ (\nx -> drawBlock (nx, ny) r) xc) yc

{--
let sx = [0.. (xCount_ r) - 1] :: [Int]
let sy = [0.. (yCount_ r) - 1] :: [Int]
let wx = xEdge_ r; wy = yEdge_ r
let v (x, y, z) = Vertex3 x y z
-- top left corner
let x0 = minX_ r; y0 = maxY_ r
-- bottom right corner
let x1 = maxY_ r; y1 = minY_ r
let vx0 = v(x0, y0, 0)
let vx1 = v(x1, y1, 0)
let bWidth = (x1 - x0) / (fi $ xCount_ r)
let bHeight = (y0 - y1) / (fi $ yCount_ r)
-- drawRect (vx0, vx1)
mapM_(\ny -> mapM_(\nx -> let ex0 = x0 + (fi $ nx) * bWidth  + wx
                              ey0 = y0 - (fi $ ny) * bHeight - wy
                              ex1 = x0 + (fi $ nx + 1) * bWidth  - wx
                              ey1 = y0 - (fi $ ny + 1) * bHeight + wy
                          in  drawRectColor green (v(ex0, ey0, 0), v(ex1, ey1, 0))
                  ) sx ) sy
--}

drawBlock :: (Int, Int) -> RectGrid -> IO ()
drawBlock (nx, ny) r = do
  let wx = xEdge_ r; wy = yEdge_ r
  let v (x, y, z) = Vertex3 x y z
  -- top left corner
  let x0 = minX_ r; y0 = maxY_ r
  -- bottom right corner
  let x1 = maxY_ r; y1 = minY_ r
  let vx0 = v (x0, y0, 0)
  let vx1 = v (x1, y1, 0)
  let bWidth = (x1 - x0) / fi (xCount_ r)
  let bHeight = (y0 - y1) / fi (yCount_ r)
  -- drawRect (vx0, vx1)
  let ex0 = x0 + fi nx * bWidth + wx
      ey0 = y0 - fi ny * bHeight - wy
      ex1 = x0 + (fi nx + 1) * bWidth - wx
      ey1 = y0 - (fi ny + 1) * bHeight + wy
   in drawRectColor green (v (ex0, ey0, 0), v (ex1, ey1, 0))


-- |
--
--   The cube is from following URL
--   http://localhost/html/indexUnderstandOpenGL.html
drawCube :: IO ()
drawCube = do
  
  -- drawPrimitive' TriangleStrip green [v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12]
  preservingMatrix $ do
    renderPrimitive TriangleStrip $
      mapM_ (\(c, v) -> do
               color c
               vertex v
             ) ls
  {--
  preservingMatrix $ do
    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color green
            vertex v
        )
        ls_top
    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color magenta
            vertex v
        )
        ls_bot
    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color cyan
            vertex v
        )
        ls_front

    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color blue
            vertex v
        )
        ls_back

    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color yellow
            vertex v
        )
        ls_left

    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color gray
            vertex v
        )
        ls_right
  --}
  where
    f (Color3 a b c) = Color3 (a * 0.5) (b * 0.5) (c * 0.5)
    ls =
      [ (green, v0),
        (red, v1),
        (cyan, v2),
        (yellow, v3),
        (blue, v4),
        (white, v5),
        (gray, v6),
        (yellow, v7),
        (green, v8),
        (cyan, v9),
        (red, v10),
        (cyan, v11),
        (magenta, v12)
      ]
    a = 0.4 :: GLfloat

    v0 = Vertex3 0 0 0
    v1 = Vertex3 a 0 0
    v2 = Vertex3 0 a 0
    v3 = Vertex3 a a 0

    v4 = Vertex3 0 a (- a)
    v5 = Vertex3 a a (- a)
    v6 = Vertex3 0 0 (- a)
    v7 = Vertex3 a 0 (- a)

    v8 = Vertex3 a a 0
    v9 = Vertex3 a 0 0
    v10 = Vertex3 0 0 (- a)
    v11 = Vertex3 0 0 0

    v12 = Vertex3 0 a (- a)
    v13 = Vertex3 0 a 0

    lt =
      [ (green, x0),
        (red, x1),
        (cyan, x2),
        (yellow, x3),
        (blue, x4),
        (white, x5),
        (gray, x6),
        (yellow, x7)
      ]
    lt' =
      [ (green, x2),
        (red, x4),
        (cyan, x0),
        (yellow, x6),
        (blue, x1),
        (white, x7),
        (gray, x3),
        (yellow, x5)
      ]
    x0 = Vertex3 0 0 0
    x1 = Vertex3 a 0 0
    x2 = Vertex3 0 0 (- a)
    x3 = Vertex3 a 0 (- a)

    x4 = Vertex3 0 (- a) (- a)
    x5 = Vertex3 a (- a) (- a)
    x6 = Vertex3 0 (- a) 0
    x7 = Vertex3 a (- a) 0

    b = 0.3 :: GLfloat
    p0 = Vertex3 b b (- b)
    p1 = Vertex3 (- b) b (- b)
    p2 = Vertex3 (- b) b b
    p3 = Vertex3 b b b

    q0 = Vertex3 b (- b) (- b)
    q1 = Vertex3 (- b) (- b) (- b)
    q2 = Vertex3 (- b) (- b) b
    q3 = Vertex3 b (- b) b

    ls_top = [p0, p1, p2, p3]
    ls_bot = [q0, q1, q2, q3]
    ls_front = [p3, p2, q2, q3]
    ls_back = [p0, p1, q1, q0]
    ls_left = [p1, p2, q2, q1]
    ls_right = [p0, p3, q3, q0]

{--
               p1       p0

          p2        p3

               q1     q0

         q2       q3

--}

-- |
--
--   The cube is from following URL
--   http://localhost/html/indexUnderstandOpenGL.html
--
--   @
--   GLfloat vertices[]  = {
--        .5f, .5f, .5f,  -.5f, .5f, .5f,  -.5f,-.5f, .5f,  .5f,-.5f, .5f, // v0,v1,v2,v3 (front)
--        .5f, .5f, .5f,   .5f,-.5f, .5f,   .5f,-.5f,-.5f,  .5f, .5f,-.5f, // v0,v3,v4,v5 (right)
--        .5f, .5f, .5f,   .5f, .5f,-.5f,  -.5f, .5f,-.5f, -.5f, .5f, .5f, // v0,v5,v6,v1 (top)
--       -.5f, .5f, .5f,  -.5f, .5f,-.5f,  -.5f,-.5f,-.5f, -.5f,-.5f, .5f, // v1,v6,v7,v2 (left)
--       -.5f,-.5f,-.5f,   .5f,-.5f,-.5f,   .5f,-.5f, .5f, -.5f,-.5f, .5f, // v7,v4,v3,v2 (bottom)
--        .5f,-.5f,-.5f,  -.5f,-.5f,-.5f,  -.5f, .5f,-.5f,  .5f, .5f,-.5f  // v4,v7,v6,v5 (back)
--   };
--   @
drawCube2 :: IO ()
drawCube2 = do
  renderPrimitive Triangles $
    mapM_
      ( \(c, v) -> do
          color c
          vertex v
      )
      ls
  where
    a = 0.1 :: GLfloat
    v00 = Vertex3 a a a
    v01 = Vertex3 (- a) a a
    v02 = Vertex3 (- a) (- a) a
    v03 = Vertex3 a (- a) a

    v10 = Vertex3 a a a
    v11 = Vertex3 a (- a) a
    v12 = Vertex3 a (- a) (- a)
    v13 = Vertex3 a a (- a)

    v20 = Vertex3 a a a
    v21 = Vertex3 a a (- a)
    v22 = Vertex3 (- a) a (- a)
    v23 = Vertex3 (- a) a a

    v30 = Vertex3 (- a) a a
    v31 = Vertex3 (- a) a (- a)
    v32 = Vertex3 (- a) (- a) (- a)
    v33 = Vertex3 (- a) (- a) a

    v40 = Vertex3 (- a) (- a) (- a)
    v41 = Vertex3 a (- a) (- a)
    v42 = Vertex3 a (- a) a
    v43 = Vertex3 (- a) (- a) a

    v50 = Vertex3 a (- a) (- a)
    v51 = Vertex3 (- a) (- a) (- a)
    v52 = Vertex3 (- a) a (- a)
    v53 = Vertex3 a a (- a)
    ls =
      [ (green, v00),
        (green, v01),
        (green, v02),
        (green, v03),
        (cyan, v10),
        (cyan, v11),
        (cyan, v12),
        (cyan, v13),
        (yellow, v20),
        (yellow, v21),
        (yellow, v22),
        (yellow, v23),
        (magenta, v30),
        (magenta, v31),
        (magenta, v32),
        (magenta, v33),
        (blue, v40),
        (blue, v41),
        (blue, v42),
        (blue, v43),
        (gray, v50),
        (gray, v51),
        (gray, v52),
        (gray, v53)
      ]

-- |
--   Center Brick,
--
--        (0.1 0.1)
--    + - +
--    |   |
--    + - +
-- (0,0)
centerBlock00 :: (Int, Int) -> RectGrid -> Color3 GLdouble -> IO ()
centerBlock00 (nx, ny) r color = do
  preservingMatrix $ do
    let wx = xEdge_ r; wy = yEdge_ r
    let v (x, y, z) = Vertex3 x y z
    -- top left corner
    let x0 = minX_ r; y0 = maxY_ r
    -- bottom right corner
    let x1 = maxY_ r; y1 = minY_ r
    let vx0 = v (x0, y0, 0)
    let vx1 = v (x1, y1, 0)
    let width = (x1 - x0) / fi (xCount_ r)
    let height = (y0 - y1) / fi (yCount_ r)
    let vo = Vertex3 0 0 0
    let vf = Vertex3 (fi nx * width) (fi ny * height) 0
    let vm = vf -: vo
    translate vm
    preservingMatrix $ do
      let x = width / 2
      let y = height / 2
      let vf = Vertex3 x y 0 :: (Vertex3 GLfloat)
      let ov = Vertex3 0 0 0 :: (Vertex3 GLfloat)
      let vv = vf -: ov
      translate vv
      drawRectFill2dX color (width, height)


showCurrBoardArr :: IOArray (Int, Int, Int) BlockAttr -> IO ()
showCurrBoardArr arr = do
  ls <- getAssocs arr
  preservingMatrix $ do
    mapM_
      ( \((x, y, _), b) ->
          preservingMatrix $ do
            when (isFilled_ b) $ do
              centerBlock00 (x, y) initRectGrid (color_ b)
      )
      ls


initBlockAttr :: BlockAttr
initBlockAttr =
  BlockAttr
    { isFilled_ = False,
      typeId_ = -1,
      tetrisNum_ = -1,
      color_ = red
    }

data AnimaState = AnimaState
  { animaTime_ :: Int,
    animaIndex_ :: Int,
    animaInterval_ :: Int,
    animaSlot_ :: Int
  }
  deriving (Show, Eq)

initAnimaState :: IO (IOArray Int AnimaState)
initAnimaState = do
  currTime <- timeNowMilli <&> fi
  let an = AnimaState {animaTime_ = currTime, animaIndex_ = 0, animaInterval_ = 1000, animaSlot_ = 0}
  -- let anx = AnimaState {animaTime_ = currTime, animaIndex_ = 0, animaInterval_ = 4000, animaSlot_ = 0}
  -- DAO.newArray (0, 5) an
  let ls = replicate 100 an
  DAO.newListArray (0, len ls - 1) ls

readAnimaState :: IOArray Int AnimaState -> Int -> Int -> IO (Bool, Int, AnimaState)
readAnimaState arr ix interval = do
  currTime <- timeNowMilli <&> fi
  an <- DAO.readArray arr ix
  DAO.writeArray arr ix an {animaInterval_ = interval}
  oldTime <- DAO.readArray arr ix <&> animaTime_
  interval <- DAO.readArray arr ix <&> animaInterval_
  oldIndex <- DAO.readArray arr ix <&> animaIndex_
  let newIndex = oldIndex + 1
  let isNext = currTime - oldTime >= interval
  if isNext
    then do
      return (isNext, newIndex, an {animaTime_ = currTime, animaIndex_ = newIndex, animaSlot_ = ix})
    else do
      return (isNext, oldIndex, an {animaSlot_ = ix})

writeAnimaState :: IOArray Int AnimaState -> AnimaState -> IO ()
writeAnimaState arr state = do
  let ix = animaSlot_ state
  DAO.writeArray arr ix state


colorChange :: GLdouble -> Color3 GLdouble -> Color3 GLdouble
colorChange x (Color3 a b c) = Color3 (a*x) (b*x) (c*x)
  
{-|
   KEY: draw cylinder

   @
   let r = 0.5             -- radius of circle
   let l = 0.4             -- length of cylinder
   let leftClose = True    -- close the left end
   let rightClose = False  -- close the right end
   cylinder r l (leftClose, rightClose)
   @
-}
cylinder :: GLfloat -> GLfloat -> (Bool, Bool) -> [Color3 GLdouble]-> IO()
cylinder r leng (left, right) cl = do
  let d = 2.0*pi/10.0
  let lw = [0, 0 + d .. 2*pi]
  let lv = map (\x -> Vertex3 (r * cos x) leng (r * sin x)) lw
  let ls = map (\x -> Vertex3 (r * cos x) 0    (r * sin x)) lw
  let lt = join $ zipWith (\x y -> [x, y]) lv ls
  -- let lt' = zip lt $ join $ repeat [green, yellow, blue, cyan, gray, white]
  let lt' = zip lt $ (join . repeat) cl
  renderPrimitive TriangleStrip $ mapM_(\(v, c) -> do
      color c
      vertex v) lt'
  when left $ do
    renderPrimitive TriangleFan $ mapM_ (\v -> do
        color green
        vertex v) $ Vertex3 (r * cos 0) leng (r * sin 0) : lv
  when right $ do
    renderPrimitive TriangleFan $ mapM_ (\v -> do
        color yellow
        vertex v) $ Vertex3 (r * cos 0) 0 (r * sin 0) : ls

cylinderXX :: GLfloat -> GLfloat -> (Bool, Bool) -> [Color3 GLdouble]-> IO()
cylinderXX r leng (left, right) cl = do
  let d = 2.0*pi/10.0
  let lw = [0, 0 + d .. 2*pi]
  let lv = map (\x -> Vertex3 leng  (r * cos x) (r * sin x)) lw
  let ls = map (\x -> Vertex3 0     (r * cos x) (r * sin x)) lw
  let lt = join $ zipWith (\x y -> [x, y]) lv ls
  let lt' = zip lt $ (join . repeat) cl
  renderPrimitive TriangleStrip $ mapM_(\(v, c) -> do
      color c
      vertex v) lt'
  when left $ do
    renderPrimitive TriangleFan $ mapM_ (\v -> do
        color green
        vertex v) $ Vertex3 leng (r * cos 0) (r * sin 0) : lv
  when right $ do
    renderPrimitive TriangleFan $ mapM_ (\v -> do
        color yellow
        vertex v) $ Vertex3 0 (r * cos 0) (r * sin 0) : ls

skewMatrix3 :: (Num a) => (a, a, a) -> [[a]]
skewMatrix3 (x, y, z) = [
                         [0,  -z, y],
                         [z,  0, -x],
                         [-y, x,  0]
                        ]
  
{-|

  KEY: rotate around arbitrary axis in 3d

   * Rodrigues's rotation formula

   <https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula Rodrigues_Rotation_Formula>


  @
   rotVec :: Vector3 GLdouble -> Vector3 GLdouble -> GLfloat -> Vector3 GLdouble

   -- k is unit vector rotation axis
   -- v is rotate around k in radian
   rotVec k v radius
  @
-}
rotVec :: (Floating a) => Vector3 a -> Vector3 a -> a -> Vector3 a
rotVec k v theta = ax + bx + cx
  where
    f (Vector3 x y z) = (x, y, z)
    toList (Vector3 x y z) = [x, y, z]
    vec x = Vector3 (head x) ((head . tail) x) (last x)
    skew = skewMatrix3 $ f k
    ax = v
    bx = let a = sin theta
             b = (vec . join) $ skew `multiVec` toList v
         in  a *: b
    cx = let a  = 1 - cos theta
             b = (vec $ join $  skew `multiVec` join (skew `multiVec` toList v))
         in a *: b

{-|

  KEY: rotate around arbitrary axis in 3d

   * Rodrigues's rotation formula

   <https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula Rodrigues_Rotation_Formula>

  @
   rotMat :: Vector3 GLdouble -> GLdouble -> [[GLdouble]]

   -- k is unit vector rotation axis
   -- φ is rotate around k in radius
   rotMat k φ radius
  @
-}
rotMat :: (Floating a) => Vector3 a -> a -> [[a]]
rotMat k θ = id3 + ax + bx
  where
    id3 = out (\a b -> a == b ? 1 $ 0) [1..3] [1..3]
    f (Vector3 x y z) = (x, y, z)
    m = skewMatrix3 $ f k
    ax = sin θ ×× m
    bx = (1 - cos θ) ×× (m `multiMat` m)
  
-- let m = (map . map) rf $ padMat3To4 $ rotMat k (rf $ del * fi i)  
rotMat4Tup :: (Floating a) => Vector3 a -> a -> ([[a]], [a])
rotMat4Tup k θ = (m4, join m4)
  where
    id3 = out (\a b -> a == b ? 1 $ 0) [1..3] [1..3]
    f (Vector3 x y z) = (x, y, z)
    m = skewMatrix3 $ f k
    ax = sin θ ×× m
    bx = (1 - cos θ) ×× (m `multiMat` m)
    m3 = id3 + ax + bx
    m4 = padMat3To4 m3

  
drawAxis :: Vector3 GLfloat -> [Color3 GLdouble] -> IO()
drawAxis v cl = do
  preservingMatrix $ do
    let v0 = Vector3 1 0 0 :: (Vector3 GLfloat)
    let v1 = v
    let m = padMat3To4 $ rotToVecMat v0 v1
    multiModelviewMat $ join m
    cylinderArrow 1.0 cl
  
{-|
  === KEY: draw arrow from two 'Vertex3' 'GLfloat'
 -}
drawArrow :: (Vertex3 GLfloat, Vertex3 GLfloat) -> IO()
drawArrow (p0, p1) = do 
  let v = p0 -: p1
  -- let ang = let r = if ve_2 v < 0 then  else 180/pi in r * angle2Vector (Vector3 1 0 0) v
  let ang = let rad =  angle2Vector (Vector3 1 0 0) v 
            in if ve_2 v < 0 then 180/pi * rad - pi else 180/pi * rad
  preservingMatrix $ do
    translate $ vec_ p0  
    rotate ang (Vector3 0 0 1 :: Vector3 GLfloat)
    cylinderArrowX l cl
    where  
      cl = [green, blue, yellow]
      l = nr $ p0 -: p1

drawArrowX :: (Vertex3 GLfloat, Vertex3 GLfloat) -> IO()
drawArrowX (p0, p1) = do 
  let ang = let rad =  angle2Vector (Vector3 1 0 0) v 
            in if ve_2 v < 0 then -(180/pi * rad) else 180/pi * rad
  preservingMatrix $ do
    translate $ vec_ p0  
    rotate ang (Vector3 0 0 1 :: Vector3 GLfloat)
    cylinderArrowXX l cl
    where  
      cl = [green, blue, yellow]
      v = p0 -: p1
      l = nr v 

drawArrowXX :: (Vertex3 GLfloat, Vertex3 GLfloat) -> IO()
drawArrowXX (p0, p1) = do 
  let ang = let rad =  angle2Vector (Vector3 1 0 0) v 
            in if ve_2 v < 0 then -(180/pi * rad) else 180/pi * rad
  preservingMatrix $ do
    translate $ vec_ p0  
    rotate ang (Vector3 0 0 1 :: Vector3 GLfloat)
    cylinderArrowXXX l cl
    where  
      cl = [green, blue, yellow]
      v = p0 -: p1
      l = nr v 

drawArrow3dCen ::Vector3 GLfloat -> [Color3 GLdouble] -> IO()
drawArrow3dCen v cl = do
  preservingMatrix $ do
    let v0 = Vector3 1 0 0
    let v1 = v
    let m = padMat3To4 $ rotToVecMat v0 v 
    multiModelviewMat $ join m
    cylinderArrow (nr v) cl

drawArrow3d ::(Vertex3 GLfloat, Vertex3 GLfloat) -> [Color3 GLdouble] -> IO()
drawArrow3d (p0, p1) cl = do
  preservingMatrix $ do
    let v = p0 ➞ p1  -- vector from p0 to p1
    let v0 = Vector3 1 0 0
    let m = padMat3To4 $ rotToVecMat v0 v 
    translate (vec_ p0)
    multiModelviewMat $ join m
    cylinderArrow (nr v) cl
  where
    (➞) :: (Floating a) => Vertex3 a -> Vertex3 a -> Vector3 a
    (➞) (Vertex3 x y z) (Vertex3 x' y' z') = Vector3 (x' - x) (y' - y) (z' - z) 
    --HEAVY TRIANGLE-HEADED RIGHTWARDS ARROW
    --Unicode: U+279E, UTF-8: E2 9E 9E

drawArrowColor :: (Vertex3 GLfloat, Vertex3 GLfloat) -> [Color3 GLdouble] -> IO()
drawArrowColor (p0, p1) cr = do 
  let v = p0 -: p1
  let ang = let r = 180/pi in r * angle2Vector (Vector3 1 0 0) v
  preservingMatrix $ do
    translate $ vec_ p0 
    rotate ang (Vector3 0 0 1 :: Vector3 GLfloat)
    cylinderArrowX l cr 
    where  
      l = nr $ p0 -: p1


{-| 
  
   === KEY: rotate vector to other vector

   * rotate v₀ to v₁ in angle θ radian around v₀ ⊗ v₁

   @
   let v0 = Vector3 1 0 0
   let v1 = Vector3 0 0 (-1)
   let θ  = pi/2
   rotToVecMat v₀ v₁ θ
   @
-}
rotToVecMat :: (Floating a, Eq a) => Vector3 a -> Vector3 a  -> [[a]]
rotToVecMat v₀ v₁ = rotMat vᵤ θ
  where
    vᵤ = case v₀ ⊗ v₁ of
              Nothing -> uv v₀
              -- Nothing -> error "ERROR: two vectors can not be parallel, ERROR123"
              Just v -> uv v
    θ = angle2Vector v₀ v₁

{-|

  @
  -- 3x3  to 4x4 matrix

    1 2 3
    4 5 6
    7 8 9

    1 2 3 0
    4 5 6 0
    7 8 9 0
    0 0 0 1
  @
-}
padMat3To4 :: (Num a) => [[a]] -> [[a]]
padMat3To4 m = tran mx
  where
    m' = m ++ [[0, 0, 0]]
    mt = tran m'
    mx = mt ++ [[0, 0, 0, 1]]
    
{-|
  
  @
  (Vertex3 0 0 0) ->  (Vertex3 1 0 0)

  cylinder + cone
  -- leng is the total length of cylinder and cone
  cylinderArrow leng [yellow, gray]
  @
-}
cylinderArrow :: GLfloat -> [Color3 GLdouble] -> IO()
cylinderArrow leng cl = do
  let cyRatio = 0.9 :: GLfloat
  let cyLen =   rf $leng * cyRatio
  let cyRadius = cyLen * 0.01
  let coneRadius = cyRadius * 2.0
  let coneHeigh = rf $ leng * (1.0 - cyRatio)
  rotate (-90) (Vector3 0 0 1 ::Vector3 GLdouble)
  preservingMatrix $ do
    cylinder cyRadius cyLen (True, True)  cl
    translate (Vector3 0 (rf cyLen) 0 :: Vector3 GLdouble)
    cone coneRadius coneHeigh 8 cl

cylinderArrowX :: GLfloat -> [Color3 GLdouble] -> IO()
cylinderArrowX leng cl = do
  let cyRatio = leng <= 0.1 ? 0.8 $ 0.9
  let cyLen =   rf $leng * cyRatio
  let cyRadius = leng <= 0.1 ? 0.002 $ cyLen * 0.01 
  let coneRadius = cyRadius * 2
  let coneHeigh = rf $ leng * (1.0 - cyRatio) 
  logFileGEx False "" [
                        "leng=" ++ show leng 
                       ,"cyRatio=" ++ show cyRatio
                       ,"cyLen=" ++ show cyLen
                       ,"cyRadius=" ++ show cyRadius
                       ,"coneRadius=" ++ show coneRadius
                       ,"coneHeigh=" ++ show coneHeigh
                       ]
  preservingMatrix $ do
    rotate (-90) (Vector3 0 0 1 :: Vector3 GLdouble)
    cylinder cyRadius cyLen (True, True)  cl
    translate (Vector3 0 (rf cyLen) 0 :: Vector3 GLdouble)
    cone coneRadius coneHeigh 8 [red] 

cylinderArrowXX :: GLfloat -> [Color3 GLdouble] -> IO()
cylinderArrowXX leng cl = do
  let cyRatio = leng <= 0.1 ? 0.8 $ 0.9
  let cyLen =   rf $leng * cyRatio
  let cyRadius = leng <= 0.1 ? 0.002 $ cyLen * 0.01 
  let coneRadius = cyRadius * 2
  let coneHeigh = rf $ leng * (1.0 - cyRatio) 
  logFileGEx False "" [
                        "leng=" ++ show leng 
                       ,"cyRatio=" ++ show cyRatio
                       ,"cyLen=" ++ show cyLen
                       ,"cyRadius=" ++ show cyRadius
                       ,"coneRadius=" ++ show coneRadius
                       ,"coneHeigh=" ++ show coneHeigh
                       ]
  preservingMatrix $ do
    -- rotate (-90) (Vector3 0 0 1 :: Vector3 GLdouble)
    cylinderXX cyRadius cyLen (True, True) cl 
    translate (Vector3 (rf cyLen) 0 0 :: Vector3 GLdouble)
    coneX coneRadius coneHeigh 8 [red] 

cylinderArrowXXX :: GLfloat -> [Color3 GLdouble] -> IO()
cylinderArrowXXX leng cl = do
  let cyRatio = 0.9 
  let cyLen =   rf $leng * cyRatio
  let cyRadius = 0.003
  let coneRadius = cyRadius * 2
  let coneHeigh = rf $ leng * (1.0 - cyRatio) 
  logFileGEx False "" [
                        "leng=" ++ show leng 
                       ,"cyRatio=" ++ show cyRatio
                       ,"cyLen=" ++ show cyLen
                       ,"cyRadius=" ++ show cyRadius
                       ,"coneRadius=" ++ show coneRadius
                       ,"coneHeigh=" ++ show coneHeigh
                       ]
  preservingMatrix $ do
    -- rotate (-90) (Vector3 0 0 1 :: Vector3 GLdouble)
    cylinderXX cyRadius cyLen (True, True) cl 
    translate (Vector3 (rf cyLen) 0 0 :: Vector3 GLdouble)
    coneX coneRadius coneHeigh 8 [red] 

coord :: IO()
coord = do
  preservingMatrix $ do
    let r = 0.02  -- radius of cone
    let clen = 0.95 :: GLdouble  -- length of cylinder
    let lo = rf $ 1.0 - clen     -- length of cone
    let nStep = 8                -- n-polygon, etc approximate circle
    
    -- +X-axis
    preservingMatrix $ do
      rotate (-90) (Vector3 0 0 1 :: Vector3 GLdouble)
      cylinder (r * 0.5) (rf $ clen - 0.01) (True, True) [red, colorChange 0.5 red]
    preservingMatrix $ do
      translate (Vector3 clen 0 0 :: Vector3 GLdouble)
      rotate (-90) (Vector3 0 0 1 :: Vector3 GLdouble)
      cone r lo nStep [red]
  
    -- +Y-axis
    preservingMatrix $ do
      cylinder (r * 0.5) (rf $ clen - 0.01) (True, True) [green, colorChange 0.5 green]
    preservingMatrix $ do
      translate (Vector3 0 clen 0 :: Vector3 GLdouble)
      cone r lo nStep [green]
  
    -- +Z-axis
    preservingMatrix $ do
      -- deg <- readAndParse "/tmp/kee.x" :: IO GLdouble
      let deg = 30
      rotate 90 (Vector3 1 0 0 :: Vector3 GLdouble)
      cylinder (r * 0.5) (rf $ clen - 0.01) (True, True) [blue, colorChange 0.5 blue]
    
    preservingMatrix $ do
      translate (Vector3 0 0 clen :: Vector3 GLdouble)
      rotate 90 (Vector3 1 0 0 :: Vector3 GLdouble)
      cone r lo nStep [blue]
    
{-|

   KEY: cone alond +y-axis

   @
   let r = 0.05 -- radius of circle
   let l = 0.5  -- length of cylinder
   let n = 8    -- n steps
   cone r l n
   @
-}
cone :: GLfloat -> GLfloat -> Int -> [Color3 GLdouble] -> IO()
cone r leng n cl = do
  let d = 2.0*pi/fi n
  -- let lc = [green, yellow, blue, cyan, gray, white]
  let ld = [yellow, green, white, blue, gray, cyan]
  let lw = [0, 0 + d .. 2.0*pi]
  let lv = map (\x -> Vertex3 (r * cos x) leng (r * sin x)) lw
  let ls = map (\x -> Vertex3 (r * cos x) 0    (r * sin x)) lw
  let lp = zip ls $ join $ repeat cl
  let lt = join $ zipWith (\x y -> [x, y]) lv ls
  -- (x, y, z) <- readAndParse "/tmp/tu.x" :: IO(GLfloat, GLfloat, GLfloat)
  renderPrimitive TriangleFan $ mapM_ (\(v, c) -> do
      color c
      vertex v) $ (Vertex3 0 leng 0 :: Vertex3 GLfloat, white) : lp
  renderPrimitive TriangleFan $ mapM_ (\(v, c) -> do
      color c
      vertex v) $ (Vertex3 0 0 0 :: Vertex3 GLfloat, white) : lp

{-|

   KEY: cone alone +x-axis

   @
   let r = 0.05 -- radius of circle
   let l = 0.5  -- length of cylinder
   let n = 8    -- n steps
   coneX r l n
   @
-}
coneX :: GLfloat -> GLfloat -> Int -> [Color3 GLdouble] -> IO()
coneX r leng n cl = do
  let d = 2.0*pi/fi n
  -- let lc = [green, yellow, blue, cyan, gray, white]
  let ld = [yellow, green, white, blue, gray, cyan]
  let lw = [0, 0 + d .. 2.0*pi]
  let lv = map (\x -> Vertex3 leng (r * cos x) (r * sin x)) lw
  let ls = map (\x -> Vertex3 0    (r * cos x) (r * sin x)) lw
  let lp = zip ls $ join $ repeat cl
  let lt = join $ zipWith (\x y -> [x, y]) lv ls
  -- (x, y, z) <- readAndParse "/tmp/tu.x" :: IO(GLfloat, GLfloat, GLfloat)
  renderPrimitive TriangleFan $ mapM_ (\(v, c) -> do
      color c
      vertex v) $ (Vertex3 leng 0 0 :: Vertex3 GLfloat, white) : lp
  renderPrimitive TriangleFan $ mapM_ (\(v, c) -> do
      color c
      vertex v) $ (Vertex3 0 0 0 :: Vertex3 GLfloat, white) : lp


vecToList3 :: Vector3 a -> [a]
vecToList3 (Vector3 x y z) = [x, y, z]

vecToM4x :: Vector3 GLdouble -> [[GLdouble]]
vecToM4x (Vector3 x y z) = [
                            [x, 0, 0, 0],
                            [y, 0, 0, 0],
                            [z, 0, 0, 0],
                            [0, 0, 0, 0]
                          ]

vecToM4y :: Vector3 GLdouble -> [[GLdouble]]
vecToM4y (Vector3 x y z) = [
                            [0, x, 0, 0],
                            [0, y, 0, 0],
                            [0, z, 0, 0],
                            [0, 0, 0, 0]
                          ]

vecToM4z :: Vector3 GLdouble -> [[GLdouble]]
vecToM4z (Vector3 x y z) = [
                            [0, 0, x, 0],
                            [0, 0, y, 0],
                            [0, 0, z, 0],
                            [0, 0, 0, 0]
                          ]

fx (Vector3 x y z) = Vector3 (rf x) (rf y) (rf z)

rotateWorldX :: IORef CameraRot -> IO ()
rotateWorldX refCamRot = do
  currXYZ <- readIORef refCamRot <&> currXYZ_
  coordFrame <- readIORef refCamRot <&> coordFrame_
  case currXYZ of
    n | n == 1 -> do
          coordFrameMat <- readIORef refCamRot <&> coordFrameMat_
          let ls = join coordFrameMat
          multiModelviewMat ls
          return ()
      | n == 2 -> do
          coordFrameMat <- readIORef refCamRot <&> coordFrameMat_
          let ls = join coordFrameMat
          multiModelviewMat ls
          return ()
      | n == 3 -> do
          coordFrameMat <- readIORef refCamRot <&> coordFrameMat_
          let ls = join coordFrameMat
          multiModelviewMat ls
          return ()
      | n == 4 -> do
          coordFrameMat <- readIORef refCamRot <&> coordFrameMat_
          let ls = join coordFrameMat
          multiModelviewMat ls
          return ()
      | otherwise -> do
          error $ "currXYZ invalid Integer = " ++ show currXYZ

gg cx = map (\(a, b) -> zipWith (\x y -> [x, y]) a b) cx
fg x y = zipWith (\a b -> (a, b)) (init x) (tail y)
hg m = map join $ gg $ fg m m

sleepSecRedis :: String -> IO()
sleepSecRedis s = do
    bracket
      (redisConnectDefault)
      (redisDisconnect)
      (\conn -> flip redisGetConn s conn <&> \x -> case x of
                                      Just s -> case DT.readMaybe s :: Maybe Int of
                                                     Just x -> x
                                                     Nothing -> 0
                                      Nothing -> 0
      ) >>= usleep

{-|
   === Better get redis value

   DATA: Tuesday, 27 February 2024 12:18 PST

   @
   v <- getRedisX "xx3"
   @
-}
getRedisX :: String -> IO Int
getRedisX s = do
  bracket
    (redisConnectDefault)
    (redisDisconnect)
    (\conn -> flip redisGetConn s conn <&> \x -> case x of
                                    Just s -> case DT.readMaybe s :: Maybe Int of
                                                   Just x -> x
                                                   Nothing -> 0
                                    Nothing -> 0
    )
  
getRedisXf :: String -> IO GLfloat
getRedisXf s = do
  bracket
    (redisConnectDefault)
    (redisDisconnect)
    (\conn -> flip redisGetConn s conn <&> \x -> case x of
                                    Just s -> case DT.readMaybe s :: Maybe GLfloat of
                                                   Just x -> x
                                                   Nothing -> 0.0
                                    Nothing -> 0.0
    )
  
getRedisXStr :: String -> IO String
getRedisXStr s = do
  bracket
    (redisConnectDefault)
    (redisDisconnect)
    (\conn -> flip redisGetConn s conn <&> \x -> case x of
                                    Just s -> s
                                    Nothing -> "[]"
    )
  
nx_1 (Vertex3 x y z) = Vertex3 (-x) y    z
nx_2 (Vertex3 x y z) = Vertex3  x   (-y) z
nx_3 (Vertex3 x y z) = Vertex3  x   y    (-z)
nx_12 (Vertex3 x y z) = Vertex3 (-x) (-y) z
  
vx_1 (Vertex3 x y z) = x
vx_2 (Vertex3 x y z) = y
vx_3 (Vertex3 x y z) = z

ve_1 (Vector3 x y z) = x
ve_2 (Vector3 x y z) = y
ve_3 (Vector3 x y z) = z

v_x (Vector3 x y z) = x
v_y (Vector3 x y z) = y
v_z (Vector3 x y z) = z

     

f k = let x = round $ (rf . round) 1e10 * sin k; 
          nd a = ceiling $ logBase 10 $ (rf . round) a; 
          y = fi $ nd x - 1 in mod x y

ns (Vertex3 x y z) | x + y > 0.3 = sin (x - y - z) 
                   | x - y < 0   = sin (x + y + z)
                   | x + y - z > 0.5 = sin (x + y - z)
                   | x + y + z > 1   = cos (y + z) 
                   | x + z     > 0.5 = sin (x) * cos (x - y - z) 
                   | otherwise       = sin x

drawTorusX::GLfloat -> GLfloat -> Int -> [Color3 GLdouble]-> IO()
drawTorusX r br n cx = do
  torus3' <- (mapM . mapM) (\v -> do 
               let mod' = flip mod
               let ve = vec_ v
               let r =  ns v 
               let ve' = (1 + r) *: ve
               let vx = vecToVex ve' 
               return vx
             ) torus3
  let cm = combinePt torus3' cx
  mapM_ (drawSegmentFromTo red) torus3'
  -- mapM_ (drawSegmentFromTo yellow) $ tran torus3
  mapM_ (\row -> do
            renderPrimitive TriangleStrip $ mapM_ (\(c, v) -> do
                                                      color c
                                                      vertex v
                                                      ) row
        ) cm
  where
    torus3::[[Vertex3 GLfloat]]
    torus3 =[[Vertex3 (fx i j) 
                (fy i j) 
                (fz i j) | i <- [0..n]] | j <-[0..n]]
            where
                δ = 2*pi/rf n
                fx = \i j -> let i' = rf i; j' = rf j in (br + r*cos (δ*i'))*cos (δ*j')
                fy = \i j -> let i' = rf i; j' = rf j in r*sin (δ*i')
                fz = \i j -> let i' = rf i; j' = rf j in (br + r*cos (δ*i'))*sin (δ*j')    

sdfRect :: Vertex3 GLfloat -> Vertex3 GLfloat -> Bool -> GLfloat
sdfRect r s isRound = isRound ? (if dx > 0 && dy > 0 then ds else  max dx dy) $ (min ds $ max dx dy)
  where
   dx = vx_1 $ s - r
   dy = vx_2 $ s - r
   ds = nr (r -: s)

sdfRect3d :: Vertex3 GLfloat -> Vertex3 GLfloat -> Bool -> GLfloat
sdfRect3d r s isRound = isRound ? (if dx > 0 && dy > 0 && dz > 0 then ds else  max dz $ max dz $ max dx dy) $ (min ds $ max dz $ max dx dy)
  where
   u = s - r
   dx = vx_1 u
   dy = vx_2 u
   dz = vx_3 u
   ds = nr (r -: s)

sdfCircle :: Vertex3 GLfloat -> GLfloat -> Vertex3 GLfloat -> GLfloat
sdfCircle c r s = nr (c -: s) - r
addy (Vertex3 x y z) = Vertex3 x (y + 0.2) z

mulMat :: [[GLfloat]] -> Vertex3 GLfloat -> Vertex3 GLfloat
mulMat cx vx = v0
  where
    ls = vexToList vx
    v0 = listToVex $ join $ cx `multiVec` ls

beginWindow3d :: G.Window -> IORef CameraRot -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> IO()
beginWindow3d w3d refCamRot refGlobal ioArray = do
  (width, height) <- G.getFramebufferSize w3d
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  GL.clear [ColorBuffer, DepthBuffer]
  GL.depthFunc $= Just Lequal
  G.getWindowFocused w3d >>= \b -> when b $ G.setKeyCallback w3d (Just $ keyBoardCallBack2d refCamRot refGlobal ioArray)
  G.getWindowFocused w3d >>= \b -> when b $ G.setMouseButtonCallback w3d (Just $ mouseCallbackX refGlobal)
  loadIdentity
  
  fov <- readIORef refCamRot <&> persp_ <&> fov_
  zf <- readIORef refCamRot <&> persp_ <&> zf_
  zn <- readIORef refCamRot <&> persp_ <&> zn_
  eye <- readIORef refCamRot <&> modelview_ <&> eye_
  matrixMode $= Projection
  loadIdentity

  perspective fov 1.0 zn zf

  matrixMode $= Modelview 0
  loadIdentity
  GL.lookAt eye (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
  
beginWindow2d :: G.Window -> IO()
beginWindow2d w = do
  GL.clear [ColorBuffer, DepthBuffer]
  (width, height) <- G.getFramebufferSize w
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  GL.depthFunc $= Just Lequal
  matrixMode $= Projection
  loadIdentity
  ortho2D (-1) 1 (-1) 1
  matrixMode $= Modelview 0
  loadIdentity  

endWindow2d :: G.Window -> IO()
endWindow2d w = do
  G.makeContextCurrent $ Just w
  G.swapBuffers w
  G.pollEvents
  
endWindow3d :: G.Window -> IO()
endWindow3d w = do
  G.makeContextCurrent $ Just w
  G.swapBuffers w
  G.pollEvents
    

saveImageFrame :: G.Window -> IOArray Int AnimaState -> IO()
saveImageFrame w animaStateArr = do
  let anima1 = 1
  let intervalx = 0 -- larger number is slower
  (isNext1, index1, animaState1) <- readAnimaState animaStateArr anima1 intervalx
  let fn = "/tmp/img_" ++ show (index1 + 1000) ++ ".png"
  saveImageOpenGL w fn
  writeAnimaState animaStateArr animaState1 {animaIndex_ = index1}
  

drawFinal :: G.Window -> IOArray (Int, Int, Int) BlockAttr -> RectGrid -> IO ()
drawFinal w arr rr = do
  showCurrBoardArr arr
  drawRectGridX rr
  G.swapBuffers w
  G.pollEvents
  
bk1 :: [[Int]]
bk1 =
  [ [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [1, 1, 1, 1, 1],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]
  ]

drawPolygon :: [Vertex3 GLfloat] -> IO()
drawPolygon cx = do 
  let cl = join $ repeat [green, blue, yellow]
  renderPrimitive LineLoop $ mapM_ (\(c, v) -> do
                                    color c
                                    vertex v
                                   ) $ zip cl cx

