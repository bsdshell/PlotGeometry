{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}

-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE CPP #-}

module Main where

import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL as GL 
import qualified Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL.GLU.Matrix as GM
import Graphics.Rendering.OpenGL 
import Graphics.Rendering.OpenGL.GL.CoordTrans
-- import qualified Graphics.Rendering.FTGL as FTGL
import qualified Graphics.UI.GLUT as GLUT
-- import Graphics.GL.Types

import System.Exit
import System.IO
import Control.Monad
import Data.Set(Set) 
import Data.IORef 
import Data.Int
import Data.Maybe
import qualified Data.Set as S
import qualified Data.List as DL
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import Data.Typeable
import qualified Text.Printf as PR
import qualified Data.Map as DM
import Language.Haskell.Interpreter

import GHC.Float.RealFracMethods

import System.Directory
import System.Environment
import System.Exit
import System.IO
import Control.Concurrent
  
import qualified Control.Monad.State as CMS

-- Saturday, 07 October 2023 11:46 PDT
-- Complex number,
-- c = 1 :+ 2
import Data.Complex

import Data.StateVar
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

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
import AronModule hiding (rw)
import AronGraphic hiding (dist)
import AronOpenGL
-- import AronDevLib

import qualified Data.Vector as VU

{-|

   | -------------------------------------------------------------------------------- 
   | compile: run.sh  
   | ghc -i/Users/cat/myfile/bitbucket/haskelllib -o file file.hs
   | 
   | KEY: keyboard example, keypress example, modifyIORef example,
   |
   | Tuesday, 09 November 2021 11:56 PST
   |
   | TODO: Combine Cam{..} and Step{..} in one function with Keyboard input
   |     Current issue: try to implement orthogonal projective with key one press
   |                    but Step{..} and Cam{..} are different type class.
   |
   | mainLoop w refCam refStep refCount lssVex
   | keyboardRot refCam refStep (fromIntegral width) (fromIntegral height)

   @
   data Cam = Cam{alpha::Double, beta::Double, gramma::Double, dist::Double} deriving(Show)

   data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double} deriving(Show)
   initCam = Cam{alpha=0.0, beta=0.0, gramma=0.0, dist = 0.0}
   initStep = Step{xx=0.0, yy=0.0, zz=0.0, ww = 0.01}
   @
-}
  
tmpfile = "/tmp/tmpfile.txt"

mc::(GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
mc (a, b) (a', b') = (a*a' - b*b', a*b' + a'*b)
  
convexPts::IO [Vertex3 GLfloat]
convexPts = return cx
    where
      cx = [
              Vertex3 0.1   0.1  0
             ,Vertex3 0.2   0.6  0
             ,Vertex3 0.88  0.9  0
             ,Vertex3 0.25  0.34 0
             ,Vertex3 0.12  0.8  0
             ,Vertex3 1.3   0.12 0
            ]
  
drawCylinderX::[[Vertex3 GLfloat]] -> IO()
drawCylinderX cx = drawSurfaceFromList cx

  
helpme::IO()
helpme = do
  let (+) = (++)
  b <- en "b"
  -- AronModule.clear
  let ls = ["file => " + b + "/tmp/draw.x",
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

say::String -> Interpreter()
say = liftIO . putStrLn

emptyLine::Interpreter()
emptyLine = say ""

colorls = VU.fromList [
  Color3 0.1 0.1 0.1,
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

testHint::[String] -> [String] -> Interpreter()
testHint xs expls = do
        loadModules ["/Users/cat/myfile/bitbucket/stackproject/HelloSimple/src/AronSimple.hs"]
        setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M"), ("AronSimple", Nothing)]        
        let stmts = xs
        forM_ stmts $ \s -> do
          say $ " " ++ s
          runStmt s        
        emptyLine


        -- let expr3 = "\\(x, y) -> x + 1 + y"
        VU.mapM_ (\(exp, c) -> do
                  fun <- interpret exp (as :: GLfloat -> GLfloat)
                  -- say $ show $ fun 3.141569
                  liftIO $ drawCurveV fun (-1.0, 1.0) c
              ) $ VU.zip (VU.fromList expls) colorls

        emptyLine
            


          


  
-- (a + bi)(a' + b'i)
-- (aa' - bb') + (ab' + ba')i
-- (a, b) * (a', b') = (aa' - bb', ab' + a'b)

-- conform= [(0.1 * (fst $ mc c c'), 0.1* (snd $ mc c c'), 0) 
--            | c <- zip (map(*0.01) [1..aa]) (map(*0.01) [1..bb]), c' <- zip (map(*0.01) [1..aa]) (map(*0.01) [1..bb])]
conform::[[Vertex3 GLfloat]]
conform= [[Vertex3 (fst $ mc c1 c2) (snd $ mc c1 c2)  0 | c1 <- c] | c2 <- c] 
--conform= [[(fst $ c1, snd $ c1 , 0) | c1 <- c] | c2 <- c] 
        where 
            fa = 0.1 
            aa = map(\x -> fa * x) [1..10]
            bb = map(\x -> fa * x) [1..3]
            c = foldr(++) [] $ [[(a, b) | a <- aa] | b <- bb]


  
grid::[[[Vertex3 GLfloat]]]
grid =[[[Vertex3 a b (a*a - b*b) | a <- aa] | b <- bb] | c <- cc] 
        where 
            n  = 10
            fa = 1/n 
            aa = map(\x -> fa * x) [1..n]
            bb = map(\x -> fa * x) [1..n]
            cc = map(\x -> fa * x) [1..n]

grid2::[[Vertex3 GLfloat]]
grid2 =[[Vertex3 a b (a*a - b*b) | a <- aa] | b <- bb] 
        where 
            n  = 30
            fa = 1/(2*n)
            aa = map(\x -> fa * x) [-n..n]
            bb = map(\x -> fa * x) [-n..n]


  
-- f z = z*z
-- 
-- a    => x
-- b    => y
-- re c => z
-- im c => color
grid4::[[(Vertex3 GLfloat, GLfloat)]]
grid4 =[[ let c = (C a b)*(C a b) in (Vertex3 a b (re c), im c) | a <- aa] | b <- bb]
        where 
            ne = [[ let c = sqrtC' (C a b) in (Vertex3 a b (re c), im c) | a <- aa] | b <- bb]
            n  = 20 
            fa = 1/(1.5*n)
            aa = map(\x -> fa * x) [-n..n]
            bb = map(\x -> fa * x) [-n..n]

grid4' = (map . map) (\x -> fst x) grid4


  
trig s1 s2 = map(\x -> foldr(++) [] x) $ (zipWith . zipWith)(\x y -> [x, y]) (init s1) (tail s2)

segment1 = zipWith(\x y -> [Vertex3 (-1) 0 0, y]) [1..] test_circle

test_circle::[Vertex3 GLfloat]
test_circle=[ let x = (1-t*t)/(1 + t*t); 
                  y = (2*t)/(1 + t*t) in Vertex3 x y 0 | t <- aa]
        where 
            n  = 50 
            fa = 1/(0.1*n)
            aa = map(\x -> fa * x) [-n..n]

splitPt::Int->[(GLfloat, GLfloat, GLfloat)]->[[(GLfloat, GLfloat, GLfloat)]]
splitPt _ [] = []
splitPt n xs = take n xs : (splitPt n $ drop n xs)


mergeChunk::Int->[(GLfloat, GLfloat, GLfloat)]->[(GLfloat, GLfloat, GLfloat)]
mergeChunk n c = mergeList  (take n c)  (take n $ drop n c) 

bigChunk::Int->[(GLfloat, GLfloat, GLfloat)]->[[(GLfloat, GLfloat, GLfloat)]]
bigChunk n xs = splitPt n xs

renderSurface::[(GLfloat, GLfloat, GLfloat)]->IO()
renderSurface xs = do 
        iterateList (bigChunk 80 xs) (\chunk -> do
                        let len = length chunk
                        let n = div len 2 
                        let c = mergeChunk n chunk
                        let cc = zipWith(\x y -> (x, y)) c [1..]
                        renderPrimitive TriangleStrip $ mapM_(\((x, y, z), n) -> do 
                            case mod n 3 of 
                                0 -> do 
                                        color(Color3 0.8 1 0 :: Color3 GLdouble) 
                                1 -> do 
                                        color(Color3 0 0.5 1 :: Color3 GLdouble) 
                                _ -> do 
                                        color(Color3 1 0 0.7 :: Color3 GLdouble) 
                            normal $ (Normal3 x y z::Normal3 GLfloat)
                            vertex $ Vertex4 x y z 0.8) cc  
                            )


{-|

    translate rotate drawRect drawCircle
       

  
-}


type Vex3 = Vertex3 GLfloat
type V3d = Vertex3 GLdouble
  

drawHistogram::IO()
drawHistogram = do
  preservingMatrix $ do
    translate (Vector3 0.0 0 0 :: Vector3 GLdouble)
    drawRect (Vertex3 (-0.1) (-0.1) 0, Vertex3 0.1 0.1 0)



drawHistogram2::[GLfloat] -> IO()
drawHistogram2 cx = do
  let n = len cx
  let δ = 1 / rf n
  let w = δ - 0.002
  let zz = map(\(a, b) -> (rf a, b)) $ zip cx [0..]
  mapM (\(h, c) -> do
           pp h
           pp c
           let off = rf $ c * δ
           preservingMatrix $ do
             translate (Vector3 off (h/2) 0 :: Vector3 GLdouble)
             -- drawRect2d w (rf h)
             drawRectFill2d white (w, (rf h))
         
             -- translate (Vector3 off (-0.3) 0 :: Vector3 GLdouble)
           preservingMatrix $ do
             let strNum = PR.printf "%.1f" h :: String
             strWidth <- GLUT.stringWidth GLUT.Roman strNum
             -- strHeight <- GLUT.stringHeight GLUT.Roman str
             -- 1000 => 1000 pixel
             print $ "strWidth=" ++ (show $ rf strWidth/scaleFont)
             let cen = off - ((rf strWidth) /(scaleFont*2.0))
             print $ "cen=" ++ (show cen)
             print $ "off=" ++ (show off)
             translate (Vector3 cen (-0.1) 0 :: Vector3 GLdouble)
             renderText strNum
           return ()
       ) zz
  return ()



drawHis::[GLfloat] -> IO()
drawHis cx = do
  preservingMatrix $ do
    translate (Vector3 (-0.5) 0 0 ::Vector3 GLdouble)
    drawHistogram2 cx
  

drawTri::[(Vertex3 GLfloat)] -> IO()
drawTri cx = do
    drawPrimitiveVex LineLoop green cx
 
{-|
   === Move object alone X-Axis

   1. move drawRect2d to the right in x

   @
   moveToX drawRect2d (w, h) x    -- move to the right
   moveToX drawRect2d (w, h) (-x) -- move to the left 
   @

       drawRect2d w h

           ↑ y
           |
         ⌜---⌝
  ← -x   |   |  → x
         | + |–--->      X-Axis
         |   |
         ⌞---⌟

-}
moveToX::((GLfloat, GLfloat) -> IO()) -> (GLfloat, GLfloat) -> GLdouble -> IO()  -- moveToX drawRect2d (w, h) x  -- move to the (x) right, (-1) left
moveToX f (w, h) x = do
  preservingMatrix $ do
    translate (Vector3 x 0 0 ::Vector3 GLdouble)
    f (w, h)

moveToY::((GLfloat, GLfloat) -> IO()) -> (GLfloat, GLfloat) -> GLdouble -> IO()
moveToY f (w, h) y = do
  preservingMatrix $ do
    translate (Vector3 0 y 0 ::Vector3 GLdouble)
    f (w, h)
    
moveToZ::((GLfloat, GLfloat) -> IO()) -> (GLfloat, GLfloat) -> GLdouble -> IO()
moveToZ f (w, h) z = do
      preservingMatrix $ do
        translate (Vector3 0 0 z ::Vector3 GLdouble)
        f (w, h)


  
renderText::String -> IO()
renderText str = do
  preservingMatrix $ do
    -- rotate (60)$ (Vector3 0 0 1 :: Vector3 GLdouble)
    -- translate (Vector3 0 (-0.1) 0 ::Vector3 GLdouble)
    GL.scale (1/scaleFont :: GL.GLdouble) (1/scaleFont) 1
    GLUT.renderString GLUT.Roman str
    
    -- KEY: string width, string height, font width, font height
    -- strWidth <- GLUT.stringWidth GLUT.Roman str
    -- strHeight <- GLUT.stringHeight GLUT.Roman str
  

  
{-|
    === cylinder xz-plane, perpendicular to xz-plane
-}
cylinder::GLfloat -> IO()
cylinder r = drawSurfaceFromList cylinderPt
  

  

cylinderPt::[[Vertex3 GLfloat]]
cylinderPt = cm
  where
      cm = let n = 40::Int
               δ = (2*pi)/(rf(n-1)) :: Float
               r = 0.1
               br = 0.2
               σ = 1/rf(n-1)

               fx::Int -> Int -> GLfloat
               fx i j = let i' = rf i
                            j' = rf j
                        in (1/rf 40)* j'

               fy::Int -> Int -> GLfloat
               fy i j = let i' = rf i
                            j' = rf j
                            n = 3
                        in r * cos(δ * i')

               fz::Int -> Int -> GLfloat
               fz i j = let i' = rf i
                            j' = rf j
                        in r*sin(δ * i')

           in [[Vertex3 (fx j j) (fy i i) (fz i i) | i <- [1..n]] | j <- [1..n]]


lsfun = [          
  "\\x -> x*x",
  "\\x -> 2*x*x",
  "\\x -> 3*x*x",
  "\\x -> 4*x*x",
  "\\x -> 5*x*x",
  "\\x -> 6*x*x",
  "\\x -> 7*x*x",
  "\\x -> 8*x*x",
  "\\x -> 9*x*x"
  ]  

{-|
    KEY: convert 'String' to 'Vector3' 'GLdouble'

    @
     Input:
     vector3
     0.3 0.4 0.0
     0.2 0.3 0.0
     endvector3

     => [Vector3 0.3 0.4 0.0, Vector3 0.2 0.3 0.0]
    @
-}    
takeVector3::[String] -> [Vector3 GLdouble]
takeVector3 [] = []
takeVector3 cx = cs
  where
    beg = "vector3"
    end = "endvector3"
    ss = filter (\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    cs = map (\x -> strToVector3 x) ss
      
{-|
    KEY: convert 'String' to 'Vertex3' 'GLfloat'

    @
     Input:
     triangle
     0.3 0.4 0.0
     0.2 0.3 0.0
     0.1 0.2 0.0
     endtriangle

     => [Vertex3 0.3 0.4 0.0, Vertex3 0.2 0.3 0.0, Vertex3 0.1 0.2 0.0]
    @
-}    
takeTriangleVex::[String] -> [Vertex3 GLfloat]
takeTriangleVex [] = []
takeTriangleVex cx = xs
  where
    beg = "triangle"
    end = "endtriangle"
    ss = filter(\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    xs = map(\x -> strToVertex3 x ) ss


{-|
    === KEY: vertex to tuple
-}
vertex3Triple::[Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)]
vertex3Triple cx = ts
  where
    ls = let s = partList 3 cx in if (len . last) s == 3 then s else init s
    ts = map(\(a:b:c:_) -> (a, b, c)) ls
  

{-|
    === KEY: Convert a list vertices to tuple3 vertices

    @
     [Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2, Vertex3 0.3 0.3 0.3, Vertex3 0.4 0.4 0.4]

     => [(Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2, Vertex3 0.3 0.3 03)]
    @
-}
listToTuple3Vex::[Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)]
listToTuple3Vex cx = let lss = partList 3 cx
                         lst = if len lss > 0 then (let s = last lss
                                                    in len s == 3 ? lss $ init lss)
                               else lss
                     in map(\x -> let a = x ! 0
                                      b = x ! 1
                                      c = x ! 2
                                      (!) = (!!)
                                  in (a, b, c)) lst :: [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)]

listToTuple3VexD::[Vertex3 GLdouble] -> [(Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble)]
listToTuple3VexD cx = let lss = partList 3 cx
                          lst = if len lss > 0 then (let s = last lss in len s == 3 ? lss $ init lss)
                                else lss
                      in map(\x -> let a = x ! 0
                                       b = x ! 1
                                       c = x ! 2
                                       (!) = (!!)
                                   in (a, b, c)) lst :: [(Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble)]                                                      

{-|
    === KEY: Convert a list vertices to tuple2 vertices

    @
     [Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2, Vertex3 0.3 0.3 0.3, Vertex3 0.4 0.4 0.4]

     => [(Vertex3 0.1 0.1 0.1, Vertex3 0.2 0.2 0.2),
         (Vertex3 0.3 0.3 0.3,  Vertex3 0.4 0.4 0.4)
        ]
    @
-}                                                    
listToTuple2Vex::[Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
listToTuple2Vex cx = let lss = partList num cx
                         lst = if ρ lss > 0 then (let s = last lss
                                                    in ρ s == num ? lss $ init lss)
                               else lss
                     in map(\x -> let a = x ! 0
                                      b = x ! 1
                                      (!) = (!!)
                                  in (a, b)) lst :: [(Vertex3 GLfloat, Vertex3 GLfloat)]
  where
    num = 2
  
--   #endif
  

  

{-|   
renderText :: GLUT.Font -> String -> IO ()
renderText font str = do
    GL.scale (1/64 :: GL.GLdouble) (1/64) 1
    GLUT.renderString GLUT.Roman str
-}    
mymain :: IO ()
mymain = do
    successfulInit <- G.init
    G.windowHint (G.WindowHint'DoubleBuffer True)
    -- if init failed, we exit the program
    bool successfulInit exitFailure $ do
        mw <- G.createWindow 1000 1000 "PlotGeometry" Nothing Nothing
        maybe' mw (G.terminate >> exitFailure) $ \window -> do
            G.makeContextCurrent mw
  

            
            
            ref <- newIORef initCam 
            refStep <- newIORef initStep
            refGlobal <- newIORef initGlobal
            globalRef <- readIORef refGlobal
            writeIORef refGlobal globalRef


  
            -- pre cylinderPt
            -- cylinder
            -- writeIORef refGlobal $ setDrawPts globalRef cylinderPt
            -- sphere
            -- randomPts <- randomVertex (10*3)
            randomPts <- convexPts
  

            
            -- writeIORef refGlobal $ setDrawPts   globalRef spherePtsX
            modifyIORef refGlobal (\x -> x{drawPts_ = spherePtsX})
            
            globalRef2 <- readIORef refGlobal
            -- writeIORef refGlobal $ setRandomPts globalRef2 randomPts
            modifyIORef refGlobal (\x -> x{randomPts_ = randomPts})

            refFrame <- (timeNowMilli >>= \x -> newIORef FrameCount{frameTime = x, frameCount = 1, frameIndex = 0})
    
            let cx = circleNArc' (Vertex3 0.4 0 0) 0.4 40 (0, pi)
            let cy = curvePtK (\x -> 0) (0, 0.8) 40
            let cx' = [cx, cy]
            
            ls <- randomIntList 10 (1, 4) >>= \cx -> return $ randomVexList (Vertex3 0.0 0.0 0.0) cx
            modifyIORef refGlobal (\x ->x{randomWalk_ = ls})
            
            mainLoop window ref refStep refGlobal refFrame cx'
            G.destroyWindow window
            G.terminate
            exitSuccess
    
{-|
    KEY: convert 'String' to 'Vertex3' 'GLfloat'

    @
     Input:
     segment
     0.3 0.4 0.0
     0.2 0.3 0.0
     0.1 0.2 0.0
     endsegment

     => [Vertex3 0.3 0.4 0.0, Vertex3 0.2 0.3 0.0, Vertex3 0.1 0.2 0.0]
    @
-}
takeSegment::[String] -> [Vertex3 GLfloat]
takeSegment [] = []
takeSegment cx = cs
  where
    beg = "segment"
    end = "endsegment"
    ss = filter(\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    cs = map(\x -> strToVertex3 x ) ss

{-|
    KEY: convert 'String' to 'Vertex3' 'GLfloat'

    @
     Input:
     point
     0.3 0.4 0.0
     0.2 0.3 0.0
     endpoint

     => [Vertex3 0.3 0.4 0.0, Vertex3 0.2 0.3 0.0]
    @
-}
takePoint::[String] -> [Vertex3 GLfloat]
takePoint [] = []
takePoint cx = cs
  where
    beg = "point"
    end = "endpoint"
    ss = filter(\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    cs = map(\x -> strToVertex3 x ) ss
                                                      
circleNArc'::Vertex3 GLfloat -> Double -> Integer -> (GLfloat, GLfloat) -> [Vertex3 GLfloat]
circleNArc' (Vertex3 x₀ y₀ z₀) r n (r₀, r₁) = [let δ = (r₁ - r₀)/(rf n); r' = rf r
                                             in Vertex3 (r' * cos (r₀  + (rf x)* δ) + x₀)  (r' * sin (r₀ + (rf x)* δ) + y₀)  z₀ | x <- [0..n]]
                                               
{-|
   [0, 1, 2]
   x0 -- x1 -- x2

   curvePtK::(GLfloat -> GLfloat)->(GLfloat,    GLfloat) -> Integer ->[Vertex3 GLfloat]
                                     ↑             ↑          ↑ 
                                     +-  interval  +          |
                                                              + - n steps 
                                     |             |
                                     x_0           x_1
                                        

                                      (x_1 - x_0) / n
                    f(x) = x^2 


    curvePtK f (0, 1.0) 10
     f(x) = x^2
    |-----+------|
    |   x |  x^2 |
    | 0.1 | 0.01 |
    | 0.2 | 0.04 |
    | 0.3 | 0.09 |
    | 0.4 | 0.16 |
    | 0.5 | 0.25 |
    | 0.6 | 0.36 |
    | 0.7 | 0.49 |
    | 0.8 | 0.64 |
    | 0.9 | 0.81 |
    | 1.0 |  1.0 |
    |-----+------|  
-}
curvePtK::(GLfloat -> GLfloat)->(GLfloat, GLfloat) -> Integer ->[Vertex3 GLfloat]
curvePtK f (x₀, x₁) n = [Vertex3 x (f x) 0 | x <- let δ = (x₁ - x₀)/(rf n) in map(\x -> x₀ + (rf x) * δ) [0..n]]
          
interpolate::[[Vertex3 GLfloat]]
interpolate = cz
  where
    n = 40::Integer
    f x = 0
    line = curvePtK f (0, 0.8) n
    arc = circleNArc' (Vertex3 0.4 0 0) 0.4 n (0, pi)
    cz = map(\t -> zipWith(\(Vertex3 x₀ y₀ z₀) (Vertex3 x₁ y₁ z₁)  -> Vertex3 x₁ ((1 - t)*y₀ + t * y₁) 0) line arc) [0, 0.01..1]


drawArc::(GLdouble, GLdouble, GLdouble) -> [Vertex3 GLdouble] -> IO()
drawArc (x, y, z) cx = do
  preservingMatrix $ do
     translate (Vector3 x y z :: Vector3 GLdouble)
     drawSegmentFromTo2 blue cx
     mapM_ (drawSegmentD green) $ zip zi cx
  where
    zi = take (len cx) $ repeat (Vertex3 0 0 0::Vertex3 GLdouble)


{-|
    === KEY: generate a set of polynomials
-}
curveSet::[[Vertex3 GLfloat]]
curveSet = c1
  where
    f t x = t * x * x
    n = 40::Integer
    c1 = map(\t -> curvePtK (f t)( -0.8, 0.8) n) [0.5, 1.0..20]



  
{-|
    === KEY: throw up, falling object

    DATE: Monday, 17 April 2023 22:30 PDT
    FIXED: mixed-up x y z
-}
throwUp::[Vertex3 GLfloat] -> [Vertex3 GLfloat]
throwUp cx = map (\(Vertex3 x y z) -> Vertex3 x y z :: Vertex3 GLfloat) cx

throwUpPath::[Vertex3 GLfloat]
throwUpPath = throwUp parabolicPath

{-|
   === KEY: use multMatrix, multiply matrix, multiply matrix with vector

   https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html

  @
      (cos α  + i sin α) ( cos β + i sin β)
    = cos α  * cos β + sin α * cos β + i cos α * cos β + i cos α * sin β
    = (cos α  * cos β + sin α * cos β) + i (cos α * cos β + cos α * sin β) 
    = cos (α + β) + i sin (α + β)

     m 1    => 0 
       0       1

     m 0    => -1
       1       0

    1  0      0 -1 
    0  1      1  0

                     →  X -  \         
                              ↓       
                     |       Y                 
                     \      /              
                       Z  ←                      

                Right-hand Rule

                      y
                      ^
                      |                       
                      + - ->   x
                     /
                    /
                   z

                 x (x) y => Z     
             rotate around Z-axis
             cos x   -sin x  0 
             sin x    cos x  0    
             0        0      1
                   ↓ 
             Swap row(2, 3) => M = -M
                   ↓ 

             rotate around X-axis
             cos x   sin x  0 
             0        0      1
             -sin x    cos x  0     

                    ↓  
             Swap row (1, 2) => M = -M
                    ↓         

             rotate around Y-axis
             0        0      1
             cos x   -sin x  0 
             sin x    cos x  0     
  @
-}
testmultMatrix::IO()
testmultMatrix = do
                  let x = 1.0::GLfloat
                  let y = 0.0::GLfloat
                  let z = 0.0::GLfloat
                  mat <- newMatrix RowMajor [ 1, 0, 0, x
                                             ,0, 1, 0, y
                                             ,0, 0, 1, z
                                             ,0, 0, 0, 1]::IO (GLmatrix GLfloat)

                  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html
                  -- multMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
                  GL.multMatrix mat

                  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
                    -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
                  ls <- getMatrixComponents RowMajor mat  -- [GLfloat]
                  -- pre ls
                  writeFileList "/tmp/m.x" $ map show ls

multModelviewVec::Vertex3 GLfloat -> IO [GLfloat]
multModelviewVec (Vertex3 x y z) = do
--                      let x = 1.0::GLfloat
--                      let y = 0.0::GLfloat
--                      let z = 0.0::GLfloat
                      mat <- newMatrix RowMajor [ 1, 0, 0, x
                                                 ,0, 1, 0, y
                                                 ,0, 0, 1, z
                                                 ,0, 0, 0, 1]::IO (GLmatrix GLfloat)

                      -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html
                      -- multMatrix :: (Matrix m, MatrixComponent c) => m c -> IO ()
                      GL.multMatrix mat

                      -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/src/Graphics.Rendering.OpenGL.GL.CoordTrans.html#getMatrixComponents
                        -- getMatrixComponents :: MatrixComponent c => MatrixOrder -> m c -> IO [c]
                      ls <- getMatrixComponents RowMajor mat  -- [GLfloat]
                      -- pre ls
                      writeFileList "/tmp/m.x" $ map show ls
                      return ls 


getMatrixTest::IO()
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

getModelviewMatrix::IO[GLfloat]
getModelviewMatrix = do
                      let stateVar = GL.matrix (Just $ Modelview 16) :: StateVar (GLmatrix GLfloat)
                      m1 <- Data.StateVar.get stateVar
                      pre m1
                      -- ls <- getMatrixComponents RowMajor m1  -- [GLfloat]
                      ls <- getMatrixComponents ColumnMajor m1  -- [GLfloat]
                      pre ls
                      writeFileList "/tmp/m1.x" $ map show ls 
                      return ls 

matrixTest::IO()
matrixTest = do
              let stateVar = GL.matrix (Just $ Modelview 16) :: StateVar (GLmatrix GLfloat)
              m1 <- Data.StateVar.get stateVar
              pre m1
              ls <- getMatrixComponents RowMajor m1  -- [GLfloat]
              pre ls
              writeFileList "/tmp/m1.x" $ map show ls 
              return ()


xor::Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

{-|
   x  y  z
   0  1  0       
   1  0  0   XOR
  ---------
   1  1  0

   x  y  z
   0  1  0
   0  1  0   XOR
  ---------
   0  0  0

   x  y  z
   0  1  0
   0  0  1   XOR
  ---------
   0  1  1

-}
flipAxis::XYZAxis -> XYZAxis -> XYZAxis
flipAxis axisOld axisNew | x' = XYZAxis{xa = xor x x', ya = False,    za = False}
                         | y' = XYZAxis{xa = False,    ya = xor y y', za = False}
                         | z' = XYZAxis{xa = False,    ya = False,    za = xor z z'}
                         | otherwise = XYZAxis{xa = False, ya = False, za = False}
  where
    x = xa axisOld
    y = ya axisOld
    z = za axisOld
    x' = xa axisNew
    y' = ya axisNew
    z' = za axisNew
  
xAxis::XYZAxis
xAxis = XYZAxis{xa = True, ya = False, za = False}
  
yAxis::XYZAxis
yAxis = XYZAxis{xa = False, ya = True, za = False}
  
zAxis::XYZAxis
zAxis = XYZAxis{xa = False, ya = False, za = True}

initXYZAxis::XYZAxis
initXYZAxis = XYZAxis{xa = False, ya = False, za = False}

initGlobal::GlobalRef
initGlobal = GlobalRef{
             str_ = "" 
             ,cursor_ = (0.0, 0.0) 
             ,xyzAxis_ = initXYZAxis 
             ,mousePressed_ = (False, (0.0, 0.0))
             ,drawRectX_ = (Vertex3 (-0.2) (-0.2) (0.0::GLfloat), Vertex3 0.2 0.2 (0.0::GLfloat)) 
             ,tranDrawRectX_ = Vector3 0.0 0.0 (0.0::GLdouble)
             ,fovDegree_ = 100.0
             ,drawPts_ = [[Vertex3 0.0 0.0 0.0]]
             ,randomWalk_ = [Vertex3 0.0 0.0 0.0, Vertex3 0.1 0.1 0.1]
             ,boardMap_ = DM.empty
             }

{-|
    NOTE: NOT BEEN USED
-}
keyBoardCallBackXYZ::IORef XYZAxis -> G.KeyCallback
keyBoardCallBackXYZ ref window key scanCode keyState modKeys = do
    pp "keyBoardCallBack in $b/haskelllib/AronOpenGL.hs"
    putStrLn $ "inside =>" ++ show keyState ++ " " ++ show key
    axisOld <- readIORef ref
    case keyState of
        G.KeyState'Pressed -> do 
            case key of
              k | k == G.Key'X -> writeIORef ref $ flipAxis axisOld xAxis
                | k == G.Key'Y -> writeIORef ref $ flipAxis axisOld yAxis
                | k == G.Key'Z -> writeIORef ref $ flipAxis axisOld zAxis
                | otherwise -> pp "Unknown Key Press"
        G.KeyState'Released -> do
            if key == G.Key'X then pp "Release Key => Right" else pp "Press   No Right"
            if key == G.Key'Y then pp "Release Key => Left"  else pp "Press   No Right"
            if key == G.Key'Z then pp "Release Key => Up"    else pp "Release No Up"
  
    when (key == G.Key'Escape && keyState == G.KeyState'Pressed)
        (G.setWindowShouldClose window True)

{-|
    KEY: 
    NOTE: USED
-}
keyBoardCallBack2::IORef Step -> IORef GlobalRef -> G.KeyCallback
keyBoardCallBack2 refStep refGlobalRef window key scanCode keyState modKeys = do
    pp "keyBoardCallBack in $b/haskelllib/AronOpenGL.hs"
    putStrLn $ "inside =>" ++ show keyState ++ " " ++ show key
    globalRef <- readIORef refGlobalRef
    let axisOld = xyzAxis_ globalRef
    let fovOld = fovDegree_ globalRef
    logFileG ["fovOld=" ++ show fovOld]
    case keyState of
        G.KeyState'Pressed -> do 
            -- write Step{...} to ref
            case key of
              k | k == G.Key'Right -> writeIORef refStep Step{xx=_STEP,    yy =0.0,      zz = 0.0,    ww = 0.0}
                | k == G.Key'Left  -> writeIORef refStep Step{xx=(-_STEP), yy =0.0,      zz = 0.0,    ww = 0.0}
                | k == G.Key'Up    -> writeIORef refStep Step{xx=0.0,      yy =_STEP,    zz = 0.0,    ww = 0.0}
                | k == G.Key'Down  -> writeIORef refStep Step{xx=0.0,      yy =(-_STEP), zz = 0.0,    ww = 0.0}
                | k == G.Key'9     -> writeIORef refStep Step{xx=0.0,      yy =0.0,      zz = _STEP,  ww = 0.0}
                | k == G.Key'0     -> writeIORef refStep Step{xx=0.0,      yy =0.0,      zz = -_STEP, ww = 0.0}
                | k == G.Key'8     -> writeIORef refStep Step{xx=0.0,      yy =0.0,      zz = 0.0,    ww = _STEP}
                | k == G.Key'7     -> writeIORef refStep Step{xx=0.0,      yy =0.0,      zz = 0.0,    ww = -_STEP}
                
                -- | k == G.Key'X     -> writeIORef refGlobalRef $ setXYZAxis globalRef $ flipAxis axisOld xAxis
                | k == G.Key'X     -> modifyIORef refGlobalRef (\x -> x{xyzAxis_ = flipAxis (xyzAxis_ x) xAxis})
                --                                  ↑  
                --                                  + -> Update Coord to YZ-plane

                -- | k == G.Key'Y     -> writeIORef refGlobalRef $ setXYZAxis globalRef $ flipAxis axisOld yAxis
                | k == G.Key'Y     -> modifyIORef refGlobalRef (\x -> x{xyzAxis_ = flipAxis (xyzAxis_ x) yAxis})
                --                                  ↑ 
                --                                  + -> Update Coord to XZ-plane

                -- | k == G.Key'Z     -> writeIORef refGlobalRef $ setXYZAxis globalRef $ flipAxis axisOld zAxis
                | k == G.Key'Z     -> modifyIORef refGlobalRef (\x -> x{xyzAxis_ = flipAxis (xyzAxis_ x) zAxis})
                --                                  ↑ 
                --                                  + -> Update Coord to XY-plane

                -- zoom out
                -- | k == G.Key'O     -> writeIORef refGlobalRef $ setFOVDegree globalRef $ fovOld + 5.0
                | k == G.Key'O     -> modifyIORef refGlobalRef (\x -> x{fovDegree_ = fovDegree_ x + 5.0})
                --                                  ↑ 
                --                                  + -> Update Coord to XY-plane
                -- zoom in 
                -- | k == G.Key'I     -> writeIORef refGlobalRef $ setFOVDegree globalRef $ fovOld - 5.0
                | k == G.Key'I     -> modifyIORef refGlobalRef (\x -> x{fovDegree_ = fovDegree_ x - 5.0})
                --                                  ↑ 
                --                                  + -> Update Coord to XY-plane
  
                -- TODO: In orthogonal projective status, 
                | k == G.Key'Space -> writeIORef refStep initStep
                | otherwise -> pp "Unknown Key Press"
        G.KeyState'Released -> do
            if key == G.Key'Right then pp "Release Key => Right" else pp "Press No Right"
            if key == G.Key'Left  then pp "Release Key => left"  else pp "Press No Right"
            if key == G.Key'Up    then pp "Release Key => up"    else pp "Release No Up"
            if key == G.Key'Down  then pp "Release Key => Down"  else pp "Release No Down"
        _   -> pp "Unknow keyState"
    when (key == G.Key'Escape && keyState == G.KeyState'Pressed)
        (G.setWindowShouldClose window True)

{-|
     
                    | upLeftY
         upLeftX -> v +----------+ 
                                 |
                                 |
                      +          + 
                                 ^ <-  downRightX 
                                 |
                              downRightY

                                 + -> upLeft 
                                 |             
                                 |            + -> downRight
                                 |            |                    + -> cursor pos
                                 ↓            ↓                    ↓       
-}
drawRectX::G.Window -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat) -> IO()
drawRectX w (p0@(Vertex3 x0 y0 z0), p1@(Vertex3 x1 y1 z1)) c@(x, y) = do
        -- NDC [-x, +x] = [-2, +2], [-y, +y] = [-2, +2]
        let ndcWidth = 4.0
        (width, height) <- G.getFramebufferSize w 
        (winW, winH) <- G.getWindowSize w >>= \(w, h) -> return $ (rf w, rf h)
        -- let (winW, winH) = (rf winWidth, rf winHeight)
        let (w, h) = (rf width, rf height)
        let upLeftX = winW / 2 + x0 * (winW / ndcWidth)
        let upLeftY = winH / 2 + y0 * (winH / ndcWidth)
        let downRightX = winW / 2 + x1 * (winW / ndcWidth)
        let downRightY = winH / 2 + y1 * (winH / ndcWidth)
        logFileG ["upLeftX=" ++ sw upLeftX, "upLeftY=" ++ sw upLeftY, "downRightX=" ++ sw downRightX, "downRightY=" ++ sw downRightY]
        logFileG ["FrameBuffer.x=" ++ sw w, "FrameBuffer.y=" ++ sw h]
        logFileG ["WindowSize.x=" ++ sw winW, "WindowSize.y=" ++ sw winH]
        if upLeftX <= x && x <= downRightX && upLeftY <= y && y <= downRightY then do
            drawBox green
        else do
            drawBox red
    where
      sw = show
      drawSeg = drawSegmentNoEnd
      drawBox color = do
            drawSeg color (Vertex3 x0 y0 z0, Vertex3 x1 y0 z0)  --  top horizontal 
            drawSeg color (Vertex3 x0 y0 z0, Vertex3 x0 y1 z0)  --  left vertical  
            drawSeg color (Vertex3 x1 y0 z0, Vertex3 x1 y1 z1)  --  right vertical
            drawSeg color (Vertex3 x0 y1 z0, Vertex3 x1 y1 z1)  --  bottom horizontal

vecdTovecf::Vector3 GLdouble -> Vector3 GLfloat
vecdTovecf (Vector3 x y z) = Vector3 x' y' z'
    where
        x' = rf x
        y' = rf y 
        z' = rf z 


{-|
   === KEY: points set for sphere
   The function from AronGraphic.hs spherePts
-}
spherePtsX::[[Vertex3 GLfloat]]
spherePtsX = geneParamSurface fx fy fz n
    where
        n = 20::Int
        δ = (2*pi)/(rf(n-1)) :: Float
        r = 0.1
        br = 0.2
        σ = 1/rf(n-1)

        fx::Int -> Int -> GLfloat
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*cos(α)*cos(β)
        fy::Int -> Int -> GLfloat
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*cos(α)*sin(β)
        
        fz::Int -> Int -> GLfloat
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*sin(α)


drawRectX2::G.Window -> IORef GlobalRef -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat) -> IO()
drawRectX2 w ioGlobalRef (p0@(Vertex3 x0 y0 z0), p1@(Vertex3 x1 y1 z1)) c@(x, y) = do
    preservingMatrix $ do
        tvec <- getTranVecDrawRectX ioGlobalRef
        (isPre, _) <- getMousePressed ioGlobalRef 
        drawRectMouse w tvec (p0, p1) (x, y, 0.0) isPre

drawRectMouse::G.Window -> (Vector3 GLdouble) -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat, GLfloat) -> Bool -> IO()
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
    let tvec' = Vector3 vx (-vy) vz 
    (winWidth, winHeight) <- G.getWindowSize w
    let (winW, winH) = (rf winWidth, rf winHeight)
    isIn <- isPtInsideRect w (p0 +: (d2f tvec'), p1 +: (d2f tvec')) (mx, my)
    drawRectColor (isPre && isIn ? green $ red) (p0, p1)


d2f::Vector3 GLdouble -> Vector3 GLfloat
d2f(Vector3 x y z) = Vector3 x' y' z'
    where
        x' = rf x
        y' = rf y 
        z' = rf z 

{-|
    === KEY: check whether point (x, y) is inside the rectangle

    @ 
                    | upLeftY
         upLeftX -> v +----------+ 
                                 |
                                 |
                      +          + 
                                 ^ <-  downRightX 
                                 |
                              downRightY                                    
                                                                             +- - -> translate Vector3
                                 + -> upLeft                                 |
                                 |                                           |
                                 |                     + -> downRight        |
                                 |                     |                     |                    +-> cursor pos
                                 ↓                     ↓                     ↓                    ↓       
    @

-}
isPtInsideRectTran::G.Window ->(Vertex3 GLfloat, Vertex3 GLfloat) -> Vector3 GLdouble -> IO Bool
isPtInsideRectTran w (p0, p1) (Vector3 a b c) = do
    cursorPos <- getCursorPosf w  -- IO (GLfloat, GLfloat)
    let tvec = Vector3 a (-b) c
    isPtInsideRect w (p0 +: (d2f tvec), p1 +: (d2f tvec)) cursorPos 

isPtInsideRect::G.Window -> (Vertex3 GLfloat, Vertex3 GLfloat) -> (GLfloat, GLfloat) -> IO Bool
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


{--
-- modifyIORef xx (\x -> x{str_ = str}) instead

setStr::GlobalRef -> String -> GlobalRef
setStr gf s = GlobalRef{ 
                str_ = s, 
                cursor_ = cursor, 
                xyzAxis_ = xyzaxis, 
                mousePressed_ = mp, 
                drawRectX_ = vx,
                tranDrawRectX_ = td,
                fovDegree_ = fov,
                drawPts_ = drawPts,
                randomPts_ = randomPts,
                randomWalk_ = rw
                }
  where
    xyzaxis = xyzAxis_ gf
    cursor = cursor_ gf
    mp = mousePressed_ gf
    vx = drawRectX_ gf
    td = tranDrawRectX_ gf
    fov= fovDegree_ gf
    drawPts = drawPts_ gf
    randomPts = randomPts_ gf
    rw = randomWalk_ gf
--}

{--
setDrawPts::GlobalRef -> [[Vertex3 GLfloat]] -> GlobalRef
setDrawPts gf cx = GlobalRef{ 
                str_ = str,
                cursor_ = cursor, 
                xyzAxis_ = xyzaxis, 
                mousePressed_ = mp, 
                drawRectX_ = vx,
                tranDrawRectX_ = td,
                fovDegree_ = fov,
                drawPts_ = cx,
                randomPts_ = rts,
                randomWalk_ = rw
                }
  where
    str = str_ gf
    xyzaxis = xyzAxis_ gf
    cursor = cursor_ gf
    mp = mousePressed_ gf
    vx = drawRectX_ gf
    td = tranDrawRectX_ gf
    fov= fovDegree_ gf
    rts= randomPts_ gf
    rw = randomWalk_ gf
--}

{--
setRandomPts::GlobalRef -> [Vertex3 GLfloat] -> GlobalRef
setRandomPts gf rpt = GlobalRef{ 
                str_ = str,
                cursor_ = cursor, 
                xyzAxis_ = xyzaxis, 
                mousePressed_ = mp, 
                drawRectX_ = vx,
                tranDrawRectX_ = td,
                fovDegree_ = fov,
                drawPts_ = dp,
                randomPts_ = rpt,
                randomWalk_ = rw
                }
  where
    str = str_ gf
    xyzaxis = xyzAxis_ gf
    cursor = cursor_ gf
    mp = mousePressed_ gf
    vx = drawRectX_ gf
    td = tranDrawRectX_ gf
    fov= fovDegree_ gf
    dp = drawPts_ gf
    rw = randomWalk_ gf
--}
  

{-|
    KEY: getter for fovDegree_ 
    NOTE: zoom in, zoom out
-}
getFOVDegree::IORef GlobalRef -> IO GLdouble 
getFOVDegree ioGlobalRef = readIORef ioGlobalRef >>= return . fovDegree_ 

{--
{-|
    KEY: getter for setFOVDegree 

    NOTE: zoom in, zoom out
-}
setFOVDegree::GlobalRef -> GLdouble -> GlobalRef
setFOVDegree gf fov = GlobalRef{
                  str_ = s, 
                  cursor_ = g, 
                  xyzAxis_ = xyzaxis, 
                  mousePressed_ = mp, 
                  drawRectX_ = vx,
                  tranDrawRectX_ = td,
                  fovDegree_ = fov,
                  drawPts_ = drawPts,
                  randomPts_ = randomPts,
                  randomWalk_ = rw
                  }
  where
    s = str_ gf
    g = cursor_ gf
    xyzaxis = xyzAxis_ gf
    mp = mousePressed_ gf
    vx = drawRectX_ gf
    td = tranDrawRectX_ gf
    drawPts = drawPts_ gf
    randomPts = randomPts_ gf
    rw = randomWalk_ gf
--}
  

{-|
    KEY: getter for str_ 
-}
getStr::IORef GlobalRef -> IO String
getStr ioGlobalRef = readIORef ioGlobalRef >>= return . str_

{--
{-|
    KEY: setter for xyzAxis_ 
-}
setXYZAxis::GlobalRef -> XYZAxis -> GlobalRef
setXYZAxis gf xyzAxis = GlobalRef{
                  str_ = s, 
                  cursor_ = g, 
                  xyzAxis_ = xyzAxis, 
                  mousePressed_ = mp, 
                  drawRectX_ = vx,
                  tranDrawRectX_ = td,
                  fovDegree_ = fov,
                  drawPts_ = drawPts,
                  randomPts_ = randomPts,
                  randomWalk_ = rw
                  }
  where
    s = str_ gf
    g = cursor_ gf
    mp = mousePressed_ gf
    vx = drawRectX_ gf
    td = tranDrawRectX_ gf
    fov = fovDegree_ gf
    drawPts = drawPts_ gf
    randomPts = randomPts_ gf
    rw = randomWalk_ gf
--}
  
{-|
    KEY: getter for xyzAxis_
-}
getXYZAxis::IORef GlobalRef -> IO XYZAxis
getXYZAxis ioGlobalRef = readIORef ioGlobalRef >>= return . xyzAxis_

{-|
    KEY: getter for drawPts_
-}
getDrawPts::IORef GlobalRef -> IO [[Vertex3 GLfloat]]
getDrawPts ioGlobalRef = readIORef ioGlobalRef >>= return . drawPts_
  
getRandomPts::IORef GlobalRef -> IO [Vertex3 GLfloat]
getRandomPts ioGlobalRef = readIORef ioGlobalRef >>= return . randomPts_
  
--setCursor::GlobalRef -> (GLfloat, GLfloat) -> GlobalRef
--setCursor gf pos = GlobalRef{str_ = s, cursor_ = pos, xyzAxis_ = xyz}
--    where
--      s = str_ gf
--      xyz = xyzAxis_ gf
{--
{-|
    KEY: setter for cursor_ 
-}
setCursor::(GLfloat, GLfloat) -> GlobalRef -> GlobalRef
setCursor pos gf = GlobalRef{
                    str_ = s, 
                    cursor_ = pos, 
                    xyzAxis_ = xyz, 
                    mousePressed_ = mp, 
                    drawRectX_ = vx,
                    tranDrawRectX_ = td,
                    fovDegree_ = fov,
                    drawPts_ = drawPts,
                    randomPts_ = randomPts,
                    randomWalk_ = rw
                    }
    where
      s = str_ gf
      xyz = xyzAxis_ gf
      mp = mousePressed_ gf
      vx = drawRectX_ gf
      td = tranDrawRectX_ gf
      fov = fovDegree_ gf
      drawPts = drawPts_ gf
      randomPts = randomPts_ gf
      rw = randomWalk_ gf
--}
--setMousePressed::Bool -> GlobalRef -> GlobalRef
--setMousePressed b gf = GlobalRef{str_ = s, cursor_ = cursor, xyzAxis_ = xyz, mousePressed_ = b} 
--    where
--      s = str_ gf
--      xyz = xyzAxis_ gf
--      cursor = cursor_ gf
{--
{-|
    KEY: setter for mousePressed_ 
-}
setMousePressed::(Bool, (GLfloat, GLfloat)) -> GlobalRef -> GlobalRef
setMousePressed (b, mpos) gf = GlobalRef{
                    str_ = s, 
                    cursor_ = cursor, 
                    xyzAxis_ = xyz, 
                    mousePressed_ = (b, mpos), 
                    drawRectX_ = vx,
                    tranDrawRectX_ = td,
                    fovDegree_ = fov,
                    drawPts_ = drawPts,
                    randomPts_ = randomPts,
                    randomWalk_ = rw
                    } 
    where
      s = str_ gf
      xyz = xyzAxis_ gf
      cursor = cursor_ gf
      vx = drawRectX_ gf
      td = tranDrawRectX_ gf
      fov= fovDegree_ gf
      drawPts = drawPts_ gf
      randomPts = randomPts_ gf
      rw = randomWalk_ gf
--}
  
{-|
    KEY: 
-}
getCursorPosf::G.Window -> IO (GLfloat, GLfloat)
getCursorPosf w = G.getCursorPos w >>= \(x, y) -> return $ (rf x, rf y) :: IO (GLfloat, GLfloat)

{-|
    KEY: 
-}
getMousePressed::IORef GlobalRef -> IO (Bool, (GLfloat, GLfloat)) 
getMousePressed ioGlobalRef = readIORef ioGlobalRef >>= return . mousePressed_

{-|
    KEY: 
-}
getCursor::IORef GlobalRef -> IO (GLfloat, GLfloat)
getCursor ioGlobalRef = readIORef ioGlobalRef >>= return . cursor_

{-|
    KEY: 
-}
getDrawRectX::IORef GlobalRef -> IO (Vertex3 GLfloat, Vertex3 GLfloat)
getDrawRectX ioGlobalRef = readIORef ioGlobalRef >>= return . drawRectX_ 


{--
incTranVecDrawRectX::(Vector3 GLdouble -> Vector3 GLdouble) -> GlobalRef -> GlobalRef
incTranVecDrawRectX f gf = GlobalRef{ 
                    str_ = s, 
                    cursor_ = cursor, 
                    xyzAxis_ = xyz, 
                    mousePressed_ = mp, 
                    drawRectX_ = vex, 
                    tranDrawRectX_  = f vec,
                    fovDegree_ = fov,
                    drawPts_ = drawPts,
                    randomPts_ = randomPts
                    }
    where
      s = str_ gf
      xyz = xyzAxis_ gf
      cursor = cursor_ gf
      mp = mousePressed_ gf
      vex = drawRectX_ gf
      vec = tranDrawRectX_ gf
      fov = fovDegree_ gf
      drawPts = drawPts_ gf
      randomPts = randomPts_ gf
--}
  
getTranVecDrawRectX::IORef GlobalRef -> IO (Vector3 GLdouble)
getTranVecDrawRectX ioGlobalRef = readIORef ioGlobalRef >>= return . tranDrawRectX_

{-|
    KEY:

    t0 pressed
       (x0, y0) 

    t1 released
       (x1, y1) 
-}
mouseCallback:: IORef GlobalRef -> G.MouseButtonCallback
mouseCallback globalRef window but butState mk = do
  case butState of
    G.MouseButtonState'Pressed -> do
      case but of
        v | v == G.MouseButton'1 -> do
              (fbw, fbh) <- G.getFramebufferSize window 
              pos <- G.getCursorPos window >>= \(x, y) -> return $ (rf x, rf y) :: IO (GLfloat, GLfloat)
              ws <- G.getWindowSize window
              let str = PR.printf "cx=%.2f cy=%.2f wx=%d wy=%d bx=%d by=%d" (fst pos) (snd pos) (fst ws) (snd ws) fbw fbh:: String

              gRef <- readIORef globalRef
              -- ↑ 
              -- +---------------------------+
              --                             ↓ 
              -- writeIORef globalRef $ setStr gRef str
              modifyIORef globalRef (\x -> x{str_ = str})
                
              -- newGlobalRef <- readIORef globalRef >>= return . setCursor pos 
              -- readIORef globalRef >>= return . setCursor pos >>= \x -> writeIORef globalRef $ setMousePressed (True, pos) x
              -- readIORef globalRef >>= return . setCursor pos >>= \x -> writeIORef globalRef $ setMousePressed (True, pos) x
              modifyIORef globalRef (\x -> x{cursor_ = pos})
              modifyIORef globalRef (\x -> x{mousePressed_ = (True, pos)})
              
              --  ↑ 
              --  +--------------------------------------------------+
              --                                                     ↓  
              -- writeIORef globalRef $ setMousePressed (True, pos) newGlobalRef 

              pp str
          | otherwise            -> pp "No button pressed"
    G.MouseButtonState'Released -> do
      -- pos <- G.getCursorPos window >>= \(x, y) -> return $ (rf x, rf y) :: IO (GLfloat, GLfloat)
      let pos = (0.0, 0.0)
      -- readIORef globalRef >>= \x -> writeIORef globalRef $ setMousePressed (False, pos) x
      modifyIORef globalRef (\x -> x{mousePressed_ = (False, pos)})
  
      pp "Button Released"


{-|
    KEY:
    NOTE: USED NOW, green triangle
-}
drawTriangleMouse::G.Window -> IO()
drawTriangleMouse w = do
    let v0 = Vertex3 0   0.6 0
    let v1 = Vertex3 0.6 0.6 0
    let v2 = Vertex3 0.5 0.1 0
    (winW, winH) <- G.getWindowSize w >>= \(x, y) -> return (rf x, rf y)
    (xx, yy) <- getCursorPosf w
    cursorVex <- screenCSToGraphicCS w (xx, yy)
    logFileG ["CursorPos_xx=" ++ show xx, "cursorPos_yy=" ++ show yy]
    logFileG ["cursorVex=" ++ show cursorVex]
    let b = ptInsidePolygon cursorVex [v0, v1, v2] 
    drawTriangleVex (b ? blue $ green) (v0, v1, v2) 

{-|
    green triangle
-}
drawTriangleClick::G.Window ->(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> IO()
drawTriangleClick w (v0, v1, v2) = do
--    let v0 = Vertex3 0   0.6 0
--    let v1 = Vertex3 0.6 0.6 0
--    let v2 = Vertex3 0.5 0.1 0
    (winW, winH) <- G.getWindowSize w >>= \(x, y) -> return (rf x, rf y)
    (xx, yy) <- getCursorPosf w
    cursorVex <- screenCSToGraphicCS w (xx, yy)
    logFileG ["CursorPos_xx=" ++ show xx, "cursorPos_yy=" ++ show yy]
    logFileG ["cursorVex=" ++ show cursorVex]
    let b = ptInsidePolygon cursorVex [v0, v1, v2] 
    drawTriangleVex (b ? blue $ green) (v0, v1, v2)
  
{-|
    green polygon, LineLoop can not be filled with color
    NOTE: Non filled color
-}
drawPolygonClick::G.Window -> [Vertex3 GLfloat] -> IO()
drawPolygonClick w cx = do
    (winW, winH) <- G.getWindowSize w >>= \(x, y) -> return (rf x, rf y)
    (xx, yy) <- getCursorPosf w
    cursorVex <- screenCSToGraphicCS w (xx, yy)
    let b = ptInsidePolygon cursorVex cx
    drawPrimitiveVex LineLoop (b ? blue $ green) cx
  

{-|
    KEY:
    NOTE: USED
-}
drawRectFillColorClick::G.Window -> [Vertex3 GLfloat] -> IO()
drawRectFillColorClick w vex = do
    (winW, winH) <- G.getWindowSize w >>= \(x, y) -> return (rf x, rf y)
    (xx, yy) <- getCursorPosf w
    cursorVex <- screenCSToGraphicCS w (xx, yy)
    logFileG ["cursorVex=" ++ show cursorVex]
    let b = ptInsidePolygon cursorVex vex
    drawPrimitiveVex Polygon (b ? green $ blue) vex
  
{--
Fri 24 Mar 10:46:03 2023 
DELETE IT
show3dStr::String -> Color3 GLdouble ->  GLdouble -> IO()
show3dStr str c u = do
    preservingMatrix $ do
        translate (Vector3 u 0 0 :: Vector3 GLdouble)
        rotate (90)$ (Vector3 0 0 1 :: Vector3 GLdouble)
         -- coordTip c
    preservingMatrix $ do
        translate (Vector3 (-u) (-u) 0 :: Vector3 GLdouble)
        GL.scale (1/4000 :: GL.GLdouble) (1/4000::GL.GLdouble) 1
        -- Ref: https://hackage.haskell.org/package/gloss-rendering-1.13.1.1/docs/src/Graphics.Gloss.Internals.Rendering.Picture.html#renderPicture
        -- text looks weird when we have got blend on
        GL.blend $= GL.Disabled        
        preservingMatrix $ GLUT.renderString GLUT.Roman str
        GL.blend $= GL.Enabled
--}

{-| 
    === KEY: screen coordinates-system to graphic coordinates-system

    @
        (x, y) <- getCursorPosf w = G.getCursorPos w >>= \(x, y) -> return $ (rf x, rf y)
        screen Coordinates-System

        [winW = 1000, winH = 1000]
        topLeft
         ↓ (0,0) 
         + - - - ->  winW
         |
         |
         |
         v winH

        Scale XY-axes to [0, 1.0]
        (winW, winH) <- G.getWindowSize w >>= \(u, v) -> return (rf u, rf v)

        (x/winW, y/winH)

         topLeft
         ↓ (0,0)
         + - - - ->  1.0 
         |
         |
         |
         v 1.0 


         Move (0,0) => (0, 0.5) 
         (x/winW, y/winH - 0.5)
         topLeft
         ↓
         + -     ->  1.0 
         |                 
 (0,0)   + - - - -> 
         |
         v 1.0 

         Flip Y-Axis
         (x/winW, (0.5 - y/winH))

         ^
         | 
         |
         + - - - ->
         |
         |

         ↑ 
         bottomLeft
         
         Move (0,0) to (0, 0.5), 0.5 on X-axis
         (0.5, 0.5) => (0.0, 0.0)

         (x/winW - 0.5,  0.5 - y/winH)

                 ^
                 |
                 | 
                 | (0,0)
          -------+------->
                 |
                 |
                 |
                 

         Test 1, x = winW/2, y=winH/2 => (0, 0)       ✓ 
         Test 2, x = winW,   y=winH   => (0.5, -0.5)  ✓ 
         Test 3, x = (1/4)winW, y= (1/4) winH => (0.25 - 0.5, 0.5 - 0.25) = (-0.25, 0.25)  ✓

    @
-} 
screenCSToGraphicCS::G.Window -> (GLfloat, GLfloat) -> IO (Vertex3 GLfloat)
screenCSToGraphicCS ws (wpx, hpx) = do
    let ndcWidth = 4.0
    (winW, winH) <- G.getWindowSize ws >>= \(u, v) -> return (rf u, rf v)
    let cenx = ndcWidth/2.0
    let ceny = ndcWidth/2.0
    -- [0, ndcWidth] - (ndcWidth/2)
    -- x: [0, 2] - 1 => [-1, 1], move (0, 0) => (1, 0) 
    -- y: [0, 2] - 1 => [-1, 1], flip Y-axis => -1*[-1, 1] => [1, -1] 
    let x' = ndcWidth/winW
    let y' = ndcWidth/winH
    return $ Vertex3 (x' * wpx - cenx) (negate(y' * hpx - ceny)) 0.0


normDist::(GLfloat, GLfloat) -> (GLfloat, GLfloat) -> GLfloat
normDist (x0, y0) (x1, y1) = (x1 - x0)^2 + (y1 - y0)^2

drawConvexHull::[Vertex3 GLfloat] -> IO()
drawConvexHull pts = do
    let n = len pts
    let ls = convexHull n pts
    mapM_ (\x -> drawDot x) pts
    mapM_ (\x -> drawSegmentWithEndPt red x) ls

-- listTupeToList::[(Vertex3 GLfloat, Vertex3 GLfloat)] -> [[Vertex3 GLfloat]]
-- listTupeToList cx = map (\x -> ((fromJust . head) cx, (fromJust . last) cx) ) cx

seg::Vertex3 GLfloat -> Vertex3 GLfloat -> [Vertex3 GLfloat]
seg v0 v1 = cmpVex v0 v1 ? [v0, v1] $ [v1, v0]

findAllChildren::Vertex3 GLfloat -> [(Vertex3 GLfloat, Vertex3 GLfloat)] -> [Vertex3 GLfloat]
findAllChildren v cx = unique $ map fromJust $ filter (\x -> x /= Nothing ) $ map (\(v0, v1) -> v == v0 ? Just v1 $ (v == v1 ? Just v0 $ Nothing)) cx

  
{-|
     === KEY: combine a function and a function 
     @
     let f = \x -> x * x
     let g = \x -> x * sin x
     combineListAndCurve f (-0.5, 0.5) g (-0.2, 0.3) 
     @
-}
combineCurveAndCurve :: Integer -> (GLfloat -> GLfloat) -> (GLfloat, GLfloat) -> (GLfloat -> GLfloat) -> (GLfloat, GLfloat) -> [(GLfloat, GLfloat)]
combineCurveAndCurve n f (a, b) g (a', b') = pxf ++ pxg
  where
    del = (b - a) / (fromIntegral n)
    pt = [a, a + del .. b] :: [GLfloat]
    del' = (b' - a') / (fromIntegral n)
    pt' = [a', a' + del' .. b'] :: [GLfloat]
    off = a' - b
    -- pxf = map (\x -> Vertex3 x (f x) 0) pt
    pxf = map (\x -> (x,  f x)) pt
    g' = \x -> g (x + off) + (f b) - (g a')
    -- pxg = map (\x -> Vertex3 x (g' x) 0) $ map (\x -> x - off) pt'
    pxg = map (\x -> (x, g' x)) $ map (\x -> x - off) pt'
  
{-|
     === KEY: combine list of tuple and a function

     @ 
     let cx = map (\x -> x * x) [-0.3, -0.3 + 0.1 .. 0.6]
     let f = \x -> x * sin x
     combineListAndCurve cx f (-0.5, 0.5)
     @
-}
combineListAndCurve :: Integer -> [(GLfloat, GLfloat)] -> (GLfloat -> GLfloat) -> (GLfloat, GLfloat) -> [(GLfloat, GLfloat)]
combineListAndCurve n cx g (a, b) = combineListAndList cx cy
  where
    del = (b - a) / (fromIntegral n)
    pt = [a, a + del .. b]
    cy = map (\x -> (x, g x)) pt

{-|
    === KEY: combine two list of tupels

     @
     let f = \x -> x * x
     let g = \x -> x * sin x
     let cx = map (\x -> (x, f x)) [-0.3, -0.3 + 0.1 .. 0.6]
     let cy = map (\x -> (x, g x)) [-0.2, -0.2 + 0.1 .. 0.5]
     combineListAndList cx cy
     @
-}
combineListAndList :: [(GLfloat, GLfloat)] -> [(GLfloat, GLfloat)] -> [(GLfloat, GLfloat)]
combineListAndList cx cy = cx ++ cy'
  where
    (x0, y0) = if len cx > 0 then last cx else (0, 0)
    (x1, y1) = if len cy > 0 then head cy else (0, 0)
    off = x0 - x1
    cy' = map (\(x, y) -> (x + off, y + (y0 - y1)) ) cy

combineCurveAndList :: Integer -> (GLfloat -> GLfloat) -> (GLfloat, GLfloat) -> [(GLfloat, GLfloat)] -> [(GLfloat, GLfloat)]
combineCurveAndList n f (a, b) cy = combineListAndList cx cy
  where
    del = (b - a) / (fromIntegral n)
    pt = [a, a + del .. b]
    cx = map (\x -> (x, f x)) pt
      
{-|
    === KEY: Generate a set of parabolic (Vertex3 x y 0) pairs

    Ref formula
    http://localhost/html/indexCalculatethefallingobjectusingNewtonLaw.html
-}
parabolicPath::[Vertex3 GLfloat]
parabolicPath = curvePtK f (-1.0, 1.0) 200
    where
        v0 = 5.0 :: GLfloat
        g  = 9.8 :: GLfloat
        -- f t = v0*t - 0.5 * g * t**2
        f t = v0*(t + v0/g) - 0.5 * g * (t + v0/g)**2

{--  
movingPath::[Vertex3 GLfloat]
movingPath = map (\(x, y) -> Vertex3 x y 0) $ combineCurveAndCurve 30 f t1 g t2 
    where
      t1 = (-0.2, 0.2) :: (GLfloat, GLfloat)
      t2 = (-0.2, 0.2) :: (GLfloat, GLfloat)
      f = \x -> -1.5*(x - 0.2) * (x + 0.2)
      g = \x -> f x
--}

{--
    [ 3 
      [ 2
         [1
           [0]
           [1, 0]
         ]
         [2, 1, 0]
      ]
      [3 2 1 0]
    ]


    [3
      [2
        [1
          [0
            []
            [0]
          ]
          [0 1]
        ]
        [0 1 2]
      ]
    [0 1 2 3]
   ]

--}
  

io :: IO a -> CMS.StateT (Vertex3 GLfloat) IO a
io = liftIO
  
moveNext :: Vertex3 GLfloat -> IO (Vertex3 GLfloat)
moveNext (Vertex3 x y z) = do
  n <- randomInt 1 4
  let b = 0.8 :: GLfloat
  v <- case n of
            1 -> if x < b then let v = Vertex3 (x + 0.1) y z in return v else moveNext (Vertex3 x y z)
            2 -> if x > -b then let v = Vertex3 (x - 0.1) y z in return v else moveNext (Vertex3 x y z)
            3 -> if y < b then let v = Vertex3 x (y + 0.1) z in return v else moveNext (Vertex3 x y z)
            4 -> if y > -b then let v = Vertex3 x (y - 0.1) z in return v else moveNext (Vertex3 x y z)
            _ -> error "ERROR randomInt"
  return v
  
randomVexList :: Vertex3 GLfloat -> [Int] -> [Vertex3 GLfloat]
randomVexList _ [] = []
randomVexList v@(Vertex3 x y z) (c:cx) = case c of
                                            1 -> x < 1.0  ? (let u = Vertex3 (x + 0.1) y z in u:(randomVexList u cx)) $ randomVexList v cx
                                            2 -> x > -1.0 ? (let u = Vertex3 (x - 0.1) y z in u:(randomVexList u cx)) $ randomVexList v cx
                                            3 -> y < 1.0  ? (let u = Vertex3 x (y + 0.1) z in u:(randomVexList u cx)) $ randomVexList v cx
                                            4 -> y > -1.0 ? (let u = Vertex3 x (y - 0.1) z in u:(randomVexList u cx)) $ randomVexList v cx
                                            _ -> error "ERROR randomInt"


{-|
                (x0, y0)

                    +--
                    |
                         |
                        -+ (-x0, -y0)


                    boundary    init pt     random list   next move list
                       |            |            |             |
-}
randomVexListX :: (Int, Int) -> (Int, Int) -> [Int] -> [(Int, Int)]
randomVexListX _ _ [] = []
randomVexListX  ix@(x0, y0) v@(x, y) (c:cx) = case c of
                                            1 -> x > x0  ? (let u = (x - 1, y) in u:(randomVexListX ix u cx)) $ randomVexListX ix v cx
                                            2 -> x < -x0 ? (let u = (x + 1, y) in u:(randomVexListX ix u cx)) $ randomVexListX ix v cx
                                            3 -> y < y0  ? (let u = (x, y + 1) in u:(randomVexListX ix u cx)) $ randomVexListX ix v cx
                                            4 -> y > -y0 ? (let u = (x, y - 1) in u:(randomVexListX ix u cx)) $ randomVexListX ix v cx
                                            _ -> error "ERROR randomInt"
  
drawFirstRect :: IO()
drawFirstRect  = do
  let wx = 0.02; wy = 0.02
  let v (x, y, z) = Vertex3 x y z
  -- top left corner
  let x0 = -1.0; y0 = 1.0
  -- bottom right corner
  let x1 = 1.0; y1 = -1.0
  let vx0 = v(x0, y0, 0)
  let vx1 = v(x1, y1, 0)
  let delx = abs $ (x1 - x0) / 20
  let dely = abs $ (y1 - y0) / 20
  let cx = delx / 2
  let cy = dely / 2
  mapM_(\ny -> mapM_(\nx -> let ex0 = x0 + nx * delx + wx
                                ey0 = y0 - ny * dely - wy
                                ex1 = x0 + (nx + 1) * delx - wx
                                ey1 = y0 - (ny + 1) * dely + wy
                            in  drawRectColor yellow (v(ex0, ey0, 0), v(ex1, ey1, 0))
                    ) [0] ) [0]
  
drawRectGrid :: [Int] -> [Int] -> GLfloat -> IO()
drawRectGrid sx sy e = do
      let wx = e; wy = e
      let v (x, y, z) = Vertex3 x y z
      -- top left corner
      let x0 = -1.0 :: GLfloat; y0 = 1.0 :: GLfloat
      -- bottom right corner
      let x1 = 1.0; y1 = -1.0
      let vx0 = v(x0, y0, 0)
      let vx1 = v(x1, y1, 0)
      let delx = (x1 - x0) / 20.0
      let dely = (y0 - y1) / 20.0
      drawRect (vx0, vx1)
      mapM_(\ny -> mapM_(\nx -> let ex0 = x0 + (fi $ nx) * delx + wx
                                    ey0 = y0 - (fi $ ny) * dely - wy
                                    ex1 = x0 + (fi $ nx + 1) * delx - wx
                                    ey1 = y0 - (fi $ ny + 1) * dely + wy
                                in  drawRectColor green (v(ex0, ey0, 0), v(ex1, ey1, 0))
                        ) sx ) sy
  
moveSqure :: IO ()
moveSqure = do
 preservingMatrix $ do
   let vf = Vertex3 (-1.0) 1.0 0.0 :: (Vertex3 GLfloat)
   let vv = Vertex3 0.0 0.0 0.0 -: vf
   translate vv
   drawRectGrid [0] [0] 0.02
  
  
movingPath :: [(GLfloat, GLfloat)]
movingPath = ls
  where
    c = combineListAndList
    ff = \x -> -(8.0)*x * x
    f = \x -> ff (x + 1)
    g = f
    h = f
    t1 = (-0.3 - 1, 0.3 - 1)
    t2 = t1
    t3 = t1
    n = 40
    del = (snd t1 - fst t1) / (fromIntegral n)
    lf = map (\x -> (x, f x)) [fst t1, (fst t1) + del .. snd t1]
    lg = map (\x -> (x, g x)) [fst t2, (fst t2) + del .. snd t2]
    lh = map (\x -> (x, h x)) [fst t3, (fst t3) + del .. snd t3]
    ls = c lh $ c lh (c lf lg)
  
movingPathVex :: [Vertex3 GLfloat]
movingPathVex = map (\(x, y) -> Vertex3 x y (0:: GLfloat)) $ c (c cx xc) (c cxr xd)
  where
    c = combineListAndList
    fp (a, b) = (b, a)
    n = 30
    t1@(a, b) = (-0.8, 0.2)
    del = (b - a) / (fromIntegral n)
    cx =            map (\(x0, y0)-> (x0 - 0.8, y0)) $ map (\x -> fp (x, 0.6)) [a, a + del .. b]
    cxr = reverse $ map (\(x0, y0)-> (x0 - 0.8, y0)) $ map (\x -> fp (x, 0.6)) [a, a + del .. b]
    cy = movingPath
    xc = map (\x -> (x, 0.3 :: GLfloat)) [a, a + del .. b]
    xd = reverse xc
  
mainLoop :: G.Window ->
            IORef Cam ->
            IORef Step ->
            IORef GlobalRef ->
            IORef FrameCount ->
            [[Vertex3 GLfloat]]-> IO ()
mainLoop w refCam refStep refGlobal refCount lssVex = unless' (G.windowShouldClose w) $ do
    (width, height) <- G.getFramebufferSize w
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    GL.clear [ColorBuffer, DepthBuffer]

    G.setKeyCallback w (Just $ keyBoardCallBack2 refStep refGlobal)    -- AronOpenGL
    G.setMouseButtonCallback w (Just $ mouseCallback refGlobal)  -- mouse event
    -- lightingInfo
    loadIdentity  -- glLoadIdentity
    -- view matrix: http://xfido.com/html/indexUnderstandOpenGL.html
    -- matrixMode $= Modelview 0
  
    -- A matrix stack.
    --
    --data 
    -- MatrixMode =
    --     Modelview GLsizei  -- ^ The modelview matrix stack of the specified vertex unit.
    --   | Projection         -- ^ The projection matrix stack.
    --   | Texture            -- ^ The texture matrix stack.
    --   | Color              -- ^ The color matrix stack.
    --   | MatrixPalette      -- ^ The matrix palette stack.
    --   deriving ( Eq, Ord, Show )

    -- projection matrix stack
    -- glMatrixModel(GL_PROJECTION)

    -- matrixMode $= Modelview 0 
--    let fovy = 80.0 
--        aspect = 1.0 
--        zNear = 2.0 
--        zFar = (-2)
--        in GM.perspective fovy aspect zNear zFar

    -- matrixMode $= Modelview 0
    -- loadIdentity

{--
    KeyBoard Character
    URL: https://hackage.haskell.org/package/GLFW-b-3.3.0.0/docs/Graphics-UI-GLFW.html

    @
    lookAt :: Vertex3 GLdouble -> Vertex3 GLdouble -> Vector3 GLdouble -> IO ()
    lookAt (Vertex3 eyeX    eyeY    eyeZ)
       (Vertex3 centerX centerY centerZ)
       (Vector3 upX     upY     upZ) =
    gluLookAt eyeX eyeY eyeZ centerX centerY centerZ upX upY upZx

    data XYZAxis = XYZAxis{xa::Bool, ya::Bool, za::Bool}                      deriving(Show)
    @
--}
    -- modelview matrix stack
    -- matrixMode $= Modelview 0

    step <- readIORef refStep
    xyzAxis <- getXYZAxis refGlobal
    fovNew <- getFOVDegree refGlobal
    case xyzAxis of
      --                                 +---> YZ-plane
      --                                 ↓
      var | xa var -> GL.lookAt (Vertex3 1.0 0 0::Vertex3 GLdouble) (Vertex3 0 0 0::Vertex3 GLdouble) (Vector3 0 1 0::Vector3 GLdouble)
  
          --                               +---> XZ-plane
          --                               ↓
          | ya var -> GL.lookAt (Vertex3 0 1.0 0::Vertex3 GLdouble) (Vertex3 0 0 0::Vertex3 GLdouble) (Vector3 1 0 0::Vector3 GLdouble)

          --                                 +---> XY-plane
          --                                 ↓
          | za var -> do 
            -- Graphics.Rendering.OpenGL.GLU.Matrix perspective :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

            -- glFrustum describes a perspective matrix that produces a perspective projection. (left,bottom,-near) and (right,top,-near) specify the points on the near clipping plane that
            -- are mapped to the lower left and upper right corners of the window, respectively, assuming that the eye is located at (0, 0, 0). -far specifies the location of the far clipping
            -- plane. Both near and far must be positive. The corresponding matrix is

            let zf = 0.5 
            -- perspective (field of view) width/height zNear zFar
            -- perspective 90 1.0 zf (zf + 4.0) 
            -- perspective 126.934 1.0 zf (zf + 4.0) 
            -- zoom in, zoom out
            perspective fovNew 1.0 zf (zf + 4.0) 
            -- GL.lookAt (Vertex3 0 4 1.0::Vertex3 GLdouble) (Vertex3 0 4 0::Vertex3 GLdouble) (Vector3 0 1 0::Vector3 GLdouble)
            GL.lookAt (Vertex3 0 0 1.0::Vertex3 GLdouble) (Vertex3 0 0 0::Vertex3 GLdouble) (Vector3 0 1 0::Vector3 GLdouble)
          | otherwise -> do
              GM.lookAt (Vertex3 0.2 0.2 0.2::Vertex3 GLdouble) (Vertex3 0 0 0::Vertex3 GLdouble) (Vector3 0 1 0::Vector3 GLdouble)
              keyboardRot refCam refStep (fromIntegral width) (fromIntegral height)

              preservingMatrix $ do
                loadIdentity
                fw "loadIdentity"
                ls <- getModelviewMatrix
                let lss = partList 4 ls
                printMat lss
                GM.lookAt (Vertex3 0 2 3::Vertex3 GLdouble) (Vertex3 0 0 0::Vertex3 GLdouble) (Vector3 0 1 0::Vector3 GLdouble)
                fw "getModelviewMatrix"
                lr <- getModelviewMatrix
                let lt = partList 4 lr
                printMat lt

    -- AronOpenGL.hs
    -- delta refStep to modify Cam{xx, yy, zz} in degree
    -- keyboardRot => rotate around {x-axis, y-axis, y-axis} in some degrees
    -- keyboardRot refCam refStep (fromIntegral width) (fromIntegral height)
    renderCoordinates
    -- show3dStr "1234" red 0.8

    curStr <- getStr refGlobal 
    show3dStr curStr red 0.8
    logFileG ["str_=" ++ (show curStr)]

    vx <- getDrawRectX refGlobal  -- (p0, p1) => (upLeft, bottomRight)

    cursorPos <- getCursor refGlobal

    (isPressed, (mx0, my0))<- getMousePressed refGlobal 
    let pos = (mx0, my0) 
    logFileG ["isPressed=" ++ (show isPressed) ++ " pos=" ++ (show pos)]

    -- TODO: add isPtInsideRect here
    tranVec <- getTranVecDrawRectX refGlobal 
    (p0, p1) <- getDrawRectX refGlobal
    -- isInside <- isPtInsideRect w (p0 +: (d2f tvec'), p1 +: (d2f tvec')) (x, y)
    isInTran <- isPtInsideRectTran w (p0, p1) tranVec
    when (isPressed && isInTran) $ do

        (width, height) <- G.getFramebufferSize w
        (winWidth, winHeight) <- G.getWindowSize w

        (nx1, ny1) <- G.getCursorPos w >>= \(x, y) -> return $ (rf x, rf y) :: IO (GLfloat, GLfloat)
        logFileG ["mx0=" ++ (show mx0) ++ " my0=" ++ (show my0) ++ " nx1=" ++ (show nx1) ++ " ny1=" ++ (show ny1)]
        (Vector3 tx ty tz) <- getTranVecDrawRectX refGlobal
        logFileG ["getTranVecDrawRectX=" ++ (show (Vector3 tx ty tz))]
        let (x', y') = (rf $ (nx1 - mx0)/(rf winWidth), rf $ (my0 - ny1)/(rf winHeight))
        -- readIORef refGlobal >>= \x -> writeIORef refGlobal $ setTranVecDrawRectX (Vector3 (x') (y') 0.0) x
        let vec = Vector3 x' y' 0.0
        -- readIORef refGlobal >>= \x -> writeIORef refGlobal $ incTranVecDrawRectX (+vec) x
        modifyIORef refGlobal (\x -> x{tranDrawRectX_ = (tranDrawRectX_ x) + vec})
        -- update current cursor position
        -- readIORef refGlobal >>= \x -> writeIORef refGlobal $ setMousePressed (isPressed, (nx1, ny1)) x
        modifyIORef refGlobal (\x -> x{mousePressed_ = (isPressed, (nx1, ny1))})
  
        pp "ok"
            
    vex <- getDrawRectX refGlobal

  
    rls <- randomFloat 10
    let cx = [Vertex3 0.1 0.1 0, Vertex3 0.2 0.1 0, Vertex3 0.2 (-0.1) 0, Vertex3 (-0.1) (-0.1) 0]
    let cl= let n = 40::Int
                δ = (2*pi)/(rf(n-1)) :: Float
                r = 0.3
                br = 0.2
                σ = 1/rf(n-1)

                fx::Int -> Int -> GLfloat
                fx i j = let i' = rf i
                             j' = rf j
                         in (1/ rf 40)*i'

                fy::Int -> Int -> GLfloat
                fy i j = let i' = rf i
                             j' = rf j
                             n = 3
                         in r * cos(δ * i')

                fz::Int -> Int -> GLfloat
                fz i j = let i' = rf i
                             j' = rf j
                         in r*sin(δ * i')

            in [[Vertex3 (fx i i) (fy i i) (fz i i) | i <- [1..n]] | j <- [1..n]]

    ls3 <- (en "b" >>= \x -> readGLScript ("/tmp/draw.x"))
    pre ls3
    when True $ do  
      (index, isNext, currRef) <- readRefFrame2 refCount 1000
      --                                                  | 
      --                                                  + -> speed, larger = slower
      movSphere <- getDrawPts refGlobal

      -- pre movSphere
  
      logFileG ["index=" ++ (show index)]
      pp $ " index= " ++ (show index)

      -- when (isNext && index < len (parabolicPath ++ throwUpPath)) $ do
      -- xx1
      drawSegmentFromTo green movingPathVex
      lt <- readIORef refGlobal >>= return . randomWalk_
      logFileG $ map show lt


              
-- /Users/aaa/myfile/bitbucket/tmp/xx_3937.x
      -- let rw = randomWalk_ globalRx
      if index < len lt then do
        preservingMatrix $ do
          -- let vx = throwUpPath !! inx
          let x0 = lt !! index
          -- affine geometry: vertex - vertex => vector
          let v = x0 -: Vertex3 0 0 0
          translate v
        
-- /Users/aaa/myfile/bitbucket/tmp/xx_8590.x
        
          -- drawSurfaceFromList movSphere
          moveSqure
          writeIORef refCount currRef
      else do
        let vx = lt !! (index - 1)
        cx <- randomIntList 10 (1, 4)
        modifyIORef refGlobal (\x ->x{randomWalk_ = randomVexList vx cx})
        -- writeIORef refCount currRef
        resetRefFrame refCount
        pp "ok"
-- C-; BACKUP, insertContent /Users/aaa/myfile/bitbucket/tmp/xx_6507.x
  
-- C-; BACKUP insertContent /Users/aaa/myfile/bitbucket/tmp/xx_2440.x drawComplex
  
    -- xxx
    when True $ do
      drawRectGrid [0..19] [0..19] 0.01
  
    let fn = "/tmp/c5.x"
    fb <- fExist fn
    when fb $ do
      let cur = (500, 500::GLfloat)
      curVex <- screenCSToGraphicCS w cur
      logFileG ["GraphicCS=" ++ show curVex ++ " cursorCS=" ++ show cur]

      let cur1 = (600, 600::GLfloat)
      curVex1 <- screenCSToGraphicCS w cur1
      logFileG ["GraphicCS=" ++ show curVex1 ++ " cursorCS=" ++ show cur1]

      let cur2 = (400, 600::GLfloat)
      curVex2 <- screenCSToGraphicCS w cur2
      logFileG ["GraphicCS=" ++ show curVex2 ++ " cursorCS=" ++ show cur2]

    -- drawSphere

--    let v0 = Vertex3 0   0.6 0
--    let v1 = Vertex3 0.6 0.6 0
--    let v2 = Vertex3 0.5 0.1 0

    -- drawTriangleMouse w
{--
    drawTriangleClick w $ let v0 = Vertex3 0    0    0 
                              v1 = Vertex3 0    0.1  0
                              v2 = Vertex3 0.1  0.1  0
                          in (v0, v1, v2)
--}
  
{--
    -- draw grid
    mapM_ (\y -> do
      mapM_ (\x -> do 
        drawPolygonClick w  $ let v0 = Vertex3 (0 + x)    (0 + y)   0 
                                  v1 = Vertex3 (0 + x)    (0.1 + y) 0
                                  v2 = Vertex3 (0.1 + x)  (0.1 + y) 0
                                  v3 = Vertex3 (0.1 + x)  (0 + y)   0
                              in [v0, v1, v2, v3]
            ) [0, 0.15 .. 1]
          ) [0, 0.15 .. 1]

    mapM_ (\y -> do
      mapM_ (\x -> do 
        drawRectFillColorClick w$ let v0 = Vertex3 (0   + x - 2.0)  (0   + y) 0
                                      v1 = Vertex3 (0   + x - 2.0)  (0.1 + y) 0
                                      v2 = Vertex3 (0.1 + x - 2.0)  (0.1 + y) 0
                                      v3 = Vertex3 (0.1 + x - 2.0)  (0   + y) 0
                                  in [v0, v1, v2, v3]
            ) [0, 0.15 .. 1]
         ) [0, 0.15 .. 1]
--}
  
    -- testmultMatrix

{--
    let vv0 = Vertex3 1   (negate 1  ) 0.0
    let vv1 = Vertex3 1.8 (negate 1.5) 0.0
    let v = [vv0, vv1]
    drawSegmentWithEndPt red v
    -- perpenLine::GLfloat -> GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> [Vertex3 GLfloat]
    let vls = perpenLine 0.9 (-0.2) vv0 vv1
    
    drawSegmentWithEndPt green vls

    let p0 = Vertex3 0 0 0
    let p1 = Vertex3 1 0 0
    let q0 = Vertex3 1 0 0
    let q1 = Vertex3 2 0 0
    let ret = intersectLine p0 p1 q0 q1
    print ret
  
    let xp0 = Vertex3 0 0 0
    let xp1 = Vertex3 1 0 0
    let xq0 = Vertex3 1 0 0
    let xq1 = Vertex3 1 1 0
    let xret = intersectLine xp0 xp1 xq0 xq1
    print xret
  
    let up0 = Vertex3 0 0    0
    let up1 = Vertex3 1 1    0
    let uq0 = Vertex3 1 0    0
    let uq1 = Vertex3 2 (-1) 0
    let uret = intersectLine up0 up1 uq0 uq1
    print uret


    let q0 = Vertex3 0 0 0
    let q1 = Vertex3 0.8 0.8 0
    let q2 = Vertex3 1.5 0.5 0
    let pt = threePtCircle q0 q1 q2
    print pt
    let c0 = case pt of
                  Just x  -> x
                  Nothing -> (Vertex3 0 0 0)
--}
  
    -- drawCircle2 c0 $ rf $ distX c0 q1
{--
    drawCircleThreePt q0 q1 q2 green

    drawSegmentWithEndPt green [q0, q1]
    drawSegmentWithEndPt blue  [q1, q2]
  
    drawSegments red  let v0 = Vertex3 0.1 (-0.1) 0
                          v1 = Vertex3 0.2 0.1    0
                          v2 = Vertex3 0.3 0.4    0
                          v3 = Vertex3 0.6 0.2    0
                          ls = [v0, v1, v2, v3, v0]
                      in join $ zipWith (\x y -> [x, y]) (init ls) (tail ls)

    let xs = [Vertex3 0 0 0, Vertex3 0.5 0 0, Vertex3 0.5 0.5 0, Vertex3 0 0.5 0]
--}
  

    -- BEG_triangulation
    -- drawConvexHull randomPts
    -- convexHullAllSeg::[Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
    randomPts <- getRandomPts refGlobal
    let lsx = convexHullAllSeg randomPts
    let f1 = \(a, b) (a', b') -> (a == a' && b == b') || (a == b' && b == a')
    let lsz = qqsort f1 lsx
    let lsy = map head $ DL.groupBy f1 lsz
    let ls = unique lsx
    mapM_(\vx -> do 
                drawSegment green vx 
                -- threadDelay 500
         ) lsy
    let lt = sortVex $ map(\t -> [fst t, snd t]) ls
    
    -- rsync: rsync_publicfile.sh 
    -- http://xfido.com/publicfile/codedoc/h1.txt
    -- vertex v to all children
    let toAllChildren = map (\v -> (v, unique $ qqsort cmpVex $ map fromJust $ filter (Nothing /=) $ map(\seg -> vexOnSeg v seg ? Just (otherEnd v seg) $ Nothing ) lt)) randomPts
              where vexOnSeg v [x, y] = v == x || v == y
                    otherEnd v [x, y] = v == x ? y $ x

    -- KEY: all children, all vertexes, last vertex
    -- One vertex to all chidlren
    -- BEG_last_pt
    -- Take the last vertex in toAllChildren

{--
     toAllChildren

            + -> vertexPt
            ↓ 
    ( Vertex3 1.3 0.12 0.0
    ,
        [ Vertex3 0.1 0.1 0.0
        , Vertex3 0.12 0.8 0.0
        , Vertex3 0.2 0.6 0.0
        , Vertex3 0.25 0.34 0.0
        , Vertex3 0.88 0.9 0.0
        ]
              ↑
              + allChildren
    )
--}

    let fb = toAllChildren !! 5
    let vertexPt = fst fb
    let allChildren = snd fb
    -- mapM_ (\y -> drawSegments cyan [fst fb, y]) allChildren

    -- find segment that use vertex $ fst fb
    let set = S.fromList allChildren
     
    -- Cartesian product of two sets allChildren
    let cartesian_pair = filter(\x -> x /= []) $ join $ map (\x -> map (\y -> x /= y ? [x, y] $ []) allChildren) allChildren
    let cartesian_pairX = combin 2 allChildren 
    -- let tsegs = map (\x -> (head x, last x) ) cartesian_pair
    let qsegs = qqsort (\[a, b] [c, d] -> a == d && b == c) cartesian_pair
    let gsegs = DL.groupBy (\[a, b] [c, d] -> a == d && b == c) qsegs
    let pairSeg = map head gsegs
    -- END_last_pt
    let sortPairSeg = sortVex pairSeg
    --  a b c d e  f g h i j  k l m n o  p q   r s t  u v w  x y z
    let segSet1 = S.fromList lt
    let pair1 = head pairSeg
    let bo = S.member pair1 segSet1
    let b1 = S.member [Vertex3 0 0 0, Vertex3 0 0 0] segSet1
    let segInTriangle = filter (\x -> x /= [] ) $ map (\x -> S.member x segSet1 ? x $ []) sortPairSeg
    -- remove one segment (two vertexes) from allChildren
    let segHasPt = map (\seg ->
                          map (\pt ->
                                 (fst $ (isPtInsideTriList pt $ [vertexPt] ++ seg)) ? Just seg $ Nothing
                              ) $ (DL.\\) allChildren seg) segInTriangle

    let xseg = join $ (map . map) fromJust $ filter (\s -> s /= []) $ map (\cx -> filter (\x -> x /= Nothing) cx) segHasPt
    let goodSeg = (DL.\\) segInTriangle xseg
    let validTriangle = sortVex $ map (\x -> vertexPt : x) goodSeg

    let cirList = map (\[x, y, z] ->
                       ([x, y, z],
                         threePtCircle x y z,
                         let may_cen = threePtCircle x y z
                         in case may_cen of
                             Just c  -> distX c y
                             Nothing -> (-1)
                        )
                     ) validTriangle

    -- Check whether other vertexes are inside the circle which is fixed by a three pts(a triangle)
    let isInCirList =  map(\(tri_vex, cen_vex, d) ->
                            map(\vex ->
                                let cen = case cen_vex of
                                            Just c  -> c
                                            Nothing -> error "Three pts can not form a circle."
                                in distX vex cen <= d ? (True, tri_vex, vex) $ (False, tri_vex, vex)
                              ) $ (DL.\\) randomPts tri_vex
                           ) cirList

    -- NOTE: newTri has duplicated set of segment and triangle
    let removeIntersectSeg [x, y, z] v = if | interSeg (x, v) (y, z) /= Nothing -> (sortSeg [y, z], sortSeg [x, v], map sortSeg [[v, x, z],[v, x, y]])
                                            | interSeg (y, v) (x, z) /= Nothing -> (sortSeg [x, z], sortSeg [y, v], map sortSeg [[v, y, z],[v, x, y]])
                                            | interSeg (z, v) (x, y) /= Nothing -> (sortSeg [x, y], sortSeg [z, v], map sortSeg [[v, y, z],[v, x, z]])
                                            | otherwise                         -> error $ "ERROR to remove segment" ++ (show [x, y, z]) ++ " v=" ++ show v
          where
            interSeg = intersectSegNoEndPt2
            sortSeg = qqsort cmpVex

    -- url xfido.com/publicfile/codedoc/triangle_split.pdf
    -- Remove the intersection segment from a Triangle, see doc above.
    -- Return two new Triangle with newly added Segment
    let noIntersectSeg = removeIntersectSeg [Vertex3 0 0 0, Vertex3 1 1 0, Vertex3 1 0 0] (Vertex3 0 1 0)

    -- If vertex is in the circumference, then the vertex is considered to be inside.
    -- url xfido.com/publicfile/codedoc/h4.txt
    -- url xfido.com/publicfile/codedoc/triangle_vex.png

    -- url xfido.com/publicfile/codedoc/incircle.txt
    -- NOTE: Found bug => Found a vertex is inside the triangle
    -- [Vertex3 0.1 0.1 0.0, Vertex3 0.12 0.8 0.0, Vertex3 1.3 0.12 0.0] v=Vertex3 0.2 0.6 0.0
    -- v is inside the triangle
    let inCircleList = join $ filter (\x -> x /= []) $ map (\ls -> filter (\(b,_,_) -> b) ls ) isInCirList

    -- url http://xfido.com/publicfile/codedoc/trivex.txt
    let triVex = map (\(_, cx, v) -> (cx, v)) inCircleList

    -- url http://xfido.com/publicfile/codedoc/segtri.txt
    -- mapM_ (\([x, y, z], v) -> triangle red (x, y, z)) triVex

    let ranPts = map (\(Vertex3 x y z) -> Vertex3 (x - 1.5) y z) randomPts

    -- ERROR: A vertex is inside a triangle
    -- URL:  http://xfido.com/publicfile/codedoc/tri_err.txt
    when False $ do
        let newTri = unique $ map (\(cx, v) -> removeIntersectSeg cx v) triVex
        fw "newTri"
        pre newTri
        mapM_ (\(_, _, cx) -> mapM_ (\[x, y, z] -> drawCircleThreePt x y z green) cx) newTri

        let rmSeg = len newTri > 0 ? (t1 . head) newTri $ []
        let finalSegment = filter (\x -> len x > 0) $ ((DL.\\) lt [rmSeg]) ++ [ len newTri > 0 ? (t2 . head) newTri $ []]
        fw "len lt"
        pp $ "len=" ++ (show $ len lt)
        fw "finalSegment"
        pre finalSegment

        mapM_(\[x, y] -> do 
                  drawSegment blue (x, y)
                  threadDelay 500
           ) finalSegment
      
        fw "len finalSegment"
        pp $ "len=" ++ (show $ len finalSegment)
  

    -- END_triangulation
 
    G.swapBuffers w
    G.pollEvents
    mainLoop w refCam refStep refGlobal refCount lssVex

main = do
  argList <- getArgs
  if len argList > 0 then do
    case head argList of
      "-h" -> helpme
      _    -> do
        print $ "Wrong option => " ++ (head argList) ++ ", -h => Help"
  else mymain

