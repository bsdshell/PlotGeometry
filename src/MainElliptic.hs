{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE CPP #-}

module Main where

-- import qualified Graphics.Rendering.FTGL as FTGL

-- import Graphics.GL.Types

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
import qualified Data.Map as DM
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
      PrimitiveMode(Triangles, TriangleStrip, Lines, LineLoop, Quads, TriangleFan, Points),
      GLdouble,
      Color3(..),
      Vertex3(..),
      VertexComponent,
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
import System.FilePath.Posix
import qualified Text.Printf as PR
import System.IO.Silently
import PlotGeometryLib
import AronUnicodeOp

mymain :: String -> IO ()
mymain fpath = do
  successfulInit <- G.init
  G.windowHint (G.WindowHint'DoubleBuffer True)
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
    mw2d <- G.createWindow 1000 1000 "PlotGeometry 2D" Nothing Nothing
    mw3d <- G.createWindow 1000 1000 "PlotGeometry 3D" Nothing Nothing
    -- maybe' :: Maybe a -> b -> (a -> b) -> b  
    -- maybe' mw (G.terminate >> exitFailure) $ \window -> do
    maybeX' (mw3d, mw2d) (G.terminate >> exitFailure)  $ \(window3d, window2d) -> do      
      -- ref <- newIORef initCam
      refCamRot3d <- newIORef initCameraRot
      refCamRot2d <- newIORef initCameraRot
      refStep <- newIORef initStep
      refGlobal <- newIORef initGlobal
      globalRef <- readIORef refGlobal
      writeIORef refGlobal globalRef
      randomPts <- convexPts

      updateBufferMap refGlobal fpath
      {--
      ls <- readGLScriptDraw fpath 
      mapM_ (\block -> do 
                     bufferMap <- readIORef refGlobal <&> bufferMap_
                     let mx = let s = head block 
                                  m = DM.fromList $ cmdTable s 
                                  cmd = case DM.lookup "cmd" m of
                                               Just x -> x
                                               Nothing -> error "Invalid format44"
                                  key = case DM.lookup "key" m of
                                                Just x -> x
                                                Nothing -> error "Invalid format55"
                                  shape = case DM.lookup "shape" m of
                                                Just x -> x
                                                Nothing -> error "Invalid format66"
                                  ma = case cmd of 
                                            c | c == "add" -> DM.insert key (shape, tail block) bufferMap 
                                              | c == "del" -> DM.delete key bufferMap
                                              | otherwise -> error "Invalid cmd99"
                                  in ma
                     modifyIORef refGlobal (\s -> s{bufferMap_ = mx})
                     ) ls
      --}
                         
                                
      -- writeIORef refGlobal $ setDrawPts   globalRef spherePtsX
      modifyIORef refGlobal (\x -> x {drawPts_ = spherePtsX})
      modifyIORef refGlobal (\x -> x {randomPts_ = randomPts})
      refFrame <- timeNowMilli >>= \x -> newIORef FrameCount {frameTime = x, frameCount = 1, frameNewCount = 0, frameIndex = 0}
      let cx = circleNArc' (Vertex3 0.4 0 0) 0.4 40 (0, pi)
      let cy = curvePtK (const 0) (0, 0.8) 40
      let cx' = [cx, cy]

      ls <- randomIntList 10 (1, 4) >>= \cx -> return $ randomVexList (Vertex3 0.0 0.0 0.0) cx
      modifyIORef refGlobal (\x -> x {randomWalk_ = ls})

      lt <- randomIntList 10 (1, 4) >>= \cx -> return $ randomVexListInt (-8, 0) cx
      modifyIORef refGlobal (\x -> x {randomWalkInt_ = lt})

      let rr = initRectGrid
      let nx = div (xCount_ rr) 2
      let ny = div (yCount_ rr) 2

      let blockAttr = BlockAttr {isFilled_ = False, typeId_ = 0, tetrisNum_ = 0, color_ = green}
      ioArray <- DAO.newArray ((- nx, - ny, 0), (nx - 1, ny - 1, 0)) blockAttr :: IO (IOArray (Int, Int, Int) BlockAttr)
      animaStateArr <- initAnimaState

      -- mymain
      -- thead 1
      -- G.makeContextCurrent mw0
      -- xx1
      mainLoop (window3d, window2d) (refCamRot3d, refCamRot2d) refGlobal refFrame animaStateArr cx' ioArray

      G.destroyWindow window3d
      G.destroyWindow window2d
      G.terminate
      exitSuccess

{-|
    \(\color{red}{Deprecated} \) Use 'circlePt'

    === Draw xy-plane circle

    KEY: draw simple circle on xy-plane

    DATE: Sunday, 25 February 2024 23:09 PST
-}
circlePtX :: Vertex3 GLfloat -> GLfloat -> [Vertex3 GLfloat]
circlePtX (Vertex3 x0 y0 z0) r =[let alpha = (pi2*n)/num in Vertex3 (rf r*sin alpha + x0) (rf r*cos alpha + y0) (0 + z0) | n <- [1..num]]
   where
       num = 4
       pi2 = 2*pi::Float

ptsList :: [Vertex3 GLfloat]
ptsList =  
      [ Vertex3 0.1 0.1 0,
        Vertex3 0.2 0.2 0,
        Vertex3 0.4 0.2 0,
        Vertex3 0.25 0.34 0,
        Vertex3 0.12 0.4 0,
        Vertex3 0.0 0.0 0,
        Vertex3 0.3 0.12 0,
        Vertex3 0.4 0.1 0,
        Vertex3 (-0.4) 0.1 0,
        Vertex3 (-0.4) (-0.1) 0
      ]

{--
{-|
 
    NOTE: q0 q1 q3 should be in CW

                                 
                                 q0
                                /  \ 
                           |   /    \
                           |  /      \
                           | /        \
                          q2 ---------- q1

                              v10 x v12


 -}
perpPlaneX::(Fractional a, Eq a)=> Vertex3 a -> (Vertex3 a, Vertex3 a, Vertex3 a) -> Vertex3 a
perpPlaneX p0@(Vertex3 e0 e1 e2) (q0@(Vertex3 m0 m1 m2), q1@(Vertex3 k0 k1 k2), q2@(Vertex3 d0 d1 d2)) = vx 
  where       
    v10 = q1 -: q0
    v12 = q1 -: q2
    vp = v10 `cross` v12
    v00 = q0 -: p0
    v_vp = case vp of
              Just v -> projv v00 v 
              Nothing -> error "ERROR: cross product"
    vx = q0 +: (v00 + (-v_vp))
--}

projvX :: (Fractional a, Eq a) => Vector3 a -> Vector3 a -> Vector3 a
projvX u v = w'
  where
    u' = veMat u
    v' = veMat v
    w  = projnX u' v'
    w' = matVe w



{-|
 
  @ 
  <u, v>
  ------  v = proj_uv
  <v, v>
  @
 -}
projnX:: (Fractional a, Eq a)=>[[a]]->[[a]]->[[a]]
projnX u v = c `mu` v
  where
   dot w k = (sum . join) $ (zipWith . zipWith) (*) w k
   d      = dot u v
   c      = d/dot v v
   mu a w = (map . map)(*a) w


drawArrowProj :: [(Vertex3 GLdouble, Vertex3 GLdouble)] -> (Bool, Bool, Bool) -> IO()
drawArrowProj ls (xy, yz, zx) = do
    preservingMatrix $ do
      let cc = [green, blue, cyan, magenta, yellow]
      let xzp = (Vertex3 0.0 0.0 0.0, Vertex3 1 0 0, Vertex3 0 0 (-1))
      let xyp = (Vertex3 0.0 0.0 0.0, Vertex3 1 0 0, Vertex3 0 1 0)
      let yzp = (Vertex3 0.0 0.0 0.0, Vertex3 0 1 0, Vertex3 0 0 (-1))
      mapM_ (\t -> do 
                   drawArrow3d t cc
                   when zx $ do
                     let vx = perpPlaneX (snd t) xzp 
                     drawArrow3d (fst t, vx) [colorChange 0.4 gray]
                   when xy $ do
                     let xy = perpPlaneX (snd t) xyp 
                     drawArrow3d (fst t, xy) [colorChange 0.3 white]
                   when yz $ do
                     let yz = perpPlaneX (snd t) yzp 
                     drawArrow3d (fst t, yz) [colorChange 0.2 gray, white]
            ) ls 

{--
intersectLineTri :: (Floating a, Ord a) => (Vertex3 a, Vertex3 a) -> (Vertex3 a, Vertex3 a, Vertex3 a) -> Maybe (Vertex3 a)
intersectLineTri (p0, p1) q@(q0, q1, q2) = isPerp || isPara ? Nothing $ Just vx 
  where
   epsilon = 0.000001
   vPerp = crossF (q1 -: q0) (q1 -: q2)
   isPerp = case vPerp of 
              Just v -> False 
              Nothing -> True 
   -- is line parallel to the plane q
   p0' = perpPlaneX p0 q 
   p1' = perpPlaneX p1 q
   v01  = p0 -: p1
   v01' = p0' -: p1'  
   ang = angle2Vector v01 v01'
   h0 = nr $ p0 -: p0' 
   h1 = nr $ p1 -: p1' 
   isPara = abs (h0 - h1) < epsilon
   vx | h0 > h1 = let u = uv v01' 
                      x = h0/(tan ang)
                  in p0' +: (x *: u)
      | otherwise = let u = uv $ (- v01') 
                        x = h1/(tan ang)
                    in p1' +: (x *: u)
--}


drawSphereNX_2::Int -> GLdouble ->Int -> Bool -> [Color3 GLdouble] -> IO()
drawSphereNX_2 n radius k isFilled cc = do
    let δ = 2*pi / rf n :: GLdouble
        ϵ = pi / rf n :: GLdouble
        r = radius
        fx::Int -> Int -> GLdouble
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ * i'
                     β  = ϵ * j'
                 in r * cos β * cos α
        fy::Int -> Int -> GLdouble
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ * i'
                     β  = ϵ * j'
                 in r * cos β * sin α
        fz::Int -> Int -> GLdouble
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ * i'
                     β  = ϵ * j'
                 in r * sin β 
        ss = [[Vertex3 (fx i j)
                       (fy i j)
                       (fz i j) | i <- take (n+1) [0..n]] | j <- let m = div n 2 in take (k+1) [m, m - 1 .. -m]] :: [[Vertex3 GLdouble]]
        in drawParamSphereX isFilled ss cc

currRotatedAxis :: IORef CameraRot -> IO ()
currRotatedAxis refCamRot = do
  currXYZ <- readIORef refCamRot <&> currXYZ_
  let cc = [green, blue, cyan, magenta, yellow]
  let r = 0.04
  let leng = 0.02
  let v0 = Vector3 0 0.7 0 :: Vector3 GLfloat 
  case currXYZ of
     v | v == 1 -> do
           preservingMatrix $ do
             rotate (-90) (Vector3 0 0 1 :: Vector3 GLfloat)
             translate v0 
             cylinder r leng (True, True) cc
       | v == 2 -> do
           preservingMatrix $ do
             translate v0 
             cylinder r leng (True, True) cc
       | v == 3 -> do
           preservingMatrix $ do
             rotate 90 (Vector3 1 0 0 :: Vector3 GLfloat)
             translate v0 
             cylinder r leng (True, True) cc
       | otherwise -> return () 


drawTriangleVexX:: (VertexComponent  a) => (Vertex3 a, Vertex3 a, Vertex3 a) -> [Color3 GLdouble] -> IO ()
drawTriangleVexX (a, b, c) lc = renderPrimitive TriangleStrip $ mapM_(\(co, v) -> do
                                                                  color co 
                                                                  vertex v
                                                                ) $ zip (join $ repeat lc) [a, b, c] 

{-|
   KEY: draw a triangle and a normal

   NOTE: Input pts CCW in right hand rule
 -
 -}
drawTriangleNormal:: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> [Color3 GLdouble] -> IO ()
drawTriangleNormal (v0, v1, v2) lc = do 
  renderPrimitive TriangleStrip $ mapM_(\(co, v) -> do
                                          color co 
                                          vertex v
                                        ) $ zip ((join . repeat) lc) [v0, v1, v2] 
  let v01 = v0 -: v1 
  let v12 = v1 -: v2 
  let vc = fmap (/3) $ v0 + v1 + v2
  let cc = [green, blue, cyan, magenta, yellow]
  let ve = v01 `crossF` v12 
  case ve of
    -- Just v -> drawArrow3dX (vc, vc +: v) lc 
    Just v -> do 
      if isCCW (v0, v1, v2) (vec_ vc) then drawArrow3dX (vc, vc +: v) cc
        else drawArrow3dX (vc, vc +: negate v) cc
      -- drawArrowN1 (vc, vc +: v) 
      -- drawArrowN1 (vc, vc +: negate v) 
    Nothing -> return () 
    -- Nothing -> error "ERROR: three pts are colinear" 

drawSphereNX::Int -> Int -> GLdouble -> Bool -> [Color3 GLdouble] -> IO()
drawSphereNX n k radius isFilled cc = do
    let δ = 2*pi / rf n :: GLdouble
        ϵ = pi / rf n :: GLdouble
        r = radius
        fx::Int -> Int -> GLdouble
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ * i'
                     -- β  = ϵ * j'
                     β  = pi/2 - ϵ * j'
                 in r * cos β * cos α
        fy::Int -> Int -> GLdouble
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ * i'
                     -- β  = ϵ * j'
                     β  = pi/2 - ϵ * j'
                 in r * cos β * sin α
        fz::Int -> Int -> GLdouble
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ * i'
                     β  = pi/2 - ϵ * j'
                 in r * sin β 

        ss = [[Vertex3 (fx i j)
                       (fy i j)
                       (fz i j) | i <- take (n+1) [0..n]] | j <- take (k+1) [0..]] :: [[Vertex3 GLdouble]]
                       -- (fz i j) | i <- take (n+1) [0..n]] | j <- let m = div n 2 in take (k+1) [m, m - 1 .. -m]] :: [[Vertex3 GLdouble]]
        in drawParamSphereX isFilled ss cc

{-|
 
  === KEY: draw partial sphere 

  DATE: Sun 17 Mar 01:23:49 2024 
  latitude
  longitude

  NOTE: take k latitude from the North

    @
    [
    [

      -- Singular pts
      Vertex3 (-1.7484556e-8) (-0.0) 0.4)
      Vertex3 (-1.4145303e-8) (-1.0277164e-8) 0.4),

      (Color3 0.0 1.0 0.0,Vertex3 (-1.7484556e-8) (-0.0) 0.4),       <----
      (Color3 0.0 0.0 1.0,Vertex3 0.12360679 0.0 0.38042262),
      (Color3 0.0 1.0 1.0,Vertex3 (-1.4145303e-8) (-1.0277164e-8) 0.4), <----
      (Color3 1.0 0.0 1.0,Vertex3 9.9999994e-2 7.265425e-2 0.38042262)
    ]
    ]

    -- NOTE: North pole is singular pt
    let lxx = [[
                {--
                [
                  (Color3 0.0 1.0 0.0,Vertex3 (-1.7484556e-8) (-0.0) 0.4),
                  (Color3 0.0 0.0 1.0,Vertex3 0.12360679 0.0 0.38042262),
                  (Color3 0.0 1.0 1.0,Vertex3 (-1.4145303e-8) (-1.0277164e-8) 0.4)
                ]
                --}
                [
                  (Color3 0.0 0.0 1.0,Vertex3 0.12360679 0.0 0.38042262),
                  (Color3 0.0 1.0 1.0,Vertex3 (-1.4145303e-8) (-1.0277164e-8) 0.4),
                  (Color3 1.0 0.0 1.0,Vertex3 9.9999994e-2 7.265425e-2 0.38042262)
                ]
              ]] :: [[[(Color3 GLdouble, Vertex3 GLdouble)]]]



                                               k latitude
                                                |
    drawParamSphereX::Fx -> Fy -> Fz -> Int -> Int -> [Color3 GLdouble]-> IO ()
    drawParamSphereX fx fy fz n k cc = do


    [[v 0, v 1], [v2, v3]]
      v 2, v 3
    @
 -}
drawParamSphereX::Bool -> [[Vertex3 GLdouble]] -> [Color3 GLdouble]-> IO ()
drawParamSphereX isFilled ss cc = do
  preservingMatrix $ do
        let mx = combinePt ss cc 
        when False $ do
          mapM_ (\(k, v) -> drawSegmentFromToD (odd k ? red $ white) v
                ) $ zip [1..] ss
        when isFilled $ do       
          mapM_ (\row -> renderPrimitive TriangleStrip $ mapM_ (\(c, v) -> do
                                                              color c
                                                              vertex v
                                                              ) row
                    ) mx
        when True $ do       
          preservingMatrix $ do
            {--
            s <- (cap . print) mx
            writeFileList "/tmp/ee.hs" [s]
            --}
            -- translate (Vector3 0.0 0 0 :: Vector3 GLdouble)
            
            (mapM_ . mapM_) (\tr -> do 
                                    {--
                                    renderPrimitive TriangleStrip $ mapM_ (\(c, v) -> do
                                                                          color c 
                                                                          vertex v
                                                                        ) tr
                                    --}
                                    let ve = fmap snd tr
                                    drawTriangleNormal (ve !! 0, ve !! 1, ve !! 2)  [green, blue, yellow]
                             )  $ map (`listSlide` 3) mx 
            
            -- xxx1
            let p0 = Vertex3 0 0 0.4
            let p1 = Vertex3 0.1 0.1 0.319
            let xs = (map . map) (\((_, a):(_, b):(_,c):_) -> do 
                                    case intersectLineTri (p0, p1) (a, b, c) of
                                          Just t -> ptInsideTri t (a, b, c)
                                          Nothing -> (False, -1) 
                                 ) $ map (`listSlide` 3) mx 

            let lc = circlePtD (Vertex3 0.7 0.7 0) 0.2 10
            renderPrimitive LineLoop $ mapM_(\v -> do
                      color red 
                      vertex v
                  ) lc

            (mapM_ . mapM_) (\((_, a):(_, b):(_,c):_) -> do 
                          mapM_(\px -> do
                            case intersectLineTri (p0, px) (a, b, c) of
                                  Just t -> do 
                                    let (isIn, _) = ptInsideTri t (a, b, c)
                                    if isIn then do
                                      logFileGT "xx_yes" [show t]
                                      drawCubeQuadX t 0.002
                                      {--
                                      renderPrimitive Points $ mapM_(\v ->do 
                                            color yellow 
                                            vertex v
                                        ) [t]
                                      --}
                                    else return ()

                                  Nothing -> return () 
                                ) lc
                       ) $ map (`listSlide` 3) mx 

        
fpath = "/Users/aaa/myfile/github/PlotGeometry/" </> "draw.hs"

data ArgInput = ArgInput{
                      cmd_ :: String, 
                      fpath_ :: String
                      } deriving (Show, Eq)


-- -f /tmp/adf.x
-- -triangle /tmp/a.x
-- -cube /tmp/c
parseArg :: String -> ArgInput
parseArg s = ArgInput {cmd_ = cmd, fpath_ = fpath} 
  where
    st = splitSPC s
    t@(cmd, fpath) = len st == 2 ? (let a = head st
                                        b = last st
                                        in hasPrefix "-" a ? (tail a, b) $ error $ "Invalid cmd=" ++ a) $ error "Invalid input argument"

circleX::(GLdouble, GLdouble) -> Vertex3 GLdouble ->[Vertex3 GLdouble]
circleX (r2, r1) (Vertex3 x0 y0 z0) =[let alpha = (pi2*n)/num in Vertex3 (r2 * cos alpha + x0) y0 (r1 * sin alpha + z0) | n <- [0..num]]
        where
          num = 10
          pi2 = 2*pi::GLdouble
  
projXZ :: Vector3 GLdouble -> Vector3 GLdouble
projXZ (Vector3 x y z) = Vector3 x 0 z
  
projVerXY :: Vertex3 GLdouble -> Vertex3 GLdouble
projVerXY (Vertex3 x y z) = Vertex3 x y 0

{-|
  KEY: three pts are counter clockwise, it is based on right hand rule
  
  p0 = Vertex3 0 0 0
  p1 = Vertex3 1 0 0
  p2 = Vertex3 0 1 0
  v = Vector3 0 0 1
  isCCW (p0, p1, p2) v
  True

  @
  if any <  pi/2
  if any == pi/2
  if any >  pi/2
  @

-}
isCCW :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> Vector3 GLdouble -> Bool
-- isCCW :: (Floating a, Ord a) => (Vertex3 a, Vertex3 a, Vertex3 a) -> Vector3 a -> Bool
isCCW (p0, p1, p2) up = if notAll ve then ang < pi/2 ? True $ False else sum (vecToList ve) > 0 ? True $ False
  where
    epsilon = 1e-12
    v01 = p0 -: p1
    v12 = p1 -: p2
    vc = v01 `crossF` v12
    notAll (Vector3 x y z) = x /= 0 && y /= 0 && z /= 0
    ve = case vc of
            Just v -> v
            Nothing -> error "ERROR: three pts are colinear"
    ang = angle2Vector ve up
    del = abs $ ang - pi/2
    fa = case del of
              d | del <= epsilon -> True
                | ang < pi/2     -> True
                | otherwise      -> False



projVer :: (Num a) => (Int, Int, Int) -> Vertex3 a -> Vertex3 a
projVer (a, b, c) (Vertex3 x y z) = Vertex3 (a /= 0 ? x $ 0) (b /= 0 ? y $ 0) (c /= 0 ? z $ 0)

{-|
  === KEY: colinear on xy-plane

  NOTE: 
-}
isColinearXY2d::(Num a, Eq a) => Vertex3 a ->Vertex3 a ->Vertex3 a -> (Bool, a)
isColinearXY2d (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = (dx == 0, dx)
                where
                    dx = (x1 - x0)*(y2 - y0) - (x2 - x0)*(y1 - y0)
  
{-|

  KEY:

  TESTFILE: /Users/aaa/myfile/github/haskell-test/detVer_test.hs


  * Project three pts 'Vertex3 a' on x-plane, y-plane and z-plane
  * Check whether they are colinear or not
  
  @
  p0 = Vertex3 0 0 0
  p1 = Vertex3 1 0 0
  p2 = Vertex3 0 1 0

  -- v01 = p0 -: p1
  -- v01 = 1 0 0
  -- v12 = p1 -: p2
  -- v12 = -1 1 0

  --  1  -1   x
  --  0   1
  --  0   0

  dx1 = detVerProj (0, 1, 1) p0 p1 p2
  fw "test1"
  print $ dx1 == 0
  @

-}
detVerProj :: (Show a, Num a) => (Int, Int, Int) -> (Vertex3 a, Vertex3 a, Vertex3 a) -> a
detVerProj t@(a, b, c) (p0@(Vertex3 x0 y0 z0), p1@(Vertex3 x1 y1 z1), p2@(Vertex3 x2 y2 z2))= dx
  where
    em = " "
    sw x = show x
    v01@(Vector3 x01 y01 z01) = p0 -: p1
    v12@(Vector3 x12 y12 z12) = p1 -: p2
    cat' cx = tail $ concatMap (" " ++) cx
    dx = case (a, b, c) of
              t | t ^._1 == 0 -> det2 [[y01, z01], [y12, z12]]  -- project on x-plane
                | t ^._2 == 0 -> det2 [[x01, z01], [x12, z12]]  -- project on y-plane
                | t ^._3 == 0 -> det2 [[x01, y01], [x12, y12]]  -- project on z-plane
                | otherwise -> error $ "ERROR333: detVerProj, invaid input " ++ cat' [sw t, sw p0, sw p1, sw p2]
{-|

  KEY:
  * Project three pts 'Vertex3 a' on x-plane, y-plane and z-plane
  * Compute the determinants of three pts on x-plane, y-plane, z-plane

  @
  ls = [(Vertex3 1 2 3, Vertex3 2 1 0, Vertex3 2 1 3)]
  let lt = map (\t -> let epsilon = 1e-12 in detVerProjXYZ t epsilon) ls
  @
-}
detVerProjXYZ :: (Ord a, Show a, Num a) => (Vertex3 a, Vertex3 a, Vertex3 a) -> a -> (a, a, a)
detVerProjXYZ t@(p0, p1, p2) epsilon = (f x, f y, f z)
  where
    f x = abs x <= epsilon ? 0 $ x
    x = detVerProj (0, 1, 1) t
    y = detVerProj (1, 0, 1) t
    z = detVerProj (1, 1, 0) t
   

{-|
  KEY: check whether three pts are in counter clockwise or not, check ccw
  TESTFILE: /Users/aaa/myfile/github/haskell-test/isCCWUp_test.hs
-}
isCCWUp :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> Vector3 GLdouble -> (Bool, GLdouble)
isCCWUp (p0, p1, p2) up = t 
  where
    epsilon = 1e-12 
    v01 = p0 -: p1
    v12 = p1 -: p2
    vc = v01 `crossF` v12
    t = case vc of
            Just v -> let ang = angle2Vector v up 
                          del = ang - pi/2
                      in case del of
                             d | abs del <= epsilon -> (False, ang)
                               | del < 0            -> (True, ang)
                               | otherwise          -> (False, ang)

            Nothing -> (False, -1) 

isCCWXY :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> Maybe Bool
isCCWXY (p0, p1, p2) = let d = det [v01, v12] in d == 0 ? Nothing $ (d > 0 ? Just True $ Just False)
  where
    p0' = projVerXY p0
    p1' = projVerXY p1
    p2' = projVerXY p2
    -- isCo = isColinear3d p0' p1' p2'
    v01 = take 2 $ vecToList $ p0' -: p1'
    v12 = take 2 $ vecToList $ p1' -: p2'

rotXAxisYUp :: Vector3 GLdouble -> (GLdouble, GLdouble)
rotXAxisYUp v@(Vector3 x y z) = (angX, angYUp)
  where
    e₁ = Vector3 1 0 0
    vxz = projXZ v
    angX = angle2Vector e₁ vxz
    angYUp = angle2Vector vxz v
  
-- xxx1
drawCylinderEllipseX :: (GLdouble, Vertex3 GLdouble, Vector3 GLdouble, [Color3 GLdouble]) -> (GLdouble, Vertex3 GLdouble, Vector3 GLdouble, [Color3 GLdouble]) ->
  [Color3 GLdouble] ->
  Bool ->
  IO ([Vertex3 GLdouble], [Vertex3 GLdouble], [[GLdouble]])
drawCylinderEllipseX (r0, p0, v0, c0) (r1, p1, v1, c1) cc isShown = do
  preservingMatrix $ do
    let (cx0, _) = let majorAxis = r0
                       (angX, angUp) = rotXAxisYUp v0
                       minorAxis = majorAxis / cos angUp
                       in ellipseRot (majorAxis, minorAxis) (angX, angUp) p0
    let (cx1, m₄) = let majorAxis = r1
                        px0 = projXZ v1
                        angX = angle2Vector e₁ px0
                        angUp = angle2Vector v1 px0
                        minorAxis = majorAxis / cos angUp
                        in ellipseRot (majorAxis, minorAxis) (angX, angUp) p1
    let cx0' = p0 : cx0
    let cx1' = p1 : cx1
    let cy = combineList cx0 cx1
    when isShown $ do
      drawPrimitiveX TriangleFan cx0' c0
      drawPrimitiveX TriangleFan cx1' c1
      drawPrimitiveX TriangleStrip cy cc'
    return (cx0, cx1, m₄)
    where
      e₁ = Vector3 1 0 0
      c1' = cls c1
      cc' = cls cc
  
                           
drawCylinder3dX :: GLdouble -> (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> [Color3 GLdouble] -> IO()
drawCylinder3dX r (p0, p1, p2) cl = do
  preservingMatrix $ do
    let v = p0 ➞ p1  -- vector from p0 to p1
    let e₂ = Vector3 0 1 0 :: Vector3 GLdouble
    -- let m1 = padMat3To4 $ rotToVecMat e₂ v 
    -- translate (vec_ p0)
    -- multiModelviewMatD $ join m1
  
      {--  
    b1 <- getRedisX "b1"
    when (b1 /= 0) $ do
      cylinderArrow (nr v) cl
      --}
    
    let v01 = p0 -: p1
    let v12 = p1 -: p2
    let k = case v01 `crossF` v12 of
                   Just v -> v
                   Nothing -> error "p0 p1 p2 can not be colinear."
    
    let r0 = r
    let v0 = Vector3 1 0 0

    let c0 = [cyan, blue]
    let t0 = (r0, p0, v0, c0)
    
    let r1 = r
    let v1 = Vector3 1 0 0
    let c1 = [green, yellow]
    let t1 = (r1, p1, v1, c1)
    -- let t1 = (r1, p1, k, c1)    
    
    let cc = [gray, magenta]
    b2 <- getRedisX "b2"
    when (b2 /= 0) $ do
      let mt x = tran [vl x]
      let m = rotToVecMat e₂ v
      (cx0, cx1, m₄) <- drawCylinderEllipseX t0 t1 cc False
      -- let cx0' = map (\v -> lv $ join $ m ∘ mt v) cx0
      let t0@(sx0, sx0')= let s = rotVerMap m cx0 p0 in (s, p0:s)
      let t1@(sx1, sx1')= let s = rotVerMap m cx1 p1 in (s, p1:s)
      let cy' = combineList sx0 sx1
      
      drawArrow3d (p1, p1 +: (0.5 *: uv k)) [cyan]
      
      let θ = let x = angle2Vector (-v01) v12 in x/2
      let m₁ = rotMat k (pi/2  - θ)
      let cxx = rotVerMap m₁ sx1 p1
      let cx11 = p1 : cxx
      let r = 0.1


      -- let cirls = map (\(Vertex3 x y z) -> [x, y, z, 1]) $ ellipticPtXZ (Vertex3 0 0 0) (r, r/cos (pi/2 - θ)) 10
      let cirls = map (\(Vertex3 x y z) -> [x, y, z, 1]) $ ellipticPtXZ (Vertex3 0 0 0) (r, r) 10
      let eccx =  map (\x -> let ls = join $ m₄ `multiVec` x in (listToVer . init) ls) cirls
      let eccx' = rotVerMap m₁ eccx p1

      -- drawPrimitiveX TriangleFan cx11 (cls [red, white])
      drawPrimitiveX TriangleFan eccx' (cls [blue, green])
      drawPrimitiveX TriangleFan eccx (cls [blue, red])  
  
      drawPrimitiveX TriangleFan sx0' c0
      drawPrimitiveX TriangleFan sx1' c1
      drawPrimitiveX TriangleStrip (combineList sx0 cxx) (cls cc)
    when True $ do
      let rx = 0.1
      let rz = 0.2
      let p0 = Vertex3 0 0 0
      let pts = p0 : ellipticPtXZ p0 (rx, rz) 10
      drawPrimitiveX TriangleFan pts (cls [magenta, white])
  where
    vl = verToList
    lv = listToVer
    (➞) :: (Floating a) => Vertex3 a -> Vertex3 a -> Vector3 a
    (➞) (Vertex3 x y z) (Vertex3 x' y' z') = Vector3 (x' - x) (y' - y) (z' - z) 
    --HEAVY TRIANGLE-HEADED RIGHTWARDS ARROW
    --Unicode: U+279E, UTF-8: E2 9E 9E

{-|
  KEY: normal of three pts, normal of three points, normal of 3 pts

  @
  p0 = Vertex3 0 0 0
  p1 = Vertex3 1 0 0
  p2 = Vertex3 1 1 0 

  pn = normalThreePts (p0, p1, p2)
  fw "pn"
  pn
  @
-}
normalThreePts :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> Maybe (Vector3 GLdouble)
normalThreePts (p0, p1, p2) = v01 `crossF` v12
  where
    v01 = p0 -: p1
    v12 = p1 -: p2
  
isParallelPlane :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) ->
                    (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) ->
                    Bool
isParallelPlane p@(p0, p1, p2) q@(q0, q1, q2) = case n0 `crossF` n1 of
                                                      Just x -> False
                                                      Nothing -> True
  where
    n0 = case normalThreePts p of
              Just x -> x
              Nothing -> error "ERROR:444, three pts can be colinear."
    n1 = case normalThreePts q of
              Just x -> x
              Nothing -> error "ERROR:555, three pts can be colinear."

isPerpPlane :: Vector3 GLdouble -> (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> Bool
isPerpPlane v (p0, p1, p2) = case v01 `crossF` v12 of
                                Just u -> case u `crossF` v of
                                           Just x -> False
                                           Nothing -> True
                                Nothing -> error "ERROR:123, three pts can not be colinear."
  where
    v01 = p0 -: p1
    v12 = p1 -: p2


{-|
  KEY: form a 4x4 translation matrix

  @
  > printMat $ mat4Tran (Vertex3 1 2 3)
  1.0 0.0 0.0 1.0
  0.0 1.0 0.0 2.0
  0.0 0.0 1.0 3.0
  0.0 0.0 0.0 1.0
  @
-}
mat4Tran :: Vertex3 GLdouble -> [[GLdouble]]
mat4Tran v = m
  where
    m = padMat3To4Tran (matId 3) (verToList v ++ [1])
  
multiM4Ver :: [[GLdouble]] -> Vertex3 GLdouble -> Vertex3 GLdouble
multiM4Ver m₄ v = listToVer ls 
  where
    ls = join $ m₄ `multiVec` (verToList v ++ [1])

flipY :: [Vertex3 GLdouble] -> [Vertex3 GLdouble]
flipY = map (\(Vertex3 x y z) -> Vertex3 x (-y) z)

m1 = [
       [1, 0, 0],
       [0, 0, 1],
       [0, 1, 0]
     ]
  
ppm x = (cap . printMat) x

assertF :: Bool -> String -> IO()
assertF b s = do
  unless b $ do
    error s

{-|
  KEY: list to tuple3
-}
toTuple3 :: [a] -> Maybe (a, a, a)
toTuple3 [a, b, c] = Just (a, b, c)
toTuple3  _        = Nothing

  
ellipticThreePts :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> GLdouble -> Int -> IO [Vertex3 GLdouble]
ellipticThreePts (p0x', p1x', p2x') r num = do
  let epsilon = 1e-12
  let tz@(b, deg) = let up = Vector3 0 0 1 in isCCWUp (p0x', p1x', p2x') up
  logFileGT "xyplane" [show tz]

  let matYZ = case not b && abs (pi/2 - deg) < epsilon of
                   t | t -> let m = [
                                      [0, 0, 1],
                                      [0, 1, 0],
                                      [1, 0, 0]
                                    ]
                             in m
                      | otherwise -> matId 3

  -- logFileMat "matYZ" matYZ
  let matFlipY = case isCCWXY (p0x', p1x', p2x') of
                        Just x -> x ? out (\a b -> a == b && a == 2 ? (-1) $ (a == b ? 1 $ 0)) [1..3] [1..3] $ matId 3
                        Nothing -> matId 3

  -- logFileMat "matFlipY" matFlipY
  -- let flipY v = matId 3
  {--  
  let mm = matId 3
  let mmr = matId 3
  --} 
  
  let mm = matFlipY ∘ matYZ
  let mmr = matYZ ∘ matFlipY
  
  let p0x = multiVertex mm p0x'
  let p1x = multiVertex mm p1x'
  let p2x = multiVertex mm p2x'  
  let p0 = p0x + (-p0x)
  let p1 = p1x + (-p0x)
  let p2 = p2x + (-p0x)
  
  let vz = Vertex3 0 0 0
  let vx = Vector3 1 0 0
  let vy = Vector3 0 1 0
  let vzAxis = Vector3 0 0 1
  let c0 = [green, blue]
  let c1 = [red, white]
  let c2 = [yellow, white]
  {--  
  drawArrow3d (p0, p1) c0
  drawArrow3d (p1, p2) c1
  --}  
  let v01 = p0 -: p1
  let v12 = p1 -: p2
  let v02 = p0 -: p2

  let ry = r
  let rz = r
  -- let num = 10
  let cc = [gray, magenta]
  let c0 = Vertex3 0 0 0
  let c1 = Vertex3 (nr v01) 0 0
  let cx0 = ellipticPtYZ c0 (ry, rz) num
  let cx1 = ellipticPtYZ c1 (ry, rz) num

  -- cylinder on x-axis
  b1 <- getRedisX "b1"
  when (b1 /= 0) $ do
    drawPrimitiveX TriangleFan (c0 : cx0) [green, white]
    drawPrimitiveX TriangleFan (c1 : cx1) [red, white]
    drawPrimitiveX TriangleStrip (combineList cx0 cx1) (cls cc)

  let m = rotToVecMat vx v01  
  let cx0' = rotVerMap m (c0:cx0) vz
  let cx1' = rotVerMap m (c1:cx1) vz
  logFileGT "cx0'xxk" [show $ len cx0']
  -- move cylinder x-axis to v01
  b2 <- getRedisX "b2"
  when (b2 /= 0) $ do
    drawPrimitiveX TriangleFan cx0' [white, blue]
    drawPrimitiveX TriangleFan cx1' [white, yellow]
    drawPrimitiveX TriangleStrip (combineList (tail cx0') (tail cx1')) [white, gray]  

  -- move cylinder x-axis to v12
  let m₁ = rotToVecMat vx v12
  let s1 = Vertex3 0 0 0
  let s2 = Vertex3 (nr v12) 0 0
  let q1 = ellipticPtYZ s1 (ry, rz) num
  let q2 = ellipticPtYZ s2 (ry, rz) num
  let q11 = map (+: v01) $ rotVerMap m₁ (s1 : q1) vz
  let q22 = map (+: v01) $ rotVerMap m₁ (s2 : q2) vz
  
  b3 <- getRedisX "b3"
  when (b3 /= 0) $ do
    let (!) = (!!)
    drawPrimitiveX TriangleFan q11 [white, cyan]
    drawPrimitiveX TriangleFan q22 [white, red]
    drawPrimitiveX TriangleStrip (combineList (tail q11) (tail q22)) [white, blue]
  
    drawArrow3d (head q11, head q22) c2
    let b = isParallelPlane (q11 ! 0, q11 ! 1, q11 ! 2) (q22 ! 0, q22 ! 1, q22 ! 2)
    logFileGT "bxx" [show b]
    let b1 = isPerpPlane v12 (q11 ! 0, q11 ! 1, q11 ! 2)
    logFileGT "bkk" [show b1]
    return ()
  
  -- v01 to vy = Vector3 0 1 0
  let m₂ = rotToVecMat v01 vy
  logFileGT "mxx" [show m₂]
  let cx00' = rotVerMap m₂ cx0' vz
  let cx11' = rotVerMap m₂ cx1' vz
  b4 <- getRedisX "b4"
  when (b4 /= 0) $ do  
    drawPrimitiveX TriangleFan cx00' [white, green]
    drawPrimitiveX TriangleFan cx11' [white, red]
    drawPrimitiveX TriangleStrip (combineList (tail cx00') (tail cx11')) (cls [blue, red])
  -- transform v12 to new position with matrix m₂
  -- b5 <- getRedisX "b5"
  let q11' = rotVerMap m₂ q11 vz
  let q22' = rotVerMap m₂ q22 vz
  b5 <- getRedisX "b5"
  when (b5 /= 0) $ do
    drawPrimitiveX TriangleFan q11' [blue, magenta]
    drawPrimitiveX TriangleFan q22' [blue, yellow]
    drawPrimitiveX TriangleStrip (combineList (tail q11') (tail q22')) (cls [white, yellow])
  assertF (len q11' == num + 2) "len q11' == 12 ?"
  assertF (len q22' == num + 2) "len q22' == 12 ?"
  
  
  -- xxx2
  -- when True $ do
  -- project v12 onto y-axis
  let v12' = head q11' -: head q22'
  -- u + x = v12' = v12' - u
  let vr = let u = projv v12' vy in v12' - u
  -- FIXME: is there bug here?
  -- let ang = let a = angle2Vector vx vr in ve_3 vr < 0 ? negate a $ a
  -- FIXME: fixed, angle2Vector <= pi, if ve_3 vr > 0, then there is error in ang
  -- let ang = let a = angle2Vector vx vr in a
  let ang = let a = angle2Vector vx vr in ve_3 vr > 0 ? 2*pi - a $ a
  -- let ang = let a = angle2Vector vx vr in a
  logFileGT "angxx" [show ang]
  logFileGT "vrxx" [show vr]
  -- rotate around Vector3 0 1 0 in ang, onto plane xy plane
  let m₃ = rotMat vy (-ang)
  let ax00' = rotVerMap m₃ cx00' vz
  let ax11' = rotVerMap m₃ cx11' vz
  let q111 = rotVerMap m₃ q11' vz
  let q222 = rotVerMap m₃ q22' vz
  b6 <- getRedisX "b6"
  when (b6 /= 0) $ do
    drawPrimitiveX TriangleFan q111 [white, blue]
    drawPrimitiveX TriangleFan q222 [cyan, white]
    drawPrimitiveX TriangleStrip (combineList (tail q111) (tail q222)) (cls [green, red])
  logFileGT "pxx1" [show $ head q111]
  logFileGT "pxx2" [show $ head q222]
  let x1 = head q111
  let x2 = head q222
  let vxx = uv $ x2 -: x1
  drawArrow3d (x2, x2 +: (1.2 *: vxx) ) [color2, color3]

  let angHalf = let a = angle2Vector vy vxx in a/2
  logFileGT "angHalfxx" [show angHalf]
  let angRot = pi/2 + angHalf
  logFileGT "angRotxx" [show angRot]  
  let m₄ = rotMat (Vector3 0 0 1) angRot
  let ellipicCirc = rotVerMap m₄ ax11' (head ax11')
  -- drawPrimitiveX TriangleFan ellipicCirc [color3, color1]
  let rx = r
  let rz = r
  -- if angRot > pi/2, then cos angRot < 0
  let majorAxis = abs $ rz /cos angRot
  let ax0 = ellipticPtXZ (Vertex3 0 0 0) (rz, rz) num
  let ax1 = ellipticPtXZ (Vertex3 0 0 0) (majorAxis, rz) num
  let cen = head ax11'
  let pts' = cen:map (+cen) ax1
  let ax111' = rotVerMap m₄ pts' cen

  b7 <- getRedisX "b7"
  when (b7 /= 0) $ do
    drawPrimitiveX TriangleFan ax111' [color1, color2]
    drawPrimitiveX TriangleStrip (combineList (tail q222) (tail ax111')) (cls [color2, red])
    drawPrimitiveX TriangleStrip (combineList (tail ax00') (tail ax111')) (cls [color3, blue])

  let mr₃ = rotMat vy (ang)
  b8 <- getRedisX "b8"
  when (b8 /= 0) $ do
    let dx1 = combineList (tail q222) (tail ax111')
    let dx0 = combineList (tail ax00') (tail ax111')
    let dx1' = rotVerMap mr₃ dx1 vz
    let dx0' = rotVerMap mr₃ dx0 vz      
    drawPrimitiveX TriangleStrip dx1' (cls [color2, green])
    drawPrimitiveX TriangleStrip dx0' (cls [color3, magenta])

  let dx1 = combineList (tail q222) (tail ax111')
  let dx0 = combineList (tail ax00') (tail ax111')
  logFileGT "lenax111'" [show $ len ax111']
  logFileGT "lenax00'" [show $ len ax00']
  logFileGT "lenq222" [show $ len q222]
  logFileGT "lendx1" [show $ len dx1]
  logFileGT "lendx0" [show $ len dx0]
  let dx1' = rotVerMap mr₃ dx1 vz
  let dx0' = rotVerMap mr₃ dx0 vz
  let retx = rotVerMap mr₃ ax111' vz

  let mr₂ = rotToVecMat vy v01
  -- translate back to p0
  {--      
  let dx11' = map (+p0x) $ rotVerMap matY (rotVerMap mr₂ dx1' vz) vz
  let dx00' = map (+p0x) $ rotVerMap matY (rotVerMap mr₂ dx0' vz) vz
  --}      
  -- let dx11' = map (multiVertex matFlipY) $ map (+p0x) $ rotVerMap mr₂ dx1' vz
  let dx11' = map (\x -> multiVertex mmr $ (+) p0x x) $ rotVerMap mr₂ dx1' vz
  let dx00' = map (\x -> multiVertex mmr $ (+) p0x x) $ rotVerMap mr₂ dx0' vz
  let retx' = map (\x -> multiVertex mmr $ (+) p0x x) $ rotVerMap mr₂ retx vz
  b9 <- getRedisX "b9"
  when (b9 /= 0) $ do
    drawPrimitiveX TriangleStrip dx11' (cls [color2, blue])
    drawPrimitiveX TriangleStrip dx00' (cls [color3, white])
    drawArrow3d (p0x', p1x') [color2, color4]
    drawArrow3d (p1x', p2x') [color3, color1]
  return $ tail retx'

ellipticThreePtsX2 :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> GLdouble -> Int -> IO [Vertex3 GLdouble]
ellipticThreePtsX2 (p0x', p1x', p2x') r num = do
  let epsilon = 1e-12
  let tz@(b, deg) = let up = Vector3 0 0 1 in isCCWUp (p0x', p1x', p2x') up
  logFileGT "xyplane" [show tz]

  let matYZ = case not b && abs (pi/2 - deg) < epsilon of
                   b | b -> let m = [
                                      [0, 0, 1],
                                      [0, 1, 0],
                                      [1, 0, 0]
                                    ]
                             in m
                      | otherwise -> matId 3
    
  let matFlipY = case isCCWXY (p0x', p1x', p2x') of
                        Just x -> x ? out (\a b -> a == b && a == 2 ? (-1) $ (a == b ? 1 $ 0)) [1..3] [1..3] $ matId 3
                        Nothing -> matId 3

  -- logFileMat "matYZxx" matYZ
  -- logFileMat "matFlipYxx" matFlipY
  
  -- let flipY v = matId 3
  {--  
  let mm = matFlipY `multiMat` matYZ
  let mmr = matYZ `multiMat` matFlipY
  --}  
  let mm = matId 3
  let mmr = matId 3

  -- let flipY v = matId 3
  let p0x = multiVertex mm p0x'
  let p1x = multiVertex mm p1x'
  let p2x = multiVertex mm p2x'  
  let p0 = p0x + (-p0x)
  let p1 = p1x + (-p0x)
  let p2 = p2x + (-p0x)
  
  let vz = Vertex3 0 0 0
  let vx = Vector3 1 0 0
  let vy = Vector3 0 1 0
  let vzAxis = Vector3 0 0 1
  let c0 = [green, blue]
  let c1 = [red, white]
  let c2 = [yellow, white]

  let v01 = p0 -: p1
  let v12 = p1 -: p2
  let v02 = p0 -: p2

  let ry = r
  let rz = r
  let cc = [gray, magenta]
  let c0 = Vertex3 0 0 0
  let c1 = Vertex3 (nr v01) 0 0
  let cx0 = ellipticPtYZ c0 (ry, rz) num
  let cx1 = ellipticPtYZ c1 (ry, rz) num

  -- cylinder on x-axis
  
  _ <- rotToVecMatX vx v01
  let m = rotToVecMat vx v01
  let cx0' = rotVerMap m (c0:cx0) vz
  let cx1' = rotVerMap m (c1:cx1) vz
  
  -- move cylinder x-axis to v12
  _ <- rotToVecMatX vx v12
  let m₁ = rotToVecMat vx v12
  let s1 = Vertex3 0 0 0
  let s2 = Vertex3 (nr v12) 0 0
  let q1 = ellipticPtYZ s1 (ry, rz) num
  let q2 = ellipticPtYZ s2 (ry, rz) num
  let q11 = map (+: v01) $ rotVerMap m₁ (s1 : q1) vz
  let q22 = map (+: v01) $ rotVerMap m₁ (s2 : q2) vz
  
  -- v01 to vy = Vector3 0 1 0
  _ <- rotToVecMatX v01 vy
  let m₂ = rotToVecMat v01 vy
  let cx00' = rotVerMap m₂ cx0' vz
  let cx11' = rotVerMap m₂ cx1' vz

  -- transform v12 to new position with matrix m₂
  -- b5 <- getRedisX "b5"
  let q11' = rotVerMap m₂ q11 vz
  let q22' = rotVerMap m₂ q22 vz

  -- xxx2
  -- when True $ do
  -- project v12 onto y-axis
  let v12' = head q11' -: head q22'
  -- u + x = v12' = v12' - u
  let vr = let u = projv v12' vy in v12' - u
  -- FIXME: is there bug here?
  -- let ang = let a = angle2Vector vx vr in ve_3 vr < 0 ? negate a $ a
  -- FIXME: fixed, angle2Vector <= pi, if ve_3 vr > 0, then there is error in ang
  -- let ang = let a = angle2Vector vx vr in a
  let ang = let a = angle2Vector vx vr in ve_3 vr > 0 ? 2*pi - a $ a
  -- let ang = let a = angle2Vector vx vr in a
  logFileGT "angxx" [show ang]
  logFileGT "vrxx" [show vr]
  -- rotate around Vector3 0 1 0 in ang, onto plane xy plane
  let m₃ = rotMat vy (-ang)
  let ax00' = rotVerMap m₃ cx00' vz
  let ax11' = rotVerMap m₃ cx11' vz
  let q111 = rotVerMap m₃ q11' vz
  let q222 = rotVerMap m₃ q22' vz
  b6 <- getRedisX "b6"

  logFileGT "pxx1" [show $ head q111]
  logFileGT "pxx2" [show $ head q222]
  let x1 = head q111
  let x2 = head q222
  let vxx = uv $ x2 -: x1

  let angHalf = let a = angle2Vector vy vxx in a/2
  logFileGT "angHalfxx" [show angHalf]
  let angRot = pi/2 + angHalf
  logFileGT "angRotxx" [show angRot]  
  let m₄ = rotMat (Vector3 0 0 1) angRot
  let ellipicCirc = rotVerMap m₄ ax11' (head ax11')
  -- drawPrimitiveX TriangleFan ellipicCirc [color3, color1]
  let rx = r
  let rz = r
  -- if angRot > pi/2, then cos angRot < 0
  let majorAxis = abs $ rz /cos angRot
  let ax0 = ellipticPtXZ (Vertex3 0 0 0) (rz, rz) num
  let ax1 = ellipticPtXZ (Vertex3 0 0 0) (majorAxis, rz) num
  let cen = head ax11'
  let pts' = cen:map (+cen) ax1
  let ax111' = rotVerMap m₄ pts' cen


  let mr₃ = rotMat vy (ang)

  -- when (b9 /= 0) $ do
  let dx1 = combineList (tail q222) (tail ax111')
  let dx0 = combineList (tail ax00') (tail ax111')
  let dx1' = rotVerMap mr₃ dx1 vz
  let dx0' = rotVerMap mr₃ dx0 vz
  let retx = rotVerMap mr₃ ax111' vz

  let mr₂ = rotToVecMat vy v01
  -- translate back to p0
  -- let dx11' = map (multiVertex matFlipY) $ map (+p0x) $ rotVerMap mr₂ dx1' vz
  let dx11' = map (\x -> multiVertex mmr $ (+) p0x x) $ rotVerMap mr₂ dx1' vz
  let dx00' = map (\x -> multiVertex mmr $ (+) p0x x) $ rotVerMap mr₂ dx0' vz
  let retx' = map (\x -> multiVertex mmr $ (+) p0x x) $ rotVerMap mr₂ retx vz

  return $ tail retx'


ellipticThreePtsX :: (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble) -> GLdouble -> Int -> IO [Vertex3 GLdouble]
ellipticThreePtsX (p0x, p1x, p2x) r num = do
  logFileGT "pxx" $ map show [p0x, p1x, p2x]
  let isPY = isPerpPlane (Vector3 0 1 0) (p0x, p1x, p2x)
  logFileGT "isPY" [show isPY]
  when isPY $ do
    logFileGT "p0x p1x p2x" $ map show [p0x, p1x, p2x]
  
  let isPX = isPerpPlane (Vector3 1 0 0) (p0x, p1x, p2x)
  logFileGT "isPX" [show isPX]
  when isPX $ do
    logFileGT "p0x p1x p2x" $ map show [p0x, p1x, p2x]
  
  let isPZ = isPerpPlane (Vector3 0 0 1) (p0x, p1x, p2x)
  logFileGT "isPZ" [show isPZ]
  when isPZ $ do
    logFileGT "p0x p1x p2x" $ map show [p0x, p1x, p2x]
  
  -- let mx = mat4Tran (-p0x)
  let p0 = p0x + (-p0x)
  let p1 = p1x + (-p0x)
  let p2 = p2x + (-p0x)
  
  let vz = Vertex3 0 0 0
  let vx = Vector3 1 0 0
  let vy = Vector3 0 1 0
  let vzAxis = Vector3 0 0 1
  let c0 = [green, blue]
  let c1 = [red, white]
  let c2 = [yellow, white]

  let v01 = p0 -: p1
  let v12 = p1 -: p2
  let v02 = p0 -: p2

  let ry = r
  let rz = r
  let cc = [gray, magenta]
  let q0 = Vertex3 0 0 0
  let q1 = Vertex3 (nr v01) 0 0
  let cx0 = ellipticPtYZ q0 (ry, rz) num
  let cx1 = ellipticPtYZ q1 (ry, rz) num

  let m = rotToVecMat vx v01  
  let cx0' = rotVerMap m (q0:cx0) vz
  let cx1' = rotVerMap m (q1:cx1) vz
  
  let m₁ = rotToVecMat vx v12
  let s0 = Vertex3 0 0 0
  let s1 = Vertex3 (nr v12) 0 0
  let sx0 = ellipticPtYZ s0 (ry, rz) num
  let sx1 = ellipticPtYZ s1 (ry, rz) num
  let sx0' = map (+: v01) $ rotVerMap m₁ (s0 : sx0) vz
  let sx1' = map (+: v01) $ rotVerMap m₁ (s1 : sx1) vz
  
  -- reverse v01 to vy
  let m₂ = rotToVecMat v01 vy
  let cx11' = rotVerMap m₂ cx1' vz

  -- b5 <- getRedisX "b5"
  let sx00' = rotVerMap m₂ sx0' vz
  let sx11' = rotVerMap m₂ sx1' vz

  -- xxx2
  -- when True $ do
  -- project v12 onto y-axis
  let v12' = let x0 = head sx00'; x1 = head sx11' in x0 -: x1
  let vr = let u = projv v12' vy in v12' - u
  logFileGT "v12xx" [show v12']
  logFileGT "vrxx" [show vr]
  -- FIXME: is there bug here?
  -- let ang = let a = angle2Vector vx vr in ve_3 vr < 0 ? negate a $ a
  let ang = let a = angle2Vector vx vr in a

  -- rotate around Vector3 0 1 0 in ang, onto plane xy plane
  logFileGT "angxx" [show ang]
  let m₃ = rotMat vy (-ang)
  let ax11' = rotVerMap m₃ cx11' vz
  let bx00' = rotVerMap m₃ sx00' vz
  let bx11' = rotVerMap m₃ sx11' vz

  let x0 = head bx00'
  let x1 = head bx11'
  let vxx = uv $ x1 -: x0
  -- drawArrow3d (x1, x1 +: (1.2 *: vxx) ) [color2, color3]

  let angHalf = let a = angle2Vector vy vxx in a/2
  -- FIXME: twist bug
  -- let angRot = vx_1 p2x > vx_1 p1x ? pi/2 + angHalf $ pi/2 - angHalf
  let angRot = pi/2 + angHalf
  logFileGT "angHalfxx" [show angHalf]
  logFileGT "angRotxx" [show angRot]
  let m₄ = rotMat (Vector3 0 0 1) angRot
  -- drawPrimitiveX TriangleFan ellipicCirc [color3, color1]
  let rx = r
  let rz = r
  -- if angRot > pi/2, then cos angRot < 0
  let majorAxis = abs $ rz /cos angRot
  let ax1 = ellipticPtXZ (Vertex3 0 0 0) (majorAxis, rz) num    
  let cen = head ax11'
  let pts' = cen:map (+cen) ax1
  let ax111' = rotVerMap m₄ pts' cen

  let mr₃ = rotMat vy (ang)

  let mr₂ = rotToVecMat vy v01

  let retx = rotVerMap mr₃ ax111' vz

  let retx' = map (+p0x) $ rotVerMap mr₂ retx vz

  return $ tail retx'


mainLoop ::
  (G.Window, G.Window) ->
  (IORef CameraRot, IORef CameraRot) ->
  IORef GlobalRef ->
  IORef FrameCount ->
  IOArray Int AnimaState ->
  [[Vertex3 GLfloat]] ->
  DAO.IOArray (Int, Int, Int) BlockAttr ->
  IO ()
mainLoop (w3d, w2d) (refCamRot3d, refCamRot2d) refGlobal refGlobalFrame animaStateArr lssVex ioArray = unlessX' (G.windowShouldClose w3d) (G.windowShouldClose w2d) $ do

  G.getWindowFocused w3d >>= \b -> when b $ G.setKeyCallback w3d (Just $ keyBoardCallBack3d refCamRot3d refGlobal ioArray)
  G.getWindowFocused w2d >>= \b -> when b $ G.setKeyCallback w2d (Just $ keyBoardCallBack2d refCamRot2d refGlobal ioArray)
  G.getWindowFocused w3d >>= \b -> when b $ G.setMouseButtonCallback w3d (Just $ mouseCallbackX refGlobal)
  G.getWindowFocused w2d >>= \b -> when b $ G.setMouseButtonCallback w2d (Just $ mouseCallbackX refGlobal) 

  beginWindow3d w3d refCamRot3d refGlobal ioArray
  
-- /Users/aaa/myfile/bitbucket/tmp/xx_9059.x
  rotateWorldX refCamRot3d
  currRotatedAxis refCamRot3d
  
  when False $ do
    let p0 = Vertex3 0.1 0.1 0.1
    let p1 = Vertex3 0.4 0.4 0.4
    drawArrow3d (p0, p1) [gray, white]
  
    let p4 = Vertex3 0.1 0.1 0.1
    let p5 = Vertex3 0.3 0.3 0.3
    let p5' = Vertex3 0.4 (-0.3) 0.1
    let v55 = p5' -: p5
    let r = 0.1
    drawCylinder3dX r (p4, p5, p5') [green, yellow]
    drawArrow3d (p5', p5 +: (0.2 *: v55)) [green]
  
    -- let p6 = p5
    -- let p7 = Vertex3 0.4 (-0.3) 0.1
    -- let p7' = Vertex3 0.1 0.2 0.4    
    -- let r = 0.1
    -- drawCylinder3dX r (p5, p5', p7') [red, blue]
    -- drawArrow3d (p7, p6 +: (0.2 *: (p7 -: p6))) [magenta, blue]

    let v0 = p4 -: p5
    let v1 = p5 -: p5'
    let vn = case v0 `crossF` v1 of
                     Just v -> v
                     Nothing -> error "ERROR: p4 p5 p6 are colinear"
    
    -- drawArrow3d (p5, p5 +: (0.2 *: uv vn)) [red, green]
    let ang01 = angle2Vector v0 (-v1)
    logFileGT "ang01" [show ang01]
  
  when True $ do
    let vz = Vertex3 0 0 0
    let r0 = 0.1
    let p0 = Vertex3 0 0 0
    let v0 = Vector3 1 0 1
    let c0 = [cyan, blue]
    let t0 = (r0, p0, v0, c0)
    
    let r1 = r0
    let p1 = Vertex3 0 0.6 0
    let v1 = Vector3 1 0 1
    let c1 = [green, yellow]
    let t1 = (r1, p1, v1, c1)
    let cc = [gray, magenta]
    let py = Vector3 0 1 0
    let q₁ = Vertex3 0 0 0
    let q₂ = Vertex3 0.1 0.1 (-0.1)
    let qv = q₁ -: q₂
    (cx0, cx1, _) <- drawCylinderEllipseX t0 t1 cc False
    -- let cy = combineList cx0 cx1
    when False $ do
      drawPrimitiveX TriangleFan (p0:cx0) [green, white]
      drawPrimitiveX TriangleFan (p1:cx1) [red, blue]
      -- drawPrimitiveX TriangleStrip cy cc'
    when False $ do
      let m = rotToVecMat py qv
      let m' = rotToVecMat qv py
      let cx0' = rotVerMap m (p0:cx0) vz
      -- translation from p1 to q₂

      let cx1' = rotVerMap m (p1:cx1) vz
      drawPrimitiveX TriangleFan cx0' [magenta, yellow]
      drawPrimitiveX TriangleFan cx1' [green, red]
      let p1' = m `multiVertex` p1
      drawArrow3d (p0, p1') [white, gray]
      drawPrimitiveX TriangleFan (rotVerMap m' cx0' vz) [red, yellow]
      drawPrimitiveX TriangleFan (rotVerMap m' cx1' vz) [blue, red]
    when False $ do
      p0' <- getRedisStr "p0" >>= \x -> return (read x :: Vertex3 GLdouble)
      p1' <- getRedisStr "p1" >>= \x -> return (read x :: Vertex3 GLdouble)
      p2' <- getRedisStr "p2" >>= \x -> return (read x :: Vertex3 GLdouble)
      {--  
      let isCCW = isCCWXY (p0', p1', p2')
      logFileGT "isCCWxx" [show isCCW]
      --}
      nx <- getRedisStr "num" >>= \x -> return (read x :: Int)
      let num = nx
      -- let p0 = Vertex3 0.1 (-0.2) 0.2
      let r = 0.05
      -- let p0 = Vertex3 (-0.1) (-0.2) (0.2)
      let p0 = p0'
      -- BUG: there is issue when y is negative
      -- let p1 = Vertex3 0.1 (-0.6) (-0.1)
      -- let p1 = Vertex3 0.1 (0.1) (-0.1)
      let p1 = p1'      
      -- let p2 = Vertex3 0.2 (-0.1) (-0.3)
      let p2 = p2'      
      ex <- ellipticThreePts (p0, p1, p2) r num 
      return ()
    when True $ do
      let r = 0.02
      -- 8 9 10 11 23 => 8 9 10, 9 10 11, 10 11 12 => 8 10 11
      -- let pts = ellipticPtYZ (Vertex3 0 0 0)  (0.4, 0.6) 10
      -- 4 5 6 7
      -- 4 5 6
      --   5 6 7
      let pts = [let r = 0.2; alpha = 2*pi/20; x = r * cos (alpha * d); y = r * sin (alpha * d) * cos (alpha * d); z = r * sin (alpha * d); in Vertex3 x y z | d <- [-60..60]]
      -- let pts = [let r = 0.2; alpha = 2*pi/20; x = r * cos (alpha * d); y = d * 0.01; z = r * sin (alpha * d); in Vertex3 x y z | d <- [-60..60]]
      -- let pts = [let r = 0.2; alpha = 2*pi/20; x = r * cos (alpha * d); y = r * sin (alpha * d); z = d * 0.01 in Vertex3 x y z | d <- [-60..60]]
      -- let pts = [let r = 0.2; alpha = 2*pi/20; x = d * 0.01; y = r * cos (alpha * d); z = r * sin (alpha * d); in Vertex3 x y z | d <- [-60..60]]            
      -- let ls = take (len pts - 1) $ listSlide (pts ++ pts) 3
              {--
      let matFlipYZ = [
                [0, 0, 1],
                [0, 1, 0],
                [1, 0, 0]
              ]
              --}
      let matFlipYZ = matId 3
      let pts' = rotVerMap matFlipYZ pts (Vertex3 0 0 0)

      let ls = listSlide pts' 3
      let ls' = map fromJust $ filter (/= Nothing) $ map toTuple3 ls
      let lv = map (\t -> let epsilon = 1e-12 in (detVerProjXYZ t epsilon, t)) ls'
      let lx = map (\((x, y, z), ps) -> x == 0 || y == 0 || z == 0 ? ps $ (0, 0, 0)) lv
      logFileGT "lvxx" $ map show lv
      logFileGT "lxxx" $ map show lx
      let num = 4
      lt <- mapM (\(a:b:c:_) -> do
                    ellipticThreePtsX2 (a, b, c) r num
            ) ls
      -- let lr = listSlide (lt ++ take 1 lt) 2
      let lr = listSlide lt 2
      mapM_ (\(a:b:_) -> do
                let la = combineList a b
                drawPrimitiveX TriangleStrip la [color1, gray]
                ) lr
  
    when False $ do
      let p0 = Vertex3 0 0 0
      let p1 = Vertex3 0.2 0.2 0
      let p2 = Vertex3 0.2 0.4 0.3    
      drawArrow3d (p0, p1) [white, red]
      drawArrow3d (p1, p2) [white, green]

      let vz = Vertex3 0 0 0
      let vx = Vector3 1 0 0
      let num = 10
      let c0 = Vertex3 0 0 0
      let c1 = Vertex3 0.4 0 0
      let b0 = Vertex3 0.1 0.1 0.1
      let b1 = Vertex3 0.2 0.2 0.2
      let v01 = b0 -: b1
      let r = 0.01
      let s0 = ellipticPtYZ c0 (r, r) num
      let s1 = ellipticPtYZ c1 (r, r) num
      let ss = combineList s0 s1
      drawPrimitiveX TriangleStrip ss [white, color1]
      let m = rotToVecMat vx v01
      logFileGT "m33" [show m]
      let  a₁ x = x + 1
      logFileGT "bxx" [show $ a₁ 1]
      let ss' = translateMap (vec_ b0) $ rotVerMap m ss vz
      drawPrimitiveX TriangleStrip ss' [white, red]      
      
      return ()

    return ()
    {--  
    let r00 = 0.2
    let p00 = Vertex3 0 0 0
    let v00 = Vector3 1 0 0
    let c00 = [green, red]
    let t00 = (r00, p00, v00, c00)
    
    let r11 = 0.2
    let p11 = Vertex3 0 0.4 0
    let v11 = Vector3 1 0 0
    let c11 = [green, yellow]
    let t11 = (r11, p11, v11, c11)
    let ccc = [white, blue]
    (cx0, cx1, _) <- drawCylinderEllipseX t00 t11 ccc True
    pp "ok"
    --}
        {--  
    let p0 = Vertex3 0 0 0
        p1 = Vertex3 0.2 0 0
        in drawArrow3d (p0, p1) [blue, yellow]
  
    let p0 = Vertex3 0.2 0 0
        p1 = Vertex3 0 0 0.3
        in drawArrow3d (p0, p1) [gray, white]
        --}  
  when False $ do
    let lc = cls [magenta, red, blue]    
    let nv = Vector3 0.1 0.1 0.1
    let vxz = projXZ nv
    let angX = angle2Vector (Vector3 1 0 0) vxz
    let angUp = angle2Vector nv vxz
    let px = Vertex3 0 0 0
    let majorAxis = 0.2
    let minorAxis = majorAxis / cos angUp
    let (ls, _)= ellipseRot (majorAxis, minorAxis) (-angX, angUp) px
    drawArrow3d (px, px +: nv) [gray, white]
    drawPrimitiveX TriangleFan (px : ls) lc


  -- (r = GLdouble, Vertex3 GLdouble, Vector3 GLdouble) -> (r, Vertex3 GLdouble, Vector3 GLdouble) -> IO()
  -- xxx0
  when False $ do
    let pxz (Vector3 x y z) = Vector3 x 0 z    
    let px = Vertex3 0 0 0
    let nv = Vector3 0.1 0.1 0.1
    let vxz = pxz nv
    let angX = angle2Vector (Vector3 1 0 0) vxz
    let angUp = angle2Vector nv vxz
    let p0 = Vertex3 0 0 0
    let p1 = Vertex3 0 0.4 0
    let majorAxis = 0.2
    let minorAxis = majorAxis / cos angUp

    logFileGT "majorAxis" [show majorAxis] 
    logFileGT "minorAxis" [show minorAxis] 
    logFileGT "angX" [show angX] 
    logFileGT "angUp" [show angUp] 
  
    let (cx0, _) = ellipseRot (majorAxis, majorAxis) (0, 0) p0
    
    let (cx1, _) = ellipseRot (majorAxis, minorAxis) (-angX, angUp) p1
    let t0 = (p0, cx0, [green, blue, gray])
    let t1 = (p1, cx1, [yellow, white, red])

    drawArrow3d (p1, p1 +: nv) [gray, white]    
    drawCylinderEX t0 t1
  
  when False $ do
    let t = (Vertex3 0 0 0, Vertex3 0 0.3 0.1)
    let lc = join $ repeat [cyan, magenta, yellow]
    drawCylinderEllipse t lc
  
  when False $ do
    preservingMatrix $ do
      let la = join $ repeat [magenta, red, blue]
      let lc = join $ repeat [cyan, magenta, yellow]
      let vc = Vertex3 0 0 0 :: Vertex3 GLdouble
      let rw = 0.2
      ang <- getRedisD "ang1"
      let a0 = rw / cos ang
      -- let ls = circleX (a0, rw) vc

      let slot = 7
      let interval = 500
      (_, inx, timeIndex, tup0, inx0, tup1, inx1, animaState) <- readAnimaState animaStateArr slot interval
  
      let f0 = tup0 ^._1
      let f1 = tup1 ^._1
      
      let d = pi/20
      -- let ls = ellipseRot (0.3, 0.2) (d * rf y0, d * rf y1) (Vertex3 0 0 0)
      let (ls, _) = ellipseRot (0.3, 0.2) (d * rf inx0, d * rf inx1) (Vertex3 0 0 0)
      logFileGT "timeIndex" [show timeIndex]
      logFileGT "inx0" [show inx0]
      logFileGT "inx1" [show inx1]
      preservingMatrix $ do
        -- rotate (180/pi * ang) (Vector3 0 0 1 :: Vector3 GLdouble)
        preservingMatrix $ do
          renderPrimitive LineLoop $ mapM_(\v -> do
              color black
              vertex v) ls
          renderPrimitive TriangleFan $ mapM_(\(c, v) -> do
              color c
              vertex v) $ zip lc (vc : ls)
      case timeIndex of
          t | t `elem` [0..9] -> do
                writeAnimaState animaStateArr animaState{tup0_ = (f0, inx0), inx0_ = inx0, inx1_ = inx1}
            | t `elem` [10..19] -> do
                writeAnimaState animaStateArr animaState{tup0_ = (id, inx0), inx0_ = inx0, tup1_ = (\x -> x + 1, inx1), inx1_ = inx1}
            | otherwise -> do
                writeAnimaState animaStateArr animaState{tup0_ = (id, inx0), inx0_ = inx0, tup1_ = (id, inx1), inx1_ = inx1}

      preservingMatrix $ do
        -- rotate (180/pi * (2*ang)) (Vector3 0 1 0 :: Vector3 GLdouble)        
        -- rotate (180/pi * ang) (Vector3 0 0 1 :: Vector3 GLdouble)
        preservingMatrix $ do
          renderPrimitive LineLoop $ mapM_(\v -> do
              color black
              vertex v) ls
          renderPrimitive TriangleFan $ mapM_(\(c, v) -> do
              color c
              vertex v) $ zip la (vc : ls)  
  
  
    isx <- getRedisX "isx"
    when (isx == 0) $ do  
      preservingMatrix $ do
        let lc = join $ repeat [cyan, magenta, yellow]
        let nv = Vector3 0 1 0
        let p0 = Vertex3 0 (-0.3) 0
        let p1 = Vertex3 0 0.3 0
        let r = 0.2
        drawCylinder3d r (p0, p1) (nv, 0) (nv, 0) lc

  when False $ do
    let vx = Vertex3
    let lc = join $ repeat [cyan, magenta, yellow]
    let ld = join $ repeat [white, gray]
    let le = join $ repeat [blue, gray]
    let r = 0.05
    let p0 = vx 0.1 0.1 (-0.2) :: (Vertex3 GLdouble)
    -- let p1 = vx 0.15 (-0.4) (0.3)
    let p1 = vx 0.15 0.4 0.3
    let p2 = vx 0.1 0.5 0.2
    let v01 = p0 -: p1 
    let v12 = p1 -: p2
    let nv = case v01 `crossF` v12 of
                  Just v -> v
                  Nothing -> error "Two vectors are parallel"
    let ang = let a = angle2Vector (-v01) v12 in pi - a > 0 ? pi - a $ a - pi 
    logFileGT "nv_xx" [show nv]
    logFileGT "ang_xx=" [show $ 180/pi * ang]
    logFileGT "ang_xx1/2" [show $ 180/pi * (ang/2)]

--  let m =  let m1 = rotMat vk1 beta in padMat3To4Tran m1 [0, leng, 0, 1]
--  let rm =  padMat3To4Tran (matId 3) [0, -leng, 0, 1]
--
    preservingMatrix $ do
      rd1 <- getRedisX "r11"
      when (rd1 == 0) $ do
        drawCylinder3d r (p0, p1) (nv, 0) (nv, ang/2) le 
      rd2 <- getRedisX "r22"
      when (rd2 == 0) $ do
        drawCylinder3d r (p1, p2) (nv, -ang/2) (nv, 0) lc
    {--
    preservingMatrix $ do
      let leng = nr v01
      let m =  let m1 = rotMat nv (pi/3) in padMat3To4Tran m1 [0, leng, 0, 1]
      let rm =  padMat3To4Tran (matId 3) [0, -leng, 0, 1]
      multiModelviewMatD $ join $ rm `multiMat` m 
      drawCylinder3d r (p0, p1) (nv, 0) (nv, 0) lc
    -- drawCylinder3d r (p1, p2) (nv, 0) (nv, 0) ld
    --}
    preservingMatrix $ do
      drawArrow3dX (p1, p1 +: nv) [red, blue]
      drawArrow3dX (p2, p2 +: nv) [cyan, green]
      drawArrow3dX (p0, p0 +: nv) [gray, white]

  isShownAxis <- readIORef refCamRot3d <&> isShownAxis_
  when isShownAxis $ do
    drawAxis (Vector3 1 0 0) [red, fmap (*0.5) red]
    drawAxis (Vector3 0 1 0) [green, fmap (*0.5) green]
    drawAxis (Vector3 0 0 1) [blue, fmap (*0.5) blue]
    drawCubeQuad 0.02

    bracket
      (redisConnectDefault)
      (redisDisconnect)
      (\conn -> do
        -- θ <- readAndParse "/tmp/aa.x"
        θ <- redisGetConn conn "kk0" <&> \x -> read (fromMaybe "0" x) :: GLfloat 
        return ()
        {--
        let k = Vector3 0 1 0
        let m = (map . map) rf $ padMat3To4 $ rotMat k θ
        multiModelviewMat $ join m
        preservingMatrix $ do
          drawAxis (Vector3 1 0 0) [red, fmap (*0.5) red]
          drawAxis (Vector3 0 1 0) [green, fmap (*05) green]
          drawAxis (Vector3 0 0 1) [blue, fmap (*05) blue]
          drawCubeQuad 0.02
        --}
      ) 
    
  showCurrBoardArr ioArray

  isShownGrid <- readIORef refCamRot3d <&> isShownGrid_
  when isShownGrid $ do
    renderCoordinates
    drawRectGridX initRectGrid

  when False $ do
    let slot = 8
    let interval = 500
    (_, inx, timeIndex, tup0, inx0, _, _, animaState) <- readAnimaState animaStateArr slot interval
    let fn = "/tmp/img/img" ++ (inx0 < 10 ? "0" ++ show inx0 $ show inx0) ++ ".png"
    saveImageOpenGL w3d fn
    writeAnimaState animaStateArr animaState{tup0_ = ((+)1, inx0), inx0_ = inx0}
  
  endWindow3d w3d

  beginWindow2d w2d
  when True $ do
    let pts = ptsList 
    -- pts <- rfl "./aa.x" >>= \cx -> return $ map (\x -> read x :: (Vertex3 GLfloat)) cx
    -- let pts = [Vertex3 0 0 0, Vertex3 0.4 0.4 0, Vertex3 0.2 (-0.3) 0, Vertex3 (-0.3) 0.35 0] 
    let pair x = zip (init x) (tail x)
    let co = join $ repeat [green, yellow, cyan, gray, magenta, white, blue]
    preservingMatrix $ 
      mapM_ (\t -> do 
                  drawArrowX t 
           ) $ pair pts 

  when False $ do
    let pts = ptsList 
    -- drawPolygon pts
    -- cylinderArrow 0.5 [yellow, green, blue]
    drawCircle' (Vertex3 0 0 0) 0.05 
    drawArrow (Vertex3 0.1 0.1 0, Vertex3 0.5 0.5 0) 
    drawArrow (Vertex3 0.0 0.0 0, Vertex3 (-0.5) 0.5 0) 
    preservingMatrix $ do
      GL.scale (1 :: GLfloat) 1 1
      drawArrow (Vertex3 0.0 0.0 0, Vertex3 (-0.5) 0.5 0) 
    preservingMatrix $ do
      drawArrow (Vertex3 0.1 0.1 0, Vertex3 0.5 0.5 0) 

  when False $ do
    let pts = ptsList 
    let co = join $ repeat [green, yellow, cyan, gray, magenta, white, blue]
    preservingMatrix $ 
      mapM_ (\(c, v) -> do 
                  drawDotXX c v 0.02
           ) $ zip co pts 
  
  r2 <- redisGet "r2" <&> \s -> case s of
                                  Just x -> read x :: Bool 
                                  Nothing -> False

  rls <- redisGet "rls" <&> \s -> case s of
                                  Just x -> read x :: [GLfloat] 
                                  Nothing -> [0.02, 0.02 + 0.02 .. 0.1] 
  when r2 $ do
    let ax = [0.02, 0.02 + 0.02 .. 0.90] :: [GLdouble] 
    let cx = map (\y -> Vertex3 0 y 0) ax 
    let cy = map (\(x, y) -> Vertex3 x y 0) $ zip ax ax 
    let pair x = zip (init x) (tail x)
    let co = join $ repeat [green, yellow, cyan, gray, magenta, white, blue]
    preservingMatrix $ do 
      -- translate (Vector3 (-0.90) 0 0 :: Vector3 GLdouble) 
      mapM_ (\t -> do 
                  drawArrowN1 t 
                  -- drawArrowX t 
           ) $ zip cx cy 
      logFileG ["cx=" ++ show cx]
      logFileG ["cy=" ++ show cy]
      logFileG ["zipcxcy=" ++ (show $ zip cx cy)]

  when False $ do
    preservingMatrix $ do 
      let cc = [cyan, magenta, yellow]
      drawArrowX (Vertex3 0 0 0, Vertex3 0.1 0.1 0)    
    preservingMatrix $ do 
      let cc = [cyan, magenta, yellow]
      drawArrowX (Vertex3 0.1 0.1 0, Vertex3 (-0.3) 0.5 0)    

  when False $ do
    let width = 0.3
    let height = 0.2
    delta <- getRedisXf "delta" <&> rf
    str <- getRedisStr "str"
    let s = DT.readMaybe str :: (Maybe [GLfloat])
    let ls = fromMaybe [] s
    -- n = len ls > 0 ? last ls $ 10
    n <- getRedisXf "n" <&> rf

    let anima1 = 6
    xx <- getRedisX "int"
    let interval = xx
    (isNext1, index1, _, tup0, inx0, tup1, inx1,animaState1) <- readAnimaState animaStateArr anima1 interval
    let del = pi/100
    let lv = [[Vertex3 (1/n*x) (1/n*y) 0 | x <- [0.0..n]]| y <- [0.0..n]] :: [[Vertex3 GLfloat]]
    renderPrimitive Points $ mapM_(\v@(Vertex3 x y z) -> do
-- /Users/aaa/myfile/bitbucket/tmp/xx_6177.x
      let dl = sdfRect3d (Vertex3 0.2 0.3 0.4) v False
      case dl of
         -- sd | abs sd <= delta -> do color yellow; vertex (let m = rotz $ (del * rf index1) in mulMat m v);
         sd | abs sd <= delta -> do
              let m = rotx $ (del * rf index1)
              color magenta; vertex $ mulMat m $ v;
              color yellow;  vertex $ mulMat m $ nx_1 v;
              color blue;    vertex $ mulMat m $ nx_2 v;
              color cyan;    vertex $ mulMat m $ nx_12 v;
            --  | sd > 0          -> do color gray;    vertex v; vertex $ nx_1 v; vertex $ nx_2 v; vertex $ nx_12 v;
            --  | otherwise       -> do color white;   vertex v; vertex $ nx_1 v; vertex $ nx_2 v; vertex $ nx_12 v;
            | otherwise -> return ()
     ) $ join lv


    if index1 >= 200 then do
      writeAnimaState animaStateArr animaState1{animaIndex_ = 0}
    else do
      writeAnimaState animaStateArr animaState1

  drawDot (Vertex3 0 0 0)
  endWindow2d w2d

  -- saveImageFrame w animaStateArr

  -- saveImageOpenGL w "/tmp/img0.png"
  -- draw 20 x 20 grid
  -- drawFinal w initRectGrid
  -- KEY: draw  grid
  -- drawRectGridX initRectGrid
  -- END_triangulation
  -- G.swapBuffers w

  -- G.pollEvents
  mainLoop (w3d, w2d) (refCamRot3d, refCamRot2d) refGlobal refGlobalFrame animaStateArr lssVex ioArray

main = do
  print fpath
  argList <- getArgs
  if len argList > 0
    then do
      case head argList of
        "-h" -> helpme
        _ -> do
          let s = head argList
          mymain s
          print $ "Wrong option => " ++ head argList ++ ", -h => Help"
    else do
      mymain [] 

