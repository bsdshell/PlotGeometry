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
      PrimitiveMode(Triangles, TriangleStrip, LineLoop, Quads, TriangleFan, Points),
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
circlePtX (Vertex3 x0 y0 z0) r =[let alpha = (pi2*n)/num in Vertex3 ((rf r)*sin(alpha) + x0) ((rf r)*cos(alpha) + y0) (0 + z0) | n <- [1..num]]
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
   c      = d/(dot v v)
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

{-|
 - KEY: 
 - NOTE: right hand rule
 -}
isCCW3d :: (Floating a, Ord a) => (Vertex3 a, Vertex3 a, Vertex3 a) -> Bool 
isCCW3d (p0, p1, p2) | not $ ((abs $ ang - pi/2) < epsilon) && ang < pi/2 = True
                     | not $ ((abs $ ang - pi/2) < epsilon) && ang > pi/2 = False
                     | otherwise = error "ERROR: isCCW3d"
  where
    epsilon = 1e-12
    v10 = p1 -: p0
    v12 = p1 -: p2
    vn = v10 `crossF` v12
    ang = case vn of
            Just v -> angle2Vector v (Vector3 0 1 0)     
            Nothing -> error "ERROR: three pts does not form a triangle"


isCCW :: (Floating a, Ord a) => (Vertex3 a, Vertex3 a, Vertex3 a) -> (Vector3 a) -> Bool
isCCW (p0, p1, p2) up = if notAll ve then (ang < pi/2 ? True $ False) else ((x' + y' + z') > 0 ? True $ False)
  where
    v01 = p0 -: p1
    v12 = p1 -: p2
    vc = v01 `crossF` v12
    notAll (Vector3 x y z) = x /= 0 && y /= 0 && z /= 0
    ve = case vc of
            Just v -> v
            Nothing -> error "ERROR: three pts are colinear"
    ang = angle2Vector ve up 
    x' = ve_1 ve 
    y' = ve_2 ve
    z' = ve_3 ve
        
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

  -- xxx
  when True $ do
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

  when True $ do
    argStr <- readIORef refGlobal <&> argStr_
    ls <- null argStr ? readGLScriptDraw fpath $ let arg = parseArg argStr in readGLScriptDraw (fpath_ arg)
    logFileGT "fpath" $ map show ls

    bufferMap <- readIORef refGlobal <&> bufferMap_
    let lb = map snd $ DM.toList bufferMap
    mapM_ (\(shape, buffer) -> do
      case shape of 
        c | c == "arrow" -> do 
            let mls = map (\x -> DT.readMaybe x :: Maybe (Vertex3 GLdouble, Vertex3 GLdouble)) buffer 
            let pts = map (fromJust) $ filter (/= Nothing) mls
            let c1 = [green, blue]
            let c2 = [cyan, magenta, yellow]
            let c3 = [magenta, yellow]
            let p0  = Vertex3 0 0.1 0.1 
            let p1  = Vertex3 0 0.3 (-0.2) 
            let p2  = Vertex3 0 0.2 0.3 
            mapM_ (\t -> drawArrow3dX t c1) pts
          | c == "triangle" -> do 
            logFileGT "triangle_me" [c]
            let mls = map (\x -> DT.readMaybe x :: Maybe (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble)) buffer 
            let pts = map (fromJust) $ filter (/= Nothing) mls
            let c1 = [green, blue]
            let c2 = [cyan, magenta, yellow]
            let c3 = [magenta, yellow]
            let tri = (Vertex3 0 0 0, Vertex3 0.4 0 0, Vertex3 0 0 (negate 0.5) :: (Vertex3 GLdouble))
            mapM_ (\t -> drawTriangleVexX t c1) pts
          | c == "curve" -> do 
            let mls = map (\x -> DT.readMaybe x :: Maybe (Vertex3 GLdouble)) buffer 
            let pts = map (fromJust) $ filter (/= Nothing) mls
            let c1 = [green, blue]
            let c2 = [cyan, magenta, yellow]
            let c3 = [magenta, yellow]
            drawSegmentFromToD red pts
          | c == "cube" -> do 
            let mls = map (\x -> DT.readMaybe x :: Maybe (GLdouble, Vertex3 GLdouble)) buffer 
            let pts = map (fromJust) $ filter (/= Nothing) mls
            let c1 = [green, blue]
            let c2 = [cyan, magenta, yellow]
            let c3 = [magenta, yellow]
            mapM_ (\t -> drawCubeQuadT t) pts 
          | otherwise -> return ()
          ) lb 

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

