{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

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
import PlotGeometryLib

mymain :: IO ()
mymain = do
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
      -- writeIORef refGlobal $ setDrawPts   globalRef spherePtsX
      modifyIORef refGlobal (\x -> x {drawPts_ = spherePtsX})
      globalRef2 <- readIORef refGlobal
      -- writeIORef refGlobal $ setRandomPts globalRef2 randomPts
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


drawArrowProj :: [(Vertex3 GLfloat, Vertex3 GLfloat)] -> (Bool, Bool, Bool) -> IO()
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

{--
drawSphereNX_1::Int -> GLfloat ->Int -> Bool -> [Color3 GLdouble] -> (GLfloat -> GLfloat)-> IO()
drawSphereNX_1 n radius k isFilled cc f = do
    let δ = 2*pi / rf n :: GLfloat
        ϵ = pi / rf n :: GLfloat
        r = radius
        fx::Int -> Int -> GLfloat
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ * i'
                     β  = ϵ * j'
                 in r * cos β * f α
        fy::Int -> Int -> GLfloat
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ * i'
                     β  = ϵ * j'
                 in r * sin α
        fz::Int -> Int -> GLfloat
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ * i'
                     β  = ϵ * j'
                 in r * cos α * sin β
        in drawParamSphereX fx fy fz n k isFilled cc
--}

mainLoop ::
  (G.Window, G.Window) ->
  (IORef CameraRot, IORef CameraRot) ->
  -- IORef Step ->
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

  when False $ do
    preservingMatrix $ do
      let cc = [green, blue, cyan, magenta, yellow]
      drawArrow3dCen (Vector3 0.2 0.2 0.2) cc 

  when False $ do
    ls <- rfl "./bb.x" >>= \cx -> return $ map (\x -> read x :: (Vertex3 GLfloat, Vertex3 GLfloat)) cx
    drawArrowProj ls (True, False, False)

  when False $ do
    let cc = [green, blue, cyan, magenta, yellow]
    let p0 = Vertex3 0.2 0.2    (-0.2)
    let p1 = Vertex3 0.1 (-0.1) (-0.3)

    let q0 = Vertex3 0.0 0.0 0.0
    let q1 = Vertex3 0.7 0.01 0.0
    let q2 = Vertex3 0.02 0 (-0.6)
    let ls = [(q0, p0), (q0, p1), (p0, p1), (q0, q1), (q0, q2)]

    let t = (q0, q1, q2)
    let vx = intersectLineTri (p0, p1) t
    case vx of
      Just px ->  do 
        pp "ok"
        -- drawCubeQuadX (p0 -: px) 0.01
      Nothing -> pp "do not intersect line and plane"
    renderPrimitive TriangleStrip $ mapM_ (\(c, v) -> do
                                          color c
                                          vertex v
                                        ) $ zip cc [q0, q1, q2] 
    mapM_ (\t -> drawArrow3d t cc) ls

  when False $ do
    let cc = [green, blue, cyan, magenta, yellow]
    ls <- rfl "./cc.x" >>= \cx -> return $ map (\x -> read x :: (Vertex3 GLfloat, Vertex3 GLfloat)) cx
    mapM_ (\t -> drawArrow3d t cc) ls

  when False $ do
    preservingMatrix $ do
      let cc = [green, blue, cyan, magenta, yellow]
      GL.scale (1:: GL.GLdouble) 2.0 1
      drawTorusX 0.1 0.2 20 cc

  when True $ do
    preservingMatrix $ do
      mapM_ (\x -> do
        let cc = [green, blue, cyan, magenta, yellow]
        -- let cc = [gray, white]
        let isFilled = False 
        -- translate (Vector3 (0.2 * x) 0 0 :: Vector3 GLdouble)
        -- drawSphereN 10 0.4 cc
        drawSphereNX 40 0.4 30 isFilled cc
        -- mapM_ (\f -> drawSphereNX_1 10 0.4 1 isFilled cc f) [sin, cos, tan, f, g]
            ) [1]

  when True $ do
    drawParaboloid

  when False $ do
    preservingMatrix $ do 
      let cc = [green, blue, cyan, magenta, yellow]
      let rad = 0.1
      let leng = 0.4
      let leftClose = True
      let rightClose = True
      cylinderXX rad leng (leftClose, rightClose) cc    

  when False $ do
    preservingMatrix $ do 
      let cc = [green, blue, cyan, magenta, yellow]
      cylinderArrowXX 0.8 cc    

  when False $ do
    preservingMatrix $ do 
      let cc = [cyan, magenta, yellow]
      drawArrowX (Vertex3 0 0 0, Vertex3 0.1 0.1 0)    
    preservingMatrix $ do 
      let cc = [cyan, magenta, yellow]
      drawArrowX (Vertex3 0.1 0.1 0, Vertex3 (-0.3) 0.5 0)    

  preservingMatrix $ do
    renderCoordinates

  when False $ do
    preservingMatrix $ do
      let ax = map (\x -> Vertex3 (0.2 * x) 0.3 0) [0..3]
      let bx = map (\x -> Vertex3 (0.2 * x) 0.7 0) [0..3]
      mapM_ (drawSegmentFromTo yellow) [ax, bx]
      let g a b = let h x y = zipWith (\a b -> [a, b]) x y
                  in join $ h a b
      let zm = join $ zipWith(\a b -> [a, b]) ax bx
      let zm' = zip [yellow, white, green, blue, cyan, red, colorChange 0.5 yellow, colorChange 0.5 cyan] zm
      let ls = [Vertex3 0 0 0] :: [Vertex3 GLfloat]    
      renderPrimitive TriangleStrip $ mapM_ (\(c, v) -> do
                                            color c
                                            vertex v
                                          ) zm'

  curStr <- getStr refGlobal
  show3dStr curStr red 0.8
  logFileG ["str_=" ++ show curStr]
  isPaused <- readIORef refGlobal <&> isPaused_
  unless isPaused $ do
    (index, isNext, currFrame) <- readRefFrame2 refGlobalFrame 1000
    --                                                  |
    --                                                  + -> speed, larger = slower
    let slotNum0 = 0
    (isNext0, index, animaState) <- readAnimaState animaStateArr slotNum0 1000

    -- logFileG ["index=" ++ show index]
    logFileG ["isNextX=" ++ show isNext0 ++ " animaIndex_=" ++ show (animaIndex_ animaState)]
    -- my <- readIORef refGlobal >>= return . moveY_
    -- KEY: falling block, drop block


  when True $ do
    drawAxis (Vector3 1 0 0) [red, fmap (*0.5) red]
    drawAxis (Vector3 0 1 0) [green, fmap (*05) green]
    drawAxis (Vector3 0 0 1) [blue, fmap (*05) blue]
    drawCubeQuad 0.02

    bracket
      (redisConnectDefault)
      (redisDisconnect)
      (\conn -> do
        -- θ <- readAndParse "/tmp/aa.x"
        θ <- redisGetConn conn "kk0" <&> \x -> read (fromMaybe "0" x) :: GLfloat 

        let k = Vector3 0 1 0
        let m = (map . map) rf $ padMat3To4 $ rotMat k θ
        multiModelviewMat $ join m
        preservingMatrix $ do
          drawAxis (Vector3 1 0 0) [red, fmap (*0.5) red]
          drawAxis (Vector3 0 1 0) [green, fmap (*05) green]
          drawAxis (Vector3 0 0 1) [blue, fmap (*05) blue]
          drawCubeQuad 0.02
      ) 
    
  -- drawFinal w ioArray initRectGrid
  showCurrBoardArr ioArray
  drawRectGridX initRectGrid
  -- G.swapBuffers w
  endWindow3d w3d

  beginWindow2d w2d
  when True $ do
    -- let pts = ptsList 
    pts <- rfl "./aa.x" >>= \cx -> return $ map (\x -> read x :: (Vertex3 GLfloat)) cx
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
  
  when True $ do
    -- let pts = ptsList 
    pts <- rfl "./aa.x" >>= \cx -> return $ map (\x -> read x :: (Vertex3 GLfloat)) cx
    -- let pts = [Vertex3 0 0 0, Vertex3 0.4 0.4 0, Vertex3 0.2 (-0.3) 0, Vertex3 (-0.3) 0.35 0] 
    let pair x = zip (init x) (tail x)
    let co = join $ repeat [green, yellow, cyan, gray, magenta, white, blue]
    preservingMatrix $ 
      mapM_ (\t -> do 
                  drawArrowX t 
           ) $ pair pts 
    mapM_ (\v -> drawCircleFilled green v 0.02) pts
    
  r1 <- redisGet "r1" <&> \s -> case s of
                                  Just x -> read x :: Bool 
                                  Nothing -> False
  when r1 $ do
    let ax = [0.02, 0.02 + 0.02 .. 0.90] 
    let cx = map (\x -> Vertex3 x 0 0) ax 
    let cy = map (\(x, y) -> Vertex3 x y 0) $ zip ax ax 
    let pair x = zip (init x) (tail x)
    let co = join $ repeat [green, yellow, cyan, gray, magenta, white, blue]
    preservingMatrix $ 
      mapM_ (\t -> do 
                  drawArrowX t 
           ) $ zip cx cy 
{--
 
   [(0, 0.30),    (0, 0.32),   (0, 0.34)]
   [(0.30, 0.30), (0.32, 0.32) (0.34, 0.34)]

   0.28 0.30 0.32 0.34
   0.22 0.24 0.26 0.28 0.30 0.32 0.24
 --}

  
  r2 <- redisGet "r2" <&> \s -> case s of
                                  Just x -> read x :: Bool 
                                  Nothing -> False

  rls <- redisGet "rls" <&> \s -> case s of
                                  Just x -> read x :: [GLfloat] 
                                  Nothing -> [0.02, 0.02 + 0.02 .. 0.1] 
  when r2 $ do
    let ax = [0.02, 0.02 + 0.02 .. 0.90] :: [GLfloat] 
    let cx = map (\y -> Vertex3 0 y 0) ax 
    let cy = map (\(x, y) -> Vertex3 x y 0) $ zip ax ax 
    let pair x = zip (init x) (tail x)
    let co = join $ repeat [green, yellow, cyan, gray, magenta, white, blue]
    preservingMatrix $ do 
      -- translate (Vector3 (-0.90) 0 0 :: Vector3 GLdouble) 
      mapM_ (\t -> do 
                  drawArrowXX t 
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
    str <- getRedisXStr "str"
    let s = DT.readMaybe str :: (Maybe [GLfloat])
    let ls = fromMaybe [] s
    -- n = len ls > 0 ? last ls $ 10
    n <- getRedisXf "n" <&> rf

    let anima1 = 6
    xx <- getRedisX "int"
    let interval = xx
    (isNext1, index1, animaState1) <- readAnimaState animaStateArr anima1 interval
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
  argList <- getArgs
  if len argList > 0
    then do
      case head argList of
        "-h" -> helpme
        _ -> do
          print $ "Wrong option => " ++ head argList ++ ", -h => Help"
    else do
      mymain

