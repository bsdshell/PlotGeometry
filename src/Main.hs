{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

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
    mw3d <- G.createWindow 1000 1000 "PlotGeometry 3D" Nothing Nothing
    mw2d <- G.createWindow 1000 1000 "PlotGeometry 2D" Nothing Nothing
    -- maybe' :: Maybe a -> b -> (a -> b) -> b  
    -- maybe' mw (G.terminate >> exitFailure) $ \window -> do
    maybeX' (mw3d, mw2d) (G.terminate >> exitFailure)  $ \(window3d, window2d) -> do      
      -- ref <- newIORef initCam
      refCamRot <- newIORef initCameraRot
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
      mainLoop (window3d, window2d) refCamRot refGlobal refFrame animaStateArr cx' ioArray

      G.destroyWindow window3d
      G.destroyWindow window2d
      G.terminate
      exitSuccess

  
mainLoop ::
  (G.Window, G.Window) ->
  IORef CameraRot ->
  -- IORef Step ->
  IORef GlobalRef ->
  IORef FrameCount ->
  IOArray Int AnimaState ->
  [[Vertex3 GLfloat]] ->
  DAO.IOArray (Int, Int, Int) BlockAttr ->
  IO ()
mainLoop (w2d, w3d) refCamRot refGlobal refGlobalFrame animaStateArr lssVex ioArray = unlessX' (G.windowShouldClose w2d) (G.windowShouldClose w3d) $ do

  G.getWindowFocused w2d >>= \b -> when b $ G.setKeyCallback w2d (Just $ keyBoardCallBack2d refCamRot refGlobal ioArray)
  G.getWindowFocused w3d >>= \b -> when b $ G.setKeyCallback w3d (Just $ keyBoardCallBack3d refCamRot refGlobal ioArray)
  G.getWindowFocused w2d >>= \b -> when b $ G.setMouseButtonCallback w2d (Just $ mouseCallbackX refGlobal) 
  G.getWindowFocused w3d >>= \b -> when b $ G.setMouseButtonCallback w3d (Just $ mouseCallbackX refGlobal)

  beginWindow3d w3d refCamRot refGlobal ioArray
  
-- /Users/aaa/myfile/bitbucket/tmp/xx_9059.x
  rotateWorldX refCamRot
  
  let cc = [green, blue, cyan, magenta, yellow]
  preservingMatrix $ do
    GL.scale (1:: GL.GLdouble) 2.0 1
    drawTorus 0.1 0.2 10 cc

  preservingMatrix $ do
    renderCoordinates
    pp "ok"
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

    -- θ <- readAndParse "/tmp/aa.x"
    conn <- redisConnectDefault
    -- s  <- redisGetConn conn "kk0"
    θ <- redisGetConn conn "kk0" <&> \x -> case x of
                                      Just s -> case DT.readMaybe s :: Maybe GLfloat of
                                                     Just x -> x
                                                     Nothing -> 0
                                      Nothing -> 0
    let k = Vector3 0 1 0
    let m = (map . map) rf $ padMat3To4 $ rotMat k θ
    multiModelviewMat $ join m
    preservingMatrix $ do
      drawAxis (Vector3 1 0 0) [red, fmap (*0.5) red]
      drawAxis (Vector3 0 1 0) [green, fmap (*05) green]
      drawAxis (Vector3 0 0 1) [blue, fmap (*05) blue]
      drawCubeQuad 0.02
    redisDisconnect conn
    pp "ok"

  -- drawFinal w ioArray initRectGrid
  showCurrBoardArr ioArray
  drawRectGridX initRectGrid
  -- G.swapBuffers w

  endWindow3d w3d

  -- xx1
  -- window 1
  beginWindow2d w2d
  when True $ do
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
  mainLoop (w2d, w3d) refCamRot refGlobal refGlobalFrame animaStateArr lssVex ioArray

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

