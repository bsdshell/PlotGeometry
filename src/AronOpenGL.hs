module AronOpenGL where

-- import Graphics.UI.GLUT
-- import Graphics.UI.GLUT.Callbacks.Global
import AronGraphic hiding (dist)
import AronModule hiding (rw)
import Codec.Picture
import Codec.Picture.Png
import Codec.Picture.Types
import Control.Lens hiding (pre, re)
import Control.Monad
import Control.Monad (unless, when)
import qualified Data.Array.IO as DAO
import Data.IORef
import qualified Data.Map as DM
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector.Mutable as DVM
import qualified Data.Vector.Storable as STO
import qualified Data.Vector.Storable.Mutable as STOM
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GLU.Matrix as GM
import qualified Graphics.UI.GLFW as G
import qualified Graphics.UI.GLUT as GLUT
import System.Exit
import System.IO
import qualified Text.Printf as PR

-- |
--    === Vertex3 List, 25 vertices on x-y-plane only
vertexList =
  [ Vertex3 0.9 0.8 0,
    Vertex3 0.5 0.1 0,
    Vertex3 0.7 0.1 0,
    Vertex3 0.2 0.4 0,
    Vertex3 0.4 0.29 0,
    Vertex3 0.9 0.7 0,
    Vertex3 0.71 0.1 0,
    Vertex3 0.2 0.9 0,
    Vertex3 0.23 0.3 0,
    Vertex3 0.5 0.5 0,
    Vertex3 0.1 0.9 0,
    Vertex3 0.2 0.31 0,
    Vertex3 0.471 0.21 0,
    Vertex3 0.442 0.34 0,
    Vertex3 0.2333 0.6 0,
    Vertex3 0.555 0.245 0,
    Vertex3 0.111 0.399 0,
    Vertex3 0.222 0.231 0,
    Vertex3 0.89 0.33 0,
    Vertex3 0.21 0.31 0,
    Vertex3 0.69 0.13 0,
    Vertex3 0.121 0.51 0,
    Vertex3 0.49 0.43 0,
    Vertex3 0.44 0.66 0,
    Vertex3 0.49 0.26 0
  ] ::
    [Vertex3 GLfloat]

-- |
--    === GLfloat pts
pts =
  [ 0.3,
    0.1,
    0.4,
    0.2,
    0.32,
    0.19,
    0.21,
    0.39,
    0.19,
    0.09,
    0.29,
    0.239
  ] ::
    [GLfloat]

_STEP = 1.0

data RectGrid = RectGrid
  { minX_ :: Float,
    maxX_ :: Float,
    maxY_ :: Float,
    minY_ :: Float,
    xCount_ :: Int,
    yCount_ :: Int,
    xEdge_ :: Float,
    yEdge_ :: Float,
    rotStep :: Int
  }
  deriving (Show, Eq)

data Cam = Cam {alpha :: Double, beta :: Double, gramma :: Double, dist :: Double} deriving (Show)

data CameraRot = CameraRot
  { alpha_ :: Double,
    vecRotX_ :: Vector3 GLdouble,
    beta_ :: Double,
    vecRotY_ :: Vector3 GLdouble,
    gramma_ :: Double,
    vecRotZ_ :: Vector3 GLdouble,
    xyzRotDeg_ :: Double,
    xyzRotVec_ :: Vector3 GLdouble,
    currXYZ_ :: Int,  -- x-axis = 1, y-axis = 2, z-axis = 3
    
    dist_ :: Double,
    fovDeg_ :: Double,
    delta_ :: Step,
    persp_ :: Persp,
    modelview_ :: ModelView,

    rotX_ :: GLdouble,
    vecX_ :: Vector3 GLfloat,
    vecAxisX_ :: Vector3 GLfloat,
    rotY_ :: GLdouble,
    vecY_ :: Vector3 GLfloat,
    vecAxisY_ :: Vector3 GLfloat,
    rotZ_ :: GLdouble,
    vecZ_ :: Vector3 GLfloat,
    vecAxisZ_ :: Vector3 GLfloat,
    coordFrame_ :: (Vector3 GLfloat, Vector3 GLfloat, Vector3 GLfloat),
    coordFrameMat_ :: [[GLfloat]]
  }
  deriving (Eq, Show)

data Persp = Persp
  { fov_ :: Double,
    aspect_ :: Double,
    zf_ :: Double,
    zn_ :: Double
  }
  deriving (Eq, Show)

data ModelView = ModelView
  { eye_ :: Vertex3 GLdouble,
    at_ :: Vertex3 GLdouble,
    up_ :: Vector3 GLdouble
  }
  deriving (Eq, Show)

data Step = Step {xx :: Double, yy :: Double, zz :: Double, ww :: Double} deriving (Eq, Show)

data XYZAxis = XYZAxis {xa :: Bool, ya :: Bool, za :: Bool} deriving (Eq, Show)

-- data GlobalRef = GlobalRef{str_::String, cursor_::(Int, Int), xyzAxis_::XYZAxis} deriving (Show)
-- data GlobalRef = GlobalRef{str_::String, cursor_::(GLfloat, GLfloat), xyzAxis_::XYZAxis} deriving (Show)

{--
data Block_ = Block_ {
                     blockGrid_ :: [[(Int, Int, Color3 GLdouble)]]
                     } deriving (Show, Eq)
--}

data BlockAttr = BlockAttr
  { isFilled_ :: Bool,
    typeId_ :: Int,
    tetrisNum_ :: Int,
    color_ :: Color3 GLdouble
  }
  deriving (Show, Eq)

data GlobalRef = GlobalRef
  { str_ :: String,
    windowSize_ :: (Int, Int),
    cursor_ :: (GLfloat, GLfloat),
    xyzAxis_ :: XYZAxis,
    mousePressed_ :: (Bool, (GLfloat, GLfloat)),
    drawRectX_ :: (Vertex3 GLfloat, Vertex3 GLfloat),
    tranDrawRectX_ :: Vector3 GLdouble,
    fovDegree_ :: GLdouble,
    drawPts_ :: [[Vertex3 GLfloat]],
    randomPts_ :: [Vertex3 GLfloat],
    randomWalk_ :: [Vertex3 GLfloat],
    randomWalkInt_ :: [(Int, Int)],
    boardMap_ :: DM.Map (Int, Int) (Vertex3 GLfloat, Color3 GLdouble, Bool),
    boardMap1_ :: DM.Map (Int, Int) (Int, Int, Color3 GLdouble),
    moveX_ :: Int,
    moveY_ :: Int,
    block1_ :: [((Int, Int), Color3 GLdouble)],
    rectGrid_ :: RectGrid,
    centerBrick_ :: [[(Int, Int)]],
    rot_ :: Bool,
    rotDeg_ :: Double,
    time1_ :: Integer,
    count1_ :: Integer,
    rotN_ :: Int,
    --                 |    |     |
    --                 x    y    1 = Show Brick
    bk1_ :: [[Int]],
    bk2_ :: [[(Int, Int, Color3 GLdouble)]],
    --           |    |
    --           |    |
    --           |  block type rep count
    --           |
    --        block type
    blockCount_ :: Int,
    tetrisCount_ :: Int,
    --                                      shape
    --                        color           |
    --            tetris type   |             |
    --       Global ID  |       |             |
    --             |    |       |             |
    tetris1_ :: (Int, Int, Color3 GLdouble, [[Int]]),
    tetris1X_ :: (BlockAttr, [[Int]]),
    isPaused_ :: Bool
  }
  deriving (Show)

initCam = Cam {alpha = 0.0, beta = 0.0, gramma = 0.0, dist = 0.0}

initCameraRot =
  CameraRot
    { alpha_ = 0.0,
      vecRotX_ = Vector3 1 0 0,
      beta_ = 0.0,
      vecRotY_ = Vector3 0 1 0,
      gramma_ = 0.0,
      vecRotZ_ = Vector3 0 0 1,
      xyzRotDeg_ = 0.0,
      xyzRotVec_ = Vector3 1 0 0,
      currXYZ_ = 1,
      
      dist_ = 0.0,
      fovDeg_ = 60.0,
      delta_ = Step {xx = 0, yy = 0, zz = 0, ww = 0},
      persp_ = Persp {fov_ = 90, aspect_ = 1.0, zn_ = 0.5, zf_ = 0.5 + 4.0},
      modelview_ = ModelView {eye_ = Vertex3 0.2 0.2 0.2, at_ = Vertex3 0 0 0, up_ = Vector3 0 1.0 0},
      
      rotX_ = 0.0,
      vecX_ = Vector3 1 0 0,
      vecAxisX_ = Vector3 1 0 0,
      rotY_ = 0.0,
      vecY_ = Vector3 0 1 0,
      vecAxisY_ = Vector3 0 1 0,
      rotZ_ = 0.0,
      vecZ_ = Vector3 0 0 1,
      vecAxisZ_ = Vector3 0 0 1,
      coordFrame_ = (Vector3 1 0 0, Vector3 0 1 0, Vector3 0 0 1),
      coordFrameMat_ = matId 4
    }

initStep = Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = 0.01}

-- |
--    === keyboard call back function
--
--    >data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double} deriving(Show)
--
--    TODO: combine Cam{..} and Step{..} here
--         do all the calculation here, remove keyboardRot
keyBoardCallBack :: IORef Step -> G.KeyCallback
keyBoardCallBack ref window key scanCode keyState modKeys = do
  ps "keyBoardCallBack in $b/haskelllib/AronOpenGL.hs"
  putStrLn $ "inside =>" ++ show keyState ++ " " ++ show key
  case keyState of
    G.KeyState'Pressed -> do
      -- write Step{...} to ref
      case key of
        k
          | k == G.Key'Right -> writeIORef ref Step {xx = _STEP, yy = 0.0, zz = 0.0, ww = 0.0}
          | k == G.Key'Left -> writeIORef ref Step {xx = (- _STEP), yy = 0.0, zz = 0.0, ww = 0.0}
          | k == G.Key'Up -> writeIORef ref Step {xx = 0.0, yy = _STEP, zz = 0.0, ww = 0.0}
          | k == G.Key'Down -> writeIORef ref Step {xx = 0.0, yy = (- _STEP), zz = 0.0, ww = 0.0}
          | k == G.Key'9 -> writeIORef ref Step {xx = 0.0, yy = 0.0, zz = _STEP, ww = 0.0}
          | k == G.Key'0 -> writeIORef ref Step {xx = 0.0, yy = 0.0, zz = - _STEP, ww = 0.0}
          | k == G.Key'8 -> writeIORef ref Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = _STEP}
          | k == G.Key'7 -> writeIORef ref Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = - _STEP}
          -- TODO: In orthogonal projective status,
          --   | k == G.Key'F1    -> writeIORef ref Step{xx=0.0,      yy =0.0,      zz = 0.0,    ww = 0.1}
          | k == G.Key'Space -> writeIORef ref initStep
          | otherwise -> pp "Unknown Key Press"
    G.KeyState'Released -> do
      if key == G.Key'Right then pp "Release Key => Right" else pp "Press No Right"
      if key == G.Key'Left then pp "Release Key => left" else pp "Press No Right"
      if key == G.Key'Up then pp "Release Key => up" else pp "Release No Up"
      if key == G.Key'Down then pp "Release Key => Down" else pp "Release No Down"
  when
    (key == G.Key'Escape && keyState == G.KeyState'Pressed)
    (G.setWindowShouldClose window True)

{--
-- |
--    KEY:
--    NOTE: USED
keyBoardCallBack2 :: IORef Step -> IORef GlobalRef -> DAO.IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
keyBoardCallBack2 refStep refGlobalRef ioArray window key scanCode keyState modKeys = do
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
        case key of
          k
            | k == G.Key'Right -> modifyIORef refStep (\s -> s {xx = _STEP})
            --   | k == G.Key'Right -> writeIORef refStep Step {xx = _STEP, yy = 0.0, zz = 0.0, ww = 0.0}
            | k == G.Key'Left -> modifyIORef refStep (\s -> s {xx = - _STEP})
            --   | k == G.Key'Right -> writeIORef refStep Step {xx = _STEP, yy = 0.0, zz = 0.0, ww = 0.0}

            | k == G.Key'Up -> writeIORef refStep Step {xx = 0.0, yy = _STEP, zz = 0.0, ww = 0.0}
            | k == G.Key'Down -> writeIORef refStep Step {xx = 0.0, yy = - _STEP, zz = 0.0, ww = 0.0}
            | k == G.Key'9 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = _STEP, ww = 0.0}
            | k == G.Key'0 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = - _STEP, ww = 0.0}
            | k == G.Key'8 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = _STEP}
            | k == G.Key'7 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = - _STEP}
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
            | k == G.Key'Space -> writeIORef refStep initStep
            | k == G.Key'W -> do
              fw "Rotate Block"
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
--}
  
mouseCallback :: IORef GlobalRef -> G.MouseButtonCallback
mouseCallback globalRef window but butState mk = do
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

flipAxis :: XYZAxis -> XYZAxis -> XYZAxis
flipAxis axisOld axisNew
  | x' = XYZAxis {xa = xor x x', ya = False, za = False}
  | y' = XYZAxis {xa = False, ya = xor y y', za = False}
  | z' = XYZAxis {xa = False, ya = False, za = xor z z'}
  | otherwise = XYZAxis {xa = False, ya = False, za = False}
  where
    x = xa axisOld
    y = ya axisOld
    z = za axisOld
    x' = xa axisNew
    y' = ya axisNew
    z' = za axisNew
    xor :: Bool -> Bool -> Bool
    xor True True = False
    xor True False = True
    xor False True = True
    xor False False = False

xAxis :: XYZAxis
xAxis = XYZAxis {xa = True, ya = False, za = False}

yAxis :: XYZAxis
yAxis = XYZAxis {xa = False, ya = True, za = False}

zAxis :: XYZAxis
zAxis = XYZAxis {xa = False, ya = False, za = True}
  
{--
rotateWorld :: IORef CameraRot -> Double -> Double -> IO ()
rotateWorld refCamRot w h = do
  {--
  modifyIORef refCamRot (\c -> c {alpha_ = alpha_ c + xx (delta_ c)})
  modifyIORef refCamRot (\c -> c {beta_ = beta_ c + yy (delta_ c)})
  modifyIORef refCamRot (\c -> c {gramma_ = gramma_ c + zz (delta_ c)})
  modifyIORef refCamRot (\c -> c {dist_ = ww (delta_ c)})
  --}
  -- rotate ( alpha_  cam) ( Vector3 1 0 0 :: Vector3 GLdouble)  -- rotate x-axix alpha degree/radian
  --                ↑→ degree
  -- rotate ( beta_   cam) ( Vector3 0 1 0 :: Vector3 GLdouble)  -- rotate y-axis beta  degree/radian
  --                ↑→ degree
  -- rotate ( gramma_ cam) ( Vector3 0 0 1 :: Vector3 GLdouble)  -- rotate z-axis gamma degree/radian
  --                ↑→ degree
  -- multiModelviewVec v
  currXYZ <- readIORef refCamRot <&> currXYZ_
  xyzRotVec <- readIORef refCamRot<&> xyzRotVec_
  xyzRotDeg <- readIORef refCamRot <&> xyzRotDeg_
  rotate xyzRotDeg xyzRotVec -- rotate y-axis beta  degree/radian
--}
  
-- |
--    === Camera rotates around x y z axis
--
--    @
--
--    * Press a key, trigger keyBoardCallBack to UPDATE Ref Step
--    * Increase/decrease _STEP (degree), modify the ref Step
--                                                        ↑
--    *                               +-------------------+
--                                    ↓
--    * keyboardRot IORef Cam -> IORef Step -> Double -> Double -> IO()
--    * Update Cam::{alpha, beta, gramma, dist}
--
--    @
--    @
--    data Cam = Cam{alpha::Double, beta::Double, gramma::Double, dist::Double} deriving(Show)
--
--    data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double} deriving(Show)
--
--    IORef Cam  => ref
--    IORef Step => refStep
--    keyboardRot ref refStep (fromIntegral width) (fromIntegral height)
--    @
keyboardRot :: IORef Cam -> IORef Step -> Double -> Double -> IO ()
keyboardRot refCam refStep w h = do
  step <- readIORef refStep -- data Step = {...} refStep is not modified actually.
  modifyIORef refCam (\c -> c {alpha = alpha c + xx step})
  modifyIORef refCam (\c -> c {beta = beta c + yy step})
  modifyIORef refCam (\c -> c {gramma = gramma c + zz step})
  modifyIORef refCam (\c -> c {dist = ww step})
  cam <- readIORef refCam -- data Cam = {...}
  -- modifyIORef refCam (rx (xx step))  -- Update Camera x-axis
  -- modifyIORef refCam (ry (yy step))  -- Update Camera y-axis
  -- modifyIORef refCam (rz (zz step))  -- Update Camera z-axis
  -- modifyIORef refCam (rw (ww step))  -- Not been used for now
  -- Tuesday, 09 November 2021 12:39 PST
  -- TODO: fix the rotation issue
  -- ERROR: after the rotation of x-axis, y-axis and z-axis are not longer standard axes any more
  -- M-x gx
  -- https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-CoordTrans.html#v:rotate
  rotate (alpha cam) (Vector3 1 0 0 :: Vector3 GLdouble) -- rotate x-axix alpha degree/radian
  --                ↑→ degree
  rotate (beta cam) (Vector3 0 1 0 :: Vector3 GLdouble) -- rotate y-axis beta  degree/radian
  --                ↑→ degree
  rotate (gramma cam) (Vector3 0 0 1 :: Vector3 GLdouble) -- rotate z-axis gamma degree/radian
  --                ↑→ degree
  -- rotate ( beta   cam) $ ( Vector3 0 1 0 :: Vector3 GLdouble)  -- rotate y-axis beta  degree/radian

-- y' = y cos alpha
-- z' = z cos gramma
-- lookAt (Vertex3 1.1 + (gamma cam) 0 0) (Vertex3 0 0 0)  (Vector3 0 1 0)
-- get viewport width and height for ratio
-- let ratio = width/height
-- perspective 60.0 ratio 0.5 4.0
-- perspective (field of view) width/height zNear zFar
-- lookAt eye_vertex center_vertex lookat_vector
-- perspective 65.0 (w/h) 1.0 4.0
{--
    where
        rx::Double -> Cam -> Cam
        rx d (Cam x y z w) = Cam{alpha = x + d, beta = y,     gramma = z,     dist = w}

        ry::Double -> Cam -> Cam
        ry d (Cam x y z w) = Cam{alpha = x,     beta = y + d, gramma = z,     dist = w}

        rz::Double -> Cam -> Cam
        rz d (Cam x y z w) = Cam{alpha = x,     beta = y,     gramma = z + d, dist = w}

        rw::Double -> Cam -> Cam
        rw d (Cam x y z w) = Cam{alpha = x,     beta = y,     gramma = z,     dist = w}
--}

-- |
--    === Draw Triangle
--
--
--
--                0.5 ← v₀
--                 |
--                 |
--                 |
--        - 0.25 - + - 0.25
--           ↑           ↑
--           v₁          v₂
drawTriangle :: IO ()
drawTriangle =
  preservingMatrix $ do
    translate (Vector3 0 0 0 :: Vector3 GLdouble)
    renderPrimitive Triangles $ do
      color (Color3 1 0 0 :: Color3 GLdouble)
      vertex (Vertex3 0 0.5 0 :: Vertex3 GLdouble) -- v₀
      color (Color3 0 1 0 :: Color3 GLdouble)
      vertex (Vertex3 (ne 0.25) 0 0 :: Vertex3 GLdouble) -- v₁
      color (Color3 0 0 1 :: Color3 GLdouble)
      vertex (Vertex3 0.25 0 0 :: Vertex3 GLdouble) -- v₂
  where
    ne = negate

-- |
--    === Draw Triangle specified location and size
--                             ↑                ↑
--                             v               0.1
--    >let v = Vector3 0.0 0 0
--    >drawTriangle' v 0.1
drawTriangle' :: Vector3 GLdouble -> GLdouble -> IO ()
drawTriangle' v s =
  preservingMatrix $ do
    translate v
    renderPrimitive Triangles $ do
      color (Color3 1 0 0 :: Color3 GLdouble)
      vertex (Vertex3 (negate s) (negate s) 0 :: Vertex3 GLdouble)
      color (Color3 0 1 0 :: Color3 GLdouble)
      vertex (Vertex3 s (negate s) 0 :: Vertex3 GLdouble)
      color (Color3 0 0 1 :: Color3 GLdouble)
      vertex (Vertex3 0 s 0 :: Vertex3 GLdouble)

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
  b <- action
  unless b falseAction
  
unlessX' :: Monad m => m Bool -> m Bool -> m () -> m ()
unlessX' action1 action2 falseAction = do
  b1 <- action1
  b2 <- action2
  unless (b1 || b2) falseAction
  
maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
  Nothing -> nothingRes
  Just x -> f x

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description

printGLVersion :: G.Window -> IO ()
printGLVersion window = do
  major <- G.getWindowContextVersionMajor window
  minor <- G.getWindowContextVersionMinor window
  rev <- G.getWindowContextVersionRevision window
  putStrLn $ "OpenGL context version = " ++ show major ++ "." ++ show minor ++ "." ++ show rev

-- |
--    === Torus paremater equation center at (0, 0, 0)
--
--    Torus equation: <http://xfido.com/html/indexThebeautyofTorus.html Torus>
--
--    * drawPrimitive Points red torus
torus :: [(GLfloat, GLfloat, GLfloat)]
torus =
  [ ( fx i k,
      fy i k,
      fz i k
    )
    | i <- [1 .. n],
      k <- [1 .. n]
  ]
  where
    del = rf (2 * pi / (n -1))
    n = 100
    r = 0.2
    br = 0.3

    fx = \i k -> (br + r ** cos (del * i)) * cos (del * k)
    fy = \i k -> sin (rf del * i)
    fz = \i k -> (br + r * cos (rf del * i)) * sin (rf del * k)

-- |
--    === Draw torus with small and large radius
--
--    >mapM_ (\lo -> drawPrimitive LineLoop red lo) $ torusR 0.1 0.2
torusR :: GLfloat -> GLfloat -> [[(GLfloat, GLfloat, GLfloat)]]
torusR r br =
  [ [ ( (br + r * cos (del * i)) * cos (del * k),
        sin (rf del * i),
        (br + r * cos (rf del * i)) * sin (rf del * k)
      )
      | i <- [1 .. n]
    ]
    | k <- [1 .. n]
  ]
  where
    del = rf (2 * pi / (n -1))
    n = 100

-- |
--    === sphere rotates around y-axis
--
--    === cut the sphere alone the xz-plane
--
--    * The radius of the circle is \( r \cos \alpha \)
--
--    * The circle on the xz-plane is
--
--    \[
--        \begin{equation}
--        \begin{aligned}
--        x &= r \cos \alpha \sin \beta \\
--        z &= r \cos \alpha \cos \beta \\
--        \end{aligned}
--        \end{equation}
--    \]
--
--    * The center of the circle from the center of sphere is \( r \cos \alpha \sin \beta \)
--
--    * Put all together
--    \[
--        \begin{equation}
--        \begin{aligned}
--        x &= r \cos \alpha \sin \beta \\
--        y &= r \sin \beta \\
--        z &= r \cos \alpha \cos \beta \\
--        \end{aligned}
--        \end{equation}
--    \]
sphere :: GLfloat -> [[(GLfloat, GLfloat, GLfloat)]]
sphere r =
  [ [ ( (r * cos (del * i)) * cos (del * k),
        sin (rf del * i),
        (r * cos (rf del * i)) * sin (rf del * k)
      )
      | i <- [1 .. n]
    ]
    | k <- [1 .. n]
  ]
  where
    del = rf (2 * pi / (n -1))
    n = 40

torus' :: [[(GLfloat, GLfloat, GLfloat)]]
torus' =
  [ [ ( (br + r * cos (del * i)) * cos (del * k),
        sin (rf del * i),
        (br + r * cos (rf del * i)) * sin (rf del * k)
      )
      | i <- [1 .. n]
    ]
    | k <- [1 .. n]
  ]
  where
    del = rf (2 * pi / (n -1))
    n = 100
    r = 0.3
    br = 0.3

parabolic :: [[(GLfloat, GLfloat, GLfloat)]]
parabolic = [[(x' * i, y' * j, 2 * (x' * i) ^ 2 + 3 * (y' * j) ^ 2) | i <- [- n .. n]] | j <- [- n .. n]]
  where
    n = 20
    del = rf (1 / n)
    x' = del
    y' = del

-- x^4 + y^2 + z^6 = 6
surface1 :: [[(GLfloat, GLfloat, GLfloat)]]
surface1 = [[(u * d, v * d, (c - (u * d) ^ 4 - (v * d) ^ 2) ** (1 / 6)) | u <- [- n .. n]] | v <- [- n .. n]]
  where
    n = 160
    d = rf (1 / n)
    c = 0.1

surface2 :: [[(GLfloat, GLfloat, GLfloat)]]
surface2 = [[(u * d, v * d, - (c - (u * d) ^ 4 - (v * d) ^ 2) ** (1 / 6)) | u <- [- n .. n]] | v <- [- n .. n]]
  where
    n = 100
    d = rf (1 / n)
    c = 0.1

surface3 :: [[(GLfloat, GLfloat, GLfloat)]]
surface3 = [[let x' = x * d; y' = y * d in (x', y', 1 - (1 - x') ^ 2 - 100 * (y' - x' ^ 2) ^ 2) | y <- [- n .. n]] | x <- [- n .. n]]
  where
    n = 100
    d = rf (1 / n)
    c = 0.1

lightingInfo :: IO ()
lightingInfo = do
  diffuse (Light 0) $= lightDiffuse
  ambient (Light 0) $= lightAmbient
  specular (Light 0) $= lightSpecular
  position (Light 0) $= lightPosition
  light (Light 0) $= Enabled
  -- lighting           $= Enabled
  depthFunc $= Just Lequal
  blend $= Enabled
  lineSmooth $= Enabled

-- |
--    === Generate random Vertex3 in xy-plan, Vertex3 x y 0
randomVertex :: Integer -> IO [Vertex3 GLfloat]
randomVertex n = do
  ranl <- randomDouble (fromIntegral n)
  let ranlist = map (\x -> 1.5 * x) ranl
  let vexList = fmap (\x -> x - 0.5) $ fmap realToFrac ranlist
  let vexTuple = map (\x -> tripleToVertex3 x) $ filter (\x -> length x == 3) $ partList 3 vexList
        where
          tripleToVertex3 :: [GLfloat] -> Vertex3 GLfloat
          tripleToVertex3 [a, b, c] = Vertex3 a b 0.0
  return vexTuple

{--
 Sun  3 Mar 16:12:57 2024 
 Move to AronGraphic

renderText :: String -> IO ()
renderText str = do
  preservingMatrix $ do
    GL.scale (1 / scaleFont :: GL.GLdouble) (1 / scaleFont) 1
    GLUT.renderString GLUT.Roman str
--}

-- |
--    KEY: save image, save png, opengl save image, save png opengl
--
--    DATE: Thursday, 20 April 2023 12:07 PDT
--    NOTE: The image will be up side down
--    TODO: Fix it
--
--    https://www.reddit.com/r/haskell/comments/dee0iz/converting_opengl_window_to_a_png_image_in_haskell/
--    read pixels, convert them to first a Vector then a JuicyPixel image
--    and save the image as a PNG file
--
--
--    Graphics.Rendering.OpenGL.GL.ReadCopyPixels readPixels :: Position -> Size -> PixelData a -> IO ()
--    readPixels::(Position x y) -> (Size w h) -> PixelData a -> IO ()
--
--    readPixels (Position x y) (Size w h) pd =
--      withPixelData pd $ glReadPixels x y w h
--
--
--    data PixelData a = PixelData PixelFormat DataType (Ptr a)
--
--    https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadPixels.xhtml
--
--    glReadPixels(GLint x       ,
--                 GLint y       ,
--                 GLsizei width ,
--                 GLsizei height,
--                 GLenum  format,
--                 GLenum  type  ,
--                 void * data);
saveImageOpenGL :: G.Window -> FilePath -> IO ()
saveImageOpenGL window fp = do
  -- (Int, Int) <- G.getFramebufferSize window
  (w, h) <- G.getFramebufferSize window
  let w' = fromIntegral w
  let h' = fromIntegral h
  -- currentWindow $= Just window
  -- Size w h <- get windowSize
  let npixels = fromIntegral (w * h) :: Int
      nbytes = 3 * npixels
  fptr <- mallocForeignPtrArray nbytes :: IO (ForeignPtr Word8)
  -- withForeignPtr::ForeignPtr a -> (Ptr a -> IO b) -> IO b
  withForeignPtr fptr $ \ptr -> do
    let pdata = GL.PixelData GL.RGB GL.UnsignedByte ptr :: PixelData Word8
    readPixels (Position 0 0) (Size w' h') pdata
  -- readPixels (Position 0 0) (Size Int32 Int32) pdata
  let fptr' = castForeignPtr fptr :: ForeignPtr (PixelBaseComponent PixelRGB8)
  let vecImage = STO.unsafeFromForeignPtr0 fptr' npixels :: STO.Vector (PixelBaseComponent PixelRGB8)
  print $ "w=" ++ show w
  print $ "h=" ++ show h
  print $ "len=" ++ (show $ STO.length vecImage)
  let pix1 = vecImage STO.! 0
  let image = Image (fromIntegral w) (fromIntegral h) vecImage :: Image PixelRGB8
  writePng fp image

-- |
--
--  @
--    b = 0.3 :: GLfloat
--    p0 = Vertex3 b    b    (-b)
--    p1 = Vertex3 (-b) b    (-b)
--    p2 = Vertex3 (-b) b    b
--    p3 = Vertex3 b    b    b
--
--    let ls_top = [p0, p1, p2, p3]
--
--    let lc = [green, blue, yellow, cyan]
--
--    drawQuadColor lc ls_top
--  @
drawQuadColor :: [Color3 GLdouble] -> [Vertex3 GLfloat] -> IO ()
drawQuadColor lc ls = do
  let lt = zip lc ls
  renderPrimitive Quads $
    mapM_
      ( \(c, v) -> do
          color c
          vertex v
      )
      lt

-- |
--  @
--    b = 0.3 :: GLfloat
--    p0 = Vertex3 b    b    (-b)
--    p1 = Vertex3 (-b) b    (-b)
--    p2 = Vertex3 (-b) b    b
--    p3 = Vertex3 b    b    b
--
--    let ls_top = [p0, p1, p2, p3]
--    drawQuad green ls_top
--  @
drawQuad :: Color3 GLdouble -> [Vertex3 GLfloat] -> IO ()
drawQuad c ls = do
  renderPrimitive Quads $
    mapM_
      ( \v -> do
          color c
          vertex v
      )
      ls

-- |
-- -
--  KEY: draw cube with quad
--  @
--  @
drawCubeQuad :: GLfloat -> IO ()
drawCubeQuad r =
  let nfaces = zip3 n [(green, gray), (cyan, blue), (yellow, green), (gray, blue), (white, yellow), (blue, cyan)] facesx
   in do
        mapM_
          ( \(n, (c1, c2), [v0, v1, v2, v3]) -> do
              renderPrimitive Quads $ do
                normal n
                color c1
                vertex v0
                color c2
                vertex v1
                vertex v2
                vertex v3
          )
          nfaces
  where
    n :: [Normal3 GLfloat]
    n =
      [ Normal3 (-1.0) 0.0 0.0,
        Normal3 0.0 1.0 0.0,
        Normal3 1.0 0.0 0.0,
        Normal3 0.0 (-1.0) 0.0,
        Normal3 0.0 0.0 1.0,
        Normal3 0.0 0.0 (-1.0)
      ]
    faces :: [[Vertex3 GLfloat]]
    faces =
      [ [ v 0, v 1, v 2, v 3],
        [ v 3, v 2, v 6, v 7],
        [ v 7, v 6, v 5, v 4],
        [ v 4, v 5, v 1, v 0],
        [ v 5, v 6, v 2, v 1],
        [ v 7, v 4, v 0, v 3]
      ]

    v :: Int -> Vertex3 GLfloat
    v x = Vertex3 v0 v1 v2
      where
        v0
          | x == 0 || x == 1 || x == 2 || x == 3 = - r
          | x == 4 || x == 5 || x == 6 || x == 7 = r
        v1
          | x == 0 || x == 1 || x == 4 || x == 5 = - r
          | x == 2 || x == 3 || x == 6 || x == 7 = r
        v2
          | x == 0 || x == 3 || x == 4 || x == 7 = r
          | x == 1 || x == 2 || x == 5 || x == 6 = - r

    facesx :: [[Vertex3 GLfloat]]
    facesx =
      [ [v0, v1, v2, v3],
        [v3, v2, v6, v7],
        [v7, v6, v5, v4],
        [v4, v5, v1, v0],
        [v5, v6, v2, v1],
        [v7, v4, v0, v3]
      ]
    v0 = Vertex3 (- r) (- r) r
    v1 = Vertex3 (- r) (- r) (- r)
    v2 = Vertex3 (- r) r (- r)
    v3 = Vertex3 (- r) r r
    v4 = Vertex3 r (- r) r
    v5 = Vertex3 r (- r) (- r)
    v6 = Vertex3 r r (- r)
    v7 = Vertex3 r r r
