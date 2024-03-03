keyBoardCallBackNew :: IORef CameraRot -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
keyBoardCallBackNew refCamRot refGlobalRef ioArray window key scanCode keyState modKeys = do
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
                         modifyIORef refCamRot (\s -> let s' = s{alpha_  = alpha_ s  + _STEP} in s'{xyzRotDeg_ = alpha_ s'})
                     | v == 2 -> do
                         modifyIORef refCamRot (\s -> let s' = s{beta_ = beta_ s + _STEP} in s'{xyzRotDeg_ = beta_ s'})
                     | v == 3 -> do
                         modifyIORef refCamRot (\s -> let s' = s{gramma_ = gramma_ s + _STEP} in s'{xyzRotDeg_ = gramma_ s'})
                     | otherwise -> error "Invalid currXYZ_ value, Key Right"
  
            | k == G.Key'Left -> do
                currXYZ <- readIORef refCamRot <&> currXYZ_
                case currXYZ of
                   v | v == 1 -> do
                         modifyIORef refCamRot (\s -> let s' = s{alpha_  = alpha_ s  - _STEP} in s'{xyzRotDeg_ = alpha_ s'})
                     | v == 2 -> do
                         modifyIORef refCamRot (\s -> let s' = s{beta_  = beta_ s  - _STEP} in s'{xyzRotDeg_ = beta_ s'})
                     | v == 3 -> do
                         modifyIORef refCamRot (\s -> let s' = s{gramma_  = gramma_ s  - _STEP} in s'{xyzRotDeg_ = gramma_ s'})
                     | otherwise -> error "Invalid currXYZ_ value, Key Left"
  
            --  | k == G.Key'Left -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = - _STEP, yy = 0, zz = 0}})
            | k == G.Key'Up -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = _STEP, zz = 0}})
            | k == G.Key'Down -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = - _STEP, zz = 0}})
            | k == G.Key'9 -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = 0, zz = _STEP}})
            | k == G.Key'0 -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = 0, zz = - _STEP}})
            | k == G.Key'8 -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = 0, zz = 0, ww = _STEP}})
            | k == G.Key'7 -> modifyIORef refCamRot (\s -> let a = delta_ s in s {delta_ = a {xx = 0, yy = 0, zz = 0, ww = - _STEP}})
            --  | k == G.Key'Left -> modifyIORef refStep (\s -> s{xx = - _STEP, yy = 0, zz = 0})
            --  | k == G.Key'Up   -> modifyIORef refStep (\s -> s{xx = 0, yy = _STEP, zz = 0})
            --  | k == G.Key'Down -> modifyIORef refStep (\s -> s{xx = 0, yy = - _STEP, zz = 0})

            --  | k == G.Key'9 -> modifyIORef refStep (\s -> s{xx = 0, yy = 0, zz = _STEP})
            --  | k == G.Key'0 -> modifyIORef refStep (\s -> s{xx = 0, yy = 0, zz = - _STEP})

            --  | k == G.Key'8 -> modifyIORef refStep (\s -> s{ww = _STEP})
            --  | k == G.Key'7 -> modifyIORef refStep (\s -> s{ww = - _STEP})

            --  | k == G.Key'X -> modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) xAxis})
            | k == G.Key'X -> do
                modifyIORef refCamRot (\s -> let a = modelview_ s in s{modelview_ = a{eye_ = Vertex3 0 1.0 0}})
                -- modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) xAxis})
            --                                  ↑
            --                                  + -> Update Coord to YZ-plane

            --  | k == G.Key'Y -> modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) yAxis})
            | k == G.Key'A -> do
                vec <- readIORef refCamRot <&> vecRotX_
                modifyIORef refCamRot (\s -> s{xyzRotVec_ = vec, currXYZ_ = 1})
            | k == G.Key'B -> do
                vec <- readIORef refCamRot <&> vecRotY_
                modifyIORef refCamRot (\s -> s{xyzRotVec_ = vec, currXYZ_ = 2})
            | k == G.Key'C -> do
                vec <- readIORef refCamRot <&> vecRotZ_
                modifyIORef refCamRot (\s -> s{xyzRotVec_ = vec, currXYZ_ = 3})
  
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
            -- zoom in
            | k == G.Key'I -> modifyIORef refCamRot (\s -> s {fovDeg_ = fovDeg_ s - 5.0})
            --  | k == G.Key'Space -> writeIORef refStep initStep
            {--
            | k == G.Key'R -> do
                              bmap <- readIORef refGlobalRef >>= return . boardMap_
                              block1 <- readIORef refGlobalRef >>= \x -> return $ map fst $ block1_ x
                              modifyIORef refGlobalRef (\s -> s{moveX_ =  let mx = moveX_ s
                                                                              my = moveY_ s
                                                                              m = map (\(a, b) -> (a + mx + 1, b + my)) block1
                                                                              b = checkMove m bmap rr
                                                                          in  b ? mx + 1 $ mx
                                                               }
                                                       )
                              print block1
            --}

            | k == G.Key'W -> do
              nowTime <- timeNowMilli
              modifyIORef refGlobalRef (\s -> s {count1_ = 0})
              modifyIORef refGlobalRef (\s -> s {time1_ = nowTime})
              modifyIORef refGlobalRef (\s -> s {rot_ = True})
              fw "Rotate Block"
              pp "rotate me"
            | k == G.Key'P -> do
              modifyIORef refGlobalRef (\s -> s {isPaused_ = not $ isPaused_ s})
              isPaused <- readIORef refGlobalRef <&> isPaused_
              pauseTetris isPaused refGlobalRef initRectGrid ioArray
              pp "Pause"
            | k == G.Key'A -> do
              moveX <- readIORef refGlobalRef <&> moveX_
              moveY <- readIORef refGlobalRef <&> moveY_
              bmap <- readIORef refGlobalRef <&> boardMap_
              bmapX <- readIORef refGlobalRef <&> boardMap1_
              centerBrick <- readIORef refGlobalRef <&> centerBrick_
              rotN <- readIORef refGlobalRef <&> rotN_
              bk1 <- readIORef refGlobalRef <&> bk1_
              tet <- readIORef refGlobalRef <&> tetris1_
              tetX <- readIORef refGlobalRef <&> tetris1X_
              array <- getAssocs ioArray
              -- let bk1' = rotateN 1 bk1
              let bk1' = rotateN 1 (snd tetX)
              -- let bk1'X = rotateN 0 (tet ^._4)
              let bk1'X = rotateN 0 (snd tetX)

              -- /Users/aaa/myfile/bitbucket/tmp/xx_5248.x
              let currBr = innerBrick (moveX, moveY) centerBrick bk1'
              let currBrX = innerBrick (moveX, moveY) centerBrick bk1'X
              let currBrXX = map (\(x, y) -> (x, y, 0)) currBrX
              let -- b = checkMove currBr bmap rr
              -- let bX = checkMoveX currBrX bmapX rr
              let bX = checkMoveArr currBrXX array rr
              -- modifyIORef refGlobalRef (\s -> s{bk1_ = b ? bk1' $ bk1})
              -- modifyIORef refGlobalRef (\s -> s {count1_ = b ? 0 $ count1_ s})
              modifyIORef refGlobalRef (\s -> s {count1_ = bX ? 0 $ count1_ s})
              -- modifyIORef refGlobalRef (\s -> s{rotN_ = bX ? (let n = (rotN_ s) + 1 in mod n 4 )$ rotN_ s})
              pp "rotateN 1"
            | k == G.Key'L || k == G.Key'R -> do
              bmap <- readIORef refGlobalRef <&> boardMap_
              bmapX <- readIORef refGlobalRef <&> boardMap1_
              centerBrick <- readIORef refGlobalRef <&> centerBrick_
              rotN <- readIORef refGlobalRef <&> rotN_
              -- bk1 <- readIORef refGlobalRef >>= return . bk1_
              tet <- readIORef refGlobalRef <&> tetris1_
              tetX <- readIORef refGlobalRef <&> tetris1X_
              let bk1' = rotateN rotN bk1
              -- let bk1'X = rotateN rotN (tet ^._4)
              let bk1'X = rotateN rotN (snd tetX)
              lsArr <- getAssocs ioArray
              -- let mk = (join . join) $ (map . map) fst $ (map . filter) (\(_, n) -> n == 1) $ (map . zip) centerBrick bk1
              -- let lz = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'
              -- let ls = map fst lz
              -- let ls = getShape centerBrick bk1'
              let lsX = getShape centerBrick bk1'X
              logFileG ["lsX"]
              logFileG $ map show lsX
              logFileG ["lsArr"]
              logFileG $ map show lsArr
              -- block1 <- readIORef refGlobalRef >>= \x -> return $ map fst $ block1_ x
              modifyIORef
                refGlobalRef
                ( \s ->
                    s
                      { moveX_ =
                          let mx = moveX_ s
                              my = moveY_ s
                              one = k == G.Key'L ? -1 $ 1
                              mX = map (\(a, b) -> (a + mx + one, b + my, 0)) lsX
                              b = checkMoveArr mX lsArr rr
                           in b ? mx + one $ mx
                      }
                )

              print "kk"
            | k == G.Key'U -> do
              bmap <- readIORef refGlobalRef <&> boardMap_
              bmapX <- readIORef refGlobalRef <&> boardMap1_
              centerBrick <- readIORef refGlobalRef <&> centerBrick_
              rotN <- readIORef refGlobalRef <&> rotN_
              bk1 <- readIORef refGlobalRef <&> bk1_
              tet <- readIORef refGlobalRef <&> tetris1_
              let bk1' = rotateN rotN bk1
              let bk1'X = rotateN rotN (tet ^. _4)

              -- let mk = (join . join) $ (map . map) fst $ (map . filter) (\(_, n) -> n == 1) $ (map . zip) centerBrick bk1
              let lz = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'
              let ls = map fst lz
              let lzX = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'X
              let lsX = map fst lzX

              print "kk"
            | k == G.Key'D -> do
              mx <- readIORef refGlobalRef <&> moveX_
              my <- readIORef refGlobalRef <&> moveY_
              centerBrick <- readIORef refGlobalRef <&> centerBrick_
              tetX <- readIORef refGlobalRef <&> tetris1X_
              rotN <- readIORef refGlobalRef <&> rotN_
              bk1 <- readIORef refGlobalRef <&> bk1_
              lsArr <- getAssocs ioArray
              -- let bk1' = rotateN rotN bk1

              -- let lz = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'
              -- let ls = map fst lz
              -- let ls = getShape centerBrick bk1'
              -- let ls = getShape centerBrick $ rotateN rotN (tet ^._4)
              let ls = getShape centerBrick $ rotateN rotN (snd tetX)

              let mX = map (\(a, b) -> (a + mx, b + my - 1, 0)) ls

              let b = checkMoveArr mX lsArr rr

              when (not b) $ do
                let ls3 = map (\(a, b) -> ((a + mx, b + my, 0), fst tetX)) ls
                mapM_ (uncurry $ DAO.writeArray ioArray) ls3

              newBlock <- randomBlockX refGlobalRef
              modifyIORef
                refGlobalRef
                ( \s ->
                    s
                      { moveX_ = b ? moveX_ s $ 0,
                        moveY_ = b ? my - 1 $ 0,
                        count1_ = b ? count1_ s $ 10000000,
                        rotN_ = b ? rotN_ s $ 0,
                        tetris1X_ = b ? tetris1X_ s $ newBlock
                      }
                )

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
