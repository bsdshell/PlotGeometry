
#if (1==0)
  
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
  
#endif