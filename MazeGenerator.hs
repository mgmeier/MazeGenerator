
{-# LANGUAGE RecordWildCards, LambdaCase #-}

import  Graphics.Rendering.OpenGL       as Gl
import  Graphics.UI.GLUT                as Glut

import  qualified Data.HashMap.Strict   as HM
import  qualified Data.HashSet          as HS

import  Data.Char                       (ord)
import  Data.Bool                       (bool)

import  Control.Concurrent.MVar
import  Control.Concurrent
import  Control.Monad
import  Control.Exception

import  System.Random                   (randomRIO)
import  System.Environment              (getArgs)


mazeDims        = (56, 48)                                              -- refers to the amount of empty cells in a maze
screenDims      = (800, 600)                                            -- initial window dimensions


type MazeIx     = (Int, Int)
type Maze       = ([MazeIx], HM.HashMap MazeIx [MazeIx])                -- lists all maze fields that are walkable (empty cells, teardowns)
                                                                        -- for each cell, memo a list of neighbouring empty cells (for performance)



-- influence the likelihood of a neighbouring cell being chosen
-- during maze generation, resulting in different visual patterns
data GenerationBias =
    NoBias | VertBias | HorizBias | CheckerBoard  | DiagonalSplit
    deriving Enum


data AppState = AppState
    { asMaze        :: Maze                                             -- the current maze
    , asDims        :: (Int, Int)                                       -- maze dimensions
    , asQuadWH      :: (GLfloat, GLfloat)                               -- quad dimensions on screen
    , asScreenWH    :: (GLint, GLint)                                   -- current screen resolution
    , asNeedBuild   :: Bool                                             -- need to build a new maze?
    , asShowBuild   :: Bool                                             -- step-by-step animation of the generation process? (slow)
    , asBuildBias   :: GenerationBias                                   -- bias will influence the pattern of the resulting maze
    , asBuilding    :: Bool                                             -- building in progress
    , asEmptyMaze   :: Maze                                             -- an empty maze, memo'd for performance issues
    , asSolution    :: Maybe [MazeIx]
    }


{-
  Source:
    http://en.wikipedia.org/wiki/Maze_generation#Recursive_backtracker
    
    The depth-first search algorithm of maze generation is frequently
    implemented using backtracking:

    Make the initial cell the current cell and mark it as visited
    While there are unvisited cells
        If the current cell has any unvisited neighbours                -- see (Note: 1)
            Choose randomly one of the unvisited neighbours
            Push the current cell to the stack
            Remove the wall between the current cell and the chosen cell
            Make the chosen cell the current cell and mark it as visited
        Else if stack is not empty                                      -- see (Note: 2)
            Pop a cell from the stack
            Make it the current cell
        Else                                                            -- see (Note: 3)
            Pick a random unvisited cell, make it the current cell and
            mark it as visited          
-}


-- tail-recursive implementation of Depth-first search algorithm
-- * drawNewFrame action is executed whenever a maze wall is torn down
-- * randomFunc is a function that has different implementations
--   regarding GenereationBias
depthFirstSearch :: ([MazeIx] -> IO ()) -> (MazeIx -> [MazeIx] -> IO MazeIx) -> Maze -> Int -> IO Maze
depthFirstSearch buildSnapshot randomFunc (maze_, neighbMemos) = 
    depthFirstSearch' maze_ HS.empty (0, 0) []                          -- (0, 0) with empty stack results in a random starting point

  where
    emptyCells = HS.fromList maze_

    depthFirstSearch' maze _ _ _ 0 =                                    -- no remaining unvisited cells? done!
        return (maze, neighbMemos)

    depthFirstSearch' maze visit current@(cx, cy) stack rem =
        let
            unvisitedNeighbs =
                [ix | ix <- HM.lookupDefault [] current neighbMemos
                    , not (HS.member ix visit)] 
            (visit', rem')                                              -- adjust remaining unvisited cell count and mark current cell as visited if necessary
                | HS.member current visit   = (visit, rem)
                | otherwise                 = (HS.insert current visit, rem-1)
      
        in if null unvisitedNeighbs
            then case stack of
                [] -> do
                    next <- randomElement $
                        HS.toList (HS.difference emptyCells visit)      -- all unvisited cells is the set difference between empty and visited cells
                    depthFirstSearch' maze visit' next stack rem'       -- Note: 3                            
                c:cs ->
                    depthFirstSearch' maze visit' c cs rem'             -- Note: 2

            else do
                next@(nx, ny) <- randomFunc current unvisitedNeighbs
                let
                    tearDown    = ((cx+nx) `div` 2, (cy+ny) `div` 2)
                    maze'       = tearDown:maze
                buildSnapshot maze'                                           -- possibly a visual update of the generation process
                depthFirstSearch' maze' visit' next (current:stack) rem'     -- Note: 1



emptyMaze :: (Int, Int) -> Maze
emptyMaze (mazeW, mazeH) = 
    (emptyCells, HM.fromList neighboursList)

  where 
    neighboursAround (x, y) =                                           -- the neighboring empty maze cells to be memo'd
        filter clipping [(x-2, y), (x+2, y), (x, y-2), (x, y+2)]
      where
        clipping (i, j) =
            i > 0 && j > 0 && i < mazeW*2 && j < mazeH*2

    emptyCells =
        [(mazeProject x, mazeProject y) 
            | x <- [0 .. mazeW-1], y <- [0 .. mazeH-1]]

    neighboursList =
        [(ix, neighboursAround ix) | ix <- emptyCells]
            

generateMaze appState = do
    AppState {..} <- readMVar appState
    buildMV     <- newMVar []
    buildDoneMV <- newEmptyMVar
    let
        renderLoop = do
            when asShowBuild (readMVar buildMV >>= showMaze asQuadWH Nothing)
            threadDelay 20000
            tryTakeMVar buildDoneMV >>= maybe renderLoop return
        
        buildSnapshot False _  = return ()
        buildSnapshot True  xs = swapMVar buildMV xs >> threadDelay 1000
        buildThread =
            depthFirstSearch
                (buildSnapshot asShowBuild)
                (randomBias asBuildBias asDims)
                asEmptyMaze 
                (uncurry (*) asDims)
            >>= putMVar buildDoneMV         

    modifyMVar_ appState $
        \st -> return st {asNeedBuild = False, asBuilding = True, asSolution = Nothing}
    forkIO buildThread
    maze <- renderLoop
    modifyMVar_ appState $
        \st -> return st {asMaze = maze, asBuilding = False}
    glutDisplayCallback appState


solveMaze :: MVar AppState -> IO ()
solveMaze appState = do
    AppState {..} <- readMVar appState
    
    recurse
  where
    recurse = return ()


-- projection function from a maze coordinate (empty cell) to its
-- corresponding index in the Maze data structure
mazeProject :: Int -> Int
mazeProject = (+1) . (*2)


randomElement :: [a] -> IO a
randomElement []    = fail "randomElement: empty list"
randomElement [x]   = return x
randomElement xs    = (xs !!) `fmap` randomRIO (0, length xs - 1)


-- implementations for GenerationBias; the basic
-- functionality is shifting the odds between randomly
-- choosing the horizontal or vertical neighbour of a maze cell
randomBias :: GenerationBias -> (Int, Int) -> MazeIx -> [MazeIx] -> IO MazeIx
randomBias NoBias _ _ xs =
    randomElement xs

randomBias CheckerBoard dims@(mazeW, mazeH) ix@(x, y) xs =
    let
        sqX     = 2*x `div` mazeW
        sqY     = 2*y `div` mazeH
        bias    = bool HorizBias VertBias $
            (odd sqY && even sqX) || (odd sqX && even sqY)
    in randomBias bias dims ix xs

randomBias DiagonalSplit dims@(mazeW, mazeH) ix@(x, y) xs =
    let bias    = bool VertBias HorizBias $ x > (y * mazeW) `div` mazeH
    in randomBias bias dims ix xs    

randomBias bias _ (x, y) xs =
    let biased = filter filterFunc xs
    in randomElement $ concat $ xs : replicate 3 biased

  where 
    filterFunc = case bias of
        VertBias    -> (== x) . fst
        _           -> (== y) . snd                                     -- other biases than HorizBias matched beforehand




--
-- OpenGL stuff and GLUT callbacks
--

-- calculate quad size in pixels of a maze cell, 
-- depending on screen size and maze dimensions
getQuadWH :: (GLint, GLint) -> (Int, Int) -> (GLfloat, GLfloat)
getQuadWH (w, h) (mazeW, mazeH) =
    (fromIntegral w / fromIntegral (mazeProject mazeW)
    , fromIntegral h / fromIntegral (mazeProject mazeH))


-- produces 1 frame, rendering the maze with given quad sizes
showMaze :: (GLfloat, GLfloat) -> Maybe [MazeIx] -> [MazeIx] -> IO ()
showMaze (w, h) redCells maze = do
    Gl.clear [ColorBuffer, DepthBuffer]
    Gl.unsafeRenderPrimitive Quads $
        mapM_ drawQuad maze
    flip (maybe (return ())) redCells $ \ rs -> do
        Gl.color        $  Color3 0.6 0.1 (0.1 :: GLfloat)
        Gl.unsafeRenderPrimitive Quads $
            mapM_ drawQuad rs
        Gl.color        $  Color3 0.8 0.8 (0.8 :: GLfloat)

    Glut.swapBuffers

  where
    glVertex2f x y = Gl.vertex $ Vertex2 x y
  
    -- draws a quad (counter-clockwise)
    drawQuad (blX, blY) = do                             
        let
            x = fromIntegral blX * w
            y = fromIntegral blY * h
        glVertex2f x y
        glVertex2f (x+w) y
        glVertex2f (x+w) (y+h)
        glVertex2f x (y+h)


glutInputCallback :: MVar AppState -> Glut.KeyboardMouseCallback
glutInputCallback appState key Down _ _ = do
    AppState {..} <- readMVar appState
    let
        (w, h)      = asDims
        key'        = bool key (SpecialKey $ KeyUnknown 0) asBuilding   -- disregard keyboard input while building

        newMazeDims mazeDims' =
             modifyMVar_ appState $ \st -> return st
                { asDims        = mazeDims'
                , asQuadWH      = getQuadWH asScreenWH mazeDims'
                , asNeedBuild   = True
                , asShowBuild   = False
                , asEmptyMaze   = emptyMaze mazeDims'
                }

        cycleBias withFunc =
            try (evaluate $ withFunc asBuildBias) >>= either
                (\(SomeException _) -> return ())
                (\bias' ->  modifyMVar_ appState $ \st -> return st
                    { asNeedBuild   = True
                    , asShowBuild   = False
                    , asBuildBias   = bias'
                    })

    case key' of
        Char c
            | ord c == 27   -> terminateMainLoop
            | c == '+'      -> cycleBias succ
            | c == '-'      -> cycleBias pred
            | c == ' '      ->  
                 modifyMVar_ appState $ \st -> return st {asNeedBuild = True}

        SpecialKey sk
            | sk == KeyLeft     && w > 8    -> newMazeDims (w-1, h)
            | sk == KeyRight    && w < 256  -> newMazeDims (w+1, h)
            | sk == KeyUp       && h < 256  -> newMazeDims (w, h+1)
            | sk == KeyDown     && h > 8    -> newMazeDims (w, h-1)
            | sk == KeyF1 ->
                 modifyMVar_ appState $
                    \st -> return st {asShowBuild = not asShowBuild}

        _   -> return ()

glutInputCallback _ _ _ _ _ = 
    return ()


glutReshapeCallback :: MVar AppState -> Glut.ReshapeCallback
glutReshapeCallback appState (Size w h) = do
    glSetup2D (w, h)
    dims <- asDims `fmap` readMVar appState
    modifyMVar_ appState $ \st -> return st
        { asQuadWH      = getQuadWH (w, h) dims
        , asScreenWH    = (w, h)
        }
    Glut.postRedisplay Nothing


glutDisplayCallback :: MVar AppState -> Glut.DisplayCallback
glutDisplayCallback appState = do
    AppState {..} <- readMVar appState
    unless asBuilding $ showMaze asQuadWH asSolution (fst asMaze)


-- handleRebuild periodically checks if a new maze has to
-- be generated, triggering it when necessary
handleRebuild :: MVar AppState -> Glut.TimerCallback
handleRebuild appState = do
    build <- asNeedBuild `fmap` readMVar appState
    when build (generateMaze appState)
    Glut.addTimerCallback 50 (handleRebuild appState)


-- set up openGL for a 2D scene
glSetup2D (w, h) = do
    Gl.viewport     $= (Position 0 0, Size w h)
    Gl.matrixMode   $= Projection
    Gl.loadIdentity
    Gl.ortho 0.0 (fromIntegral w) 0.0 (fromIntegral h) 0.0 1.0
    Gl.matrixMode   $= Modelview 0
    Gl.loadIdentity


-- start/terminateMainLoop work around missing function
-- 'Glut.leaveMainLoop' when not using freeGLUT
terminateMainLoop =
    get Glut.currentWindow
    >>= maybe (return ()) Glut.destroyWindow
    >> throwIO UserInterrupt

startMainLoop =
    handle (\UserInterrupt -> return ()) Glut.mainLoop

--
-- MAIN
--

main = do
    putStrLn "Maze Generator (c) by M. G. Meier 2014\n"

    Glut.getArgsAndInitialize
    Glut.initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
    Glut.initialWindowSize  $= uncurry Size screenDims
    Glut.createWindow       "Maze Generator"

    glSetup2D       screenDims
    Gl.clearColor   $= Color4 0.1 0.1 0.1 (1.0 :: GLfloat)
    Gl.color        $  Color3 0.8 0.8 (0.8 :: GLfloat)
             
    appState <- newMVar AppState
        { asMaze        = emptyMaze mazeDims
        , asDims        = mazeDims
        , asQuadWH      = getQuadWH screenDims mazeDims
        , asScreenWH    = screenDims
        , asNeedBuild   = False
        , asShowBuild   = False
        , asBuildBias   = NoBias
        , asBuilding    = False
        , asEmptyMaze   = emptyMaze mazeDims
        , asSolution    = Nothing
        }
    
    getArgs >>= \case
        ["--speedtest"] ->
            let howMany = 300 
            in do
                putStrLn ("generating " ++ show howMany ++ " mazes")
                Glut.displayCallback        $= glutDisplayCallback appState
                Glut.addTimerCallback 20    (replicateM_ howMany (generateMaze appState) >> terminateMainLoop)
                startMainLoop
                
        _ -> do
            showKeyBindings
            Glut.displayCallback        $= glutDisplayCallback appState
            Glut.reshapeCallback        $= Just (glutReshapeCallback appState)
            Glut.keyboardMouseCallback  $= Just (glutInputCallback appState)
            Glut.addTimerCallback 50    (generateMaze appState >> handleRebuild appState)            
            startMainLoop

  where
    showKeyBindings = putStrLn
        "Key Bindings:\n\
        \  (Space)              - create new maze\n\
        \  (Arrow left/right)   - adjust maze width\n\
        \  (Arrow up/down)      - adjust maze height\n\
        \  +, -                 - change maze pattern via generation bias\n\
        \  F1                   - toggle step-by-step animated maze creation\n\
        \  Esc                  - quit"
