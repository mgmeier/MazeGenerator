{-# LANGUAGE RecordWildCards, LambdaCase #-}

import  Types
import  GL

import  Graphics.Rendering.OpenGL       as Gl
import  Graphics.UI.GLUT                as Glut

import  qualified Data.Set              as HS

import  Control.Concurrent

import  System.Random                   (randomRIO)
import  System.Environment              (getArgs)


mazeDims        = (56, 48)                                              -- refers to the amount of empty cells in a maze

screenDims      = (800, 600)                                            -- initial window dimensions

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
-- * buildSnapshot action is executed whenever a maze wall is torn down
-- * randomFunc is a function that has different implementations
--   regarding GenereationBias
depthFirstSearch
    :: ([MazeIx] -> IO ()) 
    -> (MazeIx -> [MazeIx] -> IO MazeIx) 
    -> (MazeIx -> [MazeIx])
    -> [MazeIx] 
    -> Int 
    -> IO [MazeIx]
depthFirstSearch buildSnapshot randomFunc neighboursAround maze_ = 
    depthFirstSearch' maze_ HS.empty (0, 0) []                          -- (0, 0) with empty stack results in a random starting point

  where
    emptyCells = HS.fromList maze_

    depthFirstSearch' maze _ _ _ 0 = return maze                        -- no remaining unvisited cells? done!
    depthFirstSearch' maze visit current@(cx, cy) stack rem =
        let
            unvisitedNeighbs =
                [ix | ix <- neighboursAround current, not (HS.member ix visit)] 
            
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



generateMaze :: MVar AppState -> IO ()
generateMaze appState = do
    AppState {..} <- readMVar appState
    buildMV     <- newMVar []
    buildDoneMV <- newEmptyMVar
    let
        neighboursAround (x, y) =                                           -- the neighboring empty maze cells
            filter clipping [(x-2, y), (x+2, y), (x, y-2), (x, y+2)]
          where
            clipping (i, j) =
                i > 0 && j > 0 && i < 2 * fst asDims && j < 2 * snd asDims
        
        buildSnapshot False _  = return ()
        buildSnapshot True  xs = swapMVar buildMV xs >> threadDelay 2000 
        
        renderLoop = do
            when asShowBuild (readMVar buildMV >>= showMaze asQuadWH Nothing)
            threadDelay 18000
            tryTakeMVar buildDoneMV >>= maybe renderLoop return
        
        buildThread =
            depthFirstSearch
                (buildSnapshot asShowBuild)
                (randomBias asBuildBias asDims)
                neighboursAround
                (emptyMaze asDims) 
                (uncurry (*) asDims)
            >>= putMVar buildDoneMV         

    modifyMVar_ appState $
        \st -> return st {asNeedBuild = False, asRunning = True, asSolution = Nothing}
    forkIO buildThread
    maze <- renderLoop
    modifyMVar_ appState $
        \st -> return st {asMaze = maze, asRunning = False}
    glutDisplayCallback appState


solveMaze :: MVar AppState -> IO ()
solveMaze appState = do
    AppState {..} <- readMVar appState
    solveMV     <- newMVar []
    solveDoneMV <- newEmptyMVar
    let
        maze    = exit:asMaze
        enter   = (0, 1)
        exit    = let (right, upper) = maximum asMaze in (right+1, upper)      
        
        doesExit     = dropWhile ((/= exit) . head)
        moves (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

        renderLoop = do
            readMVar solveMV >>= flip (showMaze asQuadWH) maze . Just
            threadDelay 18000
            tryTakeMVar solveDoneMV >>= maybe renderLoop return  
     
        solveRecursive free sols 
            | HS.null free || null sols = putMVar solveDoneMV Nothing
            | otherwise = do
                let 
                    sols' = concat [ map (:sol) ms
                        | sol@(s:_) <- sols
                        , let ms = filter (flip HS.member free) $ moves s
                        , (not . null) ms
                        ]
                    free' = foldr HS.delete free (map head sols')
                    reds  = (HS.toList . HS.fromList . concat) sols'
                
                swapMVar solveMV reds
                threadDelay 7600
                case doesExit sols' of
                    x:_ -> putMVar solveDoneMV (Just x)
                    _   -> solveRecursive free' sols'
    
    modifyMVar_ appState $
        \st -> return st {asNeedSolve = False, asRunning = True, asSolution = Nothing}    
    forkIO (solveRecursive (HS.fromList maze) [[enter]])
    sol <- renderLoop
    modifyMVar_ appState $
        \st -> return st {asSolution = sol, asRunning = False}
    glutDisplayCallback appState
  
    

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




-- triggerAction periodically checks if a maze has to
-- be generated or solved and runs the appropriate action
triggerAction :: MVar AppState -> Glut.TimerCallback
triggerAction appState = do
    readMVar appState >>= \case
        AppState {asNeedBuild = True, asRunning = False} ->
            generateMaze appState
        AppState {asNeedSolve = True, asRunning = False} ->
            solveMaze appState
        _ -> return ()
    
    Glut.addTimerCallback 50 (triggerAction appState)



--
-- MAIN
--

main = do
    putStrLn "Maze Generator (c) by M. G. Meier 2014-15\n"

    initializeGL screenDims "MazeGenerator"
    Gl.color $ Color3 0.8 0.8 (0.8 :: GLfloat)
    
    appState <- newMVar (initialAppState screenDims mazeDims)
    
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
            Glut.addTimerCallback 50    (generateMaze appState >> triggerAction appState)            
            startMainLoop

  where
    showKeyBindings = putStrLn
        "Key Bindings:\n\
        \  (Space)              - create new maze\n\
        \  (Enter)              - solve maze\n\
        \  (Arrow left/right)   - adjust maze width\n\
        \  (Arrow up/down)      - adjust maze height\n\
        \  +, -                 - change maze pattern via generation bias\n\
        \  F1                   - toggle step-by-step animated maze creation\n\
        \  Esc                  - quit"
