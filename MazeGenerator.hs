{-# LANGUAGE RecordWildCards #-}

import           GL
import           RandomGeneration
import           Types

import           Graphics.Rendering.OpenGL as Gl
import           Graphics.UI.GLUT          as Glut

import qualified Data.Set                  as S

import           Control.Concurrent

import           System.Environment        (getArgs)


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
-- * buildSnapshot must announce a visual update of the generation process
-- * randomFunc must make a random pick which is GenerationBias-aware
depthFirstSearch
    :: ([MazeIx] -> IO ())
    -> (MazeIx -> [MazeIx] -> IO MazeIx)
    -> (MazeIx -> [MazeIx])
    -> [MazeIx]
    -> Int
    -> IO [MazeIx]
depthFirstSearch buildSnapshot randomFunc neighboursAround maze_ =
    depthFirstSearch' maze_ S.empty (0, 0) []                           -- (0, 0) with empty stack results in a random starting point

  where
    emptyCells = S.fromList maze_

    depthFirstSearch' maze _ _ _ 0 = return maze                        -- no remaining unvisited cells? done!
    depthFirstSearch' maze visit current@(cx, cy) stack rem =
        let
            unvisitedNeighbs =
                [ix | ix <- neighboursAround current, not (S.member ix visit)]

            (visit', rem')                                              -- adjust remaining unvisited cell count and mark current cell as visited if necessary
                | S.member current visit   = (visit, rem)
                | otherwise                 = (S.insert current visit, rem-1)

        in if null unvisitedNeighbs
            then case stack of
                [] -> do
                    next <- randomElement $
                        S.toList (S.difference emptyCells visit)        -- all unvisited cells is the set difference between empty and visited cells
                    depthFirstSearch' maze visit' next stack rem'       -- Note: 3
                c:cs ->
                    depthFirstSearch' maze visit' c cs rem'             -- Note: 2

            else do
                next@(nx, ny) <- randomFunc current unvisitedNeighbs
                let
                    tearDown    = ((cx+nx) `div` 2, (cy+ny) `div` 2)
                    maze'       = tearDown:maze
                buildSnapshot maze'                                          -- things have changed; give an update to whom it may concern
                depthFirstSearch' maze' visit' next (current:stack) rem'     -- Note: 1


-- (possibly animated) generation of a new maze conforming to the
-- parameters held in the application state.
generateMaze :: MVar AppState -> IO ()
generateMaze appState = do
    AppState {..}   <- readMVar appState
    buildMV         <- newMVar []
    buildDoneMV     <- newEmptyMVar
    let
        neighboursAround (x, y) =                                       -- the neighboring empty maze cells
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


-- animates the algorithm that solves the current maze on display.
solveMaze :: MVar AppState -> IO ()
solveMaze appState = do
    AppState {..}   <- readMVar appState
    solveMV         <- newMVar []
    solveDoneMV     <- newEmptyMVar
    let
        maze    = exit:asMaze
        enter   = (0, 1)
        exit    = let (right, upper) = maximum asMaze in (right+1, upper)

        doesExit     = dropWhile ((/= exit) . head)                     -- look for the first path to hit the exit
        moves (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

        renderLoop = do
            readMVar solveMV >>= flip (showMaze asQuadWH) maze . Just
            threadDelay 18000
            tryTakeMVar solveDoneMV >>= maybe renderLoop return

        -- the solver basically keeps track of all paths it is walking
        -- simultaneously as a list of paths (sols). it tries to extend
        -- these paths with maze indices found in the Set (free). paths
        -- which can't be extended further are removed from the list.
        -- NB. this algorithm can't solve mazes with circular paths.
        solveRecursive free sols
            | S.null free || null sols = putMVar solveDoneMV Nothing    -- conditions on which a maze is unsolvable
            | otherwise = do
                let
                    sols' = concat [ map (:sol) ms
                        | sol@(s:_) <- sols
                        , let ms = filter (flip S.member free) $ moves s
                        , (not . null) ms
                        ]
                    free' = foldr S.delete free (map head sols')        -- remove recent path extensions from the Set
                    reds  = (S.toList . S.fromList . concat) sols'

                swapMVar solveMV reds
                threadDelay 7600

                case doesExit sols' of
                    x:_ -> putMVar solveDoneMV (Just x)
                    _   -> solveRecursive free' sols'

    modifyMVar_ appState $
        \st -> return st {asNeedSolve = False, asRunning = True, asSolution = Nothing}
    forkIO (solveRecursive (S.fromList maze) [[enter]])
    sol <- renderLoop
    modifyMVar_ appState $
        \st -> return st {asSolution = sol, asRunning = False}
    glutDisplayCallback appState


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
        \  (Enter)              - solve maze (animated)\n\
        \  (Arrow left/right)   - adjust maze width\n\
        \  (Arrow up/down)      - adjust maze height\n\
        \  +, -                 - change maze pattern via generation bias\n\
        \  F1                   - toggle step-by-step animated maze creation\n\
        \  Esc                  - quit"
