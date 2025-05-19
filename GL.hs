{-# LANGUAGE RecordWildCards #-}

module  GL where

import           Types

import           Graphics.Rendering.OpenGL as Gl
import           Graphics.UI.GLUT          as Glut

import           Control.Exception

import           Data.Char                 (ord)



-- produces 1 frame, rendering the maze with given quad sizes.
-- a list of red cells indicating the maze's solution is optional.
-- beware: always draws the complete maze, no optimisation
showMaze :: (GLfloat, GLfloat) -> Maybe [MazeIx] -> [MazeIx] -> IO ()
showMaze (w, h) redCells maze = do
    Gl.clear [ColorBuffer]
    Gl.unsafeRenderPrimitive Quads $
        mapM_ drawQuad maze
    forM_ redCells $ \rs -> do
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
        key'        = bool key (SpecialKey $ KeyUnknown 0) asRunning    -- disregard keyboard input during animation

        newMazeDims mazeDims' =
             modifyMVar_ appState $ \st -> return st
                { asDims        = mazeDims'
                , asQuadWH      = getQuadWH asScreenWH mazeDims'
                , asNeedBuild   = True
                , asShowBuild   = False
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
            | ord c == 13   ->
                 modifyMVar_ appState $ \st -> return st {asNeedSolve = True}

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
    AppState{..} <- readMVar appState
    unless asRunning $ showMaze asQuadWH asSolution asMaze


-- start/terminateMainLoop work around missing function
-- 'Glut.leaveMainLoop' when not using freeGLUT
terminateMainLoop = do
    Gl.get Glut.currentWindow >>= mapM_ Glut.destroyWindow
    throwIO UserInterrupt


startMainLoop =
    handle (\UserInterrupt -> return ()) Glut.mainLoop


-- set up OpenGL for a 2D scene
glSetup2D (w, h) = do
    Gl.viewport             $= (Position 0 0, Size w h)
    Gl.matrixMode           $= Projection
    Gl.loadIdentity
    Gl.ortho                0.0 (fromIntegral w) 0.0 (fromIntegral h) 0.0 1.0
    Gl.matrixMode           $= Modelview 0
    Gl.loadIdentity


-- OpenGL initialization
initializeGL :: (GLint, GLint) -> String -> IO ()
initializeGL screenDims windowName = do
    Glut.getArgsAndInitialize
    Glut.initialDisplayMode $= [DoubleBuffered, RGBMode]
    Glut.initialWindowSize  $= uncurry Size screenDims
    Glut.createWindow       windowName
    glSetup2D               screenDims
    Gl.clearColor           $= Color4 0.1 0.1 0.1 (1.0 :: GLfloat)
