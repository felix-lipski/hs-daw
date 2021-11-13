{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Control.Lens
import Control.Monad
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent


import BMT.Audio.Playback

type ResName = ()

type EventType = ()

data UI = UI
    { _loop :: Loop
    , _debugMsg :: String
    }

data Loop = Loop 
    { _cols :: [[Int]]
    , _colSelect :: Int
    , _rowSelect :: Int
    }

makeLenses ''UI
makeLenses ''Loop

data VisualBlock
  = NormalBlock
  | HardDropBlock String

app :: App UI EventType ResName
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

drawUI :: UI -> [Widget ()]
drawUI ui = [ vBox 
                [ center $ border $ drawGrid ui 20 30
                , str $ show $ view (loop . cols) ui
                , str $ show $ view (loop . rowSelect) ui
                , str $ view (debugMsg) ui
                ]
            ]

drawGrid :: UI -> Int -> Int -> Widget ()
drawGrid ui span offset = hBox $ (over (element $ selectedColumn) $ withAttr columnAttr) $ map drawColumn $ view (loop . cols) ui where
    selectedColumn = view (loop . colSelect) ui
    selectedRow = view (loop . rowSelect) ui
    emptyColumn = replicate span (str "-:-")
    drawColumn :: [Int] -> Widget ()
    drawColumn  = vBox . (over (element $ offset - selectedRow + span) $ withAttr columnAttr) . foldr (\note -> \col -> set (element $ span + offset - note) (withAttr noteBlock $ str "< >") col) emptyColumn

handleEvent :: UI -> BrickEvent ResName EventType -> EventM ResName (Next UI)
handleEvent ui (VtyEvent (V.EvKey V.KEsc        [])) = halt ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ over (loop . colSelect) (1 +) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ over (loop . colSelect) (-1 +) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ over (loop . rowSelect) (1 +) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ over (loop . rowSelect) (-1 +) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ switchCell ui
handleEvent ui x                                     = continue $ set debugMsg (show x) ui

switchCell :: UI -> UI
switchCell ui = over (loop . cols . element x) transform ui where
    transform = if found then filter (y /=) else ((y) :)
    x = view (loop . colSelect) ui
    y = view (loop . rowSelect) ui
    found = (0 /=) $ length $ filter (y ==) $ view (loop . cols . element x) ui

columnAttr, noteBlock :: AttrName
columnAttr = attrName "columnAttr"
noteBlock = attrName "noteBlock"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (columnAttr, V.black `on` V.white)
  , (noteBlock, V.black `on` V.green)
  ]

uiThread :: IO ()
uiThread = do
    let initialState = UI { 
        _loop = Loop { _cols = [[40], [], [], [40], [], [40], [42], [43, 45]], 
        _colSelect = 2, _rowSelect = 40 }, _debugMsg = ""}
    finalState <- defaultMain app initialState
    return ()

uiProducer :: Producer Float IO r
uiProducer = forever $ do
    lift $ uiThread
    yield 0.03

player :: Consumer Float IO ()
player = do
    lift $ aloop [0.05 * sin x | x <- [0.0, 0.02 .. 800.0]]
    n <- await
    lift $ aloop [0.05 * sin x | x <- [0.0, n .. 800.0]]
    player

main :: IO ()
main = do
    (output, input) <- spawn Unbounded
    let ac1 = do runEffect $ uiProducer >-> toOutput output
                 performGC
    let ac2 = do runEffect $ fromInput input >-> player
                 performGC
    res <- concurrently ac1 ac2
    print res

