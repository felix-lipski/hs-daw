-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Brick
import Control.Lens
import qualified Graphics.Vty as V

type ResName = ()

type EventType = ()

data UI = UI
    { _loop :: Loop
    }

type Loop = [[Int]]

makeLenses ''UI

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
drawUI ui = [drawGrid ui 20 30]

drawGrid :: UI -> Int -> Int -> Widget ()
drawGrid ui span offset = hBox $ map drawColumn $ view loop ui where
    emptyColumn = replicate span (str ".")
    drawColumn :: [Int] -> Widget ()
    drawColumn  = vBox . foldr (\note -> \col -> set (element $ span + offset - note) (str "#") col) emptyColumn

handleEvent :: UI -> BrickEvent ResName EventType -> EventM ResName (Next UI)
handleEvent ui (VtyEvent (V.EvKey V.KEsc        [])) = halt ui


theMap :: AttrMap
theMap = attrMap V.defAttr []


ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = do
    let initialState = UI { _loop = [[40], [], [], [40], [], [40], [42], [43, 45]] }
    finalState <- defaultMain app initialState
    return ()
