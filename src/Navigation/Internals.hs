module Navigation.Internals where
import Data.List
import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import ViewPortTransform
import Vector
import Debug.Trace

data Navigation = Navigation { navigationViewPort :: ViewPort
                             , navigationDragPosition :: Maybe Point
                             }

zoomSpeed = 0.05

navigationInit :: Navigation
navigationInit = Navigation { navigationViewPort = viewPortInit
                            , navigationDragPosition = Nothing
                            }

applyNavigationToPicture :: Navigation -> Picture -> Picture
applyNavigationToPicture navigation picture = applyViewPortToPicture viewPort picture
    where viewPort = navigationViewPort navigation

applyNavigationToPoint :: Navigation -> Point -> Point
applyNavigationToPoint navigation p = invertViewPort viewPort p
    where viewPort = navigationViewPort navigation

zoomControl :: Event -> Navigation -> Navigation
zoomControl (EventKey (MouseButton WheelUp) Down _ mouseCursor) navigation =
    navigation { navigationViewPort = zoomWithPivot zoomSpeed mouseCursor $ navigationViewPort navigation }
zoomControl (EventKey (MouseButton WheelDown) Down _ mouseCursor) navigation =
    navigation { navigationViewPort = zoomWithPivot (-zoomSpeed) mouseCursor $ navigationViewPort navigation }
zoomControl _ navigation = navigation

dragControl :: Event -> Navigation -> Navigation
dragControl (EventKey (MouseButton LeftButton) Down _ position) navigation =
    navigation { navigationDragPosition = Just position }
dragControl (EventMotion position) navigation =
    navigation { navigationViewPort = fromMaybe viewPort draggedViewPort
               , navigationDragPosition = position <$ navigationDragPosition navigation
               }
    where draggedViewPort = do prevPosition <- navigationDragPosition navigation
                               let dragVector = fromPoints prevPosition position
                               return $ translateViewPort (mulSV zoomFactor dragVector) $ viewPort
          viewPort = navigationViewPort navigation
          zoomFactor = 1.0 / viewPortScale viewPort
          (transX, transY) = viewPortTranslate viewPort
dragControl (EventKey (MouseButton LeftButton) Up _ _) navigation =
    navigation { navigationDragPosition = Nothing }
dragControl _ world = world

navigationInput :: Event -> Navigation -> Navigation
navigationInput event navigation = foldl' (\n c -> c event n) navigation controllers
    where controllers = [zoomControl, dragControl]
