module Navigation.Internals where
import Data.List
import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import ViewPortTransform
import Vector

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

-- TODO(cb053b98-8a4c-4f53-b5c2-6fc8e5b78999): take cursor position
-- into account during zooming
zoomControl :: Event -> Navigation -> Navigation
zoomControl (EventKey (MouseButton WheelUp) Down _ _) navigation =
    navigation { navigationViewPort = zoom zoomSpeed $ navigationViewPort navigation }
zoomControl (EventKey (MouseButton WheelDown) Down _ _) navigation =
    navigation { navigationViewPort = zoom (-zoomSpeed) $ navigationViewPort navigation }
zoomControl _ navigation = navigation

dragControl :: Event -> Navigation -> Navigation
dragControl (EventKey (MouseButton LeftButton) Down _ position) navigation =
    navigation { navigationDragPosition = Just position }
dragControl (EventMotion position) navigation =
    navigation { navigationViewPort = fromMaybe viewPort draggedViewPort
               , navigationDragPosition = position <$ navigationDragPosition navigation
               }
    where draggedViewPort = do prevPosition <- navigationDragPosition navigation
                               let (dragX, dragY) = fromPoints prevPosition position
                               return $ viewPort { viewPortTranslate = ( transX + dragX * zoomFactor
                                                                       , transY + dragY * zoomFactor
                                                                       )
                                                 }
          viewPort = navigationViewPort navigation
          zoomFactor = 1.0 / viewPortScale viewPort
          (transX, transY) = viewPortTranslate viewPort
dragControl (EventKey (MouseButton LeftButton) Up _ _) navigation =
    navigation { navigationDragPosition = Nothing }
dragControl _ world = world

navigationInput :: Event -> Navigation -> Navigation
navigationInput event navigation = foldl' (\n c -> c event n) navigation controllers
    where controllers = [zoomControl, dragControl]
