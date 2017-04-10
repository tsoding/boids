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

-- TODO(cb053b98-8a4c-4f53-b5c2-6fc8e5b78999): take cursor position
-- into account during zooming
zoomControl :: Event -> Navigation -> Navigation
zoomControl (EventKey (MouseButton WheelUp) Down _ _) navigation =
    navigation { navigationViewPort = zoom zoomSpeed $ navigationViewPort navigation }
zoomControl (EventKey (MouseButton WheelDown) Down _ _) navigation =
    navigation { navigationViewPort = zoom (-zoomSpeed) $ navigationViewPort navigation }
zoomControl _ navigation = navigation

-- TODO(38bb2eed-3ee9-4ff8-b892-46645e85229c): take zoom factor into
-- account while dragging the view
dragControl :: Event -> Navigation -> Navigation
dragControl (EventKey (MouseButton LeftButton) Down _ position) navigation =
    navigation { navigationDragPosition = Just position }
dragControl (EventMotion position) navigation = navigation { navigationViewPort = fromMaybe viewPort draggedViewPort
                                                 , navigationDragPosition = position <$ navigationDragPosition navigation
                                                 }
    where draggedViewPort = do prevPosition <- navigationDragPosition navigation
                               let dragVector = fromPoints prevPosition position
                               return $ viewPort { viewPortTranslate = addTwoVectors (viewPortTranslate viewPort) $ dragVector }
          viewPort = navigationViewPort navigation
dragControl (EventKey (MouseButton LeftButton) Up _ _) navigation =
    navigation { navigationDragPosition = Nothing }
dragControl _ world = world

navigationInput :: Event -> Navigation -> Navigation
navigationInput event navigation = foldl' (\n c -> c event n) navigation controllers
    where controllers = [zoomControl, dragControl]
