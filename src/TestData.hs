module TestData where

import Boids
import Control.Monad
import Data.List
import Text.XML.Light.Types
import Text.XML.Light.Input
import Text.XML.Light.Cursor
import Text.Read

readXmlTestData :: FilePath -> IO [Content]
readXmlTestData = undefined

getAllCircles :: Maybe Cursor -> [Element]
getAllCircles Nothing = []
getAllCircles _ = undefined

getAttrValue :: String -> [Attr] -> Maybe String
getAttrValue name attrs =
    find (\a -> (qName $ attrKey a) == name) attrs >>= (Just . attrVal)

readAttrMaybe :: Read a => String -> [Attr] -> Maybe a
readAttrMaybe name attrs = getAttrValue name attrs >>= readMaybe

circleToBoid :: Element -> Maybe Boid
circleToBoid element
    | (qName $ elName element) == "circle" = position >>= \(x, y) ->
        return $ Boid { boidPosition = (x, y)
                      , boidHeading = 0.0
                      , boidSteer = 0.0
                      }
    | otherwise = Nothing
    where cx = readAttrMaybe "cx" attrs
          cy = readAttrMaybe "cy" attrs
          position = liftM2 (,) cx cy
          attrs = elAttribs element

onlyDefined :: [Maybe a] -> [a]
onlyDefined [] = []
onlyDefined (Nothing:xs) = onlyDefined xs
onlyDefined (Just x:xs) = x:onlyDefined xs

testBoids :: IO [Boid]
testBoids = do cursor <- fmap fromForest $ readXmlTestData "./boids-test-data.svg"
               return $ onlyDefined $ map circleToBoid $ getAllCircles cursor
