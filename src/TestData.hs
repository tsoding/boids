module TestData ( getAllBoids
                , readXmlTestData
                , getBoidById) where

import Boids
import Control.Monad
import Data.List
import Data.Maybe
import Text.XML.Light.Types
import Text.XML.Light.Input
import Text.XML.Light.Cursor
import Text.XML.Light.Proc
import Text.Read

svgCircleQName :: QName
svgCircleQName = QName { qName = "circle"
                       , qURI = Just "http://www.w3.org/2000/svg"
                       , qPrefix = Nothing
                       }

idAttrQName = QName { qName = "id"
                    , qURI = Nothing
                    , qPrefix = Nothing
                    }

readXmlTestData :: FilePath -> IO (Maybe Element)
readXmlTestData = fmap parseXMLDoc . readFile

getAllCircles :: Maybe Element -> [Element]
getAllCircles Nothing = []
getAllCircles (Just root) = findElements svgCircleQName root

getAttrValue :: String -> [Attr] -> Maybe String
getAttrValue name attrs =
    find (\a -> (qName $ attrKey a) == name) attrs >>= (Just . attrVal)

readAttrMaybe :: Read a => String -> [Attr] -> Maybe a
readAttrMaybe name attrs = getAttrValue name attrs >>= readMaybe

circleToBoid :: Element -> Maybe Boid
circleToBoid element
    | (qName $ elName element) == "circle" =
        do cx <- readAttrMaybe "cx" attrs
           cy <- readAttrMaybe "cy" attrs
           return $ Boid { boidPosition = (cx, cy)
                         , boidHeading = 0.0
                         , boidSteer = 0.0
                         }
    | otherwise = Nothing
    where attrs = elAttribs element

isCircle :: Element -> Bool
isCircle element = elName element == svgCircleQName

hasId :: Element -> String -> Bool
hasId element id =
    fromMaybe False $ lookupAttr idAttrQName (elAttribs element) >>= \a -> return (a == id)

isCircleWithId :: String -> Element -> Bool
isCircleWithId id element = isCircle element && hasId element id

getAllBoids :: Maybe Element -> [Boid]
getAllBoids = catMaybes . map circleToBoid . getAllCircles

getBoidById :: Maybe Element -> String -> Maybe Boid
getBoidById Nothing _ = Nothing
getBoidById (Just root) id = filterElement (isCircleWithId id) root >>= circleToBoid
