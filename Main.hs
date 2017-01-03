{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import BasicPrelude
import Data.Function ((&))
import Control.Lens ((.~), (^.), (^?), (^..), to)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import           Data.Aeson ((.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Network.Wreq as Wreq
import Data.String.Conversions (convertString)
import Control.Monad.Catch (MonadThrow, throwM)
import GHC.Generics (Generic)

import qualified Web.Scotty as Scotty
-- import Network.Wai.Middleware.Static (static)
import qualified Data.Vector as V

(->>) :: a -> b -> (a,b)
(->>) = (,)

data MapExtent = MapExtent
  { _xmin :: !Double
  , _xmax :: !Double
  , _ymin :: !Double
  , _ymax :: !Double
  } deriving (Show, Generic)

instance Aeson.FromJSON MapExtent where

type WKID = Int

req1 :: MapExtent -> WKID -> Wreq.Options
req1 mapExtent@(MapExtent{..}) wkid = req mapExtent geometryValue wkid "all" "esriGeometryPoint"
-- req1 mapExtent@(MapExtent{..}) wkid = req mapExtent geometryValue wkid "all:2,3" "esriGeometryPoint"
  where
    geometryValue = Aeson.object
      [ "x" .= tshow ((_xmin + _xmax) / 2.0)
      , "y" .= tshow ((_ymin + _ymax) / 2.0)
      , "spatialReference" .= Aeson.object
        [ "wkid" .= wkid
        , "latestWkid" .= wkid
        ]
      ]

req :: MapExtent -> Aeson.Value -> WKID -> Text -> Text -> Wreq.Options
req MapExtent{..} geometryValue wkid layers geometryType =
  Wreq.defaults
  & Wreq.params .~
  [ "f" ->> "json"
  , "geometry" ->> (convertString . Aeson.encode $ geometryValue)
  , "tolerance" ->> tshow 2
  , "returnGeometry" ->> "true"
  , "mapExtent" ->> (convertString . Aeson.encode $ Aeson.object
    [ "xmin" .= _xmin
    , "xmax" .= _xmax
    , "ymin" .= _ymin
    , "ymax" .= _ymax
    , "spatialReference" .= Aeson.object [ "wkid" .= wkid ]
    ])
  , "imageDisplay" ->> "400,400,96"
  , "geometryType" ->> geometryType
  , "sr" ->> tshow wkid
  , "layers" ->> layers
  ]

data MaybeException = MaybeException deriving (Show)
instance Exception MaybeException

liftMaybe :: MonadThrow m => Maybe a -> m a
liftMaybe (Just a) = return a
liftMaybe Nothing  = throwM MaybeException

identify :: String
identify = "http://www.austintexas.gov/GIS/REST/ZoningProfile/ZoningProfile/MapServer/identify"

prettyPrint :: Aeson.Value -> Text
prettyPrint = convertString . encodePretty

main :: IO ()
main = do
  Scotty.scotty 3000 $ do
    Scotty.get "/index.html" $ Scotty.file "typescript-austin-zoning/index.html"
    Scotty.get "/bundle.js"  $ Scotty.file "typescript-austin-zoning/bundle.js"
    Scotty.post "/area" $ do
      mapExtent :: MapExtent <- Scotty.jsonData
      Scotty.json =<< liftIO (doStuff mapExtent)

-- mapExtentDef = MapExtent (-10880963.8504) (-10880810.976) 3540104.5279 3540257.402

wkid = 4326 -- lat/long

doStuff :: MapExtent -> IO Aeson.Value
doStuff mapExtent = do

  -- r <- Wreq.getWith (req1 mapExtent wkid) identify

  let geometry = mkGeometry mapExtent
  print geometry

  -- geometry <- liftMaybe $ r ^? Wreq.responseBody
  --                           . Aeson.key "results"
  --                           . Aeson.nth 0
  --                           . Aeson.key "geometry"

  let req_ = (req mapExtent geometry wkid "all:5" "esriGeometryPolygon")
  putStrLn "req_ is "
  print req_
  r2 <- Wreq.getWith req_ identify

  resp2 <- liftMaybe $ r2 ^? Wreq.responseBody
  json2 :: Aeson.Value <- liftMaybe $ Aeson.decode resp2

  return json2

mkGeometry :: MapExtent -> Aeson.Value
mkGeometry MapExtent{..} =
  Aeson.object
  [ "spatialReference" .= Aeson.object [ "wkid" .= wkid, "latestWkid" .= wkid ]
  , "rings" .= V.fromList [
      V.fromList
      [ V.fromList [ _xmin, _ymin ]
      , V.fromList [ _xmin, _ymax ]
      , V.fromList [ _xmax, _ymax ]
      , V.fromList [ _xmax, _ymin ]
      ]
    ]
  ]

string :: Text -> Text
string = id

int :: Int -> Int
int = id

  -- putStrLn (prettyPrint geometry)
