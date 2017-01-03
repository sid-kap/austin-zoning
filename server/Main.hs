{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Data.Aeson.QQ (aesonQQ)

import qualified Web.Scotty as Scotty
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

main :: IO ()
main = do
  Scotty.scotty 3000 $ do
    Scotty.get "/index.html" $ Scotty.file "../client/index.html"
    Scotty.get "/bundle.js"  $ Scotty.file "../client/bundle.js"
    Scotty.post "/area" $ do
      mapExtent :: MapExtent <- Scotty.jsonData
      Scotty.json =<< liftIO (doStuff mapExtent)

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

wkid :: WKID
wkid = 4326 -- lat/long

doStuff :: MapExtent -> IO Aeson.Value
doStuff mapExtent = do
  let geometry = mkGeometry mapExtent

      -- 5 is for zoning overlay
      opts = req mapExtent geometry wkid "all:5" "esriGeometryPolygon"

  r2 <- Wreq.getWith opts identify

  resp2 <- liftMaybe $ r2 ^? Wreq.responseBody
  liftMaybe $ Aeson.decode resp2

mkGeometry :: MapExtent -> Aeson.Value
mkGeometry MapExtent{..} =
  [aesonQQ|
    { spatialReference: { wkid: #{wkid}, latestWkid: #{wkid} }
    , rings: [
        [ [#{_xmin}, #{_ymin}]
        , [#{_xmin}, #{_ymax}]
        , [#{_xmax}, #{_ymax}]
        , [#{_xmax}, #{_ymin}]
        ]
      ]
    }
  |]
