{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import ZendoParse (tersmu)
import ZendoEval (satisfiesRule)
import Koan
import Servant
import Data.Aeson.Types
import Network.Wai.Handler.Warp (run)
import GHC.Generics

type API = "query" :> ReqBody '[JSON] QueryInfo :> Post '[JSON] (Maybe Bool)
instance FromJSON Colour
instance FromJSON Size
instance FromJSON Pyramid
instance FromJSON Direction
instance FromJSON KoanPart
instance ToJSON Colour
instance ToJSON Size
instance ToJSON Pyramid
instance ToJSON Direction
instance ToJSON KoanPart

data QueryInfo = QueryInfo
  { koan :: Koan
  , rule :: String
  } deriving Generic
instance FromJSON QueryInfo

mykoan = [ Stack [Pyramid Large Red, Pyramid Small Blue]
         , Empty
         , Pointing Koan.Left (Pyramid Medium Green)
         ]

server :: Server API
server = query
  where query :: QueryInfo -> Handler (Maybe Bool)
        query (QueryInfo {koan = koan, rule = s}) = return $ do
          rule <- tersmu s
          satisfiesRule rule koan

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn (show (toEncoding mykoan));
  run 8081 app
