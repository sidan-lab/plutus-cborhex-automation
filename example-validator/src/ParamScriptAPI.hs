{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ParamScriptAPI where

import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson       hiding (json)
import           Data.Text        (pack)
import           GHC.Generics
import           Utils            (writeTestingValidatorJSON)

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

data ParamInput = ParamInput {
  piNumber :: !Integer,
  piAddr   :: !String
} deriving (Show, Generic)

instance FromJSON ParamInput
instance ToJSON ParamInput

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  post "testingscript" $ do
    scriptParam <- jsonBody' :: ApiAction ParamInput
    json $ pack $ show $ writeTestingValidatorJSON (piNumber scriptParam) (piAddr scriptParam)
