{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Prelude hiding (sequence)
import Control.Monad.IO.Class
import Data.Text (pack, unpack)
import Data.Aeson
import Data.Aeson.Types(Parser)
import Data.Proxy
import Data.Vector hiding ((++))
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant
import Servant.API
import Servant.Client
import Network.Wai
import Network.Wai.Handler.Warp

data Register = Register
  { registerPlayerName :: String
  , registerUrl :: String }
  deriving Show
instance ToJSON Register where
  toJSON (Register name url) =
    object [ "playerName"  .= name
           , "url"         .= url
           ]

data RegisterResponse = RegisterResponse
  { registerResponseId :: String
  , registerResponsePlayer :: Player
  , registerResponseGameState :: GameState
  } deriving Show
instance FromJSON RegisterResponse where
  parseJSON (Object v) =
    RegisterResponse <$> v .: "id"
                     <*> v .: "player"
                     <*> v .: "gameState"
  parseJSON _ = fail "parsing registerResponse Failed"

data GameStateChanged = GameStateChanged
  { changedGameState :: GameState
  , changedPlayerState :: Player
  }
instance FromJSON GameStateChanged where
  parseJSON (Object v) =
    GameStateChanged <$> v .: "gameState"
                     <*> v .: "playerState"
  parseJSON _ = fail "Invalid game state change"

data PlayerState = Move | Use | Pick
  deriving Show
instance FromJSON PlayerState where
  parseJSON (String "MOVE") = return Move
  parseJSON (String "USE") = return Use
  parseJSON (String "PICK") = return Pick
  parseJSON _ = fail "Parsing PLayerState Failed"


data Player = Player
  { playerName :: String
  , playerUrl :: String
  , playerPosition :: Position
  , playerScore :: Int
  , playerMoney :: Int
  , playerState :: PlayerState
  , playerTimeInState :: Int
  , playerUsableItems :: [Item]
  , playerActionCount :: Int
  , playerHealth :: Int
  } deriving Show
instance FromJSON Player where
 parseJSON (Object v) =
    Player <$> v .: "name"
           <*> v .: "url"
           <*> v .: "position"
           <*> v .: "score"
           <*> v .: "money"
           <*> v .: "state"
           <*> v .: "timeInState"
           <*> v .: "usableItems"
           <*> v .: "actionCount"
           <*> v .: "health"
 parseJSON _ = fail "parsing Player Failed"

data Position = Position
  { x :: Int
  , y :: Int
  } deriving (Generic, Show)
instance FromJSON Position

data ShootingLine = ShootingLine { from :: Position
                                 , to :: Position
                                 } deriving (Generic, Show)
instance FromJSON ShootingLine

data GameState = GameState
  { stateMap :: Map
  , statePlayers :: [Player]
  , stateFinishedPlayers :: [Player]
  , stateItems :: [Item]
  , stateRound :: Int
  , stateShootingLines :: [ShootingLine]
  } deriving Show
instance FromJSON GameState where
  parseJSON (Object v) =
    GameState <$> v .: "map"
              <*> v .: "players"
              <*> v .: "finishedPlayers"
              <*> v .: "items"
              <*> v .: "round"
              <*> v .: "shootingLines"
  parseJSON _ = fail "Parsing gameState failed"

data Tile = Empty | Wall | Exit deriving (Eq, Show)

newtype Tiles = Tiles (Vector (Vector Tile)) deriving Show

data TurnAction = ActionUp | ActionDown | ActionLeft | ActionRight | ActionPick | ActionUse
instance Show TurnAction where
  show ActionUp = "UP"
  show ActionDown = "DOWN"
  show ActionLeft = "LEFT"
  show ActionRight = "RIGHT"
  show ActionPick = "PICK"
  show ActionUse = "USE"
instance ToJSON TurnAction where
  toJSON =  toJSON . pack . show

parseRows :: Value -> Parser (Vector Tile)
parseRows (String s) = fmap fromList . traverse charToTile $ unpack s
    where charToTile :: Char -> Parser Tile
          charToTile '_' = return Empty
          charToTile 'x' = return Wall
          charToTile 'o' = return Exit
          charToTile _ = fail "could not understand a char in the tiles "
parseRows _ = fail "parsing TileRow failed"

instance FromJSON Tiles where
  parseJSON (Array a) = parseTiles a
  parseJSON _ = fail "not an array"
parseTiles :: Array -> Parser Tiles
parseTiles a = (fmap Tiles) $ sequence $ parseRows <$> a

data Map = Map
  { mapWidth :: Int
  , mapHeight :: Int
  , mapMaxItemCount :: Int
  , mapTiles :: Tiles
  , mapName :: String
  , mapExit :: Position
  } deriving Show
instance FromJSON Map where
  parseJSON o@(Object v) =
    Map <$> v .: "width"
        <*> v .: "height"
        <*> v .: "maxItemCount"
        <*> v .: "tiles"
        <*> v .: "name"
        <*> v .: "exit"
  parseJSON _ = fail "Parsing Map Failed"

data ItemType = JustSomeJunk | Weapon deriving Show
instance FromJSON ItemType where
  parseJSON (String "JUST_SOME_JUNK") = return JustSomeJunk
  parseJSON (String "WEAPON") = return Weapon
  parseJSON _ = fail "Parsing ItemType failed"

data Item = Item
  { itemPrice :: Int
  , itemDiscountPercent :: Int
  , itemPosition :: Position
  , itemType :: ItemType
  , itemIsUsable :: Bool
  } deriving Show
instance FromJSON Item where
  parseJSON (Object v) =
    Item <$> v .: "price"
         <*> v .: "discountPercent"
         <*> v .: "position"
         <*> v .: "type"
         <*> v .: "isUsable"
  parseJSON _ = fail "Parsing Item failed"

type ReceiveAPI = "bot" :> PostNoContent '[JSON] NoContent
             :<|> "move" :> ReqBody '[JSON] GameStateChanged :> Post '[JSON] TurnAction
type TransmitAPI = "register" :> ReqBody '[JSON] Register :> Post '[JSON] RegisterResponse
receiveApi :: Proxy ReceiveAPI
receiveApi = Proxy
transmitApi :: Proxy TransmitAPI
transmitApi = Proxy

postRegister :: Register -> ClientM RegisterResponse
postRegister = client transmitApi

handleRegister :: ClientEnv -> IO ()
handleRegister clientEnv = do
  res <- runClientM (postRegister $ Register "testi" "http://localhost:8081/move") clientEnv
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right s -> putStrLn $ show s

server :: ClientEnv -> Server ReceiveAPI
server clientEnv = handleBot :<|> handleMove
  where handleBot :: Handler NoContent
        handleBot = do
          liftIO $ handleRegister clientEnv
          return NoContent
        handleMove :: GameStateChanged -> Handler TurnAction
        handleMove _ = return ActionRight


main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Http "localhost" 8080 "")
  let app = serve receiveApi $ server clientEnv
  run 8081 app
