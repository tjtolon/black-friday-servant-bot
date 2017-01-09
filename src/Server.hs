{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified Data.Aeson.Parser
import Lucid


data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
  , User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
  ]

type UserAPI1 = "users" :> Get '[JSON] [User]

server1 :: Server UserAPI1
server1 = return users1

userAPI1 :: Proxy UserAPI1
userAPI1 = Proxy

--  'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver
app1 :: Application
app1 = serve userAPI1 server1

main1 :: IO  ()
main1 = run 8081 app1

type UserAPI2 = "users" :> Get '[JSON] [User]
                :<|> "albert" :> Get '[JSON] User
                :<|> "isaac" :> Get '[JSON] User

isaac, albert :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server2 :: Server UserAPI2
server2 = return users2
  :<|> return albert
  :<|> return isaac

userAPI2 :: Proxy UserAPI2
userAPI2 = Proxy

app2 :: Application
app2 = serve userAPI2 server2

main2 :: IO ()
main2 = run 8081 app2

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
  :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
  :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from' = "great@company.com"
        to'   = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body' = "Hi " ++ clientName c ++ ",\n\n"
          ++ "Since you've recently turned " ++ show (clientAge c)
          ++ ", have you checked out our latest "
          ++ intercalate ", " (clientInterestedIn c)
          ++ " products? Give us a visit!"

server3 :: Server API
server3 = position
  :<|> hello
  :<|> marketing

  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing clientinfo = return (emailForClient clientinfo)
userAPI3 :: Proxy API
userAPI3 = Proxy

app3 :: Application
app3 = serve userAPI3 server3

main3 :: IO ()
main3 = run 8081 app3

data HTMLLucid

instance Accept HTMLLucid where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
  mimeRender _ = renderBS . toHtml

instance MimeRender HTMLLucid (Html a) where
  mimeRender _ = renderBS

type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]

data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Generic

instance ToJSON Person

instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)

  toHtmlRaw = toHtml

instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    foldMap toHtml persons

  toHtmlRaw = toHtml

people :: [Person]
people =
  [ Person "Isaac" "Newton"
  , Person "Albert" "Einstein"
  ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server4 :: Server PersonAPI
server4 = return people

app4 :: Application
app4 = serve personAPI server4

main4 :: IO ()
main4 = run 8081 app4

type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = do
  filecontent <- liftIO (readFile "myfile.txt")
  return (FileContent filecontent)

ioAPI1 :: Proxy IOAPI1
ioAPI1 = Proxy

main5 :: IO ()
main5 = run 8081 (serve ioAPI1 server5)

server6 :: Server IOAPI1
server6 = do
  exists <- liftIO (doesFileExist "myfile.txt")
  if exists
    then liftIO (readFile "myfile.txt") >>= return . FileContent
    else throwError custom404Err

  where custom404Err = err404 { errBody = "myfile.txt just isn't here, pleaes leave this server alone." }

main6 :: IO ()
main6 = run 8081 (serve ioAPI1 server6)


-- API for values of type 'a'
-- indexed by values of type 'i'
-- type APIFor a i =
--     Get '[JSON] [a] -- list 'a's
--   :<|> ReqBody '[JSON] a :> PostNoContent '[JSON] NoContent -- add an 'a'
--   :<|> Capture "id" i :>
--     ( Get '[JSON] a -- view an 'a' given its "identifier" of type 'i'
--     :<|> ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent -- update an 'a'
--     :<|> DeleteNoContent '[JSON] NoContent -- delete a user
--     )

-- usersServer = getUsers :<|> newUser <|> userOperations

--   where getUsers :: Handler [User]
--         getUsers = error "..."

--         newUser  :: User -> Handler NoContent
--         newUser = error "..."

--         userOperations userid =
--           viewUser userid :<|> updateUser userid :<|> deleteUser userid

--           where
--             viewUser :: Int -> Handler User
--             viewUser = error "..."
--             updateUser :: Int -> User -> Handler NoContent
--             updateUser = error "..."
--             deleteUser :: Int -> Handler NoContent
--             deleteUser = error "..."

main :: IO ()
main = main3
