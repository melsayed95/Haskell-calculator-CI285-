{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

import           Data.Text (Text, unpack)

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/   HomeR GET
/add/#Int/#Int AddR GET
/sub/#Int/#Int SubR GET
/mul/#Int/#Int MulR GET
/div/#Int/#Int DivR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ defaultLayout $ do [shamlet|welcome to my Haskul calcluator!|]

getAddR :: Int -> Int -> Handler TypedContent
getAddR a b = selectRep $ do
    provideRep $ return
        [shamlet|
            <p>Add, #{ans}
        |]
    provideJson ans 
  where
    ans = a + b
	
	getSubR :: Int -> Int -> Handler TypedContent
getSubR a b = selectRep $ do
    provideRep $ return
        [shamlet|
            <p>Sub, #{ans}
        |]
    provideJson ans 
  where
    ans = a - b
	
	getAMulR :: Int -> Int -> Handler TypedContent
getMulR a b = selectRep $ do
    provideRep $ return
        [shamlet|
            <p>Mul, #{ans}
        |]
    provideJson ans 
  where
    ans = a * b
	
	getDivR :: Int -> Int -> Handler TypedContent
getDivR a b = selectRep $ do
    provideRep $ return
        [shamlet|
            <p>Div, #{ans}
        |]
    provideJson ans 
  where
    ans = a `div` b
  
main :: IO ()
main = warp 3000 HelloWorld
