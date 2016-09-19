{-# LANGUAGE OverloadedStrings #-}  -- this extension automatically makes a type "Text" from hardcoded "[Char]"

-----------------------------------------------------------------------------
--
-- Module      :  Operationserver
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Operationserver (
    operationserver
) where

import Web.Scotty
import Network.Wai
import Network.Wai.Middleware.RequestLogger

import Control.Monad.Trans
import Data.Monoid

import Calculator as C

import qualified Data.Text.Lazy as T
-- import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)

data Calcaction = Add | Mult | Sub | Endless


operationserver :: IO ()
operationserver = scotty 3002 $ do
    middleware logStdoutDev     -- the request logger automatically outputs requests to the console

    get "/" $ html $ T.pack ( concat ["<p>You have successfully connected to the OperationServer of the Test-Calculator via its REST interface.</p>"
                                     , "<p>The following commands are available"
                                     , "<ul><li>operationadd: Addition with parameters 'a' and 'b'</li>"
                                     , "<li>operationmult: Addition with parameters 'a' and 'b'</li>"
                                     , "<li>operationsub: Addition with parameters 'a' and 'b'</li>"
                                     , "<li>operationpower: Addition with parameter 'a'</li>"
                                     , "<li>operationendless: A stream of numbers without any parameters</li>"
                                     , "</ul></p>"
                                     ] )

    get "/operationadd" $ do
        numone <- param "a"
        numtwo <- param "b"
        let dnumone = read (T.unpack numone) :: Integer
        let dnumtwo = read (T.unpack numtwo) :: Integer
        let result = T.pack $ show (C.add dnumone dnumtwo)
        text $ result

    get "/operationmult" $ do
        numone <- param "a"
        numtwo <- param "b"
        let dnumone = read (T.unpack numone) :: Integer
        let dnumtwo = read (T.unpack numtwo) :: Integer
        let result = T.pack $ show (C.mult dnumone dnumtwo)
        text $ result

    get "/operationsub" $ do
        numone <- param "a"
        numtwo <- param "b"
        let dnumone = read (T.unpack numone) :: Integer
        let dnumtwo = read (T.unpack numtwo) :: Integer
        let result = T.pack $ show (C.sub dnumone dnumtwo)
        text $ result

    get "/operationendless" $ do
        -- let result = T.pack $ show ( [ (show x) ++ "</br>" | x <- [1..] ] )
        let result = T.pack $ show C.endl
        text $ result

    -- this operation utilizes only the C.multiplication operation
    get "/operationpower" $ do
        numone <- param "a"
        let dnumone = read (T.unpack numone) :: Integer
        let result = T.pack $ show (C.mult dnumone dnumone)
        text $ result

