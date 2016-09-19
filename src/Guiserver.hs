{-# LANGUAGE OverloadedStrings #-}  -- this extension automatically makes a type "Text" from hardcoded "[Char]"

-----------------------------------------------------------------------------
--
-- Module      :  Guiserver
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


module Guiserver (
    guiserver
) where

-- REST server
import Web.Scotty
import Network.Wai
import Network.Wai.Middleware.RequestLogger

-- REST client
import Network.HTTP
-- import Control.Applicative

import Control.Monad.Trans
import Data.Monoid

import qualified Data.Text.Lazy as T

data HtmlElement = Html String
                   | HtmlEnd
                   | Examplebody
                   | Button String String String
                   | Input String
                   | Form String
                   | Info String
                   | Br

data JSElement = AjaxFunctions

instance Show HtmlElement where
    show (Html a) = if a == "start" then "<html><head></head><body>" else "</body></html>"
    show Examplebody = "<h1>REST Calculator Interface</h1><p>This is an HTML interface to operate a purely functional Haskell program.</p>"
    show (Button name caption action) = "<input type='button' id='" ++ name ++ "' value='" ++ caption ++ "' onclick='" ++ action ++ "'></input>"
    show (Input name) = "<input type='text' id='" ++ name ++ "'></input>"
    show (Form a) = if a == "start" then "<form>" else "</form>"
    show (Info name) = "<div id = '" ++ name ++ "'></div>"
    show Br = "</br>"

instance Show JSElement where
    show AjaxFunctions = "<script src='http://code.jquery.com/jquery-1.9.1.min.js'></script><script type='text/javascript'></script>"


guiserver :: IO ()
guiserver = scotty 3005 $ do
    middleware logStdoutDev     -- the request logger automatically outputs requests to the console

    -- generating the default GUI
    get "/" $ html (T.pack ( (show $ Html "start")
                          ++ (show AjaxFunctions)
                          ++ (show Examplebody)
                          ++ (show $ Form "start")
                          ++ (show $ Input "addvala")
                          ++ (show $ Br)
                          ++ (show $ Input "addvalb")
                          ++ (show $ Button "doadd"
                                             "Perform Addition"
                                             "$.get(\"http://localhost:3005/guiadd?a=\" + $(\"#addvala\").val() + \"&b=\" + $(\"#addvalb\").val(), function(data){ $(\"#addresult\").html(data); } )"
                             )
                          ++ (show $ Info "addresult")
                          ++ (show $ Form "end")
                          ++ (show $ Form "start")
                          ++ (show $ Input "subvala")
                          ++ (show $ Br)
                          ++ (show $ Input "subvalb")
                          ++ (show $ Button "dosub"
                                             "Perform Substraction"
                                             "$.get(\"http://localhost:3005/guisub?a=\" + $(\"#subvala\").val() + \"&b=\" + $(\"#subvalb\").val(), function(data){ $(\"#subresult\").html(data); } )"
                             )
                          ++ (show $ Info "subresult")
                          ++ (show $ Form "end")
                          ++ (show $ Form "start")
                          ++ (show $ Input "multvala")
                          ++ (show $ Br)
                          ++ (show $ Input "multvalb")
                          ++ (show $ Button "domult"
                                             "Perform Multiplication"
                                             "$.get(\"http://localhost:3005/guimult?a=\" + $(\"#multvala\").val() + \"&b=\" + $(\"#multvalb\").val(), function(data){ $(\"#multresult\").html(data); } )"
                             )
                          ++ (show $ Info "multresult")
                          ++ (show $ Form "end")
                          ++ (show $ Form "start")
                          ++ (show $ Input "powervala")
                          ++ (show $ Button "dopower"
                                             "Perform Power of Two"
                                             "$.get(\"http://localhost:3005/guipower?a=\" + $(\"#powervala\").val(), function(data){ $(\"#powerresult\").html(data); } )"
                             )
                          ++ (show $ Info "powerresult")
                          ++ (show $ Form "end")
                          ++ (show $ Form "start")
                          ++ (show $ Button "doendless"
                                             "Generate an (!!) endless stream of numbers and adore the power of lazy evaluation even when using REST"
                                             --"$.get(\"http://localhost:3005/guiendless\", function(data){ $(\"#endlessresult\").html(data); } )"
                                             "location.href=\"http://localhost:3002/operationendless\""
                             )
                          ++ (show $ Info "endlessresult")
                          ++ (show $ Html "end")
                             )
                   )


    -- the GUI-Operation when pressing the "Perform Addition" button
    get "/guiadd" $ do                                      -- ScottyM ()
        parama <- param "a"
        paramb <- param "b"
        let numbera = read (T.unpack parama) :: Integer
        let numberb = read (T.unpack paramb) :: Integer

        -- call the appropriate operation on the OperationServer
        let url = concat ["http://localhost:3002/operationadd?a=", show numbera, "&b=", show numberb]
        resp <- liftIO (simpleHTTP (getRequest url))        -- getting the result from a type of: IO (Result (Response ty))
        body <- liftIO (getResponseBody resp)               -- getting the result from a type of: IO ty
        text $ T.pack body                                  -- ActionM ()

    -- the GUI-Operation when pressing the "Perform Substraction" button
    get "/guisub" $ do                                      -- ScottyM ()
        parama <- param "a"
        paramb <- param "b"
        let numbera = read (T.unpack parama) :: Integer
        let numberb = read (T.unpack paramb) :: Integer

        -- call the appropriate operation on the OperationServer
        let url = concat ["http://localhost:3002/operationsub?a=", show numbera, "&b=", show numberb]
        resp <- liftIO (simpleHTTP (getRequest url))        -- getting the result from a type of: IO (Result (Response ty))
        body <- liftIO (getResponseBody resp)               -- getting the result from a type of: IO ty
        text $ T.pack body                                  -- ActionM ()

    -- the GUI-Operation when pressing the "Perform Multiplilcation" button
    get "/guimult" $ do                                      -- ScottyM ()
        parama <- param "a"
        paramb <- param "b"
        let numbera = read (T.unpack parama) :: Integer
        let numberb = read (T.unpack paramb) :: Integer

        -- call the appropriate operation on the OperationServer
        let url = concat ["http://localhost:3002/operationmult?a=", show numbera, "&b=", show numberb]
        resp <- liftIO (simpleHTTP (getRequest url))        -- getting the result from a type of: IO (Result (Response ty))
        body <- liftIO (getResponseBody resp)               -- getting the result from a type of: IO ty
        text $ T.pack body                                  -- ActionM ()

    -- the GUI-Operation when pressing the "Endless Stream" button
    -- this function is not used since it is not compatible with streams - the operationserver is called directly
    get "/guiendless" $ do                                  -- ScottyM ()
        -- call the appropriate operation on the OperationServer
        let url = "http://localhost:3002/operationendless"
        resp <- liftIO (simpleHTTP (getRequest url))        -- getting the result from a type of: IO (Result (Response ty))
        body <- liftIO (getResponseBody resp)               -- getting the result from a type of: IO ty
        text $ T.pack body                                  -- ActionM ()

    -- the GUI-Operation when pressing the "Perform Power" button
    get "/guipower" $ do                                      -- ScottyM ()
        parama <- param "a"
        let numbera = read (T.unpack parama) :: Integer

        -- call the appropriate operation on the OperationServer
        let url = concat ["http://localhost:3002/operationpower?a=", show numbera]
        resp <- liftIO (simpleHTTP (getRequest url))        -- getting the result from a type of: IO (Result (Response ty))
        body <- liftIO (getResponseBody resp)               -- getting the result from a type of: IO ty
        text $ T.pack body                                  -- ActionM ()


