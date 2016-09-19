{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}  -- this extension automatically makes a type "Text" from hardcoded "[Char]"

-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main ( main ) where

import Control.Concurrent
import Operationserver (operationserver)
import Guiserver (guiserver)

-- import qualified Data.Text.Lazy as T
-- import Data.Text.Lazy.Encoding (decodeUtf8)


main :: IO ()
main = do
    forkIO operationserver
    forkIO guiserver
    putStrLn "press [ENTER] to quit"
    aaa <- getLine  -- this is neccessary to keep the servers running
    return ()
