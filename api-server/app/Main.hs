{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import GroupNote (start, Options(..))
import Options.Declarative

main :: IO ()
main = run_ groupNote

groupNote
    :: Flag "" '["debug"] "BOOL" "enable debug mode" Bool
    -> Arg "CONF" String
    -> Cmd "Group Note API server" ()
groupNote debug conf = liftIO $ start $ Options (get conf) (get debug)
