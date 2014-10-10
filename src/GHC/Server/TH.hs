{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Macros.

module GHC.Server.TH where

import           GHC.Server.Logging
import           GHC.Server.Types

import           Control.Concurrent
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.AttoLisp as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Monoid
import qualified Data.Text as T
import           Language.Haskell.TH

-- | Dispatch
dispatch :: Name -> Q Exp
dispatch t =
  do info <- reify t
     case info of
       TyConI (DataD _ _ _ cs _) ->
         [|\ghcChan' ix' h' inputChan' cmd' state' ->
             do $(logDebug)
                  ("Command: " <>
                   T.pack (show cmd'))
                $(caseE [|cmd'|]
                        (map (\(ForallC _ [EqualP _ returnType] (NormalC name args)) ->
                                let argNames =
                                      zipWith (const . mkName . ("x" <>) . show)
                                              [0 :: Int ..]
                                              args
                                in match (conP name (map varP argNames))
                                         (normalB [|(do outChan <- liftIO newChan
                                                        void (liftIO (forkIO (runLogging
                                                                                (do contents <- liftIO (getChanContents outChan)
                                                                                    forM_ contents
                                                                                          (\o ->
                                                                                             do $(logDebug)
                                                                                                  ("Sending " <>
                                                                                                   T.pack (show (L.encode (Outgoing ix' (FeedOut o)))))
                                                                                                liftIO (L8.hPutStrLn
                                                                                                          h'
                                                                                                          (L.encode (Outgoing $([|ix'|]) (FeedOut o)))))
                                                                                    $(logDebug) ("Finished consuming outChan.")))))
                                                        r <- liftIO (runReaderT (runDuplexT ($(foldl (\f a ->
                                                                                                        appE f a)
                                                                                                     (varE (mkName (decapitalize (nameBase name))))
                                                                                                     (map varE argNames)) :: $(return returnType)))
                                                                                (DuplexState inputChan'
                                                                                             outChan
                                                                                             ghcChan'
                                                                                             state'))
                                                        $(logDebug)
                                                          ("Result: " <>
                                                           T.pack (show r))
                                                        liftIO (L8.hPutStrLn h'
                                                                             (L.encode (Outgoing ix' (EndResult r)))))|])
                                         [])
                             cs))|]
       _ -> error "Dispatch argument should be a GADT type."
  where decapitalize :: String -> String
        decapitalize [] = []
        decapitalize (x:xs) = toLower x : xs
