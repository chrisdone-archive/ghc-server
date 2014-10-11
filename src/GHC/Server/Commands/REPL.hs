-- | REPL-context commands.

module GHC.Server.Commands.REPL
  (typeOf,kindOf,infoOf)
  where

import           GHC.Compat
import           GHC.Server.Duplex
import           GHC.Server.Types

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

-- | Type of identifier.
typeOf :: Text -> Returns Text
typeOf expr =
  withGhc (do typ <- exprType (T.unpack expr)
              df <- getSessionDynFlags
              return (formatType df typ))

-- | Kind of the identifier.
kindOf :: Text -> Returns Text
kindOf expr =
  withGhc (do typ <- typeKind (T.unpack expr)
              df <- getSessionDynFlags
              return (formatType df typ))

-- | Info of the identifier.
infoOf :: Text -> Returns [Text]
infoOf ident =
  withGhc (do names <- parseName (T.unpack ident)
              df <- getSessionDynFlags
              infos <- fmap (concatMap (\(t,f,cs) ->
                                          showppr df t :
                                          showppr df f :
                                          map (showppr df) cs) .
                             catMaybes)
                            (mapM getInfo names)
              let spans' =
                    map ((\x ->
                            case x of
                              (RealSrcSpan i) -> printSpan i
                              _ -> "???") .
                         getSrcSpan)
                        names
                    where printSpan s =
                            "Defined in " ++
                            unpackFS (srcSpanFile s)
              return (map T.pack
                          (zipWith (\x y ->
                                      unlines [x,y])
                                   spans'
                                   infos)))

-- | Pretty print a type.
formatType :: DynFlags -> Type -> Text
formatType dflags = T.pack . unwords . lines . showppr dflags . snd .
                    splitForAllTys
