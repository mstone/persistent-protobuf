{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- | 'derivePersistFieldPB' uses Template Haskell to produce
--   'Database.Persist.PersistField' instances for types with
--   'Text.ProtocolBuffers.Reflections.ReflectDescriptor' and
--   'Text.ProtocolBuffers.WireMessage.Wire' instances just as
--   'Database.Persist.TH.derivePersistField' produces
--   'Database.Persist.PersistField' instances for types with 'Read' and 'Show'
--   instances.
module Database.Persist.TH.ProtocolBuffers (
    derivePersistFieldPB
  ) where
import Database.Persist.Store
import Language.Haskell.TH.Syntax
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

strictify :: BSL.ByteString -> BS.ByteString
strictify x = BS.concat $ BSL.toChunks x

lazify :: BS.ByteString -> BSL.ByteString
lazify x = BSL.fromChunks [x]

-- | Derive 'Database.Persist.PersistField' instances for 'typName'. 'typName'
--   should be an instance of the
--   'Text.ProtocolBuffers.Reflections.ReflectDescriptor' and
--   'Text.ProtocolBuffers.WireMessage.Wire' classes.
derivePersistFieldPB :: String  -- ^ Name of the type to derive instances for.
                     -> Q [Dec]
derivePersistFieldPB typName = do
    ss <- [|SqlBlob|]
    tpv <- [|PersistByteString . strictify . messagePut|]
    fpv <- [|\dt v ->
                case fromPersistValue v of
                    Left e -> Left e
                    Right s' ->
                        case (messageGet . lazify) s' of
                            Left e ->
                              Left $ T.concat ["Invalid ", dt, ": ", T.pack e]
                            Right (_, x) | BSL.length x /= 0 ->
                              Left $ T.concat ["Invalid ", dt, ": excess input"]
                            Right (msg, _) ->
                              Right msg|]
    return
        [ InstanceD [] (ConT ''PersistField `AppT` ConT (mkName typName))
            [ FunD (mkName "sqlType")
                [ Clause [WildP] (NormalB ss) []
                ]
            , FunD (mkName "toPersistValue")
                [ Clause [] (NormalB tpv) []
                ]
            , FunD (mkName "fromPersistValue")
                [ Clause [] (NormalB $ fpv `AppE` LitE (StringL typName)) []
                ]
            ]
        ]
