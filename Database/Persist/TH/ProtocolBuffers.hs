{-# LANGUAGE TemplateHaskell #-}
module Database.Persist.TH.ProtocolBuffers (
    derivePersistFieldPB
  ) where
import Database.Persist.Base
import Language.Haskell.TH.Syntax
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

strictify :: BSL.ByteString -> BS.ByteString
strictify x = BS.concat $ BSL.toChunks x

lazify :: BS.ByteString -> BSL.ByteString
lazify x = BSL.fromChunks [x]

derivePersistFieldPB :: String -> Q [Dec]
derivePersistFieldPB s = do
    ss <- [|SqlBlob|]
    tpv <- [|PersistByteString . strictify . messagePut|]
    fpv <- [|\dt v ->
                case fromPersistValue v of
                    Left e -> Left e
                    Right s' ->
                        case (messageGet . lazify) s' of
                            Left e ->
                              Left $ "Invalid " ++ dt ++ ": " ++ e
                            Right (_, x) | BSL.length x /= 0 ->
                              Left $ "Invalid " ++ dt ++ ": " ++ "excess input"
                            Right (msg, _) ->
                              Right msg|]
    return
        [ InstanceD [] (ConT ''PersistField `AppT` ConT (mkName s))
            [ FunD (mkName "sqlType")
                [ Clause [WildP] (NormalB ss) []
                ]
            , FunD (mkName "toPersistValue")
                [ Clause [] (NormalB tpv) []
                ]
            , FunD (mkName "fromPersistValue")
                [ Clause [] (NormalB $ fpv `AppE` LitE (StringL s)) []
                ]
            ]
        ]
