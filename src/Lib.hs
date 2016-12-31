module Lib where

import Language.Haskell.TH
import Control.Arrow

-- updated from https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial
autoDeriveAll :: Name -> Q [Dec]
autoDeriveAll name = do
  wrappedDec <- getDec name
  typeDerivs <- getWrappedTypeDerivs wrappedDec
  return $ mkVal (mkName $ "showType_" ++ nameBase name) $ show wrappedDec

--  return $ mkVal (mkName $ "showType_" ++ nameBase name) (stringifyName name)

-- p <- return $ mkVal (mkName $ "showType_" ++ nameBase name) (stringifyName name)
-- typeDerivs <- getWrappedTypeDerivs wrappedDec
-- return $ mkVal (mkName $ "showType_" ++ nameBase name) $ concatMap show typeDerivs

-- autoDeriveAll name = do
--   wrappedName <- getWrappedTypeName <$> getDec name
--   showType <- mkVal (mkName $ "showType_" ++ nameBase name) <$> wrappedName
--   return showType

stringifyName :: Name -> String
stringifyName name = modName ++ nameBase name
  where modName = maybe "" (++ ".") $ nameModule name

mkVal :: Name -> String -> [Dec]
mkVal tomake s = [ValD var exp []]
  where var = VarP tomake
        exp = NormalB . LitE . StringL $ s

getDec :: Name -> Q Dec
getDec name = reify name >>= (\x -> case x of
  TyConI tyDec -> return tyDec
  _ -> fail "no declaration found")

getWrappedTypeDerivs :: Dec -> Q [Type]
getWrappedTypeDerivs (NewtypeD a _ _ _ _ c) = return $ a ++ c
getWrappedTypeDerivs _ = fail "improper newtype declaration"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
