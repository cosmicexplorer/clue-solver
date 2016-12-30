{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Language.Haskell.TH
import Control.Arrow

-- updated from https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial
autoDeriveAll :: Name -> Q [Dec]
autoDeriveAll name = do
  wrappedName <- getWrappedTypeName <$> getDec name
  return []

getDec :: Name -> Q Dec
getDec name = reify name >>= (\x -> case x of
  TyConI tyDec -> return tyDec
  _ -> fail "no declaration found")

getWrappedTypeName :: Dec -> Q Name
getWrappedTypeName (NewtypeD _ _ _ _ (NormalC _ [(_, ConT name)]) _) =
  return name
getWrappedTypeName _ = fail "improper newtype declaration"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
