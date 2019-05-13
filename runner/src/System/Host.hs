{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Host where

import Prologue
-- import Luna.Manager.Component.Pretty
import Control.Lens.Aeson
import Control.Monad.State.Layered
import Type.Known

import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.Text           as Text


-------------------
-- === Hosts === --
-------------------

-- === Definition === --

data System = Linux
            | Darwin
            | Windows
            deriving (Generic, Show, Read, Eq, Ord)

data SysArch = X32 | X64              deriving (Generic, Show, Read, Eq, Ord)
data SysDesc = SysDesc System SysArch deriving (Generic, Show, Eq, Ord)


-- === System discovery === --

currentHost :: System


#ifdef linux_HOST_OS
type CurrentHost = 'Linux
currentHost      =  Linux
#elif darwin_HOST_OS
type CurrentHost = 'Darwin
currentHost      =  Darwin
#elif mingw32_HOST_OS
type CurrentHost = 'Windows
currentHost      =  Windows
#else
Running on unsupported system.
#endif


-- === Arch discovery === --

currentArch :: SysArch

#ifdef i386_HOST_ARCH
type CurrentArch = 'X32
currentArch      =  X32
#elif x86_64_HOST_ARCH
type CurrentArch = 'X64
currentArch      =  X64
#else
Running on unsupported system architecture.
#endif


-- === Utils === --

currentSysDesc :: SysDesc
currentSysDesc = SysDesc currentHost currentArch

instance Known 'Linux   where fromType = Linux
instance Known 'Darwin  where fromType = Darwin
instance Known 'Windows where fromType = Windows

instance Known 'X32     where fromType = X32
instance Known 'X64     where fromType = X64


-- === Instances === --

-- JSON
instance ToJSON   System  where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   SysArch where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON   SysDesc where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON
instance FromJSON System  where parseJSON  = lensJSONParse
instance FromJSON SysArch where parseJSON  = lensJSONParse
instance FromJSON SysDesc where parseJSON  = lensJSONParse
instance FromJSONKey SysDesc where
    fromJSONKey = JSON.FromJSONKeyTextParser $ either (fail . convert) return . readPretty
instance ToJSONKey   SysDesc where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty

-- Show
class Pretty a where
    showPretty :: a -> Text
    readPretty :: Text -> Either Text a

    default showPretty :: Show a => a -> Text
    default readPretty :: Read a => Text -> Either Text a
    showPretty = convert . show
    readPretty = mapLeft (convert . ("Read error. Remaining part: " <>)) . tryReads

instance Pretty Text where
    showPretty = id
    readPretty = Right

instance Pretty SysDesc where
    showPretty (SysDesc s a) = showPretty s <> "." <> showPretty a
    readPretty t = case Text.splitOn "." t of
        [s,a] -> mapLeft (const "Conversion error") $ SysDesc <$> readPretty s <*> readPretty a
        _     -> Left "Incorrect system architecture format"

instance Pretty System  where
    showPretty = Text.toLower . convert . show
    readPretty = mapLeft (const "Conversion error") . tryReads . Text.toTitle

instance Pretty SysArch where
    showPretty = Text.toLower . convert . show
    readPretty = mapLeft (const "Conversion error") . tryReads . Text.toTitle



-------------------------------------------
-- === Host dependend configurations === --
-------------------------------------------

-- === Definition === --

class Monad m => MonadHostConfig cfg (system :: System) (arch :: SysArch) m where
    defaultHostConfig :: m cfg


-- === Utils === --

defaultHostConfigFor :: forall system arch cfg m. MonadHostConfig cfg system arch m => m cfg
defaultHostConfigFor = defaultHostConfig @cfg @system @arch

type MonadHostConfig' cfg = MonadHostConfig cfg CurrentHost CurrentArch
defHostConfig :: MonadHostConfig' cfg m => m cfg
defHostConfig = defaultHostConfigFor @CurrentHost @CurrentArch

evalDefHostConfig :: forall s m a. MonadHostConfig' s m => StateT s m a -> m a
evalDefHostConfig p = evalStateT @s p =<< defHostConfig


-- === Multiple configs evaluator ===

class MultiConfigRunner (cfgs :: [*]) m where
    evalDefHostConfigs :: forall a. StatesT cfgs m a -> m a

instance (MultiConfigRunner ss m, MonadHostConfig' s (StatesT ss m))
      => MultiConfigRunner (s ': ss) m where evalDefHostConfigs = evalDefHostConfigs @ss . evalDefHostConfig
instance MultiConfigRunner '[]       m where evalDefHostConfigs = id
