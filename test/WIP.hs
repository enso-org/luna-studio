-- extensions --
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- module --
module Main where

-- imports --
import Luna.Target.HS

-- body --
#include "pragmas.cpp"

-- ====== Main type ====== --
data Main  = Main deriving (Show, Eq, Ord, Generic, Typeable)
data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic, Typeable)

-- ------ Main.Main constructor ------ --
cons_Main = _member("Main") (val Cls_Main)
memDef_Cls_Main_Main = liftCons0 Main

-- ====== Method: Cls_Main.Main ====== --
memSig_Cls_Main_Main = _rtup1(_nuSigArg("self"))
memFnc_Cls_Main_Main = (memSig_Cls_Main_Main, memDef_Cls_Main_Main)
$(registerMethod ''Cls_Main "Main")

-- ------ Main methods ------ --

-- ====== Method: Main.print ====== --
memSig_Main_print = _rtup2(_nuSigArg("self"), _npSigArg("s", val ("" :: String)))
memDef_Main_print self s = do 
     
    polyJoin . liftF1 (Value . fmap Safe . print) $ s
     

memFnc_Main_print = (memSig_Main_print, memDef_Main_print)
$(registerMethod ''Main "print")

-- ====== Method: Main.id_50 ====== --
memSig_Main_id_50 = _rtup1(_nuSigArg("self"))
memDef_Main_id_50 _self = do 
     val (4 :: Int)
     

memFnc_Main_id_50 = (memSig_Main_id_50, memDef_Main_id_50)
$(registerMethod ''Main "id_50")

-- ====== Method: Main.id ====== --
memSig_Main_id = _rtup2(_nuSigArg("self"), _nuSigArg("x"))
memDef_Main_id _self _x = do 
     _x
     

memFnc_Main_id = (memSig_Main_id, memDef_Main_id)
$(registerMethod ''Main "id")

-- ====== Method: Main.foo ====== --
memSig_Main_foo = _rtup2(_nuSigArg("self"), _nuSigArg("f"))
memDef_Main_foo _self _f = do 
     val (_call(19) (appNext (val (5 :: Int)) _f), _call(24) (appNext (val ("a" :: String)) _f))
     

memFnc_Main_foo = (memSig_Main_foo, memDef_Main_foo)
$(registerMethod ''Main "foo")

-- ====== Method: Main.bar ====== --
memSig_Main_bar = _rtup3(_nuSigArg("self"), _nuSigArg("a"), _nuSigArg("b"))
memDef_Main_bar _self _a _b = do 
     val (_a, _b)
     

memFnc_Main_bar = (memSig_Main_bar, memDef_Main_bar)
$(registerMethod ''Main "bar")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1(_nuSigArg("self"))
memDef_Main_main _self = do 
     _call(31) (appNext (_call(34) (appNext (_member("id") _self) (_member("foo") _self))) (_member("print") _self))
     _call(38) (appNext (_call(40) (_member("id_50") _self)) (_member("print") _self))
     _g <- appNext (val (1 :: Int)) (_member("bar") _self)
     _call(49) (appNext (_call(53) (appNext (val (3 :: Int)) _g)) (_member("print") _self))
     

memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

