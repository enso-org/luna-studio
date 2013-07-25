---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import qualified Luna.Samples.HelloWorld        as HelloWorld
import qualified Luna.Tools.CodeGenerator       as CG
import qualified Luna.Network.Def.DefManager    as DefManager


data    Vector a b = Vector {x::a, y::b}
--newtype Vector  a b = Vector  {realtype :: Vector' a b}

newtype T1 a = T1 {realtype :: Vector Int a}
out x y = realtype (T1 $ Vector x y)

type X = Vector

foreign export ccall main2 :: IO ()

main2 = main

main :: IO ()
main = do 
    let
    	x = Vector
    --print $ DefManager.pathOf 1 HelloWorld.full_manager
    --putStrLn $ CG.generateTypeCode $ HelloWorld.base_workspace
    --putStrLn $ CG.generateDefCode 1 HelloWorld.full_manager
    putStrLn $ CG.generateFunction $ HelloWorld.myFun3
    --putStrLn $ Cg.generateDefCode $
    return ()

--        let 
--                (node, manager) = Samples.sample_helloWorld
--                nodeDef = Node.def node
--                graph = NodeDef.graph nodeDef
--        Graphviz.showGraph graph
--        print $ show $ TC.typeCheck graph manager
--        showCode node manager
--        putStrLn "=================================="
--        testSerialization
--        return ()


--testSerialization = do
--      let 
--            lib = Library.Library $ Path.fromUnixString "lunalib/std"
--        putStrLn "Hello programmer! I am Lunac, the Luna compiler"
--        pwd <- System.Directory.getCurrentDirectory
--        putStrLn $ "My PWD is " ++ pwd
--        print "Original manager:"
--        print $ snd Samples.sample_helloWorld
--        print "====================================="
--        print "load.save :"
--        DefManager.saveManager (Path.fromUnixString "lunalib") $ snd Samples.sample_helloWorld
--      manager <- DefManager.load lib DefManager.empty
--      print manager
        

--showCode :: Node -> DefManager -> IO ()
--showCode node manager = putStrLn $ CG.generateCode node manager

