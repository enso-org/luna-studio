{-# LANGUAGE OverloadedStrings #-}
module React.View.Port where

import qualified Data.JSString.Text as JS
import qualified Event.UI           as UI
import qualified Numeric            as Numeric
import           Object.Widget.Port (Port)
import qualified Object.Widget.Port as Port
import           React.Flux
import qualified React.Flux         as React
import           React.Store        (Ref, dispatch)
import           React.Store.Node   (Node)
import qualified React.Store.Node   as Node
import           Utils.PreludePlus


data Portkind a b = Input  Int Int
                  | Output Int Int
                  | Self
                  deriving (Eq, Ord)


showF :: Float -> String
showF a = Numeric.showFFloat (Just 1) a ""


name :: JSString
name = "port"


port :: Ref Node -> ReactView Port
port ref = React.defineView name $ \port -> do
    drawPort_ (Output 1 3)


port_ :: Ref Node -> Port -> ReactElementM ViewEventHandler ()
port_ ref p = React.view (port ref) p mempty


drawPort_ :: Portkind a b -> ReactElementM ViewEventHandler ()
drawPort_ Self = let color = "#8ABEB7" in
    circle_
        [ "className" $= "port port--self"
        , "fill"      $= color
        , "stroke"    $= color
        ] mempty
drawPort_ (Input  a b) = drawPortIO_ a b   1  "0" "1"
drawPort_ (Output a b) = drawPortIO_ a b (-1) "1" "0"


drawPortIO_ :: Int -> Int -> Float -> String -> String -> ReactElementM ViewEventHandler ()
drawPortIO_ number inputs mod1 mod2 mod3 = do
    let color = "#8ABEB7"
        r1    = 20 :: Float
        line  = 3 :: Float
        gap   = 0.15 :: Float
        r2   = r1 - line
        gap' = gap * (r1/r2)

        t   = pi / fromIntegral inputs
        t1  = fromIntegral number * t - pi - t + gap/2
        t2  = fromIntegral number * t - pi - gap/2
        t1' = fromIntegral number * t - pi - t + gap'/2
        t2' = fromIntegral number * t - pi - gap'/2

        ax = showF $ r1 * sin(t1 * mod1) + r1
        ay = showF $ r1 * cos(t1 * mod1) + r1

        bx = showF $ r1 * sin(t2 * mod1) + r1
        by = showF $ r1 * cos(t2 * mod1) + r1

        cx = showF $ r2 * sin(t2' * mod1) + r1
        cy = showF $ r2 * cos(t2' * mod1) + r1

        dx = showF $ r2 * sin(t1' * mod1) + r1
        dy = showF $ r2 * cos(t1' * mod1) + r1

        svgPath = fromString $ "M" <> ax <> " " <> ay <> " A " <> show r1 <> " " <> show r1 <> " 1 0 " <> mod2 <> " " <> bx <> " " <> by <>
                              " L" <> cx <> " " <> cy <> " A " <> show r2 <> " " <> show r2 <> " 1 0 " <> mod3 <> " " <> dx <> " " <> dy <>
                              " L" <> ax <> " " <> ay

    path_
        [ "className" $= (fromString $ "port port--i port--i--" <> show number)
        , "fill"      $= color
        , "stroke"    $= color
        , "d"         $= svgPath
        ] mempty


--TODO[react] probably remove
-- displayPorts :: WidgetId -> Node -> Command Global.State ()
-- displayPorts wid node = do
--         portGroup <- inRegistry $ UICmd.get wid $ Model.elements . Model.portGroup
--         oldPorts  <- inRegistry $ UICmd.children portGroup
--         oldPortWidgets <- forM oldPorts $ \wid' -> inRegistry $ (UICmd.lookup wid')
--         let portRefs = (view PortModel.portRef) <$> oldPortWidgets
--         forM_ portRefs $ \wid' -> Global.graph . Graph.portWidgetsMap . at wid' .= Nothing
--         inRegistry $ mapM_ UICmd.removeWidget oldPorts
--
--         groupId      <- inRegistry $ Node.portControlsGroupId wid
--         portControls <- inRegistry $ UICmd.children groupId
--         inRegistry $ mapM_ UICmd.removeWidget portControls
--
--         inLabelsGroupId <- inRegistry $ Node.inLabelsGroupId wid
--         inLabels        <- inRegistry $ UICmd.children inLabelsGroupId
--         inRegistry $ mapM_ UICmd.removeWidget inLabels
--
--         outLabelsGroupId <- inRegistry $ Node.outLabelsGroupId wid
--         outLabels        <- inRegistry $ UICmd.children outLabelsGroupId
--         inRegistry $ mapM_ UICmd.removeWidget outLabels
--
--         forM_ (makePorts node    ) $ \p -> do
--             portWidgetId <- inRegistry $ UICmd.register portGroup p def
--             Global.graph . Graph.portWidgetsMap . at (p ^. PortModel.portRef) ?= portWidgetId
--         inRegistry $ forM_ (node ^. Node.ports) $ makePortControl groupId node
--         inRegistry $ forM_ (node ^. Node.ports) $ \p -> case p ^. Port.portId of
--             InPortId  Self -> return ()
--             InPortId  _    -> makePortLabel inLabelsGroupId  p
--             OutPortId _    -> makePortLabel outLabelsGroupId p

--TODO[react] probably remove
-- makePortLabel :: WidgetId -> Port -> Command UIRegistry.State ()
-- makePortLabel parent port = do
--     let align = case port ^. Port.portId of
--             InPortId  _ -> Label.Right
--             OutPortId _ -> Label.Left
--         label = Label.create (Vector2 360 15) text & Label.alignment .~ align
--         text  = (Text.pack $ port ^. Port.name) <> " :: " <> portType
--         portType = port ^. Port.valueType . ValueType.toText
--     UICmd.register_ parent label def
