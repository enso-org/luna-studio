{-# LANGUAGE OverloadedStrings #-}

module Style.Layout where

import           Luna.Studio.Data.Vector (Position (Position), Size (Size), Vector2 (Vector2))
import           Luna.Studio.Prelude

import qualified Object.Widget.Group     as Group
import           Style.Types



sidebarPadding                 :: Double
sidebarPadding                 = 10.0

sidebarBackground              :: Color
sidebarBackground              = Color 0.35 0.35 0.35 1.0

sidebar                        :: Group.Style
sidebar                        = def & Group.background   ?~ sidebarBackground
                                     & Group.padding      .~ uniformPadding sidebarPadding
                                     & Group.borderRadius .~ (0, 0, 0, 0)

sidebarWidth                   :: Double
sidebarWidth                   = 0.0

breadcrumbBackground           :: Color
breadcrumbBackground           = sidebarBackground
breadcrumbPosition             :: Position
breadcrumbPosition             = Position (Vector2 sidebarWidth 0.0)

breadcrumbStyle               :: Group.Style
breadcrumbStyle                = def & Group.background   ?~ Color 1.0 1.0 1.0 0.05
                                     & Group.padding      .~ xyPadding 5.0 0.0
                                     & Group.borderRadius .~ (0, 0, 0, 0)

breadcrumbsHeight              :: Double
breadcrumbsHeight              = 20.0

-- breadcrumbItemStyle            :: Button.Style
-- breadcrumbItemSize             = Size (Vector2 150.0 20.0) :: Size
--
-- breadcrumbItemSize            :: Size
-- breadcrumbItemStyle            = def & Button.rounded    .~ False
--                                      & Button.background .~ transparent
--                                      & Button.alignment  .~ Label.Left

projectChooser                 :: Group.Style
projectChooser                 = def & Group.padding    .~ uniformPadding 5.0
                                     & Group.background ?~ sidebarBackground

projectListItemSize, createProjectButtonSize :: Size
projectListItemSize            = Size (Vector2 190.0 20.0)
createProjectButtonSize        = Size (Vector2 200.0 20.0)

createProjectDialogPosition   :: Position
createProjectDialogPosition    = Position (Vector2 230.0 40.0)

createProjectDialogTextBoxSize, createProjectDialogOKSize, createProjectDialogCancelSize :: Size
createProjectDialogTextBoxSize = Size (Vector2 200.0 20.0)
createProjectDialogOKSize      = Size (Vector2 100.0 20.0)
createProjectDialogCancelSize  = Size (Vector2  80.0 20.0)

createProjectDialogStyle      :: Group.Style
createProjectDialogStyle       = def & Group.background ?~ Color 0.3 0.3 0.5 1.0
                               & Group.padding    .~ uniformPadding 5.0

projectChooserStyle :: Group.Style
projectChooserStyle = def & Group.background   ?~ Color 0.55 0.55 0.55 1.0
                          & Group.padding      .~ uniformPadding 5.0

-- textEditorToggle :: Button.Button
-- textEditorToggle = Button.create (Size (Vector2 10 1000)) ":" & Button.style .~ style where
--      style = def & Button.rounded    .~ False
--                  & Button.background .~ Color 0.1 0.1 0.1 1.0
--                  & Button.alignment  .~ Label.Center

outputsEdgeStyle :: Group.Style
outputsEdgeStyle = def & Group.background   ?~ Color 0.0 0.0 0.0 0.0
                       & Group.padding      .~ uniformPadding 5.0

inputsEdgeStyle :: Group.Style
inputsEdgeStyle = def & Group.background   ?~ Color 0.0 0.0 0.0 0.0
                      & Group.padding      .~ uniformPadding 5.0


errorMessageWrapMargin :: Int
errorMessageWrapMargin = 30

errorMessageWidgetSize, visualizationWidgetSize :: Size
errorMessageWidgetSize  = Size (Vector2 200 200)
visualizationWidgetSize = Size (Vector2 200 200)


gridSize :: Int
gridSize = 16
