{-# LANGUAGE OverloadedStrings #-}

module Style.Layout where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import qualified Object.Widget.Button as Button
import qualified Object.Widget.Group  as Group
import qualified Object.Widget.Label  as Label
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
breadcrumbPosition             :: Vector2 Double
breadcrumbPosition             = Vector2 sidebarWidth 0.0

breadcrumbStyle               :: Group.Style
breadcrumbStyle                = def & Group.background   ?~ Color 1.0 1.0 1.0 0.05
                                     & Group.padding      .~ xyPadding 5.0 0.0
                                     & Group.borderRadius .~ (0, 0, 0, 0)

breadcrumbsHeight              :: Double
breadcrumbsHeight              = 20.0

breadcrumbItemStyle            :: Button.Style
breadcrumbItemSize             = Vector2 150.0 20.0 :: Vector2 Double

breadcrumbItemSize            :: Vector2 Double
breadcrumbItemStyle            = def & Button.rounded    .~ False
                                     & Button.background .~ transparent
                                     & Button.alignment  .~ Label.Left

projectChooser                 :: Group.Style
projectChooser                 = def & Group.padding    .~ uniformPadding 5.0
                                     & Group.background ?~ sidebarBackground

projectListItemSize, createProjectButtonSize :: Vector2 Double
projectListItemSize            = Vector2 190.0 20.0
createProjectButtonSize        = Vector2 200.0 20.0

createProjectDialogPosition, createProjectDialogTextBoxSize, createProjectDialogOKSize, createProjectDialogCancelSize :: Vector2 Double
createProjectDialogPosition    = Vector2 230.0 40.0
createProjectDialogTextBoxSize = Vector2 200.0 20.0
createProjectDialogOKSize      = Vector2 100.0 20.0
createProjectDialogCancelSize  = Vector2  80.0 20.0

createProjectDialogStyle      :: Group.Style
createProjectDialogStyle       = def & Group.background ?~ Color 0.3 0.3 0.5 1.0
                               & Group.padding    .~ uniformPadding 5.0

projectChooserStyle :: Group.Style
projectChooserStyle = def & Group.background   ?~ Color 0.55 0.55 0.55 1.0
                          & Group.padding      .~ uniformPadding 5.0

textEditorToggle :: Button.Button
textEditorToggle = Button.create (Vector2 10 1000) ":" & Button.style .~ style where
     style = def & Button.rounded    .~ False
                 & Button.background .~ Color 0.1 0.1 0.1 1.0
                 & Button.alignment  .~ Label.Center

outputsEdgeStyle :: Group.Style
outputsEdgeStyle = def & Group.background   ?~ Color 0.0 0.0 0.0 0.0
                       & Group.padding      .~ uniformPadding 5.0

inputsEdgeStyle :: Group.Style
inputsEdgeStyle = def & Group.background   ?~ Color 0.0 0.0 0.0 0.0
                      & Group.padding      .~ uniformPadding 5.0


errorMessageWrapMargin :: Int
errorMessageWrapMargin = 30

errorMessageWidgetSize, visualizationWidgetSize :: Vector2 Double
errorMessageWidgetSize  = Vector2 200 200
visualizationWidgetSize = Vector2 200 200


gridSize :: Int
gridSize = 8
