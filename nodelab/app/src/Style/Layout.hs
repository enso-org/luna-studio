{-# LANGUAGE OverloadedStrings #-}

module Style.Layout where

import           Luna.Studio.Data.Vector (Position (Position), Size (Size), Vector2 (Vector2))
import           Luna.Studio.Prelude

import           Style.Types



sidebarPadding                 :: Double
sidebarPadding                 = 10.0

sidebarBackground              :: Color
sidebarBackground              = Color 0.35 0.35 0.35 1.0

sidebarWidth                   :: Double
sidebarWidth                   = 0.0

breadcrumbBackground           :: Color
breadcrumbBackground           = sidebarBackground
breadcrumbPosition             :: Position
breadcrumbPosition             = Position (Vector2 sidebarWidth 0.0)

breadcrumbsHeight              :: Double
breadcrumbsHeight              = 20.0

projectListItemSize, createProjectButtonSize :: Size
projectListItemSize            = Size (Vector2 190.0 20.0)
createProjectButtonSize        = Size (Vector2 200.0 20.0)

createProjectDialogPosition   :: Position
createProjectDialogPosition    = Position (Vector2 230.0 40.0)

createProjectDialogTextBoxSize, createProjectDialogOKSize, createProjectDialogCancelSize :: Size
createProjectDialogTextBoxSize = Size (Vector2 200.0 20.0)
createProjectDialogOKSize      = Size (Vector2 100.0 20.0)
createProjectDialogCancelSize  = Size (Vector2  80.0 20.0)


errorMessageWrapMargin :: Int
errorMessageWrapMargin = 30

errorMessageWidgetSize, visualizationWidgetSize :: Size
errorMessageWidgetSize  = Size (Vector2 200 200)
visualizationWidgetSize = Size (Vector2 200 200)


gridSize :: Int
gridSize = 16
