module Messages exposing (..)

import Utils.Tree as Tree exposing (TreeNodePath)
import Time


{-| TODO: move into main Arborist module on upcoming major release.
-}
type Msg
    = AnimationFrameTick Time.Time
    | Tick Time.Time
    | NodeMouseDown Bool TreeNodePath Float Float
    | NodeMouseUp Float Float
    | NodeMouseEnter TreeNodePath
    | NodeMouseLeave TreeNodePath
    | CanvasMouseMove Float Float
    | CanvasMouseDown Float Float
    | CanvasMouseUp Float Float
    | CanvasMouseLeave
    | NoOp
