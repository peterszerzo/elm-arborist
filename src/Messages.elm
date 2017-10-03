module Messages exposing (..)

import Utils.Tree exposing (TreeNodePath)


type Msg item
    = NodeMouseDown Bool TreeNodePath Float Float
    | NodeMouseUp Float Float
    | NodeMouseEnter TreeNodePath
    | NodeMouseLeave TreeNodePath
    | CanvasMouseMove Float Float
    | CanvasMouseDown Float Float
    | CanvasMouseUp Float Float
    | CanvasMouseLeave
    | NoOp
