module Internals.Drag exposing
    ( Drag
    , init
    , start, move
    , state
    )

{-| This module holds the drag state.


# Types

@docs Drag


# Constructors

@docs init


# Update utilities

@docs start, move


# Accessors

@docs state

-}

import Internals.Offset as Offset


{-| This opaque data type contains the current drag state. If there is dragging, it contains the screen position where the drag began, where the cursor currently is, and an identifier of the point being dragged. The client can define any way of identifying points, as indicated by the `identifier` type variable.
-}
type Drag identifier
    = Drag
        (Maybe
            { id : identifier
            , x0 : Float
            , y0 : Float
            , offset : Offset.Offset
            }
        )


{-| Initialize new drag with an empty drag state.
-}
init : Drag identifier
init =
    Drag Nothing


{-| Initialize a new drag with a drag state where the current drag position and the drag start positions are the same.
-}
start : identifier -> Float -> Float -> Drag identifier
start id x0 y0 =
    Drag
        (Just
            { id = id
            , x0 = x0
            , y0 = y0
            , offset = Offset.noOffset
            }
        )


{-| Set a new drag position under the current drag.
-}
move : Offset.GetterSetterConfig -> Float -> Float -> Drag identifier -> Drag identifier
move config newX newY (Drag maybeDrag) =
    case maybeDrag of
        Just drag ->
            Drag
                (Just
                    { drag
                        | offset =
                            Offset.fromPt config
                                ( newX - drag.x0
                                , newY - drag.y0
                                )
                    }
                )

        Nothing ->
            Drag Nothing


{-| Retrieve drag state.
-}
state : Drag identifier -> Maybe ( identifier, Offset.Offset )
state (Drag drag) =
    drag
        |> Maybe.map
            (\d ->
                ( d.id, d.offset )
            )
