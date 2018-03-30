module ExperimentalEditorApi.Editor exposing (..)

import Time
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Arborist
import Arborist.Settings as Settings
import ExperimentalEditorApi.Node as Node


type alias Data =
    Arborist.Tree Node.Node


type alias State =
    { arborist : Arborist.Model Node.Node
    , newNode : Node.Node
    }


init : Data -> State
init startTree =
    { arborist =
        Arborist.initWith
            [ Settings.nodeWidth 80
            , Settings.nodeHeight 40
            , Settings.throttleMouseMoves (250 * Time.millisecond)
            , Settings.sturdyMode True
            , Settings.dragAndDrop True
            ]
            startTree
    , newNode = Node.placeholder
    }


subscriptions : Config msg -> Sub msg
subscriptions config =
    Arborist.subscriptions config.state.arborist
        |> Sub.map
            (\msg ->
                let
                    state =
                        config.state
                in
                    config.toMsg
                        { state | arborist = Arborist.update msg state.arborist }
                        config.data
            )


type alias Config msg =
    { data : Data
    , state : State
    , toMsg : State -> Data -> msg
    }


view : Config msg -> Html msg
view config =
    div []
        [ Arborist.view
            (\_ _ ->
                div
                    [ style
                        [ ( "width", "80px" )
                        , ( "height", "40px" )
                        , ( "display", "flex" )
                        , ( "align-items", "center" )
                        , ( "justify-content", "center" )
                        , ( "background-color", "#adadad" )
                        ]
                    ]
                    [ Html.text "a" ]
            )
            [ style [ ( "border", "1px solid black" ) ] ]
            config.state.arborist
            |> Html.map
                (\msg ->
                    let
                        state =
                            config.state

                        newArborist =
                            Arborist.update msg state.arborist
                    in
                        config.toMsg { state | arborist = newArborist } config.data
                )
        ]
