module ExperimentalEditorApi.Main exposing (..)

import Html exposing (Html, div, h1, program)
import Html.Lazy exposing (lazy)
import ExperimentalEditorApi.Editor as Editor
import ExperimentalEditorApi.Node as Node
import Arborist
import Arborist.Tree as Tree


type alias Model =
    { editorData : Editor.Data
    , editorState : Editor.State
    }


type Msg
    = EditorMsg Editor.State Editor.Data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorState editorData ->
            ( { model
                | editorData = editorData
                , editorState = editorState
              }
            , Cmd.none
            )


init : ( Model, Cmd Msg )
init =
    ( { editorData = Tree.Node Node.placeholder []
      , editorState = Editor.init Tree.Empty
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Editor.view
        { state = model.editorState
        , data = model.editorData
        , toMsg = EditorMsg
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Editor.subscriptions
        { state = model.editorState
        , data = model.editorData
        , toMsg = EditorMsg
        }


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
