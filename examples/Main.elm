module Main exposing (..)

{-| Code for the Arborist website.
-}

import Task
import Json.Decode as Decode
import Html exposing (Html, div, node, h1, h2, h3, p, a, text, program, label, input, map, button)
import Html.Attributes exposing (class, style, value, type_, href)
import Html.Events exposing (onInput, onClick)
import Svg exposing (svg, path)
import Svg.Attributes exposing (viewBox, d, stroke)
import Arborist
import Arborist.Tree as Tree
import Arborist.Settings as Settings
import Arborist.Context exposing (NodeState(..))
import Styles
import Conversation
import Window exposing (size, resizes)


{-| The Node data type held in each of the tree's nodes.
-}
type alias Node =
    { question : String
    , answer : String
    }


setQuestion : String -> Node -> Node
setQuestion val item =
    { item | question = val }


setAnswer : String -> Node -> Node
setAnswer val item =
    { item | answer = val }


{-| Program model.
-}
type alias Model =
    { conversation : Conversation.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( conversation, conversationCmd ) =
            Conversation.init
    in
        ( { conversation = conversation }
        , Cmd.map ConversationMsg conversationCmd
        )


{-| Program message
-}
type Msg
    = ConversationMsg Conversation.Msg



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConversationMsg msg ->
            let
                ( conversation, conversationCmd ) =
                    Conversation.update msg model.conversation
            in
                ( { model | conversation = conversation }
                , Cmd.map ConversationMsg conversationCmd
                )


logo : Html msg
logo =
    svg [ viewBox "0 0 1000 1000" ]
        [ path [ d "M520,720l220,-220l220,220l-440,0Z" ] []
        , path [ d "M40,720l220,-220l220,220l-440,0Z" ] []
        , path [ d "M280,480l220,-220l220,220l-440,0Z" ] []
        ]



-- View


view : Model -> Html Msg
view model =
    div [] <|
        [ node "style" [] [ text Styles.raw ]
        , div [ class "intro" ]
            [ div [ class "intro__icon" ] [ logo ]
            , h1 [] [ text "elm-arborist" ]
            , p []
                [ a [ href "http://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest" ] [ text "v2.1 Docs" ]
                , a [ href "https://github.com/peterszerzo/elm-arborist" ] [ text "GitHub" ]
                , a [ href "Egg/index.html" ] [ text "* Fun stuff!" ]
                ]
            ]
        , Conversation.view model.conversation |> Html.map ConversationMsg
        ]


{-| Entry point
-}
main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions =
            (\model ->
                Sub.batch
                    [ Conversation.subscriptions model.conversation |> Sub.map ConversationMsg
                    ]
            )
        }
