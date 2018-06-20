module DemoWebsite.Main exposing (..)

{-| Code for the Arborist website.
-}

import Css exposing (..)
import Css.Foreign as Foreign
import Html exposing (program)
import Html.Styled exposing (Html, div, node, h1, h2, h3, p, a, text, label, input, map, button, toUnstyled)
import Html.Styled.Attributes exposing (class, style, value, type_, href, css)
import Svg.Styled exposing (svg, path)
import Svg.Styled.Attributes exposing (viewBox, d, stroke)
import DemoWebsite.Styles as Styles
import DemoWebsite.Conversation as Conversation


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
    svg [ Svg.Styled.Attributes.viewBox "0 0 1000 1000", Svg.Styled.Attributes.fill "currentColor" ]
        [ path [ d "M520,720l220,-220l220,220l-440,0Z" ] []
        , path [ d "M40,720l220,-220l220,220l-440,0Z" ] []
        , path [ d "M280,480l220,-220l220,220l-440,0Z" ] []
        ]



-- View


view : Model -> Html Msg
view model =
    div [] <|
        [ node "style" [] [ text Styles.raw ]
        , div
            [ css
                [ textAlign center
                , position absolute
                , bottom <| px 20
                , left <| px 20
                , property "z-index" "100"
                , padding <| px 20
                , borderRadius <| px 4
                , backgroundColor <| (hex "fafafa")
                , property "box-shadow" "0 2px 4px rgba(0, 0, 0, 0.1)"
                , Foreign.descendants
                    [ Foreign.a
                        [ display inlineBlock
                        , margin2 (px 0) (px 8)
                        ]
                    ]
                ]
            ]
            [ div
                [ css
                    [ width (px 60)
                    , height (px 60)
                    , margin2 (px -10) auto
                    , color (hex "037C4E")
                    ]
                ]
                [ logo ]
            , h1 [] [ text "elm-arborist" ]
            , p []
                [ a [ href "http://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest" ] [ text "v4.0 Docs" ]
                , a [ href "https://github.com/peterszerzo/elm-arborist" ] [ text "GitHub" ]
                , a [ href "Egg/index.html" ] [ text "* Fun stuff!" ]
                ]
            ]
        , Conversation.view model.conversation |> Html.Styled.map ConversationMsg
        ]


{-| Entry point
-}
main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions =
            (\model ->
                Sub.batch
                    [ Conversation.subscriptions model.conversation |> Sub.map ConversationMsg
                    ]
            )
        }
