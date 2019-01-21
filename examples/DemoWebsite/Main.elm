module DemoWebsite.Main exposing
    ( Model
    , Msg(..)
    , init
    , logo
    , main
    , update
    , view
    )

{-| Code for the Arborist website.
-}

import Browser
import Css exposing (..)
import Css.Global as Global
import DemoWebsite.Conversation as Conversation
import DemoWebsite.Styles as Styles
import Html.Styled exposing (Html, a, button, div, h1, h2, h3, input, label, map, node, p, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, href, style, type_, value)
import Json.Encode as Encode
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes exposing (d, stroke, viewBox)


type alias Flags =
    Encode.Value


{-| Program model.
-}
type alias Model =
    { conversation : Conversation.Model
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( conversation, conversationCmd ) =
            Conversation.init Encode.null
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
        ConversationMsg conversationMsg ->
            let
                ( conversation, conversationCmd ) =
                    Conversation.update conversationMsg model.conversation
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
                , backgroundColor <| hex "fafafa"
                , property "box-shadow" "0 2px 4px rgba(0, 0, 0, 0.1)"
                , Global.descendants
                    [ Global.a
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
                [ a [ href "http://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest" ] [ text "v6.0.3" ]
                , a [ href "https://github.com/peterszerzo/elm-arborist" ] [ text "GitHub" ]
                ]
            ]
        , Conversation.view model.conversation |> Html.Styled.map ConversationMsg
        ]


{-| Entry point
-}
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions =
            \model ->
                Sub.batch
                    [ Conversation.subscriptions model.conversation |> Sub.map ConversationMsg
                    ]
        }
