module Utils exposing (addFloatTuples, compareLists, convertElm018Styles, floatToPxString, onClickStopPropagation, startsWith)

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Decode


floatToPxString : Float -> String
floatToPxString =
    (\a -> (++) a "px") << String.fromInt << floor


addFloatTuples : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
addFloatTuples ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


startsWith : List a -> List a -> Bool
startsWith start list =
    List.take (List.length start) list == start


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation message =
    stopPropagationOn "click" (Decode.succeed ( message, True ))


convertElm018Styles : List ( String, String ) -> List (Attribute msg)
convertElm018Styles =
    List.map (\( property, value ) -> style property value)


compareLists : List comparable -> List comparable -> Order
compareLists l1 l2 =
    case ( l1, l2 ) of
        ( head1 :: tail1, head2 :: tail2 ) ->
            let
                comp =
                    compare head1 head2
            in
            if comp == EQ then
                compareLists tail1 tail2

            else
                comp

        ( _, _ ) ->
            EQ
