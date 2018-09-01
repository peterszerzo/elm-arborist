module Internal.Settings exposing (Setting(..), Settings, ShowPlaceholderLeavesAdvanced, apply, defaults, showPlaceholderLeavesAdvanced)


type alias ShowPlaceholderLeavesAdvanced node =
    { node : node
    , parent : Maybe node
    , children : List node
    , siblings : List node
    }
    -> Bool


type alias Settings node =
    { canvasWidth : Float
    , canvasHeight : Float
    , nodeWidth : Float
    , nodeHeight : Float
    , level : Float
    , gutter : Float
    , centerOffset : ( Float, Float )
    , connectorStroke : String
    , connectorStrokeWidth : String
    , isDragAndDropEnabled : Bool
    , showPlaceholderLeaves : Bool
    , showPlaceholderLeavesAdvanced : Maybe (ShowPlaceholderLeavesAdvanced node)
    , isSturdyMode : Bool
    , defaultNode : Maybe node
    }


defaults : Settings node
defaults =
    { nodeWidth = 120
    , nodeHeight = 36
    , canvasWidth = 600
    , canvasHeight = 480
    , level = 80
    , gutter = 20
    , centerOffset = ( 0, 0 )
    , connectorStroke = "#E2E2E2"
    , connectorStrokeWidth = "2"
    , isDragAndDropEnabled = True
    , showPlaceholderLeaves = True
    , showPlaceholderLeavesAdvanced = Nothing
    , isSturdyMode = False
    , defaultNode = Nothing
    }


type Setting node
    = NodeWidth Int
    | NodeHeight Int
    | CanvasWidth Int
    | CanvasHeight Int
    | Level Int
    | Gutter Int
    | CenterOffset Int Int
    | ConnectorStroke String
    | ConnectorStrokeWidth String
    | DragAndDrop Bool
    | ShowPlaceholderLeaves Bool
    | ShowPlaceholderLeavesAdvanced (ShowPlaceholderLeavesAdvanced node)
    | SturdyMode Bool
    | DefaultNode node


showPlaceholderLeavesAdvanced : Settings node -> ShowPlaceholderLeavesAdvanced node
showPlaceholderLeavesAdvanced settings =
    settings.showPlaceholderLeavesAdvanced
        |> Maybe.withDefault (\_ -> settings.showPlaceholderLeaves)


apply : List (Setting node) -> Settings node -> Settings node
apply newSettings settings =
    case newSettings of
        [] ->
            settings

        head :: tail ->
            apply tail
                (case head of
                    NodeWidth w ->
                        { settings | nodeWidth = toFloat w }

                    NodeHeight h ->
                        { settings | nodeHeight = toFloat h }

                    CanvasWidth w ->
                        { settings | canvasWidth = toFloat w }

                    CanvasHeight h ->
                        { settings | canvasHeight = toFloat h }

                    Level l ->
                        { settings | level = toFloat l }

                    Gutter g ->
                        { settings | gutter = toFloat g }

                    CenterOffset x y ->
                        { settings | centerOffset = ( toFloat x, toFloat y ) }

                    ConnectorStroke stroke ->
                        { settings | connectorStroke = stroke }

                    ConnectorStrokeWidth strokeWidth ->
                        { settings | connectorStrokeWidth = strokeWidth }

                    DragAndDrop isEnabled ->
                        { settings | isDragAndDropEnabled = isEnabled }

                    ShowPlaceholderLeaves show ->
                        { settings | showPlaceholderLeaves = show }

                    ShowPlaceholderLeavesAdvanced show ->
                        { settings | showPlaceholderLeavesAdvanced = Just show }

                    SturdyMode isSturdyMode ->
                        { settings | isSturdyMode = isSturdyMode }

                    DefaultNode node ->
                        { settings | defaultNode = Just node }
                )
