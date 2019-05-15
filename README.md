```
  🌲           🌲                             🌲          
 🌲🌲   🌲🌲 🌲      🌲   🌲  🌲   🌲     🌲  🌲      🌲🌲             🌲    
🌲🌲🌲  🌲🌲🌲 🌲  🌲🌲🌲 🌲🌲  🌲  🌲🌲🌲 🌲🌲🌲   🌲 🌲🌲        🌲🌲      🌲   
```

# elm-arborist

Drag-and-drop interface to edit, dissect and-rearrange tree structures, with nodes holding any data type you wish. Here is a [demo](https://peterszerzo.github.io/elm-arborist/), and here some [docs](http://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest).

## Getting started

tldr: [simple app example](examples/Simple.elm) (run with `cd examples && elm reactor && open http://localhost:8000/Simple.elm`)

First things first, we need to specify what kind of data structure our tree's nodes will hold. For this demo, it will be a record with a question and an answer.

```elm
type alias MyNode =
    { question : String
    , answer : String
    }

exampleNode = MyNode "How are you?" "Fine thanks"
```

We can then use the `Arborist.Tree` module to recursively construct a tree structure:

```elm
import Arborist.Tree as Tree

tree : Tree.Tree MyNode
tree =
  Tree.Node (MyNode "Q1" "A1")
    [ Tree.Node (MyNode "Q2" "A2") []
    , Tree.Node (MyNode "Q3" "A3")
        [ Tree.Node ( "Q4", "A4" )
        ]
    ]
```

Now, we can now define a model for your app:

```elm
import Arborist
import Arborist.Tree as Tree

type alias Model =
  { tree : Tree.Tree MyNode
  , arborist : Arborist.State
  }
```

Next, we configure the editor:

```elm
```

Rendering the editor will look like this:

```
view : Model -> Html Msg
view model =
    Arborist.view
        []
        { state = model.arborist
        , tree = model.tree
        , settings = arboristSettings
        , nodeView = nodeView
        , toMsg = Arborist
        }
```

The rest is pretty much standard Elm architecture:

```elm
type Msg
    = Arborist (Arborist.Updater MyNode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Arborist updater ->
            let
                ( newState, newTree ) =
                    updater model.arborist model.tree
            in
            ( { model
                | arborist = newState
                , tree = newTree
              }
            , Cmd.none
            )
```





The final missing piece is `nodeView`. It specifies how a node should be displayed, and has the following form:

```elm
nodeView : Arborist.NodeView MyNode Msg
nodeView _ maybeNode =
    case maybeNode of
        Just node ->
            text node.question

        Nothing ->
            text "+ add node"
```

That's it - your very own tree editor is ready.

## Next steps: a glimpse

### Context

The context object provides, as its name suggests, contextual information to the node when it is rendered, including their parent node and list of siblings (see `Arborist.Context` docs for details). You may for instance want to signal to the user that a child can't be older than their parent in the family tree as they edit it, but traversing the tree to find that information is tedious and inefficient - so Arborist gives you access to it directly.

This should work for a large number of tree editing cases. If you need a broader context, you will need to traverse the tree for it yourself.

### Settings

Replace `init` with `initWith [ Arborist.Settings.canvasWidth 800, Arborist.Settings.nodeHeight 40, Arborist.Settings.gutter 20 ]` to add all kinds of customizations to the editor. See `Arborist.Settings`. 

### Animations

Using the `Arborist.subscriptions`, you can listen to keyboard events to help navigating the tree. See example for details.

## Contributing

Contributions welcome - please feel free to go ahead with issues and PR's, or reach out to me on Elm Slack at `@peterszerzo`.

## License

MIT.


```
🌲🌲🌲  🌲🌲🌲 🌲  🌲🌲🌲 🌲🌲  🌲  🌲🌲🌲 🌲🌲🌲   🌲 🌲🌲        🌲🌲      🌲   
 🌲🌲   🌲🌲 🌲      🌲   🌲  🌲   🌲     🌲  🌲      🌲🌲             🌲    
  🌲           🌲                             🌲          
```
