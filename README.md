```
  🌲           🌲                             🌲          
 🌲🌲   🌲🌲 🌲      🌲   🌲  🌲   🌲     🌲  🌲      🌲🌲             🌲    
🌲🌲🌲  🌲🌲🌲 🌲  🌲🌲🌲 🌲🌲  🌲  🌲🌲🌲 🌲🌲🌲   🌲 🌲🌲        🌲🌲      🌲   
```

# elm-arborist

Drag-and-drop interface to edit, dissect and-rearrange tree structures, with nodes holding any data type you wish. Here is a [demo](https://peterszerzo.github.io/elm-arborist/), and here some [docs](http://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest).

## Getting started

**TLDR:** [simple app example](examples/Simple.elm) (run with `cd examples && elm reactor`, then open http://localhost:8000/Simple.elm)

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
arboristSettings : List (Arborist.Setting MyNode)
arboristSettings =
    [ Settings.keyboardNavigation True
    , Settings.defaultNode (MyNode "A" "Q")
    , Settings.nodeWidth 100
    , Settings.level 80
    , Settings.gutter 20
    ]
```

Rendering the editor will look like this:

```elm
view : Model -> Html Msg
view model =
    Arborist.view
        []
        { state = model.arborist
        , tree = model.tree
        , settings = arboristSettings
        -- We get to these two in a second
        , nodeView = nodeView
        , toMsg = Arborist
        }
```

Now let's look at the two bits we left out: `nodeView` and `toMsg`.

### `nodeView`

This function specifies how a node should be displayed within its bounding box, and looks like this:

```elm
-- Don't worry about `context` for now
nodeView : Arborist.NodeView MyNode Msg
nodeView context maybeNode =
    case maybeNode of
        Just node ->
            text node.question

        Nothing ->
            text "+ add node"
```

### `toMsg`

Arborist uses the teach-me-how-to-message pattern, passing an `Arborist.Updater` to your app's `update` function. The updater is a function that works out the new tree and new state based on the previous tree and previous state.

```elm
type Msg
    -- This is the message constructor that goes into the `toMsg` field above
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

> Arborist cannot give new values straight away because mouse events are fired so quickly that they can undo each others' changes in the runtime, hence the complexity in the update logic above.

And that's it - your very own tree editor is ready.

## Going further

### Context

The [context object](https://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest/Arborist#Context) exposed in the `nodeView` function above provides, as its name suggests, contextual information to the node when it is rendered, including their parent node and list of siblings. You may for instance want to signal to the user that a child can't be older than their parent in the family tree as they edit it, but traversing the tree to find that information is tedious and inefficient - so Arborist gives you access to it directly.

This should work for a large number of tree editing cases. If you need a broader context, you will need to traverse the tree yourself.

### UX goodies

In the [settings module](https://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest/Arborist-Settings), you will find setup instructions for advanced features like node clustering and keyboard navigation. For a real-world example, have a look at the [elm-arborist landing page](landing/src/Landing.elm).

## Contributing

Contributions welcome - please feel free to go ahead with issues and PR's, or reach out to me on Elm Slack at `@peterszerzo`.

## License

MIT.


```
🌲🌲🌲  🌲🌲🌲 🌲  🌲🌲🌲 🌲🌲  🌲  🌲🌲🌲 🌲🌲🌲   🌲 🌲🌲        🌲🌲      🌲   
 🌲🌲   🌲🌲 🌲      🌲   🌲  🌲   🌲     🌲  🌲      🌲🌲             🌲    
  🌲           🌲                             🌲          
```
