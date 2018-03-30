```
  🌲           🌲                             🌲          
 🌲🌲   🌲🌲 🌲      🌲   🌲  🌲   🌲     🌲  🌲      🌲🌲             🌲    
🌲🌲🌲  🌲🌲🌲 🌲  🌲🌲🌲 🌲🌲  🌲  🌲🌲🌲 🌲🌲🌲   🌲 🌲🌲        🌲🌲      🌲   
```

# elm-arborist

Drag-and-drop interface to edit, dissect and-rearrange tree structures, with nodes holding any data type you wish. Here is a [demo](https://peterszerzo.github.io/elm-arborist/), and here some [docs](http://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest).

## Getting started: a family tree

First things first, we need to specify what kind of data structure our tree's nodes will hold. For this demo, it'll be a tuple of names and ages.

```elm
type alias Node = ( String, Int )

exampleNode = ( "Frank", 54 )
```

We can then use the `Arborist.Tree` module to construct a tree structure:

```elm
import Arborist.Tree as Tree

tree =
  Arborist.node ( "Frank", 54 )
    [ Arborist.node ( "Mark", 36 ) []
    , Arborist.node ( "Sally", 31 )
        [ Arborist.node ( "Robert", 14 )
        ]
    ]
```

The tree is defined recursively, with each node holding an arbitrary number of children. This is similar to the [binary tree](http://elm-lang.org/examples/binary-tree) example on the Elm website.

Anyway, we can now define a model Arborist can work with:

```elm
import Arborist

type alias Model = Arborist.Model Node

init : Model
init = Arborist.init tree
```

Notice how the model needs to know what data structure it holds, hence the type variable reference to the `Node` structure defined above.

The rest is pretty much standard Elm architecture:

```elm
type Msg = ArboristMsg Arborist.Msg

update msg model =
  case msg of
    ArboristMsg arboristMsg ->
      Arborist.update arboristMsg model

view model =
  Arborist.view nodeView [] model
```

The final missing piece is `nodeView`. It specifies how a node should be displayed, and has the following form:

```elm
-- Ignore `Context` for now
view : Context -> Maybe Node -> Html msg
view _ maybeNode =
  -- The node is not always available, because we also need to
  -- specify how placeholders are rendered. 
  case maybeNode of
    Just ( name, age ) ->
      div [] [ span [] [ text name ], span [] [ text (toString age) ] ]    
    Nothing ->
      div [] [ text "Insert node" ] 
```

That's it - your very own tree editor is ready.

## Next steps: a glimpse

### Context

The context object provides, as its name suggests, contextual information to the node when it is rendered, including their parent node and list of siblings (see `Arborist.Context` docs for details). You may for instance want to signal to the user that a child can't be older than their parent in the family tree as they edit it, but traversing the tree to find that information is tedious and inefficient - so Arborist gives you access to it directly.

This should work for a large number of tree editing cases. If you need a broader context, you will need to traverse the tree for it yourself.

### Settings

Replace `init` with `initWith [ Arborist.canvasWidth 800, Arborist.nodeHeight 40, Arborist.gutter 20 ]` to add all kinds of customizations to the editor. See `Arborist.Settings`. 

### Animations

Using the `Arborist.subscriptions`, you can smoothly animate to center a node when it is activated. See example for details.

```
🌲🌲🌲  🌲🌲🌲 🌲  🌲🌲🌲 🌲🌲  🌲  🌲🌲🌲 🌲🌲🌲   🌲 🌲🌲        🌲🌲      🌲   
 🌲🌲   🌲🌲 🌲      🌲   🌲  🌲   🌲     🌲  🌲      🌲🌲             🌲    
  🌲           🌲                             🌲          
```
