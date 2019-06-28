This package allows you to make multiple elements draggable simultaneously.


You are responsible for setting up handlers for the `mousedown` event on each
element you want to be draggable. For each handler you specify the interaction
location using a union type you define. The library handles the subscriptions
to the subsequent mousemoved and mouseup events, giving to you an easy to
pattern match report of movements, end of drag, and clicks (for when there was
no mouse movement).

### How it works
1) You create a single type to represent every possible interaction location.
``` elm
type InteractionLocation
    = GraphNode Int
    | Label Int
```
2) In your view functions, add an attribute to each element you want to make draggable 
```elm
viewGraphNode: Int -> Node -> Html Msg
viewGraphNode key node =
    div
        [ Drag.onMouseDownWithOptions
            { stopPropagation = False, preventDefault = False }
            (GraphNode key)
        -- ...
        ]
        [ text <| "Graph Node " ++ (String.fromInt key) ]
```

3) In your update, handle the start, movement, and end of a drag as well as
clicks on an interaction location.
```elm
    (Drag.Start location currentMousePosition) as dragMsg ->
        { model | dragState = Drag.update dragMsg model.dragState }

    (Drag.Moved location currentMousePosition) as dragMsg ->
        let
            (dx, dy) = Drag.delta dragMsg model.dragState
            updatedModel =
                { model | dragState = Drag.update dragMsg model.dragState }
        in
            case location of
                GraphNode key ->
                    -- Handle the drag of the node ...

                Label key ->
                    -- Handle the drag of a label ...

    (Drag.End location currentMousePosition) as dragMsg ->
        { model | dragState = Drag.update dragMsg model.dragState }

    (Drag.Click location currentMousePosition) as dragMsg ->
        { model | dragState = Drag.update dragMsg model.dragState }
    
```
4) Add this library's subscriptions to your subscriptions.
```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Drag.subscriptions
        --, ... your subscriptions
        ]
```
