module Drag exposing (State, Msg(..), onMouseDownWithOptions, subscriptions, update, init, delta)

{-| This package allows you to make multiple elements draggable simultaneously.

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
            { stopPropogation = False, preventDefault = False }
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

@docs State, Msg, onMouseDownWithOptions, subscriptions, update, init, delta

-}

import Json.Decode exposing (Decoder)
import Browser.Events as Events
import VirtualDom

type alias Position =
    { x: Int
    , y: Int
    }

{-| -}
type State interactionLocation
    = MouseDown
        { current : Position
        , start : Position
        , location : interactionLocation
        }
    | MouseMoved
        { current : Position
        , start : Position
        , location : interactionLocation
        }
    | NotDragging


{-| -}
init : State interactionLocation
init =
    NotDragging


{-| -}
type Msg interactionLocation
    = Start interactionLocation Position
    | Moved interactionLocation Position
    | End interactionLocation Position
    | Click interactionLocation Position


type alias Delta =
    ( Int, Int )


{-| -}
update : Msg interactionLocation -> State interactionLocation -> State interactionLocation
update msg model =
    case msg of
        Start location xy ->
            MouseDown
                { current = xy
                , start = xy
                , location = location
                }

        Moved location xy ->
            case model of
                NotDragging ->
                    NotDragging

                MouseDown { start } ->
                    MouseMoved
                        { current = xy
                        , start = start
                        , location = location
                        }

                MouseMoved { start } ->
                    MouseMoved
                        { current = xy
                        , start = start
                        , location = location
                        }

        End location xy ->
            NotDragging

        Click location xy ->
            NotDragging


{-| -}
delta : Msg interactionLocation -> State interactionLocation -> ( Int, Int )
delta msg model =
    case msg of
        Start location xy ->
            ( 0, 0 )

        Click _ _ ->
            ( 0, 0 )

        Moved location xy ->
            case model of
                NotDragging ->
                    ( 0, 0 )

                MouseDown { current } ->
                    ( xy.x - current.x, xy.y - current.y )

                MouseMoved { current } ->
                    ( xy.x - current.x, xy.y - current.y )

        End location xy ->
            case model of
                NotDragging ->
                    ( 0, 0 )

                MouseDown { current } ->
                    ( xy.x - current.x, xy.y - current.y )

                MouseMoved { current } ->
                    ( xy.x - current.x, xy.y - current.y )


type alias Options msg =
    { message: msg
    , stopPropagation: Bool
    , preventDefault: Bool
    }

{-| -}
onMouseDownWithOptions : { stopPropagation: Bool, preventDefault: Bool } -> location -> VirtualDom.Attribute (Msg location)
onMouseDownWithOptions options interactionLocation =
    VirtualDom.on "mousedown"
        (VirtualDom.Custom
            ( Json.Decode.map3 Options
                (Json.Decode.map (\p -> Start interactionLocation p) mousePosition)
                (Json.Decode.succeed options.stopPropagation) 
                (Json.Decode.succeed options.preventDefault)
            )
        )

mousePosition : Decoder Position
mousePosition =
    Json.Decode.map2 Position
        (Json.Decode.field "clientX" Json.Decode.int)
        (Json.Decode.field "clientY" Json.Decode.int)


{-| -}
subscriptions : State interactionLocation -> Sub (Msg interactionLocation)
subscriptions lastState =
    case lastState of
        MouseDown { location } ->
            Sub.batch
                [ Events.onMouseMove (Json.Decode.map (\p -> Moved location p) mousePosition)
                , Events.onMouseUp (Json.Decode.map (\p -> Click location p) mousePosition)
                ]

        MouseMoved { location } ->
            Sub.batch
                [ Events.onMouseMove (Json.Decode.map (\p -> Moved location p) mousePosition)
                , Events.onMouseUp (Json.Decode.map (\p -> End location p) mousePosition)
                ]

        NotDragging ->
            Sub.none
