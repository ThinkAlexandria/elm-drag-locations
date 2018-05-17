module Drag
    exposing
        ( State
        , Msg(Start, Moved, End, Click)
        , onMouseDownWithOptions
        , subscriptions
        , update
        , init
        , delta
        )
{-| Open sourced this snippet so I could hack on new idea on train laptop. TODO:
fill in readme details.


@docs State, Msg, onMouseDownWithOptions, subscriptions, update, init, delta

-}

import VirtualDom
import Mouse exposing (Position)
import Json.Decode

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


{-| -}
onMouseDownWithOptions : VirtualDom.Options -> location -> VirtualDom.Property (Msg location)
onMouseDownWithOptions options interactionLocation =
    VirtualDom.onWithOptions "mousedown"
        options
        (Json.Decode.map (\p -> Start interactionLocation p) Mouse.position)


{-| -}
subscriptions : State interactionLocation -> Sub (Msg interactionLocation)
subscriptions lastState =
    case lastState of
        MouseDown { location } ->
            Sub.batch
                [ Mouse.moves (\p -> Moved location p)
                , Mouse.ups (\p -> Click location p)
                ]

        MouseMoved { location } ->
            Sub.batch
                [ Mouse.moves (\p -> Moved location p)
                , Mouse.ups (\p -> End location p)
                ]

        NotDragging ->
            Sub.none
