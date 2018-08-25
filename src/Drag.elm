module Drag exposing (State, Msg(..), onMouseDownWithOptions, subscriptions, update, init, delta)

{-|
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
