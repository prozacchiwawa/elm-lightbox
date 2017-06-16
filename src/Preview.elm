module Preview exposing (..)

import Base64
import Debounce
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Mouse
import Return
import Task
import Time exposing (Time, second)

import ImageData

type alias PreviewModel =
    { rawLayout : String
    , rawCSS : String
    , useImage : ImageData.ImageDesc
    , imageOpacity : Float
    , imageScale : Float
    , dragging : Maybe Mouse.Position
    , at : Mouse.Position
    , imageAt : (Float,Float)
    , locked : Bool
    , currentHTML : String
    , debounceHTML : Debounce.Debounce ()
    }

type Msg
    = SetLayout String
    | SetCSS String
    | UseImage ImageData.ImageDesc
    | SetImageO Float
    | SetImageS Float
    | MouseDown
    | MouseDownPosition Mouse.Position
    | MouseUp
    | MouseMove Mouse.Position
    | SetLocked Bool
    | SetHTML
    | DeliverHTML
    | DebounceMsg Debounce.Msg

type alias Model a =
    { a
    | preview : PreviewModel
    }

htmlDebounceStrat =
    { strategy = Debounce.later (0.33 * second)
    , transform = DebounceMsg
    }

init : String -> String -> PreviewModel
init rl rc =
    { rawLayout = rl
    , rawCSS = rc
    , useImage = ImageData.emptyImage
    , imageOpacity = 50.0
    , imageScale = 1.0
    , dragging = Nothing
    , at = Mouse.Position 0 0
    , imageAt = (0.0,0.0)
    , locked = False
    , debounceHTML = Debounce.init
    , currentHTML = makeSrc rc rl
    }

newImageAt model =
    case model.dragging of
        Just p ->
            let x = toFloat p.x in
            let y = toFloat p.y in
            let atX = toFloat model.at.x in
            let atY = toFloat model.at.y in
            let (iX, iY) = model.imageAt in
            (iX - (x - atX), iY - (y - atY))
        Nothing ->
            model.imageAt

updatePreview : Msg -> PreviewModel -> (PreviewModel, Cmd Msg)
updatePreview msg model =
    case msg of
        SetLayout l -> updatePreview SetHTML { model | rawLayout = l }
        SetCSS c -> updatePreview SetHTML { model | rawCSS = c }
        SetLocked l -> { model | locked = l } ! []
        UseImage i -> { model | useImage = i } ! []
        SetImageO o -> { model | imageOpacity = o } ! []
        SetImageS s -> { model | imageScale = s / 100.0 } ! []
        MouseDown -> model ! []
        MouseDownPosition p ->
            if model.locked then
                model ! []
            else
                { model | dragging = Just p } ! []
        MouseUp ->
            { model | dragging = Nothing, imageAt = newImageAt model } ! []
        MouseMove at -> { model | at = at } ! []
        SetHTML ->
            Debounce.push htmlDebounceStrat () model.debounceHTML
            |> Return.map (\d -> { model | debounceHTML = d })
        DeliverHTML ->
            { model | currentHTML = makeSrc model.rawCSS model.rawLayout } ! []
        DebounceMsg d ->
            Debounce.update htmlDebounceStrat (Debounce.takeLast (\_ -> deliver ())) d model.debounceHTML
            |> Return.map (\d -> { model | debounceHTML = d })

deliver : () -> Cmd Msg
deliver _ =
    Task.succeed DeliverHTML |> Task.perform identity

update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    updatePreview msg model.preview
    |> Return.map (\p -> { model | preview = p })

makeSrc : String -> String -> String
makeSrc c t =
    let html =
        case String.indices "</head>" t of
            hd :: _ -> (String.slice 0 hd t) ++ "<style>" ++ c ++ "</style>" ++ (String.slice hd (String.length t) t)
            _ -> "<style>" ++ c ++ "<style>" ++ t
    in
    "data:text/html;base64," ++ (Base64.encode html |> Result.withDefault "")

viewPreview : PreviewModel -> Html Msg
viewPreview model =
    let (offx, offy) = newImageAt model in
    let transform =
        "translate(-50%,-50%) scale(" ++ (toString model.imageScale) ++ ") translate(" ++ (toString (offx / model.imageScale)) ++ "px, " ++ (toString (offy / model.imageScale)) ++ "px)"
    in
    let defOptions = HE.defaultOptions in
    let preventDef = { defOptions | preventDefault = not model.locked, stopPropagation = not model.locked } in
    Html.div
        [ HA.style [("display", "flex"), ("flex-direction", "column"), ("height", "100vh"), ("width", "100%")] ]
        [ Html.div
            [ HA.style [("flex-grow", "0"), ("flex-shrink", "0")] ]
            [ Html.text "Image Opacity:"
            , Html.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max "100"
                , HE.onInput (\i -> String.toFloat i |> Result.withDefault 50.0 |> SetImageO)
                ] []
            ]
        , Html.div
            [ HA.style [("flex-grow", "0"), ("flex-shrink", "0")] ]
            [ Html.text "Scale:"
            , Html.input
                [ HA.type_ "range"
                , HA.min "10"
                , HA.max "500"
                , HE.onInput (\i -> String.toFloat i |> Result.withDefault 1.0 |> SetImageS)
                ] []
            ]
        , Html.div
            [ HA.style [("flex-grow", "0"), ("flex-shrink", "0")] ]
            [ Html.button
                [ HE.onClick (SetLocked (not model.locked)) ]
                [ Html.text (if model.locked then "mouse interaction" else "mouse panning") ] ]
        , Html.div
            [ HA.style
                [ ("position", "relative")
                , ("overflow", "hidden")
                , ("width", "100%")
                , ("flex-grow", "1")
                , ("flex-shrink", "1")
                ]
            ]
            [ Html.div
                ([ HA.style
                    [ ("position", "relative")
                    , ("left", "50%")
                    , ("top", "50%")
                    , ("transform", transform)
                    , ("width", (toString model.useImage.size.width) ++ "px")
                    , ("height", (toString model.useImage.size.height) ++ "px")
                    , ("z-index", "2")
                    , ("border", "1px solid black")
                    , ("box-sizing", "border-box")
                    ]
                ] ++
                    (if not model.locked then
                        [HE.onWithOptions "mousedown" preventDef (JD.succeed MouseDown)]
                    else
                        []
                    )
                )
                [ Html.iframe
                    [ HA.src model.currentHTML
                    , HA.style
                        [ ("position", "absolute")
                        , ("width", "100%")
                        , ("height", "100%")
                        , ("z-index", "2")
                        ]
                    ] []
                , Html.img
                    [ HA.style
                        [ ("position", "absolute")
                        , ("width", "100%")
                        , ("height", "100%")
                        , ("z-index", "3")
                        , ("opacity", (toString (model.imageOpacity / 100.0)))
                        ]
                    , HA.src model.useImage.data
                    ] []
                ]
            ]
        ]

view : Model a -> Html Msg
view model =
    viewPreview model.preview
