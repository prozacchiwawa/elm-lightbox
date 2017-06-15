module Main exposing (..)

import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, text, div, program)
import Html.Events exposing (onClick, onMouseUp, onMouseDown, onInput)
import Html.Attributes as HA
import Css exposing (..)
-- import Css.Elements exposing (body)
import Html.CssHelpers
import Css.Namespace exposing (namespace)
import Mouse
import Json.Decode as JD
import Json.Encode as JE
import Result.Extra as ResultX
import Return
import Task exposing (Task)
import Window

import ImageData
import Images
import LocalStorage
import Preview

type alias Flags =
    { screenWidth : Float }

type SelectedView = Image | Layout | Preview

type Msg
    = NoOp
    | GeneralStopDrag
    | StartDivDrag
    | MouseMove Mouse.Position
    | WindowSize Window.Size
    | SetDivider (Mouse.Position, Mouse.Position)
    | SelectView SelectedView
    | SetLayout String
    | ImagesMsg Images.Msg
    | PreviewMsg Preview.Msg
    | HaveState JD.Value

type alias ModelW slice =
    { slice
    | windowWidth : Float
    , screenWidth : Float
    , draggingDivider : Maybe Mouse.Position
    , at : Mouse.Position
    , rawLayout : String
    , resolutions : List (Int,Int)
    , viewScale : Float
    , selectedView : SelectedView
    }

type alias Model = ModelW (Images.Model (Preview.Model {}))

type CssClasses
    = App
    | Window
    | Tree
    | WindowContainer
    | WindowTabCntr
    | WindowTab
    | WindowView
    | Divider
    | Fill
    | SelView
    | SelTab
    | FillText

css : Stylesheet
css = (stylesheet << namespace "lightbox")
    [ class App
        [ width (vw 100)
        , height (vh 100)
        , position relative
        , displayFlex
        ]
    , class WindowContainer
        [ width (pct 70)
        , height (vh 100)
        , displayFlex
        , flexDirection column
        , flexGrow (num 0)
        , flexShrink (num 0)
        ]
    , class Window
        [ width (pct 100)
        , height (vmin 6)
        , displayFlex
        ]
    , class WindowView
        [ width (pct 100)
        , displayFlex
        , flexGrow (num 1)
        , flexShrink (num 0)
        ]
    , class WindowTabCntr
        [ width (pct 100)
        , height (vmin 6)
        , displayFlex
        , flexDirection row
        , flexGrow (num 0)
        , flexShrink (num 0)
        ]
    , class WindowTab
        [ width (pct 20)
        , height (vmin 5)
        , fontFamily sansSerif
        , fontSize (vmin 5)
        , displayFlex
        , backgroundColor (rgb 192 192 192)
        ]
    , class SelTab
        [ backgroundColor (rgb 255 255 255)
        ]
    , class Tree
        [ width (pct 30)
        , height (vh 100)
        , displayFlex
        , flexGrow (num 1)
        , flexShrink (num 1)
        ]
    , class Divider
        [ width (px 5)
        , height (vh 100)
        , displayFlex
        , borderLeft3 (px 1) solid (rgb 255 255 255)
        , borderRight3 (px 1) solid (rgb 0 0 0)
        , flexGrow (num 0)
        , flexShrink (num 0)
        , backgroundColor (rgb 192 192 192)
        ]
    , class Fill
        [ width (pct 100)
        , height (pct 100)
        , display none
        , position relative
        ]
    , class SelView
        [ display block ]
    , class FillText
        [ width (pct 100)
        , height (pct 100)
        , boxSizing borderBox
        , border2 (px 2) inset
        ]
    ]

c : Html.CssHelpers.Namespace String class id msg
c = Html.CssHelpers.withNamespace "lightbox"
cssdata : String
cssdata = (Css.compile [css]).css

encodeImage : (String,ImageData.ImageDesc) -> JE.Value
encodeImage (_,im) =
    JE.object
        [ ("name", JE.string im.name)
        , ("data", JE.string im.data)
        , ("size", JE.object [("w", JE.int im.size.width), ("h", JE.int im.size.height)])
        ]

encodeState : Model -> JE.Value
encodeState model =
    JE.object
        [ ("rawLayout", JE.string model.rawLayout)
        , ("images", JE.list (List.map encodeImage (Dict.toList model.images.images)))
        ]

decodeSize : JD.Decoder Window.Size
decodeSize =
    JD.map2 Window.Size
        (JD.field "w" JD.int)
        (JD.field "h" JD.int)

decodeImage : JD.Decoder ImageData.ImageDesc
decodeImage =
    JD.map3 ImageData.ImageDesc
        (JD.field "size" decodeSize)
        (JD.field "name" JD.string)
        (JD.field "data" JD.string)

decodeState : JD.Decoder (List Msg)
decodeState =
    let messages images rl =
        [SetLayout rl] ++
            (List.map (\i -> ImagesMsg (Images.ImageLoaded i)) images)
    in
    JD.map2 messages
        (JD.field "images" (JD.list decodeImage))
        (JD.field "rawLayout" JD.string)

save : Model -> (Model, Cmd Msg)
save model =
    model ! [LocalStorage.storeState (encodeState model)]

load : Model -> JD.Value -> (Model, Cmd Msg)
load model v =
    JD.decodeValue decodeState v
    |> Result.map
        (\l -> List.foldl (\m model -> Return.andThen (update m) model) (Return.singleton model) l)
    |> Result.withDefault (model ! [])

init : Flags -> (Model, Cmd Msg)
init flags =
    let rl = "<div style='display: flex; align-items: center; justify-content: center;'>Edit layout tab to change ...</div>" in
    { windowWidth = 0.7
    , screenWidth = flags.screenWidth
    , draggingDivider = Nothing
    , at = Mouse.Position 0 0
    , rawLayout = rl
    , resolutions = []
    , viewScale = 1.0
    , selectedView = Preview
    , images = Images.init
    , preview = Preview.init rl
    } ! [LocalStorage.getState ()]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case log "msg" msg of
        StartDivDrag -> { model | draggingDivider = Just model.at } ! []
        GeneralStopDrag ->
            let dragOp =
                case model.draggingDivider of
                    Just at -> SetDivider (model.at, at)
                    Nothing -> NoOp
            in
            update dragOp { model | draggingDivider = Nothing }
        SetDivider (newAt, oldAt) ->
            let movePx = toFloat (newAt.x - oldAt.x) in
            { model | windowWidth = model.windowWidth + (movePx / model.screenWidth) } ! []
        MouseMove to ->
            { model | at = to } ! []
        WindowSize s ->
            { model | screenWidth = toFloat s.width } ! []
        SelectView v ->
            { model | selectedView = v } ! []
        SetLayout l ->
            Preview.update (Preview.SetLayout l) { model | rawLayout = l }
            |> Return.mapCmd PreviewMsg
            |> Return.andThen save
        ImagesMsg (Images.SelectImage i) ->
            Images.update (Images.SelectImage i) model
            |> Return.mapCmd ImagesMsg
            |> Return.andThen (update (PreviewMsg (Preview.UseImage i)))
        ImagesMsg msg ->
            Images.update msg model
            |> Return.mapCmd ImagesMsg
            |> Return.andThen save
        PreviewMsg msg ->
            Preview.update msg model
            |> Return.mapCmd PreviewMsg
        HaveState s ->
            load model s
        _ -> model ! []

view : Model -> Html Msg
view model =
    let dividerAt =
        case model.draggingDivider of
            Just at ->
                let movePx = toFloat (model.at.x - at.x) in
                model.windowWidth + (movePx / model.screenWidth)
            Nothing -> model.windowWidth
    in
    let pct v = (toString (100.0 * v)) ++ "%" in
    let addIfSelected selected class prev =
        if selected == model.selectedView then
            c.class (prev ++ class)
        else
            c.class prev
    in
    div [ c.class [App], onMouseUp GeneralStopDrag ]
        [ div [ c.class [WindowContainer], HA.style [("width", pct dividerAt)] ]
            [ div [ c.class [WindowTabCntr] ]
                [ div [ addIfSelected Image [SelTab] [WindowTab], onClick (SelectView Image) ] [ Html.text "Image" ]
                , div [ addIfSelected Layout [SelTab] [WindowTab], onClick (SelectView Layout) ] [ Html.text "Layout" ]
                , div [ addIfSelected Preview [SelTab] [WindowTab], onClick (SelectView Preview) ] [ Html.text "Preview" ]
                ]
            , div [ c.class [WindowView] ]
                [ div [ addIfSelected Image [SelView] [Fill] ] [ Images.view model |> Html.map ImagesMsg ]
                , div [ addIfSelected Layout [SelView] [Fill] ]
                    [ Html.textarea [ c.class [FillText], HA.property "value" (JE.string model.rawLayout), onInput SetLayout ] []
                    ]
                , div [ addIfSelected Preview [SelView] [Fill] ]
                    [ Preview.view model |> Html.map PreviewMsg
                    ]
                ]
            ]
        , div [ c.class [Divider], onMouseDown StartDivDrag ] []
        , div [ c.class [Tree] ] [ Html.text "tree" ]
        , Html.node "style" [] [ Html.text cssdata ]
        , Html.node "style" [] [ Html.text Images.cssdata ]
        ]

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \m ->
                Sub.batch
                    [ Mouse.moves MouseMove
                    , Window.resizes WindowSize
                    , Sub.map ImagesMsg (Images.subs m)
                    , LocalStorage.haveState (log "haveState" >> HaveState)
                    ]
        }
