module Main exposing (..)

import Html exposing (Html, text, div, program)
import Html.Events exposing (onClick, onMouseUp, onMouseDown, onInput)
import Html.Attributes as HA
import Css exposing (..)
-- import Css.Elements exposing (body)
import Html.CssHelpers
import Css.Namespace exposing (namespace)
import Mouse
-- import Json.Decode as JD
import Json.Encode as JE
import Window

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

type alias Model =
    { windowWidth : Float
    , screenWidth : Float
    , draggingDivider : Maybe Mouse.Position
    , at : Mouse.Position
    , rawLayout : String
    , resolutions : List (Int,Int)
    , viewScale : Float
    , selectedView : SelectedView
    }

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
        ]
    , class SelTab
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

init : Flags -> (Model, Cmd Msg)
init flags =
    { windowWidth = 0.7
    , screenWidth = flags.screenWidth
    , draggingDivider = Nothing
    , at = Mouse.Position 0 0
    , rawLayout = "<div></div>"
    , resolutions = []
    , viewScale = 1.0
    , selectedView = Image
    } ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
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
            { model | rawLayout = l } ! []
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
                [ div [ c.class [WindowTab], onClick (SelectView Image) ] [ Html.text "Image" ]
                , div [ c.class [WindowTab], onClick (SelectView Layout) ] [ Html.text "Layout" ]
                , div [ c.class [WindowTab], onClick (SelectView Preview) ] [ Html.text "Preview" ]
                ]
            , div [ c.class [WindowView] ]
                [ div [ addIfSelected Image [SelTab] [Fill] ] [ Html.text "Image" ]
                , div [ addIfSelected Layout [SelTab] [Fill] ]
                    [ Html.textarea [ c.class [FillText], HA.property "value" (JE.string model.rawLayout), onInput SetLayout ] []
                    ]
                , div [ addIfSelected Preview [SelTab] [Fill], HA.property "innerHTML" (JE.string model.rawLayout) ] []
                ]
            ]
        , div [ c.class [Divider], onMouseDown StartDivDrag ] []
        , div [ c.class [Tree] ] [ Html.text "tree" ]
        , Html.node "style" [] [ Html.text cssdata ]
        ]

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.batch [Mouse.moves MouseMove, Window.resizes WindowSize]
        }
