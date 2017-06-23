module Main exposing (..)

import Base64
import Debounce
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, text, div, program)
import Html.Events exposing (onClick, onMouseUp, onMouseDown, onInput, defaultOptions, onWithOptions)
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
import Time exposing (Time, second)
import Window

import ImageData
import Images
import LocalStorage
import Ports
import Preview

type alias Flags =
    { screenWidth : Float }

type SelectedView = File | Image | Layout | CSS

type Msg
    = NoOp
    | GeneralStopDrag
    | StartDivDrag
    | MouseMove Mouse.Position
    | WindowSize Window.Size
    | SetDivider (Mouse.Position, Mouse.Position)
    | SetDividerTo Float
    | SelectView SelectedView
    | SetLayout String
    | SetCSS String
    | ImagesMsg Images.Msg
    | PreviewMsg Preview.Msg
    | HaveState JD.Value
    | InitiateSave
    | DebounceMsg Debounce.Msg
    | SetSaveData String
    | MakeSaveData
    | LoadFile
    | DataLoaded Ports.ImagePortData
    | OutputSaveData

type alias ModelW slice =
    { slice
    | windowWidth : Float
    , screenWidth : Float
    , draggingDivider : Maybe Mouse.Position
    , at : Mouse.Position
    , rawLayout : String
    , rawCSS : String
    , resolutions : List (Int,Int)
    , viewScale : Float
    , selectedView : SelectedView
    , debounce : Debounce.Debounce ()
    , savedata : String
    }

saveDebounceStrat =
    { strategy = Debounce.later (1 * second)
    , transform = DebounceMsg
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
    | FilePart
    | SaveButton
    | LoadButton

css : Stylesheet
css = (stylesheet << namespace "lightbox")
    [ class App
        [ width (vw 100)
        , height (vh 100)
        , position relative
        , displayFlex
        ]
    , class WindowContainer
        [ width (pct 39.5)
        , height (vh 100)
        , displayFlex
        , flexDirection column
        , flexGrow (num 0)
        , flexShrink (num 0)
        , minWidth (vmin 30)
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
        , displayFlex
        , flexDirection row
        , flexGrow (num 0)
        , flexShrink (num 0)
        , boxSizing borderBox
        , backgroundColor (rgb 255 255 255)
        , alignItems center
        , boxShadow5 (px 0) (px 5) (px 5) (px 0) (rgba 50 50 50 0.37)
        , zIndex (int 3)
        , padding (vmin 1)
        , paddingLeft (px 0)
        , flexWrap wrap
        , overflow hidden
        ]
    , class WindowTab
        [ height (vmin 6)
        , marginLeft (vmin 2)
        , fontFamily sansSerif
        , fontSize (vmin 3)
        , displayFlex
        , flexDirection row
        , backgroundColor (rgb 255 255 255)
        , alignItems center
        , justifyContent center
        , cursor pointer
        , paddingLeft (vmin 1)
        , paddingRight (vmin 1)
        , borderRadius (vmin 1)
        , backgroundColor (rgb 221 221 221)
        ]
    , class SelTab
        [ backgroundColor (rgb 192 192 192)
        ]
    , class Tree
        [ width (pct 60)
        , height (vh 100)
        , displayFlex
        , flexGrow (num 1)
        , flexShrink (num 1)
        , backgroundColor (rgb 255 255 255)
        ]
    , class Divider
        [ width (pct 0.5)
        , minWidth (px 4)
        , height (vh 100)
        , displayFlex
        , borderLeft3 (px 1) solid (rgb 255 255 255)
        , borderRight3 (px 1) solid (rgb 0 0 0)
        , flexGrow (num 0)
        , flexShrink (num 0)
        , backgroundColor (rgb 192 192 192)
        ]
    , class Fill
        [ display none
        , width (pct 100)
        , height (pct 100)
        , position relative
        , margin (px 0)
        , boxSizing borderBox
        ]
    , class SelView
        [ display block ]
    , class FillText
        [ width (pct 100)
        , height (vh 90)
        , maxHeight (vh 90)
        , boxSizing borderBox
        , border2 (px 2) inset
        , margin (px 0)
        ]
    , class FilePart
        [ margin (vmin 1)
        , padding (vmin 2)
        , boxShadow5 (px 0) (px 5) (px 5) (px 0) (rgba 50 50 50 0.37)
        ]
    , class SaveButton
        [ width (pct 100)
        , height (vmin 5)
        , margin (vmin 1)
        , marginLeft (px 0)
        , boxSizing borderBox
        , borderRadius (vmin 1)
        , backgroundColor (rgb 192 192 192)
        , boxShadow5 (px 0) (px 5) (px 5) (px 0) (rgba 50 50 50 0.37)
        , border (px 0)
        ]
    , class LoadButton
        [ width (pct 100)
        , height (vmin 5)
        , margin (vmin 1)
        , marginLeft (px 0)
        , boxSizing borderBox
        , borderRadius (vmin 1)
        , backgroundColor (rgb 192 192 192)
        , boxShadow5 (px 0) (px 5) (px 5) (px 0) (rgba 50 50 50 0.37)
        , border (px 0)
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
        , ("rawCSS", JE.string model.rawCSS)
        , ("images", JE.list (List.map encodeImage (Dict.toList model.images.images)))
        , ("divider", JE.float model.windowWidth)
        , ("imagesel", JE.string model.images.imagesel)
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
    let messages images rl rc divi imsel =
        [SetLayout rl, SetCSS rc, SetDividerTo divi] ++
            (List.map (\i -> ImagesMsg (Images.ImageLoaded i)) images) ++
            [ImagesMsg (Images.DoImageSelect imsel)]
    in
    JD.map5 messages
        (JD.field "images" (JD.list decodeImage))
        (JD.field "rawLayout" JD.string)
        (JD.field "rawCSS" JD.string)
        (JD.oneOf [JD.field "divider" JD.float, JD.succeed 0.3])
        (JD.oneOf [JD.field "imagesel" JD.string, JD.succeed "empty.jpg"])

save : Model -> Cmd Msg
save model =
    Cmd.batch
        [ LocalStorage.storeState (encodeState model)
        , Task.succeed MakeSaveData |> Task.perform identity
        ]

doSave : Model -> (Model, Cmd Msg)
doSave model =
    model ! [Task.succeed InitiateSave |> Task.perform identity]


load : Model -> JD.Value -> (Model, Cmd Msg)
load model v =
    JD.decodeValue decodeState v
    |> Result.map
        (\l -> List.foldl (\m model -> Return.andThen (update m) model) (Return.singleton model) l)
    |> Result.withDefault (model ! [])

init : Flags -> (Model, Cmd Msg)
init flags =
    let rl = """<html>
<head></head>
<body>
<h1>What is this?</h1>
<p>It's a lightbox that lets you overlay some images onto an iframe while
editing the contents of the HTML and CSS that's presented.  You can zoom and
pan, as well as changing the opacity of the image overlay.</p>
<h2>It's for building properly-aligned HTML when what you have is a reference image.</h2>
<p>Ever been frustrated about not having a tool that just shows you HTML and
a design image at the same time?  This is made to help you quickly align HTML
with a design so you can have pixel-perfect accuracy, and allows you to load
multiple images so you can check all possible form factors.</p>
<h2>So try loading an image</h2>
<p>In the images, tab, try loading an image and selecting it, then editing some HTML
to match the particular alignment.</p>
<h2>An example</h2>
<p>You can try <a target="_blank" href='https://pastebin.com/raw/Zs6rzzTh'>this</a> pastebin.
Just load it in the File tab.</p>
</body></html>"""
    in
    let rc = """
body {
    font-size: 5vmin;
    font-family: sans-serif;
}
h1 {
    font-size: 8vmin;
    font-family: sans-serif;
}
h2 {
    font-size: 6vmin;
    font-family: sans-serif;
}
"""
    in
    { windowWidth = 0.3
    , screenWidth = flags.screenWidth
    , draggingDivider = Nothing
    , at = Mouse.Position 0 0
    , rawLayout = rl
    , rawCSS = rc
    , resolutions = []
    , viewScale = 1.0
    , selectedView = Layout
    , images = Images.init
    , preview = Preview.init rl rc
    , debounce = Debounce.init
    , savedata = ""
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
            |> Return.andThen (update (PreviewMsg Preview.MouseUp))
        SetDivider (newAt, oldAt) ->
            let movePx = toFloat (newAt.x - oldAt.x) in
            let newDiv = model.windowWidth + (movePx / model.screenWidth) in
            update (SetDividerTo newDiv) model
        SetDividerTo d ->
            { model | windowWidth = d } ! []
            |> Return.andThen doSave
        MouseMove to ->
            { model | at = to } ! []
            |> Return.andThen (update (PreviewMsg (Preview.MouseMove to)))
        WindowSize s ->
            { model | screenWidth = toFloat s.width } ! []
        SelectView v ->
            { model | selectedView = v } ! []
        SetLayout l ->
            Preview.update (Preview.SetLayout l) { model | rawLayout = l }
            |> Return.mapCmd PreviewMsg
            |> Return.andThen doSave
        SetCSS c ->
            Preview.update (Preview.SetCSS c) { model | rawCSS = c }
            |> Return.mapCmd PreviewMsg
            |> Return.andThen doSave
        ImagesMsg (Images.SelectImage i) ->
            Images.update (Images.SelectImage i) model
            |> Return.mapCmd ImagesMsg
            |> Return.andThen (update (PreviewMsg (Preview.UseImage i)))
            |> Return.andThen doSave
        ImagesMsg msg ->
            Images.update msg model
            |> Return.mapCmd ImagesMsg
            |> Return.andThen doSave
        PreviewMsg Preview.MouseDown ->
            update (PreviewMsg (Preview.MouseDownPosition model.at)) model
        PreviewMsg msg ->
            Preview.update msg model
            |> Return.mapCmd PreviewMsg
        HaveState s ->
            load model s
        InitiateSave ->
            Debounce.push saveDebounceStrat () model.debounce
            |> Return.map (\d -> { model | debounce = d })
        DebounceMsg d ->
            Debounce.update saveDebounceStrat (Debounce.takeLast (\_ -> save model)) d model.debounce
            |> Return.map (\d -> { model | debounce = d })
        MakeSaveData ->
            { model | savedata = JE.encode 0 (encodeState model) } ! []
        SetSaveData s ->
            JD.decodeString decodeState s
            |> Result.map (\l -> List.foldl (\msg model -> Return.andThen (update msg) model) (Return.singleton model) l)
            |> Result.withDefault (model ! [])
        LoadFile ->
            model ! [Ports.fileSelected "load-input"]
        DataLoaded f ->
            if f.id == "load-input" then
                let rawBase64 = String.split "," f.contents in
                let base64 =
                    case rawBase64 of
                        [] -> ""
                        hd :: [] -> hd
                        junk :: data :: _ -> data
                in
                base64
                |> Base64.decode
                |> Result.andThen (JD.decodeString JD.value)
                |> Result.map (load model)
                |> Result.withDefault (model ! [])
            else
                model ! []
        OutputSaveData ->
            let savedata = JE.encode 0 (encodeState model) in
            { model | savedata = savedata } ! [Ports.saveFile ("layout.json", savedata)]
        _ -> model ! []

view : Model -> Html Msg
view model =
    let defOptions = defaultOptions in
    let preventDef = { defOptions | preventDefault = True, stopPropagation = True } in
    let clickNoDef msg = Html.Events.onWithOptions "click" preventDef (JD.succeed msg) in
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
                [ div [ addIfSelected File [SelTab] [WindowTab], clickNoDef (SelectView File) ] [ Html.text "File" ]
                , div [ addIfSelected Image [SelTab] [WindowTab], clickNoDef (SelectView Image) ] [ Html.text "Image" ]
                , div [ addIfSelected Layout [SelTab] [WindowTab], clickNoDef (SelectView Layout) ] [ Html.text "Layout" ]
                , div [ addIfSelected CSS [SelTab] [WindowTab], clickNoDef (SelectView CSS) ] [ Html.text "CSS" ]
                ]
            , div [ c.class [WindowView] ]
                [ div [ addIfSelected File [SelView] [Fill] ]
                    [ Html.div
                        [ c.class [FilePart] ]
                        [ Html.text "Load a previously saved session"
                        , Html.input
                            [ c.class [LoadButton], HA.id "load-input", HA.type_ "file", Html.Events.on "change" (JD.succeed LoadFile) ]
                            []
                        ]
                    , Html.div
                        [ c.class [FilePart] ]
                        [ Html.text "Save as json to reload later"
                        , Html.button [ c.class [SaveButton], Html.Events.onClick OutputSaveData ] [ Html.text "Save" ] ]
                    ]
                , div [ addIfSelected Image [SelView] [Fill] ] [ Images.view model |> Html.map ImagesMsg ]
                , div [ addIfSelected Layout [SelView] [Fill] ]
                    [ Html.textarea
                        [ c.class [FillText]
                        , HA.property "value" (JE.string model.rawLayout)
                        , HA.property "__arty__enableCodeMirror" (JE.bool True)
                        , HA.property "__arty__visible" (JE.bool (Layout == model.selectedView))
                        , Html.Events.on "__arty__change" (JD.field "detail" JD.string |> JD.map SetLayout)
                        ] []
                    ]
                , div [ addIfSelected CSS [SelView] [Fill] ]
                    [ Html.textarea
                        [ c.class [FillText]
                        , HA.property "value" (JE.string model.rawCSS)
                        , HA.property "__arty__enableCodeMirror" (JE.bool True)
                        , HA.property "__arty__visible" (JE.bool (CSS == model.selectedView))
                        , Html.Events.on "__arty__change" (JD.field "detail" JD.string |> JD.map SetCSS)
                        ] []
                    ]
                ]
            ]
        , div [ c.class [Divider], onWithOptions "mousedown" preventDef (JD.succeed StartDivDrag) ] []
        , div [ c.class [Tree] ]
            [ div [ c.class [Fill, SelView] ] [ Preview.view model |> Html.map PreviewMsg ] ]
        , Html.node "style" [] [ Html.text cssdata ]
        , Html.node "style" [] [ Html.text Images.cssdata ]
        , Html.node "style" [] [ Html.text Preview.cssdata ]
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
                    , LocalStorage.haveState HaveState
                    , Ports.fileContentRead DataLoaded
                    ]
        }
