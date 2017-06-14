module Images exposing (..)

import Css exposing (..)
-- import Css.Elements exposing (body)
import Css.Namespace exposing (namespace)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.CssHelpers
import Html.Events as HE
import Json.Decode as JD
import Return
import Window

import Ports

type alias ImageDesc =
    { size : Window.Size
    , name : String
    , data : String
    }

type Msg
    = NoOp
    | ImageRead Ports.ImagePortData
    | ImageSelected
    | ImageLoaded ImageDesc
    | SelectImage String

type alias ImagesModel =
    { images : Dict String ImageDesc
    , showing : Maybe (String,String)
    , imagesel : String
    }

type alias Model =
    { images : ImagesModel }

type CssClasses
    = PreviewRow
    | SelPrevRow

css : Stylesheet
css = (stylesheet << namespace "images")
    [ class PreviewRow
        [ width (pct 100)
        , height (vmin 10)
        , position relative
        , displayFlex
        , backgroundColor (rgb 255 255 255)
        ]
    , class SelPrevRow
        [ width (pct 100)
        , height (vmin 10)
        , position relative
        , displayFlex
        , backgroundColor (rgb 192 192 192)
        ]
    ]

c : Html.CssHelpers.Namespace String class id msg
c = Html.CssHelpers.withNamespace "images"
cssdata : String
cssdata = (Css.compile [css]).css

init : ImagesModel
init =
    { images = Dict.empty
    , showing = Nothing
    , imagesel = ""
    }

updateImages : Msg -> ImagesModel -> (ImagesModel, Cmd Msg)
updateImages msg model =
    case msg of
        ImageSelected ->
            model ! [Ports.fileSelected "file-input"]
        ImageRead data ->
            { model | showing = Just (data.filename, data.contents) } ! []
        ImageLoaded v ->
            case model.showing of
                Just s -> { model | images = Dict.insert v.name v model.images, showing = Nothing } ! []
                Nothing -> model ! []
        SelectImage s ->
            { model | imagesel = s } ! []
        _ -> model ! []

update : Msg -> { model | images : ImagesModel } -> ({ model | images : ImagesModel }, Cmd Msg)
update msg model =
    updateImages msg model.images
    |> Return.map (\i -> { model | images = i })

decodeImageLoad : String -> String -> JD.Decoder ImageDesc
decodeImageLoad name data =
    JD.map3 ImageDesc
        (JD.map2 Window.Size
            (JD.at ["target","naturalWidth"] JD.int)
            (JD.at ["target","naturalHeight"] JD.int)
        )
        (JD.succeed name)
        (JD.succeed data)

imagePreview : ImagesModel -> (String,ImageDesc) -> Html Msg
imagePreview model (_,ent) =
    let whRatio = (toFloat ent.size.width) / (toFloat ent.size.height) in
    let width = if whRatio >= 1.0 then 10.0 else (whRatio * 10.0) in
    let height = if whRatio >= 1.0 then (10.0 / whRatio) else 10.0 in
    let vmin v = (toString v) ++ "vmin" in
    Html.div
        [ c.class [if model.imagesel == ent.name then SelPrevRow else PreviewRow]
        , HE.onClick (SelectImage ent.name) ]
        [ Html.img [HA.src ent.data, HA.style [("width", vmin width), ("height", vmin height)]] []
        , Html.text ent.name
        ]


imagePreviews : ImagesModel -> List (Html Msg)
imagePreviews model =
    model.images |> Dict.toList |> List.map (imagePreview model)

viewImages : ImagesModel -> Html Msg
viewImages model =
    case model.showing of
        Just (name,image) ->
            Html.div [] [ Html.img [ HA.src image, HE.on "load" (decodeImageLoad name image |> JD.map ImageLoaded) ] [] ]
        Nothing ->
            Html.div
                []
                ([ Html.input
                    [ HA.id "file-input", HA.type_ "file", HE.on "change" (JD.succeed ImageSelected) ]
                    []
                ] ++ (imagePreviews model))

view : { model | images : ImagesModel } -> Html Msg
view model =
    viewImages model.images

subs : { model | images : ImagesModel } -> Sub Msg
subs model =
    Ports.fileContentRead ImageRead
