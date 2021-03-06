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
import Task exposing (Task)
import Window

import ImageData
import Ports

type Msg
    = NoOp
    | ImageRead Ports.ImagePortData
    | ImageSelected
    | ImageLoaded ImageData.ImageDesc
    | SelectImage ImageData.ImageDesc
    | RemoveImage String
    | DoImageSelect String

type alias ImagesModel =
    { images : Dict String ImageData.ImageDesc
    , showing : Maybe (String,String)
    , imagesel : String
    }

type alias Model a =
    { a | images : ImagesModel }

type CssClasses
    = PreviewRow
    | SelPrevRow
    | RemoveImageButton
    | ImageData
    | LoadImg
    | Container
    | LoadButton
    | ListContainer
    | PreviewContainer
    | ImageDataRow
    | Explanation

css : Stylesheet
css = (stylesheet << namespace "images")
    [ class Container
        [ width (pct 100)
        , position relative
        , displayFlex
        , flexDirection column
        ]
    , class PreviewRow
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
    , class RemoveImageButton
        [ position relative
        , displayFlex
        , fontSize (vmin 3)
        , fontWeight (int 900)
        , flexGrow (num 0)
        , flexShrink (num 0)
        , backgroundColor (rgba 192 0 0 0.7)
        , border (px 0)
        , borderRadius (vmin 1)
        , width (vmin 5)
        , height (vmin 5)
        , alignItems center
        , justifyContent center
        , marginRight (vmin 0.5)
        , marginTop (vmin 0.5)
        ]
    , class ImageData
        [ position relative
        , displayFlex
        , flexDirection column
        , flexGrow (num 1)
        , flexShrink (num 1)
        , fontSize (vmin 3)
        ]
    , class ImageDataRow
        [ position relative
        , margin (px 0)
        , padding (px 0)
        ]
    , class LoadImg
        [ zIndex (int -1)
        ]
    , class LoadButton
        [ margin (vmin 1)
        ]
    , class ListContainer
        [ displayFlex
        , flexDirection column
        ]
    , class PreviewContainer
        [ displayFlex
        , flexDirection row
        , width (vmin 10)
        , height (vmin 10)
        , alignItems center
        , justifyContent center
        ]
    , class Explanation
        [ displayFlex
        , flexDirection column
        , width (pct 100)
        , alignItems center
        , justifyContent center
        , fontSize (vmin 3)
        , fontWeight (int 600)
        , padding (vmin 1)
        ]
    ]

c : Html.CssHelpers.Namespace String class id msg
c = Html.CssHelpers.withNamespace "images"
cssdata : String
cssdata = (Css.compile [css]).css

init : ImagesModel
init =
    let ei = ImageData.emptyImage in
    { images = Dict.insert ei.name ei Dict.empty
    , showing = Nothing
    , imagesel = ei.name
    }

selectImage : ImagesModel -> String -> List (Cmd Msg)
selectImage model i =
    model.images
    |> Dict.get i
    |> Maybe.map (\i -> [Task.succeed (SelectImage i) |> Task.perform identity])
    |> Maybe.withDefault []

updateImages : Msg -> ImagesModel -> (ImagesModel, Cmd Msg)
updateImages msg model =
    case msg of
        ImageSelected ->
            model ! [Ports.fileSelected "file-input"]
        ImageRead data ->
            if data.id == "file-input" then
                { model | showing = Just (data.filename, data.contents) } ! []
            else
                model ! []
        ImageLoaded v ->
            { model | images = Dict.insert v.name v model.images, showing = Nothing } ! []
        SelectImage s ->
            { model | imagesel = s.name } ! []
        DoImageSelect s ->
            model ! (selectImage model s)
        RemoveImage s ->
            let newsel =
                if model.imagesel == s then
                    Dict.toList model.images
                    |> List.map (\(n,v) -> n)
                    |> List.head
                    |> Maybe.withDefault "empty.png"
                else
                    model.imagesel
            in
            if s == "empty.png" then
                model ! []
            else
                ({ model
                | images = Dict.remove s model.images
                , imagesel = newsel
                } !
                    (if newsel == model.imagesel then
                        []
                    else
                        selectImage model newsel
                    )
                )
        _ -> model ! []

update : Msg -> { model | images : ImagesModel } -> ({ model | images : ImagesModel }, Cmd Msg)
update msg model =
    updateImages msg model.images
    |> Return.map (\i -> { model | images = i })

decodeImageLoad : String -> String -> JD.Decoder ImageData.ImageDesc
decodeImageLoad name data =
    JD.map3 ImageData.ImageDesc
        (JD.map2 Window.Size
            (JD.at ["target","naturalWidth"] JD.int)
            (JD.at ["target","naturalHeight"] JD.int)
        )
        (JD.succeed name)
        (JD.succeed data)

imagePreview : ImagesModel -> (String,ImageData.ImageDesc) -> Html Msg
imagePreview model (_,ent) =
    let whRatio = (toFloat ent.size.width) / (toFloat ent.size.height) in
    let width = if whRatio >= 1.0 then 10.0 else (whRatio * 10.0) in
    let height = if whRatio >= 1.0 then (10.0 / whRatio) else 10.0 in
    let vmin v = (toString v) ++ "vmin" in
    Html.div
        [ c.class [if model.imagesel == ent.name then SelPrevRow else PreviewRow]
        , HE.onClick (SelectImage ent) ]
        [ Html.div [ c.class [PreviewContainer] ] [ Html.img [HA.src ent.data, HA.style [("width", vmin width), ("height", vmin height)]] [] ]
        , Html.div
            [ c.class [ImageData] ]
            [ Html.p [c.class [ImageDataRow]] [Html.text ent.name]
            , Html.p [c.class [ImageDataRow]] [Html.text ((toString ent.size.width) ++ "x" ++ (toString ent.size.height))]
            ]
        , if ent.name /= "empty.png" then
            Html.button [ c.class [RemoveImageButton], HE.onClick (RemoveImage ent.name) ] [ Html.text "X" ]
          else
            Html.div [] []
        ]


imagePreviews : ImagesModel -> List (Html Msg)
imagePreviews model =
    model.images |> Dict.toList |> List.map (imagePreview model)

viewImages : ImagesModel -> Html Msg
viewImages model =
    case model.showing of
        Just (name,image) ->
            Html.div
                [ c.class [LoadImg] ]
                [ Html.img [ HA.src image, HE.on "load" (decodeImageLoad name image |> JD.map ImageLoaded) ] [] ]
        Nothing ->
            Html.div
                [ c.class [Container] ]
                [ Html.p [ c.class [Explanation] ] [Html.text "Load images to overlay on your html"]
                , Html.input
                    [ c.class [LoadButton], HA.id "file-input", HA.type_ "file", HE.on "change" (JD.succeed ImageSelected) ]
                    []
                , Html.div
                    [ c.class [ListContainer] ]
                    (imagePreviews model)
                ]

view : { model | images : ImagesModel } -> Html Msg
view model =
    viewImages model.images

subs : { model | images : ImagesModel } -> Sub Msg
subs model =
    Ports.fileContentRead ImageRead
