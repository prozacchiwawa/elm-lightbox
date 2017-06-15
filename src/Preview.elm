module Preview exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Json.Encode as JE
import Return

import ImageData

type alias PreviewModel =
    { rawLayout : String
    , useImage : ImageData.ImageDesc
    }

type Msg
    = SetLayout String
    | UseImage ImageData.ImageDesc

type alias Model a =
    { a
    | preview : PreviewModel
    }

init : String -> PreviewModel
init rl = { rawLayout = rl, useImage = ImageData.emptyImage }

updatePreview : Msg -> PreviewModel -> (PreviewModel, Cmd Msg)
updatePreview msg model =
    case msg of
        SetLayout l -> { model | rawLayout = l } ! []
        UseImage i -> { model | useImage = i } ! []

update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    updatePreview msg model.preview
    |> Return.map (\p -> { model | preview = p })

viewPreview : PreviewModel -> Html Msg
viewPreview model =
    Html.div
        []
        [ Html.div
            [ HA.style
                [ ("position", "absolute")
                , ("left", "50%")
                , ("top", "50%")
                , ("transform", "translate(-50%,-50%)")
                , ("width", (toString model.useImage.size.width) ++ "px")
                , ("height", (toString model.useImage.size.height) ++ "px")
                , ("z-index", "2")
                ]
            , HA.property "innerHTML" (JE.string model.rawLayout)
            ] []
        , Html.img
            [ HA.style
                [ ("position", "absolute")
                , ("left", "50%")
                , ("top", "50%")
                , ("transform", "translate(-50%,-50%)")
                , ("width", (toString model.useImage.size.width) ++ "px")
                , ("height", (toString model.useImage.size.height) ++ "px")
                , ("opacity", "0.4")
                , ("z-index", "3")
                ]
            , HA.src model.useImage.data
            ] []
        ]

view : Model a -> Html Msg
view model =
    viewPreview model.preview
