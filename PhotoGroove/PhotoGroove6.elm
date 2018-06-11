-- here we start ch5 - JavaScript image filters
port module PhotoGroove exposing (..)

import Html exposing (canvas, node, Attribute, div, h1, img, text, Html, button, input, label, h3)
import Html.Attributes as Attr exposing (id, class, src, name, max, checked, type_, title)
import Html.Events exposing (onClick, on, targetValue)
import Array exposing (Array)
import Random
import Http
import Json.Decode as JD exposing (string, int, list, field, Decoder, at, maybe, map3, map)
import Debug exposing (log)

-- type Bool = True | False
-- type Maybe valueType = Just valueType | Nothing

type ThumbnailSize = Small | Medium | Large
type Msg =
    SelectByUrl String
  | SurpriseMe
  | SelectByIndex Int
  | SetSize ThumbnailSize
--  | LoadPhotos (Result Http.Error String)
  | LoadJsonPhotos (Result Http.Error (List Photo))
  | SetHue Int
  | SetRipple Int
  | SetNoise Int

type alias Photo = {
  url : String,
  size : Int,
  title : Maybe String
}
-- no implementation needed. ELM runtime creates it!
port setFilters : FilterOptions -> Cmd msg
type alias FilterOptions = {
  url : String,
  filters : List {name : String, amount : Int}
}
type alias Model = {
  photos : List Photo,
  selectedUrl : Maybe String,
  loadingError: Maybe String,
  chosenSize : ThumbnailSize,
  hue: Int,
  ripple: Int,
  noise: Int
}
--type alias Msg = {operation : String, data : MsgData}

initialModel : Model
initialModel = {
    photos = [],
    selectedUrl = Nothing,
    loadingError = Nothing,
    chosenSize = Medium,
    hue = 0,
    ripple = 0,
    noise = 0
  }

-- decodeString photoDecoder "{\"url\":\"1.jpeg\",\"size\":1234,\"title\":\"photo title\"}"
photoDecoder : Decoder Photo
photoDecoder = map3
  Photo --(\url size title -> {url = url, size = size, title = title})
  (field "url" string)
  (field "size" int)
  (maybe (field "title" string))

photoArray : Array Photo
photoArray = Array.fromList initialModel.photos

getPhotoUrl : Int -> Maybe String
getPhotoUrl index =
  case Array.get index photoArray of
    Just photo -> Just photo.url
    Nothing -> Nothing

urlPrefix : String
urlPrefix = "http://elm-in-action.com/"

{-
-- type Result errValueType okValueType = Err errValueType | Ok okValueType
-- Http.send : (Result Http.Error okValueType -> msg) -> Request okValueType -> Cmd msg
initialCmd : Cmd Msg
initialCmd =
  Http.send
    -- LoadPhotos : Result Http.Error String
    LoadPhotos
    -- LoadJsonPhotos : String -> Request String
    -- type of next line is Request String
      (Http.getString "http://elm-in-action.com/photos/list")
-}

-- Json.Decode.decodeString : Decoder val -> String -> Result String val
-- Http.get : String -> Decoder value -> Request value
-- photoDecoder : Decoder Photo
-- Json.Decode.list photoDecoder : Decoder (List Photo)
initialCmdWithDecoding : Cmd Msg
initialCmdWithDecoding =
-- Http.send : (Result Http.Error okValueType -> msg) -> Request okValueType -> Cmd msg
  Http.send
    LoadJsonPhotos -- Result Http.Error (List Photo) -> Msg
    (Http.get -- String -> Decoder value -> Request value
      "http://elm-in-action.com/photos/list.json"
      (JD.list photoDecoder))

initialCmdWithDecodingChained : Cmd Msg
initialCmdWithDecodingChained =
  JD.list photoDecoder
    |> Http.get "http://elm-in-action.com/photos/list.json"
    |> Http.send LoadJsonPhotos

terno : Bool -> String -> String -> String
terno exp trueCond falseCond = if exp then trueCond else falseCond

-- paperSlider [...attributes] [...children] = DOM stuff
paperSlider : List(Attribute Msg) -> List(Html Msg) -> Html Msg
paperSlider = node "paper-slider"

loggingDecoder : String -> JD.Decoder msg -> JD.Decoder msg
loggingDecoder category decoder =
--    let _ = Debug.log "message: " message
    JD.value |> JD.andThen (\ev ->
        case JD.decodeValue decoder ev of
            Ok decoded -> -- e.g. decoded == SetHue 1
                let _ = Debug.log category decoded
              --  let _ = Debug.log category (JD.decodeValue targetValue ev)
                in JD.succeed decoded
            Err err ->
                JD.fail <| Debug.log category <| err)

onImmediateValueChange : (Int -> Msg) -> Attribute Msg
onImmediateValueChange intToMsg = -- intToMsg : Int -> Msg
  let
    targetImmediateValue : Decoder Int
    targetImmediateValue = field "target" (field "immediateValue" int)

    msgDecoder : Decoder Msg
    msgDecoder = JD.map intToMsg targetImmediateValue
  in
-- Html.Attributes.on : String -> Json.Decode.Decoder Msg -> Attribute Msg
    on "immediate-value-changed" (loggingDecoder "EVENT_DECODER" msgDecoder)

onImmediateValueChangePipelined : (Int -> Msg) -> Attribute Msg
onImmediateValueChangePipelined intToMsg =
  field "target" (field "immediateValue" int)
  |> JD.map intToMsg
  |> on "immediate-value-changed"

viewSizeChooser : Model -> ThumbnailSize -> Html Msg -- Html Msg is the standard type for UX-enabled markup
viewSizeChooser model thumbnailSize =
  label []
    [
      input [
        type_ "radio",
        name "size",
        checked (model.chosenSize == thumbnailSize),
     -- onClick : msg -> Html.Attribute msg
        onClick (SetSize thumbnailSize) -- the click will return the message object (SetSize ThumbnailSize belongs to Msg)
      ] [],
      text (sizeToString thumbnailSize)
    ]

sizeToString : ThumbnailSize -> String
sizeToString size =
  case size of
    Small -> "small"
    Medium -> "med"
    Large -> "large"

viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selected thumbnail = img [
    src (urlPrefix ++ thumbnail.url),
    title ((Maybe.withDefault "TITTOLO" thumbnail.title) ++ " [" ++ toString thumbnail.size ++ " KB]"),
    class (terno (Just thumbnail.url == selected) "selected" ""),
 -- onClick : msg -> Html.Attribute msg
    onClick (SelectByUrl thumbnail.url)
  ] []

viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name intToMsg magnitude =
  div [class "filter-slider"] [ -- children
    label []  [text name],
    paperSlider [
     -- Html.Attributes.max : String -> Html.Attribute Msg
        Attr.max "11",
     -- onImmediateValueChange : (Int -> Msg) -> Html.Attribute Msg
        onImmediateValueChange intToMsg
      ] [],
    label [] [text (toString magnitude)]
  ]

largeImg : Maybe String -> Html Msg
largeImg maybeUrl =
  case maybeUrl of
    -- Just url -> img [class "large", src (urlPrefix ++ "large/" ++ url)] []
    Just url -> canvas [ id "main-canvas", class "large" ] []
    Nothing -> text ""

view : Model -> Html Msg
view model = div [class "content"] [
    h1 [] [text "PhotoGroove"],
    button [onClick SurpriseMe] [text "Surprise me!"],
    div [class "filters"] [
      viewFilter "Hue" SetHue model.hue,
      viewFilter "Ripple" SetRipple model.ripple,
      viewFilter "Noise" SetNoise model.noise
    ],
    h3 [] [text "Thumbnail Size"],
    div [id "choose-size"] (List.map (viewSizeChooser model) [Small, Medium, Large]),
    div [
        id "thumbnails",
        class (sizeToString model.chosenSize)
      ] (List.map (viewThumbnail model.selectedUrl) model.photos),
    largeImg model.selectedUrl,
    text (Maybe.withDefault "" model.loadingError)
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- Cmd.none : Cmd msg <-- lowercase m
    SelectByUrl url ->
      let
        filters = [
          {name = "Hue", amount = model.hue},
          {name = "Ripple", amount = model.ripple},
          {name = "Noise", amount = model.noise}
        ]
        completeUrl = urlPrefix ++ "large/" ++ url
        cmd = setFilters {url = completeUrl, filters = filters}
      in ({model | selectedUrl = Just url}, cmd) -- a modified model is returned and goes automatically to view method
      -- ((a -> msg) -> Generator a -> Cmd msg)(Int -> Msg)(Random.Generator Int) => Cmd Msg
    SurpriseMe ->
      let
        randomPhotoIndexPicker : Random.Generator Int
        randomPhotoIndexPicker = Random.int 0 (List.length model.photos - 1)
      in (model, Random.generate SelectByIndex randomPhotoIndexPicker)
    SelectByIndex index ->
      let
        newSelectedPhoto : Maybe Photo
--        newSelectedPhoto = Array.get index (Array.fromList model.photos)
        newSelectedPhoto =
          model.photos
            |> Array.fromList
            |> Array.get index

        newSelectedUrl : Maybe String
--        newSelectedUrl = Maybe.map (\photo -> photo.url) newSelectedPhoto
        newSelectedUrl =
          newSelectedPhoto
          |> Maybe.map .url

      in ({model | selectedUrl = newSelectedUrl}, Cmd.none)
    SetSize thumbnailSize -> ({model | chosenSize = thumbnailSize}, Cmd.none)
    LoadJsonPhotos (Ok photos) ->
      let _ = Debug.log "Ok: " photos
      in ({model | photos = photos, selectedUrl = Maybe.map .url (List.head photos)}, Cmd.none)
    LoadJsonPhotos (Err e) ->
      let _ = Debug.log "Err: " e
      in ({model | loadingError = Just "ERROR!!!"}, Cmd.none)
    SetHue newHue -> ({model | hue = newHue}, Cmd.none)
    SetRipple newRipple -> ({model | ripple = newRipple}, Cmd.none)
    SetNoise newNoise -> ({model | noise = newNoise}, Cmd.none)

main : Program Never Model Msg
main = Html.program {
    init = (initialModel, initialCmdWithDecodingChained),
    view = view,
    update = update,
    subscriptions = (\_ -> Sub.none)
  }
