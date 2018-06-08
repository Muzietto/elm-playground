module PhotoGroove exposing (..)

import Html exposing (div, h1, img, text, Html, button, input, label, h3)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)
import Random
import Http
import Json.Decode exposing (..)
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

type alias Photo = {
  url : String,
  size : Int,
  title : Maybe String
}
type alias Model = {
  photos : List Photo,
  selectedUrl : Maybe String,
  loadingError: Maybe String,
  chosenSize : ThumbnailSize
}
--type alias Msg = {operation : String, data : MsgData}

initialModel : Model
initialModel = {
    photos = [],
    selectedUrl = Nothing,
    loadingError = Nothing,
    chosenSize = Medium
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
      (Json.Decode.list photoDecoder))

initialCmdWithDecoding2 : Cmd Msg
initialCmdWithDecoding2 =
  Json.Decode.list photoDecoder
    |> Http.get "http://elm-in-action.com/photos/list.json"
    |> Http.send LoadJsonPhotos

terno : Bool -> String -> String -> String
terno exp trueCond falseCond = if exp then trueCond else falseCond

viewSizeChooser : Model -> ThumbnailSize -> Html Msg -- Html Msg is the standard type for UX-enabled markup
viewSizeChooser model thumbnailSize =
  label []
    [
      input [
        type_ "radio",
        name "size",
        checked (model.chosenSize == thumbnailSize),
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
    onClick (SelectByUrl thumbnail.url)
  ] []

largeImg : Maybe String -> Html Msg
largeImg maybeUrl =
  case maybeUrl of
    Just url -> img [class "large", src (urlPrefix ++ "large/" ++ url)] []
    Nothing -> text ""

view : Model -> Html Msg
view model = div [class "content"] [
    h1 [] [text "PhotoGroove"],
    button [onClick SurpriseMe] [text "Surprise me!"],
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
    SelectByUrl url -> ({model | selectedUrl = Just url}, Cmd.none) -- a modified model is returned and goes automatically to view method
      -- ((a -> msg) -> Generator a -> Cmd msg)(Int -> Msg)(Random.Generator Int) => Cmd Msg
    SurpriseMe ->
      let
        randomPhotoIndexPicker : Random.Generator Int
        randomPhotoIndexPicker = Random.int 0 (List.length model.photos - 1)
      in (model, Random.generate SelectByIndex randomPhotoIndexPicker)
    SelectByIndex index ->
      let
        newSelectedPhoto : Maybe Photo
        newSelectedPhoto = Array.get index (Array.fromList model.photos)

        newSelectedUrl : Maybe String
        newSelectedUrl = Maybe.map (\photo -> photo.url) newSelectedPhoto

      in ({model | selectedUrl = newSelectedUrl}, Cmd.none)
    SetSize thumbnailSize -> ({model | chosenSize = thumbnailSize}, Cmd.none)
    LoadJsonPhotos (Ok photos) ->
      let _ = Debug.log "foo" photos
      in
      ({model | photos = photos, selectedUrl = Maybe.map .url (List.head photos)}, Cmd.none)
    LoadJsonPhotos (Err gigio) ->
      let _ = Debug.log "foo" gigio
      in ({model | loadingError = Just "ERROR!!!"}, Cmd.none)

main : Program Never Model Msg
main = Html.program {
    init = (initialModel, initialCmdWithDecoding),
    view = view,
    update = update,
    subscriptions = (\_ -> Sub.none)
  }
