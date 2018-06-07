module PhotoGroove exposing (..)

import Html exposing (div, h1, img, text, Html, button, input, label, h3)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)
import Random

-- type Bool = True | False
-- type Maybe valueType = Just valueType | Nothing
type ThumbnailSize = Small | Medium | Large
--type MsgData = ThumbnailSize | String
type Msg =
    SelectByUrl String
  | SurpriseMe
  | SelectByIndex Int
  | SetSize ThumbnailSize

type alias Photo = {url : String}
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

photoArray : Array Photo
photoArray = Array.fromList initialModel.photos

getPhotoUrl : Int -> Maybe String
getPhotoUrl index =
  case Array.get index photoArray of
    Just photo -> Just photo.url
    Nothing -> Nothing

urlPrefix : String
urlPrefix = "http://elm-in-action.com/"

terno : Bool -> String -> String -> String
terno exp trueCond falseCond = if exp then trueCond else falseCond

randomPhotoIndexPicker : Random.Generator Int
randomPhotoIndexPicker = Random.int 0 (Array.length photoArray - 1)

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
    largeImg model.selectedUrl
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectByUrl url -> ({model | selectedUrl = Just url}, Cmd.none) -- a modified model is returned and goes automatically to view method
      -- ((a -> msg) -> Generator a -> Cmd msg)(Int -> Msg)(Random.Generator Int) => Cmd Msg
    SurpriseMe -> (model, Random.generate SelectByIndex randomPhotoIndexPicker)
    SelectByIndex index ->
      let
        newSelectedPhoto : Maybe Photo
        newSelectedPhoto = Array.get index (Array.fromList model.photos)

        newSelectedUrl : Maybe String
        newSelectedUrl =
          model.photos
          |> Array.fromList
          |> Array.get index
          |> Maybe.map .url
            in ({model | selectedUrl = newSelectedUrl}, Cmd.none)
    SetSize thumbnailSize -> ({model | chosenSize = thumbnailSize}, Cmd.none)

main = Html.program {
    init = (initialModel, Cmd.none),
    view = view,
    update = update,
    subscriptions = (\model -> Sub.none)
  }
