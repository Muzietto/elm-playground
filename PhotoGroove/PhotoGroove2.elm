module PhotoGroove exposing (..)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

terno : Bool -> String -> String -> String
terno exp trueCond falseCond = if exp then trueCond else falseCond

initialModel : {photos : List {url : String}, selectedUrl : String}
initialModel = {
    photos = [
      {url = "1.jpeg"},
      {url = "2.jpeg"},
      {url = "3.jpeg"}
    ],
    selectedUrl = "1.jpeg"
  }

urlPrefix : String
urlPrefix = "http://elm-in-action.com/"

selectPhotoMsg : String -> {operation : String, data : String}
selectPhotoMsg url = {operation = "SELECT_PHOTO", data = url}  -- the click will return the message object

viewThumbnail selected pic = img [
    src (urlPrefix ++ pic.url), 
    class (terno (pic.url == selected) "selected" ""),
    onClick (selectPhotoMsg pic.url)
  ] []

view model = div [class "content"] [
    h1 [] [text "PhotoGroove"],
    div [id "thumbnails"] (List.map (viewThumbnail model.selectedUrl) model.photos),
    img [class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl)] []
  ]
    
update msg model = 
  if (msg.operation == "SELECT_PHOTO") then 
    { model | selectedUrl = msg.data } -- a modified model is returned and goes automatically to view method
  else
    model

main = Html.beginnerProgram {
    model = initialModel,
    view = view,
    update = update
  }
  
