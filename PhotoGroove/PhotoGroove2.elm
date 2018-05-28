module PhotoGroove exposing (..)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

terno exp trueCond falseCond = if exp then trueCond else falseCond

initialModel = 
  { photos =
    [ {url = "1.jpeg"}
      , {url = "2.jpeg"}
      , {url = "3.jpeg"}
    ],
    selectedUrl = "1.jpeg"
  }

urlPrefix = "http://elm-in-action.com/"

viewThumbnail selected pic = img 
  [
    src (urlPrefix ++ pic.url), 
    class (terno (pic.url == selected) "selected" ""),
    onClick {operation = "SELECT_PHOTO", data = pic.url}
  ] []

view model = 
  div [class "content"]
    [
      h1 [] [text "PhotoGroove"],
      div [id "thumbnails"] (List.map (viewThumbnail model.selectedUrl) model.photos),
      img [class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl)] []
    ]
    
update msg model = 
  if (msg.operation == "SELECT_PHOTO") then 
    { model | selectedUrl = msg.data }
  else
    model

main = 
  Html.beginnerProgram
    {
      model = initialModel,
      view = view,
      update = update
    }
  
