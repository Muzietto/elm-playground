import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)

initialModel = 
  [ {url = "1.jpeg"}
    , {url = "2.jpeg"}
    , {url = "3.jpeg"}
  ]

urlPrefix = "http://elm-in-action.com/"

viewThumbnail pic = img [src (urlPrefix ++ pic.url)] []

view model = 
  div [class "content"]
    [h1 [] [text "PhotoGroove"]
    , div [id "thumbnails"] (List.map viewThumbnail model)
    ]
    
main = 
  view initialModel
  
