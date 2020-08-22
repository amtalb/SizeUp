port module Main exposing (..)

import Html exposing (div, h1, p, form, label, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, style)
import Browser
import Dropdown
import Array
import List.Extra exposing (elemIndex)
import Json.Encode
import Json.Decode exposing (Decoder, field, string, float)
import List.Split exposing (chunksOfRight)



-- MAIN


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias GeoEnt =
  { name : String
  , total : Float
  , land : Float
  , water : Float
  , latitude : Float
  , longitude : Float
  }
  
type ViewOption
  = Name
  | Total
  | Land
  | Water
  | Latitude
  | Longitude

type alias Model = 
  { array : Array.Array GeoEnt 
  , selectedValue : Maybe GeoEnt
  , compareValue : Maybe GeoEnt
  , units: Units
  , splash: Bool
  }

type Units
  = Kilometers
  | Miles
  


-- INIT


init : () -> (Model, Cmd Msg)
init _ = 
  ( Model Array.empty Nothing Nothing Kilometers True
  , Cmd.none
  )



-- UPDATE


type Msg
  = DropdownChanged (Maybe String)
  | ValueSelected
  | UpdateJs
  | Received (Result Json.Decode.Error (Array.Array GeoEnt))
  | ConvertToMiles
  | ConvertToKilometers
  | HideSplash

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DropdownChanged selectedValue ->
      update ValueSelected { model | selectedValue = searchSelectedGeoEnt selectedValue model }

    ValueSelected ->
      update UpdateJs { model | compareValue = searchCompareGeoEnt model }
    
    UpdateJs ->
      (model, sendCoords (getCoords model))
    
    Received result ->
      case result of
        Ok geoEnts ->
          ( { model | array =  geoEnts }
          , Cmd.none)
        Err _ ->
          (model, Cmd.none)
    
    ConvertToMiles ->
      case model.selectedValue of
        Just sv ->
          case model.compareValue of
            Just cv ->
              let
                newSelectedValue = 
                  { sv | total = sqKmToSqMiles model sv.total }
                newCompareValue =
                  { cv | total = sqKmToSqMiles model cv.total }
              in
                ({ model | selectedValue = Maybe.Just newSelectedValue, compareValue = Maybe.Just newCompareValue, units = Miles }
                , Cmd.none)
            Nothing ->
              (model, Cmd.none)
        Nothing ->
          (model, Cmd.none)

    ConvertToKilometers ->
      case model.selectedValue of
        Just sv ->
          case model.compareValue of
            Just cv ->
              let
                newSelectedValue = 
                  { sv | total = sqMilesToSqKm model sv.total }
                newCompareValue =
                  { cv | total =  sqMilesToSqKm model cv.total }
              in
                ({ model | selectedValue = Maybe.Just newSelectedValue, compareValue = Maybe.Just newCompareValue, units = Kilometers }
                , Cmd.none)
            Nothing ->
              (model, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    
    HideSplash ->
      ({ model | splash = False }, Cmd.none)


searchSelectedGeoEnt : Maybe String -> Model -> Maybe GeoEnt
searchSelectedGeoEnt selectedValue model =
  case selectedValue of
    Just val ->
      Array.get 0 (Array.filter (\x -> x.name == val) model.array)
    Nothing ->
      model.selectedValue

searchCompareGeoEnt : Model -> Maybe GeoEnt
searchCompareGeoEnt model = 
  case model.selectedValue of
    Just geoEnt ->
      let
        index = elemIndex geoEnt (Array.toList model.array)
      in
        case index of
          Just idx ->
            let
              before = Array.get (idx - 1) model.array
              current = Array.get idx model.array
              after = Array.get (idx + 1) model.array
            in
              case before of
                Just bf ->
                  case current of
                    Just cu ->
                      case after of
                        Just af ->
                          if (cu.total - af.total) > (bf.total - cu.total) then
                            before
                          else
                            after
                        Nothing ->
                          before
                    Nothing ->
                      model.compareValue
                Nothing ->
                  after
          Nothing ->
            model.compareValue
    Nothing ->
      model.compareValue

getCoords : Model -> Json.Encode.Value
getCoords model =
  case model.selectedValue of
    Just sv ->
      case model.compareValue of
        Just cv ->
          Json.Encode.object
            [ ( "sv", encodeCoords sv )
            , ( "cv", encodeCoords cv )
            ]
        Nothing ->
          Json.Encode.string "error cv"
    Nothing ->
      Json.Encode.string "error sv"

encodeCoords : GeoEnt -> Json.Encode.Value
encodeCoords ge =
  Json.Encode.object
    [ ( "latitude", Json.Encode.float ge.latitude )
    , ( "longitude", Json.Encode.float ge.longitude )
    ]
  
sqKmToSqMiles : Model -> Float -> Float
sqKmToSqMiles model km =
  if model.units == Kilometers then
    km * 0.38610
  else
    km

sqMilesToSqKm : Model -> Float -> Float
sqMilesToSqKm model miles =
  if model.units == Miles then
    miles * 2.589988
  else
    miles



-- PORTS


port receiveJson : (Json.Decode.Value -> msg) -> Sub msg
port sendCoords : Json.Encode.Value -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  receiveJson decodeJson


decodeJson : Json.Decode.Value -> Msg
decodeJson json =
  Received 
  <| Json.Decode.decodeValue geoEntArrayDecoder json


geoEntArrayDecoder : Decoder (Array.Array GeoEnt)
geoEntArrayDecoder =
  Json.Decode.array geoEntDecoder

geoEntDecoder : Json.Decode.Decoder GeoEnt
geoEntDecoder =
  Json.Decode.map6 GeoEnt
    (field "name" string)
    (field "total" float)
    (field "land" float)
    (field "water" float)
    (field "latitude" float)
    (field "longitude" float)



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "SizeUp"
  , body = 
    [ div           
      [ id "splash" 
      , style "visibility" (getSplashVisibility model)
      ]
      [ div [ id "text" ]
        [ h1 [] [ text "SizeUp" ]
        , p [] [ text "Pick a geographical entity from the list and I'll show you which worldwide entity is the most similar in size."]
        , Html.button [ onClick HideSplash ] [ text "Okay" ]
        ]
      ]
    , div [ id "buttons" ] 
      [ Html.button [ onClick ConvertToMiles ] [ text "Miles" ]
      , Html.button [ onClick ConvertToKilometers ] [ text "Kilometers" ]
      ]
    , div [ id "dropdown-container" ] 
      [ div [ id "dropdown" ]
        [ Html.form []
          [ p []
            [ label []
              [ Dropdown.dropdown
                (dropdownOptions model)
                []
                (viewCountry model.selectedValue Name)
              ]
            ]
          ]
        ]
      ]
    , div [ id "earth_div1" ] []
    , div [ id "earth_div2" ] []
    , div [ id "filler" ] []
    , case model.selectedValue of
        Just _ ->
          div [ id "bottom" ]
          [ div [ id "selected" ]
            [ h1 [] [ text 
              <| Maybe.withDefault "Not Selected" 
              <| viewCountry model.selectedValue Name ]
            , p [] [ text "Area: "
                    , text 
                    <| Maybe.withDefault "Not Selected" 
                    <| viewCountry model.selectedValue Total 
                    , text 
                    <| viewUnits model.units
                    ]
            ]
          , div [ id "bottom-filler" ] []
          , div [ id "compare" ]
            [ h1 [] [ text 
              <| Maybe.withDefault "Not Selected" 
              <| viewCountry model.compareValue Name ]
            , p [] [ text "Area: "
                    , text 
                    <| Maybe.withDefault "Not Selected" 
                    <| viewCountry model.compareValue Total 
                    , text 
                    <| viewUnits model.units
                    ]
            ]
          ]
        Nothing ->
          div [] []
    ]
  }

viewCountry : Maybe GeoEnt -> ViewOption -> Maybe String
viewCountry geoEnt option =
  case geoEnt of
    Just ge ->
      case option of
        Name ->
          Maybe.Just ge.name
        Total ->
          Maybe.Just
          <| commafyNumber ge.total
        Land ->
          Maybe.Just 
          <| String.fromInt
          <| truncate ge.land
        Water ->
          Maybe.Just 
          <| String.fromInt
          <| truncate ge.water
        Latitude ->
          Maybe.Just 
          <| String.fromFloat ge.latitude
        Longitude ->
          Maybe.Just 
          <| String.fromFloat ge.longitude
    Nothing ->
      Nothing

commafyNumber : Float -> String
commafyNumber num = 
  String.join ","
  <| List.reverse
  <| List.map (String.join "")
  <| chunksOfRight 3
  <| String.split ""
  <| String.fromInt
  <| truncate num

dropdownOptions : Model -> Dropdown.Options Msg
dropdownOptions model =
    let
      defaultOptions = Dropdown.defaultOptions DropdownChanged
    in
      { defaultOptions
        | items = List.sortBy .value (Array.toList(Array.map convertToItem model.array))
        , emptyItem = Just { value = "0", text = "[Please Select]", enabled = True }
      }

convertToItem : GeoEnt -> Dropdown.Item
convertToItem geoEnt =
  { value = geoEnt.name, text = geoEnt.name, enabled = True }

viewUnits : Units -> String
viewUnits unit =
  if unit == Kilometers then 
    " km\u{0032}"
  else
    " mi\u{0032}"

getSplashVisibility : Model -> String
getSplashVisibility model = 
  if model.splash == True then
    "visible"
  else
    "hidden"