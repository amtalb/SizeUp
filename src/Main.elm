port module Main exposing (..)

import Array
import Browser
import Dropdown
import Html exposing (div, form, h1, label, p, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, at, field, float, index, string)
import Json.Encode
import List exposing (sortBy)
import List.Extra exposing (elemIndex, getAt)
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
    , area : Float
    , latitude : Float
    , longitude : Float
    }


type ViewOption
    = Name
    | Area


type alias Model =
    { array : Array.Array GeoEnt
    , selectedValue : Maybe GeoEnt
    , compareValue : Maybe GeoEnt
    , units : Units
    , splash : Bool
    }


type Units
    = Kilometers
    | Miles



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Array.empty Nothing Nothing Kilometers True
    , getJSON
    )



-- UPDATE


type Msg
    = DropdownChanged (Maybe String)
    | ValueSelected
    | UpdateJs
    | GotJSON (Result Http.Error (Array.Array GeoEnt))
    | ConvertToMiles
    | ConvertToKilometers
    | HideSplash


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownChanged selectedValue ->
            update ValueSelected { model | selectedValue = searchSelectedGeoEnt selectedValue model }

        ValueSelected ->
            update UpdateJs { model | compareValue = searchCompareGeoEnt model }

        UpdateJs ->
            ( model, sendCoords (getCoords model) )

        GotJSON result ->
            case result of
                Ok geoEnts ->
                    ( { model | array = geoEnts }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ConvertToMiles ->
            case model.selectedValue of
                Just sv ->
                    case model.compareValue of
                        Just cv ->
                            let
                                newSelectedValue =
                                    { sv | area = sqKmToSqMiles model sv.area }

                                newCompareValue =
                                    { cv | area = sqKmToSqMiles model cv.area }
                            in
                            ( { model | selectedValue = Maybe.Just newSelectedValue, compareValue = Maybe.Just newCompareValue, units = Miles }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ConvertToKilometers ->
            case model.selectedValue of
                Just sv ->
                    case model.compareValue of
                        Just cv ->
                            let
                                newSelectedValue =
                                    { sv | area = sqMilesToSqKm model sv.area }

                                newCompareValue =
                                    { cv | area = sqMilesToSqKm model cv.area }
                            in
                            ( { model | selectedValue = Maybe.Just newSelectedValue, compareValue = Maybe.Just newCompareValue, units = Kilometers }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        HideSplash ->
            ( { model | splash = False }, Cmd.none )


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
        Just selectedGeoEnt ->
            let
                sortedGeoEntList =
                    sortBy .area (Array.toList model.array)

                index =
                    elemIndex selectedGeoEnt (sortBy .area (Array.toList model.array))
            in
            case index of
                Just idx ->
                    let
                        before =
                            getAt (idx - 1) sortedGeoEntList

                        current =
                            getAt idx sortedGeoEntList

                        after =
                            getAt (idx + 1) sortedGeoEntList
                    in
                    case ( before, current, after ) of
                        ( Just before_, Just current_, Just after_ ) ->
                            if (before_.area - current_.area) < (current_.area - after_.area) then
                                before

                            else
                                after

                        _ ->
                            model.compareValue

                _ ->
                    model.compareValue

        _ ->
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
        km * 0.3861

    else
        km


sqMilesToSqKm : Model -> Float -> Float
sqMilesToSqKm model miles =
    if model.units == Miles then
        miles * 2.589988

    else
        miles



-- PORTS


port sendCoords : Json.Encode.Value -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
                , p [] [ text "Pick a geographical entity from the list and I'll show you which worldwide entity is the most similar in size." ]
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
                        [ h1 []
                            [ text <|
                                Maybe.withDefault "Not Selected" <|
                                    viewCountry model.selectedValue Name
                            ]
                        , p []
                            [ text "Area: "
                            , text <|
                                Maybe.withDefault "Not Selected" <|
                                    viewCountry model.selectedValue Area
                            , text <|
                                viewUnits model.units
                            ]
                        ]
                    , div [ id "bottom-filler" ] []
                    , div [ id "compare" ]
                        [ h1 []
                            [ text <|
                                Maybe.withDefault "Not Selected" <|
                                    viewCountry model.compareValue Name
                            ]
                        , p []
                            [ text "Area: "
                            , text <|
                                Maybe.withDefault "Not Selected" <|
                                    viewCountry model.compareValue Area
                            , text <|
                                viewUnits model.units
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

                Area ->
                    Maybe.Just <|
                        commafyNumber ge.area

        Nothing ->
            Nothing


commafyNumber : Float -> String
commafyNumber num =
    String.join "," <|
        List.reverse <|
            List.map (String.join "") <|
                chunksOfRight 3 <|
                    String.split "" <|
                        String.fromInt <|
                            truncate num


dropdownOptions : Model -> Dropdown.Options Msg
dropdownOptions model =
    let
        defaultOptions =
            Dropdown.defaultOptions DropdownChanged
    in
    { defaultOptions
        | items = List.sortBy .value (Array.toList (Array.map convertToItem model.array))
        , emptyItem = Just { value = "0", text = "[Please Select]", enabled = True }
    }


convertToItem : GeoEnt -> Dropdown.Item
convertToItem geoEnt =
    { value = geoEnt.name, text = geoEnt.name, enabled = True }


viewUnits : Units -> String
viewUnits unit =
    if unit == Kilometers then
        " km2"

    else
        " mi2"


getSplashVisibility : Model -> String
getSplashVisibility model =
    if model.splash == True then
        "visible"

    else
        "hidden"



-- HTTP


getJSON : Cmd Msg
getJSON =
    Http.get
        { url = "https://raw.githubusercontent.com/mledoze/countries/master/countries.json"
        , expect = Http.expectJson GotJSON geoEntArrayDecoder
        }


geoEntArrayDecoder : Decoder (Array.Array GeoEnt)
geoEntArrayDecoder =
    Json.Decode.array geoEntDecoder


geoEntDecoder : Json.Decode.Decoder GeoEnt
geoEntDecoder =
    Json.Decode.map4 GeoEnt
        (at [ "name", "common" ] string)
        (field "area" float)
        (field "latlng" (index 0 float))
        (field "latlng" (index 1 float))
