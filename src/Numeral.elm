module Numeral exposing (fromInt, fromFloat, value, withLocale, format)

import Locale exposing (Locale, Settings)
import List.Extra exposing (greedyGroupsOf)


type alias Settings =
    { localeSettings : Locale.Settings
    , isNegative : Bool
    , absoluteValue : Float
    , decimalPlaces : Int
    }


type Numeral
    = Numeral Settings


fromInt : Int -> Numeral
fromInt int =
    fromFloat (toFloat int)


fromFloat : Float -> Numeral
fromFloat float =
    let
        settings =
            { localeSettings = Locale.settingsFromLocale Locale.Default
            , isNegative = float < 0
            , absoluteValue = abs float
            }
    in
        Numeral settings


value : Numeral -> Float
value (Numeral settings) =
    settings.absoluteValue


withLocale : Locale -> Numeral -> Numeral
withLocale locale (Numeral settings) =
    Numeral { settings | localeSettings = Locale.settingsFromLocale locale }


format : Numeral -> String
format (Numeral { localeSettings, absoluteValue, isNegative, decimalPlaces }) =
    let
        splitted =
            String.split "." <| toString absoluteValue

        ( int, decimal ) =
            case splitted of
                [ i, d ] ->
                    ( i, d )

                [ i ] ->
                    ( i, "" )

                _ ->
                    ( "", "" )

        integralString =
            int
                |> String.reverse
                |> String.toList
                |> greedyGroupsOf 3
                |> List.map String.fromList
                |> String.join (String.fromChar localeSettings.thousandsSeparator)
                |> String.reverse

        sign =
            if isNegative then
                "-"
            else
                ""
    in
        if String.isEmpty decimal then
            sign ++ integralString
        else
            sign ++ integralString ++ (String.fromChar localeSettings.decimalsSeparator) ++ decimal
