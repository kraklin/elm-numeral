module Numeral exposing (fromInt, fromFloat, value, withLocale, format)

import Locale exposing (Locale, Settings)
import List.Extra exposing (greedyGroupsOf)


type alias Settings =
    { localeSettings : Locale.Settings
    , isNegative : Bool
    , absoluteValue : Float
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
format (Numeral { localeSettings, absoluteValue, isNegative }) =
    let
        ( int, decimal ) =
            ( truncate absoluteValue, absoluteValue - toFloat (truncate absoluteValue) )

        decimalString =
            (toString decimal) |> String.dropLeft 2 |> String.left 2

        integralString =
            toString int
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
        if String.isEmpty decimalString then
            sign ++ integralString
        else
            sign ++ integralString ++ (String.fromChar localeSettings.decimalsSeparator) ++ decimalString
