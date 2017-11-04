module Locale exposing (..)


type alias Settings =
    { thousandsSeparator : Char
    , decimalsSeparator : Char
    }


type Locale
    = Default
    | CS


settingsFromLocale : Locale -> Settings
settingsFromLocale locale =
    case locale of
        Default ->
            { thousandsSeparator = ',', decimalsSeparator = '.' }

        CS ->
            { thousandsSeparator = ' ', decimalsSeparator = ',' }
