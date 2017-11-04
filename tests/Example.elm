module Example exposing (..)

import Numeral exposing (..)
import Locale exposing (Locale)
import Test exposing (..)
import Expect


creatingTests : Test
creatingTests =
    describe "Creation"
        [ test "value from int" <|
            (\_ ->
                Numeral.fromInt 1000
                    |> Numeral.value
                    |> Expect.equal 1000
            )
        , test "value from float" <|
            (\_ ->
                Numeral.fromFloat 1000.23
                    |> Numeral.value
                    |> Expect.equal 1000.23
            )
        ]


formattingTests : Test
formattingTests =
    describe "Formatting"
        [ test "basic float format" <|
            (\_ ->
                Numeral.fromFloat 1000.23
                    |> Numeral.format
                    |> Expect.equal "1,000.23"
            )
        , test "basic float with no decimal part" <|
            (\_ ->
                Numeral.fromFloat 1000
                    |> Numeral.format
                    |> Expect.equal "1,000"
            )
        , test "basic float with long decimal part" <|
            (\_ ->
                Numeral.fromFloat 1.1234567
                    |> Numeral.format
                    |> Expect.equal "1.1234567"
            )
        , test "float less than 1000" <|
            (\_ ->
                Numeral.fromFloat 123.23
                    |> Numeral.format
                    |> Expect.equal "123.23"
            )
        , test "float greater than 1000" <|
            (\_ ->
                Numeral.fromFloat 12345678.23
                    |> Numeral.format
                    |> Expect.equal "12,345,678.23"
            )
        , test "int number" <|
            (\_ ->
                Numeral.fromInt 12345678
                    |> Numeral.format
                    |> Expect.equal "12,345,678"
            )
        , test "negative int number" <|
            (\_ ->
                Numeral.fromInt -12
                    |> Numeral.format
                    |> Expect.equal "-12"
            )
        , test "negative large int number" <|
            (\_ ->
                Numeral.fromInt -123456
                    |> Numeral.format
                    |> Expect.equal "-123,456"
            )
        ]


localeTests : Test
localeTests =
    describe "Locale"
        [ test "basic locale format" <|
            (\_ ->
                Numeral.fromFloat 1234567.23
                    |> Numeral.withLocale Locale.CS
                    |> Numeral.format
                    |> Expect.equal "1 234 567,23"
            )
        ]
