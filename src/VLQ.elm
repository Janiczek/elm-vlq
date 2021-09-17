module VLQ exposing
    ( encode, encodeSingle, decode
    , minInt, maxInt
    )

{-| VLQ ([variable-length
quantity](http://en.wikipedia.org/wiki/Variable-length_quantity)) is a way to
encode integers via text.

This base64-encoded variant is notably used in [sourcemaps](https://www.youtube.com/watch?v=6LI0BJIiamg).


# Conversion

@docs encode, encodeSingle, decode


# Constants

The minimum and maximum VLQ integer values representable by JavaScript bitwise
operators are -2147483648 (-2^31) and 2147483647 (2^31 - 1) respectively.

@docs minInt, maxInt

-}

import Bitwise
import Dict exposing (Dict)


{-| Maximum integer usable with this library: 2147483647 (2^31 - 1)
-}
maxInt : Int
maxInt =
    2 ^ 31 - 1


{-| Minimum integer usable with this library: -2147483648 (-2^31)
-}
minInt : Int
minInt =
    -2 ^ 31


{-| Encode a list of integers to the VLQ representation.

    VLQ.encode [ 1, 2, 3 ]
    --> "CEG"

    VLQ.encode [ 8, 0, 4, 16 ]
    --> "QAIgB"

    VLQ.encode [ 123456789 ]
    --> "qxmvrH"

-}
encode : List Int -> String
encode ns =
    ns
        |> List.map encodeSingle
        |> String.join ""


{-| Encode a single integer to the VLQ representation.

This will always behave the same as if you wrapped the integer in a list and encoded it with [`encode`](#encode)

    VLQ.encodeSingle 1
    --> "C"

    VLQ.encodeSingle 123456789
    --> "qxmvrH"

-}
encodeSingle : Int -> String
encodeSingle n =
    let
        n_ : Int
        n_ =
            if n < 0 then
                Bitwise.or 1 (Bitwise.shiftLeftBy 1 -n)

            else
                Bitwise.shiftLeftBy 1 n

        go : Int -> List Char -> String
        go num acc =
            let
                clamped : Int
                clamped =
                    Bitwise.and num 31

                newNum : Int
                newNum =
                    Bitwise.shiftRightZfBy 5 num

                newClamped : Int
                newClamped =
                    if newNum > 0 then
                        Bitwise.or 32 clamped

                    else
                        clamped

                newAcc : List Char
                newAcc =
                    unsafeGetChar newClamped
                        :: acc
            in
            if newNum > 0 then
                go newNum newAcc

            else
                newAcc
                    |> List.reverse
                    |> String.fromList
    in
    go n_ []


{-| Decode a VLQ-encoded string back into a list of integers.

    VLQ.decode "C"
    --> Just [ 1 ]

    VLQ.decode "AAAA"
    --> Just [ 0, 0, 0, 0 ]

    VLQ.decode "2HwcqxB"
    --> Just [ 123, 456, 789 ]

    VLQ.decode "Not a VLQ string"
    --> Nothing

-}
decode : String -> Maybe (List Int)
decode string =
    let
        go : Int -> Int -> List Int -> List Char -> Maybe (List Int)
        go shift value acc chars =
            case chars of
                [] ->
                    Just (List.reverse acc)

                char :: restChars ->
                    case Dict.get char charToInt of
                        Nothing ->
                            Nothing

                        Just int ->
                            let
                                hasContinuationBit : Bool
                                hasContinuationBit =
                                    Bitwise.and int 32 > 0

                                int_ : Int
                                int_ =
                                    Bitwise.and int 31

                                value1 : Int
                                value1 =
                                    value + Bitwise.shiftLeftBy shift int_
                            in
                            if hasContinuationBit then
                                go (shift + 5) value1 acc restChars

                            else
                                let
                                    shouldNegate : Bool
                                    shouldNegate =
                                        Bitwise.and value1 1 > 0

                                    value2 : Int
                                    value2 =
                                        Bitwise.shiftRightZfBy 1 value1

                                    addedValue : Int
                                    addedValue =
                                        if shouldNegate then
                                            if value2 == 0 then
                                                -0x80000000

                                            else
                                                -value2

                                        else
                                            value2
                                in
                                go 0 0 (addedValue :: acc) restChars
    in
    go 0 0 [] (String.toList string)



-- HELPERS


charset : List Char
charset =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='"
        |> String.toList


charToInt : Dict Char Int
charToInt =
    charset
        |> List.indexedMap (\i c -> ( c, i ))
        |> Dict.fromList


intToChar : Dict Int Char
intToChar =
    charset
        |> List.indexedMap (\i c -> ( i, c ))
        |> Dict.fromList


unsafeGetChar : Int -> Char
unsafeGetChar n =
    Dict.get n intToChar
        -- We'll be careful to not go outside the bounds. Pinky promise.
        |> Maybe.withDefault ' '
