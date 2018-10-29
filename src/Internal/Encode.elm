module Internal.Encode exposing (address, bigInt, blockHash, blockId, hex, hexInt, listOfMaybesToVal, logFilter, topicsList, txCall, txHash)

import BigInt as BigInt exposing (BigInt)
import Eth.Types exposing (..)
import Eth.Utils exposing (..)
import Hex
import Internal.Utils exposing (..)
import Json.Encode as Encode exposing (Value, int, list, null, object, string)



-- Simple


{-| -}
address : Address -> Value
address =
    addressToString >> string


{-| -}
txHash : TxHash -> Value
txHash =
    txHashToString >> string


{-| -}
blockHash : BlockHash -> Value
blockHash =
    blockHashToString >> string



-- Complex


listOfMaybesToVal : List ( String, Maybe Value ) -> Value
listOfMaybesToVal keyValueList =
    keyValueList
        |> List.filter (\( k, v ) -> v /= Nothing)
        |> List.map (\( k, v ) -> ( k, Maybe.withDefault Encode.null v ))
        |> Encode.object


{-| -}
txCall : Call a -> Value
txCall { to, from, gas, gasPrice, value, data } =
    listOfMaybesToVal
        [ ( "to", Maybe.map address to )
        , ( "from", Maybe.map address from )
        , ( "gas", Maybe.map hexInt gas )
        , ( "gasPrice", Maybe.map bigInt gasPrice )
        , ( "value", Maybe.map bigInt value )
        , ( "data", Maybe.map hex data )
        ]


{-| -}
blockId : BlockId -> Value
blockId blockId_ =
    case blockId_ of
        BlockNum num ->
            Hex.toString num
                |> add0x
                |> string

        EarliestBlock ->
            string "earliest"

        LatestBlock ->
            string "latest"

        PendingBlock ->
            string "pending"


{-| -}
logFilter : LogFilter -> Value
logFilter logFilter_ =
    object
        [ ( "fromBlock", blockId logFilter_.fromBlock )
        , ( "toBlock", blockId logFilter_.toBlock )
        , ( "address", address logFilter_.address )
        , ( "topics", topicsList logFilter_.topics )
        ]


topicsList : List (Maybe Hex) -> Value
topicsList topicsList_ =
    let
        toVal val =
            case val of
                Just hex_ ->
                    string (hexToString hex_)

                Nothing ->
                    null
    in
    List.map toVal topicsList_ |> list identity



-- Rudiments


{-| -}
bigInt : BigInt -> Value
bigInt =
    BigInt.toHexString >> add0x >> Encode.string


{-| -}
hex : Hex -> Value
hex =
    hexToString >> Encode.string


{-| -}
hexInt : Int -> Value
hexInt =
    Hex.toString >> add0x >> Encode.string
