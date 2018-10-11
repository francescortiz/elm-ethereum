module Internal.Decode exposing (address, bigInt, block, blockHash, blockHead, event, hex, hexBool, hexInt, hexTime, log, nonZero, resultToDecoder, stringInt, syncStatus, tx, txHash, txReceipt, uncle)

import Eth.Types exposing (..)
import Eth.Utils exposing (toAddress, toBlockHash, toHex, toTxHash)
import Hex
import Internal.Utils exposing (add0x, remove0x)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Legacy.BigInt as BigInt exposing (BigInt)
import Time exposing (Posix, millisToPosix)


{-| -}
block : Decoder a -> Decoder (Block a)
block txsDecoder =
    succeed Block
        |> required "number" hexInt
        |> required "hash" blockHash
        |> required "parentHash" blockHash
        |> optional "nonce" (maybe string) Nothing
        |> required "sha3Uncles" string
        |> required "logsBloom" string
        |> required "transactionsRoot" string
        |> required "stateRoot" string
        |> required "receiptsRoot" string
        |> required "miner" address
        |> required "difficulty" bigInt
        |> optional "totalDifficulty" bigInt (BigInt.fromInt 0)
        -- Noticed nodes will occasionally return null values in block responses. Have only tested this on Infura metamask-mainnet endpoint
        |> required "extraData" string
        |> required "size" hexInt
        |> required "gasLimit" hexInt
        |> required "gasUsed" hexInt
        |> optional "timestamp" hexTime (millisToPosix 0)
        -- See comment above
        |> optional "transactions" (list txsDecoder) []
        |> optional "uncles" (list string) []


{-| -}
uncle : Decoder (Block ())
uncle =
    block (succeed ())


{-| -}
blockHead : Decoder BlockHead
blockHead =
    succeed BlockHead
        |> required "number" hexInt
        |> required "hash" blockHash
        |> required "parentHash" blockHash
        |> required "nonce" string
        |> required "sha3Uncles" string
        |> required "logsBloom" string
        |> required "transactionsRoot" string
        |> required "stateRoot" string
        |> required "receiptsRoot" string
        |> required "miner" address
        |> required "difficulty" bigInt
        |> required "extraData" string
        |> required "gasLimit" hexInt
        |> required "gasUsed" hexInt
        |> required "mixHash" string
        |> required "timestamp" hexTime


{-| -}
tx : Decoder Tx
tx =
    succeed Tx
        |> required "hash" txHash
        |> required "nonce" hexInt
        |> required "blockHash" (nonZero blockHash)
        |> required "blockNumber" (nullable hexInt)
        |> required "transactionIndex" hexInt
        |> required "from" address
        |> required "to" (nullable address)
        |> required "value" bigInt
        |> required "gasPrice" bigInt
        |> required "gas" hexInt
        |> required "input" string


{-| -}
txReceipt : Decoder TxReceipt
txReceipt =
    succeed TxReceipt
        |> required "transactionHash" txHash
        |> required "transactionIndex" hexInt
        |> required "blockHash" blockHash
        |> required "blockNumber" hexInt
        |> required "gasUsed" bigInt
        |> required "cumulativeGasUsed" bigInt
        |> custom (maybe (field "contractAddress" address))
        |> required "logs" (list log)
        |> required "logsBloom" string
        |> custom (maybe (field "root" string))
        |> custom (maybe (field "status" hexBool))


{-| -}
log : Decoder Log
log =
    succeed Log
        |> required "address" address
        |> required "data" string
        |> required "topics" (list hex)
        |> optional "removed" bool False
        |> required "logIndex" hexInt
        |> required "transactionIndex" hexInt
        |> required "transactionHash" txHash
        |> required "blockHash" blockHash
        |> required "blockNumber" hexInt


{-| -}
event : Decoder a -> Decoder (Event a)
event returnDataDecoder =
    succeed Event
        |> required "address" address
        |> required "data" string
        |> required "topics" (list hex)
        |> optional "removed" bool False
        |> required "logIndex" hexInt
        |> required "transactionIndex" hexInt
        |> required "transactionHash" txHash
        |> required "blockHash" blockHash
        |> required "blockNumber" hexInt
        |> custom returnDataDecoder


{-| -}
syncStatus : Decoder (Maybe SyncStatus)
syncStatus =
    succeed SyncStatus
        |> required "startingBlock" int
        |> required "currentBlock" int
        |> required "highestBlock" int
        |> required "knownStates" int
        |> required "pulledStates" int
        |> maybe



-- Primitives


{-| -}
address : Decoder Address
address =
    resultToDecoder toAddress


{-| -}
txHash : Decoder TxHash
txHash =
    resultToDecoder toTxHash


{-| -}
blockHash : Decoder BlockHash
blockHash =
    resultToDecoder toBlockHash


{-| -}
hex : Decoder Hex
hex =
    resultToDecoder toHex


{-| -}
stringInt : Decoder Int
stringInt =
    maybeToDecoder String.toInt


{-| -}
hexInt : Decoder Int
hexInt =
    resultToDecoder (remove0x >> Hex.fromString)


{-| -}
bigInt : Decoder BigInt
bigInt =
    resultToDecoder (add0x >> BigInt.fromString >> Result.fromMaybe "Error decoding hex to BigInt")


{-| -}
hexTime : Decoder Posix
hexTime =
    resultToDecoder (remove0x >> Hex.fromString >> Result.map (\v -> v * 1000 |> millisToPosix))


{-| -}
hexBool : Decoder Bool
hexBool =
    let
        isBool n =
            case n of
                "0x0" ->
                    Ok False

                "0x1" ->
                    Ok True

                _ ->
                    Err <| "Error decoding " ++ n ++ "as bool."
    in
    resultToDecoder isBool



-- Utils


{-| -}
resultToDecoder : (String -> Result String a) -> Decoder a
resultToDecoder strToResult =
    let
        convert n =
            case strToResult n of
                Ok val ->
                    Decode.succeed val

                Err error ->
                    Decode.fail error
    in
    Decode.string |> Decode.andThen convert


{-| -}
maybeToDecoder : (String -> Maybe a) -> Decoder a
maybeToDecoder strToMaybe =
    let
        convert n =
            case strToMaybe n of
                Just val ->
                    Decode.succeed val

                Nothing ->
                    Decode.fail ("Decoding failed for '" ++ n ++ "'")
    in
    Decode.string |> Decode.andThen convert


{-| -}
nonZero : Decoder a -> Decoder (Maybe a)
nonZero decoder =
    let
        checkZero str =
            if str == "0x" || str == "0x0" then
                Decode.succeed Nothing

            else if remove0x str |> String.all (\s -> s == '0') then
                Decode.succeed Nothing

            else
                Decode.map Just decoder
    in
    Decode.string |> Decode.andThen checkZero
