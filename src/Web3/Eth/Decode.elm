module Web3.Eth.Decode exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required, decode, custom, optional)
import Web3.Decode exposing (resultToDecoder, hexInt, bigInt, hexTime, hexBool)
import Web3.Types exposing (..)
import Web3.Eth.Types exposing (..)
import Web3.Utils exposing (remove0x, toAddress, toHex, toTxHash, toBlockHash)


-- Rudimentary Types


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



-- Record Types


block : Decoder a -> Decoder (Block a)
block txsDecoder =
    decode Block
        |> required "number" int
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
        |> required "totalDifficulty" bigInt
        |> required "extraData" string
        |> required "size" hexInt
        |> required "gasLimit" hexInt
        |> required "gasUsed" hexInt
        |> required "timestamp" hexTime
        |> optional "transactions" (list txsDecoder) []
        |> required "uncles" (list string)


uncle : Decoder (Block ())
uncle =
    block (succeed ())


{-| -}
tx : Decoder Tx
tx =
    decode Tx
        |> required "hash" txHash
        |> required "nonce" hexInt
        |> required "blockHash" blockHash
        |> required "blockNumber" hexInt
        |> required "transactionIndex" hexInt
        |> required "from" address
        |> required "to" (nullable address)
        |> required "value" bigInt
        |> required "gasPrice" bigInt
        |> required "gas" hexInt
        |> required "input" string


pendingTx : Decoder PendingTx
pendingTx =
    decode PendingTx
        |> required "hash" txHash
        |> required "nonce" hexInt
        |> required "from" address
        |> required "to" (nullable address)
        |> required "value" bigInt
        |> required "gasPrice" bigInt
        |> required "gas" hexInt
        |> required "input" string


{-| -}
txReceipt : Decoder TxReceipt
txReceipt =
    decode TxReceipt
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
    decode Log
        |> required "address" address
        |> required "data" string
        |> required "topics" (list string)
        |> required "removed" bool
        |> required "logIndex" hexInt
        |> required "transactionIndex" hexInt
        |> required "transactionHash" txHash
        |> required "blockHash" blockHash
        |> required "blockNumber" hexInt


{-| -}
event : Decoder a -> Decoder (Event a)
event returnDataDecoder =
    decode Event
        |> required "address" address
        |> required "data" string
        |> required "topics" (list string)
        |> required "removed" bool
        |> required "logIndex" hexInt
        |> required "transactionIndex" hexInt
        |> required "transactionHash" txHash
        |> required "blockHash" blockHash
        |> required "blockNumber" hexInt
        |> custom returnDataDecoder


syncStatus : Decoder (Maybe SyncStatus)
syncStatus =
    decode SyncStatus
        |> required "startingBlock" int
        |> required "currentBlock" int
        |> required "highestBlock" int
        |> required "knownStates" int
        |> required "pulledStates" int
        |> maybe
