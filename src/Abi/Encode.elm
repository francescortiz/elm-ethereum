module Abi.Encode exposing
    ( Encoding, functionCall, functionCallWithDebug
    , address, uint, int, bool, staticBytes, ipfsHash, custom
    , abiEncode
    )

{-| Encode before sending RPC Calls

@docs Encoding, functionCall, functionCallWithDebug

@docs address, uint, int, bool, staticBytes, ipfsHash, custom


# Low-Level

@docs abiEncode

-}

import Abi.Int as AbiInt
import Eth.Types exposing (Address, Hex, IPFSHash)
import Eth.Utils as EthUtils exposing (functionSig, ipfsToBytes32)
import Internal.Types as Internal
import Internal.Utils as IU exposing (..)
import Legacy.BigInt as BigInt exposing (BigInt)
import Legacy.Logging


{-| Not yet implemented : Dynamic Bytes, String, List
-}
type Encoding
    = AddressE Address
    | UintE BigInt
    | IntE BigInt
    | BoolE Bool
    | DBytesE Hex
    | BytesE Hex
    | StringE String
    | ListE Encoding
    | IPFSHashE IPFSHash
    | Custom String


{-| -}
functionCall : String -> List Encoding -> Hex
functionCall =
    functionCall_ False


{-| -}
functionCallWithDebug : String -> List Encoding -> Hex
functionCallWithDebug =
    functionCall_ True



-- Encoders


{-| -}
address : Address -> Encoding
address =
    AddressE


{-| -}
uint : BigInt -> Encoding
uint =
    UintE


{-| -}
int : BigInt -> Encoding
int =
    IntE


{-| -}
bool : Bool -> Encoding
bool =
    BoolE


{-| -}
staticBytes : Hex -> Encoding
staticBytes =
    BytesE


{-| -}
ipfsHash : IPFSHash -> Encoding
ipfsHash =
    IPFSHashE


{-| -}
custom : String -> Encoding
custom =
    Custom



-- Low Level


{-| -}
abiEncode : Encoding -> Hex
abiEncode =
    lowLevelEncode >> Internal.Hex



-- Internal


{-| -}
functionCall_ : Bool -> String -> List Encoding -> Hex
functionCall_ isDebug sig encodings =
    let
        byteCodeEncodings =
            List.map lowLevelEncode encodings
                |> String.join ""

        data_ =
            EthUtils.functionSig sig
                |> EthUtils.hexToString
                |> IU.remove0x
                |> (\str -> str ++ byteCodeEncodings)

        data =
            if isDebug then
                Legacy.Logging.log ("Debug Contract Call " ++ sig) data_

            else
                data_
    in
    Internal.Hex data


{-| -}
lowLevelEncode : Encoding -> String
lowLevelEncode enc =
    case enc of
        AddressE (Internal.Address address_) ->
            IU.leftPadTo64 address_

        UintE uint_ ->
            BigInt.toHexString uint_
                |> IU.leftPadTo64

        IntE int_ ->
            AbiInt.toString int_

        BoolE True ->
            IU.leftPadTo64 "1"

        BoolE False ->
            IU.leftPadTo64 "0"

        DBytesE _ ->
            "not implemeneted yet"

        BytesE (Internal.Hex hexString) ->
            IU.remove0x hexString
                |> IU.leftPadTo64

        StringE string ->
            "not implemeneted yet"

        ListE _ ->
            "not implemeneted yet"

        IPFSHashE ipfsHash_ ->
            EthUtils.ipfsToBytes32 ipfsHash_
                |> (\(Internal.Hex zerolessHex) -> zerolessHex)

        Custom string ->
            IU.remove0x string
