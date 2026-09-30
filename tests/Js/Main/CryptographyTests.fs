module Fable.Tests.Cryptography

open System.Security.Cryptography
open System.Text
open Util.Testing
open Fable.Tests.Util

let private hash (value: string) =
    use sha256 = SHA256.Create()
    value |> Encoding.UTF8.GetBytes |> sha256.ComputeHash

let tests =
    testList
        "Cryptography"
        [
            testCase "SHA256 computes the empty-string digest" <| fun () ->
                hash ""
                |> equal
                    [|
                        0xe3uy; 0xb0uy; 0xc4uy; 0x42uy; 0x98uy; 0xfcuy; 0x1cuy; 0x14uy
                        0x9auy; 0xfbuy; 0xf4uy; 0xc8uy; 0x99uy; 0x6fuy; 0xb9uy; 0x24uy
                        0x27uy; 0xaeuy; 0x41uy; 0xe4uy; 0x64uy; 0x9buy; 0x93uy; 0x4cuy
                        0xa4uy; 0x95uy; 0x99uy; 0x1buy; 0x78uy; 0x52uy; 0xb8uy; 0x55uy
                    |]

            testCase "SHA256 computes a single-block digest" <| fun () ->
                hash "abc"
                |> equal
                    [|
                        0xbauy; 0x78uy; 0x16uy; 0xbfuy; 0x8fuy; 0x01uy; 0xcfuy; 0xeauy
                        0x41uy; 0x41uy; 0x40uy; 0xdeuy; 0x5duy; 0xaeuy; 0x22uy; 0x23uy
                        0xb0uy; 0x03uy; 0x61uy; 0xa3uy; 0x96uy; 0x17uy; 0x7auy; 0x9cuy
                        0xb4uy; 0x10uy; 0xffuy; 0x61uy; 0xf2uy; 0x00uy; 0x15uy; 0xaduy
                    |]

            testCase "SHA256 computes a multi-block digest" <| fun () ->
                hash "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
                |> equal
                    [|
                        0x24uy; 0x8duy; 0x6auy; 0x61uy; 0xd2uy; 0x06uy; 0x38uy; 0xb8uy
                        0xe5uy; 0xc0uy; 0x26uy; 0x93uy; 0x0cuy; 0x3euy; 0x60uy; 0x39uy
                        0xa3uy; 0x3cuy; 0xe4uy; 0x59uy; 0x64uy; 0xffuy; 0x21uy; 0x67uy
                        0xf6uy; 0xecuy; 0xeduy; 0xd4uy; 0x19uy; 0xdbuy; 0x06uy; 0xc1uy
                    |]
        ]
