module rec Fable.Import.Node.Crypto

open Fable.Core
open Fable.Import.JS
open Fable.Import.Node

type [<AllowNullLiteral>] CredentialDetails =
    abstract pfx: string with get, set
    abstract key: string with get, set
    abstract passphrase: string with get, set
    abstract cert: string with get, set
    abstract ca: obj with get, set
    abstract crl: obj with get, set
    abstract ciphers: string with get, set

type [<AllowNullLiteral>] Credentials =
    abstract context: obj option with get, set

type [<AllowNullLiteral>] Hash =
    abstract update: data: obj * ?input_encoding: string -> Hash
    [<Emit("$0.digest('buffer')")>] abstract digest_buffer: unit -> Buffer.Buffer
    abstract digest: encoding: string -> obj
    abstract digest: unit -> Buffer.Buffer

type [<AllowNullLiteral>] Hmac =
    abstract update: data: obj * ?input_encoding: string -> Hmac
    [<Emit("$0.digest('buffer')")>] abstract digest_buffer: unit -> Buffer.Buffer
    abstract digest: encoding: string -> obj
    abstract digest: unit -> Buffer.Buffer

type [<AllowNullLiteral>] Cipher =
    abstract update: data: Buffer.Buffer -> Buffer.Buffer
    abstract update: data: string * ?input_encoding: string * ?output_encoding: string -> string
    abstract final: unit -> Buffer.Buffer
    abstract final: output_encoding: string -> string
    abstract setAutoPadding: auto_padding: bool -> unit

type [<AllowNullLiteral>] Decipher =
    abstract update: data: Buffer.Buffer -> Buffer.Buffer
    abstract update: data: string * ?input_encoding: string * ?output_encoding: string -> string
    abstract final: unit -> Buffer.Buffer
    abstract final: output_encoding: string -> string
    abstract setAutoPadding: auto_padding: bool -> unit

type [<AllowNullLiteral>] Signer =
    inherit Stream.Writable<Buffer.Buffer>
    abstract update: data: obj -> unit
    abstract sign: private_key: string * output_format: string -> string

type [<AllowNullLiteral>] Verify =
    inherit Stream.Writable<Buffer.Buffer>
    abstract update: data: obj -> unit
    abstract verify: ``object``: string * signature: string * ?signature_format: string -> bool

type [<AllowNullLiteral>] DiffieHellman =
    abstract generateKeys: ?encoding: string -> string
    abstract computeSecret: other_public_key: string * ?input_encoding: string * ?output_encoding: string -> string
    abstract getPrime: ?encoding: string -> string
    abstract getGenerator: encoding: string -> string
    abstract getPublicKey: ?encoding: string -> string
    abstract getPrivateKey: ?encoding: string -> string
    abstract setPublicKey: public_key: string * ?encoding: string -> unit
    abstract setPrivateKey: public_key: string * ?encoding: string -> unit

type IExports =
    abstract createCredentials: details: CredentialDetails -> Credentials
    abstract createHash: algorithm: string -> Hash
    abstract createHmac: algorithm: string * key: string -> Hmac
    abstract createHmac: algorithm: string * key: Buffer.Buffer -> Hmac
    abstract createCipher: algorithm: string -> password: obj -> Cipher
    abstract createCipheriv: algorithm: string -> key: obj -> iv: obj -> Cipher
    abstract createDecipher: algorithm: string -> password: obj -> Decipher
    abstract createDecipheriv: algorithm: string -> key: obj -> iv: obj -> Decipher
    abstract createSign: algorithm: string -> Signer
    abstract createVerify: algorith: string -> Verify
    abstract createDiffieHellman: prime_length: float -> DiffieHellman
    abstract createDiffieHellman: prime: float * ?encoding: string -> DiffieHellman
    abstract getDiffieHellman: group_name: string -> DiffieHellman
    abstract pbkdf2: password: string * salt: string * iterations: float * keylen: float * callback: (Error -> Buffer.Buffer -> obj) -> unit
    abstract pbkdf2: password: string * salt: string * iterations: float * keylen: float * digest: string * callback: (Error -> Buffer.Buffer -> obj) -> unit
    abstract pbkdf2Sync: password: string * salt: string * iterations: float * keylen: float -> Buffer.Buffer
    abstract pbkdf2Sync: password: string * salt: string * iterations: float * keylen: float * digest: string -> Buffer.Buffer
    abstract randomBytes: size: float -> Buffer.Buffer
    abstract randomBytes: size: float * callback: (Error -> Buffer.Buffer -> unit) -> unit
    abstract pseudoRandomBytes: size: float -> Buffer.Buffer
    abstract pseudoRandomBytes: size: float * callback: (Error -> Buffer.Buffer -> unit) -> unit
