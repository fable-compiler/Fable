// Check two files with same name in different folders don't conflict
// https://github.com/Microsoft/visualfsharp/issues/2679
// https://github.com/fable-compiler/Fable/issues/781
namespace tempet

module SayB =
    let hello name = sprintf "Hello %s from SayB" name