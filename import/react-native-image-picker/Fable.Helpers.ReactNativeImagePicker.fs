[<Fable.Core.Erase>]
module Fable.Helpers.ReactNativeImagePicker

open Fable.Core
open Fable.Import
open Fable.Import.ReactNativeImagePicker
type IP = ReactNativeImagePicker.Globals

module Props =
    [<KeyValueList>]
    type IImagePickerOptions =
        interface end

    [<KeyValueList>]
    type ImagePickerOptions =
    | Title of string
    | CancelButtonTitle of string
    | TakePhotoButtonTitle of string
    | ChooseFromLibraryButtonTitle of string
    | CameraType of CameraType
    | MediaType of MediaType
    | MaxWidth of int
    | MaxHeight of int
    | Quality of float
    | VideoQuality of VideoQuality
    | DurationLimit of int
    | Rotation of int
    | AllowsEditing of bool
    | NoData of bool
    | StorageOptions of StorageOptions
        interface IImagePickerOptions

open Props

let inline showImagePicker (props: IImagePickerOptions list) f =
    IP.ImagePicker.showImagePicker(props |> unbox, f)

