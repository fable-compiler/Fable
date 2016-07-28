namespace Fable.Import

open System
open Fable.Core
open Fable.Import
open Fable.Import.JS
open Fable.Import.Browser

module ReactImagePicker =

    [<StringEnum>]
    type CameraType =
    | Front
    | Back

    [<StringEnum>]
    type MediaType =
    | Photo
    | Video
    | Mixed

    [<StringEnum>]
    type VideoQuality =
    | Low
    | Medium
    | High    

    type ImagePicker =
        abstract member showImagePicker: ImagePickerOptions * (ImagePickerResult -> unit) -> unit
        abstract member launchCamera: ImagePickerOptions * (ImagePickerResult -> unit) -> unit
        abstract member launchImageLibrary: ImagePickerOptions * (ImagePickerResult -> unit) -> unit

    and StorageOptions =
        abstract skipBackup: bool option with get, set
        abstract path: string option with get, set

    and ImagePickerOptions =
        abstract title: string option with get, set
        abstract cancelButtonTitle: string option with get, set
        abstract takePhotoButtonTitle: string option with get, set
        abstract chooseFromLibraryButtonTitle: string option with get, set
        abstract cameraType: CameraType option with get, set
        abstract mediaType: MediaType option with get, set
        abstract maxWidth: int option with get, set
        abstract maxHeight: int option with get, set
        abstract quality: float option with get, set
        abstract videoQuality: VideoQuality option with get, set
        abstract durationLimit: int option with get, set
        abstract rotation: int option with get, set
        abstract allowsEditing: bool option with get, set
        abstract noData: bool option with get, set
        abstract storageOptions: StorageOptions option with get, set

    and ImagePickerResult =     
        abstract didCancel: bool with get, set
        abstract error: string with get, set
        abstract data: string with get, set
        abstract uri: string with get, set
        abstract origURL: string with get, set
        abstract isVertical: bool with get, set
        abstract width: int with get, set
        abstract height: int with get, set
        abstract fileSize: int with get, set

    type Globals =
        [<Import("default", from="react-native-image-picker")>]
        static member ImagePicker with get(): ImagePicker = failwith "JS only" and set(v: ImagePicker): unit = failwith "JS only"