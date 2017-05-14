namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

[<Import("*","three")>]
module Three =
    // https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent.button
    type MOUSE =
        | LEFT = 0
        | MIDDLE = 1
        | RIGHT = 2

    // GL STATE CONSTANTS
    type CullFace = interface end
    let CullFaceNone: CullFace = failwith "JS only"
    let CullFaceBack: CullFace = failwith "JS only"
    let CullFaceFront: CullFace = failwith "JS only"
    let CullFaceFrontBack: CullFace = failwith "JS only"

    type FrontFaceDirection = interface end
    let FrontFaceDirectionCW: FrontFaceDirection = failwith "JS only"
    let FrontFaceDirectionCCW: FrontFaceDirection = failwith "JS only"

    // Shadowing Type
    type ShadowMapType = interface end
    let BasicShadowMap: ShadowMapType = failwith "JS only"
    let PCFShadowMap: ShadowMapType = failwith "JS only"
    let PCFSoftShadowMap: ShadowMapType = failwith "JS only"

    // MATERIAL CONSTANTS

    // side
    type Side = interface end
    let FrontSide: Side = failwith "JS only"
    let BackSide: Side = failwith "JS only"
    let DoubleSide: Side = failwith "JS only"

    // shading
    type Shading = interface end
    let FlatShading: Shading = failwith "JS only"
    let SmoothShading: Shading = failwith "JS only"

    // colors
    type Colors = interface end
    let NoColors: Colors = failwith "JS only"
    let FaceColors: Colors = failwith "JS only"
    let VertexColors: Colors = failwith "JS only"

    // blending modes
    type Blending = interface end
    let NoBlending: Blending = failwith "JS only"
    let NormalBlending: Blending = failwith "JS only"
    let AdditiveBlending: Blending = failwith "JS only"
    let SubtractiveBlending: Blending = failwith "JS only"
    let MultiplyBlending: Blending = failwith "JS only"
    let CustomBlending: Blending = failwith "JS only"

    // custom blending equations
    // (numbers start from 100 not to clash with other
    //  mappings to OpenGL constants defined in Texture.js)
    type BlendingEquation = interface end
    let AddEquation: BlendingEquation = failwith "JS only"
    let SubtractEquation: BlendingEquation = failwith "JS only"
    let ReverseSubtractEquation: BlendingEquation = failwith "JS only"
    let MinEquation: BlendingEquation = failwith "JS only"
    let MaxEquation: BlendingEquation = failwith "JS only"

    // custom blending destination factors
    type BlendingDstFactor = interface end
    let ZeroFactor: BlendingDstFactor = failwith "JS only"
    let OneFactor: BlendingDstFactor = failwith "JS only"
    let SrcColorFactor: BlendingDstFactor = failwith "JS only"
    let OneMinusSrcColorFactor: BlendingDstFactor = failwith "JS only"
    let SrcAlphaFactor: BlendingDstFactor = failwith "JS only"
    let OneMinusSrcAlphaFactor: BlendingDstFactor = failwith "JS only"
    let DstAlphaFactor: BlendingDstFactor = failwith "JS only"
    let OneMinusDstAlphaFactor: BlendingDstFactor = failwith "JS only"

   // custom blending src factors
    type BlendingSrcFactor = interface end
    let DstColorFactor: BlendingSrcFactor = failwith "JS only"
    let OneMinusDstColorFactor: BlendingSrcFactor = failwith "JS only"
    let SrcAlphaSaturateFactor: BlendingSrcFactor = failwith "JS only"

    // depth modes
    type DepthModes = interface end
    let NeverDepth: DepthModes = failwith "JS only"
    let AlwaysDepth: DepthModes = failwith "JS only"
    let LessDepth: DepthModes = failwith "JS only"
    let LessEqualDepth: DepthModes = failwith "JS only"
    let EqualDepth: DepthModes = failwith "JS only"
    let GreaterEqualDepth: DepthModes = failwith "JS only"
    let GreaterDepth: DepthModes = failwith "JS only"
    let NotEqualDepth: DepthModes = failwith "JS only"

    // TEXTURE CONSTANTS
    // Operations
    type Combine = interface end
    let MultiplyOperation: Combine = failwith "JS only"
    let MixOperation: Combine = failwith "JS only"
    let AddOperation: Combine = failwith "JS only"

    // Tone Mapping modes
    type ToneMapping = interface end
    let NoToneMapping: ToneMapping = failwith "JS only"
    let LinearToneMapping: ToneMapping = failwith "JS only"
    let ReinhardToneMapping: ToneMapping = failwith "JS only"
    let Uncharted2ToneMapping: ToneMapping = failwith "JS only"
    let CineonToneMapping: ToneMapping = failwith "JS only"

    // Mapping modes
    type Mapping = interface end
    let UVMapping: Mapping = failwith "JS only"
    let CubeReflectionMapping: Mapping = failwith "JS only"
    let CubeRefractionMapping: Mapping = failwith "JS only"
    let EquirectangularReflectionMapping: Mapping = failwith "JS only"
    let EquirectangularRefractionMapping: Mapping = failwith "JS only"
    let SphericalReflectionMapping: Mapping = failwith "JS only"
    let CubeUVReflectionMapping: Mapping = failwith "JS only"
    let CubeUVRefractionMapping: Mapping = failwith "JS only"

    // Wrapping modes
    type Wrapping = interface end
    let RepeatWrapping: Wrapping = failwith "JS only"
    let ClampToEdgeWrapping: Wrapping = failwith "JS only"
    let MirroredRepeatWrapping: Wrapping = failwith "JS only"

    // Filters
    type TextureFilter = interface end
    let NearestFilter: TextureFilter = failwith "JS only"
    let NearestMipMapNearestFilter: TextureFilter = failwith "JS only"
    let NearestMipMapLinearFilter: TextureFilter = failwith "JS only"
    let LinearFilter: TextureFilter = failwith "JS only"
    let LinearMipMapNearestFilter: TextureFilter = failwith "JS only"
    let LinearMipMapLinearFilter: TextureFilter = failwith "JS only"

    // Data types
    type TextureDataType = interface end
    let UnsignedByteType: TextureDataType = failwith "JS only"
    let ByteType: TextureDataType = failwith "JS only"
    let ShortType: TextureDataType = failwith "JS only"
    let UnsignedShortType: TextureDataType = failwith "JS only"
    let IntType: TextureDataType = failwith "JS only"
    let UnsignedIntType: TextureDataType = failwith "JS only"
    let FloatType: TextureDataType = failwith "JS only"
    let HalfFloatType: TextureDataType = failwith "JS only"

    // Pixel types
    type PixelType = interface end
    let UnsignedShort4444Type: PixelType = failwith "JS only"
    let UnsignedShort5551Type: PixelType = failwith "JS only"
    let UnsignedShort565Type: PixelType = failwith "JS only"

    // Pixel formats
    type PixelFormat = interface end
    let AlphaFormat: PixelFormat = failwith "JS only"
    let RGBFormat: PixelFormat = failwith "JS only"
    let RGBAFormat: PixelFormat = failwith "JS only"
    let LuminanceFormat: PixelFormat = failwith "JS only"
    let LuminanceAlphaFormat: PixelFormat = failwith "JS only"
    let RGBEFormat: PixelFormat = failwith "JS only"

    // Compressed texture formats
    // DDS / ST3C Compressed texture formats
    type CompressedPixelFormat = interface end
    let RGB_S3TC_DXT1_Format: CompressedPixelFormat = failwith "JS only"
    let RGBA_S3TC_DXT1_Format: CompressedPixelFormat = failwith "JS only"
    let RGBA_S3TC_DXT3_Format: CompressedPixelFormat = failwith "JS only"
    let RGBA_S3TC_DXT5_Format: CompressedPixelFormat = failwith "JS only"

    // PVRTC compressed texture formats
    let RGB_PVRTC_4BPPV1_Format: CompressedPixelFormat = failwith "JS only"
    let RGB_PVRTC_2BPPV1_Format: CompressedPixelFormat = failwith "JS only"
    let RGBA_PVRTC_4BPPV1_Format: CompressedPixelFormat = failwith "JS only"
    let RGBA_PVRTC_2BPPV1_Format: CompressedPixelFormat = failwith "JS only"

    // ETC compressed texture formats
    let RGB_ETC1_Format: CompressedPixelFormat = failwith "JS only"

    // Loop styles for AnimationAction
    type AnimationActionLoopStyles = interface end
    let LoopOnce: AnimationActionLoopStyles = failwith "JS only"
    let LoopRepeat: AnimationActionLoopStyles = failwith "JS only"
    let LoopPingPong: AnimationActionLoopStyles = failwith "JS only"

    // Interpolation
    type InterpolationModes = interface end
    let InterpolateDiscrete: InterpolationModes = failwith "JS only"
    let InterpolateLinear: InterpolationModes = failwith "JS only"
    let InterpolateSmooth: InterpolationModes = failwith "JS only"

    // Interpolant ending modes
    type InterpolationEndingModes = interface end
    let ZeroCurvatureEnding: InterpolationEndingModes = failwith "JS only"
    let ZeroSlopeEnding: InterpolationEndingModes = failwith "JS only"
    let WrapAroundEnding: InterpolationEndingModes = failwith "JS only"

    // Triangle Draw modes
    type TrianglesDrawModes = interface end
    let TrianglesDrawModesMode: TrianglesDrawModes = failwith "JS only"
    let TriangleStripDrawMode: TrianglesDrawModes = failwith "JS only"
    let TriangleFanDrawMode: TrianglesDrawModes = failwith "JS only"

    // Texture Encodings
    type TextureEncoding = interface end
    let LinearEncoding: TextureEncoding = failwith "JS only"
    let sRGBEncoding: TextureEncoding = failwith "JS only"
    let GammaEncoding: TextureEncoding = failwith "JS only"
    let RGBEEncoding: TextureEncoding = failwith "JS only"
    let LogLuvEncoding: TextureEncoding = failwith "JS only"
    let RGBM7Encoding: TextureEncoding = failwith "JS only"
    let RGBM16Encoding: TextureEncoding = failwith "JS only"
    let RGBDEncoding: TextureEncoding = failwith "JS only"

    type [<Import("AnimationClip","three")>] AnimationClip(?name: string, ?duration: float, ?tracks: ResizeArray<KeyframeTrack>) =
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.tracks with get(): ResizeArray<KeyframeTrack> = failwith "JS only" and set(v: ResizeArray<KeyframeTrack>): unit = failwith "JS only"
        member __.duration with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.results with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.resetDuration(): unit = failwith "JS only"
        member __.trim(): AnimationClip = failwith "JS only"
        member __.optimize(): AnimationClip = failwith "JS only"
        static member CreateFromMorphTargetSequence(name: string, morphTargetSequence: ResizeArray<MorphTarget>, fps: float): AnimationClip = failwith "JS only"
        static member findByName(clipArray: AnimationClip, name: string): AnimationClip = failwith "JS only"
        static member CreateClipsFromMorphTargetSequences(morphTargets: ResizeArray<MorphTarget>, fps: float): ResizeArray<AnimationClip> = failwith "JS only"
        static member parse(json: obj): AnimationClip = failwith "JS only"
        static member parseAnimation(animation: obj, bones: ResizeArray<Bone>, nodeName: string): AnimationClip = failwith "JS only"
        static member toJSON(): obj = failwith "JS only"

    and [<Import("AnimationMixer","three")>] AnimationMixer(root: obj) =
        member __.time with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.timeScale with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clipAction(clip: AnimationClip, ?root: obj): obj = failwith "JS only"
        member __.existingAction(clip: AnimationClip, ?root: obj): obj = failwith "JS only"
        member __.stopAllAction(clip: AnimationClip, ?root: obj): AnimationMixer = failwith "JS only"
        member __.update(deltaTime: float): AnimationMixer = failwith "JS only"
        member __.getRoot(): obj = failwith "JS only"
        member __.uncacheClip(clip: AnimationClip): unit = failwith "JS only"
        member __.uncacheRoot(root: obj): unit = failwith "JS only"
        member __.uncazcheAction(clip: AnimationClip, ?root: obj): unit = failwith "JS only"

    and [<Import("AnimationObjectGroup","three")>] AnimationObjectGroup([<ParamArray>] args: obj[]) =
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.stats with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.add([<ParamArray>] args: obj[]): unit = failwith "JS only"
        member __.remove([<ParamArray>] args: obj[]): unit = failwith "JS only"
        member __.uncache([<ParamArray>] args: obj[]): unit = failwith "JS only"

    and [<Import("KeyframeTrack","three")>] KeyframeTrack(name: string, times: ResizeArray<obj>, values: ResizeArray<obj>, interpolation: InterpolationModes) =
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.times with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.values with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.ValueTypeName with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.TimeBufferType with get(): Float32Array = failwith "JS only" and set(v: Float32Array): unit = failwith "JS only"
        member __.ValueBufferType with get(): Float32Array = failwith "JS only" and set(v: Float32Array): unit = failwith "JS only"
        member __.DefaultInterpolation with get(): InterpolationModes = failwith "JS only" and set(v: InterpolationModes): unit = failwith "JS only"
        member __.InterpolantFactoryMethodDiscrete(result: obj): DiscreteInterpolant = failwith "JS only"
        member __.InterpolantFactoryMethodLinear(result: obj): LinearInterpolant = failwith "JS only"
        member __.InterpolantFactoryMethodSmooth(result: obj): CubicInterpolant = failwith "JS only"
        member __.setInterpolation(interpolation: InterpolationModes): unit = failwith "JS only"
        member __.getInterpolation(): InterpolationModes = failwith "JS only"
        member __.getValuesize(): float = failwith "JS only"
        member __.shift(timeOffset: float): KeyframeTrack = failwith "JS only"
        member __.scale(timeScale: float): KeyframeTrack = failwith "JS only"
        member __.trim(startTime: float, endTime: float): KeyframeTrack = failwith "JS only"
        member __.validate(): bool = failwith "JS only"
        member __.optimize(): KeyframeTrack = failwith "JS only"
        static member parse(json: obj): KeyframeTrack = failwith "JS only"
        static member toJSON(track: KeyframeTrack): obj = failwith "JS only"

    and [<Import("PropertyBinding","three")>] PropertyBinding(rootNode: obj, path: string, ?parsedPath: obj) =
        member __.path with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.parsedPath with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.node with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.rootNode with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.BindingType with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.Versioning with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.GetterByBindingType with get(): ResizeArray<Function> = failwith "JS only" and set(v: ResizeArray<Function>): unit = failwith "JS only"
        member __.SetterByBindingTypeAndVersioning with get(): ResizeArray<ResizeArray<Function>> = failwith "JS only" and set(v: ResizeArray<ResizeArray<Function>>): unit = failwith "JS only"
        member __.getValue(targetArray: obj, offset: float): obj = failwith "JS only"
        member __.setValue(sourceArray: obj, offset: float): unit = failwith "JS only"
        member __.bind(): unit = failwith "JS only"
        member __.unbind(): unit = failwith "JS only"
        static member create(root: obj, path: obj, ?parsedPath: obj): PropertyBinding = failwith "JS only"
        static member parseTrackName(trackName: string): obj = failwith "JS only"
        static member findNode(root: obj, nodeName: string): obj = failwith "JS only"

    and [<Import("PropertyMixer","three")>] PropertyMixer(binding: obj, typeName: string, valueSize: float) =
        member __.binding with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.valueSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.buffer with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.cumulativeWeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.useCount with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.referenceCount with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.accumulate(accuIndex: float, weight: float): unit = failwith "JS only"
        member __.apply(accuIndex: float): unit = failwith "JS only"
        member __.saveOriginalState(): unit = failwith "JS only"
        member __.restoreOriginalState(): unit = failwith "JS only"

    and [<Import("BooleanKeyframeTrack","three")>] BooleanKeyframeTrack(name: string, times: ResizeArray<obj>, values: ResizeArray<obj>) =
        inherit KeyframeTrack(name, times, values, unbox null)

    and [<Import("NumberKeyframeTrack","three")>] NumberKeyframeTrack(name: string, times: ResizeArray<obj>, values: ResizeArray<obj>, interpolation: InterpolationModes) =
        inherit KeyframeTrack(name, times, values, interpolation)

    and [<Import("QuaternionKeyframeTrack","three")>] QuaternionKeyframeTrack(name: string, times: ResizeArray<obj>, values: ResizeArray<obj>, interpolation: InterpolationModes) =
        inherit KeyframeTrack(name, times, values, interpolation)

    and [<Import("StringKeyframeTrack","three")>] StringKeyframeTrack(name: string, times: ResizeArray<obj>, values: ResizeArray<obj>, interpolation: InterpolationModes) =
        inherit KeyframeTrack(name, times, values, interpolation)

    and [<Import("VectorKeyframeTrack","three")>] VectorKeyframeTrack(name: string, times: ResizeArray<obj>, values: ResizeArray<obj>, interpolation: InterpolationModes) =
        inherit KeyframeTrack(name, times, values, interpolation)

    and [<Import("Camera","three")>] Camera() =
        inherit Object3D()
        member __.matrixWorldInverse with get(): Matrix4 = failwith "JS only" and set(v: Matrix4): unit = failwith "JS only"
        member __.projectionMatrix with get(): Matrix4 = failwith "JS only" and set(v: Matrix4): unit = failwith "JS only"
        member __.getWorldDirection(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.lookAt(vector: Vector3): unit = failwith "JS only"
        member __.clone(): Camera = failwith "JS only"
        member __.copy(?camera: Camera): Camera = failwith "JS only"

    and [<Import("CubeCamera","three")>] CubeCamera(?near: float, ?far: float, ?cubeResolution: float) =
        inherit Object3D()
        member __.renderTarget with get(): WebGLRenderTargetCube = failwith "JS only" and set(v: WebGLRenderTargetCube): unit = failwith "JS only"
        member __.updateCubeMap(renderer: Renderer, scene: Scene): unit = failwith "JS only"

    and [<Import("OrthographicCamera","three")>] OrthographicCamera(left: float, right: float, top: float, bottom: float, ?near: float, ?far: float) =
        inherit Camera()
        member __.zoom with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.left with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.right with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.top with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.bottom with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.near with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.far with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.updateProjectionMatrix(): unit = failwith "JS only"
        member __.clone(): OrthographicCamera = failwith "JS only"
        member __.copy(source: OrthographicCamera): OrthographicCamera = failwith "JS only"
        member __.toJSON(?meta: obj): obj = failwith "JS only"

    and [<Import("PerspectiveCamera","three")>] PerspectiveCamera(?fov: float, ?aspect: float, ?near: float, ?far: float) =
        inherit Camera()
        member __.focalLength with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.zoom with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.fov with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.aspect with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.near with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.far with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.setLens(focalLength: float, ?frameHeight: float): unit = failwith "JS only"
        member __.setViewOffset(fullWidth: float, fullHeight: float, x: float, y: float, width: float, height: float): unit = failwith "JS only"
        member __.updateProjectionMatrix(): unit = failwith "JS only"
        member __.clone(): PerspectiveCamera = failwith "JS only"
        member __.copy(source: PerspectiveCamera): PerspectiveCamera = failwith "JS only"
        member __.toJSON(?meta: obj): obj = failwith "JS only"

    and [<Import("StereoCamera","three")>] StereoCamera() =
        inherit Camera()
        member __.aspect with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.cameraL with get(): PerspectiveCamera = failwith "JS only" and set(v: PerspectiveCamera): unit = failwith "JS only"
        member __.cameraR with get(): PerspectiveCamera = failwith "JS only" and set(v: PerspectiveCamera): unit = failwith "JS only"
        member __.update(camera: PerspectiveCamera): unit = failwith "JS only"

    and [<Import("BufferAttribute","three")>] BufferAttribute(array: ArrayLike<float>, itemSize: float) =
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.array with get(): ArrayLike<float> = failwith "JS only" and set(v: ArrayLike<float>): unit = failwith "JS only"
        member __.itemSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.dynamic with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.updateRange with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.version with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.needsUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.count with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.length with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.setDynamic(dynamic: bool): BufferAttribute = failwith "JS only"
        member __.clone(): BufferAttribute = failwith "JS only"
        member __.copy(source: BufferAttribute): BufferAttribute = failwith "JS only"
        member __.copyAt(index1: float, attribute: BufferAttribute, index2: float): BufferAttribute = failwith "JS only"
        member __.copyArray(array: ArrayLike<float>): BufferAttribute = failwith "JS only"
        member __.copyColorsArray(colors: ResizeArray<obj>): BufferAttribute = failwith "JS only"
        member __.copyIndicesArray(indices: ResizeArray<obj>): BufferAttribute = failwith "JS only"
        member __.copyVector2sArray(vectors: ResizeArray<obj>): BufferAttribute = failwith "JS only"
        member __.copyVector3sArray(vectors: ResizeArray<obj>): BufferAttribute = failwith "JS only"
        member __.copyVector4sArray(vectors: ResizeArray<obj>): BufferAttribute = failwith "JS only"
        member __.set(value: ArrayLike<float>, ?offset: float): BufferAttribute = failwith "JS only"
        member __.getX(index: float): float = failwith "JS only"
        member __.setX(index: float, x: float): BufferAttribute = failwith "JS only"
        member __.getY(index: float): float = failwith "JS only"
        member __.setY(index: float, y: float): BufferAttribute = failwith "JS only"
        member __.getZ(index: float): float = failwith "JS only"
        member __.setZ(index: float, z: float): BufferAttribute = failwith "JS only"
        member __.getW(index: float): float = failwith "JS only"
        member __.setW(index: float, z: float): BufferAttribute = failwith "JS only"
        member __.setXY(index: float, x: float, y: float): BufferAttribute = failwith "JS only"
        member __.setXYZ(index: float, x: float, y: float, z: float): BufferAttribute = failwith "JS only"
        member __.setXYZW(index: float, x: float, y: float, z: float, w: float): BufferAttribute = failwith "JS only"

    and [<Import("Int8Attribute","three")>] Int8Attribute(array: obj, itemSize: float) =
        inherit BufferAttribute(unbox array, itemSize)

    and [<Import("Uint8Attribute","three")>] Uint8Attribute(array: obj, itemSize: float) =
        inherit BufferAttribute(unbox array, itemSize)

    and [<Import("Uint8ClampedAttribute","three")>] Uint8ClampedAttribute(array: obj, itemSize: float) =
        inherit BufferAttribute(unbox array, itemSize)

    and [<Import("Int16Attribute","three")>] Int16Attribute(array: obj, itemSize: float) =
        inherit BufferAttribute(unbox array, itemSize)

    and [<Import("Uint16Attribute","three")>] Uint16Attribute(array: obj, itemSize: float) =
        inherit BufferAttribute(unbox array, itemSize)

    and [<Import("Int32Attribute","three")>] Int32Attribute(array: obj, itemSize: float) =
        inherit BufferAttribute(unbox array, itemSize)

    and [<Import("Uint32Attribute","three")>] Uint32Attribute(array: obj, itemSize: float) =
        inherit BufferAttribute(unbox array, itemSize)

    and [<Import("Float32Attribute","three")>] Float32Attribute(array: obj, itemSize: float) =
        inherit BufferAttribute(unbox array, itemSize)

    and [<Import("Float64Attribute","three")>] Float64Attribute(array: obj, itemSize: float) =
        inherit BufferAttribute(unbox array, itemSize)

    and [<Import("DynamicBufferAttribute","three")>] DynamicBufferAttribute() =
        inherit BufferAttribute(unbox null, 0.)

    and [<Import("BufferGeometry","three")>] BufferGeometry() =
        member __.MaxIndex with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.id with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.index with get(): BufferAttribute = failwith "JS only" and set(v: BufferAttribute): unit = failwith "JS only"
        member __.attributes with get(): U2<BufferAttribute, ResizeArray<InterleavedBufferAttribute>> = failwith "JS only" and set(v: U2<BufferAttribute, ResizeArray<InterleavedBufferAttribute>>): unit = failwith "JS only"
        member __.morphAttributes with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.drawcalls with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.offsets with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.groups with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.boundingBox with get(): Box3 = failwith "JS only" and set(v: Box3): unit = failwith "JS only"
        member __.boundingSphere with get(): BoundingSphere = failwith "JS only" and set(v: BoundingSphere): unit = failwith "JS only"
        member __.drawRange with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.getIndex(): BufferAttribute = failwith "JS only"
        member __.setIndex(index: BufferAttribute): unit = failwith "JS only"
        member __.addAttribute(name: string, attribute: U2<BufferAttribute, InterleavedBufferAttribute>): BufferGeometry = failwith "JS only"
        member __.getAttribute(name: string): U2<BufferAttribute, InterleavedBufferAttribute> = failwith "JS only"
        member __.removeAttribute(name: string): BufferGeometry = failwith "JS only"
        member __.addGroup(start: float, count: float, ?materialIndex: float): unit = failwith "JS only"
        member __.clearGroups(): unit = failwith "JS only"
        member __.setDrawRange(start: float, count: float): unit = failwith "JS only"
        member __.applyMatrix(matrix: Matrix4): BufferGeometry = failwith "JS only"
        member __.rotateX(angle: float): BufferGeometry = failwith "JS only"
        member __.rotateY(angle: float): BufferGeometry = failwith "JS only"
        member __.rotateZ(angle: float): BufferGeometry = failwith "JS only"
        member __.translate(x: float, y: float, z: float): BufferGeometry = failwith "JS only"
        member __.scale(x: float, y: float, z: float): BufferGeometry = failwith "JS only"
        member __.lookAt(v: Vector3): unit = failwith "JS only"
        member __.center(): Vector3 = failwith "JS only"
        member __.setFromObject(``object``: Object3D): unit = failwith "JS only"
        member __.updateFromObject(``object``: Object3D): unit = failwith "JS only"
        member __.fromGeometry(geometry: Geometry, ?settings: obj): BufferGeometry = failwith "JS only"
        member __.fromDirectGeometry(geometry: DirectGeometry): BufferGeometry = failwith "JS only"
        member __.computeBoundingBox(): unit = failwith "JS only"
        member __.computeBoundingSphere(): unit = failwith "JS only"
        member __.computeVertexNormals(): unit = failwith "JS only"
        member __.merge(geometry: BufferGeometry, offset: float): BufferGeometry = failwith "JS only"
        member __.normalizeNormals(): unit = failwith "JS only"
        member __.toNonIndexed(): BufferGeometry = failwith "JS only"
        member __.toJSON(): obj = failwith "JS only"
        member __.clone(): BufferGeometry = failwith "JS only"
        member __.copy(source: BufferGeometry): BufferGeometry = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.addEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.hasEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.removeEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.dispatchEvent(``event``: obj): unit = failwith "JS only"
        member __.addIndex(index: obj): unit = failwith "JS only"
        member __.addAttribute(name: obj, array: obj, itemSize: obj): obj = failwith "JS only"
        member __.addDrawCall(start: obj, count: obj, ?indexOffset: obj): unit = failwith "JS only"
        member __.clearDrawCalls(): unit = failwith "JS only"
        member __.computeFaceNormals(): unit = failwith "JS only"

    and [<Import("Clock","three")>] Clock(?autoStart: bool) =
        member __.autoStart with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.startTime with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.oldTime with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.elapsedTime with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.running with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.start(): unit = failwith "JS only"
        member __.stop(): unit = failwith "JS only"
        member __.getElapsedTime(): float = failwith "JS only"
        member __.getDelta(): float = failwith "JS only"

    and [<Import("DirectGeometry","three")>] DirectGeometry() =
        member __.id with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.indices with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.vertices with get(): ResizeArray<Vector3> = failwith "JS only" and set(v: ResizeArray<Vector3>): unit = failwith "JS only"
        member __.normals with get(): ResizeArray<Vector3> = failwith "JS only" and set(v: ResizeArray<Vector3>): unit = failwith "JS only"
        member __.colors with get(): ResizeArray<Color> = failwith "JS only" and set(v: ResizeArray<Color>): unit = failwith "JS only"
        member __.uvs with get(): ResizeArray<Vector2> = failwith "JS only" and set(v: ResizeArray<Vector2>): unit = failwith "JS only"
        member __.uvs2 with get(): ResizeArray<Vector2> = failwith "JS only" and set(v: ResizeArray<Vector2>): unit = failwith "JS only"
        member __.groups with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.morphTargets with get(): ResizeArray<MorphTarget> = failwith "JS only" and set(v: ResizeArray<MorphTarget>): unit = failwith "JS only"
        member __.skinWeights with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.skinIndices with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.boundingBox with get(): Box3 = failwith "JS only" and set(v: Box3): unit = failwith "JS only"
        member __.boundingSphere with get(): BoundingSphere = failwith "JS only" and set(v: BoundingSphere): unit = failwith "JS only"
        member __.verticesNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.normalsNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.colorsNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.uvsNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.groupsNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.computeBoundingBox(): unit = failwith "JS only"
        member __.computeBoundingSphere(): unit = failwith "JS only"
        member __.computeGroups(geometry: Geometry): unit = failwith "JS only"
        member __.fromGeometry(geometry: Geometry): DirectGeometry = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.addEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.hasEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.removeEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.dispatchEvent(``event``: obj): unit = failwith "JS only"

    and [<Import("EventDispatcher","three")>] EventDispatcher() =
        member __.apply(``object``: obj): unit = failwith "JS only"
        member __.addEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.hasEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.removeEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.dispatchEvent(``event``: obj): unit = failwith "JS only"

    and Event =
        abstract ``type``: string with get, set
        abstract target: obj with get, set

    and [<Import("Face3","three")>] Face3(a: float, b: float, c: float, ?vertexNormals: ResizeArray<Vector3>, ?vertexColors: ResizeArray<Color>, ?materialIndex: float) =
        member __.a with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.b with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.c with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.normal with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.vertexNormals with get(): ResizeArray<Vector3> = failwith "JS only" and set(v: ResizeArray<Vector3>): unit = failwith "JS only"
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.vertexColors with get(): ResizeArray<Color> = failwith "JS only" and set(v: ResizeArray<Color>): unit = failwith "JS only"
        member __.materialIndex with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): Face3 = failwith "JS only"
        member __.copy(source: Face3): Face3 = failwith "JS only"

    and [<Import("Face4","three")>] Face4() =
        inherit Face3(0., 0., 0.)
        
    and MorphTarget =
        abstract name: string with get, set
        abstract vertices: ResizeArray<Vector3> with get, set

    and MorphColor =
        abstract name: string with get, set
        abstract colors: ResizeArray<Color> with get, set

    and MorphNormals =
        abstract name: string with get, set
        abstract normals: ResizeArray<Vector3> with get, set

    and BoundingSphere =
        abstract radius: float with get, set

    and [<Import("Geometry","three")>] Geometry() =
        member __.id with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.vertices with get(): ResizeArray<Vector3> = failwith "JS only" and set(v: ResizeArray<Vector3>): unit = failwith "JS only"
        member __.colors with get(): ResizeArray<Color> = failwith "JS only" and set(v: ResizeArray<Color>): unit = failwith "JS only"
        member __.faces with get(): ResizeArray<Face3> = failwith "JS only" and set(v: ResizeArray<Face3>): unit = failwith "JS only"
        member __.faceVertexUvs with get(): ResizeArray<ResizeArray<ResizeArray<Vector2>>> = failwith "JS only" and set(v: ResizeArray<ResizeArray<ResizeArray<Vector2>>>): unit = failwith "JS only"
        member __.morphTargets with get(): ResizeArray<MorphTarget> = failwith "JS only" and set(v: ResizeArray<MorphTarget>): unit = failwith "JS only"
        member __.morphNormals with get(): ResizeArray<MorphNormals> = failwith "JS only" and set(v: ResizeArray<MorphNormals>): unit = failwith "JS only"
        member __.skinWeights with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.skinIndices with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.lineDistances with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.boundingBox with get(): Box3 = failwith "JS only" and set(v: Box3): unit = failwith "JS only"
        member __.boundingSphere with get(): BoundingSphere = failwith "JS only" and set(v: BoundingSphere): unit = failwith "JS only"
        member __.verticesNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.elementsNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.uvsNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.normalsNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.colorsNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.lineDistancesNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.groupsNeedUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.bones with get(): ResizeArray<Bone> = failwith "JS only" and set(v: ResizeArray<Bone>): unit = failwith "JS only"
        member __.animation with get(): AnimationClip = failwith "JS only" and set(v: AnimationClip): unit = failwith "JS only"
        member __.animations with get(): ResizeArray<AnimationClip> = failwith "JS only" and set(v: ResizeArray<AnimationClip>): unit = failwith "JS only"
        member __.applyMatrix(matrix: Matrix4): Geometry = failwith "JS only"
        member __.rotateX(angle: float): Geometry = failwith "JS only"
        member __.rotateY(angle: float): Geometry = failwith "JS only"
        member __.rotateZ(angle: float): Geometry = failwith "JS only"
        member __.translate(x: float, y: float, z: float): Geometry = failwith "JS only"
        member __.scale(x: float, y: float, z: float): Geometry = failwith "JS only"
        member __.lookAt(vector: Vector3): unit = failwith "JS only"
        member __.fromBufferGeometry(geometry: BufferGeometry): Geometry = failwith "JS only"
        member __.center(): Vector3 = failwith "JS only"
        member __.normalize(): Geometry = failwith "JS only"
        member __.computeFaceNormals(): unit = failwith "JS only"
        member __.computeVertexNormals(?areaWeighted: bool): unit = failwith "JS only"
        member __.computeMorphNormals(): unit = failwith "JS only"
        member __.computeLineDistances(): unit = failwith "JS only"
        member __.computeBoundingBox(): unit = failwith "JS only"
        member __.computeBoundingSphere(): unit = failwith "JS only"
        member __.merge(geometry: Geometry, matrix: Matrix, ?materialIndexOffset: float): unit = failwith "JS only"
        member __.mergeMesh(mesh: Mesh): unit = failwith "JS only"
        member __.mergeVertices(): float = failwith "JS only"
        member __.sortFacesByMaterialIndex(): unit = failwith "JS only"
        member __.toJSON(): obj = failwith "JS only"
        member __.clone(): Geometry = failwith "JS only"
        member __.copy(source: Geometry): Geometry = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.addEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.hasEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.removeEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.dispatchEvent(``event``: obj): unit = failwith "JS only"

    and [<Import("InstancedBufferAttribute","three")>] InstancedBufferAttribute(data: ArrayLike<float>, itemSize: float, ?meshPerAttribute: float) =
        inherit BufferAttribute(unbox data, itemSize)
        member __.meshPerAttribute with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): InstancedBufferAttribute = failwith "JS only"
        member __.copy(source: InstancedBufferAttribute): InstancedBufferAttribute = failwith "JS only"

    and [<Import("InstancedBufferGeometry","three")>] InstancedBufferGeometry() =
        inherit BufferGeometry()
        member __.groups with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.maxInstancedCount with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.addGroup(start: float, count: float, instances: float): unit = failwith "JS only"
        member __.clone(): InstancedBufferGeometry = failwith "JS only"
        member __.copy(source: InstancedBufferGeometry): InstancedBufferGeometry = failwith "JS only"

    and [<Import("InterleavedBuffer","three")>] InterleavedBuffer(array: ArrayLike<float>, stride: float) =
        member __.array with get(): ArrayLike<float> = failwith "JS only" and set(v: ArrayLike<float>): unit = failwith "JS only"
        member __.stride with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.dynamic with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.updateRange with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.version with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.length with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.count with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.needsUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setDynamic(dynamic: bool): InterleavedBuffer = failwith "JS only"
        member __.clone(): InterleavedBuffer = failwith "JS only"
        member __.copy(source: InterleavedBuffer): InterleavedBuffer = failwith "JS only"
        member __.copyAt(index1: float, attribute: InterleavedBufferAttribute, index2: float): InterleavedBuffer = failwith "JS only"
        member __.set(value: ArrayLike<float>, index: float): InterleavedBuffer = failwith "JS only"

    and [<Import("InstancedInterleavedBuffer","three")>] InstancedInterleavedBuffer(array: ArrayLike<float>, stride: float, ?meshPerAttribute: float) =
        inherit InterleavedBuffer(array, stride)
        member __.meshPerAttribute with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): InstancedInterleavedBuffer = failwith "JS only"
        member __.copy(source: InstancedInterleavedBuffer): InstancedInterleavedBuffer = failwith "JS only"

    and [<Import("InterleavedBufferAttribute","three")>] InterleavedBufferAttribute(interleavedBuffer: InterleavedBuffer, itemSize: float, offset: float) =
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.data with get(): InterleavedBuffer = failwith "JS only" and set(v: InterleavedBuffer): unit = failwith "JS only"
        member __.itemSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.offset with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.count with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.length with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.getX(index: float): float = failwith "JS only"
        member __.setX(index: float, x: float): InterleavedBufferAttribute = failwith "JS only"
        member __.getY(index: float): float = failwith "JS only"
        member __.setY(index: float, y: float): InterleavedBufferAttribute = failwith "JS only"
        member __.getZ(index: float): float = failwith "JS only"
        member __.setZ(index: float, z: float): InterleavedBufferAttribute = failwith "JS only"
        member __.getW(index: float): float = failwith "JS only"
        member __.setW(index: float, z: float): InterleavedBufferAttribute = failwith "JS only"
        member __.setXY(index: float, x: float, y: float): InterleavedBufferAttribute = failwith "JS only"
        member __.setXYZ(index: float, x: float, y: float, z: float): InterleavedBufferAttribute = failwith "JS only"
        member __.setXYZW(index: float, x: float, y: float, z: float, w: float): InterleavedBufferAttribute = failwith "JS only"

    and [<Import("Object3D","three")>] Object3D() =
        member __.id with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.parent with get(): Object3D = failwith "JS only" and set(v: Object3D): unit = failwith "JS only"
        member __.children with get(): ResizeArray<Object3D> = failwith "JS only" and set(v: ResizeArray<Object3D>): unit = failwith "JS only"
        member __.up with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.position with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.rotation with get(): Euler = failwith "JS only" and set(v: Euler): unit = failwith "JS only"
        member __.eulerOrder with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.quaternion with get(): Quaternion = failwith "JS only" and set(v: Quaternion): unit = failwith "JS only"
        member __.scale with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.modelViewMatrix with get(): Matrix4 = failwith "JS only" and set(v: Matrix4): unit = failwith "JS only"
        member __.normalMatrix with get(): Matrix3 = failwith "JS only" and set(v: Matrix3): unit = failwith "JS only"
        member __.rotationAutoUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.matrix with get(): Matrix4 = failwith "JS only" and set(v: Matrix4): unit = failwith "JS only"
        member __.matrixWorld with get(): Matrix4 = failwith "JS only" and set(v: Matrix4): unit = failwith "JS only"
        member __.matrixAutoUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.matrixWorldNeedsUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.layers with get(): Layers = failwith "JS only" and set(v: Layers): unit = failwith "JS only"
        member __.visible with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.castShadow with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.receiveShadow with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.frustumCulled with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.renderOrder with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.userData with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.DefaultUp with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.DefaultMatrixAutoUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.applyMatrix(matrix: Matrix4): unit = failwith "JS only"
        member __.setRotationFromAxisAngle(axis: Vector3, angle: float): unit = failwith "JS only"
        member __.setRotationFromEuler(euler: Euler): unit = failwith "JS only"
        member __.setRotationFromMatrix(m: Matrix4): unit = failwith "JS only"
        member __.setRotationFromQuaternion(q: Quaternion): unit = failwith "JS only"
        member __.rotateOnAxis(axis: Vector3, angle: float): Object3D = failwith "JS only"
        member __.rotateX(angle: float): Object3D = failwith "JS only"
        member __.rotateY(angle: float): Object3D = failwith "JS only"
        member __.rotateZ(angle: float): Object3D = failwith "JS only"
        member __.translateOnAxis(axis: Vector3, distance: float): Object3D = failwith "JS only"
        member __.translate(distance: float, axis: Vector3): Object3D = failwith "JS only"
        member __.translateX(distance: float): Object3D = failwith "JS only"
        member __.translateY(distance: float): Object3D = failwith "JS only"
        member __.translateZ(distance: float): Object3D = failwith "JS only"
        member __.localToWorld(vector: Vector3): Vector3 = failwith "JS only"
        member __.worldToLocal(vector: Vector3): Vector3 = failwith "JS only"
        member __.lookAt(vector: Vector3): unit = failwith "JS only"
        member __.add(``object``: Object3D): unit = failwith "JS only"
        member __.remove(``object``: Object3D): unit = failwith "JS only"
        member __.getObjectById(id: float): Object3D = failwith "JS only"
        member __.getObjectByName(name: string): Object3D = failwith "JS only"
        member __.getObjectByProperty(name: string, value: string): Object3D = failwith "JS only"
        member __.getWorldPosition(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.getWorldQuaternion(?optionalTarget: Quaternion): Quaternion = failwith "JS only"
        member __.getWorldRotation(?optionalTarget: Euler): Euler = failwith "JS only"
        member __.getWorldScale(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.getWorldDirection(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.raycast(raycaster: Raycaster, intersects: obj): unit = failwith "JS only"
        member __.traverse(callback: Func<Object3D, obj>): unit = failwith "JS only"
        member __.traverseVisible(callback: Func<Object3D, obj>): unit = failwith "JS only"
        member __.traverseAncestors(callback: Func<Object3D, obj>): unit = failwith "JS only"
        member __.updateMatrix(): unit = failwith "JS only"
        member __.updateMatrixWorld(force: bool): unit = failwith "JS only"
        member __.toJSON(?meta: obj): obj = failwith "JS only"
        member __.clone(?``recursive``: bool): Object3D = failwith "JS only"
        member __.copy(source: Object3D, ?``recursive``: bool): Object3D = failwith "JS only"
        member __.addEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.hasEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.removeEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.dispatchEvent(``event``: obj): unit = failwith "JS only"
        member __.getChildByName(name: string): Object3D = failwith "JS only"

    and Intersection =
        abstract distance: float with get, set
        abstract distanceToRay: float with get, set
        abstract point: Vector3 with get, set
        abstract index: float with get, set
        abstract face: Face3 with get, set
        abstract faceIndex: float with get, set
        abstract ``object``: Object3D with get, set

    and RaycasterParameters =
        abstract Mesh: obj option with get, set
        abstract Line: obj option with get, set
        abstract LOD: obj option with get, set
        abstract Points: obj option with get, set
        abstract Sprite: obj option with get, set

    and [<Import("Raycaster","three")>] Raycaster(?origin: Vector3, ?direction: Vector3, ?near: float, ?far: float) =
        member __.ray with get(): Ray = failwith "JS only" and set(v: Ray): unit = failwith "JS only"
        member __.near with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.far with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.``params`` with get(): RaycasterParameters = failwith "JS only" and set(v: RaycasterParameters): unit = failwith "JS only"
        member __.precision with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.linePrecision with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.set(origin: Vector3, direction: Vector3): unit = failwith "JS only"
        member __.setFromCamera(coords: obj, camera: Camera): unit = failwith "JS only"
        member __.intersectObject(``object``: Object3D, ?``recursive``: bool): ResizeArray<Intersection> = failwith "JS only"
        member __.intersectObjects(objects: ResizeArray<Object3D>, ?``recursive``: bool): ResizeArray<Intersection> = failwith "JS only"

    and [<Import("Layers","three")>] Layers() =
        member __.mask with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.set(channel: float): unit = failwith "JS only"
        member __.enable(channel: float): unit = failwith "JS only"
        member __.toggle(channel: float): unit = failwith "JS only"
        member __.disable(channel: float): unit = failwith "JS only"
        member __.test(layers: Layers): bool = failwith "JS only"

    and [<Import("Font","three")>] Font(data: obj) =
        member __.data with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.generateShapes(text: string, size: float, divisions: float): ResizeArray<obj> = failwith "JS only"

    and [<Import("Light","three")>] Light(?hex: U2<float, string>, ?intensity: float) =
        inherit Object3D()
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.intensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.receiveShadow with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.shadow with get(): LightShadow = failwith "JS only" and set(v: LightShadow): unit = failwith "JS only"
        member __.shadowCameraFov with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowCameraLeft with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowCameraRight with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowCameraTop with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowCameraBottom with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowCameraNear with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowCameraFar with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowBias with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowMapWidth with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowMapHeight with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.copy(source: Light): Light = failwith "JS only"
        member __.clone(?``recursive``: bool): Light = failwith "JS only"

    and [<Import("LightShadow","three")>] LightShadow(camera: Camera) =
        member __.camera with get(): Camera = failwith "JS only" and set(v: Camera): unit = failwith "JS only"
        member __.bias with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.radius with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.mapSize with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.map with get(): RenderTarget = failwith "JS only" and set(v: RenderTarget): unit = failwith "JS only"
        member __.matrix with get(): Matrix4 = failwith "JS only" and set(v: Matrix4): unit = failwith "JS only"
        member __.copy(source: LightShadow): LightShadow = failwith "JS only"
        member __.clone(?``recursive``: bool): LightShadow = failwith "JS only"

    and [<Import("AmbientLight","three")>] AmbientLight(?hex: U2<float, string>, ?intensity: float) =
        inherit Light()
        member __.castShadow with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.copy(source: AmbientLight): AmbientLight = failwith "JS only"
        member __.clone(?``recursive``: bool): AmbientLight = failwith "JS only"

    and [<Import("DirectionalLight","three")>] DirectionalLight(?hex: U2<float, string>, ?intensity: float) =
        inherit Light()
        member __.target with get(): Object3D = failwith "JS only" and set(v: Object3D): unit = failwith "JS only"
        member __.intensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.shadow with get(): LightShadow = failwith "JS only" and set(v: LightShadow): unit = failwith "JS only"
        member __.copy(source: DirectionalLight): DirectionalLight = failwith "JS only"
        member __.clone(?``recursive``: bool): HemisphereLight = failwith "JS only"

    and [<Import("HemisphereLight","three")>] HemisphereLight(?skyColorHex: U2<float, string>, ?groundColorHex: U2<float, string>, ?intensity: float) =
        inherit Light()
        member __.groundColor with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.intensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.copy(source: HemisphereLight): HemisphereLight = failwith "JS only"
        member __.clone(?``recursive``: bool): HemisphereLight = failwith "JS only"

    and [<Import("PointLight","three")>] PointLight(?hex: U2<float, string>, ?intensity: float, ?distance: float, ?decay: float) =
        inherit Light()
        member __.intensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.distance with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.decay with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.shadow with get(): LightShadow = failwith "JS only" and set(v: LightShadow): unit = failwith "JS only"
        member __.power with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.copy(source: PointLight): PointLight = failwith "JS only"
        member __.clone(?``recursive``: bool): PointLight = failwith "JS only"

    and [<Import("SpotLight","three")>] SpotLight(?hex: U2<float, string>, ?intensity: float, ?distance: float, ?angle: float, ?exponent: float, ?decay: float) =
        inherit Light()
        member __.target with get(): Object3D = failwith "JS only" and set(v: Object3D): unit = failwith "JS only"
        member __.intensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.distance with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.angle with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.exponent with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.decay with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.shadow with get(): LightShadow = failwith "JS only" and set(v: LightShadow): unit = failwith "JS only"
        member __.power with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(?``recursive``: bool): SpotLight = failwith "JS only"
        member __.copy(source: PointLight): SpotLight = failwith "JS only"

    and [<Import("Loader","three")>] Loader() =
        member __.onLoadStart with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.onLoadProgress with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.onLoadComplete with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.crossOrigin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.Handlers with get(): LoaderHandler = failwith "JS only" and set(v: LoaderHandler): unit = failwith "JS only"
        member __.extractUrlBase(url: string): string = failwith "JS only"
        member __.initMaterials(materials: ResizeArray<Material>, texturePath: string): ResizeArray<Material> = failwith "JS only"
        member __.createMaterial(m: Material, texturePath: string, ?crossOrigin: string): bool = failwith "JS only"

    and LoaderHandler =
        abstract handlers: ResizeArray<U2<Regex, Loader>> with get, set
        abstract add: regex: Regex * loader: Loader -> unit
        abstract get: file: string -> Loader

    and [<Import("XHRLoader","three")>] XHRLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.path with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.responseType with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.withCredentials with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.load(url: string, ?onLoad: Func<string, unit>, ?onProgress: Func<obj, unit>, ?onError: Func<obj, unit>): obj = failwith "JS only"
        member __.setPath(path: string): unit = failwith "JS only"
        member __.setResponseType(responseType: string): unit = failwith "JS only"
        member __.setWithCredentials(withCredentials: bool): unit = failwith "JS only"

    and [<Import("FontLoader","three")>] FontLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.load(url: string, ?onLoad: Func<string, unit>, ?onProgress: Func<obj, unit>, ?onError: Func<obj, unit>): unit = failwith "JS only"

    and [<Import("ImageLoader","three")>] ImageLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.crossOrigin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.path with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.load(url: string, ?onLoad: Func<HTMLImageElement, unit>, ?onProgress: Func<obj, unit>, ?onError: Func<obj, unit>): HTMLImageElement = failwith "JS only"
        member __.setCrossOrigin(crossOrigin: string): unit = failwith "JS only"
        member __.setPath(value: obj): unit = failwith "JS only"

    and [<Import("JSONLoader","three")>] JSONLoader(?manager: LoadingManager) =
        inherit Loader()
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.withCredentials with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.statusDomElement with get(): HTMLElement = failwith "JS only" and set(v: HTMLElement): unit = failwith "JS only"
        member __.load(url: string, ?onLoad: Func<Geometry, ResizeArray<Material>, unit>, ?onProgress: Func<obj, unit>, ?onError: Func<obj, unit>): unit = failwith "JS only"
        member __.setTexturePath(value: string): unit = failwith "JS only"
        member __.parse(json: obj, ?texturePath: string): obj = failwith "JS only"

    and [<Import("LoadingManager","three")>] LoadingManager(?onLoad: Func<unit>, ?onProgress: Func<string, float, float, unit>, ?onError: Func<unit>) =
        member __.onStart with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.onLoad with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.onProgress with get(): Func<obj, float, float, unit> = failwith "JS only" and set(v: Func<obj, float, float, unit>): unit = failwith "JS only"
        member __.onError with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.itemStart(url: string): unit = failwith "JS only"
        member __.itemEnd(url: string): unit = failwith "JS only"
        member __.itemError(url: string): unit = failwith "JS only"

    and [<Import("BufferGeometryLoader","three")>] BufferGeometryLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.load(url: string, onLoad: Func<BufferGeometry, unit>, ?onProgress: Func<obj, unit>, ?onError: Func<obj, unit>): unit = failwith "JS only"
        member __.parse(json: obj): BufferGeometry = failwith "JS only"

    and [<Import("MaterialLoader","three")>] MaterialLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.textures with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.load(url: string, onLoad: Func<Material, unit>): unit = failwith "JS only"
        member __.setTextures(textures: obj): unit = failwith "JS only"
        member __.getTexture(name: string): Texture = failwith "JS only"
        member __.parse(json: obj): Material = failwith "JS only"

    and [<Import("ObjectLoader","three")>] ObjectLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.texturePass with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.crossOrigin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.load(url: string, ?onLoad: Func<Object3D, unit>): unit = failwith "JS only"
        member __.setTexturePath(value: string): unit = failwith "JS only"
        member __.setCrossOrigin(crossOrigin: string): unit = failwith "JS only"
        member __.parse(json: obj, ?onLoad: Func<Object3D, unit>): 'T = failwith "JS only"
        member __.parseGeometries(json: obj): ResizeArray<obj> = failwith "JS only"
        member __.parseMaterials(json: obj, textures: ResizeArray<Texture>): ResizeArray<Material> = failwith "JS only"
        member __.parseAnimations(json: obj): ResizeArray<AnimationClip> = failwith "JS only"
        member __.parseImages(json: obj, onLoad: Func<unit>): ResizeArray<obj> = failwith "JS only"
        member __.parseTextures(json: obj, images: obj): ResizeArray<Texture> = failwith "JS only"
        member __.parseObject(data: obj, geometries: ResizeArray<obj>, materials: ResizeArray<Material>): 'T = failwith "JS only"

    and [<Import("TextureLoader","three")>] TextureLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.crossOrigin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.path with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.load(url: string, ?onLoad: Func<Texture, unit>): Texture = failwith "JS only"
        member __.setCrossOrigin(crossOrigin: string): unit = failwith "JS only"
        member __.setPath(path: string): unit = failwith "JS only"

    and [<Import("CubeTextureLoader","three")>] CubeTextureLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.corssOrigin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.path with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.load(urls: ResizeArray<string>, ?onLoad: Func<CubeTexture, unit>, ?onProgress: Func<obj, unit>, ?onError: Func<obj, unit>): unit = failwith "JS only"
        member __.setCrossOrigin(crossOrigin: string): unit = failwith "JS only"
        member __.setPath(path: string): unit = failwith "JS only"

    and [<Import("BinaryTextureLoader","three")>] BinaryTextureLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.load(url: string, onLoad: Func<DataTexture, unit>, ?onProgress: Func<obj, unit>, ?onError: Func<obj, unit>): unit = failwith "JS only"

    and [<Import("DataTextureLoader","three")>] DataTextureLoader() =
        inherit BinaryTextureLoader()


    and [<Import("CompressedTextureLoader","three")>] CompressedTextureLoader(?manager: LoadingManager) =
        member __.manager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        member __.path with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.load(url: string, onLoad: Func<CompressedTexture, unit>, ?onProgress: Func<obj, unit>, ?onError: Func<obj, unit>): unit = failwith "JS only"
        member __.setPath(path: string): unit = failwith "JS only"

    and MaterialParameters =
        abstract name: string option with get, set
        abstract side: Side option with get, set
        abstract opacity: float option with get, set
        abstract transparent: bool option with get, set
        abstract blending: Blending option with get, set
        abstract blendSrc: BlendingDstFactor option with get, set
        abstract blendDst: BlendingSrcFactor option with get, set
        abstract blendEquation: BlendingEquation option with get, set
        abstract blendSrcAlpha: float option with get, set
        abstract blendDstAlpha: float option with get, set
        abstract blendEquationAlpha: float option with get, set
        abstract depthFunc: DepthModes option with get, set
        abstract depthTest: bool option with get, set
        abstract depthWrite: bool option with get, set
        abstract colorWrite: bool option with get, set
        abstract precision: float option with get, set
        abstract polygonOffset: bool option with get, set
        abstract polygonOffsetFactor: float option with get, set
        abstract polygonOffsetUnits: float option with get, set
        abstract alphaTest: float option with get, set
        abstract premultipliedAlpha: bool option with get, set
        abstract overdraw: float option with get, set
        abstract visible: bool option with get, set

    and [<Import("Material","three")>] Material() =
        member __.id with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.side with get(): Side = failwith "JS only" and set(v: Side): unit = failwith "JS only"
        member __.opacity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.transparent with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.blending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        member __.blendSrc with get(): BlendingDstFactor = failwith "JS only" and set(v: BlendingDstFactor): unit = failwith "JS only"
        member __.blendDst with get(): BlendingSrcFactor = failwith "JS only" and set(v: BlendingSrcFactor): unit = failwith "JS only"
        member __.blendEquation with get(): BlendingEquation = failwith "JS only" and set(v: BlendingEquation): unit = failwith "JS only"
        member __.blendSrcAlpha with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.blendDstAlpha with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.blendEquationAlpha with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.depthFunc with get(): DepthModes = failwith "JS only" and set(v: DepthModes): unit = failwith "JS only"
        member __.depthTest with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.depthWrite with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.colorWrite with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.precision with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.polygonOffset with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.polygonOffsetFactor with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.polygonOffsetUnits with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.alphaTest with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.premultipliedAlpha with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.overdraw with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.visible with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.needsUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.warpRGB with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.setValues(parameters: MaterialParameters): unit = failwith "JS only"
        member __.toJSON(?meta: obj): obj = failwith "JS only"
        member __.clone(): Material = failwith "JS only"
        member __.copy(source: Material): Material = failwith "JS only"
        member __.update(): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.addEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.hasEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.removeEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.dispatchEvent(``event``: obj): unit = failwith "JS only"

    and LineBasicMaterialParameters =
        inherit MaterialParameters
        abstract color: U2<float, string> option with get, set
        abstract linewidth: float option with get, set
        abstract linecap: string option with get, set
        abstract linejoin: string option with get, set
        abstract blending: Blending option with get, set
        abstract vertexColors: Colors option with get, set
        abstract fog: bool option with get, set

    and [<Import("LineBasicMaterial","three")>] LineBasicMaterial(?parameters: LineBasicMaterialParameters) =
        inherit Material()
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.linewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.linecap with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.linejoin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.blending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        member __.vertexColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        member __.fog with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setValues(parameters: LineBasicMaterialParameters): unit = failwith "JS only"
        member __.clone(): LineBasicMaterial = failwith "JS only"
        member __.copy(source: LineBasicMaterial): LineBasicMaterial = failwith "JS only"

    and LineDashedMaterialParameters =
        inherit MaterialParameters
        abstract color: U2<float, string> option with get, set
        abstract linewidth: float option with get, set
        abstract scale: float option with get, set
        abstract dashSize: float option with get, set
        abstract gapSize: float option with get, set
        abstract blending: Blending option with get, set
        abstract vertexColors: Colors option with get, set
        abstract fog: bool option with get, set

    and [<Import("LineDashedMaterial","three")>] LineDashedMaterial(?parameters: LineDashedMaterialParameters) =
        inherit Material()
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.linewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.scale with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.dashSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.gapSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.blending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        member __.vertexColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        member __.fog with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setValues(parameters: LineDashedMaterialParameters): unit = failwith "JS only"
        member __.clone(): LineDashedMaterial = failwith "JS only"
        member __.copy(source: LineDashedMaterial): LineDashedMaterial = failwith "JS only"

    and MeshBasicMaterialParameters =
        inherit MaterialParameters
        abstract color: U2<float, string> option with get, set
        abstract opacity: float option with get, set
        abstract map: Texture option with get, set
        abstract aoMap: Texture option with get, set
        abstract aoMapIntensity: float option with get, set
        abstract specularMap: Texture option with get, set
        abstract alphaMap: Texture option with get, set
        abstract envMap: Texture option with get, set
        abstract combine: Combine option with get, set
        abstract reflectivity: float option with get, set
        abstract refractionRatio: float option with get, set
        abstract shading: Shading option with get, set
        abstract blending: Blending option with get, set
        abstract wireframe: bool option with get, set
        abstract wireframeLinewidth: float option with get, set
        abstract wireframeLinecap: string option with get, set
        abstract wireframeLinejoin: string option with get, set
        abstract vertexColors: Colors option with get, set
        abstract skinning: bool option with get, set
        abstract morphTargets: bool option with get, set
        abstract fog: bool option with get, set

    and [<Import("MeshBasicMaterial","three")>] MeshBasicMaterial(?parameters: MeshBasicMaterialParameters) =
        inherit Material()
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.map with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.aoMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.aoMapIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.specularMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.alphaMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.envMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.combine with get(): Combine = failwith "JS only" and set(v: Combine): unit = failwith "JS only"
        member __.reflectivity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.refractionRatio with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.fog with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.shading with get(): Shading = failwith "JS only" and set(v: Shading): unit = failwith "JS only"
        member __.blending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        member __.wireframe with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.wireframeLinewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.wireframeLinecap with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.wireframeLinejoin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.vertexColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        member __.skinning with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.morphTargets with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setValues(parameters: MeshBasicMaterialParameters): unit = failwith "JS only"
        member __.clone(): MeshBasicMaterial = failwith "JS only"
        member __.copy(source: MeshBasicMaterial): MeshBasicMaterial = failwith "JS only"

    and MeshDepthMaterialParameters =
        inherit MaterialParameters
        abstract wireframe: bool option with get, set
        abstract wireframeLinewidth: float option with get, set

    and [<Import("MeshDepthMaterial","three")>] MeshDepthMaterial(?parameters: MeshDepthMaterialParameters) =
        inherit Material()
        member __.wireframe with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.wireframeLinewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.setValues(parameters: MeshDepthMaterialParameters): unit = failwith "JS only"
        member __.clone(): MeshDepthMaterial = failwith "JS only"
        member __.copy(source: MeshDepthMaterial): MeshDepthMaterial = failwith "JS only"

    and MeshLambertMaterialParameters =
        inherit MaterialParameters
        abstract color: U2<float, string> option with get, set
        abstract emissive: U2<float, string> option with get, set
        abstract emissiveIntensity: float option with get, set
        abstract emissiveMap: Texture option with get, set
        abstract map: Texture option with get, set
        abstract lighhtMap: Texture option with get, set
        abstract lightMapIntensity: float option with get, set
        abstract aoMap: Texture option with get, set
        abstract aoMapIntensity: float option with get, set
        abstract specularMap: Texture option with get, set
        abstract alphaMap: Texture option with get, set
        abstract envMap: Texture option with get, set
        abstract combine: Combine option with get, set
        abstract reflectivity: float option with get, set
        abstract refractionRatio: float option with get, set
        abstract fog: bool option with get, set
        abstract blending: Blending option with get, set
        abstract wireframe: bool option with get, set
        abstract wireframeLinewidth: float option with get, set
        abstract wireframeLinecap: string option with get, set
        abstract wireframeLinejoin: string option with get, set
        abstract vertexColors: Colors option with get, set
        abstract skinning: bool option with get, set
        abstract morphTargets: bool option with get, set
        abstract morphNormals: bool option with get, set

    and [<Import("MeshLambertMaterial","three")>] MeshLambertMaterial(?parameters: MeshLambertMaterialParameters) =
        inherit Material()
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.emissive with get(): U2<float, string> = failwith "JS only" and set(v: U2<float, string>): unit = failwith "JS only"
        member __.emissiveIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.emissiveMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.map with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.lighhtMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.lightMapIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.aoMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.aoMapIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.specularMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.alphaMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.envMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.combine with get(): Combine = failwith "JS only" and set(v: Combine): unit = failwith "JS only"
        member __.reflectivity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.refractionRatio with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.fog with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.blending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        member __.wireframe with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.wireframeLinewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.wireframeLinecap with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.wireframeLinejoin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.vertexColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        member __.skinning with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.morphTargets with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.morphNormals with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setValues(parameters: MeshLambertMaterialParameters): unit = failwith "JS only"
        member __.clone(): MeshLambertMaterial = failwith "JS only"
        member __.copy(source: MeshLambertMaterial): MeshLambertMaterial = failwith "JS only"

    and MeshStandardMaterialParameters =
        inherit MaterialParameters
        abstract color: U2<float, string> option with get, set
        abstract roughness: float option with get, set
        abstract metalness: float option with get, set
        abstract map: Texture option with get, set
        abstract lighhtMap: Texture option with get, set
        abstract lightMapIntensity: float option with get, set
        abstract aoMap: Texture option with get, set
        abstract aoMapIntensity: float option with get, set
        abstract emissive: Color option with get, set
        abstract emissiveIntensity: float option with get, set
        abstract emissiveMap: Texture option with get, set
        abstract bumpMap: Texture option with get, set
        abstract bumpScale: float option with get, set
        abstract normalMap: Texture option with get, set
        abstract normalScale: float option with get, set
        abstract displacementMap: Texture option with get, set
        abstract displacementScale: float option with get, set
        abstract displacementBias: float option with get, set
        abstract roughnessMap: Texture option with get, set
        abstract metalMap: Texture option with get, set
        abstract alphaMap: Texture option with get, set
        abstract envMap: Texture option with get, set
        abstract envMapIntensity: float option with get, set
        abstract refractionRatio: float option with get, set
        abstract shading: Shading option with get, set
        abstract blending: Blending option with get, set
        abstract wireframe: bool option with get, set
        abstract wireframeLinewidth: float option with get, set
        abstract vertexColors: Colors option with get, set
        abstract skinning: bool option with get, set
        abstract morphTargets: bool option with get, set
        abstract morphNormals: bool option with get, set
        abstract fog: bool option with get, set

    and [<Import("MeshStandardMaterial","three")>] MeshStandardMaterial(?parameters: MeshStandardMaterialParameters) =
        inherit Material()
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.roughness with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.metalness with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.map with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.lighhtMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.lightMapIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.aoMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.aoMapIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.emissive with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.emissiveIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.emissiveMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.bumpMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.bumpScale with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.normalMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.normalScale with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.displacementMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.displacementScale with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.displacementBias with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.roughnessMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.metalMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.alphaMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.envMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.envMapIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.refractionRatio with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.shading with get(): Shading = failwith "JS only" and set(v: Shading): unit = failwith "JS only"
        member __.blending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        member __.wireframe with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.wireframeLinewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.vertexColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        member __.skinning with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.morphTargets with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.morphNormals with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.fog with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setValues(parameters: MeshStandardMaterialParameters): unit = failwith "JS only"
        member __.clone(): MeshStandardMaterial = failwith "JS only"
        member __.copy(source: MeshStandardMaterial): MeshStandardMaterial = failwith "JS only"

    and MeshNormalMaterialParameters =
        inherit MaterialParameters
        abstract wireframe: bool option with get, set
        abstract wireframeLinewidth: float option with get, set
        abstract morphTargets: bool option with get, set

    and [<Import("MeshNormalMaterial","three")>] MeshNormalMaterial(?parameters: MeshNormalMaterialParameters) =
        inherit Material()
        member __.wireframe with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.wireframeLinewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.morphTargets with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setValues(parameters: MeshNormalMaterialParameters): unit = failwith "JS only"
        member __.clone(): MeshNormalMaterial = failwith "JS only"
        member __.copy(source: MeshNormalMaterial): MeshNormalMaterial = failwith "JS only"

    and MeshPhongMaterialParameters =
        inherit MaterialParameters
        abstract color: U2<float, string> option with get, set
        abstract specular: float option with get, set
        abstract shininess: float option with get, set
        abstract opacity: float option with get, set
        abstract map: Texture option with get, set
        abstract lightMap: Texture option with get, set
        abstract lightMapIntensity: float option with get, set
        abstract aoMap: Texture option with get, set
        abstract aoMapIntensity: float option with get, set
        abstract emissive: float option with get, set
        abstract emissiveIntensity: float option with get, set
        abstract emissiveMap: Texture option with get, set
        abstract bumpMap: Texture option with get, set
        abstract bumpScale: float option with get, set
        abstract normalMap: Texture option with get, set
        abstract normalScale: Vector2 option with get, set
        abstract displacementMap: Texture option with get, set
        abstract displacementScale: float option with get, set
        abstract displacementBias: float option with get, set
        abstract specularMap: Texture option with get, set
        abstract alphaMap: Texture option with get, set
        abstract envMap: Texture option with get, set
        abstract combine: Combine option with get, set
        abstract reflectivity: float option with get, set
        abstract refractionRatio: float option with get, set
        abstract shading: Shading option with get, set
        abstract blending: Blending option with get, set
        abstract wireframe: bool option with get, set
        abstract wireframeLinewidth: float option with get, set
        abstract wireframeLinecap: string option with get, set
        abstract wireframeLinejoin: string option with get, set
        abstract vertexColors: Colors option with get, set
        abstract skinning: bool option with get, set
        abstract morphTargets: bool option with get, set
        abstract morphNormals: bool option with get, set
        abstract fog: bool option with get, set

    and [<Import("MeshPhongMaterial","three")>] MeshPhongMaterial(?parameters: MeshPhongMaterialParameters) =
        inherit Material()
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.specular with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.shininess with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.map with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.lightMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.lightMapIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.aoMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.aoMapIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.emissive with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.emissiveIntensity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.emissiveMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.bumpMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.bumpScale with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.normalMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.normalScale with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.displacementMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.displacementScale with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.displacementBias with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.specularMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.alphaMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.envMap with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.combine with get(): Combine = failwith "JS only" and set(v: Combine): unit = failwith "JS only"
        member __.reflectivity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.refractionRatio with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.fog with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.shading with get(): Shading = failwith "JS only" and set(v: Shading): unit = failwith "JS only"
        member __.blending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        member __.wireframe with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.wireframeLinewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.wireframeLinecap with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.wireframeLinejoin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.vertexColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        member __.skinning with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.morphTargets with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.morphNormals with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.metal with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setValues(parameters: MeshPhongMaterialParameters): unit = failwith "JS only"
        member __.clone(): MeshPhongMaterial = failwith "JS only"
        member __.copy(source: MeshPhongMaterial): MeshPhongMaterial = failwith "JS only"

    and [<Import("MultiMaterial","three")>] MultiMaterial(?materials: ResizeArray<Material>) =
        inherit Material()
        member __.materials with get(): ResizeArray<Material> = failwith "JS only" and set(v: ResizeArray<Material>): unit = failwith "JS only"
        member __.toJSON(meta: obj): obj = failwith "JS only"
        member __.clone(): MultiMaterial = failwith "JS only"

    and [<Import("MeshFaceMaterial","three")>] MeshFaceMaterial() =
        inherit MultiMaterial()


    and PointsMaterialParameters =
        inherit MaterialParameters
        abstract color: U2<float, string> option with get, set
        abstract map: Texture option with get, set
        abstract size: float option with get, set
        abstract sizeAttenuation: bool option with get, set
        abstract blending: Blending option with get, set
        abstract vertexColors: Colors option with get, set
        abstract fog: bool option with get, set

    and [<Import("PointsMaterial","three")>] PointsMaterial(?parameters: PointsMaterialParameters) =
        inherit Material()
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.map with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.size with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.sizeAttenuation with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.blending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        member __.vertexColors with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.fog with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setValues(parameters: PointsMaterialParameters): unit = failwith "JS only"
        member __.clone(): PointsMaterial = failwith "JS only"
        member __.copy(source: PointsMaterial): PointsMaterial = failwith "JS only"

    and [<Import("PointCloudMaterial","three")>] PointCloudMaterial() =
        inherit PointsMaterial()


    and [<Import("ParticleBasicMaterial","three")>] ParticleBasicMaterial() =
        inherit PointsMaterial()


    and [<Import("ParticleSystemMaterial","three")>] ParticleSystemMaterial() =
        inherit PointsMaterial()


    and ShaderMaterialParameters =
        inherit MaterialParameters
        abstract defines: obj option with get, set
        abstract uniforms: obj option with get, set
        abstract vertexShader: string option with get, set
        abstract fragmentShader: string option with get, set
        abstract shading: Shading option with get, set
        abstract lineWidth: float option with get, set
        abstract wireframe: bool option with get, set
        abstract wireframeLinewidth: float option with get, set
        abstract fog: bool option with get, set
        abstract lights: bool option with get, set
        abstract vertexColors: Colors option with get, set
        abstract skinning: bool option with get, set
        abstract morphTargets: bool option with get, set
        abstract morphNormals: bool option with get, set

    and [<Import("ShaderMaterial","three")>] ShaderMaterial(?parameters: ShaderMaterialParameters) =
        inherit Material()
        member __.defines with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.uniforms with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.vertexShader with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.fragmentShader with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.shading with get(): Shading = failwith "JS only" and set(v: Shading): unit = failwith "JS only"
        member __.linewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.wireframe with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.wireframeLinewidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.fog with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.lights with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.vertexColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        member __.skinning with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.morphTargets with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.morphNormals with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.derivatives with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.extensions with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.defaultAttributeValues with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.index0AttributeName with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.setValues(parameters: ShaderMaterialParameters): unit = failwith "JS only"
        member __.clone(): ShaderMaterial = failwith "JS only"
        member __.copy(source: ShaderMaterial): ShaderMaterial = failwith "JS only"
        member __.toJSON(meta: obj): obj = failwith "JS only"

    and [<Import("RawShaderMaterial","three")>] RawShaderMaterial(?parameters: ShaderMaterialParameters) =
        inherit ShaderMaterial()


    and SpriteMaterialParameters =
        inherit MaterialParameters
        abstract color: U2<float, string> option with get, set
        abstract map: Texture option with get, set
        abstract rotation: float option with get, set
        abstract fog: bool option with get, set

    and [<Import("SpriteMaterial","three")>] SpriteMaterial(?parameters: SpriteMaterialParameters) =
        inherit Material()
        member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.map with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.rotation with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.fog with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setValues(parameters: SpriteMaterialParameters): unit = failwith "JS only"
        member __.clone(): SpriteMaterial = failwith "JS only"
        member __.copy(source: SpriteMaterial): SpriteMaterial = failwith "JS only"

    and [<Import("Box2","three")>] Box2(?min: Vector2, ?max: Vector2) =
        member __.max with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.min with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.set(min: Vector2, max: Vector2): Box2 = failwith "JS only"
        member __.setFromPoints(points: ResizeArray<Vector2>): Box2 = failwith "JS only"
        member __.setFromCenterAndSize(center: Vector2, size: Vector2): Box2 = failwith "JS only"
        member __.clone(): Box2 = failwith "JS only"
        member __.copy(box: Box2): Box2 = failwith "JS only"
        member __.makeEmpty(): Box2 = failwith "JS only"
        member __.empty(): obj = failwith "JS only"
        member __.isEmpty(): bool = failwith "JS only"
        member __.center(?optionalTarget: Vector2): Vector2 = failwith "JS only"
        member __.size(?optionalTarget: Vector2): Vector2 = failwith "JS only"
        member __.expandByPoint(point: Vector2): Box2 = failwith "JS only"
        member __.expandByVector(vector: Vector2): Box2 = failwith "JS only"
        member __.expandByScalar(scalar: float): Box2 = failwith "JS only"
        member __.containsPoint(point: Vector2): bool = failwith "JS only"
        member __.containsBox(box: Box2): bool = failwith "JS only"
        member __.getParameter(point: Vector2): Vector2 = failwith "JS only"
        member __.intersectsBox(box: Box2): bool = failwith "JS only"
        member __.clampPoint(point: Vector2, ?optionalTarget: Vector2): Vector2 = failwith "JS only"
        member __.distanceToPoint(point: Vector2): float = failwith "JS only"
        member __.intersect(box: Box2): Box2 = failwith "JS only"
        member __.union(box: Box2): Box2 = failwith "JS only"
        member __.translate(offset: Vector2): Box2 = failwith "JS only"
        member __.equals(box: Box2): bool = failwith "JS only"
        member __.isIntersectionBox(b: obj): obj = failwith "JS only"

    and [<Import("Box3","three")>] Box3(?min: Vector3, ?max: Vector3) =
        member __.max with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.min with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.set(min: Vector3, max: Vector3): Box3 = failwith "JS only"
        member __.setFromArray(array: ResizeArray<float>): Box3 = failwith "JS only"
        member __.setFromPoints(points: ResizeArray<Vector3>): Box3 = failwith "JS only"
        member __.setFromCenterAndSize(center: Vector3, size: Vector3): Box3 = failwith "JS only"
        member __.setFromObject(``object``: Object3D): Box3 = failwith "JS only"
        member __.clone(): Box3 = failwith "JS only"
        member __.copy(box: Box3): Box3 = failwith "JS only"
        member __.makeEmpty(): Box3 = failwith "JS only"
        member __.isEmpty(): bool = failwith "JS only"
        member __.center(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.size(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.expandByPoint(point: Vector3): Box3 = failwith "JS only"
        member __.expandByVector(vector: Vector3): Box3 = failwith "JS only"
        member __.expandByScalar(scalar: float): Box3 = failwith "JS only"
        member __.containsPoint(point: Vector3): bool = failwith "JS only"
        member __.containsBox(box: Box3): bool = failwith "JS only"
        member __.getParameter(point: Vector3): Vector3 = failwith "JS only"
        member __.intersectsBox(box: Box3): bool = failwith "JS only"
        member __.intersectsSphere(sphere: Sphere): bool = failwith "JS only"
        member __.intersectsPlane(plane: Plane): bool = failwith "JS only"
        member __.clampPoint(point: Vector3, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.distanceToPoint(point: Vector3): float = failwith "JS only"
        member __.getBoundingSphere(?optionalTarget: Sphere): Sphere = failwith "JS only"
        member __.intersect(box: Box3): Box3 = failwith "JS only"
        member __.union(box: Box3): Box3 = failwith "JS only"
        member __.applyMatrix4(matrix: Matrix4): Box3 = failwith "JS only"
        member __.translate(offset: Vector3): Box3 = failwith "JS only"
        member __.equals(box: Box3): bool = failwith "JS only"
        member __.empty(): obj = failwith "JS only"
        member __.isIntersectionBox(b: obj): obj = failwith "JS only"
        member __.isIntersectionSphere(s: obj): obj = failwith "JS only"

    and HSL =
        abstract h: float with get, set
        abstract s: float with get, set
        abstract l: float with get, set

    and [<Import("Color","three")>] Color(r: float, g: float, b: float) =
        member __.r with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.g with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.b with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.set(color: Color): Color = failwith "JS only"
        member __.set(color: float): Color = failwith "JS only"
        member __.set(color: string): Color = failwith "JS only"
        member __.setScalar(scalar: float): Color = failwith "JS only"
        member __.setHex(hex: float): Color = failwith "JS only"
        member __.setRGB(r: float, g: float, b: float): Color = failwith "JS only"
        member __.setHSL(h: float, s: float, l: float): Color = failwith "JS only"
        member __.setStyle(style: string): Color = failwith "JS only"
        member __.clone(): Color = failwith "JS only"
        member __.copy(color: Color): Color = failwith "JS only"
        member __.copyGammaToLinear(color: Color, ?gammaFactor: float): Color = failwith "JS only"
        member __.copyLinearToGamma(color: Color, ?gammaFactor: float): Color = failwith "JS only"
        member __.convertGammaToLinear(): Color = failwith "JS only"
        member __.convertLinearToGamma(): Color = failwith "JS only"
        member __.getHex(): float = failwith "JS only"
        member __.getHexString(): string = failwith "JS only"
        member __.getHSL(): HSL = failwith "JS only"
        member __.getStyle(): string = failwith "JS only"
        member __.offsetHSL(h: float, s: float, l: float): Color = failwith "JS only"
        member __.add(color: Color): Color = failwith "JS only"
        member __.addColors(color1: Color, color2: Color): Color = failwith "JS only"
        member __.addScalar(s: float): Color = failwith "JS only"
        member __.multiply(color: Color): Color = failwith "JS only"
        member __.multiplyScalar(s: float): Color = failwith "JS only"
        member __.lerp(color: Color, alpha: float): Color = failwith "JS only"
        member __.equals(color: Color): bool = failwith "JS only"
        member __.fromArray(rgb: ResizeArray<float>, ?offset: float): Color = failwith "JS only"
        member __.toArray(?array: ResizeArray<float>, ?offset: float): ResizeArray<float> = failwith "JS only"

    and [<Import("Euler","three")>] Euler(?x: float, ?y: float, ?z: float, ?order: string) =
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.z with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.order with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.onChangeCallback with get(): Function = failwith "JS only" and set(v: Function): unit = failwith "JS only"
        member __.RotationOrders with get(): ResizeArray<string> = failwith "JS only" and set(v: ResizeArray<string>): unit = failwith "JS only"
        member __.DefaultOrder with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.set(x: float, y: float, z: float, ?order: string): Euler = failwith "JS only"
        member __.clone(): Euler = failwith "JS only"
        member __.copy(euler: Euler): Euler = failwith "JS only"
        member __.setFromRotationMatrix(m: Matrix4, ?order: string, ?update: bool): Euler = failwith "JS only"
        member __.setFromQuaternion(q: Quaternion, ?order: string, ?update: bool): Euler = failwith "JS only"
        member __.setFromVector3(v: Vector3, ?order: string): Euler = failwith "JS only"
        member __.reorder(newOrder: string): Euler = failwith "JS only"
        member __.equals(euler: Euler): bool = failwith "JS only"
        member __.fromArray(xyzo: ResizeArray<obj>): Euler = failwith "JS only"
        member __.toArray(?array: ResizeArray<float>, ?offset: float): ResizeArray<float> = failwith "JS only"
        member __.toVector3(?optionalResult: Vector3): Vector3 = failwith "JS only"
        member __.onChange(callback: Function): unit = failwith "JS only"

    and [<Import("Frustum","three")>] Frustum(?p0: Plane, ?p1: Plane, ?p2: Plane, ?p3: Plane, ?p4: Plane, ?p5: Plane) =
        member __.planes with get(): ResizeArray<Plane> = failwith "JS only" and set(v: ResizeArray<Plane>): unit = failwith "JS only"
        member __.set(?p0: float, ?p1: float, ?p2: float, ?p3: float, ?p4: float, ?p5: float): Frustum = failwith "JS only"
        member __.clone(): Frustum = failwith "JS only"
        member __.copy(frustum: Frustum): Frustum = failwith "JS only"
        member __.setFromMatrix(m: Matrix4): Frustum = failwith "JS only"
        member __.intersectsObject(``object``: Object3D): bool = failwith "JS only"
        member __.intersectsSphere(sphere: Sphere): bool = failwith "JS only"
        member __.intersectsBox(box: Box3): bool = failwith "JS only"
        member __.containsPoint(point: Vector3): bool = failwith "JS only"

    and [<Import("Line3","three")>] Line3(?start: Vector3, ?``end``: Vector3) =
        member __.start with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.``end`` with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.set(?start: Vector3, ?``end``: Vector3): Line3 = failwith "JS only"
        member __.clone(): Line3 = failwith "JS only"
        member __.copy(line: Line3): Line3 = failwith "JS only"
        member __.center(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.delta(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.distanceSq(): float = failwith "JS only"
        member __.distance(): float = failwith "JS only"
        member __.at(t: float, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.closestPointToPointParameter(point: Vector3, ?clampToLine: bool): float = failwith "JS only"
        member __.closestPointToPoint(point: Vector3, ?clampToLine: bool, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.applyMatrix4(matrix: Matrix4): Line3 = failwith "JS only"
        member __.equals(line: Line3): bool = failwith "JS only"

    and Matrix =
        abstract elements: Float32Array with get, set
        abstract identity: unit -> Matrix
        abstract copy: m: Matrix -> Matrix
        abstract multiplyScalar: s: float -> Matrix
        abstract determinant: unit -> float
        abstract getInverse: matrix: Matrix * ?throwOnInvertible: bool -> Matrix
        abstract transpose: unit -> Matrix
        abstract clone: unit -> Matrix

    and [<Import("Matrix3","three")>] Matrix3() =
        interface Matrix with
            member __.elements with get(): Float32Array = failwith "JS only" and set(v: Float32Array): unit = failwith "JS only"
            member __.identity(): Matrix = failwith "JS only"
            member __.copy(m: Matrix): Matrix = failwith "JS only"
            member __.multiplyScalar(s: float): Matrix = failwith "JS only"
            member __.determinant(): float = failwith "JS only"
            member __.getInverse(matrix: Matrix, ?throwOnInvertible: bool): Matrix = failwith "JS only"
            member __.transpose(): Matrix = failwith "JS only"
            member __.clone(): Matrix = failwith "JS only"
        member __.set(n11: float, n12: float, n13: float, n21: float, n22: float, n23: float, n31: float, n32: float, n33: float): Matrix3 = failwith "JS only"
        member __.copy(m: Matrix3): Matrix3 = failwith "JS only"
        member __.setFromMatix4(m: Matrix4): Matrix3 = failwith "JS only"
        member __.multiplyVector3Array(a: obj): obj = failwith "JS only"
        member __.applyToVector3Array(array: ResizeArray<float>, ?offset: float, ?length: float): ResizeArray<float> = failwith "JS only"
        member __.applyToBuffer(buffer: BufferAttribute, ?offset: float, ?length: float): BufferAttribute = failwith "JS only"
        member __.getInverse(matrix: Matrix3, ?throwOnDegenerate: bool): Matrix3 = failwith "JS only"
        member __.getInverse(matrix: Matrix4, ?throwOnDegenerate: bool): Matrix3 = failwith "JS only"
        member __.flattenToArrayOffset(array: ResizeArray<float>, offset: float): ResizeArray<float> = failwith "JS only"
        member __.getNormalMatrix(matrix4: Matrix4): Matrix3 = failwith "JS only"
        member __.transposeIntoArray(r: ResizeArray<float>): ResizeArray<float> = failwith "JS only"
        member __.fromArray(array: ResizeArray<float>): Matrix3 = failwith "JS only"
        member __.toArray(): ResizeArray<float> = failwith "JS only"
        member __.multiplyVector3(vector: Vector3): obj = failwith "JS only"

    and [<Import("Matrix4","three")>] Matrix4() =
        interface Matrix with
            member __.elements with get(): Float32Array = failwith "JS only" and set(v: Float32Array): unit = failwith "JS only"
            member __.identity(): Matrix = failwith "JS only"
            member __.copy(m: Matrix): Matrix = failwith "JS only"
            member __.multiplyScalar(s: float): Matrix = failwith "JS only"
            member __.determinant(): float = failwith "JS only"
            member __.getInverse(matrix: Matrix, ?throwOnInvertible: bool): Matrix = failwith "JS only"
            member __.transpose(): Matrix = failwith "JS only"
            member __.clone(): Matrix = failwith "JS only"
        member __.set(n11: float, n12: float, n13: float, n14: float, n21: float, n22: float, n23: float, n24: float, n31: float, n32: float, n33: float, n34: float, n41: float, n42: float, n43: float, n44: float): Matrix4 = failwith "JS only"
        member __.copy(m: Matrix4): Matrix4 = failwith "JS only"
        member __.extractPosition(m: Matrix4): Matrix4 = failwith "JS only"
        member __.copyPosition(m: Matrix4): Matrix4 = failwith "JS only"
        member __.extractBasis(xAxis: Vector3, yAxis: Vector3, zAxis: Vector3): Matrix4 = failwith "JS only"
        member __.makeBasis(xAxis: Vector3, yAxis: Vector3, zAxis: Vector3): Matrix4 = failwith "JS only"
        member __.extractRotation(m: Matrix4): Matrix4 = failwith "JS only"
        member __.makeRotationFromEuler(euler: Euler): Matrix4 = failwith "JS only"
        member __.setRotationFromQuaternion(q: Quaternion): Matrix4 = failwith "JS only"
        member __.makeRotationFromQuaternion(q: Quaternion): Matrix4 = failwith "JS only"
        member __.lookAt(eye: Vector3, target: Vector3, up: Vector3): Matrix4 = failwith "JS only"
        member __.multiply(m: Matrix4): Matrix4 = failwith "JS only"
        member __.multiplyMatrices(a: Matrix4, b: Matrix4): Matrix4 = failwith "JS only"
        member __.multiplyToArray(a: Matrix4, b: Matrix4, r: ResizeArray<float>): Matrix4 = failwith "JS only"
        member __.multiplyVector3Array(array: ResizeArray<float>): ResizeArray<float> = failwith "JS only"
        member __.applyToVector3Array(array: ResizeArray<float>, ?offset: float, ?length: float): ResizeArray<float> = failwith "JS only"
        member __.applyToBuffer(buffer: BufferAttribute, ?offset: float, ?length: float): BufferAttribute = failwith "JS only"
        member __.flattenToArrayOffset(array: ResizeArray<float>, offset: float): ResizeArray<float> = failwith "JS only"
        member __.setPosition(v: Vector3): Matrix4 = failwith "JS only"
        member __.getInverse(m: Matrix4, ?throwOnDegeneratee: bool): Matrix4 = failwith "JS only"
        member __.scale(v: Vector3): Matrix4 = failwith "JS only"
        member __.getMaxScaleOnAxis(): float = failwith "JS only"
        member __.makeTranslation(x: float, y: float, z: float): Matrix4 = failwith "JS only"
        member __.makeRotationX(theta: float): Matrix4 = failwith "JS only"
        member __.makeRotationY(theta: float): Matrix4 = failwith "JS only"
        member __.makeRotationZ(theta: float): Matrix4 = failwith "JS only"
        member __.makeRotationAxis(axis: Vector3, angle: float): Matrix4 = failwith "JS only"
        member __.makeScale(x: float, y: float, z: float): Matrix4 = failwith "JS only"
        member __.compose(translation: Vector3, rotation: Quaternion, scale: Vector3): Matrix4 = failwith "JS only"
        member __.decompose(?translation: Vector3, ?rotation: Quaternion, ?scale: Vector3): ResizeArray<obj> = failwith "JS only"
        member __.makeFrustum(left: float, right: float, bottom: float, top: float, near: float, far: float): Matrix4 = failwith "JS only"
        member __.makePerspective(fov: float, aspect: float, near: float, far: float): Matrix4 = failwith "JS only"
        member __.makeOrthographic(left: float, right: float, top: float, bottom: float, near: float, far: float): Matrix4 = failwith "JS only"
        member __.equals(matrix: Matrix4): bool = failwith "JS only"
        member __.fromArray(array: ResizeArray<float>): Matrix4 = failwith "JS only"
        member __.toArray(): ResizeArray<float> = failwith "JS only"
        member __.getPosition(): obj = failwith "JS only"
        member __.multiplyVector3(v: obj): obj = failwith "JS only"
        member __.multiplyVector4(v: obj): obj = failwith "JS only"
        member __.rotateAxis(v: obj): unit = failwith "JS only"
        member __.crossVector(v: obj): unit = failwith "JS only"

    and [<Import("Plane","three")>] Plane(?normal: Vector3, ?constant: float) =
        member __.normal with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.constant with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.set(normal: Vector3, constant: float): Plane = failwith "JS only"
        member __.setComponents(x: float, y: float, z: float, w: float): Plane = failwith "JS only"
        member __.setFromNormalAndCoplanarPoint(normal: Vector3, point: Vector3): Plane = failwith "JS only"
        member __.setFromCoplanarPoints(a: Vector3, b: Vector3, c: Vector3): Plane = failwith "JS only"
        member __.clone(): Plane = failwith "JS only"
        member __.copy(plane: Plane): Plane = failwith "JS only"
        member __.normalize(): Plane = failwith "JS only"
        member __.negate(): Plane = failwith "JS only"
        member __.distanceToPoint(point: Vector3): float = failwith "JS only"
        member __.distanceToSphere(sphere: Sphere): float = failwith "JS only"
        member __.projectPoint(point: Vector3, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.orthoPoint(point: Vector3, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.intersectLine(line: Line3, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.intersectsLine(line: Line3): bool = failwith "JS only"
        member __.intersectsBox(box: Box3): bool = failwith "JS only"
        member __.coplanarPoint(?optionalTarget: bool): Vector3 = failwith "JS only"
        member __.applyMatrix4(matrix: Matrix4, ?optionalNormalMatrix: Matrix3): Plane = failwith "JS only"
        member __.translate(offset: Vector3): Plane = failwith "JS only"
        member __.equals(plane: Plane): bool = failwith "JS only"
        member __.isIntersectionLine(l: obj): obj = failwith "JS only"

    and [<Import("Spherical","three")>] Spherical(?radius: float, ?phi: float, ?theta: float) =
        member __.set(radius: float, phi: float, theta: float): unit = failwith "JS only"
        member __.clone(): Spherical = failwith "JS only"
        member __.copy(other: Spherical): Spherical = failwith "JS only"
        member __.makeSafe(): unit = failwith "JS only"
        member __.setFromVector3(vec3: Vector3): Spherical = failwith "JS only"

    and [<Import("Quaternion","three")>] Quaternion(?x: float, ?y: float, ?z: float, ?w: float) =
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.z with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.w with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.onChangeCallback with get(): Function = failwith "JS only" and set(v: Function): unit = failwith "JS only"
        member __.set(x: float, y: float, z: float, w: float): Quaternion = failwith "JS only"
        member __.clone(): Quaternion = failwith "JS only"
        member __.copy(q: Quaternion): Quaternion = failwith "JS only"
        member __.setFromEuler(euler: Euler, ?update: bool): Quaternion = failwith "JS only"
        member __.setFromAxisAngle(axis: Vector3, angle: float): Quaternion = failwith "JS only"
        member __.setFromRotationMatrix(m: Matrix4): Quaternion = failwith "JS only"
        member __.setFromUnitVectors(vFrom: Vector3, vTo: Vector3): Quaternion = failwith "JS only"
        member __.inverse(): Quaternion = failwith "JS only"
        member __.conjugate(): Quaternion = failwith "JS only"
        member __.dot(v: Vector3): float = failwith "JS only"
        member __.lengthSq(): float = failwith "JS only"
        member __.length(): float = failwith "JS only"
        member __.normalize(): Quaternion = failwith "JS only"
        member __.multiply(q: Quaternion): Quaternion = failwith "JS only"
        member __.multiplyQuaternions(a: Quaternion, b: Quaternion): Quaternion = failwith "JS only"
        member __.multiplyVector3(v: obj): obj = failwith "JS only"
        member __.slerp(qb: Quaternion, t: float): Quaternion = failwith "JS only"
        member __.equals(v: Quaternion): bool = failwith "JS only"
        member __.fromArray(n: ResizeArray<float>): Quaternion = failwith "JS only"
        member __.toArray(): ResizeArray<float> = failwith "JS only"
        member __.fromArray(xyzw: ResizeArray<float>, ?offset: float): Quaternion = failwith "JS only"
        member __.toArray(?xyzw: ResizeArray<float>, ?offset: float): ResizeArray<float> = failwith "JS only"
        member __.onChange(callback: Function): Quaternion = failwith "JS only"
        static member slerp(qa: Quaternion, qb: Quaternion, qm: Quaternion, t: float): Quaternion = failwith "JS only"
        static member slerpFlat(dst: ResizeArray<float>, dstOffset: float, src0: ResizeArray<float>, srcOffset: float, src1: ResizeArray<float>, stcOffset1: float, t: float): Quaternion = failwith "JS only"

    and [<Import("Ray","three")>] Ray(?origin: Vector3, ?direction: Vector3) =
        member __.origin with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.direction with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.set(origin: Vector3, direction: Vector3): Ray = failwith "JS only"
        member __.clone(): Ray = failwith "JS only"
        member __.copy(ray: Ray): Ray = failwith "JS only"
        member __.at(t: float, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.lookAt(v: Vector3): Vector3 = failwith "JS only"
        member __.recast(t: float): Ray = failwith "JS only"
        member __.closestPointToPoint(point: Vector3, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.distanceToPoint(point: Vector3): float = failwith "JS only"
        member __.distanceSqToPoint(point: Vector3): float = failwith "JS only"
        member __.distanceSqToSegment(v0: Vector3, v1: Vector3, ?optionalPointOnRay: Vector3, ?optionalPointOnSegment: Vector3): float = failwith "JS only"
        member __.intersectSphere(sphere: Sphere, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.intersectsSphere(sphere: Sphere): bool = failwith "JS only"
        member __.distanceToPlane(plane: Plane): float = failwith "JS only"
        member __.intersectPlane(plane: Plane, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.intersectsPlane(plane: Plane): bool = failwith "JS only"
        member __.intersectBox(box: Box3, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.intersectsBox(box: Box3): bool = failwith "JS only"
        member __.intersectTriangle(a: Vector3, b: Vector3, c: Vector3, backfaceCulling: bool, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.applyMatrix4(matrix4: Matrix4): Ray = failwith "JS only"
        member __.equals(ray: Ray): bool = failwith "JS only"
        member __.isIntersectionSphere(s: obj): obj = failwith "JS only"
        member __.isIntersectionPlane(p: obj): obj = failwith "JS only"
        member __.isIntersectionBox(b: obj): obj = failwith "JS only"

    and [<Import("Sphere","three")>] Sphere(?center: Vector3, ?radius: float) =
        member __.center with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.radius with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.set(center: Vector3, radius: float): Sphere = failwith "JS only"
        member __.setFromPoints(points: ResizeArray<Vector3>, ?optionalCenter: Vector3): Sphere = failwith "JS only"
        member __.clone(): Sphere = failwith "JS only"
        member __.copy(sphere: Sphere): Sphere = failwith "JS only"
        member __.empty(): bool = failwith "JS only"
        member __.containsPoint(point: Vector3): bool = failwith "JS only"
        member __.distanceToPoint(point: Vector3): float = failwith "JS only"
        member __.intersectsSphere(sphere: Sphere): bool = failwith "JS only"
        member __.intersectsBox(box: Box3): bool = failwith "JS only"
        member __.intersectsPlane(plane: Plane): bool = failwith "JS only"
        member __.clampPoint(point: Vector3, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.getBoundingBox(?optionalTarget: Box3): Box3 = failwith "JS only"
        member __.applyMatrix4(matrix: Matrix4): Sphere = failwith "JS only"
        member __.translate(offset: Vector3): Sphere = failwith "JS only"
        member __.equals(sphere: Sphere): bool = failwith "JS only"

    and SplineControlPoint =
        abstract x: float with get, set
        abstract y: float with get, set
        abstract z: float with get, set

    and [<Import("Spline","three")>] Spline(points: ResizeArray<SplineControlPoint>) =
        member __.points with get(): ResizeArray<SplineControlPoint> = failwith "JS only" and set(v: ResizeArray<SplineControlPoint>): unit = failwith "JS only"
        member __.initFromArray(a: ResizeArray<ResizeArray<float>>): unit = failwith "JS only"
        member __.getPoint(k: float): SplineControlPoint = failwith "JS only"
        member __.getControlPointsArray(): ResizeArray<ResizeArray<float>> = failwith "JS only"
        member __.getLength(?nSubDivisions: float): obj = failwith "JS only"
        member __.reparametrizeByArcLength(samplingCoef: float): unit = failwith "JS only"

    and [<Import("Triangle","three")>] Triangle(?a: Vector3, ?b: Vector3, ?c: Vector3) =
        member __.a with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.b with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.c with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.set(a: Vector3, b: Vector3, c: Vector3): Triangle = failwith "JS only"
        member __.setFromPointsAndIndices(points: ResizeArray<Vector3>, i0: float, i1: float, i2: float): Triangle = failwith "JS only"
        member __.clone(): Triangle = failwith "JS only"
        member __.copy(triangle: Triangle): Triangle = failwith "JS only"
        member __.area(): float = failwith "JS only"
        member __.midpoint(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.normal(?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.plane(?optionalTarget: Vector3): Plane = failwith "JS only"
        member __.barycoordFromPoint(point: Vector3, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        member __.containsPoint(point: Vector3): bool = failwith "JS only"
        member __.equals(triangle: Triangle): bool = failwith "JS only"
        static member normal(a: Vector3, b: Vector3, c: Vector3, ?optionalTarget: Vector3): Vector3 = failwith "JS only"
        static member barycoordFromPoint(point: Vector3, a: Vector3, b: Vector3, c: Vector3, optionalTarget: Vector3): Vector3 = failwith "JS only"
        static member containsPoint(point: Vector3, a: Vector3, b: Vector3, c: Vector3): bool = failwith "JS only"

    and Vector =
        abstract setComponent: index: float * value: float -> unit
        abstract getComponent: index: float -> float
        abstract copy: v: Vector -> Vector
        abstract add: v: Vector -> Vector
        abstract addVectors: a: Vector * b: Vector -> Vector
        abstract sub: v: Vector -> Vector
        abstract subVectors: a: Vector * b: Vector -> Vector
        abstract multiplyScalar: s: float -> Vector
        abstract divideScalar: s: float -> Vector
        abstract negate: unit -> Vector
        abstract dot: v: Vector -> float
        abstract lengthSq: unit -> float
        abstract length: unit -> float
        abstract normalize: unit -> Vector
        abstract distanceTo: v: Vector -> float
        abstract distanceToSquared: v: Vector -> float
        abstract setLength: l: float -> Vector
        abstract lerp: v: Vector * alpha: float -> Vector
        abstract equals: v: Vector -> bool
        abstract clone: unit -> Vector

    and [<Import("Vector2","three")>] Vector2(?x: float, ?y: float) =
        interface Vector with
            member __.setComponent(index: float, value: float): unit = failwith "JS only"
            member __.getComponent(index: float): float = failwith "JS only"
            member __.copy(v: Vector): Vector = failwith "JS only"
            member __.add(v: Vector): Vector = failwith "JS only"
            member __.addVectors(a: Vector, b: Vector): Vector = failwith "JS only"
            member __.sub(v: Vector): Vector = failwith "JS only"
            member __.subVectors(a: Vector, b: Vector): Vector = failwith "JS only"
            member __.multiplyScalar(s: float): Vector = failwith "JS only"
            member __.divideScalar(s: float): Vector = failwith "JS only"
            member __.negate(): Vector = failwith "JS only"
            member __.dot(v: Vector): float = failwith "JS only"
            member __.lengthSq(): float = failwith "JS only"
            member __.length(): float = failwith "JS only"
            member __.normalize(): Vector = failwith "JS only"
            member __.distanceTo(v: Vector): float = failwith "JS only"
            member __.distanceToSquared(v: Vector): float = failwith "JS only"
            member __.setLength(l: float): Vector = failwith "JS only"
            member __.lerp(v: Vector, alpha: float): Vector = failwith "JS only"
            member __.equals(v: Vector): bool = failwith "JS only"
            member __.clone(): Vector = failwith "JS only"
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.set(x: float, y: float): Vector2 = failwith "JS only"
        member __.setScalar(scalar: float): Vector2 = failwith "JS only"
        member __.setX(x: float): Vector2 = failwith "JS only"
        member __.setY(y: float): Vector2 = failwith "JS only"
        member __.copy(v: Vector2): Vector2 = failwith "JS only"
        member __.add(v: Vector2): Vector2 = failwith "JS only"
        member __.addScalar(s: float): Vector2 = failwith "JS only"
        member __.addVectors(a: Vector2, b: Vector2): Vector2 = failwith "JS only"
        member __.addScaledVector(v: Vector2, s: float): Vector2 = failwith "JS only"
        member __.sub(v: Vector2): Vector2 = failwith "JS only"
        member __.subVectors(a: Vector2, b: Vector2): Vector2 = failwith "JS only"
        member __.multiply(v: Vector2): Vector2 = failwith "JS only"
        member __.divide(v: Vector2): Vector2 = failwith "JS only"
        member __.min(v: Vector2): Vector2 = failwith "JS only"
        member __.max(v: Vector2): Vector2 = failwith "JS only"
        member __.clamp(min: Vector2, max: Vector2): Vector2 = failwith "JS only"
        member __.clampScalar(min: float, max: float): Vector2 = failwith "JS only"
        member __.clampLength(min: float, max: float): Vector2 = failwith "JS only"
        member __.floor(): Vector2 = failwith "JS only"
        member __.ceil(): Vector2 = failwith "JS only"
        member __.round(): Vector2 = failwith "JS only"
        member __.roundToZero(): Vector2 = failwith "JS only"
        member __.dot(v: Vector2): float = failwith "JS only"
        member __.lengthManhattan(): float = failwith "JS only"
        member __.angle(): float = failwith "JS only"
        member __.distanceTo(v: Vector2): float = failwith "JS only"
        member __.distanceToSquared(v: Vector2): float = failwith "JS only"
        member __.lerp(v: Vector2, alpha: float): Vector2 = failwith "JS only"
        member __.lerpVectors(v1: Vector2, v2: Vector2, alpha: float): Vector2 = failwith "JS only"
        member __.equals(v: Vector2): bool = failwith "JS only"
        member __.fromArray(xy: ResizeArray<float>, ?offset: float): Vector2 = failwith "JS only"
        member __.toArray(?xy: ResizeArray<float>, ?offset: float): ResizeArray<float> = failwith "JS only"
        member __.fromAttribute(attribute: BufferAttribute, index: float, ?offset: float): Vector2 = failwith "JS only"
        member __.rotateAround(center: Vector2, angle: float): Vector2 = failwith "JS only"

    and [<Import("Vector3","three")>] Vector3(?x: float, ?y: float, ?z: float) =
        interface Vector with
            member __.setComponent(index: float, value: float): unit = failwith "JS only"
            member __.getComponent(index: float): float = failwith "JS only"
            member __.copy(v: Vector): Vector = failwith "JS only"
            member __.add(v: Vector): Vector = failwith "JS only"
            member __.addVectors(a: Vector, b: Vector): Vector = failwith "JS only"
            member __.sub(v: Vector): Vector = failwith "JS only"
            member __.subVectors(a: Vector, b: Vector): Vector = failwith "JS only"
            member __.multiplyScalar(s: float): Vector = failwith "JS only"
            member __.divideScalar(s: float): Vector = failwith "JS only"
            member __.negate(): Vector = failwith "JS only"
            member __.dot(v: Vector): float = failwith "JS only"
            member __.lengthSq(): float = failwith "JS only"
            member __.length(): float = failwith "JS only"
            member __.normalize(): Vector = failwith "JS only"
            member __.distanceTo(v: Vector): float = failwith "JS only"
            member __.distanceToSquared(v: Vector): float = failwith "JS only"
            member __.setLength(l: float): Vector = failwith "JS only"
            member __.lerp(v: Vector, alpha: float): Vector = failwith "JS only"
            member __.equals(v: Vector): bool = failwith "JS only"
            member __.clone(): Vector = failwith "JS only"
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.z with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.set(x: float, y: float, z: float): Vector3 = failwith "JS only"
        member __.setScalar(scalar: float): Vector3 = failwith "JS only"
        member __.setX(x: float): Vector3 = failwith "JS only"
        member __.setY(y: float): Vector3 = failwith "JS only"
        member __.setZ(z: float): Vector3 = failwith "JS only"
        member __.copy(v: Vector3): Vector3 = failwith "JS only"
        member __.add(a: Vector3): Vector3 = failwith "JS only"
        member __.addScalar(s: float): Vector3 = failwith "JS only"
        member __.addScaledVector(v: Vector3, s: float): Vector3 = failwith "JS only"
        member __.addVectors(a: Vector3, b: Vector3): Vector3 = failwith "JS only"
        member __.sub(a: Vector3): Vector3 = failwith "JS only"
        member __.subScalar(s: float): Vector3 = failwith "JS only"
        member __.subVectors(a: Vector3, b: Vector3): Vector3 = failwith "JS only"
        member __.multiply(v: Vector3): Vector3 = failwith "JS only"
        member __.multiplyVectors(a: Vector3, b: Vector3): Vector3 = failwith "JS only"
        member __.applyEuler(euler: Euler): Vector3 = failwith "JS only"
        member __.applyAxisAngle(axis: Vector3, angle: float): Vector3 = failwith "JS only"
        member __.applyMatrix3(m: Matrix3): Vector3 = failwith "JS only"
        member __.applyMatrix4(m: Matrix4): Vector3 = failwith "JS only"
        member __.applyProjection(m: Matrix4): Vector3 = failwith "JS only"
        member __.applyQuaternion(q: Quaternion): Vector3 = failwith "JS only"
        member __.project(camrea: Camera): Vector3 = failwith "JS only"
        member __.unproject(camera: Camera): Vector3 = failwith "JS only"
        member __.transformDirection(m: Matrix4): Vector3 = failwith "JS only"
        member __.divide(v: Vector3): Vector3 = failwith "JS only"
        member __.min(v: Vector3): Vector3 = failwith "JS only"
        member __.max(v: Vector3): Vector3 = failwith "JS only"
        member __.clamp(min: Vector3, max: Vector3): Vector3 = failwith "JS only"
        member __.clampScalar(min: float, max: float): Vector3 = failwith "JS only"
        member __.clampLength(min: float, max: float): Vector3 = failwith "JS only"
        member __.floor(): Vector3 = failwith "JS only"
        member __.ceil(): Vector3 = failwith "JS only"
        member __.round(): Vector3 = failwith "JS only"
        member __.roundToZero(): Vector3 = failwith "JS only"
        member __.dot(v: Vector3): float = failwith "JS only"
        member __.lengthManhattan(): float = failwith "JS only"
        member __.lerp(v: Vector3, alpha: float): Vector3 = failwith "JS only"
        member __.lerpVectors(v1: Vector3, v2: Vector3, alpha: float): Vector3 = failwith "JS only"
        member __.cross(a: Vector3): Vector3 = failwith "JS only"
        member __.crossVectors(a: Vector3, b: Vector3): Vector3 = failwith "JS only"
        member __.projectOnVector(v: Vector3): Vector3 = failwith "JS only"
        member __.projectOnPlane(planeNormal: Vector3): Vector3 = failwith "JS only"
        member __.reflect(vector: Vector3): Vector3 = failwith "JS only"
        member __.angleTo(v: Vector3): float = failwith "JS only"
        member __.distanceTo(v: Vector3): float = failwith "JS only"
        member __.distanceToSquared(v: Vector3): float = failwith "JS only"
        member __.setFromSpherical(s: Spherical): Matrix3 = failwith "JS only"
        member __.getPositionFromMatrix(m: Matrix4): Vector3 = failwith "JS only"
        member __.setFromMatrixPosition(m: Matrix4): Vector3 = failwith "JS only"
        member __.getScaleFromMatrix(m: Matrix4): Vector3 = failwith "JS only"
        member __.setFromMatrixScale(m: Matrix4): Vector3 = failwith "JS only"
        member __.getColumnFromMatrixColumn(index: float, matrix: Matrix4): Vector3 = failwith "JS only"
        member __.setFromMatrixColumn(matrix: Matrix4, index: float): Vector3 = failwith "JS only"
        member __.setFromMatrixColumn(index: float, matrix: Matrix4): Vector3 = failwith "JS only"
        member __.equals(v: Vector3): bool = failwith "JS only"
        member __.fromArray(xyz: ResizeArray<float>, ?offset: float): Vector3 = failwith "JS only"
        member __.toArray(?xyz: ResizeArray<float>, ?offset: float): ResizeArray<float> = failwith "JS only"
        member __.fromAttribute(attribute: BufferAttribute, index: float, ?offset: float): Vector3 = failwith "JS only"

    and [<Import("Vertex","three")>] Vertex() =
        inherit Vector3()


    and [<Import("Vector4","three")>] Vector4(?x: float, ?y: float, ?z: float, ?w: float) =
        interface Vector with
            member __.setComponent(index: float, value: float): unit = failwith "JS only"
            member __.getComponent(index: float): float = failwith "JS only"
            member __.copy(v: Vector): Vector = failwith "JS only"
            member __.add(v: Vector): Vector = failwith "JS only"
            member __.addVectors(a: Vector, b: Vector): Vector = failwith "JS only"
            member __.sub(v: Vector): Vector = failwith "JS only"
            member __.subVectors(a: Vector, b: Vector): Vector = failwith "JS only"
            member __.multiplyScalar(s: float): Vector = failwith "JS only"
            member __.divideScalar(s: float): Vector = failwith "JS only"
            member __.negate(): Vector = failwith "JS only"
            member __.dot(v: Vector): float = failwith "JS only"
            member __.lengthSq(): float = failwith "JS only"
            member __.length(): float = failwith "JS only"
            member __.normalize(): Vector = failwith "JS only"
            member __.distanceTo(v: Vector): float = failwith "JS only"
            member __.distanceToSquared(v: Vector): float = failwith "JS only"
            member __.setLength(l: float): Vector = failwith "JS only"
            member __.lerp(v: Vector, alpha: float): Vector = failwith "JS only"
            member __.equals(v: Vector): bool = failwith "JS only"
            member __.clone(): Vector = failwith "JS only"
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.z with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.w with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.set(x: float, y: float, z: float, w: float): Vector4 = failwith "JS only"
        member __.setScalar(scalar: float): Vector4 = failwith "JS only"
        member __.setX(x: float): Vector4 = failwith "JS only"
        member __.setY(y: float): Vector4 = failwith "JS only"
        member __.setZ(z: float): Vector4 = failwith "JS only"
        member __.setW(w: float): Vector4 = failwith "JS only"
        member __.copy(v: Vector4): Vector4 = failwith "JS only"
        member __.add(v: Vector4): Vector4 = failwith "JS only"
        member __.addScalar(s: float): Vector4 = failwith "JS only"
        member __.addVectors(a: Vector4, b: Vector4): Vector4 = failwith "JS only"
        member __.addScaledVector(v: Vector4, s: float): Vector4 = failwith "JS only"
        member __.sub(v: Vector4): Vector4 = failwith "JS only"
        member __.subScalar(s: float): Vector4 = failwith "JS only"
        member __.subVectors(a: Vector4, b: Vector4): Vector4 = failwith "JS only"
        member __.applyMatrix4(m: Matrix4): Vector4 = failwith "JS only"
        member __.setAxisAngleFromQuaternion(q: Quaternion): Vector4 = failwith "JS only"
        member __.setAxisAngleFromRotationMatrix(m: Matrix3): Vector4 = failwith "JS only"
        member __.min(v: Vector4): Vector4 = failwith "JS only"
        member __.max(v: Vector4): Vector4 = failwith "JS only"
        member __.clamp(min: Vector4, max: Vector4): Vector4 = failwith "JS only"
        member __.clampScalar(min: float, max: float): Vector4 = failwith "JS only"
        member __.floor(): Vector4 = failwith "JS only"
        member __.ceil(): Vector4 = failwith "JS only"
        member __.round(): Vector4 = failwith "JS only"
        member __.roundToZero(): Vector4 = failwith "JS only"
        member __.dot(v: Vector4): float = failwith "JS only"
        member __.lengthManhattan(): float = failwith "JS only"
        member __.lerp(v: Vector4, alpha: float): Vector4 = failwith "JS only"
        member __.lerpVectors(v1: Vector4, v2: Vector4, alpha: float): Vector4 = failwith "JS only"
        member __.equals(v: Vector4): bool = failwith "JS only"
        member __.fromArray(xyzw: ResizeArray<float>, ?offset: float): Vector4 = failwith "JS only"
        member __.toArray(?xyzw: ResizeArray<float>, ?offset: float): ResizeArray<float> = failwith "JS only"
        member __.fromAttribute(attribute: BufferAttribute, index: float, ?offset: float): Vector4 = failwith "JS only"

    and [<Import("Interpolant","three")>] Interpolant(parameterPositions: obj, samplesValues: obj, sampleSize: float, ?resultBuffer: obj) =
        member __.parameterPositions with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.samplesValues with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.valueSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.resultBuffer with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.evaluate(time: float): obj = failwith "JS only"

    and [<Import("CubicInterpolant","three")>] CubicInterpolant(parameterPositions: obj, samplesValues: obj, sampleSize: float, ?resultBuffer: obj) =
        inherit Interpolant(parameterPositions, samplesValues, sampleSize, ?resultBuffer=resultBuffer)
        member __.interpolate_(i1: float, t0: float, t: float, t1: float): obj = failwith "JS only"

    and [<Import("DiscreteInterpolant","three")>] DiscreteInterpolant(parameterPositions: obj, samplesValues: obj, sampleSize: float, ?resultBuffer: obj) =
        inherit Interpolant(parameterPositions, samplesValues, sampleSize, ?resultBuffer=resultBuffer)
        member __.interpolate_(i1: float, t0: float, t: float, t1: float): obj = failwith "JS only"

    and [<Import("LinearInterpolant","three")>] LinearInterpolant(parameterPositions: obj, samplesValues: obj, sampleSize: float, ?resultBuffer: obj) =
        inherit Interpolant(parameterPositions, samplesValues, sampleSize, ?resultBuffer=resultBuffer)
        member __.interpolate_(i1: float, t0: float, t: float, t1: float): obj = failwith "JS only"

    and [<Import("QuaternionLinearInterpolant","three")>] QuaternionLinearInterpolant(parameterPositions: obj, samplesValues: obj, sampleSize: float, ?resultBuffer: obj) =
        inherit Interpolant(parameterPositions, samplesValues, sampleSize, ?resultBuffer=resultBuffer)
        member __.interpolate_(i1: float, t0: float, t: float, t1: float): obj = failwith "JS only"

    and [<Import("Bone","three")>] Bone(skin: SkinnedMesh) =
        inherit Object3D()
        member __.skin with get(): SkinnedMesh = failwith "JS only" and set(v: SkinnedMesh): unit = failwith "JS only"
        member __.clone(): Bone = failwith "JS only"
        member __.copy(source: Bone): Bone = failwith "JS only"

    and [<Import("Group","three")>] Group() =
        inherit Object3D()


    and [<Import("LOD","three")>] LOD() =
        inherit Object3D()
        member __.levels with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.objects with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.addLevel(``object``: Object3D, ?distance: float): unit = failwith "JS only"
        member __.getObjectForDistance(distance: float): Object3D = failwith "JS only"
        member __.raycast(raycaster: Raycaster, intersects: obj): unit = failwith "JS only"
        member __.update(camera: Camera): unit = failwith "JS only"
        member __.clone(): LOD = failwith "JS only"
        member __.copy(source: LOD): LOD = failwith "JS only"
        member __.toJSON(meta: obj): obj = failwith "JS only"

    and LensFlareProperty =
        abstract texture: Texture with get, set
        abstract size: float with get, set
        abstract distance: float with get, set
        abstract x: float with get, set
        abstract y: float with get, set
        abstract z: float with get, set
        abstract scale: float with get, set
        abstract rotation: float with get, set
        abstract opacity: float with get, set
        abstract color: Color with get, set
        abstract blending: Blending with get, set

    and [<Import("LensFlare","three")>] LensFlare(?texture: Texture, ?size: float, ?distance: float, ?blending: Blending, ?color: Color) =
        inherit Object3D()
        member __.lensFlares with get(): ResizeArray<LensFlareProperty> = failwith "JS only" and set(v: ResizeArray<LensFlareProperty>): unit = failwith "JS only"
        member __.positionScreen with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.customUpdateCallback with get(): Func<LensFlare, unit> = failwith "JS only" and set(v: Func<LensFlare, unit>): unit = failwith "JS only"
        member __.add(``object``: Object3D): unit = failwith "JS only"
        member __.add(texture: Texture, ?size: float, ?distance: float, ?blending: Blending, ?color: Color): unit = failwith "JS only"
        member __.updateLensFlares(): unit = failwith "JS only"
        member __.clone(): LensFlare = failwith "JS only"
        member __.copy(source: LensFlare): LensFlare = failwith "JS only"

    and [<Import("Line","three")>] Line(?geometry: U2<Geometry, BufferGeometry>, ?material: U3<LineDashedMaterial, LineBasicMaterial, ShaderMaterial>, ?mode: float) =
        inherit Object3D()
        member __.geometry with get(): U2<Geometry, BufferGeometry> = failwith "JS only" and set(v: U2<Geometry, BufferGeometry>): unit = failwith "JS only"
        member __.material with get(): Material = failwith "JS only" and set(v: Material): unit = failwith "JS only"
        member __.raycast(raycaster: Raycaster, intersects: obj): unit = failwith "JS only"
        member __.clone(): Line = failwith "JS only"
        member __.copy(source: Line): Line = failwith "JS only"

    and [<Import("LineSegments","three")>] LineSegments(?geometry: U2<Geometry, BufferGeometry>, ?material: U3<LineDashedMaterial, LineBasicMaterial, ShaderMaterial>, ?mode: float) =
        inherit Line()
        member __.clone(): LineSegments = failwith "JS only"
        member __.copy(source: LineSegments): LineSegments = failwith "JS only"

    and LineMode =
        interface end

    and [<Import("Mesh","three")>] Mesh(?geometry: BufferGeometry, ?material: Material) =
        inherit Object3D()
        member __.geometry with get(): U2<Geometry, BufferGeometry> = failwith "JS only" and set(v: U2<Geometry, BufferGeometry>): unit = failwith "JS only"
        member __.material with get(): Material = failwith "JS only" and set(v: Material): unit = failwith "JS only"
        member __.drawMode with get(): TrianglesDrawModes = failwith "JS only" and set(v: TrianglesDrawModes): unit = failwith "JS only"
        member __.setDrawMode(drawMode: TrianglesDrawModes): unit = failwith "JS only"
        member __.updateMorphTargets(): unit = failwith "JS only"
        member __.getMorphTargetIndexByName(name: string): float = failwith "JS only"
        member __.raycast(raycaster: Raycaster, intersects: obj): unit = failwith "JS only"
        member __.clone(): Mesh = failwith "JS only"
        member __.copy(source: Mesh): Mesh = failwith "JS only"

    and [<Import("Points","three")>] Points(?geometry: U2<Geometry, BufferGeometry>, ?material: Material) =
        inherit Object3D()
        member __.geometry with get(): U2<Geometry, BufferGeometry> = failwith "JS only" and set(v: U2<Geometry, BufferGeometry>): unit = failwith "JS only"
        member __.material with get(): Material = failwith "JS only" and set(v: Material): unit = failwith "JS only"
        member __.raycast(raycaster: Raycaster, intersects: obj): unit = failwith "JS only"
        member __.clone(): Points = failwith "JS only"
        member __.copy(source: Points): Points = failwith "JS only"

    and [<Import("PointCloud","three")>] PointCloud() =
        inherit Points()


    and [<Import("ParticleSystem","three")>] ParticleSystem() =
        inherit Points()


    and [<Import("Skeleton","three")>] Skeleton(bones: ResizeArray<Bone>, ?boneInverses: ResizeArray<Matrix4>, ?useVertexTexture: bool) =
        member __.useVertexTexture with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.identityMatrix with get(): Matrix4 = failwith "JS only" and set(v: Matrix4): unit = failwith "JS only"
        member __.bones with get(): ResizeArray<Bone> = failwith "JS only" and set(v: ResizeArray<Bone>): unit = failwith "JS only"
        member __.boneTextureWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.boneTextureHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.boneMatrices with get(): Float32Array = failwith "JS only" and set(v: Float32Array): unit = failwith "JS only"
        member __.boneTexture with get(): DataTexture = failwith "JS only" and set(v: DataTexture): unit = failwith "JS only"
        member __.boneInverses with get(): ResizeArray<Matrix4> = failwith "JS only" and set(v: ResizeArray<Matrix4>): unit = failwith "JS only"
        member __.calculateInverses(bone: Bone): unit = failwith "JS only"
        member __.pose(): unit = failwith "JS only"
        member __.update(): unit = failwith "JS only"
        member __.clone(): Skeleton = failwith "JS only"

    and [<Import("SkinnedMesh","three")>] SkinnedMesh(?geometry: U2<Geometry, BufferGeometry>, ?material: ShaderMaterial, ?useVertexTexture: bool) =
        inherit Mesh()
        member __.bindMode with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.bindMatrix with get(): Matrix4 = failwith "JS only" and set(v: Matrix4): unit = failwith "JS only"
        member __.bindMatrixInverse with get(): Matrix4 = failwith "JS only" and set(v: Matrix4): unit = failwith "JS only"
        member __.skeleton with get(): Skeleton = failwith "JS only" and set(v: Skeleton): unit = failwith "JS only"
        member __.bind(skeleton: Skeleton, ?bindMatrix: Matrix4): unit = failwith "JS only"
        member __.pose(): unit = failwith "JS only"
        member __.normalizeSkinWeights(): unit = failwith "JS only"
        member __.updateMatrixWorld(?force: bool): unit = failwith "JS only"
        member __.clone(): SkinnedMesh = failwith "JS only"
        member __.copy(source: SkinnedMesh): SkinnedMesh = failwith "JS only"

    and [<Import("Sprite","three")>] Sprite(?material: Material) =
        inherit Object3D()
        member __.geometry with get(): BufferGeometry = failwith "JS only" and set(v: BufferGeometry): unit = failwith "JS only"
        member __.material with get(): SpriteMaterial = failwith "JS only" and set(v: SpriteMaterial): unit = failwith "JS only"
        member __.raycast(raycaster: Raycaster, intersects: obj): unit = failwith "JS only"
        member __.clone(): Sprite = failwith "JS only"
        member __.copy(source: Sprite): Sprite = failwith "JS only"

    and [<Import("Particle","three")>] Particle() =
        inherit Sprite()


    and Renderer =
        abstract domElement: HTMLCanvasElement with get, set
        abstract render: scene: Scene * camera: Camera -> unit
        abstract setSize: width: float * height: float * ?updateStyle: bool -> unit

    and WebGLRendererParameters =
        abstract canvas: HTMLCanvasElement option with get, set
        abstract precision: string option with get, set
        abstract alpha: bool option with get, set
        abstract premultipliedAlpha: bool option with get, set
        abstract antialias: bool option with get, set
        abstract stencil: bool option with get, set
        abstract preserveDrawingBuffer: bool option with get, set
        abstract clearColor: float option with get, set
        abstract clearAlpha: float option with get, set
        abstract devicePixelRatio: float option with get, set
        abstract logarithmicDepthBuffer: bool option with get, set

    and [<Import("WebGLRenderer","three")>] WebGLRenderer(?parameters: WebGLRendererParameters) =
        interface Renderer with
            member __.domElement with get(): HTMLCanvasElement = failwith "JS only" and set(v: HTMLCanvasElement): unit = failwith "JS only"
            member __.render(scene: Scene, camera: Camera): unit = failwith "JS only"
            member __.setSize(width: float, height: float, ?updateStyle: bool): unit = failwith "JS only"
        member __.context with get(): WebGLRenderingContext = failwith "JS only" and set(v: WebGLRenderingContext): unit = failwith "JS only"
        member __.autoClear with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.autoClearColor with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.autoClearDepth with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.autoClearStencil with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.sortObjects with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.extensions with get(): WebGLExtensions = failwith "JS only" and set(v: WebGLExtensions): unit = failwith "JS only"
        member __.gammaFactor with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.gammaInput with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.gammaOutput with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.physicallyCorrectLights with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.toneMapping with get(): ToneMapping = failwith "JS only" and set(v: ToneMapping): unit = failwith "JS only"
        member __.toneMappingExposure with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.toneMappingWhitePoint with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.shadowMapDebug with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.maxMorphTargets with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.maxMorphNormals with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.autoScaleCubemaps with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.info with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.shadowMap with get(): WebGLShadowMap = failwith "JS only" and set(v: WebGLShadowMap): unit = failwith "JS only"
        member __.shadowMapType with get(): ShadowMapType = failwith "JS only" and set(v: ShadowMapType): unit = failwith "JS only"
        member __.shadowMapEnabled with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.shadowMapCullFace with get(): CullFace = failwith "JS only" and set(v: CullFace): unit = failwith "JS only"
        member __.pixelRation with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.capabilities with get(): WebGLCapabilities = failwith "JS only" and set(v: WebGLCapabilities): unit = failwith "JS only"
        member __.properties with get(): WebGLProperties = failwith "JS only" and set(v: WebGLProperties): unit = failwith "JS only"
        member __.state with get(): WebGLState = failwith "JS only" and set(v: WebGLState): unit = failwith "JS only"
        member __.getContext(): WebGLRenderingContext = failwith "JS only"
        member __.getContextAttributes(): obj = failwith "JS only"
        member __.forceContextLoss(): unit = failwith "JS only"
        member __.getMaxAnisotropy(): float = failwith "JS only"
        member __.getPrecision(): string = failwith "JS only"
        member __.getPixelRatio(): float = failwith "JS only"
        member __.setPixelRatio(value: float): unit = failwith "JS only"
        member __.getSize(): obj = failwith "JS only"
        member __.setViewport(?x: float, ?y: float, ?width: float, ?height: float): unit = failwith "JS only"
        member __.setScissor(x: float, y: float, width: float, height: float): unit = failwith "JS only"
        member __.setScissorTest(enable: bool): unit = failwith "JS only"
        member __.getClearColor(): Color = failwith "JS only"
        member __.setClearColor(color: Color, ?alpha: float): unit = failwith "JS only"
        member __.setClearColor(color: string, ?alpha: float): unit = failwith "JS only"
        member __.setClearColor(color: float, ?alpha: float): unit = failwith "JS only"
        member __.getClearAlpha(): float = failwith "JS only"
        member __.setClearAlpha(alpha: float): unit = failwith "JS only"
        member __.clear(?color: bool, ?depth: bool, ?stencil: bool): unit = failwith "JS only"
        member __.clearColor(): unit = failwith "JS only"
        member __.clearDepth(): unit = failwith "JS only"
        member __.clearStencil(): unit = failwith "JS only"
        member __.clearTarget(renderTarget: WebGLRenderTarget, color: bool, depth: bool, stencil: bool): unit = failwith "JS only"
        member __.resetGLState(): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.renderBufferImmediate(``object``: Object3D, program: obj, material: Material): unit = failwith "JS only"
        member __.renderBufferDirect(camera: Camera, fog: Fog, material: Material, geometryGroup: obj, ``object``: Object3D): unit = failwith "JS only"
        member __.render(scene: Scene, camera: Camera, ?renderTarget: RenderTarget, ?forceClear: bool): unit = failwith "JS only"
        member __.setFaceCulling(?cullFace: CullFace, ?frontFace: FrontFaceDirection): unit = failwith "JS only"
        member __.setTexture(texture: Texture, slot: float): unit = failwith "JS only"
        member __.getCurrentRenderTarget(): RenderTarget = failwith "JS only"
        member __.setRenderTarget(renderTarget: RenderTarget): unit = failwith "JS only"
        member __.readRenderTargetPixels(renderTarget: RenderTarget, x: float, y: float, width: float, height: float, buffer: obj): unit = failwith "JS only"
        member __.supportsFloatTextures(): obj = failwith "JS only"
        member __.supportsHalfFloatTextures(): obj = failwith "JS only"
        member __.supportsStandardDerivatives(): obj = failwith "JS only"
        member __.supportsCompressedTextureS3TC(): obj = failwith "JS only"
        member __.supportsCompressedTexturePVRTC(): obj = failwith "JS only"
        member __.supportsBlendMinMax(): obj = failwith "JS only"
        member __.supportsVertexTextures(): obj = failwith "JS only"
        member __.supportsInstancedArrays(): obj = failwith "JS only"
        member __.enableScissorTest(boolean: obj): obj = failwith "JS only"

    and RenderTarget =
        interface end

    and WebGLRenderTargetOptions =
        abstract wrapS: Wrapping option with get, set
        abstract wrapT: Wrapping option with get, set
        abstract magFilter: TextureFilter option with get, set
        abstract minFilter: TextureFilter option with get, set
        abstract format: float option with get, set
        abstract ``type``: TextureDataType option with get, set
        abstract anisotropy: float option with get, set
        abstract depthBuffer: bool option with get, set
        abstract stencilBuffer: bool option with get, set

    and [<Import("WebGLRenderTarget","three")>] WebGLRenderTarget(width: float, height: float, ?options: WebGLRenderTargetOptions) =
        interface RenderTarget
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.scissor with get(): Vector4 = failwith "JS only" and set(v: Vector4): unit = failwith "JS only"
        member __.scissorTest with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.viewpport with get(): Vector4 = failwith "JS only" and set(v: Vector4): unit = failwith "JS only"
        member __.texture with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.depthBuffer with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.stencilBuffer with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.wrapS with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.wrapT with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.magFilter with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.minFilter with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.anisotropy with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.offset with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.repeat with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.format with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.``type`` with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.generateMipmaps with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.setSize(width: float, height: float): unit = failwith "JS only"
        member __.clone(): WebGLRenderTarget = failwith "JS only"
        member __.copy(source: WebGLRenderTarget): WebGLRenderTarget = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.addEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.hasEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.removeEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.dispatchEvent(``event``: obj): unit = failwith "JS only"

    and [<Import("WebGLRenderTargetCube","three")>] WebGLRenderTargetCube(width: float, height: float, ?options: WebGLRenderTargetOptions) =
        inherit WebGLRenderTarget(width, height, ?options=options)
        member __.activeCubeFace with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.activeMipMapLevel with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

    and ShaderChunkType =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: name: string -> string with get, set
        abstract alphamap_fragment: string with get, set
        abstract alphamap_pars_fragment: string with get, set
        abstract alphatest_fragment: string with get, set
        abstract aomap_fragment: string with get, set
        abstract aomap_pars_fragment: string with get, set
        abstract begin_vertex: string with get, set
        abstract beginnormal_vertex: string with get, set
        abstract bsdfs: string with get, set
        abstract bumpmap_pars_fragment: string with get, set
        abstract color_fragment: string with get, set
        abstract color_pars_fragment: string with get, set
        abstract color_pars_vertex: string with get, set
        abstract color_vertex: string with get, set
        abstract common: string with get, set
        abstract cube_frag: string with get, set
        abstract cube_vert: string with get, set
        abstract cube_uv_reflection_fragment: string with get, set
        abstract defaultnormal_vertex: string with get, set
        abstract depth_frag: string with get, set
        abstract depth_vert: string with get, set
        abstract depthRGBA_frag: string with get, set
        abstract depthRGBA_vert: string with get, set
        abstract distanceRGBA_frag: string with get, set
        abstract distanceRGBA_vert: string with get, set
        abstract displacementmap_vertex: string with get, set
        abstract displacementmap_pars_vertex: string with get, set
        abstract emissivemap_fragment: string with get, set
        abstract emissivemap_pars_fragment: string with get, set
        abstract encodings_pars_fragment: string with get, set
        abstract encodings_fragment: string with get, set
        abstract envmap_fragment: string with get, set
        abstract envmap_pars_fragment: string with get, set
        abstract envmap_pars_vertex: string with get, set
        abstract envmap_vertex: string with get, set
        abstract equirect_frag: string with get, set
        abstract equirect_vert: string with get, set
        abstract fog_fragment: string with get, set
        abstract fog_pars_fragment: string with get, set
        abstract linedashed_frag: string with get, set
        abstract linedashed_vert: string with get, set
        abstract lightmap_fragment: string with get, set
        abstract lightmap_pars_fragment: string with get, set
        abstract lights_lambert_vertex: string with get, set
        abstract lights_pars: string with get, set
        abstract lights_phong_fragment: string with get, set
        abstract lights_phong_pars_fragment: string with get, set
        abstract lights_phong_pars_vertex: string with get, set
        abstract lights_phong_vertex: string with get, set
        abstract lights_standard_fragment: string with get, set
        abstract lights_standard_pars_fragment: string with get, set
        abstract lights_template: string with get, set
        abstract logdepthbuf_fragment: string with get, set
        abstract logdepthbuf_pars_fragment: string with get, set
        abstract logdepthbuf_pars_vertex: string with get, set
        abstract logdepthbuf_vertex: string with get, set
        abstract map_fragment: string with get, set
        abstract map_pars_fragment: string with get, set
        abstract map_particle_fragment: string with get, set
        abstract map_particle_pars_fragment: string with get, set
        abstract meshbasic_frag: string with get, set
        abstract meshbasic_vert: string with get, set
        abstract meshlambert_frag: string with get, set
        abstract meshlambert_vert: string with get, set
        abstract meshphong_frag: string with get, set
        abstract meshphong_vert: string with get, set
        abstract meshstandard_frag: string with get, set
        abstract meshstandard_vert: string with get, set
        abstract metalnessmap_fragment: string with get, set
        abstract metalnessmap_pars_fragment: string with get, set
        abstract morphnormal_vertex: string with get, set
        abstract morphtarget_pars_vertex: string with get, set
        abstract morphtarget_vertex: string with get, set
        abstract normal_frag: string with get, set
        abstract normal_fragment: string with get, set
        abstract normal_vert: string with get, set
        abstract normalmap_pars_fragment: string with get, set
        abstract points_frag: string with get, set
        abstract points_vert: string with get, set
        abstract premultiplied_alpha_fragment: string with get, set
        abstract project_vertex: string with get, set
        abstract roughnessmap_fragment: string with get, set
        abstract roughnessmap_pars_fragment: string with get, set
        abstract shadowmap_pars_fragment: string with get, set
        abstract shadowmap_pars_vertex: string with get, set
        abstract shadowmap_vertex: string with get, set
        abstract shadowmask_pars_fragment: string with get, set
        abstract skinbase_vertex: string with get, set
        abstract skinning_pars_vertex: string with get, set
        abstract skinning_vertex: string with get, set
        abstract skinnormal_vertex: string with get, set
        abstract specularmap_fragment: string with get, set
        abstract specularmap_pars_fragment: string with get, set
        abstract tonemapping_fragment: string with get, set
        abstract tonemapping_pars_fragment: string with get, set
        abstract uv2_pars_fragment: string with get, set
        abstract uv2_pars_vertex: string with get, set
        abstract uv2_vertex: string with get, set
        abstract uv_pars_fragment: string with get, set
        abstract uv_pars_vertex: string with get, set
        abstract uv_vertex: string with get, set
        abstract worldpos_vertex: string with get, set

    and Shader =
        abstract uniforms: obj with get, set
        abstract vertexShader: string with get, set
        abstract fragmentShader: string with get, set

    and ShaderLibType =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: name: string -> Shader with get, set
        abstract basic: Shader with get, set
        abstract lambert: Shader with get, set
        abstract phong: Shader with get, set
        abstract standard: Shader with get, set
        abstract points: Shader with get, set
        abstract dashed: Shader with get, set
        abstract depth: Shader with get, set
        abstract normal: Shader with get, set
        abstract cube: Shader with get, set
        abstract equirect: Shader with get, set
        abstract depthRGBA: Shader with get, set
        abstract distanceRGBA: Shader with get, set

    and IUniform =
        abstract ``type``: string with get, set
        abstract value: obj with get, set

    and UniformsLibType =
        abstract common: obj with get, set
        abstract aomap: obj with get, set
        abstract lightmap: obj with get, set
        abstract emissivemap: obj with get, set
        abstract bumpmap: obj with get, set
        abstract normalmap: obj with get, set
        abstract displacementmap: obj with get, set
        abstract roughnessmap: obj with get, set
        abstract metalnessmap: obj with get, set
        abstract fog: obj with get, set
        abstract lights: obj with get, set
        abstract points: obj with get, set

    and [<Import("Uniform","three")>] Uniform(``type``: string, value: string) =
        member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.value with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.dynamic with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.onUpdateCallback with get(): Function = failwith "JS only" and set(v: Function): unit = failwith "JS only"
        member __.onUpdate(callback: Function): Uniform = failwith "JS only"

    and [<Import("WebGLBufferRenderer","three")>] WebGLBufferRenderer(_gl: WebGLRenderingContext, extensions: obj, _infoRender: obj) =
        member __.setMode(value: obj): unit = failwith "JS only"
        member __.render(start: obj, count: obj): unit = failwith "JS only"
        member __.renderInstances(geometry: obj): unit = failwith "JS only"

    and WebGLCapabilitiesParameters =
        abstract precision: obj option with get, set
        abstract logarithmicDepthBuffer: obj option with get, set

    and [<Import("WebGLCapabilities","three")>] WebGLCapabilities(gl: WebGLRenderingContext, extensions: obj, parameters: WebGLCapabilitiesParameters) =
        member __.precision with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.logarithmicDepthBuffer with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.maxTextures with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.maxVertexTextures with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.maxTextureSize with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.maxCubemapSize with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.maxAttributes with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.maxVertexUniforms with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.maxVaryings with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.maxFragmentUniforms with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.vertexTextures with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.floatFragmentTextures with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.floatVertexTextures with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.getMaxPrecision(precision: obj): obj = failwith "JS only"

    and [<Import("WebGLExtensions","three")>] WebGLExtensions(gl: WebGLRenderingContext) =
        member __.get(name: string): obj = failwith "JS only"

    and [<Import("WebGLGeometries","three")>] WebGLGeometries(gl: WebGLRenderingContext, extensions: obj, _infoRender: obj) =
        member __.get(``object``: obj): obj = failwith "JS only"

    and [<Import("WebGLLights","three")>] WebGLLights(gl: WebGLRenderingContext, properties: obj, info: obj) =
        member __.get(light: obj): obj = failwith "JS only"

    and [<Import("WebGLIndexedBufferRenderer","three")>] WebGLIndexedBufferRenderer(gl: WebGLRenderingContext, properties: obj, info: obj) =
        member __.setMode(value: obj): unit = failwith "JS only"
        member __.setIndex(index: obj): unit = failwith "JS only"
        member __.render(start: obj, count: float): unit = failwith "JS only"
        member __.renderInstances(geometry: obj, start: obj, count: float): unit = failwith "JS only"

    and [<Import("WebGLObjects","three")>] WebGLObjects(gl: WebGLRenderingContext, properties: obj, info: obj) =
        member __.getAttributeBuffer(attribute: obj): obj = failwith "JS only"
        member __.getWireframeAttribute(geometry: obj): obj = failwith "JS only"
        member __.update(``object``: obj): unit = failwith "JS only"

    and [<Import("WebGLProgram","three")>] WebGLProgram(renderer: WebGLRenderer, code: string, material: ShaderMaterial, parameters: WebGLRendererParameters) =
        member __.id with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.code with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.usedTimes with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.program with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.vertexShader with get(): WebGLShader = failwith "JS only" and set(v: WebGLShader): unit = failwith "JS only"
        member __.fragmentShader with get(): WebGLShader = failwith "JS only" and set(v: WebGLShader): unit = failwith "JS only"
        member __.uniforms with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.attributes with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.getUniforms(): obj = failwith "JS only"
        member __.getAttributes(): obj = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and [<Import("WebGLPrograms","three")>] WebGLPrograms(renderer: WebGLRenderer, capabilities: obj) =
        member __.programs with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.getParameters(material: ShaderMaterial, lights: obj, fog: obj, ``object``: obj): ResizeArray<obj> = failwith "JS only"
        member __.getProgramCode(material: ShaderMaterial, parameters: obj): string = failwith "JS only"
        member __.acquireProgram(material: ShaderMaterial, parameters: obj, code: string): WebGLProgram = failwith "JS only"
        member __.releaseProgram(program: WebGLProgram): unit = failwith "JS only"

    and [<Import("WebGLProperties","three")>] WebGLProperties() =
        member __.get(``object``: obj): obj = failwith "JS only"
        member __.delete(``object``: obj): unit = failwith "JS only"
        member __.clear(): unit = failwith "JS only"

    and [<Import("WebGLShader","three")>] WebGLShader(gl: obj, ``type``: string, string: string) =
        class end

    and [<Import("WebGLShadowMap","three")>] WebGLShadowMap(_renderer: Renderer, _lights: ResizeArray<obj>, _objects: ResizeArray<obj>) =
        member __.enabled with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.autoUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.needsUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.``type`` with get(): ShadowMapType = failwith "JS only" and set(v: ShadowMapType): unit = failwith "JS only"
        member __.cullFace with get(): CullFace = failwith "JS only" and set(v: CullFace): unit = failwith "JS only"
        member __.render(scene: Scene, camera: Camera): unit = failwith "JS only"

    and [<Import("WebGLState","three")>] WebGLState(gl: obj, extensions: obj, paramThreeToGL: Function) =
        member __.init(): unit = failwith "JS only"
        member __.initAttributes(): unit = failwith "JS only"
        member __.enableAttribute(attribute: string): unit = failwith "JS only"
        member __.enableAttributeAndDivisor(attribute: string, meshPerAttribute: obj, extension: obj): unit = failwith "JS only"
        member __.disableUnusedAttributes(): unit = failwith "JS only"
        member __.enable(id: string): unit = failwith "JS only"
        member __.disable(id: string): unit = failwith "JS only"
        member __.getCompressedTextureFormats(): ResizeArray<obj> = failwith "JS only"
        member __.setBlending(blending: float, blendEquation: float, blendSrc: float, blendDst: float, blendEquationAlpha: float, blendSrcAlpha: float, blendDstAlpha: float): unit = failwith "JS only"
        member __.setDepthFunc(func: Function): unit = failwith "JS only"
        member __.setDepthTest(depthTest: float): unit = failwith "JS only"
        member __.setDepthWrite(depthWrite: float): unit = failwith "JS only"
        member __.setColorWrite(colorWrite: float): unit = failwith "JS only"
        member __.setStencilFunc(stencilFunc: Function, stencilRef: obj, stencilMask: obj): unit = failwith "JS only"
        member __.setStencilOp(stencilFail: obj, stencilZFail: obj, stencilZPass: obj): unit = failwith "JS only"
        member __.setStencilTest(stencilTest: bool): unit = failwith "JS only"
        member __.setStencilWrite(stencilWrite: obj): unit = failwith "JS only"
        member __.setFlipSided(flipSided: float): unit = failwith "JS only"
        member __.setLineWidth(width: float): unit = failwith "JS only"
        member __.setPolygonOffset(polygonoffset: float, factor: float, units: float): unit = failwith "JS only"
        member __.setScissorTest(scissorTest: bool): unit = failwith "JS only"
        member __.getScissorTest(): bool = failwith "JS only"
        member __.activeTexture(webglSlot: obj): unit = failwith "JS only"
        member __.bindTexture(webglType: obj, webglTexture: obj): unit = failwith "JS only"
        member __.compressedTexImage2D(): unit = failwith "JS only"
        member __.texImage2D(): unit = failwith "JS only"
        member __.clearColor(r: float, g: float, b: float, a: float): unit = failwith "JS only"
        member __.clearDepth(depth: float): unit = failwith "JS only"
        member __.clearStencil(stencil: obj): unit = failwith "JS only"
        member __.scissor(scissor: obj): unit = failwith "JS only"
        member __.viewport(viewport: obj): unit = failwith "JS only"
        member __.reset(): unit = failwith "JS only"

    and [<Import("LensFlarePlugin","three")>] LensFlarePlugin(renderer: WebGLRenderer, flares: ResizeArray<obj>) =
        member __.render(scene: Scene, camera: Camera, viewportWidth: float, viewportHeight: float): unit = failwith "JS only"

    and [<Import("SpritePlugin","three")>] SpritePlugin(renderer: WebGLRenderer, sprites: ResizeArray<obj>) =
        member __.render(scene: Scene, camera: Camera, viewportWidth: float, viewportHeight: float): unit = failwith "JS only"

    and [<Import("Scene","three")>] Scene() =
        inherit Object3D()
        member __.fog with get(): IFog = failwith "JS only" and set(v: IFog): unit = failwith "JS only"
        member __.overrideMaterial with get(): Material = failwith "JS only" and set(v: Material): unit = failwith "JS only"
        member __.autoUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.copy(source: Scene, ?``recursive``: bool): Scene = failwith "JS only"

    and IFog =
        abstract name: string with get, set
        abstract color: Color with get, set
        abstract clone: unit -> IFog

    and [<Import("Fog","three")>] Fog(hex: float, ?near: float, ?far: float) =
        interface IFog with
            member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
            member __.clone(): IFog = failwith "JS only"
        member __.near with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.far with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

    and [<Import("FogExp2","three")>] FogExp2(hex: U2<float, string>, ?density: float) =
        interface IFog with
            member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.color with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
            member __.clone(): IFog = failwith "JS only"
        member __.density with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

    and [<Import("Texture","three")>] Texture(image: U3<HTMLImageElement, HTMLCanvasElement, HTMLVideoElement>, ?mapping: Mapping, ?wrapS: Wrapping, ?wrapT: Wrapping, ?magFilter: TextureFilter, ?minFilter: TextureFilter, ?format: PixelFormat, ?``type``: TextureDataType, ?anisotropy: float) =
        member __.id with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.uuid with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.sourceFile with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.image with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.mipmaps with get(): ResizeArray<ImageData> = failwith "JS only" and set(v: ResizeArray<ImageData>): unit = failwith "JS only"
        member __.mapping with get(): Mapping = failwith "JS only" and set(v: Mapping): unit = failwith "JS only"
        member __.wrapS with get(): Wrapping = failwith "JS only" and set(v: Wrapping): unit = failwith "JS only"
        member __.wrapT with get(): Wrapping = failwith "JS only" and set(v: Wrapping): unit = failwith "JS only"
        member __.magFilter with get(): TextureFilter = failwith "JS only" and set(v: TextureFilter): unit = failwith "JS only"
        member __.minFilter with get(): TextureFilter = failwith "JS only" and set(v: TextureFilter): unit = failwith "JS only"
        member __.anisotropy with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.format with get(): PixelFormat = failwith "JS only" and set(v: PixelFormat): unit = failwith "JS only"
        member __.``type`` with get(): TextureDataType = failwith "JS only" and set(v: TextureDataType): unit = failwith "JS only"
        member __.offset with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.repeat with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.generateMipmaps with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.premultiplyAlpha with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.flipY with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.unpackAlignment with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.encoding with get(): TextureEncoding = failwith "JS only" and set(v: TextureEncoding): unit = failwith "JS only"
        member __.version with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.needsUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.onUpdate with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.DEFAULT_IMAGE with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.DEFAULT_MAPPING with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.clone(): Texture = failwith "JS only"
        member __.copy(source: Texture): Texture = failwith "JS only"
        member __.toJSON(meta: obj): obj = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.transformUv(uv: Vector): unit = failwith "JS only"
        member __.addEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.hasEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.removeEventListener(``type``: string, listener: Func<Event, unit>): unit = failwith "JS only"
        member __.dispatchEvent(``event``: obj): unit = failwith "JS only"

    and [<Import("CanvasTexture","three")>] CanvasTexture(canvas: U3<HTMLImageElement, HTMLCanvasElement, HTMLVideoElement>, ?mapping: Mapping, ?wrapS: Wrapping, ?wrapT: Wrapping, ?magFilter: TextureFilter, ?minFilter: TextureFilter, ?format: PixelFormat, ?``type``: TextureDataType, ?anisotropy: float) =
        inherit Texture(unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null)
        member __.clone(): CanvasTexture = failwith "JS only"
        member __.copy(source: CanvasTexture): CanvasTexture = failwith "JS only"

    and [<Import("CubeTexture","three")>] CubeTexture(?images: ResizeArray<obj>, ?mapping: Mapping, ?wrapS: Wrapping, ?wrapT: Wrapping, ?magFilter: TextureFilter, ?minFilter: TextureFilter, ?format: PixelFormat, ?``type``: TextureDataType, ?anisotropy: float) =
        inherit Texture(unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null)
        member __.images with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.copy(source: CubeTexture): CubeTexture = failwith "JS only"

    and [<Import("CompressedTexture","three")>] CompressedTexture(mipmaps: ResizeArray<ImageData>, width: float, height: float, ?format: PixelFormat, ?``type``: TextureDataType, ?mapping: Mapping, ?wrapS: Wrapping, ?wrapT: Wrapping, ?magFilter: TextureFilter, ?minFilter: TextureFilter, ?anisotropy: float) =
        inherit Texture(unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null)
        member __.image with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.clone(): CompressedTexture = failwith "JS only"
        member __.copy(source: CompressedTexture): CompressedTexture = failwith "JS only"

    and [<Import("DataTexture","three")>] DataTexture(data: obj (* U10<ArrayBuffer, Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float32Array, Float64Array> *), width: float, height: float, format: PixelFormat, ``type``: TextureDataType, mapping: Mapping, wrapS: Wrapping, wrapT: Wrapping, magFilter: TextureFilter, minFilter: TextureFilter, ?anisotropy: float) =
        inherit Texture(unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null)
        member __.image with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.clone(): DataTexture = failwith "JS only"
        member __.copy(source: DataTexture): DataTexture = failwith "JS only"

    and [<Import("VideoTexture","three")>] VideoTexture(video: HTMLVideoElement, ?mapping: Mapping, ?wrapS: Wrapping, ?wrapT: Wrapping, ?magFilter: TextureFilter, ?minFilter: TextureFilter, ?format: PixelFormat, ?``type``: TextureDataType, ?anisotropy: float) =
        inherit Texture(unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null, unbox null)
        member __.clone(): VideoTexture = failwith "JS only"
        member __.copy(source: VideoTexture): VideoTexture = failwith "JS only"

    and [<Import("Audio","three")>] Audio(listener: AudioListener) =
        inherit Object3D()
        member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.context with get(): AudioContext = failwith "JS only" and set(v: AudioContext): unit = failwith "JS only"
        member __.source with get(): AudioBufferSourceNode = failwith "JS only" and set(v: AudioBufferSourceNode): unit = failwith "JS only"
        member __.gain with get(): GainNode = failwith "JS only" and set(v: GainNode): unit = failwith "JS only"
        member __.autoplay with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.startTime with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.playbackRate with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.hasPlaybackControl with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.isPlaying with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.sourceType with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.filter with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.getOutput(): GainNode = failwith "JS only"
        member __.load(file: string): Audio = failwith "JS only"
        member __.setNodeSource(audioNode: AudioBufferSourceNode): Audio = failwith "JS only"
        member __.setBuffer(audioBuffer: AudioBuffer): Audio = failwith "JS only"
        member __.play(): unit = failwith "JS only"
        member __.pause(): unit = failwith "JS only"
        member __.stop(): unit = failwith "JS only"
        member __.connect(): unit = failwith "JS only"
        member __.disconnect(): unit = failwith "JS only"
        member __.setFilter(value: obj): unit = failwith "JS only"
        member __.getFilter(): obj = failwith "JS only"
        member __.setPlaybackRate(value: float): unit = failwith "JS only"
        member __.getPlaybackRate(): float = failwith "JS only"
        member __.onEnded(): unit = failwith "JS only"
        member __.setLoop(value: bool): unit = failwith "JS only"
        member __.getLoop(): bool = failwith "JS only"
        member __.setVolume(value: float): unit = failwith "JS only"
        member __.getVolume(): float = failwith "JS only"

    and [<Import("AudioAnalyser","three")>] AudioAnalyser(audio: obj, fftSize: float) =
        member __.analyser with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.data with get(): Uint8Array = failwith "JS only" and set(v: Uint8Array): unit = failwith "JS only"
        member __.getData(): Uint8Array = failwith "JS only"

    and [<Import("AudioBuffer","three")>] AudioBuffer(context: obj) =
        member __.context with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.ready with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.readyCallbacks with get(): ResizeArray<Function> = failwith "JS only" and set(v: ResizeArray<Function>): unit = failwith "JS only"
        member __.load(file: string): AudioBuffer = failwith "JS only"
        member __.onReady(callback: Function): unit = failwith "JS only"

    and [<Import("PositionalAudio","three")>] PositionalAudio(listener: AudioListener) =
        inherit Audio(listener)
        member __.panner with get(): PannerNode = failwith "JS only" and set(v: PannerNode): unit = failwith "JS only"
        member __.setRefDistance(value: float): unit = failwith "JS only"
        member __.getRefDistance(): float = failwith "JS only"
        member __.setRolloffFactor(value: float): unit = failwith "JS only"
        member __.getRolloffFactor(): float = failwith "JS only"
        member __.setDistanceModel(value: float): unit = failwith "JS only"
        member __.getDistanceModel(): float = failwith "JS only"
        member __.setMaxDistance(value: float): unit = failwith "JS only"
        member __.getMaxDistance(): float = failwith "JS only"

    and [<Import("AudioListener","three")>] AudioListener() =
        inherit Object3D()
        member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.context with get(): AudioContext = failwith "JS only" and set(v: AudioContext): unit = failwith "JS only"
        member __.gain with get(): GainNode = failwith "JS only" and set(v: GainNode): unit = failwith "JS only"
        member __.getInput(): GainNode = failwith "JS only"
        member __.removeFilter(): unit = failwith "JS only"
        member __.setFilter(value: obj): unit = failwith "JS only"
        member __.getFilter(): obj = failwith "JS only"
        member __.setMasterVolume(value: float): unit = failwith "JS only"
        member __.getMasterVolume(): float = failwith "JS only"

    and [<Import("Curve","three")>] Curve<'T>() =
        member __.getPoint(t: float): 'T = failwith "JS only"
        member __.getPointAt(u: float): 'T = failwith "JS only"
        member __.getPoints(?divisions: float): ResizeArray<'T> = failwith "JS only"
        member __.getSpacedPoints(?divisions: float): ResizeArray<'T> = failwith "JS only"
        member __.getLength(): float = failwith "JS only"
        member __.getLengths(?divisions: float): ResizeArray<float> = failwith "JS only"
        member __.updateArcLengths(): unit = failwith "JS only"
        member __.getUtoTmapping(u: float, distance: float): float = failwith "JS only"
        member __.getTangent(t: float): 'T = failwith "JS only"
        member __.getTangentAt(u: float): 'T = failwith "JS only"
        static member create(constructorFunc: Function, getPointFunc: Function): Function = failwith "JS only"

    and [<Import("CurvePath","three")>] CurvePath<'T>() =
        inherit Curve<'T>()
        member __.curves with get(): ResizeArray<Curve<'T>> = failwith "JS only" and set(v: ResizeArray<Curve<'T>>): unit = failwith "JS only"
        member __.autoClose with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.add(curve: Curve<'T>): unit = failwith "JS only"
        member __.checkConnection(): bool = failwith "JS only"
        member __.closePath(): unit = failwith "JS only"
        member __.getPoint(t: float): 'T = failwith "JS only"
        member __.getLength(): float = failwith "JS only"
        member __.getCurveLengths(): ResizeArray<float> = failwith "JS only"
        member __.createPointsGeometry(divisions: float): Geometry = failwith "JS only"
        member __.createSpacedPointsGeometry(divisions: float): Geometry = failwith "JS only"
        member __.createGeometry(points: ResizeArray<'T>): Geometry = failwith "JS only"

    and PathActions =
        | MOVE_TO = 0
        | LINE_TO = 1
        | QUADRATIC_CURVE_TO = 2
        | BEZIER_CURVE_TO = 3
        | CSPLINE_THRU = 4
        | ARC = 5
        | ELLIPSE = 6

    and PathAction =
        abstract action: PathActions with get, set
        abstract args: obj with get, set

    and [<Import("Path","three")>] Path(?points: ResizeArray<Vector2>) =
        inherit CurvePath<Vector2>()
        member __.actions with get(): ResizeArray<PathAction> = failwith "JS only" and set(v: ResizeArray<PathAction>): unit = failwith "JS only"
        member __.fromPoints(vectors: ResizeArray<Vector2>): unit = failwith "JS only"
        member __.moveTo(x: float, y: float): unit = failwith "JS only"
        member __.lineTo(x: float, y: float): unit = failwith "JS only"
        member __.quadraticCurveTo(aCPx: float, aCPy: float, aX: float, aY: float): unit = failwith "JS only"
        member __.bezierCurveTo(aCP1x: float, aCP1y: float, aCP2x: float, aCP2y: float, aX: float, aY: float): unit = failwith "JS only"
        member __.splineThru(pts: ResizeArray<Vector2>): unit = failwith "JS only"
        member __.arc(aX: float, aY: float, aRadius: float, aStartAngle: float, aEndAngle: float, aClockwise: bool): unit = failwith "JS only"
        member __.absarc(aX: float, aY: float, aRadius: float, aStartAngle: float, aEndAngle: float, aClockwise: bool): unit = failwith "JS only"
        member __.ellipse(aX: float, aY: float, xRadius: float, yRadius: float, aStartAngle: float, aEndAngle: float, aClockwise: bool, aRotation: float): unit = failwith "JS only"
        member __.absellipse(aX: float, aY: float, xRadius: float, yRadius: float, aStartAngle: float, aEndAngle: float, aClockwise: bool, aRotation: float): unit = failwith "JS only"
        member __.getSpacedPoints(?divisions: float): ResizeArray<Vector2> = failwith "JS only"
        member __.getPoints(?divisions: float, ?closedPath: bool): ResizeArray<Vector2> = failwith "JS only"
        member __.toShapes(isCCW: bool, noHoles: obj): ResizeArray<Shape> = failwith "JS only"

    and [<Import("Shape","three")>] Shape(?points: ResizeArray<Vector2>) =
        inherit Path()
        member __.holes with get(): ResizeArray<Path> = failwith "JS only" and set(v: ResizeArray<Path>): unit = failwith "JS only"
        member __.extrude(?options: obj): ExtrudeGeometry = failwith "JS only"
        member __.makeGeometry(?options: obj): ShapeGeometry = failwith "JS only"
        member __.getPointsHoles(divisions: float): ResizeArray<ResizeArray<Vector2>> = failwith "JS only"
        member __.extractAllPoints(divisions: float): obj = failwith "JS only"
        member __.extractPoints(divisions: float): ResizeArray<Vector2> = failwith "JS only"

    and [<Import("CatmullRomCurve3","three")>] CatmullRomCurve3(?points: ResizeArray<Vector3>) =
        inherit Curve<Vector3>()
        member __.points with get(): ResizeArray<Vector3> = failwith "JS only" and set(v: ResizeArray<Vector3>): unit = failwith "JS only"
        member __.getPoint(t: float): Vector3 = failwith "JS only"

    and [<Import("ClosedSplineCurve3","three")>] ClosedSplineCurve3() =
        inherit CatmullRomCurve3()


    and [<Import("SplineCurve3","three")>] SplineCurve3() =
        inherit CatmullRomCurve3()


    and [<Import("CubicBezierCurve","three")>] CubicBezierCurve(v0: Vector2, v1: Vector2, v2: Vector2, v3: Vector2) =
        inherit Curve<Vector2>()
        member __.v0 with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.v1 with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.v2 with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.v3 with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"

    and [<Import("CubicBezierCurve3","three")>] CubicBezierCurve3(v0: Vector3, v1: Vector3, v2: Vector3, v3: Vector3) =
        inherit Curve<Vector3>()
        member __.v0 with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.v1 with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.v2 with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.v3 with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.getPoint(t: float): Vector3 = failwith "JS only"

    and [<Import("EllipseCurve","three")>] EllipseCurve(aX: float, aY: float, xRadius: float, yRadius: float, aStartAngle: float, aEndAngle: float, aClockwise: bool, aRotation: float) =
        inherit Curve<Vector2>()
        member __.aX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.aY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.xRadius with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.yRadius with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.aStartAngle with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.aEndAngle with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.aClockwise with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.aRotation with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

    and [<Import("ArcCurve","three")>] ArcCurve(aX: float, aY: float, aRadius: float, aStartAngle: float, aEndAngle: float, aClockwise: bool) =
        inherit EllipseCurve(0., 0., 0., 0., 0., 0., false, 0.)

    and [<Import("LineCurve","three")>] LineCurve(v1: Vector2, v2: Vector2) =
        inherit Curve<Vector2>()
        member __.v1 with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.v2 with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"

    and [<Import("LineCurve3","three")>] LineCurve3(v1: Vector3, v2: Vector3) =
        inherit Curve<Vector3>()
        member __.v1 with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.v2 with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.getPoint(t: float): Vector3 = failwith "JS only"

    and [<Import("QuadraticBezierCurve","three")>] QuadraticBezierCurve(v0: Vector2, v1: Vector2, v2: Vector2) =
        inherit Curve<Vector2>()
        member __.v0 with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.v1 with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"
        member __.v2 with get(): Vector2 = failwith "JS only" and set(v: Vector2): unit = failwith "JS only"

    and [<Import("QuadraticBezierCurve3","three")>] QuadraticBezierCurve3(v0: Vector3, v1: Vector3, v2: Vector3) =
        inherit Curve<Vector3>()
        member __.v0 with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.v1 with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.v2 with get(): Vector3 = failwith "JS only" and set(v: Vector3): unit = failwith "JS only"
        member __.getPoint(t: float): Vector3 = failwith "JS only"

    and [<Import("SplineCurve","three")>] SplineCurve(?points: ResizeArray<Vector2>) =
        inherit Curve<Vector2>()
        member __.points with get(): ResizeArray<Vector2> = failwith "JS only" and set(v: ResizeArray<Vector2>): unit = failwith "JS only"

    and [<Import("BoxBufferGeometry","three")>] BoxBufferGeometry(width: float, height: float, depth: float, ?widthSegments: float, ?heightSegments: float, ?depthSegments: float) =
        inherit BufferGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("BoxGeometry","three")>] BoxGeometry(width: float, height: float, depth: float, ?widthSegments: float, ?heightSegments: float, ?depthSegments: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.clone(): BoxGeometry = failwith "JS only"

    and [<Import("CubeGeometry","three")>] CubeGeometry() =
        inherit BoxGeometry(0., 0., 0.)

    and [<Import("CircleBufferGeometry","three")>] CircleBufferGeometry(?radius: float, ?segments: float, ?thetaStart: float, ?thetaLength: float) =
        inherit BufferGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("CircleGeometry","three")>] CircleGeometry(?radius: float, ?segments: float, ?thetaStart: float, ?thetaLength: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("CylinderBufferGeometry","three")>] CylinderBufferGeometry(?radiusTop: float, ?radiusBottom: float, ?height: float, ?radialSegments: float, ?heightSegments: float, ?openEnded: bool, ?thetaStart: float, ?thetaLength: float) =
        inherit BufferGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("CylinderGeometry","three")>] CylinderGeometry(?radiusTop: float, ?radiusBottom: float, ?height: float, ?radiusSegments: float, ?heightSegments: float, ?openEnded: bool, ?thetaStart: float, ?thetaLength: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("DodecahedronGeometry","three")>] DodecahedronGeometry(radius: float, detail: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("EdgesGeometry","three")>] EdgesGeometry(geometry: BufferGeometry, thresholdAngle: float) =
        inherit BufferGeometry()
        member __.clone(): EdgesGeometry = failwith "JS only"

    and [<Import("ExtrudeGeometry","three")>] ExtrudeGeometry(?shapes: ResizeArray<Shape>, ?options: obj) =
        inherit Geometry()
        member __.WorldUVGenerator with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.addShapeList(shapes: ResizeArray<Shape>, ?options: obj): unit = failwith "JS only"
        member __.addShape(shape: Shape, ?options: obj): unit = failwith "JS only"

    and [<Import("IcosahedronGeometry","three")>] IcosahedronGeometry(radius: float, detail: float) =
        inherit PolyhedronGeometry(unbox null, unbox null)

    and [<Import("LatheBufferGeometry","three")>] LatheBufferGeometry(points: ResizeArray<Vector3>, ?segments: float, ?phiStart: float, ?phiLength: float) =
        inherit BufferGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("LatheGeometry","three")>] LatheGeometry(points: ResizeArray<Vector3>, ?segments: float, ?phiStart: float, ?phiLength: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("OctahedronGeometry","three")>] OctahedronGeometry(radius: float, detail: float) =
        inherit PolyhedronGeometry(unbox null, unbox null)

    and [<Import("ParametricGeometry","three")>] ParametricGeometry(func: Func<float, float, Vector3>, slices: float, stacks: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("PlaneBufferGeometry","three")>] PlaneBufferGeometry(width: float, height: float, ?widthSegments: float, ?heightSegments: float) =
        inherit BufferGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("PlaneGeometry","three")>] PlaneGeometry(width: float, height: float, ?widthSegments: float, ?heightSegments: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("PolyhedronGeometry","three")>] PolyhedronGeometry(vertices: ResizeArray<Vector3>, faces: ResizeArray<Face3>, ?radius: float, ?detail: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.boundingSphere with get(): Sphere = failwith "JS only" and set(v: Sphere): unit = failwith "JS only"

    and [<Import("RingBufferGeometry","three")>] RingBufferGeometry(?innerRadius: float, ?outerRadius: float, ?thetaSegments: float, ?phiSegments: float, ?thetaStart: float, ?thetaLength: float) =
        inherit BufferGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("RingGeometry","three")>] RingGeometry(?innerRadius: float, ?outerRadius: float, ?thetaSegments: float, ?phiSegments: float, ?thetaStart: float, ?thetaLength: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("ShapeGeometry","three")>] ShapeGeometry(shapes: ResizeArray<Shape>, ?options: obj) =
        inherit Geometry()
        member __.addShapeList(shapes: ResizeArray<Shape>, options: obj): ShapeGeometry = failwith "JS only"
        member __.addShape(shape: Shape, ?options: obj): unit = failwith "JS only"

    and [<Import("SphereBufferGeometry","three")>] SphereBufferGeometry(radius: float, ?widthSegments: float, ?heightSegments: float, ?phiStart: float, ?phiLength: float, ?thetaStart: float, ?thetaLength: float) =
        inherit BufferGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("SphereGeometry","three")>] SphereGeometry(radius: float, ?widthSegments: float, ?heightSegments: float, ?phiStart: float, ?phiLength: float, ?thetaStart: float, ?thetaLength: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("TetrahedronGeometry","three")>] TetrahedronGeometry(?radius: float, ?detail: float) =
        inherit PolyhedronGeometry(unbox null, unbox null)

    and TextGeometryParameters =
        abstract font: Font with get, set
        abstract size: float with get, set
        abstract height: float with get, set
        abstract curveSegments: float with get, set
        abstract bevelEnabled: bool with get, set
        abstract bevelThickness: float with get, set
        abstract bevelSize: float with get, set

    and [<Import("TextGeometry","three")>] TextGeometry(text: string, ?parameters: TextGeometryParameters) =
        inherit ExtrudeGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("TorusBufferGeometry","three")>] TorusBufferGeometry(?radius: float, ?tube: float, ?radialSegments: float, ?tubularSegments: float, ?arc: float) =
        inherit BufferGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("TorusGeometry","three")>] TorusGeometry(?radius: float, ?tube: float, ?radialSegments: float, ?tubularSegments: float, ?arc: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("TorusKnotBufferGeometry","three")>] TorusKnotBufferGeometry(?radius: float, ?tube: float, ?radialSegments: float, ?tubularSegments: float, ?p: float, ?q: float, ?heightScale: float) =
        inherit BufferGeometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("TorusKnotGeometry","three")>] TorusKnotGeometry(?radius: float, ?tube: float, ?radialSegments: float, ?tubularSegments: float, ?p: float, ?q: float, ?heightScale: float) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

    and [<Import("TubeGeometry","three")>] TubeGeometry(path: Path, ?segments: float, ?radius: float, ?radiusSegments: float, ?closed: bool, ?taper: Func<float, float>) =
        inherit Geometry()
        member __.parameters with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.tangents with get(): ResizeArray<Vector3> = failwith "JS only" and set(v: ResizeArray<Vector3>): unit = failwith "JS only"
        member __.normals with get(): ResizeArray<Vector3> = failwith "JS only" and set(v: ResizeArray<Vector3>): unit = failwith "JS only"
        member __.binormals with get(): ResizeArray<Vector3> = failwith "JS only" and set(v: ResizeArray<Vector3>): unit = failwith "JS only"
        static member NoTaper(?u: float): float = failwith "JS only"
        static member SinusoidalTaper(u: float): float = failwith "JS only"
        static member FrenetFrames(path: Path, segments: float, closed: bool): unit = failwith "JS only"

    and [<Import("WireframeGeometry","three")>] WireframeGeometry(geometry: U2<Geometry, BufferGeometry>) =
        inherit BufferGeometry()

    and [<Import("ArrowHelper","three")>] ArrowHelper(dir: Vector3, ?origin: Vector3, ?length: float, ?hex: float, ?headLength: float, ?headWidth: float) =
        inherit Object3D()
        member __.line with get(): Line = failwith "JS only" and set(v: Line): unit = failwith "JS only"
        member __.cone with get(): Mesh = failwith "JS only" and set(v: Mesh): unit = failwith "JS only"
        member __.setDirection(dir: Vector3): unit = failwith "JS only"
        member __.setLength(length: float, ?headLength: float, ?headWidth: float): unit = failwith "JS only"
        member __.setColor(hex: float): unit = failwith "JS only"

    and [<Import("AxisHelper","three")>] AxisHelper(?size: float) =
        inherit LineSegments()

    and [<Import("BoundingBoxHelper","three")>] BoundingBoxHelper(?``object``: Object3D, ?hex: float) =
        inherit Mesh()
        member __.``object`` with get(): Object3D = failwith "JS only" and set(v: Object3D): unit = failwith "JS only"
        member __.box with get(): Box3 = failwith "JS only" and set(v: Box3): unit = failwith "JS only"
        member __.update(): unit = failwith "JS only"

    and [<Import("BoxHelper","three")>] BoxHelper(?``object``: Object3D) =
        inherit LineSegments()
        member __.update(?``object``: Object3D): unit = failwith "JS only"

    and [<Import("CameraHelper","three")>] CameraHelper(camera: Camera) =
        inherit LineSegments()
        member __.camera with get(): Camera = failwith "JS only" and set(v: Camera): unit = failwith "JS only"
        member __.pointMap with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.update(): unit = failwith "JS only"

    and [<Import("DirectionalLightHelper","three")>] DirectionalLightHelper(light: Light, ?size: float) =
        inherit Object3D()
        member __.light with get(): Light = failwith "JS only" and set(v: Light): unit = failwith "JS only"
        member __.lightPlane with get(): Line = failwith "JS only" and set(v: Line): unit = failwith "JS only"
        member __.targetLine with get(): Line = failwith "JS only" and set(v: Line): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.update(): unit = failwith "JS only"

    and [<Import("EdgesHelper","three")>] EdgesHelper(``object``: Object3D, ?hex: float, ?thresholdAngle: float) =
        inherit LineSegments()

    and [<Import("FaceNormalsHelper","three")>] FaceNormalsHelper(``object``: Object3D, ?size: float, ?hex: float, ?linewidth: float) =
        inherit LineSegments()
        member __.``object`` with get(): Object3D = failwith "JS only" and set(v: Object3D): unit = failwith "JS only"
        member __.size with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.update(?``object``: Object3D): unit = failwith "JS only"

    and [<Import("GridHelper","three")>] GridHelper(size: float, step: float) =
        inherit LineSegments()
        member __.color1 with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.color2 with get(): Color = failwith "JS only" and set(v: Color): unit = failwith "JS only"
        member __.setColors(colorCenterLine: float, colorGrid: float): unit = failwith "JS only"

    and [<Import("HemisphereLightHelper","three")>] HemisphereLightHelper(light: Light, sphereSize: float) =
        inherit Object3D()
        member __.light with get(): Light = failwith "JS only" and set(v: Light): unit = failwith "JS only"
        member __.colors with get(): ResizeArray<Color> = failwith "JS only" and set(v: ResizeArray<Color>): unit = failwith "JS only"
        member __.lightSphere with get(): Mesh = failwith "JS only" and set(v: Mesh): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.update(): unit = failwith "JS only"

    and [<Import("PointLightHelper","three")>] PointLightHelper(light: Light, sphereSize: float) =
        inherit Object3D()
        member __.light with get(): Light = failwith "JS only" and set(v: Light): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.update(): unit = failwith "JS only"

    and [<Import("SkeletonHelper","three")>] SkeletonHelper(bone: Object3D) =
        inherit LineSegments()
        member __.bones with get(): ResizeArray<Bone> = failwith "JS only" and set(v: ResizeArray<Bone>): unit = failwith "JS only"
        member __.root with get(): Object3D = failwith "JS only" and set(v: Object3D): unit = failwith "JS only"
        member __.getBoneList(``object``: Object3D): ResizeArray<Bone> = failwith "JS only"
        member __.update(): unit = failwith "JS only"

    and [<Import("SpotLightHelper","three")>] SpotLightHelper(light: Light, sphereSize: float, arrowLength: float) =
        inherit Object3D()
        member __.light with get(): Light = failwith "JS only" and set(v: Light): unit = failwith "JS only"
        member __.cone with get(): Mesh = failwith "JS only" and set(v: Mesh): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.update(): unit = failwith "JS only"

    and [<Import("VertexNormalsHelper","three")>] VertexNormalsHelper(``object``: Object3D, ?size: float, ?hex: float, ?linewidth: float) =
        inherit LineSegments()
        member __.``object`` with get(): Object3D = failwith "JS only" and set(v: Object3D): unit = failwith "JS only"
        member __.size with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.update(?``object``: Object3D): unit = failwith "JS only"

    and [<Import("WireframeHelper","three")>] WireframeHelper(``object``: Object3D, ?hex: float) =
        inherit LineSegments()

    and [<Import("ImmediateRenderObject","three")>] ImmediateRenderObject(material: Material) =
        inherit Object3D()
        member __.material with get(): Material = failwith "JS only" and set(v: Material): unit = failwith "JS only"
        member __.render(renderCallback: Function): unit = failwith "JS only"

    and MorphBlendMeshAnimation =
        abstract start: float with get, set
        abstract ``end``: float with get, set
        abstract length: float with get, set
        abstract fps: float with get, set
        abstract duration: float with get, set
        abstract lastFrame: float with get, set
        abstract currentFrame: float with get, set
        abstract active: bool with get, set
        abstract time: float with get, set
        abstract direction: float with get, set
        abstract weight: float with get, set
        abstract directionBackwards: bool with get, set
        abstract mirroredLoop: bool with get, set

    and [<Import("MorphBlendMesh","three")>] MorphBlendMesh(geometry: Geometry, material: Material) =
        inherit Mesh()
        member __.animationsMap with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.animationsList with get(): ResizeArray<MorphBlendMeshAnimation> = failwith "JS only" and set(v: ResizeArray<MorphBlendMeshAnimation>): unit = failwith "JS only"
        member __.createAnimation(name: string, start: float, ``end``: float, fps: float): unit = failwith "JS only"
        member __.autoCreateAnimations(fps: float): unit = failwith "JS only"
        member __.setAnimationDirectionForward(name: string): unit = failwith "JS only"
        member __.setAnimationDirectionBackward(name: string): unit = failwith "JS only"
        member __.setAnimationFPS(name: string, fps: float): unit = failwith "JS only"
        member __.setAnimationDuration(name: string, duration: float): unit = failwith "JS only"
        member __.setAnimationWeight(name: string, weight: float): unit = failwith "JS only"
        member __.setAnimationTime(name: string, time: float): unit = failwith "JS only"
        member __.getAnimationTime(name: string): float = failwith "JS only"
        member __.getAnimationDuration(name: string): float = failwith "JS only"
        member __.playAnimation(name: string): unit = failwith "JS only"
        member __.stopAnimation(name: string): unit = failwith "JS only"
        member __.update(delta: float): unit = failwith "JS only"

    type [<Import("*","three")>] Globals =
        static member REVISION with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        static member CullFaceNone with get(): CullFace = failwith "JS only" and set(v: CullFace): unit = failwith "JS only"
        static member CullFaceBack with get(): CullFace = failwith "JS only" and set(v: CullFace): unit = failwith "JS only"
        static member CullFaceFront with get(): CullFace = failwith "JS only" and set(v: CullFace): unit = failwith "JS only"
        static member CullFaceFrontBack with get(): CullFace = failwith "JS only" and set(v: CullFace): unit = failwith "JS only"
        static member FrontFaceDirectionCW with get(): FrontFaceDirection = failwith "JS only" and set(v: FrontFaceDirection): unit = failwith "JS only"
        static member FrontFaceDirectionCCW with get(): FrontFaceDirection = failwith "JS only" and set(v: FrontFaceDirection): unit = failwith "JS only"
        static member BasicShadowMap with get(): ShadowMapType = failwith "JS only" and set(v: ShadowMapType): unit = failwith "JS only"
        static member PCFShadowMap with get(): ShadowMapType = failwith "JS only" and set(v: ShadowMapType): unit = failwith "JS only"
        static member PCFSoftShadowMap with get(): ShadowMapType = failwith "JS only" and set(v: ShadowMapType): unit = failwith "JS only"
        static member FrontSide with get(): Side = failwith "JS only" and set(v: Side): unit = failwith "JS only"
        static member BackSide with get(): Side = failwith "JS only" and set(v: Side): unit = failwith "JS only"
        static member DoubleSide with get(): Side = failwith "JS only" and set(v: Side): unit = failwith "JS only"
        static member FlatShading with get(): Shading = failwith "JS only" and set(v: Shading): unit = failwith "JS only"
        static member SmoothShading with get(): Shading = failwith "JS only" and set(v: Shading): unit = failwith "JS only"
        static member NoColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        static member FaceColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        static member VertexColors with get(): Colors = failwith "JS only" and set(v: Colors): unit = failwith "JS only"
        static member NoBlending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        static member NormalBlending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        static member AdditiveBlending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        static member SubtractiveBlending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        static member MultiplyBlending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        static member CustomBlending with get(): Blending = failwith "JS only" and set(v: Blending): unit = failwith "JS only"
        static member AddEquation with get(): BlendingEquation = failwith "JS only" and set(v: BlendingEquation): unit = failwith "JS only"
        static member SubtractEquation with get(): BlendingEquation = failwith "JS only" and set(v: BlendingEquation): unit = failwith "JS only"
        static member ReverseSubtractEquation with get(): BlendingEquation = failwith "JS only" and set(v: BlendingEquation): unit = failwith "JS only"
        static member MinEquation with get(): BlendingEquation = failwith "JS only" and set(v: BlendingEquation): unit = failwith "JS only"
        static member MaxEquation with get(): BlendingEquation = failwith "JS only" and set(v: BlendingEquation): unit = failwith "JS only"
        static member ZeroFactor with get(): BlendingDstFactor = failwith "JS only" and set(v: BlendingDstFactor): unit = failwith "JS only"
        static member OneFactor with get(): BlendingDstFactor = failwith "JS only" and set(v: BlendingDstFactor): unit = failwith "JS only"
        static member SrcColorFactor with get(): BlendingDstFactor = failwith "JS only" and set(v: BlendingDstFactor): unit = failwith "JS only"
        static member OneMinusSrcColorFactor with get(): BlendingDstFactor = failwith "JS only" and set(v: BlendingDstFactor): unit = failwith "JS only"
        static member SrcAlphaFactor with get(): BlendingDstFactor = failwith "JS only" and set(v: BlendingDstFactor): unit = failwith "JS only"
        static member OneMinusSrcAlphaFactor with get(): BlendingDstFactor = failwith "JS only" and set(v: BlendingDstFactor): unit = failwith "JS only"
        static member DstAlphaFactor with get(): BlendingDstFactor = failwith "JS only" and set(v: BlendingDstFactor): unit = failwith "JS only"
        static member OneMinusDstAlphaFactor with get(): BlendingDstFactor = failwith "JS only" and set(v: BlendingDstFactor): unit = failwith "JS only"
        static member DstColorFactor with get(): BlendingSrcFactor = failwith "JS only" and set(v: BlendingSrcFactor): unit = failwith "JS only"
        static member OneMinusDstColorFactor with get(): BlendingSrcFactor = failwith "JS only" and set(v: BlendingSrcFactor): unit = failwith "JS only"
        static member SrcAlphaSaturateFactor with get(): BlendingSrcFactor = failwith "JS only" and set(v: BlendingSrcFactor): unit = failwith "JS only"
        static member NeverDepth with get(): DepthModes = failwith "JS only" and set(v: DepthModes): unit = failwith "JS only"
        static member AlwaysDepth with get(): DepthModes = failwith "JS only" and set(v: DepthModes): unit = failwith "JS only"
        static member LessDepth with get(): DepthModes = failwith "JS only" and set(v: DepthModes): unit = failwith "JS only"
        static member LessEqualDepth with get(): DepthModes = failwith "JS only" and set(v: DepthModes): unit = failwith "JS only"
        static member EqualDepth with get(): DepthModes = failwith "JS only" and set(v: DepthModes): unit = failwith "JS only"
        static member GreaterEqualDepth with get(): DepthModes = failwith "JS only" and set(v: DepthModes): unit = failwith "JS only"
        static member GreaterDepth with get(): DepthModes = failwith "JS only" and set(v: DepthModes): unit = failwith "JS only"
        static member NotEqualDepth with get(): DepthModes = failwith "JS only" and set(v: DepthModes): unit = failwith "JS only"
        static member MultiplyOperation with get(): Combine = failwith "JS only" and set(v: Combine): unit = failwith "JS only"
        static member MixOperation with get(): Combine = failwith "JS only" and set(v: Combine): unit = failwith "JS only"
        static member AddOperation with get(): Combine = failwith "JS only" and set(v: Combine): unit = failwith "JS only"
        static member NoToneMapping with get(): ToneMapping = failwith "JS only" and set(v: ToneMapping): unit = failwith "JS only"
        static member LinearToneMapping with get(): ToneMapping = failwith "JS only" and set(v: ToneMapping): unit = failwith "JS only"
        static member ReinhardToneMapping with get(): ToneMapping = failwith "JS only" and set(v: ToneMapping): unit = failwith "JS only"
        static member Uncharted2ToneMapping with get(): ToneMapping = failwith "JS only" and set(v: ToneMapping): unit = failwith "JS only"
        static member CineonToneMapping with get(): ToneMapping = failwith "JS only" and set(v: ToneMapping): unit = failwith "JS only"
        static member UVMapping with get(): Mapping = failwith "JS only" and set(v: Mapping): unit = failwith "JS only"
        static member CubeReflectionMapping with get(): Mapping = failwith "JS only" and set(v: Mapping): unit = failwith "JS only"
        static member CubeRefractionMapping with get(): Mapping = failwith "JS only" and set(v: Mapping): unit = failwith "JS only"
        static member EquirectangularReflectionMapping with get(): Mapping = failwith "JS only" and set(v: Mapping): unit = failwith "JS only"
        static member EquirectangularRefractionMapping with get(): Mapping = failwith "JS only" and set(v: Mapping): unit = failwith "JS only"
        static member SphericalReflectionMapping with get(): Mapping = failwith "JS only" and set(v: Mapping): unit = failwith "JS only"
        static member CubeUVReflectionMapping with get(): Mapping = failwith "JS only" and set(v: Mapping): unit = failwith "JS only"
        static member CubeUVRefractionMapping with get(): Mapping = failwith "JS only" and set(v: Mapping): unit = failwith "JS only"
        static member RepeatWrapping with get(): Wrapping = failwith "JS only" and set(v: Wrapping): unit = failwith "JS only"
        static member ClampToEdgeWrapping with get(): Wrapping = failwith "JS only" and set(v: Wrapping): unit = failwith "JS only"
        static member MirroredRepeatWrapping with get(): Wrapping = failwith "JS only" and set(v: Wrapping): unit = failwith "JS only"
        static member NearestFilter with get(): TextureFilter = failwith "JS only" and set(v: TextureFilter): unit = failwith "JS only"
        static member NearestMipMapNearestFilter with get(): TextureFilter = failwith "JS only" and set(v: TextureFilter): unit = failwith "JS only"
        static member NearestMipMapLinearFilter with get(): TextureFilter = failwith "JS only" and set(v: TextureFilter): unit = failwith "JS only"
        static member LinearFilter with get(): TextureFilter = failwith "JS only" and set(v: TextureFilter): unit = failwith "JS only"
        static member LinearMipMapNearestFilter with get(): TextureFilter = failwith "JS only" and set(v: TextureFilter): unit = failwith "JS only"
        static member LinearMipMapLinearFilter with get(): TextureFilter = failwith "JS only" and set(v: TextureFilter): unit = failwith "JS only"
        static member UnsignedByteType with get(): TextureDataType = failwith "JS only" and set(v: TextureDataType): unit = failwith "JS only"
        static member ByteType with get(): TextureDataType = failwith "JS only" and set(v: TextureDataType): unit = failwith "JS only"
        static member ShortType with get(): TextureDataType = failwith "JS only" and set(v: TextureDataType): unit = failwith "JS only"
        static member UnsignedShortType with get(): TextureDataType = failwith "JS only" and set(v: TextureDataType): unit = failwith "JS only"
        static member IntType with get(): TextureDataType = failwith "JS only" and set(v: TextureDataType): unit = failwith "JS only"
        static member UnsignedIntType with get(): TextureDataType = failwith "JS only" and set(v: TextureDataType): unit = failwith "JS only"
        static member FloatType with get(): TextureDataType = failwith "JS only" and set(v: TextureDataType): unit = failwith "JS only"
        static member HalfFloatType with get(): TextureDataType = failwith "JS only" and set(v: TextureDataType): unit = failwith "JS only"
        static member UnsignedShort4444Type with get(): PixelType = failwith "JS only" and set(v: PixelType): unit = failwith "JS only"
        static member UnsignedShort5551Type with get(): PixelType = failwith "JS only" and set(v: PixelType): unit = failwith "JS only"
        static member UnsignedShort565Type with get(): PixelType = failwith "JS only" and set(v: PixelType): unit = failwith "JS only"
        static member AlphaFormat with get(): PixelFormat = failwith "JS only" and set(v: PixelFormat): unit = failwith "JS only"
        static member RGBFormat with get(): PixelFormat = failwith "JS only" and set(v: PixelFormat): unit = failwith "JS only"
        static member RGBAFormat with get(): PixelFormat = failwith "JS only" and set(v: PixelFormat): unit = failwith "JS only"
        static member LuminanceFormat with get(): PixelFormat = failwith "JS only" and set(v: PixelFormat): unit = failwith "JS only"
        static member LuminanceAlphaFormat with get(): PixelFormat = failwith "JS only" and set(v: PixelFormat): unit = failwith "JS only"
        static member RGBEFormat with get(): PixelFormat = failwith "JS only" and set(v: PixelFormat): unit = failwith "JS only"
        static member RGB_S3TC_DXT1_Format with get(): CompressedPixelFormat = failwith "JS only" and set(v: CompressedPixelFormat): unit = failwith "JS only"
        static member RGBA_S3TC_DXT1_Format with get(): CompressedPixelFormat = failwith "JS only" and set(v: CompressedPixelFormat): unit = failwith "JS only"
        static member RGBA_S3TC_DXT3_Format with get(): CompressedPixelFormat = failwith "JS only" and set(v: CompressedPixelFormat): unit = failwith "JS only"
        static member RGBA_S3TC_DXT5_Format with get(): CompressedPixelFormat = failwith "JS only" and set(v: CompressedPixelFormat): unit = failwith "JS only"
        static member RGB_PVRTC_4BPPV1_Format with get(): CompressedPixelFormat = failwith "JS only" and set(v: CompressedPixelFormat): unit = failwith "JS only"
        static member RGB_PVRTC_2BPPV1_Format with get(): CompressedPixelFormat = failwith "JS only" and set(v: CompressedPixelFormat): unit = failwith "JS only"
        static member RGBA_PVRTC_4BPPV1_Format with get(): CompressedPixelFormat = failwith "JS only" and set(v: CompressedPixelFormat): unit = failwith "JS only"
        static member RGBA_PVRTC_2BPPV1_Format with get(): CompressedPixelFormat = failwith "JS only" and set(v: CompressedPixelFormat): unit = failwith "JS only"
        static member RGB_ETC1_Format with get(): CompressedPixelFormat = failwith "JS only" and set(v: CompressedPixelFormat): unit = failwith "JS only"
        static member LoopOnce with get(): AnimationActionLoopStyles = failwith "JS only" and set(v: AnimationActionLoopStyles): unit = failwith "JS only"
        static member LoopRepeat with get(): AnimationActionLoopStyles = failwith "JS only" and set(v: AnimationActionLoopStyles): unit = failwith "JS only"
        static member LoopPingPong with get(): AnimationActionLoopStyles = failwith "JS only" and set(v: AnimationActionLoopStyles): unit = failwith "JS only"
        static member InterpolateDiscrete with get(): InterpolationModes = failwith "JS only" and set(v: InterpolationModes): unit = failwith "JS only"
        static member InterpolateLinear with get(): InterpolationModes = failwith "JS only" and set(v: InterpolationModes): unit = failwith "JS only"
        static member InterpolateSmooth with get(): InterpolationModes = failwith "JS only" and set(v: InterpolationModes): unit = failwith "JS only"
        static member ZeroCurvatureEnding with get(): InterpolationEndingModes = failwith "JS only" and set(v: InterpolationEndingModes): unit = failwith "JS only"
        static member ZeroSlopeEnding with get(): InterpolationEndingModes = failwith "JS only" and set(v: InterpolationEndingModes): unit = failwith "JS only"
        static member WrapAroundEnding with get(): InterpolationEndingModes = failwith "JS only" and set(v: InterpolationEndingModes): unit = failwith "JS only"
        static member TrianglesDrawModesMode with get(): TrianglesDrawModes = failwith "JS only" and set(v: TrianglesDrawModes): unit = failwith "JS only"
        static member TriangleStripDrawMode with get(): TrianglesDrawModes = failwith "JS only" and set(v: TrianglesDrawModes): unit = failwith "JS only"
        static member TriangleFanDrawMode with get(): TrianglesDrawModes = failwith "JS only" and set(v: TrianglesDrawModes): unit = failwith "JS only"
        static member LinearEncoding with get(): TextureEncoding = failwith "JS only" and set(v: TextureEncoding): unit = failwith "JS only"
        static member sRGBEncoding with get(): TextureEncoding = failwith "JS only" and set(v: TextureEncoding): unit = failwith "JS only"
        static member GammaEncoding with get(): TextureEncoding = failwith "JS only" and set(v: TextureEncoding): unit = failwith "JS only"
        static member RGBEEncoding with get(): TextureEncoding = failwith "JS only" and set(v: TextureEncoding): unit = failwith "JS only"
        static member LogLuvEncoding with get(): TextureEncoding = failwith "JS only" and set(v: TextureEncoding): unit = failwith "JS only"
        static member RGBM7Encoding with get(): TextureEncoding = failwith "JS only" and set(v: TextureEncoding): unit = failwith "JS only"
        static member RGBM16Encoding with get(): TextureEncoding = failwith "JS only" and set(v: TextureEncoding): unit = failwith "JS only"
        static member RGBDEncoding with get(): TextureEncoding = failwith "JS only" and set(v: TextureEncoding): unit = failwith "JS only"
        static member GeometryIdCount with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Object3DIdCount with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member DefaultLoadingManager with get(): LoadingManager = failwith "JS only" and set(v: LoadingManager): unit = failwith "JS only"
        static member MaterialIdCount with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member LineStrip with get(): LineMode = failwith "JS only" and set(v: LineMode): unit = failwith "JS only"
        static member LinePieces with get(): LineMode = failwith "JS only" and set(v: LineMode): unit = failwith "JS only"
        static member ShaderChunk with get(): ShaderChunkType = failwith "JS only" and set(v: ShaderChunkType): unit = failwith "JS only"
        static member ShaderLib with get(): ShaderLibType = failwith "JS only" and set(v: ShaderLibType): unit = failwith "JS only"
        static member UniformsLib with get(): UniformsLibType = failwith "JS only" and set(v: UniformsLibType): unit = failwith "JS only"
        static member TextureIdCount with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member warn(message: obj, [<ParamArray>] optionalParams: obj[]): unit = failwith "JS only"
        static member error(message: obj, [<ParamArray>] optionalParams: obj[]): unit = failwith "JS only"
        static member log(message: obj, [<ParamArray>] optionalParams: obj[]): unit = failwith "JS only"

    module AnimationUtils =
        type [<Import("AnimationUtils","three")>] Globals =
            static member arraySlice(array: obj, from: float, ``to``: float): obj = failwith "JS only"
            static member convertArray(array: obj, ``type``: obj, forceClone: bool): obj = failwith "JS only"
            static member isTypedArray(``object``: obj): bool = failwith "JS only"
            static member getKeyFrameOrder(times: float): ResizeArray<float> = failwith "JS only"
            static member sortedArray(values: ResizeArray<obj>, stride: float, order: ResizeArray<float>): ResizeArray<obj> = failwith "JS only"
            static member flattenJSON(jsonKeys: ResizeArray<string>, times: ResizeArray<obj>, values: ResizeArray<obj>, valuePropertyName: string): unit = failwith "JS only"

    module PropertyBindingModule =
        type [<Import("PropertyBinding.Composite","three")>] Composite(targetGroup: obj, path: obj, ?parsedPath: obj) =
            member __.getValue(array: obj, offset: float): obj = failwith "JS only"
            member __.setValue(array: obj, offset: float): unit = failwith "JS only"
            member __.bind(): unit = failwith "JS only"
            member __.unbind(): unit = failwith "JS only"

    module GeometryUtils =
        type [<Import("GeometryUtils","three")>] Globals =
            static member merge(goemetry1: obj, goemetry2: obj, ?materialIndexOffset: obj): obj = failwith "JS only"
            static member center(geometry: obj): obj = failwith "JS only"

    module Cache =
        type [<Import("Cache","three")>] Globals =
            static member enabled with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            static member files with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            static member add(key: string, file: obj): unit = failwith "JS only"
            static member get(key: string): obj = failwith "JS only"
            static member remove(key: string): unit = failwith "JS only"
            static member clear(): unit = failwith "JS only"

    module ColorKeywords =
        type [<Import("ColorKeywords","three")>] Globals =
            static member aliceblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member antiquewhite with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member aqua with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member aquamarine with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member azure with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member beige with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member bisque with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member black with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member blanchedalmond with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member blue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member blueviolet with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member brown with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member burlywood with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member cadetblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member chartreuse with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member chocolate with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member coral with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member cornflowerblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member cornsilk with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member crimson with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member cyan with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkcyan with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkgoldenrod with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkgray with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkgreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkgrey with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkkhaki with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkmagenta with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkolivegreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkorange with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkorchid with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkred with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darksalmon with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkseagreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkslateblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkslategray with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkslategrey with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkturquoise with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member darkviolet with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member deeppink with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member deepskyblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member dimgray with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member dimgrey with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member dodgerblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member firebrick with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member floralwhite with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member forestgreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member fuchsia with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member gainsboro with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member ghostwhite with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member gold with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member goldenrod with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member gray with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member green with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member greenyellow with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member grey with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member honeydew with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member hotpink with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member indianred with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member indigo with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member ivory with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member khaki with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lavender with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lavenderblush with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lawngreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lemonchiffon with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightcoral with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightcyan with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightgoldenrodyellow with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightgray with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightgreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightgrey with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightpink with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightsalmon with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightseagreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightskyblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightslategray with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightslategrey with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightsteelblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lightyellow with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member lime with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member limegreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member linen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member magenta with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member maroon with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mediumaquamarine with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mediumblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mediumorchid with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mediumpurple with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mediumseagreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mediumslateblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mediumspringgreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mediumturquoise with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mediumvioletred with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member midnightblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mintcream with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member mistyrose with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member moccasin with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member navajowhite with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member navy with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member oldlace with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member olive with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member olivedrab with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member orange with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member orangered with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member orchid with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member palegoldenrod with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member palegreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member paleturquoise with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member palevioletred with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member papayawhip with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member peachpuff with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member peru with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member pink with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member plum with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member powderblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member purple with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member red with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member rosybrown with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member royalblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member saddlebrown with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member salmon with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member sandybrown with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member seagreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member seashell with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member sienna with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member silver with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member skyblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member slateblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member slategray with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member slategrey with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member snow with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member springgreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member steelblue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member tan with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member teal with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member thistle with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member tomato with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member turquoise with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member violet with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member wheat with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member white with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member whitesmoke with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member yellow with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            static member yellowgreen with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

    module Math =
        type [<Import("Math","three")>] Globals =
            static member generateUUID(): string = failwith "JS only"
            static member clamp(value: float, min: float, max: float): float = failwith "JS only"
            static member euclideanModulo(n: float, m: float): float = failwith "JS only"
            static member mapLinear(x: float, a1: float, a2: float, b1: float, b2: float): float = failwith "JS only"
            static member smoothstep(x: float, min: float, max: float): float = failwith "JS only"
            static member smootherstep(x: float, min: float, max: float): float = failwith "JS only"
            static member random16(): float = failwith "JS only"
            static member randInt(low: float, high: float): float = failwith "JS only"
            static member randFloat(low: float, high: float): float = failwith "JS only"
            static member randFloatSpread(range: float): float = failwith "JS only"
            static member degToRad(degrees: float): float = failwith "JS only"
            static member radToDeg(radians: float): float = failwith "JS only"
            static member isPowerOfTwo(value: float): bool = failwith "JS only"
            static member nearestPowerOfTwo(value: float): float = failwith "JS only"
            static member nextPowerOfTwo(value: float): float = failwith "JS only"

    module UniformsUtils =
        type [<Import("UniformsUtils","three")>] Globals =
            static member merge(uniforms: ResizeArray<obj>): obj = failwith "JS only"
            static member clone(uniforms_src: obj): obj = failwith "JS only"

    module CurveUtils =
        type [<Import("CurveUtils","three")>] Globals =
            static member tangentQuadraticBezier(t: float, p0: float, p1: float, p2: float): float = failwith "JS only"
            static member tangentCubicBezier(t: float, p0: float, p1: float, p2: float, p3: float): float = failwith "JS only"
            static member tangentSpline(t: float, p0: float, p1: float, p2: float, p3: float): float = failwith "JS only"
            static member interpolate(p0: float, p1: float, p2: float, p3: float, t: float): float = failwith "JS only"

    module ImageUtils =
        type [<Import("ImageUtils","three")>] Globals =
            static member crossOrigin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member loadTexture(url: string, ?mapping: Mapping, ?onLoad: Func<Texture, unit>, ?onError: Func<string, unit>): Texture = failwith "JS only"
            static member loadTextureCube(array: ResizeArray<string>, ?mapping: Mapping, ?onLoad: Func<Texture, unit>, ?onError: Func<string, unit>): Texture = failwith "JS only"

    module SceneUtils =
        type [<Import("SceneUtils","three")>] Globals =
            static member createMultiMaterialObject(geometry: Geometry, materials: ResizeArray<Material>): Object3D = failwith "JS only"
            static member detach(child: Object3D, parent: Object3D, scene: Scene): unit = failwith "JS only"
            static member attach(child: Object3D, scene: Scene, parent: Object3D): unit = failwith "JS only"

    module ShapeUtils =
        type [<Import("ShapeUtils","three")>] Globals =
            static member area(contour: ResizeArray<float>): float = failwith "JS only"
            static member triangulate(contour: ResizeArray<float>, indices: bool): ResizeArray<float> = failwith "JS only"
            static member triangulateShape(contour: ResizeArray<float>, holes: ResizeArray<obj>): ResizeArray<float> = failwith "JS only"
            static member isClockWise(pts: ResizeArray<float>): bool = failwith "JS only"
            static member b2(t: float, p0: float, p1: float, p2: float): float = failwith "JS only"
            static member b3(t: float, p0: float, p1: float, p2: float, p3: float): float = failwith "JS only"
