// Type definitions for WebGL Extensions
// Project: http://webgl.org/
// Definitions by: Arthur Langereis <https://github.com/zenmumbler/>
// Definitions: https://github.com/DefinitelyTyped/DefinitelyTyped/webgl-ext

// These definitions go beyond those already defined in TS 1.6.2 stdlib
// All non-draft WebGL 1.0 extensions and prefixed extension names are
// covered.

namespace Fable.Import
open System
open Fable.Core
open Fable.Import.JS

module WebGLExt =
    open Fable.Import.Browser

    type ANGLEInstancedArrays =
        abstract VERTEX_ATTRIB_ARRAY_DIVISOR_ANGLE: float with get, set
        abstract drawArraysInstancedANGLE: mode: float * first: float * count: float * primcount: float -> unit
        abstract drawElementsInstancedANGLE: mode: float * count: float * ``type``: float * offset: float * primcount: float -> unit
        abstract vertexAttribDivisorANGLE: index: float * divisor: float -> unit

    and EXTBlendMinMax =
        abstract MIN_EXT: float with get, set
        abstract MAX_EXT: float with get, set

    and EXTColorBufferHalfFloat =
        abstract RGBA16F_EXT: float with get, set
        abstract RGB16F_EXT: float with get, set
        abstract FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT: float with get, set
        abstract UNSIGNED_NORMALIZED_EXT: float with get, set

    and EXTFragDepth =
        interface end

    and EXTsRGB =
        abstract SRGB_EXT: float with get, set
        abstract SRGB_ALPHA_EXT: float with get, set
        abstract SRGB8_ALPHA8_EXT: float with get, set
        abstract FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_EXT: float with get, set

    and EXTShaderTextureLOD =
        interface end

    and EXTTextureFilterAnisotropic =
        abstract TEXTURE_MAX_ANISOTROPY_EXT: float with get, set
        abstract MAX_TEXTURE_MAX_ANISOTROPY_EXT: float with get, set

    and OESElementIndexUint =
        interface end

    and OESStandardDerivatives =
        abstract FRAGMENT_SHADER_DERIVATIVE_HINT_OES: float with get, set

    and OESTextureFloat =
        interface end

    and OESTextureFloatLinear =
        interface end

    and OESTextureHalfFloat =
        abstract HALF_FLOAT_OES: float with get, set

    and OESTextureHalfFloatLinear =
        interface end

    and WebGLVertexArrayObjectOES =
        inherit WebGLObject

    and OESVertexArrayObject =
        abstract VERTEX_ARRAY_BINDING_OES: float with get, set
        abstract createVertexArrayOES: unit -> WebGLVertexArrayObjectOES
        abstract deleteVertexArrayOES: arrayObject: WebGLVertexArrayObjectOES -> unit
        abstract isVertexArrayOES: arrayObject: WebGLVertexArrayObjectOES -> bool
        abstract bindVertexArrayOES: arrayObject: WebGLVertexArrayObjectOES -> unit

    and WebGLColorBufferFloat =
        abstract RGBA32F_EXT: float with get, set
        abstract FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT: float with get, set
        abstract UNSIGNED_NORMALIZED_EXT: float with get, set

    and WebGLCompressedTextureATC =
        abstract COMPRESSED_RGB_ATC_WEBGL: float with get, set
        abstract COMPRESSED_RGBA_ATC_EXPLICIT_ALPHA_WEBGL: float with get, set
        abstract COMPRESSED_RGBA_ATC_INTERPOLATED_ALPHA_WEBGL: float with get, set

    and WebGLCompressedTextureETC1 =
        abstract COMPRESSED_RGB_ETC1_WEBGL: float with get, set

    and WebGLCompressedTexturePVRTC =
        abstract COMPRESSED_RGB_PVRTC_4BPPV1_IMG: float with get, set
        abstract COMPRESSED_RGB_PVRTC_2BPPV1_IMG: float with get, set
        abstract COMPRESSED_RGBA_PVRTC_4BPPV1_IMG: float with get, set
        abstract COMPRESSED_RGBA_PVRTC_2BPPV1_IMG: float with get, set

    and WebGLCompressedTextureS3TC =
        abstract COMPRESSED_RGB_S3TC_DXT1_EXT: float with get, set
        abstract COMPRESSED_RGBA_S3TC_DXT1_EXT: float with get, set
        abstract COMPRESSED_RGBA_S3TC_DXT3_EXT: float with get, set
        abstract COMPRESSED_RGBA_S3TC_DXT5_EXT: float with get, set

    and WebGLDebugRendererInfo =
        abstract UNMASKED_VENDOR_WEBGL: float with get, set
        abstract UNMASKED_RENDERER_WEBGL: float with get, set

    and WebGLDebugShaders =
        abstract getTranslatedShaderSource: shader: WebGLShader -> string

    and WebGLDepthTexture =
        abstract UNSIGNED_INT_24_8_WEBGL: float with get, set

    and WebGLDrawBuffers =
        abstract COLOR_ATTACHMENT0_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT1_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT2_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT3_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT4_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT5_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT6_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT7_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT8_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT9_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT10_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT11_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT12_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT13_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT14_WEBGL: float with get, set
        abstract COLOR_ATTACHMENT15_WEBGL: float with get, set
        abstract DRAW_BUFFER0_WEBGL: float with get, set
        abstract DRAW_BUFFER1_WEBGL: float with get, set
        abstract DRAW_BUFFER2_WEBGL: float with get, set
        abstract DRAW_BUFFER3_WEBGL: float with get, set
        abstract DRAW_BUFFER4_WEBGL: float with get, set
        abstract DRAW_BUFFER5_WEBGL: float with get, set
        abstract DRAW_BUFFER6_WEBGL: float with get, set
        abstract DRAW_BUFFER7_WEBGL: float with get, set
        abstract DRAW_BUFFER8_WEBGL: float with get, set
        abstract DRAW_BUFFER9_WEBGL: float with get, set
        abstract DRAW_BUFFER10_WEBGL: float with get, set
        abstract DRAW_BUFFER11_WEBGL: float with get, set
        abstract DRAW_BUFFER12_WEBGL: float with get, set
        abstract DRAW_BUFFER13_WEBGL: float with get, set
        abstract DRAW_BUFFER14_WEBGL: float with get, set
        abstract DRAW_BUFFER15_WEBGL: float with get, set
        abstract MAX_COLOR_ATTACHMENTS_WEBGL: float with get, set
        abstract MAX_DRAW_BUFFERS_WEBGL: float with get, set
        abstract drawBuffersWEBGL: buffers: ResizeArray<float> -> unit

    and WebGLLoseContext =
        abstract loseContext: unit -> unit
        abstract restoreContext: unit -> unit

    type HTMLCanvasElement with
        [<Emit("$0.getContext('webgl')")>] member __.getContext_webgl: unit -> WebGLRenderingContext = failwith "JS only"

    type WebGLRenderingContext with
        [<Emit("$0.getExtension('ANGLE_instanced_arrays')")>] member __.getExtension_ANGLE_instanced_arrays: unit -> ANGLEInstancedArrays = failwith "JS only"
        [<Emit("$0.getExtension('EXT_blend_minmax')")>] member __.getExtension_EXT_blend_minmax: unit -> EXTBlendMinMax = failwith "JS only"
        [<Emit("$0.getExtension('EXT_color_buffer_half_float')")>] member __.getExtension_EXT_color_buffer_half_float: unit -> EXTColorBufferHalfFloat = failwith "JS only"
        [<Emit("$0.getExtension('EXT_frag_depth')")>] member __.getExtension_EXT_frag_depth: unit -> EXTFragDepth = failwith "JS only"
        [<Emit("$0.getExtension('EXT_sRGB')")>] member __.getExtension_EXT_sRGB: unit -> EXTsRGB = failwith "JS only"
        [<Emit("$0.getExtension('EXT_shader_texture_lod')")>] member __.getExtension_EXT_shader_texture_lod: unit -> EXTShaderTextureLOD = failwith "JS only"
        [<Emit("$0.getExtension('EXT_texture_filter_anisotropic')")>] member __.getExtension_EXT_texture_filter_anisotropic: unit -> EXTTextureFilterAnisotropic = failwith "JS only"
        [<Emit("$0.getExtension('OES_element_index_uint')")>] member __.getExtension_OES_element_index_uint: unit -> OESElementIndexUint = failwith "JS only"
        [<Emit("$0.getExtension('OES_standard_derivatives')")>] member __.getExtension_OES_standard_derivatives: unit -> OESStandardDerivatives = failwith "JS only"
        [<Emit("$0.getExtension('OES_texture_float')")>] member __.getExtension_OES_texture_float: unit -> OESTextureFloat = failwith "JS only"
        [<Emit("$0.getExtension('OES_texture_float_linear')")>] member __.getExtension_OES_texture_float_linear: unit -> OESTextureFloatLinear = failwith "JS only"
        [<Emit("$0.getExtension('OES_texture_half_float')")>] member __.getExtension_OES_texture_half_float: unit -> OESTextureHalfFloat = failwith "JS only"
        [<Emit("$0.getExtension('OES_texture_half_float_linear')")>] member __.getExtension_OES_texture_half_float_linear: unit -> OESTextureHalfFloatLinear = failwith "JS only"
        [<Emit("$0.getExtension('OES_vertex_array_object')")>] member __.getExtension_OES_vertex_array_object: unit -> OESVertexArrayObject = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_color_buffer_float')")>] member __.getExtension_WEBGL_color_buffer_float: unit -> WebGLColorBufferFloat = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_compressed_texture_atc')")>] member __.getExtension_WEBGL_compressed_texture_atc: unit -> WebGLCompressedTextureATC = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_compressed_texture_etc1')")>] member __.getExtension_WEBGL_compressed_texture_etc1: unit -> WebGLCompressedTextureETC1 = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_compressed_texture_pvrtc')")>] member __.getExtension_WEBGL_compressed_texture_pvrtc: unit -> WebGLCompressedTexturePVRTC = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_compressed_texture_s3tc')")>] member __.getExtension_WEBGL_compressed_texture_s3tc: unit -> WebGLCompressedTextureS3TC = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_debug_renderer_info')")>] member __.getExtension_WEBGL_debug_renderer_info: unit -> WebGLDebugRendererInfo = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_debug_shaders')")>] member __.getExtension_WEBGL_debug_shaders: unit -> WebGLDebugShaders = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_depth_texture')")>] member __.getExtension_WEBGL_depth_texture: unit -> WebGLDepthTexture = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_draw_buffers')")>] member __.getExtension_WEBGL_draw_buffers: unit -> WebGLDrawBuffers = failwith "JS only"
        [<Emit("$0.getExtension('WEBGL_lose_context')")>] member __.getExtension_WEBGL_lose_context: unit -> WebGLLoseContext = failwith "JS only"
        [<Emit("$0.getExtension('WEBKIT_EXT_texture_filter_anisotropic')")>] member __.getExtension_WEBKIT_EXT_texture_filter_anisotropic: unit -> EXTTextureFilterAnisotropic = failwith "JS only"
        [<Emit("$0.getExtension('WEBKIT_WEBGL_compressed_texture_atc')")>] member __.getExtension_WEBKIT_WEBGL_compressed_texture_atc: unit -> WebGLCompressedTextureATC = failwith "JS only"
        [<Emit("$0.getExtension('WEBKIT_WEBGL_compressed_texture_pvrtc')")>] member __.getExtension_WEBKIT_WEBGL_compressed_texture_pvrtc: unit -> WebGLCompressedTexturePVRTC = failwith "JS only"
        [<Emit("$0.getExtension('WEBKIT_WEBGL_compressed_texture_s3tc')")>] member __.getExtension_WEBKIT_WEBGL_compressed_texture_s3tc: unit -> WebGLCompressedTextureS3TC = failwith "JS only"
        [<Emit("$0.getExtension('WEBKIT_WEBGL_depth_texture')")>] member __.getExtension_WEBKIT_WEBGL_depth_texture: unit -> WebGLDepthTexture = failwith "JS only"
        [<Emit("$0.getExtension('WEBKIT_WEBGL_lose_context')")>] member __.getExtension_WEBKIT_WEBGL_lose_context: unit -> WebGLLoseContext = failwith "JS only"
        [<Emit("$0.getExtension('MOZ_WEBGL_compressed_texture_s3tc')")>] member __.getExtension_MOZ_WEBGL_compressed_texture_s3tc: unit -> WebGLCompressedTextureS3TC = failwith "JS only"
        [<Emit("$0.getExtension('MOZ_WEBGL_depth_texture')")>] member __.getExtension_MOZ_WEBGL_depth_texture: unit -> WebGLDepthTexture = failwith "JS only"
        [<Emit("$0.getExtension('MOZ_WEBGL_lose_context')")>] member __.getExtension_MOZ_WEBGL_lose_context: unit -> WebGLLoseContext = failwith "JS only"
