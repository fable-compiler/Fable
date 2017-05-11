// Type definitions for Pixi.js 3.0.9 dev
// Project: https://github.com/GoodBoyDigital/pixi.js/
// Definitions by: clark-stevenson <https://github.com/pixijs/pixi-typescript>
// Definitions: https://github.com/DefinitelyTyped/DefinitelyTyped

namespace Fable.Import

#nowarn "1182"

open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

[<Erase>]
module PIXI =
    type RendererType =
        abstract member UNKNOWN: float
        abstract member WEBGL: float
        abstract member CANVAS: float

    type BlendModes =
        abstract member NORMAL: float
        abstract member ADD: float
        abstract member MULTIPLY: float
        abstract member SCREEN: float
        abstract member OVERLAY: float
        abstract member DARKEN: float
        abstract member LIGHTEN: float
        abstract member COLOR_DODGE: float
        abstract member COLOR_BURN: float
        abstract member HARD_LIGHT: float
        abstract member SOFT_LIGHT: float
        abstract member DIFFERENCE: float
        abstract member EXCLUSION: float
        abstract member HUE: float
        abstract member SATURATION: float
        abstract member COLOR: float
        abstract member LUMINOSITY: float

    type DrawModes =
        abstract member POINTS: float
        abstract member LINES: float
        abstract member LINE_LOOP: float
        abstract member LINE_STRIP: float
        abstract member TRIANGLES: float
        abstract member TRIANGLE_STRIP: float
        abstract member TRIANGLE_FAN: float

    type ScaleModes =
        abstract member DEFAULT: float
        abstract member LINEAR: float
        abstract member NEAREST: float

    type DefaultRenderOptions =
        abstract member view: HTMLCanvasElement
        abstract member resolution: float
        abstract member antialias: bool
        abstract member forceFXAA: bool
        abstract member autoResize: bool
        abstract member transparent: bool
        abstract member backgroundColor: float
        abstract member clearBeforeRender: bool
        abstract member preserveDrawingBuffer: bool
        abstract member roundPixels: bool

    type Shapes =
        abstract member POLY: float
        abstract member RECT: float
        abstract member CIRC: float
        abstract member ELIP: float
        abstract member RREC: float

    type InteractionEvent =
        abstract stopped: bool with get, set
        abstract target: obj with get, set
        abstract ``type``: string with get, set
        abstract data: obj (*InteractionData*) with get, set
        abstract stopPropagation: unit -> unit

    type [<Import("EventEmitter","PIXI")>] EventEmitter() =
        member __.listeners(``event``: string): ResizeArray<Function> = failwith "JS only"
        member __.emit(``event``: string, [<ParamArray>] args: obj[]): bool = failwith "JS only"
        member __.on(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"
        member __.once(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"
        member __.removeListener(``event``: string, ?fn: Function, ?context: obj, ?once: bool): EventEmitter = failwith "JS only"
        member __.removeAllListeners(?``event``: string): EventEmitter = failwith "JS only"
        member __.off(``event``: string, ?fn: Function, ?context: obj, ?once: bool): EventEmitter = failwith "JS only"
        member __.addListener(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"

    and [<Import("DisplayObject","PIXI")>] DisplayObject() =
        inherit EventEmitter()
        // interface interaction.InteractiveTarget
        member __._originalRenderWebGL with get(): WebGLRenderer = failwith "JS only" and set(v: WebGLRenderer): unit = failwith "JS only"
        member __._originalRenderCanvas with get(): CanvasRenderer = failwith "JS only" and set(v: CanvasRenderer): unit = failwith "JS only"
        member __._originalUpdateTransform with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __._originalHitTest with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __._cachedSprite with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __._originalDestroy with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.cacheAsBitmap with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __._sr with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __._cr with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __._bounds with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __._currentBounds with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __._mask with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __._cachedObject with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.position with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
        member __.scale with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
        member __.pivot with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
        member __.rotation with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.renderable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.alpha with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.visible with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.parent with get(): Container = failwith "JS only" and set(v: Container): unit = failwith "JS only"
        member __.worldAlpha with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.worldTransform with get(): Matrix = failwith "JS only" and set(v: Matrix): unit = failwith "JS only"
        member __.filterArea with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.worldVisible with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.mask with get(): U2<Graphics, Sprite> option = failwith "JS only" and set(v: U2<Graphics, Sprite> option): unit = failwith "JS only"
        member __.filters with get(): ResizeArray<AbstractFilter> option = failwith "JS only" and set(v: ResizeArray<AbstractFilter> option): unit = failwith "JS only"
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.interactive with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.buttonMode with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.interactiveChildren with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.defaultCursor with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.hitArea with get(): HitArea = failwith "JS only" and set(v: HitArea): unit = failwith "JS only"
        member __._renderCachedWebGL(renderer: WebGLRenderer): unit = failwith "JS only"
        member __._initCachedDisplayObject(renderer: WebGLRenderer): unit = failwith "JS only"
        member __._renderCachedCanvas(renderer: CanvasRenderer): unit = failwith "JS only"
        member __._initCachedDisplayObjectCanvas(renderer: CanvasRenderer): unit = failwith "JS only"
        member __._getCachedBounds(): Rectangle = failwith "JS only"
        member __._destroyCachedDisplayObject(): unit = failwith "JS only"
        member __._cacheAsBitmapDestroy(): unit = failwith "JS only"
        member __.updateTransform(): unit = failwith "JS only"
        member __.getBounds(?matrix: Matrix): Rectangle = failwith "JS only"
        member __.getLocalBounds(): Rectangle = failwith "JS only"
        member __.toGlobal(position: Point): Point = failwith "JS only"
        member __.toLocal(position: Point, ?from: DisplayObject): Point = failwith "JS only"
        member __.generateTexture(renderer: U2<CanvasRenderer, WebGLRenderer>, scaleMode: float, resolution: float): Texture = failwith "JS only"
        member __.setParent(container: Container): Container = failwith "JS only"
        member __.setTransform(?x: float, ?y: float, ?scaleX: float, ?scaleY: float, ?rotation: float, ?skewX: float, ?skewY: float, ?pivotX: float, ?pivotY: float): DisplayObject = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"
        member __.getChildByName(name: string): DisplayObject = failwith "JS only"
        member __.getGlobalPosition(point: Point): Point = failwith "JS only"
        [<Emit("$0.on('click',$1...)")>] member __.on_click(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('mousedown',$1...)")>] member __.on_mousedown(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('mouseout',$1...)")>] member __.on_mouseout(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('mouseover',$1...)")>] member __.on_mouseover(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('mouseup',$1...)")>] member __.on_mouseup(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('mouseclick',$1...)")>] member __.on_mouseclick(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('mouseupoutside',$1...)")>] member __.on_mouseupoutside(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('rightclick',$1...)")>] member __.on_rightclick(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('rightdown',$1...)")>] member __.on_rightdown(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('rightup',$1...)")>] member __.on_rightup(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('rightupoutside',$1...)")>] member __.on_rightupoutside(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('tap',$1...)")>] member __.on_tap(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('touchend',$1...)")>] member __.on_touchend(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('touchendoutside',$1...)")>] member __.on_touchendoutside(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('touchmove',$1...)")>] member __.on_touchmove(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('touchstart',$1...)")>] member __.on_touchstart(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        member __.on(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('click',$1...)")>] member __.once_click(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('mousedown',$1...)")>] member __.once_mousedown(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('mouseout',$1...)")>] member __.once_mouseout(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('mouseover',$1...)")>] member __.once_mouseover(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('mouseup',$1...)")>] member __.once_mouseup(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('mouseclick',$1...)")>] member __.once_mouseclick(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('mouseupoutside',$1...)")>] member __.once_mouseupoutside(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('rightclick',$1...)")>] member __.once_rightclick(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('rightdown',$1...)")>] member __.once_rightdown(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('rightup',$1...)")>] member __.once_rightup(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('rightupoutside',$1...)")>] member __.once_rightupoutside(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('tap',$1...)")>] member __.once_tap(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('touchend',$1...)")>] member __.once_touchend(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('touchendoutside',$1...)")>] member __.once_touchendoutside(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('touchmove',$1...)")>] member __.once_touchmove(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('touchstart',$1...)")>] member __.once_touchstart(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        member __.once(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"

    and [<Import("Container","PIXI")>] Container() =
        inherit DisplayObject()
        member __.onChildrenChange with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.children with get(): ResizeArray<DisplayObject> = failwith "JS only" and set(v: ResizeArray<DisplayObject>): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __._renderWebGL(renderer: WebGLRenderer): unit = failwith "JS only"
        member __._renderCanvas(renderer: CanvasRenderer): unit = failwith "JS only"
        member __.addChild([<ParamArray>] child: DisplayObject[]): DisplayObject = failwith "JS only"
        member __.addChildAt(child: DisplayObject, index: float): DisplayObject = failwith "JS only"
        member __.swapChildren(child: DisplayObject, child2: DisplayObject): unit = failwith "JS only"
        member __.getChildIndex(child: DisplayObject): float = failwith "JS only"
        member __.setChildIndex(child: DisplayObject, index: float): unit = failwith "JS only"
        member __.getChildAt(index: float): DisplayObject = failwith "JS only"
        member __.removeChild(child: DisplayObject): DisplayObject = failwith "JS only"
        member __.removeChildAt(index: float): DisplayObject = failwith "JS only"
        member __.removeChildren(?beginIndex: float, ?endIndex: float): ResizeArray<DisplayObject> = failwith "JS only"
        member __.destroy(?destroyChildren: bool): unit = failwith "JS only"
        member __.generateTexture(renderer: U2<CanvasRenderer, WebGLRenderer>, ?resolution: float, ?scaleMode: float): Texture = failwith "JS only"
        member __.renderWebGL(renderer: WebGLRenderer): unit = failwith "JS only"
        member __.renderCanvas(renderer: CanvasRenderer): unit = failwith "JS only"
        [<Emit("$0.once('added',$1...)")>] member __.once_added(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        member __.once(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('removed',$1...)")>] member __.once_removed(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('added',$1...)")>] member __.on_added(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"
        member __.on(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('removed',$1...)")>] member __.on_removed(fn: Func<InteractionEvent, unit>, ?context: obj): EventEmitter = failwith "JS only"

    and [<Import("GraphicsData","PIXI")>] GraphicsData(lineWidth: float, lineColor: float, lineAlpha: float, fillColor: float, fillAlpha: float, fill: bool, shape: U4<Circle, Rectangle, Ellipse, Polygon>) =
        member __.lineWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.lineColor with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.lineAlpha with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.fillColor with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.fillAlpha with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.fill with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.shape with get(): U4<Circle, Rectangle, Ellipse, Polygon> = failwith "JS only" and set(v: U4<Circle, Rectangle, Ellipse, Polygon>): unit = failwith "JS only"
        member __.``type`` with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __._lineTint with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __._fillTint with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): GraphicsData = failwith "JS only"

    and [<Import("Graphics","PIXI")>] Graphics() =
        inherit Container()
        member __.boundsDirty with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.dirty with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.glDirty with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.lineWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.lineColor with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.tint with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.blendMode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.isMask with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.boundsPadding with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): Graphics = failwith "JS only"
        member __.lineStyle(?lineWidth: float, ?color: float, ?alpha: float): Graphics = failwith "JS only"
        member __.moveTo(x: float, y: float): Graphics = failwith "JS only"
        member __.lineTo(x: float, y: float): Graphics = failwith "JS only"
        member __.quadraticCurveTo(cpX: float, cpY: float, toX: float, toY: float): Graphics = failwith "JS only"
        member __.bezierCurveTo(cpX: float, cpY: float, cpX2: float, cpY2: float, toX: float, toY: float): Graphics = failwith "JS only"
        member __.arcTo(x1: float, y1: float, x2: float, y2: float, radius: float): Graphics = failwith "JS only"
        member __.arc(cx: float, cy: float, radius: float, startAngle: float, endAngle: float, ?anticlockwise: bool): Graphics = failwith "JS only"
        member __.beginFill(color: float, ?alpha: float): Graphics = failwith "JS only"
        member __.endFill(): Graphics = failwith "JS only"
        member __.drawRect(x: float, y: float, width: float, height: float): Graphics = failwith "JS only"
        member __.drawRoundedRect(x: float, y: float, width: float, height: float, radius: float): Graphics = failwith "JS only"
        member __.drawCircle(x: float, y: float, radius: float): Graphics = failwith "JS only"
        member __.drawEllipse(x: float, y: float, width: float, height: float): Graphics = failwith "JS only"
        member __.drawPolygon(path: U2<ResizeArray<float>, ResizeArray<Point>>): Graphics = failwith "JS only"
        member __.clear(): Graphics = failwith "JS only"
        member __.generateTexture(renderer: U2<WebGLRenderer, CanvasRenderer>, ?resolution: float, ?scaleMode: float): Texture = failwith "JS only"
        member __.getBounds(?matrix: Matrix): Rectangle = failwith "JS only"
        member __.containsPoint(point: Point): bool = failwith "JS only"
        member __.updateLocalBounds(): unit = failwith "JS only"
        member __.drawShape(shape: U4<Circle, Rectangle, Ellipse, Polygon>): GraphicsData = failwith "JS only"

    and GraphicsRenderer =
        abstract start: unit -> unit
        abstract stop: unit -> unit
        abstract flush: unit -> unit
        abstract render: ?``object``: obj -> unit


    and WebGLGraphicsData =
        interface end

    and [<Import("Point","PIXI")>] Point(?x: float, ?y: float) =
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): Point = failwith "JS only"
        member __.copy(p: Point): unit = failwith "JS only"
        member __.equals(p: Point): bool = failwith "JS only"
        member __.set(?x: float, ?y: float): unit = failwith "JS only"

    and [<Import("Matrix","PIXI")>] Matrix() =
        member __.a with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.b with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.c with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.d with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.tx with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.ty with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.IDENTITY with get(): Matrix = failwith "JS only" and set(v: Matrix): unit = failwith "JS only"
        member __.TEMP_MATRIX with get(): Matrix = failwith "JS only" and set(v: Matrix): unit = failwith "JS only"
        member __.fromArray(array: ResizeArray<float>): unit = failwith "JS only"
        member __.toArray(?transpose: bool, ?out: ResizeArray<float>): ResizeArray<float> = failwith "JS only"
        member __.apply(pos: Point, ?newPos: Point): Point = failwith "JS only"
        member __.applyInverse(pos: Point, ?newPos: Point): Point = failwith "JS only"
        member __.translate(x: float, y: float): Matrix = failwith "JS only"
        member __.scale(x: float, y: float): Matrix = failwith "JS only"
        member __.rotate(angle: float): Matrix = failwith "JS only"
        member __.append(matrix: Matrix): Matrix = failwith "JS only"
        member __.prepend(matrix: Matrix): Matrix = failwith "JS only"
        member __.invert(): Matrix = failwith "JS only"
        member __.identity(): Matrix = failwith "JS only"
        member __.clone(): Matrix = failwith "JS only"
        member __.copy(matrix: Matrix): Matrix = failwith "JS only"
        member __.set(a: float, b: float, c: float, d: float, tx: float, ty: float): Matrix = failwith "JS only"
        member __.setTransform(a: float, b: float, c: float, d: float, sr: float, cr: float, cy: float, sy: float, nsx: float, cs: float): Matrix = failwith "JS only"

    and HitArea =
        abstract contains: x: float * y: float -> bool

    and [<Import("Circle","PIXI")>] Circle(?x: float, ?y: float, ?radius: float) =
        interface HitArea with
            member __.contains(x: float, y: float): bool = failwith "JS only"
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.radius with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.``type`` with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): Circle = failwith "JS only"
        member __.getBounds(): Rectangle = failwith "JS only"

    and [<Import("Ellipse","PIXI")>] Ellipse(?x: float, ?y: float, ?width: float, ?height: float) =
        interface HitArea with
            member __.contains(x: float, y: float): bool = failwith "JS only"
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.``type`` with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): Ellipse = failwith "JS only"
        member __.getBounds(): Rectangle = failwith "JS only"

    and [<Import("Polygon","PIXI")>] Polygon([<ParamArray>] points: float[]) =
        interface HitArea with
            member __.contains(x: float, y: float): bool = failwith "JS only"
        member __.closed with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.points with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.``type`` with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): Polygon = failwith "JS only"

    and [<Import("Rectangle","PIXI")>] Rectangle(?x: float, ?y: float, ?width: float, ?height: float) =
        interface HitArea with
            member __.contains(x: float, y: float): bool = failwith "JS only"
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.``type`` with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.EMPTY with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __.clone(): Rectangle = failwith "JS only"

    and [<Import("RoundedRectangle","PIXI")>] RoundedRectangle(?x: float, ?y: float, ?width: float, ?height: float, ?radius: float) =
        interface HitArea with
            member __.contains(x: float, y: float): bool = failwith "JS only"
        member __.x with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.radius with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.``type`` with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.EMPTY with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __.clone(): Rectangle = failwith "JS only"

    and ParticleContainerProperties =
        | Scale of bool 
        | Position of bool
        | Rotation of bool 
        | Uvs of bool
        | Alpha of bool
        
    and [<Import("ParticleContainer","PIXI")>] ParticleContainer(?size: float, ?properties: ParticleContainerProperties list, ?batchSize: float) =
        inherit Container()
        member __._maxSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __._batchSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __._properties with get(): ResizeArray<bool> = failwith "JS only" and set(v: ResizeArray<bool>): unit = failwith "JS only"
        member __._buffers with get(): ResizeArray<WebGLBuffer> = failwith "JS only" and set(v: ResizeArray<WebGLBuffer>): unit = failwith "JS only"
        member __._bufferToUpdate with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.onChildrenChange with get(): Func<float, unit> = failwith "JS only" and set(v: Func<float, unit>): unit = failwith "JS only"
        member __.interactiveChildren with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.blendMode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.roundPixels with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.setProperties(properties: ParticleContainerProperties list): unit = failwith "JS only"

    and ParticleBuffer =
        abstract gl: WebGLRenderingContext with get, set
        abstract vertSize: float with get, set
        abstract vertByteSize: float with get, set
        abstract size: float with get, set
        abstract dynamicProperties: ResizeArray<obj> with get, set
        abstract staticProperties: ResizeArray<obj> with get, set
        abstract staticStride: float with get, set
        abstract staticBuffer: obj with get, set
        abstract staticData: obj with get, set
        abstract dynamicStride: float with get, set
        abstract dynamicBuffer: obj with get, set
        abstract dynamicData: obj with get, set
        abstract initBuffers: unit -> unit
        abstract bind: unit -> unit
        abstract destroy: unit -> unit

    and ParticleRenderer =
        interface end

    and ParticleShader =
        interface end

    and RendererOptions =
        | View of HTMLCanvasElement
        | Transparent of bool
        | Antialias of bool
        | AutoResize of bool
        | Resolution of float
        | ClearBeforeRendering of bool
        | PreserveDrawingBuffer of bool
        | ForceFXAA of bool
        | RoundPixels of bool
        | BackgroundColor of float

    and [<Import("SystemRenderer","PIXI")>] SystemRenderer(system: string, ?width: float, ?height: float, ?options: RendererOptions list) =
        inherit EventEmitter()
        member __._backgroundColor with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __._backgroundColorRgb with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __._backgroundColorString with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __._tempDisplayObjectParent with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __._lastObjectRendered with get(): DisplayObject = failwith "JS only" and set(v: DisplayObject): unit = failwith "JS only"
        member __.``type`` with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.view with get(): HTMLCanvasElement = failwith "JS only" and set(v: HTMLCanvasElement): unit = failwith "JS only"
        member __.resolution with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.transparent with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.autoResize with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.blendModes with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.preserveDrawingBuffer with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.clearBeforeRender with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.roundPixels with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.backgroundColor with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.render(``object``: DisplayObject): unit = failwith "JS only"
        member __.resize(width: float, height: float): unit = failwith "JS only"
        member __.destroy(?removeView: bool): unit = failwith "JS only"

    and [<Import("CanvasRenderer","PIXI")>] CanvasRenderer(?width: float, ?height: float, ?options: RendererOptions list) =
        inherit SystemRenderer("")
        member __.context with get(): CanvasRenderingContext2D = failwith "JS only" and set(v: CanvasRenderingContext2D): unit = failwith "JS only"
        member __.refresh with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.maskManager with get(): CanvasMaskManager = failwith "JS only" and set(v: CanvasMaskManager): unit = failwith "JS only"
        member __.roundPixels with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.smoothProperty with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.renderDisplayObject(displayObject: DisplayObject, context: CanvasRenderingContext2D): unit = failwith "JS only"
        member __._mapBlendModes(): unit = failwith "JS only"
        member __.render(``object``: DisplayObject): unit = failwith "JS only"
        member __.resize(w: float, h: float): unit = failwith "JS only"

    and [<Import("CanvasBuffer","PIXI")>] CanvasBuffer(width: float, height: float) =
        member __.canvas with get(): HTMLCanvasElement = failwith "JS only" and set(v: HTMLCanvasElement): unit = failwith "JS only"
        member __.context with get(): CanvasRenderingContext2D = failwith "JS only" and set(v: CanvasRenderingContext2D): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clear(): unit = failwith "JS only"
        member __.resize(width: float, height: float): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and [<Import("CanvasGraphics","PIXI")>] CanvasGraphics() =
        static member renderGraphicsMask(graphics: Graphics, context: CanvasRenderingContext2D): unit = failwith "JS only"
        static member updateGraphicsTint(graphics: Graphics): unit = failwith "JS only"
        static member renderGraphics(graphics: Graphics, context: CanvasRenderingContext2D): unit = failwith "JS only"

    and [<Import("CanvasMaskManager","PIXI")>] CanvasMaskManager() =
        member __.pushMask(maskData: obj, renderer: U2<WebGLRenderer, CanvasRenderer>): unit = failwith "JS only"
        member __.popMask(renderer: U2<WebGLRenderer, CanvasRenderer>): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and [<Import("CanvasTinter","PIXI")>] CanvasTinter() =
        member __.cacheStepsPerColorChannel with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.convertTintToImage with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.vanUseMultiply with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.tintMethod with get(): Function = failwith "JS only" and set(v: Function): unit = failwith "JS only"
        static member getTintedTexture(sprite: DisplayObject, color: float): HTMLCanvasElement = failwith "JS only"
        static member tintWithMultiply(texture: Texture, color: float, canvas: HTMLDivElement): unit = failwith "JS only"
        static member tintWithOverlay(texture: Texture, color: float, canvas: HTMLCanvasElement): unit = failwith "JS only"
        static member tintWithPerPixel(texture: Texture, color: float, canvas: HTMLCanvasElement): unit = failwith "JS only"
        static member roundColor(color: float): float = failwith "JS only"

    and [<Import("WebGLRenderer","PIXI")>] WebGLRenderer(?width: float, ?height: float, ?options: RendererOptions list) =
        inherit SystemRenderer("")
        member __._useFXAA with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __._FXAAFilter with get(): obj (* filters.FXAAFilter *) = failwith "JS only" and set(v: obj (* filters.FXAAFilter *)): unit = failwith "JS only"
        member __._contextOptions with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __._renderTargetStack with get(): ResizeArray<RenderTarget> = failwith "JS only" and set(v: ResizeArray<RenderTarget>): unit = failwith "JS only"
        member __.handleContextLost with get(): Func<WebGLContextEvent, unit> = failwith "JS only" and set(v: Func<WebGLContextEvent, unit>): unit = failwith "JS only"
        member __._managedTextures with get(): ResizeArray<Texture> = failwith "JS only" and set(v: ResizeArray<Texture>): unit = failwith "JS only"
        member __.drawCount with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.shaderManager with get(): ShaderManager = failwith "JS only" and set(v: ShaderManager): unit = failwith "JS only"
        member __.maskManager with get(): MaskManager = failwith "JS only" and set(v: MaskManager): unit = failwith "JS only"
        member __.stencilManager with get(): StencilManager = failwith "JS only" and set(v: StencilManager): unit = failwith "JS only"
        member __.filterManager with get(): FilterManager = failwith "JS only" and set(v: FilterManager): unit = failwith "JS only"
        member __.blendModeManager with get(): BlendModeManager = failwith "JS only" and set(v: BlendModeManager): unit = failwith "JS only"
        member __.currentRenderTarget with get(): RenderTarget = failwith "JS only" and set(v: RenderTarget): unit = failwith "JS only"
        member __.currentRenderer with get(): ObjectRenderer = failwith "JS only" and set(v: ObjectRenderer): unit = failwith "JS only"
        member __._initContext(): unit = failwith "JS only"
        member __._createContext(): unit = failwith "JS only"
        member __._mapGlModes(): unit = failwith "JS only"
        member __.render(``object``: DisplayObject): unit = failwith "JS only"
        member __.renderDisplayObject(displayObject: DisplayObject, renderTarget: RenderTarget, clear: bool): unit = failwith "JS only"
        member __.setObjectRenderer(objectRenderer: ObjectRenderer): unit = failwith "JS only"
        member __.setRenderTarget(renderTarget: RenderTarget): unit = failwith "JS only"
        member __.updateTexture(texture: U2<BaseTexture, Texture>): U2<BaseTexture, Texture> = failwith "JS only"
        member __.destroyTexture(texture: U2<BaseTexture, Texture>, ?_skipRemove: bool): unit = failwith "JS only"

    and [<Import("AbstractFilter","PIXI")>] AbstractFilter(?vertexSrc: U2<string, ResizeArray<string>>, ?fragmentSrc: U2<string, ResizeArray<string>>, ?uniforms: obj) =
        member __.vertexSrc with get(): ResizeArray<string> = failwith "JS only" and set(v: ResizeArray<string>): unit = failwith "JS only"
        member __.fragmentSrc with get(): ResizeArray<string> = failwith "JS only" and set(v: ResizeArray<string>): unit = failwith "JS only"
        member __.uniforms with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.padding with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.getShader(renderer: WebGLRenderer): Shader = failwith "JS only"
        member __.applyFilter(renderer: WebGLRenderer, input: RenderTarget, output: RenderTarget, ?clear: bool): unit = failwith "JS only"
        member __.syncUniform(uniform: WebGLUniformLocation): unit = failwith "JS only"

    and [<Import("SpriteMaskFilter","PIXI")>] SpriteMaskFilter(sprite: Sprite) =
        inherit AbstractFilter()
        member __.maskSprite with get(): Sprite = failwith "JS only" and set(v: Sprite): unit = failwith "JS only"
        member __.maskMatrix with get(): Matrix = failwith "JS only" and set(v: Matrix): unit = failwith "JS only"
        member __.map with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.offset with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
        member __.applyFilter(renderer: WebGLRenderbuffer, input: RenderTarget, output: RenderTarget): unit = failwith "JS only"

    and [<Import("BlendModeManager","PIXI")>] BlendModeManager(renderer: WebGLRenderer) =
        inherit WebGLManager(unbox null)
        member __.setBlendMode(blendMode: float): bool = failwith "JS only"

    and [<Import("FilterManager","PIXI")>] FilterManager(renderer: WebGLRenderer) =
        inherit WebGLManager(unbox null)
        member __.filterStack with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.renderer with get(): WebGLRenderer = failwith "JS only" and set(v: WebGLRenderer): unit = failwith "JS only"
        member __.texturePool with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.onContextChange with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.setFilterStack(filterStack: ResizeArray<obj>): unit = failwith "JS only"
        member __.pushFilter(target: RenderTarget, filters: ResizeArray<obj>): unit = failwith "JS only"
        member __.popFilter(): AbstractFilter = failwith "JS only"
        member __.getRenderTarget(?clear: bool): RenderTarget = failwith "JS only"
        member __.returnRenderTarget(renderTarget: RenderTarget): unit = failwith "JS only"
        member __.applyFilter(shader: U2<Shader, AbstractFilter>, inputTarget: RenderTarget, outputTarget: RenderTarget, ?clear: bool): unit = failwith "JS only"
        member __.calculateMappedMatrix(filterArea: Rectangle, sprite: Sprite, ?outputMatrix: Matrix): Matrix = failwith "JS only"
        member __.capFilterArea(filterArea: Rectangle): unit = failwith "JS only"
        member __.resize(width: float, height: float): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and [<Import("MaskManager","PIXI")>] MaskManager() =
        inherit WebGLManager(unbox null)
        member __.stencilStack with get(): StencilMaskStack = failwith "JS only" and set(v: StencilMaskStack): unit = failwith "JS only"
        member __.reverse with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.count with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.alphaMaskPool with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.pushMask(target: RenderTarget, maskData: obj): unit = failwith "JS only"
        member __.popMask(target: RenderTarget, maskData: obj): unit = failwith "JS only"
        member __.pushSpriteMask(target: RenderTarget, maskData: obj): unit = failwith "JS only"
        member __.popSpriteMask(): unit = failwith "JS only"
        member __.pushStencilMask(target: RenderTarget, maskData: obj): unit = failwith "JS only"
        member __.popStencilMask(target: RenderTarget, maskData: obj): unit = failwith "JS only"

    and [<Import("ShaderManager","PIXI")>] ShaderManager(renderer: WebGLRenderer) =
        inherit WebGLManager(unbox null)
        member __._currentId with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.currentShader with get(): Shader = failwith "JS only" and set(v: Shader): unit = failwith "JS only"
        member __.maxAttibs with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.attribState with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.tempAttribState with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.stack with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.setAttribs(attribs: ResizeArray<obj>): unit = failwith "JS only"
        member __.setShader(shader: Shader): bool = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and [<Import("StencilManager","PIXI")>] StencilManager(renderer: WebGLRenderer) =
        inherit WebGLManager(unbox null)
        member __.setMaskStack(stencilMaskStack: StencilMaskStack): unit = failwith "JS only"
        member __.pushStencil(graphics: Graphics, webGLData: WebGLGraphicsData): unit = failwith "JS only"
        member __.bindGraphics(graphics: Graphics, webGLData: WebGLGraphicsData): unit = failwith "JS only"
        member __.popStencil(graphics: Graphics, webGLData: WebGLGraphicsData): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"
        member __.pushMask(maskData: ResizeArray<obj>): unit = failwith "JS only"
        member __.popMask(maskData: ResizeArray<obj>): unit = failwith "JS only"

    and [<Import("WebGLManager","PIXI")>] WebGLManager(renderer: WebGLRenderer) =
        member __.onContextChange with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
        member __.renderer with get(): WebGLRenderer = failwith "JS only" and set(v: WebGLRenderer): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and [<Import("Shader","PIXI")>] Shader(shaderManager: ShaderManager, vertexSrc: string, fragmentSrc: string, uniforms: obj, attributes: obj) =
        member __.attributes with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.textureCount with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.uniforms with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.uuid with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.gl with get(): WebGLRenderingContext = failwith "JS only" and set(v: WebGLRenderingContext): unit = failwith "JS only"
        member __.shaderManager with get(): ShaderManager = failwith "JS only" and set(v: ShaderManager): unit = failwith "JS only"
        member __.program with get(): WebGLProgram = failwith "JS only" and set(v: WebGLProgram): unit = failwith "JS only"
        member __.vertexSrc with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.fragmentSrc with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __._glCompile(``type``: obj, src: obj): Shader = failwith "JS only"
        member __.init(): unit = failwith "JS only"
        member __.cacheUniformLocations(keys: ResizeArray<string>): unit = failwith "JS only"
        member __.cacheAttributeLocations(keys: ResizeArray<string>): unit = failwith "JS only"
        member __.compile(): WebGLProgram = failwith "JS only"
        member __.syncUniform(uniform: obj): unit = failwith "JS only"
        member __.syncUniforms(): unit = failwith "JS only"
        member __.initSampler2D(uniform: obj): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and [<Import("ComplexPrimitiveShader","PIXI")>] ComplexPrimitiveShader(shaderManager: ShaderManager) =
        inherit Shader(unbox null, null, null, null, null)


    and [<Import("PrimitiveShader","PIXI")>] PrimitiveShader(shaderManager: ShaderManager) =
        inherit Shader(unbox null, null, null, null, null)


    and [<Import("TextureShader","PIXI")>] TextureShader(shaderManager: ShaderManager, ?vertexSrc: string, ?fragmentSrc: string, ?customUniforms: obj, ?customAttributes: obj) =
        inherit Shader(unbox null, null, null, null, null)


    and StencilMaskStack =
        abstract stencilStack: ResizeArray<obj> with get, set
        abstract reverse: bool with get, set
        abstract count: float with get, set

    and [<Import("ObjectRenderer","PIXI")>] ObjectRenderer() =
        inherit WebGLManager(unbox null)
        member __.start(): unit = failwith "JS only"
        member __.stop(): unit = failwith "JS only"
        member __.flush(): unit = failwith "JS only"
        member __.render(?``object``: obj): unit = failwith "JS only"

    and [<Import("RenderTarget","PIXI")>] RenderTarget(gl: WebGLRenderingContext, width: float, height: float, scaleMode: float, resolution: float, root: bool) =
        member __.gl with get(): WebGLRenderingContext = failwith "JS only" and set(v: WebGLRenderingContext): unit = failwith "JS only"
        member __.frameBuffer with get(): WebGLFramebuffer = failwith "JS only" and set(v: WebGLFramebuffer): unit = failwith "JS only"
        member __.texture with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.size with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __.resolution with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.projectionMatrix with get(): Matrix = failwith "JS only" and set(v: Matrix): unit = failwith "JS only"
        member __.transform with get(): Matrix = failwith "JS only" and set(v: Matrix): unit = failwith "JS only"
        member __.frame with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __.stencilBuffer with get(): WebGLRenderbuffer = failwith "JS only" and set(v: WebGLRenderbuffer): unit = failwith "JS only"
        member __.stencilMaskStack with get(): StencilMaskStack = failwith "JS only" and set(v: StencilMaskStack): unit = failwith "JS only"
        member __.filterStack with get(): ResizeArray<obj> = failwith "JS only" and set(v: ResizeArray<obj>): unit = failwith "JS only"
        member __.scaleMode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.root with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.clear(?bind: bool): unit = failwith "JS only"
        member __.attachStencilBuffer(): unit = failwith "JS only"
        member __.activate(): unit = failwith "JS only"
        member __.calculateProjection(protectionFrame: Matrix): unit = failwith "JS only"
        member __.resize(width: float, height: float): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and Quad =
        abstract gl: WebGLRenderingContext with get, set
        abstract vertices: ResizeArray<float> with get, set
        abstract uvs: ResizeArray<float> with get, set
        abstract colors: ResizeArray<float> with get, set
        abstract indices: ResizeArray<float> with get, set
        abstract vertexBuffer: WebGLBuffer with get, set
        abstract indexBuffer: WebGLBuffer with get, set
        abstract map: rect: Rectangle * rect2: Rectangle -> unit
        abstract upload: unit -> unit
        abstract destroy: unit -> unit

    and [<Import("Sprite","PIXI")>] Sprite(?texture: Texture) =
        inherit Container()
        member __._texture with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __._width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __._height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.cachedTint with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.anchor with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
        member __.tint with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.blendMode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.shader with get(): U2<Shader, AbstractFilter> = failwith "JS only" and set(v: U2<Shader, AbstractFilter>): unit = failwith "JS only"
        member __.texture with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member fromFrame(frameId: string): Sprite = failwith "JS only"
        static member fromImage(imageId: string, ?crossorigin: bool, ?scaleMode: float): Sprite = failwith "JS only"
        member __._onTextureUpdate(): unit = failwith "JS only"
        member __.getBounds(?matrix: Matrix): Rectangle = failwith "JS only"
        member __.getLocalBounds(): Rectangle = failwith "JS only"
        member __.containsPoint(point: Point): bool = failwith "JS only"
        member __.destroy(?destroyTexture: bool, ?destroyBaseTexture: bool): unit = failwith "JS only"

    and [<Import("SpriteRenderer","PIXI")>] SpriteRenderer() =
        inherit ObjectRenderer()
        member __.vertSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.vertByteSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.size with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.vertices with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.positions with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.colors with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.indices with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
        member __.currentBatchSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.sprites with get(): ResizeArray<Sprite> = failwith "JS only" and set(v: ResizeArray<Sprite>): unit = failwith "JS only"
        member __.shader with get(): U2<Shader, AbstractFilter> = failwith "JS only" and set(v: U2<Shader, AbstractFilter>): unit = failwith "JS only"
        member __.renderBatch(texture: Texture, size: float, startIndex: float): unit = failwith "JS only"
        member __.render(sprite: Sprite): unit = failwith "JS only"
        member __.flush(): unit = failwith "JS only"
        member __.start(): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and TextStyle =
        | Font of string
        | Fill of U2<string,float>
        | Align of string
        | Stroke of U2<string,float>
        | StrokeThickness of float
        | WordWrap of bool
        | WordWrapWidth of float
        | LineHeight of float 
        | DropShadow of bool 
        | DropShadowColor of U2<string, float> 
        | DropShadowAngle of float 
        | DropShadowDistance of float 
        | Padding of float 
        | TextBaseline of string 
        | LineJoin of string 
        | MiterLimit of float 
        
    and [<Import("Text","PIXI")>] Text(?text: string, ?style: TextStyle list, ?resolution: float) =
        inherit Sprite()
        member __.fontPropertiesCache with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.fontPropertiesCanvas with get(): HTMLCanvasElement = failwith "JS only" and set(v: HTMLCanvasElement): unit = failwith "JS only"
        member __.fontPropertiesContext with get(): CanvasRenderingContext2D = failwith "JS only" and set(v: CanvasRenderingContext2D): unit = failwith "JS only"
        member __._text with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __._style with get(): TextStyle list = failwith "JS only" and set(v: TextStyle list): unit = failwith "JS only"
        member __.canvas with get(): HTMLCanvasElement = failwith "JS only" and set(v: HTMLCanvasElement): unit = failwith "JS only"
        member __.context with get(): CanvasRenderingContext2D = failwith "JS only" and set(v: CanvasRenderingContext2D): unit = failwith "JS only"
        member __.dirty with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.resolution with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.text with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.style with get(): TextStyle list = failwith "JS only" and set(v: TextStyle list): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.updateText(): unit = failwith "JS only"
        member __.updateTexture(): unit = failwith "JS only"
        member __.determineFontProperties(fontStyle: TextStyle list): TextStyle list = failwith "JS only"
        member __.wordWrap(text: string): bool = failwith "JS only"

    and [<Import("BaseTexture","PIXI")>] BaseTexture(source: U2<HTMLImageElement, HTMLCanvasElement>, ?scaleMode: float, ?resolution: float) =
        inherit EventEmitter()
        member __._glTextures with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.uuid with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.resolution with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.realWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.realHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.scaleMode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.hasLoaded with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.isLoading with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.source with get(): U3<HTMLImageElement, HTMLCanvasElement, HTMLVideoElement> = failwith "JS only" and set(v: U3<HTMLImageElement, HTMLCanvasElement, HTMLVideoElement>): unit = failwith "JS only"
        member __.premultipliedAlpha with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.imageUrl with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.isPowerOfTwo with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.mipmap with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        static member fromImage(imageUrl: string, ?crossorigin: bool, ?scaleMode: float): BaseTexture = failwith "JS only"
        static member fromCanvas(canvas: HTMLCanvasElement, ?scaleMode: float): BaseTexture = failwith "JS only"
        member __._sourceLoaded(): unit = failwith "JS only"
        member __.update(): unit = failwith "JS only"
        member __.loadSource(source: U2<HTMLImageElement, HTMLCanvasElement>): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.updateSourceImage(newSrc: string): unit = failwith "JS only"
        [<Emit("$0.on('dispose',$1...)")>] member __.on_dispose(fn: Func<BaseTexture, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('error',$1...)")>] member __.on_error(fn: Func<BaseTexture, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('loaded',$1...)")>] member __.on_loaded(fn: Func<BaseTexture, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.on('update',$1...)")>] member __.on_update(fn: Func<BaseTexture, unit>, ?context: obj): EventEmitter = failwith "JS only"
        member __.on(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('dispose',$1...)")>] member __.once_dispose(fn: Func<BaseTexture, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('error',$1...)")>] member __.once_error(fn: Func<BaseTexture, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('loaded',$1...)")>] member __.once_loaded(fn: Func<BaseTexture, unit>, ?context: obj): EventEmitter = failwith "JS only"
        [<Emit("$0.once('update',$1...)")>] member __.once_update(fn: Func<BaseTexture, unit>, ?context: obj): EventEmitter = failwith "JS only"
        member __.once(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"

    and [<Import("RenderTexture","PIXI")>] RenderTexture(renderer: U2<CanvasRenderer, WebGLRenderer>, ?width: float, ?height: float, ?scaleMode: float, ?resolution: float) =
        inherit Texture(unbox null)
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.resolution with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.renderer with get(): U2<CanvasRenderer, WebGLRenderer> = failwith "JS only" and set(v: U2<CanvasRenderer, WebGLRenderer>): unit = failwith "JS only"
        member __.valid with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.renderWebGL(displayObject: DisplayObject, ?matrix: Matrix, ?clear: bool, ?updateTransform: bool): unit = failwith "JS only"
        member __.renderCanvas(displayObject: DisplayObject, ?matrix: Matrix, ?clear: bool, ?updateTransform: bool): unit = failwith "JS only"
        member __.render(displayObject: DisplayObject, ?matrix: Matrix, ?clear: bool, ?updateTransform: bool): unit = failwith "JS only"
        member __.resize(width: float, height: float, ?updateBase: bool): unit = failwith "JS only"
        member __.clear(): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"
        member __.getImage(): HTMLImageElement = failwith "JS only"
        member __.getPixels(): ResizeArray<float> = failwith "JS only"
        member __.getPixel(x: float, y: float): ResizeArray<float> = failwith "JS only"
        member __.getBase64(): string = failwith "JS only"
        member __.getCanvas(): HTMLCanvasElement = failwith "JS only"

    and [<Import("Texture","PIXI")>] Texture(baseTexture: BaseTexture, ?frame: Rectangle, ?crop: Rectangle, ?trim: Rectangle, ?rotate: float) =
        inherit BaseTexture(unbox null)
        member __.EMPTY with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
        member __._frame with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __._uvs with get(): TextureUvs = failwith "JS only" and set(v: TextureUvs): unit = failwith "JS only"
        member __.noFrame with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.baseTexture with get(): BaseTexture = failwith "JS only" and set(v: BaseTexture): unit = failwith "JS only"
        member __.trim with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __.valid with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.requiresUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.crop with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        member __.rotate with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.frame with get(): Rectangle = failwith "JS only" and set(v: Rectangle): unit = failwith "JS only"
        static member fromImage(imageUrl: string, ?crossOrigin: bool, ?scaleMode: float): Texture = failwith "JS only"
        static member fromFrame(frameId: string): Texture = failwith "JS only"
        static member fromCanvas(canvas: HTMLCanvasElement, ?scaleMode: float): Texture = failwith "JS only"
        static member fromVideo(video: U2<HTMLVideoElement, string>, ?scaleMode: float): Texture = failwith "JS only"
        static member fromVideoUrl(videoUrl: string, ?scaleMode: float): Texture = failwith "JS only"
        static member addTextureToCache(texture: Texture, id: string): unit = failwith "JS only"
        static member removeTextureFromCache(id: string): Texture = failwith "JS only"
        member __.onBaseTextureUpdated(baseTexture: BaseTexture): unit = failwith "JS only"
        member __.onBaseTextureLoaded(baseTexture: BaseTexture): unit = failwith "JS only"
        member __._updateUvs(): unit = failwith "JS only"
        member __.update(): unit = failwith "JS only"
        member __.destroy(?destroyBase: bool): unit = failwith "JS only"
        member __.clone(): Texture = failwith "JS only"

    and [<Import("TextureUvs","PIXI")>] TextureUvs() =
        member __.x0 with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y0 with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.x1 with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y1 with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.x2 with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y2 with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.x3 with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.y3 with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.set(frame: Rectangle, baseFrame: Rectangle, rotate: bool): unit = failwith "JS only"

    and [<Import("VideoBaseTexture","PIXI")>] VideoBaseTexture(source: HTMLVideoElement, ?scaleMode: float) =
        inherit BaseTexture(unbox null)
        member __._loaded with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.autoUpdate with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        static member fromVideo(video: HTMLVideoElement, ?scaleMode: float): VideoBaseTexture = failwith "JS only"
        static member fromUrl(videoSrc: U4<string, obj, ResizeArray<string>, ResizeArray<obj>>): VideoBaseTexture = failwith "JS only"
        member __._onUpdate(): unit = failwith "JS only"
        member __._onPlayStart(): unit = failwith "JS only"
        member __._onPlayStop(): unit = failwith "JS only"
        member __._onCanPlay(): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    and [<Import("utils","PIXI")>] utils() =
        member __.TextureCache with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.BaseTextureCache with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        static member uuid(): float = failwith "JS only"
        static member hex2rgb(hex: float, ?out: ResizeArray<float>): ResizeArray<float> = failwith "JS only"
        static member hex2String(hex: float): string = failwith "JS only"
        static member rgb2hex(rgb: ResizeArray<float>): float = failwith "JS only"
        static member canUseNewCanvasBlendModel(): bool = failwith "JS only"
        static member getNextPowerOfTwo(number: float): float = failwith "JS only"
        static member isPowerOfTwo(width: float, height: float): bool = failwith "JS only"
        static member getResolutionOfUrl(url: string): float = failwith "JS only"
        static member sayHello(``type``: string): unit = failwith "JS only"
        static member isWebGLSupported(): bool = failwith "JS only"
        static member sign(n: float): float = failwith "JS only"

    and [<Import("GroupD8","PIXI")>] GroupD8 =
        static member E with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member MIRROR_HORIZONTAL with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member MIRROR_VERTICAL with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member N with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member NE with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member NW with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member S with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member SE with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member SW with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member W with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member add(rotationSecond: float, rotationFirst: float): float = failwith "JS only"
        static member byDirection(dx: float, dy: float): float = failwith "JS only"
        static member inv(rotation: float): float = failwith "JS only"
        static member isSwapWidthHeight(rotation: float): bool = failwith "JS only"
        static member matrixAppendRotationInv(matrix: Matrix, rotation: float, tx: float, ty: float): unit = failwith "JS only"
        static member rotate180(rotation: float): float = failwith "JS only"
        static member sub(rotationSecond: float, rotationFirst: float): float = failwith "JS only"
        static member uX(ind: float): float = failwith "JS only"
        static member uY(ind: float): float = failwith "JS only"
        static member vX(ind: float): float = failwith "JS only"
        static member vY(ind: float): float = failwith "JS only"

    module extras =

        type BitmapTextStyle =
            | Font of U2<string, obj>
            | Align of string 
            | Tint of float 

        and [<Import("extras.BitmapText","PIXI")>] BitmapText(text: string, ?style: BitmapTextStyle list) =
            inherit Container()
            member __.fonts with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __._glyphs with get(): ResizeArray<Sprite> = failwith "JS only" and set(v: ResizeArray<Sprite>): unit = failwith "JS only"
            member __._font with get(): U2<string, obj> = failwith "JS only" and set(v: U2<string, obj>): unit = failwith "JS only"
            member __._text with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.textWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.textHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.maxWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.maxLineHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.dirty with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.tint with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.align with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.font with get(): U2<string, obj> = failwith "JS only" and set(v: U2<string, obj>): unit = failwith "JS only"
            member __.text with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.updateText(): unit = failwith "JS only"

        and [<Import("extras.MovieClip","PIXI")>] MovieClip(textures: ResizeArray<Texture>) =
            inherit Sprite()
            member __._textures with get(): ResizeArray<Texture> = failwith "JS only" and set(v: ResizeArray<Texture>): unit = failwith "JS only"
            member __._durations with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __._currentTime with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.animationSpeed with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.loop with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.onComplete with get(): Func<unit> = failwith "JS only" and set(v: Func<unit>): unit = failwith "JS only"
            member __.currentFrame with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.playing with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.totalFrames with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.textures with get(): ResizeArray<Texture> = failwith "JS only" and set(v: ResizeArray<Texture>): unit = failwith "JS only"
            static member fromFrames(frame: ResizeArray<string>): MovieClip = failwith "JS only"
            static member fromImages(images: ResizeArray<string>): MovieClip = failwith "JS only"
            member __.update(deltaTime: float): unit = failwith "JS only"
            member __.stop(): unit = failwith "JS only"
            member __.play(): unit = failwith "JS only"
            member __.gotoAndStop(frameName: float): unit = failwith "JS only"
            member __.gotoAndPlay(frameName: float): unit = failwith "JS only"
            member __.destroy(): unit = failwith "JS only"

        and [<Import("extras.TilingSprite","PIXI")>] TilingSprite(texture: Texture, width: float, height: float) =
            inherit Sprite()
            member __._tileScaleOffset with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
            member __._tilingTexture with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __._refreshTexture with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __._uvs with get(): ResizeArray<TextureUvs> = failwith "JS only" and set(v: ResizeArray<TextureUvs>): unit = failwith "JS only"
            member __.tileScale with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
            member __.tilePosition with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
            member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.originalTexture with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
            static member fromFrame(frameId: string): Sprite = failwith "JS only"
            static member fromImage(imageId: string, ?crossorigin: bool, ?scaleMode: float): Sprite = failwith "JS only"
            static member fromFrame(frameId: string, ?width: float, ?height: float): TilingSprite = failwith "JS only"
            static member fromImage(imageId: string, ?width: float, ?height: float, ?crossorigin: bool, ?scaleMode: float): TilingSprite = failwith "JS only"
            member __.getBounds(): Rectangle = failwith "JS only"
            member __.generateTilingTexture(renderer: U2<WebGLRenderer, CanvasRenderer>, texture: Texture, ?forcePowerOfTwo: bool): Texture = failwith "JS only"
            member __.containsPoint(point: Point): bool = failwith "JS only"
            member __.destroy(): unit = failwith "JS only"



    module filters =
        type [<Import("filters.AsciiFilter","PIXI")>] AsciiFilter() =
            inherit AbstractFilter()
            member __.size with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.BloomFilter","PIXI")>] BloomFilter() =
            inherit AbstractFilter()
            member __.blur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.blurX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.blurY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.BlurFilter","PIXI")>] BlurFilter() =
            inherit AbstractFilter()
            member __.blurXFilter with get(): BlurXFilter = failwith "JS only" and set(v: BlurXFilter): unit = failwith "JS only"
            member __.blurYFilter with get(): BlurYFilter = failwith "JS only" and set(v: BlurYFilter): unit = failwith "JS only"
            member __.blur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.passes with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.blurX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.blurY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.BlurXFilter","PIXI")>] BlurXFilter() =
            inherit AbstractFilter()
            member __.passes with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.strength with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.blur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.BlurYFilter","PIXI")>] BlurYFilter() =
            inherit AbstractFilter()
            member __.passes with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.strength with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.blur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.SmartBlurFilter","PIXI")>] SmartBlurFilter() =
            inherit AbstractFilter()


        and [<Import("filters.ColorMatrixFilter","PIXI")>] ColorMatrixFilter() =
            inherit AbstractFilter()
            member __.matrix with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __._loadMatrix(matrix: ResizeArray<float>, multiply: bool): unit = failwith "JS only"
            member __._multiply(out: ResizeArray<float>, a: ResizeArray<float>, b: ResizeArray<float>): unit = failwith "JS only"
            member __._colorMatrix(matrix: ResizeArray<float>): unit = failwith "JS only"
            member __.brightness(b: float, ?multiply: bool): unit = failwith "JS only"
            member __.greyscale(scale: float, ?multiply: bool): unit = failwith "JS only"
            member __.blackAndWhite(?multiply: bool): unit = failwith "JS only"
            member __.hue(rotation: float, ?multiply: bool): unit = failwith "JS only"
            member __.contrast(amount: float, ?multiply: bool): unit = failwith "JS only"
            member __.saturate(amount: float, ?multiply: bool): unit = failwith "JS only"
            member __.desaturate(?multiply: bool): unit = failwith "JS only"
            member __.negative(?multiply: bool): unit = failwith "JS only"
            member __.sepia(?multiply: bool): unit = failwith "JS only"
            member __.technicolor(?multiply: bool): unit = failwith "JS only"
            member __.polaroid(?multiply: bool): unit = failwith "JS only"
            member __.toBGR(?multiply: bool): unit = failwith "JS only"
            member __.kodachrome(?multiply: bool): unit = failwith "JS only"
            member __.browni(?multiply: bool): unit = failwith "JS only"
            member __.vintage(?multiply: bool): unit = failwith "JS only"
            member __.colorTone(desaturation: float, toned: float, lightColor: string, darkColor: string, ?multiply: bool): unit = failwith "JS only"
            member __.night(intensity: float, ?multiply: bool): unit = failwith "JS only"
            member __.predator(amount: float, ?multiply: bool): unit = failwith "JS only"
            member __.lsd(?multiply: bool): unit = failwith "JS only"
            member __.reset(): unit = failwith "JS only"

        and [<Import("filters.ColorStepFilter","PIXI")>] ColorStepFilter() =
            inherit AbstractFilter()
            member __.step with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.ConvolutionFilter","PIXI")>] ConvolutionFilter(matrix: ResizeArray<float>, width: float, height: float) =
            inherit AbstractFilter()
            member __.matrix with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.CrossHatchFilter","PIXI")>] CrossHatchFilter() =
            inherit AbstractFilter()


        and [<Import("filters.DisplacementFilter","PIXI")>] DisplacementFilter(sprite: Sprite, ?scale: float) =
            inherit AbstractFilter()
            member __.map with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
            member __.scale with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"

        and [<Import("filters.DotScreenFilter","PIXI")>] DotScreenFilter() =
            inherit AbstractFilter()
            member __.scale with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.angle with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.BlurYTintFilter","PIXI")>] BlurYTintFilter() =
            inherit AbstractFilter()
            member __.blur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.DropShadowFilter","PIXI")>] DropShadowFilter() =
            inherit AbstractFilter()
            member __.blur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.blurX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.blurY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.color with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.alpha with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.distance with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.angle with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.GrayFilter","PIXI")>] GrayFilter() =
            inherit AbstractFilter()
            member __.gray with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.InvertFilter","PIXI")>] InvertFilter() =
            inherit AbstractFilter()
            member __.invert with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.NoiseFilter","PIXI")>] NoiseFilter() =
            inherit AbstractFilter()
            member __.noise with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.PixelateFilter","PIXI")>] PixelateFilter() =
            inherit AbstractFilter()
            member __.size with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"

        and [<Import("filters.RGBSplitFilter","PIXI")>] RGBSplitFilter() =
            inherit AbstractFilter()
            member __.red with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.green with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.blue with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.SepiaFilter","PIXI")>] SepiaFilter() =
            inherit AbstractFilter()
            member __.sepia with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.ShockwaveFilter","PIXI")>] ShockwaveFilter() =
            inherit AbstractFilter()
            member __.center with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __.``params`` with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.time with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.TiltShiftAxisFilter","PIXI")>] TiltShiftAxisFilter() =
            inherit AbstractFilter()
            member __.blur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.gradientBlur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.start with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.``end`` with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.updateDelta(): unit = failwith "JS only"

        and [<Import("filters.TiltShiftFilter","PIXI")>] TiltShiftFilter() =
            inherit AbstractFilter()
            member __.blur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.gradientBlur with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.start with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.``end`` with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.TiltShiftXFilter","PIXI")>] TiltShiftXFilter() =
            inherit AbstractFilter()
            member __.updateDelta(): unit = failwith "JS only"

        and [<Import("filters.TiltShiftYFilter","PIXI")>] TiltShiftYFilter() =
            inherit AbstractFilter()
            member __.updateDelta(): unit = failwith "JS only"

        and [<Import("filters.TwistFilter","PIXI")>] TwistFilter() =
            inherit AbstractFilter()
            member __.offset with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
            member __.radius with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.angle with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("filters.FXAAFilter","PIXI")>] FXAAFilter() =
            inherit AbstractFilter()
            member __.applyFilter(renderer: WebGLRenderer, input: RenderTarget, output: RenderTarget): unit = failwith "JS only"



    module interaction =
        type [<Import("interaction.InteractionData","PIXI")>] InteractionData() =
            member __.``global`` with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
            member __.target with get(): DisplayObject = failwith "JS only" and set(v: DisplayObject): unit = failwith "JS only"
            member __.originalEvent with get(): Event = failwith "JS only" and set(v: Event): unit = failwith "JS only"
            member __.getLocalPosition(displayObject: DisplayObject, ?point: Point, ?globalPos: Point): Point = failwith "JS only"

        and [<Import("interaction.InteractionManager","PIXI")>] InteractionManager(renderer: U2<CanvasRenderer, WebGLRenderer>, ?options: obj) =
            member __.interactionDOMElement with get(): HTMLElement = failwith "JS only" and set(v: HTMLElement): unit = failwith "JS only"
            member __.eventsAdded with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __._tempPoint with get(): Point = failwith "JS only" and set(v: Point): unit = failwith "JS only"
            member __.onMouseDown with get(): Func<Event, unit> = failwith "JS only" and set(v: Func<Event, unit>): unit = failwith "JS only"
            member __.processMouseDown with get(): Func<DisplayObject, bool, unit> = failwith "JS only" and set(v: Func<DisplayObject, bool, unit>): unit = failwith "JS only"
            member __.onMouseUp with get(): Func<Event, unit> = failwith "JS only" and set(v: Func<Event, unit>): unit = failwith "JS only"
            member __.processMouseUp with get(): Func<DisplayObject, bool, unit> = failwith "JS only" and set(v: Func<DisplayObject, bool, unit>): unit = failwith "JS only"
            member __.onMouseMove with get(): Func<Event, unit> = failwith "JS only" and set(v: Func<Event, unit>): unit = failwith "JS only"
            member __.processMouseMove with get(): Func<DisplayObject, bool, unit> = failwith "JS only" and set(v: Func<DisplayObject, bool, unit>): unit = failwith "JS only"
            member __.onMouseOut with get(): Func<Event, unit> = failwith "JS only" and set(v: Func<Event, unit>): unit = failwith "JS only"
            member __.processMouseOverOut with get(): Func<DisplayObject, bool, unit> = failwith "JS only" and set(v: Func<DisplayObject, bool, unit>): unit = failwith "JS only"
            member __.onTouchStart with get(): Func<Event, unit> = failwith "JS only" and set(v: Func<Event, unit>): unit = failwith "JS only"
            member __.processTouchStart with get(): Func<DisplayObject, bool, unit> = failwith "JS only" and set(v: Func<DisplayObject, bool, unit>): unit = failwith "JS only"
            member __.onTouchEnd with get(): Func<Event, unit> = failwith "JS only" and set(v: Func<Event, unit>): unit = failwith "JS only"
            member __.processTouchEnd with get(): Func<DisplayObject, bool, unit> = failwith "JS only" and set(v: Func<DisplayObject, bool, unit>): unit = failwith "JS only"
            member __.onTouchMove with get(): Func<Event, unit> = failwith "JS only" and set(v: Func<Event, unit>): unit = failwith "JS only"
            member __.processTouchMove with get(): Func<DisplayObject, bool, unit> = failwith "JS only" and set(v: Func<DisplayObject, bool, unit>): unit = failwith "JS only"
            member __.renderer with get(): U2<CanvasRenderer, WebGLRenderer> = failwith "JS only" and set(v: U2<CanvasRenderer, WebGLRenderer>): unit = failwith "JS only"
            member __.autoPreventDefault with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.interactionFrequency with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.mouse with get(): InteractionData = failwith "JS only" and set(v: InteractionData): unit = failwith "JS only"
            member __.eventData with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.interactiveDataPool with get(): ResizeArray<InteractionData> = failwith "JS only" and set(v: ResizeArray<InteractionData>): unit = failwith "JS only"
            member __.last with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.currentCursorStyle with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.resolution with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.setTargetElement(element: HTMLElement, resolution: float): unit = failwith "JS only"
            member __.addEvents(): unit = failwith "JS only"
            member __.removeEvents(): unit = failwith "JS only"
            member __.dispatchEvent(displayObject: DisplayObject, eventString: string, eventData: obj): unit = failwith "JS only"
            member __.getTouchData(touchEvent: InteractionData): InteractionData = failwith "JS only"
            member __.returnTouchData(touchData: InteractionData): unit = failwith "JS only"
            member __.update(deltaTime: float): unit = failwith "JS only"
            member __.mapPositionToPoint(point: Point, x: float, y: float): unit = failwith "JS only"
            member __.processInteractive(point: Point, displayObject: DisplayObject, func: Func<DisplayObject, bool, unit>, hitTest: bool, interactive: bool): bool = failwith "JS only"
            member __.destroy(): unit = failwith "JS only"

        and InteractiveTarget =
            abstract interactive: bool with get, set
            abstract buttonMode: bool with get, set
            abstract interactiveChildren: bool with get, set
            abstract defaultCursor: string with get, set
            abstract hitArea: HitArea with get, set


    module loaders =
        type LoaderOptions =
            abstract crossOrigin: bool option with get, set
            abstract loadType: float option with get, set
            abstract xhrType: string option with get, set

        and ResourceDictionary =
            [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: string -> Resource with get, set

        and [<Import("loaders.Loader","PIXI")>] Loader(?baseUrl: string, ?concurrency: float) =
            // interface EventEmitter
            member __.baseUrl with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.progress with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.loading with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.resources with get(): ResourceDictionary = failwith "JS only" and set(v: ResourceDictionary): unit = failwith "JS only"
            member __.add(name: string, url: string, ?options: LoaderOptions, ?cb: Func<unit>): Loader = failwith "JS only"
            member __.add(url: string, ?options: LoaderOptions, ?cb: Func<unit>): Loader = failwith "JS only"
            member __.add(obj: obj, ?options: LoaderOptions, ?cb: Func<unit>): Loader = failwith "JS only"
            [<Emit("$0.on('complete',$1...)")>] member __.on_complete(fn: Func<Loader, obj, unit>, ?context: obj): EventEmitter = failwith "JS only"
            [<Emit("$0.on('error',$1...)")>] member __.on_error(fn: Func<Error, Loader, Resource, unit>, ?context: obj): EventEmitter = failwith "JS only"
            [<Emit("$0.on('load',$1...)")>] member __.on_load(fn: Func<Loader, Resource, unit>, ?context: obj): EventEmitter = failwith "JS only"
            [<Emit("$0.on('progress',$1...)")>] member __.on_progress(fn: Func<Loader, Resource, unit>, ?context: obj): EventEmitter = failwith "JS only"
            [<Emit("$0.on('start',$1...)")>] member __.on_start(fn: Func<Loader, unit>, ?context: obj): EventEmitter = failwith "JS only"
            member __.on(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"
            [<Emit("$0.once('complete',$1...)")>] member __.once_complete(fn: Func<Loader, obj, unit>, ?context: obj): EventEmitter = failwith "JS only"
            [<Emit("$0.once('error',$1...)")>] member __.once_error(fn: Func<Error, Loader, Resource, unit>, ?context: obj): EventEmitter = failwith "JS only"
            [<Emit("$0.once('load',$1...)")>] member __.once_load(fn: Func<Loader, Resource, unit>, ?context: obj): EventEmitter = failwith "JS only"
            [<Emit("$0.once('progress',$1...)")>] member __.once_progress(fn: Func<Loader, Resource, unit>, ?context: obj): EventEmitter = failwith "JS only"
            [<Emit("$0.once('start',$1...)")>] member __.once_start(fn: Func<Loader, unit>, ?context: obj): EventEmitter = failwith "JS only"
            member __.once(``event``: string, fn: Function, ?context: obj): EventEmitter = failwith "JS only"
            member __.before(fn: Function): Loader = failwith "JS only"
            member __.pre(fn: Function): Loader = failwith "JS only"
            member __.after(fn: Function): Loader = failwith "JS only"
            member __.``use``(fn: Function): Loader = failwith "JS only"
            member __.reset(): unit = failwith "JS only"
            member __.load(?cb: Loader -> obj -> unit): Loader = failwith "JS only"

        and [<Import("loaders.Resource","PIXI")>] Resource(?name: string, ?url: U2<string, ResizeArray<string>>, ?options: LoaderOptions) =
            inherit EventEmitter()
            member __.LOAD_TYPE with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.XHR_READ_STATE with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.XHR_RESPONSE_TYPE with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.texture with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
            member __.textures with get(): ResizeArray<Texture> = failwith "JS only" and set(v: ResizeArray<Texture>): unit = failwith "JS only"
            member __.url with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.data with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.crossOrigin with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.loadType with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.xhrType with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.error with get(): Error = failwith "JS only" and set(v: Error): unit = failwith "JS only"
            member __.xhr with get(): XMLHttpRequest = failwith "JS only" and set(v: XMLHttpRequest): unit = failwith "JS only"
            member __.complete(): unit = failwith "JS only"
            member __.load(?cb: Func<unit>): unit = failwith "JS only"


    module mesh =
        type [<Import("mesh.Mesh","PIXI")>] Mesh(texture: Texture, ?vertices: ResizeArray<float>, ?uvs: ResizeArray<float>, ?indices: ResizeArray<float>, ?drawMode: float) =
            inherit Container()
            member __.DRAW_MODES with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.texture with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
            member __.uvs with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __.vertices with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __.indices with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __.dirty with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.blendMode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.canvasPadding with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.drawMode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.shader with get(): U2<Shader, AbstractFilter> = failwith "JS only" and set(v: U2<Shader, AbstractFilter>): unit = failwith "JS only"
            member __._texture with get(): Texture = failwith "JS only" and set(v: Texture): unit = failwith "JS only"
            member __.getBounds(?matrix: Matrix): Rectangle = failwith "JS only"
            member __.containsPoint(point: Point): bool = failwith "JS only"
            member __._renderCanvasTriangleMesh(context: CanvasRenderingContext2D): unit = failwith "JS only"
            member __._renderCanvasTriangles(context: CanvasRenderingContext2D): unit = failwith "JS only"
            member __._renderCanvasDrawTriangle(context: CanvasRenderingContext2D, vertices: float, uvs: float, index0: float, index1: float, index2: float): unit = failwith "JS only"
            member __.renderMeshFlat(mesh: Mesh): unit = failwith "JS only"
            member __._onTextureUpdate(): unit = failwith "JS only"

        and [<Import("mesh.Rope","PIXI")>] Rope(texture: Texture, points: ResizeArray<Point>) =
            inherit Mesh(unbox null)
            member __._ready with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.points with get(): ResizeArray<Point> = failwith "JS only" and set(v: ResizeArray<Point>): unit = failwith "JS only"
            member __.colors with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __.getTextureUvs(): TextureUvs = failwith "JS only"
            member __.refresh(): unit = failwith "JS only"

        and [<Import("mesh.Plane","PIXI")>] Plane(texture: Texture, ?segmentsX: float, ?segmentsY: float) =
            inherit Mesh(unbox null)
            member __.segmentsX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.segmentsY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("mesh.MeshRenderer","PIXI")>] MeshRenderer(renderer: WebGLRenderer) =
            inherit ObjectRenderer()
            member __.indices with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __._initWebGL(mesh: Mesh): unit = failwith "JS only"
            member __.render(mesh: Mesh): unit = failwith "JS only"
            member __.flush(): unit = failwith "JS only"
            member __.start(): unit = failwith "JS only"
            member __.destroy(): unit = failwith "JS only"

        // and MeshShader =
        //     inherit Shader


    module ticker =
        type [<Import("ticker.Ticker","PIXI")>] Ticker() =
            member __._emitter with get(): EventEmitter = failwith "JS only" and set(v: EventEmitter): unit = failwith "JS only"
            member __._requestId with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __._maxElapsedMS with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.autoStart with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.deltaTime with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.elapsedMS with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.lastTime with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.speed with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.started with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.FPS with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.minFPS with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __._tick(time: float): unit = failwith "JS only"
            member __._requestIfNeeded(): unit = failwith "JS only"
            member __._cancelIfNeeded(): unit = failwith "JS only"
            member __._startIfPossible(): unit = failwith "JS only"
            member __.add(fn: Func<float, unit>, ?context: obj): Ticker = failwith "JS only"
            member __.addOnce(fn: Func<float, unit>, ?context: obj): Ticker = failwith "JS only"
            member __.remove(fn: Func<float, unit>, ?context: obj): Ticker = failwith "JS only"
            member __.start(): unit = failwith "JS only"
            member __.stop(): unit = failwith "JS only"
            member __.update(): unit = failwith "JS only"

        type [<Import("ticker","PIXI")>] Globals =
            static member shared with get(): Ticker = failwith "JS only" and set(v: Ticker): unit = failwith "JS only"

    type [<Import("*","PIXI")>] Globals =
        static member VERSION with get(): string = failwith "JS only"
        static member PI_2 with get(): float = failwith "JS only"
        static member RAD_TO_DEG with get(): float = failwith "JS only"
        static member DEG_TO_RAD with get(): float = failwith "JS only"
        static member TARGET_FPMS with get(): float = failwith "JS only"
        static member RENDERER_TYPE with get(): RendererType = failwith "JS only"
        static member BLEND_MODES with get(): BlendModes = failwith "JS only"
        static member DRAW_MODES with get(): DrawModes = failwith "JS only"
        static member SCALE_MODES with get(): ScaleModes = failwith "JS only"
        static member RETINA_PREFIX with get(): string = failwith "JS only"
        static member RESOLUTION with get(): float = failwith "JS only"
        static member FILTER_RESOLUTION with get(): float = failwith "JS only"
        static member DEFAULT_RENDER_OPTIONS with get(): DefaultRenderOptions = failwith "JS only"
        static member SHAPES with get(): Shapes = failwith "JS only"
        static member SPRITE_BATCH_SIZE with get(): float = failwith "JS only"

        static member loader with get(): loaders.Loader = failwith "JS only"
        static member autoDetectRenderer(width: float, height: float, ?options: RendererOptions list, ?noWebGL: bool): U2<WebGLRenderer, CanvasRenderer> = failwith "JS only"

