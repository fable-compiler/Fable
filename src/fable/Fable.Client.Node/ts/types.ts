import * as types from "babel-types";
import * as constants from "./constants";

export type ContFunc = (data:any)=>void;

export interface Continuation {
    reject: ContFunc,
    resolve: ContFunc
}

export interface Scripts {
    prebuild?: string,
    postbuild?: string,
    ["postbuild-once"]?: string
}

export interface FableOptions {
    projFile: string[],
    watch: boolean | string | string[],
    watching?: keyof constants.STATUS,
    ecma: string,
    module: string,
    rollup: boolean | string | any,
    babelrc: boolean,
    babelPlugins: any[],
    workingDir: string,
    outDir: string,
    babel: any,
    sourceMaps: boolean | "inline" | "both",
    scripts: Scripts,
    coreLib: string,
    refs: {[k:string]: string}
    targets: {[k:string]: FableOptions},
    extra?: any,
    loose?: boolean,
    declaration?: boolean
}

export interface JsInclude {
    name: string,
    sourcePath: string
}

export interface BabelAst extends types.Node {
    isEntry: boolean,
    fileName: string,
    originalFileName: string,
    jsIncludes?: JsInclude[]
}

export interface Foo {
    isEntry: boolean,
    fileName: string,
    code: string,
    map: any
}