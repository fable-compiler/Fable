export type BuildResult = "SUCCESS" | "FAIL" | "NEEDS_FULL_REBUILD";

export type WatchStatus = "WATCHING" | "BUILDING";

export type Dic<T> = { [k: string]: T }

export interface Continuation {
    reject: Function,
    resolve: Function
}

export interface Scripts {
    prebuild?: string,
    postbuild?: string,
    ["postbuild-once"]?: string
}

export interface FableOptions {
    projFile: string[],
    watch: boolean | string | string[],
    watching?: WatchStatus,
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
    declaration?: boolean,
    verbose?: boolean,
    inMemory?: boolean
}

export interface JsInclude {
    name: string,
    sourcePath: string
}

export interface BabelAst {
    isEntry: boolean,
    fileName: string,
    originalFileName: string,
    jsIncludes?: JsInclude[]
}

export interface BabelFile {
    isEntry: boolean,
    fileName: string,
    code: string,
    map: Object
};

export interface Foo {
    isEntry: boolean,
    fileName: string,
    code: string,
    map: any
}