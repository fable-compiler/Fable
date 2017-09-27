import * as Babel from "babel-core";
export declare type CompilationInfo = {
    entry: string;
    projectFiles: string[];
    compiledPaths: Set<string>;
    dedupOutPaths: Set<string>;
    mapInOutPaths: Map<string, string>;
    logs: {
        [severity: string]: string[];
    };
};
export declare type FableOptions = {
    define?: string[];
    plugins?: string[];
    fableCore?: string;
    typedArrays?: boolean;
    clampByteArrays?: boolean;
};
export declare type FableCompilerOptions = {
    entry: string;
    outDir: string;
    path?: string;
    port?: number;
    babel?: Babel.TransformOptions;
    fable?: FableOptions;
    prepack?: any;
    extra?: {
        [k: string]: any;
    };
    postbuild?: () => void;
};
export declare function ensureDirExists(dir: string, cont?: () => void): void;
export default function fableSplitter(options: FableCompilerOptions, previousInfo?: CompilationInfo): Promise<CompilationInfo>;
