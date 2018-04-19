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
    typedArrays?: boolean;
    clampByteArrays?: boolean;
};
export declare type FableSplitterOptions = {
    entry: string;
    outDir: string;
    path?: string;
    port?: number;
    babel?: Babel.TransformOptions;
    fable?: FableOptions;
    allFiles?: boolean;
    prepack?: any;
    postbuild?: () => void;
};
export declare function ensureDirExists(dir: string, cont?: () => void): void;
export default function fableSplitter(options: FableSplitterOptions, previousInfo?: CompilationInfo): Promise<CompilationInfo>;
