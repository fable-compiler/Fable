import start, { ICompilerProxy } from "fable-compiler";

let cache: ICompilerProxy|null = null;

export default function getCompiler(args) {
    return cache == null ? cache = start(args) : cache;
}
