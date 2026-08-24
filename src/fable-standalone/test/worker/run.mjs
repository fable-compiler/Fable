// Drives dist/worker.min.js from Node, over the same Thoth-encoded messages a browser sends.
//
//   node run.mjs                      compile the fixture, check stack headroom
//   node run.mjs --files 12           more files
//   node run.mjs --dist <path>        a different build (e.g. an unpacked npm tarball)
//   node --stack-size=300 run.mjs     a browser worker gets roughly 700-810 of these

import fs from "node:fs"
import path from "node:path"
import vm from "node:vm"

const HERE = path.dirname(new URL(import.meta.url).pathname)

const arg = (name, fallback) => {
    const i = process.argv.indexOf(`--${name}`)
    return i >= 0 ? process.argv[i + 1] : fallback
}

const dist = path.resolve(arg("dist", path.join(HERE, "../../dist")))
const libDir = path.resolve(arg("metadata", path.join(HERE, "../../../fable-metadata/lib")))
const fileCount = Number(arg("files", "6"))

for (const [what, where] of [["worker", path.join(dist, "worker.min.js")], ["assemblies", libDir]]) {
    if (!fs.existsSync(where)) {
        console.error(`No ${what} at ${where}. Run './build.sh standalone' first, or pass --dist.`)
        process.exit(1)
    }
}

let onMessage = null
const answers = []
const headroom = []

const ctx = {
    console,
    setTimeout, clearTimeout, setInterval, clearInterval, queueMicrotask,
    performance, crypto, TextDecoder, TextEncoder,
    addEventListener: (kind, fn) => { if (kind === "message") onMessage = fn },
    postMessage: (msg) => answers.push(msg),
    importScripts: (p) => vm.runInContext(fs.readFileSync(path.join(dist, p), "utf8"), ctx, { filename: p }),
    fetch: async (url) => {
        const file = path.join(libDir, path.basename(url))
        if (!fs.existsSync(file)) return { ok: false, status: 404, statusText: `no such assembly: ${url}` }
        const b = fs.readFileSync(file)
        return { ok: true, arrayBuffer: async () => b.buffer.slice(b.byteOffset, b.byteOffset + b.byteLength) }
    },
    reportHeadroom: (frames) => headroom.push(frames),
}
ctx.self = ctx
ctx.globalThis = ctx
vm.createContext(ctx)

vm.runInContext(fs.readFileSync(path.join(dist, "worker.min.js"), "utf8"), ctx, { filename: "worker.min.js" })

// init() is called lazily on CreateChecker, so wrapping it here catches every compile
vm.runInContext(`
    __FABLE_STANDALONE__.init = ((init) => () => {
        const manager = init()
        const compile = manager.CompileToTargetAst.bind(manager)
        manager.CompileToTargetAst = function (...args) {
            let frames = 0
            const down = () => { frames++; down() }
            try { down() } catch { }
            reportHeadroom(frames)
            return compile(...args)
        }
        return manager
    })(__FABLE_STANDALONE__.init)
`, ctx)

const post = (request) => onMessage({ data: JSON.stringify(request) })

async function answer(expected) {
    for (;;) {
        while (answers.length) {
            const parsed = JSON.parse(answers.shift())
            if (parsed[0] === expected) return parsed
            if (parsed[0] === "CompilerCrashed") throw new Error(`CompilerCrashed: ${parsed[1]}`)
            if (parsed[0] === "LoadFailed") throw new Error("LoadFailed")
        }
        await new Promise((r) => setTimeout(r, 0))
    }
}

const fixture = Array.from({ length: fileCount }, (_, i) => ({
    Name: `Module${i}.fs`,
    Content: [
        `module Module${i}`,
        "",
        ...Array.from({ length: 60 }, (_, j) => [
            `let value${j} (input: int) =`,
            `    let doubled = input * 2`,
            `    let name = sprintf "%s-%i" "item" doubled`,
            `    let items = [ for k in 1..doubled -> k, name ]`,
            `    items |> List.filter (fun (k, _) -> k % 2 = 0) |> List.map fst |> List.sum`,
        ].join("\n")),
    ].join("\n"),
}))

const sizeKb = Math.round(fixture.reduce((n, f) => n + f.Content.length, 0) / 1024)
console.log(`worker: ${path.relative(process.cwd(), dist)}`)

post(["CreateChecker", "file:///assemblies", [], null, [], null])
await answer("Loaded")
console.log(`checker ready (${fs.readdirSync(libDir).filter((f) => f.endsWith(".dll")).length} assemblies)`)

console.log(`CompileFiles: ${fixture.length} files, ${sizeKb} KB`)
const started = Date.now()
post(["CompileFiles", fixture, [], "javascript", []])
const [, modules, , errors, stats] = await answer("CompilationsFinished")
console.log(`  ${modules.length} modules in ${((Date.now() - started) / 1000).toFixed(1)}s ` +
    `(type-check ${stats.FCS_parsing.toFixed(0)}ms, transform ${stats.Fable_transform.toFixed(0)}ms)`)

const failures = []
if (modules.length !== fixture.length) failures.push(`expected ${fixture.length} modules, got ${modules.length}`)

const hard = (errors ?? []).filter((e) => !e.IsWarning)
if (hard.length) failures.push(`${hard.length} compile error(s), first: ${hard[0].Message}`)

// V8 caps the stack in bytes, so a frame count is only comparable against another count from this
// same probe - hence a share of the first file rather than of any absolute capacity
const [first, ...rest] = headroom
const worst = Math.min(...headroom)
const kept = Math.round((worst / first) * 100)
console.log(`  stack headroom: ${first} frames on the first file, worst file kept ${kept}%`)
if (rest.length && kept < 95) {
    failures.push(`the worst file was handed ${kept}% of the stack the first one got ` +
        `(${headroom.join(", ")}) - a compile is running on a stack the one before it used up`)
}

const last = fixture[fixture.length - 1].Name
post(["CompileFiles", fixture, [last], "javascript", []])
const [, subset, , subsetErrors, subsetStats] = await answer("CompilationsFinished")
console.log(`re-emit ${last} only: ${subset.length} module ` +
    `(type-check ${subsetStats.FCS_parsing.toFixed(0)}ms, transform ${subsetStats.Fable_transform.toFixed(0)}ms ` +
    `vs ${stats.Fable_transform.toFixed(0)}ms for all)`)

if (subset.length !== 1) failures.push(`expected 1 module for a single-file emit, got ${subset.length}`)
if (subset[0] !== modules[modules.length - 1]) failures.push(`re-emitting ${last} produced different output`)
if ((subsetErrors ?? []).filter((e) => !e.IsWarning).length) failures.push("subset emit reported errors")
if (subsetStats.Fable_transform >= stats.Fable_transform) {
    failures.push(`emitting 1 of ${fixture.length} files took as long as emitting all of them`)
}

if (failures.length) {
    console.error("\nFAILED")
    for (const f of failures) console.error(`  - ${f}`)
    process.exit(1)
}
console.log("\nOK")
