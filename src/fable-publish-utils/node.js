// @ts-check

const os = require("os");
const fs = require("fs");
const path = require("path");

class SingleObservable {
    constructor(onDispose) {
        this.onDispose = onDispose;
        this.disposed = false;
        this.listener = null;
    }
    Dispose() {
        if (!this.disposed) {
            this.onDispose ? this.onDispose() : void 0;
            this.listener ? this.listener.OnCompleted : void 0;
            this.disposed = true;
            this.listener = null;
        }
    }
    Trigger(v) {
        this.listener ? this.listener.OnNext(v) : void 0;
    }
    Subscribe(w) {
        const self = this;
        if (!this.disposed) {
            this.listener = w;
        }
        return {
            Dispose() {
                self.Dispose();
            }
        }
    }
}

export const File = {
    GetBytesLength(p) {
        return fs.lstatSync(p).size;
    },
    Exists(p) {
        return fs.existsSync(p) && fs.lstatSync(p).isFile();
    },
    Delete(p) {
        fs.unlinkSync(p);
    },
    Copy(source, target, overwrite) {
        const flags = overwrite ? 0 : fs.constants.COPYFILE_EXCL;
        fs.copyFileSync(source, target, flags)
    },
    WriteAllText(path, contents) {
        fs.writeFileSync(path, contents);
    },
    ReadAllText(p) {
        return fs.readFileSync(p).toString();
    },
    ReadLines(p) {
        const readline = require("readline");
        const rl = readline.createInterface({
            input: fs.createReadStream(p),
            // Note: we use the crlfDelay option to recognize all instances of CR LF
            // ('\r\n') in input.txt as a single line break.
            crlfDelay: Infinity,
        });
        const obs = new SingleObservable(() => rl.close());
        rl.on("line", line => obs.Trigger(line));
        rl.on("close", _ => obs.Dispose());
        return obs;
    },
}

export const Path = {
    Combine(...ps) {
        return path.join(...ps);
    },
    GetFullPath(p) {
        return path.resolve(p);
    },
    GetDirectoryName(p) {
        return path.dirname(p);
    },
    GetFileName(p) {
        return path.basename(p);
    },
    GetTempPath() {
        return os.tmpdir();
    }
}

export const Directory = {
    GetCurrentDirectory() {
        return process.cwd();
    },
    GetFiles(p) {
        return fs.readdirSync(p).map(f => path.join(p, f)).filter(f => fs.lstatSync(f).isFile());
    },
    GetDirectories(p) {
        return fs.readdirSync(p).map(d => path.join(p, d)).filter(d => fs.lstatSync(d).isDirectory())
    },
    Exists(p) {
        return fs.existsSync(p) && fs.lstatSync(p).isDirectory();
    },
    DeleteEmpty(p) {
        fs.rmdirSync(p);
    },
    CreateDirectory(p) {
        fs.mkdirSync(p, {
            recursive: true
        });
    },
}

export const Environment = {
    IsWindows() {
        return process.platform === "win32";
    },
    GetEnvironmentVariable(varName) {
        return process.env[varName];
    },
    SetEnvironmentVariable(varName, value) {
        return process.env[varName] = value;
    },
}

export const Json = {
    Parse(json) {
        return JSON.parse(json);
    },
    TryGetProperty(key, json) {
        return json[key];
    },
    GetString(jsonValue) {
        // TODO: check typeof?
        return jsonValue;
    },
}