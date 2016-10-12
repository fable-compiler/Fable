# Installing Fable and Launching Samples

## Release Builds

Fable is distributed through [npm](https://www.npmjs.com/package/fable-compiler)! You can install and run it just by typing:

```shell
npm install -g fable-compiler
fable path/to/your/project.fsproj
```

> Note the package name is `fable-compiler` while the command is just `fable`

## Using a local build

- Use `build.sh` (unix-like) or `build.cmd` (Windows) from the repo root directory to build Fable.
- Run `node <path-to-repo>/build/fable <arguments>` to run fable. You can run Fable without any arguments if there is a `fableconfig.json` in the current directory. 
 
## Building and Running samples

Many samples in the `samples/browser` folder depend on a running HTTP server.  A JS-compiled HTTP server is provided in `samples/node/server`

- Build the static server using `node build/fable samples/node/server`
- Build your sample of interest. For example, the d3map sample: `node build/fable samples/browser/d3map` 
- Make your current working directory the directory of the sample, and launch the server.
```
cd samples/browser/d3map
node ../../node/server/out
```
and point your brower to http://localhost:8080 

## TODO

Place `index.html` in the top samples directory, to point to the various samples, and provide server launch instructions.
