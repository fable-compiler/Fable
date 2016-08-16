# fable-samples-electron-helloworld

> Adapted from [electron-quick-start](https://github.com/electron/electron-quick-start)

**Clone and run for a quick way to see an Electron in action.**

This is a minimal Electron application based on the [Quick Start Guide](http://electron.atom.io/docs/latest/tutorial/quick-start) within the Electron documentation.

A basic Electron application needs just these files:

- `index.html` - A web page to render.
- `main.fsx` - Starts the app and creates a browser window to render HTML.
- `package.json` - Points to the app's main file and lists its details and dependencies.

You can learn more about each of these components within the [Quick Start Guide](http://electron.atom.io/docs/latest/tutorial/quick-start).

## To Use

To clone and run this repository you'll need [Git](https://git-scm.com) and [Node.js](https://nodejs.org/en/download/) (which comes with [npm](http://npmjs.com)) installed on your computer. From your command line:

```bash
# Install fable-compiler
npm install -g fable-compiler

# Clone this repository
git clone https://github.com/fable-compiler/Fable/samples/electron/helloworld

# Go into the repository
cd Fable/samples/electron/helloworld

# Run fable (fableconfig.json contains the compiler options)
fable
```

Learn more about Electron and its API in the [documentation](http://electron.atom.io/docs/latest).

#### License [CC0 (Public Domain)](LICENSE.md)