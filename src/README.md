# Code Structure

Hello, fellow developer! If you're here it means you're interested in Fable compiler source and maybe also in contributing. If that's the case, you're more than welcome! We'll be placing README files like this one in the relevant folders so it's easy to navigate the sources.

The main folders you can go from here are:

- `dotnet`: Projects that will be compiled as netstandard/netcore apps. This is the .NET part of Fable compiler.

- `templates`: dotnet SDK templates for Fable projects.

- `tests`: Contains the tests for Fable, they will be run both in .NET and Node platforms.

- `tools`: A place to put some files to help development of the other projects. The most interesting is QuickTest.fsx, to quickly test changes to the compiler (more instructions in the file itself).

- `js`: Despite the name, it contains both Typescript and JS projects, like the JS part of fable-core and the Webpack/Rollup loaders.
