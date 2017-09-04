 - tagline: Manage your Fable libraries like a pro

# Paket integration

Fable 1.0 beta has been out in the wild for several weeks now and the community has already provided very valuable feedback. Some people are concerned about the mix of .NET and JS tools, as it can be a bit confusing if you are used to a single ecosystem. This is actually a good thing(TM), because the effort you put to learn about dotnet SDK or Webpack is not exclusive to Fable, and can be reused for other .NET/JS projects. There are already big communities where you can get support from if you have an issue with the tooling as well.

However it's true there were some friction points we had to fix. The main one concerned Fable libraries: by nature they belong to the .NET ecosystem, but their destiny is to be converted to JS and must be distributed by source (Fable cannot read assemblies, only raw F# code), something Nuget (the register for .NET libraries) is not designed for. Because of these reasons, we've been distributing libraries through npm so far, though this wasn't ideal either as npm obviously is not prepared for F# packages and it was creating other problems on its own.

## Paket

We wanted to move libraries back to the .NET side of things, but Fable still needed some collaboration from the package manager to locate the packages and know the dependency order, something Nuget cannot offer. Also, Fable community was very concerned about the need of **lock files** to ensure reproducible builds of the samples and templates in all machines. Enter [Paket](https://fsprojects.github.io/Paket/), a package manager that has been around for a long time and it's the choice of trust for most F# projects (and also many C# ones). Its author, [Steffen Forkmann](https://twitter.com/sforkmann), is also a great Fable supporter and user and he's been really fast to implement the features needed by Fable. Thanks to this I'm happy to announce Fable now comes with Paket integration, and the best thing is you may not even notice! Paket is also integrated with the dotnet SDK: you only need to run `dotnet restore` to download your packages (if you need to add or update dependencies, you can do it normally as with other Paket projects).

Fable simple (`Fable.Template`) and Elmish (`Fable.Template.Elmish.React`) templates have been updated to use Paket, download or update them to start enjoying reproducible builds and sane dependency management!

```shell
dotnet new -i Fable.Template::*
dotnet new fable -n MyProject
```


## Yarn

Fable libraries will managed by Paket from now on, but we still need development tools like Webpack and other JS libraries from npm. Although not a strict requirement, yarn will be now the tool of choice for Fable samples and templates to manage JS packages, as it provides similar guarantees for reproducible builds as Paket thanks to the use of lock files.

> Please note that, unlike Paket, yarn requires a global installation in your system. Check [their website](https://yarnpkg.com/en/docs/install) for details.

## For library authors

At the moment we're using a couple of conventions so Fable can handle the code from libraries:

- Fable libraries must be prefixed with `Fable` (e.g. `Fable.Elmish`).
- The project file (with same name as the package) must be put together with the sources in a folder named `fable` within the package.

The last point may look complicated, but it's only a matter of adding a [couple of lines](https://github.com/fable-elmish/react/blob/1e97c734bfd943899958e1ca849974e3baea3500/src/Fable.Elmish.React.fsproj#L15-L19) to your project file and let the `dotnet pack` command do all the job :)

<br />

With Paket integration, the workflow for Fable 1.0 is basically done and we are now ready to publish the stable release very soon after fixing last issues and updating the documentation. But remember you can already use Fable 1.0 for your projects just by downloading the templates and creating a new project with them.

And remember: [FableConf](https://www.eventbrite.es/e/fableconf-bordeaux-tickets-34089709238) is approaching, get your ticket before it's too late!
