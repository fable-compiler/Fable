[<RequireQualifiedAccess>]
module ChangelogParser

open Fable.Core
open System
open System.Text.RegularExpressions
open Semver

[<AutoOpen>]
module Types =

    type CategoryBody =
        | ListItem of string
        | Text of string
        | Section of string

    type OtherItem =
        {
            ListItem: string
            TextBody: string option
        }

    type Categories =
        {
            Added: CategoryBody list
            Changed: CategoryBody list
            Deprecated: CategoryBody list
            Removed: CategoryBody list
            Improved: CategoryBody list
            Fixed: CategoryBody list
            Security: CategoryBody list
            Custom: Map<string, CategoryBody list>
        }

    type Version =
        {
            Version: SemVersion
            Title: string
            Date: DateTime option
            Categories: Categories
            OtherItems: OtherItem list
        }

    type Changelog =
        {
            Title: string
            Description: string
            Versions: Version list
        }

        static member Empty =
            {
                Title = ""
                Description = ""
                Versions = []
            }

    [<RequireQualifiedAccess>]
    type Symbols =
        | Title of title: string
        | RawText of body: string
        | SectionHeader of
            title: string *
            version: string option *
            date: string option
        | SubSection of tag: string
        | SubSubSection of tag: string
        | ListItem of content: string


[<RequireQualifiedAccess>]
module Lexer =

    [<return: Struct>]
    let private (|Match|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            ValueSome m
        else
            ValueNone

    [<return: Struct>]
    let private (|Title|_|) (input: string) =
        match input with
        | Match "^# ?[^#]" _ -> input.Substring(1).Trim() |> ValueSome
        | _ -> ValueNone

    [<return: Struct>]
    let private (|Semver|_|) (input: string) =
        match input with
        | Match "\\[?v?([\\w\\d.-]+\\.[\\w\\d.-]+[a-zA-Z0-9])\\]?" m ->
            ValueSome m.Groups.[1].Value
        | _ -> ValueNone

    [<return: Struct>]
    let private (|Date|_|) (input: string) =
        match input with
        | Match "(\\d{4}-\\d{2}-\\d{2})" m -> ValueSome m.Groups.[0].Value
        | _ -> ValueNone

    [<return: Struct>]
    let private (|Version|_|) (input: string) =
        match input with
        | Match "^##? ?[^#]" _ ->
            let version =
                match input with
                | Semver version -> Some version
                | _ -> None

            let date =
                match input with
                | Date date -> Some date
                | _ -> None

            let title = input.Substring(2).Trim()

            ValueSome(title, version, date)

        | _ -> ValueNone

    [<return: Struct>]
    let private (|SubSection|_|) (input: string) =
        match input with
        | Match "^### ?[^#]" _ -> input.Substring(3).Trim() |> ValueSome
        | _ -> ValueNone

    [<return: Struct>]
    let private (|SubSubSection|_|) (input: string) =
        match input with
        | Match "^#### ?[^#]" _ -> input.Substring(4).Trim() |> ValueSome
        | _ -> ValueNone

    [<return: Struct>]
    let private (|ListItem|_|) (input: string) =
        match input with
        | Match "^[*-]" _ -> input.Substring(1).Trim() |> ValueSome
        | _ -> ValueNone

    let toSymbols (lines: string list) =
        lines
        |> List.map (
            function
            | Title title -> Symbols.Title title
            | Version(title, version, date) ->
                Symbols.SectionHeader(title, version, date)
            | SubSection tag -> Symbols.SubSection tag
            | SubSubSection tag -> Symbols.SubSubSection tag
            | ListItem content -> Symbols.ListItem content
            | rawText -> Symbols.RawText(rawText.TrimEnd())
        )

[<RequireQualifiedAccess>]
module Transform =

    let rec private parseCategoryBody
        (symbols: Symbols list)
        (sectionContent: CategoryBody list)
        =
        match symbols with
        | Symbols.ListItem item :: tail ->

            parseCategoryBody
                tail
                (sectionContent @ [ CategoryBody.ListItem item ])
        // If this is the beginning of a text block
        | Symbols.RawText _ :: _ ->
            // Capture all the lines of the text block
            let textLines =
                symbols
                |> List.takeWhile (
                    function
                    | Symbols.RawText _ -> true
                    | _ -> false
                )

            // Regroup everything into a single string
            let content =
                textLines
                |> List.map (
                    function
                    | Symbols.RawText text -> text
                    | _ ->
                        failwith "Should not happen the list has been filtered"
                )
                // Skip empty lines at the beginning
                |> List.skipWhile String.IsNullOrEmpty
                // Skip empty lines at the end
                |> List.rev
                |> List.skipWhile String.IsNullOrEmpty
                |> List.rev
                // Join the lines
                |> String.concat "\n"

            // Remove already handle symbols
            let rest = symbols |> List.skip textLines.Length

            // If details is an empty line don't store it
            if String.IsNullOrEmpty content then
                parseCategoryBody rest sectionContent
            else
                parseCategoryBody
                    rest
                    (sectionContent @ [ CategoryBody.Text content ])

        | Symbols.SubSubSection tag :: tail ->
            parseCategoryBody
                tail
                (sectionContent @ [ CategoryBody.Section tag ])

        // End of the Section, return the built content
        | _ -> symbols, sectionContent

    let rec private tryEatRawText (symbols: Symbols list) =
        match symbols with
        // If this is the beginning of a text block
        | Symbols.RawText _ :: _ ->
            // Capture all the lines of the text block
            let textLines =
                symbols
                |> List.takeWhile (
                    function
                    | Symbols.RawText _ -> true
                    | _ -> false
                )

            // Regroup everything into a single string
            let content =
                textLines
                |> List.map (
                    function
                    | Symbols.RawText text -> text
                    | _ ->
                        failwith "Should not happen the list has been filtered"
                )
                // Skip empty lines at the beginning
                |> List.skipWhile String.IsNullOrEmpty
                // Skip empty lines at the end
                |> List.rev
                |> List.skipWhile String.IsNullOrEmpty
                |> List.rev
                // Join the lines
                |> String.concat "\n"

            // Remove already handle symbols
            let rest = symbols |> List.skip textLines.Length

            rest, Some content
        // End of the Section, return the built content
        | _ -> symbols, None

    let rec private parse (symbols: Symbols list) (changelog: Changelog) =
        match symbols with
        | Symbols.Title title :: tail ->
            if String.IsNullOrEmpty changelog.Title then
                { changelog with Title = title }
            else
                changelog
            |> parse tail

        | Symbols.SubSubSection _ :: tail ->
            // Should not happen here, it should be captured in the parseCategoryBody function
            parse tail changelog

        | Symbols.SectionHeader(title, version, date) :: tail ->
            let version =
                {
                    Version =
                        match version with
                        | Some version ->
                            SemVersion.Parse(version, SemVersionStyles.Strict)
                        | None ->
                            // If no version is provided, use a dummy version
                            // This happens when handling the unreleased section
                            SemVersion.Parse(
                                "0.0.0-Unreleased",
                                SemVersionStyles.Strict
                            )
                    Title = title
                    Date = date |> Option.map DateTime.Parse
                    Categories =
                        {
                            Added = []
                            Changed = []
                            Deprecated = []
                            Removed = []
                            Improved = []
                            Fixed = []
                            Security = []
                            Custom = Map.empty
                        }
                    OtherItems = []
                }

            parse
                tail
                { changelog with Versions = version :: changelog.Versions }

        | Symbols.SubSection tag :: tail ->
            let (unparsedSymbols, categoryBody) = parseCategoryBody tail []

            match changelog.Versions with
            | currentVersion :: otherVersions ->
                let updatedCategories =
                    match tag.ToLower() with
                    | "added" ->
                        { currentVersion.Categories with
                            Added =
                                currentVersion.Categories.Added @ categoryBody
                        }
                    | "changed" ->
                        { currentVersion.Categories with
                            Changed =
                                currentVersion.Categories.Changed @ categoryBody
                        }
                    | "deprecated" ->
                        { currentVersion.Categories with
                            Deprecated =
                                currentVersion.Categories.Deprecated
                                @ categoryBody
                        }
                    | "removed" ->
                        { currentVersion.Categories with
                            Removed =
                                currentVersion.Categories.Removed @ categoryBody
                        }
                    | "improved" ->
                        { currentVersion.Categories with
                            Improved =
                                currentVersion.Categories.Improved
                                @ categoryBody
                        }
                    | "fixed" ->
                        { currentVersion.Categories with
                            Fixed =
                                currentVersion.Categories.Fixed @ categoryBody
                        }
                    | "security" ->
                        { currentVersion.Categories with
                            Security =
                                currentVersion.Categories.Security
                                @ categoryBody
                        }
                    | unknown ->
                        { currentVersion.Categories with
                            Custom =
                                currentVersion.Categories.Custom.Add(
                                    unknown,
                                    categoryBody
                                )
                        }

                let versions =
                    { currentVersion with Categories = updatedCategories }
                    :: otherVersions

                parse unparsedSymbols { changelog with Versions = versions }
            | _ -> Error "A category should always be under a version"

        | Symbols.RawText _ :: _ ->
            // Capture all the lines of the text block
            let textLines =
                symbols
                |> List.takeWhile (
                    function
                    | Symbols.RawText _ -> true
                    | _ -> false
                )

            // Regroup everything into a single string
            let content =
                textLines
                |> List.map (
                    function
                    | Symbols.RawText text -> text
                    | _ ->
                        failwith "Should not happen the list has been filtered"
                )
                |> String.concat "\n"

            // Remove already handle symbols
            let rest = symbols |> List.skip textLines.Length

            parse rest { changelog with Description = content }

        | Symbols.ListItem text :: tail ->
            match changelog.Versions with
            | currentVersion :: otherVersions ->
                let (unparsedSymbols, textBody) = tryEatRawText tail

                let otherItemItem =
                    {
                        ListItem = text
                        TextBody = textBody
                    }

                let versions =
                    { currentVersion with
                        OtherItems =
                            currentVersion.OtherItems @ [ otherItemItem ]
                    }
                    :: otherVersions

                parse unparsedSymbols { changelog with Versions = versions }
            | _ ->

                sprintf
                    "A list item should always be under version. The following list item made the parser failed:\n\n%s\n"
                    text
                |> Error

        | [] -> Ok { changelog with Versions = changelog.Versions |> List.rev }

    let fromSymbols (symbols: Symbols list) = parse symbols Changelog.Empty

let parse (changelogContent: string) =
    changelogContent.Split(
        [|
            '\r'
            '\n'
        |]
    )
    |> Array.toList
    |> Lexer.toSymbols
    |> Transform.fromSymbols

module Version =

    let bodyAsMarkdown (version: Types.Version) =
        let renderCategoryBody
            (categoryLabel: string)
            (items: CategoryBody list)
            =
            if items.IsEmpty then
                ""
            else
                let categoryBody =
                    items
                    |> List.map (
                        function
                        | ListItem item -> sprintf "- %s" item
                        | Text text -> text
                        | Section section -> sprintf "\n### %s\n" section
                    )
                    |> String.concat "\n"

                $"## {categoryLabel}\n\n{categoryBody}\n\n"

        let mutable body = ""

        body <- body + renderCategoryBody "Added" version.Categories.Added
        body <- body + renderCategoryBody "Changed" version.Categories.Changed

        body <-
            body + renderCategoryBody "Deprecated" version.Categories.Deprecated

        body <- body + renderCategoryBody "Removed" version.Categories.Removed
        body <- body + renderCategoryBody "Improved" version.Categories.Improved
        body <- body + renderCategoryBody "Fixed" version.Categories.Fixed
        body <- body + renderCategoryBody "Security" version.Categories.Security

        for unkownCategory in version.Categories.Custom.Keys do
            let items = version.Categories.Custom.[unkownCategory]
            body <- body + renderCategoryBody unkownCategory items

        if not version.OtherItems.IsEmpty then
            body <- body + "\n"

        for otherItems in version.OtherItems do
            body <- body + sprintf "- %s" otherItems.ListItem

            match otherItems.TextBody with
            | Some textBody -> body <- body + sprintf "\n%s" textBody
            | None -> ()

            body <- body + "\n"

        body
