[<RequireQualifiedAccess>]
module ChangelogParser

open Fable.Core
open System
open System.Text.RegularExpressions

[<AutoOpen>]
module Types =

    type CategoryBody =
        | ListItem of string
        | Text of string

    type OtherItem =
        {
            ListItem : string
            TextBody : string option
        }

    [<RequireQualifiedAccess>]
    type CategoryType =
        | Added
        | Changed
        | Deprecated
        | Removed
        | Improved
        | Fixed
        | Security
        | Unknown of string

        member this.Text
            with get () =
                match this with
                | Added -> "Added"
                | Changed -> "Changed"
                | Deprecated -> "Deprecated"
                | Removed -> "Removed"
                | Improved -> "Improved"
                | Fixed -> "Fixed"
                | Security -> "Security"
                | Unknown tag -> tag

    type Version =
        { Version : string option
          Title : string
          Date : DateTime option
          Categories : Map<CategoryType, CategoryBody list>
          OtherItems : OtherItem list }

    type Changelog =
        { Title : string
          Description : string
          Versions : Version list }

        static member Empty =
            { Title = ""
              Description = ""
              Versions = [] }

    [<RequireQualifiedAccess>]
    type Symbols =
        | Title of title : string
        | RawText of body : string
        | SectionHeader of title : string * version : string option * date : string option
        | SubSection of tag : string
        | ListItem of content : string


[<RequireQualifiedAccess>]
module Lexer =

    let private (|Match|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then
            Some m
        else
            None

    let private (|Title|_|) (input : string) =
        match input with
        | Match "^# ?[^#]" _ ->
            input.Substring(1).Trim()
            |> Some
        | _ -> None

    let private (|Semver|_|) (input : string) =
        match input with
        | Match "\\[?v?([\\w\\d.-]+\\.[\\w\\d.-]+[a-zA-Z0-9])\\]?" m ->
            Some m.Groups.[1].Value
        | _ ->
            None

    let private (|Date|_|) (input : string) =
        match input with
        | Match "(\\d{4}-\\d{2}-\\d{2})" m ->
            Some m.Groups.[0].Value
        | _ -> None

    let private (|Version|_|) (input : string) =
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

            Some (title, version, date)

        | _ -> None

    let private (|SubSection|_|) (input : string) =
        match input with
        | Match "^###" _ ->
            input.Substring(3).Trim()
            |> Some
        | _ -> None

    let private (|ListItem|_|) (input : string) =
        match input with
        | Match "^[*-]" _ ->
            input.Substring(1).Trim()
            |> Some
        | _ -> None

    let toSymbols (lines : string list) =
        lines
        |> List.map (function
            | Title title -> Symbols.Title title
            | Version (title, version, date) -> Symbols.SectionHeader (title, version, date)
            | SubSection tag -> Symbols.SubSection tag
            | ListItem content -> Symbols.ListItem content
            | rawText -> Symbols.RawText (rawText.TrimEnd())
        )

[<RequireQualifiedAccess>]
module Transform =

    let rec private parseCategoryBody (symbols : Symbols list) (sectionContent : CategoryBody list) =
        match symbols with
        | Symbols.ListItem item::tail ->

            parseCategoryBody tail (sectionContent @ [ CategoryBody.ListItem item ])
        // If this is the beginning of a text block
        | Symbols.RawText _::_ ->
            // Capture all the lines of the text block
            let textLines =
                symbols
                |> List.takeWhile (function
                    | Symbols.RawText _ -> true
                    | _ -> false
                )

            // Regroup everything into a single string
            let content =
                textLines
                |> List.map (function
                    | Symbols.RawText text -> text
                    | _ -> failwith "Should not happen the list has been filtered"
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
            let rest =
                symbols
                |> List.skip textLines.Length

            // If details is an empty line don't store it
            if String.IsNullOrEmpty content then
                parseCategoryBody rest sectionContent
            else
                parseCategoryBody rest (sectionContent @ [ CategoryBody.Text content ])

        // End of the Section, return the built content
        | _ -> symbols, sectionContent

    let rec private tryEatRawText (symbols : Symbols list) =
        match symbols with
        // If this is the beginning of a text block
        | Symbols.RawText _::_ ->
            // Capture all the lines of the text block
            let textLines =
                symbols
                |> List.takeWhile (function
                    | Symbols.RawText _ -> true
                    | _ -> false
                )

            // Regroup everything into a single string
            let content =
                textLines
                |> List.map (function
                    | Symbols.RawText text -> text
                    | _ -> failwith "Should not happen the list has been filtered"
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
            let rest =
                symbols
                |> List.skip textLines.Length

            rest, Some content
        // End of the Section, return the built content
        | _ -> symbols, None

    let rec private parse (symbols : Symbols list) (changelog : Changelog) =
        match symbols with
        | Symbols.Title title::tail ->
            if String.IsNullOrEmpty changelog.Title then
                { changelog with Title = title }
            else
                printfn "Title has already been filled."
                printfn $"Discarding: %s{title}"
                changelog
            |> parse tail

        | Symbols.SectionHeader (title, version, date)::tail ->
            let version =
                {
                    Version = version
                    Title = title
                    Date = date |> Option.map DateTime.Parse
                    Categories = Map.empty
                    OtherItems = []
                }

            parse tail { changelog with Versions = version :: changelog.Versions }

        | Symbols.SubSection tag::tail ->
            let (unparsedSymbols, categoryBody) = parseCategoryBody tail []

            let categoryType =
                match tag.ToLower() with
                | "added" -> CategoryType.Added
                | "changed" -> CategoryType.Changed
                | "deprecated" -> CategoryType.Deprecated
                | "removed" -> CategoryType.Removed
                | "improved" -> CategoryType.Improved
                | "fixed" -> CategoryType.Fixed
                | "security" -> CategoryType.Security
                | unknown -> CategoryType.Unknown unknown

            match changelog.Versions with
            | currentVersion::otherVersions ->
                let categoryBody =
                    match Map.tryFind categoryType currentVersion.Categories with
                    | Some existingBody ->
                        existingBody @ categoryBody
                    | None -> categoryBody

                let updatedCategories = currentVersion.Categories.Add(categoryType, categoryBody)
                let versions = { currentVersion with Categories = updatedCategories } :: otherVersions
                parse unparsedSymbols { changelog with Versions = versions }
            | _ ->
                Error "A category should always be under a version"

        | Symbols.RawText _::_ ->
            // Capture all the lines of the text block
            let textLines =
                symbols
                |> List.takeWhile (function
                    | Symbols.RawText _ -> true
                    | _ -> false
                )

            // Regroup everything into a single string
            let content =
                textLines
                |> List.map (function
                    | Symbols.RawText text -> text
                    | _ -> failwith "Should not happen the list has been filtered"
                )
                |> String.concat "\n"

            // Remove already handle symbols
            let rest =
                symbols
                |> List.skip textLines.Length

            parse rest { changelog with Description = content }

        | Symbols.ListItem text :: tail ->
            match changelog.Versions with
            | currentVersion::otherVersions ->
                let (unparsedSymbols, textBody) = tryEatRawText tail

                let otherItemItem =
                    {
                        ListItem = text
                        TextBody = textBody
                    }

                let versions =
                    { currentVersion with
                        OtherItems = currentVersion.OtherItems @ [ otherItemItem ]
                    } :: otherVersions

                parse unparsedSymbols { changelog with Versions = versions }
            | _ ->

                sprintf "A list item should always be under version. The following list item made the parser failed:\n\n%s\n" text
                |> Error

        | [] -> Ok { changelog with Versions = changelog.Versions |> List.rev }

    let fromSymbols (symbols : Symbols list) =
        parse symbols Changelog.Empty

let parse (changelogContent : string) =
    changelogContent.Split([| '\r'; '\n' |])
    |> Array.toList
    |> Lexer.toSymbols
    |> Transform.fromSymbols
