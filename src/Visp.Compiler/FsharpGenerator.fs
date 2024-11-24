// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.ProjectGenerator

open Visp.Compiler.Core
open System.IO.Abstractions
open System.IO
open System
open Visp.Compiler.Transforms.Helpers

[<RequireQualifiedAccess>]
type RuntimeLibraryReference =
    | Project
    | Package

let tempDirPath name =
    let src_dir = __SOURCE_DIRECTORY__

    Path.Combine(src_dir, "..", "..", ".tmp", name) |> Path.GetFullPath

let CoreLibRoot () =
    match Environment.GetEnvironmentVariable("VISP_FS_LIB_PATH") with
    | null ->
        let src_dir = __SOURCE_DIRECTORY__
        Path.Combine(src_dir, "..", "..", "visp", "lib") |> Path.GetFullPath
    | path -> path |> Path.GetFullPath

let coreLibPath name =
    Path.Combine(CoreLibRoot(), name) |> Path.GetFullPath

let runtimeLibPath =
    let src_dir = __SOURCE_DIRECTORY__

    Path.Combine(src_dir, "..", "Visp.Runtime.Library", "Visp.Runtime.Library.fsproj")
    |> Path.GetFullPath

[<Literal>]
let runtimePackageName = "Visp.Runtime.Library"

[<Literal>]
let commonPackageName = "Visp.Common"

let artifactsPackagePath =
    let src_dir = __SOURCE_DIRECTORY__
    Path.Combine(src_dir, "..", "..", "artifacts", "packages") |> Path.GetFullPath

let runtimePackagePath =
    let src_dir = __SOURCE_DIRECTORY__

    match Environment.GetEnvironmentVariable("VISP_FS_RUNTIME_PACKAGE_PATH") with
    | null ->
        if Directory.Exists(artifactsPackagePath) then
            artifactsPackagePath
        else
            Path.Combine(src_dir, "..", "Visp.Runtime.Library", "bin", "Release")
            |> Path.GetFullPath
    | it -> it

let commonPackagePath =
    let src_dir = __SOURCE_DIRECTORY__

    match Environment.GetEnvironmentVariable("VISP_FS_COMMON_PACKAGE_PATH") with
    | null ->
        if Directory.Exists(artifactsPackagePath) then
            artifactsPackagePath
        else
            Path.Combine(src_dir, "..", "Visp.Common", "bin", "Release") |> Path.GetFullPath
    | it -> it

let runtimePackageExists () =
    let path = runtimePackagePath

    if Directory.Exists(path) then
        Directory.GetFiles(path, $"{runtimePackageName}*.nupkg", SearchOption.TopDirectoryOnly)
        |> Seq.isEmpty
        |> not
    else
        false

let commonPackageExists () =
    let path = commonPackagePath

    if Directory.Exists(path) then
        Directory.GetFiles(path, $"{commonPackageName}*.nupkg", SearchOption.TopDirectoryOnly)
        |> Seq.isEmpty
        |> not
    else
        false


let private generateNuGetConfig () =
    let feedPath =
        match Environment.GetEnvironmentVariable("VISP_FS_PACKAGE_FEED_PATH") with
        | null -> None
        | path -> Some(path)

    match feedPath with
    | Some(feed) ->
        let t =
            $"""
<?xml version="1.0" encoding="utf-8"?>
<configuration>
    <fallbackPackageFolders>
        <add key="PackageFeed" value="{feed}" />
    </fallbackPackageFolders>
</configuration>
"""

        t.Trim() + Environment.NewLine

    | None ->
        let t =
            $"""
<?xml version="1.0" encoding="utf-8"?>
<configuration>
    <packageSources>
        <add key="CommonSource" value="{commonPackagePath}" />
        <add key="RuntimeSource" value="{runtimePackagePath}" />
    </packageSources>
</configuration>
"""

        t.Trim() + Environment.NewLine

let private runtimeProjectOrPackageReference (typ: RuntimeLibraryReference) =
    match typ with
    | RuntimeLibraryReference.Project -> $"    <ProjectReference Include=\"{runtimeLibPath}\" />"
    | RuntimeLibraryReference.Package ->
        $"""
    <PackageReference Include="{runtimePackageName}" Version="1.0.0" />
    <PackageReference Include="{commonPackageName}" Version="1.0.0" />
"""
    |> (fun (it: string) -> it.Trim([| '\r'; '\n' |]))

let private generateFsProjectFile
    (files: string seq)
    (deps: Set<Require>)
    (typ: RuntimeLibraryReference)
    (flags: string)
    =
    let compileIncludes files =
        let compileInclude file =
            sprintf "  <Compile Include=\"%s\" />" file

        Seq.map compileInclude files
        |> String.concat (System.Environment.NewLine + "  ")

    let pkgReferences deps =
        let pkgRef (Require(name, version)) =
            sprintf "  <PackageReference Include=\"%s\" Version=\"%s\" />" name version

        Seq.map pkgRef deps |> String.concat (System.Environment.NewLine + "  ")


    let template =
        $"""
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net9.0</TargetFramework>
    <SatelliteResourceLanguages>en</SatelliteResourceLanguages>
    <InvariantGlobalization>true</InvariantGlobalization>
    <OtherFlags>$(OtherFlags) {flags}</OtherFlags>
  </PropertyGroup>

  <ItemGroup>
  {compileIncludes (files)}
  </ItemGroup>

  <ItemGroup>
{runtimeProjectOrPackageReference typ}
  </ItemGroup>

  <ItemGroup>
  {pkgReferences deps}
  </ItemGroup>

</Project>
"""

    template

[<RequireQualifiedAccess>]
type VispFile =
    | CoreLib of string
    | Lib of string
    | Main of string

    member this.IsLibrary =
        match this with
        | CoreLib _
        | Lib _ -> true
        | _ -> false

    member this.ReturnLast =
        match this with
        | CoreLib _
        | Lib _ -> false
        | Main _ -> true

    member this.Path =
        match this with
        | CoreLib n -> coreLibPath n
        | Lib n
        | Main n -> n

    member this.Name =
        match this with
        | CoreLib n -> Path.GetFileName n
        | Lib n -> Path.GetFileName n
        | Main n -> Path.GetFileName n

    member this.NameWithoutExtension =
        match this with
        | CoreLib n -> Path.GetFileNameWithoutExtension n
        | Lib n -> Path.GetFileNameWithoutExtension n
        | Main n -> Path.GetFileNameWithoutExtension n

    member this.Template =
        match this with
        | CoreLib _
        | Lib _ ->
            $"""
// this file is auto-generated
[<AutoOpen>]
module ``{this.NameWithoutExtension}``

open Visp.Runtime.Library

let ARGV = System.Environment.GetCommandLineArgs()

"""
        | Main _ ->
            $"""
// This file is auto-generated
module ``{this.NameWithoutExtension}``

open Visp.Runtime.Library

let ARGV = System.Environment.GetCommandLineArgs()

let state = {{ Todo = () }}
"""

let CoreLibs = [ VispFile.CoreLib "core-macros.visp"; VispFile.CoreLib "core.visp" ]

type WriteOptions =
    { DebugTokens: bool
      DebugParse: bool
      Flags: string option }

    static member Default =
        { DebugTokens = false
          DebugParse = false
          Flags = None }


type FsharpGenerator(fs: IFileSystem, dir: string) =
    member _.fs = fs
    member _.dir = dir

    member this.PathOf name =
        this.fs.Path.Combine(this.dir, name) |> this.fs.Path.GetFullPath

    member this.NameOfWithoutExtension(name: string) =
        this.fs.Path.GetFileNameWithoutExtension name

    member this.WriteVispFiles
        (typ: RuntimeLibraryReference)
        (files: VispFile list)
        (options: WriteOptions)
        =
        let dir = this.fs.Directory.CreateDirectory this.dir
        let existingFiles = dir.GetFiles("*.fs", SearchOption.TopDirectoryOnly)

        for file in existingFiles do
            file.Delete()

        let writeFile (file: VispFile) =
            let filePath = file.Path
            let name = this.NameOfWithoutExtension filePath
            let fsfileName = sprintf "%s.fs" name
            let outputPath = this.PathOf fsfileName

            let parsed =
                CoreParser.parseFile
                    filePath
                    { ParserOptions.Default with
                        DebugTokens = options.DebugTokens && not file.IsLibrary
                        DebugParse = options.DebugParse && not file.IsLibrary
                        ReturnLast = not file.IsLibrary }

            let requires = Transforms.Helpers.getAllRequires parsed

            (use stream = this.OpenFileForWriting outputPath
             CoreParser.writeParsedFile parsed stream file.Template)

            (fsfileName, requires)

        let results = List.map writeFile files
        let fileNames = results |> List.map fst
        let requires = results |> List.map snd |> Set.unionMany

        let projTemplate =
            generateFsProjectFile fileNames requires typ (Option.defaultValue "" options.Flags)

        let projPath = this.PathOf "project.fsproj"

        fs.File.WriteAllText(projPath, projTemplate)

        match typ with
        | RuntimeLibraryReference.Package ->
            fs.File.WriteAllText(this.PathOf "nuget.config", generateNuGetConfig ())
        | _ -> ()

        ()

    member this.OpenFileForWriting name =
        let stream = this.fs.FileStream.New(name, FileMode.Create)
        new StreamWriter(stream)
