// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.ProjectGenerator

open Visp.Compiler.Core
open System.IO.Abstractions
open System.IO

let tempDirPath name =
    let src_dir = __SOURCE_DIRECTORY__

    Path.Combine(src_dir, "..", "..", ".tmp", name) |> Path.GetFullPath

let coreLibPath name =
    let src_dir = __SOURCE_DIRECTORY__

    Path.Combine(src_dir, "..", "..", "visp", "lib", name) |> Path.GetFullPath

let runtimeLibPath =
    let src_dir = __SOURCE_DIRECTORY__

    Path.Combine(src_dir, "..", "Visp.Runtime.Library", "Visp.Runtime.Library.fsproj")
    |> Path.GetFullPath

let generateFsProjectFile (files: string seq) =
    let compileIncludes files =
        let compileInclude file =
            sprintf "  <Compile Include=\"%s\" />" file

        Seq.map compileInclude files
        |> String.concat (System.Environment.NewLine + "  ")

    let template =
        $"""
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <SatelliteResourceLanguages>en</SatelliteResourceLanguages>
    <InvariantGlobalization>true</InvariantGlobalization>
  </PropertyGroup>

  <ItemGroup>
  {compileIncludes (files)}
  </ItemGroup>

  <ItemGroup>
    <ProjectReference Include="{runtimeLibPath}" />
  </ItemGroup>

</Project>
"""

    template

[<RequireQualifiedAccess>]
type VispFile =
    | CoreLib of string
    | Lib of string
    | Main of string

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

type FsharpGenerator(fs: IFileSystem, dir: string) =
    member _.fs = fs
    member _.dir = dir

    member this.PathOf name =
        this.fs.Path.Combine(this.dir, name) |> this.fs.Path.GetFullPath

    member this.NameOfWithoutExtension(name: string) =
        this.fs.Path.GetFileNameWithoutExtension name

    member this.WriteVispFiles(files: VispFile list) =
        let dir = this.fs.Directory.CreateDirectory this.dir
        let existingFiles = dir.GetFiles("*.fs", SearchOption.TopDirectoryOnly)
        for file in existingFiles do
            file.Delete()

        let writeFile (file: VispFile) =
            let filePath = file.Path
            let name = this.NameOfWithoutExtension filePath
            let fsfileName = sprintf "%s.fs" name
            let outputPath = this.PathOf fsfileName
            let parsed = CoreParser.parseFile filePath file.ReturnLast

            (use stream = this.OpenFileForWriting outputPath
             CoreParser.writeParsedFile parsed stream file.Template)

            fsfileName

        let fileNames = List.map writeFile files

        let projTemplate = generateFsProjectFile fileNames

        let projPath = this.PathOf "project.fsproj"

        fs.File.WriteAllText(projPath, projTemplate)

        ()

    member this.OpenFileForWriting name =
        let stream = this.fs.FileStream.New(name, FileMode.Create)
        new StreamWriter(stream)
