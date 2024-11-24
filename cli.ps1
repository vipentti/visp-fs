#!/usr/bin/env pwsh
[CmdletBinding()]
param (
    [switch] $NoBuild,
    [Parameter(Mandatory=$false,ValueFromRemainingArguments=$true)]
    [string[]] $CmdArgs
)
function ExecSafe([scriptblock] $cmd) {
    & $cmd
    if ($LASTEXITCODE) { exit $LASTEXITCODE }
}

$buildArgs = @(
    "build"
    "-v", "q"
    "-tl:off"
)

if (-not $NoBuild) {
    ExecSafe { & dotnet @buildArgs | Out-Null }
}

$dotnetArgs = @(
    "src/Visp.Cli/bin/Debug/net9.0/Visp.Cli.dll"
)

ExecSafe { & dotnet @dotnetArgs @CmdArgs }
