#!/usr/bin/env pwsh
[CmdletBinding()]
param (
)

function ExecSafe([scriptblock] $cmd) {
    & $cmd
    if ($LASTEXITCODE) { exit $LASTEXITCODE }
}

$dotnetArgs = @(
    "build"
    "./src/Visp.LanguageServer"
    "--configuration", "Release"
)

ExecSafe {
    & dotnet @dotnetArgs
}
