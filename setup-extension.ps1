#!/usr/bin/env pwsh

[CmdletBinding()]
param (
    [switch] $NoBuild,
    [switch] $NoInstall
)

function ExecSafe([scriptblock] $cmd) {
    & $cmd
    if ($LASTEXITCODE) { exit $LASTEXITCODE }
}


$scriptPath = $PSScriptRoot
$extensionPath = Join-Path -Path $scriptPath -ChildPath "editors/vscode/visp-fs" -Resolve

if (-not $NoBuild) {
    $dotnetArgs = @(
        "build"
        "./src/Visp.LanguageServer"
        "--configuration", "Release"
    )

    ExecSafe { & dotnet @dotnetArgs }
}

$curDir = $pwd

try {
    Set-Location -Path $extensionPath

    if (-not $NoInstall) {
        ExecSafe { & npm install }
    }

    ExecSafe { & npm run package }

    $extensionFile = Join-Path -Path $extensionPath -ChildPath "visp-fs.vsix" -Resolve

    ExecSafe { & code --install-extension $extensionFile }
}
finally {
    Set-Location -Path $curDir
}
