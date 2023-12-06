#!/usr/bin/env pwsh
[CmdletBinding()]
param (
    [switch] $Release,
    [switch] $Full,
    [switch] $NoBuild,
    [ValidateSet("day1", "day2", "day3", "day4", "day5", "day6", "day7", "day8", "day9")]
    [string] $Day
)
function ExecSafe([scriptblock] $cmd) {
    & $cmd
    if ($LASTEXITCODE) { exit $LASTEXITCODE }
}

$root = $PSScriptRoot
$cliScript = Join-Path -Path $root -ChildPath "cli.ps1" -Resolve

$cliArgs = @(
    "./visp/examples/aoc2023/$Day.visp"
)
if ($Full) {
    $cliArgs += "full"
}
if ($Release) {
    $cliArgs += "--release"
}

ExecSafe {
    & $cliScript -NoBuild:$NoBuild @cliArgs
}
