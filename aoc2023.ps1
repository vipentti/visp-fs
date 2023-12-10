#!/usr/bin/env pwsh
[CmdletBinding()]
param (
    [switch] $Release,
    [switch] $Full,
    [switch] $NoBuild,
    [ValidateSet(
        "day1", "day2", "day3", "day4", "day5", "day6", "day7", "day8", "day9"
        , "day10", "day11", "day12", "day13", "day14", "day15", "day16"
        , "day17", "day18", "day19", "day20", "day21", "day22"
        , "day23", "day24", "day25"
    )]
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
