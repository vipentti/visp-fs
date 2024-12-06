#!/usr/bin/env pwsh
[CmdletBinding()]
param (
    [switch] $Release,
    [switch] $Full,
    [switch] $All,
    [switch] $NoBuild,
    [ValidateSet(
        "day1", "day2", "day3", "day4", "day5", "day6"
        , "day7", "day8", "day9"
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

$AllDays = @(
    "day1", "day2", "day3", "day4", "day5", "day6", "day7", "day8", "day9"
    , "day10", "day11", "day12", "day13", "day14", "day15", "day16"
    , "day17", "day18", "day19", "day20", "day21", "day22"
    , "day23", "day24", "day25"
)

$root = $PSScriptRoot
$cliScript = Join-Path -Path $root -ChildPath "cli.ps1" -Resolve

if ($All) {
    $NoBuild = $false

    $SelectedDays = $AllDays

    if ($Day) {
        $SelectedDays = @()
        $DidFind = $false
        foreach ($ad in $AllDays) {
            if ($ad -eq $Day) {
                $DidFind = $true
                $SelectedDays += $ad
            }
            elseif ($DidFind) {
                $SelectedDays += $ad
            }
        }
    }

    foreach ($ad in $SelectedDays) {
        $cliArgs = @(
            "./visp/examples/aoc2023/$ad.visp"
        )
        if ($Full) {
            $cliArgs += "full"
        }
        if ($Release) {
            $cliArgs += "--release"
        }

        Write-Host "Running: $ad"

        ExecSafe {
            & $cliScript -NoBuild:$NoBuild @cliArgs
        }

        $NoBuild = $true
    }
}
else {
    $cliArgs = @(
        "./visp/examples/aoc2024/$Day.visp"
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
}

