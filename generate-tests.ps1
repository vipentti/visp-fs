
[CmdletBinding()]
param (
    [switch] $NoBuild,
    [switch] $Release
)
function ExecSafe([scriptblock] $cmd) {
    & $cmd
    if ($LASTEXITCODE) { exit $LASTEXITCODE }
}

$root = $PSScriptRoot
$cliScript = Join-Path -Path $root -ChildPath "cli.ps1" -Resolve

$testScript = Join-Path -Path $root -ChildPath "./visp/utils/test-generator.visp" -Resolve

$cliArgs = @(
    $testScript
)
if ($Release) {
    $cliArgs += "--release"
}

ExecSafe {
    & $cliScript -NoBuild:$NoBuild @cliArgs
}
