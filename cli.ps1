
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
)

if (-not $NoBuild) {
    ExecSafe { & dotnet @buildArgs }
}

$dotnetArgs = @(
    "run"
    "--project", "./src/Visp.Cli"
    "--no-build"
)

ExecSafe { & dotnet @dotnetArgs `-- @CmdArgs }
