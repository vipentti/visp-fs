[CmdletBinding()]
param (
)

function ExecSafe([scriptblock] $cmd) {
    & $cmd
    if ($LASTEXITCODE) { exit $LASTEXITCODE }
}

ExecSafe {
    & dotnet tool run fantomas ./src ./tests
}

ExecSafe {
    & dotnet tool run dotnet-csharpier ./src
}
