#!/usr/bin/env pwsh

[CmdletBinding()]
param (
    [Parameter(Position=0,Mandatory=$false,ValueFromRemainingArguments=$true)]
    [string[]] $CmdArgs
)
function ExecSafe([scriptblock] $cmd) {
    & $cmd
    if ($LASTEXITCODE) { exit $LASTEXITCODE }
}


ExecSafe {
    & dotnet tool run dotnet-verify @CmdArgs
}
