#!/usr/bin/env pwsh

function ExecSafe([scriptblock] $cmd) {
    & $cmd
    if ($LASTEXITCODE) { exit $LASTEXITCODE }
}

ExecSafe {
    & npm run package
}

ExecSafe {
    & code --install-extension visp-fs.vsix
}
