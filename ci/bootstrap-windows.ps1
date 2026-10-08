$ErrorActionPreference = 'Stop'
$installer = Join-Path $env:RUNNER_TEMP 'lazarus-4.8-fpc-3.2.2-win64.exe'
$lazarus = Join-Path $env:RUNNER_TEMP 'lazarus'
# Published at https://www.lazarus-ide.org/index.php?page=checksums (SHA-256).
$expected = 'ED25EE171D55E23CF14E0633159FDD2325EFBA56E186F8AB817AD3BF97D267D7'
$url = 'https://downloads.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%204.8/lazarus-4.8-fpc-3.2.2-win64.exe'
Invoke-WebRequest -Uri $url -OutFile $installer
if ((Get-FileHash $installer -Algorithm SHA256).Hash -ne $expected) {
    throw 'Lazarus installer checksum mismatch'
}
$process = Start-Process -FilePath $installer -Wait -PassThru -ArgumentList @(
    '/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', '/SP-', "/DIR=$lazarus"
)
if ($process.ExitCode -ne 0) { throw "Lazarus installer exited $($process.ExitCode)" }
$compiler = Join-Path $lazarus 'fpc/3.2.2/bin/x86_64-win64'
"$lazarus" | Out-File -FilePath $env:GITHUB_PATH -Append -Encoding utf8
"$compiler" | Out-File -FilePath $env:GITHUB_PATH -Append -Encoding utf8
"LAZARUS_DIR=$lazarus" | Out-File -FilePath $env:GITHUB_ENV -Append -Encoding utf8
"LAZBUILD=$lazarus/lazbuild.exe" | Out-File -FilePath $env:GITHUB_ENV -Append -Encoding utf8
"FPC=$compiler/fpc.exe" | Out-File -FilePath $env:GITHUB_ENV -Append -Encoding utf8
"TPX_COMPILER=$compiler/ppcx64.exe" | Out-File -FilePath $env:GITHUB_ENV -Append -Encoding utf8
"LAZARUS_SOURCE=4.8 official installer SHA256:$expected" | Out-File -FilePath $env:GITHUB_ENV -Append -Encoding utf8
