param(
    [Parameter(Mandatory = $true)][string]$TargetBase64,
    [Parameter(Mandatory = $true)][string]$ReadyName,
    [Parameter(Mandatory = $true)][string]$StartName,
    [Parameter(Mandatory = $true)][string]$StepName,
    [Parameter(Mandatory = $true)][string]$ContinueName,
    [Parameter(Mandatory = $true)][string]$DoneName,
    [Parameter(Mandatory = $true)][string]$ReleaseName,
    [Parameter(Mandatory = $true)][string]$CaseChangedName
)

$ErrorActionPreference = 'Stop'
$target = [Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($TargetBase64))
$directory = [IO.Path]::GetDirectoryName($target)
$utf8 = [Text.Encoding]::UTF8
$ready = [Threading.EventWaitHandle]::OpenExisting($ReadyName)
$start = [Threading.EventWaitHandle]::OpenExisting($StartName)
$step = [Threading.EventWaitHandle]::OpenExisting($StepName)
$continue = [Threading.EventWaitHandle]::OpenExisting($ContinueName)
$done = [Threading.EventWaitHandle]::OpenExisting($DoneName)
$release = [Threading.EventWaitHandle]::OpenExisting($ReleaseName)
$caseChanged = [Threading.EventWaitHandle]::OpenExisting($CaseChangedName)
$away = $target + '.away'

function Write-Target([string]$Text) {
    [IO.File]::WriteAllBytes($target, $utf8.GetBytes($Text))
}

function Replace-Target([string]$Text) {
    $temporary = Join-Path $directory ('.tpx-replace-' + [Guid]::NewGuid().ToString('N'))
    $backup = $temporary + '.backup'
    [IO.File]::WriteAllBytes($temporary, $utf8.GetBytes($Text))
    [IO.File]::Replace($temporary, $target, $backup)
    [IO.File]::Delete($backup)
}

function Complete-Step([scriptblock]$Action) {
    & $Action
    $step.Set() | Out-Null
    if (-not $continue.WaitOne(15000)) {
        throw 'The watcher test did not acknowledge the helper step'
    }
}

try {
    [IO.Directory]::CreateDirectory($directory) | Out-Null
    Write-Target 'initial'
    $ready.Set() | Out-Null
    if (-not $start.WaitOne(15000)) { throw 'The watcher test did not start' }

    Complete-Step { Write-Target 'in-place-one' }
    Complete-Step {
        $oldTime = [IO.File]::GetLastWriteTimeUtc($target)
        Write-Target 'in-place-two'
        [IO.File]::SetLastWriteTimeUtc($target, $oldTime)
    }
    Complete-Step { Replace-Target 'replacement-one' }
    Complete-Step { Replace-Target 'replacement-two' }
    Complete-Step { [IO.File]::Move($target, $away) }
    Complete-Step { [IO.File]::Move($away, $target) }
    Complete-Step { [IO.File]::Delete($target) }
    Complete-Step { Write-Target 'delete-recreate-final' }
    Complete-Step {
        $upper = Join-Path $directory ([IO.Path]::GetFileName($target).ToUpperInvariant())
        if ([IO.File]::Exists($upper) -and ($upper -cne $target)) {
            $temporary = $target + '.case-' + [Guid]::NewGuid().ToString('N')
            [IO.File]::Move($target, $temporary)
            [IO.File]::Move($temporary, $upper)
            $caseChanged.Set() | Out-Null
        }
    }
    Complete-Step {
        $sibling = Join-Path $directory 'unrelated sibling noise.txt'
        [IO.File]::WriteAllBytes($sibling, $utf8.GetBytes('noise'))
    }
    Complete-Step { Write-Target 'final-source-bytes' }

    $done.Set() | Out-Null
    $release.WaitOne(15000) | Out-Null
}
finally {
    foreach ($event in @($ready, $start, $step, $continue, $done, $release, $caseChanged)) {
        if ($null -ne $event) { $event.Dispose() }
    }
    if ([IO.Directory]::Exists($directory)) {
        [IO.Directory]::Delete($directory, $true)
    }
}
