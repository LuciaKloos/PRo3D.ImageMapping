<#
.SYNOPSIS
    Runs every case in cases.json through the headless export, runs the measurable
    checks (CLAUDE.md, checks 1-3) and prints a summary table.

.EXAMPLE
    powershell -NoProfile -ExecutionPolicy Bypass -File tests\shadows-highlights\run-all.ps1 `
        -Spice "C:\path\to\spice\kernels\mk\hera_ops.tm"

    Exit code 0 = all exports succeeded and all checks passed, 1 otherwise.
    Cases without a measurable check (combined, zero-radius, large-radius) are only
    exported; inspect their PNGs visually. Check 4 (vs. reference) is not run yet.
    The synthetic tests (--synthetic-tests) run as well unless -Synthetic:$false is given.
#>
param(
    [Parameter(Mandatory = $true)] [string]$Spice,

    [string]$InputImage,
    [string]$CasesFile,
    [string]$OutputDir,

    # skip "dotnet build" (use the existing build)
    [switch]$NoBuild,

    # also run the synthetic tests (--synthetic-tests); on by default, skip with -Synthetic:$false
    [switch]$Synthetic = $true
)

$ErrorActionPreference = 'Stop'

$testDir    = $PSScriptRoot
$projectDir = (Resolve-Path (Join-Path $testDir '..\..')).Path
$project    = Join-Path $projectDir 'PRo3D.ImageMapping.fsproj'

if (-not $InputImage) { $InputImage = Join-Path $testDir 'input\butte.png' }
if (-not $OutputDir)  { $OutputDir  = Join-Path $testDir 'output' }
if (-not $CasesFile) {
    # documented location first, then the project root
    $CasesFile = @((Join-Path $testDir 'cases.json'), (Join-Path $projectDir 'cases.json')) |
        Where-Object { Test-Path $_ } | Select-Object -First 1
}

foreach ($p in @($Spice, $InputImage, $CasesFile)) {
    if (-not $p -or -not (Test-Path $p)) { throw "File not found: $p" }
}

$Spice      = (Resolve-Path $Spice).Path
$InputImage = (Resolve-Path $InputImage).Path
New-Item -ItemType Directory -Force $OutputDir | Out-Null
$OutputDir  = (Resolve-Path $OutputDir).Path

# measurable check per case name
$checkCases = @('identity', 'shadows-only', 'highlights-only')

$inv = [Globalization.CultureInfo]::InvariantCulture
function Num([double]$v) { $v.ToString('0.######', $inv) }

# runs the app; returns @{ ExitCode; Output } (stderr merged, Aardvark log lines kept)
function Invoke-App([string[]]$appArgs) {
    $previous = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'      # native stderr must not throw in Windows PowerShell
    try {
        $output = & dotnet run --no-build --project $project -- @appArgs 2>&1 | ForEach-Object { "$_" }
        @{ ExitCode = $LASTEXITCODE; Output = $output }
    } finally {
        $ErrorActionPreference = $previous
    }
}

if (-not $NoBuild) {
    Write-Host 'Building ...'
    $previous = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'
    $buildOutput = & dotnet build $project 2>&1 | ForEach-Object { "$_" }
    $buildExit = $LASTEXITCODE
    $ErrorActionPreference = $previous
    if ($buildExit -ne 0) {
        $buildOutput | Where-Object { $_ -match ' error ' } | Select-Object -Unique | ForEach-Object { Write-Host $_ }
        throw 'Build failed.'
    }
}

$cases = Get-Content -Raw $CasesFile | ConvertFrom-Json
Write-Host ("Cases: {0} ({1})" -f $cases.Count, $CasesFile)
Write-Host "Input: $InputImage"
Write-Host ''

$results = @()

foreach ($case in $cases) {
    $outputPath = Join-Path $OutputDir ($case.name + '.png')
    Write-Host ("[{0}] exporting ..." -f $case.name)

    if (Test-Path $outputPath) { Remove-Item $outputPath }   # never check a stale output

    $export = Invoke-App @(
        '--spice', $Spice,
        '--test-image', $InputImage,
        '--highlight-amount', (Num $case.highlightAmount),
        '--highlight-tone',   (Num $case.highlightTone),
        '--highlight-radius', (Num $case.highlightRadius),
        '--shadow-amount',    (Num $case.shadowAmount),
        '--shadow-tone',      (Num $case.shadowTone),
        '--shadow-radius',    (Num $case.shadowRadius),
        '--output', $outputPath)

    $row = [ordered]@{
        Case   = $case.name
        Export = 'ok'
        Check  = '-'
        MaxRGB = ''
        Mean   = ''
        Detail = ''
    }

    if ($export.ExitCode -ne 0 -or -not (Test-Path $outputPath)) {
        $row.Export = 'FAILED'
        $row.Check  = 'FAIL'
        $err = $export.Output | Where-Object { $_ -match 'Exception|error' } | Select-Object -First 1
        $row.Detail = "exit $($export.ExitCode) $err".Trim()
    }
    elseif ($checkCases -contains $case.name) {
        Write-Host ("[{0}] checking ..." -f $case.name)
        $check = Invoke-App @('--check-case', $case.name, '--check-input', $InputImage, '--check-output', $outputPath)
        $text  = $check.Output -join "`n"

        switch ($check.ExitCode) {
            0       { $row.Check = 'PASS' }
            1       { $row.Check = 'FAIL' }
            default { $row.Check = "ERROR($($check.ExitCode))" }
        }

        if ($text -match 'max \|diff\| R/G/B = (\S+), mean \|diff\| R/G/B = (\S+)') {
            $row.MaxRGB = $Matches[1]
            $row.Mean   = $Matches[2]
        }

        $detail = @()
        if ($text -match 'pixels with diff > \d+: (\d+)')            { $detail += "px over tolerance: $($Matches[1])" }
        if ($text -match 'pixels with a decreased channel: (\d+)')  { $detail += "decreased px: $($Matches[1])" }
        if ($text -match 'pixels with an increased channel: (\d+)') { $detail += "increased px: $($Matches[1])" }
        if ($text -match 'mean luminance (\S+) -> (\S+) \(delta (\S+)\)') {
            $detail += "lum $($Matches[1]) -> $($Matches[2]) ($($Matches[3]))"
        }
        if ($text -match 'FAIL: (.+)')                              { $detail += $Matches[1] }
        $row.Detail = $detail -join '; '
    }
    else {
        $row.Check  = 'visual'
        $row.Detail = 'no measurable check; inspect the PNG'
    }

    $results += [pscustomobject]$row
}

if ($Synthetic) {
    $syntheticDir = Join-Path $OutputDir 'synthetic'
    Write-Host '[synthetic] running ...'
    $syntheticRun = Invoke-App @('--synthetic-tests', '--synthetic-output', $syntheticDir)

    # result lines: "PASS  <name>  (max |diff| <x>, mean |diff| <y>)", details indented below
    $current = $null
    foreach ($line in $syntheticRun.Output) {
        if ($line -match '^(PASS|FAIL)  (\S+)  \(max \|diff\| (\S+), mean \|diff\| (\S+)\)') {
            $current = [pscustomobject][ordered]@{
                Case   = "synthetic: $($Matches[2])"
                Export = 'ok'
                Check  = $Matches[1]
                MaxRGB = $Matches[3]
                Mean   = $Matches[4]
                Detail = ''
            }
            $results += $current
        }
        elseif ($current -and $line -match '^    (decreasing steps: .+|largest difference .+)$') {
            $current.Detail = $Matches[1]
        }
    }

    if ($syntheticRun.ExitCode -ne 0 -and -not ($results | Where-Object { $_.Case -like 'synthetic:*' -and $_.Check -eq 'FAIL' })) {
        $err = $syntheticRun.Output | Where-Object { $_ -match 'Exception|error' } | Select-Object -First 1
        $results += [pscustomobject][ordered]@{
            Case = 'synthetic'; Export = 'FAILED'; Check = "ERROR($($syntheticRun.ExitCode))"
            MaxRGB = ''; Mean = ''; Detail = "$err"
        }
    }
}

Write-Host ''
Write-Host "Summary (values 0-255; outputs in $OutputDir)"
$results | Format-Table -AutoSize -Wrap | Out-String -Width 220 | Write-Host

$failed  = @($results | Where-Object { $_.Export -ne 'ok' -or $_.Check -eq 'FAIL' -or $_.Check -like 'ERROR*' })
$checked = @($results | Where-Object { $_.Check -eq 'PASS' -or $_.Check -eq 'FAIL' })
$passed  = @($results | Where-Object { $_.Check -eq 'PASS' })
$visual  = @($results | Where-Object { $_.Check -eq 'visual' })

Write-Host ("{0} of {1} checks passed, {2} case(s) need visual inspection, {3} failure(s)" -f `
    $passed.Count, $checked.Count, $visual.Count, $failed.Count)

if ($failed.Count -eq 0) { exit 0 } else { exit 1 }
