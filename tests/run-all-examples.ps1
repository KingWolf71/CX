# Test all LJ2 examples
$ErrorActionPreference = "SilentlyContinue"
$lj2 = ".\lj2.exe"
$passed = 0
$failed = 0
$failedTests = @()

Get-ChildItem "Examples\*.lj" | Sort-Object Name | ForEach-Object {
    $name = $_.Name
    $output = & $lj2 -t $_.FullName 2>&1 | Out-String

    if ($output -match "LOAD ERROR|Compile Error|^Error:") {
        $failed++
        $failedTests += $name
        Write-Host "FAIL: $name" -ForegroundColor Red
    } else {
        $passed++
        Write-Host "PASS: $name" -ForegroundColor Green
    }
}

Write-Host ""
Write-Host "================================"
Write-Host "Results: $passed passed, $failed failed"
Write-Host "================================"

if ($failedTests.Count -gt 0) {
    Write-Host "Failed tests:"
    $failedTests | ForEach-Object { Write-Host "  - $_" }
}
