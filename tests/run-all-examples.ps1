# Test all CX examples
$ErrorActionPreference = "SilentlyContinue"
$cx = ".\cx.exe"
$passed = 0
$failed = 0
$failedTests = @()

Get-ChildItem "Examples\*.cx" | Sort-Object Name | ForEach-Object {
    $name = $_.Name
    $output = & $cx -t $_.FullName 2>&1 | Out-String

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
