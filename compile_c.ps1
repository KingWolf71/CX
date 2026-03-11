# compile_c.ps1 - Compile CX with PureBasic C-backend
# Works around two pbcompilerc bugs:
#   Bug 1: Generates '->' instead of '.' after structured array indexing
#   Bug 2: Expression truncation with deeply nested macro expansions (fixed in PB source)
#
# Usage: powershell -ExecutionPolicy Bypass -File compile_c.ps1 [-Console] [-GUI]

param(
    [switch]$Console,
    [switch]$GUI
)

$PBPath = "D:\WIP\APPS\PureBasic.610\Compilers"
$SrcDir = "D:\OneDrive\WIP\Sources\intense.2020\cx"
$Source = "c2-modules-V25.pb"

if ($GUI) {
    $ExeName = "cx-win.exe"
    $ModeFlags = "/OPTIMIZER /THREAD /DYNAMICCPU"
} else {
    $ExeName = "cx.exe"
    $ModeFlags = "/OPTIMIZER /THREAD /DYNAMICCPU /CONSOLE"
}

Push-Location $SrcDir

# Step 0: Clean up
Remove-Item -Force $ExeName -ErrorAction SilentlyContinue
Remove-Item -Force purebasic.c -ErrorAction SilentlyContinue

# Step 1: Generate C source (will fail at GCC stage, that's expected)
Write-Host "Step 1: Generating C source..." -ForegroundColor Cyan
$null = & "$PBPath\pbcompilerc" /COMMENTED $ModeFlags.Split(' ') $Source /EXE $ExeName 2>&1

if (-not (Test-Path purebasic.c)) {
    Write-Host "ERROR: purebasic.c was not generated!" -ForegroundColor Red
    Pop-Location
    exit 1
}

$lineCount = (Get-Content purebasic.c).Count
Write-Host "  Generated purebasic.c ($lineCount lines)" -ForegroundColor Gray

# Step 2: Fix Bug #1 using Python (handles balanced bracket matching)
Write-Host "Step 2: Fixing C-backend Bug #1 (-> vs . after struct array index)..." -ForegroundColor Cyan

$pythonScript = @'
import re, sys

with open('purebasic.c', 'r', encoding='utf-8') as f:
    content = f.read()

lines = content.split('\n')
fixes = 0

for i, line in enumerate(lines):
    if ']->' not in line:
        continue

    newline = list(line)
    # Find all ]-> positions (scan right to left to not shift indices)
    positions = [m.start() for m in re.finditer(r']->', line)]

    for pos in reversed(positions):
        # Find the matching [ for this ]
        depth = 0
        j = pos
        while j >= 0:
            if line[j] == ']':
                depth += 1
            elif line[j] == '[':
                depth -= 1
            if depth == 0:
                break
            j -= 1

        if j <= 0:
            continue

        # Check what's before the opening [
        # If it ends with f_var.a) or f_ar.a), the -> should be .
        # (may be preceded by . or -> depending on context)
        before = line[:j]
        if before.endswith('f_var.a)') or before.endswith('f_ar.a)'):
            # Replace -> with . at position pos+1
            newline[pos+1] = '.'   # ] was at pos, - at pos+1, > at pos+2
            newline[pos+2] = ''    # remove the >
            fixes += 1

    lines[i] = ''.join(newline)

with open('purebasic.c', 'w', encoding='utf-8') as f:
    f.write('\n'.join(lines))

print(f'{fixes} fixes applied')
'@

$pythonScript | python3
if ($LASTEXITCODE -ne 0) {
    Write-Host "  ERROR: Python fixup script failed!" -ForegroundColor Red
    Pop-Location
    exit 1
}

# Step 3: Check for remaining errors before compiling
Write-Host "Step 3: Checking for remaining errors..." -ForegroundColor Cyan
$gccPath = "$PBPath\gcc"
$env:PATH = "$gccPath;$env:PATH"
$errors = & "$gccPath\gcc.exe" -fmax-errors=0 -fsyntax-only "$SrcDir\purebasic.c" 2>&1 |
    Select-String "error:" | ForEach-Object { $_.ToString() }

if ($errors.Count -gt 0) {
    Write-Host "  WARNING: $($errors.Count) remaining GCC errors:" -ForegroundColor Yellow
    $errors | ForEach-Object { Write-Host "    $_" -ForegroundColor Yellow }
    Write-Host "  Attempting compilation anyway..." -ForegroundColor Yellow
}
else {
    Write-Host "  No errors found!" -ForegroundColor Green
}

# Step 4: Compile the fixed C with /REASM
Write-Host "Step 4: Compiling fixed C with /REASM..." -ForegroundColor Cyan
Remove-Item -Force $ExeName -ErrorAction SilentlyContinue
$output = & "$PBPath\pbcompilerc" /REASM $ModeFlags.Split(' ') $Source /EXE $ExeName 2>&1
$exitCode = $LASTEXITCODE

if ($exitCode -eq 0 -and (Test-Path $ExeName)) {
    $size = [math]::Round((Get-Item $ExeName).Length / 1MB, 1)
    Write-Host "SUCCESS: $ExeName ($size MB)" -ForegroundColor Green
}
else {
    Write-Host "FAILED: Compilation failed" -ForegroundColor Red
    Write-Host $output -ForegroundColor Red
}

# Cleanup (keep purebasic.c for debugging if compilation failed)
if ($exitCode -eq 0) {
    Remove-Item -Force purebasic.c -ErrorAction SilentlyContinue
}

Pop-Location
exit $exitCode
