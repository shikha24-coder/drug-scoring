# =============================================================================
# setup_r.ps1  —  one-time setup for running the dashboard in VS Code (Windows)
# -----------------------------------------------------------------------------
# Run from the drug-scoring folder in the VS Code terminal:
#     powershell -ExecutionPolicy Bypass -File .\setup_r.ps1
#
# 1. Adds R to your user PATH (permanently, no admin rights needed)
# 2. Checks that Rscript works
# 3. Installs the R packages the app needs
# =============================================================================

$RBin = "C:\Users\shikha1.roy\AppData\Local\Programs\R\R-4.6.1\bin"

if (-not (Test-Path (Join-Path $RBin "Rscript.exe"))) {
    Write-Host "Rscript.exe was not found in $RBin" -ForegroundColor Red
    Write-Host "Check the R version folder name in C:\Users\shikha1.roy\AppData\Local\Programs\R and edit `$RBin at the top of this script."
    exit 1
}

# --- 1. Add to user PATH (only if it isn't there already) --------------------
$userPath = [Environment]::GetEnvironmentVariable("Path", "User")
if ($null -eq $userPath) { $userPath = "" }
$parts = $userPath.Split(";") | Where-Object { $_ -ne "" }

if ($parts -contains $RBin) {
    Write-Host "R is already on your PATH." -ForegroundColor Green
} else {
    $newPath = (@($parts) + $RBin) -join ";"
    [Environment]::SetEnvironmentVariable("Path", $newPath, "User")
    Write-Host "Added R to your PATH: $RBin" -ForegroundColor Green
}

# Make it work in this terminal straight away too
if (-not ($env:Path.Split(";") -contains $RBin)) { $env:Path += ";$RBin" }

# --- 2. Check Rscript --------------------------------------------------------
Write-Host ""
& Rscript --version
if ($LASTEXITCODE -ne 0) {
    Write-Host "Rscript did not run. Close VS Code, reopen it and run this script again." -ForegroundColor Red
    exit 1
}

# --- 3. Install packages -----------------------------------------------------
Write-Host ""
Write-Host "Installing R packages (first time takes a few minutes)..." -ForegroundColor Cyan
& Rscript -e "pk <- c('shiny','bslib','nanoparquet','plotly','DT'); miss <- pk[!sapply(pk, requireNamespace, quietly = TRUE)]; if (length(miss)) install.packages(miss, repos = 'https://cloud.r-project.org') else cat('All packages already installed.\n')"

Write-Host ""
Write-Host "Done. Restart VS Code once, then start the app with:  Rscript run_app.R  (or Ctrl+Shift+B)" -ForegroundColor Green
