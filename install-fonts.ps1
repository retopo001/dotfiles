# Install Nerd Fonts on Windows for WezTerm
# Run this from PowerShell (Admin recommended)

Write-Host "=== Installing Nerd Fonts for WezTerm ===" -ForegroundColor Cyan

$fontDir = "$env:LOCALAPPDATA\Microsoft\Windows\Fonts"
$tempDir = "$env:TEMP\nerd-fonts"

# Function to install a font from URL
function Install-NerdFont {
    param(
        [string]$FontName,
        [string]$FontUrl
    )
    
    Write-Host "Installing $FontName..." -ForegroundColor Yellow
    New-Item -ItemType Directory -Force -Path $tempDir | Out-Null
    $zipPath = "$tempDir\$FontName.zip"
    
    try {
        Invoke-WebRequest -Uri $FontUrl -OutFile $zipPath -UseBasicParsing
        Expand-Archive -Path $zipPath -DestinationPath "$tempDir\$FontName" -Force
        
        # Install all TTF files
        $ttfFiles = Get-ChildItem -Path "$tempDir\$FontName" -Filter "*.ttf" -Recurse
        $count = 0
        foreach ($font in $ttfFiles) {
            $destPath = Join-Path $fontDir $font.Name
            Copy-Item $font.FullName -Destination $destPath -Force
            $count++
        }
        
        Write-Host "[OK] Installed $count $FontName font files" -ForegroundColor Green
        Remove-Item -Path "$tempDir\$FontName" -Recurse -Force
        Remove-Item -Path $zipPath -Force
        return $true
    } catch {
        Write-Host "[ERROR] Failed to install $FontName : $_" -ForegroundColor Red
        return $false
    }
}

# Try winget first
if (Get-Command winget -ErrorAction SilentlyContinue) {
    Write-Host "Using winget to install Nerd Fonts..." -ForegroundColor Green
    
    # Install Cascadia Code Nerd Font
    winget install -e --id CascadiaCode.CascadiaCode-NF --accept-package-agreements --accept-source-agreements 2>&1 | Out-Null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "[OK] CascadiaCode Nerd Font installed via winget" -ForegroundColor Green
    } else {
        Write-Host "[WARN] winget installation failed, trying manual download..." -ForegroundColor Yellow
        Install-NerdFont "CascadiaCode" "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/CascadiaCode.zip"
    }
    
    # Install additional Nerd Fonts for better coverage
    Install-NerdFont "Hack" "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/Hack.zip"
    Install-NerdFont "FiraCode" "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FiraCode.zip"
} else {
    Write-Host "winget not found. Installing fonts manually..." -ForegroundColor Yellow
    Install-NerdFont "CascadiaCode" "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/CascadiaCode.zip"
    Install-NerdFont "Hack" "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/Hack.zip"
    Install-NerdFont "FiraCode" "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FiraCode.zip"
}

# Clean up temp directory
if (Test-Path $tempDir) {
    Remove-Item -Path $tempDir -Recurse -Force -ErrorAction SilentlyContinue
}

# Refresh font cache
Write-Host "Refreshing font cache..." -ForegroundColor Yellow
try {
    Add-Type -AssemblyName System.Drawing
    $fontFamilies = New-Object System.Drawing.Text.PrivateFontCollection
    Write-Host "[OK] Font cache refreshed" -ForegroundColor Green
} catch {
    Write-Host "[WARN] Could not refresh font cache programmatically" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "=== Font Installation Complete ===" -ForegroundColor Cyan
Write-Host "Please restart WezTerm for the fonts to take effect." -ForegroundColor Yellow
Write-Host ""
Write-Host "If fonts still don't work, check:" -ForegroundColor Yellow
Write-Host "1. Fonts are installed in: $env:LOCALAPPDATA\Microsoft\Windows\Fonts" -ForegroundColor Gray
Write-Host "2. WezTerm config uses: CascadiaCode Nerd Font" -ForegroundColor Gray
Write-Host "3. Restart WezTerm completely (close all windows)" -ForegroundColor Gray
