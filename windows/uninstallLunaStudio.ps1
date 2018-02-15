 $ErrorActionPreference = 'Continue'

 if(!$PSScriptRoot) {
     $PSScriptRoot = Split-Path $MyInvocation.MyCommand.Path -Parent
 }

 Write-Host "Uninstalling Luna services"
 Push-Location -Path "$($PSScriptRoot)\current\config\windows"
 & '.\uninstallAll.bat'
 Pop-Location

 Write-Host "Removing $env:APPDATA\LunaStudio*"
 Get-ChildItem -path "$env:APPDATA" -Filter "LunaStudio*" | Remove-Item -Force -Recurse

 $uncLunaStudio = "\\?\" + $PSScriptRoot
 Write-Host "Removing symlink $PSScriptRoot\current"
 $currentPath = Join-Path $PSScriptRoot -Child "current"
 [System.IO.Directory]::Delete($currentPath, $true)

 Write-Host "Removing $PSScriptRoot"
 Remove-Item -Force -Recurse $uncLunaStudio

 Write-Host "Removing $env:USERPROFILE\.luna"
 Remove-Item -Force -Recurse "$env:USERPROFILE\.luna"

 Write-Host "Removing Start Menu shortcut"
 Remove-Item -Force "$env:APPDATA\Microsoft\Windows\Start Menu\Programs\LunaStudio.lnk"

 Write-Host "Removing entries in Windows Registry"
 Remove-Item -Recurse -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\LunaStudio

 Read-Host -Prompt "Press Enter to exit"
