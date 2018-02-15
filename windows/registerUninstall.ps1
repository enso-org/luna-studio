$installationDirectory = $args[0]

$ErrorActionPreference = 'Continue'

New-Item -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\LunaStudio
New-ItemProperty -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\LunaStudio -Name DisplayName -PropertyType String -Value "Luna Studio"
$uninstallScriptPath = "$($installationDirectory)\LunaStudio\uninstallLunaStudio.ps1"
$uninstallString     = "$env:SYSTEMROOT\System32\WindowsPowerShell\v1.0\powershell.exe -executionpolicy bypass -file `"$uninstallScriptPath`""
New-ItemProperty -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\LunaStudio -Name UninstallString -PropertyType String -Value $uninstallString
New-ItemProperty -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\LunaStudio -Name DisplayIcon -PropertyType String -Value "$installationDirectory\LunaStudio\current\bin\public\luna-studio\luna-studio.exe"
New-ItemProperty -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\LunaStudio -Name NoModify -PropertyType DWord -Value 1
New-ItemProperty -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\LunaStudio -Name NoRepair -PropertyType DWord -Value 1
New-ItemProperty -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\LunaStudio -Name EstimatedSize -PropertyType DWord -Value 1048576
New-ItemProperty -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\LunaStudio -Name Publisher -PropertyType String -Value "New Byte Order"