Set-WindowsExplorerOptions -EnableShowHiddenFilesFoldersDrives -EnableShowProtectedOSFiles -EnableShowFileExtensions -EnableShowFullPathInTitleBar
Set-TaskbarOptions -Size Small -Lock -Dock Bottom

choco install microsoft-build-tools   -y
choco install MsSqlServer2012Express  -y
choco install visualstudio2013premium -y -InstallArguments "/Features:WebTools SQL"
choco install vs2013.4                -y
choco install netfx-4.5.2-devpack     -y
choco install tfs2013powertools       -y
choco install vs2013.vscommands       -y
choco install resharper               -y
choco install ncrunch2.vs2013         -y

(new-object -c shell.application).namespace("${env:SystemRoot}\system32\WindowsPowerShell\v1.0\").parsename("powershell.exe").invokeverb("taskbarpin")

Install-WindowsFeature FileAndStorage-Services
Install-WindowsFeature File-Services
Install-WindowsFeature Storage-Services
Install-WindowsFeature NET-Framework-Features
Install-WindowsFeature NET-Framework-Core
Install-WindowsFeature NET-Framework-45-Features
Install-WindowsFeature NET-Framework-45-Core
Install-WindowsFeature NET-Framework-45-ASPNET
Install-WindowsFeature NET-WCF-Services45
Install-WindowsFeature NET-WCF-Pipe-Activation45
Install-WindowsFeature NET-WCF-TCP-PortSharing45
Install-WindowsFeature FS-SMB1
Install-WindowsFeature User-Interfaces-Infra
Install-WindowsFeature Server-Gui-Mgmt-Infra
Install-WindowsFeature Server-Gui-Shell
Install-WindowsFeature Windows-Identity-Foundation
Install-WindowsFeature PowerShellRoot
Install-WindowsFeature PowerShell
Install-WindowsFeature PowerShell-V2
Install-WindowsFeature WAS
Install-WindowsFeature WAS-Process-Model
Install-WindowsFeature WAS-Config-APIs
Install-WindowsFeature WoW64-Support

Install-WindowsUpdate -AcceptEula
