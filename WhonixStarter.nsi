; Copyright (C) 2026 - 2026 ENCRYPTED SUPPORT LLC <adrelanos@whonix.org>
; See the file COPYING.txt for copying conditions.

Name "Whonix-Starter"
OutFile "Whonix-Starter-${Version}.exe"
RequestExecutionLevel admin
InstallDir $PROGRAMFILES64\Whonix-Starter
LicenseData "COPYING.txt"

Page license
Page components
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

Section "Whonix-Starter (required)"
  SectionIn RO

  SetShellVarContext all
  SetRegView 64

  SetOutPath $INSTDIR

  File "Whonix-Starter.exe"
  File "COPYING.txt"
  WriteUninstaller "$INSTDIR\uninstall.exe"

  CreateDirectory "$SMPROGRAMS\Whonix"
  CreateShortcut "$SMPROGRAMS\Whonix\Whonix-Starter.lnk" "$INSTDIR\Whonix-Starter.exe"
  CreateShortcut "$SMPROGRAMS\Whonix\Uninstall Whonix-Starter.lnk" "$INSTDIR\uninstall.exe"

  ; Register the uninstaller with Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Whonix-Starter" "DisplayName" "Whonix-Starter"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Whonix-Starter" "DisplayVersion" "${Version}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Whonix-Starter" "Publisher" "ENCRYPTED SUPPORT LLC"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Whonix-Starter" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Whonix-Starter" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Whonix-Starter" "NoRepair" 1
SectionEnd

Section "Uninstall"
  SetShellVarContext all
  SetRegView 64

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Whonix-Starter"
  Delete /REBOOTOK "$INSTDIR\Whonix-Starter.exe"
  Delete /REBOOTOK "$INSTDIR\COPYING.txt"
  Delete /REBOOTOK "$INSTDIR\uninstall.exe"
  Delete /REBOOTOK "$SMPROGRAMS\Whonix\Whonix-Starter.lnk"
  Delete /REBOOTOK "$SMPROGRAMS\Whonix\Uninstall Whonix-Starter.lnk"
  RMDir /REBOOTOK "$SMPROGRAMS\Whonix"
  RMDir /REBOOTOK "$INSTDIR"
SectionEnd

Function .onInit
  IfFileExists "$INSTDIR\Whonix-Starter.exe" 0 +3
    MessageBox MB_OK "Whonix-Starter is already installed."
    Abort
FunctionEnd
