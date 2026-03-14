{
  Copyright: 2026 - 2026 ENCRYPTED SUPPORT LLC <adrelanos@whonix.org>
  See the file COPYING for copying conditions.
}

program WhonixStarter;

{$mode objfpc}{$H+}
{$ZeroBasedStrings On}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, whonixstarter_main,
  { you can add units after this }
  Dialogs, Dos, sysutils, Types, Classes, WhonixUtils, LCLType, LCLIntF,
  Graphics;

{$R *.res}

var
  VBoxManagePath: String;
  VirtualBoxPath: String;
  VBoxVMListStr: String;
  VBoxVMListArr: TStringDynArray;
  VBoxVMLine: String;
  WhonixVMNameMaybe: String;
  WhonixWorkstationVMFound: Boolean = False;
  WhonixGatewayVMFound: Boolean = False;
  WhonixVMNameList: TStringList;
  CurrentWhonixVMName: String;
  WhonixWorkstationLXQtList: TStringList;
  WhonixWorkstationXfceList: TStringList;
  WhonixWorkstationCLIList: TStringList;
  WhonixWorkstationOtherList: TStringList;
  WhonixGatewayLXQtList: TStringList;
  WhonixGatewayXfceList: TStringList;
  WhonixGatewayCLIList: TStringList;
  WhonixGatewayOtherList: TStringList;
  WhonixWorkstationChosenVM: String;
  WhonixGatewayChosenVM: String;
  MsgReply: Integer;
  {$ifdef windows}
  PowerShellPath: String;
  WinHypervisorDetectStr: String;
  WinHypervisorDetectList: TStringDynArray;
  {$else}
  VBoxDrvPath: String;
  {$endif}

begin
  RequireDerivedFormResource := True;
  Application.Title:='Whonix-Starter';
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}
  Application.Initialize;

  {
    Ensure VirtualBox exists at the expected path. If it does not, the user most
    likely knows what they are doing and doesn't need Whonix-Starter at all.
    Either that or else they unintentionally installed VirtualBox to the wrong
    path, in which case they should probably reinstall it.

    We intentionally do NOT search for VirtualBox ourselves, since that risks
    executing anything that looks like VirtualBox on the disk. That could lead
    to unexpected execution of malicious code.
  }

  {$ifdef windows}
  VBoxManagePath := GetEnv('SYSTEMDRIVE')
    + '\Program Files\Oracle\VirtualBox\VBoxManage.exe';
  VirtualBoxPath := GetEnv('SYSTEMDRIVE')
    + '\Program Files\Oracle\VirtualBox\VirtualBox.exe';
  PowerShellPath := GetEnv('SYSTEMDRIVE')
    + '\Windows\System32\WindowsPowerShell\v1.0\powershell.exe';
  {$else}
  VBoxManagePath := '/usr/bin/VBoxManage';
  VirtualBoxPath := '/usr/bin/VirtualBox';
  VBoxDrvPath := '/dev/vboxdrv';
  {$endif}

  if not FileExists(VBoxManagePath) then
  begin
    MsgReply := Application.MessageBox(PChar('The VBoxManage executable was '
      + 'not found at ''' + VBoxManagePath + '''. Double-check that VirtualBox '
      + 'is installed properly.' + sLineBreak
      + sLineBreak
      + 'View VirtualBox troubleshooting steps in a web browser?'),
      'Whonix-Starter', MB_YESNO);
    if MsgReply = IDYES then OpenURL(
      'https://www.kicksecure.com/wiki/VirtualBox/Troubleshooting');
    Halt(1);
  end;

  if not FileExists(VirtualBoxPath) then
  begin
    MsgReply := Application.MessageBox(PChar('The VirtualBox executable was '
      + 'not found at ''' + VirtualBoxPath + '''. Double-check that VirtualBox '
      + 'is installed properly.' + sLineBreak
      + sLineBreak
      + 'View VirtualBox troubleshooting steps in a web browser?'),
      'Whonix-Starter', MB_YESNO);
    if MsgReply = IDYES then OpenURL(
      'https://www.kicksecure.com/wiki/VirtualBox/Troubleshooting');
    Halt(1);
  end;

  {
    On Linux, detect if /dev/vboxdrv exists. If it doesn't, the needed kernel
    modules for VirtualBox to work aren't present (this may happen if someone
    tries to use Whonix-Starter under Qubes OS, or if Secure Boot is
    misconfigured).
  }

  {$ifndef windows}
  if not FileExists(VBoxDrvPath) then
  begin
    {
      TODO: Should we go so far as to detect a VirtualBox misconfiguration?
      systemcheck would usually do this for us to some degree, but we probably
      won't have systemcheck available here.
    }
    MsgReply := Application.MessageBox(PChar('VirtualBox is not properly set '
      + 'up! In technical terms, the file ''' + VBoxDrvPath + ''' does not '
      + 'exist, meaning the vboxdrv kernel module is not loaded. If Secure '
      + 'Boot is enabled on this system, this may mean that VirtualBox module '
      + 'signing is broken. Double-check that VirtualBox is installed properly.'
      + sLineBreak
      + sLineBreak
      + 'View VirtualBox troubleshooting steps in a web browser?'),
      'Whonix-Starter', MB_YESNO);
    if MsgReply = IDYES then OpenURL(
      'https://www.kicksecure.com/wiki/VirtualBox/Troubleshooting');
    Halt(1);
  end;
  {$endif}

  {
    Get a list of all Whonix-Workstation and Whonix-Gateway VMs available, and
    ensure at least one workstation and one gateway exist.

    The output of 'VBoxManage list vms' is a bit tricky to parse - one VM is
    listed per line, the VM name is enclosed in double quotes, and the UUID is
    placed in braces after the name, like '"Whonix-Workstation-LXQt" {...}'.
    However, quotes and spaces are both allowed in VirtualBox VM names, and they
    are not escaped, so we have to split the line on the *last* space, then
    strip the leading and trailing quote from the first element of the resulting
    array.
  }

  if not RunCaptureCommand(VBoxManagePath, ['list', 'vms'], VBoxVMListStr) then
  begin
    MsgReply := Application.MessageBox(PChar('Failed to get a list of VMs from '
      + 'VirtualBox! In technical terms, the command ''VBoxManage list vms'' '
      + 'failed. Double-check that VirtualBox is installed properly.'
      + sLineBreak
      + sLineBreak
      + '''VBoxManage'' output the following message:' + sLineBreak
      + VBoxVMListStr + sLineBreak
      + sLineBreak
      + 'View VirtualBox troubleshooting steps in a web browser?'),
      'Whonix-Starter', MB_YESNO);
    if MsgReply = IDYES then OpenURL(
      'https://www.kicksecure.com/wiki/VirtualBox/Troubleshooting');
    Halt(1);
  end;

  VBoxVMListArr := VBoxVMListStr.Split([LineEnding],
    TStringSplitOptions.ExcludeLastEmpty);
  WhonixVMNameList := TStringList.Create;
  for VBoxVMLine in VBoxVMListArr do
  begin
    WhonixVMNameMaybe := VMNameFromLine(VBoxVMLine);
    if WhonixVMNameMaybe = '' then continue;
    if WhonixVMNameMaybe.StartsWith('Whonix-Workstation-') then
    begin
      WhonixWorkstationVMFound := True;
      WhonixVMNameList.Add(WhonixVMNameMaybe);
    end;
    if WhonixVMNameMaybe.StartsWith('Whonix-Gateway-') then
    begin
      WhonixGatewayVMFound := True;
      WhonixVMNameList.Add(WhonixVMNameMaybe);
    end;
  end;

  if not WhonixGatewayVMFound then
  begin
    MsgReply := Application.MessageBox('No Whonix-Gateway virtual machine '
      + 'is installed! View the Whonix VirtualBox installation steps in a web '
      + 'browser?', 'Whonix-Starter', MB_YESNO);
    if MsgReply = IDYES then OpenURL(
      'https://www.whonix.org/wiki/VirtualBox');
    Halt(1);
  end;

  if not WhonixWorkstationVMFound then
  begin
    MsgReply := Application.MessageBox('No Whonix-Workstation virtual '
      + 'machine is installed! View the Whonix VirtualBox installation steps '
      + 'in a web browser?', 'Whonix-Starter', MB_YESNO);
    if MsgReply = IDYES then OpenURL(
      'https://www.whonix.org/wiki/VirtualBox');
    Halt(1);
  end;

  {
    Heuristically detect which Workstation and Gateway to launch.

    For Workstations, LXQt is preferable to Xfce is preferable to CLI is
    preferable to anything else.

    For Gateways, LXQt is preferable to CLI is preferable to Xfce is preferable
    to anything else.

    If more than one VM of a particular type is found, prefer the one that sorts
    highest alphabetically.
  }

  WhonixWorkstationLXQtList := TStringList.Create;
  WhonixWorkstationLXQtList.Sorted := True;
  WhonixWorkstationXfceList := TStringList.Create;
  WhonixWorkstationXfceList.Sorted := True;
  WhonixWorkstationCLIList := TStringList.Create;
  WhonixWorkstationCLIList.Sorted := True;
  WhonixWorkstationOtherList := TStringList.Create;
  WhonixWorkstationOtherList.Sorted := True;
  WhonixGatewayLXQtList := TStringList.Create;
  WhonixGatewayLXQtList.Sorted := True;
  WhonixGatewayXfceList := TStringList.Create;
  WhonixGatewayXfceList.Sorted := True;
  WhonixGatewayCLIList := TStringList.Create;
  WhonixGatewayCLIList.Sorted := True;
  WhonixGatewayOtherList := TStringList.Create;
  WhonixGatewayOtherList.Sorted := True;

  for CurrentWhonixVMName in WhonixVMNameList do
  begin
    if CurrentWhonixVMName.StartsWith('Whonix-Workstation-LXQt') then
    begin
      WhonixWorkstationLXQtList.Add(CurrentWhonixVMName);
    end
    else if CurrentWhonixVMName.StartsWith('Whonix-Workstation-Xfce') then
    begin
      WhonixWorkstationXfceList.Add(CurrentWhonixVMName);
    end
    else if CurrentWhonixVMName.StartsWith('Whonix-Workstation-CLI') then
    begin
      WhonixWorkstationCLIList.Add(CurrentWhonixVMName);
    end
    else if CurrentWhonixVMName.StartsWith('Whonix-Workstation-') then
    begin
      WhonixWorkstationOtherList.Add(CurrentWhonixVMName);
    end
    else if CurrentWhonixVMName.StartsWith('Whonix-Gateway-LXQt') then
    begin
      WhonixGatewayLXQtList.Add(CurrentWhonixVMName);
    end
    else if CurrentWhonixVMName.StartsWith('Whonix-Gateway-Xfce') then
    begin
      WhonixGatewayXfceList.Add(CurrentWhonixVMName);
    end
    else if CurrentWhonixVMName.StartsWith('Whonix-Gateway-CLI') then
    begin
      WhonixGatewayCLIList.Add(CurrentWhonixVMName);
    end
    else { CurrentWhonixVMName.StartsWith('Whonix-Gateway-') }
    begin
      WhonixGatewayOtherList.Add(CurrentWhonixVMName);
    end;
  end;

  if WhonixWorkstationLXQtList.Count <> 0 then
  begin
    WhonixWorkstationChosenVM := WhonixWorkstationLXQtList[0];
  end
  else if WhonixWorkstationXfceList.Count <> 0 then
  begin
    WhonixWorkstationChosenVM := WhonixWorkstationXfceList[0];
  end
  else if WhonixWorkstationCLIList.Count <> 0 then
  begin
    WhonixWorkstationChosenVM := WhonixWorkstationCLIList[0];
  end
  else { WhonixWorkstationOtherList.Count <> 0 }
  begin
    WhonixWorkstationChosenVM := WhonixWorkstationOtherList[0];
  end;

  if WhonixGatewayLXQtList.Count <> 0 then
  begin
    WhonixGatewayChosenVM := WhonixGatewayLXQtList[0];
  end
  else if WhonixGatewayCLIList.Count <> 0 then
  begin
    WhonixGatewayChosenVM := WhonixGatewayCLIList[0];
  end
  else if WhonixGatewayXfceList.Count <> 0 then
  begin
    WhonixGatewayChosenVM := WhonixGatewayXfceList[0];
  end
  else { WhonixGatewayOtherList.Count <> 0 }
  begin
    WhonixGatewayChosenVM := WhonixGatewayOtherList[0];
  end;

  WhonixWorkstationLXQtList.Free;
  WhonixWorkstationXfceList.Free;
  WhonixWorkstationCLIList.Free;
  WhonixWorkstationOtherList.Free;
  WhonixGatewayLXQtList.Free;
  WhonixGatewayXfceList.Free;
  WhonixGatewayCLIList.Free;
  WhonixGatewayOtherList.Free;

  Application.CreateForm(TWhonixStarterForm, WhonixStarterForm);

  {
    On Windows, determine if we need to wait ten seconds between launching the
    Gateway and launching the Workstation. This is only necessary when Hyper-V
    is enabled, to prevent VM lockups.
  }

  {$ifdef windows}
  if not RunCaptureCommand(PowerShellPath, ['-Command',
    '(Get-CimInstance Win32_ComputerSystem).HypervisorPresent'],
    WinHypervisorDetectStr) then
  begin
    ShowMessage('Failed to check for the presence of Hyper-V! In technical '
      + 'terms, the command ''powershell -Command "(Get-CimInstance '
      + 'Win32_ComputerSystem).HypervisorPresent"'' failed.' + sLineBreak
      + sLineBreak
      + '''powershell'' output the following message:' + sLineBreak
      + WinHypervisorDetectStr);
    Halt(1);
  end;
  WinHypervisorDetectList := WinHypervisorDetectStr.Split([LineEnding],
    TStringSplitOptions.ExcludeLastEmpty);
  if Length(WinHypervisorDetectList) = 0 then
  begin
    {
      This should return 'True' or 'False', not an empty string. For now,
      treat an empty string the same as 'False'.
    }
    WhonixStarterForm.PauseAfterGatewayStart := False;
  end;
  WinHypervisorDetectStr := Trim(WinHypervisorDetectList[0]);
  if CompareText(WinHypervisorDetectStr, 'True') = 0 then
    WhonixStarterForm.PauseAfterGatewayStart := True
  else
    WhonixStarterForm.PauseAfterGatewayStart := False;
  {$else}
  WhonixStarterForm.PauseAfterGatewayStart := False;
  {$endif}

  if WhonixStarterForm.PauseAfterGatewayStart then
    WhonixStarterForm.InfoStatusBar.SimpleText := 'VMs will not run at full '
      + 'speed. Double-click for info.';

  WhonixStarterForm.VBoxManagePath := VBoxManagePath;
  WhonixStarterForm.VirtualBoxPath := VirtualBoxPath;
  WhonixStarterForm.WhonixVMNameList := WhonixVMNameList;
  WhonixStarterForm.TargetWhonixWorkstationVM := WhonixWorkstationChosenVM;
  WhonixStarterForm.TargetWhonixGatewayVM := WhonixGatewayChosenVM;
  Application.Run;
end.

