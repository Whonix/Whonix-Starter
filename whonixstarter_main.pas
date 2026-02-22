{
  Copyright: 2026 - 2026 ENCRYPTED SUPPORT LLC <adrelanos@whonix.org>
  See the file COPYING for copying conditions.
}

unit whonixstarter_main;

{$mode objfpc}{$H+}
{$ZeroBasedStrings On}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  WhonixUtils, Types, Process, LCLType, ComCtrls, LMessages, LCLIntf, Buttons;

const
  { Whonix-Starter - Launch Thread Callback }
  WST_LTCALLBACK = LM_USER + 1;

  { Launch Thread Callback - Gateway Launch Start }
  LTCALLBACK_GWLAUNCH_START = 0;
  { Launch Thread Callback - Gateway Launch Failure }
  LTCALLBACK_GWLAUNCH_FAIL = 1;
  { Launch Thread Callback - Gateway Launch Pause }
  LTCALLBACK_GWLAUNCH_PAUSE = 2;
  { Launch Thread Callback - Workstation Launch Start }
  LTCALLBACK_WSLAUNCH_START = 3;
  { Launch Thread Callback - Workstation Launch Failure }
  LTCALLBACK_WSLAUNCH_FAIL = 4;
  { Launch Thread Callback - Launch Done }
  LTCALLBACK_LAUNCH_DONE = 5;
  { Launch Thread Callback - Launch Not Needed }
  LTCALLBACK_LAUNCH_NONEED = 6;

type

  { TWhonixStarterForm }

  TWhonixStarterForm = class(TForm)
    StartWhonixButton: TButton;
    InfoStatusBar: TStatusBar;
    StopWhonixButton: TButton;
    LaunchVBoxButton: TButton;
    ActionGroupBox: TGroupBox;
    StartStopGroupPanel: TPanel;
    RunningVMNameList: TStringList;
    procedure InfoStatusBarDblClick(Sender: TObject);
    procedure LaunchVBoxButtonClick(Sender: TObject);
    procedure PopulateRunningVMNameList();
    procedure StartWhonixButtonClick(Sender: TObject);
    procedure StopWhonixButtonClick(Sender: TObject);
    procedure LaunchThreadCallback(var Msg: TLMessage); message WST_LTCALLBACK;
  private

  public
    { This variable is set by TVMLaunchThread when necessary. }
    VBoxError: String;
    { These variables are set by WhonixStarter.lpr. }
    VBoxManagePath: String;
    VirtualBoxPath: String;
    WhonixVMNameList: TStringList;
    TargetWhonixWorkstationVM: String;
    TargetWhonixGatewayVM: String;
    PauseAfterGatewayStart: Boolean;
  end;

  { TVMLaunchThread }

  TVMLaunchThread = class(TThread)
    protected
      procedure Execute; override;
    public
      VBoxManagePath: String;
      RunningVMNameList: TStringList;
      TargetWhonixWorkstationVM: String;
      TargetWhonixGatewayVM: String;
      PauseAfterGatewayStart: Boolean;
      constructor Create(CreateSuspended: boolean);
    end;

var
  WhonixStarterForm: TWhonixStarterForm;
  RunningVMNameListSet: Boolean = False;

implementation

{$R *.lfm}

{ TWhonixStarterForm }

procedure TWhonixStarterForm.PopulateRunningVMNameList();
var
  VBoxVMListStr: String;
  VBoxVMListArr: TStringDynArray;
  VBoxVMLine: String;
  VBoxVMNameMaybe: String;
begin
  if not RunCaptureCommand(Self.VBoxManagePath, ['list', 'runningvms'],
    VBoxVMListStr) then
  begin
    ShowMessage('Failed to get a list of running VMs from VirtualBox! In '
      + 'technical terms, the command ''VBoxManage list runningvms'' failed. '
      + 'Please report this bug!');
    Halt(1);
  end;
  VBoxVMListArr := VBoxVMListStr.Split([LineEnding],
    TStringSplitOptions.ExcludeLastEmpty);

  if RunningVMNameListSet then Self.RunningVMNameList.Free;
  Self.RunningVMNameList := TStringList.Create;
  for VBoxVMLine in VBoxVMListArr do
  begin
    VBoxVMNameMaybe := VMNameFromLine(VBoxVMLine);
    if VBoxVMNameMaybe = '' then continue;
    Self.RunningVMNameList.Add(VBoxVMNameMaybe);
  end;
end;

procedure TWhonixStarterForm.LaunchVBoxButtonClick(Sender: TObject);
var
  BackgroundProcess: TProcess;
begin
  BackgroundProcess := TProcess.Create(nil);
  BackgroundProcess.Executable := Self.VirtualBoxPath;
  BackgroundProcess.Execute;
  Self.InfoStatusBar.SimpleText := 'VirtualBox manager started.';
  BackgroundProcess.Free;
end;

procedure TWhonixStarterForm.InfoStatusBarDblClick(Sender: TObject);
var
  MsgReply: Integer;
begin
  if Self.PauseAfterGatewayStart then
  begin
    MsgReply := Application.MessageBox('Hyper-V is running on this system. '
      + 'VirtualBox will run in "green turtle mode", and its speed and '
      + 'stability will be impacted.' + sLineBreak
      + sLineBreak
      + 'View information about VirtualBox''s "green turtle mode" in a web '
      + 'browser?', 'Whonix-Starter', MB_YESNO);
    if MsgReply = IDYES then OpenURL(
      'https://www.kicksecure.com/wiki/VirtualBox/Green_Turtle_Issue');
  end
  else
  begin
    ShowMessage('No conflicting virtualizer is present on this system. '
      + 'VirtualBox will be able to run at full speed.' + sLineBreak
      + sLineBreak
      + 'Note that if you use other virtualizers as well, you will need to '
      + 'power off all VMs in those virtualizers before you can start Whonix '
      + 'with VirtualBox.');
  end;
end;

procedure TWhonixStarterForm.StartWhonixButtonClick(Sender: TObject);
var
  WhonixLaunchThread: TVMLaunchThread;
begin
  PopulateRunningVMNameList;
  Self.StartWhonixButton.Enabled := False;
  WhonixLaunchThread := TVMLaunchThread.Create(False);
  with WhonixLaunchThread do
  begin
    VBoxManagePath := Self.VBoxManagePath;
    RunningVMNameList := Self.RunningVMNameList;
    TargetWhonixWorkstationVM := Self.TargetWhonixWorkstationVM;
    TargetWhonixGatewayVM := Self.TargetWhonixGatewayVM;
    PauseAfterGatewayStart := Self.PauseAfterGatewayStart;
  end;
  WhonixLaunchThread.Start;
end;

procedure TWhonixStarterForm.StopWhonixButtonClick(Sender: TObject);
var
  MsgReply: Integer;
  VBoxRunningVMName: String;
  Garbage: String;
begin
  PopulateRunningVMNameList;
  if Self.RunningVMNameList.Count = 0 then
  begin
    Self.InfoStatusBar.SimpleText := 'No Whonix VMs are running.';
    Exit;
  end;

  MsgReply := Application.MessageBox('WARNING: You are about to forcibly power '
    + 'off all running Whonix-Gateway and Whonix-Workstation VirtualBox VMs. '
    + 'All unsaved work will be lost, and filesystem corruption may result. '
    + 'Are you sure you want to do this?', 'Whonix-Starter', MB_YESNO);
  if MsgReply <> IDYES then Exit;

  for VBoxRunningVMName in Self.RunningVMNameList do
    if Self.WhonixVMNameList.IndexOf(VBoxRunningVMName) <> -1 then
      RunCaptureCommand(Self.VBoxManagePath, ['controlvm', VBoxRunningVMName,
        'poweroff'], Garbage);
  Self.InfoStatusBar.SimpleText := 'Whonix VMs stopped.';
end;

procedure TWhonixStarterForm.LaunchThreadCallback(var Msg: TLMessage);
var
  MsgReply: Integer;
begin
  case Msg.wParam of
    LTCALLBACK_GWLAUNCH_START:
      begin
        Self.InfoStatusBar.SimpleText := 'Starting Whonix-Gateway...';
      end;
    LTCALLBACK_GWLAUNCH_FAIL:
      begin
        Self.InfoStatusBar.SimpleText := 'Failed to start the Whonix-Gateway!';
        Self.StartWhonixButton.Enabled := True;
        MsgReply := Application.MessageBox(PChar('Failed to start the '
          + 'Whonix-Gateway virtual machine! In technical terms, the command '
          + '''VBoxManage startvm ' + TargetWhonixGatewayVM + ''' failed.'
          + sLineBreak
          + sLineBreak
          + '''VBoxManage'' output the following message:' + sLineBreak
          + Self.VBoxError + sLineBreak
          + sLineBreak
          + 'View VirtualBox troubleshooting steps in a web browser?'),
          'Whonix-Starter', MB_YESNO);
        if MsgReply = IDYES then OpenURL(
          'https://www.kicksecure.com/wiki/VirtualBox/Troubleshooting');
      end;
    LTCALLBACK_GWLAUNCH_PAUSE:
      begin
        Self.InfoStatusBar.SimpleText := 'Pausing 30 seconds after '
          + 'starting Gateway...';
      end;
    LTCALLBACK_WSLAUNCH_START:
      begin
        Self.InfoStatusBar.SimpleText := 'Starting Whonix-Workstation...';
      end;
    LTCALLBACK_WSLAUNCH_FAIL:
      begin
        Self.InfoStatusBar.SimpleText := 'Failed to start the '
          + 'Whonix-Workstation!';
        Self.StartWhonixButton.Enabled := True;
        MsgReply := Application.MessageBox(PChar('Failed to start the '
          + 'Whonix-Workstation virtual machine! In technical terms, the '
          + 'command ''VBoxManage startvm' + TargetWhonixWorkstationVM + ''' '
          + 'failed.' + sLineBreak
          + sLineBreak
          + '''VBoxManage'' output the following message:' + sLineBreak
          + Self.VBoxError + sLineBreak
          + sLineBreak
          + 'View VirtualBox troubleshooting steps in a web browser?'),
          'Whonix-Starter', MB_YESNO);
        if MsgReply = IDYES then OpenURL(
          'https://www.kicksecure.com/wiki/VirtualBox/Troubleshooting');
      end;
    LTCALLBACK_LAUNCH_DONE:
      begin
        Self.InfoStatusBar.SimpleText := 'Whonix VMs started.';
        Self.StartWhonixButton.Enabled := True;
      end;
    LTCALLBACK_LAUNCH_NONEED:
      begin
        Self.InfoStatusBar.SimpleText := 'Whonix VMs are already running.';
        Self.StartWhonixButton.Enabled := True;
      end;
  else
    ShowMessage('Unrecognized message parameter ' + IntToStr(Msg.wParam) + '!');
    Self.StartWhonixButton.Enabled := True;
  end;
end;

{ TVMLaunchThread }

constructor TVMLaunchThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TVMLaunchThread.Execute;
var
  VBoxOutput: String;
  GWNeedsStart: Boolean;
  WSNeedsStart: Boolean;
begin
  if Self.RunningVMNameList.IndexOf(TargetWhonixGatewayVM) = -1 then
    GWNeedsStart := True;
  if Self.RunningVMNameList.IndexOf(TargetWhonixWorkstationVM) = -1 then
    WSNeedsStart := True;

  if GWNeedsStart then
  begin
    SendMessage(WhonixStarterForm.Handle, WST_LTCALLBACK,
      LTCALLBACK_GWLAUNCH_START, 0);
    if not RunCaptureCommand(Self.VBoxManagePath, ['startvm',
      TargetWhonixGatewayVM], VBoxOutput) then
    begin
      WhonixStarterForm.VBoxError := VBoxOutput;
      SendMessage(WhonixStarterForm.Handle, WST_LTCALLBACK,
        LTCALLBACK_GWLAUNCH_FAIL, 0);
      Exit;
    end;

    if WSNeedsStart and Self.PauseAfterGatewayStart then
    begin
      SendMessage(WhonixStarterForm.Handle, WST_LTCALLBACK,
        LTCALLBACK_GWLAUNCH_PAUSE, 0);
      Sleep(30000);
    end;
  end;

  if WSNeedsStart then
  begin
    SendMessage(WhonixStarterForm.Handle, WST_LTCALLBACK,
      LTCALLBACK_WSLAUNCH_START, 0);
    if not RunCaptureCommand(Self.VBoxManagePath, ['startvm',
      TargetWhonixWorkstationVM], VBoxOutput) then
    begin
      WhonixStarterForm.VBoxError := VBoxOutput;
      SendMessage(WhonixStarterForm.Handle, WST_LTCALLBACK,
        LTCALLBACK_WSLAUNCH_FAIL, 0);
      Exit;
    end;
  end;

  if GWNeedsStart or WSNeedsStart then
  begin
    SendMessage(WhonixStarterForm.Handle, WST_LTCALLBACK,
      LTCALLBACK_LAUNCH_DONE, 0);
  end
  else
  begin
    SendMessage(WhonixStarterForm.Handle, WST_LTCALLBACK,
      LTCALLBACK_LAUNCH_NONEED, 0);
  end;
end;

end.

