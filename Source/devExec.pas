{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit devExec;

interface

uses
  Windows, Classes, Forms, devcfg, utils;

type
  TPipeInputThread = class(TThread)
  private
    procedure PipeInput;
  public
    WriteHandle : THandle;
    InputFile: string;
    procedure Execute; override;
  end;

  TExecThread = class(TThread)
  private
    fFile: AnsiString;
    fPath: AnsiString;
    fParams: AnsiString;
    fTimeOut: Cardinal;
    fProcess: Cardinal;
    fVisible: boolean;
    procedure ExecAndWait;
  public
    InputWrite : THandle;
    InputRead : THandle;
    StartupEvent: THandle;
    RedirectInput : boolean;
    procedure Execute; override;
  published
    property FileName: AnsiString read fFile write fFile;
    property Path: AnsiString read fPath write fPath;
    property Params: AnsiString read fParams write fParams;
    property TimeOut: Cardinal read fTimeOut write fTimeOut;
    property Visible: boolean read fVisible write fVisible;
    property Process: Cardinal read fProcess;
  end;

  TdevExecutor = class(TPersistent)
  private
    fExec: TExecThread;
    fPipe: TPipeInputThread;
    fIsRunning: boolean;
    fOnTermEvent: TNotifyEvent;
    procedure TerminateEvent(Sender: TObject);
    constructor Create;
  public
    destructor Destroy; override;
    procedure Reset;
    procedure ExecuteAndWatch(sFileName, sParams, sPath: AnsiString; bVisible: boolean;
      bRedirectInput:boolean; InputFile: string; iTimeOut: Cardinal; OnTermEvent: TNotifyEvent);
  published
    property Running: boolean read fIsRunning;
  end;

function devExecutor: TdevExecutor;

implementation

uses
  main,sysutils,dialogs;

const
  Pipename:String = '\\.\pipe\devcpp_run';

{ TExecThread }

procedure TExecThread.Execute;
begin
  inherited;
  ExecAndWait;
end;

procedure TExecThread.ExecAndWait;
const
  BUFSIZE:integer = 4096;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  sa: TSecurityAttributes;
  params: String;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    if fVisible then
      wShowWindow := SW_SHOW
    else
      wShowWindow := SW_HIDE;
  end;
  if RedirectInput then begin
    // Set up the security attributes struct.
    sa.nLength := sizeof(TSecurityAttributes);
    sa.lpSecurityDescriptor := nil;
    sa.bInheritHandle := true;

    InputWrite := CreateNamedPipe(
          pAnsiChar(Pipename),             // pipe name
          PIPE_ACCESS_OUTBOUND,       // read/write access
          PIPE_TYPE_MESSAGE or       // message type pipe
          PIPE_READMODE_MESSAGE or   // message-read mode
          PIPE_WAIT,                // blocking mode
          PIPE_UNLIMITED_INSTANCES, // max. instances
          BUFSIZE,                  // output buffer size
          BUFSIZE,                  // input buffer size
          0,                        // client time-out
          nil);                    // default security attribute
    if InputWrite = INVALID_HANDLE_VALUE then begin
      LogError('devExec.pas TExecThread.ExecAndWait',Format('Create named pipe failed: %s',[SysErrorMessage(GetLastError)]));
      Exit;
    end;
    InputRead := CreateFile(
         PAnsiChar(Pipename),   // pipe name
         GENERIC_READ,
         0,              // no sharing
         @sa,           // default security attributes
         OPEN_EXISTING,  // opens existing pipe
         0,              // default attributes
         0);
    if InputRead = INVALID_HANDLE_VALUE then begin
      LogError('Debugger.pas TExecThread.ExecAndWait',Format('Create File on Input Pipe Failed: %s',[SysErrorMessage(GetLastError)]));
      Exit;
    end;
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
    StartupInfo.hStdInput := InputRead;
    StartupInfo.hStdOutput := 7; // Undocumented Handle of Console Output
    StartupInfo.hStdError := 11; // Undocumented Handle of Console Error Output
  end;

  if RedirectInput then
    params := '1 '+fParams
  else
    params := '0 '+fParams;
  if CreateProcess(nil, PAnsiChar('"' + fFile + '" ' + params), nil, nil, True,
    NORMAL_PRIORITY_CLASS , nil,
    PAnsiChar(fPath), StartupInfo, ProcessInfo) then begin
    fProcess := ProcessInfo.hProcess;
    SetEvent(StartupEvent);
    WaitForSingleObject(ProcessInfo.hProcess, fTimeOut);
  end else begin
    LogError('devExec.pas TExecThread.ExecAndWait',Format('Create named pipe failed: %s',[SysErrorMessage(GetLastError)]));
    SetEvent(StartupEvent);
  end;

  if RedirectInput then
    CloseHandle(InputRead);
  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
end;

var
  fDevExecutorSingleton: TdevExecutor;

function devExecutor: TdevExecutor;
begin
  if not Assigned(fDevExecutorSingleton) and not Application.Terminated then
    fDevExecutorSingleton := TdevExecutor.Create;
  Result := fDevExecutorSingleton;
end;

{ TdevExecutor }

constructor TdevExecutor.Create;
begin
  inherited;
end;

destructor TdevExecutor.Destroy;
begin
  fDevExecutorSingleton := nil;
  inherited;
end;

procedure TdevExecutor.ExecuteAndWatch(sFileName, sParams, sPath: AnsiString;
  bVisible: boolean; bRedirectInput:boolean; InputFile: string;
  iTimeOut: Cardinal; OnTermEvent: TNotifyEvent);
begin
  fIsRunning := True;
  fOnTermEvent := OnTermEvent;

  fExec := TExecThread.Create(True);
  with fExec do begin
    FileName := sFileName;
    Params := sParams;
    Path := sPath;
    TimeOut := iTimeOut;
    Visible := bVisible;
    OnTerminate := TerminateEvent;
    FreeOnTerminate := True;
    RedirectInput := bRedirectInput;
    StartupEvent := CreateEvent(nil,False,False,nil);
    Resume;
  end;
  if bRedirectInput then begin
    WaitForSingleObject(fExec.StartupEvent,INFINITE);
    fPipe := TPipeInputThread.Create(True);
    fPipe.WriteHandle := fExec.InputWrite;
    fPipe.InputFile := InputFile;
    fPipe.Execute;
  end;
end;

procedure TdevExecutor.Reset;
begin
  if Assigned(fExec) and fIsRunning then
    TerminateProcess(fExec.Process, 0);
  fIsRunning := False;
end;

procedure TdevExecutor.TerminateEvent(Sender: TObject);
begin
  fIsRunning := False;
  if Assigned(fOnTermEvent) then
    fOnTermEvent(Self);
end;

{ TPipeInputThread }

procedure TPipeInputThread.PipeInput;
const
  BufSize = 4096;
var
  buffer: pAnsichar;
  FileHandle : THandle;
  bytesRead: cardinal;
  bytesWritten: cardinal;
begin
  FileHandle := CreateFile(pAnsiChar(InputFile),GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);

  if FileHandle = INVALID_HANDLE_VALUE then begin
    LogError('devExec.pas TPipeInputThread.PipeInput',Format('Open InputFile Failed: %s',[SysErrorMessage(GetLastError)]));
    MessageDlg('Open InputFile Failed:'+SysErrorMessage(GetLastError), mtError, [mbOK], 0);
    Exit;
  end;
  GetMem(buffer,BufSize+10);
  try
    while True do begin
      if not ReadFile(FileHandle,buffer^,BufSize,bytesRead,nil) then begin
        LogError('devExec.pas TPipeInputThread.PipeInput',Format('Read InputFile Failed: %s',[SysErrorMessage(GetLastError)]));
        MessageDlg('Read InputFile Failed:'+SysErrorMessage(GetLastError), mtError, [mbOK], 0);
        Exit;
      end;
      if bytesRead = 0 then begin
        Exit;
      end;
      if not WriteFile(WriteHandle,buffer^,bytesRead,bytesWritten,nil) then begin
        Exit;
      end;
    end;
  finally
    FreeMem(buffer);
    CloseHandle(FileHandle);
    CloseHandle(WriteHandle);
  end;
end;
procedure TPipeInputThread.Execute;
begin
  inherited;
  PipeInput;
end;

end.
