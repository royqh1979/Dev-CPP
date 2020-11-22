{
    This file is part of Red Panda Dev-C++
    Copyright (c) 2020 Roy Qu(royqh1979@gmail.com)

    Red Panda Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    Red Panda Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}
unit Tabnine;

interface
uses
  Messages, Classes, Windows , TabnineForm, Variants,Controls,Graphics;

type

PTabnineSuggestion = ^TTabnineSuggestion;
TTabnineSuggestion = record
  OldPrefix:String;
  OldSuffix:String;
  NewPrefix:String;
  NewSuffix:String;
  Detail: String;
end;

TTabnineThread = Class;

TTabnine = Class(TComponent)
  private
    fTabnineForm : TTabnineForm;
    fOutputRead: THandle;
    fInputWrite: THandle;
    fProcessID: THandle;
    fPath: String;
    fMaxResultCount: integer;
    fBefore: String;
    fAfter:String;
    fSuggestions: TList;
    fVersion: String;
    fExecuting: boolean;
    fQuerying: boolean;
    fOnQueryBegin: TNotifyEvent;
    fOnQueryEnd: TNotifyEvent;
    fThread : TTabnineThread;
    fOnKeyPress: TKeyPressEvent;
    fOnKeyDown: TKeyEvent;
    fMinWidth: integer;
    fMinHeight: integer;
    fMaxWidth: integer;
    fMaxHeight: integer;
    fFontSize: integer;
    fPos: TPoint;
    fColor: TColor;
    fWidth: integer;
    fHeight: integer;
    fEnabled: boolean;  
    procedure GetVersion;
    procedure SetPath(Path:String);
    procedure SetPosition(Value: TPoint);
    function IsVisible: boolean;    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearSuggestions;
    procedure Start;
    procedure Stop;
    procedure PrefetchFile(const FileName: String);
    procedure Query(const FileName:String;Before:String; After:String);
    procedure QueryReady;
    property Path:String read fPath write SetPath;
    property MaxResultCount: integer read fMaxResultCount write fMaxResultCount;
    property Before: String read fBefore;
    property After:String write fBefore;
    property Suggestions: TList read fSuggestions;
    property Executing: boolean read fExecuting;
    property Querying: boolean read fQuerying;
    property OnQueryBegin: TNotifyEvent read fOnQueryBegin write fOnQueryBegin;
    property OnQueryEnd: TNotifyEvent read fOnQueryEnd write fOnQueryEnd;
    property OutputRead: THandle read fOutputRead;
    property InputWrite: THandle read fInputWrite;
    property OnKeyPress: TKeyPressEvent read fOnKeyPress write fOnKeyPress;
    property OnKeyDown: TKeyEvent read fOnKeyDown write fOnKeyDown;
    property Color: TColor read fColor write fColor;
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
    property Enabled: boolean read fEnabled write fEnabled;
    property MinWidth: integer read fMinWidth write fMinWidth;
    property MinHeight: integer read fMinHeight write fMinHeight;
    property MaxWidth: integer read fMaxWidth write fMaxWidth;
    property MaxHeight: integer read fMaxHeight write fMaxHeight;
    property FontSize: integer read fFontSize write fFontSize;
    property Visible: boolean read IsVisible;
    property Position: TPoint read fPos write SetPosition;
end;

TTabnineThread = class(TThread)
  private
    fTabnine: TTabnine;
    procedure ProcessResult(output:String);
  protected
    procedure execute; override;
  public
    property Tabnine: TTabnine read fTabnine write fTabnine;
end;

implementation

uses SysUtils, utils, Dialogs , uLkJSON, MultiLangSupport,Forms, main;

constructor TTabnine.Create(AOwner: TComponent);
begin
  inherited;
  fTabnineForm:=TTabnineForm.Create(self);
  fPath:='';
  fMaxResultCount:=10;
  fBefore:='';
  fAfter:='';
  fSuggestions:=TList.Create;
  fVersion:='2.0.2';
  fExecuting:=False;
  fQuerying:=False;
end;

destructor TTabnine.Destroy;
begin
  ClearSuggestions;
  FreeAndNil(fSuggestions);
  fTabnineForm.Free;
  inherited;
end;

procedure TTabnine.ClearSuggestions;
var
  i:integer;
begin
  for i:=0 to fSuggestions.Count-1 do begin
    dispose(PTabnineSuggestion(fSuggestions[i]));
  end;
  fSuggestions.Clear;
end;

procedure TTabnine.Clear;
begin
  fBefore:='';
  fAfter:='';
  ClearSuggestions;
  fVersion:='';
  fExecuting:=False;
  fQuerying:=False;
  fThread:=nil;
end;

procedure TTabnine.Start;
var
  pi: TProcessInformation;
  si: TStartupInfo;
  sa: TSecurityAttributes;
  fOutputWrite: THandle;
  fInputRead: THandle;
  TabnineCmd:String;
begin
  fExecuting := true;
  // Set up the security attributes struct.
  sa.nLength := sizeof(TSecurityAttributes);
  sa.lpSecurityDescriptor := nil;
  sa.bInheritHandle := true;

  // Create the child output pipe.
  if not CreatePipe(fOutputread, fOutputwrite, @sa, 0) then begin
    LogError('Tabnine.pas TTabnine.Start',Format('Create Child output Pipe failed: %s',[SysErrorMessage(GetLastError)]));
    fExecuting:=False;
    Exit;
  end;
  if not SetHandleInformation(fOutputread, HANDLE_FLAG_INHERIT, 0) then begin
    LogError('Tabnine.pas TTabnine.Start',Format('Set Child output Pipe handle flag failed: %s',[SysErrorMessage(GetLastError)]));
    fExecuting:=False;
    Exit;
  end;


  // Create the child input pipe.
  if not CreatePipe(fInputread, fInputwrite, @sa, 0) then begin
    LogError('Tabnine.pas TTabnine.Start',Format('Create Child input Pipe failed: %s',[SysErrorMessage(GetLastError)]));
    fExecuting:=False;
    Exit;
  end;
  if not SetHandleInformation(fInputwrite, HANDLE_FLAG_INHERIT, 0) then begin
    LogError('Tabnine.pas TTabnine.Start',Format('Set Child iutput Pipe handle failed %s',[SysErrorMessage(GetLastError)]));
    fExecuting:=False;
    Exit;
  end;

  // Set up the start up info struct.
  FillChar(si, sizeof(TStartupInfo), 0);
  si.cb := sizeof(TStartupInfo);
  si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW or STARTF_USESHOWWINDOW;
  si.hStdInput := fInputread;
  si.hStdOutput := fOutputwrite;
  si.hStdError := fOutputwrite;
  si.wShowWindow := SW_HIDE;

  try
    if not FileExists(fPath) then begin
      LogError('Tabnine.pas TTabnine.Start',Format('Can''t find Tabnine in : %s',[fPath]));
      MessageDlg(Format(Lang[ID_ERR_GDBNOTEXIST],[fPath]), mtError, [mbOK], 0);
      fExecuting:=False;
      Exit;
    end;
    TabnineCmd := '"' + fPath + '"';
    if not CreateProcess(nil, pChar(TabnineCmd), nil, nil, true,0, nil, nil, si, pi) then begin
      LogError('Tabnine.pas TTabnine.Start',Format('Create Tabnine process failed: %s',[SysErrorMessage(GetLastError)]));
      MessageDlg(Format(Lang[ID_ERR_ERRORLAUNCHINGGDB], [fPath, SysErrorMessage(GetLastError)]), mtError,
        [mbOK], 0);
      fExecuting := false;
      Exit;
    end;
  finally
    CloseHandle(fOutputWrite);
    CloseHandle(fInputRead);
  end;

  fProcessID := pi.hProcess;

  // Create a thread that will read Tabnine output.
  fThread := TTabnineThread.Create(true);
  fThread.Tabnine := self;
  fThread.FreeOnTerminate := true;
  fThread.Resume;

//  MainForm.UpdateAppTitle;
end;

procedure TTabnine.Stop;
begin
  if fExecuting then
    Exit;
  fExecuting := false;

  // stop tabnine
  TerminateProcess(fProcessID, 0);

  fThread.Terminate;
  fThread := nil;

  // Free resources
  CloseHandle(fProcessID);
  CloseHandle(fOutputRead);
  CloseHandle(fInputWrite);
end;

procedure TTabnine.GetVersion;
begin
  fVersion := Trim(RunAndGetOutput('"' + fPath + '" --print-version ' , '', nil, nil, false));
end;

procedure TTabnine.SetPath(Path:String);
begin
  fPath:=Path;
end;

procedure TTabnine.PrefetchFile(const FileName: String);
var
  nBytesWrote: DWORD;
  cmd:String;
  P:pChar;
begin
  if not fExecuting then
    Exit;
  cmd := '{"version":"'+fVersion+'", "request":{'
            + '"Prefetch":{'
                + '"filename":"'+FileName+'"'
            + '}'
        +'}}'#10;
  p:=pChar(cmd);
  WriteFile(fInputWrite, p, Length(cmd), nBytesWrote, nil);
end;

procedure TTabnine.Query(const FileName:String;Before:String; After:String);
var
  nBytesWrote: DWORD;
  cmd:String;
  P:pChar;
begin
  if not fExecuting then
    Exit;
  fQuerying:=True;
  if assigned(fOnQueryBegin) then
    fOnQueryBegin(self);
  cmd := '{"version":"'+fVersion+'", "request":{'
            + '"Autocomplete":{'
                + '"before": "'+TrimLeft(Before)+'",'
                + '"after": "'+TrimRight(After)+'",'
//                + '"filename": "'+AnsiToUTF8(FileName)+'",'
                + '"filename": null,'
                + '"region_includes_beginning": true,'
                + '"region_includes_end": true'
//                + '"max_num_results":"' + IntToStr(fMaxResultCount)+'"'
            + '}'
        +'}}'+#10;
  MainForm.LogOutput.Lines.Add(cmd);
  GetMem(P, Length(Cmd) + 1);
  try
    StrPCopy(P, cmd);
    WriteFile(fInputWrite, p^, Length(cmd), nBytesWrote, nil);
  finally
    FreeMem(P);
  end;
  fBefore:=Before;
  fAfter:=After;
end;

procedure TTabnine.QueryReady;
var
  i:integer;
  suggest:PTabnineSuggestion;
begin
  if assigned(fOnQueryEnd) then
    fOnQueryEnd(self);

    
  MainForm.LogOutput.Lines.Add('----------');
  for i:=0 to fSuggestions.Count-1 do begin
    suggest:=PTabnineSuggestion(fSuggestions[i]);
    MainForm.LogOutput.Lines.Add(inttostr(i)
    + suggest.OldPrefix
    + ' - '
    + suggest.OldSuffix );
    MainForm.LogOutput.Lines.Add('     '
    + suggest.NewPrefix + ' - ' + suggest.NewSuffix );
  end;

  fQuerying:=False;
end;

procedure TTabnineThread.ProcessResult(output:String);
var
  js,field: TlkJSONBase;
  Items: TlkJSONbase;
  i:integer;
  defaultOldprefix: String;
  p:PTabnineSuggestion;
begin
  fTabnine.ClearSuggestions;
  js := TlkJSON.ParseText(output);
  defaultOldprefix := '';
  field:=js.Field['old_prefix'];
  if assigned(field) then
    defaultOldPrefix:=VarToStr(field.Value);
  Items := js.Field['results'];
  for I := 0 to Pred(Items.Count) do begin
    new(p);
    p^.OldPrefix:=defaultOldPrefix;
    p^.OldSuffix:=VarToStr(Items.Child[i].Field['old_suffix'].Value) ;
    p^.NewPrefix:=VarToStr(Items.Child[i].Field['new_prefix'].Value) ;
    p^.NewSuffix:=VarToStr(Items.Child[i].Field['new_suffix'].Value) ;
    p^.Detail := VarToStr(Items.Child[i].Field['detail'].Value) ;
    fTabnine.Suggestions.Add(p);
  end;
end;

procedure TTabnineThread.Execute;
var
  tmp: String;
  bytesread, totalbytesread: DWORD;
const
  chunklen = 4096; //
begin
  bytesread := 0;
  totalbytesread := 0;
  while not Terminated do begin

    // Add chunklen bytes to length, and set chunklen extra bytes to zero
    SetLength(tmp, 1 + totalbytesread + chunklen);
    FillChar(tmp[1 + totalbytesread], chunklen + 1, 0);

    // ReadFile returns when there's something to read
    if not ReadFile(fTabnine.fOutputRead, tmp[1 + totalbytesread], chunklen, bytesread, nil) or (bytesread = 0) then
      break;

    Inc(totalbytesread, bytesread);

    if not Terminated then begin
      if tmp[totalbytesread]=#10 then begin// result finished
        ProcessResult(tmp);
        fTabnine.QueryReady;
        // Reset storage
        totalbytesread := 0;
      end;
    end;
  end;
end;

function TTabnine.IsVisible: boolean;
begin
  Result := fEnabled and fTabnineForm.Visible;
end;

procedure TTabnine.SetPosition(Value: TPoint);
begin
  fPos := Value;
  if fPos.X + fWidth > Screen.Width then
    fTabnineForm.Left := fPos.X - fWidth
  else
    fTabnineForm.Left := fPos.X;
  if fPos.Y + fHeight > Screen.Height then
    fTabnineForm.Top := fPos.Y - fHeight - 16
  else
    fTabnineForm.Top := fPos.Y;
end;
end.
