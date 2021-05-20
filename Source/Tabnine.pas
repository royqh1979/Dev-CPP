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

const
  TABNINE_AUTOCOMPLETE_CHAR_LIMIT = 100000;
  
type

PTabnineSuggestion = ^TTabnineSuggestion;
TTabnineSuggestion = record
  OldPrefix:String;
  OldSuffix:String;
  NewPrefix:String;
  NewSuffix:String;
  Detail: String;
end;

TTabnine = Class(TObject)
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

    procedure GetVersion;
    procedure SetPath(Path:String);
    procedure SetPosition(Value: TPoint);
    function GetColor(i:integer):TColor;
    procedure SetColor(i:integer; const Color:TColor);    
    function IsVisible: boolean;
    function SendCommand(cmd:AnsiString; wantResponse:boolean=True):AnsiString;
    procedure ProcessQueryResult(response:String);
  public
    constructor Create; 
    destructor Destroy; override;
    procedure ClearSuggestions;
    procedure Start;
    procedure Stop;
    procedure Show;
    procedure Hide;
    procedure PrefetchFile(const FileName: String);
    procedure Query(const FileName:String;Before:String; After:String;
      region_includes_beginning:boolean;
      region_includes_end:boolean);
    procedure QueryReady;
    function SelectedSuggestion: PTabnineSuggestion;
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
    property MinWidth: integer read fMinWidth write fMinWidth;
    property MinHeight: integer read fMinHeight write fMinHeight;
    property MaxWidth: integer read fMaxWidth write fMaxWidth;
    property MaxHeight: integer read fMaxHeight write fMaxHeight;
    property FontSize: integer read fFontSize write fFontSize;
    property Visible: boolean read IsVisible;
    property Position: TPoint read fPos write SetPosition;
    property Colors[Index: Integer]: TColor read GetColor write SetColor;
    
end;

implementation

uses SysUtils, utils, Dialogs , uLkJSON, MultiLangSupport,Forms, main, devCFG,version;

constructor TTabnine.Create;
begin
  fTabnineForm:=TTabnineForm.Create(MainForm,self);
  fPath:='';
  fMaxResultCount:=10;
  fBefore:='';
  fAfter:='';
  fSuggestions:=TList.Create;
  fVersion:='3.2.2';
  fExecuting:=False;
  fQuerying:=False;
  self.Width:=700;
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
  fTabnineForm.lbCompletion.Items.Clear;
  for i:=0 to fSuggestions.Count-1 do begin
    dispose(PTabnineSuggestion(fSuggestions[i]));
  end;
  fSuggestions.Clear;
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
  if not FileExists(fPath) then begin
    LogError('Tabnine.pas TTabnine.Start',Format('Can''t find Tabnine in : %s',[fPath]));
    MessageDlg(Format(Lang[ID_ERR_TABNINENOTEXIST],[fPath,TABNINE_SITE]), mtError, [mbOK], 0);
    devEditor.UseTabnine := False;
    fExecuting:=False;
    Exit;
  end;
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

end;

procedure TTabnine.Stop;
begin
  if not fExecuting then
    Exit;
  fExecuting := false;

  // stop tabnine
  TerminateProcess(fProcessID, 0);

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

function TTabnine.SendCommand(cmd:AnsiString; wantResponse:boolean):AnsiString;
var
  nBytesWrote: DWORD;
  P:pChar;

  function ReadResponse:AnsiString;
  var
    tmp: String;
    bytesread, totalbytesread: DWORD;
  const
    chunklen = 4096; //
  begin
  bytesread := 0;
  totalbytesread := 0;
  while True do begin
    // Add chunklen bytes to length, and set chunklen extra bytes to zero
    SetLength(tmp, 1 + totalbytesread + chunklen);
    FillChar(tmp[1 + totalbytesread], chunklen + 1, 0);
    // ReadFile returns when there's something to read
    if not ReadFile(fOutputRead, tmp[1 + totalbytesread], chunklen, bytesread, nil) or (bytesread = 0) then
      break;

    Inc(totalbytesread, bytesread);

    if tmp[totalbytesread]=#10 then begin// result finished
      Result:=tmp;
      Exit;
    end;
  end;
end;
begin
  GetMem(P, Length(Cmd) + 2);
  try
    StrPCopy(P, cmd+#10);
    WriteFile(fInputWrite, p^, Length(cmd), nBytesWrote, nil);
  finally
    FreeMem(P);
  end;
  Result:='';
  if wantResponse then
    Result:=ReadResponse;
end;

procedure TTabnine.PrefetchFile(const FileName: String);
var
  cmd:String;
begin
  if not fExecuting then
    Exit;
  cmd := '{"version":"'+fVersion+'", "request":{'
            + '"Prefetch":{'
                + '"filename":"'+AnsiToUTF8(FileName)+'"'
            + '}'
        +'}}'#10;
  SendCommand(cmd,False);
end;

procedure TTabnine.Query(const FileName:String;Before:String; After:String;
      region_includes_beginning:boolean;
      region_includes_end:boolean);
var
  cmd:String;
  response : AnsiString;
  newName:AnsiString;
begin
  fQuerying:=True;
  if not fExecuting then
    Exit;
  if assigned(fOnQueryBegin) then
    fOnQueryBegin(self);
  newName := ExtractFileName(FileName);
  if Pos('.', newName) < 1 then
    newName := newName + '.cpp';
  cmd := '{"version":"'+fVersion+'", "request":{'
            + '"Autocomplete":{'
                + '"before": "'+AnsiToUTF8(Before)+'",'
                + '"after": "'+AnsiToUTF8(After)+'",'
//                + '"filename": null,';
                + '"filename": "'+AnsiToUTF8(newName)+'",';
  if region_includes_beginning then
      cmd:=cmd  + '"region_includes_beginning": true,'
  else
      cmd:=cmd  + '"region_includes_beginning": false,';
  if region_includes_end then
      cmd:=cmd  + '"region_includes_end": true'
  else
      cmd:=cmd  + '"region_includes_end": false';
//                + '"max_num_results": "' + IntToStr(fMaxResultCount)+'"'
  cmd:= cmd + '}'
        +'}}'+#10;


  {
    with TStringLIst.Create do try
      Text:=cmd;
      SaveToFile(devDirs.Config+'log_cmd.txt');
    finally
      Free;
    end;
  }

  //MainForm.LogOutput.Lines.Add(cmd);
  fBefore:=Before;
  fAfter:=After;
  response := sendCommand(cmd,True);
  {
  with TStringList.Create do try
    text := cmd + '\n\r(cmd)\n\r' + response;
    saveToFile('E:\response.txt');
  finally
    Free;
  end;
  }
  ProcessQueryResult(response);
  QueryReady;
  fQuerying:=False;
end;

procedure TTabnine.QueryReady;
var
  i:integer;
  suggest:PTabnineSuggestion;
begin
  if assigned(fOnQueryEnd) then
    fOnQueryEnd(self);
    fTabnineForm.lbCompletion.Items.Clear;
    fTabnineForm.lbCompletion.Items.BeginUpdate;
{
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
}
  try
    for i:=0 to fSuggestions.Count-1 do begin
      suggest:=PTabnineSuggestion(fSuggestions[i]);
      fTabnineForm.lbCompletion.Items.AddObject('',TObject(suggest));
    end;
  finally
    fTabnineForm.lbCompletion.Items.EndUpdate;
    fQuerying:=False;
  end;
end;

procedure TTabnine.ProcessQueryResult(response:String);
var
  js,field,child: TlkJSONBase;
  Items: TlkJSONbase;
  i:integer;
  defaultOldprefix: String;
  p:PTabnineSuggestion;

  function getValue(node:TlkJSONBase;name:AnsiString):AnsiString;
  var
    f:TlkJSONBase;
  begin
    f:=node.Field[WideString(name)];
    if assigned(f) then
    Result := VarToStr(f.Value)
    else
      Result := '';
  end;

begin
  ClearSuggestions;
  if trim(response)='' then
    Exit;
  js := TlkJSON.ParseText(response);
  defaultOldprefix := '';
  field:=js.Field['old_prefix'];
  if assigned(field) then
    defaultOldPrefix:=VarToStr(field.Value);
  Items := js.Field['results'];
  for I := 0 to Pred(Items.Count) do begin
    child := Items.Child[i];
    new(p);
    p^.OldPrefix:=defaultOldPrefix;
    p^.OldSuffix:=getValue(child,'old_prefix');
    p^.NewPrefix:=getValue(child,'new_prefix') ;
    p^.NewSuffix:=getValue(child,'new_suffix') ;
    p^.Detail := getValue(child,'detail') ;
    fSuggestions.Add(p);
  end;
end;

function TTabnine.IsVisible: boolean;
begin
  Result := fTabnineForm.Visible;
end;

procedure TTabnine.SetPosition(Value: TPoint);
begin
  fPos := Value;
  if (fPos.X + fWidth > Screen.Width) and (fPos.X-fWidth>0) then
    fTabnineForm.Left := fPos.X - fWidth
  else
    fTabnineForm.Left := fPos.X;
  if fPos.Y + fHeight > Screen.Height then
    fTabnineForm.Top := fPos.Y - fHeight - 16
  else
    fTabnineForm.Top := fPos.Y;
end;

function TTabnine.GetColor(i:integer):TColor;
begin
  Result := fTabnineForm.Colors[i];
end;

procedure TTabnine.SetColor(i:integer; const Color:TColor);
begin
  fTabnineForm.Colors[i] := Color;
end;

procedure TTabnine.Show;
begin
  fTabnineForm.lbCompletion.Font.Size := FontSize;
  fTabnineForm.lbCompletion.ItemHeight := Round(2 * FontSize);
  fTabnineForm.Show;

end;

procedure TTabnine.Hide;
begin
  fTabnineForm.Hide;

end;

function TTabnine.SelectedSuggestion: PTabnineSuggestion;
begin
  if (fSuggestions.Count > fTabnineForm.lbCompletion.ItemIndex) and (fTabnineForm.lbCompletion.ItemIndex
      <> -1) then
      Result := PTabnineSuggestion(fSuggestions[fTabnineForm.lbCompletion.ItemIndex])
  else begin
    if fSuggestions.Count > 0 then
      Result := PTabnineSuggestion(fSuggestions[0])
    else
      Result := nil;
  end;
end;

end.
