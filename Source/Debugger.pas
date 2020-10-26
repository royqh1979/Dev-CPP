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

unit Debugger;

interface

uses
  Sysutils, Windows, Messages, Forms, Classes, Controls,
  debugreader, version, editor, ComCtrls, Dialogs, MultiLangSupport;

type

  TEvalReadyEvent = procedure(const evalvalue: AnsiString) of object;

  TDebugger = class(TObject)
  private
    fOutputRead: THandle;
    fOutputWrite: THandle;
    fInputRead: THandle;
    fInputWrite: THandle;
    fProcessID: THandle;
    fExecuting: boolean;
    fCommandChanged: boolean;
    fWatchView: TTreeView;
    fLeftPageIndexBackup: integer;
    fBreakPointList: TList;
    fWatchVarList: TList;
    fOnEvalReady: TEvalReadyEvent;
    fReader: TDebugReader;
    fUseUTF8: boolean;
    function GetBreakPointFile: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    // Play/pause
    procedure Start;
    procedure Stop;
    procedure SendCommand(const command, params: AnsiString; viewinui: boolean = false);

    // breakpoints
    procedure AddBreakPoint(i: integer); overload;
    procedure RemoveBreakPoint(i: integer); overload;
    procedure AddBreakPoint(Linein: integer; e: TEditor); overload;
    procedure RemoveBreakPoint(Linein: integer; e: TEditor); overload;
    procedure DeleteBreakPointsOf(editor: TEditor);
    procedure SetBreakPointCondition(i:integer; cond:ansistring);
    function GetBreakPointIndexOnLine(lineNo:integer; e: TEditor):Integer;

    // watch var
    procedure AddWatchVar(i: integer); overload;
    procedure RemoveWatchVar(i: integer); overload;
    procedure AddWatchVar(const namein: AnsiString); overload;
    procedure RemoveWatchVar(nodein: TTreeNode); overload;
    procedure RefreshWatchVars;
    procedure DeleteWatchVars(deleteparent: boolean);
    procedure InvalidateAllVars();

    // Access
    property Executing: boolean read fExecuting write fExecuting;
    property LeftPageIndexBackup: integer read fLeftPageIndexBackup write fLeftPageIndexBackup;
    property WatchVarList: TList read fWatchVarList write fWatchVarList;
    property BreakPointList: TList read fBreakPointList write fBreakPointList;
    property CommandChanged: boolean read fCommandChanged write fCommandChanged;
    property WatchView: TTreeView read fWatchView write fWatchView;
    property OnEvalReady: TEvalReadyEvent read fOnEvalReady write fOnEvalReady;
    property Reader: TDebugReader read fReader write fReader;
    property BreakPointFile: AnsiString read GetBreakPointFile;
    property UseUTF8: boolean read fUseUTF8 write fUseUTF8;
  end;

implementation

uses
  main, devcfg, utils, cpufrm;

constructor TDebugger.Create;
begin
  inherited;
  BreakPointList := TList.Create;
  WatchVarList := TList.Create;
end;

destructor TDebugger.Destroy;
var
  I: integer;
begin
  Stop;

  // Remove watch vars
  for i := 0 to WatchVarList.Count - 1 do
    Dispose(PWatchVar(WatchVarList.Items[i]));
  WatchVarList.Free;

  // Remove the breakpoints
  for i := 0 to BreakPointList.Count - 1 do
    Dispose(PBreakPoint(BreakPointList.Items[i]));
  BreakPointList.Free;

  if assigned(fReader) then
    fReader.Terminate;

  inherited;
end;

procedure TDebugger.Start;
var
  pi: TProcessInformation;
  si: TStartupInfo;
  sa: TSecurityAttributes;
  GDBFile, GDBCommand: AnsiString;
  CompilerSet: TdevCompilerSet;
begin
  Executing := true;

  // Set up the security attributes struct.
  sa.nLength := sizeof(TSecurityAttributes);
  sa.lpSecurityDescriptor := nil;
  sa.bInheritHandle := true;

  // Create the child output pipe.
  if not CreatePipe(fOutputread, fOutputwrite, @sa, 0) then
    Exit;
  if not SetHandleInformation(fOutputread, HANDLE_FLAG_INHERIT, 0) then
    Exit;


  // Create the child input pipe.
  if not CreatePipe(fInputread, fInputwrite, @sa, 0) then
    Exit;
  if not SetHandleInformation(fInputwrite, HANDLE_FLAG_INHERIT, 0) then
    Exit;

  // Set up the start up info struct.
  FillChar(si, sizeof(TStartupInfo), 0);
  si.cb := sizeof(TStartupInfo);
  si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW or STARTF_USESHOWWINDOW;
  si.hStdInput := fInputread;
  si.hStdOutput := fOutputwrite;
  si.hStdError := fOutputwrite;
  si.wShowWindow := SW_HIDE;

  // Use the GDB provided in the project if needed
  CompilerSet := devCompilerSets.CompilationSet;

  // Assume it's present in the first bin dir
  if CompilerSet.BinDir.Count > 0 then begin
    GDBFile := CompilerSet.BinDir[0] + pd + CompilerSet.gdbName;
    GDBCommand := '"' + GDBFile + '"' + ' --annotate=2 --silent';
    try
      if not CreateProcess(nil, PAnsiChar(GDBCommand), nil, nil, true, CREATE_NEW_CONSOLE, nil, nil, si, pi) then begin
       MessageDlg(Format(Lang[ID_ERR_ERRORLAUNCHINGGDB], [GDBFile, SysErrorMessage(GetLastError)]), mtError,
        [mbOK], 0);
       Executing := false;
        Exit;
    end;
    CloseHandle(fOutputWrite);
    CloseHandle(fInputRead);
    finally
    end;
  end else
    MessageDlg(Lang[ID_ERR_GDBNOUTFOUND], mtError, [mbOK], 0);

  fProcessID := pi.hProcess;

  // Create a thread that will read GDB output.
  Reader := TDebugReader.Create(true);
  Reader.PipeRead := fOutputRead;
  Reader.PipeWrite := fInputWrite;
  Reader.FreeOnTerminate := true;
  Reader.BreakpointList := BreakPointList;
  Reader.WatchVarList := WatchVarList;
  Reader.WatchView := WatchView;
  Reader.UseUTF8 := UseUTF8;
  Reader.Resume;

  MainForm.UpdateAppTitle;

  Application.HintHidePause := 5000;
end;

procedure TDebugger.Stop;
var
  I:integer;
  WatchVar:PWatchVar;
begin
  if Executing then begin
    Executing := false;

    if WatchVarList.Count = 0 then // nothing worth showing, restore view
      MainForm.LeftPageControl.ActivePageIndex := LeftPageIndexBackup;

    // Close CPU window
    if Assigned(CPUForm) then
      CPUForm.Close;

    TerminateProcess(fProcessID, 0); // stop gdb

    Reader.Terminate;
    Reader := nil;

    // Free resources
    CloseHandle(fProcessID);
    CloseHandle(fOutputRead);
    CloseHandle(fInputWrite);

    MainForm.RemoveActiveBreakpoints;

    MainForm.UpdateAppTitle;

    MainForm.OnBacktraceReady;

    Application.HintHidePause := 2500;

    WatchView.Items.BeginUpdate;
    try
      //Clear all watch values
      for I := 0 to WatchVarList.Count - 1 do begin
        WatchVar := PWatchVar(WatchVarList.Items[I]);
        WatchVar^.Node.Text := WatchVar^.Name + ' = '+Lang[ID_MSG_EXECUTE_TO_EVALUATE];

        // Delete now invalid children
        WatchVar^.Node.DeleteChildren;
      end;
    finally
    WatchView.Items.EndUpdate;
    end;
  end;
end;

procedure TDebugger.SendCommand(const Command, Params: AnsiString; ViewInUI: boolean);
begin
  if Executing then
    fReader.PostCommand(command,params,viewInUI);
end;

function TDebugger.GetBreakPointFile: AnsiString;
begin
  if Executing then
    Result := fReader.BreakPointFile
  else
    Result := '';
end;

procedure TDebugger.AddBreakPoint(i: integer);
var
  filename: AnsiString;
  condition : AnsiString;
begin
  // "filename":linenum
  filename := StringReplace(PBreakPoint(BreakPointList.Items[i])^.editor.FileName, '\', '/', [rfReplaceAll]);
  if PBreakPoint(BreakPointList.Items[i])^.condition = '' then
    condition := ''
  else
    condition := ' if '+PBreakPoint(BreakPointList.Items[i])^.condition;
  SendCommand('break',
    '"' + filename + '":' + inttostr(PBreakPoint(BreakPointList.Items[i])^.line)+ condition);
end;

procedure TDebugger.SetBreakPointCondition(i:integer; cond:ansistring);
begin
  PBreakPoint(BreakPointList[i])^.condition := cond;
  if cond = '' then
    SendCommand('cond', IntToStr(i+1))
  else
    SendCommand('cond', IntToStr(i+1)+' '+cond);
  MainForm.OnBreakPointsChanged;
end;

procedure TDebugger.RemoveBreakPoint(i: integer);
var
  filename: AnsiString;
begin
  // "filename":linenum
  filename := StringReplace(PBreakPoint(BreakPointList.Items[i])^.editor.FileName, '\', '/', [rfReplaceAll]);
  SendCommand('clear', '"' + filename + '":' + inttostr(PBreakPoint(BreakPointList.Items[i])^.line));
end;

procedure TDebugger.AddBreakPoint(linein: integer; e: TEditor);
var
  APBreakPoint: PBreakPoint;
begin
  APBreakPoint := new(PBreakPoint);
  with APBreakPoint^ do begin
    line := Linein;
    editor := e;
    condition := '';
  end;
  BreakPointList.Add(APBreakPoint);
  MainForm.OnBreakPointsChanged;

  // Debugger already running? Add it to GDB
  if Executing then
    AddBreakPoint(BreakPointList.Count - 1);
end;

procedure TDebugger.RemoveBreakPoint(Linein: integer; e: TEditor);
var
  i: integer;
begin
  for i := 0 to BreakPointList.Count - 1 do begin
    if (PBreakPoint(BreakPointList.Items[i])^.line = Linein) and (PBreakPoint(BreakPointList.Items[i])^.editor = e) then
      begin

      // Debugger already running? Remove it from GDB
      if Executing then
        RemoveBreakPoint(i);

      // Remove from list
      Dispose(PBreakPoint(BreakPointList.Items[i]));
      BreakPointList.Delete(i);
      break;
    end;
  end;
  MainForm.OnBreakPointsChanged;
end;

function TDebugger.GetBreakPointIndexOnLine(lineNo:integer; e: TEditor):Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to BreakPointList.Count - 1 do begin
    if (PBreakPoint(BreakPointList.Items[i])^.line = LineNo)
     and (PBreakPoint(BreakPointList.Items[i])^.editor = e) then begin
      Result := i;
      break;
    end;
  end;
end;


procedure TDebugger.DeleteBreakPointsOf(editor: TEditor);
var
  I: integer;
begin
  // Breakpoints in closed files need to be deleted
  for i := BreakPointList.Count - 1 downto 0 do

    if PBreakPoint(BreakPointList.Items[i])^.editor = editor then begin

      // Remove from list
      Dispose(PBreakPoint(BreakPointList.Items[i]));
      BreakPointList.Delete(i);
    end;

  MainForm.OnBreakPointsChanged;
    
end;

procedure TDebugger.AddWatchVar(i: integer);
begin
  SendCommand('display', PWatchVar(WatchVarList.Items[i])^.name);
end;

procedure TDebugger.RemoveWatchVar(i: integer);
begin
  SendCommand('undisplay', IntToStr(PWatchVar(WatchVarList.Items[i])^.gdbindex));
end;

procedure TDebugger.AddWatchVar(const namein: AnsiString);
var
  parentnode: TTreeNode;
  I: integer;
  wparent: PWatchVar;
begin

  // Don't allow duplicates...
  for I := 0 to WatchVarList.Count - 1 do
    if SameStr(PWatchVar(WatchVarList.Items[i])^.name, namein) then
      Exit;

  // Add parent to list
  wparent := New(PWatchVar);
  wparent^.name := namein;
  //	wparent^.value := Lang[ID_MSG_EXECUTE_TO_EVALUATE];
  wparent^.gdbindex := -1; // filled by GDB
  WatchVarList.Add(wparent);

  // Add parent to GUI
  parentnode := WatchView.Items.AddObject(nil, wparent^.name + ' = '+Lang[ID_MSG_EXECUTE_TO_EVALUATE], wparent);
  parentnode.ImageIndex := 21;
  parentnode.SelectedIndex := 21;

  // Refer to list from GUI
  wparent^.node := parentnode;

  // Debugger already running? Add it to GDB
  if Executing then
    AddWatchVar(WatchVarList.Count - 1);
end;

procedure TDebugger.RemoveWatchVar(nodein: TTreeNode);
var
  I: integer;
  wparent: PWatchVar;
begin
  for i := 0 to WatchVarList.Count - 1 do begin
    wparent := PWatchVar(WatchVarList.Items[I]);

    if SameStr(wparent^.name, PWatchVar(nodein.Data)^.name) then begin

      // Debugger already running and GDB scanned this one? Remove it from GDB
      if Executing and (wparent^.gdbindex <> -1) then
        RemoveWatchVar(i);

      // Remove from UI
      nodein.DeleteChildren;
      nodein.Delete;

      // Remove from list
      Dispose(PWatchVar(WatchVarList.Items[i]));
      WatchVarList.Delete(i);

      break;
    end;
  end;
end;

procedure TDebugger.RefreshWatchVars;
var
  I: integer;
begin
  // Variables that aren't found need to be re-displayed!
  for i := 0 to WatchVarList.Count - 1 do
    if PWatchVar(WatchVarList.Items[i])^.gdbindex = -1 then
      AddWatchVar(i); // resends command to display to GDB
end;

procedure TDebugger.DeleteWatchVars(deleteparent: boolean);
var
  I: integer;
  wparent: PWatchVar;
begin
  WatchView.Items.BeginUpdate;
  try
    for I := WatchVarList.Count - 1 downto 0 do begin
      wparent := PWatchVar(WatchVarList.Items[I]);

      if deleteparent then begin

        // Remove from UI
        if wparent^.node.HasChildren then
          wparent^.node.DeleteChildren;
        wparent^.node.Delete;

        // Remove from list
        Dispose(PWatchVar(WatchVarList.Items[i]));
        WatchVarList.Delete(i);
      end else begin

        // Remove from UI
        if wparent^.node.HasChildren then
          wparent^.node.DeleteChildren;

        // Leave parent node intact...
        wparent^.gdbindex := -1;
        wparent^.node.Text := wparent^.name + ' = '+Lang[ID_MSG_EXECUTE_TO_EVALUATE];
      end;
    end;
  finally
    WatchView.Items.EndUpdate;
  end;
end;

procedure TDebugger.InvalidateAllVars();
var
  I:integer;
  WatchVar:PWatchVar;
begin
{
    WatchView.Items.BeginUpdate;
    try
      //Clear all watch values
      for I := 0 to WatchVarList.Count - 1 do begin
        WatchVar := PWatchVar(WatchVarList.Items[I]);
        WatchVar^.Node.Text := WatchVar^.Name + ' = '+Lang[ID_MSG_NOT_FOUND_IN_CONTEXT];

        // Delete now invalid children
        WatchVar^.Node.DeleteChildren;
        WatchVar^.gdbindex := -1
      end;
    finally
      WatchView.Items.EndUpdate;
    end;
    }
end;

end.

