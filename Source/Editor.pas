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

unit Editor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CodeCompletion, CppParser, SynExportTeX,
  SynEditExport, SynExportRTF, Menus, ImgList, ComCtrls, StdCtrls, ExtCtrls, SynEdit, SynEditKeyCmds, version,
  SynEditCodeFolding, SynExportHTML, SynEditTextBuffer, Math, StrUtils, SynEditTypes, SynEditHighlighter, DateUtils,
  CodeToolTip, Tabnine,CBUtils, IntList, HeaderCompletion, Utils;

const
  USER_CODE_IN_INSERT_POS: AnsiString = '%INSERT%';
  USER_CODE_IN_REPL_POS_BEGIN: AnsiString = '%REPL_BEGIN%';
  USER_CODE_IN_REPL_POS_END: AnsiString = '%REPL_END%';
  MAX_CARET_COUNT = 100;

type

  PTabStop = ^TTabStop;
  TTabStop = record
    x:integer;
    endX:integer;
    y:integer;
  end;

  TSyntaxErrorType = (
    setError,
    setWarning);
  PSyntaxError = ^TSyntaxError;
  TSyntaxError = record
    col:integer;
    endCol:integer;
    char: integer;
    endChar: integer;
    errorType: TSyntaxErrorType;
    Token: string;
    Hint: String;
  end;

  TEditor = class;
  TDebugGutter = class(TSynEditPlugin)
  protected
    e: TEditor;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect; FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    constructor Create(editor: TEditor);
  end;

  // Define what we want to see of the word at any position
  TWordPurpose = (
    wpCompletion, // walk backwards over words, array, functions, parents, no forward movement
    wpEvaluation, // walk backwards over words, array, functions, parents, forwards over words, array
    wpHeaderCompletion, // walk backwards over path
    wpHeaderCompletionStart, // walk backwards over path, including start '<' or '"'
    wpDirective, // preprocessor
    wpJavadoc, //javadoc
    wpInformation // walk backwards over words, array, functions, parents, forwards over words
    );

  // Define why we are allowed to change the cursor to a handpoint
  THandPointReason = (
    hprPreprocessor, // cursor hovers above preprocessor line
    hprIdentifier, // cursor hovers above identifier
    hprSelection, // cursor hovers above selection
    hprNone, // mouseover not allowed
    hprError //Cursor hovers above error line/item;
    );

  TEditor = class(TObject)
  private
    fInProject: boolean;
    fFileName: AnsiString;
    fNew: boolean;
    fText: TSynEdit;
    fTabSheet: TTabSheet;
    fGutterClickedLine: integer;
    fLineCount : integer;
    fErrorLine: integer;
    fActiveLine: integer;
    fDebugGutter: TDebugGutter;
    fCurrentWord: AnsiString;
    fCurrentEvalWord: AnsiString;
    fIgnoreCaretChange: boolean;
    fPreviousEditors: TList;
    fDblClickTime: Cardinal;
    fDblClickMousePos: TBufferCoord;
    fLastMatchingBeginLine: integer;
    fLastMatchingEndLine: integer;
    {
      Format:  it's the offset relative to the previous tab stop position
        if y=0, then x means the offset in the same line relative to the previous tab stop postion
        if y>0, then x means the cursor postion in the new line
    }
    fUserCodeInTabStops: TList; //TList<PPoint> queue of offsets of tab stop(insertion) positions in the inserted user code template
    fXOffsetSince: integer; // cursor movement offset since enter previous tab stop position; only DELETE/LEFT/RIGHT will change this value
    fTabStopBegin: integer;
    fTabStopEnd: integer;
    fTabStopY: integer;
    fLineBeforeTabStop: AnsiString;
    fLineAfterTabStop : AnsiString;
    fCompletionTimer: TTimer;
    fCompletionBox: TCodeCompletion;
    fUpdateLock : integer;
    fHeaderCompletionBox : THeaderCompletion;
    fCompletionInitialPosition: TBufferCoord;
    fFunctionTipTimer: TTimer;
    fFunctionTip: TCodeToolTip;
    
    fFileEncoding: TFileEncodingType;
    fEncodingOption : TFileEncodingType;

    fLastIdCharPressed: integer;

    fTabnine:TTabnine;

    //TIntList<Line,TList<PSyntaxError>>
    fErrorList: TIntList; // syntax check errors
    fSelChanged: boolean;
    fParser : TCppParser;

    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorDblClick(Sender: TObject);
    procedure EditorClick(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorReplaceText(Sender: TObject; const aSearch, aReplace: AnsiString; Line, Column, wordlen: integer; var Action:
      TSynReplaceAction);
    procedure EditorDropFiles(Sender: TObject; x, y: integer; aFiles: TStrings);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditorExit(Sender: TObject);
    procedure EditorGutterClick(Sender: TObject; Button: TMouseButton; x, y, Line: integer; mark: TSynEditMark);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer; var Special: boolean; var FG, BG: TColor);
    procedure EditorImeInput(Sender: TObject; s:String);

    procedure EditorPaintHighlightToken(Sender: TObject; Row: integer;
      column: integer; token: String; attr: TSynHighlighterAttributes;
      var style:TFontStyles; var FG,BG:TColor);
    procedure ExporterFormatToken(Sender: TObject; Line: integer;
      column: integer; token: String; var attr: TSynHighlighterAttributes);
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
    procedure EditorEnter(Sender: TObject);
    procedure EditorEditingAreas(Sender: TObject; Line: Integer; areaList:TList;
      var Colborder: TColor;  var areaType:TEditingAreaType);
    procedure CompletionKeyPress(Sender: TObject; var Key: Char);
    procedure CompletionKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
    procedure HeaderCompletionKeyPress(Sender: TObject; var Key: Char);
    procedure HeaderCompletionKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
    procedure TabnineCompletionKeyPress(Sender: TObject; var Key: Char);
    procedure TabnineCompletionKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
    procedure TabnineCompletionInsert(appendFunc:boolean=False);
    procedure TabnineQuery;

    procedure CompletionInsert(appendFunc:boolean=False);
    procedure HeaderCompletionInsert;
    procedure CompletionTimer(Sender: TObject);
    function FunctionTipAllowed: boolean;
    procedure FunctionTipTimer(Sender: TObject);
    procedure HandleSymbolCompletion(var Key: Char);
    procedure HandleCodeCompletion(var Key: Char);
    function HandpointAllowed(var MousePos: TBufferCoord; ShiftState: TShiftState): THandPointReason;
    procedure SetFileName(const value: AnsiString);
    procedure OnMouseOverEvalReady(const evalvalue: AnsiString);
    function HasBreakPoint(Line: integer): integer;
    procedure DebugAfterPaint(ACanvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
    function GetPageControl: ComCtrls.TPageControl;
    procedure SetPageControl(Value: ComCtrls.TPageControl);
    procedure ClearUserCodeInTabStops;
    procedure PopUserCodeInTabStops;
    procedure ShowTabnineCompletion;
    function GetErrorAtPosition(pos:TBufferCoord):PSyntaxError;
    function GetErrorAtLine(line:integer):PSyntaxError;
    //procedure TextWindowProc(var Message: TMessage);
    procedure LinesDeleted(FirstLine,Count:integer);
    procedure LinesInserted(FirstLine,Count:integer);
    procedure InitParser;
    function GetParsing:boolean;
  public
    constructor Create(const Filename: AnsiString; Encoding:TFileEncodingType;
      InProject, NewFile: boolean; ParentPageControl: ComCtrls.TPageControl;
       textEdit: TSynEdit = nil);
    destructor Destroy; override;
    function Save(force:boolean = False; Reparse: boolean=True): boolean;
    function SaveAs: boolean;
    procedure Activate;
    procedure GotoLine;
    procedure SetCaretPosAndActivate(Line, Col: integer); // needs to activate in order to place cursor properly
    procedure SetCaretPos(Line, Col: integer); // needs to activate in order to place cursor properly
    procedure ExportToHTML;
    procedure ExportToRTF;
    procedure RTFToClipboard;
    procedure ExportToTEX;
    procedure InsertString(Value: AnsiString; MoveCursor: boolean);
    procedure InsertUserCodeIn(Code: AnsiString);
    procedure SetErrorFocus(Col, Line: integer);
    procedure GotoActiveBreakpoint;
    procedure SetActiveBreakpointFocus(Line: integer; setFocus:boolean=True);
    procedure RemoveBreakpointFocus;
    procedure UpdateCaption(const NewCaption: AnsiString='');
    procedure InsertDefaultText;
    procedure ToggleBreakPoint(Line: integer);
    procedure LoadFile(FileName:String);
    procedure SaveFile(FileName:String);
    function GetPreviousWordAtPositionForSuggestion(P: TBufferCoord): AnsiString;
    procedure IndentSelection;
    procedure UnindentSelection;
    procedure InitCompletion;
    procedure ShowCompletion(autoComplete:boolean);
    procedure ShowHeaderCompletion(autoComplete:boolean);
    procedure DestroyCompletion;
    procedure AddSyntaxError(line:integer; col:integer; errorType:TSyntaxErrorType; hint:String);
    procedure ClearSyntaxErrors;
    procedure GotoNextError;
    procedure GotoPrevError;
    procedure SetInProject(inProject:boolean);
    function HasPrevError:boolean;
    function HasNextError:boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Reparse;
    procedure SetEncodingOption(encoding:TFileEncodingType);

    property PreviousEditors: TList read fPreviousEditors;
    property FileName: AnsiString read fFileName write SetFileName;
    property InProject: boolean read fInProject write SetInProject;
    property New: boolean read fNew write fNew;
    property Text: TSynEdit read fText write fText;
//    property TabSheet: TTabSheet read fTabSheet write fTabSheet;
    property Parsing: boolean read GetParsing;
    property TabSheet: TTabSheet read fTabSheet;
    property FunctionTip: TCodeToolTip read fFunctionTip;
    property CompletionBox: TCodeCompletion read fCompletionBox;
    property PageControl: ComCtrls.TPageControl read GetPageControl write SetPageControl;
    property GutterClickedLine: integer read fGutterClickedLine;
    property CppParser: TCppParser read fParser;
    property FileEncoding: TFileEncodingType read fFileEncoding ;
    property EncodingOption: TFileEncodingType read fEncodingOption write SetEncodingOption;
  end;

  function GetWordAtPosition(editor:TSynEdit; P: TBufferCoord;
    var pWordBegin:TBufferCoord; var pWordEnd:TBufferCoord; Purpose: TWordPurpose): AnsiString;
implementation

uses
  main, project, MultiLangSupport, devcfg,
  DataFrm, GotoLineFrm, Macros, debugreader, IncrementalFrm,
  CodeCompletionForm, SynEditMiscClasses,
  devCaretList,
  devParser;

{ TDebugGutter }

constructor TDebugGutter.Create(editor: TEditor);
begin
  inherited Create(editor.Text);
  e := editor;
end;

procedure TDebugGutter.AfterPaint(ACanvas: TCanvas; const AClip: TRect; FirstLine, LastLine: integer);
begin
  e.DebugAfterPaint(ACanvas, AClip, FirstLine, LastLine);
end;

procedure TDebugGutter.LinesInserted(FirstLine, Count: integer);
var
  I, line: integer;
  bp: PBreakPoint;
  procedure LinesInsertedList(Items: TListItems);
  var
    I: integer;
  begin
    for I := 0 to Items.Count - 1 do begin
      if SameFileName(e.fFileName, Items[i].SubItems[1]) then begin
        line := StrToIntDef(Items[i].Caption, -1);
        if (line >= FirstLine) then
          Items[i].Caption := IntToStr(line + Count);
      end;
    end;
  end;
begin
  e.LinesInserted(FirstLine,Count);
  for I := 0 to MainForm.Debugger.BreakPointList.Count - 1 do begin
    bp := PBreakPoint(MainForm.Debugger.BreakPointList.Items[I]);
    if (integer(bp^.editor) = integer(e)) and (bp^.line >= FirstLine) then
      Inc(bp^.line, Count);
  end;
  MainForm.OnBreakPointsChanged;
  LinesInsertedList(MainForm.CompilerOutput.Items);
  LinesInsertedList(MainForm.ResourceOutput.Items);
  MainForm.FindOutput.LinesInserted(e.fFileName,FirstLine,Count);

  //LinesInsertedList(MainForm.FindOutput.Items);
end;

procedure TDebugGutter.LinesDeleted(FirstLine, Count: integer);
var
  I, line: integer;
  bp: PBreakPoint;

  procedure LinesDeletedList(Items: TListItems);
  var
    I: integer;
  begin
    for I := Items.Count - 1 downto 0 do begin
      if SameFileName(e.fFileName, Items[i].SubItems[1]) then begin
        line := StrToIntDef(Items[i].Caption, -1);
        if (line >= FirstLine) then begin
          if (line >= FirstLine + Count) then
            Items[i].Caption := IntToStr(line - Count)
          else
            Items.Delete(I);
        end;
      end;
    end;
  end;

begin
  e.LinesDeleted(FirstLine,Count);
  for I := MainForm.Debugger.BreakPointList.Count - 1 downto 0 do begin
    bp := PBreakPoint(MainForm.Debugger.BreakPointList.Items[I]);
    if (integer(bp^.editor) = integer(e)) and (bp^.line >= FirstLine) then begin
      if (bp^.line >= FirstLine + Count) then
        Dec(bp^.line, Count)
      else
        e.ToggleBreakPoint(bp^.line); // remove breakpoints INSIDE deleted selection
    end;
  end;

  // really delete items?
  MainForm.OnBreakPointsChanged;
  LinesDeletedList(MainForm.CompilerOutput.Items);
  LinesDeletedList(MainForm.ResourceOutput.Items);
  MainForm.FindOutput.LinesDeleted(e.fFileName,FirstLine,Count);
  //LinesDeletedList(MainForm.FindOutput.Items);
end;

{ TEditor }

constructor TEditor.Create(const Filename: AnsiString; Encoding:TFileEncodingType;
  InProject, NewFile: boolean; ParentPageControl: ComCtrls.TPageControl; textEdit: TSynEdit);
var
  s: AnsiString;
  I: integer;
  e: TEditor;
  num:integer;
begin
  fEncodingOption := Encoding;
  fUpdateLock := 0;
  fSelChanged:=False;
  fLineCount:=-1;
  fLastMatchingBeginLine:=-1;
  fLastMatchingEndLine:=-1;
  fLastIdCharPressed := 0;
  // Set generic options
  fErrorLine := -1;
  fActiveLine := -1;
  fInProject := InProject;
  if FileName <> '' then
    fFileName := Filename
  else
    fFileName := Lang[ID_UNTITLED] + IntToStr(dmMain.GetNewFileNumber);

  fTabSheet:=nil;
  // Remember previous tabs
  fPreviousEditors := TList.Create;
  if Assigned(ParentPageControl) then begin
    if Assigned(ParentPageControl.ActivePage) then begin
      e := TEditor(ParentPageControl.ActivePage.Tag); // copy list of previous editor
      for I := 0 to e.PreviousEditors.Count - 1 do
        fPreviousEditors.Add(e.PreviousEditors[i]);
      fPreviousEditors.Add(Pointer(e)); // make current editor history too
    end;

    // Create a new tab
    fTabSheet := TTabSheet.Create(ParentPageControl);
    fTabSheet.Caption := ExtractFileName(fFilename); // UntitlexX or main.cpp
    if assigned(ParentPageControl) then
      num := 30 div ParentPageControl.Canvas.TextWidth(' ')
    else
      num := 0;
    fTabSheet.Caption :=fTabSheet.Caption + StringOfChar(' ',num+1); 
    fTabSheet.PageControl := ParentPageControl;
    fTabSheet.Tag := integer(Self); // Define an index for each tab
  end;


  // Create an editor and set static options
  if Assigned(fTabSheet) then
    fText := TSynEdit.Create(fTabSheet)
  else
    fText := TextEdit;
//  fOldTextWndProc := fText.WndProc();

  // Load the file using Lines
  if not NewFile and FileExists(FileName) then begin
    LoadFile(FileName);
    fNew := False;

    // Save main.cpp as main.123456789.cpp
    if devData.Backups then begin
      s := '.' + IntToStr(DateTimeToUnix(Now)) + ExtractFileExt(FileName);
      SaveFile(ChangeFileExt(FileName, s));
    end;
  end else begin
    fNew := True;
    fFileEncoding := etAscii;
  end;

  // Set constant options
  if Assigned(fTabSheet) then
    fText.Parent := fTabSheet;
  fText.Visible := True;
  fText.Align := alClient;
  fText.PopupMenu := MainForm.EditorPopup;
  fText.ShowHint := True;
  fText.OnStatusChange := EditorStatusChange;
  fText.OnReplaceText := EditorReplaceText;
  fText.OnDropFiles := EditorDropFiles;
  fText.OnDblClick := EditorDblClick;
  fText.OnClick := EditorClick;
  fText.OnMouseUp := EditorMouseUp;
  fText.OnMouseMove := EditorMouseMove;
  fText.OnGutterClick := EditorGutterClick;
  fText.OnSpecialLineColors := EditorSpecialLineColors;
  fText.OnEnter := EditorEnter;
  fText.OnEditingAreas:=EditorEditingAreas;
  fText.OnExit := EditorExit;
  fText.OnKeyPress := EditorKeyPress;
  fText.OnImeInput := EditorImeInput;
  fText.OnKeyDown := EditorKeyDown;
  fText.OnKeyUp := EditorKeyUp;
  fText.OnPaintTransient := EditorPaintTransient;
  fText.OnPaintHighlightToken := EditorPaintHighlightToken;
  fText.WantReturns := True;
  fText.WantTabs := True;
//  fText.AddKeyDownHandler(EditorKeyDown);

  // Set the variable options
  devEditor.AssignEditor(fText, fFileName);

  fText.PopupMenu := MainForm.EditorPagePopup;

  // Create a gutter
  fDebugGutter := TDebugGutter.Create(self);

  if InProject then begin
    fParser := MainForm.Project.CppParser;
  end else begin
    // Create the parser
    InitParser;
  end;


  // Function parameter tips
  fFunctionTip := TCodeToolTip.Create(Application);
  fFunctionTip.Editor := fText;
  fFunctionTip.Parser := fParser;
  fFunctionTip.FileName := filename;

  // Initialize code completion stuff
  InitCompletion;

  //Initialize User Code Template stuff;
  fUserCodeInTabStops:=TList.Create;
  fXOffsetSince :=0;
  fTabStopY:=-1;
  fTabStopBegin:= -1;
  fTabStopEnd:= -1;
  fLineBeforeTabStop:='';
  fLineAfterTabStop := '';

  fErrorList := TIntList.Create;
  fErrorList.Sorted:=True;
  fErrorList.Duplicates:= dupIgnore;

  // Setup a monitor which keeps track of outside-of-editor changes
  MainForm.FileMonitor.Monitor(fFileName);

  // Set status bar for the first time
  EditorStatusChange(Self, [scOpenFile]);

end;

destructor TEditor.Destroy;
begin
  //prevent redraw of the synedit ui
  fText.BeginUpdate;
  // Deactivate the file change monitor
  MainForm.FileMonitor.UnMonitor(fFileName);
  MainForm.CaretList.RemoveEditor(self);

  // Delete breakpoints in this editor
  MainForm.Debugger.DeleteBreakPointsOf(self);

  // Destroy code completion stuff
  DestroyCompletion;

  // Free everything
  FreeAndNil(fFunctionTip);
  FreeAndNil(fText);
  FreeAndNil(fTabSheet);
  FreeAndNil(fPreviousEditors);

  ClearSyntaxErrors;
  FreeAndNil(fErrorList);
  ClearUserCodeInTabStops;
  FreeAndNil(fUserCodeInTabStops);
  if not InProject and assigned(fParser) then begin
    //FreeParser(fParser);
    FreeAndNil(fParser);
  end;
  // Move into TObject.Destroy...
  inherited;
end;

function TEditor.GetPageControl: ComCtrls.TPageControl;
begin
  if Assigned(fTabSheet) then
    Result := fTabSheet.PageControl
  else
    Result := nil;
end;

procedure TEditor.SetPageControl(Value: ComCtrls.TPageControl);
begin
  if Assigned(fTabSheet) then
    fTabSheet.PageControl := Value;
end;

procedure TEditor.OnMouseOverEvalReady(const evalvalue: AnsiString);
begin
  fText.Hint := fCurrentEvalWord + ' = ' + evalvalue;
  MainForm.Debugger.OnEvalReady := nil;
end;

procedure TEditor.Activate;
begin
  // Don't waste time refocusing
  if fText.Focused then
    Exit;
  {
  MainForm.ClassBrowser.BeginTreeUpdate;
  try
  }
    MainForm.UpdateClassBrowserForEditor(self);
    fText.BeginUpdate;
    try
      // Allow the user to start typing right away
      if assigned(fTabSheet) then begin
        fTabSheet.PageControl.ActivePage := fTabSheet;
        fTabSheet.PageControl.OnChange(fTabSheet.PageControl); // event is not fired when changing ActivePage
      end;
    finally
      fText.EndUpdate;
    end;
  {
  finally
    MainForm.ClassBrowser.EndTreeUpdate;
  end;
  }

  //don't need to reparse here, in EditorEnter event handler we will do it
  MainForm.UpdateFileEncodingStatusPanel;
end;

procedure TEditor.EditorGutterClick(Sender: TObject; Button: TMouseButton; x, y, Line: integer; mark: TSynEditMark);
begin
  if Button = mbLeft then
    ToggleBreakPoint(Line);
  fGutterClickedLine := Line;
end;

procedure TEditor.ToggleBreakpoint(Line: integer);
var
  thisbreakpoint: integer;
begin
  thisbreakpoint := HasBreakPoint(Line);

  if thisbreakpoint <> -1 then
    MainForm.Debugger.RemoveBreakPoint(Line, self)
  else
    MainForm.Debugger.AddBreakPoint(Line, self);

  // Convert buffer to display position
  fText.InvalidateGutterLine(Line);
  fText.InvalidateLine(Line);
end;

function TEditor.HasBreakPoint(Line: integer): integer;
var
  I: integer;
begin
  result := -1;
  for I := 0 to MainForm.Debugger.BreakPointList.Count - 1 do
    if integer(PBreakPoint(MainForm.Debugger.BreakPointList.Items[I])^.editor) = integer(self) then
      if PBreakPoint(MainForm.Debugger.BreakPointList.Items[I])^.line = Line then begin
        Result := I;
        break;
      end;
end;

procedure TEditor.EditorEditingAreas(Sender: TObject; Line: Integer; areaList:TList; var Colborder: TColor; var areaType: TEditingAreaType);
var
  p:PEditingArea;
  spaceCount :integer;
  spaceBefore :integer;
  lst:TList;
  i,idx: integer;
  pError: PSyntaxError;
  tc:TThemeColor;
begin
  if (fTabStopBegin >=0) and (fTabStopY=Line) then begin
    areaType:=eatEditing;
    System.new(p);
    spaceCount := fText.LeftSpacesEx(fLineBeforeTabStop,True);
    spaceBefore := Length(fLineBeforeTabStop) - Length(TrimLeft(fLineBeforeTabStop));
    p.beginX := fTabStopBegin + spaceCount - spaceBefore ;
    p.endX := fTabStopEnd + spaceCount - spaceBefore ;
    p.color := dmMain.Cpp.StringAttri.Foreground;
    areaList.Add(p);
    ColBorder := dmMain.Cpp.StringAttri.Foreground;
    Exit;
  end;
  StrToThemeColor(tc,devEditor.Syntax.Values[cWN]);
  idx:=CBUtils.FastIndexOf(fErrorList,line);
  if idx >=0 then begin
    areaType:=eatError;
    lst:=TList(fErrorList.Objects[idx]);
    for i:=0 to lst.Count-1 do begin
      System.new(p);
      pError := PSyntaxError(lst[i]);
      p.beginX := pError.col;
      p.endX := pError.endCol;
      if pError.errorType = setError then
        p.color := dmMain.Cpp.InvalidAttri.Foreground
      else
        p.color := tc.Foreground;
      areaList.Add(p);
    end;
    ColBorder := dmMain.Cpp.InvalidAttri.Foreground;
    Exit;
  end;

end;

procedure TEditor.EditorSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  tc: TThemeColor;
begin
  if (Line = fActiveLine) then begin
    StrToThemeColor(tc, devEditor.Syntax.Values[cABP]);
    BG := tc.Background;
    FG := tc.Foreground;
    if (BG <> clNone) or (FG<>clNone) then
      Special := TRUE;
  end else if (HasBreakpoint(Line) <> -1) then begin
    StrToThemeColor(tc, devEditor.Syntax.Values[cBP]);
    BG := tc.Background;
    FG := tc.Foreground;
    if (BG <> clNone) or (FG<>clNone) then
      Special := TRUE;
  end else if Line = fErrorLine then begin
    StrToThemeColor(tc,  devEditor.Syntax.Values[cErr]);
    BG := tc.Background;
    FG := tc.Foreground;
    if (BG <> clNone) or (FG<>clNone) then
      Special := TRUE;
  end;
end;

procedure TEditor.DebugAfterPaint(ACanvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  X, Y, I,j, Line , idx: integer;
  lst:TList;
  drawn:boolean;
  isError : boolean;
begin
  // Get point where to draw marks
  //X := (fText.Gutter.RealGutterWidth(fText.CharWidth) - fText.Gutter.RightOffset) div 2 - 3;
  X :=5;
  Y := (fText.LineHeight - dmMain.GutterImages.Height) div 2 + fText.LineHeight * (FirstLine - fText.TopLine);

  // The provided lines are actually rows...
  for I := FirstLine to LastLine do begin
    Line := fText.RowToLine(I);
    drawn:=False;
    if fActiveLine = Line then begin // prefer active line over breakpoints
      dmMain.GutterImages.Draw(ACanvas, X, Y, 1);
      drawn:=True;
    end else if HasBreakpoint(Line) <> -1 then begin
      dmMain.GutterImages.Draw(ACanvas, X, Y, 0);
      drawn:=True;
    end else if fErrorLine = Line then begin
      dmMain.GutterImages.Draw(ACanvas, X, Y, 2);
      drawn:=True;
    end;
    idx := CBUtils.FastIndexOf(fErrorList, Line);
    if idx>=0 then begin
      isError := False;
      lst:=TList(fErrorList.Objects[idx]);
      for j:=0 to lst.Count-1 do begin
        if PSyntaxError(lst[j])^.errorType = setError then begin
          isError := True;
          break;
        end;
      end;
      if isError then
        dmMain.GutterImages.Draw(ACanvas, X, Y, 2)
      else if not drawn then
        dmMain.GutterImages.Draw(ACanvas, X, Y, 3);
    end;

    Inc(Y, fText.LineHeight);
  end;
end;

procedure TEditor.EditorDropFiles(Sender: TObject; x, y: integer; aFiles: TStrings);
var
  sl: TStringList;
  I: integer;
begin
  // Insert into current editor
  if devEditor.InsDropFiles then begin
    fText.CaretXY := fText.DisplayToBufferPos(fText.PixelsToRowColumn(x, y));

    sl := TStringList.Create;
    try
      for I := 0 to pred(aFiles.Count) do begin
        sl.LoadFromFile(aFiles[I]);
        if GetFileEncodingType(sl.Text) = etUTF8 then
          fText.SelText := UTF8ToAnsi(sl.Text)
        else if GetFileEncodingType(sl.Text) = etUTF8Bom then
          fText.SelText := UTF8ToAnsi(Copy(sl.Text,4,MaxInt))
        else
          fText.SelText := sl.Text;
      end;
    finally
      sl.Free;
    end;
    // Open list of provided files
  end else begin
    MainForm.OpenFileList(TStringList(aFiles));
  end;
end;

procedure TEditor.EditorReplaceText(Sender: TObject; const aSearch, aReplace: AnsiString; Line, Column, wordlen: integer; var
  Action: TSynReplaceAction);
var
  pt: TPoint;
begin
  pt := fText.ClienttoScreen(fText.RowColumnToPixels(DisplayCoord(Column, Line + 1)));
  MessageBeep(MB_ICONQUESTION);
  case MessageDlgPos(format(Lang[ID_MSG_SEARCHREPLACEPROMPT], [aSearch]), mtConfirmation, [mbYes, mbNo, mbCancel,
    mbAll], 0, pt.x, pt.y + fText.LineHeight) of
    mrYes: Action := raReplace;
    mrNo: Action := raSkip;
    mrCancel: Action := raCancel;
    mrAll: Action := raReplaceAll;
  end;
end;

// Handle WM_KILLFOCUS instead of all the special cases to hide the code tooltip

procedure TEditor.EditorExit(Sender: TObject);
begin
  fFunctionTip.ReleaseHandle;
end;

// Handling this here instead of in PageControlChange because when switching between two active editors side by side
// PageControlChange will not happen, but the editor will be entered and this will be executed

procedure TEditor.EditorEnter(Sender: TObject);
var
  I, x, y: integer;
begin
  // Set title bar to current file
  MainForm.UpdateAppTitle;

  if devCodeCompletion.Enabled then begin
    BeginUpdate;
    try
    // Set classbrowser to current file (and parse file and refresh)
    MainForm.UpdateClassBrowserForEditor(self);
    finally
      EndUpdate;
    end;
  end;

  // Set compiler selector to current file
  MainForm.UpdateCompilerList;

  // Update status bar
  MainForm.SetStatusbarLineCol;
  MainForm.UpdateFileEncodingStatusPanel;

  // Update bookmark menu
  for i := 1 to 9 do
    if fText.GetBookMark(i, x, y) then begin
      MainForm.TogglebookmarksPopItem.Items[i - 1].Checked := true;
      MainForm.TogglebookmarksItem.Items[i - 1].Checked := true;
    end else begin
      MainForm.TogglebookmarksPopItem.Items[i - 1].Checked := false;
      MainForm.TogglebookmarksItem.Items[i - 1].Checked := false;
    end;

  // Update focus of incremental search
  if Assigned(IncrementalForm) and IncrementalForm.Showing then
    IncrementalForm.Editor := fText;
end;

procedure TEditor.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if (not (scOpenFile in Changes)) and  (fText.Lines.Count <> fLineCount)
    and (fText.Lines.Count <> 0) and ((fLineCount>0) or (fText.Lines.Count>1)) then begin
    if devCodeCompletion.Enabled
      and SameStr(mainForm.ClassBrowser.CurrentFile,FileName) // Don't reparse twice
      then begin
      Reparse;
    end;
    if fText.Focused and devEditor.AutoCheckSyntax and devEditor.CheckSyntaxWhenReturn
      and (fText.Highlighter = dmMain.Cpp) then begin
      mainForm.CheckSyntaxInBack(self);
    end;
  end;
  fLineCount := fText.Lines.Count;
  // scModified is only fired when the modified state changes
  if scModified in Changes then begin
    if fText.Modified then begin
      UpdateCaption;
    end else begin
      UpdateCaption;
    end;
  end;

  if (fTabStopBegin >=0) and (fTabStopY=fText.CaretY) then begin
    if StartsStr(fLineBeforeTabStop,fText.LineText) and EndsStr(fLineAfterTabStop, fText.LineText) then
      fTabStopBegin := Length(fLineBeforeTabStop);
      if fLineAfterTabStop = '' then
        fTabStopEnd := Length(fText.LineText)+1
      else
        fTabStopEnd := Length(fText.LineText) - Length(fLineAfterTabStop);
      fXOffsetSince := fTabStopEnd - fText.CaretX;
      if (fText.CaretX < fTabStopBegin) or (fText.CaretX >  (fTabStopEnd+1)) then begin
        fTabStopBegin :=-1;
      end;
  end;

  // scSelection includes anything caret related
  if scSelection in Changes then begin
    MainForm.SetStatusbarLineCol;


    // Update the function tip
    fFunctionTip.ForceHide := false;
    if Assigned(fFunctionTipTimer) then begin
      if fFunctionTip.Activated and FunctionTipAllowed then begin
        fFunctionTip.Parser := fParser;
        fFunctionTip.FileName := fFileName;
        fFunctionTip.Show;
      end else begin // Reset the timer
        fFunctionTipTimer.Enabled := false;
        fFunctionTipTimer.Enabled := true;
      end;
    end;

    // Remove error line colors
    if not fIgnoreCaretChange then begin
      if (fErrorLine <> -1) and not fText.SelAvail then begin
        fText.InvalidateLine(fErrorLine);
        fText.InvalidateGutterLine(fErrorLine);
        fErrorLine := -1;
      end;
    end else
      fIgnoreCaretChange := false;

    if fText.SelAvail then begin
      if fText.GetWordAtRowCol(fText.CaretXY) = fText.SelText then begin
        fSelChanged:=True;
        BeginUpdate;
        EndUpdate;
      end else if fSelChanged then begin
        fSelChanged:=False; //invalidate to unhighlight others
        BeginUpdate;
        EndUpdate;
      end;
    end else if fSelChanged then begin
      fSelChanged:=False; //invalidate to unhighlight others
      BeginUpdate;
      EndUpdate;
    end;

  end;

  if scInsertMode in Changes then begin
    with MainForm.Statusbar do begin
      // Set readonly / insert / overwrite
      if fText.ReadOnly then
        Panels[2].Text := Lang[ID_READONLY]
      else if fText.InsertMode then
        Panels[2].Text := Lang[ID_INSERT]
      else
        Panels[2].Text := Lang[ID_OVERWRITE];
    end;
  end;

  mainForm.CaretList.AddCaret(self,fText.CaretY,fText.CaretX);
end;

function TEditor.FunctionTipAllowed: boolean;
begin
  Result :=
    not fText.IsScrolling and // don't update when scrolling
  fText.Focused and // don't update other editors
  not fText.SelAvail and // don't update when a selection is available
  devEditor.ShowFunctionTip and // only update when option is enabled
  Assigned(fText.Highlighter) and // don't update in plaintext files
  not fFunctionTip.ForceHide; // don't update when user force hides it using ESC
end;

procedure TEditor.FunctionTipTimer(Sender: TObject);
begin
  if FunctionTipAllowed then begin
    fFunctionTip.Parser := fParser;
    fFunctionTip.FileName := fFileName;
    fFunctionTip.Show;
  end;
end;

procedure TEditor.ExportToHTML;
var
  SynExporterHTML: TSynExporterHTML;
  SaveFileName: AnsiString;
  newText : TStringList;
begin
  SynExporterHTML := TSynExporterHTML.Create(nil);
  try
    with TSaveDialog.Create(nil) do try
      Filter := SynExporterHTML.DefaultFilter;
      Title := Lang[ID_NV_EXPORT];
      DefaultExt := HTML_EXT;
      FileName := ChangeFileExt(fFileName, HTML_EXT);
      Options := Options + [ofOverwritePrompt];

      if Execute then
        SaveFileName := FileName
      else
        Exit; // automatically gotos finally
    finally
      Free;
    end;

    SynExporterHTML.Title := ExtractFileName(SaveFileName);
    SynExporterHTML.CreateHTMLFragment := False;
    SynExporterHTML.ExportAsText := True;
    SynExporterHTML.UseBackground := True;
    SynExporterHTML.Font := fText.Font;
    SynExporterHTML.Highlighter := fText.Highlighter;
    if FileEncoding = etUTF8 then begin
      SynExporterHTML.Charset := 'utf-8';
      newText := TStringList.Create;
      try
        newText.Text:=AnsiToUTF8(fText.Text);
        SynExporterHTML.ExportAll(newText);
      finally
        newText.Free;
      end;
    end else begin
      SynExporterHTML.Charset := GetSystemCharsetName;
      SynExporterHTML.ExportAll(fText.Lines);
    end;
    SynExporterHTML.SaveToFile(SaveFileName);
  finally
    SynExporterHTML.Free;
  end;
end;

procedure TEditor.RTFToClipboard;
var
  SynExporterRTF: TSynExporterRTF;
begin
  SynExporterRTF := TSynExporterRTF.Create(nil);
  try

    SynExporterRTF.Title := FileName;
    SynExporterRTF.ExportAsText := False;
    SynExporterRTF.UseBackground := True;
    SynExporterRTF.Font := fText.Font;
    SynExporterRTF.Highlighter := fText.Highlighter;
    SynExporterRTF.OnFormatToken := ExporterFormatToken;


    if fText.SelText = '' then
      SynExporterRTF.ExportAll(fText.Lines)
    else
      SynExporterRTF.ExportRange(fText.Lines,fText.BlockBegin,fText.BlockEnd);

    SynExporterRTF.CopyToClipboard;
  finally
    SynExporterRTF.Free;
  end;
end;

procedure TEditor.ExportToRTF;
var
  SynExporterRTF: TSynExporterRTF;
  SaveFileName: AnsiString;
begin
  SynExporterRTF := TSynExporterRTF.Create(nil);
  try
    with TSaveDialog.Create(nil) do try
      Filter := SynExporterRTF.DefaultFilter;
      Title := Lang[ID_NV_EXPORT];
      DefaultExt := RTF_EXT;
      FileName := ChangeFileExt(fFileName, RTF_EXT);
      Options := Options + [ofOverwritePrompt];

      if Execute then
        SaveFileName := FileName
      else
        Exit;
    finally
      Free;
    end;

    SynExporterRTF.Title := ExtractFileName(SaveFileName);
    SynExporterRTF.ExportAsText := True;
    SynExporterRTF.UseBackground := True;
    SynExporterRTF.Font := fText.Font;
    SynExporterRTF.Highlighter := fText.Highlighter;
    SynExporterRTF.OnFormatToken := ExporterFormatToken;

    SynExporterRTF.ExportAll(fText.Lines);

    SynExporterRTF.SaveToFile(SaveFileName);
  finally
    SynExporterRTF.Free;
  end;
end;

procedure TEditor.ExportToTEX;
var
  SynExporterTEX: TSynExporterTEX;
  SaveFileName: AnsiString;
begin
  SynExporterTEX := TSynExporterTEX.Create(nil);
  try
    with TSaveDialog.Create(nil) do try
      Filter := SynExporterTEX.DefaultFilter;
      Title := Lang[ID_NV_EXPORT];
      DefaultExt := TEX_EXT;
      FileName := ChangeFileExt(fFileName, TEX_EXT);
      Options := Options + [ofOverwritePrompt];

      if Execute then
        SaveFileName := FileName
      else
        Exit;
    finally
      Free;
    end;

    SynExporterTex.Title := ExtractFileName(SaveFileName);
    SynExporterTex.ExportAsText := True;
    SynExporterTex.UseBackground := True;
    SynExporterTex.Font := fText.Font;
    SynExporterTex.Highlighter := fText.Highlighter;

    SynExporterTex.ExportAll(fText.Lines);
    SynExporterTex.SaveToFile(SaveFileName);
  finally
    SynExporterTEX.Free;
  end;
end;

procedure TEditor.GotoLine;
begin
  with TGotoLineForm.Create(nil) do try
    if ShowModal = mrOK then
      SetCaretPosAndActivate(Line.Value, 1);
  finally
    Free;
  end;
end;

procedure TEditor.InsertUserCodeIn(Code: AnsiString);
var
  I, insertPos,insertEndPos, lastPos, lastI: integer;
  sl,newSl: TStringList;
  s :AnsiString;
  p:PTabStop;
  CursorPos: TBufferCoord;
  spaceCount :integer;
begin
  ClearUserCodeInTabStops;
  fXOffsetSince := 0;
  fTabStopBegin:= -1;
  fTabStopEnd:= -1;
  fTabStopY:=0;
  fLineBeforeTabStop:='';
  fLineAfterTabStop := '';
  sl:=TStringList.Create;
  newSl:=TStringList.Create;
  try
    // prevent lots of repaints
    fText.BeginUpdate;
    try
      sl.Text:=Code;
      lastI:=0;
      spaceCount := Length(Text.GetLeftSpacing(
        Text.LeftSpacesEx(fText.LineText,True), True));
      for i:=0 to sl.Count -1 do begin
        lastPos := 0;
        s:= sl[i];
        if i>0 then
          lastPos := -spaceCount; 
        while True do begin
          insertPos := Pos(USER_CODE_IN_INSERT_POS,s);
          if insertPos = 0 then // no %INSERT% macro in this line now
            break;
          System.new(p);
          Delete(s,insertPos,Length(USER_CODE_IN_INSERT_POS));
          dec(insertPos);
          p.x:=insertPos - lastPos;
          p.endX := p.x;
          p.y:=i-lastI;
          lastPos := insertPos;
          lastI:=i;
          fUserCodeInTabStops.Add(p);
        end;
        while True do begin
          insertPos := Pos(USER_CODE_IN_REPL_POS_BEGIN,s);
          if insertPos = 0 then // no %INSERT% macro in this line now
            break;
          System.new(p);
          Delete(s,insertPos,Length(USER_CODE_IN_REPL_POS_BEGIN));
          dec(insertPos);
          p.x:=insertPos - lastPos;

          insertEndPos := insertPos + Pos(USER_CODE_IN_REPL_POS_END,copy(s,insertPos+1,MaxInt));
          if insertEndPos <= insertPos then begin
            p.endX := length(s);
          end else begin
            Delete(s,insertEndPos,Length(USER_CODE_IN_REPL_POS_END));
            dec(insertEndPos);
            p.endX := insertEndPos - lastPos;
          end;
          p.y:=i-lastI;
          lastPos := insertEndPos;
          lastI:=i;
          fUserCodeInTabStops.Add(p);
        end;
        newSl.Add(s);
      end;
      CursorPos := Text.CaretXY;
      s:=newSl.Text;
      if EndsStr(#13#10,s) then
        Delete(s,Length(s)-1,2)
      else if EndsStr(#10, s) then
        Delete(s,Length(s),1);
      fText.SelText := s;
      Text.CaretXY := CursorPos; //restore cursor pos before insert
      if fUserCodeInTabStops.Count > 0  then begin
        fTabStopBegin :=Text.CaretX;
        fTabStopEnd := Text.CaretX;
        PopUserCodeInTabStops;
      end;
      if Code <> '' then
        fLastIdCharPressed := 0;
      // prevent lots of repaints
    finally
      fText.EndUpdate;
    end;
  finally
    sl.Free;
    newSl.Free;
  end;
end;


procedure TEditor.InsertString(Value: AnsiString; MoveCursor: boolean);
var
  NewCursorPos: TBufferCoord;
  Char, Line, I: integer;
  P: PAnsiChar;
begin
  // prevent lots of repaints
  fText.BeginUpdate;
  try
    NewCursorPos := fText.CaretXY;
    if MoveCursor then begin
      P := PAnsiChar(value);
      Char := fText.CaretX;
      Line := fText.CaretY;
      I := 0;
      while P[I] <> #0 do begin

        // Assume DOS newlines
        if (P[I] = #13) and (P[I + 1] = #10) then begin
          Inc(I, 2);
          Inc(Line);
          Char := 1;
        end else if (P[I] = '*') and (P[I + 1] = '|') and (P[I + 2] = '*') then begin
          NewCursorPos.Char := Char;
          NewCursorPos.Line := Line;
          Delete(value, I + 1, 3);
          break;
        end else begin
          Inc(Char);
          Inc(I);
        end;
      end;
    end;
    fText.SelText := value;

    // Update the cursor
    fText.CaretXY := NewCursorPos;

    // prevent lots of repaints
  finally
    fText.EndUpdate;
  end;
end;

procedure TEditor.SetErrorFocus(Col, Line: integer);
begin
  fIgnoreCaretChange := true;

  // Disable previous error focus
  if (fErrorLine <> -1) then begin
    fText.InvalidateGutterLine(fErrorLine);
    fText.InvalidateLine(fErrorLine);
  end;

  fErrorLine := Line;

  // Set new error focus
  SetCaretPosAndActivate(fErrorLine, col);

  // Redraw new error line
  fText.InvalidateGutterLine(fErrorLine);
  fText.InvalidateLine(fErrorLine);
end;

procedure TEditor.GotoActiveBreakpoint;
begin
  if fActiveLine <> -1 then
    SetCaretPosAndActivate(fActiveLine, 1);
end;

procedure TEditor.SetActiveBreakpointFocus(Line: integer; setFocus:boolean);
begin
  if Line <> fActiveLine then begin

    // Disable previous active focus
    if fActiveLine <> -1 then begin
      fText.InvalidateGutterLine(fActiveLine);
      fText.InvalidateLine(fActiveLine);
    end;

    // Put the caret at the active breakpoint
    fActiveLine := Line;

    if setFocus then
      SetCaretPosAndActivate(fActiveLine, 1)
    else
      SetCaretPos(fActiveLine,1);

    //SetCaretPosAndActivate(fActiveLine, 1);

    // Invalidate new active line
    fText.InvalidateGutterLine(fActiveLine);
    fText.InvalidateLine(fActiveLine);
  end;
end;

procedure TEditor.RemoveBreakpointFocus;
begin
  if fActiveLine <> -1 then begin
    fText.InvalidateLine(fActiveLine);
    fText.InvalidateGutterLine(fActiveLine);
    fActiveLine := -1;
  end;
end;

procedure TEditor.BeginUpdate;
begin
  if (fUpdateLock=0) then begin
    fText.LockPainter;
  end;
  inc(fUpdateLock);
end;

procedure TEditor.EndUpdate;
begin
  dec(fUpdateLock);
  if (fUpdateLock=0) then begin
    fText.UnlockPainter;
    fText.invalidate;
  end;
end;

procedure TEditor.UpdateCaption(const NewCaption: AnsiString);
var
  caption:String;
  num : integer;
begin
  caption:=NewCaption;
  if caption = '' then begin
    if fText.Modified then
      caption := ExtractFileName(fFileName) + ' [*]' 
    else
      caption := ExtractFileName(fFileName);
  end;
  if assigned(fTabSheet.PageControl) then
    num := 30 div fTabSheet.PageControl.Canvas.TextWidth(' ')
  else
    num := 0;
  caption := caption + StringOfChar(' ',num+1);
  if Assigned(fTabSheet) then begin
    if caption <> fTabSheet.Caption then begin
      fTabSheet.Caption := caption;
    end;
  end;
  MainForm.UpdateAppTitle;
end;

procedure TEditor.SetFileName(const value: AnsiString);
begin
  if value <> fFileName then begin
    fFileName := value;
    UpdateCaption(ExtractFileName(fFileName));
  end;
end;

procedure TEditor.InsertDefaultText;
var
  tmp: TStrings;
begin
  if devEditor.DefaultCode and FileExists(devDirs.Config + DEV_DEFAULTCODE_FILE) then begin
    tmp := TStringList.Create;
    try
      tmp.LoadFromFile(devDirs.Config + DEV_DEFAULTCODE_FILE);
      InsertString(ParseMacros(tmp.Text), false);
    finally
      tmp.Free;
    end;
  end;
end;

procedure TEditor.SetCaretPos(Line, Col: integer);
begin
  // Open up the closed folds around the focused line until we can see the line we're looking for
  fText.UncollapseAroundLine(Line);

  // Position the caret, call EnsureCursorPosVisibleEx after setting block
  fText.SetCaretXYCentered(True,BufferCoord(Col, Line));
end;


procedure TEditor.SetCaretPosAndActivate(Line, Col: integer);
begin
  // Open up the closed folds around the focused line until we can see the line we're looking for
  fText.UncollapseAroundLine(Line);

  // fText needs to be focused for TopLine and LinesInWindow to be correct
  if not fText.Focused then
    self.Activate;

  // Position the caret, call EnsureCursorPosVisibleEx after setting block
  fText.SetCaretXYCentered(True,BufferCoord(Col, Line));
end;

procedure TEditor.TabnineCompletionKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  if not fTabnine.Visible then
    Exit;
  fTabnine.Hide;
  //Send the key to the SynEdit
  PostMessage(fText.Handle, WM_KEYDOWN, key, 0);
end;

procedure TEditor.TabnineQuery;
  function EscapeString(s:AnsiString):AnsiString;
  var
    i:integer;
    inQuote:boolean;
  begin
    inQuote:=False;
    Result:='';
    i:=1;
    while (i<=Length(s)) do begin
      if (s[i] = '"') then begin
        Result:=Result+'\"';
        inQuote := not inQuote;
      end else if (s[i]='\') and inQuote and ((i+1)<=Length(s)) then begin
        if s[i+1]='"' then
          Result:=Result+'\\"'
        else
          Result:=Result+s[i]+s[i+1];
        inc(i);
      end else begin
        Result:=Result+s[i];
      end;
      inc(i);
    end;
  end;
begin
  //pos := fText.RowColToCharIndex(fText.CaretXY);
  fTabnine.Query(fFileName,
        TrimLeft(EscapeString(Copy(fText.LineText, 1, fText.CaretX-1))),
        TrimRight(EscapeString(Copy(fText.LineText, fText.CaretX,MaxInt))),
        False,
        False);
end;

procedure TEditor.TabnineCompletionKeyPress(Sender: TObject; var Key: Char);
var
  phrase:AnsiString;
  pWordBegin,pWordEnd : TBufferCoord;
begin
  if not fTabnine.Visible then
    Exit;
  // We received a key from the completion box...
    if (Key in [' ',',','(',')','[',']','+','-','/','*','&','|','!','~']) then begin // Continue filtering
      fLastIdCharPressed := 0;
      fText.SelText := Key;
      TabnineQuery;
    end else if Key = Char(VK_BACK) then begin
      fText.ExecuteCommand(ecDeleteLastChar, #0, nil); // Simulate backspace in editor
      phrase := GetWordAtPosition(fText,fText.CaretXY,pWordBegin,pWordEnd, wpCompletion);
      if phrase = '' then begin
        fLastIdCharPressed:=0;
        fTabnine.Hide;
      end else begin
        TabnineQuery;
      end;
    end else if Key = Char(VK_ESCAPE) then begin
      fTabnine.Hide;
    end else if (Key in [Char(VK_RETURN), #9 ]) then begin // Ending chars, don't insert
      TabnineCompletionInsert;
      fTabnine.Hide;
    end else if fLastIdCharPressed>0 then begin
      //fLastIdCharPressed := True;
      fText.SelText := Key;
      TabnineQuery;
    end else begin  // other keys, stop completion
      //stop completion now
      fTabnine.Hide;
      //Send the key to the SynEdit
      PostMessage(fText.Handle, WM_CHAR, Ord(Key), 0);
    end;
end;

procedure TEditor.HeaderCompletionKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  fHeaderCompletionBox.Hide;
  //Send the key to the SynEdit
  PostMessage(fText.Handle, WM_KEYDOWN, key, 0);
end;

procedure TEditor.CompletionKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  fCompletionBox.Hide;
  //Send the key to the SynEdit
  PostMessage(fText.Handle, WM_KEYDOWN, key, 0);
end;

procedure TEditor.HeaderCompletionKeyPress(Sender: TObject; var Key: Char);
var
  phrase:AnsiString;
  pBeginPos,pEndPos: TBufferCoord;
begin
  // We received a key from the completion box...
  if fHeaderCompletionBox.Enabled then begin
    if (Key in fText.IdentChars) or (Key in ['.']) then begin // Continue filtering
      fText.SelText := Key;
      phrase := GetWordAtPosition(fText,fText.CaretXY, pBeginPos,pEndPos, wpHeaderCompletion);
      fHeaderCompletionBox.Search(phrase , fFileName,False);
    end else if Key = Char(VK_BACK) then begin
      fText.ExecuteCommand(ecDeleteLastChar, #0, nil); // Simulate backspace in editor
      phrase := GetWordAtPosition(fText,fText.CaretXY,pBeginPos,pEndPos, wpHeaderCompletion);
      fLastIdCharPressed:=Length(phrase);
      fHeaderCompletionBox.Search(phrase, fFileName, False);
    end else if Key = Char(VK_ESCAPE) then begin
      fHeaderCompletionBox.Hide;
    end else if (Key in [Char(VK_RETURN), #9 ]) then begin // Ending chars, don't insert
      HeaderCompletionInsert;
      fHeaderCompletionBox.Hide;
    end else begin  // other keys, stop completion
      //stop completion now
      fHeaderCompletionBox.Hide;
      //Send the key to the SynEdit
      PostMessage(fText.Handle, WM_CHAR, Ord(Key), 0);
    end;
  end;
end;

procedure TEditor.CompletionKeyPress(Sender: TObject; var Key: Char);
var
  phrase,s:AnsiString;
  pBeginPos,pEndPos : TBufferCoord;
  attr:TSynHighlighterAttributes;
begin
  // We received a key from the completion box...
  if fCompletionBox.Enabled then begin
    if (Key in fText.IdentChars) then begin // Continue filtering
      fText.SelText := Key;

      s:=fText.LineText;
      phrase :='';
      if (fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX - 1, fText.CaretY), s, attr)) then begin
        if attr = dmMain.Cpp.DirecAttri then begin //Preprocessor
          phrase := GetWordAtPosition(fText,fText.CaretXY,pBeginPos,pEndPos, wpDirective);
        end else if attr = dmMain.Cpp.CommentAttri then begin //Comment, javadoc tag
          phrase:=GetWordAtPosition(fText, fText.CaretXY,pBeginPos,pEndPos, wpJavadoc);
        end;
      end;
      if (phrase = '') then begin
        phrase := GetWordAtPosition(fText,fText.CaretXY,pBeginPos,pEndPos, wpCompletion);
      end;
      
      fCompletionBox.Search(phrase , fFileName,False);
    end else if Key = Char(VK_BACK) then begin
      fText.ExecuteCommand(ecDeleteLastChar, #0, nil); // Simulate backspace in editor
      phrase := GetWordAtPosition(fText,fText.CaretXY,pBeginPos,pEndPos, wpCompletion);
      fLastIdCharPressed:=Length(phrase);
      fCompletionBox.Search(phrase, fFileName, False);
    end else if Key = Char(VK_ESCAPE) then begin
      fCompletionBox.Hide;
    end else if (Key in [Char(VK_RETURN), #9 ]) then begin // Ending chars, don't insert
      CompletionInsert(devCodeCompletion.AppendFunc);
      fCompletionBox.Hide;
    end else begin  // other keys, stop completion
      //stop completion now
      //CompletionInsert(Key);
      fCompletionBox.Hide;
      //Send the key to the SynEdit
      PostMessage(fText.Handle, WM_CHAR, Ord(Key), 0);
//      fText.SelText := Key;
//      EditorKeyPress(fText,Key);
    end;
  end;
end;

procedure TEditor.HandleSymbolCompletion(var Key: Char);
Type
  TQuoteStates = (NotQuote, SingleQuote, SingleQuoteEscape, DoubleQuote, DoubleQuoteEscape,
    RawString,RawStringNoEscape);
var
  Attr: TSynHighlighterAttributes;
  Token: AnsiString;
  status : TQuoteStates;
  tokenFinished: boolean;
  HighlightPos : TBufferCoord;
  tokenType : TSynHighlighterTokenType;

  function GetCurrentChar:AnsiChar;
  begin
    if Length(fText.LineText)<fText.CaretX then
      Result := #0
    else
      Result :=fText.LineText[fText.CaretX];
  end;

  function GetQuoteState:TQuoteStates;
  var
    Line: AnsiString;
    posX,i : Integer;
//    HighlightPos : TBufferCoord;
  begin
    Result := NotQuote;
    if (fText.CaretY>1) then begin
      if fText.HighLighter.GetIsLastLineStringNotFinish(fText.Lines.Ranges[fText.CaretY - 2]) then
          Result := DoubleQuote;
    end;

    Line := Text.Lines[fText.CaretY-1];
    posX :=fText.CaretX-1;
    if posX > Length(Line) then begin
      posX := Length(Line);
    end;
    i:=1;
    while (i<=posX) do begin
      if (Line[i] = 'R') and (Line[i+1] = '"') and (Result = NotQuote) then begin
        Result := RawString;
        inc(i); // skip R
      end else if Line[i] = '(' then begin
        Case Result of
          RawString: Result:=RawStringNoEscape;
          //RawStringNoEscape: do nothing
        end
      end else if Line[i] = ')' then begin
        Case Result of
          RawStringNoEscape: Result:=RawString;
        end
      end else if Line[i] = '"' then begin
        Case Result of
          NotQuote: Result := DoubleQuote;
          SingleQuote: Result := SingleQuote;
          SingleQuoteEscape: Result := SingleQuote;
          DoubleQuote: Result := NotQuote;
          DoubleQuoteEscape: Result := DoubleQuote;
          RawString: Result:=NotQuote;
          //RawStringNoEscape: do nothing
        end
      end else if Line[i] = '''' then
        Case Result of
          NotQuote: Result := SingleQuote;
          SingleQuote: Result := NotQuote;
          SingleQuoteEscape: Result := SingleQuote;
          DoubleQuote: Result := DoubleQuote;
          DoubleQuoteEscape: Result := DoubleQuote;
        end
      else if Line[i] = '\' then
        Case Result of
          NotQuote: Result := NotQuote;
          SingleQuote: Result := SingleQuoteEscape;
          SingleQuoteEscape: Result := SingleQuote;
          DoubleQuote: Result := DoubleQuoteEscape;
          DoubleQuoteEscape: Result := DoubleQuote;
        end
      else begin
        Case Result of
          NotQuote: Result := NotQuote;
          SingleQuote: Result := SingleQuote;
          SingleQuoteEscape: Result := SingleQuote;
          DoubleQuote: Result := DoubleQuote;
          DoubleQuoteEscape: Result := DoubleQuote;
        end;
      end;
      inc(i);
    end;
  end;



  procedure HandleParentheseCompletion;
  var
    status:TQuoteStates;
  begin
    status := GetQuoteState;
    if (status in [RawString,NotQuote]) then begin
      InsertString(')', false);
    end;
    if (status=NotQuote) and FunctionTipAllowed then
      fFunctionTip.Activated := true;
  end;

  procedure HandleParentheseSkip;
  var
    pos : TBufferCoord;
    status:TQuoteStates;
  begin
    if GetCurrentChar <> ')' then
      Exit;
    status := GetQuoteState;
    if status = RawStringNoEscape then begin
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
      Key := #0; // remove key press
      Exit;
    end;
    if status <> NotQuote then
      Exit;
    pos:=Text.GetMatchingBracket;
    if pos.Line <> 0 then begin
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
      Key := #0; // remove key press
    end;
    if FunctionTipAllowed then
      fFunctionTip.Activated := false;
  end;

  procedure HandleArrayCompletion;
  begin
    InsertString(']', false);
  end;

  procedure HandleArraySkip;
  var
    pos : TBufferCoord;
  begin
    if GetCurrentChar <> ']' then
      Exit;
    pos:=Text.GetMatchingBracket;
    if pos.Line <> 0 then begin
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
      Key := #0; // remove key press
    end;
  end;

  procedure HandleMultilineCommentCompletion;
  begin
    if ((fText.CaretX > 1) and (fText.CaretX-1 <= Length(fText.LineText)) and (fText.LineText[fText.CaretX - 1] = '/')) then begin
      InsertString('*/', false);
    end;
  end;

  procedure HandleBraceCompletion;
  var
    s:String;
    i:integer;
  begin
    s:=Trim(fText.LineText);
    i:= fText.CaretY-2;
    while (s='') and (i>=0) do begin
      s:=Trim(fText.Lines[i]);
      dec(i);
    end;
    if (
      ( (StartsStr('struct',s)
        or StartsStr('class',s)
        or StartsStr('union',s)
        or StartsStr('typedef',s)
        or StartsStr('public',s)
        or StartsStr('private',s)
        or StartsStr('enum',s) )
      and not ContainsStr(s,';')
      )
      or endsStr('=',s) ) then
      InsertString('};', false)
    else
      InsertString('}', false);
  end;

  procedure HandleGlobalIncludeCompletion;
  var
    s: AnsiString;
  begin
    s:=TrimLeft(Copy(fText.LineText,2,MaxInt)); //remove starting # and whitespaces
    if not StartsStr('include',s) then //it's not #include
      Exit;
    InsertString('>', false);
  end;

  procedure HandleGlobalIncludeSkip;
  var
    pos : TBufferCoord;
    s: AnsiString;
  begin
    if GetCurrentChar <> '>' then
      Exit;
    s:=TrimLeft(Copy(fText.LineText,2,MaxInt)); //remove starting # and whitespaces
    if not StartsStr('include',s) then //it'ss not #include
      Exit;
    pos:=Text.GetMatchingBracket;
    if pos.Line <> 0 then begin
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
      Key := #0; // remove key press
    end;
  end;

  procedure HandleBraceSkip;
  var
    pos : TBufferCoord;
    temp : AnsiString;
  begin
    if GetCurrentChar<> '}' then
      Exit;
    pos:=Text.GetMatchingBracket;
    if pos.Line <> 0 then begin

      // prevent lots of repaints
      fText.BeginUpdate;
      try
        temp := fText.Lines[fText.CaretY-1];
        Delete(temp,fText.CaretX,1);
        fText.Lines[fText.CaretY-1] := Temp;
      // prevent lots of repaints
      finally
        fText.EndUpdate;
      end;
//      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
//      Key := #0; // remove key press
    end;
  end;

  procedure HandleSingleQuoteCompletion;
  var
    status:TQuoteStates;
    ch: AnsiChar;
  begin
    status := GetQuoteState;
    ch := GetCurrentChar;
    if ch = '''' then begin
      if (status = SingleQuote) then begin
        fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
        Key := #0; // remove key press
      end;
    end else begin
      if (status = NotQuote) then begin
        if (ch in Text.Highlighter.WordBreakChars) or
          (ch in [#9,#32,#0]) then
          InsertString('''', false);
      end;
    end;
  end;

  procedure HandleDoubleQuoteCompletion;
  var
    status:TQuoteStates;
    ch: AnsiChar;
  begin
    status := GetQuoteState;
    ch := GetCurrentChar;
    if ch = '"' then begin
      if (status in [DoubleQuote,RawString]) then begin
        fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
        Key := #0; // remove key press
      end;
    end else begin
      if (status = NotQuote) then begin
        if (ch in Text.Highlighter.WordBreakChars) or
          (ch in [#9,#32,#0]) then
          InsertString('"', false);
      end;
    end;
  end;

begin
  if not devEditor.CompleteSymbols or fText.SelAvail then
    Exit;

  //todo: better methods to detect current caret type
  if fText.CaretX <= 1 then begin
    if fText.CaretY>1 then begin
      if fText.HighLighter.GetIsLastLineCommentNotFinish(fText.Lines.Ranges[fText.CaretY - 2]) then
        Exit;
      if fText.HighLighter.GetIsLastLineStringNotFinish(fText.Lines.Ranges[fText.CaretY - 2])
        and not (Key in ['"',''''])then
        Exit;
    end;
  end else begin
    HighlightPos := BufferCoord(fText.CaretX-1, fText.CaretY);

    // Check if that line is highlighted as  comment
    if fText.GetHighlighterAttriAtRowCol(HighlightPos, Token, tokenFinished, tokenType,Attr) then begin
      if (tokenType = httComment) and not tokenFinished then
        Exit;                                                                  
      if (tokenType = httString)
         and not tokenFinished
        and not (key in ['''','"','(',')']) then
        Exit;
      if (key in ['<','>']) and (tokenType <> httPreprocessDirective) then begin
          Exit;
      end;
      if (key = '''') and (Attr = dmMain.Cpp.NumberAttri) then begin
        Exit;
      end;
    end;
  end;

// Check if that line is highlighted as string or character or comment
//    if (Attr = fText.Highlighter.StringAttribute) or (Attr = fText.Highlighter.CommentAttribute) or SameStr(Attr.Name,
//      'Character') then
//      Exit;

  case Key of
    '(': begin
        if devEditor.ParentheseComplete then
          HandleParentheseCompletion;
      end;
    ')': begin
        if devEditor.ParentheseComplete then
          HandleParentheseSkip;
      end;
    '[': begin
        if devEditor.ArrayComplete then
          HandleArrayCompletion;
      end;
    ']': begin
        if devEditor.ParentheseComplete then
          HandleArraySkip;
      end;
    '*': begin
        status := GetQuoteState;
        if devEditor.CommentComplete  and (status = NotQuote) then
          HandleMultilineCommentCompletion;
      end;
    '{': begin
        if devEditor.BraceComplete then
          HandleBraceCompletion;
      end;
    '}': begin
        if devEditor.BraceComplete then
          HandleBraceSkip;
      end;
    '''': begin
        if devEditor.SingleQuoteComplete then // characters
          HandleSingleQuoteCompletion;
      end;
    '"': begin
        if devEditor.DoubleQuoteComplete then // strings
          HandleDoubleQuoteCompletion;
      end;
    '<': begin
        if devEditor.GlobalIncludeCompletion then // #include <>
          HandleGlobalIncludeCompletion;
      end;
    '>': begin
        if devEditor.GlobalIncludeCompletion then // #include <>
          HandleGlobalIncludeSkip;
      end;
  end;
end;

procedure TEditor.HandleCodeCompletion(var Key: Char);
begin
  if fCompletionBox.Enabled then begin
    // Use a timer to show the completion window when we just typed a few parent-member linking chars
    case Key of
      '.': fCompletionTimer.Enabled := True;
      '>': if (fText.CaretX > 1) and (Length(fText.LineText) > 1) and (fText.LineText[fText.CaretX - 1] = '-') then
          fCompletionTimer.Enabled := True;
      ':': if (fText.CaretX > 1) and (Length(fText.LineText) > 1) and (fText.LineText[fText.CaretX - 1] = ':') then
          fCompletionTimer.Enabled := True;
      '/','\': if fParser.IsIncludeLine(fText.LineText) then
        fCompletionTimer.Enabled := True;
    else
      fCompletionTimer.Enabled := False;
    end;

    // Stop code completion timer if the cursor moves
    fCompletionInitialPosition := BufferCoord(fText.CaretX + 1, fText.CaretY);
  end;
end;

procedure TEditor.EditorImeInput(Sender:TObject; s:String);
var
  lastWord:AnsiString;
  kind:TStatementKind;
begin
  if not IsIdentifier(s) then
    Exit;
  if fParser.IsIncludeLine(Text.LineText) then begin
    ShowHeaderCompletion(False);
  end else begin
    lastWord:=GetPreviousWordAtPositionForSuggestion(Text.CaretXY);
    if lastWord <> '' then begin
      if (CbUtils.CppTypeKeywords.ValueOf(lastWord) <> -1) then begin
        //last word is a type keyword, this is a var or param define, and dont show suggestion
        if devEditor.UseTabnine then
          ShowTabnineCompletion;
        Exit;
      end;
      kind := fParser.FindKindOfStatementOf(fFileName, lastWord, fText.CaretY);
      if (Kind in [skClass,skTypedef,skEnumType]) then begin
        //last word is a typedef/class/struct, this is a var or param define, and dont show suggestion
        if devEditor.UseTabnine then
          ShowTabnineCompletion;
        Exit;
      end;
    end;
    ShowCompletion(False);
  end;
end;

procedure TEditor.EditorKeyPress(Sender: TObject; var Key: Char);
var
  lastWord:AnsiString;
  kind:TStatementKind;
  
begin
  // Don't offer completion functions for plain text files
  if not Assigned(fText.Highlighter) then
    Exit;

  if (Key in fText.IdentChars) then begin
    inc(fLastIdCharPressed);
    if devCodeCompletion.Enabled and devCodeCompletion.ShowCompletionWhileInput then begin
      if fLastIdCharPressed=1 then begin
        if fParser.IsIncludeLine(Text.LineText) then begin
          // is a #include line
          fText.SelText := Key;
          ShowHeaderCompletion(False);
          Key:=#0;
        end else begin
          lastWord:=GetPreviousWordAtPositionForSuggestion(Text.CaretXY);
          if lastWord <> '' then begin
            if (CbUtils.CppTypeKeywords.ValueOf(lastWord) <> -1) then begin
            //last word is a type keyword, this is a var or param define, and dont show suggestion
              if devEditor.UseTabnine then
                ShowTabnineCompletion;
              Exit;
            end;
            kind := fParser.FindKindOfStatementOf(fFileName, lastWord, fText.CaretY);
            if (Kind in [skClass,skTypedef,skEnumType]) then begin
              //last word is a typedef/class/struct, this is a var or param define, and dont show suggestion
              if devEditor.UseTabnine then
                ShowTabnineCompletion;
              Exit;
            end;
          end;
          fText.SelText := Key;
          ShowCompletion(False);
          Key:=#0;
        end ;
      end;
    end;
  end else begin
    //preprocessor ?
    if (fLastIdCharPressed=0) and (key='#') and (fText.LineText='')
      and devCodeCompletion.Enabled and devCodeCompletion.ShowCompletionWhileInput
      then begin
      inc(fLastIdCharPressed);
      fText.SelText := Key;
      ShowCompletion(False);
      Key:=#0;
      Exit;
    end;
    //javadoc directive?
    if (fLastIdCharPressed=0) and (key='@') and StartsStr('* ',TrimLeft(fText.LineText))
      and devCodeCompletion.Enabled and devCodeCompletion.ShowCompletionWhileInput
      then begin
      inc(fLastIdCharPressed);
      fText.SelText := Key;
      ShowCompletion(False);
      Key:=#0;
      Exit;
    end;
    fLastIdCharPressed:=0;
    // Doing this here instead of in EditorKeyDown to be able to delete some key messages
    HandleSymbolCompletion(Key);

    if (key in [' ','+','-','*','/','<','&','|','!','~']) and devEditor.UseTabnine then begin
      if (key = '/') and fParser.IsIncludeLine(fText.LineText) and devCodeCompletion.Enabled then begin
        HandleCodeCompletion(Key);
      end;
      //Show Tabnine
      fText.SelText := Key;
      Key:=#0;
      ShowTabnineCompletion;
    end else begin
      // Spawn code completion window if we are allowed to
      if devCodeCompletion.Enabled then begin
        HandleCodeCompletion(Key);
      end;
    end;
  end;
end;

procedure TEditor.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  p: TBufferCoord;
  DeletedChar, NextChar: Char;
  S: AnsiString;
  Reason: THandPointReason;
  params,insertString:TStringList;
  funcName:AnsiString;
  isVoid : boolean;
  i:integer;

  procedure UndoSymbolCompletion;
  var
    Attr: TSynHighlighterAttributes;
    Token: AnsiString;
    tokenFinished: boolean;
    tokenType: TSynHighlighterTokenType;
    HighlightPos : TBufferCoord;
  begin
    if devEditor.DeleteSymbolPairs then begin
      HighlightPos := BufferCoord(fText.CaretX, fText.CaretY);

      // Check if that line is highlighted as  comment
      if not fText.GetHighlighterAttriAtRowCol(HighlightPos, Token, tokenFinished, tokenType, Attr) then
        Exit;
      if (tokenType = httComment) and not tokenFinished then
        Exit;
      if (tokenType = httCharacter) and not (DeletedChar = '''') then
        Exit;
      if (tokenType = httStringEscapeSequence) then
        Exit;
      if (tokenType = httString) then begin
        if not (DeletedChar in ['"','(']) then
          Exit; 
        if (DeletedChar = '"') and not SameStr(Token,'""') then
          Exit;
        if (DeletedChar = '(') and not StartsStr('R"',Token) then
          Exit;
      end;
      if (DeletedChar = '''') and (tokenType = httNumber) then begin
        Exit;
      end;
      if (DeletedChar = '<') and
          ((tokenType <> httPreprocessDirective) or not StartsStr('#include',fText.LineText)) then begin
          Exit;
      end;
      if (devEditor.ArrayComplete and (DeletedChar = '[') and (NextChar = ']')) or
        (devEditor.ParentheseComplete and (DeletedChar = '(') and (NextChar = ')')) or
        (devEditor.GlobalIncludeCompletion and (DeletedChar = '<') and (NextChar = '>')) or
        (devEditor.BraceComplete and (DeletedChar = '{') and (NextChar = '}')) or
        (devEditor.SingleQuoteComplete and (DeletedChar = '''') and (NextChar = '''')) or
        (devEditor.DoubleQuoteComplete and (DeletedChar = '"') and (NextChar = '"')) then begin
        fText.LineText := Copy(S, 1, fText.CaretX - 1) + Copy(S, fText.CaretX + 1, MaxInt);
      end;
    end;

  end;

  begin
  // Don't offer completion functions for plain text files
  if not Assigned(fText.Highlighter) then
    Exit;

  // See if we can undo what has been inserted by HandleSymbolCompletion
  case (Key) of
    VK_CONTROL: begin
        fLastIdCharPressed:=0;
        Reason := HandpointAllowed(p, Shift);
        if Reason <> hprNone then
          fText.Cursor := crHandPoint
        else
          fText.Cursor := crIBeam;
      end;
    VK_RETURN: begin
        fLastIdCharPressed:=0;
        if fTabStopBegin>=0 then begin // editing user code template
          key:=0;
          fTabStopBegin:=-1;
          fText.InvalidateLine(fText.CaretY);
          ClearUserCodeInTabStops;
        end else begin
          s:=trim(Copy(fText.LineText,1,fText.CaretX-1));
          if SameStr('/**',s) then begin //javadoc style docstring
            s:=trim(Copy(fText.LineText,fText.CaretX,MAXINT));
            if SameStr('*/',s) then begin
              p:=fText.CaretXY;
              fText.BlockBegin:=p;
              p.Char := Length(fText.LineText)+1;
              fText.BlockEnd := p;
              fText.SelText := '';
            end;
            Key:=0;
            params:=TStringList.Create;
            insertString:= TStringList.Create;
            try
              insertString.Add('');
              funcName := fParser.FindFunctionDoc(fFileName,fText.CaretY+1,
                params,isVoid);
              if funcName <> '' then begin
                insertString.Add(' * @brief '+USER_CODE_IN_INSERT_POS);
                insertString.Add(' * ');
                for i:=0 to params.Count-1 do begin
                  insertString.Add(' * @param '+params[i]+' '+USER_CODE_IN_INSERT_POS);
                end;
                if not isVoid then begin
                  insertString.Add(' * ');
                  insertString.Add(' * @return '+USER_CODE_IN_INSERT_POS);
                end;
                insertString.Add(' **/');
              end else begin
                insertString.Add(' * '+USER_CODE_IN_INSERT_POS);
                insertString.Add(' **/');
              end;
              InsertUserCodeIn(insertString.Text);
            finally
              insertString.Free;
              params.Free;
            end;
          end else if fText.Highlighter.GetIsLastLineCommentNotFinish(fText.Lines.Ranges[fText.CaretY-2]) then
            s:=trimLeft(fText.LineText);
            if StartsStr('* ',s) then begin
              Key:=0;
              s:=#13#10+'* ';
              self.insertString(s,false);
              p:=fText.CaretXY;
              inc(p.Line);
              p.Char := length(fText.Lines[p.Line-1])+1;
              fText.CaretXY := p;
            end;
        end;
      end;
    VK_ESCAPE: begin // Update function tip
        fLastIdCharPressed:=0;
        if fTabStopBegin>=0 then begin
          fTabStopBegin:=-1;
          fText.InvalidateLine(fText.CaretY);
          self.ClearUserCodeInTabStops;
        end;
        if ttoHideOnEsc in fFunctionTip.Options then begin
          fFunctionTip.ReleaseHandle;
          fFunctionTip.ForceHide := true;
        end;
      end;
    VK_TAB: begin
        if fUserCodeInTabStops.Count > 0 then begin
          Key:=0;
          PopUserCodeInTabStops;
          fText.InvalidateLine(fText.CaretY);
        end else begin
          if fTabStopBegin >= 0 then begin
            Key:=0;
            fTabStopBegin:=-1;
            fText.InvalidateLine(fText.CaretY);
          end else begin
            Key:=0;
            IndentSelection;
          end;
        end;
      end;
    VK_UP: begin
        fLastIdCharPressed:=0;
        ClearUserCodeInTabStops;
      end;
    VK_DOWN: begin
        fLastIdCharPressed:=0;
        ClearUserCodeInTabStops;
      end;
    VK_DELETE: begin
        // remove completed character
        fLastIdCharPressed:=0;
        if not fText.SelAvail then begin
          S := fText.LineText;
          if fText.CaretX < Length(S) then begin
            DeletedChar := S[fText.CaretX];
            NextChar := S[fText.CaretX + 1];
            UndoSymbolCompletion;
          end;
        end;
      end;
    VK_BACK: begin // remove completed character
        fLastIdCharPressed:=0;
        if not fText.SelAvail then begin
          S := fText.LineText;
          if (fText.CaretX > 1) and (fText.CaretX <= Length(S)) then begin
            DeletedChar := S[fText.CaretX - 1];
            NextChar := S[fText.CaretX];
            UndoSymbolCompletion;
          end;
        end;
      end;
  end;
end;

procedure TEditor.EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case(key) of
    VK_CONTROL: begin
        fText.Cursor := crIBeam;
      end;
  end;
end;

procedure TEditor.CompletionTimer(Sender: TObject);
begin
  if fParser.IsIncludeLine(fText.LineText) then begin
    ShowHeaderCompletion(False);
    Exit;
  end;
  // Don't show the completion box if the cursor has moved during the timer rundown
  if (fText.CaretX <> fCompletionInitialPosition.Char) or
    (fText.CaretY <> fCompletionInitialPosition.Line) then
    Exit;

  ShowCompletion(False);
end;

function TEditor.GetParsing: boolean;
begin
  Result := Assigned(fParser) and fParser.Parsing;
end;

procedure TEditor.InitCompletion;
begin
  fTabnine := MainForm.Tabnine;
  fCompletionBox := MainForm.CodeCompletion;
  fCompletionBox.Enabled := devCodeCompletion.Enabled;
  fHeaderCompletionBox := MainForm.HeaderCompletion;
  fHeaderCompletionBox.Enabled := devCodeCompletion.Enabled;

  if devEditor.ShowFunctionTip then begin
    if not Assigned(fFunctionTipTimer) then
      fFunctionTipTimer := TTimer.Create(nil);
    fFunctionTipTimer.Enabled := True;
    fFunctionTipTimer.OnTimer := FunctionTipTimer;
    fFunctionTipTimer.Interval := 2 * GetCaretBlinkTime; // fancy
  end else begin
    fFunctionTip.ReleaseHandle; // hide
    FreeAndNil(fFunctionTipTimer); // remove timer, because we only have a limited number avaiable
  end;

  // The other stuff is fully completion dependant
  if fCompletionBox.Enabled then begin
    fCompletionBox.Width := devCodeCompletion.Width;
    fCompletionBox.Height := devCodeCompletion.Height;

    if not Assigned(fCompletionTimer) then
      fCompletionTimer := TTimer.Create(nil);
    fCompletionTimer.Enabled := False;
    fCompletionTimer.OnTimer := CompletionTimer;
    fCompletionTimer.Interval := devCodeCompletion.Delay;
  end else begin
    FreeAndNil(fCompletionTimer);
  end;
end;

procedure TEditor.ShowTabnineCompletion;
var
  P: TPoint;
  s: AnsiString;
  attr: TSynHighlighterAttributes;
begin
  if not fTabnine.Executing then
    exit;
  if fTabnine.Visible then // already in search, don't do it again
    Exit;

  // Only scan when cursor is not placed in a comment
  if (fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX - 1, fText.CaretY), s, attr)) then
    if (attr = fText.Highlighter.CommentAttribute) then
      Exit;

  // Position it at the top of the next line
  P := fText.RowColumnToPixels(fText.DisplayXY);
  Inc(P.Y, fText.LineHeight + 2);
  fTabnine.Position := fText.ClientToScreen(P);


  //Set Font size;
  fTabnine.FontSize := fText.Font.Size;

  // Redirect key presses to completion box if applicable
  fTabnine.OnKeyPress := TabnineCompletionKeyPress;
  fTabnine.OnKeyDown := TabnineCompletionKeyDown;
  fTabnine.Show;
  TabnineQuery;
end;

procedure TEditor.ShowHeaderCompletion(autoComplete:boolean);
var
  P: TPoint;
  word: AnsiString;
  pBeginPos,pEndPos : TBufferCoord;
begin
  if not devCodeCompletion.Enabled then
    Exit;
  fCompletionTimer.Enabled := False;

  if fHeaderCompletionBox.Visible then // already in search, don't do it again
    Exit;

  // Position it at the top of the next line
  P := fText.RowColumnToPixels(fText.DisplayXY);
  Inc(P.Y, fText.LineHeight + 2);
  fHeaderCompletionBox.Position := fText.ClientToScreen(P);

  fHeaderCompletionBox.IgnoreCase := devCodeCompletion.IgnoreCase;
  fHeaderCompletionBox.ShowCount := devCodeCompletion.MaxCount;
  //Set Font size;
  fHeaderCompletionBox.FontSize := fText.Font.Size;

  // Redirect key presses to completion box if applicable
  fHeaderCompletionBox.OnKeyPress := HeaderCompletionKeyPress;
  fHeaderCompletionBox.OnKeyDown := HeaderCompletionKeyDown;
  fHeaderCompletionBox.Parser := fParser;


  word:=GetWordAtPosition(fText, fText.CaretXY,pBeginPos,pEndPos, wpHeaderCompletionStart);
  if (word = '') then
   Exit;

  if (word[1]<>'"') and (word[1]<>'<') then
    Exit;

  if (LastPos('"',word) > 1) or (LastPos('>',word) > 1) then
    Exit;

  fHeaderCompletionBox.Show;    
  fHeaderCompletionBox.SearchLocal := (word[1] = '"');
  fHeaderCompletionBox.CurrentFile := fFileName;

  Delete(word,1,1);
  
  //if not fCompletionBox.Visible then
  fHeaderCompletionBox.PrepareSearch(word, fFileName);

  // Filter the whole statement list
  if fHeaderCompletionBox.Search(word, fFileName, autoComplete) then //only one suggestion and it's not input while typing
    HeaderCompletionInsert(); // if only have one suggestion, just use it
end;

procedure TEditor.ShowCompletion(autoComplete:boolean);
var
  P: TPoint;
  s,word: AnsiString;
  attr: TSynHighlighterAttributes;
  pBeginPos,pEndPos:TBufferCoord;
begin
  if not devCodeCompletion.Enabled then
    Exit;
  if not CppParser.Enabled then
    Exit;
  fCompletionTimer.Enabled := False;

  if fCompletionBox.Visible then // already in search, don't do it again
    Exit;

  word:='';
  // Only scan when cursor is placed after a symbol, inside a word, or inside whitespace
  if (fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX - 1, fText.CaretY), s, attr)) then begin
    if attr = dmMain.Cpp.DirecAttri then begin //Preprocessor
      word:=GetWordAtPosition(fText, fText.CaretXY,pBeginPos,pEndPos, wpDirective);
      if not StartsStr('#',word) then begin
        ShowTabnineCompletion;
        Exit;
      end;
    end else if attr = dmMain.Cpp.CommentAttri then begin //Comment, javadoc tag
      word:=GetWordAtPosition(fText, fText.CaretXY,pBeginPos,pEndPos, wpJavadoc);
      if not StartsStr('@',word) then begin
        Exit;
      end;
    end else if (attr <> fText.Highlighter.SymbolAttribute) and
      (attr <> fText.Highlighter.WhitespaceAttribute) and
      (attr <> fText.Highlighter.IdentifierAttribute) then begin
      Exit;
    end;
  end;

  // Position it at the top of the next line
  P := fText.RowColumnToPixels(fText.DisplayXY);
  Inc(P.Y, fText.LineHeight + 2);
  fCompletionBox.Position := fText.ClientToScreen(P);

  fCompletionBox.RecordUsage := devCodeCompletion.RecordUsage;
  fCompletionBox.SortByScope := devCodeCompletion.SortByScope;
  fCompletionBox.ShowKeywords := devCodeCompletion.ShowKeywords;
  fCompletionBox.ShowCodeIns := devCodeCompletion.ShowCodeIns;
  fCompletionBox.IgnoreCase := devCodeCompletion.IgnoreCase;
  fCompletionBox.CodeInsList := dmMain.CodeInserts.ItemList;
  fCompletionBox.SymbolUsage := dmMain.SymbolUsage;
  fCompletionBox.ShowCount := devCodeCompletion.MaxCount;
  //Set Font size;
  fCompletionBox.FontSize := fText.Font.Size;

  // Redirect key presses to completion box if applicable
  fCompletionBox.OnKeyPress := CompletionKeyPress;
  fCompletionBox.OnKeyDown := CompletionKeyDown;
  fCompletionBox.Parser := fParser;
  fCompletionBox.Show;

  // Scan the current function body
  fCompletionBox.CurrentStatement := fParser.FindAndScanBlockAt(fFileName, fText.CaretY);

  if word='' then
    word:=GetWordAtPosition(fText, fText.CaretXY,pBeginPos,pEndPos, wpCompletion);
  //if not fCompletionBox.Visible then
  fCompletionBox.PrepareSearch(word, fFileName,pBeginPos.Line);

  // Filter the whole statement list
  if fCompletionBox.Search(word, fFileName, autoComplete) then //only one suggestion and it's not input while typing
    CompletionInsert(); // if only have one suggestion, just use it
end;

procedure TEditor.DestroyCompletion;
begin
  FreeAndNil(fCompletionTimer);
  FreeAndNil(fFunctionTipTimer);
end;

function TEditor.GetPreviousWordAtPositionForSuggestion(P: TBufferCoord): AnsiString;
var
  WordBegin, WordEnd:integer;
  s: AnsiString;
  bracketLevel:integer;
  skipNextWord: boolean;
  inFunc:boolean;

  function TestInFunc(x,y:integer):boolean;
  var
    posX,posY: integer;
    s: AnsiString;
    bracketLevel:integer;
  begin
    Result:=False;
    s := fText.Lines[y];
    posY := y;
    posX := x;
    if posX > length(s) then
      posX := length(s);
    bracketLevel:=0;
    while True do begin
      while posX < 1 do begin
        dec(posY);
        if posY < 0 then
          Exit;
        s := fText.Lines[posY];
        posX := Length(s);
      end;
      if s[posX] in ['>',']'] then begin
        inc(bracketLevel);
      end else if s[posX] in ['<','['] then begin
        dec(bracketLevel);
      end else if (bracketLevel=0) then begin
        case s[posX] of
          '(': begin
            Result:= True;
            Exit;
          end;
          ';','{': begin
            Exit;
          end;
        end;
        if not (s[posX] in [#9,#32,'*','&',',','_','0'..'9','a'..'z','A'..'Z']) then
           break;
      end;
      dec(posX);
    end;
  end;
  
begin
  result := '';
  if (p.Line >= 1) and (p.Line <= fText.Lines.Count) then begin
    inFunc := TestInFunc(p.Char-1,p.Line-1);

    s := fText.Lines[p.Line - 1];
    WordEnd := p.Char-1;
    if WordEnd > length(s) then
      WordEnd := Length(s);
    while True do begin
      bracketLevel:=0;
      skipNextWord:=False;
      while (WordEnd > 0) do begin
        if s[WordEnd] in ['>',']'] then begin
          inc(bracketLevel);
        end else if s[WordEnd] in ['<','['] then begin
          dec(bracketLevel);
        end else if (bracketLevel=0) then begin
        {we can't differentiate multiple definition and function parameter define here , so we don't handle ','}
          if s[WordEnd] = ',' then begin
            if inFunc then // in func, dont skip ','
              break
            else
              skipNextWord:=True;
          end else if not (s[WordEnd] in [#9,#32,'*','&']) then
            break;
        end;
        dec(WordEnd);
      end;
      if WordEnd<=0 then
        Exit;
      if bracketLevel > 0 then
        Exit;
      if not (s[WordEnd] in ['_','0'..'9','a'..'z','A'..'Z']) then
        Exit;

      wordBegin := WordEnd;
      while (WordBegin > 0) and (s[WordBegin] in ['_','0'..'9','a'..'z','A'..'Z']) do begin
        dec(WordBegin);
      end;
      inc(WordBegin);

      if s[WordBegin] in ['0'..'9'] then // not valid word
        Exit;

      Result := Copy(S, WordBegin , WordEnd - WordBegin+1);
      if (Result <> 'const') and not SkipNextWord then begin
        break;
      end;
      WordEnd:= WordBegin-1;
    end;
  end;
end;

function GetWordAtPosition(editor: TSynEdit; P: TBufferCoord;
  var pWordBegin:TBufferCoord; var pWordEnd:TBufferCoord; Purpose: TWordPurpose): AnsiString;
var
  WordBegin, WordEnd, ParamBegin, ParamEnd, len,line,i: integer;
  s: AnsiString;
  HighlightPos,pDummy:TBufferCoord;
begin
  result := '';
  if (p.Line >= 1) and (p.Line <= editor.Lines.Count) then begin
    s := editor.Lines[p.Line - 1];
    len := Length(s);

    WordBegin := p.Char - 1;
    WordEnd := p.Char - 1;

    // Copy forward until end of word
    if Purpose in [wpEvaluation, wpInformation] then begin
      while (WordEnd + 1 <= len) do begin
        if (Purpose = wpEvaluation) and (s[WordEnd + 1] = '[') then begin
          if not FindComplement(s, '[', ']', WordEnd, 1) then
            break;
        end else if (s[WordEnd + 1] in editor.IdentChars) then
          Inc(WordEnd)
        else
          break;
      end;
    end;

    // Copy backward until #
    if Purpose = wpDirective then begin
     while (WordBegin > 0) and (WordBegin <= len) do begin
        if (s[WordBegin] in editor.IdentChars) then begin
          Dec(WordBegin);
        end else if s[WordBegin] in ['#'] then begin // allow destructor signs
          Dec(WordBegin);
        end else
          break;
      end;
    end;

    // Copy backward until @
    if Purpose = wpJavadoc then begin
     while (WordBegin > 0) and (WordBegin <= len) do begin
        if (s[WordBegin] in editor.IdentChars) then begin
          Dec(WordBegin);
        end else if s[WordBegin] in ['@'] then begin // allow destructor signs
          Dec(WordBegin);
        end else
          break;
      end;
    end;

    // Copy backward until begin of path
    if Purpose = wpHeaderCompletion then begin
      while (WordBegin > 0) and (WordBegin <= len) do begin
        if (s[WordBegin] in ['/','\','.']) then begin
          Dec(WordBegin); 
        end else if (s[WordBegin] in editor.IdentChars) then begin
          Dec(WordBegin);
        end else
          break;
      end;
    end;

    if Purpose = wpHeaderCompletionStart then begin
      while (WordBegin > 0) and (WordBegin <= len) do begin
        if (s[WordBegin] in ['"','<']) then begin
          Dec(WordBegin);
          break;
        end else if (s[WordBegin] in ['/','\','.']) then begin
          Dec(WordBegin);
        end else if (s[WordBegin] in editor.IdentChars) then begin
          Dec(WordBegin);
        end else
          break;
      end;
    end;

    // Copy backward until begin of word
    if Purpose in [wpCompletion, wpEvaluation, wpInformation] then begin
      while (WordBegin > 0) and (WordBegin <= len) do begin
        if (s[WordBegin] = ']') then begin
          if not FindComplement(s, ']', '[', WordBegin, -1) then
            break
          else
            Dec(WordBegin); // step over [
        end else if (s[WordBegin] in editor.IdentChars) then begin
          Dec(WordBegin);
        end else if s[WordBegin] in ['.', ':', '~'] then begin // allow destructor signs
          Dec(WordBegin);
        end else if (s[WordBegin] ='>') and (wordBegin+1<=len) and (s[WordBegin+1]=':') then begin // allow template
          if not FindComplement(s, '>', '<', WordBegin, -1) then
            break
          else
            Dec(WordBegin); // step over >
        end else if (WordBegin > 1) and (s[WordBegin - 1] = '-') and (s[WordBegin] = '>') then begin
          Dec(WordBegin, 2);
        end else if (WordBegin > 1) and (s[WordBegin] = ')') then begin
          if not FindComplement(s, ')', '(', WordBegin, -1) then
            break
          else
            Dec(WordBegin); // step over (
        end else
          break;
      end;
    end;
  end;

  // Get end result
  Result := Copy(S, WordBegin + 1, WordEnd - WordBegin);
  pWordBegin.Line := p.Line;
  pWordBegin.Char := wordBegin+1;
  pWordEnd.Line := p.Line;
  pWordEnd.Char := wordEnd;
  if (Result <> '') and (Result[1] in ['.','-'])
    and (Purpose in [wpCompletion, wpEvaluation, wpInformation]) then begin
    i:=WordBegin;
    line:=p.Line;
    while line>=1 do begin
      while i>=1 do begin
        if S[i] in [' ',#9] then
          dec(i)
        else
          break;
      end;
      if i<1 then begin
        dec(line);
        if (line>=1) then begin
          S:=editor.Lines[line-1];
          i:=Length(s);
          continue;
        end else
          break;
      end else begin
        HighlightPos.Line := line;
        HighlightPos.Char := i+1;
        Result := GetWordAtPosition(editor,highlightPos,pWordBegin,pDummy,Purpose)+Result;
        break;
      end;
    end;
  end;

  // Strip function parameters
  while true do begin
    ParamBegin := Pos('(', Result);
    if ParamBegin > 0 then begin
      ParamEnd := ParamBegin;
      if (ParamBegin=1) and FindComplement(Result, '(', ')', ParamEnd, 1)
        and (ParamEnd = Length(Result)) then begin
        Delete(Result,ParamEnd,1);
        Delete(Result,ParamBegin,1);
        continue;
      end else begin
        ParamEnd := ParamBegin;
        if FindComplement(Result, '(', ')', ParamEnd, 1) then begin
          Delete(Result, ParamBegin, ParamEnd - ParamBegin + 1);
        end else
          break;
      end;
    end else
      break;
  end;

  ParamBegin := 1;
  while (ParamBegin <= Length(Result)) and (Result[ParamBegin] = '*') do begin
    inc(ParamBegin);
  end;
  Delete(Result,1,ParamBegin-1);

  {
  // Strip array stuff
  if not (Purpose = wpEvaluation) then
    while true do begin
      ParamBegin := Pos('[', Result);
      if ParamBegin > 0 then begin
        ParamEnd := ParamBegin;
        if FindComplement(Result, '[', ']', ParamEnd, 1) then begin
          Delete(Result, ParamBegin, ParamEnd - ParamBegin + 1);
        end else
          break;
      end else
        break;
    end;
  }
end;

procedure TEditor.TabnineCompletionInsert;
var
  suggestion: PTabnineSuggestion;
  P,P1: TBufferCoord;

begin
  try
    suggestion := fTabnine.SelectedSuggestion;
    if not Assigned(suggestion) then
      Exit;

    // delete the part of the word that's already been typed ...
    p:=fText.CaretXY ; // CaretXY will change after call WordStart
    p1:=p;
    p1.Char := p.Char - length(suggestion^.OldPrefix);
    fText.BlockBegin := p1;
    p1.Char :=  p.Char + length(suggestion^.OldSuffix);
    fText.BlockEnd :=p1;
    fText.SelText := suggestion^.NewPrefix+suggestion^.NewSuffix;
    p.Char := p.Char-length(suggestion^.OldPrefix) + length(suggestion^.NewPrefix);
    fText.CaretXY := p;
  finally
    fTabnine.Hide;
  end;
end;

procedure TEditor.HeaderCompletionInsert;
var
  headerName: String;
  P: TBufferCoord;
  posBegin,posEnd:integer;
  sLine:String;
begin
  headerName := fHeaderCompletionBox.SelectedFilename;
  if headerName = '' then
    Exit; 

  // delete the part of the word that's already been typed ...
  p:=fText.CaretXY ; // CaretXY will change after call WordStart
  posBegin := p.Char;
  posEnd := p.Char;
  sLine := fText.LineText;
  while (posBegin>1) and
    ((sLine[posBegin-1] in fText.IdentChars) or (sLine[posBegin-1]='.')) do
      dec(posBegin);

  while (posEnd < Length(sLine)) and
    ((sLine[posEnd] in fText.IdentChars) or (sLine[posEnd]='.')) do
      inc(posEnd);


  p.Char := posBegin;
  fText.SelStart := fText.RowColToCharIndex(p);
  p.Char := posEnd;
  fText.SelEnd := fText.RowColToCharIndex(p);
  fText.SelText := headerName;

  fCompletionBox.Hide;
end;

procedure TEditor.CompletionInsert(appendFunc:boolean);
var
  statement: PStatement;
  FuncAddOn: AnsiString;
  P: TBufferCoord;
  idx: integer;
  usageCount:integer;
begin
  Statement := fCompletionBox.SelectedStatement;
  if not Assigned(Statement) then
    Exit;

  if devCodeCompletion.RecordUsage and (Statement^._Kind <> skUserCodeIn) then begin
    idx:=Utils.FastIndexOf(dmMain.SymbolUsage,Statement^._FullName);
    if idx = -1 then begin
      usageCount:=1;
      dmMain.SymbolUsage.AddObject(Statement^._FullName, pointer(1))
    end else begin
      usageCount := 1 + integer(dmMain.SymbolUsage.Objects[idx]);
      dmMain.SymbolUsage.Objects[idx] := pointer( usageCount );
    end;
    Statement^._UsageCount := usageCount;
  end;

  FuncAddOn := '';

  // delete the part of the word that's already been typed ...
  p:=fText.CaretXY ; // CaretXY will change after call WordStart
  fText.SelStart := fText.RowColToCharIndex(fText.WordStart);
  p:=fText.WordEnd;
  fText.SelEnd := fText.RowColToCharIndex(p);

  // if we are inserting a function,
  if appendFunc then begin
    if Statement^._Kind in [skFunction, skConstructor, skDestructor] then begin
      if (Length(fText.LineText) < p.Char+1 ) // it's the last char on line
        or (fText.LineText[p.Char+1] <> '(') then begin  // it don't have '(' after it
        if not (SameStr('std::endl',Statement^._Fullname)) then
          FuncAddOn := '()';
      end;
    end;
  end;

  // ... by replacing the selection
  if Statement^._Kind = skUserCodeIn then begin // it's a user code template
    InsertUserCodeIn(Statement^._Value);
  end else begin
    if (Statement^._Kind in [skKeyword,skPreprocessor]) and
      (length(Statement^._Command)>0) and
      (Statement^._Command[1] in ['#','@']) then begin
      fText.SelText := Copy(Statement^._Command,2,MAXINT);
    end else
      fText.SelText := Statement^._Command + FuncAddOn;

    if FuncAddOn <> '' then
      fLastIdCharPressed := 0;
    // Move caret inside the ()'s, only when the user has something to do there...
    if (FuncAddOn <> '') and (Statement^._Args <> '()') and (Statement^._Args <> '(void)') then begin

      fText.CaretX := fText.CaretX - Length(FuncAddOn) + 1;

      // immediately activate function hint
      if devEditor.ShowFunctionTip and Assigned(fText.Highlighter) then begin
        fText.SetFocus;
        fFunctionTip.Parser := fParser;
        fFunctionTip.FileName := fFileName;
        fFunctionTip.Show;
      end;
    end;
  end;
  fCompletionBox.Hide;
end;

procedure TEditor.EditorDblClick(Sender: TObject);
begin
  fDblClickTime := GetTickCount;
  fText.GetPositionOfMouse(fDblClickMousePos);
end;

procedure TEditor.EditorClick(Sender: TObject);
var
  fTripleClickTime: Cardinal;
  fTripleClickMousePos: TBufferCoord;
  fNewState: TSynStateFlags;
begin
  fTripleClickTime := GetTickCount;
  fText.GetPositionOfMouse(fTripleClickMousePos);
  if (fTripleClickTime > fDblClickTime) and
    (fTripleClickTime - GetDoubleClickTime < fDblClickTime) and
    (fTripleClickMousePos.Char = fDblClickMousePos.Char) and
    (fTripleClickMousePos.Line = fDblClickMousePos.Line) then begin

    // Don't let the editor change the caret
    fNewState := fText.StateFlags;
    Exclude(fNewState, sfWaitForDragging);
    fText.StateFlags := fNewState;

    // Select the current line
    if fText.CaretY < fText.Lines.Count then begin
      fText.BlockBegin := BufferCoord(1, fText.CaretY);
      fText.BlockEnd := BufferCoord(1, fText.CaretY + 1);
    end else begin
      fText.BlockBegin := BufferCoord(1, fText.CaretY);
      fText.BlockEnd := BufferCoord(Length(fText.Lines[fText.CaretY - 1]) + 1, fText.CaretY);
    end;
  end;
end;

procedure TEditor.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  s: AnsiString;
  p: TBufferCoord;
  Reason: THandPointReason;
  IsIncludeLine: boolean;
  pError : pSyntaxError;
  line:integer;
  pBeginPos,pEndPos:TBufferCoord;

  procedure ShowFileHint;
  var
    FileName: AnsiString;
  begin
    FileName := fParser.GetHeaderFileName(fFileName, s);
    if (FileName <> '') and FileExists(FileName) then
      fText.Hint := FileName + ' - Ctrl+Click for more info'
    else
      fText.Hint := '';
  end;

  procedure ShowErrorHint;
  begin
    fText.Hint := pError.Hint;
  end;
  
  procedure ShowParserHint;
  var
    hint: string;
  begin
    // This piece of code changes the parser database, possibly making hints and code completion invalid...
    hint := fParser.GetHintFromStatement(fFileName, s, p.Line);
    if hint <> '' then begin
      // vertical bar is used to split up short and long hint versions...
      fText.Hint := hint;
    end;
  end;

  procedure ShowDebugHint;
  var
    kind: TStatementKind;
  begin
    kind := fParser.FindKindOfStatementOf(fFileName, s, p.Line);

    if kind in [skVariable, skParameter] then begin //only show debug info of variables;
      if MainForm.Debugger.Reader.CommandRunning then
        Exit;

      // Add to list
      if devData.WatchHint then
        MainForm.Debugger.AddWatchVar(s);

      // Evaluate s
      fCurrentEvalWord := s; // remember name when debugger finishes
      MainForm.Debugger.OnEvalReady := OnMouseOverEvalReady;
      MainForm.Debugger.SendCommand('print', s, False);
    end else if devEditor.ParserHints then begin
      ShowParserHint;
    end;
  end;

  procedure CancelHint;
  begin
    MainForm.Debugger.OnEvalReady := nil;

    // disable editor hint
    Application.CancelHint;
    fCurrentWord := '';
    fText.Hint := '';

    // disable page control hint
    MainForm.CurrentPageHint := '';
    if assigned(fTabSheet) then
      fTabSheet.PageControl.Hint := '';
  end;
begin
  // Leverage Ctrl-Clickability to determine if we can show any information
  Reason := HandpointAllowed(p, Shift);

  if Reason = hprError then begin
    pError := GetErrorAtPosition(p);
  end else if (Reason = hprNone) and fText.GetLineOfMouse(line) then begin //it's on gutter
    //see if its error;
    pError := GetErrorAtLine(line);
    if Assigned(pError) then begin
      Reason := hprError;
    end;
  end;

  // Get subject
  IsIncludeLine := False;
  case Reason of
    // When hovering above a preprocessor line, determine if we want to show an include or a identifier hint
    hprPreprocessor: begin
        s := fText.Lines[p.Line - 1];
        IsIncludeLine := fParser.IsIncludeLine(s); // show filename hint
        if not IsIncludeLine then
          s := fText.GetWordAtRowCol(p);
      end;
    hprIdentifier: begin
        if MainForm.Debugger.Executing then
          s := GetWordAtPosition(fText, p, pBeginPos,pEndPos, wpEvaluation) // debugging
        else if devEditor.ParserHints and not fCompletionBox.Visible then
          s := GetWordAtPosition(fText, p, pBeginPos,pEndPos, wpInformation) // information during coding
        else
          s := '';
      end;
    hprSelection: begin
        s := fText.SelText; // when a selection is available, always only use that
      end;
    hprError: begin
        s:=pError^.Token;
      end;
    hprNone: begin
        fText.Cursor := crIBeam; // nope
        CancelHint;
      end;
  end;

  // Don't rescan the same stuff over and over again (that's slow)
//  if (s = fCurrentWord) and (fText.Hint<>'') then
  s:=trim(s);
  if (s = fCurrentWord) then
    Exit; // do NOT remove hint when subject stays the same

  // Remove hint
  CancelHint;
  fCurrentWord := s;

  // We are allowed to change the cursor
  if (ssCtrl in Shift) then
    fText.Cursor := crHandPoint
  else
    fText.Cursor := crIBeam;

  // Determine what to do with subject
  case Reason of
    hprPreprocessor: begin
      if IsIncludeLine then
        ShowFileHint
      else if devEditor.ParserHints and not fCompletionBox.Visible then
        ShowParserHint;
    end;
    hprIdentifier, hprSelection: begin
      if not fCompletionBox.Visible  then begin
        if MainForm.Debugger.Executing then
          ShowDebugHint
        else if devEditor.ParserHints then
          ShowParserHint;
      end;
    end;
    hprError : begin
      ShowErrorHint;
    end;
  end;
end;

procedure TEditor.IndentSelection;
begin
  if FText.BlockBegin.Line <> FText.BlockEnd.Line then
    fText.ExecuteCommand(ecBlockIndent, #0, nil)
  else
    fText.ExecuteCommand(ecTab, #0, nil);
end;

procedure TEditor.UnindentSelection;
begin
  if fText.BlockBegin.Line <> fText.BlockEnd.Line then
    fText.ExecuteCommand(ecBlockUnIndent, #0, nil)
  else
    fText.ExecuteCommand(ecShiftTab, #0, nil);
end;

procedure TEditor.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TDisplayCoord;
  line, FileName: AnsiString;
  e: TEditor;
begin
  if (Button = mbLeft) then begin
    fLastIdCharPressed:=0;
  end;
  // if ctrl+clicked
  if (ssCtrl in Shift) and (Button = mbLeft) and not fText.SelAvail then begin

    p := fText.PixelsToRowColumn(X, Y);
    if P.Row <= fText.Lines.Count then begin

      // reset the cursor
      fText.Cursor := crIBeam;

      // Try to open the header
      line := fText.Lines[p.Row - 1];
      if fParser.IsIncludeLine(Line) then begin
        FileName := fParser.GetHeaderFileName(fFileName, line);
        e := MainForm.EditorList.GetEditorFromFileName(FileName);
        if Assigned(e) then begin
          e.SetCaretPosAndActivate(1, 1);
        end;
      end else
        MainForm.actGotoImplDeclEditorExecute(self);
    end;
  end;
end;

procedure TEditor.ExporterFormatToken(Sender: TObject; Line: integer;
      column: integer; token: String; var attr: TSynHighlighterAttributes);
var
  kind : TStatementKind;
  p: TBufferCoord;
  s:String;
  pBeginPos,pEndPos : TBufferCoord;
begin
  if token='' then
    Exit;

  if fCompletionBox.Visible then //don't do this when show code suggestion
    Exit;
  if (attr = fText.Highlighter.IdentifierAttribute) then begin
    p:=BufferCoord(column+1,line);
    s:= GetWordAtPosition(fText,p, pBeginPos,pEndPos, wpInformation);
    kind := fParser.FindKindOfStatementOf(fFileName,
      s , line);
    if Kind <> skUnknown then begin
      case Kind of
        skPreprocessor, skEnum: begin
          attr:=dmMain.Cpp.DirecAttri;
        end;
        skVariable: begin
          attr:=dmMain.Cpp.VariableAttri;
        end;
        skLocalVariable, skParameter: begin
          attr:=dmMain.Cpp.LocalVarAttri;
        end;
        skGlobalVariable: begin
          attr:=dmMain.Cpp.GlobalVarAttri;
        end;
        skFunction,skConstructor,skDestructor: begin
          attr:=dmMain.Cpp.FunctionAttri;
        end;
        skClass,skNamespace,skTypedef,skEnumType : begin
          attr:=dmMain.Cpp.ClassAttri;
        end;
      end;
      if attr.Foreground = clNone then //old color theme, use the default color
        attr := dmMain.Cpp.IdentifierAttri;
    end;
  end;
end;


procedure TEditor.EditorPaintHighlightToken(Sender: TObject; Row: integer;
  column: integer; token: String; attr: TSynHighlighterAttributes;
  var style:TFontStyles; var FG,BG:TColor);
var
  tc:TThemeColor;
  kind: TStatementKind;
  p: TBufferCoord;
  s:String;
  pBeginPos,pEndPos : TbufferCoord;
begin
  if token='' then
    Exit;
  //selection
  if fText.SelAvail then begin
    if (
      (attr = fText.Highlighter.IdentifierAttribute)
      or (attr = fText.Highlighter.KeywordAttribute)
      or (attr = dmMain.Cpp.DirecAttri)
      )
      and SameStr(token, fText.SelText) then begin
      StrToThemeColor(tc, devEditor.Syntax.Values[cSel]);
      FG := tc.Foreground;
      BG := tc.Background;
      exit;
    end;
  end;

  {
  if fCompletionBox.Visible then //don't do this when show
    Exit;
  }
  {
  if (attr = fText.Highlighter.WhitespaceAttribute) and StartsStr('.',token) then begin
    FG:=fText.Gutter.Font.Color;
    BG:=attr.Background;
    Exit;
  end;
  }
  if assigned(fParser) and (attr = fText.Highlighter.IdentifierAttribute) then begin
    //st := fFindStatementOf(fFileName, token, line);
    p:=fText.DisplayToBufferPos(DisplayCoord(column+1,Row));
    s:= GetWordAtPosition(fText,p, pBeginPos,pEndPos, wpInformation);
    kind := fParser.FindKindOfStatementOf(fFileName,
      s , p.Line);
    fg:= attr.Foreground;
    case kind of
      skPreprocessor, skEnum: begin
        fg:=dmMain.Cpp.DirecAttri.Foreground;
      end;
      skVariable: begin
        fg:=dmMain.Cpp.VariableAttri.Foreground;
      end;
      skLocalVariable, skParameter: begin
        fg:=dmMain.Cpp.LocalVarAttri.Foreground;
      end;
      skGlobalVariable: begin
        fg:=dmMain.Cpp.GlobalVarAttri.Foreground;
      end;
      skFunction,skConstructor,skDestructor: begin
        fg:=dmMain.Cpp.FunctionAttri.Foreground;
      end;
      skClass,skNamespace,skTypedef, skEnumType : begin
        fg:=dmMain.Cpp.ClassAttri.Foreground;
      end;
      skUnknown: begin
        if (pEndPos.Line>=1)
          and (pEndPos.Char+1 < length(fText.Lines[pEndPos.Line-1]))
          and (fText.Lines[pEndPos.Line-1][pEndPos.Char+1] = '(') then begin
          fg:=dmMain.Cpp.FunctionAttri.Foreground;
        end else begin
          fg:=dmMain.Cpp.VariableAttri.Foreground;
        end;
      end;
    end;
    if fg = clNone then //old color theme, use the default color
      fg := attr.Foreground;
  end;
end;

procedure TEditor.EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
const
  AllChars = ['{', '[', '(', '}', ']', ')'];
  OpenChars = ['{', '[', '('];
  CloseChars = ['}', ']', ')'];
var
  HighlightCharPos: TBufferCoord;
  ComplementCharPos: TBufferCoord;
  Pix: TPoint;
  S: AnsiString;
  Attri: TSynHighlighterAttributes;
  LineLength: integer;

  procedure SetColors(Point: TBufferCoord);
  var
    tc:TThemeColor;
  begin
    // Draw using highlighting colors
    if TransientType = ttAfter then begin
      Canvas.Font.Color := fText.Highlighter.WhitespaceAttribute.Background; // swap colors
      Canvas.Brush.Color := Attri.Foreground;

      // Draw using normal colors
    end else begin
      if devEditor.HighCurrLine and (Point.Line = fText.CaretY) then begin // matching char is inside highlighted line
        StrToThemeColor(tc, devEditor.Syntax.Values[cAL]);
        Canvas.Brush.Color := tc.Background;
        Canvas.Font.Color := Attri.Foreground;
      end else begin
        Canvas.Brush.Color := Attri.Background;
        Canvas.Font.Color := Attri.Foreground;
      end;
    end;
    if Canvas.Font.Color = clNone then
      Canvas.Font.Color := fText.Font.Color;
    if Canvas.Brush.Color = clNone then
      Canvas.Brush.Color := fText.Highlighter.WhitespaceAttribute.Background;
  end;

  procedure ClearLastMatch;
  var
    l1,l2:integer;
  begin
    if fLastMatchingBeginLine >=0 then begin
      l1:=fLastMatchingBeginLine;
      l2:=fLastMatchingEndLine;
      fLastMatchingBeginLine:=-1;
      fLastMatchingEndLine:=-1;
      if (l1 <> HighlightCharPos.Line) and (l1<> ComplementCharPos.Line) then
        fText.InvalidateLine(l1);
      if (l2 <> HighlightCharPos.Line) and (l2<> ComplementCharPos.Line) then
        fText.InvalidateLine(l2);
    end;
  end;

begin
  // Don't bother wasting time when we don't have to
  if not Assigned(fText.Highlighter) then // no highlighted file is viewed
    Exit;
  if not devEditor.Match then // user has disabled match painting
    Exit;

  HighlightCharPos.Line := -1;
  ComplementCharPos.Line := -1;
  try
    if fText.SelAvail then // not clear cut what to paint
      Exit;
    //Exit; // greatly reduces flicker

    // Is there a bracket char before us?
    LineLength := Length(fText.LineText);
    if (fText.CaretX - 1 > 0) and (fText.CaretX - 1 <= LineLength) and (fText.LineText[fText.CaretX - 1] in AllChars) then
      HighlightCharPos := BufferCoord(fText.CaretX - 1, fText.CaretY)

      // Or after us?
    else if (fText.CaretX > 0) and (fText.CaretX <= LineLength) and (fText.LineText[fText.CaretX] in AllChars) then
      HighlightCharPos := BufferCoord(fText.CaretX, fText.CaretY);

    // Character not found. Exit.
    if HighlightCharPos.Line = -1 then begin
      Exit;
    end;

    // Is the OpenChar before/after us highlighted as a symbol (not a comment or something)?
    if not (fText.GetHighlighterAttriAtRowCol(HighlightCharPos, S, Attri) and (Attri = fText.Highlighter.SymbolAttribute))
      then begin
      Exit;
    end;

    // Find the corresponding bracket
    ComplementCharPos := fText.GetMatchingBracketEx(HighlightCharPos);
    if (ComplementCharPos.Char = 0) and (ComplementCharPos.Line = 0) then begin
      Exit;
    end;

    // At this point we have found both characters. Check if both are visible
    if Assigned(fText.FoldHidesLine(HighlightCharPos.Line)) or
      Assigned(fText.FoldHidesLine(ComplementCharPos.Line)) then begin
      Exit;
    end;
  finally
    ClearLastMatch;
  end;

  // Both are visible. Draw them
  // First, draw bracket where caret is placed next to the caret
  Canvas.Brush.Style := bsSolid;
  Canvas.Font.Assign(fText.Font);
  Canvas.Font.Style := Attri.Style;

  // Draw the character the caret is at here using this color
  SetColors(HighlightCharPos);
  Pix := fText.RowColumnToPixels(fText.BufferToDisplayPos(HighlightCharPos));
  if Pix.X > fText.GutterWidth then begin // only draw if inside viewable area
    S := fText.Lines[HighlightCharPos.Line - 1][HighlightCharPos.Char];
    Canvas.TextOut(Pix.X, Pix.Y, S);
  end;

  // Then draw complement
  SetColors(ComplementCharPos);
  Pix := fText.RowColumnToPixels(fText.BufferToDisplayPos(ComplementCharPos));
  if Pix.X > fText.GutterWidth then begin // only draw if inside viewable area
    S := fText.Lines[ComplementCharPos.Line - 1][ComplementCharPos.Char];
    Canvas.TextOut(Pix.X, Pix.Y, S);
  end;

  // Reset brush
  Canvas.Brush.Style := bsSolid;

  fLastMatchingBeginLine:=HighlightCharPos.Line;
  fLastMatchingEndLine:=ComplementCharPos.Line;

end;

function TEditor.HandpointAllowed(var MousePos: TBufferCoord; ShiftState: TShiftState): THandPointReason;
var
  s: AnsiString;
  HLAttr: TSynHighlighterAttributes;
begin
  Result := hprNone;

  // Only allow in the text area...
  if fText.GetPositionOfMouse(mousepos) then begin
    if Assigned(GetErrorAtPosition(mousepos)) then begin
      Result := hprError;
      Exit;
    end;

    // Only allow hand points in highlighted areas
    if fText.GetHighlighterAttriAtRowCol(mousepos, s, HLAttr) then begin

      // Only allow Identifiers, Preprocessor directives, and selection
      if Assigned(HLAttr) then begin
        if fText.SelAvail then begin
          // do not allow when dragging selection
          if fText.IsPointInSelection(MousePos) and not (ssLeft in ShiftState) then
            Result := hprSelection; // allow selection
        end else if HLAttr.Name = 'Identifier' then
          Result := hprIdentifier // allow identifiers if no selection is present
        else if HLAttr.Name = 'Preprocessor' then
          Result := hprPreprocessor; // and preprocessor line if no selection is present
      end;
    end;
  end;
end;

function TEditor.Save(force:boolean;reparse:boolean):boolean;
begin
  Result := True;

  // We will be changing files. Stop monitoring
  MainForm.FileMonitor.BeginUpdate;
  try

    // Is this file read-only?
    if (not force) and FileExists(fFileName) and (FileGetAttr(fFileName) and faReadOnly <> 0) then begin

      // Yes, ask the user if he wants us to fix that
      if MessageDlg(Format(Lang[ID_MSG_FILEISREADONLY], [fFileName]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;

      // Yes, remove read-only attribute
      if FileSetAttr(fFileName, FileGetAttr(fFileName) - faReadOnly) <> 0 then begin
        MessageDlg(Format(Lang[ID_MSG_FILEREADONLYERROR], [fFileName]), mtError, [mbOk], 0);
        Exit;
      end;
    end;

    // Filename already present? Save without dialog
    if ( (not fNew) and fText.Modified) or force then begin

      // Save contents directly
      try
        SaveFile(fFileName);
        fText.Modified := false;
      except
        if not force then
          MessageDlg(Format(Lang[ID_ERR_SAVEFILE], [fFileName]), mtError, [mbOk], 0);
        Result := False;
      end;

      if reparse and devCodeCompletion.Enabled and assigned(fParser) then begin
        BeginUpdate;
        try
          ParseFile(fParser, fFileName, InProject);
        finally
          EndUpdate;
        end;
      end;
    end else if fNew then
      Result := SaveAs; // we need a file name, use dialog

  finally
    MainForm.FileMonitor.EndUpdate;
  end;
end;

function TEditor.SaveAs: boolean;
var
  UnitIndex: integer;
  SaveFileName: AnsiString;
begin
  Result := True;
  with TSaveDialog.Create(nil) do try
    Title := Lang[ID_NV_SAVEAS];
    Filter := BuildFilter([FLT_CS, FLT_CPPS, FLT_HEADS, FLT_RES]);
    Options := Options + [ofOverwritePrompt];

    // select appropriate filter
    if GetFileTyp(fFileName) in [utcHead, utcppHead] then begin
      FilterIndex := 4; // .h
      DefaultExt := 'h';
    end else begin
      if Assigned(MainForm.Project) and fInProject then begin
        if MainForm.Project.Options.useGPP then begin
          FilterIndex := 3; // .cpp
          DefaultExt := 'cpp';
        end else begin
          FilterIndex := 2; // .c
          DefaultExt := 'c';
        end;
      end else begin
        if devEditor.UseCpp then begin
          FilterIndex := 3; // .cpp
          DefaultExt := 'cpp';
        end else begin
          FilterIndex := 2; // .c
          DefaultExt := 'c';
        end;
      end;
    end;

    // Set save box options
    FileName := fFileName;
    if Assigned(MainForm.Project) then
      InitialDir := MainForm.Project.Directory
    else if (fFileName <> '') then
      InitialDir := ExtractFilePath(fFileName);

    // Open the save box
    if Execute then
      SaveFileName := FileName // prevent collision between TEditor.FileName and Dialog.FileName
    else begin
      Result := False;
      Exit;
    end;
  finally
    Free;
  end;

  MainForm.FileMonitor.UnMonitor(fFileName);
  // Remove *old* file from statement list
  if assigned(fParser) then
    fParser.InvalidateFile(FileName);

  // Try to save to disk
  try
    SaveFile(SaveFileName);
    fText.Modified := False;
    fNew := False;
  except
    MessageDlg(Lang[ID_ERR_SAVEFILE] + '"' + SaveFileName + '"', mtError, [mbOk], 0);
    Result := False;
  end;

  // Update highlighter
  devEditor.AssignEditor(fText, SaveFileName);

  if fText.Highlighter <> dmMain.Cpp then begin
    MainForm.CompilerOutput.Items.Clear;
    fErrorList.Clear;
  end;


  // Update project information
  if Assigned(MainForm.Project) and Self.InProject then begin
    UnitIndex := MainForm.Project.Units.IndexOf(FileName); // index of *old* filename
    if UnitIndex <> -1 then
      MainForm.Project.SaveUnitAs(UnitIndex, SaveFileName); // save as new filename
  end else begin
    if assigned(fTabSheet) then
      fTabSheet.Caption := ExtractFileName(SaveFileName);
  end;

  // Set new file name
  FileName := SaveFileName;
  MainForm.FileMonitor.Monitor(fFileName);
  if assigned(fParser) then
    fParser.InvalidateFile(FileName);

  // Update window captions
  MainForm.UpdateAppTitle;

  if devCodeCompletion.Enabled then begin
    // Update class browser, redraw once
    BeginUpdate;
    try
      MainForm.UpdateClassBrowserForEditor(self);
    finally
      EndUpdate;
    end;
  end;
end;

  procedure TEditor.LoadFile(FileName:String);
  var
    tmpList: TStringList;
  begin
    tmpList := TStringList.Create;
    try
      tmpList.LoadFromFile(FileName);
      if fEncodingOption = etAuto then begin
        fFileEncoding := GetFileEncodingType(tmpList.Text);
      end else begin
        fFileEncoding := fEncodingOption;
      end;
      if FileEncoding = etUTF8 then
        Text.Lines.Text := UTF8ToAnsi(tmpList.Text)
      else if FileEncoding = etUTF8Bom then begin
        Text.Lines.Text := UTF8ToAnsi(Copy(tmpList.Text,4,MaxInt));
      end else
        Text.Lines.Text := tmpList.Text;
    finally
      tmpList.Free;
    end;
    fLastIdCharPressed := 0;
  end;

  procedure TEditor.SaveFile(FileName:String);
  var
    tmpList: TStringList;
  begin
    tmpList := TStringList.Create;
    try
      if (fEncodingOption <> etAuto) and (fEncodingOption <> fFileEncoding) then begin
        fFileEncoding:=fEncodingOption;
      end;
      if (fEncodingOption = etAuto) and (fFileEncoding = etAscii) then begin
        fFileEncoding:= GetFileEncodingType(fText.Lines.Text);
        if (fFileEncoding <> etAscii) then begin
          if  not InProject and devEditor.UseUTF8ByDefault then
            fFileEncoding := etUTF8
          else if InProject and Assigned(MainForm.Project) and (MainForm.Project.Options.UseUTF8) then
            fFileEncoding := etUTF8;
        end;
        MainForm.UpdateFileEncodingStatusPanel;
      end;
      if fFileEncoding = etUTF8 then
        tmpList.Text := AnsiToUTF8(Text.Lines.Text)
      else if fFileEncoding = etUTF8Bom then
        tmpList.Text := #$EF#$BB#$BF + AnsiToUTF8(Text.Lines.Text)
      else
        tmpList.Text := Text.Lines.Text;
      tmpList.SaveToFile(FileName);
    finally
      tmpList.Free;
    end;
    fLastIdCharPressed := 0;
  end;

  procedure  TEditor.ClearUserCodeInTabStops;
  var
    p:PTabStop;
    i:integer;
  begin
    for i:=0 to fUserCodeInTabStops.Count-1 do begin
      p:=PTabStop(fUserCodeInTabStops[i]);
      dispose(PPoint(p));
    end;
    fUserCodeInTabStops.Clear;
  end;

  procedure TEditor.PopUserCodeInTabStops;
  var
      NewCursorPos: TBufferCoord;
      p:PTabStop;
      tabStopEnd:integer;
  begin
    if fTabStopBegin < 0 then begin
      ClearUserCodeInTabStops;
      Exit;
    end;
    if fUserCodeInTabStops.Count > 0 then begin
      p:=PTabStop(fUserCodeInTabStops[0]);
      // Update the cursor
      if p^.Y = 0 then begin
//        NewCursorPos.Char := fText.CaretX - fXOffsetSince + p^.X;
//        tabStopEnd := fText.CaretX - fXOffsetSince + p^.endX;
        NewCursorPos.Char := fTabStopEnd + p^.X;
        tabStopEnd := fTabStopEnd + p^.endX;
      end else begin
        NewCursorPos.Char := p^.X+1;
        tabStopEnd := p^.endX+1;
      end;
      NewCursorPos.Line := fText.CaretY + p^.Y;
      fText.CaretXY := NewCursorPos;
      dispose(p);
      
      fTabStopY:=fText.CaretY;
      fText.BlockBegin := NewCursorPos;
      NewCursorPos.Char := tabStopEnd;
      fText.BlockEnd := NewCursorPos;
      fTabStopBegin:=fText.CaretX;
      fTabStopEnd:=tabStopEnd;
      fLineBeforeTabStop:= Copy(fText.LineText,1,fTabStopBegin) ;
      fLineAfterTabStop := Copy(fText.LineText,fTabStopEnd+1,MaxInt) ;
      fXOffsetSince:=0;
      fUserCodeInTabStops.Delete(0);
    end;
  end;

procedure TEditor.ClearSyntaxErrors;
var
  i,t:integer;
  lst:TList;
begin
  for i:=0 to fErrorList.Count -1 do begin
    lst:=TList(fErrorList.Objects[i]);
    for t:=0 to lst.Count-1 do begin
      dispose(PSyntaxError(lst[t]));
    end;
    lst.Free;
  end;
  fErrorList.Clear;
  //fText.Invalidate;
end;

procedure TEditor.AddSyntaxError(line:integer; col:integer; errorType:TSyntaxErrorType; hint:String);
var
  pError:PSyntaxError;
  p:TBufferCoord;
  p1:TDisplayCoord;
  token:String;
  tokenType : TSynHighlighterTokenType;
  start:integer;
  Attri: TSynHighlighterAttributes;
  idx:integer;
  lst:TList;
begin
  if (line<1) or (line>fText.Lines.Count) then
    Exit; 
  System.new(pError);
  p.Char:=col;
  p.Line:=line;
  if col>= Length(fText.Lines[line-1]) then begin
    start := 1;
    token:=fText.Lines[line-1];
  end else begin
    fText.GetHighlighterAttriAtRowColEx(p,token,tokenType,start,Attri);
  end;
  pError^.char := start;
  pError^.endChar := start + length(token)-1;
  p.Line:=line;
  p.Char := start;
  p1:=fText.BufferToDisplayPos(p);
  pError^.col:=p1.Column;
  p.Line:=line;
  p.Char := start+length(token);
  p1:=fText.BufferToDisplayPos(p);
  pError^.endCol:=p1.Column;
  pError^.Hint:=hint;
  pError^.Token := Token;
  pError^.errorType:=errorType;
  idx:=CBUtils.FastIndexOf(fErrorList,line);
  if idx >= 0 then
    lst := TList(fErrorList.Objects[idx])
  else begin
    lst := TList.Create;
    fErrorList.AddObject(line,lst);
  end;
  lst.Add(pError);
end;

function TEditor.GetErrorAtLine(line:integer):PSyntaxError;
var
  idx:integer;
  lst:TList;
begin
  Result := nil;
  idx:=CBUtils.FastIndexOf(fErrorList,line);
  if idx >=0 then begin
    lst := TList(fErrorList.Objects[idx]);
    if lst.Count>0 then
      Result := PSyntaxError(lst[0]);
  end;
end;


function TEditor.GetErrorAtPosition(pos:TBufferCoord):PSyntaxError;
var
  idx,i:integer;
  lst:TList;
  pError:PSyntaxError;
begin
  Result := nil;
  idx:=CBUtils.FastIndexOf(fErrorList,pos.Line);
  if idx >=0 then begin
    lst := TList(fErrorList.Objects[idx]);
    for i:=0 to lst.Count-1 do begin
      pError := PSyntaxError(lst[i]);
      if (pos.Char >= pError.char) and (pos.Char <= pError.endChar) then begin
        Result := pError;
        Exit;
      end;
    end;
  end;
end;

procedure TEditor.LinesDeleted(FirstLine,Count:integer);


  procedure UpdateErrorList;
  var
    newList:TIntList;
    i,j:integer;
    lineNo:integer;
    lst:TList;
  begin
    newList:=TIntList.Create;
    try
      for i:=0 to fErrorList.Count-1 do begin
        lineNo := fErrorList[i];
        if (lineNo>=FirstLine) and (lineNo<FirstLine+Count) then begin
          lst:=TList(fErrorList.Objects[i]);
          for j:=0 to lst.Count-1 do begin
            dispose(PSyntaxError(lst[j]));
          end;
          lst.Free;
          Continue;
        end;
        if (lineNo >= FirstLine+Count) then
          dec(lineNo,Count);
        newList.AddObject(lineNo,fErrorList.Objects[i]);
      end;
      fErrorList.Sorted:=False;
      fErrorList.Assign(newList);
      fErrorList.Sorted:=True;
      if not devCodeCompletion.Enabled then begin
        BeginUpdate;
        EndUpdate;
      end;
    finally
      newList.Free;
    end;
  end;
begin
  if not devEditor.CheckSyntaxWhenReturn then begin
    UpdateErrorList;
  end;
  MainForm.CaretList.LinesDeleted(self,firstLine,count);
end;

procedure TEditor.Reparse;
begin
//  BeginUpdate;
  // Reparse whole file (not function bodies) if it has been modified
  // use stream, don't read from disk (not saved yet)
  ParseFile(fParser,fFileName, InProject, False, True);
//  EndUpdate;
end;


procedure TEditor.LinesInserted(FirstLine,Count:integer);
  procedure UpdateErrorList;
  var
    newList:TIntList;
    i:integer;
    lineNo:integer;
  begin
  newList:=TIntList.Create;
    try
      for i:=0 to fErrorList.Count-1 do begin
        lineNo := fErrorList[i];
        if (lineNo >= FirstLine) then
          inc(lineNo,Count);
        newList.AddObject(lineNo,fErrorList.Objects[i]);
      end;
      fErrorList.Sorted:=False;
      fErrorList.Assign(newList);
      fErrorList.Sorted:=True;
      if not devCodeCompletion.Enabled then begin
        BeginUpdate;
        EndUpdate;
      end;
    finally
      newList.Free;
    end;
  end;
begin
  if not devEditor.CheckSyntaxWhenReturn then begin
    UpdateErrorList;
  end;
  MainForm.CaretList.LinesInserted(self,firstLine,count);
end;

procedure TEditor.GotoNextError;
var
  idx:integer;
  lst:TList;
  p:TBufferCoord;
begin
  if fErrorList.Find(fText.CaretY,idx) then begin
    //we are on a error line;
    inc(idx);
  end;
  if idx<fErrorList.Count then begin
    lst:=TList(fErrorList.Objects[idx]);
    if lst.Count>0 then begin
      p.Line:=fErrorList[idx];
      p.Char:=PSyntaxError(lst[0])^.Char;
      fText.CaretXY:=p;
    end;
  end;
end;

procedure TEditor.GotoPrevError;
var
  idx:integer;
  lst:TList;
  p:TBufferCoord;
begin
  fErrorList.Find(fText.CaretY,idx);
  dec(idx);
  if idx>=0 then begin
    lst:=TList(fErrorList.Objects[idx]);
    if lst.Count>0 then begin
      p.Line:=fErrorList[idx];
      p.Char:=PSyntaxError(lst[0])^.Char;
      fText.CaretXY:=p;
    end;
  end;
end;

function TEditor.HasPrevError:boolean;
var
  idx:integer;
  lst:TList;
begin
  Result:=False;
  fErrorList.Find(fText.CaretY,idx);
  dec(idx);
  if idx>=0 then begin
    lst:=TList(fErrorList.Objects[idx]);
    if lst.Count>0 then begin
      Result:=True;
    end;
  end;
end;

function TEditor.HasNextError:boolean;
var
  idx:integer;
  lst:TList;
begin
  Result:=False;
  if fErrorList.Find(fText.CaretY,idx) then begin
    //we are on a error line;
    inc(idx);
  end;
  if idx<fErrorList.Count then begin
    lst:=TList(fErrorList.Objects[idx]);
    if lst.Count>0 then begin
      Result:=True;
    end;
  end;
end;

procedure TEditor.InitParser;
begin
  fParser := TCppParser.Create(MainForm.Handle);
  fParser.OnGetFileStream := MainForm.EditorList.GetStreamFromOpenedEditor;
  ResetCppParser(fParser);
  fParser.Enabled := (fText.Highlighter = dmMain.Cpp);
end;

procedure TEditor.SetInProject(inProject:boolean);
begin
  if fInProject = inProject then
    Exit;
  if fInProject then begin
    InitParser;
  end else begin
    if assigned(fParser) then begin
      FreeAndNil(fParser);
    end;
    fParser := MainForm.Project.CppParser;
    //MainForm.UpdateClassBrowserForEditor(self);
  end;
  fInProject := inProject;
end;

procedure TEditor.SetEncodingOption(encoding:TFileEncodingType);
begin
  if fEncodingOption = encoding then
    exit;
  fEncodingOption := encoding;
end;

end.

