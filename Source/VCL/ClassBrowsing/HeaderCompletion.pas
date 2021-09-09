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

unit HeaderCompletion;

interface

uses
{$IFDEF WIN32}
  Windows, Classes, Forms, SysUtils, Controls, Graphics, CppParser,
  cbutils, StatementList, iniFiles;
{$ENDIF}
{$IFDEF LINUX}
Xlib, Classes, QForms, SysUtils, QControls, QGraphics, CppParser,
U_IntList, QDialogs, Types;
{$ENDIF}

type
  THeaderCompletion = class(TComponent)
  private
    fParser: TCppParser;
    fFullCompletionList: TStringList;
    fCompletionList: TStringList;
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
    fShowCount: integer;
    fOnKeyPress: TKeyPressEvent;
    fOnKeyDown: TKeyEvent;
    fOnResize: TNotifyEvent;
    fAddedFilenames : TStringHash;
    fPreparing: boolean;
    fPhrase : AnsiString;
    fIgnoreCase:boolean;
    fSearchLocal:boolean;
    fCurrentFile: String;
    procedure GetCompletionFor(FileName, Phrase: AnsiString);
    procedure FilterList(const Member: AnsiString);
    procedure SetPosition(Value: TPoint);
    procedure OnFormResize(Sender: TObject);
    function IsVisible: boolean;
    function GetColor(i:integer):TColor;
    procedure SetColor(i:integer; const Color:TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareSearch(const Phrase, Filename: AnsiString);
    function Search(const Phrase, Filename: AnsiString; AutoHideOnSingleResult:boolean):boolean;
    procedure Hide;
    procedure Show;
    function SelectedFilename: String;
    property Colors[Index: Integer]: TColor read GetColor write SetColor;
    property IgnoreCase: boolean read fIgnoreCase write fIgnoreCase;
    property Parser: TCppParser read fParser write fParser;
    property searchLocal: boolean read fSearchLocal write fSearchLocal;
    property CurrentFile: string read fCurrentFile write fCurrentFile;
  published
    property ShowCount: integer read fShowCount write fShowCount;
    property Position: TPoint read fPos write SetPosition;
    property Color: TColor read fColor write fColor;
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
    property Enabled: boolean read fEnabled write fEnabled;
    property MinWidth: integer read fMinWidth write fMinWidth;
    property MinHeight: integer read fMinHeight write fMinHeight;
    property MaxWidth: integer read fMaxWidth write fMaxWidth;
    property MaxHeight: integer read fMaxHeight write fMaxHeight;
    property FontSize: integer read fFontSize write fFontSize;
    property OnKeyDown: TKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnKeyPress: TKeyPressEvent read fOnKeyPress write fOnKeyPress;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
    property Visible: boolean read IsVisible;
  end;

procedure Register;

implementation

uses
  HeaderCompletionForm, Math;

{ THeaderCompletion }

procedure Register;
begin
  RegisterComponents('Dev-C++', [THeaderCompletion]);
end;

constructor THeaderCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fAddedFilenames := TStringHash.Create(500);
  fCompletionList := TStringList.Create;
  fFullCompletionList := TStringList.Create;
  fSearchLocal := False;
  fCurrentFile := '';

  HeaderComplForm := THeaderComplForm.Create(Self);
  HeaderComplForm.OnResize := OnFormResize;

  fWidth := 320;
  fHeight := 240;
  fColor := clWindow;
  fEnabled := True;
  fShowCount := 1000;

  fIgnoreCase := false;
end;

destructor THeaderCompletion.Destroy;
begin
  FreeAndNil(HeaderComplForm);
  FreeAndNil(fCompletionList);
  FreeAndNil(fFullCompletionList);
  FreeAndNil(fAddedFilenames);
  inherited Destroy;
end;


procedure THeaderCompletion.GetCompletionFor(FileName,Phrase: AnsiString);
var
  I,idx: integer;
  remainder,current,founddir: String;
  searchResult : TSearchRec;
  ext:string;

  procedure AddFile(word:String);
  begin
    if StartsStr('.',word) then
      Exit;
    if fAddedFileNames.ValueOf(word)<0 then begin
      fFullCompletionList.Add(word);
      fAddedFileNames.Add(word,1);
    end;
  end;

  procedure AddFilesInPath(path:String);
  begin
    path:= IncludeTrailingPathDelimiter(path);
    if FindFirst( path + '/*.*', faAnyFile,searchResult) = 0 then begin
      repeat
        ext:= ExtractFileExt(searchResult.Name);
        if Directoryexists(path +searchResult.Name) or SameText('.h',ext)
          or SameText('.hpp',ext) or (ext = '') then begin
          AddFile(searchResult.Name);
        end;
      until FindNext(searchResult) <> 0;
      FindClose(searchResult);
    end;
  end;

  function FindDirInPath(path, DirName:String):String;
  begin
    Result:='';
    path:= IncludeTrailingPathDelimiter(path);
    if Directoryexists(path+DirName) then
      Result:=path+DirName;
  end;
begin
  idx := LastPos('\', Phrase);
  if (idx = 0) then begin
    if searchLocal then begin
      AddFilesInPath(ExtractFilePath(fCurrentFile));
    end;
    for i:=0 to fParser.IncludePaths.Count-1 do begin
      AddFilesInPath(fParser.IncludePaths[i]);
    end;

    for i:=0 to fParser.ProjectIncludePaths.Count-1 do begin
      AddFilesInPath(fParser.ProjectIncludePaths[i]);
    end;
  end else begin
    current := Copy(Phrase,1,idx-1);
    remainder := Copy(Phrase,idx+1,MaxInt);

    if searchLocal then begin
      founddir:=FindDirInPath(ExtractFilePath(fCurrentFile),current);
      if founddir<>'' then  begin
        AddFilesInPath(founddir);
      end;
    end;

    for i:=0 to fParser.IncludePaths.Count-1 do begin
      founddir:=FindDirInPath(fParser.IncludePaths[i],current);
      if founddir<>'' then  begin
        AddFilesInPath(founddir);
      end;
    end;

    for i:=0 to fParser.ProjectIncludePaths.Count-1 do begin
      founddir:=FindDirInPath(fParser.ProjectIncludePaths[i],current);
      if founddir<>'' then  begin
        AddFilesInPath(founddir);
      end;
    end;

  end;
end;


procedure THeaderCompletion.FilterList(const Member: AnsiString);
var
  I: integer;
  tmpList:TStringList;
//  lastCmd:String;

begin
  fCompletionList.Clear;
  fCompletionList.Sorted := False;
  tmpList:=fCompletionList;
    if Member <> '' then begin // filter, case sensitive
      tmpList.Capacity := fFullCompletionList.Count;
      for I := 0 to fFullCompletionList.Count - 1 do
        if ignoreCase and StartsText(Member, fFullCompletionList[I]) then begin
          tmpList.Add(fFullCompletionList[I]);
        end else if StartsStr(Member, fFullCompletionList[I]) then
          tmpList.Add(fFullCompletionList[I]);
    end else
      tmpList.Assign(fFullCompletionList);
  tmpList.Sorted := True;
end;


procedure THeaderCompletion.Show;
begin
  fPreparing:=True;
  // Clear data, do not free pointed memory: data is owned by CppParser
  fAddedFilenames.Clear;
  fCompletionList.Clear;
  fFullCompletionList.Clear;
  HeaderComplForm.lbCompletion.Items.BeginUpdate;
  HeaderComplForm.lbCompletion.Items.Clear;
  HeaderComplForm.lbCompletion.Items.EndUpdate;
  HeaderComplForm.Show;
  HeaderComplForm.lbCompletion.SetFocus;
end;

procedure THeaderCompletion.Hide;
begin
  if fPreparing then
    Exit;
  OnKeyPress := nil;
  HeaderComplForm.Hide;
  // Clear data, do not free pointed memory: data is owned by CppParser
  fCompletionList.Clear;
  fFullCompletionList.Clear;
  HeaderComplForm.lbCompletion.Items.BeginUpdate;
  HeaderComplForm.lbCompletion.Items.Clear;
  HeaderComplForm.lbCompletion.Items.EndUpdate;
  fAddedFilenames.Clear;
end;

procedure THeaderCompletion.PrepareSearch(const Phrase, Filename: AnsiString);
begin
  fPreparing:=True;
  fPhrase := StringReplace(Phrase,'/','\',[rfReplaceAll]);
  Screen.Cursor := crHourglass;
  GetCompletionFor(FileName,fPhrase);
  HeaderComplForm.lbCompletion.Font.Size := FontSize;
  HeaderComplForm.lbCompletion.ItemHeight := HeaderComplForm.lbCompletion.Canvas.TextHeight('F')+6;
  // Round(2 * FontSize);
  HeaderComplForm.Update;
  Screen.Cursor := crDefault;
  fPreparing:=False;
end;

function THeaderCompletion.Search(const Phrase, Filename: AnsiString;AutoHideOnSingleResult:boolean):boolean;
var
  I: integer;
  symbol: ansistring;
begin
  Result:=False;
  fPhrase := StringReplace(Phrase,'/','\',[rfReplaceAll]);
  if fPhrase = '' then begin
    Hide;
    Exit;
  end;

  if fEnabled then begin
    Screen.Cursor := crHourglass;

    I := LastPos('\',fPhrase);
    if I>0 then
      symbol := Copy(fPhrase, I+1, MaxInt)
    else
      symbol := fPhrase;

    // filter fFullCompletionList to fCompletionList
    FilterList(symbol);

    if fCompletionList.Count > 0 then begin
      HeaderComplForm.lbCompletion.Items.BeginUpdate;
      try
        HeaderComplForm.lbCompletion.Items.Clear;

        // Only show one hundred statements...
        for I := 0 to min(fShowCount, fCompletionList.Count - 1) do begin
          HeaderComplForm.lbCompletion.Items.AddObject('',TObject(fCompletionList[I]));
        end;

        if HeaderComplForm.lbCompletion.Items.Count > 0 then begin
          HeaderComplForm.lbCompletion.ItemIndex :=0;
      end;
      finally
        HeaderComplForm.lbCompletion.Items.EndUpdate;
        Screen.Cursor := crDefault;
      end;

      // if only one suggestion, and is exactly the symbol to search, hide the frame (the search is over)
      if (fCompletionList.Count =1) and
        SameStr(symbol, fCompletionList[0]) then begin
        Result:=True;
        Exit;
      end;

      // if only one suggestion and auto hide , don't show the frame
      if (fCompletionList.Count =1) and AutoHideOnSingleResult then begin
        Result:=True;
        Exit;
      end;

    end else begin
      HeaderComplForm.lbCompletion.Items.Clear;
      Hide;
    end;

    Screen.Cursor := crDefault;
  end;
end;

function THeaderCompletion.SelectedFilename: String;
begin
  if fEnabled then begin
    if (fCompletionList.Count > HeaderComplForm.lbCompletion.ItemIndex) and (HeaderComplForm.lbCompletion.ItemIndex
      <> -1) then begin
      Result := fCompletionList[HeaderComplForm.lbCompletion.ItemIndex];
    end else begin
      if fCompletionList.Count > 0 then
        Result := fCompletionList[0]
      else
        Result := '';
    end;
  end else
    Result := '';
end;

procedure THeaderCompletion.SetPosition(Value: TPoint);
begin
  fPos := Value;
  if (fPos.X + fWidth > Screen.Width) and (fPos.X - fWidth>0) then
    HeaderComplForm.Left := fPos.X - fWidth
  else
    HeaderComplForm.Left := fPos.X;
  if fPos.Y + fHeight > Screen.Height then
    HeaderComplForm.Top := fPos.Y - fHeight - 16
  else
    HeaderComplForm.Top := fPos.Y;
end;

procedure THeaderCompletion.OnFormResize(Sender: TObject);
begin
  if Enabled then begin
    fWidth := HeaderComplForm.Width;
    fHeight := HeaderComplForm.Height;
    if Assigned(fOnResize) then
      fOnResize(Self);
  end;
end;

function THeaderCompletion.IsVisible: boolean;
begin
  Result := fEnabled and HeaderComplForm.Visible;
end;

function THeaderCompletion.GetColor(i:integer):TColor;
begin
  Result := HeaderComplForm.Colors[i];
end;

procedure THeaderCompletion.SetColor(i:integer; const Color:TColor);
begin
  HeaderComplForm.Colors[i] := Color;
end;

end.

