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

unit ViewToDoFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus, ExtCtrls;

type
  PToDoRec = ^TToDoRec;
  TToDoRec = record
    TokenIndex: integer;
    Filename: AnsiString;
    Line: integer;
    ToLine: integer;
    User: AnsiString;
    Priority: integer;
    Description: AnsiString;
    IsDone: boolean;
  end;

  TViewToDoForm = class(TForm)
    lv: TListView;
    btnClose: TButton;
    chkNoDone: TCheckBox;
    cmbFilter: TComboBox;
    lblFilter: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lvCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw:
      Boolean);
    procedure lvColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure lvDblClick(Sender: TObject);
    procedure chkNoDoneClick(Sender: TObject);
    procedure cmbFilterChange(Sender: TObject);
  private
    fToDoList: TList;
    fSortColumn: integer;
    function MatchesMask(SearchStr, MaskStr: AnsiString): boolean;
    procedure LoadText;
    procedure BuildList;
    procedure AddFiles(Current, InProject, NotInProject, OpenOnly: boolean);
    procedure AddToDo(Filename: AnsiString);
    function BreakupToDo(Filename: AnsiString; sl: TStrings; Line: integer; Token: AnsiString; HasUser, HasPriority:
      boolean): integer;
  end;

implementation

uses main, editor, project, StrUtils, MultiLangSupport, devcfg, utils;

{$R *.dfm}

function TViewToDoForm.BreakupToDo(Filename: AnsiString; sl: TStrings; Line: integer; Token: AnsiString; HasUser,
  HasPriority: boolean): integer;
var
  sUser: AnsiString;
  iPriority: integer;
  sDescription: AnsiString;
  Indent: integer;
  S: AnsiString;
  X, Y: integer;
  idx: integer;
  Done: boolean;
  MultiLine: boolean;
  td: PToDoRec;
  OrigLine: integer;
  TokenIndex: integer;
begin
  sUser := '';
  iPriority := 0;
  sDescription := '';

  OrigLine := Line;
  S := sl[Line];

  MultiLine := Pos('//', S) = 0;
  idx := Pos(Token, S);
  TokenIndex := idx;
  Inc(idx, 4); // skip "TODO"

  if HasUser or HasPriority then
    Inc(idx, 2); // skip " ("

  Delete(S, 1, idx - 1);
  if HasUser or HasPriority then begin
    idx := Pos('#', S);
    sUser := Copy(S, 1, idx - 1); // got user
    iPriority := StrToIntDef(S[idx + 1], 1); // got priority
  end;

  Indent := Pos(':', sl[Line]) + 1;
  idx := Pos(':', S);
  Delete(S, 1, idx + 1);
  Done := False;
  Y := Line;
  while (Y < sl.Count) and not Done do begin
    X := Indent;
    while (X <= Length(sl[Y])) and not Done do begin
      if (sl[Y][X] = '*') and (X < Length(sl[Y])) and (sl[Y][X + 1] = '/') then begin
        Done := True;
        Break;
      end;
      sDescription := sDescription + sl[Y][X];
      Inc(X);
    end;
    if not MultiLine then
      Break;
    if not Done then begin
      sDescription := sDescription + #13#10;
      Inc(Line);
    end;
    Inc(Y);
  end;

  td := New(PToDoRec);
  td^.TokenIndex := TokenIndex;
  td^.Filename := Filename;
  td^.Line := OrigLine;
  td^.ToLine := Line;
  td^.User := sUser;
  td^.Priority := iPriority;
  td^.Description := sDescription;
  td^.IsDone := CompareText(Token, 'TODO') <> 0;
  fToDoList.Add(td);

  Result := Line;
end;

procedure TViewToDoForm.AddToDo(Filename: AnsiString);
var
  sl: TStrings;
  I: integer;
  e: TEditor;
  found: boolean;
  FileEncoding : TFileEncodingType;
begin
  sl := TStringList.Create;
  try
    if MainForm.EditorList.IsFileOpened(FileName) then begin
      e:=MainForm.EditorList.GetEditorFromFileName(FileName);
      sl.Assign(e.Text.Lines);
    end else begin
      sl.LoadFromFile(Filename);
      FileEncoding := GetFileEncodingType(sl.Text);
      if FileEncoding = etUTF8 then
        sl.Text := UTF8ToAnsi(sl.Text)
      else if FileEncoding = etUTF8Bom then
        sl.Text := UTF8ToAnsi(Copy(sl.Text,4,MaxInt))
    end;
    I := 0;
    while I < sl.Count do begin
      //		if MatchesMask(sl[I], '*/? TODO ([a-z0-9_]*#[1-9]#)*:*') then
      if MatchesMask(sl[I], '*/? TODO (?*#?#)*:*') then
        BreakupToDo(Filename, sl, I, 'TODO', True, True) // full info TODO
      else if MatchesMask(sl[I], '*/? DONE (?*#?#)*:*') then
        BreakupToDo(Filename, sl, I, 'DONE', True, True) // full info DONE
      else if MatchesMask(sl[I], '*/? TODO (#?#)*:*') then
        BreakupToDo(Filename, sl, I, 'TODO', False, True) // only priority info TODO
      else if MatchesMask(sl[I], '*/? DONE (#?#)*:*') then
        BreakupToDo(Filename, sl, I, 'DONE', False, True) // only priority info DONE
      else if MatchesMask(sl[I], '*/?*TODO*:*') then
        BreakupToDo(Filename, sl, I, 'TODO', False, False) // custom TODO
      else if MatchesMask(sl[I], '*/?*DONE*:*') then
        BreakupToDo(Filename, sl, I, 'DONE', False, False); // custom DONE
      Inc(I);
    end;
  finally
    sl.Free;
  end;
end;

procedure TViewToDoForm.AddFiles(Current, InProject, NotInProject, OpenOnly: boolean);
var
  e: TEditor;
  I: integer;
begin
  if Current then begin
    e := MainForm.EditorList.GetEditor;
    if Assigned(e) then
      AddToDo(e.FileName);
    Exit;
  end;

  if InProject and not OpenOnly then begin
    if Assigned(MainForm.Project) then
      for I := 0 to pred(MainForm.Project.Units.Count) do
        AddToDo(MainForm.Project.Units[I].filename);
  end;

  if OpenOnly then begin
    for I := 0 to pred(MainForm.EditorList.PageCount) do begin
      e := MainForm.EditorList[i];
      if Assigned(e) then
        if InProject and e.InProject then
          AddToDo(e.FileName)
    end;
  end;

  if NotInProject then begin
    for I := 0 to pred(MainForm.EditorList.PageCount) do begin
      e := MainForm.EditorList[i];
      if Assigned(e) and not e.InProject then
          AddToDo(e.FileName);
    end;
  end;
end;

procedure TViewToDoForm.FormShow(Sender: TObject);
begin
  cmbFilter.ItemIndex := 5;
  fToDoList.Clear;
  lv.Items.Clear;
  AddFiles(True, False, True, True); // default is current only
  BuildList;
end;

procedure TViewToDoForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TViewToDoForm.FormCreate(Sender: TObject);
begin
  fToDoList := TList.Create;
  fSortColumn := 0;
  LoadText;
end;

procedure TViewToDoForm.FormDestroy(Sender: TObject);
begin
  while fToDoList.Count > 0 do
    if Assigned(fToDoList[0]) then begin
      Dispose(PToDoRec(fToDoList[0]));
      fToDoList.Delete(0);
    end;
  fToDoList.Clear;
  fToDoList.Free;
end;

procedure TViewToDoForm.lvCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if Assigned(Item.Data) then
    if PToDoRec(Item.Data)^.IsDone then
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsStrikeOut]
    else
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style - [fsStrikeOut];
  DefaultDraw := True;
end;

procedure TViewToDoForm.lvCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Assigned(Item.Data) then
    Item.Checked := PToDoRec(Item.Data)^.IsDone;
  DefaultDraw := True;
end;

procedure TViewToDoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TViewToDoForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_VIEWTODO_MENUITEM];

  lv.Column[0].Caption := Lang[ID_VIEWTODO_DONE];
  lv.Column[1].Caption := Lang[ID_ADDTODO_PRIORITY];
  lv.Column[2].Caption := Lang[ID_ADDTODO_DESCRIPTION];
  lv.Column[3].Caption := Lang[ID_VIEWTODO_FILENAME];
  lv.Column[4].Caption := Lang[ID_ADDTODO_USER];

  chkNoDone.Caption := Lang[ID_VIEWTODO_NOSHOWDONE];
  btnClose.Caption := Lang[ID_BTN_CLOSE];
  lblFilter.Caption := Lang[ID_VIEWTODO_FILTER];
  cmbFilter.Items[0] := Lang[ID_VIEWTODO_FILTERONE];
  cmbFilter.Items[1] := Lang[ID_VIEWTODO_FILTERTWO];
  cmbFilter.Items[2] := Lang[ID_VIEWTODO_FILTERTHREE];
  cmbFilter.Items[3] := Lang[ID_VIEWTODO_FILTERFOUR];
  cmbFilter.Items[4] := Lang[ID_VIEWTODO_FILTERFIVE];
  cmbFilter.Items[5] := Lang[ID_VIEWTODO_FILTERSIX];
end;

procedure TViewToDoForm.lvMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  e: TEditor;
  Item: TListItem;
begin
  if not (htOnStateIcon in lv.GetHitTestInfoAt(X, Y)) then
    Exit;

  Item := lv.GetItemAt(X, Y);
  if not Assigned(Item) then
    Exit;
  if not Assigned(Item.Data) then
    Exit;

  e := MainForm.EditorList.GetEditorFromFileName(PToDoRec(Item.Data)^.Filename);
  if Assigned(e) then begin
    PToDoRec(Item.Data)^.IsDone := Item.Checked;
    if Item.Checked then begin
      e.Text.Lines[PToDoRec(Item.Data)^.Line] :=
        StringReplace(e.Text.Lines[PToDoRec(Item.Data)^.Line], 'TODO', 'DONE', []);
      if chkNoDone.Checked then
        BuildList;
    end else
      e.Text.Lines[PToDoRec(Item.Data)^.Line] :=
        StringReplace(e.Text.Lines[PToDoRec(Item.Data)^.Line], 'DONE', 'TODO', []);
    e.Text.Modified := True;
    lv.Refresh;
  end;
end;

procedure TViewToDoForm.lvColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  fSortColumn := Column.Index;
  TCustomListView(Sender).CustomSort(nil, 0);
end;

procedure TViewToDoForm.lvCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  idx: Integer;
begin
  if fSortColumn = 0 then begin
    if PToDoRec(Item1.Data)^.IsDone and not PToDoRec(Item2.Data)^.IsDone then
      Compare := 1
    else if not PToDoRec(Item1.Data)^.IsDone and PToDoRec(Item2.Data)^.IsDone then
      Compare := -1
    else
      Compare := 0;
  end else begin
    idx := fSortColumn - 1;
    Compare := CompareText(Item1.SubItems[idx], Item2.SubItems[idx]);
  end;
end;

procedure TViewToDoForm.lvDblClick(Sender: TObject);
var
  e: TEditor;
begin
  if not Assigned(lv.Selected) then
    Exit;
  if not Assigned(lv.Selected.Data) then
    Exit;

  e := MainForm.EditorList.GetEditorFromFilename(PToDoRec(lv.Selected.Data)^.Filename);
  if Assigned(e) then begin
    e.SetCaretPosAndActivate(PToDoRec(lv.Selected.Data)^.Line + 1, 1);
    Close;
  end;
end;

procedure TViewToDoForm.chkNoDoneClick(Sender: TObject);
begin
  BuildList;
end;

procedure TViewToDoForm.BuildList;
var
  I: integer;
  td: PToDoRec;
  S: AnsiString;
begin
  lv.Items.BeginUpdate;
  lv.Items.Clear;
  for I := 0 to fToDoList.Count - 1 do begin
    td := PToDoRec(fToDoList[I]);
    if (chkNoDone.Checked and not td^.IsDone) or not chkNoDone.Checked then
      with lv.Items.Add do begin
        Caption := '';
        SubItems.Add(IntToStr(td^.Priority));
        S := StringReplace(td^.Description, #13#10, ' ', [rfReplaceAll]);
        S := StringReplace(S, #9, ' ', [rfReplaceAll]);
        SubItems.Add(S);
        SubItems.Add(td^.Filename);
        SubItems.Add(td^.User);
        Data := td;
      end;
  end;
  lv.CustomSort(nil, 0);
  lv.Items.EndUpdate;
end;

function TViewToDoForm.MatchesMask(SearchStr, MaskStr: AnsiString): boolean;
var
  Matches: boolean;
  MaskIndex: integer;
  SearchIndex: integer;
  NextMatch: Char;
begin
  Matches := True;
  MaskIndex := 1;
  SearchIndex := 1;

  if (MaskStr = '') or (SearchStr = '') then
    Matches := False;

  if Pos('*?', MaskStr) > 0 then // illegal
    Matches := False;
  if Pos('**', MaskStr) > 0 then // illegal
    Matches := False;

  while Matches do begin
    case MaskStr[MaskIndex] of
      '*': begin
          if MaskIndex < Length(MaskStr) then
            NextMatch := MaskStr[MaskIndex + 1]
          else
            NextMatch := #0;
          while SearchIndex <= Length(SearchStr) do begin
            if SearchStr[SearchIndex] = NextMatch then begin
              Inc(SearchIndex);
              Inc(MaskIndex, 2);
              Break;
            end;
            Inc(SearchIndex);
          end;
          if (SearchIndex = Length(SearchStr)) and (MaskIndex < Length(MaskStr)) then
            Matches := False;
        end;
      '?': begin
          Inc(SearchIndex);
          Inc(MaskIndex);
        end;
    else
      if MaskStr[MaskIndex] <> SearchStr[SearchIndex] then
        Matches := False
      else begin
        Inc(MaskIndex);
        Inc(SearchIndex);
      end;
    end;

    if MaskIndex > Length(MaskStr) then
      Break;
    if SearchIndex > Length(SearchStr) then
      Break;
  end;
  if (MaskIndex = Length(MaskStr)) and (MaskStr[MaskIndex] = '*') then
    MaskIndex := Length(MaskStr) + 1;

  Result := Matches and (MaskIndex > Length(MaskStr)) and (SearchIndex > Length(SearchStr));
end;

procedure TViewToDoForm.cmbFilterChange(Sender: TObject);
begin
  {
   0 = All files (in project and not)
   1 = Open files only (in project and not)
   2 = All project files
   3 = Open project files only
   4 = Non-project open files
   5 = Current file only
  }
  fToDoList.Clear;
  lv.Items.Clear;
  case cmbFilter.ItemIndex of
    0: AddFiles(False, True, True, False);
    1: AddFiles(False, True, True, True);
    2: AddFiles(False, True, False, False);
    3: AddFiles(False, True, False, True);
    4: AddFiles(False, False, True, True);
    5: AddFiles(True, False, True, True);
  else
    AddFiles(True, False, True, True); // default is the same with 5
  end;
  BuildList;
end;

end.

