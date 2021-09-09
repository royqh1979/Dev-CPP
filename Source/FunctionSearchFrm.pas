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

unit FunctionSearchFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CppParser, ComCtrls, CBUtils, StatementList;

type
  TFunctionSearchForm = class(TForm)
    lblSearch: TLabel;
    txtSearch: TEdit;
    lvEntries: TListView;
    procedure txtSearchChange(Sender: TObject);
    procedure lvEntriesDblClick(Sender: TObject);
    procedure lvEntriesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtSearchKeyPress(Sender: TObject; var Key: Char);
    procedure txtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
    fParser: TCppParser;
    fFileName: AnsiString;
  end;

implementation

uses
  MultiLangSupport, devcfg, utils;

{$R *.dfm}

procedure TFunctionSearchForm.txtSearchChange(Sender: TObject);
var
  Statement: PStatement;
  ScopeCommand: AnsiString;
  fileIncludes : PFileIncludes;
  i:integer;
begin
  if not Assigned(fParser) then
    Exit;

  lvEntries.Items.BeginUpdate;
  try
    lvEntries.Items.Clear;

    // For all statements...
    fileIncludes := fParser.FindFileIncludes(fFileName);
    if not Assigned(fileIncludes) then
      Exit;

    for i:=0 to fileIncludes.Statements.Count-1 do begin
      Statement := PStatement(fileIncludes.Statements[i]);
      if not assigned(statement) then
        continue;
      // Only show functions...
      if not (Statement^._Kind in [skFunction, skConstructor, skDestructor]) then
        Continue;

      // Add parent name (Foo::Bar)
      ScopeCommand := Statement^._FullName + Statement^._Args;

      // Add it if it matches the search keyword or if no keyword has been typed yet
      if (txtSearch.Text = '') or ContainsText(ScopeCommand, txtSearch.Text) then begin
        with lvEntries.Items.Add do begin
          ImageIndex := -1;
          case Statement^._ClassScope of
            scsPrivate: StateIndex := 5;
            scsProtected: StateIndex := 6;
            scsPublic: StateIndex := 7;
          end;
          SubItems.Add(Statement^._Type);
          SubItems.Add(ScopeCommand);
          if SameText(Statement^._FileName, fFileName) then
            SubItems.Add(IntToStr(Statement^._DefinitionLine))
          else
            SubItems.Add(IntToStr(Statement^._Line));
          Data := Statement;
        end;
      end;
    end;

    // Sort and set focus to the first item
    lvEntries.AlphaSort;
    if lvEntries.ItemIndex = -1 then
      if lvEntries.Items.Count > 0 then
        lvEntries.ItemIndex := 0;
  finally
    lvEntries.Items.EndUpdate;
  end;

  // without this, the user has to press the down arrow twice to
  // move down the listview entries (only the first time!)...
  lvEntries.Perform(WM_KEYDOWN, VK_DOWN, 0);
end;

procedure TFunctionSearchForm.txtSearchKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    Chr(VK_ESCAPE): begin
        ModalResult := mrCancel;
        Key := #0;
      end;
    Chr(VK_RETURN): begin
        if Assigned(lvEntries.Selected) then begin
          ModalResult := mrOK;
          Key := #0;
        end;
      end;
  end;
end;

procedure TFunctionSearchForm.txtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DOWN, VK_UP: begin
        lvEntries.Perform(WM_KEYDOWN, Key, 0); // send the key to lventries
      end;
  end;
end;

procedure TFunctionSearchForm.lvEntriesDblClick(Sender: TObject);
begin
  if Assigned(lvEntries.Selected) then
    ModalResult := mrOK;
end;

procedure TFunctionSearchForm.lvEntriesCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare:
  Integer);
begin
  Compare := CompareText(Item1.SubItems[1], Item2.SubItems[1]);
end;

procedure TFunctionSearchForm.LoadText;
var
  len: integer;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := StringReplace(Lang[ID_ITEM_GOTOFUNCTION], '&', '', []);
  lblSearch.Caption := Lang[ID_GF_TEXT];
  lvEntries.Column[1].Caption := Lang[ID_GF_TYPE];
  lvEntries.Column[2].Caption := Lang[ID_GF_FUNCTION];
  lvEntries.Column[3].Caption := Lang[ID_GF_LINE];

  // Make sure the translation fits
  len := Canvas.TextWidth(lblSearch.Caption);
  txtSearch.Left := len + 10;
  txtSearch.Width := ClientWidth - len - 14;
end;

procedure TFunctionSearchForm.FormCreate(Sender: TObject);
begin
  LoadText;
end;

procedure TFunctionSearchForm.FormShow(Sender: TObject);
begin
  txtSearchChange(self);
end;

end.

