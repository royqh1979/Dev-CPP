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

unit CPUFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Math, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SynEdit, SynEditTypes, ClipBrd, StrUtils, ComCtrls, ExtCtrls, Menus;

type
  TCPUForm = class(TForm)
    lblFunc: TLabel;
    CodeList: TSynEdit;
    RegisterListbox: TListView;
    DisasPanel: TPanel;
    RadioATT: TRadioButton;
    RadioIntel: TRadioButton;
    CPUPopup: TPopupMenu;
    CPUCopy: TMenuItem;
    CPUCopyAll: TMenuItem;
    CPUPaste: TMenuItem;
    CPUCut: TMenuItem;
    N2: TMenuItem;
    CPUSelectAll: TMenuItem;
    RegPanel: TPanel;
    LeftPanel: TPanel;
    edFunc: TEdit;
    VertSplit: TSplitter;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure gbSyntaxClick(Sender: TObject);
    procedure CPUCopyClick(Sender: TObject);
    procedure CPUCopyAllClick(Sender: TObject);
    procedure CPUPasteClick(Sender: TObject);
    procedure CPUCutClick(Sender: TObject);
    procedure CPUSelectAllClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateInfo;
  private
    fRegisters: TList;
    fAssembler: TStringList;

    procedure LoadText;
  public
    procedure OnAssemblerReady;
    procedure OnRegistersReady;
  end;

var
  CPUForm: TCPUForm = nil;

implementation

uses
  main, version, MultiLangSupport, debugger, debugreader, DataFrm, utils,
  devcfg, editor, Types;

{$R *.dfm}

procedure TCPUForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I: integer;
begin
  if Assigned(fRegisters) then begin
    for I := 0 to fRegisters.Count - 1 do
      Dispose(PRegister(fRegisters.Items[I]));
    fRegisters.Free;
  end;
  fRegisters := nil;
  if Assigned(fAssembler) then
    fAssembler.Free;
  fAssembler := nil;

  // Clear contents of the debug reader
  if MainForm.Debugger.Executing and Assigned(MainForm.Debugger.Reader) then begin
    MainForm.Debugger.Reader.Registers := nil;
    MainForm.Debugger.Reader.Disassembly := nil;
  end;

  // Save column widths of registerbox
  devData.CPURegisterCol1 := RegisterListbox.Column[0].Width;
  devData.CPURegisterCol2 := RegisterListbox.Column[1].Width;
  devData.CPURegisterCol3 := RegisterListbox.Column[2].Width;

  Action := caFree;
  CPUForm := nil;
end;


procedure TCPUForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_CPU_CAPTION];
  lblFunc.Caption := Lang[ID_CPU_FUNC];
  CPUCut.Caption := Lang[ID_ITEM_CUT];
  CPUCopy.Caption := Lang[ID_ITEM_COPY];
  CPUCopyAll.Caption := Lang[ID_ITEM_COPYALL];
  CPUPaste.Caption := Lang[ID_ITEM_PASTE];
  CPUSelectAll.Caption := Lang[ID_ITEM_SELECTALL];
end;

procedure TCPUForm.OnAssemblerReady;
var
  I, activeline: integer;
begin
  activeline := -1;
  edFunc.Text := fAssembler.Strings[0];

  CodeList.BeginUpdate;
  try
    CodeList.Clear;
    for I := 1 to fAssembler.Count - 1 do begin
      CodeList.Lines.Add(fAssembler.Strings[i]);
      if StartsStr('=>', fAssembler.Strings[i]) then
        activeline := i + 1;
    end;
  finally
    CodeList.EndUpdate;
  end;

  // Free list for reuse
  fAssembler.Clear;

  if activeline <> -1 then
    CodeList.CaretXY := BufferCoord(1, activeline);
end;

procedure TCPUForm.OnRegistersReady;
var
  item: TListItem;
  I: integer;
begin
  RegisterListbox.Items.BeginUpdate;
  RegisterListBox.Clear;
  for I := 0 to fRegisters.Count - 1 do begin
    item := RegisterListbox.Items.Add;
    item.Caption := UpperCase(PRegister(fRegisters.Items[I])^.name);
    item.SubItems.Add(PRegister(fRegisters.Items[I])^.valuehex);
    item.SubItems.Add(PRegister(fRegisters.Items[I])^.valuedec);
  end;
  RegisterListBox.Items.EndUpdate;

  // Free list for reuse
  for I := 0 to fRegisters.Count - 1 do
    Dispose(PRegister(fRegisters.Items[I]));
  fRegisters.Clear;
end;

procedure TCPUForm.UpdateInfo;
begin
  if MainForm.Debugger.Executing then begin

    // Load the registers...
    MainForm.Debugger.Reader.Registers := fRegisters;
    MainForm.Debugger.SendCommand('info', 'registers');

    // Set disassembly flavor and load the current function
    MainForm.Debugger.Reader.Disassembly := fAssembler;
    //if devData.UseATTSyntax then // gbSyntaxClick has NOT been called yet...
    gbSyntaxClick(nil);

    // Obtain stack trace too
    MainForm.Debugger.SendCommand('backtrace', '');
  end;
end;

procedure TCPUForm.FormCreate(Sender: TObject);
begin
  LoadText;

  // Make it look a bit like a regular editor
  devEditor.AssignEditor(CodeList,'main.cpp');

  RadioATT.Checked := devData.UseATTSyntax;
  RadioIntel.Checked := not devData.UseATTSyntax;

  fRegisters := TList.Create;
  fAssembler := TStringList.Create;
  UpdateInfo;
end;

procedure TCPUForm.gbSyntaxClick(Sender: TObject);
begin
  // Set disassembly flavor
  if RadioAtt.Checked then begin
    MainForm.Debugger.SendCommand('set disassembly-flavor', 'att');
    MainForm.Debugger.SendCommand('disas','');
    RadioIntel.Checked := false;
    devData.UseATTSyntax := true;
  end else if RadioIntel.Checked then begin
    MainForm.Debugger.SendCommand('set disassembly-flavor', 'intel');
    MainForm.Debugger.SendCommand('disas','');
    RadioAtt.Checked := false;
    devData.UseATTSyntax := false;
  end;

end;

procedure TCPUForm.CPUCutClick(Sender: TObject);
begin
  if edFunc.Focused then begin
    ClipBoard.AsText := edFunc.SelText;
    edFunc.Text := '';
  end;
end;

procedure TCPUForm.CPUCopyClick(Sender: TObject);
begin
  if edFunc.Focused then
    ClipBoard.AsText := edFunc.SelText
  else if CodeList.Focused then
    CodeList.CopyToClipboard
  else if RegisterListbox.Focused then
    Clipboard.AsText := GetPrettyLine(RegisterListbox);
end;

procedure TCPUForm.CPUCopyAllClick(Sender: TObject);
var
  i: integer;
begin
  if edFunc.Focused then
    ClipBoard.AsText := edFunc.Text
  else if CodeList.Focused then
    CodeList.CopyToClipboard
  else if RegisterListbox.Focused then begin
    ClipBoard.AsText := '';
    for i := 0 to pred(RegisterListbox.Items.Count) do
      Clipboard.AsText := Clipboard.AsText + GetPrettyLine(RegisterListbox, i) + #13#10;
  end;
end;

procedure TCPUForm.CPUPasteClick(Sender: TObject);
begin
  if edFunc.Focused then
    edFunc.SelText := ClipBoard.AsText;
end;

procedure TCPUForm.CPUSelectAllClick(Sender: TObject);
begin
  if edFunc.Focused then
    edFunc.SelectAll;
end;

procedure TCPUForm.FormShow(Sender: TObject);
begin
  // Get column widths of registerbox
  RegisterListbox.Column[0].Width := devData.CPURegisterCol1;
  RegisterListbox.Column[1].Width := devData.CPURegisterCol2;
  RegisterListbox.Column[2].Width := devData.CPURegisterCol3;
end;

end.

