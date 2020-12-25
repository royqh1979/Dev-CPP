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

unit filefrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, FileCtrl;

type
  TFileForm = class(TForm)
    Panel1: TPanel;
    lblSource: TLabel;
    edSource: TEdit;
    lblDest: TLabel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    edDest: TComboBox;
    LoadBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    procedure LoadBtnClick(Sender: TObject);
  private
    { Private declarations }
    Dir : boolean;
  public
    procedure SetMode(d : boolean);
    { Public declarations }
  end;
  
  function NewSelectDirectory(const Caption: string; const Root: WideString;
    var Directory: string): Boolean;
var
  FileForm: TFileForm;

implementation

uses main,shlobj,ActiveX;

{$R *.dfm}

procedure TFileForm.SetMode(d : boolean);
begin
  Dir := d;
  if dir then begin
    Caption := 'Add Directory';
  end
  else begin
    Caption := 'Add File';
  end
end;

procedure TFileForm.LoadBtnClick(Sender: TObject);
var s : string;
begin
  if Dir then begin
    if NewSelectDirectory('Choose Directory',ExtractFilePath(MainForm.FileName),s) then
      edSource.Text := ExtractRelativePath(MainForm.FileName, s);
  end
  else begin
    OpenDialog.InitialDir := ExtractFilePath(MainForm.FileName);
    if OpenDialog.Execute then
      edSource.Text := ExtractRelativePath(MainForm.FileName, OpenDialog.FileName);
  end;
end;

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), lpdata);
  result := 0;
end;

function NewSelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string): Boolean;
var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  OldErrorMode: Cardinal;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  if not DirectoryExists(Directory) then
    Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(Application.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := Application.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI;
        if Directory <> '' then
        begin
          lpfn := SelectDirCB;
          lParam := Integer(PChar(Directory));
        end;
      end;
      WindowList := DisableTaskWindows(0);
      OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        SetErrorMode(OldErrorMode);
        EnableTaskWindows(WindowList);
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

end.
