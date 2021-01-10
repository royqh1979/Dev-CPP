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

unit CodeInsList;

interface

uses
  Windows, Classes, cbutils;

type

  TCodeInsList = class(TObject)
  private
    fFile: AnsiString;
    fList: TList;
    procedure SetItem(index: integer; Value: PCodeIns);
    function GetItem(index: integer): PCodeIns;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadCode;
    procedure SaveCode;
    function AddItem(Value: PCodeIns): integer; overload;
    procedure AddItem(const menutext, prefix,description, code: AnsiString; section: integer); overload;
    procedure Delete(index: integer);
    procedure Clear;
    property ItemList: TList read fList;
    property Items[index: integer]: PCodeins read GetItem write SetItem; default;
    property Count: integer read GetCount;
  end;

implementation

uses
  SysUtils, IniFiles, devCFG, Utils, version, MultiLangSupport;

{ TCodeInsList }

constructor TCodeInsList.Create;
begin
  inherited Create;
  fList := TList.Create;
  fFile := devDirs.Config + DEV_CODEINS_FILE;
end;

destructor TCodeInsList.Destroy;
var
  I: integer;
begin
  for I := 0 to fList.Count - 1 do
    Dispose(PCodeIns(fList[I]));
  fList.Free;
  inherited Destroy;
end;

function TCodeInsList.AddItem(Value: PCodeIns): integer;
begin
  result := fList.Add(Value);
end;

procedure TCodeInsList.AddItem(const menutext, prefix,description, code: AnsiString; section: integer);
var
  assembleditem: PCodeIns;
begin
  new(assembleditem);
  assembleditem^.Caption := menutext;
  assembleditem^.Prefix := prefix;
  assembleditem^.Code := code;
  assembleditem^.Desc := description;
  assembleditem^.Section := section;
  fList.Add(assembleditem);
end;

procedure TCodeInsList.Clear;
var
  I: integer;
begin
  for I := 0 to fList.Count - 1 do
    Dispose(PCodeIns(fList[I])); // Free pointed memory first!
  fList.Clear;
end;

procedure TCodeInsList.Delete(index: integer);
begin
  if (index < 0) or (index > fList.Count - 1) then
    exit;

  // Free pointed memory first!
  Dispose(PCodeIns(fList[index]));
  fList.Delete(index);
end;

function TCodeInsList.GetCount: integer;
begin
  result := fList.Count;
end;

function TCodeInsList.GetItem(index: integer): PCodeIns;
begin
  if (index < 0) or (index > fList.Count - 1) then
    result := nil
  else
    result := PCodeIns(fList[index]);
end;

procedure TCodeInsList.SetItem(index: integer; Value: PCodeIns);
begin
  fList[index] := Value;
end;

procedure TCodeInsList.LoadCode;

  procedure LoadFromFile(fileName:String);
  var
    Item: PCodeIns;
    tmp: TStringList;
    I: integer;
  begin
    if not FileExists(fileName) then
      Exit;
    with TINIFile.Create(fileName) do try
      tmp := TStringList.Create;
      Clear;
      try
        ReadSections(tmp);
        for I := 0 to tmp.Count - 1 do begin
          new(Item);
          Item^.Caption := StringReplace(tmp[I], '_', ' ', [rfReplaceAll]);
          Item^.Desc := ReadString(tmp[I], 'Desc', '');
          Item^.Prefix := ReadString(tmp[I], 'Prefix', '');
          Item^.Code := StrtoCodeIns(ReadString(tmp[I], 'Code', ''));
          Item^.Section := ReadInteger(tmp[I], 'Section', 0);
          AddItem(Item);
        end;
      finally
        tmp.free;
      end;
    finally
      free;
    end;
  end;
begin
  if devData.First then begin
    LoadFromFile(devDirs.Exec + '\Contributes\codeins\codeinsertion.ini');
    SaveCode;
  end else begin // no first time launch? load from disk
    LoadFromFile(fFile);
  end;
end;

procedure TCodeInsList.SaveCode;
var
  I: integer;
  section: AnsiString;
  item: PCodeIns;
begin
  DeleteFile(fFile);
  if fList.Count = 0 then
    Exit;

  with TINIFile.Create(fFile) do try
    for I := 0 to pred(fList.Count) do begin
      item := PCodeIns(fList[I]);
      section := StringReplace(item^.Caption, ' ', '_', [rfReplaceAll]);
      WriteString(section, 'Desc', item^.Desc);
      WriteString(section, 'Prefix', item^.Prefix);
      WriteString(section, 'Code', CodeInstoStr(item^.Code));
      WriteInteger(section, 'Section', item^.Section);
    end;
  finally
    Free;
  end;
end;

end.

