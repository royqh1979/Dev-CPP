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

unit AutoLinkList;

interface

uses
  Windows, Classes, cbutils;

type

  PAutoLink = ^TAutoLink;

  TAutoLink = record
    header: AnsiString;
    linkParams: AnsiString;   
  end;

  TAutoLinkList = class(TObject)
  private
    fFile: AnsiString;
    fList: TList;
    procedure SetItem(index: integer; Value: PAutoLink);
    function GetItem(index: integer): PAutoLink;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;
    function AddItem(Value: PAutoLink): integer; overload;
    procedure AddItem(const header, linkParams: AnsiString); overload;
    procedure Delete(index: integer);
    procedure Clear;
    property ItemList: TList read fList;
    property Items[index: integer]: PAutoLink read GetItem write SetItem; default;
    property Count: integer read GetCount;
  end;

implementation

uses
  SysUtils, IniFiles, devCFG, Utils, version, MultiLangSupport;

{ TAutoLinkList }

constructor TAutoLinkList.Create;
begin
  inherited Create;
  fList := TList.Create;
  fFile := devDirs.Config + DEV_AUTOLINK_FILE;
end;

destructor TAutoLinkList.Destroy;
var
  I: integer;
begin
  for I := 0 to fList.Count - 1 do
    Dispose(PAutoLink(fList[I]));
  fList.Free;
  inherited Destroy;
end;

function TAutoLinkList.AddItem(Value: PAutoLink): integer;
begin
  result := fList.Add(Value);
end;

procedure TAutoLinkList.AddItem(const header, linkParams: AnsiString);
var
  item: PAutoLink;
begin
  new(item);
  item^.header := header;
  item^.linkParams := linkParams;
  fList.Add(item);
end;

procedure TAutoLinkList.Clear;
var
  I: integer;
begin
  for I := 0 to fList.Count - 1 do
    Dispose(PAutoLink(fList[I])); // Free pointed memory first!
  fList.Clear;
end;

procedure TAutoLinkList.Delete(index: integer);
begin
  if (index < 0) or (index > fList.Count - 1) then
    exit;

  // Free pointed memory first!
  Dispose(PAutoLink(fList[index]));
  fList.Delete(index);
end;

function TAutoLinkList.GetCount: integer;
begin
  result := fList.Count;
end;

function TAutoLinkList.GetItem(index: integer): PAutoLink;
begin
  if (index < 0) or (index > fList.Count - 1) then
    result := nil
  else
    result := PAutoLink(fList[index]);
end;

procedure TAutoLinkList.SetItem(index: integer; Value: PAutoLink);
begin
  fList[index] := Value;
end;

procedure TAutoLinkList.Load;
var
  tmp: TStringList;
  header, linkParams:AnsiString;
  I: integer;
begin
  if devData.First then begin
    AddItem('ege.h','-lgraphics -luuid -lmsimg32 -lgdi32 -limm32 -lole32 -loleaut32 -lwinmm -lgdiplus -mwindows');
    AddItem('turtle.h','-lturtle');
    AddItem('GL/gl.h','-lopengl32');
    AddItem('GL/freeglut.h','-lfreeglut');
    AddItem('GL/glut.h','-lfreeglut -lopengl32 -lgdi32');
    AddItem('GL/glew.h','-lglew32 -lopengl32 -lgdi32');
    AddItem('GLFW/glfw3.h','-lglfw2 -lopengl32 -lgdi32');

     // Save to disk as defaults
    Save;

  end else if FileExists(fFile) then begin // no first time launch? load from disk
    with TINIFile.Create(fFile) do try
      tmp := TStringList.Create;
      Clear;
      try
        ReadSections(tmp);
        for I := 0 to tmp.Count - 1 do begin
          header := tmp[I];
          linkParams := StrToCodeIns(ReadString(tmp[I], 'params', ''));
          AddItem(header,linkParams);
        end;
      finally
        tmp.free;
      end;
    finally
      free;
    end;
  end;
end;

procedure TAutoLinkList.Save;
var
  I: integer;
  section: AnsiString;
  item: PAutoLink;
begin
  DeleteFile(fFile);
  if fList.Count = 0 then
    Exit;

  with TINIFile.Create(fFile) do try
    for I := 0 to pred(fList.Count) do begin
      item := PAutoLink(fList[I]);
      section := item^.header;
      WriteString(section, 'params', CodeInstoStr(item^.linkParams));
    end;
  finally
    Free;
  end;
end;

end.

