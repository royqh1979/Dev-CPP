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

unit devFileBrowser;

interface

uses
  Windows, Classes, SysUtils, Controls, ComCtrls, Graphics,
  Forms, Messages, SyncObjs, VirtualTrees, StrUtils,devFileMonitor;

type

  TDevFileBrowser = class(TCustomVirtualStringTree)
  private
    fCurrentFolder : AnsiString;
    fOnlyShowDevFiles: boolean;
    fMonitor: TDevFileMonitor;
    procedure AddChildren(Parent:PVirtualNode; Path:String);
    procedure SetCurrentFolder(const folder:String);
    procedure SetOnlyShowDevFiles(const Value: boolean);
    procedure SetMonitor(const value: TDevFileMonitor);
    procedure InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: UnicodeString);
    procedure GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; const Text: UnicodeString; const CellRect: TRect; var DefaultDraw: Boolean);
    function GetSelectedFile: String;
    function GetSelected:PVirtualNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure LocateFile(const FileName:String);
    property Colors;
    property SelectedFile : String read GetSelectedFile;
    property OnlyShowDevFiles: boolean read fOnlyShowDevFiles write SetOnlyShowDevFiles;
    property Monitor: TDevFileMonitor read fMonitor write SetMonitor;
  published
    property Align;
    property Font;
    property Color;
    property Images;
    property Indent;
    property TabOrder;
    property PopupMenu;
    property BorderStyle;
    property Header;
    property CurrentFolder: AnsiString read fCurrentFolder write SetCurrentFolder;
    property OnDblClick;
    property OnContextPopup;
  end;

const
  CLASS_FOLDERS_MAGIC = 'DEVCF_1_0';
  TV_FIRST = $1100;
  TVM_SETEXTENDEDSTYLE = TV_FIRST + 44;
  TVM_GETEXTENDEDSTYLE = TV_FIRST + 45;
  TVS_EX_DOUBLEBUFFER = $4;

procedure Register;

implementation

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Name: String;
    ImageIndex:integer;
    isDir : boolean;
    FullPath : String;
  end;

procedure Register;
begin
  RegisterComponents('Dev-C++', [TDevFileBrowser]);
end;



{ TDevFileBrowser }

constructor TDevFileBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragMode := dmManual;
  ShowHint := True;
  NodeDataSize := sizeof(TNodeData);
  //OnAdvancedCustomDrawItem := AdvancedCustomDrawItem;

  fOnlyShowDevFiles := False;
  fCurrentFolder := '';
  //RowSelect := true;
  //ShowLines := False;
  TVirtualTreeOptions(TreeOptions).PaintOptions :=
    TVirtualTreeOptions(TreeOptions).PaintOptions - [toShowTreeLines];
  TVirtualTreeOptions(TreeOptions).SelectionOptions :=
    TVirtualTreeOptions(TreeOptions).SelectionOptions + [toFullRowSelect, toRightClickSelect];


  TVirtualTreeOptions(TreeOptions).AutoOptions :=
    TVirtualTreeOptions(TreeOptions).AutoOptions - [toAutoSort];

  self.OnInitNode := InitNode;
  self.OnInitChildren := InitChildren;
  self.OnGetText := GetText;
  //self.OnFreeNode := OnCBFreeNode;
  self.OnGetImageIndex := GetImageIndex;
  //self.OnDrawText := OnCBDrawText;
end;

destructor TDevFileBrowser.Destroy;
begin
  if assigned(fMonitor) and (fCurrentFolder <> '') then
    fMonitor.unmonitor(fCurrentFolder);
  inherited Destroy;
end;

procedure TDevFileBrowser.AddChildren(Parent:PVirtualNode; Path:String);
  var
    SR : TSearchRec;
    Node : PVirtualNode;
    Data: PNodeData;
    files: TStringList;
    folders : TStringList;
    i:integer;
    ext: String;
  begin
    if FindFirst(IncludeTrailingPathDelimiter(Path) + '*.*', faAnyFile, SR) <> 0 then
      Exit;
    files := TStringList.Create;
    folders := TStringList.Create;
    repeat
      if (SR.Attr and (faHidden or faSysFile))<>0 then
        Continue;
      if SameText(SR.Name,'.') or SameText(SR.Name,'..') then
        Continue;
      if (SR.Attr and faDirectory) <> 0 then
        folders.Add(SR.Name)
      else
        files.Add(SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
    folders.CaseSensitive := False;
    folders.Sorted := True;
    for i:=0 to folders.Count-1 do begin
      Node := AddChild(Parent);
      Data := GetNodeData(Node);
      Data^.isDir := True;
      Data^.Name := folders[i];
      Data^.FullPath := IncludeTrailingPathDelimiter(Path) + folders[i];
      Data^.ImageIndex := 0;
      Node.States := Node.States + [vsHasChildren];
      ValidateNode(Node,False);
    end;
    files.CaseSensitive := False;
    files.Sorted := True;
    for i:=0 to files.Count-1 do begin
      ext := ExtractFileExt(files[i]);
      if fOnlyShowDevFiles and not( SameText(ext,'.c') or SameText(ext,'.h')
        or SameText(ext,'.cpp') or SameText(ext,'.hpp')
        or SameText(ext,'.cxx') or SameText(ext,'.hxx')
        or SameText(ext,'.cc') or SameText(ext,'.dev') ) then
        Continue;
      Node := AddChild(Parent);
      Data := GetNodeData(Node);
      Data^.isDir := False;
      Data^.Name := files[i];
      Data^.FullPath := IncludeTrailingPathDelimiter(Path) + files[i];
      if SameText(ext,'.dev') then
        Data^.ImageIndex := 4
      else if (CompareStr(ext,'.c')=0) or (CompareStr(ext,'.h')=0) then
        Data^.ImageIndex := 2
      else if SameText(ext,'.C') or SameText(ext,'.H')
        or SameText(ext,'.cpp') or SameText(ext,'.hpp')
        or SameText(ext,'.cc') then
        Data^.ImageIndex := 3
      else
        Data^.ImageIndex := 1;
      ValidateNode(Node,False);
    end;
    files.Free;
    folders.Free;
  end;

procedure TDevFileBrowser.Refresh;

begin
  self.Clear;
  if fCurrentFolder = '' then begin
    Exit;
  end;
  AddChildren(nil,fCurrentFolder);
end;

procedure TDevFileBrowser.InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
    var InitialStates: TVirtualNodeInitStates);
begin

end;

procedure TDevFileBrowser.InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  Data:PNodeData;
begin
  if Node.Parent <> nil then begin
    data:= sender.GetNodeData(Node);
    AddChildren(Node, data^.FullPath);
    ChildCount := sender.ChildCount[Node];
  end;
end;


procedure TDevFileBrowser.GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  data :PNodeData;
begin
  if Kind = ikOverlay then begin
    ImageIndex := 0;
    Exit;
  end;
  data := sender.GetNodeData(node);
  ImageIndex := data^.ImageIndex;
end;

procedure TDevFileBrowser.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UnicodeString);
var
  data :PNodeData;
begin
  data := sender.GetNodeData(node);
  CellText := data^.Name;
end;

procedure TDevFileBrowser.SetCurrentFolder(const folder: AnsiString);
var
  folderName : String;
begin
  folderName := ExcludeTrailingPathDelimiter(ExpandFileName(folder));
  if not DirectoryExists(folderName) then
    Exit;
  if SameText(folderName, fCurrentFolder) then
    Exit;
  if assigned(fMonitor) and (fCurrentFolder <> '') then
    fMonitor.unmonitor(fCurrentFolder);
  fCurrentFolder := folderName;
  if assigned(fMonitor) and (fCurrentFolder <> '') then
    fMonitor.monitor(fCurrentFolder);  
  self.Refresh;
end;

procedure TDevFileBrowser.SetOnlyShowDevFiles(const Value: boolean);
begin
  if fOnlyShowDevFiles = Value then
    Exit;
  fOnlyShowDevFiles := Value;
  self.Refresh;
end;

procedure TDevFileBrowser.SetMonitor(const value:TDevFileMonitor);
begin
  if fMonitor = Value then
    Exit;
  fMonitor := Value;
  if assigned(fMonitor) and (fCurrentFolder <> '') then
    fMonitor.monitor(fCurrentFolder);
end;

procedure TDevFileBrowser.DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const Text: UnicodeString; const CellRect: TRect; var DefaultDraw: Boolean);
begin
end;

function TDevFileBrowser.GetSelected:PVirtualNode;
var
  nodes:TVTVirtualNodeEnumeration;
  enumerator: TVTVirtualNodeEnumerator;
begin
  Result := nil;
  nodes := self.SelectedNodes(False);
  enumerator := nodes.GetEnumerator;
  if enumerator.MoveNext then
    Result := enumerator.Current;
end;

function TDevFileBrowser.GetSelectedFile:String;
var
  node:PVirtualNode;
  data:PNodeData;
begin
  Result := '';
  node := GetSelected;
  if assigned(node) then begin
    data:=GetNodeData(node);
    if assigned(data) then
      Result := data^.FullPath;
  end;
end;

function StartsText(const subtext, text: AnsiString): boolean;
begin
  Result := SameText(subtext, Copy(text, 1, Length(subtext)));
end;

procedure TDevFileBrowser.LocateFile(const FileName:String);
var
  parentNode,node:PVirtualNode;
  enumerator: TVTVirtualNodeEnumerator;
  data: PNodeData;
  foundNext:boolean;
begin
  if fCurrentFolder = '' then
    Exit;
  parentNode := self.RootNode;
  while assigned(parentNode) do begin
    node := GetFirstChild(parentNode);
    foundNext:=False;
    while assigned(node) do begin
      data := self.GetNodeData(node);
      if SameText(FileName,data^.FullPath) then begin
        ScrollIntoView(Node,True);
        SelectNodes(node,node,False);
        SetFocus;
        Exit;
      end;
      if data^.isDir and StartsText(IncludeTrailingPathDelimiter(data^.FullPath),FileName) then begin
        foundNext := True;
        parentNode := Node;
        break;
      end;
      node := GetNextSibling(node);
    end;
    if not FoundNext then
      Exit;
  end;

end;

end.

