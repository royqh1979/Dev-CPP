{
    This file is part of Dev-C++
    Copyright (c) 2020 Roy Qu (royqh1979@gmail.com)

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit devFindOutput;

interface

uses
  Windows, Classes, SysUtils,  Controls, ComCtrls, Graphics,
   Forms, Messages;

type
    
  PFindItem = ^TFindItem;
  TFindItem = record
    filename:String;
    line: integer;
    char: integer;
    tokenlen: integer;
    lineText: string;
  end;

  PFindInfo = ^TFindInfo;
  TFindInfo = record
    token:string;
    hits:integer;
    filesearched:integer;
    filehitted:integer;
  end;

  PFileInfo = ^TFileInfo;
  TFileInfo = record
    filename: string;
    hits: integer;
  end;

  TFindOutput = class(TCustomTreeView)
  private
    fControlCanvas: TControlCanvas;
    fFindNode: TTreeNode;
    fFileNode: TTreeNode;
    fMaxFindCount: integer;
    fFinds: TList; // List of find root treenode;
    procedure RemoveFind(node:TTreeNode;deleteNode:boolean = True);
    procedure ClearFinds(deleteNode:boolean = True);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginFind(const token:string);
    procedure EndFind(const token:string;const hits,filehitted,filesearched: integer);
    procedure AddFindHit(filename:string;line:integer;char:integer; tokenlen: integer;  lineText: string);
    procedure CancelFind;
    procedure Clear;
    procedure LinesDeleted(FileName: string; FirstLine, Count: integer);
    procedure LinesInserted(FileName: string; FirstLine, Count: integer);
  published
    property Align;
    property Font;
    property Color;
    property ReadOnly;
    property Indent;
    property TabOrder;
    property BorderStyle;
    property MultiSelect;
    property MultiSelectStyle;
    property PopupMenu;
    property RowSelect;
    property ShowLines;
    property MaxFindCount: integer read fMaxFindCount write fMaxFindCount;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
       
  end;

const
  CLASS_FOLDERS_MAGIC = 'DEVCF_1_0';
  TV_FIRST = $1100;
  TVM_SETEXTENDEDSTYLE = TV_FIRST + 44;
  TVM_GETEXTENDEDSTYLE = TV_FIRST + 45;
  TVS_EX_DOUBLEBUFFER = $4;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Dev-C++', [TFindOutput]);
end;

{ TFindOutput }

constructor TFindOutput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  parent:=TWinControl(AOwner);
  DragMode := dmAutomatic;
  ShowHint := True;
  HideSelection := False;
  RightClickSelect := True;
  {
  fControlCanvas := TControlCanvas.Create;
  fControlCanvas.Control := Self;
  }
  //fControlCanvas.Font.Assign(Self.Font);
  RowSelect := true;
  ShowLines := False;
  fFindNode := nil;
  fFileNode := nil;
  fMaxFindCount := 30;
  fFinds := TList.Create;
end;

destructor TFindOutput.Destroy;
begin
  //ClearFinds(False);
  {
  FreeAndNil(fControlCanvas);
  }
  FreeAndNil(fFinds);
  inherited Destroy;
end;

procedure TFindOutput.removeFind(node:TTreeNode;deleteNode:boolean);
var
  child,oldChild:TTreeNode;

  procedure RemoveFileNode(node:TTreeNode;deleteNode:boolean);
  var
    child,oldChild:TTreeNode;
  begin
    if node.HasChildren then begin
      child := node.getFirstChild;
      while (assigned(child)) do begin
        if assigned(child.Data) then
          dispose(PFindItem(child.Data));
        oldChild := child;
        child := child.getNextSibling;
        if DeleteNode then
          items.Delete(oldChild);
      end;
    end;
    if assigned(node.Data) then
      Dispose(PFileInfo(node.Data));
    if deleteNode then
      items.Delete(node);
  end;
begin
  if node.HasChildren then begin
    child := node.getFirstChild;
    while (assigned(child)) do begin
      removeFileNode(child,deleteNode);
      oldChild := child;
      child := child.getNextSibling;
    end;
  end;
  if assigned(node.Data) then begin
    Dispose(PFindInfo(node.Data));
    node.Data := nil;
  end;
  if deleteNode then begin
    items.Delete(node);
  end;
end;


procedure TFindOutput.BeginFind(const token:string);
var
  node:TTreeNode;
begin
  Items.BeginUpdate;
  if fFinds.Count>0 then begin
    node:=TTreeNode(fFinds[0]);
    node.Collapse(True);
  end;
  fFindNode := Items.AddChildFirst(nil,token); // we'll update the text when find end
  fFileNode:= nil;
end;

procedure TFindOutput.CancelFind;
begin
  try
    removeFind(fFindNode);
    fFindNode := nil;
    fFileNode := nil;
  finally
    Items.EndUpdate;
  end;
end;

procedure TFindOutput.EndFind(const token:string;const hits,filehitted,filesearched: integer);
var
  p: PFindInfo;
  child: TTreeNode;
  pFile : PFileInfo;
begin
  try
    if fFinds.Count >= fMaxFindCount then begin
      removeFind(TTreeNode(fFinds[fFinds.Count-1]));
    end;
    new(p);
    p^.token := token;
    p^.hits:=hits;
    p^.filesearched:=filesearched;
    p^.filehitted:=filehitted;
    fFindNode.Data := p;
    fFindNode.Text := Format('Search "%s" (%d hits in %d files of %d searched)',[token,hits,filehitted,filesearched]);
    fFinds.Insert(0,fFindNode);
    if fFindNode.HasChildren then begin
      child := fFindNode.getFirstChild;
      while (assigned(child)) do begin
        if assigned(child.Data) then begin
          pFile := PFileInfo(child.Data);
          child.Text := Format('%s (%d hits)', [pFile.filename,pFile.hits]);
        end;
        child := child.getNextSibling;
      end;
    end;
    fFindNode.Expand(True);
    fFindNode.Selected:=True;
    fFindNode.MakeVisible;
  finally
    Items.EndUpdate;
  end;
end;

procedure TFindOutput.AddFindHit(filename:string;line:integer;char:integer; tokenlen: integer;  lineText: string);
var
  p:PFindItem;
  pFile:PFileInfo;
begin
  if not assigned(fFileNode)
    or (not sameText(filename,PFileInfo(fFileNode.Data)^.FileName)) then begin
    fFileNode := Items.AddChild(fFindNode,filename);
    new(pFile);
    pFile.filename:=filename;
    pFile.hits:=0;
    fFileNode.Data:=pFile;
  end;
  pFile := PFileInfo(fFileNode.Data);
  inc(pFile.hits);
  new(p);
  p^.filename:=filename;
  p^.line := line;
  p^.char := char;
  p^.tokenLen := tokenlen;
  p^.lineText := lineText;
  Items.AddChildObject(fFileNode,Format('Line %d: ',[line])+lineText,p);
end;

procedure TFindOutput.ClearFinds(deleteNode:boolean);
var
  i:integer;
begin
  for i:=0 to fFinds.Count -1 do begin
    RemoveFind(TTreeNode(fFinds[i]),deleteNode);
  end;
  fFinds.Clear;
end;

procedure TFindOutput.Clear;
begin
  Items.BeginUpdate;
  try
    ClearFinds;
    Items.Clear;
  finally
    Items.EndUpdate;
  end;
end;

procedure TFindOutput.LinesInserted(FileName: string; FirstLine, Count: integer);
var
  node : TTreeNode;
  p : PFindItem;
begin
  Items.BeginUpdate;
  try
    node := Items.GetFirstNode;
    while assigned(node) do begin
      if (node.Level=2) and assigned(node.Data) then begin
        p:=PFindItem(node.Data);
        if SameText(fileName, p.filename) then begin
          if (p.line >= FirstLine) then
            inc(p.line , Count);
        end;
      end;
      node := node.GetNext;
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TFindOutput.LinesDeleted(FileName: string; FirstLine, Count: integer);
var
  node,nodeToDelete : TTreeNode;
  p : PFindItem;
begin
  Items.BeginUpdate;
  try
    node :=  Items.GetFirstNode;
    while assigned(node) do begin
      nodeToDelete := nil;
      if (node.Level=2) and assigned(node.Data) then begin
        p:=PFindItem(node.Data);
        if SameText(fileName, p.filename) then begin
          if (p.line >= FirstLine) then begin
            if (p.line >= FirstLine + Count) then begin
              dec(p.line,Count);
            end else begin
              dispose(PFindItem(p));
              nodeToDelete := node;
            end;
          end;
        end;
      end;
      node := node.GetNext;
      if assigned(nodeToDelete) then
        items.Delete(nodeToDelete);
    end;
  finally
    Items.EndUpdate;
  end;
end;


end.

