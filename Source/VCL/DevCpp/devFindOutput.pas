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

  TFindOutput = class(TCustomTreeView)
  private
    fControlCanvas: TControlCanvas;
    fFindNode: TTreeNode;
    fFileNode: TTreeNode;
    fMaxFindCount: integer;
    fFinds: TList; // List of find root treenode;
    procedure removeFind(node:TTreeNode);
    procedure clearFinds;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginFind(const token:string);
    procedure EndFind(const token:string;const hits,filesearched,filehitted: integer);
    procedure AddFindHit(filename:string;line:integer;char:integer; tokenlen: integer;  lineText: string);
    procedure CancelFind;
    procedure Clear;
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
    property RowSelect;
    property ShowLines;
    property MaxFindCount: integer read fMaxFindCount write fMaxFindCount;
    property Finds: TList read fFinds;
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
  clearFinds;
  {
  FreeAndNil(fControlCanvas);
  }
  FreeAndNil(fFinds);
  inherited Destroy;
end;

procedure TFindOutput.removeFind(node:TTreeNode);
var
  child,oldChild:TTreeNode;

  procedure RemoveFileNode(node:TTreeNode);
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
        items.Delete(oldChild);
      end;
    end;
    //file node has no data
    items.Delete(node);
  end;
begin
  if node.Level <> 0 then begin
    Exit;
  end;
  if node.HasChildren then begin
    child := node.getFirstChild;
    while (assigned(child)) do begin
      removeFileNode(child);
      oldChild := child;
      child := child.getNextSibling;
      items.Delete(oldChild);
    end;
  end;
  if assigned(node.Data) then
    Dispose(PFindInfo(node.Data));
  items.Delete(node);
end;


procedure TFindOutput.BeginFind(const token:string);
begin
  Items.BeginUpdate;
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

procedure TFindOutput.EndFind(const token:string;const hits,filesearched,filehitted: integer);
var
  p: PFindInfo;
begin
  try
    if fFinds.Count >= fMaxFindCount then begin
      removeFind(TTreeNode(fFinds[fFinds.Count-1]));
    end;
    new(p);
    p^.token := token;
    p^.hits:=hits;
    p^.filesearched:=filesearched;
    p^.filehitted:=hits;
    fFindNode.Data := p;
    fFindNode.Text := token;
    fFinds.Insert(0,fFindNode);
  finally
    Items.EndUpdate;
  end;
end;

procedure TFindOutput.AddFindHit(filename:string;line:integer;char:integer; tokenlen: integer;  lineText: string);
var
  p:PFindItem;
begin
  if not assigned(fFileNode) or (not sameText(filename,fFileNode.text)) then
    fFileNode := Items.AddChild(fFindNode,filename);
  new(p);
  p^.filename:=filename;
  p^.line := line;
  p^.char := char;
  p^.tokenLen := tokenlen;
  p^.lineText := lineText;
  Items.AddChildObject(fFileNode,lineText,p);
end;

procedure TFindOutput.ClearFinds;
var
  i:integer;
begin
  for i:=0 to fFinds.Count -1 do begin
    RemoveFind(TTreeNode(fFinds[i]));
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

end.

