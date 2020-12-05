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
    fUpdateCount: integer;
    fOnUpdated: TNotifyEvent;
    fFindNode: TTreeNode;
    fFileNode: TTreeNode;
    fMaxFindCount: integer;
    fFinds: TList; // List of find root treenode;

    procedure clearItems;
    procedure removeFind(node:TTreeNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginFind(const token:string);
    procedure EndFind(const token:string;const hits,filesearched,filehitted: integer);
    procedure AddFindHit(filename:string;line:integer;char:integer; tokenlen: integer;  lineText: string);
    procedure CancelFind;
    procedure Clear;
    property OnUpdated: TNotifyEvent read fOnUpdated write fOnUpdated;
  published
    property Align;
    property Font;
    property Images;
    property ReadOnly;
    property Indent;
    property TabOrder;
    property PopupMenu;
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
  DragMode := dmAutomatic;
  ShowHint := True;
  HideSelection := False;
  RightClickSelect := True;
  fControlCanvas := TControlCanvas.Create;
  fControlCanvas.Control := Self;
  //fControlCanvas.Font.Assign(Self.Font);
  fUpdateCount := 0;
  RowSelect := true;
  ShowLines := False;
  fFindNode := nil;
  fFileNode := nil;
  fMaxFindCount := 30;
  fFinds := TList.Create;
end;

destructor TFindOutput.Destroy;
begin
  clear;
  FreeAndNil(fControlCanvas);
  FreeAndNil(fFinds);
  inherited Destroy;
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
    end else
      inc(fFindCount);
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
  p:PFileItem;
begin
  if not assigned(fFileNode) or (not sameText(filename,fFileNode.text)) then
    fFileNode := Items.AddChildObject(fFindNode,filename,nil);
  new(p);
  p^.filename:=filename;
  p^.line := line;
  p^.char := char;
  p^.tokenLen := tokenlen;
  p^.lineText := lineText;
  Items.AddChildObject(fFileNode,lineText,p);
end;


procedure TFindOutput.AddMembers(Node: TTreeNode; ParentStatement: PStatement);
var
  Statement: PStatement;
  NewNode: TTreeNode;
  Children: TList;
  i:integer;
  P:PFileIncludes;

  procedure AddStatement(Statement: PStatement);
  begin
    NewNode := Items.AddChildObject(Node, Statement^._Command, Statement);
    SetNodeImages(NewNode, Statement);
    if Statement^._Kind in [skClass,skNamespace] then
      AddMembers(NewNode, Statement);
  end;
begin
  if Assigned(ParentStatement) then begin
    Children := fParser.Statements.GetChildrenStatements(ParentStatement);
  end else begin
    p:=fParser.FindFileIncludes(fCurrentFile);
    if not Assigned(p) then
      Exit;
    Children := p^.Statements;
  end;

//  fParser.Statements.DumpWithScope('f:\browser.txt');
  if Assigned(Children) then begin
    for i:=0 to Children.Count-1 do begin
      Statement := Children[i];
      with Statement^ do begin
        // Do not print statements marked invisible for the class browser
        
        if _Kind = skBlock then
          Continue;

        if _Inherited and not fShowInheritedMembers then // don't show inherited members
          Continue;

        if Statement = ParentStatement then // prevent infinite recursion
          Continue;

        if Statement^._ParentScope <> ParentStatement then
          Continue;

        {
        if SameText(_FileName,CurrentFile) or SameText(_DefinitionFileName,CurrentFile) then
          AddStatement(Statement)
        }
        AddStatement(Statement)
      end;
    end;
  end;
end;

procedure TFindOutput.Clear;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    fFindCount := 0;
  finally
    Items.EndUpdate;
  end;
end;

end.

