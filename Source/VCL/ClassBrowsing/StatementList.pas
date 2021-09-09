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

unit StatementList;

interface

uses
  Classes, CBUtils, Forms, SysUtils, iniFiles;

type

  PStatementNode = ^TStatementNode;
  TStatementNode = record
    PrevNode: PStatementNode;
    NextNode: PStatementNode;
    Data: PStatement;
  end;

  { TStatementList class }
  TStatementList = class(TObject)
  private
    fCount: Integer;
    fClearing: boolean;
    fFirstNode: PStatementNode;
    fLastNode: PStatementNode;
    fOwnsObjects: boolean;
    fGlobalStatements: TList;
    fGlobalStatementIndex: TDevStringHash;
    fBatchDeleteCount: integer;
    procedure DisposeNode(Node: PStatementNode);
    procedure OnNodeAdding(Node: PStatementNode); // call when about to add this node
    procedure OnNodeDeleting(Node: PStatementNode); // call when about to delete this node
  public
    constructor Create;
    destructor Destroy; override;
    function FirstStatement: PStatement;
    function LastStatement: PStatement;
    function Add(Data: PStatement): PStatementNode;
    function DeleteFirst: Integer;
    function DeleteLast: Integer;
    procedure BeginBatchDelete;
    procedure EndBatchDelete;
    function DeleteStatement(Data: PStatement): Integer; overload;
    function DeleteNode(Node: PStatementNode): Integer; overload;
    function DeleteFromTo(FromNode, ToNode: PStatementNode): Integer;
    function GetChildrenStatements(Statement:PStatement): TList;
    function GetChildrenStatementIndex(Statement:PStatement): TDevStringHash;
    procedure DumpTo(Filename:AnsiString);
    procedure DumpWithScope(Filename:AnsiString);
    procedure Clear;
    property FirstNode: PStatementNode read fFirstNode;
    property LastNode: PStatementNode read fLastNode;
    property Count: Integer read fCount;
    property OwnsObjects: boolean read fOwnsObjects write fOwnsObjects;
  end;

implementation

{ TStatementList }

constructor TStatementList.Create;
begin
  fClearing:=False;
  fFirstNode := nil;
  fLastNode := nil;
  fCount := 0;
  fOwnsObjects := True;
  fGlobalStatements := TList.Create;
  fGlobalStatementIndex := TDevStringHash.Create(10000);
  fBatchDeleteCount := 0;
end;

destructor TStatementList.Destroy;
begin
  Clear;
  fGlobalStatementIndex.Free;
  fGlobalStatements.Free;
end;

function TStatementList.FirstStatement: PStatement;
begin
  if Assigned(fFirstNode) then
    Result := fFirstNode^.Data
  else
    Result := nil;
end;

function TStatementList.LastStatement: PStatement;
begin
  if Assigned(fLastNode) then
    Result := fLastNode^.Data
  else
    Result := nil;
end;

procedure TStatementList.OnNodeAdding(Node: PStatementNode);
begin
  // First node to add. It's alone
  if fCount = 0 then begin
    Node^.NextNode := nil;
    Node^.PrevNode := nil;
    fFirstNode := Node;
    fLastNode := Node;
  end else begin
    Node^.NextNode := nil;
    Node^.PrevNode := fLastNode; // previous last node
    Node^.PrevNode.NextNode := Node;
    fLastNode := Node; // new last node
  end;

  // Update count
  Inc(fCount);
end;

function TStatementList.Add(Data: PStatement): PStatementNode;
var
  Node: PStatementNode;
  parent: PStatement;
begin
  // Create a new one
  Node := New(PStatementNode);
  Node^.Data := Data;
  Data^._Node := Node;
  OnNodeAdding(Node);
  Result := Node;
  if Assigned(Data^._ParentScope) then begin
    parent := Data^._ParentScope;
    if not Assigned(parent^._Children) then
      parent^._Children := TList.Create;
    parent^._Children.Add(Data);
    if not Assigned(parent^._ChildrenIndex) then
      parent^._ChildrenIndex := TDevStringHash.Create(500);
    parent^._ChildrenIndex.Add(Data^._Command,integer(Data));
  end else begin
    fGlobalStatements.Add(Data);
    fGlobalStatementIndex.Add(Data^._Command,integer(Data));
  end;
end;

procedure TStatementList.DisposeNode(Node: PStatementNode);
var
  Children:TList;
  i :integer;
  child:PStatement;

  procedure RemoveStatementFromIndex(statement:PStatement; children:TList; childrenIndex:TDevStringHash);
  begin
    children.Remove(statement);
    childrenIndex.RemoveItem(statement^._Command,integer(statement));
  end;
begin
  // remove it from parent's children
  Node^.Data^._Node := nil;
  if not fClearing then begin
  // we only need to remove child from parent statement when we are not clearing the statementlist
    if Assigned(Node^.Data^._ParentScope) then begin
      RemoveStatementFromIndex(
        Node^.Data,
        Node^.Data^._ParentScope^._Children,
        Node^.Data^._ParentScope^._ChildrenIndex);
    end else begin
      RemoveStatementFromIndex(
        Node^.Data,
        fGlobalStatements,
        fGlobalStatementIndex);
    end;
  end;
  if Assigned(PStatement(Node^.Data)) and OwnsObjects then begin
    if Assigned(PStatement(Node^.Data)^._InheritanceList) then
      PStatement(Node^.Data)^._InheritanceList.Free;
    if Assigned(PStatement(Node^.Data)^._Children) then begin
      Children := PStatement(Node^.Data)^._Children;
      for i:=0 to Children.Count-1 do begin
        child:=PStatement(Children[i]);
        child^._ParentScope:=nil;
      end;
      Children.Free;
    end;
    if Assigned(PStatement(Node^.Data)^._ChildrenIndex) then begin
      PStatement(Node^.Data)^._ChildrenIndex.Free;
    end;
    if Assigned(PStatement(Node^.Data)^._Friends) then
      PStatement(Node^.Data)^._Friends.Free;
    PStatement(Node^.Data)^._Usings.Free;
    Dispose(PStatement(Node^.Data));
  end;
  Dispose(PStatementNode(Node));
end;

procedure TStatementList.OnNodeDeleting(Node: PStatementNode);
begin
  // Special easy cases
  if fCount = 0 then
    Exit;
  if fCount = 1 then begin
    fFirstNode := nil;
    fLastNode := nil;
    fCount := 0;
    Exit;
  end;

  // Otherwise, first and last both exist
  if Node = fFirstNode then begin // update first node referals
    fFirstNode^.NextNode^.PrevNode := nil;
    fFirstNode := fFirstNode^.NextNode;
  end else if Node = fLastNode then begin // update last node referals
    fLastNode^.PrevNode^.NextNode := nil;
    fLastNode := fLastNode^.PrevNode;
  end else begin // update neighbor
    Node^.PrevNode^.NextNode := Node^.NextNode;
    Node^.NextNode^.PrevNode := Node^.PrevNode;
  end;
  Dec(fCount);
end;

function TStatementList.DeleteFirst: Integer;
begin
  Result := DeleteNode(fFirstNode);
end;

function TStatementList.DeleteLast: Integer;
begin
  Result := DeleteNode(fLastNode);
end;

function TStatementList.DeleteNode(Node: PStatementNode): Integer;
begin
  if Assigned(Node) then begin
    OnNodeDeleting(Node); // updates information about linked list
    DisposeNode(Node);
  end;
  Result := fCount;
end;

procedure TStatementList.BeginBatchDelete;
begin
  inc(fBatchDeleteCount);
end;
procedure TStatementList.EndBatchDelete;
begin
  dec(fBatchDeleteCount);
end;

function TStatementList.DeleteStatement(Data: PStatement): Integer;
var
  Node: PStatementNode;
begin
  Node := PStatementNode(Data^._Node);
  if Assigned(Node) then begin
      OnNodeDeleting(Node); // updates information about linked list
      DisposeNode(Node);
  end else begin
    Node := fFirstNode;
    while Assigned(Node) do begin
      if Node^.Data = Data then begin
        OnNodeDeleting(Node); // updates information about linked list
        DisposeNode(Node);
        break;
      end;
      Node := Node^.NextNode;
    end;
  end;
  Result := fCount;
end;

function TStatementList.DeleteFromTo(FromNode, ToNode: PStatementNode): Integer; // TODO: merge with Delete?
var
  Node: PStatementNode;
begin
  Node := FromNode;
  while Assigned(Node) do begin
    OnNodeDeleting(Node); // updates information about linked list
    DisposeNode(Node);
    break;
    if Node = ToNode then
      break;
    Node := Node^.NextNode;
  end;
  Result := fCount;
end;

procedure TStatementList.Clear;
var
  Node, NextNode: PStatementNode;
begin
  fClearing:=True;
  // Search all nodes
  Node := fFirstNode;
  while Assigned(Node) do begin
    NextNode := Node^.NextNode;
    // Do not call OnNodeDeleting, because all nodes will be cleared
    DisposeNode(Node);
    Node := NextNode;
  end;
  fFirstNode := nil;
  fLastNode := nil;
  fCount := 0;
  fGlobalStatements.Clear;
  fGlobalStatementIndex.Clear;
  fClearing:=False;
end;

function TStatementList.GetChildrenStatements(Statement:PStatement): TList;
begin
  if (Assigned(Statement)) then begin
    Result:= Statement._Children
  end else
    Result:=fGlobalStatements;
end;

function TStatementList.GetChildrenStatementIndex(Statement:PStatement): TDevStringHash;
begin
  if (Assigned(Statement)) then begin
    Result:= Statement^._ChildrenIndex;
  end else
    Result:=fGlobalStatementIndex;
end;

procedure TStatementList.DumpTo(Filename:AnsiString);
var
  Node, NextNode: PStatementNode;
  statement:PStatement;
begin
  with TStringList.Create do try
    // Search all nodes
    Node := fFirstNode;
    while Assigned(Node) do begin
      NextNode := Node^.NextNode;
      // Do not call OnNodeDeleting, because all nodes will be cleared
      statement := PStatement(Node^.Data);
      Add(Format('%s,%s,%d,%s,%d,%s,%d,%d,%s,%s',[statement^._Command,statement^._Type,integer(statement^._ParentScope)
        ,statement^._FileName,statement^._Line,statement^._DefinitionFileName,statement^._DefinitionLine,Ord(statement^._HasDefinition),
          statement^._Args, statement^._NoNameArgs]));
      Node := NextNode;
    end;
    SaveToFile(Filename);
  finally
    Free;
  End;
end;

procedure TStatementList.DumpWithScope(Filename:AnsiString);
var
  i:integer;
  statement:PStatement;
  DumpFile : TStringList;
  procedure DumpStatement( statement:PStatement;level:integer);
  var
    indent:AnsiString;
    i:integer;
    children:TList;
    childStatement:PStatement;
  begin
    indent:='';
    for i:=0 to level do
      indent:=indent+'  ';
    DumpFile.Add(indent+Format('%s, %d, %s, %s, %d, %d, %s, %d, %s, %d',[statement^._Command,
      ord(statement^._Kind), statement^._Type,statement^._FullName,integer(statement^._ParentScope)
        ,integer(statement^._ClassScope),statement^._FileName,statement^._Line,statement^._DefinitionFileName,statement^._DefinitionLine]));
    children := statement^._Children;
    if not Assigned(children) then begin
      Exit;
    end;
    DumpFile.Add(indent + statement^._Command + ' {');
    for i:=0 to children.Count -1 do begin
      childStatement := PStatement(children[i]);
      DumpStatement(childStatement,level+1);
    end;
    DumpFile.Add(indent + '}');
  end;
begin
  DumpFile:=TStringList.Create;
  with DumpFile do try
    for i:=0 to fGlobalStatements.Count -1 do begin
      Statement := PStatement(fGlobalStatements[i]);
      DumpStatement(Statement,0);
    end;
    SaveToFile(Filename);
  finally
    Free;
  End;
end;

end.

