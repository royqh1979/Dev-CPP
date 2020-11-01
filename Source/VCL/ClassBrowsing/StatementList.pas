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
    fFirstNode: PStatementNode;
    fLastNode: PStatementNode;
    fOwnsObjects: boolean;
    fGlobalStatements: TList;
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
    function DeleteStatement(Data: PStatement): Integer; overload;
    function DeleteNode(Node: PStatementNode): Integer; overload;
    function DeleteFromTo(FromNode, ToNode: PStatementNode): Integer;
    function GetChildrenStatements(Statement:PStatement): TList;
    procedure DumpTo(Filename:AnsiString);
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
  fFirstNode := nil;
  fLastNode := nil;
  fCount := 0;
  fOwnsObjects := True;
  fGlobalStatements := TList.Create;
end;

destructor TStatementList.Destroy;
begin
  Clear;
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
  OnNodeAdding(Node);
  Result := Node;
  if Assigned(Data^._Parent) then begin
    parent := Data^._Parent;
    if not Assigned(parent^._Children) then
      parent^._Children := TList.Create;
    parent^._Children.Add(Data);
  end else begin
    fGlobalStatements.Add(Data);
  end;
end;

procedure TStatementList.DisposeNode(Node: PStatementNode);
begin
  // remove it from parent's children
  if Assigned(Node^.Data^._Parent) then begin
    Node^.Data^._Parent^._Children.remove(Node^.Data);
  end else begin
    fGlobalStatements.Remove(Node^.Data);
  end;

  if Assigned(PStatement(Node^.Data)) and OwnsObjects then begin
    if Assigned(PStatement(Node^.Data)^._InheritanceList) then
      PStatement(Node^.Data)^._InheritanceList.Free;
    if Assigned(PStatement(Node^.Data)^._Children) then
      PStatement(Node^.Data)^._Children.Free;
    if Assigned(PStatement(Node^.Data)^._Friends) then
      PStatement(Node^.Data)^._Friends.Free;
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

function TStatementList.DeleteStatement(Data: PStatement): Integer;
var
  Node: PStatementNode;
begin
  Node := fFirstNode;
  while Assigned(Node) do begin
    if Node^.Data = Data then begin
      OnNodeDeleting(Node); // updates information about linked list
      DisposeNode(Node);
      break;
    end;
    Node := Node^.NextNode;
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
end;

function TStatementList.GetChildrenStatements(Statement:PStatement): TList;
begin
  if (Assigned(Statement)) then begin
    Result:= Statement._Children
  end else
    Result:=fGlobalStatements;
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
      Add(Format('%s,%s,%d,%s,%d,%s,%d',[statement^._Command,statement^._Type,integer(statement^._Parent)
        ,statement^._FileName,statement^._Line,statement^._DefinitionFileName,statement^._DefinitionLine]));
      Node := NextNode;
    end;
    SaveToFile(Filename);
  finally
    Free;
  End;
end;

end.

