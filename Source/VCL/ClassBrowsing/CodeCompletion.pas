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

unit CodeCompletion;

interface

uses
{$IFDEF WIN32}
  Windows, Classes, Forms, SysUtils, Controls, Graphics, CppParser,
  cbutils, StatementList, iniFiles;
{$ENDIF}
{$IFDEF LINUX}
Xlib, Classes, QForms, SysUtils, QControls, QGraphics, CppParser,
U_IntList, QDialogs, Types;
{$ENDIF}

type
  TCodeCompletion = class(TComponent)
  private
    fParser: TCppParser;
    fFullCompletionStatementList: TList;
    fCompletionStatementList: TList;
    fMinWidth: integer;
    fMinHeight: integer;
    fMaxWidth: integer;
    fMaxHeight: integer;
    fFontSize: integer;
    fPos: TPoint;
    fColor: TColor;
    fWidth: integer;
    fHeight: integer;
    fEnabled: boolean;
    fShowCount: integer;
    fOnKeyPress: TKeyPressEvent;
    fOnKeyDown: TKeyEvent;
    fOnResize: TNotifyEvent;
    fOnlyGlobals: boolean;
    fCurrentStatement: PStatement;
    fIncludedFiles: TStringList;
    fUsings: TStringList;
    fIsIncludedCacheFileName: AnsiString;
    fIsIncludedCacheResult: boolean;
    fAddedStatements : TStringHash;
    fPreparing: boolean;
    fPhrase : AnsiString;
    function ApplyClassFilter(Statement, CurrentClass: PStatement): boolean;
    procedure GetCompletionFor(FileName,Phrase: AnsiString);
    procedure FilterList(const Member: AnsiString);
    procedure SetPosition(Value: TPoint);
    procedure OnFormResize(Sender: TObject);
    function IsIncluded(const FileName: AnsiString): boolean;
    function IsVisible: boolean;
    procedure AddChildren(ScopeStatement:PStatement);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareSearch(const Phrase, Filename: AnsiString);
    function Search(const Phrase, Filename: AnsiString; AutoHideOnSingleResult:boolean):boolean;
    procedure Hide;
    procedure Show;
    function SelectedStatement: PStatement;
    property CurrentStatement: PStatement read fCurrentStatement write fCurrentStatement;
  published
    property ShowCount: integer read fShowCount write fShowCount;
    property Parser: TCppParser read fParser write fParser;
    property Position: TPoint read fPos write SetPosition;
    property Color: TColor read fColor write fColor;
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
    property Enabled: boolean read fEnabled write fEnabled;
    property MinWidth: integer read fMinWidth write fMinWidth;
    property MinHeight: integer read fMinHeight write fMinHeight;
    property MaxWidth: integer read fMaxWidth write fMaxWidth;
    property MaxHeight: integer read fMaxHeight write fMaxHeight;
    property FontSize: integer read fFontSize write fFontSize;
    property OnKeyDown: TKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnKeyPress: TKeyPressEvent read fOnKeyPress write fOnKeyPress;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
    property OnlyGlobals: boolean read fOnlyGlobals write fOnlyGlobals;
    property Visible: boolean read IsVisible;
  end;

procedure Register;

implementation

uses
  CodeCompletionForm, Math;

{ TCodeCompletion }

procedure Register;
begin
  RegisterComponents('Dev-C++', [TCodeCompletion]);
end;

constructor TCodeCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fIncludedFiles := TStringList.Create;
  fIncludedFiles.Sorted := True;
  fIncludedFiles.Duplicates := dupIgnore;

  fUsings:=TStringList.Create;
  fUsings.Sorted := True;
  fAddedStatements := TStringHash.Create;

  fCompletionStatementList := TList.Create;
  fFullCompletionStatementList := TList.Create;

  CodeComplForm := TCodeComplForm.Create(Self);
  CodeComplForm.OnResize := OnFormResize;

  fWidth := 320;
  fHeight := 240;
  fColor := clWindow;
  fEnabled := True;
  fOnlyGlobals := False;
  fShowCount := 100; // keep things fast

  fIsIncludedCacheFileName := '';
  fIsIncludedCacheResult := false;
end;

destructor TCodeCompletion.Destroy;
begin
  FreeAndNil(CodeComplForm);
  FreeAndNil(fCompletionStatementList);
  FreeAndNil(fFullCompletionStatementList);
  FreeAndNil(fIncludedFiles);
  FreeAndNil(fUsings);
  FreeAndNil(fAddedStatements);
  inherited Destroy;
end;

function TCodeCompletion.ApplyClassFilter(Statement, CurrentClass: PStatement): boolean;
begin
  Result :=
    (
      (Statement^._Scope in [ssLocal, ssGlobal]) or // local or global var or
      ( (Statement^._Scope = ssClassLocal) and // class var
        (Statement^._ParentScope = CurrentClass) // from current class
      )
    ) and (
      IsIncluded(Statement^._FileName) or
      IsIncluded(Statement^._DefinitionFileName)
    );
end;

procedure TCodeCompletion.AddChildren(ScopeStatement:PStatement);
var
  ChildStatement: PStatement;
  Children : TList;
  i:integer;
begin
  Children := fParser.Statements.GetChildrenStatements(ScopeStatement);
  if not Assigned(Children) then
    Exit;
  for i:=0 to Children.Count-1 do begin
    ChildStatement:=PStatement(Children[i]);
    if fAddedStatements.ValueOf(ChildStatement^._Command) = -1 then begin
      fAddedStatements.Add(ChildStatement^._Command,1);
      fFullCompletionStatementList.Add(ChildStatement);
    end;
  end;
end;

procedure TCodeCompletion.GetCompletionFor(FileName,Phrase: AnsiString);
var
  scopeStatement : PStatement;
  ChildStatement,ClassTypeStatement,namespaceStatement:PStatement;
  namespaceStatementsList: TList;
  Children : TList;
  I,t,k: integer;
  ScopeTypeStatement, Statement : PStatement;
  ScopeName, namespaceName, firstName : AnsiString;
  opType: TOperatorType;
begin
  // Reset filter cache
  fIsIncludedCacheFileName := '';
  fIsIncludedCacheResult := false;


  // Pulling off the same trick as in TCppParser.FindStatementOf, but ignore everything after last operator
  I := fParser.FindLastOperator(Phrase);
  if I = 0 then begin
    scopeStatement := fCurrentStatement;
    // repeat until reach global
    while Assigned(scopeStatement) do begin
      //add members of current scope that not added before
      AddChildren(scopeStatement);
      // add members of all usings (in current scope ) and not added before
      for t:=0 to scopeStatement^._Usings.Count-1 do begin
        namespaceName := scopeStatement^._Usings[t];
        namespaceStatementsList:=fParser.FindNamespace(namespaceName);
        if not Assigned(namespaceStatementsList) then
          continue;
        for k:=0 to namespaceStatementsList.Count-1 do begin
          namespaceStatement:=PStatement(namespaceStatementsList[k]);
          AddChildren(namespaceStatement);
        end;
      end;
      scopeStatement:=scopeStatement^._ParentScope;
    end;

    // add all global members and not added before
    AddChildren(nil);
    // add members of all fusings
    for t:=0 to fUsings.Count-1 do begin
      namespaceName := fUsings[t];
      namespaceStatementsList:=fParser.FindNamespace(namespaceName);
      if not Assigned(namespaceStatementsList) then
        continue;
      for k:=0 to namespaceStatementsList.Count-1 do begin
        namespaceStatement:=PStatement(namespaceStatementsList[k]);
        AddChildren(namespaceStatement);
      end;
    end;
  end else begin
    opType:=GetOperatorType(Phrase,I);
    scopeName := Copy(Phrase,1,I-1);
    namespaceStatementsList := nil;
    //assume it's a namespace
    if OpType = otDColon then
      namespaceStatementsList:=fParser.FindNamespace(namespaceName);
    if assigned(namespaceStatementsList) then begin //yes, it's a namespace
      for k:=0 to namespaceStatementsList.Count-1 do begin
        namespaceStatement:=PStatement(namespaceStatementsList[k]);
        AddChildren(namespaceStatement);
      end;
    end else begin
      Statement := fParser.FindStatementOf(FileName, scopeName,fCurrentStatement);
      if not Assigned(statement) then
        Exit;
      ScopeTypeStatement := fCurrentStatement;
      while Assigned(ScopeTypeStatement) and not (ScopeTypeStatement^._Kind in [skClass]) do begin
        ScopeTypeStatement := ScopeTypeStatement^._ParentScope;
      end;
      if (opType in [otArrow, otDot]) and (statement^._Kind = skVariable) then  begin
        // Get type statement  of current (scope) statement

        ClassTypeStatement:=fParser.FindTypeDefinitionOf(Statement^._Type,fCurrentStatement);
        if (ClassTypeStatement = ScopeTypeStatement) or (statement^._Command = 'this') then begin
          //we can use all members
          AddChildren(ClassTypeStatement);
        end else begin // we can only use public members
          Children := fParser.Statements.GetChildrenStatements(ClassTypeStatement);
          if not Assigned(Children) then
            Exit;
          for i:=0 to Children.Count-1 do begin
            ChildStatement:=PStatement(Children[i]);
            if (ChildStatement^._ClassScope=scsPublic)
              and(fAddedStatements.ValueOf(ChildStatement^._Command) = -1) then begin
              fAddedStatements.Add(ChildStatement^._Command,1);
              fFullCompletionStatementList.Add(ChildStatement);
            end;
          end;
        end;
      //todo friend
      end else if (opType in [otDColon]) and (statement^._Kind = skClass )then begin
        ClassTypeStatement:=fParser.FindTypeDefinitionOf(Statement^._Type,fCurrentStatement);
        if (ClassTypeStatement = ScopeTypeStatement) then begin
          //we can use all static members
          Children := fParser.Statements.GetChildrenStatements(ClassTypeStatement);
          if not Assigned(Children) then
            Exit;
          for i:=0 to Children.Count-1 do begin
            ChildStatement:=PStatement(Children[i]);
            if (ChildStatement^._Static) and (fAddedStatements.ValueOf(ChildStatement^._Command) = -1) then begin
              fAddedStatements.Add(ChildStatement^._Command,1);
              fFullCompletionStatementList.Add(ChildStatement);
            end;
          end;
        end else begin // we can only use public static members
          Children := fParser.Statements.GetChildrenStatements(ClassTypeStatement);
          if not Assigned(Children) then
            Exit;
          for i:=0 to Children.Count-1 do begin
            ChildStatement:=PStatement(Children[i]);
            if (ChildStatement^._Static) and  (ChildStatement^._ClassScope=scsPublic) and(fAddedStatements.ValueOf(ChildStatement^._Command) = -1) then begin
              fAddedStatements.Add(ChildStatement^._Command,1);
              fFullCompletionStatementList.Add(ChildStatement);
            end;
          end;
        end;
        //todo friend
      end;
    end;
  end;
end;

// Return 1 to show Item2 above Item1, otherwise -1

function ListSort(Item1, Item2: Pointer): Integer;
var
  Statement1, Statement2: PStatement;
begin
  Statement1 := PStatement(Item1);
  Statement2 := PStatement(Item2);

  // Show stuff from local headers first
  if (Statement1^._InSystemHeader) and (not Statement2^._InSystemHeader) then begin
    Result := 1;
  end else if (not Statement1^._InSystemHeader) and (Statement2^._InSystemHeader) then begin
    Result := -1;

    // Show local statements first
  end else if (Statement1^._Scope in [ssGlobal]) and not (Statement2^._Scope in [ssGlobal]) then begin
    Result := 1;
  end else if not (Statement1^._Scope in [ssGlobal]) and (Statement2^._Scope in [ssGlobal]) then begin
    Result := -1;

    // otherwise, sort by name
  end else
    Result := CompareText(Statement1^._Command, Statement2^._Command);
end;

procedure TCodeCompletion.FilterList(const Member: AnsiString);
var
  I: integer;
  tmpList:TList;
  lastCmd:String;
begin
  fCompletionStatementList.Clear;
  tmpList:=TList.Create;
  try
    if Member <> '' then begin // filter, case sensitive
      tmpList.Capacity := fFullCompletionStatementList.Count;
      for I := 0 to fFullCompletionStatementList.Count - 1 do
        if StartsStr(Member, PStatement(fFullCompletionStatementList[I])^._Command) then
          tmpList.Add(fFullCompletionStatementList[I]);
    end else
      tmpList.Assign(fFullCompletionStatementList);
    tmpList.sort(@ListSort);
    // filter duplicates
    lastCmd := '';
    for I:=0 to tmpList.Count -1 do begin
      if lastCmd <> PStatement(tmpList[I])^._Command then
         fCompletionStatementList.Add(tmpList[I]);
      lastCmd:=PStatement(tmpList[I])^._Command;
    end;
  finally
    tmpList.Free;
  end;

end;


procedure TCodeCompletion.Show;
begin
  fPreparing:=True;
  // Clear data, do not free pointed memory: data is owned by CppParser
  fAddedStatements.Clear;
  fCompletionStatementList.Clear;
  fFullCompletionStatementList.Clear;
  CodeComplForm.lbCompletion.Items.BeginUpdate;
  CodeComplForm.lbCompletion.Items.Clear;
  CodeComplForm.lbCompletion.Items.EndUpdate;
  fIncludedFiles.Clear; // is recreated anyway on reshow, so save some memory when hiding
  CodeComplForm.Show;
  CodeComplForm.lbCompletion.SetFocus;
end;

procedure TCodeCompletion.Hide;
begin
  if fPreparing then
    Exit;
  OnKeyPress := nil;
  CodeComplForm.Hide;

  // Clear data, do not free pointed memory: data is owned by CppParser
  fCompletionStatementList.Clear;
  fFullCompletionStatementList.Clear;
  CodeComplForm.lbCompletion.Items.BeginUpdate;
  CodeComplForm.lbCompletion.Items.Clear;
  CodeComplForm.lbCompletion.Items.EndUpdate;
  fIncludedFiles.Clear; // is recreated anyway on reshow, so save some memory when hiding
  fUsings.Clear;
  fAddedStatements.Clear;
end;

procedure TCodeCompletion.PrepareSearch(const Phrase, Filename: AnsiString);
begin
  fPreparing:=True;
  fPhrase := Phrase;
  Screen.Cursor := crHourglass;
  fParser.GetFileIncludes(Filename, fIncludedFiles);
  GetCompletionFor(FileName,Phrase);
  CodeComplForm.lbCompletion.Font.Size := FontSize;
  CodeComplForm.lbCompletion.ItemHeight := Round(2 * FontSize);
  CodeComplForm.Update;
  Screen.Cursor := crDefault;
  fPreparing:=False;
end;

function TCodeCompletion.Search(const Phrase, Filename: AnsiString;AutoHideOnSingleResult:boolean):boolean;
var
  I: integer;
  symbol: ansistring;
begin
  Result:=False;

  fPhrase := Phrase;
  if Phrase = '' then begin
    Hide;
    Exit;
  end;

  if fEnabled then begin
    Screen.Cursor := crHourglass;


    // Sort here by member
    I := fParser.FindLastOperator(Phrase);
    while (I > 0) and (I <= Length(Phrase)) and (Phrase[i] in ['.', ':', '-', '>']) do
      Inc(I);

    symbol := Copy(Phrase, I, MaxInt);
    // filter fFullCompletionStatementList to fCompletionStatementList
    FilterList(symbol);


    if fCompletionStatementList.Count > 0 then begin
      CodeComplForm.lbCompletion.Items.BeginUpdate;
      try
        CodeComplForm.lbCompletion.Items.Clear;

        // Only slow one hundred statements...
        for I := 0 to min(fShowCount, fCompletionStatementList.Count - 1) do begin
          CodeComplForm.lbCompletion.Items.AddObject('', fCompletionStatementList[I]);
        end;
      finally
        CodeComplForm.lbCompletion.Items.EndUpdate;
      end;

      // if only one suggestion, and is exactly the symbol to search, hide the frame (the search is over)
      if (fCompletionStatementList.Count =1) and
        SameStr(symbol, PStatement(fCompletionStatementList[0])^._Command) then begin
        Result:=True;
        Exit;
      end;

      // if only one suggestion and auto hide , don't show the frame
      if (fCompletionStatementList.Count =1) and AutoHideOnSingleResult then begin
        Result:=True;
        Exit;
      end;

      //CodeComplForm.Show;
      if CodeComplForm.lbCompletion.Items.Count > 0 then
        CodeComplForm.lbCompletion.ItemIndex := 0;
    end else begin
      CodeComplForm.lbCompletion.Items.Clear;
      Hide;
    end;

    Screen.Cursor := crDefault;
  end;
end;

function TCodeCompletion.SelectedStatement: PStatement;
begin
  if fEnabled then begin
    if (fCompletionStatementList.Count > CodeComplForm.lbCompletion.ItemIndex) and (CodeComplForm.lbCompletion.ItemIndex
      <> -1) then
      Result := PStatement(fCompletionStatementList[CodeComplForm.lbCompletion.ItemIndex])
    else begin
      if fCompletionStatementList.Count > 0 then
        Result := PStatement(fCompletionStatementList[0])
      else
        Result := nil;
    end;
  end else
    Result := nil;
end;

procedure TCodeCompletion.SetPosition(Value: TPoint);
begin
  fPos := Value;
  if fPos.X + fWidth > Screen.Width then
    CodeComplForm.Left := fPos.X - fWidth
  else
    CodeComplForm.Left := fPos.X;
  if fPos.Y + fHeight > Screen.Height then
    CodeComplForm.Top := fPos.Y - fHeight - 16
  else
    CodeComplForm.Top := fPos.Y;
end;

procedure TCodeCompletion.OnFormResize(Sender: TObject);
begin
  if Enabled then begin
    fWidth := CodeComplForm.Width;
    fHeight := CodeComplForm.Height;
    if Assigned(fOnResize) then
      fOnResize(Self);
  end;
end;

function TCodeCompletion.IsIncluded(const FileName: AnsiString): boolean;
begin
  // Only do the slow check if the cache is invalid
  if not SameStr(FileName, fIsIncludedCacheFileName) then begin
    fIsIncludedCacheFileName := FileName;
    fIsIncludedCacheResult := FastIndexOf(fIncludedFiles, FileName) <> -1;
  end;

  // Cache has been updated. Use it.
  Result := fIsIncludedCacheResult;
end;

function TCodeCompletion.IsVisible: boolean;
begin
  Result := fEnabled and CodeComplForm.Visible;
end;

end.

