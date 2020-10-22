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
  cbutils, IntList, StatementList;
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
    fOnResize: TNotifyEvent;
    fOnlyGlobals: boolean;
    fCurrentStatement: PStatement;
    fIncludedFiles: TStringList;
    fIsIncludedCacheFileName: AnsiString;
    fIsIncludedCacheResult: boolean;
    function ApplyClassFilter(Statement, CurrentClass: PStatement): boolean;
    procedure GetCompletionFor(Phrase: AnsiString);
    procedure FilterList(const Member: AnsiString);
    procedure SetPosition(Value: TPoint);
    procedure OnFormResize(Sender: TObject);
    function IsIncluded(const FileName: AnsiString): boolean;
    function IsVisible: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Search(const Phrase, Filename: AnsiString; AutoHideOnSingleResult:boolean):boolean;
    procedure Hide;
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
  inherited Destroy;
end;

function TCodeCompletion.ApplyClassFilter(Statement, CurrentClass: PStatement): boolean;
begin
  Result :=
    (
      (Statement^._Scope in [ssLocal, ssGlobal]) or // local or global var or
      ( (Statement^._Scope = ssClassLocal) and // class var
        (Statement^._Parent = CurrentClass) // from current class
      )
    ) and (
      IsIncluded(Statement^._FileName) or
      IsIncluded(Statement^._DefinitionFileName)
    );
end;

procedure TCodeCompletion.GetCompletionFor(Phrase: AnsiString);
var
  Node: PStatementNode;
  ChildStatement: PStatement;
  I,t: integer;
  ScopeTypeStatement, Statement, TypeStatement: PStatement;
  Children : TList;
  isThis: boolean;
  ScopeName : AnsiString;
  opType: TOperatorType;
begin
  // Reset filter cache
  fIsIncludedCacheFileName := '';
  fIsIncludedCacheResult := false;

  // Get type statement  of current (scope) statement
  ScopeTypeStatement := fCurrentStatement;
  while Assigned(ScopeTypeStatement) and not (ScopeTypeStatement^._Kind = skClass) do begin
    ScopeTypeStatement := ScopeTypeStatement^._Parent;
  end;

  // Pulling off the same trick as in TCppParser.FindStatementOf, but ignore everything after last operator
    I := fParser.FindLastOperator(Phrase);
    if I = 0 then begin

      // only add globals and members of the current class
      //todo: namespace support

      // Also consider classes the current class inherits from
      Node := fParser.Statements.FirstNode;
      while Assigned(Node) do begin
        Statement := Node^.Data;
        if ApplyClassFilter(Statement, ScopeTypeStatement) then begin
          fFullCompletionStatementList.Add(Statement);
        end;
        Node := Node^.NextNode;
      end;

    end else begin

      opType:=GetOperatorType(Phrase,I);

      // Find last operator
      Delete(Phrase, I, MaxInt);

      // Add statements of all the text before the last operator
      Statement := fParser.FindStatementOf(Phrase, fCurrentStatement);
      if not Assigned(Statement) then
        Exit;

      //get scopename, for friend check;
      ScopeName := '';
      if Assigned(ScopeTypeStatement) then
        ScopeName := ScopeTypeStatement^._Command;

      // It is a Class, so only show static members
      if (Statement^._Kind = skClass) and (opType = otDColon) and Assigned(ScopeTypeStatement) then begin
        //Filter static members
        Children := fParser.Statements.GetChildrenStatements(Statement);
        if Assigned(Children) then begin
          for t:=0 to Children.Count-1 do begin
            ChildStatement := PStatement(Children[t]);
            if (ChildStatement^._Static) and
              (
                (ChildStatement^._ClassScope in [scsPublic,scsNone])
                or (Statement = ScopeTypeStatement) //we are inside the class
                or (                                     // we are inside the classes friend
                   Assigned(Statement^._Friends) and
                  (Statement^._Friends.ValueOf(ScopeName)>=0)
                   )
                or (
                 (ChildStatement^._ClassScope =scsProtected) and
                 IsAncestor(ScopeTypeStatement, Statement) // Is an ancestor of our scope class
                )
              ) then
              fFullCompletionStatementList.Add(ChildStatement);
          end;
        end;

      end else if (Statement^._Kind = skClass) and
              (opType = otDColon) and
               not Assigned(ScopeTypeStatement) then begin
        // we are defining class member functions, only show them
        Children := fParser.Statements.GetChildrenStatements(Statement);
        if Assigned(Children) then begin
          for t:=0 to Children.Count-1 do begin
            ChildStatement := PStatement(Children[t]);
            if (ChildStatement^._Kind in [skFunction, skConstructor,skDestructor]) then
              fFullCompletionStatementList.Add(ChildStatement);
          end;
        end;
      end else if (Statement^._Kind = skVariable) and  (opType in [otArrow, otDot]) then  begin
        //It's a var we should show its type's members
        TypeStatement := fParser.FindTypeDefinitionOf(Statement^._Type, ScopeTypeStatement);
        if Assigned(TypeStatement) then begin
          isThis :=  SameStr(Statement^._Command,'this');
          Children := fParser.Statements.GetChildrenStatements(TypeStatement);
          if Assigned(Children) then begin
            for t:=0 to Children.Count-1 do begin
              ChildStatement := PStatement(Children[t]);
              if (SameStr(ChildStatement^._Command,'this')) then
                Continue;
              if (isThis) or
                 (ChildStatement^._ClassScope in [scsPublic,scsNone]) or
                 (// we are inside the classes friend
                    Assigned(TypeStatement^._Friends) and
                    (TypeStatement^._Friends.ValueOf(ScopeName)>=0)
                  ) or
                  (
                   (ChildStatement^._ClassScope =scsProtected) and
                    IsAncestor(ScopeTypeStatement, TypeStatement) // Is an ancestor of our scope class
                   )
                  then
                fFullCompletionStatementList.Add(ChildStatement);
            end;
          end;
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

procedure TCodeCompletion.Hide;
begin
  OnKeyPress := nil;
  CodeComplForm.Hide;

  // Clear data, do not free pointed memory: data is owned by CppParser
  fCompletionStatementList.Clear;
  fFullCompletionStatementList.Clear;
  CodeComplForm.lbCompletion.Items.BeginUpdate;
  CodeComplForm.lbCompletion.Items.Clear;
  CodeComplForm.lbCompletion.Items.EndUpdate;
  fIncludedFiles.Clear; // is recreated anyway on reshow, so save some memory when hiding
end;

function TCodeCompletion.Search(const Phrase, Filename: AnsiString;AutoHideOnSingleResult:boolean):boolean;
var
  I: integer;
  symbol: ansistring;
begin
  Result:=False;

  if Phrase = '' then begin
    Hide;
    Exit;
  end;
    
  if fEnabled then begin

    Screen.Cursor := crHourglass;

    // only perform full new search if just invoked
    if not CodeComplForm.Showing then begin
      fParser.GetFileIncludes(Filename, fIncludedFiles);
      GetCompletionFor(Phrase);
      CodeComplForm.lbCompletion.Font.Size := FontSize;
      CodeComplForm.lbCompletion.ItemHeight := Round(2 * FontSize);
      CodeComplForm.Update;
    end;


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

      CodeComplForm.Show;
      CodeComplForm.lbCompletion.SetFocus;
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

