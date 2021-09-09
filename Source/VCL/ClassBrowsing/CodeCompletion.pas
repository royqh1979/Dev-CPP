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
  Windows, Classes, Forms, SysUtils, Controls, Graphics, CppParser,
  cbutils, StatementList, iniFiles, SyncObjs;

type
  TCodeCompletion = class(TComponent)
  private
    fCodeInsList: TList; // TList<PCodeIns> CodeInsList (Code template list)
    fCodeInsStatements: TList; //TList<Statement> temporary (user code template) statements created when show code suggestion
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
    fUsings: TDevStringList;
    fIsIncludedCacheFileName: AnsiString;
    fIsIncludedCacheResult: boolean;
    fAddedStatements : TStringHash;
    fPreparing: boolean;
    fPhrase : AnsiString;
    fSymbolUsage:THashedStringList;
    fRecordUsage: boolean;
    fShowKeywords: boolean;
    fShowCodeIns: boolean;
    fIgnoreCase:boolean;
    fCriticalSection: TCriticalSection;
    fParserSerialId: String;
    fSortByScope: boolean;
    fUseCppKeyword: boolean;
    procedure GetCompletionFor(FileName,Phrase: AnsiString; Line:integer);
    procedure FilterList(const Member: AnsiString);
    procedure SetPosition(Value: TPoint);
    procedure OnFormResize(Sender: TObject);
    function IsIncluded(const FileName: AnsiString): boolean;
    function IsVisible: boolean;
    procedure AddStatement(statement:PStatement; FileName: String; Line:integer);
    procedure AddChildren(ScopeStatement:PStatement; FileName: String; Line:Integer);
    function GetColor(i:integer):TColor;
    procedure SetColor(i:integer; const Color:TColor);
    procedure SetParser(parser:TCppParser);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareSearch(const Phrase, Filename: AnsiString; Line:integer);
    function Search(const Phrase, Filename: AnsiString; AutoHideOnSingleResult:boolean):boolean;
    procedure Hide;
    procedure Show;
    function FreezeParser:boolean;
    procedure UnfreezeParser;
    function SelectedStatement: PStatement;
    property CurrentStatement: PStatement read fCurrentStatement write fCurrentStatement;
    property CodeInsList: TList read fCodeInsList write fCodeInsList;
    property SymbolUsage:THashedStringList read fSymbolUsage write fSymbolUsage;
    property RecordUsage: boolean read fRecordUsage write fRecordUsage;
    property Colors[Index: Integer]: TColor read GetColor write SetColor;
    property ShowKeywords: boolean read fShowKeywords write fShowKeywords;
    property IgnoreCase: boolean read fIgnoreCase write fIgnoreCase;
    property SortByScope: boolean read fSortByScope write fSortByScope;
    property UseCppKeyword: boolean read fUseCppKeyword write fUseCppKeyword;
  published
    property ShowCount: integer read fShowCount write fShowCount;
    property ShowCodeIns: boolean read fShowCodeIns write fShowCodeIns;
    property Parser: TCppParser read fParser write setParser;
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

  fShowKeywords:=True;

  fUseCppKeyword:=True;

  fCodeInsStatements:=TList.Create;

  fIncludedFiles := TStringList.Create;
  fIncludedFiles.Sorted := True;
  fIncludedFiles.Duplicates := dupIgnore;
  fCriticalSection := TCriticalSection.Create;

  fUsings:=TDevStringList.Create;
  fUsings.Sorted := True;
  fUsings.Duplicates:=dupIgnore;
  fAddedStatements := TStringHash.Create(1000);
  fCompletionStatementList := TList.Create;
  fFullCompletionStatementList := TList.Create;

  CodeComplForm := TCodeComplForm.Create(Self);
  CodeComplForm.OnResize := OnFormResize;

  fWidth := 320;
  fHeight := 240;
  fColor := clWindow;
  fEnabled := True;
  fOnlyGlobals := False;
  fShowCount := 1000;
  fShowCodeIns:=True;

  fIsIncludedCacheFileName := '';
  fIsIncludedCacheResult := false;

  fIgnoreCase := false;
end;

destructor TCodeCompletion.Destroy;
var
  i:integer;
begin
  FreeAndNil(CodeComplForm);
  FreeAndNil(fCompletionStatementList);
  FreeAndNil(fFullCompletionStatementList);
  FreeAndNil(fIncludedFiles);
  FreeAndNil(fUsings);
  FreeAndNil(fAddedStatements);
  for i:= 0 to  fCodeInsStatements.Count-1 do begin
    dispose(PStatement(fCodeInsStatements[i]));
  end;
  FreeAndNil(fCodeInsStatements);
  fCriticalSection.Free;
  inherited Destroy;
end;

procedure TCodeCompletion.AddStatement(statement:PStatement; FileName: String; Line:integer);
begin
  if (Line <> -1) and (Line < statement^._Line) and SameText(FileName,statement^._FileName)  then
    Exit;
  fAddedStatements.Add(statement^._Command,1);
  fFullCompletionStatementList.Add(statement);
end;

procedure TCodeCompletion.AddChildren(ScopeStatement:PStatement; FileName: String; Line:integer);
var
  ChildStatement: PStatement;
  Children : TList;
  i:integer;

begin
  if assigned(ScopeStatement) and not IsIncluded(ScopeStatement^._FileName)
    and not IsIncluded(ScopeStatement^._DefinitionFileName) then
    Exit ;
  Children := fParser.Statements.GetChildrenStatements(ScopeStatement);
  if not Assigned(Children) then
    Exit;

  if not Assigned(ScopeStatement) then begin //Global scope
    for i:=0 to Children.Count-1 do begin
      ChildStatement:=PStatement(Children[i]);
      if (ChildStatement^._FileName = '') then begin
        // hard defines
        AddStatement(ChildStatement,FileName,-1);
      end else if not( ChildStatement^._Kind in [skConstructor, skDestructor, skBlock])
        and (fAddedStatements.ValueOf(ChildStatement^._Command) <0)
        and (
          IsIncluded(ChildStatement^._FileName)
          or IsIncluded(ChildStatement^._DefinitionFileName)
        ) then begin //we have to for file include for symbols in the global scope
        AddStatement(ChildStatement,FileName,Line);
      end;
    end;
  end else begin
    for i:=0 to Children.Count-1 do begin
      ChildStatement:=PStatement(Children[i]);
      if not( ChildStatement^._Kind in [skConstructor, skDestructor, skBlock])
        and (fAddedStatements.ValueOf(ChildStatement^._Command) <0) then begin
        AddStatement(ChildStatement,FileName,Line);
      end;
    end;
  end;
end;

procedure TCodeCompletion.GetCompletionFor(FileName,Phrase: AnsiString; Line:integer);
var
  scopeStatement : PStatement;
  ParentTypeStatement,ChildStatement,ClassTypeStatement,namespaceStatement:PStatement;
  namespaceStatementsList: TList;
  Children : TList;
  lastI,I,t,k: integer;
  LastScopeStatement, ScopeTypeStatement, Statement : PStatement;
  LastScopeName, ScopeName, namespaceName,typeName: AnsiString;
  opType: TOperatorType;
  codeIn:PCodeIns;
  codeInStatement:PStatement;
begin
  if not assigned(fParser) then
    Exit;
  if not fParser.Enabled then
    Exit;
  if not fParser.Freeze then
    Exit;
  try
  // Reset filter cache
  fIsIncludedCacheFileName := '';
  fIsIncludedCacheResult := false;

  if StartsStr('#',Phrase) then begin
    if fShowKeywords then begin
      //add keywords
      for i:=0 to CppDirectiveList.Count-1 do begin
        new(codeInStatement);
        codeInStatement^._Command := CppDirectiveList[i];
        codeInStatement^._Kind := skPreprocessor;
        codeInStatement^._FullName := CppDirectiveList[i];
        fCodeInsStatements.Add(pointer(codeInStatement));
        fFullCompletionStatementList.Add(pointer(codeInStatement));
      end;
    end;
    Exit;
  end;

  if StartsStr('@',Phrase) then begin
    if fShowKeywords then begin
      //add keywords
      for i:=0 to JavadocTags.Count-1 do begin
        new(codeInStatement);
        codeInStatement^._Command := JavadocTags[i];
        codeInStatement^._Kind := skKeyword;
        codeInStatement^._FullName := JavadocTags[i];
        fCodeInsStatements.Add(pointer(codeInStatement));
        fFullCompletionStatementList.Add(pointer(codeInStatement));
      end;
    end;
    Exit;
  end;

  // Pulling off the same trick as in TCppParser.FindStatementOf, but ignore everything after last operator
  I := fParser.FindLastOperator(Phrase);
  if (I = 0) then begin

    if fShowCodeIns then begin
      //add templates
      for i:=0 to fCodeInsList.Count-1 do begin
        codeIn:=PCodeIns(fCodeInsList[i]);
        new(codeInStatement);
        codeInStatement^._Command := codeIn.Prefix;
        codeInStatement^._Value := codeIn.Code;
        codeInStatement^._Kind := skUserCodeIn;
        codeInStatement^._FullName := codeIn.Prefix;
        fCodeInsStatements.Add(pointer(codeInStatement));
        fFullCompletionStatementList.Add(pointer(codeInStatement));
      end;
    end;

    if fShowKeywords then begin
      //add keywords
      if fUseCppKeyword then begin
        for i:=0 to CppKeywordsList.Count-1 do begin
          new(codeInStatement);
          codeInStatement^._Command := CppKeywordsList[i];
          codeInStatement^._Kind := skKeyword;
          codeInStatement^._FullName := CppKeywordsList[i];
          fCodeInsStatements.Add(pointer(codeInStatement));
          fFullCompletionStatementList.Add(pointer(codeInStatement));
        end;
      end else begin
        for i:=0 to CKeywordsList.Count-1 do begin
          new(codeInStatement);
          codeInStatement^._Command := CKeywordsList[i];
          codeInStatement^._Kind := skKeyword;
          codeInStatement^._FullName := CKeywordsList[i];
          fCodeInsStatements.Add(pointer(codeInStatement));
          fFullCompletionStatementList.Add(pointer(codeInStatement));
        end;
      end;
    end;

    scopeStatement := fCurrentStatement;
    // repeat until reach global
    while Assigned(scopeStatement) do begin
      //add members of current scope that not added before
      if scopeStatement^._Kind = skClass then
        AddChildren(scopeStatement, FileName, -1)
      else
        AddChildren(scopeStatement, FileName, Line);
      // add members of all usings (in current scope ) and not added before
      for t:=0 to scopeStatement^._Usings.Count-1 do begin
        namespaceName := scopeStatement^._Usings[t];
        namespaceStatementsList:=fParser.FindNamespace(namespaceName);
        if not Assigned(namespaceStatementsList) then
          continue;
        for k:=0 to namespaceStatementsList.Count-1 do begin
          namespaceStatement:=PStatement(namespaceStatementsList[k]);
          AddChildren(namespaceStatement, FileName, Line);
        end;
      end;
      scopeStatement:=scopeStatement^._ParentScope;
    end;

    // add all global members and not added before
    AddChildren(nil, FileName, Line);

    fParser.GetFileUsings(FileName,fUsings);
    // add members of all fusings
    for t:=0 to fUsings.Count-1 do begin
      namespaceName := fUsings[t];
      namespaceStatementsList:=fParser.FindNamespace(namespaceName);
      if not Assigned(namespaceStatementsList) then
        continue;
      for k:=0 to namespaceStatementsList.Count-1 do begin
        namespaceStatement:=PStatement(namespaceStatementsList[k]);
        AddChildren(namespaceStatement, FileName, Line);
      end;
    end;
  end else begin
    opType:=GetOperatorType(Phrase,I);
    scopeName := Copy(Phrase,1,I-1);
    namespaceStatementsList := nil;
    if (OpType = otDColon) and (scopeName = '') then begin
      // start with '::', we only find in global
     // add all global members and not added before
      AddChildren(nil, FileName, Line);
      Exit;
    end else
      //assume it's a namespace
      namespaceStatementsList:=fParser.FindNamespace(scopeName);
    if assigned(namespaceStatementsList) then begin //yes, it's a namespace
      for k:=0 to namespaceStatementsList.Count-1 do begin
        namespaceStatement:=PStatement(namespaceStatementsList[k]);
        AddChildren(namespaceStatement, FileName, Line);
      end;
    end else begin
      Statement := fParser.FindStatementOf(FileName, scopeName,fCurrentStatement,ParentTypeStatement);
      if not Assigned(statement) then
        Exit;
      ScopeTypeStatement := fCurrentStatement;
      while Assigned(ScopeTypeStatement) and not (ScopeTypeStatement^._Kind in ScopeTypeKinds) do begin
        ScopeTypeStatement := ScopeTypeStatement^._ParentScope;
      end;
      if (opType in [otArrow, otDot]) and (statement^._Kind in [skVariable, skParameter,skFunction]) then  begin
        // Get type statement  of current (scope) statement

        // it's an element method of STL container
        if (statement^._Kind = skFunction)
          and assigned(statement^._ParentScope)
          and  (STLContainers.ValueOf(statement^._ParentScope^._FullName)>0)
          and (STLElementMethods.ValueOf(statement^._Command)>0) then begin
          lastI:=fParser.FindLastOperator(scopeName);
          LastScopeName := Copy(scopeName,1,lastI-1);
          LastScopeStatement := fParser.FindStatementOf(FileName, LastScopeName,fCurrentStatement,ParentTypeStatement);
          if not Assigned(LastScopeStatement) then
            Exit;
          typeName:= fParser.FindFirstTemplateParamOf(fileName,LastScopeStatement^._Type,LastScopeStatement^._ParentScope);
          ClassTypeStatement:=fParser.FindTypeDefinitionOf(FileName, typeName,LastScopeStatement^._ParentScope);
        end else
          ClassTypeStatement:=fParser.FindTypeDefinitionOf(FileName, Statement^._Type,ParentTypeStatement);

        if not Assigned(ClassTypeStatement) then
          Exit;
        //is a smart pointer
        if (STLPointers.ValueOf(ClassTypeStatement^._FullName)>=0)
           and (opType=otArrow) then begin
          typeName:= fParser.FindFirstTemplateParamOf(fileName,Statement^._Type,statement^._ParentScope);
          ClassTypeStatement:=fParser.FindTypeDefinitionOf(FileName, typeName,statement^._ParentScope);
          if not Assigned(ClassTypeStatement) then
            Exit;
        end;    //is a stl container operator[]
        if (STLContainers.ValueOf(ClassTypeStatement^._FullName)>0)
          and EndsStr(']',scopeName) then begin
          typeName:= fParser.FindFirstTemplateParamOf(fileName,Statement^._Type,statement^._ParentScope);
          ClassTypeStatement:=fParser.FindTypeDefinitionOf(FileName, typeName,statement^._ParentScope);
          if not Assigned(ClassTypeStatement) then
            Exit;
        end;
        if not IsIncluded(ClassTypeStatement^._FileName) and
          not IsIncluded(ClassTypeStatement^._DefinitionFileName) then
          Exit;
        if (ClassTypeStatement = ScopeTypeStatement) or (statement^._Command = 'this') then begin
          //we can use all members
          AddChildren(ClassTypeStatement,FileName,-1);
        end else begin // we can only use public members
          Children := fParser.Statements.GetChildrenStatements(ClassTypeStatement);
          if not Assigned(Children) then
            Exit;
          for i:=0 to Children.Count-1 do begin
            ChildStatement:=PStatement(Children[i]);
            if (ChildStatement^._ClassScope=scsPublic)
              and not (ChildStatement^._Kind in [skConstructor,skDestructor])
              and( fAddedStatements.ValueOf(ChildStatement^._Command) <0) then begin
              AddStatement(ChildStatement,FileName,-1);
            end;
          end;
        end;
      //todo friend
      end else if (opType in [otDColon]) and (statement^._Kind = skClass )then begin
        ClassTypeStatement:=statement;
        if not Assigned(ClassTypeStatement) then
          Exit;
        if not IsIncluded(ClassTypeStatement^._FileName)
          and IsIncluded(ClassTypeStatement^._DefinitionFileName) then
          Exit;
        if (ClassTypeStatement = ScopeTypeStatement) then begin
          //we can use all static members
          Children := fParser.Statements.GetChildrenStatements(ClassTypeStatement);
          if not Assigned(Children) then
            Exit;
          for i:=0 to Children.Count-1 do begin
            ChildStatement:=PStatement(Children[i]);
            if (
              (ChildStatement^._Static)
              or (ChildStatement^._Kind in [skTypedef,skClass])
              ) and (fAddedStatements.ValueOf(ChildStatement^._Command) <0) then begin
              AddStatement(ChildStatement,FileName,-1);
            end
          end;
        end else begin // we can only use public static members
          Children := fParser.Statements.GetChildrenStatements(ClassTypeStatement);
          if not Assigned(Children) then
            Exit;
          for i:=0 to Children.Count-1 do begin
            ChildStatement:=PStatement(Children[i]);
            if (
              (ChildStatement^._Static)
              or (ChildStatement^._Kind in [skTypedef,skClass,skEnum,skEnumType])
              )
              and  (ChildStatement^._ClassScope=scsPublic)
              and(fAddedStatements.ValueOf(ChildStatement^._Command) <0) then begin
              AddStatement(ChildStatement,FileName,-1);
            end;
          end;
        end;
        //todo friend
      end;
    end;
  end;
  finally
    fParser.UnFreeze;
    fParserSerialId:=fParser.SerialId;
  end;
end;

// Return 1 to show Item2 above Item1, otherwise -1

function ListSort(Item1, Item2: Pointer): Integer;
var
  Statement1, Statement2: PStatement;
begin
  Statement1 := PStatement(Item1);
  Statement2 := PStatement(Item2);

  // Show user template first
  if (Statement1^._Kind = skUserCodeIn) then begin
    if not (Statement2^._Kind = skUserCodeIn) then begin
      Result := -1;
    end else begin
      Result := CompareText(Statement1^._Command, Statement2^._Command);
    end;
  end else if (Statement2^._Kind = skUserCodeIn) then begin
    Result := 1;
  // show keywords first
  end else if (Statement1^._Kind = skKeyword) and (not (Statement2^._Kind = skKeyword)) then begin
    Result := -1;
  end else if (not (Statement1^._Kind = skKeyword)) and (Statement2^._Kind = skKeyword) then begin
    Result := 1;
  end else
    Result := CompareText(Statement1^._Command, Statement2^._Command);
end;

function ListSortByScope(Item1, Item2: Pointer): Integer;
var
  Statement1, Statement2: PStatement;
  ret: integer;
begin
  Statement1 := PStatement(Item1);
  Statement2 := PStatement(Item2);


  // Show user template first
  if (Statement1^._Kind = skUserCodeIn) then begin
    if not (Statement2^._Kind = skUserCodeIn) then begin
      Result := -1;
    end else begin
      Result := CompareText(Statement1^._Command, Statement2^._Command);
    end;
  end else if (Statement2^._Kind = skUserCodeIn) then begin
    Result := 1;
  // show keywords first
  end else if (Statement1^._Kind = skKeyword) and (not (Statement2^._Kind = skKeyword)) then begin
    Result := -1;
  end else if (not (Statement1^._Kind = skKeyword)) and (Statement2^._Kind = skKeyword) then begin
    Result := 1;
  // Show stuff from local headers first
  end else if (Statement1^._InSystemHeader) and (not Statement2^._InSystemHeader) then begin
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

function ListSortWithUsage(Item1, Item2: Pointer): Integer;
var
  Statement1, Statement2: PStatement;
begin
  Statement1 := PStatement(Item1);
  Statement2 := PStatement(Item2);

  // Show user template first
  if (Statement1^._Kind = skUserCodeIn) then begin
    if not (Statement2^._Kind = skUserCodeIn) then begin
      Result := -1;
    end else begin
      Result := CompareText(Statement1^._Command, Statement2^._Command);
    end;
  end else if (Statement2^._Kind = skUserCodeIn) then begin
    Result := 1;
  end else if (Statement1^._FreqTop <> Statement2^._FreqTop) then begin
    Result := Statement2^._FreqTop - Statement1^._FreqTop;
  // show keywords first
  end else if (Statement1^._Kind = skKeyword) and (not (Statement2^._Kind = skKeyword)) then begin
    Result := -1;
  end else if (not (Statement1^._Kind = skKeyword)) and (Statement2^._Kind = skKeyword) then begin
    Result := 1;
  // Show stuff from local headers first
  end else if (Statement1^._InSystemHeader) and (not Statement2^._InSystemHeader) then begin
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
  I,idx: integer;
  tmpList:TList;
//  lastCmd:String;
  TopCount,SecondCount,ThirdCount:integer;
  usageCount:integer;
begin
  fCriticalSection.Acquire;
  try
    fCompletionStatementList.Clear;

    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze then
      Exit;
    try
      if not SameStr(fParser.SerialId, fParserSerialId) then
        Exit;

      tmpList:=fCompletionStatementList;
      if Member <> '' then begin // filter, case sensitive
        tmpList.Capacity := fFullCompletionStatementList.Count;
        for I := 0 to fFullCompletionStatementList.Count - 1 do
          if ignoreCase and StartsText(Member, PStatement(fFullCompletionStatementList[I])^._Command) then begin
            tmpList.Add(fFullCompletionStatementList[I]);
          end else if StartsStr(Member, PStatement(fFullCompletionStatementList[I])^._Command) then
            tmpList.Add(fFullCompletionStatementList[I]);
      end else
        tmpList.Assign(fFullCompletionStatementList);
      if RecordUsage then begin
        TopCount:=0; SecondCount:=0; ThirdCount:=0;
        for I:=0 to tmpList.Count -1 do begin
          if PStatement(tmpList[I])^._UsageCount = 0 then begin
            idx:=SymbolUsage.IndexOf(PStatement(tmpList[I])^._FullName);
            if idx<0 then
              continue;
            usageCount := integer(SymbolUsage.Objects[idx]);
            PStatement(tmpList[I])^._UsageCount := usageCount;
          end else
            usageCount := PStatement(tmpList[I])^._UsageCount;
          if usageCount>TopCount then begin
            ThirdCount := SecondCount;
            SecondCount := TopCount;
            TopCount:=usageCount;
          end else if usageCount>SecondCount then begin
            ThirdCount := SecondCount;
            SecondCount :=usageCount;
          end else if usageCount>ThirdCount then begin
            ThirdCount := usageCount;
          end;
        end;
        for I:=0 to tmpList.Count -1 do begin
          if PStatement(tmpList[I])^._UsageCount = 0 then begin
            PStatement(tmpList[I])^._FreqTop :=0;
          end else if PStatement(tmpList[I])^._UsageCount = TopCount then begin
            PStatement(tmpList[I])^._FreqTop :=30;
          end else if PStatement(tmpList[I])^._UsageCount = SecondCount then begin
            PStatement(tmpList[I])^._FreqTop :=20;
          end else if PStatement(tmpList[I])^._UsageCount = ThirdCount then begin
            PStatement(tmpList[I])^._FreqTop :=10;
          end;
        end;
        tmpList.sort(@ListSortWithUsage);
      end else if fSortByScope then begin
        tmpList.sort(@ListSortByScope);
      end else begin
        tmpList.sort(@ListSort);
      end;
    finally
      fParser.UnFreeze;
    end;
  finally
    fCriticalSection.Release;
  end;
end;


procedure TCodeCompletion.Show;
begin
  fCriticalSection.Acquire;
  try
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
  finally
    fCriticalSection.Release;
  end;
end;

procedure TCodeCompletion.Hide;
var
  i:integer;
begin
  if fPreparing then
    Exit;
  OnKeyPress := nil;
  CodeComplForm.Hide;
  //Clear Code Statements
  for i:= 0 to  fCodeInsStatements.Count-1 do begin
    dispose(PStatement(fCodeInsStatements[i]));
  end;
  fCodeInsStatements.Clear;
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

procedure TCodeCompletion.PrepareSearch(const Phrase, Filename: AnsiString; Line:integer);
begin
  fCriticalSection.Acquire;
  try
      fPreparing:=True;
      fPhrase := Phrase;
      Screen.Cursor := crHourglass;
      fParser.GetFileIncludes(Filename, fIncludedFiles);
      GetCompletionFor(FileName,Phrase,Line);
      CodeComplForm.lbCompletion.Font.Size := FontSize;
      CodeComplForm.lbCompletion.ItemHeight := CodeComplForm.lbCompletion.Canvas.TextHeight('F')+6;
      // Round(2 * FontSize);
      //CodeComplForm.Update;
      Screen.Cursor := crDefault;
      fPreparing:=False;
  finally
    fCriticalSection.Release;
  end;
end;

function TCodeCompletion.Search(const Phrase, Filename: AnsiString;AutoHideOnSingleResult:boolean):boolean;
var
  I: integer;
  symbol: ansistring;
begin
  fCriticalSection.Acquire;
  try
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

        // Only show one hundred statements...
          for I := 0 to min(fShowCount, fCompletionStatementList.Count - 1) do begin
            CodeComplForm.lbCompletion.Items.AddObject('', fCompletionStatementList[I]);
          end;
        finally
          CodeComplForm.lbCompletion.Items.EndUpdate;
          Screen.Cursor := crDefault;
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
  finally
    fCriticalSection.Release;
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
  if (fPos.X + fWidth > Screen.Width) and (fPos.X - fWidth>0) then
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
  if not SameText(FileName, fIsIncludedCacheFileName) then begin
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

function TCodeCompletion.GetColor(i:integer):TColor;
begin
  Result := CodeComplForm.Colors[i];
end;

procedure TCodeCompletion.SetColor(i:integer; const Color:TColor);
begin
  CodeComplForm.Colors[i] := Color;
end;

procedure TCodeCompletion.SetParser(parser:TCppParser);
begin
  if parser = fParser then
    Exit;
  fCriticalSection.Acquire;
  try
    fParser:=Parser;
  finally
    fCriticalSection.Release;
  end;
end;

function TCodeCompletion.FreezeParser:boolean;
begin
  Result := False;
  if not assigned(fParser) then
    Exit;
  if not fParser.enabled then
    Exit;
  if not fParser.Freeze then
    Exit;
  if not SameStr(fParserSerialId, fParser.SerialId) then begin
    fParser.Unfreeze;
    Exit;
  end;
  Result:=True;
end;

procedure TCodeCompletion.UnfreezeParser;
begin
  fParser.Unfreeze;
end;

end.

