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

unit CppParser;

interface

uses
  Dialogs, Windows, Classes, SysUtils, StrUtils, ComCtrls, StatementList, CppTokenizer, CppPreprocessor,
  cbutils, IntList,SyncObjs, iniFiles,Controls, Forms;


type

  PCppParserProgressMessage = ^TCppParserProgressMessage;
  TCppParserProgressMessage = Record
    Total:integer;
    Current:integer;
    FileName:String;
  end;
  TCppParser = class(TComponent)
  private
    fUniqId : integer;
    fEnabled: boolean;
    fIndex: integer;
    fIsHeader: boolean;
    fIsSystemHeader: boolean;
    fCurrentFile: AnsiString;
    fHandle : HWND;
    { stack list , each element is a list of one/many scopes(like intypedef struct  s1,s2;}
    { It's used for store scope nesting infos }
    fCurrentScope: TList;  //TList<PStatement>
    fCurrentClassScope: TList; // TList<TStatementClassScope>
    { the start index in tokens to skip to ; when parsing typedef struct we need to skip
      the names after the closing bracket because we have processed it }
    fSkipList: TList; // TList<Integer>
    fClassScope: TStatementClassScope;
    fStatementList: TStatementList;
    fIncludesList: TStringList; //TStringList<String,PFileIncludes>,List of scaned files and it's infos
    {It's used in preprocessor, so we can't use fIncludeList instead}
    fScannedFiles: TStringList; // List of scaned file names
    fTokenizer: TCppTokenizer;
    fPreprocessor: TCppPreprocessor;
    { List of current compiler set's include path}
    fIncludePaths: TStringList;
    { List of current project's include path }
    fProjectIncludePaths: TStringList;
    { List of current project's include path }
    fProjectFiles: TStringList;
    fBlockBeginSkips: TIntList; //list of for/catch block begin token index;
    fBlockEndSkips: TIntList; //list of for/catch block end token index;
    fInlineNamespaceEndSkips: TIntList; // list for inline namespace end token index;
    fFilesToScan: TStringList; // list of base files to scan
    fFilesScannedCount: Integer; // count of files that have been scanned
    fFilesToScanCount: Integer; // count of files and files included in files that have to be scanned
    fParseLocalHeaders: boolean;
    fParseGlobalHeaders: boolean;
    fOnEndParsing: TProgressEndEvent;
    fIsProjectFile: boolean;
    //fMacroDefines : TList;
    fLocked: boolean; // lock(don't reparse) when we need to find statements in a batch
    fParsing: boolean;
    fNamespaces :TDevStringList;  //TStringList<String,List<Statement>> namespace and the statements in its scope
    fRemovedStatements: THashedStringList; //THashedStringList<String,PRemovedStatements>
    fCriticalSection: TCriticalSection;
    function AddInheritedStatement(derived:PStatement; inherit:PStatement; access:TStatementClassScope):PStatement;

    function AddChildStatement(// support for multiple parents (only typedef struct/union use multiple parents)
      Parent: PStatement;
      const FileName: AnsiString;
      const HintText: AnsiString;
      const aType: AnsiString; // "Type" is already in use
      const Command: AnsiString;
      const Args: AnsiString;
      const Value: AnsiString;
      Line: integer;
      Kind: TStatementKind;
      Scope: TStatementScope;
      ClassScope: TStatementClassScope;
      IsDefinition: boolean;
      isStatic: boolean): PStatement; // TODO: InheritanceList not supported
    function AddStatement(
      Parent: PStatement;
      const FileName: AnsiString;
      const HintText: AnsiString;
      const aType: AnsiString; // "Type" is already in use
      const Command: AnsiString;
      const Args: AnsiString;
      const Value: AnsiString;
      Line: integer;
      Kind: TStatementKind;
      Scope: TStatementScope;
      ClassScope: TStatementClassScope;
      IsDefinition: boolean;
      InheritanceList: TList;
      isStatic: boolean): PStatement;
    procedure SetInheritance(Index: integer; ClassStatement: PStatement; IsStruct:boolean);
    function GetCurrentScope: PStatement; // gets last item from last level
    function IsInCurrentScopeLevel(const Command: AnsiString): PStatement;
    procedure AddSoloScopeLevel(Statement: PStatement;line:integer); // adds new solo level
    procedure RemoveScopeLevel(line:integer); // removes level
    procedure CheckForSkipStatement;
    function SkipBraces(StartAt: integer): integer;
    function SkipBracket(StartAt: integer): integer;
    function CheckForPreprocessor: boolean;
    function CheckForKeyword: boolean;
    function CheckForNamespace: boolean;
    function CheckForUsing: boolean;

    function CheckForTypedef: boolean;
    function CheckForTypedefEnum: boolean;
    function CheckForTypedefStruct: boolean;
    function CheckForStructs : boolean;
    function CheckForMethod(var sType, sName, sArgs: AnsiString;
      var IsStatic:boolean; var IsFriend:boolean): boolean; // caching of results
    function CheckForScope: boolean;
    function CheckForVar: boolean;
    function CheckForEnum: boolean;
    function CheckForForBlock: boolean;
    function CheckForCatchBlock: boolean;
    function GetScope: TStatementScope;
    function GetCurrentBlockEndSkip:integer;
    function GetCurrentBlockBeginSkip:integer;
    function GetCurrentInlineNamespaceEndSkip:integer;
    procedure HandlePreprocessor;
    procedure HandleOtherTypedefs;
    procedure HandleStructs(IsTypedef: boolean = False);
    procedure HandleMethod(const sType, sName, sArgs: AnsiString; isStatic: boolean;IsFriend:boolean);
    procedure ScanMethodArgs(const FunctionStatement:PStatement; ArgStr:string);
    procedure HandleScope;
    procedure HandleKeyword;
    procedure HandleVar;
    procedure HandleEnum;
    procedure HandleNamespace;
    procedure HandleUsing;
    procedure HandleForBlock;
    procedure HandleCatchBlock;
    function HandleStatement: boolean;
    procedure InternalParse(const FileName: AnsiString; ManualUpdate: boolean = False; Stream: TMemoryStream = nil);
//    function FindMacroDefine(const Command: AnsiString): PStatement;
    function expandMacroType(const name:AnsiString): AnsiString;
    procedure InheritClassStatement(derived: PStatement; isStruct:boolean; base: PStatement; access:TStatementClassScope);
    function GetIncompleteClass(const Command:AnsiString; parentScope:PStatement): PStatement;
    procedure SetTokenizer(tokenizer: TCppTokenizer);
    procedure SetPreprocessor(preprocessor: TCppPreprocessor);
    function GetFullStatementName(command:String; parent:PStatement):string;
    {procedure ResetDefines;}
    function FindMemberOfStatement(const Phrase: AnsiString; ScopeStatement: PStatement):PStatement;
    procedure getFullNameSpace(const Phrase:AnsiString; var namespace:AnsiString; var member:AnsiString);
    function FindStatementInScope(const Name , NoNameArgs:string;kind:TStatementKind; scope:PStatement):PStatement;
    procedure InternalInvalidateFile(const FileName: AnsiString);
    function GetClass(const Phrase: AnsiString): AnsiString;
    function GetMember(const Phrase: AnsiString): AnsiString;
    function GetOperator(const Phrase: AnsiString): AnsiString;
    function GetRemainder(const Phrase: AnsiString): AnsiString;
    function getStatementKey(const _Name,_Type,_NoNameArgs:AnsiString):AnsiString;
    procedure OnProgress(FileName:String;Total,Current:integer);
    procedure OnBusy;
    procedure OnUpdate;
    procedure OnStartParsing;
    procedure OnEndParsing(total:integer);
  public
    procedure ParseHardDefines;
    function FindFileIncludes(const Filename: AnsiString; DeleteIt: boolean = False): PFileIncludes;
    procedure AddHardDefineByLine(const Line: AnsiString);
    procedure InvalidateFile(const FileName: AnsiString);
    procedure GetFileDirectIncludes(const Filename: AnsiString; var List: TStringList);
    procedure GetFileIncludes(const Filename: AnsiString; var List: TStringList);
    procedure GetFileUsings(const Filename: AnsiString; var List: TDevStringList);

    function IsSystemHeaderFile(const FileName: AnsiString): boolean;
    function IsProjectHeaderFile(const FileName: AnsiString): boolean;
    procedure GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
    procedure GetClassesList(var List: TStringList);
    function SuggestMemberInsertionLine(ParentStatement: PStatement; Scope: TStatementClassScope; var AddScopeStr:
      boolean):
      integer;
      {
    function GetSystemHeaderFileName(const FileName: AnsiString): AnsiString; // <file.h>
    function GetProjectHeaderFileName(const FileName: AnsiString): AnsiString; // <file.h>
    function GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString): AnsiString; // "file.h"
    }
    function GetHeaderFileName(const RelativeTo, Line: AnsiString): AnsiString; // both
    function IsIncludeLine(const Line: AnsiString): boolean;
    constructor Create(AOwner: TComponent; wnd:HWND); 
    destructor Destroy; override;
    procedure ParseFileList;
    procedure ParseFile(const FileName: AnsiString; InProject: boolean; OnlyIfNotParsed: boolean = False; UpdateView:
      boolean = True; Stream: TMemoryStream = nil);
    function StatementKindStr(Value: TStatementKind): AnsiString;
    function StatementClassScopeStr(Value: TStatementClassScope): AnsiString;
    procedure Reset;
    procedure ClearIncludePaths;
    procedure ClearProjectIncludePaths;
    procedure ClearProjectFiles;
    procedure AddIncludePath(const Value: AnsiString);
    procedure AddProjectIncludePath(const Value: AnsiString);
    procedure AddFileToScan(Value: AnsiString; InProject: boolean = False);
    function PrettyPrintStatement(Statement: PStatement; line:integer = -1): AnsiString;
    procedure FillListOfFunctions(const FileName, Phrase: AnsiString; const Line: integer;  List: TStringList);
    function FindAndScanBlockAt(const Filename: AnsiString; Line: integer): PStatement;
    function FindStatementOf(FileName, Phrase: AnsiString; Line: integer): PStatement; overload;
    function FindStatementOf(FileName, Phrase: AnsiString; CurrentClass: PStatement;
      var CurrentClassType: PStatement ; force:boolean = False): PStatement; overload;
    function FindStatementOf(FileName, Phrase: AnsiString; CurrentClass: PStatement; force:boolean = False): PStatement; overload;

    function FindKindOfStatementOf(FileName, Phrase: AnsiString; Line: integer): TStatementKind;
    function GetHintFromStatement(FileName, Phrase: AnsiString; Line: integer):AnsiString;
    {Find statement starting from startScope}
    function FindStatementStartingFrom(const FileName, Phrase: AnsiString; startScope: PStatement; force:boolean = False): PStatement;
    function FindTypeDefinitionOf(const FileName: AnsiString;const aType: AnsiString; CurrentClass: PStatement): PStatement;
    function FindFirstTemplateParamOf(const FileName: AnsiString;const aPhrase: AnsiString; currentClass: PStatement): String;
    function FindLastOperator(const Phrase: AnsiString): integer;
    function FindNamespace(const name:AnsiString):TList; // return a list of PSTATEMENTS (of the namespace)
    procedure Freeze;  // Freeze/Lock (stop reparse while searching)
    procedure UnFreeze; // UnFree/UnLock (reparse while searching)
    function GetParsing: boolean;

    function FindFunctionDoc(const FileName:AnsiString; const Line: integer;
      params:TStringList; var isVoid:boolean): AnsiString;
    
    property IncludePaths: TStringList read fIncludePaths;
    property ProjectIncludePaths: TStringList read fProjectIncludePaths;
  published
    property Parsing: boolean read GetParsing;
    property Enabled: boolean read fEnabled write fEnabled;
    property Tokenizer: TCppTokenizer read fTokenizer;
    property Preprocessor: TCppPreprocessor read fPreprocessor;
    property Statements: TStatementList read fStatementList write fStatementList;
    property ParseLocalHeaders: boolean read fParseLocalHeaders write fParseLocalHeaders;
    property ParseGlobalHeaders: boolean read fParseGlobalHeaders write fParseGlobalHeaders;
    property ScannedFiles: TStringList read fScannedFiles;
    property FilesToScan: TStringList read fFilesToScan;
  end;

procedure Register;

implementation

uses
  DateUtils;

procedure Register;
begin
  RegisterComponents('Dev-C++', [TCppParser]);
end;


constructor TCppParser.Create(AOwner: TComponent; wnd:HWND);
begin
  inherited Create(AOwner);
  fPreprocessor := TCppPreprocessor.Create(AOwner);
  fTokenizer := TCppTokenizer.Create(AOwner);
  fHandle := wnd;
  fUniqId := 0;
  fParsing:=False;
  fStatementList := TStatementList.Create; // owns the objects
  fIncludesList := TStringList.Create;
  fIncludesList.Sorted := True;
  fIncludesList.duplicates:=dupIgnore;
  fFilesToScan := TStringList.Create;
  fFilesToScan.Sorted := True;
  fScannedFiles := TStringList.Create;
  fScannedFiles.Sorted := True;
  fIncludePaths := TStringList.Create;
  fIncludePaths.Sorted := True;
  fProjectIncludePaths := TStringList.Create;
  fProjectIncludePaths.Sorted := True;
  fProjectFiles := TStringList.Create;
  fProjectFiles.Sorted := True;
  //fMacroDefines := TList.Create;
  fCurrentScope := TList.Create;
  fCurrentClassScope := TList.Create;
  fSkipList:= TList.Create;
  fParseLocalHeaders := False;
  fParseGlobalHeaders := False;
  fLocked := False;
  fNamespaces := TDevStringList.Create;
  fNamespaces.Sorted := True;
  fBlockBeginSkips := TIntList.Create;
  fBlockEndSkips := TIntList.Create;
  fInlineNamespaceEndSkips := TIntList.Create;
  fRemovedStatements := THashedStringList.Create;
  fRemovedStatements.CaseSensitive:=True;
  fCriticalSection:= TCriticalSection.Create;
end;

destructor TCppParser.Destroy;
var
  i: Integer;
  namespaceList: TList;
begin
  while True do begin
    fCriticalSection.Acquire;
    try
      if not fParsing then begin
        fParsing:=True;
        break;
      end;
    finally
      fCriticalSection.Release;
    end;
    Sleep(50);
    Application.ProcessMessages;
  end;
  //FreeAndNil(fMacroDefines);
  FreeAndNil(fCurrentScope);
  FreeAndNil(fCurrentClassScope);
  FreeAndNil(fProjectFiles);

  for i:=0 to fIncludesList.Count - 1 do begin
    PFileIncludes(fIncludesList.Objects[i])^.IncludeFiles.Free;
    PFileIncludes(fIncludesList.Objects[i])^.DirectIncludeFiles.Free;
    PFileIncludes(fIncludesList.Objects[i])^.Usings.Free;
    PFileIncludes(fIncludesList.Objects[i])^.Statements.Free;
    PFileIncludes(fIncludesList.Objects[i])^.StatementsIndex.Free;
    PFileIncludes(fIncludesList.Objects[i])^.DeclaredStatements.Free;
    PFileIncludes(fIncludesList.Objects[i])^.Scopes.Free;
    Dispose(PFileIncludes(fIncludesList.Objects[i]));
  end;
  FreeAndNil(fIncludesList);

  for i:=0 to fNamespaces.Count-1 do begin
    namespaceList := TList(fNamespaces.Objects[i]);
    namespaceList.Free;
  end;
  FreeAndNil(fNamespaces);
  FreeAndNil(fRemovedStatements);

  FreeAndNil(fSkipList);
  FreeAndNil(fBlockBeginSkips);
  FreeAndNil(fBlockEndSkips);
  FreeAndNil(fInlineNamespaceEndSkips);

  FreeAndNil(fStatementList);
  FreeAndNil(fFilesToScan);
  FreeAndNil(fScannedFiles);
  FreeAndNil(fIncludePaths);
  FreeAndNil(fProjectIncludePaths);
  FreeAndNil(fCriticalSection);
  FreeAndNil(fPreprocessor);
  FreeAndNil(fTokenizer);
  inherited Destroy;
end;

function TCppParser.StatementClassScopeStr(Value: TStatementClassScope): AnsiString;
begin
  case Value of
    scsPublic: Result := 'public';
    scsPrivate: Result := 'private';
    scsProtected: Result := 'protected';
    scsNone: Result := '';
  end;
end;

function TCppParser.StatementKindStr(Value: TStatementKind): AnsiString;
begin
  case Value of
    skPreprocessor: Result := 'P';
    skVariable, skParameter: Result := 'V';
    skConstructor: Result := 'Ct';
    skDestructor: Result := 'Dt';
    skFunction: Result := 'F';
    skClass: Result := 'C';
    skTypedef: Result := 'T';
    skEnum: Result := 'E';
    skEnumType: Result := 'T';
    skUnknown: Result := 'U';
    skNamespace: Result := 'N';
    skUserCodeIn: Result := '';
    skKeyword: Result :='K';
    else
      Result := 'U';
  end;
end;

function TCppParser.SkipBracket(StartAt: integer): integer;
var
  I, Level: integer;
begin
  I := StartAt;
  Level := 0; // assume we start on top of {
  while (I < fTokenizer.Tokens.Count) do begin
    case fTokenizer[I]^.Text[1] of
      '[': Inc(Level);
      ']': begin
          Dec(Level);
          if Level = 0 then begin
            Result := I;
            Exit;
          end;
        end;
    end;
    Inc(I);
  end;
  Result := StartAt;
end;

function TCppParser.SkipBraces(StartAt: integer): integer;
var
  I, Level: integer;
begin
  I := StartAt;
  Level := 0; // assume we start on top of {
  while (I < fTokenizer.Tokens.Count) do begin
    case fTokenizer[I]^.Text[1] of
      '{': Inc(Level);
      '}': begin
          Dec(Level);
          if Level = 0 then begin
            Result := I;
            Exit;
          end;
        end;
    end;
    Inc(I);
  end;
  Result := StartAt;
end;

{

function TCppParser.SkipParenthesis(StartAt: integer): integer;
var
  I, Level: integer;
begin
  I := StartAt;
  Level := 0; // assume we start on top of (
  while (I < fTokenizer.Tokens.Count) do begin
    case fTokenizer[I]^.Text[1] of
      '(': Inc(Level);
      ')': begin
          Dec(Level);
          if Level = 0 then begin
            Result := I;
            Exit;
          end;
        end;
    end;
    Inc(I);
  end;
  Result := StartAt;
end;
function TCppParser.FindMacroDefine(const Command:AnsiString):PStatement;
var
  Statement: PStatement;
  I: integer;
begin
  // we do a backward search, because most possible is to be found near the end ;) - if it exists :(
  for I := 0 to fMacroDefines.Count - 1 do begin
    Statement := fMacroDefines[i];
    if Statement^._Command = Command then begin
      Result := Statement;
      Exit;
    end;
  end;
  Result := nil;
end;
}
function TCppParser.expandMacroType(const name:AnsiString): AnsiString;
//var
//  Statement: PStatement;
begin
  Result := name;
  {
  if StartsStr('__',name) then begin
    Result := '';
    Exit;
  end;
  }
  { we have expanded macro in the preprocessor , so we don't need do it here{
  {
  Statement := FindMacroDefine(name);
  if Assigned(Statement) then begin
    if (Statement^._Value <> '') then
      Result:= Name
    else
      Result:='';
  end;
  }
end;

function TCppParser.FindStatementInScope(const Name , NoNameArgs:string;kind:TStatementKind; scope:PStatement):PStatement;
var
  namespaceStatement:PStatement;
  i: integer;
  namespaceStatementsList:TList;

  function DoFind(const _Name , _NoNameArgs:string; _kind:TStatementKind; _scope:PStatement):PStatement;
  var
    index: TDevStringHash;
    item: PPHashItem;
    itemStatement: PStatement;
  begin
    Result := nil;
    index := self.fStatementList.GetChildrenStatementIndex(_scope);
    if not assigned(index) then
      Exit;
    item:=index.FindItem(_Name);
    while (assigned(item)) and assigned(item^) and SameStr(item^.Key,_Name) do begin
      itemStatement:=PStatement(item^.Value);
      if assigned(itemStatement) and (itemStatement^._Kind  = _kind)
        and SameStr(itemStatement^._NoNameArgs, _NoNameArgs) then begin
        Result:=itemStatement;
        Exit;
      end;
      item:=@item^.Next;
    end;
  end;
begin
  Result:=nil;
  if assigned(scope) and (scope^._Kind = skNamespace) then begin
      namespaceStatementsList:=FindNamespace(scope^._Command);
      if not Assigned(namespaceStatementsList) then
        Exit;
      for i:=0 to namespaceStatementsList.Count-1 do begin
        namespaceStatement:=PStatement(namespaceStatementsList[i]);
        Result:=DoFind(Name,NoNameArgs,Kind,namespaceStatement);
        if Assigned(Result) then
          Exit;
      end;
  end else begin
    Result := DoFind(Name,NoNameArgs,Kind, Scope);
  end;
end;

// When finding declaration/definition pairs only search the separate incomplete pair list

function TCppParser.GetIncompleteClass(const Command: AnsiString; parentScope:PStatement): PStatement;
var
  p: integer;
  s: String;
begin
  Result:=nil;
  s:=command;
  p:=Pos('<',s);
  if p>0 then
    Delete(s,p,MaxInt);
  Result := FindStatementOf(fCurrentFile,s,parentScope,True);
  if Assigned(Result) and (Result^._Kind <> skClass) then
    Result:=nil;
end;

function TCppParser.getStatementKey(const _Name,_Type,_NoNameArgs:AnsiString):AnsiString;
begin
  Result := _Name + '--'+_Type+'--'+_NoNameArgs;
end;

function TCppParser.AddChildStatement(
  Parent: PStatement;
  const FileName: AnsiString;
  const HintText: AnsiString;
  const aType: AnsiString; // "Type" is already in use
  const Command: AnsiString;
  const Args: AnsiString;
  const Value: AnsiString;
  Line: integer;
  Kind: TStatementKind;
  Scope: TStatementScope;
  ClassScope: TStatementClassScope;
  IsDefinition: boolean;
  isStatic: boolean): PStatement;
begin
  Result := nil;
      Result := AddStatement(
        Parent,
        FileName,
        HintText,
        aType,
        Command,
        Args,
        Value,
        Line,
        Kind,
        Scope,
        ClassScope,
        IsDefinition,
        nil,
        isStatic);
end;

function TCppParser.GetFullStatementName(command:String; parent:PStatement):string;
var
  scopeStatement:PStatement;
begin
  scopeStatement:=Parent;
  while Assigned(scopeStatement) and not (scopeStatement^._Kind in [skClass, skNamespace, skFunction]) do
     scopeStatement := scopeStatement^._parentScope;
  if Assigned(scopeStatement)  then
    Result := scopeStatement^._FullName + '::'+command
  else
    Result := command;
end;

{
function TCppParser.GetPendingKey(command:String; parent:PStatement; kind: TStatementKind;Args:String):String;
begin
  if assigned(Parent) then
    Result := Parent^._FullName
  else
    Result := '';
  Result := Result + IntToStr(ord(kind))+Command+Args;
end;
}

function TCppParser.AddStatement(
  Parent: PStatement;
  const FileName: AnsiString;
  const HintText: AnsiString;
  const aType: AnsiString; // "Type" is already in use
  const Command: AnsiString;
  const Args: AnsiString;
  const Value: AnsiString;
  Line: integer;
  Kind: TStatementKind;
  Scope: TStatementScope;
  ClassScope: TStatementClassScope;
  IsDefinition: boolean;
  InheritanceList: TList;
  isStatic:boolean): PStatement;
var
  oldStatement: PStatement;
  //NewKind: TStatementKind;
  NewType, NewCommand,NoNameArgs: AnsiString;
  node: PStatementNode;
  fileIncludes1:PFileIncludes;
  idx,idx2:integer;
  key:String;
  removedStatement: PRemovedStatement;
  //t,lenCmd:integer;

  function AddToList: PStatement;
  var
    i: integer;
    NamespaceList: TList;
    fileIncludes:PFileIncludes;
  begin
    Result := New(PStatement);
    with Result^ do begin
      _ParentScope := Parent;
      _HintText := HintText;
      _Type := NewType;
      if NewCommand<>'' then
        _Command := NewCommand
      else begin
        inc(fUniqId);
        _Command := '__STATEMENT__'+IntToStr(fUniqId);
      end;
      _Args := Args;
      _NoNameArgs := NoNameArgs;
      _Value := Value;
      _Kind := Kind;
      _InheritanceList := InheritanceList;
      _Scope := Scope;
      _ClassScope := ClassScope;
      _HasDefinition := IsDefinition;
      _Line := Line;
      _DefinitionLine := Line;
      _FileName := FileName;
      _DefinitionFileName := FileName;
      _InProject := fIsProjectFile;
      _InSystemHeader := fIsSystemHeader;
      _Children := nil;
      _ChildrenIndex := nil;
      _Friends := nil;
      _Static := isStatic;
      _Inherited:= False;
      if Scope = ssLocal then begin
        _FullName :=  NewCommand;
      end else
        _FullName :=  GetFullStatementName(NewCommand, Parent);
      _Usings:=TStringList.Create;
      _Usings.Duplicates:=dupIgnore;
      _Usings.Sorted:=True;
      _Node := nil; // will set by the fStatementlist.add()
      _UsageCount :=0;
      _FreqTop:=0;
    end;
    node:=fStatementList.Add(Result);
    if (Result^._Kind = skNamespace) then begin
      i:=FastIndexOf(fNamespaces,Result^._FullName);
      if (i<>-1) then
        NamespaceList := TList(fNamespaces.objects[i])
      else begin
        NamespaceList := TList.Create;
        fNamespaces.AddObject(Result^._FullName,NamespaceList);
      end;
      NamespaceList.Add(result);
    end;

    fileIncludes:=FindFileIncludes(FileName);
    if Assigned(fileIncludes) and (Result^._Kind <>skBlock) then begin
      idx:=fileIncludes^.Statements.Add(Result);
      fileIncludes^.StatementsIndex.Add(IntToStr(integer(Result)),idx);
      fileIncludes^.DeclaredStatements.Add(Result);
    end;
  end;

  function RemoveArgNames(args:AnsiString):AnsiString;
  var
    i:integer;
    argsLen:integer;
    word:AnsiString;
    currentArg: AnsiString;
    brackLevel:integer;
    typeGetted: boolean;
  begin
    Result:='';
    argsLen := Length(args);
    if argsLen < 2 then
      Exit;
    i:=2;   // skip start '('
    currentArg:='';
    word:='';
    brackLevel := 0;
    typeGetted := False;
    while i<argsLen do begin // we use the last ')' as a word seperator
      case args[i] of
        ',': begin
          if brackLevel >0 then begin
            word:=word+args[i];
          end else begin
            if not typeGetted then begin
              currentArg:= currentArg + ' ' + word;
            end else begin
              if isKeyword(word) then begin
                currentArg:= currentArg + ' ' + word;
              end;
            end;
            word := '';
            Result := Result + TrimLeft(currentArg) +',';
            currentArg := '';
            typeGetted := False;
          end;
        end;
        '<','[','(': begin
          inc(brackLevel);
          word:=word+args[i];
        end;
        '>',']',')': begin
          dec(brackLevel);
          word:=word+args[i];
        end;
        ' ',#9: begin
          if (brackLevel >0) and not (args[i-1] in [' ',#9]) then begin
            word:=word+args[i];
          end else begin
            if not typeGetted then begin
              currentArg:= currentArg + ' ' + word;
              if (CppTypeKeywords.ValueOf(word)>0) or (not IsKeyword(word)) then
                typeGetted := True;
            end else begin
              if isKeyword(word) then begin
                currentArg:= currentArg + ' ' + word;
              end;
            end;
            word := '';
          end;
        end;
        '0'..'9','a'..'z','A'..'Z','_','*','&': begin
            word:=word+args[i];
        end;
      end;
      inc(i);
    end;
    if not typeGetted then begin
      currentArg:= currentArg + ' ' + word;
    end else begin
      if isKeyword(word) then begin
        currentArg:= currentArg + ' ' + word;
      end;
    end;
    Result := Result + TrimLeft(currentArg);
  end;

begin
  // Move '*', '&' to type rather than cmd (it's in the way for code-completion)
  NewType := aType;
  NewCommand := Command;
  while (Length(NewCommand) > 0) and (NewCommand[1] in ['*', '&']) do begin
    NewType := NewType + NewCommand[1];
    Delete(NewCommand, 1, 1); // remove first
  end;


  NoNameArgs:='';
  if kind in [skConstructor, skFunction,skDestructor] then begin
    NoNameArgs := RemoveArgNames(Args);
    //find
    oldStatement := FindStatementInScope(command,NoNameArgs,kind,parent);
    if assigned(oldStatement) then begin
      Result:= oldStatement;
      if IsDefinition then begin
        if not SameText(oldStatement^._FileName, FileName) then begin
          fileIncludes1:=FindFileIncludes(FileName);
          if Assigned(fileIncludes1) then begin
            idx:=fileIncludes1^.Statements.Add(oldStatement);
            fileIncludes1^.StatementsIndex.Add(IntToStr(integer(oldStatement)),idx);
          end;
        end;
        oldStatement^._DefinitionLine := Line;
        oldStatement^._DefinitionFileName := FileName;
      end else begin
        if not SameText(oldStatement^._DefinitionFileName, FileName) then begin
          fileIncludes1:=FindFileIncludes(FileName);
          if Assigned(fileIncludes1) then begin
            idx:=fileIncludes1^.Statements.Add(oldStatement);
            fileIncludes1^.StatementsIndex.Add(IntToStr(integer(oldStatement)),idx);
          end;
        end;
        oldStatement^._Line:=Line;
        oldStatement^._FileName:=FileName;
      end;
      Exit;
    end;
  end;
  Result := AddToList;

  if kind in [skConstructor, skFunction,skDestructor] then begin
    key:=GetStatementKey(Result^._FullName,Result^._Type,Result^._NoNameArgs);
    idx := fRemovedStatements.IndexOf(key);
    if idx>=0 then begin
      removedStatement := PRemovedStatement(fRemovedStatements.Objects[idx]);
      Result^._DefinitionLine := removedStatement^._DefinitionLine;
      Result^._DefinitionFileName := removedStatement^._DefinitionFileName;
      fileIncludes1:=FindFileIncludes(Result^._DefinitionFileName);
      if Assigned(fileIncludes1) then begin
        idx2:=fileIncludes1^.Statements.Add(Result);
        fileIncludes1^.StatementsIndex.Add(IntToStr(integer(Result)),idx2);
      end;
      dispose(PRemovedStatement(removedStatement));
      fRemovedStatements.Delete(idx);
    end;
  end;
end;

{
function TCppParser.GetCurrentNonBlockScope: PStatement;
var
  i:integer;
  scope:PStatement;
begin
  Result := nil;
  for i:=fCurrentScope.Count - 1 downto 0 do begin
    scope := PStatement(fCurrentScope[fCurrentScope.Count - 1]);
    if assigned(scope) and (scope^._Kind  <> skBlock) then begin
      Result:=scope;
      Exit;
    end;
  end;
end;
}
function TCppParser.GetCurrentScope: PStatement;
begin
  if fCurrentScope.Count = 0 then begin
    Result := nil;
    Exit;
  end;

  Result := PStatement(fCurrentScope[fCurrentScope.Count - 1]);
end;

function TCppParser.IsInCurrentScopeLevel(const Command: AnsiString): PStatement;
var
  Statement: PStatement;
  s:AnsiString;
  i:integer;
begin
  Result := nil;

  Statement := GetCurrentScope;
  // remove template staff
  i:=Pos('<',command);
  if i>0 then
    s:=Copy(Command,1,i-1)
  else
    s:=Command;
  if Assigned(Statement) and SameStr(Command, Statement^._Command) then begin
    Result := Statement;
  end;
end;

procedure TCppParser.AddSoloScopeLevel(Statement: PStatement; line:integer);
var
  fileIncludes : PFileIncludes;
  parentScope : PStatement;
begin
  // Add class list

  if Assigned(statement) and (statement^._Kind = skBlock) then begin
    parentScope := statement^._ParentScope;
    while Assigned(parentScope) and (parentScope^._Kind = skBlock) do
      parentScope := parentScope^._ParentScope;
    if not Assigned(parentScope) then
      statement:=nil;
  end;

  if fCurrentClassScope.Count > 0 then begin
    fCurrentClassScope[fCurrentClassScope.Count-1] := TObject(fClassScope);
  end;

  fCurrentScope.Add(Statement);

  fileIncludes := FindFileIncludes(fCurrentFile);

  if assigned(fileIncludes) then begin
    fileIncludes.Scopes.AddObject(line,TObject(statement));
  end;

  // Set new scope
  if not Assigned(Statement) then
    fClassScope := scsNone // {}, namespace or class that doesn't exist
  else if Statement^._Kind = skNamespace then
    fClassScope := scsNone
  else if (Statement^._Type = 'class') then
    fClassScope := scsPrivate // classes are private by default
  else
    fClassScope := scsPublic; // structs are public by default
  fCurrentClassScope.Add(TObject(fClassScope));
end;

procedure TCppParser.RemoveScopeLevel(line:integer);
var
  CurrentScope: PStatement;
  fileIncludes: PFileIncludes;
  idx:integer;
begin
  // Remove class list
  if fCurrentScope.Count = 0 then
    Exit; // TODO: should be an exception
  CurrentScope := PStatement(fCurrentScope[fCurrentScope.Count-1]);
  fileIncludes:=FindFileIncludes(fCurrentFile);
  if (assigned(CurrentScope) and (CurrentScope^._Kind = skBlock)) then begin
    if (
      (not Assigned(CurrentScope^._Children))
      or (CurrentScope^._Children.Count=0)) then begin
      if assigned(fileIncludes) and (fileIncludes.Scopes.Count>0) then begin
        fileIncludes.Scopes.Delete(fileIncludes.Scopes.Count-1);
      end;
      Statements.DeleteStatement(CurrentScope); // remove no children block
    end else begin
      idx:=fileIncludes.Statements.Add(CurrentScope);
      fileIncludes^.StatementsIndex.Add(IntToStr(integer(CurrentScope)), idx);
      fileIncludes.DeclaredStatements.Add(CurrentScope);
    end;
  end;
  fCurrentScope.Delete(fCurrentScope.Count - 1);
  fCurrentClassScope.Delete( fCurrentClassScope.Count -1);

  // Set new scope
  CurrentScope := GetCurrentScope;
//  fileIncludes:=FindFileIncludes(fCurrentFile);
  if assigned(fileIncludes) and (fileIncludes.Scopes.Count>0)
    and (fileIncludes.Scopes.Objects[fileIncludes.Scopes.Count-1]<>TObject(CurrentScope)) then begin
    fileIncludes.Scopes.AddObject(line,TObject(CurrentScope));
  end;

  if not Assigned(CurrentScope) then
    fClassScope := scsNone
  else begin
    fClassScope :=  TStatementClassScope(fCurrentClassScope[fCurrentClassScope.Count-1]);
  end;
end;

procedure TCppParser.SetInheritance(Index: integer; ClassStatement: PStatement; IsStruct:boolean);
var
  Statement: PStatement;
  inheritScopeType: TStatementClassScope;
  lastInheritScopeType : TStatementClassScope;
  basename: AnsiString;
  pBegin:integer;

  function CheckForInheritScopeType(Index: integer): TStatementClassScope;
  begin
    Result := scsNone;
    if SameStr(fTokenizer[Index]^.Text, 'public') then
      Result := scsPublic
    else if SameStr(fTokenizer[Index]^.Text, 'protected') then
      Result := scsProtected
    else if SameStr(fTokenizer[Index]^.Text, 'private') then
      Result := scsPrivate;
  end;

begin
  // Clear it. Assume it is assigned
  ClassStatement._InheritanceList.Clear;
  lastInheritScopeType := scsNone;
  // Assemble a list of statements in text form we inherit from
  repeat
    inheritScopeType := CheckForInheritScopeType(Index);
    if inheritScopeType = scsNone  then
      if not (fTokenizer[Index]^.Text[1] in [',', ':', '(']) then begin
        basename:=fTokenizer[Index]^.Text;
        //remove template staff
        pBegin:=Pos('<',basename);
        if pBegin>0 then begin
          Delete(basename,pBegin,MaxInt);
        end;

        // Find the corresponding PStatement
        Statement := FindStatementOf(fCurrentFile,basename,ClassStatement^._ParentScope,true);
        if assigned(statement) and (Statement^._Kind = skClass) then begin
            ClassStatement._InheritanceList.Add(Statement); // next I please
            InheritClassStatement(ClassStatement,isStruct, Statement,lastInheritScopeType);
            break;
        end;
      end;
    Inc(Index);
    lastInheritScopeType := inheritScopeType;
  until (Index >= fTokenizer.Tokens.Count) or (fTokenizer[Index]^.Text[1] in ['{', ';']);
end;

function TCppParser.GetScope: TStatementScope;
var
  CurrentScope: PStatement;
begin
  // Don't blindly trust levels. Namespaces and externs can have levels too
  CurrentScope := GetCurrentScope;

  // Invalid class or namespace/extern
  if not Assigned(CurrentScope) or (CurrentScope^._Kind = skNamespace) then
    Result := ssGlobal
    // We are inside a class body
  else if (CurrentScope^._Kind = skClass) then
    Result := ssClassLocal
  else
    Result := ssLocal;
end;

procedure TCppParser.CheckForSkipStatement;
begin
  if (fSkipList.Count>0) and (fIndex = integer(fSkipList[fSkipList.Count-1])) then begin // skip to next ';'
    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';']);
    Inc(fIndex); //skip ';'
    fSkipList.Delete(fSkipList.Count-1);
  end;
end;

function TCppParser.CheckForPreprocessor: boolean;
begin
  result := StartsStr('#', fTokenizer[fIndex]^.Text);
end;

function TCppParser.CheckForKeyword: boolean;
var
  v:integer;
begin
  v := CppKeywords.ValueOf(fTokenizer[fIndex]^.Text);
  Result := (v>=0) and (v <> Ord(skNone));   
end;

function TCppParser.CheckForTypedef: boolean;
begin
  Result := (fIndex < fTokenizer.Tokens.Count) and SameStr(fTokenizer[fIndex]^.Text, 'typedef');
end;

function TCppParser.CheckForEnum: boolean;
begin
  Result := (fIndex < fTokenizer.Tokens.Count) and SameStr(fTokenizer[fIndex]^.Text, 'enum');
end;

function TCppParser.CheckForForBlock: boolean;
begin
  Result := (fIndex < fTokenizer.Tokens.Count) and SameStr(fTokenizer[fIndex]^.Text, 'for');
end;

function TCppParser.CheckForCatchBlock: boolean;
begin
  Result := (fIndex < fTokenizer.Tokens.Count) and SameStr(fTokenizer[fIndex]^.Text, 'catch');
end;


function TCppParser.CheckForTypedefEnum: boolean;
begin
  //we assume that typedef is the current index, so we check the next
  //should call CheckForTypedef first!!!
  Result := (fIndex < fTokenizer.Tokens.Count - 1) and
    SameStr(fTokenizer[fIndex + 1]^.Text, 'enum');
end;

function TCppParser.CheckForTypedefStruct: boolean;
var
  keyLen:integer;
  word:String;
begin
  //we assume that typedef is the current index, so we check the next
  //should call CheckForTypedef first!!!
  Result := False;
  if (fIndex >= fTokenizer.Tokens.Count) then
    Exit;
  keyLen := -1;
  word := fTokenizer[fIndex + 1]^.Text;
  if StartsStr('struct',word) then
    keyLen:=6
  else if StartsStr('class',word)
    or StartsStr('union',word) then
    keyLen:=5;
  if keyLen<0 then
    Exit;
  Result := (length(word) = keyLen)
    or (word[keyLen+1] in [' ',#9,'[']);
end;

function TCppParser.CheckForNamespace: boolean;
begin
  Result := ((fIndex < fTokenizer.Tokens.Count -1 ) and SameStr(fTokenizer[fIndex]^.Text, 'namespace'))
    or (
      (fIndex+1 < fTokenizer.Tokens.Count -1 ) and SameStr(fTokenizer[fIndex]^.Text, 'inline')
        and SameStr(fTokenizer[fIndex+1]^.Text, 'namespace')
    );
end;

function TCppParser.CheckForUsing: boolean;
begin
  Result := (fIndex < fTokenizer.Tokens.Count -1) and SameStr(fTokenizer[fIndex]^.Text, 'using');
end;

function TCppParser.CheckForStructs: boolean;
var
  I,keyLen: integer;
  dis: integer;
  word:string;
begin
  if SameStr(fTokenizer[fIndex]^.Text, 'friend')
   or SameStr(fTokenizer[fIndex]^.Text, 'public') or SameStr(fTokenizer[fIndex]^.Text, 'private') then
    dis := 1
  else
    dis := 0;
  Result := False;
  if (fIndex >= fTokenizer.Tokens.Count - 2 - dis) then
    Exit;
  keyLen := -1;
  word := fTokenizer[fIndex+dis]^.Text;
  if StartsStr('struct',word) then
    keyLen:=6
  else if StartsStr('class', word)
    or StartsStr('union',word) then
    keyLen:=5;
  if keyLen<0 then
    Exit;
  Result := (length(word) = keyLen)
    or (word[keyLen+1] in [' ',#9,'[']);

  if Result then begin
    if fTokenizer[fIndex + 2+dis]^.Text[1] <> ';' then begin // not: class something;
      I := fIndex+dis +1;
      // the check for ']' was added because of this example:
      // struct option long_options[] = {
      //		{"debug", 1, 0, 'D'},
      //		{"info", 0, 0, 'i'},
      //    ...
      // };
      while (I < fTokenizer.Tokens.Count) do begin
        if (fTokenizer[I]^.Text[Length(fTokenizer[I]^.Text)]
          in [';', ':', '{', '}', ',', ')', ']','=','*','&'])  then begin
          break;
        end;
        Inc(I);
      end;

      if (I < fTokenizer.Tokens.Count) and not (fTokenizer[I]^.Text[1] in ['{', ':']) then begin
        Result := False;
      end;
    end;
  end;
end;

function TCppParser.CheckForMethod(var sType, sName, sArgs: AnsiString;
  var isStatic:boolean;var IsFriend:boolean): boolean;
var
  fIndexBackup, DelimPos,pos1: integer;
  bTypeOK, bNameOK, bArgsOK: boolean;
  s:AnsiString;
  scope:PStatement;

  function IsNotFuncArgs(args:String):boolean;
  var
    i:integer;
    endPos:integer;
    inComment:boolean;
    word : string;
    lastCharIsId:boolean;
    statement:PStatement;
  begin
    Result:=True;
    i:=2; //skip '('
    endPos:=Length(args)-1;//skip ')'
    inComment:=False;
    lastCharIsId:=False;
    while i<=endPos do begin
      if (args[i]='/') and (args[i+1]='*') and not inComment then begin
        if word <> '' then
          break;
        inComment:=True;
        inc(i,2);
        lastCharIsId:=False;
      end else if (args[i]='*') and (args[i+1]='/') and inComment then begin
        inComment:=False;
        inc(i,2);
        lastCharIsId:=False;
      end else if not inComment and (args[i] in ['"','''']) then begin
        // args contains a string/char, can't be a func define
        Exit;
      end else if not inComment and (args[i] in ['_','a'..'z','A'..'Z']) then begin
        word := word+args[i];
        lastCharIsId:=True;
        inc(i);
      end else if not inComment and (args[i] = ':') and (args[i+1] = ':') then begin
        lastCharIsId:=False;
        word := word+'::';
        inc(i,2);
      end else if not inComment and (args[i] in ['0'..'9']) then begin
        if not lastCharIsId then begin
          Exit;
        End;
        word := word+args[i];
        inc(i);
      end else if not inComment and (args[i] in [' ',#9,#13,#10])  then begin
        if word <> '' then
          break;
        inc(i);
      end else if not inComment and (word='') then begin
        Exit;
      end else
        inc(i);
    end;
    if (i>endPos) and (word='') then begin
      Result:=False;
      Exit;
    end;

    if isKeyword(word) then begin
      if sameStr(word,'true') or sameStr(word,'false') or sameStr(word,'nullptr') then
        Result:=True
      else
        Result:=False;
      Exit;
    end;

    statement := self.FindStatementOf(fCurrentFile,word,getCurrentScope,true);
    if assigned(statement) and not (statement^._Kind in [skClass,skTypedef,skEnumType]) then begin
      Result:=True;
      Exit;
    end;
    

    Result:=False;
  end;

begin
  scope := getCurrentScope();

  if assigned(scope) and not (scope^._Kind in [ skNamespace,skClass]) then begin //don't care function declaration in the function's
    Result:=False;
    Exit;
  end;

  // Function template:
  // compiler directives (>= 0 words), added to type
  // type (>= 1 words)
  // name (1 word)
  // (argument list)
  // ; or {

  IsStatic := False;
  IsFriend := False;

  sType := ''; // should contain type "int"
  sName := ''; // should contain function name "foo::function"
  sArgs := ''; // should contain argument list "(int a)"

  bTypeOK := false;
  bNameOK := false;
  bArgsOK := false;

  // Don't modify index
  fIndexBackup := fIndex;

  // Gather data for the string parts
  while (fIndex < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in ['(', ';', ':', '{', '}', '#']) do
    begin

    if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] = '(') then
      begin // and start of a function

      //it's not a function define
      if (fIndex+2 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 2]^.Text[1] = ',') then
        break;

      if (fIndex+2 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 2]^.Text[1] = ';') then begin
        if isNotFuncArgs(fTokenizer[fIndex + 1]^.Text) then
          break;
      end;

      sName := fTokenizer[fIndex]^.Text;
      sArgs := fTokenizer[fIndex + 1]^.Text;
      bTypeOK := sType <> '';
      bNameOK := sName <> '';
      bArgsOK := sArgs <> '';

      // Allow constructor/destructor too
      if not bTypeOk then begin

        // Check for constructor/destructor outside class body
        DelimPos := Pos('::', sName);
        if DelimPos > 0 then begin
          bTypeOK := true;
          sType := Copy(sName, 1, DelimPos - 1);

          // remove template staff
          pos1 := Pos('<', sType);
          if pos1>0 then begin
            Delete(sType,pos1,MaxInt);
            sName:=sType+Copy(sName,DelimPos,MaxInt);
          end;

        end;
      end;

      // Are we inside a class body?
      if not bTypeOK then begin
        sType := fTokenizer[fIndex]^.Text;
        if sType[1] = '~' then
          Delete(sType, 1, 1);
        bTypeOK := Assigned(IsInCurrentScopeLevel(sType)); // constructor/destructor
      end;
      break;

      // Still walking through type
    end else {if IsValidIdentifier(fTokenizer[fIndex]^.Text) then} begin
      s:=expandMacroType(fTokenizer[fIndex]^.Text);
      if SameStr(s, 'static') then
        IsStatic := True;
      if SameStr(s, 'friend') then
        IsFriend := True;
      if s<>'' then
        sType := sType + ' '+ s;
      bTypeOK := sType <> '';
    end;
    Inc(fIndex);
  end;


  fIndex := fIndexBackup;

  // Correct function, don't jump over
  if bTypeOK and bNameOK and bArgsOK then begin

    Result := true;
    sType := TrimRight(sType); // should contain type "int"
    sName := TrimRight(sName); // should contain function name "foo::function"
    sArgs := TrimRight(sArgs); // should contain argument list "(int a)"
  end else
    Result := false;
end;

function TCppParser.CheckForScope: boolean;
begin
  Result := (fIndex < fTokenizer.Tokens.Count - 1) and
    (fTokenizer[fIndex + 1]^.Text = ':') and
    (SameStr(fTokenizer[fIndex]^.Text, 'public') or
    SameStr(fTokenizer[fIndex]^.Text, 'protected') or
    SameStr(fTokenizer[fIndex]^.Text, 'private'));
end;

function TCppParser.CheckForVar: boolean;
var
  I, fIndexBackup: integer;
begin
  // Be pessimistic
  Result := False;

  // Store old index
  fIndexBackup := fIndex;

  // Use fIndex so we can reuse checking functions
  if fIndex + 1 < fTokenizer.Tokens.Count then begin

    // Check the current and the next token
    for I := 0 to 1 do begin
      if CheckForKeyword or
        (fTokenizer[fIndex]^.Text[1] in ['#', ',', ';', ':', '{', '}', '!', '/', '+', '-', '<', '>']) or
        (fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] = '.') or
      ((Length(fTokenizer[fIndex]^.Text) > 1) and
        (fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text) - 1] = '-') and
        (fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] = '>')) then begin

        // Reset index and fail
        fIndex := fIndexBackup;
        Exit; // fail

        // Could be a function pointer?
      end else if (fTokenizer[fIndex]^.Text[1] in ['(']) then begin

        // Quick fix: there must be a pointer operator in the first tiken
        if (fIndex + 1 >= fTokenizer.Tokens.Count) or
          (fTokenizer[fIndex + 1]^.Text[1] <> '(') or
          (Pos('*', fTokenizer[fIndex]^.Text) = 0) then begin

          // Reset index and fail
          fIndex := fIndexBackup;
          Exit; // fail
        end;
      end;
      Inc(fIndex);
    end;
  end;

  // Revert to the point we started at
  fIndex := fIndexBackup;

  // Fail if we do not find a comma or a semicolon or a ( (inline constructor)
  while fIndex < fTokenizer.Tokens.Count do begin
    if (fTokenizer[fIndex]^.Text[1] in ['#', '}']) or CheckForKeyword then begin
      Break; // fail
    end else if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '(') and
      (fTokenizer[fIndex]^.Text[2] = '(') then begin // TODO: is this used to remove __attribute stuff?
      Break;
    end else if fTokenizer[fIndex]^.Text[1] in [',', ';','{'] then begin
      Result := True;
      Break;
    end;
    Inc(fIndex);
  end;

  // Revert to the point we started at
  fIndex := fIndexBackup;
end;

procedure TCppParser.HandleOtherTypedefs;
var
  NewType, OldType: AnsiString;
  startLine: integer;
  p:integer;
begin
  startLine := fTokenizer[fIndex]^.Line;
  // Skip typedef word
  Inc(fIndex);

  if (fIndex>=fTokenizer.Tokens.Count) then
    Exit;

  if fTokenizer[fIndex]^.Text[1] in ['(', ',', ';'] then begin // error typedef
    //skip to ;
    while (fIndex< fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] = ';') do
      Inc(fIndex);
    //skip ;
    if (fIndex< fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = ';') then
      Inc(fIndex);
  end;
  // Walk up to first new word (before first comma or ;)
  repeat
    OldType := OldType + fTokenizer[fIndex]^.Text + ' ';
    {
    if fTokenizer[fIndex]^.Text = '*' then begin // * is not part of type info
      Inc(fIndex);
      break;
    end;
    }
    Inc(fIndex);
  until (fIndex + 1 >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex + 1]^.Text[1] in [',', ';'])
    or (fIndex + 2 >= fTokenizer.Tokens.Count) or
      ((fTokenizer[fIndex + 1]^.Text[1]='(') and (fTokenizer[fIndex + 2]^.Text[1] in [',', ';']));
  OldType:= TrimRight(OldType);


  // Add synonyms for old
  if (fIndex+1 < fTokenizer.Tokens.Count) and (OldType <> '') then begin
    repeat
      // Support multiword typedefs
      if (fIndex+2 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 2]^.Text[1] in [',', ';']) then begin // function define
        if (fIndex + 2 < fTokenizer.Tokens.Count) and (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] = '(') then begin
          //valid function define
          NewType:=TrimRight(fTokenizer[fIndex]^.Text);
          NewType:=Copy(NewType,2,Length(NewType)-2); //remove '(' and ')';
          NewType:=TrimRight(NewType);
          p:=LastDelimiter(' ',NewType);
          if p <> 0 then
            NewType := Copy(NewType,p+1,Length(NewType)-p);
          AddStatement(
            GetCurrentScope,
            fCurrentFile,
            'typedef ' + OldType + ' ' + fTokenizer[fIndex]^.Text + ' ' + fTokenizer[fIndex + 1]^.Text, // do not override hint
            OldType,
            NewType,
            fTokenizer[fIndex + 1]^.Text,
            '',
            startLine,
            skTypedef,
            GetScope,
            fClassScope,
            True,
            nil,
            False);
         end;
         NewType:='';
         //skip to ',' or ';'
         Inc(fIndex,2);
         {
         while (fIndex< fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in [',', ';']) do
            Inc(fIndex);
         }
      end else if not (fTokenizer[fIndex+1]^.Text[1] in [',', ';', '(']) then begin
        NewType := NewType + fTokenizer[fIndex]^.Text + ' ';
        Inc(fIndex);
      end else begin
        NewType := NewType + fTokenizer[fIndex]^.Text + ' ';
        NewType := TrimRight(NewType);
        AddStatement(
          GetCurrentScope,
          fCurrentFile,
          'typedef ' + OldType + ' ' + NewType, // override hint
          OldType,
          NewType,
          '',
          '',
          //fTokenizer[fIndex]^.Line,
          startLine,
          skTypedef,
          GetScope,
          fClassScope,
          True,
          nil,
          False);
        NewType := '';
        Inc(fIndex);
      end;
      if (fIndex>= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] = ';') then
        break
      else if fTokenizer[fIndex]^.Text[1] = ',' then
        Inc(fIndex);
    until (fIndex+1 >= fTokenizer.Tokens.Count);
  end;

  // Step over semicolon (saves one HandleStatement loop)
  Inc(fIndex);
end;

procedure TCppParser.OnProgress(FileName:String;Total,Current:integer);
var
  msg: PCppParserProgressMessage;
begin
  new(msg);
  msg^.FileName := FileName;
  msg^.Total := total;
  msg^.Current := Current;
  PostMessage(fHandle, WM_PARSER_PROGRESS,integer(msg),0);
end;

procedure TCppParser.HandlePreprocessor;
var
  DelimPos, Line: Integer;
  S, Name, Args, Value, HintText: AnsiString;
begin
  if StartsStr('#include ', fTokenizer[fIndex]^.Text) then begin // start of new file
    // format: #include fullfilename:line
    // Strip keyword
    S := Copy(fTokenizer[fIndex]^.Text, Length('#include ') + 1, MaxInt);
    DelimPos := LastPos(':', S);
    if DelimPos > 3 then begin // ignore full file name stuff
      fCurrentFile := Copy(S, 1, DelimPos - 1);
      fIsSystemHeader := IsSystemHeaderFile(fCurrentFile) or IsProjectHeaderFile(fCurrentFile);
      fIsProjectFile := FastIndexOf(fProjectFiles,fCurrentFile) <> -1;
      fIsHeader := IsHfile(fCurrentFile);

      // Mention progress to user if we enter a NEW file
      Line := StrToIntDef(Copy(S, DelimPos + 1, MaxInt), -1);
      if Line = 1 then begin
        Inc(fFilesScannedCount);
        Inc(fFilesToScanCount);
        OnProgress(fCurrentFile,fFilesToScanCount,fFilesScannedCount);
      end;
    end;
  end else if StartsStr('#define ', fTokenizer[fIndex]^.Text) then begin

    // format: #define A B, remove define keyword
    S := TrimLeft(Copy(fTokenizer[fIndex]^.Text, Length('#define ') + 1, MaxInt));

    // Ask the preprocessor to cut parts up
    if Assigned(fPreprocessor) then
      fPreprocessor.GetDefineParts(S, Name, Args, Value);

    // Generate custom hint
    HintText := '#define';
    if Name <> '' then
      HintText := HintText + ' ' + Name;
    if Args <> '' then
      HintText := HintText + ' ' + Args;
    if Value <> '' then
      HintText := HintText + ' ' + Value;

    AddStatement(
      nil, // defines don't belong to any scope
      fCurrentFile,
      HintText, // override hint
      '', // define has no type
      Name,
      Args,
      Value,
      fTokenizer[FIndex]^.Line,
      skPreprocessor,
      ssGlobal,
      scsNone,
      True,
      nil,
      False);
  end;
  //TODO "#undef support"
  Inc(fIndex);
end;

procedure TCppParser.HandleUsing;
var
  scopeStatement: PStatement;
  usingName,fullname: AnsiString;
  usingList : TStringList;
  i: integer;
  fileInfo:PFileIncludes;
begin
  if fCurrentFile='' then begin
    //skip to ;
    while (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex].Text<>';') do
      inc(fIndex);
    exit;
  end;

  Inc(fIndex); //skip 'using'

  //todo: handle using std:vector;
  if (fIndex+2>=fTokenizer.Tokens.Count) or not (fTokenizer[fIndex].Text = 'namespace') then begin
    Exit;
  end;
  Inc(fIndex);  // skip namespace
  scopeStatement:=GetCurrentScope;

  usingName := fTokenizer[fIndex]^.Text;
  inc(fIndex);

  if Assigned(scopeStatement) then begin
    usingList := scopeStatement^._Usings;
    fullname := scopeStatement^._FullName + '::' + usingName;
  end else begin
    fileInfo:=FindFileIncludes(fCurrentFile);
    if not Assigned(fileInfo) then
      Exit;
    usingList:=fileInfo.Usings;
    fullname := usingName;
  end;
  i:=FastIndexOf(fNamespaces, fullname);
  if (i=-1) and Assigned(scopeStatement) then begin //assume it's a global namespace, try agin
    fullname := usingName;
    i:=FastIndexOf(fNamespaces, fullname);
  end;
  if (i<>-1) and (FastIndexOf(usingList,fullname)=-1) then
    usingList.Add(fullname);
end;

procedure TCppParser.HandleNamespace;
var
  Command, aliasName: AnsiString;
  NamespaceStatement: PStatement;
  startLine,i: integer;
  isInline: boolean;
begin
  isInline:=False;
  if SameStr(fTokenizer[fIndex]^.Text, 'inline') then begin
    isInline:=True;
    Inc(fIndex); //skip 'inline'
  end;


  startLine := fTokenizer[fIndex]^.Line;
  Inc(fIndex); //skip 'namespace'

  if (fTokenizer[fIndex]^.Text[1] in [';', '=', '{']) then begin
    //wrong namespace define, stop handling
    Exit;
  end;
  Command := fTokenizer[fIndex]^.Text;
  if StartsStr('__', Command) then // hack for inline namespaces
    isInline:= True;
  inc(fIndex);
  if (fIndex>=fTokenizer.Tokens.Count) then
    Exit;
  if (fIndex+2<fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '=') then begin
    aliasName:=fTokenizer[fIndex+1].Text;
    //namespace alias
    AddStatement(
      GetCurrentScope,
      fCurrentFile,
      '', // do not override hint
      aliasName, // name of the alias namespace
      Command, // command
      '', // args
      '', // values
      //fTokenizer[fIndex]^.Line,
      startLine,
      skNamespaceAlias,
      GetScope,
      fClassScope,
      True,
      nil,
      False);
    inc(fIndex,2); //skip ;
    Exit;
  end else if isInline then begin
    //inline namespace , just skip it
    // Skip to '{'
    while (fIndex<fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] <> '{') do
      Inc(fIndex);
    i:=SkipBraces(fIndex); //skip '}'
    if i=fIndex then
      fInlineNamespaceEndSkips.Add(fTokenizer.Tokens.Count)
    else
      fInlineNamespaceEndSkips.Add(i);

    if (fIndex<fTokenizer.Tokens.Count) then
      Inc(fIndex); //skip '{'  
  end else  begin
    NamespaceStatement := AddStatement(
      GetCurrentScope,
      fCurrentFile,
      '', // do not override hint
      '', // type
      Command, // command
      '', // args
      '', // values
      startLine,
      skNamespace,
      GetScope,
      fClassScope,
      True,
      nil, //inheritance
      False);
    AddSoloScopeLevel(NamespaceStatement,startLine);

    // Skip to '{'
    while (fIndex<fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] <> '{') do
      Inc(fIndex);
    if (fIndex<fTokenizer.Tokens.Count) then
      Inc(fIndex); //skip '{'
  end;
end;

procedure TCppParser.HandleStructs(IsTypedef: boolean = False);
var
  Command, Prefix, OldType, NewType, Args: AnsiString;
  I: integer;
  IsStruct: boolean;
  IsFriend: boolean;
  ParentStatement,FirstSynonym: PStatement;
  startLine: integer;
begin
  IsFriend := False;
  Prefix := fTokenizer[fIndex]^.Text;
  if SameStr(Prefix, 'friend') then begin
    IsFriend := True;
    Inc(fIndex);
  end;
  // Check if were dealing with a struct or union
  Prefix := fTokenizer[fIndex]^.Text;
  IsStruct := StartsStr('struct',Prefix) or StartsStr('union',Prefix);
  startLine := fTokenizer[fIndex]^.Line;

  Inc(fIndex); //skip struct/class/union

  if fIndex>=fTokenizer.Tokens.Count then
    Exit;

  // Do not modifiy index initially
  I := fIndex;

  // Skip until the struct body starts
  while (I < fTokenizer.Tokens.Count) and not (fTokenizer[I]^.Text[1] in [';', '{']) do
    Inc(I);

  // Forward class/struct decl *or* typedef, e.g. typedef struct some_struct synonym1, synonym2;
  if (I < fTokenizer.Tokens.Count) and (fTokenizer[I]^.Text[1] = ';') then begin
    // typdef struct Foo Bar
    if IsTypedef then begin
      OldType := fTokenizer[fIndex]^.Text;
      repeat
        // Add definition statement for the synonym
        if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] in [',', ';']) then begin
          NewType := fTokenizer[fIndex]^.Text;
          AddStatement(
            GetCurrentScope,
            fCurrentFile,
            'typedef ' + Prefix + ' ' + OldType + ' ' + NewType, // override hint
            OldType,
            NewType,
            '',
            '',
            //fTokenizer[fIndex]^.Line,
            startLine,
            skTypedef,
            GetScope,
            fClassScope,
            True,
            nil,
            False);
        end;
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] = ';');

      // Forward declaration, struct Foo. Don't mention in class browser
    end else begin
      if IsFriend then begin
        ParentStatement := GetCurrentScope;
        if Assigned(ParentStatement) then begin
          if not Assigned(ParentStatement^._Friends) then
            ParentStatement^._Friends:=TStringHash.Create;
          ParentStatement^._Friends.Add(fTokenizer[fIndex]^.Text,1);
        end;
      end;
      Inc(I); // step over ;
      fIndex := I;
    end;

    // normal class/struct decl
  end else begin
    FirstSynonym := nil;

    // Add class/struct name BEFORE opening brace
    if fTokenizer[fIndex]^.Text[1] <> '{' then begin
      repeat
        if (fIndex + 1 < fTokenizer.Tokens.Count)
          and (fTokenizer[fIndex + 1]^.Text[1] in [',', ';', '{', ':']) then begin
          Command := fTokenizer[fIndex]^.Text;
          if (Command <> '') then begin
            FirstSynonym := AddStatement(
              GetCurrentScope,
              fCurrentFile,
              '', // do not override hint
              Prefix, // type
              Command, // command
              '', // args
              '', // values
              //fTokenizer[fIndex]^.Line,
              startLine,
              skClass,
              GetScope,
              fClassScope,
              True,
              TList.Create,
              False);
            Command := '';
          end;
          Inc(fIndex);
        end else if (fIndex + 2 < fTokenizer.Tokens.Count)
          and SameText(fTokenizer[fIndex + 1]^.Text,'final')
          and (fTokenizer[fIndex + 2]^.Text[1] in [',', ';', '{', ':']) then begin
          Command := fTokenizer[fIndex]^.Text;
          if (Command <> '') then begin
            FirstSynonym := AddStatement(
              GetCurrentScope,
              fCurrentFile,
              '', // do not override hint
              Prefix, // type
              Command, // command
              '', // args
              '', // values
              //fTokenizer[fIndex]^.Line,
              startLine,
              skClass,
              GetScope,
              fClassScope,
              True,
              TList.Create,
              False);
            Command := '';
          end;
          Inc(fIndex,2);
        end else
          Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [':', '{', ';']);
    end;

    // Walk to opening brace if we encountered inheritance statements
    if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = ':') then begin
      if Assigned(FirstSynonym) then
        SetInheritance(fIndex, FirstSynonym,IsStruct); // set the _InheritanceList value
      while (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] <> '{') do // skip decl after ':'
        Inc(fIndex);
    end;

    // Check for struct synonyms after close brace
    if IsStruct then begin

      // Walk to closing brace
      I := SkipBraces(fIndex); // step onto closing brace

      // When encountering names again after struct body scanning, skip it
      if (I + 1 < fTokenizer.Tokens.Count) and not (fTokenizer[I + 1]^.Text[1] in [';', '}']) then
        fSkipList.Add(Pointer(I+1)); // add first name to skip statement so that we can skip it until the next ;

      if (I + 1 < fTokenizer.Tokens.Count) and not (fTokenizer[I + 1]^.Text[1] in [';', '}']) then begin
        Command := '';
		    Args:='';
          // Add synonym before opening brace
        repeat
          Inc(I);

          if not (fTokenizer[I]^.Text[1] in ['{', ',', ';']) then begin
            if (fTokenizer[I]^.Text[1] = '_') and (fTokenizer[I]^.Text[Length(fTokenizer[I]^.Text)] = '_') then begin
              // skip possible gcc attributes
              // start and end with 2 underscores (i.e. __attribute__)
              // so, to avoid slow checks of strings, we just check the first and last letter of the token
              // if both are underscores, we split
              Break;
            end else begin
              if (fTokenizer[I]^.Text[Length(fTokenizer[I]^.Text)] = ']') then begin // cut-off array brackets
                Command := Command + Copy(fTokenizer[I]^.Text, 1, Pos('[', fTokenizer[I]^.Text) - 1) + ' ';
                Args := Copy(fTokenizer[I]^.Text, Pos('[', fTokenizer[I]^.Text),MaxInt);
              end else if fTokenizer[I]^.Text[1] in ['*', '&'] then // do not add spaces after pointer operator
                Command := Command + fTokenizer[I]^.Text
              else
                Command := Command + fTokenizer[I]^.Text + ' ';
              end;
            end else begin
              Command := TrimRight(Command);
              if (Command <> '') and
                (
                  (not assigned(FirstSynonym)) or
                  (not SameStr(Command,FirstSynonym^._Command))
                ) then begin
                //not define the struct yet, we define a unamed struct
                if not assigned(FirstSynonym) then begin
                    FirstSynonym := AddStatement(
                      GetCurrentScope,
                      fCurrentFile,
                      '', // do not override hint
                      Prefix,
                      '__'+Command,
                      '',
                      '',
                      startLine,
                      skClass,
                      GetScope,
                      fClassScope,
                      True,
                      TList.Create,
                      False);
                end;
                if isTypeDef then begin
                  //typedef
                  AddStatement(
                    GetCurrentScope,
                    fCurrentFile,
                    'typedef ' + FirstSynonym^._Command + ' ' + Command, // override hint
                    FirstSynonym^._Command,
                    Command,
                    '',
                    '',
                    fTokenizer[fIndex]^.Line,
                    skTypedef,
                    GetScope,
                    fClassScope,
                    True,
                    nil,
                    False); // typedef
                end else begin
                  //variable define
                  AddStatement(
                    GetCurrentScope,
                    fCurrentFile,
                    '', // do not override hint
                    FirstSynonym^._Command,
                    Command,
                    Args,
                    '',
                    fTokenizer[I]^.Line,
                    skVariable,
                    GetScope,
                    fClassScope,
                    True,
                    nil,
                    False); // TODO: not supported to pass list
                end;
              end;
              Command := '';
            end;
        until (I >= fTokenizer.Tokens.Count - 1) or (fTokenizer[I]^.Text[1] in ['{', ';']);

        // Nothing worth mentioning after closing brace
        // Proceed to set first synonym as current class
      end;
    end;
    AddSoloScopeLevel(FirstSynonym,startLine);

    // Step over {
    if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '{') then
      Inc(fIndex);
  end;
end;

function TCppParser.GetCurrentBlockEndSkip:integer;
begin
  Result := fTokenizer.Tokens.Count+1;
  if fBlockEndSkips.Count<=0 then
    Exit;
  Result := fBlockEndSkips[fBlockEndSkips.Count-1];
end;


function TCppParser.GetCurrentInlineNamespaceEndSkip:integer;
begin
  Result := fTokenizer.Tokens.Count+1;
  if fInlineNamespaceEndSkips.Count<=0 then
    Exit;
  Result := fInlineNamespaceEndSkips[fInlineNamespaceEndSkips.Count-1];
end;


function TCppParser.GetCurrentBlockBeginSkip:integer;
begin
  Result := fTokenizer.Tokens.Count+1;
  if fBlockBeginSkips.Count<=0 then
    Exit;
  Result := fBlockBeginSkips[fBlockBeginSkips.Count-1];
end;


procedure TCppParser.HandleForBlock;
var
  i,i2:integer;
  block:PStatement;
  startLine:integer;
begin
  startLine:= fTokenizer[fIndex].Line;
  Inc(fIndex); // skip for/catch;
  if not (fIndex < fTokenizer.Tokens.Count) then
    Exit;
  i:=fIndex;
  while (i<fTokenizer.Tokens.Count) and (fTokenizer[i].Text[1]<>';') do
    inc(i);
  if i>=fTokenizer.Tokens.Count then
    Exit;
  i2:=i+1; //skip to ';'
  if i2>=fTokenizer.Tokens.Count then
    Exit;
  if fTokenizer[i2].Text[1] = '{' then begin
    fBlockBeginSkips.Add(i2);
    i:=SkipBraces(i2);
    if i=i2 then
      fBlockEndSkips.Add(fTokenizer.Tokens.Count)
    else
      fBlockEndSkips.Add(i);
  end else begin
    i:=i2;
    while (i<fTokenizer.Tokens.Count) and (fTokenizer[i].Text[1]<>';') do
      inc(i);
    fBlockEndSkips.Add(i);
  end;
  // add a block
  block := AddStatement(
        GetCurrentScope,
        fCurrentFile,
        '', // override hint
        '',
        '',
        '',
        '',
        startLine,
        skBlock,
        GetScope,
        fClassScope,
        True,
        nil,
        False);

  AddSoloScopeLevel(block,startLine);
end;

procedure TCppParser.HandleCatchBlock;
var
  i,i2:integer;
  block:PStatement;
  startLine:integer;
begin
  startLine:= fTokenizer[fIndex]^.Line;
  Inc(fIndex); // skip for/catch;
  if not ((fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '(')) then
    Exit;
  //skip params
  i2:=fIndex+1;
  if i2>=fTokenizer.Tokens.Count then
    Exit;
  if fTokenizer[i2].Text[1] = '{' then begin
    fBlockBeginSkips.Add(i2);
    i:=SkipBraces(i2);
    if i=i2 then
      fBlockEndSkips.Add(fTokenizer.Tokens.Count)
    else
      fBlockEndSkips.Add(i);
  end else begin
    i:=i2;
    while (i<fTokenizer.Tokens.Count) and (fTokenizer[i].Text[1]<>';') do
      inc(i);
    fBlockEndSkips.Add(i);
  end;
  // add a block
  block := AddStatement(
        GetCurrentScope,
        fCurrentFile,
        '', // override hint
        '',
        '',
        '',
        '',
        startLine,
        skBlock,
        GetScope,
        fClassScope,
        True,
        nil,
        False);
  AddSoloScopeLevel(block,startLine);
  if not containsStr('...',fTokenizer[fIndex]^.Text) then
    scanMethodArgs(block,fTokenizer[fIndex]^.Text);
end;


procedure TCppParser.HandleMethod(const sType, sName, sArgs: AnsiString; isStatic: boolean; IsFriend:boolean);
var
  IsValid, IsDeclaration: boolean;
  I, DelimPos: integer;
  FunctionKind: TStatementKind;
  ParentClassName, ScopelessName: AnsiString;
  FunctionClass, FunctionStatement: PStatement;
  startLine : integer;

begin
  IsValid := True;
  IsDeclaration := False; // assume it's not a prototype
  I := fIndex;
  startLine := fTokenizer[fIndex]^.Line;

  // Skip over argument list
  while (fIndex < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in [';', ':', '{', '}']) do
    Inc(fIndex);

  if (fIndex >= fTokenizer.Tokens.Count) then // not finished define, just skip it;
    Exit;

  FunctionClass := GetCurrentScope;
  // Check if this is a prototype
  if (fTokenizer[fIndex]^.Text[1] in [';', '}']) then begin // prototype
    IsDeclaration := True;
    {
    if not fIsHeader and not Assigned(FunctionClass) then // in a CPP file
      IsValid := False; // not valid
    }
  end else begin

    // Find the function body start after the inherited constructor
    if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = ':') then
      while (fIndex < fTokenizer.Tokens.Count) and (not (fTokenizer[fIndex]^.Text[1] in [';', '{', '}'])) do
        Inc(fIndex);

    // Still a prototype
    if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] in [';', '}']) then begin
      IsDeclaration := True;
      //Forward declaration, that's ok
      {
      if not fIsHeader and not Assigned(FunctionClass) then
        IsValid := False;
      }
    end;
  end;

  if IsFriend and IsDeclaration and Assigned(FunctionClass) then begin
    DelimPos := Pos('::', sName);
    if DelimPos > 0 then begin
      ScopelessName := Copy(sName, DelimPos + 2, MaxInt);
//      ParentClassName := Copy(sName, 1, DelimPos - 1);
//        FunctionClass := GetIncompleteClass(ParentClassName);
    end else
      ScopelessName := sName;
//TODO : we should check namespace
    if not Assigned(FunctionClass^._Friends) then
      FunctionClass^._Friends:=TStringHash.Create;
    FunctionClass^._Friends.Add(ScopelessName,1);
  end else if IsValid then begin

    // Use the class the function belongs to as the parent ID if the function is declared outside of the class body
    DelimPos := Pos('::', sName);
    if DelimPos > 0 then begin

      // Provide Bar instead of Foo::Bar
      ScopelessName := Copy(sName, DelimPos + 2, MaxInt);

      // Check what class this function belongs to
      ParentClassName := Copy(sName, 1, DelimPos - 1);
      FunctionClass := GetIncompleteClass(ParentClassName, GetCurrentScope);
    end else
      ScopelessName := sName;

    // Determine function type
    if SameStr(ScopelessName, sType) then
      FunctionKind := skConstructor
    else if SameStr(ScopelessName, '~' + sType) then
      FunctionKind := skDestructor
    else
      FunctionKind := skFunction;

    // For function definitions, the parent class is given. Only use that as a parent
    if not IsDeclaration then begin
      FunctionStatement:=AddStatement(
        FunctionClass,
        fCurrentFile,
        '', // do not override hint
        sType,
        ScopelessName,
        sArgs,
        '',
        //fTokenizer[fIndex - 1]^.Line,
        startLine,
        FunctionKind,
        GetScope,
        fClassScope,
        not IsDeclaration,
        nil,
        IsStatic);
      ScanMethodArgs( FunctionStatement, FunctionStatement^._Args);
      // For function declarations, any given statement can belong to multiple typedef names
      if assigned(FunctionClass) and (FunctionClass^._Kind = skClass) and not isStatic then begin
        //add this to non-static class member function
       AddStatement(
        FunctionStatement,
        fCurrentFile,
        '', // do not override hint
        FunctionClass^._Command,
        'this',
        '',
        '',
        startLine,
        skVariable,
        ssLocal,
        scsNone,
        true,
        nil,
        False);
      end;
    end else
      FunctionStatement:=AddChildStatement(
        GetCurrentScope,
        fCurrentFile,
        '', // do not override hint
        sType,
        ScopelessName,
        sArgs,
        '',
        //fTokenizer[fIndex - 1]^.Line,
        startLine,
        FunctionKind,
        GetScope,
        fClassScope,
        not IsDeclaration,
        IsStatic);
  end;

  if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '{') then begin
    AddSoloScopeLevel(FunctionStatement,startLine);
    inc(fIndex); //skip '{'
  end else if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = ';') then begin
    AddSoloScopeLevel(FunctionStatement,startLine);
    if (fTokenizer[fIndex]^.Line <> startLine) then
      RemoveScopeLevel(fTokenizer[fIndex]^.Line+1)
    else
      RemoveScopeLevel(startLine+1);
    Inc(fIndex);
  end;
  if I = fIndex then // if not moved ahead, something is wrong but don't get stuck ;)
    if fIndex < fTokenizer.Tokens.Count then
      if not (fTokenizer[fIndex]^.Text[1] in ['{', '}']) then
        Inc(fIndex);
end;

procedure TCppParser.HandleScope;
begin
  if SameStr(fTokenizer[fIndex]^.Text, 'public') then
    fClassScope := scsPublic
  else if SameStr(fTokenizer[fIndex]^.Text, 'private') then
    fClassScope := scsPrivate
  else if SameStr(fTokenizer[fIndex]^.Text, 'protected') then
    fClassScope := scsProtected
  else
    fClassScope := scsNone;
  Inc(fIndex, 2); // the scope is followed by a ':'
end;

procedure TCppParser.HandleKeyword;
var
  skipType : TSkipType;
  v: integer;
begin
  // Skip

  v := CppKeywords.ValueOf(fTokenizer[fIndex]^.Text);
  { {no need check here because we have checked in CheckForKeyword )
  if v<0 then
    Exit;
    }
  skipType := TSkipType(v);
  case skipType of
    skItself: begin
      // skip it;
      Inc(fIndex);
    end;
    skToSemicolon: begin
      // Skip to ;
      repeat
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';']);
      Inc(fIndex); // step over
    end;
    skToColon: begin
      // Skip to :
      repeat
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [':']);
    end;
    skToRightParenthesis: begin
      // skip to )
      repeat
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] in [')']);
      Inc(fIndex); // step over
    end;
    skToLeftBrace: begin
      // Skip to {
      repeat
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in ['{']);
    end;
    skToRightBrace: begin
      // Skip to }
      repeat
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in ['}']);
      Inc(fIndex); // step over
    end;
  end;
end;


procedure TCppParser.HandleVar;
var
  LastType, Args, Cmd, S: AnsiString;
  IsFunctionPointer: boolean;
  IsStatic : boolean;
  varAdded:boolean;
  i:integer;
begin
  IsStatic := False;
  // Keep going and stop on top of the variable name
  LastType := '';
  IsFunctionPointer := False;
  varAdded:=False;
  repeat
    if (fIndex + 2 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] = '(') and (fTokenizer[fIndex +
      2]^.Text[1] = '(') then begin
      IsFunctionPointer := Pos('*', fTokenizer[fIndex + 1]^.Text) > 0;
      if not IsFunctionPointer then
        break; // inline constructor
    end else if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] in ['(', ',', ';', ':', '}',
      '#','{']) then begin
        break;
    end;


    // we've made a mistake, this is a typedef , not a variable definition.
    if SameStr(fTokenizer[fIndex]^.Text, 'typedef') then
      Exit;

    // struct/class/union is part of the type signature
    // but we dont store it in the type cache, so must trim it to find the type info
    if (not SameStr(fTokenizer[fIndex]^.Text, 'struct')) and
      (not SameStr(fTokenizer[fIndex]^.Text, 'class')) and
      (not SameStr(fTokenizer[fIndex]^.Text, 'union')) then  begin
      if fTokenizer[fIndex]^.Text = ':' then begin
          LastType := LastType + ':';
      end else begin
        s:=expandMacroType(fTokenizer[fIndex]^.Text);
        if s<>'' then
          LastType := LastType + ' '+s;
        if SameStr(s,'static') then
          isStatic := True;
      end;
    end;
    Inc(fIndex);
  until (fIndex >= fTokenizer.Tokens.Count) or IsFunctionPointer;
  LastType := Trim(LastType);

  // Don't bother entering the scanning loop when we have failed
  if fIndex >= fTokenizer.Tokens.Count then
    Exit;

  // Find the variable name
  repeat

    // Skip bit identifiers,
    // e.g.:
    // handle
    // unsigned short bAppReturnCode:8,reserved:6,fBusy:1,fAck:1
    // as
    // unsigned short bAppReturnCode,reserved,fBusy,fAck
    if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = ':') then begin
      repeat
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [',', ';', '{', '}']);
    end;

    // Skip inline constructors,
    // e.g.:
    // handle
    // int a(3)
    // as
    // int a
    if (not IsFunctionPointer) and (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '(') then begin
      while (fIndex < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in [',', ';', '{', '}']) do
        Inc(fIndex);
    end;

    // Did we stop on top of the variable name?
    if fIndex < fTokenizer.Tokens.Count then begin
      if not (fTokenizer[fIndex]^.Text[1] in [',', ';']) then begin
        if IsFunctionPointer and (fIndex + 1 < fTokenizer.Tokens.Count) then begin
          S := fTokenizer[fIndex]^.Text;
          Cmd := Trim(Copy(S, 3, Length(S) - 3)); // (*foo) -> foo
          Args := fTokenizer[fIndex + 1]^.Text; // (int a,int b)
          LastType := LastType + '(*)' + Args; // void(int a,int b)
          Inc(fIndex);
        end else if fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] = ']' then begin //array; break args
          Cmd := Copy(fTokenizer[fIndex]^.Text, 1, Pos('[', fTokenizer[fIndex]^.Text) - 1);
          Args := Copy(fTokenizer[fIndex]^.Text, Pos('[', fTokenizer[fIndex]^.Text), Length(fTokenizer[fIndex]^.Text) -
            Pos('[', fTokenizer[fIndex]^.Text) + 1);
        end else begin
          Cmd := fTokenizer[fIndex]^.Text;
          Args := '';
        end;

        // Add a statement for every struct we are in
        AddChildStatement(
          GetCurrentScope,
          fCurrentFile,
          '', // do not override hint
          LastType,
          Cmd,
          Args,
          '',
          fTokenizer[fIndex]^.Line,
          skVariable,
          GetScope,
          fClassScope,
          True,
          IsStatic); // TODO: not supported to pass list

        varAdded:=True;
      end;

      // Step over the variable name
      if not (fTokenizer[fIndex]^.Text[1] in [';', '{', '}']) then
        Inc(fIndex);
    end;
  until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';', '{', '}']);

  if varAdded and (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text = '{') then begin
    // skip { } like A x {new A};
    i:=skipBraces(fIndex);
    if (i<>fIndex) then
      fIndex := i+1;
  end;
  // Skip ; and ,
  if (fIndex < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in ['{', '}']) then
    Inc(fIndex);

end;

procedure TCppParser.HandleEnum;
var
  LastType: AnsiString;
  Args: AnsiString;
  Cmd: AnsiString;
  EnumName: AnsiString;
  I: integer;
  startLine: integer;
begin
  EnumName := '';
  startLine := fTokenizer[fIndex]^.Line;
  Inc(fIndex); //skip 'enum'
  if fTokenizer[fIndex]^.Text[1] = '{' then begin // enum {...} NAME

    // Skip to the closing brace
    I := SkipBraces(fIndex);

    // Have we found the name?
    if (I + 1 < fTokenizer.Tokens.Count) and (fTokenizer[I]^.Text[1] = '}') then
      if fTokenizer[I + 1]^.Text[1] <> ';' then
        EnumName := Trim(fTokenizer[I + 1]^.Text);
  end else begin // enum NAME {...};
    while (fIndex < fTokenizer.Tokens.Count) and (not (fTokenizer[fIndex]^.Text[1] in ['{', ';'])) do begin
      EnumName := EnumName + fTokenizer[fIndex]^.Text + ' ';
      Inc(fIndex);
    end;
    EnumName := Trim(EnumName);

    // An opening brace must be present after NAME
    if (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] <> '{') then
      Exit;
  end;

  // Add statement for enum name too
  if EnumName <> '' then
    AddStatement(
      GetCurrentScope,
      fCurrentFile,
      'enum '+EnumName,
      'enum',
      EnumName,
      Args,
      '',
      //fTokenizer[fIndex]^.Line,
      startLine,
      skEnumType,
      GetScope,
      fClassScope,
      True,
      nil,
      False);

  // Skip opening brace
  Inc(fIndex);

  // Call every member "enum NAME ITEMNAME"
  LastType := TrimRight('enum ' + EnumName);
  if fTokenizer[fIndex]^.Text[1] <> '}' then begin
    repeat
      if not (fTokenizer[fIndex]^.Text[1] in [',', ';']) then begin
        if fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] = ']' then begin //array; break args
          Cmd := Copy(fTokenizer[fIndex]^.Text, 1, Pos('[', fTokenizer[fIndex]^.Text) - 1);
          Args := Copy(fTokenizer[fIndex]^.Text, Pos('[', fTokenizer[fIndex]^.Text), Length(fTokenizer[fIndex]^.Text) -
            Pos('[', fTokenizer[fIndex]^.Text) + 1);
        end else begin
          Cmd := fTokenizer[fIndex]^.Text;
          Args := '';
        end;
        AddStatement(
          GetCurrentScope,
          fCurrentFile,
          LastType + ' ' + fTokenizer[fIndex]^.Text, // override hint
          LastType,
          Cmd,
          Args,
          '',
          //fTokenizer[fIndex]^.Line,
          startLine,
          skEnum,
          GetScope,
          fClassScope,
          True,
          nil,
          False);
      end;
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';', '{', '}']);
  end;
  // Step over closing brace
  if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '}') then
    Inc(fIndex);
end;

function TCppParser.HandleStatement: boolean;
var
  S1, S2, S3: AnsiString;
  isStatic,isFriend: boolean;
  block : PStatement;
  idx,idx2,idx3:integer;
begin
  idx:=GetCurrentBlockEndSkip;
  idx2:=GetCurrentBlockBeginSkip;
  idx3:=GetCurrentInlineNamespaceEndSkip;
  if fIndex >= idx2 then begin
    fBlockBeginSkips.Delete(fBlockBeginSkips.Count-1);
    if fIndex = idx2 then
      inc(fIndex)
    else if (fIndex<fTokenizer.Tokens.Count) then  //error happens, but we must remove an (error) added scope
      RemoveScopeLevel(fTokenizer[fIndex]^.Line);
  end else if fIndex >= idx then begin
    fBlockEndSkips.Delete(fBlockEndSkips.Count-1);
    if (idx+1) < fTokenizer.Tokens.Count then
      RemoveScopeLevel(fTokenizer[idx+1]^.Line);
    if fIndex = idx then
      inc(fIndex);
  end else if fIndex >= idx3 then begin
    fInlineNamespaceEndSkips.Delete(fInlineNamespaceEndSkips.Count-1);
    if fIndex = idx3 then
      inc(fIndex);
  end else if (fTokenizer[fIndex]^.Text[1] = '{') then begin
    block := AddStatement(
        GetCurrentScope,
        fCurrentFile,
        '', // override hint
        '',
        '',
        '',
        '',
        //fTokenizer[fIndex]^.Line,
        fTokenizer[fIndex]^.Line,
        skBlock,
        GetScope,
        fClassScope,
        True,
        nil,
        False);
    AddSoloScopeLevel(block,fTokenizer[fIndex]^.Line);
    Inc(fIndex);
  end else if fTokenizer[fIndex]^.Text[1] = '}' then begin
    RemoveScopeLevel(fTokenizer[fIndex]^.Line);
    Inc(fIndex);
  end else if CheckForPreprocessor then begin
    HandlePreprocessor;
  end else if CheckForKeyword then begin // includes template now
    HandleKeyword;
  end else if CheckForForBlock then begin // (for/catch)
    HandleForBlock;
  end else if CheckForCatchBlock then begin // (for/catch)
    HandleCatchBlock;
  end else if CheckForScope then begin
    HandleScope;
  end else if CheckForEnum then begin
    HandleEnum;
  end else if CheckForTypedef then begin
    if fIndex+1 < fTokenizer.Tokens.Count then begin
      if CheckForTypedefStruct then begin // typedef struct something
        Inc(fIndex); // skip 'typedef'
        HandleStructs(True);
      end else if CheckForTypedefEnum then begin // typedef enum something
        Inc(fIndex); // skip 'typedef'
        HandleEnum;
      end else
        HandleOtherTypedefs; // typedef Foo Bar
    end else
      inc(fIndex);
  end else if CheckForNamespace then begin
    HandleNamespace;
  end else if CheckForUsing then begin
    HandleUsing;
  end else if CheckForStructs then begin
    HandleStructs(False);
  end else if CheckForMethod(S1, S2, S3, isStatic, isFriend) then begin
    HandleMethod(S1, S2, S3, isStatic, isFriend); // don't recalculate parts
  end else if CheckForVar then begin
    HandleVar;
  end else
    Inc(fIndex);

  CheckForSkipStatement;

  Result := fIndex < fTokenizer.Tokens.Count;
end;

procedure TCppParser.OnStartParsing;
begin
  PostMessage(fHandle,WM_PARSER_BEGIN_PARSE,0,0);
end;

procedure TCppParser.OnEndParsing(total:integer);
begin
  PostMessage(fHandle,WM_PARSER_END_PARSE,total,0);
end;

procedure TCppParser.OnBusy;
begin
  PostMessage(fHandle,WM_PARSER_BUSY,0,0);
end;

procedure TCppParser.OnUpdate;
begin
  PostMessage(fHandle,WM_PARSER_UPDATE,0,0);
end;


procedure TCppParser.InternalParse(const FileName: AnsiString; ManualUpdate: boolean = False; Stream: TMemoryStream =
  nil);
begin
  // Perform some validation before we start
  if not fEnabled then
    Exit;
  if not Assigned(Stream) and not (IsCfile(Filename) or IsHfile(Filename)) then // support only known C/C++ files
    Exit;
  if (fTokenizer = nil) or (fPreprocessor = nil) then
    Exit;

  // Start a timer here
  if (not ManualUpdate) then begin
    OnStartParsing;
  end;

  if not ManualUpdate then
    OnBusy;
  // Preprocess the file...
  try
    // Let the preprocessor augment the include records
    fPreprocessor.SetIncludesList(fIncludesList);
    fPreprocessor.SetIncludePaths(fIncludePaths);
    fPreprocessor.SetProjectIncludePaths(fProjectIncludePaths);
    fPreprocessor.SetScannedFileList(fScannedFiles);
    fPreprocessor.SetScanOptions(fParseGlobalHeaders, fParseLocalHeaders);
    if Assigned(Stream) then
      fPreprocessor.PreprocessStream(FileName, Stream)
    else
      fPreprocessor.PreprocessFile(FileName); // load contents from disk
  except
    if (not ManualUpdate) then begin
      OnEndParsing(-1);
    end;
    fPreprocessor.Reset; // remove buffers from memory
    Exit;
  end;

  {
  with TStringList.Create do try
    Text:=fPreprocessor.Result;
    SaveToFile('f:\\Preprocess.txt');
  finally
    Free;
  end;
  }  


  //fPreprocessor.DumpIncludesListTo('f:\\includes.txt');

  // Tokenize the preprocessed buffer file
  try
    fTokenizer.TokenizeBuffer(PAnsiChar(fPreprocessor.Result));
    if fTokenizer.Tokens.Count = 0 then begin
      fPreprocessor.Reset;
      fTokenizer.Reset;
      Exit;
    end;
  except
    if (not ManualUpdate) then
      OnEndParsing(-1);
    fPreprocessor.Reset;
    fTokenizer.Reset;
    Exit;
  end;

  // Process the token list
  fCurrentScope.Clear;
  fCurrentClassScope.Clear;
  fIndex := 0;
  fClassScope := scsNone;
  fSkipList.Clear;
  fBlockBeginSkips.Clear;
  fBlockEndSkips.Clear;
  fInlineNamespaceEndSkips.Clear;
  try
    repeat
    until not HandleStatement;
    //fTokenizer.DumpTokens('f:\tokens.txt');
    //Statements.DumpTo('f:\stats.txt');
    //Statements.DumpWithScope('f:\\statements.txt');
    //fPreprocessor.DumpDefinesTo('f:\defines.txt');
    //fPreprocessor.DumpIncludesListTo('f:\\includes.txt');
  finally
    //fSkipList:=-1; // remove data from memory, but reuse structures
    //fCurrentScope.Clear;
    //fCurrentClassScope.Clear;
    fPreprocessor.Reset;
    fTokenizer.Reset;
    if (not ManualUpdate) then begin
      OnEndParsing(1);
    end;
  end;

  if not ManualUpdate then
    OnUpdate;
end;

procedure TCppParser.Reset;
var
  I: integer;
  namespaceList:TList;
begin
  fCriticalSection.Acquire;
  try
    OnBusy;
  if Assigned(fPreprocessor) then
    fPreprocessor.Clear;
  fUniqId:=0;
  fParsing:=False;
  fSkipList.Clear;
  fBlockBeginSkips.Clear;
  fBlockEndSkips.Clear;
  fInlineNamespaceEndSkips.Clear;
  fLocked:=False;
  fParseLocalHeaders := False;
  fParseGlobalHeaders := False;

  //remove all macrodefines;
  //fMacroDefines.Clear;
  fCurrentScope.Clear;
  fCurrentClassScope.Clear;
  fProjectFiles.Clear;
  fFilesToScan.Clear;
  if Assigned(fTokenizer) then
    fTokenizer.Reset;

  // Remove all statements
  fStatementList.Clear;

  // We haven't scanned anything anymore
  fScannedFiles.Clear;

  // We don't include anything anymore
  for I := fIncludesList.Count - 1 downto 0 do begin
    PFileIncludes(fIncludesList.Objects[i])^.IncludeFiles.Free;
    PFileIncludes(fIncludesList.Objects[i])^.DirectIncludeFiles.Free;
    PFileIncludes(fIncludesList.Objects[i])^.Usings.Free;
    PFileIncludes(fIncludesList.Objects[i])^.Statements.Free;
    PFileIncludes(fIncludesList.Objects[i])^.StatementsIndex.Free;
    PFileIncludes(fIncludesList.Objects[i])^.DeclaredStatements.Free;
    PFileIncludes(fIncludesList.Objects[i])^.Scopes.Free;
    Dispose(PFileIncludes(fIncludesList.Objects[i]));
  end;
  fIncludesList.Clear;

  for i:=0 to fNamespaces.Count-1 do begin
    namespaceList := TList(fNamespaces.Objects[i]);
    namespaceList.Free;
  end;
  fNamespaces.Clear;

  for i:=0 to fRemovedStatements.Count-1 do begin
    dispose(PRemovedStatement(fRemovedStatements.Objects[i]));
  end;
  fRemovedStatements.Clear;

  fProjectIncludePaths.Clear;
  fIncludePaths.Clear;

  fProjectFiles.Clear;

  OnUpdate;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TCppParser.ParseFileList;
var
  I: integer;
begin
  fCriticalSection.Acquire;
  try
    if fParsing or fLocked then
      Exit;
    fParsing:=True;
  finally
    fCriticalSection.Release;
  end;
  try
    if not fEnabled then
      Exit;
    OnBusy;
    OnStartParsing;
    try
      // Support stopping of parsing when files closes unexpectedly
      fFilesScannedCount := 0;
      fFilesToScanCount := fFilesToScan.Count;
      //we only parse CFile in the first parse
      for i:=0 to fFilesToScan.Count-1 do begin
        if not IsCFile(fFilesToScan[i]) then
          continue;
        Inc(fFilesScannedCount); // progress is mentioned before scanning begins
        OnProgress(fCurrentFile,fFilesToScanCount,fFilesScannedCount);
        if FastIndexOf(fScannedFiles,fFilesToScan[i]) = -1 then begin
          InternalParse(fFilesToScan[i], True);
        end;
      end;
      // parse other files in the second parse
      for i:=0 to fFilesToScan.Count-1 do begin
        if IsCFile(fFilesToScan[i]) then
          continue;
        Inc(fFilesScannedCount); // progress is mentioned before scanning begins
        OnProgress(fCurrentFile,fFilesToScanCount,fFilesScannedCount);
        if FastIndexOf(fScannedFiles,fFilesToScan[i]) = -1 then begin
          InternalParse(fFilesToScan[i], True);
        end;
      end;
      fFilesToScan.Clear;
    finally
      OnEndParsing(fFilesScannedCount);
    end;
    OnUpdate;
  finally
    fParsing:=False;
  end;
end;

{
function TCppParser.GetSystemHeaderFileName(const FileName: AnsiString): AnsiString;
begin
  Result := cbutils.GetSystemHeaderFileName(FileName, fIncludePaths);
end;

function TCppParser.GetProjectHeaderFileName(const FileName: AnsiString): AnsiString;
begin
  Result := cbutils.GetSystemHeaderFileName(FileName, fIncludePaths);
end;

function TCppParser.GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString): AnsiString;
begin
  Result := cbutils.GetLocalHeaderFileName(RelativeTo, FileName);
end;
}



function TCppParser.GetHeaderFileName(const RelativeTo, Line: AnsiString): AnsiString;
begin
  fCriticalSection.Acquire;
  try
  Result := cbutils.GetHeaderFileName(RelativeTo, Line, fIncludePaths, fProjectIncludePaths);
  finally
    fCriticalSection.Release;
  end;  
end;

function TCppParser.IsIncludeLine(const Line: AnsiString): boolean;
begin
  fCriticalSection.Acquire;
  try
  Result := cbutils.IsIncludeLine(Line);
  finally
    fCriticalSection.Release;
  end;  
end;

function TCppParser.GetParsing:boolean;
begin
  Result:= fParsing or fLocked;
end;

procedure TCppParser.AddFileToScan(Value: AnsiString; InProject: boolean);
begin
  fCriticalSection.Acquire;
  try
  Value := StringReplace(Value, '/', '\', [rfReplaceAll]); // only accept full file names

  // Update project listing
  if InProject then
    if FastIndexOf(fProjectFiles,Value) = -1 then
      fProjectFiles.Add(Value);

  // Only parse given file
  if FastIndexOf(fFilesToScan,Value) = -1 then // check scheduled files
    if FastIndexOf(fScannedFiles,Value) = -1 then // check files already parsed
      fFilesToScan.Add(Value);
  finally
    fCriticalSection.Release;
  end;      
end;

procedure TCppParser.AddIncludePath(const Value: AnsiString);
var
  S: AnsiString;
begin
  fCriticalSection.Acquire;
  try
  S := AnsiDequotedStr(Value, '"');
  if FastIndexOf(fIncludePaths,S) = -1 then
    fIncludePaths.Add(S);
  finally
    fCriticalSection.Release;
  end;  
end;

procedure TCppParser.AddProjectIncludePath(const Value: AnsiString);
var
  S: AnsiString;
begin
  fCriticalSection.Acquire;
  try
  S := AnsiDequotedStr(Value, '"');
  if FastIndexOf(fProjectIncludePaths,S) = -1 then
    fProjectIncludePaths.Add(S);
  finally
    fCriticalSection.Release;
  end;    
end;

procedure TCppParser.ClearIncludePaths;
begin
  fCriticalSection.Acquire;
  try
    fIncludePaths.Clear;
  finally
    fCriticalSection.Release;
  end;    
end;

procedure TCppParser.ClearProjectIncludePaths;
begin
  fCriticalSection.Acquire;
  try
  fProjectIncludePaths.Clear;
  finally
    fCriticalSection.Release;
  end;  
end;

procedure TCppParser.ClearProjectFiles;
begin
  fCriticalSection.Acquire;
  try
  fProjectFiles.Clear;
  finally
    fCriticalSection.Release;
  end;
end;

{
procedure TCppParser.ResetDefines;
begin
  if Assigned(fPreprocessor) then
    fPreprocessor.ResetDefines;
end;
}

{
procedure TCppParser.AddHardDefineByParts(const Name, Args, Value: AnsiString);
begin
  if Assigned(fPreprocessor) then
    fPreprocessor.AddDefineByParts(Name, Args, Value, True);
end;
}

procedure TCppParser.ParseHardDefines;
var
  i:integer;
  define :PDefine;
  HintText:String;
begin
  fCriticalSection.Acquire;
  try
  if fParsing then
    Exit;
  fParsing:=True;
  try
    for i:=0 to fPreprocessor.HardDefines.Count-1 do begin
      define:=PDefine(fPreprocessor.HardDefines.Objects[i]);
      HintText := '#define';
      if define^.Name <> '' then
        HintText := HintText + ' ' + define^.Name;
      if define^.Args <> '' then
        HintText := HintText + ' ' + define^.Args;
      if define^.Value <> '' then
        HintText := HintText + ' ' + define^.Value;
      AddStatement(
        nil, // defines don't belong to any scope
        '',
        HintText, // override hint
        '', // define has no type
        define^.Name,
        define^.Value,
        define^.Args,
        -1,
        skPreprocessor,
        ssGlobal,
        scsNone,
        True,
        nil,
        False);
    end;
  finally
    fParsing:=False;
  end;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TCppParser.AddHardDefineByLine(const Line: AnsiString);
begin
  fCriticalSection.Acquire;
  try
  if Assigned(fPreprocessor) then begin
    if Pos('#', Line) = 1 then
      fPreprocessor.AddDefineByLine(TrimLeft(Copy(Line, 2, MaxInt)), True)
    else
      fPreprocessor.AddDefineByLine(Line, True);
  end;
  finally
    fCriticalSection.Release;
  end;  
end;

function TCppParser.IsSystemHeaderFile(const FileName: AnsiString): boolean;
begin
  fCriticalSection.Acquire;
  try
  Result := cbutils.IsSystemHeaderFile(FileName, fIncludePaths);
  finally
    fCriticalSection.Release;
  end;  
end;

function TCppParser.IsProjectHeaderFile(const FileName: AnsiString): boolean;
begin
  fCriticalSection.Acquire;
  try
  Result := cbutils.IsSystemHeaderFile(FileName, fProjectIncludePaths);
  finally
    fCriticalSection.Release;
  end;
end;

procedure TCppParser.ParseFile(const FileName: AnsiString; InProject: boolean; OnlyIfNotParsed: boolean = False; UpdateView:
  boolean = True; Stream: TMemoryStream = nil);
var
  FName: AnsiString;
  I: integer;
begin
  fCriticalSection.Acquire;
  try
    if fParsing or fLocked then
      Exit;
    fParsing:=True;
  finally
    fCriticalSection.Release;
  end;
  try
    if UpdateView then begin
      OnBusy;
    end;

    if not fEnabled then
      Exit;
    FName := FileName;

    if OnlyIfNotParsed and (FastIndexOf(fScannedFiles, FName) <> -1) then begin
      Exit;
    end;

    // Always invalidate file pairs. If we don't, reparsing the header
    // screws up the information inside the source file
    {
    GetSourcePair(FName, CFile, HFile);
    InvalidateFile(CFile);
    InvalidateFile(HFile);
    }
    InternalInvalidateFile(FileName);

    if InProject then begin
      fProjectFiles.Add(FileName);
      {
      if (CFile <> '') and (FastIndexOf(fProjectFiles,CFile) = -1) then
        fProjectFiles.Add(CFile);
      if (HFile <> '') and (FastIndexOf(fProjectFiles,HFile) = -1) then
        fProjectFiles.Add(HFile);
      }
    end else begin
      I := FastIndexOf(fProjectFiles,FileName);
      if I <> -1 then
        fProjectFiles.Delete(I);

    {
      I := FastIndexOf(fProjectFiles,CFile);
      if I <> -1 then
        fProjectFiles.Delete(I);
      I := FastIndexOf(fProjectFiles,HFile);
      if I <> -1 then
        fProjectFiles.Delete(I);
    }
    end;

    // Parse from disk or stream
    OnStartParsing;
    try
      fFilesToScanCount := 0;
      fFilesScannedCount := 0;
      if not Assigned(Stream) then begin
        {
        if CFile = '' then
          InternalParse(HFile, True) // headers should be parsed via include
        else
          InternalParse(CFile, True); // headers should be parsed via include
        }
        InternalParse(FileName);
      end else
        InternalParse(FileName, True, Stream); // or from stream
      for i:=0 to fRemovedStatements.Count-1 do begin
        dispose(PRemovedStatement(fRemovedStatements.Objects[i]));
      end;
      fRemovedStatements.Clear;
      fFilesToScan.Clear;
    finally
      OnEndParsing(1);
    end;
  finally
    if UpdateView  then begin
        OnUpdate;
    end;
    fParsing:=False;
  end;

end;

procedure TCppParser.InvalidateFile(const FileName: AnsiString);
begin
  fCriticalSection.Acquire;
  try
  if fParsing or fLocked then
    Exit;
  fParsing:=True;
  try
    InternalInvalidateFile(FileName);
  finally
    fParsing:=False;
  end;
  finally
    fCriticalSection.Release;
  end;  
end;

procedure TCppParser.InternalInvalidateFile(const FileName: AnsiString);
var
  I,j,idx: integer;
  P,fileIncludes: PFileIncludes;
//  Node, NextNode: PStatementNode;
  Statement: PStatement;
  namespaceList:TList;
  removedStatement: PRemovedStatement;
  key:string;
begin
  if Filename = '' then
    Exit;

  i:=0;
  while (i<fNamespaces.Count) do begin
    namespaceList := TList(fNamespaces.Objects[i]);
    j:=0;
    while (j<namespaceList.Count) do begin
      Statement:=PStatement(namespaceList[j]);
      if SameText(Statement^._FileName, FileName) or SameText(Statement^._DefinitionFileName, FileName) then begin
        namespaceList.Delete(j)
      end else
        inc(j);
    end;
    if namespaceList.Count = 0 then begin
      namespaceList.Free;
      fNamespaces.Delete(i);
    end else
      inc(i);
  end;

  // delete it from scannedfiles
  I := FastIndexOf(fScannedFiles,FileName);
  if I <> -1 then
    fScannedFiles.Delete(I);

  // remove its include files list
  P := FindFileIncludes(FileName, True);
  if Assigned(P) then begin
    //fPreprocessor.InvalidDefinesInFile(FileName); //we don't need this, since we reset defines after each parse
    P^.IncludeFiles.Free;
    P^.DirectIncludeFiles.Free;
    P^.Usings.Free;
    for i:=0 to P^.DeclaredStatements.Count-1 do begin
      Statement:= P^.DeclaredStatements[i];
      fileIncludes := nil;
      if not SameText(Statement^._FileName,FileName) then begin
        fileIncludes := FindFileIncludes(Statement^._FileName);
      end else if not SameText(Statement^._DefinitionFileName,FileName) then begin
        fileIncludes := FindFileIncludes(Statement^._DefinitionFileName);
        new(removedStatement);
        removedStatement^._Type := Statement^._Type;
        removedStatement^._Command := Statement^._Command;
        removedStatement^._DefinitionLine := Statement^._DefinitionLine;
        removedStatement^._DefinitionFileName := Statement^._DefinitionFileName;
        removedStatement^._FullName := Statement^._FullName;
        removedStatement^._NoNameArgs := Statement^._NoNameArgs;
        key:=getStatementKey(removedStatement^._FullName,
          removedStatement^._Type,
          removedStatement^._NoNameArgs);
        fRemovedStatements.AddObject(key,TObject(removedStatement));
      end;
      if Assigned(fileIncludes) then begin
        idx:=fileIncludes.StatementsIndex.ValueOf(IntToStr(integer(statement)));
        if idx>=0 then
          fileIncludes.Statements[idx]:=nil;
      end;
      fStatementList.DeleteStatement(Statement);
    end;
    PFileIncludes(P)^.DeclaredStatements.Free;
    PFileIncludes(P)^.Statements.Free;
    PFileIncludes(P)^.StatementsIndex.Free;
    PFileIncludes(P)^.Scopes.Free;
    Dispose(PFileIncludes(P));
  end;

  //Statements.DumpTo('f:\\after.txt');
end;


function TCppParser.SuggestMemberInsertionLine(ParentStatement: PStatement; Scope: TStatementClassScope; var
  AddScopeStr: boolean): integer;
var
  //Node: PStatementNode;
  Statement: PStatement;
  maxInScope: integer;
  maxInGeneral: integer;
  i:integer;
  children: TList;
begin
  fCriticalSection.Acquire;
  try
  // this function searches in the statements list for statements with
  // a specific _ParentID, and returns the suggested line in file for insertion
  // of a new var/method of the specified class scope. The good thing is that
  // if there is no var/method by that scope, it still returns the suggested
  // line for insertion (the last line in the class).
  maxInScope := -1;
  maxInGeneral := -1;
  children := Statements.GetChildrenStatements(ParentStatement);
  if Assigned(children) then begin
    for i:=0 to children.Count-1 do
    begin
      Statement := PStatement(children[i]);
      if Statement^._HasDefinition then begin
        if Statement^._Line > maxInGeneral then
          maxInGeneral := Statement^._Line;
        if Statement^._ClassScope = scope then
          if Statement^._Line > maxInScope then
            maxInScope := Statement^._Line;
      end else begin
        if Statement^._DefinitionLine > maxInGeneral then
          maxInGeneral := Statement^._Line;
        if Statement^._ClassScope = scope then
          if Statement^._DefinitionLine > maxInScope then
            maxInScope := Statement^._DefinitionLine;
      end;
    end;
  end;

  if maxInScope = -1 then begin
    AddScopeStr := True;
    Result := maxInGeneral;
  end else begin
    AddScopeStr := False;
    Result := maxInScope;
  end;
  finally
    fCriticalSection.Release;
  end;  
end;

procedure TCppParser.GetClassesList(var List: TStringList);
var
  Node: PStatementNode;
  Statement: PStatement;
begin
  fCriticalSection.Acquire;
  try
  if fParsing then
    Exit;
  // fills List with a list of all the known classes
  List.Clear;
  Node := fStatementList.LastNode;
  while Assigned(Node) do begin
    Statement := Node^.Data;
    if Statement^._Kind = skClass then
      List.AddObject(Statement^._Command, Pointer(Statement));
    Node := Node^.PrevNode;
  end;
  finally
    fCriticalSection.Release;
  end;  
end;

function TCppParser.FindAndScanBlockAt(const Filename: AnsiString; Line: integer): PStatement;
var
  fileIncludes: PFileIncludes;
  idx,i:integer;
  statement:PStatement;
begin
  fCriticalSection.Acquire;
  try
  Result := nil;
  if fParsing then
    Exit;
  fileIncludes := FindFileIncludes(FileName);
  if not Assigned(fileIncludes) then
    Exit;

  {
  with TStringList.Create do try
    for idx:=0 to fileIncludes.Scopes.Count-1 do begin
      statement:=PStatement(fileIncludes.Scopes.Objects[idx]);
      if assigned(statement) then
        Add(IntToStr(fileIncludes.Scopes[idx])+' : '+statement^._Command)
      else
        Add(IntToStr(fileIncludes.Scopes[idx])+' : ');
    end;
    SaveToFile('f:\scopes.txt');
  finally
    Free;
  end;
  }


  fileIncludes.Scopes.Find(Line,idx);
  if idx>=fileIncludes.Scopes.Count then begin
    for i:=fCurrentScope.Count-1 downto 0 do begin
      statement := PStatement(fCurrentScope[i]);
      if (not assigned(statement)) or (Line >= statement^._definitionLine) then begin
        Result:=statement;
        break;
      end;
    end;
    Exit;
  end;
  while (idx>=0) and (Line < fileIncludes.Scopes[idx]) do
    dec(idx);
  if idx<0 then
    Exit;
  Result:= PStatement(fileIncludes.Scopes.Objects[idx]);
  finally
    fCriticalSection.Release;
  end;  
end;

function TCppParser.GetClass(const Phrase: AnsiString): AnsiString;
var
  I, FirstOp: integer;
begin
  // Obtain stuff before first operator
  FirstOp := Length(Phrase) + 1;
  for I := 1 to Length(Phrase) - 1 do begin
    if (phrase[i] = '-') and (Phrase[i + 1] = '>') then begin
      FirstOp := I;
      break;
    end else if (Phrase[i] = ':') and (Phrase[i + 1] = ':') then begin
      FirstOp := I;
      break;
    end else if (Phrase[i] = '.') then begin
      FirstOp := I;
      break;
    end;
  end;

  Result := Copy(Phrase, 1, FirstOp - 1);
end;

function TCppParser.GetRemainder(const Phrase: AnsiString): AnsiString;
var
  FirstOp, I: integer;
begin
  // Obtain stuff after first operator
  FirstOp := 0;
  for I := 1 to Length(Phrase) - 1 do begin
    if (phrase[i] = '-') and (Phrase[i + 1] = '>') then begin
      FirstOp := I + 2;
      break;
    end else if (Phrase[i] = ':') and (Phrase[i + 1] = ':') then begin
      FirstOp := I + 2;
      break;
    end else if (Phrase[i] = '.') then begin
      FirstOp := I + 1;
      break;
    end;
  end;

  if FirstOp = 0 then begin
    Result := '';
    Exit;
  end;

  Result := Copy(Phrase, FirstOp, MaxInt);
end;


function TCppParser.GetMember(const Phrase: AnsiString): AnsiString;
var
  FirstOp, SecondOp, I: integer;
begin
  // Obtain stuff after first operator
  FirstOp := 0;
  for I := 1 to Length(Phrase) - 1 do begin
    if (phrase[i] = '-') and (Phrase[i + 1] = '>') then begin
      FirstOp := I + 2;
      break;
    end else if (Phrase[i] = ':') and (Phrase[i + 1] = ':') then begin
      FirstOp := I + 2;
      break;
    end else if (Phrase[i] = '.') then begin
      FirstOp := I + 1;
      break;
    end;
  end;

  if FirstOp = 0 then begin
    Result := '';
    Exit;
  end;

  // ... and before second op, if there is one
  SecondOp := 0;
  for I := firstop to Length(Phrase) - 1 do begin
    if (phrase[i] = '-') and (Phrase[i + 1] = '>') then begin
      SecondOp := I;
      break;
    end else if (Phrase[i] = ':') and (Phrase[i + 1] = ':') then begin
      SecondOp := I;
      break;
    end else if (Phrase[i] = '.') then begin
      SecondOp := I;
      break;
    end;
  end;

  if SecondOp = 0 then
    Result := Copy(Phrase, FirstOp, MaxInt)
  else
    Result := Copy(Phrase, FirstOp, SecondOp - FirstOp);
end;

function TCppParser.GetOperator(const Phrase: AnsiString): AnsiString;
var
  I: integer;
begin
  Result := '';
  // Find first operator
  for I := 1 to Length(Phrase) - 1 do begin
    if (phrase[i] = '-') and (Phrase[i + 1] = '>') then begin
      Result := '->';
      break;
    end else if (Phrase[i] = ':') and (Phrase[i + 1] = ':') then begin
      Result := '::';
      break;
    end else if (Phrase[i] = '.') then begin
      Result := '.';
      break;
    end;
  end;
end;

function TCppParser.FindNamespace(const name:AnsiString):TList; // return a list of PSTATEMENTS (of the namespace)
var
  i:integer;
begin
  fCriticalSection.Acquire;
  try
  i:=FastIndexOf(fNamespaces,name);
  if i = -1 then
    Result:=nil
  else
    Result:=TList(fNamespaces.objects[i]);
  finally
    fCriticalSection.Release;
  end;    
end;


function TCppParser.FindLastOperator(const Phrase: AnsiString): integer;
var
  I: integer;
begin
  I := Length(phrase);

  // Obtain stuff after first operator
  while I > 0 do begin
    if (phrase[i + 1] = '>') and (phrase[i] = '-') then begin
      Result := i;
      Exit;
    end else if (phrase[i + 1] = ':') and (phrase[i] = ':') then begin
      Result := i;
      Exit;
    end else if (phrase[i] = '.') then begin
      Result := i;
      Exit;
    end;
    Dec(i);
  end;
  Result := 0;
end;

function TCppParser.PrettyPrintStatement(Statement: PStatement;line:integer): AnsiString;

  function GetScopePrefix: AnsiString;
  var
    ScopeStr: AnsiString;
  begin
    ScopeStr := StatementClassScopeStr(Statement^._ClassScope); // can be blank
    if ScopeStr <> '' then
      Result := ScopeStr + ' '
    else
      Result := '';
  end;
  function GetParentPrefix: AnsiString;
  var
    WalkStatement: PStatement;
  begin
    Result := '';
    WalkStatement := Statement;
    while Assigned(WalkStatement^._ParentScope) do begin
      if not (WalkStatement^._ParentScope^._Kind in [skBlock,skFunction]) then
        Result := WalkStatement^._ParentScope^._Command + '::' + Result;
      WalkStatement := WalkStatement^._ParentScope;
    end;
  end;
  function GetArgsSuffix: AnsiString;
  begin
    if Statement^._Args <> '' then
      Result := ' ' + Statement^._Args
    else
      Result := '';
  end;
begin
  fCriticalSection.Acquire;
  try
  if fParsing then
    Exit; 
  Result := '';
  if Statement^._HintText <> '' then begin
    if Statement^._Kind <> skPreprocessor then begin
      Result := Statement^._HintText;
    end else if SameStr(Statement^._Command,'__FILE__') then begin
      Result := '"'+fCurrentFile+'"';
    end else if SameStr(Statement^._Command,'__LINE__') then begin
      Result := '"'+IntToStr(line)+'"';
    end else if SameStr(Statement^._Command,'__DATE__') then begin
      Result := '"'+DateToStr(Now)+'"';
    end else if SameStr(Statement^._Command,'__TIME__') then begin
      Result := '"'+TimeToStr(Now)+'"';
    end else begin
      Result := Statement^._HintText;
    end;
  end else begin
    case Statement^._Kind of
      skFunction,
        skVariable,
        skParameter,
        skClass: begin
          if Statement^._Scope <> ssLocal then
            Result := GetScopePrefix; // public
          Result := Result + Statement^._Type + ' '; // void
          if Statement^._Scope <> ssLocal then
            Result := Result + GetParentPrefix; // A::B::C::
          Result := Result + Statement^._Command; // Bar
          Result := Result + GetArgsSuffix; // (int a)
        end;
      skNamespace: begin
          Result := Result + Statement^._Command; // Bar
        end;
      skConstructor: begin
          Result := GetScopePrefix; // public
          Result := Result + 'constructor' + ' '; // constructor
          Result := Result + GetParentPrefix; // A::B::C::
          Result := Result + Statement^._Command; // Bar
          Result := Result + GetArgsSuffix; // (int a)
        end;
      skDestructor: begin
          Result := GetScopePrefix; // public
          Result := Result + 'destructor' + ' '; // destructor
          Result := Result + GetParentPrefix; // A::B::C::
          Result := Result + Statement^._Command; // Bar
          Result := Result + GetArgsSuffix; // (int a)
        end;
      skTypedef: begin
          Result := 'skTypedef hint'; // should be set by HintText
        end;
      skEnum, skEnumType: begin
          Result := 'skEnum hint'; // should be set by HintText
        end;
      skPreprocessor: begin
          Result := 'skPreprocessor hint'; // should be set by HintText
        end;
      skUnknown: begin
          Result := 'skUnknown hint'; // should be set by HintText
        end;
    end;
  end;
  finally
    fCriticalSection.Release;
  end;  
end;

procedure TCppParser.FillListOfFunctions(const FileName, Phrase: AnsiString; const Line: integer; List: TStringList);
var
  Statement,namespaceStatement: PStatement;
  k:integer;
  namespaceStatementsList:TList;

  procedure FillList(statement, scopeStatement:PStatement);
  var
    childStatement:PStatement;
    children: TList;
    i:integer;
  begin
    children := Statements.GetChildrenStatements(scopeStatement);
    if not Assigned(children) then
      Exit;
    for i:=0 to children.Count-1 do begin
      childStatement:=PStatement(children[i]);
      if samestr(Statement^._Command,childStatement^._Command)
        or samestr(Statement^._Command + 'A',childStatement^._Command)
        or samestr(Statement^._Command + 'W',childStatement^._Command) then begin
          List.Add(PrettyPrintStatement(childStatement));
      end;
    end;
  end;
begin
  fCriticalSection.Acquire;
  try
  List.Clear;
  Statement := self.FindStatementOf(FileName,Phrase, Line);
  if not Assigned(statement) then
    Exit;
  if Assigned(statement^._ParentScope) and (statement^._ParentScope^._Kind = skNamespace) then begin
    namespaceStatementsList:=FindNamespace(statement^._ParentScope^._Command);
    if Assigned(namespaceStatementsList) then begin
        for k:=0 to namespaceStatementsList.Count-1 do begin
          namespaceStatement:=PStatement(namespaceStatementsList[k]);
          if Assigned(namespaceStatement) then
            FillList(statement, namespaceStatement);
        end;
      end;
  end else
    FillList(statement, statement^._ParentScope);
  finally
    fCriticalSection.Release;
  end;
end;

function TCppParser.FindFirstTemplateParamOf(const FileName: AnsiString;const aPhrase: AnsiString; currentClass: PStatement): String;
var
  //Node: PStatementNode;
  Statement: PStatement;
  position,i,t: integer;
  s: AnsiString;
//  Children: TList;
  scopeStatement:PStatement;

  function GetTemplateParam(statement:PStatement):String;
  begin
    Result:='';
    if not Assigned(Statement) then
      Exit;
    if not (Statement^._Kind = skTypedef) then
      Exit;
    if SameStr(aPhrase, Statement^._Type) then // prevent infinite loop
      Exit;
    Result := FindFirstTemplateParamOf(FileName,Statement^._Type, CurrentClass)
  end;

  function getBracketEnd(s:AnsiString;startAt:integer):integer;
  var
    I, Level: integer;
  begin
    I := StartAt;
    Level := 0; // assume we start on top of [
    while (I <= length(s)) do begin
      case s[i] of
      '<': Inc(Level);
      ',': begin
          if Level = 1 then begin
            Result := I;
            Exit;
          end;
        end;
      '>': begin
          Dec(Level);
          if Level = 0 then begin
            Result := I;
            Exit;
          end;
        end;
      end;
      Inc(I);
    end;
    Result := StartAt;
  end;
begin
  fCriticalSection.Acquire;
  try
  Result := '';
  if fParsing then
    Exit;
  // Remove pointer stuff from type
  s := aPhrase; // 'Type' is a keyword
  i:=Pos('<',s);
  if i>0 then begin
    t:=getBracketEnd(s,i);
    Result := Copy(s,i+1,t-i-1);
    Exit;
  end;
  position := Length(s);
  while (position > 0) and (s[position] in ['*',' ','&']) do
    Dec(position);
  if position <> Length(s) then
    Delete(s, position + 1, Length(s) - 1);

  scopeStatement:= currentClass;

  Statement :=FindStatementOf(FileName,s,currentClass);
  Result := GetTemplateParam(Statement);
  finally
    fCriticalSection.Release;
  end;  
end;

function TCppParser.FindFunctionDoc(const FileName:AnsiString; const Line: integer;
      params:TStringList; var isVoid:boolean): AnsiString;
var
  fileIncludes: PFileIncludes;
  i:integer;
  statement,funcStatement: PStatement;
  children : TList;
begin
  fCriticalSection.Acquire;
  try
    Result := '';
    fileIncludes:= self.FindFileIncludes(fileName);
    if not assigned(fileIncludes) then
      Exit;
    funcStatement:=nil;
    for i:=0 to fileIncludes.Statements.Count-1 do begin
      statement:= PStatement(fileIncludes.Statements[i]);
      if not assigned(statement) then
        continue;
      if (statement^._Kind in [skFunction,skConstructor,skDestructor])
        and (
          (statement^._Line = Line)
          or (statement^._DefinitionLine = Line)
        ) then begin
        funcStatement := statement;
        break;
      end;
    end;
    if not Assigned(funcStatement) then
      Exit;
    Result := funcStatement^._FullName;
    isVoid := SameStr(trim(funcStatement^._Type), 'void');
    children:= funcStatement^._Children;
    params.Clear;
    if assigned(children) then begin
      for i:=0 to children.Count-1 do begin
        statement:=PStatement(children[i]);
        if not assigned(statement) then
          continue;
        if statement^._Kind = skParameter then begin
          params.Add(statement^._Command);
        end;
      end;
    end;
  finally
    fCriticalSection.Release;
  end;
end;


function TCppParser.FindTypeDefinitionOf(const FileName: AnsiString;const aType: AnsiString; currentClass: PStatement): PStatement;
var
  //Node: PStatementNode;
  Statement: PStatement;
  position, endPos: integer;
  s: AnsiString;
//  Children: TList;
  scopeStatement:PStatement;

  function GetTypeDef(statement:PStatement):PStatement;
  begin
    if not Assigned(Statement) then begin
      Result:=nil;
      Exit;
    end;
    if Statement^._Kind = skClass then begin
      Result:=statement;
    end else if Statement^._Kind = skTypedef then begin
      if not SameStr(aType, Statement^._Type) then // prevent infinite loop
        Result := FindTypeDefinitionOf(FileName,Statement^._Type, Statement^._ParentScope)
      else
        Result := Statement; // stop walking the trail here
      if Result = nil then // found end of typedef trail, return result
        Result := Statement;
    end else
      Result := nil;
  end;

  function getBracketEnd(s:AnsiString;startAt:integer):integer;
  var
    I, Level: integer;
  begin
    I := StartAt;
    Level := 0; // assume we start on top of [
    while (I <= length(s)) do begin
      case s[i] of
      '<': Inc(Level);
      '>': begin
          Dec(Level);
          if Level = 0 then begin
            Result := I;
            Exit;
          end;
        end;
      end;
      Inc(I);
    end;
    Result := StartAt;
  end;
begin
  fCriticalSection.Acquire;
  try
  Result := nil;
  if fParsing then
    Exit;
  // Remove pointer stuff from type
  s := aType; // 'Type' is a keyword
  position := Length(s);
  while (position > 0) and (s[position] in ['*',' ','&']) do
    Dec(position);
  if position <> Length(s) then
    Delete(s, position + 1, Length(s) - 1);

  // Strip template stuff
  position := Pos('<', s);
  if position > 0 then begin
    endPos := getBracketEnd(s,position);
    Delete(s, position, endPos-position+1);
  end;

  // Use last word only (strip 'const', 'static', etc)
  position := LastPos(' ', s);
  if position > 0 then
    Delete(s, 1, position);

  scopeStatement:= currentClass;

  Statement :=FindStatementOf(FileName,s,currentClass);
  Result := GetTypeDef(Statement);
  finally
    fCriticalSection.Release;
  end;
end;

function TCppParser.FindMemberOfStatement(const Phrase: AnsiString; ScopeStatement: PStatement):PStatement;
var
  ChildrenIndex:TStringHash;
  i,p:integer;
  s:string;
begin
  Result := nil;
  ChildrenIndex := Statements.GetChildrenStatementIndex(ScopeStatement);
  if not Assigned(ChildrenIndex) then
    Exit;

  //remove []
  p:=Pos('[',Phrase);
  if p>0 then
    s:=Copy(Phrase,1,p-1)
  else
    s:=Phrase;

  //remove <>
  p:=Pos('<',s);
  if p>0 then
    s:=Copy(s,1,p-1);
    
  i:=ChildrenIndex.ValueOf(s);
  if i<>-1 then
    Result := PStatement(i);

end;

// find allStatment
function TCppParser.FindStatementStartingFrom(const FileName, Phrase: AnsiString; startScope: PStatement; force:boolean): PStatement;
var
//  Statement: PStatement;
  namespaceStatement,scopeStatement: PStatement;
//  Children:TList;
  namespaceStatementsList :TList;
  t,k:integer;
  namespacename: AnsiString;
  FileUsings: TDevStringList;
begin
  fCriticalSection.Acquire;
  try
  Result := nil;
  if fParsing and not force then
    Exit;

  scopeStatement := startScope;

  // repeat until reach global
  while Assigned(scopeStatement) do begin
    //search members of current scope
    Result:=FindMemberOfStatement(Phrase,scopeStatement);
    if Assigned(Result) then
      Exit;
    if (scopeStatement^._Kind = skNamespace) then begin
      namespaceStatementsList:=FindNamespace(scopeStatement^._Command);
      if Assigned(namespaceStatementsList) then begin
        for k:=0 to namespaceStatementsList.Count-1 do begin
          namespaceStatement:=PStatement(namespaceStatementsList[k]);
          Result:=FindMemberOfStatement(Phrase,namespaceStatement);
          if Assigned(Result) then
            Exit;
        end;
      end;
    end;
    // search members of all usings (in current scope )
    for t:=0 to scopeStatement^._Usings.Count-1 do begin
      namespaceName := scopeStatement^._Usings[t];
      namespaceStatementsList:=FindNamespace(namespaceName);
      if not Assigned(namespaceStatementsList) then
        continue;
      for k:=0 to namespaceStatementsList.Count-1 do begin
        namespaceStatement:=PStatement(namespaceStatementsList[k]);
        Result:=FindMemberOfStatement(Phrase,namespaceStatement);
        if Assigned(Result) then
          Exit;
      end;
    end;
    scopeStatement:=scopeStatement^._ParentScope;
  end;

  // Search all global members
  Result:=FindMemberOfStatement(Phrase,nil);
  if Assigned(Result) then
    Exit;
  Result := nil;

  //FindFileUsings
  FileUsings:=TDevStringList.Create;
  try
    GetFileUsings(FileName,FileUsings);
    // add members of all fusings
    for t:=0 to FileUsings.Count-1 do begin
      namespaceName := FileUsings[t];
      namespaceStatementsList:=FindNamespace(namespaceName);
      if not Assigned(namespaceStatementsList) then
        continue;
      for k:=0 to namespaceStatementsList.Count-1 do begin
        namespaceStatement:=PStatement(namespaceStatementsList[k]);
        Result:=FindMemberOfStatement(Phrase,namespaceStatement);
        if Assigned(Result) then
          Exit;
      end;
    end;
  finally
    FileUsings.Free;
  end;
  finally
    fCriticalSection.Release;
  end;  
end;

function TCppParser.FindStatementOf(FileName, Phrase: AnsiString; CurrentClass: PStatement; force:boolean = False): PStatement;
var
  StatementParentType:PStatement;
begin
  fCriticalSection.Acquire;
  try
  Result:=FindStatementOf(FileName,Phrase,CurrentClass,StatementParentType,force);
  finally
    fCriticalSection.Release;
  end;
end;

function TCppParser.FindStatementOf(FileName, Phrase: AnsiString;
  CurrentClass: PStatement;var CurrentClassType : PStatement ; force:boolean): PStatement;
var
  //Node: PStatementNode;
  OperatorToken, typeName: AnsiString;
  LastScopeStatement,currentNamespace, Statement, MemberStatement, TypeStatement: PStatement;
  i,idx: integer;
  namespaceName, NextScopeWord, memberName,remainder : AnsiString;
  namespaceList:TList;
begin
  fCriticalSection.Acquire;
  try
  Result := nil;
  CurrentClassType := CurrentClass;
  if fParsing and not force then
    Exit;


  getFullNamespace(Phrase, namespaceName, remainder);
  if namespaceName <> '' then begin  // (namespace )qualified Name
    idx:=FastIndexOf(fNamespaces,namespaceName) ;
    namespaceList := TList(fNamespaces.Objects[idx]);

    if remainder = '' then begin
      Result := namespaceList[0];
      Exit;
    end;

    NextScopeWord := GetClass(remainder);
    OperatorToken := GetOperator(remainder);
    MemberName := GetMember(remainder);
    remainder := GetRemainder(remainder);
    statement:=nil;
    for i:=0 to namespaceList.Count-1 do begin
      currentNamespace:=PStatement(namespaceList[i]);
      statement:=findMemberOfStatement(NextScopeWord,currentNamespace);
      if assigned(statement) then
        break;
    end;

    //not found in namespaces;
    if not assigned(statement) then
      Exit;
    // found in namespace
  end else if (Length(Phrase)>2) and (Phrase[1]=':') and (Phrase[2]=':') then begin
    //global
    remainder:=Copy(Phrase,3,MAXINT);
    NextScopeWord := GetClass(remainder);
    OperatorToken := GetOperator(remainder);
    MemberName := GetMember(remainder);
    remainder := GetRemainder(remainder);
    statement:=findMemberOfStatement(NextScopeWord,nil);
    if not assigned(statement) then
      Exit;
  end else begin   //unqualified name
    CurrentClassType := CurrentClass;
    {
    while assigned(CurrentClassType) and not (CurrentClassType._Kind in [skClass,skNamespace]) do begin
      CurrentClassType:=CurrentClassType^._ParentScope;
    end;
    }
    NextScopeWord := GetClass(remainder);
    OperatorToken := GetOperator(remainder);
    MemberName := GetMember(remainder);
    remainder := GetRemainder(remainder);
    if assigned(CurrentClass) and (CurrentClass^._Kind = skNamespace)  then begin
      idx:=FastIndexOf(fNamespaces,CurrentClass^._Command);
      if idx>=0 then begin
        namespaceList := TList(fNamespaces.Objects[idx]);
        statement:=nil;
        for i:=0 to namespaceList.Count-1 do begin
          currentNamespace:=PStatement(namespaceList[i]);
          statement:=findMemberOfStatement(NextScopeWord,currentNamespace);
          if assigned(statement) then
            break;
        end;
      end else begin
        statement := FindStatementStartingFrom(FileName,nextScopeWord,currentClassType,force);
      end;
    end else begin
      statement := FindStatementStartingFrom(FileName,nextScopeWord,currentClassType,force);
    end;
    if not Assigned(statement) then
      Exit;
  end;
  CurrentClassType := CurrentClass;

  if (MemberName <> '') and (statement._Kind in [skTypedef]) then begin
    TypeStatement := FindTypeDefinitionOf(FileName,statement^._Type, CurrentClassType);
    if Assigned(TypeStatement) then
      Statement := TypeStatement;
  end;

  if (statement._Kind = skConstructor) then begin // we need the class, not the construtor
    statement:=statement^._ParentScope;
    if not assigned(statement) then
      Exit;
  end;

  LastScopeStatement:=nil;
  while MemberName <> '' do begin
    if statement._Kind in [skVariable,skParameter,skFunction] then begin
      if (statement^._Kind = skFunction)
          and assigned(statement^._ParentScope)
          and  (STLContainers.ValueOf(statement^._ParentScope^._FullName)>0)
          and (STLElementMethods.ValueOf(statement^._Command)>0)
          and assigned(LastScopeStatement) then begin
        typeName:=self.FindFirstTemplateParamOf(FileName,LastScopeStatement^._Type,LastScopeStatement^._ParentScope);
        TypeStatement:=FindTypeDefinitionOf(FileName, typeName,LastScopeStatement^._ParentScope);
      end else
        TypeStatement := FindTypeDefinitionOf(FileName,statement^._Type, CurrentClassType);

      if assigned(TypeStatement) and (
        STLPointers.Valueof(TypeStatement^._FullName)>=0)
         and SameStr(OperatorToken,'->') then begin
        typeName:=self.FindFirstTemplateParamOf(FileName,Statement^._Type,statement^._ParentScope);
        TypeStatement:=FindTypeDefinitionOf(FileName, typeName,statement^._ParentScope);
      end else if assigned(TypeStatement) and (STLContainers.ValueOf(TypeStatement^._FullName)>0)
          and EndsStr(']',NextScopeWord) then begin
        typeName:=self.FindFirstTemplateParamOf(FileName,Statement^._Type,statement^._ParentScope);
        TypeStatement:=FindTypeDefinitionOf(FileName, typeName,statement^._ParentScope);
      end;
      lastScopeStatement:= statement;
      if Assigned(TypeStatement) then
        Statement := TypeStatement;
    end else
      lastScopeStatement:= statement;
    NextScopeWord := GetClass(remainder);
    OperatorToken := GetOperator(remainder);
    MemberName := GetMember(remainder);
    remainder := GetRemainder(remainder);
    MemberStatement := FindMemberOfStatement(NextScopeWord,statement);
    if not Assigned(MemberStatement) then begin;
      Exit;
    end;

    CurrentClassType:=statement;
    Statement:=MemberStatement;
    if (MemberName <> '') and (statement._Kind in [skTypedef]) then begin
      TypeStatement := FindTypeDefinitionOf(FileName,statement^._Type, CurrentClassType);
      if Assigned(TypeStatement) then
        Statement := TypeStatement;
    end;
  end;
  Result := Statement;

  finally
    fCriticalSection.Release;
  end;
end;

function TCppParser.FindStatementOf(FileName, Phrase: AnsiString; Line: integer): PStatement;
begin
  fCriticalSection.Acquire;
  try
  Result := FindStatementOf(FileName, Phrase,FindAndScanBlockAt(FileName, Line));
  //Statements.DumpWithScope('f:\\local-statements.txt');
  finally
    fCriticalSection.Release;
  end;
end;

function TCppParser.GetHintFromStatement(FileName, Phrase: AnsiString; Line: integer):AnsiString;
var
  hint:AnsiString;
  k:integer;
  namespaceStatementsList:TList;
  namespaceStatement:PStatement;
  st: PStatement;

  function GetHintForFunction(statement,ScopeStatement:PStatement):String;
  var
    children:TList;
    childStatement:PStatement;
    i:integer;
  begin
    Result := '';
    children := Statements.GetChildrenStatements(ScopeStatement);
    if not assigned(children) then
      Exit;
    for i:=0 to children.Count-1 do begin
      childStatement:=PStatement(children[i]);
      if samestr(st^._Command,childStatement^._Command)
        and (childStatement^._Kind in [skFunction,skConstructor,skDestructor]) then begin
          if Result <> '' then
            Result:=Result+#13;
          Result := Result + PrettyPrintStatement(childStatement)
            + ' - ' + ExtractFileName(childStatement^._FileName)
            + ' ('  + IntToStr(childStatement^._Line) + ')';
      end;
    end;
  end;
begin
  fCriticalSection.Acquire;
  try
    Result:='';
    st := FindStatementOf(fileName,phrase,line);
    if not Assigned(st) then
      Exit;
    if st^._Kind in [skFunction,skConstructor,skDestructor] then begin
      if Assigned(st^._ParentScope) and (st^._ParentScope^._Kind = skNamespace) then begin
        namespaceStatementsList:=FindNamespace(st^._ParentScope^._Command);
        if Assigned(namespaceStatementsList) then begin
          for k:=0 to namespaceStatementsList.Count-1 do begin
            namespaceStatement:=PStatement(namespaceStatementsList[k]);
            if Assigned(namespaceStatement) then begin
              hint := GetHintForFunction(st,namespaceStatement);
              if hint <> '' then begin
                if Result <> '' then
                  Result :=Result+#13;
                Result := Result + hint;
              end;
            end;
          end;
        end;
      end else
        Result:=GetHintForFunction(st,st^._ParentScope);
    end else if st^._Line>0 then begin
      Result := PrettyPrintStatement(st) + ' - ' + ExtractFileName(st^._FileName) + ' (' +
        IntToStr(st^._Line) + ') - Ctrl+Click for more info';
    end else begin  // hard defines
      Result := PrettyPrintStatement(st,Line);
    end;
    Result := StringReplace(Result, '|', #5, [rfReplaceAll]);
  finally
    fCriticalSection.Release;
  end;
end;

function TCppParser.FindKindOfStatementOf(FileName, Phrase: AnsiString; Line: integer): TStatementKind;
var
  st:PStatement;
begin
  fCriticalSection.Acquire;
  try
    Result:=skUnknown;
    st := FindStatementOf(FileName, Phrase,Line);
    if assigned(st) then begin
      if st^._Kind = skVariable then begin
        if not Assigned(st^._ParentScope) then begin
          Result:=skGlobalVariable;
        end else if st^._Scope = ssLocal then begin
          Result:=skLocalVariable;
        end else
          Result:=skVariable;
      end else
        Result := st^._Kind;
    end;
  finally
    fCriticalSection.Release;
  end;
end;


procedure TCppParser.ScanMethodArgs(const FunctionStatement:PStatement; ArgStr:string);
var
  I, ParamStart, SpacePos, bracketPos,assignPos: integer;
  S,Args: AnsiString;
begin
  // Split up argument string by ,
  I := 2; // assume it starts with ( and ends with )
  ParamStart := I;

  while I <= Length(ArgStr) do begin
    if (ArgStr[i] = ',') or ((I = Length(ArgStr)) and (ArgStr[i] = ')')) then begin

      // We've found "int* a" for example
      S := Trim(Copy(ArgStr, ParamStart, I - ParamStart));

      //remove default value
      assignPos := Pos('=',S);
      if assignPos > 0 then begin
        Delete(S, assignPos,MaxInt);
        S:=TrimRight(S);
      end;
      // we don't support function pointer parameters now, till we can tokenize function parameters
      {
      // Can be a function pointer. If so, scan after last )
      BracePos := LastPos(')', S);
      if (BracePos > 0) then // it's a function pointer... begin
        SpacePos := LastPos(' ', Copy(S, BracePos, MaxInt)) // start search at brace
      end else begin
      }
        SpacePos := LastPos(' ', S); // Cut up at last space

        if SpacePos > 0 then begin
          Args:='';
          bracketPos := Pos('[',S);
          if bracketPos > 0 then begin
            Args := Copy(S, bracketPos,MaxInt);
            Delete(S,bracketPos,MaxInt);
          end;
          AddStatement(
            FunctionStatement,
            FunctionStatement^._FileName,
            '', // do not override hint
            Copy(S, 1, SpacePos - 1), // 'int*'
            Copy(S, SpacePos + 1, MaxInt), // a
            Args,
            '',
            FunctionStatement^._DefinitionLine,
            skParameter,
            ssLocal,
            scsNone,
            True,
            nil,
            False);
        end;
        
      ParamStart := I + 1; // step over ,
    end;
    Inc(I);
  end;
end;

function TCppParser.FindFileIncludes(const Filename: AnsiString; DeleteIt: boolean): PFileIncludes;
var
  I: integer;
begin
  fCriticalSection.Acquire;
  try
  Result := nil;
  i:=FastIndexOf(fIncludesList,Filename);
  if (i<> -1) then begin
    Result := PFileIncludes(fIncludesList.Objects[I]);
    if DeleteIt then
      fIncludesList.Delete(I);
  end;
  finally
    fCriticalSection.Release;
  end;  
end;
{

function TCppParser.IsCfile(const Filename: AnsiString): boolean;
begin
  result := cbutils.IsCfile(FileName);
end;

function TCppParser.IsHfile(const Filename: AnsiString): boolean;
begin
  result := cbutils.IsHfile(Filename);
end;
}

procedure TCppParser.GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
begin
  cbutils.GetSourcePair(FName, CFile, HFile);
end;

procedure TCppParser.GetFileUsings(const Filename: AnsiString; var List: TDevStringList);
var
  I,t: integer;
  P,Q: PFileIncludes;
  sl: TStrings;
  name: AnsiString;
begin
  fCriticalSection.Acquire;
  try
  if FileName = '' then
    Exit;
  List.Clear;
  if fParsing then
    Exit;
  List.Sorted := False;

  P := FindFileIncludes(FileName);
  if Assigned(P) then begin
    for t:=0 to P^.Usings.Count -1 do begin
      if FastIndexOf(List,P^.Usings[t])=-1 then
        List.Add(P^.Usings[t]);
    end;
    sl := P^.IncludeFiles;
    for I := 0 to sl.Count - 1 do begin
      name:=sl[I];
      Q:=FindFileIncludes(name);
      if Assigned(Q) then begin
        for t:=0 to Q^.Usings.Count -1 do begin
          if FastIndexOf(List,Q^.Usings[t])=-1 then
            List.Add(Q^.Usings[t]);
        end;
      end;
    end;
  end;
  List.Sorted := True;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TCppParser.GetFileDirectIncludes(const Filename: AnsiString; var List: TStringList);
var
  I: integer;
  P: PFileIncludes;
  sl: TStrings;
begin
  fCriticalSection.Acquire;
  try
  if FileName = '' then
    Exit;
  List.Clear;
  if fParsing then
    Exit;

  P := FindFileIncludes(FileName);
  if Assigned(P) then begin
    sl := P^.DirectIncludeFiles;
    for I := 0 to sl.Count - 1 do
      List.Add(sl[I]);
  end;
  finally
    fCriticalSection.Release;
  end;
end;


//Since we have save all include files info, don't need to recursive find anymore
procedure TCppParser.GetFileIncludes(const Filename: AnsiString; var List: TStringList);
var
  I: integer;
  P: PFileIncludes;
  sl: TStrings;
begin
  fCriticalSection.Acquire;
  try
  if FileName = '' then
    Exit;
  List.Clear;
  if fParsing then
    Exit;
  List.Sorted := False;
  List.Add(FileName);

  P := FindFileIncludes(FileName);
  if Assigned(P) then begin
    sl := P^.IncludeFiles;
    for I := 0 to sl.Count - 1 do
      List.Add(sl[I]);
  end;
  List.Sorted := True;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TCppParser.InheritClassStatement(derived: PStatement; isStruct:boolean; base: PStatement; access:TStatementClassScope);
var
  Children:TList;
  i:integer;
  Statement:PStatement;
  m_acc:TStatementClassScope;
begin
{
  if not Assigned(base) then
    Exit;
  if not Assigned(derived) then
    Exit;
    }
  //differentiate class and struct
  if access = scsNone then
    if isStruct then
      access := scsPublic
    else
      access := scsPrivate;
  Children := base^._Children;
  if not assigned(Children) then
    Exit;
  for i:=0 to Children.Count-1 do begin
    Statement := PStatement(Children[i]);
    // don't inherit private members, constructors and destructors;
    if (Statement^._ClassScope = scsPrivate)
      or (Statement^._Kind in [skConstructor,skDestructor]) then
      continue;
    case access of
      scsPublic: m_acc:=Statement^._ClassScope;
      scsProtected: m_acc:=scsProtected;
      scsPrivate: m_acc:=scsPrivate;
    else
      m_acc:=scsPrivate;
    end;
    //inherit
    AddInheritedStatement(derived,Statement,m_acc);
  end;
end;

function TCppParser.AddInheritedStatement(derived:PStatement; inherit:PStatement; access:TStatementClassScope):PStatement;
var
  InheritanceList:TList;
begin
  if Assigned(inherit^._InheritanceList) then begin
    // we should copy it, or we will got trouble when clear StatementList
    InheritanceList := TList.Create;
    InheritanceList.Assign(inherit^._InheritanceList);
  end else
    InheritanceList := nil;
  Result:=AddStatement(
    derived,
    inherit^._FileName,
    inherit^._HintText,
    inherit^._Type, // "Type" is already in use
    inherit^._Command,
    inherit^._Args,
    inherit^._Value,
    inherit^._Line,
    inherit^._Kind,
    inherit^._Scope,
    access,
    True,
    InheritanceList,
    inherit^._Static);
end;

procedure TCppParser.Freeze;
begin
  fCriticalSection.Acquire;
  try
  fLocked := True;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TCppParser.UnFreeze;
begin
  fCriticalSection.Acquire;
  try
  fLocked := False;
  finally
    fCriticalSection.Release;
  end;  
end;


procedure TCppParser.SetTokenizer(tokenizer: TCppTokenizer);
begin
  fTokenizer := tokenizer;
end;

procedure TCppParser.SetPreprocessor(preprocessor: TCppPreprocessor);
begin
  fPreprocessor := preprocessor;
end;

procedure TCppParser.getFullNameSpace(const Phrase:AnsiString; var namespace:AnsiString; var member:AnsiString);
var
  lastI,i,idx,strLen:integer;
begin
  nameSpace := '';
  member:=Phrase;
  strLen := Length(Phrase);
  if strLen = 0 then
    Exit;
  lastI:=-1;
  i:=1;
  while (i<=strLen) do begin
    if (i<strLen) and (Phrase[i]=':') and (Phrase[i+1]=':') then begin
      idx := FastIndexOf(fNamespaces,nameSpace);
      if idx = -1 then
        break
      else
        lastI := i;
    end;
    nameSpace := nameSpace + Phrase[i];
    inc(i);
  end;
  if (i>strLen) then begin
      idx := FastIndexOf(fNamespaces,nameSpace);
      if idx <> -1 then begin
        namespace := Phrase;
        member := '';
        Exit;
      end;
  end;
  if lastI > 0 then begin
    namespace := Copy(Phrase,1,lastI-1);
    member := Copy(Phrase, lastI+2,MaxInt);
  end else begin
    namespace := '';
    member := Phrase;
  end;
end;

end.

