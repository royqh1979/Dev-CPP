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
{$IFDEF WIN32}
  Dialogs, Windows, Classes, SysUtils, StrUtils, ComCtrls, StatementList, CppTokenizer, CppPreprocessor,
  cbutils;
{$ENDIF}
{$IFDEF LINUX}
QDialogs, Classes, SysUtils, StrUtils, QComCtrls, U_IntList, CppTokenizer;
{$ENDIF}

type
  TCppParser = class(TComponent)
  private
    fEnabled: boolean;
    fIndex: integer;
    fIsHeader: boolean;
    fIsSystemHeader: boolean;
    fCurrentFile: AnsiString;
    fCurrentClass: TList; // list of lists
    fSkipList: integer;
    fClassScope: TStatementClassScope;
    fStatementList: TStatementList;
    fIncludesList: TList;
    fTokenizer: TCppTokenizer;
    fPreprocessor: TCppPreprocessor;
    fIncludePaths: TStringList;
    fProjectIncludePaths: TStringList;
    fProjectFiles: TStringList;
    fFilesToScan: TStringList; // list of base files to scan
    fFilesScannedCount: Integer; // count of files that have been scanned
    fFilesToScanCount: Integer; // count of files and files included in files that have to be scanned
    fScannedFiles: TStringList;
    fParseLocalHeaders: boolean;
    fParseGlobalHeaders: boolean;
    fProjectDir: AnsiString;
    fOnBusy: TNotifyEvent;
    fOnUpdate: TNotifyEvent;
    fOnTotalProgress: TProgressEvent;
    fLaterScanning: boolean;
    fOnStartParsing: TNotifyEvent;
    fOnEndParsing: TProgressEndEvent;
    fIsProjectFile: boolean;
    fInvalidatedStatements: TList;
    fPendingDeclarations: TList;
    fMacroDefines : TList;
    fTempNodes : TList;
    fLocked: boolean; // lock(don't reparse) when we need to find statements in a batch
    fParsing: boolean;
   
    function AddInheritedStatement(derived:PStatement; inherit:PStatement; access:TStatementClassScope):PStatement;

    function AddChildStatement(// support for multiple parents (only typedef struct/union use multiple parents)
      Parents: TList;
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
      FindDeclaration: boolean;
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
      FindDeclaration: boolean;
      IsDefinition: boolean;
      InheritanceList: TList;
      isStatic: boolean): PStatement;
    function IsSystemHeaderFile(const FileName: AnsiString): boolean;
    procedure SetInheritance(Index: integer; ClassStatement: PStatement; IsStruct:boolean);
    function GetLastCurrentClass: PStatement; // gets last item from lastt level
    function GetCurrentClassLevel: TList;
    function IsInCurrentClassLevel(const Command: AnsiString): PStatement;
    procedure AddSoloClassLevel(Statement: PStatement); // adds new solo level
    procedure AddMultiClassLevel(StatementList: TList); // adds new multi level
    procedure RemoveClassLevel; // removes level
    procedure CheckForSkipStatement;
    function SkipBraces(StartAt: integer): integer;
    function CheckForPreprocessor: boolean;
    function CheckForKeyword: boolean;

    function CheckForTypedef: boolean;
    function CheckForTypedefEnum: boolean;
    function CheckForTypedefStruct: boolean;
    function CheckForStructs: boolean;
    function CheckForMethod(var sType, sName, sArgs: AnsiString;
      var IsStatic:boolean; var IsFriend:boolean): boolean; // caching of results
    function CheckForScope: boolean;
    function CheckForVar: boolean;
    function CheckForEnum: boolean;
    function GetScope: TStatementScope;
    procedure HandlePreprocessor;
    procedure HandleOtherTypedefs;
    procedure HandleStructs(IsTypedef: boolean = False);
    procedure HandleMethod(const sType, sName, sArgs: AnsiString; isStatic: boolean;IsFriend:boolean);
    procedure ScanMethodArgs(const ArgStr: AnsiString; const Filename: AnsiString; Line: Integer);
    procedure HandleScope;
    procedure HandleKeyword;
    procedure HandleVar;
    procedure HandleEnum;
    function HandleStatement: boolean;
    procedure InternalParse(const FileName: AnsiString; ManualUpdate: boolean = False; Stream: TMemoryStream = nil);
    procedure DeleteTemporaries;
    function FindFileIncludes(const Filename: AnsiString; DeleteIt: boolean = False): PFileIncludes;
    function FindMacroDefine(const Command: AnsiString): PStatement;
    function expandMacroType(const name:AnsiString): AnsiString;
    procedure InheritClassStatement(derived: PStatement; isStruct:boolean; base: PStatement; access:TStatementClassScope);
    function GetIncompleteClass(const Command: AnsiString): PStatement;
    procedure ReProcessInheritance;
  public
    procedure ResetDefines;
    procedure AddHardDefineByParts(const Name, Args, Value: AnsiString);
    procedure AddHardDefineByLine(const Line: AnsiString);
    procedure InvalidateFile(const FileName: AnsiString);
    procedure GetFileIncludes(const Filename: AnsiString; var List: TStringList);
    function IsCfile(const Filename: AnsiString): boolean;
    function IsHfile(const Filename: AnsiString): boolean;
    procedure GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
    procedure GetClassesList(var List: TStringList);
    function SuggestMemberInsertionLine(ParentStatement: PStatement; Scope: TStatementClassScope; var AddScopeStr:
      boolean):
      integer;
    function GetSystemHeaderFileName(const FileName: AnsiString): AnsiString; // <file.h>
    function GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString): AnsiString; // "file.h"
    function GetHeaderFileName(const RelativeTo, Line: AnsiString): AnsiString; // both
    function IsIncludeLine(const Line: AnsiString): boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParseFileList;
    procedure ParseFile(const FileName: AnsiString; InProject: boolean; OnlyIfNotParsed: boolean = False; UpdateView:
      boolean = True; Stream: TMemoryStream = nil);
    function StatementKindStr(Value: TStatementKind): AnsiString;
    function StatementClassScopeStr(Value: TStatementClassScope): AnsiString;
    function FetchPendingDeclaration(const Command, Args: AnsiString; Kind: TStatementKind; Parent: PStatement):
      PStatement;
    procedure Reset;
    procedure ClearIncludePaths;
    procedure ClearProjectIncludePaths;
    procedure ClearProjectFiles;
    procedure AddIncludePath(const Value: AnsiString);
    procedure AddProjectIncludePath(const Value: AnsiString);
    procedure AddFileToScan(Value: AnsiString; InProject: boolean = False);
    function PrettyPrintStatement(Statement: PStatement): AnsiString;
    procedure FillListOfFunctions(const Full: AnsiString; List: TStringList);
    function FindAndScanBlockAt(const Filename: AnsiString; Row: integer; Stream: TMemoryStream): PStatement;
    function FindStatementOf(FileName, Phrase: AnsiString; Row: integer; Stream: TMemoryStream): PStatement; overload;
    function FindStatementOf(Phrase: AnsiString; CurrentClass: PStatement): PStatement; overload;
    function FindVariableOf(const Phrase: AnsiString; CurrentClass: PStatement): PStatement;
    function FindTypeDefinitionOf(const aType: AnsiString; CurrentClass: PStatement): PStatement;
    function GetClass(const Phrase: AnsiString): AnsiString;
    function GetMember(const Phrase: AnsiString): AnsiString;
    function GetOperator(const Phrase: AnsiString): AnsiString;
    function FindLastOperator(const Phrase: AnsiString): integer;
    procedure Freeze(FileName:AnsiString; Stream: TMemoryStream);  // Freeze/Lock (stop reparse while searching)
    procedure UnFreeze(); // UnFree/UnLock (reparse while searching)
  published
    property Enabled: boolean read fEnabled write fEnabled;
    property OnUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;
    property OnBusy: TNotifyEvent read fOnBusy write fOnBusy;
    property OnTotalProgress: TProgressEvent read fOnTotalProgress write fOnTotalProgress;
    property Tokenizer: TCppTokenizer read fTokenizer write fTokenizer;
    property Preprocessor: TCppPreprocessor read fPreprocessor write fPreprocessor;
    property Statements: TStatementList read fStatementList write fStatementList;
    property ParseLocalHeaders: boolean read fParseLocalHeaders write fParseLocalHeaders;
    property ParseGlobalHeaders: boolean read fParseGlobalHeaders write fParseGlobalHeaders;
    property ScannedFiles: TStringList read fScannedFiles;
    property ProjectDir: AnsiString read fProjectDir write fProjectDir;
    property OnStartParsing: TNotifyEvent read fOnStartParsing write fOnStartParsing;
    property OnEndParsing: TProgressEndEvent read fOnEndParsing write fOnEndParsing;
    property FilesToScan: TStringList read fFilesToScan;
  end;

procedure Register;

implementation

uses
  DateUtils, IniFiles;

procedure Register;
begin
  RegisterComponents('Dev-C++', [TCppParser]);
end;

constructor TCppParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fParsing:=False;
  fStatementList := TStatementList.Create; // owns the objects
  fIncludesList := TList.Create;
  fFilesToScan := TStringList.Create;
  fScannedFiles := TStringList.Create;
  fIncludePaths := TStringList.Create;
  fProjectIncludePaths := TStringList.Create;
  fProjectFiles := TStringList.Create;
  fInvalidatedStatements := TList.Create;
  fPendingDeclarations := TList.Create;
  fMacroDefines := TList.Create;
  fCurrentClass := TList.Create;
  fSkipList:= -1;
  fParseLocalHeaders := False;
  fParseGlobalHeaders := False;
  fLocked := False;
  fTempNodes := TList.Create;
end;

destructor TCppParser.Destroy;
var
  i: Integer;
begin
  FreeAndNil(fMacroDefines);
  FreeAndNil(fPendingDeclarations);
  FreeAndNil(fInvalidatedStatements);
  FreeAndNil(fCurrentClass);
  FreeAndNil(fProjectFiles);
  FreeAndNil(fTempNodes);

  for i := 0 to fIncludesList.Count - 1 do begin
    PFileIncludes(fIncludesList[i])^.IncludeFiles.Free;
    Dispose(PFileIncludes(fIncludesList[i]));
  end;
  FreeAndNil(fIncludesList);

  FreeAndNil(fStatementList);
  FreeAndNil(fFilesToScan);
  FreeAndNil(fScannedFiles);
  FreeAndNil(fIncludePaths);
  FreeAndNil(fProjectIncludePaths);

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
//    skPreprocessor: Result := 'preprocessor';
//    skVariable: Result := 'variable';
//    skConstructor: Result := 'constructor';
//    skDestructor: Result := 'destructor';
//    skFunction: Result := 'function';
//    skClass: Result := 'class';
//    skTypedef: Result := 'typedef';
//    skEnum: Result := 'enum';
//    skUnknown: Result := 'unknown';
    skPreprocessor: Result := 'P';
    skVariable: Result := 'V';
    skConstructor: Result := 'Ct';
    skDestructor: Result := 'Dt';
    skFunction: Result := 'F';
    skClass: Result := 'C';
    skTypedef: Result := 'T';
    skEnum: Result := 'E';
    skUnknown: Result := 'U';
  end;
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

function TCppParser.expandMacroType(const name:AnsiString): AnsiString;
var
  Statement: PStatement;
begin
  Result := name;
  if StartsStr('__',name) then begin
    Result := '';
    Exit;
  end;
  Statement := FindMacroDefine(name);
  if Assigned(Statement) then begin
    if (Statement^._Value <> '') then
      Result:= Name
    else
      Result:='';
  end;
end;

// When finding declaration/definition pairs only search the separate incomplete pair list

function TCppParser.FetchPendingDeclaration(const Command, Args: AnsiString; Kind: TStatementKind; Parent: PStatement):
  PStatement;
var
  Statement: PStatement;
  I: integer;
begin
  // we do a backward search, because most possible is to be found near the end ;) - if it exists :(
  for I := fPendingDeclarations.Count - 1 downto 0 do begin
    Statement := fPendingDeclarations[i];

    // Only do an expensive string compare with the right kinds and parents
    if Statement^._Parent = Parent then begin
      if Statement^._Kind = Kind then begin
        if Statement^._Command = Command then begin
          if Statement^._Args = Args then begin
            fPendingDeclarations.Delete(i); // remove it when we have found it
            Result := Statement;
            Exit;
          end;
        end;
      end;
    end;
  end;

  Result := nil;
end;

// When finding a parent class for a function definition, only search classes of incomplete decl/def pairs

function TCppParser.GetIncompleteClass(const Command: AnsiString): PStatement;
var
  Statement, ParentStatement: PStatement;
  I: integer;
begin
  // we do a backward search, because most possible is to be found near the end ;) - if it exists :(
  for I := fPendingDeclarations.Count - 1 downto 0 do begin
    Statement := fPendingDeclarations[i];
    ParentStatement := Statement^._Parent;
    if Assigned(ParentStatement) then begin
      if ParentStatement^._Command = Command then begin
        Result := ParentStatement;
        Exit;
      end;
    end;
  end;

  Result := nil;
end;

function TCppParser.AddChildStatement(
  Parents: TList;
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
  FindDeclaration: boolean;
  IsDefinition: boolean;
  isStatic: boolean): PStatement;
var
  I: integer;
begin
  Result := nil;
  if Assigned(Parents) then begin
    for I := 0 to Parents.Count - 1 do
      Result := AddStatement(
        Parents[i],
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
        FindDeclaration,
        IsDefinition,
        nil,
        isStatic);
  end else begin
    Result := AddStatement(
      nil,
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
      FindDeclaration,
      IsDefinition,
      nil,
      isStatic);
  end;
end;

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
  FindDeclaration: boolean;
  IsDefinition: boolean;
  InheritanceList: TList;
  isStatic:boolean): PStatement;
var
  Declaration: PStatement;
  OperatorPos: integer;
  NewKind: TStatementKind;
  NewType, NewCommand: AnsiString;
  node: PStatementNode;
  function AddToList: PStatement;
  begin
    Result := New(PStatement);
    with Result^ do begin
      _Parent := Parent;
      _HintText := HintText;
      _Type := NewType;
      _Command := NewCommand;
      _Args := Args;
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
      _Temporary := fLaterScanning; // true if it's added by a function body scan
      _InProject := fIsProjectFile;
      _InSystemHeader := fIsSystemHeader;
      _Children := nil;
      _Friends := nil;
      _Static := isStatic;
      _Inherited:= False;
    end;
    node:=fStatementList.Add(Result);
    if (Result^._Temporary) then
      fTempNodes.Add(node);
  end;
begin
  // Move '*', '&' to type rather than cmd (it's in the way for code-completion)
  NewType := aType;
  NewCommand := Command;
  while (Length(NewCommand) > 0) and (NewCommand[1] in ['*', '&']) do begin
    NewType := NewType + NewCommand[1];
    Delete(NewCommand, 1, 1); // remove first
  end;

  NewKind := Kind;

  // Remove namespace stuff from type (until we support namespaces)
  if NewKind in [skFunction, skVariable] then begin
    OperatorPos := Pos('::', NewType);
    if OperatorPos > 0 then
      Delete(NewType, 1, OperatorPos + 1);
  end;

  // Find a declaration/definition pair
  if FindDeclaration and IsDefinition then
    Declaration := FetchPendingDeclaration(NewCommand, Args, Kind, Parent)
  else
    Declaration := nil;

  // We already have a statement with the same identifier...
  if Assigned(Declaration) then begin

    Declaration^._DefinitionLine := Line;
    Declaration^._DefinitionFileName := FileName;
    Declaration^._HasDefinition := True;
    Result := Declaration;

    // No duplicates found. Proceed as usual
  end else begin
    Result := AddToList;
    if not IsDefinition then // add declarations to separate list to speed up searches for them
      fPendingDeclarations.Add(Result);
  end;

  if Kind = skPreprocessor then begin
     fMacroDefines.Add(Result);
  end;
end;

function TCppParser.GetLastCurrentClass: PStatement;
var
  CurrentClassLevel: TList;
begin
  if fCurrentClass.Count = 0 then begin
    Result := nil;
    Exit;
  end;

  // Get current classes at same level
  CurrentClassLevel := fCurrentClass[fCurrentClass.Count - 1];

  // Pick last one
  if CurrentClassLevel.Count > 0 then
    Result := CurrentClassLevel[CurrentClassLevel.Count - 1]
  else
    Result := nil;
end;

function TCppParser.GetCurrentClassLevel: TList;
begin
  if fCurrentClass.Count = 0 then begin
    Result := nil;
    Exit;
  end;
  Result := fCurrentClass[fCurrentClass.Count - 1];
end;

function TCppParser.IsInCurrentClassLevel(const Command: AnsiString): PStatement;
var
  CurrentClassLevel: TList;
  I: integer;
  Statement: PStatement;
begin
  Result := nil;
  CurrentClassLevel := GetCurrentClassLevel;
  if Assigned(CurrentClassLevel) then begin
    for I := 0 to CurrentClassLevel.Count - 1 do begin
      Statement := CurrentClassLevel[i];
      if Assigned(Statement) and SameStr(Command, Statement^._Command) then begin
        Result := Statement;
        Break;
      end;
    end;
  end;
end;

procedure TCppParser.AddSoloClassLevel(Statement: PStatement);
var
  NewLevel: TList;
begin
  // Add class list
  NewLevel := TList.Create;
  NewLevel.Add(Statement);
  fCurrentClass.Add(NewLevel);

  // Set new scope
  if Statement = nil then begin
    fClassScope := scsNone // {}, namespace or class that doesn't exist
  end else if Statement^._Type = 'class' then
    fClassScope := scsPrivate // classes are private by default
  else
    fClassScope := scsPublic; // structs are public by default
end;

procedure TCppParser.AddMultiClassLevel(StatementList: TList);
var
  Statement: PStatement;
  ListCopy: TList;
begin
  // Add list to list
  ListCopy := TList.Create;
  ListCopy.Assign(StatementList);
  fCurrentClass.Add(ListCopy);

  // Check scope of first one
  if StatementList.Count > 0 then
    Statement := StatementList[0]
  else
    Statement := nil;

  // Set new scope
  if Statement = nil then begin
    fClassScope := scsNone // {}, namespace or class that doesn't exist
  end else if Statement^._Type = 'class' then
    fClassScope := scsPrivate // classes are private by default
  else
    fClassScope := scsPublic; // structs are public by default
end;

procedure TCppParser.RemoveClassLevel;
var
  CurrentLevel: TList;
  CurrentClass: PStatement;
begin
  // Remove class list
  if fCurrentClass.Count = 0 then
    Exit; // TODO: should be an exception
  TList(fCurrentClass[fCurrentClass.Count - 1]).Free;
  fCurrentClass.Delete(fCurrentClass.Count - 1);

  // Set new scope
  CurrentLevel := GetCurrentClassLevel;
  if not Assigned(CurrentLevel) then begin
    fClassScope := scsNone // no classes or structs remaining
  end else begin
    CurrentClass := GetLastCurrentClass;
    if Assigned(CurrentClass) and (CurrentClass^._Type = 'class') then
      fClassScope := scsPrivate // classes are private by default
    else
      fClassScope := scsPublic;
  end;
end;

procedure TCppParser.SetInheritance(Index: integer; ClassStatement: PStatement; IsStruct:boolean);
var
  Node: PStatementNode;
  Statement: PStatement;
  inheritScopeType: TStatementClassScope;
  lastInheritScopeType : TStatementClassScope;
  basename: AnsiString;
  
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
        // Find the corresponding PStatement
        //todo: add class list to speed up search
        Node := fStatementList.LastNode;
        while Assigned(Node) do begin
          Statement := Node^.Data;
          if (Statement^._Kind = skClass) and SameStr(basename, Statement^._Command) then begin
            ClassStatement._InheritanceList.Add(Statement); // next I please
            InheritClassStatement(ClassStatement,isStruct, Statement,lastInheritScopeType);
            break;
          end;
          Node := Node^.PrevNode;
        end;
      end;
    Inc(Index);
    lastInheritScopeType := inheritScopeType;
  until (Index >= fTokenizer.Tokens.Count) or (fTokenizer[Index]^.Text[1] in ['{', ';']);
end;

function TCppParser.GetScope: TStatementScope;
var
  CurrentClass: PStatement;
begin
  // We are scanning function bodies
  if fLaterScanning then begin
    Result := ssLocal;
    Exit;
  end;

  // Don't blindly trust levels. Namespaces and externs can have levels too
  CurrentClass := GetLastCurrentClass;

  // Invalid class or namespace/extern
  if CurrentClass = nil then
    Result := ssGlobal

    // We are inside a class body
  else if Assigned(CurrentClass) then
    Result := ssClassLocal

    // Everything else
  else
    Result := ssLocal;
end;

procedure TCppParser.CheckForSkipStatement;
begin
  if fIndex = fSkipList then begin // skip to next ';'
    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';']);
    Inc(fIndex); //skip ';'
    fSkipList:=-1;
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
  Result := SameStr(fTokenizer[fIndex]^.Text, 'typedef');
end;

function TCppParser.CheckForEnum: boolean;
begin
  Result := SameStr(fTokenizer[fIndex]^.Text, 'enum');
end;

function TCppParser.CheckForTypedefEnum: boolean;
begin
  //we assume that typedef is the current index, so we check the next
  //should call CheckForTypedef first!!!
  Result := (fIndex < fTokenizer.Tokens.Count - 1) and
    SameStr(fTokenizer[fIndex + 1]^.Text, 'enum');
end;

function TCppParser.CheckForTypedefStruct: boolean;
begin
  //we assume that typedef is the current index, so we check the next
  //should call CheckForTypedef first!!!
  Result := (fIndex < fTokenizer.Tokens.Count - 1) and (
    SameStr(fTokenizer[fIndex + 1]^.Text, 'struct') or
    SameStr(fTokenizer[fIndex + 1]^.Text, 'class') or
    SameStr(fTokenizer[fIndex + 1]^.Text, 'union'));
end;

function TCppParser.CheckForStructs: boolean;
var
  I: integer;
  dis: integer;
begin
  if SameStr(fTokenizer[fIndex]^.Text, 'friend') then
    dis := 1
  else
    dis := 0;
  Result := (fIndex < fTokenizer.Tokens.Count -2-dis) and (
    SameStr(fTokenizer[fIndex+dis]^.Text, 'struct') or
    SameStr(fTokenizer[fIndex+dis]^.Text, 'class') or
    SameStr(fTokenizer[fIndex+dis]^.Text, 'union'));
  if Result then begin
    if fTokenizer[fIndex + 2+dis]^.Text[1] <> ';' then begin // not: class something;
      I := fIndex+dis;
      // the check for ']' was added because of this example:
      // struct option long_options[] = {
      //		{"debug", 1, 0, 'D'},
      //		{"info", 0, 0, 'i'},
      //    ...
      //  };
      while (I < fTokenizer.Tokens.Count) and not (fTokenizer[I]^.Text[Length(fTokenizer[I]^.Text)] in [';', ':', '{',
        '}', ',', ')', ']']) do
        Inc(I);
      if (I < fTokenizer.Tokens.Count) and not (fTokenizer[I]^.Text[1] in ['{', ':']) then
        Result := False;
    end;
  end;
end;

function TCppParser.CheckForMethod(var sType, sName, sArgs: AnsiString;
  var isStatic:boolean;var IsFriend:boolean): boolean;
var
  CurrentClassLevel: TList;
  fIndexBackup, DelimPos: integer;
  bTypeOK, bNameOK, bArgsOK: boolean;
  s:AnsiString;
begin

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
        end;
      end;

      // Are we inside a class body?
      if not bTypeOK then begin
        CurrentClassLevel := GetCurrentClassLevel;
        if Assigned(CurrentClassLevel) then begin
          sType := fTokenizer[fIndex]^.Text;
          if sType[1] = '~' then
            Delete(sType, 1, 1);
          bTypeOK := Assigned(IsInCurrentClassLevel(sType)); // constructor/destructor
        end;
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
    if (fTokenizer[fIndex]^.Text[1] in ['#', '{', '}']) or CheckForKeyword then begin
      Break; // fail
    end else if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '(') and
      (fTokenizer[fIndex]^.Text[2] = '(') then begin // TODO: is this used to remove __attribute stuff?
      Break;
    end else if fTokenizer[fIndex]^.Text[1] in [',', ';'] then begin
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
    if fTokenizer[fIndex]^.Text = '*' then begin // * is not part of type info
      Inc(fIndex);
      break;
    end;
    Inc(fIndex);
  until (fIndex + 1 >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex + 1]^.Text[1] in ['(', ',', ';']);
  OldType:= TrimRight(OldType);


  // Add synonyms for old
  if (fIndex+1 < fTokenizer.Tokens.Count) and (OldType <> '') then begin
    repeat
      // Support multiword typedefs
      if (fTokenizer[fIndex]^.Text[1] = '(') then begin // function define
        if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] = '(') then begin
          //valid function define
          p:=LastDelimiter(' ',fTokenizer[fIndex]^.Text);
          if p = 0 then
            NewType := Copy(fTokenizer[fIndex]^.Text,2,Length(fTokenizer[fIndex]^.Text)-2)
          else
            NewType := Copy(fTokenizer[fIndex]^.Text,p+1,Length(fTokenizer[fIndex]^.Text)-p-1);
          AddStatement(
            GetLastCurrentClass,
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
            False, // check for declarations when we find an definition of a function
            True,
            nil,
            False);
         end;
         NewType:='';
         //skip to ',' or ';'
         while (fIndex< fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in [',', ';']) do
            Inc(fIndex);
      end else if not (fTokenizer[fIndex+1]^.Text[1] in [',', ';', '(']) then begin
        NewType := NewType + fTokenizer[fIndex]^.Text + ' ';
        Inc(fIndex);
      end else begin
        NewType := NewType + fTokenizer[fIndex]^.Text + ' ';
        NewType := TrimRight(NewType);
        AddStatement(
          GetLastCurrentClass,
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
          False,
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
      fIsSystemHeader := IsSystemHeaderFile(fCurrentFile);
      fIsProjectFile := fProjectFiles.IndexOf(fCurrentFile) <> -1;
      fIsHeader := IsHfile(fCurrentFile);

      // Mention progress to user if we enter a NEW file
      Line := StrToIntDef(Copy(S, DelimPos + 1, MaxInt), -1);
      if Line = 1 then begin
        Inc(fFilesScannedCount);
        Inc(fFilesToScanCount);
        if Assigned(fOnTotalProgress) and not fLaterScanning then
          fOnTotalProgress(Self, fCurrentFile, fFilesToScanCount, fFilesScannedCount);
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
      False,
      True,
      nil,
      False);
  end;
  //TODO "#undef support"
  Inc(fIndex);
end;

procedure TCppParser.HandleStructs(IsTypedef: boolean = False);
var
  Command, Prefix, OldType, NewType: AnsiString;
  I: integer;
  IsStruct: boolean;
  IsFriend: boolean;
  ParentStatement,FirstSynonym, LastStatement: PStatement;
  SharedInheritance, NewClassLevel: TList;
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
  IsStruct := SameStr(Prefix, 'struct') or SameStr(Prefix, 'union');
  startLine := fTokenizer[fIndex]^.Line;
  Inc(fIndex); //skip struct/class/union

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
            GetLastCurrentClass,
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
            False,
            True,
            nil,
            False);
        end;
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] = ';');

      // Forward declaration, struct Foo. Don't mention in class browser
    end else begin
      if IsFriend then begin
        ParentStatement := GetLastCurrentClass;
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
        if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] in [',', ';', '{', ':']) then
          begin
          Command := fTokenizer[fIndex]^.Text;
          if Command <> '' then begin
            FirstSynonym := AddStatement(
              GetLastCurrentClass,
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
              False,
              True,
              TList.Create,
              False);
            Command := '';
          end;
        end;
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
        fSkipList:=I+1; // add first name to skip statement so that we can skip it until the next ;

      // Add class/struct synonyms after close brace
      if (I + 1 < fTokenizer.Tokens.Count) and not (fTokenizer[I + 1]^.Text[1] in [';', '}']) then begin
        Command := '';
        NewClassLevel := TList.Create;
        try
          // Add synonym before opening brace
          if Assigned(FirstSynonym) then
            NewClassLevel.Add(FirstSynonym);
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
                if fTokenizer[I]^.Text[Length(fTokenizer[I]^.Text)] = ']' then // cut-off array brackets
                  Command := Command + Copy(fTokenizer[I]^.Text, 1, Pos('[', fTokenizer[I]^.Text) - 1) + ' '
                else if fTokenizer[I]^.Text[1] in ['*', '&'] then // do not add spaces after pointer operator
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
                if Assigned(FirstSynonym) then begin
                  SharedInheritance := TList.Create;
                  SharedInheritance.Assign(FirstSynonym^._InheritanceList);
                end else
                  SharedInheritance := nil;
                LastStatement := AddStatement(
                  GetLastCurrentClass,
                  fCurrentFile,
                  '', // do not override hint
                  Prefix,
                  Command,
                  '',
                  '',
                  //fTokenizer[I]^.Line,
                  startLine,
                  skClass,
                  GetScope,
                  fClassScope,
                  False,
                  True,
                  SharedInheritance,
                  False); // all synonyms inherit from the same statements
                NewClassLevel.Add(LastStatement);
                Command := '';
              end;
            end;
          until (I >= fTokenizer.Tokens.Count - 1) or (fTokenizer[I]^.Text[1] in ['{', ';']);

          // Set current class level
          AddMultiClassLevel(NewClassLevel);
        finally
          NewClassLevel.Free;
        end;

        // Nothing worth mentioning after closing brace
        // Proceed to set first synonym as current class
      end else if Assigned(FirstSynonym) then
        AddSoloClassLevel(FirstSynonym);

      // Classes do not have synonyms after the brace
      // Proceed to set first synonym as current class
    end else
      AddSoloClassLevel(FirstSynonym);

    // Step over {
    if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '{') then
      Inc(fIndex);
  end;
end;

procedure TCppParser.HandleMethod(const sType, sName, sArgs: AnsiString; isStatic: boolean; IsFriend:boolean);
var
  IsValid, IsDeclaration: boolean;
  I, DelimPos: integer;
  FunctionKind: TStatementKind;
  ParentClassName, ScopelessName: AnsiString;
  FunctionClass: PStatement;
  startLine : integer;

begin
  IsValid := True;
  IsDeclaration := False; // assume it's not a prototype
  I := fIndex;
  startLine := fTokenizer[fIndex]^.Line;

  // Skip over argument list
  while (fIndex < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in [';', ':', '{', '}']) do
    Inc(fIndex);

  // Check if this is a prototype
  FunctionClass := GetLastCurrentClass;
  if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] in [';', '}']) then begin // prototype
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
      FunctionClass := GetIncompleteClass(ParentClassName);
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
    if not IsDeclaration then
      AddStatement(
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
        not IsDeclaration, // check for declarations when we find an definition of a function
        not IsDeclaration,
        nil,
        IsStatic)

      // For function declarations, any given statement can belong to multiple typedef names
    else
      AddChildStatement(
        GetCurrentClassLevel,
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
        not IsDeclaration, // check for declarations when we find an definition of a function
        not IsDeclaration,
        IsStatic);
  end;

  // Don't parse the function's block now... It will be parsed when user presses ctrl+space inside it ;)
  if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '{') then
    fIndex := SkipBraces(fIndex) + 1 // add 1 so that '}' is not visible to parser
  else if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = ';') then
    Inc(fIndex);
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
begin
  IsStatic := False;
  // Keep going and stop on top of the variable name
  LastType := '';
  IsFunctionPointer := False;
  repeat
    if (fIndex + 2 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] = '(') and (fTokenizer[fIndex +
      2]^.Text[1] = '(') then begin
      IsFunctionPointer := Pos('*', fTokenizer[fIndex + 1]^.Text) > 0;
      if not IsFunctionPointer then
        break; // inline constructor
    end else if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] in ['(', ',', ';', ':', '}',
      '#']) then begin
      Break;
    end;


    // we've made a mistake, this is a typedef , not a variable definition.
    if SameStr(fTokenizer[fIndex]^.Text, 'typedef') then
      Exit;

    // struct/class/union is part of the type signature
    // but we dont store it in the type cache, so must trim it to find the type info
    if (not SameStr(fTokenizer[fIndex]^.Text, 'struct')) and
      (not SameStr(fTokenizer[fIndex]^.Text, 'class')) and
      (not SameStr(fTokenizer[fIndex]^.Text, 'union')) then  begin
      s:=expandMacroType(fTokenizer[fIndex]^.Text);
      if s<>'' then
        LastType := LastType + ' '+expandMacroType(fTokenizer[fIndex]^.Text);
      if SameStr(s,'static') then
        isStatic := True;
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
          GetCurrentClassLevel,
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
          False,
          True,
          IsStatic); // TODO: not supported to pass list
      end;

      // Step over the variable name
      if not (fTokenizer[fIndex]^.Text[1] in [';', '{', '}']) then
        Inc(fIndex);
    end;
  until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';', '{', '}']);

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
      GetLastCurrentClass,
      fCurrentFile,
      '', // do not override hint
      'enum',
      EnumName,
      Args,
      '',
      //fTokenizer[fIndex]^.Line,
      startLine,
      skEnum,
      GetScope,
      fClassScope,
      False,
      True,
      nil,
      False);

  // Skip opening brace
  Inc(fIndex);

  // Call every member "enum NAME ITEMNAME"
  LastType := TrimRight('enum ' + EnumName);
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
        GetLastCurrentClass,
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
        False,
        True,
        nil,
        False);
    end;
    Inc(fIndex);
  until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';', '{', '}']);

  // Step over closing brace
  if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '}') then
    Inc(fIndex);
end;

function TCppParser.HandleStatement: boolean;
var
  S1, S2, S3: AnsiString;
  isStatic,isFriend: boolean;
begin
  if fTokenizer[fIndex]^.Text[1] = '{' then begin
    AddSoloClassLevel(nil);
    Inc(fIndex);
  end else if fTokenizer[fIndex]^.Text[1] = '}' then begin
    RemoveClassLevel;
    Inc(fIndex);
  end else if CheckForPreprocessor then begin
    HandlePreprocessor;
  end else if CheckForKeyword then begin // includes template now
    HandleKeyword;
  end else if CheckForScope then begin
    HandleScope;
  end else if CheckForEnum then begin
    HandleEnum;
  end else if CheckForTypedef then begin
    if CheckForTypedefStruct then begin // typedef struct something
      Inc(fIndex); // skip 'typedef'
      HandleStructs(True)
    end else if CheckForTypedefEnum then begin // typedef enum something
      Inc(fIndex); // skip 'typedef'
      HandleEnum;
    end else
      HandleOtherTypedefs; // typedef Foo Bar
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
  if (not ManualUpdate) and Assigned(fOnStartParsing) then
    fOnStartParsing(Self);

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
    if (not ManualUpdate) and Assigned(fOnEndParsing) then
      fOnEndParsing(Self, -1);
    fPreprocessor.Reset; // remove buffers from memory
    Exit;
  end;

   {
  with TStringList.Create do try
    Text:=fPreprocessor.Result;
    SaveToFile('f:\\result.txt');
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
    if (not ManualUpdate) and Assigned(fOnEndParsing) then
      fOnEndParsing(Self, -1);
    fPreprocessor.Reset;
    fTokenizer.Reset;
    Exit;
  end;

  // Tokenize the token list
  fIndex := 0;
  fClassScope := scsNone;
  fSkipList:=-1;
  try
    repeat
    until not HandleStatement;
    //Statements.DumpTo('f:\\statements.txt');
  finally
    //fSkipList:=-1; // remove data from memory, but reuse structures
    fCurrentClass.Clear;
    fPreprocessor.Reset;
    fTokenizer.Reset;
    if (not ManualUpdate) and Assigned(fOnEndParsing) then
      fOnEndParsing(Self, 1);
  end;
  if not ManualUpdate then
    if Assigned(fOnUpdate) then
      fOnUpdate(Self);
end;

procedure TCppParser.Reset;
var
  I: integer;
begin
  if Assigned(fOnBusy) then
    fOnBusy(Self);
  if Assigned(fPreprocessor) then
    fPreprocessor.Clear;
    
  fParsing:=False;
  fSkipList := -1;
  fLocked:=False;
  fParseLocalHeaders := False;
  fParseGlobalHeaders := False;
  //remove all macrodefines;
  fMacroDefines.Clear;
  fPendingDeclarations.Clear; // should be empty anyways
  fInvalidatedStatements.Clear;
  fCurrentClass.Clear;
  fProjectFiles.Clear;
  fTempNodes.Clear;
  fFilesToScan.Clear;
  if Assigned(fTokenizer) then
    fTokenizer.Reset;

  // Remove all statements
  fStatementList.Clear;

  // We haven't scanned anything anymore
  fScannedFiles.Clear;

  // We don't include anything anymore
  for I := fIncludesList.Count - 1 downto 0 do begin
    //fPreprocessor.InvalidDefinesInFile(PFileIncludes(fIncludesList[I])^.BaseFile);
    PFileIncludes(fIncludesList[I])^.IncludeFiles.Free;
    Dispose(PFileIncludes(fIncludesList[I]));
  end;
  fIncludesList.Clear;

  fProjectIncludePaths.Clear;
  fIncludePaths.Clear;

  fProjectFiles.Clear;

  if Assigned(fOnUpdate) then
    fOnUpdate(Self);
end;

procedure TCppParser.ParseFileList;
var
  I: integer;
begin
  if fParsing then
    Exit;
  fParsing:=True;
  try
    if not fEnabled then
      Exit;
    if Assigned(fOnBusy) then
      fOnBusy(Self);
    if Assigned(fOnStartParsing) then
      fOnStartParsing(Self);
    try
      // Support stopping of parsing when files closes unexpectedly
      I := 0;
      fFilesScannedCount := 0;
      fFilesToScanCount := fFilesToScan.Count;
      while I < fFilesToScan.Count do begin
        Inc(fFilesScannedCount); // progress is mentioned before scanning begins
        if Assigned(fOnTotalProgress) then
          fOnTotalProgress(Self, fFilesToScan[i], fFilesToScanCount, fFilesScannedCount);
        if fScannedFiles.IndexOf(fFilesToScan[i]) = -1 then begin
          InternalParse(fFilesToScan[i], True);
        end;
        Inc(I);
      end;
      fPendingDeclarations.Clear; // should be empty anyways
      fFilesToScan.Clear;
    finally
      if Assigned(fOnEndParsing) then
        fOnEndParsing(Self, fFilesScannedCount);
    end;
    if Assigned(fOnUpdate) then
      fOnUpdate(Self);
  finally
    fParsing:=False;
  end;
end;

function TCppParser.GetSystemHeaderFileName(const FileName: AnsiString): AnsiString;
begin
  Result := cbutils.GetSystemHeaderFileName(FileName, fIncludePaths);
end;

function TCppParser.GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString): AnsiString;
begin
  Result := cbutils.GetLocalHeaderFileName(RelativeTo, FileName, fProjectIncludePaths);
end;

function TCppParser.GetHeaderFileName(const RelativeTo, Line: AnsiString): AnsiString;
begin
  Result := cbutils.GetHeaderFileName(RelativeTo, Line, fIncludePaths, fProjectIncludePaths);
end;

function TCppParser.IsIncludeLine(const Line: AnsiString): boolean;
begin
  Result := cbutils.IsIncludeLine(Line);
end;

procedure TCppParser.AddFileToScan(Value: AnsiString; InProject: boolean);
begin
  Value := StringReplace(Value, '/', '\', [rfReplaceAll]); // only accept full file names

  // Update project listing
  if InProject then
    if fProjectFiles.IndexOf(Value) = -1 then
      fProjectFiles.Add(Value);

  // Only parse given file
  if fFilesToScan.IndexOf(Value) = -1 then // check scheduled files
    if fScannedFiles.IndexOf(Value) = -1 then // check files already parsed
      fFilesToScan.Add(Value);
end;

procedure TCppParser.AddIncludePath(const Value: AnsiString);
var
  S: AnsiString;
begin
  S := AnsiDequotedStr(Value, '"');
  if fIncludePaths.IndexOf(S) = -1 then
    fIncludePaths.Add(S);
end;

procedure TCppParser.AddProjectIncludePath(const Value: AnsiString);
var
  S: AnsiString;
begin
  S := AnsiDequotedStr(Value, '"');
  if fProjectIncludePaths.IndexOf(S) = -1 then
    fProjectIncludePaths.Add(S);
end;

procedure TCppParser.ClearIncludePaths;
begin
  fIncludePaths.Clear;
end;

procedure TCppParser.ClearProjectIncludePaths;
begin
  fProjectIncludePaths.Clear;
end;

procedure TCppParser.ClearProjectFiles;
begin
  fProjectFiles.Clear;
end;

procedure TCppParser.ResetDefines;
begin
  if Assigned(fPreprocessor) then
    fPreprocessor.ResetDefines;
end;

procedure TCppParser.AddHardDefineByParts(const Name, Args, Value: AnsiString);
begin
  if Assigned(fPreprocessor) then
    fPreprocessor.AddDefineByParts(Name, Args, Value, True);
end;

procedure TCppParser.AddHardDefineByLine(const Line: AnsiString);
begin
  if Assigned(fPreprocessor) then begin
    if Pos('#', Line) = 1 then
      fPreprocessor.AddDefineByLine(TrimLeft(Copy(Line, 2, MaxInt)), True)
    else
      fPreprocessor.AddDefineByLine(Line, True);
  end;
end;

function TCppParser.IsSystemHeaderFile(const FileName: AnsiString): boolean;
begin
  Result := cbutils.IsSystemHeaderFile(FileName, fIncludePaths);
end;

procedure TCppParser.ParseFile(const FileName: AnsiString; InProject: boolean; OnlyIfNotParsed: boolean = False;
  UpdateView: boolean = True; Stream: TMemoryStream = nil);
var
  FName: AnsiString;
  CFile, HFile: AnsiString;
  I: integer;
begin
  if fParsing then
    Exit;
  fParsing:=True;
  try
    if not fEnabled then
      Exit;
    FName := FileName;
    if OnlyIfNotParsed and (fScannedFiles.IndexOf(FName) <> -1) then
      Exit;
    if UpdateView then
      if Assigned(fOnBusy) then
        fOnBusy(Self);

    // Always invalidate file pairs. If we don't, reparsing the header
    // screws up the information inside the source file
    GetSourcePair(FName, CFile, HFile);
    fInvalidatedStatements.Clear;
    InvalidateFile(CFile);
    InvalidateFile(HFile);

    if InProject then begin
      if (CFile <> '') and (fProjectFiles.IndexOf(CFile) = -1) then
        fProjectFiles.Add(CFile);
      if (HFile <> '') and (fProjectFiles.IndexOf(HFile) = -1) then
        fProjectFiles.Add(HFile);
    end else begin
      I := fProjectFiles.IndexOf(CFile);
      if I <> -1 then
        fProjectFiles.Delete(I);
      I := fProjectFiles.IndexOf(HFile);
      if I <> -1 then
        fProjectFiles.Delete(I);
    end;

    // Parse from disk or stream
    if Assigned(fOnStartParsing) then
      fOnStartParsing(Self);
    try
      fFilesToScanCount := 0;
      fFilesScannedCount := 0;
      if not Assigned(Stream) then begin
        if CFile = '' then
          InternalParse(HFile, True) // headers should be parsed via include
        else
          InternalParse(CFile, True); // headers should be parsed via include
      end else
        InternalParse(FileName, True, Stream); // or from stream
      fFilesToScan.Clear;
      fPendingDeclarations.Clear; // should be empty anyways
      ReProcessInheritance; // account for inherited statements that have dissappeared
    finally
      if Assigned(fOnEndParsing) then
        fOnEndParsing(Self, 1);
    end;
    if UpdateView then
      if Assigned(fOnUpdate) then
        fOnUpdate(Self);
  finally
    fParsing:=False;
  end;
end;

procedure TCppParser.InvalidateFile(const FileName: AnsiString);
var
  I: integer;
  P: PFileIncludes;
  Node, NextNode: PStatementNode;
  Statement: PStatement;
begin
  if Filename = '' then
    Exit;

  DeleteTemporaries; // do it before deleting invalid nodes, in case some temp nodes deleted by invalid and cause error (redelete) 
  // delete statements of file
  Node := fStatementList.FirstNode;
  while Assigned(Node) do begin
    Statement := Node^.Data;
    NextNode := Node^.NextNode; // parent classname is encountered first

    // Is this statement part of this file? Remove
    if SameText(Statement^._FileName, FileName) or SameText(Statement^._DefinitionFileName, FileName) then begin
      if Statement^._Kind = skClass then // only classes have inheritance
        fInvalidatedStatements.Add(Statement);

      fMacroDefines.Remove(Statement);
      fStatementList.Delete(Node);
    end;
    Node := NextNode;
  end;

  // delete it from scannedfiles
  I := fScannedFiles.IndexOf(FileName);
  if I <> -1 then
    fScannedFiles.Delete(I);

  // remove its include files list
  P := FindFileIncludes(FileName, True);
  if Assigned(P) then begin
    fPreprocessor.InvalidDefinesInFile(FileName);
    PFileIncludes(P)^.IncludeFiles.Free;
    Dispose(PFileIncludes(P));
  end;

  //Statements.DumpTo('f:\\after.txt');
end;

procedure TCppParser.ReProcessInheritance;
var
  Node: PStatementNode;
  Statement, InvalidatedStatement: PStatement;
  I: integer;
  sl: TStringList;
begin

  // after reparsing a file, we have to reprocess inheritance,
  // because by invalidating the file, we might have deleted
  // some Statements that were inherited by other, valid, statements.
  // we need to re-adjust the IDs now...

  if fInvalidatedStatements.Count = 0 then
    Exit;
  sl := TStringList.Create;
  try
    sl.Sorted := True;
    sl.Duplicates := dupIgnore;

    // Create a list of files that contain invalidated IDs that are inherited from
    Node := fStatementList.FirstNode;
    while Assigned(Node) do begin
      Statement := Node^.Data;

      // Does this statement inherit from any invalidated statement?
      for I := 0 to fInvalidatedStatements.Count - 1 do begin
        InvalidatedStatement := fInvalidatedStatements[i];
        if Assigned(Statement._InheritanceList) and (Statement._InheritanceList.IndexOf(InvalidatedStatement) <> -1) then
          begin
          sl.Add(Statement^._FileName);
          break; // don't bother checking other invalidated statements
        end;
      end;
      Node := Node^.NextNode;
    end;

    // Reparse every file that contains invalidated IDs
    for I := 0 to sl.Count - 1 do
      ParseFile(sl[I], fProjectFiles.IndexOf(sl[I]) <> -1, False, False);
    //InternalParse(sl[I], True, nil); // TODO: do not notify user
  finally
    sl.Free;
  end;
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
end;

procedure TCppParser.GetClassesList(var List: TStringList);
var
  Node: PStatementNode;
  Statement: PStatement;
begin
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
end;

function TCppParser.FindAndScanBlockAt(const Filename: AnsiString; Row: integer; Stream: TMemoryStream): PStatement;
  function GetFuncStartLine(Index, StartLine: integer): integer;
  begin
    Result := Index;

    // Keep advancing until we find the start line
    while Index < fTokenizer.Tokens.Count do begin
      if fTokenizer[Index]^.Line = StartLine then begin

        // Find the opening brace from here
        while (Index < fTokenizer.Tokens.Count) and (fTokenizer[Index]^.Text[1] <> '{') do
          Inc(Index);

        // Found it before the file stopped? Yay
        if (Index < fTokenizer.Tokens.Count) and (fTokenizer[Index]^.Text[1] = '{') then begin
          Result := Index;
          Break;
        end;
      end;
      Inc(Index);
    end;
  end;
  function GetFuncEndLine(Index: integer): integer; // basic brace skipper
  var
    Level: integer;
  begin
    Level := 0; // when this goes negative, we 're there (we have skipped the opening brace already)
    while (Index < fTokenizer.Tokens.Count) and (Level >= 0) do begin
      if fTokenizer[Index]^.Text[1] = '{' then
        Inc(Level)
      else if fTokenizer[Index]^.Text[1] = '}' then
        Dec(Level);
      Inc(Index);
    end;
    Result := Index;
  end;
var
  ClosestLine, FuncStartIndex, FuncEndIndex: integer;
  Node: PStatementNode;
  Statement, ClosestStatement, ParentStatement: PStatement;
  InsideBody: Boolean;
begin
  Result := nil;
  if fParsing then
    Exit;
  if (fTokenizer = nil) or (fPreprocessor = nil) then
    Exit;
  DeleteTemporaries;
  ClosestLine := -1;
  ClosestStatement := nil;
  InsideBody := False;

  // Search for the current function/class we are pointing at
  Node := fStatementList.FirstNode;
  while Assigned(Node) do begin
    Statement := Node^.Data;
    case Statement^._Kind of
      skClass: begin
          if SameFileName(Statement^._FileName, FileName) then
            if (Statement^._Line <= Row) and (Statement^._Line > ClosestLine) then begin
              ClosestStatement := Statement;
              ClosestLine := Statement^._Line;
              InsideBody := Statement^._Line < Row;
            end;
        end;
      skFunction, skConstructor, skDestructor: begin
          if SameFileName(Statement^._FileName, Filename) then begin
          // Check definition
            if Statement^._HasDefinition and (Statement^._DefinitionLine <= Row) and (Statement^._DefinitionLine > ClosestLine) then begin
              ClosestStatement := Statement;
              ClosestLine := Statement^._DefinitionLine;
              InsideBody := Statement^._Line < Row;
            end else if (Statement^._Line <= Row) and (Statement^._Line > ClosestLine) then begin
              // Check declaration
              ClosestStatement := Statement;
              ClosestLine := Statement^._Line;
              InsideBody := True; // no body, so assume true
            end;
          end;
        end;
    end;
    Node := Node^.NextNode;
  end;

  // We have found the function or class body we are in
  if Assigned(ClosestStatement) then begin

    // For classes, the line with the class keyword on it belongs to the parent
    if ClosestStatement^._Kind = skClass then begin
      if not InsideBody then begin // Hovering above a class name
        ParentStatement := ClosestStatement^._Parent;
      end else begin // inside class body
        ParentStatement := ClosestStatement; // class
      end;

      // For functions, it does not
    end else begin // it's a function
      ParentStatement := ClosestStatement^._Parent; // class::function
    end;

    // The result is the class the function belongs to or the class body we're in
    Result := ParentStatement;

    // Scan the function definition body if we're inside a function
    if (ClosestStatement^._Kind in [skFunction, skConstructor, skDestructor]) and (ClosestStatement^._HasDefinition) and
      (ClosestStatement^._DefinitionLine = ClosestLine) then begin

      if not fLocked then begin
        // Preprocess the stream that contains the latest version of the current file (not on disk)
        fPreprocessor.SetIncludesList(fIncludesList);
        fPreprocessor.SetIncludePaths(fIncludePaths);
        fPreprocessor.SetProjectIncludePaths(fProjectIncludePaths);
        fPreprocessor.SetScannedFileList(fScannedFiles);
        fPreprocessor.SetScanOptions(fParseGlobalHeaders, fParseLocalHeaders);
        fPreprocessor.PreProcessStream(FileName, Stream);
        // Tokenize the stream so we can find the start and end of the function body
        fTokenizer.TokenizeBuffer(PAnsiChar(fPreprocessor.Result));
      end;

      // Find start of the function block and start from the opening brace
      FuncStartIndex := GetFuncStartLine(0, ClosestLine);

      // Now find the end of the function block and check that the Row is still in scope
      FuncEndIndex := GetFuncEndLine(FuncStartIndex + 1);

      // if we 're past the end or before the start of function or class body, we are not in the scope...
      if (Row > fTokenizer[FuncEndIndex - 1]^.Line) or (Row < fTokenizer[FuncStartIndex]^.Line) then begin
        Result := nil;
        Exit;
      end;

      // Set current file manually because we aren't parsing whole files
      fCurrentFile := Filename;
      fIsSystemHeader := IsSystemHeaderFile(fCurrentFile);
      fIsProjectFile := fProjectFiles.IndexOf(fCurrentFile) <> -1;
      fIsHeader := IsHfile(fCurrentFile);

      // We've found the function body. Scan it
      fIndex := FuncStartIndex;
      fClassScope := scsNone;
      fLaterScanning := True;
      repeat
        if fTokenizer[fIndex]^.Text[1] = '{' then begin
          AddSoloClassLevel(nil);
          Inc(fIndex);
        end else if fTokenizer[fIndex]^.Text[1] = '}' then begin
          RemoveClassLevel;
          Inc(fIndex);
          if fCurrentClass.Count = 0 then
            Break; // we've gone out of scope
        end else if CheckForPreprocessor then begin
          HandlePreprocessor;
        end else if CheckForKeyword then begin
          HandleKeyword;
        end else if CheckForEnum then begin
          HandleEnum;
        end else if CheckForVar then begin
          HandleVar;
        end else
          Inc(fIndex);

        CheckForSkipStatement;
      until (fIndex >= fTokenizer.Tokens.Count) or (fIndex >= FuncEndIndex);
      // add the all-important "this" pointer as a local variable
      if Assigned(ParentStatement) then begin
        AddStatement(
          ParentStatement,
          Filename,
          '',
          ParentStatement^._Command + '*',
          'this',
          '',
          '',
          ParentStatement^._DefinitionLine + 1,
          skVariable,
          ssClassLocal,
          scsPrivate,
          False,
          True,
          nil,
          False);
      end;

      // Try to use arglist which includes names (implementation, not declaration)
      ScanMethodArgs(
        ClosestStatement^._Args,
        ClosestStatement^._DefinitionFileName,
        ClosestStatement^._DefinitionLine);

      // Everything scanned before this point should be removed
      fLaterScanning := False;
    end;
  end;
  Result := ClosestStatement;
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

function TCppParser.PrettyPrintStatement(Statement: PStatement): AnsiString;
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
    while Assigned(WalkStatement^._Parent) do begin
      Result := WalkStatement^._Parent^._Command + '::' + Result;
      WalkStatement := WalkStatement^._Parent;
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
  Result := '';
  if Statement^._HintText <> '' then begin
    Result := Statement^._HintText;
  end else begin
    case Statement^._Kind of
      skFunction,
        skVariable,
        skClass: begin
          Result := GetScopePrefix; // public
          Result := Result + Statement^._Type + ' '; // void
          Result := Result + GetParentPrefix; // A::B::C::
          Result := Result + Statement^._Command; // Bar
          Result := Result + GetArgsSuffix; // (int a)
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
      skEnum: begin
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
end;

procedure TCppParser.FillListOfFunctions(const Full: AnsiString; List: TStringList);
var
  Node: PStatementNode;
  Statement: PStatement;
begin
  List.Clear;
  // Tweaked for specific use by CodeToolTip. Also avoids AnsiString compares whenever possible
  Node := fStatementList.LastNode; // Prefer user declared names
  while Assigned(Node) do begin
    Statement := Node^.Data;
    if Statement^._Kind in [skFunction, skConstructor, skDestructor] then begin

      // Also add Win32 Ansi/Wide variants...
      if SameStr(Full, Statement^._Command) or
        SameStr(Full + 'A', Statement^._Command) or
        SameStr(Full + 'W', Statement^._Command) then begin
        List.Add(PrettyPrintStatement(Statement));
      end;
    end;
    Node := Node^.PrevNode;
  end;
end;

function TCppParser.FindTypeDefinitionOf(const aType: AnsiString; CurrentClass: PStatement): PStatement;
var
  //Node: PStatementNode;
  Statement: PStatement;
  position: integer;
  s: AnsiString;
  i:integer;
  Children: TList;
begin
  Result := nil;
  if fParsing then
    Exit;
  // Remove pointer stuff from type
  s := aType; // 'Type' is a keyword
  position := Length(s);
  while (position > 0) and (s[position] in ['*', '&']) do
    Dec(position);
  if position <> Length(s) then
    Delete(s, position + 1, Length(s) - 1);

  // Strip template stuff
  position := Pos('<', s);
  if position > 0 then
    Delete(s, position, MaxInt);

  // Use last word only (strip 'const', 'static', etc)
  position := LastPos(' ', s);
  if position > 0 then
    Delete(s, 1, position);

  // Seach them
  // TODO: type definitions can have scope too
  while True do begin
    Children := Statements.GetChildrenStatements(CurrentClass);
    if (Assigned(Children)) then begin
      for i:=0 to Children.Count-1 do begin
        Statement:=PStatement(Children[i]);
        if Statement^._Kind = skClass then begin // these have type 'class'
          // We have found the statement of the type directly
          if SameStr(Statement^._Command, s) then begin
            Result := Statement; // 'class foo'
            Exit;
          end;
        end else if Statement^._Kind = skTypedef then begin
          // We have found a variable with the same name, search for type
          if SameStr(Statement^._Command, s) then begin
            if not SameStr(aType, Statement^._Type) then // prevent infinite loop
              Result := FindTypeDefinitionOf(Statement^._Type, CurrentClass)
            else
              Result := Statement; // stop walking the trail here
            if Result = nil then // found end of typedef trail, return result
              Result := Statement;
            Exit;
          end;
        end;
      end;
    end;
    if not assigned(CurrentClass) then
      break;
    CurrentClass := CurrentClass^._Parent;
  end;
  Result := nil;
end;

function TCppParser.FindVariableOf(const Phrase: AnsiString; CurrentClass: PStatement): PStatement;
var
  Statement: PStatement;
  GlobalStatement: PStatement;
  Children:TList;
  i:integer;
begin
  Result := nil;
  if fParsing then
    Exit;
  GlobalStatement:=nil;
  // Check local variables
  Children := Statements.GetChildrenStatements(nil);
  if Assigned(Children) then begin
    for i:=0 to Children.Count-1 do
    begin
      Statement := PStatement(Children[i]);
      // Local scope variables (includes function arguments)
      if (Statement^._Scope = ssLocal) then begin
        if SameStr(Statement^._Command, Phrase) then begin
          result := Statement;
          Exit;
        end;
      end;
      // found a Global define, cache it but not use now;
      if (Statement^._Scope = ssGlobal) then begin
        if SameStr(Statement^._Command, Phrase) then begin
          GlobalStatement := Statement;
        end;
      end;
    end;
  end;

    // Then, assume the variable belongs to the current scope/class, if there is one
  if Assigned(CurrentClass) then begin
    Repeat
      if CurrentClass._Kind in [skClass] then begin
        Children := Statements.GetChildrenStatements(CurrentClass);
        if Assigned(Children) then begin
          for i:=0 to Children.Count-1 do begin
            Statement := PStatement(Children[i]);
            // Class members
            if (Statement^._Scope = ssClassLocal) then begin
              if SameStr(Statement^._Command, Phrase) then begin
                result := Statement;
                Exit;
              end;
            end
          end;
        end;
      end;
      CurrentClass := CurrentClass^._Parent;
    Until (not assigned(CurrentClass));
  end;

  Result := GlobalStatement;
end;

function TCppParser.FindStatementOf(Phrase: AnsiString; CurrentClass: PStatement): PStatement;
var
  //Node: PStatementNode;
  ParentWord, MemberWord, OperatorToken: AnsiString;
  Statement, MemberStatement, TypedefStatement, VariableStatement, CurrentClassType: PStatement;
  i: integer;
  Children: TList;
begin
  Result := nil;
  if fParsing then
    Exit;
  CurrentClassType := CurrentClass;
  while assigned(CurrentClassType) and not (CurrentClassType._Kind = skClass) do begin
    CurrentClassType:=CurrentClassType^._Parent;
  end;

  // Get the FIRST class and member, surrounding the FIRST operator
  ParentWord := GetClass(Phrase);
  OperatorToken := GetOperator(Phrase);
  MemberWord := GetMember(Phrase);

  // First, assume the parentword is a type (type names have more priority than variables)
  // For example, find "class Foo" or "typedef Foo"
  TypedefStatement := FindTypeDefinitionOf(ParentWord, CurrentClass);
  if Assigned(TypedefStatement) then
    Result := TypedefStatement;

  // If it was not a type, check if it was a variable name
  if not Assigned(TypedefStatement) then begin
    VariableStatement := FindVariableOf(ParentWord, CurrentClass);

    // We have found a variable with name "Phrase"
    if Assigned(VariableStatement) then begin
      // If we do not need to find children of this variable, stop here
      if OperatorToken = '' then begin
        Result := VariableStatement;
        Exit;

        // We need to find children of this variable.
        // What we need to find now is the type of the variable
      end else begin
        if VariableStatement^._Kind = skClass then
          TypedefStatement := VariableStatement // a class statement is equal to its type
        else begin
          TypedefStatement := FindTypeDefinitionOf(VariableStatement^._Type, CurrentClassType);
        end;

        // If we cannot find the type, stop here
        if not Assigned(TypedefStatement) then begin
          Result := VariableStatement;
          Exit;
        end;
      end;
    end;
  end;

  // Walk the chain of operators
  while (MemberWord <> '') do begin
    MemberStatement := nil;

    // Add members of this type

    Children := Statements.GetChildrenStatements(TypedefStatement);
    if assigned(Children) then begin
      for i:=0 to Children.Count-1 do begin
        Statement := PStatement(Children[i]);
        if SameStr(Statement^._Command, MemberWord) then begin
          MemberStatement := Statement;
          break; // there can be only one with an equal name
        end;
      end;
    end;
    // Child not found. Stop searching
    if not Assigned(MemberStatement) then
      break;
    Result := MemberStatement; // otherwise, continue

    // next operator
    Delete(Phrase, 1, Length(ParentWord) + Length(OperatorToken));

    // Get the NEXT member, surrounding the next operator
    ParentWord := GetClass(Phrase);
    OperatorToken := GetOperator(Phrase);
    MemberWord := GetMember(Phrase);

    // Don't bother finding types
    if MemberWord = '' then
      break;

    // At this point, we have a list of statements that conform to the a(operator)b demand.
    // Now make these statements "a(operator)b" the parents, so we can use them as filters again
    if MemberStatement^._Kind = skClass then
      TypedefStatement := MemberStatement // a class statement is equal to its type
    else
      TypedefStatement := FindTypeDefinitionOf(MemberStatement^._Type, CurrentClass);
    if not Assigned(TypedefStatement) then
      break;
  end;
end;

function TCppParser.FindStatementOf(FileName, Phrase: AnsiString; Row: integer; Stream: TMemoryStream): PStatement;
begin
  Result := FindStatementOf(Phrase, FindAndScanBlockAt(FileName, Row, Stream));
end;

procedure TCppParser.DeleteTemporaries;
var
  node: PStatementNode;
  i : integer;
begin
  // Remove every temp statement
  for i := 0 to fTempNodes.Count -1 do
  begin
    node := PStatementNode(fTempNodes[i]);
    fMacroDefines.Remove(Node^.Data);
    fStatementList.Delete(Node);
  end;
  fTempNodes.Clear;
end;

procedure TCppParser.ScanMethodArgs(const ArgStr: AnsiString; const Filename: AnsiString; Line: Integer);
var
  I, ParamStart, SpacePos, BracePos: integer;
  S: AnsiString;
begin

  // Split up argument string by ,
  I := 2; // assume it starts with ( and ends with )
  ParamStart := I;

  while I <= Length(ArgStr) do begin
    if (ArgStr[i] = ',') or ((I = Length(ArgStr)) and (ArgStr[i] = ')')) then begin

      // We've found "int* a" for example
      S := Trim(Copy(ArgStr, ParamStart, I - ParamStart));

      // Can be a function pointer. If so, scan after last )
      BracePos := LastPos(')', S);
      if BracePos > 0 then // it's a function pointer...
        SpacePos := LastPos(' ', Copy(S, BracePos, MaxInt)) // start search at brace
      else
        SpacePos := LastPos(' ', S); // Cut up at last space

      if SpacePos > 0 then begin
        AddStatement(
          nil,
          Filename,
          '', // do not override hint
          Copy(S, 1, SpacePos - 1), // 'int*'
          Copy(S, SpacePos + 1, MaxInt), // a
          '',
          '',
          Line,
          skVariable,
          ssLocal,
          scsNone,
          False,
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
  Result := nil;
  for I := 0 to fIncludesList.Count - 1 do
    if SameText(PFileIncludes(fIncludesList[I])^.BaseFile, Filename) then begin
      Result := PFileIncludes(fIncludesList[I]);
      if DeleteIt then
        fIncludesList.Delete(I);
      Break;
    end;
end;

function TCppParser.IsCfile(const Filename: AnsiString): boolean;
begin
  result := cbutils.IsCfile(FileName);
end;

function TCppParser.IsHfile(const Filename: AnsiString): boolean;
begin
  result := cbutils.IsHfile(Filename);
end;

procedure TCppParser.GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
begin
  cbutils.GetSourcePair(FName, CFile, HFile);
end;
{
procedure TCppParser.GetFileIncludes(const Filename: AnsiString; var List: TStringList);

  procedure RecursiveFind(const FileName: AnsiString);
  var
    I: integer;
    P: PFileIncludes;
    sl: TStrings;
  begin
    if FileName = '' then
      Exit;
    List.Add(FileName);

    // Find the files this file includes
    P := FindFileIncludes(FileName);
    if Assigned(P) then begin

      // recursively search included files
      sl := TStringList.Create;
      try
        // For each file this file includes, perform the same trick
        sl.CommaText := P^.IncludeFiles;
        for I := 0 to sl.Count - 2 do // Last one is always an empty item
          if FastIndexOf(List, sl[I]) = -1 then
            RecursiveFind(sl[I]);
      finally
        sl.Free;
      end;
    end;
  end;
begin
  List.Clear;
  List.Sorted := false;
  RecursiveFind(Filename);
end;
}
//Since we have save all include files info, don't need to recursive find anymore
procedure TCppParser.GetFileIncludes(const Filename: AnsiString; var List: TStringList);
var
  I: integer;
  P: PFileIncludes;
  sl: TStrings;
begin
  if FileName = '' then
    Exit;
  List.Clear;
  if fParsing then
    Exit;
  List.Sorted := false;
  List.Add(FileName);

  P := FindFileIncludes(FileName);
  if Assigned(P) then begin
    sl := P^.IncludeFiles;
    for I := 0 to sl.Count - 1 do // Last one is always an empty item
        List.Add(sl[I]);
  end;
  List.Sorted := True;
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
  node:PStatementNode;
begin
  Result := new(PStatement);
  with Result^ do begin
    _Parent := derived;
    _HintText := inherit^._HintText;
      _Type := inherit^._Type;
      _Command := inherit^._Command;
      _Args := inherit^._Args;
      _Value := inherit^._Value;
      _Kind := inherit^._Kind;
      if Assigned(inherit^._InheritanceList) then begin
      // we should copy it, or we will got trouble when clear StatementList
        _InheritanceList := TList.Create;
        _InheritanceList.Assign(inherit^._InheritanceList);
      end else
        _InheritanceList := nil;
      _Scope := inherit^._Scope;
      _ClassScope := access;
      _HasDefinition := inherit^._HasDefinition;
      _Line := inherit^._Line;
      _DefinitionLine := inherit^._DefinitionLine;
      _FileName := inherit^._FileName;
      _DefinitionFileName := inherit^._DefinitionFileName;
      _Temporary := derived^._Temporary; // true if it's added by a function body scan
      _InProject := derived^._InProject;
      _InSystemHeader := derived^._InSystemHeader;
      _Children := nil; //Todo: inner class inheritance?
      _Friends := nil; // Friends are not inherited;
      _Static := inherit^._Static;
      _Inherited:=True;
    end;
    node:=fStatementList.Add(Result);
    if Result^._Temporary then
      fTempNodes.Add(node);
end;

procedure TCppParser.Freeze(FileName:AnsiString;Stream: TMemoryStream);
begin
  // Preprocess the stream that contains the latest version of the current file (not on disk)
  fPreprocessor.SetIncludesList(fIncludesList);
  fPreprocessor.SetIncludePaths(fIncludePaths);
  fPreprocessor.SetProjectIncludePaths(fProjectIncludePaths);
  fPreprocessor.SetScannedFileList(fScannedFiles);
  fPreprocessor.SetScanOptions(fParseGlobalHeaders, fParseLocalHeaders);
  fPreprocessor.PreProcessStream(FileName, Stream);

  // Tokenize the stream so we can find the start and end of the function body
  fTokenizer.TokenizeBuffer(PAnsiChar(fPreprocessor.Result));
  fLocked := True;
end;

procedure TCppParser.UnFreeze();
begin
  fLocked := False;
end;

end.

