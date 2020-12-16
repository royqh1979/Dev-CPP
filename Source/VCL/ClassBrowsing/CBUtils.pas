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

unit CBUtils;

interface

uses
  SysUtils, StrUtils, Classes, IniFiles, intList;

const
  HeaderExts: array[0..6] of AnsiString = ('.h', '.hpp', '.rh', '.hh', '.hxx', '.inl', '');
  SourceExts: array[0..5] of AnsiString = ('.c', '.cpp', '.cc', '.cxx', '.c++', '.cp');
  BackColor = 0;
  ForeColor = 1;
  FunctionColor = 2;
  ClassColor = 3;
  VarColor = 4;
  NamespaceColor = 5;
  TypedefColor = 6;
  PreprocessorColor = 7;
  EnumColor = 8;
  SelectedBackColor = 9;
  SelectedForeColor = 10;
  InheritedColor = 11;
  KeywordColor = 12;

var
  CppKeywordsList:TStringList;
  
type

  { TStringList is case insensitive by default}
  TDevStringList = class(TStringList)
  protected
    function CompareStrings(const S1, S2: string): Integer; override;
  end;

  TDevStringHash = class(TStringHash)
  public
    function FindItem(const Key: string): PPHashItem;
    procedure RemoveItem(const Key: string; const Value:integer);
  end;

  PCodeIns = ^TCodeIns;
  TCodeIns = record
    Caption: AnsiString; //Name
    Prefix: AnsiString; //Prefix used in code suggestion
    Code: AnsiString;  //Code body
    Desc: AnsiString;  //Description
    Section: integer;  //Section in the menu
  end;
  
  //macro define
  PDefine = ^TDefine;
  TDefine = record
    Name: AnsiString;
    Args: AnsiString;
    Value: AnsiString;
    FileName: AnsiString;
    IsMultiLine: boolean; // if true the expanded macro will span multiline
    HardCoded: boolean; // if true, don't free memory (points to hard defines)
    ArgList: TIntList; // args list to format values
    FormatValue: String;  // format template to format values
  end;


  TSkipType = (
    skItself,  // skip itself
    skToSemicolon, // skip to ;
    skToColon, // skip to :
    skToRightParenthesis, // skip to )
    skToLeftBrace,// Skip to {
    skToRightBrace, // skip to }
    skNone // It's a keyword but don't process here
  );


  TStatementKind = (
    skPreprocessor,
    skEnum,
    skTypedef,
    skClass,
    skFunction,
    skConstructor,
    skDestructor,
    skVariable,
    skNamespace,
    skNamespaceAlias,
    skBlock,
    skUserCodeIn,  // user code template
    skKeyword, // keywords
    skUnknown
    );
  TStatementKindSet = set of TStatementKind;

  TStatementScope = (
    ssGlobal,
    ssLocal,
    ssClassLocal
    );

  TStatementClassScope = (
    scsNone,
    scsPrivate,
    scsProtected,
    scsPublic
    );

  TOperatorType = (
    otArrow,
    otDot,
    otDColon,
    otOther
  );

  PStatement = ^TStatement;
  TStatement = record
    _ParentScope: PStatement; // parent class/struct/namespace scope
    _HintText: AnsiString; // text to force display when using PrettyPrintStatement
    _Type: AnsiString; // type "int"
    _Command: AnsiString; // identifier/name of statement "foo"
    _Args: AnsiString; // args "(int a,float b)"
    _Value: AnsiString; // Used for macro defines, "100" in "#defin COUNT 100"
    _Kind: TStatementKind; // kind of statement class/variable/function/etc
    _InheritanceList: TList; // list of pstatements this one inherits from, can be nil
    _Scope: TStatementScope; // global/local/classlocal
    _ClassScope: TStatementClassScope; // protected/private/public
    _HasDefinition: boolean; // definiton line/filename is valid
    _Line: integer; // declaration
    _DefinitionLine: integer; // definition
    _FileName: AnsiString; // declaration
    _DefinitionFileName: AnsiString; // definition
    _InProject: boolean; // statement in project
    _InSystemHeader: boolean; // statement in system header (#include <>)
    _Children: TList; // Children Statement to speedup search
    _ChildrenIndex: TDevStringHash; // children statements index to speedup search
    _Friends: TStringHash; // friend class / functions
    _Static: boolean; // static function / variable
    _Inherited: boolean; // inherted member;
    _FullName: AnsiString; // fullname(including class and namespace)
    _Usings: TStringList;
    _Node: Pointer;    // the Node TStatementList used to save this statement
    _UsageCount : integer; //Usage Count, used by TCodeCompletion
    _FreqTop: integer; // Usage Count Rank, used by TCodeCompletion
    _NoNameArgs: AnsiString; // Args without name
  end;

  PUsingNamespace =^TUsingNamespace;
  TUsingNamespace = record
    _namespace : TStringList; // List['std','foo'] for using namespace std::foo;
    _Filename: AnsiString;
    _Line: integer;
    _EndLine: integer;
    _FromHeader: boolean;
  end;

  TProgressEvent = procedure(Sender: TObject; const FileName: AnsiString; Total, Current: integer) of object;
  TProgressEndEvent = procedure(Sender: TObject; Total: integer) of object;

  PIncompleteClass = ^TIncompleteClass;
  TIncompleteClass = record
    statement:PStatement;
    count: integer;
  end;

  PFileIncludes = ^TFileIncludes;
  TFileIncludes = record
    BaseFile: AnsiString;
    IncludeFiles: TStringList; // "file","file" etc
    Usings: TDevStringList; // namespaces it usings
    Statements: TList; // List<Pointer,PStatement> , but we don't save temporary statements
    StatementsIndex: TDevStringHash; // List<Pionter, PStatement>
    DeclaredStatements: TList; // List<PStatement> statement declared in this file
    Scopes: TIntList; // List<Int,PStatement> int is start line of the statement scope
  end;

const
  ScopeTypeKinds : TStatementKindSet = [skClass,skNamespace,skFunction,skConstructor,skDestructor];
var
  CppKeywords : TStringHash;
  CppTypeKeywords : TStringHash;
  STLPointers : TStringHash;
  STLContainers: TStringHash;
  STLElementMethods: TStringHash;

  // These functions are about six times faster than the locale sensitive AnsiX() versions

function StartsStr(const subtext, text: AnsiString): boolean;
function StartsText(const subtext, text: AnsiString): boolean;

function SameStr(const s1, s2: AnsiString): boolean;
function SameText(const s1, s2: AnsiString): boolean;

function EndsStr(const subtext, text: AnsiString): boolean;
function EndsText(const subtext, text: AnsiString): boolean;

function ContainsStr(const text, subtext: AnsiString): boolean;
function ContainsText(const text, subtext: AnsiString): boolean;

// Same as StringReplace, but only replace first OldPattern (a lot faster)
function ReplaceFirstStr(const S, OldPattern, NewPattern: AnsiString): AnsiString;
function ReplaceFirstText(const S, OldPattern, NewPattern: AnsiString): AnsiString;

// Reverse Pos() function
function LastPos(const SubStr, S: AnsiString): integer;

// Fast implementation of StringReplace which does not use AnsiX (MBCS ready) comparison
function FastStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;

// Fast implementation of IndexOf which does not use AnsiX comparison
function FastIndexOf(List: TStrings; const S: AnsiString): integer; overload;
function FastIndexOf(List: TStringlist; const S: AnsiString): integer; overload;
function FastIndexOf(List: TIntlist; const val: int64): integer; overload;

// Needed by Parser and Preprocessor (and class browser)
function IsSystemHeaderFile(const FileName: AnsiString; IncludePaths: TStringList): boolean;
function GetSystemHeaderFileName(const FileName: AnsiString; IncludePaths: TStringList): AnsiString; // <file.h>
function GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString): AnsiString;
// "file.h"
function GetHeaderFileName(const RelativeTo, Line: AnsiString; IncludePaths, ProjectIncludePaths: TStringList):
  AnsiString; // both
function IsCfile(const Filename: AnsiString): boolean;
function IsHfile(const Filename: AnsiString): boolean;
procedure GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
function IsIncludeLine(const Line: AnsiString): boolean;
function IsKeyword(const name:AnsiString): boolean;

function GetOperatorType(Phrase:AnsiString;index:integer):TOperatorType;

function IsAncestor(a:PStatement;b:PStatement):boolean;

implementation

function TDevStringList.CompareStrings(const S1, S2: string): Integer;
begin
  Result := AnsiCompareStr(S1, S2);
end;

function TDevStringHash.FindItem(const Key: string): PPHashItem;
begin
  Result:=self.Find(Key);
end;

procedure TDevStringHash.RemoveItem(const Key: string;const Value:integer);
var
  P: PHashItem;
  Prev: PPHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  while (P<>nil) and (P^.Value <> Value) do begin
    Prev:=@P^.Next;
    p:=Prev^;
    if (P <> nil) and (p^.Key<>Key) then
      Exit;
  end;
  if (P <> nil) then
  begin
    Prev^ := P^.Next;
    Dispose(P);
  end;
end;
function FastStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
var
  SearchStr, Patt, NewStr: AnsiString;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then begin
    SearchStr := UpperCase(S);
    Patt := UpperCase(OldPattern);
  end else begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function FastIndexOf(List: TStrings; const S: AnsiString): integer;
begin
  with List do begin
    for Result := 0 to Count - 1 do
      if CompareText(List[Result], S) = 0 then
        Exit;
    Result := -1;
  end;
end;

function FastIndexOf(List: TIntlist; const val: int64): integer;
begin
  Result := List.IndexOf(val);
end;


function FastIndexOf(List: TStringlist; const S: AnsiString): integer;
begin
  if not List.Sorted then
    Result := FastIndexOf(TStrings(List), S)
  else if not List.Find(S, Result) then
    Result := -1;
end;

function StartsStr(const subtext, text: AnsiString): boolean;
begin
  Result:=CompareMem(pChar(subText),pChar(text),Length(subtext));
  {
  Result := SameStr(subtext, Copy(text, 1, Length(subtext)));
  }
end;

function StartsText(const subtext, text: AnsiString): boolean;
begin
  Result := SameText(subtext, Copy(text, 1, Length(subtext)));
end;

function SameStr(const s1, s2: AnsiString): boolean;
begin
  Result := (CompareStr(s1, s2) = 0);
end;

function SameText(const s1, s2: AnsiString): boolean;
begin
  Result := (CompareText(s1, s2) = 0);
end;

function EndsStr(const subtext, text: AnsiString): boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(text) - Length(subtext) + 1;
  if (SubTextLocation > 0) and (subtext <> '') then
    Result := StrComp(Pointer(subtext), Pointer(@text[SubTextLocation])) = 0
  else
    Result := False;
end;

function EndsText(const subtext, text: AnsiString): boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(text) - Length(subtext) + 1;
  if (SubTextLocation > 0) and (subtext <> '') then
    Result := StrIComp(Pointer(subtext), Pointer(@text[SubTextLocation])) = 0
  else
    Result := False;
end;

function ContainsStr(const text, subtext: AnsiString): boolean;
begin
  Result := Pos(subtext, text) > 0;
end;

function ContainsText(const text, subtext: AnsiString): boolean;
begin
  Result := Pos(UpperCase(subtext), UpperCase(text)) > 0;
end;

function ReplaceFirstStr(const S, OldPattern, NewPattern: AnsiString): AnsiString;
var
  Offset: Integer;
begin
  Offset := Pos(OldPattern, S);
  if Offset = 0 then begin
    Result := S;
  end else begin
    // Copy the preceding stuff, append the new part, append old stuff after old pattern
    Result := Copy(S, 1, Offset - 1) + NewPattern + Copy(S, Offset + Length(OldPattern), MaxInt);
  end;
end;

function ReplaceFirstText(const S, OldPattern, NewPattern: AnsiString): AnsiString;
var
  Offset: Integer;
  UpperS, UpperOldPattern: string;
begin
  UpperS := UpperCase(S);
  UpperOldPattern := UpperCase(OldPattern);

  Offset := Pos(UpperOldPattern, UpperS);
  if Offset = 0 then begin
    Result := S;
  end else begin

    // Copy the preceding stuff, append the new part, append old stuff after old pattern
    Result := Copy(S, 1, Offset - 1) + NewPattern + Copy(S, Offset + Length(UpperOldPattern), MaxInt);
  end;
end;

function LastPos(const SubStr, s: AnsiString): integer;
begin
  result := Pos(ReverseString(SubStr), ReverseString(S));
  if result <> 0 then
    result := ((Length(S) - Length(SubStr)) + 1) - result + 1;
end;

function IsSystemHeaderFile(const FileName: AnsiString; IncludePaths: TStringList): boolean;
var
  FilePath: AnsiString;
  I: integer;
begin
  Result := false;
  if not assigned(IncludePaths) then
    Exit;

  // If it's a full file name, check if its directory is an include path
  if (Length(FileName) > 2) and (FileName[2] = ':') then begin // full file name
    Result := false;
    if FileExists(FileName) then begin // the file must exist
      FilePath := ExtractFileDir(FileName); // also extracts last \
      for I := 0 to IncludePaths.Count - 1 do begin
        if StartsText(IncludePaths[I],FilePath) then begin
          Result := true;
          Exit;
        end;
      end;
    end;

    // Not a full file name, check if it's in a include path
  end else begin
    for I := 0 to IncludePaths.Count - 1 do
      if FileExists(IncludePaths[I] + '\' + FileName) then begin
        Result := true;
        Exit;
      end;
  end;
end;

function IsIncludeLine(const Line: AnsiString): boolean;
var
  TrimmedLine: AnsiString;
begin
  Result := False;
  TrimmedLine := Trim(Line);
  if (Length(TrimmedLine) > 0) and (TrimmedLine[1] = '#') then begin // it's a preprocessor line
    if StartsStr('include', TrimLeft(Copy(TrimmedLine, 2, MaxInt))) then begin // the first word after # is 'include'
      Result := True;
    end;
  end;
end;

function GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString): AnsiString;
var
  Dir: AnsiString;
  s:AnsiString;
begin
  s := StringReplace(FileName,'/','\',[rfReplaceAll]);
//  Result := FileName;

  // Try to convert a C++ filename from cxxx to xxx.h (ignore std:: namespace versions)
  if StartsStr('c', s) and not ContainsStr(s, '.') then begin
    Delete(s, 1, 1);
    s := s + '.h';
  end;

  // Search local directory
  Dir := ExtractFilePath(RelativeTo);
  if FileExists(Dir + s) then begin // same dir as file
    Result := Dir + s;
    Exit;
  end;

  Result := '';
  {
  // Search project include directories
  for I := 0 to ProjectIncludePaths.Count - 1 do
    if FileExists(ProjectIncludePaths[I] + '\' + FileName) then begin
      Result := ProjectIncludePaths[I] + '\' + FileName;
      Exit;
    end;

  Result := FileName; // signifies failure
  }
end;

function GetSystemHeaderFileName(const FileName: AnsiString; IncludePaths: TStringList): AnsiString;
var
  I: integer;
  s:AnsiString;
begin
  if  not Assigned(IncludePaths) then begin
    Result :='';
    Exit;
  end;

  s := StringReplace(FileName,'/','\',[rfReplaceAll]);
//  Result := FileName;

  // Try to convert a C++ filename from cxxx to xxx.h (ignore std:: namespace versions)
  if StartsStr('c', s) and not ContainsStr(s, '.') then begin
    Delete(s, 1, 1);
    s := s + '.h';
  end;

  // Search compiler include directories
  for I := 0 to IncludePaths.Count - 1 do
    if FileExists(IncludePaths[I] + '\' + s) then begin
      Result := IncludePaths[I] + '\' + s;
      Exit;
    end;

  Result := ''; //not found, don't use it
end;


function GetHeaderFileName(const RelativeTo, Line: AnsiString; IncludePaths, ProjectIncludePaths: TStringList):
  AnsiString;
var
  OpenTokenPos, CloseTokenPos: integer;
  FileName : AnsiString;
begin
  Result := '';

  // Handle <>
  OpenTokenPos := Pos('<', Line);
  if OpenTokenPos > 0 then begin
    CloseTokenpos := Pos('>', Line);
    if CloseTokenPos > 0 then begin
      FileName := Copy(Line, OpenTokenPos + 1, CloseTokenPos - OpenTokenPos - 1);
      Result := GetSystemHeaderFileName(FileName, IncludePaths);
      if Result = '' then
        Result := GetSystemHeaderFileName(FileName, ProjectIncludePaths);
    end;
  end else begin

    // Try ""
    OpenTokenPos := Pos('"', Line);
    if OpenTokenPos > 0 then begin
      CloseTokenpos := Pos('"', Copy(Line, OpenTokenPos + 1, MaxInt));
      if CloseTokenPos > 0 then begin
        Inc(CloseTokenPos, OpenTokenPos);
        FileName := Copy(Line, OpenTokenPos + 1, CloseTokenPos - OpenTokenPos - 1);
        Result := GetLocalHeaderFileName(RelativeTo, FileName);
      end;
    end;
  end;
end;

function IsCfile(const Filename: AnsiString): boolean;
var
  ext: AnsiString;
  i: integer;
begin
  result := false;

  ext := LowerCase(ExtractFileExt(Filename));
  for I := Low(SourceExts) to High(SourceExts) do
    if ext = SourceExts[i] then begin
      result := true;
      Exit;
    end;
end;

function IsHfile(const Filename: AnsiString): boolean;
var
  ext: AnsiString;
  i: integer;
begin
  result := false;
  if FileName = '' then
    Exit;

  // Files without an extension can be headers too
  ext := LowerCase(ExtractFileExt(Filename));
  for I := Low(HeaderExts) to High(HeaderExts) do
    if ext = HeaderExts[i] then begin
      result := true;
      Exit;
    end;
end;

procedure GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
var
  i: integer;
begin
  if IsCfile(FName) then begin

    CFile := FName;
    HFile := '';

    // Find corresponding header
    for I := Low(HeaderExts) to High(HeaderExts) do
      if FileExists(ChangeFileExt(FName, HeaderExts[i])) then begin
        HFile := ChangeFileExt(FName, HeaderExts[i]);
        break;
      end;
  end else if IsHfile(FName) then begin

    HFile := FName;
    CFile := '';

    // Find corresponding source
    for I := Low(SourceExts) to High(SourceExts) do
      if FileExists(ChangeFileExt(FName, SourceExts[i])) then begin
        CFile := ChangeFileExt(FName, SourceExts[i]);
        break;
      end;
  end else begin
    CFile := FName;
    HFile := '';
  end;
end;

function IsKeyword(const name:AnsiString): boolean;
begin
  Result:= CppKeywords.ValueOf(name)>=0;
end;

function GetOperatorType(Phrase:AnsiString;index:integer):TOperatorType;
begin
  Result:=otOther;
  if index>Length(Phrase) then begin
    Exit;
  end;
  if (Phrase[index] = '.') then begin
    Result:=otDot;
    Exit;
  end;
  if (index+1)>Length(Phrase) then begin
    Exit;
  end;
  if (Phrase[index] = '-') and (Phrase[index+1] = '>') then begin
    Result:=otArrow;
    Exit;
  end;
  if (Phrase[index] = ':') and (Phrase[index+1] = ':') then begin
    Result:=otDColon;
    Exit;
  end;
end;

{
 Test if b is ancestor of a
}
function IsAncestor(a:PStatement;b:PStatement):boolean;
var
  i: integer;
  toVisit: TList;
  vis_id: integer;
  s: PStatement;
begin
  Result := False;
  if (not Assigned(a)) or (not Assigned(b)) then
    Exit;
  if not (b._Kind = skClass) or not (a._Kind = skClass) then
    Exit;
  toVisit := TList.Create;
  toVisit.Add(a);
  try
    vis_id :=0;
    repeat
      s:=toVisit[vis_id];
      if (s = b) then begin
        Result:= True;
        Exit;
      end;
      if Assigned(s^._InheritanceList) then begin
        for i :=0 to s^._InheritanceList.Count-1 do begin
          toVisit.Add(s^._InheritanceList[i]);
        end;
      end;
      inc(vis_id);
    Until vis_id >= toVisit.Count;
  finally
    toVisit.Free;
  end;
end;


initialization
begin

  CppTypeKeywords := TStringHash.Create();
  CppKeywords := TStringHash.Create();
  CppKeywordsList := TStringList.Create;
  STLContainers := TStringHash.Create();
  STLElementMethods := TStringHash.Create();
  STLPointers := TStringHash.Create();
  { we use TSkipType value to tell cpppaser how to handle this keyword }

  // skip itself
  CppKeywords.Add('and',Ord(skItself));
  CppKeywords.Add('and_eq',Ord(skItself));
  CppKeywords.Add('bitand',Ord(skItself));
  CppKeywords.Add('bitor',Ord(skItself));
  CppKeywords.Add('break',Ord(skItself));
  CppKeywords.Add('compl',Ord(skItself));
  CppKeywords.Add('constexpr',Ord(skItself));
  CppKeywords.Add('const_cast',Ord(skItself));
  CppKeywords.Add('continue',Ord(skItself));
  CppKeywords.Add('dynamic_cast',Ord(skItself));
  CppKeywords.Add('else',Ord(skItself));
  CppKeywords.Add('explicit',Ord(skItself));
  CppKeywords.Add('export',Ord(skItself));
  CppKeywords.Add('extern',Ord(skItself));
  CppKeywords.Add('false',Ord(skItself));
  //CppKeywords.Add('for',Ord(skItself));
  CppKeywords.Add('mutable',Ord(skItself));
  CppKeywords.Add('noexcept',Ord(skItself));
  CppKeywords.Add('not',Ord(skItself));
  CppKeywords.Add('not_eq',Ord(skItself));
  CppKeywords.Add('nullptr',Ord(skItself));
  CppKeywords.Add('or',Ord(skItself));
  CppKeywords.Add('or_eq',Ord(skItself));
  CppKeywords.Add('register',Ord(skItself));
  CppKeywords.Add('reinterpret_cast',Ord(skItself));
  CppKeywords.Add('static_assert',Ord(skItself));
  CppKeywords.Add('static_cast',Ord(skItself));
  CppKeywords.Add('template',Ord(skItself));
  CppKeywords.Add('this',Ord(skItself));
  CppKeywords.Add('thread_local',Ord(skItself));
  CppKeywords.Add('true',Ord(skItself));
  CppKeywords.Add('typename',Ord(skItself));
  CppKeywords.Add('virtual',Ord(skItself));
  CppKeywords.Add('volatile',Ord(skItself));
  CppKeywords.Add('xor',Ord(skItself));
  CppKeywords.Add('xor_eq',Ord(skItself));

  
  //CppKeywords.Add('catch',Ord(skItself));
  CppKeywords.Add('do',Ord(skItself));
  CppKeywords.Add('try',Ord(skItself));

  // Skip to ;
  CppKeywords.Add('delete',Ord(skToSemicolon));
  CppKeywords.Add('delete[]',Ord(skToSemicolon));
  CppKeywords.Add('goto',Ord(skToSemicolon));
  CppKeywords.Add('new',Ord(skToSemicolon));
  CppKeywords.Add('return',Ord(skToSemicolon));
  CppKeywords.Add('throw',Ord(skToSemicolon));
//  CppKeywords.Add('using',Ord(skToSemicolon)); //won't use it

  // Skip to :
  CppKeywords.Add('case',Ord(skToColon));
  CppKeywords.Add('default',Ord(skToColon));

  // Skip to )
  CppKeywords.Add('__attribute__',Ord(skToRightParenthesis));
  CppKeywords.Add('alignas',Ord(skToRightParenthesis));  // not right
  CppKeywords.Add('alignof',Ord(skToRightParenthesis));  // not right
  CppKeywords.Add('decltype',Ord(skToRightParenthesis)); // not right
  CppKeywords.Add('if',Ord(skToRightParenthesis));
  CppKeywords.Add('sizeof',Ord(skToRightParenthesis));
  CppKeywords.Add('switch',Ord(skToRightParenthesis));
  CppKeywords.Add('typeid',Ord(skToRightParenthesis));
  CppKeywords.Add('while',Ord(skToRightParenthesis));

  // Skip to }
  CppKeywords.Add('asm',Ord(skToRightBrace));
  //CppKeywords.Add('namespace',Ord(skToLeftBrace)); // won't process it
  // Skip to {

  // wont handle

  //Not supported yet
  CppKeywords.Add('atomic_cancel',Ord(skNone));
  CppKeywords.Add('atomic_commit',Ord(skNone));
  CppKeywords.Add('atomic_noexcept',Ord(skNone));
  CppKeywords.Add('concept',Ord(skNone));
  CppKeywords.Add('consteval',Ord(skNone));
  CppKeywords.Add('constinit',Ord(skNone));
  CppKeywords.Add('co_wait',Ord(skNone));
  CppKeywords.Add('co_return',Ord(skNone));
  CppKeywords.Add('co_yield',Ord(skNone));
  CppKeywords.Add('reflexpr',Ord(skNone));
  CppKeywords.Add('requires',Ord(skNone));

  // its a type
  CppKeywords.Add('auto',Ord(skNone));
  CppKeywords.Add('bool',Ord(skNone));
  CppKeywords.Add('char',Ord(skNone));
  CppKeywords.Add('char8_t',Ord(skNone));
  CppKeywords.Add('char16_t',Ord(skNone));
  CppKeywords.Add('char32_t',Ord(skNone));
  CppKeywords.Add('double',Ord(skNone));
  CppKeywords.Add('float',Ord(skNone));
  CppKeywords.Add('int',Ord(skNone));
  CppKeywords.Add('long',Ord(skNone));
  CppKeywords.Add('short',Ord(skNone));
  CppKeywords.Add('signed',Ord(skNone));
  CppKeywords.Add('unsigned',Ord(skNone));
  CppKeywords.Add('void',Ord(skNone));
  CppKeywords.Add('wchar_t',Ord(skNone));

  CppTypeKeywords.Add('auto',1);
  CppTypeKeywords.Add('bool',1);
  CppTypeKeywords.Add('char',1);
  CppTypeKeywords.Add('char8_t',1);
  CppTypeKeywords.Add('char16_t',1);
  CppTypeKeywords.Add('char32_t',1);
  CppTypeKeywords.Add('double',1);
  CppTypeKeywords.Add('float',1);
  CppTypeKeywords.Add('int',1);
  CppTypeKeywords.Add('long',1);
  CppTypeKeywords.Add('short',1);
  CppTypeKeywords.Add('signed',1);
  CppTypeKeywords.Add('unsigned',1);
  CppTypeKeywords.Add('void',1);
  CppTypeKeywords.Add('wchar_t',1);

  // it's part of type info
  CppKeywords.Add('const',Ord(skNone));
  CppKeywords.Add('inline',Ord(skItself));

  // handled elsewhere
  CppKeywords.Add('class',Ord(skNone));
  CppKeywords.Add('enum',Ord(skNone));
  CppKeywords.Add('friend',Ord(skNone));
  CppKeywords.Add('operator',Ord(skNone));
  CppKeywords.Add('private',Ord(skNone));
  CppKeywords.Add('protected',Ord(skNone));
  CppKeywords.Add('public',Ord(skNone));
  CppKeywords.Add('static',Ord(skNone));
  CppKeywords.Add('struct',Ord(skNone));
  CppKeywords.Add('typedef',Ord(skNone));
  CppKeywords.Add('union',Ord(skNone));
  // namespace
  CppKeywords.Add('namespace',Ord(skNone));
  CppKeywords.Add('using',Ord(skNone));

  CppKeywords.Add('for',Ord(skNone));
  CppKeywords.Add('catch',Ord(skNone));



  // nullptr is value
  CppKeywords.Add('nullptr',Ord(skNone));


  //CppKeywordsList

  CppKeywordsList.Add('and');
  CppKeywordsList.Add('and_eq');
  CppKeywordsList.Add('bitand');
  CppKeywordsList.Add('bitor');
  CppKeywordsList.Add('break');
  CppKeywordsList.Add('compl');
  CppKeywordsList.Add('constexpr');
  CppKeywordsList.Add('const_cast');
  CppKeywordsList.Add('continue');
  CppKeywordsList.Add('dynamic_cast');
  CppKeywordsList.Add('else');
  CppKeywordsList.Add('explicit');
  CppKeywordsList.Add('export');
  CppKeywordsList.Add('extern');
  CppKeywordsList.Add('false');
  CppKeywordsList.Add('mutable');
  CppKeywordsList.Add('noexcept');
  CppKeywordsList.Add('not');
  CppKeywordsList.Add('not_eq');
  CppKeywordsList.Add('nullptr');
  CppKeywordsList.Add('or');
  CppKeywordsList.Add('or_eq');
  CppKeywordsList.Add('register');
  CppKeywordsList.Add('reinterpret_cast');
  CppKeywordsList.Add('static_assert');
  CppKeywordsList.Add('static_cast');
  CppKeywordsList.Add('template');
//  CppKeywordsList.Add('this');
  CppKeywordsList.Add('thread_local');
  CppKeywordsList.Add('true');
  CppKeywordsList.Add('typename');
  CppKeywordsList.Add('virtual');
  CppKeywordsList.Add('volatile');
  CppKeywordsList.Add('xor');
  CppKeywordsList.Add('xor_eq');


  CppKeywordsList.Add('do');
  CppKeywordsList.Add('try');

  // Skip to ;
  CppKeywordsList.Add('delete');
  CppKeywordsList.Add('delete[]');
  CppKeywordsList.Add('goto');
  CppKeywordsList.Add('new');
  CppKeywordsList.Add('return');
  CppKeywordsList.Add('throw');

  // Skip to :
  CppKeywordsList.Add('case');
  CppKeywordsList.Add('default');

  // Skip to )
  CppKeywordsList.Add('__attribute__');
  CppKeywordsList.Add('alignas');  // not right
  CppKeywordsList.Add('alignof');  // not right
  CppKeywordsList.Add('decltype'); // not right
  CppKeywordsList.Add('if');
  CppKeywordsList.Add('sizeof');
  CppKeywordsList.Add('switch');
  CppKeywordsList.Add('typeid');
  CppKeywordsList.Add('while');

  // Skip to }
  CppKeywordsList.Add('asm');

  // wont handle

  //Not supported yet
  CppKeywordsList.Add('atomic_cancel');
  CppKeywordsList.Add('atomic_commit');
  CppKeywordsList.Add('atomic_noexcept');
  CppKeywordsList.Add('concept');
  CppKeywordsList.Add('consteval');
  CppKeywordsList.Add('constinit');
  CppKeywordsList.Add('co_wait');
  CppKeywordsList.Add('co_return');
  CppKeywordsList.Add('co_yield');
  CppKeywordsList.Add('reflexpr');
  CppKeywordsList.Add('requires');

  // its a type
  CppKeywordsList.Add('auto');
  CppKeywordsList.Add('bool');
  CppKeywordsList.Add('char');
  CppKeywordsList.Add('char8_t');
  CppKeywordsList.Add('char16_t');
  CppKeywordsList.Add('char32_t');
  CppKeywordsList.Add('double');
  CppKeywordsList.Add('float');
  CppKeywordsList.Add('int');
  CppKeywordsList.Add('long');
  CppKeywordsList.Add('short');
  CppKeywordsList.Add('signed');
  CppKeywordsList.Add('unsigned');
  CppKeywordsList.Add('void');
  CppKeywordsList.Add('wchar_t');


  // it's part of type info
  CppKeywordsList.Add('const');
  CppKeywordsList.Add('inline');

  // handled elsewhere
  CppKeywordsList.Add('class');
  CppKeywordsList.Add('enum');
  CppKeywordsList.Add('friend');
  CppKeywordsList.Add('operator');
  CppKeywordsList.Add('private');
  CppKeywordsList.Add('protected');
  CppKeywordsList.Add('public');
  CppKeywordsList.Add('static');
  CppKeywordsList.Add('struct');
  CppKeywordsList.Add('typedef');
  CppKeywordsList.Add('union');
  // namespace
  CppKeywordsList.Add('namespace');
  CppKeywordsList.Add('using');

  CppKeywordsList.Add('for');
  CppKeywordsList.Add('catch');

  // nullptr is value
  CppKeywordsList.Add('nullptr');

  {STL Containers}
  STLContainers.Add('std::array',1);
  STLContainers.Add('std::vector',1);
  STLContainers.Add('std::deque',1);
  STLContainers.Add('std::forward_list',1);
  STLContainers.Add('std::list',1);

  STLContainers.Add('std::set',1);
  STLContainers.Add('std::map',1);
  STLContainers.Add('std::multilist',1);
  STLContainers.Add('std::multimap',1);

  STLContainers.Add('std::unordered_set',1);
  STLContainers.Add('std::unordered_map',1);
  STLContainers.Add('std::unordered_multiset',1);
  STLContainers.Add('std::unordered_multimap',1);

  STLContainers.Add('std::stack',1);
  STLContainers.Add('std::queue',1);
  STLContainers.Add('std::priority_queue',1);

  STLContainers.Add('std::span',1);

  {STL element access methods}
  STLElementMethods.Add('at',1);
  STLElementMethods.Add('back',1);
  STLElementMethods.Add('front',1);
  STLElementMethods.Add('top',1);

  {STL pointers }
  STLPointers.Add('std::unique_ptr',1);
  STLPointers.Add('std::auto_ptr',1);
  STLPointers.Add('std::shared_ptr',1);
  STLPointers.Add('std::weak_ptr',1);
  STLPointers.Add('__gnu_cxx::__normal_iterator',1);
  STLPointers.Add('std::reverse_iterator',1);
  STLPointers.Add('std::iterator',1);    

end;

finalization
begin
  CppKeywords.Free;
  CppKeywordsList.Free;
  CppTypeKeywords.Free;
  STLContainers.Free;
  STLPointers.Free;
end;
end.

