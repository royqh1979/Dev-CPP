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
{$IFDEF WIN32}
  SysUtils, StrUtils, Classes, IniFiles;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, StrUtils;
{$ENDIF}

const
  HeaderExts: array[0..6] of String = ('.h', '.hpp', '.rh', '.hh', '.hxx', '.inl', '');
  SourceExts: array[0..5] of String = ('.c', '.cpp', '.cc', '.cxx', '.c++', '.cp');

type

  PCodeIns = ^TCodeIns;
  TCodeIns = record
    Caption: String; //Name
    Prefix: String; //Prefix used in code suggestion
    Code: String;  //Code body
    Desc: String;  //Description
    Section: integer;  //Section in the menu
  end;
  
  //macro define
  PDefine = ^TDefine;
  TDefine = record
    Name: String;
    Args: String;
    Value: String;
    FileName: String;
    IsMultiLine: boolean; // if true the expanded macro will span multiline
    HardCoded: boolean; // if true, don't free memory (points to hard defines)
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
    _HintText: String; // text to force display when using PrettyPrintStatement
    _Type: String; // type "int"
    _Command: String; // identifier/name of statement "foo"
    _Args: String; // args "(int a,float b)"
    _Value: String; // Used for macro defines, "100" in "#defin COUNT 100"
    _Kind: TStatementKind; // kind of statement class/variable/function/etc
    _InheritanceList: TList; // list of pstatements this one inherits from, can be nil
    _Scope: TStatementScope; // global/local/classlocal
    _ClassScope: TStatementClassScope; // protected/private/public
    _HasDefinition: boolean; // definiton line/filename is valid
    _Line: integer; // declaration
    _DefinitionLine: integer; // definition
    _FileName: String; // declaration
    _DefinitionFileName: String; // definition
    _Temporary: boolean; // statements to be deleted after parsing
    _InProject: boolean; // statement in project
    _InSystemHeader: boolean; // statement in system header (#include <>)
    _Children: TList; // Children Statement to speedup search
    _Friends: TStringHash; // friend class / functions
    _Static: boolean; // static function / variable
    _Inherited: boolean; // inherted member;
    _FullName: String; // fullname(including class and namespace)
    _Usings: TStringList;
    _Node: Pointer;
    _UsageCount : integer;
    _FreqTop: integer;
  end;

  PUsingNamespace =^TUsingNamespace;
  TUsingNamespace = record
    _namespace : TStringList; // List['std','foo'] for using namespace std::foo;
    _Filename: String;
    _Line: integer;
    _EndLine: integer;
    _FromHeader: boolean;
  end;

  TProgressEvent = procedure(Sender: TObject; const FileName: String; Total, Current: integer) of object;
  TProgressEndEvent = procedure(Sender: TObject; Total: integer) of object;

  { TStringList is case insensitive }
  TDevStringList = class(TStringList)
  protected
    function CompareStrings(const S1, S2: string): Integer; override;
  end;

  PFileIncludes = ^TFileIncludes;
  TFileIncludes = record
    BaseFile: String;
    IncludeFiles: TStringList; // "file","file" etc
    Usings: TDevStringList; // namespaces it usings
    Statements: TList; // List<PStatement> , but we don't save temporary statements
    DeclaredStatements: TList; // List<PStatement> statement declared in this file 
  end;

var
  CppKeywords : TStringHash;
  // These functions are about six times faster than the locale sensitive AnsiX() versions

function StartsStr(const subtext, text: String): boolean;
function StartsText(const subtext, text: String): boolean;

function SameStr(const s1, s2: String): boolean;
function SameText(const s1, s2: String): boolean;

function EndsStr(const subtext, text: String): boolean;
function EndsText(const subtext, text: String): boolean;

function ContainsStr(const text, subtext: String): boolean;
function ContainsText(const text, subtext: String): boolean;

// Same as StringReplace, but only replace first OldPattern (a lot faster)
function ReplaceFirstStr(const S, OldPattern, NewPattern: String): String;
function ReplaceFirstText(const S, OldPattern, NewPattern: String): String;

// Reverse Pos() function
function LastPos(const SubStr, S: String): integer;

// Fast implementation of StringReplace which does not use AnsiX (MBCS ready) comparison
function FastStringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;

// Fast implementation of IndexOf which does not use AnsiX comparison
function FastIndexOf(List: TStrings; const S: String): integer; overload;
function FastIndexOf(List: TStringlist; const S: String): integer; overload;

// Needed by Parser and Preprocessor (and class browser)
function IsSystemHeaderFile(const FileName: String; IncludePaths: TStringList): boolean;
function GetSystemHeaderFileName(const FileName: String; IncludePaths: TStringList): String; // <file.h>
function GetLocalHeaderFileName(const RelativeTo, FileName: String): String;
// "file.h"
function GetHeaderFileName(const RelativeTo, Line: String; IncludePaths, ProjectIncludePaths: TStringList):
  String; // both
function IsCfile(const Filename: String): boolean;
function IsHfile(const Filename: String): boolean;
procedure GetSourcePair(const FName: String; var CFile, HFile: String);
function IsIncludeLine(const Line: String): boolean;
function IsKeyword(const name:String): boolean;

function GetOperatorType(Phrase:String;index:integer):TOperatorType;

function IsAncestor(a:PStatement;b:PStatement):boolean;

implementation

function TDevStringList.CompareStrings(const S1, S2: string): Integer;
begin
  Result := AnsiCompareStr(S1, S2);
end;

function FastStringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;
var
  SearchStr, Patt, NewStr: String;
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

function FastIndexOf(List: TStrings; const S: String): integer;
begin
  with List do begin
    for Result := 0 to Count - 1 do
      if CompareText(List[Result], S) = 0 then
        Exit;
    Result := -1;
  end;
end;

function FastIndexOf(List: TStringlist; const S: String): integer;
begin
  if not List.Sorted then
    Result := FastIndexOf(TStrings(List), S)
  else if not List.Find(S, Result) then
    Result := -1;
end;

function StartsStr(const subtext, text: String): boolean;
begin
  Result := SameStr(subtext, Copy(text, 1, Length(subtext)));
end;

function StartsText(const subtext, text: String): boolean;
begin
  Result := SameText(subtext, Copy(text, 1, Length(subtext)));
end;

function SameStr(const s1, s2: String): boolean;
begin
  Result := (CompareStr(s1, s2) = 0);
end;

function SameText(const s1, s2: String): boolean;
begin
  Result := (CompareText(s1, s2) = 0);
end;

function EndsStr(const subtext, text: String): boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(text) - Length(subtext) + 1;
  if (SubTextLocation > 0) and (subtext <> '') then
    Result := StrComp(Pointer(subtext), Pointer(@text[SubTextLocation])) = 0
  else
    Result := False;
end;

function EndsText(const subtext, text: String): boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(text) - Length(subtext) + 1;
  if (SubTextLocation > 0) and (subtext <> '') then
    Result := StrIComp(Pointer(subtext), Pointer(@text[SubTextLocation])) = 0
  else
    Result := False;
end;

function ContainsStr(const text, subtext: String): boolean;
begin
  Result := Pos(subtext, text) > 0;
end;

function ContainsText(const text, subtext: String): boolean;
begin
  Result := Pos(UpperCase(subtext), UpperCase(text)) > 0;
end;

function ReplaceFirstStr(const S, OldPattern, NewPattern: String): String;
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

function ReplaceFirstText(const S, OldPattern, NewPattern: String): String;
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

function LastPos(const SubStr, s: String): integer;
begin
  result := Pos(ReverseString(SubStr), ReverseString(S));
  if result <> 0 then
    result := ((Length(S) - Length(SubStr)) + 1) - result + 1;
end;

function IsSystemHeaderFile(const FileName: String; IncludePaths: TStringList): boolean;
var
  FilePath: String;
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

function IsIncludeLine(const Line: String): boolean;
var
  TrimmedLine: String;
begin
  Result := False;
  TrimmedLine := Trim(Line);
  if (Length(TrimmedLine) > 0) and (TrimmedLine[1] = '#') then begin // it's a preprocessor line
    if StartsStr('include', TrimLeft(Copy(TrimmedLine, 2, MaxInt))) then begin // the first word after # is 'include'
      Result := True;
    end;
  end;
end;

function GetLocalHeaderFileName(const RelativeTo, FileName: String): String;
var
  Dir: String;
  s:String;
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

function GetSystemHeaderFileName(const FileName: String; IncludePaths: TStringList): String;
var
  I: integer;
  s:String;
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


function GetHeaderFileName(const RelativeTo, Line: String; IncludePaths, ProjectIncludePaths: TStringList):
  String;
var
  OpenTokenPos, CloseTokenPos: integer;
  FileName : String;
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

function IsCfile(const Filename: String): boolean;
var
  ext: String;
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

function IsHfile(const Filename: String): boolean;
var
  ext: String;
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

procedure GetSourcePair(const FName: String; var CFile, HFile: String);
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

function IsKeyword(const name:String): boolean;
begin
  Result:= CppKeywords.ValueOf(name)>=0;
end;

function GetOperatorType(Phrase:String;index:integer):TOperatorType;
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

  CppKeywords := TStringHash.Create();
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
  CppKeywords.Add('for',Ord(skItself));
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

  // Skip to {
  CppKeywords.Add('asm',Ord(skToRightBrace));
  CppKeywords.Add('catch',Ord(skToLeftBrace));
  CppKeywords.Add('do',Ord(skToLeftBrace));
  //CppKeywords.Add('namespace',Ord(skToLeftBrace)); // won't process it
  CppKeywords.Add('try',Ord(skToLeftBrace));

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


  // nullptr is value
  CppKeywords.Add('nullptr',Ord(skNone));

end;

finalization
begin
  CppKeywords.Clear;
  CppKeywords.Free;
end;
end.

