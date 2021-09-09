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

unit CppPreprocessor;

interface

uses
  Windows, Classes, SysUtils, StrUtils, ComCtrls, Math, cbutils, iniFiles,IntList;

const
  LineChars: set of Char = [#13, #10];
  SpaceChars: set of Char = [' ', #9];
  OperatorChars: set of Char = ['+', '-', '*', '/', '!', '=', '<', '>', '&', '|', '^'];
  IdentChars: set of Char = ['A'..'Z', '0'..'9', 'a'..'z', '_', '*', '&', '~'];
  MacroIdentChars: set of Char = ['A'..'Z', 'a'..'z', '_'];
  Operators: array[0..16] of string = ('*', '/', '+', '-', '<', '<=', '>', '>=', '==', '!=', '&', '^', '|', '&&', '||',
    'and', 'or');

type
  PFile = ^TFile;
  TFile = record
    Index: integer; // 0-based for programming convenience
    FileName: AnsiString; // Record filename, but not used now
    Buffer: TStringList; // do not concat them all
    Branches: integer; //branch levels;
    FileIncludes: PFileIncludes; // includes of this file
  end;

  TCppPreprocessor = class
  private
    fIndex: integer; // points to current file buffer. do not free
    fFileName: AnsiString; // idem
    fBuffer: TStringList; // idem
    fResult: TStringList;
    fCurrentIncludes: PFileIncludes;
    fPreProcIndex: integer;
    fIncludesList: TStringList;
    fHardDefines: TDevStringList; // set by "cpp -dM -E -xc NUL"
    fDefines: TStringList; // working set, editable
    fDefineIndex: TStringHash; // index of defines
    fFileDefines: TStringList; //dictionary to save defines for each headerfile; PDefine should be diposed here
    fIncludes: TList; // stack of files we've stepped into. last one is current file, first one is source file
    fBranchResults: TList;
    // list of branch results (boolean). last one is current branch, first one is outermost branch
    fIncludePaths: TStringList; // *pointer* to buffer of CppParser
    fProjectIncludePaths: TStringList;
    fScannedFiles: TStringList; // idem
    fParseSystem: boolean;
    fParseLocal: boolean;
    fProcessed: TStringHash; // dictionary to save filename already processed
    procedure PreprocessBuffer;
    procedure SkipToEndOfPreprocessor;
    procedure SkipToPreprocessor;
    function GetNextPreprocessor: AnsiString;
    procedure Simplify(var Output: AnsiString);
    procedure HandlePreprocessor(const Value: AnsiString);
    procedure HandleDefine(const Line: AnsiString);
    procedure HandleUndefine(const Line: AnsiString);
    procedure HandleBranch(const Line: AnsiString);
    procedure HandleInclude(const Line: AnsiString);
    function ExpandMacros(const Line: AnsiString; const depth :integer): AnsiString;
    function RemoveGCCAttributes(const Line: AnsiString): AnsiString;
    function RemoveSuffixes(const Input: AnsiString): AnsiString;
    // current file stuff
    function GetInclude(index: integer): PFile;
    procedure OpenInclude(const FileName: AnsiString; Stream: TMemoryStream = nil);
    property Includes[index: integer]: PFile read GetInclude; default;
    procedure CloseInclude;
    // branch stuff
    function GetCurrentBranch: boolean;
    procedure SetCurrentBranch(value: boolean);
    procedure RemoveCurrentBranch;
    function GetResult: AnsiString;
    // include stuff
    function GetFileIncludesEntry(const FileName: AnsiString): PFileIncludes;
    procedure AddDefinesInFile(const FileName:AnsiString);
  public
    constructor Create; 
    destructor Destroy; override;
    procedure Clear;
    procedure AddDefineByParts(const Name, Args, Value: AnsiString; HardCoded: boolean);
    procedure GetDefineParts(const Input: AnsiString; var Name, Args, Value: AnsiString);
    procedure AddDefineByLine(const Line: AnsiString; HardCoded: boolean);
    function GetDefine(const Name: AnsiString; var Index: integer): PDefine;
    function GetHardDefine(const Name: AnsiString; var Index: integer): PDefine;
    procedure Reset; //reset but don't clear generated defines
    procedure ResetDefines;
    procedure SetScanOptions(ParseSystem, ParseLocal: boolean);
    procedure SetIncludePaths(var List: TStringList);
    procedure SetProjectIncludePaths(var List: TStringList);
    procedure SetScannedFileList(var List: TStringList);
    procedure SetIncludesList(var List: TStringList);
    procedure PreprocessStream(const FileName: AnsiString; Stream: TMemoryStream);
    procedure PreprocessFile(const FileName: AnsiString);
    property Result: AnsiString read GetResult;
    procedure InvalidDefinesInFile(const FileName:AnsiString);
    //debug procedures
    procedure DumpIncludesListTo(FileName:ansiString);
    procedure DumpDefinesTo(FileName:ansiString);
    property HardDefines:TDevStringList read fHardDefines;
  end;

procedure Register;

implementation

procedure Register;
begin
  //RegisterComponents('Dev-C++', [TCppPreprocessor]);
end;

constructor TCppPreprocessor.Create;
begin
  inherited Create;
  fIncludes := TList.Create;
  fHardDefines := TDevStringList.Create;
  fDefines := TStringList.Create;
  fDefines.CaseSensitive := True;
  fDefineIndex := TStringHash.Create(2000);
  fDefines.Duplicates := dupAccept; // duplicate defines should generate warning
  fProcessed := TStringHash.Create;
  fFileDefines := TStringList.Create;
  fFileDefines.Sorted:=True;
  fBranchResults := TList.Create;
  fResult := TStringList.Create;
end;

procedure TCppPreprocessor.Clear;
var
  I,t: integer;
  DefineList: TList;
  FileName : AnsiString;
begin
  for I := 0 to fIncludes.Count - 1 do begin
    PFile(fIncludes[i])^.Buffer.Free;
    Dispose(PFile(fIncludes[i]));
  end;
  fIncludes.Clear;
  fDefines.Clear;
  fDefineIndex.Clear;
  for I := 0 to fHardDefines.Count - 1 do  begin
    PDefine(fHardDefines.Objects[i]).ArgList.Free;
    Dispose(PDefine(fHardDefines.Objects[i]));
  end;
  fProcessed.Clear;
  fHardDefines.Clear;
  for I:=0 to fFileDefines.Count -1 do begin
    FileName := fFileDefines[I];
    DefineList:=TList(fFileDefines.Objects[I]);
    for t:=0 to DefineList.Count-1 do begin
      PDefine(DefineList[t])^.ArgList.Free;
      dispose(PDefine(DefineList[t]));
    end;
    DefineList.Free;
  end;
  fFileDefines.Clear;
  fBranchResults.Clear;
  fResult.Clear;
end;

destructor TCppPreprocessor.Destroy;
begin
  Clear;
  fIncludes.Free;
  fDefines.Free;
  fDefineIndex.Free;
  fProcessed.Free;
  fHardDefines.Free;
  fFileDefines.Free;
  fBranchResults.Free;
  fResult.Free;

  inherited Destroy;
end;

procedure TCppPreprocessor.Reset;
var
  I: integer;
begin
  fResult.Clear;

  // Clear extracted data
  for I := 0 to fIncludes.Count - 1 do begin
    PFile(fIncludes[i])^.Buffer.Free;
    Dispose(PFile(fIncludes[i]));
  end;
  fIncludes.Clear;
  fBranchResults.Clear;
  fCurrentIncludes := nil;
  fProcessed.Clear;
  ResetDefines; // do not throw away hardcoded
end;

function TCppPreprocessor.GetInclude(index: integer): PFile;
begin
  result := PFile(fIncludes[index]);
end;

procedure TCppPreprocessor.AddDefinesInFile(const FileName:AnsiString);
var
  FileIncludes:PFileIncludes;
  i,idx,index: integer;
  DefineList:TList;
  define:PDefine;
  sl : TStringList;
begin
  if fProcessed.ValueOf(FileName) >0 then
    Exit;
  fProcessed.Add(FileName,1);
  if FastIndexOf(fScannedFiles, FileName) = -1 then
    Exit;
  idx := FastIndexOf(fFileDefines,FileName);
  if idx <>-1 then begin
    DefineList := TList(fFileDefines.Objects[idx]);
    for i:=0 to DefineList.Count-1 do begin
      define := PDefine(DefineList[I]);
      if Assigned(define) then begin
        index:=fDefines.AddObject(define^.Name,Pointer(define));
        fDefineIndex.Add(define^.Name,index);
      end;
    end;
  end;
  FileIncludes:=GetFileIncludesEntry(FileName);
  if Assigned(FileIncludes) then begin
    sl := FileIncludes^.IncludeFiles;
    for I := 0 to sl.Count - 1 do 
      AddDefinesInFile(sl[I]);
  end;
end;

procedure TCppPreprocessor.OpenInclude(const FileName: AnsiString; Stream: TMemoryStream = nil);
var
  FileItem,tempItem: PFile;
  IsSystemFile: boolean;
  IncludeLine: AnsiString;
  I,t: integer;
  FileIncludes:PFileIncludes;
begin
  for i:=0 to fIncludes.Count-1 do begin
    tempItem := PFile(fIncludes[i]);
    if SameText(tempItem^.FileName,FileName) then // don't include recursively
      Exit;
    if FastIndexOf(tempItem^.FileIncludes^.IncludeFiles,FileName)=-1 then
      tempItem^.FileIncludes^.IncludeFiles.Add(FileName)
    else
      Exit; // already included
    // := tempItem^.FileIncludes^.IncludeFiles + AnsiQuotedStr(FileName, '"') + ',';
  end;

  // Backup old position if we're entering a new file
  if fIncludes.Count > 0 then begin
    PFile(fIncludes[fIncludes.Count - 1])^.Index := fIndex;
    PFile(fIncludes[fIncludes.Count - 1])^.Branches := fBranchResults.Count;
  end;

  // Add the new file to the includes of the current file
  // Only add items to the include list of the given file if the file hasn't been scanned yet
  // The above is fixed by checking for duplicates.
  // The proper way would be to do backtracking of files we have FINISHED scanned.
  // These are not the same files as the ones in fScannedFiles. We have STARTED scanning these.
  {
  if Assigned(fCurrentIncludes) then
    with fCurrentIncludes^ do
      if not ContainsText(IncludeFiles, FileName) then
        IncludeFiles := IncludeFiles + AnsiQuotedStr(FileName, '"') + ',';
  }

  // Create and add new buffer/position
  FileItem := new(PFile);
  FileItem^.Index := 0; // 0-based line counter
  FileItem^.FileName := FileName;
  FileItem^.Buffer := TStringList.Create;
  FileItem^.Branches := 0;


  // Keep track of files we include here
  // Only create new items for files we have NOT scanned yet
  fCurrentIncludes := GetFileIncludesEntry(FileName);
  if not Assigned(fCurrentIncludes) then begin // do NOT create a new item for a file that's already in the list
    fCurrentIncludes := New(PFileIncludes);
    fCurrentIncludes^.BaseFile := FileName;
    fCurrentIncludes^.IncludeFiles := TStringList.Create;
    fCurrentIncludes^.IncludeFiles.Duplicates:=dupIgnore;
    fCurrentIncludes^.IncludeFiles.Sorted:=True;
    fCurrentIncludes^.DirectIncludeFiles := TStringList.Create;
    fCurrentIncludes^.DirectIncludeFiles.Duplicates:=dupIgnore;
    fCurrentIncludes^.Usings := TDevStringList.Create;
    fCurrentIncludes^.Usings.Duplicates := dupIgnore;
    fCurrentIncludes^.Usings.Sorted:=True;
    //fCurrentIncludes^.Usings.Add('std'); //using std by default
    fCurrentIncludes^.Statements:=TList.Create;
    fCurrentIncludes^.StatementsIndex:=TDevStringHash.Create;
    fCurrentIncludes^.DeclaredStatements:=TList.Create;
    fCurrentIncludes^.DeclaredStatementsIndex:=TDevStringHash.Create;
    fCurrentIncludes^.Scopes := TIntList.Create;
    fCurrentIncludes^.Scopes.Sorted := True;
    fCurrentIncludes^.DependingFiles:=THashedStringList.Create;
    fCurrentIncludes^.DependingFiles.CaseSensitive:=False;
    fCurrentIncludes^.DependingFiles.Sorted := True;
    fCurrentIncludes^.DependingFiles.Duplicates := dupIgnore;

    fCurrentIncludes^.DependedFiles:=THashedStringList.Create;
    fCurrentIncludes^.DependedFiles.CaseSensitive := False;
    fCurrentIncludes^.DependedFiles.Sorted := True;
    fCurrentIncludes^.DependedFiles.Duplicates := dupIgnore;
    fIncludesList.AddObject(FileName,TObject(fCurrentIncludes));
  end;

  FileItem^.FileIncludes := fCurrentIncludes;

  // Don't parse stuff we have already parsed
  if Assigned(Stream) or (FastIndexOf(fScannedFiles, FileName) = -1) then begin

    // Parse ONCE
    //if not Assigned(Stream) then
    fScannedFiles.Add(FileName);

    // Only load up the file if we are allowed to parse it
    IsSystemFile := cbutils.IsSystemHeaderFile(FileName, fIncludePaths);
    if (fParseSystem and IsSystemFile) or (fParseLocal and not IsSystemFile) then begin
      if Assigned(Stream) then begin
        Stream.Position := 0; // start scanning from here
        FileItem^.Buffer.LoadFromStream(Stream)
      end else if FileExists(FileName) then
        FileItem^.Buffer.LoadFromFile(FileName); // load it now
    end;
  end else begin
    //add defines of already parsed including headers;
    AddDefinesInFile(FileName);
    FileIncludes:=GetFileIncludesEntry(FileName);
    for i:=0 to fIncludes.Count-1 do begin
      tempItem := PFile(fIncludes[i]);
      for t:=0 to FileIncludes^.IncludeFiles.Count -1 do begin
        PFileIncludes(tempItem^.FileIncludes)^.IncludeFiles.Add(FileIncludes^.IncludeFiles[t]);
      end;
    end;
  end;
  fIncludes.Add(FileItem);

  {
  //Convert Unix EOL to Windows EOL
  if not ContainsStr(FileItem^.Buffer.Text,#13#10) then begin
    StringReplace(FileItem^.Buffer.Text,#10,#13#10,[rfReplaceAll]);
  end;
  }
  
  // Process it
  fIndex := FileItem^.Index;
  fFileName := FileItem^.FileName;
  fBuffer := FileItem^.Buffer;

  // Trim all lines
  for I := 0 to fBuffer.Count - 1 do
    fBuffer[i] := Trim(fBuffer[i]);

  // Update result file
  IncludeLine := '#include ' + FileName + ':1';
  if fIncludes.Count > 1 then // include from within a file
    fResult[fPreProcIndex] := IncludeLine
  else // new file
    fResult.Add(IncludeLine);
end;

procedure TCppPreprocessor.CloseInclude;
begin
  if fIncludes.Count > 0 then begin

    // Close current buffer
    PFile(fIncludes[fIncludes.Count - 1])^.Buffer.Free;
    Dispose(PFile(fIncludes[fIncludes.Count - 1]));
    fIncludes.Delete(fIncludes.Count - 1);

    if fIncludes.Count > 0 then begin

      // Continue where we left off
      fIndex := Includes[fIncludes.Count - 1]^.Index;
      fFileName := Includes[fIncludes.Count - 1]^.FileName;
      fBuffer := Includes[fIncludes.Count - 1]^.Buffer;
      while (fBranchResults.Count > Includes[fIncludes.Count - 1]^.Branches) do begin
        fBranchResults.Delete(fBranchResults.Count-1);
      end;
      // Point to previous buffer and start past the include we walked into

      // Start augmenting previous include list again
      //fCurrentIncludes := GetFileIncludesEntry(fFileName);
      fCurrentIncludes := Includes[fIncludes.Count - 1]^.FileIncludes;

      // Update result file (we've left the previous file)
      fResult.Add('#include ' + Includes[fIncludes.Count - 1]^.FileName + ':' + IntToStr(Includes[fIncludes.Count -
        1]^.Index + 1));
    end;
  end;
end;

function TCppPreprocessor.GetCurrentBranch: boolean;
begin
  if fBranchResults.Count > 0 then
    Result := boolean(Integer(fBranchResults[fBranchResults.Count - 1]))
  else
    Result := True;
end;

procedure TCppPreprocessor.SetCurrentBranch(value: boolean);
begin
  fBranchResults.Add(Pointer(integer(value)));
end;

procedure TCppPreprocessor.RemoveCurrentBranch;
begin
  if fBranchResults.Count > 0 then
    fBranchResults.Delete(fBranchResults.Count - 1);
end;

procedure TCppPreprocessor.SetScanOptions(ParseSystem, ParseLocal: boolean);
begin
  fParseSystem := ParseSystem;
  fParseLocal := ParseLocal;
end;

procedure TCppPreprocessor.SetIncludesList(var List: TStringList);
begin
  fIncludesList := List;
end;

procedure TCppPreprocessor.SetIncludePaths(var List: TStringList);
begin
  fIncludePaths := List;
end;

procedure TCppPreprocessor.SetProjectIncludePaths(var List: TStringList);
begin
  fProjectIncludePaths := List;
end;

procedure TCppPreprocessor.SetScannedFileList(var List: TStringList);
begin
  fScannedFiles := List;
end;

procedure TCppPreprocessor.SkipToPreprocessor;
var
  InCComment:boolean;
  function FirstLineChar(const Line: AnsiString): Char;
  begin
    if Length(Line) > 0 then
      Result := TrimLeft(Line)[1] // assume trimmed lines
    else
      Result := #0;
  end;
begin
  InCComment:=False;
  // Increment until a line begins with a #
  while (fIndex < fBuffer.Count) and (InCComment or(FirstLineChar(fBuffer[fIndex]) <> '#')) do begin
    if Pos('/*',fBuffer[fIndex]) >0 then
      InCComment:=True;
    if Pos('*/',fBuffer[fIndex]) >0 then
      InCComment:=False;
    if GetCurrentBranch then // if not skipping, expand current macros
      fResult.Add(ExpandMacros(fBuffer[fIndex],1))
    else // If skipping due to a failed branch, clear line
      fResult.Add('');
    Inc(fIndex);
  end;
end;

procedure TCppPreprocessor.SkipToEndOfPreprocessor;
  function LastLineChar(const Line: AnsiString): Char;
  begin
    if Length(Line) > 0 then
      Result := Line[Length(Line)] // assume trimmed lines
    else
      Result := #0;
  end;
begin
  // Skip until last char of line is NOT \ anymore
  while (fIndex < fBuffer.Count) and (LastLineChar(fBuffer[fIndex]) = '\') do begin
    Inc(fIndex);
  end;
end;

function TCppPreprocessor.GetNextPreprocessor: AnsiString;
var
  I: integer;
  PreProcFrom, PreProcTo: integer;
begin
  Result := '';

  SkipToPreprocessor; // skip until # at start of line
  PreProcFrom := fIndex;
  if PreProcFrom >= fBuffer.Count then // we've gone past the final #preprocessor line. Yay
    Exit;

  SkipToEndOfPreprocessor;
  PreProcTo := fIndex;

  // Calculate index to insert defines in in result file
  fPreProcIndex := (fResult.Count - 1) + 1; // offset by one for #include rootfile

  // Assemble whole line, including newlines
  for I := PreProcFrom to PreProcTo do begin
    Result := Result + fBuffer[i] + #13#10;
    fResult.Add(''); // defines resolve into empty files, except #define and #include
  end;

  // Step over
  Inc(fIndex);
end;

procedure TCppPreprocessor.Simplify(var Output: AnsiString);
var
  DelimPosFrom, DelimPosTo: integer;
begin
  // Remove #
  Output := Copy(Output, 2, MaxInt);

  // Remove newlines in concatenated expressions
  Output := FastStringReplace(Output, '\'#13, '', [rfReplaceAll]);
  Output := FastStringReplace(Output, '\'#10, '', [rfReplaceAll]);
  Output := FastStringReplace(Output, #13, '', [rfReplaceAll]);
  Output := FastStringReplace(Output, #10, '', [rfReplaceAll]);

  // Remove C-style comments
  while true do begin
    DelimPosFrom := Pos('/*', Output);
    if DelimPosFrom > 0 then begin
      DelimPosTo := PosEx('*/', Output, DelimPosFrom);
      if DelimPosTo > 0 then
        Delete(Output, DelimPosFrom, DelimPosTo - DelimPosFrom + Length('*/'))
      else
        break; // invalid syntax. ignore
    end else
      break;
  end;

  // Don't remove multiple spaces. This can ruin defines which explicity use multiple spaces in their values

  Output := Trim(Output); // removes spaces between # and the first word
end;

function TCppPreprocessor.GetDefine(const Name: AnsiString; var Index: integer): PDefine;
begin
  Index := fDefineIndex.ValueOf(Name); // use hashed searching. is really fast
  if Index <> -1 then
    Result := PDefine(fDefines.Objects[Index])
  else
    Result := nil;
end;

function TCppPreprocessor.GetHardDefine(const Name: AnsiString; var Index: integer): PDefine;
begin
  Index := FastIndexOf(fHardDefines,Name); // use sorted searching. is really fast
  if Index <> -1 then
    Result := PDefine(fHardDefines.Objects[Index])
  else
    Result := nil;
end;

procedure TCppPreprocessor.AddDefineByParts(const Name, Args, Value: AnsiString; HardCoded: boolean);
var
  Item: PDefine;
  idx,index: integer;
  DefineList:TList;

  procedure tokenizeValue(const Value: string; tokens:TStringList);
  var
    i,endPos:integer;
    token:string;
  begin
    i:=1;
    endPos := length(value);
    token:='';
    while i<=endPos do begin
      if value[i] in [' ',#9] then begin
        if token <> '' then begin
          tokens.Add(token);
        end;
        token:='';
        inc(i);
      end else if value[i] = '#' then begin
        if token <> '' then begin
          tokens.Add(token);
        end;
        token:='';
        if (i+1<=endPos) and (value[i+1]='#') then begin
          tokens.Add('##');
          inc(i,2);
        end else begin
          tokens.Add('#');
          inc(i,1);
        end;
      end else if value[i] in ['0'..'9','a'..'z','A'..'Z','_'] then begin
        token:=token+value[i];
        inc(i);
      end else begin
        if token <> '' then begin
          tokens.Add(token);
        end;
        token:='';
        tokens.Add(value[i]);
        inc(i);
      end;
    end;
    if token<>'' then begin
      tokens.Add(token);
    end;
  end;

  procedure ParseArgs(item:PDefine);
  var
    args,arg,formatStr:string;
    tokens: TStringList;
    i:integer;
    lastIsConcat: boolean;
    s:string;
  begin
    args := Copy(item^.Args,2,length(item^.Args)-2); // remove '(' ')'
    args := Trim(Args);
    if args = '' then // we only handle macros with exactly one param here
      Exit;
    if Pos(',',Args)>0 then // we only handle macros with exactly one param here
      Exit;
    arg:=Args;
    tokens:=TStringList.Create;
    formatStr := '';
    try
      tokenizeValue(item^.Value,tokens);
      lastIsConcat := False;
      for i:=0 to tokens.Count-1 do begin
        if sameStr(tokens[i],'#') then //we don't handle '#' operator
          Exit;
        if sameStr(tokens[i],'##') then begin// ignore ##
          lastIsConcat:=True;
          Continue;
        end;
        if not lastIsConcat then begin
          formatStr:=formatStr+' ';
        end;
        s:=tokens[i];
        if s='%' then
          s:='%%';
        if sameStr(tokens[i],arg) then begin
          item^.ArgList.Add(1);
          formatStr:=formatStr+'%s';
        end else begin
          formatStr:=formatStr+s;
        end;
        lastIsConcat := False;
      end;
      item^.FormatValue := trim(formatStr);
    finally
      tokens.Free;
    end;
  end;
begin
  // Do not check for duplicates. It's too slow
  Item := new(PDefine);
  Item^.Name := Name;
  Item^.Args := Args;
  Item^.Value := Value;
  Item^.FileName := fFileName;
  Item^.ArgList := TIntList.Create;
  Item^.FormatValue := '';
  Item^.IsMultiLine := ContainsStr(Item^.Value, #10);
  Item^.HardCoded := HardCoded;
  if Args <> '' then begin
    parseArgs(Item);
  end;
  if HardCoded then
    fHardDefines.AddObject(Name, Pointer(Item)) // uses TStringList too to be able to assign to fDefines easily
  else begin
    idx := FastIndexOf(fFileDefines,fFileName);
    if idx = -1 then begin
      DefineList := TList.Create;
      fFileDefines.AddObject(fFileName, DefineList);
    end else begin
      DefineList := TList(fFileDefines.Objects[idx]);
    end;
    DefineList.Add(Pointer(Item));
    index:=fDefines.AddObject(Name, Pointer(Item));
    fDefineIndex.Add(Name,index);
  end;
end;

//todo: expand macros in the macro define!
// input should omit the define word
procedure TCppPreprocessor.GetDefineParts(const Input: AnsiString; var Name, Args, Value: AnsiString);
var
  I, Level, ArgStart: integer;
  S: AnsiString;
  IsFunction: boolean;
begin
  S := TrimLeft(Input);
  Name := '';
  Args := '';
  Value := '';

  // Rules:
  // When the character before the first opening brace is nonblank, a function is defined.
  // After that point, switch from name to args
  // The value starts after the first blank character outside of the outermost () pair

  I := 1;
  Level := 0;
  IsFunction := False;
  ArgStart := 0;
  while I <= Length(S) do begin

    // When we find the first opening brace, check if this is a function define
    if S[i] = '(' then begin
      Inc(Level);
      if (Level = 1) and not IsFunction then begin // found a function define!
        Name := Copy(S, 1, I - 1);
        ArgStart := I;
        IsFunction := True;
      end;
    end else if S[i] = ')' then begin
      Dec(Level);
    end else if (S[i] in SpaceChars) and (Level = 0) then
      break; // found the end of our idenfifier
    Inc(I);
  end;

  if IsFunction then begin
    // Name has already been found
    Args := Copy(S, ArgStart, I - ArgStart);
    //todo: expand macro (if already have)
  end else begin
    Name := Trim(Copy(S, 1, I));
    Args := '';
  end;
  Value := RemoveGCCAttributes(TrimLeft(Copy(S, I + 1, MaxInt)));
end;

procedure TCppPreprocessor.AddDefineByLine(const Line: AnsiString; HardCoded: boolean);
var
  Name, Args, Value, S: AnsiString;
begin
  // Remove define
  S := TrimLeft(Copy(Line, Length('define') + 1, MaxInt));

  // Get parts from generalized function
  GetDefineParts(S, Name, Args, Value);

  // Add to the list
  AddDefineByParts(Name, Args, Value, HardCoded);
end;

procedure TCppPreprocessor.ResetDefines;
var
  I,idx: integer;
  define : PDefine;
  //DefineList: TList;
begin
  fDefines.Clear;
  fDefineIndex.Clear;

  for i:=0 to fHardDefines.Count -1 do
  begin
    define := PDefine(fHardDefines.Objects[i]);
    idx:=fDefines.AddObject(define^.Name,Pointer(define));
    fDefineIndex.Add(define^.Name,idx);
  end;

end;

procedure TCppPreprocessor.HandlePreprocessor(const Value: AnsiString);
begin
  if StartsStr('define', Value) then
    HandleDefine(Value)
  else if StartsStr('undef', Value) then
    HandleUndefine(Value)
  else if StartsStr('if', Value) or StartsStr('else', Value) or StartsStr('elif', Value) or StartsStr('endif', Value)
    then
    HandleBranch(Value)
  else if StartsStr('include', Value) then
    HandleInclude(Value);
end;

procedure TCppPreprocessor.HandleDefine(const Line: AnsiString);
begin
  if GetCurrentBranch then begin
    AddDefineByLine(Line, false);
    fResult[fPreProcIndex] := '#' + Line; // add define to result file so the parser can handle it
  end;
end;

procedure TCppPreprocessor.HandleUndefine(const Line: AnsiString);
var
  Define: PDefine;
  Name: AnsiString;
  Index,idx,i: integer;
  DefineList:TList;
  files:TStringList;
begin
  // Remove undef
  Name := TrimLeft(Copy(Line, Length('undef') + 1, MaxInt));

  files:=TStringList.Create;
  files.Sorted:=True;
  files.Duplicates:=dupIgnore;
  try
    // may be defined many times
    while True do begin
      Define := GetDefine(Name, Index);
      if Assigned(Define) then begin
        fDefineIndex.Remove(Name);
        fDefines.objects[index]:=nil;
        files.AddObject(Define^.FileName,Pointer(Define));
      end else
        break;
    end;
    for i:=0 to files.Count-1 do begin
      Define:= PDefine(files.objects[i]);
      idx:=FastIndexOf(fFileDefines,files[i]);
      if idx>0 then begin
        DefineList:=TList(fFileDefines.Objects[idx]);
        DefineList.Remove(Pointer(Define));
        define^.ArgList.Free;
        Dispose(PDefine(Define));
      end;
    end;
  finally
    files.Free;
  end;
end;

procedure TCppPreprocessor.HandleBranch(const Line: AnsiString);
var
  Name, IfLine: AnsiString;
  testResult,OldResult: boolean;
  I, Dummy: integer;

  // Should start on top of the opening char
  function SkipBraces(const Line: AnsiString; var Index: integer; Step: integer = 1): boolean;
  var
    Level: integer;
  begin
    Level := 0;
    while (Index > 0) and (Index <= Length(Line)) do begin // Find the corresponding opening brace
      if Line[Index] = '(' then begin
        Inc(Level);
        if Level = 0 then begin
          Result := true;
          Exit;
        end;
      end else if Line[Index] = ')' then begin
        Dec(Level);
        if Level = 0 then begin
          Result := true;
          Exit;
        end;
      end;
      Inc(Index, Step);
    end;
    Result := false;
  end;

  // Expand any token that isn't a number
  function ExpandDefines(Line: AnsiString): AnsiString;
  var
    SearchPos, Head, Head1, Tail, NameStart, NameEnd: integer;
    Name, Args, InsertValue: AnsiString;
    Define: PDefine;

    function ExpandFunction(FunctionDefine: PDefine; const ArgValueString: AnsiString): AnsiString;
    var
      ArgNames, ArgValues: TStringList;
      I: integer;
    begin
      // Replace function by this string
      Result := FunctionDefine^.Value;

      // Replace names by values...
      ArgNames := TStringList.Create;
      ArgValues := TStringList.Create;
      try
        ExtractStrings([',', '(', ')'], [], PAnsiChar(FunctionDefine^.Args), ArgNames);
        ExtractStrings([',', '(', ')'], [], PAnsiChar(ArgValueString), ArgValues); // extract from Line string

        // If the argument count matches up, replace names by values
        if ArgNames.Count = ArgValues.Count then begin
          for I := 0 to ArgNames.Count - 1 do
            Result := StringReplace(Result, Trim(ArgNames[i]), Trim(ArgValues[i]), [rfReplaceAll]);
        end;
      finally
        ArgNames.Free;
        ArgValues.Free;
      end;
    end;
  begin
    SearchPos := 1;
    while (SearchPos <= Length(Line)) do begin

      // We have found an identifier. It is not a number suffix. Try to expand it
      if (Line[SearchPos] in MacroIdentChars) and ((SearchPos = 1) or not (Line[SearchPos - 1] in ['0'..'9'])) then begin
        Tail := SearchPos;
        Head := SearchPos;

        // Get identifier name (numbers are allowed, but not at the start
        while (Head <= Length(Line)) and ((Line[Head] in MacroIdentChars) or (Line[Head] in ['0'..'9'])) do
          Inc(Head);
        Name := Copy(Line, Tail, Head - Tail);
        NameStart := Tail;
        NameEnd := Head;

        // Skip over contents of these built-in functions
        if Name = 'defined' then begin
          Head := SearchPos + Length(Name);
          while (Head <= Length(Line)) and (Line[Head] in SpaceChars) do
            Inc(Head); // skip spaces

          Head1:=Head;
          // Skip over its arguments
          if SkipBraces(Line, Head) then begin
            SearchPos := Head;
          end else begin
            //Skip none braced argument (next word)
            {
            Line := ''; // broken line
            break;
            }
            Head:=Head1;
            if (Head>Length(Line)) or not (Line[SearchPos] in MacroIdentChars) then begin
              Line := ''; // broken line
              break;
            end;
            while (Head <= Length(Line)) and ((Line[Head] in MacroIdentChars) or (Line[Head] in ['0'..'9'])) do
              Inc(Head);
            end;
            SearchPos := Head;
        end else if (Name = 'and') or (Name = 'or') then begin
          SearchPos := Head; // Skip logical operators

          // We have found a regular define. Replace it by its value
        end else begin

          // Does it exist in the database?
          Define := GetDefine(Name,Dummy);
          if not Assigned(Define) then begin
            InsertValue := '0';
          end else begin
            while (Head <= Length(Line)) and (Line[Head] in SpaceChars) do
              Inc(Head); // skip spaces

            // It is a function. Expand arguments
            if (Head <= Length(Line)) and (Line[Head] = '(') then begin
              Tail := Head;
              if SkipBraces(Line, Head) then begin
                Args := Copy(Line, Tail, Head - Tail + 1);
                InsertValue := ExpandFunction(Define, Args);
                NameEnd := Head + 1;
              end else begin
                Line := ''; // broken line
                break;
              end;

              // Replace regular define
            end else begin
              if Define^.Value <> '' then
                InsertValue := Define^.Value
              else
                InsertValue := '0';
            end;
          end;

          // Insert found value at place
          Delete(Line, NameStart, NameEnd - NameStart);
          Insert(InsertValue, Line, SearchPos);
        end;
      end else
        Inc(SearchPos);
    end;
    Result := Line;
  end;

  function EvaluateDefines(Line: AnsiString): AnsiString;
  var
    S: AnsiString;
    I,I1, Head, Tail: integer;
    DefineResult, InvertResult: boolean;
  begin
    while true do begin
      I := Pos('defined', Line);
      if I > 0 then begin
        // Check for boolean inverter
        I1:= I-1;
        while (I1>0) and (Line[I1] in [#9,#32]) do begin
          dec(I1);
        end;
        if (I1<=0) then
          I1:=I;


        InvertResult := (Line[I1] = '!');

        // Find expression enclosed in () or after space
        Tail := I + Length('defined'); // find (

        // Skip spaces after defined keyword
        while (Tail <= Length(Line)) and (Line[Tail] in SpaceChars) do
          Inc(Tail);

        // If we find an opening brace, find its closing brace
        Head := Tail;
        if (Head <= Length(Line)) and (Line[Head] = '(') then begin
          if not SkipBraces(Line, Head) then begin
            Result := '';
            Exit;
          end;
          S := Copy(Line, Tail + 1, Head - Tail - 1);
          // If we find an identifier, walk until it ends
        end else begin
          while (Head <= Length(Line)) and (Line[Head] in IdentChars) do // find end of identifier
            Inc(Head);
          S := Copy(Line, Tail, Head - Tail);
        end;

        // Delete expression from string
        Tail := I;
        if InvertResult then
          Tail:=I1;
        Delete(Line, Tail, Head - Tail + 1);

        // Evaludate and replace expression by 1 or 0 (true or false)
        DefineResult := Assigned(GetDefine(S,Dummy));
        if (DefineResult and not InvertResult) or (not DefineResult and InvertResult) then
          Insert('1', Line, Tail)
        else
          Insert('0', Line, Tail);
      end else
        break;
    end;
    Result := Line;
  end;

  function EvaluateExpression(Line: AnsiString): AnsiString;
  var
    Head, Tail, EquatStart, EquatEnd, OperatorPos: integer;
    LeftOpValue, RightOpValue, ResultValue: Int64;
    LeftOp, RightOp, OperatorToken, ResultLine: AnsiString;

    function GetNextOperator(var Offset: integer): AnsiString;
    var
      I, PastOperatorEnd: integer;
    begin
      for I := Low(Operators) to High(Operators) do begin
        Offset := Pos(Operators[i], Line);

        // Is this operator present in the line?
        if Offset > 0 then begin

          // Aren't we misinterpreting && for & (for example)?
          PastOperatorEnd := Offset + Length(Operators[i]);
          if (PastOperatorEnd <= Length(Line)) and not (Line[PastOperatorEnd] in OperatorChars) then begin
            Result := Operators[i];
            Exit;
          end;
        end;
      end;
      Offset := 0;
      Result := '';
    end;
  begin
    // Find the first closing brace (this should leave us at the innermost brace pair)
    while true do begin
      Head := Pos(')', Line);
      if Head > 0 then begin
        Tail := Head;
        if SkipBraces(Line, Tail, -1) then begin // find the corresponding opening brace
          ResultLine := EvaluateExpression(Copy(Line, Tail + 1, Head - Tail - 1)); // evaluate this (without braces)
          Delete(Line, Tail, Head - Tail + 1); // Remove the old part AND braces
          Insert(ResultLine, Line, Tail); // and replace by result
        end else begin
          Result := '';
          Exit;
        end;
      end else
        break;
    end;

    // Then evaluate braceless part
    while true do begin
      OperatorToken := GetNextOperator(OperatorPos);
      if OperatorPos > 0 then begin

        // Get left operand
        Tail := OperatorPos - 1;
        while (Tail >= 0) and (Line[Tail] in SpaceChars) do
          Dec(Tail); // skip spaces
        Head := Tail;
        while (Head >= 0) and (Line[Head] in IdentChars) do
          Dec(Head); // step over identifier
        LeftOp := Copy(Line, Head + 1, Tail - Head);
        EquatStart := Head + 1; // marks begin of equation

        // Get right operand
        Tail := OperatorPos + Length(OperatorToken) + 1;
        while (Tail <= Length(Line)) and (Line[Tail] in SpaceChars) do
          Inc(Tail); // skip spaces
        Head := Tail;
        while (Head <= Length(Line)) and (Line[Head] in IdentChars) do
          Inc(Head); // step over identifier
        RightOp := Copy(Line, Tail, Head - Tail);
        EquatEnd := Head; // marks begin of equation

        // Evaluate after removing length suffixes...
        LeftOpValue := StrToIntDef(RemoveSuffixes(LeftOp), 0);
        RightOpValue := StrToIntDef(RemoveSuffixes(RightOp), 0);
        if OperatorToken = '*' then
          ResultValue := LeftOpValue * RightOpValue
        else if OperatorToken = '/' then begin
          if RightOpValue = 0 then
            ResultValue := LeftOpValue
          else
            ResultValue := LeftOpValue div RightOpValue; // int division
        end else if OperatorToken = '+' then
          ResultValue := LeftOpValue + RightOpValue
        else if OperatorToken = '-' then
          ResultValue := LeftOpValue - RightOpValue
        else if OperatorToken = '<' then
          ResultValue := integer(LeftOpValue < RightOpValue)
        else if OperatorToken = '<=' then
          ResultValue := integer(LeftOpValue <= RightOpValue)
        else if OperatorToken = '>' then
          ResultValue := integer(LeftOpValue > RightOpValue)
        else if OperatorToken = '>=' then
          ResultValue := integer(LeftOpValue >= RightOpValue)
        else if OperatorToken = '==' then
          ResultValue := integer(LeftOpValue = RightOpValue)
        else if OperatorToken = '!=' then
          ResultValue := integer(LeftOpValue <> RightOpValue)
        else if OperatorToken = '&' then // bitwise and
          ResultValue := integer(LeftOpValue and RightOpValue)
        else if OperatorToken = '|' then // bitwise or
          ResultValue := integer(LeftOpValue or RightOpValue)
        else if OperatorToken = '^' then // bitwise xor
          ResultValue := integer(LeftOpValue xor RightOpValue)
        else if (OperatorToken = '&&') or (OperatorToken = 'and') then
          ResultValue := integer(LeftOpValue and RightOpValue)
        else if (OperatorToken = '||') or (OperatorToken = 'or') then
          ResultValue := integer(LeftOpValue or RightOpValue)
        else
          ResultValue := 0;

        // And replace by result in string form
        Delete(Line, EquatStart, EquatEnd - EquatStart);
        Insert(IntToStr(ResultValue), Line, EquatStart);
      end else
        break;
    end;
    Result := Line;
  end;

  function EvaluateIf(Line: AnsiString): boolean;
  begin
    Line := ExpandDefines(Line); // replace FOO by numerical value of FOO
    Line := EvaluateDefines(Line); // replace all defined() by 1 or 0
    Result := StrToIntDef(RemoveSuffixes(EvaluateExpression(Line)), -1) > 0; // perform the remaining int arithmetic
  end;
begin

  if StartsStr('ifdef', Line) then begin
    // if a branch that is not at our level is false, current branch is false too;
    for I := 0 to fBranchResults.Count - 2 do
      if integer(fBranchResults[i]) = 0 then begin
        SetCurrentBranch(false);
        Exit;
      end;
    if not GetCurrentBranch then // we are already inside an if that is NOT being taken
      SetCurrentBranch(false) // so don't take this one either
    else begin
      Name := TrimLeft(Copy(Line, Length('ifdef') + 1, MaxInt));
      SetCurrentBranch(Assigned(GetDefine(Name,Dummy)));
    end;
  end else if StartsStr('ifndef', Line) then begin
    // if a branch that is not at our level is false, current branch is false too;
    for I := 0 to fBranchResults.Count - 2 do
      if integer(fBranchResults[i]) = 0 then begin
        SetCurrentBranch(false);
        Exit;
      end;
    if not GetCurrentBranch then // we are already inside an if that is NOT being taken
      SetCurrentBranch(false) // so don't take this one either
    else begin
      Name := TrimLeft(Copy(Line, Length('ifndef') + 1, MaxInt));
      SetCurrentBranch(not Assigned(GetDefine(Name,Dummy)));
    end;
  end else if StartsStr('if', Line) then begin
    // if a branch that is not at our level is false, current branch is false too;
    for I := 0 to fBranchResults.Count - 2 do
      if integer(fBranchResults[i]) = 0 then begin
        SetCurrentBranch(false);
        Exit;
      end;
    if not GetCurrentBranch then // we are already inside an if that is NOT being taken
      SetCurrentBranch(false) // so don't take this one either
    else begin
      IfLine := TrimLeft(Copy(Line, Length('if') + 1, MaxInt)); // remove if
      testResult := EvaluateIf(IfLine);
      SetCurrentBranch(testResult);
    end;
  end else if StartsStr('else', Line) then begin
    // if a branch that is not at our level is false, current branch is false too;
    for I := 0 to fBranchResults.Count - 2 do
      if integer(fBranchResults[i]) = 0 then begin
        RemoveCurrentBranch;
        SetCurrentBranch(false);
        Exit;
      end;
    OldResult := GetCurrentBranch; // take either if or else
    RemoveCurrentBranch;
    SetCurrentBranch(not OldResult);
  end else if StartsStr('elif', Line) then begin
    // if a branch that is not at our level is false, current branch is false too;
    for I := 0 to fBranchResults.Count - 2 do
      if integer(fBranchResults[i]) = 0 then begin
        RemoveCurrentBranch;
        SetCurrentBranch(false);
        Exit;
      end;  
    OldResult := GetCurrentBranch; // take either if or else
    RemoveCurrentBranch;        
    if OldResult then begin // don't take this one, previous if has been taken
      SetCurrentBranch(false);
    end else begin // previous ifs failed. try this one
      IfLine := TrimLeft(Copy(Line, Length('elif') + 1, MaxInt)); // remove elif
      SetCurrentBranch(EvaluateIf(IfLine));
    end;
  end else if StartsStr('endif', Line) then
    RemoveCurrentBranch;
end;

procedure TCppPreprocessor.HandleInclude(const Line: AnsiString);
var
  FileName: AnsiString;
begin
  if not GetCurrentBranch then // we're skipping due to a branch failure
    Exit;

  // Get full header file name
  FileName := cbutils.GetHeaderFileName(Includes[fIncludes.Count - 1]^.FileName, Line, fIncludePaths,
    fProjectIncludePaths);

  if FileName = '' then
    Exit;

  fCurrentIncludes.DirectIncludeFiles.Add(FileName);
  // And open a new entry
  OpenInclude(ExpandFileName(FileName));
end;

function TCppPreprocessor.RemoveGCCAttributes(const Line: AnsiString): AnsiString; //we only expand non-parameter macros here
var
  word:AnsiString;
  i:integer;
  lenLine: integer;
  newLine: AnsiString;

  procedure RemoveGCCAttribute;
  var
    level:integer;
  begin
    if (word='__attribute__') then begin
      while (i<= lenLine) and (Line[i] in [' ',#9]) do
        inc(i);
      if (i<=LenLine) and (Line[i]='(') then begin
        level:=0;
        while (i<= lenLine) do begin
          case Line[i] of
            '(': inc(level);
            ')': dec(level);
          end;
          inc(i);
          if (level=0) then
            break;
        end;
      end;
    end else begin
      newLine:=newLine+word;
    end;
  end;

begin
  newLine:='';
  word := '';
  lenLine := Length(Line);
  i:=1;
  while i<= lenLine do begin
    if Line[i] in ['_','a'..'z','A'..'Z','0'..'9'] then begin
      word:=word+Line[i];
    end else if (Line[i]='/') and (i+1<=lenLine) and (Line[i+1]='/') then begin //skip line end comment
      inc(i,2); //skip '//'
      while (i<=LenLine) and not (Line[i] in LineChars) do
        inc(i);
    end else begin
      if word<>'' then begin
        RemoveGCCAttribute;
      end;
      word :='';
      if i<= lenLine then
        newLine:= newLine+Line[i];
    end;
    inc(i);
  end;
  if word<>'' then begin
    RemoveGCCAttribute;
  end;
  Result := newLine;
end;

{ We also remove gcc's __attribtue__ here }
function TCppPreprocessor.ExpandMacros(const Line: AnsiString;const depth:integer): AnsiString; //we only expand non-parameter macros here
var
  word:AnsiString;
  i:integer;
  lenLine: integer;
  newLIne: AnsiString;

  procedure ExpandMacro;
  var
    argStart,argEnd,t:integer;
    level,Index:integer;
    define:PDefine;
    arg:string;
    formatParams:array of TVarRec;
  begin
    if (SameStr(word,'__attribute__')) then begin
      while (i<= lenLine) and (Line[i] in [' ',#9]) do
        inc(i);
      if (i<=LenLine) and (Line[i]='(') then begin
        level:=0;
        while (i<= lenLine) do begin
          case Line[i] of
            '(': inc(level);
            ')': dec(level);
          end;
          inc(i);
          if (level=0) then
            break;
        end;
      end;
    end else begin
      define:=GetDefine(word,index);
      if Assigned(define) and (define^.args='') and not (define^.IsMultiLine) then begin
        //newLine:=newLine+RemoveGCCAttributes(define^.Value);
        if define^.Value <> word then
          newLine:=newLine+ExpandMacros(define^.Value,depth+1)
        else
          newLine:=newLine+word;
      end else if Assigned(define) and not (define^.IsMultiLine) and (define^.Args<>'') then begin
        while (i<= lenLine) and (Line[i] in [' ',#9]) do
          inc(i);
        argStart:=-1;
        argEnd:=-1;
        if (i<=LenLine) and (Line[i]='(') then begin
          argStart:=i+1;
          level:=0;
          while (i<= lenLine) do begin
            case Line[i] of
              '(': inc(level);
              ')': dec(level);
            end;
            inc(i);
            if (level=0) then
              break;
          end;
          if level=0 then begin
            argEnd:=i-2;
            arg:=Trim(Copy(Line,argStart,argEnd-argStart+1));
            setLength(formatParams, define^.ArgList.Count);
            for t:=0 to define^.ArgList.Count-1 do begin
              formatParams[t].vtype := vtAnsiString;
              formatParams[t].VAnsiString := pointer(arg);
            end;

            newLine:=newLine+ExpandMacros(Format(define^.FormatValue,formatParams),depth+1);
          end;
        end;
      end else
        newLine:=newLine+word;
    end;
  end;

begin
  if depth > 20 then begin
    Result := Line;
    Exit;
  end;
  word := '';
  newLine:='';
  lenLine := Length(Line);
  i:=1;
  while i<= lenLine do begin
    if Line[i] in ['_','a'..'z','A'..'Z','0'..'9'] then begin
      word:=word+Line[i];
    end else begin
      if word<>'' then begin
        ExpandMacro;
      end;
      word :='';
      if i<= lenLine then
        newLine:= newLine+Line[i];
    end;
    inc(i);
  end;
  if word<>'' then begin
    ExpandMacro;
  end;
  Result := newLine;
end;

function TCppPreprocessor.RemoveSuffixes(const Input: AnsiString): AnsiString;
var
  I: integer;
begin
  Result := Input; // remove suffixes like L from integer values
  if Length(Input) > 0 then begin
    if not (Result[1] in ['0'..'9']) then
      Exit; // don't process names
    I := Length(Result);
    while (I >= 0) and (Result[i] in ['A'..'Z', 'a'..'z']) do // find first alphabetical character at end
      Dec(I);
    Delete(Result, I + 1, MaxInt); // remove from there
  end;
end;

function TCppPreprocessor.GetFileIncludesEntry(const FileName: AnsiString): PFileIncludes;
var
  I: integer;
begin
  Result := nil;
  i:=FastIndexOf(fIncludesList,FileName);
  if i<>-1 then begin
    Result := PFileIncludes(fIncludesList.Objects[I]);
  end;
end;

procedure TCppPreprocessor.PreprocessBuffer;
var
  S: AnsiString;
begin
  while fIncludes.Count > 0 do begin
    repeat
      S := GetNextPreprocessor;
      if StartsStr('#', S) then begin
        Simplify(S);
        if S <> '' then
          HandlePreprocessor(S);
      end;
    until S = '';
    CloseInclude;
  end;
end;

procedure TCppPreprocessor.PreprocessStream(const FileName: AnsiString; Stream: TMemoryStream);
begin
  fFileName:=FileName;
  Reset;
  OpenInclude(FileName, Stream);
  PreprocessBuffer;
end;

procedure TCppPreprocessor.PreprocessFile(const FileName: AnsiString);
begin
  fFileName:=FileName;
  Reset;
  OpenInclude(FileName, nil);
//  fBuffer.SaveToFile('f:\\buffer.txt');
  PreprocessBuffer;
//  fBuffer.SaveToFile('f:\\buffer.txt');
  //fResult.SaveToFile('f:\\log.txt');
  //fResult.SaveToFile('C:\TCppPreprocessorResult' + ExtractFileName(FileName) + '.txt');
end;

function TCppPreprocessor.GetResult: AnsiString;
begin
  Result := fResult.Text; // sloooow
end;

procedure TCppPreprocessor.DumpIncludesListTo(FileName:ansiString);
var
  i:integer;
  t:integer;
  FileIncludes:PFileIncludes;
  s:PStatement;
begin
  with TStringList.Create do try
    for i:=0 to fIncludesList.Count -1 do begin
      FileIncludes := PFileIncludes(fIncludesList.Objects[i]);
      Add(fIncludesList[i]+' : ');
      for t:=0 to FileIncludes^.IncludeFiles.Count-1 do begin
        Add(#9+'--'+FileIncludes^.IncludeFiles[t]);
      end;
      for t:=0 to FileIncludes^.DependingFiles.Count-1 do begin
        Add(#9+'^^'+FileIncludes^.DependingFiles[t]);
      end;
      for t:=0 to FileIncludes^.DependedFiles.Count-1 do begin
        Add(#9+'&&'+FileIncludes^.DependedFiles[t]);
      end;
      for t:=0 to FileIncludes^.Usings.Count-1 do begin
        Add(#9+'++'+FileIncludes^.Usings[t]);
      end;
      for t:=0 to FileIncludes^.Statements.Count-1 do begin
        S:=FileIncludes^.Statements[t];
        if not Assigned(S) then
          Continue;
        Add(#9+'**'+Format('%s , %s',[s^._Command, s^._FullName] ));
      end;
    end;
  finally
    SaveToFile(FileName);
    Free;
  end;
end;

procedure TCppPreprocessor.DumpDefinesTo(FileName:ansiString);
var
  i:integer;
  define:PDefine;
begin
  with TStringList.Create do try
    for i:=0 to fDefines.Count -1 do begin
      define := PDefine(fDefines.Objects[i]);
      if assigned(define) then begin
        Add(Format('%s %s %s %d',
          [define^.Name,define^.Args,define^.Value,integer(define^.HardCoded)]));
      end;
    end;
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TCppPreprocessor.InvalidDefinesInFile(const FileName:AnsiString);
var
  i,idx:integer;
  DefineList:TList;
  define: PDefine;
begin
  idx := FastIndexOf(fFileDefines,FileName);
  if idx<>-1 then begin
    i:=fDefines.Count-1;
    while (i>=0) do begin
      define:=PDefine(fDefines.objects[i]);
      if assigned(define) and SameText(define^.FileName,FileName) then begin
        fDefineIndex.Remove(define^.Name);
        fDefines.objects[i]:=nil;
      end;
      dec(i);
    end;
    DefineList := TList(fFileDefines.Objects[idx]);
    for i:=0 to DefineList.Count-1 do begin
      define:=PDefine(DefineList[i]);
      define^.ArgList.Free;
      Dispose(PDefine(define));
    end;
    DefineList.Free;
    fFileDefines.Delete(idx);
  end;
end;


end.

