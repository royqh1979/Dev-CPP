{
    This file is part of Dev-C++
    Copyright (c) 2020 royqh1979@gmail.com

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
}

unit Refactorer;

interface

uses
 Editor,devCFG,Project,Compiler;

type
  TRefactorer = class
  private
    fConfig: TdevRefactorer;
  public
    function IsValidIdentifier(word:String):boolean;
    constructor Create(config:TdevRefactorer);
    function RenameSymbol(Editor: TEditor;
      word: AnsiString; Target: TTarget; Project:TProject):AnsiString;
    function ParseErrorMessage(msg:AnsiString):AnsiString;
  end;

implementation

uses
  sysutils,SynEdit,utils,Classes;

const
  ValidIdentifierChars = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];

constructor TRefactorer.Create(config:TdevRefactorer);
begin
  fConfig :=config;
end;

function TRefactorer.IsValidIdentifier(word:AnsiString):boolean;
var
  i,Len:Integer;
begin
  Result := False;
  Len:=Length(word);
  if Len<1 then
    Exit;

  //valid identifier can't start with number
  if word[1] in ['0'..'9'] then
    Exit;

  for i:=1 to Len do
  begin
    if not (word[i] in ValidIdentifierChars) and (ord(word[i])<128) then
      Exit;
  end;
  Result:=True;
end;

function CalculateWordStartInUtf8(Editor:TEditor):integer;
var
  pos : TBufferCoord;
  i:integer;
  utf8Str: String;
  offset : integer;
  substr : String;
begin
  pos := Editor.Text.WordStart;
  offset :=0;
  for i:=0 to pos.Line-2 do begin
    utf8Str := AnsiToUTF8(Editor.Text.Lines[i]);
    offset := offset + length(utf8str) + 2;
  end;
  substr := copy(Editor.Text.Lines[pos.Line-1],1,pos.Char-1);
  utf8str := AnsiToUTF8(substr);
  offset := offset + length(utf8str)+1;
  Result := offset;
end;

function TRefactorer.RenameSymbol(Editor: TEditor;
  word: AnsiString; Target: TTarget; Project:TProject):AnsiString;
resourcestring
  cAppendStr = '%s -I"%s"';
var
  strList:TStringList;
  ErrorOutput : ansiString;
  FileName, WorkDir, RenameFileName: String;
  IncludesParams : ansiString;
  Cmd: ansiString;
  i: Integer;
  offset: Integer;
begin
  Result :='';
  WorkDir := IncludeTrailingPathDelimiter(ExtractFileDir(Editor.FileName));
  Filename := ExtractFileName(Editor.FileName);

  case GetFileTyp(FileName) of
    utcSrc,utcHead: begin
      RenameFileName := WorkDir  + '~devcpp-rename-temp.c';
      IncludesParams := FormatList(devCompilerSets.CompilationSet.CDir, cAppendStr);
    end;
    utCppSrc, utcppHead: begin
      RenameFileName := WorkDir  + '~devcpp-rename-temp.cpp';
      IncludesParams := FormatList(devCompilerSets.CompilationSet.CppDir, cAppendStr);
    end;
  else
    Exit;
  end;

//  IncludesParams := IncludesParams + ' -I"'+ExtractFileDir(Editor.FileName)+'" ';

  if (Target = ctProject) and assigned(Project) then
    for i := 0 to pred(Project.Options.Includes.Count) do
      if DirectoryExists(Project.Options.Includes[i]) then begin
        IncludesParams := format(cAppendStr, [IncludesParams, Project.Options.Includes[i]]);
      end;

  Editor.SaveFile(RenameFileName);
  try
    if Editor.UseUTF8 then begin
      offset := CalculateWordStartInUtf8(Editor);
    end else begin
      offset := Editor.Text.WordOffsetAtCursor;
    end;

    Cmd := devDirs.Exec + fConfig.RefactorerDir+fConfig.RenameFile
      +' -i --offset='+IntToStr(offset)+' --new-name="<__dev_cpp__rename__>"  "'
      +RenameFileName+'" -- '+IncludesParams+' -Wno-everything -target i686-pc-windows-gnu';

    if Editor.UseUTF8 then
      Cmd := Cmd + ' -finput-charset=utf-8'
    else
      Cmd := Cmd + ' -finput-charset='+GetSystemCharsetName;

    ErrorOutput:= RunAndGetOutput(Cmd, WorkDir, nil, nil, False);

    Result := Cmd + #13#10 + ErrorOutput;
    if ParseErrorMessage(ErrorOutput)<>'' then
      Exit;

    //MessageBox(Application.Handle,PAnsiChar(Cmd),     PChar( 'Look'), MB_OK);
    //MessageBox(Application.Handle,PAnsiChar(ErrorOutput),     PChar( 'Look'), MB_OK);

    StrList := TStringList.Create;
    try
      // Use replace selection trick to preserve undo list
      StrList.LoadFromFile(RenameFileName);
      if Editor.UseUTF8 then
        StrList.Text := UTF8ToAnsi(StrList.Text);
      // Use replace all functionality
      Editor.Text.BeginUpdate;
      try
        Editor.Text.SelectAll;
        Editor.Text.SelText := StringReplace(StrList.Text,'<__dev_cpp__rename__>',word,[rfReplaceAll]); // do NOT use Lines.LoadFromFile which is not undo-able
      finally
        Editor.Text.EndUpdate; // repaint once
      end;
    finally
      StrList.Free;
    end;
  finally
    DeleteFile(RenameFileName);
  end;
end;

  function TRefactorer.ParseErrorMessage(msg:AnsiString):AnsiString;
  var
    lines: TStringList;
    i: Integer;
  begin
    Result:='';
    lines := TStringList.Create;
    try
      lines.Text := msg;
      i := lines.Count -1 ;
      while i>=0 do
      begin
        if StartsStr('ERROR:',lines[i]) then begin
          Result:=lines[i];
          break;
        end;
        dec(i);
      end;
    finally
      lines.Free;
    end;
  end;


end.

