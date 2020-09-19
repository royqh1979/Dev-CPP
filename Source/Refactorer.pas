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
    function RenameSymbol(Editor: TEditor; offset: Integer;
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
    if not (word[i] in ValidIdentifierChars) then
      Exit;
  end;
  Result:=True;
end;

function TRefactorer.RenameSymbol(Editor: TEditor; offset: Integer;
  word: AnsiString; Target: TTarget; Project:TProject):AnsiString;
resourcestring
  cAppendStr = '%s -I"%s"';
var
  DummyEditor : TSynEdit;
  ErrorOutput : ansiString;
  FileName, WorkDir, RenameFileName: String;
  IncludesParams : ansiString;
  Cmd: ansiString;
  i: Integer;
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

  Editor.Text.Lines.SaveToFile(RenameFileName);
  try
    Cmd := devDirs.Exec + fConfig.RefactorerDir+fConfig.RenameFile
      +' -i --offset='+IntToStr(offset)+' --new-name='+word+' "'
      +RenameFileName+'" -- '+IncludesParams+' -D__GNUC__';
    ErrorOutput:= RunAndGetOutput(Cmd, WorkDir, nil, nil, False);

    Result := Cmd + #13#10 + ErrorOutput;
    if ParseErrorMessage(ErrorOutput)<>'' then
      Exit;

    //MessageBox(Application.Handle,PAnsiChar(Cmd),     PChar( 'Look'), MB_OK);
    //MessageBox(Application.Handle,PAnsiChar(ErrorOutput),     PChar( 'Look'), MB_OK);

    DummyEditor := TSynEdit.Create(nil);
    try
      // Use replace selection trick to preserve undo list
      DummyEditor.Lines.LoadFromFile(RenameFileName);
      // Use replace all functionality
      Editor.Text.BeginUpdate;
      try
        Editor.Text.SelectAll;
        Editor.Text.SelText := DummyEditor.Lines.Text; // do NOT use Lines.LoadFromFile which is not undo-able
      finally
        Editor.Text.EndUpdate; // repaint once
      end;
    finally
      DummyEditor.Free;
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

