{
    This file is part of Dev-C++
    Copyright (c) 2020 royqh1979@gmail.com

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
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
 Editor,devCFG,SynEdit,Project,Compiler,CppParser;

type
  TRefactorer = class
  private
    fConfig: TdevRefactorer;
    fCppParser: TCppParser;

  public
    constructor Create(config:TdevRefactorer; Parser:TCppParser);
    function RenameSymbol(Editor: TEditor;   OldCaretXY:TBufferCoord;
      oldName,newName: AnsiString; Target: TTarget; Project:TProject):AnsiString;
    property CppParser: TCppParser read fCppParser;
  end;

implementation

uses
  sysutils,CBUtils,SynEditTextBuffer,SynEditHighlighter,utils,Classes,
  MultiLangSupport;

constructor TRefactorer.Create(config:TdevRefactorer;Parser:TCppParser);
begin
  fConfig :=config;
  fCppParser := Parser;
end;

function TRefactorer.RenameSymbol(Editor: TEditor; OldCaretXY:TBufferCoord;
  oldName,newName: AnsiString; Target: TTarget; Project:TProject):AnsiString;
var
  Lines:TSynEditStringList;
  newLines : TStringList;
  PosY:integer;
  CurrentNewLine,phrase,newphrase : string;
  pOldStatement,pNewStatement: PStatement;
  oldStatement : TStatement;
  M: TMemoryStream;

  procedure ProcessLine;
  var
    Line,Token:String;
    start:integer;
    p: TBufferCoord;
    statement: PStatement;
  begin
    CurrentNewLine := '';
    Line := Lines[PosY];
    if Line = '' then
      Exit;

    if PosY = 0 then
      Editor.Text.Highlighter.ResetRange
    else
      Editor.Text.Highlighter.SetRange(Lines.Ranges[PosY - 1]);
    Editor.Text.Highlighter.SetLine(Line, PosY);
    while not Editor.Text.Highlighter.GetEol do begin
        Start := Editor.Text.Highlighter.GetTokenPos + 1;
        Token := Editor.Text.Highlighter.GetToken;
        if SameStr(oldname,Token) then begin
          //same name symbol , test if the same statement;
          p.Line := PosY+1;
          p.Char := Start;
          phrase := Editor.GetWordAtPosition(p,wpInformation);
          statement := CppParser.FindStatementOf(
            Editor.FileName,
            phrase, p.Line, M);
          if assigned(statement) and ((statement^._DefinitionFileName = oldStatement._DefinitionFileName)
            and (statement^._DefinitionLine = oldStatement._DefinitionLine)) then // same statement
            CurrentNewLine := Concat(CurrentNewLine,NewName)
          else
            CurrentNewLine := Concat(CurrentNewLine,Token);
        end else begin
          //not same name symbol
          CurrentNewLine := Concat(CurrentNewLine,Token);
        end;
        Editor.Text.Highlighter.Next;
      end;
  end;
begin
//TODO: 1. 检查定义是否在本文件中（单文件） 或者 在本项目文件列表中（项目）
//TODO: 2.修改项目中其他文件（定义所在文件放在最后修改）
  Result:='';
  //Test if newword is a valid id
  if not IsIdentifier(newName) then begin
    Result := Format(Lang[ID_ERR_NOT_IDENTIFIER],[newName]);
    Exit;
  end;

  //Test if newName is a C++ keyword
  if IsKeyword(newName) then begin
    Result := Format(Lang[ID_ERR_IS_KEYWORD],[newName]);
    Exit;
  end;

  Lines := Editor.Text.Lines;
  if Lines.Count<1 then
    Exit;
  newLines := TStringList.Create;
  M := TMemoryStream.Create;
  try
    Lines.SaveToStream(M);
    // get full phrase (such as s.name instead of name)
    phrase := Editor.GetWordAtPosition(oldCaretXY,wpInformation);
    // Find it's definition
    pOldStatement := CppParser.FindStatementOf(
      Editor.FileName,
      phrase, oldCaretXY.Line, M);
    // definition of the old name is not found
    if not Assigned(pOldStatement) then begin
      Result := Format(Lang[ID_ERR_STATEMENT_NOT_FOUND],[phrase]);
      Exit;
    end;
    // found but not in this file
    if not SameStr(pOldStatement^._DefinitionFileName, Editor.FileName) then begin
      Result := Format(Lang[ID_ERR_STATEMENT_OUT_OF_BOUND],[phrase,Editor.FileName]);
      Exit;
    end;
    oldStatement := pOldStatement^; // save it  (cause statement node may change each time of find)
    // check if newWord is duplicate with existing definitions
    newphrase :=Copy(phrase,1,Length(phrase)-Length(oldName)) + newName;
    pNewStatement := CppParser.FindStatementOf(
      Editor.FileName,
      newphrase, oldCaretXY.Line, M);
    if Assigned(pNewStatement) then begin // definition with same name existing
      Result := Format(Lang[ID_ERR_STATEMENT_EXISTING],[newphrase,pNewStatement^._DefinitionFileName,
        pNewStatement^._DefinitionLine]);
      Exit;
    end;
    PosY := 0;
    while (PosY < Lines.Count) do begin
      ProcessLine;
      newLines.Add(currentNewLine);
      inc(PosY);
    end;
    // Use replace all functionality
    Editor.Text.BeginUpdate;
    try
      Editor.Text.SelectAll;
      Editor.Text.SelText := newLines.Text;
    finally
      Editor.Text.EndUpdate; // repaint once
    end;
  finally
    M.Free;
    newLines.Free;
  end;

end;

end.

