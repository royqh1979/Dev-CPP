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
    function IsValidIdentifier(word:String):boolean;
    constructor Create(config:TdevRefactorer);
    function RenameSymbol(Editor: TEditor;   OldCaretXY:TBufferCoord;
      oldName,newName: AnsiString; Target: TTarget; Project:TProject):AnsiString;
    function ParseErrorMessage(msg:AnsiString):AnsiString;
    property CppParser: TCppParser read fCppParser write fCppParser;
  end;

implementation

uses
  main,sysutils,CBUtils,SynEditTextBuffer,SynEditHighlighter,utils,Classes;

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

function TRefactorer.RenameSymbol(Editor: TEditor; OldCaretXY:TBufferCoord;
  oldName,newName: AnsiString; Target: TTarget; Project:TProject):AnsiString;
var
  Lines:TSynEditStringList;
  newLines : TStringList;
  PosY:integer;
  CurrentNewLine,phrase : string;
  pOldStatement : PStatement;
  oldStatement : TStatement;
  M: TMemoryStream;

  procedure ProcessLine;
  var
    Line,Token:String;
    start:integer;
    Attri: TSynHighlighterAttributes;
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
        Attri := Editor.Text.Highlighter.GetTokenAttribute;
        if SameStr(oldname,Token) then begin
          //same name symbol , test if the same statement;
          p.Line := PosY+1;
          p.Char := Start;
          phrase := Editor.GetWordAtPosition(p,wpInformation);
          statement := MainForm.CppParser.FindStatementOf(
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
  Result:='';
  Lines := Editor.Text.Lines;
  if Lines.Count<1 then
    Exit;
  newLines := TStringList.Create;
  M := TMemoryStream.Create;
  try
    Lines.SaveToStream(M);
    phrase := Editor.GetWordAtPosition(oldCaretXY,wpInformation);
    pOldStatement := MainForm.CppParser.FindStatementOf(
      Editor.FileName,
      phrase, oldCaretXY.Line, M);
    if not Assigned(pOldStatement) then
      Exit;
    oldStatement := pOldStatement^;
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

