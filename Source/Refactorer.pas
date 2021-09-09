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
 Editor,devCFG,SynEdit,Project,Compiler,CppParser, Dialogs, CBUtils;

type

  TRefactorer = class
  private
    fConfig: TdevRefactorer;
    fCppParser: TCppParser;
    function CheckNotSpanTokens(FileName:AnsiString;edit: TSynEdit;
      startLine,startChar,endChar:integer):boolean;
    function CheckOnlyLiteralAndFunc(FileName:AnsiString;edit: TSynEdit;
      startLine,startChar,endChar:integer):boolean;

   function RenameSymbolInFile(const FileName: AnsiString; pOldStatement:PStatement;
      oldName,newName: AnsiString):boolean;
  public
    constructor Create(config:TdevRefactorer; Parser:TCppParser);
    function RenameSymbol(Editor: TEditor;   OldCaretXY:TBufferCoord;
      oldName,newName: AnsiString; Project:TProject):boolean;
    function TestExtractMacro(e: TEditor):boolean;
    {
    function GuessConstType(e:TEditor):AnsiString;
    }
    function ExtractMacro(e:TEditor; MacroName:AnsiString):boolean;
    property CppParser: TCppParser read fCppParser;
  end;

implementation

uses
  sysutils,SynEditTextBuffer,SynEditHighlighter,utils,Classes,
  MultiLangSupport, DataFrm, main;

constructor TRefactorer.Create(config:TdevRefactorer;Parser:TCppParser);
begin
  fConfig :=config;
  fCppParser := Parser;
end;

function TRefactorer.RenameSymbolInFile(const FileName: AnsiString;   pOldStatement:PStatement;
      oldName,newName: AnsiString):boolean;
var
  Lines:TSynEditStringList;
  DummyEditor :TSynEdit;
  PosY:integer;
  CurrentNewLine,phrase: string;
  Editor:TSynEdit;
  ownEditor:boolean;
  pBeginPos,pEndPos : TBufferCoord;
  OldCaretXY: TBufferCoord;
  OldTopLine: integer;
  changed : boolean;

  function ProcessLine:boolean;
  var
    Line,Token:String;
    start:integer;
    p: TBufferCoord;
    statement: PStatement;
  begin
    Result := False;
    CurrentNewLine := '';
    Line := Lines[PosY];
    if Line = '' then
      Exit;

    if PosY = 0 then
      Editor.Highlighter.ResetRange
    else
      Editor.Highlighter.SetRange(Lines.Ranges[PosY - 1]);
    Editor.Highlighter.SetLine(Line, PosY);
    while not Editor.Highlighter.GetEol do begin
        Start := Editor.Highlighter.GetTokenPos + 1;
        Token := Editor.Highlighter.GetToken;
        if SameStr(oldname,Token) then begin
          //same name symbol , test if the same statement;
          p.Line := PosY+1;
          p.Char := Start;
          phrase := GetWordAtPosition(Editor, p, pBeginPos,pEndPos, wpInformation);
          statement := CppParser.FindStatementOf(
            FileName,
            phrase, p.Line);
          if assigned(statement) and ((statement^._FileName = pOldStatement^._FileName)
            and (statement^._Line = pOldStatement^._Line)) then begin// same statement
            CurrentNewLine := Concat(CurrentNewLine,NewName);
            Result := True;
          end else
            CurrentNewLine := Concat(CurrentNewLine,Token);
        end else begin
          //not same name symbol
          CurrentNewLine := Concat(CurrentNewLine,Token);
        end;
        Editor.Highlighter.Next;
      end;
  end;

begin
  Result:=False;
  ownEditor := False;
  if mainForm.EditorList.IsFileOpened(FileName) then begin
    Editor := mainForm.EditorList.GetEditorFromFileName(FileName).Text;
  end else begin
    ownEditor := True;
    Editor := TSynEdit.Create(nil);
    devEditor.AssignEditor(editor,FileName);
    with TStringList.Create do try
      LoadFromFile(FileName);
      editor.Lines.Text:=Text;
    finally
      Free;
    end;
  end;

  OldTopLine := Editor.TopLine;
  OldCaretXY := Editor.CaretXY;

  Lines := Editor.Lines;
  if Lines.Count<1 then
    Exit;

  changed := False;
  DummyEditor := TSynEdit.Create(nil);
  try
    PosY := 0;
    while (PosY < Lines.Count) do begin
      changed := ProcessLine or changed;
      DummyEditor.Lines.Add(currentNewLine);
      inc(PosY);
    end;

    // remove added last new line
    // Use replace all functionality

    // only save if changed
    if changed then begin
      Editor.BeginUpdate;
      try
        Editor.SelectAll;
        Editor.SelText := DummyEditor.Lines.Text;
        Editor.TopLine := OldTopLine;
        Editor.CaretXY := OldCaretXY;
      finally
        Editor.EndUpdate; // repaint once
      end;

      if ownEditor then begin
        with TStringList.Create do try
          Text:=editor.Lines.Text;
          SaveToFile(FileName);
        finally
          Free;
        end;
      end else begin
        //mainForm.EditorList.GetEditorFromFileName(FileName).Save;
      end;
    end;
    Result := True;
  finally
    DummyEditor.Free;
    if ownEditor then begin
      Editor.Free;
    end;
  end;

end;

function TRefactorer.RenameSymbol(Editor: TEditor; OldCaretXY:TBufferCoord;
  oldName,newName: AnsiString; Project:TProject):boolean;
var
  Lines:TSynEditStringList;
  phrase,newphrase,oldScopeName : string;
  pOldStatement,pNewStatement: PStatement;
  oldStatement : TStatement;
  i:integer;
  pBeginPos,pEndPos : TBufferCoord;

  function getFullName(statement:PStatement):AnsiString;
  begin
    if Assigned(statement) then
      Result := statement^._FullName
    else
      Result := '';
  end;

begin
//TODO: 1. ��鶨���Ƿ��ڱ��ļ��У����ļ��� ���� �ڱ���Ŀ�ļ��б��У���Ŀ��
//TODO: 2.�޸���Ŀ�������ļ������������ļ���������޸ģ�
  Result:=False;
  //Test if newword is a valid id
  if not IsIdentifier(newName) then begin
    MessageDlg(Format(Lang[ID_ERR_NOT_IDENTIFIER],[newName]), mtInformation, [mbOK], 0);
    Exit;
  end;

  //Test if newName is a C++ keyword
  if IsKeyword(newName) then begin
    MessageDlg(Format(Lang[ID_ERR_IS_KEYWORD],[newName]), mtInformation, [mbOK], 0);
    Exit;
  end;

  Lines := Editor.Text.Lines;
  if Lines.Count<1 then
    Exit;

  if not Editor.CppParser.Freeze then
    Exit;
  try
    // get full phrase (such as s.name instead of name)
    phrase := GetWordAtPosition(Editor.Text,oldCaretXY,pBeginPos,pEndPos,wpInformation);
    // Find it's definition
    pOldStatement := CppParser.FindStatementOf(
      Editor.FileName,
      phrase, oldCaretXY.Line);
    // definition of the old name is not found
    if not Assigned(pOldStatement) then begin
      MessageDlg(Format(Lang[ID_ERR_STATEMENT_NOT_FOUND],[phrase]), mtInformation, [mbOK], 0);
      Exit;
    end;
    oldStatement := pOldStatement^; // save it  (cause statement node may change each time of find)
    oldScopeName := getFullName(pOldStatement^._ParentScope);
    // check if newWord is duplicate with existing definitions
    newphrase :=Copy(phrase,1,Length(phrase)-Length(oldName)) + newName;
    pNewStatement := CppParser.FindStatementOf(
      Editor.FileName,
      newphrase, oldCaretXY.Line);
    if Assigned(pNewStatement) and (getFullName(pNewStatement^._ParentScope) = oldScopeName) then begin // definition with same name existing
      MessageDlg(Format(Lang[ID_ERR_STATEMENT_EXISTING],[newphrase,pNewStatement^._DefinitionFileName,
        pNewStatement^._DefinitionLine]), mtInformation, [mbOK], 0);
      Exit;
    end;


  if assigned(project) and (project.Units.IndexOf(editor.FileName)>=0) then begin
    // found but not in this project
    if not (project.Units.IndexOf(oldStatement._FileName)>=0) then begin
      MessageDlg(Format(Lang[ID_ERR_STATEMENT_NOT_IN_PROJECT],[phrase,oldStatement._FileName]), mtInformation, [mbOK], 0);
      Exit;
    end;
    if not (project.Units.IndexOf(oldStatement._DefinitionFileName)>=0) then begin
      MessageDlg(Format(Lang[ID_ERR_STATEMENT_NOT_IN_PROJECT],[phrase,oldStatement._DefinitionFileName]), mtInformation, [mbOK], 0);
      Exit;
    end;
    if (oldStatement._Scope = ssLocal) then begin
      if (oldStatement._Kind = skVariable) then begin
        Result:=RenameSymbolInFile(oldStatement._FileName,@oldStatement,oldName,newName);
      end else begin
        if (not assigned(oldStatement._ParentScope)) then begin
          // should not happen
          MessageDlg('Parser Error: parameter no parent', mtInformation, [mbOK], 0);
          Exit;
        end;
        if not (project.Units.IndexOf(oldStatement._ParentScope^._FileName)>=0) then begin
          MessageDlg(Format(Lang[ID_ERR_STATEMENT_NOT_IN_PROJECT],[phrase,oldStatement._ParentScope^._FileName]), mtInformation, [mbOK], 0);
          Exit;
        end;
        if not (project.Units.IndexOf(oldStatement._ParentScope^._DefinitionFileName)>=0) then begin
          MessageDlg(Format(Lang[ID_ERR_STATEMENT_NOT_IN_PROJECT],[phrase,oldStatement._ParentScope^._DefinitionFileName]), mtInformation, [mbOK], 0);
          Exit;
        end;
        Result:=RenameSymbolInFile(oldStatement._ParentScope^._FileName,@oldStatement,oldName,newName);
        if not SameText(oldStatement._ParentScope^._FileName, oldStatement._ParentScope^._DefinitionFileName) then
          Result:=RenameSymbolInFile(oldStatement._ParentScope^._DefinitionFileName,@oldStatement,oldName,newName);
      end;
    end else begin
      for i:=0 to project.Units.Count-1 do begin
        if not SameText(project.Units[i].FileName,oldStatement._FileName) then begin
          Result:=RenameSymbolInFile(project.Units[i].FileName,@oldStatement,oldName,newName);
          if not Result then
            Exit;
        end;
      end;
      Result:=RenameSymbolInFile(oldStatement._FileName,@oldStatement,oldName,newName);
    end;
  end else begin
    // found but not in this file
    if not SameText(oldStatement._FileName, Editor.FileName)
      or not SameText(oldStatement._DefinitionFileName, Editor.FileName) then begin
      MessageDlg(Format(Lang[ID_ERR_STATEMENT_OUT_OF_BOUND],[phrase,Editor.FileName]), mtInformation, [mbOK], 0);
      Exit;
    end;
    Result:=RenameSymbolInFile(editor.FileName,@oldStatement,oldName,newName);
  end;

  finally
    Editor.CppParser.UnFreeze;
  end;
end;

function TRefactorer.CheckNotSpanTokens(FileName:AnsiString;edit: TSynEdit;
  startLine,startChar,endChar:integer):boolean;
var
  PosX, PosY, endPos, Start: integer;
  Line,Token: string;
  Attri: TSynHighlighterAttributes;
  bracketLevel,i : integer;
begin
  Result:=False;
  dec(endChar);
  with edit do begin
    PosY := startLine - 1;
    if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then begin
      Line := Lines[PosY];
      if PosY = 0 then
        Highlighter.ResetRange
      else
        Highlighter.SetRange(Lines.Ranges[PosY - 1]);
      Highlighter.SetLine(Line, PosY);
      PosX := startChar;
      bracketLevel := 0;
      start:=PosX;
      if (PosX > 0) and (PosX <= Length(Line)) then begin
        while (not Highlighter.GetEol) and (Start<=endChar) do begin
          Start := Highlighter.GetTokenPos + 1;
          Token := Highlighter.GetToken;
          endPos := Start + Length(Token)-1;
          Attri := Highlighter.GetTokenAttribute;
          if (Attri = dmMain.Cpp.DirecAttri) then begin// we are on Preprocessor Line
            Exit;
          end;
          if ( (start<startChar) and (endPos>=startChar) )
            or ( (start<=endChar) and (endPos>endChar) ) then begin // we are span a token
            if Attri <> dmMain.Cpp.SpaceAttri then //and the token is not whitespaces
              Exit;
          end;
          if (start>=startChar) and (endPos<=endChar) then begin
            if (start=startChar) and (endPos=endChar) then begin
              if (Attri = dmMain.Cpp.CommentAttri) then begin // selection is a comment
                Exit;
              end;
            end;
            if (Attri <> dmMain.Cpp.StringAttri)
              and (Attri <> dmMain.Cpp.CommentAttri)
              and (Attri <> dmMain.Cpp.CharAttri) then begin
              for i:= start to endPos do begin
                if Line[i] in ['('] then begin
                  inc(bracketLevel);
                end else if Line[i] in [')'] then begin
                  inc(bracketLevel);
                end else if Line[i] in [';','{','}','[',']'] then begin
                  Exit;
                end;
              end;
            end;
          end;
          Highlighter.Next;
        end;
        if bracketLevel <> 0 then begin
          Exit;
        end;
        Result:=True;
      end;
    end;
  end;
end;

function TRefactorer.CheckOnlyLiteralAndFunc(FileName:AnsiString;edit: TSynEdit;
  startLine,startChar,endChar:integer):boolean;
var
  PosX, PosY, endPos, Start: integer;
  statement: PStatement;
  Line,Token: string;
  Attri: TSynHighlighterAttributes;
begin
  Result:=True;
  dec(endChar);
  with edit do begin
    PosY := startLine - 1;
    if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then begin
      Line := Lines[PosY];
      if PosY = 0 then
        Highlighter.ResetRange
      else
        Highlighter.SetRange(Lines.Ranges[PosY - 1]);
      Highlighter.SetLine(Line, PosY);
      PosX := startChar;
      start:=PosX;
      if (PosX > 0) and (PosX <= Length(Line)) then begin
        while (not Highlighter.GetEol) and (Start<=endChar) do begin
          Start := Highlighter.GetTokenPos + 1;
          Token := Highlighter.GetToken;
          endPos := Start + Length(Token)-1;
          Attri := Highlighter.GetTokenAttribute;
          if (start>=startChar) and (endPos<=endChar) then begin
            if (Attri = dmMain.Cpp.IdentifierAttri) then begin
              statement := fCppParser.FindStatementStartingFrom(FileName,token,nil); //only global functions are allowed
              if not assigned(statement) or (statement^._Kind <> skFunction) then
                Result := False;
                Exit;
            end
          end;
          Highlighter.Next;
        end;
        Result:=True;
      end;
    end;
  end;
end;


function TRefactorer.TestExtractMacro(e: TEditor):boolean;
begin
  Result:=False;
  if not Assigned(e) then begin
    Exit;
  end;
  with e.Text do begin
    if (Trim(SelText) = '') then begin
      Exit;
    end;
    if (BlockBegin.Line <> BlockEnd.Line) then begin// we don't process multiline selection
      MessageDlg(Lang[ID_ERR_MULTI_LINE_SELECTED], mtInformation, [mbOK], 0);
      Exit;
    end;
    Result := CheckNotSpanTokens(e.FileName,e.Text,BlockBegin.Line,
      BlockBegin.Char,BlockEnd.Char);
    if not Result then begin
      MessageDlg(Lang[ID_ERR_SELECTION_NOT_COMPLETE], mtInformation, [mbOK], 0);
      Exit;
    end;
    Result := CheckOnlyLiteralAndFunc(e.FileName,e.Text,BlockBegin.Line,
      BlockBegin.Char,BlockEnd.Char);
    if not Result then begin
      MessageDlg(Lang[ID_ERR_SELECTION_HAS_INVALID_IDENTIFIER], mtInformation, [mbOK], 0);
      Exit;
    end;
  end;
end;
{
function TRefactorer.GuessConstType(e:TEditor):AnsiString;
var
  s:AnsiString;
begin
  s:=Trim(e.Text.SelText);
  if (s[1]='"') or (s[Length(s)]='"') then
    Result := 'char *'
  else if (s[1]='''') or (s[Length(s)]='''') then
    Result := 'char'
  else if Pos('.',s)>0 then
    Result := 'double'
  else
    Resuls := 'int';
end;
}
function TRefactorer.ExtractMacro(e:TEditor; MacroName:AnsiString):boolean;
var
  s:AnsiString;
  newName : AnsiString;
  statement:PStatement;
  caretXY,  insertXY:TBufferCoord;
  insertLine : integer;

begin
  Result := False;
  s:=Trim(e.Text.SelText);
  newName := Trim(MacroName);
  if newName = '' then begin
    MessageDlg(Lang[ID_ERR_EMPTY_NAME], mtInformation, [mbOK], 0);
    Exit;
  end;

  if not IsIdentifier(newName) then begin
    MessageDlg(Format(Lang[ID_ERR_NOT_IDENTIFIER], [newName]), mtInformation, [mbOK], 0);
    Exit;
  end;

  //Test if newName is a C++ keyword
  if IsKeyword(newName) then begin
    MessageDlg(Format(Lang[ID_ERR_IS_KEYWORD], [newName]), mtInformation, [mbOK], 0);
    Exit;
  end;

  statement := fCppParser.FindStatementStartingFrom(e.FileName,newName,nil);
  if Assigned(statement) then begin
    MessageDlg(Format(Lang[ID_ERR_MACRO_EXISTS], [newName]), mtInformation, [mbOK], 0);
    Exit;
  end;
  insertLine := e.Text.BlockBegin.Line;
  while insertLine > 0 do begin
    if startsStr('#',e.Text.Lines[insertLine-1]) then
      break;
    dec(insertLine);
  end;
  inc(insertLine);
  insertXY.Line := insertLine;
  insertXY.Char := 1;
  e.Text.SelText := newName;
  caretXY := e.Text.CaretXY;
  e.Text.CaretXY := insertXY;
  e.InsertString('#define '+newName+' ('+s+')'#10,False);
  inc(caretXY.Line);
  e.Text.CaretXY := caretXY;
  Result := True;
end;

end.

