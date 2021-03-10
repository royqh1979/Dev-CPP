{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTypes.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditTypes.pas,v 1.13 2004/07/29 19:24:40 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITTYPES}
unit SynEditTypes;
{$ENDIF}

{$I SynEdit.inc}

interface

const
  TSynSpecialChars = ['�'..'�', '�'..'�', '�'..'�'];
{  // utf8 identifier support
  TSynValidStringChars = ['_', '0'..'9', 'A'..'Z', 'a'..'z',#128..#255] + TSynSpecialChars;
}
  TSynValidStringChars = ['_', '0'..'9', 'A'..'Z', 'a'..'z'] + TSynSpecialChars;
  TSynWordBreakChars = ['.', ',', ';', ':', '"', '''', '!', '?', '[', ']', '(',
                        ')', '{', '}', '^', '-', '=', '+', '-', '*', '/', '\',
                        '|'];

  TSynTabChar = #9;
  SLineBreak = #13#10;

var
//These might need to be localized depending on the characterset because they might be
//interpreted as valid ident characters.
  SynTabGlyph:String;        //'�'
  SynSoftBreakGlyph:String;//'�'
  SynLineBreakGlyph:String; //'�'
  SynSpaceGlyph:String;    //'�'


type
  TSynSearchOption = (ssoMatchCase, ssoWholeWord, ssoBackwards,
    ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt,
    ssoRegExp);
  TSynSearchOptions = set of TSynSearchOption;

  TSynIdentChars = set of char;

  //NOTE: This will need to be localized and currently will not work will with
  //      MBCS languages like Japanese or Korean.

  PSynSelectionMode = ^TSynSelectionMode;
  TSynSelectionMode = (smNormal, smLine, smColumn);

  //todo: better field names. CharIndex and LineIndex?
  TBufferCoord = record
    Char: integer;
    Line: integer;
  end;

  TDisplayCoord = record
    Column: integer;
    Row: integer;
  end;

function DisplayCoord(AColumn, ARow: Integer): TDisplayCoord;
function BufferCoord(AChar, ALine: Integer): TBufferCoord;

implementation

uses
  Windows, sysutils;

function DisplayCoord(AColumn, ARow: Integer): TDisplayCoord;
begin
  Result.Column := AColumn;
  Result.Row := ARow;
end;

function BufferCoord(AChar, ALine: Integer): TBufferCoord;
begin
  Result.Char := AChar;
  Result.Line := ALine;
end;

function UTF8ToUnicode(s:AnsiString):WideString;
var
  lpWC : PWideChar;
  len:integer;
begin
  len := (Length(s)+1)*4;
  lpWC := AllocMem(len);
  MultibyteToWideChar(CP_UTF8,0,PChar(s),Length(s),
    lpWC,len);
  Result :=lpWC;
  FreeMem(lpWC);
end;

function UnicodeToAnsi(s:WideString):AnsiString;
var
  lpC : PChar;
  len:integer;
begin
  len := (Length(s)+1)*4;
  lpC := AllocMem(len);
  WideCharToMultibyte(CP_ACP,0,PWideChar(s),Length(s),
    lpC,len,nil,nil);
  Result :=lpC;
  FreeMem(lpC);
end;

function UTF8ToAnsi(s:String):String;
var
  ws: WideString;
begin
  ws := UTF8ToUnicode(s);
  Result:= UnicodeToAnsi(ws);
end;

initialization
begin
  SynTabGlyph := UnicodeToAnsi(#$2192);       //'�'
  SynSoftBreakGlyph := UnicodeToAnsi(#$2193); //'�'
  SynLineBreakGlyph := UnicodeToAnsi(#$2193); //'�'
  SynSpaceGlyph := '.';     //'�'

end;

end.
