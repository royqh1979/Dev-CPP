{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCpp.pas, released 2000-04-10.
The Original Code is based on the dcjCppSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Trier.
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

$Id: SynHighlighterCpp.pas,v 1.24 2005/01/28 16:53:21 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a C++ syntax highlighter for SynEdit)
@author(Michael Trier)
@created(1998)
@lastmod(2001-11-21)
The SynHighlighterCpp unit provides SynEdit with a C++ syntax highlighter.
Thanks to Martin Waldenburg.
}

{$IFNDEF QSYNHIGHLIGHTERCPP}
unit SynHighlighterLua;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkStringEscapeSeq, tkSymbol, tkUnknown,
    tkFloat, tkHex, tkOctal, tkLabel);

  TxtkTokenKind = (
    xtkAdd, xtkSub, xtkMul,xtkDiv, xtkMod, xtkPow,xtkLen,
    xtkBAnd, xtkBOr, xtkBNot, xtkShl, xtkShr, xtkIDiv,
    xtkEq, xtkNotEq,xtkLe,xtkLt,xtkGe,xtkGt, xtkAssign,
    xtkRoundOpen,xtkRoundClose, xtkBraceOpen,xtkBraceClose,
    xtkSquareOpen,xtkSquareClose, xtkBind,
    xtkSemiColon, xtkColon, xtkComma,
    xtkPeriod, xtkConcat, xtkVarArg);

  {
    rsUnknow: all other characters don't need highlight
    rsDirective: C/CPP precompile directives like #include <iostream>
    rsDirectiveComment: ANSI-Style Comment embedded in Directive
    rsMultiLineDirective:  C/CPP precompile directives breaked to multiple lines
    rsAnsiC: ANSI-Style c comment like /* xxx */
    rsCppComment: cpp style comment like //xxx
    rsString: string like "xxx"
    rsMultiLineString: a string on multiline    like
        "xxxxx \
        xxxxx"
  }
  TRangeState = (rsUnknown, rsString,
    rsMultiLineString, rsComment, rsMultiLineComment, 
    rsStringEscapeSeq, rsMultiLineStringEscapeSeq,
    rsSpace,rsLabel);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TSynHighlighterLua = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fSpaceRange: TRangeState;
    fParenthesisLevel: integer;
    fBracketLevel: integer;
    fBraceLevel:integer;
    fStringEndSymbol : string;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fLineNumber: Integer;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fOctalAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fStringEscapeSeqAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fClassAttri: TSynHighlighterAttributes;
    fGlobalVarAttri: TSynHighlighterAttributes;
    fLocalVarAttri : TSynHighlighterAttributes;
    fLabelAttri: TSynHighlighterAttributes;
    procedure CommentProc;
    procedure CommentStartProc;
    procedure AndSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure LengthProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure LabelProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure CaretProc;
    procedure UnknownProc;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure StringStartProc;
    procedure StringEscapeSeqProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetExtTokenID: TxtkTokenKind;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetBraceLevel: integer; override;
    function GetBracketLevel: integer; override;
    function GetParenthesisLevel: integer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function GetTokenFinished: boolean; override;
    function GetIsLastLineCommentNotFinish(value:Pointer):boolean; override;
    function GetIsLastLineStringNotFinish(value:Pointer):boolean; override;
    function GetTokenType: TSynHighlighterTokenType; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure SetParenthesisLevel(Value: integer); override;
    procedure SetBracketLevel(Value: integer); override;
    procedure SetBraceLevel(Value: integer); override;
    procedure ResetRange; override;
    procedure ResetParenthesisLevel; override;
    procedure ResetBracketLevel; override;
    procedure ResetBraceLevel; override;
    function UseUserSettings(settingIndex: integer): boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property FloatAttri: TSynHighlighterAttributes read fFloatAttri
      write fFloatAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri
      write fHexAttri;
    property OctalAttri: TSynHighlighterAttributes read fOctalAttri
      write fOctalAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property StringEscapeSeqAttri: TSynHighlighterAttributes read fStringEscapeSeqAttri
      write fStringEscapeSeqAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property LabelAttri: TSynHighlighterAttributes read fLabelAttri
      write fLabelAttri;

  end;



implementation

uses
  iniFiles,
  Windows,
  SynEditStrConst;

var
  Identifiers: array[#0..#255] of ByteBool;
  // mHashTable: array[#0..#255] of Integer;
  LuaKeywords : TStringHash;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    {
    Case I in['_', 'a'..'z', 'A'..'Z'] of
      True:
        begin
          if (I > #64) and (I < #91) then mHashTable[I] := Ord(I) - 64 else
            if (I > #96) then mHashTable[I] := Ord(I) - 95;
        end;
    else mHashTable[I] := 0;
    end;
    }
  end;
end;


function TSynHighlighterLua.IdentKind(MayBe: PChar): TtkTokenKind;
var
  ToHash : PChar;
  fStringLen : integer;
  Key:String;
begin
  fToIdent := MayBe; { things like 'int a;' }
  ToHash := fToIdent;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
  SetString(Key, fToIdent, fStringLen); { we need to get 'int' }
  if LuaKeywords.ValueOf(Key) = -1 then
    Result := tkIdentifier
  else
    Result := tkKey;
end;

procedure TSynHighlighterLua.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := AndSymbolProc;
      #39: fProcTable[I] := StringStartProc;
      '}': fProcTable[I] := BraceCloseProc;
      '{': fProcTable[I] := BraceOpenProc;
      #13: fProcTable[I] := CRProc;
      ':': fProcTable[I] := ColonProc;
      ',': fProcTable[I] := CommaProc;
      '#': fProcTable[I] := LengthProc;
      '=': fProcTable[I] := EqualProc;
      '>': fProcTable[I] := GreaterProc;
      'A'..'Z', 'a'..'z', '_' : fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := MinusProc;
      '%': fProcTable[I] := ModSymbolProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      '|': fProcTable[I] := OrSymbolProc;
      '+': fProcTable[I] := PlusProc;
      '.': fProcTable[I] := PointProc;
      ')': fProcTable[I] := RoundCloseProc;
      '(': fProcTable[I] := RoundOpenProc;
      ';': fProcTable[I] := SemiColonProc;
      '/': fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      ']': fProcTable[I] := SquareCloseProc;
      '[': fProcTable[I] := SquareOpenProc;
      '*': fProcTable[I] := StarProc;
      #34: fProcTable[I] := StringStartProc;
      '~': fProcTable[I] := TildeProc;
      '^': fProcTable[I] := CaretProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynHighlighterLua.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat);
  AddAttribute(fFloatAttri);
  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal);
  AddAttribute(fHexAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar);
  AddAttribute(fInvalidAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fOctalAttri := TSynHighlighterAttributes.Create(SYNS_AttrOctal);
  AddAttribute(fOctalAttri);
  fLabelAttri := TSynHighlighterAttributes.Create(SYNS_AttrLabel);
  AddAttribute(fLabelAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  fSpaceAttri.Foreground := clWindow;
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fStringEscapeSeqAttri := TSynHighlighterAttributes.Create(SYNS_AttrStringEscapeSequences);
  AddAttribute(fStringEscapeSeqAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  MakeMethodTables;
  fRange := rsUnknown;
  fSpaceRange := rsUnknown;
  fParenthesisLevel := 0;
  fBracketLevel := 0;
  fBraceLevel := 0;
  fDefaultFilter := SYNS_FilterCPP;
end; { Create }

procedure TSynHighlighterLua.SetLine(NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fSpaceRange := rsUnknown;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynHighlighterLua.CommentStartProc;
var
  pos:integer;
begin
  fTokenID := tkComment;
  if fLine[Run]='[' then begin
    pos := Run;
    inc(Run);
    while not (fLine[Run] in ['[',#32,#9,#0]) do begin
      if fLine[Run] <> '=' then begin
        fTokenID:=tkUnknown;
        fRange:=rsUnknown;
        Exit;
      end;
      inc(Run);
    end;
    if fLine[Run] <> '[' then begin
      {string not finished, but we don't count it as an error}
      fRange:=rsUnknown;
      Exit;
    end;
    inc(Run);
    fStringEndSymbol := ']'+StringOfChar('=',(Run-pos)-2)+']';
    if (fLine[Run]=#0) then begin
      fRange := rsComment;
      Exit;
    end;
  end else begin
    if (fLine[Run]=#0) then begin
      fRange:=rsUnknown;
      Exit;
    end;
    fStringEndSymbol := '';
  end;
  fRange := rsComment;
  CommentProc;
end;

procedure TSynHighlighterLua.CommentProc;
begin
  fTokenID := tkComment;
  if fLine[Run]=#0 then begin
    NullProc;
    if (length(fStringEndSymbol)<2) then begin
      fRange := rsUnknown;
    end;    
    Exit;
  end;
  while not (fLine[Run] in [#0, #10, #13]) do begin
    if (Length(fStringEndSymbol)>0) and (StrLComp(fLine+Run,PChar(fStringEndSymbol),Length(fStringEndSymbol))=0) then begin
      fRange := rsUnknown;    
      inc(Run,Length(fStringEndSymbol));
      Break;
    end;
    if (fLine[Run] in [#9,#32]) then begin
      fSpaceRange := rsComment;
      fRange := rsSpace;
      Exit;
    end;
    inc(Run);
  end;
  if (fRange = rsComment) and (Length(fStringEndSymbol)<2) then
    fRange := rsUnknown;
end;

procedure TSynHighlighterLua.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  FExtTokenID := xtkBAnd;
end;

procedure TSynHighlighterLua.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceClose;
  dec(fBraceLevel);
end;

procedure TSynHighlighterLua.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceOpen;
  inc(fBraceLevel);
end;

procedure TSynHighlighterLua.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynHighlighterLua.ColonProc;
begin
  if FLine[Run + 1] = ':' then begin
    {Label}
    inc(Run, 2);
    LabelProc;
    Exit;
  end else begin
    {colon}
    fTokenID := tkSymbol;
    inc(Run);
    FExtTokenID := xtkColon;
  end;
end;

procedure TSynHighlighterLua.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynHighlighterLua.LabelProc;
var
  beginPos:integer;
begin
  fTokenId := tkComment;
  fRange:=rsUnknown;
  beginPos := Run;
  while Identifiers[fLine[Run]] do inc(Run);
  if (fLine[Run]=':') then begin
    if Run=beginPos then begin
      {':::'}
      fTokenId := tkUnknown;
      Exit;
    end;
    inc(Run);
    if not (fLine[Run] in [':',#0,#32,#9,';']) then begin
      {'::xxx:='}
      fTokenId := tkUnknown;
      Exit;
    end;
    if (fLine[Run]=':') then
      inc(Run);
  end else if not (fLine[Run] in [#0,#32,#9,';']) then begin
    {'::xxx='}
    fTokenId := tkUnknown;
    Exit;
  end;
end;

procedure TSynHighlighterLua.LengthProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  fExtTokenID := xtkLen;
end;

procedure TSynHighlighterLua.EqualProc;
begin
  fTokenID := tkSymbol;
  if FLine[Run + 1] = '=' then begin
    {logical equal}
    inc(Run, 2);
    FExtTokenID := xtkEq;
  end else begin
    {assign}
    inc(Run);
    FExtTokenID := xtkAssign;
  end;
end;

procedure TSynHighlighterLua.GreaterProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkGe;
      end;
    '>':
      begin
        inc(Run, 2);
        FExtTokenID := xtkShr;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      FExtTokenID := xtkGt;
    end;
  end;
end;

procedure TSynHighlighterLua.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynHighlighterLua.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynHighlighterLua.LowerProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLt;
      end;
    '<':
      begin
        inc(Run, 2);
        FExtTokenID := xtkShl;
      end;
  else                                 {less than}
    begin
      inc(Run);
      FExtTokenID := xtkLt;
    end;
  end;
end;

procedure TSynHighlighterLua.MinusProc;
begin
  if fLine[Run+1] = '-' then begin
    {comment}
    fRange := rsComment;
    inc(Run,2);
    CommentStartProc;
  end else begin
    {substract}
    fTokenID := tkSymbol;
    FExtTokenID := xtkSub;
    inc(Run);
  end;
end;

procedure TSynHighlighterLua.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  FExtTokenID := xtkMod;
end;

procedure TSynHighlighterLua.NotSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        inc(Run, 2);
        FExtTokenID := xtkNotEq;
      end;
  else                                 {not}
    begin
      inc(Run);
      FExtTokenID := xtkBNot;
    end;
  end;
end;

procedure TSynHighlighterLua.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynHighlighterLua.NumberProc;
var
  idx1: Integer; // token[1]
  i: Integer;
begin
  idx1 := Run;
  Inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
    ['0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X', '-', '+',''''] do
  begin
    case FLine[Run] of
      '''': if (fTokenID  <> tkNumber) then begin
          fTokenID := tkUnknown;
          Exit;
        end;
      '.':
        if FLine[Succ(Run)] = '.' then
          Break
        else
          if (fTokenID <> tkHex) then
            fTokenID := tkFloat
          else // invalid
          begin
            fTokenID := tkUnknown;
            Exit;
          end;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // number <> float. an arithmetic operator
            Exit;
          if not (FLine[Pred(Run)] in ['e', 'E']) then
            Exit; // number = float, but no exponent. an arithmetic operator
          if not (FLine[Succ(Run)] in ['0'..'9', '+', '-']) then // invalid
          begin
            Inc(Run);
            fTokenID := tkUnknown;
            Exit;
          end
        end;
      '0'..'7':
        if (Run = Succ(idx1)) and (FLine[idx1] = '0') then // octal number
          fTokenID := tkOctal;
      '8', '9':
        if (FLine[idx1] = '0') and
           ((fTokenID <> tkHex) and (fTokenID <> tkFloat)) then // invalid octal char
             fTokenID := tkUnknown;
      'a'..'d', 'A'..'D':
        if fTokenID <> tkHex then // invalid char
          Break;
      'e', 'E':
        if (fTokenID <> tkHex) then
          if FLine[Pred(Run)] in ['0'..'9'] then // exponent
          begin
            for i := idx1 to Pred(Run) do
              if FLine[i] in ['e', 'E'] then // too many exponents
              begin
                fTokenID := tkUnknown;
                Exit;
              end;
            if not (FLine[Succ(Run)] in ['0'..'9', '+', '-']) then
              Break
            else
              fTokenID := tkFloat
          end
          else // invalid char
            Break;
      'f', 'F':
        if fTokenID <> tkHex then
        begin
          for i := idx1 to Pred(Run) do
            if FLine[i] in ['f', 'F'] then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
          if fTokenID = tkFloat then
          begin
            if fLine[Pred(Run)] in ['l', 'L'] then // can't mix
              Break;
          end
          else
            fTokenID := tkFloat;
        end;
      'l', 'L':
        begin
          for i := idx1 to Run - 2 do
            if FLine[i] in ['l', 'L'] then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
          if fTokenID = tkFloat then
            if fLine[Pred(Run)] in ['f', 'F'] then // can't mix
              Break;
        end;
      'u', 'U':
        if fTokenID = tkFloat then // not allowed
          Break
        else
          for i := idx1 to Pred(Run) do
            if FLine[i] in ['u', 'U'] then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
      'x', 'X':
        if (Run = Succ(idx1)) and   // 0x... 'x' must be second char
           (FLine[idx1] = '0') and  // 0x...
           (FLine[Succ(Run)] in ['0'..'9', 'a'..'f', 'A'..'F']) then // 0x... must be continued with a number
             fTokenID := tkHex
           else // invalid char
           begin
             if (not Identifiers[fLine[Succ(Run)]]) and
                (FLine[Succ(idx1)] in ['x', 'X']) then
             begin
               Inc(Run); // highlight 'x' too
               fTokenID := tkUnknown;
             end;
             Break;
           end;
    end; // case
    Inc(Run);
  end; // while
  {
  if (FLine[Run] in ['A'..'Z', 'a'..'z', '_']) then
    fTokenID := tkIdentifier
  }
  if (FLine[Run-1] in ['''']) then
    fTokenID := tkUnknown;
end;

procedure TSynHighlighterLua.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  FExtTokenID := xtkBOr;
end;

procedure TSynHighlighterLua.PlusProc;
begin
  fTokenID := tkSymbol;
  FExtTokenID := xtkAdd;
  inc(Run);
end;

procedure TSynHighlighterLua.PointProc;
begin
  fTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then begin
    {var args like 'f(...)'}
    inc(Run, 3);
    FExtTokenID := xtkVarArg;
  end else if (FLine[Run + 1] = '.') then begin
    {concat operation}
    inc(Run,2);
    FExtTokenId := xtkConcat;
  end else if FLine[Run + 1] in ['0'..'9'] then begin
    { float }
    Dec(Run); // numberproc must see the point
    NumberProc;
  end else begin
    {point}
    inc(Run);
    FExtTokenID := xtkPeriod;
  end;
end;

procedure TSynHighlighterLua.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
  dec(fParenthesisLevel);
end;

procedure TSynHighlighterLua.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
  inc(fParenthesisLevel);
end;

procedure TSynHighlighterLua.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
end;

procedure TSynHighlighterLua.SlashProc;
begin
  if FLine[Run + 1] = '/' then begin {integer divide}
    inc(Run);
    fTokenID := tkSymbol;
    FExtTokenID := xtkIDiv;
  end else begin {divide}
    inc(Run);
    fTokenID := tkSymbol;
    FExtTokenID := xtkDiv;
  end;
end;

procedure TSynHighlighterLua.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
  fRange := fSpaceRange;
  fSpaceRange := rsUnknown;

  {
  if (fRange in [rsCppComment, rsMultiLineDirective,
    rsString]) and (FLine[Run]=#0) then begin
    fRange := rsUnknown;
  end;
  }
end;

procedure TSynHighlighterLua.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
  dec(fBracketLevel);
end;

procedure TSynHighlighterLua.SquareOpenProc;
begin
  if fLine[Run+1] in ['=','['] then begin
    {string}
    fRange := rsString;
    StringStartProc;
    Exit;
  end;
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
  inc(fBracketLevel);
end;

procedure TSynHighlighterLua.StarProc;
begin
  fTokenID := tkSymbol;
  FExtTokenID := xtkMul;
  inc(Run);
end;

procedure TSynHighlighterLua.StringEscapeSeqProc;
begin
  fTokenID := tkStringEscapeSeq;

  inc(Run);
  case fLine[Run] of
    '''','"','?','a','b','f','n','r','t','v','\': begin
        inc(Run);
      end;
    '0'..'9': begin
        if not (fLine[Run] in ['0'..'7'] )
          or not (fLine[Run+1] in ['0'..'7'] )
          or not (fLine[Run+2] in ['0'..'7']) then
          fTokenID := tkUnknown;
        inc(Run,3);
      end;
    'x': begin
        if not (fLine[Run+1] in ['0'..'9','a'..'f','A'..'F'] )
          or not (fLine[Run+2] in ['0'..'9','a'..'f','A'..'F']) then
          fTokenID := tkUnknown;
        inc(Run,3);
      end;
    'u': begin
        if not (fLine[Run+1] in ['0'..'9','a'..'f','A'..'F'] )
          or not (fLine[Run+2] in ['0'..'9','a'..'f','A'..'F'])
          or not (fLine[Run+3] in ['0'..'9','a'..'f','A'..'F'])
          or not (fLine[Run+4] in ['0'..'9','a'..'f','A'..'F']) then
          fTokenID := tkUnknown;
        inc(Run,5);
      end;
    'U': begin
        if not (fLine[Run+1] in ['0'..'9','a'..'f','A'..'F'] )
          or not (fLine[Run+2] in ['0'..'9','a'..'f','A'..'F'])
          or not (fLine[Run+3] in ['0'..'9','a'..'f','A'..'F'])
          or not (fLine[Run+4] in ['0'..'9','a'..'f','A'..'F'])
          or not (fLine[Run+5] in ['0'..'9','a'..'f','A'..'F'])
          or not (fLine[Run+6] in ['0'..'9','a'..'f','A'..'F'])
          or not (fLine[Run+7] in ['0'..'9','a'..'f','A'..'F'])
          or not (fLine[Run+8] in ['0'..'9']) then
          fTokenID := tkUnknown;
        inc(Run,9);
      end;
  end;
  if fRange = rsMultilineStringEscapeSeq then
    fRange:=rsMultilineString
  else
    fRange:=rsString;
end;

procedure TSynHighlighterLua.StringStartProc;
var
  pos: integer;
begin
  fTokenID := tkString;
  if fLine[Run] in [#34,#39] then begin
    fStringEndSymbol := fLine[Run];
    inc(Run);
    if (fLine[Run]=#0) then begin
      fRange:=rsUnknown;
      Exit;
    end;
  end else begin
    pos := Run;
    inc(Run);
    while not (fLine[Run] in ['[',#32,#9,#0]) do begin
      if fLine[Run] <> '=' then begin
        fTokenID:=tkUnknown;
        fRange:=rsUnknown;
        Exit;
      end;
      inc(Run);
    end;
    if fLine[Run] <> '[' then begin
      {string not finished, but we don't count it as an error}
      fRange:=rsUnknown;
      Exit;
    end;
    inc(Run);
    fStringEndSymbol := ']'+StringOfChar('=',(Run-pos)-2)+']';
    if (fLine[Run]=#0) then begin
      fRange := rsString;
      Exit;
    end;
  end;
  StringProc;
end;

procedure TSynHighlighterLua.StringProc;
begin
  if (fLine[Run]=#0) then begin
    NullProc;
    if (length(fStringEndSymbol)<2) then begin
      fRange := rsUnknown;
    end;
    Exit;
  end;
  fTokenID := tkString;
  fRange := rsString;
  while not (fLine[Run] in [#0, #10, #13]) do begin
    if StrLComp(fLine+Run,PChar(fStringEndSymbol),Length(fStringEndSymbol))=0 then begin
      fRange := rsUnknown;    
      inc(Run,Length(fStringEndSymbol));
      Break;
    end;
    if (fLine[Run] in [#9,#32]) then begin
      fSpaceRange := rsString;
      fRange := rsSpace;
      Exit;
    end;
    if fLine[Run] = '\' then begin
      case fLine[Run + 1] of
        '''','"','\','?','a','b','f','n','r','t','v','0'..'9','x','u','U':
          begin
            fRange := rsStringEscapeSeq;
            Exit;
          end;
        {
        #00:
          begin
            Inc(Run);
            fRange := rsString;
            Exit;
          end;
        }
      end;
    end;
    inc(Run);
  end;
  if (fRange = rsString) and (Length(fStringEndSymbol)<2) then
    fRange := rsUnknown;
  {end;}
end;

procedure TSynHighlighterLua.TildeProc;
begin
  if FLine[Run+1] = '=' then begin
    inc(Run,2);                            {Not equal}
    fTokenId := tkSymbol;
    FExtTokenID := xtkNotEq;
  end else begin
    inc(Run);                            {bitwise complement}
    fTokenId := tkSymbol;
    FExtTokenID := xtkBNot;
  end;
end;

procedure TSynHighlighterLua.CaretProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkPow;
end;

procedure TSynHighlighterLua.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynHighlighterLua.Next;
begin
  fTokenPos := Run;
  repeat
    case fRange of
      rsLabel : LabelProc;
      rsString: StringProc;
      rsComment: CommentProc;
      rsMultilineString: StringProc;
      rsSpace: SpaceProc;
      rsStringEscapeSeq, rsMultilineStringEscapeSeq : StringEscapeSeqProc;
      else begin
        fRange := rsUnknown;
        fProcTable[fLine[Run]];
      end;
    end;
  until (fTokenID = tkNull) or (Run > fTokenPos);
end;

function TSynHighlighterLua.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynHighlighterLua.GetEol: Boolean;
begin
  Result := (fTokenID = tkNull);
end;

function TSynHighlighterLua.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynHighlighterLua.GetParenthesisLevel: integer;
begin
  Result := fParenthesisLevel;
end;

function TSynHighlighterLua.GetBracketLevel: integer;
begin
  Result := fBracketLevel;
end;

function TSynHighlighterLua.GetBraceLevel: integer;
begin
  Result := fBraceLevel;
end;

function TSynHighlighterLua.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynHighlighterLua.GetTokenFinished: boolean;
begin
  case fTokenId of
    tkComment:
      Result := (fRange = rsUnknown);
      {
      Result := not (fRange  in [rsAnsiC, rsAnsiCAsm,
        rsAnsiCAsmBlock, rsDirectiveComment,rsCppComment]);
      }
    tkString:
      Result := (fRange = rsUnknown);
  else
    Result := False;
  end;
end;

function TSynHighlighterLua.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynHighlighterLua.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynHighlighterLua.GetTokenType: TSynHighlighterTokenType;
begin
  case fTokenID of
    tkComment: Result := httComment;
    tkIdentifier: Result := httIdentifier;
    tkKey: Result := httKeyword;
    tkSpace: begin
      case fRange of
        rsString, rsMultiLineString, rsStringEscapeSeq,
          rsMultiLineStringEscapeSeq:
          Result := httString;
        rsComment :
          Result := httComment;
      else
        Result := httSpace;
      end;
    end;
    tkString: Result := httString;
    tkStringEscapeSeq: Result := httStringEscapeSequence;
    tkSymbol: Result := httSymbol;
    else Result := httDefault;
  end;
end;

function TSynHighlighterLua.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkFloat: Result := fFloatAttri;
    tkHex: Result := fHexAttri;
    tkOctal: Result := fOctalAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkStringEscapeSeq: Result := fStringEscapeSeqAttri;
    tkLabel: Result := fLabelAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    else Result := nil;
  end;
end;

function TSynHighlighterLua.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynHighlighterLua.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynHighlighterLua.ResetRange;
begin
  fRange:= rsUnknown;
  fSpaceRange := rsUnknown;
end;

procedure TSynHighlighterLua.ResetParenthesisLevel;
begin
  fParenthesisLevel := 0;
end;

procedure TSynHighlighterLua.ResetBracketLevel;
begin
  fBracketLevel := 0;
end;

procedure TSynHighlighterLua.ResetBraceLevel;
begin
  fBraceLevel := 0;
end;

procedure TSynHighlighterLua.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynHighlighterLua.SetParenthesisLevel(Value: integer);
begin
  fParenthesisLevel := Value;
end;

procedure TSynHighlighterLua.SetBracketLevel(Value: integer);
begin
  fBracketLevel := Value;
end;

procedure TSynHighlighterLua.SetBraceLevel(Value: integer);
begin
  fBraceLevel := Value;
end;


function TSynHighlighterLua.GetIsLastLineCommentNotFinish(Value:Pointer):boolean;
var
  range: TRangeState;
begin
  range := TRangeState(Value);
  Result:= (range in [rsComment]);
end;

function TSynHighlighterLua.GetIsLastLineStringNotFinish(Value:Pointer):boolean;
var
  range: TRangeState;
begin
  range := TRangeState(Value);
  Result:= (range in [rsMultiLineString]);
end;

procedure TSynHighlighterLua.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
  with TBetterRegistry.Create do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Borland\C++Builder') then
      begin
        try
          GetKeyNames(settings);
        finally
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TSynHighlighterLua.UseUserSettings(settingIndex: integer): boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

  function ReadCPPBSettings(settingIndex: integer): boolean;

    function ReadCPPBSetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): boolean;

      function ReadCPPB1(settingTag: string; attri: TSynHighlighterAttributes; name: string): boolean;
      var
        i: integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
             '\SOFTWARE\Borland\C++Builder\'+settingTag+'\Highlight',name,true);
      end; { ReadCPPB1 }

      function ReadCPPB3OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                 '\Software\Borland\C++Builder\'+settingTag+'\Editor\Highlight',
                 key,false);
      end; { ReadCPPB3OrMore }

    begin { ReadCPPBSetting }
      try
        if (settingTag[1] = '1')
          then Result := ReadCPPB1(settingTag,attri,key)
          else Result := ReadCPPB3OrMore(settingTag,attri,key);
      except Result := false; end;
    end; { ReadCPPBSetting }

  var
    tmpStringAttri    : TSynHighlighterAttributes;
    tmpNumberAttri    : TSynHighlighterAttributes;
    tmpFloatAttri     : TSynHighlighterAttributes;
    tmpHexAttri       : TSynHighlighterAttributes;
    tmpOctalAttri     : TSynHighlighterAttributes;
    tmpKeyAttri       : TSynHighlighterAttributes;
    tmpSymbolAttri    : TSynHighlighterAttributes;
    tmpCommentAttri   : TSynHighlighterAttributes;
    tmpIdentifierAttri: TSynHighlighterAttributes;
    tmpInvalidAttri   : TSynHighlighterAttributes;
    tmpSpaceAttri     : TSynHighlighterAttributes;
    tmpLabelAttri     : TSynHighlighterAttributes;
    s                 : TStringList;

  begin { ReadCPPBSettings }
    s := TStringList.Create;
    try
      EnumUserSettings(s);
      if settingIndex >= s.Count then Result := false
      else begin
        tmpStringAttri    := TSynHighlighterAttributes.Create('');
        tmpNumberAttri    := TSynHighlighterAttributes.Create('');
        tmpFloatAttri     := TSynHighlighterAttributes.Create('');
        tmpHexAttri       := TSynHighlighterAttributes.Create('');
        tmpOctalAttri     := TSynHighlighterAttributes.Create('');
        tmpKeyAttri       := TSynHighlighterAttributes.Create('');
        tmpSymbolAttri    := TSynHighlighterAttributes.Create('');
        tmpCommentAttri   := TSynHighlighterAttributes.Create('');
        tmpIdentifierAttri:= TSynHighlighterAttributes.Create('');
        tmpInvalidAttri   := TSynHighlighterAttributes.Create('');
        tmpSpaceAttri     := TSynHighlighterAttributes.Create('');
        tmpLabelAttri     := TSynHighlighterAttributes.Create('');
        tmpStringAttri    .Assign(fStringAttri);
        tmpNumberAttri    .Assign(fNumberAttri);
        tmpFloatAttri     .Assign(fFloatAttri);
        tmpHexAttri       .Assign(fHexAttri);
        tmpOctalAttri     .Assign(fOctalAttri);
        tmpKeyAttri       .Assign(fKeyAttri);
        tmpSymbolAttri    .Assign(fSymbolAttri);
        tmpCommentAttri   .Assign(fCommentAttri);
        tmpIdentifierAttri.Assign(fIdentifierAttri);
        tmpInvalidAttri   .Assign(fInvalidAttri);
        tmpSpaceAttri     .Assign(fSpaceAttri);
        tmpLabelAttri     .Assign(fLabelAttri);
        Result := ReadCPPBSetting(s[settingIndex],fCommentAttri,'Comment')       and
                  ReadCPPBSetting(s[settingIndex],fIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(s[settingIndex],fInvalidAttri,'Illegal Char')  and
                  ReadCPPBSetting(s[settingIndex],fKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(s[settingIndex],fNumberAttri,'Integer')        and
                  ReadCPPBSetting(s[settingIndex],fFloatAttri,'Float')           and
                  ReadCPPBSetting(s[settingIndex],fHexAttri,'Hex')               and
                  ReadCPPBSetting(s[settingIndex],fOctalAttri,'Octal')           and
                  ReadCPPBSetting(s[settingIndex],fSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(s[settingIndex],fStringAttri,'String')         and
                  ReadCPPBSetting(s[settingIndex],fSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(s[settingIndex],fLabelAttri,'Preprocessor');
        if not Result then begin
          fStringAttri    .Assign(tmpStringAttri);
          fNumberAttri    .Assign(tmpNumberAttri);
          fFloatAttri     .Assign(tmpFloatAttri);
          fHexAttri       .Assign(tmpHexAttri);
          fOctalAttri     .Assign(tmpOctalAttri);
          fKeyAttri       .Assign(tmpKeyAttri);
          fSymbolAttri    .Assign(tmpSymbolAttri);
          fCommentAttri   .Assign(tmpCommentAttri);
          fIdentifierAttri.Assign(tmpIdentifierAttri);
          fInvalidAttri   .Assign(tmpInvalidAttri);
          fSpaceAttri     .Assign(tmpSpaceAttri);
          fLabelAttri     .Assign(tmpLabelAttri);
        end;
        tmpStringAttri    .Free;
        tmpNumberAttri    .Free;
        tmpFloatAttri     .Free;
        tmpHexAttri       .Free;
        tmpOctalAttri     .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpCommentAttri   .Free;
        tmpIdentifierAttri.Free;
        tmpInvalidAttri   .Free;
        tmpSpaceAttri     .Free;
        tmpLabelAttri     .Free;
      end;
    finally s.Free; end;
  end; { ReadCPPBSettings }

begin
  Result := ReadCPPBSettings(settingIndex);
end; { TSynHighlighterLua.UseUserSettings }

function TSynHighlighterLua.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynHighlighterLua.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCPP;
end;

class function TSynHighlighterLua.GetLanguageName: string;
begin
  Result := SYNS_LangCPP;
end;

class function TSynHighlighterLua.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynHighlighterLua.GetSampleSource: string;
begin
  Result := '// Syntax Highlighting'#13#10+
            'void __fastcall TForm1::Button1Click(TObject *Sender)'#13#10+
            '{'#13#10+
            '  int number = 123456;'#13#10+
            '  char c = ''a'';'#13#10+
            '  Caption = "The number is " + IntToStr(i);'#13#10+
            '  for (int i = 0; i <= number; i++)'#13#10+
            '  {'#13#10+
            '    x -= 0xff;'#13#10+
            '    x -= 023;'#13#10+
            '    x += 1.0;'#13#10+
            '    x += @; /* illegal character */'#13#10+
            '  }'#13#10+
            '  #ifdef USE_ASM'#13#10+
            '    asm'#13#10+
            '    {'#13#10+
            '      ASM MOV AX, 0x1234'#13#10+
            '      ASM MOV i, AX'#13#10+
            '    }'#13#10+
            '  #endif'#13#10+
            '}';

end;

initialization
begin
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynHighlighterLua);
{$ENDIF}

  LuaKeywords := TStringHash.Create();

  LuaKeywords.Add('and',1);
  LuaKeywords.Add('break',1);
  LuaKeywords.Add('do',1);
  LuaKeywords.Add('else',1);
  LuaKeywords.Add('elseif',1);
  LuaKeywords.Add('end',1);
  LuaKeywords.Add('false',1);
  LuaKeywords.Add('for',1);
  LuaKeywords.Add('function',1);
  LuaKeywords.Add('goto',1);
  LuaKeywords.Add('if',1);
  LuaKeywords.Add('in',1);
  LuaKeywords.Add('local',1);
  LuaKeywords.Add('nil',1);
  LuaKeywords.Add('not',1);
  LuaKeywords.Add('or',1);
  LuaKeywords.Add('repeat',1);
  LuaKeywords.Add('return',1);
  LuaKeywords.Add('then',1);
  LuaKeywords.Add('true',1);
  LuaKeywords.Add('until',1);
  LuaKeywords.Add('while',1);
end;

finalization
begin
  LuaKeywords.Clear;
  LuaKeywords.Free;
end;

end.

