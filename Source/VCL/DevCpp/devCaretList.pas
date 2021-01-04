{
    This file is part of Dev-C++
    Copyright (c) 2020 Roy Qu (royqh1979@gmail.com)

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit devCaretList;

interface

uses
  SysUtils, Classes, StdCtrls, Windows,Types;

const
  MAX_CARET_COUNT = 500;

type
  PEditorCaret = ^TEditorCaret;
  TEditorCaret = record
    editor:TObject;
    line:integer;
    char: integer;
  end;

  TDevCaretList = class(TObject)
  private
    FList: array [0..MAX_CARET_COUNT-1] of TEditorCaret;
    FIndex: integer;
    FCount: integer;
    fCurrentCaret:TEditorCaret;
    procedure removeCaret(i:integer);
  public
    procedure AddCaret(const editor:TObject; const line:integer; const char:integer);
    function hasPrevious:boolean;
    function hasNext:boolean;
    function GotoAndGetPrevious : PEditorCaret ;
    function GotoAndGetNext : PEditorCaret ;
    procedure RemoveEditor(const editor:TObject);
    procedure LinesDeleted(const editor:TObject;const FirstLine: integer; const Count:integer);
    procedure LinesInserted(const editor:TObject;const FirstLine: integer; const Count:integer);
    procedure Reset;
    constructor Create;
  end;

implementation

constructor TDevCaretList.Create;
begin
  Reset;
end;

procedure TDevCaretList.Reset;
begin
  FIndex:=0;
  FCount:=0;
  fCurrentCaret.editor:=nil;
  fCurrentCaret.line:=1;
  fCurrentCaret.char:=1;
end;

procedure TDevCaretList.AddCaret(const editor:TObject; const line:integer; const char:integer);
begin
  if (FIndex>0) and (fCurrentCaret.editor = editor)
    and  (fCurrentCaret.line - line>=-1)
    and (fCurrentCaret.line - line<=1)  then
  begin
    fCurrentCaret.line := line;
    Exit;
  end;
  if (FIndex = MAX_CARET_COUNT) then begin
    System.Move(FList[2], FList[1],
      (MAX_CARET_COUNT-1) * SizeOf(TEditorCaret));
      dec(FIndex);
  end;
  inc(FIndex);
  FList[FIndex].editor := editor;
  FList[FIndex].line := line;
  FList[FIndex].char := char;
  FCount:=FIndex;
  fCurrentCaret := FList[FIndex];
end;

function TDevCaretList.hasPrevious:boolean;
begin
  result:=FIndex>1;
end;

function TDevCaretList.hasNext:boolean;
begin
  result:=FIndex<FCount;
end;

function TDevCaretList.GotoAndGetPrevious:PEditorCaret;
begin
  Result := nil;
  if (FIndex>1) then begin
    dec(FIndex);
    Result:=@FList[FIndex];
    fCurrentCaret := FList[FIndex];
  end;
end;

function TDevCaretList.GotoAndGetNext:PEditorCaret;
begin
  Result := nil;
  if (FIndex<FCount) then begin
    inc(FIndex);
    Result:=@FList[FIndex];
    fCurrentCaret := FList[FIndex];
  end;
end;

procedure TDevCaretList.RemoveCaret(i:integer);
begin
  if (i<1) or (i>FCount) then
    Exit;
  if FIndex >= i then
    dec(FIndex);
  System.Move(FList[i+1], FList[i],
    (FCount - i) * SizeOf(TEditorCaret));
  dec(FCount);
end;

procedure TDevCaretList.RemoveEditor(const editor:TObject);
var
  i:integer;
  oldIndex: integer;
begin
  oldIndex := FIndex;
  for i:= FCount downto 1 do begin
    if FList[i].editor = editor then begin
      removeCaret(i);
    end;
  end;
  if oldIndex <> FIndex then
    fCurrentCaret := FList[FIndex];
end;

procedure TDevCaretList.LinesDeleted(const editor:TObject;const FirstLine: integer; const Count:integer);
var
  i,oldIndex: integer;
begin
  oldIndex := FIndex;
  for i:= 1 to FCount do begin
    if (FList[i].editor = editor)
      and (FList[i].line >= FirstLine) then begin
      if FList[i].line < (FirstLine+Count) then begin
        removeCaret(i);
      end else begin
        dec(FList[i].line,count);
      end;
    end;
  end;
  if oldIndex <> FIndex then
    fCurrentCaret := FList[FIndex]
  else begin
    if (fCurrentCaret.editor = editor)
      and (fCurrentCaret.line >= FirstLine) then begin
      if fCurrentCaret.line < (FirstLine+Count) then begin
        fCurrentCaret := FList[FIndex];
      end else begin
        dec(fCurrentCaret.line,count);
      end;
    end;
  end;
end;

procedure TDevCaretList.LinesInserted(const editor:TObject;const FirstLine: integer; const Count:integer);
var
  i:integer;
begin
  for i:= 1 to FCount do begin
    if (FList[i].editor = editor)
      and (FList[i].line >= FirstLine) then begin
        inc(FList[i].line,count);
    end;
  end;
  if (fCurrentCaret.editor = editor)
      and (fCurrentCaret.line >= FirstLine) then begin
    inc(fCurrentCaret.line,count);
  end;
end;

end.

