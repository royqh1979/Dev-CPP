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

unit CodeCompletionForm;

interface

uses
  Windows, Classes, Graphics, Forms, StdCtrls, Controls,
  CodeCompletion, CppParser, CBUtils,Messages;

type
  TCodeComplForm = class(TForm)
    lbCompletion: TListBox;
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure lbCompletionDblClick(Sender: TObject);
    procedure lbCompletionDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbCompletionKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);              
  private
    { Private declarations }
    fOwner: TCodeCompletion;
    FOldWndProc: TWndMethod;
    fColors : array[0..14] of TColor;
    procedure lbCompletionWindowProc(var Message: TMessage);
    function GetColor(i:integer):TColor;
    procedure SetColor(i:integer; const Color:TColor);    protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Colors[Index: Integer]: TColor read GetColor write SetColor;
  end;

var
  CodeComplForm: TCodeComplForm;

implementation

{$R *.dfm}

procedure TCodeComplForm.FormShow(Sender: TObject);
begin
  Width := fOwner.Width;
  Height := fOwner.Height;
  lbCompletion.DoubleBuffered := true; // performance hit, but reduces flicker a lit
end;

procedure TCodeComplForm.FormDeactivate(Sender: TObject);
begin
  fOwner.Hide;
end;

procedure TCodeComplForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
//  Params.Style := (Params.Style or WS_SIZEBOX) ;
end;

constructor TCodeComplForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fOwner := TCodeCompletion(AOwner);
end;

procedure TCodeComplForm.lbCompletionDblClick(Sender: TObject);
var
  Key: Char;
begin
  // Send command to TEditor
  if Assigned(fOwner.OnKeyPress) then begin
    Key := Char(VK_RETURN);
    fOwner.OnKeyPress(self, Key);
  end;
end;

procedure TCodeComplForm.lbCompletionDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
var
  Offset: integer;
  statement: PStatement;
  level : integer;
begin
  if not fOwner.FreezeParser then
    Exit;
  try
  Offset := 4;

  with lbCompletion do begin

    // Draw statement kind string, like 'Preprocessor'
    if odSelected in State then begin
      Canvas.Brush.Color := Colors[SelectedBackColor];
    end else begin
      Canvas.Brush.Color := Colors[BackColor];
    end;
      Canvas.FillRect(Rect);

    statement := PStatement(Items.Objects[Index]);

    level := 0;
    while Assigned(statement) and (statement^._Kind = skAlias) do begin
      statement := fOwner.Parser.FindStatementOf(
        statement^._FileName, statement^._Type, statement^._Line);
      inc(level);
      //break infinite loop
      if level > 10 then
        break;
    end;
    if not assigned(statement) then
      statement := PStatement(Items.Objects[Index]);

    case statement^._Kind of
      skFunction, skConstructor, skDestructor: Canvas.Font.Color := Colors[FunctionColor];
      skClass: Canvas.Font.Color := Colors[ClassColor];
      skVariable: begin
        if not Assigned(statement^._ParentScope) then begin
          Canvas.Font.Color := Colors[GlobalVarColor];
        end else if statement^._Scope = ssLocal then begin
          Canvas.Font.Color := Colors[LocalVarColor];
        end else
          Canvas.Font.Color := Colors[VarColor];
      end;
      skParameter: Canvas.Font.Color := Colors[LocalVarColor];
      skNamespace: Canvas.Font.Color := Colors[NamespaceColor];
      skTypedef, skAlias: Canvas.Font.Color := Colors[TypedefColor];
      skPreprocessor, skEnum: Canvas.Font.Color := Colors[PreprocessorColor];
      skEnumType: Canvas.Font.Color := Colors[EnumColor];
      skKeyword, skUserCodeIn: Canvas.Font.Color := Colors[KeywordColor];
    else
      Canvas.Font.Color := Colors[ForeColor];
    end;

    Canvas.TextOut(Offset, Rect.Top, fOwner.Parser.StatementKindStr(statement^._Kind));
    Offset := Offset +
      Canvas.TextWidth(fOwner.Parser.StatementKindStr(statement^._Kind)+' '); // worst case width + spacing
    {
    if not (odSelected in State) then
      Canvas.Font.Color := Colors[ForeColor];
    }

    // Draw data type string, like 'int', hide for defines/others that don't have this property
// MinGW gcc's type info is too long , so we don't print it
//    if Length(statement^._Type) > 0 then begin
//      Canvas.TextOut(Offset, Rect.Top, statement^._Type);
//      Offset := Offset + Canvas.TextWidth(statement^._Type + ' ');
//    end;

    // draw statement name, like 'foo'
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(Offset, Rect.Top, statement^._Command);
//    Offset := Offset + Canvas.TextWidth(statement^._Command + ' ');
    Offset := Offset + Canvas.TextWidth(statement^._Command );
    // if applicable, draw arguments
    if statement^._Kind in [skFunction, skConstructor, skDestructor] then begin
      Canvas.Font.Style := [];
      Canvas.TextOut(Offset, Rect.Top, statement^._Args);
    end;
  end;
  finally
    fOwner.UnfreezeParser;
  end;
end;

procedure TCodeComplForm.lbCompletionKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(fOwner.OnKeyPress) then
    fOwner.OnKeyPress(self, Key);
end;

procedure TCodeComplForm.FormCreate(Sender: TObject);
begin
  FOldWndProc:= lbCompletion.WindowProc;
  lbCompletion.WindowProc:= lbCompletionWindowProc;
end;

procedure TCodeComplForm.lbCompletionWindowProc(var Message: TMessage);
var
  ch:Char;
  code:Word;
begin
  if Message.Msg = CN_KEYDOWN then begin
    code := TWMKey(Message).CharCode;
    case code of
      VK_TAB: begin
        ch := #9;
        lbCompletionKeyPress(lbCompletion, ch);
        Exit;
      end;
      VK_LEFT,VK_RIGHT,VK_HOME,VK_END: begin
        if Assigned(fOwner) and Assigned(fOwner.OnKeyDown) then
          fOwner.OnKeyDown(self, code, [] );
      end;
    end;
  end;
  if Assigned(FOldWndProc) then
    FOldWndProc(Message);
  if Message.Msg = WM_GETDLGCODE then
    Message.Result:= Message.Result or DLGC_WANTTAB;
end;

function TCodeComplForm.GetColor(i:integer):TColor;
begin
  Result := fColors[i];
end;

procedure TCodeComplForm.SetColor(i:integer; const Color:TColor);
begin
  fColors[i] := Color;
  if i=BackColor then begin
    self.Color := Color;
  end;
end;


end.

