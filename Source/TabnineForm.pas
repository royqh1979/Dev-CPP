{
    This file is part of Red Panda Dev-C++
    Copyright (c) 2020 Roy Qu(royqh1979@gmail.com)

    Red Panda Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    Red Panda Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit TabnineForm;

interface

uses
{$IFDEF WIN32}
  Windows, Classes, Graphics, Forms, StdCtrls, Controls,
  CodeCompletion, CppParser, CBUtils,Messages;
{$ENDIF}
{$IFDEF LINUX}
Xlib, SysUtils, Classes, QGraphics, QForms, QStdCtrls, QControls,
CodeCompletion, CppParser, QGrids, QDialogs, Types;
{$ENDIF}

type
  TTabnineForm = class(TForm)
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
    procedure lbCompletionWindowProc(var Message: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

{$R *.dfm}

procedure TTabnineForm.FormShow(Sender: TObject);
begin
  Width := fOwner.Width;
  Height := fOwner.Height;
  lbCompletion.DoubleBuffered := true; // performance hit, but reduces flicker a lit
end;

procedure TTabnineForm.FormDeactivate(Sender: TObject);
begin
  fOwner.Hide;
end;

procedure TTabnineForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or WS_SIZEBOX;
end;

constructor TTabnineForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fOwner := TCodeCompletion(AOwner);
end;

procedure TTabnineForm.lbCompletionDblClick(Sender: TObject);
var
  Key: Char;
begin
  // Send command to TEditor
  if Assigned(fOwner.OnKeyPress) then begin
    Key := Char(VK_RETURN);
    fOwner.OnKeyPress(self, Key);
  end;
end;

procedure TTabnineForm.lbCompletionDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
var
  Offset: integer;
  statement: PStatement;
begin
  Offset := 4;

  with lbCompletion do begin
    statement := PStatement(Items.Objects[Index]);

    // Draw statement kind string, like 'Preprocessor'
    if odSelected in State then begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(Rect);
      Canvas.Font.Color := clHighlightText;
    end else begin
      Canvas.Brush.Color := fOwner.Color;
      Canvas.FillRect(Rect);
      case statement^._Kind of
        skFunction: Canvas.Font.Color := clGreen;
        skClass: Canvas.Font.Color := clMaroon;
        skVariable: Canvas.Font.Color := clBlue;
        skTypedef: Canvas.Font.Color := clOlive;
        skPreprocessor: Canvas.Font.Color := clPurple;
        skEnum: Canvas.Font.Color := clNavy;
      else
        Canvas.Font.Color := clGray;
      end;
    end;
    Canvas.TextOut(Offset, Rect.Top, fOwner.Parser.StatementKindStr(statement^._Kind));
    Offset := Offset +
      Canvas.TextWidth(fOwner.Parser.StatementKindStr(statement^._Kind)+' '); // worst case width + spacing
    if not (odSelected in State) then
      Canvas.Font.Color := clWindowText;

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
end;

procedure TTabnineForm.lbCompletionKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(fOwner.OnKeyPress) then
    fOwner.OnKeyPress(self, Key);
end;

procedure TTabnineForm.FormCreate(Sender: TObject);
begin
  FOldWndProc:= lbCompletion.WindowProc;
  lbCompletion.WindowProc:= lbCompletionWindowProc;
end;

procedure TTabnineForm.lbCompletionWindowProc(var Message: TMessage);
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

end.

