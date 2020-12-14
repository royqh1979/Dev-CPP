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

unit HeaderCompletionForm;

interface

uses
  Windows, Classes, Graphics, Forms, StdCtrls, Controls,
  HeaderCompletion, CppParser, CBUtils,Messages;

type
  THeaderComplForm = class(TForm)
    lbCompletion: TListBox;
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure lbCompletionDblClick(Sender: TObject);
    procedure lbCompletionDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbCompletionKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);              
  private
    { Private declarations }
    fOwner: THeaderCompletion;
    FOldWndProc: TWndMethod;
    fColors : array[0..12] of TColor;
    procedure lbCompletionWindowProc(var Message: TMessage);
    function GetColor(i:integer):TColor;
    procedure SetColor(i:integer; const Color:TColor);    protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Colors[Index: Integer]: TColor read GetColor write SetColor;
  end;

var
  HeaderComplForm: THeaderComplForm;

implementation

{$R *.dfm}

procedure THeaderComplForm.FormShow(Sender: TObject);
begin
  Width := fOwner.Width;
  Height := fOwner.Height;
  lbCompletion.DoubleBuffered := true; // performance hit, but reduces flicker a lit
end;

procedure THeaderComplForm.FormDeactivate(Sender: TObject);
begin
  fOwner.Hide;
end;

procedure THeaderComplForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
//  Params.Style := (Params.Style or WS_SIZEBOX) ;
end;

constructor THeaderComplForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fOwner := THeaderCompletion(AOwner);
end;

procedure THeaderComplForm.lbCompletionDblClick(Sender: TObject);
var
  Key: Char;
begin
  // Send command to TEditor
  if Assigned(fOwner.OnKeyPress) then begin
    Key := Char(VK_RETURN);
    fOwner.OnKeyPress(self, Key);
  end;
end;

procedure THeaderComplForm.lbCompletionDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
var
  Offset: integer;
begin
  Offset := 4;
  with lbCompletion do begin
    // Draw statement kind string, like 'Preprocessor'
    if odSelected in State then begin
      Canvas.Brush.Color := Colors[SelectedBackColor];
    end else begin
      Canvas.Brush.Color := Colors[BackColor];
    end;
    Canvas.FillRect(Rect);
    Canvas.Font.Color := Colors[PreprocessorColor];
    Canvas.TextOut(Offset, Rect.Top, String(Items.Objects[Index]));
  end;
end;

procedure THeaderComplForm.lbCompletionKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(fOwner.OnKeyPress) then
    fOwner.OnKeyPress(self, Key);
end;

procedure THeaderComplForm.FormCreate(Sender: TObject);
begin
  FOldWndProc:= lbCompletion.WindowProc;
  lbCompletion.WindowProc:= lbCompletionWindowProc;
end;

procedure THeaderComplForm.lbCompletionWindowProc(var Message: TMessage);
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

function THeaderComplForm.GetColor(i:integer):TColor;
begin
  Result := fColors[i];
end;

procedure THeaderComplForm.SetColor(i:integer; const Color:TColor);
begin
  fColors[i] := Color;
  if i=BackColor then begin
    self.Color := Color;
  end;
end;


end.

