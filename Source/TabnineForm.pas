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
  Windows, Classes, Graphics, Forms, StdCtrls, Controls,
  Messages;

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
    fOwner: TObject;
    FOldWndProc: TWndMethod;
    fColors : array[0..11] of TColor;
    procedure lbCompletionWindowProc(var Message: TMessage);
    function GetColor(i:integer):TColor;
    procedure SetColor(i:integer; const Color:TColor);    protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent; owner: TObject);
    property Colors[Index: Integer]: TColor read GetColor write SetColor;
  end;

implementation

uses Tabnine,CBUtils;
{$R *.dfm}

procedure TTabnineForm.FormShow(Sender: TObject);
begin
  Width := TTabnine(fOwner).Width;
  Height := TTabnine(fOwner).Height;
  lbCompletion.DoubleBuffered := true; // performance hit, but reduces flicker a lit
end;

procedure TTabnineForm.FormDeactivate(Sender: TObject);
begin
  TTabnine(fOwner).Hide;
end;

procedure TTabnineForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
//  Params.Style := (Params.Style or WS_SIZEBOX) ;
end;

constructor TTabnineForm.Create(AOwner: TComponent; owner:TObject);
begin
  inherited Create(AOwner);

  fOwner := owner;
end;

procedure TTabnineForm.lbCompletionDblClick(Sender: TObject);
var
  Key: Char;
begin
  // Send command to TEditor
  if Assigned(TTabnine(fOwner).OnKeyPress) then begin
    Key := Char(VK_RETURN);
    TTabnine(fOwner).OnKeyPress(self, Key);
  end;
end;

procedure TTabnineForm.lbCompletionDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
var
  Offset: integer;
  suggestion: PTabnineSuggestion;
begin
  Offset := 4;

  with lbCompletion do begin
    suggestion := PTabnineSuggestion(Items.Objects[Index]);

    // Draw statement kind string, like 'Preprocessor'
    Canvas.Font.Style:= [fsBold, fsUnderline];
    if odSelected in State then begin
      Canvas.Brush.Color := Colors[SelectedBackColor];
      Canvas.FillRect(Rect);
      Canvas.Font.Color := Colors[SelectedForeColor];
    end else begin
      Canvas.Brush.Color := Colors[BackColor];
      Canvas.FillRect(Rect);
      Canvas.Font.Color := Colors[ForeColor];
    end;
    Canvas.TextOut(Offset, Rect.Top,
      suggestion^.oldPrefix);
    Offset:=Offset+Canvas.TextWidth(suggestion^.oldPrefix);

    Canvas.Font.Style:= [];
    Canvas.TextOut(Offset, Rect.Top,
      Copy(suggestion^.newPrefix, length(suggestion^.oldPrefix)+1,MaxInt)+suggestion^.newSuffix );
    Canvas.TextOut(Rect.Right-Canvas.TextWidth(suggestion^.detail)-4, Rect.Top,
      suggestion^.detail);
  end;
end;

procedure TTabnineForm.lbCompletionKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(TTabnine(fOwner).OnKeyPress) then
    TTabnine(fOwner).OnKeyPress(self, Key);
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
        if Assigned(fOwner) and Assigned(TTabnine(fOwner).OnKeyDown) then
          TTabnine(fOwner).OnKeyDown(self, code, [] );
      end;
    end;
  end;
  if Assigned(FOldWndProc) then
    FOldWndProc(Message);
  if Message.Msg = WM_GETDLGCODE then
    Message.Result:= Message.Result or DLGC_WANTTAB;
end;

function TTabnineForm.GetColor(i:integer):TColor;
begin
  Result := fColors[i];
end;

procedure TTabnineForm.SetColor(i:integer; const Color:TColor);
begin
  fColors[i] := Color;
  if i=BackColor then begin
    self.Color := Color;
  end;
end;


end.



