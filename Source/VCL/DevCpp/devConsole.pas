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

unit devConsole;

interface

uses
{$IFDEF WIN32}
  Windows, StdCtrls,Messages, SysUtils, Classes, Forms, Controls;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, Classes, QForms, QControls,
devMonitorThread, devMonitorTypes;
{$ENDIF}

type
  TDevConsole = class(TCustomMemo)
  private
    fCurrentCommand : AnsiString;
    fInputEnabled : boolean;

    procedure setInputEnabled(enabled:boolean);
    procedure setCurrentCommand(command:AnsiString);
  protected
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update; override;
    procedure Clear; override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property CurrentCommand: AnsiString read fCurrentCommand write SetCurrentCommand;
    property InputEnabled: boolean read fInputEnabled write SetInputEnabled;
  end;

procedure Register;

implementation

{ TdevConsole }

procedure Register;
begin
  RegisterComponents('Dev-C++', [TDevConsole]);
end;

constructor TDevConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := False;
  WordWrap := False;
  WantReturns := True;
  ReadOnly := True;
  fCurrentCommand:='';
end;
procedure TDevConsole.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  if not fInputEnabled then
    Exit;
  case(key) of
    VK_BACK: begin
      if fCurrentCommand<>'' then begin
        fCurrentCommand:=Copy(fCurrentCommand,1,Length(fCurrentCommand)-1);
        Update;
      end;
    end;
  end;
end;


procedure TDevConsole.KeyPress(var Key:char);
begin
  inherited KeyPress(Key);
  if not fInputEnabled then
    Exit;
  fCurrentCommand := fCurrentCommand + Key;
  Update;
end;


procedure TDevConsole.setInputEnabled(enabled:boolean);
begin
  fInputEnabled:=enabled;
end;

procedure TDevConsole.setCurrentCommand(command:AnsiString);
begin
  fCurrentCommand := command;
  if not fInputEnabled then
    Exit;
  Update;
end;

procedure TDevConsole.Update;
begin
  if Lines.Count>0 then begin
    Lines[Lines.Count-1]:='(gdb)'+fCurrentCommand;
  end;
  inherited;
end;

procedure TDevConsole.Clear;
begin
  fCurrentCommand := '';
  fInputEnabled := False;
  inherited;
end;

end.

