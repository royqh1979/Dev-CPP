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

unit devRun;

interface

uses
  Classes, Windows, Dialogs, utils;

type
  TLineOutputEvent = procedure(Sender: TObject; const Line: AnsiString) of object;

  TPipeStdinThread = class(TThread)
  private
    procedure PipeInput;
  public
    WriteHandle : THandle;
    InputText: string;
    procedure Execute; override;
  end;

  TDevRun = class(TThread)
  private
    fCurrentLine: AnsiString;
    fLineOutput: TLineOutputEvent;
    fCheckAbort: TCheckAbortFunc;
    fInputText : String;
    fPipe:TPipeStdinThread;
    fRedirectStdin: boolean;
  protected
    procedure CallLineOutputEvent;
    procedure Execute; override;
    procedure LineOutput(const Line: AnsiString);
    procedure OnInputHandle(const hInputWrite:THandle);
  public
    Command: AnsiString;
    Directory: AnsiString;
    Output: AnsiString;
    property OnLineOutput: TLineOutputEvent read FLineOutput write FLineOutput;
    property OnCheckAbort: TCheckAbortFunc read FCheckAbort write FCheckAbort;
    property InputText: String read fInputText write fInputText;
    property RedirectStdin: boolean read fRedirectStdin write fRedirectStdin;
  end;

implementation

uses sysutils;

procedure TDevRun.CallLineOutputEvent;
begin
  fLineOutput(Self, fCurrentLine);
end;

procedure TDevRun.LineOutput(const Line: AnsiString);
begin
  fCurrentLine := Line;
  if Assigned(fLineOutput) then
    Synchronize(CallLineOutputEvent);
end;

procedure TDevRun.OnInputHandle(const hInputWrite:THandle);
begin
  fPipe := TPipeStdinThread.Create(True);
  fPipe.WriteHandle := hInputWrite;
  fPipe.InputText := InputText;
  fPipe.FreeOnTerminate := True;
  fPipe.Resume;
end;

procedure TDevRun.Execute;
begin
  if not fRedirectStdin then
    Output := RunAndGetOutput(Command, Directory, LineOutput, FCheckAbort)
  else
    Output := RunAndGetOutput(Command, Directory, LineOutput, nil,true,nil,OnInputHandle);
end;

{ TPipeStdinThread }

procedure TPipeStdinThread.PipeInput;
var
  buffer: pAnsichar;
  bytesWritten: cardinal;
  bufSize: cardinal;
begin
  buffer := PAnsiChar(self.InputText);
  bufSize := Length(self.InputText);
  try
    while bufSize>0 do begin
      if not WriteFile(WriteHandle,buffer^,bufSize,bytesWritten,nil) then begin
        LogError('devRun.pas PipeInput',Format('WriteFile failed: %s',[SysErrorMessage(GetLastError)]));
        break;
      end;
      inc(buffer,bytesWritten);
      dec(bufSize,bytesWritten);
    end;
  finally
    CloseHandle(WriteHandle);
  end;
end;

procedure TPipeStdinThread.Execute;
begin
  inherited;
  PipeInput;
end;

end.

