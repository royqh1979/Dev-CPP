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

unit devParser;

interface

uses
  Classes, Windows, Dialogs, utils, CppParser;

type
  
  TCppParserThread = class(TThread)
  public
    Parser: TCppParser;
    FileName: AnsiString; 
    InProject: boolean; 
    OnlyIfNotParsed: boolean; 
    UpdateView: boolean; 
    fileContent: TStrings;
    procedure Execute; override;
  end;

  TCppParseFileListThread = class(TThread)
  public
    Parser: TCppParser;
    procedure Execute; override;
  end;

procedure ParseFile(Parser: TCppParser;
    FileName: AnsiString; 
    InProject: boolean; 
    OnlyIfNotParsed: boolean = False; 
    UpdateView: boolean = True;
    fileContent:TStrings = nil);

procedure ParseFileList(Parser: TCppParser);

implementation

uses sysutils;

procedure ParseFileList(Parser: TCppParser);
var
   parserThread: TCppParseFileListThread;
begin
  parserThread:=TCppParseFileListThread.Create(True);
  parserThread.FreeOnTerminate := True;
  parserThread.Resume;
end;

procedure ParseFile(Parser: TCppParser;
    FileName: AnsiString; 
    InProject: boolean; 
    OnlyIfNotParsed: boolean = False;
    UpdateView: boolean = True;
    fileContent:TStrings = nil);
var
   parserThread: TCppParserThread;
begin
  parserThread:=TCppParserThread.Create(True);
  parserThread.FreeOnTerminate := True;
  parserThread.Parser := Parser;
  parserThread.FileName := FileName;
  parserThread.InProject := InProject;
  parserThread.OnlyIfNotParsed:= OnlyIfNotParsed;
  parserThread.UpdateView:=UpdateView;
  parserThread.fileContent:=fileContent;
  parserThread.Resume;
end;

procedure TCppParserThread.Execute;
var
  M: TMemoryStream;
begin
  inherited;
  if assigned(Parser) and not (Parser.Parsing) then begin
    if fileContent <> nil then begin
      M := TMemoryStream.Create;
      try
        fileContent.SaveToStream(M);
        Parser.ParseFile(FileName, InProject, OnlyIfNotParsed, UpdateView, M);
      finally
        M.Free;
      end;
    end else begin
      Parser.ParseFile(FileName,InProject,OnlyIfNotParsed, UpdateView,nil);
    end;
  end;
end;

procedure TCppParseFileListThread.Execute;
begin
  inherited;
  if assigned(Parser) and not (Parser.Parsing) then begin
    Parser.ParseFileList;
  end;
end;

end.

