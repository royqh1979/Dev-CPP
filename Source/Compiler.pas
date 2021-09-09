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

unit Compiler;

interface

uses
  Windows, SysUtils, Dialogs, StdCtrls, Controls, ComCtrls, Forms,
  devRun, version, project, utils, ProjectTypes, Classes, Graphics, devCFG;

type
  TLogEntryEvent = procedure(const Msg: AnsiString) of object;
  TOutputEvent = procedure(const _Line, _Col, _Unit, _Message: AnsiString) of object;
  TResOutputEvent = procedure(const _Line, _Col, _Unit, _Message: AnsiString) of object;
  TCompEndEvent = procedure of object;
  TCompSuccessEvent = procedure of object;
  TRunEndEvent = procedure of object;

  TTarget = (cttInvalid, cttNone, cttFile, cttProject, cttStdIn);

  TGCCMessageType = (gmtError,gmtWarning,gmtInfo,gmtNote,gmtNone);

  TCompiler = class
  private
    fOnLogEntry: TLogEntryEvent;
    fOnOutput: TOutputEvent;
    fOnResOutput: TResOutputEvent;
    fOnCompEnd: TCompEndEvent;
    fOnCompSuccess: TCompSuccessEvent;
    fOnRunEnd: TRunEndEvent;
    fProject: TProject;
    fSourceFile: AnsiString;
    fSourceText: AnsiString;
    fUseRunParams: boolean;
    fRunParams: AnsiString;
    fUseInputFile: boolean;
    fInputFile: AnsiString;
    fMakefile: AnsiString;
    fTarget: TTarget;
    fErrCount: integer;
    fCheckSyntax: Boolean;
    fWarnCount: integer;
    fStartTime: cardinal;
    fShowOutputInfo: boolean;
    fCompilerSet: TdevCompilerSet;
    fUseUTF8: boolean;
    procedure DoLogEntry(const msg: AnsiString);
    procedure DoOutput(const s1, s2, s3, s4: AnsiString);
    procedure DoResOutput(const s1, s2, s3, s4: AnsiString);
    function GetMakeFile: AnsiString;
    function GetCompiling: Boolean;
    procedure RunTerminate(Sender: TObject);
    procedure InitProgressForm;
    procedure ProcessProgressForm(const Line: AnsiString);
    procedure EndProgressForm;
  public
    procedure BuildMakeFile;
    procedure CheckSyntax(silent:boolean = False);
    procedure Compile(silent:boolean = False);
    procedure Run;
    procedure Clean;
    procedure RebuildAll;
    property Compiling:boolean read GetCompiling;
    property Project: TProject read fProject write fProject;
    property OnLogEntry: TLogEntryEvent read fOnLogEntry write fOnLogEntry;
    property OnOutput: TOutputEvent read fOnOutput write fOnOutput;
    property OnResOutput: TResOutputEvent read fOnResOutput write fOnResOutput;
    property OnCompEnd: TCompEndEvent read fOnCompEnd write fOnCompEnd;
    property OnCompSuccess: TCompSuccessEvent read fOnCompSuccess write fOnCompSuccess;
    property OnRunEnd: TRunEndEvent read fOnRunEnd write fOnRunEnd;
    property SourceFile: AnsiString read fSourceFile write fSourceFile;
    property SourceText: AnsiString read fSourceText write fSourceText;
    property CompilerSet: TdevCompilerSet read fCompilerSet write fCompilerSet;
    property RunParams: AnsiString read fRunParams write fRunParams; // only for nonproject compilations
    property UseRunParams: boolean read fUseRunParams write fUseRunParams;
    property UseInputFile: boolean read fUseInputFile write fUseInputFile;
    property InputFile: AnsiString read fInputFile write fInputFile;
    property MakeFile: AnsiString read GetMakeFile;
    property Target: TTarget read fTarget write fTarget;
    property WarningCount: integer read fWarnCount;
    property ErrorCount: integer read fErrCount;
    property UseUTF8: boolean read fUseUTF8 write fUseUTF8;
    procedure AbortThread;
  protected
    fCompileParams: AnsiString;
    fCppCompileParams: AnsiString;
    fLibrariesParams: AnsiString;
    fIncludesParams: AnsiString;
    fCppIncludesParams: AnsiString;
    fBinDirs: AnsiString;
    fDevRun: TDevRun;
    fAbortThread: boolean;
    procedure CreateStandardMakeFile; // executable creation
    procedure CreateStaticMakeFile; // static library creation
    procedure CreateDynamicMakeFile; // dynamic library creation
    procedure GetCompileParams;
    procedure GetLibrariesParams;
    procedure GetIncludesParams;
    procedure LaunchThread(const s, dir: AnsiString; redirectStdin: boolean = False);
    procedure ThreadCheckAbort(var AbortThread: boolean);
    procedure OnCompilationTerminated(Sender: TObject);
    procedure OnLineOutput(Sender: TObject; const Line: AnsiString);
    procedure ProcessOutput(const line: AnsiString);
    procedure NewMakeFile(var F: TextFile); // create a fits-all makefile
    procedure WriteMakeHeader(var F: TextFile); // append commented header
    procedure WriteMakeDefines(var F: TextFile); // append definitions
    procedure WriteMakeTarget(var F: TextFile); // append target definitions (PHONY, all)
    procedure WriteMakeIncludes(var F: TextFile); // append list of includes
    procedure WriteMakeClean(var F: TextFile); // append cleaning rules
    procedure WriteMakeObjFilesRules(var F: TextFile); // append linkage rules
  end;

implementation

uses
  MultiLangSupport, Macros, devExec, main, StrUtils, CppParser, Editor, dataFrm, iniFiles;

procedure TCompiler.DoLogEntry(const msg: AnsiString);
begin
  if Assigned(fOnLogEntry) then
    fOnLogEntry(msg);
end;

procedure TCompiler.DoOutput(const s1, s2, s3, s4: AnsiString);
begin
  if Assigned(fOnOutput) then
    fOnOutput(s1, s2, s3, s4);
end;

procedure TCompiler.DoResOutput(const s1, s2, s3, s4: AnsiString);
begin
  if Assigned(fOnResOutput) then
    fOnResOutput(s1, s2, s3, s4);
end;

function TCompiler.GetMakeFile: AnsiString;
begin
  if not FileExists(fMakeFile) then
    BuildMakeFile;
  result := fMakefile;
end;

procedure TCompiler.BuildMakeFile;
begin
  if not Assigned(fProject) then begin
    fMakeFile := '';
    Exit;
  end else if fProject.Options.UseCustomMakefile then begin
    fMakefile := fProject.Options.CustomMakefile;
    Exit;
  end;

  case fProject.Options.typ of
    dptStat: CreateStaticMakeFile;
    dptDyn: CreateDynamicMakeFile;
  else
    CreateStandardMakeFile;
  end;

  if FileExists(fMakeFile) then
    FileSetDate(fMakefile, DateTimeToFileDate(Now)); // fix the "Clock skew detected" warning ;)
end;

procedure TCompiler.NewMakeFile(var F: TextFile);
begin
  // Create OBJ output directory
  SetCurrentDir(fProject.Directory); // .dev file location
  if fProject.Options.ObjectOutput <> '' then
    if not DirectoryExists(fProject.Options.ObjectOutput) then
      CreateDir(fProject.Options.ObjectOutput);

  // Should not return custom filename
  fMakefile := fProject.MakeFileName;

  // Write more information to the log file than before
  DoLogEntry(Lang[ID_LOG_BUILDINGMAKEFILE]);
  DoLogEntry('--------');
  DoLogEntry(Format(Lang[ID_LOG_MAKEFILENAME], [fMakefile]));

  // Create the actual file
  Assignfile(F, fMakefile);
  Rewrite(F);

  // Write header
  WriteMakeHeader(F);

  // Writes definition list
  WriteMakeDefines(F);

  // Write PHONY and all targets
  WriteMakeTarget(F);

  // Write list of includes
  WriteMakeIncludes(F);

  // Write clean command
  WriteMakeClean(F);
end;

procedure TCompiler.WriteMakeHeader(var F: TextFile);
begin
  Writeln(F, '# Project: ' + fProject.Name);
  Writeln(F, '# Makefile created by Dev-C++ ' + DEVCPP_VERSION);
  Writeln(F);
  if fCheckSyntax then begin
    Writeln(F, '# This Makefile is written for syntax check!');
    Writeln(F, '# Regenerate it if you want to use this Makefile to build.');
    Writeln(F);
  end;
end;

procedure TCompiler.WriteMakeDefines(var F: TextFile);
var
  Objects, ObjResFile, LinkObjects, ObjFile, RelativeName, OutputFileDir, LibOutputFile: AnsiString;
  I: integer;
begin
  // Get list of object files
  Objects := '';
  LinkObjects := '';

  // Create a list of object files
  for i := 0 to Pred(fProject.Units.Count) do begin

    if not fProject.Units[i].Compile and not fProject.Units[i].Link then
      Continue;

    // Only process source files
    RelativeName := ExtractRelativePath(fProject.FileName, fProject.Units[i].FileName);
    if not (GetFileTyp(RelativeName) in [utcHead, utcppHead, utResSrc, utOther]) then begin
      if fProject.Options.ObjectOutput <> '' then begin

        // ofile = C:\MyProgram\obj\main.o
        ObjFile := IncludeTrailingPathDelimiter(fProject.Options.ObjectOutput) +
          ExtractFileName(fProject.Units[i].FileName);
        ObjFile := GenMakePath1(ExtractRelativePath(fProject.FileName, ChangeFileExt(ObjFile, OBJ_EXT)));
        Objects := Objects + ' ' + ObjFile;

        if fProject.Units[i].Link then
          LinkObjects := LinkObjects + ' ' + ObjFile;
      end else begin
        Objects := Objects + ' ' + GenMakePath1(ChangeFileExt(RelativeName, OBJ_EXT));
        if fProject.Units[i].Link then
          LinkObjects := LinkObjects + ' ' + GenMakePath1(ChangeFileExt(RelativeName, OBJ_EXT));
      end;
    end;
  end;

  Objects := Trim(Objects);
  LinkObjects := Trim(LinkObjects);

  // Get windres file
  if Length(fProject.Options.PrivateResource) = 0 then begin
    ObjResFile := '';
  end else begin
    if fProject.Options.ObjectOutput <> '' then begin
      ObjResFile := IncludeTrailingPathDelimiter(fProject.Options.ObjectOutput) +
        ChangeFileExt(fProject.Options.PrivateResource, RES_EXT);
    end else
      ObjResFile := ChangeFileExt(fProject.Options.PrivateResource, RES_EXT);
  end;

  // Mention progress in the logs
  if ObjResFile <> '' then
    DoLogEntry(Format(Lang[ID_LOG_RESFILENAME], [ExpandFileto(ObjResFile, fProject.Directory)]));
  DoLogEntry('');

  // Get list of applicable flags
  GetCompileParams;
  GetLibrariesParams;
  GetIncludesParams;

  if (Pos(' -g3', fCompileParams) > 0) or (Pos('-g3', fCompileParams) = 1) then begin
    Writeln(F, 'CPP      = ' + fCompilerSet.gppName + ' -D__DEBUG__');
    Writeln(F, 'CC       = ' + fCompilerSet.gccName + ' -D__DEBUG__');
  end else begin
    Writeln(F, 'CPP      = ' + fCompilerSet.gppName);
    Writeln(F, 'CC       = ' + fCompilerSet.gccName);
  end;
  Writeln(F, 'WINDRES  = ' + fCompilerSet.windresName);
  if (ObjResFile <> '') then begin
    Writeln(F, 'RES      = ' + GenMakePath1(ObjResFile));
    Writeln(F, 'OBJ      = ' + Objects + ' $(RES)');
    Writeln(F, 'LINKOBJ  = ' + LinkObjects + ' $(RES)');
  end else begin
    Writeln(F, 'OBJ      = ' + Objects);
    Writeln(F, 'LINKOBJ  = ' + LinkObjects);
  end;
  Writeln(F, 'LIBS     = ' + StringReplace(fLibrariesParams, '\', '/', [rfReplaceAll]));
  Writeln(F, 'INCS     = ' + StringReplace(fIncludesParams, '\', '/', [rfReplaceAll]));
  Writeln(F, 'CXXINCS  = ' + StringReplace(fCppIncludesParams, '\', '/', [rfReplaceAll]));
  Writeln(F, 'BIN      = ' + GenMakePath1(ExtractRelativePath(Makefile, fProject.Executable)));
  Writeln(F, 'CXXFLAGS = $(CXXINCS) ' + fCppCompileParams);
  Writeln(F, 'ENCODINGS = -finput-charset=utf-8 -fexec-charset='+GetSystemCharsetName);
  Writeln(F, 'CFLAGS   = $(INCS) ' + fCompileParams);
//  Writeln(F, 'RM       = ' + CLEAN_PROGRAM + ' -f'); // TODO: use del or rm?
  Writeln(F, 'RM       = ' + CLEAN_PROGRAM + ' /f'); // TODO: use del or rm?
  if fProject.Options.UsePrecompiledHeader then begin
    Writeln(F, 'PCH_H = ' + fProject.Options.PrecompiledHeader );
    Writeln(F, 'PCH = ' + fProject.Options.PrecompiledHeader +'.gch' );
  end;

  // This needs to be put in before the clean command.
  if fProject.Options.typ = dptDyn then begin
    OutputFileDir := ExtractFilePath(Project.Executable);
    LibOutputFile := OutputFileDir + 'lib' + ExtractFileName(Project.Executable);
    if FileSamePath(LibOutputFile, Project.Directory) then
      LibOutputFile := ExtractFileName(LibOutputFile)
    else
      LibOutputFile := ExtractRelativePath(Makefile, LibOutputFile);

    Writeln(F, 'DEF      = ' + GenMakePath1(ChangeFileExt(LibOutputFile, DEF_EXT)));
    Writeln(F, 'STATIC   = ' + GenMakePath1(ChangeFileExt(LibOutputFile, LIB_EXT)));
  end;
  Writeln(F);
end;

procedure TCompiler.WriteMakeTarget(var F: TextFile);
begin
  if fCheckSyntax then
    Writeln(F, '.PHONY: all all-before all-after clean clean-custom $(OBJ) $(BIN)')
  else
    Writeln(F, '.PHONY: all all-before all-after clean clean-custom');
  Writeln(F);
  Writeln(F, 'all: all-before $(BIN) all-after');
  Writeln(F);
  if fProject.Options.UsePrecompiledHeader then begin
    Writeln(F, '$(PCH) : $(PCH_H)');
    Writeln(F, '	$(CPP) -x c++-header "$(PCH_H)" -o "$(PCH)" $(CXXFLAGS)');
    Writeln(F);
  end;
end;

procedure TCompiler.WriteMakeIncludes(var F: TextFile);
var
  I: integer;
begin
  for i := 0 to fProject.Options.MakeIncludes.Count - 1 do
    Writeln(F, 'include ' + GenMakePath1(fProject.Options.MakeIncludes.Strings[i]));
  if fProject.Options.MakeIncludes.Count > 0 then
    Writeln(F);
end;

procedure TCompiler.WriteMakeObjFilesRules(var F: TextFile);
resourcestring
  cSyntaxCmdLine = '%s "%s" %s';
var
  i,j: integer;
  FileName, ShortFileName, objStr,ObjFileName, BuildCmd, ResFiles, ResIncludes, ResFile, PrivResName, WindresArgs: AnsiString;
  encodingStr: AnsiString;
  fileIncludes: TStringList;
  headerName: AnsiString;
  parser:TCppParser;
  precompileStr:AnsiString;
begin
  parser:=MainForm.GetCppParser;
  if fProject.Options.UsePrecompiledHeader then
    precompileStr := ' $(PCH) '
  else
    precompileStr := '';

  for i := 0 to pred(fProject.Units.Count) do begin
    if not fProject.Units[i].Compile then
      Continue;

    // skip resource files
    if GetFileTyp(fProject.Units[i].FileName) = utResSrc then
      Continue;

    // Get unit filename relative to project or relative to makefile
    FileName := fProject.Units[i].FileName;
    if FileSamePath(FileName, fProject.Directory) then
      ShortFileName := ExtractFileName(FileName)
    else
      ShortFileName := ExtractRelativePath(Makefile, FileName);

    // Only process source files
    if GetFileTyp(ShortFileName) in [utcSrc, utcppSrc] then begin
      Writeln(F);
      objStr:=GenMakePath2(ShortFileName);
      if parser.ScannedFiles.IndexOf(FileName)<>-1 then begin // if we have scanned it, use scanned info
        fileIncludes := TStringList.Create;
        try
          parser.GetFileIncludes(FileName,fileIncludes);
          for j:=0 to fileIncludes.Count-1 do begin
            headerName := fileIncludes[j];
            if headerName = FileName then
              continue;
            if (not parser.IsSystemHeaderFile(headerName))
              and (not parser.IsProjectHeaderFile(headerName)) then begin
              objStr := objStr + ' ' + GenMakePath2(ExtractRelativePath(Makefile,headerName));
            end;
          end;
        finally
          fileIncludes.Free;
        end;
      end else begin
        for j := 0 to pred(fProject.Units.Count) do begin
          if GetFileTyp(fProject.Units[j].FileName) in [utcHead, utcppHead] then begin  // or  we simply use unit headers
              objStr := objStr + ' ' + GenMakePath2(ExtractRelativePath(Makefile,fProject.Units[j].FileName));
          end;
        end;
      end;

      if fProject.Options.ObjectOutput <> '' then begin
        ObjFileName := IncludeTrailingPathDelimiter(fProject.Options.ObjectOutput) +
          ExtractFileName(fProject.Units[i].FileName);
        ObjFileName := GenMakePath1(ExtractRelativePath(fProject.FileName, ChangeFileExt(ObjFileName, OBJ_EXT)));
        if ExtractFileDir(objFileName)<>'' then begin
          objStr := GenMakePath2(IncludeTrailingPathDelimiter(ExtractFileDir(objFileName))) + objStr;
        end;
      end else begin
        {
        if shortFileName <> ExtractFileName(FileName) then begin
          objStr := GenMakePath1(IncludeTrailingPathDelimiter(ExtractFileDir(shortFileName))) + objStr;
        end;
        }
        ObjFileName := GenMakePath1(ChangeFileExt(ShortFileName, OBJ_EXT));
      end;

      objStr:=ObjFileName + ': '+objStr+precompileStr;

      Writeln(F,ObjStr);

      // Write custom build command
      if fProject.Units[i].OverrideBuildCmd and (fProject.Units[i].BuildCmd <> '') then begin
        BuildCmd := fProject.Units[i].BuildCmd;
        BuildCmd := StringReplace(BuildCmd, '<CRTAB>', #10#9, [rfReplaceAll]);
        Writeln(F, #9 + BuildCmd);

        // Or roll our own
      end else begin
        encodingStr := '';
        if fProject.Options.AddCharset then begin
          if fProject.Units[i].Encoding in [etUTF8,etUTF8Bom] then begin
            encodingStr := ' $(ENCODINGS) ';
          end else if fProject.Units[i].Encoding = etAuto then begin
            if assigned(fProject.Units[i].Editor) and (fProject.Units[i].Editor.FileEncoding in [etUTF8,etUTF8Bom]) then begin
              encodingStr := ' $(ENCODINGS) ';
            end else begin
              with TStringList.Create do try
                LoadFromFile(fProject.Units[i].FileName);
                if GetFileEncodingType(Text) in [etUTF8,etUTF8Bom] then
                  encodingStr := ' $(ENCODINGS) '
              finally
                Free;
              end;
            end;
          end;
        end;

        if fCheckSyntax then begin
          if fProject.Units[i].CompileCpp then
            Writeln(F, #9 + '$(CPP) -c ' + GenMakePath1(ShortFileName) + ' $(CXXFLAGS) '+encodingStr)
          else
            Writeln(F, #9 + '$(CC) -c ' + GenMakePath1(ShortFileName) + ' $(CFLAGS) '+encodingStr);
        end else begin
          if fProject.Units[i].CompileCpp then
            Writeln(F, #9 + '$(CPP) -c ' + GenMakePath1(ShortFileName) + ' -o ' + ObjFileName + ' $(CXXFLAGS) ' + encodingStr)
          else
            Writeln(F, #9 + '$(CC) -c ' + GenMakePath1(ShortFileName) + ' -o ' + ObjFileName + ' $(CFLAGS) ' + encodingStr);
        end;
      end;
    end;
  end;

  if (Length(fProject.Options.PrivateResource) > 0) then begin

    // Concatenate all resource include directories
    ResIncludes := ' ';
    for i := 0 to fProject.Options.ResourceIncludes.Count - 1 do begin
      ShortFileName := GetShortName(fProject.Options.ResourceIncludes[i]);
      if ShortFileName <> '' then
        ResIncludes := ResIncludes + ' --include-dir ' + GenMakePath1(ShortFileName);
    end;

    // Concatenate all resource filenames (not created when syntax checking)
    if not fCheckSyntax then begin
      ResFiles := '';
      for i := 0 to fProject.Units.Count - 1 do begin
        if GetFileTyp(fProject.Units[i].FileName) <> utResSrc then
          Continue;
        ResFile := ExtractRelativePath(fProject.Executable, fProject.Units[i].FileName);
        if FileExists(GetRealPath(ResFile, fProject.Directory)) then
          ResFiles := ResFiles + GenMakePath2(ResFile) + ' ';
      end;
      ResFiles := Trim(ResFiles);
    end;

    // Determine resource output file
    if fProject.Options.ObjectOutput <> '' then
      ObjFileName := IncludeTrailingPathDelimiter(fProject.Options.ObjectOutput) +
        ChangeFileExt(fProject.Options.PrivateResource, RES_EXT)
    else
      ObjFileName := ChangeFileExt(fProject.Options.PrivateResource, RES_EXT);
    ObjFileName := GenMakePath1(ExtractRelativePath(fProject.FileName, ObjFileName));
    PrivResName := GenMakePath1(ExtractRelativePath(fProject.FileName, fProject.Options.PrivateResource));

    // Build final cmd
    if ContainsStr(fCompileParams, '-m32') then
      WindresArgs := ' -F pe-i386'
    else
      WindresArgs := '';
    if fCheckSyntax then begin
      Writeln(F);
      Writeln(F, ObjFileName + ':');
      Writeln(F, #9 + '$(WINDRES) -i ' + PrivResName + WindresArgs + ' --input-format=rc -o nul -O coff' + ResIncludes)
    end else begin
      Writeln(F);
      Writeln(F, ObjFileName + ': ' + PrivResName + ' ' + ResFiles);
      Writeln(F, #9 + '$(WINDRES) -i ' + PrivResName + WindresArgs + ' --input-format=rc -o ' + ObjFileName + ' -O coff'
        + ResIncludes);
    end;
    Writeln(F);
  end;
end;

procedure TCompiler.WriteMakeClean(var F: TextFile);
begin
  Writeln(F, 'clean: clean-custom');
  case fProject.Options.typ of
    dptDyn:
      Writeln(F, #9 + '${RM} $(OBJ) $(BIN) $(DEF) $(STATIC)');
  else
    Writeln(F, #9 + '${RM} $(OBJ) $(BIN)');
  end;
  Writeln(F);
end;

procedure TCompiler.CreateStandardMakefile;
var
  F: TextFile;
begin
  try
    NewMakeFile(F);
    Writeln(F, '$(BIN): $(OBJ)');
    if not fCheckSyntax then
      if fProject.Options.useGPP then
        Writeln(F, #9 + '$(CPP) $(LINKOBJ) -o "$(BIN)" $(LIBS)')
      else
        Writeln(F, #9 + '$(CC) $(LINKOBJ) -o "$(BIN)" $(LIBS)');
    WriteMakeObjFilesRules(F);
  finally
    CloseFile(F);
  end;
end;

procedure TCompiler.CreateStaticMakeFile;
var
  F: TextFile;
begin
  try
    NewMakeFile(F);
    Writeln(F, '$(BIN): $(LINKOBJ)');
    if not fCheckSyntax then begin
      Writeln(F, #9 + 'ar r $(BIN) $(LINKOBJ)');
      Writeln(F, #9 + 'ranlib $(BIN)');
    end;
    WriteMakeObjFilesRules(F);
  finally
    CloseFile(F);
  end;
end;

procedure TCompiler.CreateDynamicMakeFile;
var
  F: TextFile;
begin
  try
    NewMakeFile(F);
    Writeln(F, '$(BIN): $(LINKOBJ)');
    if not fCheckSyntax then begin
      if fProject.Options.useGPP then

      //  Writeln(F, #9 +
      //    '$(CPP) -shared $(LINKOBJ) -o "$(BIN)" $(LIBS) -Wl,--output-def,$(DEF),--out-implib,$(STATIC),--add-stdcall-alias')
        Writeln(F, #9 +
          '$(CPP) -mdll $(LINKOBJ) -o "$(BIN)" $(LIBS) -Wl,--output-def,$(DEF),--out-implib,$(STATIC)')
      else
        Writeln(F, #9 +
          '$(CC) -mdll $(LINKOBJ) -o "$(BIN)" $(LIBS) -Wl,--output-def,$(DEF),--out-implib,$(STATIC)')
    end;
    WriteMakeObjFilesRules(F);
  finally
    CloseFile(F);
  end;
end;

procedure TCompiler.GetCompileParams;
var
  I, val: integer;
  option: TCompilerOption;
begin
  // Force syntax checking when we have to
  if fCheckSyntax then begin
    fCompileParams := '-fsyntax-only';
    fCppCompileParams := '-fsyntax-only';
  end else begin
    fCompileParams := '';
    fCppCompileParams := '';
  end;

  // Walk all options
  for I := 0 to fCompilerSet.Options.Count - 1 do begin
    option := PCompilerOption(fCompilerSet.Options[I])^;

    // consider project specific options for the compiler, else global compiler options
    if (Assigned(fProject) and (I < Length(fProject.Options.CompilerOptions))) or (not Assigned(fProject) and
      (option.Value > 0)) then begin
      if option.IsC then begin
        if Assigned(option.Choices) then begin
          if Assigned(fProject) then
            val := CharToValue(fProject.Options.CompilerOptions[I + 1])
          else
            val := option.Value;
          if (val > 0) and (val < option.Choices.Count) then
            fCompileParams := fCompileParams + ' ' + option.Setting +
              option.Choices.Values[option.Choices.Names[val]];
        end else if (Assigned(fProject) and (StrToIntDef(fProject.Options.CompilerOptions[I + 1], 0) = 1)) or (not
          Assigned(fProject)) then begin
          fCompileParams := fCompileParams + ' ' + option.Setting;
        end;
      end;
      if option.IsCpp then begin
        if Assigned(option.Choices) then begin
          if Assigned(fProject) then
            val := CharToValue(fProject.Options.CompilerOptions[I + 1])
          else
            val := option.Value;
          if (val > 0) and (val < option.Choices.Count) then
            fCppCompileParams := fCppCompileParams + ' ' + option.Setting +
              option.Choices.Values[option.Choices.Names[val]];
        end else if (Assigned(fProject) and (StrToIntDef(fProject.Options.CompilerOptions[I + 1], 0) = 1)) or (not
          Assigned(fProject)) then begin
          fCppCompileParams := fCppCompileParams + ' ' + option.Setting;
        end;
      end;
    end;
  end;

  // Add custom commands inherited from Tools >> Compiler Options
  with fCompilerSet do begin
    if (Length(CompOpts) > 0) and AddtoComp then begin
      fCompileParams := fCompileParams + ' ' + CompOpts;
      fCppCompileParams := fCppCompileParams + ' ' + CompOpts;
    end;
  end;

  // Add custom commands at the end so the advanced user can control everything
  if Assigned(fProject) and (fTarget = cttProject) then begin
    if Length(fProject.Options.CompilerCmd) > 0 then
      fCompileParams := fCompileParams + ' ' + Trim(StringReplace(fProject.Options.CompilerCmd, '_@@_', ' ',
        [rfReplaceAll]));
    if Length(fProject.Options.CppCompilerCmd) > 0 then
      fCppCompileParams := fCppCompileParams + ' ' + Trim(StringReplace(fProject.Options.CppCompilerCmd, '_@@_', ' ',
        [rfReplaceAll]));
  end;

  fCompileParams := Trim(ParseMacros(fCompileParams));
  fCppCompileParams := Trim(ParseMacros(fCppCompileParams));
end;

procedure TCompiler.CheckSyntax(silent:boolean = False);
begin
  fCheckSyntax := True;
  Compile(silent);
  fCheckSyntax := False;
end;

procedure TCompiler.Compile(silent:boolean = False);
resourcestring
  // windres, input, output
  cResourceCmdLine = '%s --input-format=rc -i %s -o %s';
  // gcc, input, compileparams, includeparams, librariesparams
  cSyntaxCmdLine = '%s "%s" %s %s %s';
  // gcc, filetype, compileparams, includeparams
  cStdinSyntaxCmdLine = '%s -x %s - %s %s ';
  // gcc, input, compileparams, includeparams, librariesparams
  cHeaderCmdLine = '%s "%s" %s %s %s';
  // gcc, input, output, compileparams, includeparams, librariesparams
  cSourceCmdLine = '%s "%s" -o "%s" %s %s %s';
  // make, makefile
  cMakeLine = '%s -f "%s" all';
var
  cmdline: AnsiString;
  //s: AnsiString;
  compilerName: AnsiString;
  redirectStdin: boolean;
begin
  if fCompilerSet.BinDir.Count < 1 then begin
    if not silent then begin
      LogError('Compiler.pas TCompiler.Compile:', 'Active compiler set''s bin directory is not set!');
      MessageDlg(Lang[ID_ERR_BINDIR_NOT_SET], mtError, [mbOK], 0);
    end;
    Exit;
  end;

  redirectStdin := False;
  case Target of
    cttFile,cttStdin: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_COMPILINGFILE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_SOURCEFILE], [fSourceFile]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [fCompilerSet.Name]));
        DoLogEntry('');

        // Gather commands to pass to gcc/g++
        GetCompileParams;
        GetLibrariesParams;
        GetIncludesParams;

        if fCompilerSet.AddCharset and (fCompilerSet.CompilerType = ctGCC) then begin
          if not fCheckSyntax and  UseUTF8 then begin
            fCompileParams := fCompileParams + ' -finput-charset=utf-8 -fexec-charset='
              +GetSystemCharsetName();
            fCppCompileParams := fCppCompileParams + ' -finput-charset=utf-8 -fexec-charset='
              +GetSystemCharsetName();
          end else begin
            fCompileParams := fCompileParams + ' -finput-charset='+GetSystemCharsetName()+' -fexec-charset='
              +GetSystemCharsetName();
            fCppCompileParams := fCppCompileParams + ' -finput-charset='+GetSystemCharsetName()+' -fexec-charset='
              +GetSystemCharsetName();
          end;
        end;

        // Determine command line to execute
        case GetFileTyp(fSourceFile) of
          utResSrc: begin
              compilerName := fCompilerSet.windresName;
              if fCheckSyntax then
                cmdline := Format(cResourceCmdLine, [compilerName, fSourceFile, 'nul'])
              else
                cmdline := Format(cResourceCmdLine, [compilerName, fSourceFile, ChangeFileExt(fSourceFile, OBJ_EXT)]);

              DoLogEntry(Lang[ID_LOG_PROCESSINGRES]);
              DoLogEntry('--------');
              DoLogEntry(Format(Lang[ID_LOG_WINDRESNAME], [IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) + compilerName]));
              DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
            end;
          utcSrc: begin
              compilerName := fCompilerSet.gccName;
              if fCheckSyntax then begin
                if Target = cttFile then
                  cmdline := Format(cSyntaxCmdLine, [compilerName, fSourceFile, fCompileParams, fIncludesParams, fLibrariesParams])
                else begin
                  cmdline := Format(cStdinSyntaxCmdLine, [compilerName, 'c',fCompileParams, fIncludesParams]);
                  redirectStdin := True;
                end;
              end else begin
                cmdline := Format(cSourceCmdLine, [compilerName, fSourceFile, ChangeFileExt(fSourceFile, EXE_EXT), fCompileParams,
                  fIncludesParams, fLibrariesParams]);
                DeleteFile(ChangeFileExt(fSourceFile, EXE_EXT));
              end;

              DoLogEntry(Lang[ID_LOG_PROCESSINGCSRC]);
              DoLogEntry('--------');
              DoLogEntry(Format(Lang[ID_LOG_GCCNAME],
                [IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) + compilerName]));
              DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
            end;
          utCppSrc: begin
              compilerName := fCompilerSet.gppName;
              if fCheckSyntax then begin
                if Target = cttFile then
                  cmdline := Format(cSyntaxCmdLine, [compilerName, fSourceFile,
                    fCppCompileParams, fCppIncludesParams,
                    fLibrariesParams])
                else  begin
                  cmdline := Format(cStdinSyntaxCmdLine, [compilerName, 'c++',
                    fCppCompileParams, fCppIncludesParams]);
                  redirectStdin := True;
                end;
              end else begin
                cmdline := Format(cSourceCmdLine, [compilerName, fSourceFile, ChangeFileExt(fSourceFile, EXE_EXT),
                  fCppCompileParams, fCppIncludesParams, fLibrariesParams]);
                DeleteFile(ChangeFileExt(fSourceFile, EXE_EXT));
              end;
              
              DoLogEntry(Lang[ID_LOG_PROCESSINGCPPSRC]);
              DoLogEntry('--------');
              DoLogEntry(Format(Lang[ID_LOG_GPPNAME],
                [IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) + compilerName]));
              DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
            end;
          utcHead, utcppHead: begin // any header files
              compilerName := fCompilerSet.gppName;
              if fCheckSyntax then begin
                if target = cttFile then
                  cmdline := Format(cSyntaxCmdLine, [compilerName, fSourceFile,
                    fCppCompileParams, fCppIncludesParams,
                    fLibrariesParams])
                else begin
                  cmdline := Format(cStdinSyntaxCmdLine, [compilerName, 'c++',
                    fCppCompileParams, fCppIncludesParams]);
                  redirectStdin := True;
                end;
              end else
                cmdline := Format(cHeaderCmdLine, [compilerName, fSourceFile, fCompileParams, fIncludesParams,
                  fLibrariesParams]);

              DoLogEntry(Lang[ID_LOG_PROCESSINGHEADER]);
              DoLogEntry('--------');
              DoLogEntry(Format(Lang[ID_LOG_GCCNAME],
                [IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) + compilerName]));
              DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
            end;
        else begin
            compilerName := fCompilerSet.gppName;
            if fCheckSyntax then begin
              if Target = cttFile then
                cmdline := Format(cSyntaxCmdLine, [compilerName, fSourceFile,
                  fCppCompileParams, fCppIncludesParams,
                  fLibrariesParams])
              else  begin
                cmdline := Format(cStdinSyntaxCmdLine, [compilerName, 'c++',
                  fCppCompileParams, fCppIncludesParams]);
                redirectStdin := True;
              end;
            end else
              cmdline := Format(cHeaderCmdLine, [compilerName, fSourceFile, fCompileParams, fIncludesParams, fLibrariesParams]);

            DoLogEntry(Lang[ID_LOG_PROCESSINGUNKNOWN]);
            DoLogEntry('--------');
            DoLogEntry(Format(Lang[ID_LOG_GCCNAME],
              [IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) + compilerName]));
            DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
          end;
        end;
        if not FileExists(IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) + compilerName) then begin
          if not silent then begin
            LogError('Compiler.pas TCompiler.Compile:',Format('The compiler execuatble ''%s'' doesn''t exist',[IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) + compilerName]));
            MessageDlg(Format(Lang[ID_ERR_COMPILER_NOT_EXISTS],[compilerName]), mtError, [mbOK], 0);
          end;
          Exit;
        end;

        // Execute it
        LaunchThread(cmdline, ExtractFilePath(fSourceFile), redirectStdin);
      end;
    cttProject: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_PROJECTCOMPILE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_PROJECTFILE], [fProject.FileName]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [fCompilerSet.Name]));
        DoLogEntry('');

        BuildMakeFile;
        cmdline := Format(cMakeLine, [fCompilerSet.makeName, fMakeFile]);

        DoLogEntry(Lang[ID_LOG_PROCESSINGMAKE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_MAKEFILEPROC], [IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) +
          fCompilerSet.MakeName]));
        DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
        DoLogEntry('');

        // Execute it
        LaunchThread(cmdline, ExtractFilePath(Project.FileName));
      end;
  end;
end;

procedure TCompiler.RunTerminate(Sender: TObject);
begin
  Application.Restore;

  if Assigned(fOnRunEnd) then
    fOnRunEnd;

  MainForm.UpdateAppTitle;
end;

procedure TCompiler.Run;
var
  FileToRun: AnsiString;
  Parameters: AnsiString;
begin
  case fTarget of
    cttNone:
      Exit;
    cttFile: begin
        // Determine file to execute
        case GetFileTyp(fSourceFile) of
          utcSrc, utcppSrc: begin
              FileToRun := ChangeFileExt(fSourceFile, EXE_EXT);
            end;
          //else begin
          //    Exit; // nothing to run...
          //  end;
        end;

        // Check if it exists
        if not FileExists(FileToRun) then begin
          if MainForm.actCompRun.Enabled then begin // suggest a compile
            if MessageDlg(Lang[ID_ERR_SRCNOTCOMPILEDSUGGEST], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
              MainForm.actCompRunExecute(nil);
              Exit;
            end;
          end else
            MessageDlg(Lang[ID_ERR_SRCNOTCOMPILED], mtWarning, [mbOK], 0);
        end else begin
          if CompareFileModifyTime(fSourceFile,FileToRun)>=0 then begin
            if MessageDlg(Lang[ID_MSG_SOURCEMORERECENT], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
              MainForm.actCompRunExecute(nil);
                Exit;
            end;
          end;
          // Pause programs if they contain a console
          if devData.ConsolePause and ProgramHasConsole(FileToRun) then begin
            if fUseRunParams then
              Parameters := '"' + FileToRun + '" ' + fRunParams
            else
              Parameters := '"' + FileToRun + '"';

            FileToRun := devDirs.Exec + 'ConsolePauser.exe';
          end else begin
            if fUseRunParams then
              Parameters := fRunParams
            else
              Parameters := '';
            FileToRun := FileToRun;
          end;

          if devData.MinOnRun then
            Application.Minimize;
          devExecutor.ExecuteAndWatch(FileToRun, Parameters, ExtractFilePath(fSourceFile),
            True, UseInputFile,InputFile, INFINITE, RunTerminate);
          MainForm.UpdateAppTitle;
        end;
      end;
    cttProject: begin
        if fProject.Options.typ = dptStat then
          MessageDlg(Lang[ID_ERR_NOTEXECUTABLE], mtError, [mbOK], 0)
        else if not FileExists(fProject.Executable) then begin
          if MainForm.actCompRun.Enabled then begin // suggest a compile
            if MessageDlg(Lang[ID_ERR_PROJECTNOTCOMPILEDSUGGEST], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
              MainForm.actCompRunExecute(nil); // move this to mainform?
            end;
          end else
            MessageDlg(Lang[ID_ERR_SRCNOTCOMPILED], mtWarning, [mbOK], 0);
        end else if fProject.Options.typ = dptDyn then begin
          if fProject.Options.HostApplication = '' then
            MessageDlg(Lang[ID_ERR_HOSTMISSING], mtWarning, [mbOK], 0)
          else if not FileExists(fProject.Options.HostApplication) then
            MessageDlg(Lang[ID_ERR_HOSTNOTEXIST], mtWarning, [mbOK], 0)
          else begin // execute DLL's host application
            if devData.MinOnRun then
              Application.Minimize;

            devExecutor.ExecuteAndWatch(fProject.Options.HostApplication, fProject.Options.CmdLineArgs,
              ExtractFileDir(fProject.Options.HostApplication), True,
              UseInputFile,InputFile, INFINITE, RunTerminate);
            MainForm.UpdateAppTitle;
          end;
        end else begin // execute normally
          if devData.ConsolePause and ProgramHasConsole(fProject.Executable) then begin
            Parameters := '"' + fProject.Executable + '" ' + fProject.Options.CmdLineArgs;
            FileToRun := devDirs.Exec + 'ConsolePauser.exe';
          end else begin
            Parameters := fProject.Options.CmdLineArgs;
            FileToRun := fProject.Executable;
          end;

          if devData.MinOnRun then
            Application.Minimize;
          devExecutor.ExecuteAndWatch(FileToRun, Parameters, ExtractFileDir(fProject.Executable),
            True, UseInputFile,InputFile,INFINITE, RunTerminate);
          MainForm.UpdateAppTitle;
        end;
      end;
  end;
end;

procedure TCompiler.Clean;
const
  cCleanLine = '%s clean -f "%s"';
  cmsg = 'make clean';
var
  cmdLine: AnsiString;
  FileName: AnsiString;
begin
  case fTarget of
    cttFile: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_CLEANINGFILE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_SOURCEFILE], [fSourceFile]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [fCompilerSet.Name]));
        DoLogEntry('');

        FileName := '';
        case GetFileTyp(fSourceFile) of
          utresSrc: begin
              FileName := ChangeFileExt(fSourceFile, OBJ_EXT);
            end;
          utcSrc: begin
              FileName := ChangeFileExt(fSourceFile, EXE_EXT);
            end;
          utcppSrc: begin
              FileName := ChangeFileExt(fSourceFile, EXE_EXT);
            end;
        else begin
            FileName := fSourceFile + GCH_EXT;
          end;
        end;
        if FileExists(FileName) then begin
          DoLogEntry(Format(Lang[ID_LOG_CLEANEDFILE], [FileName]));
          DeleteFile(FileName);
        end else
          DoLogEntry(Lang[ID_LOG_NOCLEANFILE]);
      end;
    cttProject: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_CLEANINGPROJECT]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_PROJECTFILE], [fProject.FileName]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [fCompilerSet.Name]));
        DoLogEntry('');

        // Try to create a makefile that cleans for the whole project...
        BuildMakeFile;
        if not FileExists(fMakefile) then begin
          DoLogEntry(Lang[ID_ERR_NOMAKEFILE]);
          DoLogEntry(Lang[ID_ERR_CLEANFAILED]);
          MessageBox(MainForm.Handle, PAnsiChar(Lang[ID_ERR_NOMAKEFILE]), PAnsiChar(Lang[ID_ERROR]), MB_OK or
            MB_ICONERROR);
          Exit;
        end;

        cmdLine := Format(cCleanLine, [fCompilerSet.makeName, fMakeFile]);

        DoLogEntry(Lang[ID_LOG_PROCESSINGMAKE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_MAKEFILEPROC], [IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) +
          fCompilerSet.MakeName]));
        DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
        DoLogEntry('');

        // Let make parse the makefile
        LaunchThread(cmdLine, fProject.Directory);
      end;
  end;
end;

procedure TCompiler.RebuildAll; // TODO: unite with TCompiler.Clean?
const
  cCleanLine = '%s -f "%s" clean all';
var
  cmdLine: AnsiString;
begin
  case Target of
    cttFile: begin
        Compile;
      end;
    cttProject: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_REBUILDINGPROJECT]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_PROJECTFILE], [fProject.FileName]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [fCompilerSet.Name]));
        DoLogEntry('');

        // Try to create a makefile for the whole project...
        BuildMakeFile;
        if not FileExists(fMakefile) then begin
          DoLogEntry(Lang[ID_ERR_NOMAKEFILE]);
          DoLogEntry(Lang[ID_ERR_CLEANFAILED]);
          MessageBox(MainForm.Handle, PAnsiChar(Lang[ID_ERR_NOMAKEFILE]), PAnsiChar(Lang[ID_ERROR]), MB_OK or
            MB_ICONERROR);
          Exit;
        end;

        cmdLine := Format(cCleanLine, [fCompilerSet.makeName, fMakeFile]);

        DoLogEntry(Lang[ID_LOG_PROCESSINGMAKE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_MAKEFILEPROC],
          [IncludeTrailingPathDelimiter(fCompilerSet.BinDir[0]) + fCompilerSet.MakeName]));
        DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
        DoLogEntry('');

        // Let make parse the makefile
        LaunchThread(cmdLine, fProject.Directory);
      end;
  end;
end;

procedure TCompiler.LaunchThread(const s, dir: AnsiString; redirectStdin: boolean);
var
  waitCount:integer;
begin
{
  if Assigned(fDevRun) then
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0)
  else begin
}
  // wait check syntax ends, at most 10 seconds
  waitCount := 0;
  while Assigned(fDevRun) do begin
    if (not fDevRun.RedirectStdin) or (waitCount > 100) then begin
      MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
      Exit;
    end;
    inc(waitCount);
    Sleep(100);
    Application.ProcessMessages;
  end;
  fAbortThread := False;
  fDevRun := TDevRun.Create(true);
  fDevRun.Command := s;
  fDevRun.Directory := dir;
  fDevRun.OnTerminate := OnCompilationTerminated;
  fDevRun.OnLineOutput := OnLineOutput;
  fDevRun.OnCheckAbort := ThreadCheckAbort;
  fDevRun.InputText := SourceText;
  fDevRun.FreeOnTerminate := True;
  fDevRun.RedirectStdin:= redirectStdin;
  fDevRun.Resume;

  MainForm.UpdateAppTitle;
end;

procedure TCompiler.ThreadCheckAbort(var AbortThread: boolean);
begin
  AbortThread := fAbortThread;
end;

procedure TCompiler.AbortThread;
begin
  if not Assigned(fDevRun) then
    Exit;
  fAbortThread := True;
end;

procedure TCompiler.OnCompilationTerminated(Sender: TObject);
begin
  if Assigned(fOnCompEnd) then
    fOnCompEnd;

  fDevRun := nil;

  MainForm.UpdateAppTitle;

  EndProgressForm;

  if (fErrCount = 0) and not fAbortThread and Assigned(fOnCompSuccess) then
    fOnCompSuccess;
end;

procedure TCompiler.OnLineOutput(Sender: TObject; const Line: AnsiString);
var
  List: TStringList;
  I: integer;
begin
  DoLogEntry(Line);

  List := TStringList.Create;
  List.Text := Line;
  for I := 0 to List.Count - 1 do begin
    ProcessOutput(List.Strings[I]);
    ProcessProgressForm(List.Strings[I]);
  end;
  List.Free;
end;

procedure TCompiler.ProcessOutput(const line: AnsiString);
var
  OLine, OCol, OFile, OMsg, S: AnsiString;
  delim: integer;
  messageType : TGCCMessageType;

  procedure GetFileName; // obtain delimiter AFTER (full) filename
  begin
    while True do begin
      OMsg := Trim(OMsg);
      if (Length(OMsg) > 2) and (OMsg[2] = ':') then begin // full file path at start, ignore this one
        delim := FPos(':', OMsg, 3);
      end else begin // find first
        delim := FPos(':', OMsg, 1);
      end;

      if delim > 0 then begin
        OFile := Copy(OMsg, 1, delim - 1);
        Delete(OMsg, 1, delim);
      end else
        break;

      if not EndsText('ld.exe',OFile) then // it's not a ld.exe output, stop parsing
        break;
    end;
    if EndsStr('<stdin>',oFile) then
      oFile := SourceFile;
  end;

  procedure GetLineNumber;
  begin
    OMsg := Trim(OMsg);
    delim := FPos(':', OMsg, 1);
    if delim = 0 then
      delim := FPos(',', OMsg, 1);
    if delim > 0 then begin
      OLine := Copy(OMsg, 1, delim - 1);
      if StrToIntDef(OLine, -1) = -1 then // don't accept
        OLine := ''
      else
        Delete(OMsg, 1, delim);
    end;
  end;

  procedure GetColNumber;
  begin
    OMsg := Trim(OMsg);
    delim := FPos(':', OMsg, 1);
    if delim = 0 then
      delim := FPos(',', OMsg, 1);
    if delim > 0 then begin
      OCol := Copy(OMsg, 1, delim - 1);
      if StrToIntDef(OCol, -1) = -1 then // don't accept
        OCol := ''
      else
        Delete(OMsg, 1, delim);
    end;
  end;

  function GetMessageType:TGCCMessageType;
  begin
    Result := gmtNone;
    OMsg := Trim(OMsg);
    delim := FPos(':', OMsg, 1);
    if delim > 0 then begin
      S := Copy(OMsg, 1, delim - 1);
      if SameStr(S, 'error') or SameStr(S, 'fatal error') then begin
        Inc(fErrCount);
        Delete(OMsg, 1, delim + 1);
        OMsg := '[Error] ' + Trim(OMsg);
        Result:=gmtError;
      end else if SameStr(S, 'warning') then begin
        Inc(fWarnCount);
        Delete(OMsg, 1, delim + 1);
        OMsg := '[Warning] ' + Trim(OMsg);
        Result:=gmtWarning;
      end else if SameStr(S, 'info') then begin
        //Inc(fInfoCount);
        Delete(OMsg, 1, delim + 1);
        OMsg := '[Info] ' + Trim(OMsg);
        Result:=gmtInfo;
      end else if SameStr(S, 'note') then begin
        //Inc(fInfoCount);
        Delete(OMsg, 1, delim + 1);
        OMsg := '[Note] ' + Trim(OMsg);
        Result:=gmtNote;
      end;
    end;
  end;
begin
  OLine := '';
  OCol := '';
  OFile := '';
  OMsg := Trim(Line);

  // Ignore generic 'we are starting program x' messages
  if (Pos(fCompilerSet.gccName + ' ', Line) = 1) or
    (Pos(fCompilerSet.gppName + ' ', Line) = 1) or
    (Pos(fCompilerSet.makeName, Line) = 1) or // ignore all make errors for now
  (Pos(fCompilerSet.windresName + ' ', Line) = 1) or
    (Pos(CLEAN_PROGRAM + ' ', Line) = 1) then
    Exit;

  // Direction strings
  if StartsStr('In file included from ', OMsg) then begin
    Delete(OMsg, 1, Length('In file included from '));

    GetFileName;
    GetLineNumber;
    GetColNumber;

    OMsg := 'In file included from ' + OFile;
    DoOutput(OLine, OCol, OFile, OMsg);
    Exit;
  end else if StartsStr('from ', OMsg) then begin
    Delete(OMsg, 1, Length('from '));

    GetFileName;
    GetLineNumber;
    GetColNumber;

    OMsg := '                 from ' + OFile;
    DoOutput(OLine, OCol, OFile, OMsg);
    Exit;
  end;

  // Ignore code snippets that GCC 4.8 produces
  if (Length(line) > 0) and (Line[1] = ' ') then // they always start with a space
    Exit;

  GetFileName; // assume regular main.cpp:line:col: message

  if SameStr(OFile, 'windres.exe') then begin // resource error
    GetFileName;
    GetLineNumber;
    Inc(fErrCount); // assume it's always an error

    DoResOutput(OLine, '', OFile, OMsg);
  end else begin
    GetLineNumber;
    GetColNumber;
    messageType:= GetMessageType;

    DoOutput(OLine, OCol, OFile, OMsg);
  end;
end;

procedure TCompiler.GetLibrariesParams;
resourcestring
  cAppendStr = '%s -L"%s"';
var
  i, val: integer;
  option: TCompilerOption;
  e:TEditor;
  fullPath : String;
  autolinkIndexes : TStringHash;
  parsedFiles: TStringHash;
  logger:TStringList;
  waitCount: integer;

  procedure ParseFileInclude(_FileName:String);
  var
    includedFiles : TStringList;
    i,idx: integer;
  begin
    if parsedFiles.ValueOf(_FileName)>=0 then
      Exit;
    parsedFiles.Add(_FileName,1);
    includedFiles := TStringList.Create;
    try
      idx := autolinkIndexes.ValueOf(_filename);
      if idx>=0 then begin
         fLibrariesParams := fLibrariesParams + ' ' + dmMain.AutoLinks[idx]^.linkParams;
      end;
      //the includedFiles must not be sorted
      e.CppParser.GetFileDirectIncludes(_filename,includedFiles);
      //last included file parsed first,cause it may depends on the files included before it
      for i:=includedFiles.Count-1 downto 0 do begin
        if (SameText(_filename,includedFiles[i])) then
          continue;
        ParseFileInclude(includedFiles[i]);
      end;
    finally
      includedFiles.Free;
    end;
  end;
begin
  // Add libraries
  fLibrariesParams := FormatList(fCompilerSet.LibDir, cAppendStr);   

  // Add project settings that need to be passed to the linker
  for I := 0 to fCompilerSet.Options.Count - 1 do begin
    option := PCompilerOption(fCompilerSet.Options[I])^;
    if (Assigned(fProject) and (I < Length(fProject.Options.CompilerOptions))) or (not Assigned(fProject) and
      (option.Value > 0)) then begin
      if option.IsLinker then
        if Assigned(option.Choices) then begin
          if Assigned(fProject) then
            val := CharToValue(fProject.Options.CompilerOptions[I + 1])
          else
            val := option.Value;
          if (val > 0) and (val < option.Choices.Count) then
            fLibrariesParams := fLibrariesParams + ' ' + option.Setting +
              option.Choices.Values[option.Choices.Names[val]];
        end else if (Assigned(fProject) and (StrToIntDef(fProject.Options.CompilerOptions[I + 1], 0) = 1)) or (not
          Assigned(fProject)) then
          fLibrariesParams := fLibrariesParams + ' ' + option.Setting;
    end;
  end;

  fLibrariesParams := Trim(fLibrariesParams);

  //Add auto links
  if (fTarget = cttFile) and devCompiler.EnableAutoLinks then begin
    e:=MainForm.EditorList.GetEditor();
    if Assigned(e) then begin
      waitCount := 0;
      //wait parsing ends, at most 1 second
      while (e.CppParser.Parsing) do begin
        if waitCount> 10 then begin
          break;
        end;
        inc(waitCount);
        Sleep(100);
        Application.ProcessMessages;
      end;
      autolinkIndexes := TStringHash.Create;
      parsedFiles := TStringHash.Create;
      try
        for i:=0 to dmMain.AutoLinks.Count - 1 do begin
          fullPath := e.CppParser.GetHeaderFileName(e.FileName,'"'+dmMain.AutoLinks[i]^.header+'"');
          autolinkIndexes.Add(fullPath,i);
        end;
        ParseFileInclude(e.FileName);
      finally
        parsedFiles.Free;
        autolinkIndexes.Free;
      end;
    end;
  end;

  fLibrariesParams := Trim(fLibrariesParams);  

  // Add global compiler linker extras
  if fCompilerSet.AddtoLink and (Length(fCompilerSet.LinkOpts) > 0) then
    fLibrariesParams := fLibrariesParams + ' ' + fCompilerSet.LinkOpts;

  // Add libs added via project
  if (fTarget = cttProject) and assigned(fProject) then begin
    for i := 0 to pred(fProject.Options.Libs.Count) do
      fLibrariesParams := format(cAppendStr, [fLibrariesParams, fProject.Options.Libs[i]]);

    // got sick of "symbol 'blah blah' is deprecated"
    if fProject.Options.typ = dptGUI then
      fLibrariesParams := fLibrariesParams + ' -mwindows';

    // Add project compiler linker extras
    if Length(fProject.Options.LinkerCmd) > 0 then
      fLibrariesParams := fLibrariesParams + ' ' + StringReplace(fProject.Options.LinkerCmd, '_@@_', ' ', [rfReplaceAll])
  end;

  fLibrariesParams := Trim(fLibrariesParams);

  if (fCompilerSet.StaticLinkStdlib and not Assigned(fProject))
    or
   (Assigned(fProject)  and fProject.Options.StaticLink) then begin
//    fLibrariesParams := fLibrariesParams + ' -static-libgcc -static-libstdc++ -Wl,-Bstatic -lstdc++ -lpthread -Wl,-Bdynamic';
    fLibrariesParams := fLibrariesParams + ' -static';
  end;

  fLibrariesParams := Trim(fLibrariesParams);
end;

procedure TCompiler.GetIncludesParams;
resourcestring
  cAppendStr = '%s -I"%s"';
var
  i: integer;
begin
  fIncludesParams := FormatList(fCompilerSet.CDir, cAppendStr);
  fCppIncludesParams := FormatList(fCompilerSet.CppDir, cAppendStr);

 if ((fTarget = cttProject) or fCheckSyntax) and assigned(fProject) then
    for i := 0 to pred(fProject.Options.Includes.Count) do
      if DirectoryExists(fProject.Options.Includes[i]) then begin
        fIncludesParams := format(cAppendStr, [fIncludesParams, fProject.Options.Includes[i]]);
        fCppIncludesParams := format(cAppendStr, [fCppIncludesParams, fProject.Options.Includes[i]]);
      end;

  fIncludesParams := Trim(fIncludesParams);
  fCppIncludesParams := Trim(fCppIncludesParams);
end;

function TCompiler.GetCompiling: Boolean;
begin
  Result := Assigned(fDevRun);
end;

procedure TCompiler.InitProgressForm;
var
  numsourcefiles, I: integer;
begin
  // Count file types
  if Assigned(fProject) then begin
    numsourcefiles := 0;
    for I := 0 to fProject.Units.Count - 1 do
      if GetFileTyp(fProject.Units[I].FileName) in [utcSrc, utcppSrc] then
        Inc(numsourcefiles);

    MainForm.pbCompilation.Min := 0;
    MainForm.pbCompilation.Max := numsourcefiles + 3; // cleaning + all project units + linking output + private resource
    MainForm.pbCompilation.Position := 0;
  end else
    MainForm.pbCompilation.Max := 1; // just fSourceFile

  // Initialize counters
  fStartTime := GetTickCount;
  fWarnCount := 0;
  fErrCount := 0;

  // Set some preferences
  fShowOutputInfo := not fCheckSyntax;
end;

procedure TCompiler.ProcessProgressForm(const Line: AnsiString);
var
  filename: AnsiString;
  I: integer;
  Done: boolean;
begin
  Done := false;

  // The compiler started to compile a new file
  if StartsStr(fCompilerSet.gppName + ' ', Line) or StartsStr(fCompilerSet.gccName + ' ', Line) then begin
    filename := '';
    if Assigned(fProject) then begin
      for I := 0 to fProject.Units.Count - 1 do begin
        filename := ExtractFilename(fProject.Units[I].FileName);
        if Pos(filename, Line) > 0 then begin
          MainForm.pbCompilation.StepIt;
          Done := true;
          break;
        end;
      end;
    end else if Pos(fSourceFile, Line) > 0 then begin
      MainForm.pbCompilation.StepIt;
      Done := true;
    end;

    if not Done then begin // might be the linker
      if Assigned(fProject) then
        filename := ExtractFileName(fProject.Executable)
      else
        filename := fSourceFile;

      if ContainsStr(Line, filename) then begin
        MainForm.pbCompilation.StepIt;
      end;
    end;
  end else if StartsStr(CLEAN_PROGRAM + ' ', Line) then begin // Cleaning obj files
    MainForm.pbCompilation.StepIt;
  end else if StartsStr(fCompilerSet.windresName + ' ', Line) and Assigned(fProject) then begin // Resource files
    filename := ExtractFileName(fProject.Options.PrivateResource);
    if ContainsStr(Line, filename) then begin
      MainForm.pbCompilation.StepIt;
    end;
  end;
end;

procedure TCompiler.EndProgressForm;
var
  CompileTime: Extended; // fp
  FileName: AnsiString;
begin
  MainForm.pbCompilation.Position := 0;

  CompileTime := (GetTickCount - fStartTime) / 1000;

  DoLogEntry('');
  DoLogEntry(Lang[ID_LOG_COMPILERESULTS]);
  DoLogEntry('--------');
  DoLogEntry(Format(Lang[ID_LOG_ERRORS], [fErrCount]));
  DoLogEntry(Format(Lang[ID_LOG_WARNINGS], [fWarnCount]));

  // Check if the output file has been created
  if Assigned(fProject) then
    FileName := ExpandFileto(fProject.Executable, fProject.Directory)
  else begin
    case GetFileTyp(fSourceFile) of
      utresSrc: begin
          FileName := ChangeFileExt(fSourceFile, OBJ_EXT);
        end;
      utcSrc: begin
          FileName := ChangeFileExt(fSourceFile, EXE_EXT);
        end;
      utcppSrc: begin
          FileName := ChangeFileExt(fSourceFile, EXE_EXT);
        end;
      utcHead, utcppHead: begin
          FileName := fSourceFile + GCH_EXT;
        end;
    else begin
        FileName := fSourceFile;
      end;
    end;
  end;

  // Only show information if we managed to create the file and if it was the result of the current compilation
  if fShowOutputInfo and FileExists(FileName) then begin
    DoLogEntry(Format(Lang[ID_LOG_OUTPUTFILE], [FileName]));
    DoLogEntry(Format(Lang[ID_LOG_OUTPUTSIZE], [FormatFileSize(GetFileSize(FileName))]));
  end;
  DoLogEntry(Format(Lang[ID_LOG_COMPILETIME], [CompileTime]));

  if Assigned(fProject) then
    fProject.SaveToLog;
end;

end.

