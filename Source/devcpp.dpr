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

program devcpp;
{$R 'icons.res' 'icons.rc'}
{$R 'DefaultFiles.res' 'DefaultFiles.rc'}
{%File 'LangIDs.inc'}
{$R 'manifest.res' 'manifest.rc'}
{%File 'FastMM4Options.inc'}
uses
  FastMM4 in 'FastMM4.pas',
  Windows,
  Forms,
  sysUtils,
  SHFolder,
  Messages,
  main in 'main.pas' {MainForm},
  MultiLangSupport in 'MultiLangSupport.pas',
  Version in 'Version.pas',
  Utils in 'Utils.pas',
  Tests in 'Tests.pas',
  LangFrm in 'LangFrm.pas' {LangForm},
  Project in 'Project.pas',
  Templates in 'Templates.pas',
  NewProjectFrm in 'NewProjectFrm.pas' {NewProjectForm},
  RemoveUnitFrm in 'RemoveUnitFrm.pas' {RemoveUnitForm},
  GotoLineFrm in 'GotoLineFrm.pas' {GotoLineForm},
  PrintFrm in 'PrintFrm.pas' {PrintForm},
  AboutFrm in 'AboutFrm.pas' {AboutForm},
  Compiler in 'Compiler.pas',
  devrun in 'devrun.pas',
  ProjectOptionsFrm in 'ProjectOptionsFrm.pas' {ProjectOptionsForm},
  ToolFrm in 'ToolFrm.pas' {ToolFrom},
  ToolEditFrm in 'ToolEditFrm.pas' {ToolEditForm},
  IconFrm in 'IconFrm.pas' {IconForm},
  devcfg in 'devCFG.pas',
  DataFrm in 'DataFrm.pas' {dmMain: TDataModule},
  EditorOptFrm in 'EditorOptfrm.pas' {EditorOptForm},
  IncrementalFrm in 'IncrementalFrm.pas' {IncrementalForm},
  FindFrm in 'FindFrm.pas' {FindForm},
  Editor in 'Editor.pas',
  EnviroFrm in 'EnviroFrm.pas' {EnviroForm},
  DebugReader in 'DebugReader.pas',
  Debugger in 'Debugger.pas',
  EditorList in 'EditorList.pas',
  CFGData in 'CFGData.pas',
  ProjectTypes in 'ProjectTypes.pas',
  Macros in 'Macros.pas',
  devExec in 'devExec.pas',
  NewTemplateFrm in 'NewTemplateFrm.pas' {NewTemplateForm},
  FunctionSearchFrm in 'FunctionSearchFrm.pas' {FunctionSearchForm},
  NewVarFrm in 'NewVarFrm.pas' {NewVarForm},
  NewFunctionFrm in 'NewFunctionFrm.pas' {NewFunctionForm},
  NewClassFrm in 'NewClassFrm.pas' {NewClassForm},
  ProfileAnalysisFrm in 'ProfileAnalysisFrm.pas' {ProfileAnalysisForm},
  FilePropertiesFrm in 'FilePropertiesFrm.pas' {FilePropertiesForm},
  AddToDoFrm in 'AddToDoFrm.pas' {AddToDoForm},
  ViewToDoFrm in 'ViewToDoFrm.pas' {ViewToDoForm},
  ImportMSVCFrm in 'ImportMSVCFrm.pas' {ImportMSVCForm},
  ImportCBFrm in 'ImportCBFrm.pas' {ImportCBForm},
  CPUFrm in 'CPUFrm.pas' {CPUForm},
  FileAssocs in 'FileAssocs.pas',
  TipOfTheDayFrm in 'TipOfTheDayFrm.pas' {TipOfTheDayForm},
  ExceptionFrm in 'ExceptionFrm.pas' {ExceptionFrm},
  WindowListFrm in 'WindowListFrm.pas' {WindowListForm},
  ParamsFrm in 'ParamsFrm.pas' {ParamsForm},
  CompOptionsFrame in 'CompOptionsFrame.pas' {CompOptionsFrame: TFrame},
  CompOptionsFrm in 'CompOptionsFrm.pas' {CompOptionsForm},
  FormatterOptionsFrm in 'FormatterOptionsFrm.pas' {FormatterOptionsForm},
  ProcessListFrm in 'ProcessListFrm.pas' {ProcessListForm},
  PackmanExitCodesU in 'Tools\Packman\PackmanExitCodesU.pas',
  ImageTheme in 'ImageTheme.pas',
  Instances in 'Instances.pas',
  RenameFrm in 'RenameFrm.pas' {RenameForm},
  Refactorer in 'Refactorer.pas',
  CodePage in 'CodePage.pas',
  Registry,
  CodeInsList in 'CodeInsList.pas',
  TabnineForm in 'TabnineForm.pas' {TabnineForm},
  Tabnine in 'Tabnine.pas',
  AutoLinkList in 'AutoLinkList.pas';

{$R *.res}

var
  AppData, INIFileName, ExeFolder: AnsiString;
  Buffer: array[0..MAX_PATH] of char;
  PrevInstance: THandle;
  lReg: TRegistry;
  regPath: AnsiString;
  configFolder : AnsiString;
  I:integer;
  hasFileToOpen:boolean;
  hasDevToOpen:boolean;
  TempDir : string;
  lockPath : string;
  hLockFile : THandle;
  count :integer;  
begin
  I := 1; // skip first one
  hasFileToOpen := False;
  hasDevToOpen := False;
  while (I <= ParamCount) do begin
    // Skip the configuration redirect stuff
    if ParamStr(i) = '-c' then begin
      I := I + 2;
      Continue;
    end;

    hasFileToOpen := True;
    if EndsStr('.dev',ParamStr(i)) then
      hasDevToOpen := True;
    Inc(I);
  end;

  count:=0;
  TempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'));
  LockPath := TempDir + 'RedPandaDevCppStartUp.lock';
  while True do begin
    hLockFile := CreateFile(pAnsiChar(LockPath), GENERIC_READ or GENERIC_WRITE, 0,
      nil, OPEN_ALWAYS, FILE_ATTRIBUTE_TEMPORARY,0);
    if hLockFile = INVALID_HANDLE_VALUE then begin
      if GetLastError <> ERROR_SHARING_VIOLATION then begin
        break;
      end;
      inc(count);
      Sleep(100);
      if count>50 then begin
        break;
      end;
    end else
      break;
  end;
  try
    // Check for previous instances (only allow once instance)
    // If we are able to find a previous instance, activate that one instead
    if hasFileToOpen and not hasDevToOpen then begin
      PrevInstance := GetPreviousInstance;
      if PrevInstance <> 0 then begin
        if PrevInstance <> INVALID_HANDLE_VALUE then  begin
          SendToPreviousInstance(PrevInstance, AnsiString(GetCommandLineW));
          Exit;
        end;
      end;
    end;

    // Read INI filename
    INIFileName := ChangeFileExt(ExtractFileName(Application.ExeName), INI_EXT);
    ExeFolder := ExtractFilePath(Application.ExeName);

    // open registry, set root and key
    regPath:= '';
    lReg := TRegistry.Create;
    try
      lReg.RootKey := HKEY_LOCAL_MACHINE;
      if lReg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Uninstall\Dev-C++', True) then begin
        // write last Left, Top, Width and Height
        regPath := ExtractFilePath(lReg.ReadString('UninstallString'));
        // close all
        lReg.CloseKey;
      end;
    finally
      lReg.Free;
    end;

    // Create config files directory
    // Set devData.INIFileName, ConfigMode
    // Did someone pass the -c command to us?
    if (ParamCount >= 2) and SameStr(ParamStr(1), '-c') then begin
      if not DirectoryExists(ParamStr(2)) then
        CreateDir(ParamStr(2));

      // Store the INI file in the directory given to us
      if ParamStr(2)[2] <> ':' then begin// if a relative path is specified...
        devData.INIFileName := ExeFolder + IncludeTrailingBackslash(ParamStr(2)) + INIFileName;
      end else begin
        devData.INIFileName := IncludeTrailingBackslash(ParamStr(2)) + INIFileName;
      end;
    end else begin
      // default dir should be %APPDATA%\Dev-Cpp
      AppData := '';
      if SUCCEEDED(SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, Buffer)) then
        AppData := IncludeTrailingBackslash(AnsiString(Buffer));

      // Store the INI file in %APPDATA% or if we are not allowed to do so, in the exe directory
      if SameStr(regPath,exeFolder) and
         (AppData <> '') and
        (DirectoryExists(AppData + 'Dev-Cpp') or CreateDir(AppData + 'Dev-Cpp')) then begin
        devData.INIFileName := AppData + 'Dev-Cpp\' + INIFileName;
        devData.Portable := False;
      end else begin
          // store it in the default portable config folder anyways...
        configFolder := ExeFolder + 'config';
        if not DirectoryExists(configFolder) then
          CreateDir(configFolder);
        devData.INIFileName := configFolder + '\' + INIFileName;
        devData.Portable := True;
      end;
    end;

  // free ansistrings...
    SetLength(AppData, 0);
    SetLength(INIFileName, 0);
    SetLength(ExeFolder, 0);

    // Load settings
    devData.ReadSelf;
    CreateOptions;

    // Create main window
    Application.Initialize;
    Application.Title := 'Dev-C++';
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TRenameForm, RenameForm);
  finally
    if hLockFile<>INVALID_HANDLE_VALUE then
      CloseHandle(hLockFile);
  end;
  Application.Run;
end.

