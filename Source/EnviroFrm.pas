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

unit EnviroFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, ExtDlgs, Buttons,
  CheckLst, Grids, ValEdit, ComCtrls;

type
  TEnviroForm = class(TForm)
    dlgPic: TOpenPictureDialog;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    PagesMain: TPageControl;
    tabGeneral: TTabSheet;
    lblMRU: TLabel;
    lblMsgTabs: TLabel;
    lblLang: TLabel;
    cbBackups: TCheckBox;
    cbMinOnRun: TCheckBox;
    cbDefCpp: TCheckBox;
    cbShowBars: TCheckBox;
    cbMultiLineTab: TCheckBox;
    rgbAutoOpen: TRadioGroup;
    gbDebugger: TGroupBox;
    cbWatchHint: TCheckBox;
    gbProgress: TGroupBox;
    cbShowProgress: TCheckBox;
    cbAutoCloseProgress: TCheckBox;
    seMRUMax: TSpinEdit;
    cboTabsTop: TComboBox;
    cboLang: TComboBox;
    tabPaths: TTabSheet;
    lblUserDir: TLabel;
    lblTemplatesDir: TLabel;
    lblSplash: TLabel;
    lblIcoLib: TLabel;
    lblLangPath: TLabel;
    btnDefBrws: TSpeedButton;
    btnOutputbrws: TSpeedButton;
    btnBrwIcon: TSpeedButton;
    btnBrwLang: TSpeedButton;
    btnBrwSplash: TSpeedButton;
    edUserDir: TEdit;
    edTemplatesDir: TEdit;
    edSplash: TEdit;
    edIcoLib: TEdit;
    edLang: TEdit;
    tabExternal: TTabSheet;
    lblExternal: TLabel;
    btnExtAdd: TSpeedButton;
    btnExtDel: TSpeedButton;
    vleExternal: TValueListEditor;
    tabAssocs: TTabSheet;
    lblAssocFileTypes: TLabel;
    lblAssocDesc: TLabel;
    lstAssocFileTypes: TCheckListBox;
    UIfontlabel: TLabel;
    cbUIfont: TComboBox;
    cbUIfontsize: TComboBox;
    cbPauseConsole: TCheckBox;
    cbCheckAssocs: TCheckBox;
    edOptionsDir: TEdit;
    lblOptionsDir: TLabel;
    btnResetDev: TButton;
    cbShowDbgCmd: TCheckBox;
    cbShowDbgFullAnnotation: TCheckBox;
    btnHighDPIFixExit: TButton;
    lblProjectsDir: TLabel;
    edProjectsDir: TEdit;
    btnProjectsDir: TSpeedButton;
    btnOpenOptionsDir: TSpeedButton;
    tabIcon: TTabSheet;
    lblMenuIconSize: TLabel;
    cbMenuIconSize: TComboBox;
    lblToolbarIconSize: TLabel;
    cbToolbarIconSize: TComboBox;
    lblTabIconSize: TLabel;
    cbTabIconSize: TComboBox;
    procedure BrowseClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vleExternalEditButtonClick(Sender: TObject);
    procedure vleExternalValidate(Sender: TObject; ACol, ARow: Integer; const KeyName, KeyValue: string);
    procedure btnExtAddClick(Sender: TObject);
    procedure btnExtDelClick(Sender: TObject);
    procedure cbUIfontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cbUIfontsizeDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cbUIfontsizeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnResetDevClick(Sender: TObject);
    procedure btnHighDPIFixExitClick(Sender: TObject);
    procedure btnOpenOptionsDirClick(Sender: TObject);
  private
    procedure LoadText;
  end;

implementation

uses
  Registry,ShellAPI, Filectrl, devcfg, MultiLangSupport, version, DataFrm, utils, FileAssocs, ImageTheme, main;

{$R *.dfm}

procedure TEnviroForm.BrowseClick(Sender: TObject);
var
  s: AnsiString;
begin
  case TComponent(Sender).Tag of
    1: {// default dir browse} begin
        s := edUserDir.Text;
        if NewSelectDirectory(Lang[ID_ENV_SELUSERDIR], '', s) then
          edUserDir.Text := IncludeTrailingPathDelimiter(s);
      end;

    2: {// output dir browse} begin
        s := ExpandFileto(edTemplatesDir.Text, devDirs.Exec);
        if NewSelectDirectory(Lang[ID_ENV_SELTEMPLATESDIR], '', s) then
          edTemplatesDir.Text := IncludeTrailingPathDelimiter(s);
      end;

    3: {// icon library browse} begin
        s := ExpandFileto(edIcoLib.Text, devDirs.Exec);
        if NewSelectDirectory(Lang[ID_ENV_SELICOLIB], '', s) then
          edIcoLib.Text := IncludeTrailingPathDelimiter(s);
      end;

    4: {// splash screen browse} begin
        dlgPic.InitialDir := ExtractFilePath(edSplash.Text);
        if dlgPic.Execute then
          edSplash.Text := dlgPic.FileName;
      end;

    5: {// Language Dir} begin
        s := ExpandFileto(edLang.Text, devDirs.Exec);
        if NewSelectDirectory(Lang[ID_ENV_SELLANGDIR], '', s) then
          edLang.Text := IncludeTrailingPathDelimiter(ExtractRelativePath(devDirs.Exec, s));
      end;

    6: {// Project Dir} begin
        s := edProjectsDir.Text;
        if NewSelectDirectory(Lang[ID_ENV_SELPROJDIR], '', s) then
          edProjectsDir.Text := IncludeTrailingPathDelimiter(s);
      end;
  end;
end;

procedure TEnviroForm.btnOkClick(Sender: TObject);
var
  I: integer;
  s: AnsiString;
begin
  with devData do begin
    DefCpp := cbDefCpp.Checked;
    ShowBars := cbShowBars.Checked;
    MultiLineTab := cbMultiLineTab.Checked;
    BackUps := cbBackups.Checked;
    MinOnRun := cbMinOnRun.Checked;
    ConsolePause := cbPauseConsole.Checked;
    CheckAssocs := cbCheckAssocs.Checked;
    MRUMax := seMRUMax.Value;
    if not MultiLineTab then begin
      if cboTabsTop.ItemIndex in [2, 3] then begin
        MessageBox(application.handle, PAnsiChar(Lang[ID_ENV_MULTILINETABERROR]), PAnsiChar(Lang[ID_ERROR]), MB_OK);
        cboTabsTop.ItemIndex := 0;
      end;
    end;
    MsgTabs := TTabPosition(cboTabsTop.ItemIndex);
    AutoOpen := rgbAutoOpen.ItemIndex;
    Splash := edSplash.Text;

    s := Lang.FileFromDescription(cboLang.Text);
    LangChange := s <> Language;
    Language := s;
    ThemeChange := (cbMenuIconSize.Text <> MenuIconSize)
      or (cbToolbarIconSize.Text <> ToolbarIconSize)
      or (cbTabIconSize.Text <> TabIconSize);
    MenuIconSize := cbMenuIconSize.Text;
    ToolbarIconSize := cbToolbarIconSize.Text;
    TabIconSize := cbTabIconSize.Text;
    ShowProgress := cbShowProgress.Checked;
    AutoCloseProgress := cbAutoCloseProgress.Checked;
    WatchHint := cbWatchHint.Checked;
    InterfaceFont := cbUIFont.Text;
    InterfaceFontSize := StrToIntDef(cbUIfontsize.Text, 9);
  end;


  devDebugger.ShowCommandLog := cbShowDbgCmd.Checked;
  devDebugger.ShowAnnotations := cbShowDbgFullAnnotation.Checked;

  MainForm.Font.Name := devData.InterfaceFont;
  MainForm.Font.Size := devData.InterfaceFontSize;
  MainForm.DebugOutput.Font.Size :=  devData.InterfaceFontSize;

  try

    // Force update
    for I := 0 to AssociationsCount - 1 do
      if lstAssocFileTypes.Checked[I] and (not IsAssociated(I)) then
        Associate(I)
      else if (not lstAssocFileTypes.Checked[I]) and IsAssociated(I) then
        Unassociate(I);
  except
    MessageBox(application.handle, PAnsiChar(Lang[ID_ENV_UACERROR]), PAnsiChar(Lang[ID_ERROR]), MB_OK);
    devData.CheckAssocs := false; // don't bother the user again on next startup
  end;

  devDirs.Icons := IncludeTrailingPathDelimiter(ExpandFileto(edIcoLib.Text, devDirs.Exec));
  devDirs.Templates := IncludeTrailingPathDelimiter(ExpandFileto(edTemplatesDir.Text, devDirs.Exec));
  devDirs.Default := edUserDir.Text;
  devDirs.Projects := edProjectsDir.Text;


  if edLang.Text <> ExtractRelativePath(devDirs.Exec, devDirs.Lang) then begin
    devDirs.Lang := IncludeTrailingPathDelimiter(ExpandFileto(edLang.Text, devDirs.Exec));
    Lang.CheckLanguageFiles;
  end;

  devExternalPrograms.Programs.Assign(vleExternal.Strings);
end;

procedure TEnviroForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_ENV];

  //Tabs
  tabGeneral.Caption := Lang[ID_ENV_GENTAB];
  tabPaths.Caption := Lang[ID_ENV_PATHTAB];
  tabAssocs.Caption := Lang[ID_ENV_FASSTAB];
  tabExternal.Caption := Lang[ID_ENV_EXTERNALS];
  tabIcon.Caption := Lang[ID_ENV_ICONS];

  //Buttons
  btnOk.Caption := Lang[ID_BTN_OK];
  btnCancel.Caption := Lang[ID_BTN_CANCEL];
  btnHelp.Caption := Lang[ID_BTN_HELP];

  //Controls
  cbDefCpp.Caption := Lang[ID_ENV_DEFCPP];
  cbShowBars.Caption := Lang[ID_ENV_SHOWBARS];
  cbMultiLineTab.Caption := Lang[ID_ENV_MULTILINETABS];
  cbBackups.Caption := Lang[ID_ENV_BACKUPS];
  cbMinOnRun.Caption := Lang[ID_ENV_MINONRUN];
  cbPauseConsole.Caption := Lang[ID_ENV_PAUSECONSOLE];
  cbCheckAssocs.Caption := Lang[ID_ENV_CHECKASSOCS];


  gbProgress.Caption := Lang[ID_ENV_COMPPROGRESSWINDOW];
  cbShowProgress.Caption := Lang[ID_ENV_SHOWPROGRESS];
  cbAutoCloseProgress.Caption := Lang[ID_ENV_AUTOCLOSEPROGRESS];

  cbWatchHint.Caption := Lang[ID_ENV_WATCHHINT];
  gbDebugger.Caption := Lang[ID_ENV_DEBUGGER];
  cbShowDbgCmd.Caption := Lang[ID_DEB_SHOWCOMMAND];
  cbShowDbgFullAnnotation.Caption := Lang[ID_DEB_FULLANNOATION];

  rgbAutoOpen.Caption := Lang[ID_ENV_AUTOOPEN];
  rgbAutoOpen.Items[0] := Lang[ID_ENV_AUTOALL];
  rgbAutoOpen.Items[1] := Lang[ID_ENV_AUTOFIRST];
  rgbAutoOpen.Items[2] := Lang[ID_ENV_AUTOREMEMBER];
  rgbAutoOpen.Items[3] := Lang[ID_ENV_AUTONONE];

  cboTabsTop.Items[0] := Lang[ID_ENV_TOP];
  cboTabsTop.Items[1] := Lang[ID_ENV_BOTTOM];
  cboTabsTop.Items[2] := Lang[ID_ENV_LEFT];
  cboTabsTop.Items[3] := Lang[ID_ENV_RIGHT];

  lblLang.Caption := Lang[ID_ENV_LANGUAGE];
  lblmsgTabs.Caption := Lang[ID_ENV_MSGTABS];
  lblMRU.Caption := Lang[ID_ENV_MRU];

  lblMenuIconSize.Caption := Lang[ID_LANGFORM_MENU_ICON_SIZE];
  lblToolbarIconSize.Caption := Lang[ID_LANGFORM_TOOLBAR_ICON_SIZE];
  lblTabIconSize.Caption := Lang[ID_LANGFORM_TAB_ICON_SIZE];


  lblOptionsDir.Caption := Lang[ID_ENV_OPTIONSDIRHINT];
  btnResetDev.Caption := Lang[ID_ENV_RESETDEV];
  lblUserDir.Caption := Lang[ID_ENV_USERDIR];
  lblTemplatesDir.Caption := Lang[ID_ENV_TEMPLATESDIR];
  lblIcoLib.Caption := Lang[ID_ENV_ICOLIB];
  lblSplash.Caption := Lang[ID_ENV_SPLASH];
  lblLangPath.Caption := Lang[ID_ENV_SELLANGDIR];
  lblProjectsDir.Caption := Lang[ID_ENV_SELPROJDIR];

  // externals tab
  lblExternal.Caption := Lang[ID_ENV_EXTERNPROGASSOCS];
  vleExternal.TitleCaptions.Clear;
  vleExternal.TitleCaptions.Add(Lang[ID_ENV_EXTERNEXT]);
  vleExternal.TitleCaptions.Add(Lang[ID_ENV_EXTERNPROG]);

  btnExtAdd.Caption := Lang[ID_BTN_ADD];
  btnExtDel.Caption := Lang[ID_BTN_DELETE];
  btnHighDPIFixExit.Caption := Lang[ID_BTN_DPI_FIX_EXIT];

  // associations tab
  lblAssocFileTypes.Caption := Lang[ID_ENV_FASSTYPES];
  lblAssocDesc.Caption := Lang[ID_ENV_FASSDESC];
end;

procedure TEnviroForm.btnHelpClick(Sender: TObject);
begin
  OpenHelpFile('index.htm');
end;

procedure TEnviroForm.FormCreate(Sender: TObject);
var
  I, sel: integer;
begin
  LoadText;

  cbUIFont.ItemHeight := MainForm.Canvas.TextHeight('F');
  cbUIFontSize.ItemHeight := MainForm.Canvas.TextHeight('F');
  cbShowDbgCmd.Checked := devDebugger.ShowCommandLog;
  cbShowDbgFullAnnotation.Checked := devDebugger.ShowAnnotations;
  with devData do begin
    // General, left column
    cbDefCpp.Checked := defCpp;
    cbBackups.Checked := BackUps;
    cbMinOnRun.Checked := MinOnRun;
    cbShowBars.Checked := ShowBars;
    cbMultiLineTab.Checked := MultiLineTab;
    cbPauseConsole.Checked := ConsolePause;
    cbCheckAssocs.Checked := CheckAssocs;
    cbWatchHint.Checked := WatchHint;
    cbShowProgress.Checked := ShowProgress;
    cbAutoCloseProgress.Checked := AutoCloseProgress;

    // General, right column
    seMRUMax.Value := MRUMax;
    cboTabsTop.ItemIndex := Integer(msgTabs);

    // List the languages
    cboLang.Items.BeginUpdate;
    try
      cboLang.Clear;
      for I := 0 to Lang.Langs.Count - 1 do begin
        sel := cboLang.Items.Add(Lang.Langs.ValueFromIndex[I]);
        if SameText(Lang.CurrentLanguage, cboLang.Items[I]) then
          cboLang.ItemIndex := sel;
      end;
    finally
      cboLang.Items.EndUpdate;
    end;

    // List the themes

    cbMenuIconSize.ItemIndex := cbMenuIconSize.Items.IndexOf(devData.MenuIconSize);
    cbToolbarIconSize.ItemIndex := cbMenuIconSize.Items.IndexOf(devData.ToolbarIconSize);
    cbTabIconSize.ItemIndex := cbMenuIconSize.Items.IndexOf(devData.TabIconSize);

    // Add all font families and select the current one
    cbUIfont.Items.Assign(Screen.Fonts);
    for I := 0 to cbUIfont.Items.Count - 1 do
      if cbUIfont.Items.Strings[I] = InterfaceFont then begin
        cbUIfont.ItemIndex := I;
        break;
      end;

    // Do the same for the size selection
    for I := 0 to cbUIfontsize.Items.Count - 1 do
      if StrToIntDef(cbUIfontsize.Items.Strings[I], -1) = InterfaceFontSize then begin
        cbUIfontsize.ItemIndex := I;
        break;
      end;

    // General tab, right column, last entry
    rgbAutoOpen.ItemIndex := AutoOpen;

    // Directories tab
    edOptionsDir.Text := devDirs.Config;
    edUserDir.Text := devDirs.Default;
    edTemplatesDir.Text := ExtractRelativePath(devDirs.Exec, devDirs.Templates);
    edIcoLib.Text := ExtractRelativePath(devDirs.Exec, devDirs.Icons);
    edLang.Text := ExtractRelativePath(devDirs.Exec, devDirs.Lang);
    edSplash.Text := Splash;
    edProjectsDir.Text := devDirs.Projects;

    // External Programs tab
    vleExternal.Strings.Assign(devExternalPrograms.Programs);
    for I := 0 to vleExternal.Strings.Count - 1 do
      vleExternal.ItemProps[I].EditStyle := esEllipsis;

    // File associations tab
    CheckAssociations(false); // read only, don't try to fix them
    lstAssocFileTypes.Clear;
    for I := 0 to AssociationsCount - 1 do begin
      lstAssocFileTypes.Items.Add(Format('%s  (*.%s)', [Associations[I, 1], Associations[I, 0]]));
      lstAssocFileTypes.Checked[lstAssocFileTypes.Items.Count - 1] := IsAssociated(I);
    end;
  end;
end;

procedure TEnviroForm.vleExternalEditButtonClick(Sender: TObject);
begin
  if Trim(vleExternal.Cells[0, vleExternal.Row]) = '' then begin
    MessageDlg('Add an extension first!', mtError, [mbOk], 0);
    Exit;
  end;

  with TOpenDialog.Create(Self) do try
    Filter := FLT_ALLFILES;
    if Execute then
      vleExternal.Cells[1, vleExternal.Row] := Filename;
  finally
    Free;
  end;
end;

procedure TEnviroForm.vleExternalValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
var
  idx: integer;
begin
  if vleExternal.FindRow(KeyName, idx) and (idx <> ARow) then begin
    MessageDlg('Extension exists...', mtError, [mbOk], 0);
    vleExternal.Col := 0;
    vleExternal.Row := ARow;
    Abort;
  end;
  vleExternal.ItemProps[ARow - 1].EditStyle := esEllipsis;
end;

procedure TEnviroForm.btnExtAddClick(Sender: TObject);
begin
  vleExternal.InsertRow('', '', True);
  vleExternal.Row := vleExternal.RowCount - 1;
  vleExternal.Col := 0;
  vleExternal.SetFocus;
end;

procedure TEnviroForm.btnExtDelClick(Sender: TObject);
begin
  if (vleExternal.Row = 1) and (vleExternal.RowCount = 2) and (vleExternal.Cells[0, 1] = '') then
    exit;
  if (vleExternal.RowCount > 1) and (vleExternal.Row > 0) then
    vleExternal.DeleteRow(vleExternal.Row);
end;

procedure TEnviroForm.cbUIfontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control) do begin
    Canvas.Font.Name := Items.Strings[Index];
    Canvas.Font.Size := devData.InterfaceFontSize;
    Canvas.FillRect(Rect);
    Canvas.TextOut(Rect.Left, Rect.Top, Canvas.Font.Name);
  end;
end;

procedure TEnviroForm.cbUIfontsizeDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control) do begin
    Canvas.Font.Name := cbUIfont.Text;
    //Canvas.Font.Size := strtoint(Items.Strings[Index]);
    Canvas.Font.Size := devData.InterfaceFontSize;
    Canvas.FillRect(Rect);
    Canvas.TextOut(Rect.Left, Rect.Top, Items.Strings[Index]);
  end;
end;

procedure TEnviroForm.cbUIfontsizeChange(Sender: TObject);
begin
  cbUIfont.Repaint;
end;

procedure TEnviroForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TEnviroForm.btnResetDevClick(Sender: TObject);
begin
  if MessageDlg(Lang[ID_MSG_CLEARCONFIG], mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  // Remove settings on exit
  RemoveOptionsDir(devDirs.Config);

  // Quit without saving
  TerminateProcess(GetCurrentProcess, 0);
end;

procedure TEnviroForm.btnHighDPIFixExitClick(Sender: TObject);
var
  lReg: TRegistry;
begin
  // open registry, set root and key
  lReg := TRegistry.Create;
  try
    lReg.RootKey := HKEY_CURRENT_USER;
    lReg.OpenKey('\Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers', True);
    // write last Left, Top, Width and Height
    lReg.WriteString(Application.ExeName, '~ HIGHDPIAWARE');
    lReg.WriteString(ExtractFilePath(Application.ExeName)+'PackMaker.exe', '~ HIGHDPIAWARE');
    lReg.WriteString(ExtractFilePath(Application.ExeName)+'PackMan.exe', '~ HIGHDPIAWARE');

  // close all
  lReg.CloseKey;
  finally
    lReg.Free;
  end;
  MainForm.Close;
end;

procedure TEnviroForm.btnOpenOptionsDirClick(Sender: TObject);
var
  Folder: AnsiString;
begin
  Folder:=edOptionsDir.Text;
  if DirectoryExists(Folder) then
    ShellExecute(Application.Handle, 'open', 'explorer.exe', PAnsiChar(Folder), nil, SW_SHOWNORMAL);
end;

end.

