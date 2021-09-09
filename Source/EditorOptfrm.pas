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

unit EditorOptFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Spin,
  SynEdit, SynEditHighlighter, SynHighlighterCpp,
  Buttons, ClassBrowser, CppParser, CppTokenizer, StrUtils, Grids,
  CppPreprocessor, Utils;

type
  // Keep history of what we have accessed (does not mean changed)
  TEditorOptFormTabs = (taGeneral, taFonts, taColors, taSnippets, taCompletion, taAutosave);
  TEditorOptFormHistory = set of TEditorOptFormTabs;

  TEditorOptForm = class(TForm)
    PagesMain: TPageControl;
    tabDisplay: TTabSheet;
    grpGutter: TGroupBox;
    cbGutterVis: TCheckBox;
    cbGutterAuto: TCheckBox;
    cbLineNum: TCheckBox;
    cbFirstZero: TCheckBox;
    cbLeadZero: TCheckBox;
    cbGutterFnt: TCheckBox;
    lblGutterFont: TLabel;
    cboGutterFont: TComboBox;
    lblGutterWidth: TLabel;
    lblGutterFontSize: TLabel;
    edGutterSize: TSpinEdit;
    tabGeneral: TTabSheet;
    tabSyntax: TTabSheet;
    CppEdit: TSynEdit;
    ElementList: TListBox;
    grpStyle: TGroupBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderlined: TCheckBox;
    cpp: TSynCppSyn;
    grpEditorFont: TGroupBox;
    lblEditorSize: TLabel;
    lblEditorFont: TLabel;
    cboEditorFont: TComboBox;
    edEditorSize: TSpinEdit;
    grpMargin: TGroupBox;
    lblMarginWidth: TLabel;
    lblMarginColor: TLabel;
    cbMarginVis: TCheckBox;
    grpCaret: TGroupBox;
    lblInsertCaret: TLabel;
    lblOverCaret: TLabel;
    cboInsertCaret: TComboBox;
    cboOverwriteCaret: TComboBox;
    tabCode: TTabSheet;
    PagesSnippets: TPageControl;
    tabCPInserts: TTabSheet;
    tabCPDefault: TTabSheet;
    btnAdd: TButton;
    btnRemove: TButton;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    cboQuickColor: TComboBox;
    lblSpeed: TLabel;
    CodeIns: TSynEdit;
    btnSaveSyntax: TSpeedButton;
    tabCBCompletion: TTabSheet;
    lblCompletionDelay: TLabel;
    tbCompletionDelay: TTrackBar;
    cbMatch: TCheckBox;
    grpEditorOpts: TGroupBox;
    edMarginWidth: TSpinEdit;
    edGutterWidth: TSpinEdit;
    cbHighCurrLine: TCheckBox;
    cbSpecialChars: TCheckBox;
    cbSmartScroll: TCheckBox;
    cbScrollHint: TCheckBox;
    cbPastEOL: TCheckBox;
    cbPastEOF: TCheckBox;
    cbParserHints: TCheckBox;
    cbInsertMode: TCheckBox;
    cbHalfPage: TCheckBox;
    cbGroupUndo: TCheckBox;
    cbShowFoldOutline: TCheckBox;
    cbEHomeKey: TCheckBox;
    cbDropFiles: TCheckBox;
    cbAddIndent: TCheckBox;
    cbAutoIndent: TCheckBox;
    cbTrimTrailingSpaces: TCheckBox;
    ScrollHint: TLabel;
    tabAutosave: TTabSheet;
    cbAutoSave: TCheckBox;
    OptionsGroup: TGroupBox;
    SaveInterval: TLabel;
    MinutesDelay: TTrackBar;
    FileOptions: TRadioGroup;
    cbFunctionHint: TCheckBox;
    edSyntaxExt: TEdit;
    cbSyntaxHighlight: TCheckBox;
    grpTabs: TGroupBox;
    seTabSize: TSpinEdit;
    lblTabSize: TLabel;
    cbUseTabs: TCheckBox;
    cbSmartTabs: TCheckBox;
    cbDefaultCode: TCheckBox;
    seDefault: TSynEdit;
    NameOptions: TRadioGroup;
    lvCodeIns: TStringGrid;
    gbCBEngine: TGroupBox;
    chkCBParseGlobalH: TCheckBox;
    chkCBParseLocalH: TCheckBox;
    lblTimeStampExample: TLabel;
    cpMarginColor: TColorBox;
    cpForeground: TColorBox;
    cpBackground: TColorBox;
    PagesCompletion: TPageControl;
    tabSymbolCompletion: TTabSheet;
    grpSpecific: TGroupBox;
    cbParenth: TCheckBox;
    cbBraces: TCheckBox;
    cbComments: TCheckBox;
    cbArray: TCheckBox;
    cbSymbolComplete: TCheckBox;
    tabCodeCompletion: TTabSheet;
    cbDeleteCompleted: TCheckBox;
    cbSingleQuotes: TCheckBox;
    cbDoubleQuotes: TCheckBox;
    cbUseUTF8AsDefault: TCheckBox;
    cbUseAltSlash: TCheckBox;
    cbShowCompletionWhileInputing: TCheckBox;
    cbForeground: TCheckBox;
    cbBackground: TCheckBox;
    txtCodeSuggestionMaxCount: TSpinEdit;
    lbCodeSuggestionShowCount: TLabel;
    chkEnableCompletion: TCheckBox;
    cbGlobalIncludes: TCheckBox;
    chkRecordUsage: TCheckBox;
    btnClearUsageData: TButton;
    tabTabnine: TTabSheet;
    chkUseTabnine: TCheckBox;
    lblUseTabnine: TLabel;
    chkShowRainbowColor: TCheckBox;
    tabCheckSyntax: TTabSheet;
    chkAutoCheckSyntaxInBack: TCheckBox;
    grpDefaultFileType: TGroupBox;
    rbCFile: TRadioButton;
    rbCppFile: TRadioButton;
    chkCheckSyntaxReturn: TCheckBox;
    btnDownloadTabnine: TButton;
    chkShowKeywords: TCheckBox;
    chkIgnoreCase: TCheckBox;
    chkAppendFunc: TCheckBox;
    lbCodeSuggestionWidth: TLabel;
    txtCodeSuggestionWidth: TSpinEdit;
    lbCodeSuggestionHeight: TLabel;
    txtCodeSuggestionHeight: TSpinEdit;
    chkShowCodeIns: TCheckBox;
    chkSortByScope: TCheckBox;
    tabMisc: TTabSheet;
    chkLoadLastOpens: TCheckBox;
    GroupBox1: TGroupBox;
    lblIndentGuideColor: TLabel;
    cbShowIndentGuides: TCheckBox;
    cbIndentGuideColor: TColorBox;
    procedure FormCreate(Sender: TObject);
    procedure SetGutter;
    procedure ElementListClick(Sender: TObject);
    procedure cppEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure StyleChange(Sender: TObject);
    procedure cbLineNumClick(Sender: TObject);
    procedure cbSyntaxHighlightClick(Sender: TObject);
    procedure cbGutterFntClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure CodeInsStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure cboQuickColorSelect(Sender: TObject);
    procedure CppEditSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure tbCompletionDelayChange(Sender: TObject);
    procedure chkEnableCompletionClick(Sender: TObject);
    procedure btnSaveSyntaxClick(Sender: TObject);
    procedure OnGutterClick(Sender: TObject; Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
    procedure cbAutoSaveClick(Sender: TObject);
    procedure MinutesDelayChange(Sender: TObject);
    procedure cbSymbolCompleteClick(Sender: TObject);
    procedure cboEditorFontDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cboGutterFontDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure edEditorSizeChange(Sender: TObject);
    procedure edGutterSizeChange(Sender: TObject);
    procedure cboEditorFontChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvCodeInsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure PagesMainChange(Sender: TObject);
    procedure NameOptionsClick(Sender: TObject);
    procedure cbForegroundClick(Sender: TObject);
    procedure cbBackgroundClick(Sender: TObject);
    procedure chkRecordUsageClick(Sender: TObject);
    procedure btnClearUsageDataClick(Sender: TObject);
    procedure chkAutoCheckSyntaxInBackClick(Sender: TObject);
    procedure btnDownloadTabnineClick(Sender: TObject);
    procedure cbMarginVisClick(Sender: TObject);
    procedure cbShowIndentGuidesClick(Sender: TObject);
  private
    ffgColor: TColor;
    fbgColor: TColor;
    fGutColor: TThemeColor;
    fBPColor: TThemeColor;
    fErrColor: TThemeColor;
    fABPColor: TThemeColor;
    fSelColor: TThemeColor;
    fFoldColor: TThemeColor;
    fALColor : TThemeColor;
    fWNColor : TThemeColor;
    fPNLColor : TThemeColor;
    fPredefinedColorThemeCount: integer;
    procedure LoadFonts;
    procedure LoadText;
    procedure LoadCodeIns;
    procedure SaveCodeIns;
    procedure ClearCodeIns;
    procedure UpdateCIButtons;
    procedure LoadSyntax(const Name: AnsiString);
    procedure LoadPredefinedSyntax(const Name: AnsiString);
    procedure LoadSyntaxFromFile(const FileName: AnsiString);
    procedure FillSyntaxSets;
    procedure UpdateDemoEditColor;
    procedure CppEditPaintHighlightToken(Sender: TObject; Row: integer;
      column: integer; token: String; attr: TSynHighlighterAttributes;
      var style:TFontStyles; var FG,BG:TColor);
  public
    AccessedTabs: TEditorOptFormHistory;
  end;

implementation

uses
  shlobj, MultiLangSupport, devcfg, version, math, CommCtrl, DateUtils,cbUtils, CodeInsList, DataFrm, IniFiles, editor,
  main,shellAPI;

{$R *.dfm}
const
  cBreakLine = 9;
  cABreakLine = 11;
  cErrorLine = 13;
  cSelection = 17;
  cActiveLine = 4;

procedure TEditorOptForm.UpdateDemoEditColor;
begin
  CppEdit.Gutter.Color := fGutColor.Background;
  CppEdit.Gutter.Font.Color := fGutColor.Foreground;
  if cbHighCurrLine.Checked then
    CppEdit.ActiveLineColor := fALColor.Background
  else
    CppEdit.ActiveLineColor := clNone;

  CppEdit.CaretY := cActiveLine;
  CppEdit.CodeFolding.FolderBarLinesColor := fFoldColor.Foreground;
  CppEdit.UseCodeFolding := True;
end;

procedure TEditorOptForm.FormCreate(Sender: TObject);
var
  AttrName: AnsiString;
  Attribute: TSynHighlighterAttributes;
  I: integer;
begin
  LoadText;
  
  with devEditor do begin

    // Make editors look similar to main ones
    CppEdit.Font.Assign(Font);
    CodeIns.Font.Assign(devEditor.Font);
    seDefault.Font.Assign(devEditor.Font);

    // Gutters too
    CppEdit.Gutter.Font.Assign(Gutterfont);
    CodeIns.Gutter.Font.Assign(Gutterfont);
    seDefault.Gutter.Font.Assign(Gutterfont);

    cbUseUTF8AsDefault.Checked := UseUTF8ByDefault;
    // General
    cbGutterAuto.Checked := GutterAuto;
    cbGutterVis.Checked := GutterVis;
    edGutterWidth.Value := GutterSize;
    cbLineNum.Checked := LineNumbers;
    cbLeadZero.Checked := LeadZero;
    cbFirstZero.Checked := FirstLineZero;
    cbAutoIndent.Checked := AutoIndent;
    cbAddIndent.Checked := AddIndent;
    cbInsertMode.Checked := InsertMode;
    cbUseTabs.Checked := UseTabs;
    cbSmartTabs.Checked := SmartTabs;
    cbGroupUndo.Checked := GroupUndo;
    cbEHomeKey.Checked := EHomeKey;
    cbPastEOF.Checked := PastEOF;
    cbPastEOL.Checked := PastEOL;
    cbShowFoldOutline.Checked := ShowFoldOutline;
    cbShowIndentGuides.Checked := ShowIndentGuides;
    cbIndentGuideColor.Selected := IndentGuideColor;
    cbSmartScroll.Checked := Scrollbars;
    cbHalfPage.Checked := HalfPageScroll;
    cbScrollHint.Checked := ScrollHint;
    cbSpecialChars.Checked := SpecialChars;
    cbFunctionHint.Checked := ShowFunctionTip;
    cbTrimTrailingSpaces.Checked := TrimTrailingSpaces;
    chkShowRainbowColor.Checked := ShowRainbowBacket;
    cbMarginVis.Checked := MarginVis;
    edMarginWidth.Value := MarginSize;
    cpMarginColor.Selected := MarginColor;
    seTabSize.Value := TabSize;
    cbSyntaxHighlight.Checked := UseSyntax;
    edSyntaxExt.Text := SyntaxExt;
    cboInsertCaret.ItemIndex := InsertCaret;
    cboOverwriteCaret.ItemIndex := OverwriteCaret;
    cbDropFiles.Checked := InsDropFiles;
    cbParserHints.Checked := ParserHints;
    cbMatch.Checked := Match;
    cbDefaultCode.Checked := DefaultCode;
    cbHighCurrLine.Checked := HighCurrLine;

    edMarginWidth.Enabled := cbMarginVis.Checked;
    cpMarginColor.Enabled := cbMarginVis.Checked;
    cbIndentGuideColor.Enabled := cbShowIndentGuides.Checked;

    // Fonts
    LoadFonts; // fill dropdowns
    cboEditorFont.ItemIndex := cboEditorFont.Items.IndexOf(Font.Name);
    edEditorSize.Value := Font.Size;
    cbGutterFnt.Checked := Gutterfnt;
    cboGutterFont.ItemIndex := cboGutterFont.Items.IndexOf(Gutterfont.Name);
    edGutterSize.Value := GutterFont.Size;

    //misc
    chkLoadLastOpens.Checked := LoadLastOpens;

    // Colors
    FillSyntaxSets; // Load color themes
    StrToThemeColor(fSelColor, Syntax.Values[cSel]);
    StrToThemeColor(fGutColor, Syntax.Values[cGut]);
    StrToThemeColor(fbpColor, Syntax.Values[cBP]);
    StrToThemeColor(fErrColor, Syntax.Values[cErr]);
    StrToThemeColor(fABPColor, Syntax.Values[cABP]);
    StrToThemeColor(fFoldColor, Syntax.Values[cFld]);
    StrToThemeColor(fALColor, Syntax.Values[cAL]);
    StrToThemeColor(fWNColor, Syntax.Values[cWN]);
    StrToThemeColor(fPNLColor, Syntax.Values[cPNL]);
    UpdateDemoEditColor;

    if UseCpp then
      rbCppFile.Checked:=True
    else
      rbCFile.Checked:=True;
  end;

  // Colors, cont.
  ElementList.Items.BeginUpdate;
  try
    ElementList.Clear;
    for I := 0 to cpp.AttrCount - 1 do begin
      AttrName := cpp.Attribute[I].Name;

      if devEditor.Syntax.IndexOfName(AttrName) <> -1 then begin
        Attribute := TSynHighlighterAttributes.Create(AttrName);
        try
          StrtoAttr(Attribute, devEditor.Syntax.Values[AttrName]);
          cpp.Attribute[I].Assign(Attribute);
        finally
          Attribute.Free;
        end;
      end;

      // Add to list
      ElementList.Items.Add(cpp.Attribute[I].Name);
    end;

    // selection color
    ElementList.Items.Append(cSel);

    // gutter colors
    ElementList.Items.Append(cGut);

    // breakpoint
    ElementList.Items.Append(cBP);

    // error line
    ElementList.Items.Append(cErr);

    // active breakpoint
    ElementList.Items.Append(cABP);

    // folding color
    ElementList.Items.Append(cFld);

    // active line
    ElementList.Items.Append(cAL);

    // warnings
    ElementList.Items.Append(cWN);

    // Panel
    ElementList.Items.Append(cPNL);


    ffgColor := cpp.WhitespaceAttribute.Foreground;
    fbgColor := cpp.WhitespaceAttribute.Background;
  finally
    ElementList.Items.EndUpdate; // redraw once
  end;

  // Ensure UI is set correctly
  if ElementList.Items.Count > 0 then begin
    ElementList.ItemIndex := 0;
    ElementListClick(nil);
  end;

  CppEdit.OnPaintHighlightToken := CppEditPaintHighlightToken;
  // Snippets, Inserts
  LoadCodeIns;
  UpdateCIButtons;

  // Snippets, Default Insert
  if FileExists(devDirs.Config + DEV_DEFAULTCODE_FILE) then
    seDefault.Lines.LoadFromFile(devDirs.Config + DEV_DEFAULTCODE_FILE);

  // Code Completion
  chkEnableCompletion.Checked := devCodeCompletion.Enabled;
  chkCBParseLocalH.Checked := devCodeCompletion.ParseLocalHeaders;
  chkCBParseGlobalH.Checked := devCodeCompletion.ParseGlobalHeaders;
  tbCompletionDelay.Position := devCodeCompletion.Delay;
  chkEnableCompletionClick(nil);
  cbUseAltSlash.Checked := devCodeCompletion.UseAltSlash;
  cbShowCompletionWhileInputing.Checked := devCodeCompletion.ShowCompletionWhileInput;
  chkRecordUsage.Checked := devCodeCompletion.RecordUsage;
  chkSortByScope.Checked := devCodeCompletion.SortByScope;
  chkShowKeywords.Checked := devCodeCompletion.ShowKeywords;
  chkShowCodeIns.Checked := devCodeCompletion.ShowCodeIns;
  chkIgnoreCase.Checked := devCodeCompletion.IgnoreCase;
  chkAppendFunc.Checked := devCodeCompletion.AppendFunc;
  btnClearUsageData.Enabled := devCodeCompletion.RecordUsage;
  txtCodeSuggestionMaxCount.Value := devCodeCompletion.MaxCount;
  txtCodeSuggestionWidth.Value := devCodeCompletion.Width;
  txtCodeSuggestionHeight.Value := devCodeCompletion.Height;
  chkUseTabnine.Checked := devEditor.UseTabnine;
  
  // Symbol Completion
  with devEditor do begin
    cbSymbolComplete.Checked := CompleteSymbols;
    cbDeleteCompleted.Checked := DeleteSymbolPairs;
    cbArray.Checked := ArrayComplete;
    cbBraces.Checked := BraceComplete;
    cbComments.Checked := CommentComplete;
    //cbInclude.Checked := IncludeComplete;
    cbParenth.Checked := ParentheseComplete;
    cbSingleQuotes.Checked := SingleQuoteComplete;
    cbDoubleQuotes.Checked := DoubleQuoteComplete;
    cbGlobalIncludes.Checked := GlobalIncludeCompletion;

    // Completion. Only enable if CompleteSymbols is true
    cbSymbolCompleteClick(nil);

    // Autosave
    MinutesDelay.Position := devEditor.Interval;
    FileOptions.ItemIndex := devEditor.AutoSaveFilter;
    NameOptions.ItemIndex := devEditor.AutoSaveMode;
    cbAutoSave.Checked := devEditor.EnableAutoSave;
    cbAutoSaveClick(nil);

    chkAutoCheckSyntaxInBack.Checked := devEditor.AutoCheckSyntax;
    chkCheckSyntaxReturn.Enabled := chkAutoCheckSyntaxInBack.Checked;
    chkCheckSyntaxReturn.Checked := devEditor.CheckSyntaxWhenReturn;
  end;

  // Colors, cont. 2
  SetGutter;

  // Set defaults of color buttons, don't want all system colors too
  cpMarginColor.Items.InsertObject(1, 'Default', TObject(cpMarginColor.DefaultColorColor));
end;

procedure TEditorOptForm.cboEditorFontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
var
  alignleft: integer;
  aligntop: integer;
begin
  with TComboBox(Control) do begin
    Canvas.Font.Name := Items.Strings[Index];
    Canvas.Font.Size := edEditorSize.Value;
    Canvas.FillRect(Rect);
    alignleft := (Rect.Right - Rect.Left) div 2 - Canvas.TextWidth(Canvas.Font.Name) div 2;
    aligntop := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - Canvas.TextHeight(Canvas.Font.Name) div 2;
    Canvas.TextOut(alignleft, aligntop, Canvas.Font.Name);
  end;
end;

procedure TEditorOptForm.cboGutterFontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
var
  alignleft: integer;
  aligntop: integer;
begin
  with TComboBox(Control) do begin

    if cbGutterFnt.Checked then begin
      Canvas.Font.Name := Items.Strings[Index];
      Canvas.Font.Size := edGutterSize.Value;
    end else begin
      Canvas.Font.Name := cboEditorFont.Text;
      Canvas.Font.Size := edEditorSize.Value;
    end;
    Canvas.FillRect(Rect);
    alignleft := (Rect.Right - Rect.Left) div 2 - Canvas.TextWidth(Canvas.Font.Name) div 2;
    aligntop := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - Canvas.TextHeight(Canvas.Font.Name) div 2;
    Canvas.TextOut(alignleft, aligntop, Canvas.Font.Name);
  end;
end;

procedure TEditorOptForm.cboEditorFontChange(Sender: TObject);
begin
  if not cbGutterfnt.Checked then
    cboGutterFont.ItemIndex := cboEditorFont.ItemIndex;
  cboGutterFont.Repaint;
end;

procedure TEditorOptForm.edEditorSizeChange(Sender: TObject);
begin
  if not cbGutterfnt.Checked then
    edGutterSize.Value := edEditorSize.Value;
  cboEditorFont.Repaint;
  cboGutterFont.Repaint;
end;

procedure TEditorOptForm.edGutterSizeChange(Sender: TObject);
begin
  cboGutterFont.Repaint;
end;

procedure TEditorOptForm.cbGutterFntClick(Sender: TObject);
begin
  cboGutterFont.Enabled := cbGutterFnt.Checked;
  edGutterSize.Enabled := cbGutterfnt.Checked;
  if not cbGutterfnt.Checked then begin
    cboGutterFont.ItemIndex := cboEditorFont.ItemIndex;
    edGutterSize.Value := edEditorSize.Value;
  end;
end;

// Fill listboxes with available fonts

procedure TEditorOptForm.LoadFonts;
begin
  // Sum up all the available fonts
  cboEditorFont.Items.Assign(Screen.Fonts);
  cboGutterFont.Items := cboEditorFont.Items;

  cbLineNumClick(nil);
  cbGutterFntClick(nil);
end;

{ ---------- Form Init/Done Methods ----------}

procedure TEditorOptForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  lbCodeSuggestionShowCount.Caption := Lang[ID_EOPT_CODECOMPLETE_MAXCOUNT];
  lbCodeSuggestionWidth.Caption := Lang[ID_EOPT_CODECOMPLETE_WIDTH];
  lbCodeSuggestionHeight.Caption := Lang[ID_EOPT_CODECOMPLETE_HEIGHT];
  cbShowCompletionWhileInputing.Caption := Lang[ID_EOPT_CODECOMPLETE_WHILE_INPUT];
  chkRecordUsage.Caption := Lang[ID_EOPT_CODECOMPLETE_RECORD_USAGE];
  chkSortByScope.Caption := Lang[ID_EOPT_CODECOMPLETE_SORT_BY_SCOPE];
  chkShowKeywords.Caption := Lang[ID_EOPT_SHOW_KEYWORDS];
  chkShowCodeIns.Caption := Lang[ID_EOPT_SHOW_CODEINS];
  chkIgnoreCase.Caption := Lang[ID_EOPT_IGNORE_CASE];
  chkAppendFunc.Caption := Lang[ID_EOPT_APPEND_FUNC];
  btnClearUsageData.Caption := Lang[ID_EOPT_CODECOMPLETE_CLEAR_USAGE];
  cbUseAltSlash.Caption := Lang[ID_EOPT_ALTSLASH];
  cbUseUTF8AsDefault.Caption := Lang[ID_EOPT_UTF8];
  btnOk.Caption := Lang[ID_BTN_OK];
  btnCancel.Caption := Lang[ID_BTN_CANCEL];
  btnHelp.Caption := Lang[ID_BTN_HELP];

  // Top level tabs
  Caption := Lang[ID_EOPT];
  tabGeneral.Caption := Lang[ID_EOPT_GENTAB];
  tabDisplay.Caption := Lang[ID_EOPT_DISPLAYTAB];
  tabSyntax.Caption := Lang[ID_EOPT_SYNTAXTAB];
  tabCode.Caption := Lang[ID_EOPT_CODETAB];
  tabCBCompletion.Caption := Lang[ID_EOPT_COMPLETIONTAB];
  tabAutosave.Caption := Lang[ID_EOPT_AUTOSAVETAB];

  // Sub tabs (inserts)
  tabCPInserts.Caption := Lang[ID_EOPT_CPINSERTS];
  tabCPDefault.Caption := Lang[ID_EOPT_CPDEFAULT];

  // Sub tabs (completion)
  tabSymbolCompletion.Caption := Lang[ID_EOPT_CPSYMBOLS];
  tabCodeCompletion.Caption := Lang[ID_EOPT_CPCODES];

  // General Tab
  grpEditorOpts.Caption := Lang[ID_EOPT_EDOPTIONS];
  cbAutoIndent.Caption := Lang[ID_EOPT_AUTOINDENT2];
  cbInsertMode.Caption := Lang[ID_EOPT_INSERTMODE];

  cbGroupUndo.Caption := Lang[ID_EOPT_GROUPUNDO];
  cbDropFiles.Caption := Lang[ID_EOPT_DROPFILES];
  cbSpecialChars.Caption := Lang[ID_EOPT_SPECIALCHARS];
  cbTrimTrailingSpaces.Caption := Lang[ID_EOPT_TRIMTRAILINGSPACES];
  chkShowRainbowColor.Caption := Lang[ID_EOPT_SHOWRAINBOW_COLOR];
  cbEHomeKey.Caption := Lang[ID_EOPT_EHOMEKEY];
  cbPastEOF.Caption := Lang[ID_EOPT_PASTEOF];
  cbPastEOL.Caption := Lang[ID_EOPT_PASTEOL];
  cbAddIndent.Caption := Lang[ID_EOPT_ADDINDENT];
  cbShowFoldOutline.Caption := Lang[ID_EOPT_FOLD_OUTLINE];
  cbShowIndentGuides.Caption := Lang[ID_EOPT_SHOW_INDENT_GUIDES];
  lblIndentGuideColor.Caption := Lang[ID_EOPT_COLOR];
  cbSmartScroll.Caption := Lang[ID_EOPT_SMARTSCROLL];
  cbHalfPage.Caption := Lang[ID_EOPT_HALFPAGE];
  cbScrollHint.Caption := Lang[ID_EOPT_SCROLLHINT];
  cbParserHints.Caption := Lang[ID_EOPT_PARSERHINTS];
  cbFunctionHint.Caption := Lang[ID_EOPT_CLOSEBRACE];
  ScrollHint.Caption := Lang[ID_EOPT_CTRLSCROLLHINT];
  cbSyntaxHighlight.Caption := Lang[ID_EOPT_USESYNTAX];

  grpMargin.Caption := Lang[ID_EOPT_MARGIN];
  cbMarginVis.Caption := Lang[ID_EOPT_GENERICENABLED];
  lblMarginWidth.Caption := Lang[ID_EOPT_WIDTH];
  lblMarginColor.Caption := Lang[ID_EOPT_COLOR];

  grpCaret.Caption := Lang[ID_EOPT_CARET];
  lblInsertCaret.Caption := Lang[ID_EOPT_INSCARET];
  lblOverCaret.Caption := Lang[ID_EOPT_OVERCARET];
  cbMatch.Caption := Lang[ID_EOPT_MATCH];

  grpTabs.Caption := Lang[ID_EOPT_TABS];
  lblTabSize.Caption := Lang[ID_EOPT_TABSIZE];
  cbUseTabs.Caption := Lang[ID_EOPT_TAB2SPC];
  cbSmartTabs.Caption := Lang[ID_EOPT_SMARTTABS];

  cbHighCurrLine.Caption := Lang[ID_EOPT_HIGHCURLINE];

  cboInsertCaret.Clear;
  cboInsertCaret.Items.Add(Lang[ID_EOPT_CARET1]);
  cboInsertCaret.Items.Add(Lang[ID_EOPT_CARET2]);
  cboInsertCaret.Items.Add(Lang[ID_EOPT_CARET3]);
  cboInsertCaret.Items.Add(Lang[ID_EOPT_CARET4]);

  cboOverwriteCaret.Clear;
  cboOverwriteCaret.Items.Add(Lang[ID_EOPT_CARET1]);
  cboOverwriteCaret.Items.Add(Lang[ID_EOPT_CARET2]);
  cboOverwriteCaret.Items.Add(Lang[ID_EOPT_CARET3]);
  cboOverwriteCaret.Items.Add(Lang[ID_EOPT_CARET4]);

  grpDefaultFileType.Caption := Lang[ID_EOPT_FILE_TYPE];
  rbCppFile.Caption := Lang[ID_EOPT_CPP_FILE];
  rbCFile.Caption := Lang[ID_EOPT_C_FILE];
  // Fonts Tab
  grpEditorFont.Caption := Lang[ID_EOPT_EDFONT];
  lblEditorFont.Caption := Lang[ID_EOPT_FONT];
  lblEditorSize.Caption := Lang[ID_EOPT_SIZE];

  grpGutter.Caption := Lang[ID_EOPT_GUTTER];
  cbGutterVis.Caption := Lang[ID_EOPT_VISIBLE];
  cbGutterAuto.Caption := Lang[ID_EOPT_GUTTERAUTO];
  cbLineNum.Caption := Lang[ID_EOPT_LINENUM];
  cbLeadZero.Caption := Lang[ID_EOPT_LEADZERO];
  cbFirstZero.Caption := Lang[ID_EOPT_FIRSTZERO];
  cbGutterFnt.Caption := Lang[ID_EOPT_GUTTERFNT];
  lblGutterWidth.Caption := Lang[ID_EOPT_GUTTERWIDTH];
  lblGutterFont.Caption := Lang[ID_EOPT_FONT];
  lblGutterFontSize.Caption := Lang[ID_EOPT_SIZE];

  // Colors tab
  cbForeground.Caption := Lang[ID_EOPT_FORE];
  cbBackground.Caption := Lang[ID_EOPT_BACK];
  grpStyle.Caption := Lang[ID_EOPT_STYLE];
  cbBold.Caption := Lang[ID_EOPT_BOLD];
  cbItalic.Caption := Lang[ID_EOPT_ITALIC];
  cbUnderlined.Caption := Lang[ID_EOPT_UNDERLINE];
  lblSpeed.Caption := Lang[ID_EOPT_SPEED];
  btnSaveSyntax.Hint := Lang[ID_EOPT_SAVESYNTAX];

  // Snippets tab
  btnAdd.Caption := Lang[ID_BTN_ADD];
  btnRemove.Caption := Lang[ID_BTN_REMOVE];
  lvCodeIns.Cols[0][0] := Lang[ID_EOPT_CIMENU];
  lvCodeIns.Cols[1][0] := Lang[ID_EOPT_CIPREFIX];
  lvCodeIns.Cols[2][0] := Lang[ID_EOPT_CISECTION];
  lvCodeIns.Cols[3][0] := Lang[ID_EOPT_CIDESC];
  cbDefaultCode.Caption := Lang[ID_EOPT_DEFCODE];

  // Completion tab, code
  chkEnableCompletion.Caption := Lang[ID_EOPT_COMPLETIONENABLE];
  gbCBEngine.Caption := Lang[ID_EOPT_BROWSERENGINE];
  chkCBParseLocalH.Caption := Lang[ID_EOPT_BROWSERLOCAL];
  chkCBParseGlobalH.Caption := Lang[ID_EOPT_BROWSERGLOBAL];
  chkUseTabnine.Caption := Lang[ID_EOPT_USETABNINE];
  lblUseTabnine.Caption := Lang[ID_EOPT_USETABNINE_NOTE];
  self.btnDownloadTabnine.Caption := Lang[ID_EOPT_DOWNLOAD_TABNINE];

  // Completion tab, symbol
  cbSymbolComplete.Caption := Lang[ID_EOPT_SYMBOLCOMPLETE];
  grpSpecific.Caption := Lang[ID_EOPT_SYMBOLGROUP];
  cbBraces.Caption := Lang[ID_EOPT_SYMBOLBRACES];
  cbParenth.Caption := Lang[ID_EOPT_SYMBOLPARENT];
  cbArray.Caption := Lang[ID_EOPT_SYMBOLSQUARE];
  cbComments.Caption := Lang[ID_EOPT_SYMBOLCOMMENT];
  cbSingleQuotes.Caption := Lang[ID_EOPT_SYMBOLSINGLEQUOTE];
  cbDoubleQuotes.Caption := Lang[ID_EOPT_SYMBOLDOUBLEQUOTE];
  cbGlobalIncludes.Caption := Lang[ID_EOPT_SYMBOLINCLUDE];
  cbDeleteCompleted.Caption := Lang[ID_EOPT_DELETESYMBOLPAIRS];

  // Autosave
  cbAutoSave.Caption := Lang[ID_EOPT_ENABLEAUTOSAVE];
  OptionsGroup.Caption := Lang[ID_EOPT_OPTIONS];

  FileOptions.Caption := Lang[ID_EOPT_AUTOSAVEFILE];
  FileOptions.Items[0] := Lang[ID_EOPT_AUTOSAVEONLYOPENFILE];
  FileOptions.Items[1] := Lang[ID_EOPT_AUTOSAVEALLFILES];
  FileOptions.Items[2] := Lang[ID_EOPT_AUTOSAVEPROJECT];

  NameOptions.Caption := Lang[ID_EOPT_AUTOSAVEMODE];
  NameOptions.Items[0] := Lang[ID_EOPT_AUTOSAVEOVERWRITE];
  NameOptions.Items[1] := Lang[ID_EOPT_AUTOSAVEUNIX];
  NameOptions.Items[2] := Lang[ID_EOPT_AUTOSAVETIME];

  //Syntab Check
  tabCheckSyntax.Caption :=  Lang[ID_EOPT_SYNTAXCHECK];
  chkAutoCheckSyntaxInBack.Caption := LANG[ID_EOPT_SYNTAXCHECK_IN_BACK];
  chkCheckSyntaxReturn.Caption := LANG[ID_EOPT_SYNTAXCHECK_WHEN_RETURN];

  tbCompletionDelayChange(nil);
  MinutesDelayChange(nil);
  NameOptionsClick(nil);

  //misc tab
  tabMisc.Caption := LANG[ID_EOPT_MISC];
  chkLoadLastOpens.Caption := LANG[ID_EOPT_LOAD_LAST_OPENS];
end;

procedure TEditorOptForm.btnOkClick(Sender: TObject);
var
  s, aName: AnsiString;
  a, idx: integer;
begin
  with devEditor do begin
    UseUTF8ByDefault :=  cbUseUTF8AsDefault.Checked;
    AutoIndent := cbAutoIndent.Checked;
    AddIndent := cbAddIndent.Checked;
    InsertMode := cbInsertMode.Checked;
    UseTabs := cbUseTabs.Checked;
    SmartTabs := cbSmartTabs.Checked;
    GroupUndo := cbGroupUndo.Checked;
    EHomeKey := cbEHomeKey.Checked;
    PastEOF := cbPastEOF.Checked;
    PastEOL := cbPastEOL.Checked;
    ShowFoldOutline := cbShowFoldOutline.Checked;
    ShowIndentGuides := cbShowIndentGuides.Checked;
    IndentGuideColor := cbIndentGuideColor.Selected;

    Scrollbars := cbSmartScroll.Checked;
    HalfPageScroll := cbHalfPage.Checked;
    ScrollHint := cbScrollHint.Checked;
    SpecialChars := cbSpecialChars.Checked;
    ShowFunctionTip := cbFunctionHint.Checked;
    TrimTrailingSpaces := cbTrimTrailingSpaces.Checked;
    ShowRainbowBacket := chkShowRainbowColor.Checked;
    MarginVis := cbMarginVis.Checked;
    MarginSize := edMarginWidth.Value;
    MarginColor := cpMarginColor.Selected;
    InsertCaret := cboInsertCaret.ItemIndex;
    OverwriteCaret := cboOverwriteCaret.ItemIndex;
    Match := cbMatch.Checked;

    HighCurrLine := cbHighCurrLine.Checked;

    UseSyntax := cbSyntaxHighlight.Checked;
    SyntaxExt := edSyntaxExt.Text;
    TabSize := seTabSize.Value;

    Font.Name := cboEditorFont.Text;
    Font.Size := edEditorSize.Value;

    Gutterfont.Name := cboGutterFont.Text;
    GutterFont.Size := edGutterSize.Value;

    Gutterfnt := cbGutterFnt.Checked;
    GutterAuto := cbGutterAuto.Checked;
    GutterVis := cbGutterVis.Checked;
    GutterSize := edGutterWidth.Value;
    LineNumbers := cbLineNum.Checked;
    LeadZero := cbLeadZero.Checked;
    FirstLineZero := cbFirstZero.Checked;
    InsDropFiles := cbDropFiles.Checked;

    ParserHints := cbParserHints.Checked;

    // Completion
    CompleteSymbols := cbSymbolComplete.Checked;
    ArrayComplete := cbArray.Checked;
    BraceComplete := cbBraces.Checked;
    CommentComplete := cbComments.Checked;
    //IncludeComplete := cbInclude.Checked;
    ParentheseComplete := cbParenth.Checked;
    SingleQuoteComplete := cbSingleQuotes.Checked;
    DoubleQuoteComplete := cbDoubleQuotes.Checked;
    GlobalIncludeCompletion := cbGlobalIncludes.Checked;
    DeleteSymbolPairs := cbDeleteCompleted.Checked;

    UseTabnine := chkUseTabnine.Checked;

    // Autosave
    EnableAutoSave := cbAutoSave.Checked;
    Interval := MinutesDelay.Position;
    AutoSaveFilter := FileOptions.ItemIndex;
    AutoSaveMode := NameOptions.ItemIndex;

    AutoCheckSyntax := chkAutoCheckSyntaxInBack.Checked;
    CheckSyntaxWhenReturn := chkCheckSyntaxReturn.Checked;

    //misc
    LoadLastOpens := chkLoadLastOpens.Checked;


    // Default source
    DefaultCode := cbDefaultCode.Checked;

    // load in attributes
    for idx := 0 to pred(cpp.AttrCount) do begin
      aName := cpp.Attribute[idx].Name;
      a := Syntax.IndexOfName(aName);
      if a = -1 then
        Syntax.Append(format('%s=%s', [aName, AttrtoStr(cpp.Attribute[idx])]))
      else
        Syntax.Values[aName] := AttrtoStr(cpp.Attribute[idx]);
    end;

    // selected text
    s := ThemeColortoStr(fSelColor);
    a := Syntax.IndexofName(cSel);
    if a = -1 then
      Syntax.Append(format('%s=%s', [cSel, s]))
    else
      Syntax.Values[cSel] := s;

    // gutter
    s := ThemeColortoStr(fGutColor);
    a := Syntax.IndexofName(cGut);
    if a = -1 then
      Syntax.Append(format('%s=%s', [cGut, s]))
    else
      Syntax.Values[cGut] := s;

    // breakpoints
    s := ThemeColortoStr(fbpColor);
    a := Syntax.IndexofName(cBP);
    if a = -1 then
      Syntax.Append(format('%s=%s', [cBP, s]))
    else
      Syntax.Values[cBP] := s;

    // error line
    s := ThemeColortoStr(fErrColor);
    a := Syntax.IndexofName(cErr);
    if a = -1 then
      Syntax.Append(format('%s=%s', [cErr, s]))
    else
      Syntax.Values[cErr] := s;

    // active breakpoint
    s := ThemeColortoStr(fAbpColor);
    a := Syntax.IndexofName(cABP);
    if a = -1 then
      Syntax.Append(format('%s=%s', [cABP, s]))
    else
      Syntax.Values[cABP] := s;

    // fold bar
    s := ThemeColortoStr(fFoldColor);
    a := Syntax.IndexofName(cFld);
    if a = -1 then
      Syntax.Append(format('%s=%s', [cFld, s]))
    else
      Syntax.Values[cFld] := s;

    // active line
    s := ThemeColortoStr(fALColor);
    a := Syntax.IndexofName(cAL);
    if a = -1 then
      Syntax.Append(format('%s=%s', [cAL, s]))
    else
      Syntax.Values[cAL] := s;

    s := ThemeColortoStr(fWNColor);
    a := Syntax.IndexofName(cWN);
    if a = -1 then
      Syntax.Append(format('%s=%s', [cWN, s]))
    else
      Syntax.Values[cWN] := s;

    s := ThemeColortoStr(fPNLColor);
    a := Syntax.IndexofName(cPNL);
    if a = -1 then
      Syntax.Append(format('%s=%s', [cPNL, s]))
    else
      Syntax.Values[cPNL] := s;


    UseCpp := rbCppFile.Checked;
  end;


  // Save our code snippet even if we opted not to use it (user may want to keep it)
  if not seDefault.IsEmpty then
    seDefault.Lines.SavetoFile(devDirs.Config + DEV_DEFAULTCODE_FILE)
  else
    DeleteFile(devDirs.Config + DEV_DEFAULTCODE_FILE);

  SaveCodeIns;

  with devCodeCompletion do begin
    Enabled := chkEnableCompletion.Checked;
    Delay := tbCompletionDelay.Position;
    ParseLocalHeaders := chkCBParseLocalH.Checked;
    ParseGlobalHeaders := chkCBParseGlobalH.Checked;
    UseAltSlash := cbUseAltSlash.Checked;
    RecordUsage := chkRecordUsage.Checked;
    SortByScope := chkSortByScope.Checked;
    ShowKeywords := chkShowKeywords.Checked;
    ShowCodeIns := chkShowCodeIns.Checked;
    IgnoreCase := chkIgnoreCase.Checked;
    AppendFunc := chkAppendFunc.Checked;
    ShowCompletionWhileInput := cbShowCompletionWhileInputing.Checked;
    MaxCount := txtCodeSuggestionMaxCount.Value;
    width := txtCodeSuggestionWidth.Value;
    height := txtCodeSuggestionHeight.Value;
  end;

  // Only create the timer if autosaving is enabled
  if devEditor.EnableAutoSave then begin
    if not Assigned(MainForm.AutoSaveTimer) then
      MainForm.AutoSaveTimer := TTimer.Create(nil);
    MainForm.AutoSaveTimer.Interval := devEditor.Interval * 60 * 1000; // miliseconds to minutes
    MainForm.AutoSaveTimer.Enabled := devEditor.EnableAutoSave;
    MainForm.AutoSaveTimer.OnTimer := MainForm.EditorSaveTimer;
  end else begin
    MainForm.AutoSaveTimer.Free;
    MainForm.AutoSaveTimer := nil;
  end;

  SaveOptions;
  dmMain.LoadDataMod;
end;

procedure TEditorOptForm.btnHelpClick(Sender: TObject);
begin
  OpenHelpFile('index.htm');
end;

procedure TEditorOptForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{ ---------- Syntax Style Methods ---------- }

procedure TEditorOptForm.SetGutter;
begin
  // update preview
  cppedit.Gutter.Color := fgutColor.Background;
  cppedit.Gutter.Font.Color := fgutColor.Foreground;
  cppedit.CodeFolding.FolderBarLinesColor := fFoldColor.Foreground;

  // update snippet edit
  CodeIns.Gutter.Color := fgutColor.Background;
  CodeIns.Gutter.Font.Color := fgutColor.Foreground;
  CodeIns.CodeFolding.FolderBarLinesColor := fFoldColor.Foreground;

  // update default source edit
  seDefault.Gutter.Color := fgutColor.Background;
  seDefault.Gutter.Font.Color := fgutColor.Foreground;
  seDefault.CodeFolding.FolderBarLinesColor := fFoldColor.Foreground;
end;

procedure TEditorOptForm.ElementListClick(Sender: TObject);
var
  tc: TThemeColor;
  procedure SetColor(fg,bg:TColor);
  begin
    cpBackground.Selected:=clWhite;
    if bg = clNone then begin
      cbBackground.Checked := False;
      cpBackground.Selected := clNone;
    end else begin
      cbBackground.Checked := True;
      cpBackground.Selected := bg;
    end;
    cpForeground.Selected:=clWhite;
    if fg = clNone then begin
      cbForeground.Checked := False;
      cpForeground.Selected := clNone;
    end else begin
      cbForeground.Checked := True;
      cpForeground.Selected := fg;
    end;

  end;

  procedure setColorSettingsEnabled(enabled:boolean);
  begin
    if not enabled then begin
      cbForeground.OnClick:=nil;
      cbBackground.OnClick:=nil;
      cpForeground.OnChange := nil;
      cpBackground.OnChange := nil;
      cbBold.OnClick:=nil;
      cbItalic.OnClick := nil;
      cbUnderlined.OnClick := nil
    end else begin
      cbForeground.OnClick:=styleChange;
      cbBackground.OnClick:=styleChange;
      cpForeground.OnChange := styleChange;
      cpBackground.OnChange := styleChange;
      cbBold.OnClick:=styleChange;
      cbItalic.OnClick := styleChange;
      cbUnderlined.OnClick := styleChange;
    end;
  end;
begin
  setColorSettingsEnabled(false);
  cbBackground.Enabled := True;
  cbForeground.Enabled := True;

  // Special additions not directly exposed by TSynHighlighter
  if ElementList.ItemIndex > pred(cpp.AttrCount) then begin

    // Select proper color for special items. Disable background for fold colors?
    // TODO: support specific colors for folds
    if SameText(ElementList.Items[ElementList.ItemIndex], cSel) then begin
      tc := fSelColor;
    end else if SameText(ElementList.Items[ElementList.ItemIndex], cBP) then begin
      tc := fBPColor;
    end else if SameText(ElementList.Items[ElementList.ItemIndex], cErr) then begin
      tc := fErrColor;
    end else if SameText(ElementList.Items[ElementList.ItemIndex], cABP) then begin
      tc := fABPColor;
    end else if SameText(ElementList.Items[ElementList.ItemIndex], cGut) then begin
      tc := fGutColor;
    end else if SameText(ElementList.Items[ElementList.ItemIndex], cFld) then begin
      tc := fFoldColor;
      cbBackground.Enabled := False;      
    end else if SameText(ElementList.Items[ElementList.ItemIndex], cAL) then begin
      tc := fALColor;
      cbForeground.Enabled := False;      
    end else if SameText(ElementList.Items[ElementList.ItemIndex], cWN) then begin
      tc := fWNColor;
      cbBackground.Enabled := False;
    end else if SameText(ElementList.Items[ElementList.ItemIndex], cPNL) then begin
      tc := fPNLColor;
    end;

    SetColor(tc.Foreground,tc.Background);

    cbUnderlined.Checked := False;

    cbBold.Enabled := False;
    cbItalic.Enabled := False;
    cbUnderlined.Enabled := False;

    // regular SynEdit attributes
  end else if ElementList.ItemIndex <> -1 then begin
    with Cpp.Attribute[ElementList.ItemIndex] do begin

    SetColor(Foreground,Background);

    {
      if Foreground = clNone then begin
        cpForeground.Selected := ffgcolor //clNone
      end else begin
        cpForeground.Selected := Foreground;
      end
      if Background = clNone then begin
        cpBackground.Selected := fbgcolor //clNone
      end else begin
        cpBackground.Selected := Background;
      end
      cpBackground.Enabled := True;
    }


      cbBold.Enabled := True;
      cbItalic.Enabled := True;
      cbUnderlined.Enabled := True;

      cbBold.Checked := fsBold in Style;
      cbItalic.Checked := fsItalic in Style;
      cbUnderlined.Checked := fsUnderline in Style;
    end;
  end;
  setColorSettingsEnabled(true);
end;

procedure TEditorOptForm.StyleChange(Sender: TObject);
var
  attr: TSynHighlighterAttributes;
  tc: TThemeColor;
  s: AnsiString;
  fg,bg:TColor;
  procedure ReadColor(var fg:TColor;var bg:TColor);
  begin
    if cbBackground.Checked then begin
      bg:=cpBackground.Selected;
    end else begin
      bg:=clNone;
    end;
    if cbForeground.Checked then begin
      fg:=cpForeground.Selected;
    end else begin
      fg:=clNone;
    end;
  end;
begin
  if ElementList.ItemIndex < 0 then
    exit;

  // Special additions not directly exposed by TSynHighlighter
  if ElementList.ItemIndex > pred(cpp.AttrCount) then begin
    ReadColor(tc.Foreground,tc.Background);

    // use local AnsiString just to ease readability
    s := ElementList.Items[ElementList.ItemIndex];

    if SameText(s, cSel) then
      fSelColor := tc
    else if SameText(s, cBP) then
      fBPColor := tc
    else if SameText(s, cABP) then
      fABPColor := tc
    else if SameText(s, cerr) then
      fErrColor := tc
    else if SameText(s, cGut) then begin
      fGutColor := tc;
      SetGutter;
    end else if SameText(s, cFld) then begin
      fFoldColor := tc;
      SetGutter;
    end else if SameText(s, cAL) then begin
      fALColor := tc;
    end else if SameText(s, cWN) then begin
      fWNColor := tc;
    end else if SameText(s, cPNL) then begin
      fPNLColor := tc;
    end;
    UpdateDemoEditColor;

    // regular SynEdit attributes
  end else begin
    Attr := TSynHighlighterAttributes.Create(ElementList.Items[ElementList.ItemIndex]);
    Attr.Assign(cpp.Attribute[ElementList.ItemIndex]);
    with Attr do try
      ReadColor(fg,bg);
      Foreground := fg;
      Background := bg;

      // Update default color
      if SameText(Name, 'WhiteSpace') then begin
        ffgColor := Foreground;
        fbgColor := Background;
      end;

      Style := [];
      if cbBold.checked then
        Style := Style + [fsBold];
      if cbItalic.Checked then
        Style := Style + [fsItalic];
      if cbUnderlined.Checked then
        Style := Style + [fsUnderline];

      cpp.Attribute[ElementList.ItemIndex].Assign(Attr);
    finally
      Free;
    end;
  end;

  cppEdit.Repaint;
end;

procedure TEditorOptForm.CppEditPaintHighlightToken(Sender: TObject; Row: integer;
  column: integer; token: String; attr: TSynHighlighterAttributes;
  var style:TFontStyles; var FG,BG:TColor);
var
  tc:TThemeColor;
  kind: TStatementKind;
  p: TBufferCoord;
  s:String;
  pBeginPos,pEndPos : TbufferCoord;
begin
  if token='' then
    Exit;
  //selection
  if CppEdit.SelAvail then begin
    if (
      (attr = CppEdit.Highlighter.IdentifierAttribute)
      or (attr = CppEdit.Highlighter.KeywordAttribute)
      or (attr = Cpp.DirecAttri)
      )
      and SameStr(token, CppEdit.SelText) then begin
      tc := self.fSelColor;
      FG := tc.Foreground;
      BG := tc.Background;
      exit;
    end;
  end;

  if SameStr(token,'int') then
    fg := Cpp.KeyAttri.Foreground
  else if SameStr(token,'x') then
    fg := Cpp.GlobalVarAttri.Foreground
  else if SameStr(token,'main') then
    fg := Cpp.FunctionAttri.Foreground
  else if SameStr(token,'argc') then
    fg := Cpp.LocalVarAttri.Foreground
  else if SameStr(token,'argv') then
    fg := Cpp.LocalVarAttri.Foreground
  else if SameStr(token,'char') then
    fg := Cpp.KeyAttri.Foreground
  else if SameStr(token,'numbers') then
    fg := Cpp.LocalVarAttri.Foreground
  else if SameStr(token,'float') then
    fg := Cpp.KeyAttri.Foreground
  else if SameStr(token,'average') then
    fg := Cpp.LocalVarAttri.Foreground
  else if SameStr(token,'total') then
    fg := Cpp.LocalVarAttri.Foreground
  else if SameStr(token,'i') then
    fg := Cpp.LocalVarAttri.Foreground
  else if SameStr(token,'for') then
    fg := Cpp.KeyAttri.Foreground
  else if SameStr(token,'cout') then
    fg := Cpp.VariableAttri.Foreground
  else if SameStr(token,'getch') then
    fg := Cpp.FunctionAttri.Foreground;

  if fg = clNone then //old color theme, use the default color
    fg := attr.Foreground;
end;

procedure TEditorOptForm.cppEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  Token: AnsiString;
  attr: TSynHighlighterAttributes;
begin
  if assigned(cppEdit.Highlighter) and
    (Changes * [scAll, scCaretX, scCaretY] <> []) then begin
    if not cppEdit.GetHighlighterAttriAtRowCol(cppEdit.CaretXY, Token, Attr) then
      Attr := cppEdit.Highlighter.WhiteSpaceAttribute;
    if assigned(Attr) then begin
      if Attr = cpp.IdentifierAttri then begin
        if SameStr(token,'int') then
          Attr := Cpp.KeyAttri
        else if SameStr(token,'x') then
          Attr := Cpp.GlobalVarAttri
        else if SameStr(token,'main') then
          Attr := Cpp.FunctionAttri
        else if SameStr(token,'argc') then
          Attr := Cpp.LocalVarAttri
        else if SameStr(token,'argv') then
          Attr := Cpp.LocalVarAttri
        else if SameStr(token,'char') then
          Attr := Cpp.KeyAttri
        else if SameStr(token,'numbers') then
          Attr := Cpp.LocalVarAttri
        else if SameStr(token,'float') then
          Attr := Cpp.KeyAttri
        else if SameStr(token,'average') then
          Attr := Cpp.LocalVarAttri
        else if SameStr(token,'total') then
          Attr := Cpp.LocalVarAttri
        else if SameStr(token,'i') then
          Attr := Cpp.LocalVarAttri
        else if SameStr(token,'for') then
          Attr := Cpp.KeyAttri
        else if SameStr(token,'cout') then
          Attr := Cpp.VariableAttri
        else if SameStr(token,'getch') then
          Attr := Cpp.FunctionAttri
      end;
      ElementList.ItemIndex := ElementList.Items.Indexof(Attr.Name);
      ElementListClick(Self);
    end;
    if Attr = cppEdit.Highlighter.WhiteSpaceAttribute then begin
      case cppEdit.CaretY of
      cSelection: begin
          ElementList.ItemIndex := ElementList.Items.Indexof(cSel);
          ElementListClick(Self);
        end;
      cActiveLine: begin
          ElementList.ItemIndex := ElementList.Items.Indexof(cAL);
          ElementListClick(Self);
        end;
      cBreakLine: begin
          ElementList.ItemIndex := ElementList.Items.Indexof(cBP);
          ElementListClick(Self);
        end;
      cABreakLine: begin
          ElementList.ItemIndex := ElementList.Items.Indexof(cABP);
          ElementListClick(Self);
        end;
      cErrorLine: begin
          ElementList.ItemIndex := ElementList.Items.Indexof(cErr);
          ElementListClick(Self);
        end;
      end;
    end;
  end;
end;

procedure TEditorOptForm.CppEditSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  case Line of
    cSelection: begin
        if fSelColor.Background <> clNone then
          BG := fSelColor.Background;
        if fSelColor.Foreground <> clNone then
          FG := fSelColor.Foreground;
        Special := TRUE;
      end;
      {
    cActiveLine: begin
        if fALColor.Background <> clNone then
          BG := fALColor.background;
        Special := TRUE;
      end;
      }
    cBreakLine: begin
        if fBPColor.Background <> clNone then
          BG := fBPColor.Background;
        if fBPColor.Foreground <> clNone then
          FG := fBPColor.Foreground;
        if (fBPColor.Background <> clNone) and (fBPColor.Foreground <> clNone) then
          Special := TRUE;
      end;
    cABreakLine: begin
        if fABPColor.Background <> clNone then
          BG := fABPColor.Background;
        if fABPColor.Foreground <> clNone then
          FG := fABPColor.Foreground;
        if (fABPColor.Background <> clNone) and (fABPColor.Foreground <> clNone) then
          Special := TRUE;
      end;
    cErrorLine: begin
        if fErrColor.Background <> clNone then
          BG := fErrColor.Background;
        if fErrColor.Foreground <> clNone then
          FG := fErrColor.Foreground;
        Special := TRUE;
      end;
  end;
end;

procedure TEditorOptForm.cbLineNumClick(Sender: TObject);
begin
  cbLeadZero.Enabled := cbLineNum.Checked;
  cbFirstZero.Enabled := cbLineNum.Checked;
end;

procedure TEditorOptForm.cbSyntaxHighlightClick(Sender: TObject);
begin
  edSyntaxExt.Enabled := cbSyntaxHighlight.Checked;
end;

procedure TEditorOptForm.cboQuickColorSelect(Sender: TObject);
var
  offset: integer;
  i: integer;
  attr: TSynHighlighterAttributes;
begin
  if cboQuickColor.ItemIndex >= fPredefinedColorThemeCount then begin 
    // custom style; load from disk
    LoadSyntax(cboQuickColor.Items[cboQuickColor.ItemIndex]);
  end else begin
    LoadPredefinedSyntax(cboQuickColor.Items[cboQuickColor.ItemIndex]);
  end;

  SetGutter;
  cppEdit.Repaint;
  ElementListClick(nil);
end;

{ ---------- Code insert methods ---------- }

procedure TEditorOptForm.btnAddClick(Sender: TObject);
begin
  CodeIns.ClearAll; // clear example editor

  lvCodeIns.RowCount := lvCodeIns.RowCount + 1; // add blank row, assume fixedrows remains at 1

  // Fill
  lvCodeIns.Objects[0, lvCodeIns.RowCount - 1] := TStringList.Create;
  lvCodeIns.Cells[0, lvCodeIns.RowCount - 1] := '';
  lvCodeIns.Cells[1, lvCodeIns.RowCount - 1] := '';
  lvCodeIns.Cells[2, lvCodeIns.RowCount - 1] := '';

  lvCodeIns.Row := lvCodeIns.RowCount - 1; // set selection
  lvCodeIns.Col := 0; // set selection
  lvCodeIns.SetFocus;

  UpdateCIButtons;
end;

procedure TEditorOptForm.btnRemoveClick(Sender: TObject);
var
  I: integer;
  sl: TStringList;
begin
  if (lvCodeIns.Row >= lvCodeIns.FixedRows) then begin
    if (lvCodeIns.RowCount > 2) then begin // remove completely
      dmMain.CodeInserts.Delete(lvCodeIns.Row);

      // Delete object containing text too
      TStringList(lvCodeIns.Objects[0, lvCodeIns.Row]).Free;
      lvCodeIns.Objects[0, lvCodeIns.Row] := nil;

      for I := lvCodeIns.Row to lvCodeins.RowCount - 2 do
        lvCodeIns.Rows[i].Assign(lvCodeIns.Rows[i + 1]); // moves objects too

      lvCodeIns.RowCount := lvCodeIns.RowCount - 1;
    end else begin // leave blank row
      sl := TStringList(lvCodeIns.Objects[0, lvCodeIns.Row]); // leave blank data
      sl.Clear;
      lvCodeIns.Rows[lvCodeIns.RowCount - 1].Text := ''; // removes data pointer
      lvCodeIns.Objects[0, lvCodeIns.Row] := sl;
    end;

    lvCodeIns.Repaint;
    CodeIns.ClearAll;
    UpdateCIButtons;
  end;
end;

procedure TEditorOptForm.UpdateCIButtons;
begin
  btnAdd.Enabled := true;
  btnRemove.Enabled := lvCodeIns.Row > 0;
end;

procedure TEditorOptForm.lvCodeInsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  CodeIns.ClearAll;
  if (lvCodeIns.Row >= lvCodeIns.FixedRows) then begin
    CodeIns.Text := StrToCodeIns(TStringList(lvCodeIns.Objects[0, ARow]).Text); // store code in first column object
    UpdateCIButtons;
  end;
end;

procedure TEditorOptForm.CodeInsStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if (lvCodeIns.Row >= lvCodeIns.FixedRows) then begin
    if (scModified in Changes) then begin
      TStringList(lvCodeIns.Objects[0, lvCodeIns.Row]).Text := CodeInstoStr(CodeIns.Text); // store text in hidden field
      CodeIns.Modified := false;
    end;
  end;
end;

procedure TEditorOptForm.LoadCodeIns;
var
  ins: PCodeIns;
  I: integer;
  sl: TStringList;
  canselect: boolean;
begin
  lvCodeIns.FixedRows := 0;
  lvCodeIns.RowCount := 1; // re-add fixed row later on

  for I := 0 to dmMain.CodeInserts.Count - 1 do begin
    lvCodeIns.RowCount := lvCodeIns.RowCount + 1; // add new blank row

    // Don't forget to delete!
    sl := TStringList.Create;

    // Fill cols
    ins := dmMain.CodeInserts[I];
    sl.Text := ins^.Code;
    lvCodeIns.Objects[0, lvCodeIns.RowCount - 1] := sl;
    lvCodeIns.Cells[0, lvCodeIns.RowCount - 1] := ins^.Caption;
    lvCodeIns.Cells[1, lvCodeIns.RowCount - 1] := ins^.Prefix;
    lvCodeIns.Cells[2, lvCodeIns.RowCount - 1] := IntToStr(ins^.Section);
    lvCodeIns.Cells[3, lvCodeIns.RowCount - 1] := ins^.Desc;
  end;

  // Add empty, but configured row
  if lvCodeIns.RowCount = 1 then begin
    lvCodeIns.RowCount := lvCodeIns.RowCount + 1;

    // Fill
    lvCodeIns.Objects[0, lvCodeIns.RowCount - 1] := TStringList.Create;
    lvCodeIns.Cells[0, lvCodeIns.RowCount - 1] := '';
    lvCodeIns.Cells[1, lvCodeIns.RowCount - 1] := '';
    lvCodeIns.Cells[2, lvCodeIns.RowCount - 1] := '';
    lvCodeIns.Cells[3, lvCodeIns.RowCount - 1] := '';
  end;

  lvCodeIns.FixedRows := 1; // gets reset to 0 when removing all editable rows

  lvCodeInsSelectCell(nil, 0, 1, canselect);
end;

procedure TEditorOptForm.SaveCodeIns;
var
  I: integer;
  Item: PCodeIns;
begin
  dmMain.CodeInserts.Clear;
  for I := lvCodeIns.FixedRows to lvCodeIns.RowCount - 1 do begin
    if lvCodeIns.Cells[0, I] <> '' then begin
      Item := new(PCodeIns);

      // Get snippet from attached object
      Item.Caption := lvCodeIns.Cells[0, I];
      Item.Prefix := lvCodeIns.Cells[1, I];
      Item.Section := StrToIntDef(lvCodeIns.Cells[2, I], 0);
      Item.Desc := lvCodeIns.Cells[3, I];
      Item.Code := TStringList(lvCodeIns.Objects[0, I]).Text;

      dmMain.CodeInserts.AddItem(Item);
    end;
  end;
  dmMain.CodeInserts.SaveCode;
end;

procedure TEditorOptForm.ClearCodeIns;
var
  I: integer;
begin
  // Clear the code insertion string grid objects
  for I := 1 to lvCodeIns.RowCount - 1 do begin
    if Assigned(lvCodeIns.Objects[0, I]) then begin
      TStringList(lvCodeIns.Objects[0, I]).Free;
      lvCodeIns.Objects[0, I] := nil;
    end;
  end;
end;

{ ---------- Code completion ---------- }

procedure TEditorOptForm.tbCompletionDelayChange(Sender: TObject);
begin
  lblCompletionDelay.Caption := Lang[ID_EOPT_COMPLETIONDELAY] + ' ' + IntToStr(tbCompletionDelay.Position) + ' ms';
end;

procedure TEditorOptForm.chkEnableCompletionClick(Sender: TObject);
begin
  with chkEnableCompletion do begin
    tbCompletionDelay.Enabled := Checked;
    chkCBParseGlobalH.Enabled := Checked;
    chkCBParseLocalH.Enabled := Checked;
    cbUseAltSlash.Enabled := Checked;
    cbShowCompletionWhileInputing.Enabled:=Checked;
    chkRecordUsage.Enabled := Checked;
    chkSortByScope.Enabled := Checked;
    chkShowKeywords.Enabled := Checked;
    chkShowCodeIns.Enabled := Checked;
    chkIgnoreCase.Enabled := Checked;
    chkAppendFunc.Enabled := Checked;
    txtCodeSuggestionMaxCount.Enabled:=Checked;
    txtCodeSuggestionWidth.Enabled:=Checked;
    txtCodeSuggestionHeight.Enabled:=Checked;   
  end;
end;

procedure TEditorOptForm.btnSaveSyntaxClick(Sender: TObject);
var
  idx: integer;
  fINI: TIniFile;
  S: AnsiString;
  tc: TThemeColor;
begin
  s := 'New syntax';
  if not ShowInputQuery(Lang[ID_EOPT_SAVESYNTAX], Lang[ID_EOPT_SAVESYNTAXQUESTION], s) or (s = '') then
    Exit;

  fINI := TIniFile.Create(devDirs.Config + s + SYNTAX_EXT);
  try
    for idx := 0 to pred(Cpp.AttrCount) do
      fINI.WriteString('Editor.Custom', Cpp.Attribute[idx].Name, AttrtoStr(Cpp.Attribute[idx]));

    for idx := Cpp.AttrCount to pred(ElementList.Items.Count) do begin
      if CompareText(ElementList.Items[idx], cSel) = 0 then
        tc := fSelColor
      else if CompareText(ElementList.Items[idx], cBP) = 0 then
        tc := fBPColor
      else if CompareText(ElementList.Items[idx], cErr) = 0 then
        tc := fErrColor
      else if CompareText(ElementList.Items[idx], cABP) = 0 then
        tc := fABPColor
      else if CompareText(ElementList.Items[idx], cGut) = 0 then
        tc := fGutColor
      else if CompareText(ElementList.Items[idx], cFld) = 0 then
        tc := fFoldColor
      else if CompareText(ElementList.Items[idx], cAL) = 0 then
        tc := fALColor
      else if CompareText(ElementList.Items[idx], cWN) = 0 then
        tc := fWNColor
      else if CompareText(ElementList.Items[idx], cPNL) = 0 then
        tc := fPNLColor;
      fINI.WriteString('Editor.Custom', ElementList.Items[idx], ThemeColortoStr(tc));
    end;
  finally
    fINI.Free;
  end;
  if cboQuickColor.Items.IndexOf(S) = -1 then
    cboQuickColor.Items.Add(S);
  cboQuickColor.ItemIndex := cboQuickColor.Items.IndexOf(S);
end;

procedure TEditorOptForm.LoadSyntax(const Name: AnsiString);
begin
  LoadSyntaxFromFile(devDirs.Config + Name + SYNTAX_EXT);
end;

procedure TEditorOptForm.LoadPredefinedSyntax(const Name: AnsiString);
begin
  LoadSyntaxFromFile(devDirs.Exec + 'Contributes\syntax\'+Name+SYNTAX_EXT);
end;


procedure TEditorOptForm.LoadSyntaxFromFile(const FileName: AnsiString);
var
  idx: integer;
  fINI: TIniFile;
  Attr: TSynHighlighterAttributes;
  tc: TThemeColor;
  attrStr: String;
begin
  fINI := TIniFile.Create(FileName);
  try
    for idx := 0 to pred(Cpp.AttrCount) do begin
      Attr := TSynHighlighterAttributes.Create(Cpp.Attribute[idx].Name);
      try
        attrStr:= fINI.ReadString('Editor.Custom', Cpp.Attribute[idx].Name,
          devEditor.Syntax.Values[Cpp.Attribute[idx].Name]);
        StrToAttr(Attr, attrStr);
        Cpp.Attribute[idx].Assign(Attr);
      finally
        Attr.Free;
      end;
    end;

    for idx := Cpp.AttrCount to pred(ElementList.Items.Count) do begin
      StrToThemeColor(tc, fINI.ReadString('Editor.Custom', ElementList.Items[idx], ''));
      if CompareText(ElementList.Items[idx], cSel) = 0 then
        fSelColor := tc
      else if CompareText(ElementList.Items[idx], cBP) = 0 then
        fBPColor := tc
      else if CompareText(ElementList.Items[idx], cErr) = 0 then
        fErrColor := tc
      else if CompareText(ElementList.Items[idx], cABP) = 0 then
        fABPColor := tc
      else if CompareText(ElementList.Items[idx], cGut) = 0 then begin
        fGutColor := tc;
        SetGutter;
      end else if CompareText(ElementList.Items[idx], cFld) = 0 then begin
        fFoldColor := tc;
        SetGutter;
      end else if CompareText(ElementList.Items[idx], cAL) = 0 then begin
        fALColor := tc;
      end else if CompareText(ElementList.Items[idx], cWN) = 0 then begin
        fWNColor := tc;
      end else if CompareText(ElementList.Items[idx], cPNL) = 0 then begin
        fPNLColor := tc;
      end;
    end;
    UpdateDemoEditColor;
  finally
    fINI.Free;
  end;
  ElementListClick(nil);
end;

procedure TEditorOptForm.FillSyntaxSets;
var
  SR: TSearchRec;
begin
  if FindFirst(devDirs.Exec + 'Contributes\syntax\' + '*' + SYNTAX_EXT, faAnyFile, SR) = 0 then
    repeat
      cboQuickColor.Items.Add(StringReplace(SR.Name, SYNTAX_EXT, '', [rfIgnoreCase]));
    until FindNext(SR) <> 0;
  fPredefinedColorThemeCount := cboQuickColor.Items.Count;
  if FindFirst(devDirs.Config + '*' + SYNTAX_EXT, faAnyFile, SR) = 0 then
    repeat
      cboQuickColor.Items.Add(StringReplace(SR.Name, SYNTAX_EXT, '', [rfIgnoreCase]));
    until FindNext(SR) <> 0;
end;

procedure TEditorOptForm.OnGutterClick(Sender: TObject; Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
var
  idx: integer;
begin
  idx := ElementList.Items.IndexOf(cGut);
  if idx <> -1 then begin
    ElementList.ItemIndex := idx;
    ElementListClick(Self);
  end;
end;

procedure TEditorOptForm.cbAutoSaveClick(Sender: TObject);
begin
  MinutesDelay.Enabled := cbAutoSave.Checked;
  SaveInterval.Enabled := cbAutoSave.Checked;
  FileOptions.Enabled := cbAutoSave.Checked;
  OptionsGroup.Enabled := cbAutoSave.Checked;
  NameOptions.Enabled := cbAutoSave.Checked;
  lblTimeStampExample.Enabled := cbAutoSave.Checked;
end;

procedure TEditorOptForm.MinutesDelayChange(Sender: TObject);
begin
  if MinutesDelay.Position = 1 then
    SaveInterval.Caption := Lang[ID_EOPT_AUTOSAVEINTERNAL] + ' ' + IntToStr(MinutesDelay.Position) + ' minute'
  else
    SaveInterval.Caption := Lang[ID_EOPT_AUTOSAVEINTERNAL] + ' ' + IntToStr(MinutesDelay.Position) + ' minutes';
end;

procedure TEditorOptForm.cbSymbolCompleteClick(Sender: TObject);
begin
  cbArray.Enabled := cbSymbolComplete.Checked;
  cbBraces.Enabled := cbSymbolComplete.Checked;
  cbComments.Enabled := cbSymbolComplete.Checked;
  cbParenth.Enabled := cbSymbolComplete.Checked;
  cbSingleQuotes.Enabled := cbSymbolComplete.Checked;
  cbGlobalIncludes.Enabled := cbSymbolComplete.Checked;
  cbDoubleQuotes.Enabled := cbSymbolComplete.Checked;
  cbDeleteCompleted.Enabled := cbSymbolComplete.Checked;
end;

procedure TEditorOptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ClearCodeIns;
  // Our caller needs the form, so let it call Free instead
  action := caHide;
end;

procedure TEditorOptForm.PagesMainChange(Sender: TObject);
begin
  case PagesMain.ActivePageIndex of
    0:
      AccessedTabs := AccessedTabs + [taGeneral];
    1:
      AccessedTabs := AccessedTabs + [taFonts];
    2:
      AccessedTabs := AccessedTabs + [taColors];
    3:
      AccessedTabs := AccessedTabs + [taSnippets];
    4:
      AccessedTabs := AccessedTabs + [taCompletion];
    5:
      AccessedTabs := AccessedTabs + [taAutosave];
  end;
end;

procedure TEditorOptForm.NameOptionsClick(Sender: TObject);
begin
  case NameOptions.ItemIndex of
    0: begin
        lblTimeStampExample.Caption := Format(Lang[ID_EOPT_AUTOSAVEEXAMPLE],
          ['main.cpp']);
      end;
    1: begin
        lblTimeStampExample.Caption := Format(Lang[ID_EOPT_AUTOSAVEEXAMPLE],
          [ChangeFileExt('main.cpp', '.' + IntToStr(DateTimeToUnix(Now)) + ExtractFileExt('main.cpp'))]);
      end;
    2: begin
        lblTimeStampExample.Caption := Format(Lang[ID_EOPT_AUTOSAVEEXAMPLE],
          [ChangeFileExt('main.cpp', '.' + FormatDateTime('yyyy mm dd hh mm ss', Now) + ExtractFileExt('main.cpp'))]);
      end;
  end;
end;


procedure TEditorOptForm.cbForegroundClick(Sender: TObject);
begin
  cpForeground.Enabled := cbForeground.Checked;
  self.StyleChange(Sender);
end;

procedure TEditorOptForm.cbBackgroundClick(Sender: TObject);
begin
  cpBackground.Enabled := cbBackground.Checked;
  self.StyleChange(Sender);
end;

procedure TEditorOptForm.chkRecordUsageClick(Sender: TObject);
begin
  btnClearUsageData.Enabled := chkRecordUsage.Checked;
end;

procedure TEditorOptForm.btnClearUsageDataClick(Sender: TObject);
begin
  dmMain.SymbolUsage.Clear;
end;

procedure TEditorOptForm.chkAutoCheckSyntaxInBackClick(Sender: TObject);
begin
  chkCheckSyntaxReturn.Enabled:=chkAutoCheckSyntaxInBack.Checked;
end;

procedure TEditorOptForm.btnDownloadTabnineClick(Sender: TObject);
var
  s:string;
begin
  s:=TABNINE_SITE;
  ShellExecute(GetDesktopWindow(), 'open', PAnsiChar(s), nil, nil, SW_SHOWNORMAL);
end;

procedure TEditorOptForm.cbMarginVisClick(Sender: TObject);
begin
  edMarginWidth.Enabled := cbMarginVis.Checked;
  cpMarginColor.Enabled := cbMarginVis.Checked;
end;

procedure TEditorOptForm.cbShowIndentGuidesClick(Sender: TObject);
begin
  cbIndentGuideColor.Enabled := self.cbShowIndentGuides.Checked;
end;

end.

