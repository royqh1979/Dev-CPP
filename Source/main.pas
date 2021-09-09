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

{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$APPTYPE GUI}
unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs,Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ComCtrls, ToolWin, ExtCtrls, Buttons, utils, SynEditPrint,
  Project, editor, DateUtils, compiler, ActnList, ToolFrm, AppEvnts,
  debugger, ClassBrowser, CodeCompletion, CppParser, SyncObjs,
  StrUtils, SynEditTypes, devFileMonitor, devMonitorTypes, DdeMan, EditorList,
  devShortcuts, debugreader, ExceptionFrm, CommCtrl, devcfg, SynEditTextBuffer,
  CBUtils, StatementList, FormatterOptionsFrm, RenameFrm, Refactorer, devConsole,
  Tabnine,devCaretList, devFindOutput, HeaderCompletion, VirtualTrees,
  devFileBrowser;

type
  TRunEndAction = (reaNone, reaProfile);
  TCompSuccessAction = (csaNone, csaRun, csaDebug, csaProfile);

  TTabsheet = class( ComCtrls.TTabSheet )
  private
    Procedure WMEraseBkGnd( var msg: TWMEraseBkGnd );
      message WM_ERASEBKGND;
  end;

  TPageControl = class( ComCtrls.TPageControl )
  private
    Procedure WMEraseBkGnd( var msg: TWMEraseBkGnd );
      message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMLDblClick(var msg:TWMRButtonDblClk); message WM_LBUTTONDBLCLK;
  protected
    procedure CreateParams(var Params: TCreateParams);override;  
  public
    constructor Create(AOwner:TComponent); override;
  end;

  TPanel = class( ExtCtrls.TPanel )
  private
    Procedure WMEraseBkGnd( var msg: TWMEraseBkGnd );
      message WM_ERASEBKGND;
  end;

  TListView = class(ComCtrls.TListView)
  private
    FHeaderHandle: HWND;
    procedure WMNotify(var AMessage: TWMNotify); message WM_NOTIFY;
  protected
    procedure CreateWnd; override;
  end;

  TMenuItemHint = class(THintWindow)
  private
    activeMenuItem : TMenuItem;
    showTimer : TTimer;
    hideTimer : TTimer;
    procedure HideTime(Sender : TObject) ;
    procedure ShowTime(Sender : TObject) ;
  public
    constructor Create(AOwner : TComponent) ; override;
    procedure DoActivateHint(menuItem : TMenuItem) ;
    destructor Destroy; override;
  end;

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    NewprojectItem: TMenuItem;
    NewTemplateItem: TMenuItem;
    N34: TMenuItem;
    ClearhistoryItem: TMenuItem;
    N11: TMenuItem;
    NewSourceFileItem: TMenuItem;
    SaveUnitItem: TMenuItem;
    SaveUnitAsItem: TMenuItem;
    SaveallItem: TMenuItem;
    N33: TMenuItem;
    CloseprojectItem: TMenuItem;
    CloseItem: TMenuItem;
    ExportItem: TMenuItem;
    HTMLItem: TMenuItem;
    RTFItem: TMenuItem;
    N19: TMenuItem;
    ProjecttoHTMLItem: TMenuItem;
    PrintItem: TMenuItem;
    PrinterSetupItem: TMenuItem;
    N3: TMenuItem;
    ExitItem: TMenuItem;
    EditMenu: TMenuItem;
    UndoItem: TMenuItem;
    RedoItem: TMenuItem;
    N4: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    SelectallItem: TMenuItem;
    SearchMenu: TMenuItem;
    FindItem: TMenuItem;
    ReplaceItem: TMenuItem;
    N7: TMenuItem;
    GotolineItem: TMenuItem;
    ViewMenu: TMenuItem;
    ProjectManagerItem: TMenuItem;
    StatusbarItem: TMenuItem;
    ToolbarsItem: TMenuItem;
    ToolMainItem: TMenuItem;
    ToolCompileandRunItem: TMenuItem;
    ToolProjectItem: TMenuItem;
    ToolSpecialsItem: TMenuItem;
    ProjectMenu: TMenuItem;
    NewunitinprojectItem: TMenuItem;
    AddtoprojectItem: TMenuItem;
    RemovefromprojectItem: TMenuItem;
    N6: TMenuItem;
    ProjectoptionsItem: TMenuItem;
    ExecuteMenu: TMenuItem;
    CompileItem: TMenuItem;
    RunItem: TMenuItem;
    CompileandRunItem: TMenuItem;
    RebuildallItem: TMenuItem;
    N8: TMenuItem;
    CompileroptionsItem: TMenuItem;
    EnvironmentoptionsItem: TMenuItem;
    ToolsMenu: TMenuItem;
    ConfiguretoolsItem: TMenuItem;
    WindowMenu: TMenuItem;
    CloseAllItem: TMenuItem;
    N28: TMenuItem;
    FullscreenmodeItem: TMenuItem;
    N36: TMenuItem;
    NextItem: TMenuItem;
    PreviousItem: TMenuItem;
    N32: TMenuItem;
    HelpMenu: TMenuItem;
    AboutDevCppItem: TMenuItem;
    CompSheet: TTabSheet;
    ResSheet: TTabSheet;
    ResourceOutput: TListView;
    LogSheet: TTabSheet;
    ToolbarDock: TControlBar;
    tbMain: TToolBar;
    OpenBtn: TToolButton;
    tbCompile: TToolBar;
    NewFileBtn: TToolButton;
    SaveBtn: TToolButton;
    CompileBtn: TToolButton;
    RunBtn: TToolButton;
    CompileAndRunBtn: TToolButton;
    RebuildAllBtn: TToolButton;
    tbProject: TToolBar;
    AddToProjectBtn: TToolButton;
    RemoveFromProjectBtn: TToolButton;
    ProjectOptionsBtn: TToolButton;
    CloseSheet: TTabSheet;
    SaveAllBtn: TToolButton;
    SplitterLeft: TSplitter;
    EditorPopup: TPopupMenu;
    MenuItem2: TMenuItem;
    UnitPopup: TPopupMenu;
    RemoveFilefromprojectPopItem: TMenuItem;
    RenamefilePopItem: TMenuItem;
    N30: TMenuItem;
    ClosefilePopItem: TMenuItem;
    ProjectPopup: TPopupMenu;
    NewunitinprojectPopItem: TMenuItem;
    AddtoprojectPopItem: TMenuItem;
    RemovefromprojectPopItem: TMenuItem;
    MenuItem18: TMenuItem;
    ProjectoptionsPopItem: TMenuItem;
    InfoGroupBox: TPanel;
    Statusbar: TStatusbar;
    FindSheet: TTabSheet;
    FindinallfilesItem: TMenuItem;
    N20: TMenuItem;
    mnuNew: TMenuItem;
    N13: TMenuItem;
    ActionList: TActionList;
    actNewSource: TAction;
    actNewProject: TAction;
    actNewTemplate: TAction;
    actOpen: TAction;
    actHistoryClear: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    actSaveAll: TAction;
    actClose: TAction;
    actCloseAll: TAction;
    actCloseProject: TAction;
    actExportHTML: TAction;
    actExportRTF: TAction;
    actExportProject: TAction;
    actPrint: TAction;
    actPrintSU: TAction;
    actExit: TAction;
    actUndo: TAction;
    actRedo: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actSelectAll: TAction;
    actFind: TAction;
    actFindAll: TAction;
    actReplace: TAction;
    actGotoLine: TAction;
    actProjectManager: TAction;
    actStatusbar: TAction;
    actProjectNew: TAction;
    actProjectAdd: TAction;
    actProjectRemove: TAction;
    actProjectOptions: TAction;
    actCompile: TAction;
    actRun: TAction;
    actCompRun: TAction;
    actRebuild: TAction;
    actClean: TAction;
    actDebug: TAction;
    actCompOptions: TAction;
    actEnviroOptions: TAction;
    actEditorOptions: TAction;
    actConfigTools: TAction;
    actFullScreen: TAction;
    actNext: TAction;
    actPrev: TAction;
    actAbout: TAction;
    actProjectSource: TAction;
    actUnitRemove: TAction;
    actUnitRename: TAction;
    actUnitHeader: TAction;
    actUnitOpen: TAction;
    actUnitClose: TAction;
    EditorOptionsItem: TMenuItem;
    tbEdit: TToolBar;
    BackBtn: TToolButton;
    ForwardBtn: TToolButton;
    tbSearch: TToolBar;
    FindBtn: TToolButton;
    ReplaceBtn: TToolButton;
    FindNextBtn: TToolButton;
    GotoLineBtn: TToolButton;
    OpenPopItem: TMenuItem;
    ToolEditItem: TMenuItem;
    ToolSearchItem: TMenuItem;
    N2: TMenuItem;
    N9: TMenuItem;
    tbSpecials: TToolBar;
    actProjectMakeFile: TAction;
    MessagePopup: TPopupMenu;
    MsgCopyItem: TMenuItem;
    MsgCopyAllItem: TMenuItem;
    MsgSaveAllItem: TMenuItem;
    MsgClearItem: TMenuItem;
    actBreakPoint: TAction;
    actIncremental: TAction;
    IncrementalSearch1: TMenuItem;
    actShowBars: TAction;
    EditorPageControlLeft: TPageControl;
    Close1: TMenuItem;
    N16: TMenuItem;
    actAddWatch: TAction;
    actEditWatch: TAction;
    actStepOver: TAction;
    actContinue: TAction;
    actWatchItem: TAction;
    actRemoveWatch: TAction;
    actStopExecute: TAction;
    InsertBtn: TToolButton;
    ToggleBtn: TToolButton;
    GotoBtn: TToolButton;
    CodeCompletion: TCodeCompletion;
    Swapheadersource1: TMenuItem;
    N23: TMenuItem;
    actSwapHeaderSource: TAction;
    actSyntaxCheck: TAction;
    Shortcuts: TdevShortcuts;
    actConfigdevShortcuts: TAction;
    ConfiguredevShortcuts1: TMenuItem;
    N25: TMenuItem;
    Programreset1: TMenuItem;
    actComment: TAction;
    actUncomment: TAction;
    actIndent: TAction;
    actUnindent: TAction;
    Indent1: TMenuItem;
    Unindent1: TMenuItem;
    N27: TMenuItem;
    actGotoFunction: TAction;
    Gotofunction1: TMenuItem;
    BrowserPopup: TPopupMenu;
    mnuBrowserGotoDecl: TMenuItem;
    mnuBrowserGotoImpl: TMenuItem;
    mnuBrowserSep1: TMenuItem;
    mnuBrowserNewClass: TMenuItem;
    mnuBrowserNewMember: TMenuItem;
    mnuBrowserNewVariable: TMenuItem;
    actBrowserGotoDeclaration: TAction;
    actBrowserGotoDefinition: TAction;
    actBrowserNewClass: TAction;
    actBrowserNewMember: TAction;
    actBrowserNewVar: TAction;
    actProfile: TAction;
    Profileanalysis1: TMenuItem;
    N24: TMenuItem;
    actBrowserAddFolder: TAction;
    actBrowserRemoveFolder: TAction;
    actBrowserRenameFolder: TAction;
    actCloseAllButThis: TAction;
    CloseAll1: TMenuItem;
    Closeallexceptthis1: TMenuItem;
    CloseAll2: TMenuItem;
    actStepInto: TAction;
    DebugPopup: TPopupMenu;
    AddwatchPop: TMenuItem;
    RemoveWatchPop: TMenuItem;
    FileMonitor: TdevFileMonitor;
    actFileProperties: TAction;
    N35: TMenuItem;
    N1: TMenuItem;
    Properties1: TMenuItem;
    actViewToDoList: TAction;
    actAddToDo: TAction;
    oDolist1: TMenuItem;
    N39: TMenuItem;
    actProjectNewFolder: TAction;
    actProjectRemoveFolder: TAction;
    actProjectRenameFolder: TAction;
    Newfolder1: TMenuItem;
    N40: TMenuItem;
    actImportMSVC: TAction;
    ImportItem: TMenuItem;
    actViewCPU: TAction;
    actExecParams: TAction;
    mnuExecParameters: TMenuItem;
    DevCppDDEServer: TDdeServerConv;
    actShowTips: TAction;
    ShowTipsItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    N43: TMenuItem;
    PackageManagerItem: TMenuItem;
    btnAbortCompilation: TSpeedButton;
    actAbortCompilation: TAction;
    N48: TMenuItem;
    ListItem: TMenuItem;
    mnuFileProps: TMenuItem;
    mnuUnitProperties: TMenuItem;
    actBrowserShowInherited: TAction;
    LeftPageControl: TPageControl;
    LeftProjectSheet: TTabSheet;
    ProjectView: TTreeView;
    LeftClassSheet: TTabSheet;
    ClassBrowser: TClassBrowser;
    actSaveProjectAs: TAction;
    SaveprojectasItem: TMenuItem;
    mnuOpenWith: TMenuItem;
    cmbCompilers: TComboBox;
    N67: TMenuItem;
    actAttachProcess: TAction;
    ModifyWatchPop: TMenuItem;
    actModifyWatch: TAction;
    ClearallWatchPop: TMenuItem;
    CompilerOutput: TListView;
    N5: TMenuItem;
    NewClassItem: TMenuItem;
    DeleteProfilingInformation: TMenuItem;
    actDeleteProfile: TAction;
    actGotoDeclEditor: TAction;
    actGotoImplEditor: TAction;
    ToolButton1: TToolButton;
    CompResGroupBox: TPanel;
    LogOutput: TMemo;
    N64: TMenuItem;
    CollapseAll: TMenuItem;
    UncollapseAll: TMenuItem;
    actCollapse: TAction;
    actUnCollapse: TAction;
    actInsert: TAction;
    actToggle: TAction;
    actGoto: TAction;
    TEXItem: TMenuItem;
    actExportTex: TAction;
    DebugOutput: TDevConsole;
    EvaluateInput: TComboBox;
    lblEvaluate: TLabel;
    EvalOutput: TMemo;
    actStepOut: TAction;
    actRunToCursor: TAction;
    MsgPasteItem: TMenuItem;
    actMsgCopy: TAction;
    actMsgCopyAll: TAction;
    actMsgPaste: TAction;
    actMsgClear: TAction;
    actMsgSaveAll: TAction;
    actReplaceAll: TAction;
    ReplaceAll1: TMenuItem;
    N72: TMenuItem;
    actMsgCut: TAction;
    actMsgCut1: TMenuItem;
    N71: TMenuItem;
    N73: TMenuItem;
    N74: TMenuItem;
    MsgSellAllItem: TMenuItem;
    actMsgSelAll: TAction;
    actNewClass: TAction;
    actSearchAgain: TAction;
    actSearchAgain1: TMenuItem;
    N75: TMenuItem;
    SplitterBottom: TSplitter;
    N76: TMenuItem;
    N12: TMenuItem;
    Abortcompilation1: TMenuItem;
    oggleBreakpoint1: TMenuItem;
    actRevSearchAgain: TAction;
    SearchAgainBackwards1: TMenuItem;
    actDeleteLine: TAction;
    pbCompilation: TProgressBar;
    N21: TMenuItem;
    actToggleComment: TAction;
    ToggleComment1: TMenuItem;
    ToolCompilersItem: TMenuItem;
    tbCompilers: TToolBar;
    actDuplicateLine: TAction;
    N37: TMenuItem;
    DuplicateLine1: TMenuItem;
    DeleteLine1: TMenuItem;
    actMoveSelUp: TAction;
    actMoveSelDown: TAction;
    actCodeCompletion: TAction;
    actPackageCheck: TAction;
    actPackageManager: TAction;
    actHelp: TAction;
    FolderPopup: TPopupMenu;
    Addfolder2: TMenuItem;
    Renamefolder2: TMenuItem;
    Removefolder2: TMenuItem;
    N44: TMenuItem;
    Addfile1: TMenuItem;
    N69: TMenuItem;
    CloseProject1: TMenuItem;
    SourceFile1: TMenuItem;
    chkShortenPaths: TCheckBox;
    actShortenCompPaths: TAction;
    actSyntaxCheckFile: TAction;
    PageControlPanel: TPanel;
    EditorPageControlRight: TPageControl;
    EditorPageControlSplitter: TSplitter;
    actSwapEditor: TAction;
    N57: TMenuItem;
    Movetootherview1: TMenuItem;
    N14: TMenuItem;
    MoveToOtherViewItem: TMenuItem;
    SwapHeaderSourceItem: TMenuItem;
    actOpenFolder: TAction;
    N77: TMenuItem;
    actOpenFolder1: TMenuItem;
    actGotoBreakPoint: TAction;
    Abortcompilation2: TMenuItem;
    actToggleCommentInline: TAction;
    actFormatCurrentFile: TAction;
    actFormatOptions: TAction;
    actRunTests: TAction;
    actDonate: TAction;
    actRenameSymbol: TAction;
    RefactorMenu: TMenuItem;
    Rename: TMenuItem;
    N47: TMenuItem;
    actUseUTF8: TAction;
    CustomDebugPopup: TPopupMenu;
    miRuntoCursor: TMenuItem;
    miCallStack: TMenuItem;
    miCallStackFull: TMenuItem;
    N50: TMenuItem;
    Locals1: TMenuItem;
    Globals1: TMenuItem;
    FunctionParameters1: TMenuItem;
    DebugOutputPopup: TPopupMenu;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    N51: TMenuItem;
    DisplayGDBCommandsBtn: TMenuItem;
    MessageControl: TPageControl;
    actMsgDisplayGDBCommands: TAction;
    actMsgDisplayGDBAnnotations: TAction;
    StepOver1: TMenuItem;
    StepInto1: TMenuItem;
    StepInto2: TMenuItem;
    EditMakefile1: TMenuItem;
    Clean1: TMenuItem;
    N18: TMenuItem;
    Continue1: TMenuItem;
    Addwatch1: TMenuItem;
    RuntoCursor1: TMenuItem;
    N10: TMenuItem;
    EncodingItem: TMenuItem;
    CodeMenu: TMenuItem;
    FormatCurrentFile1: TMenuItem;
    InsertItem: TMenuItem;
    N26: TMenuItem;
    CommentHeaderMenuItem: TMenuItem;
    DateTimeMenuItem: TMenuItem;
    ToggleBookmarksItem: TMenuItem;
    GotoBookmarksItem: TMenuItem;
    N29: TMenuItem;
    actMoveSelUp1: TMenuItem;
    actMoveSelDown1: TMenuItem;
    N46: TMenuItem;
    SyntaxCheckCurrentFile1: TMenuItem;
    N49: TMenuItem;
    SyntaxCheck1: TMenuItem;
    FormattingOptions1: TMenuItem;
    C1: TMenuItem;
    actBreakPointProperties: TAction;
    DebugSheet: TTabSheet;
    DebugViews: TPageControl;
    DebugConsoleSheet: TTabSheet;
    CallStackSheet: TTabSheet;
    BreakpointsSheet: TTabSheet;
    StackTrace: TListView;
    BreakpointsView: TListView;
    actConvertToUTF8: TAction;
    ConvertToUTF8Item: TMenuItem;
    DebugButtonsPanel: TPanel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    WatchSheet: TTabSheet;
    WatchView: TTreeView;
    Panel2: TPanel;
    CompilerPopup: TPopupMenu;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    actMsgCompilerCopy: TAction;
    BreakpointsPopup: TPopupMenu;
    BreakpointProperties1: TMenuItem;
    actRemoveBreakpointInPane: TAction;
    RemoveBreakpoint1: TMenuItem;
    actBreakPointPropInPane: TAction;
    Openprojectorfile1: TMenuItem;
    Panel3: TPanel;
    tbClasses: TToolBar;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    actBrowserSortAlphabetically: TAction;
    actBrowserSortByType: TAction;
    tbDebug: TToolBar;
    ToolButton17: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    actOpenConsole: TAction;
    OpenShellHere1: TMenuItem;
    Panel4: TPanel;
    tbWatch: TToolBar;
    actSaveWatchList: TAction;
    actLoadWatchList: TAction;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    actLoadWatchList1: TMenuItem;
    actSaveWatchList1: TMenuItem;
    N52: TMenuItem;
    actOpenProjectFoloder: TAction;
    actOpenProjectConsole: TAction;
    N53: TMenuItem;
    OpenProjectFolder1: TMenuItem;
    OpenConsoleHere1: TMenuItem;
    actExtractMacro: TAction;
    ExtractMacro1: TMenuItem;
    ToolDebugItem: TMenuItem;
    actCopyAsRTF: TAction;
    CopyAsItem: TMenuItem;
    CopyAsRTF1: TMenuItem;
    actBack: TAction;
    actForward: TAction;
    N31: TMenuItem;
    Back1: TMenuItem;
    Forward1: TMenuItem;
    actCloseMessageSheet: TAction;
    N42: TMenuItem;
    CloseMessageSheet1: TMenuItem;
    actPrevError: TAction;
    actNextError: TAction;
    N54: TMenuItem;
    GotoPreviousError1: TMenuItem;
    GotoNextError1: TMenuItem;
    EditorPagePopup: TPopupMenu;
    MenuItem13: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    InsertPopItem: TMenuItem;
    TogglebookmarksPopItem: TMenuItem;
    GotobookmarksPopItem: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    N15: TMenuItem;
    CompileRun1: TMenuItem;
    Debug1: TMenuItem;
    tbUndo: TToolBar;
    ToolButton7: TToolButton;
    ToolButton25: TToolButton;
    ToolUndoItem: TMenuItem;
    N22: TMenuItem;
    FormatCurrentFile2: TMenuItem;
    N38: TMenuItem;
    RenameSymbol1: TMenuItem;
    ToolButton26: TToolButton;
    ReformatBtn: TToolButton;
    FindOutput: TFindOutput;
    FindPopup: TPopupMenu;
    mnuClearAllFindItems: TMenuItem;
    N41: TMenuItem;
    AutoDetect1: TMenuItem;
    UTF81: TMenuItem;
    ANSI1: TMenuItem;
    actAutoDetectEncoding: TAction;
    actANSI: TAction;
    actUTF8: TAction;
    actConvertToAnsi: TAction;
    ConvertToAnsi1: TMenuItem;
    LocalSheet: TTabSheet;
    txtLocals: TMemo;
    actOpenWindowsTerminal: TAction;
    OpenWindowsTerminalHere1: TMenuItem;
    OpenWindowsTerminalHere2: TMenuItem;
    AddToDoitem1: TMenuItem;
    N17: TMenuItem;
    LocalPopup: TPopupMenu;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem41: TMenuItem;
    HeaderCompletion: THeaderCompletion;
    actStatementsTypeFile: TAction;
    actStatementsTypeProject: TAction;
    N45: TMenuItem;
    ShowMembersintheFile1: TMenuItem;
    ShowMembersintheProject1: TMenuItem;
    FilesSheet: TTabSheet;
    fileBrowser: TDevFileBrowser;
    actSetCurrentFolder: TAction;
    OpenFolder1: TMenuItem;
    tbFiles: TToolBar;
    ToolButton27: TToolButton;
    Panel5: TPanel;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    actOnlyShowDevFiles: TAction;
    actLocateFile: TAction;
    ToolButton30: TToolButton;
    LocateFileinthefolder1: TMenuItem;
    LocateFileinthefolder2: TMenuItem;
    actUTF8Bom: TAction;
    actConvertToUTF8Bom: TAction;
    UTF8withBOM1: TMenuItem;
    ConvertToUTF8withBom1: TMenuItem;
    actClearAllBreakpoints: TAction;
    actClearAllBreakpoints1: TMenuItem;
    actClearAllBreakpointsInEditor: TAction;
    Clearbreakpointsintheeditor1: TMenuItem;
    actOpenCurrentFolder: TAction;
    FileBrowserPopup: TPopupMenu;
    OpenCurrentFolder1: TMenuItem;
    N55: TMenuItem;
    OnlyShowdevcppfiles1: TMenuItem;
    actOpenCurrentFolderInConsole: TAction;
    OpenCurrentFolderinConsole1: TMenuItem;
    actOpenCurrentFolderInWindowsTerminal: TAction;
    actOpenSelectedFile: TAction;
    OpenCurrentFolderinConsole2: TMenuItem;
    N56: TMenuItem;
    actOpenSelectedFile1: TMenuItem;
    mnuFileBrowserOpenWith: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure ToggleBookmarkClick(Sender: TObject);
    procedure GotoBookmarkClick(Sender: TObject);
    procedure MessageControlChange(Sender: TObject);
    procedure ProjectViewContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure ToolbarDockClick(Sender: TObject);
    procedure ToolbarDockContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure SplitterBottomMoved(Sender: TObject);
    procedure actNewSourceExecute(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
    procedure actNewTemplateExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actHistoryClearExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseAllExecute(Sender: TObject);
    procedure actCloseProjectExecute(Sender: TObject);
    procedure actExportHTMLExecute(Sender: TObject);
    procedure actExportRTFExecute(Sender: TObject);
    procedure actExportTexExecute(Sender: TObject);
    procedure actExportProjectExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actPrintSUExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actProjectManagerExecute(Sender: TObject);
    procedure actStatusBarExecute(Sender: TObject);
    procedure actFullScreenExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actCompOptionsExecute(Sender: TObject);
    procedure actEditorOptionsExecute(Sender: TObject);
    procedure actConfigToolsExecute(Sender: TObject);
    procedure actUnitRemoveExecute(Sender: TObject);
    procedure actUnitRenameExecute(Sender: TObject);
    procedure actUnitOpenExecute(Sender: TObject);
    procedure actUnitCloseExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectAddExecute(Sender: TObject);
    procedure actProjectRemoveExecute(Sender: TObject);
    procedure actProjectOptionsExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actFindAllExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actGotoLineExecute(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actCompRunExecute(Sender: TObject);
    procedure actRebuildExecute(Sender: TObject);
    procedure actCleanExecute(Sender: TObject);
    procedure actDebugExecute(Sender: TObject);
    procedure actEnviroOptionsExecute(Sender: TObject);
    procedure actProjectMakeFileExecute(Sender: TObject);
    procedure actMsgCopyExecute(Sender: TObject);
    procedure actMsgClearExecute(Sender: TObject);
    procedure actMsgHideExecute(Sender: TObject);
    procedure actUpdatePageCount(Sender: TObject); // enable on pagecount> 0
    procedure actUpdateProject(Sender: TObject); // enable on fproject assigned
    procedure actUpdateMakeFile(Sender: TObject); // enable on fproject assigned and compilation allowed
    procedure actUpdatePageorProject(Sender: TObject); // enable on either of above
    procedure actUpdateEmptyEditor(Sender: TObject); // enable on unempty editor
    procedure actUpdateDebuggerRunning(Sender: TObject); // enable when debugger running
    procedure actUpdateDeleteWatch(Sender: TObject); // enable on watch var selection
    procedure actIncrementalExecute(Sender: TObject);
    procedure CompilerOutputDblClick(Sender: TObject);
    procedure FindOutputDblClick(Sender: TObject);
    procedure actShowBarsExecute(Sender: TObject);
    procedure btnFullScrRevertClick(Sender: TObject);
    procedure FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure actAddWatchExecute(Sender: TObject);
    procedure ProjectViewClick(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actRemoveWatchExecute(Sender: TObject);
    procedure actContinueExecute(Sender: TObject);
    procedure actStopExecuteExecute(Sender: TObject);
    procedure actUndoUpdate(Sender: TObject);
    procedure actRedoUpdate(Sender: TObject);
    procedure actCutUpdate(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure actPasteUpdate(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure actSaveAsUpdate(Sender: TObject);
    procedure actFileMenuExecute(Sender: TObject);
    procedure actToolsMenuExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure ClassBrowserSelect(Sender: TObject; Filename: TFileName; Line: Integer);
//    procedure CppParserTotalProgress(Sender: TObject; const FileName: string; Total, Current: Integer);
    procedure CodeCompletionResize(Sender: TObject);
    procedure actSwapHeaderSourceExecute(Sender: TObject);
    procedure actSyntaxCheckExecute(Sender: TObject);
    procedure EditorPageControlChange(Sender: TObject);
    procedure actConfigdevShortcutsExecute(Sender: TObject);
    procedure DateTimeMenuItemClick(Sender: TObject);
    procedure CommentheaderMenuItemClick(Sender: TObject);
    procedure EditorPageControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure actNewTemplateUpdate(Sender: TObject);
    procedure actCommentExecute(Sender: TObject);
    procedure actUncommentExecute(Sender: TObject);
    procedure actIndentExecute(Sender: TObject);
    procedure actUnindentExecute(Sender: TObject);
    procedure EditorPageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure EditorPageControlDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept:
      Boolean);
    procedure actGotoFunctionExecute(Sender: TObject);
    procedure actBrowserGotoDeclarationUpdate(Sender: TObject);
    procedure actBrowserGotoDefinitionUpdate(Sender: TObject);
    procedure actBrowserGotoDeclarationExecute(Sender: TObject);
    procedure actBrowserGotoDefinitionExecute(Sender: TObject);
    procedure actBrowserNewClassUpdate(Sender: TObject);
    procedure actBrowserNewMemberUpdate(Sender: TObject);
    procedure actBrowserNewVarUpdate(Sender: TObject);
    procedure actBrowserNewClassExecute(Sender: TObject);
    procedure actBrowserNewMemberExecute(Sender: TObject);
    procedure actBrowserNewVarExecute(Sender: TObject);
    procedure actProfileExecute(Sender: TObject);
    procedure actCloseAllButThisExecute(Sender: TObject);
    procedure actStepIntoExecute(Sender: TObject);
    procedure lvBacktraceCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var
      DefaultDraw: Boolean);
    procedure lvBacktraceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
//    procedure actRunUpdate(Sender: TObject);
//    procedure actCompileRunUpdate(Sender: TObject);
    procedure actDebugExecuteUpdate(Sender: TObject);
    procedure FileMonitorNotifyChange(Sender: TObject; ChangeType: TdevMonitorChangeType; Filename: string);
    procedure FileMonitorTimer(Sender: TObject);
    procedure actFilePropertiesExecute(Sender: TObject);
    procedure actViewToDoListExecute(Sender: TObject);
    procedure actAddToDoExecute(Sender: TObject);
    procedure actProjectNewFolderExecute(Sender: TObject);
    procedure actProjectRemoveFolderExecute(Sender: TObject);
    procedure actProjectRenameFolderExecute(Sender: TObject);
    procedure ProjectViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ProjectViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure actImportMSVCExecute(Sender: TObject);
    procedure ViewCPUItemClick(Sender: TObject);
    procedure actExecParamsExecute(Sender: TObject);
    procedure DevCppDDEServerExecuteMacro(Sender: TObject; Msg: TStrings);
    procedure actShowTipsExecute(Sender: TObject);
    procedure actAbortCompilationUpdate(Sender: TObject);
    procedure actAbortCompilationExecute(Sender: TObject);
    procedure ProjectViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RemoveItem(Node: TTreeNode);
    procedure ProjectViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure actOpenEditorByTagExecute(Sender: TObject);
    procedure actWindowMenuExecute(Sender: TObject);
    procedure actGotoProjectManagerExecute(Sender: TObject);
    procedure ListItemClick(Sender: TObject);
    procedure ProjectViewCompare(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer);
    procedure ProjectViewKeyPress(Sender: TObject; var Key: Char);
    procedure ProjectViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GoToClassBrowserItemClick(Sender: TObject);
    procedure actBrowserShowInheritedExecute(Sender: TObject);
    procedure ReportWindowClose(Sender: TObject; var Action: TCloseAction);
    procedure actSaveProjectAsExecute(Sender: TObject);
    procedure mnuOpenWithClick(Sender: TObject);
    procedure CompilerOutputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FindOutputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WatchViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DebugPopupPopup(Sender: TObject);
    procedure actAttachProcessUpdate(Sender: TObject);
    procedure actAttachProcessExecute(Sender: TObject);
    procedure actModifyWatchExecute(Sender: TObject);
    procedure actModifyWatchUpdate(Sender: TObject);
    procedure ClearallWatchPopClick(Sender: TObject);
    procedure actMsgCopyAllExecute(Sender: TObject);
    procedure actMsgSaveAllExecute(Sender: TObject);
    procedure actDeleteProfileExecute(Sender: TObject);
    procedure actGotoImplDeclEditorExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled:
      Boolean);
    procedure ImportCBCprojectClick(Sender: TObject);
    procedure CompilerOutputAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure cmbGenericDropDown(Sender: TObject);
    procedure NewFileBtnClick(Sender: TObject);
    procedure actUnCollapseExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actInsertExecute(Sender: TObject);
    procedure actToggleExecute(Sender: TObject);
    procedure actGotoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditorPageControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure actBreakPointExecute(Sender: TObject);
    procedure EvaluateInputKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure actStepOutExecute(Sender: TObject);
    procedure actRunToCursorExecute(Sender: TObject);
    procedure actMsgPasteExecute(Sender: TObject);
    procedure actUpdateDebuggerRunningCPU(Sender: TObject);
    procedure actUpdateEmptyEditorFindForm(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure WatchViewAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage:
      TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    {
    procedure FindOutputAdvancedCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State:
      TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    }
    procedure actMsgCutExecute(Sender: TObject);
    {
    procedure FindOutputAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage; var
      DefaultDraw: Boolean);
    }
    procedure CompilerOutputAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage; var
      DefaultDraw: Boolean);
    procedure actMsgSelAllExecute(Sender: TObject);
    procedure actSearchAgainExecute(Sender: TObject);
    procedure FindOutputDeletion(Sender: TObject; Item: TListItem);
    procedure CompilerOutputDeletion(Sender: TObject; Item: TListItem);
    procedure ResourceOutputDeletion(Sender: TObject; Item: TListItem);
    procedure actStopExecuteUpdate(Sender: TObject);
    procedure actUpdateIndent(Sender: TObject);
    procedure actRevSearchAgainExecute(Sender: TObject);
    procedure actDeleteLineExecute(Sender: TObject);
    procedure actToggleCommentExecute(Sender: TObject);
    procedure cmbCompilersChange(Sender: TObject);
    procedure actDuplicateLineExecute(Sender: TObject);
    procedure actMoveSelUpExecute(Sender: TObject);
    procedure actMoveSelDownExecute(Sender: TObject);
    procedure actCodeCompletionUpdate(Sender: TObject);
    procedure actCodeCompletionExecute(Sender: TObject);
    procedure actPackageManagerExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actShortenCompPathsExecute(Sender: TObject);
    procedure actSyntaxCheckFileExecute(Sender: TObject);
    procedure LeftPageControlChange(Sender: TObject);
    procedure actSwapEditorExecute(Sender: TObject);
    procedure actSwapEditorUpdate(Sender: TObject);
    procedure actSaveAllUpdate(Sender: TObject);
    procedure PageControlPanelResize(Sender: TObject);
    procedure actOpenFolderExecute(Sender: TObject);
    procedure actGotoBreakPointExecute(Sender: TObject);
    procedure actToggleCommentInlineExecute(Sender: TObject);
    procedure actToggleCommentInlineUpdate(Sender: TObject);
    procedure actFormatCurrentFileExecute(Sender: TObject);
    procedure actFormatOptionsExecute(Sender: TObject);
    procedure FindOutputSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actRunTestsExecute(Sender: TObject);
    procedure WMCopyData(var Message: TMessage); message WM_COPYDATA;
    procedure WMQueryEndSession(var   Msg:TMessage); message   WM_QUERYENDSESSION;
    procedure WMMenuSelect(var Msg: TWMMenuSelect) ; message WM_MENUSELECT;
    procedure actDonateExecute(Sender: TObject);
    procedure actRenameSymbolExecute(Sender: TObject);
    procedure actUseUTF8Execute(Sender: TObject);
    procedure actUseUTF8Update(Sender: TObject);
    procedure actEncodingTypeUpdate(Sender: TObject);
    procedure actMsgDisplayGDBCommandsExecute(Sender: TObject);
    procedure actMsgDisplayGDBCommandsUpdate(Sender: TObject);
    procedure actMsgDisplayGDBAnnotationsUpdate(Sender: TObject);
    procedure actMsgDisplayGDBAnnotationsExecute(Sender: TObject);
    procedure actBreakPointPropertiesUpdate(Sender: TObject);
    procedure actBreakPointPropertiesExecute(Sender: TObject);
    procedure StackTraceClick(Sender: TObject);
    procedure OnBreakPointsChanged;
    procedure actConvertToUTF8Update(Sender: TObject);
    procedure actConvertToUTF8Execute(Sender: TObject);
    procedure actConvertToUTF8BomUpdate(Sender: TObject);
    procedure actConvertToUTF8BomExecute(Sender: TObject);
    procedure BreakpointsViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actMsgCompilerCopyExecute(Sender: TObject);
    procedure actBreakPointPropInPaneExecute(Sender: TObject);
    procedure actRemoveBreakpointInPaneExecute(Sender: TObject);
    procedure actBrowserSortByTypeExecute(Sender: TObject);
    procedure actBrowserSortAlphabeticallyExecute(Sender: TObject);
    procedure DebugOutputEnter(Sender: TObject);
    procedure actOpenConsoleExecute(Sender: TObject);
    procedure WatchViewDblClick(Sender: TObject);
    procedure actSaveWatchListUpdate(Sender: TObject);
    procedure actLoadWatchListExecute(Sender: TObject);
    procedure actSaveWatchListExecute(Sender: TObject);
    procedure actAddWatchUpdate(Sender: TObject);
    procedure actOpenProjectFoloderExecute(Sender: TObject);
    procedure actOpenProjectConsoleExecute(Sender: TObject);
    procedure actExtractMacroExecute(Sender: TObject);
    procedure actCopyAsRTFExecute(Sender: TObject);
    procedure actForwardUpdate(Sender: TObject);
    procedure actForwardExecute(Sender: TObject);
    procedure actBackExecute(Sender: TObject);
    procedure actBackUpdate(Sender: TObject);
    procedure actCloseMessageSheetExecute(Sender: TObject);
    procedure actNextErrorUpdate(Sender: TObject);
    procedure actPrevErrorUpdate(Sender: TObject);
    procedure actPrevErrorExecute(Sender: TObject);
    procedure actNextErrorExecute(Sender: TObject);
    procedure OnDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure FindOutputAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure mnuClearAllFindItemsClick(Sender: TObject);
    procedure CompileClean;
    procedure actCloseProjectUpdate(Sender: TObject);
    procedure actCloseAllUpdate(Sender: TObject);
    procedure actCloseUpdate(Sender: TObject);
    procedure EditorPageControlMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure actCompileUpdate(Sender: TObject);
    procedure actSyntaxCheckFileUpdate(Sender: TObject);
    procedure actRenameSymbolUpdate(Sender: TObject);
    procedure actExtractMacroUpdate(Sender: TObject);
    procedure actFindAllUpdate(Sender: TObject);
    procedure actEncodingTypeExecute(Sender: TObject);
    procedure actConvertToAnsiExecute(Sender: TObject);
    procedure actConvertToAnsiUpdate(Sender: TObject);
    procedure MessageControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SplitterLeftMoved(Sender: TObject);
    procedure LeftPageControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actOpenWindowsTerminalExecute(Sender: TObject);
    procedure actOpenWindowsTerminalUpdate(Sender: TObject);
    procedure actStatementsTypeFileUpdate(Sender: TObject);
    procedure actStatementsTypeFileExecute(Sender: TObject);
    procedure actStatementsTypeProjectExecute(Sender: TObject);
    procedure actIndentUpdate(Sender: TObject);
    procedure actSetCurrentFolderExecute(Sender: TObject);
    procedure fileBrowserDblClick(Sender: TObject);
    procedure actOnlyShowDevFilesExecute(Sender: TObject);
    procedure actLocateFileExecute(Sender: TObject);
    procedure actClearAllBreakpointsUpdate(Sender: TObject);
    procedure actClearAllBreakpointsExecute(Sender: TObject);
    procedure actClearAllBreakpointsInEditorExecute(Sender: TObject);
    procedure actOpenCurrentFolderExecute(Sender: TObject);
    procedure actOpenCurrentFolderUpdate(Sender: TObject);
    procedure actOpenCurrentFolderInConsoleExecute(Sender: TObject);
    procedure actOpenCurrentFolderInWindowsTerminalExecute(
      Sender: TObject);
    procedure fileBrowserContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure mnuFileBrowserOpenWithClick(Sender: TObject);
    procedure actOpenSelectedFileExecute(Sender: TObject);
    procedure actOpenSelectedFileUpdate(Sender: TObject);
  private
    fPreviousHeight: integer; // stores MessageControl height to be able to restore to previous height
    fPreviousWidth: integer; //stores LeftPageControl width;
    fPreviousLeftPanelOpened: boolean;
    fPreviousBottomPanelOpened: boolean;
    fPreviousLeftPageIndex : integer;
    fPreviousBottomPageIndex: integer;
    fTools: TToolController; // tool list controller
    fProjectToolWindow: TForm; // floating left tab control
    fReportToolWindow: TForm; // floating bottom tab control
    WindowPlacement: TWindowPlacement; // idem
    fFirstShow: boolean; // true for first WM_SHOW, false for others
    fRunEndAction: TRunEndAction; // determines what to do when program execution finishes
    fCompSuccessAction: TCompSuccessAction; // determines what to do when compilation finishes
    fParseStartTime: Cardinal; // TODO: move to CppParser?
    fOldCompilerToolbarIndex: integer;
    fAutoSaveTimer: TTimer;
    fProject: TProject;
    fDebugger: TDebugger;
    fCompiler: TCompiler;
    fSyntaxChecker: TCompiler;
    fEditorList: TEditorList;
    fCurrentPageHint: AnsiString;
    fLogOutputRawData: TStringList;
    fCriticalSection: TCriticalSection; // protects fFilesToOpen
    fFilesToOpen: TStringList; // files to open on show
    fQuitting: boolean ;
    fTabnine: TTabnine;
    fCheckSyntaxInBack : boolean;
    fCaretList: TDevCaretList;
    fClosing: boolean;
    fWindowsTurnedOff:boolean;
    fMessageControlChanged : boolean;
    fLeftPageControlChanged : boolean;
    fDummyCppParser: TCppParser;
    fMenuItemHint : TMenuItemHint;
    fMonitorTimer : TTimer;
    function ParseToolParams(s: AnsiString): AnsiString;
    procedure BuildBookMarkMenus;
    procedure SetHints;
    procedure MRUClick(Sender: TObject);
    procedure CodeInsClick(Sender: TObject);
    procedure ToolItemClick(Sender: TObject);
    procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;
    procedure LogEntryProc(const Msg: AnsiString);
    procedure CompOutputProc(const _Line, _Col, _Unit, _Message: AnsiString);
    procedure CompResOutputProc(const _Line, _Col, _Unit, _Message: AnsiString);
    procedure CompEndProc;
    procedure CompSuccessProc;
    procedure RunEndProc;
    procedure LoadText;
    procedure LoadColor;
    procedure ReloadColor;
    procedure SaveLastOpens;
    procedure LoadLastOpens;
    procedure OpenUnit;
    function PrepareForRun(ForcedCompileTarget: TTarget = cttInvalid): Boolean;
    function PrepareForCompile(ForcedCompileTarget: TTarget = cttInvalid; checkSyntax:boolean = False): Boolean;
    function PrepareForClean(ForcedCompileTarget: TTarget = cttInvalid): Boolean;
    procedure LoadTheme;
    procedure CheckForDLLProfiling;
    procedure ProjectWindowClose(Sender: TObject; var Action: TCloseAction);
    procedure BuildOpenWith;
    procedure PrepareDebugger;
    procedure ClearCompileMessages;
    procedure ClearMessageControl;
    procedure UpdateClassBrowsing;
    function ParseParameters(const Parameters: WideString): Integer;
    procedure CloseProject(RefreshEditor:boolean);
    procedure StartTabnine;
    procedure StopTabnine;
    procedure ChangeEncoding(encoding:TFileEncodingType);
    procedure UpdateDebugInfo;
    procedure OpenShell(Sender: TObject;const folder; const shellName:string);
    procedure UpdateStatementsType;
    procedure setLeftPageControlPage( page: TTabSheet);
    procedure CppParserTotalProgress(var message:TMessage); message WM_PARSER_PROGRESS;
    procedure CppParserStartParsing(var message:TMessage); message WM_PARSER_BEGIN_PARSE;
    procedure CppParserEndParsing(var message:TMessage); message WM_PARSER_END_PARSE;
  public
    function GetCppParser:TCppParser;
    procedure CheckSyntaxInBack(e:TEditor);
    procedure UpdateClassBrowserForEditor(e:TEditor);
    procedure UpdateFileEncodingStatusPanel;
    procedure ScanActiveProject(parse:boolean=False);
    procedure UpdateCompilerList;
    function GetCompileTarget: TTarget;
    procedure UpdateProjectEditorsEncoding;
    procedure UpdateAppTitle;
    procedure OpenCloseMessageSheet(Open: boolean);
    procedure OpenCloseLeftPageControl(Open: boolean);
    procedure OpenFile(const FileName: AnsiString;Encoding:TFileEncodingType);
    procedure OpenFileList(List: TStringList);
    procedure OpenProject(const s: AnsiString);
    procedure GotoBreakpoint(const FileName: AnsiString; Line: integer; setFocus:boolean=True);
    procedure RemoveActiveBreakpoints;
    procedure AddFindOutputItem(const line, col:integer; filename, msg:AnsiString;wordlen:integer);
    procedure EditorSaveTimer(sender: TObject);
    procedure OnInputEvalReady(const evalvalue: AnsiString);
    procedure SetStatusbarLineCol;
    procedure SetStatusbarMessage(const msg: AnsiString);
    procedure OnBacktraceReady;
    procedure SetCppParserProject(Parser:TCppParser; Project:TProject);
    // Hide variables
    property AutoSaveTimer: TTimer read fAutoSaveTimer write fAutoSaveTimer;
    property Project: TProject read fProject write fProject;
    property Debugger: TDebugger read fDebugger write fDebugger;
    property EditorList: TEditorList read fEditorList write fEditorList;
    property CurrentPageHint: AnsiString read fCurrentPageHint write fCurrentPageHint;
    property Tabnine: TTabnine read fTabnine;
    property CaretList: TDevCaretList read fCaretList;
  end;

var
  MainForm: TMainForm;

implementation

uses
  ShellAPI, IniFiles, Clipbrd, MultiLangSupport, version,
  DataFrm, NewProjectFrm, AboutFrm, PrintFrm,
  CompOptionsFrm, EditorOptFrm, IncrementalFrm, EnviroFrm,
  SynEdit, Math, ImageTheme, SynEditKeyCmds, Instances,
  Types, FindFrm, ProjectTypes, devExec, Tests,
  NewTemplateFrm, FunctionSearchFrm, NewFunctionFrm, NewVarFrm, NewClassFrm,
  ProfileAnalysisFrm, FilePropertiesFrm, AddToDoFrm, ViewToDoFrm,
  ImportMSVCFrm, ImportCBFrm, CPUFrm, FileAssocs, TipOfTheDayFrm,
  WindowListFrm, RemoveUnitFrm, ParamsFrm, ProcessListFrm, SynEditHighlighter,
  devParser;

{$R *.dfm}

{ TMenuItemHint }
{ from https://capecodgunny.blogspot.com/2014/12/how-to-display-menu-item-hints-in.html }
procedure TMenuItemHint.HideTime(Sender: TObject);
begin
  //hide (destroy) hint window
  self.ReleaseHandle;
  hideTimer.OnTimer := nil;
end;

procedure TMenuItemHint.ShowTime(Sender: TObject);
  procedure Split(Delim: Char; Str: string; Lst: TStrings) ;
  begin
    Lst.Clear;
    Lst.Delimiter     := Delim;
    Lst.DelimitedText := Str;
  end;
var
  r : TRect;
  wdth : integer;
  list : TStringList;
  s,str  : string;
  j,h,w : integer;

begin
  if activeMenuItem <> nil then begin
    str := activeMenuItem.Hint;
    str := StringReplace(str,#13#10,'|',[rfReplaceAll]);
    str := StringReplace(str,#13,'|',[rfReplaceAll]);
    str := StringReplace(str,#10,'|',[rfReplaceAll]);
    while AnsiPos('||',str) > 0 do begin
      str := StringReplace(str,'||','|',[]);
    end;

    list := TStringList.Create;
    split('|',str,list);
    s := '';
    h := Canvas.TextHeight(str) * (list.Count);
    w := 0;
    for j := 0 to list.Count -1 do begin
      if j > 0 then s := s + #13#10;
      s := s + list[j];
      wdth := Canvas.TextWidth(list[j]);
      if wdth > w then w := wdth;
    end;
    list.Free;

    //position and resize
    r.Left := Mouse.CursorPos.X;
    r.Top := Mouse.CursorPos.Y + 20;
    r.Right := r.Left + w + 8;
    r.Bottom := r.Top + h + 2;//6;
    ActivateHint(r,s);
  end;

  showTimer.OnTimer := nil;
end; (*ShowTime*)


constructor TMenuItemHint.Create(AOwner : TComponent);
begin
  inherited;
  showTimer := TTimer.Create(self) ;
  showTimer.Interval := Application.HintPause;

  hideTimer := TTimer.Create(self) ;
  hideTimer.Interval := Application.HintHidePause;
end;

procedure TMenuItemHint.DoActivateHint(menuItem : TMenuItem) ;
begin
  //force remove of the "old" hint window
  hideTime(self) ;

  if (menuItem = nil) or (menuItem.Hint = '') or assigned(menuItem.Action) then begin
    activeMenuItem := nil;
    Exit;
  end;

  activeMenuItem := menuItem;
  showTimer.OnTimer := ShowTime;
  hideTimer.OnTimer := HideTime;
end;

destructor TMenuItemHint.Destroy;
begin
  inherited;
end;
{ TTabsheet }

procedure TTabsheet.WMEraseBkGnd(var msg: TWMEraseBkGnd);
var
  FColor:TColor;
begin
  FColor := MainForm.Color;
  If FColor = clBtnFace Then
    inherited
  Else Begin
    Brush.Color := FColor;
    Windows.FillRect( msg.dc, Clientrect, Brush.handle );
    msg.result := 1;
  End;
end;

{ TPageControl }

constructor TPageControl.Create(AOwner:TComponent);
begin
  inherited;
  self.Font:=MainForm.Font;
end;

procedure TPageControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

procedure TPageControl.WMEraseBkGnd(var msg: TWMEraseBkGnd);
var
  FColor:TColor;
  r:TRect;
begin
  FColor := MainForm.Color;
  If FColor = clBtnFace Then
    inherited
  Else Begin
    Brush.Color := FColor;
    R.Top:=0;
    R.Left:=0;
    R.Bottom:=Height;
    R.Right:=Width;
    Windows.FillRect( msg.dc,self.ClientRect, Brush.handle );
    msg.result := 1;
  End;
end;

procedure TPageControl.WMLDblClick(var msg:TWMRButtonDblClk);
begin
  if (self = MainForm.EditorPageControlLeft)
    or (self = MainForm.EditorPageControlRight) then begin
    MainForm.FullscreenmodeItem.Checked := not MainForm.FullscreenmodeItem.Checked;
    MainForm.actFullScreenExecute(MainForm);
  end;
end;

procedure TPageControl.WMPaint(var Message: TWMPaint);
var
//  C: TControlCanvas;
  R: TRect;
  i,iTab:integer;
  DC: HDC;
  PS: TPaintStruct;
  bgColor,fgColor: TColor;
  gtc : TThemeColor;

begin
  if not self.OwnerDraw or not Assigned(self.OnDrawTab) then begin
    inherited;
    exit;
  end;
  self.Font := MainForm.Font;
  DC := Message.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);
  try
    self.Canvas.Font := MainForm.Font;
    strToThemeColor(gtc, devEditor.Syntax.Values[cPNL]);
    bgColor := gtc.Background;
    fgColor := gtc.Foreground;
    self.Canvas.Brush.Color := bgColor;
//    self.Canvas.FillRect(self.DisplayRect);
    R.Top:=0;
    R.Left:=0;
    R.Bottom:=Height;
    R.Right:=Width;
    self.Canvas.FillRect(R);

    iTab:=0;
    for i:=0 to self.PageCount-1 do begin
      if not self.Pages[i].TabVisible then
        continue;
      if iTab = self.TabIndex then begin
        R:=self.TabRect(iTab);
        dec(R.left,2);
        dec(R.top,2);
        self.OnDrawTab(self,i,R,true);
      end else begin
        R:=self.TabRect(iTab);
        self.OnDrawTab(self,i,R,false);
      end;
      inc(iTab);
    end;
  finally
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
  self.PaintHandler(message);
end;

{ TPanel }
procedure TPanel.WMEraseBkGnd(var msg: TWMEraseBkGnd);
var
  FColor:TColor;
begin
  FColor := MainForm.Color;
  If FColor = clBtnFace Then
    inherited
  Else Begin
    Brush.Color := FColor;
    Windows.FillRect( msg.dc, Clientrect, Brush.handle );
    msg.result := 1;
  End;
end;

{TListView}

procedure TListView.CreateWnd;
begin
  inherited;
  FHeaderHandle := ListView_GetHeader(Handle);
end;

procedure TListView.WMNotify(var AMessage: TWMNotify);
var
  FontColor: TColor;
  NMCustomDraw: TNMCustomDraw;
  PS:PAINTSTRUCT;
  oldColor:TColor;
  s:string;
  tc:TThemeColor;
begin
  if (AMessage.NMHdr.hwndFrom = FHeaderHandle) and
    (AMessage.NMHdr.code = NM_CUSTOMDRAW) then
  begin
    NMCustomDraw := PNMCustomDraw(TMessage(AMessage).LParam)^;
    case NMCustomDraw.dwDrawStage of
      CDDS_PREPAINT: begin
        AMessage.Result := CDRF_NOTIFYITEMDRAW;
      end;
      CDDS_ITEMPREPAINT:
      begin
        BeginPaint(FHeaderHandle,PS);
        try
        strToThemeColor(tc, devEditor.Syntax.Values[cPNL]);
        oldColor := self.Brush.Color;
        self.Brush.Color := tc.Background;
        FillRect(NMCustomDraw.hdc,NMCustomDraw.rc,self.Brush.Handle);

        FontColor := tc.Foreground;
        SetTextColor(NMCustomDraw.hdc, ColorToRGB(FontColor));
        SetBkColor(NMCustomDraw.hdc, ColorToRGB(self.Brush.Color));
        s:=  self.Columns[NMCustomDraw.dwItemSpec].Caption;
        TextOut(NMCustomDraw.hdc ,
          NMCustomDraw.rc.Left
            +(NMCustomDraw.rc.Right - NMCustomDraw.rc.Left - Canvas.TextWidth(s) ) div 2
          ,
          NMCustomDraw.rc.top
            +(NMCustomDraw.rc.Bottom - NMCustomDraw.rc.Top - Canvas.TextHeight(s) ) div 2,
          pChar(s),Length(s));
          {
        SetDCBrushColor(NMCustomDraw.hdc, ColorToRGB(dmMain.Cpp.IdentifierAttri.Foreground));
        SetBkColor(NMCustomDraw.hdc, ColorToRGB(dmMain.Cpp.IdentifierAttri.Foreground));
        }
        SelectObject(NMCustomDraw.hdc, GetStockObject(DC_PEN));
        SetDCPenColor(NMCustomDraw.hdc, ColorToRGB(dmMain.Cpp.IdentifierAttri.Foreground));
        MoveToEx(NMCustomDraw.hdc, NMCustomDraw.rc.left, NMCustomDraw.rc.top, nil);
        LineTo(NMCustomDraw.hdc, NMCustomDraw.rc.right, NMCustomDraw.rc.top);
        if NMCustomDraw.dwItemSpec <> 0 then begin
        MoveToEx(NMCustomDraw.hdc, NMCustomDraw.rc.left, NMCustomDraw.rc.top, nil);
        LineTo(NMCustomDraw.hdc, NMCustomDraw.rc.left, NMCustomDraw.rc.bottom);
        end;
        if (NMCustomDraw.dwItemSpec <> self.Columns.Count-1) then begin
          MoveToEx(NMCustomDraw.hdc, NMCustomDraw.rc.right, NMCustomDraw.rc.top, nil);
          LineTo(NMCustomDraw.hdc, NMCustomDraw.rc.right, NMCustomDraw.rc.bottom);
        end;
        AMessage.Result := CDRF_SKIPDEFAULT;
        finally
          endPaint(FHeaderHandle,PS);
        end;
        self.Brush.Color := oldColor;
      end;
    else
      AMessage.Result := CDRF_DODEFAULT;
    end;
  end
  else
    inherited;
end;

{TMainForm}

procedure TMainForm.LoadTheme;
  function GetImageList(size:string):TImageList;
  var
    images:TImageList;
  begin
    images := dmMain.MenuImages_NewLook;
    if sameText(size,'16x16') then begin
      images := dmMain.MenuImages_NewLook;
    end;
    if sameText(size,'24x24') then begin
      images := dmMain.MenuImages_NewLook24;
    end;
    if sameText(size,'28x28') then begin
      images := dmMain.MenuImages_NewLook28;
    end;
    if sameText(size,'32x32') then begin
      images := dmMain.MenuImages_NewLook32;
    end;
    if sameText(size,'48x48') then begin
      images := dmMain.MenuImages_NewLook48;
    end;
    Result:=images;
  end;
  procedure LoadMenuIcons;
  var
    images :TImageList;
  begin
    images := GetImageList(devData.MenuIconSize);
    ActionList.Images := images;
    MainMenu.Images := images;
    BrowserPopup.Images := images;
    DebugPopup.Images := images;
    ProjectPopup.Images := images;
    UnitPopup.Images := images;
    FolderPopup.Images := images;
    CustomDebugPopup.Images := images;
    BreakpointsPopup.Images := images;
    EditorPopup.Images := images;
    EditorPagePopup.Images := images;
    CompilerPopup.Images := images;
    MessagePopup.Images := images;
    DebugOutputPopup.Images := images;
    LocalPopup.Images := images;
    FindPopup.Images := images;
  end;
  procedure LoadTabIcons;
  var
    images :TImageList;
  begin
    images := GetImageList(devData.TabIconSize);
    LeftPageControl.Images := images;
    MessageControl.Images := images;
  end;
  procedure SetToolbarImages(toolbar:TToolbar; images:TImageList; visible:boolean);
  var
    size,i:integer;
    height: integer;
  begin
    toolbar.Visible := False;
    toolbar.Images := images;
    if images.Width < 28 then begin
      toolbar.ButtonWidth := 28;
      toolbar.ButtonHeight := 28;
    end else begin
      toolbar.ButtonWidth := images.Width;
      toolbar.ButtonHeight := images.Height;
    end;
    height := toolbar.ButtonHeight + 2;
    size := 0;
    for i:=0 to toolbar.ButtonCount-1 do begin
      size := size + toolbar.Buttons[i].Width;
    end;
    toolbar.Width := size + 2;
    toolbar.Height := height;
    toolbar.Visible:= visible;
  end;
  procedure LoadToolbarIcons;
  var
    images :TImageList;
  begin
    toolbarDock.AutoSize := False;
    toolbarDock.RowSize := 10;
    images := GetImageList(devData.ToolbarIconSize);
    SetToolbarImages(tbMain,images, devData.ToolbarMain);
    toolbarDock.RowSize := tbMain.Height+4;
    SetToolbarImages(tbCompile,images, devData.ToolbarCompile);
    SetToolbarImages(tbProject,images, devData.ToolbarProject);
    SetToolbarImages(tbEdit,images, devData.ToolbarEdit);
    SetToolbarImages(tbSearch,images, devData.ToolbarSearch);
    SetToolbarImages(tbSpecials,images, devData.ToolbarSpecials);
    SetToolbarImages(tbUndo,images, devData.ToolbarUndo);
    SetToolbarImages(tbDebug,images, devData.ToolbarDebug);

    SetToolbarImages(tbClasses,images, true);
    SetToolbarImages(tbWatch,images, true);
    SetToolbarImages(tbFiles,images, true);

    tbCompilers.Height := tbMain.ButtonHeight + 2;
    toolbarDock.AutoSize := True;
    // let the dock realign toolbars (it's realign() procedure only aligns itself)
    SendMessage(toolbarDock.Handle, WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(0,0));
    SendMessage(toolbarDock.Handle, WM_LBUTTONUP, MK_LBUTTON, MakeLParam(0,0));
  end;
begin
  LoadMenuIcons;
  LoadTabIcons;
  LoadToolbarIcons;
  if devImageThemes.IndexOf(devData.Theme) = -1 then
    devData.Theme := devImageThemes.Themes[0].Title; // 0 = New look (see ImageTheme.pas)
  {
  // make sure the theme in question is in the list
  if devImageThemes.IndexOf(devData.Theme) <> -1 then begin
    devImageThemes.ActivateTheme(devData.Theme);

    with devImageThemes do begin
      // Misc items images

      MessageControl.Images := CurrentTheme.MenuImages;

      // Set toolbar images
      tbMain.Images := CurrentTheme.MenuImages;
      tbCompile.Images := CurrentTheme.MenuImages;
      tbProject.Images := CurrentTheme.MenuImages;
      tbedit.Images := CurrentTheme.MenuImages;
      tbSearch.Images := CurrentTheme.MenuImages;
      tbSpecials.Images := CurrentTheme.MenuImages;
      tbCompilers.Images := CurrentTheme.MenuImages;
      tbUndo.Images := CurrentTheme.MenuImages;

      // Set left control images
      ProjectView.Images := CurrentTheme.ProjectImages;
      ClassBrowser.Images := CurrentTheme.BrowserImages;
      WatchView.Images := CurrentTheme.MenuImages;

      // Set left control and editor popup images
      ProjectPopup.Images := CurrentTheme.MenuImages;
      UnitPopup.Images := CurrentTheme.MenuImages;
      FolderPopup.Images := CurrentTheme.MenuImages;
      BrowserPopup.Images := CurrentTheme.MenuImages;
      DebugPopup.Images := CurrentTheme.MenuImages;
      EditorPopup.Images := CurrentTheme.MenuImages;
    end;
  end;
  }
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if fClosing then
    Exit;
  fQuitting:=True;

  SaveLastOpens;
  //CppParser.Enabled := False; // disable parser, because we are exiting;
  // Try to close the current project. If it stays open (user says cancel), stop quitting
  if Assigned(fProject) then
    CloseProject(False);

  if Assigned(fProject) and not fWindowsTurnedOff then begin
    Action := caNone;
    Exit;
  end;

  // Stop Debugger executing
  if fDebugger.Executing then
    fDebugger.Stop;
  // stop Exectuting file
  if devExecutor.Running then
    devExecutor.Reset;

  //we must clear watch view, or it will crash
  WatchView.Items.Clear;

  // Try to close all editors. If some are left open, stop quitting
  actCloseAllExecute(Self);
  if fEditorList.PageCount > 0 then begin
    Action := caNone;
    Exit;
  end;

  fDummyCppParser.Free;

  // Remember toolbar placement
  devData.LeftActivePage := LeftPageControl.ActivePageIndex;
  devData.ToolbarMainX := tbMain.Left;
  devData.ToolbarMainY := tbMain.Top;
  devData.ToolbarEditX := tbEdit.Left;
  devData.ToolbarEditY := tbEdit.Top;
  devData.ToolbarCompileX := tbCompile.Left;
  devData.ToolbarCompileY := tbCompile.Top;
  devData.ToolbarProjectX := tbProject.Left;
  devData.ToolbarProjectY := tbProject.Top;
  devData.ToolbarSpecialsX := tbSpecials.Left;
  devData.ToolbarSpecialsY := tbSpecials.Top;
  devData.ToolbarSearchX := tbSearch.Left;
  devData.ToolbarSearchY := tbSearch.Top;
  devData.ToolbarCompilersX := tbCompilers.Left;
  devData.ToolbarCompilersY := tbCompilers.Top;
  devData.ToolbarDebugX := tbDebug.Left;
  devData.ToolbarDebugY := tbDebug.Top;
  devData.ToolbarUndoX := tbUndo.Left;
  devData.ToolbarUndoY := tbUndo.Top;

  if StartsText( IncludeTrailingPathDelimiter(devDirs.Exec),
    IncludeTrailingPathDelimiter(fileBrowser.CurrentFolder) ) then begin
    devData.FileBrowserFolder := '*'+Copy(
      IncludeTrailingPathDelimiter(fileBrowser.CurrentFolder),
      Length(IncludeTrailingPathDelimiter(devDirs.Exec))+1,
      MaxInt);
  end else
    devData.FileBrowserFolder := fileBrowser.CurrentFolder;
  devData.FileBrowserOnlyShowDevFiles := fileBrowser.OnlyShowDevFiles;
  // Save left page control states
  devData.ProjectWidth := fPreviousWidth;
  devData.OutputHeight := fPreviousHeight;
  // Remember window placement
  devData.WindowState.GetPlacement(Self.Handle);
  if Assigned(fProjectToolWindow) then
    devData.ProjectWindowState.GetPlacement(fProjectToolWindow.Handle);
  if Assigned(fReportToolWindow) then
    devData.ReportWindowState.GetPlacement(fReportToolWindow.Handle);
  try
    SaveOptions;
  finally
    Action := caFree;
  end;
  StopTabnine;
  FreeAndNil(fTabnine);
  FreeAndNil(fCaretList);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fFilesToOpen);
  FreeAndNil(fCriticalSection);
  FreeAndNil(fLogOutputRawData);
  FreeAndNil(fTools);
  FreeAndNil(fAutoSaveTimer);
  FreeAndNil(devImageThemes);
  FreeAndNil(fEditorList);
  FreeAndNil(fCompiler);
  FreeAndNil(fSyntaxChecker);
  FreeAndNil(fDebugger);
  FreeAndNil(dmMain);
  devExecutor.Free; // sets itself to nil
  Lang.Free; // sets itself to nil
  DestroyOptions;
end;

procedure TMainForm.BuildBookMarkMenus;
var
  idx: integer;
  Text: AnsiString;
  Shortcutnumber: Word;
  GItem, TItem: TMenuItem;
begin
  Text := Lang[ID_MARKTEXT];
  ToggleBookMarksItem.Clear;
  GotoBookmarksItem.Clear;
  for idx := 1 to 9 do begin
    Shortcutnumber := Ord(inttostr(idx)[1]);

    TItem := TMenuItem.Create(ToggleBookmarksItem);
    TItem.Caption := format('%s &%d', [Text, idx]);
    TItem.OnClick := ToggleBookmarkClick;
    TItem.Tag := idx;
    TItem.ShortCut := ShortCut(Shortcutnumber, [ssCtrl]);
    ToggleBookmarksItem.Add(TItem);

    GItem := TMenuItem.Create(GotoBookmarksItem);
    GItem.Caption := TItem.Caption;
    GItem.OnClick := GotoBookmarkClick;
    GItem.Tag := idx;
    GItem.ShortCut := ShortCut(Shortcutnumber, [ssAlt]);
    GotoBookmarksItem.Add(GItem);
  end;

  CloneMenu(ToggleBookmarksItem, TogglebookmarksPopItem);
  CloneMenu(GotoBookmarksItem, GotobookmarksPopItem);
end;

procedure TMainForm.SetHints;
var
  idx: integer;
begin
  for idx := 0 to pred(ActionList.ActionCount) do
    TCustomAction(ActionList.Actions[idx]).Hint := StripHotKey(TCustomAction(ActionList.Actions[idx]).Caption);
end;

procedure TMainForm.WMDropFiles(var msg: TMessage);
var
  I, Count: integer;
  FileNameBuffer: array[0..260] of char;
  MessageHandle: THandle;
  FileList: TStringList;
begin
  try
    // Get drop information
    MessageHandle := THandle(msg.wParam);
    Count := DragQueryFile(MessageHandle, $FFFFFFFF, nil, 0);

    // Build file list
    FileList := TStringList.Create;
    try
      for I := 0 to Count - 1 do begin
        DragQueryFile(MessageHandle, I, FileNameBuffer, SizeOf(FileNameBuffer));
        FileList.Add(FileNameBuffer);
      end;
      OpenFileList(FileList);
      Application.BringToFront;
    finally
      FileList.Free;
    end;
  finally
    msg.Result := 0;
    DragFinish(THandle(msg.WParam));
  end;
end;

procedure TMainForm.LoadLastOpens;
var
  lastOpenIni: TIniFile;
  editor: TEditor;
  i:integer;
  count:integer;
  fileName :String;
  onLeft: boolean;
  page: ComCtrls.TPageControl;
  pos : TBufferCoord;
  focusedEditor : TEditor;
begin
  lastOpenIni := TIniFile.Create(devDirs.Config + DEV_LASTOPENS_FILE);
  try
    focusedEditor := nil;
    count := lastOpenIni.ReadInteger('LastOpens','Count',0);
    for i:=0 to Count-1 do begin
      filename := lastOpenIni.ReadString('Editor_'+IntToStr(i),'FileName','');
      if (not FileExists(fileName)) then
        continue;
      onLeft := lastOpenIni.ReadBool('Editor_'+IntToStr(i),'OnLeft',True);
      if onLeft then
        page := EditorList.LeftPageControl
      else
        page := EditorList.RightPageControl;
      editor:=EditorList.NewEditor(fileName,etAuto,False,False,page);
      if not assigned(editor) then
        Continue;
      pos.Char := lastOpenIni.ReadInteger('Editor_' + IntToStr(I), 'CursorCol', 0);
      pos.Line := lastOpenIni.ReadInteger('Editor_' + IntToStr(I), 'CursorRow', 0);
      editor.Text.CaretXY := pos;
      Editor.Text.TopLine := lastOpenIni.ReadInteger('Editor_' + IntToStr(I), 'TopLine', 0);
      Editor.Text.LeftChar := lastOpenIni.ReadInteger('Editor_' + IntToStr(I), 'LeftChar', 0);
      if lastOpenIni.ReadBool('Editor_'+IntToStr(i),'Focused',False) then begin
        focusedEditor := Editor;
      end;
      dmMain.RemoveFromHistory(FileName);
    end;
    filename := lastOpenIni.ReadString('LastOpens','Project','');
    if FileExists(filename) then begin
      OpenProject(filename);
    end else begin
      UpdateFileEncodingStatusPanel;
    end;
    if assigned(focusedEditor) then begin
      focusedEditor.Activate;
    end;
  finally
    lastOpenIni.Free;
  end;
end;

procedure TMainForm.SaveLastOpens;
var
  lastOpenIni: TIniFile;
  editor: TEditor;
  i:integer;
begin
  DeleteFile(devDirs.Config + DEV_LASTOPENS_FILE);
  lastOpenIni := TIniFile.Create(devDirs.Config + DEV_LASTOPENS_FILE);
  try
    lastOpenIni.WriteInteger('LastOpens','Count',EditorList.PageCount);
    if assigned(fProject) then begin
      lastOpenIni.WriteString('LastOpens','Project',fProject.FileName);
    end else begin
      lastOpenIni.WriteString('LastOpens','Project','');
    end;
    for i:=0 to EditorList.PageCount-1 do begin
      editor := EditorList.GetEditor(i);
      if not assigned(editor) then
        continue;
      lastOpenIni.WriteString('Editor_'+IntToStr(i),'FileName',editor.FileName);
      lastOpenIni.WriteBool('Editor_'+IntToStr(i),'OnLeft',editor.PageControl = EditorList.LeftPageControl);
      lastOpenIni.WriteBool('Editor_'+IntToStr(i),'Focused',editor.Text.Focused);
      lastOpenIni.WriteInteger('Editor_' + IntToStr(I), 'CursorCol', Editor.Text.CaretX);
      lastOpenIni.WriteInteger('Editor_' + IntToStr(I), 'CursorRow', Editor.Text.CaretY);
      lastOpenIni.WriteInteger('Editor_' + IntToStr(I), 'TopLine', Editor.Text.TopLine);
      lastOpenIni.WriteInteger('Editor_' + IntToStr(I), 'LeftChar', Editor.Text.LeftChar);
    end;
  finally
    lastOpenIni.Free;
  end;
end;

procedure TMainForm.LoadColor;
var
  selectedTC:TThemeColor;
  panelTC:TThemeColor;
  ForegroundColor :TColor;
  BackgroundColor: TColor;
  tc:TThemeColor;
begin
  strToThemeColor(panelTC, devEditor.Syntax.Values[cPNL]);
  BackgroundColor := dmMain.Cpp.WhitespaceAttribute.Background;
  ForegroundColor := dmMain.Cpp.IdentifierAttri.Foreground;  
  MainForm.Color := panelTC.Background;
  MainForm.Font.Color := panelTC.Foreground;
  LeftPageControl.OwnerDraw:=True;
  MessageControl.OwnerDraw :=True;
  DebugViews.OwnerDraw:=True;
  cmbCompilers.Color := panelTC.Background;
  cmbCompilers.Font := mainForm.Font;
  evaluateInput.Color := panelTC.Background;
  evaluateInput.Font := MainForm.Font;
  evaluateInput.Font.Color := ForegroundColor;


  strToThemeColor(selectedTC, devEditor.Syntax.Values[cSel]);
  debugOutput.Color := BackgroundColor;
  debugOutput.Font := MainForm.Font;
  debugOutput.Font.Color := ForegroundColor;

  WatchView.Color := BackgroundColor;
  WatchView.Font := MainForm.Font;
  WatchView.Font.Color := ForegroundColor;
  LocalSheet.Color := BackgroundColor;
  LocalSheet.Font := MainForm.Font;
  LocalSheet.Font.Color := ForegroundColor;
  ProjectView.Color := BackgroundColor;
  ProjectView.Font := MainForm.Font;
  ProjectView.Font.Color := ForegroundColor;
  ClassBrowser.Font := MainForm.Font;
  ClassBrowser.Font.Color := ForegroundColor;
  ClassBrowser.TreeColors[ForeColor]:=ForegroundColor;
  ClassBrowser.TreeColors[BackColor]:=BackgroundColor;
  ClassBrowser.TreeColors[SelectedBackColor]:= selectedTC.Background;
  ClassBrowser.TreeColors[SelectedForeColor]:=selectedTC.Foreground;
  ClassBrowser.TreeColors[FunctionColor] := dmMain.Cpp.FunctionAttri.Foreground;
  ClassBrowser.TreeColors[ClassColor] := dmMain.Cpp.ClassAttri.Foreground;
  ClassBrowser.TreeColors[VarColor] := dmMain.Cpp.VariableAttri.Foreground;
  ClassBrowser.TreeColors[NamespaceColor] := dmMain.Cpp.StringAttribute.Foreground;
  ClassBrowser.TreeColors[TypedefColor] := dmMain.Cpp.ClassAttri.Foreground;
  ClassBrowser.TreeColors[PreprocessorColor] := dmMain.Cpp.DirecAttri.Foreground;
  ClassBrowser.TreeColors[EnumColor] := dmMain.Cpp.IdentifierAttribute.Foreground;
  ClassBrowser.TreeColors[KeywordColor] := dmMain.Cpp.KeywordAttribute.Foreground;
  ClassBrowser.TreeColors[LocalVarColor] := dmMain.Cpp.LocalVarAttri.Foreground;
  ClassBrowser.TreeColors[GlobalVarColor] := dmMain.Cpp.GlobalVarAttri.Foreground;
  ClassBrowser.TreeColors[InheritedColor] := dmMain.Cpp.LocalVarAttri.Foreground;
  fileBrowser.Font := MainForm.Font;
  fileBrowser.Font.Color := ForegroundColor;
  fileBrowser.Color := BackgroundColor;
  fileBrowser.Colors.SelectionTextColor := selectedTC.Foreground;
  fileBrowser.Colors.SelectionRectangleBlendColor := selectedTC.Background;
  fileBrowser.Colors.SelectionRectangleBorderColor := selectedTC.Background;
  //fileBrowser.Colors.UnfocusedColor := selectedTC.Background;
  //fileBrowser.Colors.UnfocusedSelectionColor := selectedTC.Background;
  //fileBrowser.Colors.UnfocusedSelectionBorderColor := selectedTC.Background;

  //Set CompletionBox Color
  strToThemeColor(tc, devEditor.Syntax.Values[cPNL]);
  if tc.Background = dmMain.Cpp.WhitespaceAttribute.Background then begin
    strToThemeColor(tc, devEditor.Syntax.Values[cAl]);
  end;
  CodeCompletion.Colors[BackColor] := tc.Background;
  CodeCompletion.Colors[ForeColor] := dmMain.Cpp.IdentifierAttribute.Foreground;
  CodeCompletion.Colors[FunctionColor] := dmMain.Cpp.FunctionAttri.Foreground;
  CodeCompletion.Colors[ClassColor] := dmMain.Cpp.ClassAttri.Foreground;
  CodeCompletion.Colors[VarColor] := dmMain.Cpp.VariableAttri.Foreground;
  CodeCompletion.Colors[NamespaceColor] := dmMain.Cpp.StringAttribute.Foreground;
  CodeCompletion.Colors[TypedefColor] := dmMain.Cpp.ClassAttri.Foreground;
  CodeCompletion.Colors[PreprocessorColor] := dmMain.Cpp.DirecAttri.Foreground;
  CodeCompletion.Colors[EnumColor] := dmMain.Cpp.DirecAttri.Foreground;
  CodeCompletion.Colors[KeywordColor] := dmMain.Cpp.KeywordAttribute.Foreground;
  CodeCompletion.Colors[LocalVarColor] := dmMain.Cpp.LocalVarAttri.Foreground;
  CodeCompletion.Colors[GlobalVarColor] := dmMain.Cpp.GlobalVarAttri.Foreground;
  

  CodeCompletion.Colors[SelectedBackColor] := BackgroundColor;
  CodeCompletion.Colors[SelectedForeColor] := ForegroundColor;
  CodeCompletion.Color := dmMain.Cpp.WhitespaceAttribute.Background;

  HeaderCompletion.Colors[BackColor] := tc.Background;
  HeaderCompletion.Colors[ForeColor] := dmMain.Cpp.IdentifierAttribute.Foreground;
  HeaderCompletion.Colors[PreprocessorColor] := dmMain.Cpp.DirecAttri.Foreground;
  HeaderCompletion.Colors[SelectedBackColor] := BackgroundColor;
  HeaderCompletion.Color := dmMain.Cpp.WhitespaceAttribute.Background;

  if Assigned(fTabnine) then begin
    fTabnine.Colors[BackColor] := tc.Background;
    fTabnine.Colors[ForeColor] := dmMain.Cpp.IdentifierAttribute.Foreground;
    fTabnine.Colors[FunctionColor] := dmMain.Cpp.CommentAttribute.Foreground;
    fTabnine.Colors[ClassColor] := dmMain.Cpp.KeywordAttribute.Foreground;
    fTabnine.Colors[VarColor] := dmMain.Cpp.IdentifierAttribute.Foreground;
    fTabnine.Colors[NamespaceColor] := dmMain.Cpp.StringAttribute.Foreground;
    fTabnine.Colors[TypedefColor] := dmMain.Cpp.SymbolAttribute.Foreground;
    fTabnine.Colors[PreprocessorColor] := dmMain.Cpp.IdentifierAttribute.Foreground;
    fTabnine.Colors[EnumColor] := dmMain.Cpp.IdentifierAttribute.Foreground;
    fTabnine.Colors[SelectedBackColor] := BackgroundColor;
    fTabnine.Colors[SelectedForeColor] := ForegroundColor;
    fTabnine.Color := dmMain.Cpp.WhitespaceAttribute.Background;
  end;

  StackTrace.Color := BackgroundColor;
  StackTrace.Font := MainForm.Font;
  StackTrace.Font.Color := ForegroundColor;
  BreakpointsView.Color := BackgroundColor;
  BreakpointsView.Font := MainForm.Font;
  BreakpointsView.Font.Color := ForegroundColor;
  CompilerOutput.Color := BackgroundColor;
  CompilerOutput.Font := MainForm.Font;
  CompilerOutput.Font.Color := ForegroundColor;
  ResourceOutput.Color := BackgroundColor;
  ResourceOutput.Font := MainForm.Font;
  ResourceOutput.Font.Color := ForegroundColor;
  LogOutput.Color := BackgroundColor;
  LogOutput.Font := MainForm.Font;
  LogOutput.Font.Color := ForegroundColor;
  FindOutput.Color := BackgroundColor;
  FindOutput.Font := MainForm.Font;
  FindOutput.Font.Color := ForegroundColor;
  EvalOutput.Color := BackgroundColor;
  EvalOutput.Font := MainForm.Font;
  EvalOutput.Font.Color := ForegroundColor;

  txtLocals.Color := BackgroundColor;
  txtLocals.Font := MainForm.Font;
  txtLocals.Font.Color := ForegroundColor;

  chkShortenPaths.Color := BackgroundColor;
  chkShortenPaths.Font := MainForm.Font;
  chkShortenPaths.Font.Color := ForegroundColor;
end;

procedure TMainForm.ReloadColor;
begin
  LoadColor;
  debugOutput.Repaint;
  WatchView.Repaint;
  LocalSheet.Repaint;
  ProjectView.Repaint;
  ClassBrowser.Repaint;
  fileBrowser.Repaint;
  StackTrace.Repaint;
  BreakpointsView.Repaint;
  CompilerOutput.Repaint;
  ResourceOutput.Repaint;
  LogOutput.Repaint;
  FindOutput.Repaint;
  EvalOutput.Repaint;
  txtLocals.Repaint;
  MainForm.Repaint;
  PageControlPanel.Repaint;
  EditorPageControlLeft.Invalidate;
  EditorPageControlRight.Invalidate;
  chkShortenPaths.Invalidate;
end;

procedure TMainForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  debugoutput.Font.Size :=  devData.InterfaceFontSize;

  // Menus
  FileMenu.Caption := Lang[ID_MNU_FILE];
  EditMenu.Caption := Lang[ID_MNU_EDIT];
  SearchMenu.Caption := Lang[ID_MNU_SEARCH];
  ViewMenu.Caption := Lang[ID_MNU_VIEW];
  ProjectMenu.Caption := Lang[ID_MNU_PROJECT];
  ExecuteMenu.Caption := Lang[ID_MNU_EXECUTE];
  ToolsMenu.Caption := Lang[ID_MNU_TOOLS];
  WindowMenu.Caption := Lang[ID_MNU_WINDOW];
  HelpMenu.Caption := Lang[ID_MNU_HELP];
  RefactorMenu.Caption := Lang[ID_MNU_REFACTOR];
  CodeMenu.Caption := Lang[ID_MNU_CODE];

  // File menu
  mnuNew.Caption := Lang[ID_SUB_NEW];

  // New submenu
  NewFileBtn.Hint := Lang[ID_HINT_NEW];
  actNewSource.Caption := Lang[ID_ITEM_NEWSOURCE];
  actNewProject.Caption := Lang[ID_ITEM_NEWPROJECT];
  actNewTemplate.Caption := Lang[ID_ITEM_NEWTEMPLATE];
  actNewClass.Caption := Lang[ID_ITEM_NEWCLASS];

  // Top level
  actOpen.Caption := Lang[ID_ITEM_OPEN];
  actSave.Caption := Lang[ID_ITEM_SAVEFILE];
  actSaveAs.Caption := Lang[ID_ITEM_SAVEAS];
  actSaveProjectAs.Caption := Lang[ID_ITEM_SAVEASPROJECT];
  actSaveAll.Caption := Lang[ID_ITEM_SAVEALL];
  actClose.Caption := Lang[ID_ITEM_CLOSEFILE];
  actCloseProject.Caption := Lang[ID_ITEM_CLOSEPROJECT];
  actCloseAll.Caption := Lang[ID_ITEM_CLOSEALL];
  actFileProperties.Caption := Lang[ID_ITEM_PROPERTIES];

  actSetCurrentFolder.Caption := Lang[ID_ITEM_SET_CURRENT_FOLDER];
  actOnlyShowDevFiles.Caption := Lang[ID_ITEM_ONLY_SHOW_DEV_FILES];
  actLocateFile.Caption := Lang[ID_ITEM_LOCATE_FILE];

  // Import submenu
  ImportItem.Caption := Lang[ID_SUB_IMPORT];
  actImportMSVC.Caption := Lang[ID_MSVC_MENUITEM];

  // Exprt submenu
  ExportItem.Caption := Lang[ID_SUB_EXPORT];
  actExportHTML.Caption := Lang[ID_ITEM_EXPORTHTML];
  actExportRTF.Caption := Lang[ID_ITEM_EXPORTRTF];
  actExportTex.Caption := Lang[ID_ITEM_EXPORTTEX];
  actExportProject.Caption := Lang[ID_ITEM_EXPORTPROJECT];

  // Top level
  actPrint.Caption := Lang[ID_ITEM_PRINT];
  actPrintSU.Caption := Lang[ID_ITEM_PRINTSETUP];
  actHistoryClear.Caption := Lang[ID_ITEM_CLEARHISTORY];
  actExit.Caption := Lang[ID_ITEM_EXIT];

  actMsgCut.Caption := Lang[ID_ITEM_CUT];
  actMsgCopy.Caption := Lang[ID_ITEM_COPY];
  actMsgCompilerCopy.Caption := Lang[ID_ITEM_COPY];
  actMsgCopyAll.Caption := Lang[ID_ITEM_COPYALL];
  actMsgPaste.Caption := Lang[ID_ITEM_PASTE];
  actMsgSelAll.Caption := Lang[ID_ITEM_SELECTALL];
  actMsgSaveAll.Caption := Lang[ID_ITEM_SAVEALL];
  actMsgClear.Caption := Lang[ID_ITEM_CLEAR];
  actMsgDisplayGDBCommands.Caption := Lang[ID_DEB_SHOWCOMMAND];
  actMsgDisplayGDBAnnotations.Caption := Lang[ID_DEB_FULLANNOATION];

  // Edit menu
  actUndo.Caption := Lang[ID_ITEM_UNDO];
  actRedo.Caption := Lang[ID_ITEM_REDO];
  actCut.Caption := Lang[ID_ITEM_CUT];
  actCopy.Caption := Lang[ID_ITEM_COPY];
  actPaste.Caption := Lang[ID_ITEM_PASTE];
  actSelectAll.Caption := Lang[ID_ITEM_SELECTALL];
  CopyAsItem.Caption := LANG[ID_ITEM_COPYAS];
  actCopyAsRTF.Caption := LANG[ID_ITEM_COPYAS_RTF];


  EncodingItem.Caption := Lang[ID_ITEM_ENCODING];
  actUseUTF8.Caption := Lang[ID_ITEM_UTF8];
  actUseUTF8.Caption := Lang[ID_ITEM_UTF8_BOM];
  actConvertToUTF8.Caption := Lang[ID_ITEM_CONV_UTF8];
  actConvertToUTF8Bom.Caption := Lang[ID_ITEM_CONV_UTF8_BOM];
  actConvertToANSI.Caption := Format(Lang[ID_ITEM_CONV_ANSI],[UpperCase(GetSystemCharsetName)]);
  actANSI.Caption := 'ANSI('+UpperCase(GetSystemCharsetName)+')';
  actAutoDetectEncoding.Caption := Lang[ID_ITEM_AUTO_DETECT_ENCODING];

  // Insert submenu
  actInsert.Caption := Lang[ID_TB_INSERT];
  DateTimeMenuItem.Caption := Lang[ID_ITEM_DATETIME];
  CommentHeaderMenuItem.Caption := Lang[ID_ITEM_COMMENTHEADER];

  // Top level
  actToggle.Caption := Lang[ID_TB_TOGGLE];
  actGoto.Caption := Lang[ID_TB_GOTO];

  actComment.Caption := Lang[ID_ITEM_COMMENTSELECTION];
  actUncomment.Caption := Lang[ID_ITEM_UNCOMMENTSELECTION];
  actToggleComment.Caption := Lang[ID_ITEM_TOGGLECOMMENT];
  actToggleCommentInline.Caption := Lang[ID_ITEM_TOGGLECOMMENTINLINE];
  actIndent.Caption := Lang[ID_ITEM_INDENTSELECTION];
  actUnindent.Caption := Lang[ID_ITEM_UNINDENTSELECTION];
  actCollapse.Caption := Lang[ID_ITEM_COLLAPSEALL];
  actUnCollapse.Caption := Lang[ID_ITEM_UNCOLLAPSEALL];
  actDuplicateLine.Caption := Lang[ID_ITEM_DUPLICATELINE];
  actDeleteLine.Caption := Lang[ID_ITEM_DELETELINE];

  // Code menu
  actBack.Caption := LANG[ID_CODE_BACK];
  actForward.Caption := LANG[ID_CODE_FORWARD];
  actPrevError.Caption := LANG[ID_CODE_PREVERROR];
  actNextError.Caption := LANG[ID_CODE_NEXTERROR];


  actFormatCurrentFile.Caption := Lang[ID_FORMATTER_FORMATCURFILE];
  actFormatOptions.Caption := Lang[ID_FORMATTER_MENU];

  actMoveSelUp.Caption := Lang[ID_ITEM_MOVESELUP];
  actMoveSelDown.Caption := Lang[ID_ITEM_MOVESELDOWN];

  // Search Menu
  actFind.Caption := Lang[ID_ITEM_FIND];
  actFindAll.Caption := Lang[ID_ITEM_FINDINALL];
  actReplace.Caption := Lang[ID_ITEM_REPLACE];
  actReplaceAll.Caption := Lang[ID_ITEM_REPLACEFILES];
  actSearchAgain.Caption := Lang[ID_ITEM_FINDNEXT];
  actRevSearchAgain.Caption := Lang[ID_ITEM_FINDPREV];
  actIncremental.Caption := Lang[ID_ITEM_INCREMENTAL];
  actGotoFunction.Caption := Lang[ID_ITEM_GOTOFUNCTION];
  actGotoLine.Caption := Lang[ID_ITEM_GOTO];

  // View Menu
  actProjectManager.Caption := Lang[ID_ITEM_PROJECTVIEW];
  actStatusbar.Caption := Lang[ID_ITEM_STATUSBAR];
  actSwapHeaderSource.Caption := Lang[ID_ITEM_SWAPHEADERSOURCE];
  actSwapEditor.Caption := Lang[ID_ITEM_SWAPEDITOR];
  actCloseMessageSheet.Caption := LANG[ID_ITEM_CLOSEMESSAGE_SHEET];

  // Toolbar submenu
  ToolBarsItem.Caption := Lang[ID_SUB_TOOLBARS];
  ToolMainItem.Caption := Lang[ID_TOOLMAIN];
  ToolEditItem.Caption := Lang[ID_TOOLEDIT];
  ToolSearchItem.Caption := Lang[ID_TOOLSEARCH];
  ToolCompileAndRunItem.Caption := Lang[ID_TOOLCOMPRUN];
  ToolProjectItem.Caption := Lang[ID_TOOLPROJECT];
  ToolSpecialsItem.Caption := Lang[ID_TOOLSPECIAL];
  ToolCompilersItem.Caption := Lang[ID_TOOLCOMPILERS];
  ToolDebugItem.Caption := Lang[ID_TOOLDEBUG];
  ToolUndoItem.Caption := Lang[ID_TOOLUNDO];


  // Top level
  actViewToDoList.Caption := Lang[ID_VIEWTODO_MENUITEM];

  // Project menu
  actProjectNew.Caption := Lang[ID_ITEM_NEWFILE];
  actProjectAdd.Caption := Lang[ID_ITEM_ADDFILE];
  actProjectRemove.Caption := Lang[ID_ITEM_REMOVEFILE];
  actProjectOptions.Caption := Lang[ID_ITEM_PROJECTOPTIONS];
  actProjectMakeFile.Caption := Lang[ID_ITEM_EDITMAKE];

  // Execute menu
  actCompile.Caption := Lang[ID_ITEM_COMP];
  actRun.Caption := Lang[ID_ITEM_RUN];
  actCompRun.Caption := Lang[ID_ITEM_COMPRUN];
  actRebuild.Caption := Lang[ID_ITEM_REBUILD];
  actExecParams.Caption := Lang[ID_ITEM_EXECPARAMS];
  actSyntaxCheck.Caption := Lang[ID_ITEM_SYNTAXCHECK];
  actSyntaxCheckFile.Caption := Lang[ID_ITEM_SYNTAXCHECKFILE];
  actClean.Caption := Lang[ID_ITEM_CLEAN];
  actProfile.Caption := Lang[ID_ITEM_PROFILE];
  actDebug.Caption := Lang[ID_ITEM_DEBUG];
  actGotoBreakPoint.Caption := Lang[ID_ITEM_GOTOBREAKPOINT];
  actBreakPoint.Caption := Lang[ID_ITEM_TOGGLEBREAK];
  actBreakPointProperties.Caption := Lang[ID_ITEM_BREAKPOINT_PROP];
  actBreakPointPropInPane.Caption := Lang[ID_ITEM_BREAKPOINT_PROP];
  actRemoveBreakPointInPane.Caption := Lang[ID_ITEM_REMOVE_BREAKPOINT];
  actDeleteProfile.Caption := Lang[ID_ITEM_DELPROFINFORMATION];
  actAbortCompilation.Caption := Lang[ID_ITEM_ABORTCOMP];
  actClearAllBreakpoints.Caption := Lang[ID_ITEM_CLEAR_ALL_BREAKPOINTS];
  actClearAllBreakpointsInEditor.Caption := Lang[ID_ITEM_CLEAR_ALL_BREAKPOINTS_IN_FILE];

  // Tools menu
  actCompOptions.Caption := Lang[ID_ITEM_COMPOPTIONS];
  actEnviroOptions.Caption := Lang[ID_ITEM_ENVIROOPTIONS];
  actEditorOptions.Caption := Lang[ID_ITEM_EDITOROPTIONS];
  actConfigTools.Caption := Lang[ID_ITEM_TOOLCONFIG];
  actConfigdevShortcuts.Caption := Lang[ID_ITEM_SHORTCUTSCONFIG];
  actPackageCheck.Caption := Lang[ID_ITEM_UPDATECHECK];
  actPackageManager.Caption := Lang[ID_ITEM_PACKMAN];


  // Window menu
  if devData.FullScreen then
    actFullScreen.Caption := Lang[ID_ITEM_FULLSCRBACK]
  else
    actFullScreen.Caption := Lang[ID_ITEM_FULLSCRMODE];
  actNext.Caption := Lang[ID_ITEM_NEXT];
  actPrev.Caption := Lang[ID_ITEM_PREV];
  ListItem.Caption := Lang[ID_ITEM_LIST];

  // Help menu
  actHelp.Caption := Lang[ID_ITEM_HELPDEVCPP];
  actAbout.Caption := Lang[ID_ITEM_ABOUT];
  actShowTips.Caption := Lang[ID_ITEM_TIP];
  actDonate.Caption := Lang[ID_ITEM_DONATE];

  //Refactor menu
  actRenameSymbol.Caption := Lang[ID_ITEM_RENAMESYMBOL];
  actExtractMacro.Caption := Lang[ID_ITEM_EXTRACTMACRO];

  // Debugging buttons
  actAddWatch.Caption := Lang[ID_ITEM_WATCHADD];
  actEditWatch.Caption := Lang[ID_ITEM_WATCHEDIT];
  actModifyWatch.Caption := Lang[ID_ITEM_MODIFYVALUE];
  actRemoveWatch.Caption := Lang[ID_ITEM_WATCHREMOVE];
  actContinue.Caption := Lang[ID_ITEM_CONTINUE];
  actStepOver.Caption := Lang[ID_ITEM_STEPOVER];
  actStepInto.Caption := Lang[ID_ITEM_STEPINTO];
  actStepOut.Caption := Lang[ID_ITEM_STEPOUT];
  actRunToCursor.Caption := Lang[ID_ITEM_RUNTOCURSOR];

  actWatchItem.Caption := Lang[ID_ITEM_WATCHITEMS];
  actStopExecute.Caption := Lang[ID_ITEM_STOPEXECUTION];
  actViewCPU.Caption := Lang[ID_ITEM_CPUWINDOW];
  ClearallWatchPop.Caption := Lang[ID_ITEM_CLEARALL];
  actSaveWatchList.Caption := Lang[ID_NV_SAVEWATCH];
  actLoadWatchList.Caption := Lang[ID_NV_LOADWATCH];

  // Project/Unit/Folder popup
  actUnitRemove.Caption := Lang[ID_POP_REMOVE];
  actUnitRename.Caption := Lang[ID_POP_RENAME];
  actUnitOpen.Caption := Lang[ID_POP_OPEN];
  actUnitClose.Caption := Lang[ID_POP_CLOSE];
  actProjectNewFolder.Caption := Lang[ID_POP_ADDFOLDER];
  actProjectRemoveFolder.Caption := Lang[ID_POP_REMOVEFOLDER];
  actProjectRenameFolder.Caption := Lang[ID_POP_RENAMEFOLDER];
  mnuOpenWith.Caption := Lang[ID_POP_OPENWITH];
  self.actOpenProjectFoloder.Caption := Lang[ID_POP_PRJOPENFOLDER];
  actOpenProjectConsole.Caption := Lang[ID_POP_PRJOPENCONSOLE];

  // Editor popup
  actGotoDeclEditor.Caption := Lang[ID_POP_GOTODECL];
  actGotoImplEditor.Caption := Lang[ID_POP_GOTOIMPL];
  actCloseAllButThis.Caption := Lang[ID_POP_CLOSEALLBUTTHIS];
  actOpenFolder.Caption := Lang[ID_POP_OPENCONTAINING];
  actAddToDo.Caption := Lang[ID_POP_ADDTODOITEM];
  actOpenConsole.Caption := Lang[ID_POP_OPEN_CONSOLE];
  actOpenWindowsTerminal.Caption := Lang[ID_POP_OPEN_WT];

  // Class Browser Popup
  actBrowserGotoDeclaration.Caption := Lang[ID_POP_GOTODECL];
  actBrowserGotoDefinition.Caption := Lang[ID_POP_GOTOIMPL];
  actBrowserNewClass.Caption := Lang[ID_POP_NEWCLASS];
  actBrowserNewMember.Caption := Lang[ID_POP_NEWMEMBER];
  actBrowserNewVar.Caption := Lang[ID_POP_NEWVAR];
  actBrowserAddFolder.Caption := Lang[ID_POP_ADDFOLDER];
  actBrowserRemoveFolder.Caption := Lang[ID_POP_REMOVEFOLDER];
  actBrowserRenameFolder.Caption := Lang[ID_POP_RENAMEFOLDER];
  actBrowserShowInherited.Caption := Lang[ID_POP_SHOWINHERITED];
  actBrowserSortAlphabetically.Caption := Lang[ID_POP_SORT_ALPHABETICALLY];
  actBrowserSortByType.Caption := Lang[ID_POP_SORT_BY_TYPE];
  actStatementsTypeFile.Caption := Lang[ID_POP_SHOW_MEMBERS_IN_FILE];
  actStatementsTypeProject.Caption := Lang[ID_POP_SHOW_MEMBERS_IN_PROJECT];

  //FindOutput Popup
  mnuClearAllFindItems.Caption := Lang[ID_POP_CLEAR_ALL_FINDS];

  //File Browser Popup
  self.actOpenSelectedFile.Caption := Lang[ID_POP_OPEN];
  mnuFileBrowserOpenWith.Caption := Lang[ID_POP_OPENWITH];
  actOpenCurrentFolder.Caption := Lang[ID_POP_OPEN_CURRENT_FOLDER];
  actOpenCurrentFolderInConsole.Caption := Lang[ID_POP_OPEN_CURRENT_FOLDER_IN_CONSOLE];
  actOpenCurrentFolderInWindowsTerminal.Caption := Lang[ID_POP_OPEN_CURRENT_FOLDER_IN_WINDOWS_TERMINAL];
  // Message Control tabs
  CompSheet.Caption := Lang[ID_SHEET_COMP];
  ResSheet.Caption := Lang[ID_SHEET_RES];
  LogSheet.Caption := Lang[ID_SHEET_COMPLOG];
  DebugSheet.Caption := Lang[ID_SHEET_DEBUG];
  FindSheet.Caption := Lang[ID_SHEET_FIND];
  CloseSheet.Caption := Lang[ID_SHEET_CLOSE];

  // Compiler Tab
  CompilerOutput.Columns[0].Caption := Lang[ID_COL_LINE];
  CompilerOutput.Columns[1].Caption := Lang[ID_COL_COL];
  CompilerOutput.Columns[2].Caption := Lang[ID_COL_FILE];
  CompilerOutput.Columns[3].Caption := Lang[ID_COL_MSG];

  // Resource Tab
  ResourceOutput.Columns[0].Caption := Lang[ID_COL_LINE];
  ResourceOutput.Columns[1].Caption := Lang[ID_COL_COL];
  ResourceOutput.Columns[2].Caption := Lang[ID_COL_FILE];
  ResourceOutput.Columns[3].Caption := Lang[ID_COL_MSG];

  // Debug Tab
  lblEvaluate.Caption := Lang[ID_DEB_EVALUATE];
  WatchSheet.Caption := Lang[ID_DEB_WATCH];
  DebugConsoleSheet.Caption := Lang[ID_DEB_CONSOLE_SHEET];
  CallStackSheet.Caption := Lang[ID_DEB_CALLSTACK];
  BreakPointsSheet.Caption := Lang[ID_DEB_BREAK_POINTS];
  LocalSheet.Caption := Lang[ID_DEB_LOCALS];

  FilesSheet.Caption := Lang[ID_SHEET_FILES];


  {
  // Adapt UI spacing to translation text length
  len := Canvas.TextWidth(lblSendCommandGdb.Caption);
  edGdbCommand.Left := len + 10;
  edGdbCommand.Width := DebugOutput.Width - len - 6;

  len := Canvas.TextWidth(lblEvaluate.Caption);
  EvaluateInput.Left := len + 10;
  EvaluateInput.Width := EvalOutput.Width - len - 6;
  }

  // Find Results Tab
  {
  FindOutput.Columns[0].Caption := '';
  FindOutput.Columns[1].Caption := Lang[ID_COL_LINE];
  FindOutput.Columns[2].Caption := Lang[ID_COL_COL];
  FindOutput.Columns[3].Caption := Lang[ID_COL_FILE];
  FindOutput.Columns[4].Caption := Lang[ID_COL_MSG];
  }

  StackTrace.Columns[0].Caption := Lang[ID_COL_FUNC];
  StackTrace.Columns[1].Caption := Lang[ID_COL_FILE];
  StackTrace.Columns[2].Caption := Lang[ID_COL_LINE];

  BreakpointsView.Columns[0].Caption := Lang[ID_COL_FILE];
  BreakpointsView.Columns[1].Caption := Lang[ID_COL_LINE];
  BreakpointsView.Columns[2].Caption := Lang[ID_COL_CONDITION];
  // Left page control
  LeftProjectSheet.Caption := Lang[ID_LP_PROJECT];
  LeftClassSheet.Caption := Lang[ID_LP_CLASSES];


  BuildBookMarkMenus;
  SetHints;
end;

procedure TMainForm.SetStatusbarLineCol;
var
  e: TEditor;
  msg:string;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    msg := Format(Lang[ID_STATUSBARPLUS],
      [e.Text.CaretY,
      e.Text.DisplayX,
        e.Text.SelLength,
        e.Text.Lines.Count,
        e.Text.Lines.GetTextLength]);
    Statusbar.Panels[0].Text := msg;
    statusBar.Panels[0].Width := Canvas.TextWidth(msg)+20;
  end else begin
    StatusBar.Panels.BeginUpdate;
    try
      Statusbar.Panels[0].Text := '';
      Statusbar.Panels[1].Text := '';
      StatusBar.Panels[2].Text := '';
      StatusBar.Panels[3].Text := '';
    finally
      StatusBar.Panels.EndUpdate; // redraw once
    end;
  end;
end;

procedure TMainForm.SetStatusbarMessage(const msg: AnsiString);
begin
  Statusbar.Panels[3].Text := msg;
end;

procedure TMainForm.ToggleBookmarkClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  with Sender as TMenuItem do
    if Assigned(e) then begin
      Checked := not Checked;
      if (Parent = ToggleBookmarksItem) then
        TogglebookmarksPopItem.Items[Tag - 1].Checked := Checked
      else
        TogglebookmarksItem.Items[Tag - 1].Checked := Checked;
      if Checked then
        e.Text.SetBookMark(Tag, e.Text.CaretX, e.Text.CaretY)
      else
        e.Text.ClearBookMark(Tag);
    end;
end;

procedure TMainForm.GotoBookmarkClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) then
    e.Text.GotoBookMark(TMenuItem(Sender).Tag);
end;

procedure TMainForm.OpenCloseMessageSheet(Open: boolean);
begin
  if Assigned(fReportToolWindow) then
    Exit;

  // Switch between open and close
  with MessageControl do
    if Open then
      Height := fPreviousHeight // remember height
    else begin
      Height := Height - CompSheet.Height; // only show the tab captions
      ActivePageIndex := -1;
    end;
  CloseSheet.TabVisible := Open;
  SplitterBottom.Visible := Open;
  Statusbar.Top := Self.ClientHeight;
end;

procedure TMainForm.OpenCloseLeftPageControl(Open: boolean);
begin
  // Switch between open and close
  if Open then begin
    LeftPageControl.Width := fPreviousWidth
  end else begin
    LeftPageControl.Width := LeftPageControl.Width - LeftClassSheet.Width; // only show the tab captions
    LeftPageControl.ActivePageIndex := -1;
  end;
  SplitterLeft.Visible := Open;
end;

procedure TMainForm.MessageControlChange(Sender: TObject);
begin
  fMessageControlChanged := True;
  if MessageControl.ActivePage = CloseSheet then begin
    if Assigned(fReportToolWindow) then begin
      fReportToolWindow.Close;
      MessageControl.ActivePageIndex := 0;
    end else
      OpenCloseMessageSheet(false);
  end else
    OpenCloseMessageSheet(true);
end;

procedure TMainForm.MRUClick(Sender: TObject);
var
  s: AnsiString;
begin
  s := PMRUItem(dmMain.MRU[TMenuItem(Sender).Tag])^.FileName;
  if FileExists(s) then begin
    if GetFileTyp(s) = utPrj then
      OpenProject(s)
    else begin
      OpenFile(s, etAuto);
    end;
  end else
    MessageDlg(Format(Lang[ID_ERR_RENAMEDDELETED], [s]), mtInformation, [mbOK], 0);
end;

procedure TMainForm.CodeInsClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.InsertUserCodeIn(dmMain.CodeInserts[TMenuItem(Sender).Tag].Code);
end;

procedure TMainForm.ToolItemClick(Sender: TObject);
var
  idx: integer;
begin // TODO: ask on SO
  idx := (Sender as TMenuItem).Tag;

  with fTools.ToolList[idx]^ do begin
    if (PauseAfterExit) and ProgramHasConsole(ParseToolParams(Exec)) then begin
      ExecuteFile(devDirs.Exec + 'ConsolePauser.exe',
       ' 0 "'+ParseToolParams(Exec)+'" '+ParseToolParams(Params), ParseToolParams(WorkDir), SW_SHOW);
    end else
      ExecuteFile(ParseToolParams(Exec), ParseToolParams(Params), ParseToolParams(WorkDir), SW_SHOW);
  end;
end;

procedure TMainForm.setLeftPageControlPage( page: TTabSheet);
begin
  LeftPageControl.ActivePage := Page;
  fLeftPageControlChanged := False;
  ClassBrowser.TabVisible:= (LeftPageControl.ActivePage = LeftClassSheet);
end;

procedure TMainForm.OpenProject(const s: AnsiString);
var
  s2: AnsiString;
  e:TEditor;
  i:integer;
begin
  if Assigned(fProject) then begin
    if fProject.Name = '' then
      s2 := fProject.FileName
    else
      s2 := fProject.Name;
    if MessageDlg(format(Lang[ID_MSG_CLOSEPROJECTPROMPT], [s2]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      CloseProject(False)
    else
      exit;
  end;
  LeftProjectSheet.TabVisible := True;
  setLeftPageControlPage( LeftProjectSheet );
  {
  LeftPageControl.ActivePage := LeftProjectSheet;
  fLeftPageControlChanged := False;
  ClassBrowser.TabVisible:= False;
  }
  // Only update class browser once
  ClassBrowser.BeginTreeUpdate;
  try
    fProject := TProject.Create(s, DEV_INTERNAL_OPEN);

    if fProject.FileName <> '' then begin
      dmMain.RemoveFromHistory(s);

      // if project manager isn't open then open it
      if not devData.ShowLeftPages then
        actProjectManager.Execute;

      CheckForDLLProfiling;
      UpdateAppTitle;
      UpdateCompilerList;

      //parse the project
      UpdateClassBrowsing;
      ScanActiveProject(True);
      fProject.DoAutoOpen;
      //ScanActiveProject;
    end else begin
      fProject.Free;
      fProject := nil;
    end;

    //update editor's inproject flag
    for i:=0 to fProject.Units.Count-1 do begin
      if EditorList.IsFileOpened(fProject.Units[i].FileName) then begin
        e:=EditorList.GetEditorFromFileName(fProject.Units[i].FileName);
        fProject.Units[i].Editor := e;
        fProject.Units[i].Encoding := e.EncodingOption;
        e.InProject := True;
      end;
    end;

    e:=EditorList.GetEditor();
    if assigned(e) then begin
      //CppParser.InvalidateFile(e.FileName);
      CheckSyntaxInBack(e);
    end;
    UpdateClassBrowserForEditor(e);
  finally
    ClassBrowser.EndTreeUpdate;
  end;
  UpdateFileEncodingStatusPanel;
end;

procedure TMainForm.OpenFile(const FileName: AnsiString; Encoding:TFileEncodingType);
var
  e: TEditor;
begin
  // Don't bother opening duplicates
  e := fEditorList.FileIsOpen(FileName);
  if Assigned(e) then begin
    e.Activate;
    Exit;
  end;

  // Issue an error if it doesn't exist
  if not FileExists(FileName) then begin
    MessageBox(Application.Handle, PAnsiChar(Format(Lang[ID_ERR_FILENOTFOUND], [FileName])), PChar(Lang[ID_ERROR]),
      MB_ICONHAND);
    Exit;
  end;

  {
  if fTabnine.Executing then begin
    fTabnine.PrefetchFile(FileName);
  end;
  }

  // Open the file in an editor
  e := fEditorList.NewEditor(FileName,Encoding, False, False);
  if Assigned(fProject) then begin
    if (not SameFileName(fProject.FileName, FileName)) and (fProject.GetUnitFromString(FileName) = -1) then
      dmMain.RemoveFromHistory(FileName);
  end else begin
    dmMain.RemoveFromHistory(FileName);
  end;

  e.Activate;
  CheckSyntaxInBack(e);
  UpdateFileEncodingStatusPanel;

  if not Assigned(fProject) and (LeftPageControl.ActivePage <> self.FilesSheet ) then begin
    setLeftPageControlPage(LeftClassSheet);
    {
    LeftPageControl.ActivePage := LeftClassSheet;
    fLeftPageControlChanged := False;
    ClassBrowser.TabVisible := True;
    }
  end;
end;

procedure TMainForm.OpenFileList(List: TStringList);
var
  I: integer;
begin
  if List.Count = 0 then
    Exit;

  // Check if there is a project file inside the list
  for I := 0 to List.Count - 1 do begin
    if GetFileTyp(List[I]) = utPrj then begin
      OpenProject(List[I]); // open only the first found project
      Exit;
    end;
  end;

  // Didn't find a project? Open the whole list
  ClassBrowser.BeginTreeUpdate;
  try
    fEditorList.BeginUpdate;
    try
      for I := 0 to List.Count - 1 do  begin
        // open all files
        OpenFile(List[I], etAuto)
      end;
    finally
      fEditorList.EndUpdate;
    end;
  finally
    ClassBrowser.EndTreeUpdate;
  end;
end;

procedure TMainForm.AddFindOutputItem(const line, col:integer; filename, msg: AnsiString; wordlen:integer);
begin
  FindOutput.AddFindHit(filename,line,col,wordlen,msg);
end;

function TMainForm.ParseToolParams(s: AnsiString): AnsiString;
resourcestring
  cEXEName = '<EXENAME>';
  cPrjName = '<PROJECTNAME>';
  cPrjFile = '<PROJECTFILE>';
  cPrjPath = '<PROJECTPATH>';
  cCurSrc = '<SOURCENAME>';
  cSrcPath = '<SOURCEPATH>';
  cDevVer = '<DEVCPPVERSION>';

  cDefault = '<DEFAULT>';
  cExecDir = '<EXECPATH>';
  cSrcList = '<SOURCESPCLIST>';
  cWordxy = '<WORDXY>';
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;

  // <DEFAULT>
  s := StringReplace(s, cDefault, devDirs.Default, [rfReplaceAll]);

  // <EXECPATH>
  s := Stringreplace(s, cExecDir, devDirs.Exec, [rfReplaceAll]);

  // <DEVCPPVERISON>
  s := StringReplace(s, cDevVer, DEVCPP_VERSION, [rfReplaceAll]);

  if assigned(fProject) then begin
    // <EXENAME>
    s := StringReplace(s, cEXEName, '"' + fProject.Executable + '"', [rfReplaceAll]);

    // <PROJECTNAME>
    s := StringReplace(s, cPrjName, fProject.Name, [rfReplaceAll]);

    // <PROJECTFILE>
    s := StringReplace(s, cPrjFile, fProject.FileName, [rfReplaceAll]);

    // <PROJECTPATH>
    s := StringReplace(s, cPrjPath, fProject.Directory, [rfReplaceAll]);

    if Assigned(e) then begin
      // <SOURCENAME>
      s := StringReplace(s, cCurSrc, e.FileName, [rfReplaceAll]);

      // <SOURCEPATH>
      s := StringReplace(s, cSrcPath, ExtractFilePath(e.FileName), [rfReplaceAll]);
    end;

    // <SOURCESPCLIST>
    s := StringReplace(s, cSrcList, fProject.ListUnitStr(' '), [rfReplaceAll]);
  end else if assigned(e) then begin
    // <EXENAME>
    s := StringReplace(s, cEXEName, '"' + ChangeFileExt(e.FileName, EXE_EXT) + '"', [rfReplaceAll]);

    // <PROJECTNAME>
    s := StringReplace(s, cPrjName, e.FileName, [rfReplaceAll]);

    // <PRJECTFILE>
    s := StringReplace(s, cPrjFile, e.FileName, [rfReplaceAll]);

    // <PROJECTPATH>
    s := StringReplace(s, cPrjPath, ExtractFilePath(e.FileName), [rfReplaceAll]);

    // <SOURCENAME>
    s := StringReplace(s, cCurSrc, e.FileName, [rfReplaceAll]);

    // <SOURCEPATH>
    s := StringReplace(s, cSrcPath, ExtractFilePath(e.FileName), [rfReplaceAll]);

    // <WORDXY>
    s := StringReplace(s, cWordXY, e.Text.WordAtCursor, [rfReplaceAll]);
  end;

  // clear unchanged macros

  if not assigned(fProject) then
    s := StringReplace(s, cSrcList, '', [rfReplaceAll]);

  if not assigned(e) then begin
    s := StringReplace(s, cCurSrc, '', [rfReplaceAll]);
    s := StringReplace(s, cWordXY, '', [rfReplaceAll]);
    // if no editor assigned return users default directory
    s := StringReplace(s, cSrcPath, devDirs.Default, [rfReplaceAll]);
  end;

  if not assigned(fProject) and not assigned(e) then begin
    s := StringReplace(s, cEXEName, '', [rfReplaceAll]);
    s := StringReplace(s, cPrjName, '', [rfReplaceAll]);
    s := StringReplace(s, cPrjFile, '', [rfReplaceAll]);
    s := StringReplace(s, cPrjPath, '', [rfReplaceAll]);
  end;

  Result := s;
end;

procedure TMainForm.CompOutputProc(const _Line, _Col, _Unit, _Message: AnsiString);
var
  e:TEditor;
  col,line:integer;
begin
  with CompilerOutput.Items.Add do begin
    Caption := _Line;
    SubItems.Add(_Col);
    //SubItems.Add(GetRealPath(_Unit));
    SubItems.Add(_Unit);
    SubItems.Add(_Message);
  end;


  // Update tab caption
  if CompilerOutput.Items.Count = 1 then
    CompSheet.Caption := Lang[ID_SHEET_COMP] + ' (' + IntToStr(CompilerOutput.Items.Count) + ')';

  if ( StartsStr('[Error]',_Message)
      or StartsStr('[Warning]',_Message)
      ) then begin
    if EditorList.IsFileOpened(_Unit)
      and IsNumeric(_Line) then begin
      e:=EditorList.GetEditorFromFileName(_Unit);
      line:=StrToInt(_Line);
      if (line<1) or (line>e.Text.Lines.Count) then
        Exit;
      if isNumeric(_Col) then
        col:=StrToInt(_Col)
      else
        col:=Length(e.Text.Lines[line-1])+1;
      if StartsStr('[Error]',_Message) then
        e.AddSyntaxError(line,col,setError,_Message)
      else
        e.AddSyntaxError(line,col,setWarning,_Message)
    end;
  end;
end;

procedure TMainForm.CompResOutputProc(const _Line, _Col, _Unit, _Message: AnsiString);
begin
  with ResourceOutput.Items.Add do begin
    Caption := _Line;
    SubItems.Add(_Col);
    SubItems.Add(GetRealPath(_Unit));
    SubItems.Add(_Message);
  end;

  // Update tab caption
  ResSheet.Caption := Lang[ID_SHEET_RES] + ' (' + IntToStr(ResourceOutput.Items.Count) + ')'
end;

procedure TMainForm.CompEndProc;
var
  I: integer;
  e:TEditor;
begin
  // Update tab caption
  CompSheet.Caption := Lang[ID_SHEET_COMP] + ' (' + IntToStr(CompilerOutput.Items.Count) + ')';

  // Close it if there's nothing to show
  if (fCheckSyntaxInBack) then begin
    // check syntax in back, don't change message panel
  end else if (
      (CompilerOutput.Items.Count = 0)
      and (ResourceOutput.Items.Count = 0)
      and devData.AutoCloseProgress) then begin
    OpenCloseMessageSheet(FALSE)
    // Or open it if there is anything to show
  end else begin
    if (CompilerOutput.Items.Count > 0) then begin
      if MessageControl.ActivePage <> CompSheet then  begin
        MessageControl.ActivePage := CompSheet;
        fMessageControlChanged := False;
      end;
    end else if (ResourceOutput.Items.Count > 0) then begin
      if MessageControl.ActivePage <> ResSheet then begin
        MessageControl.ActivePage := ResSheet;
        fMessageControlChanged := False;
      end;
    end;
    OpenCloseMessageSheet(TRUE);
  end;

  e:=EditorList.GetEditor();
  if Assigned(e) then begin
    e.BeginUpdate;
    e.EndUpdate;
  end;

  // Jump to problem location, sorted by significance
  if (fCompiler.ErrorCount > 0) and (not fCheckSyntaxInBack) then begin

    // First try to find errors
    for I := 0 to CompilerOutput.Items.Count - 1 do begin

      // Caption equals the 'Line' column
      if not SameStr(CompilerOutput.Items[I].Caption, '') then begin

        if StartsStr('[Error]', CompilerOutput.Items[I].SubItems[2]) then begin

          // This item has a line number, proceed to set cursor properly
          CompilerOutput.Selected := CompilerOutput.Items[I];
          CompilerOutput.Selected.MakeVisible(False);
          CompilerOutputDblClick(CompilerOutput);
          Exit;
        end;
      end;
    end;

    // Then try to find warnings
    for I := 0 to CompilerOutput.Items.Count - 1 do begin
      if not SameStr(CompilerOutput.Items[I].Caption, '') then begin
        if StartsStr('[Warning]', CompilerOutput.Items[I].SubItems[2]) then begin
          CompilerOutput.Selected := CompilerOutput.Items[I];
          CompilerOutput.Selected.MakeVisible(False);
          CompilerOutputDblClick(CompilerOutput);
          Exit;
        end;
      end;
    end;

    // Then try to find anything with a line number...
    for I := 0 to CompilerOutput.Items.Count - 1 do begin
      if not SameStr(CompilerOutput.Items[I].Caption, '') then begin
        CompilerOutput.Selected := CompilerOutput.Items[I];
        CompilerOutput.Selected.MakeVisible(False);
        CompilerOutputDblClick(CompilerOutput);
        Exit;
      end;
    end;

    // Then try to find a resource error
    if ResourceOutput.Items.Count > 0 then begin
      ResourceOutput.Selected := ResourceOutput.Items[0];
      ResourceOutput.Selected.MakeVisible(False);
      CompilerOutputDblClick(ResourceOutput);
    end;
  end;
  fCheckSyntaxInBack:=False;  
end;

procedure TMainForm.CompSuccessProc;
begin
  case fCompSuccessAction of
    csaRun: begin
        fCompiler.Run;
      end;
    csaDebug: begin
        actDebug.Execute;
      end;
    csaProfile: begin
        actProfile.Execute
      end;
  end;
  fCompSuccessAction := csaNone;
end;

procedure TMainForm.RunEndProc;
begin
  case fRunEndAction of
    reaProfile: begin
        if not Assigned(ProfileAnalysisForm) then
          ProfileAnalysisForm := TProfileAnalysisForm.Create(Self);
        ProfileAnalysisForm.Show;
      end;
  end;
  fRunEndAction := reaNone;
end;

procedure TMainForm.LogEntryProc(const Msg: AnsiString);
var
  BackSlashMsg: AnsiString;
begin
  // Keep original, but use backslash everywhere
  BackSlashMsg := StringReplace(Msg, '/', '\', [rfReplaceAll]);
  fLogOutputRawData.Add(BackSlashMsg);

  // Immediately add to UI component
  if devData.ShortenCompPaths then
    LogOutput.Lines.Add(ShortenLogOutput(BackSlashMsg))
  else
    LogOutput.Lines.Add(Msg);
end;

procedure TMainForm.ProjectViewContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  Node: TTreeNode;
begin
  if not assigned(fProject) or devData.FullScreen then
    exit;

  pt := ProjectView.ClientToScreen(MousePos);
  Node := ProjectView.GetNodeAt(MousePos.X, MousePos.Y);
  if not assigned(Node) then
    exit;
  ProjectView.Selected := Node;

  // Is this the project node?
  if Node.Level = 0 then
    ProjectPopup.Popup(pt.x, pt.Y)

    // This is either a folder or a file
  else if Node.Data <> Pointer(-1) then begin
    BuildOpenWith;
    UnitPopup.Popup(pt.X, pt.Y);
  end else begin
    FolderPopup.Popup(pt.X, pt.Y);
  end;
  Handled := TRUE;
end;

procedure TMainForm.OpenUnit;
var
  Node: TTreeNode;
  i: integer;
  pt: TPoint;
  e: TEditor;
begin
  // Determine what node we have clicked on
  if Assigned(ProjectView.Selected) then
    Node := ProjectView.Selected
  else begin
    pt := ProjectView.ScreenToClient(Mouse.CursorPos);
    Node := ProjectView.GetNodeAt(pt.x, pt.y);
  end;

  // Open the file that belongs to said node
  if Assigned(Node) and (integer(Node.Data) <> -1) then begin

    // Do not accept the project node
    if (Node.Level >= 1) then begin
      i := integer(Node.Data);

      // Only open the file if it hasn't been opened yet
      e := fEditorList.FileIsOpen(fProject.Units[i].FileName, TRUE);
      if not Assigned(e) then
        e := fProject.OpenUnit(i);

      // Activate it
      if Assigned(e) then
        e.Activate;
    end;
  end;
end;

procedure TMainForm.ProjectViewClick(Sender: TObject);
var
  e: TEditor;
begin
  if not Assigned(ProjectView.Selected) then
    Exit;
  if ProjectView.Selected.Data <> Pointer(-1) then
    OpenUnit
  else begin
    e := fEditorList.GetEditor;
    if Assigned(e) then
      e.Activate;
  end;
end;

procedure TMainForm.actNewSourceExecute(Sender: TObject);
var
  NewEditor: TEditor;
begin
  if Assigned(fProject) then begin
    if MessageDlg(Lang[ID_MSG_NEWFILE], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      actProjectNewExecute(Sender);
      exit;
    end;
  end;

  NewEditor := fEditorList.NewEditor('',etAuto, False, True) ;
  NewEditor.InsertDefaultText;
  NewEditor.Activate;
  UpdateFileEncodingStatusPanel;
  setLeftPageControlPage(LeftClassSheet);
  {
  LeftPageControl.ActivePage := LeftClassSheet;
  fLeftPageControlChanged := False;
  ClassBrowser.TabVisible := True;
  }
end;

procedure TMainForm.actNewProjectExecute(Sender: TObject);
var
  s: AnsiString;
begin
  with TNewProjectForm.Create(nil) do try
    rbCpp.Checked := devData.DefCpp;
    rbC.Checked := not rbCpp.Checked;
    if ShowModal = mrOk then begin

      // Take care of the currently opened project
      if Assigned(fProject) then begin
        if fProject.Name = '' then
          s := fProject.FileName
        else
          s := fProject.Name;

        // Ask if the user wants to close the current one. If not, abort
        if MessageDlg(format(Lang[ID_MSG_CLOSECREATEPROJECT], [s]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          CloseProject(False)
        else
          Exit;
      end;

      //Create the project folder
      if not DirectoryExists(edProjectLocation.Text) then begin
        if MessageDlg(format(Lang[ID_MSG_CREATEFOLDER], [edProjectLocation.Text]),
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          Exit;
        if not CreateDirRecursive(edProjectLocation.Text) then begin
          MessageDlg(format(Lang[ID_ERR_CREATEFOLDER], [edProjectLocation.Text]),
            mtError, [mbOK], 0);
          LogError('main.pas TMainForm.actNewProjectExecute',
            Format('Create Dir ''%s'' failed:%s',[edProjectLocation.Text,SysErrorMessage(GetLastError)]));
          Exit;
        end;
      end;
      leftProjectSheet.TabVisible := True;
      setLeftPageControlPage(self.LeftProjectSheet);

      if cbDefault.Checked then
        devData.DefCpp := rbCpp.Checked;

      s := IncludeTrailingPathDelimiter(edProjectLocation.Text)+edProjectName.Text+DEV_EXT;

      if FileExists(s) then begin
        with TSaveDialog.Create(nil) do try
          Filter := FLT_PROJECTS;
          Options := Options + [ofOverwritePrompt];
          Title := Lang[ID_NV_SAVEPROJECT];

          if Execute then
            s := FileName;
        finally
          Free
        end;
      end;


      // Create an empty project
      fProject := TProject.Create(s, edProjectName.Text);

      // Assign the selected template to it
      if not fProject.AssignTemplate(s, GetTemplate) then begin
        FreeAndNil(fProject);
        MessageBox(Application.Handle, PAnsiChar(Lang[ID_ERR_TEMPLATE]), PAnsiChar(Lang[ID_ERROR]), MB_OK or
          MB_ICONERROR);
        Exit;
      end;

      // Save project preferences to disk. Don't force save all editors yet
      fProject.SaveAll;

    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actNewTemplateExecute(Sender: TObject);
begin
  // change to save cur project as template maybe?
  with TNewTemplateForm.Create(Self) do begin
    TempProject := fProject;
    ShowModal;
  end;
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do try
    Filter := BuildFilter([FLT_PROJECTS, FLT_CS, FLT_CPPS, FLT_RES, FLT_HEADS]);
    Title := Lang[ID_NV_OPENFILE];
    Options := Options + [ofAllowMultiSelect];

    // Open all provided files
    if Execute then
      OpenFileList(TStringList(Files));
  finally
    Free;
  end;
end;

procedure TMainForm.actHistoryClearExecute(Sender: TObject);
begin
  dmMain.ClearHistory;
end;

procedure TMainForm.CheckSyntaxInBack(e:TEditor);
begin
  if not Assigned(e) then
    Exit;
  if not devEditor.AutoCheckSyntax then
    Exit;
  //not c or cpp file
  if e.Text.Highlighter <> dmMain.Cpp then
    Exit;
  if not Assigned(devCompilerSets.CompilationSet) then
    Exit;
  if fCompiler.Compiling then
    Exit;
  if fSyntaxChecker.Compiling then
    Exit;
  if fCheckSyntaxInBack then
    Exit;

  fCheckSyntaxInBack:=True;
  if not PrepareForCompile(cttStdin,True) then begin
    fCheckSyntaxInBack:=False;
    Exit;
  end;
  if e.InProject then begin
    if not assigned(MainForm.fProject) then
      Exit;
    fSyntaxChecker.Project := MainForm.fProject;
  end;
  fSyntaxChecker.CheckSyntax(True);
end;

procedure TMainForm.actSaveExecute(Sender: TObject);
var
  e: TEditor;
begin   
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    e.Save;
    if e.InProject and Assigned(fProject) then begin
      fProject.SaveAll;
    end;
    CheckSyntaxInBack(e);
  end;
end;

procedure TMainForm.actSaveAsExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    e.SaveAs;
    if e.InProject and Assigned(fProject) then begin
      fProject.SaveAll;
    end;    
    CheckSyntaxInBack(e);
  end;
end;

procedure TMainForm.actSaveAllExecute(Sender: TObject);
var
  I: integer;
  e: TEditor;
begin
  // Pause the change notifier
  FileMonitor.BeginUpdate;
  try
    // Make changes to files
    if Assigned(fProject) then begin
      fProject.SaveAll;
      UpdateAppTitle;
      if Project.CppParser.Statements.Count = 0 then // only scan entire project if it has not already been scanned...
        ScanActiveProject;
    end;

    // Make changes to files
    for I := 0 to pred(EditorList.PageCount) do begin
      e := fEditorList[I];
      if e.Text.Modified and not e.InProject then
        if not e.Save then
          Break;
    end;
  finally
    FileMonitor.EndUpdate;
  end;
end;

procedure TMainForm.actCloseExecute(Sender: TObject);
var
  e: TEditor;
begin
  fClosing:=True;
  e := fEditorList.GetEditor;
  ClassBrowser.BeginTreeUpdate;
  try
    UpdateClassBrowserForEditor(nil);
    if Assigned(e) then
      fEditorList.CloseEditor(e);
  finally
    ClassBrowser.EndTreeUpdate;
  end;

  fClosing:=False;
end;

procedure TMainForm.actCloseAllExecute(Sender: TObject);
begin
  fClosing:=True;
  ClassBrowser.BeginTreeUpdate;
  try
    fEditorList.CloseAll(fWindowsTurnedOff); // PageControlChange triggers other UI updates
    UpdateClassBrowserForEditor(nil);
  finally
    ClassBrowser.EndTreeUpdate;
  end;
  fClosing:=False;
end;

procedure  TMainForm.CloseProject(RefreshEditor:boolean);
var
  s: AnsiString;
  e:TEditor;
begin
  // Stop executing program
  actStopExecuteExecute(Self);

  // Only update file monitor once (and ignore updates)
  FileMonitor.BeginUpdate;
  try
    // TODO: should we save watches?
    if fProject.Modified then begin
      if fProject.Name = '' then
        s := fProject.FileName
      else
        s := fProject.Name;

      if fWindowsTurnedOff then
        fProject.SaveAll
      else
        case MessageDlg(Format(Lang[ID_MSG_SAVEPROJECT], [s]), mtConfirmation, mbYesNoCancel, 0) of
        mrYes: begin
            fProject.SaveAll; // do NOT save layout twice
          end;
        mrNo: begin
            fProject.Modified := FALSE;
            fProject.SaveLayout;
          end;
        mrCancel: begin
            fProject.SaveLayout;
            Exit;
          end;
      end;
    end else
      fProject.SaveLayout; // always save layout, but not when SaveAll has been called

    ClassBrowser.BeginTreeUpdate;
    try
      // Remember it
      dmMain.AddtoHistory(fProject.FileName);

      // Only update page control once
      fEditorList.BeginUpdate;
      try
        FreeandNil(fProject);
        ClassBrowser.Parser:=nil; // set parser to nil will do the clear
        ClassBrowser.CurrentFile := '';

        if not fQuitting and RefreshEditor then begin
          //reset Class browsing
          setLeftPageControlPage(LeftClassSheet);
          {
          LeftPageControl.ActivePage := LeftClassSheet;
          fLeftPageControlChanged := False;
          ClassBrowser.TabVisible := True;
          }
          e:=EditorList.GetEditor();
          if Assigned(e) and not e.InProject then begin
            UpdateClassBrowserForEditor(e);
          end;
        end;
      finally
        fEditorList.EndUpdate;
      end;
    finally
      ClassBrowser.EndTreeUpdate;
    end;
    if not fQuitting then begin
      // Clear project browser
      ProjectView.Items.Clear;

      // Clear error browser
      ClearMessageControl;

      leftProjectSheet.TabVisible := False;
    end;

    if not fQuitting and RefreshEditor then begin
      // Because fProject was assigned during editor closing, force update trigger again
      EditorPageControlLeft.OnChange(EditorPageControlLeft);
    end;

  finally
    FileMonitor.EndUpdate;
  end;

end;


procedure TMainForm.actCloseProjectExecute(Sender: TObject);
begin
  fClosing:=True;
  CloseProject(True);
  fClosing:=False;
end;

procedure TMainForm.actExportHTMLExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) then
    e.ExportToHTML;
end;

procedure TMainForm.actExportRTFExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) then
    e.ExportToRTF;
end;

procedure TMainForm.actExportTexExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) then
    e.ExportToTEX;
end;

procedure TMainForm.actExportProjectExecute(Sender: TObject);
begin
  if assigned(fProject) then
    fProject.ExportToHTML;
end;

procedure TMainForm.actPrintExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if not Assigned(e) then
    Exit; // Action checks this...

  with TPrintForm.Create(Self) do try
    if ShowModal = mrOk then begin
      with TSynEditPrint.Create(Self) do try
        SynEdit := e.Text;
        Copies := seCopies.Value;
        Wrap := cbWordWrap.Checked;
        Highlight := cbHighlight.Checked;
        SelectedOnly := cbSelection.Checked;
        Colors := cbColors.Checked;
        LineNumbers := not rbNoLN.checked;
        Highlighter := e.Text.Highlighter;
        LineNumbersInMargin := rbLNMargin.Checked;
        TabWidth := devEditor.TabSize;
        Title := ExtractFileName(e.FileName);
        Color := e.Text.Highlighter.WhitespaceAttribute.Background;
        Print;
      finally
        Free;
      end;

      devData.PrintColors := cbColors.Checked;
      devData.PrintHighlight := cbHighlight.Checked;
      devData.PrintWordWrap := cbWordWrap.Checked;
      devData.PrintLineNumbers := rbLN.Checked;
      devData.PrintLineNumbersMargins := rbLNMargin.Checked;
    end;
  finally
    Close;
  end;
end;

procedure TMainForm.actPrintSUExecute(Sender: TObject);
var
  PrinterSetupDialog: TPrinterSetupDialog;
begin
  try
    PrinterSetupDialog := TPrinterSetupDialog.Create(Self);
    PrinterSetupDialog.Execute;
    // frees itself on close
  except
    MessageDlg(Lang[ID_ENV_PRINTERFAIL], mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.actUndoExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.Undo;
end;

procedure TMainForm.actRedoExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.Redo;
end;

procedure TMainForm.actCutExecute(Sender: TObject);
var
  e: TEditor;
  oldbottomline: integer;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) and e.Text.Focused then begin
    oldbottomline := e.Text.TopLine + e.Text.LinesInWindow;
    e.Text.CutToClipboard;
    if (e.Text.TopLine + e.Text.LinesInWindow) <> oldbottomline then
      e.Text.Repaint; // fix for repaint fail
  end;
end;

procedure TMainForm.actCopyExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) and e.Text.Focused then
    e.Text.CopyToClipboard;
end;

procedure TMainForm.actPasteExecute(Sender: TObject);
var
  e: TEditor;
  oldbottomline: integer;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) and e.Text.Focused then begin
    oldbottomline := e.Text.TopLine + e.Text.LinesInWindow;
    e.Text.PasteFromClipboard;
    if (e.Text.TopLine + e.Text.LinesInWindow) <> oldbottomline then
      e.Text.Repaint; // fix for repaint fail
  end;
end;

procedure TMainForm.actSelectAllExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) and e.Text.Focused then
    e.Text.SelectAll;
end;

procedure TMainForm.actDeleteExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.ClearSelection;
end;

procedure TMainForm.actProjectManagerExecute(Sender: TObject);
begin
  // Hide/show this first, or otherwhise it'll show up to the left of ProjectToolWindow
  SplitterLeft.Visible := actProjectManager.Checked;
  if Assigned(fProjectToolWindow) then
    fProjectToolWindow.Close;
  LeftPageControl.Visible := actProjectManager.Checked;
  devData.ShowLeftPages := actProjectManager.Checked;
end;

procedure TMainForm.actStatusbarExecute(Sender: TObject);
begin
  devData.Statusbar := actStatusbar.Checked;
  Statusbar.Visible := actStatusbar.Checked;
  Statusbar.Top := Self.ClientHeight;
end;

procedure TMainForm.btnFullScrRevertClick(Sender: TObject);
begin
  actFullScreen.Execute;
end;

procedure TMainForm.actFullScreenExecute(Sender: TObject);
{
var
  Active: TWinControl;
}
begin
  devData.FullScreen := FullScreenModeItem.Checked;
  if devData.FullScreen then begin
    fPreviousLeftPanelOpened := splitterLeft.Visible;
    fPreviousBottomPanelOpened:= splitterBottom.Visible;
    fPreviousLeftPageIndex := leftPageControl.ActivePageIndex;
    fPreviousBottomPageIndex := MessageControl.ActivePageIndex;
    OpenCloseMessageSheet(False);
    OpenCloseLeftPageControl(False);
  end else begin
    if fPreviousLeftPanelOpened then begin
      leftPageControl.ActivePageIndex := fPreviousLeftPageIndex;
      fLeftPageControlChanged := False;
      OpenCloseLeftPageControl(true);
    end;
    if fPreviousBottomPanelOpened then begin
      MessageControl.ActivePageIndex := fPreviousBottomPageIndex;
      fMessageControlChanged := False;
      OpenCloseMessageSheet(true);
    end;
  end;
{
  // Remember focus
  Active := Screen.ActiveControl;

  devData.FullScreen := FullScreenModeItem.Checked;
  if devData.FullScreen then begin

    // Remember old window position
    WindowPlacement.length := sizeof(WINDOWPLACEMENT);
    GetWindowPlacement(Self.Handle, @WindowPlacement);

    // Hide stuff the user has hidden
    BorderStyle := bsNone;
    FullScreenModeItem.Caption := Lang[ID_ITEM_FULLSCRBACK];
    ToolbarDock.Visible := devData.ShowBars;

    // set size to hide form menu
    // works with multi monitors now.
    SetBounds(
      (Left + Monitor.WorkAreaRect.Left) - ClientOrigin.X,
      (Top + Monitor.WorkAreaRect.Top) - ClientOrigin.Y,
      Monitor.Width + (Width - ClientWidth),
      Monitor.Height + (Height - ClientHeight));

    // Put hint in status bar
    SetStatusbarMessage(Format(Lang[ID_FULLSCREEN_MSGNEW], [ShortCutToText(actFullScreen.ShortCut),
      ShortCutToText(actShowBars.ShortCut)]));
  end else begin

    // Reset old window position
    WindowPlacement.length := sizeof(WINDOWPLACEMENT);
    SetWindowPlacement(Self.Handle, @WindowPlacement);

    // Reset borders etc
    BorderStyle := bsSizeable;
    FullScreenModeItem.Caption := Lang[ID_ITEM_FULLSCRMODE];
    ToolbarDock.Visible := TRUE;
  end;

  // Remember focus
  if Active.CanFocus then
    Active.SetFocus;
}
end;

procedure TMainForm.actNextExecute(Sender: TObject);
begin
  EditorList.SelectNextPage;
end;

procedure TMainForm.actPrevExecute(Sender: TObject);
begin
  EditorList.SelectPrevPage;
end;

procedure TMainForm.actCompOptionsExecute(Sender: TObject);
begin
  with TCompOptForm.Create(nil) do try
    if ShowModal = mrOk then begin
      CheckForDLLProfiling;
      if (fOldCompilerToolbarIndex <> cmbCompilers.ItemIndex)
        and (assigned(fProject) or (assigned(editorList.GetEditor()))) then
        CompileClean;
      UpdateCompilerList;

    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actEditorOptionsExecute(Sender: TObject);
var
  I: integer;
  e1, e2: TEditor;
  oldCodeCompletion : boolean;
begin
  oldCodeCompletion := devCodeCompletion.Enabled;
  with TEditorOptForm.Create(nil) do try
    if ShowModal = mrOk then begin

      // Apply editor options
      dmMain.UpdateHighlighter;
      for I := 0 to EditorList.PageCount - 1 do begin
        e1 := EditorList.Editors[i];
        if Assigned(e1) then begin
          devEditor.AssignEditor(e1.Text, e1.FileName);
          e1.InitCompletion;
        end;
      end;

      ReLoadColor;

      // Repaint current editor to show new colors
      fEditorList.GetVisibleEditors(e1, e2);
      if Assigned(e1) then
        e1.Text.Repaint;
      if Assigned(e2) then
        e2.Text.Repaint;

      // Only do a lengthy reparse if completion options have changed
      if taCompletion in AccessedTabs then begin
        UpdateClassBrowsing;
        if not oldCodeCompletion and devCodeCompletion.Enabled then
          ScanActiveProject;
      end;

      //start/stop tabnine
      if devEditor.UseTabnine then
        startTabnine
      else
        stopTabnine;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actConfigToolsExecute(Sender: TObject);
begin
  fTools.Edit;
end;

procedure TMainForm.actUnitRemoveExecute(Sender: TObject);
var
  idx: integer;
  node: TTreeNode;
begin
  if not assigned(fProject) then
    exit;
  while ProjectView.SelectionCount > 0 do begin
    node := ProjectView.Selections[0];
    if not assigned(node) or (node.Level < 1) then
      Continue;
    if node.Data = Pointer(-1) then
      Continue;

    idx := integer(node.Data);

    if not fProject.RemoveEditor(idx, true) then
      exit;
  end;
end;

procedure TMainForm.actUnitRenameExecute(Sender: TObject);
var
  I, ProjIndex: integer;
  OldName, NewName, CurDir: AnsiString;
  e: TEditor;
begin
  if not assigned(fProject) then
    exit;
  if not assigned(ProjectView.Selected) or
    (ProjectView.Selected.Level < 1) then
    exit;

  if ProjectView.Selected.Data = Pointer(-1) then
    Exit;

  // Get the original name
  I := integer(ProjectView.Selected.Data);
  OldName := fProject.Units[I].FileName;
  NewName := ExtractFileName(OldName);
  CurDir := ExtractFilePath(OldName);

  // Do we want to rename?
  if ShowInputQuery(Lang[ID_RENAME], Lang[ID_MSG_FILERENAME], NewName) and (ExtractFileName(NewName) <> '') then begin

    NewName := ExpandFileto(NewName, CurDir);

    // Only continue if the user says so...
    if FileExists(NewName) and not SameFileName(OldName, NewName) then
      begin // don't remove when changing case for example
      if MessageDlg(Lang[ID_MSG_FILEEXISTS], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin

        // Close the target file...
        e := fEditorList.FileIsOpen(NewName);
        if Assigned(e) then
          fEditorList.CloseEditor(e);

        // Remove it from the current project...
        projindex := fProject.Units.IndexOf(NewName);
        if projindex <> -1 then
          fProject.RemoveEditor(projindex, false);

        // All references to the file are removed. Delete the file from disk
        DeleteFile(NewName);

        // User didn't want to overwrite
      end else
        Exit;
    end;

    // Target filename does not exist anymore. Do a rename
    try
      // change name in project file first (no actual file renaming on disk)
      fProject.SaveUnitAs(I, NewName);

      // remove old file from monitor list
      FileMonitor.UnMonitor(OldName);

      // Finally, we can rename without issues
      RenameFile(OldName, NewName);

      // Add new filename to file minitor
      FileMonitor.Monitor(NewName);
    except
      MessageDlg(Format(Lang[ID_ERR_RENAMEFILE], [OldName]), mtError, [mbok], 0);
    end;
  end;
end;

procedure TMainForm.actUnitOpenExecute(Sender: TObject);
var
  I: integer;
  e: TEditor;
begin
  if not Assigned(fProject) then
    Exit;
  if not Assigned(ProjectView.Selected) or (ProjectView.Selected.Level < 1) then
    Exit;
  if ProjectView.Selected.Data = Pointer(-1) then
    Exit;

  I := integer(ProjectView.Selected.Data);
  e := fEditorList.FileIsOpen(fProject.Units[I].FileName, TRUE);
  if Assigned(e) then
    e.Activate
  else
    fProject.OpenUnit(I);
end;

procedure TMainForm.actUnitCloseExecute(Sender: TObject);
var
  e: TEditor;
begin
  if Assigned(fProject) and Assigned(ProjectView.Selected) then begin
    e := fEditorList.FileIsOpen(fProject.Units[integer(ProjectView.Selected.Data)].FileName, TRUE);
    if Assigned(e) then
      fEditorList.CloseEditor(e);
  end;
end;

procedure TMainForm.actAboutExecute(Sender: TObject);
begin
  with TAboutForm.Create(Self) do begin
    ShowModal;
  end;
end;

procedure TMainForm.actProjectNewExecute(Sender: TObject);
var
  idx: integer;
  FolderNode: TTreeNode;
begin
  idx := -1;
  if Assigned(fProject) then begin
    if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data = Pointer(-1)) then
      FolderNode := ProjectView.Selected
    else
      FolderNode := fProject.Node;
    idx := fProject.NewUnit(FALSE, FolderNode);
  end;
  LeftProjectSheet.TabVisible := True;
  setLeftPageControlPage(LeftProjectSheet);
  {
  LeftPageControl.ActivePage := LeftProjectSheet;
  fLeftPageControlChanged := False;
  ClassBrowser.TabVisible:=False;
  }
  if idx <> -1 then
    with fProject.OpenUnit(idx) do begin
      Activate;
      Text.Modified := True;
    end;
end;

procedure TMainForm.actProjectAddExecute(Sender: TObject);
var
  idx: integer;
  FolderNode: TTreeNode;
begin
  if not Assigned(fProject) then
    Exit;

  // Let the user point to a file
  with TOpenDialog.Create(nil) do try
    Title := Lang[ID_NV_OPENADD];
    Filter := BuildFilter([FLT_CS, FLT_CPPS, FLT_RES, FLT_HEADS]);
    Options := Options + [ofAllowMultiSelect];
    InitialDir := fProject.Directory;

    // If sucessful, add to selected node
    if Execute then begin
      if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data = Pointer(-1)) then
        FolderNode := ProjectView.Selected
      else
        FolderNode := fProject.Node;

      // Add all files
      for idx := 0 to Files.Count - 1 do begin
        fProject.AddUnit(Files[idx], FolderNode, false); // add under folder
        fProject.CppParser.AddFileToScan(Files[idx], true);
      end;

      // Rebuild project tree and parse
      fProject.RebuildNodes;
      //CppParser.ParseFileList;
    end;
  finally
    Free;
  end;
end;

{ end XXXKF changed }

procedure TMainForm.actProjectRemoveExecute(Sender: TObject);
var
  I: integer;
begin
  with TRemoveUnitForm.Create(MainForm) do begin

    // Add list of project files
    for i := 0 to fProject.Units.Count - 1 do
      UnitList.Items.Add(fProject.Units[i].FileName);

    ShowModal;
  end;
end;

procedure TMainForm.UpdateProjectEditorsEncoding;
var
  i:integer;
  e:TEditor;
begin
  for i:=0 to fProject.Units.Count-1 do begin
    e := fProject.Units.Items[i].Editor;
    if Assigned(e) and (
      (e.EncodingOption<>fProject.Units.Items[i].Encoding))then begin
      e.EncodingOption := fProject.Units.Items[i].Encoding;
      e.LoadFile(e.FileName);
    end;
  end;
end;


procedure TMainForm.actProjectOptionsExecute(Sender: TObject);
begin
  if Assigned(fProject) then begin
    if fProject.ShowOptions = mrOk then begin
      SetCppParserProject(fProject.CppParser,fProject);
      UpdateAppTitle;
      if fOldCompilerToolbarIndex <> cmbCompilers.ItemIndex then
        CompileClean;
      UpdateCompilerList;
      UpdateProjectEditorsEncoding;
      fProject.SaveOptions;
    end;
  end;
end;

procedure TMainForm.actFindExecute(Sender: TObject);
var
  e: TEditor;
  s: AnsiString;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin

    // Create it when needed!
    if not Assigned(FindForm) then
      FindForm := TFindForm.Create(Self);

    s := e.Text.SelText;
    if s = '' then
      s := e.Text.WordAtCursor;

    FindForm.TabIndex := 0;
    FindForm.cboFindText.Text := s;
    FindForm.Show;
  end;
end;

procedure TMainForm.actFindAllExecute(Sender: TObject);
var
  e: TEditor;
  s: AnsiString;
begin
  s:='';
  // Create it when needed!
  if not Assigned(FindForm) then
    FindForm := TFindForm.Create(Self);

  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    s := e.Text.SelText;
    if s = '' then
      s := e.Text.WordAtCursor;
  end;

  FindForm.TabIndex := 1;
  FindForm.cboFindText.Text := s;
  FindForm.Show;
end;

procedure TMainForm.actReplaceExecute(Sender: TObject);
var
  e: TEditor;
  s: AnsiString;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin

    // Create it when needed!
    if not Assigned(FindForm) then
      FindForm := TFindForm.Create(Self);

    s := e.Text.SelText;
    if s = '' then
      s := e.Text.WordAtCursor;

    FindForm.TabIndex := 2;
    FindForm.cboFindText.Text := s;
    FindForm.Show;
  end;
end;

procedure TMainForm.actReplaceAllExecute(Sender: TObject);
var
  e: TEditor;
  s: AnsiString;
begin
  s:='';
  // Create it when needed!
  if not Assigned(FindForm) then
    FindForm := TFindForm.Create(Self);

  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    s := e.Text.SelText;
    if s = '' then
      s := e.Text.WordAtCursor;
  end;

  FindForm.TabIndex := 3;
  FindForm.cboFindText.Text := s;
  FindForm.Show;
end;

procedure TMainForm.actGotoLineExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.GotoLine;
end;

function TMainForm.GetCompileTarget: TTarget;
var
  e: TEditor;
begin
  // Check if the current file belongs to a project
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    // Treat makefiles as InProject files too
    if Assigned(fProject) and (e.InProject or (fProject.MakeFileName = e.FileName)) then begin
      Result := cttProject;
    end else begin
      Result := cttFile;
    end;

    // No editors have been opened. Check if a project is open
  end else if Assigned(fProject) then begin
    Result := cttProject;

    // No project, no editor...
  end else begin
    Result := cttNone;
  end;
end;

function TMainForm.PrepareForRun(ForcedCompileTarget: TTarget = cttInvalid): Boolean;
var
  e: TEditor;
begin
  Result := False;

  // Determine what to run
  fCompiler.Project := nil;
  fCompiler.SourceFile := '';
  fCompiler.SourceText := '';
  fCompiler.CompilerSet := devCompilerSets.CompilationSet;
  if ForcedCompileTarget <> cttInvalid then
    fCompiler.Target := ForcedCompileTarget
  else
    fCompiler.Target := GetCompileTarget;
  case fCompiler.Target of
    cttInvalid:
      Exit;
    cttNone:
      Exit;
    cttFile: begin
        e := fEditorList.GetEditor; // always succeeds if ctFile is returned
        if not Assigned(e) then
          Exit;
        if e.Text.Modified then
          if not e.Save(False,False) then
            Exit;
        fCompiler.SourceFile := fEditorList.GetEditor.FileName;
      end;
    cttProject: begin
        fCompiler.Project := fProject;
      end;
  end;

  Result := True;
end;

function TMainForm.PrepareForCompile(ForcedCompileTarget: TTarget = cttInvalid;checkSyntax:boolean = False): Boolean;
var
  i: Integer;
  e: TEditor;
  compiler : TCompiler;
begin
  Result := False;
  ClearCompileMessages;

  // always show compilation log (no intrusive windows anymore)
  if devData.ShowProgress and not (fCheckSyntaxInBack) then begin
    OpenCloseMessageSheet(True);
    MessageControl.ActivePage := LogSheet;
    fMessageControlChanged := False;
  end;

  // Set PATH variable (not overriden by SetCurrentDir)
  if Assigned(devCompilerSets.CompilationSet) then begin // should NOT be false
    if devCompilerSets.CompilationSet.BinDir.Count > 0 then
      SetPath(devCompilerSets.CompilationSet.BinDir[0])
    else begin
      LogEntryProc(Format(Lang[ID_LOG_NOBINDIRABORT], [devCompilerSets.CompilationSet.Name]));
      MessageDlg(Lang[ID_ERR_BINDIR_NOT_SET], mtError, [mbOK], 0);
      Exit; // returns false
    end;
  end else begin
   // MessageDlg(Lang[ID_ERR_BINDIR_NOT_SET], mtError, [mbOK], 0);
    Exit;
  end;

  if CheckSyntax then begin
    compiler := fSyntaxChecker;
  end else
    compiler := fCompiler;
  // Determine what to compile
  compiler.Project := nil;
  compiler.SourceFile := '';
  compiler.SourceText := '';
  compiler.CompilerSet := devCompilerSets.CompilationSet;
  if ForcedCompileTarget <> cttInvalid then
    compiler.Target := ForcedCompileTarget
  else
    compiler.Target := GetCompileTarget;
  case compiler.Target of
    cttInvalid: begin
        Exit;
      end;
    cttNone: begin
        LogEntryProc(Lang[ID_LOG_NOTHINGABORT]);
        Exit;
      end;
    cttFile: begin
        e := fEditorList.GetEditor; // always succeeds if ctFile is returned
        if not Assigned(e) then
          Exit;
        if e.Text.Modified then
          if not e.Save(False,False) then
            Exit;
        compiler.UseUTF8 := (e.FileEncoding in [etUTF8,etUTF8Bom]);
        compiler.SourceFile := e.FileName;
      end;
    cttStdin: begin
        e := fEditorList.GetEditor; // always succeeds if ctFile is returned
        if not Assigned(e) then
          Exit;
        if e.Text.Lines.Text = '' then
          Exit;
        compiler.UseUTF8 := (e.FileEncoding in [etUTF8,etUTF8Bom]);
        compiler.SourceFile := e.FileName;
        compiler.sourceText := e.Text.Lines.Text;
      end;
    cttProject: begin
        compiler.Project := fProject;

        // Save all files that are in a project
        if not fProject.SaveUnits then
          Exit;
        UpdateAppTitle;
        if fProject.CppParser.Statements.Count = 0 then // only scan entire project if it has not already been scanned...
          ScanActiveProject;

        // Check if saves have been succesful
        for i := 0 to EditorList.PageCount - 1 do begin // check if saving failed
          e := fEditorList.Editors[i];
          if e.InProject and e.Text.Modified then
            Exit;
        end;

        // Increment build number automagically
        if fProject.Options.VersionInfo.AutoIncBuildNr then
          fProject.IncrementBuildNumber;
        fProject.BuildPrivateResource;
      end;
  end;
  e := fEditorList.GetEditor; // always succeeds if ctFile is returned
  if Assigned(e) then
    e.ClearSyntaxErrors;
  Result := True;
end;

function TMainForm.PrepareForClean(ForcedCompileTarget: TTarget = cttInvalid): Boolean;
var
  e: TEditor;
begin
  Result := False;

  ClearCompileMessages;

  // always show compilation log (no intrusive windows anymore)
  if devData.ShowProgress then begin
    OpenCloseMessageSheet(True);
    MessageControl.ActivePage := LogSheet;
    fMessageControlChanged := False;
  end;

  // Set PATH variable (not overriden by SetCurrentDir)
  if Assigned(devCompilerSets.CompilationSet) then begin // should NOT be false
    if devCompilerSets.CompilationSet.BinDir.Count > 0 then
      SetPath(devCompilerSets.CompilationSet.BinDir[0])
    else begin
      LogEntryProc(Format(Lang[ID_LOG_NOBINDIRABORT], [devCompilerSets.CompilationSet.Name]));
      Exit; // returns false
    end;
  end;

  // Determine what to compile
  fCompiler.Project := nil;
  fCompiler.SourceFile := '';
  fCompiler.CompilerSet := devCompilerSets.CompilationSet;
  if ForcedCompileTarget <> cttInvalid then
    fCompiler.Target := ForcedCompileTarget
  else
    fCompiler.Target := GetCompileTarget;
  case fCompiler.Target of
    cttInvalid: begin
        Exit;
      end;
    cttNone: begin
        LogEntryProc(Lang[ID_LOG_NOTHINGABORT]);
        Exit;
      end;
    cttFile: begin
        e := fEditorList.GetEditor; // always succeeds if ctFile is returned
        fCompiler.SourceFile := e.FileName;
      end;
    cttProject: begin
        fCompiler.Project := fProject;
      end;
  end;

  Result := True;
end;

procedure TMainForm.actCompileExecute(Sender: TObject);
begin
  if devExecutor.Running then begin
    if MessageDlg(Lang[ID_MSG_STOPRUNNING], mtConfirmation, [mbYes,
            mbNo], 0) <> mrYes then
            Exit;
  end;
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForCompile then
    Exit;
  fCompiler.Compile;
end;

procedure TMainForm.actRunExecute(Sender: TObject);
begin
  if devExecutor.Running then begin
    if MessageDlg(Lang[ID_MSG_STOPRUNNING], mtConfirmation, [mbYes,
            mbNo], 0) <> mrYes then
            Exit;
  end;
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if Assigned(devCompilerSets.CompilationSet) then begin
    if devCompilerSets.CompilationSet.BinDir.Count > 0 then
      SetPath(devCompilerSets.CompilationSet.BinDir[0]);
  end;
  if not PrepareForRun then
    Exit;
  fCompiler.Run;
end;

procedure TMainForm.actCompRunExecute(Sender: TObject);
begin
  if devExecutor.Running then begin
    if MessageDlg(Lang[ID_MSG_STOPRUNNING], mtConfirmation, [mbYes,
            mbNo], 0) <> mrYes then
            Exit;
  end;
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForCompile then
    Exit;
  fCompSuccessAction := csaRun;
  fCompiler.Compile;
end;

procedure TMainForm.actRebuildExecute(Sender: TObject);
begin
  if devExecutor.Running then begin
    if MessageDlg(Lang[ID_MSG_STOPRUNNING], mtConfirmation, [mbYes,
            mbNo], 0) <> mrYes then
            Exit;
  end;
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForCompile then
    Exit;
  fCompiler.RebuildAll;
end;

procedure TMainForm.actCleanExecute(Sender: TObject);
begin
  if devExecutor.Running then begin
    if MessageDlg(Lang[ID_MSG_STOPRUNNING], mtConfirmation, [mbYes,
            mbNo], 0) <> mrYes then
            Exit;
  end;
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForClean then
    Exit;
  fCompiler.Clean;
end;

procedure TMainForm.CompileClean;
begin
  actStopExecuteExecute(nil);
  if not PrepareForClean then
    Exit;
  fCompiler.Clean;
end;

procedure TMainForm.PrepareDebugger;
begin
  fDebugger.Stop;

  // Clear logs
  DebugOutput.Clear;
  EvalOutput.Clear;

  // Restore when no watch vars are shown
  fDebugger.LeftPageIndexBackup := MainForm.LeftPageControl.ActivePageIndex;

  // Focus on the debugging buttons
  setLeftPageControlPage(WatchSheet);
  {
  LeftPageControl.ActivePage := WatchSheet;
  fLeftPageControlChanged := False;
  ClassBrowser.TabVisible:=False;
  }
  MessageControl.ActivePage := DebugSheet;
  fMessageControlChanged := False;
  DebugViews.ActivePage := LocalSheet;  // we must have this line or devcpp ui will freeze, if the local sheet is not active in the designer
  DebugViews.ActivePage := DebugConsoleSheet;
  OpenCloseMessageSheet(True);


  // Reset watch vars
  fDebugger.DeleteWatchVars(false);
end;



procedure TMainForm.UpdateDebugInfo;
begin
  fDebugger.SendCommand('backtrace', '');
  fDebugger.SendCommand('info locals', '');
  fDebugger.SendCommand('info args', '');  
end;

procedure TMainForm.actDebugExecute(Sender: TObject);
var
  e: TEditor;
  i: integer;
  filepath: AnsiString;
  DebugEnabled, StripEnabled: boolean;
  params: string;
  t: TValueRelationShip;

  function hasBreakPoint:boolean;
  var
    i:integer;
    e:TEditor;
    e1:TEditor;
  begin
    Result := False;
    e := editorList.GetEditor;
    if not assigned(e) then
      Exit;
    if not e.InProject then begin
      for i:=0 to fDebugger.BreakPointList.Count -1 do begin
        if e=PBreakPoint(fDebugger.BreakPointList[i])^.editor then begin
          Result:=True;
          exit;
        end;
      end;
    end else begin
      for i:=0 to fDebugger.BreakPointList.Count -1 do begin
        e1:=PBreakPoint(fDebugger.BreakPointList[i])^.editor;
        if assigned(e1) and e1.InProject then begin
          Result:=True;
          exit;
        end;
      end;
    end;
  end;
begin
  if fCompiler.Compiling then
    Exit;
  if devExecutor.Running then begin
    if MessageDlg(Lang[ID_MSG_STOPRUNNING], mtConfirmation, [mbYes,
            mbNo], 0) <> mrYes then
      Exit
    else
      actStopExecuteExecute(nil);
  end;
  case GetCompileTarget of
    cttProject: begin
        // Check if we enabled proper options
        DebugEnabled := fProject.GetCompilerOption('-g3') <> '0';
        StripEnabled := fProject.GetCompilerOption('-s') <> '0';

        // Ask the user if he wants to enable debugging...
        if (not DebugEnabled or StripEnabled) then begin
          if  (MessageDlg(Lang[ID_MSG_NODEBUGSYMBOLS], mtConfirmation, [mbYes,
            mbNo], 0) = mrYes) then begin

            // Enable debugging, disable stripping
            fProject.SetCompilerOption('-g3', '1');
            fProject.SetCompilerOption('-s', '0');

            fCompSuccessAction := csaDebug;
            actRebuildExecute(nil);
          end;
          Exit;
        end;

        // Did we compile?
        if not FileExists(fProject.Executable) then begin
          if MessageDlg(Lang[ID_ERR_PROJECTNOTCOMPILEDSUGGEST], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
            fCompSuccessAction := csaDebug;
            actCompileExecute(nil);
          end;
          Exit;
        end;


        // Did we choose a host application for our DLL?
        if fProject.Options.typ = dptDyn then begin
          if fProject.Options.HostApplication = '' then begin
            MessageDlg(Lang[ID_ERR_HOSTMISSING], mtWarning, [mbOK], 0);
            exit;
          end else if not FileExists(fProject.Options.HostApplication) then begin
            MessageDlg(Lang[ID_ERR_HOSTNOTEXIST], mtWarning, [mbOK], 0);
            exit;
          end;
        end;

        // Reset UI, remove invalid breakpoints
        PrepareDebugger;

        filepath := fProject.Executable;

        fDebugger.Start;
        if not fDebugger.Executing then
          Exit;
        fDebugger.SendCommand('file', '"' + StringReplace(filepath, '\', '/', [rfReplaceAll]) + '"');

        if fProject.Options.typ = dptDyn then
          fDebugger.SendCommand('exec-file', '"' + StringReplace(fProject.Options.HostApplication, '\', '/',
            [rfReplaceAll])
            + '"');

        for i:=0 to fProject.Units.Count-1 do begin
          fDebugger.SendCommand('dir', '"'+StringReplace(
            ExtractFilePath(fProject.Units[i].FileName),'\', '/',[rfReplaceAll])
            + '"');
        end;
        for i:=0 to fProject.Options.Includes.Count-1 do begin
          fDebugger.SendCommand('dir', '"'+StringReplace(
            fProject.Options.Includes[i],'\', '/',[rfReplaceAll])
            + '"');
        end;
        for i:=0 to fProject.Options.Libs.Count-1 do begin
          fDebugger.SendCommand('dir', '"'+StringReplace(
            fProject.Options.Includes[i],'\', '/',[rfReplaceAll])
            + '"');
        end;

      end;
    cttFile: begin
        // Check if we enabled proper options
        with devCompilerSets.CompilationSet do begin
          DebugEnabled := GetOption('-g3') <> '0';
          StripEnabled := GetOption('-s') <> '0';
        end;

        // Ask the user if he wants to enable debugging...
        if (not DebugEnabled or StripEnabled) and (MessageDlg(Lang[ID_MSG_NODEBUGSYMBOLS], mtConfirmation, [mbYes,
          mbNo], 0) = mrYes) then begin

          // Enable debugging, disable stripping
          with devCompilerSets.CompilationSet do begin
            SetOption('-g3', '1');
            SetOption('-s', '0');
          end;

          // Save changes to compiler set
          devCompilerSets.SaveSet(devCompilerSets.CompilationSetIndex);

          fCompSuccessAction := csaDebug;
          actRebuildExecute(nil);
          Exit;
        end;

        e := fEditorList.GetEditor;
        if Assigned(e) then begin
          // Did we save?
          if e.Text.Modified then begin // if file is modified
            if not e.Save(false,false) then // save it first
              Exit;
          end;

          // Did we compile?
          if not FileExists(ChangeFileExt(e.FileName, EXE_EXT)) then begin
            if MessageDlg(Lang[ID_ERR_SRCNOTCOMPILEDSUGGEST], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
              fCompSuccessAction := csaDebug;
              actCompileExecute(nil);
            end;
            Exit;
          end else begin
            if CompareFileModifyTime(e.FileName,ChangeFileExt(e.FileName, EXE_EXT))>=0 then
              if MessageDlg(Lang[ID_MSG_SOURCEMORERECENT], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
                fCompSuccessAction := csaDebug;
                MainForm.actCompileExecute(nil);
                Exit;
              end;
          end;

          PrepareDebugger;

          fDebugger.UseUTF8 := (e.FileEncoding in [etUTF8,etUTF8Bom]);

          filepath := ChangeFileExt(e.FileName, EXE_EXT);

          fDebugger.Start;
          if not fDebugger.Executing then
            Exit;
          fDebugger.SendCommand('file', '"' + StringReplace(filepath, '\', '/', [rfReplaceAll]) + '"');
        end;
      end;
    cttNone: Exit;
  end;


  // Add library folders
  with devCompilerSets.CompilationSet do begin
    for I := 0 to LibDir.Count - 1 do
      fDebugger.SendCommand('dir', '"' + StringReplace(LibDir[i], '\', '/', [rfReplaceAll]) + '"');

    // Add include folders
    for I := 0 to CDir.Count - 1 do
      fDebugger.SendCommand('dir', '"' + StringReplace(CDir[i], '\', '/', [rfReplaceAll]) + '"');

    // Add more include folders, duplicates will be added/moved to front of list
    for I := 0 to CppDir.Count - 1 do
      fDebugger.SendCommand('dir', '"' + StringReplace(CppDir[i], '\', '/', [rfReplaceAll]) + '"');
  end;

  // Add breakpoints and watch vars
  for i := 0 to fDebugger.WatchVarList.Count - 1 do
    fDebugger.AddWatchVar(i);

  for i := 0 to fDebugger.BreakPointList.Count - 1 do
    fDebugger.AddBreakpoint(i);

  // Run the debugger
  fDebugger.SendCommand('set', 'width 0'); // don't wrap output, very annoying
  fDebugger.SendCommand('set', 'new-console on');
  fDebugger.SendCommand('set', 'confirm off');
  fDebugger.SendCommand('cd', ExcludeTrailingPathDelimiter(ExtractFileDir(filepath))); // restore working directory
  if not hasBreakPoint then begin
    case GetCompileTarget of
      cttNone:
        Exit;
      cttFile:
      begin
        params := '';
        if fCompiler.UseRunParams then
          params := params + ' ' + fCompiler.RunParams;
        if fCompiler.UseInputFile then
          params := params + ' < "' + fCompiler.InputFile + '"';
        fDebugger.SendCommand('start', params);
        UpdateDebugInfo;
      end;
      cttProject:  begin
        params := '';
        if fCompiler.UseRunParams then
          params := params + ' ' + fProject.Options.CmdLineArgs;
        if fCompiler.UseInputFile then
          params := params + ' < "' + fCompiler.InputFile + '"';

        fDebugger.SendCommand('start', params);
        UpdateDebugInfo;
      end;
    end;
  end else begin
    case GetCompileTarget of
      cttNone:
        Exit;
      cttFile: begin
        params := '';
        if fCompiler.UseRunParams then
          params := params + ' ' + fCompiler.RunParams;
        if fCompiler.UseInputFile then
          params := params + ' < "' + fCompiler.InputFile + '"';
        fDebugger.SendCommand('run', params);
        UpdateDebugInfo;
      end;
      cttProject: begin
        params := '';
        if fCompiler.UseRunParams then
          params := params + ' ' + fProject.Options.CmdLineArgs;
        if fCompiler.UseInputFile then
          params := params + ' < "' + fCompiler.InputFile + '"';

        fDebugger.SendCommand('run', params);
        UpdateDebugInfo;
      end;
    end;
  end;
end;

procedure TMainForm.actEnviroOptionsExecute(Sender: TObject);
begin
  with TEnviroForm.Create(nil) do try
    if ShowModal = mrOk then begin
      // Update pagecontrol
      EditorList.SetPreferences(devData.MsgTabs, devData.MultiLineTab);

      // Update fullscreen options
      if devData.FullScreen then
        ToolbarDock.Visible := devData.ShowBars;

      // Pick new language if we have to (long process)
      if devData.LangChange then begin
        Lang.SetLang(devData.Language);
        LoadText;
      end;

      // Load new icons, also only if we have to
      if devData.ThemeChange then
        Loadtheme;
      Shortcuts.Filename := devDirs.Config + DEV_SHORTCUTS_FILE;

      // Rebuild recent file list (max count could have changed
      dmMain.RebuildMRU;
    end;
  finally
    Close;
  end;
end;

procedure TMainForm.actUpdatePageCount(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fEditorList.PageCount > 0;
end;

procedure TMainForm.actUpdateDeleteWatch(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := Assigned(WatchView.Selected);
end;

procedure TMainForm.actUpdatePageorProject(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := Assigned(fProject) or (fEditorList.PageCount > 0);
end;

procedure TMainForm.actDebugExecuteUpdate(Sender: TObject);
var
  e:TEditor;
begin
  case GetCompileTarget of
    cttFile: begin
        e:=EditorList.GetEditor();
        if not Assigned(e) then
          Exit;
        TCustomAction(Sender).Enabled :=
         ( (e.New and e.Text.Modified) or IsCfile(e.FileName)) and (not fCompiler.Compiling)
          and Assigned(devCompilerSets.CompilationSet) and (not fDebugger.Executing);
//          and (not devExecutor.Running);
      end;
    cttProject: begin
        TCustomAction(Sender).Enabled := (fProject.Options.typ <> dptStat) and (not fCompiler.Compiling)
          and Assigned(devCompilerSets.CompilationSet) and (not fDebugger.Executing);
//          and (not devExecutor.Running);
      end;
    else begin
      TCustomAction(Sender).Enabled:=False;
    end;
  end;
end;

procedure TMainForm.actUpdateProject(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := assigned(fProject);
end;

procedure TMainForm.actUpdateMakeFile(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := assigned(fProject) and (not fCompiler.Compiling) and (GetCompileTarget <> cttNone) and
    Assigned(devCompilerSets.CompilationSet);
end;

procedure TMainForm.actUpdateEmptyEditor(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and e.Text.Focused and not e.Text.IsEmpty;
end;

procedure TMainForm.actUpdateDebuggerRunning(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fDebugger.Executing and (not fDebugger.Reader.CommandRunning);
end;

procedure TMainForm.actUpdateDebuggerRunningCPU(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fDebugger.Executing and not Assigned(CPUForm);
end;

procedure TMainForm.actUpdateEmptyEditorFindForm(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and (not Assigned(FindForm) or not FindForm.Showing);
end;

{
procedure TMainForm.actRunUpdate(Sender: TObject);
var
  e:TEditor;
begin
  TCustomAction(Sender).Enabled:=False;
  case GetCompileTarget of
    ctFile: begin
      TCustomAction(Sender).Enabled := IsCfile(e.FileName) and (not fCompiler.Compiling)
        and (not fDebugger.Executing)  and (not devExecutor.Running);
      end;
    ctProject: begin
      TCustomAction(Sender).Enabled := (fProject.Options.typ <> dptStat) and (not
        fCompiler.Compiling) and (not fDebugger.Executing) and (not devExecutor.Running)
      end;
  end;
end;
}
{
procedure TMainForm.actCompileRunUpdate(Sender: TObject);
var
  e:TEditor;
begin
  TCustomAction(Sender).Enabled:=False;
  case GetCompileTarget of
    ctFile: begin
        TCustomAction(Sender).Enabled := IsCfile(e.FileName) and (not fCompiler.Compiling)
          and Assigned(devCompilerSets.CompilationSet) and (not fDebugger.Executing)
          and (not devExecutor.Running);
      end;
    ctProject: begin
        TCustomAction(Sender).Enabled := (fProject.Options.typ <> dptStat) and (not fCompiler.Compiling)
          and Assigned(devCompilerSets.CompilationSet) and (not fDebugger.Executing)
          and (not devExecutor.Running);
      end;
  end;
end;
}
procedure TMainForm.ToolbarDockClick(Sender: TObject);
begin
  tbMain.Visible := ToolMainItem.Checked;
  tbEdit.Visible := ToolEditItem.Checked;
  tbCompile.Visible := ToolCompileandRunItem.Checked;
  tbProject.Visible := ToolProjectItem.Checked;
  tbSpecials.Visible := ToolSpecialsItem.Checked;
  tbSearch.Visible := ToolSearchItem.Checked;
  tbCompilers.Visible := ToolCompilersItem.Checked;
  tbDebug.Visible := ToolDebugItem.Checked;
  tbUndo.Visible := ToolUndoItem.Checked;

  devData.ToolbarMain := ToolMainItem.Checked;
  devData.ToolbarEdit := ToolEditItem.Checked;
  devData.ToolbarCompile := ToolCompileandRunItem.Checked;
  devData.ToolbarProject := ToolProjectItem.Checked;
  devData.ToolbarSpecials := ToolSpecialsItem.Checked;
  devData.ToolbarSearch := ToolSearchItem.Checked;
  devData.ToolbarCompilers := ToolCompilersItem.Checked;
  devData.ToolbarDebug := ToolDebugItem.Checked;
  devData.ToolbarUndo := ToolUndoItem.Checked;
end;

procedure TMainForm.ToolbarDockContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
begin
  pt := ToolbarDock.ClientToScreen(MousePos);
  TrackPopupMenu(ToolbarsItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON,
    pt.x, pt.y, 0, Self.Handle, nil);
  Handled := TRUE;
end;

procedure TMainForm.SplitterBottomMoved(Sender: TObject);
begin
  fPreviousHeight := MessageControl.Height;
end;

procedure TMainForm.actProjectMakeFileExecute(Sender: TObject);
begin
  // Fake a project compilation step
  ClearCompileMessages;
  fCompiler.Target := cttProject;
  fCompiler.Project := fProject;
  fCompiler.CompilerSet := devCompilerSets.CompilationSet;
  fCompiler.BuildMakeFile;

  // Show the results
  OpenFile(fCompiler.MakeFile, etAuto)
end;

procedure TMainForm.actMsgCutExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    3: begin
        if EvaluateInput.Focused then begin
          Clipboard.AsText := EvaluateInput.SelText;
          EvaluateInput.SelText := '';
        end else if txtLocals.Focused then begin
          Clipboard.AsText := txtLocals.SelText;
          txtLocals.SelText := '';
        end;
      end;
  end;
end;

procedure TMainForm.actMsgCopyExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    0:
      Clipboard.AsText := GetPrettyLine(CompilerOutput);
    1:
      Clipboard.AsText := GetPrettyLine(ResourceOutput);
    2: begin
        if LogOutput.Focused then
          LogOutput.CopyToClipboard;
      end;
    3: begin
        if EvaluateInput.Focused then
          Clipboard.AsText := EvaluateInput.SelText
        else if EvalOutput.Focused then
          EvalOutput.CopyToClipboard
        else if txtLocals.Focused then
          Clipboard.AsText := txtLocals.SelText
        else if DebugOutput.Focused then
          DebugOutput.CopyToClipboard;
      end;
    4: begin
      {
      Clipboard.AsText := GetPrettyLine(FindOutput);
      }
    end;
  end;
end;

procedure TMainForm.actMsgCopyAllExecute(Sender: TObject);
var
  i: integer;
begin
  case MessageControl.ActivePageIndex of
    0: begin
        ClipBoard.AsText := '';
        for i := 0 to pred(CompilerOutput.Items.Count) do
          Clipboard.AsText := Clipboard.AsText + GetPrettyLine(CompilerOutput, i) + #13#10;
      end;
    1: begin
        ClipBoard.AsText := '';
        for i := 0 to pred(ResourceOutput.Items.Count) do
          Clipboard.AsText := Clipboard.AsText + GetPrettyLine(ResourceOutput, i) + #13#10;
      end;
    2: begin
      Clipboard.AsText := LogOutput.Text;
      end;
    3: begin
        if EvaluateInput.Focused then
          Clipboard.AsText := EvaluateInput.Text
        else if EvalOutput.Focused then
          Clipboard.AsText := EvalOutput.Text
        else if txtLocals.Focused then
          Clipboard.AsText := txtLocals.Text
        else if DebugOutput.Focused then
          Clipboard.AsText := DebugOutput.Text
      end;
    4: begin
    {
        ClipBoard.AsText := '';
        for i := 0 to pred(FindOutput.Items.Count) do
          Clipboard.AsText := Clipboard.AsText + GetPrettyLine(FindOutput, i) + #13#10;
    }
      end;
  end;
end;

procedure TMainForm.actMsgPasteExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    3: begin
        if EvaluateInput.Focused then
          EvaluateInput.SelText := ClipBoard.AsText;
      end;
  end;
end;

procedure TMainForm.actMsgSelAllExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    2: begin
        if LogOutput.Focused then
          LogOutput.SelectAll;
      end;
    3: begin
        if EvaluateInput.Focused then
          EvaluateInput.SelectAll
        else if EvalOutput.Focused then
          EvalOutput.SelectAll
        else if txtLocals.Focused then
          txtLocals.SelectAll
        else if DebugOutput.Focused then
          DebugOutput.SelectAll;
      end;
  end;
end;

procedure TMainForm.actMsgSaveAllExecute(Sender: TObject);
var
  i: integer;
  fulloutput: AnsiString;
  Stream: TFileStream;
  e: TEditor;
begin
  fulloutput := '';
  with TSaveDialog.Create(self) do try
    case MessageControl.ActivePageIndex of
      0: begin
          FileName := 'Formatted Compiler Output';
          for i := 0 to CompilerOutput.Items.Count - 1 do
            fulloutput := fulloutput + GetPrettyLine(CompilerOutput, i) + #13#10;
        end;
      1: begin
          FileName := 'Resource Error Log';
          for i := 0 to ResourceOutput.Items.Count - 1 do
            fulloutput := fulloutput + GetPrettyLine(ResourceOutput, i) + #13#10;
        end;
      2: begin
          FileName := 'Raw Build Log';
          if Length(LogOutput.Text) > 0 then
            fulloutput := LogOutput.Text;
        end;
      3: begin
          FileName := 'Raw GDB Output';
          if Length(DebugOutput.Text) > 0 then
            fulloutput := DebugOutput.Text;
        end;
      4: begin
          FileName := 'Find Results';
      {
          for i := 0 to FindOutput.Items.Count - 1 do
            fulloutput := fulloutput + GetPrettyLine(FindOutput, i) + #13#10;
      }
        end;
    end;

    if Length(fulloutput) > 0 then begin

      Title := Lang[ID_NV_SAVEFILE];
      Filter := BuildFilter([FLT_TEXTS]);
      DefaultExt := 'txt';
      FilterIndex := 1;
      Options := Options + [ofOverwritePrompt];

      if Assigned(fProject) then begin
        InitialDir := fProject.Directory;
      end else begin
        e := fEditorList.GetEditor;
        if Assigned(e) then
          InitialDir := ExtractFilePath(e.FileName)
        else
          InitialDir := 'C:\';
      end;

      if Execute then begin
        Stream := TFileStream.Create(FileName, fmCreate);
        try
          Stream.Write(fulloutput[1], Length(fulloutput));
        finally
          Stream.Free;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actMsgClearExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    0:
      CompilerOutput.Items.Clear;
    1:
      ResourceOutput.Items.Clear;
    2:
      LogOutput.Clear;
    3: begin
        if EvaluateInput.Focused then
          EvaluateInput.Clear
        else if EvalOutput.Focused then
          EvalOutput.Clear
        else if DebugOutput.Focused then
          DebugOutput.Clear;
      end;
    4: begin
        FindOutput.Clear;
      end;
  end;
end;

procedure TMainForm.actMsgHideExecute(Sender: TObject);
begin
  OpenCloseMessageSheet(MessageControl.Height <> fPreviousHeight);
end;

procedure TMainForm.actIncrementalExecute(Sender: TObject);
var
  pt: TPoint;
  e: TEditor;
  PageControl: ComCtrls.TPageControl;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin

    // Only create the form when we need to do so
    if not Assigned(IncrementalForm) then
      IncrementalForm := TIncrementalForm.Create(self);

    PageControl := e.PageControl;
    pt := ClienttoScreen(point(PageControl.Left + PageControl.Width - IncrementalForm.Width - 10, PageControl.Top));

    IncrementalForm.Left := pt.x;
    IncrementalForm.Top := pt.y;
    IncrementalForm.editor := e.Text;
    IncrementalForm.Show;
  end;
end;

procedure TMainForm.CompilerOutputDblClick(Sender: TObject);
var
  Col, Line: integer;
  selection: TListItem;
  errorfiletab: TEditor;
begin
  selection := TListView(Sender).Selected; // used by compile and resource lists
  if Assigned(selection) then begin
    Line := StrToIntDef(selection.Caption, -1);
    if Line<=0 then
      Exit;
    Col := StrToIntDef(selection.SubItems[0], 1);
    errorfiletab := fEditorList.GetEditorFromFileName(selection.SubItems[1]);

    if Assigned(errorfiletab) then begin
      errorfiletab.SetErrorFocus(Col, Line);
      errorfiletab.Activate;
    end;
  end;
end;

procedure TMainForm.FindOutputDblClick(Sender: TObject);
var
  col, line: integer;
  e: TEditor;
  selected: TTreeNode;
begin
  selected := FindOutPut.Selected;
  if Assigned(selected) and (selected.Level = 2) and assigned(selected.Data) then begin
    Col := PFindITem(selected.Data)^.char;
    Line := PFindITem(selected.Data)^.line;

    // And open up
    e := fEditorList.GetEditorFromFileName(PFindITem(selected.Data)^.filename);
    if Assigned(e) then begin

      // Position the caret
      // uncollapse folds if needed
      e.SetCaretPosAndActivate(Line, Col);

      // Then select the word we searched for
      e.Text.SetSelWord;
      e.Text.CaretXY := e.Text.BlockBegin;
    end;
  end;
end;

procedure TMainForm.actShowBarsExecute(Sender: TObject);
begin
  if devData.FullScreen then
    ToolbarDock.Visible := not ToolbarDock.Visible;
end;

procedure TMainForm.FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
begin
  pt := ClientToScreen(MousePos);
  TrackPopupMenu(ViewMenu.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.x, pt.y, 0, Self.Handle, nil);
  Handled := TRUE;
end;

procedure TMainForm.actAddWatchExecute(Sender: TObject);
var
  s: AnsiString;
  e: TEditor;
begin
  s := '';
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    if e.Text.SelAvail then
      s := e.Text.SelText
    else begin
      s := e.Text.WordAtCursor;
      if not ShowInputQuery(Lang[ID_NV_ADDWATCH], Lang[ID_NV_ENTERVAR], s) then
        Exit;
    end;
    s:=Trim(s);
    if s <> '' then
      fDebugger.AddWatchVar(s);
  end;
end;

procedure TMainForm.actStepOverExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    WatchView.Items.BeginUpdate();
    fDebugger.InvalidateAllVars;
    fDebugger.SendCommand('next', '');
    UpdateDebugInfo;
    if assigned(CPUForm) then
      CPUForm.UpdateInfo;
    WatchView.Items.EndUpdate();
    //fDebugger.RefreshWatchVars;
  end;
end;

procedure TMainForm.actStepIntoExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    WatchView.Items.BeginUpdate();
    fDebugger.InvalidateAllVars;
    fDebugger.SendCommand('step', '');
    UpdateDebugInfo;

    if assigned(CPUForm) then
      CPUForm.UpdateInfo;
    WatchView.Items.EndUpdate();
    //fDebugger.RefreshWatchVars;
  end;
end;

procedure TMainForm.actRemoveWatchExecute(Sender: TObject);
var
  node: TTreeNode;
begin
  node := WatchView.Selected;
  if Assigned(node) then begin

    // Retrieve topmost node
    while Assigned(node.Parent) do
      node := node.Parent;

    fDebugger.RemoveWatchVar(node);
  end;
end;

procedure TMainForm.RemoveActiveBreakpoints;
var
  i: integer;
begin
  for i := 0 to EditorList.PageCount - 1 do
    fEditorList[i].RemoveBreakpointFocus;
end;

procedure TMainForm.GotoBreakpoint(const FileName: AnsiString; Line: integer; setFocus:boolean);
var
  e: TEditor;
begin
  // Remove line focus in files left behind
  RemoveActiveBreakpoints;

  // Then active the current line in the current file
  e := fEditorList.GetEditorFromFileName(StringReplace(FileName, '/', '\', [rfReplaceAll]));
  if Assigned(e) then begin
    e.SetActiveBreakpointFocus(Line,setFocus);
    //e.SetActiveBreakpointFocus(Line);
    //e.Activate;
  end;
//  if setFocus then
    Application.BringToFront;
end;

procedure TMainForm.actContinueExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    WatchView.Items.BeginUpdate();
    RemoveActiveBreakpoints;
    fDebugger.InvalidateAllVars;
    fDebugger.SendCommand('continue', '');
    UpdateDebugInfo;
    if assigned(CPUForm) then
      CPUForm.UpdateInfo;
    WatchView.Items.EndUpdate();
        //fDebugger.RefreshWatchVars;
  end;
end;

procedure TMainForm.actStopExecuteExecute(Sender: TObject);
begin
  if assigned(CPUForm) then
    CPUForm.Close;
  if fDebugger.Executing then
    fDebugger.Stop
  else if devExecutor.Running then
    devExecutor.Reset;
end;

procedure TMainForm.actUndoUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actUndo.Enabled := assigned(e) and e.Text.CanUndo;
end;

procedure TMainForm.actRedoUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actRedo.enabled := assigned(e) and e.Text.CanRedo;
end;

procedure TMainForm.actCutUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actCut.Enabled := assigned(e) and e.Text.SelAvail;
end;

procedure TMainForm.actCopyUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TAction(Sender).Enabled := Assigned(e) and e.Text.Focused and e.Text.SelAvail;
end;

procedure TMainForm.actPasteUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actPaste.Enabled := Assigned(e) and e.Text.Focused and e.Text.CanPaste;
end;

procedure TMainForm.actSaveUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actSave.Enabled := Assigned(e) and (e.Text.Modified or (e.FileName = ''));
end;

procedure TMainForm.actSaveAsUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actSaveAs.Enabled := Assigned(e);
end;

procedure TMainForm.ClearCompileMessages;
begin
  CompilerOutput.Clear;
  ResourceOutput.Clear;
  LogOutput.Clear;
  fLogOutputRawData.Clear;
  DebugOutput.Clear;
end;

procedure TMainForm.ClearMessageControl;
begin
//  FindOutput.Clear; // don't clear this when compiling...
  ClearCompileMessages;
end;

procedure TMainForm.actFileMenuExecute(Sender: TObject);
begin
  //	dummy event to keep menu active
end;

procedure TMainForm.actToolsMenuExecute(Sender: TObject);
var
  I, J: integer;
begin
  for I := (ToolsMenu.IndexOf(PackageManagerItem) + 2) to ToolsMenu.Count - 1 do begin
    J := ToolsMenu.Items[I].tag;
    ToolsMenu.Items[I].Enabled := FileExists(ParseToolParams(fTools.ToolList[J]^.Exec));
    if not ToolsMenu.Items[I].Enabled then
      ToolsMenu.Items[I].Caption := fTools.ToolList[J]^.Title + ' (Tool not found)';
  end;
end;

procedure TMainForm.UpdateCompilerList;
var
  I: integer;
begin
  cmbCompilers.Items.BeginUpdate;
  try
    cmbCompilers.Clear;

    // Populate list
    for I := 0 to devCompilerSets.Count - 1 do
      cmbCompilers.Items.Add(devCompilerSets[i].Name);

    // Select current compiler
    cmbCompilers.ItemIndex := devCompilerSets.CompilationSetIndex;

    // Mention change to compiler sets
    //devCompilerSets.OnCompilerSetChanged(
    //  devCompilerSets[fOldCompilerToolbarIndex],
    //  devCompilerSets[cmbCompilers.ItemIndex]);

    // Remember current index
    fOldCompilerToolbarIndex := cmbCompilers.ItemIndex;
  finally
    cmbCompilers.Items.EndUpdate;
  end;
end;

procedure TMainForm.UpdateClassBrowsing;
begin
  //ResetCppParser(CppParser);

  //actCodeCompletionAlt.Enabled := devCodeCompletion.UseAltSlash;
  //actCodeCompletion.Enabled := not devCodeCompletion.UseAltSlash;
  if devCodeCompletion.UseAltSlash then
    actCodeCompletion.ShortCut := 32959 { TextToShortcut('Alt+/') is 32879, which / is the one on the numpad}
  else
    actCodeCompletion.ShortCut := TextToShortCut('Ctrl+Space');

  // Configure code completion
  CodeCompletion.Width := devCodeCompletion.Width;
  CodeCompletion.Height := devCodeCompletion.Height;

  HeaderCompletion.Width := devCodeCompletion.Width;
  HeaderCompletion.Height := devCodeCompletion.Height;

  // Only attempt to redraw once
  ClassBrowser.BeginTreeUpdate;
  try
    ClassBrowser.ShowInheritedMembers := devClassBrowsing.ShowInheritedMembers;
    ClassBrowser.SortByType := devClassBrowsing.SortByType;
    ClassBrowser.SortAlphabetically := devClassBrowsing.SortAlphabetically;
    ClassBrowser.TabVisible := (LeftPageControl.ActivePage = LeftClassSheet);
  finally
    ClassBrowser.EndTreeUpdate;
  end;

  // Configure class browser actions
  actBrowserShowInherited.Checked := ClassBrowser.ShowInheritedMembers;
  actBrowserSortByType.Checked := ClassBrowser.SortByType;
  actBrowserSortAlphabetically.Checked := ClassBrowser.SortAlphabetically;
end;

procedure TMainForm.SetCppParserProject(Parser:TCppParser; Project:TProject);
var
  i:integer;
begin
  if not Assigned(Project) then begin
    Exit;
  end;
  Parser.ClearProjectFiles;
  Parser.ClearProjectIncludePaths;
  for I := 0 to Project.Units.Count - 1 do
    Parser.AddFileToScan(Project.Units[I].FileName, True);
  for I := 0 to Project.Options.Includes.Count - 1 do
    Parser.AddProjectIncludePath(Project.Options.Includes[I]);
end;

procedure TMainForm.ScanActiveProject(parse:boolean);
begin
  //UpdateClassBrowsing;
  if Assigned(fProject) and parse then begin
    ResetCppParser(fProject.CppParser);
    SetCppParserProject(fProject.CppParser,fProject);
    ParseFileList(Project.CppParser);
  end else begin
    SetCppParserProject(fProject.CppParser,fProject);
  end;
end;

procedure TMainForm.ClassBrowserSelect(Sender: TObject; Filename: TFileName; Line: Integer);
var
  e: TEditor;
begin
  e := fEditorList.GetEditorFromFilename(FileName);
  if Assigned(e) then begin
    e.SetCaretPosAndActivate(Line, 1);
  end;
  ClassBrowser.SetFocus;
end;

procedure TMainForm.CodeCompletionResize(Sender: TObject);
begin
  devCodeCompletion.Width := CodeCompletion.Width;
  devCodeCompletion.Height := CodeCompletion.Height;
end;

procedure TMainForm.actSwapHeaderSourceExecute(Sender: TObject);
var
  FromEditor, ToEditor: TEditor;
  CFile, HFile, ToFile: AnsiString;
  iscfile, ishfile: boolean;
begin
  FromEditor := fEditorList.GetEditor;
  if not Assigned(FromEditor) then
    Exit;

  iscfile := CBUtils.IsCfile(FromEditor.FileName);
  ishfile := CBUtils.IsHfile(FromEditor.FileName);

  CBUtils.GetSourcePair(FromEditor.FileName, CFile, HFile);
  if iscfile then begin
    ToFile := HFile
  end else if ishfile then begin
    ToFile := CFile
  end else begin
    Exit; // don't know file type, don't bother searching
  end;

  // Fetch the file we want to go to
  ToEditor := fEditorList.FileIsOpen(ToFile);
  if Assigned(ToEditor) then
    ToEditor.Activate
  else begin
    // We need to open a new editor
    ToEditor := fEditorList.GetEditorFromFileName(ToFile);
    if Assigned(ToEditor) then begin

      // Move the editors next to each other if possible
      // Do not try to do so if they are visible in separate editors
      if FromEditor.PageControl = ToEditor.PageControl then begin

        // Move source file to the left of the header file
        if iscfile then begin
          ToEditor.TabSheet.PageIndex := min(FromEditor.PageControl.PageCount - 1, FromEditor.TabSheet.PageIndex + 1);
        end else begin
          ToEditor.TabSheet.PageIndex := max(0, FromEditor.TabSheet.PageIndex);
        end;
      end;
      ToEditor.Activate;
    end else if iscfile then begin
      SetStatusBarMessage(Lang[ID_MSG_CORRESPONDINGHEADER]);
    end else if ishfile then begin
      SetStatusBarMessage(Lang[ID_MSG_CORRESPONDINGSOURCE]);
    end;
  end;
end;

procedure TMainForm.actSyntaxCheckExecute(Sender: TObject);
begin
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForCompile then
    Exit;
  fCompiler.CheckSyntax;
end;

procedure TMainForm.EditorSaveTimer(Sender: TObject);
var
  e: TEditor;
  i: integer;
  EditorSaveList: TList;
  NewFileName: AnsiString;
begin
  EditorSaveList := TList.Create;
  try
    // Assemble a list of files that need saving
    case devEditor.AutoSaveFilter of
      0: begin // Save current file
          e := fEditorList.GetEditor;
          if Assigned(e) and not e.New then // don't save untitled files
            EditorSaveList.Add(e);
        end;
      1: begin // Save all open files
          for I := 0 to EditorList.PageCount - 1 do begin
            e := fEditorList[i];
            if Assigned(e) and not e.New then
              EditorSaveList.Add(e);
          end;
        end;
      2: begin // Save all project files
          for I := 0 to EditorList.PageCount - 1 do begin
            e := fEditorList[i];
            if Assigned(e) and e.InProject and not e.New then
              EditorSaveList.Add(e);
          end;
        end;
    end;

    // Then process the list
    // For each file, get original filename, determine copy filename, and save the copy
    for I := 0 to EditorSaveList.Count - 1 do begin
      e := TEditor(EditorSaveList[i]);
      case devEditor.AutoSaveMode of
        0: begin // overwrite (standard save)
            if e.Text.Modified and e.Save then
              SetStatusbarMessage(Format(Lang[ID_AUTOSAVEDFILE], [e.FileName]));
          end;
        1: begin // append UNIX timestamp (backup copy, don't update class browser)
            NewFileName := ChangeFileExt(e.FileName, '.' + IntToStr(DateTimeToUnix(Now)) + ExtractFileExt(e.FileName));
            e.SaveFile(NewFileName);
            SetStatusbarMessage(Format(Lang[ID_AUTOSAVEDFILEAS], [e.FileName, NewFileName]));
          end;
        2: begin // append formatted timestamp (backup copy, don't update class browser)
            NewFileName := ChangeFileExt(e.FileName, '.' + FormatDateTime('yyyy mm dd hh mm ss', Now) +
              ExtractFileExt(e.FileName));
            e.SaveFile(NewFileName);
            SetStatusbarMessage(Format(Lang[ID_AUTOSAVEDFILEAS], [e.FileName, NewFileName]));
          end;
      end;
    end;
  finally
    // Do not save members.
    EditorSaveList.Free;
  end;
end;

procedure TMainForm.EditorPageControlChange(Sender: TObject);
var
  e: TEditor;
  PageControl: TPageControl;
  I: integer;
begin
  // Determine who sent this message
  PageControl := TPageControl(Sender);
  e := fEditorList.GetEditor(-1, PageControl);
  if Assigned(e) then begin
    // Update focus so user can keep typing
    if e.Text.CanFocus then // TODO: can fail for some reason
      e.Text.SetFocus; // this should trigger then OnEnter even of the Text control
    // UpdateClassBrowserForEditor(e);
  end else begin
    // No editors are visible
    // Set title bar to current file
    UpdateAppTitle;

    // Set Parser to nil;
    ClassBrowser.Parser := nil;
    // Set classbrowser to current file
    ClassBrowser.CurrentFile := '';

    // Set compiler selector to current file
    UpdateCompilerList;

    // Update status bar
    SetStatusbarLineCol;

    // Update bookmark menu
    for i := 1 to 9 do begin
      TogglebookmarksPopItem.Items[i - 1].Checked := false;
      TogglebookmarksItem.Items[i - 1].Checked := false;
    end;

    // Update focus of incremental search
    if Assigned(IncrementalForm) and IncrementalForm.Showing then
      IncrementalForm.Close;
  end;
end;

procedure TMainForm.actConfigdevShortcutsExecute(Sender: TObject);
begin
  Shortcuts.Edit(
    Lang[ID_SC_CAPTION], // window
    Lang[ID_SC_HDRENTRY], // column 1
    Lang[ID_SC_HDRSHORTCUT], // column 2
    Lang[ID_SC_NEWTIP], // hint
    Lang[ID_BTN_OK], // OK button
    Lang[ID_BTN_CANCEL], // Cancel button
    Lang[ID_SC_RESETALL], // Reset All button
    Lang[ID_SC_RESETCURRENT], // Reset Current button
    Lang[ID_SC_NEWREPLACEHINT], // messagebox text when replacing
    Lang[ID_SC_CONFIRMALL], // messagebox text when setting to defaults
    Lang[ID_SC_CONFIRMONE], // messagebox text when setting to defaults
    Lang[ID_SC_BUTTON]); // Button/Misc
end;

procedure TMainForm.DateTimeMenuItemClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.InsertString(FormatDateTime('dd/mm/yy hh:nn', Now), TRUE);
end;

procedure TMainForm.CommentheaderMenuItemClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    e.InsertString('/*' + #13#10 +
      '	Name: ' + #13#10 +
      '	Copyright: ' + #13#10 +
      '	Author: ' + #13#10 +
      '	Date: ' + FormatDateTime('dd/mm/yy hh:nn', Now) + #13#10 +
      '	Description: ' + #13#10 +
      '*/' + #13#10, true);
  end;
end;

procedure TMainForm.EditorPageControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
var
  PageIndex: integer;
  SenderPageControl: TPageControl;
  e: TEditor;
  rect,closeRect:TRect;
  size: integer;
begin
  SenderPageControl := TPageControl(Sender);
  if Button = mbRight then begin // select new tab even with right mouse button
    PageIndex := SenderPageControl.IndexOfTabAt(X, Y);
    if PageIndex <> -1 then begin
      SenderPageControl.ActivePageIndex := PageIndex;
      SenderPageControl.OnChange(SenderPageControl);
    end;
  end else if Button = mbMiddle then begin
    PageIndex := SenderPageControl.IndexOfTabAt(X, Y);
    if PageIndex <> -1 then begin
      e := fEditorList.GetEditor(PageIndex, SenderPageControl);
      if Assigned(e) then
        fEditorList.CloseEditor(e);
    end;
  end else begin// see if it's a drag operation
    PageIndex := SenderPageControl.IndexOfTabAt(X, Y);
    if PageIndex <> -1 then begin
      rect := SenderPageControl.TabRect(PageIndex);
      closeRect.Bottom := Rect.Bottom;
      closeRect.Top := Rect.Top;
      size := Rect.Bottom - Rect.Top - 4;
      if (size>20) then
        size := 20;
      closeRect.Top := Rect.Top + (Rect.Bottom - Rect.Top - size) div 2;
      closeRect.Bottom := closeRect.Top + size;
      closeRect.Right := Rect.Right - 5;
      closeRect.Left := closeRect.Right - size;
      if (X>=closeRect.Left) and (X<=closeRect.Right)
        and (Y>=closeRect.Top) and (Y<=closeRect.Bottom) then begin
        e := fEditorList.GetEditor(PageIndex, SenderPageControl);
        if Assigned(e) then
          fEditorList.CloseEditor(e);
        Exit;
      end;
    end;
    SenderPageControl.Pages[SenderPageControl.ActivePageIndex].BeginDrag(False);
  end;
end;

procedure TMainForm.actNewTemplateUpdate(Sender: TObject);
begin
  actNewTemplate.Enabled := Assigned(fProject);
end;

procedure TMainForm.actCommentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(ecComment, #0, nil);
end;

procedure TMainForm.actUncommentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(ecUncomment, #0, nil);
end;

procedure TMainForm.actToggleCommentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) and e.Text.Focused then
    e.Text.CommandProcessor(ecToggleComment, #0, nil);
end;

procedure TMainForm.actIndentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) and e.Text.Focused then
    e.IndentSelection;
end;

procedure TMainForm.actUnindentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) and e.Text.Focused then
    e.UnindentSelection;
end;

procedure TMainForm.EditorPageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  I: integer;
  SenderPageControl: TPageControl;
begin
  SenderPageControl := TPageControl(Sender);
  I := SenderPageControl.IndexOfTabAt(X, Y);
  if (Source is TTabSheet) and (I <> SenderPageControl.ActivePageIndex) then
    SenderPageControl.Pages[SenderPageControl.ActivePageIndex].PageIndex := I;
end;

procedure TMainForm.EditorPageControlDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept:
  Boolean);
var
  I: integer;
  SenderPageControl: TPageControl;
begin
  SenderPageControl := TPageControl(Sender);
  I := SenderPageControl.IndexOfTabAt(X, Y);
  Accept := (Source is TTabSheet) and (I <> SenderPageControl.ActivePageIndex);
end;

procedure TMainForm.actGotoFunctionExecute(Sender: TObject);
var
  e: TEditor;
  st: PStatement;
begin
  e := fEditorList.GetEditor;
  if not Assigned(e) then
    Exit;

  with TFunctionSearchForm.Create(Self) do try

    fFileName := e.FileName;
    fParser := GetCppParser;

    if ShowModal = mrOK then begin
      st := PStatement(lvEntries.Selected.Data);
      if Assigned(st) then begin
        if st^._HasDefinition then
          e.SetCaretPosAndActivate(st^._DefinitionLine, 1)
        else
          e.SetCaretPosAndActivate(st^._Line, 1);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actBrowserGotoDeclarationUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := (ClassBrowser.SelectedKind <> skUnknown)
end;

procedure TMainForm.actBrowserGotoDefinitionUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := (ClassBrowser.SelectedKind <> skUnknown)
end;

procedure TMainForm.actBrowserNewClassUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(fProject);
end;

procedure TMainForm.actBrowserNewMemberUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := (ClassBrowser.SelectedKind = skClass)
end;

procedure TMainForm.actBrowserNewVarUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := (ClassBrowser.SelectedKind = skClass)
end;

procedure TMainForm.actBrowserGotoDeclarationExecute(Sender: TObject);
var
  Editor: TEditor;
  FileName: AnsiString;
  Line: integer;
begin
  FileName := ClassBrowser.SelectedFile;
  Line := ClassBrowser.SelectedLine;
  if FileName = '' then
    Exit;
  Editor := fEditorList.GetEditorFromFileName(FileName);
  if Assigned(Editor) then begin
    Editor.SetCaretPosAndActivate(Line, 1);
  end;
end;

procedure TMainForm.actBrowserGotoDefinitionExecute(Sender: TObject);
var
  Editor: TEditor;
  FileName: AnsiString;
  Line: integer;
begin
  FileName := ClassBrowser.SelectedDefFile;
  Line := ClassBrowser.SelectedDefLine;
  if FileName = '' then
    Exit;
  Editor := fEditorList.GetEditorFromFileName(FileName);
  if Assigned(Editor) then begin
    Editor.SetCaretPosAndActivate(Line, 1);
  end;
end;

procedure TMainForm.actBrowserNewClassExecute(Sender: TObject);
begin
  with TNewClassForm.Create(Self) do
    ShowModal;
end;

procedure TMainForm.actBrowserNewMemberExecute(Sender: TObject);
begin
  with TNewFunctionForm.Create(Self) do try
    ShowModal;
  finally
    Close;
  end;
end;

procedure TMainForm.actBrowserNewVarExecute(Sender: TObject);
begin
  with TNewVarForm.Create(Self) do try
    ShowModal;
  finally
    Close;
  end;
end;

procedure TMainForm.UpdateClassBrowserForEditor(e:TEditor);
begin
  if not Assigned(e) then begin
    ClassBrowser.Parser := nil;
    ClassBrowser.CurrentFile:='';
    Exit;
  end;
  if self.fQuitting then
    Exit;
  if not devCodeCompletion.Enabled then
    Exit;
  if (ClassBrowser.CurrentFile = e.FileName)
    and (ClassBrowser.Parser = e.CppParser) then begin
    Exit;

  end;
  {
   else if not assigned(fProject) then begin
    UpdateClassBrowsing;
  end else begin
    p1:=fProject.Units.IndexOf(ClassBrowser.CurrentFile);
    p2:=fProject.Units.IndexOf(e.FileName);
    if (p1<0) or (p2<0) then
      UpdateClassBrowsing;
  end;
  }
  ClassBrowser.BeginTreeUpdate;
  try
    // ClassBrowser.ClearTree; //set parser will do the clear
    ClassBrowser.Parser := GetCppParser;
    if Assigned(e) then begin
      if e.InProject then begin
        ClassBrowser.StatementsType := devClassBrowsing.StatementsType;
      end else
        ClassBrowser.StatementsType := cbstFile;
      ClassBrowser.CurrentFile := e.FileName;
      if (e.FileName <> '') and
        ( (not e.New)  or e.Text.Modified) then begin
        ParseFile(GetCppParser,e.FileName,e.InProject);
      end;
    end else begin
      ClassBrowser.Parser:=nil; // set parser to nil will do the clear
      ClassBrowser.CurrentFile := '';
    end;
  finally
    ClassBrowser.EndTreeUpdate;
  end;
end;

procedure TMainForm.actProfileExecute(Sender: TObject);
var
  ProfilingEnabled, StripEnabled: boolean;
  path: AnsiString;
  e: TEditor;
begin
  case GetCompileTarget of
    cttProject: begin
        // Check if we enabled proper options
        ProfilingEnabled := fProject.GetCompilerOption('-pg') <> '0';
        StripEnabled := fProject.GetCompilerOption('-s') <> '0';

        // Ask the user if he wants to enable profiling
        if (not ProfilingEnabled or StripEnabled) and (MessageDlg(Lang[ID_MSG_NOPROFILE], mtConfirmation, [mbYes, mbNo],
          0) = mrYes) then begin

          // Enable profiling, disable stripping
          fProject.SetCompilerOption('-pg', '1');
          fProject.SetCompilerOption('-s', '0');

          fCompSuccessAction := csaProfile;
          actRebuildExecute(nil);
          Exit;
        end;

        path := ExtractFilePath(fProject.Executable) + GPROF_CHECKFILE;

        //Gather data by running our project...
        fRunEndAction := reaProfile;
        if not FileExists(path) then begin
          fRunEndAction := reaProfile;
          actRunExecute(nil);
        end else begin // If the data is there, open up the form
          RunEndProc;
        end;
     end;
    cttFile: begin
        // Check if we enabled proper options
        with devCompilerSets.CompilationSet do begin
          ProfilingEnabled := GetOption('-pg') <> '0';
          StripEnabled := GetOption('-s') <> '0';
        end;

        // Ask the user if he wants to enable profiling
        if (not ProfilingEnabled or StripEnabled) and (MessageDlg(Lang[ID_MSG_NOPROFILE], mtConfirmation, [mbYes, mbNo],
          0) = mrYes) then begin

          // Enable profiling, disable stripping
          with devCompilerSets.CompilationSet do begin
            SetOption('-pg', '1');
            SetOption('-s', '0');
          end;

          // Save changes to compiler set
          devCompilerSets.SaveSet(devCompilerSets.CompilationSetIndex);

          fRunEndAction := reaProfile;
          actCompRunExecute(nil);
          Exit;
        end;

        e := fEditorList.GetEditor;
        if Assigned(e) then
          path := ExtractFilePath(ChangeFileExt(e.FileName, EXE_EXT)) + GPROF_CHECKFILE
        else
          Exit;

        // Did we save?
        if e.Text.Modified then begin // if file is modified
          if not e.Save then // save it first
            Exit;
        end;

        if not FileExists(path) then begin
          fRunEndAction := reaProfile;
          actRunExecute(nil);
          Exit;
        end;

        // source file is newer than execute file
        if CompareFileModifyTime(e.FileName,ChangeFileExt(e.FileName, EXE_EXT))>=0 then
         if MessageDlg(Lang[ID_MSG_SOURCEMORERECENT], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
          fRunEndAction := reaProfile;
          actCompRunExecute(nil);
          Exit;
         end;

        if CompareFileModifyTime(e.FileName,path)>=0 then
         if MessageDlg(Lang[ID_MSG_SOURCEMORERECENT], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
          fRunEndAction := reaProfile;
          actCompRunExecute(nil);
          Exit;
         end;

        fRunEndAction := reaProfile;
        RunEndProc;
      end;
    cttNone: Exit;
   end;

end;

procedure TMainForm.actCloseAllButThisExecute(Sender: TObject);
begin
  ClassBrowser.BeginTreeUpdate;
  try
    fEditorList.CloseAllButThis;
  finally
    ClassBrowser.EndTreeUpdate;
  end;
end;

procedure TMainForm.lvBacktraceCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not (cdsSelected in State) then begin
    if Assigned(Item.Data) then
      Sender.Canvas.Font.Color := clBlue
    else
      Sender.Canvas.Font.Color := clWindowText;
  end;
end;

procedure TMainForm.lvBacktraceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  It: TListItem;
begin
  with Sender as TListView do begin
    It := GetItemAt(X, Y);
    if Assigned(It) and Assigned(It.Data) then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
  end;
end;

procedure TMainForm.UpdateFileEncodingStatusPanel;
var
  e:TEditor;
  s:String;
begin
  e := fEditorList.GetEditor;
  s:='';
  if Assigned(e) then begin
    if (e.FileEncoding = etUTF8) then
      s := s+'UTF-8'
    else if (e.FileEncoding = etUTF8Bom) then
      s := s+'UTF-8 BOM'
    else if (e.FileEncoding = etAscii) then
      s := s+'ASCII'
    else
      s := 'ANSI('+UpperCase(GetSystemCharsetName)+')';
  end;
  Statusbar.Panels[1].Text := s;
  Statusbar.Panels[1].Width := Canvas.TextWidth(s)+20;
end;

procedure TMainForm.FileMonitorTimer(Sender: TObject);
begin
  fMonitorTimer.OnTimer := nil;
  fileBrowser.Refresh;
end;


procedure TMainForm.FileMonitorNotifyChange(Sender: TObject; ChangeType: TdevMonitorChangeType; Filename: string);
var
  e: TEditor;
  p: TBufferCoord;
begin
  // Deactivate monitoring for this file. One message is enough
  FileMonitor.BeginUpdate;
  try
    case ChangeType of
      mctDirectory: begin
        fMonitorTimer.OnTimer := FileMonitorTimer;
        fMonitorTimer.Interval := 1000;
        exit;
      end;
      mctChanged: begin
          Application.Restore;
          if MessageDlg(Format(Lang[ID_ERR_FILECHANGED], [Filename]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
            begin
            e := fEditorList.GetEditorFromFileName(Filename);
            if Assigned(e) then begin
              p := e.Text.CaretXY;
              e.LoadFile(Filename);
              if (p.Line <= e.Text.Lines.Count) then
                e.Text.CaretXY := p;
            end;
          end;
        end;
      mctDeleted: begin
          Application.Restore;
          case MessageDlg(Format(Lang[ID_ERR_RENAMEDDELETEDKEEP], [Filename]), mtInformation, [mbYes, mbNo], 0) of
            mrYes: begin
                e := fEditorList.FileIsOpen(Filename);
                if Assigned(e) then
                  e.Text.Modified := True;
              end;
            mrNo: begin
                e := fEditorList.FileIsOpen(Filename);
                if Assigned(e) then
                  fEditorList.CloseEditor(e);
              end;
          end;
        end;
    end;
  finally
    FileMonitor.EndUpdate;
  end;
end;

procedure TMainForm.actFilePropertiesExecute(Sender: TObject);
begin
  with TFilePropertiesForm.Create(Self) do begin
    if TAction(Sender).ActionComponent = mnuUnitProperties then
      if Assigned(fProject) and
        Assigned(ProjectView.Selected) and
        (ProjectView.Selected.Data <> Pointer(-1)) then
        SetFile(fProject.Units[integer(ProjectView.Selected.Data)].FileName);
    ShowModal;
  end;
end;

procedure TMainForm.actViewToDoListExecute(Sender: TObject);
begin
  TViewToDoForm.Create(Self).ShowModal;
end;

procedure TMainForm.actAddToDoExecute(Sender: TObject);
begin
  TAddToDoForm.Create(Self).ShowModal;
end;

procedure TMainForm.actProjectNewFolderExecute(Sender: TObject);
var
  fp, S: AnsiString;
begin
  S := 'New folder';
  if ShowInputQuery(Lang[ID_POP_ADDFOLDER], Lang[ID_MSG_ADDBROWSERFOLDER], S) and (S <> '') then begin
    fp := fProject.GetFolderPath(ProjectView.Selected);
    if fp <> '' then
      fProject.AddFolder(fp + '/' + S)
    else
      fProject.AddFolder(S);
  end;
end;

procedure TMainForm.actProjectRemoveFolderExecute(Sender: TObject);
begin
  // Useless check...
  if not Assigned(fProject) then
    exit;

  // Check if the current item is a folder
  if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data = Pointer(-1)) then

    // Ask if we want to remove...
    if MessageDlg(Lang[ID_MSG_REMOVEBROWSERFOLDER], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      ProjectView.Items.BeginUpdate;
      try
        fProject.RemoveFolder(ProjectView.Selected);
      finally
        ProjectView.Items.EndUpdate;
      end;
    end;
end;

procedure TMainForm.actProjectRenameFolderExecute(Sender: TObject);
var
  S: AnsiString;
begin
  if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data = Pointer(-1)) then begin
    S := ProjectView.Selected.Text;
    if ShowInputQuery(Lang[ID_POP_RENAMEFOLDER], Lang[ID_MSG_RENAMEBROWSERFOLDER], S) and (S <> '') then begin
      ProjectView.Selected.Text := S;
      fProject.UpdateFolders;
    end;
  end;
end;

{ begin XXXKF changed }

procedure TMainForm.ProjectViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: TTreeNode;
  Hits: THitTests;
begin
  Hits := ProjectView.GetHitTestInfoAt(X, Y);
  if ([htOnItem, htOnRight, htToRight] * Hits) <> [] then
    Item := ProjectView.GetNodeAt(X, Y)
  else
    Item := nil;
  Accept :=
    (
    (Sender = ProjectView) and Assigned(ProjectView.Selected) and Assigned(Item) //and
    // drop node is a folder or the project
    and ((Item.Data = Pointer(-1)) or (Item.SelectedIndex = 0))
    );
end;

procedure TMainForm.ProjectViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Item: TTreeNode;
  srcNode: TTreeNode;
  WasExpanded: Boolean;
  I: integer;
begin
  if not (Source is TTreeView) then
    exit;
  if ([htOnItem, htOnRight, htToRight] * ProjectView.GetHitTestInfoAt(X, Y)) <> [] then
    Item := ProjectView.GetNodeAt(X, Y)
  else
    Item := nil;
  for I := 0 to ProjectView.SelectionCount - 1 do begin
    srcNode := ProjectView.Selections[I];
    if not Assigned(Item) then begin
      fProject.Units[Integer(srcNode.Data)].Folder := '';
      srcNode.MoveTo(ProjectView.Items[0], naAddChild);
    end else begin
      if srcNode.Data <> Pointer(-1) then begin
        if Item.Data = Pointer(-1) then
          fProject.Units[Integer(srcNode.Data)].Folder := fProject.GetFolderPath(Item)
        else
          fProject.Units[Integer(srcNode.Data)].Folder := '';
      end;
      WasExpanded := Item.Expanded;
      ProjectView.Items.BeginUpdate;
      srcNode.MoveTo(Item, naAddChild);
      if not WasExpanded then
        Item.Collapse(False);
      Item.AlphaSort(False);
      ProjectView.Items.EndUpdate;

      fProject.UpdateFolders;
    end;
  end;
end;

procedure TMainForm.actImportMSVCExecute(Sender: TObject);
begin
  with TImportMSVCForm.Create(Self) do begin
    if ShowModal = mrOK then
      OpenProject(GetFilename);
  end;
end;

procedure TMainForm.ViewCPUItemClick(Sender: TObject);
begin
  if not Assigned(CPUForm) then begin
    CPUForm := TCPUForm.Create(self);
  end;
  CPUForm.Show;
  Debugger.SendCommand('info', 'registers');
  if (devDebugger.BlendMode) then
    Debugger.SendCommand('disas','/s')
  else
    Debugger.SendCommand('disas','');
end;

procedure TMainForm.CheckForDLLProfiling;
var
  prof: boolean;
begin
  if not Assigned(fProject) then
    Exit;

  // If this is a DLL / static link library project, then check for profiling
  if (fProject.Options.typ in [dptDyn, dptStat]) then begin

    // Check if profiling is enabled
    prof := fProject.GetCompilerOption('-pg') <> '0';

    // Check for the existence of "-lgmon" in project's linker options
    if prof and (Pos('-lgmon', fProject.Options.LinkerCmd) = 0) then begin

      // Warn the user that we should update its project options and include
      // -lgmon in linker options, or else the compilation will fail
      if MessageDlg('You have profiling enabled in Compiler Options and you are ' +
        'working on a dynamic or static library. Do you want to add a special ' +
        'linker option in your project to allow compilation with profiling enabled? ' +
        'If you choose No, and you do not disable profiling from the Compiler Options ' +
        'chances are that your library''s compilation will fail...', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
        fProject.Options.LinkerCmd := fProject.Options.LinkerCmd + ' -lgmon';
        fProject.Modified := True;
      end;
    end;
  end;
end;

procedure TMainForm.actExecParamsExecute(Sender: TObject);
begin
  with TParamsForm.Create(self) do try
    case GetCompileTarget of
      cttNone, cttFile: begin
        ParamEdit.Text := fCompiler.RunParams;
        cbUseParams.Checked := fCompiler.UseRunParams;
        cbUseInputFile.Checked := fCompiler.UseInputFile;
        txtInputFile.Text := fCompiler.InputFile;
        end;
      cttProject: begin
          HostEdit.Text := fProject.Options.HostApplication;
          ParamEdit.Text := fProject.Options.CmdLineArgs;
        end;
    end;
    if not Assigned(fProject) or (fProject.Options.typ <> dptDyn) then
      DisableHost;
      
    if (ShowModal = mrOk) then begin
      fCompiler.UseRunParams := cbUseParams.Checked;
      fCompiler.UseInputFile := cbUseInputFile.Checked;
      fCompiler.InputFile := txtInputFile.Text;
      case GetCompileTarget of
        cttNone, cttFile:
          fCompiler.RunParams := ParamEdit.Text;
        cttProject: begin
            if (HostEdit.Enabled) then
              fProject.Options.HostApplication := HostEdit.Text;
            fProject.Options.CmdLineArgs := ParamEdit.Text;
            fProject.Modified := true;
          end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.DevCppDDEServerExecuteMacro(Sender: TObject; Msg: TStrings);
var
  filename: AnsiString;
  i, n: Integer;
begin
  if Msg.Count > 0 then begin
    for i := 0 to Msg.Count - 1 do begin
      filename := Msg[i];
      if Pos('[Open(', filename) = 1 then begin
        n := Pos('"', filename);
        if n > 0 then begin
          Delete(filename, 1, n);
          n := Pos('"', filename);
          if n > 0 then
            Delete(filename, n, maxint);
          try
            OpenFile(filename, etAuto)
          except
          end;
        end;
      end;
    end;
    Application.BringToFront;
  end;
end;

procedure TMainForm.actShowTipsExecute(Sender: TObject);
begin
  with TTipOfTheDayForm.Create(Self) do
    ShowModal;
end;

procedure TMainForm.CppParserStartParsing(var message:TMessage);
var
  e: TEditor;
begin
  // Update UI before the parser starts working
  //Application.ProcessMessages;

  // Loading...
  Screen.Cursor := crHourglass;

  //don't call classbrowser to beginupdate, CppParser will do it

  // Hide anything that uses the database (which will be invalidated)
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    e.FunctionTip.ReleaseHandle;
    e.CompletionBox.Hide;
  end;
  ClassBrowser.BeginTreeUpdate;

  fParseStartTime := GetTickCount;
end;

procedure TMainForm.CppParserTotalProgress(var message:TMessage);
var
  ShowStep: Integer;
  pMsg: PCppParserProgressMessage;
  Total,Current:Integer;
begin
  pMsg:= PCppParserProgressMessage(message.WParam);
  Total := pMsg.Total;
  Current := pMsg.Current;
  // Mention every 5% progress
  ShowStep := Total div 20;

  // For Total = 1, avoid division by zero
  if ShowStep = 0 then
    ShowStep := 1;

  // Only show if needed (it's a very slow operation)
  if (Current mod ShowStep = 0) or (Current = 1) then begin
    SetStatusBarMessage(Format(Lang[ID_PARSINGFILECOUNT], [Current, Total, pMsg.Filename]));
    //Application.ProcessMessages;
  end;
  Dispose(PCppParserProgressMessage(pMsg));
end;

procedure TMainForm.CppParserEndParsing(var message:TMessage);
var
  ParseTimeFloat, ParsingFrequency: Extended;
  total:integer;
  e:TEditor;
  updateView:integer;
begin
  Screen.Cursor := crDefault;

  total := message.wParam;
  updateView := message.LParam;

  //CppParser will call class browser to redraw, don't do it twice

  // do this work only if this was the last file scanned
  ParseTimeFloat := (GetTickCount - fParseStartTime) / 1000;
  if Total > 1 then begin
    if ParseTimeFloat <> 0 then
      ParsingFrequency := Total / ParseTimeFloat
    else
      ParsingFrequency := 999;
    SetStatusbarMessage(Format(Lang[ID_DONEPARSINGINCOUNT], [Total, ParseTimeFloat, ParsingFrequency]))
  end else
    SetStatusbarMessage(Format(Lang[ID_DONEPARSINGIN], [ParseTimeFloat]));

  if updateView <> 0 then begin
    ClassBrowser.EndTreeUpdate;
    e:=EditorList.GetEditor;
    if assigned(e) then begin
      e.Text.Invalidate;
    end;
  end else begin
    ClassBrowser.EndTreeUpdate(False);
  end;
end;

procedure TMainForm.UpdateAppTitle;
var
  e: TEditor;
  str: String;
  appName : String;
begin
  appName := Lang[ID_DEVCPP];
  e := fEditorList.GetEditor;
  if Assigned(e) and not e.InProject then begin
    if e.Text.Modified then
      str := e.FileName + ' [*]'
    else
      str := e.FileName;
    if fDebugger.Executing then begin
      Caption := Format('%s - [Debugging] - %s %s', [str, appName, DEVCPP_VERSION]);
      Application.Title := Format('%s - [Debugging] - %s', [ExtractFileName(e.FileName), appName]);
    end else if devExecutor.Running then begin
      Caption := Format('%s - [Executing] - %s %s', [str, appName, DEVCPP_VERSION]);
      Application.Title := Format('%s - [Executing] - %s', [ExtractFileName(e.FileName), appName]);
    end else if fCompiler.Compiling then begin
      Caption := Format('%s - [Compiling] - %s %s', [str, appName, DEVCPP_VERSION]);
      Application.Title := Format('%s - [Compiling] - %s', [ExtractFileName(e.FileName), appName]);
    end else begin
      Caption := Format('%s - %s %s', [str, appName, DEVCPP_VERSION]);
      Application.Title := Format('%s - %s', [ExtractFileName(e.FileName), appName]);
    end;
  end else if Assigned(fProject) then begin
    if fDebugger.Executing then begin
      Caption := Format('%s - [%s] - [Debugging] - %s %s',
        [fProject.Name, ExtractFilename(fProject.Filename), appName, DEVCPP_VERSION]);
      Application.Title := Format('%s - [Debugging] - %s', [fProject.Name, appName]);
    end else if devExecutor.Running then begin
      Caption := Format('%s - [%s] - [Executing] - %s %s',
        [fProject.Name, ExtractFilename(fProject.Filename), appName, DEVCPP_VERSION]);
      Application.Title := Format('%s - [Executing] - %s', [fProject.Name, appName]);
    end else if fCompiler.Compiling then begin
      Caption := Format('%s - [%s] - [Compiling] - %s %s',
        [fProject.Name, ExtractFilename(fProject.Filename), appName, DEVCPP_VERSION]);
      Application.Title := Format('%s - [Compiling] - %s', [fProject.Name, appName]);
    end else begin
      Caption := Format('%s - [%s] - %s %s',
        [fProject.Name, ExtractFilename(fProject.Filename), appName, DEVCPP_VERSION]);
      Application.Title := Format('%s - %s', [fProject.Name, appName]);
    end;
  end else begin
    Caption := Format('%s %s', [appName, DEVCPP_VERSION]);
    Application.Title := Format('%s', [DEVCPP]);
  end;
end;

procedure TMainForm.actAbortCompilationUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := fCompiler.Compiling;
end;

procedure TMainForm.actAbortCompilationExecute(Sender: TObject);
begin
  fCompiler.AbortThread;
  fCheckSyntaxInBack := False;
end;

{ begin XXXKF }

procedure TMainForm.actWindowMenuExecute(Sender: TObject);
var
  Item: TMenuItem;
  E: TEditor;
  i: integer;
  Act: TAction;
begin
  // Remove old items
  while WindowMenu.Count > 8 do begin
    Item := WindowMenu.Items[7];
    WindowMenu.Remove(Item);
    Item.Destroy;
  end;

  // Add new items, 9 at most
  for i := 0 to min(9, pred(EditorList.PageCount)) do begin
    e := fEditorList[i];
    if Assigned(e) then begin
      Item := TMenuItem.Create(self);

      // Create an action that executes when clicking on the item
      Act := TAction.Create(Item);
      Act.Name := 'dynactOpenEditorByTag';
      Act.Tag := Integer(e);
      Act.OnExecute := actOpenEditorByTagExecute;

      // Attach it to the new menu item
      Item.Action := Act;
      Item.Caption := '&' + IntToStr(i) + ' ' + e.FileName;
      if e.Text.Modified then
        Item.Caption := Item.Caption + ' *';
      WindowMenu.Insert(WindowMenu.Count - 1, Item);
    end;
  end;
end;

procedure TMainForm.RemoveItem(Node: TTreeNode);
begin
  if Assigned(Node) and (Node.Level >= 1) then begin
    if Node.Data = Pointer(-1) then
      actProjectRemoveFolderExecute(nil)
    else
      fProject.RemoveEditor(integer(Node.Data), true);
  end;
end;

procedure TMainForm.ProjectViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and Assigned(ProjectView.Selected) then
    RemoveItem(ProjectView.Selected)
  else if (Key = VK_ESCAPE) and (Shift = []) then begin
    if (EditorList.PageCount > 0) then
      fEditorList.GetEditor(-1).Activate;
  end else if (Key = VK_ESCAPE) and (Shift = [ssShift]) then begin
    actProjectManager.Checked := False;
    actProjectManagerExecute(nil);
    if (EditorList.PageCount > 0) then
      fEditorList.GetEditor(-1).Activate;
  end else if (Key = VK_RETURN) and Assigned(ProjectView.Selected) then begin
    if ProjectView.Selected.Data <> Pointer(-1) then begin { if not a directory }
      OpenUnit;
      if Shift = [ssShift] then begin
        {
         crap hack, SHIFT+ENTER=open file and close projman
         I *really* don't think it's the acceptable interface idea.
         Can't find a better one though.
        }
        actProjectManager.Checked := False;
        actProjectManagerExecute(nil);
      end;
    end;
  end;
end;

procedure TMainForm.ProjectViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  Node.MakeVisible;
end;

procedure TMainForm.actOpenEditorByTagExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditorFromTag(TAction(Sender).Tag);
  if Assigned(e) then
    e.Activate;
end;

{ end XXXKF }

procedure TMainForm.actGotoProjectManagerExecute(Sender: TObject);
begin
  if not actProjectManager.Checked then begin
    actProjectManager.Checked := True;
    actProjectManagerExecute(nil);
  end;
  LeftPageControl.ActivePageIndex := 0;
  fLeftPageControlChanged := False;
  
  ProjectView.SetFocus;
end;

procedure TMainForm.ListItemClick(Sender: TObject);
var
  I: integer;
  e: TEditor;
  item: TListItem;
begin
  with TWindowListForm.Create(self) do try
    // Fill here instead of in Window itself?
    UnitList.Items.BeginUpdate;
    try
      for I := 0 to fEditorList.PageCount - 1 do begin
        e := fEditorList[i];

        item := UnitList.Items.Add;
        item.Caption := ExtractFileName(e.FileName);
        item.SubItems.Add(e.FileName);
      end;
    finally
      UnitList.Items.EndUpdate;
    end;

    if (ShowModal = mrOk) and (UnitList.ItemIndex <> -1) then begin
      e := fEditorList[UnitList.ItemIndex];
      if Assigned(e) then
        e.Activate;
    end;
  finally
    Close;
  end;
end;

procedure TMainForm.ProjectViewCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  if (Node1.Data = pointer(-1)) and (Node2.Data = pointer(-1)) then
    Compare := CompareStr(Node1.Text, Node2.Text)
  else if Node1.Data = pointer(-1) then
    Compare := -1
  else if Node2.Data = pointer(-1) then
    Compare := +1
  else
    Compare := CompareStr(Node1.Text, Node2.Text);
end;

procedure TMainForm.ProjectViewKeyPress(Sender: TObject; var Key: Char);
begin
  // fixs an annoying bug/behavior of the tree ctrl (a beep on enter key)
  if Key = #13 then
    Key := #0;
end;

procedure TMainForm.ProjectViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // bug-fix: when *not* clicking on an item, re-opens the last clicked file-node
  // this was introduced in the latest commit by XXXKF (?)
  if (Button = mbLeft) and not (htOnItem in ProjectView.GetHitTestInfoAt(X, Y)) then
    ProjectView.Selected := nil;
end;

procedure TMainForm.GoToClassBrowserItemClick(Sender: TObject);
begin
  if not actProjectManager.Checked then begin
    actProjectManager.Checked := True;
    actProjectManagerExecute(nil);
  end;
  LeftPageControl.ActivePageIndex := 1;
  fLeftPageControlChanged := False;
  
  if ClassBrowser.Visible then
    ClassBrowser.SetFocus;
end;

procedure TMainForm.actBrowserShowInheritedExecute(Sender: TObject);
begin
  ClassBrowser.ShowInheritedMembers := not ClassBrowser.ShowInheritedMembers;
  devClassBrowsing.ShowInheritedMembers := ClassBrowser.ShowInheritedMembers;
  actBrowserShowInherited.Checked := ClassBrowser.ShowInheritedMembers;
  ClassBrowser.Refresh;
end;

procedure TMainForm.ProjectWindowClose(Sender: TObject; var Action: TCloseAction);
begin
  LeftPageControl.Visible := false;
  (Sender as TForm).RemoveControl(LeftPageControl);

  LeftPageControl.Left := 0;
  LeftPageControl.Top := ToolbarDock.Height;
  LeftPageControl.Align := alLeft;
  LeftPageControl.Visible := true;
  InsertControl(LeftPageControl);
  fProjectToolWindow.Free;
  fProjectToolWindow := nil;

  if assigned(fProject) then
    fProject.SetNodeValue(ProjectView.TopItem); // nodes needs to be recreated
end;

procedure TMainForm.ReportWindowClose(Sender: TObject; var Action: TCloseAction);
begin
  MessageControl.Visible := false;
  (Sender as TForm).RemoveControl(MessageControl);

  MessageControl.Left := 0;
  MessageControl.Top := SplitterBottom.Top;
  MessageControl.Align := alBottom;
  MessageControl.Visible := true;
  InsertControl(MessageControl);
  Statusbar.Top := MessageControl.Top + MessageControl.Height;
  fReportToolWindow.Free;
  fReportToolWindow := nil;
end;

procedure TMainForm.actSaveProjectAsExecute(Sender: TObject);
begin
  if not Assigned(fProject) then
    Exit;

  with TSaveDialog.Create(nil) do try

    Filter := FLT_PROJECTS;
    Options := Options + [ofOverwritePrompt];
    Title := Lang[ID_NV_SAVEPROJECT];

    if Execute then begin
      fProject.FileName := FileName;
      fProject.SaveAll;
      UpdateAppTitle;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.BuildOpenWith;
var
  idx: integer;
  FileName:String;

  procedure Build(OpenWithItem:TMenuItem;FileName:String; clickHandler: TNotifyEvent);
  var
    idx2: integer;
    item: TMenuItem;
    ext, s, s1: AnsiString;
  begin
    ext := ExtractFileExt(FileName);
    if SameStr('.rc',ext) then begin
      item := TMenuItem.Create(nil);
      item.Caption := ExtractFilename('ResEd.exe');
      item.Tag := -3;
      item.OnClick := clickHandler;
      OpenWithItem.Add(item);
    end;
    idx2 := devExternalPrograms.AssignedProgram(ext);
    if idx2 <> -1 then begin
      if (OpenWithItem.Count = 1) then begin
        item := TMenuItem.Create(nil);
        item.Caption := '-';
        item.Tag := -2;
        OpenWithItem.Add(item);
      end;
      item := TMenuItem.Create(nil);
      item.Caption := ExtractFilename(devExternalPrograms.ProgramName[idx2]);
      item.Tag := idx2;
      item.OnClick := clickHandler;
      OpenWithItem.Add(item);
    end;
    if GetAssociatedProgram(ext, s, s1) then begin
      if (OpenWithItem.Count = 1) then begin
        item := TMenuItem.Create(nil);
        item.Caption := '-';
        item.Tag := -2;
        OpenWithItem.Add(item);
      end;
      item := TMenuItem.Create(nil);
      item.Caption := s1;
      item.Tag := -1;
      item.OnClick := clickHandler;
      OpenWithItem.Add(item);
    end;
  end;
begin
  mnuOpenWith.Clear;
  if assigned(fProject) and
     assigned(ProjectView.Selected) and (ProjectView.Selected.Level > 0) and
     (ProjectView.Selected.Data <> Pointer(-1)) then begin
    idx := integer(ProjectView.Selected.Data);
    FileName := fProject.Units[idx].FileName;
    Build(mnuOpenWith, FileName, mnuOpenWithClick);
  end;
  mnuFileBrowserOpenWith.Clear;
  if FileBrowser.SelectedFile <> '' then begin
    Build(mnuFileBrowserOpenWith, FileBrowser.SelectedFile,mnuFileBrowserOpenWithClick);
  end;
end;

procedure TMainForm.mnuOpenWithClick(Sender: TObject);
var
  idx, idx2: integer;
  item: TMenuItem;
  e: TEditor;
begin
  if (Sender = mnuOpenWith) and (mnuOpenWith.Count > 0) then
    Exit;
  if not Assigned(fProject) then
    Exit;
  if not assigned(ProjectView.Selected) or
    (ProjectView.Selected.Level < 1) then
    exit;
  if ProjectView.Selected.Data = Pointer(-1) then
    Exit;
  idx2 := integer(ProjectView.Selected.Data);

  item := TMenuItem(Sender);
  if item = mnuOpenWith then begin
    idx := -2;
    with TOpenDialog.Create(Self) do try
      Filter := FLT_ALLFILES;
      if Execute then
        idx := devExternalPrograms.AddProgram(ExtractFileExt(fProject.Units[idx2].FileName), Filename);
    finally
      Free;
    end;
  end else
    idx := item.Tag;

  e := fEditorList.FileIsOpen(fProject.Units[idx2].FileName, TRUE);
  if Assigned(e) then
    fEditorList.CloseEditor(e);

  if idx > -1 then begin // devcpp-based
    ShellExecute(0, 'open',
      PAnsiChar(devExternalPrograms.ProgramName[idx]),
      PAnsiChar(fProject.Units[idx2].FileName),
      PAnsiChar(ExtractFilePath(fProject.Units[idx2].FileName)),
      SW_SHOW)
      // idx=-2 means we prompted the user for a program, but didn't select one
  end else if idx = -1 then begin// registry-based
    ShellExecute(0, 'open',
      PAnsiChar(fProject.Units[idx2].FileName),
      nil,
      PAnsiChar(ExtractFilePath(fProject.Units[idx2].FileName)),
      SW_SHOW);
  end else if idx = -3 then begin// ResEd.exe
    ShellExecute(0, 'open',
      PAnsiChar(devDirs.Exec + 'ResEd/ResEd.exe'),
      PAnsiChar(fProject.Units[idx2].FileName),
      PAnsiChar(ExtractFilePath(fProject.Units[idx2].FileName)),
      SW_SHOW);
  end
end;

procedure TMainForm.CompilerOutputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    CompilerOutputDblClick(sender);
end;

procedure TMainForm.FindOutputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    FindOutputDblClick(sender);
end;

procedure TMainForm.WatchViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    actRemoveWatchExecute(sender);
end;

procedure TMainForm.DebugPopupPopup(Sender: TObject);
begin
  RemoveWatchPop.Enabled := Assigned(WatchView.Selected);
end;

procedure TMainForm.actAttachProcessUpdate(Sender: TObject);
begin
  if assigned(fProject) and (fProject.Options.typ = dptDyn) then begin
    TCustomAction(Sender).Visible := true;
    TCustomAction(Sender).Enabled := not devExecutor.Running;
  end else
    TCustomAction(Sender).Visible := false;
end;

procedure TMainForm.actAttachProcessExecute(Sender: TObject);
var
  s: AnsiString;
begin
  PrepareDebugger;
  if assigned(fProject) then begin
    if not FileExists(fProject.Executable) then begin
      MessageDlg(Lang[ID_ERR_PROJECTNOTCOMPILED], mtWarning, [mbOK], 0);
      exit;
    end;

    try
      ProcessListForm := TProcessListForm.Create(self);
      if (ProcessListForm.ShowModal = mrOK) and (ProcessListForm.ProcessCombo.ItemIndex > -1) then begin
        s := IntToStr(integer(ProcessListForm.ProcessList[ProcessListForm.ProcessCombo.ItemIndex]));

        fDebugger.Start;
        if not fDebugger.Executing then
          Exit;
        fDebugger.SendCommand('file', '"' + StringReplace(fProject.Executable, '\', '\\', [rfReplaceAll]) + '"');
        fDebugger.SendCommand('attach', s);
      end
    finally
      ProcessListForm.Free;
    end;
  end;
end;

procedure TMainForm.actModifyWatchExecute(Sender: TObject);
var
  curnode: TTreeNode;
  fullname: AnsiString;
  value : AnsiString;

  function GetNodeName(node: TTreeNode): AnsiString;
  var
    epos: integer;
  begin
    Result := '';
    epos := Pos(' = ', node.Text);
    if epos > 0 then
      Result := Copy(node.Text, 1, epos - 1);
  end;

  function GetNodeValue(node: TTreeNode): AnsiString;
  var
    epos: integer;
  begin
    Result := '';
    epos := Pos(' = ', node.Text);
    if epos > 0 then
      Result := Copy(node.Text, epos + 3, Length(node.Text) - epos);
  end;

begin
  curnode := WatchView.Selected;
  if Assigned(curnode) then begin // only edit members

    fullname := GetNodeName(curnode);

    // Assemble full name including parents
    while Assigned(curnode.Parent) do begin
      curnode := curnode.Parent;
      if not StartsStr('{', GetNodeName(curnode)) then
        fullname := GetNodeName(curnode) + '.' + fullname;
    end;

    value := GetNodeValue(WatchView.Selected);

    if ShowInputQuery(Lang[ID_NV_MODIFYVALUE], fullname, value) then
      fDebugger.SendCommand('set variable', fullname + ' = ' + value);
  end;
end;

procedure TMainForm.actModifyWatchUpdate(Sender: TObject);
var
  curnode: TTreeNode;
  fullname: AnsiString;

  function GetNodeName(node: TTreeNode): AnsiString;
  var
    epos: integer;
  begin
    Result := '';
    epos := Pos(' = ', node.Text);
    if epos > 0 then
      Result := Copy(node.Text, 1, epos - 1);
  end;

begin
  curnode := WatchView.Selected;
  if Assigned(curnode) then begin // only edit members
    fullname := GetNodeName(curnode);
    if IsIdentifier(fullname) then begin
      TCustomAction(Sender).Enabled := fDebugger.Executing;
      Exit;
    end;
  end;
  TCustomAction(Sender).Enabled := False;
end;

procedure TMainForm.ClearallWatchPopClick(Sender: TObject);
begin
  fDebugger.DeleteWatchVars(true);
end;

procedure TMainForm.actDeleteProfileExecute(Sender: TObject);
var
  path: AnsiString;
  e: TEditor;
begin
  path := '';
  case GetCompileTarget of
    cttProject: begin
        path := ExtractFilePath(fProject.Executable) + GPROF_CHECKFILE;
      end;
    cttFile: begin
        e := fEditorList.GetEditor;
        if Assigned(e) then
          path := ExtractFilePath(ChangeFileExt(e.FileName, EXE_EXT)) + GPROF_CHECKFILE;
      end;
    cttNone: Exit;
  end;

  if path <> '' then begin
    if DeleteFile(PAnsiChar(path)) then begin
      SetStatusbarMessage(Format(Lang[ID_DELETEDPROFDATA], [path]));
    end else
      SetStatusbarMessage(Format(Lang[ID_COULDNOTFINDPROFDATA], [path]));
  end;
end;

procedure TMainForm.actGotoImplDeclEditorExecute(Sender: TObject);
var
  statement: PStatement;
  filename, phrase: AnsiString;
  line: integer;
  e: TEditor;
  pBeginPos,pEndPos : TBufferCoord;
begin
  e := fEditorList.GetEditor;

  if Assigned(e) then begin

    // Exit early, don't bother creating a stream (which is slow)
    phrase := GetWordAtPosition(e.Text,e.Text.CaretXY,pBeginPos,pEndPos, wpInformation);
    if Phrase = '' then
      Exit;

    // When searching using menu shortcuts, the caret is set to the proper place
    // When searching using ctrl+click, the cursor is set properly too, so do NOT use WordAtMouse
    statement := GetCppParser.FindStatementOf(
        e.FileName,
        phrase, e.Text.CaretY);

    // Otherwise scan the returned class browser statement
    if Assigned(statement) then begin

      // If the caret position was used
      if Sender = e then begin // Ctrl+Click from current editor

        // Switching between declaration and definition
        if (e.Text.CaretY = statement^._DefinitionLine) and SameFileName(e.FileName, statement^._DefinitionFileName) then
          begin // clicked on declaration, move to definition
          filename := statement^._FileName;
          line := statement^._Line;
        end else begin // clicked anywhere, go to declaration
          filename := statement^._DefinitionFileName;
          line := statement^._DefinitionLine;
        end;

        // Menu item or mouse cursor
      end else begin

        // Switching between declaration and definition
        if Pos('Decl', TCustomAction(Sender).Name) > 0 then begin
          filename := statement^._FileName;
          line := statement^._Line;
        end else begin
          filename := statement^._DefinitionFileName;
          line := statement^._DefinitionLine;
        end;
      end;

      // Go to the location
      e := fEditorList.GetEditorFromFileName(filename);
      if Assigned(e) then begin
        e.SetCaretPosAndActivate(line, 1);
      end;

      SetStatusbarMessage('');
    end else
      SetStatusbarMessage(Format(Lang[ID_INFONOTFOUND], [phrase]));
  end;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var
  Handled: Boolean);
var
  e: TEditor;
  State: TKeyboardState;
  I: integer;
begin

  // Check if control is pressed
  GetKeyboardState(State);
  e := fEditorList.GetEditor;

  // Check if we're focussing on an editor and holding ctrl
  if Assigned(e) and (Screen.ActiveControl = e.Text) and ((State[vk_Control] and 128) <> 0) then begin

    for I := 0 to pred(EditorList.PageCount) do begin
      e := fEditorList[I];

      // If so, set the font size of all editors...
      if WheelDelta > 0 then begin
        e.Text.Font.Size := Max(1, e.Text.Font.Size + 1);
        e.Text.Gutter.Font.Size := Max(1, e.Text.Gutter.Font.Size + 1);
      end else begin
        e.Text.Font.Size := Max(1, e.Text.Font.Size - 1);
        e.Text.Gutter.Font.Size := Max(1, e.Text.Gutter.Font.Size - 1);
      end;
    end;

    // And set the corresponding option
    if WheelDelta > 0 then begin
      devEditor.Font.Size := Max(1, devEditor.Font.Size + 1);
      devEditor.Gutterfont.Size := Max(1, devEditor.Gutterfont.Size + 1);
    end else if WheelDelta < 0 then begin
      devEditor.Font.Size := Max(1, devEditor.Font.Size - 1);
      devEditor.Gutterfont.Size := Max(1, devEditor.Gutterfont.Size - 1);
    end;

    // We don't like to send the actual scrolling message to the editor when zooming
    Abort;
  end;
end;

procedure TMainForm.ImportCBCprojectClick(Sender: TObject);
begin
  with TImportCBForm.Create(Self) do begin
    if ShowModal = mrOK then
      OpenProject(GetFilename);
  end;
end;

procedure TMainForm.CompilerOutputAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State:
  TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  lowersubitem: AnsiString;
  tc:TThemeColor;
begin
  if StartsStr('[Warning] ', Item.SubItems[2]) then begin
    Sender.Canvas.Font.Style := [fsBold];
    StrToThemeColor(tc,devEditor.Syntax.Values[cWN]);
    Sender.Canvas.Font.Color := tc.Foreground;
  end else if StartsStr('[Error] ', Item.SubItems[2]) then begin
    Sender.Canvas.Font.Style := [fsBold];
    Sender.Canvas.Font.Color := dmMain.Cpp.InvalidAttri.Foreground;
  end else if StartsStr('[Hint] ', Item.SubItems[2]) then begin
    Sender.Canvas.Font.Style := [fsBold];  
    Sender.Canvas.Font.Color := dmMain.Cpp.KeyAttri.Foreground;
  end else begin
  end;

  // Make direction stuff bold
  lowersubitem := Trim(LowerCase(Item.SubItems[2]));
  if StartsStr('in ', lowersubitem) or
    StartsStr('at ', lowersubitem) then // direction stuff
    Sender.Canvas.Font.Style := [fsBold];
end;

procedure TMainForm.cmbGenericDropDown(Sender: TObject);
var
  widestwidth, I: integer;
  ComboBox: TComboBox;
begin
  ComboBox := TComboBox(Sender);

  // Set to default width first...
  SendMessage(ComboBox.Handle, CB_SETDROPPEDWIDTH, 0, 0);

  widestwidth := 0;

  // Fix, cmbMembers.Canvas.Font was set to Tahoma for some reason?
  ComboBox.Canvas.Font.Name := 'Courier New';

  // get the max needed with of the items in dropdown state
  for I := 0 to ComboBox.Items.Count - 1 do
    widestwidth := Max(widestwidth, ComboBox.Canvas.TextWidth(ComboBox.Items[I]) + 8); // padding

  if (widestwidth > ComboBox.Width) then begin

    // Add scrollbar width
    if ComboBox.DropDownCount < ComboBox.Items.Count then
      widestwidth := widestwidth + GetSystemMetrics(SM_CXVSCROLL);

    SendMessage(ComboBox.Handle, CB_SETDROPPEDWIDTH, widestwidth, 0);
  end;
end;

procedure TMainForm.NewFileBtnClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := tbMain.ClientToScreen(point(NewFileBtn.Left, NewFileBtn.Top + NewFileBtn.Height));
  TrackPopupMenu(mnuNew.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.X, pt.y, 0, Self.Handle, nil);
end;

procedure TMainForm.actUnCollapseExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.UncollapseAll;
end;

procedure TMainForm.actCollapseExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CollapseAll;
end;

procedure TMainForm.actInsertExecute(Sender: TObject);
var
  pt: TPoint;
begin
  pt := tbSpecials.ClientToScreen(point(Insertbtn.Left, Insertbtn.Top + Insertbtn.Height));
  TrackPopupMenu(InsertItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.X, pt.Y, 0, Self.Handle, nil);
end;

procedure TMainForm.actToggleExecute(Sender: TObject);
var
  pt: TPoint;
begin
  pt := tbSpecials.ClientToScreen(point(Togglebtn.Left, Togglebtn.Top + togglebtn.Height));
  TrackPopupMenu(ToggleBookmarksItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.x, pt.y, 0, Self.Handle, nil);
end;

procedure TMainForm.actGotoExecute(Sender: TObject);
var
  pt: TPoint;
begin
  pt := tbSpecials.ClientToScreen(point(Gotobtn.Left, Gotobtn.Top + Gotobtn.Height));
  TrackPopupMenu(GotoBookmarksItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.x, pt.y, 0, Self.Handle, nil);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.HintHidePause:=300000; //5mins before the hint auto disapear
  fMenuItemHint := TMenuItemHint.Create(self);
  fQuitting:=False;
  fClosing:=False;
  fWindowsTurnedOff:=False;
  fFirstShow := true;
  fCheckSyntaxInBack:=False;
  fCaretList:=TDevCaretList.Create;
  fMessageControlChanged := False;
  fLeftPageControlChanged := False;
  fMonitorTimer := TTimer.Create(Self);


  fDummyCppParser := TCppParser.Create(MainForm.Handle);
  // Backup PATH variable
  devDirs.OriginalPath := GetEnvironmentVariable('PATH');

  // Create a compiler
  fCompiler := TCompiler.Create;
  with fCompiler do begin
    OnLogEntry := LogEntryProc;
    OnOutput := CompOutputProc;
    OnResOutput := CompResOutputProc;
    OnCompEnd := CompEndProc;
    OnCompSuccess := CompSuccessProc;
    OnRunEnd := RunEndProc;
  end;

  //Create a syntaxchecker
  fSyntaxChecker := TCompiler.Create;
  with fSyntaxChecker do begin
    OnLogEntry := LogEntryProc;
    OnOutput := CompOutputProc;
    OnResOutput := CompResOutputProc;
    OnCompEnd := CompEndProc;
    //OnCompSuccess := CompSuccessProc;
    //OnRunEnd := RunEndProc;
  end;


  // Remember long version of paths
  fLogOutputRawData := TStringList.Create;

  // Create a debugger
  fDebugger := TDebugger.Create;
  fDebugger.UseUTF8 := devEditor.UseUTF8ByDefault;
  with fDebugger do begin
    WatchView := Self.WatchView;
  end;

  // Create an editor manager
  fEditorList := TEditorList.Create;
  with fEditorList do begin
    LeftPageControl := Self.EditorPageControlLeft;
    RightPageControl := Self.EditorPageControlRight;
    Splitter := Self.EditorPageControlSplitter;
    Panel := Self.PageControlPanel;
  end;

  // Custom tools
  fTools := TToolController.Create;
  with fTools do begin
    Menu := ToolsMenu;
    Offset := ToolsMenu.IndexOf(PackageManagerItem);
    ToolClick := ToolItemClick;
    BuildMenu;
  end;

  // Don't create the autosave timer when we don't need it
  if devEditor.EnableAutoSave then begin
    AutoSaveTimer := TTimer.Create(nil);
    with AutoSaveTimer do begin
      OnTimer := EditorSaveTimer;
      Interval := devEditor.Interval * 60 * 1000; // miliseconds to minutes
      Enabled := devEditor.EnableAutoSave;
    end;
  end;

  // Create critical section to support a single instance
  fCriticalSection := TCriticalSection.Create;
  fFilesToOpen := TStringList.Create;

  // Apply shortcuts BEFORE TRANSLATING!!!
  with Shortcuts do begin
    Filename := devDirs.Config + DEV_SHORTCUTS_FILE;
    Load(ActionList);
  end;

  // Accept file drags
  DragAcceptFiles(Self.Handle, true);


  // Set bottom page control to previous state
  fPreviousHeight := devData.OutputHeight;
  fPreviousWidth := devData.ProjectWidth;

  actShortenCompPaths.Checked := devData.ShortenCompPaths;

  // Set statusbar to previous state
  actStatusbar.Checked := devData.Statusbar;
  actStatusbarExecute(nil);

  // Set toolbars to previous state.
  // 1) hide all
  tbMain.Visible := False;
  tbEdit.Visible := False;
  tbCompile.Visible := False;
  tbProject.Visible := False;
  tbSpecials.Visible := False;
  tbSearch.Visible := False;
  tbCompilers.Visible := False;
  tbDebug.Visible := False;
  tbUndo.Visible := False;

  // Set toolbars to previous state.
  // 2) position and Visibility
  tbMain.Left := devData.ToolbarMainX;
  tbMain.Top := devData.ToolbarMainY;
  tbMain.Visible := devData.ToolbarMain;

  tbEdit.Left := devData.ToolbarEditX;
  tbEdit.Top := devData.ToolbarEditY;
  tbEdit.Visible := devData.ToolbarEdit;

  tbCompile.Left := devData.ToolbarCompileX;
  tbCompile.Top := devData.ToolbarCompileY;
  tbCompile.Visible := devData.ToolbarCompile;

  tbProject.Left := devData.ToolbarProjectX;
  tbProject.Top := devData.ToolbarProjectY;
  tbProject.Visible := devData.ToolbarProject;

  tbSpecials.Left := devData.ToolbarSpecialsX;
  tbSpecials.Top := devData.ToolbarSpecialsY;
  tbSpecials.Visible := devData.ToolbarSpecials;

  tbSearch.Left := devData.ToolbarSearchX;
  tbSearch.Top := devData.ToolbarSearchY;
  tbSearch.Visible := devData.ToolbarSearch;


  tbCompilers.Left := devData.ToolbarCompilersX;
  tbCompilers.Top := devData.ToolbarCompilersY;
  tbCompilers.Visible := devData.ToolbarCompilers;

  tbDebug.Left := devData.ToolbarDebugX;
  tbDebug.Top := devData.ToolbarDebugY;
  tbDebug.Visible := devData.ToolbarDebug;

  tbUndo.Left := devData.ToolbarUndoX;
  tbUndo.Top := devData.ToolbarUndoY;
  tbUndo.Visible := devData.ToolbarUndo;

  // Set toolbars to previous state.
  // 3) UI components
  ToolMainItem.Checked := devData.ToolbarMain;
  ToolEditItem.Checked := devData.ToolbarEdit;
  ToolCompileandRunItem.Checked := devData.ToolbarCompile;
  ToolProjectItem.Checked := devData.ToolbarProject;
  ToolSpecialsItem.Checked := devData.ToolbarSpecials;
  ToolSearchItem.Checked := devData.ToolbarSearch;
  ToolCompilersItem.Checked := devData.ToolbarCompilers;
  ToolDebugItem.Checked := devData.ToolbarDebug;
  ToolUndoItem.Checked := devData.ToolbarUndo;

  // PageControl settings
  fEditorList.SetPreferences(devData.MsgTabs, devData.MultiLineTab);

  // Create datamod
  dmMain := TdmMain.Create(Self);
  with dmMain do begin
    MRUMenu := ClearhistoryItem;
    MRUClick := Self.MRUClick;
    CodeMenu := InsertItem;
    CodePop := InsertPopItem;
    CodeClick := CodeInsClick;
    CodeOffset := 2;
    LoadDataMod;
  end;

  // Create icon themes
  devImageThemes := TDevImageThemeFactory.Create;
  devImageThemes.LoadFromDirectory(devDirs.Themes);

  // Set languages and other first time stuff
  if devData.First or (devData.Language = '') then
    Lang.SelectLanguage
  else
    Lang.Open(devData.Language);

  LoadTheme;

  // Load bookmarks, captions and hints
  LoadText;

  // Load the current compiler set
  devCompilerSets.LoadSets;

  // Update toolbar
  UpdateCompilerList;

  // Try to fix the file associations. Needs write access to registry, which might cause exceptions to be thrown
  DDETopic := DevCppDDEServer.Name;
  if devData.CheckAssocs then begin
    try
      CheckAssociations(true); // check and fix
    except
      MessageBox(Application.Handle, PAnsiChar(Lang[ID_ENV_UACERROR]), PAnsiChar(Lang[ID_ERROR]), MB_OK);
      devData.CheckAssocs := false; // don't bother again
    end;
  end;

  UpdateClassBrowsing;
  UpdateStatementsType;

  // Showing for the first time? Maximize
  if devData.First then
    WindowState := wsMaximized
  else // Remember window placement
    devData.WindowState.SetPlacement(Self.Handle);

  // We need this variable during the whole startup process
  devData.First := FALSE;

  LeftPageControl.Constraints.MinWidth := LeftPageControl.Width - LeftClassSheet.Width;
  MessageControl.Constraints.MinHeight := MessageControl.Height - CompSheet.Height;
  //windows xp hack
  if Win32MajorVersion < 6 then begin
    LeftPageControl.TabPosition := tpTop;
  end;

  //Tabnine
  fTabnine:=TTabnine.Create;
  if devEditor.UseTabnine then
    startTabnine;

  //Load Colors
  LoadColor;

  // Set left page control to previous state
  actProjectManager.Checked := devData.ShowLeftPages;
  LeftPageControl.ActivePageIndex := devData.LeftActivePage;
  ClassBrowser.TabVisible := (LeftPageControl.ActivePage = LeftClassSheet);
  fLeftPageControlChanged := False;
  actProjectManagerExecute(nil);
//  LeftPageControl.Width := devData.ProjectWidth;
//  MessageControl.Height := devData.OutputHeight;
  LeftProjectSheet.TabVisible := False;
  

  actOpenWindowsTerminal.Visible:= devEnvironment.HasWindowsTerminal;
  actOpenCurrentFolderInWindowsTerminal.Visible := actOpenWindowsTerminal.Visible;

  if StartsStr('*',devData.FileBrowserFolder) then begin
    fileBrowser.CurrentFolder := IncludeTrailingPathDelimiter(devDirs.Exec) + Copy(devData.FileBrowserFolder,2,MaxInt);
  end else
    fileBrowser.CurrentFolder := devData.FileBrowserFolder;
  fileBrowser.OnlyShowDevFiles := devData.FileBrowserOnlyShowDevFiles;
  actOnlyShowDevFiles.Checked := devData.FileBrowserOnlyShowDevFiles;

  fileBrowser.Monitor := FileMonitor;


end;

procedure TMainForm.EditorPageControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PageIndex: integer;
  newhint: AnsiString;
  e: TEditor;
  SenderPageControl: TPageControl;
  rect,closeRect:TRect;
  size:integer;
begin
  SenderPageControl := TPageControl(Sender);
  PageIndex := SenderPageControl.IndexOfTabAt(X, Y);
  if PageIndex <> -1 then begin
    e := fEditorList.GetEditor(PageIndex, SenderPageControl);
    if Assigned(e) then
      newhint := e.FileName;
    rect := SenderPageControl.TabRect(PageIndex);
    closeRect.Bottom := Rect.Bottom;
    closeRect.Top := Rect.Top;
    size := Rect.Bottom - Rect.Top - 4;
    if (size>20) then
      size := 20;
    closeRect.Top := Rect.Top + (Rect.Bottom - Rect.Top - size) div 2;
    closeRect.Bottom := closeRect.Top + size;
    closeRect.Right := Rect.Right - 5;
    closeRect.Left := closeRect.Right - size;
    if (X>=closeRect.Left) and (X<=closeRect.Right)
      and (Y>=closeRect.Top) and (Y<=closeRect.Bottom) then begin
      newhint := Lang[ID_ITEM_CLOSEEDITORWINDOW];
    end;
  end else
    newhint := '';

  // Only call CancelHint when the hint changes
  if (newhint <> fCurrentPageHint) then begin
    Application.CancelHint;
    fCurrentPageHint := newhint;
    SenderPageControl.Hint := newhint;
  end;
end;

procedure TMainForm.actBreakPointExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.ToggleBreakPoint(e.Text.CaretY);
end;

procedure TMainForm.OnInputEvalReady(const evalvalue: AnsiString);
begin
  EvalOutput.Text := evalvalue;
  fDebugger.OnEvalReady := nil;
end;

procedure TMainForm.EvaluateInputKeyPress(Sender: TObject; var Key: Char);
begin
  if fDebugger.Executing and (not fDebugger.Reader.CommandRunning) then begin
    if Key = Chr(VK_RETURN) then begin
      if Length(EvaluateInput.Text) > 0 then begin

        // Disable key, but make sure not to remove selected text
        EvaluateInput.SelStart := 0;
        EvaluateInput.SelLength := 0;
        Key := #0;

        fDebugger.OnEvalReady := OnInputEvalReady;
        fDebugger.SendCommand('print', EvaluateInput.Text,False);

        // Add to history
        if EvaluateInput.Items.IndexOf(EvaluateInput.Text) = -1 then
          EvaluateInput.AddItem(EvaluateInput.Text, nil);
      end;
    end;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  FileCount: Integer;
begin
  if not fFirstShow then
    Exit;

  fFirstShow := false;

  // Below a possible fix for the disappearing of controls in MessageControl is given:
  // Toggle visibility of bottom page control here.
  // If we do this in OnCreate, it messes with alignment options
  OpenCloseMessageSheet(False);

  // Open files passed to us (HAS to be done at FormShow)
  // This includes open file commands send to us via WMCopyData
  FileCount := ParseParameters(GetCommandLine);


  // Open them according to OpenFileList rules
  OpenFileList(fFilesToOpen);
  if FileCount = 0 then begin
    if devEditor.LoadLastOpens then
      LoadLastOpens;
    DeleteFile(devDirs.Config + DEV_LASTOPENS_FILE);
    UpdateAppTitle;
  end;
  fFilesToOpen.Clear;

  // do not show tips if Dev-C++ is launched with a file and only slow
  // when the form shows for the first time, not when going fullscreen too
  if devData.ShowTipsOnStart and (FileCount = 0) then
    actShowTips.Execute;
end;

procedure TMainForm.actStepOutExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    WatchView.Items.BeginUpdate();
    fDebugger.InvalidateAllVars;
    fDebugger.SendCommand('finish', '');
    UpdateDebugInfo;
    if assigned(CPUForm) then
      CPUForm.UpdateInfo;
    WatchView.Items.EndUpdate();
      //fDebugger.RefreshWatchVars;
  end;
end;

procedure TMainForm.actRunToCursorExecute(Sender: TObject);
var
  e:TEditor;
begin
  if fDebugger.Executing then begin
    e:=fEditorList.GetEditor;
    if assigned(e) then begin
      WatchView.Items.BeginUpdate();
      fDebugger.InvalidateAllVars;
      fDebugger.SendCommand('tbreak', ' '+IntToStr(e.Text.CaretY));
      fDebugger.SendCommand('continue', '');
      UpdateDebugInfo;
      if assigned(CPUForm) then
        CPUForm.UpdateInfo;
      WatchView.Items.EndUpdate();
      //fDebugger.RefreshWatchVars;
    end;
  end;
end;

procedure TMainForm.WatchViewAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  curnode: TTreeNode;
  tc:TThemeColor;  
begin
  curnode := node;
  while Assigned(curnode.Parent) do
    curnode := curnode.Parent;

  // TODO: why does this happen?
  if not Assigned(curnode.Data) then
    Exit;

  // Paint top level items in red if they aren't found
  if (curnode.Level = 0) and (PWatchVar(curnode.Data)^.gdbindex = -1) then begin
    if cdsSelected in State then begin
      strToThemeColor(tc, devEditor.Syntax.Values[cSel]);
      Sender.Canvas.Font.Color := tc.Foreground;
      Sender.Canvas.Brush.Color := tc.Background;
    end else begin
      Sender.Canvas.Font.Color := dmMain.Cpp.WhitespaceAttribute.Background;
      Sender.Canvas.Font.Color := dmMain.Cpp.CommentAttri.Foreground;
    end;
  end else begin
    if cdsSelected in State then begin
      strToThemeColor(tc, devEditor.Syntax.Values[cSel]);
      Sender.Canvas.Font.Color := tc.Foreground;
      Sender.Canvas.Brush.Color := tc.Background;
    end else begin
      Sender.Canvas.Font.Color := dmMain.Cpp.WhitespaceAttribute.Background;
      Sender.Canvas.Font.Color := dmMain.Cpp.IdentifierAttri.Foreground;
    end;
  end;
end;


{
procedure TMainForm.FindOutputAdvancedCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  LineToDraw: AnsiString;
  BoldStart, BoldLen, i: integer;
  Rect: TRect;
  OldBrushColor, OldFontColor: TColor;

  procedure Draw(const s: AnsiString);
  var
    sizerect: TRect;
  begin
    DrawText(Sender.Canvas.Handle, PAnsiChar(s), Length(s), rect, DT_EXPANDTABS or DT_NOPREFIX);

    // Get text extent
    FillChar(sizerect, sizeof(sizerect), 0);
    DrawText(Sender.Canvas.Handle, PAnsiChar(s), Length(s), sizerect, DT_CALCRECT or DT_EXPANDTABS or DT_NOCLIP or
      DT_NOPREFIX);
    Inc(rect.Left, sizerect.Right - sizerect.Left + 1); // 1 extra pixel for extra width caused by bold
  end;
begin
  if SubItem = 4 then begin

    // Get rect of subitem to draw
    Rect := Item.DisplayRect(drBounds);
    for i := 0 to SubItem - 1 do begin
      Rect.Left := Rect.Left + Sender.Column[i].Width;
      Rect.Right := Rect.Right + Sender.Column[i].Width;
    end;

    OldBrushColor := Sender.Canvas.Brush.Color;
    OldFontColor := Sender.Canvas.Font.Color;
    // Draw background
    if (cdsSelected in State) then begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Font.Color := clHighlightText;
      Sender.Canvas.FillRect(rect);
    end;

    // Get text to draw
    LineToDraw := Item.SubItems[SubItem - 1];

    // Make text location appear 'native', like Windows would draw it
    OffsetRect(Rect, 4, 2);

    // Get location of bold part
    BoldStart := StrToIntDef(Item.SubItems[1], 1);
    BoldLen := Integer(Item.Data);

    // Draw part before bold highlight
    Draw(Copy(LineToDraw, 1, BoldStart - 1));

    // Enable bold
    Sender.Canvas.Font.Style := [fsBold];
    Sender.Canvas.Refresh;

    // Draw bold highlight
    Draw(Copy(LineToDraw, BoldStart, BoldLen));

    // Disable bold
    Sender.Canvas.Font.Style := [];
    Sender.Canvas.Refresh;

    // Draw part after bold highlight
    Draw(Copy(LineToDraw, BoldStart + BoldLen, MaxInt));

    // Restore colors
    Sender.Canvas.Brush.Color := OldBrushColor;
    Sender.Canvas.Font.Color := OldFontColor;

    DefaultDraw := false;
  end else begin
    DefaultDraw := True;
  end;

end;
}

{
procedure TMainForm.FindOutputAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  SendMessage(FindOutput.Handle, WM_CHANGEUISTATE, MAKEWPARAM(UIS_SET, UISF_HIDEFOCUS), 0);
end;
}

procedure TMainForm.CompilerOutputAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage:
  TCustomDrawStage; var DefaultDraw: Boolean);
begin
  SendMessage(CompilerOutput.Handle, WM_CHANGEUISTATE, MAKEWPARAM(UIS_SET, UISF_HIDEFOCUS), 0);
end;

procedure TMainForm.actSearchAgainExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if not Assigned(e) then
    Exit;

  // Repeat action if it was a finding action
  if Assigned(FindForm) then begin
    with FindForm do begin
      if FindTabs.TabIndex < 2 then begin // it's a find action

        // Disable entire scope searching
        if grpOrigin.Visible then
          rbEntireScope.Checked := false;

        // Always search forwards
        if grpDirection.Visible then
          rbForward.Checked := true;

        btnExecute.Click;
        e.Text.SetFocus;
      end;
    end;
  end;
end;

procedure TMainForm.actRevSearchAgainExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if not Assigned(e) then
    Exit;

  // Repeat action if it was a finding action
  if Assigned(FindForm) then begin
    with FindForm do begin
      if FindTabs.TabIndex < 2 then begin // it's a find action

        // Disable entire scope searching
        if grpOrigin.Visible then
          rbEntireScope.Checked := false;

        // Always search backwards
        if grpDirection.Visible then
          rbBackward.Checked := true;

        btnExecute.Click;
        e.Activate;
      end;
    end;
  end;
end;

procedure TMainForm.FindOutputDeletion(Sender: TObject; Item: TListItem);
begin
  if Application.Terminated then
    Exit; // form is being destroyed, don't use Lang which has been freed already...
  {
  if FindOutput.Items.Count > 1 then
    FindSheet.Caption := Lang[ID_SHEET_FIND] + ' (' + IntToStr(FindOutput.Items.Count - 1) + ')'
  else
    FindSheet.Caption := Lang[ID_SHEET_FIND];
  }  
end;

procedure TMainForm.CompilerOutputDeletion(Sender: TObject; Item: TListItem);
begin
  if Application.Terminated or fQuitting then
    Exit; // form is being destroyed
  if (CompilerOutput.Items.Count > 1) then
    CompSheet.Caption := Lang[ID_SHEET_COMP] + ' (' + IntToStr(CompilerOutput.Items.Count - 1) + ')'
  else
    CompSheet.Caption := Lang[ID_SHEET_COMP];
end;

procedure TMainForm.ResourceOutputDeletion(Sender: TObject; Item: TListItem);
begin
  if Application.Terminated then
    Exit; // form is being destroyed
  if ResourceOutput.Items.Count > 1 then
    ResSheet.Caption := Lang[ID_SHEET_RES] + ' (' + IntToStr(ResourceOutput.Items.Count - 1) + ')'
  else
    ResSheet.Caption := Lang[ID_SHEET_RES];
end;

procedure TMainForm.actStopExecuteUpdate(Sender: TObject);
begin
  if Assigned(fProject) then
    TCustomAction(Sender).Enabled := (not (fProject.Options.typ = dptStat) and devExecutor.Running) or
      fDebugger.Executing
  else
    TCustomAction(Sender).Enabled := ((EditorList.PageCount > 0) and devExecutor.Running) or fDebugger.Executing;
end;

procedure TMainForm.actUpdateIndent(Sender: TObject);
begin // special action to prevent tabs from being processed when using the find form
  TCustomAction(Sender).Enabled := (EditorList.PageCount > 0) and (not Assigned(FindForm) or not FindForm.Showing);
end;

procedure TMainForm.actDeleteLineExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(ecDeleteLine, #0, nil);
end;

procedure TMainForm.cmbCompilersChange(Sender: TObject);
var
  index: integer;
  e: TEditor;

  procedure ChangeProjectCompilerSet;
  begin
    // If true, continue, otherwise, revert selection change
    if MessageDlg(Lang[ID_POPT_DISCARDCUSTOM], mtConfirmation, [mbYes, mbNo], 0) = mrNo then begin // user cancels change
      TComboBox(Sender).ItemIndex := fOldCompilerToolbarIndex; // undo index change
      Exit;
    end;

    // Notify project
    // Discard changes made to the old set...
    fProject.Options.CompilerSet := index;
    fProject.Options.CompilerOptions := devCompilerSets[index].INIOptions;
    fProject.Modified := true;
  end;

  procedure ChangeNonProjectCompilerSet;
  begin
    // Notify compiler list
    devCompilerSets.DefaultSetIndex := index;
    devCompilerSets.SaveSetList;
  end;
begin
  index := TComboBox(Sender).ItemIndex;
  if index = fOldCompilerToolbarIndex then
    Exit;

  // Check if the current file belongs to a project
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    if e.InProject and Assigned(fProject) then begin
      ChangeProjectCompilerSet;
    end else begin
      ChangeNonProjectCompilerSet;
    end;
    CompileClean;
    // No editors have been opened. Check if a project is open
  end else if Assigned(fProject) then begin
    ChangeProjectCompilerSet;
    CompileClean;
    // No project, no editor, modify global
  end else begin
    ChangeNonProjectCompilerSet;
  end;

  // Mention change to compiler sets
  //devCompilerSets.OnCompilerSetChanged(
  //  devCompilerSets[fOldCompilerToolbarIndex],
  //  devCompilerSets[index]);

  fOldCompilerToolbarIndex := index;

end;

procedure TMainForm.actDuplicateLineExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(ecDuplicateLine, #0, nil);
end;

procedure TMainForm.actMoveSelUpExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(ecMoveSelUp, #0, nil);
end;

procedure TMainForm.actMoveSelDownExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(ecMoveSelDown, #0, nil);
end;

procedure TMainForm.actCodeCompletionUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and e.Text.Focused;
end;

procedure TMainForm.actCodeCompletionExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.ShowCompletion(True);
end;

procedure TMainForm.actPackageManagerExecute(Sender: TObject);
var
  s: AnsiString;
begin
  s := IncludeTrailingBackslash(devDirs.Exec) + PACKMAN_PROGRAM;
  if FileExists(s) then
    ExecuteFile(s, '', IncludeTrailingBackslash(devDirs.Exec), SW_SHOW);
end;

procedure TMainForm.actHelpExecute(Sender: TObject);
begin
  OpenHelpFile('index.htm');
end;

procedure TMainForm.actShortenCompPathsExecute(Sender: TObject);
var
  I: integer;
begin
  devData.ShortenCompPaths := not devData.ShortenCompPaths;

  // Re-add data to undo/redo shortening
  LogOutput.Lines.BeginUpdate;
  try
    LogOutput.Lines.Clear;
    for I := 0 to fLogOutputRawData.Count - 1 do begin
      if devData.ShortenCompPaths then
        LogOutput.Lines.Add(ShortenLogOutput(fLogOutputRawData[i]))
      else
        LogOutput.Lines.Add(fLogOutputRawData[i]);
    end;
  finally
    LogOutput.Lines.EndUpdate;
  end;
end;

procedure TMainForm.actSyntaxCheckFileExecute(Sender: TObject);
var
  e:TEditor;
begin
  actStopExecuteExecute(Self);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  e:=EditorList.GetEditor();
  if not assigned(e) then
    Exit;
  if not PrepareForCompile(cttFile) then
    Exit;
  if e.InProject then
    fCompiler.Project := MainForm.fProject;
  fCompiler.CheckSyntax;
end;

procedure TMainForm.LeftPageControlChange(Sender: TObject);
begin
  ClassBrowser.TabVisible := (LeftPageControl.ActivePage = LeftClassSheet);
  fLeftPageControlChanged := True;
  OpenCloseLeftPageControl(true);
end;

procedure TMainForm.actSwapEditorExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    fEditorList.SwapEditor(e);
end;

procedure TMainForm.actSwapEditorUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fEditorList.PageCount > 1;
end;

procedure TMainForm.actSaveAllUpdate(Sender: TObject);
var
  e: TEditor;
  I: integer;
begin
  if Assigned(fProject) and fProject.Modified then begin
    TCustomAction(Sender).Enabled := True;
    Exit;
  End;
  for I := 0 to fEditorList.PageCount - 1 do begin
    e := fEditorList[i];
    if Assigned(e) and (e.Text.Modified or (e.FileName = '')) then begin
      TCustomAction(Sender).Enabled := True;
      Exit;
    end;
  end;
  TCustomAction(Sender).Enabled := False;
end;

procedure TMainForm.PageControlPanelResize(Sender: TObject);
begin
  fEditorList.OnPanelResize(Sender);
end;

procedure TMainForm.actOpenFolderExecute(Sender: TObject);
var
  e: TEditor;
  Folder: AnsiString;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    Folder := ExtractFilePath(e.FileName);
    if Folder <> '' then
      ShellExecute(Application.Handle, 'open', 'explorer.exe', PAnsiChar(Folder), nil, SW_SHOWNORMAL);
  end;
end;

procedure TMainForm.actGotoBreakPointExecute(Sender: TObject);
var
  e: TEditor;
  FileName: AnsiString;
begin
  FileName := fDebugger.BreakPointFile;
  if FileName <> '' then begin
    e := fEditorList.GetEditorFromFileName(FileName);
    if Assigned(e) then begin
      e.GotoActiveBreakpoint;
      e.Activate;
    end;
  end;
end;

procedure TMainForm.actToggleCommentInlineExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(ecCommentInline, #0, nil);
end;

procedure TMainForm.actToggleCommentInlineUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and e.Text.SelAvail;
end;

procedure TMainForm.actFormatCurrentFileExecute(Sender: TObject);
var
  e: TEditor;
  OldCaretXY: TBufferCoord;
  OldTopLine: integer;
  oldCursor : TCursor;
begin
  if devFormatter.Validate then begin
    e := fEditorList.GetEditor;
    if Assigned(e) then begin
      oldCursor := e.Text.Cursor;
      e.Text.Cursor := crHourglass;
      e.BeginUpdate;
      try
      // Save for undo list creation
      OldTopLine := e.Text.TopLine;
      OldCaretXY := e.Text.CaretXY;

      devFormatter.FormatMemory(e, devFormatter.FullCommand);

      // Attempt to not scroll view
      e.Text.TopLine := OldTopLine;
      e.Text.CaretXY := OldCaretXY;

      e.Reparse;
      finally
        e.EndUpdate;
        e.Text.Cursor := oldCursor;
      end;
    end;
  end else
    MessageDlg(Lang[ID_FORMATTER_NOTVALID], mtWarning, [mbOK], 0);
end;

procedure TMainForm.actFormatOptionsExecute(Sender: TObject);
begin
  with TFormatterOptionsForm.Create(nil) do try
    if ShowModal = mrOk then begin
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.FindOutputSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    Item.Caption := '>'
  else
    Item.Caption := '';
end;

procedure TMainForm.actRunTestsExecute(Sender: TObject);
begin
  with TTestClass.Create do try
    TestAll;
  finally
    Free;
  end;
end;

procedure TMainForm.WMQueryEndSession(var   Msg:TMessage);
begin
  fWindowsTurnedOff := True;
  Close;
  msg.Result:=1;
end;

procedure TMainForm.WMMenuSelect(var Msg: TWMMenuSelect) ;
var
  menuItem : TMenuItem;
  hSubMenu : HMENU;
begin
  inherited; // from TCustomForm (so that Application.Hint is assigned)
  menuItem := nil;
  if (Msg.MenuFlag <> $FFFF) or (Msg.IDItem <> 0) then begin
  {
    if Msg.MenuFlag and MF_POPUP = MF_POPUP then begin
      hSubMenu := GetSubMenu(Msg.Menu, Msg.IDItem) ;
      menuItem := Self.Menu.FindItem(hSubMenu, fkHandle) ;
    end else begin
  }
      menuItem := Self.MainMenu.FindItem(Msg.IDItem, fkCommand) ;
  end;
  fMenuItemHint.DoActivateHint(menuItem) ;
end; (*WMMenuSelect*)

procedure TMainForm.WMCopyData(var Message: TMessage);
var
  MessageData: AnsiString;
begin
  MessageData := GetSentStructData(Message);
  if MessageData <> '' then begin
    fCriticalSection.Acquire;
    try
      ParseParameters(MessageData); // not thread safe

      // Ff the window has already opened, force open here
      // Otherwise, these files get added to the big file of files to open
      // when the main instance executes FormShow.
      if not fFirstShow then begin
        OpenFileList(fFilesToOpen);
        fFilesToOpen.Clear;
      end;
      ShowWindow(Application.Handle,SW_RESTORE);
      Application.BringToFront;
    finally
      fCriticalSection.Release;
    end;
  end;
end;

function TMainForm.ParseParameters(const Parameters: WideString): Integer;
type
  TPWideCharArray = array[0..0] of PWideChar;
var
  I, ParameterCount: Integer;
  ParameterList: PPWideChar;
  Item: WideString;
begin
  Result := 0;

  // Convert string to list of items
  ParameterList := CommandLineToArgvW(PWideChar(Parameters), ParameterCount);
  if not Assigned(ParameterList) then
    Exit;

  // Walk the list
  I := 1; // skip first one
  while I < ParameterCount do begin
    Item := TPWideCharArray(ParameterList^)[i];

    // Skip the configuration redirect stuff
    if Item = '-c' then begin
      I := I + 2;
      Continue;
    end;

    fFilesToOpen.Add(Item);
    Inc(I);
  end;
  Result := fFilesToOpen.Count;

  // Free list of pointers
  LocalFree(Cardinal(ParameterList));
end;

procedure TMainForm.actDonateExecute(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'open',
    PAnsiChar('https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=7FD675DNV8KKJ'), nil, nil,
    SW_SHOWNORMAL);
end;

procedure TMainForm.actRenameSymbolExecute(Sender: TObject);
var
  Editor: TEditor;
  word,newword: ansiString;
  OldCaretXY: TBufferCoord;
  oldCursor : TCursor;
  parser:TCppParser;
begin
  Editor := fEditorList.GetEditor;
  if Assigned(Editor) then begin
    Editor.BeginUpdate;
    ClassBrowser.BeginTreeUpdate;
    oldCursor := Editor.Text.Cursor;
    Editor.Text.Cursor := crHourglass;
    try
    word := Editor.Text.WordAtCursor;
    // MessageBox(Application.Handle,PAnsiChar(Concat(word,IntToStr(offset))),     PChar( 'Look'), MB_OK);
    if Word = '' then begin
      Exit;
    end;

    if not IsIdentifier(word) then begin
      MessageDlg(Format(Lang[ID_ERR_NOT_IDENTIFIER],[word]), mtInformation, [mbOK], 0);
      Exit;
    end;

    //Test if newName is a C++ keyword
    if IsKeyword(word) then begin
      MessageDlg(Format(Lang[ID_ERR_IS_KEYWORD],[word]), mtInformation, [mbOK], 0);
      Exit;
    end;

    with TRenameForm.Create(Self) do try
      txtVarName.Text := word;
      txtVarName.SelectAll;
      if ShowModal <> mrOK then
        Exit;

      newword := txtVarName.Text;

      if word = newword then
        Exit;
        
      OldCaretXY := Editor.Text.CaretXY;
      if assigned(fProject) and editor.InProject then begin
        parser := TCppParser.Create(self.Handle);
        parser.OnGetFileStream := EditorList.GetStreamFromOpenedEditor;
        ResetCppParser(parser);
        parser.Enabled := True;
      end else begin
        parser := editor.CppParser;
      end;
      with TRefactorer.Create(devRefactorer,parser) do try
        if assigned(fProject) and editor.InProject then begin
          //here we must reparse the project in sync, or rename may fail
          SetCppParserProject(parser, fProject);
          parser.ParseFileList(false);
        end else begin
          //here we must reparse the file in sync, or rename may fail
          parser.ParseFile(editor.FileName, editor.InProject, false, false);
        end;
        RenameSymbol(Editor,OldCaretXY,word,newword,fProject);
      finally
        Free;
        if parser <> editor.CppParser then
          parser.Free;
      end;

      //Editor.Save;
      if assigned(fProject) and editor.InProject then begin
        ScanActiveProject(true);
      end else
        editor.reparse;
    finally
      Free;
    end;
    finally
      Editor.EndUpdate;
      ClassBrowser.EndTreeUpdate;
      Editor.Text.Cursor := oldCursor;
    end;
  end;
end;

procedure TMainForm.actUseUTF8Execute(Sender: TObject);
var
  e:TEditor;
  choice,i: integer;
begin
  e:=fEditorList.GetEditor;
  if assigned(e) then begin
    if e.Text.Modified then begin
      choice := MessageDlg(Lang[ID_ERR_FILEMODIFIED], mtConfirmation, [mbYes, mbNo, mbAbort], 0) ;
      if choice = mrYes then
        e.SaveFile(e.FileName)
      else if choice = mrAbort then
          Exit;
    end;
    //e.UseUTF8 := not e.UseUTF8;
    e.LoadFile(e.FileName);
    if e.InProject and Assigned(fProject) then begin
      for i:=0 to fProject.Units.Count-1 do begin
        if e = fProject.Units[i].Editor then begin
          fProject.Units[i].Encoding := e.EncodingOption;
          fProject.Modified:=True;
          break;
        end;
      end;
    end;
    UpdateFileEncodingStatusPanel;
  end;
end;

procedure TMainForm.actUseUTF8Update(Sender: TObject);
var
  e:TEditor;
begin
  e:=fEditorList.GetEditor;
  if not assigned(e) then begin
    actUseUTF8.Enabled := False;
  end else begin
    actUseUTF8.Enabled := True;
    //actUseUTF8.Checked := e.UseUTF8;
  end;
end;


procedure TMainForm.actMsgDisplayGDBCommandsExecute(Sender: TObject);
begin
  devDebugger.ShowCommandLog := not devDebugger.ShowCommandLog;
end;

procedure TMainForm.actMsgDisplayGDBCommandsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked:=devDebugger.ShowCommandLog;
end;

procedure TMainForm.actMsgDisplayGDBAnnotationsUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := devDebugger.ShowAnnotations;
end;

procedure TMainForm.actMsgDisplayGDBAnnotationsExecute(Sender: TObject);
begin
  devDebugger.ShowAnnotations := not devDebugger.ShowAnnotations;
end;

procedure TMainForm.actBreakPointPropertiesUpdate(Sender: TObject);
var
  e:TEditor;
  breakId: integer;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    breakId:=fDebugger.GetBreakPointIndexOnLine(e.GutterClickedLine,e);
    TAction(Sender).Visible := (breakId <> -1);
  end;
end;

procedure TMainForm.actBreakPointPropertiesExecute(Sender: TObject);
var
  e:TEditor;
  breakId: integer;
  oldCond: String;
  newCond: String;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    breakId:=fDebugger.GetBreakPointIndexOnLine(e.GutterClickedLine,e);
    if breakId = -1 then
      Exit;
    oldCond := PBreakPoint(fDebugger.BreakPointList[breakId])^.condition;
    newCond := Trim(ShowInputBox(LANG[ID_ITEM_BREAKPOINT_PROP], LANG[ID_MSG_BREAKPOINT_CONDITION]
        , oldCond));
    fDebugger.SetBreakPointCondition(breakId,newCond);
  end;
end;

procedure TMainForm.StackTraceClick(Sender: TObject);
var
  sel: TListItem;
  e: TEditor;
  i: integer;
begin
  sel := StackTrace.Selected;
  if Assigned(sel) then begin
    e := EditorList.GetEditorFromFileName(sel.SubItems[0]);
    if Assigned(e) then begin
      e.SetCaretPosAndActivate(StrToIntDef(sel.SubItems[1], 1), 1);
    end;
    i := sel.Index;
    if Debugger.Executing then begin
      WatchView.Items.BeginUpdate();
      Debugger.SendCommand('select-frame',IntToStr(i));
      //update register info
      // Load the registers...
      Debugger.SendCommand('info', 'registers');
      if (devDebugger.BlendMode) then
        Debugger.SendCommand('disas','/s')
      else
        Debugger.SendCommand('disas','');
      fDebugger.InvalidateAllVars;
      //Debugger.SendCommand('display','');
      fDebugger.RefreshWatchVars;
      WatchView.Items.EndUpdate();
    end;
  end;
end;

procedure TMainForm.OnBacktraceReady;
var
  I: integer;
  item: TListItem;
begin
  StackTrace.Items.BeginUpdate;
  StackTrace.Clear;
  if Assigned(fDebugger.Reader) then begin
    for I := 0 to fDebugger.Reader.Backtrace.Count - 1 do begin
      item := StackTrace.Items.Add;
      item.Caption := PTrace(fDebugger.Reader.Backtrace.Items[I])^.funcname;
      item.SubItems.Add(PTrace(fDebugger.Reader.Backtrace.Items[I])^.filename);
      item.SubItems.Add(PTrace(fDebugger.Reader.Backtrace.Items[I])^.line);
    end;
  end;
  StackTrace.Items.EndUpdate;

  if Assigned(fDebugger.Reader) then begin
    // Free list for reuse
    for I := 0 to fDebugger.Reader.Backtrace.Count - 1 do
      Dispose(PTrace(fDebugger.Reader.Backtrace.Items[I]));
    fDebugger.Reader.Backtrace.Clear;
  end;
end;

procedure TMainForm.OnBreakPointsChanged;
var
  I: integer;
  item: TListItem;
  filename: string;
begin
  BreakpointsView.Items.BeginUpdate;
  BreakpointsView.Clear;
  for I := 0 to fDebugger.BreakPointList.Count - 1 do begin
      item := BreakpointsView.Items.Add;
      filename := PBreakPoint(fDebugger.BreakPointList[i])^.editor.FileName;

      item.Caption := filename;
      item.SubItems.Add(IntToStr(PBreakPoint(fDebugger.BreakPointList[I])^.line));
      item.SubItems.Add(PBreakPoint(fDebugger.BreakPointList[I])^.condition);
  end;
  BreakpointsView.Items.EndUpdate;

end;

procedure TMainForm.actConvertToUTF8BomUpdate(Sender: TObject);
var
  e:TEditor;
begin
  e:=fEditorList.GetEditor;
  if not assigned(e) then begin
    actConvertToUTF8Bom.Enabled := False;
  end else begin
    actConvertToUTF8Bom.Enabled := (e.FileEncoding <> etUTF8Bom);
  end;
end;

procedure TMainForm.actConvertToUTF8BomExecute(Sender: TObject);
var
  e:TEditor;
  i:integer;
begin
  e:=fEditorList.GetEditor;
  if MessageDlg(Lang[ID_MSG_CONVERTTOUTF8BOM], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    e.EncodingOption := etUTF8Bom;
    e.Text.Modified := True; // set modified flag to make sure save.
    e.Save;
    UpdateFileEncodingStatusPanel;
    // set project unit's utf-8 flag 
    if e.InProject and Assigned(fProject) then begin
      for i:=0 to fProject.Units.Count-1 do begin
        if e = fProject.Units[i].Editor then begin
          fProject.Units[i].Encoding := e.EncodingOption;
          fProject.Modified:=True;
          break;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.actConvertToUTF8Update(Sender: TObject);
var
  e:TEditor;
begin
  e:=fEditorList.GetEditor;
  if not assigned(e) then begin
    actConvertToUTF8.Enabled := False;
  end else begin
    actConvertToUTF8.Enabled := (e.FileEncoding <> etUTF8);
  end;
end;

procedure TMainForm.actConvertToUTF8Execute(Sender: TObject);
var
  e:TEditor;
  i:integer;
begin
  e:=fEditorList.GetEditor;
  if MessageDlg(Lang[ID_MSG_CONVERTTOUTF8], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    e.EncodingOption := etUTF8;
    e.Text.Modified := True; // set modified flag to make sure save.
    e.Save;
    UpdateFileEncodingStatusPanel;
    // set project unit's utf-8 flag 
    if e.InProject and Assigned(fProject) then begin
      for i:=0 to fProject.Units.Count-1 do begin
        if e = fProject.Units[i].Editor then begin
          fProject.Units[i].Encoding := e.EncodingOption;
          fProject.Modified:=True;
          break;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.BreakpointsViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  e: TEditor;
begin
  if Selected then begin
    e := EditorList.GetEditorFromFileName(Item.Caption);
    if Assigned(e) then begin
      e.SetCaretPosAndActivate(StrToIntDef(Item.SubItems[0], 1), 1);
      e.Activate;
    end;
  end;
end;

procedure TMainForm.actMsgCompilerCopyExecute(Sender: TObject);
var
  item:TListItem;
begin
  item:=CompilerOutput.Selected;
  if assigned(item) then begin
    Clipboard.AsText:=item.SubItems[2];
  end;
end;

procedure TMainForm.actBreakPointPropInPaneExecute(Sender: TObject);
var
  breakId: integer;
  oldCond: String;
  newCond: String;
begin
  breakId:=BreakpointsView.ItemIndex;
  if breakId>=0 then begin
    oldCond := PBreakPoint(fDebugger.BreakPointList[breakId])^.condition;
    newCond := Trim(ShowInputBox(LANG[ID_ITEM_BREAKPOINT_PROP], LANG[ID_MSG_BREAKPOINT_CONDITION]
      , oldCond));
    fDebugger.SetBreakPointCondition(breakId,newCond);
  end;
end;

procedure TMainForm.actRemoveBreakpointInPaneExecute(Sender: TObject);
var
  e: TEditor;
  Item: TListItem;
  LineNo : integer;
begin
  Item :=  BreakpointsView.Selected;
  if assigned(Item) then begin
    e := EditorList.GetEditorFromFileName(Item.Caption);
    if Assigned(e) then begin
      LineNo := StrToIntDef(Item.SubItems[0], -1);
      if LineNo<1 then
        Exit;
      e.SetCaretPosAndActivate(LineNo, 1);
      e.Activate;
      e.ToggleBreakPoint(LineNo);
    end;
  end;
end;


procedure TMainForm.actBrowserSortByTypeExecute(Sender: TObject);
begin
  ClassBrowser.SortByType := not ClassBrowser.SortByType;
  devClassBrowsing.SortByType := ClassBrowser.SortByType;
  actBrowserSortByType.Checked := ClassBrowser.SortByType;
end;

procedure TMainForm.actBrowserSortAlphabeticallyExecute(Sender: TObject);
begin
  ClassBrowser.SortAlphabetically := not ClassBrowser.SortAlphabetically;
  devClassBrowsing.SortAlphabetically := ClassBrowser.SortAlphabetically;
  actBrowserSortAlphabetically.Checked := ClassBrowser.SortAlphabetically;
end;

procedure TMainForm.DebugOutputEnter(Sender: TObject);
var
  s:string;
begin
  if not fDebugger.Executing then
    Exit;
  s:=Trim(DebugOutput.CurrentCommand);
  if Length(s) > 0 then begin

    //User shouldn't quit the gdb in command line
    if SameText(s,'quit') then
       Exit;

       {
    // Disable key, but make sure not to remove selected text
    EvaluateInput.SelStart := 0;
        EvaluateInput.SelLength := 0;
        }

    fDebugger.SendCommand(s, '',true, true, dcsConsole);

  end;

  // View command examples only when edit is empty or when the UI itself added the current command
  fDebugger.CommandChanged := true;
end;

procedure TMainForm.OpenShell(Sender: TObject;const folder; const shellName:string);
var
  buffer: PChar;
  size,ret,i: integer;
  path:AnsiString;
begin
  ret:=GetEnvironmentVariable(PChar('PATH'),nil,0);
  if ret = 0 then begin
    LogError('main.pas TMainForm.actOpenConsoleExecute',
      Format('Get size of environment variable ''PATH'' failed: %s',[SysErrorMessage(GetLastError)]));
    Exit;
  end;
  size:=ret;
  buffer:=AllocMem(size*sizeof(Char));
  try
    ret:=GetEnvironmentVariable('PATH',buffer,size);
    if ret = 0 then begin
      LogError('main.pas TMainForm.actOpenConsoleExecute',
        Format('Get content of environment variable ''PATH'' failed: %s',[SysErrorMessage(GetLastError)]));
      Exit;
    end;
    path:=String(buffer);
  finally
    FreeMem(buffer);
  end;
  if Assigned(devCompilerSets.CompilationSet) then begin
    for i:=0 to devCompilerSets.CompilationSet.BinDir.Count-1 do begin
      path:=path+';'+devCompilerSets.CompilationSet.BinDir[i];
    end;
  end;
  path:=path+';'+devDirs.Exec;
  ret := integer(SetEnvironmentVariable(PChar('PATH'),pChar(path)));
  if ret = 0 then begin
        LogError('main.pas TMainForm.actOpenConsoleExecute',
          Format('Set content of environment variable ''PATH'' failed: %s',[SysErrorMessage(GetLastError)]));
        Exit;
      end;
      ShellExecute(Application.Handle, 'open', pAnsiChar(shellName),  nil,PAnsiChar(Folder), SW_SHOWNORMAL);
end;


procedure TMainForm.actOpenConsoleExecute(Sender: TObject);
var
  e:TEditor;
  Folder: String;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    Folder := ExtractFilePath(e.FileName);
    if Folder <> '' then begin
      OpenShell(Sender,Folder,'cmd.exe');
    end;
  end;
end;

procedure TMainForm.WatchViewDblClick(Sender: TObject);
var
  curnode: TTreeNode;
  name: AnsiString;
  newName: AnsiString;
begin
  curnode := WatchView.Selected;
  if Assigned(curnode) then begin // only edit members
    if curnode.Level <> 0 then
      Exit;
    if not Assigned(curnode.Data) then
      Exit;
    name := PWatchVar(curnode.Data)^.Name;
    newName := name;
    if ShowInputQuery(Lang[ID_NV_RENAMEWATCH], Lang[ID_NV_NEWVAR], newName) then begin
      fDebugger.RenameWatchVar(name, newName);
      WatchView.Update;
    end;
  end;
end;

procedure TMainForm.actSaveWatchListUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fDebugger.WatchVarList.Count > 0;
end;

procedure TMainForm.actSaveWatchListExecute(Sender: TObject);
var
  i:integer;
  WatchVar: PWatchVar;
begin
  with TSaveDialog.Create(Self) do try
    Filter := FLT_WATCHLIST;
    Title := Lang[ID_NV_SAVEWATCH];
    DefaultExt := WATCH_EXT;
    Options := Options + [ofOverwritePrompt];

    // Open all provided files
    if Execute and (FileName <> '') then begin
      with TStringList.Create do try
        for i:=0 to fDebugger.WatchVarList.Count-1 do begin
          WatchVar := PWatchVar(fDebugger.WatchVarList[i]);
          Add(WatchVar^.Name);
        end;
        SaveToFile(FileName);
      finally
        Free;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actLoadWatchListExecute(Sender: TObject);
var
  i:integer;
begin
  with TOpenDialog.Create(Self) do try
    Filter := FLT_WATCHLIST;
    Title := Lang[ID_NV_LOADWATCH];
    DefaultExt := WATCH_EXT;

    // Open all provided files
    if Execute and (FileName <> '') then begin
      with TStringList.Create do try
        LoadFromFile(FileName);
        WatchView.Items.BeginUpdate;
        try
          fDebugger.DeleteWatchVars(true);
          for i:=0 to Count-1 do begin
            fDebugger.AddWatchVar(Strings[i]);
          end;
        finally
          WatchView.Items.EndUpdate;
        end;
      finally
        Free;
      end;
    end;
  finally
    Free;
  end;
end;


procedure TMainForm.actAddWatchUpdate(Sender: TObject);
begin
  if fDebugger.Executing then
    TCustomAction(Sender).Enabled := (not fDebugger.Reader.CommandRunning)
  else
    TCustomAction(Sender).Enabled := (fEditorList.PageCount > 0);
end;

procedure TMainForm.actOpenProjectFoloderExecute(Sender: TObject);
var
  Folder: AnsiString;
begin
  if not Assigned(fProject) then
    Exit;
  Folder := fProject.Directory;
  if Folder <> '' then
    ShellExecute(Application.Handle, 'open', 'explorer.exe', PAnsiChar(Folder), nil, SW_SHOWNORMAL);
end;

procedure TMainForm.actOpenProjectConsoleExecute(Sender: TObject);
var
  Folder: AnsiString;
  buffer: PChar;
  size,ret,i: integer;
  path:AnsiString;
begin
  if Assigned(fProject) then begin
    Folder := fProject.Directory;
    if Folder <> '' then begin
      ret:=GetEnvironmentVariable(PChar('PATH'),nil,0);
      if ret = 0 then begin
        LogError('main.pas TMainForm.actOpenConsoleExecute',
          Format('Get size of environment variable ''PATH'' failed: %s',[SysErrorMessage(GetLastError)]));
        Exit;
      end;
      size:=ret;
      buffer:=AllocMem(size*sizeof(Char));
      try
        ret:=GetEnvironmentVariable('PATH',buffer,size);
        if ret = 0 then begin
          LogError('main.pas TMainForm.actOpenConsoleExecute',
            Format('Get content of environment variable ''PATH'' failed: %s',[SysErrorMessage(GetLastError)]));
          Exit;
        end;
        path:=String(buffer);
      finally
        FreeMem(buffer);
      end;
      if Assigned(devCompilerSets.CompilationSet) then begin
        for i:=0 to devCompilerSets.CompilationSet.BinDir.Count-1 do begin
          path:=path+';'+devCompilerSets.CompilationSet.BinDir[i];
        end;
      end;
      path:=path+';'+devDirs.Exec;
      ret := integer(SetEnvironmentVariable(PChar('PATH'),pChar(path)));
      if ret = 0 then begin
        LogError('main.pas TMainForm.actOpenConsoleExecute',
          Format('Set content of environment variable ''PATH'' failed: %s',[SysErrorMessage(GetLastError)]));
        Exit;
      end;
      ShellExecute(Application.Handle, 'open', 'cmd',  nil,PAnsiChar(Folder), SW_SHOWNORMAL);
    end;
  end;
end;

procedure TMainForm.actExtractMacroExecute(Sender: TObject);
var
  e:TEditor;
  newName:AnsiString;
begin
  e:=EditorList.GetEditor();
  if Assigned(e) then begin
    e.Save;
    with TRefactorer.Create(devRefactorer,GetCppParser) do try
      if not TestExtractMacro(e) then
        Exit;
      newName := 'NEW_MACRO';
      if ShowInputQuery(LANG[ID_ITEM_EXTRACTMACRO], LANG[ID_NV_EXTRACT_MACRO_NAME],newName) then
        ExtractMacro(e,newName);
    finally
      Free;
    end;
  end;
end;

procedure TMainForm.StartTabnine;
begin
  if not fTabnine.Executing then begin
    fTabnine.Path := devDirs.Exec + 'tabnine.exe';
    fTabnine.Start;
  end;
end;
procedure TMainForm.StopTabnine;
begin
  if fTabnine.Executing then
      fTabnine.Stop;
end;


procedure TMainForm.OnDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  y    : Integer;
  x    : Integer;
  aRect, closeRect: TRect;
  size: integer;
  bgColor,fgColor,abgColor,afgColor: TColor;
  ptc: TThemeColor;
  tabs:integer;
  tabRect: TRect;
  oldColor : TColor;
  oldSize : integer;
begin
  strToThemeColor(ptc, devEditor.Syntax.Values[cPNL]);
  bgColor := ptc.Background;
  fgColor := ptc.Foreground;
  abgColor := dmMain.Cpp.WhitespaceAttribute.Background;
  afgColor := dmMain.Cpp.IdentifierAttri.Foreground;
  if Active then begin
    //Fill the tab rect
    Control.Canvas.Brush.Color := abgColor;
    Control.Canvas.Font.Color := afgColor;
    Control.Canvas.FillRect(Rect);
  end else begin
    //Fill the tab rect
    Control.Canvas.Brush.Color := bgColor;
    Control.Canvas.Font.Color := fgColor;
    Control.Canvas.FillRect(Rect);
  end;
  tabs:=TTabControl(Control).Tabs.Count;

  //Fill the background
  if tabs>0 then begin
    tabRect:=Control.TabRect(tabs-1);
    case TTabControl(Control).TabPosition of
      tpLeft,tpRight: begin
        aRect.Left:=0;
        aRect.Right:=Control.Width;
        aRect.Bottom:=Control.Height;
        aRect.Top:=tabRect.Bottom+1;
      end;
      tpTop,tpBottom: begin
        aRect.Left:=tabRect.Right+1;
        aRect.Right:=Control.Width;
        aRect.Bottom:=Control.Height;
        aRect.Top:=Rect.Top;
      end;
    end;
  end else begin
    aRect.Left:=0;
    aRect.Right:=Control.Width;
    aRect.Bottom:=Control.Height;
    aRect.Top:=Rect.Top;
  end;
  Control.Canvas.Brush.Color := bgColor;
  Control.Canvas.FillRect(aRect);
  
  if Active then begin
    //Fill the tab rect
    Control.Canvas.Brush.Color := abgColor;
    Control.Canvas.Font.Color := afgColor;
  end;

  //draw the tab title
  case TTabControl(Control).TabPosition of
    tpLeft,tpRight: begin
      if Assigned(TPageControl(Control).Images)
        and (TPageControl(Control).Pages[TabIndex].ImageIndex<TPageControl(Control).Images.Count)
        and (TPageControl(Control).Pages[TabIndex].ImageIndex>=0) then begin
        x  := Rect.Left + ((Rect.Right - Rect.Left
          - TTabControl(Control).Images.Width
        ) div 2) + 1;
        y  := Rect.bottom - TTabControl(Control).Images.Height-5;
        TTabControl(Control).Images.Draw(Control.Canvas,x,y,TPageControl(Control).Pages[TabIndex].ImageIndex);
        y  := y - ((Rect.Bottom - Rect.Top
           - Control.Canvas.TextWidth(TPageControl(Control).Pages[TabIndex].Caption)
           - TTabControl(Control).Images.Height
           -5) div 2) ;
      end else begin
        y  := Rect.Bottom - ((Rect.Bottom - Rect.Top
           - Control.Canvas.TextWidth(TPageControl(Control).Pages[TabIndex].Caption)
           ) div 2) - 1;
      end;
      x  := Rect.Left + ((Rect.Right - Rect.Left
        - Control.Canvas.TextHeight (TPageControl(Control).Pages[TabIndex].Caption)
        ) div 2) + 1;

      AngleTextOut(Control.Canvas,TPageControl(Control).Pages[TabIndex].Caption,x,y,90);
    end;
    tpTop,tpBottom: begin
      if Assigned(TPageControl(Control).Images)
        and (TPageControl(Control).Pages[TabIndex].ImageIndex<TPageControl(Control).Images.Count)
        and (TPageControl(Control).Pages[TabIndex].ImageIndex>=0) then begin
        x  := Rect.Left + 5;
        y  := Rect.Top + ((Rect.Bottom - Rect.Top
          - TTabControl(Control).Images.Height) div 2) + 1;
        TTabControl(Control).Images.Draw(Control.Canvas,x,y,TPageControl(Control).Pages[TabIndex].ImageIndex);
        x  := x + TTabControl(Control).Images.Width + ((Rect.Right - Rect.Left
          - Control.Canvas.TextWidth (TPageControl(Control).Pages[TabIndex].Caption)
          - TTabControl(Control).Images.Width
          - 5) div 2);
      end else begin
        x  := Rect.Left + ((Rect.Right - Rect.Left - Control.Canvas.TextWidth(TPageControl(Control).Pages[TabIndex].Caption)) div 2) + 1;
      end;
      y  := Rect.Top + ((Rect.Bottom - Rect.Top - Control.Canvas.TextHeight(TPageControl(Control).Pages[TabIndex].Caption)) div 2) + 1;
      Control.Canvas.TextOut(x,y,TPageControl(Control).Pages[TabIndex].Caption);
    end;
  end;

  //hack for editor close button
  if (Control = EditorPageControlLeft) or (Control = EditorPageControlRight) then begin
    closeRect.Bottom := Rect.Bottom;
    closeRect.Top := Rect.Top;
    size := Rect.Bottom - Rect.Top - 4;
    if (size>20) then
      size := 20;
    closeRect.Top := Rect.Top + (Rect.Bottom - Rect.Top - size) div 2;
    closeRect.Bottom := closeRect.Top + size;
    closeRect.Right := Rect.Right - 5;
    closeRect.Left := closeRect.Right - size;
    {
    oldColor := Control.Canvas.Brush.Color;
    Control.Canvas.Brush.Color := clRed;
    Control.Canvas.RoundRect(closeRect.Left,CloseRect.Top,closeRect.Right,CloseRect.Bottom,6,6);
    Control.Canvas.Brush.Color := oldColor;
    }
    oldColor := Control.Canvas.Pen.Color;
    oldSize := Control.Canvas.Pen.Width;
    Control.Canvas.Pen.Color := Control.Canvas.Font.Color;
    Control.Canvas.Pen.Width := 2;
    inflateRect(closeRect,-5,-5);
    Control.Canvas.MoveTo(closeRect.Left-1,closeRect.Top);
    Control.Canvas.LineTo(closeRect.Right,closeRect.Bottom);
    Control.Canvas.MoveTo(closeRect.Left-1,closeRect.Bottom);
    Control.Canvas.LineTo(closeRect.Right,closeRect.Top);
    Control.Canvas.Pen.Color := oldColor;
    Control.Canvas.Pen.Width := oldSize;
  end;
end;


procedure TMainForm.actCopyAsRTFExecute(Sender: TObject);
var
  e:TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) and e.Text.Focused then
    e.RTFToClipboard;
end;

procedure TMainForm.actForwardUpdate(Sender: TObject);
begin
  actForward.Enabled := fCaretList.hasNext;
end;

procedure TMainForm.actForwardExecute(Sender: TObject);
var
  pCaret:PEditorCaret;
  p:TBufferCoord;
  e:TEditor;
begin
  pCaret := fCaretList.GotoAndGetNext;
  if assigned(pCaret) then begin
    e:=TEditor(pCaret.editor);
    p.Line := pCaret.line;
    p.Char := pCaret.char;
    e.Text.CaretXY := p;
    e.Activate;
  end;
end;

procedure TMainForm.actBackExecute(Sender: TObject);
var
  pCaret:PEditorCaret;
  p:TBufferCoord;
  e:TEditor;
begin
  pCaret := fCaretList.GotoAndGetPrevious;
  if assigned(pCaret) then begin
    e:= TEditor(pCaret.editor);
    p.Line := pCaret.line;
    p.Char := pCaret.char;
    e.Text.CaretXY := p;
    e.Activate;
  end;
end;

procedure TMainForm.actBackUpdate(Sender: TObject);
begin
  actBack.Enabled := fCaretList.hasPrevious;
end;

procedure TMainForm.actCloseMessageSheetExecute(Sender: TObject);
begin
  OpenCloseMessageSheet(false);
end;

procedure TMainForm.actNextErrorUpdate(Sender: TObject);
var
  e:TEditor;
begin
  e:=EditorList.GetEditor();
  if assigned(e) then begin
    actNextError.Enabled := e.HasNextError;
  end;
end;

procedure TMainForm.actPrevErrorUpdate(Sender: TObject);
var
  e:TEditor;
begin
  e:=EditorList.GetEditor();
  if assigned(e) then begin
    actPrevError.Enabled := e.HasPrevError;
  end;
end;


procedure TMainForm.actPrevErrorExecute(Sender: TObject);
var
  e:TEditor;
begin
  e:=EditorList.GetEditor();
  if assigned(e) then begin
    e.GotoPrevError;
  end;
end;

procedure TMainForm.actNextErrorExecute(Sender: TObject);
var
  e:TEditor;
begin
  e:=EditorList.GetEditor();
  if assigned(e) then begin
    e.GotoNextError;
  end;
end;


procedure TMainForm.FindOutputAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);

var
  LineToDraw: AnsiString;
  i: integer;
  Rect,iRect: TRect;
  OldBrushColor, OldPenColor, OldFontColor, oBgColor,oFgColor: TColor;
  tc:TThemeColor;
  bg,fg, tbg, tfg, abg,afg: TColor;
  pFind : PFindInfo;
  pFile : PFileInfo;
  pItem : PFindItem;



  procedure Draw(const s: AnsiString);
  var
    txt: string;
    fillRect : TRect;
  begin
    txt:=StringReplace(s,#9, StringOfChar(' ',devEditor.TabSize),[rfReplaceAll]);

    fillRect := rect;
    fillRect.Right := fillRect.Left + Sender.Canvas.TextWidth(txt);
    Sender.Canvas.FillRect(fillRect);
    DrawText(Sender.Canvas.Handle, PAnsiChar(txt), Length(txt), rect, DT_NOPREFIX or DT_NOCLIP);

    // Get text extent
    Inc(rect.Left, Sender.Canvas.TextWidth(txt) + 1); // 1 extra pixel for extra width caused by bold
  end;

begin
  if Stage <> cdPrePaint then
    Exit;

  // Get rect of subitem to draw
  Rect := Node.DisplayRect(False);

  OldBrushColor := Sender.Canvas.Brush.Color;
  OldPenColor := Sender.Canvas.Pen.Color;

  OldFontColor := Sender.Canvas.Font.Color;
  StrToThemeColor(tc,devEditor.Syntax.Values[cAL]);
  tbg := tc.Background;
  tfg := dmMain.Cpp.VariableAttri.Foreground;
  abg := tc.Background;
  afg := dmMain.Cpp.IdentifierAttri.Foreground;
  fg := dmMain.Cpp.IdentifierAttri.Foreground;
  bg := dmMain.Cpp.WhitespaceAttribute.Background;

  // Draw background
  if (cdsSelected in State) then begin
    Sender.Canvas.Brush.Color := abg;
    Sender.Canvas.Font.Color := afg;
  end else begin
    Sender.Canvas.Brush.Color := bg;
    Sender.Canvas.Font.Color := fg;
  end;
  Sender.Canvas.FillRect(rect);

  Rect := Node.DisplayRect(True);
  if Node.HasChildren then begin
    Sender.Canvas.Pen.Color := fg;
    iRect.Right := rect.Left;
    iRect.Bottom := rect.Bottom-1;
    if (rect.bottom-rect.top) < 16 then begin
      iRect.Left := iRect.Right - (rect.bottom-rect.top);
      iRect.Top := iRect.Bottom - (rect.bottom-rect.top);
    end else begin
      iRect.Left := iRect.Right - 16;
      iRect.Bottom := rect.Bottom-(rect.bottom-rect.top-16) div 2;
      iRect.Top := iRect.Bottom - 16;
    end;
    oBGColor := Sender.Canvas.Brush.Color;
    Sender.Canvas.Brush.Color  := tbg;
    Sender.Canvas.FillRect(iRect);
    Sender.Canvas.Brush.Color  := oBGColor;
    sender.Canvas.MoveTo(iRect.Left,iRect.Top);
    sender.Canvas.LineTo(iRect.Right,iRect.Top);
    sender.Canvas.LineTo(iRect.Right,iRect.Bottom);
    sender.Canvas.LineTo(iRect.Left,iRect.Bottom);
    sender.Canvas.LineTo(iRect.Left,iRect.Top);
    sender.Canvas.MoveTo(iRect.Left+3,iRect.Top + (iRect.Bottom - iRect.Top) div 2);
    sender.Canvas.LineTo(iRect.Right-2,iRect.Top + (iRect.Bottom - iRect.Top) div 2);
    if not Node.Expanded then begin
      sender.Canvas.MoveTo(iRect.Left + (iRect.Right - iRect.Left) div 2,iRect.Top+3);
      sender.Canvas.LineTo(iRect.Left + (iRect.Right - iRect.Left) div 2,iRect.Bottom-2);
    end;
  end;
  OffsetRect(Rect, 4, 2);
  case node.Level of
    0: begin // find info node
      if assigned(node.Data) then begin
        pFind := PFindInfo(node.Data);
        Draw(
          Format('Search "%s" (%d hits in %d files of %d searched)',[
            pFind.token,
            pFind.hits,
            pFind.filehitted,
            pFind.filesearched])
        );
      end;
    end;
    1: begin // file info node
      if assigned(node.Data) then begin
        pFile := PFileInfo(node.Data);
        Draw(
          Format('%s (%d hits)', [pFile.filename,pFile.hits])
        );
      end;
    end;
    2: begin // file info node
      if assigned(node.Data) then begin
        pItem := PFindItem(node.Data);
        LineToDraw := pItem.lineText;
        Draw(Format('Line %d: ',[pItem.line]));
        Draw(Copy(LineToDraw, 1, pItem.char - 1));
        // Enable bold
        Sender.Canvas.Font.Style := [fsBold];
        oBgColor := Sender.Canvas.Brush.Color;
        Sender.Canvas.Brush.Color := tbg;
        oFgColor := Sender.Canvas.Font.Color;
        Sender.Canvas.Font.Color := tfg;
        Sender.Canvas.Refresh;

        // Draw bold highlight
        Draw(Copy(LineToDraw, pItem.char, pItem.tokenlen));

        // Disable bold
        Sender.Canvas.Font.Style := [];
        Sender.Canvas.Brush.Color := oBgColor;
        Sender.Canvas.Font.Color := oFgColor;
        Sender.Canvas.Refresh;
        Draw(Copy(LineToDraw, pItem.char + pItem.tokenlen, MaxInt));

      end;
    end;
  end;
  // Restore colors
  Sender.Canvas.Brush.Color := OldBrushColor;
  Sender.Canvas.Font.Color := OldFontColor;
  Sender.Canvas.Pen.Color := OldPenColor;
  
  DefaultDraw := false;
end;

procedure TMainForm.mnuClearAllFindItemsClick(Sender: TObject);
begin
  FindOutput.Clear;
end;

procedure TMainForm.actCloseProjectUpdate(Sender: TObject);
begin
  actCloseProject.Enabled := Assigned(fProject) and not fClosing;
end;

procedure TMainForm.actCloseAllUpdate(Sender: TObject);
begin
  actCloseAll.Enabled:= ( fEditorList.PageCount > 0)
    and not fClosing;
end;

procedure TMainForm.actCloseUpdate(Sender: TObject);
begin
  actClose.Enabled := (fEditorList.PageCount > 0)
    and not fClosing and not GetCppParser.Parsing;
end;

procedure TMainForm.EditorPageControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SenderPageControl: TPageControl;
begin
  SenderPageControl := TPageControl(Sender);
  SenderPageControl.Pages[SenderPageControl.ActivePageIndex].EndDrag(False);
end;

function TMainForm.GetCppParser:TCppParser;
var
  e:TEditor;
begin
  Result := fDummyCppParser;
  case GetCompileTarget of
    cttProject: begin
      if not Assigned(fProject) then
        Exit;
      Result := fProject.CppParser;
    end;
    cttFile: begin
      e:=EditorList.GetEditor();
      if not Assigned(e) then
        Exit;
      Result := e.CppParser;
    end;
  end;
end;

procedure TMainForm.actCompileUpdate(Sender: TObject);
var
  e:TEditor;
begin
  case GetCompileTarget of
    cttFile: begin
        e:=EditorList.GetEditor();
        if not Assigned(e) then
          Exit;
        TCustomAction(Sender).Enabled :=
          ( (e.New and e.Text.Modified) or IsCfile(e.FileName)) and (not fCompiler.Compiling)
          and Assigned(devCompilerSets.CompilationSet) and (not fDebugger.Executing);
//          and (not devExecutor.Running);
      end;
    cttProject: begin
        TCustomAction(Sender).Enabled := (not fCompiler.Compiling)
          and Assigned(devCompilerSets.CompilationSet) and (not fDebugger.Executing);
//          and (not devExecutor.Running);
      end;
    else begin
      TCustomAction(Sender).Enabled:=False;        
    end;
  end;
end;

procedure TMainForm.actSyntaxCheckFileUpdate(Sender: TObject);
var
  e:TEditor;
begin
  e:=EditorList.GetEditor();
  actSyntaxCheck.Enabled := Assigned(e)
    and (
      IsCFile(e.FileName)
      or IsHFile(e.FileName)
      or (e.New and e.Text.Modified)
    );
end;

procedure TMainForm.actRenameSymbolUpdate(Sender: TObject);
var
  e:TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and not e.Text.IsEmpty
    and not e.CppParser.Parsing;
end;

procedure TMainForm.actExtractMacroUpdate(Sender: TObject);
var
  e:TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and not e.Text.IsEmpty
    and not e.CppParser.Parsing;
end;

procedure TMainForm.actFindAllUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := Assigned(fProject)
    or (fEditorList.PageCount>0)
end;

procedure TMainForm.ChangeEncoding(encoding:TFileEncodingType);
var
  e:TEditor;
  choice,i: integer;
begin
  e:=fEditorList.GetEditor;
  if assigned(e) then begin
    if e.Text.Modified then begin
      choice := MessageDlg(Lang[ID_ERR_FILEMODIFIED], mtConfirmation, [mbYes, mbNo, mbAbort], 0) ;
      if choice = mrYes then
        e.SaveFile(e.FileName)
      else if choice = mrAbort then
          Exit;
    end;
    e.EncodingOption := encoding;
    e.LoadFile(e.FileName);
    if e.InProject and Assigned(fProject) then begin
      for i:=0 to fProject.Units.Count-1 do begin
        if e = fProject.Units[i].Editor then begin
          fProject.Units[i].Encoding := e.EncodingOption;
          fProject.Modified:=True;
          break;
        end;
      end;
    end;
    UpdateFileEncodingStatusPanel;
  end;
end;

procedure TMainForm.actEncodingTypeExecute(Sender: TObject);
begin
  actAutoDetectEncoding.Checked := False;
  actUTF8.Checked := False;
  actANSI.Checked := False;
  TCustomAction(Sender).Checked := True;
  if Sender = actAutoDetectEncoding then begin
    ChangeEncoding(etAuto);
  end else if Sender = actUTF8 then begin
    ChangeEncoding(etUTF8);
  end else if Sender = actUTF8Bom then begin
    ChangeEncoding(etUTF8Bom);
  end else begin
    ChangeEncoding(etANSI);
  end;
end;

procedure TMainForm.actEncodingTypeUpdate(Sender: TObject);
var
  e:TEditor;
begin
  e:=EditorList.GetEditor();
  TCustomAction(Sender).Enabled := Assigned(e);
  if Assigned(e) then begin
    if Sender = actAutoDetectEncoding then begin
      TCustomAction(Sender).Checked := (e.EncodingOption = etAuto);
    end else if Sender = actUTF8 then begin
      TCustomAction(Sender).Checked := (e.EncodingOption = etUTF8);
    end else if Sender = actUTF8Bom then begin
      TCustomAction(Sender).Checked := (e.EncodingOption = etUTF8Bom);
    end else if Sender = actANSI then begin
      TCustomAction(Sender).Checked := (e.EncodingOption = etANSI);
    end;
  end;
end;

procedure TMainForm.actConvertToAnsiExecute(Sender: TObject);
var
  e:TEditor;
  i:integer;
begin
  e:=fEditorList.GetEditor;
  if MessageDlg(
      Format(Lang[ID_MSG_CONVERTTOANSI],[UpperCase(GetSystemCharsetName)]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    e.EncodingOption := etANSI;
    e.Text.Modified := True; // set modified flag to make sure save.
    e.Save;
    UpdateFileEncodingStatusPanel;
    // set project unit's utf-8 flag
    if e.InProject and Assigned(fProject) then begin
      for i:=0 to fProject.Units.Count-1 do begin
        if e = fProject.Units[i].Editor then begin
          fProject.Units[i].Encoding := e.EncodingOption;
          fProject.Modified:=True;
          break;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.actConvertToAnsiUpdate(Sender: TObject);
var
  e:TEditor;
begin
  e:=fEditorList.GetEditor;
  if not assigned(e) then begin
    actConvertToAnsi.Enabled := False;
  end else begin
    actConvertToAnsi.Enabled := (e.FileEncoding <> etAnsi);
  end;
end;

procedure TMainForm.MessageControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  hi: TTCHitTestInfo;
  tabindex: Integer;
  oldTabIndex: integer;
begin
  If Button = mbLeft Then Begin
    hi.pt.x := X;
    hi.pt.y := Y;
    hi.flags := 0;
    if not fMessageControlChanged then begin
      oldTabIndex := MessageControl.ActivePageIndex;
      tabindex := MessageControl.Perform( TCM_HITTEST, 0, longint(@hi));
      if (tabindex>=0) and (tabIndex = oldTabIndex) then begin
        OpenCloseMessageSheet(not SplitterBottom.Visible);
      end;
    end;
    fMessageControlChanged := False;
  End;
end;


procedure TMainForm.SplitterLeftMoved(Sender: TObject);
begin
  fPreviousWidth:= LeftPageControl.Width;
end;

procedure TMainForm.LeftPageControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  hi: TTCHitTestInfo;
  tabindex: Integer;
  oldTabIndex: integer;
begin
  If Button = mbLeft Then Begin
    hi.pt.x := X;
    hi.pt.y := Y;
    hi.flags := 0;
    if not fLeftPageControlChanged then begin
      oldTabIndex := LeftPageControl.TabIndex;
      tabindex := LeftPageControl.Perform( TCM_HITTEST, 0, longint(@hi));
      if (tabindex>=0) and (tabIndex = oldTabIndex) then begin
        OpenCloseLeftPageControl(not SplitterLeft.Visible);
      end;
    end;
    fLeftPageControlChanged := False;
  End;
end;


procedure TMainForm.actOpenWindowsTerminalExecute(Sender: TObject);
var
  e:TEditor;
  Folder: String;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    Folder := ExtractFilePath(e.FileName);
    if Folder <> '' then begin
      OpenShell(Sender,Folder,'wt.exe');
    end;
  end;
end;

procedure TMainForm.actOpenWindowsTerminalUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := devEnvironment.HasWindowsTerminal
    and (fEditorList.PageCount > 0);
end;

procedure TMainForm.actStatementsTypeFileUpdate(Sender: TObject);
var
  e:TEditor;
begin
  e:=EditorList.GetEditor;
  if assigned(e) then begin
    TCustomAction(Sender).Enabled := e.InProject;
  end else
    TCustomAction(Sender).Enabled := False;
end;

procedure TMainForm.UpdateStatementsType;
var
  e:TEditor;
begin
  e:=EditorList.GetEditor;
  if assigned(e) and e.InProject then begin
    ClassBrowser.StatementsType := devClassBrowsing.StatementsType;
  end;
  actStatementsTypeFile.Checked := (devClassBrowsing.StatementsType = cbstFile);
  actStatementsTypeProject.Checked := (devClassBrowsing.StatementsType = cbstProject);
end;

procedure TMainForm.actStatementsTypeFileExecute(Sender: TObject);
begin
  devClassBrowsing.StatementsType := cbstFile;
  UpdateStatementsType;
end;

procedure TMainForm.actStatementsTypeProjectExecute(Sender: TObject);
begin
  devClassBrowsing.StatementsType := cbstProject;
  UpdateStatementsType;
end;

procedure TMainForm.actIndentUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled:= Assigned(e) and e.Text.Focused;
end;

procedure TMainForm.actSetCurrentFolderExecute(Sender: TObject);
var
  folder : String;
begin
  folder:=fileBrowser.CurrentFolder;
  if NewSelectDirectory('','',folder) then
    fileBrowser.CurrentFolder := folder;
  setLeftPageControlPage(FilesSheet);
  {
  LeftPageControl.ActivePage := self.FilesSheet;
  fLeftPageControlChanged := False;
  ClassBrowser.TabVisible:=False;
  }
end;

procedure TMainForm.fileBrowserDblClick(Sender: TObject);
begin
  if (fileBrowser.SelectedFile <> '') and (FileExists(fileBrowser.SelectedFile)) then begin
    if GetFileTyp(fileBrowser.SelectedFile) = utPrj then
      OpenProject(fileBrowser.SelectedFile)
    else
      OpenFile(fileBrowser.SelectedFile, etAuto);
  end;
  fileBrowser.ClearSelection;
end;

procedure TMainForm.actOnlyShowDevFilesExecute(Sender: TObject);
begin
  fileBrowser.OnlyShowDevFiles := actOnlyShowDevFiles.Checked;
end;

procedure TMainForm.actLocateFileExecute(Sender: TObject);
var
  editor:TEditor;
begin
  editor := EditorList.GetEditor();
  if not assigned(editor) then
    Exit;   
  if ( fileBrowser.CurrentFolder <> '') then begin
    if not StartsText(IncludeTrailingPathDelimiter(fileBrowser.CurrentFolder), editor.FileName) then begin
      if (MessageDlg(Format(Lang[ID_MSG_CHANGE_FOLDER_TO_FILE], [ExtractFileName(editor.FileName)]),
        mtConfirmation, [mbYes,mbNo], 0) = mrYes) then begin
        fileBrowser.CurrentFolder := ExtractFileDir(editor.FileName);
      end else
        Exit;
    end else
  end else
    fileBrowser.CurrentFolder := ExtractFileDir(editor.FileName);
  fileBrowser.LocateFile(editor.FileName);
end;

procedure TMainForm.actClearAllBreakpointsUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled:=self.BreakpointsView.Items.Count>0
end;

procedure TMainForm.actClearAllBreakpointsExecute(Sender: TObject);
var
  i:integer;
  breakpoint:PBreakPoint;
  editor : TEditor;
begin
  for i:=fDebugger.BreakPointList.Count-1 downto 0 do begin
    breakpoint := PBreakpoint(fDebugger.BreakPointList[i]);
    editor := breakpoint^.editor;
    editor.ToggleBreakPoint(breakpoint^.line);
  end;
  OnBreakPointsChanged;
end;

procedure TMainForm.actClearAllBreakpointsInEditorExecute(Sender: TObject);
var
  i:integer;
  breakpoint:PBreakPoint;
  editor : TEditor;
begin
  editor:=editorlist.GetEditor();
  if not assigned(editor) then
    Exit;
  for i:=fDebugger.BreakPointList.Count-1 downto 0 do begin
    breakpoint := PBreakpoint(fDebugger.BreakPointList[i]);
    if (breakpoint^.editor = editor) then
      editor.ToggleBreakPoint(breakpoint^.line);
  end;
  OnBreakPointsChanged;
end;

procedure TMainForm.actOpenCurrentFolderExecute(Sender: TObject);
var
  Folder: String;
begin
  Folder := fileBrowser.CurrentFolder;
  if Folder <> '' then
    ShellExecute(Application.Handle, 'open', 'explorer.exe', PAnsiChar(fileBrowser.CurrentFolder), nil, SW_SHOWNORMAL);
end;

procedure TMainForm.actOpenCurrentFolderUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fileBrowser.CurrentFolder <> '';
end;

procedure TMainForm.actOpenCurrentFolderInConsoleExecute(Sender: TObject);
var
  Folder: String;
begin
  Folder := fileBrowser.CurrentFolder;
  if Folder <> '' then
    OpenShell(Sender,Folder,'cmd.exe');
end;

procedure TMainForm.actOpenCurrentFolderInWindowsTerminalExecute(
  Sender: TObject);
var
  Folder: String;  
begin
  Folder := fileBrowser.CurrentFolder;
  if Folder <> '' then
    OpenShell(Sender,Folder,'wt.exe');
end;

procedure TMainForm.fileBrowserContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  BuildOpenWith;
  {
  if FileBrowser.SelectedFile <> '' then begin
  end;
  }
end;

procedure TMainForm.mnuFileBrowserOpenWithClick(Sender: TObject);
var
  idx: integer;
  item: TMenuItem;
  e: TEditor;
begin
  if (Sender = mnuFileBrowserOpenWith) and (mnuFileBrowserOpenWith.Count > 0) then
    Exit;
  if FileBrowser.SelectedFile='' then
    Exit;
  item := TMenuItem(Sender);
  if item = mnuFileBrowserOpenWith then begin
    idx := -2;
    with TOpenDialog.Create(Self) do try
      Filter := FLT_ALLFILES;
      if Execute then
        idx := devExternalPrograms.AddProgram(ExtractFileExt(FileBrowser.SelectedFile), Filename);
    finally
      Free;
    end;
  end else
    idx := item.Tag;

  e := fEditorList.FileIsOpen(FileBrowser.SelectedFile, TRUE);
  if Assigned(e) then
    fEditorList.CloseEditor(e);

  if idx > -1 then begin // devcpp-based
    ShellExecute(0, 'open',
      PAnsiChar(devExternalPrograms.ProgramName[idx]),
      PAnsiChar(FileBrowser.SelectedFile),
      PAnsiChar(ExtractFilePath(FileBrowser.SelectedFile)),
      SW_SHOW)
      // idx=-2 means we prompted the user for a program, but didn't select one
  end else if idx = -1 then begin// registry-based
    ShellExecute(0, 'open',
      PAnsiChar(FileBrowser.SelectedFile),
      nil,
      PAnsiChar(ExtractFilePath(FileBrowser.SelectedFile)),
      SW_SHOW);
  end else if idx = -3 then begin// ResEd.exe
    ShellExecute(0, 'open',
      PAnsiChar(devDirs.Exec + 'ResEd/ResEd.exe'),
      PAnsiChar(FileBrowser.SelectedFile),
      PAnsiChar(ExtractFilePath(FileBrowser.SelectedFile)),
      SW_SHOW);
  end
end;

procedure TMainForm.actOpenSelectedFileExecute(Sender: TObject);
var
  e:TEditor;
begin
  if FileBrowser.SelectedFile = '' then
    Exit;
  e := fEditorList.FileIsOpen(FileBrowser.SelectedFile, TRUE);
  if Assigned(e) then
    e.Activate
  else begin
    if GetFileTyp(FileBrowser.SelectedFile) = utPrj then
      OpenProject(FileBrowser.SelectedFile)
    else
      OpenFile(FileBrowser.SelectedFile,etAuto);
  end;
end;

procedure TMainForm.actOpenSelectedFileUpdate(Sender: TObject);
var
  item : TAction;
begin
  item := TAction(Sender);
  item.Enabled := (FileBrowser.SelectedFile <> '');
end;

end.

