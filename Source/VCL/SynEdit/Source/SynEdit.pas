{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEdit.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEdit.pas,v 1.457.2.12 2007/07/11 22:41:34 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDIT}
unit SynEdit;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  Controls,
  Graphics,
  Forms,
  StdCtrls,
  ExtCtrls,
  Windows,
  Messages,
  Themes,
{$IFDEF SYN_MBCSSUPPORT}
  Imm,
{$ENDIF}
  SynTextDrawer,
  SynEditTypes,
  SynEditKeyConst,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynEditTextBuffer,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditKbdHandler,
  SynEditCodeFolding,
  Math,
  SysUtils,
  Classes;

const

  // maximum scroll range
  MAX_SCROLL = 32767;

  // Max number of book/gutter marks returned from GetEditMarksForLine - that
  // really should be enough.
  MAX_MARKS = 16;

  SYNEDIT_CLIPBOARD_FORMAT = 'SynEdit Control Block Type';

var
  SynEditClipboardFormat: UINT;

type
  TSynBorderStyle = TBorderStyle;
  TBufferCoord = SynEditTypes.TBufferCoord;
  TDisplayCoord = SynEditTypes.TDisplayCoord;
  TSynEditMark = SynEditMiscClasses.TSynEditMark;

  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  ESynEditError = class(Exception);

  TDropFilesEvent = procedure(Sender: TObject; X, Y: integer; AFiles: TStrings)
    of object;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: boolean;
    var Handled: boolean; var Command: TSynEditorCommand; var AChar: char;
    Data: pointer; HandlerData: pointer) of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TSynEditorCommand; var AChar: char; Data: pointer) of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    string; Line, Column, wordlen: integer;  var Action: TSynReplaceAction) of object;

  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: integer;
    var Special: boolean; var FG, BG: TColor) of object;

  TPaintHighlightTokenEvent = procedure(Sender: TObject; Row: integer;
    column: integer; token: String; attr: TSynHighlighterAttributes;
     var style:TFontStyles; var FG,BG:TColor) of object;

  PEditingArea = ^TEditingArea;
  TEditingArea = Record
    beginX: integer;
    endX: integer;
    color: TColor;
  end;

  TEditingAreaType = (
    eatEditing,
    eatError,
    eatWarning);

  TEditingAreasEvent = procedure(Sender: TObject; Line:integer;
    areaList:TList; var borderColor:TColor; var areaType:TEditingAreaType) of object;  // areaList : TList<TEditingArea>

  TTransientType = (ttBefore, ttAfter);
  TPaintTransient = procedure(Sender: TObject; Canvas: TCanvas;
    TransientType: TTransientType) of object;

  TScrollEvent = procedure(Sender: TObject; ScrollBar: TScrollBarKind) of object;

  TGutterGetTextEvent = procedure(Sender: TObject; aLine: integer;
    var aText: string) of object;

  TGutterPaintEvent = procedure(Sender: TObject; aLine: integer;
    X, Y: integer) of object;

  TImeInputEvent = procedure(Sender: TObject; s:String) of object;

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfCaretVisible, sfDblClicked, sfWaitForDragging);

  TSynStateFlags = set of TSynStateFlag;

  TScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TSynEditorOption = (
    eoAltSetsColumnMode, //Holding down the Alt Key will put the selection mode into columnar format
    eoAutoIndent, //Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoAddIndent, //Will add one tab width of indent when typing { and :, and remove the same amount when typing }
    eoAutoSizeMaxScrollWidth, //Automatically resizes the MaxScrollWidth property when inserting text
    eoDisableScrollArrows, //Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoDragDropEditing, //Allows you to select a block of text and drag it within the document to another location
    eoDropFiles, //Allows the editor accept OLE file drops
    eoEnhanceHomeKey, //enhances home key positioning, similar to visual studio
    eoEnhanceEndKey, //enhances End key positioning, similar to JDeveloper
    eoGroupUndo, //When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll, //When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars, //if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoKeepCaretX, //When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret, //Makes it so the caret is never visible
    eoNoSelection, //Disables selecting text
    eoRightMouseMovesCursor, //When clicking with the right mouse for a popup menu, move the cursor to that location
    eoScrollByOneLess, //Forces scrolling to be one less
    eoScrollHintFollows, //The scroll hint follows the mouse when scrolling vertically
    eoScrollPastEof, //Allows the cursor to go past the end of file marker
    eoScrollPastEol, //Allows the cursor to go past the last character into the white space at the end of a line
    eoShowScrollHint, //Shows a hint of the visible line numbers when scrolling vertically
    eoShowSpecialChars, //Shows the special Characters
    eoSmartTabDelete, //similar to Smart Tabs, but when you delete characters
    eoSmartTabs, //When tabbing, the cursor will go to the next non-white space character of the previous line
    eoSpecialLineDefaultFg, //disables the foreground text color override when using the OnSpecialLineColor event
    eoTabIndent, //When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces, //Converts a tab character to a specified number of space characters
    eoShowRainbowColor,
    eoTrimTrailingSpaces //Spaces at the end of lines will be trimmed and not saved
    );

  TSynEditorOptions = set of TSynEditorOption;

  TSynFontSmoothMethod = (fsmNone, fsmAntiAlias, fsmClearType);

const
  SYNEDIT_DEFAULT_OPTIONS = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces,
    eoSmartTabDelete, eoGroupUndo];

type
  // use scAll to update a statusbar when another TCustomSynEdit got the focus
  TSynStatusChange = (scAll, scCaretX, scCaretY, scLeftChar, scTopLine,
    scInsertMode, scModified, scSelection, scReadOnly,scOpenFile);
  TSynStatusChanges = set of TSynStatusChange;

  TContextHelpEvent = procedure(Sender: TObject; word: string)
    of object;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TMouseCursorEvent = procedure(Sender: TObject; const aLineCharPos: TBufferCoord;
    var aCursor: TCursor) of object;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark)
    of object;

  TCustomSynEdit = class;

  TGutterClickEvent = procedure(Sender: TObject; Button: TMouseButton;
    X, Y, Line: integer; Mark: TSynEditMark) of object;

  // aIndex parameters of Line notifications are 0-based.
  // aRow parameter of GetRowLength() is 1-based.
  ISynEditBufferPlugin = interface
    // conversion methods
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RowCount: integer;
    function GetRowLength(aRow: integer): integer;
    // plugin notifications
    function LinesInserted(aIndex: integer; aCount: integer): integer;
    function LinesDeleted(aIndex: integer; aCount: integer): integer;
    function LinesPutted(aIndex: integer; aCount: integer): integer;
    // font or size change
    procedure DisplayChanged;
    // pretty clear, heh?
    procedure Reset;
  end;

  TSynEditPlugin = class(TObject)
  private
    fOwner: TCustomSynEdit;
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: integer); virtual; abstract;
    procedure LinesInserted(FirstLine, Count: integer); virtual; abstract;
    procedure LinesDeleted(FirstLine, Count: integer); virtual; abstract;
  protected
    property Editor: TCustomSynEdit read fOwner;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
  end;

  TCustomSynEdit = class(TCustomControl)
  private
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMClear(var Msg: TMessage); message WM_CLEAR;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetText(var Msg: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var Msg: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
{$IFDEF SYN_MBCSSUPPORT}
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Msg: TMessage); message WM_IME_NOTIFY;
{$ENDIF}
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
  private
    fAllFoldRanges: TSynEditFoldRanges;
    fCodeFolding: TSynCodeFolding;
    fUseCodeFolding: boolean;
    fAlwaysShowCaret: Boolean;
    fBlockBegin: TBufferCoord;
    fBlockEnd: TBufferCoord;
    fCaretX: Integer;
    fLastCaretX: integer;
    fCaretY: Integer;
    fCharsInWindow: Integer;
    fCharWidth: Integer;
    fFontDummy: TFont;
    fFontSmoothing: TSynFontSmoothMethod;
    fMouseMoved: boolean;
{$IFDEF SYN_MBCSSUPPORT}
    fImeCount: Integer;
    fMBCSStepAside: Boolean;
{$ENDIF}
    fInserting: Boolean;
    fPainting: boolean;
    fLines: TSynEditStringList;
    fOrigLines: TSynEditStringList;
    fOrigUndoList: TSynEditUndoList;
    fOrigRedoList: TSynEditUndoList;
    fLinesInWindow: Integer;
    fLeftChar: Integer;
    fMaxScrollWidth: Integer;
    fPaintLock: Integer;
    fReadOnly: Boolean;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    fScrollHintColor: TColor;
    fScrollHintFormat: TScrollHintFormat;
    FScrollBars: TScrollStyle;
    fTextHeight: Integer;
    fTextOffset: Integer;
    fTopLine: Integer;
    fHighlighter: TSynCustomHighlighter;
    fSelectedColor: TSynSelectedColor;
    fActiveLineColor: TColor;
    fUndoList: TSynEditUndoList;
    fRedoList: TSynEditUndoList;
    fBookMarks: array[0..9] of TSynEditMark;
    fMouseDownX: integer;
    fMouseDownY: integer;
    fBookMarkOpt: TSynBookMarkOpt;
    fBorderStyle: TSynBorderStyle;
    fHideSelection: boolean;
    fMouseWheelAccumulator: integer;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    fCaretOffset: TPoint;
    fKeyStrokes: TSynEditKeyStrokes;
    fModified: Boolean;
    fLastModifyTime: TDateTime;
    fMarkList: TSynEditMarkList;
    fExtraLineSpacing: integer;
    fSelectionMode: TSynSelectionMode;
    fActiveSelectionMode: TSynSelectionMode; //mode of the active selection
    fWantReturns: boolean;
    fWantTabs: boolean;
    fGutter: TSynGutter;
    fTabWidth: integer;
    fTextDrawer: TheTextDrawer;
    fInvalidateRect: TRect;
    fStateFlags: TSynStateFlags;
    fOptions: TSynEditorOptions;
    fStatusChanges: TSynStatusChanges;
    fLastKey: word;
    fLastShiftState: TShiftState;
    fSearchEngine: TSynEditSearchCustom;
    fHookedCommandHandlers: TList;
    fKbdHandler: TSynEditKbdHandler;
    fFocusList: TList;
    fPlugins: TList;
    fScrollTimer: TTimer;
    fScrollDeltaX, fScrollDeltaY: Integer;
    // event handlers
    fOnChange: TNotifyEvent;
    fOnClearMark: TPlaceMarkEvent;
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TDropFilesEvent;
    fOnGutterClick: TGutterClickEvent;
    fOnImeInput: TImeInputEvent;
    fOnMouseCursor: TMouseCursorEvent;
    fOnPaint: TPaintEvent;
    fOnPaintHighlightToken : TPaintHighlightTokenEvent;
    fOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent;
    fOnEditingAreas: TEditingAreasEvent;
    fOnContextHelp: TContextHelpEvent;
    fOnPaintTransient: TPaintTransient;
    fOnScroll: TScrollEvent;
    fOnGutterGetText: TGutterGetTextEvent;
    fOnGutterPaint: TGutterPaintEvent;
    fOnStatusChange: TStatusChangeEvent;
    fShowSpecChar: Boolean;
    fPaintTransientLock: Integer;
    fIsScrolling: Boolean;
    fChainListCleared: TNotifyEvent;
    fChainListDeleted: TStringListChangeEvent;
    fChainListInserted: TStringListChangeEvent;
    fChainListPutted: TStringListChangeEvent;
    fChainLinesChanging: TNotifyEvent;
    fChainLinesChanged: TNotifyEvent;
    fChainedEditor: TCustomSynEdit;
    fChainUndoAdded: TNotifyEvent;
    fChainRedoAdded: TNotifyEvent;
    fPainterLock:integer;
    fUndoing: boolean;

    procedure ReScanForFoldRanges;
    procedure ReScan;
    procedure ScanForFoldRanges(TopFoldRanges: TSynEditFoldRanges; LinesToScan: TStrings);
    function GetLineIndent(const Line: AnsiString): Integer;
    function GetPreviousLeftBracket(x:Integer; y: Integer): TBufferCoord;
    procedure SetUseCodeFolding(value: boolean);
    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    procedure ComputeScroll(X, Y: Integer);
    procedure DoBlockIndent;
    procedure DoBlockUnindent;
    procedure DoHomeKey(Selection: boolean);
    procedure DoEndKey(aSelection: Boolean);
    procedure DoLinesDeleted(FirstLine, Count: integer);
    procedure DoLinesInserted(FirstLine, Count: integer);
    procedure DoShiftTabKey;
    procedure DoTabKey;
    procedure DoComment;
    procedure DoUncomment;
    procedure DoCaseChange(const Cmd: TSynEditorCommand);
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): integer;
    procedure SynFontChanged(Sender: TObject);
    function GetBlockBegin: TBufferCoord;
    function GetBlockEnd: TBufferCoord;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretXY: TBufferCoord;
    function GetDisplayX: Integer;
    function GetDisplayY: Integer;
    function GetDisplayXY: TDisplayCoord;
    function GetDisplayLineCount: integer;
    function GetFont: TFont;
    function GetHookedCommandHandlersCount: integer;
    function GetLineText: string;
    function GetMaxUndo: Integer;
    function GetOptions: TSynEditorOptions;
    function GetSelAvail: Boolean;
    function GetSelTabBlock: Boolean;
    function GetSelTabLine: Boolean;
    function GetSelText: string;
    function SynGetText: string;
    function GetWordOffsetAtCursor: Integer;
    function GetWordAtCursor: string;
    function GetWordAtMouse: string;
    procedure GutterChanged(Sender: TObject);
    procedure InsertBlock(const BB, BE: TBufferCoord; ChangeStr: PChar; AddToUndoList: Boolean);
    procedure LinesChanging(Sender: TObject);
    procedure MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
      SelectionCommand: boolean);
    procedure MoveCaretHorz(DX: integer; SelectionCommand: boolean);
    procedure MoveCaretVert(DY: integer; SelectionCommand: boolean);
    procedure PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: integer);
    procedure ReadAddedKeystrokes(Reader: TReader);
    procedure ReadRemovedKeystrokes(Reader: TReader);
    function ScanFrom(Index: integer): integer;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SelectedColorsChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TBufferCoord);
    procedure SetBlockEnd(Value: TBufferCoord);
    procedure SetBorderStyle(Value: TSynBorderStyle);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure InternalSetCaretX(Value: Integer);
    procedure InternalSetCaretY(Value: Integer);
    procedure SetInternalDisplayXY(const aPos: TDisplayCoord);
    procedure SetActiveLineColor(Value: TColor);
    procedure SetExtraLineSpacing(const Value: integer);
    procedure SetFont(const Value: TFont);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetGutterWidth(Value: Integer);
    procedure SetHideSelection(const Value: boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetLeftChar(Value: Integer);
    procedure SetLines(Value: TSynEditStringList);
    procedure SetLineText(Value: string);
    procedure SetMaxScrollWidth(Value: integer);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetSearchEngine(Value: TSynEditSearchCustom);
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure SetActiveSelectionMode(const Value: TSynSelectionMode);
    procedure SetSelTextExternal(const Value: string);
    procedure SetTabWidth(Value: integer);
    procedure SynSetText(const Value: string);
    procedure SetTopLine(Value: Integer);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure ProperSetLine(ALine: integer; const ALineText: string);
    procedure UpdateModifiedStatus;
    procedure UndoRedoAdded(Sender: TObject);
    procedure UpdateLastCaretX;
    procedure UpdateScrollBars;
    procedure WriteAddedKeystrokes(Writer: TWriter);
    procedure WriteRemovedKeystrokes(Writer: TWriter);
    procedure ClearAreaList(areaList:TList);
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure InvalidateRect(const aRect: TRect; aErase: boolean); virtual;
    procedure DblClick; override;
    procedure DecPaintLock;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; virtual;
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    function GetReadOnly: boolean; virtual;
    procedure HighlighterAttrChanged(Sender: TObject);
    procedure IncPaintLock;
    procedure InitializeCaret;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure LinesChanged(Sender: TObject); virtual;
    procedure ListCleared(Sender: TObject);
    procedure ListDeleted(Sender: TObject; aIndex: integer; aCount: integer);
    procedure ListInserted(Sender: TObject; Index: integer; aCount: integer);
    procedure ListPutted(Sender: TObject; Index: integer; aCount: integer);
    //helper procs to chain list commands
    procedure ChainListCleared(Sender: TObject);
    procedure ChainListDeleted(Sender: TObject; aIndex: integer; aCount: integer);
    procedure ChainListInserted(Sender: TObject; aIndex: integer; aCount: integer);
    procedure ChainListPutted(Sender: TObject; aIndex: integer; aCount: integer);
    procedure ChainLinesChanging(Sender: TObject);
    procedure ChainLinesChanged(Sender: TObject);
    procedure ChainUndoRedoAdded(Sender: TObject);
    procedure ScanRanges;
    procedure Loaded; override;
    procedure MarkListChange(Sender: TObject);
{$IFDEF SYN_MBCSSUPPORT}
    procedure MBCSGetSelRangeInLineWhenColumnSelectionMode(const s: string;
      var ColFrom, ColTo: Integer);
{$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure NotifyHookedCommandHandlers(AfterProcessing: boolean;
      var Command: TSynEditorCommand; var AChar: char; Data: pointer); virtual;
    procedure Paint; override;
    procedure PaintGutter(const AClip: TRect; const aFirstRow,
      aLastRow: integer); virtual;
    procedure PaintTextLines(AClip: TRect; const aFirstRow, aLastRow,
      FirstCol, LastCol: integer); virtual;
    procedure RecalcCharExtent;
    procedure RedoItem;
    procedure InternalSetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TBufferCoord); virtual;
    procedure SetFontSmoothing(AValue: TSynFontSmoothMethod);
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetWantReturns(Value: Boolean);
    procedure SetSelTextPrimitive(const aValue: string);
    procedure SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode; Value: PChar;
      AddToUndoList: Boolean);
    procedure SetWantTabs(Value: Boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState;
      var Data: pointer): TSynEditorCommand;
    procedure UndoItem;
    procedure UpdateMouseCursor; virtual;
  protected
    fGutterWidth: Integer;
    fInternalImage: TSynInternalImage;
    procedure HideCaret;
    procedure ShowCaret;
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand; AChar: char;
      Data: pointer); virtual;
    // no method DoOnDropFiles, intercept the WM_DROPFILES instead
    procedure DoOnGutterClick(Button: TMouseButton; X, Y: integer); virtual;
    procedure DoOnPaint; virtual;
    procedure DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean); virtual;
    procedure DoOnPaintTransient(TransientType: TTransientType); virtual;

    procedure DoOnPlaceMark(var Mark: TSynEditMark); virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: char; Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: string;
      Line, Column, wordlen: integer): TSynReplaceAction; virtual;
    function DoOnSpecialLineColors(Line: integer;
      var Foreground, Background: TColor): boolean; virtual;
    function DoOnEditAreas(Line:integer; areaList:TList; var borderColor:TColor;var areaType:TEditingAreaType): boolean; virtual;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    function GetSelEnd: integer;
    function GetSelStart: integer;
    function GetSelLength: integer;
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelLength(const Value: integer);
    procedure SetAlwaysShowCaret(const Value: Boolean);
    procedure PrepareIdentChars(var IdentChars, WhiteChars: TSynIdentChars);
    procedure LinesHookChanged;
    property InternalCaretX: Integer write InternalSetCaretX;
    property InternalCaretY: Integer write InternalSetCaretY;
    property InternalCaretXY: TBufferCoord write InternalSetCaretXY;
    property FontSmoothing: TSynFontSmoothMethod read fFontSmoothing write SetFontSmoothing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property AlwaysShowCaret: Boolean read FAlwaysShowCaret
      write SetAlwaysShowCaret;
    procedure UpdateCaret;
    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word = 0; SS2: TShiftState = []);
    function LeftSpaces(const Line: string): Integer;
    function LeftSpacesEx(const Line: string; WantTabs: Boolean): Integer;
    procedure BeginUndoBlock;
    procedure BeginUpdate;
    function CaretInView: Boolean;
    function CharIndexToRowCol(Index: integer): TBufferCoord;
    procedure Clear;
    procedure ClearAll;
    procedure ClearBookMark(BookMark: Integer);
    procedure ClearSelection;
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: char;
      Data: pointer); virtual;
    procedure ClearUndo;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure DoCopyToClipboard(const SText: string);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUndoBlock;
    procedure EndUpdate;
    procedure EnsureCursorPosVisible;
    procedure EnsureCursorPosVisibleEx(ForceToMiddle: Boolean);
    procedure FindMatchingBracket; virtual;
    function GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): string;
    function GetMatchingBracket: TBufferCoord; virtual;
    function GetMatchingBracketEx(const APoint: TBufferCoord): TBufferCoord; virtual;
    function ExecuteAction(Action: TBasicAction): boolean; override;
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: char;
      Data: pointer); virtual;
    function GetBookMark(BookMark: integer; var X, Y: integer): boolean;
    function GetHighlighterAttriAtRowCol(const XY: TBufferCoord; var Token: string;
      var Attri: TSynHighlighterAttributes): boolean; overload;
    function GetHighlighterAttriAtRowCol(const XY: TBufferCoord; var Token: string;
      var tokenFinished: boolean;  var TokenType: TSynHighlighterTokenType;
      var Attri: TSynHighlighterAttributes): boolean; overload;
    function GetHighlighterAttriAtRowColEx(const XY: TBufferCoord; var Token: string;
      var TokenType: TSynhighlighterTokenType ; var TokenKind,Start: Integer;
      var Attri: TSynHighlighterAttributes): boolean;
    function GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
    function GetLineOfMouse(out line: integer): boolean;
    function GetWordAtRowCol(XY: TBufferCoord): string;
    procedure GotoBookMark(BookMark: Integer);
    procedure SetCaretXYCentered(ForceToMiddle: Boolean; const Value: TBufferCoord);
    function IdentChars: TSynIdentChars;
    procedure Invalidate; override;
    procedure InvalidateGutter;
    procedure InvalidateGutterLine(aLine: integer);
    procedure InvalidateGutterLines(FirstLine, LastLine: integer);
    procedure InvalidateLine(Line: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    procedure InvalidateSelection;
    function IsBookmark(BookMark: integer): boolean;
    function IsPointInSelection(const Value: TBufferCoord): boolean;
    procedure LockUndo;
    function BufferToDisplayPos(const p: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const p: TDisplayCoord): TBufferCoord;
    function LineToRow(aLine: integer): integer;
    function RowToLine(aRow: integer): integer;
    function NextWordPos: TBufferCoord; virtual;
    function NextWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PasteFromClipboard;
    function WordStart: TBufferCoord; virtual;
    function WordStartEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordEnd: TBufferCoord; virtual;
    function WordEndEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function PrevWordPos: TBufferCoord; virtual;
    function PrevWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function PixelsToRowColumn(aX, aY: integer): TDisplayCoord;
    function PixelsToNearestRowColumn(aX, aY: integer): TDisplayCoord;
    procedure Redo;
    procedure RegisterCommandHandler(const AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    function RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
    function RowColToCharIndex(RowCol: TBufferCoord): integer;
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions): integer;
    procedure SelectAll;
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetCaretAndSelection(const ptCaret, ptBefore, ptAfter: TBufferCoord);
    procedure SetDefaultKeystrokes; virtual;
    procedure SetSelWord;
    procedure SetWordBlock(Value: TBufferCoord);
    procedure Undo;
    procedure UnlockUndo;
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);
    function UpdateAction(Action: TBasicAction): boolean; override;
    procedure SetFocus; override;

    procedure AddKeyUpHandler(aHandler: TKeyEvent);
    procedure RemoveKeyUpHandler(aHandler: TKeyEvent);
    procedure AddKeyDownHandler(aHandler: TKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TKeyEvent);
    procedure AddKeyPressHandler(aHandler: TKeyPressEvent);
    procedure RemoveKeyPressHandler(aHandler: TKeyPressEvent);
    procedure AddFocusControl(aControl: TWinControl);
    procedure RemoveFocusControl(aControl: TWinControl);
    procedure AddMouseDownHandler(aHandler: TMouseEvent);
    procedure RemoveMouseDownHandler(aHandler: TMouseEvent);
    procedure AddMouseUpHandler(aHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(aHandler: TMouseEvent);
    procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);

    procedure WndProc(var Msg: TMessage); override;
    procedure SetLinesPointer(ASynEdit: TCustomSynEdit);
    procedure RemoveLinesPointer;
    procedure HookTextBuffer(aBuffer: TSynEditStringList;
      aUndo, aRedo: TSynEditUndoList);
    procedure UnHookTextBuffer;
    function IsEmpty: boolean;
    procedure CollapseAll;
    procedure UncollapseAll;
    procedure Collapse(FoldRange: TSynEditFoldRange);
    procedure Uncollapse(FoldRange: TSynEditFoldRange);
    procedure UncollapseAroundLine(Line: Integer);
    function FoldStartAtLine(Line: Integer): TSynEditFoldRange;
    function CollapsedFoldStartAtLine(Line: Integer): TSynEditFoldRange;
    function FoldEndAtLine(Line: Integer): TSynEditFoldRange;
    function FoldAroundLineEx(Line: Integer; WantCollapsed, AcceptFromLine, AcceptToLine: Boolean): TSynEditFoldRange;
    function CollapsedFoldAroundLine(Line: Integer): TSynEditFoldRange; // insides + collapsed
    function FoldAroundLine(Line: Integer): TSynEditFoldRange; // insides
    function FoldHidesLine(Line: Integer): TSynEditFoldRange; // inside + toline
    function FoldExtendsLine(Line: Integer): TSynEditFoldRange; // fromline + inside + toline
    function FoldLineToRow(Line: Integer): Integer;
    function FoldRowToLine(Row: Integer): Integer;
    procedure FoldOnListCleared;
    procedure FoldOnListDeleted(Line: Integer; Count: Integer);
    procedure FoldOnListInserted(Line: Integer; Count: Integer);
    procedure LockPainter;
    procedure UnlockPainter;
    property CodeFolding: TSynCodeFolding read fCodeFolding;
    property BlockBegin: TBufferCoord read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TBufferCoord read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: boolean read GetCanRedo;
    property CanUndo: boolean read GetCanUndo;
    property CaretX: Integer read fCaretX write SetCaretX;
    property CaretY: Integer read fCaretY write SetCaretY;
    property CaretXY: TBufferCoord read GetCaretXY write SetCaretXY;
    property ActiveLineColor: TColor read fActiveLineColor
      write SetActiveLineColor default clNone;
    property DisplayX: Integer read GetDisplayX;
    property DisplayY: Integer read GetDisplayY;
    property DisplayXY: TDisplayCoord read GetDisplayXY;
    property DisplayLineCount: integer read GetDisplayLineCount;
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: integer read fCharWidth;
    property Color;
    property Font: TFont read GetFont write SetFont;
    property GutterWidth: Integer read fGutterWidth;
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow;
    property LineText: string read GetLineText write SetLineText;
    property Lines: TSynEditStringList read fLines write SetLines;
    property Marks: TSynEditMarkList read fMarkList;
    property MaxScrollWidth: integer read fMaxScrollWidth write SetMaxScrollWidth
      default 1024;
    property Modified: Boolean read fModified write SetModified;
    property LastModifyTime: TDateTime read fLastModifyTime;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default FALSE;
    property SearchEngine: TSynEditSearchCustom read fSearchEngine write SetSearchEngine;
    property SelAvail: Boolean read GetSelAvail;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelTabBlock: Boolean read GetSelTabBlock;
    property SelTabLine: Boolean read GetSelTabLine;
    property SelText: string read GetSelText write SetSelTextExternal;
    property StateFlags: TSynStateFlags read fStateFlags write fStateFlags;
    property Text: string read SynGetText write SynSetText;
    property TopLine: Integer read fTopLine write SetTopLine;
    property WordOffsetAtCursor: Integer read GetWordOffsetAtCursor;
    property WordAtCursor: string read GetWordAtCursor;
    property WordAtMouse: string read GetWordAtMouse;
    property UndoList: TSynEditUndoList read fUndoList;
    property RedoList: TSynEditUndoList read fRedoList;
  public
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;

    property BookMarkOptions: TSynBookMarkOpt
      read fBookMarkOpt write fBookMarkOpt;
    property BorderStyle: TSynBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property ExtraLineSpacing: integer
      read fExtraLineSpacing write SetExtraLineSpacing default 0;
    property Gutter: TSynGutter read fGutter write SetGutter;
    property HideSelection: boolean read fHideSelection write SetHideSelection
      default false;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property IsScrolling: Boolean read FIsScrolling;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes stored False;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 1024;
    property Options: TSynEditorOptions read GetOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret
      write SetOverwriteCaret default ctBlock;
    property RightEdge: Integer read fRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor
      read fRightEdgeColor write SetRightEdgeColor default clSilver;
    property ScrollHintColor: TColor read fScrollHintColor
      write fScrollHintColor default clInfoBk;
    property ScrollHintFormat: TScrollHintFormat read fScrollHintFormat
      write fScrollHintFormat default shfTopLineOnly;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property SelectedColor: TSynSelectedColor
      read FSelectedColor write FSelectedColor;
    property UseCodeFolding: boolean read fUseCodeFolding write SetUseCodeFolding default False;
    property SelectionMode: TSynSelectionMode
      read FSelectionMode write SetSelectionMode default smNormal;
    property ActiveSelectionMode: TSynSelectionMode read fActiveSelectionMode
      write SetActiveSelectionMode stored False;
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantReturns: boolean read fWantReturns write SetWantReturns default True;
    property WantTabs: boolean read fWantTabs write SetWantTabs default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClearBookmark: TPlaceMarkEvent read fOnClearMark
      write fOnClearMark;
    property OnCommandProcessed: TProcessCommandEvent
      read fOnCommandProcessed write fOnCommandProcessed;
    property OnContextHelp: TContextHelpEvent
      read fOnContextHelp write fOnContextHelp;
    property OnDropFiles: TDropFilesEvent read fOnDropFiles write fOnDropFiles;
    property OnGutterClick: TGutterClickEvent
      read fOnGutterClick write fOnGutterClick;
    property OnGutterGetText: TGutterGetTextEvent read fOnGutterGetText
      write fOnGutterGetText;
    property OnGutterPaint: TGutterPaintEvent read fOnGutterPaint
      write fOnGutterPaint;
    property OnImeInput: TImeInputEvent read fOnImeInput write fOnImeInput;
    property OnMouseCursor: TMouseCursorEvent read fOnMouseCursor
      write fOnMouseCursor;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    property OnPaintHighlightToken : TPaintHighlightTokenEvent
      read fOnPaintHighlightToken write fOnPaintHighlightToken;
    property OnPlaceBookmark: TPlaceMarkEvent
      read FOnPlaceMark write FOnPlaceMark;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText
      write fOnReplaceText;
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read fOnSpecialLineColors write fOnSpecialLineColors;
    property OnEditingAreas: TEditingAreasEvent
      read fOnEditingAreas write fOnEditingAreas;
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
    property OnPaintTransient: TPaintTransient
      read fOnPaintTransient write fOnPaintTransient;
    property OnScroll: TScrollEvent
      read fOnScroll write fOnScroll;
  published
    property Cursor default crIBeam;
  end;

  TSynEdit = class(TCustomSynEdit)
  published
    // inherited properties
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property ActiveLineColor;
    property Ctl3D;
    property ParentCtl3D;
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    // TCustomSynEdit properties
    property BookMarkOptions;
    property BorderStyle;
    property ExtraLineSpacing;
    property Gutter;
    property HideSelection;
    property Highlighter;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property UseCodeFolding;
    property Lines;
    property MaxScrollWidth;
    property MaxUndo;
    property Options;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollHintColor;
    property ScrollHintFormat;
    property ScrollBars;
    property SearchEngine;
    property SelectedColor;
    property SelectionMode;
    property TabWidth;
    property WantReturns;
    property WantTabs;
    // TCustomSynEdit events
    property OnChange;
    property OnClearBookmark;
    property OnCommandProcessed;
    property OnContextHelp;
    property OnDropFiles;
    property OnGutterClick;
    property OnGutterGetText;
    property OnGutterPaint;
    property OnMouseCursor;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnScroll;
    property OnSpecialLineColors;
    property OnStatusChange;
    property OnPaintTransient;
    property FontSmoothing;
  end;

implementation

{$R SynEdit.res}

uses
  StdActns,
  Clipbrd,
  ShellAPI,
  SynEditWordWrap,
  SynEditStrConst;

function TrimTrailingSpaces(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] in [#32, #9]) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function StringIsBlank(const S: string): Boolean;
var
  i: Integer;
begin
  for i := Length(S) downto 1 do begin
    if not (S[i] in [#32, #9]) then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{ THookedCommandHandlerEntry }

type
  THookedCommandHandlerEntry = class(TObject)
  private
    fEvent: THookedCommandEvent;
    fData: pointer;
    constructor Create(AEvent: THookedCommandEvent; AData: pointer);
    function Equals(AEvent: THookedCommandEvent): boolean;
  end;

constructor THookedCommandHandlerEntry.Create(AEvent: THookedCommandEvent;
  AData: pointer);
begin
  inherited Create;
  fEvent := AEvent;
  fData := AData;
end;

function THookedCommandHandlerEntry.Equals(AEvent: THookedCommandEvent): boolean;
begin
  with TMethod(fEvent) do
    Result := (Code = TMethod(AEvent).Code) and (Data = TMethod(AEvent).Data);
end;

{ TCustomSynEdit }

function TCustomSynEdit.PixelsToNearestRowColumn(aX, aY: integer): TDisplayCoord;
// Result is in display coordinates
var
  f: Single;
begin
  f := (aX - fGutterWidth - 2) / fCharWidth;
  // don't return a partially visible last line
  if aY >= fLinesInWindow * fTextHeight then begin
    aY := fLinesInWindow * fTextHeight - 1;
    if aY < 0 then
      aY := 0;
  end;
  Result.Column := Max(1, LeftChar + Round(f));
  Result.Row := Max(1, TopLine + (aY div fTextHeight));
end;

function TCustomSynEdit.PixelsToRowColumn(aX, aY: integer): TDisplayCoord;
begin
  Result.Column := Max(1, LeftChar + ((aX - fGutterWidth - 2) div fCharWidth));
  Result.Row := Max(1, TopLine + (aY div fTextHeight));
end;

function TCustomSynEdit.RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
begin
  Result.X := (RowCol.Column - 1) * fCharWidth + fTextOffset;
  Result.Y := (RowCol.Row - fTopLine) * fTextHeight;
end;

procedure TCustomSynEdit.ComputeCaret(X, Y: Integer);
//X,Y are pixel coordinates
var
  vCaretNearestPos: TDisplayCoord;
begin
  vCaretNearestPos := PixelsToNearestRowColumn(X, Y);
  vCaretNearestPos.Row := MinMax(vCaretNearestPos.Row, 1, DisplayLineCount);
  SetInternalDisplayXY(vCaretNearestPos);
end;

procedure TCustomSynEdit.ComputeScroll(X, Y: Integer);
//X,Y are pixel coordinates
var
  iScrollBounds: TRect; { relative to the client area }
begin
  { don't scroll if dragging text from other control }
  if (not MouseCapture) and (not Dragging) then begin
    fScrollTimer.Enabled := False;
    Exit;
  end;

  iScrollBounds := Bounds(fGutterWidth, 0, fCharsInWindow * fCharWidth,
    fLinesInWindow * fTextHeight);
  if BorderStyle = bsNone then
    InflateRect(iScrollBounds, -2, -2);

  if X < iScrollBounds.Left then
    fScrollDeltaX := (X - iScrollBounds.Left) div fCharWidth - 1
  else if X >= iScrollBounds.Right then
    fScrollDeltaX := (X - iScrollBounds.Right) div fCharWidth + 1
  else
    fScrollDeltaX := 0;

  if Y < iScrollBounds.Top then
    fScrollDeltaY := (Y - iScrollBounds.Top) div fTextHeight - 1
  else if Y >= iScrollBounds.Bottom then
    fScrollDeltaY := (Y - iScrollBounds.Bottom) div fTextHeight + 1
  else
    fScrollDeltaY := 0;

  fScrollTimer.Enabled := (fScrollDeltaX <> 0) or (fScrollDeltaY <> 0);
end;

procedure TCustomSynEdit.DoCopyToClipboard(const SText: string);
var
  Mem: HGLOBAL;
  P: PChar;
  SLen: integer;
  Failed: boolean;
begin
  if SText <> '' then begin
    Failed := TRUE; // assume the worst.
    SLen := Length(SText);
    // Open and Close are the only TClipboard methods we use because TClipboard
    // is very hard (impossible) to work with if you want to put more than one
    // format on it at a time.
    Clipboard.Open;
    try
      // Clear anything already on the clipboard.
      EmptyClipboard;
      // Put it on the clipboard as normal text format so it can be pasted into
      // things like notepad or Delphi.
      Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen + 1);
      if Mem <> 0 then begin
        P := GlobalLock(Mem);
        try
          if P <> nil then begin
            Move(PChar(SText)^, P^, SLen + 1);
            // Put it on the clipboard in text format
            Failed := SetClipboardData(CF_TEXT, Mem) = 0;
          end;
        finally
          GlobalUnlock(Mem);
        end;
      end;
      // Don't free Mem!  It belongs to the clipboard now, and it will free it
      // when it is done with it.
      if not Failed then begin
        // Copy it in our custom format so we know what kind of block it is.
        // That effects how it is pasted in.
        Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen +
          SizeOf(TSynSelectionMode) + 1);
        P := GlobalLock(Mem);
        try
          if P <> nil then begin
            // Our format:  TSynSelectionMode value followed by text.
            PSynSelectionMode(P)^ := fActiveSelectionMode;
            inc(P, SizeOf(TSynSelectionMode));
            Move(PChar(SText)^, P^, SLen + 1);
            Failed := SetClipboardData(SynEditClipboardFormat, Mem) = 0;
          end;
        finally
          GlobalUnlock(Mem);
        end;
        // Don't free Mem!  It belongs to the clipboard now, and it will free it
        // when it is done with it.
      end;
    finally
      Clipboard.Close;
      if Failed then
        raise ESynEditError.Create('Clipboard copy operation failed');
    end;
  end;
end;

procedure TCustomSynEdit.CopyToClipboard;
var
  SText: string;
  ChangeTrim: boolean;
begin
  if SelAvail then begin
    ChangeTrim := (fActiveSelectionMode = smColumn) and (eoTrimTrailingSpaces in Options);
    try
      if ChangeTrim then
        Exclude(fOptions, eoTrimTrailingSpaces);
      SText := SelText;
    finally
      if ChangeTrim then
        Include(fOptions, eoTrimTrailingSpaces);
    end;
    DoCopyToClipboard(SText);
  end;
end;

procedure TCustomSynEdit.CutToClipboard;
begin
  if not ReadOnly and SelAvail then begin
    BeginUndoBlock;
    try
      DoCopyToClipboard(SelText);
      SelText := '';
    finally
      EndUndoBlock;
    end;
  end;
end;

constructor TCustomSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPaintLock := 0;
  fPainting := False;
  fLines := TSynEditStringList.Create;
  fOrigLines := fLines;
  fPlugins := TList.Create;
  fMouseMoved := False;
  fUndoing:=False;
  with fLines do begin
    OnChange := LinesChanged;
    OnChanging := LinesChanging;
    OnCleared := ListCleared;
    OnDeleted := ListDeleted;
    OnInserted := ListInserted;
    OnPutted := ListPutted;
  end;
  fFontDummy := TFont.Create;
  fUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := UndoRedoAdded;
  fOrigUndoList := fUndoList;
  fRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := UndoRedoAdded;
  fOrigRedoList := fRedoList;

  DoubleBuffered := false;
  fActiveLineColor := clNone;
  fSelectedColor := TSynSelectedColor.Create;
  fSelectedColor.OnChange := SelectedColorsChanged;
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := BookMarkOptionsChanged;
  //  fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  fGutter := TSynGutter.Create;
  fGutter.RightOffset := 21;
  fGutter.OnChange := GutterChanged;
  fGutterWidth := fGutter.Width;
  fTextOffset := fGutterWidth + 2;
  ControlStyle := ControlStyle + [csOpaque, csSetCaption, csNeedsBorderPaint];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  Color := clWindow;
  fFontDummy.Name := 'Courier New';
  fFontDummy.Size := 10;
{$IFDEF SYN_KYLIX}
  fFontDummy.Name := 'adobe-courier';
  if fFontDummy.Name = 'adobe-courier' then
    fFontDummy.Size := 12
  else begin
    fFontDummy.Name := 'terminal';
    fFontDummy.Size := 14;
  end;
{$ENDIF}
  fFontDummy.CharSet := DEFAULT_CHARSET;
  fTextDrawer := TheTextDrawer.Create([fsBold], fFontDummy);
  Font.Assign(fFontDummy);
  Font.OnChange := SynFontChanged;
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  fInserting := True;
  fMaxScrollWidth := 1024;
  fScrollBars := ssBoth;
  fBorderStyle := bsSingle;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  FSelectionMode := smNormal;
  fActiveSelectionMode := smNormal;
  fFocusList := TList.Create;
  fKbdHandler := TSynEditKbdHandler.Create;
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  fMarkList := TSynEditMarkList.Create();
  fMarkList.OnChange := MarkListChange;
  SetDefaultKeystrokes;
  fRightEdgeColor := clSilver;
{$IFDEF SYN_MBCSSUPPORT}
  fImeCount := 0;
  fMBCSStepAside := False;
{$ENDIF}
  fWantReturns := True;
  fWantTabs := False;
  fTabWidth := 8;
  fLeftChar := 1;
  fTopLine := 1;
  fCaretX := 1;
  fLastCaretX := 1;
  fCaretY := 1;
  fBlockBegin.Char := 1;
  fBlockBegin.Line := 1;
  fBlockEnd := fBlockBegin;
  fOptions := SYNEDIT_DEFAULT_OPTIONS;
  fScrollTimer := TTimer.Create(Self);
  fScrollTimer.Enabled := False;
  fScrollTimer.Interval := 100;
  fScrollTimer.OnTimer := ScrollTimerHandler;

  fScrollHintColor := clInfoBk;
  fScrollHintFormat := shfTopLineOnly;

  SynFontChanged(nil);

  fAllFoldRanges := TSynEditFoldRanges.Create;
  fAllFoldRanges.OwnsObjects := true;
  fCodeFolding := TSynCodeFolding.Create;
end;

procedure TCustomSynEdit.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  // Clear WindowText to avoid it being used as Caption, or else window creation will
  // fail if it's bigger than 64KB. It's useless to set the Caption anyway.
  StrDispose(WindowText);
  WindowText := nil;
  inherited CreateParams(Params);
  with Params do begin
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or BorderStyles[fBorderStyle] or WS_CLIPCHILDREN;

    if NewStyleControls and Ctl3D and (fBorderStyle = bsSingle) then begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TCustomSynEdit.DecPaintLock;
begin
  Assert(fPaintLock > 0);
  Dec(fPaintLock);
  if (fPaintLock = 0) and HandleAllocated then begin
    if sfScrollbarChanged in fStateFlags then
      UpdateScrollbars;
    if sfCaretChanged in fStateFlags then
      UpdateCaret;
    if fStatusChanges <> [] then
      DoOnStatusChange(fStatusChanges);
  end;
end;

destructor TCustomSynEdit.Destroy;
var
  i: integer;
begin
  Highlighter := nil;
  if (fChainedEditor <> nil) or (fLines <> fOrigLines) then
    RemoveLinesPointer;

  inherited Destroy;

  // free listeners while other fields are still valid
  if Assigned(fHookedCommandHandlers) then begin
    for i := 0 to fHookedCommandHandlers.Count - 1 do
      THookedCommandHandlerEntry(fHookedCommandHandlers[i]).Free;
    FreeAndNil(fHookedCommandHandlers);
  end;
  if fPlugins <> nil then begin
    for i := fPlugins.Count - 1 downto 0 do
      TSynEditPlugin(fPlugins[i]).Free;
    fPlugins.Free;
  end;
  fMarkList.Free;
  fBookMarkOpt.Free;
  fKeyStrokes.Free;
  fKbdHandler.Free;
  fFocusList.Free;
  fSelectedColor.Free;
  fOrigUndoList.Free;
  fOrigRedoList.Free;
  fGutter.Free;
  fTextDrawer.Free;
  fInternalImage.Free;
  fFontDummy.Free;
  fOrigLines.Free;
  fAllFoldRanges.Free;
  fCodeFolding.Free;
end;

function TCustomSynEdit.GetBlockBegin: TBufferCoord;
begin
  if (fBlockEnd.Line < fBlockBegin.Line)
    or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char)) then
    Result := fBlockEnd
  else
    Result := fBlockBegin;
end;

function TCustomSynEdit.GetBlockEnd: TBufferCoord;
begin
  if (fBlockEnd.Line < fBlockBegin.Line)
    or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char)) then
    Result := fBlockBegin
  else
    Result := fBlockEnd;
end;

procedure TCustomSynEdit.SynFontChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(TRUE);
end;

function TCustomSynEdit.GetFont: TFont;
begin
  Result := inherited Font;
end;

function TCustomSynEdit.GetLineText: string;
begin
  if (CaretY >= 1) and (CaretY <= Lines.Count) then
    Result := Lines[CaretY - 1]
  else
    Result := '';
end;

function TCustomSynEdit.GetSelAvail: Boolean;
begin
  Result := (fBlockBegin.Char <> fBlockEnd.Char) or
    ((fBlockBegin.Line <> fBlockEnd.Line) and (fActiveSelectionMode <> smColumn));
end;

function TCustomSynEdit.GetSelTabBlock: Boolean;
begin
  Result := (fBlockBegin.Line <> fBlockEnd.Line) and (fActiveSelectionMode <> smColumn);
end;

function TCustomSynEdit.GetSelTabLine: Boolean;
begin
  Result := (BlockBegin.Char <= 1) and (BlockEnd.Char > length(Lines[CaretY - 1])) and SelAvail;
end;

function TCustomSynEdit.GetSelText: string;

  function CopyPadded(const S: string; Index, Count: integer): string;
  var
    SrcLen: Integer;
    DstLen: integer;
    P: PChar;
  begin
    SrcLen := Length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else begin
      SetLength(Result, DstLen);
      P := PChar(Result);
      StrPCopy(P, Copy(S, Index, Count));
      Inc(P, Length(S));
      FillChar(P^, DstLen - Srclen, $20);
    end;
  end;

  procedure CopyAndForward(const S: string; Index, Count: Integer; var P:
    PChar);
  var
    pSrc: PChar;
    SrcLen: Integer;
    DstLen: Integer;
  begin
    SrcLen := Length(S);
    if (Index <= SrcLen) and (Count > 0) then begin
      Dec(Index);
      pSrc := PChar(S) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, P^, DstLen);
      Inc(P, DstLen);
      P^ := #0;
    end;
  end;

  function CopyPaddedAndForward(const S: string; Index, Count: Integer;
    var P: PChar): Integer;
    // Return the number of trimmed blank spaces.
  var
    OldP: PChar;
    Len: Integer;
  begin
    Result := 0;
    OldP := P;
    CopyAndForward(S, Index, Count, P);
    Len := Count - (P - OldP);
    if not (eoTrimTrailingSpaces in Options) then begin
      FillChar(P^, Len, #$20);
      Inc(P, Len);
    end else
      Result := Len;
  end;

var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
  l, r: Integer;
  s: string;
  P: PChar;
  cRow: Integer;
  vAuxLineChar: TBufferCoord;
  vAuxRowCol: TDisplayCoord;
  vTrimCount: Integer;
begin
  if not SelAvail then
    Result := ''
  else begin
    ColFrom := BlockBegin.Char;
    First := BlockBegin.Line - 1;
    //
    ColTo := BlockEnd.Char;
    Last := BlockEnd.Line - 1;
    //
    TotalLen := 0;
    case fActiveSelectionMode of
      smNormal:
        if (First = Last) then
          Result := Copy(Lines[First], ColFrom, ColTo - ColFrom)
        else begin
          // step1: calculate total length of result string
          TotalLen := Max(0, Length(Lines[First]) - ColFrom + 1);
          for i := First + 1 to Last - 1 do
            Inc(TotalLen, Length(Lines[i]));
          Inc(TotalLen, ColTo - 1);
          Inc(TotalLen, Length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          CopyAndForward(Lines[First], ColFrom, MaxInt, P);

          CopyAndForward(sLineBreak, 1, MaxInt, P);

          for i := First + 1 to Last - 1 do begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, ColTo - 1, P);
        end;
      smColumn: begin
          with BufferToDisplayPos(BlockBegin) do begin
            First := Row;
            ColFrom := Column;
          end;
          with BufferToDisplayPos(BlockEnd) do begin
            Last := Row;
            ColTo := Column;
          end;
          if ColFrom > ColTo then
            SwapInt(ColFrom, ColTo);
          // step1: pre-allocate string large enough for worst case
          TotalLen := ((ColTo - ColFrom) + Length(sLineBreak)) *
            (Last - First + 1);
{$IFDEF SYN_MBCSSUPPORT}
          TotalLen := TotalLen * 2;
{$ENDIF}
          SetLength(Result, TotalLen);
          P := PChar(Result);
          // step2: copy chunks to the pre-allocated string
          TotalLen := 0;
          for cRow := First to Last do begin
            vAuxRowCol.Row := cRow;
            vAuxRowCol.Column := ColFrom;
            vAuxLineChar := DisplayToBufferPos(vAuxRowCol);
            l := vAuxLineChar.Char;
            s := Lines[vAuxLineChar.Line - 1];
            vAuxRowCol.Column := ColTo;
            r := DisplayToBufferPos(vAuxRowCol).Char;
{$IFDEF SYN_MBCSSUPPORT}
            MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
{$ENDIF}
            vTrimCount := CopyPaddedAndForward(s, l, r - l, P);
            TotalLen := TotalLen + (r - l) - vTrimCount + Length(sLineBreak);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          SetLength(Result, TotalLen - Length(sLineBreak));
        end;
      smLine: begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calculate total length of result string
          for i := First to Last do
            Inc(TotalLen, Length(Lines[i]) + Length(sLineBreak));
          if Last = Lines.Count - 1 then
            Dec(TotalLen, Length(sLineBreak));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          for i := First to Last - 1 do begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, MaxInt, P);
          if Last < Lines.Count - 1 then
            CopyAndForward(sLineBreak, 1, MaxInt, P);
        end;
    end;
  end;
end;

function TCustomSynEdit.SynGetText: string;
begin
  Result := Lines.Text;
end;

function TCustomSynEdit.GetWordOffsetAtCursor: Integer;
var
  Line: string;
  Len: Integer;
  i,total,Start: Integer;
  XY: TBufferCoord;
begin
  XY := CaretXY;
  Result := -1;
  if (XY.Line >= 1) and (XY.Line <= Lines.Count) then begin
    Line := Lines[XY.Line - 1];
    Len := Length(Line);
    if Len = 0 then
      Exit;
    if (XY.Char < 1) or (XY.Char > Len) then
      Exit;

    Start := XY.Char;
    if  (Start> 1) and not (Line[Start] in IdentChars) then
      Dec(Start);

    if Line[Start] in IdentChars then begin
      while (Start > 1) and (Line[Start - 1] in IdentChars) do
        Dec(Start);
      total :=0;
      for  i:=0 to XY.Line-2 do
        begin
          total := total + Length(Lines[i])+2;
        end;
      Result := total + XY.Char -1;
    end;
  end;
end;

function TCustomSynEdit.GetWordAtCursor: string;
begin
  Result := GetWordAtRowCol(CaretXY);
end;

procedure TCustomSynEdit.HideCaret;
begin
  if sfCaretVisible in fStateFlags then
    if Windows.HideCaret(Handle) then
      Exclude(fStateFlags, sfCaretVisible);
end;

{$IFDEF SYN_MBCSSUPPORT}

procedure TCustomSynEdit.WMImeComposition(var Msg: TMessage);
var
  imc: HIMC;
  p: PChar;
begin
  if ((Msg.LParam and GCS_RESULTSTR) <> 0) then begin
    imc := ImmGetContext(Handle);
    try
      fImeCount := ImmGetCompositionString(imc, GCS_RESULTSTR, nil, 0);
      GetMem(p, fImeCount + 1);
      try
        ImmGetCompositionString(imc, GCS_RESULTSTR, p, fImeCount + 1);
        p[fImeCount] := #0;
        CommandProcessor(ecImeStr, #0, p);
      finally
        FreeMem(p, fImeCount + 1);
      end;
    finally
      ImmReleaseContext(Handle, imc);
    end;
  end;
  inherited;
end;

procedure TCustomSynEdit.WMImeNotify(var Msg: TMessage);
var
  imc: HIMC;
  logFont: TLogFont;
begin
  with Msg do begin
    case WParam of
      IMN_SETOPENSTATUS: begin
          imc := ImmGetContext(Handle);
          if (imc <> 0) then begin
            GetObject(Font.Handle, SizeOf(TLogFont), @logFont);
            ImmSetCompositionFont(imc, @logFont);

            ImmReleaseContext(Handle, imc);
          end;
        end;
    end;
  end;
  inherited;
end;
{$ENDIF}

procedure TCustomSynEdit.IncPaintLock;
begin
  inc(fPaintLock);
end;

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TCustomSynEdit.InvalidateGutterLine(aLine: integer);
begin
  if (aLine < 1) or (aLine > Lines.Count) then
    Exit;

  InvalidateGutterLines(aLine, aLine);
end;

procedure TCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := Rect(0, 0, fGutterWidth, ClientHeight);
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, False);
    end else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);
      if UseCodeFolding then begin
        FirstLine := LineToRow(FirstLine);
        if LastLine <= Lines.Count then
          LastLine := LineToRow(LastLine)
        else
          LastLine := MaxInt;
      end;
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);
      { any line visible? }
      if (LastLine >= FirstLine) then begin
        rcInval := Rect(0, fTextHeight * (FirstLine - TopLine),
          fGutterWidth, fTextHeight * (LastLine - TopLine + 1));
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, FALSE);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateLines(FirstLine, LastLine: integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TRect;
begin
  if (fPainterLock>0) then
    Exit;

  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := ClientRect;
      Inc(rcInval.Left, fGutterWidth);
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, FALSE);
    end else begin
      FirstLine := Max(FirstLine, 1);
      LastLine := Max(LastLine, 1);
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);

      if LastLine >= Lines.Count then
        LastLine := MaxInt; // paint empty space beyond last line

      if UseCodeFolding then begin
        FirstLine := LineToRow(FirstLine);
        // Could avoid this conversion if (First = Last) and
        // (Length < CharsInWindow) but the dependency isn't worth IMO.
        if LastLine < Lines.Count then
          LastLine := LineToRow(LastLine + 1) - 1;
      end;

      // TopLine is in display coordinates, so FirstLine and LastLine must be
      // converted previously.
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);

      { any line visible? }
      if (LastLine >= FirstLine) then begin
        rcInval := Rect(fGutterWidth, fTextHeight * (FirstLine - TopLine),
          ClientWidth, fTextHeight * (LastLine - TopLine + 1));
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, FALSE);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateSelection;
begin
  if (fPainterLock>0) then
    Exit;
  InvalidateLines(BlockBegin.Line, BlockEnd.Line);
end;

procedure TCustomSynEdit.Invalidate;
begin
  if (fPainterLock>0) then
    Exit;
  Inherited;
end;

procedure TCustomSynEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  fKbdHandler.ExecuteKeyUp(Self, Key, Shift);
end;

procedure TCustomSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: pointer;
  C: char;
  Cmd: TSynEditorCommand;
begin
  inherited;
  fKbdHandler.ExecuteKeyDown(Self, Key, Shift);

  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data);
    if Cmd <> ecNone then begin
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    end else
      Exclude(fStateFlags, sfIgnoreNextChar);
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
end;

procedure TCustomSynEdit.Loaded;
begin
  inherited Loaded;
  GutterChanged(Self);
  UpdateScrollBars;
end;

procedure TCustomSynEdit.KeyPress(var Key: Char);
begin
{$IFDEF SYN_MBCSSUPPORT}
  if (fImeCount > 0) then begin
    Dec(fImeCount);
    Exit;
  end;
{$ENDIF}
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then begin
    inherited;
    fKbdHandler.ExecuteKeyPress(Self, Key);
    CommandProcessor(ecChar, Key, nil);
  end else
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
end;

function TCustomSynEdit.LeftSpaces(const Line: string): Integer;
begin
  Result := LeftSpacesEx(Line, false);
end;

function TCustomSynEdit.LeftSpacesEx(const Line: string; WantTabs: Boolean): Integer;
var
  p: PChar;
begin
  p := pointer(Line);
  if Assigned(p) and (eoAutoIndent in fOptions) then begin
    Result := 0;
    while p^ in [#1..#32] do begin
      if (p^ = #9) and (WantTabs) then
        Inc(Result, TabWidth)
      else
        Inc(Result);
      Inc(p);
    end;
  end else
    Result := 0;
end;

function TCustomSynEdit.GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): string;
begin
  if (WantTabs) and (not (eoTabsToSpaces in Options)) and (CharCount >= TabWidth) then
    Result := StringOfChar(#9, CharCount div TabWidth) + StringOfChar(#32, CharCount mod TabWidth)
  else
    Result := StringOfChar(#32, CharCount);
end;

procedure TCustomSynEdit.LinesChanging(Sender: TObject);
begin
  Include(fStateFlags, sfLinesChanging);
end;

procedure TCustomSynEdit.LinesChanged(Sender: TObject);
var
  vOldMode: TSynSelectionMode;
begin
  Exclude(fStateFlags, sfLinesChanging);
  if fUseCodeFolding then
    ReScan;
  if HandleAllocated then begin
    UpdateScrollBars;
    vOldMode := fActiveSelectionMode;
    SetBlockBegin(CaretXY);
    fActiveSelectionMode := vOldMode;
    InvalidateRect(fInvalidateRect, False);
    FillChar(fInvalidateRect, SizeOf(TRect), 0);
    if fGutter.ShowLineNumbers and fGutter.AutoSize then
      fGutter.AutoSizeDigitCount(Lines.Count);
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  bWasSel: Boolean;
  bStartDrag: Boolean;
  TmpBegin, TmpEnd: TBufferCoord;
begin
  bWasSel := False;
  bStartDrag := False;
  fMouseMoved := False;
{
  if (X < fGutterWidth + 2) then begin
    DoOnGutterClick(Button, X, Y);
    Exit; // do NOT wait for MouseUp
  end;
}
  TmpBegin := FBlockBegin;
  TmpEnd := FBlockEnd;

  if Button = mbLeft then begin
    if SelAvail then begin
      //remember selection state, as it will be cleared later
      bWasSel := True;
      fMouseDownX := X;
      fMouseDownY := Y;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) and (ssDouble in Shift) then
    Exit;

  fKbdHandler.ExecuteMouseDown(Self, Button, Shift, X, Y);

  if (Button in [mbLeft, mbRight]) then begin
    if Button = mbRight then begin
      if (eoRightMouseMovesCursor in Options) and
        (SelAvail and not IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y)))
        or not SelAvail) then begin
        InvalidateSelection;
        FBlockEnd := FBlockBegin;
        ComputeCaret(X, Y);
      end else
        Exit;
    end else
      ComputeCaret(X, Y);
  end;

  if Button = mbLeft then begin
    //I couldn't track down why, but sometimes (and definitely not all the time)
    //the block positioning is lost.  This makes sure that the block is
    //maintained in case they started a drag operation on the block
    FBlockBegin := TmpBegin;
    FBlockEnd := TmpEnd;

    MouseCapture := True;
    //if mousedown occurred in selected block begin drag operation
    Exclude(fStateFlags, sfWaitForDragging);
    if bWasSel and (eoDragDropEditing in fOptions) and (X >= fGutterWidth + 2)
      and (SelectionMode = smNormal) and IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y))) then begin
      bStartDrag := True
    end;
  end;

  if (Button = mbLeft) and bStartDrag then
    Include(fStateFlags, sfWaitForDragging)
  else begin
    if not (sfDblClicked in fStateFlags) then begin
      if ssShift in Shift then
        //BlockBegin and BlockEnd are restored to their original position in the
        //code from above and SetBlockEnd will take care of proper invalidation
        SetBlockEnd(CaretXY)
      else begin
        if (eoAltSetsColumnMode in Options) and (fActiveSelectionMode <> smLine) then begin
          if ssAlt in Shift then
            SelectionMode := smColumn
          else
            SelectionMode := smNormal;
        end;
        //Selection mode must be set before calling SetBlockBegin
        SetBlockBegin(CaretXY);
      end;
    end;
  end;

  SetFocus;
  Windows.SetFocus(Handle);
end;

procedure TCustomSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TDisplayCoord;
begin
  inherited MouseMove(Shift, x, y);
  fMouseMoved := True;
  if MouseCapture and (sfWaitForDragging in fStateFlags) then begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
      or (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG)) then begin
      Exclude(fStateFlags, sfWaitForDragging);
      BeginDrag(false);
    end;
  end else if (ssLeft in Shift) and MouseCapture and (X > fGutter.Width) then begin
    // should we begin scrolling?
    ComputeScroll(X, Y);
    { compute new caret }
    P := PixelsToNearestRowColumn(X, Y);
    P.Row := MinMax(P.Row, 1, DisplayLineCount);
    if fScrollDeltaX <> 0 then
      P.Column := DisplayX;
    if fScrollDeltaY <> 0 then
      P.Row := DisplayY;
    InternalCaretXY := DisplayToBufferPos(P);
    BlockEnd := CaretXY;
  end;
end;

procedure TCustomSynEdit.ScrollTimerHandler(Sender: TObject);
var
  iMousePos: TPoint;
  C: TDisplayCoord;
  X, Y: Integer;
  vCaret: TBufferCoord;
begin
  GetCursorPos(iMousePos);
  iMousePos := ScreenToClient(iMousePos);
  C := PixelsToRowColumn(iMousePos.X, iMousePos.Y);
  C.Row := MinMax(C.Row, 1, DisplayLineCount);
  if fScrollDeltaX <> 0 then begin
    LeftChar := LeftChar + fScrollDeltaX;
    X := LeftChar;
    if fScrollDeltaX > 0 then // scrolling right?
      Inc(X, CharsInWindow);
    C.Column := X;
  end;
  if fScrollDeltaY <> 0 then begin
    if GetKeyState(SYNEDIT_SHIFT) < 0 then
      TopLine := TopLine + fScrollDeltaY * LinesInWindow
    else
      TopLine := TopLine + fScrollDeltaY;
    Y := TopLine;
    if fScrollDeltaY > 0 then // scrolling down?
      Inc(Y, LinesInWindow - 1);
    C.Row := MinMax(Y, 1, DisplayLineCount);
  end;
  vCaret := DisplayToBufferPos(C);
  if (CaretX <> vCaret.Char) or (CaretY <> vCaret.Line) then begin
    // changes to line / column in one go
    IncPaintLock;
    try
      InternalCaretXY := vCaret;
      // if MouseCapture is True we're changing selection. otherwise we're dragging
      if MouseCapture then
        SetBlockEnd(CaretXY);
    finally
      DecPaintLock;
    end;
  end;
  ComputeScroll(iMousePos.x, iMousePos.y);
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  fKbdHandler.ExecuteMouseUp(Self, Button, Shift, X, Y);

  if not fMouseMoved and (X < fGutterWidth + 2) then begin
    DoOnGutterClick(Button, X, Y);
  end;

  fScrollTimer.Enabled := False;
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
    exit;
  MouseCapture := False;
  if fStateFlags * [sfDblClicked, sfWaitForDragging] = [sfWaitForDragging] then begin
    ComputeCaret(X, Y);
    if not (ssShift in Shift) then
      SetBlockBegin(CaretXY);
    SetBlockEnd(CaretXY);
    Exclude(fStateFlags, sfWaitForDragging);
  end;
  Exclude(fStateFlags, sfDblClicked);
end;

procedure TCustomSynEdit.DoOnGutterClick(Button: TMouseButton; X, Y: integer);
var
  i: integer;
  offs: integer;
  allmrk: TSynEditLineMarks;
  mark: TSynEditMark;
  FoldRange: TSynEditFoldRange;
  rect: TRect;
  RowColumn: TDisplayCoord;
  Line: integer;
begin
  RowColumn := PixelsToRowColumn(X, Y);
  Line := RowToLine(RowColumn.Row);

  // Check if we clicked on a folding thing
  if fUseCodeFolding then begin
    FoldRange := FoldStartAtLine(Line);
    if Assigned(FoldRange) then begin

      // See if we actually clicked on the rectangle...
      //rect.Left := Gutter.RealGutterWidth(CharWidth) - Gutter.RightOffset;
      rect.Left := fGutterWidth - Gutter.RightOffset;
      rect.Right := rect.Left + Gutter.RightOffset - 4;
      rect.Top := (RowColumn.Row - fTopLine) * LineHeight;
      rect.Bottom := rect.Top + LineHeight;

      if PtInRect(rect, Point(X, Y)) then begin
        if FoldRange.Collapsed then
          Uncollapse(FoldRange)
        else
          Collapse(FoldRange);
        Exit;
      end;
    end;
  end;

  // If not, check gutter marks
  if Assigned(fOnGutterClick) then begin
    if line <= Lines.Count then begin
      Marks.GetMarksForLine(Line, allmrk);
      offs := 0;
      mark := nil;
      for i := Low(allmrk) to High(allmrk) do begin
        if assigned(allmrk[i]) then begin
          Inc(offs, BookMarkOptions.XOffset);
          if X < offs then begin
            mark := allmrk[i];
            break;
          end;
        end;
      end; //for
      fOnGutterClick(Self, Button, X, Y, Line, mark);
    end;
  end;
end;

procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2, nC1, nC2: integer;
begin
  if fPainterLock>0 then
    Exit;
  if fPainting then
    Exit;
  fPainting:=True;
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  rcClip := Canvas.ClipRect;
  // columns
  nC1 := LeftChar;
  if (rcClip.Left > fGutterWidth + 2) then
    Inc(nC1, (rcClip.Left - fGutterWidth - 2) div CharWidth);
  nC2 := LeftChar +
    (rcClip.Right - fGutterWidth - 2 + CharWidth - 1) div CharWidth;
  // lines
  nL1 := MinMax(TopLine + rcClip.Top div fTextHeight, TopLine, DisplayLineCount);
  nL2 := MinMax(TopLine + (rcClip.Bottom + fTextHeight - 1) div fTextHeight, 1, DisplayLineCount);

  // Now paint everything while the caret is hidden.
  HideCaret;
  try

    // First paint paint the text area if it was (partly) invalidated.
    if (rcClip.Right > fGutterWidth) then begin
      rcDraw := rcClip;
      rcDraw.Left := Max(rcDraw.Left, fGutterWidth);
      PaintTextLines(rcDraw, nL1, nL2, nC1, nC2);
    end;

    // Then the gutter area if it was (partly) invalidated. 
    if (rcClip.Left < fGutterWidth) then begin
      rcDraw := rcClip;
      rcDraw.Right := fGutterWidth;
      PaintGutter(rcDraw, nL1, nL2);
    end;

    PluginsAfterPaint(Canvas, rcClip, nL1, nL2);
    // If there is a custom paint handler call it.
    DoOnPaint;
    DoOnPaintTransient(ttAfter);

  finally
    UpdateCaret;
    fPainting:=False;
  end;
end;

procedure TCustomSynEdit.PaintGutter(const AClip: TRect;
  const aFirstRow, aLastRow: integer);

  procedure DrawMark(aMark: TSynEditMark; var aGutterOff: integer;
    aMarkRow: integer);
  begin
    if (not aMark.InternalImage) and Assigned(fBookMarkOpt.BookmarkImages) then begin
      if aMark.ImageIndex <= fBookMarkOpt.BookmarkImages.Count then begin
        if aMark.IsBookmark = BookMarkOptions.DrawBookmarksFirst then
          aGutterOff := 0
        else if aGutterOff = 0 then
          aGutterOff := fBookMarkOpt.XOffset;
        with fBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOff,
            (aMarkRow - TopLine) * fTextHeight, aMark.ImageIndex);
        Inc(aGutterOff, fBookMarkOpt.XOffset);
      end;
    end else begin
      if aMark.ImageIndex in [0..9] then begin
        if not Assigned(fInternalImage) then begin
          fInternalImage := TSynInternalImage.Create(HINSTANCE,
            'SynEditInternalImages', 10);
        end;
        if aGutterOff = 0 then begin
          fInternalImage.Draw(Canvas, aMark.ImageIndex,
            fBookMarkOpt.LeftMargin + aGutterOff,
            (aMarkRow - TopLine) * fTextHeight, fTextHeight);
        end;
        Inc(aGutterOff, fBookMarkOpt.XOffset);
      end;
    end;
  end;

var
  cRow: integer;
  cMark: integer;
  rcLine, rcFold: TRect;
  aGutterOffs: PIntArray;
  bHasOtherMarks: boolean;
  s: string;
  vFirstLine: integer;
  vLastLine: integer;
  vLine: integer;
  vMarkRow: integer;
  vGutterRow: integer;
  vLineTop: integer;
  dc: HDC;
  TextSize: TSize;
  x: Integer;
  FoldRange: TSynEditFoldRange;
begin
  //todo: Does the following comment still apply?
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
  // If we have to draw the line numbers then we don't want to erase
  // the background first. Do it line by line with TextRect instead
  // and fill only the area after the last visible line.
  dc := Canvas.Handle;

  Canvas.Brush.Color := fGutter.Color;

  if fGutter.ShowLineNumbers then begin
    if fGutter.UseFontStyle then
      fTextDrawer.SetBaseFont(fGutter.Font)
    else
      fTextDrawer.Style := [];
    fTextDrawer.BeginDrawing(dc);
    try
      if fGutter.UseFontStyle then
        fTextDrawer.SetForeColor(fGutter.Font.Color)
      else
        fTextDrawer.SetForeColor(Self.Font.Color);
      fTextDrawer.SetBackColor(fGutter.Color);

      // prepare the rect initially
      rcLine := AClip;
      rcLine.Right := Max(rcLine.Right, fGutterWidth - 2);
      rcLine.Bottom := rcLine.Top;

      // draw each line if it is not hidden by a fold
      for cRow := aFirstRow to aLastRow do begin
        vLine := RowToLine(cRow);
        if (vLine > Lines.Count) and not (Lines.Count = 0) then
          break;
        vLineTop := (cRow - TopLine) * fTextHeight;

        // next line rect
        rcLine.Top := vLineTop;
        rcLine.Bottom := rcLine.Top + fTextHeight;

        s := fGutter.FormatLineNumber(vLine);

        if Assigned(OnGutterGetText) then
          OnGutterGetText(Self, vLine, s);
        GetTextExtentPoint32(DC, PChar(s), Length(s), TextSize);
        if fGutter.Gradient then begin
          SetBkMode(DC, TRANSPARENT);
          Windows.ExtTextOut(DC, (fGutterWidth - fGutter.RightOffset - 2) - TextSize.cx,
            rcLine.Top + ((fTextHeight - Integer(TextSize.cy)) div 2), 0,
            @rcLine, PChar(s), Length(s), nil);
          SetBkMode(DC, OPAQUE);
        end else
          Windows.ExtTextOut(DC, (fGutterWidth - fGutter.RightOffset - 2) - TextSize.cx,
            rcLine.Top + ((fTextHeight - integer(TextSize.cy)) div 2), ETO_OPAQUE,
            @rcLine, PChar(s), Length(s), nil);
      end;

      // now erase the remaining area if any
      if (AClip.Bottom > rcLine.Bottom) and not fGutter.Gradient then begin
        rcLine.Top := rcLine.Bottom;
        rcLine.Bottom := AClip.Bottom;
        with rcLine do
          fTextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, rcLine, nil, 0);
      end;
    finally
      fTextDrawer.EndDrawing;
      if fGutter.UseFontStyle then
        fTextDrawer.SetBaseFont(Self.Font);
    end;
  end else if not fGutter.Gradient then
    Canvas.FillRect(AClip);

  // draw word wrap glyphs transparently over gradient
  if fGutter.Gradient then
    Canvas.Brush.Style := bsClear;

  // restore brush
  if fGutter.Gradient then
    Canvas.Brush.Style := bsSolid;

  // Draw the folding lines and squares
  if fUseCodeFolding then begin
    for cRow := aFirstRow to aLastRow do begin
      vLine := RowToLine(cRow);
      if (vLine > Lines.Count) and not (Lines.Count = 0) then
        break;

      // Form a rectangle for the square the user can click on
      //rcFold.Left := Gutter.RealGutterWidth(CharWidth) - Gutter.RightOffset;
      rcFold.Left := fGutterWidth - Gutter.RightOffset;
      rcFold.Right := rcFold.Left + Gutter.RightOffset - 4;
      rcFold.Top := (cRow - fTopLine) * LineHeight;
      rcFold.Bottom := rcFold.Top + LineHeight;

      Canvas.Pen.Color := fCodeFolding.FolderBarLinesColor;

      // Need to paint a line?
      if Assigned(FoldAroundLine(vLine)) then begin
        x := rcFold.Left + ((rcFold.Right - rcFold.Left) div 2);
        Canvas.MoveTo(x, rcFold.Top);
        Canvas.LineTo(x, rcFold.Bottom);
      end;

      // Need to paint a line end?
      if Assigned(FoldEndAtLine(vLine)) then begin
        x := rcFold.Left + ((rcFold.Right - rcFold.Left) div 2);
        Canvas.MoveTo(x, rcFold.Top);
        Canvas.LineTo(x, rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));
        Canvas.LineTo(rcFold.Right - 2, rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));
      end;

      // Any fold ranges beginning on this line?
      FoldRange := FoldStartAtLine(vLine);
      if Assigned(FoldRange) then begin

        // Draw the bottom part of a line
        if not FoldRange.Collapsed then begin
          x := rcFold.Left + ((rcFold.Right - rcFold.Left) div 2);
          Canvas.MoveTo(x, (rcFold.Top + rcFold.Bottom) div 2);
          Canvas.LineTo(x, rcFold.Bottom);
        end;

        // make a square rect
        InflateRect(rcFold, -2, -0);
        rcFold.Top := rcFold.Top + ((LineHeight - (rcFold.Right - rcFold.Left)) div 2);
        rcFold.Bottom := rcFold.Top + (rcFold.Right - rcFold.Left);

        // Paint the square the user can click on
        if assigned(self.Gutter) then
          Canvas.Brush.Color := Gutter.Color
        else
          Canvas.Brush.Color := clWindow;
        Canvas.FillRect(rcFold);
        Canvas.Brush.Color := fCodeFolding.FolderBarLinesColor;
        Canvas.FrameRect(rcFold);

        // Paint minus sign
        Canvas.Pen.Color := fCodeFolding.FolderBarLinesColor;
        Canvas.MoveTo(rcFold.Left + 2, rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));
        Canvas.LineTo(rcFold.Right - 2, rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));

        // Paint vertical line of plus sign
        if FoldRange.Collapsed then begin
          x := rcFold.Left + ((rcFold.Right - rcFold.Left) div 2);
          Canvas.MoveTo(x, rcFold.Top + 2);
          Canvas.LineTo(x, rcFold.Bottom - 2);
        end;
      end;
    end;
  end;

  // the gutter separator if visible
  if (fGutter.BorderStyle <> gbsNone) and (AClip.Right >= fGutterWidth - 2) then
    with Canvas do begin
      Pen.Color := fGutter.BorderColor;
      Pen.Width := 1;
      with AClip do begin
        if fGutter.BorderStyle = gbsMiddle then begin
          MoveTo(fGutterWidth - 2, Top);
          LineTo(fGutterWidth - 2, Bottom);
          Pen.Color := fGutter.Color;
        end;
        MoveTo(fGutterWidth - 1, Top);
        LineTo(fGutterWidth - 1, Bottom);
      end;
    end;

  // now the gutter marks
  if BookMarkOptions.GlyphsVisible and (Marks.Count > 0) and (aLastRow >= aFirstRow) then begin
    aGutterOffs := AllocMem((aLastRow - aFirstRow + 1) * SizeOf(integer));
    vFirstLine := RowToLine(aFirstRow);
    vLastLine := RowToLine(aLastRow);
    try
      // Instead of making a two pass loop we look while drawing the bookmarks
      // whether there is any other mark to be drawn
      bHasOtherMarks := FALSE;
      for cMark := 0 to Marks.Count - 1 do
        with Marks[cMark] do
          if Visible and (Line >= vFirstLine) and (Line <= vLastLine) then begin
            if IsBookmark <> BookMarkOptions.DrawBookmarksFirst then
              bHasOtherMarks := TRUE
            else begin
              vMarkRow := LineToRow(Line);
              if vMarkRow >= aFirstRow then
                DrawMark(Marks[cMark], aGutterOffs[vMarkRow - aFirstRow], vMarkRow);
            end
          end;
      if bHasOtherMarks then
        for cMark := 0 to Marks.Count - 1 do
          with Marks[cMark] do begin
            if Visible and (IsBookmark <> BookMarkOptions.DrawBookmarksFirst)
              and (Line >= vFirstLine) and (Line <= vLastLine) then begin
              vMarkRow := LineToRow(Line);
              if vMarkRow >= aFirstRow then
                DrawMark(Marks[cMark], aGutterOffs[vMarkRow - aFirstRow], vMarkRow);
            end;
          end;
      if Assigned(OnGutterPaint) then
        for cRow := aFirstRow to aLastRow do begin
          OnGutterPaint(Self, cRow, aGutterOffs[cRow - aFirstRow],
            (vGutterRow - TopLine) * LineHeight);
        end;
    finally
      FreeMem(aGutterOffs);
    end;
  end else if Assigned(OnGutterPaint) then begin
    for cRow := aFirstRow to aLastRow do begin
      OnGutterPaint(Self, cRow, 0, (cRow - TopLine) * LineHeight);
    end;
  end;
end;

procedure TCustomSynEdit.PaintTextLines(AClip: TRect; const aFirstRow, aLastRow,
  FirstCol, LastCol: integer);
var
  bDoRightEdge: boolean; // right edge
  nRightEdge: integer;
  // selection info
  bAnySelection: boolean; // any selection visible?
  vSelStart: TDisplayCoord; // start of selected area
  vSelEnd: TDisplayCoord; // end of selected area
  // info about normal and selected text and background colors
  bSpecialLine, bLineSelected, bCurrentLine: boolean;
  colFG, colBG: TColor;
  colSelFG, colSelBG: TColor;
  // info about selection of the current line
  nLineSelStart, nLineSelEnd: integer;
  bComplexLine: boolean;
  // painting the background and the text
  rcLine, rcToken: TRect;
  TokenAccu: record
    // Note: s is not managed as a string, it will only grow!!!
    // Never use AppendStr or "+", use Len and MaxLen instead and
    // copy the string chars directly. This is for efficiency.
    Len, MaxLen, CharsBefore: integer;
    s: string;
    FG, BG: TColor;
    Style: TFontStyles;
  end;
  dc: HDC;
  SynTabGlyphString: string;

  vFirstLine: integer;
  vLastLine: integer;

  { local procedures }

  function colEditorBG: TColor;
  var
    iAttri: TSynHighlighterAttributes;
  begin
    if (ActiveLineColor <> clNone) and (bCurrentLine) then
      Result := ActiveLineColor
    else begin
      Result := Color;
      if Highlighter <> nil then begin
        iAttri := Highlighter.WhitespaceAttribute;
        if (iAttri <> nil) and (iAttri.Background <> clNone) then
          Result := iAttri.Background;
      end;
    end;
  end;

  procedure ComputeSelectionInfo;
  var
    vStart: TBufferCoord;
    vEnd: TBufferCoord;
  begin
    bAnySelection := FALSE;
    // Only if selection is visible anyway.
    if (not HideSelection or Self.Focused) then begin
      bAnySelection := TRUE;
      // Get the *real* start of the selected area.
      if (fBlockBegin.Line < fBlockEnd.Line) then begin
        vStart := fBlockBegin;
        vEnd := fBlockEnd;
      end else if (fBlockBegin.Line > fBlockEnd.Line) then begin
        vEnd := fBlockBegin;
        vStart := fBlockEnd;
      end else if (fBlockBegin.Char <> fBlockEnd.Char) then begin
        // No selection at all, or it is only on this line.
        vStart.Line := fBlockBegin.Line;
        vEnd.Line := vStart.Line;
        if (fBlockBegin.Char < fBlockEnd.Char) then begin
          vStart.Char := fBlockBegin.Char;
          vEnd.Char := fBlockEnd.Char;
        end else begin
          vStart.Char := fBlockEnd.Char;
          vEnd.Char := fBlockBegin.Char;
        end;
      end else
        bAnySelection := FALSE;
      // If there is any visible selection so far, then test if there is an
      // intersection with the area to be painted.
      if bAnySelection then begin
        // Don't care if the selection is not visible.
        bAnySelection := (vEnd.Line >= vFirstLine) and (vStart.Line <= vLastLine);
        if bAnySelection then begin
          // Transform the selection from text space into screen space
          vSelStart := BufferToDisplayPos(vStart);
          vSelEnd := BufferToDisplayPos(vEnd);
          // In the column selection mode sort the begin and end of the selection,
          // this makes the painting code simpler.
          if (fActiveSelectionMode = smColumn) and (vSelStart.Column > vSelEnd.Column) then
            SwapInt(vSelStart.Column, vSelEnd.Column);
        end;
      end;
    end;
  end;

  procedure SetDrawingColors(Selected: boolean);
  begin
    with fTextDrawer do
      if Selected then begin
        SetBackColor(colSelBG);
        SetForeColor(colSelFG);
        Canvas.Brush.Color := colSelBG;
      end else begin
        SetBackColor(colBG);
        SetForeColor(colFG);
        Canvas.Brush.Color := colBG;
      end;
  end;

  function ColumnToXValue(Col: integer): integer;
  begin
    Result := fTextOffset + Pred(Col) * fCharWidth;
  end;

  //todo: Review SpecialChars and HardTabs painting. Token parameter of PaintToken procedure could very probably be passed by reference.

  // Note: The PaintToken procedure will take care of invalid parameters
  // like empty token rect or invalid indices into TokenLen.
  // CharsBefore tells if Token starts at column one or not
  procedure PaintToken(Token: string;
    TokenLen, CharsBefore, First, Last: integer; isSelection: boolean);
  var
    pszText: PChar;
    Counter, nX, nCharsToPaint, nX1: integer;
    sTabbedToken: string;
    newToken:String;
    DoTabPainting: Boolean;
    i, TabStart, TabLen: Integer;
    rcTab: TRect;
    isSpace : boolean;
  const
    ETOOptions = ETO_OPAQUE;
  begin
    sTabbedToken := Token;
    DoTabPainting := False;

    Counter := Last - CharsBefore;
    newToken := '';

    while Counter > First - CharsBefore - 1 do begin
      if (Length(Token) >= Counter) then begin
        if (fShowSpecChar) and (not bSpecialLine) and (not isSelection) and (Token[Counter] = #32) then
          newToken := SynSpaceGlyph + newToken
        else if (Token[Counter] = TSynTabChar) then begin
          newToken := #32 + newToken;
          DoTabPainting := fShowSpecChar and (not isSelection) and (not bSpecialLine);
        end else begin
          newToken := Token[Counter] + newToken ;
        end;
      end;
      Dec(Counter);
    end;

    if (Last >= First) and (rcToken.Right > rcToken.Left) then begin
      nX := ColumnToXValue(First);
      Dec(First, CharsBefore);
      Dec(Last, CharsBefore);
      if (First > TokenLen) then begin
        pszText := nil;
        nCharsToPaint := 0;
      end else begin
{$IFDEF SYN_MBCSSUPPORT}
        if (First > 1) and (ByteType(Token, First) = mbTrailByte) then begin
          Dec(First);
          Dec(nX, fCharWidth);
          newToken := Token[First]+newToken;
        end;
{$ENDIF}
        pszText := PChar(newToken);
        nCharsToPaint := Length(newToken);
      end;
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, rcToken,
        pszText, nCharsToPaint);

      if DoTabPainting then begin
        //fix everything before the FirstChar
        for i := 1 to First - 1 do //wipe the text out so we don't
          if sTabbedToken[i] = TSynTabChar then //count it out of the range
            sTabbedToken[i] := #32; //we're looking for

        TabStart := pos(TSynTabChar, sTabbedToken);
        rcTab.Top := rcToken.Top;
        rcTab.Bottom := rcToken.Bottom;
        while (TabStart > 0) and (TabStart >= First) and (TabStart <= Last) do begin
          TabLen := 1;
          while (TabStart + CharsBefore + TabLen - 1) mod FTabWidth <> 0 do
            inc(TabLen);
          pszText := PChar(@SynTabGlyphString[1]);

          nX1 := ColumnToXValue(CharsBefore + TabStart + (TabLen div 2) - 1);
          if TabLen mod 2 = 0 then
            nX1 := nX1 + (fCharWidth div 2)
          else
            nX1 := nX1 + fCharWidth;

          rcTab.Left := nX1;
          rcTab.Right := nX1 + fTextDrawer.GetCharWidth;

          fTextDrawer.ExtTextOut(nX1, rcTab.Top, ETOOptions or ETO_CLIPPED, rcTab,
            pszText, length(SynTabGlyphString));

          for i := 0 to TabLen - 1 do //wipe the text out so we don't
            sTabbedToken[TabStart + i] := #32; //count it again

          TabStart := pos(TSynTabChar, sTabbedToken);
        end;
      end;

      rcToken.Left := rcToken.Right;
    end;
  end;

  //todo: eliminate AdjustEndRect
  procedure AdjustEndRect;
    { trick to avoid clipping the last pixels of text in italic }
  var
    iLastChar: cardinal;
    iCharWidth: integer;
    iCharInfo: TABC;
  begin
    iLastChar := Ord(TokenAccu.s[TokenAccu.Len]);
    if GetCharABCWidths(dc, iLastChar, iLastChar, iCharInfo) then begin
      iCharWidth := iCharInfo.abcA + integer(iCharInfo.abcB);
      if iCharInfo.abcC >= 0 then
        Inc(iCharWidth, iCharInfo.abcC);
    end else
      GetCharWidth(dc, iLastChar, iLastChar, iCharWidth);
    Dec(iCharWidth, CharWidth);
    if iCharWidth > 0 then
      Inc(rcToken.Left, iCharWidth);
  end;

  procedure PaintEditAreas(areaList:TList; colBorder:TColor;areaType:TEditingAreaType);
  var
    rc:TRect;
    i,t:integer;
    p:PEditingArea;
    x1,x2:integer;
    offset:integer;
  begin
    rc:=rcLine;
    dec(rc.bottom);
    SetDrawingColors(False);
    for i:=0 to areaList.Count-1 do begin
      p:=PEditingArea(areaList[i]);
      if p.beginX > LastCol then
        continue;
      if p.endX < FirstCol then
        continue;
      if p.beginX<FirstCol then
        x1:=FirstCol
      else
        x1:=p.beginX;
      if p.endX>LastCol then
        x2:=LastCol
      else
        x2:=p.endX;
      rc.Left := ColumnToXValue(x1);
      rc.Right := ColumnToXValue(x2);
      Canvas.Pen.Color := p.color;
      case areaType of
        eatEditing: begin
          canvas.MoveTo(rc.Left,rc.Top);
          canvas.LineTo(rc.Right,rc.Top);
          canvas.LineTo(rc.Right,rc.bottom);
          canvas.LineTo(rc.Left,rc.bottom);
          canvas.LineTo(rc.Left,rc.Top);
        end;
        eatError: begin
          offset:=3;
          canvas.MoveTo(rc.Left,rc.Bottom-offset);
          t:=rc.left;
          inc(t,3);
          while t<=rc.Right do begin
            if t>rc.Right then
              t:=rc.Right;
            offset := 3 - offset;
            canvas.LineTo(t,rc.Bottom-offset);
            inc(t,3);
          end;
        end;
      end;
    end;
  end;

  procedure PaintHighlightToken(bFillToEOL: boolean);
  var
    bComplexToken: boolean;
    nC1, nC2, nC1Sel, nC2Sel: integer;
    bU1, bSel, bU2: boolean;
    nX1, nX2: integer;
  begin
    // Compute some helper variables.
    nC1 := Max(FirstCol, TokenAccu.CharsBefore + 1);
    nC2 := Min(LastCol, TokenAccu.CharsBefore + TokenAccu.Len + 1);
    if bComplexLine then begin
      bU1 := (nC1 < nLineSelStart);
      bSel := (nC1 < nLineSelEnd) and (nC2 >= nLineSelStart);
      bU2 := (nC2 >= nLineSelEnd);
      bComplexToken := bSel and (bU1 or bU2);
    end else begin
      bSel := bLineSelected;
      bComplexToken := FALSE;
      bU1 := False; // to shut up compiler warning.
      bU2 := False; // to shut up compiler warning.
    end;
    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then begin
      // Initialize the colors and the font style.
      if not bSpecialLine then begin
        colBG := TokenAccu.BG;
        colFG := TokenAccu.FG;
      end;

      if bSpecialLine and (eoSpecialLineDefaultFg in fOptions) then
        colFG := TokenAccu.FG;

      fTextDrawer.SetStyle(TokenAccu.Style);
      // Paint the chars
      if bComplexToken then begin
        // first unselected part of the token
        if bU1 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ColumnToXValue(nLineSelStart);
          with TokenAccu do
            PaintToken(s, Len, CharsBefore, nC1, nLineSelStart,False);
        end;
        // selected part of the token
        SetDrawingColors(TRUE);
        nC1Sel := Max(nLineSelStart, nC1);
        nC2Sel := Min(nLineSelEnd, nC2);
        rcToken.Right := ColumnToXValue(nC2Sel);
        with TokenAccu do
          PaintToken(s, Len, CharsBefore, nC1Sel, nC2Sel,True);
        // second unselected part of the token
        if bU2 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ColumnToXValue(nC2);
          with TokenAccu do
            PaintToken(s, Len, CharsBefore, nLineSelEnd, nC2,False);
        end;
      end else begin
        SetDrawingColors(bSel);
        rcToken.Right := ColumnToXValue(nC2);
        with TokenAccu do
          PaintToken(s, Len, CharsBefore, nC1, nC2,bSel);
      end;
    end;

    // Fill the background to the end of this line if necessary.
    if bFillToEOL and (rcToken.Left < rcLine.Right) then begin
      if not bSpecialLine then
        colBG := colEditorBG;
      if bComplexLine then begin
        nX1 := ColumnToXValue(nLineSelStart);
        nX2 := ColumnToXValue(nLineSelEnd);
        if (rcToken.Left < nX1) then begin
          SetDrawingColors(FALSE);
          rcToken.Right := nX1;
          if (TokenAccu.Len <> 0) and (TokenAccu.Style <> []) then
            AdjustEndRect;
          Canvas.FillRect(rcToken);
          rcToken.Left := nX1;
        end;
        if (rcToken.Left < nX2) then begin
          SetDrawingColors(TRUE);
          rcToken.Right := nX2;
          Canvas.FillRect(rcToken);
          rcToken.Left := nX2;
        end;
        if (rcToken.Left < rcLine.Right) then begin
          SetDrawingColors(FALSE);
          rcToken.Right := rcLine.Right;
          Canvas.FillRect(rcToken);
        end;
      end else begin
        SetDrawingColors(bLineSelected);
        rcToken.Right := rcLine.Right;
        if (TokenAccu.Len <> 0) and (TokenAccu.Style <> []) then
          AdjustEndRect;
        Canvas.FillRect(rcToken);
      end;
    end;
  end;

  // Store the token chars with the attributes in the TokenAccu
  // record. This will paint any chars already stored if there is
  // a (visible) change in the attributes.
  procedure AddHighlightToken(const Token: AnsiString;
    CharsBefore, TokenLen: integer; cRow:integer; p_Attri: TSynHighlighterAttributes);
  var
    bSpacesTest, bIsSpaces: boolean;

    function TokenIsSpaces: boolean;
    var
      pTok: PChar;
    begin
      if not bSpacesTest then begin
        bSpacesTest := TRUE;
        pTok := PChar(Token);
        while pTok^ = #32 do {//todo: should include tabs?} begin
          Inc(pTok);
        end;
        bIsSpaces := (pTok^ = #0) and (pTok - PChar(Token) = Length(Token));
      end;
      Result := bIsSpaces;
    end;

  var
    bCanAppend: boolean;
    i: integer;
    Foreground, Background: TColor;
    Style: TFontStyles;
  begin
    if Assigned(p_Attri) then begin
      Foreground := p_Attri.Foreground;
      Background := p_Attri.Background;
      Style := p_Attri.Style;
    end else begin
      Foreground := colFG;
      Background := colBG;
      Style := Font.Style;
    end;

    if (Background = clNone) or
      ((ActiveLineColor <> clNone) and (bCurrentLine)) then begin
      Background := colEditorBG;
    end;
    if Foreground = clNone then
      Foreground := Font.Color;

    if Assigned(OnPaintHighlightToken) then
      OnPaintHighlightToken(self,cRow,fHighlighter.GetTokenPos,
        token,p_Attri,Style,Foreground,Background);

{$IFDEF SYN_MBCSSUPPORT}
    if (byteType(Token,tokenlen) = mbLeadByte) and (tokenlen < length(token)) then begin
      inc(tokenlen);
    end;
{$ENDIF}
    // Do we have to paint the old chars first, or can we just append?
    bCanAppend := FALSE;
    bSpacesTest := FALSE;
    if (TokenAccu.Len > 0) then begin
      // font style must be the same or token is only spaces
      if (TokenAccu.Style = Style)
        or (not (fsUnderline in Style) and not (fsUnderline in TokenAccu.Style)
        and TokenIsSpaces) then begin
        // either special colors or same colors
        if (bSpecialLine and not (eoSpecialLineDefaultFg in fOptions)) or bLineSelected or
          // background color must be the same and
        ((TokenAccu.BG = Background) and
          // foreground color must be the same or token is only spaces
          ((TokenAccu.FG = Foreground) or (TokenIsSpaces and not fShowSpecChar))) then begin
          bCanAppend := TRUE;
        end;
      end;
      // If we can't append it, then we have to paint the old token chars first.
      if not bCanAppend then
        PaintHighlightToken(FALSE);
    end;
    // Don't use AppendStr because it's more expensive.
    if bCanAppend then begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do
        TokenAccu.s[TokenAccu.Len + i] := Token[i];
      Inc(TokenAccu.Len, TokenLen);
    end else begin
      TokenAccu.Len := TokenLen;
      if (TokenAccu.Len > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do
        TokenAccu.s[i] := Token[i];
      TokenAccu.CharsBefore := CharsBefore;
      TokenAccu.FG := Foreground;
      TokenAccu.BG := Background;
      TokenAccu.Style := Style;
    end;
  end;

  procedure PaintFoldAttributes;
  var
    i, TabSteps, LineIndent, LastNonBlank, X, Y, cRow, vLine: integer;
    DottedPen, OldPen: HPEN;
    DottedPenDesc: LOGBRUSH;
  begin
    // Paint indent guides. Use folds to determine indent value of these
    // Use a separate loop so we can use a custom pen
    // Paint indent guides using custom pen
    if fCodeFolding.IndentGuides then begin
      DottedPenDesc.lbStyle := BS_SOLID;
      DottedPenDesc.lbColor := fCodeFolding.IndentGuidesColor;
      DottedPen := ExtCreatePen(PS_COSMETIC or PS_ALTERNATE, 1, DottedPenDesc, 0, nil);
      try
        OldPen := SelectObject(Canvas.Handle, DottedPen);

        // Now loop through all the lines. The indices are valid for Lines.
        for cRow := aFirstRow to aLastRow do begin
          vLine := RowToLine(cRow);
          if (vLine > Lines.Count) and not (Lines.Count = 0) then
            break;

          // Set vertical coord
          Y := (vLine - TopLine) * fTextHeight; // limit inside clip rect
          if (fTextHeight mod 2 = 1) and (vLine mod 2 = 0) then // even
            Inc(Y);

          // Get next nonblank line
          LastNonBlank := vLine - 1;
          while (LastNonBlank + 1 < fLines.Count) and (TrimLeft(fLines[LastNonBlank]) = '') do
            Inc(LastNonBlank);
          LineIndent := GetLineIndent(fLines[LastNonBlank]);

          // Step horizontal coord
          TabSteps := TabWidth;
          while TabSteps < LineIndent do begin
            X := TabSteps * CharWidth + fTextOffset - 2;
            Inc(TabSteps, TabWidth);

            // Move to top of vertical line
            Canvas.MoveTo(X, Y);
            Inc(Y, fTextHeight);

            // Draw down and move back up
            Canvas.LineTo(X, Y);
            Dec(Y, fTextHeight);
          end;
        end;

        // Reset pen
        SelectObject(Canvas.Handle, OldPen);
      finally
        DeleteObject(DottedPen);
      end;
    end;

    if not UseCodeFolding then
      Exit;

    // Paint collapsed lines using changed pen
    if fCodeFolding.ShowCollapsedLine then begin
      Canvas.Pen.Color := fCodeFolding.CollapsedLineColor;
      for i := 0 to fAllFoldRanges.Count - 1 do begin
        with fAllFoldRanges[i] do begin
          if Collapsed and not ParentCollapsed and (FromLine <= vLastLine) and (FromLine >= vFirstLine) then begin

            // Get starting and end points
            Y := (LineToRow(FromLine) - TopLine + 1) * fTextHeight - 1;
            Canvas.MoveTo(AClip.Left, Y);
            Canvas.LineTo(AClip.Right, Y);
          end;
        end;
      end;
    end;
  end;

  procedure GetBraceColorAttr(level:integer; var attr:TSynHighlighterAttributes);
  begin
    if not (eoShowRainbowColor in fOptions) then
      Exit;
    if (attr <> fHighlighter.SymbolAttribute) then
      Exit;
    case (level mod 4) of
      0: begin
        attr := fHighlighter.KeywordAttribute;
      end;
      1: begin
        attr := fHighlighter.SymbolAttribute;
      end;
      2: begin
        attr := fHighlighter.StringAttribute;
      end;
      3: begin
        attr := fHighlighter.IdentifierAttribute;
      end;
    end;
  end;

  procedure PaintLines;
  var
    cRow: integer; // row index for the loop
    vLine: integer;
    sLine: string; // the current line (expanded)
    sToken: string; // highlighter token info
    nTokenPos, nTokenLen: integer;
    attr: TSynHighlighterAttributes;
    vFirstChar: integer;
    vLastChar: integer;
    areaList:TList;
    colBorder: TColor;
    areaType:TEditingAreaType;
    foldRange:TSynEditFoldRange;
    nC1,nC2,nFold:integer;
    sFold: string;
  begin
    areaList:=TList.Create;
    try
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Bottom := (aFirstRow - TopLine) * fTextHeight;
    // Make sure the token accumulator string doesn't get reassigned too often.
    if Assigned(fHighlighter) then begin
      TokenAccu.MaxLen := Max(128, fCharsInWindow);
      SetLength(TokenAccu.s, TokenAccu.MaxLen);
    end;
    // Now loop through all the lines. The indices are valid for Lines.
    for cRow := aFirstRow to aLastRow do begin
      vLine := RowToLine(cRow);
      if (vLine > Lines.Count) and not (Lines.Count = 0) then
        break;

      // Get the expanded line.
      sLine := Lines.ExpandedStrings[vLine - 1];
      // determine whether will be painted with ActiveLineColor
      bCurrentLine := CaretY = vLine;
      // Initialize the text and background colors, maybe the line should
      // use special values for them.
      colFG := Font.Color;
      colBG := colEditorBG;
      bSpecialLine := DoOnSpecialLineColors(vLine, colFG, colBG);
      if bSpecialLine then begin
        // The selection colors are just swapped, like seen in Delphi.
        colSelFG := colBG;
        colSelBG := colFG;
      end else begin
        colSelFG := fSelectedColor.Foreground;
        colSelBG := fSelectedColor.Background;
        DoOnEditAreas(vLine, areaList,colBorder,areaType);
      end;

      // Removed word wrap support
      vFirstChar := FirstCol;
      vLastChar := LastCol;

      // Get the information about the line selection. Three different parts
      // are possible (unselected before, selected, unselected after), only
      // unselected or only selected means bComplexLine will be FALSE. Start
      // with no selection, compute based on the visible columns.
      bComplexLine := FALSE;
      nLineSelStart := 0;
      nLineSelEnd := 0;
      // Does the selection intersect the visible area?
      if bAnySelection and (cRow >= vSelStart.Row) and (cRow <= vSelEnd.Row) then begin
        // Default to a fully selected line. This is correct for the smLine
        // selection mode and a good start for the smNormal mode.
        nLineSelStart := FirstCol;
        nLineSelEnd := LastCol + 1;
        if (fActiveSelectionMode = smColumn) or
          ((fActiveSelectionMode = smNormal) and (cRow = vSelStart.Row)) then
          if (vSelStart.Column > LastCol) then begin
            nLineSelStart := 0;
            nLineSelEnd := 0;
          end else if (vSelStart.Column > FirstCol) then begin
            nLineSelStart := vSelStart.Column;
            bComplexLine := TRUE;
          end;
        if (fActiveSelectionMode = smColumn) or
          ((fActiveSelectionMode = smNormal) and (cRow = vSelEnd.Row)) then
          if (vSelEnd.Column < FirstCol) then begin
            nLineSelStart := 0;
            nLineSelEnd := 0;
          end else if (vSelEnd.Column < LastCol) then begin
            nLineSelEnd := vSelEnd.Column;
            bComplexLine := TRUE;
          end;
{$IFDEF SYN_MBCSSUPPORT}
        //todo: nLineSelStart & nLineSelEnd must be buffer coordinates
        if (fActiveSelectionMode = smColumn) then
          MBCSGetSelRangeInLineWhenColumnSelectionMode(sLine, nLineSelStart,
            nLineSelEnd);
{$ENDIF}
      end; //endif bAnySelection

      // Update the rcLine rect to this line.
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, fTextHeight);

      bLineSelected := (not bComplexLine) and (nLineSelStart > 0);
      rcToken := rcLine;

      if not Assigned(fHighlighter) or (not fHighlighter.Enabled) then begin
        // Remove text already displayed (in previous rows)
        if (vFirstChar <> FirstCol) or (vLastChar <> LastCol) then
          sToken := Copy(sLine, vFirstChar, vLastChar - vFirstChar)
        else
          sToken := sLine;
        if fShowSpecChar and (not bLineSelected) and (not bSpecialLine) and (Length(sLine) < vLastChar) then
          sToken := sToken + SynLineBreakGlyph;
        nTokenLen := Length(sToken);
        if bComplexLine then begin
          SetDrawingColors(FALSE);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(FirstCol));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(nLineSelStart));
          PaintToken(sToken, nTokenLen, 0, FirstCol, nLineSelStart,False);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(nLineSelEnd));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(LastCol));
          PaintToken(sToken, nTokenLen, 0, nLineSelEnd, LastCol,True);
          SetDrawingColors(TRUE);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(nLineSelStart));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(nLineSelEnd));
          PaintToken(sToken, nTokenLen, 0, nLineSelStart, nLineSelEnd - 1,False);
        end else begin
          SetDrawingColors(bLineSelected);
          PaintToken(sToken, nTokenLen, 0, FirstCol, LastCol,bLineSelected);
        end;
      end else begin
        // Initialize highlighter with line text and range info. It is
        // necessary because we probably did not scan to the end of the last
        // line - the internal highlighter range might be wrong.
        if vLine = 1 then begin
          fHighlighter.ResetRange;
          fHighlighter.ResetParenthesisLevel;
          fHighlighter.ResetBracketLevel;
          fHighlighter.ResetBraceLevel;
        end else begin
          fHighlighter.SetRange(Lines.Ranges[vLine - 2]);
          fHighlighter.SetParenthesisLevel(Lines.ParenthesisLevels[vLine - 2]);
          fHighlighter.SetBracketLevel(Lines.BracketLevels[vLine - 2]);
          fHighlighter.SetBraceLevel(Lines.BraceLevels[vLine - 2]);
        end;
        fHighlighter.SetLine(sLine, vLine - 1);
        // Try to concatenate as many tokens as possible to minimize the count
        // of ExtTextOut calls necessary. This depends on the selection state
        // or the line having special colors. For spaces the foreground color
        // is ignored as well.
        TokenAccu.Len := 0;
        nTokenPos := 0;
        // Test first whether anything of this token is visible.
        while not fHighlighter.GetEol do begin
          sToken := fHighlighter.GetToken;
          // Work-around buggy highlighters which return empty tokens.
          if sToken = '' then begin
            fHighlighter.Next;
            if fHighlighter.GetEol then
              break;
            sToken := fHighlighter.GetToken;
            // Maybe should also test whether GetTokenPos changed...
            if sToken = '' then
              raise Exception.Create('The highlighter seems to be in an infinite loop');
          end;
          nTokenPos := fHighlighter.GetTokenPos;
          nTokenLen := Length(sToken);
          if nTokenPos + nTokenLen >= vFirstChar then begin
            if nTokenPos + nTokenLen >= vLastChar then begin
              if nTokenPos >= vLastChar then
                break; //*** BREAK ***

              nTokenLen := vLastChar - nTokenPos - 1;
            end;
            // It's at least partially visible. Get the token attributes now.
            attr := fHighlighter.GetTokenAttribute;
            {
            if (nTokenPos = 0) and (attr = fHighlighter.WhitespaceAttribute) then begin
              sToken := StringOfChar('.',nTokenLen);
            end;
            }
            if sToken = '[' then begin
              GetBraceColorAttr(fHighlighter.GetBracketLevel,attr);
            end else if sToken = ']' then begin
              GetBraceColorAttr(fHighlighter.GetBracketLevel+1,attr);
            end else if sToken = '(' then begin
              GetBraceColorAttr(fHighlighter.GetParenthesisLevel,attr);
            end else if sToken = ')' then begin
              GetBraceColorAttr(fHighlighter.GetParenthesisLevel+1,attr);
            end else if sToken = '{' then begin
              GetBraceColorAttr(fHighlighter.GetBraceLevel,attr);
            end else if sToken = '}' then begin
              GetBraceColorAttr(fHighlighter.GetBraceLevel+1,attr);
            end;
            AddHighlightToken(sToken, nTokenPos - (vFirstChar - FirstCol),
              nTokenLen, cRow,attr);
          end;
          // Let the highlighter scan the next token.
          fHighlighter.Next;
        end;
        // Don't assume HL.GetTokenPos is valid after HL.GetEOL = True.
        Inc(nTokenPos, Length(sToken));
        if fHighlighter.GetEol and (nTokenPos < vLastChar) then begin
          // Draw text that couldn't be parsed by the highlighter, if any.
          if nTokenPos < Length(sLine) then begin
            if nTokenPos + 1 < vFirstChar then
              nTokenPos := vFirstChar - 1;
            nTokenLen := Min(Length(sLine), vLastChar) - (nTokenPos + 1);
            if nTokenLen > 0 then begin
              sToken := Copy(sLine, nTokenPos + 1, nTokenLen);
              AddHighlightToken(sToken, nTokenPos - (vFirstChar - FirstCol),
                nTokenLen, cRow, nil);
            end;
          end;
          // Draw LineBreak glyph.
          if (eoShowSpecialChars in fOptions) and (not bLineSelected)
            and (not bSpecialLine) and (Length(sLine) < vLastChar) then begin
            AddHighlightToken(SynLineBreakGlyph,
              Length(sLine) - (vFirstChar - FirstCol),
              Length(SynLineBreakGlyph),cRow, fHighLighter.WhitespaceAttribute);
          end;
        end;

        // Paint folding
        foldRange := FoldStartAtLine(vLine);
        if assigned(foldRange) and foldRange.Collapsed then begin
          sFold := ' ... }';
          nFold := Length(sFold);
          Attr := fHighlighter.SymbolAttribute;
          GetBraceColorAttr(fHighlighter.GetBraceLevel,attr);
          AddHighlightToken(sFold,Length(sLine)+1 - (vFirstChar - FirstCol)
            , nFold, cRow, attr);
          // Compute some helper variables.
          //nC1 := Max(FirstCol, Length(sLine)+1);
          //nC2 := Min(LastCol, Length(sLine) +1 + nFold + 1);
          //SetDrawingColors(FALSE);
          //PaintToken(sFold,nFold, Length(sLine)+1,nC1, nC2);
        end;

        // Draw anything that's left in the TokenAccu record. Fill to the end
        // of the invalid area with the correct colors.
        PaintHighlightToken(TRUE);
        
        //Paint editingAreaBorders
        PaintEditAreas(areaList,colBorder,areaType);

      end;

      // Now paint the right edge if necessary. We do it line by line to reduce
      // the flicker. Should not cost very much anyway, compared to the many
      // calls to ExtTextOut.
      if bDoRightEdge then begin
        Canvas.MoveTo(nRightEdge, rcLine.Top);
        Canvas.LineTo(nRightEdge, rcLine.Bottom + 1);
      end;
      bCurrentLine := False;
    end; //endfor cRow
    finally
      clearAreaList(areaList);
      areaList.Free;
    end;
  end;

  { end local procedures }

begin
  vFirstLine := RowToLine(aFirstRow);
  vLastLine := RowToLine(aLastRow);

  bCurrentLine := False;
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  SynTabGlyphString := SynTabGlyph;
  bDoRightEdge := FALSE;
  if (fRightEdge > 0) then begin // column value
    nRightEdge := fTextOffset + fRightEdge * fCharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then begin
      bDoRightEdge := TRUE;
      Canvas.Pen.Color := fRightEdgeColor;
      Canvas.Pen.Width := 1;
    end;
  end;
  // Do everything else with API calls. This (maybe) realizes the new pen color.
  dc := Canvas.Handle;

  // Paint the visible text lines. To make this easier, compute first the
  // necessary information about the selected area: is there any visible
  // selected area, and what are its lines / columns?
  if (vLastLine >= vFirstLine) then begin
    ComputeSelectionInfo;
    fTextDrawer.Style := Font.Style;
    fTextDrawer.BeginDrawing(dc);
    try
      PaintLines;
    finally
      fTextDrawer.EndDrawing;
    end;
  end;


  // If anything of the two pixel space before the text area is visible, then
  // fill it with the component background color.
  if (AClip.Left < fGutterWidth + 2) then begin
    rcToken := AClip;
    rcToken.Left := Max(AClip.Left, fGutterWidth);
    rcToken.Right := fGutterWidth + 2;
    // Paint whole left edge of the text with same color.
    // (value of WhiteAttribute can vary in e.g. MultiSyn)
    if Highlighter <> nil then begin
      fHighlighter.ResetRange;
      fHighlighter.ResetParenthesisLevel;
      fHighlighter.ResetBracketLevel;
      fHighlighter.ResetBraceLevel;
    end;
    Canvas.Brush.Color := colEditorBG;
    Canvas.FillRect(rcToken);
    // Adjust the invalid area to not include this area.
    AClip.Left := rcToken.Right;
  end;
    
  // If there is anything visible below the last line, then fill this as well.
  rcToken := AClip;
  rcToken.Top := (aLastRow - TopLine + 1) * fTextHeight;
  if (rcToken.Top < rcToken.Bottom) then begin
    if Highlighter <> nil then begin
      fHighlighter.ResetRange;
      fHighlighter.ResetParenthesisLevel;
      fHighlighter.ResetBracketLevel;
      fHighlighter.ResetBraceLevel;
    end;
    Canvas.Brush.Color := colEditorBG;
    Canvas.FillRect(rcToken);
    // Draw the right edge if necessary.
    if bDoRightEdge then begin
      Canvas.MoveTo(nRightEdge, rcToken.Top);
      Canvas.LineTo(nRightEdge, rcToken.Bottom + 1);
    end;
  end;

  // This messes with pen colors, so draw after right margin has been drawn
  PaintFoldAttributes;
end;

procedure TCustomSynEdit.PasteFromClipboard;
var
  AddPasteEndMarker: boolean;
  vStartOfBlock: TBufferCoord;
  vEndOfBlock: TBufferCoord;
  StoredPaintLock: integer;

  PasteMode: TSynSelectionMode;
  Mem: HGLOBAL;
  P: PChar;
begin
  if not CanPaste then
    exit;
  DoOnPaintTransient(ttBefore);
  BeginUndoBlock;
  AddPasteEndMarker := False;
  try
    // Check for our special format first.
    if Clipboard.HasFormat(SynEditClipboardFormat) then begin
      Clipboard.Open;
      try
        Mem := Clipboard.GetAsHandle(SynEditClipboardFormat);
        P := GlobalLock(Mem);
        if P <> nil then try
          fUndoList.AddChange(crPasteBegin, BlockBegin, BlockEnd, '', smNormal);
          AddPasteEndMarker := True;
          if SelAvail then begin
            fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
              fActiveSelectionMode);
          end;

          // Our format: SelectionMode value followed by text.
          // See CopyToClipboard
          PasteMode := PSynSelectionMode(P)^;
          inc(P, SizeOf(TSynSelectionMode));
          if SelAvail then begin
            vStartOfBlock := BlockBegin;
            vEndOfBlock := BlockEnd;
            fBlockBegin := vStartOfBlock;
            fBlockEnd := vEndOfBlock;
            // Pasting always occurs at column 0 when current selection is
            // smLine type
            if fActiveSelectionMode = smLine then
              vStartOfBlock.Char := 1;
          end else
            vStartOfBlock := CaretXY;

          SetSelTextPrimitiveEx(PasteMode, P, True);
          vEndOfBlock := BlockEnd;
          if PasteMode = smNormal then
            fUndoList.AddChange(crPaste, vStartOfBlock, vEndOfBlock, SelText,
              PasteMode)
          else if PasteMode = smColumn then
            // Do nothing. Moved to InsertColumn
          else if PasteMode = smLine then
            if CaretX = 1 then
              fUndoList.AddChange(crPaste, BufferCoord(1, vStartOfBlock.Line),
                BufferCoord(CharsInWindow, vEndOfBlock.Line - 1), SelText, smLine)
            else
              fUndoList.AddChange(crPaste, BufferCoord(1, vStartOfBlock.Line),
                vEndOfBlock, SelText, smNormal);
        finally
          GlobalUnlock(Mem);
        end else
          raise ESynEditError.Create('Clipboard paste operation failed.');
      finally
        Clipboard.Close;
      end;
      // If our special format isn't there, check for regular text format.
    end else if Clipboard.HasFormat(CF_TEXT) then begin
      fUndoList.AddChange(crPasteBegin, BlockBegin, BlockEnd, '', smNormal);
      AddPasteEndMarker := True;
      if SelAvail then begin
        fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd,
          SelText, fActiveSelectionMode);
      end else
        ActiveSelectionMode := SelectionMode;
      vStartOfBlock := BlockBegin;
      vEndOfBlock := BlockEnd;
      fBlockBegin := vStartOfBlock;
      fBlockEnd := vEndOfBlock;
      SetSelTextPrimitive(Clipboard.AsText);
      if fActiveSelectionMode <> smColumn then
        fUndoList.AddChange(crPaste, vStartOfBlock, BlockEnd, SelText, fActiveSelectionMode);
    end;
  finally
    if AddPasteEndMarker then
      fUndoList.AddChange(crPasteEnd, BlockBegin, BlockEnd, '', smNormal);
    EndUndoBlock;
  end;

  // ClientRect can be changed by UpdateScrollBars if eoHideShowScrollBars
  // is enabled
  if eoHideShowScrollBars in Options then begin
    StoredPaintLock := fPaintLock;
    try
      fPaintLock := 0;
      UpdateScrollBars;
    finally
      fPaintLock := StoredPaintLock;
    end;
  end;

  EnsureCursorPosVisible;
  // Selection should have changed...
  StatusChanged([scSelection]);
  DoOnPaintTransient(ttAfter);
end;

procedure TCustomSynEdit.SelectAll;
var
  LastPt: TBufferCoord;
begin
  LastPt.Char := 1;
  LastPt.Line := Lines.Count;
  if LastPt.Line > 0 then
    Inc(LastPt.Char, Length(Lines[LastPt.Line - 1]))
  else
    LastPt.Line := 1;
  SetCaretAndSelection(LastPt, BufferCoord(1, 1), LastPt);
  // Selection should have changed...
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockBegin(Value: TBufferCoord);
var
  nInval1, nInval2: integer;
  SelChanged: boolean;
begin
  ActiveSelectionMode := SelectionMode;
  if (eoScrollPastEol in Options) then
    Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  if (fActiveSelectionMode = smNormal) then
    if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
      Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
    else
      Value.Char := 1;
  if SelAvail then begin
    if fBlockBegin.Line < fBlockEnd.Line then begin
      nInval1 := Min(Value.Line, fBlockBegin.Line);
      nInval2 := Max(Value.Line, fBlockEnd.Line);
    end else begin
      nInval1 := Min(Value.Line, fBlockEnd.Line);
      nInval2 := Max(Value.Line, fBlockBegin.Line);
    end;
    fBlockBegin := Value;
    fBlockEnd := Value;
    InvalidateLines(nInval1, nInval2);
    SelChanged := TRUE;
  end else begin
    SelChanged :=
      (fBlockBegin.Char <> Value.Char) or (fBlockBegin.Line <> Value.Line) or
      (fBlockEnd.Char <> Value.Char) or (fBlockEnd.Line <> Value.Line);
    fBlockBegin := Value;
    fBlockEnd := Value;
  end;
  if SelChanged then
    StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockEnd(Value: TBufferCoord);
var
  nLine: integer;
{$IFDEF SYN_MBCSSUPPORT}
  s: string;
{$ENDIF}
begin
  ActiveSelectionMode := SelectionMode;
  if not (eoNoSelection in Options) then begin
    if (eoScrollPastEol in Options) then
      Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
    else
      Value.Char := Max(Value.Char, 1);
    Value.Line := MinMax(Value.Line, 1, Lines.Count);
    if (fActiveSelectionMode = smNormal) then
      if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
        Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
      else
        Value.Char := 1;
    if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then begin
{$IFDEF SYN_MBCSSUPPORT}
      if Value.Line <= Lines.Count then begin
        s := Lines[Value.Line - 1];
        if (Length(s) >= Value.Char) and (mbTrailByte = ByteType(s, Value.Char)) then
          Dec(Value.Char);
      end;
{$ENDIF}
      if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then begin
        if (fActiveSelectionMode = smColumn) and (Value.Char <> fBlockEnd.Char) then begin
          InvalidateLines(
            Min(fBlockBegin.Line, Min(fBlockEnd.Line, Value.Line)),
            Max(fBlockBegin.Line, Max(fBlockEnd.Line, Value.Line)));
          fBlockEnd := Value;
        end else begin
          nLine := fBlockEnd.Line;
          fBlockEnd := Value;
          if (fActiveSelectionMode <> smColumn) or (fBlockBegin.Char <> fBlockEnd.Char) then
            InvalidateLines(nLine, fBlockEnd.Line);
        end;
        StatusChanged([scSelection]);
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretX(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Char := Value;
  vNewCaret.Line := CaretY;
  SetCaretXY(vNewCaret);
end;

procedure TCustomSynEdit.SetCaretY(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Line := Value;
  vNewCaret.Char := CaretX;
  SetCaretXY(vNewCaret);
end;

procedure TCustomSynEdit.InternalSetCaretX(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Char := Value;
  vNewCaret.Line := CaretY;
  InternalSetCaretXY(vNewCaret);
end;

procedure TCustomSynEdit.InternalSetCaretY(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Line := Value;
  vNewCaret.Char := CaretX;
  InternalSetCaretXY(vNewCaret);
end;

function TCustomSynEdit.GetCaretXY: TBufferCoord;
begin
  Result.Char := CaretX;
  Result.Line := CaretY;
end;

function TCustomSynEdit.GetDisplayX: Integer;
begin
  Result := DisplayXY.Column; // account for tabs
end;

function TCustomSynEdit.GetDisplayY: Integer;
begin
  Result := DisplayXY.Row; // account for folds
end;

function TCustomSynEdit.GetDisplayXY: TDisplayCoord;
begin
  Result := BufferToDisplayPos(CaretXY);
end;

procedure TCustomSynEdit.SetCaretXY(const Value: TBufferCoord);
begin
  SetCaretXYCentered(False, Value);
end;

procedure TCustomSynEdit.SetCaretXYCentered(ForceToMiddle: Boolean; const Value: TBufferCoord);
begin
  IncPaintLock;
  try
    Include(fStatusChanges, scSelection);
    if ForceToMiddle then
      SetCaretXYEx(False, Value) // do not call EnsureCursorPosVisible here
    else
      SetCaretXYEx(True, Value);
    if SelAvail then
      InvalidateSelection;
    fBlockBegin.Char := fCaretX;
    fBlockBegin.Line := fCaretY;
    fBlockEnd := fBlockBegin;
    if ForceToMiddle then
      EnsureCursorPosVisibleEx(True); // but here after block has been set
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.InternalSetCaretXY(const Value: TBufferCoord);
begin
  SetCaretXYEx(True, Value);
end;

procedure TCustomSynEdit.UpdateLastCaretX;
begin
{$IFDEF SYN_MBCSSUPPORT}
  fMBCSStepAside := False;
{$ENDIF}
  fLastCaretX := DisplayX;
end;

procedure TCustomSynEdit.SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TBufferCoord);
var
  nMaxX: Integer;
  vTriggerPaint: boolean;
begin
  vTriggerPaint := HandleAllocated;
  if vTriggerPaint then
    DoOnPaintTransient(ttBefore);
  nMaxX := MaxScrollWidth + 1;
  if Value.Line > Lines.Count then
    Value.Line := Lines.Count;
  if Value.Line < 1 then begin
    // this is just to make sure if Lines stringlist should be empty
    Value.Line := 1;
    if not (eoScrollPastEol in fOptions) then
      nMaxX := 1;
  end else begin
    if not (eoScrollPastEol in fOptions) then
      nMaxX := Length(Lines[Value.Line - 1]) + 1;
  end;
  if (Value.Char > nMaxX) and (not (eoScrollPastEol in Options) or
    not (eoAutoSizeMaxScrollWidth in Options)) then begin
    Value.Char := nMaxX;
  end;
  if Value.Char < 1 then
    Value.Char := 1;
  if (Value.Char <> fCaretX) or (Value.Line <> fCaretY) then begin
    IncPaintLock;
    try
      // simply include the flags, fPaintLock is > 0
      if fCaretX <> Value.Char then begin
        fCaretX := Value.Char;
        Include(fStatusChanges, scCaretX);
      end;
      if fCaretY <> Value.Line then begin
        if ActiveLineColor <> clNone then begin
          InvalidateLine(Value.Line);
          InvalidateLine(fCaretY);
        end;
        fCaretY := Value.Line;
        Include(fStatusChanges, scCaretY);
      end;
      // Call UpdateLastCaretX before DecPaintLock because the event handler it
      // calls could raise an exception, and we don't want fLastCaretX to be
      // left in an undefined state if that happens.
      UpdateLastCaretX;
      if CallEnsureCursorPos then
        EnsureCursorPosVisible;
      Include(fStateFlags, sfCaretChanged);
      Include(fStateFlags, sfScrollbarChanged);
    finally
      DecPaintLock;
    end;
  end else begin
    // Also call UpdateLastCaretX if the caret didn't move. Apps don't know
    // anything about fLastCaretX and they shouldn't need to. So, to avoid any
    // unwanted surprises, always update fLastCaretX whenever CaretXY is
    // assigned to.
    // Note to SynEdit developers: If this is undesirable in some obscure
    // case, just save the value of fLastCaretX before assigning to CaretXY and
    // restore it afterward as appropriate.
    UpdateLastCaretX;
  end;
  if vTriggerPaint then
    DoOnPaintTransient(ttAfter);
end;

function TCustomSynEdit.CaretInView: Boolean;
var
  vCaretRowCol: TDisplayCoord;
begin
  vCaretRowCol := DisplayXY;
  Result := (vCaretRowCol.Column >= LeftChar)
    and (vCaretRowCol.Column <= LeftChar + CharsInWindow)
    and (vCaretRowCol.Row >= TopLine)
    and (vCaretRowCol.Row <= TopLine + LinesInWindow);
end;

procedure TCustomSynEdit.SetActiveLineColor(Value: TColor);
begin
  if (fActiveLineColor <> Value) then begin
    fActiveLineColor := Value;
    InvalidateLine(CaretY);
  end;
end;

procedure TCustomSynEdit.SetFont(const Value: TFont);
var
  DC: HDC;
  Save: THandle;
  Metrics: TTextMetric;
  AveCW, MaxCW: Integer;
begin
  DC := GetDC(0);
  Save := SelectObject(DC, Value.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, Save);
  ReleaseDC(0, DC);
  with Metrics do begin
    AveCW := tmAveCharWidth;
    MaxCW := tmMaxCharWidth;
  end;
  case AveCW = MaxCW of
    True: inherited Font := Value;
    False: begin
        with fFontDummy do begin
          Color := Value.Color;
          Pitch := fpFixed;
          Size := Value.Size;
          Style := Value.Style;
          Name := Value.Name;
        end;
        inherited Font := fFontDummy;
      end;
  end;
  if fGutter.ShowLineNumbers then
    GutterChanged(Self);
end;

procedure TCustomSynEdit.SetGutterWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if fGutterWidth <> Value then begin
    fGutterWidth := Value;
    fTextOffset := fGutterWidth + 2 - (LeftChar - 1) * fCharWidth;
    if HandleAllocated then begin
      fCharsInWindow := Max(ClientWidth - fGutterWidth - 2, 0) div fCharWidth;
      UpdateScrollBars;
      Invalidate;
    end;
  end;
end;

procedure TCustomSynEdit.SetLeftChar(Value: Integer);
var
  MaxVal: integer;
  iDelta: integer;
  iTextArea: TRect;
begin
  if eoScrollPastEol in Options then begin
    if eoAutoSizeMaxScrollWidth in Options then
      MaxVal := MaxInt - CharsInWindow
    else
      MaxVal := MaxScrollWidth - CharsInWindow + 1
  end else begin
    MaxVal := Lines.LengthOfLongestLine;
    if MaxVal > CharsInWindow then
      MaxVal := MaxVal - CharsInWindow + 1
    else
      MaxVal := 1;
  end;
  Value := MinMax(Value, 1, MaxVal);
  if Value <> fLeftChar then begin
    iDelta := fLeftChar - Value;
    fLeftChar := Value;
    fTextOffset := fGutterWidth + 2 - (LeftChar - 1) * fCharWidth;
    if Abs(iDelta) < CharsInWindow then begin
      iTextArea := ClientRect;
      Inc(iTextArea.Left, fGutterWidth + 2);
      ScrollWindow(Handle, iDelta * CharWidth, 0, @iTextArea, @iTextArea);
    end else
      InvalidateLines(-1, -1);
    if (Options >= [eoAutoSizeMaxScrollWidth, eoScrollPastEol]) and
      (MaxScrollWidth < LeftChar + CharsInWindow) then begin
      MaxScrollWidth := LeftChar + CharsInWindow
    end else
      UpdateScrollBars;
    StatusChanged([scLeftChar]);
  end;
end;

procedure TCustomSynEdit.SetLines(Value: TSynEditStringList);
begin
  Lines.Assign(Value);
end;

procedure TCustomSynEdit.SetLineText(Value: string);
begin
  if (CaretY >= 1) and (CaretY <= Max(1, Lines.Count)) then
    Lines[CaretY - 1] := Value;
end;

procedure TCustomSynEdit.SetFontSmoothing(AValue: TSynFontSmoothMethod);
const
  NONANTIALIASED_QUALITY = 3;
  ANTIALIASED_QUALITY = 4;
  CLEARTYPE_QUALITY = 5;
var
  bMethod: Byte;
  lf: TLogFont;
begin
  if fFontSmoothing <> AValue then begin
    fFontSmoothing := AValue;
    case fFontSmoothing of
      fsmAntiAlias:
        bMethod := ANTIALIASED_QUALITY;
      fsmClearType:
        bMethod := CLEARTYPE_QUALITY;
    else // fsmNone also
      bMethod := NONANTIALIASED_QUALITY;
    end;
    GetObject(Font.Handle, SizeOf(TLogFont), @lf);
    lf.lfQuality := bMethod;
    Font.Handle := CreateFontIndirect(lf);
  end;
end;

procedure TCustomSynEdit.SetName(const Value: TComponentName);
var
  TextToName: Boolean;
begin
  TextToName := (ComponentState * [csDesigning, csLoading] = [csDesigning])
    and (TrimRight(Text) = Name);
  inherited SetName(Value);
  if TextToName then
    Text := Value;
end;

procedure TCustomSynEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then begin
    FScrollBars := Value;
    UpdateScrollBars;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetSelTextPrimitive(const aValue: string);
begin
  SetSelTextPrimitiveEx(fActiveSelectionMode, PChar(aValue), True);
end;

procedure TCustomSynEdit.SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode;
  Value: PChar; AddToUndoList: Boolean);
var
  BB, BE: TBufferCoord;
  TempString: string;

  procedure DeleteSelection;
  var
    x, MarkOffset: Integer;
    UpdateMarks: boolean;
  {$IFDEF SYN_MBCSSUPPORT}
    l, r: Integer;
  {$ENDIF}
  begin
    UpdateMarks := FALSE;
    MarkOffset := 0;
    case fActiveSelectionMode of
      smNormal: begin
          if Lines.Count > 0 then begin
            // Create a string that contains everything on the first line up
            // to the selection mark, and everything on the last line after
            // the selection mark.
            TempString := Copy(Lines[BB.Line - 1], 1, BB.Char - 1) +
              Copy(Lines[BE.Line - 1], BE.Char, MaxInt);
            // Delete all lines in the selection range.
            Lines.DeleteLines(BB.Line, BE.Line - BB.Line);
            // Put the stuff that was outside of selection back in.
            if Options >= [eoScrollPastEol, eoTrimTrailingSpaces] then
              TempString := TrimTrailingSpaces(TempString);
            Lines[BB.Line - 1] := TempString;
          end;
          UpdateMarks := TRUE;
          InternalCaretXY := BB;
        end;
      smColumn: begin
          // swap X if needed
          if BB.Char > BE.Char then
            SwapInt(integer(BB.Char), integer(BE.Char));

          for x := BB.Line - 1 to BE.Line - 1 do begin
            TempString := Lines[x];
{$IFNDEF SYN_MBCSSUPPORT}
            Delete(TempString, BB.Char, BE.Char - BB.Char);
{$ELSE}
            l := BB.Char;
            r := BE.Char;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(TempString, l, r);
            Delete(TempString, l, r - l);
{$ENDIF}
            ProperSetLine(x, TempString);
          end;
          // Lines never get deleted completely, so keep caret at end.
          InternalCaretXY := BufferCoord(BB.Char, fBlockEnd.Line);
          // Column deletion never removes a line entirely, so no mark
          // updating is needed here.
        end;
      smLine: begin
          if BE.Line = Lines.Count then begin
            Lines[BE.Line - 1] := '';
            for x := BE.Line - 2 downto BB.Line - 1 do
              Lines.Delete(x);
          end else begin
            for x := BE.Line - 1 downto BB.Line - 1 do
              Lines.Delete(x);
          end;
          // smLine deletion always resets to first column.
          InternalCaretXY := BufferCoord(1, BB.Line);
          UpdateMarks := TRUE;
          MarkOffset := 1;
        end;
    end;
    // Update marks
    if UpdateMarks then
      DoLinesDeleted(BB.Line, BE.Line - BB.Line + MarkOffset);
  end;

  procedure InsertText;

    function CountLines(p: PChar): integer;
    begin
      Result := 0;
      while p^ <> #0 do begin
        if p^ = #13 then
          Inc(p);
        if p^ = #10 then
          Inc(p);
        Inc(Result);
        p := GetEOL(p);
      end;
    end;

    function InsertNormal: Integer;
    var
      sLeftSide: string;
      sRightSide: string;
      Str: string;
      Start: PChar;
      P: PChar;
      bChangeScroll: Boolean;
      spaceCount : integer;
    begin
      Result := 0;
      sLeftSide := Copy(LineText, 1, CaretX - 1);
      if CaretX - 1 > Length(sLeftSide) then begin
        if not (eoTabsToSpaces in Options) and StringIsBlank(sLeftSide) then
          sLeftSide := GetLeftSpacing(DisplayX - 1, True)
        else
          sLeftSide := sLeftSide + StringOfChar(#32,
            CaretX - 1 - Length(sLeftSide));
      end;
      sRightSide := Copy(LineText, CaretX, MaxInt);
      if fUndoing then begin
        SpaceCount := 0
      end else begin
        SpaceCount := LeftSpacesEx(sLeftSide, true);
      end;
      // step1: insert the first line of Value into current line
      Start := PChar(Value);
      P := GetEOL(Start);
      if P^ <> #0 then begin
        Str := sLeftSide + Copy(Value, 1, P - Start);
        ProperSetLine(CaretY - 1, Str);
        Lines.InsertLines(CaretY, CountLines(P));
      end else begin
        Str := sLeftSide + Value + sRightSide;
        ProperSetLine(CaretY - 1, Str);
      end;
      // step2: insert remaining lines of Value
      while P^ <> #0 do begin
        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        Inc(fCaretY);
        Include(fStatusChanges, scCaretY);
        Start := P;
        P := GetEOL(Start);
        if P = Start then begin
          if p^ <> #0 then
            Str := ''
          else
            Str := sRightSide;
        end else begin
          SetString(Str, Start, P - Start);
          if p^ = #0 then
            Str := Str + sRightSide
        end;
        str := GetLeftSpacing(SpaceCount, true)+Str;
        ProperSetLine(CaretY - 1, str);
        Inc(Result);
      end;
      bChangeScroll := not (eoScrollPastEol in fOptions);
      Include(fOptions, eoScrollPastEol);
      try
        if (eoTrimTrailingSpaces in Options) and (sRightSide = '') then begin
          InternalCaretX := Length(LineText) + GetExpandedLength(Str, TabWidth)
            - GetExpandedLength(LineText, TabWidth) + 1;
        end else
          InternalCaretX := Length(Str) - Length(sRightSide) + 1;
      finally
        if bChangeScroll then
          Exclude(fOptions, eoScrollPastEol);
      end;
    end;

    function InsertColumn: Integer;
    var
      Str: string;
      Start: PChar;
      P: PChar;
      Len: Integer;
      InsertPos: Integer;
      LineBreakPos: TBufferCoord;
    begin
      Result := 0;
      // Insert string at current position
      InsertPos := CaretX;
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
          if CaretY > Lines.Count then begin
            Inc(Result);
            TempString := StringOfChar(#32, InsertPos - 1) + Str;
            Lines.Add('');
            if AddToUndoList then begin
              LineBreakPos.Line := CaretY - 1;
              LineBreakPos.Char := Length(Lines[CaretY - 2]) + 1;
              fUndoList.AddChange(crLineBreak, LineBreakPos, LineBreakPos, '', smNormal);
            end;
          end else begin
            TempString := Lines[CaretY - 1];
            Len := Length(TempString);
            if Len < InsertPos then begin
              TempString :=
                TempString + StringOfChar(#32, InsertPos - Len - 1) + Str
            end else begin
  {$IFDEF SYN_MBCSSUPPORT}
              if mbTrailByte = ByteType(TempString, InsertPos) then
                Insert(Str, TempString, InsertPos + 1)
              else
  {$ENDIF}
                Insert(Str, TempString, InsertPos);
            end;
          end;
          ProperSetLine(CaretY - 1, TempString);
          // Add undo change here from PasteFromClipboard
          if AddToUndoList then begin
            fUndoList.AddChange(crPaste, BufferCoord(InsertPos, CaretY),
              BufferCoord(InsertPos + (P - Start), CaretY), '', fActiveSelectionMode);
          end;
        end;
        if P^ = #13 then begin
          Inc(P);
          if P^ = #10 then
            Inc(P);
          Inc(fCaretY);
          Include(fStatusChanges, scCaretY);
        end;
        Start := P;
      until P^ = #0;
      Inc(fCaretX, Length(Str));
      Include(fStatusChanges, scCaretX);
    end;

    function InsertLine: Integer;
    var
      Start: PChar;
      P: PChar;
      Str: string;
    begin
      Result := 0;
      fCaretX := 1;
      StatusChanged([scCaretX]);
      // Insert string before current line
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then
          SetString(Str, Start, P - Start)
        else
          Str := '';

        if (CaretY = Lines.Count) or InsertMode then begin
          Lines.Insert(CaretY - 1, '');
          Inc(Result);
        end;
        ProperSetLine(CaretY - 1, Str);
        Inc(fCaretY);
        Include(fStatusChanges, scCaretY);

        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        Start := P;
      until P^ = #0;
    end;

  var
    StartLine: Integer;
    StartCol: Integer;
    InsertedLines: Integer;
  begin
    if Value = '' then
      Exit;

    StartLine := CaretY;
    StartCol := CaretX;
    case PasteMode of
      smNormal:
        InsertedLines := InsertNormal;
      smColumn:
        InsertedLines := InsertColumn;
      smLine:
        InsertedLines := InsertLine;
    else
      InsertedLines := 0;
    end;
    // We delete selected based on the current selection mode, but paste
    // what's on the clipboard according to what it was when copied.
    // Update marks
    if InsertedLines > 0 then begin
      if (PasteMode = smNormal) and (StartCol > 1) then
        Inc(StartLine);
      DoLinesInserted(StartLine, InsertedLines);
    end;
    EnsureCursorPosVisible;
  end;

begin
  IncPaintLock;
  Lines.BeginUpdate;
  try
    BB := BlockBegin;
    BE := BlockEnd;
    if SelAvail then begin
      DeleteSelection;
      InternalCaretXY := BB;
    end;
    if (Value <> nil) and (Value[0] <> #0) then
      InsertText;
    if CaretY < 1 then
      InternalCaretY := 1;
  finally
    Lines.EndUpdate;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SynSetText(const Value: string);
begin
  Lines.Text := Value;
end;

procedure TCustomSynEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
begin
  if (eoScrollPastEof in Options) then
    Value := Min(Value, DisplayLineCount)
  else
    Value := Min(Value, DisplayLineCount - fLinesInWindow + 1);
  Value := Max(Value, 1);
  if Value <> TopLine then begin
    Delta := TopLine - Value;
    fTopLine := Value;
    if fPainterLock = 0 then
      if Abs(Delta) < fLinesInWindow then begin
        ScrollWindow(Handle, 0, fTextHeight * Delta, nil, nil)
      end else
        Invalidate;
    UpdateScrollBars;
    StatusChanged([scTopLine]);
  end;
end;

procedure TCustomSynEdit.ShowCaret;
begin
  if not (eoNoCaret in Options) and not (sfCaretVisible in fStateFlags) then begin
    if Windows.ShowCaret(Handle) then
      Include(fStateFlags, sfCaretVisible);
  end;
end;

procedure TCustomSynEdit.UpdateCaret;
var
  CX, CY: Integer;
{$IFDEF SYN_MBCSSUPPORT}
  cf: TCompositionForm;
{$ENDIF}
  iClientRect: TRect;
  vCaretDisplay: TDisplayCoord;
  vCaretPix: TPoint;
begin
  if (PaintLock <> 0) or not (Focused or FAlwaysShowCaret) then
    Include(fStateFlags, sfCaretChanged)
  else begin
    Exclude(fStateFlags, sfCaretChanged);
    vCaretDisplay := DisplayXY;
    vCaretPix := RowColumnToPixels(vCaretDisplay);
    CX := vCaretPix.X + FCaretOffset.X;
    CY := vCaretPix.Y + FCaretOffset.Y + 1;
    iClientRect := GetClientRect;
    Inc(iClientRect.Left, fGutterWidth);
    if (CX >= iClientRect.Left) and (CX < iClientRect.Right)
      and (CY >= iClientRect.Top) and (CY < iClientRect.Bottom) then begin
      SetCaretPos(CX, CY);
      ShowCaret;
    end else begin
      SetCaretPos(CX, CY);
      HideCaret;
    end;
{$IFDEF SYN_MBCSSUPPORT}
    cf.dwStyle := CFS_POINT;
    cf.ptCurrentPos := Point(CX, CY);
    ImmSetCompositionWindow(ImmGetContext(Handle), @cf);
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.UpdateScrollBars;
var
  nMaxScroll: integer;
  ScrollInfo: TScrollInfo;
  iRightChar: Integer;
begin
  if not HandleAllocated or (PaintLock <> 0) then
    Include(fStateFlags, sfScrollbarChanged)
  else begin
    Exclude(fStateFlags, sfScrollbarChanged);
    if fScrollBars <> ssNone then begin
      ScrollInfo.cbSize := SizeOf(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL;
      if not (eoHideShowScrollbars in Options) then begin
        ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;
      end;

      if (fScrollBars in [ssBoth, ssHorizontal]) then begin
        if eoScrollPastEol in Options then
          nMaxScroll := MaxScrollWidth
        else
          nMaxScroll := Max(Lines.LengthOfLongestLine, 1);
        if nMaxScroll <= MAX_SCROLL then begin
          ScrollInfo.nMin := 1;
          ScrollInfo.nMax := nMaxScroll;
          ScrollInfo.nPage := CharsInWindow;
          ScrollInfo.nPos := LeftChar;
        end else begin
          ScrollInfo.nMin := 0;
          ScrollInfo.nMax := MAX_SCROLL;
          ScrollInfo.nPage := MulDiv(MAX_SCROLL, CharsInWindow, nMaxScroll);
          ScrollInfo.nPos := MulDiv(MAX_SCROLL, LeftChar, nMaxScroll);
        end;

        ShowScrollBar(Handle, SB_HORZ, not (eoHideShowScrollbars in Options) or
          (ScrollInfo.nMin = 0) or (ScrollInfo.nMax > CharsInWindow));
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

        //Now for the arrows
        if (eoDisableScrollArrows in Options) or (nMaxScroll <= CharsInWindow) then begin
          iRightChar := LeftChar + CharsInWindow - 1;
          if (LeftChar <= 1) and (iRightChar >= nMaxScroll) then begin
            EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_BOTH);
          end else begin
            EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);
            if (LeftChar <= 1) then
              EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_LEFT)
            else if iRightChar >= nMaxScroll then
              EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_RIGHT);
          end;
        end else
          EnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH);
      end else
        ShowScrollBar(Handle, SB_HORZ, False);

      if fScrollBars in [ssBoth, ssVertical] then begin
        nMaxScroll := DisplayLineCount;
        if (eoScrollPastEof in Options) then
          Inc(nMaxScroll, LinesInWindow - 1);
        if nMaxScroll <= MAX_SCROLL then begin
          ScrollInfo.nMin := 1;
          ScrollInfo.nMax := Max(1, nMaxScroll);
          ScrollInfo.nPage := LinesInWindow;
          ScrollInfo.nPos := TopLine;
        end else begin
          ScrollInfo.nMin := 0;
          ScrollInfo.nMax := MAX_SCROLL;
          ScrollInfo.nPage := MulDiv(MAX_SCROLL, LinesInWindow, nMaxScroll);
          ScrollInfo.nPos := MulDiv(MAX_SCROLL, TopLine, nMaxScroll);
        end;

        ShowScrollBar(Handle, SB_VERT, not (eoHideShowScrollbars in Options) or
          (ScrollInfo.nMin = 0) or (ScrollInfo.nMax > LinesInWindow));
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);

        if (eoDisableScrollArrows in Options) or (nMaxScroll <= LinesInWindow) then begin
          if (TopLine <= 1) and (nMaxScroll <= LinesInWindow) then begin
            EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH);
          end else begin
            EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
            if (TopLine <= 1) then
              EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_UP)
            else if ((DisplayLineCount - TopLine - LinesInWindow + 1) = 0) then
              EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_DOWN);
          end;
        end else
          EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
      end else
        ShowScrollBar(Handle, SB_VERT, False);
    end {endif fScrollBars <> ssNone}
    else
      ShowScrollBar(Handle, SB_BOTH, False);
  end;
end;

function TCustomSynEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  WHEEL_DIVISOR = 120; // Mouse Wheel standard
var
  iWheelClicks: integer;
  iLinesToScroll: integer;
begin
  // Use control for text resizing in Dev-C++, so don't handle the message here
  if ssCtrl in Shift then begin
    Result := false;
    exit;
  end;
  // use alt for fast scrolling
  if ssAlt in Shift then
    iLinesToScroll := LinesInWindow shr Ord(eoHalfPageScroll in fOptions)
  else
    iLinesToScroll := 3;
  Inc(fMouseWheelAccumulator, WheelDelta);
  iWheelClicks := fMouseWheelAccumulator div WHEEL_DIVISOR;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DIVISOR;
  TopLine := TopLine - iWheelClicks * iLinesToScroll;
  Update;
  if Assigned(OnScroll) then
    OnScroll(Self, sbVertical);
  Result := True;
end;

procedure TCustomSynEdit.WMCaptureChanged(var Msg: TMessage);
begin
  fScrollTimer.Enabled := False;
  inherited;
end;

procedure TCustomSynEdit.WMClear(var Msg: TMessage);
begin
  if not ReadOnly then
    SelText := '';
end;

procedure TCustomSynEdit.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMCut(var Message: TMessage);
begin
  if not ReadOnly then
    CutToClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMDropFiles(var Msg: TMessage);
var
  i, iNumberDropped: integer;
  szPathName: array[0..260] of char;
  Point: TPoint;
  FilesList: TStringList;
begin
  try
    if Assigned(fOnDropFiles) then begin
      FilesList := TStringList.Create;
      try
        iNumberDropped := DragQueryFile(THandle(Msg.wParam), Cardinal(-1),
          nil, 0);
        DragQueryPoint(THandle(Msg.wParam), Point);

        for i := 0 to iNumberDropped - 1 do begin
          DragQueryFile(THandle(Msg.wParam), i, szPathName,
            SizeOf(szPathName));
          FilesList.Add(szPathName);
        end;
        fOnDropFiles(Self, Point.X, Point.Y, FilesList);
      finally
        FilesList.Free;
      end;
    end;
  finally
    Msg.Result := 0;
    DragFinish(THandle(Msg.wParam));
  end;
end;

procedure TCustomSynEdit.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TCustomSynEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if fWantTabs then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
  if fWantReturns then
    Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;

procedure TCustomSynEdit.WMGetText(var Msg: TWMGetText);
begin
  StrPLCopy(Msg.Text, Text, Msg.TextMax - 1);
  Msg.Result := StrLen(Msg.Text);
end;

procedure TCustomSynEdit.WMGetTextLength(var Msg: TWMGetTextLength);
begin
  // Avoid (useless) temporary copy of WindowText while window is recreated
  // because of docking.
  if csDocking in ControlState then
    Msg.Result := 0
  else
    Msg.Result := Length(Text);
end;

procedure TCustomSynEdit.WMHScroll(var Msg: TWMScroll);
var
  iMaxWidth: integer;
begin
  Msg.Result := 0;
  case Msg.ScrollCode of
    // Scrolls to start / end of the line
    SB_TOP: LeftChar := 1;
    SB_BOTTOM:
      if eoScrollPastEol in Options then
        LeftChar := MaxScrollWidth - CharsInWindow + 1
      else
        // Simply set LeftChar property to the LengthOfLongestLine,
        // it would do the range checking and constrain the value if necessary
        LeftChar := Lines.LengthOfLongestLine;
    // Scrolls one char left / right
    SB_LINEDOWN: LeftChar := LeftChar + 1;
    SB_LINEUP: LeftChar := LeftChar - 1;
    // Scrolls one page of chars left / right
    SB_PAGEDOWN: LeftChar := LeftChar
      + (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGEUP: LeftChar := LeftChar
      - (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
    // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
      SB_THUMBTRACK: begin
        FIsScrolling := True;
        if eoScrollPastEol in Options then
          iMaxWidth := MaxScrollWidth
        else
          iMaxWidth := Max(Lines.LengthOfLongestLine, 1);
        if iMaxWidth > MAX_SCROLL then
          LeftChar := MulDiv(iMaxWidth, Msg.Pos, MAX_SCROLL)
        else
          LeftChar := Msg.Pos;
      end;
    SB_ENDSCROLL: FIsScrolling := False;
  end;
  if Assigned(OnScroll) then
    OnScroll(Self, sbHorizontal);
end;

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  CommandProcessor(ecLostFocus, #0, nil);
  //Added check for focused to prevent caret disappearing problem
  if Focused or FAlwaysShowCaret then
    exit;
  HideCaret;
  Windows.DestroyCaret;
  if FHideSelection and SelAvail then
    InvalidateSelection;
end;

procedure TCustomSynEdit.WMPaste(var Message: TMessage);
begin
  if not ReadOnly then
    PasteFromClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMCancelMode(var Message: TMessage);
begin

end;

procedure TCustomSynEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  CommandProcessor(ecGotFocus, #0, nil);

  InitializeCaret;
  if FHideSelection and SelAvail then
    InvalidateSelection;
end;

procedure TCustomSynEdit.WMSetText(var Msg: TWMSetText);
begin
  Msg.Result := 1;
  try
    Text := Msg.Text
  except
    Msg.Result := 0;
    raise
  end
end;

procedure TCustomSynEdit.WMSize(var Msg: TWMSize);
begin
  inherited;
  SizeOrFontChanged(False);
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then
    ScrollHintWnd := HintWindowClass.Create(Application);
  Result := ScrollHintWnd;
end;

procedure TCustomSynEdit.WMUndo(var Msg: TMessage);
begin
  Undo;
end;

procedure TCustomSynEdit.WMVScroll(var Msg: TWMScroll);
var
  s: ShortString;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
  ButtonH: Integer;
  ScrollInfo: TScrollInfo;
begin
  Msg.Result := 0;
  case Msg.ScrollCode of
    // Scrolls to start / end of the text
    SB_TOP: TopLine := 1;
    SB_BOTTOM: TopLine := DisplayLineCount;
    // Scrolls one line up / down
    SB_LINEDOWN: TopLine := TopLine + 1;
    SB_LINEUP: TopLine := TopLine - 1;
    // Scrolls one page of lines up / down
    SB_PAGEDOWN: TopLine := TopLine
      + (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGEUP: TopLine := TopLine
      - (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
    // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
      SB_THUMBTRACK: begin
        FIsScrolling := True;
        if DisplayLineCount > MAX_SCROLL then
          TopLine := MulDiv(LinesInWindow + DisplayLineCount - 1, Msg.Pos,
            MAX_SCROLL)
        else
          TopLine := Msg.Pos;

        if eoShowScrollHint in fOptions then begin
          ScrollHint := GetScrollHint;
          ScrollHint.Color := fScrollHintColor;
          case FScrollHintFormat of
            shfTopLineOnly:
              s := Format(SYNS_ScrollInfoFmtTop, [RowToLine(TopLine)]);
          else
            s := Format(SYNS_ScrollInfoFmt, [RowToLine(TopLine),
              RowToLine(TopLine + Min(LinesInWindow, DisplayLineCount - TopLine))]);
          end;

          rc := ScrollHint.CalcHintRect(200, s, nil);
          if eoScrollHintFollows in fOptions then begin
            ButtonH := GetSystemMetrics(SM_CYVSCROLL);

            FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
            ScrollInfo.cbSize := SizeOf(ScrollInfo);
            ScrollInfo.fMask := SIF_ALL;
            GetScrollInfo(Handle, SB_VERT, ScrollInfo);

            pt := ClientToScreen(Point(ClientWidth - rc.Right - 4,
              ((rc.Bottom - rc.Top) shr 1) + //half the size of the hint window
              Round((ScrollInfo.nTrackPos / ScrollInfo.nMax) *
              //The percentage of the page that has been scrolled
              (ClientHeight - (ButtonH * 2))) //The height minus the arrow buttons
              + ButtonH)); //The height of the top button
          end else
            pt := ClientToScreen(Point(ClientWidth - rc.Right - 4, 10));

          OffsetRect(rc, pt.x, pt.y);
          ScrollHint.ActivateHint(rc, s);
          SendMessage(ScrollHint.Handle, WM_NCPAINT, 1, 0);
          ScrollHint.Update;
        end;
      end;
    // Ends scrolling
    SB_ENDSCROLL: begin
        FIsScrolling := False;
        if eoShowScrollHint in fOptions then
          ShowWindow(GetScrollHint.Handle, SW_HIDE);
      end;
  end;
  Update;
  if Assigned(OnScroll) then
    OnScroll(Self, sbVertical);
end;

function TCustomSynEdit.ScanFrom(Index: integer): integer;
var
  iRange: TSynEditRange;
begin
  Result := Index;
  if Result >= Lines.Count then
    Exit;

  if Result = 0 then begin
    fHighlighter.ResetRange;
    fHighlighter.ResetParenthesisLevel;
    fHighlighter.ResetBracketLevel;
    fHighlighter.ResetBraceLevel;
  end else begin
    fHighlighter.SetRange(Lines.Ranges[Result - 1]);
    fHighlighter.SetParenthesisLevel(Lines.ParenthesisLevels[Result - 1]);
    fHighlighter.SetBracketLevel(Lines.BracketLevels[Result - 1]);
    fHighlighter.SetBraceLevel(Lines.BraceLevels[Result - 1]);
  end;

  repeat
    fHighlighter.SetLine(Lines[Result], Result);
    fHighlighter.NextToEol;
    iRange := fHighlighter.GetRange;
    {
    if Lines.Ranges[Result] = iRange then
      Exit; // avoid the final Decrement
    }
    Lines.Ranges[Result] := iRange;
    Lines.ParenthesisLevels[Result] := fHighlighter.GetParenthesisLevel;
    Lines.BracketLevels[Result] := fHighlighter.GetBracketLevel;
    Lines.BraceLevels[Result] := fHighlighter.GetBraceLevel;
    Inc(Result);
  until (Result = Lines.Count);
  Dec(Result);
end;

procedure TCustomSynEdit.ListCleared(Sender: TObject);
begin
  if UseCodeFolding then
    FoldOnListCleared;
  ClearUndo;
  // invalidate the *whole* client area
  FillChar(fInvalidateRect, SizeOf(TRect), 0);
  Invalidate;
  // set caret and selected block to start of text
  CaretXY := BufferCoord(1, 1);
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
  Include(fStatusChanges, scAll);
end;

procedure TCustomSynEdit.ListDeleted(Sender: TObject; aIndex: Integer; aCount: integer);
begin
  if UseCodeFolding then
    FoldOnListDeleted(aIndex + 1, aCount);
  if Assigned(fHighlighter) and (Lines.Count > 0) then
    ScanFrom(aIndex);
  InvalidateLines(aIndex + 1, MaxInt);
  InvalidateGutterLines(aIndex + 1, MaxInt);
end;

procedure TCustomSynEdit.ListInserted(Sender: TObject; Index: Integer; aCount: integer);
var
  L: Integer;
  vLastScan: integer;
begin
  if UseCodeFolding then
    FoldOnListInserted(Index + 1, aCount);
  if Assigned(fHighlighter) and (Lines.Count > 0) then begin
    vLastScan := Index;
    repeat
      vLastScan := ScanFrom(vLastScan);
      Inc(vLastScan);
    until vLastScan >= Index + aCount;
  end;

  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);

  if (eoAutoSizeMaxScrollWidth in fOptions) then begin
    L := Lines.ExpandedStringLengths[Index];
    if L > MaxScrollWidth then
      MaxScrollWidth := L;
  end;
end;

procedure TCustomSynEdit.ListPutted(Sender: TObject; Index: Integer;
  aCount: integer);
var
  L: Integer;
  vEndLine: integer;
begin
  vEndLine := Index + 1;
  if Assigned(fHighlighter) then begin
    vEndLine := Max(vEndLine, ScanFrom(Index) + 1);
    // If this editor is chained then the real owner of text buffer will probably
    // have already parsed the changes, so ScanFrom will return immediately.
    if fLines <> fOrigLines then
      vEndLine := MaxInt;
  end;
  InvalidateLines(Index + 1, vEndLine);

  if (eoAutoSizeMaxScrollWidth in fOptions) then begin
    L := Lines.ExpandedStringLengths[Index];
    if L > MaxScrollWidth then
      MaxScrollWidth := L;
  end;
end;

procedure TCustomSynEdit.ScanRanges;
var
  i: integer;
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then begin
    fHighlighter.ResetRange;
    fHighlighter.ResetParenthesisLevel;
    fHighlighter.ResetBracketLevel;
    fHighlighter.ResetBraceLevel;
    i := 0;
    repeat
      fHighlighter.SetLine(Lines[i], i);
      fHighlighter.NextToEol;
      Lines.Ranges[i] := fHighlighter.GetRange;
      Lines.ParenthesisLevels[i] := fHighlighter.GetParenthesisLevel;
      Lines.BracketLevels[i] := fHighlighter.GetBracketLevel;
      Lines.BraceLevels[i] := fHighlighter.GetBraceLevel;
      Inc(i);
    until i >= Lines.Count;
  end;
end;

procedure TCustomSynEdit.SetWordBlock(Value: TBufferCoord);
var
  v_WordStart: TBufferCoord;
  v_WordEnd: TBufferCoord;
  TempString: string;
begin
  if (eoScrollPastEol in Options) then
    Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  TempString := Lines[Value.Line - 1] + #0; //needed for CaretX = LineLength +1
  if Value.Char > Length(TempString) then begin
    InternalCaretXY := BufferCoord(Length(TempString), Value.Line);
    exit;
  end;

  v_WordStart := WordStartEx(Value);
  v_WordEnd := WordEndEx(Value);
  if (v_WordStart.Line = v_WordEnd.Line) and (v_WordStart.Char < v_WordEnd.Char) then
    SetCaretAndSelection(v_WordEnd, v_WordStart, v_WordEnd);
end;

procedure TCustomSynEdit.DblClick;
var
  ptMouse: TPoint;
begin
  GetCursorPos(ptMouse);
  ptMouse := ScreenToClient(ptMouse);
  if ptMouse.X >= fGutterWidth + 2 then begin
    if not (eoNoSelection in fOptions) then
      SetWordBlock(CaretXY);
    inherited;
    Include(fStateFlags, sfDblClicked);
    MouseCapture := FALSE;
  end else
    inherited;
end;

function TCustomSynEdit.GetCanUndo: Boolean;
begin
  result := not ReadOnly and fUndoList.CanUndo;
end;

function TCustomSynEdit.GetCanRedo: Boolean;
begin
  result := not ReadOnly and fRedoList.CanUndo;
end;

function TCustomSynEdit.GetCanPaste;
begin
  Result := not ReadOnly and (Clipboard.HasFormat(CF_TEXT)
    or Clipboard.HasFormat(SynEditClipboardFormat));
end;

procedure TCustomSynEdit.InsertBlock(const BB, BE: TBufferCoord; ChangeStr: PChar;
  AddToUndoList: Boolean);
// used by BlockIndent and Redo
begin
  SetCaretAndSelection(BB, BB, BE);
  ActiveSelectionMode := smColumn;
  SetSelTextPrimitiveEx(smColumn, ChangeStr, AddToUndoList);
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.Redo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: integer;
  begin
    if fRedoList.LastChangeReason = crGroupBreak then begin
      OldBlockNumber := UndoList.BlockChangeNumber;
      Item := fRedoList.PopItem;
      try
        UndoList.BlockChangeNumber := Item.ChangeNumber;
        fUndoList.AddGroupBreak;
      finally
        UndoList.BlockChangeNumber := OldBlockNumber;
        Item.Free;
      end;
      UpdateModifiedStatus;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange: TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    exit;

  FLastChange := FRedoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteBegin;
  FPasteAction := FLastChange = crPasteBegin;
  FSpecial1 := FLastChange = crSpecial1Begin;
  FSpecial2 := FLastChange = crSpecial2Begin;

  Item := fRedoList.PeekItem;
  if Item <> nil then begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fUndoList.BlockChangeNumber;
    fUndoList.BlockChangeNumber := Item.ChangeNumber;
    try
      repeat
        RedoItem;
        Item := fRedoList.PeekItem;
        if Item = nil then
          FKeepGoing := False
        else begin
          if FAutoComplete then
            FKeepGoing := (FRedoList.LastChangeReason <> crAutoCompleteEnd)
          else if FPasteAction then
            FKeepGoing := (FRedoList.LastChangeReason <> crPasteEnd)
          else if FSpecial1 then
            FKeepGoing := (FRedoList.LastChangeReason <> crSpecial1End)
          else if FSpecial2 then
            FKeepGoing := (FRedoList.LastChangeReason <> crSpecial2End)
          else if Item.ChangeNumber = OldChangeNumber then
            FKeepGoing := True
          else begin
            FKeepGoing := ((eoGroupUndo in FOptions) and
              (FLastChange = Item.ChangeReason) and
              not (FLastChange in [crIndent, crUnindent]));
          end;
          FLastChange := Item.ChangeReason;
        end;
      until not (FKeepGoing);

      //we need to eat the last command since it does nothing and also update modified status...
      if (FAutoComplete and (FRedoList.LastChangeReason = crAutoCompleteEnd)) or
        (FPasteAction and (FRedoList.LastChangeReason = crPasteEnd)) or
        (FSpecial1 and (FRedoList.LastChangeReason = crSpecial1End)) or
        (FSpecial2 and (FRedoList.LastChangeReason = crSpecial2End)) then begin
        RedoItem;
        UpdateModifiedStatus;
      end;

    finally
      fUndoList.BlockChangeNumber := SaveChangeNumber;
    end;
    RemoveGroupBreak;
  end;
end;

procedure TCustomSynEdit.RedoItem;
var
  Item: TSynEditUndoItem;
  Run, StrToDelete: PChar;
  Len: integer;
  TempString: string;
  CaretPt: TBufferCoord;
  ChangeScrollPastEol: boolean;
  BeginX: integer;
begin
  ChangeScrollPastEol := not (eoScrollPastEol in Options);
  Item := fRedoList.PopItem;
  if Assigned(Item) then try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);
    fUndoList.InsideRedo := True;
    case Item.ChangeReason of
      crCaret: begin
          fUndoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, '', fActiveSelectionMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection: begin
          fUndoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, '', fActiveSelectionMode);
          SetCaretAndSelection(CaretXY, Item.ChangeStartPos, Item.ChangeEndPos);
        end;
      crInsert, crPaste, crDragDropInsert: begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeStartPos);
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PChar(Item.ChangeStr),
            False);
          InternalCaretXY := Item.ChangeEndPos;
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, SelText, Item.ChangeSelMode);
          if Item.ChangeReason = crDragDropInsert then begin
            SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;
        end;
      crDeleteAfterCursor, crSilentDeleteAfterCursor: begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TempString := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PChar(Item.ChangeStr),
            False);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TempString, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeEndPos;
        end;
      crDelete, crSilentDelete: begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TempString := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PChar(Item.ChangeStr),
            False);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TempString, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crLineBreak: begin
          CaretPt := Item.ChangeStartPos;
          SetCaretAndSelection(CaretPt, CaretPt, CaretPt);
          CommandProcessor(ecLineBreak, #13, nil);
        end;
      crIndent: begin
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
      crUnindent: begin // re-delete the (raggered) column
          // Delete string
          StrToDelete := PChar(Item.ChangeStr);
          InternalCaretY := Item.ChangeStartPos.Line;
          if Item.ChangeSelMode = smColumn then
            BeginX := Min(Item.ChangeStartPos.Char, Item.ChangeEndPos.Char)
          else
            BeginX := 1;
          repeat
            Run := GetEOL(StrToDelete);
            if Run <> StrToDelete then begin
              Len := Run - StrToDelete;
              if Len > 0 then begin
                TempString := Lines[CaretY - 1];
                Delete(TempString, BeginX, Len);
                Lines[CaretY - 1] := TempString;
              end;
            end else
              Len := 0;
            if Run^ = #13 then begin
              Inc(Run);
              if Run^ = #10 then
                Inc(Run);
              Inc(fCaretY);
            end;
            StrToDelete := Run;
          until Run^ = #0;
          if Item.ChangeSelMode = smColumn then
            SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
              Item.ChangeEndPos)
          else begin
            // restore selection
            CaretPt.Char := Item.ChangeStartPos.Char - fTabWidth;
            CaretPt.Line := Item.ChangeStartPos.Line;
            SetCaretAndSelection(CaretPt, CaretPt,
              BufferCoord(Item.ChangeEndPos.Char - Len, Item.ChangeEndPos.Line));
          end;
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
    end;
  finally
    fUndoList.InsideRedo := False;
    if ChangeScrollPastEol then
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.Undo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: integer;
  begin
    if fUndoList.LastChangeReason = crGroupBreak then begin
      OldBlockNumber := RedoList.BlockChangeNumber;
      try
        Item := fUndoList.PopItem;
        RedoList.BlockChangeNumber := Item.ChangeNumber;
        Item.Free;
        fRedoList.AddGroupBreak;
      finally
        RedoList.BlockChangeNumber := OldBlockNumber;
      end;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange: TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    exit;

  RemoveGroupBreak;

  FLastChange := FUndoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteEnd;
  FPasteAction := FLastChange = crPasteEnd;
  FSpecial1 := FLastChange = crSpecial1End;
  FSpecial2 := FLastChange = crSpecial2End;

  Item := fUndoList.PeekItem;
  if Item <> nil then begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fRedoList.BlockChangeNumber;
    fRedoList.BlockChangeNumber := Item.ChangeNumber;

    try
      repeat
        UndoItem;
        Item := fUndoList.PeekItem;
        if Item = nil then
          FKeepGoing := False
        else begin
          if FAutoComplete then
            FKeepGoing := (FUndoList.LastChangeReason <> crAutoCompleteBegin)
          else if FPasteAction then
            FKeepGoing := (FUndoList.LastChangeReason <> crPasteBegin)
          else if FSpecial1 then
            FKeepGoing := (FUndoList.LastChangeReason <> crSpecial1Begin)
          else if FSpecial2 then
            FKeepGoing := (FUndoList.LastChangeReason <> crSpecial2Begin)
          else if Item.ChangeNumber = OldChangeNumber then
            FKeepGoing := True
          else begin
            FKeepGoing := ((eoGroupUndo in FOptions) and
              (FLastChange = Item.ChangeReason) and
              not (FLastChange in [crIndent, crUnindent]));
          end;
          FLastChange := Item.ChangeReason;
        end;
      until not (FKeepGoing);

      //we need to eat the last command since it does nothing and also update modified status...
      if (FAutoComplete and (FUndoList.LastChangeReason = crAutoCompleteBegin)) or
        (FPasteAction and (FUndoList.LastChangeReason = crPasteBegin)) or
        (FSpecial1 and (FUndoList.LastChangeReason = crSpecial1Begin)) or
        (FSpecial2 and (FUndoList.LastChangeReason = crSpecial2Begin)) then begin
        UndoItem;
        UpdateModifiedStatus;
      end;

    finally
      fRedoList.BlockChangeNumber := SaveChangeNumber;
    end;
  end;
end;

procedure TCustomSynEdit.UndoItem;
var
  Item: TSynEditUndoItem;
  TmpPos: TBufferCoord;
  TmpStr: string;
  ChangeScrollPastEol: boolean;
  BeginX: integer;
begin
  fUndoing:=True;
  ChangeScrollPastEol := not (eoScrollPastEol in Options);
  Item := fUndoList.PopItem;
  if Assigned(Item) then try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);
    case Item.ChangeReason of
      crCaret: begin
          fRedoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, '', fActiveSelectionMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection: begin
          fRedoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, '', fActiveSelectionMode);
          SetCaretAndSelection(CaretXY, Item.ChangeStartPos, Item.ChangeEndPos);
        end;
      crInsert, crPaste, crDragDropInsert: begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TmpStr := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PChar(Item.ChangeStr),
            False);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TmpStr, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crDeleteAfterCursor, crDelete,
        crSilentDelete, crSilentDeleteAfterCursor,
        crDeleteAll: begin
          // If there's no selection, we have to set
          // the Caret's position manualy.
          if Item.ChangeSelMode = smColumn then
            TmpPos := BufferCoord(
              Min(Item.ChangeStartPos.Char, Item.ChangeEndPos.Char),
              Min(Item.ChangeStartPos.Line, Item.ChangeEndPos.Line))
          else
            TmpPos := TBufferCoord(MinPoint(
              TPoint(Item.ChangeStartPos), TPoint(Item.ChangeEndPos)));
          if (Item.ChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor]) and (TmpPos.Line > Lines.Count) then begin
            InternalCaretXY := BufferCoord(1, Lines.Count);
            fLines.Add('');
          end;
          CaretXY := TmpPos;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PChar(Item.ChangeStr),
            False);
          if Item.ChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor] then
            TmpPos := Item.ChangeStartPos
          else
            TmpPos := Item.ChangeEndPos;
          if Item.ChangeReason in [crSilentDelete, crSilentDeleteAfterCursor] then
            InternalCaretXY := TmpPos
          else begin
            SetCaretAndSelection(TmpPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
          if Item.ChangeReason = crDeleteAll then begin
            InternalCaretXY := BufferCoord(1, 1);
            fBlockEnd := BufferCoord(1, 1);
          end;
          EnsureCursorPosVisible;
        end;
      crLineBreak: begin
          // If there's no selection, we have to set
          // the Caret's position manualy.
          InternalCaretXY := Item.ChangeStartPos;
          if CaretY > 0 then begin
            TmpStr := Lines.Strings[CaretY - 1];
            if (Length(TmpStr) < CaretX - 1)
              and (LeftSpaces(Item.ChangeStr) = 0) then
              TmpStr := TmpStr + StringOfChar(#32, CaretX - 1 - Length(TmpStr));
            ProperSetLine(CaretY - 1, TmpStr + Item.ChangeStr);
            Lines.Delete(Item.ChangeEndPos.Line);
          end else
            ProperSetLine(CaretY - 1, Item.ChangeStr);
          DoLinesDeleted(CaretY + 1, 1);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
        end;
      crIndent: begin
          // restore the selection
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
      crUnindent: {// reinsert the (raggered) column that was deleted} begin
          // reinsert the string
          if Item.ChangeSelMode <> smColumn then
            InsertBlock(BufferCoord(1, Item.ChangeStartPos.Line),
              BufferCoord(1, Item.ChangeEndPos.Line),
              PChar(Item.ChangeStr), False)
          else begin
            BeginX := Min(Item.ChangeStartPos.Char, Item.ChangeEndPos.Char);
            InsertBlock(BufferCoord(BeginX, Item.ChangeStartPos.Line),
              BufferCoord(BeginX, Item.ChangeEndPos.Line),
              PChar(Item.ChangeStr), False);
          end;
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
    end;
  finally
    fUndoing:=False;
    if ChangeScrollPastEol then
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark]) then begin
    DoOnClearBookmark(fBookMarks[BookMark]);
    FMarkList.Remove(fBookMarks[Bookmark]);
    fBookMarks[BookMark] := nil;
  end
end;

procedure TCustomSynEdit.GotoBookMark(BookMark: Integer);
var
  iNewPos: TBufferCoord;
begin
  if (BookMark in [0..9]) and Assigned(fBookMarks[BookMark]) and (fBookMarks[BookMark].Line <= fLines.Count) then begin
    iNewPos.Char := fBookMarks[BookMark].Char;
    iNewPos.Line := fBookMarks[BookMark].Line;
    SetCaretXYCentered(True, iNewPos);
  end;
end;

procedure TCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  mark: TSynEditMark;
begin
  if (BookMark in [0..9]) and (Y >= 1) and (Y <= Max(1, fLines.Count)) then begin
    mark := TSynEditMark.Create();
    with mark do begin
      Line := Y;
      Char := X;
      ImageIndex := Bookmark;
      BookmarkNumber := Bookmark;
      Visible := true;
      InternalImage := (fBookMarkOpt.BookmarkImages = nil);
    end;
    DoOnPlaceMark(Mark);
    if (mark <> nil) then begin
      if assigned(fBookMarks[BookMark]) then
        ClearBookmark(BookMark);
      fBookMarks[BookMark] := mark;
      FMarkList.Add(fBookMarks[BookMark]);
    end;
  end;
end;

procedure TCustomSynEdit.WndProc(var Msg: TMessage);
// Prevent Alt-Backspace from beeping
const
  ALT_KEY_DOWN = $20000000;
var
  keyMsg : TWMKey;
  code: word;
//  temp: char;
begin
  if (Msg.Msg = CN_KEYDOWN)
    and ((GetKeyState(VK_SHIFT) and $8000 )=0)
    and ((GetKeyState(VK_MENU) and $8000 )=0)
    and ((GetKeyState(VK_CONTROL) and $8000 )=0) then begin
    keyMsg := TWMKey(Msg);
    code := keyMsg.CharCode;
    case code of
      VK_TAB: begin
        Exit; // must discard the first TAB CN_KEYDOWN message, don't know why?
      end;
    end;
  end else if (Msg.Msg = WM_SYSCHAR) and (Msg.wParam = VK_BACK) and
    (Msg.lParam and ALT_KEY_DOWN <> 0) then begin
    Msg.Msg := 0 ;
    Exit;
  end;
  inherited;
end;

procedure TCustomSynEdit.ChainListCleared(Sender: TObject);
begin
  if Assigned(fChainListCleared) then
    fChainListCleared(Sender);
  Lines.OnCleared(Sender);
end;

procedure TCustomSynEdit.ChainListDeleted(Sender: TObject; aIndex: integer;
  aCount: integer);
begin
  if Assigned(fChainListDeleted) then
    fChainListDeleted(Sender, aIndex, aCount);
  fOrigLines.OnDeleted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListInserted(Sender: TObject; aIndex: integer;
  aCount: integer);
begin
  if Assigned(fChainListInserted) then
    fChainListInserted(Sender, aIndex, aCount);
  fOrigLines.OnInserted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListPutted(Sender: TObject; aIndex: integer;
  aCount: integer);
begin
  if Assigned(fChainListPutted) then
    fChainListPutted(Sender, aIndex, aCount);
  fOrigLines.OnPutted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainLinesChanging(Sender: TObject);
begin
  if Assigned(fChainLinesChanging) then
    fChainLinesChanging(Sender);
  fOrigLines.OnChanging(Sender);
end;

procedure TCustomSynEdit.ChainLinesChanged(Sender: TObject);
begin
  if Assigned(fChainLinesChanged) then
    fChainLinesChanged(Sender);
  fOrigLines.OnChange(Sender);
end;

procedure TCustomSynEdit.ChainUndoRedoAdded(Sender: TObject);
var
  iList: TSynEditUndoList;
  iHandler: TNotifyEvent;
begin
  if Sender = fUndoList then begin
    iList := fOrigUndoList;
    iHandler := fChainUndoAdded;
  end else { if Sender = fRedoList then } begin
    iList := fOrigRedoList;
    iHandler := fChainRedoAdded;
  end;
  if Assigned(iHandler) then
    iHandler(Sender);
  iList.OnAddedUndo(Sender);
end;

procedure TCustomSynEdit.UnHookTextBuffer;
begin
  Assert(fChainedEditor = nil);
  if fLines = fOrigLines then
    Exit;

  //first put back the real methods
  with fLines do begin
    OnCleared := fChainListCleared;
    OnDeleted := fChainListDeleted;
    OnInserted := fChainListInserted;
    OnPutted := fChainListPutted;
    OnChanging := fChainLinesChanging;
    OnChange := fChainLinesChanged;
  end;
  fUndoList.OnAddedUndo := fChainUndoAdded;
  fRedoList.OnAddedUndo := fChainRedoAdded;

  fChainListCleared := nil;
  fChainListDeleted := nil;
  fChainListInserted := nil;
  fChainListPutted := nil;
  fChainLinesChanging := nil;
  fChainLinesChanged := nil;
  fChainUndoAdded := nil;

  //make the switch
  fLines := fOrigLines;
  fUndoList := fOrigUndoList;
  fRedoList := fOrigRedoList;
  LinesHookChanged;
end;

function TCustomSynEdit.IsEmpty: boolean;
var
  i: integer;
begin
  Result := true;
  for i := 0 to fLines.Count - 1 do begin
    if Length(fLines[i]) > 0 then begin
      Result := false;
      break;
    end;
  end;
end;

procedure TCustomSynEdit.HookTextBuffer(aBuffer: TSynEditStringList; aUndo, aRedo: TSynEditUndoList);
begin
  Assert(fChainedEditor = nil);
  Assert(fLines = fOrigLines);

  if fChainedEditor <> nil then
    RemoveLinesPointer
  else if fLines <> fOrigLines then
    UnHookTextBuffer;

  //store the current values and put in the chained methods
  fChainListCleared := aBuffer.OnCleared;
  aBuffer.OnCleared := ChainListCleared;
  fChainListDeleted := aBuffer.OnDeleted;
  aBuffer.OnDeleted := ChainListDeleted;
  fChainListInserted := aBuffer.OnInserted;
  aBuffer.OnInserted := ChainListInserted;
  fChainListPutted := aBuffer.OnPutted;
  aBuffer.OnPutted := ChainListPutted;
  fChainLinesChanging := aBuffer.OnChanging;
  aBuffer.OnChanging := ChainLinesChanging;
  fChainLinesChanged := aBuffer.OnChange;
  aBuffer.OnChange := ChainLinesChanged;

  fChainUndoAdded := aUndo.OnAddedUndo;
  aUndo.OnAddedUndo := ChainUndoRedoAdded;
  fChainRedoAdded := aRedo.OnAddedUndo;
  aRedo.OnAddedUndo := ChainUndoRedoAdded;

  //make the switch
  fLines := aBuffer;
  fUndoList := aUndo;
  fRedoList := aRedo;
  LinesHookChanged;
end;

procedure TCustomSynEdit.LinesHookChanged;
var
  iLongestLineLength: integer;
begin
  Invalidate;
  if eoAutoSizeMaxScrollWidth in fOptions then begin
    iLongestLineLength := Lines.LengthOfLongestLine;
    if iLongestLineLength > MaxScrollWidth then
      MaxScrollWidth := iLongestLineLength;
  end;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.SetLinesPointer(ASynEdit: TCustomSynEdit);
begin
  HookTextBuffer(ASynEdit.Lines,
    ASynEdit.UndoList, ASynEdit.RedoList);

  fChainedEditor := ASynEdit;
  ASynEdit.FreeNotification(Self);
end;

procedure TCustomSynEdit.RemoveLinesPointer;
begin
  if Assigned(fChainedEditor) then
    RemoveFreeNotification(fChainedEditor);
  fChainedEditor := nil;

  UnHookTextBuffer;
end;

procedure TCustomSynEdit.DragCanceled;
begin
  fScrollTimer.Enabled := False;
  inherited;
end;

procedure TCustomSynEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  vNewPos: TDisplayCoord;
begin
  inherited;
  if (Source is TCustomSynEdit) and not ReadOnly then begin
    Accept := True;
    //Ctrl is pressed => change cursor to indicate copy instead of move
    if GetKeyState(VK_CONTROL) < 0 then
      DragCursor := crMultiDrag
    else
      DragCursor := crDrag;
    if Dragging then {//if the drag source is the SynEdit itself} begin
      if State = dsDragLeave then //restore prev caret position
        ComputeCaret(FMouseDownX, FMouseDownY)
      else begin
        vNewPos := PixelsToNearestRowColumn(X, Y);
        vNewPos.Column := MinMax(vNewPos.Column, LeftChar, LeftChar + CharsInWindow - 1);
        vNewPos.Row := MinMax(vNewPos.Row, TopLine, TopLine + LinesInWindow - 1);
        InternalCaretXY := DisplayToBufferPos(vNewPos);
        ComputeScroll(X, Y);
      end;
    end else //if is dragging from another SynEdit
      ComputeCaret(X, Y); //position caret under the mouse cursor
  end;
end;

procedure TCustomSynEdit.DragDrop(Source: TObject; X, Y: Integer);
var
  vNewCaret: TBufferCoord;
  DoDrop, DropAfter, DropMove: boolean;
  vBB, vBE: TBufferCoord;
  DragDropText: string;
  ChangeScrollPastEOL: boolean;
begin
  if not ReadOnly and (Source is TCustomSynEdit)
    and TCustomSynEdit(Source).SelAvail then begin
    IncPaintLock;
    try
      inherited;
      ComputeCaret(X, Y);
      vNewCaret := CaretXY;
      // if from other control then move when SHIFT, else copy
      // if from Self then copy when CTRL, else move
      if Source <> Self then begin
        DropMove := GetKeyState(VK_SHIFT) < 0;
        DoDrop := TRUE;
        DropAfter := FALSE;
      end else begin
        DropMove := GetKeyState(VK_CONTROL) >= 0;
        vBB := BlockBegin;
        vBE := BlockEnd;
        DropAfter := (vNewCaret.Line > vBE.Line)
          or ((vNewCaret.Line = vBE.Line) and ((vNewCaret.Char > vBE.Char) or
          ((not DropMove) and (vNewCaret.Char = vBE.Char))));
        DoDrop := DropAfter or (vNewCaret.Line < vBB.Line)
          or ((vNewCaret.Line = vBB.Line) and ((vNewCaret.Char < vBB.Char) or
          ((not DropMove) and (vNewCaret.Char = vBB.Char))));
      end;
      if DoDrop then begin
        BeginUndoBlock;
        try
          DragDropText := TCustomSynEdit(Source).SelText;
          // delete the selected text if necessary
          if DropMove then begin
            TCustomSynEdit(Source).SelText := '';
            if (Source = Self) and DropAfter then begin
              // adjust horizontal drop position
              if vNewCaret.Line = vBE.Line then
                Dec(vNewCaret.Char, vBE.Char - vBB.Char);
              // adjust vertical drop position
              if vBE.Line > vBB.Line then
                Dec(vNewCaret.Line, vBE.Line - vBB.Line);
            end;
          end;
          //todo: this is probably already done inside SelText
          // insert the selected text
          ChangeScrollPastEOL := not (eoScrollPastEol in fOptions);
          try
            if ChangeScrollPastEOL then
              Include(fOptions, eoScrollPastEol);
            InternalCaretXY := vNewCaret;
            BlockBegin := vNewCaret;
            { Add the text. Undo is locked so the action is recorded as crDragDropInsert
            instead of crInsert (code right bellow). }
            Assert(not SelAvail);
            LockUndo;
            try
              SelText := DragDropText;
            finally
              UnlockUndo;
            end;
          finally
            if ChangeScrollPastEOL then
              Exclude(fOptions, eoScrollPastEol);
          end;
          // save undo information
          if Source = Self then begin
            fUndoList.AddChange(crDragDropInsert, vNewCaret, BlockEnd, SelText,
              fActiveSelectionMode);
          end else begin
            fUndoList.AddChange(crInsert, vNewCaret, BlockEnd,
              SelText, fActiveSelectionMode);
          end;
          BlockEnd := CaretXY;
          CommandProcessor(ecSelGotoXY, #0, @vNewCaret);
        finally
          EndUndoBlock;
        end;
      end;
    finally
      DecPaintLock;
    end;
  end else
    inherited;
end;

procedure TCustomSynEdit.SetRightEdge(Value: Integer);
begin
  if fRightEdge <> Value then begin
    fRightEdge := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetRightEdgeColor(Value: TColor);
var
  nX: integer;
  rcInval: TRect;
begin
  if fRightEdgeColor <> Value then begin
    fRightEdgeColor := Value;
    if HandleAllocated then begin
      nX := fTextOffset + fRightEdge * fCharWidth;
      rcInval := Rect(nX - 1, 0, nX + 1, Height);
      InvalidateRect(rcInval, FALSE);
    end;
  end;
end;

function TCustomSynEdit.GetMaxUndo: Integer;
begin
  result := fUndoList.MaxUndoActions;
end;

procedure TCustomSynEdit.SetMaxUndo(const Value: Integer);
begin
  if Value > -1 then begin
    fUndoList.MaxUndoActions := Value;
    fRedoList.MaxUndoActions := Value;
  end;
end;

procedure TCustomSynEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = fSearchEngine then begin
      SearchEngine := nil;
    end;

    if AComponent = fHighlighter then begin
      Highlighter := nil;
    end;

    if AComponent = fChainedEditor then begin
      RemoveLinesPointer;
    end;

    if (fBookmarkOpt <> nil) then
      if (AComponent = fBookmarkOpt.BookmarkImages) then begin
        fBookmarkOpt.BookmarkImages := nil;
        InvalidateGutterLines(-1, -1);
      end;
  end;
end;

procedure TCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  if Value <> fHighlighter then begin
    if Assigned(fHighlighter) then begin
      fHighlighter.UnhookAttrChangeEvent(HighlighterAttrChanged);
      fHighlighter.RemoveFreeNotification(Self);
    end;
    if Assigned(Value) then begin
      Value.HookAttrChangeEvent(HighlighterAttrChanged);
      Value.FreeNotification(Self);
    end;
    fHighlighter := Value;
    if not (csDestroying in ComponentState) then
      HighlighterAttrChanged(fHighlighter);
  end;
end;

procedure TCustomSynEdit.SetBorderStyle(Value: TSynBorderStyle);
begin
  if fBorderStyle <> Value then begin
    fBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomSynEdit.SetHideSelection(const Value: boolean);
begin
  if fHideSelection <> Value then begin
    FHideSelection := Value;
    InvalidateSelection;
  end;
end;

procedure TCustomSynEdit.SetInsertMode(const Value: boolean);
begin
  if fInserting <> Value then begin
    fInserting := Value;
    if not (csDesigning in ComponentState) then
      // Reset the caret.
      InitializeCaret;
    StatusChanged([scInsertMode]);
  end;
end;

procedure TCustomSynEdit.InitializeCaret;
var
  ct: TSynEditCaretType;
  cw, ch: integer;
begin
  // CreateCaret automatically destroys the previous one, so we don't have to
  // worry about cleaning up the old one here with DestroyCaret.
  // Ideally, we will have properties that control what these two carets look like.
  if InsertMode then
    ct := FInsertCaret
  else
    ct := FOverwriteCaret;
  case ct of
    ctHorizontalLine: begin
        cw := fCharWidth;
        ch := 2;
        FCaretOffset := Point(0, fTextHeight - 2);
      end;
    ctHalfBlock: begin
        cw := fCharWidth;
        ch := (fTextHeight - 2) div 2;
        FCaretOffset := Point(0, ch);
      end;
    ctBlock: begin
        cw := fCharWidth;
        ch := fTextHeight - 2;
        FCaretOffset := Point(0, 0);
      end;
  else begin // ctVerticalLine
      cw := 2;
      ch := fTextHeight - 2;
      FCaretOffset := Point(-1, 0);
    end;
  end;
  Exclude(fStateFlags, sfCaretVisible);

  if Focused or FAlwaysShowCaret then begin
    CreateCaret(Handle, 0, cw, ch);
    UpdateCaret;
  end;
end;

procedure TCustomSynEdit.SetInsertCaret(const Value: TSynEditCaretType);
begin
  if FInsertCaret <> Value then begin
    FInsertCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetOverwriteCaret(const Value: TSynEditCaretType);
begin
  if FOverwriteCaret <> Value then begin
    FOverwriteCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetMaxScrollWidth(Value: integer);
begin
  Value := MinMax(Value, 1, MaxInt - 1);
  if MaxScrollWidth <> Value then begin
    fMaxScrollWidth := Value;
    if eoScrollPastEol in Options then
      UpdateScrollBars;
  end;
end;

procedure TCustomSynEdit.EnsureCursorPosVisible;
begin
  EnsureCursorPosVisibleEx(False);
end;

procedure TCustomSynEdit.EnsureCursorPosVisibleEx(ForceToMiddle: Boolean);
var
  VisibleX: Integer;
  vCaretRow: integer;
begin
  IncPaintLock;
  try
    // Make sure X is visible
    VisibleX := DisplayX;
    if VisibleX < LeftChar then
      LeftChar := VisibleX
    else if (VisibleX >= CharsInWindow + LeftChar) and (CharsInWindow > 0) then
      LeftChar := VisibleX - CharsInWindow + 1
    else
      LeftChar := LeftChar;

    // Make sure Y is visible
    vCaretRow := DisplayY;
    if ForceToMiddle then begin
      if (vCaretRow < TopLine) or (vCaretRow>(TopLine + (LinesInWindow - 1))) then
        TopLine := vCaretRow - (LinesInWindow - 1) div 2;
    end else begin
      if vCaretRow < TopLine then
        TopLine := vCaretRow
      else if (vCaretRow > TopLine + (LinesInWindow - 1)) and (LinesInWindow > 0) then
        TopLine := vCaretRow - (LinesInWindow - 1)
      else
        TopLine := TopLine;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetKeystrokes(const Value: TSynEditKeyStrokes);
begin
  if Value = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(Value);
end;

procedure TCustomSynEdit.SetDefaultKeystrokes;
begin
  FKeystrokes.ResetDefaults;
end;

// If the translations requires Data, memory will be allocated for it via a
// GetMem call.  The client must call FreeMem on Data if it is not NIL.

function TCustomSynEdit.TranslateKeyCode(Code: word; Shift: TShiftState;
  var Data: pointer): TSynEditorCommand;
var
  i: integer;
begin
  i := KeyStrokes.FindKeycode2(fLastKey, fLastShiftState, Code, Shift);
  if i >= 0 then
    Result := KeyStrokes[i].Command
  else begin
    i := Keystrokes.FindKeycode(Code, Shift);
    if i >= 0 then
      Result := Keystrokes[i].Command
    else
      Result := ecNone;
  end;
  if (Result = ecNone) and (Code >= VK_ACCEPT) and (Code <= VK_SCROLL) then
  begin
    fLastKey := Code;
    fLastShiftState := Shift;
  end else begin
    fLastKey := 0;
    fLastShiftState := [];
  end;
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: char; Data: pointer);
begin
  // first the program event handler gets a chance to process the command
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then begin
    // notify hooked command handlers before the command is executed inside of
    // the class
    NotifyHookedCommandHandlers(FALSE, Command, AChar, Data);
    // internal command handler
    if (Command <> ecNone) and (Command < ecUserFirst) then
      ExecuteCommand(Command, AChar, Data);
    // notify hooked command handlers after the command was executed inside of
    // the class
    if Command <> ecNone then
      NotifyHookedCommandHandlers(TRUE, Command, AChar, Data);
  end;
  DoOnCommandProcessed(Command, AChar, Data);
end;

procedure TCustomSynEdit.ExecuteCommand(Command: TSynEditorCommand; AChar: char;
  Data: pointer);

  procedure SetSelectedTextEmpty;
  var
    vSelText: string;
    vUndoBegin, vUndoEnd: TBufferCoord;
  begin
    vUndoBegin := fBlockBegin;
    vUndoEnd := fBlockEnd;
    vSelText := SelText;
    SetSelTextPrimitive('');
    if (vUndoBegin.Line < vUndoEnd.Line) or (
      (vUndoBegin.Line = vUndoEnd.Line) and (vUndoBegin.Char < vUndoEnd.Char)) then begin
      fUndoList.AddChange(crDelete, vUndoBegin, vUndoEnd, vSelText,
        fActiveSelectionMode);
    end else begin
      fUndoList.AddChange(crDeleteAfterCursor, vUndoBegin, vUndoEnd, vSelText,
        fActiveSelectionMode);
    end;
  end;

  procedure ForceCaretX(aCaretX: integer);
  var
    vRestoreScroll: boolean;
  begin
    vRestoreScroll := not (eoScrollPastEol in fOptions);
    Include(fOptions, eoScrollPastEol);
    try
      InternalCaretX := aCaretX;
    finally
      if vRestoreScroll then
        Exclude(fOptions, eoScrollPastEol);
    end;
  end;

var
  CX: Integer;
  Len: Integer;
  Temp: string;
  Temp2: string;
  Temp3: AnsiString;
  helper: string;
  TabBuffer: string;
  SpaceBuffer: string;
  SpaceCount1: Integer;
  SpaceCount2: Integer;
  BackCounter: Integer;
  OrigBlockBegin: TBufferCoord;
  OrigBlockEnd: TBufferCoord;
  OrigCaret: TBufferCoord;
  MoveDelim: TBufferCoord;
  BeginIndex: integer;
  EndIndex: integer;
  StartOfBlock: TBufferCoord;
  CurrentBracketPos, MatchBracketPos: TBufferCoord;
  bChangeScroll: boolean;
  moveBkm: boolean;
  WP: TBufferCoord;
  Caret: TBufferCoord;
  CaretNew: TBufferCoord;
  i, j: integer;
{$IFDEF SYN_MBCSSUPPORT}
  s: string;
{$ENDIF}
  counter: Integer;
  InsDelta: integer;
  iUndoBegin, iUndoEnd: TBufferCoord;
  vCaretRow: integer;
  vTabTrim: integer;
  Attr: TSynHighlighterAttributes;
  StartPos: Integer;
  EndPos: Integer;
  tempStr : AnsiString;
  nLinesInserted: integer;
begin
  IncPaintLock;
  try
    case Command of
      // horizontal caret movement or selection
      ecLeft, ecSelLeft:
        MoveCaretHorz(-1, Command = ecSelLeft);
      ecRight, ecSelRight:
        MoveCaretHorz(1, Command = ecSelRight);
      ecPageLeft, ecSelPageLeft:
        MoveCaretHorz(-CharsInWindow, Command = ecSelPageLeft);
      ecPageRight, ecSelPageRight:
        MoveCaretHorz(CharsInWindow, Command = ecSelPageRight);
      ecLineStart, ecSelLineStart: begin
          DoHomeKey(Command = ecSelLineStart);
        end;
      ecLineEnd, ecSelLineEnd:
        DoEndKey(Command = ecSelLineEnd);
      // vertical caret movement or selection
      ecUp, ecSelUp: begin
          MoveCaretVert(-1, Command = ecSelUp);
          Update;
        end;
      ecDown, ecSelDown: begin
          MoveCaretVert(1, Command = ecSelDown);
          Update;
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown: begin
          counter := fLinesInWindow shr Ord(eoHalfPageScroll in fOptions);
          if eoScrollByOneLess in fOptions then
            Dec(counter);
          if (Command in [ecPageUp, ecSelPageUp]) then
            counter := -counter;
          TopLine := TopLine + counter;
          MoveCaretVert(counter, Command in [ecSelPageUp, ecSelPageDown]);
          Update;
        end;
      ecPageTop, ecSelPageTop: begin
          CaretNew := DisplayToBufferPos(
            DisplayCoord(DisplayX, TopLine));
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageTop);
          Update;
        end;
      ecPageBottom, ecSelPageBottom: begin
          CaretNew := DisplayToBufferPos(
            DisplayCoord(DisplayX, TopLine + LinesInWindow - 1));
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageBottom);
          Update;
        end;
      ecEditorTop, ecSelEditorTop: begin
          CaretNew.Char := 1;
          CaretNew.Line := 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorTop);
          Update;
        end;
      ecEditorBottom, ecSelEditorBottom: begin
          CaretNew.Char := 1;
          CaretNew.Line := Lines.Count;
          if (CaretNew.Line > 0) then
            CaretNew.Char := Length(Lines[CaretNew.Line - 1]) + 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorBottom);
          Update;
        end;
      // goto special line / column position
      ecGotoXY, ecSelGotoXY:
        if Assigned(Data) then begin
          MoveCaretAndSelection(CaretXY, TBufferCoord(Data^), Command = ecSelGotoXY);
          Update;
        end;
      // word selection
      ecWordLeft, ecSelWordLeft: begin
          CaretNew := PrevWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordLeft);
        end;
      ecWordRight, ecSelWordRight: begin
          CaretNew := NextWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordRight);
        end;
      ecSelWord: begin
          SetSelWord;
        end;
      ecSelectAll: begin
          SelectAll;
        end;
      ecDeleteLastChar:
        if not ReadOnly then begin
          DoOnPaintTransientEx(ttBefore, true);
          try
            if SelAvail then
              SetSelectedTextEmpty
            else begin
              Temp := LineText;
              TabBuffer := Lines.ExpandedStrings[CaretY - 1];
              Len := Length(Temp);
              Caret := CaretXY;
              vTabTrim := 0;
              if CaretX > Len + 1 then begin
                helper := '';
                if eoSmartTabDelete in fOptions then begin
                  //It's at the end of the line, move it to the length
                  if Len > 0 then
                    InternalCaretX := Len + 1
                  else begin
                    //move it as if there were normal spaces there
                    SpaceCount1 := CaretX - 1;
                    SpaceCount2 := 0;
                    // unindent
                    if SpaceCount1 > 0 then begin
                      BackCounter := CaretY - 2;
                      //It's better not to have if statement inside loop
                      if (eoTrimTrailingSpaces in Options) and (Len = 0) then
                        while BackCounter >= 0 do begin
                          SpaceCount2 := LeftSpacesEx(Lines[BackCounter], True);
                          if SpaceCount2 < SpaceCount1 then
                            break;
                          Dec(BackCounter);
                        end else
                        while BackCounter >= 0 do begin
                          SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                          if SpaceCount2 < SpaceCount1 then
                            break;
                          Dec(BackCounter);
                        end;
                      if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                        SpaceCount2 := 0;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                    UpdateLastCaretX;
                    fStateFlags := fStateFlags + [sfCaretChanged];
                    StatusChanged([scCaretX]);
                  end;
                end else begin
                  // only move caret one column
                  InternalCaretX := CaretX - 1;
                end;
              end else if CaretX = 1 then begin
                // join this line with the last line if possible
                if CaretY > 1 then begin
                  InternalCaretY := CaretY - 1;
                  InternalCaretX := Length(Lines[CaretY - 1]) + 1;
                  Lines.Delete(CaretY);
                  DoLinesDeleted(CaretY+1, 1);
                  if eoTrimTrailingSpaces in Options then
                    Temp := TrimTrailingSpaces(Temp);

                  LineText := LineText + Temp;
                  helper := #13#10;
                end;
              end else begin
                // delete text before the caret
                SpaceCount1 := LeftSpaces(Temp);
                SpaceCount2 := 0;
                if (Temp[CaretX - 1] <= #32) and (SpaceCount1 = CaretX - 1) then begin
                  if eoSmartTabDelete in fOptions then begin
                    // unindent
                    if SpaceCount1 > 0 then begin
                      BackCounter := CaretY - 2;
                      while BackCounter >= 0 do begin
                        SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                        if SpaceCount2 < SpaceCount1 then
                          break;
                        Dec(BackCounter);
                      end;
                      if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                        SpaceCount2 := 0;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    helper := Copy(Temp, 1, SpaceCount1 - SpaceCount2);
                    Delete(Temp, 1, SpaceCount1 - SpaceCount2);
                  end else begin
                    SpaceCount2 := SpaceCount1;
                    //how much till the next tab column
                    BackCounter := (DisplayX - 1) mod FTabWidth;
                    if BackCounter = 0 then
                      BackCounter := FTabWidth;

                    SpaceCount1 := 0;
                    CX := DisplayX - BackCounter;
                    while (SpaceCount1 < FTabWidth) and
                      (SpaceCount1 < BackCounter) and
                      (TabBuffer[CX] <> #9) do begin
                      Inc(SpaceCount1);
                      Inc(CX);
                    end;
{$IFOPT R+}
                    // Avoids an exception when compiled with $R+.
                    // 'CX' can be 'Length(TabBuffer)+1', which isn't an AV and evaluates
                    //to #0. But when compiled with $R+, Delphi raises an Exception.
                    if CX <= Length(TabBuffer) then
{$ENDIF}
                      if TabBuffer[CX] = #9 then
                        SpaceCount1 := SpaceCount1 + 1;

                    if SpaceCount2 = SpaceCount1 then begin
                      helper := Copy(Temp, 1, SpaceCount1);
                      Delete(Temp, 1, SpaceCount1);
                    end else begin
                      helper := Copy(Temp, SpaceCount2 - SpaceCount1 + 1, SpaceCount1);
                      Delete(Temp, SpaceCount2 - SpaceCount1 + 1, SpaceCount1);
                    end;
                    SpaceCount2 := 0;
                  end;
                  ProperSetLine(CaretY - 1, Temp);
                  fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                  UpdateLastCaretX;
                  fStateFlags := fStateFlags + [sfCaretChanged];
                  StatusChanged([scCaretX]);
                end else begin
                  // delete char
                  counter := 1;
{$IFDEF SYN_MBCSSUPPORT}
                  if (CaretX >= 3) and (ByteType(Temp, CaretX - 2) = mbLeadByte) then
                    Inc(counter);
{$ENDIF}
                  InternalCaretX := CaretX - counter;
                  // Stores the previous "expanded" CaretX if the line contains tabs.
                  if (eoTrimTrailingSpaces in Options) and (Len <> Length(TabBuffer)) then
                    vTabTrim := CharIndex2CaretPos(CaretX, TabWidth, Temp);
                  helper := Copy(Temp, CaretX, counter);
                  Delete(Temp, CaretX, counter);
                  ProperSetLine(CaretY - 1, Temp);
                  // Calculates a delta to CaretX to compensate for trimmed tabs.
                  if vTabTrim <> 0 then
                    if Length(Temp) <> Length(LineText) then
                      Dec(vTabTrim, CharIndex2CaretPos(CaretX, TabWidth, LineText))
                    else
                      vTabTrim := 0;
                end;
              end;
              if (Caret.Char <> CaretX) or (Caret.Line <> CaretY) then begin
                fUndoList.AddChange(crSilentDelete, CaretXY, Caret, helper,
                  smNormal);
                if vTabTrim <> 0 then
                  ForceCaretX(CaretX + vTabTrim);
              end;
            end;
            EnsureCursorPosVisible;
          finally
            DoOnPaintTransientEx(ttAfter, true);
          end;
        end;
      ecDeleteChar:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);

          if SelAvail then
            SetSelectedTextEmpty
          else begin
            // Call UpdateLastCaretX. Even though the caret doesn't move, the
            // current caret position should "stick" whenever text is modified.
            UpdateLastCaretX;
            Temp := LineText;
            Len := Length(Temp);
            if CaretX <= Len then begin
              // delete char
              counter := 1;
{$IFDEF SYN_MBCSSUPPORT}
              if ByteType(Temp, CaretX) = mbLeadByte then
                Inc(counter);
{$ENDIF}
              helper := Copy(Temp, CaretX, counter);
              Caret.Char := CaretX + counter;
              Caret.Line := CaretY;
              Delete(Temp, CaretX, counter);
              ProperSetLine(CaretY - 1, Temp);
            end else begin
              // join line with the line after
              if CaretY < Lines.Count then begin
                helper := StringOfChar(#32, CaretX - 1 - Len);
                ProperSetLine(CaretY - 1, Temp + helper + Lines[CaretY]);
                Caret.Char := 1;
                Caret.Line := CaretY + 1;
                helper := #13#10;
                Lines.Delete(CaretY);
                if CaretX=1 then
                  DoLinesDeleted(CaretY, 1)
                else
                  DoLinesDeleted(CaretY + 1, 1);
              end;
            end;
            if (Caret.Char <> CaretX) or (Caret.Line <> CaretY) then begin
              fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, Caret,
                helper, smNormal);
            end;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);
          Len := Length(LineText);
          if Command = ecDeleteWord then begin
            WP := WordEnd;
            Temp := LineText;
            if (WP.Char < CaretX) or ((WP.Char = CaretX) and (WP.Line < fLines.Count)) then begin
              if WP.Char > Len then begin
                Inc(WP.Line);
                WP.Char := 1;
                Temp := Lines[WP.Line - 1];
              end else if Temp[WP.Char] <> #32 then
                Inc(WP.Char);
            end;
{$IFOPT R+}
            Temp := Temp + #0;
{$ENDIF}
            if Temp <> '' then
              while Temp[WP.Char] = #32 do
                Inc(WP.Char);
          end else begin
            WP.Char := Len + 1;
            WP.Line := CaretY;
          end;
          if (WP.Char <> CaretX) or (WP.Line <> CaretY) then begin
            SetBlockBegin(CaretXY);
            SetBlockEnd(WP);
            ActiveSelectionMode := smNormal;
            helper := SelText;
            SetSelTextPrimitive(StringOfChar(' ', CaretX - BlockBegin.Char));
            fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, WP,
              helper, smNormal);
            InternalCaretXY := CaretXY;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecDeleteLastWord, ecDeleteBOL:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);
          if Command = ecDeleteLastWord then
            WP := PrevWordPos
          else begin
            WP.Char := 1;
            WP.Line := CaretY;
          end;
          if (WP.Char <> CaretX) or (WP.Line <> CaretY) then begin
            SetBlockBegin(CaretXY);
            SetBlockEnd(WP);
            ActiveSelectionMode := smNormal;
            helper := SelText;
            SetSelTextPrimitive('');
            fUndoList.AddChange(crSilentDelete, WP, CaretXY, helper,
              smNormal);
            InternalCaretXY := WP;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecDeleteLine:
        if not ReadOnly and (Lines.Count > 0) and not ((CaretY = Lines.Count) and (Length(Lines[CaretY - 1]) =
          0)) then begin
          DoOnPaintTransient(ttBefore);
          if SelAvail then
            SetBlockBegin(CaretXY);
          helper := LineText;
          if CaretY = Lines.Count then begin
            Lines[CaretY - 1] := '';
            fUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
              BufferCoord(Length(helper) + 1, CaretY), helper, smNormal);
          end else begin
            Lines.Delete(CaretY - 1);
            helper := helper + #13#10;
            fUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
              BufferCoord(1, CaretY + 1), helper, smNormal);
            DoLinesDeleted(CaretY, 1);
          end;
          InternalCaretXY := BufferCoord(1, CaretY); // like seen in the Delphi editor
          DoOnPaintTransient(ttAfter);
        end;
      ecDuplicateLine:
        if not ReadOnly and (Lines.Count > 0) then begin
          DoOnPaintTransient(ttBefore);
          Lines.Insert(CaretY, Lines[CaretY - 1]);
          DoLinesInserted(CaretY + 1, 1);
          fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
          InternalCaretXY := BufferCoord(1, CaretY); // like seen in the Delphi editor
          DoOnPaintTransient(ttAfter);
        end;
      ecMoveSelUp:
        if not ReadOnly and (Lines.Count > 0) and (BlockBegin.Line > 1) then begin
          DoOnPaintTransient(ttBefore);

          // Backup caret and selection
          OrigBlockBegin := BlockBegin;
          OrigBlockEnd := BlockEnd;

          // Delete line above selection
          s := Lines[OrigBlockBegin.Line - 2]; // before start, 0 based
          Lines.Delete(OrigBlockBegin.Line - 2); // before start, 0 based
          DoLinesDeleted(OrigBlockBegin.Line - 1, 1); // before start, 1 based

          // Insert line below selection
          Lines.Insert(OrigBlockEnd.Line - 1, S);
          DoLinesInserted(OrigBlockEnd.Line, 1);

          // Restore caret and selection
          SetCaretAndSelection(
            BufferCoord(CaretX, CaretY - 1),
            BufferCoord(1, OrigBlockBegin.Line - 1), // put start of selection at start of top line
            BufferCoord(Length(Lines[OrigBlockEnd.Line - 2]) + 1, OrigBlockEnd.Line - 1));
          // put end of selection at end of top line

      // Retrieve end of line we moved up
          MoveDelim := BufferCoord(Length(Lines[OrigBlockEnd.Line - 1]) + 1, OrigBlockEnd.Line);

          // Support undo, implement as drag and drop
          fUndoList.BeginBlock;
          try
            fUndoList.AddChange(crSelection, // backup original selection
              OrigBlockBegin,
              OrigBlockEnd,
              '',
              smNormal);
            fUndoList.AddChange(crDragDropInsert,
              BlockBegin, // modified
              MoveDelim, // put at end of line me moved up
              S + #13#10 + SelText,
              smNormal);
          finally
            fUndoList.EndBlock;
          end;

          DoOnPaintTransient(ttAfter);
        end;
      ecMoveSelDown:
        if not ReadOnly and (Lines.Count > 0) and (BlockEnd.Line < Lines.Count) then begin
          DoOnPaintTransient(ttBefore);

          // Backup caret and selection
          OrigBlockBegin := BlockBegin;
          OrigBlockEnd := BlockEnd;

          // Delete line below selection
          s := Lines[OrigBlockEnd.Line]; // after end, 0 based
          Lines.Delete(OrigBlockEnd.Line); // after end, 0 based
          DoLinesDeleted(OrigBlockEnd.Line, 1); // before start, 1 based

          // Insert line above selection
          Lines.Insert(OrigBlockBegin.Line - 1, S);
          DoLinesInserted(OrigBlockBegin.Line, 1);

          // Restore caret and selection
          SetCaretAndSelection(
            BufferCoord(CaretX, CaretY + 1),
            BufferCoord(1, OrigBlockBegin.Line + 1),
            BufferCoord(Length(Lines[OrigBlockEnd.Line]) + 1, OrigBlockEnd.Line + 1));

          // Retrieve start of line we moved down
          MoveDelim := BufferCoord(1, OrigBlockBegin.Line);

          // Support undo, implement as drag and drop
          fUndoList.BeginBlock;
          try
            fUndoList.AddChange(crSelection,
              OrigBlockBegin,
              OrigBlockEnd,
              '',
              smNormal);
            fUndoList.AddChange(crDragDropInsert,
              MoveDelim, // put at start of line me moved down
              BlockEnd, // modified
              SelText + #13#10 + S,
              smNormal);
          finally
            fUndoList.EndBlock;
          end;

          DoOnPaintTransient(ttAfter);
        end;
      ecClearAll: begin
          if not ReadOnly then
            ClearAll;
        end;
      ecInsertLine,
        ecLineBreak:
        if not ReadOnly then begin
          nLinesInserted:=0;
          UndoList.BeginBlock;
          try
            if SelAvail then begin
              helper := SelText;
              iUndoBegin := fBlockBegin;
              iUndoEnd := fBlockEnd;
              SetSelTextPrimitive('');
              fUndoList.AddChange(crDelete, iUndoBegin, iUndoEnd, helper,
                fActiveSelectionMode);
            end;
            Temp := LineText;
            Temp2 := Temp;
            // This is sloppy, but the Right Thing would be to track the column of markers
            // too, so they could be moved depending on whether they are after the caret...
            InsDelta := Ord(CaretX = 1);
            Len := Length(Temp);
            if Len > 0 then begin
              if Len >= CaretX then begin
                if CaretX > 1 then begin

                  Temp:= Copy(LineText, 1, CaretX - 1);
                  Temp3:=Temp;
                  SpaceCount1 := LeftSpacesEx(Temp, true);
                  Delete(Temp2, 1, CaretX - 1);
                  ProperSetLine(CaretY-1,Temp);
                  Lines.Insert(CaretY, GetLeftSpacing(SpaceCount1, true));
                  inc(nLinesInserted);
                  if (eoAddIndent in Options) and GetHighlighterAttriAtRowCol(BufferCoord(Length(Temp3), CaretY),
                    Temp3, Attr) then begin // only add indent to source files
                    if Attr <> Highlighter.CommentAttribute then begin // and outside of comments
                      if (Temp[Length(Temp)] =':')
                        or (
                          (Temp[Length(Temp)] ='{')
                          and ((Length(Temp2)<=0) or (Temp2[1]<>'}'))
                        )then begin // add more indent for these too
                        if not (eoTabsToSpaces in Options) then begin
                          Lines[CaretY] := Lines[CaretY] + TSynTabChar;
                        end else begin
                          Lines[CaretY] := Lines[CaretY] + StringOfChar(' ', TabWidth);
                        end;
                      end;
                    end;
                  end;
                  SpaceCount1 := Length(Lines[CaretY]);
                  Lines[CaretY] := Lines[CaretY] + Temp2;
                  fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
                    smNormal);

                  if (Length(Temp)>0) and (Temp[Length(Temp)] = '{') and (Length(Temp2)>0) and (Temp2[1]='}') then begin
                    Lines.Insert(CaretY, GetLeftSpacing(LeftSpacesEx(Temp, true), true));
                    inc(nLinesInserted);
                    if (eoAddIndent in Options) then begin;
                      if not (eoTabsToSpaces in Options) then begin
                        Lines[CaretY] := Lines[CaretY] + TSynTabChar;
                      end else begin
                        Lines[CaretY] := Lines[CaretY] + StringOfChar(' ', TabWidth);
                      end;
                    end;
                    fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '',
                      smNormal);
                    if Command = ecLineBreak then
                      InternalCaretXY := BufferCoord(
                        Length(Lines[CaretY])+1,
                        CaretY + 1);
                  end else begin
                    if Command = ecLineBreak then
                      InternalCaretXY := BufferCoord(
                        SpaceCount1+1,
                        CaretY + 1);
                  end;
                end else begin
                  Lines.Insert(CaretY - 1, '');
                  inc(nLinesInserted);
                  fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
                    smNormal);
                  if Command = ecLineBreak then
                    InternalCaretY := CaretY + 1;
                end;
              end else begin
                SpaceCount2 := 0;
                BackCounter := CaretY;
                if eoAutoIndent in Options then begin
                  repeat
                    Dec(BackCounter);
                    Temp := Lines[BackCounter];
                    SpaceCount2 := LeftSpaces(Temp);
                  until (BackCounter = 0) or (Temp <> '');
                end;
                Lines.Insert(CaretY, '');
                inc(nLinesInserted);
                Caret := CaretXY;
                if Command = ecLineBreak then begin
                  if SpaceCount2 > 0 then begin
                    Lines[CaretY] := Copy(Lines[BackCounter], 1, SpaceCount2); // copy previous indent
                  end;
                  if (eoAddIndent in Options) and GetHighlighterAttriAtRowCol(BufferCoord(Length(Temp), CaretY),
                    Temp,
                    Attr) then begin // only add indent to source files
                    if Attr <> Highlighter.CommentAttribute then begin // and outside of comments
                      if Temp[Length(Temp)] in ['{', ':'] then begin // add more indent for these too
                        if not (eoTabsToSpaces in Options) then begin
                          Lines[CaretY] := Lines[CaretY] + TSynTabChar;
                          Inc(SpaceCount2, 1);
                        end else begin
                          Lines[CaretY] := Lines[CaretY] + StringOfChar(' ', TabWidth);
                          Inc(SpaceCount2, TabWidth); // update caret counter
                        end;
                      end;
                    end;
                  end;
                  InternalCaretXY := BufferCoord(SpaceCount2 + 1, CaretY + 1);
                end;
                fUndoList.AddChange(crLineBreak, Caret, Caret, '', smNormal);
              end;
            end else begin
              if fLines.Count = 0 then
                fLines.Add('');

              SpaceCount2 := 0;
              if eoAutoIndent in Options then begin
                BackCounter := CaretY - 1;
                while BackCounter >= 0 do begin
                  SpaceCount2 := LeftSpacesEx(Lines[BackCounter], True);
                  if Length(Lines[BackCounter]) > 0 then
                    break;
                  dec(BackCounter);
                end;
              end;
              Lines.Insert(CaretY - 1, '');
              inc(nLinesInserted);
              fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
              if Command = ecLineBreak then begin
                InternalCaretXY := BufferCoord(SpaceCount2 + 1, CaretY + 1);
              end;
            end;
            DoLinesInserted(CaretY - InsDelta, nLinesInserted);
            BlockBegin := CaretXY;
            BlockEnd := CaretXY;
            EnsureCursorPosVisible;
            UpdateLastCaretX;
          finally
            UndoList.EndBlock;
          end;
        end;
      ecTab:
        if not ReadOnly then
          DoTabKey;
      ecShiftTab:
        if not ReadOnly then
          DoShiftTabKey;
      ecComment:
        DoComment;
      ecUnComment:
        DoUncomment;
      ecToggleComment:
        if not ReadOnly then begin
          OrigBlockBegin := BlockBegin;
          OrigBlockEnd := BlockEnd;

          BeginIndex := OrigBlockBegin.Line - 1;
          // Ignore the last line the cursor is placed on
          if (OrigBlockEnd.Char = 1) and (OrigBlockBegin.Line < OrigBlockEnd.Line) then
            EndIndex := max(0, OrigBlockEnd.Line - 2)
          else
            EndIndex := OrigBlockEnd.Line - 1;

          // if everything is commented, then uncomment
          for I := BeginIndex to EndIndex do begin
            if Pos('//', TrimLeft(fLines[i])) <> 1 then begin // not fully commented
              DoComment; // comment everything
              Exit;
            end;
          end;
          DoUncomment;
        end;
      ecCommentInline: // toggle inline comment
        if not ReadOnly and SelAvail then begin
          Temp := SelText;

          // Check if the selection starts with /* after blanks
          StartPos := -1;
          I := 1;
          while I <= Length(Temp) do begin
            if Temp[I] in [#9, #32] then
              Inc(I)
            else if ((I + 1) <= Length(Temp)) and (Temp[i] = '/') and (Temp[i + 1] = '*') then begin
              StartPos := I;
              break;
            end else
              break;
          end;

          // Check if the selection ends with /* after blanks
          EndPos := -1;
          if StartPos <> -1 then begin
            I := Length(Temp);
            while I > 0 do begin
              if Temp[I] in [#9, #32] then
                Dec(I)
              else if ((I - 1) > 0) and (Temp[i] = '/') and (Temp[i - 1] = '*') then begin
                EndPos := I;
                break;
              end else
                break;
            end;
          end;

          // Keep selection
          OrigBlockBegin := BlockBegin;
          OrigBlockEnd := BlockEnd;

          // Toggle based on current comment status
          if (StartPos <> -1) and (EndPos <> -1) then begin
            SelText := Copy(SelText, StartPos + 2, EndPos - StartPos - 3);
            BlockBegin := OrigBlockBegin;
            BlockEnd := BufferCoord(OrigBlockEnd.Char - 4, OrigBlockEnd.Line);
          end else begin
            SelText := '/*' + SelText + '*/';
            BlockBegin := BufferCoord(OrigBlockBegin.Char, OrigBlockBegin.Line);
            BlockEnd := BufferCoord(OrigBlockEnd.Char + 4, OrigBlockEnd.Line);
          end;
        end;
      ecMatchBracket:
        FindMatchingBracket;
      ecChar:
        // #127 is Ctrl + Backspace, #32 is space
        if not ReadOnly and (AChar >= #32) and (AChar <> #127) then begin
          //DoOnPaintTransient(ttBefore);
          if (InsertMode = False) and (not SelAvail) then begin
            SelLength := 1;
          end;

          if eoAddIndent in Options then begin

            // Remove TabWidth of indent of the current line when typing a }
            if AChar in ['}'] then begin
              temp := Copy(Lines[CaretY-1],1,CaretX-1);
              // and the first nonblank char is this new }
              if TrimLeft(temp) = '' then begin
                MatchBracketPos := GetPreviousLeftBracket(CaretX, CaretY);
                if (MatchBracketPos.Line > 0) then begin
                  i := 1;
                  while (i<=Length(Lines[MatchBracketPos.Line-1])) do begin
                    if  not (Lines[MatchBracketPos.Line-1][i] in [#9,#32]) then
                      break;
                    inc(i);
                  end;
                  temp := Copy(Lines[MatchBracketPos.Line-1], 1, i-1)
                    + Copy(Lines[CaretY - 1],CaretX,MaxInt);
                  Lines[CaretY - 1] := temp;
                  InternalCaretXY := BufferCoord(i, CaretY);
                end;
              end;
            end;
          end;

          SelText := AChar;

          //DoOnPaintTransient(ttAfter);
        end;
      ecUpperCase,
        ecLowerCase,
        ecToggleCase,
        ecTitleCase,
        ecUpperCaseBlock,
        ecLowerCaseBlock,
        ecToggleCaseBlock:
        if not ReadOnly then
          DoCaseChange(Command);
      ecUndo: begin
          if not ReadOnly then
            Undo;
        end;
      ecRedo: begin
          if not ReadOnly then
            Redo;
        end;
      ecGotoMarker0..ecGotoMarker9: begin
          if BookMarkOptions.EnableKeys then
            GotoBookMark(Command - ecGotoMarker0);
        end;
      ecSetMarker0..ecSetMarker9: begin
          if BookMarkOptions.EnableKeys then begin
            CX := Command - ecSetMarker0;
            if Assigned(Data) then
              Caret := TBufferCoord(Data^)
            else
              Caret := CaretXY;
            if assigned(fBookMarks[CX]) then begin
              moveBkm := (fBookMarks[CX].Line <> Caret.Line);
              ClearBookMark(CX);
              if moveBkm then
                SetBookMark(CX, Caret.Char, Caret.Line);
            end else
              SetBookMark(CX, Caret.Char, Caret.Line);
          end; // if BookMarkOptions.EnableKeys
        end;
      ecCut: begin
          if (not ReadOnly) and SelAvail then
            CutToClipboard;
        end;
      ecCopy: begin
          CopyToClipboard;
        end;
      ecPaste: begin
          if not ReadOnly then
            PasteFromClipboard;
        end;
      ecScrollUp, ecScrollDown: begin
          vCaretRow := DisplayY;
          if (vCaretRow < TopLine) or (vCaretRow >= TopLine + LinesInWindow) then
            // If the caret is not in view then, like the Delphi editor, move
            // it in view and do nothing else
            EnsureCursorPosVisible
          else begin
            if Command = ecScrollUp then begin
              TopLine := TopLine - 1;
              if vCaretRow > TopLine + LinesInWindow - 1 then
                MoveCaretVert((TopLine + LinesInWindow - 1) - vCaretRow, False);
            end else begin
              TopLine := TopLine + 1;
              if vCaretRow < TopLine then
                MoveCaretVert(TopLine - vCaretRow, False);
            end;
            EnsureCursorPosVisible;
            Update;
          end;
        end;
      ecScrollLeft: begin
          LeftChar := LeftChar - 1;
          // todo: The following code was commented out because it is not MBCS or hard-tab safe.
          //if CaretX > LeftChar + CharsInWindow then
          //  InternalCaretX := LeftChar + CharsInWindow;
          Update;
        end;
      ecScrollRight: begin
          LeftChar := LeftChar + 1;
          // todo: The following code was commented out because it is not MBCS or hard-tab safe.
          //if CaretX < LeftChar then
          //  InternalCaretX := LeftChar;
          Update;
        end;
      ecInsertMode: begin
          InsertMode := TRUE;
        end;
      ecOverwriteMode: begin
          InsertMode := FALSE;
        end;
      ecToggleMode: begin
          InsertMode := not InsertMode;
        end;
      ecBlockIndent:
        if not ReadOnly then
          DoBlockIndent;
      ecBlockUnindent:
        if not ReadOnly then
          DoBlockUnindent;
      ecNormalSelect:
        SelectionMode := smNormal;
      ecColumnSelect:
        SelectionMode := smColumn;
      ecLineSelect:
        SelectionMode := smLine;
      ecContextHelp: begin
          if Assigned(fOnContextHelp) then
            fOnContextHelp(self, WordAtCursor);
        end;
{$IFDEF SYN_MBCSSUPPORT}
      ecImeStr: begin;
        if not ReadOnly then begin
          SetString(s, PChar(Data), StrLen(Data));
          if SelAvail then begin
          {
            BeginUndoBlock;
            try
              fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, selText,
                smNormal);
              StartOfBlock := fBlockBegin;
              SetSelTextPrimitive(s);
              fUndoList.AddChange(crInsert, fBlockBegin, fBlockEnd, s,
                smNormal);
            finally
              EndUndoBlock;
            end;
          }
            SetSelTextExternal(s);
            InvalidateGutterLines(-1, -1);
          end else begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < (CaretX-1) then
              Temp := Temp + StringOfChar(#32, (CaretX-1) - Len);
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then
                Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;
              // Processing of case character covers on LeadByte.
              Len := Length(s);
              if not fInserting then begin
                i := (CaretX + Len);
                if (ByteType(Temp, i) = mbTrailByte) then begin
                  s := s + Temp[i - 1];
                  helper := Copy(Temp, CaretX, Len - 1);
                end else
                  helper := Copy(Temp, CaretX, Len);
                Delete(Temp, CaretX, Len);
              end;
              Insert(s, Temp, CaretX);
              InternalCaretX := (CaretX + Len);
              ProperSetLine(CaretY - 1, Temp);
              if fInserting then
                helper := '';
              fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, helper,
                smNormal);
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then
                Exclude(fOptions, eoScrollPastEol);
            end;
          end;
        end;
          if assigned(fOnImeInput) then
            fOnImeInput(self,s);
        end;
{$ENDIF}
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand;
  AChar: char; Data: pointer);
begin
  if Assigned(fOnCommandProcessed) then
    fOnCommandProcessed(Self, Command, AChar, Data);
end;

procedure TCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: char; Data: pointer);
begin
  if Command < ecUserFirst then begin
    if Assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, Command, AChar, Data);
  end else begin
    if Assigned(FOnProcessUserCommand) then
      FOnProcessUserCommand(Self, Command, AChar, Data);
  end;
end;

procedure TCustomSynEdit.ClearAll;
begin
  Lines.Clear;
  fMarkList.Clear;
  fUndoList.Clear;
  fRedoList.Clear;
  Modified := False;
end;

procedure TCustomSynEdit.ClearSelection;
begin
  if SelAvail then
    SelText := '';
end;

function TCustomSynEdit.NextWordPosEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY, LineLen, MultiPos: integer;
  Line: string;
  IdentChars, WhiteChars: TSynIdentChars;
begin
  CX := XY.Char;
  CY := XY.Line;

  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];

    prepareIdentChars(IdentChars,WhiteChars);

    LineLen := Length(Line);
    if CX >= LineLen then begin
      // find first IdentChar or multibyte char in the next line
      if CY < Lines.Count then begin
        Line := Lines[CY];
        Inc(CY);
{$IFDEF SYN_MBCSSUPPORT}
        MultiPos := StrScanForMultiByteChar(Line, 1);
{$ELSE}
        MultiPos := 0;
{$ENDIF}
        CX := StrScanForCharInSet(Line, 1, IdentChars);
        // stop on any multibyte chars
        if (MultiPos > 0) and ((CX = 0) or (CX > MultiPos)) then
          CX := MultiPos;
        if CX = 0 then
          Inc(CX);
      end;
    end else begin
{$IFDEF SYN_MBCSSUPPORT}
      MultiPos := StrScanForMultiByteChar(Line, CX + 1);
      // find next "whitespace" if current char is an IdentChar
      if (Line[CX] in IdentChars) and (ByteType(Line, CX) = mbSingleByte) then
        CX := StrScanForCharInSet(Line, CX, WhiteChars);
{$ELSE}
      MultiPos := 0;
      // find next "whitespace" if current char is an IdentChar
      if Line[CX] in IdentChars then
        CX := StrScanForCharInSet(Line, CX, WhiteChars);
{$ENDIF}
      // if "whitespace" found, find the next IdentChar
      if CX > 0 then
        CX := StrScanForCharInSet(Line, CX, IdentChars);
      // stop on any multibyte chars
      if (MultiPos > 0) and ((CX = 0) or (CX > MultiPos)) then
        CX := MultiPos;
      // if one of those failed just position at the end of the line
      if CX = 0 then
        CX := LineLen + 1;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

procedure TCustomSynEdit.LockPainter;
begin
  inc(fPainterLock);
end;

procedure TCustomSynEdit.UnLockPainter;
begin
  dec(fPainterLock);
end;


function TCustomSynEdit.WordStartEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: integer;
  Line: string;
  IdentChars, WhiteChars: TSynIdentChars;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    prepareIdentChars(IdentChars,WhiteChars);

    if CX > 1 then begin // only find previous char, if not already on start of line
      // if previous char isn't a "whitespace" search for the last IdentChar
      if not (Line[CX - 1] in WhiteChars) then
        CX := StrRScanForCharInSet(Line, CX - 1, WhiteChars) + 1;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TCustomSynEdit.WordEndEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: integer;
  Line: string;
  IdentChars, WhiteChars: TSynIdentChars;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];

    prepareIdentChars(IdentChars,WhiteChars);

    CX := StrScanForCharInSet(Line, CX, WhiteChars);
    // if no "whitespace" is found just position at the end of the line
    if CX = 0 then
      CX := Length(Line) + 1;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TCustomSynEdit.PrevWordPosEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY, MultiPos: integer;
  Line: string;
  IdentChars, WhiteChars: TSynIdentChars;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    prepareIdentChars(IdentChars,WhiteChars);

    if CX <= 1 then begin
      // find last IdentChar in the previous line
      if CY > 1 then begin
        Dec(CY);
        Line := Lines[CY - 1];
        CX := Length(Line) + 1;
      end;
    end else begin
{$IFDEF SYN_MBCSSUPPORT}
      MultiPos := StrRScanForMultiByteChar(Line, CX - 1);
      // if previous char is a "whitespace" search for the last IdentChar
      if (Line[CX - 1] in WhiteChars) and (ByteType(Line, CX - 1) = mbSingleByte) then
        CX := StrRScanForCharInSet(Line, CX - 1, IdentChars);
{$ELSE}
      MultiPos := 0;
      // if previous char is a "whitespace" search for the last IdentChar
      if Line[CX - 1] in WhiteChars then
        CX := StrRScanForCharInSet(Line, CX - 1, IdentChars);
{$ENDIF}
      if CX > 0 then
        // search for the first IdentChar of this "word"
        CX := StrRScanForCharInSet(Line, CX - 1, WhiteChars) + 1;
      // stop on any multibyte chars
      if (MultiPos > 0) and ((CX = 0) or (CX < MultiPos)) then
        CX := MultiPos;
      if CX = 0 then begin
        // else just position at the end of the previous line
        if CY > 1 then begin
          Dec(CY);
          Line := Lines[CY - 1];
          CX := Length(Line) + 1;
        end else
          CX := 1;
      end;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

procedure TCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  if FSelectionMode <> Value then begin
    fSelectionMode := Value;
    ActiveSelectionMode := Value;
  end;
end;

procedure TCustomSynEdit.SetActiveSelectionMode(const Value: TSynSelectionMode);
begin
  if fActiveSelectionMode <> Value then begin
    if SelAvail then
      InvalidateSelection;
    fActiveSelectionMode := Value;
    if SelAvail then
      InvalidateSelection;
    StatusChanged([scSelection]);
  end;
end;

procedure TCustomSynEdit.BeginUndoBlock;
begin
  fUndoList.BeginBlock;
end;

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

procedure TCustomSynEdit.EndUndoBlock;
begin
  fUndoList.EndBlock;
end;

procedure TCustomSynEdit.EndUpdate;
begin
  DecPaintLock;
end;

procedure TCustomSynEdit.AddKey(Command: TSynEditorCommand;
  Key1: word; SS1: TShiftState; Key2: word; SS2: TShiftState);
var
  Key: TSynEditKeyStroke;
begin
  Key := Keystrokes.Add;
  Key.Command := Command;
  Key.Key := Key1;
  Key.Shift := SS1;
  Key.Key2 := Key2;
  Key.Shift2 := SS2;
end;

{ Called by FMarkList if change }

procedure TCustomSynEdit.MarkListChange(Sender: TObject);
begin
  InvalidateGutter;
end;

function TCustomSynEdit.GetSelStart: integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(BlockBegin)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TCustomSynEdit.PrepareIdentChars(var IdentChars,
  WhiteChars: TSynIdentChars);
var
  WordBreakChars: TSynIdentChars;
begin
  if Assigned(Highlighter) then begin
    IdentChars := Highlighter.IdentChars;
    WordBreakChars := Highlighter.WordBreakChars;
  end else begin
    IdentChars := TSynValidStringChars;
    WordBreakChars := TSynWordBreakChars;
  end;
  IdentChars := IdentChars - WordBreakChars;
  WhiteChars := [#1..#255] - IdentChars;
end;

procedure TCustomSynEdit.SetAlwaysShowCaret(const Value: Boolean);
begin
  if FAlwaysShowCaret <> Value then begin
    FAlwaysShowCaret := Value;
    if not (csDestroying in ComponentState) and not (focused) then begin
      if Value then begin
        InitializeCaret;
      end else begin
        HideCaret;
        Windows.DestroyCaret;
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetSelStart(const Value: integer);
begin
  { if we don't call HandleNeeded, CharsInWindow may be 0 and LeftChar will
  be set to CaretX }
  HandleNeeded;
  InternalCaretXY := CharIndexToRowCol(Value);
  BlockBegin := CaretXY;
end;

function TCustomSynEdit.GetSelEnd: integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(Blockend)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TCustomSynEdit.SetSelEnd(const Value: integer);
begin
  HandleNeeded;
  BlockEnd := CharIndexToRowCol(Value);
end;

procedure TCustomSynEdit.SetSelWord;
begin
  SetWordBlock(CaretXY);
end;

procedure TCustomSynEdit.SetExtraLineSpacing(const Value: integer);
begin
  fExtraLineSpacing := Value;
  SynFontChanged(self);
end;

function TCustomSynEdit.GetBookMark(BookMark: integer; var X, Y: integer):
  boolean;
var
  i: integer;
begin
  Result := false;
  if assigned(Marks) then
    for i := 0 to Marks.Count - 1 do
      if Marks[i].IsBookmark and (Marks[i].BookmarkNumber = BookMark) then begin
        X := Marks[i].Char;
        Y := Marks[i].Line;
        Result := true;
        Exit;
      end;
end;

function TCustomSynEdit.IsBookmark(BookMark: integer): boolean;
var
  x, y: integer;
begin
  Result := GetBookMark(BookMark, x, y);
end;

procedure TCustomSynEdit.ClearUndo;
begin
  fUndoList.Clear;
  fRedoList.Clear;
end;

procedure TCustomSynEdit.SetSelTextExternal(const Value: string);
var
  StartOfBlock, EndOfBlock: TBufferCoord;
begin
  BeginUndoBlock;
  try
    if SelAvail then begin
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd,
        SelText, fActiveSelectionMode);
    end else
      ActiveSelectionMode := SelectionMode;
    StartOfBlock := BlockBegin;
    EndOfBlock := BlockEnd;
    fBlockBegin := StartOfBlock;
    fBlockEnd := EndOfBlock;
    SetSelTextPrimitive(Value);
    if (Value <> '') and (fActiveSelectionMode <> smColumn) then
      fUndoList.AddChange(crInsert, StartOfBlock, BlockEnd, '', fActiveSelectionMode);
  finally
    EndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  fGutter.Assign(Value);
end;

procedure TCustomSynEdit.GutterChanged(Sender: TObject);
var
  nW: integer;
begin
  if not (csLoading in ComponentState) then begin
    if fGutter.ShowLineNumbers and fGutter.AutoSize then
      fGutter.AutoSizeDigitCount(Lines.Count);
    if fGutter.UseFontStyle then begin
      fTextDrawer.SetBaseFont(fGutter.Font);
      nW := fGutter.RealGutterWidth(fTextDrawer.CharWidth);
      fTextDrawer.SetBaseFont(Font);
    end else
      nW := fGutter.RealGutterWidth(fCharWidth);
    if nW = fGutterWidth then
      InvalidateGutter
    else
      SetGutterWidth(nW);
  end;
end;

procedure TCustomSynEdit.LockUndo;
begin
  fUndoList.Lock;
  fRedoList.Lock;
end;

procedure TCustomSynEdit.UnlockUndo;
begin
  fUndoList.Unlock;
  fRedoList.Unlock;
end;

procedure TCustomSynEdit.WMSetCursor(var Msg: TWMSetCursor);
begin
  if (Msg.HitTest = HTCLIENT) and (Msg.CursorWnd = Handle) and
    not (csDesigning in ComponentState) then begin
    UpdateMouseCursor;
  end else
    inherited;
end;

procedure TCustomSynEdit.SetTabWidth(Value: integer);
begin
  Value := MinMax(Value, 1, 256);
  if (Value <> fTabWidth) then begin
    fTabWidth := Value;
    Lines.TabWidth := Value;
    Invalidate; // to redraw text containing tab chars
  end;
end;

procedure TCustomSynEdit.SelectedColorsChanged(Sender: TObject);
begin
  InvalidateSelection;
end;

// find / replace

function TCustomSynEdit.SearchReplace(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions): integer;
var
  ptStart, ptEnd: TBufferCoord; // start and end of the search range
  ptCurrent: TBufferCoord; // current search position
  nSearchLen, nReplaceLen, n, nFound: integer;
  nInLine: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  bEndUndoBlock: boolean;
  nAction: TSynReplaceAction;
  iResultOffset: integer;

  function InValidSearchRange(First, Last: integer): boolean;
  begin
    Result := True;
    if (fActiveSelectionMode = smNormal) or not (ssoSelectedOnly in AOptions) then begin
      if ((ptCurrent.Line = ptStart.Line) and (First < ptStart.Char)) or
        ((ptCurrent.Line = ptEnd.Line) and (Last > ptEnd.Char)) then
        Result := False;
    end else if (fActiveSelectionMode = smColumn) then
      // solves bug in search/replace when smColumn mode active and no selection
      Result := (First >= ptStart.Char) and (Last <= ptEnd.Char) or (ptEnd.Char - ptStart.Char < 1);
  end;

begin
  if not assigned(fSearchEngine) then begin
    raise ESynEditError.Create('No search engine has been assigned');
  end;

  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then
    exit;
  // get the text range to search in, ignore the "Search in selection only"
  // option if nothing is selected
  bBackward := (ssoBackwards in AOptions);
  bPrompt := (ssoPrompt in AOptions);
  bReplace := (ssoReplace in AOptions);
  bReplaceAll := (ssoReplaceAll in AOptions);
  bFromCursor := not (ssoEntireScope in AOptions);
  if not SelAvail then
    Exclude(AOptions, ssoSelectedOnly);
  if (ssoSelectedOnly in AOptions) then begin
    ptStart := BlockBegin;
    ptEnd := BlockEnd;
    // search the whole line in the line selection mode
    if (fActiveSelectionMode = smLine) then begin
      ptStart.Char := 1;
      ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    end else if (fActiveSelectionMode = smColumn) then
      // make sure the start column is smaller than the end column
      if (ptStart.Char > ptEnd.Char) then
        SwapInt(integer(ptStart.Char), integer(ptEnd.Char));
    // ignore the cursor position when searching in the selection
    if bBackward then
      ptCurrent := ptEnd
    else
      ptCurrent := ptStart;
  end else begin
    ptStart.Char := 1;
    ptStart.Line := 1;
    ptEnd.Line := Lines.Count;
    ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    if bFromCursor then
      if bBackward then
        ptEnd := CaretXY
      else
        ptStart := CaretXY;
    if bBackward then
      ptCurrent := ptEnd
    else
      ptCurrent := ptStart;
  end;
  // initialize the search engine
  fSearchEngine.Options := AOptions;
  fSearchEngine.Pattern := ASearch;
  // search while the current search position is inside of the search range
  nReplaceLen := 0;
  DoOnPaintTransient(ttBefore);
  if bReplaceAll and not bPrompt then begin
    IncPaintLock;
    BeginUndoBlock;
    bEndUndoBlock := True;
  end else
    bEndUndoBlock := False;
  try
    while (ptCurrent.Line >= ptStart.Line) and (ptCurrent.Line <= ptEnd.Line) do begin
      nInLine := fSearchEngine.FindAll(Lines[ptCurrent.Line - 1]);
      iResultOffset := 0;
      if bBackward then
        n := Pred(fSearchEngine.ResultCount)
      else
        n := 0;
      // Operate on all results in this line.
      while nInLine > 0 do begin
        // An occurrence may have been replaced with a text of different length
        nFound := fSearchEngine.Results[n] + iResultOffset;
        nSearchLen := fSearchEngine.Lengths[n];
        if bBackward then
          Dec(n)
        else
          Inc(n);
        Dec(nInLine);
        // Is the search result entirely in the search range?
        if not InValidSearchRange(nFound, nFound + nSearchLen) then
          continue;
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.

        ptCurrent.Char := nFound;
        BlockBegin := ptCurrent;
        //Be sure to use the Ex version of CursorPos so that it appears in the middle if necessary
        SetCaretXYEx(False, BufferCoord(1, ptCurrent.Line));
        EnsureCursorPosVisibleEx(True);
        Inc(ptCurrent.Char, nSearchLen);
        BlockEnd := ptCurrent;
        InternalCaretXY := ptCurrent;
        if bBackward then
          InternalCaretXY := BlockBegin
        else
          InternalCaretXY := ptCurrent;
        // If it's a search only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then
          exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if bPrompt and Assigned(fOnReplaceText) then begin
          nAction := DoOnReplaceText(ASearch, AReplace, ptCurrent.Line, nFound, nSearchLen);
          if nAction = raCancel then
            exit;
        end else
          nAction := raReplace;
        if nAction <> raSkip then begin
          // user has been prompted and has requested to silently replace all
          // so turn off prompting
          if nAction = raReplaceAll then begin
            if (not bReplaceAll) or bPrompt then begin
              bReplaceAll := TRUE;
              IncPaintLock;
            end;
            bPrompt := False;
            if bEndUndoBlock = false then
              BeginUndoBlock;
            bEndUndoBlock := true;
          end;
          //Allow advanced substition in the search engine
          SelText := fSearchEngine.Replace(SelText, AReplace);
          nReplaceLen := CaretX - nFound;
        end;
        // fix the caret position and the remaining results
        if not bBackward then begin
          InternalCaretX := nFound + nReplaceLen;
          if (nSearchLen <> nReplaceLen) and (nAction <> raSkip) then begin
            Inc(iResultOffset, nReplaceLen - nSearchLen);
            if (fActiveSelectionMode <> smColumn) and (CaretY = ptEnd.Line) then begin
              Inc(ptEnd.Char, nReplaceLen - nSearchLen);
              BlockEnd := ptEnd;
            end;
          end;
        end;
        if not bReplaceAll then
          exit;
      end;
      // search next / previous line
      if bBackward then
        Dec(ptCurrent.Line)
      else
        Inc(ptCurrent.Line);
    end;
  finally
    if bReplaceAll and not bPrompt then
      DecPaintLock;
    if bEndUndoBlock then
      EndUndoBlock;
    DoOnPaintTransient(ttAfter);
  end;
end;

{$IFDEF SYN_MBCSSUPPORT}

procedure TCustomSynEdit.MBCSGetSelRangeInLineWhenColumnSelectionMode(
  const s: string; var ColFrom, ColTo: Integer);
// --ColFrom and ColTo are in/out parameter. their range
//    will be from 1 to MaxInt.
// --a range of selection means:  Copy(s, ColFrom, ColTo - ColFrom);
//    be careful what ColTo means.
var
  Len: Integer;
begin
  Len := Length(s);
  if (0 < ColFrom) and (ColFrom <= Len) then
    if mbTrailByte = ByteType(s, ColFrom) then
      Inc(ColFrom);
  if (0 < ColTo) and (ColTo <= Len) then
    if mbTrailByte = ByteType(s, ColTo) then
      Inc(ColTo);
end;

{$ENDIF}

function TCustomSynEdit.IsPointInSelection(const Value: TBufferCoord): boolean;
var
  ptBegin, ptEnd: TBufferCoord;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Line >= ptBegin.Line) and (Value.Line <= ptEnd.Line) and
    ((ptBegin.Line <> ptEnd.Line) or (ptBegin.Char <> ptEnd.Char)) then begin
    if fActiveSelectionMode = smLine then
      Result := TRUE
    else if (fActiveSelectionMode = smColumn) then begin
      if (ptBegin.Char > ptEnd.Char) then
        Result := (Value.Char >= ptEnd.Char) and (Value.Char < ptBegin.Char)
      else if (ptBegin.Char < ptEnd.Char) then
        Result := (Value.Char >= ptBegin.Char) and (Value.Char < ptEnd.Char)
      else
        Result := FALSE;
    end else
      Result := ((Value.Line > ptBegin.Line) or (Value.Char >= ptBegin.Char)) and
        ((Value.Line < ptEnd.Line) or (Value.Char < ptEnd.Char));
  end else
    Result := FALSE;
end;

procedure TCustomSynEdit.SetFocus;
begin
  if (fFocusList.Count > 0) then begin
    if TWinControl(fFocusList.Last).CanFocus then
      TWinControl(fFocusList.Last).SetFocus;
    exit;
  end;
  inherited;
end;

procedure TCustomSynEdit.UpdateMouseCursor;
var
  ptCursor: TPoint;
  ptLineCol: TBufferCoord;
  iNewCursor: TCursor;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
  if (ptCursor.X < fGutterWidth) then
    SetCursor(Screen.Cursors[fGutter.Cursor])
  else begin
    ptLineCol := DisplayToBufferPos(PixelsToRowColumn(ptCursor.X, ptCursor.Y));
    if (eoDragDropEditing in fOptions) and (not MouseCapture) and IsPointInSelection(ptLineCol) then
      iNewCursor := crArrow
    else
      iNewCursor := Cursor;
    if Assigned(OnMouseCursor) then
      OnMouseCursor(Self, ptLineCol, iNewCursor);
    fKbdHandler.ExecuteMouseCursor(Self, ptLineCol, iNewCursor);
    SetCursor(Screen.Cursors[iNewCursor]);
  end;
end;

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

function TCustomSynEdit.GetOptions: TSynEditorOptions;
begin
  Result := fOptions;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
const
  ScrollOptions = [eoDisableScrollArrows, eoHideShowScrollbars,
    eoScrollPastEof, eoScrollPastEol];
var
  bSetDrag: boolean;
  TmpBool: Boolean;
  bUpdateScroll: boolean;
  vTempBlockBegin, vTempBlockEnd: TBufferCoord;
begin
  if (Value <> fOptions) then begin
    bSetDrag := (eoDropFiles in fOptions)<>(eoDropFiles in Value);

    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;

    bUpdateScroll := (Options * ScrollOptions)<>(Value * ScrollOptions);

    fOptions := Value;

    // constrain caret position to MaxScrollWidth if eoScrollPastEol is enabled
    InternalCaretXY := CaretXY;
    if (eoScrollPastEol in Options) then begin
      vTempBlockBegin := BlockBegin;
      vTempBlockEnd := BlockEnd;
      SetBlockBegin(vTempBlockBegin);
      SetBlockEnd(vTempBlockEnd);
    end;

    // (un)register HWND as drop target
    if bSetDrag and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (eoDropFiles in fOptions));
    TmpBool := eoShowSpecialChars in Value;
    if TmpBool <> fShowSpecChar then begin
      fShowSpecChar := TmpBool;
      Invalidate;
    end;
    if bUpdateScroll then
      UpdateScrollBars;
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
  if HandleAllocated and (fCharWidth <> 0) then begin
    fCharsInWindow := Max(ClientWidth - fGutterWidth - 2, 0) div fCharWidth;
    fLinesInWindow := ClientHeight div fTextHeight;
    if bFont then begin
      if Gutter.ShowLineNumbers then
        GutterChanged(Self)
      else
        UpdateScrollbars;
      InitializeCaret;
      Exclude(fStateFlags, sfCaretChanged);
      Invalidate;
    end else
      UpdateScrollbars;
    Exclude(fStateFlags, sfScrollbarChanged);
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MoveCaretHorz(DX: integer; SelectionCommand: boolean);
var
  ptO, ptDst: TBufferCoord;
  s: string;
  nLineLen: integer;
  bChangeY: boolean;
begin
  ptO := CaretXY;
  ptDst := ptO;
  s := LineText;
  nLineLen := Length(s);
  // only moving or selecting one char can change the line
  bChangeY := not (eoScrollPastEol in fOptions);
  if bChangeY and (DX = -1) and (ptO.Char = 1) and (ptO.Line > 1) then begin
    // end of previous line
    Dec(ptDst.Line);
    ptDst.Char := Length(Lines[ptDst.Line - 1]) + 1;
  end else if bChangeY and (DX = 1) and (ptO.Char > nLineLen) and (ptO.Line < Lines.Count) then begin
    // start of next line
    Inc(ptDst.Line);
    ptDst.Char := 1;
  end else begin
    ptDst.Char := Max(1, ptDst.Char + DX);
    // don't go past last char when ScrollPastEol option not set
    if (DX > 0) and bChangeY then
      ptDst.Char := Min(ptDst.Char, nLineLen + 1);
{$IFDEF SYN_MBCSSUPPORT}
    // prevent from getting inside of a doublebyte char
    if (ptDst.Char > 1) and (ptDst.Char <= nLineLen) then begin
      DX := ptDst.Char - ptO.Char;
      if (DX < 0) then begin
        if ByteType(s, ptDst.Char) = mbTrailByte then
          Dec(ptDst.Char);
      end else if (DX > 0) then begin
        if ByteType(s, ptDst.Char) = mbTrailByte then
          Inc(ptDst.Char);
      end;
    end;
{$ENDIF}
  end;
  // set caret and block begin / end
  MoveCaretAndSelection(fBlockBegin, ptDst, SelectionCommand);
end;

procedure TCustomSynEdit.MoveCaretVert(DY: integer; SelectionCommand: boolean);
var
  ptO, ptDst: TDisplayCoord;
  vDstLineChar: TBufferCoord;
{$IFDEF SYN_MBCSSUPPORT}
  NewStepAside: Boolean;
  s: string;
{$ENDIF}
  SaveLastCaretX: Integer;
begin
  ptO := DisplayXY;
  ptDst := ptO;

  Inc(ptDst.Row, DY);
  if DY >= 0 then begin
    if RowToLine(ptDst.Row) > Lines.Count then
      ptDst.Row := Max(1, DisplayLineCount);
  end else begin
    if ptDst.Row < 1 then
      ptDst.Row := 1;
  end;

  if (ptO.Row <> ptDst.Row) then begin
    if eoKeepCaretX in Options then
      ptDst.Column := fLastCaretX;
  end;
  vDstLineChar := DisplayToBufferPos(ptDst);
{$IFDEF SYN_MBCSSUPPORT}
  if (ptO.Row <> ptDst.Row) then begin
    if fMBCSStepAside and not (eoKeepCaretX in Options) then
      Inc(vDstLineChar.Char);
    NewStepAside := False;
    s := Lines[vDstLineChar.Line - 1];
    if vDstLineChar.Char <= Length(s) then
      if ByteType(s, vDstLineChar.Char) = mbTrailByte then begin
        NewStepAside := True;
        Dec(vDstLineChar.Char);
      end;
  end else
    NewStepAside := fMBCSStepAside;
{$ENDIF}
  SaveLastCaretX := fLastCaretX;

  // set caret and block begin / end
  IncPaintLock;
  MoveCaretAndSelection(fBlockBegin, vDstLineChar, SelectionCommand);
  DecPaintLock;

  // Set fMBCSStepAside and restore fLastCaretX after moving caret, since
  // UpdateLastCaretX, called by SetCaretXYEx, changes them. This is the one
  // case where we don't want that.
{$IFDEF SYN_MBCSSUPPORT}
  fMBCSStepAside := NewStepAside;
{$ENDIF}
  fLastCaretX := SaveLastCaretX;
end;

procedure TCustomSynEdit.MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
  SelectionCommand: boolean);
begin
  if (eoGroupUndo in FOptions) and UndoList.CanUndo then
    fUndoList.AddGroupBreak;

  IncPaintLock;
  if SelectionCommand then begin
    if not SelAvail then
      SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  end else
    SetBlockBegin(ptAfter);
  InternalCaretXY := ptAfter;
  DecPaintLock;
end;

procedure TCustomSynEdit.SetCaretAndSelection(const ptCaret, ptBefore,
  ptAfter: TBufferCoord);
var
  vOldMode: TSynSelectionMode;
begin
  vOldMode := fActiveSelectionMode;
  IncPaintLock;
  try
    InternalCaretXY := ptCaret;
    SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  finally
    ActiveSelectionMode := vOldMode;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.RecalcCharExtent;
const
  iFontStyles: array[0..3] of TFontStyles = ([], [fsItalic], [fsBold],
    [fsItalic, fsBold]);
var
  iHasStyle: array[0..3] of boolean;
  cAttr: integer;
  cStyle: integer;
  iCurr: TFontStyles;
begin
  FillChar(iHasStyle, SizeOf(iHasStyle), 0);
  if Assigned(fHighlighter) and (fHighlighter.AttrCount > 0) then begin
    for cAttr := 0 to fHighlighter.AttrCount - 1 do begin
      iCurr := fHighlighter.Attribute[cAttr].Style * [fsItalic, fsBold];
      for cStyle := 0 to 3 do
        if iCurr = iFontStyles[cStyle] then begin
          iHasStyle[cStyle] := True;
          break;
        end;
    end;
  end else begin
    iCurr := Font.Style * [fsItalic, fsBold];
    for cStyle := 0 to 3 do
      if iCurr = iFontStyles[cStyle] then begin
        iHasStyle[cStyle] := True;
        break;
      end;
  end;

  fTextHeight := 0;
  fCharWidth := 0;
  fTextDrawer.BaseFont := Self.Font;
  for cStyle := 0 to 3 do
    if iHasStyle[cStyle] then begin
      fTextDrawer.BaseStyle := iFontStyles[cStyle];
      fTextHeight := Max(fTextHeight, fTextDrawer.CharHeight);
      fCharWidth := Max(fCharWidth, fTextDrawer.CharWidth);
    end;
  Inc(fTextHeight, fExtraLineSpacing);
end;

procedure TCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
begin
  RecalcCharExtent;
  if Sender is TSynCustomHighlighter then begin
    Lines.BeginUpdate;
    try
      ScanRanges;
    finally
      Lines.EndUpdate;
    end;
  end else
    Invalidate;
  SizeOrFontChanged(True);
end;

procedure TCustomSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(fStatusChanges);
end;

procedure TCustomSynEdit.DoCaseChange(const Cmd: TSynEditorCommand);

  function ToggleCase(const aStr: string): string;
  var
    i: Integer;
    s1, s2: string;
  begin
    Result := '';
    s1 := AnsiUpperCase(aStr);
    s2 := AnsiLowerCase(aStr);
    for i := 1 to Length(aStr) do begin
      if aStr[i] = s1[i] then
        Result := Result + s2[i]
      else
        Result := Result + s1[i];
    end;
  end;

var
  w: string;
  oldCaret, oldBlockBegin, oldBlockEnd: TBufferCoord;
  bHadSel: Boolean;
begin
  Assert((Cmd >= ecUpperCase) and (Cmd <= ecToggleCaseBlock));
  if SelAvail then begin
    bHadSel := True;
    oldBlockBegin := BlockBegin;
    oldBlockEnd := BlockEnd;
  end else begin
    bHadSel := False;
  end;
  oldCaret := CaretXY;
  try
    if Cmd < ecUpperCaseBlock then begin
      { word commands }
      SetSelWord;
      if SelText = '' then begin
        { searches a previous word }
        InternalCaretXY := PrevWordPos;
        SetSelWord;
        if SelText = '' then begin
          { try once more since PrevWordPos may have failed last time.
          (PrevWordPos "points" to the end of the previous line instead of the
          beggining of the previous word if invoked (e.g.) when CaretX = 1) }
          InternalCaretXY := PrevWordPos;
          SetSelWord;
        end;
      end;
    end else begin
      { block commands }
      if not SelAvail then begin
        if CaretX <= Length(LineText) then
          MoveCaretHorz(1, True)
        else if CaretY < Lines.Count then
          InternalCaretXY := BufferCoord(1, CaretY + 1);
      end;
    end;

    w := SelText;
    if w <> '' then begin
      case Cmd of
        ecUpperCase, ecUpperCaseBlock:
          w := AnsiUpperCase(w);
        ecLowerCase, ecLowerCaseBlock:
          w := AnsiLowerCase(w);
        ecToggleCase, ecToggleCaseBlock:
          w := ToggleCase(w);
        ecTitleCase:
          w := AnsiUpperCase(w[1]) + AnsiLowerCase(Copy(w, 2, Length(w)));
      end;
      BeginUndoBlock;
      try
        if bHadSel then
          fUndoList.AddChange(crSelection, oldBlockBegin, oldBlockEnd, '', fActiveSelectionMode)
        else
          fUndoList.AddChange(crSelection, oldCaret, oldCaret, '', fActiveSelectionMode);
        fUndoList.AddChange(crCaret, oldCaret, oldCaret, '', fActiveSelectionMode);
        SelText := w;
      finally
        EndUndoBlock;
      end;
    end;
  finally
    { "word" commands do not restore Selection }
    if bHadSel and (Cmd >= ecUpperCaseBlock) then begin
      BlockBegin := oldBlockBegin;
      BlockEnd := oldBlockEnd;
    end;
    { "block" commands with empty Selection move the Caret }
    if bHadSel or (Cmd < ecUpperCaseBlock) then
      CaretXY := oldCaret;
  end;
end;

procedure TCustomSynEdit.DoTabKey;
var
  StartOfBlock: TBufferCoord;
  i, MinLen, iLine: integer;
  PrevLine, Spaces: string;
  p: PChar;
  NewCaretX: integer;
  ChangeScroll: boolean;
  nPhysX, nDistanceToTab, nSpacesToNextTabStop: Integer;
  OldSelTabLine, vIgnoreSmartTabs: Boolean;
begin
  // Provide Visual Studio like block indenting
  OldSelTabLine := SelTabLine;
  if (eoTabIndent in Options) and ((SelTabBlock) or (OldSelTabLine)) then begin
    DoBlockIndent;
    if OldSelTabLine then begin
      if fBlockBegin.Char < fBlockEnd.Char then
        FBlockBegin.Char := 1
      else
        fBlockEnd.Char := 1;
    end;
    exit;
  end;
  i := 0;
  iLine := 0;
  MinLen := 0;
  vIgnoreSmartTabs := False;
  if eoSmartTabs in fOptions then begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then begin
      Dec(iLine);
      repeat
        //todo: rethink it
        MinLen := DisplayToBufferPos(
          DisplayCoord(BufferToDisplayPos(CaretXY).Column, LineToRow(iLine + 1))).Char;
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MinLen) then begin
          p := @PrevLine[MinLen];
          // scan over non-whitespaces
          repeat
            if p^ in [#9, #32] then
              break;
            Inc(i);
            Inc(p);
          until p^ = #0;
          // scan over whitespaces
          if p^ <> #0 then
            repeat
              if not (p^ in [#9, #32]) then
                break;
              Inc(i);
              Inc(p);
            until p^ = #0;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end else
      vIgnoreSmartTabs := True;
  end;
  fUndoList.BeginBlock;
  try
    if SelAvail then begin
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
        fActiveSelectionMode);
      SetSelTextPrimitive('');
    end;
    StartOfBlock := CaretXY;

    if i = 0 then begin
      if (eoTabsToSpaces in fOptions) then begin
        i := TabWidth - (StartOfBlock.Char - 1) mod TabWidth;
        if i = 0 then
          i := TabWidth;
      end else
        i := TabWidth;
    end;

    if eoTabsToSpaces in fOptions then begin
      Spaces := StringOfChar(#32, i);
      NewCaretX := StartOfBlock.Char + i;
    end else if (eoTrimTrailingSpaces in Options) and (StartOfBlock.Char > Length(LineText)) then begin
      // work-around for trimming Tabs
      nPhysX := BufferToDisplayPos(CaretXY).Column;
      if (eoSmartTabs in fOptions) and not vIgnoreSmartTabs and (iLine > -1) then begin
        i := BufferToDisplayPos(BufferCoord(MinLen + i, iLine + 1)).Column;
        nDistanceToTab := i - nPhysX;
      end else
        nDistanceToTab := TabWidth - ((nPhysX - 1) mod TabWidth);
      NewCaretX := StartOfBlock.Char + nDistanceToTab;
    end else begin
      if (eoSmartTabs in fOptions) and not vIgnoreSmartTabs and (iLine > -1) then begin
        Spaces := Copy(fLines[CaretXY.Line - 1], 1, CaretXY.Char - 1);
        while Pos(#9, Spaces) > 0 do
          Delete(Spaces, Pos(#9, Spaces), 1);
        Spaces := Trim(Spaces);

        //smart tabs are only in the front of the line *NOT IN THE MIDDLE*
        if Spaces = '' then begin
          i := BufferToDisplayPos(BufferCoord(MinLen + i, iLine + 1)).Column;

          nPhysX := DisplayX;
          nDistanceToTab := i - nPhysX;
          nSpacesToNextTabStop := TabWidth - ((nPhysX - 1) mod TabWidth);
          if nSpacesToNextTabStop <= nDistanceToTab then begin
            Spaces := TSynTabChar;
            Dec(nDistanceToTab, nSpacesToNextTabStop);
          end;
          while nDistanceToTab >= TabWidth do begin
            Spaces := Spaces + TSynTabChar;
            Dec(nDistanceToTab, TabWidth);
          end;
          if nDistanceToTab > 0 then
            Spaces := Spaces + StringOfChar(#32, nDistanceToTab);
        end else
          Spaces := TSynTabChar;
      end else begin
        Spaces := TSynTabChar;
      end;
      if (eoTrimTrailingSpaces in Options) and (Length(TrimTrailingSpaces(LineText)) = 0) then
        NewCaretX := StartOfBlock.Char + GetExpandedLength(Spaces, TabWidth)
      else
        NewCaretX := StartOfBlock.Char + Length(Spaces);
    end;

    SetSelTextPrimitive(Spaces);
    // Undo is already handled in SetSelText when SelectionMode is Column
    if fActiveSelectionMode <> smColumn then begin
      fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, SelText,
        fActiveSelectionMode);
    end;
  finally
    fUndoList.EndBlock;
  end;

  ChangeScroll := not (eoScrollPastEol in fOptions);
  try
    Include(fOptions, eoScrollPastEol);
    InternalCaretX := NewCaretX;
  finally
    if ChangeScroll then
      Exclude(fOptions, eoScrollPastEol);
  end;

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.DoShiftTabKey;
// shift-tab key handling
var
  NewX: integer;
  Line: string;
  LineLen: integer;
  DestX: integer;

  MaxLen, iLine: integer;
  PrevLine, OldSelText: string;
  p: PChar;
  OldCaretXY: TBufferCoord;
begin
  // Provide Visual Studio like block indenting
  if (eoTabIndent in Options) and ((SelTabBlock) or (SelTabLine)) then begin
    DoBlockUnIndent;
    exit;
  end;

  NewX := CaretX;

  if (NewX <> 1) and (eoSmartTabs in fOptions) then begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then begin
      Dec(iLine);
      MaxLen := CaretX - 1;
      repeat
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MaxLen) then begin
          p := @PrevLine[MaxLen];
          // scan over whitespaces
          repeat
            if p^ <> #32 then
              break;
            Dec(NewX);
            Dec(p);
          until NewX = 1;
          // scan over non-whitespaces
          if NewX <> 1 then
            repeat
              if p^ = #32 then
                break;
              Dec(NewX);
              Dec(p);
            until NewX = 1;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end;
  end;

  if NewX = CaretX then begin
    Line := LineText;
    LineLen := Length(Line);

    // find real un-tab position

    DestX := ((CaretX - 2) div TabWidth) * TabWidth + 1;
    if NewX > LineLen then
      NewX := DestX
    else if (NewX > DestX) and ((Line[NewX - 1] = #9)) then
      dec(NewX)
    else begin
      while (NewX > DestX) and ((NewX - 1 > LineLen) or (Line[NewX - 1] = #32)) do
        dec(NewX);
    end;
  end;

  // perform un-tab
  if (NewX <> CaretX) then begin
    SetBlockBegin(BufferCoord(NewX, CaretY));
    SetBlockEnd(CaretXY);
    OldCaretXY := CaretXY;

    OldSelText := SelText;
    SetSelTextPrimitive('');

    fUndoList.AddChange(crSilentDelete, BufferCoord(NewX, CaretY),
      OldCaretXY, OldSelText, smNormal);

    InternalCaretX := NewX;
  end;
end;

procedure TCustomSynEdit.DoComment;
var
  OrigBlockBegin, OrigBlockEnd, OrigCaret: TBufferCoord;
  EndLine, I: integer;
begin
  if not ReadOnly then begin
    DoOnPaintTransient(ttBefore);
    fUndoList.BeginBlock;
    try
      OrigBlockBegin := BlockBegin;
      OrigBlockEnd := BlockEnd;
      OrigCaret := CaretXY;

      // Ignore the last line the cursor is placed on
      if OrigBlockEnd.Char = 1 then
        EndLine := max(OrigBlockBegin.Line - 1, OrigBlockEnd.Line - 2)
      else
        EndLine := OrigBlockEnd.Line - 1;

      for I := OrigBlockBegin.Line - 1 to EndLine do begin
        Lines[i] := '//' + Lines[i];
        fUndoList.AddChange(crInsert,
          BufferCoord(1, i + 1),
          BufferCoord(3, i + 1),
          '', smNormal);
      end;

      // When grouping similar commands, process one comment action per undo/redo
      fUndoList.AddChange(crNothing, BufferCoord(0, 0), BufferCoord(0, 0), '', smNormal);
    finally
      fUndoList.EndBlock;
    end;

    // Move begin of selection
    if OrigBlockBegin.Char > 1 then
      Inc(OrigBlockBegin.Char, 2);

    // Move end of selection
    if OrigBlockEnd.Char > 1 then
      Inc(OrigBlockEnd.Char, 2);

    // Move caret
    if OrigCaret.Char > 1 then
      Inc(OrigCaret.Char, 2);

    SetCaretAndSelection(OrigCaret, OrigBlockBegin, OrigBlockEnd);
  end;
end;

procedure TCustomSynEdit.DoUncomment;
var
  OrigBlockBegin, OrigBlockEnd, OrigCaret: TBufferCoord;
  EndLine, I, J: integer;
  S: AnsiString;
begin
  if not ReadOnly then begin
    DoOnPaintTransient(ttBefore);
    fUndoList.BeginBlock;
    try
      OrigBlockBegin := BlockBegin;
      OrigBlockEnd := BlockEnd;
      OrigCaret := CaretXY;

      // Ignore the last line the cursor is placed on
      if OrigBlockEnd.Char = 1 then
        EndLine := max(OrigBlockBegin.Line - 1, OrigBlockEnd.Line - 2)
      else
        EndLine := OrigBlockEnd.Line - 1;

      for I := OrigBlockBegin.Line - 1 to EndLine do begin
        S := Lines[i];
        // Find // after blanks only
        J := 1;
        while (J + 1 <= length(S)) and (S[j] in [#0..#32]) do
          Inc(J);
        if (j + 1 <= length(S)) and (S[j] = '/') and (S[j + 1] = '/') then begin
          Delete(S, J, 2);
          Lines[i] := S;

          fUndoList.AddChange(crDelete,
            BufferCoord(J, i + 1),
            BufferCoord(J + 2, i + 1),
            '//', smNormal);

          // Move begin of selection
          if (I = OrigBlockBegin.Line - 1) and (OrigBlockBegin.Char > 1) then
            Dec(OrigBlockBegin.Char, 2);

          // Move end of selection
          if (I = OrigBlockEnd.Line - 1) and (OrigBlockEnd.Char > 1) then
            Dec(OrigBlockEnd.Char, 2);

          // Move caret
          if (I = OrigCaret.Line - 1) and (OrigCaret.Char > 1) then
            Dec(OrigCaret.Char, 2);
        end;
      end;

      // When grouping similar commands, process one uncomment action per undo/redo
      fUndoList.AddChange(crNothing, BufferCoord(0, 0), BufferCoord(0, 0), '', smNormal);

      CaretXY := OrigCaret;
      BlockBegin := OrigBlockBegin;
      BlockEnd := OrigBlockEnd;
    finally
      fUndoList.EndBlock;
    end;
  end;
end;

procedure TCustomSynEdit.DoHomeKey(Selection: boolean);
var
  newX: integer;
  first_nonblank: integer;
  s: string;
  vMaxX: Integer;
begin
  // home key enhancement
  if (eoEnhanceHomeKey in fOptions) and (LineToRow(CaretY) = DisplayY) then begin
    s := fLines[CaretXY.Line - 1];

    first_nonblank := 1;
    vMaxX := Length(s);
    while (first_nonblank <= vMaxX) and (s[first_nonblank] in [#32, #9]) do
      inc(first_nonblank);
    dec(first_nonblank);

    newX := CaretXY.Char - 1;

    if (newX > first_nonblank) or (newX = 0) then
      newX := first_nonblank + 1
    else
      newX := 1;
  end else
    newX := 1;

  MoveCaretAndSelection(CaretXY, BufferCoord(newX, CaretY), Selection);
end;

procedure TCustomSynEdit.DoEndKey(aSelection: Boolean);

  function CaretInLastRow: Boolean;
  begin
    Result := True;
  end;

  function FirstCharInRow: Integer;
  var
    vPos: TDisplayCoord;
  begin
    vPos.Row := DisplayY;
    vPos.Column := 1;
    Result := DisplayToBufferPos(vPos).Char;
  end;

var
  vText: string;
  vLastNonBlank: Integer;
  vNewX: Integer;
  vMinX: Integer;
begin
  if (eoEnhanceEndKey in fOptions) and CaretInLastRow then begin
    vText := LineText;
    vLastNonBlank := Length(vText);
    vMinX := 0;
    while (vLastNonBlank > vMinX) and (vText[vLastNonBlank] in [#32, #9]) do
      Dec(vLastNonBlank);

    vNewX := CaretX - 1;
    if vNewX = vLastNonBlank then
      vNewX := Length(LineText) + 1
    else
      vNewX := vLastNonBlank + 1;
  end else begin
    vNewX := Length(LineText) + 1;
  end;

  MoveCaretAndSelection(CaretXY, BufferCoord(vNewX, CaretY), aSelection);
end;

procedure TCustomSynEdit.CreateWnd;
begin
  inherited;
  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, TRUE);
  UpdateScrollBars;
end;

procedure TCustomSynEdit.DestroyWnd;
begin
  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, FALSE);
  inherited;
end;

procedure TCustomSynEdit.InvalidateRect(const aRect: TRect; aErase: boolean);
begin
  if not (fPainterLock>0) then
    Windows.InvalidateRect(Handle, @aRect, aErase);
end;

procedure TCustomSynEdit.DoBlockIndent;
var
  OrgCaretPos: TBufferCoord;
  BB, BE: TBufferCoord;
  Run,
    StrToInsert: PChar;
  e, x,
    i, InsertStrLen: integer;
  Spaces: string;
  OrgSelectionMode: TSynSelectionMode;
  InsertionPos: TBufferCoord;
begin
  OrgSelectionMode := fActiveSelectionMode;
  OrgCaretPos := CaretXY;

  StrToInsert := nil;
  if SelAvail then try
    // keep current selection detail
    BB := BlockBegin;
    BE := BlockEnd;

    // build text to insert
    if (BE.Char = 1) then begin
      e := BE.Line - 1;
      x := 1;
    end else begin
      e := BE.Line;
      if eoTabsToSpaces in Options then
        x := CaretX + FTabWidth
      else
        x := CaretX + 1;
    end;
    if (eoTabsToSpaces in Options) then begin
      InsertStrLen := (FTabWidth + 2) * (e - BB.Line) + FTabWidth + 1;
      //               chars per line * lines-1    + last line + null char
      StrToInsert := StrAlloc(InsertStrLen);
      Run := StrToInsert;
      Spaces := StringOfChar(#32, FTabWidth);
    end else begin
      InsertStrLen := 3 * (e - BB.Line) + 2;
      //         #9#13#10 * lines-1 + (last line's #9 + null char)
      StrToInsert := StrAlloc(InsertStrLen);
      Run := StrToInsert;
      Spaces := #9;
    end;
    for i := BB.Line to e - 1 do begin
      StrPCopy(Run, Spaces + #13#10);
      Inc(Run, length(spaces) + 2);
    end;
    StrPCopy(Run, Spaces);

    fUndoList.BeginBlock;
    try
      InsertionPos.Line := BB.Line;
      if fActiveSelectionMode = smColumn then
        InsertionPos.Char := Min(BB.Char, BE.Char)
      else
        InsertionPos.Char := 1;
      InsertBlock(InsertionPos, InsertionPos, StrToInsert, True);
      fUndoList.AddChange(crIndent, BB, BE, '', smColumn);
      //We need to save the position of the end block for redo
      fUndoList.AddChange(crIndent,
        BufferCoord(BB.Char + length(Spaces), BB.Line),
        BufferCoord(BE.Char + length(Spaces), BE.Line),
        '', smColumn);
    finally
      fUndoList.EndBlock;
    end;

    //adjust the x position of orgcaretpos appropriately
    OrgCaretPos.Char := X;
  finally
    if BE.Char > 1 then
      Inc(BE.Char, Length(Spaces));
    StrDispose(StrToInsert);
    SetCaretAndSelection(OrgCaretPos,
      BufferCoord(BB.Char + Length(Spaces), BB.Line), BE);
    ActiveSelectionMode := OrgSelectionMode;
  end;
end;

procedure TCustomSynEdit.DoBlockUnindent;
var
  OrgCaretPos,
    BB, BE: TBufferCoord;
  Line, Run,
    FullStrToDelete,
    StrToDelete: PChar;
  Len,
    x, StrToDeleteLen,
    i, TmpDelLen,
    FirstIndent,
    LastIndent,
    e: integer;
  TempString: string;
  OrgSelectionMode: TSynSelectionMode;
  SomethingToDelete: Boolean;

  function GetDelLen: integer;
  var
    Run: PChar;
  begin
    Result := 0;
    Run := Line;
    //Take care of tab character
    if (Run[0] = #9) then begin
      Result := 1;
      SomethingToDelete := True;
      exit;
    end;
    //Deal with compound tabwidths  Sometimes they have TabChars after a few
    //spaces, yet we need to delete the whole tab width even though the char
    //count might not be FTabWidth because of the TabChar
    while (Run[0] = #32) and (Result < FTabWidth) do begin
      Inc(Result);
      Inc(Run);
      SomethingToDelete := True;
    end;
    if (Run[0] = #9) and (Result < FTabWidth) then
      Inc(Result);
  end;

begin
  OrgSelectionMode := fActiveSelectionMode;
  Len := 0;
  LastIndent := 0;
  if SelAvail then begin
    // store current selection detail
    BB := BlockBegin;
    BE := BlockEnd;
    OrgCaretPos := CaretXY;
    x := fCaretX;

    // convert selection to complete lines
    if BE.Char = 1 then
      e := BE.Line - 1
    else
      e := BE.Line;

    // build string to delete
    StrToDeleteLen := (FTabWidth + 2) * (e - BB.Line) + FTabWidth + 1;
    //                chars per line * lines-1    + last line + null char
    StrToDelete := StrAlloc(StrToDeleteLen);
    StrToDelete[0] := #0;
    SomethingToDelete := False;
    for i := BB.Line to e - 1 do begin
      Line := PChar(Lines[i - 1]);
      //'Line' is 0-based, 'BB.x' is 1-based, so the '-1'
      //And must not increment 'Line' pointer by more than its 'Length'
      if fActiveSelectionMode = smColumn then
        Inc(Line, MinIntValue([BB.Char - 1, BE.Char - 1, Length(Lines[i - 1])]));
      //Instead of doing a StringOfChar, we need to get *exactly* what was
      //being deleted incase there is a TabChar
      TmpDelLen := GetDelLen;
      StrCat(StrToDelete, PChar(Copy(Line, 1, TmpDelLen)));
      StrCat(StrToDelete, PChar(#13#10));
      if (fCaretY = i) and (x <> 1) then
        x := x - TmpDelLen;
    end;
    Line := PChar(Lines[e - 1]);
    if fActiveSelectionMode = smColumn then
      Inc(Line, MinIntValue([BB.Char - 1, BE.Char - 1, Length(Lines[e - 1])]));
    TmpDelLen := GetDelLen;
    StrCat(StrToDelete, PChar(Copy(Line, 1, TmpDelLen)));
    if (fCaretY = e) and (x <> 1) then
      x := x - TmpDelLen;

    FirstIndent := -1;
    FullStrToDelete := nil;
    // Delete string
    if SomethingToDelete then begin
      FullStrToDelete := StrToDelete;
      InternalCaretY := BB.Line;
      if fActiveSelectionMode <> smColumn then
        i := 1
      else
        i := Min(BB.Char, BE.Char);
      repeat
        Run := GetEOL(StrToDelete);
        if Run <> StrToDelete then begin
          Len := Run - StrToDelete;
          if FirstIndent = -1 then
            FirstIndent := Len;
          if Len > 0 then begin
            TempString := Lines[CaretY - 1];
            Delete(TempString, i, Len);
            Lines[CaretY - 1] := TempString;
          end;
        end;
        if Run^ = #13 then begin
          Inc(Run);
          if Run^ = #10 then
            Inc(Run);
          Inc(fCaretY);
        end;
        StrToDelete := Run;
      until Run^ = #0;
      LastIndent := Len;
      fUndoList.AddChange(crUnindent, BB, BE, FullStrToDelete, fActiveSelectionMode);
    end;
    // restore selection
    if FirstIndent = -1 then
      FirstIndent := 0;
    //adjust the x position of orgcaretpos appropriately
    if fActiveSelectionMode = smColumn then
      SetCaretAndSelection(OrgCaretPos, BB, BE)
    else begin
      OrgCaretPos.Char := X;
      Dec(BB.Char, FirstIndent);
      Dec(BE.Char, LastIndent);
      SetCaretAndSelection(OrgCaretPos, BB, BE);
    end;
    ActiveSelectionMode := OrgSelectionMode;
    if FullStrToDelete <> nil then
      StrDispose(FullStrToDelete)
    else
      StrDispose(StrToDelete);
  end;
end;

function TCustomSynEdit.ExecuteAction(Action: TBasicAction): boolean;
begin
  if Action is TEditAction then begin
    Result := TRUE;
    if Action is TEditCut then
      CutToClipboard
    else if Action is TEditCopy then
      CopyToClipboard
    else if Action is TEditPaste then
      PasteFromClipboard
    else if Action is TEditDelete then
      ClearSelection
    else if Action is TEditUndo then
      Undo
    else if Action is TEditSelectAll then
      SelectAll;
  end else
    Result := inherited ExecuteAction(Action);
end;

function TCustomSynEdit.UpdateAction(Action: TBasicAction): boolean;
begin
  if Action is TEditAction then begin
    Result := Focused;
    if Result then begin
      if (Action is TEditCut) or (Action is TEditCopy) then
        TEditAction(Action).Enabled := SelAvail
      else if Action is TEditPaste then
        TEditAction(Action).Enabled := CanPaste
      else if Action is TEditDelete then
        TEditAction(Action).Enabled := SelAvail
      else if Action is TEditUndo then
        TEditAction(Action).Enabled := CanUndo
      else if Action is TEditSelectAll then
        TEditAction(Action).Enabled := TRUE;
    end;
  end else
    Result := inherited UpdateAction(Action);
end;

procedure TCustomSynEdit.SetModified(Value: boolean);
begin
  if Value then
    fLastModifyTime := Now;
  if Value <> fModified then begin
    fModified := Value;
    if (eoGroupUndo in Options) and (not Value) and UndoList.CanUndo then
      UndoList.AddGroupBreak;
    UndoList.InitialState := not Value;
    StatusChanged([scModified]);
  end;
end;

function TCustomSynEdit.DoOnSpecialLineColors(Line: integer; var Foreground,
  Background: TColor): boolean;
begin
  Result := FALSE;
  if Assigned(fOnSpecialLineColors) then
    fOnSpecialLineColors(Self, Line, Result, Foreground, Background);
end;

procedure TCustomSynEdit.ClearAreaList(areaList:TList);
var
  i:integer;
begin
  for i:=0 to areaList.Count-1 do begin
    dispose(PEditingArea(areaList[i]));
  end;
  areaList.Clear;
end;

function TCustomSynEdit.DoOnEditAreas(Line:integer; areaList:TList; var borderColor: TColor;var areaType:TEditingAreaType): boolean;
begin
  Result := FALSE;
  clearAreaList(areaList);
  if Assigned(fOnEditingAreas) then begin
    fOnEditingAreas(Self, Line, areaList, borderColor, areaType);
    Result := areaList.Count > 0;
  end;
end;

procedure TCustomSynEdit.InvalidateLine(Line: integer);
var
  rcInval: TRect;
begin
  if (fPainterLock>0) then
    Exit;
  if (not HandleAllocated) or (Line < 1) or (Line > Lines.Count) or (not Visible) then
    Exit;

  // invalidate text area of this line
  if UseCodeFolding then
    Line := LineToRow(Line);

  if (Line >= TopLine) and (Line <= TopLine + LinesInWindow) then begin
    rcInval := Rect(fGutterWidth, fTextHeight * (Line - TopLine), ClientWidth, 0);
    rcInval.Bottom := rcInval.Top + fTextHeight;
    if sfLinesChanging in fStateFlags then
      UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
    else
      InvalidateRect(rcInval, False);
  end;
end;

function TCustomSynEdit.GetReadOnly: boolean;
begin
  Result := fReadOnly;
end;

procedure TCustomSynEdit.SetReadOnly(Value: boolean);
begin
  if fReadOnly <> Value then begin
    fReadOnly := Value;
    StatusChanged([scReadOnly]);
  end;
end;

procedure TCustomSynEdit.FindMatchingBracket;
begin
  InternalCaretXY := GetMatchingBracket;
end;

function TCustomSynEdit.GetMatchingBracket: TBufferCoord;
begin
  Result := GetMatchingBracketEx(CaretXY);
end;


function TCustomSynEdit.GetPreviousLeftBracket(x:Integer; y: Integer): TBufferCoord;
var
  Line: string;
  PosX, PosY: integer;
  Test: char;
  NumBrackets: integer;
  vDummy: string;
  attr: TSynHighlighterAttributes;
  p: TBufferCoord;
  isCommentOrStringOrChar: boolean;
begin
  Result.Char := 0;
  Result.Line := 0;
  // get char at caret
  PosX := x-1;
  PosY := y;
  if PosX<1 then
    dec(PosY);
  if PosY<1 then
    Exit;
  Line := Lines[PosY - 1];
  if (PosX > Length(Line)) or (PosX<1) then
    PosX := Length(Line);
  numBrackets := 1;
  while True do begin
    if Length(Line)=0 then begin;
      dec(PosY);
      if PosY<1 then
        Exit;
      Line := Lines[PosY - 1];
      PosX := Length(Line);      
      continue;
    end;
    Test := Line[PosX];
    p.Char := PosX;
    p.Line := PosY;
    if Test in ['{','}'] then begin
      if GetHighlighterAttriAtRowCol(p, vDummy, attr) then
        isCommentOrStringOrChar :=
           (attr = Highlighter.StringAttribute) or (attr = Highlighter.CommentAttribute) or (attr.Name
                    =
                    'Character')
      else
        isCommentOrStringOrChar := false;
      if (Test = '{') and (not isCommentOrStringOrChar) then
        dec(NumBrackets)
      else if (Test = '}') and (not isCommentOrStringOrChar) then
        inc(NumBrackets);
      if NumBrackets = 0 then begin
        // matching bracket found, set caret and bail out
        Result := p;
        exit;
      end;
    end;
    dec(PosX);
    if PosX<1 then begin
      dec(PosY);
      if PosY<1 then
        Exit;
      Line := Lines[PosY - 1];
      PosX := Length(Line);
    end;
  end;
end;

function TCustomSynEdit.GetMatchingBracketEx(
  const APoint: TBufferCoord): TBufferCoord;
const
  Brackets: array[0..7] of char = ('(', ')', '[', ']', '{', '}', '<', '>');
var
  Line: string;
  i, PosX, PosY, Len: integer;
  Test, BracketInc, BracketDec: char;
  NumBrackets: integer;
  vDummy: string;
  attr: TSynHighlighterAttributes;
  p: TBufferCoord;
  isCommentOrStringOrChar: boolean;
begin
  Result.Char := 0;
  Result.Line := 0;
  // get char at caret
  PosX := APoint.Char;
  PosY := APoint.Line;
  Line := Lines[APoint.Line - 1];
  if Length(Line) >= PosX then begin
    Test := Line[PosX];
    // is it one of the recognized brackets?
    for i := Low(Brackets) to High(Brackets) do
      if Test = Brackets[i] then begin
        // this is the bracket, get the matching one and the direction
        BracketInc := Brackets[i];
        BracketDec := Brackets[i xor 1]; // 0 -> 1, 1 -> 0, ...
        // search for the matching bracket (that is until NumBrackets = 0)
        NumBrackets := 1;
        if Odd(i) then begin
          repeat
            // search until start of line
            while PosX > 1 do begin
              Dec(PosX);
              Test := Line[PosX];
{$IFDEF SYN_MBCSSUPPORT}
              if (Test in LeadBytes) then begin
                Dec(PosX);
                Continue;
              end;
{$ENDIF}
              p.Char := PosX;
              p.Line := PosY;
              if (Test = BracketInc) or (Test = BracketDec) then begin
                if GetHighlighterAttriAtRowCol(p, vDummy, attr) then
                  isCommentOrStringOrChar :=
                    (attr = Highlighter.StringAttribute) or (attr = Highlighter.CommentAttribute) or (attr.Name
                    =
                    'Character')
                else
                  isCommentOrStringOrChar := false;
                if (Test = BracketInc) and (not isCommentOrStringOrChar) then
                  Inc(NumBrackets)
                else if (Test = BracketDec) and (not isCommentOrStringOrChar) then begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then begin
                    // matching bracket found, set caret and bail out
                    Result := P;
                    exit;
                  end;
                end;
              end;
            end;
            // get previous line if possible
            if PosY = 1 then
              break;
            Dec(PosY);
            Line := Lines[PosY - 1];
            PosX := Length(Line) + 1;
          until FALSE;
        end else begin
          repeat
            // search until end of line
            Len := Length(Line);
            while PosX < Len do begin
              Inc(PosX);
              Test := Line[PosX];
{$IFDEF SYN_MBCSSUPPORT}
              if (Test in LeadBytes) then begin
                Inc(PosX);
                Continue;
              end;
{$ENDIF}
              p.Char := PosX;
              p.Line := PosY;
              if (Test = BracketInc) or (Test = BracketDec) then begin
                if GetHighlighterAttriAtRowCol(p, vDummy, attr) then
                  isCommentOrStringOrChar :=
                    (attr = Highlighter.StringAttribute) or (attr = Highlighter.CommentAttribute) or (attr.Name
                    =
                    'Character')
                else
                  isCommentOrStringOrChar := false;
                if (Test = BracketInc) and (not isCommentOrStringOrChar) then
                  Inc(NumBrackets)
                else if (Test = BracketDec) and (not isCommentOrStringOrChar) then begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then begin
                    // matching bracket found, set caret and bail out
                    Result := P;
                    exit;
                  end;
                end;
              end;
            end;
            // get next line if possible
            if PosY = Lines.Count then
              Break;
            Inc(PosY);
            Line := Lines[PosY - 1];
            PosX := 0;
          until False;
        end;
        // don't test the other brackets, we're done
        break;
      end;
  end;
end;



function TCustomSynEdit.GetHighlighterAttriAtRowCol(const XY: TBufferCoord;
  var Token: string; var Attri: TSynHighlighterAttributes): boolean;
var
  TmpType: TSynhighlighterTokenType;
  TmpKind, TmpStart: Integer;
begin
  Result := GetHighlighterAttriAtRowColEx(XY, Token, TmpType, TmpKind,TmpStart, Attri);
end;

function TCustomSynEdit.GetHighlighterAttriAtRowColEx(const XY: TBufferCoord;
  var Token: string; var TokenType: TSynHighlighterTokenType ; var TokenKind,Start: Integer;
  var Attri: TSynHighlighterAttributes): boolean;
var
  PosX, PosY: integer;
  Line: string;
begin
  PosY := XY.Line - 1;
  if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then begin
    Line := Lines[PosY];
    if PosY = 0 then begin
      fHighlighter.ResetRange;
      fHighlighter.ResetParenthesisLevel;
      fHighlighter.ResetBracketLevel;
      fHighlighter.ResetBraceLevel;
    end else begin
      fHighlighter.SetRange(Lines.Ranges[PosY - 1]);
      fHighlighter.SetParenthesisLevel(Lines.ParenthesisLevels[PosY - 1]);
      fHighlighter.SetBracketLevel(Lines.BracketLevels[PosY - 1]);
      fHighlighter.SetBraceLevel(Lines.BraceLevels[PosY - 1]);
    end;
    Highlighter.SetLine(Line, PosY);
    PosX := XY.Char;
    if (PosX > 0) and (PosX <= Length(Line)) then
      while not Highlighter.GetEol do begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if ((PosX >= Start) and (PosX < Start + Length(Token))) then begin
          Attri := Highlighter.GetTokenAttribute;
          TokenKind := Highlighter.GetTokenKind;
          TokenType := Highlighter.GetTokenType;
          Result := TRUE;
          exit;
        end;
        Highlighter.Next;
      end;
  end;
  Token := '';
  Attri := nil;
  Result := FALSE;
end;


function TCustomSynEdit.GetHighlighterAttriAtRowCol(const XY: TBufferCoord;
  var Token: string; var TokenFinished: boolean;
  var TokenType: TSynHighlighterTokenType;
  var Attri: TSynHighlighterAttributes): boolean;
var
  PosX, PosY, endPos, Start: integer;
  Line: string;
begin
  PosY := XY.Line - 1;
  if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then begin
    Line := Lines[PosY];
    if PosY = 0 then begin
      fHighlighter.ResetRange;
      fHighlighter.ResetParenthesisLevel;
      fHighlighter.ResetBracketLevel;
      fHighlighter.ResetBraceLevel;
    end else begin
      fHighlighter.SetRange(Lines.Ranges[PosY - 1]);
      fHighlighter.SetParenthesisLevel(Lines.ParenthesisLevels[PosY - 1]);
      fHighlighter.SetBracketLevel(Lines.BracketLevels[PosY - 1]);
      fHighlighter.SetBraceLevel(Lines.BraceLevels[PosY - 1]);
    end;
    Highlighter.SetLine(Line, PosY);
    PosX := XY.Char;
    if (PosX > 0) and (PosX <= Length(Line)) then
      while not Highlighter.GetEol do begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        endPos := Start + Length(Token)-1;
        if ((PosX >= Start) and (PosX <= endPos)) then begin
          Attri := Highlighter.GetTokenAttribute;
          if PosX = endPos then
            TokenFinished := Highlighter.GetTokenFinished
          else
            TokenFinished := False;
          TokenType := Highlighter.GetTokenType;
          Result := TRUE;
          exit;
        end;
        Highlighter.Next;
      end;
  end;
  Token := '';
  Attri := nil;
  TokenFinished := False;
  Result := FALSE;
end;

function TCustomSynEdit.FindHookedCmdEvent(AHandlerProc: THookedCommandEvent):
  integer;
var
  Entry: THookedCommandHandlerEntry;
begin
  Result := GetHookedCommandHandlersCount - 1;
  while Result >= 0 do begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[Result]);
    if Entry.Equals(AHandlerProc) then
      break;
    Dec(Result);
  end;
end;

function TCustomSynEdit.GetHookedCommandHandlersCount: integer;
begin
  if Assigned(fHookedCommandHandlers) then
    Result := fHookedCommandHandlers.Count
  else
    Result := 0;
end;

procedure TCustomSynEdit.RegisterCommandHandler(
  const AHandlerProc: THookedCommandEvent; AHandlerData: pointer);
begin
  if not Assigned(AHandlerProc) then begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in RegisterCommandHandler');
{$ENDIF}
    exit;
  end;
  if not Assigned(fHookedCommandHandlers) then
    fHookedCommandHandlers := TList.Create;
  if FindHookedCmdEvent(AHandlerProc) = -1 then
    fHookedCommandHandlers.Add(THookedCommandHandlerEntry.Create(
      AHandlerProc, AHandlerData))
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) already registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.UnregisterCommandHandler(AHandlerProc:
  THookedCommandEvent);
var
  i: integer;
begin
  if not Assigned(AHandlerProc) then begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in UnregisterCommandHandler');
{$ENDIF}
    exit;
  end;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then begin
    THookedCommandHandlerEntry(fHookedCommandHandlers[i]).Free;
    fHookedCommandHandlers.Delete(i);
  end else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) is not registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.NotifyHookedCommandHandlers(AfterProcessing: boolean;
  var Command: TSynEditorCommand; var AChar: char; Data: pointer);
var
  Handled: boolean;
  i: integer;
  Entry: THookedCommandHandlerEntry;
begin
  Handled := FALSE;
  for i := 0 to GetHookedCommandHandlersCount - 1 do begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[i]);
    // NOTE: Command should NOT be set to ecNone, because this might interfere
    // with other handlers.  Set Handled to False instead (and check its value
    // to not process the command twice).
    Entry.fEvent(Self, AfterProcessing, Handled, Command, AChar, Data,
      Entry.fData);
  end;
  if Handled then
    Command := ecNone;
end;

procedure TCustomSynEdit.DoOnClearBookmark(var Mark: TSynEditMark);
begin
  if Assigned(fOnClearMark) then
    fOnClearMark(Self, Mark);
end;

procedure TCustomSynEdit.DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean);
var
  DoTransient: Boolean;
begin
  DoTransient := (FPaintTransientLock = 0);
  if Lock then begin
    if (TransientType = ttBefore) then
      inc(FPaintTransientLock)
    else begin
      dec(FPaintTransientLock);
      DoTransient := (FPaintTransientLock = 0);
    end;
  end;

  if DoTransient and Assigned(fOnPaintTransient) then begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    HideCaret;
    try
      fOnPaintTransient(Self, Canvas, TransientType);
    finally
      ShowCaret;
    end;
  end;
end;

procedure TCustomSynEdit.DoOnPaintTransient(TransientType: TTransientType);
begin
  DoOnPaintTransientEx(TransientType, false);
end;

procedure TCustomSynEdit.DoOnPaint;
begin
  if Assigned(fOnPaint) then begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    fOnPaint(Self, Canvas);
  end;
end;

procedure TCustomSynEdit.DoOnPlaceMark(var Mark: TSynEditMark);
begin
  if Assigned(fOnPlaceMark) then
    fOnPlaceMark(Self, Mark);
end;

function TCustomSynEdit.DoOnReplaceText(const ASearch, AReplace: string;
  Line, Column,wordlen: integer): TSynReplaceAction;
begin
  Result := raCancel;
  if Assigned(fOnReplaceText) then
    fOnReplaceText(Self, ASearch, AReplace, Line, Column,wordlen, Result);
end;

procedure TCustomSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if Assigned(fOnStatusChange) then begin
    fOnStatusChange(Self, fStatusChanges);
    fStatusChanges := [];
  end;
end;

procedure TCustomSynEdit.UpdateModifiedStatus;
begin
  Modified := not UndoList.InitialState;
end;

procedure TCustomSynEdit.UndoRedoAdded(Sender: TObject);
begin
  UpdateModifiedStatus;

  // we have to clear the redo information, since adding undo info removes
  // the necessary context to undo earlier edit actions
  if (Sender = fUndoList) and not fUndoList.InsideRedo and
    (fUndoList.PeekItem <> nil) and (fUndoList.PeekItem.ChangeReason <> crGroupBreak) then
    fRedoList.Clear;
  if TSynEditUndoList(Sender).BlockCount = 0 then
    DoChange;
end;

function TCustomSynEdit.GetWordAtRowCol(XY: TBufferCoord): string;
var
  Line: string;
  Len, Stop, Start: Integer;
begin
  Result := '';
  if (XY.Line >= 1) and (XY.Line <= Lines.Count) then begin
    Line := Lines[XY.Line - 1];
    Len := Length(Line);
    if Len = 0 then
      Exit;

    if (XY.Char < 1) or (XY.Char > Len) then
      Exit;

    Start := XY.Char;
    if  (Start> 1) and not (Line[Start] in IdentChars) then
      Dec(Start);

    if Line[Start] in IdentChars then begin
      Stop := Start;
      while (Stop <= Len) and (Line[Stop] in IdentChars) do
        Inc(Stop);
      while (Start >= 1) and (Line[Start - 1] in IdentChars) do
        Dec(Start);
      if Stop > Start then
        Result := Copy(Line, Start, Stop - Start);
    end;
  end;
end;

function TCustomSynEdit.BufferToDisplayPos(const p: TBufferCoord): TDisplayCoord;
// BufferToDisplayPos takes a position in the text and transforms it into
// the row and column it appears to be on the screen
var
  s: string;
  i, L: integer;
  x: integer;
begin
  Result := TDisplayCoord(p);

  // Account for tabs
  if p.Line - 1 < Lines.Count then begin
    s := Lines[p.Line - 1];
    l := Length(s);
    x := 0;
    for i := 1 to p.Char - 1 do begin
      if (i <= l) and (s[i] = TSynTabChar) then
        inc(x, TabWidth - (x mod TabWidth))
      else
        inc(x);
    end;
    Result.Column := x + 1;
  end;

  // Account for code folding
  if UseCodeFolding then
    Result.Row := FoldLineToRow(Result.Row);
end;

function TCustomSynEdit.DisplayToBufferPos(const p: TDisplayCoord): TBufferCoord;
// DisplayToBufferPos takes a position on screen and transfrom it
// into position of text
var
  s: string;
  i, L: integer;
  x: integer;
begin
  Result := TBufferCoord(p);

  // Account for code folding
  if UseCodeFolding then
    Result.Line := FoldRowToLine(Result.Line);

  // Account for tabs
  if Result.Line <= lines.Count then begin
    s := Lines[Result.Line - 1];
    l := Length(s);
    x := 0;
    i := 0;

    while x < Result.Char do begin
      inc(i);
      if (i <= l) and (s[i] = TSynTabChar) then
        inc(x, TabWidth - (x mod TabWidth))
      else
        inc(x);
    end;
    Result.Char := i;
  end;
{$IFDEF SYN_MBCSSUPPORT}
  if (Result.Line >= 1) and (Result.Line <= Lines.Count) then begin
    s := Lines[Result.Line - 1];
    if (Length(s) >= Result.Char) and (ByteType(s, Result.Char) = mbTrailByte) then
      Inc(Result.Char);
  end;
{$ENDIF}
end;

procedure TCustomSynEdit.DoLinesDeleted(FirstLine, Count: integer);
var
  i: integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do begin
    if Marks[i].Line >= FirstLine + Count then
      Marks[i].Line := Marks[i].Line - Count
    else if Marks[i].Line > FirstLine then
      Marks[i].Line := FirstLine;
  end;
  // plugins
  if fPlugins <> nil then begin
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).LinesDeleted(FirstLine, Count);
  end;
end;

procedure TCustomSynEdit.DoLinesInserted(FirstLine, Count: integer);
var
  i: integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do begin
    if Marks[i].Line >= FirstLine then
      Marks[i].Line := Marks[i].Line + Count;
  end;
  // plugins
  if fPlugins <> nil then begin
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).LinesInserted(FirstLine, Count);
  end;
end;

procedure TCustomSynEdit.PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: integer);
var
  i: integer;
begin
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do begin
      TSynEditPlugin(fPlugins[i]).AfterPaint(ACanvas, AClip, FirstLine,
        LastLine);
    end;
end;

procedure TCustomSynEdit.ProperSetLine(ALine: integer; const ALineText: string);
begin
  if eoTrimTrailingSpaces in Options then
    Lines[ALine] := TrimTrailingSpaces(ALineText)
  else
    Lines[ALine] := ALineText;
end;

procedure TCustomSynEdit.AddKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.AddKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.RemoveKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.AddKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.RemoveKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyPressHandler(aHandler: TKeyPressEvent);
begin
  fKbdHandler.AddKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyPressHandler(aHandler: TKeyPressEvent);
begin
  fKbdHandler.RemoveKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.AddFocusControl(aControl: TWinControl);
begin
  fFocusList.Add(aControl);
end;

procedure TCustomSynEdit.RemoveFocusControl(aControl: TWinControl);
begin
  fFocusList.Remove(aControl);
end;

function TCustomSynEdit.IdentChars: TSynIdentChars;
begin
  if Highlighter <> nil then
    Result := Highlighter.IdentChars
  else
    Result := [#33..#255];
end;

procedure TCustomSynEdit.SetSearchEngine(Value: TSynEditSearchCustom);
begin
  if (fSearchEngine <> Value) then begin
    fSearchEngine := Value;
    if Assigned(fSearchEngine) then
      fSearchEngine.FreeNotification(Self);
  end;
end;

function TCustomSynEdit.NextWordPos: TBufferCoord;
begin
  Result := NextWordPosEx(CaretXY);
end;

function TCustomSynEdit.WordStart: TBufferCoord;
begin
  Result := WordStartEx(CaretXY);
end;

function TCustomSynEdit.WordEnd: TBufferCoord;
begin
  Result := WordEndEx(CaretXY);
end;

function TCustomSynEdit.PrevWordPos: TBufferCoord;
begin
  Result := PrevWordPosEx(CaretXY);
end;

function TCustomSynEdit.GetLineOfMouse(out line: integer): boolean;
var
  Point: TPoint;
  aPos: TBufferCoord;
begin
  GetCursorPos(Point); // mouse position (on screen)
  Point := Self.ScreenToClient(Point); // convert to SynEdit coordinates
  { Make sure it fits within the SynEdit bounds }
  if (Point.X < 0) or (Point.Y < 0) or (Point.X > Self.Width) or (Point.Y > Self.Height) then begin
    Result := False;
    EXIT;
  end;

  { inside the editor, get the word under the mouse pointer }
  aPos := DisplayToBufferPos(PixelsToRowColumn(Point.X, Point.Y));
  line := aPos.Line;
  Result := True;
end;

function TCustomSynEdit.GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
// Get XY caret position of mouse. Returns False if point is outside the
// region of the SynEdit control.
var
  Point: TPoint;
begin
  GetCursorPos(Point); // mouse position (on screen)
  Point := Self.ScreenToClient(Point); // convert to SynEdit coordinates
  { Make sure it fits within the SynEdit bounds }
  if (Point.X < Self.GutterWidth) or (Point.Y < 0) or (Point.X > Self.Width) or (Point.Y > Self.Height) then begin
    Result := False;
    EXIT;
  end;

  { inside the editor, get the word under the mouse pointer }
  aPos := DisplayToBufferPos(PixelsToRowColumn(Point.X, Point.Y));
  Result := True;
end;

function TCustomSynEdit.GetWordAtMouse: string;
var
  Point: TBufferCoord;
begin
  { Return the word under the mouse }
  if GetPositionOfMouse(Point) then // if point is valid
    Result := GetWordAtRowCol(Point); // return the point at the mouse position
end;

function TCustomSynEdit.CharIndexToRowCol(Index: integer): TBufferCoord;
{ Index is 0-based; Result.x and Result.y are 1-based }
var
  x, y, Chars: integer;
begin
  x := 0;
  y := 0;
  Chars := 0;
  while y < Lines.Count do begin
    x := Length(Lines[y]);
    if Chars + x + 2 > Index then begin
      x := Index - Chars;
      break;
    end;
    Inc(Chars, x + 2);
    x := 0;
    Inc(y);
  end;
  Result.Char := x + 1;
  Result.Line := y + 1;
end;

function TCustomSynEdit.RowColToCharIndex(RowCol: TBufferCoord): integer;
{ Row and Col are 1-based; Result is 0 based }
var
  i: integer;
begin
  Result := 0;
  RowCol.Line := Min(Lines.Count, RowCol.Line) - 1;
  for i := 0 to RowCol.Line - 1 do
    Result := Result + Length(Lines[i]) + 2;
  Result := Result + (RowCol.Char - 1);
end;

procedure TCustomSynEdit.Clear;
{ just to attain interface compatibility with TMemo }
begin
  ClearAll;
end;

function TCustomSynEdit.GetSelLength: integer;
begin
  if SelAvail then
    Result := RowColToCharIndex(BlockEnd) - RowColToCharIndex(BlockBegin)
  else
    Result := 0;
end;

procedure TCustomSynEdit.SetSelLength(const Value: integer);
var
  iNewCharIndex: integer;
  iNewBegin: TBufferCoord;
  iNewEnd: TBufferCoord;
begin
  iNewCharIndex := RowColToCharIndex(BlockBegin) + Value;
  if (Value >= 0) or (iNewCharIndex < 0) then begin
    if iNewCharIndex < 0 then begin
      iNewEnd.Char := Length(Lines[Lines.Count - 1]) + 1;
      iNewEnd.Line := Lines.Count;
    end else
      iNewEnd := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewEnd, BlockBegin, iNewEnd);
  end else begin
    iNewBegin := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewBegin, iNewBegin, BlockBegin);
  end;
end;

procedure TCustomSynEdit.DefineProperties(Filer: TFiler);

  function CollectionsEqual(C1, C2: TCollection): boolean;
  begin
    Result := Classes.CollectionsEqual(C1, C2, nil, nil);
  end;

  function HasKeyData: boolean;
  var
    iDefKeys: TSynEditKeyStrokes;
  begin
    if Filer.Ancestor <> nil then begin
      Result := not CollectionsEqual(Keystrokes,
        TCustomSynEdit(Filer.Ancestor).Keystrokes);
    end else begin
      iDefKeys := TSynEditKeyStrokes.Create(nil);
      try
        iDefKeys.ResetDefaults;
        Result := not CollectionsEqual(Keystrokes, iDefKeys);
      finally
        iDefKeys.Free;
      end;
    end;
  end;

var
  iSaveKeyData: boolean;
begin
  inherited;
  iSaveKeyData := HasKeyData;
  Filer.DefineProperty('RemovedKeystrokes', ReadRemovedKeystrokes,
    WriteRemovedKeystrokes, iSaveKeyData);
  Filer.DefineProperty('AddedKeystrokes', ReadAddedKeystrokes, WriteAddedKeystrokes,
    iSaveKeyData);
end;

procedure TCustomSynEdit.DoChange;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TCustomSynEdit.ReadAddedKeystrokes(Reader: TReader);
var
  iAddKeys: TSynEditKeyStrokes;
  cKey: integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iAddKeys := TSynEditKeyStrokes.Create(Self);
  try
    Reader.ReadCollection(iAddKeys);
    for cKey := 0 to iAddKeys.Count - 1 do
      Keystrokes.Add.Assign(iAddKeys[cKey]);
  finally
    iAddKeys.Free;
  end;
end;

procedure TCustomSynEdit.ReadRemovedKeystrokes(Reader: TReader);
var
  iDelKeys: TSynEditKeyStrokes;
  cKey: integer;
  iKey: TSynEditKeyStroke;
  iToDelete: integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iDelKeys := TSynEditKeyStrokes.Create(nil);
  try
    Reader.ReadCollection(iDelKeys);
    for cKey := 0 to iDelKeys.Count - 1 do begin
      iKey := iDelKeys[cKey];
      iToDelete := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iToDelete >= 0) and (Keystrokes[iToDelete].Command = iKey.Command) then
        Keystrokes[iToDelete].Free;
    end;
  finally
    iDelKeys.Free;
  end;
end;

procedure TCustomSynEdit.WriteAddedKeystrokes(Writer: TWriter);
var
  iDefaultKeys: TSynEditKeyStrokes;
  iAddedKeys: TSynEditKeyStrokes;
  cKey: integer;
  iKey: TSynEditKeyStroke;
  iDelIndex: integer;
begin
  iDefaultKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iDefaultKeys.Assign(TSynEdit(Writer.Ancestor).Keystrokes)
    else
      iDefaultKeys.ResetDefaults;
    iAddedKeys := TSynEditKeyStrokes.Create(nil);
    try
      for cKey := 0 to Keystrokes.Count - 1 do begin
        iKey := Keystrokes[cKey];
        iDelIndex := iDefaultKeys.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
        //if it's not a default keystroke, add it
        if (iDelIndex < 0) or (iDefaultKeys[iDelIndex].Command <> iKey.Command) then
          iAddedKeys.Add.Assign(iKey);
      end;
      Writer.WriteCollection(iAddedKeys);
    finally
      iAddedKeys.Free;
    end;
  finally
    iDefaultKeys.Free;
  end;
end;

procedure TCustomSynEdit.WriteRemovedKeystrokes(Writer: TWriter);
var
  iRemovedKeys: TSynEditKeyStrokes;
  cKey: integer;
  iKey: TSynEditKeyStroke;
  iFoundAt: integer;
begin
  iRemovedKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iRemovedKeys.Assign(TSynEdit(Writer.Ancestor).Keystrokes)
    else
      iRemovedKeys.ResetDefaults;
    cKey := 0;
    while cKey < iRemovedKeys.Count do begin
      iKey := iRemovedKeys[cKey];
      iFoundAt := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iFoundAt >= 0) and (Keystrokes[iFoundAt].Command = iKey.Command) then
        iKey.Free //if exists in Keystrokes, then shouldn't be in "removed" list
      else
        Inc(cKey);
    end;
    Writer.WriteCollection(iRemovedKeys);
  finally
    iRemovedKeys.Free;
  end;
end;

procedure TCustomSynEdit.AddMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseUpHandler(aHandler);
end;

procedure TCustomSynEdit.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.AddMouseCursorHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.RemoveMouseCursorHandler(aHandler);
end;

function TCustomSynEdit.GetDisplayLineCount: integer;
begin
  Result := LineToRow(Lines.Count);
end;

function TCustomSynEdit.LineToRow(aLine: integer): integer;
begin
  Result := BufferToDisplayPos(BufferCoord(1, aLine)).Row;
end;

function TCustomSynEdit.RowToLine(aRow: integer): integer;
begin
  Result := DisplayToBufferPos(DisplayCoord(1, aRow)).Line;
end;

procedure TCustomSynEdit.SetInternalDisplayXY(const aPos: TDisplayCoord);
begin
  IncPaintLock;
  InternalCaretXY := DisplayToBufferPos(aPos);
  DecPaintLock;
  UpdateLastCaretX;
end;

procedure TCustomSynEdit.SetWantReturns(Value: Boolean);
begin
  fWantReturns := Value;
end;

procedure TCustomSynEdit.SetWantTabs(Value: Boolean);
begin
  fWantTabs := Value;
end;

procedure TCustomSynEdit.Collapse(FoldRange: TSynEditFoldRange);
begin
  with FoldRange do begin
    LinesCollapsed := ToLine - FromLine;
    Collapsed := True;

    // Extract caret from fold
    if (fCaretY > FromLine) and (fCaretY <= ToLine) then
      CaretXY := BufferCoord(Length(Lines[FromLine - 1]) + 1, FromLine);
  end;

  // Redraw the collapsed line
  InvalidateLines(FoldRange.FromLine, MaxInt);

  // Redraw fold mark
  InvalidateGutterLines(FoldRange.FromLine, MaxInt);
end;

procedure TCustomSynEdit.CollapseAll;
var
  i: Integer;
begin
  for i := fAllFoldRanges.Count - 1 downto 0 do
    with fAllFoldRanges[i] do
      Collapse(fAllFoldRanges[i]);
end;

procedure TCustomSynEdit.Uncollapse(FoldRange: TSynEditFoldRange);
begin
  with FoldRange do begin
    LinesCollapsed := 0;
    Collapsed := False;
  end;

  // Redraw the collapsed line
  InvalidateLines(FoldRange.FromLine, MaxInt);

  // Redraw fold mark
  InvalidateGutterLines(FoldRange.FromLine, MaxInt);
end;

procedure TCustomSynEdit.UncollapseAroundLine(Line: Integer);
var
  Fold: TSynEditFoldRange;
begin
  repeat // Open up the closed folds around the focused line until we can see the line we're looking for
    Fold := FoldHidesLine(line);
    if Assigned(Fold) then
      Uncollapse(Fold);
  until not Assigned(Fold);
end;

procedure TCustomSynEdit.UncollapseAll;
var
  i: Integer;
begin
  for i := fAllFoldRanges.Count - 1 downto 0 do
    with fAllFoldRanges[i] do
      Uncollapse(fAllFoldRanges[i]);
end;

function TCustomSynEdit.GetLineIndent(const Line: AnsiString): Integer;
var
  P: PAnsiChar;
begin
  Result := 0;

  P := PAnsiChar(Line);
  while P^ in [#9, #32] do begin
    if P^ = #9 then
      Inc(Result, TabWidth)
    else
      Inc(Result);
    Inc(P);
  end;
end;

procedure TCustomSynEdit.ReScan;
begin
  if not fUseCodeFolding then
    Exit;
  ReScanForFoldRanges;
  InvalidateGutter;
end;

procedure TCustomSynEdit.ReScanForFoldRanges;
var
  i, j: Integer;
  TemporaryAllFoldRanges: TSynEditFoldRanges;
begin
  // Delete all uncollapsed folds
  for i := fAllFoldRanges.Count - 1 downto 0 do
    with fAllFoldRanges[i] do
      if not Collapsed and not ParentCollapsed then
        fAllFoldRanges.Delete(i);

  // Did we leave any collapsed folds and are we viewing a code file?
  if fAllFoldRanges.Count > 0 then begin

    // Add folds to a separate list
    TemporaryAllFoldRanges := TSynEditFoldRanges.Create;
    TemporaryAllFoldRanges.OwnsObjects := false;
    ScanForFoldRanges(TemporaryAllFoldRanges, fLines);

    // Combine new with old folds, preserve parent order
    for i := 0 to TemporaryAllFoldRanges.Count - 1 do begin
      for j := 0 to fAllFoldRanges.Count - 1 do begin
        if TemporaryAllFoldRanges[i].FromLine < fAllFoldRanges[j].FromLine then begin
          fAllFoldRanges.Ranges.Insert(j, TemporaryAllFoldRanges[i]);
          Break; // #i is done...
        end;

        // If we can't prepend #i anywhere, just dump it at the end
        if j = fAllFoldRanges.Count - 1 then
          fAllFoldRanges.Ranges.Add(TemporaryAllFoldRanges[i]);
      end;
    end;

    // Pointers have been moved to fAllFoldRanges, remove this list
    TemporaryAllFoldRanges.Free;
  end else begin

    // We ended up with no folds after deleting, just pass standard data...
    ScanForFoldRanges(fAllFoldRanges, fLines);
  end;
end;

procedure TCustomSynEdit.ScanForFoldRanges(TopFoldRanges: TSynEditFoldRanges; LinesToScan: TStrings);
var
  FoldIndex: integer;
  token: AnsiString;
  attr: TSynHighlighterAttributes;
  CurLine: AnsiString;
  ParentFoldRanges: TSynEditFoldRanges;

  function LineHasChar(Line: integer; character: char): boolean; // faster than Pos!
  var
    i: integer;
  begin
    result := false;
    for I := 1 to Length(CurLine) do begin
      if CurLine[i] = character then begin

        // Char must have proper highlighting (ignore stuff inside comments...)
        if (GetHighlighterAttriAtRowCol(BufferCoord(i, Line + 1), token, attr) and (attr.Name =
          fCodeFolding.FoldRegions[FoldIndex].Highlight)) then begin
          result := true;
          break;
        end;
      end;
    end;
  end;

  procedure FindSubFoldRange(Parent: TSynEditFoldRange);
  var
    Col, Line: integer;
    CollapsedFold: TSynEditFoldRange;
  begin
    Line := 0;
    while Line < LinesToScan.Count do begin // index is valid for LinesToScan and fLines
      // If there is a collapsed fold over here, skip it
      CollapsedFold := CollapsedFoldStartAtLine(Line + 1); // only collapsed folds remain
      if Assigned(CollapsedFold) then begin
        Line := CollapsedFold.ToLine;
        Continue;
      end;

      // Find an opening character on this line
      CurLine := LinesToScan[Line];
      for Col := 1 to Length(CurLine) do begin

        // We've found a starting character
        if CurLine[col] = fCodeFolding.FoldRegions[FoldIndex].Open then begin

          // Char must have proper highlighting (ignore stuff inside comments...)
          if (GetHighlighterAttriAtRowCol(BufferCoord(Col, Line + 1), token, attr) and
            (attr.Name = fCodeFolding.FoldRegions[FoldIndex].Highlight)) then begin

            // And ignore lines with both opening and closing chars in them
            if not LineHasChar(Line, fCodeFolding.FoldRegions[FoldIndex].Close) then begin

              // Add it to the top list of folds
              Parent := ParentFoldRanges.AddByParts(
                Parent,
                TopFoldRanges,
                Line + 1,
                fCodeFolding.FoldRegions[FoldIndex],
                Line + 1);
              ParentFoldRanges := Parent.SubFoldRanges;

              // Skip until a newline
              break;
            end;
          end;
        end else if CurLine[col] = fCodeFolding.FoldRegions[FoldIndex].Close then begin

          // Char must have symbol attri too
          if (GetHighlighterAttriAtRowCol(BufferCoord(Col, Line + 1), token, attr) and
            (attr.Name = fCodeFolding.FoldRegions[FoldIndex].Highlight)) then begin

            // And ignore lines with both opening and closing chars in them
            if not LineHasChar(Line, fCodeFolding.FoldRegions[FoldIndex].Open) then begin

              // Stop the recursion if we find a closing char, and return to our parent
              if Assigned(Parent) then begin
                Parent.ToLine := Line + 1;
                Parent := Parent.Parent;
              end;

              // Skip until a newline
              break;
            end;
          end;
        end;
      end; // for Col
      Inc(Line);
    end; // while Line
  end;

begin
  ParentFoldRanges := TopFoldRanges;

  // Recursively scan for folds (all types)
  for FoldIndex := 0 to fCodeFolding.FoldRegions.Count - 1 do
    FindSubFoldRange(nil);
end;

procedure TCustomSynEdit.SetUseCodeFolding(value: boolean);
begin
  if value <> fUseCodeFolding then begin
    fUseCodeFolding := value;
    ReScan;
  end;
end;

function TCustomSynEdit.FoldStartAtLine(Line: Integer): TSynEditFoldRange;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fAllFoldRanges.Count - 1 do
    with fAllFoldRanges[i] do
      if (FromLine = Line) then begin
        Result := fAllFoldRanges[i];
        Break;
      end else if FromLine > Line then
        break; // sorted by line. don't bother scanning further
end;

function TCustomSynEdit.CollapsedFoldStartAtLine(Line: Integer): TSynEditFoldRange;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fAllFoldRanges.Count - 1 do
    with fAllFoldRanges[i] do
      if (FromLine = Line) and Collapsed then begin
        Result := fAllFoldRanges[i];
        Break;
      end else if FromLine > Line then
        break; // sorted by line. don't bother scanning further
end;

function TCustomSynEdit.FoldEndAtLine(Line: Integer): TSynEditFoldRange;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fAllFoldRanges.Count - 1 do
    with fAllFoldRanges[i] do
      if (ToLine = Line) then begin
        Result := fAllFoldRanges[i];
        Break;
      end else if FromLine > Line then
        break; // sorted by line. don't bother scanning further
end;

function TCustomSynEdit.FoldAroundLineEx(Line: Integer; WantCollapsed, AcceptFromLine, AcceptToLine: Boolean):
  TSynEditFoldRange;
var
  ResultChild: TSynEditFoldRange;

  function CheckFoldRange(FoldRangeToCheck: TSynEditFoldRanges): TSynEditFoldRange;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to FoldRangeToCheck.Count - 1 do begin
      with FoldRangeToCheck[i] do begin
        if ((FromLine < Line) or ((FromLine <= Line) and AcceptFromLine)) and
          ((ToLine > Line) or ((ToLine >= Line) and AcceptToLine)) then begin
          if Collapsed = WantCollapsed then begin
            Result := FoldRangeToCheck[i];
            Exit;
          end;
        end;
      end;
    end;
  end;
begin
  // Check global list
  Result := CheckFoldRange(fAllFoldRanges);

  // Found an item in the top level list?
  if Assigned(Result) then begin
    while True do begin
      ResultChild := CheckFoldRange(Result.SubFoldRanges);
      if not Assigned(ResultChild) then
        Exit
      else
        Result := ResultChild; // repeat for this one
    end;
  end;
end;

function TCustomSynEdit.CollapsedFoldAroundLine(Line: Integer): TSynEditFoldRange;
begin
  Result := FoldAroundLineEx(Line, True, False, False);
end;

function TCustomSynEdit.FoldAroundLine(Line: Integer): TSynEditFoldRange;
begin
  Result := FoldAroundLineEx(Line, False, False, False);
end;

function TCustomSynEdit.FoldHidesLine(Line: Integer): TSynEditFoldRange;
begin
  Result := FoldAroundLineEx(Line, True, False, True);
end;

function TCustomSynEdit.FoldExtendsLine(Line: Integer): TSynEditFoldRange;
begin
  Result := FoldAroundLineEx(Line, True, True, True);
end;

function TCustomSynEdit.FoldLineToRow(Line: Integer): Integer;
var
  i: Integer;
begin
  Result := Line;
  for i := fAllFoldRanges.Count - 1 downto 0 do
    with fAllFoldRanges[i] do
      if Collapsed and not ParentCollapsed then begin
        // Line is found after fold
        if ToLine < Line then
          Dec(Result, LinesCollapsed)
            // Inside fold
        else if (FromLine < Line) and (Line <= ToLine) then
          Dec(Result, Line - FromLine)
      end;
end;

function TCustomSynEdit.FoldRowToLine(Row: Integer): Integer;
var
  i: Integer;
begin
  Result := Row;
  for i := 0 to fAllFoldRanges.Count - 1 do
    with fAllFoldRanges[i] do
      if Collapsed and not ParentCollapsed and (FromLine < Result) then
        Inc(Result, LinesCollapsed)
end;

procedure TCustomSynEdit.FoldOnListCleared;
var
  i: Integer;
begin
  for i := fAllFoldRanges.Count - 1 downto 0 do
    fAllFoldRanges.Delete(i);
end;

procedure TCustomSynEdit.FoldOnListDeleted(Line: Integer; Count: Integer);
var
  I: Integer;
begin
  // Delete collapsed inside selection
  for i := fAllFoldRanges.Count - 1 downto 0 do
    with fAllFoldRanges[i] do
      if Collapsed or ParentCollapsed then
        if (FromLine = Line) and (Count = 1) then // open up because we are messing with the starting line
          Uncollapse(fAllFoldRanges[i])
        else if (FromLine >= Line - 1) and (FromLine < Line + Count) then // delete inside affectec area
          fAllFoldRanges.Delete(i)
        else if (FromLine >= Line + Count) then // Move after affected area
          fAllFoldRanges[i].Move(-Count);
end;

procedure TCustomSynEdit.FoldOnListInserted(Line: Integer; Count: Integer);
var
  i: Integer;
begin
  // Delete collapsed inside selection
  for i := fAllFoldRanges.Count - 1 downto 0 do
    with fAllFoldRanges[i] do
      if Collapsed or ParentCollapsed then
        if (FromLine = Line - 1) then // insertion starts at fold line
          Uncollapse(fAllFoldRanges[i])
        else if (FromLine >= Line) then // insertion of count lines above FromLine
          fAllFoldRanges[i].Move(Count);
end;

{ TSynEditPlugin }

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  if AOwner <> nil then begin
    fOwner := AOwner;
    if fOwner.fPlugins = nil then
      fOwner.fPlugins := TList.Create;
    fOwner.fPlugins.Add(Self);
  end;
end;

destructor TSynEditPlugin.Destroy;
begin
  if fOwner <> nil then
    fOwner.fPlugins.Remove(Self);
  inherited Destroy;
end;

initialization
  SynEditClipboardFormat := RegisterClipboardFormat(SYNEDIT_CLIPBOARD_FORMAT);
end.

