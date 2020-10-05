object MainForm: TMainForm
  Left = 726
  Top = 225
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  AutoScroll = False
  Caption = 'Dev-C++ 2020'
  ClientHeight = 671
  ClientWidth = 1232
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDefault
  ShowHint = True
  OnClose = FormClose
  OnContextPopup = FormContextPopup
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object SplitterLeft: TSplitter
    Left = 238
    Top = 56
    Height = 361
    MinSize = 45
    ResizeStyle = rsUpdate
  end
  object SplitterBottom: TSplitter
    Left = 0
    Top = 417
    Width = 1232
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    OnMoved = SplitterBottomMoved
  end
  object MessageControl: TPageControl
    Left = 0
    Top = 421
    Width = 1232
    Height = 225
    ActivePage = DebugSheet
    Align = alBottom
    Images = dmMain.MenuImages_NewLook
    MultiLine = True
    TabOrder = 2
    OnChange = MessageControlChange
    object CompSheet: TTabSheet
      Caption = 'Compiler'
      ImageIndex = 28
      object CompilerOutput: TListView
        Left = 0
        Top = 0
        Width = 1224
        Height = 194
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Line'
            Width = 49
          end
          item
            Caption = 'Col'
            Width = 49
          end
          item
            Caption = 'Unit'
            Width = 394
          end
          item
            AutoSize = True
            Caption = 'Message'
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        PopupMenu = MessagePopup
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDraw = CompilerOutputAdvancedCustomDraw
        OnAdvancedCustomDrawItem = CompilerOutputAdvancedCustomDrawItem
        OnDblClick = CompilerOutputDblClick
        OnDeletion = CompilerOutputDeletion
        OnKeyDown = CompilerOutputKeyDown
      end
    end
    object ResSheet: TTabSheet
      Caption = 'Resource'
      ImageIndex = 2
      object ResourceOutput: TListView
        Left = 0
        Top = 0
        Width = 1224
        Height = 194
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Line'
            Width = 49
          end
          item
            Caption = 'Col'
            Width = 49
          end
          item
            Caption = 'Unit'
            Width = 394
          end
          item
            AutoSize = True
            Caption = 'Message'
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        PopupMenu = MessagePopup
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDraw = CompilerOutputAdvancedCustomDraw
        OnAdvancedCustomDrawItem = CompilerOutputAdvancedCustomDrawItem
        OnDblClick = CompilerOutputDblClick
        OnDeletion = ResourceOutputDeletion
        OnKeyDown = CompilerOutputKeyDown
      end
    end
    object LogSheet: TTabSheet
      Caption = 'Compile log'
      ImageIndex = 43
      object InfoGroupBox: TPanel
        Left = 0
        Top = 0
        Width = 192
        Height = 194
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          192
          194)
        object btnAbortCompilation: TSpeedButton
          Left = 5
          Top = 5
          Width = 177
          Height = 37
          Action = actAbortCompilation
        end
        object pbCompilation: TProgressBar
          Left = 5
          Top = 49
          Width = 177
          Height = 30
          Anchors = [akLeft, akTop, akRight]
          Step = 1
          TabOrder = 0
        end
        object chkShortenPaths: TCheckBox
          Left = 5
          Top = 85
          Width = 177
          Height = 21
          Action = actShortenCompPaths
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
        end
      end
      object CompResGroupBox: TPanel
        Left = 192
        Top = 0
        Width = 1032
        Height = 194
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object LogOutput: TMemo
          Left = 0
          Top = 0
          Width = 1032
          Height = 194
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -17
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PopupMenu = MessagePopup
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
          WantTabs = True
          WordWrap = False
        end
      end
    end
    object DebugSheet: TTabSheet
      Caption = 'Debugging'
      ImageIndex = 32
      object DebugSendPanel: TPanel
        Left = 725
        Top = 0
        Width = 499
        Height = 194
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          499
          194)
        object lblSendCommandGdb: TLabel
          Left = 5
          Top = 9
          Width = 144
          Height = 16
          Caption = 'Send command to GDB:'
        end
        object edGdbCommand: TComboBox
          Left = 167
          Top = 4
          Width = 325
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 16
          TabOrder = 0
          OnKeyPress = edGdbCommandKeyPress
        end
        object DebugOutput: TMemo
          Left = 5
          Top = 37
          Width = 489
          Height = 133
          Anchors = [akLeft, akTop, akRight, akBottom]
          PopupMenu = DebugOutputPopup
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
      object DebugStartPanel: TPanel
        Left = 0
        Top = 0
        Width = 725
        Height = 194
        Align = alLeft
        BevelOuter = bvNone
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        TabOrder = 1
        DesignSize = (
          725
          194)
        object DDebugBtn: TSpeedButton
          Left = 5
          Top = 9
          Width = 138
          Height = 30
          Action = actDebug
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0000000000FF7BB500FF7BB50000000000FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF0000000000FF9CC600FF9CC600FF7BB500FF7BB50000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
            0000FF9CC600FF9CC600FF9CC600FF9CC600FF7BB500FF7BB50000000000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF006B6B6B00FFB5
            D600FFB5D600FF9CC600FFCEE700FFCEE700FFB5D600FF9CC600FF7BB5000000
            0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFCE
            E700FFB5D600FFCEE7000000000000000000FFDEEF00FFB5D600FF9CC600FF7B
            B50000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFCE
            E700FFCEE70000000000FF00FF00FF00FF0000000000FFDEEF00FFB5D600FF9C
            C600FF7BB50000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000F7F7
            F70000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000FFDEEF00FFB5
            D600FF9CC600FF7BB50000000000FF00FF00FF00FF00FF00FF00FF00FF000000
            0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFDE
            EF00FFB5D600FF9CC600FF7BB50000000000FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
            0000FFDEEF00FFB5D600FF9CC600FF7BB50000000000FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF0000000000FFDEEF00FFB5D600FF9CC600FF7BB50000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0000000000FFDEEF00FFB5D600FF7BB50000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF0000000000FFDEEF00FF7BB50000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF0000000000FFC6DE0000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00}
        end
        object StopExecBtn: TSpeedButton
          Left = 5
          Top = 47
          Width = 138
          Height = 31
          Action = actStopExecute
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000000000000A50000000000FF00FF00FF00FF00FF00FF00FF00
            FF00000000000000A50000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00000000000000FF000000D6000000A50000000000FF00FF00FF00FF000000
            00000000D6000000D6000000A50000000000FF00FF00FF00FF00FF00FF000000
            00007B7BFF006B6BFF000000FF000000D6000000A50000000000000000000000
            D6000000FF000000FF000000D6000000A50000000000FF00FF00FF00FF00FF00
            FF00000000007B7BFF006B6BFF000000FF000000D6000000FF000000FF000000
            FF000000FF000000FF000000D60000000000FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000000007B7BFF006B6BFF000000FF000000FF000000FF000000
            FF000000FF000000D60000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00000000000000FF000000FF000000FF000000FF000000
            FF000000FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00000000000000FF000000FF000000FF000000FF000000
            FF000000FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000000006B6BFF000000FF000000FF000000FF000000FF000000
            FF000000D6000000A50000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00000000006B6BFF000000FF000000FF000000FF000000FF000000FF006B6B
            FF000000FF000000D6000000A50000000000FF00FF00FF00FF00FF00FF000000
            00007B7BFF006B6BFF000000FF000000FF006B6BFF0000000000000000007B7B
            FF006B6BFF000000FF000000D6000000A50000000000FF00FF00FF00FF00FF00
            FF00000000007B7BFF006B6BFF006B6BFF0000000000FF00FF00FF00FF000000
            00007B7BFF006B6BFF000000FF0000000000FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000000007B7BFF0000000000FF00FF00FF00FF00FF00FF00FF00
            FF00000000007B7BFF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
        object lblEvaluate: TLabel
          Left = 5
          Top = 94
          Width = 56
          Height = 16
          Caption = 'Evaluate:'
        end
        object StepOverBtn: TSpeedButton
          Left = 290
          Top = 9
          Width = 138
          Height = 30
          Action = actStepOver
        end
        object ContinueBtn: TSpeedButton
          Left = 148
          Top = 9
          Width = 138
          Height = 30
          Action = actContinue
        end
        object StepIntoBtn: TSpeedButton
          Left = 433
          Top = 9
          Width = 138
          Height = 30
          Action = actStepInto
        end
        object AddWatchBtn: TSpeedButton
          Left = 290
          Top = 47
          Width = 138
          Height = 31
          Action = actAddWatch
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000000000002929
            2900292929002929290029292900292929002929290029292900292929002929
            29002929290000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000D6D6
            D600BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
            BD009494940029292900FF00FF0000000000000000000000000000000000F7F7
            F700D6D6D600D6D6D600D6D6D600D6D6D600CECECE00CECECE00CECECE00CECE
            CE00BDBDBD002929290000000000006BA500006BA5000000000000000000F7F7
            F700D6D6D6006363630063636300D6D6D60063636300CECECE0063636300CECE
            CE00BDBDBD0000000000006BA5006BCEFF00006BA5000000000000000000F7F7
            F700DEDEDE00D6D6D600D6D6D600D6D6D6000808080008080800080808000808
            080008080800006BA5006BCEFF00006BA50000000000FF00FF0000000000F7F7
            F700DEDEDE006363630063636300080808008C8C8C00E7CE9400E7CE8C00DEC6
            84006B6B6B006BCEFF00006BA50000000000FF00FF00FF00FF0000000000F7F7
            F700DEDEDE00DEDEDE00080808008C8C8C00E7DEAD00EFDEB500E7DEAD00E7D6
            9400EFD68C006B6B6B0008080800FF00FF00FF00FF00FF00FF0000000000F7F7
            F700DEDEDE006363630008080800EFDEAD00EFE7C600EFEFD600EFE7C600E7DE
            AD00EFD69400B59C520008080800FF00FF00FF00FF00FF00FF0000000000F7F7
            F700DEDEDE00DEDEDE0008080800EFDEB500EFEFD600F7F7EF00F7EFD600EFDE
            B500EFDE9C00B59C520008080800FF00FF00FF00FF00FF00FF0000000000F7F7
            F700DEDEDE006363630008080800EFDEAD00EFE7C600EFEFD600EFE7C600E7DE
            AD00EFD69C00B59C520008080800FF00FF00FF00FF00FF00FF0000000000F7F7
            F700DEDEDE00DEDEDE00080808008C8C8C00EFDEAD00EFE7B500E7DEAD00E7D6
            9400EFD68C008C8C8C0008080800FF00FF00FF00FF00FF00FF0000000000F7F7
            F700DEDEDE0063636300DEDEDE006B6B6B008C8C8C00E7D69400E7D69400DEC6
            84008C8C8C0008080800FF00FF00FF00FF00FF00FF00FF00FF0000000000F7F7
            F700DEDEDE00DEDEDE00DEDEDE00DEDEDE000808080008080800080808000808
            08000808080029292900FF00FF00FF00FF00FF00FF00FF00FF0000000000F7F7
            F700DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDE
            DE00BDBDBD0029292900FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF
            FF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
            F700DEDEDE0029292900FF00FF00FF00FF00FF00FF00FF00FF006B6B6B002929
            2900292929002929290029292900292929002929290029292900000000000000
            00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00}
        end
        object ViewCPUBtn: TSpeedButton
          Left = 148
          Top = 47
          Width = 138
          Height = 31
          Action = actViewCPU
        end
        object EvaluateInput: TComboBox
          Left = 79
          Top = 89
          Width = 637
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 16
          TabOrder = 0
          OnKeyPress = EvaluateInputKeyPress
        end
        object EvalOutput: TMemo
          Left = 5
          Top = 123
          Width = 711
          Height = 46
          Anchors = [akLeft, akTop, akRight, akBottom]
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
        end
        object StepOutBtn: TButton
          Left = 576
          Top = 9
          Width = 138
          Height = 30
          Action = actStepOut
          TabOrder = 2
        end
        object ChooseCustomCommandBtn: TButton
          Left = 680
          Top = 47
          Width = 34
          Height = 31
          Action = actChooseCustomCommand
          TabOrder = 3
        end
        object RunCustomCommandBtn: TButton
          Left = 433
          Top = 47
          Width = 248
          Height = 31
          Action = actRunCustomCommand
          TabOrder = 4
        end
      end
    end
    object FindSheet: TTabSheet
      Caption = 'Find results'
      ImageIndex = 21
      object FindOutput: TListView
        Left = 0
        Top = 0
        Width = 1224
        Height = 194
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            Width = 18
          end
          item
            Caption = 'Line'
            Width = 49
          end
          item
            Caption = 'Col'
            Width = 49
          end
          item
            Caption = 'Unit'
            Width = 394
          end
          item
            AutoSize = True
            Caption = 'Message'
          end>
        ColumnClick = False
        GridLines = True
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        PopupMenu = MessagePopup
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDraw = FindOutputAdvancedCustomDraw
        OnAdvancedCustomDrawSubItem = FindOutputAdvancedCustomDrawSubItem
        OnDblClick = FindOutputDblClick
        OnDeletion = FindOutputDeletion
        OnKeyDown = FindOutputKeyDown
        OnSelectItem = FindOutputSelectItem
      end
    end
    object CloseSheet: TTabSheet
      Caption = 'Close'
      ImageIndex = 9
    end
  end
  object ToolbarDock: TControlBar
    Left = 0
    Top = 0
    Width = 1232
    Height = 56
    Align = alTop
    AutoDock = False
    AutoSize = True
    BevelInner = bvNone
    BevelOuter = bvNone
    BevelKind = bkNone
    RowSize = 28
    TabOrder = 0
    OnClick = ToolbarDockClick
    OnContextPopup = ToolbarDockContextPopup
    object tbMain: TToolBar
      Left = 11
      Top = 2
      Width = 172
      Height = 24
      Caption = 'Main'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Wrapable = False
      object NewFileBtn: TToolButton
        Left = 0
        Top = 0
        Caption = '&Source File'
        ImageIndex = 1
        OnClick = NewFileBtnClick
      end
      object OpenBtn: TToolButton
        Left = 23
        Top = 0
        Action = actOpen
      end
      object SaveBtn: TToolButton
        Left = 46
        Top = 0
        Action = actSave
      end
      object SaveAllBtn: TToolButton
        Left = 69
        Top = 0
        Action = actSaveAll
      end
      object CloseBtn: TToolButton
        Left = 92
        Top = 0
        Action = actClose
      end
      object CloseAllBtn: TToolButton
        Left = 115
        Top = 0
        Action = actCloseAll
      end
      object ToolButton7: TToolButton
        Left = 138
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object PrintBtn: TToolButton
        Left = 146
        Top = 0
        Action = actPrint
      end
    end
    object tbCompile: TToolBar
      Left = 464
      Top = 2
      Width = 209
      Height = 24
      Caption = 'Compile and Run'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Wrapable = False
      object CompileBtn: TToolButton
        Left = 0
        Top = 0
        Action = actCompile
      end
      object RunBtn: TToolButton
        Left = 23
        Top = 0
        Action = actRun
      end
      object CompileAndRunBtn: TToolButton
        Left = 46
        Top = 0
        Action = actCompRun
      end
      object RebuildAllBtn: TToolButton
        Left = 69
        Top = 0
        Action = actRebuild
      end
      object ToolButton2: TToolButton
        Left = 92
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 33
        Style = tbsSeparator
      end
      object DebugBtn: TToolButton
        Left = 100
        Top = 0
        Action = actDebug
      end
      object ToolButton3: TToolButton
        Left = 123
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 49
        Style = tbsSeparator
      end
      object StopBtn: TToolButton
        Left = 131
        Top = 0
        Action = actStopExecute
      end
      object ToolButton5: TToolButton
        Left = 154
        Top = 0
        Width = 8
        Caption = 'ToolButton5'
        ImageIndex = 48
        Style = tbsSeparator
      end
      object ProfileBtn: TToolButton
        Left = 162
        Top = 0
        Action = actProfile
      end
      object ProfilingInforBtn: TToolButton
        Left = 185
        Top = 0
        Action = actDeleteProfile
      end
    end
    object tbProject: TToolBar
      Left = 373
      Top = 2
      Width = 78
      Height = 24
      Caption = 'Project'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Wrapable = False
      object AddToProjectBtn: TToolButton
        Left = 0
        Top = 0
        Action = actProjectAdd
      end
      object RemoveFromProjectBtn: TToolButton
        Left = 23
        Top = 0
        Action = actProjectRemove
      end
      object ToolButton20: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Caption = 'ToolButton20'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ProjectOptionsBtn: TToolButton
        Left = 54
        Top = 0
        Action = actProjectOptions
      end
    end
    object tbEdit: TToolBar
      Left = 196
      Top = 2
      Width = 47
      Height = 24
      Caption = 'Edit'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Wrapable = False
      object UndoBtn: TToolButton
        Left = 0
        Top = 0
        Action = actUndo
      end
      object RedoBtn: TToolButton
        Left = 23
        Top = 0
        Action = actRedo
      end
    end
    object tbSearch: TToolBar
      Left = 256
      Top = 2
      Width = 104
      Height = 24
      Caption = 'Search'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Wrapable = False
      object FindBtn: TToolButton
        Left = 0
        Top = 0
        Action = actFind
      end
      object ReplaceBtn: TToolButton
        Left = 23
        Top = 0
        Action = actReplace
      end
      object ToolButton1: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 25
        Style = tbsSeparator
      end
      object FindNextBtn: TToolButton
        Left = 54
        Top = 0
        Action = actGotoFunction
      end
      object GotoLineBtn: TToolButton
        Left = 77
        Top = 0
        Action = actGotoLine
      end
    end
    object tbSpecials: TToolBar
      Left = 11
      Top = 30
      Width = 71
      Height = 24
      Caption = 'Specials'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Transparent = False
      Wrapable = False
      object InsertBtn: TToolButton
        Left = 0
        Top = 0
        Action = actInsert
      end
      object ToggleBtn: TToolButton
        Left = 23
        Top = 0
        Action = actToggle
      end
      object GotoBtn: TToolButton
        Left = 46
        Top = 0
        Action = actGoto
      end
    end
    object tbClasses: TToolBar
      Left = 95
      Top = 30
      Width = 700
      Height = 24
      Caption = 'tbClasses'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      TabOrder = 7
      Wrapable = False
      object cmbClasses: TComboBox
        Left = 0
        Top = 0
        Width = 350
        Height = 22
        Style = csDropDownList
        Ctl3D = True
        DropDownCount = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 14
        ItemIndex = 0
        ParentCtl3D = False
        ParentFont = False
        Sorted = True
        TabOrder = 0
        Text = '(globals)'
        OnChange = cmbClassesChange
        OnDropDown = cmbGenericDropDown
        Items.Strings = (
          '(globals)')
      end
      object cmbMembers: TComboBox
        Left = 350
        Top = 0
        Width = 350
        Height = 22
        Style = csDropDownList
        Ctl3D = True
        DropDownCount = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 14
        ParentCtl3D = False
        ParentFont = False
        Sorted = True
        TabOrder = 1
        OnChange = cmbMembersChange
        OnDropDown = cmbGenericDropDown
      end
    end
    object tbCompilers: TToolBar
      Left = 687
      Top = 2
      Width = 255
      Height = 24
      Caption = 'tbCompilers'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      TabOrder = 6
      Wrapable = False
      object cmbCompilers: TComboBox
        Left = 0
        Top = 0
        Width = 250
        Height = 22
        Style = csDropDownList
        Ctl3D = True
        DropDownCount = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 14
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 0
        OnChange = cmbCompilersChange
        OnDropDown = cmbGenericDropDown
      end
    end
  end
  object Statusbar: TStatusBar
    Left = 0
    Top = 646
    Width = 1232
    Height = 25
    Panels = <
      item
        Width = 480
      end
      item
        Width = 80
      end
      item
        Width = 80
      end
      item
        Width = 80
      end>
    ParentFont = True
    UseSystemFont = False
  end
  object FileMonitor: TdevFileMonitor
    Left = 138
    Top = 187
    Width = 0
    Height = 0
    OnNotifyChange = FileMonitorNotifyChange
  end
  object LeftPageControl: TPageControl
    Left = 0
    Top = 56
    Width = 238
    Height = 361
    ActivePage = LeftDebugSheet
    Align = alLeft
    Images = dmMain.ProjectImage_NewLook
    TabOrder = 1
    OnChange = LeftPageControlChange
    object LeftProjectSheet: TTabSheet
      Caption = 'Project'
      ImageIndex = -1
      object ProjectView: TTreeView
        Left = 0
        Top = 0
        Width = 230
        Height = 330
        Align = alClient
        Anchors = [akLeft, akTop, akBottom]
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ChangeDelay = 1
        DragMode = dmAutomatic
        HideSelection = False
        HotTrack = True
        Images = dmMain.ProjectImage_NewLook
        Indent = 19
        MultiSelect = True
        MultiSelectStyle = [msControlSelect, msShiftSelect]
        ReadOnly = True
        RightClickSelect = True
        SortType = stText
        TabOrder = 0
        OnClick = ProjectViewClick
        OnCompare = ProjectViewCompare
        OnContextPopup = ProjectViewContextPopup
        OnDragDrop = ProjectViewDragDrop
        OnDragOver = ProjectViewDragOver
        OnKeyDown = ProjectViewKeyDown
        OnKeyPress = ProjectViewKeyPress
        OnMouseDown = ProjectViewMouseDown
      end
    end
    object LeftClassSheet: TTabSheet
      Caption = 'Classes'
      ImageIndex = -1
      object ClassBrowser: TClassBrowser
        Left = 0
        Top = 0
        Width = 230
        Height = 330
        Align = alClient
        Color = clWhite
        Images = dmMain.ClassImages
        ReadOnly = True
        Indent = 19
        TabOrder = 0
        PopupMenu = BrowserPopup
        BorderStyle = bsNone
        MultiSelectStyle = []
        ShowFilter = sfAll
        OnSelect = ClassBrowserSelect
        Parser = CppParser
        ItemImages.Globals = 0
        ItemImages.Classes = 1
        ItemImages.VariablePrivate = 2
        ItemImages.VariableProtected = 3
        ItemImages.VariablePublic = 4
        ItemImages.MethodPrivate = 5
        ItemImages.MethodProtected = 6
        ItemImages.MethodPublic = 7
        ItemImages.InheritedMethodProtected = 8
        ItemImages.InheritedMethodPublic = 10
        ItemImages.InheritedVariableProtected = 9
        ItemImages.InheritedVariablePublic = 11
        ShowInheritedMembers = False
        TabVisible = False
      end
    end
    object LeftDebugSheet: TTabSheet
      Caption = 'Debug'
      ImageIndex = -1
      object DebugView: TTreeView
        Left = 0
        Top = 0
        Width = 230
        Height = 330
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ChangeDelay = 1
        Images = dmMain.MenuImages_NewLook
        Indent = 19
        MultiSelectStyle = []
        PopupMenu = DebugPopup
        ReadOnly = True
        RightClickSelect = True
        TabOrder = 0
        OnAdvancedCustomDrawItem = DebugViewAdvancedCustomDrawItem
        OnKeyDown = DebugViewKeyDown
      end
    end
  end
  object PageControlPanel: TPanel
    Left = 241
    Top = 56
    Width = 991
    Height = 361
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 5
    OnResize = PageControlPanelResize
    object EditorPageControlSplitter: TSplitter
      Left = 991
      Top = 0
      Width = 0
      Height = 361
      Align = alRight
      ResizeStyle = rsUpdate
      Visible = False
    end
    object EditorPageControlLeft: TPageControl
      Left = 0
      Top = 0
      Width = 991
      Height = 361
      Align = alClient
      HotTrack = True
      MultiLine = True
      PopupMenu = EditorPopup
      TabOrder = 0
      Visible = False
      OnChange = EditorPageControlChange
      OnDragDrop = EditorPageControlDragDrop
      OnDragOver = EditorPageControlDragOver
      OnMouseDown = EditorPageControlMouseDown
      OnMouseMove = EditorPageControlMouseMove
    end
    object EditorPageControlRight: TPageControl
      Left = 991
      Top = 0
      Width = 0
      Height = 361
      Align = alRight
      HotTrack = True
      MultiLine = True
      PopupMenu = EditorPopup
      TabOrder = 1
      Visible = False
      OnChange = EditorPageControlChange
      OnDragDrop = EditorPageControlDragDrop
      OnDragOver = EditorPageControlDragOver
      OnMouseDown = EditorPageControlMouseDown
      OnMouseMove = EditorPageControlMouseMove
    end
  end
  object MainMenu: TMainMenu
    AutoLineReduction = maManual
    Images = dmMain.MenuImages_NewLook
    Left = 206
    Top = 79
    object FileMenu: TMenuItem
      Caption = '&File'
      OnClick = actFileMenuExecute
      object mnuNew: TMenuItem
        Caption = 'New'
        object NewSourceFileItem: TMenuItem
          Tag = 2
          Action = actNewSource
        end
        object NewprojectItem: TMenuItem
          Action = actNewProject
        end
        object N13: TMenuItem
          Caption = '-'
        end
        object NewTemplateItem: TMenuItem
          Action = actNewTemplate
        end
        object N5: TMenuItem
          Caption = '-'
        end
        object NewClassItem: TMenuItem
          Action = actNewClass
        end
      end
      object N34: TMenuItem
        Caption = '-'
      end
      object OpenprojectItem: TMenuItem
        Tag = 1
        Action = actOpen
      end
      object SaveUnitItem: TMenuItem
        Tag = 3
        Action = actSave
      end
      object SaveUnitAsItem: TMenuItem
        Action = actSaveAs
      end
      object SaveprojectasItem: TMenuItem
        Action = actSaveProjectAs
      end
      object SaveallItem: TMenuItem
        Action = actSaveAll
      end
      object N33: TMenuItem
        Caption = '-'
      end
      object CloseItem: TMenuItem
        Tag = 4
        Action = actClose
      end
      object CloseprojectItem: TMenuItem
        Action = actCloseProject
      end
      object CloseAll2: TMenuItem
        Action = actCloseAll
      end
      object N35: TMenuItem
        Caption = '-'
      end
      object Properties1: TMenuItem
        Action = actFileProperties
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ImportItem: TMenuItem
        Caption = '&Import'
        object ImportMSVisualCproject: TMenuItem
          Action = actImportMSVC
        end
        object ImportCBCproject: TMenuItem
          Caption = 'Code::Blocks project'
          Enabled = False
          Visible = False
          OnClick = ImportCBCprojectClick
        end
      end
      object ExportItem: TMenuItem
        Caption = '&Export'
        ImageIndex = 12
        object HTMLItem: TMenuItem
          Action = actExportHTML
        end
        object RTFItem: TMenuItem
          Action = actExportRTF
        end
        object TEXItem: TMenuItem
          Action = actExportTex
        end
        object N19: TMenuItem
          Caption = '-'
        end
        object ProjecttoHTMLItem: TMenuItem
          Action = actExportProject
        end
      end
      object N43: TMenuItem
        Caption = '-'
      end
      object PrintItem: TMenuItem
        Tag = 5
        Action = actPrint
      end
      object PrinterSetupItem: TMenuItem
        Action = actPrintSU
        GroupIndex = 9
      end
      object N21: TMenuItem
        Caption = '-'
        GroupIndex = 9
      end
      object N76: TMenuItem
        Caption = '-'
        GroupIndex = 9
      end
      object N11: TMenuItem
        Caption = '-'
        GroupIndex = 9
      end
      object ClearhistoryItem: TMenuItem
        Action = actHistoryClear
        GroupIndex = 9
      end
      object N3: TMenuItem
        Caption = '-'
        Enabled = False
        GroupIndex = 9
      end
      object ExitItem: TMenuItem
        Action = actExit
        GroupIndex = 9
      end
    end
    object EditMenu: TMenuItem
      Caption = '&Edit'
      OnClick = actFileMenuExecute
      object UndoItem: TMenuItem
        Tag = 6
        Action = actUndo
      end
      object RedoItem: TMenuItem
        Action = actRedo
      end
      object N4: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object CutItem: TMenuItem
        Action = actCut
        AutoHotkeys = maAutomatic
        AutoLineReduction = maAutomatic
      end
      object CopyItem: TMenuItem
        Action = actCopy
      end
      object PasteItem: TMenuItem
        Action = actPaste
      end
      object SelectallItem: TMenuItem
        Action = actSelectAll
      end
      object N23: TMenuItem
        Caption = '-'
      end
      object UseUTF8Encoding: TMenuItem
        Action = actUseUTF8
      end
      object N47: TMenuItem
        Caption = '-'
      end
      object InsertItem: TMenuItem
        Action = actInsert
        object DateTimeMenuItem: TMenuItem
          Caption = '&Date/Time'
          OnClick = DateTimeMenuItemClick
        end
        object CommentheaderMenuItem: TMenuItem
          Caption = '&Comment Header'
          OnClick = CommentheaderMenuItemClick
        end
        object N999: TMenuItem
          Caption = '-'
        end
      end
      object ToggleBookmarksItem: TMenuItem
        Action = actToggle
      end
      object GotoBookmarksItem: TMenuItem
        Action = actGoto
      end
      object N26: TMenuItem
        Caption = '-'
      end
      object Comment1: TMenuItem
        Action = actComment
      end
      object Uncomment1: TMenuItem
        Action = actUncomment
      end
      object ToggleComment1: TMenuItem
        Action = actToggleComment
      end
      object actCommentInlineSel1: TMenuItem
        Action = actToggleCommentInline
      end
      object N27: TMenuItem
        Caption = '-'
      end
      object Indent1: TMenuItem
        Action = actIndent
      end
      object Unindent1: TMenuItem
        Action = actUnindent
      end
      object N64: TMenuItem
        Caption = '-'
      end
      object CollapseAll: TMenuItem
        Action = actCollapse
      end
      object UncollapseAll: TMenuItem
        Action = actUnCollapse
      end
      object N37: TMenuItem
        Caption = '-'
      end
      object DuplicateLine1: TMenuItem
        Action = actDuplicateLine
      end
      object DeleteLine1: TMenuItem
        Action = actDeleteLine
      end
      object N68: TMenuItem
        Caption = '-'
      end
      object actMoveSelUp1: TMenuItem
        Action = actMoveSelUp
      end
      object actMoveSelDown1: TMenuItem
        Action = actMoveSelDown
      end
    end
    object SearchMenu: TMenuItem
      Caption = '&Search'
      OnClick = actFileMenuExecute
      object FindItem: TMenuItem
        Tag = 7
        Action = actFind
      end
      object FindinallfilesItem: TMenuItem
        Action = actFindAll
      end
      object ReplaceItem: TMenuItem
        Action = actReplace
      end
      object ReplaceAll1: TMenuItem
        Action = actReplaceAll
      end
      object N72: TMenuItem
        Caption = '-'
      end
      object actSearchAgain1: TMenuItem
        Action = actSearchAgain
      end
      object SearchAgainBackwards1: TMenuItem
        Action = actRevSearchAgain
      end
      object N75: TMenuItem
        Caption = '-'
      end
      object IncrementalSearch1: TMenuItem
        Action = actIncremental
      end
      object N7: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object Gotofunction1: TMenuItem
        Action = actGotoFunction
      end
      object GotolineItem: TMenuItem
        Action = actGotoLine
      end
    end
    object ViewMenu: TMenuItem
      Caption = '&View'
      OnClick = actFileMenuExecute
      object ProjectManagerItem: TMenuItem
        Action = actProjectManager
        AutoCheck = True
      end
      object StatusbarItem: TMenuItem
        Action = actStatusbar
        AutoCheck = True
      end
      object ToolbarsItem: TMenuItem
        Caption = '&Toolbars'
        ImageIndex = 44
        object ToolMainItem: TMenuItem
          AutoCheck = True
          Caption = '&Main'
          Checked = True
          OnClick = ToolbarDockClick
        end
        object ToolEditItem: TMenuItem
          AutoCheck = True
          Caption = 'Edit'
          Checked = True
          OnClick = ToolbarDockClick
        end
        object ToolSearchItem: TMenuItem
          AutoCheck = True
          Caption = 'Search'
          Checked = True
          OnClick = ToolbarDockClick
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object ToolProjectItem: TMenuItem
          AutoCheck = True
          Caption = '&Project'
          Checked = True
          OnClick = ToolbarDockClick
        end
        object ToolCompileandRunItem: TMenuItem
          AutoCheck = True
          Caption = '&Compile and Run'
          Checked = True
          OnClick = ToolbarDockClick
        end
        object ToolCompilersItem: TMenuItem
          AutoCheck = True
          Caption = 'Compilers'
          Checked = True
          OnClick = ToolbarDockClick
        end
        object N9: TMenuItem
          Caption = '-'
        end
        object ToolSpecialsItem: TMenuItem
          AutoCheck = True
          Caption = '&Specials'
          Checked = True
          OnClick = ToolbarDockClick
        end
        object N17: TMenuItem
          Caption = '-'
        end
        object ToolClassesItem: TMenuItem
          AutoCheck = True
          Caption = 'Classes'
          Checked = True
          OnClick = ToolbarDockClick
        end
      end
      object oDolist1: TMenuItem
        Action = actViewToDoList
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object MoveToOtherViewItem: TMenuItem
        Action = actSwapEditor
      end
      object SwapHeaderSourceItem: TMenuItem
        Action = actSwapHeaderSource
      end
      object N63: TMenuItem
        Caption = '-'
      end
      object FloatingPojectManagerItem: TMenuItem
        Caption = '&Floating Project Manager'
        OnClick = FloatingPojectManagerItemClick
      end
      object FloatingReportwindowItem: TMenuItem
        Caption = 'Floating &Report window'
        OnClick = FloatingReportwindowItemClick
      end
    end
    object ProjectMenu: TMenuItem
      Caption = '&Project'
      OnClick = actFileMenuExecute
      object NewunitinprojectItem: TMenuItem
        Tag = 2
        Action = actProjectNew
      end
      object AddtoprojectItem: TMenuItem
        Action = actProjectAdd
      end
      object RemovefromprojectItem: TMenuItem
        Action = actProjectRemove
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object ProjectoptionsItem: TMenuItem
        Action = actProjectOptions
      end
    end
    object ExecuteMenu: TMenuItem
      Caption = 'E&xecute'
      OnClick = actFileMenuExecute
      object CompileItem: TMenuItem
        Tag = 8
        Action = actCompile
      end
      object RunItem: TMenuItem
        Tag = 9
        Action = actRun
      end
      object CompileandRunItem: TMenuItem
        Action = actCompRun
      end
      object RebuildallItem: TMenuItem
        Action = actRebuild
      end
      object N69: TMenuItem
        Caption = '-'
      end
      object SyntaxCheckItem: TMenuItem
        Action = actSyntaxCheck
      end
      object SyntaxCheckCurrentFile1: TMenuItem
        Action = actSyntaxCheckFile
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object mnuExecParameters: TMenuItem
        Action = actExecParams
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object N49: TMenuItem
        Action = actProjectMakeFile
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object CleanItem: TMenuItem
        Action = actClean
      end
      object N29: TMenuItem
        Caption = '-'
      end
      object Profileanalysis1: TMenuItem
        Action = actProfile
      end
      object DeleteProfilingInformation: TMenuItem
        Action = actDeleteProfile
      end
      object N25: TMenuItem
        Caption = '-'
      end
      object Abortcompilation2: TMenuItem
        Action = actGotoBreakPoint
        ShortCut = 113
      end
      object oggleBreakpoint1: TMenuItem
        Action = actBreakPoint
      end
      object Abortcompilation1: TMenuItem
        Action = actDebug
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object Programreset1: TMenuItem
        Action = actStopExecute
      end
    end
    object ToolsMenu: TMenuItem
      Caption = '&Tools'
      OnClick = actToolsMenuExecute
      object CompileroptionsItem: TMenuItem
        Tag = 11
        Action = actCompOptions
      end
      object EnvironmentoptionsItem: TMenuItem
        Tag = 12
        Action = actEnviroOptions
      end
      object EditorOptionsItem: TMenuItem
        Action = actEditorOptions
      end
      object N20: TMenuItem
        Caption = '-'
      end
      object ConfiguredevShortcuts1: TMenuItem
        Action = actConfigdevShortcuts
      end
      object ConfiguretoolsItem: TMenuItem
        Action = actConfigTools
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object PackageManagerItem: TMenuItem
        Action = actPackageManager
        Caption = 'Package Manager'
      end
    end
    object RefactorMenu: TMenuItem
      Caption = 'Refactor'
      GroupIndex = 9
      object Rename: TMenuItem
        Action = actRenameSymbol
      end
    end
    object FormatMenu: TMenuItem
      Caption = 'AStyle'
      GroupIndex = 9
      object FormatCurrentFile1: TMenuItem
        Action = actFormatCurrentFile
      end
      object N46: TMenuItem
        Caption = '-'
      end
      object actFormatOptions1: TMenuItem
        Action = actFormatOptions
      end
    end
    object WindowMenu: TMenuItem
      Caption = '&Window'
      GroupIndex = 9
      OnClick = actWindowMenuExecute
      object CloseAllItem: TMenuItem
        Action = actCloseAll
      end
      object N28: TMenuItem
        Caption = '-'
      end
      object FullscreenmodeItem: TMenuItem
        Action = actFullScreen
        AutoCheck = True
      end
      object N36: TMenuItem
        Caption = '-'
      end
      object NextItem: TMenuItem
        Action = actNext
      end
      object PreviousItem: TMenuItem
        Action = actPrev
      end
      object N32: TMenuItem
        Caption = '-'
      end
      object ListItem: TMenuItem
        Caption = '&List...'
        OnClick = ListItemClick
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Help'
      GroupIndex = 9
      OnClick = actFileMenuExecute
      object HelpMenuItem: TMenuItem
        Action = actHelp
      end
      object ShowTipsItem: TMenuItem
        Action = actShowTips
      end
      object AboutDevCppItem: TMenuItem
        Tag = 18
        Action = actAbout
      end
      object DonateItem: TMenuItem
        Action = actDonate
      end
    end
  end
  object EditorPopup: TPopupMenu
    Left = 403
    Top = 216
    object GotoDeclEditor: TMenuItem
      Action = actGotoDeclEditor
    end
    object GotoDefineEditor: TMenuItem
      Action = actGotoImplEditor
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object Close1: TMenuItem
      Action = actClose
    end
    object CloseAll1: TMenuItem
      Action = actCloseAll
    end
    object Closeallexceptthis1: TMenuItem
      Action = actCloseAllButThis
    end
    object N77: TMenuItem
      Caption = '-'
    end
    object actOpenFolder1: TMenuItem
      Action = actOpenFolder
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object UndoPopItem: TMenuItem
      Action = actUndo
    end
    object RedoPopItem: TMenuItem
      Action = actRedo
    end
    object MenuItem1: TMenuItem
      Caption = '-'
    end
    object CutPopItem: TMenuItem
      Action = actCut
    end
    object CopyPopItem: TMenuItem
      Action = actCopy
    end
    object PastePopItem: TMenuItem
      Action = actPaste
    end
    object SelectAllPopItem: TMenuItem
      Action = actSelectAll
    end
    object N22: TMenuItem
      Caption = '-'
    end
    object Swapheadersource1: TMenuItem
      Action = actSwapHeaderSource
    end
    object N57: TMenuItem
      Caption = '-'
    end
    object Movetootherview1: TMenuItem
      Action = actSwapEditor
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object InsertPopItem: TMenuItem
      Action = actInsert
    end
    object TogglebookmarksPopItem: TMenuItem
      Action = actToggle
    end
    object GotobookmarksPopItem: TMenuItem
      Action = actGoto
    end
    object N41: TMenuItem
      Caption = '-'
    end
    object ToggleBreakpointPopupItem: TMenuItem
      Action = actBreakPoint
    end
    object AddWatchPopupItem: TMenuItem
      Action = actAddWatch
    end
    object N38: TMenuItem
      Caption = '-'
    end
    object AddToDoitem1: TMenuItem
      Action = actAddToDo
    end
    object N45: TMenuItem
      Caption = '-'
    end
    object mnuFileProps: TMenuItem
      Action = actFileProperties
    end
  end
  object UnitPopup: TPopupMenu
    Images = dmMain.MenuImages_NewLook
    Left = 11
    Top = 135
    object RemoveFilefromprojectPopItem: TMenuItem
      Action = actUnitRemove
    end
    object RenamefilePopItem: TMenuItem
      Action = actUnitRename
    end
    object N30: TMenuItem
      Caption = '-'
    end
    object OpenPopItem: TMenuItem
      Action = actUnitOpen
    end
    object mnuOpenWith: TMenuItem
      Caption = 'Open with'
      OnClick = mnuOpenWithClick
    end
    object ClosefilePopItem: TMenuItem
      Action = actUnitClose
    end
    object N40: TMenuItem
      Caption = '-'
    end
    object mnuUnitProperties: TMenuItem
      Action = actFileProperties
    end
  end
  object ProjectPopup: TPopupMenu
    Images = dmMain.MenuImages_NewLook
    Left = 11
    Top = 104
    object NewunitinprojectPopItem: TMenuItem
      Tag = 2
      Action = actProjectNew
      Caption = '&New File'
    end
    object AddtoprojectPopItem: TMenuItem
      Action = actProjectAdd
    end
    object RemovefromprojectPopItem: TMenuItem
      Action = actProjectRemove
    end
    object N39: TMenuItem
      Caption = '-'
    end
    object Newfolder1: TMenuItem
      Action = actProjectNewFolder
    end
    object MenuItem18: TMenuItem
      Caption = '-'
    end
    object ProjectoptionsPopItem: TMenuItem
      Action = actProjectOptions
    end
    object N48: TMenuItem
      Caption = '-'
    end
    object CloseProject1: TMenuItem
      Action = actCloseProject
    end
  end
  object ActionList: TActionList
    Images = dmMain.MenuImages_NewLook
    Left = 624
    Top = 164
    object actViewCPU: TAction
      Category = 'Debug'
      Caption = '&View CPU window'
      OnExecute = ViewCPUItemClick
      OnUpdate = actUpdateDebuggerRunningCPU
    end
    object actNewSource: TAction
      Tag = 1
      Category = 'File'
      Caption = '&Source File'
      ImageIndex = 1
      ShortCut = 16462
      OnExecute = actNewSourceExecute
    end
    object actNewProject: TAction
      Tag = 2
      Category = 'File'
      Caption = '&Project...'
      ImageIndex = 0
      OnExecute = actNewProjectExecute
    end
    object actNewTemplate: TAction
      Tag = 4
      Category = 'File'
      Caption = '&Template...'
      ImageIndex = 3
      OnExecute = actNewTemplateExecute
      OnUpdate = actNewTemplateUpdate
    end
    object actNewClass: TAction
      Category = 'File'
      Caption = '&Class...'
      ImageIndex = 51
      OnExecute = actBrowserNewClassExecute
      OnUpdate = actBrowserNewClassUpdate
    end
    object actOpen: TAction
      Tag = 1
      Category = 'File'
      Caption = '&Open project or file...'
      ImageIndex = 4
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
    object actHistoryClear: TAction
      Tag = 2
      Category = 'File'
      Caption = '&Clear History'
      ImageIndex = 5
      OnExecute = actHistoryClearExecute
    end
    object actSave: TAction
      Tag = 3
      Category = 'File'
      Caption = '&Save'
      ImageIndex = 6
      ShortCut = 16467
      OnExecute = actSaveExecute
      OnUpdate = actSaveUpdate
    end
    object actSaveAs: TAction
      Tag = 4
      Category = 'File'
      Caption = 'Save &As'
      ImageIndex = 7
      OnExecute = actSaveAsExecute
      OnUpdate = actSaveAsUpdate
    end
    object actSaveProjectAs: TAction
      Category = 'File'
      Caption = 'Save project as...'
      ImageIndex = 7
      OnExecute = actSaveProjectAsExecute
      OnUpdate = actUpdateProject
    end
    object actSaveAll: TAction
      Tag = 5
      Category = 'File'
      Caption = 'Save A&ll'
      ImageIndex = 8
      ShortCut = 24659
      OnExecute = actSaveAllExecute
      OnUpdate = actSaveAllUpdate
    end
    object actClose: TAction
      Tag = 7
      Category = 'File'
      Caption = '&Close'
      ImageIndex = 9
      ShortCut = 16471
      OnExecute = actCloseExecute
      OnUpdate = actUpdatePageCount
    end
    object actCloseAll: TAction
      Tag = 11
      Category = 'File'
      Caption = 'Close All'
      ImageIndex = 50
      ShortCut = 24663
      OnExecute = actCloseAllExecute
      OnUpdate = actUpdatePageCount
    end
    object actCloseProject: TAction
      Tag = 6
      Category = 'File'
      Caption = 'Close Project'
      ImageIndex = 11
      OnExecute = actCloseProjectExecute
      OnUpdate = actUpdateProject
    end
    object actExportHTML: TAction
      Tag = 1
      Category = 'File'
      Caption = 'to &HTML'
      OnExecute = actExportHTMLExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actExportRTF: TAction
      Tag = 2
      Category = 'File'
      Caption = 'to &RTF'
      OnExecute = actExportRTFExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actExportTex: TAction
      Tag = 3
      Category = 'File'
      Caption = 'to &Tex'
      OnExecute = actExportTexExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actExportProject: TAction
      Tag = 3
      Category = 'File'
      Caption = '&Project to HTML'
      OnExecute = actExportProjectExecute
      OnUpdate = actUpdateProject
    end
    object actPrint: TAction
      Tag = 8
      Category = 'File'
      Caption = '&Print'
      ImageIndex = 10
      ShortCut = 16464
      OnExecute = actPrintExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actPrintSU: TAction
      Tag = 9
      Category = 'File'
      Caption = 'Prin&ter Setup...'
      OnExecute = actPrintSUExecute
    end
    object actExit: TAction
      Tag = 10
      Category = 'File'
      Caption = 'E&xit Dev-C++'
      ImageIndex = 11
      ShortCut = 32883
      OnExecute = actExitExecute
    end
    object actUndo: TAction
      Tag = 1
      Category = 'Edit'
      Caption = '&Undo'
      ImageIndex = 13
      ShortCut = 16474
      OnExecute = actUndoExecute
      OnUpdate = actUndoUpdate
    end
    object actRedo: TAction
      Tag = 2
      Category = 'Edit'
      Caption = '&Redo'
      ImageIndex = 14
      ShortCut = 16473
      OnExecute = actRedoExecute
      OnUpdate = actRedoUpdate
    end
    object actCut: TAction
      Tag = 3
      Category = 'Edit'
      Caption = 'C&ut'
      ImageIndex = 15
      ShortCut = 16472
      OnExecute = actCutExecute
      OnUpdate = actCutUpdate
    end
    object actCopy: TAction
      Tag = 4
      Category = 'Edit'
      Caption = '&Copy'
      ImageIndex = 16
      ShortCut = 16451
      OnExecute = actCopyExecute
      OnUpdate = actCopyUpdate
    end
    object actPaste: TAction
      Tag = 5
      Category = 'Edit'
      Caption = '&Paste'
      ImageIndex = 17
      ShortCut = 16470
      OnExecute = actPasteExecute
      OnUpdate = actPasteUpdate
    end
    object actSelectAll: TAction
      Tag = 6
      Category = 'Edit'
      Caption = '&Select All'
      ShortCut = 16449
      OnExecute = actSelectAllExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actFind: TAction
      Tag = 1
      Category = 'Search'
      Caption = '&Find'
      ImageIndex = 21
      ShortCut = 16454
      OnExecute = actFindExecute
      OnUpdate = actUpdateEmptyEditorFindForm
    end
    object actFindAll: TAction
      Tag = 2
      Category = 'Search'
      Caption = 'Fin&d in all Files'
      ShortCut = 24646
      OnExecute = actFindAllExecute
      OnUpdate = actUpdateEmptyEditorFindForm
    end
    object actReplace: TAction
      Tag = 3
      Category = 'Search'
      Caption = '&Replace'
      ImageIndex = 22
      ShortCut = 16466
      OnExecute = actReplaceExecute
      OnUpdate = actUpdateEmptyEditorFindForm
    end
    object actReplaceAll: TAction
      Category = 'Search'
      Caption = 'Replace All'
      ShortCut = 24658
      OnExecute = actReplaceAllExecute
      OnUpdate = actUpdateEmptyEditorFindForm
    end
    object actIncremental: TAction
      Category = 'Search'
      Caption = 'Incremental Search'
      ShortCut = 16457
      OnExecute = actIncrementalExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actGotoLine: TAction
      Tag = 5
      Category = 'Search'
      Caption = '&Go to line...'
      ImageIndex = 24
      ShortCut = 16455
      OnExecute = actGotoLineExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actProjectManager: TAction
      Category = 'View'
      AutoCheck = True
      Caption = '&Project Manager'
      OnExecute = actProjectManagerExecute
    end
    object actStatusbar: TAction
      Category = 'View'
      AutoCheck = True
      Caption = '&Statusbar'
      OnExecute = actStatusbarExecute
    end
    object actProjectNew: TAction
      Tag = 1
      Category = 'Project'
      Caption = '&New Unit'
      ImageIndex = 1
      OnExecute = actProjectNewExecute
      OnUpdate = actUpdateProject
    end
    object actProjectAdd: TAction
      Tag = 2
      Category = 'Project'
      Caption = '&Add file...'
      ImageIndex = 25
      OnExecute = actProjectAddExecute
      OnUpdate = actUpdateProject
    end
    object actProjectRemove: TAction
      Tag = 3
      Category = 'Project'
      Caption = '&Remove file...'
      ImageIndex = 26
      OnExecute = actProjectRemoveExecute
      OnUpdate = actUpdateProject
    end
    object actProjectOptions: TAction
      Tag = 5
      Category = 'Project'
      Caption = '&Options...'
      ImageIndex = 27
      ShortCut = 16456
      OnExecute = actProjectOptionsExecute
      OnUpdate = actUpdateProject
    end
    object actProjectMakeFile: TAction
      Category = 'Project'
      Caption = 'Edit &Makefile'
      OnExecute = actProjectMakeFileExecute
      OnUpdate = actUpdateMakeFile
    end
    object actProjectSource: TAction
      Tag = 6
      Category = 'Project'
      Caption = 'Source'
      OnUpdate = actUpdateProject
    end
    object actCompile: TAction
      Tag = 1
      Category = 'Execute'
      Caption = '&Compile'
      ImageIndex = 28
      ShortCut = 120
      OnExecute = actCompileExecute
      OnUpdate = actDebugExecuteUpdate
    end
    object actRun: TAction
      Tag = 2
      Category = 'Execute'
      Caption = '&Run'
      ImageIndex = 31
      ShortCut = 121
      OnExecute = actRunExecute
      OnUpdate = actRunUpdate
    end
    object actCompRun: TAction
      Tag = 3
      Category = 'Execute'
      Caption = 'Compile && Run'
      ImageIndex = 33
      ShortCut = 122
      OnExecute = actCompRunExecute
      OnUpdate = actCompileRunUpdate
    end
    object actRebuild: TAction
      Tag = 4
      Category = 'Execute'
      Caption = 'R&ebuild All'
      ImageIndex = 30
      ShortCut = 123
      OnExecute = actRebuildExecute
      OnUpdate = actDebugExecuteUpdate
    end
    object actClean: TAction
      Tag = 5
      Category = 'Execute'
      Caption = 'C&lean'
      ImageIndex = 5
      OnExecute = actCleanExecute
      OnUpdate = actDebugExecuteUpdate
    end
    object actDebug: TAction
      Tag = 6
      Category = 'Debug'
      Caption = '&Debug'
      ImageIndex = 32
      ShortCut = 116
      OnExecute = actDebugExecute
      OnUpdate = actDebugExecuteUpdate
    end
    object actCompOptions: TAction
      Tag = 1
      Category = 'Tools'
      Caption = '&Compiler Options...'
      ImageIndex = 34
      OnExecute = actCompOptionsExecute
    end
    object actEnviroOptions: TAction
      Tag = 2
      Category = 'Tools'
      Caption = '&Environment Options...'
      ImageIndex = 35
      OnExecute = actEnviroOptionsExecute
    end
    object actEditorOptions: TAction
      Tag = 3
      Category = 'Tools'
      Caption = 'E&ditor Options...'
      ImageIndex = 36
      OnExecute = actEditorOptionsExecute
    end
    object actConfigTools: TAction
      Tag = 4
      Category = 'Tools'
      Caption = 'Configure &Tools...'
      ImageIndex = 37
      OnExecute = actConfigToolsExecute
    end
    object actFullScreen: TAction
      Tag = 1
      Category = 'Window'
      AutoCheck = True
      Caption = '&Full screen mode'
      ImageIndex = 38
      ShortCut = 16506
      OnExecute = actFullScreenExecute
    end
    object actNext: TAction
      Tag = 2
      Category = 'Window'
      Caption = '&Next'
      ImageIndex = 39
      ShortCut = 16393
      OnExecute = actNextExecute
      OnUpdate = actUpdatePageCount
    end
    object actPrev: TAction
      Tag = 3
      Category = 'Window'
      Caption = '&Previous'
      ImageIndex = 40
      ShortCut = 24585
      OnExecute = actPrevExecute
      OnUpdate = actUpdatePageCount
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      ImageIndex = 42
      OnExecute = actAboutExecute
    end
    object actUnitRemove: TAction
      Tag = 1
      Category = 'Project'
      Caption = '&Remove from project'
      OnExecute = actUnitRemoveExecute
      OnUpdate = actUpdateProject
    end
    object actUnitRename: TAction
      Tag = 2
      Category = 'Project'
      Caption = 'Re&name file'
      ShortCut = 113
      OnExecute = actUnitRenameExecute
      OnUpdate = actUpdateProject
    end
    object actUnitHeader: TAction
      Tag = 5
      Category = 'Project'
      Caption = 'Open &Header'
      OnUpdate = actUpdateProject
    end
    object actUnitOpen: TAction
      Tag = 4
      Category = 'Project'
      Caption = '&Open'
      OnExecute = actUnitOpenExecute
      OnUpdate = actUpdateProject
    end
    object actUnitClose: TAction
      Tag = 3
      Category = 'Project'
      Caption = '&Close'
      OnExecute = actUnitCloseExecute
      OnUpdate = actUpdateProject
    end
    object actShowBars: TAction
      Category = 'View'
      Caption = 'Show Toolbars'
      ShortCut = 16507
      OnExecute = actShowBarsExecute
    end
    object actBreakPoint: TAction
      Category = 'Debug'
      Caption = 'Toggle Breakpoint'
      ShortCut = 115
      OnExecute = actBreakPointExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actAddWatch: TAction
      Category = 'Debug'
      Caption = '&Add watch'
      ImageIndex = 21
      OnExecute = actAddWatchExecute
      OnUpdate = actUpdatePageCount
    end
    object actEditWatch: TAction
      Category = 'Debug'
      Caption = '&Edit watch'
      ImageIndex = 36
    end
    object actContinue: TAction
      Category = 'Debug'
      Caption = '&Continue'
      OnExecute = actContinueExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actWatchItem: TAction
      Category = 'Debug'
      Caption = '&Watch variables'
      OnUpdate = actUpdatePageorProject
    end
    object actRemoveWatch: TAction
      Category = 'Debug'
      Caption = '&Remove watch'
      ImageIndex = 5
      OnExecute = actRemoveWatchExecute
      OnUpdate = actUpdateDeleteWatch
    end
    object actStopExecute: TAction
      Category = 'Debug'
      Caption = 'Stop execution'
      ImageIndex = 11
      ShortCut = 117
      OnExecute = actStopExecuteExecute
      OnUpdate = actStopExecuteUpdate
    end
    object actSwapHeaderSource: TAction
      Category = 'Edit'
      Caption = '&Swap Header/Source'
      ShortCut = 16465
      OnExecute = actSwapHeaderSourceExecute
      OnUpdate = actUpdatePageCount
    end
    object actSyntaxCheck: TAction
      Category = 'Execute'
      Caption = '&Syntax Check'
      ImageIndex = 49
      OnExecute = actSyntaxCheckExecute
      OnUpdate = actDebugExecuteUpdate
    end
    object actConfigdevShortcuts: TAction
      Category = 'Tools'
      Caption = 'Configure &Shortcuts'
      ImageIndex = 31
      OnExecute = actConfigdevShortcutsExecute
    end
    object actComment: TAction
      Category = 'Edit'
      Caption = 'Comment'
      ShortCut = 16574
      OnExecute = actCommentExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actUncomment: TAction
      Category = 'Edit'
      Caption = 'Uncomment'
      ShortCut = 16572
      OnExecute = actUncommentExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actToggleComment: TAction
      Category = 'Edit'
      Caption = 'Toggle Comment'
      ShortCut = 16575
      OnExecute = actToggleCommentExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actToggleCommentInline: TAction
      Category = 'Edit'
      Caption = 'Toggle Inline Comment'
      ShortCut = 16570
      OnExecute = actToggleCommentInlineExecute
      OnUpdate = actToggleCommentInlineUpdate
    end
    object actIndent: TAction
      Category = 'Edit'
      Caption = 'Indent'
      ShortCut = 9
      OnExecute = actIndentExecute
      OnUpdate = actUpdateIndent
    end
    object actUnindent: TAction
      Category = 'Edit'
      Caption = 'Unindent'
      ShortCut = 8201
      OnExecute = actUnindentExecute
      OnUpdate = actUpdateIndent
    end
    object actGotoFunction: TAction
      Category = 'Search'
      Caption = 'Goto function'
      ImageIndex = 44
      ShortCut = 24647
      OnExecute = actGotoFunctionExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actBrowserGotoDeclaration: TAction
      Category = 'ClassBrowser'
      Caption = 'Goto declaration'
      OnExecute = actBrowserGotoDeclarationExecute
      OnUpdate = actBrowserGotoDeclarationUpdate
    end
    object actBrowserGotoDefinition: TAction
      Category = 'ClassBrowser'
      Caption = 'Goto definition'
      OnExecute = actBrowserGotoDefinitionExecute
      OnUpdate = actBrowserGotoDefinitionUpdate
    end
    object actBrowserNewClass: TAction
      Category = 'ClassBrowser'
      Caption = 'New class'
      ImageIndex = 51
      OnExecute = actBrowserNewClassExecute
      OnUpdate = actBrowserNewClassUpdate
    end
    object actBrowserNewMember: TAction
      Category = 'ClassBrowser'
      Caption = 'New member function'
      ImageIndex = 52
      OnExecute = actBrowserNewMemberExecute
      OnUpdate = actBrowserNewMemberUpdate
    end
    object actBrowserNewVar: TAction
      Category = 'ClassBrowser'
      Caption = 'New variable'
      ImageIndex = 53
      OnExecute = actBrowserNewVarExecute
      OnUpdate = actBrowserNewVarUpdate
    end
    object actBrowserViewAll: TAction
      Category = 'ClassBrowser'
      Caption = 'All files'
      Checked = True
      OnExecute = actBrowserViewAllExecute
      OnUpdate = actBrowserViewAllUpdate
    end
    object actBrowserViewProject: TAction
      Category = 'ClassBrowser'
      Caption = 'Project files'
      OnExecute = actBrowserViewProjectExecute
      OnUpdate = actBrowserViewAllUpdate
    end
    object actBrowserViewCurrent: TAction
      Category = 'ClassBrowser'
      Caption = 'Current file'
      OnExecute = actBrowserViewCurrentExecute
      OnUpdate = actBrowserViewAllUpdate
    end
    object actSyntaxCheckFile: TAction
      Category = 'Execute'
      Caption = '&Syntax Check Current File'
      ImageIndex = 49
      ShortCut = 16504
      OnExecute = actSyntaxCheckFileExecute
      OnUpdate = actDebugExecuteUpdate
    end
    object actProfile: TAction
      Category = 'Execute'
      Caption = 'Profile analysis'
      ImageIndex = 43
      OnExecute = actProfileExecute
      OnUpdate = actCompileRunUpdate
    end
    object actBrowserViewIncludes: TAction
      Category = 'ClassBrowser'
      Caption = 'System Headers'
      OnExecute = actBrowserViewIncludesExecute
      OnUpdate = actBrowserViewAllUpdate
    end
    object actBrowserAddFolder: TAction
      Category = 'ClassBrowser'
      Caption = 'Add folder'
    end
    object actBrowserRemoveFolder: TAction
      Category = 'ClassBrowser'
      Caption = 'Remove folder'
    end
    object actBrowserRenameFolder: TAction
      Category = 'ClassBrowser'
      Caption = 'Rename folder'
    end
    object actCloseAllButThis: TAction
      Category = 'File'
      Caption = 'Close All Except This'
      OnExecute = actCloseAllButThisExecute
    end
    object actFileProperties: TAction
      Category = 'File'
      Caption = 'Properties'
      OnExecute = actFilePropertiesExecute
      OnUpdate = actUpdatePageCount
    end
    object actViewToDoList: TAction
      Category = 'View'
      Caption = 'To-Do list...'
      OnExecute = actViewToDoListExecute
      OnUpdate = actUpdatePageorProject
    end
    object actAddToDo: TAction
      Category = 'Edit'
      Caption = 'Add To-Do item...'
      ShortCut = 16468
      OnExecute = actAddToDoExecute
      OnUpdate = actUpdatePageorProject
    end
    object actProjectNewFolder: TAction
      Category = 'Project'
      Caption = 'Add folder'
      OnExecute = actProjectNewFolderExecute
      OnUpdate = actUpdateProject
    end
    object actProjectRemoveFolder: TAction
      Category = 'Project'
      Caption = 'Remove folder'
      OnExecute = actProjectRemoveFolderExecute
      OnUpdate = actUpdateProject
    end
    object actProjectRenameFolder: TAction
      Category = 'Project'
      Caption = 'Rename folder'
      OnExecute = actProjectRenameFolderExecute
      OnUpdate = actUpdateProject
    end
    object actImportMSVC: TAction
      Category = 'File'
      Caption = 'MS Visual C++ project'
      OnExecute = actImportMSVCExecute
    end
    object actExecParams: TAction
      Category = 'Execute'
      Caption = 'Parameters...'
      OnExecute = actExecParamsExecute
    end
    object actShowTips: TAction
      Category = 'Help'
      Caption = 'Tips'
      OnExecute = actShowTipsExecute
    end
    object actAbortCompilation: TAction
      Category = 'Execute'
      Caption = 'Abort compilation'
      OnExecute = actAbortCompilationExecute
      OnUpdate = actAbortCompilationUpdate
    end
    object actBrowserShowInherited: TAction
      Category = 'ClassBrowser'
      Caption = 'Show inherited members'
      OnExecute = actBrowserShowInheritedExecute
    end
    object actAttachProcess: TAction
      Category = 'Debug'
      Caption = 'Attach to process...'
      OnExecute = actAttachProcessExecute
      OnUpdate = actAttachProcessUpdate
    end
    object actModifyWatch: TAction
      Category = 'Debug'
      Caption = '&Modify watch'
      ImageIndex = 37
      OnExecute = actModifyWatchExecute
      OnUpdate = actModifyWatchUpdate
    end
    object actStepOver: TAction
      Category = 'Debug'
      Caption = 'Step &Over'
      ShortCut = 118
      OnExecute = actStepOverExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actStepInto: TAction
      Category = 'Debug'
      Caption = 'Step Into'
      ShortCut = 119
      OnExecute = actStepIntoExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actDeleteProfile: TAction
      Category = 'Execute'
      Caption = 'Delete Profiling information'
      ImageIndex = 47
      OnExecute = actDeleteProfileExecute
      OnUpdate = actRunUpdate
    end
    object actGotoDeclEditor: TAction
      Category = 'ClassBrowser'
      Caption = 'Goto Declaration'
      OnExecute = actGotoImplDeclEditorExecute
    end
    object actGotoImplEditor: TAction
      Category = 'ClassBrowser'
      Caption = 'Goto Implementation'
      OnExecute = actGotoImplDeclEditorExecute
    end
    object actCollapse: TAction
      Category = 'Edit'
      Caption = 'Collapse All'
      OnExecute = actCollapseExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actUnCollapse: TAction
      Category = 'Edit'
      Caption = 'Uncollapse All'
      OnExecute = actUnCollapseExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actInsert: TAction
      Category = 'Edit'
      Caption = 'Insert'
      ImageIndex = 18
      OnExecute = actInsertExecute
      OnUpdate = actUpdatePageCount
    end
    object actToggle: TAction
      Category = 'Edit'
      Caption = 'Toggle Bookmarks'
      ImageIndex = 19
      OnExecute = actToggleExecute
      OnUpdate = actUpdatePageCount
    end
    object actGoto: TAction
      Category = 'Edit'
      Caption = 'Goto Bookmark'
      ImageIndex = 20
      OnExecute = actGotoExecute
      OnUpdate = actUpdatePageCount
    end
    object actRunToCursor: TAction
      Category = 'Debug'
      Caption = '&Run to Cursor'
      OnExecute = actRunToCursorExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actCallStack: TAction
      Category = 'Debug'
      Caption = 'Call Stack'
      OnExecute = actCallStackExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actStepOut: TAction
      Category = 'Debug'
      Caption = 'Step O&ut'
      OnExecute = actStepOutExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actMsgCut: TAction
      Category = 'Messages'
      Caption = 'Cut'
      ShortCut = 16472
      OnExecute = actMsgCutExecute
    end
    object actMsgCopy: TAction
      Category = 'Messages'
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = actMsgCopyExecute
    end
    object actMsgCopyAll: TAction
      Category = 'Messages'
      Caption = 'Copy All'
      ShortCut = 24643
      OnExecute = actMsgCopyAllExecute
    end
    object actMsgPaste: TAction
      Category = 'Messages'
      Caption = 'Paste'
      ShortCut = 16470
      OnExecute = actMsgPasteExecute
    end
    object actMsgSelAll: TAction
      Category = 'Messages'
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = actMsgSelAllExecute
    end
    object actMsgSaveAll: TAction
      Category = 'Messages'
      Caption = 'Save All'
      ShortCut = 16467
      OnExecute = actMsgSaveAllExecute
    end
    object actMsgClear: TAction
      Category = 'Messages'
      Caption = 'Clear'
      OnExecute = actMsgClearExecute
    end
    object actSearchAgain: TAction
      Category = 'Search'
      Caption = 'Search Again'
      ShortCut = 114
      OnExecute = actSearchAgainExecute
      OnUpdate = actUpdateEmptyEditorFindForm
    end
    object actRevSearchAgain: TAction
      Category = 'Search'
      Caption = 'Search Again Backwards'
      ShortCut = 8306
      OnExecute = actRevSearchAgainExecute
      OnUpdate = actUpdateEmptyEditorFindForm
    end
    object actDeleteLine: TAction
      Category = 'Edit'
      Caption = 'Delete Line'
      ShortCut = 16452
      OnExecute = actDeleteLineExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actDuplicateLine: TAction
      Category = 'Edit'
      Caption = 'Duplicate Line'
      ShortCut = 16453
      OnExecute = actDuplicateLineExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actMoveSelUp: TAction
      Category = 'Edit'
      Caption = 'actMoveSelUp'
      ShortCut = 24614
      OnExecute = actMoveSelUpExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actMoveSelDown: TAction
      Category = 'Edit'
      Caption = 'actMoveSelDown'
      ShortCut = 24616
      OnExecute = actMoveSelDownExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actCodeCompletion: TAction
      Category = 'ClassBrowser'
      Caption = 'Show Code Completion'
      ShortCut = 16416
      OnExecute = actCodeCompletionExecute
      OnUpdate = actCodeCompletionUpdate
    end
    object actPackageCheck: TAction
      Category = 'Tools'
      Caption = '&Check for Packages'
      ImageIndex = 41
    end
    object actPackageManager: TAction
      Category = 'Tools'
      Caption = 'actPackageManager'
      ImageIndex = 48
      OnExecute = actPackageManagerExecute
    end
    object actHelp: TAction
      Category = 'Help'
      Caption = 'Help'
      ImageIndex = 46
      ShortCut = 112
      OnExecute = actHelpExecute
    end
    object actShortenCompPaths: TAction
      Category = 'Execute'
      Caption = 'Shorten compiler paths'
      OnExecute = actShortenCompPathsExecute
    end
    object actSwapEditor: TAction
      Category = 'View'
      Caption = 'Move To Other View'
      ShortCut = 16461
      OnExecute = actSwapEditorExecute
      OnUpdate = actSwapEditorUpdate
    end
    object actOpenFolder: TAction
      Category = 'File'
      Caption = 'Open Containing Folder'
      ImageIndex = 0
      ShortCut = 16450
      OnExecute = actOpenFolderExecute
      OnUpdate = actUpdatePageCount
    end
    object actGotoBreakPoint: TAction
      Category = 'Debug'
      Caption = 'Goto Breakpoint'
      OnExecute = actGotoBreakPointExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actFormatCurrentFile: TAction
      Category = 'AStyle'
      Caption = 'Format Current File'
      ShortCut = 24641
      OnExecute = actFormatCurrentFileExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actFormatOptions: TAction
      Category = 'AStyle'
      Caption = 'Formatting Options'
      OnExecute = actFormatOptionsExecute
    end
    object actRunTests: TAction
      Category = 'Tests'
      Caption = 'Run Tests'
      Enabled = False
      ShortCut = 16497
      OnExecute = actRunTestsExecute
    end
    object actDonate: TAction
      Caption = 'Donate :)'
      ImageIndex = 41
      OnExecute = actDonateExecute
    end
    object actRenameSymbol: TAction
      Category = 'Refactor'
      Caption = 'Rename Symbol'
      ShortCut = 24693
      OnExecute = actRenameSymbolExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actUseUTF8: TAction
      Category = 'Edit'
      Caption = 'Use UTF8 Encoding'
      OnExecute = actUseUTF8Execute
      OnUpdate = actUseUTF8Update
    end
    object actCallStackFull: TAction
      Category = 'Debug'
      Caption = 'Call Stack Full'
      OnExecute = actCallStackFullExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actLocals: TAction
      Category = 'Debug'
      Caption = 'Locals'
      OnExecute = actLocalsExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actGlobals: TAction
      Category = 'Debug'
      Caption = 'Globals'
      OnExecute = actGlobalsExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actParameters: TAction
      Category = 'Debug'
      Caption = 'Function Parameters'
      OnExecute = actParametersExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actRunCustomCommand: TAction
      Category = 'Debug'
      Caption = 'Custom Command'
    end
    object actChooseCustomCommand: TAction
      Category = 'Debug'
      Caption = '>'
      OnExecute = actChooseCustomCommandExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actMsgDisplayGDBCommands: TAction
      Category = 'Messages'
      Caption = 'Display GDB Commands'
      OnExecute = actMsgDisplayGDBCommandsExecute
      OnUpdate = actMsgDisplayGDBCommandsUpdate
    end
    object actMsgDisplayGDBAnnotations: TAction
      Category = 'Messages'
      Caption = 'Display GDB Annotations'
      OnExecute = actMsgDisplayGDBAnnotationsExecute
      OnUpdate = actMsgDisplayGDBAnnotationsUpdate
    end
  end
  object MessagePopup: TPopupMenu
    Left = 339
    Top = 291
    object actMsgCut1: TMenuItem
      Action = actMsgCut
    end
    object MsgCopyItem: TMenuItem
      Action = actMsgCopy
    end
    object MsgCopyAllItem: TMenuItem
      Action = actMsgCopyAll
    end
    object MsgPasteItem: TMenuItem
      Action = actMsgPaste
    end
    object N74: TMenuItem
      Caption = '-'
    end
    object MsgSellAllItem: TMenuItem
      Action = actMsgSelAll
    end
    object N71: TMenuItem
      Caption = '-'
    end
    object MsgSaveAllItem: TMenuItem
      Action = actMsgSaveAll
    end
    object N73: TMenuItem
      Caption = '-'
    end
    object MsgClearItem: TMenuItem
      Action = actMsgClear
    end
  end
  object CppParser: TCppParser
    Enabled = True
    OnTotalProgress = CppParserTotalProgress
    ParseLocalHeaders = False
    ParseGlobalHeaders = False
    OnStartParsing = CppParserStartParsing
    OnEndParsing = CppParserEndParsing
    Left = 60
    Top = 196
  end
  object CodeCompletion: TCodeCompletion
    ShowCount = 100
    Parser = CppParser
    Color = clWhite
    Width = 320
    Height = 240
    Enabled = True
    MinWidth = 256
    MinHeight = 128
    MaxWidth = 0
    MaxHeight = 0
    OnResize = CodeCompletionResize
    OnlyGlobals = False
    Left = 60
    Top = 228
  end
  object Shortcuts: TdevShortcuts
    Filename = 'shortcuts.ini'
    Left = 628
    Top = 280
  end
  object BrowserPopup: TPopupMenu
    Images = dmMain.MenuImages_NewLook
    Left = 56
    Top = 100
    object mnuBrowserGotoDecl: TMenuItem
      Action = actBrowserGotoDeclaration
    end
    object mnuBrowserGotoImpl: TMenuItem
      Action = actBrowserGotoDefinition
      Default = True
    end
    object mnuBrowserSep1: TMenuItem
      Caption = '-'
    end
    object mnuBrowserNewClass: TMenuItem
      Action = actBrowserNewClass
    end
    object mnuBrowserNewMember: TMenuItem
      Action = actBrowserNewMember
    end
    object mnuBrowserNewVariable: TMenuItem
      Action = actBrowserNewVar
    end
    object N31: TMenuItem
      Caption = '-'
    end
    object mnuBrowserViewMode: TMenuItem
      Caption = 'View mode'
      object mnuBrowserViewAll: TMenuItem
        Action = actBrowserViewAll
        RadioItem = True
      end
      object mnuBrowserViewProject: TMenuItem
        Action = actBrowserViewProject
        RadioItem = True
      end
      object mnuBrowserViewCurrent: TMenuItem
        Action = actBrowserViewCurrent
        RadioItem = True
      end
      object mnuBrowserViewInclude: TMenuItem
        Action = actBrowserViewIncludes
        RadioItem = True
      end
      object N42: TMenuItem
        Caption = '-'
      end
      object Showinheritedmembers1: TMenuItem
        Action = actBrowserShowInherited
      end
    end
  end
  object DebugPopup: TPopupMenu
    Images = dmMain.MenuImages_NewLook
    OnPopup = DebugPopupPopup
    Left = 104
    Top = 100
    object AddwatchPop: TMenuItem
      Action = actAddWatch
    end
    object ModifyWatchPop: TMenuItem
      Action = actModifyWatch
    end
    object N67: TMenuItem
      Caption = '-'
    end
    object RemoveWatchPop: TMenuItem
      Action = actRemoveWatch
    end
    object ClearallWatchPop: TMenuItem
      Caption = '&Clear all'
      OnClick = ClearallWatchPopClick
    end
  end
  object DevCppDDEServer: TDdeServerConv
    OnExecuteMacro = DevCppDDEServerExecuteMacro
    Left = 628
    Top = 240
  end
  object CppPreprocessor: TCppPreprocessor
    Left = 60
    Top = 134
  end
  object CppTokenizer: TCppTokenizer
    Left = 60
    Top = 166
  end
  object FolderPopup: TPopupMenu
    Images = dmMain.MenuImages_NewLook
    Left = 12
    Top = 168
    object Addfolder2: TMenuItem
      Action = actProjectNewFolder
    end
    object Renamefolder2: TMenuItem
      Action = actProjectRenameFolder
    end
    object Removefolder2: TMenuItem
      Action = actProjectRemoveFolder
    end
    object N44: TMenuItem
      Caption = '-'
    end
    object SourceFile1: TMenuItem
      Action = actProjectNew
    end
    object Addfile1: TMenuItem
      Action = actProjectAdd
    end
  end
  object CustomDebugPopup: TPopupMenu
    Left = 329
    Top = 176
    object miRuntoCursor: TMenuItem
      Action = actRunToCursor
    end
    object N50: TMenuItem
      Caption = '-'
    end
    object Locals1: TMenuItem
      Action = actLocals
    end
    object FunctionParameters1: TMenuItem
      Action = actParameters
    end
    object Globals1: TMenuItem
      Action = actGlobals
    end
    object miCallStack: TMenuItem
      Action = actCallStack
    end
    object miCallStackFull: TMenuItem
      Action = actCallStackFull
    end
  end
  object DebugOutputPopup: TPopupMenu
    Left = 443
    Top = 291
    object MenuItem3: TMenuItem
      Action = actMsgCut
    end
    object MenuItem4: TMenuItem
      Action = actMsgCopy
    end
    object MenuItem5: TMenuItem
      Action = actMsgCopyAll
    end
    object MenuItem6: TMenuItem
      Action = actMsgPaste
    end
    object MenuItem7: TMenuItem
      Caption = '-'
    end
    object MenuItem8: TMenuItem
      Action = actMsgSelAll
    end
    object MenuItem9: TMenuItem
      Caption = '-'
    end
    object MenuItem10: TMenuItem
      Action = actMsgSaveAll
    end
    object MenuItem11: TMenuItem
      Caption = '-'
    end
    object DisplayGDBCommandsBtn: TMenuItem
      Action = actMsgDisplayGDBCommands
    end
    object DisplayGDBAnnotationsBtn: TMenuItem
      Action = actMsgDisplayGDBAnnotations
    end
    object N51: TMenuItem
      Caption = '-'
    end
    object MenuItem12: TMenuItem
      Action = actMsgClear
    end
  end
end
