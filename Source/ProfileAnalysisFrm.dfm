object ProfileAnalysisForm: TProfileAnalysisForm
  Left = 992
  Top = 474
  Width = 649
  Height = 531
  Caption = 'Profile analysis'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 631
    Height = 484
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Parsing profiling results - Please wait...'
    TabOrder = 0
    object ProfilePageControl: TPageControl
      Left = 0
      Top = 0
      Width = 631
      Height = 484
      ActivePage = tabFlat
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = ProfilePageControlChange
      object tabFlat: TTabSheet
        Caption = 'Flat output'
        object Splitter2: TSplitter
          Left = 0
          Top = 277
          Width = 623
          Height = 9
          Cursor = crVSplit
          Align = alBottom
        end
        object memFlat: TMemo
          Left = 0
          Top = 286
          Width = 623
          Height = 163
          Align = alBottom
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object lvFlat: TListView
          Left = 0
          Top = 0
          Width = 623
          Height = 277
          Align = alClient
          Columns = <
            item
              Caption = 'Function name'
              Width = 246
            end
            item
              Alignment = taRightJustify
              Caption = '% time'
              Width = 62
            end
            item
              Alignment = taRightJustify
              Caption = 'Cumul. secs'
              Width = 92
            end
            item
              Alignment = taRightJustify
              Caption = 'Self secs'
              Width = 92
            end
            item
              Alignment = taRightJustify
              Caption = 'Calls'
              Width = 74
            end
            item
              Alignment = taRightJustify
              Caption = 'Self ts/call'
              Width = 92
            end
            item
              Alignment = taRightJustify
              Caption = 'Total ts/call'
              Width = 92
            end>
          GridLines = True
          ReadOnly = True
          RowSelect = True
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          ViewStyle = vsReport
          OnAdvancedCustomDraw = lvFlatAdvancedCustomDraw
          OnClick = lvFlatClick
          OnCustomDrawItem = lvFlatCustomDrawItem
          OnMouseMove = lvFlatMouseMove
        end
      end
      object tabGraph: TTabSheet
        Caption = 'Call graph'
        ImageIndex = 1
        object Splitter1: TSplitter
          Left = 0
          Top = 398
          Width = 769
          Height = 9
          Cursor = crVSplit
          Align = alBottom
        end
        object memGraph: TMemo
          Left = 0
          Top = 407
          Width = 769
          Height = 163
          Align = alBottom
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object lvGraph: TListView
          Left = 0
          Top = 0
          Width = 769
          Height = 398
          Align = alClient
          Columns = <
            item
              Caption = 'Function name'
              Width = 308
            end
            item
              Alignment = taRightJustify
              Caption = 'Index'
              Width = 62
            end
            item
              Alignment = taRightJustify
              Caption = '% time'
              Width = 92
            end
            item
              Alignment = taRightJustify
              Caption = 'Self'
              Width = 92
            end
            item
              Alignment = taRightJustify
              Caption = 'Children'
              Width = 92
            end
            item
              Alignment = taRightJustify
              Caption = 'Called'
              Width = 92
            end>
          GridLines = True
          ReadOnly = True
          RowSelect = True
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          ViewStyle = vsReport
          OnAdvancedCustomDraw = lvGraphAdvancedCustomDraw
          OnClick = lvFlatClick
          OnCustomDrawItem = lvGraphCustomDrawItem
          OnMouseMove = lvFlatMouseMove
        end
      end
      object tabOpts: TTabSheet
        Caption = 'Profiling Options'
        ImageIndex = 2
        object FuncHiding: TGroupBox
          Left = 10
          Top = 20
          Width = 346
          Height = 129
          Caption = ' Function Hiding '
          TabOrder = 0
          object PrevText: TLabel
            Left = 20
            Top = 96
            Width = 219
            Height = 20
            Caption = 'Supress functions called less than'
          end
          object PostText: TLabel
            Left = 295
            Top = 96
            Width = 36
            Height = 20
            Caption = 'times'
          end
          object chkHideNotCalled: TCheckBox
            Left = 20
            Top = 30
            Width = 316
            Height = 20
            Caption = 'Hide functions not called long enough'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = commandUpdate
          end
          object chkSuppressStatic: TCheckBox
            Left = 20
            Top = 59
            Width = 316
            Height = 21
            Caption = 'Suppress statically declared (private) functions'
            TabOrder = 1
            OnClick = commandUpdate
          end
          object spnMinCount: TSpinEdit
            Left = 226
            Top = 92
            Width = 61
            Height = 24
            MaxValue = 999999999
            MinValue = 0
            TabOrder = 2
            Value = 1
            OnChange = commandUpdate
          end
        end
        object btnApply: TButton
          Left = 256
          Top = 158
          Width = 92
          Height = 30
          Caption = 'Apply'
          TabOrder = 1
          OnClick = btnApplyClick
        end
        object CustomCommands: TGroupBox
          Left = 364
          Top = 20
          Width = 395
          Height = 99
          Caption = ' Custom Commands '
          TabOrder = 2
          object chkCustom: TCheckBox
            Left = 20
            Top = 30
            Width = 306
            Height = 20
            Caption = 'Use these commands instead:'
            TabOrder = 0
            OnClick = chkCustomClick
          end
          object editCustom: TEdit
            Left = 20
            Top = 59
            Width = 355
            Height = 23
            Enabled = False
            TabOrder = 1
            Text = 'editCustom'
          end
        end
      end
    end
  end
end
