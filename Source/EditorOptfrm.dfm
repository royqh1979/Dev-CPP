object EditorOptForm: TEditorOptForm
  Left = 390
  Top = 121
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 656
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    645
    656)
  PixelsPerInch = 120
  TextHeight = 20
  object PagesMain: TPageControl
    Left = 0
    Top = 0
    Width = 647
    Height = 607
    ActivePage = tabCBCompletion
    TabOrder = 0
    OnChange = PagesMainChange
    object tabGeneral: TTabSheet
      Caption = 'General'
      object grpMargin: TGroupBox
        Left = 352
        Top = 293
        Width = 268
        Height = 120
        Caption = '  Right Margin  '
        TabOrder = 2
        object lblMarginWidth: TLabel
          Left = 11
          Top = 57
          Width = 40
          Height = 20
          Caption = 'Width'
        end
        object lblMarginColor: TLabel
          Left = 132
          Top = 57
          Width = 36
          Height = 20
          Caption = 'Color'
        end
        object cbMarginVis: TCheckBox
          Left = 11
          Top = 27
          Width = 246
          Height = 22
          Caption = 'Visible'
          TabOrder = 0
        end
        object edMarginWidth: TSpinEdit
          Left = 21
          Top = 79
          Width = 80
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object cpMarginColor: TColorBox
          Left = 128
          Top = 77
          Width = 129
          Height = 22
          DefaultColorColor = cl3DLight
          Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 2
        end
      end
      object grpEditorOpts: TGroupBox
        Left = 12
        Top = 4
        Width = 608
        Height = 285
        Caption = '  Editor Options  '
        TabOrder = 0
        object cbFunctionHint: TCheckBox
          Left = 321
          Top = 221
          Width = 278
          Height = 23
          Caption = 'Show function hints'
          TabOrder = 15
        end
        object cbTrimTrailingSpaces: TCheckBox
          Left = 11
          Top = 221
          Width = 278
          Height = 23
          Caption = 'Trim Trailing Spaces'
          TabOrder = 7
        end
        object cbAutoIndent: TCheckBox
          Left = 11
          Top = 35
          Width = 278
          Height = 22
          Caption = 'Auto Indent'
          TabOrder = 0
        end
        object cbAddIndent: TCheckBox
          Left = 11
          Top = 61
          Width = 278
          Height = 23
          Caption = 'Add indent to {} and :'
          TabOrder = 2
        end
        object cbDropFiles: TCheckBox
          Left = 11
          Top = 168
          Width = 278
          Height = 23
          Caption = 'Insert Dropped Files'
          TabOrder = 5
        end
        object cbEHomeKey: TCheckBox
          Left = 321
          Top = 35
          Width = 278
          Height = 22
          Caption = 'Enhance home key'
          TabOrder = 8
        end
        object cbInsertMode: TCheckBox
          Left = 11
          Top = 88
          Width = 278
          Height = 23
          Caption = 'Insert Mode'
          TabOrder = 1
        end
        object cbParserHints: TCheckBox
          Left = 321
          Top = 195
          Width = 278
          Height = 22
          Caption = 'Show editor hints'
          TabOrder = 14
        end
        object cbHalfPage: TCheckBox
          Left = 321
          Top = 141
          Width = 278
          Height = 23
          Caption = 'Half Page Scrolling'
          TabOrder = 12
        end
        object cbGroupUndo: TCheckBox
          Left = 11
          Top = 141
          Width = 278
          Height = 23
          Caption = 'Group Undo'
          TabOrder = 4
        end
        object cbFindText: TCheckBox
          Left = 11
          Top = 115
          Width = 278
          Height = 22
          Caption = 'Find Text at Cursor'
          TabOrder = 3
        end
        object cbPastEOL: TCheckBox
          Left = 321
          Top = 88
          Width = 278
          Height = 23
          Caption = 'Cursor Past EOL'
          TabOrder = 10
        end
        object cbPastEOF: TCheckBox
          Left = 321
          Top = 61
          Width = 278
          Height = 23
          Caption = 'Cursor Past EOF'
          TabOrder = 9
        end
        object cbScrollHint: TCheckBox
          Left = 321
          Top = 168
          Width = 278
          Height = 23
          Caption = 'Scroll Hint'
          TabOrder = 13
        end
        object cbSmartScroll: TCheckBox
          Left = 321
          Top = 115
          Width = 278
          Height = 22
          Caption = 'Scollbars on need'
          TabOrder = 11
        end
        object cbSpecialChars: TCheckBox
          Left = 11
          Top = 195
          Width = 278
          Height = 22
          Caption = 'Show Special Line Chars'
          TabOrder = 6
        end
        object cbUseUTF8AsDefault: TCheckBox
          Left = 11
          Top = 253
          Width = 278
          Height = 23
          Caption = 'Use UTF-8 As the default charset'
          TabOrder = 16
        end
      end
      object grpCaret: TGroupBox
        Left = 12
        Top = 293
        Width = 331
        Height = 120
        Caption = '  Caret  '
        TabOrder = 1
        object lblInsertCaret: TLabel
          Left = 11
          Top = 21
          Width = 76
          Height = 20
          Caption = 'Insert caret:'
        end
        object lblOverCaret: TLabel
          Left = 11
          Top = 52
          Width = 104
          Height = 20
          Caption = 'Overwrite caret:'
        end
        object cboInsertCaret: TComboBox
          Left = 181
          Top = 16
          Width = 134
          Height = 28
          Style = csDropDownList
          ItemHeight = 20
          TabOrder = 0
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cboOverwriteCaret: TComboBox
          Left = 181
          Top = 48
          Width = 134
          Height = 28
          Style = csDropDownList
          ItemHeight = 20
          TabOrder = 1
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cbMatch: TCheckBox
          Left = 11
          Top = 84
          Width = 310
          Height = 23
          Caption = 'Highlight matching symbols'
          TabOrder = 2
        end
      end
      object grpHighCurLine: TGroupBox
        Left = 352
        Top = 443
        Width = 268
        Height = 112
        Caption = 'Highlight current line'
        TabOrder = 4
        object cbHighlightColor: TLabel
          Left = 132
          Top = 47
          Width = 36
          Height = 20
          Caption = 'Color'
        end
        object cbHighCurrLine: TCheckBox
          Left = 11
          Top = 27
          Width = 246
          Height = 22
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = cbHighCurrLineClick
        end
        object cpHighColor: TColorBox
          Left = 128
          Top = 67
          Width = 129
          Height = 22
          DefaultColorColor = 16777164
          Style = [cbStandardColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 1
        end
      end
      object grpTabs: TGroupBox
        Left = 12
        Top = 443
        Width = 331
        Height = 112
        Caption = '  Tabs  '
        TabOrder = 3
        object lblTabSize: TLabel
          Left = 235
          Top = 43
          Width = 57
          Height = 20
          Caption = 'Tab Size:'
        end
        object seTabSize: TSpinEdit
          Left = 235
          Top = 69
          Width = 76
          Height = 24
          MaxValue = 64
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object cbUseTabs: TCheckBox
          Left = 11
          Top = 27
          Width = 225
          Height = 22
          Caption = 'Use Tab Character'
          TabOrder = 0
        end
        object cbSmartTabs: TCheckBox
          Left = 11
          Top = 53
          Width = 214
          Height = 23
          Caption = 'Smart Tabs'
          TabOrder = 1
        end
      end
    end
    object tabDisplay: TTabSheet
      Caption = 'Fonts'
      object ScrollHint: TLabel
        Left = 0
        Top = 509
        Width = 631
        Height = 44
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'It is also possible to edit text size by using Control+Scroll, j' +
          'ust like in browsers!'
      end
      object grpGutter: TGroupBox
        Left = 12
        Top = 213
        Width = 608
        Height = 247
        Caption = '  Gutter  '
        TabOrder = 1
        DesignSize = (
          608
          247)
        object lblGutterFont: TLabel
          Left = 11
          Top = 121
          Width = 32
          Height = 20
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Font:'
        end
        object lblGutterWidth: TLabel
          Left = 480
          Top = 44
          Width = 119
          Height = 19
          Anchors = [akLeft, akRight, akBottom]
          AutoSize = False
          Caption = 'Gutter Width'
          WordWrap = True
        end
        object lblGutterFontSize: TLabel
          Left = 480
          Top = 121
          Width = 27
          Height = 20
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Size'
        end
        object cbLeadZero: TCheckBox
          Left = 233
          Top = 88
          Width = 216
          Height = 20
          Caption = 'Show Leading Zeros'
          TabOrder = 5
        end
        object cbFirstZero: TCheckBox
          Left = 233
          Top = 61
          Width = 216
          Height = 20
          Caption = 'Start at Zero'
          TabOrder = 4
        end
        object cbLineNum: TCheckBox
          Left = 233
          Top = 35
          Width = 216
          Height = 20
          Caption = 'Show Line Numbers'
          TabOrder = 3
          OnClick = cbLineNumClick
        end
        object cbGutterVis: TCheckBox
          Left = 11
          Top = 35
          Width = 193
          Height = 20
          Caption = 'Visible'
          TabOrder = 0
        end
        object cbGutterAuto: TCheckBox
          Left = 11
          Top = 61
          Width = 193
          Height = 20
          Caption = 'Auto Size'
          TabOrder = 1
        end
        object cbGutterFnt: TCheckBox
          Left = 11
          Top = 88
          Width = 193
          Height = 20
          Caption = 'Use Custom Font'
          TabOrder = 2
          OnClick = cbGutterFntClick
        end
        object cboGutterFont: TComboBox
          Left = 16
          Top = 143
          Width = 455
          Height = 66
          AutoComplete = False
          Style = csOwnerDrawVariable
          ItemHeight = 60
          Sorted = True
          TabOrder = 6
          OnDrawItem = cboGutterFontDrawItem
        end
        object edGutterSize: TSpinEdit
          Left = 480
          Top = 143
          Width = 107
          Height = 24
          MaxValue = 999
          MinValue = 1
          TabOrder = 7
          Value = 10
          OnChange = edGutterSizeChange
        end
        object edGutterWidth: TSpinEdit
          Left = 480
          Top = 65
          Width = 107
          Height = 24
          MaxValue = 999
          MinValue = 1
          TabOrder = 8
          Value = 32
        end
      end
      object grpEditorFont: TGroupBox
        Left = 12
        Top = 4
        Width = 608
        Height = 145
        Caption = '  Editor Font  '
        TabOrder = 0
        object lblEditorSize: TLabel
          Left = 480
          Top = 21
          Width = 30
          Height = 20
          Caption = 'Size:'
        end
        object lblEditorFont: TLabel
          Left = 11
          Top = 21
          Width = 32
          Height = 20
          Caption = 'Font:'
        end
        object cboEditorFont: TComboBox
          Left = 16
          Top = 43
          Width = 455
          Height = 66
          AutoComplete = False
          Style = csOwnerDrawVariable
          ItemHeight = 60
          Sorted = True
          TabOrder = 0
          OnChange = cboEditorFontChange
          OnDrawItem = cboEditorFontDrawItem
        end
        object edEditorSize: TSpinEdit
          Left = 480
          Top = 43
          Width = 107
          Height = 24
          MaxValue = 999
          MinValue = 1
          TabOrder = 1
          Value = 10
          OnChange = edEditorSizeChange
        end
      end
    end
    object tabSyntax: TTabSheet
      Caption = 'Colors'
      object lblForeground: TLabel
        Left = 12
        Top = 243
        Width = 80
        Height = 20
        Caption = 'Foreground:'
      end
      object lblBackground: TLabel
        Left = 12
        Top = 304
        Width = 82
        Height = 20
        Caption = 'Background:'
      end
      object lblSpeed: TLabel
        Left = 12
        Top = 488
        Width = 136
        Height = 20
        Caption = 'Color Speed Setting:'
      end
      object btnSaveSyntax: TSpeedButton
        Left = 11
        Top = 515
        Width = 30
        Height = 30
        Hint = 'Save color theme'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000CE0E0000D80E0000000000000000000000FF0000FF00
          00FF0000FF0000FF0000000000000000000000000000FF0000FF0000FF0000FF
          0000FF0000FF0000FF0000FF0000FF0000FF0000FF00000000666148A89F77DD
          DDDD9B9A8F00000000000000FF0000FF0000FF0000FF0000FF0000FF0000FF00
          00FF0000FF00000000A89F77A89F77E9E9E9B2B0A7D5D4D29392894848480000
          0000FF0000FF0000FF0000FF0000FF0000FF00000000B9B294A89F77756E534A
          473AACA47EDADAD5E0E0E0B4B4B476736500000000FF0000FF0000FF0000FF00
          00FF00000000A89F77A89F77756E53F3F3F3F1F1F1E7E7E7E1E1E1B4B2A96661
          48635E464A463400000000FF0000FF00000000B2AA87F0EFE8EBE9E0A89F7763
          5E46ADABA4EAEAEAE4E4E4646360A89F77A89F7700000000000000FF0000FF00
          000000EBE9E0FFFFFFFFFFFFF5F4F0A89F77A89F77635E465A574B787255A89F
          77A89F7700000000FF0000FF00000000B2AA87F2F1EBFFFFFFFFFFFFFFFFFFF5
          F4F0EBE9E0A89F77A89F77756E53756E5300000000000000FF0000FF00000000
          EBE9E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F6F2EBE9E0A89F77A89F
          7700000000FF0000FF000000008D8A78F2F1EBFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFF0EFE800000000000000FF0000FF00000000B9B294
          DFDCCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEEECE40000
          0000FF0000FF0000FF0000FF00000000000000C9C4AED5D1BFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF9F8F500000000000000FF0000FF0000FF0000FF0000FF00
          00FF00000000000000C9C4AED5D1BFD5D1BFFFFFFFF4F3EEA89F7700000000FF
          0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF00000000000000D5
          D1BFC5C1A8EDECE400000000000000FF0000FF0000FF0000FF0000FF0000FF00
          00FF0000FF0000FF0000FF0000FF00000000000000A89F7700000000FF0000FF
          0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
          FF0000FF0000000000000000FF0000FF0000FF0000FF0000FF00}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnSaveSyntaxClick
      end
      object CppEdit: TSynEdit
        Left = 192
        Top = 9
        Width = 439
        Height = 458
        Align = alCustom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -18
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 3
        Gutter.AutoSize = True
        Gutter.BorderStyle = gbsNone
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -12
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 4
        Gutter.RightOffset = 21
        Gutter.ShowLineNumbers = True
        Gutter.Width = 32
        Gutter.GradientEndColor = clBackground
        HideSelection = True
        Highlighter = cpp
        UseCodeFolding = True
        Lines.Strings = (
          '#include <iostream>'
          '#include <conio.h>'
          ''
          'int main(int argc, char **argv)'
          '{'
          #9'int numbers[20];'
          #9'float average, total; //breakpoint'
          #9'for (int i = 0; i <= 19; i++)'
          #9'{ // active breakpoint'
          #9#9'numbers[i] = i;'
          #9#9'Total += i; // error line'
          #9'}'
          #9'average = total / 20; // comment'
          #9'cout << "total: " << total << "\nAverage: " << average;'
          #9'getch();'
          '}')
        Options = [eoAutoIndent, eoDisableScrollArrows, eoHideShowScrollbars, eoNoCaret, eoNoSelection, eoSmartTabs, eoTrimTrailingSpaces]
        ReadOnly = True
        RightEdge = 0
        ScrollHintFormat = shfTopToBottom
        TabWidth = 4
        WantTabs = True
        OnGutterClick = OnGutterClick
        OnSpecialLineColors = CppEditSpecialLineColors
        OnStatusChange = cppEditStatusChange
        FontSmoothing = fsmNone
        RemovedKeystrokes = <
          item
            Command = ecDeleteLastChar
            ShortCut = 8200
          end
          item
            Command = ecLineBreak
            ShortCut = 8205
          end
          item
            Command = ecContextHelp
            ShortCut = 112
          end>
        AddedKeystrokes = <>
      end
      object ElementList: TListBox
        Left = 9
        Top = 9
        Width = 174
        Height = 204
        ImeName = 'CN'#177'??i(CN'#177'U)'
        IntegralHeight = True
        ItemHeight = 20
        Items.Strings = (
          'Comment'
          'Identifier'
          'Keyword'
          'Number'
          'Background'
          'String'
          'Symbol'
          'WhiteSpace'
          'Directives')
        TabOrder = 0
        OnClick = ElementListClick
      end
      object grpStyle: TGroupBox
        Left = 20
        Top = 367
        Width = 147
        Height = 97
        Caption = '  Style:  '
        TabOrder = 2
        object cbBold: TCheckBox
          Left = 11
          Top = 20
          Width = 133
          Height = 23
          Caption = 'Bold'
          TabOrder = 0
          OnClick = StyleChange
        end
        object cbItalic: TCheckBox
          Left = 11
          Top = 43
          Width = 133
          Height = 22
          Caption = 'Italic'
          TabOrder = 1
          OnClick = StyleChange
        end
        object cbUnderlined: TCheckBox
          Left = 11
          Top = 67
          Width = 133
          Height = 22
          Caption = 'Underlined'
          TabOrder = 2
          OnClick = StyleChange
        end
      end
      object cboQuickColor: TComboBox
        Left = 43
        Top = 515
        Width = 173
        Height = 28
        Style = csDropDownList
        ItemHeight = 20
        TabOrder = 1
        OnSelect = cboQuickColorSelect
        Items.Strings = (
          'Classic'
          'Classic Plus'
          'Twilight'
          'Ocean'
          'Visual Studio'
          'Borland'
          'Matrix'
          'Obsidian'
          'GSS Hacker'
          'Obvilion'
          'PlasticCodeWrap')
      end
      object edSyntaxExt: TEdit
        Left = 240
        Top = 515
        Width = 380
        Height = 28
        TabOrder = 4
      end
      object cbSyntaxHighlight: TCheckBox
        Left = 233
        Top = 480
        Width = 400
        Height = 23
        Caption = 'Use Syntax Highlighting'
        TabOrder = 5
        OnClick = cbSyntaxHighlightClick
      end
      object cpForeground: TColorBox
        Left = 23
        Top = 269
        Width = 146
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 6
        OnChange = StyleChange
      end
      object cpBackground: TColorBox
        Left = 23
        Top = 328
        Width = 146
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 7
        OnChange = StyleChange
      end
    end
    object tabCode: TTabSheet
      Caption = 'Snippets'
      object PagesSnippets: TPageControl
        Left = 0
        Top = 0
        Width = 639
        Height = 572
        ActivePage = tabCPInserts
        Align = alClient
        TabOrder = 0
        object tabCPInserts: TTabSheet
          Caption = 'Inserts'
          object btnAdd: TButton
            Left = 512
            Top = 60
            Width = 104
            Height = 32
            Caption = 'Add'
            TabOrder = 0
            OnClick = btnAddClick
          end
          object btnRemove: TButton
            Left = 509
            Top = 103
            Width = 107
            Height = 32
            Caption = 'Remove'
            TabOrder = 1
            OnClick = btnRemoveClick
          end
          object CodeIns: TSynEdit
            Left = 0
            Top = 245
            Width = 625
            Height = 282
            Align = alBottom
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -18
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 2
            TabStop = False
            Gutter.AutoSize = True
            Gutter.BorderStyle = gbsNone
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -12
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.LeftOffset = 4
            Gutter.RightOffset = 21
            Gutter.ShowLineNumbers = True
            Highlighter = cpp
            UseCodeFolding = True
            Options = [eoAutoIndent, eoHideShowScrollbars, eoKeepCaretX, eoSmartTabs, eoTabIndent, eoTrimTrailingSpaces]
            TabWidth = 4
            WantTabs = True
            OnStatusChange = CodeInsStatusChange
            FontSmoothing = fsmNone
            RemovedKeystrokes = <
              item
                Command = ecContextHelp
                ShortCut = 112
              end>
            AddedKeystrokes = <
              item
                Command = ecContextHelp
                ShortCut = 16496
              end>
          end
          object lvCodeIns: TStringGrid
            Left = 11
            Top = 11
            Width = 492
            Height = 182
            ColCount = 3
            DefaultColWidth = 115
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
            ScrollBars = ssVertical
            TabOrder = 3
            OnSelectCell = lvCodeInsSelectCell
          end
        end
        object tabCPDefault: TTabSheet
          Caption = 'Default Insert'
          object cbDefaultCode: TCheckBox
            Left = 5
            Top = 3
            Width = 615
            Height = 22
            Caption = 'Insert Default Code into Empty Projects'
            TabOrder = 0
          end
          object seDefault: TSynEdit
            Left = 0
            Top = 32
            Width = 624
            Height = 441
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -18
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 1
            TabStop = False
            Gutter.AutoSize = True
            Gutter.BorderStyle = gbsNone
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -12
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.LeftOffset = 4
            Gutter.RightOffset = 21
            Gutter.ShowLineNumbers = True
            Highlighter = cpp
            Options = [eoAutoIndent, eoHideShowScrollbars, eoKeepCaretX, eoSmartTabs, eoTabIndent, eoTrimTrailingSpaces]
            TabWidth = 4
            WantTabs = True
            OnStatusChange = CodeInsStatusChange
            FontSmoothing = fsmNone
            RemovedKeystrokes = <
              item
                Command = ecContextHelp
                ShortCut = 112
              end>
            AddedKeystrokes = <
              item
                Command = ecContextHelp
                ShortCut = 16496
              end>
          end
        end
      end
    end
    object tabCBCompletion: TTabSheet
      Caption = 'Completion'
      object PagesCompletion: TPageControl
        Left = 0
        Top = 0
        Width = 639
        Height = 572
        ActivePage = tabCodeCompletion
        Align = alClient
        TabOrder = 0
        object tabCodeCompletion: TTabSheet
          Caption = 'Code Completion'
          ImageIndex = 1
          object lblCompletionColor: TLabel
            Left = 437
            Top = 144
            Width = 120
            Height = 20
            Caption = 'Background color:'
          end
          object lblCompletionDelay: TLabel
            Left = 11
            Top = 144
            Width = 74
            Height = 20
            Caption = 'Delay (ms):'
          end
          object chkEnableCompletion: TCheckBox
            Left = 11
            Top = 11
            Width = 609
            Height = 22
            Caption = 'Enable code-completion'
            TabOrder = 0
            OnClick = chkEnableCompletionClick
          end
          object cpCompletionBackground: TColorBox
            Left = 444
            Top = 171
            Width = 149
            Height = 22
            DefaultColorColor = clWhite
            Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 1
          end
          object gbCBEngine: TGroupBox
            Left = 11
            Top = 48
            Width = 609
            Height = 87
            Caption = 'Engine behaviour'
            TabOrder = 2
            object chkCBParseLocalH: TCheckBox
              Left = 11
              Top = 27
              Width = 460
              Height = 22
              Caption = 'Scan local files referenced in #include'#39's'
              TabOrder = 0
            end
            object chkCBParseGlobalH: TCheckBox
              Left = 11
              Top = 53
              Width = 460
              Height = 23
              Caption = 'Scan global files referenced in #include'#39's'
              TabOrder = 1
            end
          end
          object tbCompletionDelay: TTrackBar
            Left = 21
            Top = 176
            Width = 396
            Height = 49
            Max = 2000
            Min = 1
            ParentShowHint = False
            Frequency = 50
            Position = 1000
            ShowHint = False
            TabOrder = 3
            TickMarks = tmBoth
            OnChange = tbCompletionDelayChange
          end
          object cbUseAltSlash: TCheckBox
            Left = 11
            Top = 235
            Width = 609
            Height = 22
            Caption = 'Use Alt+/ instead of Ctrl+Space'
            TabOrder = 4
          end
          object cbShowCompletionWhileInputing: TCheckBox
            Left = 11
            Top = 267
            Width = 609
            Height = 22
            Caption = 'Code Completion while inputing'
            TabOrder = 5
          end
        end
        object tabSymbolCompletion: TTabSheet
          Caption = 'Symbol Completion'
          ImageIndex = 2
          object grpSpecific: TGroupBox
            Left = 21
            Top = 53
            Width = 364
            Height = 226
            Caption = 'Specific completion options'
            TabOrder = 1
            object cbParenth: TCheckBox
              Left = 21
              Top = 64
              Width = 320
              Height = 23
              Caption = 'Complete parentheses '#39'()'#39
              TabOrder = 1
            end
            object cbBraces: TCheckBox
              Left = 21
              Top = 32
              Width = 320
              Height = 23
              Caption = 'Complete braces '#39'{}'#39
              TabOrder = 0
            end
            object cbComments: TCheckBox
              Left = 21
              Top = 128
              Width = 320
              Height = 23
              Caption = 'Complete multiline comments '#39'/**/'#39
              TabOrder = 3
            end
            object cbArray: TCheckBox
              Left = 21
              Top = 96
              Width = 320
              Height = 23
              Caption = 'Complete square braces '#39'[]'#39
              TabOrder = 2
            end
            object cbSingleQuotes: TCheckBox
              Left = 21
              Top = 160
              Width = 320
              Height = 23
              Caption = 'Complete single quotes '#39#39#39#39
              TabOrder = 4
            end
            object cbDoubleQuotes: TCheckBox
              Left = 21
              Top = 192
              Width = 320
              Height = 23
              Caption = 'Complete double quotes '#39'""'#39
              TabOrder = 5
            end
          end
          object cbSymbolComplete: TCheckBox
            Left = 11
            Top = 11
            Width = 620
            Height = 22
            Caption = 'Enable symbol completion'
            TabOrder = 0
            OnClick = cbSymbolCompleteClick
          end
          object cbDeleteCompleted: TCheckBox
            Left = 11
            Top = 288
            Width = 598
            Height = 23
            Caption = 'Delete completed symbols as pairs'
            TabOrder = 2
          end
        end
      end
    end
    object tabAutosave: TTabSheet
      Caption = 'Autosave'
      ImageIndex = 5
      object cbAutoSave: TCheckBox
        Left = 11
        Top = 11
        Width = 609
        Height = 22
        Caption = 'Enable editor autosave'
        TabOrder = 0
        OnClick = cbAutoSaveClick
      end
      object OptionsGroup: TGroupBox
        Left = 11
        Top = 43
        Width = 609
        Height = 513
        Caption = ' Options '
        TabOrder = 1
        object SaveInterval: TLabel
          Left = 21
          Top = 48
          Width = 52
          Height = 20
          Caption = 'Interval:'
        end
        object lblTimeStampExample: TLabel
          Left = 32
          Top = 395
          Width = 60
          Height = 20
          Caption = 'Example:'
        end
        object MinutesDelay: TTrackBar
          Left = 192
          Top = 32
          Width = 396
          Height = 53
          Max = 60
          Min = 1
          PageSize = 1
          Position = 1
          TabOrder = 0
          TickMarks = tmBoth
          OnChange = MinutesDelayChange
        end
        object FileOptions: TRadioGroup
          Left = 21
          Top = 107
          Width = 319
          Height = 129
          Caption = 'Files'
          Items.Strings = (
            'Save only the currently visible file'
            'Save all open files after each interval'
            'Save all project files')
          TabOrder = 1
        end
        object NameOptions: TRadioGroup
          Left = 21
          Top = 251
          Width = 319
          Height = 129
          Caption = 'Filenames'
          Items.Strings = (
            'Overwrite file'
            'Append UNIX timestamp'
            'Append formatted timestamp')
          TabOrder = 2
          OnClick = NameOptionsClick
        end
      end
    end
  end
  object btnOk: TBitBtn
    Left = 280
    Top = 613
    Width = 113
    Height = 34
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object btnCancel: TBitBtn
    Left = 400
    Top = 613
    Width = 113
    Height = 34
    Anchors = [akRight, akBottom]
    TabOrder = 2
    OnClick = btnCancelClick
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 520
    Top = 613
    Width = 113
    Height = 34
    Anchors = [akRight, akBottom]
    Enabled = False
    TabOrder = 3
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object cpp: TSynCppSyn
    DefaultFilter = 'C++ Files (*.c,*.cpp,*.h,*.hpp)|*.c;*.cpp;*.h;*.hpp'
    Left = 5
    Top = 458
  end
end
