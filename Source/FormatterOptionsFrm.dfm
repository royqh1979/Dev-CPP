object FormatterOptionsForm: TFormatterOptionsForm
  Left = 698
  Top = 84
  BorderStyle = bsDialog
  Caption = 'Formatter Options'
  ClientHeight = 761
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    800
    761)
  PixelsPerInch = 120
  TextHeight = 20
  object lblPoweredBy: TLabel
    Left = 0
    Top = 727
    Width = 428
    Height = 20
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Powered by AStyle'
  end
  object lblPreview: TLabel
    Left = 11
    Top = 491
    Width = 54
    Height = 20
    Caption = 'Preview:'
  end
  object btnOk: TBitBtn
    Left = 440
    Top = 718
    Width = 113
    Height = 34
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
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
    Left = 560
    Top = 718
    Width = 113
    Height = 34
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object btnHelp: TBitBtn
    Left = 680
    Top = 718
    Width = 113
    Height = 34
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object synExample: TSynEdit
    Left = 11
    Top = 520
    Width = 778
    Height = 193
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.RightOffset = 21
    Lines.Strings = (
      'int Foo(bool isBar) {'
      #9'if(isBar) {'
      #9#9'bar();'
      #9#9'return 1;'
      #9'} else {'
      #9#9'return 0;'
      #9'}'
      '}')
    ReadOnly = True
    FontSmoothing = fsmNone
  end
  object grpOptions: TGroupBox
    Left = 11
    Top = 11
    Width = 778
    Height = 470
    Caption = 'Options'
    TabOrder = 4
    object lblBracketStyle: TLabel
      Left = 21
      Top = 32
      Width = 86
      Height = 20
      Caption = 'Bracket style:'
    end
    object lblIndentStyle: TLabel
      Left = 21
      Top = 75
      Width = 79
      Height = 20
      Caption = 'Indent style:'
    end
    object lblTabWidth: TLabel
      Left = 21
      Top = 117
      Width = 67
      Height = 20
      Caption = 'Tab width:'
    end
    object lblCommand: TLabel
      Left = 21
      Top = 365
      Width = 281
      Height = 20
      Caption = 'Final command (add customizations here):'
    end
    object bvCustom: TBevel
      Left = 27
      Top = 275
      Width = 725
      Height = 2
    end
    object lblPointerAlign: TLabel
      Left = 21
      Top = 200
      Width = 88
      Height = 20
      Caption = 'Align Pointer:'
    end
    object lblAlignReference: TLabel
      Left = 21
      Top = 240
      Width = 108
      Height = 20
      Caption = 'Align Reference:'
    end
    object lblCustomOption: TLabel
      Left = 21
      Top = 285
      Width = 103
      Height = 20
      Caption = 'Custom Option:'
    end
    object cmbBracketStyle: TComboBox
      Left = 139
      Top = 27
      Width = 193
      Height = 28
      Style = csDropDownList
      ItemHeight = 20
      ItemIndex = 0
      TabOrder = 0
      Text = '(do not modify)'
      OnChange = OptionChange
      Items.Strings = (
        '(do not modify)'
        'Allman'
        'Java'
        'K&R'
        'Stroustrup'
        'Whitesmith'
        'Banner'
        'GNU'
        'Linux'
        'Horstmann'
        'OTBS'
        'Pico'
        'Lisp')
    end
    object cmbIndentStyle: TComboBox
      Left = 139
      Top = 69
      Width = 193
      Height = 28
      Style = csDropDownList
      ItemHeight = 20
      ItemIndex = 0
      TabOrder = 1
      Text = '(do not modify)'
      OnChange = OptionChange
      Items.Strings = (
        '(do not modify)'
        'Spaces'
        'Tabs'
        'Force Tab'
        'Force Tab X')
    end
    object spinTabWidth: TSpinEdit
      Left = 181
      Top = 112
      Width = 67
      Height = 31
      MaxValue = 20
      MinValue = 2
      TabOrder = 2
      Value = 4
      OnChange = OptionChange
    end
    object memFullCommand: TMemo
      Left = 21
      Top = 392
      Width = 736
      Height = 68
      ReadOnly = True
      TabOrder = 3
      OnChange = CommandChange
    end
    object spinMaxLineLength: TSpinEdit
      Left = 181
      Top = 155
      Width = 67
      Height = 31
      MaxValue = 200
      MinValue = 50
      TabOrder = 4
      Value = 80
      OnChange = OptionChange
    end
    object grpIndentParts: TGroupBox
      Left = 360
      Top = 16
      Width = 401
      Height = 137
      Caption = 'Indent the following kinds of code:'
      TabOrder = 5
      object chkClasses: TCheckBox
        Left = 13
        Top = 29
        Width = 172
        Height = 23
        Caption = 'Classes'
        TabOrder = 0
        OnClick = OptionChange
      end
      object chkSwitches: TCheckBox
        Left = 13
        Top = 63
        Width = 172
        Height = 23
        Caption = 'Switches'
        TabOrder = 1
        OnClick = OptionChange
      end
      object chkPreprocessor: TCheckBox
        Left = 205
        Top = 98
        Width = 172
        Height = 23
        Caption = 'Preprocessor'
        TabOrder = 2
        OnClick = OptionChange
      end
      object chkNamespace: TCheckBox
        Left = 205
        Top = 29
        Width = 172
        Height = 23
        Caption = 'Namespaces'
        TabOrder = 3
        OnClick = OptionChange
      end
      object chkLabels: TCheckBox
        Left = 205
        Top = 63
        Width = 172
        Height = 23
        Caption = 'Labels'
        TabOrder = 4
        OnClick = OptionChange
      end
      object chkCases: TCheckBox
        Left = 13
        Top = 98
        Width = 172
        Height = 23
        Caption = 'Cases'
        TabOrder = 5
        OnClick = OptionChange
      end
    end
    object chkMaxLineLength: TCheckBox
      Left = 21
      Top = 162
      Width = 150
      Height = 23
      Caption = 'Max line length:'
      TabOrder = 6
      OnClick = OptionChange
    end
    object chkPadOper: TCheckBox
      Left = 357
      Top = 162
      Width = 348
      Height = 23
      Caption = 'Add spaces around operators (+,-,...)'
      TabOrder = 7
      OnClick = OptionChange
    end
    object chkPadHeader: TCheckBox
      Left = 357
      Top = 202
      Width = 348
      Height = 23
      Caption = 'Add spaces after command (if, for,...)'
      TabOrder = 8
      OnClick = OptionChange
    end
    object cbAlignPointer: TComboBox
      Left = 139
      Top = 197
      Width = 193
      Height = 28
      Style = csDropDownList
      ItemHeight = 20
      TabOrder = 9
      OnChange = OptionChange
      Items.Strings = (
        'None'
        'Type'
        'Middle'
        'Name')
    end
    object cbAlignReference: TComboBox
      Left = 139
      Top = 237
      Width = 193
      Height = 28
      Style = csDropDownList
      ItemHeight = 20
      TabOrder = 10
      OnChange = OptionChange
      Items.Strings = (
        'None'
        'Type'
        'Middle'
        'Name')
    end
    object chkDeleteEmptyLines: TCheckBox
      Left = 357
      Top = 242
      Width = 164
      Height = 23
      Caption = 'Delete Empty Lines'
      TabOrder = 11
      OnClick = OptionChange
    end
    object chkDeleteRedundantEmptyLines: TCheckBox
      Left = 536
      Top = 242
      Width = 241
      Height = 23
      Caption = 'Delete Redundant Empty Lines'
      TabOrder = 12
      OnClick = OptionChange
    end
    object memoCustomCommand: TMemo
      Left = 21
      Top = 312
      Width = 736
      Height = 41
      TabOrder = 13
      OnChange = OptionChange
    end
  end
end
