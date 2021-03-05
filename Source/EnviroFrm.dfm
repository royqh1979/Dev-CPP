object EnviroForm: TEnviroForm
  Left = 971
  Top = 159
  BorderStyle = bsDialog
  Caption = 'Environment Options'
  ClientHeight = 616
  ClientWidth = 648
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
    648
    616)
  PixelsPerInch = 120
  TextHeight = 20
  object btnOk: TBitBtn
    Left = 283
    Top = 573
    Width = 113
    Height = 34
    Anchors = [akRight, akBottom]
    Caption = 'OK'
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
    Left = 403
    Top = 573
    Width = 113
    Height = 34
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 523
    Top = 573
    Width = 113
    Height = 34
    Anchors = [akRight, akBottom]
    Enabled = False
    TabOrder = 2
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object PagesMain: TPageControl
    Left = 0
    Top = 0
    Width = 645
    Height = 567
    ActivePage = tabGeneral
    HotTrack = True
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      ParentShowHint = False
      ShowHint = False
      DesignSize = (
        637
        532)
      object lblMRU: TLabel
        Left = 400
        Top = 19
        Width = 157
        Height = 20
        Caption = 'Max Files in History List:'
      end
      object lblMsgTabs: TLabel
        Left = 403
        Top = 83
        Width = 131
        Height = 20
        Caption = 'Editor Tab Location:'
      end
      object lblLang: TLabel
        Left = 403
        Top = 147
        Width = 65
        Height = 20
        Caption = 'Language'
      end
      object lblTheme: TLabel
        Left = 403
        Top = 211
        Width = 45
        Height = 20
        Caption = 'Theme'
        Visible = False
      end
      object UIfontlabel: TLabel
        Left = 331
        Top = 277
        Width = 48
        Height = 20
        Caption = 'UI font:'
      end
      object cbBackups: TCheckBox
        Left = 21
        Top = 49
        Width = 354
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Create File Backups'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object cbMinOnRun: TCheckBox
        Left = 21
        Top = 77
        Width = 324
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Minimize on Run'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object cbDefCpp: TCheckBox
        Left = 21
        Top = 21
        Width = 324
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Default to C++ on New Project'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object cbShowBars: TCheckBox
        Left = 21
        Top = 107
        Width = 354
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show Toolbars in Full Screen'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object cbMultiLineTab: TCheckBox
        Left = 21
        Top = 135
        Width = 354
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable Editor Multiline Tabs'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object rgbAutoOpen: TRadioGroup
        Left = 328
        Top = 363
        Width = 287
        Height = 145
        Caption = 'Auto Open'
        Items.Strings = (
          'All project files'
          'Only first project file'
          'Opened files at previous closing'
          'None')
        TabOrder = 15
      end
      object gbDebugger: TGroupBox
        Left = 20
        Top = 227
        Width = 287
        Height = 129
        Caption = 'Debug Variable Browser'
        TabOrder = 7
        object cbWatchHint: TCheckBox
          Left = 21
          Top = 32
          Width = 260
          Height = 23
          Caption = 'Watch variable under mouse'
          TabOrder = 0
        end
        object cbShowDbgCmd: TCheckBox
          Left = 21
          Top = 67
          Width = 260
          Height = 22
          Caption = 'Display Debug Commands'
          TabOrder = 1
        end
        object cbShowDbgFullAnnotation: TCheckBox
          Left = 21
          Top = 96
          Width = 260
          Height = 23
          Caption = 'Show All gdb Annotations'
          TabOrder = 2
        end
      end
      object gbProgress: TGroupBox
        Left = 20
        Top = 375
        Width = 287
        Height = 93
        Caption = 'Compilation Progress Window '
        TabOrder = 8
        object cbShowProgress: TCheckBox
          Left = 19
          Top = 29
          Width = 260
          Height = 23
          Caption = '&Show during compilation'
          TabOrder = 0
        end
        object cbAutoCloseProgress: TCheckBox
          Left = 19
          Top = 57
          Width = 260
          Height = 23
          Caption = '&Auto close after compile'
          TabOrder = 1
        end
      end
      object seMRUMax: TSpinEdit
        Left = 400
        Top = 43
        Width = 68
        Height = 31
        MaxLength = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 9
        Value = 0
      end
      object cboTabsTop: TComboBox
        Left = 403
        Top = 107
        Width = 213
        Height = 28
        Style = csDropDownList
        ItemHeight = 20
        TabOrder = 10
        Items.Strings = (
          'Top'
          'Bottom'
          'Left'
          'Right')
      end
      object cboLang: TComboBox
        Left = 403
        Top = 171
        Width = 213
        Height = 28
        Style = csDropDownList
        ItemHeight = 20
        TabOrder = 11
      end
      object cboTheme: TComboBox
        Left = 403
        Top = 235
        Width = 213
        Height = 28
        Style = csDropDownList
        ItemHeight = 20
        TabOrder = 12
        Visible = False
      end
      object cbUIfont: TComboBox
        Left = 331
        Top = 309
        Width = 204
        Height = 26
        AutoComplete = False
        Style = csOwnerDrawVariable
        DropDownCount = 10
        ItemHeight = 20
        Sorted = True
        TabOrder = 13
        OnDrawItem = cbUIfontDrawItem
      end
      object cbUIfontsize: TComboBox
        Left = 547
        Top = 309
        Width = 62
        Height = 26
        AutoComplete = False
        Style = csOwnerDrawVariable
        DropDownCount = 10
        ItemHeight = 20
        TabOrder = 14
        OnChange = cbUIfontsizeChange
        OnDrawItem = cbUIfontsizeDrawItem
        Items.Strings = (
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15'
          '16')
      end
      object cbPauseConsole: TCheckBox
        Left = 21
        Top = 163
        Width = 354
        Height = 22
        Caption = 'Pause console programs after return'
        TabOrder = 5
      end
      object cbCheckAssocs: TCheckBox
        Left = 21
        Top = 191
        Width = 354
        Height = 22
        Caption = 'Check file associations on startup'
        TabOrder = 6
      end
      object btnHighDPIFixExit: TButton
        Left = 16
        Top = 488
        Width = 297
        Height = 33
        Caption = 'Fix HighDPI and Exit'
        TabOrder = 16
        OnClick = btnHighDPIFixExitClick
      end
    end
    object tabIcon: TTabSheet
      Caption = 'Icons'
      ImageIndex = 4
      object lblMenuIconSize: TLabel
        Left = 16
        Top = 24
        Width = 103
        Height = 20
        Caption = 'Menu Icon Size:'
      end
      object lblToolbarIconSize: TLabel
        Left = 16
        Top = 76
        Width = 117
        Height = 20
        Caption = 'Toolbar Icon Size:'
      end
      object lblTabIconSize: TLabel
        Left = 16
        Top = 128
        Width = 95
        Height = 20
        Caption = 'Tabs Icon Size:'
      end
      object cbMenuIconSize: TComboBox
        Left = 155
        Top = 21
        Width = 150
        Height = 28
        AutoComplete = False
        Style = csDropDownList
        DropDownCount = 10
        ItemHeight = 20
        Sorted = True
        TabOrder = 0
        Items.Strings = (
          '16x16'
          '24x24'
          '28x28'
          '32x32'
          '48x48')
      end
      object cbToolbarIconSize: TComboBox
        Left = 155
        Top = 73
        Width = 150
        Height = 28
        AutoComplete = False
        Style = csDropDownList
        DropDownCount = 10
        ItemHeight = 20
        Sorted = True
        TabOrder = 1
        Items.Strings = (
          '16x16'
          '24x24'
          '28x28'
          '32x32'
          '48x48')
      end
      object cbTabIconSize: TComboBox
        Left = 155
        Top = 125
        Width = 150
        Height = 28
        AutoComplete = False
        Style = csDropDownList
        DropDownCount = 10
        ItemHeight = 20
        Sorted = True
        TabOrder = 2
        Items.Strings = (
          '16x16'
          '24x24'
          '28x28'
          '32x32'
          '48x48')
      end
    end
    object tabPaths: TTabSheet
      Caption = 'Directories'
      ParentShowHint = False
      ShowHint = False
      object lblUserDir: TLabel
        Left = 11
        Top = 91
        Width = 156
        Height = 20
        Caption = 'User'#39's Default Directory'
      end
      object lblTemplatesDir: TLabel
        Left = 11
        Top = 240
        Width = 133
        Height = 20
        Caption = 'Templates Directory'
      end
      object lblSplash: TLabel
        Left = 11
        Top = 449
        Width = 137
        Height = 20
        Caption = 'Splash Screen Image'
      end
      object lblIcoLib: TLabel
        Left = 11
        Top = 309
        Width = 109
        Height = 20
        Caption = 'Icon Library Path'
      end
      object lblLangPath: TLabel
        Left = 11
        Top = 380
        Width = 97
        Height = 20
        Caption = 'Language Path'
      end
      object btnDefBrws: TSpeedButton
        Tag = 1
        Left = 584
        Top = 117
        Width = 31
        Height = 30
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000000000
          000000000000000000000000000000000000000000000000000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF00000000AEFF0096DB0096DB0096DB0096DB0096DB00
          96DB0096DB0082BE000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF00AEFF00AEFF00AEFF00
          AEFF00AEFF0096DB000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF5DCCFF5DCCFF5DCCFF5D
          CCFF5DCCFF00AEFF000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF686868BDEBFF
          5DCCFF5DCCFF000000000000000000000000000000000000BFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBF000000000000000000BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
        OnClick = BrowseClick
      end
      object btnOutputbrws: TSpeedButton
        Tag = 2
        Left = 583
        Top = 267
        Width = 30
        Height = 29
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000000000
          000000000000000000000000000000000000000000000000000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF00000000AEFF0096DB0096DB0096DB0096DB0096DB00
          96DB0096DB0082BE000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF00AEFF00AEFF00AEFF00
          AEFF00AEFF0096DB000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF5DCCFF5DCCFF5DCCFF5D
          CCFF5DCCFF00AEFF000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF686868BDEBFF
          5DCCFF5DCCFF000000000000000000000000000000000000BFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBF000000000000000000BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
        OnClick = BrowseClick
      end
      object btnBrwIcon: TSpeedButton
        Tag = 3
        Left = 583
        Top = 336
        Width = 30
        Height = 29
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000000000
          000000000000000000000000000000000000000000000000000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF00000000AEFF0096DB0096DB0096DB0096DB0096DB00
          96DB0096DB0082BE000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF00AEFF00AEFF00AEFF00
          AEFF00AEFF0096DB000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF5DCCFF5DCCFF5DCCFF5D
          CCFF5DCCFF00AEFF000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF686868BDEBFF
          5DCCFF5DCCFF000000000000000000000000000000000000BFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBF000000000000000000BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
        OnClick = BrowseClick
      end
      object btnBrwLang: TSpeedButton
        Tag = 5
        Left = 583
        Top = 407
        Width = 30
        Height = 29
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000000000
          000000000000000000000000000000000000000000000000000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF00000000AEFF0096DB0096DB0096DB0096DB0096DB00
          96DB0096DB0082BE000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF00AEFF00AEFF00AEFF00
          AEFF00AEFF0096DB000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF5DCCFF5DCCFF5DCCFF5D
          CCFF5DCCFF00AEFF000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF686868BDEBFF
          5DCCFF5DCCFF000000000000000000000000000000000000BFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBF000000000000000000BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
        OnClick = BrowseClick
      end
      object btnBrwSplash: TSpeedButton
        Tag = 4
        Left = 583
        Top = 476
        Width = 30
        Height = 29
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000000000
          000000000000000000000000000000000000000000000000000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF00000000AEFF0096DB0096DB0096DB0096DB0096DB00
          96DB0096DB0082BE000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF00AEFF00AEFF00AEFF00
          AEFF00AEFF0096DB000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF5DCCFF5DCCFF5DCCFF5D
          CCFF5DCCFF00AEFF000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF686868BDEBFF
          5DCCFF5DCCFF000000000000000000000000000000000000BFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBF000000000000000000BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
        OnClick = BrowseClick
      end
      object lblOptionsDir: TLabel
        Left = 11
        Top = 13
        Width = 400
        Height = 20
        Caption = 'Current Options directory. Click the button to reset Dev-C++.'
      end
      object lblProjectsDir: TLabel
        Left = 11
        Top = 168
        Width = 117
        Height = 20
        Caption = 'Projects Directory'
      end
      object btnProjectsDir: TSpeedButton
        Tag = 6
        Left = 583
        Top = 195
        Width = 30
        Height = 29
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
          BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
          00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000000000
          000000000000000000000000000000000000000000000000000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF00000000AEFF0096DB0096DB0096DB0096DB0096DB00
          96DB0096DB0082BE000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF00AEFF00AEFF00AEFF00
          AEFF00AEFF0096DB000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
          00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF5DCCFF5DCCFF5DCCFF5D
          CCFF5DCCFF00AEFF000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF686868BDEBFF
          5DCCFF5DCCFF000000000000000000000000000000000000BFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBF000000000000000000BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
        OnClick = BrowseClick
      end
      object btnOpenOptionsDir: TSpeedButton
        Left = 408
        Top = 43
        Width = 33
        Height = 33
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C30E0000C30E00000000000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBF000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000000000
          C5DE000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF00000000000000C5DE00BDD600BDD6000000BFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000ADBD00000000
          BDD600ADBD00C5DE00C5DE000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBF00000000BDD600000000C5DE00BDCE00BDD600BDD600BDD600C5
          DE000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000ADBD00ADBD00000000
          BDD600ADBD00C5DE00BDD600BDD600C5DE00BDD6000000BFBFBFBFBFBFBFBFBF
          BFBFBF00000000C5DE00C5DE00000000C5DE00BDCE00ADBD00BDD600BDCE00BD
          CE00ADBD00BDD6000000BFBFBFBFBFBF00000000BDD600C5DE00C5DE00000000
          BDD600BDD600C5DE00BDD600ADBD00BDD600C5DE00BDD6000000BFBFBFBFBFBF
          00000000BDD600C5DE00C5DE00000000BDCE00ADBD00BDD600ADBD00BDD600BD
          CE00ADBD00ADBD000000BFBFBF00000000C5DE00C5DE00C5DE00C5DE00C5DE00
          000000000000000000BDCE00BDCE00ADBD00BDCE00ADBD000000BFBFBF000000
          00BDD600BDD600C5DE00C5DE00C5DE00C5DE00C5DE00C5DE00000000ADBD00AD
          BD00ADBD00BDCE00000000000000BDD600ADBD00BDD600C5DE00C5DE00C5DE00
          C5DE00C5DE00C5DE00C5DE00000000000000BDCE00ADBD00000000000000C5DE
          00BDD600000000000000C5DE00BDD600C5DE00C5DE00C5DE00C5DE00C5DE00C5
          DE00000000ADBD000000BFBFBF000000000000BFBFBFBFBFBF00000000000000
          000000ADBD00BDD600C5DE00C5DE000000BFBFBF000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000000000ADBD000000BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
        OnClick = btnOpenOptionsDirClick
      end
      object edUserDir: TEdit
        Left = 21
        Top = 117
        Width = 546
        Height = 28
        ReadOnly = True
        TabOrder = 0
        Text = 'edUserDir'
      end
      object edTemplatesDir: TEdit
        Left = 21
        Top = 267
        Width = 546
        Height = 28
        ReadOnly = True
        TabOrder = 1
        Text = 'edTemplatesDir'
      end
      object edSplash: TEdit
        Left = 21
        Top = 476
        Width = 546
        Height = 28
        ReadOnly = True
        TabOrder = 2
        Text = 'edSplash'
      end
      object edIcoLib: TEdit
        Left = 21
        Top = 336
        Width = 546
        Height = 28
        ReadOnly = True
        TabOrder = 3
        Text = 'edIcoLib'
      end
      object edLang: TEdit
        Left = 21
        Top = 407
        Width = 546
        Height = 28
        ReadOnly = True
        TabOrder = 4
        Text = 'edLang'
      end
      object edOptionsDir: TEdit
        Left = 21
        Top = 45
        Width = 375
        Height = 28
        ReadOnly = True
        TabOrder = 5
        Text = 'edOptionsDir'
      end
      object btnResetDev: TButton
        Left = 448
        Top = 43
        Width = 185
        Height = 33
        Caption = 'Remove settings and exit'
        TabOrder = 6
        OnClick = btnResetDevClick
      end
      object edProjectsDir: TEdit
        Left = 21
        Top = 195
        Width = 546
        Height = 28
        ReadOnly = True
        TabOrder = 7
      end
    end
    object tabExternal: TTabSheet
      Caption = 'External Programs'
      DesignSize = (
        637
        532)
      object lblExternal: TLabel
        Left = 11
        Top = 11
        Width = 208
        Height = 20
        Caption = 'External programs associations:'
      end
      object btnExtAdd: TSpeedButton
        Left = 171
        Top = 485
        Width = 132
        Height = 34
        Anchors = [akBottom]
        Caption = 'Add'
        OnClick = btnExtAddClick
      end
      object btnExtDel: TSpeedButton
        Left = 348
        Top = 485
        Width = 132
        Height = 34
        Anchors = [akBottom]
        Caption = 'Delete'
        OnClick = btnExtDelClick
      end
      object vleExternal: TValueListEditor
        Left = 37
        Top = 40
        Width = 556
        Height = 440
        Anchors = [akLeft, akTop, akRight, akBottom]
        DefaultRowHeight = 30
        KeyOptions = [keyEdit, keyAdd, keyDelete]
        Options = [goVertLine, goHorzLine, goEditing, goAlwaysShowEditor, goThumbTracking]
        TabOrder = 0
        TitleCaptions.Strings = (
          'Extension'
          'External program')
        OnEditButtonClick = vleExternalEditButtonClick
        OnValidate = vleExternalValidate
        ColWidths = (
          84
          466)
      end
    end
    object tabAssocs: TTabSheet
      Caption = 'File Associations'
      ParentShowHint = False
      ShowHint = False
      DesignSize = (
        637
        532)
      object lblAssocFileTypes: TLabel
        Left = 11
        Top = 11
        Width = 67
        Height = 20
        Caption = 'File Types:'
      end
      object lblAssocDesc: TLabel
        Left = 40
        Top = 469
        Width = 557
        Height = 40
        Alignment = taCenter
        Caption = 
          'Just check or un-check for which file types Dev-C++ will be regi' +
          'stered as the default application to open them...'
        WordWrap = True
      end
      object lstAssocFileTypes: TCheckListBox
        Left = 37
        Top = 40
        Width = 556
        Height = 427
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 20
        TabOrder = 0
      end
    end
  end
  object dlgPic: TOpenPictureDialog
    Left = 14
    Top = 426
  end
end
