object NewTemplateForm: TNewTemplateForm
  Left = 557
  Top = 743
  BorderStyle = bsDialog
  Caption = 'New Template'
  ClientHeight = 395
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object devPages1: TPageControl
    Left = 0
    Top = 0
    Width = 441
    Height = 346
    ActivePage = pgTemplate
    Align = alTop
    TabOrder = 0
    object pgTemplate: TTabSheet
      Caption = 'Template info'
      object lblName: TLabel
        Left = 10
        Top = 15
        Width = 40
        Height = 16
        Caption = 'Name:'
      end
      object lblDescr: TLabel
        Left = 10
        Top = 49
        Width = 71
        Height = 16
        Caption = 'Description:'
      end
      object lblCateg: TLabel
        Left = 10
        Top = 84
        Width = 58
        Height = 16
        Caption = 'Category:'
      end
      object txtDescr: TEdit
        Left = 138
        Top = 44
        Width = 277
        Height = 24
        TabOrder = 1
        Text = 'txtDescr'
      end
      object cmbCateg: TComboBox
        Left = 138
        Top = 79
        Width = 277
        Height = 24
        AutoDropDown = True
        ItemHeight = 16
        Sorted = True
        TabOrder = 2
        Text = 'cmbCateg'
        OnChange = cmbNameChange
      end
      object cmbName: TComboBox
        Left = 138
        Top = 10
        Width = 277
        Height = 24
        AutoDropDown = True
        ItemHeight = 16
        Sorted = True
        TabOrder = 0
        Text = 'cmbName'
        OnChange = cmbNameChange
      end
      object grpIcons: TGroupBox
        Left = 10
        Top = 148
        Width = 405
        Height = 145
        Caption = 'Icons'
        TabOrder = 3
        object lstIcons: TListBox
          Left = 15
          Top = 25
          Width = 223
          Height = 104
          Style = lbOwnerDrawFixed
          ExtendedSelect = False
          ItemHeight = 36
          TabOrder = 0
          OnClick = lstIconsClick
          OnDrawItem = lstIconsDrawItem
        end
        object btnLib: TBitBtn
          Left = 256
          Top = 21
          Width = 139
          Height = 29
          Hint = 'Select a icon from Dev-C++'#39's icon collection'
          Caption = 'Library'
          TabOrder = 1
          OnClick = btnLibClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
            55555FF555FFFFFFF5FF0075500000005000775F57777777F7770B0550CC0090
            50E07F7F57FF77F7F7F70B055000000050E07F75F7777777F7F70B7750CC0090
            50E075F7F7F577F7F7F777B050CC009050E057F7F7F577F7F7F750B050CC0090
            50E057F757F577F7F7F750B770CC009050E0575F77F577F7F7F7577B00CC0090
            50E0557F77F577F7F7F7550B30CC00905000557F57FF77F7F777550B30000090
            50E05575F77777F7F7F7557000CC00905000555777FF77F7F777555550000090
            55555555577777F7F5555555555550005555555555555777F555555555555090
            55555555555557F7F55555555555500055555555555557775555}
          NumGlyphs = 2
        end
        object btnBrowse: TBitBtn
          Left = 256
          Top = 63
          Width = 139
          Height = 27
          Hint = 'Select a custom icon'
          Caption = 'Browse...'
          TabOrder = 2
          OnClick = btnBrowseClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
            5555555555555555555555555555555555555555555555555555555555555555
            555555555555555555555555555555555555555FFFFFFFFFF555550000000000
            55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
            B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
            000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
            555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
            55555575FFF75555555555700007555555555557777555555555555555555555
            5555555555555555555555555555555555555555555555555555}
          NumGlyphs = 2
        end
        object btnRemove: TBitBtn
          Left = 256
          Top = 102
          Width = 139
          Height = 27
          Hint = 'Do not use an icon for this project'
          Caption = 'Remove'
          Enabled = False
          TabOrder = 3
          OnClick = btnRemoveClick
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000AAAAAAAAAAAA
            AAAAAA000000000000000000000000000000000000000000000000080808AAAA
            AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA00000075977D72947A718F786E8D746B
            8A7268856F67826D647D69000000080808080808AAAAAAAAAAAAAAAAAA000000
            789A809AB2A05F786596AD9A5C72606B8A724C5B4F67836D536858617A670000
            00080808080808AAAAAAAAAAAA000000637D68B4C5B75F786596AD9A4E5F526D
            8A724C5C4F67836D48584C637A68000000080808080808080808AAAAAA000000
            637D68B4C6B75F786596AD9A4E5F526D8A724C5C4F67836E48584D637A680000
            00080808080808080808AAAAAA000000637D68B4C6B95F786596AD9B4F5F526D
            8A724C5C4F67836E49584D637B68000000080808080808080808AAAAAA000000
            637D68B4C6B95F786596AD9B4F5F526D8A724C5C4F68836E49584D637B680000
            00080808080808AAAAAAAAAAAA000000637D68B4C6B95F786597AE9B4F5F526D
            8A724C5C4F68846E49584D637B68000000AAAAAAAAAAAAAAAAAAAAAAAA000000
            637D68252525252525252525252525252525252525252525252525637B680000
            00AAAAAAAAAAAAAAAAAAAAAAAA0000002525256E8B73637B68637B685A6F5E52
            64574553494B5B4F3D453F252525000000AAAAAAAAAAAAAAAAAA000000547363
            6B8C748DAA948AA58F8AA58F74967C74967C68866F617966586E5D4B5A4D4552
            48000000AAAAAAAAAAAA00000074967CABC0B0C8D6CED6E1DAB1C4BB94ADA091
            AB9E8FAA9B8EA79A8CA59874967C576B5B000000AAAAAAAAAAAA0000008DA596
            D2DED7B5C9BE98B3A496B1A295B0A094ADA091AB9E8FAA9B8EA79A8CA5988AA3
            97000000AAAAAAAAAAAA000000ADC4B6B7CCBF9BB6A7B5C9BE5E6F65282E2B31
            393522262491AB9E8FAA9B8EA79A8CA598000000AAAAAAAAAAAAAAAAAA252525
            0000009EBAA8ABC1B4DDE7E17D8A83869F90434E4894ADA091AB9E0000000000
            00AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA00000000000000000000000000
            0000000000000000000000AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA}
        end
      end
    end
    object pgFiles: TTabSheet
      Caption = 'Files'
      object lblFiles: TLabel
        Left = 10
        Top = 15
        Width = 100
        Height = 16
        Caption = 'Files in template:'
      end
      object lstFiles: TCheckListBox
        Left = 20
        Top = 34
        Width = 395
        Height = 263
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pgExtras: TTabSheet
      Caption = 'Extras'
      object lblCompiler: TLabel
        Left = 10
        Top = 15
        Width = 136
        Height = 16
        Caption = 'Compiler extra options:'
      end
      object lblLinker: TLabel
        Left = 300
        Top = 15
        Width = 118
        Height = 16
        Caption = 'Linker extra options:'
      end
      object lblCppCompiler: TLabel
        Left = 158
        Top = 15
        Width = 128
        Height = 16
        Caption = 'C++ compiler options:'
      end
      object memCompiler: TMemo
        Left = 15
        Top = 34
        Width = 124
        Height = 184
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object memLinker: TMemo
        Left = 295
        Top = 36
        Width = 130
        Height = 182
        ScrollBars = ssVertical
        TabOrder = 2
      end
      object memCppCompiler: TMemo
        Left = 148
        Top = 34
        Width = 134
        Height = 184
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object cbInclude: TCheckBox
        Left = 20
        Top = 236
        Width = 405
        Height = 21
        Caption = 'Use project'#39's Include directories'
        TabOrder = 3
      end
      object cbLibrary: TCheckBox
        Left = 20
        Top = 256
        Width = 405
        Height = 21
        Caption = 'Use project'#39's Library directories'
        TabOrder = 4
      end
      object cbResource: TCheckBox
        Left = 20
        Top = 276
        Width = 405
        Height = 21
        Caption = 'Use project'#39's Resource directories'
        TabOrder = 5
      end
    end
  end
  object btnCreate: TButton
    Left = 123
    Top = 357
    Width = 92
    Height = 31
    Caption = 'Create'
    Default = True
    TabOrder = 1
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 226
    Top = 357
    Width = 93
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object dlgPic: TOpenPictureDialog
    DefaultExt = 'ico'
    Filter = 'Icons (*.ico)|*.ico'
    Title = 'Open icon'
    Left = 295
    Top = 286
  end
end
