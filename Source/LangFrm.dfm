object LangForm: TLangForm
  Left = 380
  Top = 221
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Dev-C++ first time configuration'
  ClientHeight = 427
  ClientWidth = 747
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 20
  object OkBtn: TBitBtn
    Left = 360
    Top = 373
    Width = 373
    Height = 40
    Caption = '&Next'
    Default = True
    TabOrder = 0
    OnClick = OkBtnClick
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF000000009836000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000A13900A13900983600
      0000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      0000008FFF8F00C54600B03F00B03F009836000000BFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000008FFF8F00C54600B03F00
      B03F009836000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF0000008FFF8F00C54600B03F00B03F009836000000BFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000008FFF8F00
      B03F00B03F00A139009836000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBF00000000B03F00B03F00B03F00A1390098360000
      00BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000B03F00
      B03F00B03F00B03F00A139000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF00000000C54600B03F00B03F00B03F00A139000000BFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000C54600C54600B03F00
      B03F00B03F000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      0000008FFF8F00DD0000C54600C54600C546000000BFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000008FFF8F00DD0000C54600
      0000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF0000008FFF8F000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
  end
  object LangPanel: TPanel
    Left = 347
    Top = 0
    Width = 400
    Height = 367
    BevelOuter = bvNone
    TabOrder = 1
    object lblLangInfo: TLabel
      Left = 11
      Top = 307
      Width = 378
      Height = 64
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'You can later change the language at Tools >> Environment Option' +
        's >> General.'
      WordWrap = True
    end
    object grpLanguages: TGroupBox
      Left = 21
      Top = 16
      Width = 358
      Height = 284
      Caption = 'Select your language:'
      TabOrder = 0
      object lbLanguages: TListBox
        Left = 11
        Top = 27
        Width = 333
        Height = 241
        ItemHeight = 20
        TabOrder = 0
      end
    end
  end
  object FinishPanel: TPanel
    Left = 347
    Top = 0
    Width = 400
    Height = 367
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object Finish2: TLabel
      Left = 11
      Top = 85
      Width = 364
      Height = 98
      AutoSize = False
      Caption = 
        'If you need help using Dev-C++, please refer to the Dev-C++ help' +
        ' file in the Help menu or send the developer a message (he doesn' +
        #39't mind!).'
      WordWrap = True
    end
    object Finish3: TLabel
      Left = 11
      Top = 195
      Width = 378
      Height = 100
      AutoSize = False
      Caption = 
        'You can also download packages (like libraries or tools) to use ' +
        'with Dev-C++ using WebUpdate, which you will find in Tools menu ' +
        '>> Check for Packages.'
      WordWrap = True
    end
    object Finish1: TLabel
      Left = 11
      Top = 11
      Width = 378
      Height = 60
      AutoSize = False
      Caption = 
        'Dev-C++ has been configured successfully, you may now click OK t' +
        'o proceed to its loading.'
      WordWrap = True
    end
  end
  object synExample: TSynEdit
    Left = 0
    Top = 0
    Width = 347
    Height = 427
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 3
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.RightOffset = 21
    Lines.Strings = (
      '#include <iostream>'
      ''
      'int main(int argc, char** argv) {'
      #9'std::cout << "Hello world!\n";'
      #9'return 0;'
      '}')
    FontSmoothing = fsmNone
  end
  object EditPanel: TPanel
    Left = 347
    Top = 0
    Width = 400
    Height = 367
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object lblEditInfo: TLabel
      Left = 11
      Top = 307
      Width = 378
      Height = 64
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'You can later change themes at Tools >> Editor Options >> Fonts/' +
        'Colors.'
      WordWrap = True
    end
    object grpThemes: TGroupBox
      Left = 21
      Top = 16
      Width = 358
      Height = 273
      Caption = 'Select your theme:'
      TabOrder = 0
      object lblMenuIconSize: TLabel
        Left = 11
        Top = 131
        Width = 101
        Height = 20
        Caption = 'Menu icon size:'
      end
      object lblColor: TLabel
        Left = 11
        Top = 80
        Width = 39
        Height = 20
        Caption = 'Color:'
      end
      object lblFont: TLabel
        Left = 11
        Top = 43
        Width = 32
        Height = 20
        Caption = 'Font:'
      end
      object lblToolbarIconSize: TLabel
        Left = 11
        Top = 179
        Width = 115
        Height = 20
        Caption = 'Toolbar icon size:'
      end
      object lblTabIconSize: TLabel
        Left = 11
        Top = 227
        Width = 89
        Height = 20
        Caption = 'Tab icon Size:'
      end
      object cmbMenuIconSize: TComboBox
        Left = 200
        Top = 128
        Width = 143
        Height = 28
        Style = csDropDownList
        ItemHeight = 20
        TabOrder = 0
        Items.Strings = (
          '16x16'
          '24x24'
          '28x28'
          '32x32'
          '48x48')
      end
      object cmbColors: TComboBox
        Left = 75
        Top = 77
        Width = 268
        Height = 28
        Style = csDropDownList
        ItemHeight = 20
        TabOrder = 1
        OnChange = ColorChange
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
          'PlasticCodeWrap'
          'VS Code')
      end
      object cmbFont: TComboBox
        Left = 75
        Top = 29
        Width = 268
        Height = 34
        AutoComplete = False
        Style = csOwnerDrawVariable
        ItemHeight = 28
        Sorted = True
        TabOrder = 2
        OnChange = FontChange
        OnDrawItem = cmbFontDrawItem
      end
      object cmbToolbarIconSize: TComboBox
        Left = 200
        Top = 176
        Width = 143
        Height = 28
        Style = csDropDownList
        ItemHeight = 20
        TabOrder = 3
        Items.Strings = (
          '16x16'
          '24x24'
          '28x28'
          '32x32'
          '48x48')
      end
      object cmbTabIconSize: TComboBox
        Left = 200
        Top = 224
        Width = 143
        Height = 28
        Style = csDropDownList
        ItemHeight = 20
        TabOrder = 4
        Items.Strings = (
          '16x16'
          '24x24'
          '28x28'
          '32x32'
          '48x48')
      end
    end
  end
end
