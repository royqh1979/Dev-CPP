object NewVarForm: TNewVarForm
  Left = 1171
  Top = 586
  BorderStyle = bsDialog
  Caption = 'New variable'
  ClientHeight = 460
  ClientWidth = 546
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object lblType: TLabel
    Left = 10
    Top = 15
    Width = 71
    Height = 16
    Caption = 'Return type:'
  end
  object lblName: TLabel
    Left = 10
    Top = 44
    Width = 91
    Height = 16
    Caption = 'Variable name:'
  end
  object lblImplementIn: TLabel
    Left = 10
    Top = 74
    Width = 78
    Height = 16
    Caption = 'Implement in:'
  end
  object rgScope: TRadioGroup
    Left = 10
    Top = 108
    Width = 523
    Height = 51
    Caption = 'Access scope'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Private'
      'Protected'
      'Public')
    TabOrder = 3
  end
  object txtType: TEdit
    Left = 108
    Top = 10
    Width = 429
    Height = 24
    TabOrder = 0
    OnChange = txtTypeChange
  end
  object txtName: TEdit
    Left = 108
    Top = 39
    Width = 429
    Height = 24
    TabOrder = 1
    OnChange = txtTypeChange
  end
  object btnCreate: TButton
    Left = 175
    Top = 423
    Width = 92
    Height = 31
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 279
    Top = 423
    Width = 93
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object cmbClass: TComboBox
    Left = 108
    Top = 69
    Width = 429
    Height = 24
    AutoDropDown = True
    ItemHeight = 16
    Sorted = True
    TabOrder = 2
    Items.Strings = (
      'bool'
      'double'
      'float'
      'int'
      'void')
  end
  object grpReadFunc: TGroupBox
    Left = 10
    Top = 197
    Width = 523
    Height = 86
    TabOrder = 6
    object Label3: TLabel
      Left = 8
      Top = 25
      Width = 40
      Height = 16
      Alignment = taRightJustify
      Caption = 'Name:'
    end
    object chkInlineR: TCheckBox
      Left = 10
      Top = 54
      Width = 65
      Height = 21
      Caption = 'Inline'
      Enabled = False
      TabOrder = 0
    end
    object txtReadFunc: TEdit
      Left = 103
      Top = 20
      Width = 410
      Height = 24
      TabOrder = 1
      OnChange = txtTypeChange
      OnKeyUp = txtReadFuncKeyUp
    end
  end
  object grpWriteFunc: TGroupBox
    Left = 10
    Top = 320
    Width = 523
    Height = 86
    TabOrder = 7
    object Label4: TLabel
      Left = 8
      Top = 25
      Width = 40
      Height = 16
      Alignment = taRightJustify
      Caption = 'Name:'
    end
    object chkInlineW: TCheckBox
      Left = 10
      Top = 54
      Width = 65
      Height = 21
      Caption = 'Inline'
      Enabled = False
      TabOrder = 0
    end
    object txtWriteFunc: TEdit
      Left = 103
      Top = 20
      Width = 410
      Height = 24
      TabOrder = 1
      OnChange = txtTypeChange
      OnKeyUp = txtWriteFuncKeyUp
    end
  end
  object chkReadFunc: TCheckBox
    Left = 15
    Top = 172
    Width = 410
    Height = 21
    Caption = 'Create member function to read from this variable'
    TabOrder = 8
    OnClick = chkReadFuncClick
  end
  object chkWriteFunc: TCheckBox
    Left = 15
    Top = 295
    Width = 410
    Height = 21
    Caption = 'Create member function to write to this variable'
    TabOrder = 9
    OnClick = chkWriteFuncClick
  end
end
