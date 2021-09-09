object NewFunctionForm: TNewFunctionForm
  Left = 297
  Top = 417
  BorderStyle = bsDialog
  Caption = 'New member function'
  ClientHeight = 332
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
  object lblReturnType: TLabel
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
  object lblArguments: TLabel
    Left = 10
    Top = 74
    Width = 67
    Height = 16
    Caption = 'Arguments:'
  end
  object lblImplementIn: TLabel
    Left = 10
    Top = 103
    Width = 78
    Height = 16
    Caption = 'Implement in:'
  end
  object rgScope: TRadioGroup
    Left = 10
    Top = 138
    Width = 149
    Height = 119
    Caption = 'Access scope'
    ItemIndex = 2
    Items.Strings = (
      'Private'
      'Protected'
      'Public')
    TabOrder = 4
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
  object txtArguments: TEdit
    Left = 108
    Top = 69
    Width = 429
    Height = 24
    TabOrder = 2
  end
  object btnCreate: TButton
    Left = 165
    Top = 295
    Width = 92
    Height = 31
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 7
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 270
    Top = 295
    Width = 92
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object cmbClass: TComboBox
    Left = 108
    Top = 98
    Width = 429
    Height = 24
    AutoDropDown = True
    ItemHeight = 16
    Sorted = True
    TabOrder = 3
    Items.Strings = (
      'bool'
      'double'
      'float'
      'int'
      'void')
  end
  object chkToDo: TCheckBox
    Left = 15
    Top = 266
    Width = 518
    Height = 21
    Caption = 'Add TODO item'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object grpAttr: TGroupBox
    Left = 167
    Top = 138
    Width = 218
    Height = 119
    Caption = 'Attributes'
    TabOrder = 5
    object chkStatic: TCheckBox
      Left = 15
      Top = 25
      Width = 193
      Height = 21
      Caption = 'Static'
      TabOrder = 0
      OnClick = chkStaticClick
    end
    object chkVirtual: TCheckBox
      Left = 15
      Top = 54
      Width = 85
      Height = 21
      Caption = 'Virtual'
      TabOrder = 1
      OnClick = chkStaticClick
    end
    object chkPure: TCheckBox
      Left = 118
      Top = 54
      Width = 70
      Height = 21
      Caption = 'Pure'
      TabOrder = 2
      OnClick = chkStaticClick
    end
    object chkInline: TCheckBox
      Left = 15
      Top = 84
      Width = 193
      Height = 21
      Caption = 'Inline'
      TabOrder = 3
      OnClick = chkStaticClick
    end
  end
end
