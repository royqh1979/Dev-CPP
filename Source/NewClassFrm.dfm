object NewClassForm: TNewClassForm
  Left = 1000
  Top = 72
  BorderStyle = bsDialog
  Caption = 'New class'
  ClientHeight = 416
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
  object lblClassName: TLabel
    Left = 10
    Top = 15
    Width = 40
    Height = 16
    Caption = 'Name:'
  end
  object lblCppFile: TLabel
    Left = 10
    Top = 281
    Width = 97
    Height = 16
    Caption = 'Source filename'
  end
  object lblHFile: TLabel
    Left = 10
    Top = 315
    Width = 103
    Height = 16
    Caption = 'Header filename:'
  end
  object btnBrowseCpp: TSpeedButton
    Left = 507
    Top = 276
    Width = 28
    Height = 27
    Caption = '...'
    OnClick = btnBrowseCppClick
  end
  object btnBrowseH: TSpeedButton
    Left = 507
    Top = 310
    Width = 28
    Height = 27
    Caption = '...'
    OnClick = btnBrowseCppClick
  end
  object lblArguments: TLabel
    Left = 10
    Top = 44
    Width = 67
    Height = 16
    Caption = 'Arguments:'
  end
  object txtName: TEdit
    Left = 108
    Top = 10
    Width = 429
    Height = 24
    TabOrder = 0
    OnChange = txtNameChange
    OnKeyPress = txtNameKeyPress
  end
  object grpInheritance: TGroupBox
    Left = 10
    Top = 143
    Width = 523
    Height = 114
    Caption = 'Inheritance'
    TabOrder = 5
    object lblAccess: TLabel
      Left = 10
      Top = 25
      Width = 89
      Height = 16
      Caption = 'Access scope:'
    end
    object lblInherit: TLabel
      Left = 10
      Top = 54
      Width = 102
      Height = 16
      Caption = 'Inherit from class:'
    end
    object lblHeaderFile: TLabel
      Left = 10
      Top = 84
      Width = 103
      Height = 16
      Caption = 'Header filename:'
    end
    object cmbClass: TComboBox
      Left = 153
      Top = 49
      Width = 360
      Height = 24
      Enabled = False
      ItemHeight = 16
      TabOrder = 1
      OnChange = cmbClassChange
    end
    object cmbScope: TComboBox
      Left = 153
      Top = 20
      Width = 360
      Height = 24
      Style = csDropDownList
      Enabled = False
      ItemHeight = 16
      TabOrder = 0
      Items.Strings = (
        'private'
        'protected'
        'public')
    end
    object txtIncFile: TEdit
      Left = 153
      Top = 79
      Width = 360
      Height = 24
      Enabled = False
      TabOrder = 2
    end
  end
  object txtCppFile: TEdit
    Left = 162
    Top = 276
    Width = 341
    Height = 24
    TabOrder = 6
    OnChange = txtCppFileChange
  end
  object txtHFile: TEdit
    Left = 162
    Top = 310
    Width = 341
    Height = 24
    TabOrder = 7
    OnChange = txtCppFileChange
  end
  object chkAddToProject: TCheckBox
    Left = 10
    Top = 350
    Width = 523
    Height = 20
    Caption = 'Add to current project'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object btnCreate: TButton
    Left = 175
    Top = 379
    Width = 92
    Height = 31
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 9
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 279
    Top = 379
    Width = 93
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object chkInherit: TCheckBox
    Left = 10
    Top = 113
    Width = 523
    Height = 21
    Caption = 'Inherit from another class'
    TabOrder = 4
    OnClick = chkInheritClick
  end
  object txtArgs: TEdit
    Left = 108
    Top = 39
    Width = 429
    Height = 24
    TabOrder = 1
  end
  object chkConstruct: TCheckBox
    Left = 108
    Top = 76
    Width = 208
    Height = 21
    Caption = 'Create Constructor'
    TabOrder = 2
  end
  object chkDestruct: TCheckBox
    Left = 325
    Top = 76
    Width = 208
    Height = 21
    Caption = 'Create Destructor'
    TabOrder = 3
  end
end
