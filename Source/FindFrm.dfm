object FindForm: TFindForm
  Left = 238
  Top = 183
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Find Text'
  ClientHeight = 450
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  PopupMenu = FindPopup
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    406
    450)
  PixelsPerInch = 120
  TextHeight = 16
  object btnExecute: TButton
    Left = 10
    Top = 414
    Width = 123
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 'Find'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnExecuteClick
  end
  object btnCancel: TButton
    Left = 273
    Top = 414
    Width = 123
    Height = 30
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object FindTabs: TTabControl
    Left = 0
    Top = 0
    Width = 406
    Height = 401
    Align = alTop
    TabOrder = 2
    Tabs.Strings = (
      'Find'
      'Find in files'
      'Replace'
      'Replace in files')
    TabIndex = 0
    OnChange = FindTabsChange
    object lblFind: TLabel
      Left = 10
      Top = 36
      Width = 67
      Height = 16
      Caption = '&Text to find:'
      FocusControl = cboFindText
    end
    object lblReplace: TLabel
      Left = 10
      Top = 87
      Width = 80
      Height = 16
      Caption = 'Replace with:'
      FocusControl = cboFindText
    end
    object cboFindText: TComboBox
      Left = 10
      Top = 57
      Width = 386
      Height = 24
      ItemHeight = 16
      TabOrder = 0
      OnKeyUp = cboFindTextKeyUp
    end
    object grpOptions: TGroupBox
      Left = 10
      Top = 145
      Width = 187
      Height = 144
      Caption = '  Options:  '
      TabOrder = 2
      object cbMatchCase: TCheckBox
        Left = 10
        Top = 20
        Width = 148
        Height = 21
        Caption = 'C&ase sensitive'
        TabOrder = 0
      end
      object cbWholeWord: TCheckBox
        Left = 10
        Top = 49
        Width = 149
        Height = 21
        Caption = '&Whole words only'
        TabOrder = 1
      end
      object cbPrompt: TCheckBox
        Left = 10
        Top = 111
        Width = 148
        Height = 21
        Caption = '&Prompt on Replace'
        TabOrder = 2
      end
      object cbRegExp: TCheckBox
        Left = 10
        Top = 81
        Width = 149
        Height = 21
        Caption = 'Regular Expression'
        TabOrder = 3
      end
    end
    object grpDirection: TGroupBox
      Left = 208
      Top = 145
      Width = 187
      Height = 86
      Caption = '  Direction:  '
      TabOrder = 3
      object rbBackward: TRadioButton
        Left = 10
        Top = 52
        Width = 149
        Height = 21
        Caption = '&Backward'
        TabOrder = 0
      end
      object rbForward: TRadioButton
        Left = 10
        Top = 22
        Width = 149
        Height = 21
        Caption = '&Forward'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object grpWhere: TGroupBox
      Left = 208
      Top = 145
      Width = 187
      Height = 144
      Caption = '  Where:  '
      TabOrder = 4
      object rbProjectFiles: TRadioButton
        Left = 10
        Top = 22
        Width = 149
        Height = 21
        Caption = 'Files in Project'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbOpenFiles: TRadioButton
        Left = 10
        Top = 52
        Width = 149
        Height = 21
        Caption = 'Open Files'
        TabOrder = 1
      end
      object rbCurFile: TRadioButton
        Left = 10
        Top = 79
        Width = 149
        Height = 21
        Caption = 'Current file'
        TabOrder = 2
      end
    end
    object grpScope: TGroupBox
      Left = 10
      Top = 302
      Width = 187
      Height = 84
      Caption = '  Scope:  '
      TabOrder = 5
      object rbGlobal: TRadioButton
        Left = 10
        Top = 22
        Width = 149
        Height = 21
        Caption = '&Global'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbSelectedOnly: TRadioButton
        Left = 10
        Top = 52
        Width = 149
        Height = 21
        Caption = '&Selected only'
        TabOrder = 1
      end
    end
    object grpOrigin: TGroupBox
      Left = 208
      Top = 302
      Width = 187
      Height = 84
      Caption = '  Origin:  '
      TabOrder = 6
      object rbFromCursor: TRadioButton
        Left = 10
        Top = 22
        Width = 149
        Height = 21
        Caption = 'From &cursor'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbEntireScope: TRadioButton
        Left = 10
        Top = 52
        Width = 149
        Height = 21
        Caption = 'Entire &scope'
        TabOrder = 1
      end
    end
    object cboReplaceText: TComboBox
      Left = 10
      Top = 108
      Width = 386
      Height = 24
      ItemHeight = 16
      TabOrder = 1
      OnKeyUp = cboReplaceTextKeyUp
    end
  end
  object FindPopup: TPopupMenu
    Left = 112
    Top = 296
    object FindCut: TMenuItem
      Caption = 'Cut'
      ShortCut = 16472
      OnClick = FindCutClick
    end
    object FindCopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = FindCopyClick
    end
    object FindPaste: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = FindPasteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object FindSelAll: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = FindSelAllClick
    end
  end
end
