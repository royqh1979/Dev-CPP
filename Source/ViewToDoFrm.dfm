object ViewToDoForm: TViewToDoForm
  Left = 486
  Top = 308
  Width = 795
  Height = 666
  BorderStyle = bsSizeToolWin
  Caption = 'To-Do list'
  Color = clBtnFace
  Constraints.MinHeight = 136
  Constraints.MinWidth = 394
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 20
  object lv: TListView
    Left = 0
    Top = 41
    Width = 777
    Height = 545
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        Caption = 'Done'
        Width = 56
      end
      item
        Caption = 'Priority'
        Width = 61
      end
      item
        Caption = 'Description'
        Width = 351
      end
      item
        Caption = 'Filename'
        Width = 192
      end
      item
        Caption = 'User'
        Width = 120
      end>
    ReadOnly = True
    RowSelect = True
    SortType = stBoth
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = lvColumnClick
    OnCompare = lvCompare
    OnCustomDrawItem = lvCustomDrawItem
    OnCustomDrawSubItem = lvCustomDrawSubItem
    OnDblClick = lvDblClick
    OnMouseDown = lvMouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 777
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      777
      41)
    object lblFilter: TLabel
      Left = 27
      Top = 8
      Width = 36
      Height = 20
      Anchors = [akLeft, akBottom]
      Caption = 'Filter:'
    end
    object cmbFilter: TComboBox
      Left = 136
      Top = 8
      Width = 449
      Height = 28
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
      ItemHeight = 20
      TabOrder = 0
      OnChange = cmbFilterChange
      Items.Strings = (
        'All files (in project and not)'
        'Open files only (in project and not)'
        'All project files'
        'Open project files only'
        'Non-project open files'
        'Current file only')
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 586
    Width = 777
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      777
      33)
    object chkNoDone: TCheckBox
      Left = 11
      Top = 8
      Width = 385
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Don'#39't show items marked as done'
      TabOrder = 0
      OnClick = chkNoDoneClick
    end
    object btnClose: TButton
      Left = 665
      Top = -1
      Width = 100
      Height = 34
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Close'
      TabOrder = 1
      OnClick = btnCloseClick
    end
  end
end
