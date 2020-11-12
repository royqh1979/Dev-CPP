object FunctionSearchForm: TFunctionSearchForm
  Left = 694
  Top = 563
  BorderStyle = bsDialog
  Caption = 'Goto function...'
  ClientHeight = 503
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    625
    503)
  PixelsPerInch = 120
  TextHeight = 20
  object lblSearch: TLabel
    Left = 5
    Top = 11
    Width = 70
    Height = 20
    Caption = 'Search for:'
  end
  object lvEntries: TListView
    Left = 0
    Top = 43
    Width = 625
    Height = 460
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    Columns = <
      item
        Width = 40
      end
      item
        Caption = 'Type'
        Width = 67
      end
      item
        Caption = 'Function'
        Width = 400
      end
      item
        Caption = 'Line'
        Width = 67
      end>
    ColumnClick = False
    HideSelection = False
    LargeImages = dmMain.ClassImages
    ReadOnly = True
    RowSelect = True
    SmallImages = dmMain.ClassImages
    StateImages = dmMain.ClassImages
    TabOrder = 0
    TabStop = False
    ViewStyle = vsReport
    OnCompare = lvEntriesCompare
    OnDblClick = lvEntriesDblClick
  end
  object txtSearch: TEdit
    Left = 112
    Top = 5
    Width = 505
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = txtSearchChange
    OnKeyDown = txtSearchKeyDown
    OnKeyPress = txtSearchKeyPress
  end
end
