object RemoveUnitForm: TRemoveUnitForm
  Left = 798
  Top = 405
  Width = 362
  Height = 325
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Remove from project'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    344
    278)
  PixelsPerInch = 120
  TextHeight = 16
  object DelBtn: TButton
    Left = 242
    Top = 240
    Width = 93
    Height = 31
    Anchors = [akRight, akBottom]
    Caption = 'Delete'
    TabOrder = 0
    OnClick = DelBtnClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 345
    Height = 233
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object UnitList: TListBox
      Left = 0
      Top = 0
      Width = 345
      Height = 233
      Align = alClient
      ItemHeight = 16
      MultiSelect = True
      TabOrder = 0
      OnKeyPress = UnitListKeyPress
    end
  end
end
