object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Delphi Excercise'
  ClientHeight = 300
  ClientWidth = 403
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 92
    Height = 13
    Caption = 'Please select a file:'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 78
    Height = 13
    Caption = 'Type of Widget:'
  end
  object ResultListBox: TListBox
    Left = 8
    Top = 64
    Width = 387
    Height = 228
    Columns = 2
    ItemHeight = 13
    TabOrder = 0
  end
  object DirectoryTxtBox: TEdit
    Left = 106
    Top = 10
    Width = 208
    Height = 21
    TabOrder = 1
  end
  object BrowseBtn: TButton
    Left = 320
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Browse'
    TabOrder = 2
    OnClick = BrowseBtnClick
  end
  object WidgetComboBox: TComboBox
    Left = 106
    Top = 37
    Width = 208
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    Items.Strings = (
      'TBlueWidget'
      'TRedWidget')
  end
  object LoadBtn: TButton
    Left = 320
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Load File'
    TabOrder = 4
    OnClick = LoadBtnClick
  end
end
