object OptionsForm: TOptionsForm
  Left = 259
  Top = 125
  Width = 476
  Height = 502
  Caption = 'Picture properties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object VLE: TValueListEditor
    Left = 0
    Top = 0
    Width = 468
    Height = 334
    Align = alClient
    TabOrder = 0
    OnEditButtonClick = VLEEditButtonClick
    OnGetPickList = VLEGetPickList
    OnSelectCell = VLESelectCell
    ColWidths = (
      161
      301)
  end
  object Panel1: TPanel
    Left = 0
    Top = 434
    Width = 468
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object Button1: TButton
      Left = 290
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 370
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 334
    Width = 468
    Height = 100
    Align = alBottom
    BevelInner = bvNone
    BevelOuter = bvNone
    Color = clBtnFace
    Ctl3D = False
    Lines.Strings = (
      ' ')
    ParentCtl3D = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Options = [fdTrueTypeOnly]
    Left = 132
    Top = 116
  end
end
