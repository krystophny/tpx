object SettingsForm: TSettingsForm
  Left = 259
  Top = 125
  Width = 469
  Height = 490
  Caption = 'TpX Settings'
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
    Width = 461
    Height = 371
    Align = alClient
    TabOrder = 0
    OnEditButtonClick = VLEEditButtonClick
    OnGetPickList = VLEGetPickList
    OnSelectCell = VLESelectCell
    ColWidths = (
      161
      294)
  end
  object Panel1: TPanel
    Left = 0
    Top = 423
    Width = 461
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
    Top = 371
    Width = 461
    Height = 52
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
  object OpenDialog1: TOpenDialog
    Left = 248
    Top = 4
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Options = [fdTrueTypeOnly, fdNoSizeSel, fdNoStyleSel]
    Left = 284
    Top = 8
  end
end
