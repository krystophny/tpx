object ScaleStandardForm: TScaleStandardForm
  Left = 259
  Top = 125
  BorderStyle = bsDialog
  Caption = 'Scale standard'
  ClientHeight = 188
  ClientWidth = 235
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 93
    Height = 16
    Caption = 'Width limit (mm)'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 98
    Height = 16
    Caption = 'Height limit (mm)'
  end
  object Edit1: TEdit
    Left = 16
    Top = 28
    Width = 121
    Height = 24
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 16
    Top = 84
    Width = 121
    Height = 24
    TabOrder = 1
    Text = 'Edit2'
  end
  object Button1: TButton
    Left = 64
    Top = 160
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 144
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ScalePhysical: TCheckBox
    Left = 16
    Top = 124
    Width = 197
    Height = 17
    Caption = 'Scale physical units'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
end
