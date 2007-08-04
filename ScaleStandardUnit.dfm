object ScaleStandardForm: TScaleStandardForm
  Left = 259
  Top = 125
  BorderStyle = bsDialog
  Caption = 'Scale standard'
  ClientHeight = 242
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 32
    Width = 93
    Height = 16
    Caption = 'Width limit (mm)'
  end
  object Label2: TLabel
    Left = 152
    Top = 32
    Width = 98
    Height = 16
    Caption = 'Height limit (mm)'
  end
  object Label3: TLabel
    Left = 16
    Top = 120
    Width = 128
    Height = 16
    Caption = 'PicScale, mm per unit'
  end
  object Edit1: TEdit
    Left = 16
    Top = 52
    Width = 121
    Height = 24
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 152
    Top = 52
    Width = 121
    Height = 24
    TabOrder = 1
    Text = 'Edit2'
  end
  object Button1: TButton
    Left = 64
    Top = 208
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 144
    Top = 208
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ScalePhysical: TCheckBox
    Left = 16
    Top = 180
    Width = 197
    Height = 17
    Caption = 'Scale physical units'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object RadioButton1: TRadioButton
    Left = 16
    Top = 8
    Width = 113
    Height = 17
    Caption = 'Scale to limits'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 16
    Top = 96
    Width = 305
    Height = 17
    Caption = 'Change picture scale'
    TabOrder = 6
    OnClick = RadioButton1Click
  end
  object Edit3: TEdit
    Left = 16
    Top = 140
    Width = 121
    Height = 24
    TabOrder = 7
    Text = 'Edit3'
  end
end
