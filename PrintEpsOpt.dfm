object PrintEpsOptForm: TPrintEpsOptForm
  Left = 506
  Top = 357
  BorderStyle = bsDialog
  Caption = 'EPS print preferences'
  ClientHeight = 241
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 12
    Top = 33
    Width = 38
    Height = 16
    Caption = 'Printer'
  end
  object Label2: TLabel
    Left = 12
    Top = 100
    Width = 58
    Height = 16
    Caption = 'Scale (%)'
  end
  object Label3: TLabel
    Left = 12
    Top = 136
    Width = 91
    Height = 16
    Caption = 'Max width (mm)'
    Visible = False
  end
  object Label4: TLabel
    Left = 12
    Top = 168
    Width = 97
    Height = 16
    Caption = 'Max height (mm)'
    Visible = False
  end
  object OKBtn: TButton
    Left = 141
    Top = 202
    Width = 93
    Height = 30
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 248
    Top = 202
    Width = 92
    Height = 30
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Button1: TButton
    Left = 324
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 2
    OnClick = Button1Click
  end
  object PrinterEdit: TEdit
    Left = 80
    Top = 32
    Width = 233
    Height = 24
    TabOrder = 3
    Text = 'none'
  end
  object UseOffsetBox: TCheckBox
    Left = 24
    Top = 68
    Width = 217
    Height = 17
    Caption = 'UseOffset'
    TabOrder = 4
  end
  object SpinEdit1: TSpinEdit
    Left = 100
    Top = 96
    Width = 101
    Height = 26
    Increment = 10
    MaxValue = 10000
    MinValue = 0
    TabOrder = 5
    Value = 100
  end
  object Edit1: TEdit
    Left = 128
    Top = 132
    Width = 121
    Height = 24
    TabOrder = 6
    Text = '500'
    Visible = False
  end
  object Edit2: TEdit
    Left = 128
    Top = 164
    Width = 121
    Height = 24
    TabOrder = 7
    Text = '500'
    Visible = False
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 12
    Top = 8
  end
end
