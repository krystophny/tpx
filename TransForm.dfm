object TransfForm: TTransfForm
  Left = 275
  Top = 125
  BorderStyle = bsSingle
  Caption = 'Custom transform'
  ClientHeight = 347
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 272
    Width = 33
    Height = 16
    Caption = 'Skew'
  end
  object Button1: TButton
    Left = 264
    Top = 312
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 352
    Top = 312
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object LabeledEdit1: TLabeledEdit
    Left = 12
    Top = 24
    Width = 121
    Height = 24
    EditLabel.Width = 208
    EditLabel.Height = 16
    EditLabel.Caption = 'Rotate counterclockwise (degrees)'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 2
  end
  object LabeledEdit2: TLabeledEdit
    Left = 12
    Top = 72
    Width = 121
    Height = 24
    EditLabel.Width = 36
    EditLabel.Height = 16
    EditLabel.Caption = 'Shift X'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 3
  end
  object LabeledEdit3: TLabeledEdit
    Left = 148
    Top = 72
    Width = 121
    Height = 24
    EditLabel.Width = 37
    EditLabel.Height = 16
    EditLabel.Caption = 'Shift Y'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 4
  end
  object LabeledEdit4: TLabeledEdit
    Left = 12
    Top = 128
    Width = 121
    Height = 24
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'Scale X'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 5
  end
  object LabeledEdit5: TLabeledEdit
    Left = 148
    Top = 128
    Width = 121
    Height = 24
    EditLabel.Width = 47
    EditLabel.Height = 16
    EditLabel.Caption = 'Scale Y'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 6
  end
  object RadioGroup1: TRadioGroup
    Left = 312
    Top = 104
    Width = 133
    Height = 53
    Caption = 'Scale'
    ItemIndex = 0
    Items.Strings = (
      'Grow'
      'Shrink')
    TabOrder = 7
  end
  object RadioGroup2: TRadioGroup
    Left = 16
    Top = 164
    Width = 297
    Height = 85
    Caption = 'Reference point'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Center'
      'Lower left'
      'Upper left'
      'Lower right'
      'Upper right'
      'User defined')
    TabOrder = 8
    OnClick = RadioGroup2Click
  end
  object LabeledEdit6: TLabeledEdit
    Left = 324
    Top = 180
    Width = 121
    Height = 24
    EditLabel.Width = 67
    EditLabel.Height = 16
    EditLabel.Caption = 'Ref. point X'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 9
  end
  object LabeledEdit7: TLabeledEdit
    Left = 324
    Top = 224
    Width = 121
    Height = 24
    EditLabel.Width = 68
    EditLabel.Height = 16
    EditLabel.Caption = 'Ref. point Y'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 10
  end
  object LabeledEdit8: TLabeledEdit
    Left = 68
    Top = 276
    Width = 121
    Height = 24
    EditLabel.Width = 96
    EditLabel.Height = 16
    EditLabel.Caption = 'Horiz. (degrees)'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 11
  end
  object LabeledEdit9: TLabeledEdit
    Left = 204
    Top = 276
    Width = 121
    Height = 24
    EditLabel.Width = 89
    EditLabel.Height = 16
    EditLabel.Caption = 'Vert. (degrees)'
    LabelPosition = lpAbove
    LabelSpacing = 3
    TabOrder = 12
  end
end
