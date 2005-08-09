object PropertiesForm: TPropertiesForm
  Left = 435
  Top = 222
  BorderStyle = bsSingle
  Caption = 'Primitive properties'
  ClientHeight = 401
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object PropPages: TPageControl
    Left = 0
    Top = 101
    Width = 460
    Height = 267
    ActivePage = TextSheet
    Align = alClient
    MultiLine = True
    TabHeight = 1
    TabIndex = 2
    TabOrder = 0
    object VoidSheet: TTabSheet
      Caption = ' '
      ImageIndex = 2
    end
    object LineSheet: TTabSheet
      Caption = 'Line'
      object CheckBox1: TCheckBox
        Left = 28
        Top = 20
        Width = 97
        Height = 17
        Caption = 'Arrow 1'
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 28
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Arrow 2'
        TabOrder = 1
      end
      object SpinEdit1: TSpinEdit
        Left = 92
        Top = 44
        Width = 121
        Height = 26
        MaxValue = 8
        MinValue = 0
        TabOrder = 2
        Value = 2
      end
      object SpinEdit2: TSpinEdit
        Left = 92
        Top = 116
        Width = 121
        Height = 26
        MaxValue = 8
        MinValue = 0
        TabOrder = 3
        Value = 2
      end
    end
    object TextSheet: TTabSheet
      Caption = 'Text'
      ImageIndex = 1
      object LabeledEdit1: TLabeledEdit
        Left = 20
        Top = 20
        Width = 313
        Height = 24
        EditLabel.Width = 26
        EditLabel.Height = 16
        EditLabel.Caption = 'Text'
        LabelPosition = lpAbove
        LabelSpacing = 3
        TabOrder = 0
      end
      object LabeledEdit2: TLabeledEdit
        Left = 20
        Top = 120
        Width = 121
        Height = 24
        EditLabel.Width = 39
        EditLabel.Height = 16
        EditLabel.Caption = 'Height'
        LabelPosition = lpAbove
        LabelSpacing = 3
        TabOrder = 2
      end
      object RadioGroup1: TRadioGroup
        Left = 20
        Top = 152
        Width = 125
        Height = 85
        Caption = 'H. justification'
        ItemIndex = 0
        Items.Strings = (
          'Left'
          'Center'
          'Right')
        TabOrder = 3
      end
      object RadioGroup2: TRadioGroup
        Left = 192
        Top = 152
        Width = 125
        Height = 101
        Caption = 'V. justification'
        ItemIndex = 0
        Items.Strings = (
          'Baseline'
          'Bottom'
          'Center'
          'Top')
        TabOrder = 4
      end
      object LabeledEdit3: TLabeledEdit
        Left = 20
        Top = 68
        Width = 313
        Height = 24
        EditLabel.Width = 48
        EditLabel.Height = 16
        EditLabel.Caption = 'TeX text'
        LabelPosition = lpAbove
        LabelSpacing = 3
        TabOrder = 1
      end
      object LabeledEdit4: TLabeledEdit
        Left = 164
        Top = 120
        Width = 121
        Height = 24
        EditLabel.Width = 35
        EditLabel.Height = 16
        EditLabel.Caption = 'Angle'
        LabelPosition = lpAbove
        LabelSpacing = 3
        TabOrder = 5
      end
    end
    object StarsSheet: TTabSheet
      Caption = 'Stars'
      ImageIndex = 3
      inline StarsFrame1: TStarsFrame
        Left = 66
        Top = 31
        Width = 134
        Height = 68
        AutoSize = True
        TabOrder = 0
        inherited ToolBar1: TToolBar
          Height = 68
          EdgeBorders = [ebLeft]
          Indent = 5
          inherited ToolButton1: TToolButton
            Left = 5
            Down = True
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton2: TToolButton
            Left = 28
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton3: TToolButton
            Left = 51
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton4: TToolButton
            Left = 74
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton5: TToolButton
            Left = 5
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton6: TToolButton
            Left = 28
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton7: TToolButton
            Left = 51
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton8: TToolButton
            Left = 74
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton9: TToolButton
            Left = 5
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton10: TToolButton
            Left = 28
            Grouped = True
            Style = tbsCheck
          end
          inherited ToolButton11: TToolButton
            Left = 51
            Grouped = True
            Style = tbsCheck
          end
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 368
    Width = 460
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    object Button1: TButton
      Left = 200
      Top = 4
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 284
      Top = 4
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Button3: TButton
      Left = 20
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Points'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 460
    Height = 101
    Align = alTop
    Caption = ' '
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 12
      Width = 25
      Height = 16
      Caption = 'Line'
    end
    object Label2: TLabel
      Left = 16
      Top = 40
      Width = 53
      Height = 16
      Caption = 'Hatching'
    end
    object Label3: TLabel
      Left = 16
      Top = 72
      Width = 17
      Height = 16
      Caption = 'Fill'
    end
    object ComboBox1: TComboBox
      Left = 84
      Top = 8
      Width = 100
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      TabOrder = 0
      Items.Strings = (
        'None'
        'Solid'
        'Dotted'
        'Dashed')
    end
    object ComboBox2: TComboBox
      Left = 84
      Top = 40
      Width = 100
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      ItemIndex = 0
      TabOrder = 1
      Text = 'None'
      Items.Strings = (
        'None'
        'Horizontal'
        'Vertical'
        'FDiagonal'
        'BDiagonal'
        'Cross'
        'DiagCross')
    end
    object ComboBox3: TComboBox
      Left = 188
      Top = 8
      Width = 157
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 2
      OnDrawItem = ColorBox_DrawItem
      OnSelect = ColorBox_Select
    end
    object ComboBox4: TComboBox
      Left = 188
      Top = 40
      Width = 157
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 3
      OnDrawItem = ColorBox_DrawItem
      OnSelect = ColorBox_Select
    end
    object ComboBox5: TComboBox
      Left = 84
      Top = 72
      Width = 157
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 4
      OnDrawItem = ColorBox_DrawItem
      OnSelect = ColorBox_Select
    end
    object ComboBox6: TComboBox
      Left = 348
      Top = 8
      Width = 100
      Height = 24
      ItemHeight = 16
      TabOrder = 5
      Text = ' '
      Items.Strings = (
        '0.5'
        '1'
        '2'
        '4')
    end
  end
end
