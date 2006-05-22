object TableForm: TTableForm
  Left = 259
  Top = 125
  Width = 461
  Height = 501
  Caption = 'Edit coordinates'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Button1: TButton
    Left = 340
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 340
    Top = 36
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 335
    Top = 92
    Width = 86
    Height = 101
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 2
    object Button3: TButton
      Left = 5
      Top = 0
      Width = 75
      Height = 25
      Action = AddPoints
      TabOrder = 0
    end
    object SpinEdit1: TSpinEdit
      Left = 5
      Top = 28
      Width = 73
      Height = 26
      MaxValue = 10000
      MinValue = 1
      TabOrder = 1
      Value = 1
    end
    object RadioButton1: TRadioButton
      Left = 5
      Top = 60
      Width = 73
      Height = 17
      Caption = 'above'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 5
      Top = 80
      Width = 73
      Height = 17
      Caption = 'below'
      TabOrder = 3
    end
  end
  object CheckBox1: TCheckBox
    Left = 340
    Top = 216
    Width = 105
    Height = 17
    Caption = 'Inplace editor'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 333
    Height = 473
    Align = alLeft
    Caption = 'Panel2'
    TabOrder = 4
    object Grid: TStringGrid
      Left = 1
      Top = 25
      Width = 331
      Height = 447
      Align = alClient
      ColCount = 3
      DefaultColWidth = 100
      DefaultRowHeight = 20
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowMoving, goEditing]
      PopupMenu = PopupMenu1
      ScrollBars = ssVertical
      TabOrder = 0
      OnClick = GridClick
      OnKeyUp = GridKeyUp
      OnRowMoved = GridRowMoved
      OnSelectCell = GridSelectCell
      OnSetEditText = GridSetEditText
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 331
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 1
      object Edit1: TEdit
        Left = 0
        Top = 0
        Width = 329
        Height = 24
        TabOrder = 0
        OnChange = Edit1Change
        OnEnter = Edit1Enter
      end
    end
  end
  object ActionList1: TActionList
    Left = 120
    Top = 104
    object DeletePoints: TAction
      Caption = 'Delete points'
      OnExecute = DeletePointsExecute
    end
    object AddPoints: TAction
      Caption = 'Add points'
      ShortCut = 45
      OnExecute = AddPointsExecute
    end
    object Copy: TAction
      Caption = 'Copy'
      ShortCut = 16451
      SecondaryShortCuts.Strings = (
        'ctrl+ins')
      OnExecute = CopyExecute
    end
    object Paste: TAction
      Caption = 'Paste'
      ShortCut = 16470
      SecondaryShortCuts.Strings = (
        'shift+ins')
      OnExecute = PasteExecute
    end
    object SelectAll: TAction
      Caption = 'Select all'
      ShortCut = 16449
      OnExecute = SelectAllExecute
    end
    object AddFromClipboard: TAction
      Caption = 'Add from clipboard'
      ShortCut = 49238
      OnExecute = AddFromClipboardExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 184
    Top = 96
    object Copy1: TMenuItem
      Action = Copy
    end
    object Paste1: TMenuItem
      Action = Paste
    end
    object AddFromClipboard1: TMenuItem
      Action = AddFromClipboard
    end
    object Selectall1: TMenuItem
      Action = SelectAll
    end
    object Addpoints1: TMenuItem
      Action = AddPoints
    end
    object Deletepoints1: TMenuItem
      Action = DeletePoints
    end
  end
end
