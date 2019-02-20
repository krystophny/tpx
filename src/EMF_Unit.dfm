object EMF_Form: TEMF_Form
  Left = 328
  Top = 200
  Caption = 'Image tool'
  ClientHeight = 516
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object ScrollBox1: TScrollBox
    Left = 185
    Top = 29
    Width = 495
    Height = 462
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 503
    ExplicitHeight = 449
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 499
      Height = 445
      Align = alClient
      Proportional = True
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 680
    Height = 29
    ButtonHeight = 26
    Caption = 'ToolBar1'
    TabOrder = 1
    ExplicitWidth = 688
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Action = Open
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 2
      Action = SaveAs
    end
    object ToolButton8: TToolButton
      Left = 46
      Top = 2
      Action = PrintAsEps
    end
    object ToolButton3: TToolButton
      Left = 69
      Top = 2
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 23
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 77
      Top = 2
      Action = ClipboardCopy
    end
    object ToolButton5: TToolButton
      Left = 100
      Top = 2
      Action = ClipboardPaste
    end
    object ToolButton6: TToolButton
      Left = 123
      Top = 2
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 39
      Style = tbsSeparator
    end
    object Panel1: TPanel
      Left = 131
      Top = 2
      Width = 109
      Height = 26
      Caption = ' '
      TabOrder = 0
      object CheckBox1: TCheckBox
        Left = 7
        Top = 3
        Width = 97
        Height = 17
        Action = FitWindow
        State = cbChecked
        TabOrder = 0
      end
    end
    object ToolButton7: TToolButton
      Left = 240
      Top = 2
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 40
      Style = tbsSeparator
    end
    object SpinEdit1: TSpinEdit
      Left = 248
      Top = 2
      Width = 65
      Height = 26
      Increment = 10
      MaxValue = 10000
      MinValue = 0
      TabOrder = 1
      Value = 100
      OnChange = SpinEdit1Change
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 29
    Width = 185
    Height = 462
    Align = alLeft
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 2
    ExplicitHeight = 449
    object ListView1: TListView
      Left = 0
      Top = 33
      Width = 185
      Height = 416
      Align = alClient
      Checkboxes = True
      Columns = <
        item
          AutoSize = True
        end>
      ColumnClick = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnKeyUp = ListView1KeyUp
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      Caption = ' '
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 1
      object SpeedButton1: TSpeedButton
        Left = 166
        Top = 4
        Width = 17
        Height = 17
        Flat = True
        Glyph.Data = {
          EE000000424DEE0000000000000076000000280000000F0000000F0000000100
          0400000000007800000074120000741200001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
          8880888888888888888088888888888888808888888888888880888008888008
          8880888800880088888088888000088888808888880088888880888880000888
          8880888800880088888088800888800888808888888888888880888888888888
          888088888888888888808888888888888880}
        Layout = blGlyphBottom
        OnClick = SpeedButton1Click
      end
      object Button5: TButton
        Left = 0
        Top = 4
        Width = 85
        Height = 25
        Caption = 'Contents'
        TabOrder = 0
        OnClick = ListEmfContentsExecute
      end
      object Button6: TButton
        Left = 88
        Top = 4
        Width = 75
        Height = 25
        Caption = '>>'
        TabOrder = 1
        OnClick = EmfFromListExecute
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 497
    Width = 680
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ParentFont = True
    UseSystemFont = False
    ExplicitTop = 484
    ExplicitWidth = 688
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 491
    Width = 680
    Height = 6
    Align = alBottom
    Position = 50
    Step = 1
    TabOrder = 4
    ExplicitTop = 478
    ExplicitWidth = 688
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Metafiles (*.wmf,*.emf)|*.wmf;*.emf|Bitmaps (*.bmp)|*.bmp'
    FilterIndex = 0
    Left = 280
    Top = 56
  end
  object ActionList1: TActionList
    Left = 236
    Top = 52
    object Open: TAction
      Category = 'File'
      Caption = 'Open'
      ImageIndex = 13
      ShortCut = 16463
      OnExecute = OpenExecute
    end
    object SaveAs: TAction
      Category = 'File'
      Caption = 'Save as'
      ImageIndex = 22
      ShortCut = 16467
      OnExecute = SaveAsExecute
    end
    object PrintAsEps: TAction
      Category = 'File'
      Caption = 'Print as EPS'
      ImageIndex = 57
      ShortCut = 16464
      OnExecute = PrintAsEpsExecute
    end
    object CloseWindow: TAction
      Category = 'File'
      Caption = 'Close'
      ShortCut = 27
      OnExecute = CloseWindowExecute
    end
    object ClipboardCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      ImageIndex = 37
      ShortCut = 16451
      OnExecute = ClipboardCopyExecute
    end
    object ClipboardPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      ImageIndex = 38
      ShortCut = 16470
      OnExecute = ClipboardPasteExecute
    end
    object FitWindow: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Fit window'
      Checked = True
      OnExecute = FitWindowExecute
    end
    object ListEmfContents: TAction
      Category = 'View'
      Caption = 'List EMF contents'
      OnExecute = ListEmfContentsExecute
    end
    object EmfFromList: TAction
      Category = 'Edit'
      Caption = 'EMF from list'
      OnExecute = EmfFromListExecute
    end
    object ShowEmfPanel: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Show EMF panel'
      Checked = True
      OnExecute = ShowEmfPanelExecute
    end
    object SetPreferences: TAction
      Category = 'File'
      Caption = 'Preferences'
      OnExecute = SetPreferencesExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 204
    Top = 56
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Action = Open
      end
      object Saveas1: TMenuItem
        Action = SaveAs
      end
      object PrintasEPS1: TMenuItem
        Action = PrintAsEps
      end
      object Preferences1: TMenuItem
        Action = SetPreferences
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Action = CloseWindow
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Copy1: TMenuItem
        Action = ClipboardCopy
      end
      object Paste1: TMenuItem
        Action = ClipboardPaste
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object EMFfromlist1: TMenuItem
        Action = EmfFromList
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Fitwindow1: TMenuItem
        Action = FitWindow
        AutoCheck = True
      end
      object ListEMFcontents1: TMenuItem
        Action = ListEmfContents
      end
      object ShowEMFpanel1: TMenuItem
        Action = ShowEmfPanel
        AutoCheck = True
      end
    end
  end
  object PrintDialog1: TPrintDialog
    Left = 360
    Top = 56
  end
  object SaveDialog1: TSaveDialog
    Left = 396
    Top = 56
  end
end
