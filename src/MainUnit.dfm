object MainForm: TMainForm
  Left = 362
  Top = 196
  HelpType = htKeyword
  HelpContext = 101
  Caption = 'TpX Editor'
  ClientHeight = 513
  ClientWidth = 646
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDefault
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 13
  object Image1: TImage
    Left = 233
    Top = 2
    Width = 17
    Height = 22
    Center = True
    Picture.Data = {
      07544269746D6170F6000000424DF60000000000000076000000280000001000
      0000100000000100040000000000800000007412000074120000100000000000
      0000000000000000800000800000008080008000000080008000808000008080
      8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00888888888888888888888008800888888888880088008888888008800880
      0888888800880088008880088008800880088800880088008808888008800880
      0888888800880088008880088008800880088800880088008808888008800880
      0888888800880088088888888008800888888888880088088888888888808888
      8888}
    Transparent = True
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 498
    Width = 646
    Height = 15
    Panels = <
      item
        Width = 150
      end
      item
        Width = 50
      end>
    ParentFont = True
    UseSystemFont = False
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 492
    Width = 646
    Height = 6
    Align = alBottom
    Position = 50
    Smooth = True
    Step = 1
    TabOrder = 0
  end
  object Panel30: TPanel
    Left = 0
    Top = 0
    Width = 646
    Height = 68
    Align = alTop
    AutoSize = True
    BevelInner = bvRaised
    BevelOuter = bvNone
    TabOrder = 2
    object ToolBar1: TToolBar
      Left = 1
      Top = 1
      Width = 644
      Height = 22
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = True
      Customizable = True
      DragKind = dkDock
      EdgeBorders = [ebTop, ebBottom]
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = ImageList2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object ToolButton9: TToolButton
        Left = 0
        Top = 0
        Action = NewDoc
      end
      object ToolButton10: TToolButton
        Left = 23
        Top = 0
        Action = OpenDoc
      end
      object ToolButton11: TToolButton
        Left = 46
        Top = 0
        Action = SaveDoc
      end
      object ToolButton13: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'ToolButton13'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object BasicModeBtn: TToolButton
        Left = 77
        Top = 0
        Hint = 'Basic mode'
        Caption = 'BasicModeBtn'
        ImageIndex = 21
        OnClick = BasicModeExecute
      end
      object AreaSelectBtn: TToolButton
        Left = 100
        Top = 0
        Action = AreaSelect
      end
      object ToolButton17: TToolButton
        Left = 123
        Top = 0
        Width = 8
        Caption = 'ToolButton17'
        ImageIndex = 40
        Style = tbsSeparator
      end
      object ClipboardCutBtn: TToolButton
        Left = 131
        Top = 0
        Action = ClipboardCut
      end
      object ClipboardCopyBtn: TToolButton
        Left = 154
        Top = 0
        Action = ClipboardCopy
      end
      object ClipboardPasteBtn: TToolButton
        Left = 177
        Top = 0
        Action = ClipboardPaste
      end
      object ToolButton7: TToolButton
        Left = 200
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object UndoBtn: TToolButton
        Left = 208
        Top = 0
        Action = Undo
      end
      object RedoBtn: TToolButton
        Left = 231
        Top = 0
        Action = Redo
      end
      object ToolButton14: TToolButton
        Left = 254
        Top = 0
        Width = 8
        Caption = 'ToolButton14'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ZoomAreaBtn: TToolButton
        Left = 262
        Top = 0
        Action = ZoomArea
      end
      object ToolButton3: TToolButton
        Left = 285
        Top = 0
        Action = ZoomIn
      end
      object ToolButton5: TToolButton
        Left = 308
        Top = 0
        Action = ZoomOut
      end
      object ToolButton6: TToolButton
        Left = 331
        Top = 0
        Action = ZoomAll
      end
      object PanningBtn: TToolButton
        Left = 354
        Top = 0
        Action = HandTool
      end
      object ToolButton12: TToolButton
        Left = 377
        Top = 0
        Width = 8
        Caption = 'ToolButton12'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ToolButton8: TToolButton
        Left = 385
        Top = 0
        Action = ImageTool
      end
      object ToolButton21: TToolButton
        Left = 408
        Top = 0
        Width = 8
        Caption = 'ToolButton21'
        ImageIndex = 59
        Style = tbsSeparator
      end
      object ToolButton18: TToolButton
        Left = 416
        Top = 0
        Action = PreviewLaTeX
      end
      object ToolButton20: TToolButton
        Left = 439
        Top = 0
        Action = PreviewLaTeX_PS
      end
      object ToolButton1: TToolButton
        Left = 462
        Top = 0
        Action = TeXFormat
      end
      object ToolButton19: TToolButton
        Left = 485
        Top = 0
        Action = PreviewPdfLaTeX
      end
      object ToolButton2: TToolButton
        Left = 508
        Top = 0
        Action = PdfTeXFormat
      end
      object ToolButton15: TToolButton
        Left = 531
        Top = 0
        Width = 8
        Caption = 'ToolButton15'
        ImageIndex = 66
        Style = tbsSeparator
        Visible = False
      end
      object ToolButton4: TToolButton
        Left = 539
        Top = 0
        Action = SimplifyPoly
        Visible = False
      end
      object ToolButton16: TToolButton
        Left = 562
        Top = 0
        Caption = 'ToolButton16'
        ImageIndex = 74
        Visible = False
        OnClick = ToolButton16Click
      end
    end
    object PropertiesToolbar1: TToolBar
      Left = 1
      Top = 23
      Width = 644
      Height = 22
      AutoSize = True
      Caption = 'PropertiesToolbar1'
      Customizable = True
      DragKind = dkDock
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = ImageList2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object ToolButton24: TToolButton
        Left = 0
        Top = 0
        Action = PickUpProperties
      end
      object ToolButton26: TToolButton
        Left = 23
        Top = 0
        Action = DefaultProperties
      end
      object ToolButton27: TToolButton
        Left = 46
        Top = 0
        Action = ApplyProperties
      end
      object ToolButton25: TToolButton
        Left = 69
        Top = 0
        Width = 10
        Caption = 'ToolButton25'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object Panel4: TPanel
        Left = 79
        Top = 0
        Width = 24
        Height = 22
        BevelOuter = bvNone
        Caption = ' '
        TabOrder = 6
        object Image3: TImage
          Left = 0
          Top = 0
          Width = 24
          Height = 22
          Align = alClient
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000076000000280000001000
            0000100000000100040000000000800000007412000074120000100000000000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00888888888888888888888888888888888000880008800088888888888888
            888888044478888888888880F4448888888888880F444888888888880F444488
            8888888880F444488888888880FF444888888888880FF4488888888888800008
            8888888888870BB08888888888880BB088888888888880BB0888888888888888
            8888}
          Transparent = True
        end
      end
      object ComboBox1: TComboBox
        Left = 103
        Top = 0
        Width = 63
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnSelect = ChangeProperties
        Items.Strings = (
          'None'
          'Solid'
          'Dotted'
          'Dashed')
      end
      object ComboBox3: TComboBox
        Left = 166
        Top = 0
        Width = 72
        Height = 22
        Style = csOwnerDrawFixed
        TabOrder = 1
        OnDrawItem = ColorBox_DrawItem
        OnSelect = ChangeProperties
      end
      object ComboBox6: TComboBox
        Left = 238
        Top = 0
        Width = 63
        Height = 21
        TabOrder = 5
        OnChange = ChangeProperties
        Items.Strings = (
          '0.5'
          '1'
          '2'
          '4')
      end
      object ToolButton22: TToolButton
        Left = 301
        Top = 0
        Width = 10
        Caption = 'ToolButton22'
        Style = tbsSeparator
      end
      object Panel5: TPanel
        Left = 311
        Top = 0
        Width = 24
        Height = 22
        BevelOuter = bvNone
        Caption = ' '
        TabOrder = 7
        object Image4: TImage
          Left = 0
          Top = 0
          Width = 24
          Height = 22
          Align = alClient
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000076000000280000001000
            0000100000000100040000000000800000007412000074120000100000000000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00888888888888888888888008800888888888880088008888888008800880
            0888888800880088008880088008800880088800880088008808888008800880
            0888888800880088008880088008800880088800880088008808888008800880
            0888888800880088088888888008800888888888880088088888888888808888
            8888}
          Transparent = True
        end
      end
      object ComboBox2: TComboBox
        Left = 335
        Top = 0
        Width = 68
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = 'None'
        OnSelect = ChangeProperties
        Items.Strings = (
          'None'
          'Horizontal'
          'Vertical'
          'FDiagonal'
          'BDiagonal'
          'Cross'
          'DiagCross')
      end
      object ComboBox4: TComboBox
        Left = 403
        Top = 0
        Width = 72
        Height = 22
        Style = csOwnerDrawFixed
        TabOrder = 3
        OnDrawItem = ColorBox_DrawItem
        OnSelect = ChangeProperties
      end
      object ToolButton23: TToolButton
        Left = 475
        Top = 0
        Width = 10
        Caption = 'ToolButton23'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object Panel6: TPanel
        Left = 485
        Top = 0
        Width = 24
        Height = 22
        BevelOuter = bvNone
        Caption = ' '
        TabOrder = 8
        object Image2: TImage
          Left = 0
          Top = 0
          Width = 24
          Height = 22
          Align = alClient
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000076000000280000001000
            0000100000000100040000000000800000007412000074120000100000000000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00888888888888888888888888888888888888880888888888888880708888
            888888880777088878888880888770884788880F888877084488808FF8888774
            44888808FF808874448888808F0408444488888808F487444888888840847078
            8888888848040788888888884884788888888888744788888888888888888888
            8888}
          Transparent = True
        end
      end
      object ComboBox5: TComboBox
        Left = 509
        Top = 0
        Width = 72
        Height = 22
        Style = csOwnerDrawFixed
        TabOrder = 4
        OnDrawItem = ColorBox_DrawItem
        OnSelect = ChangeProperties
      end
    end
    object PropertiesToolbar2: TToolBar
      Left = 1
      Top = 45
      Width = 644
      Height = 22
      ButtonHeight = 21
      Caption = 'PropertiesToolbar2'
      Customizable = True
      DragKind = dkDock
      EdgeInner = esNone
      EdgeOuter = esNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 24
        Height = 21
        BevelOuter = bvNone
        Caption = ' '
        TabOrder = 3
        object Image5: TImage
          Left = 0
          Top = 0
          Width = 24
          Height = 21
          Align = alClient
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000076000000280000001000
            0000100000000100040000000000800000007412000074120000100000000000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF0FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFF000FFFFFFFFFFFFFF0000F
            FFFFF777777000000FFFF00000000000000FF777777000000FFFFFFFFFF0000F
            FFFFFFFFFF000FFFFFFFFFFFF00FFFFFFFFFFFFF0FFFFFFFFFFFFFFFFFFFFFFF
            FFFF}
          Transparent = True
        end
      end
      object ComboBox8: TComboBox
        Left = 24
        Top = 0
        Width = 97
        Height = 22
        Style = csOwnerDrawFixed
        ItemIndex = 0
        TabOrder = 0
        Text = 'none'
        OnSelect = ChangeProperties
        Items.Strings = (
          'none'
          'h40'
          'h41'
          'h42'
          'h43'
          'h44'
          'h45'
          'h46'
          'h47'
          'h48'
          't40'
          't43'
          't44'
          't45'
          'h20'
          'h21'
          'h22'
          'h23'
          'h24'
          't20'
          't21'
          't22'
          't23'
          'hr10'
          'hr11'
          'hr12'
          'tr10'
          'h10'
          'h11'
          'h12'
          'h12c'
          't10'
          'r0'
          'r10'
          'r11'
          'r12'
          'r20'
          'r20c'
          'r21'
          'r33'
          'ts10'
          'ts11'
          'ts12'
          'hs10'
          'hs12'
          'ts20'
          'ts21'
          'ts23'
          'hs20'
          'hs23'
          'o'
          'oc')
      end
      object ComboBox9: TComboBox
        Left = 121
        Top = 0
        Width = 96
        Height = 22
        Style = csOwnerDrawFixed
        ItemIndex = 0
        TabOrder = 1
        Text = 'none'
        OnSelect = ChangeProperties
        Items.Strings = (
          'none'
          'h40'
          'h41'
          'h42'
          'h43'
          'h44'
          'h45'
          'h46'
          'h47'
          'h48'
          't40'
          't43'
          't44'
          't45'
          'h20'
          'h21'
          'h22'
          'h23'
          'h24'
          't20'
          't21'
          't22'
          't23'
          'hr10'
          'hr11'
          'hr12'
          'tr10'
          'h10'
          'h11'
          'h12'
          'h12c'
          't10'
          'r0'
          'r10'
          'r11'
          'r12'
          'r20'
          'r20c'
          'r21'
          'r33'
          'ts10'
          'ts11'
          'ts12'
          'hs10'
          'hs12'
          'ts20'
          'ts21'
          'ts23'
          'hs20'
          'hs23'
          'o'
          'oc')
      end
      object Edit3: TEdit
        Left = 217
        Top = 0
        Width = 41
        Height = 21
        TabOrder = 2
        Text = '1'
        OnChange = ChangeProperties
      end
      object ToolButton28: TToolButton
        Left = 258
        Top = 0
        Width = 10
        Caption = 'ToolButton28'
        Style = tbsSeparator
      end
      object Panel8: TPanel
        Left = 268
        Top = 0
        Width = 24
        Height = 21
        BevelOuter = bvNone
        Caption = ' '
        TabOrder = 4
        object Image6: TImage
          Left = 0
          Top = 0
          Width = 24
          Height = 21
          Align = alClient
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000076000000280000001000
            0000100000000100040000000000800000007412000074120000100000000000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFF0000FF00FFF00FFFF00FFFF00F00FFFFF00FFFFF00
            0FFFFFF00FFFFF000FFFFFF00FFFF00F00FFFFF00FFF00FFF00FF0F00F0FFFFF
            FFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF}
          Transparent = True
        end
      end
      object Edit4: TEdit
        Left = 292
        Top = 0
        Width = 37
        Height = 21
        TabOrder = 6
        Text = '5'
        OnChange = ChangeProperties
      end
      object ComboBox7: TComboBox
        Left = 329
        Top = 0
        Width = 64
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 5
        Text = 'Left'
        OnSelect = ChangeProperties
        Items.Strings = (
          'Left'
          'Center'
          'Right')
      end
      object ToolButton29: TToolButton
        Left = 393
        Top = 0
        Width = 10
        Caption = 'ToolButton29'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object Panel9: TPanel
        Left = 403
        Top = 0
        Width = 24
        Height = 21
        BevelOuter = bvNone
        Caption = ' '
        TabOrder = 7
        object Image7: TImage
          Left = 0
          Top = 0
          Width = 24
          Height = 21
          Align = alClient
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000076000000280000001000
            0000100000000100040000000000800000007412000074120000100000000000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00888888888888888888800088888888888801110888888888880111088888
            8888880111088888888888800088008880088888888880080088888888888000
            008888888888870007888888888800000008800000800000000080CCC0888800
            088880CCC0888880888880CCC088888088888000008888888888888888888888
            8888}
          Transparent = True
        end
      end
      object ComboBox10: TComboBox
        Left = 427
        Top = 0
        Width = 54
        Height = 22
        Style = csOwnerDrawFixed
        TabOrder = 8
        OnDrawItem = ComboBox10DrawItem
        OnSelect = ChangeProperties
        Items.Strings = (
          ' '
          ' '
          ' '
          ' '
          ' '
          ' '
          ' '
          ' '
          ' '
          ' '
          ' '
          ' '
          ' '
          ' '
          ' ')
      end
      object Edit5: TEdit
        Left = 481
        Top = 0
        Width = 37
        Height = 21
        TabOrder = 9
        Text = '1'
        OnChange = ChangeProperties
      end
    end
  end
  object Panel31: TPanel
    Left = 0
    Top = 68
    Width = 646
    Height = 424
    Align = alClient
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 3
    object Panel20: TPanel
      Left = 0
      Top = 0
      Width = 25
      Height = 424
      Align = alLeft
      AutoSize = True
      BevelInner = bvRaised
      BevelOuter = bvNone
      TabOrder = 0
      object ToolBar2: TToolBar
        Left = 1
        Top = 1
        Width = 23
        Height = 422
        Align = alLeft
        AutoSize = True
        Customizable = True
        DragKind = dkDock
        EdgeBorders = [ebTop, ebBottom]
        EdgeInner = esNone
        EdgeOuter = esNone
        Images = ImageList2
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object InsertLineBtn: TToolButton
          Left = 0
          Top = 0
          Action = InsertLine
          Wrap = True
        end
        object InsertRectangleBtn: TToolButton
          Left = 0
          Top = 22
          Action = InsertRectangle
          Wrap = True
        end
        object InsertCircleBtn: TToolButton
          Left = 0
          Top = 44
          Action = InsertCircle
          Wrap = True
        end
        object InsertEllipseBtn: TToolButton
          Left = 0
          Top = 66
          Action = InsertEllipse
          Wrap = True
        end
        object InsertArcBtn: TToolButton
          Left = 0
          Top = 88
          Action = InsertArc
          Wrap = True
        end
        object InsertSectorBtn: TToolButton
          Left = 0
          Top = 110
          Action = InsertSector
          Wrap = True
        end
        object InsertSegmentBtn: TToolButton
          Left = 0
          Top = 132
          Action = InsertSegment
          Wrap = True
        end
        object InsertPolylineBtn: TToolButton
          Left = 0
          Top = 154
          Action = InsertPolyline
          Wrap = True
        end
        object InsertPolygonBtn: TToolButton
          Left = 0
          Top = 176
          Action = InsertPolygon
          Wrap = True
        end
        object InsertCurveBtn: TToolButton
          Left = 0
          Top = 198
          Action = InsertCurve
          Wrap = True
        end
        object InsertClosedCurveBtn: TToolButton
          Left = 0
          Top = 220
          Action = InsertClosedCurve
          Wrap = True
        end
        object InsertBezierPathBtn: TToolButton
          Left = 0
          Top = 242
          Action = InsertBezierPath
          Wrap = True
        end
        object InsertClosedBezierPathBtn: TToolButton
          Left = 0
          Top = 264
          Action = InsertClosedBezierPath
          Wrap = True
        end
        object InsertTextBtn: TToolButton
          Left = 0
          Top = 286
          Action = InsertText
          Wrap = True
        end
        object InsertStarBtn: TToolButton
          Left = 0
          Top = 308
          Action = InsertStar
          Wrap = True
        end
        object InsertSymbolBtn: TToolButton
          Left = 0
          Top = 330
          Action = InsertSymbol
          Wrap = True
        end
        object InsertBitmapBtn: TToolButton
          Left = 0
          Top = 352
          Action = InsertBitmap
          Wrap = True
        end
        object FreehandPolylineBtn: TToolButton
          Left = 0
          Top = 374
          Action = FreehandPolyline
          Wrap = True
        end
        object FreehandBezierBtn: TToolButton
          Left = 0
          Top = 396
          Action = FreehandBezier
        end
      end
    end
    object Panel32: TPanel
      Left = 25
      Top = 0
      Width = 621
      Height = 424
      Align = alClient
      BevelOuter = bvNone
      Caption = ' '
      TabOrder = 1
      object Panel33: TPanel
        Left = 57
        Top = 0
        Width = 564
        Height = 424
        Align = alClient
        BevelOuter = bvNone
        Caption = ' '
        PopupMenu = LocalPopUp
        TabOrder = 0
        object Panel34: TPanel
          Left = 0
          Top = 0
          Width = 564
          Height = 407
          Align = alClient
          BevelOuter = bvNone
          Caption = ' '
          TabOrder = 0
          object Panel2: TPanel
            Left = 0
            Top = 381
            Width = 564
            Height = 26
            Align = alBottom
            BevelOuter = bvNone
            Caption = ' '
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 0
            Visible = False
          end
          object Panel1: TPanel
            Left = 0
            Top = 0
            Width = 564
            Height = 381
            Align = alClient
            BevelOuter = bvNone
            Caption = ' '
            TabOrder = 1
            object VScrollBar: TScrollBar
              Left = 547
              Top = 0
              Width = 17
              Height = 381
              Align = alRight
              Ctl3D = False
              Kind = sbVertical
              LargeChange = 4
              PageSize = 0
              ParentCtl3D = False
              Position = 50
              TabOrder = 0
              TabStop = False
              OnScroll = ScrollBarScroll
            end
          end
        end
        object HScrollBar: TScrollBar
          Left = 0
          Top = 407
          Width = 564
          Height = 17
          Align = alBottom
          Ctl3D = False
          LargeChange = 4
          PageSize = 0
          ParentCtl3D = False
          Position = 50
          TabOrder = 1
          TabStop = False
          OnScroll = ScrollBarScroll
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 57
        Height = 424
        Align = alLeft
        BevelOuter = bvNone
        Caption = ' '
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        Visible = False
      end
    end
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Images = ImageList2
    Left = 57
    Top = 128
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Action = NewDoc
      end
      object Newwindow1: TMenuItem
        Action = NewWindow
      end
      object OpenDoc1: TMenuItem
        Action = OpenDoc
      end
      object Recentfiles1: TMenuItem
        Caption = 'Recent files'
        object TMenuItem
        end
      end
      object Save1: TMenuItem
        Action = SaveDoc
      end
      object Saveas1: TMenuItem
        Action = SaveAs
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object Copytoclipboard2: TMenuItem
        Action = CopyPictureToClipboard
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object TpXsettings1: TMenuItem
        Action = TpXSettings
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = ExitProgram
      end
    end
    object Edit2: TMenuItem
      Caption = 'Edit'
      object Undo1: TMenuItem
        Action = Undo
      end
      object Redo1: TMenuItem
        Action = Redo
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Action = ClipboardCut
      end
      object Copy1: TMenuItem
        Action = ClipboardCopy
      end
      object Paste1: TMenuItem
        Action = ClipboardPaste
      end
      object Duplicateselected1: TMenuItem
        Action = DuplicateSelected
        ShortCut = 16452
      end
      object Deleteselected1: TMenuItem
        Action = DeleteSelected
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Selectall1: TMenuItem
        Action = SelectAll
      end
      object Selectnext1: TMenuItem
        Action = SelNext
      end
      object Selectprevious1: TMenuItem
        Action = SelPrev
      end
      object Areaselect2: TMenuItem
        Action = AreaSelect
      end
      object Areaselectinsideonly1: TMenuItem
        Action = AreaSelectInsideAction
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Usesnap2: TMenuItem
        Action = SnapToGrid
      end
      object AngularSnap1: TMenuItem
        Action = AngularSnap
      end
      object Smoothbeziernodes1: TMenuItem
        Action = SmoothBezierNodesAction
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Copytoclipboard1: TMenuItem
        Caption = 'Copy drawing to clipboard'
        Visible = False
      end
      object Objectproperties1: TMenuItem
        Action = ObjectProperties
      end
      object Options1: TMenuItem
        Action = PictureProperties
      end
    end
    object Insert1: TMenuItem
      Caption = 'Insert'
      object Insertline1: TMenuItem
        Action = InsertLine
      end
      object InsertRectangle1: TMenuItem
        Action = InsertRectangle
      end
      object InsertCircle1: TMenuItem
        Action = InsertCircle
      end
      object InsertEllipse1: TMenuItem
        Action = InsertEllipse
      end
      object InsertArc1: TMenuItem
        Action = InsertArc
      end
      object Insertsector1: TMenuItem
        Action = InsertSector
      end
      object Insertsegment1: TMenuItem
        Action = InsertSegment
      end
      object InsertPolyline1: TMenuItem
        Action = InsertPolyline
      end
      object InsertPolygon1: TMenuItem
        Action = InsertPolygon
      end
      object Insertcurve1: TMenuItem
        Action = InsertCurve
      end
      object Insertclosedcurve1: TMenuItem
        Action = InsertClosedCurve
      end
      object InsertBezierpath1: TMenuItem
        Action = InsertBezierPath
      end
      object InsertclosedBezierpath1: TMenuItem
        Action = InsertClosedBezierPath
      end
      object InsertText1: TMenuItem
        Action = InsertText
      end
      object Insertstar1: TMenuItem
        Action = InsertStar
      end
      object Insertsymbol1: TMenuItem
        Action = InsertSymbol
      end
      object Insertbitmap1: TMenuItem
        Action = InsertBitmap
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object Freehandpolyline1: TMenuItem
        Action = FreehandPolyline
      end
      object FreehandBeziercurve1: TMenuItem
        Action = FreehandBezier
      end
    end
    object Transform1: TMenuItem
      Caption = 'Transform'
      object Fliphorizontally1: TMenuItem
        Action = FlipH
      end
      object Flipvertically1: TMenuItem
        Action = FlipV
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Move1: TMenuItem
        Caption = 'Move'
        object Moveup1: TMenuItem
          Action = MoveUp
        end
        object Movedown1: TMenuItem
          Action = MoveDown
        end
        object Moveleft1: TMenuItem
          Action = MoveLeft
        end
        object Moveright1: TMenuItem
          Action = MoveRight
        end
        object N9: TMenuItem
          Caption = '-'
        end
        object Moveup1pixel1: TMenuItem
          Action = MoveUpPixel
        end
        object Movedown1pixel1: TMenuItem
          Action = MoveDownPixel
        end
        object Moveleft1pixel1: TMenuItem
          Action = MoveLeftPixel
        end
        object Moveright1pixel1: TMenuItem
          Action = MoveRightPixel
        end
      end
      object Rotate1: TMenuItem
        Caption = 'Rotate'
        object Rotateclockwise1: TMenuItem
          Action = RotateClockW
        end
        object Rotatecounterclockwise1: TMenuItem
          Action = RotateCounterclockW
        end
        object Rotateclockwise1deg1: TMenuItem
          Action = RotateClockWDegree
        end
        object Rotatecounterclockwise1deg1: TMenuItem
          Action = RotateCounterclockWDegree
        end
      end
      object Scale2: TMenuItem
        Caption = 'Scale'
        object Grow101: TMenuItem
          Action = Grow10
        end
        object Grow102: TMenuItem
          Action = Shrink10
        end
        object Grow11: TMenuItem
          Action = Grow1
        end
        object Shrink11: TMenuItem
          Action = Shrink1
        end
      end
      object Align1: TMenuItem
        Caption = 'Align'
        object Left1: TMenuItem
          Action = AlignLeft
        end
        object HCenter1: TMenuItem
          Action = AlignHCenter
        end
        object Right1: TMenuItem
          Action = AlignRight
        end
        object Bottom1: TMenuItem
          Action = AlignBottom
        end
        object VCenter1: TMenuItem
          Action = AlignVCenter
        end
        object op1: TMenuItem
          Action = AlignTop
        end
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Startmove1: TMenuItem
        Action = StartMove
      end
      object Startrotate1: TMenuItem
        Action = StartRotate
      end
      object Deleteselected2: TMenuItem
        Caption = '-'
        Hint = 'Delete selected'
        ShortCut = 46
        OnClick = UserEventExecute
      end
      object Areaselect3: TMenuItem
        Action = CustomTransform
      end
      object Scalestandard1: TMenuItem
        Action = ScaleStandard
      end
      object Scalephysicalunits1: TMenuItem
        Action = ScalePhysical
        Visible = False
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Scalelinewidth1: TMenuItem
        Action = ScaleLineWidthAction
      end
      object Scaletext1: TMenuItem
        Action = ScaleTextAction
      end
      object Rotatetext1: TMenuItem
        Action = RotateTextAction
      end
      object Rotatesymbols1: TMenuItem
        Action = RotateSymbolsAction
      end
    end
    object Modify1: TMenuItem
      Caption = 'Modify'
      object Edit1: TMenuItem
        Caption = 'Arrange'
        object MoveForward1: TMenuItem
          Action = MoveBackward
        end
        object MoveForward4: TMenuItem
          Action = MoveForward
        end
        object MoveForward3: TMenuItem
          Action = MoveToBack
        end
        object MoveForward2: TMenuItem
          Action = MoveToFront
        end
      end
      object Group1: TMenuItem
        Action = Group
      end
      object Ungroup1: TMenuItem
        Action = Ungroup
      end
      object Makecompound1: TMenuItem
        Action = MakeCompound
      end
      object Uncompound1: TMenuItem
        Action = Uncompound
      end
      object Converttopolyline1: TMenuItem
        Action = ConvertTo
      end
      object Simplifypolylinepolygon1: TMenuItem
        Action = SimplifyPoly
      end
      object SimplifyBezier1: TMenuItem
        Action = SimplifyBezier
      end
      object Connectpaths1: TMenuItem
        Action = ConnectPaths
      end
      object Reversepoints1: TMenuItem
        Action = ReversePoints
      end
      object Deletesmallobjects1: TMenuItem
        Action = DeleteSmallObjects
      end
      object Converttograyscale1: TMenuItem
        Action = ConvertToGrayScale
      end
    end
    object Tools1: TMenuItem
      Caption = 'Tools'
      OnClick = Tools1Click
      object PreviewLaTeX1: TMenuItem
        Action = PreviewLaTeX
      end
      object PreviewLaTeXDVIPS1: TMenuItem
        Action = PreviewLaTeX_PS
      end
      object PreviewPdfLaTeX1: TMenuItem
        Action = PreviewPdfLaTeX
      end
      object Preview1: TMenuItem
        Caption = 'Preview as image'
        object PreviewEPS1: TMenuItem
          Action = PreviewEPS
        end
        object PreviewPDF1: TMenuItem
          Action = PreviewPDF
        end
        object PreviewSVG1: TMenuItem
          Action = PreviewSVG
        end
        object PreviewPNG1: TMenuItem
          Action = PreviewPNG
        end
        object PreviewBMP1: TMenuItem
          Action = PreviewBMP
        end
        object PreviewBMP2: TMenuItem
          Action = PreviewEMF
        end
      end
      object Viewsource1: TMenuItem
        Caption = 'View source'
        object Drawingsource1: TMenuItem
          Action = DrawingSource
        end
        object previewtexinc1: TMenuItem
          Action = preview_tex_inc
        end
        object metaposttexinc1: TMenuItem
          Action = metapost_tex_inc
        end
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object CaptureEMF1: TMenuItem
        Action = CaptureEMF
      end
      object ImagetoEPStool1: TMenuItem
        Action = ImageTool
      end
    end
    object u1: TMenuItem
      Caption = 'View'
      object Zoomarea2: TMenuItem
        Action = ZoomArea
      end
      object Zoomin2: TMenuItem
        Action = ZoomIn
      end
      object Zoomout2: TMenuItem
        Action = ZoomOut
      end
      object Zoomall2: TMenuItem
        Action = ZoomAll
      end
      object Panning2: TMenuItem
        Action = HandTool
      end
      object Setpoint2: TMenuItem
        Caption = 'Set point'
        Visible = False
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object Showgrid2: TMenuItem
        Action = ShowGrid
      end
      object Gridontop1: TMenuItem
        Action = GridOnTop
      end
      object Showcrosshair1: TMenuItem
        Action = ShowCrossHair
      end
      object ShowRulers1: TMenuItem
        Action = ShowRulers
      end
      object Showscrollbars1: TMenuItem
        Action = ShowScrollBars
      end
      object ShowPropertiesToolbar11: TMenuItem
        Action = ShowPropertiesToolbar1
      end
      object ShowPropertiesToolbar21: TMenuItem
        Action = ShowPropertiesToolbar2
      end
      object Useareatoselectobjects2: TMenuItem
        Caption = 'Use area to select objects'
        Visible = False
      end
    end
    object Help2: TMenuItem
      Caption = 'Help'
      object Help1: TMenuItem
        Action = TpXHelp
      end
      object Pictureinfo1: TMenuItem
        Action = PictureInfo
      end
      object About1: TMenuItem
        Action = About
      end
    end
  end
  object LocalPopUp: TPopupMenu
    AutoHotkeys = maManual
    AutoPopup = False
    Images = ImageList2
    Left = 87
    Top = 128
    object Test1: TMenuItem
      Caption = 'Test'
      OnClick = Test1Click
    end
  end
  object DrawingSaveDlg: TSaveDialog
    DefaultExt = 'qqq'
    Filter = 
      'TpX drawing|*.TpX|Scalable vector graphics (SVG)|*.svg|Enhanced ' +
      'metafile (EMF)|*.emf|Encapsulated PostScript (EPS)|*.eps|Portabl' +
      'e network graphics (PNG)|*.png|Windows bitmap (BMP)|*.bmp|Portab' +
      'le document format (PDF)|*.pdf|MetaPost (.mp)|*.mp|MetaPost EPS ' +
      'output (.mps)|*.mps|PDF from EPS|*.pdf|LaTeX EPS (latex-dvips)|*' +
      '.eps|PDF from LaTeX EPS (latex-dvips-gs-pdf)|*.pdf|LaTeX custom ' +
      '(latex-dvips-gs)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 123
    Top = 90
  end
  object EMFOpenDialog: TOpenPictureDialog
    DefaultExt = 'tpx'
    FilterIndex = 2
    Left = 200
    Top = 84
  end
  object ActionList1: TActionList
    Images = ImageList2
    Left = 152
    Top = 92
    object Undo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Hint = 'Undo'
      ImageIndex = 50
      SecondaryShortCuts.Strings = (
        'Alt+BkSp')
      ShortCut = 16474
      OnExecute = UserEventExecute
    end
    object Redo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      Hint = 'Redo'
      ImageIndex = 51
      SecondaryShortCuts.Strings = (
        'Shift+Alt+BkSp')
      ShortCut = 24666
      OnExecute = UserEventExecute
    end
    object ClipboardCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      Hint = 'Copy to clipboard'
      ImageIndex = 37
      SecondaryShortCuts.Strings = (
        'ctrl+ins')
      ShortCut = 16451
      OnExecute = UserEventExecute
    end
    object ClipboardPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      Hint = 'Paste from clipboard'
      ImageIndex = 38
      SecondaryShortCuts.Strings = (
        'shift+ins')
      ShortCut = 16470
      OnExecute = UserEventExecute
    end
    object ClipboardCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      Hint = 'Cut to clipboard'
      ImageIndex = 36
      SecondaryShortCuts.Strings = (
        'Shift+Del')
      ShortCut = 16472
      OnExecute = UserEventExecute
    end
    object ShowCrossHair: TAction
      Category = 'View'
      Caption = 'Show crosshair'
      Checked = True
      OnExecute = ShowCrossHairExecute
    end
    object DeleteSelected: TAction
      Category = 'Edit'
      Caption = 'Delete selected'
      Hint = 'Delete selected'
      ImageIndex = 23
      SecondaryShortCuts.Strings = (
        'Ctrl+Del')
      ShortCut = 46
      OnExecute = UserEventExecute
    end
    object DuplicateSelected: TAction
      Category = 'Edit'
      Caption = 'Duplicate selected'
      Hint = 'Duplicate selected'
      ImageIndex = 48
      OnExecute = UserEventExecute
    end
    object MoveUp: TAction
      Category = 'Transform'
      Caption = 'Move up'
      ImageIndex = 40
      ShortCut = 38
      OnExecute = UserEventExecute
    end
    object MoveDown: TAction
      Category = 'Transform'
      Caption = 'Move down'
      ImageIndex = 43
      ShortCut = 40
      OnExecute = UserEventExecute
    end
    object MoveLeft: TAction
      Category = 'Transform'
      Caption = 'Move left'
      ImageIndex = 41
      ShortCut = 37
      OnExecute = UserEventExecute
    end
    object MoveRight: TAction
      Category = 'Transform'
      Caption = 'Move right'
      ImageIndex = 42
      ShortCut = 39
      OnExecute = UserEventExecute
    end
    object MoveUpPixel: TAction
      Category = 'Transform'
      Caption = 'Move up 1 pixel'
      ShortCut = 16422
      OnExecute = UserEventExecute
    end
    object MoveDownPixel: TAction
      Category = 'Transform'
      Caption = 'Move down 1 pixel'
      ShortCut = 16424
      OnExecute = UserEventExecute
    end
    object MoveLeftPixel: TAction
      Category = 'Transform'
      Caption = 'Move left 1 pixel'
      ShortCut = 16421
      OnExecute = UserEventExecute
    end
    object MoveRightPixel: TAction
      Category = 'Transform'
      Caption = 'Move right 1 pixel'
      ShortCut = 16423
      OnExecute = UserEventExecute
    end
    object FlipV: TAction
      Category = 'Transform'
      Caption = 'Flip vertically'
      ImageIndex = 29
      SecondaryShortCuts.Strings = (
        'Alt+Shift+Down')
      ShortCut = 40998
      OnExecute = UserEventExecute
    end
    object SelectAll: TAction
      Category = 'Edit'
      Caption = 'Select all'
      Hint = 'Select all'
      ShortCut = 16449
      OnExecute = UserEventExecute
    end
    object SelNext: TAction
      Category = 'Edit'
      Caption = 'Select next'
      Hint = 'Select next'
      ShortCut = 9
      OnExecute = UserEventExecute
    end
    object SelPrev: TAction
      Category = 'Edit'
      Caption = 'Select previous'
      Hint = 'Select previous'
      ShortCut = 8201
      OnExecute = UserEventExecute
    end
    object FlipH: TAction
      Category = 'Transform'
      Caption = 'Flip horizontally'
      ImageIndex = 28
      SecondaryShortCuts.Strings = (
        'Alt+Shift+Left')
      ShortCut = 40999
      OnExecute = UserEventExecute
    end
    object ConvertToGrayScale: TAction
      Category = 'Modify'
      Caption = 'Convert to grayscale'
      Hint = 'Convert to grayscale'
      OnExecute = UserEventExecute
    end
    object RotateCounterclockW: TAction
      Category = 'Transform'
      Caption = 'Rotate counterclockwise 90deg'
      ImageIndex = 31
      ShortCut = 32805
      OnExecute = UserEventExecute
    end
    object RotateClockW: TAction
      Category = 'Transform'
      Caption = 'Rotate clockwise 90deg'
      ImageIndex = 30
      ShortCut = 32807
      OnExecute = UserEventExecute
    end
    object RotateCounterclockWDegree: TAction
      Category = 'Transform'
      Caption = 'Rotate counterclockwise 1deg'
      ShortCut = 49189
      OnExecute = UserEventExecute
    end
    object RotateClockWDegree: TAction
      Category = 'Transform'
      Caption = 'Rotate clockwise 1deg'
      ShortCut = 49191
      OnExecute = UserEventExecute
    end
    object Grow10: TAction
      Category = 'Transform'
      Caption = 'Grow 10%'
      ShortCut = 32806
      OnExecute = UserEventExecute
    end
    object Shrink10: TAction
      Category = 'Transform'
      Caption = 'Shrink 10%'
      ShortCut = 32808
      OnExecute = UserEventExecute
    end
    object Grow1: TAction
      Category = 'Transform'
      Caption = 'Grow 1%'
      ShortCut = 49190
      OnExecute = UserEventExecute
    end
    object Shrink1: TAction
      Category = 'Transform'
      Caption = 'Shrink 1%'
      ShortCut = 49192
      OnExecute = UserEventExecute
    end
    object MoveForward: TAction
      Category = 'Arrange'
      Caption = 'Move forward'
      Hint = 'Move forward'
      ImageIndex = 24
      ShortCut = 33
      OnExecute = UserEventExecute
    end
    object MoveBackward: TAction
      Category = 'Arrange'
      Caption = 'Move backward'
      Hint = 'Move backward'
      ImageIndex = 25
      ShortCut = 34
      OnExecute = UserEventExecute
    end
    object MoveToFront: TAction
      Category = 'Arrange'
      Caption = 'Bring to front'
      Hint = 'Bring to front'
      ImageIndex = 26
      ShortCut = 36
      OnExecute = UserEventExecute
    end
    object MoveToBack: TAction
      Category = 'Arrange'
      Caption = 'Put to  back'
      Hint = 'Put to  back'
      ImageIndex = 27
      ShortCut = 35
      OnExecute = UserEventExecute
    end
    object StartRotate: TAction
      Category = 'Transform'
      Caption = 'Start rotate'
      ImageIndex = 32
      ShortCut = 16466
      OnExecute = UserEventExecute
    end
    object StartMove: TAction
      Category = 'Transform'
      Caption = 'Start move'
      ImageIndex = 49
      ShortCut = 16461
      OnExecute = UserEventExecute
    end
    object InsertLine: TAction
      Category = 'Insert'
      Caption = 'Insert line'
      Hint = 'Insert line'
      ImageIndex = 0
      ShortCut = 76
      OnExecute = InsertLineExecute
    end
    object InsertRectangle: TAction
      Category = 'Insert'
      Caption = 'Insert rectangle'
      Hint = 'Insert rectangle'
      ImageIndex = 1
      ShortCut = 82
      OnExecute = InsertRectangleExecute
    end
    object InsertCircle: TAction
      Category = 'Insert'
      Caption = 'Insert circle'
      Hint = 'Insert circle'
      ImageIndex = 10
      ShortCut = 67
      OnExecute = InsertCircleExecute
    end
    object InsertEllipse: TAction
      Category = 'Insert'
      Caption = 'Insert ellipse'
      Hint = 'Insert ellipse'
      ImageIndex = 3
      ShortCut = 69
      OnExecute = InsertEllipseExecute
    end
    object InsertArc: TAction
      Category = 'Insert'
      Caption = 'Insert arc'
      Hint = 'Insert arc'
      ImageIndex = 2
      ShortCut = 65
      OnExecute = InsertArcExecute
    end
    object InsertPolyline: TAction
      Category = 'Insert'
      Caption = 'Insert polyline'
      Hint = 'Insert polyline'
      ImageIndex = 4
      ShortCut = 80
      OnExecute = InsertPolylineExecute
    end
    object InsertPolygon: TAction
      Category = 'Insert'
      Caption = 'Insert polygon'
      Hint = 'Insert polygon'
      ImageIndex = 5
      ShortCut = 71
      OnExecute = InsertPolygonExecute
    end
    object InsertText: TAction
      Category = 'Insert'
      Caption = 'Insert text'
      Hint = 'Insert text'
      ImageIndex = 7
      ShortCut = 84
      OnExecute = InsertTextExecute
    end
    object InsertStar: TAction
      Category = 'Insert'
      Caption = 'Insert star'
      Hint = 'Insert star'
      ImageIndex = 11
      ShortCut = 66
      OnExecute = InsertStarExecute
    end
    object NewDoc: TAction
      Category = 'File'
      Caption = 'New'
      Hint = 'New drawing'
      ImageIndex = 12
      ShortCut = 16462
      OnExecute = UserEventExecute
    end
    object NewWindow: TAction
      Category = 'File'
      Caption = 'New window'
      ImageIndex = 61
      ShortCut = 16471
      OnExecute = UserEventExecute
    end
    object OpenDoc: TAction
      Category = 'File'
      Caption = 'Open'
      Hint = 'Open drawing'
      ImageIndex = 13
      ShortCut = 16463
      OnExecute = UserEventExecute
    end
    object SaveDoc: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save drawing'
      ImageIndex = 14
      ShortCut = 16467
      OnExecute = UserEventExecute
    end
    object Print: TAction
      Category = 'File'
      Caption = 'Print'
      Hint = 'Print'
      ImageIndex = 15
      ShortCut = 16464
      OnExecute = UserEventExecute
    end
    object ZoomArea: TAction
      Category = 'View'
      Caption = 'Zoom area'
      Hint = 'Zoom area'
      ImageIndex = 16
      ShortCut = 32881
      OnExecute = ZoomAreaExecute
    end
    object ZoomIn: TAction
      Category = 'View'
      Caption = 'Zoom in'
      Hint = 'Zoom in'
      ImageIndex = 17
      SecondaryShortCuts.Strings = (
        'Alt+=')
      ShortCut = 107
      OnExecute = UserEventExecute
    end
    object ZoomOut: TAction
      Category = 'View'
      Caption = 'Zoom out'
      Hint = 'Zoom out'
      ImageIndex = 18
      SecondaryShortCuts.Strings = (
        'Alt+-')
      ShortCut = 109
      OnExecute = UserEventExecute
    end
    object ZoomAll: TAction
      Category = 'View'
      Caption = 'Zoom all'
      Hint = 'Zoom all'
      ImageIndex = 19
      SecondaryShortCuts.Strings = (
        'Alt+0')
      ShortCut = 106
      OnExecute = UserEventExecute
    end
    object HandTool: TAction
      Category = 'View'
      Caption = 'Hand tool'
      Hint = 'Hand tool'
      ImageIndex = 20
      OnExecute = HandToolExecute
    end
    object SaveAs: TAction
      Category = 'File'
      Caption = 'Save as...'
      Hint = 'Save as...'
      ImageIndex = 22
      ShortCut = 24659
      OnExecute = UserEventExecute
    end
    object InsertSector: TAction
      Category = 'Insert'
      Caption = 'Insert sector'
      Hint = 'Insert sector'
      ImageIndex = 33
      ShortCut = 83
      OnExecute = InsertSectorExecute
    end
    object InsertSegment: TAction
      Category = 'Insert'
      Caption = 'Insert segment'
      Hint = 'Insert segment'
      ImageIndex = 34
      ShortCut = 8275
      OnExecute = InsertSegmentExecute
    end
    object InsertCurve: TAction
      Category = 'Insert'
      Caption = 'Insert curve'
      Hint = 'Insert curve'
      ImageIndex = 52
      ShortCut = 85
      OnExecute = InsertCurveExecute
    end
    object InsertClosedCurve: TAction
      Category = 'Insert'
      Caption = 'Insert closed curve'
      Hint = 'Insert closed curve'
      ImageIndex = 53
      ShortCut = 79
      OnExecute = InsertClosedCurveExecute
    end
    object ShowGrid: TAction
      Category = 'View'
      Caption = 'Show grid'
      Checked = True
      OnExecute = ShowGridExecute
    end
    object SnapToGrid: TAction
      Category = 'Edit'
      Caption = 'Snap to grid'
      Hint = 'Snap to grid'
      ShortCut = 114
      OnExecute = UserEventExecute
    end
    object AngularSnap: TAction
      Category = 'Edit'
      Caption = 'Angular snap'
      Hint = 'Use orto'
      ShortCut = 117
      OnExecute = UserEventExecute
    end
    object SmoothBezierNodesAction: TAction
      Category = 'Edit'
      Caption = 'Smooth bezier nodes'
      Checked = True
      Hint = 'Smooth bezier nodes'
      OnExecute = UserEventExecute
    end
    object AreaSelect: TAction
      Category = 'Edit'
      Caption = 'Area select'
      Hint = 'Area select'
      ImageIndex = 35
      ShortCut = 113
      OnExecute = AreaSelectExecute
    end
    object AreaSelectInsideAction: TAction
      Category = 'Edit'
      Caption = 'Area select inside only'
      Checked = True
      Hint = 'Area select inside only'
      ShortCut = 16497
      OnExecute = UserEventExecute
    end
    object ConvertToPolyline: TAction
      Category = 'Modify'
      Caption = 'Convert to polyline'
      Hint = 'Convert to polyline'
      ImageIndex = 4
      OnExecute = UserEventExecute
    end
    object CustomTransform: TAction
      Category = 'Transform'
      Caption = 'Custom transform'
      ShortCut = 115
      OnExecute = UserEventExecute
    end
    object ShowRulers: TAction
      Category = 'View'
      Caption = 'Show rulers'
      OnExecute = ShowRulersExecute
    end
    object ShowScrollBars: TAction
      Category = 'View'
      Caption = 'Show scroll bars'
      OnExecute = ShowScrollBarsExecute
    end
    object ConvertTo: TAction
      Category = 'Modify'
      Caption = 'Convert to...'
      Hint = 'Convert to...'
      ShortCut = 116
      OnExecute = ConvertToExecute
    end
    object DoConvertTo: TAction
      Category = 'Modify'
      Caption = 'DoConvert'
      OnExecute = DoConvertToExecute
    end
    object PreviewLaTeX: TAction
      Category = 'Tools'
      Caption = 'Preview LaTeX->DVI'
      Hint = 'Preview LaTeX->DVI'
      ImageIndex = 54
      ShortCut = 24662
      OnExecute = UserEventExecute
    end
    object PreviewPdfLaTeX: TAction
      Category = 'Tools'
      Caption = 'Preview PdfLaTeX'
      Hint = 'Preview PdfLaTeX'
      ImageIndex = 55
      ShortCut = 24658
      OnExecute = UserEventExecute
    end
    object PreviewLaTeX_PS: TAction
      Category = 'Tools'
      Caption = 'Preview LaTeX->DVI->PS'
      Hint = 'Preview LaTeX->DVI->PS'
      ImageIndex = 58
      ShortCut = 24647
      OnExecute = UserEventExecute
    end
    object OpenRecent: TAction
      Category = 'File'
      Caption = 'OpenRecent'
      OnExecute = OpenRecentExecute
    end
    object PreviewSVG: TAction
      Category = 'Tools'
      Caption = 'Preview SVG'
      Hint = 'Preview SVG'
      ShortCut = 49235
      OnExecute = UserEventExecute
    end
    object PreviewEPS: TAction
      Category = 'Tools'
      Caption = 'Preview EPS'
      Hint = 'Preview EPS'
      ShortCut = 49221
      OnExecute = UserEventExecute
    end
    object PreviewPDF: TAction
      Category = 'Tools'
      Caption = 'Preview PDF'
      Hint = 'Preview PDF'
      ShortCut = 49232
      OnExecute = UserEventExecute
    end
    object PreviewPNG: TAction
      Category = 'Tools'
      Caption = 'Preview PNG'
      Hint = 'Preview PNG'
      ShortCut = 49230
      OnExecute = UserEventExecute
    end
    object PreviewBMP: TAction
      Category = 'Tools'
      Caption = 'Preview BMP'
      Hint = 'Preview BMP'
      ShortCut = 49218
      OnExecute = UserEventExecute
    end
    object PreviewEMF: TAction
      Category = 'Tools'
      Caption = 'Preview EMF'
      Hint = 'Preview EMF'
      ShortCut = 49229
      OnExecute = UserEventExecute
    end
    object ScalePhysical: TAction
      Category = 'Transform'
      Caption = 'Scale physical units'
      OnExecute = ScalePhysicalExecute
    end
    object InsertBezierPath: TAction
      Category = 'Insert'
      Caption = 'Insert Bezier path'
      Hint = 'Insert Bezier path'
      ImageIndex = 59
      ShortCut = 90
      OnExecute = InsertBezierPathExecute
    end
    object InsertClosedBezierPath: TAction
      Category = 'Insert'
      Caption = 'Insert closed Bezier path'
      Hint = 'Insert closed Bezier path'
      ImageIndex = 60
      ShortCut = 68
      OnExecute = InsertClosedBezierPathExecute
    end
    object InsertSymbol: TAction
      Category = 'Insert'
      Caption = 'Insert symbol'
      Hint = 'Insert closed Bezier path'
      ImageIndex = 62
      ShortCut = 88
      OnExecute = InsertSymbolExecute
    end
    object SimplifyPoly: TAction
      Category = 'Modify'
      Caption = 'Simplify polyline/polygon'
      Hint = 'Simplify polyline/polygon'
      OnExecute = UserEventExecute
    end
    object SimplifyBezier: TAction
      Category = 'Modify'
      Caption = 'Simplify Bezier'
      Hint = 'Simplify Bezier'
      OnExecute = UserEventExecute
    end
    object ConnectPaths: TAction
      Category = 'Modify'
      Caption = 'Connect paths'
      Hint = 'Connect paths'
      OnExecute = UserEventExecute
    end
    object RotateTextAction: TAction
      Category = 'Transform'
      Caption = 'Rotate text'
      Checked = True
      OnExecute = RotateTextActionExecute
    end
    object RotateSymbolsAction: TAction
      Category = 'Transform'
      Caption = 'Rotate symbols'
      Checked = True
      OnExecute = RotateSymbolsActionExecute
    end
    object ScaleLineWidthAction: TAction
      Category = 'Transform'
      Caption = 'Scale line width'
      Checked = True
      OnExecute = ScaleLineWidthActionExecute
    end
    object DrawingSource: TAction
      Category = 'Tools'
      Caption = 'Drawing source'
      Hint = 'Drawing source'
      ShortCut = 24644
      OnExecute = UserEventExecute
    end
    object preview_tex_inc: TAction
      Category = 'Tools'
      Caption = 'preview.tex.inc'
      Hint = 'preview.tex.inc'
      OnExecute = UserEventExecute
    end
    object metapost_tex_inc: TAction
      Category = 'Tools'
      Caption = 'metapost.tex.inc'
      Hint = 'metapost.tex.inc'
      OnExecute = UserEventExecute
    end
    object CaptureEMF: TAction
      Category = 'Tools'
      Caption = 'Capture EMF'
      Hint = 'Capture EMF'
      OnExecute = UserEventExecute
    end
    object ImageTool: TAction
      Category = 'Tools'
      Caption = 'Image tool'
      Hint = 'Image tool'
      ImageIndex = 56
      OnExecute = UserEventExecute
    end
    object ObjectProperties: TAction
      Category = 'Edit'
      Caption = 'Object properties'
      SecondaryShortCuts.Strings = (
        'f3')
      ShortCut = 13
      OnExecute = UserEventExecute
    end
    object TeXFormat: TAction
      Category = 'Edit'
      Caption = 'TeX output format'
      Hint = 'TeX output format'
      ImageIndex = 66
      OnExecute = TeXFormatExecute
    end
    object PdfTeXFormat: TAction
      Category = 'Edit'
      Caption = 'PdfTeX output format'
      Hint = 'TeX output format'
      ImageIndex = 66
      OnExecute = PdfTeXFormatExecute
    end
    object PictureProperties: TAction
      Category = 'Edit'
      Caption = 'Picture properties'
      ImageIndex = 46
      ShortCut = 32781
      OnExecute = UserEventExecute
    end
    object CopyPictureToClipboard: TAction
      Category = 'File'
      Caption = 'Copy picture to clipboard'
      ImageIndex = 39
      ShortCut = 49219
      OnExecute = UserEventExecute
    end
    object ScaleTextAction: TAction
      Category = 'Transform'
      Caption = 'Scale text'
      Checked = True
      OnExecute = ScaleTextActionExecute
    end
    object ScaleStandard: TAction
      Category = 'Transform'
      Caption = 'Scale standard'
      ShortCut = 121
      OnExecute = UserEventExecute
    end
    object TpXHelp: TAction
      Category = 'Help'
      Caption = 'TpX help'
      ShortCut = 112
      OnExecute = UserEventExecute
    end
    object PictureInfo: TAction
      Category = 'Help'
      Caption = 'Picture info'
      ShortCut = 32880
      OnExecute = UserEventExecute
    end
    object TpXSettings: TAction
      Category = 'File'
      Caption = 'TpX settings'
      ImageIndex = 47
      OnExecute = UserEventExecute
    end
    object ExitProgram: TAction
      Category = 'File'
      Caption = 'Exit'
      OnExecute = UserEventExecute
    end
    object About: TAction
      Category = 'Help'
      Caption = 'About'
      OnExecute = UserEventExecute
    end
    object BasicModeAction: TAction
      Category = 'File'
      Caption = 'BasicModeAction'
      ShortCut = 27
      OnExecute = BasicModeExecute
    end
    object InsertBitmap: TAction
      Category = 'Insert'
      Caption = 'Insert bitmap'
      Hint = 'Insert bitmap'
      ImageIndex = 76
      ShortCut = 77
      OnExecute = InsertBitmapExecute
    end
    object FreehandPolyline: TAction
      Category = 'Insert'
      Caption = 'Freehand polyline'
      Hint = 'Freehand polyline'
      ImageIndex = 63
      Visible = False
      OnExecute = FreehandPolylineExecute
    end
    object ReversePoints: TAction
      Category = 'Modify'
      Caption = 'Reverse points'
      Hint = 'Reverse points'
      OnExecute = UserEventExecute
    end
    object AlignLeft: TAction
      Category = 'Align'
      Caption = 'Left'
      ImageIndex = 67
      OnExecute = UserEventExecute
    end
    object AlignRight: TAction
      Category = 'Align'
      Caption = 'Right'
      ImageIndex = 68
      OnExecute = UserEventExecute
    end
    object AlignHCenter: TAction
      Category = 'Align'
      Caption = 'HCenter'
      ImageIndex = 69
      OnExecute = UserEventExecute
    end
    object AlignTop: TAction
      Category = 'Align'
      Caption = 'Top'
      ImageIndex = 70
      OnExecute = UserEventExecute
    end
    object AlignBottom: TAction
      Category = 'Align'
      Caption = 'Bottom'
      ImageIndex = 71
      OnExecute = UserEventExecute
    end
    object AlignVCenter: TAction
      Category = 'Align'
      Caption = 'VCenter'
      ImageIndex = 72
      OnExecute = UserEventExecute
    end
    object DeleteSmallObjects: TAction
      Category = 'Modify'
      Caption = 'Delete small objects'
      Hint = 'Delete small objects'
      OnExecute = UserEventExecute
    end
    object Group: TAction
      Category = 'Modify'
      Caption = 'Group'
      Hint = 'Group'
      ShortCut = 16455
      OnExecute = UserEventExecute
    end
    object Ungroup: TAction
      Category = 'Modify'
      Caption = 'Ungroup'
      Hint = 'Ungroup'
      SecondaryShortCuts.Strings = (
        'Ctrl+U')
      ShortCut = 24647
      OnExecute = UserEventExecute
    end
    object MakeCompound: TAction
      Category = 'Modify'
      Caption = 'Make compound'
      Enabled = False
      ShortCut = 16459
      Visible = False
      OnExecute = UserEventExecute
    end
    object Uncompound: TAction
      Category = 'Modify'
      Caption = 'Uncompound'
      Enabled = False
      Hint = 'Uncompound'
      ShortCut = 24651
      Visible = False
      OnExecute = UserEventExecute
    end
    object BreakPath: TAction
      Category = 'Modify'
      Caption = 'Break path'
      Hint = 'Break path'
      ImageIndex = 75
      OnExecute = UserEventExecute
    end
    object DeletePoint: TAction
      Category = 'Modify'
      Caption = 'Delete point'
      Hint = 'Delete point'
      ImageIndex = 74
      OnExecute = UserEventExecute
    end
    object AddPoint: TAction
      Category = 'Modify'
      Caption = 'Add point'
      Hint = 'Add point'
      ImageIndex = 73
      OnExecute = UserEventExecute
    end
    object FreehandBezier: TAction
      Category = 'Insert'
      Caption = 'Freehand Bezier curve'
      Hint = 'Freehand Bezier curve'
      ImageIndex = 64
      ShortCut = 70
      OnExecute = FreehandBezierExecute
    end
    object GridOnTop: TAction
      Category = 'View'
      Caption = 'Grid on top'
      OnExecute = GridOnTopExecute
    end
    object PickUpProperties: TAction
      Category = 'Edit'
      Caption = 'Pick up properties'
      Hint = 'Pick up properties'
      ImageIndex = 77
      OnExecute = PickUpPropertiesExecute
    end
    object DefaultProperties: TAction
      Category = 'Edit'
      Caption = 'Set default properties'
      Hint = 'Set default properties'
      ImageIndex = 78
      OnExecute = DefaultPropertiesExecute
    end
    object ApplyProperties: TAction
      Category = 'Edit'
      Caption = 'Apply properties'
      Hint = 'Apply properties'
      ImageIndex = 79
      OnExecute = ApplyPropertiesExecute
    end
    object ShowPropertiesToolbar1: TAction
      Category = 'View'
      Caption = 'Show the first properties toolbar'
      Enabled = False
      Visible = False
      OnExecute = ShowPropertiesToolbar1Execute
    end
    object ShowPropertiesToolbar2: TAction
      Category = 'View'
      Caption = 'Show the second properties toolbar'
      Enabled = False
      Visible = False
      OnExecute = ShowPropertiesToolbar2Execute
    end
  end
  object ImageList2: TImageList
    Left = 164
    Top = 132
    Bitmap = {
      494C010150005400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000005001000001002000000000000050
      0100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080800000808000008080
      0000808000008080000080800000808000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000000080000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080000080800000000000000000
      0000000000008080000080800000808000008080000080800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008080800000000000FFFFFF00800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080800000000000008080
      0000808000000000000080800000808000008080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00800000008000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF008080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF008000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF0080800000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000C0C0
      C00080808000C0C0C00080808000C0C0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800080808000FFFF
      FF00800000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00808000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0008080
      8000C0C0C00080808000C0C0C000808080000000000000808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF008080800000000000000000000000
      000000000000000000000000000000000000000000000000000080808000C0C0
      C00080808000C0C0C00080808000C0C0C0000000000000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF0080808000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000008080000080
      8000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800000000000000000000000000080008000000000008080
      80000000000000FFFF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000080
      8000008080000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C0008080800080808000000000000000000080008000800080000000
      0000000000000000000000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      00000080800000808000000000000000000000000000C0C0C000FFFFFF00C0C0
      C000C0C0C000C0C0C00080808000808080008000800080008000800080000000
      0000000000000000000000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00000000000080800000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C00000000000C0C0C000808080008080800080008000800080000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00000000000000000000000000000000000000000000000000C0C0
      C000000000008000000000000000C0C0C0008000800080008000800080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000800000000000
      0000C0C0C00080000000C0C0C000808080008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080000000C0C0
      C000000000008000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080000000C0C0
      C000C0C0C0008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008000
      0000800000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000800000000000000080000000000000008000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008000000080000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      80000000FF000000000000000000000000000000000000000000000000000000
      80000000FF000000000000000000000000000000000000000000000000000000
      80000000FF000000000000000000000000000000000000000000000000000000
      80000000FF000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000C0C0C000FFFFFF00C0C0C000FFFFFF00800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000080000000FF0080000000800000008000000080000000000080000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000080000000FF0000000000000000000000000000000000000080000000
      FF000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF0080000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000
      0000FF00FF00FF00FF00FF00FF00FF00FF008080800080808000808080008080
      80008080800080000000FFFFFF00C0C0C000FFFFFF00C0C0C000800000008080
      8000808080008080800080808000808080000000000000000000000000000000
      000000000000000080000000FF00FFFFFF00FFFFFF00000080000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000080000000FF000000000000000000000080000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000C0C0C000FFFFFF00C0C0C000FFFFFF00800000000000
      0000000000000000000000000000000000008080800080808000808080008080
      80008080800080000000FFFFFF000000FF0000008000FFFFFF00800000008080
      8000808080008080800080808000808080008080800080808000808080008080
      80008080800080808000000080000000FF00000080000000FF00808080008080
      8000808080008080800080808000808080000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFFFF00C0C0C000FFFFFF00C0C0C000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFFFF000000FF0000008000FFFFFF00800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000080000000FF00000080000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000080000000FF00FFFFFF00FFFFFF00000080000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000080000000FF000000000000000000000080000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008000000080000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000080000000FF0080000000800000008000000080000000000080000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000080000000FF0000000000000000000000000000000000000080000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000800000000000000080000000000000008000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      80000000FF000000000000000000000000000000000000000000000000000000
      80000000FF000000000000000000000000000000000000000000000000000000
      80000000FF000000000000000000000000000000000000000000000000000000
      80000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000080000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080000000800000008000000080000000800000008000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00FF000000000000000000000000000000
      000000000000000000000000000000000000FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF008000000080000000800000008000000080000000800000008000
      0000FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00FF000000000000000000000000000000
      000000000000000000000000000000000000FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00FF000000000000000000000000000000
      000000000000000000000000000000000000FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00FF000000000000000000000000000000
      000000000000000000000000000000000000FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000000000000000000000000000000000
      0000000000000000000080000000800000008000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008000000000000000000000000000008000
      0000FFFF0000FFFF0000FFFF0000800000000000000000000000000000000080
      0000000000000000000080000000FFFF0000FFFF0000FFFF0000800000000000
      0000000000000080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000008000
      0000FFFF0000FFFF0000FFFF0000800000000000000000000000000000000000
      0000008000000000000080000000FFFF0000FFFF0000FFFF0000800000000000
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000800000000000000080000000000000008000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000008000000080000000800000008000000080000000800000000000008000
      0000FFFF0000FFFF0000FFFF0000800000000000000000000000008000000080
      0000008000000080000080000000FFFF0000FFFF0000FFFF0000800000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000008000000080000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008000000080000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000008000
      0000FFFF0000FFFF0000FFFF0000800000000000000000000000000000000000
      0000008000000000000080000000FFFF0000FFFF0000FFFF0000800000000000
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000800000000000000080000000000000008000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008000000000000000000000000000008000
      0000FFFF0000FFFF0000FFFF0000800000000000000000000000000000000080
      0000000000000000000080000000FFFF0000FFFF0000FFFF0000800000000000
      0000000000000080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000000000000000000000000000000000
      0000000000000000000080000000800000008000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00FF000000000000000000000000000000
      000000000000000000000000000000000000FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00FF000000000000000000000000000000
      000000000000000000000000000000000000FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00FF000000000000000000000000000000
      000000000000000000000000000000000000FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00FF000000000000000000000000000000
      000000000000000000000000000000000000FF00FF0000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF008000000080000000800000008000000080000000800000008000
      0000FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000008000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000FFFF0000FFFF
      0000FFFF00008000000000000000000000000000000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF0000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000FFFF0000FFFF
      0000FFFF00008000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFF0000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000FFFF0000FFFF
      0000FFFF00008000000000000000008000000080000000800000008000000080
      0000008000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000FFFF0000FFFF
      0000FFFF00008000000000000000000000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000800000008000000080000000800000008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000FFFF0000FFFF
      0000FFFF00008000000000000000000000000000000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000008000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000008080
      8000808080008080800080000000800000008080800080808000808080008000
      0000800000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000080000000800000000000000000000000000000008000
      00008000000000000000000000000000000080808000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF00C0C0
      C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000FFFFFF00C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF00C0C0
      C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000FFFFFF00C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF0000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFF0000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF00C0C0
      C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000FFFFFF00C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF00C0C0
      C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000FFFFFF00C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF00000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000080000000800000000000000000000000000000000000
      0000800000008000000000000000000000008080800080000000800000008000
      0000800000008000000080000000800000008000000080000000800000008000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000FF0000000000000000
      0000000000000000000000000000000000000000000080000000800000008080
      8000808080008080800080000000800000008080800080808000808080008080
      80008000000080000000000000000000000080808000FFFFFF00800000008000
      000080000000800000008000000080000000800000008000000080000000FFFF
      FF0080000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000008000
      0000000000008000000000000000000000000000000080000000800000008000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000000000000000
      0000000000008000000000000000000000000000000000000000000000008000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000000000000000000000000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000008000000080000000800000000000000080000000800000008000
      000000000000000000000000000000000000000000000000000000000000FF00
      000000000000FF00000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000808080000000000000FFFF000000
      0000000000000000000000000000000000000000000080000000000000000000
      0000000000008000000000000000800000000000000080000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF0000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000808080000000000000FFFF00000000008080
      8000800000008000000080000000000000000000000080000000800000008000
      0000000000008000000080000000800000000000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080000000808080000000000000FFFF000000000080808000FFFF
      FF00C0C0C000FFFFFF0080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000808080000000000000FFFF000000000080808000FFFFFF00C0C0
      C000FFFFFF00C0C0C00080000000000000000000000000000000808080000000
      0000808080000000000080808000000000008080800000000000808080000000
      0000808080000000000000000000000000000000000000000000C0C0C000FFFF
      FF000000FF00FFFFFF00C0C0C000FFFFFF00C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000FFFF0000000000808080000000FF000000FF00FFFF
      FF00C0C0C000FFFFFF00800000000000000000000000FFFFFF00C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0008080800000000000000000000000000000000000FFFFFF000000
      FF000000FF000000FF00FFFFFF00C0C0C000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000000000000000
      00000000000000FFFF0000000000808080000000FF000000FF000000FF000080
      000000800000C0C0C000800000000000000000000000FFFFFF000000FF000000
      FF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0008080800080808000000000000000000000000000C0C0C0000000
      FF000000FF000000FF00FF000000C0C0C0000000000000000000000000000000
      0000808080000000000000000000000000008000000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000000000000000
      00000000000000000000808080000000FF000000FF000000FF000000FF000080
      000000800000FFFFFF00800000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008080800080808000000000000000000000000000FFFFFF000000
      FF000000FF00FF000000FF0000000000000080808000C0C0C000C0C0C0008080
      8000000000008080800000000000000000008000000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000080808000C0C0C0000000FF000000FF00008000000080
      000000800000C0C0C00080000000000000008080800080808000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000FFFFFF0080808000000000000000000000000000C0C0C000FFFF
      FF000000FF00FF0000000000000080808000C0C0C000C0C0C000FFFF00008080
      8000808080000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000080808000FFFFFF00C0C0C00000800000008000000080
      000000800000FFFFFF0080000000000000000000000000000000C0C0C0008080
      8000000000000000000000000000000000000000000000000000000000000000
      000080808000C0C0C000FFFFFF00000000000000000000000000FFFFFF00C0C0
      C000FF000000FF00000000000000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000C0C0C0000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000FFFFFF00C0C0C00080000000000000000000000000000000000000000000
      00000000000080808000FFFFFF0080808000FFFFFF0080808000FFFFFF000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C000FFFFFF0000000000C0C0C000FFFF0000C0C0C000C0C0C0008080
      8000C0C0C0000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000800000008000
      0000800000008000000080000000000000000000000000000000000000000000
      000000000000FFFFFF0080808000FFFFFF0080808000FFFFFF00C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080808000FFFF0000FFFF0000C0C0C0008080
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000000000000000000000000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0080808000FFFFFF0080808000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000C0C0C000C0C0C0008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000000000000000000000000000FF0000000000000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      00000000000000000000FF000000FF0000000000000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      0000FF00000000000000FF00000000000000FF00000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      00000000000000000000FF00000000000000FF00000000000000FF000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      0000FF00000000000000FF00000000000000FF00000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      0000FF00000000000000FF00000000000000FF00000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      00000000000000000000FF00000000000000FF00000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      00000000000000000000FF000000FF0000000000000000000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF000000FF00FFFFFF00C0C0C000FFFFFF00C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF000000FF00FFFFFF00C0C0C000FFFFFF00C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      FF000000FF000000FF00FFFFFF00C0C0C000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      FF000000FF000000FF00FFFFFF00C0C0C000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0000000
      FF000000FF000000FF00FF000000C0C0C0000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000C0C0C0000000
      FF000000FF000000FF00FF000000C0C0C0000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      FF000000FF00FF000000FF0000000000000080808000C0C0C000C0C0C0008080
      8000000000008080800000000000000000000000000000000000FFFFFF000000
      FF000000FF00FF000000FF0000000000000080808000C0C0C000C0C0C0008080
      8000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF000000FF00FF0000000000000080808000C0C0C000C0C0C000FFFF00008080
      8000808080000000000000000000000000000000000000000000C0C0C000FFFF
      FF000000FF00FF0000000000000080808000C0C0C000C0C0C000FFFF00008080
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00C0C0
      C000FF000000FF00000000000000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000C0C0C0000000000000000000000000000000000000000000FFFFFF00C0C0
      C000FF000000FF00000000000000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C000FFFFFF0000000000C0C0C000FFFF0000C0C0C000C0C0C0008080
      8000C0C0C0000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C000FFFFFF0000000000C0C0C000FFFF0000C0C0C000C0C0C0008080
      8000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080808000FFFF0000FFFF0000C0C0C0008080
      8000808080000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080808000FFFF0000FFFF0000C0C0C0008080
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000C0C0C000C0C0C0008080
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000C0C0C000C0C0C0008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000808080000080
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008000008080
      8000008000000000000000800000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      0000808080000080000000800000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000008000008080800000800000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000008000
      0000800000008000000080000000000000008080800000000000000000000080
      0000008000000080000000800000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000000000000000000000000000000000000000000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00C0C0C000FFFFFF00C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000000000000000000000000000800000000000000000000000000000008000
      0000000000000000000000000000000000000000000000000000800000008000
      0000800000008000000080000000000000000000000000000000000000000000
      0000800000000000000000000000000000000000000000000000000000008000
      0000000000000000000000000000000000000000000080000000800000008000
      0000800000008000000000000000000000000000000080000000FFFFFF000000
      00000000000000000000FFFFFF00C0C0C0008000000000808000000000000000
      0000000000000000000000000000000000000000000000000000800000008000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000000000000000000000000000000000000000000000800000008000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000008000000000000000000000000000000000000000800000000000
      0000000000000000000000000000000000000000000000000000800000008000
      0000800000008000000000000000000000000000000080000000C0C0C0000000
      0000808080000000000080000000800000008000000080000000000000008080
      8000000000000000000000000000000000000000000080000000800000008000
      0000800000008000000000000000000000000000000080000000800000008000
      0000800000008000000000000000000000000000000000000000800000008000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000008000000000000000000000000000000000000000800000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000000000000000000000000000080000000FFFFFF000000
      00000000000000000000C0C0C000FFFFFF00C0C0C000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000800000008000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000000000000000000000000000000000000000000000800000008000
      0000000000008000000000000000000000000000000000000000000000000000
      0000000000008000000000000000000000000000000000000000800000000000
      0000000000000000000000000000000000000000000000000000800000000000
      000080000000800000000000000000000000000000000000000000000000FFFF
      FF0080000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF008000
      0000000000000000000000000000000000000000000000000000000000008000
      0000000000000000000000000000800000000000000000000000000000008000
      0000000000000000000000000000000000000000000000000000800000000000
      0000000000000000000080000000800000000000000000000000000000000000
      0000800000000000000000000000000000000000000000000000000000008000
      0000000000000000000000000000000000008000000080000000000000000000
      0000000000008000000000000000000000000000000080808000000000008000
      000080000000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C0008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0C0C000FFFFFF00C0C0C000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000080000000800000008000000080000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000808000000000000000000000FFFF000000
      0000000000000080800000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000080800000808000FFFFFF000000
      00000080800000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000008080000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000080800000FFFF00FFFFFF0000000000000000000000
      0000FFFFFF0000FFFF0000808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008080000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF0000000000000000000000FF0000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00808080000000
      000000000000808080000000000000FFFF000080800080808000FFFFFF000000
      000000FFFF000080800000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF0000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF000000
      0000000000000000000000000000800000000000000000000000000000008080
      800080808000FFFFFF0000000000008080000000000000000000FFFFFF000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      000000000000FF000000FF0000000000000000000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF00000000000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000008000000080000000000000008080800000000000FFFF
      FF0080808000FFFFFF00FFFFFF0000000000FFFFFF0000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF00000000000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008080800000000000808080000000
      000000000000FFFFFF0000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000080808000000000000000
      0000FFFFFF00808080000000000000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000808080000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00800000008000000080000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000008080800000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF008000000080000000800000008000000080000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF008000000080000000FFFFFF00FFFFFF00FFFFFF00808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080808000FFFFFF00FFFFFF00FFFFFF008000000080000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00800000008000000080000000800000008000000080000000800000008000
      0000800000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000008000000080000000800000008000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF008000000080000000FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF008000000080000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF008000000080000000800000008000000080000000FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00800000008000000080000000FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000080000000800000008000
      0000800000008000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000000000000000000080000000000000000000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000008000000080000000800000008000
      0000800000008000000080000000000000000000000000000000000000000000
      0000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00800000000000000000000000000000000000
      00000000000000000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF0000000000000000000000000000000000000000008000
      0000000000000000000080000000000000008000000000000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0080000000000000000000000080808000008080008080
      8000008080008080800080000000FFFFFF008000000080000000800000008000
      00008000000080000000FFFFFF00800000000000000000000000000000000000
      00000000000000000000FFFFFF00C0C0C0000000FF000000FF00FFFFFF00C0C0
      C000FFFFFF00C0C0C00000000000000000000000000000000000000000008000
      0000000000000000000080000000000000008000000000000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF0080000000000000000000000000808000808080000080
      8000808080000080800080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00800000000000000000000000000000000000
      00000000000000000000C0C0C0000000FF000000FF000000FF000000FF00FFFF
      FF00C0C0C000FFFFFF0000000000000000000000000000000000000000000000
      0000800000008000000080000000000000008000000000000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0080000000000000000000000080808000008080008080
      8000008080008080800080000000FFFFFF00800000008000000080000000FFFF
      FF00800000008000000080000000800000000000000000000000000000000000
      00000000000000000000FFFFFF000000FF000000FF000000FF000000FF00FF00
      0000FF000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000080000000000000008000000080000000800000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0080000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF0080000000000000000000000000808000808080000080
      8000808080000080800080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0080000000FFFFFF0080000000000000000000000000000000FFFFFF00C0C0
      C000FFFFFF0000000000C0C0C0000000FF000000FF000000FF000000FF00FF00
      0000FF000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000080000000000000008000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0080000000000000000000000080808000008080008080
      8000008080008080800080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00800000008000000000000000000000000000000000000000C0C0C000FFFF
      FF008000800000000000FFFFFF00C0C0C0000000FF000000FF00FF000000FF00
      0000FF000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0080000000FFFFFF000000000000000000FFFFFF008000
      0000800000008000000080000000000000000000000000808000808080000080
      8000808080000080800080000000800000008000000080000000800000008000
      0000800000000000000000000000000000000000000000000000FFFFFF008000
      80008080800000000000C0C0C000FFFFFF00C0C0C000FF000000FF000000FF00
      0000FF000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008000
      0000FFFFFF008000000000000000000000000000000080808000008080008080
      8000008080008080800000808000808080000080800080808000008080008080
      8000008080000000000000000000000000000000000000000000C0C0C0008080
      80008000800000000000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000FFFFFF00C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008000
      0000800000000000000000000000000000000000000000808000808080000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080000000000000000000000000000000000000000000FFFFFF008000
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000080000000800000008000000080000000800000008000
      0000000000000000000000000000000000000000000080808000808080000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000008080000000000000000000000000000000000000000000C0C0C000FFFF
      FF008000800080008000800000008080800080000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000808000808080000080
      80000000000000FFFF00000000000000000000FFFF0000000000808080000080
      8000808080000000000000000000000000000000000000000000FFFFFF00C0C0
      C000FFFFFF0080000000808080008000000080808000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000800000008000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800080000000808080008080800080000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000000000000000000000000000000000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008000000000000000000000000000000000000000800000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000008080800000000000000000000000000000000000000000000000
      0000800000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000008080
      8000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000008080
      8000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000008080
      8000800000000000000000000000000000000000000000000000000000008000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000800000000000000000000000000000000000000000000000800000008000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000000000000000000000000000000000000080000000800000008000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008000000000000000000000000000000000000000800000008000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000000000000000000000000000080808000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800080000000808080008080800080000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000800000008000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080008000800080008000
      800000000000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C0000000000000000000000000008000
      8000800080008000800080008000000000000000000080008000800080008000
      800080008000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000800080008000800000000000000000000000000000000000000000000000
      0000000000000000000000000000800080008000800080008000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000C0C0C0000000000000000000000000008000
      8000800080008000800000000000000000000000000000000000800080008000
      800080008000000000000000000000000000C0C0C000C0C0C000C0C0C0000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000008000800080008000800080008000800080008000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C0000000000000000000000000008000
      8000800080008000800000000000000000000000000000000000800080008000
      800080008000000000000000000000000000C0C0C00000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000800080000000000000000000000000000000000000000000000000008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000800080000000000000000000000000000000000000000000000000008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000800080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000800080000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000080000000000000000000
      0000000000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      0000000000000000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000800000000000
      0000000000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000000080000000800000008000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000808080008080800000000000000000000000000000000000000000000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF000000000080808000808080008080
      800080808000808080000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000808080008080800000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000808080008080800000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF000000000080808000808080008080
      8000808080008080800000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      000080808000808080000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000080808000808080000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000080808000808080008080
      800080808000808080000000000000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000080808000808080000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000080808000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000008000000080000000800000008000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000008080800000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      800000000000000000000000000000000000C0C0C00000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      800000000000000000000000000000000000C0C0C00000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00000000000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00000000000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000000000000000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000080808000000000008000
      0000800000008000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000080808000000000008000
      0000800000008000000000000000000000000000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000C0C0C000C0C0C0008080
      8000000000008080800000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000C0C0C000C0C0C00000000000000000008000
      0000800000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000C0C0C000C0C0C00000000000000000008000
      0000800000000000000000000000000000000000000080000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000000000000080000000000000000000000000000000000000000000
      000000000000000000000000000080808000C0C0C000C0C0C000FFFF00008080
      8000808080000000000000000000000000000000000000000000000000008080
      8000C0C0C000C0C0C000C0C0C000C0C0C000FFFF0000C0C0C000808080000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C000C0C0C000C0C0C000C0C0C000FFFF0000C0C0C000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000000000000000000000000000000000000000000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000C0C0C000000000000000000000000000000000008080800000000000C0C0
      C000C0C0C000C0C0C0000000000000000000C0C0C000FFFF0000C0C0C0000000
      000080808000000000000000000000000000000000008080800000000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000FFFF0000C0C0C0000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000FFFF0000C0C0C000C0C0C0008080
      8000C0C0C0000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C0000000000000000000C0C0C000C0C0C000FFFF0000C0C0
      C000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000FFFF0000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080808000FFFF0000FFFF0000C0C0C0008080
      8000808080000000000000000000000000000000000000000000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000C0C0C000C0C0C0008080
      8000000000000000000000000000000000000000000000000000C0C0C000FFFF
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000C0C0C000FFFF
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C000C0C0C0000000000000000000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800000000000FFFF
      0000FFFFFF00C0C0C0000000000000000000C0C0C000C0C0C000C0C0C0000000
      000080808000000000000000000000000000000000008080800000000000FFFF
      0000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000800000000000000000000000000000000000000000000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFF0000FFFFFF00FFFF0000C0C0C000C0C0C000C0C0C000808080000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFF0000FFFFFF00FFFF0000C0C0C000C0C0C000C0C0C000808080000000
      0000000000000000000000000000000000000000000080000000000000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000000000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000C0C0C000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000C0C0C000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000080000000800000008000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000000000000080800000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000C0C0C000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000000000000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C0000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C0000000000000808000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C00000FFFF0000FFFF0000FFFF00C0C0C000C0C0
      C00000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF000000
      0000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000808000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000808080008080800080808000C0C0C000C0C0
      C00000000000C0C0C0000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000000000008080000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000008080000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C00000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000000000000000000000000000000000000000000000000000000000000000
      00000080800000808000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000C0C0C00000000000C0C0C00000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000080800000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C00000000000C0C0C0000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C00000000000008080000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000C0C0C00000000000C0C0C00000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000080800000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000080800000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000000000000000000000000000FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C00000000000C0C0C00000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000080000000800000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008000000080000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008000000080000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008000000080000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000080000000800000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000080000000800000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008000000080000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008000000080000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008000000080000000000000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000080000000800000008000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000500100000100010000000000800A00000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFF80FFFFFFFC1F8001003FFFFFFE0F
      BFFD807FFFFFFF07BFFDC1FFC07FFF83BFFDE0FF803FFF83BFFDF07F801FF7C3
      BFFDF83F800FE3E1FFFDFC17C007C1A0CD57FE07E0038094D557FF03F0010018
      CD53FF81F8018018D555FF00FC01C01FCCB3FFC0FE01C03FFFFFFFE8FF03C0FF
      8001FFF1FFFFC1FFFFFFFFFFFFFFC3FF8000FFFFFFFFFFFFBF7EFFFFFFFFFFFF
      BD5EFFFFFFFFFFFFBE3EFFFFFFFFFFFFBF7EFFFFFFFFFFFFB80EF81FE7E7E7E7
      B80EF81FF00FF3CF80000000F81FF99FB80E000000000000B80EF81F00000000
      BF7EF81FF81FF99FBE3EFE7FF00FF3CFBD5EFE7FE7E7E7E7BF7EFC3FFFFFFFFF
      8000F81FFFFFFFFFFFFFFFFFFFFFFFFF8000800080008000BFFEBF7EBFFEB80E
      BFFEBF7EBFFEB80EBFFEBF7EBF7EB80EBFE0BC1EBF7EB80EBEE0AC1ABF7EBFFE
      BF60B416BD5EBF7EB0208000BE3EBE3EBF60B416BF7EBD5EBEE0AC1ABFFEBF7E
      BFE0BC1EB80EBF7EBFFEBF7EB80EBF7EBFFEBF7EB80EBFFEBFFEBF7EB80EBFFE
      8000800080008000FFFFFFFFFFFFFFFFD1FDFFFFFFFF8000AEFEFFFFFFFFBFFE
      E77EDFDFFFFFBFFEE37EDFDFFFFFBFFEE1BDDFDFFFFF83FEE0C3DFEFFFFF83BE
      E2FFDFEFFEFF837EF07FDFEFFC7F8206F17FDF77F83F837EF83FE837F01F83BE
      F8BFEF77FFFF83FEFC1FF7FBFFFFBFFEFC1FF7FBFFFFBFFEFE0FFBFBFFFFBFFE
      FE0FFBFDFFFF8000FF1FFDFDFFFFFFFFFFFFFFFFFFFF91FFFFFFFFFFFE7FEEFF
      80070001FC3FE77F90670001F99FE3BEEF9F0001FDBFE1DDDFEF0001EDB7E0EB
      BFF70001C183E2F7BFF700019FF9F07FBFFB00019FF9F17FDFFB0001C183F83F
      EFF70001EDB7F8BFF7CF0001FDBFFC1F98330001F99FFC1F80030001FC3FFE0F
      FFFF0001FE7FFE0FFFFFFFFFFFFFFF1FFFFF8B8FEE7FFFFFFFFFBBEFE77FF07F
      FF8F988FEAFFEFBFFF0FBABFE67FDFDFF801888FFFFFBFEFF801FFFF803FBFEF
      F8018003803CBFF7F00100018038DFF48001000080012FF48001000080032FF3
      B00100008003D7F3F00180008003EBE7E001C0018003F5CFF801F00F8003F9CF
      FFFFF807FE07F9FFFFFFF807FF0FFFFFFFFFFFFFCEDFDCDFFFE3F07FD55FCD4F
      FFDDEF9FD55FD55FFFDFDFEFCD5FCCC7E3EFBFF7FFFFFFFFEDEFBFF7803F803F
      DDF7BFFB803C803CDEF7DFFB80388038BEF7EFFB80018001BF7BF7FB80038003
      DF7BFBFB80038003EFBBFBF780038003FFB7FDF780038003FFCFFDEF80038003
      FFFFFEEFFE07FE07FFFFFF1FFF0FFF0FFFDFFFFFFFFFFFFFFF8FFFFFFFFFFFFF
      FFC5FEFFFFFFFFFFFFE1FC7FFFFFFFFF1E31F83FFFFFFFFF0021FEFFFFF7EFFF
      003FEEEFC1F7EF838007CFE7C3FBDFC380078383C7FBDFE38007CFE7CBFBDFD3
      000FEEEFDCF7EF3B000FFEFFFF0FF0FF100FF83FFFFFFFFFE007FC7FFFFFFFFF
      E007FEFFFFFFFFFFE3C7FFFFFFFFFFFFFFFFFFFFFFFFFF8FFFE3F07F000FFC89
      FFDDEF9F000FF800FFDFDFEF000FFC01E3EFBFF7000FF800EDEFBFF7000FF800
      DDF7BE3B000F9000DEF7DCDB000F0001BC77ECFB008EA001B9BBF49B1144005B
      D9FBFA1B0AB8401FD93BFBF7057C001FFC37FDF7FAFC80BFFF8FFDEFFDF8A03F
      FFFFFEEFFE04D17FFFFFFF1FFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      EAAFFFFFFFFFE00FFFFFFFFFFFFFE00FEEEFC015A803E00FFEFFC01FF803E00F
      E00FC01DB803E00FE00FC01FF803E00FE00FC005A003E00FE00FC01FF803E00F
      E00FC01DB803E00FE00FC01FF803FEFFE00FC015A803EEEFE00FFFFFFFFFFFFF
      E00FFFFFFFFFEAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3FFFFFFFC00F801
      ED9FFC018000F801ED6FFC010000F801ED6FFC010000F801F16F000100008001
      FD1F000100018001FC7F000100038001FEFF000100038001FC7F000300038001
      FD7F000700038001F93F000F0FC3801FFBBF00FF0003801FFBBF01FF8007801F
      FBBF03FFF87F801FFFFFFFFFFFFFFFFFFC3FFFFFFFFFEFEFF81FFFF9EFFFAAAB
      FBDFFFC1D7FFFFEFF3CFFE3BDBFFEFC3F7EFF1FBBDFFFEC7E7FF8FFBBEFFEE07
      E7FFBFF7BF7FFE0FE7FFBFF7BFBFEE03E7FFDFF7DFDFFE07E7E7DFEFDFEFEE0F
      E7C7EFEFEFF7FE1FF787F7EFEFFBEE2FF3C7FBDFF3FDFE7FFB9FFCDFFCF3AAAB
      F81FFF1FFF0FFFFFFC3FFFFFFFFFEFEFFFFFFFFFFFFFFFFFFFFFFFE7FFFFFFFF
      FFFFFF87004002008181FE07804002018181F807E0418207C183E007F841821F
      C183C007FE43C27FE187FFFFFFC3C3FFE187FFFFFFC7E3FFF18FC007BFC7E3FD
      F18FE007BBCFF3DDF99FF807D9CFF39BF99FFE07E0DFFB07FDBFFF87F9FFFF9F
      FFFFFFE7FBFFFFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC01FFFFFF01FF01
      FC01FC01FF01FF01FC01FC01FF01FF018001FC01E001E00180018001E001E001
      80018001E001E00180018001E001E00180018001000F000F800F8001000F000F
      800F800F000F000F800F800F000F000F800F800F000F000F800F800F01FF01FF
      FFFF800F01FF01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF807FF9FF927FFFD
      F807FF8FF927F7FFF003F70FFFFFE3FBE003F31FC00FE3F7E003F01F800FF1E7
      C003F03F800FF8CF8003F007800FFC1F8803F00F800FFE3FF803F01F800FFC1F
      F803F03F800FF8DFF807F07F800FE1E7F80FF0FF800FC3F3FC1FF1FF800FCFF9
      FE7FF3FF800FFFFFFFFFF7FFFFFFFFFFEFEFFFFCFFFCFFFFAAA4FFF8FFF8FFFF
      FFF8FFF1FFF18FF1EF01F023F0239FF9FE03E007E007AFF5EC03C00FC00FF7EF
      FC0380078007F81FEC0380078007F81FFC0380078007F81FEE0780078007F81F
      FF0F80078007F81FEFEF80078007F7EFFFFFC00FC00FAFF5AAABE01FE01F9FF9
      FFFFF03FF03F8FF1EFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC001C007
      C007001F80018003C007000F80010001C007000780010001C007000380010001
      C007000180010000C007000080010000C007001F80018000C007001F8001C000
      C007001F8001E001C0078FF18001E007C00FFFF98001F007C01FFF758001F003
      C03FFF8F8001F803FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF07FFFFFFFFF
      FC3FEF9FFC3FFFFFF3CFDFEFF3CFFFFFEFF7BFB7EFF7E38FEFF7BF97EFF7E10F
      DFFBBF9BDFFBF01FDFFBDE1BDFFBF83FDFFBEC9BDFFBE00FDFFBF49BDFFBC007
      EFF7F89BEFF7C007EFF7FA17EFF7EC6FF3CFFDF7F3CFFC7FFC3FFDEFFC3FFC7F
      FFFFFEEFFFFFFFFFFFFFFF1FFFFFFFFFFFFFFDFFFFFFFFFFFFDFFC7FFFE3FFFF
      FFDFFD9FFFDDFFFFFFAFFDE7FFDF7D87FFAFFDF9E3EF7DBB7F6FFDFEEDAFBDBD
      BF6F81FCDD9783BDBEEFBFE3DE97DBBDDEF7BFEFBE17DB9BDDF7BFEFBC9BEBA7
      EDF7BFEFDC9BF7BFEBF79FDFDC9BF7BFF3EFE7DFFE17F7BFF7DFF9DFFF8FFFFF
      FFBFFE5FFFFFFFFFFF7FFF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFFFFFF
      BFFFFFFFDFFFF81FDFFF8003BFFFE7E7EFFFBFFBBFFFDFFBF7FFBFFBBFFFBFFD
      FBFFBFFBBFFF7FFEFDFFBFFBBFFF7FFEFEFFBFFBDFFF7FFEFF7FBFFBDFFF7FFE
      FFBFBFFBEFFFBFFDFFDFBFFBEFFFDFFBFFEFBFFBF3FFE7E7FFF78003FCF3F81F
      FFFBFFFFFF0FFFFFFFFDFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object CaptureEMF_Dialog: TSaveDialog
    DefaultExt = 'emf'
    Filter = 'Enhanced metafile (*.emf)|*.emf'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 128
    Top = 132
  end
  object ConvertPopup: TPopupMenu
    AutoHotkeys = maManual
    AutoPopup = False
    Left = 156
    Top = 172
    object N13: TMenuItem
      Caption = '1'
    end
    object N21: TMenuItem
      Caption = '2'
    end
  end
  object PopupMenuDVI: TPopupMenu
    OnPopup = PopupMenuDVIPopup
    Left = 364
    Top = 108
    object tex1: TMenuItem
      Caption = 'tex'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
    object pgf1: TMenuItem
      Caption = 'pgf'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
    object pstricks1: TMenuItem
      Caption = 'pstricks'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
    object pdf1: TMenuItem
      Caption = 'eps'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
    object png1: TMenuItem
      Caption = 'png'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
    object bmp1: TMenuItem
      Caption = 'bmp'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
    object metapost1: TMenuItem
      Caption = 'metapost'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
    object tikz1: TMenuItem
      Caption = 'tikz'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
    object emf1: TMenuItem
      Caption = 'emf'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
    object none1: TMenuItem
      Caption = 'none'
      RadioItem = True
      OnClick = DVI_Format_Click
    end
  end
  object PopupMenuPdf: TPopupMenu
    OnPopup = PopupMenuPdfPopup
    Left = 400
    Top = 108
    object MenuItem1: TMenuItem
      Caption = 'tex'
      RadioItem = True
      OnClick = Pdf_Format_Click
    end
    object MenuItem2: TMenuItem
      Caption = 'pgf'
      RadioItem = True
      OnClick = Pdf_Format_Click
    end
    object MenuItem4: TMenuItem
      Caption = 'pdf'
      RadioItem = True
      OnClick = Pdf_Format_Click
    end
    object MenuItem5: TMenuItem
      Caption = 'png'
      RadioItem = True
      OnClick = Pdf_Format_Click
    end
    object MenuItem7: TMenuItem
      Caption = 'metapost'
      RadioItem = True
      OnClick = Pdf_Format_Click
    end
    object tikz2: TMenuItem
      Caption = 'tikz'
      RadioItem = True
      OnClick = Pdf_Format_Click
    end
    object MenuItem8: TMenuItem
      Caption = 'epstopdf'
      RadioItem = True
      OnClick = Pdf_Format_Click
    end
    object none2: TMenuItem
      Caption = 'none'
      RadioItem = True
      OnClick = Pdf_Format_Click
    end
  end
  object OpenBitmapDlg: TOpenPictureDialog
    Filter = 
      'All Images (*.jpg;*.jpeg;*.bmp;*.png)|*.jpg;*.jpeg;*.bmp;*.png|J' +
      'PEG (*.jpg;*.jpeg)|*.jpg;*.jpeg|BMP (*.bmp)|*.bmp|PNG (*.png)|*.' +
      'png'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 200
    Top = 120
  end
end
