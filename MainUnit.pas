unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs,
  Menus, ComCtrls, ToolWin, Printers, Clipbrd,
  CADSys4, CS4BaseTypes, CS4Tasks, CS4Shapes,
  ExtCtrls, ImgList, XUtils, XXmlDom,
  StdCtrls, ExtDlgs, ActnList, CustomizeDlg, ActnMan,
  ActnCtrls, ActnMenus, Grids, ValEdit, HH, hh_funcs, AppEvnts,
  D6OnHelpFix, Buttons, Options0, Registry;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    TheDrawing: TDrawing2D;
    StatusBar1: TStatusBar;
    LocalPopUp: TPopupMenu;
    LocalPrg: TCADPrg2D;
    Accept1: TMenuItem;
    Cancel1: TMenuItem;
    N1: TMenuItem;
    Zoomarea1: TMenuItem;
    Zoomin1: TMenuItem;
    Zoomout1: TMenuItem;
    Zoomall1: TMenuItem;
    Panning1: TMenuItem;
    N2: TMenuItem;
    Showgrid1: TMenuItem;
    Keepaspect1: TMenuItem;
    Usesnap1: TMenuItem;
    Useorto1: TMenuItem;
    File1: TMenuItem;
    Useareatoselectobjects1: TMenuItem;
    OpenDoc1: TMenuItem;
    Save1: TMenuItem;
    CADOpenDlg: TOpenDialog;
    CADSaveDlg: TSaveDialog;
    New1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    N4: TMenuItem;
    Print1: TMenuItem;
    Actualview1: TMenuItem;
    Fit1: TMenuItem;
    Scale1: TMenuItem;
    PrintDialog1: TPrintDialog;
    Copytoclipboard1: TMenuItem;
    Setpoint1: TMenuItem;
    Test1: TMenuItem;
    OpenDialog1: TOpenDialog;
    RichEdit1: TRichEdit;
    ImportEMF1: TMenuItem;
    EMFOpenDialog: TOpenPictureDialog;
    ActionList1: TActionList;
    DeleteSelected: TAction;
    MoveUp: TAction;
    MoveDown: TAction;
    MoveLeft: TAction;
    MoveRight: TAction;
    MoveUpPixel: TAction;
    MoveDownPixel: TAction;
    MoveLeftPixel: TAction;
    MoveRightPixel: TAction;
    FlipV: TAction;
    SelectAll: TAction;
    SelNext: TAction;
    SelPrev: TAction;
    FlipH: TAction;
    RotateCounterclockW: TAction;
    RotateClockW: TAction;
    RotateCounterclockWDegree: TAction;
    RotateClockWDegree: TAction;
    Grow10: TAction;
    Shrink10: TAction;
    Grow1: TAction;
    Shrink1: TAction;
    MoveForward: TAction;
    MoveBackward: TAction;
    MoveToFront: TAction;
    MoveToBack: TAction;
    StartRotate: TAction;
    StartMove: TAction;
    Edit1: TMenuItem;
    MoveForward1: TMenuItem;
    MoveForward2: TMenuItem;
    MoveForward3: TMenuItem;
    MoveForward4: TMenuItem;
    Edit2: TMenuItem;
    Selectall1: TMenuItem;
    Selectnext1: TMenuItem;
    Selectprevious1: TMenuItem;
    Deleteselected1: TMenuItem;
    N6: TMenuItem;
    ransform1: TMenuItem;
    Fliphorizontally1: TMenuItem;
    Flipvertically1: TMenuItem;
    N5: TMenuItem;
    Moveup1: TMenuItem;
    Movedown1: TMenuItem;
    Moveleft1: TMenuItem;
    Moveright1: TMenuItem;
    Grow101: TMenuItem;
    Grow102: TMenuItem;
    Grow11: TMenuItem;
    Shrink11: TMenuItem;
    N8: TMenuItem;
    Startmove1: TMenuItem;
    Startrotate1: TMenuItem;
    Move1: TMenuItem;
    N9: TMenuItem;
    Moveup1pixel1: TMenuItem;
    Movedown1pixel1: TMenuItem;
    Moveleft1pixel1: TMenuItem;
    Moveright1pixel1: TMenuItem;
    ImageList2: TImageList;
    N12: TMenuItem;
    InsertLine: TAction;
    InsertRectangle: TAction;
    InsertEllipse: TAction;
    InsertArc: TAction;
    InsertPolyline: TAction;
    InsertPolygon: TAction;
    InsertSpline: TAction;
    InsertText: TAction;
    Insert1: TMenuItem;
    Insertline1: TMenuItem;
    InsertRectangle1: TMenuItem;
    InsertEllipse1: TMenuItem;
    InsertArc1: TMenuItem;
    InsertPolyline1: TMenuItem;
    InsertPolygon1: TMenuItem;
    InsertText1: TMenuItem;
    InsertCircle: TAction;
    Rotate1: TMenuItem;
    Rotateclockwise1: TMenuItem;
    Rotatecounterclockwise1: TMenuItem;
    Rotateclockwise1deg1: TMenuItem;
    Rotatecounterclockwise1deg1: TMenuItem;
    InsertCircle1: TMenuItem;
    InsertStar: TAction;
    NewDoc: TAction;
    OpenDoc: TAction;
    SaveDoc: TAction;
    Print: TAction;
    ZoomArea: TAction;
    ZoomIn: TAction;
    ZoomOut: TAction;
    ZoomAll: TAction;
    HandTool: TAction;
    Deleteselected2: TMenuItem;
    Scalestandard1: TMenuItem;
    SaveAs: TAction;
    Saveas1: TMenuItem;
    N10: TMenuItem;
    InsertSector: TAction;
    InsertSegment: TAction;
    Insertsector1: TMenuItem;
    Insertsegment1: TMenuItem;
    InsertClosedSpline: TAction;
    DuplicateSelected: TAction;
    Duplicateselected1: TMenuItem;
    Options1: TMenuItem;
    Insertstar1: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    Help1: TMenuItem;
    U1: TMenuItem;
    Zoomarea2: TMenuItem;
    Zoomin2: TMenuItem;
    Zoomout2: TMenuItem;
    Zoomall2: TMenuItem;
    Panning2: TMenuItem;
    Setpoint2: TMenuItem;
    N15: TMenuItem;
    Showgrid2: TMenuItem;
    Keepaspect2: TMenuItem;
    Usesnap2: TMenuItem;
    Useorto2: TMenuItem;
    Useareatoselectobjects2: TMenuItem;
    ShowGrid: TAction;
    KeepAspect: TAction;
    SnapToGrid: TAction;
    UseOrto: TAction;
    N11: TMenuItem;
    AreaSelect: TAction;
    Areaselect2: TMenuItem;
    AreaSelectInside: TAction;
    Areaselectinsideonly1: TMenuItem;
    ClipboardCopy: TAction;
    ClipboardPaste: TAction;
    ClipboardCut: TAction;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    CaptureEMF1: TMenuItem;
    N14: TMenuItem;
    Copytoclipboard2: TMenuItem;
    InsertCubicSpline: TAction;
    InsertClosedCubicSpline: TAction;
    ConvertToPolyline: TAction;
    HatchingImageList: TImageList;
    BitBtn1: TBitBtn;
    N16: TMenuItem;
    pXsettings1: TMenuItem;
    ProgressBar1: TProgressBar;
    Undo: TAction;
    Redo: TAction;
    N17: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    Panel1: TPanel;
    LocalView: TCADViewport2D;
    ShowRulers1: TMenuItem;
    Help2: TMenuItem;
    About1: TMenuItem;
    CustomTransform: TAction;
    Areaselect3: TMenuItem;
    Panel2: TPanel;
    Ruler2: TRuler;
    Panel3: TPanel;
    Ruler1: TRuler;
    Scale2: TMenuItem;
    InsertCurve: TAction;
    InsertClosedCurve: TAction;
    Insertcurve1: TMenuItem;
    Insertclosedcurve1: TMenuItem;
    HScrollBar: TScrollBar;
    VScrollBar: TScrollBar;
    ShowRulers: TAction;
    ShowScrollBars: TAction;
    Showscrollbars1: TMenuItem;
    CaptureEMF_Dialog: TSaveDialog;
    Converttopolyline1: TMenuItem;
    ConvertPopup: TPopupMenu;
    ConvertTo: TAction;
    N13: TMenuItem;
    N21: TMenuItem;
    DoConvertTo: TAction;
    CaptureEMF: TAction;
    PreviewLaTeX: TAction;
    PreviewPdfLaTeX: TAction;
    Tools1: TMenuItem;
    PreviewLaTeX1: TMenuItem;
    PreviewPdfLaTeX1: TMenuItem;
    N7: TMenuItem;
    Recentfiles1: TMenuItem;
    OpenRecent: TAction;
    PreviewSVG: TAction;
    Preview1: TMenuItem;
    PreviewSVG1: TMenuItem;
    PreviewEPS: TAction;
    PreviewPDF: TAction;
    PreviewPNG: TAction;
    PreviewBMP: TAction;
    PreviewEMF: TAction;
    PreviewEPS1: TMenuItem;
    PreviewPDF1: TMenuItem;
    PreviewPNG1: TMenuItem;
    PreviewBMP1: TMenuItem;
    PreviewBMP2: TMenuItem;
    EPSOpenDialog: TOpenDialog;
    ImportMetafile: TAction;
    ImportEPS: TAction;
    ImportEPS1: TMenuItem;
    ConvertToGrayScale: TAction;
    N19: TMenuItem;
    Converttograyscale1: TMenuItem;
    ControlBar1: TControlBar;
    ToolBar1: TToolBar;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton13: TToolButton;
    BasicModeBtn: TToolButton;
    AreaSelect1: TToolButton;
    ToolButton17: TToolButton;
    ClipboardCutBtn: TToolButton;
    ClipboardCopyBtn: TToolButton;
    ClipboardPasteBtn: TToolButton;
    ToolButton7: TToolButton;
    UndoBtn: TToolButton;
    RedoBtn: TToolButton;
    ToolButton14: TToolButton;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    PanningBtn: TToolButton;
    ToolButton12: TToolButton;
    ToolButton4: TToolButton;
    ToolButton2: TToolButton;
    ToolButton8: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ControlBar2: TControlBar;
    ToolBar2: TToolBar;
    InsertLineBtn: TToolButton;
    InsertRectangleBtn: TToolButton;
    InsertCircleBtn: TToolButton;
    InsertEllipseBtn: TToolButton;
    InsertArcBtn: TToolButton;
    InsertSectorBtn: TToolButton;
    InsertSegmentBtn: TToolButton;
    InsertPolylineBtn: TToolButton;
    InsertPolygonBtn: TToolButton;
    InsertCurveBtn: TToolButton;
    InsertClosedCurveBtn: TToolButton;
    InsertTextBtn: TToolButton;
    InsertStarBtn: TToolButton;
    ControlBar3: TControlBar;
    ToolBar3: TToolBar;
    ComboBox1: TComboBox;
    ComboBox3: TComboBox;
    ComboBox2: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ScalePhysical: TAction;
    Scalephysicalunits1: TMenuItem;
    InsertBezierPath: TAction;
    InsertBezierPathBtn: TToolButton;
    InsertClosedBezierPath: TAction;
    InsertClosedBezierPathBtn: TToolButton;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    ImageTool: TAction;
    ImagetoEPStool1: TMenuItem;
    PreviewLaTeX_PS: TAction;
    ToolButton20: TToolButton;
    PreviewLaTeXDVIPS1: TMenuItem;
    ToolButton21: TToolButton;
    Pictureinfo1: TMenuItem;
    SmoothBezierNodesAction: TAction;
    Smoothbeziernodes1: TMenuItem;
    InsertBezierpath1: TMenuItem;
    InsertclosedBezierpath1: TMenuItem;
    PopupMenuDVI: TPopupMenu;
    tex1: TMenuItem;
    pstricks1: TMenuItem;
    pgf1: TMenuItem;
    pdf1: TMenuItem;
    png1: TMenuItem;
    emf1: TMenuItem;
    bmp1: TMenuItem;
    metapost1: TMenuItem;
    PopupMenuPdf: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    NewWindow: TAction;
    Newwindow1: TMenuItem;
    procedure InsertLineBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InsertArcBtnClick(Sender: TObject);
    procedure InsertPolylineBtnClick(Sender: TObject);
    procedure Accept1Click(Sender: TObject);
    procedure LocalPrgStartOperation(Sender: TObject; const
      Operation: TCADStateClass;
      const Param: TCADPrgParam);
    procedure LocalPrgEndOperation(Sender: TObject; const
      Operation: TCADStateClass;
      const Param: TCADPrgParam);
    procedure LocalPrgStopOperation(Sender: TObject; const
      Operation: TCADStateClass;
      const Param: TCADPrgParam);
    procedure Cancel1Click(Sender: TObject);
    procedure Zoomarea1Click(Sender: TObject);
    procedure Zoomout1Click(Sender: TObject);
    procedure Zoomin1Click(Sender: TObject);
    procedure Zoomall1Click(Sender: TObject);
    procedure Panning1Click(Sender: TObject);
    procedure LocalViewMouseMove2D(Sender: TObject; Shift:
      TShiftState; WX,
      WY: Single; X, Y: Integer);
    procedure LocalPrgDescriptionChanged(Sender: TObject);
    procedure LocalPopUpPopup(Sender: TObject);
    procedure ShowGridExecute(Sender: TObject);
    procedure KeepAspectExecute(Sender: TObject);
    procedure SnapToGridExecute(Sender: TObject);
    procedure UseOrtoExecute(Sender: TObject);
    procedure MoveBtnClick(Sender: TObject);
    procedure RotateBtnClick(Sender: TObject);
    procedure Useareatoselectobjects1Click(Sender: TObject);
    procedure InsertRectangleBtnClick(Sender: TObject);
    procedure InsertEllipseBtnClick(Sender: TObject);
    procedure InsertPolygonBtnClick(Sender: TObject);
    procedure InsertSplineBtnClick(Sender: TObject);
    procedure OpenDoc1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Merge1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure InsertTextBtnClick(Sender: TObject);
    procedure LocalViewDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action:
      TCloseAction);
    procedure Exit1Click(Sender: TObject);
    procedure Actualview1Click(Sender: TObject);
    procedure Fit1Click(Sender: TObject);
    procedure Scale1Click(Sender: TObject);
    procedure Setpoint1Click(Sender: TObject);
    procedure LocalViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Test1Click(Sender: TObject);
    procedure LocalViewMouseUp2D(Sender: TObject; Button:
      TMouseButton;
      Shift: TShiftState; WX, WY: Single; X, Y: Integer);
    procedure SelectBtnClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure LocalViewMouseDown2D(Sender: TObject; Button:
      TMouseButton;
      Shift: TShiftState; WX, WY: Single; X, Y: Integer);
    procedure ImportMetafileExecute(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SendUserEventExecute(Sender: TObject);
    procedure InsertCircleExecute(Sender: TObject);
    procedure InsertStarExecute(Sender: TObject);
    procedure BasicModeBtnClick(Sender: TObject);
    procedure Scalestandard1Click(Sender: TObject);
    procedure SaveAsExecute(Sender: TObject);
    procedure InsertSectorExecute(Sender: TObject);
    procedure InsertSegmentExecute(Sender: TObject);
    procedure InsertClosedSplineExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose:
      Boolean);
    procedure Options1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure AreaSelectExecute(Sender: TObject);
    procedure AreaSelectInsideExecute(Sender: TObject);
    procedure CaptureEMFExecute(Sender: TObject);
    procedure Tools1Click(Sender: TObject);
    procedure Copytoclipboard2Click(Sender: TObject);
    procedure InsertCubicSplineExecute(Sender: TObject);
    procedure InsertClosedCubicSplineExecute(Sender: TObject);
    procedure ImageToolExecute(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure pXsettings1Click(Sender: TObject);
    procedure TheDrawingChangeDrawing(Drawing: TDrawing);
    procedure UndoExecute(Sender: TObject);
    procedure RedoExecute(Sender: TObject);
    procedure LocalViewPaint(Sender: TObject);
    procedure LocalViewResize(Sender: TObject);
    procedure ShowRulersExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure LocalViewEndRedraw(Sender: TObject);
    procedure InsertCurveExecute(Sender: TObject);
    procedure InsertClosedCurveExecute(Sender: TObject);
    procedure ToolButton15Click(Sender: TObject);
    procedure LocalViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ShowScrollBarsExecute(Sender: TObject);
    procedure ToolButton16Click(Sender: TObject);
    procedure PreviewLaTeXExecute(Sender: TObject);
    procedure PreviewPdfLaTeXExecute(Sender: TObject);
    procedure CADSaveDlgTypeChange(Sender: TObject);
    procedure ConvertToExecute(Sender: TObject);
    procedure DoConvertToExecute(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure OpenRecentExecute(Sender: TObject);
    procedure PicturePreviewExecute(Sender: TObject);
    procedure ImportEPSExecute(Sender: TObject);
    procedure ColorBox_DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ComboBox3Select(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure ComboBox4Select(Sender: TObject);
    procedure ComboBox2Select(Sender: TObject);
    procedure ComboBox5Select(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ScalePhysicalExecute(Sender: TObject);
    procedure InsertBezierPathExecute(Sender: TObject);
    procedure InsertClosedBezierPathExecute(Sender: TObject);
    procedure PreviewLaTeX_PSExecute(Sender: TObject);
    procedure Pictureinfo1Click(Sender: TObject);
    procedure ToolButton22Click(Sender: TObject);
    procedure SmoothBezierNodesActionExecute(Sender: TObject);
    procedure PopupMenuDVIPopup(Sender: TObject);
    procedure DVI_Format_Click(Sender: TObject);
    procedure PopupMenuPdfPopup(Sender: TObject);
    procedure Pdf_Format_Click(Sender: TObject);
    procedure NewWindowExecute(Sender: TObject);
  private
    { Private declarations }
    fCurrentOpBtn: TToolButton;
    ScrollPos0: Integer;

    procedure OnSelectedObj(Sender: TCAD2DSelectObjectsParam;
      Obj: TObject2D; CtrlPt: Integer; Added: Boolean);
    procedure OnPasteMetafileFromClipboard(Drawing: TDrawing2D);
  public
    { Public declarations }
    RecentFiles: THistoryList;
    function TrySaveDrawing(const FileName: string): Word;
    procedure DoSaveDrawing(FileName: string);
    function DlgSaveDrawing(const FileName: string): Word;
    function AskSaveCurrentDrawing: Word;
    procedure NewDrawing(const FileName: string);
    procedure OpenDrawing(const FileName: string);
  end;

var
  MainForm: TMainForm;
var
  mHHelp: THookHelpSystem;

const
  crHand = 1;

type

  TTpXExtAssocOption = class(TOptionData)
    fTpXExtAssoc: Boolean;
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
  end;

var TpXExtAssoc: TTpXExtAssocOption;

implementation

uses InOut, StrUtils, Options, EMF_Add, Settings,
  AboutUnit, TransForm, PreView, ColorEtc, EMF_Unit, Geometry,
  ScaleStandardUnit;

{$R *.DFM}

type
  TMyCADCreateTheSourceBlock = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
  end;

constructor TMyCADCreateTheSourceBlock.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
var
  TmpStr: string;
  TmpIter: TExclusiveGraphicObjIterator;
  TmpBlk: TSourceBlock2D;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
    with TCAD2DSelectObjectsParam(Param),
      TDrawing2D(CADPrg.Viewport.Drawing) do
    begin
      if not InputQuery('Define block', 'Name', TmpStr) then
      begin
        NextState := CADPrg.DefaultState;
        Param.Free;
        Param := nil;
      end;
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
        begin
          RemoveObject(TmpIter.Current.ID);
          TmpIter.Next;
        end;
        TmpBlk := BlockObjects(StringToBlockName(TmpStr),
          TmpIter);
        if Assigned(TmpBlk) then
          TmpBlk.IsLibraryBlock := True;
      finally
        TmpIter.Free;
      end;
      CADPrg.RepaintAfterOperation;
    end;
  NextState := CADPrg.DefaultState;
  Param.Free;
  Param := nil;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  FileName, IncludePath: string;
begin
//FileExists(ExtractFilePath(Application.ExeName)+'\sdddd.sd'
  //LocalView.OnMouseWheel :=
  RecentFiles := THistoryList.Create;
  //TpXExtAssoc := TTpXExtAssoc.Create;
  mHHelp := THookHelpSystem.Create('TpX.chm', '', htHHAPI);
  Screen.Cursors[crHand] := LoadCursor(HInstance, 'HAND');
  //LocalView.Cursor := 1;
  Caption := Drawing_NewFileName;
  TheDrawing.DefaultLayersColor := clBlack;
  ScrollPos0 := -1;
  ShowScrollBars.Checked := True;
  with LocalView do
  begin
    UsePaintingThread := False;
    ZoomWindow(Rect2D(0, 0, 100, 100));
  end;
  with LocalPrg do
  begin
    ShowCursorCross := True;
    XSnap := 1.0;
    YSnap := 1.0;
    UseSnap := SnapToGrid.Checked;
    UseOrto := Self.UseOrto.Checked;
    DefaultState := TCAD2D_BasicMode;
  end;
  SmoothBezierNodes := SmoothBezierNodesAction.Checked;
  DeleteSelected.Tag := CADPRG_DeleteSelected;
  SelectAll.Tag := CADPRG_SelectAll;
  MoveUp.Tag := CADPRG_MoveUp;
  MoveDown.Tag := CADPRG_MoveDown;
  MoveLeft.Tag := CADPRG_MoveLeft;
  MoveRight.Tag := CADPRG_MoveRight;
  MoveUpPixel.Tag := CADPRG_MoveUpPixel;
  MoveDownPixel.Tag := CADPRG_MoveDownPixel;
  MoveLeftPixel.Tag := CADPRG_MoveLeftPixel;
  MoveRightPixel.Tag := CADPRG_MoveRightPixel;
  SelNext.Tag := CADPRG_SelNext;
  SelPrev.Tag := CADPRG_SelPrev;
  FlipV.Tag := CADPRG_FlipV;
  FlipH.Tag := CADPRG_FlipH;
  RotateCounterclockW.Tag := CADPRG_RotateCounterclockW;
  RotateClockW.Tag := CADPRG_RotateClockW;
  RotateCounterclockWDegree.Tag :=
    CADPRG_RotateCounterclockWDegree;
  RotateClockWDegree.Tag := CADPRG_RotateClockWDegree;
  Grow10.Tag := CADPRG_Grow10;
  Shrink10.Tag := CADPRG_Shrink10;
  Grow1.Tag := CADPRG_Grow1;
  Shrink1.Tag := CADPRG_Shrink1;
  MoveForward.Tag := CADPRG_MoveForward;
  MoveBackward.Tag := CADPRG_MoveBackward;
  MoveToFront.Tag := CADPRG_MoveToFront;
  MoveToBack.Tag := CADPRG_MoveToBack;
  StartRotate.Tag := CADPRG_StartRotate;
  StartMove.Tag := CADPRG_StartMove;
  DuplicateSelected.Tag := CADPRG_DuplicateSelected;
  ClipboardCopy.Tag := CADPRG_ClipboardCopy;
  ClipboardPaste.Tag := CADPRG_ClipboardPaste;
  ClipboardCut.Tag := CADPRG_ClipboardCut;
  ConvertToPolyline.Tag := CADPRG_ConvertSelected;
  CustomTransform.Tag := CADPRG_CustomTransform;
  ConvertToGrayScale.Tag := CADPRG_ConvertToGrayScale;
  TheDrawing.OnPasteMetafileFromClipboard :=
    OnPasteMetafileFromClipboard;

  MakeColorBox(ComboBox3);
  MakeColorBox(ComboBox4);
  MakeColorBox(ComboBox5);
  ComboBox1.ItemIndex := 0;
  ComboBox2.ItemIndex := 0;

  PreviewSVG.Tag := Ord(pview_SVG);
  PreviewEPS.Tag := Ord(pview_EPS);
  PreviewPDF.Tag := Ord(pview_PDF);
  PreviewPNG.Tag := Ord(pview_PNG);
  PreviewBMP.Tag := Ord(pview_BMP);
  PreviewEMF.Tag := Ord(pview_EMF);

  with TheDrawing do
  begin
    History := TDrawHistory.Create(TheDrawing);
    History.Save;
    Undo.Enabled := History.CanUndo;
    Redo.Enabled := History.CanRedo;
  end;
  ParseParamStr(FileName, IncludePath);
  if FileName <> '' then
  begin
    //ShowMessage(FileName);
    if FileExists(FileName) then
      OpenDrawing(FileName)
    else
      NewDrawing(FileName);
    TheDrawing.IncludePath := IncludePath;
  end;
end;

procedure TMainForm.InsertLineBtnClick(Sender: TObject);
begin
  with LocalPrg do
  begin
    StopOperation;
    fCurrentOpBtn := InsertLineBtn;
    StartOperation(TCAD2DDrawSizedPrimitive,
      TCAD2DDrawSizedPrimitiveParam.Create(nil,
      TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(0, 0)),
      0, -1, True));
  end;
end;

procedure TMainForm.InsertArcBtnClick(Sender: TObject);
begin
  with LocalPrg do
  begin
    StopOperation;
    fCurrentOpBtn := InsertArcBtn;
    StartOperation(TCAD2DDrawSizedPrimitive,
      TCAD2DDrawSizedPrimitiveParam.Create(nil,
      TArc2D.CreateSpec(-1, Point2D(0, 0), 0, 0, 0),
      0, -1, True));
  end;
end;

procedure TMainForm.InsertPolylineBtnClick(Sender: TObject);
begin
  with LocalPrg do
  begin
    StopOperation;
    fCurrentOpBtn := InsertPolylineBtn;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TPolyline2D.CreateSpec(-1, [Point2D(0, 0)]), 0, True));
  end;
end;

procedure TMainForm.InsertRectangleBtnClick(Sender: TObject);
begin
  with LocalPrg do
  begin
    StopOperation;
    fCurrentOpBtn := InsertRectangleBtn;
    StartOperation(TCAD2DDrawSizedPrimitive,
      TCAD2DDrawSizedPrimitiveParam.Create(nil,
      TRectangle2D.Create(-1), 0, 2,
      True));
  end;
end;

procedure TMainForm.InsertEllipseBtnClick(Sender: TObject);
begin
  with LocalPrg do
  begin
    StopOperation;
    fCurrentOpBtn := InsertEllipseBtn;
    StartOperation(TCAD2DDrawSizedPrimitive,
      TCAD2DDrawSizedPrimitiveParam.Create(nil,
      TEllipse2D.Create(-1),
      0, 2, True));
  end;
end;

procedure TMainForm.InsertPolygonBtnClick(Sender: TObject);
begin
  with LocalPrg do
  begin
    StopOperation;
    fCurrentOpBtn := InsertPolygonBtn;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TPolygon2D.CreateSpec(-1, [Point2D(0, 0)]), 0, True));
  end;
end;

procedure TMainForm.InsertSplineBtnClick(Sender: TObject);
var
  TmpSpline: TBSpline2D;
begin
  with LocalPrg do
  begin
    StopOperation;
    //fCurrentOpBtn := InsertClosedCurveBtn;
    TmpSpline := TBSpline2D.CreateSpec(-1, [Point2D(0, 0)]);
    TmpSpline.SavingType := stSpace;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TmpSpline, 0, True));
  end;
end;

procedure TMainForm.InsertTextBtnClick(Sender: TObject);
var
  TmpText: TText2D;
  TmpStr: string;
begin
  TmpStr := '';
  if not InputQuery('Add Text', 'String', TmpStr) then
    Exit;
  if TmpStr = '' then Exit;
  TmpText := TText2D.CreateSpec(-1,
    Point2D(1, 1), TheDrawing.DefaultFontHeight, TmpStr);
  TmpText.AutoSize := True;
  LocalPrg.StartOperation(TCAD2DPositionObject,
    TCAD2DPositionObjectParam.Create(nil, TmpText));
end;

procedure TMainForm.MoveBtnClick(Sender: TObject);
var
  TmpPar: TCADPrgParam;
begin
  if Useareatoselectobjects1.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside,
      TCAD2DMoveSelectedObjects);
    with LocalPrg do
      StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end
  else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5,
      TCAD2DMoveSelectedObjects);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected :=
      OnSelectedObj;
    with LocalPrg do
      StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TMainForm.RotateBtnClick(Sender: TObject);
var
  TmpPar: TCADPrgParam;
begin
  if Useareatoselectobjects1.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside,
      TCAD2DRotateSelectedObjects);
    with LocalPrg do
      StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end
  else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5,
      TCAD2DRotateSelectedObjects);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected :=
      OnSelectedObj;
    with LocalPrg do
      StartOperation(TCAD2DSelectObjects, TmpPar);
  end;
end;

procedure TMainForm.Accept1Click(Sender: TObject);
begin
  with LocalPrg do
  begin
    SendUserEvent(CADPRG_ACCEPT);
  end;
end;

procedure TMainForm.Cancel1Click(Sender: TObject);
begin
  with LocalPrg do
  begin
    SendUserEvent(CADPRG_CANCEL);
    StopOperation;
  end;
end;

procedure TMainForm.Zoomarea1Click(Sender: TObject);
begin
  with LocalPrg do
    SuspendOperation(TCADPrgZoomArea, nil);
end;

procedure TMainForm.Zoomin1Click(Sender: TObject);
begin
  LocalView.ZoomIn;
end;

procedure TMainForm.Zoomout1Click(Sender: TObject);
begin
  LocalView.ZoomOut;
end;

procedure TMainForm.Zoomall1Click(Sender: TObject);
begin
  LocalView.ZoomToExtension;
end;

procedure TMainForm.Panning1Click(Sender: TObject);
begin
  with LocalPrg do
  begin
    SuspendOperation(TCADPrgRealTimePan, nil);
    // SuspendOperation(TCADPrgPan, nil);
    //fCurrentOpBtn := PanningBtn;
  end;
end;

procedure TMainForm.ShowGridExecute(Sender: TObject);
begin
  ShowGrid.Checked := not ShowGrid.Checked;
  LocalView.ShowGrid := ShowGrid.Checked;
end;

procedure TMainForm.KeepAspectExecute(Sender: TObject);
begin
  KeepAspect.Checked := not KeepAspect.Checked;
  if KeepAspect.Checked then
    LocalView.AspectRatio := 1.0
  else
    LocalView.AspectRatio := 0.0;
  LocalView.ZoomWindow(LocalView.VisualRect);
end;

procedure TMainForm.SnapToGridExecute(Sender: TObject);
begin
  SnapToGrid.Checked := not SnapToGrid.Checked;
  LocalPrg.UseSnap := SnapToGrid.Checked;
end;

procedure TMainForm.UseOrtoExecute(Sender: TObject);
begin
  UseOrto.Checked := not UseOrto.Checked;
  LocalPrg.UseOrto := UseOrto.Checked;
end;

procedure TMainForm.SmoothBezierNodesActionExecute(Sender: TObject);
begin
  SmoothBezierNodesAction.Checked := not SmoothBezierNodesAction.Checked;
  SmoothBezierNodes := SmoothBezierNodesAction.Checked;
end;

procedure TMainForm.Useareatoselectobjects1Click(Sender:
  TObject);
begin
  Useareatoselectobjects1.Checked := not
    Useareatoselectobjects1.Checked;
end;

procedure TMainForm.OpenDrawing(const FileName: string);
var
  Loader: T_TpX_Loader;
begin
  LocalPrg.Reset;
  TheDrawing.Clear;
  Loader := T_TpX_Loader.Create(TheDrawing);
  try
    Loader.LoadFromFile(FileName);
    LocalView.ZoomToExtension;
    LocalView.Repaint;
  finally
    Loader.Free;
  end;
  Caption := ExtractFileName(TheDrawing.FileName);
  RecentFiles.Update(TheDrawing.FileName);
end;

procedure TMainForm.OpenDoc1Click(Sender: TObject);
begin
  if not CADOpenDlg.Execute then Exit;
  if CADOpenDlg.FilterIndex = 2 then
  begin
    TheDrawing.LoadFromFile(CADOpenDlg.FileName);
    TheDrawing.FileName := Drawing_NewFileName;
    Exit;
  end;
  if AskSaveCurrentDrawing = mrCancel then Exit;
  OpenDrawing(CADOpenDlg.FileName);
end;

procedure TMainForm.Save1Click(Sender: TObject);
begin
  TrySaveDrawing(TheDrawing.FileName);
end;

procedure TMainForm.Merge1Click(Sender: TObject);
begin
  if CADOpenDlg.Execute then
    TheDrawing.MergeFromFile(CADOpenDlg.FileName);
end;

function TMainForm.DlgSaveDrawing(const FileName: string): Word;
var
  Path: string;
begin
  CADSaveDlg.FileName := ChangeFileExt(FileName, '');
  if CADSaveDlg.FileName = Drawing_NewFileName then
    CADSaveDlg.FileName := '';
  Path := ExtractFilePath(CADSaveDlg.FileName);
  if Path = '' then Path := ExtractFilePath(TheDrawing.FileName);
  if Path <> '' then CADSaveDlg.InitialDir := Path;
  if not CADSaveDlg.Execute then
  begin
    Result := mrCancel;
    Exit;
  end;
  case CADSaveDlg.FilterIndex of
    2:
      StoreToFile_Saver(TheDrawing,
        CADSaveDlg.FileName, T_SVG_Export);
    3:
      TheDrawing.SaveToFile_EMF(CADSaveDlg.FileName);
    4:
      StoreToFile_Saver(TheDrawing,
        CADSaveDlg.FileName, T_PostScript_Export);
    5:
    //TheDrawing.SaveToFile_PNG(CADSaveDlg.FileName)
      StoreToFile_Saver(TheDrawing,
        CADSaveDlg.FileName, T_PNG_Export);
    6:
    //TheDrawing.SaveToFile_Bitmap(CADSaveDlg.FileName)
      StoreToFile_Saver(TheDrawing,
        CADSaveDlg.FileName, T_Bitmap_Export);
    7:
      StoreToFile_Saver(TheDrawing,
        CADSaveDlg.FileName, T_PDF_Export);
    8:
      StoreToFile_Saver(TheDrawing,
        CADSaveDlg.FileName, T_MetaPost_Export);
    9:
      StoreToFile_MPS(TheDrawing, CADSaveDlg.FileName);
    10:
      //StoreToFile_EpsToPdf(TheDrawing, CADSaveDlg.FileName, False);
      StoreToFile_Saver(TheDrawing,
        CADSaveDlg.FileName, T_EpsToPdf_Export);
  else
    DoSaveDrawing(CADSaveDlg.FileName);
  end;
  Result := mrOK;
end;

procedure TMainForm.CADSaveDlgTypeChange(Sender: TObject);
var
  List: TStringList;
begin
  List := TStringList.Create;
  ExtractStrings(['|'], [' ', '*', '.'], PChar(CADSaveDlg.Filter), List);
  CADSaveDlg.DefaultExt := List[CADSaveDlg.FilterIndex * 2 - 1];
  List.Free;
end;

function TMainForm.AskSaveCurrentDrawing: Word;
begin
  if TheDrawing.ObjectsCount = 0 then
  begin
    Result := mrOK;
    Exit;
  end;
  Result := MessageDlg('Save current drawing?',
    mtWarning, [mbYes, mbNo, mbCancel], 0);
  if Result = mrYes then
    Result := TrySaveDrawing(TheDrawing.FileName);
end;

function TMainForm.TrySaveDrawing(const FileName: string): Word;
begin
  if FileName = Drawing_NewFileName then
  begin
    Result := DlgSaveDrawing(FileName);
  end
  else
  begin
    DoSaveDrawing(FileName);
    Result := mrOK;
  end;
end;

procedure TMainForm.DoSaveDrawing(FileName: string);
begin
  if not SameText(ExtractFileExt(FileName), '.TpX') then
    FileName := ChangeFileExt(FileName, '.TpX');
  StoreToFile_TpX(TheDrawing, FileName);
  TheDrawing.FileName := FileName;
  Caption := ExtractFileName(TheDrawing.FileName);
  RecentFiles.Update(TheDrawing.FileName);
end;

procedure TMainForm.NewDrawing(const FileName: string);
begin
  LocalPrg.Reset;
  TheDrawing.Clear;
  TheDrawing.FileName := FileName;
  Caption := FileName;
  if FileName <> Drawing_NewFileName then
    RecentFiles.Update(FileName);
end;

procedure TMainForm.New1Click(Sender: TObject);
begin
  if AskSaveCurrentDrawing = mrCancel then Exit;
  NewDrawing(Drawing_NewFileName);
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.Actualview1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
  begin
    Printer.BeginDoc;
    try
      LocalView.CopyToCanvas(Printer.Canvas, cmAspect, cvActual,
        1, 1);
    finally
      Printer.EndDoc;
    end;
  end;
end;

procedure TMainForm.Fit1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
  begin
    Printer.BeginDoc;
    try
      LocalView.CopyToCanvas(Printer.Canvas, cmAspect,
        cvExtension, 1, 1);
    finally
      Printer.EndDoc;
    end;
  end;
end;

procedure TMainForm.Scale1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
  begin
    Printer.BeginDoc;
    try
      LocalView.CopyToCanvas(Printer.Canvas, cmAspect, cvScale,
        1, 1);
    finally
      Printer.EndDoc;
    end;
  end;
end;

procedure TMainForm.LocalPrgStartOperation(Sender: TObject; const
  Operation: TCADStateClass;
  const Param: TCADPrgParam);
begin
  if Assigned(fCurrentOpBtn) then
    fCurrentOpBtn.Indeterminate := True;
end;

procedure TMainForm.LocalPrgEndOperation(Sender: TObject; const
  Operation: TCADStateClass;
  const Param: TCADPrgParam);
begin
  if Assigned(fCurrentOpBtn) then
  begin
    fCurrentOpBtn.Indeterminate := False;
    fCurrentOpBtn := nil;
  end;
end;

procedure TMainForm.LocalPrgStopOperation(Sender: TObject; const
  Operation: TCADStateClass;
  const Param: TCADPrgParam);
begin
  if Assigned(fCurrentOpBtn) then
  begin
    fCurrentOpBtn.Indeterminate := False;
    fCurrentOpBtn := nil;
  end;
end;

procedure TMainForm.LocalPrgDescriptionChanged(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := TCADState(Sender).Description;
end;

procedure TMainForm.LocalViewMouseMove2D(Sender: TObject; Shift:
  TShiftState;
  WX, WY: Single; X, Y: Integer);
var
  TmpPar: TCADPrgParam;
  Objs: TGraphicObjList;
  TmpObj: TObject2D;
  TmpN: Integer;
begin
  if SSLeft in Shift then
  begin
    Exit;
    //LocalPrg.CurrentState.Description :=
    //LocalPrg.CurrentOperation.ClassName;
  //LocalView.SetFocus;
    if LocalPrg.CurrentOperation = TCAD2DSelectObjects then
    begin
      LocalPrg.SendUserEvent(CADPRG_ACCEPT);
      //LocalPrg.SendUserEvent(CADPRG_ACCEPT);
      Exit;
    end;
    if LocalPrg.CurrentOperation = TCADIdleState then
    begin
      TmpObj := LocalView.PickObject(
        LocalPrg.CurrentViewportSnappedPoint,
        5, False, TmpN);
      if TmpObj is TGraphicObject then
      begin
        Objs := TGraphicObjList.Create;
        Objs.Add(TmpObj);
        TmpPar := TCAD2DMoveObjectsParam.Create(Objs,
          nil);
        with LocalPrg do
          StartOperation(TCAD2DTransformObjectsSimple, TmpPar);
      end;
    end;
  end;
  with LocalPrg.CurrentViewportSnappedPoint do
    StatusBar1.Panels[0].Text := Format('X: %6.3f Y: %6.3f', [X,
      Y]);
  LocalView.SetFocus;
end;

procedure TMainForm.LocalPopUpPopup(Sender: TObject);
var
  IsZoomingOperation: Boolean;
begin
  Accept1.Enabled := LocalPrg.IsBusy;
  Cancel1.Enabled := LocalPrg.IsBusy;
  IsZoomingOperation := (LocalPrg.CurrentOperation =
    TCADPrgZoomArea)
    or (LocalPrg.CurrentOperation = TCADPrgRealTimePan)
    or LocalPrg.IsSuspended;
  Zoomarea1.Enabled := not IsZoomingOperation;
  Zoomin1.Enabled := not IsZoomingOperation;
  Zoomout1.Enabled := not IsZoomingOperation;
  Zoomall1.Enabled := not IsZoomingOperation;
  Panning1.Enabled := not IsZoomingOperation;
end;

procedure TMainForm.OnSelectedObj(Sender:
  TCAD2DSelectObjectsParam;
  Obj: TObject2D; CtrlPt: Integer; Added: Boolean);
begin
  {if Assigned(Obj) then
    LocalView.DrawObject2DWithRubber(Obj, True);}
end;

procedure TMainForm.LocalViewDblClick(Sender: TObject);
var
  TmpPt: TPoint2D;
  TmpObj: TObject2D;
  TmpN: Integer;
  TmpStr: string;
  TmpH: TRealType;
begin
  TmpPt := LocalPrg.CurrentViewportSnappedPoint;
  TmpObj := LocalView.PickObject(TmpPt, 5, False, TmpN);
  {if TmpObj is TText2D then
    with TText2D(TmpObj) do
    begin
      TmpStr := FloatToStr(Height);
      if not InputQuery('Add Text', 'Height', TmpStr) then
        Exit;
      Height := StrToFloat(TmpStr);
      TmpStr := Text;
      if not InputQuery('Edit Text', 'String', TmpStr) then
        Exit;
      Text := TmpStr;
      LocalView.Repaint;
    end;}
end;

procedure TMainForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
var
  TmpStr: TFileStream;
begin
  if TheDrawing.SourceBlocksCount = 0 then
    Exit;
  TmpStr :=
    TFileStream.Create(ExtractFilePath(Application.ExeName) +
    '\Library.blk', fmOpenWrite or fmCreate);
  try
    TheDrawing.SaveLibrary(TmpStr);
  finally
    TmpStr.Free;
  end;
end;

procedure TMainForm.Setpoint1Click(Sender: TObject);
var
  TmpStrX, TmpStrY: string;
begin
  TmpStrX := Format('%6.3f', [LocalPrg.CurrentViewportPoint.X]);
  TmpStrY := Format('%6.3f', [LocalPrg.CurrentViewportPoint.Y]);
  if not InputQuery('Insert point', 'X', TmpStrX) then
    Exit;
  if not InputQuery('Insert point', 'Y', TmpStrY) then
    Exit;
  LocalPrg.CurrentViewportPoint := Point2D(StrToFloat(TmpStrX),
    StrToFloat(TmpStrY));
  LocalPrg.SendCADEvent(ceMouseDown, mbLeft, [], 0);
end;

procedure TMainForm.LocalViewKeyDown(Sender: TObject; var Key:
  Word;
  Shift: TShiftState);
begin
  if Key = 104 then
    with LocalPrg.CurrentViewportPoint do
      Y := Y + 10.0;
  LocalPrg.SendCADEvent(ceMouseMove, mbLeft, [], 0);
end;

procedure TMainForm.Test1Click(Sender: TObject);
var
  St, ET: TDateTime;
  H, M, S, MS: Word;
begin
  St := Now;
  LocalView.Repaint;
  ET := Now;
  DecodeTime(ET - St, H, M, S, MS);
  ShowMessage(Format('%d:%d - %d', [S, MS,
    TheDrawing.ObjectsCount]));
end;

procedure TMainForm.LocalViewMouseDown2D(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; WX, WY: Single;
  X, Y: Integer);
begin
  if Button = mbRight then
  begin
    //if not (LocalPrg.CurrentOperation = TCAD2DDrawUnsizedPrimitive)      then
    //if not (LocalPrg.CurrentState.Description = '') then
    //LocalPopUp.Popup(X, Y);
    Exit;
  end;
end;

procedure TMainForm.LocalViewMouseUp2D(Sender: TObject; Button:
  TMouseButton;
  Shift: TShiftState; WX, WY: Single; X, Y: Integer);
var
  TmpPt: TPoint2D;
  TmpObj: TObject2D;
  TmpN: Integer;
begin
  {TmpPt := LocalPrg.CurrentViewportSnappedPoint;
  TmpObj := LocalView.PickObject(TmpPt, 5, False, TmpN);
  if TmpObj <>  LocalPrg.CurrentState.Param.UserObject then}

  //TSY:
  {with LocalPrg do
  begin
    SendUserEvent(CADPRG_ACCEPT);
  end;}

  //SelectedObjects.
end;

procedure TMainForm.SelectBtnClick(Sender: TObject);
var
  TmpPar: TCADPrgParam;
begin
  if LocalPrg.IsBusy then LocalPrg.StopOperation;
  if Useareatoselectobjects1.Checked then
  begin
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside,
      TCAD2DSelectObjectsInArea);
    with LocalPrg do
      StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
  end
  else
  begin
    TmpPar := TCAD2DSelectObjectsParam.Create(5,
      TCAD2D_BasicMode);
    TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected :=
      OnSelectedObj;
    with LocalPrg do
      StartOperation(TCAD2D_BasicMode, TmpPar);
  end;
end;

procedure TMainForm.Open1Click(Sender: TObject);
var
  XMLDoc: TXMLDDocument;
begin
  if not OpenDialog1.Execute then Exit;
  RO_Init(XMLDoc, TXMLDDocument.Create);
  //XMLDoc.LoadXML('<TpX><line/></TpX>');
  XMLDoc.Load(OpenDialog1.FileName);
  //RichEdit1.Lines.Assign(XMLDoc.XML);
  RichEdit1.Clear;
  RichEdit1.Lines.BeginUpdate;
  RichEdit1.Text := XMLNodeText(XMLDoc.DocumentElement, 0);
  RichEdit1.Lines.EndUpdate;
  RO_Free(XMLDoc);
end;

procedure TMainForm.ImportMetafileExecute(Sender: TObject);
begin
  if not EMFOpenDialog.Execute then Exit;
  if AskSaveCurrentDrawing = mrCancel then Exit;
  LocalPrg.Reset;
  if RichEdit1.Visible then
    Import_Metafile(TheDrawing, EMFOpenDialog.FileName, RichEdit1.Lines)
  else Import_Metafile(TheDrawing, EMFOpenDialog.FileName, nil);
  Caption := TheDrawing.FileName;
  LocalView.ZoomToExtension;
  LocalView.Repaint;
end;

procedure TMainForm.ToolButton4Click(Sender: TObject);
begin
  RichEdit1.Visible := not RichEdit1.Visible;
  BitBtn1.Visible := RichEdit1.Visible;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_UP then ;
end;

procedure TMainForm.SendUserEventExecute(Sender: TObject);
begin
  with LocalPrg do
  begin
    SendUserEvent((Sender as TAction).Tag);
  end;
end;

procedure TMainForm.InsertCircleExecute(Sender: TObject);
begin
  with LocalPrg do
  begin
    StopOperation;
    fCurrentOpBtn := InsertCircleBtn;
    StartOperation(TCAD2DDrawSizedPrimitive,
      TCAD2DDrawSizedPrimitiveParam.Create(nil,
      TCircle2D.Create(-1),
      0, -1, True));
  end;
end;

procedure TMainForm.InsertStarExecute(Sender: TObject);
begin
  with LocalPrg do
  begin
    StopOperation;
    fCurrentOpBtn := InsertStarBtn;
    StartOperation(TCAD2DDrawSizedPrimitive,
      TCAD2DDrawSizedPrimitiveParam.Create(nil,
      TStar2D.CreateSpec(-1, Point2D(0, 0)),
      0, -1, True));
  end;
end;

procedure TMainForm.BasicModeBtnClick(Sender: TObject);
begin
  with LocalPrg do
    StopOperation;
end;

procedure TMainForm.Scalestandard1Click(Sender: TObject);
var
  T: TTransf2D;
begin
  if not (ScaleStandardForm.ShowModal = mrOK) then Exit;
  T := TheDrawing.ScaleStandard(ScaleStandardForm.ScaleStandardMaxWidth,
    ScaleStandardForm.ScaleStandardMaxHeight);
  if ScaleStandardForm.ScalePhysical.Checked
    then TheDrawing.ScalePhysical(IsotropicScale(T));
  LocalView.ZoomToExtension;
  //LocalView.Repaint;
end;

procedure TMainForm.SaveAsExecute(Sender: TObject);
begin
  DlgSaveDrawing(Drawing_NewFileName);
end;

procedure TMainForm.InsertSectorExecute(Sender: TObject);
begin
  with LocalPrg do
  begin
    if IsBusy then
      StopOperation;
    fCurrentOpBtn := InsertSectorBtn;
    StartOperation(TCAD2DDrawSizedPrimitive,
      TCAD2DDrawSizedPrimitiveParam.Create(nil,
      TSector2D.CreateSpec(-1, Point2D(0, 0), 0, 0, 0),
      0, -1, True));
  end;
end;

procedure TMainForm.InsertSegmentExecute(Sender: TObject);
begin
  with LocalPrg do
  begin
    if IsBusy then
      StopOperation;
    fCurrentOpBtn := InsertSegmentBtn;
    StartOperation(TCAD2DDrawSizedPrimitive,
      TCAD2DDrawSizedPrimitiveParam.Create(nil,
      TSegment2D.CreateSpec(-1, Point2D(0, 0), 0, 0, 0),
      0, -1, True));
  end;
end;

procedure TMainForm.InsertClosedSplineExecute(Sender: TObject);
var
  TmpPath: TClosedBSpline2D;
begin
  with LocalPrg do
  begin
    if IsBusy then
      StopOperation;
    //fCurrentOpBtn := InsertClosedSplineBtn;
    TmpPath := TClosedBSpline2D.CreateSpec(-1, [Point2D(0, 0)]);
    TmpPath.SavingType := stSpace;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TmpPath, 0, True));
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var
  CanClose: Boolean);
begin
  CanClose := AskSaveCurrentDrawing <> mrCancel;
end;

procedure TMainForm.Options1Click(Sender: TObject);
begin
  if OptionsForm.ShowModal = mrOK then LocalView.Repaint;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  with LocalPrg do StopOperation;
  RecentFiles.Free;
  //TpXExtAssoc.Free;
  mHHelp.Free;
  HHCloseAll; //Close help before shutdown or big trouble
end;

procedure TMainForm.Help1Click(Sender: TObject);
begin
  HH.HtmlHelp(GetDesktopWindow,
    'TpX.chm::/tpx_tpxabout_tpx_drawing_tool.htm',
    HH_DISPLAY_TOPIC, 0);
end;

function TMainForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  HH.HtmlHelp(GetDesktopWindow,
    'TpX.chm::/tpx_tpxabout_tpx_drawing_tool.htm',
    HH_DISPLAY_TOPIC, 0);
end;

procedure TMainForm.AreaSelectExecute(Sender: TObject);
var
  TmpPar: TCADPrgParam;
begin
  with LocalPrg do StopOperation;
  if AreaSelectInside.Checked then
    TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside,
      nil)
  else
    TmpPar :=
      TCAD2DSelectObjectsInAreaParam.Create(gmCrossFrame,
      nil);
  with LocalPrg do
    StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
end;

procedure TMainForm.AreaSelectInsideExecute(Sender: TObject);
begin
  AreaSelectInside.Checked := not AreaSelectInside.Checked;
end;

procedure TMainForm.CaptureEMFExecute(Sender: TObject);
var
  MF: TMetaFile;
begin
  if not Clipboard.HasFormat(CF_METAFILEPICT) then Exit;
  MF := TMetaFile.Create; //CF_ENHMETAFILE
  //Save.Filter := 'Enhanced metafile (*.emf)|*.emf';
  CaptureEMF_Dialog.FileName := '';
  if CaptureEMF_Dialog.InitialDir = '' then
    CaptureEMF_Dialog.InitialDir := ExtractFilePath(ParamStr(0));
  if CaptureEMF_Dialog.Execute then
  begin
    MF.Assign(Clipboard);
    MF.SaveToFile(CaptureEMF_Dialog.FileName);
    CaptureEMF_Dialog.InitialDir :=
      ExtractFilePath(CaptureEMF_Dialog.FileName)
  end;
  MF.Free;
end;

procedure TMainForm.Tools1Click(Sender: TObject);
begin
  CaptureEMF.Enabled := Clipboard.HasFormat(CF_METAFILEPICT);
end;

procedure TMainForm.Copytoclipboard2Click(Sender: TObject);
begin
  LocalView.CopyToClipboard(Clipboard);
end;

procedure TMainForm.InsertCubicSplineExecute(Sender: TObject);
var
  TmpSpline: TCubicBSpline2D;
begin
  with LocalPrg do
  begin
    StopOperation;
    //fCurrentOpBtn := InsertCubicSplineBtn;
    TmpSpline := TCubicBSpline2D.CreateSpec(-1, [Point2D(0, 0)]);
    TmpSpline.SavingType := stSpace;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TmpSpline, 0, True));
  end;
end;

procedure TMainForm.InsertClosedCubicSplineExecute(Sender:
  TObject);
var
  TmpPath: TClosedCubicBSpline2D;
begin
  with LocalPrg do
  begin
    if IsBusy then
      StopOperation;
    //fCurrentOpBtn := InsertClosedCubicSplineBtn;
    TmpPath := TClosedCubicBSpline2D.CreateSpec(-1, [Point2D(0,
        0)]);
    TmpPath.SavingType := stSpace;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TmpPath, 0, True));
  end;
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin
  Clipboard.AsText := RichEdit1.Lines.GetText;
end;

procedure TMainForm.pXsettings1Click(Sender: TObject);
begin
  if SettingsForm.ShowModal = mrOK then LocalView.Repaint;
end;

procedure TMainForm.TheDrawingChangeDrawing(Drawing: TDrawing);
begin
  with TheDrawing do
    if Assigned(History) then
    begin
      History.Save;
      Undo.Enabled := History.CanUndo;
      Redo.Enabled := History.CanRedo;
    end;
{  if ProgressBar1.Position = ProgressBar1.Max
    then ProgressBar1.Position := ProgressBar1.Min
  else ProgressBar1.Position := ProgressBar1.Position + 1;}
end;

procedure TMainForm.UndoExecute(Sender: TObject);
begin
  with TheDrawing do
  begin
    History.Undo;
    RepaintViewports;
    Undo.Enabled := History.CanUndo;
    Redo.Enabled := History.CanRedo;
  end;
end;

procedure TMainForm.RedoExecute(Sender: TObject);
begin
  with TheDrawing do
  begin
    History.Redo;
    RepaintViewports;
    Undo.Enabled := History.CanUndo;
    Redo.Enabled := History.CanRedo;
  end;
end;

procedure TMainForm.LocalViewPaint(Sender: TObject);
begin
  //Ruler1.Paint;
  //Ruler2.Paint;
end;

procedure TMainForm.LocalViewResize(Sender: TObject);
begin
  //Ruler1.Paint;
  //Ruler2.Paint;
end;

procedure TMainForm.ShowRulersExecute(Sender: TObject);
begin
  ShowRulers.Checked := not ShowRulers.Checked;
  LocalView.ShowRulers := ShowRulers.Checked;
  //Ruler1.Visible := ShowRulers.Checked;
  //Ruler2.Visible := ShowRulers.Checked;
  Panel2.Visible := ShowRulers.Checked;
  Panel3.Visible := ShowRulers.Checked;
  //LocalView.Repaint;
end;

procedure TMainForm.ShowScrollBarsExecute(Sender: TObject);
begin
  ShowScrollBars.Checked := not ShowScrollBars.Checked;
  HScrollBar.Visible := ShowScrollBars.Checked;
  VScrollBar.Visible := ShowScrollBars.Checked;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ShowGrid.Checked := LocalView.ShowGrid;
  ShowRulers.Checked := LocalView.ShowRulers;
  Areaselectinsideonly1.Checked :=
    AreaSelectInside.Checked;
  Panel2.Visible := ShowRulers.Checked;
  Panel3.Visible := ShowRulers.Checked;
  KeepAspect.Checked := LocalView.AspectRatio = 1.0;
  HScrollBar.Visible := ShowScrollBars.Checked;
  VScrollBar.Visible := ShowScrollBars.Checked;
  Showscrollbars1.Checked := ShowScrollBars.Checked;
  //Scalephysicalunits1.Checked := ScalePhysical.Checked;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.LocalViewEndRedraw(Sender: TObject);
begin
  Ruler1.Paint;
  Ruler2.Paint;
end;

procedure TMainForm.InsertCurveExecute(Sender: TObject);
var
  TmpPath: TSmoothPath2D;
begin
  with LocalPrg do
  begin
    if IsBusy then
      StopOperation;
    fCurrentOpBtn := InsertCurveBtn;
    TmpPath := TSmoothPath2D.CreateSpec(-1, [Point2D(0, 0)]);
    TmpPath.SavingType := stSpace;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TmpPath, 0, True));
  end;
end;

procedure TMainForm.InsertClosedCurveExecute(Sender: TObject);
var
  TmpPath: TClosedSmoothPath2D;
begin
  with LocalPrg do
  begin
    if IsBusy then
      StopOperation;
    fCurrentOpBtn := InsertClosedCurveBtn;
    TmpPath := TClosedSmoothPath2D.CreateSpec(-1, [Point2D(0, 0)]);
    TmpPath.SavingType := stSpace;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TmpPath, 0, True));
  end;
end;

procedure TMainForm.ToolButton15Click(Sender: TObject);
var
  Dlg: TOpenDialog;
  Descent: TRealType;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Filter := 'Font files (*.pfb)|*.pfb';
    if not Dlg.Execute then
    begin
      Dlg.Free;
      Exit;
    end;
    pfb2pfa(Dlg.FileName, ChangeFileExt(Dlg.FileName, '.pfa'), Descent);
  finally
    Dlg.Free;
  end;
end;

procedure TMainForm.LocalViewMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  FX, FY: TRealType;
  MousePosClient: TPoint;
begin
  Handled := True;
  if Shift = [] then
    LocalView.PanWindowFraction(0, -WheelDelta / Wheel_Delta * 0.05)
  else if Shift = [SSShift] then
    LocalView.PanWindowFraction(WheelDelta / Wheel_Delta * 0.05, 0)
  else if Shift = [SSAlt] then
    LocalView.ZoomViewCenter(Exp(-WheelDelta / Wheel_Delta * Ln(2) / 8))
  else if Shift = [SSCtrl] then
  begin
    MousePosClient := LocalView.ScreenToClient(MousePos);
    FX := MousePosClient.X / LocalView.Width;
    FY := 1 - MousePosClient.Y / LocalView.Height;
    LocalView.ZoomFrac(FX, FY, Exp(-WheelDelta / Wheel_Delta * Ln(2) / 8));
  end;
end;

procedure TMainForm.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var F: Double;
begin
  case ScrollCode of
    scEndScroll:
      begin
        ScrollPos := 50;
        ScrollPos0 := -1;
      end;
    scLineUp, scLineDown, scPageUp, scPageDown, scTrack:
      begin
        if ScrollPos0 < 0 then ScrollPos0 := 50;
        F := (ScrollPos - ScrollPos0) / 50;
        if Sender = HScrollBar then LocalView.PanWindowFraction(F, 0)
        else LocalView.PanWindowFraction(0, F);
        ScrollPos0 := ScrollPos;
      end;
  end;
end;

procedure TMainForm.ToolButton16Click(Sender: TObject);
var
  Dlg: TOpenDialog;
  Heights, Depths: T_FM_Arr;
  I: Integer;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Filter := 'Font metric files (*.pl)|*.pl';
    if not Dlg.Execute then
    begin
      Dlg.Free;
      Exit;
    end;
    Parse_PL(Dlg.FileName, Heights, Depths);
    RichEdit1.Clear;
    RichEdit1.Lines.BeginUpdate;
    for I := 0 to 255 do
      RichEdit1.Lines.Add(//Format('%s'#9'>>'#9'%d'#9'%.7g'#9'%.7g',
        //[Chr(I), I, Heights[I], Depths[I]]));
        Format('%d'#9'%.7g'#9'%.7g', [I, Heights[I], Depths[I]]));
    RichEdit1.Lines.EndUpdate;
  finally
    Dlg.Free;
  end;
end;

procedure TMainForm.PreviewLaTeXExecute(Sender: TObject);
begin
  Preview_LaTeX(TheDrawing, ltxview_Dvi);
end;

procedure TMainForm.PreviewPdfLaTeXExecute(Sender: TObject);
begin
  Preview_LaTeX(TheDrawing, ltxview_Pdf);
end;

procedure TMainForm.PreviewLaTeX_PSExecute(Sender: TObject);
begin
  Preview_LaTeX(TheDrawing, ltxview_PS);
end;

procedure TMainForm.ConvertToExecute(Sender: TObject);
var
  Item: TMenuItem;
  GOClass: TGraphicObjectClass;
  I: Integer;
begin
  {if Sender is TControl then (Sender as TControl).ClientOrigin.X}
  ConvertPopup.Items.Clear;
  for I := 1 to High(GraphicObjectClasses) do
  begin
    GOClass := GraphicObjectClasses[I];
    Item := TMenuItem.Create(ConvertPopup);
    Item.Caption := TPrimitive2DClass(GOClass).GetName; //
    Item.Tag := CADPRG_ConvertSelected + I - 1;
    //Item.Action := DoConvertTo;
    Item.OnClick := DoConvertToExecute;
    ConvertPopup.Items.Add(Item);
  end;
  ConvertPopup.Popup(LocalView.ClientOrigin.X,
    LocalView.ClientOrigin.Y);
end;

procedure TMainForm.DoConvertToExecute(Sender: TObject);
begin
  if not (Sender is TMenuItem) then Exit;
  with LocalPrg do
  begin
    SendUserEvent({CADPRG_ConvertSelected}(Sender as TMenuItem).Tag);
  end;
end;

procedure TMainForm.File1Click(Sender: TObject);
var
  Item: TMenuItem;
  I: Integer;
  function ShortName(const FileName: string): string;
  const
    MaxLen = 45;
  begin
    Result := FileName;
    if Length(Result) <= MaxLen then Exit;
    begin
      Delete(Result, 1, Length(Result) - MaxLen + 1);
      Result := '..' + Result;
    end;
  end;
begin
  Recentfiles1.Clear;
  for I := 0 to RecentFiles.Count - 1 do
  begin
    Item := TMenuItem.Create(Recentfiles1);
    Item.Caption := ShortName(RecentFiles[I]);
    Item.Tag := I;
    Item.OnClick := OpenRecentExecute;
    Recentfiles1.Add(Item);
  end;
  ImportEPS.Visible := PsToEditPath <> '';
end;

procedure TMainForm.OpenRecentExecute(Sender: TObject);
var
  FileName: string;
  I: Integer;
begin
  if not (Sender is TMenuItem) then Exit;
  I := (Sender as TMenuItem).Tag;
  if (I < 0) or (I >= RecentFiles.Count) then Exit;
  FileName := RecentFiles[I];
  if not FileExists(FileName) then
  begin
    RecentFiles.Remove(FileName);
    Exit;
  end;
  if AskSaveCurrentDrawing = mrCancel then Exit;
  OpenDrawing(FileName);
end;

procedure TMainForm.PicturePreviewExecute(Sender: TObject);
begin
  Preview_Picture(TheDrawing,
    TPicturePreviewKind((Sender as TAction).Tag));
end;

procedure TMainForm.ImportEPSExecute(Sender: TObject);
begin
  if not EPSOpenDialog.Execute then Exit;
  if AskSaveCurrentDrawing = mrCancel then Exit;
  LocalPrg.Reset;
  Import_Eps(TheDrawing, EPSOpenDialog.FileName);
  Caption := TheDrawing.FileName;
  LocalView.ZoomToExtension;
  LocalView.Repaint;
end;

procedure AssociateShellExt(const Ext, ExtLabel,
  Description, CommandMenuText, ExecPath: string);
var
  St: string;
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CLASSES_ROOT;
    if R.KeyExists(Ext) then
    begin
      R.OpenKey(Ext, False);
      St := R.ReadString('');
      R.CloseKey;
      if St <> ExtLabel then
      begin
        R.OpenKey(Ext + '\UndoClass', True);
        R.WriteString('', St);
        R.CloseKey;
      end;
    end;
    R.OpenKey(Ext, True);
    R.WriteString('', ExtLabel);
    R.CloseKey;

    R.OpenKey(ExtLabel, True);
    R.WriteString('', Description);
    R.CloseKey;

    R.OpenKey(ExtLabel + '\shell\open', True);
    R.WriteString('', CommandMenuText);
    R.CloseKey;

    R.OpenKey(ExtLabel + '\shell\open\command', True);
    R.WriteString('', '"' + ExecPath + '" -f"%1"');
    R.CloseKey;

    R.OpenKey(ExtLabel + '\DefaultIcon', True);
    R.WriteString('', ExecPath + ',0');
    R.CloseKey;
  finally
    R.Free;
  end;
end;

procedure UnAssociateShellExt(const Ext, ExtLabel: string);
var
  St: string;
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CLASSES_ROOT;
    if R.KeyExists(Ext + '\UndoClass') then
    begin
      R.OpenKey(Ext + '\UndoClass', False);
      St := R.ReadString('');
      R.CloseKey;
      if St <> ExtLabel then
      begin
        R.OpenKey(Ext, True);
        R.WriteString('', St);
        R.CloseKey;
        R.OpenKey(Ext + '\UndoClass', True);
        R.WriteString('', ExtLabel);
        R.CloseKey;
      end;
    end
    else R.DeleteKey(Ext);
  finally
    R.Free;
  end;
end;

procedure TTpXExtAssocOption.SetAsString(St: string);
begin
  fTpXExtAssoc := Trim(St) = '1';
  if fTpXExtAssoc then
    AssociateShellExt('.tpx', 'TpXDrawing',
      'TpX drawing', '&Open TpX drawing', ParamStr(0))
  else
    UnAssociateShellExt('.tpx', 'TpXDrawing');
end;

function TTpXExtAssocOption.GetAsString: string;
begin
  if fTpXExtAssoc then Result := '1'
  else Result := '0';
end;

procedure TMainForm.ColorBox_DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  ColorBoxDrawItem(Control as TComboBox, Index, Rect, State);
end;

procedure TMainForm.ComboBox1Select(Sender: TObject);
var
  NewLineStyle: TLineStyle;
begin
  NewLineStyle := TLineStyle((Sender as TComboBox).ItemIndex);
  TheDrawing.ChangeSelected(TheDrawing.ChangeLineKind, @NewLineStyle);
end;

procedure TMainForm.ComboBox3Select(Sender: TObject);
var
  NewLineColor: TColor;
begin
  ColorBoxSelect(Sender as TComboBox);
  NewLineColor := ColorBoxGet(Sender as TComboBox);
  TheDrawing.ChangeSelected(TheDrawing.ChangeLineColor, @NewLineColor);
end;

procedure TMainForm.ComboBox2Select(Sender: TObject);
var
  NewHatching: THatching;
begin
  NewHatching := THatching((Sender as TComboBox).ItemIndex);
  TheDrawing.ChangeSelected(TheDrawing.ChangeHatching, @NewHatching);
end;

procedure TMainForm.ComboBox4Select(Sender: TObject);
var
  NewHatchColor: TColor;
begin
  ColorBoxSelect(Sender as TComboBox);
  NewHatchColor := ColorBoxGet(Sender as TComboBox);
  TheDrawing.ChangeSelected(TheDrawing.ChangeHatchColor, @NewHatchColor);
end;

procedure TMainForm.ComboBox5Select(Sender: TObject);
var
  NewFillColor: TColor;
begin
  ColorBoxSelect(Sender as TComboBox);
  NewFillColor := ColorBoxGet(Sender as TComboBox);
  TheDrawing.ChangeSelected(TheDrawing.ChangeFillColor, @NewFillColor);
end;

procedure TMainForm.ComboBox6Change(Sender: TObject);
var
  NewLineWidth: TRealType;
begin
  NewLineWidth := StrToFloat((Sender as TComboBox).Text);
  if NewLineWidth <= 0 then Exit;
  TheDrawing.ChangeSelected(TheDrawing.ChangeLineWidth, @NewLineWidth);
end;

procedure TMainForm.ImageToolExecute(Sender: TObject);
//var  EMF_Struct: T_EMF_Structure;
begin
{  if not EMFOpenDialog.Execute then Exit;
  EMF_Struct := T_EMF_Structure.Create;  TCanvas
  EMF_Struct.pLogStrings := RichEdit1.Lines;
  try
    EMF_Struct.LoadFromFile(EMFOpenDialog.FileName);
  finally
    EMF_Struct.Free;
  end;}
  EMF_Form.ShowModal;
end;

procedure TMainForm.OnPasteMetafileFromClipboard(Drawing: TDrawing2D);
begin
  PasteMetafileFromClipboard(Drawing);
end;

procedure TMainForm.ScalePhysicalExecute(Sender: TObject);
begin
  ScalePhysical.Checked := not ScalePhysical.Checked;
end;

procedure TMainForm.InsertBezierPathExecute(Sender: TObject);
var
  TmpPath: TBezierPath2D;
begin
  with LocalPrg do
  begin
    if IsBusy then
      StopOperation;
    fCurrentOpBtn := InsertBezierPathBtn;
    TmpPath := TBezierPath2D.CreateSpec(-1, [Point2D(0, 0)]);
    TmpPath.SavingType := stSpace;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TmpPath, 0, True));
  end;
end;


procedure TMainForm.InsertClosedBezierPathExecute(Sender: TObject);
var
  TmpPath: TClosedBezierPath2D;
begin
  with LocalPrg do
  begin
    if IsBusy then
      StopOperation;
    fCurrentOpBtn := InsertClosedBezierPathBtn;
    TmpPath := TClosedBezierPath2D.CreateSpec(-1, [Point2D(0, 0)]);
    TmpPath.SavingType := stSpace;
    StartOperation(TCAD2DDrawUnsizedPrimitive,
      TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
      TmpPath, 0, True));
  end;
end;

procedure TMainForm.Pictureinfo1Click(Sender: TObject);
var
  List: TStringList;
  R: TRect2D;
var
  MS: TMEMORYSTATUS;
begin
  List := TStringList.Create;
  R := TheDrawing.DrawingExtension;
  try
    List.Add('Picture bounds (left, bottom, right, top):');
    List.Add(Format('   %.5g, %.5g, %.5g, %.5g',
      [R.Left, R.Bottom, R.Right, R.Top]));
    List.Add(Format('Size: %.5g x %.5g (%.5gmm x %.5gmm)',
      [R.Right - R.Left, R.Top - R.Bottom,
      (R.Right - R.Left) * TheDrawing.PicScale,
        (R.Top - R.Bottom) * TheDrawing.PicScale]));
    List.Add(Format('Output size: %.5gmm x %.5gmm',
      [(R.Right - R.Left) * TheDrawing.PicScale
      * TheDrawing.PicMagnif,
        (R.Top - R.Bottom) * TheDrawing.PicScale
        * TheDrawing.PicMagnif]));
    MS.dwLength := SizeOf(TMEMORYSTATUS);
    GlobalMemoryStatus(MS);
    List.Add('');
    List.Add('Memory status:');
    List.Add(Format(' allocated = %d k %d b',
      [AllocMemSize div 1024, AllocMemSize mod 1024]));
    List.Add(Format(' available physical = %d k',
      [MS.dwAvailPhys div 1024]));
    List.Add(Format(' available virtual = %d k',
      [MS.dwAvailVirtual div 1024]));
    Application.MessageBox(List.GetText, 'Picture info');
  finally
    List.Free;
  end;
end;

procedure TMainForm.ToolButton22Click(Sender: TObject);
var
  //cColorString: PChar;
  PColorRef: ^COLORREF;
begin
  //LaBoiteACouleursLoadWindow;
  ////GetMem(cColorString, 255);
  ////cColorString := 'RGB|10|50|85';
  ////LaBoiteACouleursSelectColorStr(cColorString);
  ////FreeMem(cColorString, 255);
  //GetMem(PColorRef, SizeOf(COLORREF));
  //PColorRef^ := clBlue;
  ////LaBoiteACouleursSelectColor(PColorRef^);
  //Self.Color := PColorRef^;
  //LaBoiteACouleursUnloadWindow;
end;

procedure TMainForm.PopupMenuDVIPopup(Sender: TObject);
begin
  PopupMenuDVI.Items[Ord(TheDrawing.TeXFormat)].Checked := True;
end;

procedure TMainForm.DVI_Format_Click(Sender: TObject);
begin
  if not (Sender is TMenuItem) then Exit;
  TheDrawing.TeXFormat := TeXFormatKind((Sender as TMenuItem).MenuIndex);
end;

procedure TMainForm.PopupMenuPdfPopup(Sender: TObject);
begin
  PopupMenuPdf.Items[Ord(TheDrawing.PdfTeXFormat)].Checked := True;
end;

procedure TMainForm.Pdf_Format_Click(Sender: TObject);
begin
  if not (Sender is TMenuItem) then Exit;
  TheDrawing.PdfTeXFormat := PdfTeXFormatKind((Sender as TMenuItem).MenuIndex);
end;

procedure TMainForm.NewWindowExecute(Sender: TObject);
begin
  OpenOrExec('', Application.ExeName);
end;

initialization
  DecimalSeparator := '.';
  //ShowMessage(GetTempDir);
  {CADSysRegisterFontFromFile(0,
    ExtractFilePath(Application.ExeName) + '\RomanC.fnt');}
end.

