unit MainUnit;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Buttons,
  Forms, Menus, ComCtrls, Clipbrd, Dialogs,
  StdCtrls, ExtCtrls, ImgList, ActnList, ToolWin, ExtDlgs,
  Drawings, ViewPort, GObjBase, GObjects, Manage, Options0,
{$IFNDEF FPC}
  Windows, HH, hh_funcs
{$ELSE}
  LCLIntf, LCLType, LResources
{$ENDIF}
  , Devices, Modes;

type

//  TRealTypeX = Double;
  TRealTypeX = Single;

{ TMainForm }

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    StatusBar1: TStatusBar;
    LocalPopUp: TPopupMenu;
    File1: TMenuItem;
    OpenDoc1: TMenuItem;
    Save1: TMenuItem;
    DrawingSaveDlg: TSaveDialog;
    New1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Copytoclipboard1: TMenuItem;
    Test1: TMenuItem;
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
    Transform1: TMenuItem;
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
    DuplicateSelected: TAction;
    Duplicateselected1: TMenuItem;
    Options1: TMenuItem;
    Insertstar1: TMenuItem;
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
    Usesnap2: TMenuItem;
    AngularSnap1: TMenuItem;
    Useareatoselectobjects2: TMenuItem;
    ShowGrid: TAction;
    SnapToGrid: TAction;
    AngularSnap: TAction;
    N11: TMenuItem;
    AreaSelect: TAction;
    Areaselect2: TMenuItem;
    AreaSelectInsideAction: TAction;
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
    ConvertToPolyline: TAction;
    TpXsettings1: TMenuItem;
    ProgressBar1: TProgressBar;
    Undo: TAction;
    Redo: TAction;
    N17: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    ShowRulers1: TMenuItem;
    Help2: TMenuItem;
    About1: TMenuItem;
    CustomTransform: TAction;
    Areaselect3: TMenuItem;
    Scale2: TMenuItem;
    InsertCurve: TAction;
    InsertClosedCurve: TAction;
    Insertcurve1: TMenuItem;
    Insertclosedcurve1: TMenuItem;
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
    ConvertToGrayScale: TAction;
    Converttograyscale1: TMenuItem;
    Panel30: TPanel;
    ToolBar1: TToolBar;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton13: TToolButton;
    BasicModeBtn: TToolButton;
    AreaSelectBtn: TToolButton;
    ToolButton17: TToolButton;
    ClipboardCutBtn: TToolButton;
    ClipboardCopyBtn: TToolButton;
    ClipboardPasteBtn: TToolButton;
    ToolButton7: TToolButton;
    UndoBtn: TToolButton;
    RedoBtn: TToolButton;
    ToolButton14: TToolButton;
    ZoomAreaBtn: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    PanningBtn: TToolButton;
    ToolButton12: TToolButton;
    ToolButton4: TToolButton;
    ToolButton8: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ScalePhysical: TAction;
    Scalephysicalunits1: TMenuItem;
    InsertBezierPath: TAction;
    InsertClosedBezierPath: TAction;
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
    none1: TMenuItem;
    none2: TMenuItem;
    InsertSymbol: TAction;
    Insertsymbol1: TMenuItem;
    PropertiesToolbar1: TToolBar;
    ComboBox1: TComboBox;
    ComboBox3: TComboBox;
    ComboBox6: TComboBox;
    ComboBox2: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    SimplifyPoly: TAction;
    RotateTextAction: TAction;
    RotateSymbolsAction: TAction;
    N4: TMenuItem;
    Rotatetext1: TMenuItem;
    Rotatesymbols1: TMenuItem;
    DrawingSource: TAction;
    preview_tex_inc: TAction;
    metapost_tex_inc: TAction;
    Viewsource1: TMenuItem;
    Drawingsource1: TMenuItem;
    previewtexinc1: TMenuItem;
    metaposttexinc1: TMenuItem;
    ScaleLineWidthAction: TAction;
    Scalelinewidth1: TMenuItem;
    PictureProperties: TAction;
    ObjectProperties: TAction;
    Objectproperties1: TMenuItem;
    CopyPictureToClipboard: TAction;
    ScaleStandard: TAction;
    PictureInfo: TAction;
    ExitProgram: TAction;
    TpXHelp: TAction;
    About: TAction;
    BasicModeAction: TAction;
    tikz1: TMenuItem;
    tikz2: TMenuItem;
    FreehandPolyline: TAction;
    N18: TMenuItem;
    Freehandpolyline1: TMenuItem;
    Modify1: TMenuItem;
    Simplifypolylinepolygon1: TMenuItem;
    ReversePoints: TAction;
    Reversepoints1: TMenuItem;
    ConnectPaths: TAction;
    Connectpaths1: TMenuItem;
    TpXSettings: TAction;
    ScaleTextAction: TAction;
    Scaletext1: TMenuItem;
    ToolButton1: TToolButton;
    TeXFormat: TAction;
    PdfTeXFormat: TAction;
    ToolButton2: TToolButton;
    ToolButton15: TToolButton;
    AlignLeft: TAction;
    AlignRight: TAction;
    AlignHCenter: TAction;
    AlignTop: TAction;
    AlignBottom: TAction;
    AlignVCenter: TAction;
    Align1: TMenuItem;
    Bottom1: TMenuItem;
    Left1: TMenuItem;
    HCenter1: TMenuItem;
    Right1: TMenuItem;
    VCenter1: TMenuItem;
    op1: TMenuItem;
    Group: TAction;
    Group1: TMenuItem;
    BreakPath: TAction;
    DeletePoint: TAction;
    AddPoint: TAction;
    Panel31: TPanel;
    Panel20: TPanel;
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
    InsertBezierPathBtn: TToolButton;
    InsertClosedBezierPathBtn: TToolButton;
    InsertTextBtn: TToolButton;
    InsertStarBtn: TToolButton;
    InsertSymbolBtn: TToolButton;
    FreehandPolylineBtn: TToolButton;
    Panel32: TPanel;
    Panel33: TPanel;
    Panel34: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    HScrollBar: TScrollBar;
    Panel1: TPanel;
    VScrollBar: TScrollBar;
    SimplifyBezier: TAction;
    SimplifyBezier1: TMenuItem;
    DeleteSmallObjects: TAction;
    Deletesmallobjects1: TMenuItem;
    FreehandBezierBtn: TToolButton;
    FreehandBezier: TAction;
    FreehandBeziercurve1: TMenuItem;
    InsertBitmap: TAction;
    InsertBitmapBtn: TToolButton;
    Insertbitmap1: TMenuItem;
    OpenBitmapDlg: TOpenPictureDialog;
    Ungroup: TAction;
    Ungroup1: TMenuItem;
    MakeCompound: TAction;
    Makecompound1: TMenuItem;
    ToolButton16: TToolButton;
    Uncompound: TAction;
    Uncompound1: TMenuItem;
    ShowCrossHair: TAction;
    Showcrosshair1: TMenuItem;
    GridOnTop: TAction;
    Gridontop1: TMenuItem;
    ToolButton22: TToolButton;
    Image1: TImage;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    PickUpProperties: TAction;
    DefaultProperties: TAction;
    Panel4: TPanel;
    Image3: TImage;
    Panel5: TPanel;
    Panel6: TPanel;
    Image4: TImage;
    Image2: TImage;
    ApplyProperties: TAction;
    ToolButton27: TToolButton;
    PropertiesToolbar2: TToolBar;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Edit3: TEdit;
    Panel7: TPanel;
    Image5: TImage;
    ToolButton28: TToolButton;
    Panel8: TPanel;
    Image6: TImage;
    ComboBox7: TComboBox;
    Edit4: TEdit;
    Panel9: TPanel;
    Image7: TImage;
    ToolButton29: TToolButton;
    ComboBox10: TComboBox;
    Edit5: TEdit;
    ShowPropertiesToolbar11: TMenuItem;
    ShowPropertiesToolbar21: TMenuItem;
    ShowPropertiesToolbar2: TAction;
    ShowPropertiesToolbar1: TAction;
    procedure AreaSelectExecute(Sender: TObject);
    procedure HandToolExecute(Sender: TObject);
    procedure InsertLineExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InsertArcExecute(Sender: TObject);
    procedure InsertPolylineExecute(Sender: TObject);
    procedure LocalViewMouseMove2D(Sender: TObject;
      Shift: TShiftState; WX, WY: TRealTypeX; X, Y: Integer);
    procedure ShowGridExecute(Sender: TObject);
    procedure InsertRectangleExecute(Sender: TObject);
    procedure InsertEllipseExecute(Sender: TObject);
    procedure InsertPolygonExecute(Sender: TObject);
    procedure InsertTextExecute(Sender: TObject);
    procedure Test1Click(Sender: TObject);
    procedure LocalViewMouseUp2D(Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState; WX, WY: TRealTypeX; X, Y: Integer);
    procedure LocalViewMouseDown2D(Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState; WX, WY: TRealTypeX; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure UserEventExecute(Sender: TObject);
    procedure InsertCircleExecute(Sender: TObject);
    procedure InsertStarExecute(Sender: TObject);
    procedure BasicModeExecute(Sender: TObject);
    procedure InsertSectorExecute(Sender: TObject);
    procedure InsertSegmentExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose:
      Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure CaptureEMFExecute(Sender: TObject);
    procedure Tools1Click(Sender: TObject);
    procedure ShowRulersExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LocalViewEndRedraw(Sender: TObject);
    procedure InsertCurveExecute(Sender: TObject);
    procedure InsertClosedCurveExecute(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode:
      TScrollCode;
      var ScrollPos: Integer);
    procedure ShowScrollBarsExecute(Sender: TObject);
    procedure ConvertToExecute(Sender: TObject);
    procedure DoConvertToExecute(Sender: TObject);
    procedure OpenRecentExecute(Sender: TObject);
    procedure ColorBox_DrawItem(Control: TWinControl; Index:
      Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ChangeProperties(Sender: TObject);
    procedure ScalePhysicalExecute(Sender: TObject);
    procedure InsertBezierPathExecute(Sender: TObject);
    procedure InsertClosedBezierPathExecute(Sender: TObject);
    procedure PopupMenuDVIPopup(Sender: TObject);
    procedure DVI_Format_Click(Sender: TObject);
    procedure PopupMenuPdfPopup(Sender: TObject);
    procedure Pdf_Format_Click(Sender: TObject);
    procedure InsertSymbolExecute(Sender: TObject);
    procedure RotateTextActionExecute(Sender: TObject);
    procedure RotateSymbolsActionExecute(Sender: TObject);
    procedure ScaleLineWidthActionExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ZoomAreaExecute(Sender: TObject);
    procedure FreehandPolylineExecute(Sender: TObject);
    procedure ScaleTextActionExecute(Sender: TObject);
    procedure TeXFormatExecute(Sender: TObject);
    procedure PdfTeXFormatExecute(Sender: TObject);
    procedure FreehandBezierExecute(Sender: TObject);
    procedure InsertBitmapExecute(Sender: TObject);
    procedure ToolButton16Click(Sender: TObject);
    procedure ShowCrossHairExecute(Sender: TObject);
    procedure GridOnTopExecute(Sender: TObject);
    procedure DefaultPropertiesExecute(Sender: TObject);
    procedure PickUpPropertiesExecute(Sender: TObject);
    procedure ApplyPropertiesExecute(Sender: TObject);
    procedure ComboBox10DrawItem(Control: TWinControl; Index:
      Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ShowPropertiesToolbar1Execute(Sender: TObject);
    procedure ShowPropertiesToolbar2Execute(Sender: TObject);
  private
    { Private declarations }
    ScrollPos0: Integer;             
{$IFDEF VER140}
    procedure SetFormPosition;
    procedure GetFormPosition;
{$ENDIF}
  public
    { Public declarations }
    TheDrawing: TDrawing2D;
    EventManager: TTpXManager;
    LocalView: TViewport2D;
    Ruler1: TRuler;
    Ruler2: TRuler;
    FormPos_Left, FormPos_Top, FormPos_Width, FormPos_Height:
    Integer;
    FormPos_Maximized: Boolean;
    procedure OnExit(Sender: TObject);
    procedure LocalViewDblClick(Sender: TObject);
    procedure LocalViewMouseWheel(Sender: TObject; Shift:
      TShiftState;
      WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure LocalViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LocalViewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ShowTpXHelp;
    procedure PressModeButton(Btn: TObject; Pressed: Boolean);
    procedure RecentChanged(Sender: TObject);
    procedure ShowMouseCoordinates(const St: string);
    procedure FillLocalPopUp(
      const HasObject, HasSelection, IsOnObject,
      IsOnPoint, CanDeletePoints: Boolean);
    procedure SetCurrentProperties;
  end;

var
  MainForm: TMainForm;
{$IFDEF VER140}
var
  mHHelp: THookHelpSystem;
{$ENDIF}

const
  crHand = 1;
  crPlus = 2;
  crPen = 3;

implementation

uses Output, Input, Settings0, ColorEtc, Geometry, Options,
  Preview,
  SysBasic, Modify, Propert;

{$IFDEF VER140}
{$R *.lfm}

procedure TMainForm.SetFormPosition;
var
  WindowPlacement: TWindowPlacement;
  R: TRect;
begin
  FillChar(WindowPlacement, SizeOf(WindowPlacement), #0);
  WindowPlacement.length := SizeOf(WindowPlacement);
  if FormPos_Maximized
    then WindowPlacement.showcmd := SW_SHOWMAXIMIZED
  else WindowPlacement.showcmd := SW_SHOWNORMAL;
  R.Left := FormPos_Left;
  R.Top := FormPos_Top;
  R.Right := R.Left + FormPos_Width;
  R.Bottom := R.Top + FormPos_Height;
  WindowPlacement.rcNormalPosition := R;
  SetWindowPlacement(handle, @WindowPlacement);
end;

procedure TMainForm.GetFormPosition;
var
  WindowPlacement: TWindowPlacement;
  R: TRect;
begin
  WindowPlacement.length := SizeOf(WindowPlacement);
  GetWindowPlacement(handle, @WindowPlacement);
  R := WindowPlacement.rcNormalPosition;
  FormPos_Maximized := WindowState = wsMaximized;
  FormPos_Left := R.Left;
  FormPos_Top := R.Top;
  FormPos_Width := R.Right - R.Left;
  FormPos_Height := R.Bottom - R.Top;
end;

{$ENDIF}



procedure TMainForm.FormCreate(Sender: TObject);
begin
  FormPos_Left := MainForm.Left;
  FormPos_Top := MainForm.Top;
  FormPos_Width := MainForm.Width;
  FormPos_Height := MainForm.Height;
  TheDrawing := TDrawing2D.Create(Self);
  LocalView := TViewport2D.Create(Panel1);
  EventManager := TTpXManager.Create(TheDrawing, LocalView);
  EventManager.OnExit := OnExit;
  EventManager.PushMode(BaseMode);
  EventManager.RecentFiles.OnChange := RecentChanged;
  Ruler1 := TRuler.Create(Panel2);
  Ruler2 := TRuler.Create(Panel3);
  LocalView.Parent := Panel1;
  LocalView.Drawing2D := TheDrawing;
  LocalView.Align := alClient;
  LocalView.ControlPointsColor := clBlack;
//  LocalView.GridStep := 10;
  LocalView.ControlPointsWidth := 8;
  LocalView.ShowControlPoints := True;
  LocalView.ShowGrid := True;
  LocalView.ShowCrossHair := True;
  LocalView.GridOnTop := False;
  LocalView.OnEndRedraw := LocalViewEndRedraw;
  LocalView.OnDblClick := LocalViewDblClick;
  LocalView.OnKeyDown := LocalViewKeyDown;
  LocalView.OnKeyUp := LocalViewKeyUp;
  LocalView.OnMouseMove2D := LocalViewMouseMove2D;
  LocalView.OnMouseDown2D := LocalViewMouseDown2D;
  LocalView.OnMouseUp2D := LocalViewMouseUp2D;
  LocalView.OnMouseWheel := LocalViewMouseWheel;
  Ruler1.Parent := Panel2;
  Ruler1.LinkedViewport := LocalView;
  Ruler1.Align := alClient;
  Ruler1.Color := clBtnFace;
  Ruler1.Orientation := otHorizontal;
  Ruler2.Parent := Panel3;
  Ruler2.LinkedViewport := LocalView;
  Ruler2.Align := alClient;
  Ruler2.Color := clBtnFace;
  OpenDialog_FilterIndex := 2;
  //TpXExtAssoc := TTpXExtAssoc.Create;
{$IFDEF VER140}
  mHHelp := THookHelpSystem.Create('TpX.chm', '', htHHAPI);
  Screen.Cursors[crHand] := LoadCursor(HInstance, 'HAND');
  Screen.Cursors[crPlus] := LoadCursor(HInstance, 'PLUSCOPY');
  Screen.Cursors[crPen] := LoadCursor(HInstance, 'PEN');
{$ENDIF}
  Caption := Drawing_NewFileName;
  ScrollPos0 := -1;
  ShowScrollBars.Checked := True;
  ShowPropertiesToolbar1.Checked := True;
  ShowPropertiesToolbar2.Checked := True;
  LocalView.VisualRect := Rect2D(0, 0, 100, 100);
  SmoothBezierNodes := SmoothBezierNodesAction.Checked;
  ScaleLineWidthAction.Checked := ScaleLineWidth;
  NewDoc.Tag := Msg_New;
  NewWindow.Tag := Msg_NewWindow;
  OpenDoc.Tag := Msg_Open;
  SaveDoc.Tag := Msg_Save;
  Print.Tag := Msg_Print;
  SaveAs.Tag := Msg_SaveAs;
  CopyPictureToClipboard.Tag := Msg_CopyAsEMF;
  TpXSettings.Tag := Msg_TpXSettings;
  Undo.Tag := Msg_Undo;
  Redo.Tag := Msg_Redo;
  ExitProgram.Tag := Msg_Exit;
  ClipboardCopy.Tag := Msg_Copy;
  ClipboardPaste.Tag := Msg_Paste;
  ClipboardCut.Tag := Msg_Cut;
  DeleteSelected.Tag := Msg_Delete;
  DuplicateSelected.Tag := Msg_Duplicate;
  SelectAll.Tag := Msg_SelectAll;
  SelNext.Tag := Msg_SelNext;
  SelPrev.Tag := Msg_SelPrev;
  SnapToGrid.Tag := Msg_SnapToGrid;
  AngularSnap.Tag := Msg_AngularSnap;
  SmoothBezierNodesAction.Tag := Msg_SmoothBezierNodes;
  AreaSelectInsideAction.Tag := Msg_AreaSelectInside;
  AreaSelect.Tag := Msg_AreaSelect;
  ObjectProperties.Tag := Msg_SelectedProperties;
  PictureProperties.Tag := Msg_PictureProperties;

  ConvertTo.Tag := Msg_ConvertTo;
  SimplifyPoly.Tag := Msg_SimplifyPoly;
  SimplifyBezier.Tag := Msg_SimplifyBezierPaths;
  ConnectPaths.Tag := Msg_ConnectPaths;
  ReversePoints.Tag := Msg_ReversePoints;
  DeleteSmallObjects.Tag := Msg_DeleteSmall;
  Group.Tag := Msg_Group;
  Ungroup.Tag := Msg_Ungroup;
  MakeCompound.Tag := Msg_MakeCompound;
  Uncompound.Tag := Msg_Uncompound;
  BreakPath.Tag := Msg_BreakPath;
  DeletePoint.Tag := Msg_DeletePoint;
  AddPoint.Tag := Msg_AddPoint;

  MoveUp.Tag := Msg_MoveUp;
  MoveDown.Tag := Msg_MoveDown;
  MoveLeft.Tag := Msg_MoveLeft;
  MoveRight.Tag := Msg_MoveRight;
  MoveUpPixel.Tag := Msg_MoveUpPixel;
  MoveDownPixel.Tag := Msg_MoveDownPixel;
  MoveLeftPixel.Tag := Msg_MoveLeftPixel;
  MoveRightPixel.Tag := Msg_MoveRightPixel;
  FlipV.Tag := Msg_FlipV;
  FlipH.Tag := Msg_FlipH;
  RotateCounterclockW.Tag := Msg_RotateCounterclockW;
  RotateClockW.Tag := Msg_RotateClockW;
  RotateCounterclockWDegree.Tag :=
    Msg_RotateCounterclockWDegree;
  RotateClockWDegree.Tag := Msg_RotateClockWDegree;
  Grow10.Tag := Msg_Grow10;
  Shrink10.Tag := Msg_Shrink10;
  Grow1.Tag := Msg_Grow1;
  Shrink1.Tag := Msg_Shrink1;
  StartRotate.Tag := Msg_StartRotate;
  StartMove.Tag := Msg_StartMove;
  ScaleStandard.Tag := Msg_ScaleStandard;
  CustomTransform.Tag := Msg_CustomTransform;
  ConvertToGrayScale.Tag := Msg_ConvertToGrayScale;

  MoveForward.Tag := Msg_MoveForward;
  MoveBackward.Tag := Msg_MoveBackward;
  MoveToFront.Tag := Msg_MoveToFront;
  MoveToBack.Tag := Msg_MoveToBack;

  AlignLeft.Tag := Msg_AlignLeft;
  AlignHCenter.Tag := Msg_AlignHCenter;
  AlignRight.Tag := Msg_AlignRight;
  AlignBottom.Tag := Msg_AlignBottom;
  AlignVCenter.Tag := Msg_AlignVCenter;
  AlignTop.Tag := Msg_AlignTop;

  PreviewLaTeX.Tag := Msg_PreviewLaTeX;
  PreviewPdfLaTeX.Tag := Msg_PreviewPdfLaTeX;
  PreviewLaTeX_PS.Tag := Msg_PreviewLaTeX_PS;
  PreviewSVG.Tag := Msg_PreviewSVG;
  PreviewEMF.Tag := Msg_PreviewEMF;
  PreviewEPS.Tag := Msg_PreviewEPS;
  PreviewPNG.Tag := Msg_PreviewPNG;
  PreviewBMP.Tag := Msg_PreviewBMP;
  PreviewPDF.Tag := Msg_PreviewPDF;
  DrawingSource.Tag := Msg_DrawingSource;
  preview_tex_inc.Tag := Msg_preview_tex_inc;
  metapost_tex_inc.Tag := Msg_metapost_tex_inc;
  CaptureEMF.Tag := Msg_CaptureEMF;
  ImageTool.Tag := Msg_ImageTool;
  ZoomArea.Tag := Msg_ZoomArea;
  ZoomIn.Tag := Msg_ZoomIn;
  ZoomOut.Tag := Msg_ZoomOut;
  ZoomAll.Tag := Msg_ZoomAll;
  HandTool.Tag := Msg_Panning;
  TpXHelp.Tag := Msg_Help;
  PictureInfo.Tag := Msg_PictureInfo;
  About.Tag := Msg_About;

  MakeColorBox(ComboBox3);
  MakeColorBox(ComboBox4);
  MakeColorBox(ComboBox5);
  ComboBox1.ItemIndex := 1;
  ComboBox2.ItemIndex := 0;
  ComboBox6.ItemIndex := 1;
  ComboBox10.ItemIndex := 0;

  ComboBox8.OnDrawItem := PropertiesForm.ArrComboBoxDrawItem;
  ComboBox9.OnDrawItem := PropertiesForm.ArrComboBoxDrawItem;

  ComboBox1.Tag := Byte(chpLS) + 1;
  ComboBox3.Tag := Byte(chpLC) + 1;
  ComboBox6.Tag := Byte(chpLW) + 1;
  ComboBox2.Tag := Byte(chpHa) + 1;
  ComboBox4.Tag := Byte(chpHC) + 1;
  ComboBox5.Tag := Byte(chpFC) + 1;
  ComboBox8.Tag := Byte(chpArr1) + 1;
  ComboBox9.Tag := Byte(chpArr2) + 1;
  Edit3.Tag := Byte(chpArrS) + 1;
  Edit4.Tag := Byte(chpFH) + 1;
  ComboBox7.Tag := Byte(chpHJ) + 1;
  ComboBox10.Tag := Byte(chpSK) + 1;
  Edit5.Tag := Byte(chpSS) + 1;

{$IFDEF VER140}
{$ELSE}
  CopyPictureToClipboard.Visible := False;
  CaptureEMF.Visible := False;
  ImageTool.Visible := False;
{$ENDIF}

  EventManager.SendMessage(Msg_StartProgram, Self);   
{$IFDEF VER140}
  SetFormPosition;    
{$ENDIF}
end;

procedure TMainForm.AreaSelectExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_AreaSelect, AreaSelectBtn);
end;

procedure TMainForm.HandToolExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_Panning, PanningBtn);
end;

procedure TMainForm.ShowGridExecute(Sender: TObject);
begin
  ShowGrid.Checked := not ShowGrid.Checked;
  LocalView.ShowGrid := ShowGrid.Checked;
end;

procedure TMainForm.LocalViewMouseMove2D(Sender: TObject;
  Shift: TShiftState; WX, WY: TRealTypeX; X, Y: Integer);
var
  CurrPoint2D: TPoint2D;
begin
  CurrPoint2D := LocalView.GetSnappedPoint(
    Point2D(WX, WY));
  ShowMouseCoordinates(
    Format('X: %6.3f Y: %6.3f',
    [CurrPoint2D.X, CurrPoint2D.Y]));
  EventManager.MouseMove(Sender, Shift, X, Y);
end;

procedure TMainForm.ShowMouseCoordinates(const St: string);
begin
  StatusBar1.Panels[0].Text := St;
end;

procedure TMainForm.FillLocalPopUp(
  const HasObject, HasSelection, IsOnObject,
  IsOnPoint, CanDeletePoints: Boolean);
var
  Item: TMenuItem;
  procedure AddAction(AnAction: TBasicAction);
  begin
    Item := TMenuItem.Create(LocalPopUp);
    Item.Action := AnAction;
    LocalPopUp.Items.Add(Item);
  end;
begin
  LocalPopUp.Items.Clear;
  if HasSelection then AddAction(ClipboardCopy);
  AddAction(ClipboardPaste);
  if HasSelection then AddAction(ClipboardCut);
  if HasSelection then AddAction(DeleteSelected);
  if HasSelection then AddAction(DuplicateSelected);
  if HasObject then AddAction(ObjectProperties);
  if HasSelection then AddAction(ConvertTo);
  if IsOnObject and CanDeletePoints then AddAction(AddPoint);
  if IsOnPoint and CanDeletePoints then AddAction(DeletePoint);
  if IsOnObject then AddAction(BreakPath);
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
  Button: TMouseButton; Shift: TShiftState; WX, WY: TRealTypeX;
  X, Y: Integer);
begin
  EventManager.MouseDown(Sender, Button, Shift, X, Y);
end;

procedure TMainForm.LocalViewMouseUp2D(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState;
  WX, WY: TRealTypeX; X, Y: Integer);
begin
  EventManager.MouseUp(Sender, Button, Shift, X, Y);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //EventManager.KeyDown(Sender, Key,  Shift);
end;

procedure TMainForm.LocalViewKeyDown(Sender: TObject; var Key:
  Word;
  Shift: TShiftState);
begin
  EventManager.KeyDown(Sender, Key, Shift);
end;

procedure TMainForm.LocalViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  EventManager.KeyUp(Sender, Key, Shift);
end;

procedure TMainForm.UserEventExecute(Sender: TObject);
begin
  EventManager.SendMessage((Sender as TAction).Tag, Sender);
end;

procedure TMainForm.BasicModeExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Sender);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var
  CanClose: Boolean);
begin
  CanClose := BaseMode.AskSaveCurrentDrawing <> mrCancel;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin        
{$IFDEF VER140}
  GetFormPosition;        
{$ENDIF}
  EventManager.SendMessage(Msg_Stop, Sender);
  //TpXExtAssoc.Free;
{$IFDEF VER140}
  mHHelp.Free;
  HHCloseAll; //Close help before shutdown or big trouble
{$ENDIF}
  TheDrawing.Free;
  EventManager.Free;
  LocalView.Free;
  Ruler1.Free;
  Ruler2.Free;
end;

procedure TMainForm.ShowTpXHelp;
begin
{$IFDEF VER140}
  HH.HtmlHelp(GetDesktopWindow,
    PChar(ExtractFilePath(Application.ExeName)
    + 'TpX.chm::/tpx_tpxabout_tpx_drawing_tool.htm'),
    HH_DISPLAY_TOPIC, 0);
{$ELSE}
  OpenOrExec(HtmlViewerPath,
    PChar('file://' +
    ExtractFilePath(Application.ExeName)
    + 'help/tpx_tpxabout_tpx_drawing_tool.htm'));
//  FileExec(Format('%s "%s"',
//    [HtmlViewerPath,
//      PChar(ExtractFilePath(Application.ExeName)
//      + 'help/tpx_tpxabout_tpx_drawing_tool.htm')]), '', '',
//    TempDir, Hide, True);
{$ENDIF}
end;

procedure TMainForm.CaptureEMFExecute(Sender: TObject);
{$IFDEF VER140}
var
  MF: TMetaFile;
{$ENDIF}
begin
{$IFDEF VER140}
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
{$ENDIF}
end;

procedure TMainForm.Tools1Click(Sender: TObject);
begin
  CaptureEMF.Enabled := Clipboard.HasFormat(CF_METAFILEPICT);
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

procedure TMainForm.ShowPropertiesToolbar1Execute(Sender: TObject);
begin
  ShowPropertiesToolbar1.Checked :=
    not ShowPropertiesToolbar1.Checked;
  PropertiesToolbar1.Visible := ShowPropertiesToolbar1.Checked;
end;

procedure TMainForm.ShowPropertiesToolbar2Execute(Sender: TObject);
begin
  ShowPropertiesToolbar2.Checked :=
    not ShowPropertiesToolbar2.Checked;
  PropertiesToolbar2.Visible := ShowPropertiesToolbar2.Checked;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ShowGrid.Checked := LocalView.ShowGrid;
  ShowCrossHair.Checked := LocalView.ShowCrossHair;
  GridOnTop.Checked := LocalView.GridOnTop;
  ShowRulers.Checked := LocalView.ShowRulers;
  AreaSelectInsideAction.Checked := AreaSelectInside;
  SnapToGrid.Checked := UseSnap;
  AngularSnap.Checked := UseAngularSnap;
  Panel2.Visible := ShowRulers.Checked;
  Panel3.Visible := ShowRulers.Checked;
  HScrollBar.Visible := ShowScrollBars.Checked;
  VScrollBar.Visible := ShowScrollBars.Checked;
  Showscrollbars1.Checked := ShowScrollBars.Checked;
  Panel31.Realign;
  //Scalephysicalunits1.Checked := ScalePhysical.Checked;
end;

procedure TMainForm.LocalViewEndRedraw(Sender: TObject);
begin
  Ruler1.Paint;
  Ruler2.Paint;
end;

procedure TMainForm.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  F: Double;
begin
  case ScrollCode of
    scEndScroll, scPosition:
      begin
        ScrollPos := 50;
        ScrollPos0 := -1;
      end;
    scLineUp, scLineDown, scPageUp, scPageDown, scTrack:
      begin
        if ScrollPos0 < 0 then ScrollPos0 := 50;
        F := (ScrollPos - ScrollPos0) / 50;
        if Sender = HScrollBar then
          LocalView.PanWindowFraction(F, 0)
        else
          LocalView.PanWindowFraction(0, F);
        if ScrollCode = scTrack
          then
          ScrollPos0 := ScrollPos
        else
        begin
          ScrollPos := 50;
          ScrollPos0 := -1;
        end;
      end;
  end;
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
    Item.Tag := Msg_ConvertTo + I - 1;
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
  EventManager.SendMessage((Sender as TMenuItem).Tag, Sender);
end;

procedure TMainForm.OpenRecentExecute(Sender: TObject);
var
  I: Integer;
begin
  if not (Sender is TMenuItem) then Exit;
  EventManager.SendMessage(Msg_Escape, Self);
  I := (Sender as TMenuItem).Tag;
  OpenRecentMode.Index := I;
  EventManager.PushMode(OpenRecentMode);
end;

procedure TMainForm.ColorBox_DrawItem(Control: TWinControl;
  Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  ColorBoxDrawItem(Control as TComboBox, Index, Rect, State);
end;

procedure TMainForm.ChangeProperties(Sender: TObject);
var
  Kind: TChangePropertiesKind;
begin
  if not (Sender is TComponent) then Exit;
  if (Sender as TComponent).Tag <= 0 then Exit;
  Kind := TChangePropertiesKind((Sender as TComponent).Tag - 1);
  case Kind of
    chpLS: TheDrawing.New_LineStyle
      := TLineStyle((Sender as TComboBox).ItemIndex);
    chpLC:
      begin
        ColorBoxSelect(Sender as TComboBox);
        TheDrawing.New_LineColor
          := ColorBoxGet(Sender as TComboBox);
      end;
    chpLW:
      begin
        TheDrawing.New_LineWidth
          := StrToFloat((Sender as TComboBox).Text);
        if TheDrawing.New_LineWidth <= 0 then
        begin
          TheDrawing.New_LineWidth := 1;
          Exit;
        end;
      end;
    chpHa: TheDrawing.New_Hatching
      := THatching((Sender as TComboBox).ItemIndex);
    chpHC:
      begin
        ColorBoxSelect(Sender as TComboBox);
        TheDrawing.New_HatchColor
          := ColorBoxGet(Sender as TComboBox);
      end;
    chpFC:
      begin
        ColorBoxSelect(Sender as TComboBox);
        TheDrawing.New_FillColor
          := ColorBoxGet(Sender as TComboBox);
      end;
    chpArr1: TheDrawing.New_Arr1
      := (Sender as TComboBox).ItemIndex;
    chpArr2: TheDrawing.New_Arr2
      := (Sender as TComboBox).ItemIndex;
    chpArrS: TheDrawing.New_ArrSizeFactor
      := StrToRealType((Sender as TEdit).Text, 1);
    chpFH: TheDrawing.New_FontHeight
      := StrToRealType((Sender as TEdit).Text,
        TheDrawing.DefaultFontHeight);
    chpHJ: TheDrawing.New_HAlignment
      := (Sender as TComboBox).ItemIndex;
    chpSK: TheDrawing.New_StarKind
      := (Sender as TComboBox).ItemIndex;
    chpSS: TheDrawing.New_StarSizeFactor
      := StrToRealType((Sender as TEdit).Text, 1);
  else Exit;
  end;
  ChangeSelectedProperties(TheDrawing, [Kind]);
  TheDrawing.RepaintViewports;
end;

procedure TMainForm.SetCurrentProperties;
begin
  ComboBox1.ItemIndex := Ord(TheDrawing.New_LineStyle);
  ComboBox2.ItemIndex := Ord(TheDrawing.New_Hatching);
  ColorBoxSet(ComboBox3, TheDrawing.New_LineColor);
  ColorBoxSet(ComboBox4, TheDrawing.New_HatchColor);
  ColorBoxSet(ComboBox5, TheDrawing.New_FillColor);
  ComboBox6.Text := RealTypeToStr(TheDrawing.New_LineWidth);
  ComboBox8.ItemIndex := TheDrawing.New_Arr1;
  ComboBox9.ItemIndex := TheDrawing.New_Arr2;
  Edit3.Text := RealTypeToStr(TheDrawing.New_ArrSizeFactor);
  Edit4.Text := RealTypeToStr(TheDrawing.New_FontHeight);
  ComboBox7.ItemIndex := TheDrawing.New_HAlignment;
  ComboBox10.ItemIndex := TheDrawing.New_StarKind;
  Edit5.Text := RealTypeToStr(TheDrawing.New_StarSizeFactor);
end;

procedure TMainForm.DefaultPropertiesExecute(Sender: TObject);
begin
  TheDrawing.SetDefaultProperties;
  SetCurrentProperties;
  ApplyPropertiesExecute(Sender);
end;

procedure TMainForm.PickUpPropertiesExecute(Sender: TObject);
begin
  TheDrawing.PickUpProperties(TheDrawing.SelectedObjects.FirstObj);
  SetCurrentProperties;
end;

procedure TMainForm.ApplyPropertiesExecute(Sender: TObject);
begin
  ChangeSelectedProperties(TheDrawing,
    [TChangePropertiesKind(0)..
    TChangePropertiesKind(High(TChangePropertiesKind))]);
  TheDrawing.RepaintViewports;
end;

procedure TMainForm.ScalePhysicalExecute(Sender: TObject);
begin
  ScalePhysical.Checked := not ScalePhysical.Checked;
end;

procedure TMainForm.PopupMenuDVIPopup(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to PopupMenuDVI.Items.Count - 1 do
    PopupMenuDVI.Items[I].Checked := False;
  PopupMenuDVI.Items[Ord(TheDrawing.TeXFormat)].Checked := True;
end;

procedure TMainForm.DVI_Format_Click(Sender: TObject);
begin
  if not (Sender is TMenuItem) then Exit;
  TheDrawing.TeXFormat := TeXFormatKind((Sender as
    TMenuItem).MenuIndex);
  TheDrawing.History.SetPropertiesChanged;
  //SaveDoc.Enabled := TheDrawing.History.IsChanged;
end;

procedure TMainForm.PopupMenuPdfPopup(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to PopupMenuPdf.Items.Count - 1 do
    PopupMenuPdf.Items[I].Checked := False;
  PopupMenuPdf.Items[Ord(TheDrawing.PdfTeXFormat)].Checked := True;
end;

procedure TMainForm.Pdf_Format_Click(Sender: TObject);
begin
  if not (Sender is TMenuItem) then Exit;
  TheDrawing.PdfTeXFormat := PdfTeXFormatKind((Sender as
    TMenuItem).MenuIndex);
  TheDrawing.History.SetPropertiesChanged;
  //SaveDoc.Enabled := TheDrawing.History.IsChanged;
end;

procedure TMainForm.ScaleTextActionExecute(Sender: TObject);
begin
  ScaleTextAction.Checked := not ScaleTextAction.Checked;
  ScaleText := ScaleTextAction.Checked;
end;

procedure TMainForm.RotateTextActionExecute(Sender: TObject);
begin
  RotateTextAction.Checked := not RotateTextAction.Checked;
  RotateText := RotateTextAction.Checked;
end;

procedure TMainForm.RotateSymbolsActionExecute(Sender: TObject);
begin
  RotateSymbolsAction.Checked := not RotateSymbolsAction.Checked;
  RotateSymbols := RotateSymbolsAction.Checked;
end;

procedure TMainForm.ScaleLineWidthActionExecute(Sender: TObject);
begin
  ScaleLineWidthAction.Checked := not ScaleLineWidthAction.Checked;
  ScaleLineWidth := ScaleLineWidthAction.Checked;
end;

procedure TMainForm.OnExit(Sender: TObject);
var
  CanClose: Boolean;
begin
  FormCloseQuery(Self, CanClose);
  if CanClose then Close;
end;

procedure TMainForm.LocalViewDblClick(Sender: TObject);
begin
  EventManager.DblClick(Sender);
end;

procedure TMainForm.LocalViewMouseWheel(Sender: TObject; Shift:
  TShiftState;
  WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  EventManager.MouseWheel(Sender, Shift,
    WheelDelta, MousePos, Handled);
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift:
  TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  EventManager.MouseWheel(Sender, Shift,
    WheelDelta, MousePos, Handled);
end;

procedure TMainForm.ZoomAreaExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_ZoomArea, ZoomAreaBtn);
end;

procedure TMainForm.RecentChanged(Sender: TObject);
var
  Item: TMenuItem;
  I: Integer;
begin
  Recentfiles1.Clear;
  for I := 0 to EventManager.RecentFiles.Count - 1 do
  begin
    Item := TMenuItem.Create(Recentfiles1);
    Item.Caption := EventManager.RecentShort[I];
    Item.Tag := I;
    Item.OnClick := OpenRecentExecute;
    Recentfiles1.Add(Item);
  end;
end;

procedure TMainForm.InsertLineExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertLine, InsertLineBtn);
end;

procedure TMainForm.InsertRectangleExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertRectangle,
    InsertRectangleBtn);
end;

procedure TMainForm.InsertCircleExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertCircle, InsertCircleBtn);
end;

procedure TMainForm.InsertEllipseExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertEllipse, InsertEllipseBtn);
end;

procedure TMainForm.InsertArcExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertArc, InsertArcBtn);
end;

procedure TMainForm.InsertSectorExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertSector, InsertSectorBtn);
end;

procedure TMainForm.InsertSegmentExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertSegment, InsertSegmentBtn);
end;

procedure TMainForm.InsertPolylineExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertPolyline, InsertPolylineBtn);
end;

procedure TMainForm.InsertPolygonExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertPolygon, InsertPolygonBtn);
end;

procedure TMainForm.InsertCurveExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertCurve, InsertCurveBtn);
end;

procedure TMainForm.InsertClosedCurveExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertClosedCurve,
    InsertClosedCurveBtn);
end;

procedure TMainForm.InsertBezierPathExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertBezier, InsertBezierPathBtn);
end;

procedure TMainForm.InsertClosedBezierPathExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertClosedBezier,
    InsertClosedBezierPathBtn);
end;

procedure TMainForm.InsertTextExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertText, InsertTextBtn);
end;

procedure TMainForm.InsertStarExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertStar, InsertStarBtn);
end;

procedure TMainForm.InsertSymbolExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertSymbol, InsertSymbolBtn);
end;

procedure TMainForm.InsertBitmapExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_InsertBitmap, InsertBitmapBtn);
end;

procedure TMainForm.FreehandPolylineExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_FreehandPolyline,
    FreehandPolylineBtn);
end;

procedure TMainForm.FreehandBezierExecute(Sender: TObject);
begin
  EventManager.SendMessage(Msg_Escape, Self);
  EventManager.SendMessage(Msg_FreehandBezier,
    FreehandBezierBtn);
end;

procedure TMainForm.PressModeButton(
  Btn: TObject; Pressed: Boolean);
begin
  if not (Btn is TToolButton) then Exit;
  (Btn as TToolButton).Down := Pressed;
  (Btn as TToolButton).Marked := Pressed;
  if Pressed then
    (Btn as TToolButton).Style := tbsCheck
  else
    (Btn as TToolButton).Style := tbsButton;
end;

procedure TMainForm.TeXFormatExecute(Sender: TObject);
var
  P: TPoint;
begin
  if not (Sender is TControl) then Sender := Panel1;
  P := (Sender as TControl).ClientToScreen(
    Point((Sender as TControl).Left, (Sender as TControl).Top));
  PopupMenuDVI.Popup(P.X, P.Y);
end;

procedure TMainForm.PdfTeXFormatExecute(Sender: TObject);
var
  P: TPoint;
begin
  if not (Sender is TControl) then Sender := Panel1;
  P := (Sender as TControl).ClientToScreen(
    Point((Sender as TControl).Left, (Sender as TControl).Top));
  PopupMenuPdf.Popup(P.X, P.Y);
end;


procedure TMainForm.ToolButton16Click(Sender: TObject);
var
  I, J: Integer;
  GP: TGenericPath;
  PP: TPointsSet2D;
  P: TPoint2D;
  StartTime: Cardinal;
  procedure StartTimer;
  begin
    StartTime := GetTickCount;
  end;
  function GetTimer: Extended;
  begin
    GetTimer := (GetTickCount - StartTime) / 1000;
  //(Time - StartTime) * 24 * 60 * 60;
  end;
begin
  StartTimer;
  for J := 1 to 10000 do
  begin
    PP := TPointsSet2D.Create(0);
    GP := TGenericPath.Create(PP);
    for I := 1 to 1000 do
    begin
      GP.AddMoveTo(P);
      GP.AddBezierTo(P, P, P);
    end;
    PP.Free;
    GP.Free;
  end;
{  for J := 1 to 10000 do
  begin
    PP := TPointsSet2D.Create(0);
    for I := 1 to 6000 do
    begin
      PP.Add(P);
    end;
    PP.Free;
  end;   }
  MessageBoxInfo(Format('%8.3f', [GetTimer]));
end;

procedure TMainForm.ShowCrossHairExecute(Sender: TObject);
begin
  ShowCrossHair.Checked := not ShowCrossHair.Checked;
  LocalView.ShowCrossHair := ShowCrossHair.Checked;
end;

procedure TMainForm.GridOnTopExecute(Sender: TObject);
begin
  GridOnTop.Checked := not GridOnTop.Checked;
  LocalView.GridOnTop := GridOnTop.Checked;
end;

procedure TMainForm.ComboBox10DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if not (Control is TComboBox) then Exit;
  with (Control as TComboBox).Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(Rect);
    PropertiesForm.StarsImageList.Draw(
      (Control as TComboBox).Canvas,
      Rect.Left, Rect.Top + 1, Index);
  end;
end;

initialization
{$IFDEF FPC}
{$I MainUnit.lrs}
  DecimalSeparator := '.';
{$ELSE}
{$R *.dfm}
{$ENDIF}
end.

