unit MainUnit;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Buttons,
  Forms, Menus, ComCtrls, Clipbrd, Dialogs,
  StdCtrls, ExtCtrls, ImgList, ActnList, ToolWin, ExtDlgs,
  Drawings, ViewPort, GObjBase, GObjects, Manage, Options0,
{$IFNDEF FPC}
  Windows, HH, hh_funcs, System.ImageList, System.Actions,
{$ELSE}
  LCLIntf, LCLType, LMessages, LResources,
{$ENDIF}
  Devices, Modes, PlatformShortcuts
{$IFDEF FPC}
  , LazFileUtils, DocumentFormats, FileWatch, AutoReload
{$IFNDEF CPUWASM32}
  , SyncObjs, AutoSaveRuntime, AutoSaveStore, AutoSavePreferences
{$ENDIF}
{$ENDIF}
  ;

type

{$IFDEF FPC}
  TExternalReloadCommittedEvent = procedure(Sender: TObject;
    LocalRevision, WatchGeneration: QWord) of object;
  TExternalReloadConflictEvent = procedure(Sender: TObject;
    const Snapshot: TDocumentSnapshot) of object;
  TExternalReloadKeepLocalEvent = procedure(Sender: TObject;
    LocalRevision, WatchGeneration: QWord) of object;
  TLocalEditCommittedEvent = procedure(Sender: TObject;
    LocalRevision, WatchGeneration: QWord; Dirty: Boolean) of object;
  TRecoveryRestoreCommittedEvent = procedure(Sender: TObject;
    LocalRevision, WatchGeneration: QWord) of object;
{$ENDIF}

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
    LiveTeXPreview: TAction;
    TrustTeXPreview: TAction;
    LiveTeXPreviewItem: TMenuItem;
    SnapToGrid: TAction;
    SnapToShapes: TAction;
    SnapToShapesMenu: TMenuItem;
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
    procedure LiveTeXPreviewExecute(Sender: TObject);
    procedure TrustTeXPreviewExecute(Sender: TObject);
    procedure LiveTeXChanged(Sender: TObject);
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
    procedure UpdatePlatformShortcuts;
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
    procedure NumericPropertyExit(Sender: TObject);
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
{$IFDEF FPC}
    InitialViewQueued: Boolean;
{$IFNDEF CPUWASM32}
    FFileChangeSource: TFileChangeSource;
    FWatchDispatcher: TThread;
    FWatchEventQueue: TObject;
    FReloadCoordinator: TReloadCoordinator;
    FReloadTimer: TTimer;
    FReloadMenu: TMenuItem;
    FAutoRefreshMenu: TMenuItem;
    FPauseRefreshMenu: TMenuItem;
    FReloadDiskMenu: TMenuItem;
    FConflictMenu: TMenuItem;
    FKeepLocalMenu: TMenuItem;
    FSaveCopyMenu: TMenuItem;
    FCancelConflictMenu: TMenuItem;
    FReloadStatusPanel: TStatusPanel;
    FWatchSubscriptionID: QWord;
    FPendingSubscriptionID: QWord;
    FDeferredReload: Boolean;
    FDocumentPaused: Boolean;
    FPointerDown: Boolean;
    FInteractionEditNotified: Boolean;
    FInteractionHistoryCount: Integer;
    FInteractionStartRevision: QWord;
    FModalDepth: Integer;
    FClosingReload: Boolean;
    FConsistencyRetryCount: Integer;
    FConflictSnapshot: TDocumentSnapshot;
    FLastConflictRevisionKey: string;
    FLastReloadErrorText: string;
    FExplicitReloadRequested: Boolean;
    FExplicitReloadInProgress: Boolean;
    FExplicitDiscardArmed: Boolean;
    FConfirmedLocalRevision: QWord;
    FOnExternalReloadCommitted: TExternalReloadCommittedEvent;
    FOnExternalReloadConflict: TExternalReloadConflictEvent;
    FOnExternalReloadKeepLocal: TExternalReloadKeepLocalEvent;
    FOnLocalEditCommitted: TLocalEditCommittedEvent;
    FOnRecoveryRestoreCommitted: TRecoveryRestoreCommittedEvent;
    FSaveRuntime: TAutoSaveRuntime;
    FAutoSaveTimer: TTimer;
    FAutoSaveMenu: TMenuItem;
    FCrashRecoveryMenu: TMenuItem;
    FRecoverDraftsMenu: TMenuItem;
    FAutoSaveCheck: TCheckBox;
    FAutoSaveStatusPanel: TStatusPanel;
    FRecoveryOfferQueued: Boolean;
    FRecoveryKey: string;
    FAutoSavePreferenceEnabled: Boolean;
{$ENDIF}
{$ENDIF}
    ScrollPos0: Integer;             
{$IFDEF FPC}
    procedure InitializeEmptyView(Data: PtrInt);
{$IFNDEF CPUWASM32}
    procedure InitializeAutoReload;
    procedure ShutdownAutoReload;
    procedure ProcessFileWatchEvents(Data: PtrInt);
    procedure ProcessReloadDeadline(Sender: TObject);
    procedure ProcessReloadAt(NowMS: QWord);
    procedure QueueFileWatchEvent(const Event: TFileChangeEvent);
    procedure HandleFileWatchEvent(const Event: TFileChangeEvent);
    procedure ScheduleConsistencyRetry;
    procedure ReadAndApplyReload(const Ticket: TReloadReadTicket);
    procedure UpdateReloadUi;
    procedure UpdateReloadStatus(const Text: string);
    procedure AutoRefreshClick(Sender: TObject);
    procedure PauseRefreshClick(Sender: TObject);
    procedure ReloadDiskClick(Sender: TObject);
    procedure KeepLocalClick(Sender: TObject);
    procedure SaveCopyClick(Sender: TObject);
    procedure CancelConflictClick(Sender: TObject);
    procedure ApplicationModalBegin(Sender: TObject);
    procedure ApplicationModalEnd(Sender: TObject);
    procedure TryApplyDeferredReload(Data: PtrInt);
    procedure ApplyDeferredReload;
    function SceneInteractionActive: Boolean;
    function CurrentDocumentDirty: Boolean;
    procedure ScheduleSourceReconciliation;
    procedure RestoreExistingWatch;
    procedure InitializeAutoSave;
    procedure ShutdownAutoSave;
    procedure ArmAutoSaveTimer;
    procedure ProcessAutoSaveDeadline(Sender: TObject);
    procedure UpdateAutoSaveUi;
    procedure AutoSaveClick(Sender: TObject);
    procedure CrashRecoveryClick(Sender: TObject);
    procedure RecoverDraftsClick(Sender: TObject);
    procedure OfferRecoveryDrafts(Data: PtrInt);
    procedure RestoreRecoveryDraft(const FileName: string);
    procedure NotifyAutoSaveConflict;
    procedure NotifyAutoSaveLocalEdit(LocalRevision,
      WatchGeneration: QWord; Dirty: Boolean);
{$ENDIF}
{$ENDIF}
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
    procedure FitPropertiesToolbars;
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
{$IFDEF FPC}
{$IFNDEF CPUWASM32}
    function BeginDocumentWatchBinding(const Path: TDocumentPath;
      out WatchGeneration, SubscriptionID: QWord): TFileWatchStatus;
    procedure AcceptDocumentWatchBinding(const Snapshot: TDocumentSnapshot;
      WatchGeneration, SubscriptionID: QWord);
    procedure CancelDocumentWatchBinding(WatchGeneration,
      SubscriptionID: QWord);
    procedure ClearDocumentWatchBinding;
    procedure AcceptSavedDocumentRevision;
    procedure NotifyDocumentEdited;
    procedure BeginSceneInteraction;
    procedure EndSceneInteraction;
    procedure ProcessPendingReload(NowMS: QWord);
    procedure ApplyAutoReloadSettings;
    procedure BindAutoSaveForCurrentDocument(const RecoveryKey: string = '');
    function CurrentAutoSaveEnabled: Boolean;
    function CurrentAutoSaveKey: string;
    function AutoSaveConflict: Boolean;
    procedure AcceptSaveAsAutoSavePreference(Enabled: Boolean;
      const PreviousKey: string);
    procedure ApplyAutoSavePreference(Value: Boolean);
    property AutoReloadState: TReloadCoordinator read FReloadCoordinator;
    property OnExternalReloadCommitted: TExternalReloadCommittedEvent
      read FOnExternalReloadCommitted write FOnExternalReloadCommitted;
    property OnExternalReloadConflict: TExternalReloadConflictEvent
      read FOnExternalReloadConflict write FOnExternalReloadConflict;
    property OnExternalReloadKeepLocal: TExternalReloadKeepLocalEvent
      read FOnExternalReloadKeepLocal write FOnExternalReloadKeepLocal;
    property OnLocalEditCommitted: TLocalEditCommittedEvent
      read FOnLocalEditCommitted write FOnLocalEditCommitted;
    property OnRecoveryRestoreCommitted: TRecoveryRestoreCommittedEvent
      read FOnRecoveryRestoreCommitted write FOnRecoveryRestoreCommitted;
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
    function IsShortcut(var Message: TLMKey): Boolean; override;
{$ENDIF}
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
{$IFDEF FPC}  LiveTeX,{$ENDIF}
  SysBasic, Modify, Propert
{$IFDEF FPC}
{$IFNDEF CPUWASM32}
  , DocumentIO, FileWatchNative
{$ENDIF}
{$ENDIF}
  ;

{$IFDEF FPC}
{$IFNDEF CPUWASM32}
type
  TQueuedFileChange = class
    Event: TFileChangeEvent;
  end;

  TWatchEventQueue = class
  private
    FLock: TCriticalSection;
    FItems: TList;
    FDispatchPending: Boolean;
    FOverflowPending: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Push(const Event: TFileChangeEvent): Boolean;
    function Pop(out Event: TFileChangeEvent): Boolean;
    procedure Clear;
  end;

  TFileWatchDispatcher = class(TThread)
  private
    FSource: TFileChangeSource;
    FOwner: TMainForm;
  protected
    procedure Execute; override;
  public
    constructor Create(Source: TFileChangeSource; Owner: TMainForm);
  end;

constructor TWatchEventQueue.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FItems := TList.Create;
end;

destructor TWatchEventQueue.Destroy;
begin
  Clear;
  FItems.Free;
  FLock.Free;
  inherited Destroy;
end;

function TWatchEventQueue.Push(const Event: TFileChangeEvent): Boolean;
var Item: TQueuedFileChange; I: Integer; Collapsed: TFileChangeEvent;
begin
  Result := False;
  FLock.Acquire;
  try
    if FOverflowPending then Exit;
    if FItems.Count >= FileWatchQueueCapacity then
    begin
      for I := FItems.Count - 1 downto 0 do TObject(FItems[I]).Free;
      FItems.Clear;
      Collapsed := Event;
      Collapsed.SubscriptionID := 0;
      Collapsed.Generation := 0;
      Collapsed.Path := '';
      Collapsed.Kind := fckRescanRequired;
      Collapsed.ErrorText := 'Reload event queue overflow; rescan active source';
      FOverflowPending := True;
      Item := TQueuedFileChange.Create;
      Item.Event := Collapsed;
      FItems.Add(Item);
    end
    else
    begin
      Item := TQueuedFileChange.Create;
      Item.Event := Event;
      FItems.Add(Item);
    end;
    if not FDispatchPending then
    begin
      FDispatchPending := True;
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

function TWatchEventQueue.Pop(out Event: TFileChangeEvent): Boolean;
var Item: TQueuedFileChange;
begin
  FLock.Acquire;
  try
    Result := FItems.Count > 0;
    if Result then
    begin
      Item := TQueuedFileChange(FItems[0]);
      FItems.Delete(0);
      Event := Item.Event;
      if Event.Kind = fckRescanRequired then FOverflowPending := False;
      Item.Free;
    end
    else FDispatchPending := False;
  finally
    FLock.Release;
  end;
end;

procedure TWatchEventQueue.Clear;
var I: Integer;
begin
  FLock.Acquire;
  try
    for I := FItems.Count - 1 downto 0 do TObject(FItems[I]).Free;
    FItems.Clear;
    FDispatchPending := False;
    FOverflowPending := False;
  finally
    FLock.Release;
  end;
end;

constructor TFileWatchDispatcher.Create(Source: TFileChangeSource;
  Owner: TMainForm);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSource := Source;
  FOwner := Owner;
end;

procedure TFileWatchDispatcher.Execute;
var Event: TFileChangeEvent;
begin
  while not Terminated do
  begin
    FSource.WaitForEvent(-1);
    if Terminated or (FSource.Status = fwsStopped) then Break;
    while FSource.TryDequeue(Event) do
      FOwner.QueueFileWatchEvent(Event);
  end;
end;
{$ENDIF}
{$ENDIF}

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
{$IFDEF FPC}{$IFDEF CPUWASM32}
var InfoMenu: TMenuItem;
{$ENDIF}{$ENDIF}
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
{$IFDEF FPC}
  InitializeLiveTeX(LiveTeXChanged);
{$IFNDEF CPUWASM32}
  InitializeAutoReload;
  InitializeAutoSave;
{$ELSE}
  InfoMenu := TMenuItem.Create(Self);
  InfoMenu.Caption := 'Persistent AutoSave and recovery unavailable in browser builds';
  InfoMenu.Enabled := False;
  InfoMenu.Hint := 'Browser downloads are temporary snapshots and do not track desktop files';
  File1.Add(InfoMenu);
{$ENDIF}
{$ENDIF}
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
  UpdatePlatformShortcuts;
  ScrollPos0 := -1;
  ShowScrollBars.Checked := True;
  ShowPropertiesToolbar1.Checked := True;
  ShowPropertiesToolbar2.Checked := True;
  LocalView.VisualRect := Rect2D(0, 0,
    LocalView.ClientWidth * 25.4 / Screen.PixelsPerInch,
    LocalView.ClientHeight * 25.4 / Screen.PixelsPerInch);
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
  SnapToShapes.Tag := Msg_SnapToShapes;
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
  ComboBox6.OnExit := NumericPropertyExit;
  Edit4.OnExit := NumericPropertyExit;
  ComboBox7.Tag := Byte(chpHJ) + 1;
  ComboBox10.Tag := Byte(chpSK) + 1;
  Edit5.Tag := Byte(chpSS) + 1;

{$IFDEF VER140}
{$ELSE}
  CopyPictureToClipboard.Visible := False;
  CaptureEMF.Visible := False;
  ImageTool.Visible := False;
  PreviewPNG.Visible := False;
  PreviewBMP.Visible := False;
  PreviewEMF.Visible := False;
  PreviewPNG.Enabled := False;
  PreviewBMP.Enabled := False;
  PreviewEMF.Enabled := False;
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

procedure TMainForm.LiveTeXPreviewExecute(Sender: TObject);
begin
{$IFDEF FPC}
  SetLiveTeXEnabled(not LiveTeXEnabled);
  LiveTeXPreview.Checked := LiveTeXEnabled;
  LocalView.Repaint;
{$ENDIF}
end;

procedure TMainForm.TrustTeXPreviewExecute(Sender: TObject);
begin
{$IFDEF FPC}
  SetLiveTeXDocumentTrusted(not LiveTeXDocumentTrusted);
  TrustTeXPreview.Checked := LiveTeXDocumentTrusted;
  LocalView.Repaint;
  LiveTeXChanged(Self);
{$ENDIF}
end;

procedure TMainForm.LiveTeXChanged(Sender: TObject);
var
  Damage: TRect;
begin
{$IFDEF FPC}
  TrustTeXPreview.Checked := LiveTeXDocumentTrusted;
  StatusBar1.Panels[1].Text := LiveTeXStatus;
  StatusBar1.Hint := LiveTeXStatus;
  StatusBar1.ShowHint := True;
  if TakeLiveTeXDamage(Damage) then LocalView.RepaintScreenRect(Damage)
{$IFDEF CPUWASM32}
  else LocalView.Repaint
{$ENDIF}
  ;
{$ENDIF}
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
  if LocalView.CanFocus and not LocalView.Focused then
    LocalView.SetFocus;
{$IFDEF FPC}
{$IFNDEF CPUWASM32}
  BeginSceneInteraction;
{$ENDIF}
{$ENDIF}
  EventManager.MouseDown(Sender, Button, Shift, X, Y);
end;

procedure TMainForm.LocalViewMouseUp2D(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState;
  WX, WY: TRealTypeX; X, Y: Integer);
begin
  EventManager.MouseUp(Sender, Button, Shift, X, Y);
{$IFDEF FPC}
{$IFNDEF CPUWASM32}
  EndSceneInteraction;
{$ENDIF}
{$ENDIF}
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

{$IFDEF FPC}
function TMainForm.IsShortcut(var Message: TLMKey): Boolean;
var
  Shortcut: TShortCut;
  EditableFocus: Boolean;
begin
  EditableFocus := ActiveControl is TCustomEdit;
  if not EditableFocus and (ActiveControl is TCustomComboBox) then
    EditableFocus := TCustomComboBox(ActiveControl).Style.HasEditBox;
  if EditableFocus then
  begin
    Shortcut := Menus.ShortCut(Message.CharCode,
      KeyDataToShiftState(Message.KeyData));
    if (Shortcut <> 0) and
      ((Shortcut = Undo.ShortCut) or
       (Shortcut = Redo.ShortCut) or
       (Shortcut = ClipboardCopy.ShortCut) or
       (Shortcut = ClipboardCut.ShortCut) or
       (Shortcut = ClipboardPaste.ShortCut) or
       (Shortcut = SelectAll.ShortCut) or
       (Shortcut = DeleteSelected.ShortCut)) then
      Exit(False);
  end;
  Result := inherited IsShortcut(Message);
end;
{$ENDIF}

procedure TMainForm.UpdatePlatformShortcuts;
begin
  Undo.ShortCut := StandardShortcut(Ord('Z'), []);
  Redo.ShortCut := StandardShortcut(Ord('Z'), [ssShift]);
  ClipboardCopy.ShortCut := StandardShortcut(Ord('C'), []);
  ClipboardCut.ShortCut := StandardShortcut(Ord('X'), []);
  ClipboardPaste.ShortCut := StandardShortcut(Ord('V'), []);
  SelectAll.ShortCut := StandardShortcut(Ord('A'), []);
  NewDoc.ShortCut := StandardShortcut(Ord('N'), []);
  OpenDoc.ShortCut := StandardShortcut(Ord('O'), []);
  SaveDoc.ShortCut := StandardShortcut(Ord('S'), []);
  SaveAs.ShortCut := StandardShortcut(Ord('S'), [ssShift]);
  Print.ShortCut := StandardShortcut(Ord('P'), []);
end;

procedure TMainForm.UserEventExecute(Sender: TObject);
begin
  if (Sender as TAction).Tag = Msg_Exit then
    Close
  else
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
{$IFDEF FPC}
{$IFNDEF CPUWASM32}
  ShutdownAutoSave;
  ShutdownAutoReload;
{$ENDIF}
  ShutdownLiveTeX;
{$ENDIF}        
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

{$IFDEF FPC}
{$IFNDEF CPUWASM32}
procedure TMainForm.InitializeAutoReload;
begin
  FReloadCoordinator := TReloadCoordinator.Create(100);
  FWatchEventQueue := TWatchEventQueue.Create;
  FFileChangeSource := CreateFileChangeSource;
  FReloadTimer := TTimer.Create(Self);
  FReloadTimer.Enabled := False;
  FReloadTimer.Interval := 100;
  FReloadTimer.OnTimer := ProcessReloadDeadline;
  FReloadMenu := TMenuItem.Create(Self);
  FReloadMenu.Caption := 'Automatic refresh';
  File1.Add(FReloadMenu);
  FAutoRefreshMenu := TMenuItem.Create(Self);
  FAutoRefreshMenu.Caption := 'Auto refresh';
  FAutoRefreshMenu.OnClick := AutoRefreshClick;
  FReloadMenu.Add(FAutoRefreshMenu);
  FPauseRefreshMenu := TMenuItem.Create(Self);
  FPauseRefreshMenu.Caption := 'Pause refresh for this document';
  FPauseRefreshMenu.OnClick := PauseRefreshClick;
  FReloadMenu.Add(FPauseRefreshMenu);
  FReloadDiskMenu := TMenuItem.Create(Self);
  FReloadDiskMenu.Caption := 'Reload from disk…';
  FReloadDiskMenu.OnClick := ReloadDiskClick;
  FReloadMenu.Add(FReloadDiskMenu);
  FConflictMenu := TMenuItem.Create(Self);
  FConflictMenu.Caption := 'External changes conflict with local edits';
  FConflictMenu.Enabled := False;
  FReloadMenu.Add(FConflictMenu);
  FKeepLocalMenu := TMenuItem.Create(Self);
  FKeepLocalMenu.Caption := 'Keep local edits';
  FKeepLocalMenu.OnClick := KeepLocalClick;
  FReloadMenu.Add(FKeepLocalMenu);
  FSaveCopyMenu := TMenuItem.Create(Self);
  FSaveCopyMenu.Caption := 'Save local copy…';
  FSaveCopyMenu.OnClick := SaveCopyClick;
  FReloadMenu.Add(FSaveCopyMenu);
  FCancelConflictMenu := TMenuItem.Create(Self);
  FCancelConflictMenu.Caption := 'Cancel';
  FCancelConflictMenu.OnClick := CancelConflictClick;
  FReloadMenu.Add(FCancelConflictMenu);
  FReloadStatusPanel := StatusBar1.Panels.Add;
  FReloadStatusPanel.Width := 230;
  FReloadCoordinator.SetWatchUnavailable(True);
  Application.AddOnModalBeginHandler(ApplicationModalBegin);
  Application.AddOnModalEndHandler(ApplicationModalEnd);
  FDocumentPaused := False;
  UpdateReloadUi;
end;

procedure TMainForm.InitializeAutoSave;
begin
  FSaveRuntime := TAutoSaveRuntime.Create(
    TDocumentPath(UTF8String(IncludeTrailingPathDelimiter(
      GetAppConfigDirUTF8(False)) + 'AutoSaveRecovery')));
  FAutoSaveTimer := TTimer.Create(Self);
  FAutoSaveTimer.Enabled := False;
  FAutoSaveTimer.OnTimer := ProcessAutoSaveDeadline;
  FAutoSaveMenu := TMenuItem.Create(Self);
  FAutoSaveMenu.Caption := 'AutoSave this document';
  FAutoSaveMenu.OnClick := AutoSaveClick;
  File1.Add(FAutoSaveMenu);
  FCrashRecoveryMenu := TMenuItem.Create(Self);
  FCrashRecoveryMenu.Caption := 'Crash recovery drafts';
  FCrashRecoveryMenu.OnClick := CrashRecoveryClick;
  File1.Add(FCrashRecoveryMenu);
  FRecoverDraftsMenu := TMenuItem.Create(Self);
  FRecoverDraftsMenu.Caption := 'Recover drafts...';
  FRecoverDraftsMenu.OnClick := RecoverDraftsClick;
  File1.Add(FRecoverDraftsMenu);
  FAutoSaveCheck := TCheckBox.Create(Self);
  FAutoSaveCheck.Parent := Panel30;
  FAutoSaveCheck.Caption := 'AutoSave';
  FAutoSaveCheck.Hint := 'Save this document after completed edits';
  FAutoSaveCheck.ShowHint := True;
  FAutoSaveCheck.AutoSize := True;
  FAutoSaveCheck.Anchors := [akTop, akRight];
  FAutoSaveCheck.Top := 3;
  FAutoSaveCheck.Left := Panel30.ClientWidth - FAutoSaveCheck.Width - 10;
  FAutoSaveCheck.OnClick := AutoSaveClick;
  FAutoSaveStatusPanel := StatusBar1.Panels.Add;
  FAutoSaveStatusPanel.Width := 360;
  UpdateAutoSaveUi;
end;

procedure TMainForm.ShutdownAutoSave;
begin
  if FAutoSaveTimer <> nil then FAutoSaveTimer.Enabled := False;
  if FSaveRuntime <> nil then FSaveRuntime.CloseDocument;
  FreeAndNil(FAutoSaveTimer);
  FreeAndNil(FSaveRuntime);
end;

procedure TMainForm.BindAutoSaveForCurrentDocument(
  const RecoveryKey: string);
var Session: TDocumentSession; Preference: Boolean;
begin
  if FSaveRuntime = nil then Exit;
  Session := EventManager.DocumentSession;
  ReadDocumentAutoSavePreference(DocumentAutoSavePreferences,
    Session.SourcePath, Preference);
  FAutoSavePreferenceEnabled := Preference;
  FSaveRuntime.BindDocument(Session, TheDrawing, Preference,
    CrashRecoveryEnabled, CurrentDocumentDirty, GetTickCount64, RecoveryKey);
  FRecoveryKey := FSaveRuntime.DocumentKey;
  ArmAutoSaveTimer;
  UpdateAutoSaveUi;
end;

function TMainForm.CurrentAutoSaveEnabled: Boolean;
begin
  Result := FAutoSavePreferenceEnabled;
end;

function TMainForm.CurrentAutoSaveKey: string;
begin
  if FSaveRuntime = nil then Result := ''
  else Result := FSaveRuntime.DocumentKey;
end;

function TMainForm.AutoSaveConflict: Boolean;
begin
  Result := (FSaveRuntime <> nil) and FSaveRuntime.Coordinator.Conflict;
end;

procedure TMainForm.AcceptSaveAsAutoSavePreference(Enabled: Boolean;
  const PreviousKey: string);
var NewKey: string;
begin
  if FSaveRuntime = nil then Exit;
  WriteDocumentAutoSavePreference(DocumentAutoSavePreferences,
    EventManager.DocumentSession.SourcePath, Enabled);
  SaveSettings;
  BindAutoSaveForCurrentDocument;
  NewKey := FSaveRuntime.DocumentKey;
  if (PreviousKey <> '') and (PreviousKey <> NewKey) then
    try
      FSaveRuntime.Store.DeleteDraft(PreviousKey);
    except
      on E: Exception do
        FSaveRuntime.SetStatus('Save As succeeded, but the old recovery draft remains: ' +
          E.Message);
    end;
  UpdateAutoSaveUi;
end;

procedure TMainForm.ApplyAutoSavePreference(Value: Boolean);
var Session: TDocumentSession;
begin
  if FSaveRuntime = nil then Exit;
  Session := EventManager.DocumentSession;
  if Value and not FSaveRuntime.Coordinator.CanSaveBack then
  begin
    FSaveRuntime.SetAutoSaveEnabled(True, GetTickCount64);
    UpdateAutoSaveUi;
    Exit;
  end;
  FAutoSavePreferenceEnabled := Value;
  if Session.SourcePath <> '' then
  begin
    WriteDocumentAutoSavePreference(DocumentAutoSavePreferences,
      Session.SourcePath, Value);
    SaveSettings;
  end;
  FSaveRuntime.SetAutoSaveEnabled(Value, GetTickCount64);
  ArmAutoSaveTimer;
  UpdateAutoSaveUi;
end;

procedure TMainForm.AutoSaveClick(Sender: TObject);
var Value: Boolean;
begin
  if Sender = FAutoSaveCheck then
    Value := FAutoSaveCheck.Checked
  else
    Value := not FAutoSavePreferenceEnabled;
  ApplyAutoSavePreference(Value);
end;

procedure TMainForm.CrashRecoveryClick(Sender: TObject);
begin
  CrashRecoveryEnabled := not CrashRecoveryEnabled;
  SaveSettings;
  if FSaveRuntime <> nil then
    FSaveRuntime.SetRecoveryEnabled(CrashRecoveryEnabled, GetTickCount64);
  ArmAutoSaveTimer;
  UpdateAutoSaveUi;
end;

procedure TMainForm.RecoverDraftsClick(Sender: TObject);
begin
  OfferRecoveryDrafts(1);
end;

procedure TMainForm.NotifyAutoSaveLocalEdit(LocalRevision,
  WatchGeneration: QWord; Dirty: Boolean);
begin
  if FSaveRuntime = nil then Exit;
  FSaveRuntime.RefreshCapability(EventManager.DocumentSession,
    TheDrawing, GetTickCount64);
  FSaveRuntime.NotifyLocalEdit(LocalRevision, GetTickCount64, Dirty);
  ArmAutoSaveTimer;
  UpdateAutoSaveUi;
end;

procedure TMainForm.NotifyAutoSaveConflict;
begin
  if FSaveRuntime = nil then Exit;
  FSaveRuntime.SetConflict;
  ArmAutoSaveTimer;
  UpdateAutoSaveUi;
end;

procedure TMainForm.ArmAutoSaveTimer;
var Delay: Integer;
begin
  if FAutoSaveTimer = nil then Exit;
  FAutoSaveTimer.Enabled := False;
  if (FSaveRuntime = nil) or FClosingReload then Exit;
  Delay := FSaveRuntime.NextDelayMS(GetTickCount64);
  if Delay < 0 then Exit;
  if Delay < 1 then Delay := 1;
  FAutoSaveTimer.Interval := Delay;
  FAutoSaveTimer.Enabled := True;
end;

procedure TMainForm.ProcessAutoSaveDeadline(Sender: TObject);
var SourceSaved: Boolean;
begin
  if FAutoSaveTimer <> nil then FAutoSaveTimer.Enabled := False;
  if (FSaveRuntime = nil) or FClosingReload then Exit;
  FSaveRuntime.ProcessDue(EventManager.DocumentSession, TheDrawing,
    GetTickCount64, SourceSaved);
  if SourceSaved then AcceptSavedDocumentRevision;
  if FSaveRuntime.Coordinator.Conflict then
    ScheduleSourceReconciliation;
  UpdateAutoSaveUi;
  ArmAutoSaveTimer;
end;

procedure TMainForm.UpdateAutoSaveUi;
var Enabled: Boolean; Text: string;
begin
  if FSaveRuntime = nil then Exit;
  FSaveRuntime.RefreshCapability(EventManager.DocumentSession,
    TheDrawing, GetTickCount64);
  Enabled := FSaveRuntime.Coordinator.CanSaveBack;
  if FAutoSaveMenu <> nil then
  begin
    FAutoSaveMenu.Checked := FAutoSavePreferenceEnabled;
    FAutoSaveMenu.Enabled := Enabled;
    if not Enabled then FAutoSaveMenu.Hint := FSaveRuntime.AvailabilityReason
    else FAutoSaveMenu.Hint := 'Save this document after completed edits';
  end;
  if FAutoSaveCheck <> nil then
  begin
    FAutoSaveCheck.Checked := FAutoSavePreferenceEnabled;
    FAutoSaveCheck.Enabled := Enabled;
    if not Enabled then FAutoSaveCheck.Hint := FSaveRuntime.AvailabilityReason
    else FAutoSaveCheck.Hint := 'Save this document after completed edits';
  end;
  if FCrashRecoveryMenu <> nil then
    FCrashRecoveryMenu.Checked := CrashRecoveryEnabled;
  if FAutoSaveStatusPanel <> nil then
  begin
    Text := FSaveRuntime.LastStatus;
    if (Text = '') and FSaveRuntime.Coordinator.Conflict then
      Text := 'AutoSave suspended by external conflict';
    if FSaveRuntime.RecoveryWarning <> '' then
    begin
      if Text <> '' then Text := Text + '; ';
      Text := Text + FSaveRuntime.RecoveryWarning;
    end;
    FAutoSaveStatusPanel.Text := Text;
  end;
end;

procedure TMainForm.OfferRecoveryDrafts(Data: PtrInt);
var
  Dialog: TForm;
  Prompt: TLabel;
  DraftList: TListBox;
  RecoverButton, CancelButton: TButton;
  Files, DisplayItems: TStringList;
  Entry: TAutoSaveStoredRecord;
  I: Integer;
  ItemCaption: string;
begin
  if FSaveRuntime = nil then Exit;
  Files := TStringList.Create;
  DisplayItems := TStringList.Create;
  try
    FSaveRuntime.Store.ListDrafts(Files);
    for I := Files.Count - 1 downto 0 do
    begin
      if not FSaveRuntime.Store.LoadRecord(Files[I], Entry) or
        (Entry.Kind <> asrDraft) or
        not ((Entry.PayloadFormatId = 'tpx') or
          (Entry.PayloadFormatId = 'tpx-assets')) then
      begin
        Files.Delete(I);
        Continue;
      end;
      if Entry.SourcePath = '' then
        ItemCaption := 'Untitled drawing'
      else
        ItemCaption := string(DocumentPathFileName(Entry.SourcePath));
      ItemCaption := ItemCaption + ' - recovery draft';
      DisplayItems.Insert(0, ItemCaption);
    end;
    if Files.Count = 0 then
    begin
      if Data <> 0 then ShowMessage('No recovery drafts are available.');
      Exit;
    end;
    Dialog := TForm.Create(Self);
    try
      Dialog.Caption := 'Recover a drawing';
      Dialog.Position := poScreenCenter;
      Dialog.BorderStyle := bsDialog;
      Dialog.ClientWidth := 560;
      Dialog.ClientHeight := 340;
      Prompt := TLabel.Create(Dialog);
      Prompt.Parent := Dialog;
      Prompt.Left := 12;
      Prompt.Top := 12;
      Prompt.Caption := 'Choose a local draft to restore. The disk source is left unchanged.';
      DraftList := TListBox.Create(Dialog);
      DraftList.Parent := Dialog;
      DraftList.Left := 12;
      DraftList.Top := 40;
      DraftList.Width := Dialog.ClientWidth - 24;
      DraftList.Height := Dialog.ClientHeight - 94;
      DraftList.Anchors := [akLeft, akTop, akRight, akBottom];
      DraftList.Items.Assign(DisplayItems);
      DraftList.ItemIndex := 0;
      RecoverButton := TButton.Create(Dialog);
      RecoverButton.Parent := Dialog;
      RecoverButton.Caption := 'Recover';
      RecoverButton.ModalResult := mrOK;
      RecoverButton.Default := True;
      RecoverButton.Left := Dialog.ClientWidth - 180;
      RecoverButton.Top := Dialog.ClientHeight - 42;
      RecoverButton.Anchors := [akRight, akBottom];
      CancelButton := TButton.Create(Dialog);
      CancelButton.Parent := Dialog;
      CancelButton.Caption := 'Cancel';
      CancelButton.ModalResult := mrCancel;
      CancelButton.Left := Dialog.ClientWidth - 90;
      CancelButton.Top := Dialog.ClientHeight - 42;
      CancelButton.Anchors := [akRight, akBottom];
      if Dialog.ShowModal = mrOK then
      begin
        I := DraftList.ItemIndex;
        if (I >= 0) and (I < Files.Count) then
        begin
          if CurrentDocumentDirty and
            (MessageDlg('Recovering this draft will replace the current drawing. Continue?',
              mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then Exit;
          RestoreRecoveryDraft(Files[I]);
        end;
      end;
    finally
      Dialog.Free;
    end;
  finally
    Files.Free;
    DisplayItems.Free;
  end;
end;

procedure TMainForm.RestoreRecoveryDraft(const FileName: string);
var
  Entry: TAutoSaveStoredRecord;
  Candidate: TDrawing2D;
  CandidateContext: TObject;
  Diagnostics: TStringList;
  CanSaveBack, SafeSourceSave: Boolean;
  WatchGeneration, SubscriptionID: QWord;
  WatchStarted, WatchAccepted, SourceDiverged: Boolean;
  Snapshot: TDocumentSnapshot;
  CurrentRevision: TDiskRevision;
  SourcePath, FormatId, CandidatePath: TDocumentPath;
  PayloadBytes: RawByteString;
  RecoveryAssets: TRecoveryAssetContext;
  UnresolvedLinks: LongWord;
  RecoveryNotice: string;
  ErrorText: string;
begin
  Candidate := nil;
  CandidateContext := nil;
  RecoveryAssets := nil;
  Diagnostics := TStringList.Create;
  WatchGeneration := 0;
  SubscriptionID := 0;
  WatchStarted := False;
  WatchAccepted := False;
  SourceDiverged := False;
  try
    if not FSaveRuntime.Store.LoadRecord(FileName, Entry) or
      (Entry.Kind <> asrDraft) or
      not ((Entry.PayloadFormatId = 'tpx') or
        (Entry.PayloadFormatId = 'tpx-assets')) then
      raise EReadError.Create('The selected recovery draft is invalid');
    if (Entry.SourcePath <> '') and
      (Entry.DocumentKey <> DocumentPreferenceKey(Entry.SourcePath)) then
      raise EReadError.Create('The recovery draft source identity is invalid');
    SourcePath := TDocumentPath(Entry.SourcePath);
    if SourcePath <> '' then
    begin
      try
        CurrentRevision := ReadDocumentRevision(SourcePath);
        SourceDiverged := not SameDiskRevision(CurrentRevision,
          Entry.BaseRevision);
      except
        SourceDiverged := True;
      end;
      CandidatePath := SourcePath;
    end
    else
      CandidatePath := 'recovered.tpx';
    PayloadBytes := Entry.Payload;
    UnresolvedLinks := 0;
    if Entry.PayloadFormatId = 'tpx-assets' then
    begin
      FSaveRuntime.UnpackTpXRecoveryPayload(Entry.Payload,
        FSaveRuntime.Store.Root, PayloadBytes,
        RecoveryAssets, UnresolvedLinks);
      CandidatePath := RecoveryAssets.CandidatePath;
    end;
    Candidate := LoadDocumentCandidate('tpx', CandidatePath,
      PayloadBytes, CanSaveBack, CandidateContext, Diagnostics);
    if Candidate = nil then
      raise EReadError.Create('The recovery draft could not be parsed');
    if RecoveryAssets <> nil then
      RecoveryAssets.AdoptDrawingFiles(Candidate);
    SafeSourceSave := (SourcePath <> '') and
      SameText(Entry.SourceFormatId, 'tpx') and CanSaveBack and
      CanSerializeTpXDocumentWithoutSidecars(Candidate);
    { Recovery contains the native model, not the imported format's parser
      context. Do not arm a source watcher unless the restored candidate can
      also be safely saved back to that same source. }
    if SafeSourceSave then
    begin
      BeginDocumentWatchBinding(SourcePath, WatchGeneration,
        SubscriptionID);
      WatchStarted := True;
    end;
    CommitDocumentCandidate(TheDrawing, Candidate);
    { A recovery draft may be untitled or lack a safe source writer. Retire
      the prior document's watcher only after the replacement scene commits;
      a failed parse/commit must leave the current binding intact. }
    if not WatchStarted then ClearDocumentWatchBinding;
    BeginLiveTeXDocument(False);
    FormatId := Entry.SourceFormatId;
    if FormatId = '' then FormatId := 'tpx';
    EventManager.DocumentSession.AcceptSourceNormalized(SourcePath,
      FormatId, Entry.BaseSourceBytes, Entry.BaseRevision,
      SafeSourceSave, RecoveryAssets);
    RecoveryAssets := nil;
    if SourcePath = '' then
      TheDrawing.SetFileNameKeepingBitmapParents(Drawing_NewFileName)
    else
      TheDrawing.SetFileNameKeepingBitmapParents(SourcePath);
    TheDrawing.History.MarkDirty;
    EventManager.DocumentSession.AdvanceLocalRevision;
    if WatchStarted then
    begin
      Snapshot.SourceBytes := Entry.BaseSourceBytes;
      Snapshot.Revision := Entry.BaseRevision;
      AcceptDocumentWatchBinding(Snapshot, WatchGeneration,
        SubscriptionID);
      WatchAccepted := True;
    end;
    BindAutoSaveForCurrentDocument(Entry.DocumentKey);
    if SourceDiverged then NotifyAutoSaveConflict;
    if UnresolvedLinks > 0 then
    begin
      RecoveryNotice := Format(
        'Recovery draft omits %d linked image(s)', [UnresolvedLinks]);
      FSaveRuntime.SetRecoveryWarning(RecoveryNotice);
      if SourceDiverged then
        FSaveRuntime.SetStatus(
          'AutoSave paused: external changes conflict with local edits');
    end;
    Caption := ExtractFileName(TheDrawing.FileName);
    if SourcePath = '' then Caption := Drawing_NewFileName;
    if Assigned(FOnRecoveryRestoreCommitted) then
      FOnRecoveryRestoreCommitted(Self,
        EventManager.DocumentSession.LocalRevision,
        EventManager.DocumentSession.WatchGeneration);
    TheDrawing.RepaintViewports;
    ArmAutoSaveTimer;
    UpdateAutoSaveUi;
  except
    on E: Exception do
    begin
      ErrorText := E.Message;
      if Diagnostics.Count > 0 then ErrorText := ErrorText + LineEnding + Diagnostics.Text;
      MessageDlg('Could not recover the selected draft: ' + ErrorText,
        mtError, [mbOK], 0);
    end;
  end;
  if WatchStarted and not WatchAccepted then
    CancelDocumentWatchBinding(WatchGeneration, SubscriptionID);
  Candidate.Free;
  CandidateContext.Free;
  RecoveryAssets.Free;
  Diagnostics.Free;
end;


procedure TMainForm.ShutdownAutoReload;
begin
  if FReloadTimer <> nil then FReloadTimer.Enabled := False;
  FClosingReload := True;
  if FReloadCoordinator <> nil then FReloadCoordinator.Close;
  if FFileChangeSource <> nil then
  begin
    if FPendingSubscriptionID <> 0 then
      FFileChangeSource.Unsubscribe(FPendingSubscriptionID);
    if FWatchSubscriptionID <> 0 then
      FFileChangeSource.Unsubscribe(FWatchSubscriptionID);
    FFileChangeSource.Stop;
  end;
  if FWatchDispatcher <> nil then
  begin
    FWatchDispatcher.Terminate;
    FWatchDispatcher.WaitFor;
    FreeAndNil(FWatchDispatcher);
  end;
  Application.RemoveAsyncCalls(Self);
  Application.RemoveOnModalBeginHandler(ApplicationModalBegin);
  Application.RemoveOnModalEndHandler(ApplicationModalEnd);
  if FWatchEventQueue <> nil then
  begin
    TWatchEventQueue(FWatchEventQueue).Clear;
    FreeAndNil(FWatchEventQueue);
  end;
  FreeAndNil(FReloadTimer);
  FreeAndNil(FFileChangeSource);
  FreeAndNil(FReloadCoordinator);
end;

procedure TMainForm.QueueFileWatchEvent(const Event: TFileChangeEvent);
begin
  if FClosingReload or (FWatchEventQueue = nil) then Exit;
  if TWatchEventQueue(FWatchEventQueue).Push(Event) then
    Application.QueueAsyncCall(ProcessFileWatchEvents, 0);
end;

procedure TMainForm.ProcessFileWatchEvents(Data: PtrInt);
var Event: TFileChangeEvent;
begin
  if FClosingReload or FExplicitReloadInProgress or
    (FWatchEventQueue = nil) then Exit;
  while TWatchEventQueue(FWatchEventQueue).Pop(Event) do
    HandleFileWatchEvent(Event);
end;

procedure TMainForm.HandleFileWatchEvent(const Event: TFileChangeEvent);
var Relevant: Boolean;
begin
  if FClosingReload or (FReloadCoordinator = nil) then Exit;
  Relevant := IsFileWatchEventRelevant(Event, FWatchSubscriptionID,
    EventManager.DocumentSession.WatchGeneration,
    EventManager.DocumentSession.SourcePath);
  if not Relevant then Exit;
  if Event.Kind = fckReady then
  begin
    FReloadCoordinator.SetWatchUnavailable(Event.Status <> fwsReady);
    UpdateReloadUi;
    Exit;
  end;
  if Event.Kind = fckBackendError then
  begin
    FReloadCoordinator.SetWatchUnavailable(True);
    UpdateReloadStatus('File watching unavailable: ' + Event.ErrorText);
    UpdateReloadUi;
    Exit;
  end;
  if FWatchSubscriptionID = 0 then Exit;
  FConsistencyRetryCount := 0;
  if FReloadCoordinator.NotifyFileEvent(
    EventManager.DocumentSession.WatchGeneration, GetTickCount64) then
  begin
    FReloadTimer.Interval := 100;
    FReloadTimer.Enabled := True;
  end;
  UpdateReloadUi;
end;

procedure TMainForm.ProcessReloadDeadline(Sender: TObject);
begin
  FReloadTimer.Enabled := False;
  ProcessReloadAt(GetTickCount64);
end;

procedure TMainForm.ProcessPendingReload(NowMS: QWord);
begin
  if (FReloadTimer <> nil) and FReloadTimer.Enabled then
    FReloadTimer.Enabled := False;
  ProcessFileWatchEvents(0);
  ProcessReloadAt(NowMS);
end;

procedure TMainForm.ApplyAutoReloadSettings;
begin
  if FReloadCoordinator = nil then Exit;
  FReloadCoordinator.Paused := not AutoRefreshEnabled or FDocumentPaused;
  UpdateReloadUi;
end;

procedure TMainForm.ProcessReloadAt(NowMS: QWord);
var Ticket: TReloadReadTicket;
begin
  if FExplicitReloadInProgress then Exit;
  if FClosingReload or (FReloadCoordinator = nil) then Exit;
  if SceneInteractionActive then
  begin
    FDeferredReload := True;
    Exit;
  end;
  FDeferredReload := False;
  if FExplicitReloadRequested then
  begin
    if FExplicitDiscardArmed and
      (EventManager.DocumentSession.LocalRevision <>
       FConfirmedLocalRevision) then
    begin
      FExplicitReloadRequested := False;
      FExplicitDiscardArmed := False;
      UpdateReloadStatus('Local edits changed; confirm Reload from disk again');
    end
    else if FReloadCoordinator.BeginExplicitReload(Ticket) then
    begin
      FExplicitReloadRequested := False;
      FExplicitDiscardArmed := False;
      if FReloadTimer <> nil then FReloadTimer.Enabled := False;
      FExplicitReloadInProgress := True;
      try
        ReadAndApplyReload(Ticket);
      finally
        FExplicitReloadInProgress := False;
      end;
      ProcessFileWatchEvents(0);
    end;
  end
  else if FReloadCoordinator.TakeSettledRead(NowMS, Ticket) then
    ReadAndApplyReload(Ticket);
  UpdateReloadUi;
end;

function TMainForm.BeginDocumentWatchBinding(const Path: TDocumentPath;
  out WatchGeneration, SubscriptionID: QWord): TFileWatchStatus;
var OldID: QWord; Ready: TFileWatchStatus;
begin
  WatchGeneration := EventManager.DocumentSession.NextWatchGeneration;
  SubscriptionID := 0;
  if (FFileChangeSource = nil) or (Path = '') then
  begin
    Result := fwsUnsupported;
    Exit;
  end;
  if FFileChangeSource.Status = fwsStopped then
  begin
    FFileChangeSource.Start;
    Ready := FFileChangeSource.WaitUntilReady(3000);
    FReloadCoordinator.SetWatchUnavailable(Ready <> fwsReady);
    if (Ready in [fwsReady, fwsDegraded]) and
      (FWatchDispatcher = nil) then
    begin
      FWatchDispatcher := TFileWatchDispatcher.Create(FFileChangeSource, Self);
      FWatchDispatcher.Start;
    end;
  end;
  OldID := FPendingSubscriptionID;
  if OldID <> 0 then FFileChangeSource.Unsubscribe(OldID);
  Result := FFileChangeSource.Subscribe(NormalizeDocumentPath(Path),
    WatchGeneration, SubscriptionID);
  if Result in [fwsReady, fwsDegraded] then
  begin
    FPendingSubscriptionID := SubscriptionID;
  end
  else
    FReloadCoordinator.SetWatchUnavailable(True);
end;

procedure TMainForm.AcceptDocumentWatchBinding(
  const Snapshot: TDocumentSnapshot; WatchGeneration,
  SubscriptionID: QWord);
begin
  if (WatchGeneration <> EventManager.DocumentSession.WatchGeneration) or
    FClosingReload then Exit;
  if FWatchSubscriptionID <> 0 then
    FFileChangeSource.Unsubscribe(FWatchSubscriptionID);
  FWatchSubscriptionID := SubscriptionID;
  FPendingSubscriptionID := 0;
  FDocumentPaused := False;
  if FReloadTimer <> nil then FReloadTimer.Enabled := False;
  FReloadCoordinator.Bind(WatchGeneration,
    EventManager.DocumentSession.LocalRevision,
    DiskRevisionKey(Snapshot.Revision), Snapshot.Revision.ContentDigest,
    CurrentDocumentDirty);
  FReloadCoordinator.Paused := not AutoRefreshEnabled;
  if not EventManager.DocumentSession.CanSaveBack then
  begin
    if (FWatchSubscriptionID <> 0) and (FFileChangeSource <> nil) then
      FFileChangeSource.Unsubscribe(FWatchSubscriptionID);
    FWatchSubscriptionID := 0;
  end;
  FReloadCoordinator.SetWatchUnavailable(
    not EventManager.DocumentSession.CanSaveBack or (SubscriptionID = 0) or
    (FFileChangeSource = nil) or
    (FFileChangeSource.Status <> fwsReady));
  FConsistencyRetryCount := 0;
  FLastConflictRevisionKey := '';
  FLastReloadErrorText := '';
  ScheduleSourceReconciliation;
  UpdateReloadUi;
end;

procedure TMainForm.CancelDocumentWatchBinding(WatchGeneration,
  SubscriptionID: QWord);
begin
  if (FFileChangeSource <> nil) and (SubscriptionID <> 0) then
    FFileChangeSource.Unsubscribe(SubscriptionID);
  if FPendingSubscriptionID = SubscriptionID then
  begin
    FPendingSubscriptionID := 0;
      end;
  if (EventManager.DocumentSession.SourcePath <> '') and
    (WatchGeneration = EventManager.DocumentSession.WatchGeneration) then
    RestoreExistingWatch;
end;

procedure TMainForm.ClearDocumentWatchBinding;
begin
  if FReloadTimer <> nil then FReloadTimer.Enabled := False;
  if FFileChangeSource <> nil then
  begin
    if FPendingSubscriptionID <> 0 then
      FFileChangeSource.Unsubscribe(FPendingSubscriptionID);
    if FWatchSubscriptionID <> 0 then
      FFileChangeSource.Unsubscribe(FWatchSubscriptionID);
  end;
  FPendingSubscriptionID := 0;
  FWatchSubscriptionID := 0;
  FDeferredReload := False;
  FExplicitReloadRequested := False;
  FExplicitDiscardArmed := False;
  FDocumentPaused := False;
  FLastConflictRevisionKey := '';
  FLastReloadErrorText := '';
  FConflictSnapshot.SourceBytes := '';
  if FReloadCoordinator <> nil then FReloadCoordinator.Close;
  if FSaveRuntime <> nil then FSaveRuntime.CloseDocument;
  FRecoveryKey := '';
  UpdateReloadUi;
end;

procedure TMainForm.RestoreExistingWatch;
var SubID: QWord; Status: TFileWatchStatus; Gen: QWord;
begin
  if (FFileChangeSource = nil) or
    (EventManager.DocumentSession.SourcePath = '') or
    not EventManager.DocumentSession.CanSaveBack then Exit;
  Gen := EventManager.DocumentSession.WatchGeneration;
  Status := FFileChangeSource.Subscribe(
    EventManager.DocumentSession.SourcePath, Gen, SubID);
  if FWatchSubscriptionID <> 0 then
    FFileChangeSource.Unsubscribe(FWatchSubscriptionID);
  FWatchSubscriptionID := SubID;
  FReloadCoordinator.Bind(Gen, EventManager.DocumentSession.LocalRevision,
    DiskRevisionKey(EventManager.DocumentSession.AcceptedRevision),
    EventManager.DocumentSession.AcceptedRevision.ContentDigest,
    CurrentDocumentDirty);
  FReloadCoordinator.Paused := not AutoRefreshEnabled or FDocumentPaused;
  FReloadCoordinator.SetWatchUnavailable(Status <> fwsReady);
  if FReloadTimer <> nil then FReloadTimer.Enabled := False;
  ScheduleSourceReconciliation;
  UpdateReloadUi;
end;

procedure TMainForm.AcceptSavedDocumentRevision;
begin
  if FSaveRuntime <> nil then
    FSaveRuntime.AcceptSavedRevision(
      EventManager.DocumentSession.WatchGeneration);
  if FReloadCoordinator <> nil then
  begin
    FReloadCoordinator.AcceptSavedRevision(
      EventManager.DocumentSession.WatchGeneration,
      EventManager.DocumentSession.LocalRevision,
      DiskRevisionKey(EventManager.DocumentSession.AcceptedRevision),
      EventManager.DocumentSession.AcceptedRevision.ContentDigest,
      CurrentDocumentDirty);
    FReloadCoordinator.Paused := not AutoRefreshEnabled or FDocumentPaused;
  end;
  ArmAutoSaveTimer;
  UpdateAutoSaveUi;
  UpdateReloadUi;
end;

function TMainForm.CurrentDocumentDirty: Boolean;
begin
  Result := Assigned(TheDrawing) and Assigned(TheDrawing.History) and
    TheDrawing.History.IsChanged;
end;

procedure TMainForm.ScheduleSourceReconciliation;
begin
  if (FReloadCoordinator = nil) or FReloadCoordinator.Paused or
    (EventManager.DocumentSession.SourcePath = '') or
    not EventManager.DocumentSession.CanSaveBack then Exit;
  if FReloadCoordinator.NotifyFileEvent(
    EventManager.DocumentSession.WatchGeneration, GetTickCount64) then
  begin
    FReloadTimer.Interval := 100;
    FReloadTimer.Enabled := True;
  end;
end;

function TMainForm.SceneInteractionActive: Boolean;
begin
  Result := FPointerDown or (FModalDepth > 0);
end;

procedure TMainForm.NotifyDocumentEdited;
var Session: TDocumentSession;
begin
  Session := EventManager.DocumentSession;
  if FPointerDown then
  begin
    FInteractionEditNotified := True;
    Exit;
  end;
  if (FReloadCoordinator = nil) or
    (Session.LocalRevision = FReloadCoordinator.LocalRevision) then
    Session.AdvanceLocalRevision;
  if FReloadCoordinator <> nil then
  begin
    FReloadCoordinator.SetLocalState(Session.LocalRevision,
      CurrentDocumentDirty);
    if Assigned(FOnLocalEditCommitted) then
      FOnLocalEditCommitted(Self, Session.LocalRevision,
        Session.WatchGeneration, CurrentDocumentDirty);
  end;
  NotifyAutoSaveLocalEdit(Session.LocalRevision, Session.WatchGeneration,
    CurrentDocumentDirty);
end;

procedure TMainForm.BeginSceneInteraction;
begin
  if FPointerDown then Exit;
  if FSaveRuntime <> nil then FSaveRuntime.Coordinator.BeginInteraction;
  FPointerDown := True;
  FInteractionEditNotified := False;
  FInteractionHistoryCount := TheDrawing.History.Count;
  FInteractionStartRevision := EventManager.DocumentSession.LocalRevision;
end;

procedure TMainForm.EndSceneInteraction;
var Session: TDocumentSession; Changed: Boolean;
begin
  if not FPointerDown then Exit;
  Changed := FInteractionEditNotified or
    (TheDrawing.History.Count <> FInteractionHistoryCount);
  FPointerDown := False;
  FInteractionEditNotified := False;
  if Changed then
  begin
    Session := EventManager.DocumentSession;
    if Session.LocalRevision = FInteractionStartRevision then
      Session.AdvanceLocalRevision;
    if FReloadCoordinator <> nil then
    begin
      FReloadCoordinator.SetLocalState(Session.LocalRevision,
        CurrentDocumentDirty);
      if Assigned(FOnLocalEditCommitted) then
        FOnLocalEditCommitted(Self, Session.LocalRevision,
          Session.WatchGeneration, CurrentDocumentDirty);
    end;
    NotifyAutoSaveLocalEdit(Session.LocalRevision, Session.WatchGeneration,
      CurrentDocumentDirty);
  end;
  if FSaveRuntime <> nil then
  begin
    FSaveRuntime.Coordinator.EndInteraction(GetTickCount64);
    ArmAutoSaveTimer;
    UpdateAutoSaveUi;
  end;
  if FDeferredReload then
    Application.QueueAsyncCall(TryApplyDeferredReload, 0);
end;

procedure TMainForm.ApplicationModalBegin(Sender: TObject);
begin
  Inc(FModalDepth);
  if FSaveRuntime <> nil then FSaveRuntime.Coordinator.BeginInteraction;
end;

procedure TMainForm.ApplicationModalEnd(Sender: TObject);
begin
  if FModalDepth > 0 then Dec(FModalDepth);
  if FSaveRuntime <> nil then
  begin
    FSaveRuntime.Coordinator.EndInteraction(GetTickCount64);
    ArmAutoSaveTimer;
  end;
  if (FModalDepth = 0) and FDeferredReload then
    Application.QueueAsyncCall(TryApplyDeferredReload, 0);
end;

procedure TMainForm.TryApplyDeferredReload(Data: PtrInt);
begin
  ApplyDeferredReload;
end;

procedure TMainForm.ApplyDeferredReload;
begin
  if FDeferredReload and not SceneInteractionActive and
    (FModalDepth = 0) then
    ProcessReloadAt(GetTickCount64);
end;

procedure TMainForm.AutoRefreshClick(Sender: TObject);
begin
  AutoRefreshEnabled := not AutoRefreshEnabled;
  SaveSettings;
  FReloadCoordinator.Paused := not AutoRefreshEnabled or FDocumentPaused;
  if AutoRefreshEnabled and (FWatchSubscriptionID <> 0) then
    if FReloadCoordinator.NotifyFileEvent(
      EventManager.DocumentSession.WatchGeneration, GetTickCount64) then
    begin
      FReloadTimer.Interval := 100;
      FReloadTimer.Enabled := True;
    end;
  UpdateReloadUi;
end;

procedure TMainForm.PauseRefreshClick(Sender: TObject);
begin
  FDocumentPaused := not FDocumentPaused;
  FReloadCoordinator.Paused := not AutoRefreshEnabled or FDocumentPaused;
  if not FReloadCoordinator.Paused and (FWatchSubscriptionID <> 0) then
    if FReloadCoordinator.NotifyFileEvent(
      EventManager.DocumentSession.WatchGeneration, GetTickCount64) then
    begin
      FReloadTimer.Interval := 100;
      FReloadTimer.Enabled := True;
    end;
  UpdateReloadUi;
end;

procedure TMainForm.ReloadDiskClick(Sender: TObject);
var Session: TDocumentSession; Answer: Integer;
begin
  Session := EventManager.DocumentSession;
  if Session.SourcePath = '' then Exit;
  if CurrentDocumentDirty then
  begin
    Answer := MessageDlg('Reloading from disk will discard local edits. ' +
      'Continue?', mtConfirmation, [mbYes, mbNo], 0);
    if Answer <> mrYes then Exit;
    FExplicitDiscardArmed := True;
    FConfirmedLocalRevision := Session.LocalRevision;
  end
  else
    FExplicitDiscardArmed := False;
  FExplicitReloadRequested := True;
  ProcessReloadAt(GetTickCount64);
end;

procedure TMainForm.KeepLocalClick(Sender: TObject);
var Session: TDocumentSession;
begin
  Session := EventManager.DocumentSession;
  FReloadCoordinator.KeepLocal;
  NotifyAutoSaveConflict;
  if Assigned(FOnExternalReloadKeepLocal) then
    FOnExternalReloadKeepLocal(Self, Session.LocalRevision,
      Session.WatchGeneration);
  UpdateReloadUi;
end;

procedure TMainForm.SaveCopyClick(Sender: TObject);
begin
  EventManager.SendMessage(Msg_SaveAs, Sender);
end;

procedure TMainForm.CancelConflictClick(Sender: TObject);
begin
  { Closing the menu leaves both versions and the conflict notice intact. }
end;

procedure TMainForm.UpdateReloadStatus(const Text: string);
begin
  if FReloadStatusPanel = nil then Exit;
  FReloadStatusPanel.Text := Text;
  if (FReloadCoordinator <> nil) and FReloadCoordinator.InvalidExternal and
    (FLastReloadErrorText <> '') then
  begin
    FReloadDiskMenu.Hint := FLastReloadErrorText;
    StatusBar1.Hint := FLastReloadErrorText;
    StatusBar1.ShowHint := True;
  end
  else
  begin
    FReloadDiskMenu.Hint := Text;
    StatusBar1.Hint := Text;
    StatusBar1.ShowHint := Text <> '';
  end;
end;

procedure TMainForm.UpdateReloadUi;
var Session: TDocumentSession; Text: string;
begin
  if (FReloadCoordinator = nil) or (FAutoRefreshMenu = nil) then Exit;
  Session := EventManager.DocumentSession;
  FAutoRefreshMenu.Checked := AutoRefreshEnabled;
  FPauseRefreshMenu.Enabled := Session.SourcePath <> '';
  FPauseRefreshMenu.Checked := FDocumentPaused;
  FReloadDiskMenu.Enabled := Session.SourcePath <> '';
  FConflictMenu.Visible := FReloadCoordinator.Conflict;
  FKeepLocalMenu.Visible := FReloadCoordinator.Conflict;
  FSaveCopyMenu.Visible := FReloadCoordinator.Conflict;
  FCancelConflictMenu.Visible := FReloadCoordinator.Conflict;
  if FReloadCoordinator.Missing then
    Text := 'Source file missing'
  else if FReloadCoordinator.InvalidExternal then
    Text := 'External source is invalid; last valid drawing retained'
  else if FReloadCoordinator.Conflict then
    Text := 'External changes conflict with local edits'
  else if FReloadCoordinator.WatchUnavailable then
    Text := 'File watching unavailable; use Reload from disk'
  else if FDocumentPaused then
    Text := 'Automatic refresh paused for this document'
  else if not AutoRefreshEnabled then
    Text := 'Automatic refresh disabled'
  else
    Text := '';
  UpdateReloadStatus(Text);
end;

procedure TMainForm.ScheduleConsistencyRetry;
begin
  Inc(FConsistencyRetryCount);
  if FConsistencyRetryCount > 1 then
  begin
    UpdateReloadStatus('Source is changing; waiting for another file event');
    Exit;
  end;
  if FReloadCoordinator.NotifyFileEvent(
    EventManager.DocumentSession.WatchGeneration, GetTickCount64) then
  begin
    FReloadTimer.Interval := 100;
    FReloadTimer.Enabled := True;
  end;
end;

procedure TMainForm.ReadAndApplyReload(const Ticket: TReloadReadTicket);
var
  Session: TDocumentSession;
  Snapshot: TDocumentSnapshot;
  DiskRevision: TDiskRevision;
  Candidate: TDrawing2D;
  CandidateCanSaveBack, ErrorAgain, Exists, CommitPrepared: Boolean;
  CandidateContext: TObject;
  Diagnostics: TStringList;
  Decision: TReloadDecision;
  ErrorText, DiskKey: string;
  FinalRevision: TDiskRevision;
  OldCanSaveBack, HadFocus: Boolean;
  OldViewRect: TRect2D;
  SelectedIDs: array of Integer;
  SelectedClasses: array of TClass;
  Obj: TGraphicObject;
  I, N: Integer;
  SelectedObj: TGraphicObject;
begin
  Session := EventManager.DocumentSession;
  Candidate := nil;
  CandidateContext := nil;
  CommitPrepared := False;
  Diagnostics := TStringList.Create;
  try
    Exists := DocumentFileExists(Session.SourcePath);
    if not Exists then
    begin
      DiskRevision.ContentDigest := '';
      DiskRevision.Size := 0;
      DiskRevision.ModifiedUTC := 0;
      DiskRevision.Identity := '';
      DiskRevision.Exists := False;
      FReloadCoordinator.CompleteRead(Ticket, DiskRevisionKey(DiskRevision), '',
        False, True, Session.LocalRevision, CurrentDocumentDirty,
        Decision, ErrorAgain);
      UpdateReloadUi;
      Exit;
    end;
    try
      Snapshot := ReadDocumentSnapshot(Session.SourcePath);
    except
      on E: Exception do
      begin
        ErrorText := E.Message;
        if Pos('changed while it was being read', ErrorText) > 0 then
        begin
          FReloadCoordinator.CompleteRead(Ticket,
            FReloadCoordinator.BaseRevision, FReloadCoordinator.BaseContent,
            True, True, Session.LocalRevision, CurrentDocumentDirty,
            Decision, ErrorAgain);
          ScheduleConsistencyRetry;
          Exit;
        end;
        DiskKey := 'io:' + E.ClassName + ':' + ErrorText;
        FReloadCoordinator.CompleteRead(Ticket, DiskKey, '', True, False,
          Session.LocalRevision, CurrentDocumentDirty, Decision, ErrorAgain);
        if ErrorAgain then
          FLastReloadErrorText := 'Could not read external source ' +
            Session.SourcePath + LineEnding + ErrorText;
        UpdateReloadUi;
        Exit;
      end;
    end;
    DiskKey := DiskRevisionKey(Snapshot.Revision);
    if not Ticket.ExplicitReload and
      (Snapshot.Revision.ContentDigest = FReloadCoordinator.BaseContent) then
    begin
      FReloadCoordinator.CompleteRead(Ticket, DiskKey,
        Snapshot.Revision.ContentDigest, True, True,
        Session.LocalRevision, CurrentDocumentDirty, Decision, ErrorAgain);
      if Decision = rdNoChange then
      begin
        OldCanSaveBack := Session.CanSaveBack;
        Session.AcceptSavedRevision(Snapshot.SourceBytes,
          Snapshot.Revision, Session.CodecContext,
          Session.RecoveryBackupPath);
        Session.CanSaveBack := OldCanSaveBack;
        FLastConflictRevisionKey := '';
      end;
      UpdateReloadUi;
      Exit;
    end;
    try
      Candidate := LoadDocumentCandidate(Session.SourceFormatId,
        Session.SourcePath, Snapshot.SourceBytes, CandidateCanSaveBack,
        CandidateContext, Diagnostics);
    except
      on E: Exception do
      begin
        ErrorText := E.Message;
        if Diagnostics.Count > 0 then
          ErrorText := ErrorText + LineEnding + Diagnostics.Text;
        FReloadCoordinator.CompleteRead(Ticket, DiskKey,
          Snapshot.Revision.ContentDigest, True, False,
          Session.LocalRevision, CurrentDocumentDirty, Decision, ErrorAgain);
        if ErrorAgain then
          FLastReloadErrorText := 'Could not reload external source ' +
            Session.SourcePath + LineEnding + ErrorText;
        UpdateReloadUi;
        Exit;
      end;
    end;
    try
      FinalRevision := ReadDocumentRevision(Session.SourcePath);
    except
      on E: Exception do
      begin
        FReloadCoordinator.CompleteRead(Ticket,
          FReloadCoordinator.BaseRevision, FReloadCoordinator.BaseContent,
          True, True, Session.LocalRevision, CurrentDocumentDirty,
          Decision, ErrorAgain);
        ScheduleConsistencyRetry;
        Exit;
      end;
    end;
    if not SameDiskRevision(Snapshot.Revision, FinalRevision) then
    begin
      FReloadCoordinator.CompleteRead(Ticket,
        FReloadCoordinator.BaseRevision, FReloadCoordinator.BaseContent,
        True, True, Session.LocalRevision, CurrentDocumentDirty,
        Decision, ErrorAgain);
      ScheduleConsistencyRetry;
      Exit;
    end;
    if not CandidateCanSaveBack then
    begin
      FReloadCoordinator.CompleteRead(Ticket, DiskKey,
        Snapshot.Revision.ContentDigest, True, False,
        Session.LocalRevision, CurrentDocumentDirty, Decision, ErrorAgain);
      if ErrorAgain then
      begin
        ErrorText := 'The external source can no longer be safely saved back';
        if Diagnostics.Count > 0 then
          ErrorText := ErrorText + LineEnding + Diagnostics.Text;
        FLastReloadErrorText := 'Could not reload external source ' +
          Session.SourcePath + LineEnding + ErrorText;
      end;
      UpdateReloadUi;
      Exit;
    end;
    FReloadCoordinator.CompleteRead(Ticket, DiskKey,
      Snapshot.Revision.ContentDigest, True, True, Session.LocalRevision,
      CurrentDocumentDirty, Decision, ErrorAgain);
    if Decision <> rdInvalid then FLastReloadErrorText := '';
    case Decision of
      rdIgnore: Exit;
      rdNoChange:
        begin
          OldCanSaveBack := Session.CanSaveBack;
          Session.AcceptSavedRevision(Snapshot.SourceBytes,
            Snapshot.Revision, Session.CodecContext,
            Session.RecoveryBackupPath);
          Session.CanSaveBack := OldCanSaveBack;
          FLastConflictRevisionKey := '';
          UpdateReloadUi;
          Exit;
        end;
      rdMissing:
        begin
          UpdateReloadUi;
          Exit;
        end;
      rdInvalid:
        begin
          if ErrorAgain and (FLastReloadErrorText = '') then
            FLastReloadErrorText := 'External source is invalid: ' +
              Session.SourcePath + LineEnding + Diagnostics.Text;
          UpdateReloadUi;
          Exit;
        end;
      rdConflict:
        begin
          FConflictSnapshot := Snapshot;
          if FLastConflictRevisionKey <> DiskKey then
          begin
            FLastConflictRevisionKey := DiskKey;
            NotifyAutoSaveConflict;
            if Assigned(FOnExternalReloadConflict) then
              FOnExternalReloadConflict(Self, FConflictSnapshot);
          end;
          UpdateReloadUi;
          Exit;
        end;
      rdWatchUnavailable: begin UpdateReloadUi; Exit; end;
      rdApply: ;
    end;
    { The explicit user request already names this disk snapshot. A native
      watcher hint that arrives while parsing it is only a redundant rescan;
      consuming that hint here would invalidate the confirmed read ticket. }
    if not Ticket.ExplicitReload then
      ProcessFileWatchEvents(0);
    FReloadCoordinator.SetLocalState(Session.LocalRevision,
      CurrentDocumentDirty);
    if not FReloadCoordinator.PrepareReloadCommit(Ticket, DiskKey,
      Snapshot.Revision.ContentDigest, Session.LocalRevision + 1,
      Session.LocalRevision, CurrentDocumentDirty) then
    begin
      if CurrentDocumentDirty then
      begin
        FReloadCoordinator.KeepLocal;
        FConflictSnapshot := Snapshot;
        if FLastConflictRevisionKey <> DiskKey then
        begin
          FLastConflictRevisionKey := DiskKey;
          NotifyAutoSaveConflict;
          if Assigned(FOnExternalReloadConflict) then
            FOnExternalReloadConflict(Self, FConflictSnapshot);
        end;
      end;
      UpdateReloadUi;
      Exit;
    end;
    CommitPrepared := True;
    try
      try
        FinalRevision := ReadDocumentRevision(Session.SourcePath);
      except
        on E: Exception do
        begin
          ScheduleConsistencyRetry;
          Exit;
        end;
      end;
      if not SameDiskRevision(Snapshot.Revision, FinalRevision) then
      begin
        ScheduleConsistencyRetry;
        Exit;
      end;
      OldViewRect := LocalView.VisualRect;
      HadFocus := LocalView.Focused;
      SetLength(SelectedIDs, TheDrawing.SelectedObjects.Count);
      SetLength(SelectedClasses, Length(SelectedIDs));
      N := 0;
      SelectedObj := TheDrawing.SelectedObjects.FirstObj;
      while (SelectedObj <> nil) and (N < Length(SelectedIDs)) do
      begin
        SelectedIDs[N] := SelectedObj.ID;
        SelectedClasses[N] := SelectedObj.ClassType;
        Inc(N);
        SelectedObj := TheDrawing.SelectedObjects.NextObj;
      end;
      SetLength(SelectedIDs, N);
      SetLength(SelectedClasses, N);
      CommitDocumentCandidate(TheDrawing, Candidate);
      FReloadCoordinator.CompleteReloadCommit;
      CommitPrepared := False;
      Session.AcceptExternalRevision(Snapshot.SourceBytes,
        Snapshot.Revision, CandidateContext);
      CandidateContext := nil;
      Session.CanSaveBack := CandidateCanSaveBack;
      BeginLiveTeXDocument(False);
      if FSaveRuntime <> nil then
        FSaveRuntime.NotifyReloadCommitted(Session.LocalRevision,
          Session.WatchGeneration);
      if Assigned(FOnExternalReloadCommitted) then
        FOnExternalReloadCommitted(Self, Session.LocalRevision,
          Session.WatchGeneration);
      TheDrawing.SelectionClear;
      for I := 0 to N - 1 do
      begin
        Obj := TheDrawing.GetObject(SelectedIDs[I]);
        if (Obj <> nil) and (Obj.ClassType = SelectedClasses[I]) then
          TheDrawing.SelectionAdd(Obj);
      end;
      LocalView.VisualRect := OldViewRect;
      Undo.Enabled := TheDrawing.History.CanUndo;
      Redo.Enabled := TheDrawing.History.CanRedo;
      TheDrawing.RepaintViewports;
      if HadFocus and LocalView.CanFocus then LocalView.SetFocus;
      FConflictSnapshot.SourceBytes := '';
      FLastConflictRevisionKey := '';
      UpdateReloadUi;
    finally
      if CommitPrepared then FReloadCoordinator.CancelReloadCommit;
    end;
  finally
    Candidate.Free;
    CandidateContext.Free;
    Diagnostics.Free;
  end;
end;

{$ENDIF}
{$ENDIF}

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
    TpXResourcePath('help/tpx_tpxabout_tpx_drawing_tool.htm')));
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

procedure TMainForm.FitPropertiesToolbars;
var
  Bitmap: TBitmap;
  I, J, RequiredWidth: Integer;
  Combo: TComboBox;
  Edit: TEdit;
begin
  Bitmap := TBitmap.Create;
  try
    for I := 0 to ComponentCount - 1 do begin
      if Components[I] is TComboBox then begin
        Combo := Components[I] as TComboBox;
        if not ((Combo.Parent = PropertiesToolbar1) or
          (Combo.Parent = PropertiesToolbar2)) then Continue;
        Bitmap.Canvas.Font.Assign(Combo.Font);
        RequiredWidth := Bitmap.Canvas.TextWidth(Combo.Text);
        for J := 0 to Combo.Items.Count - 1 do
          if Bitmap.Canvas.TextWidth(Combo.Items[J]) > RequiredWidth then
            RequiredWidth := Bitmap.Canvas.TextWidth(Combo.Items[J]);
        Inc(RequiredWidth, GetSystemMetrics(SM_CXVSCROLL) + 12);
        if (Combo = ComboBox3) or (Combo = ComboBox4) or
          (Combo = ComboBox5) then Inc(RequiredWidth, Combo.Height);
        if Combo.Width < RequiredWidth then Combo.Width := RequiredWidth;
      end
      else if Components[I] is TEdit then begin
        Edit := Components[I] as TEdit;
        if not ((Edit.Parent = PropertiesToolbar1) or
          (Edit.Parent = PropertiesToolbar2)) then Continue;
        Bitmap.Canvas.Font.Assign(Edit.Font);
        RequiredWidth := Bitmap.Canvas.TextWidth('-999.99') + 12;
        if Edit.Width < RequiredWidth then Edit.Width := RequiredWidth;
      end;
    end;
  finally
    Bitmap.Free;
  end;
end;

{$IFDEF FPC}
procedure TMainForm.InitializeEmptyView(Data: PtrInt);
begin
  if (TheDrawing.ObjectsCount = 0) and (LocalView.ClientWidth > 0) and
    (LocalView.ClientHeight > 0) then
    LocalView.VisualRect := Rect2D(0, 0,
      LocalView.ClientWidth * 25.4 / Screen.PixelsPerInch,
      LocalView.ClientHeight * 25.4 / Screen.PixelsPerInch);
end;
{$ENDIF}

procedure TMainForm.FormShow(Sender: TObject);
begin
{$IFDEF FPC}
  LiveTeXPreview.Checked := LiveTeXEnabled;
  TrustTeXPreview.Checked := LiveTeXDocumentTrusted;
{$ENDIF}
  ShowGrid.Checked := LocalView.ShowGrid;
  ShowCrossHair.Checked := LocalView.ShowCrossHair;
  GridOnTop.Checked := LocalView.GridOnTop;
  ShowRulers.Checked := LocalView.ShowRulers;
  AreaSelectInsideAction.Checked := AreaSelectInside;
  SnapToGrid.Checked := UseSnap;
  SnapToShapes.Checked := UseShapeSnap;
  AngularSnap.Checked := UseAngularSnap;
  Panel2.Visible := ShowRulers.Checked;
  Panel3.Visible := ShowRulers.Checked;
  HScrollBar.Visible := ShowScrollBars.Checked;
  VScrollBar.Visible := ShowScrollBars.Checked;
  Showscrollbars1.Checked := ShowScrollBars.Checked;
  FitPropertiesToolbars;
  Panel31.Realign;
{$IFDEF FPC}
  if not InitialViewQueued then begin
    InitialViewQueued := True;
    Application.QueueAsyncCall(InitializeEmptyView, 0);
  end;
{$IFNDEF CPUWASM32}
  if not FRecoveryOfferQueued then
  begin
    FRecoveryOfferQueued := True;
    Application.QueueAsyncCall(OfferRecoveryDrafts, 0);
  end;
{$ENDIF}
{$ENDIF}
  //Scalephysicalunits1.Checked := ScalePhysical.Checked;
end;

procedure TMainForm.LocalViewEndRedraw(Sender: TObject);
begin
  Ruler1.Invalidate;
  Ruler2.Invalidate;
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
    Item.Caption := TPrimitive2DClass(GOClass).GetName;
    Item.Enabled := CanConvertSelected(TheDrawing, TPrimitive2DClass(GOClass));
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
  Value: TRealType;
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
        if not TryDimension((Sender as TComboBox).Text, Value, True) then Exit;
        TheDrawing.New_LineWidth := Value;
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
    chpFH:
      begin
        if not TryDimension((Sender as TEdit).Text, Value, False) then Exit;
        TheDrawing.New_FontHeight := Value;
      end;
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

procedure TMainForm.NumericPropertyExit(Sender: TObject);
var
  Value: TRealType;
  Changed: TNotifyEvent;
begin
  if (Sender = ComboBox6) and not TryDimension(ComboBox6.Text, Value, True) then
  begin
    Changed := ComboBox6.OnChange;
    ComboBox6.OnChange := nil;
    try ComboBox6.Text := RealTypeToStr(TheDrawing.New_LineWidth);
    finally ComboBox6.OnChange := Changed end;
  end
  else if (Sender = Edit4) and not TryDimension(Edit4.Text, Value, False) then
  begin
    Changed := Edit4.OnChange;
    Edit4.OnChange := nil;
    try Edit4.Text := RealTypeToStr(TheDrawing.New_FontHeight);
    finally Edit4.OnChange := Changed end;
  end;
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
{$IFDEF FPC}
{$IFNDEF CPUWASM32}
  NotifyDocumentEdited;
{$ELSE}
  EventManager.DocumentSession.AdvanceLocalRevision;
{$ENDIF}
{$ELSE}
  EventManager.DocumentSession.AdvanceLocalRevision;
{$ENDIF}
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
{$IFDEF FPC}
{$IFNDEF CPUWASM32}
  NotifyDocumentEdited;
{$ELSE}
  EventManager.DocumentSession.AdvanceLocalRevision;
{$ENDIF}
{$ELSE}
  EventManager.DocumentSession.AdvanceLocalRevision;
{$ENDIF}
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
begin
  if not (csDestroying in ComponentState) then Close;
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

