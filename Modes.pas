unit Modes;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses SysUtils, StrUtils, Classes, Controls, ExtCtrls, Graphics,
  Manage,
  Forms, Dialogs, ExtDlgs, Registry, Math, Options0,
  Geometry, Drawings, ViewPort, GObjects,
{$IFDEF VER140}
  Windows
{$ELSE}
  LCLIntf, LCLType
{$ENDIF}
  ;

const
// A constant for picking objects - aperture in number of pixels
  PixelApertureCoefficient = 4;

type

  TTpXManager = class(TEventManager)
  protected
    fDrawing: TDrawing2D;
    fViewPort: TViewport2D;
    fPickFilter: TObject2DClass;
    function GetRecentShort(I: Integer): string;
  public
    RecentFiles: THistoryList;
    constructor Create(ADrawing: TDrawing2D; AViewPort:
      TViewport2D);
    destructor Destroy; override;
    {: This property may contains a class variable that is used to limit
       the picking to only the objects of that class.

       This property is used in <See Method=PickObject>,
       <See Method=PickListOfObjects> and
       <See Method=GroupObjects>.}
    property PickFilter: TObject2DClass read fPickFilter write
      fPickFilter;
    property RecentShort[I: Integer]: string read GetRecentShort;
  end;

  TOpenAnyPictureDialog = class(TOpenPictureDialog)
  protected
    procedure DoSelectionChange; override;
    procedure PreviewClick(Sender: TObject); override;
  end;

  TTpXMode = class(TMode)
  protected
    function GetManager: TTpXManager;
    function GetDrawing: TDrawing2D;
    function GetViewport: TViewport2D;
    procedure EditUndo;
    procedure EditRedo;
    procedure OnChangeDrawing(ADrawing: TDrawing);
    procedure OnPasteMetafileFromClipboard(Drawing: TDrawing2D);
    procedure ChangeObjectProperties(
      Obj: TObject2D);
    procedure ChangeSelectedProperties;
    procedure PictureProperties;
    procedure TpXSettings;
    procedure TransformSelected(
      const Transf: TTransf2D; const DoNotify: Boolean);
    procedure DoSaveDrawing(FileName: string);
    procedure DrawingSaveDlgTypeChange(Sender: TObject);
    function DlgSaveDrawing(FileName: string): Word;
    function TrySaveDrawing(const FileName: string): Word;
    procedure DoOpenDrawing(const FileName: string);
    procedure DlgOpenDrawing;
    procedure NewDrawing(const FileName: string);
    procedure StartProgram;
    procedure ShowPictureInfo;
    procedure ShiftSelected(Shift: TVector2D);
    procedure FlipSelected(DX, DY: TRealType);
    procedure RotateSelected(R: TRealType);
    procedure ScaleSelected(Scale: TRealType);
    procedure CustomTransform;
    procedure ScaleStandard;
    procedure Add_Point(P: TPoint2D);
    procedure Delete_Point(P: TPoint2D);
    procedure Break_Path(P: TPoint2D);
  public
    function AskSaveCurrentDrawing: Word;
    procedure OnMessage(Msg: TEventMessage; Sender: TObject);
      override;
    property TpX_Manager: TTpXManager read GetManager;
    property Drawing: TDrawing2D read GetDrawing;
    property ViewPort: TViewport2D read GetViewport;
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean); override;
  end;

  TBaseMode = class(TTpXMode)
  protected
    fLastPoint: TPoint2D;
    procedure OnInsertGrObj(Msg: TEventMessage; Sender: TObject);
  public
    constructor Create;
    procedure OnMessage(Msg: TEventMessage; Sender: TObject);
      override;
    procedure OnDblClick(Sender: TObject; X, Y: Integer); override;
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); override;
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure OnLocalPopup(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
  end;

  TInterruptableMode = class(TTpXMode)
  public
    PressedBtn: TObject;
    constructor Create;
    procedure OnPush; override;
    procedure OnPop; override;
  end;

  TOpenRecentMode = class(TTpXMode)
  public
    Index: Integer;
    procedure OnPush; override;
  end;

  TInsertGrObjMode = class(TInterruptableMode)
  public
    Obj: TObject2D;
    constructor Create;
    procedure OnPush; override;
    procedure OnPop; override;
  end;

  TInsertSimpleGrObjMode = class(TInsertGrObjMode)
  public
    constructor Create;
    procedure OnPush; override;
    procedure OnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TInsertUnsizedGrObjMode = class(TInsertGrObjMode)
  public
    procedure OnPush; override;
    procedure OnDblClick(Sender: TObject;
      X, Y: Integer); override;
    procedure OnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TInsertSizedGrObjMode = class(TInsertGrObjMode)
  public
    NPoints: Integer;
    procedure OnPush; override;
    procedure FinishDraw;
    procedure OnDblClick(Sender: TObject;
      X, Y: Integer); override;
    procedure PointSelected(P: TPoint2D);
    procedure OnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); override;
    procedure OnMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  end;

  TFreehandPolylineMode = class(TInsertGrObjMode)
  public
    procedure OnPush; override;
    procedure OnPop; override;
    procedure FinishDraw;
    procedure OnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); override;
    procedure OnMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  end;

  TSelectBoxMode = class(TInterruptableMode)
  protected
    fIsSelecting: Boolean;
  public
    Rect: TRectangle2D;
    constructor Create;
    destructor Destroy; override;
    procedure OnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); override;
    procedure OnMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  end;

  TSelectObjectsInAreaMode = class(TSelectBoxMode)
  protected
    procedure DoSelectObjectsInArea(Rect: TRect2D);
  public
    procedure OnPush; override;
    procedure OnPop; override;
    procedure OnMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  end;

  TZoomAreaMode = class(TSelectBoxMode)
  public
    procedure OnPush; override;
    procedure OnPop; override;
    procedure OnMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  end;

  TPanningMode = class(TInterruptableMode)
  protected
    fStartPoint: TPoint2D;
    fIsMoving: Boolean;
  public
    procedure OnPush; override;
    procedure OnPop; override;
    procedure OnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); override;
    procedure OnMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  end;

  TTransformingMode = class(TInterruptableMode)
  protected
    function GetTransform(P: TPoint2D): TTransf2D; virtual;
  public
    CurrTransf: TTransf2D;
    StartPoint: TPoint2D;
    procedure ConfirmTransform(X, Y: Integer); virtual;
    procedure OnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseMove(X, Y: Integer);
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); override;
    procedure OnMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure DrawRubber;
  end;

  TDragMode = class(TTransformingMode)
  end;

  TDragCopyMode = class(TTransformingMode)
  public
    procedure ConfirmTransform(X, Y: Integer); override;
    procedure OnPush; override;
    procedure OnPop; override;
  end;

  TMoveMode = class(TTransformingMode)
  public
    procedure OnPush; override;
  end;

  TRotateMode = class(TTransformingMode)
  protected
    fLastPoint: TPoint2D;
    BaseAngle: TRealType;
    function GetTransform(P: TPoint2D): TTransf2D; override;
  public
    procedure OnPush; override;
  end;

  TMoveControlPointMode = class(TTransformingMode)
  public
    CurrentPrimitive, OriginalPrimitive: TPrimitive2D;
    PointIndex: Integer;
    Shift: TShiftState;
    procedure OnPush; override;
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); override;
    procedure OnMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  end;

type

  TTpXExtAssocOption = class(TOptionData)
    fTpXExtAssoc: Boolean;
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
  end;

var
  TpXExtAssoc: TTpXExtAssocOption;
  OpenDialog_FilterIndex: Integer;

var
  BaseMode: TBaseMode;
  OpenRecentMode: TOpenRecentMode;
  InsertSimpleGrObjMode: TInsertSimpleGrObjMode;
  InsertUnsizedGrObjMode: TInsertUnsizedGrObjMode;
  InsertSizedGrObjMode: TInsertSizedGrObjMode;
  FreehandPolylineMode: TFreehandPolylineMode;
  SelectObjectsInAreaMode: TSelectObjectsInAreaMode;
  ZoomAreaMode: TZoomAreaMode;
  PanningMode: TPanningMode;
  DragMode: TDragMode;
  DragCopyMode: TDragCopyMode;
  MoveMode: TMoveMode;
  RotateMode: TRotateMode;
  MoveControlPointMode: TMoveControlPointMode;

const
  Msg_StartProgram = Msg_User;
  Msg_Escape = Msg_StartProgram + 1;
  Msg_New = Msg_Escape + 1;
  Msg_NewWindow = Msg_New + 1;
  Msg_Open = Msg_NewWindow + 1;
  Msg_Save = Msg_Open + 1;
  Msg_Print = Msg_Save + 1;
  Msg_SaveAs = Msg_Print + 1;
  Msg_CopyAsEMF = Msg_SaveAs + 1;
  Msg_TpXSettings = Msg_CopyAsEMF + 1;
  Msg_Undo = Msg_TpXSettings + 1;
  Msg_Redo = Msg_Undo + 1;
  Msg_Copy = Msg_Redo + 1;
  Msg_Paste = Msg_Copy + 1;
  Msg_Cut = Msg_Paste + 1;
  Msg_Delete = Msg_Cut + 1;
  Msg_Duplicate = Msg_Delete + 1;
  Msg_SelectAll = Msg_Duplicate + 1;
  Msg_SelNext = Msg_SelectAll + 1;
  Msg_SelPrev = Msg_SelNext + 1;
  Msg_SnapToGrid = Msg_SelPrev + 1;
  Msg_AngularSnap = Msg_SnapToGrid + 1;
  Msg_SmoothBezierNodes = Msg_AngularSnap + 1;
  Msg_AreaSelect = Msg_SmoothBezierNodes + 1;
  Msg_AreaSelectInside = Msg_AreaSelect + 1;
  Msg_SelectedProperties = Msg_AreaSelectInside + 1;
  Msg_PictureProperties = Msg_SelectedProperties + 1;
  Msg_ConvertTo = Msg_PictureProperties + 1;
  Msg_InsertLine = Msg_ConvertTo + 20;
  Msg_InsertRectangle = Msg_InsertLine + 1;
  Msg_InsertCircle = Msg_InsertRectangle + 1;
  Msg_InsertEllipse = Msg_InsertCircle + 1;
  Msg_InsertArc = Msg_InsertEllipse + 1;
  Msg_InsertSector = Msg_InsertArc + 1;
  Msg_InsertSegment = Msg_InsertSector + 1;
  Msg_InsertPolyline = Msg_InsertSegment + 1;
  Msg_InsertPolygon = Msg_InsertPolyline + 1;
  Msg_InsertCurve = Msg_InsertPolygon + 1;
  Msg_InsertClosedCurve = Msg_InsertCurve + 1;
  Msg_InsertBezier = Msg_InsertClosedCurve + 1;
  Msg_InsertClosedBezier = Msg_InsertBezier + 1;
  Msg_InsertText = Msg_InsertClosedBezier + 1;
  Msg_InsertStar = Msg_InsertText + 1;
  Msg_InsertSymbol = Msg_InsertStar + 1;
  Msg_FreehandPolyline = Msg_InsertSymbol + 1;

  Msg_SimplifyPoly = Msg_FreehandPolyline + 1;
  Msg_ConnectPaths = Msg_SimplifyPoly + 1;
  Msg_ReversePoints = Msg_ConnectPaths + 1;
  Msg_Group = Msg_ReversePoints + 1;
  Msg_Unroup = Msg_Group + 1;
  Msg_BreakPath = Msg_Unroup + 1;
  Msg_DeletePoint = Msg_BreakPath + 1;
  Msg_AddPoint = Msg_DeletePoint + 1;

  Msg_MoveUp = Msg_AddPoint + 1;
  Msg_MoveDown = Msg_MoveUp + 1;
  Msg_MoveLeft = Msg_MoveDown + 1;
  Msg_MoveRight = Msg_MoveLeft + 1;
  Msg_MoveUpPixel = Msg_MoveRight + 1;
  Msg_MoveDownPixel = Msg_MoveUpPixel + 1;
  Msg_MoveLeftPixel = Msg_MoveDownPixel + 1;
  Msg_MoveRightPixel = Msg_MoveLeftPixel + 1;
  Msg_FlipV = Msg_MoveRightPixel + 1;
  Msg_FlipH = Msg_FlipV + 1;
  Msg_RotateCounterclockW = Msg_FlipH + 1;
  Msg_RotateClockW = Msg_RotateCounterclockW + 1;
  Msg_RotateCounterclockWDegree = Msg_RotateClockW + 1;
  Msg_RotateClockWDegree
    = Msg_RotateCounterclockWDegree + 1;
  Msg_Grow10 = Msg_RotateClockWDegree + 1;
  Msg_Shrink10 = Msg_Grow10 + 1;
  Msg_Grow1 = Msg_Shrink10 + 1;
  Msg_Shrink1 = Msg_Grow1 + 1;
  Msg_StartRotate = Msg_Shrink1 + 1;
  Msg_StartMove = Msg_StartRotate + 1;
  Msg_ScaleStandard = Msg_StartMove + 1;
  Msg_CustomTransform = Msg_ScaleStandard + 1;
  Msg_ConvertToGrayScale = Msg_CustomTransform + 1;

  Msg_MoveForward = Msg_ConvertToGrayScale + 1;
  Msg_MoveBackward = Msg_MoveForward + 1;
  Msg_MoveToFront = Msg_MoveBackward + 1;
  Msg_MoveToBack = Msg_MoveToFront + 1;

  Msg_AlignLeft = Msg_MoveToBack + 1;
  Msg_AlignHCenter = Msg_AlignLeft + 1;
  Msg_AlignRight = Msg_AlignHCenter + 1;
  Msg_AlignBottom = Msg_AlignRight + 1;
  Msg_AlignVCenter = Msg_AlignBottom + 1;
  Msg_AlignTop = Msg_AlignVCenter + 1;

  Msg_PreviewLaTeX = Msg_AlignTop + 1;
  Msg_PreviewPdfLaTeX = Msg_PreviewLaTeX + 1;
  Msg_PreviewLaTeX_PS = Msg_PreviewPdfLaTeX + 1;
  Msg_PreviewSVG = Msg_PreviewLaTeX_PS + 1;
  Msg_PreviewEMF = Msg_PreviewSVG + 1;
  Msg_PreviewEPS = Msg_PreviewEMF + 1;
  Msg_PreviewPNG = Msg_PreviewEPS + 1;
  Msg_PreviewBMP = Msg_PreviewPNG + 1;
  Msg_PreviewPDF = Msg_PreviewBMP + 1;
  Msg_DrawingSource = Msg_PreviewPDF + 1;
  Msg_preview_tex_inc = Msg_DrawingSource + 1;
  Msg_metapost_tex_inc = Msg_preview_tex_inc + 1;
  Msg_CaptureEMF = Msg_metapost_tex_inc + 1;
  Msg_ImageTool = Msg_CaptureEMF + 1;
  Msg_ZoomArea = Msg_ImageTool + 1;
  Msg_ZoomIn = Msg_ZoomArea + 1;
  Msg_ZoomOut = Msg_ZoomIn + 1;
  Msg_ZoomAll = Msg_ZoomOut + 1;
  Msg_Panning = Msg_ZoomAll + 1;
  Msg_Help = Msg_Panning + 1;
  Msg_PictureInfo = Msg_Help + 1;
  Msg_About = Msg_PictureInfo + 1;

const Drag_Aperture = 4;

implementation

uses MainUnit, SysBasic, Propert, Options, TransForm, PreView,
{$IFDEF VER140}
  EMF_Unit, WinBasic, ClpbrdOp
{$ELSE}
  LazBasic
{$ENDIF}
  , Settings0, AboutUnit, Input, Output, ScaleStandardUnit, Modify;

{ --================ TTpXManager ==================-- }

constructor TTpXManager.Create(
  ADrawing: TDrawing2D; AViewPort: TViewport2D);
begin
  inherited Create;
  fDrawing := ADrawing;
  fViewPort := AViewPort;
  fPickFilter := TObject2D;
  RecentFiles := THistoryList.Create;
  ADrawing.OnChangeDrawing := BaseMode.OnChangeDrawing;
  ADrawing.OnPasteMetafileFromClipboard :=
    BaseMode.OnPasteMetafileFromClipboard;
end;

destructor TTpXManager.Destroy;
begin
  RecentFiles.Free;
  inherited Destroy;
end;

function TTpXManager.GetRecentShort(I: Integer): string;
// Short name for recent file list
const MaxLen = 45;
begin
  Result := RecentFiles[I];
  if Length(Result) <= MaxLen then Exit;
  begin
    Delete(Result, 1, Length(Result) - MaxLen + 1);
    Result := '..' + Result;
  end;
end;

{ --================ TOpenAnyPictureDialog ==================-- }

procedure TOpenAnyPictureDialog.DoSelectionChange;
var
  Ext: string;
  ValidPicture: Boolean;
  BMP: Graphics.TBitmap;
  function ValidFile(const FileName: string): Boolean;
  begin
{$IFDEF VER140}
    Result := GetFileAttributes(PChar(FileName)) <> $FFFFFFFF;
{$ELSE}
    Result := True;
{$ENDIF}
  end;
  procedure SetCaption(St: string);
  begin
{$IFDEF VER140}
    PictureLabel.Caption := St;
{$ENDIF}
  end;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Delete(Ext, 1, 1);
  ValidPicture := FileExists(FileName) and ValidFile(FileName)
    and ((Ext = 'emf') or (Ext = 'wmf')
    or (Ext = 'eps') or (Ext = 'ps') or (Ext = 'pdf'));
  if ValidPicture then
  try
    if (Ext = 'emf') or (Ext = 'wmf') then
    begin
      ImageCtrl.Picture.LoadFromFile(FileName);
      ImageCtrl.Stretch := True;
      SetCaption(Format({SPictureDesc}'%d x %d',
        [ImageCtrl.Picture.Width, ImageCtrl.Picture.Height]));
    end
    else if (Ext = 'eps') or (Ext = 'ps') or (Ext = 'pdf') then
    begin
      BMP := PreView.GetPostScriptPreview(FileName);
      if BMP <> nil then
      begin
        ImageCtrl.Picture.Assign(BMP);
        ImageCtrl.Stretch := False;
        SetCaption(Format({SPictureDesc}'%d x %d',
          [ImageCtrl.Picture.Width, ImageCtrl.Picture.Height]));
      end
      else
      begin
        SetCaption('*no preview*');
        ImageCtrl.Picture := nil;
      end;
    end;
    //FPreviewButton.Enabled := True;
    //FPaintPanel.Caption := '';
  except
    ValidPicture := False;
  end;
  if not ValidPicture then
  begin
    SetCaption({SPictureLabel}'*no preview*');
    //FPreviewButton.Enabled := False;
    ImageCtrl.Picture := nil;
    //FPaintPanel.Caption := srNone;
  end;
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
  //inherited DoSelectionChange;
end;

procedure TOpenAnyPictureDialog.PreviewClick(Sender: TObject);
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Delete(Ext, 1, 1);
  if ((Ext = 'emf') or (Ext = 'wmf')) then
    inherited PreviewClick(Sender)
  else if ((Ext = 'eps') or (Ext = 'ps')) then
    OpenOrExec(PSViewerPath, FileName)
  else
    OpenOrExec('', FileName);
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
    else
      R.DeleteKey(Ext);
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
  if fTpXExtAssoc then
    Result := '1'
  else
    Result := '0';
end;

{* --------- TTpXMode --------- *}

function TTpXMode.GetManager: TTpXManager;
begin
  Result := Manager as TTpXManager;
end;

function TTpXMode.GetDrawing: TDrawing2D;
begin
  Result := TpX_Manager.fDrawing;
end;

function TTpXMode.GetViewport: TViewport2D;
begin
  Result := TpX_Manager.fViewPort;
end;

procedure TTpXMode.OnMouseWheel(Sender: TObject; Shift:
  TShiftState;
  WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  FX, FY: TRealType;
  MousePosClient: TPoint;
{$IFDEF VER140}
{$ELSE}
const
  WHEEL_DELTA = 120;
{$ENDIF}
begin
  Handled := True;
  Shift := Shift - [ssMiddle];
  if Shift = [] then
    ViewPort.PanWindowFraction(0, -WheelDelta / WHEEL_DELTA * 0.05)
  else if Shift = [SSShift] then
    ViewPort.PanWindowFraction(WheelDelta / WHEEL_DELTA * 0.05, 0)
  else if Shift = [SSAlt] then
    ViewPort.ZoomViewCenter(Exp(-WheelDelta / WHEEL_DELTA * Ln(2) /
      8))
  else if Shift = [SSCtrl] then
    if (ViewPort.Width > 0) and (ViewPort.Height > 0) then
    begin
      MousePosClient := ViewPort.ScreenToClient(MousePos);
      FX := MousePosClient.X / ViewPort.Width;
      FY := 1 - MousePosClient.Y / ViewPort.Height;
      ViewPort.ZoomFrac(FX, FY,
        Exp(-WheelDelta / WHEEL_DELTA * Ln(2) / 8));
    end;
end;

procedure TTpXMode.EditUndo;
begin
  with Drawing do
  begin
    History.Undo;
    RepaintViewports;
    MainForm.Undo.Enabled := History.CanUndo;
    MainForm.Redo.Enabled := History.CanRedo;
    //SaveDoc.Enabled := History.IsChanged;
  end;
end;

procedure TTpXMode.EditRedo;
begin
  with Drawing do
  begin
    History.Redo;
    RepaintViewports;
    MainForm.Undo.Enabled := History.CanUndo;
    MainForm.Redo.Enabled := History.CanRedo;
    //SaveDoc.Enabled := History.IsChanged;
  end;
end;

procedure TTpXMode.OnChangeDrawing(ADrawing: TDrawing);
begin
  with ADrawing as TDrawing2D do
    if Assigned(Drawing.History) then
    begin
      History.Save;
      MainForm.Undo.Enabled := History.CanUndo;
      MainForm.Redo.Enabled := History.CanRedo;
      //SaveDoc.Enabled := History.IsChanged;
    end;
end;

procedure TTpXMode.OnPasteMetafileFromClipboard(Drawing:
  TDrawing2D);
begin
{$IFDEF VER140}
  PasteMetafileFromClipboard(Drawing);
{$ELSE}
{$ENDIF}
end;

procedure TTpXMode.ChangeObjectProperties(
  Obj: TObject2D);
begin
  PropertiesForm.PObject := Obj;
  if PropertiesForm.ShowModal = mrOK then
  begin
    Drawing.NotifyChanged;
    Obj.UpdateExtension(nil);
    Drawing.RepaintViewports
  end;
//  Parent.SetFocus;
//  SetFocus;
end;

procedure TTpXMode.ChangeSelectedProperties;
var
  Obj: TObject2D;
begin
  Obj := Drawing.SelectionFirst as TObject2D;
  if Assigned(Obj) then ChangeObjectProperties(Obj);
end;

procedure TTpXMode.PictureProperties;
begin
  OptionsForm.POptList := Drawing.OptionsList;
  if OptionsForm.ShowModal = mrOK then
  begin
    if OptionsForm.HasChanged then
    begin
      if Drawing.PicScale <= 0 then Drawing.PicScale := 1;
      Drawing.History.SetPropertiesChanged;
      //MainForm.SaveDoc.Enabled := TheDrawing.History.IsChanged;
    end;
    Drawing.RepaintViewports;
  end;
  //ViewPort.SetFocus;
end;

procedure TTpXMode.TpXSettings;
begin
  OptionsForm.POptList := SettingsList;
  if OptionsForm.ShowModal = mrOK then
    if OptionsForm.HasChanged then Drawing.RepaintViewports;
end;

procedure TTpXMode.TransformSelected(
  const Transf: TTransf2D; const DoNotify: Boolean);
var
  ListOfObj: array of Integer;
  Iter: TGraphicObjIterator;
  Current: TGraphicObject;
  I: Integer;
begin
    //SelectedObjs.Transform(Transf);
  SetLength(ListOfObj, Drawing.SelectedObjects.Count);
  Iter := Drawing.SelectedObjects.GetIterator;
  try
    Current := nil;
    I := 0;
    while Iter.GetNext(Current) do
    begin
      ListOfObj[I] := Current.ID;
      Inc(I);
    end;
  finally
    Iter.Free;
  end;
  Drawing.TransformObjects(ListOfObj, Transf, DoNotify);
end;

procedure TTpXMode.DlgOpenDrawing;
var
  OpenDialog: TOpenAnyPictureDialog;
  FileName: string;
begin
  OpenDialog := TOpenAnyPictureDialog.Create(MainForm);
  try
    OpenDialog.FilterIndex := OpenDialog_FilterIndex;
    if PsToEditPath <> '' then
      OpenDialog.Filter :=
        'All supported formats|*.TpX;*.emf;*.wmf;*.svg;*.svgz;*.eps;*.ps;*.pdf'
    else
      OpenDialog.Filter :=
        'All supported formats|*.TpX;*.emf;*.wmf;*.svg;*.svgz';
    OpenDialog.Filter := OpenDialog.Filter +
      '|TpX drawing|*.TpX' +
      '|Windows (Enhanced) Metafiles (*.emf,*.wmf)|*.emf;*.wmf' +
      '|Scalable Vector Graphics (*.svg;*.svgz)|*.svg;*.svgz';
    if PsToEditPath <> '' then
      OpenDialog.Filter := OpenDialog.Filter +
        '|Encapsulated Postscript (*.eps)|*.eps' +
        '|All Postscripts (*.eps, *.ps)|*.eps;*.ps' +
        '|Portable document format (*.pdf)|*.pdf|*.*|*.*';
    if not OpenDialog.Execute then Exit;
    if AskSaveCurrentDrawing = mrCancel then Exit;
    FileName := OpenDialog.FileName;
    OpenDialog_FilterIndex := OpenDialog.FilterIndex;
  finally
    OpenDialog.Free;
  end;
  DoOpenDrawing(FileName);
  //DoImportMetafile(EMFOpenDialog.FileName);
end;

procedure TTpXMode.DoSaveDrawing(FileName: string);
begin
  if not SameText(ExtractFileExt(FileName), '.TpX') then
    FileName := ChangeFileExt(FileName, '.TpX');
  StoreToFile_TpX(Drawing, FileName, False);
  Drawing.FileName := FileName;
  MainForm.Caption := ExtractFileName(Drawing.FileName);
  TpX_Manager.RecentFiles.Update(Drawing.FileName);
  Drawing.History.SaveCheckSum;
  //SaveDoc.Enabled := TheDrawing.History.IsChanged;
end;

procedure TTpXMode.DrawingSaveDlgTypeChange(Sender: TObject);
var
  List: TStringList;
begin
  List := TStringList.Create;
  ExtractStrings(['|'], [' ', '*', '.'],
    PChar(AnsiReplaceStr(MainForm.DrawingSaveDlg.Filter, '.*',
    '.qqq')), List);
  MainForm.DrawingSaveDlg.DefaultExt :=
    List[MainForm.DrawingSaveDlg.FilterIndex * 2 - 1];
  if MainForm.DrawingSaveDlg.DefaultExt = 'qqq'
    then MainForm.DrawingSaveDlg.DefaultExt := '';
  List.Free;
end;

function TTpXMode.DlgSaveDrawing(FileName: string): Word;
var
  Path: string;
  Filter: string;
  ExecuteResult: Boolean;
  Device, Ext, Ext2: string;
  //LaTeX custom (latex-dvips-gs) |LaTeX custom (latex-dvips-gs)|*.*
begin
  if FileName = Drawing_NewFileName then
    FileName := Drawing.FileName;
  if FileName = Drawing_NewFileName then FileName := '';
  MainForm.DrawingSaveDlg.FileName := ChangeFileExt(FileName, '');
  {**MainForm.DrawingSaveDlg.OnShow:=  DrawingSaveDlgTypeChange;
  MainForm.DrawingSaveDlg.OnTypeChange := DrawingSaveDlgTypeChange;}
  Path := ExtractFilePath(MainForm.DrawingSaveDlg.FileName);
  if Path = '' then Path := ExtractFilePath(Drawing.FileName);
  if Path <> '' then MainForm.DrawingSaveDlg.InitialDir := Path;
  Filter := MainForm.DrawingSaveDlg.Filter;
  LaTeX_Custom_Parse(Device, Ext, Ext2);
  if Ext <> '' then
  begin
    MainForm.DrawingSaveDlg.Filter
      := AnsiReplaceStr(MainForm.DrawingSaveDlg.Filter, '-gs',
      '-gs-' + Device);
    if Ext2 <> '' then Ext := Ext + ';*.' + Ext2;
    MainForm.DrawingSaveDlg.Filter
      := AnsiReplaceStr(MainForm.DrawingSaveDlg.Filter, '*.*', '*.'
      + Ext);
  end;
  ExecuteResult := MainForm.DrawingSaveDlg.Execute;
  MainForm.DrawingSaveDlg.Filter := Filter;
  if not ExecuteResult then
  begin
    Result := mrCancel;
    Exit;
  end;
  if (MainForm.DrawingSaveDlg.FilterIndex > 1) and
    (MainForm.DrawingSaveDlg.FilterIndex - 2 <=
    Ord(High(ExportFormatKind))) then
    ExportToFile(Drawing,
      MainForm.DrawingSaveDlg.FileName,
      ExportFormatKind(MainForm.DrawingSaveDlg.FilterIndex - 2))
  else
    DoSaveDrawing(MainForm.DrawingSaveDlg.FileName);
  Result := mrOK;
end;

function TTpXMode.TrySaveDrawing(const FileName: string): Word;
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

function TTpXMode.AskSaveCurrentDrawing: Word;
begin
  if (Drawing.ObjectsCount = 0)
    or not Drawing.History.IsChanged then
  begin
    Result := mrOK;
    Exit;
  end;
  Result := MessageDlg('Save current drawing?',
    mtWarning, [mbYes, mbNo, mbCancel], 0);
  if Result = mrYes then
    Result := TrySaveDrawing(Drawing.FileName);
end;

procedure TTpXMode.OnMessage(Msg: TEventMessage; Sender: TObject);
begin
  case Msg of
    Msg_SnapToGrid:
      begin
        Drawing.UseSnap := not Drawing.UseSnap;
        MainForm.SnapToGrid.Checked := Drawing.UseSnap;
      end;
    Msg_AngularSnap:
      begin
        Drawing.UseAngularSnap :=
          not Drawing.UseAngularSnap;
        MainForm.AngularSnap.Checked := Drawing.UseAngularSnap;
      end;
    Msg_SmoothBezierNodes:
      begin
        SmoothBezierNodes := not SmoothBezierNodes;
        MainForm.SmoothBezierNodesAction.Checked :=
          SmoothBezierNodes;
      end;
    Msg_ZoomIn: ViewPort.ZoomIn;
    Msg_ZoomOut: ViewPort.ZoomOut;
    Msg_ZoomAll: ViewPort.ZoomToExtension;
    Msg_Help: MainForm.ShowTpXHelp;
    Msg_PictureInfo: ShowPictureInfo;
    Msg_About: AboutForm.ShowModal;
  end;
  inherited OnMessage(Msg, Sender);
end;

procedure TTpXMode.DoOpenDrawing(const FileName: string);
var
  Loader: T_TpX_Loader;
  Ext: string;
begin
  try
    Ext := LowerCase(ExtractFileExt(FileName));
    if Ext <> '.tpx' then
    begin
      Drawing.Clear;
      ViewPort.BeginUpdate;
      MainForm.Enabled := False;
      try
        if (Ext = '.emf') or (Ext = '.wmf') then
          Import_Metafile(Drawing, FileName, nil)
        else if (Ext = '.eps') or (Ext = '.ps') or (Ext = '.pdf')
          then
        begin
          Import_Eps(Drawing, FileName);
        end
        else if (Ext = '.svg') or (Ext = '.svgz') then
        begin
          Import_SVG(Drawing, FileName);
        end;
        ViewPort.ZoomToExtension;
      finally
        MainForm.Enabled := True;
        ViewPort.EndUpdate;
      end;
      MainForm.Caption := Drawing.FileName;
      ViewPort.Repaint;
      Drawing.History.SaveCheckSum;
    //SaveDoc.Enabled := TheDrawing.History.IsChanged;
      Exit;
    end;
    Drawing.Clear;
    Loader := T_TpX_Loader.Create(Drawing);
    try
      Loader.LoadFromFile(FileName);
    finally
      Loader.Free;
    end;
    ViewPort.ZoomToExtension;
    ViewPort.Repaint;
    MainForm.Caption := ExtractFileName(Drawing.FileName);
    TpX_Manager.RecentFiles.Update(Drawing.FileName);
    Drawing.History.SaveCheckSum;
  //SaveDoc.Enabled := TheDrawing.History.IsChanged;
  except
    MessageBoxError('Can not open ' + FileName);
  end;
end;

procedure TTpXMode.NewDrawing(const FileName: string);
begin
  Drawing.Clear;
  Drawing.FileName := FileName;
  MainForm.Caption := FileName;
  if FileName <> Drawing_NewFileName then
    TpX_Manager.RecentFiles.Update(FileName);
  Drawing.History.SaveCheckSum;
  //SaveDoc.Enabled := TheDrawing.History.IsChanged;
end;

procedure TTpXMode.StartProgram;
var
  FileName, IncludePath, OutputFormats: string;
begin
  with Drawing do
  begin
    History := TDrawHistory.Create(Drawing);
    History.Save;
    MainForm.Undo.Enabled := History.CanUndo;
    MainForm.Redo.Enabled := History.CanRedo;
    //SaveDoc.Enabled := History.IsChanged;
  end;
  LoadSettings_Ex(MainForm);

  ParseParameters(FileName, IncludePath, OutputFormats);
  if FileName <> '' then
  begin
    //ShowMessage(FileName);
    if FileExists(FileName) then
      DoOpenDrawing(FileName)
    else
      NewDrawing(FileName);
    if IncludePath <> '' then
      Drawing.IncludePath := IncludePath;
    SetOutputFormats(OutputFormats, Drawing);
  end;
end;

procedure TTpXMode.ShowPictureInfo;
var
  List: TStringList;
  R: TRect2D;
{$IFDEF VER140}
var
  MS: TMEMORYSTATUS;
{$ENDIF}
begin
  List := TStringList.Create;
  R := Drawing.DrawingExtension;
  try
    List.Add('Picture bounds (left, bottom, right, top):');
    List.Add(Format('   %.5g, %.5g, %.5g, %.5g',
      [R.Left, R.Bottom, R.Right, R.Top]));
    List.Add(Format('Size: %.5g x %.5g (%.5gmm x %.5gmm)',
      [R.Right - R.Left, R.Top - R.Bottom,
      (R.Right - R.Left) * Drawing.PicScale,
        (R.Top - R.Bottom) * Drawing.PicScale]));
    List.Add(Format('Output size: %.5gmm x %.5gmm',
      [(R.Right - R.Left) * Drawing.PicScale
      * Drawing.PicMagnif,
        (R.Top - R.Bottom) * Drawing.PicScale
        * Drawing.PicMagnif]));
    List.Add('Picture bounds including border (left, bottom, right, top):');
    if Drawing.PicScale > 0 then
      List.Add(Format('   %.5g, %.5g, %.5g, %.5g',
        [R.Left - Drawing.Border / Drawing.PicScale,
        R.Bottom - Drawing.Border / Drawing.PicScale,
          R.Right + Drawing.Border / Drawing.PicScale,
          R.Top + Drawing.Border / Drawing.PicScale]));
    if Drawing.PicScale > 0 then
      List.Add(Format('Size including border: %.5g x %.5g (%.5gmm x %.5gmm)',
        [R.Right - R.Left + Drawing.Border * 2 / Drawing.PicScale,
        R.Top - R.Bottom + Drawing.Border * 2 / Drawing.PicScale,
          (R.Right - R.Left) * Drawing.PicScale + Drawing.Border *
          2,
          (R.Top - R.Bottom) * Drawing.PicScale + Drawing.Border *
          2]));
    List.Add(Format('Output size including border: %.5gmm x %.5gmm',
      [((R.Right - R.Left) * Drawing.PicScale + Drawing.Border * 2)
      * Drawing.PicMagnif,
        ((R.Top - R.Bottom) * Drawing.PicScale + Drawing.Border * 2)
        * Drawing.PicMagnif]));
{$IFDEF VER140}
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
{$ENDIF}
    MessageBoxInfo(List.GetText);
  finally
    List.Free;
  end;
end;

procedure TTpXMode.ShiftSelected(Shift: TVector2D);
  function RoundShift(Sh, Del: TRealType): TRealType;
  var
    A: TRealType;
    N: Integer;
  begin
    if Sh = 0 then
    begin
      Result := 0;
      Exit;
    end;
    Sh := Sh / Del;
    N := Floor(Log10(Abs(Sh) / 7)) + 1;
    A := Power(10, N);
    Result := Round(Sh / A) * Del * A;
  end;
begin
  if Drawing.UseSnap then
    if ViewPort.GridStep > 0 then
    begin
      Shift.X := RoundShift(Shift.X, ViewPort.GridStep);
      Shift.Y := RoundShift(Shift.Y, ViewPort.GridStep);
    end;
  if Drawing.UseAngularSnap then
    Make45_2D_V(Shift);
  TransformSelected(Translate2D(Shift.X, Shift.Y), True);
    //fGridDeltaX,       fGridDeltaY
end;

procedure TTpXMode.FlipSelected(DX, DY: TRealType);
begin
  TransformSelected(
    Flip2D(Drawing.GetSelectionCenter, V2D(DX, DY)), True);
end;

procedure TTpXMode.RotateSelected(R: TRealType);
begin
  TransformSelected(RotateCenter2D(R,
    Drawing.GetSelectionCenter), True);
end;

procedure TTpXMode.ScaleSelected(Scale: TRealType);
begin
  TransformSelected(ScaleCenter2D(Scale, Scale,
    Drawing.GetSelectionCenter), True);
end;

procedure TTpXMode.CustomTransform;
begin
  TransfForm.CP :=
    Drawing.GetSelectionCenter;
  TransfForm.R :=
    Drawing.GetSelectionExtension;
  if TransfForm.ShowModal = mrOK then
  begin
    TransformSelected(TransfForm.T, True);
  end;
end;

procedure TTpXMode.ScaleStandard;
var
  T: TTransf2D;
begin
  ScaleStandardForm.PicScale := Drawing.PicScale;
  if not (ScaleStandardForm.ShowModal = mrOK) then Exit;
  if ScaleStandardForm.PicScale <= 0 then
    T :=
      Modify.ScaleStandard(Drawing,
      ScaleStandardForm.ScaleStandardMaxWidth,
      ScaleStandardForm.ScaleStandardMaxHeight,
      False)
  else
  begin
    T := Scale2D(Drawing.PicScale / ScaleStandardForm.PicScale,
      Drawing.PicScale / ScaleStandardForm.PicScale);
    Drawing.TransformObjects([-1], T, False);
  end;
  if ScaleStandardForm.ScalePhysical.Checked
    then ScalePhysical(Drawing, IsotropicScale(T), False);
  if ScaleStandardForm.PicScale > 0 then
    Drawing.PicScale := ScaleStandardForm.PicScale;
//  Drawing.History.Save;
  Drawing.NotifyChanged;
  ViewPort.ZoomToExtension;
end;

procedure TTpXMode.Add_Point(P: TPoint2D);
var
  TmpObj: TPrimitive2D;
begin
  if not (Drawing.SelectionFirst is TPrimitive2D) then Exit;
  TmpObj := Drawing.SelectionFirst as TPrimitive2D;
  if TmpObj = nil then Exit;
  TmpObj.InsertControlPoint(P,
    ViewPort.GetAperture({fApertureSize} Drag_Aperture),
    ViewPort.GetPixelAperture.X);
end;

procedure TTpXMode.Delete_Point(P: TPoint2D);
var
  TmpObj: TPrimitive2D;
  Distance: TRealType;
  I: Integer;
begin
  if not (Drawing.SelectionFirst is TPrimitive2D) then Exit;
  TmpObj := Drawing.SelectionFirst as TPrimitive2D;
  if TmpObj = nil then Exit;
  Distance := ViewPort.GetAperture(Drag_Aperture);
  I := TmpObj.OnMe(P, Distance, Distance);
  if I < 0 then Exit;
  TmpObj.DeleteControlPoint(I);
end;

procedure TTpXMode.Break_Path(P: TPoint2D);
var
  TmpObj: TPrimitive2D;
begin
  if not (Drawing.SelectionFirst is TPrimitive2D) then Exit;
  TmpObj := Drawing.SelectionFirst as TPrimitive2D;
  if TmpObj = nil then Exit;
  BreakPath(Drawing, TmpObj, P,
    ViewPort.GetAperture({fApertureSize} Drag_Aperture),
    ViewPort.GetPixelAperture.X);
end;

{* --------- TBaseMode --------- *}

constructor TBaseMode.Create;
begin
  inherited Create;
end;

procedure TBaseMode.OnInsertGrObj(Msg: TEventMessage;
  Sender: TObject);
  function InsertText: Boolean;
  var
    TmpText: TText2D;
    TmpStr: string;
  begin
    Result := False;
    TmpStr := '';
    if not InputQuery('Add Text', 'String', TmpStr)
      or (TmpStr = '') then
    begin
      MainForm.PressModeButton(Sender, False);
      Exit;
    end;
    TmpText := TText2D.CreateSpec(-1, Point2D(1, 1),
      Drawing.DefaultFontHeight, TmpStr);
    InsertSimpleGrObjMode.Obj := TmpText;
    Result := True;
  end;
begin
  case Msg of
    Msg_InsertLine:
      begin
        InsertSizedGrObjMode.Obj :=
          TLine2D.CreateSpec(-1, Point2D(0, 0), Point2D(0, 0));
        InsertSizedGrObjMode.NPoints := 2;
      end;
    Msg_InsertRectangle:
      begin
        InsertSizedGrObjMode.Obj := TRectangle2D.Create(-1);
        InsertSizedGrObjMode.NPoints := 2;
      end;
    Msg_InsertCircle:
      begin
        InsertSizedGrObjMode.Obj := TCircle2D.Create(-1);
        InsertSizedGrObjMode.NPoints := 2;
      end;
    Msg_InsertEllipse:
      begin
        InsertSizedGrObjMode.Obj := TEllipse2D.Create(-1);
        InsertSizedGrObjMode.NPoints := 2;
      end;
    Msg_InsertArc:
      begin
        InsertSizedGrObjMode.Obj :=
          TArc2D.CreateSpec(-1, Point2D(0, 0), 0, 0, 0);
        InsertSizedGrObjMode.NPoints := 3;
      end;
    Msg_InsertSector:
      begin
        InsertSizedGrObjMode.Obj :=
          TSector2D.CreateSpec(-1, Point2D(0, 0), 0, 0, 0);
        InsertSizedGrObjMode.NPoints := 3;
      end;
    Msg_InsertSegment:
      begin
        InsertSizedGrObjMode.Obj :=
          TSegment2D.CreateSpec(-1, Point2D(0, 0), 0, 0, 0);
        InsertSizedGrObjMode.NPoints := 3;
      end;
    Msg_InsertPolyline:
      InsertUnsizedGrObjMode.Obj
        := TPolyline2D.CreateSpec(-1, [Point2D(0, 0)]);
    Msg_InsertPolygon:
      InsertUnsizedGrObjMode.Obj
        := TPolygon2D.CreateSpec(-1, [Point2D(0, 0)]);
    Msg_InsertCurve:
      begin
        InsertUnsizedGrObjMode.Obj
          := TSmoothPath2D.CreateSpec(-1, [Point2D(0, 0)]);
      end;
    Msg_InsertClosedCurve:
      begin
        InsertUnsizedGrObjMode.Obj
          := TClosedSmoothPath2D.CreateSpec(-1, [Point2D(0, 0)]);
      end;
    Msg_InsertBezier:
      begin
        InsertUnsizedGrObjMode.Obj
          := TBezierPath2D.CreateSpec(-1, [Point2D(0, 0)]);
      end;
    Msg_InsertClosedBezier:
      begin
        InsertUnsizedGrObjMode.Obj
          := TClosedBezierPath2D.CreateSpec(-1, [Point2D(0, 0)]);
      end;
    Msg_InsertText: if not InsertText then Exit;
    Msg_InsertStar:
      InsertSimpleGrObjMode.Obj
        := TStar2D.CreateSpec(-1, Point2D(0, 0));
    Msg_InsertSymbol:
      InsertSimpleGrObjMode.Obj
        := TSymbol2D.CreateSpec(-1, Point2D(1, 1),
        Drawing.DefaultSymbolSize);
  end;
  case Msg of
    Msg_InsertLine, Msg_InsertRectangle, Msg_InsertCircle,
      Msg_InsertEllipse,
      Msg_InsertArc, Msg_InsertSector, Msg_InsertSegment:
      begin
        InsertSizedGrObjMode.PressedBtn := Sender;
        PushMode(InsertSizedGrObjMode);
      end;
    Msg_InsertPolyline, Msg_InsertPolygon, Msg_InsertCurve,
      Msg_InsertClosedCurve, Msg_InsertBezier,
      Msg_InsertClosedBezier:
      begin
        InsertUnsizedGrObjMode.PressedBtn := Sender;
        PushMode(InsertUnsizedGrObjMode);
      end;
    Msg_InsertText, Msg_InsertStar, Msg_InsertSymbol:
      begin
        InsertSimpleGrObjMode.PressedBtn := Sender;
        PushMode(InsertSimpleGrObjMode);
      end;
  end;
end;

procedure TBaseMode.OnMessage(Msg: TEventMessage; Sender: TObject);
begin
  case Msg of
    Msg_StartProgram: StartProgram;
    Msg_Escape:
      begin
        Drawing.SelectionClear;
        Drawing.RepaintViewports;
      end;
    Msg_Stop: SaveSettings;
    Msg_New:
      begin
        if AskSaveCurrentDrawing <> mrCancel then
          NewDrawing(Drawing_NewFileName);
      end;
    Msg_NewWindow: OpenOrExec('', Application.ExeName);
    Msg_Open: DlgOpenDrawing;
    Msg_Save: TrySaveDrawing(Drawing.FileName);
    Msg_Print: {--not implemented--};
    Msg_SaveAs: DlgSaveDrawing(Drawing_NewFileName);
    Msg_CopyAsEMF:
      begin{$IFDEF VER140}
        CopyToClipboardAsMetaFile(Drawing); {$ELSE}{$ENDIF}
      end;
    Msg_TpXSettings: TpXSettings;
    Msg_Undo: EditUndo;
    Msg_Redo: EditRedo;
    Msg_Copy: Drawing.CopySelectionToClipboard;
    Msg_Paste: Drawing.PasteFromClipboard;
    Msg_Cut:
      begin
        Drawing.CopySelectionToClipboard;
        Drawing.DeleteSelected;
      end;
    Msg_Delete: Drawing.DeleteSelected;
    Msg_Duplicate:
      begin
        DuplicateSelected(Drawing, ViewPort.GetPixelAperture);
        Drawing.NotifyChanged;
        Drawing.RepaintViewports;
      end;
    Msg_SelectAll: Drawing.SelectAll;
    Msg_SelNext: Drawing.SelectNext(1);
    Msg_SelPrev: Drawing.SelectNext(-1);
    Msg_AreaSelect:
      begin
        SelectObjectsInAreaMode.PressedBtn := Sender;
        PushMode(SelectObjectsInAreaMode);
      end;
    Msg_AreaSelectInside:
      begin
        AreaSelectInside := not AreaSelectInside;
        MainForm.AreaSelectInsideAction.Checked :=
          AreaSelectInside;
      end;
    Msg_SelectedProperties: ChangeSelectedProperties;
    Msg_PictureProperties: PictureProperties;
    Msg_InsertLine..Msg_InsertSymbol: OnInsertGrObj(Msg, Sender);
    Msg_FreehandPolyline:
      begin
        FreehandPolylineMode.PressedBtn := Sender;
        PushMode(FreehandPolylineMode);
      end;
    Msg_SimplifyPoly:
      begin
        SimplifyPoly(Drawing, ViewPort.GetPixelAperture.Y);
        Drawing.RepaintViewports;
      end;
    Msg_ConnectPaths:
      begin
        ConnectPaths(Drawing);
        Drawing.RepaintViewports;
      end;
    Msg_ReversePoints:
      begin
        SelectedReversePoints(Drawing);
        Drawing.RepaintViewports;
      end;
    Msg_Group:
      begin
        GroupSelected(Drawing);
        Drawing.RepaintViewports;
      end;
    Msg_AddPoint:
      begin
        Add_Point(fLastPoint);
        Drawing.RepaintViewports;
      end;
    Msg_DeletePoint:
      begin
        Delete_Point(fLastPoint);
        Drawing.RepaintViewports;
      end;
    Msg_BreakPath:
      begin
        Break_Path(fLastPoint);
        Drawing.RepaintViewports;
      end;
    //Msg_ConvertTo: MainForm.ConvertToExecute(Self);
    Msg_MoveUp:
      ShiftSelected(V2D(0,
        ViewPort.GetPixelAperture.Y * 20));
    Msg_MoveDown:
      ShiftSelected(V2D(0,
        -ViewPort.GetPixelAperture.Y * 20));
    Msg_MoveLeft:
      ShiftSelected(V2D(-ViewPort.GetPixelAperture.X *
        20, 0));
    Msg_MoveRight:
      ShiftSelected(V2D(ViewPort.GetPixelAperture.X *
        20, 0));
    Msg_MoveUpPixel:
      ShiftSelected(V2D(0, ViewPort.GetPixelAperture.Y));
    Msg_MoveDownPixel:
      ShiftSelected(V2D(0, -ViewPort.GetPixelAperture.Y));
    Msg_MoveLeftPixel:
      ShiftSelected(V2D(-ViewPort.GetPixelAperture.X, 0));
    Msg_MoveRightPixel:
      ShiftSelected(V2D(ViewPort.GetPixelAperture.X, 0));
    Msg_FlipV:
      FlipSelected(0, 1);
    Msg_FlipH:
      FlipSelected(1, 0);
    Msg_RotateCounterclockW:
      RotateSelected(Pi / 2);
    Msg_RotateClockW:
      RotateSelected(-Pi / 2);
    Msg_RotateCounterclockWDegree:
      RotateSelected(Pi / 180);
    Msg_RotateClockWDegree:
      RotateSelected(-Pi / 180);
    Msg_Grow10:
      ScaleSelected(1.1);
    Msg_Shrink10:
      ScaleSelected(1 / 1.1);
    Msg_Grow1:
      ScaleSelected(1.01);
    Msg_Shrink1:
      ScaleSelected(1 / 1.01);
    Msg_StartMove: PushMode(MoveMode);
    Msg_StartRotate:
      begin
        RotateMode.fLastPoint := fLastPoint;
        PushMode(RotateMode);
      end;
    Msg_ScaleStandard: ScaleStandard;
    Msg_CustomTransform: CustomTransform;
    Msg_ConvertToGrayScale:
      begin
        MakeGrayscale(Drawing);
        Drawing.RepaintViewports;
      end;
    Msg_MoveForward, Msg_MoveBackward, Msg_MoveToFront,
      Msg_MoveToBack:
      begin
        BackwardForward(Drawing,
          TMvBackwardForward(Msg - Msg_MoveForward));
        Drawing.RepaintViewports;
      end;
    Msg_AlignLeft, Msg_AlignHCenter, Msg_AlignRight,
      Msg_AlignBottom, Msg_AlignVCenter, Msg_AlignTop:
      begin
        AlignSelected(Drawing,
          TObjectsAlignment(Msg - Msg_AlignLeft));
        Drawing.RepaintViewports;
      end;
    Msg_PreviewLaTeX: Preview_LaTeX(Drawing, ltxview_Dvi);
    Msg_PreviewPdfLaTeX: Preview_LaTeX(Drawing, ltxview_Pdf);
    Msg_PreviewLaTeX_PS: Preview_LaTeX(Drawing, ltxview_PS);
    Msg_PreviewSVG: Preview_Picture(Drawing, export_SVG);
    Msg_PreviewEMF: Preview_Picture(Drawing, export_EMF);
    Msg_PreviewEPS: Preview_Picture(Drawing, export_EPS);
    Msg_PreviewPNG: Preview_Picture(Drawing, export_PNG);
    Msg_PreviewBMP: Preview_Picture(Drawing, export_BMP);
    Msg_PreviewPDF: Preview_Picture(Drawing, export_PDF);
    Msg_DrawingSource: View_Source(Drawing);
    Msg_preview_tex_inc: OpenOrExec(TextViewerPath,
        ExtractFilePath(ParamStr(0)) + 'preview.tex.inc');
    Msg_metapost_tex_inc: OpenOrExec(TextViewerPath,
        ExtractFilePath(ParamStr(0)) + 'metapost.tex.inc');
    Msg_CaptureEMF: MainForm.CaptureEMFExecute(Self);
    Msg_ImageTool:
{$IFDEF VER140}
      EMF_Form.ShowModal;
{$ELSE}
      ;
{$ENDIF}
    Msg_ZoomArea:
      begin
        ZoomAreaMode.PressedBtn := Sender;
        PushMode(ZoomAreaMode);
      end;
    Msg_Panning:
      begin
        PanningMode.PressedBtn := Sender;
        PushMode(PanningMode);
      end;
  else if (Msg >= Msg_ConvertTo)
    and (Msg < Msg_ConvertTo + 20) then
  begin
    ConvertSelected(Drawing,
      TPrimitive2DClass(GraphicObjectClasses[
      Msg - Msg_ConvertTo + 1]));
    Drawing.RepaintViewports;
  end
  end;
  inherited OnMessage(Msg, Sender);
end;

procedure TBaseMode.OnDblClick(Sender: TObject; X, Y: Integer);
var
  TmpObj: TObject2D;
  OnObjectIndex: Integer;
begin
  TmpObj := Drawing.PickObject(
    ViewPort.ScreenToViewport(Point2D(X, Y)),
    ViewPort.GetPixelAperture.X * PixelApertureCoefficient,
    ViewPort.VisualRect, TpX_Manager.PickFilter,
    {FirstFound} False, OnObjectIndex);
  if OnObjectIndex < PICK_INOBJECT
    then TmpObj := nil;
  if TmpObj is TObject2D then
    ChangeObjectProperties(TmpObj as TObject2D);
end;

procedure TBaseMode.OnMouseDown(Sender: TObject; Button:
  TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fLastPoint := ViewPort.ScreenToViewport(Point2D(X, Y));
end;

procedure TBaseMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  CurrPoint: TPoint2D;
  TmpObj: TObject2D;
  CurrentCtrlPt, OnObjectIndex: Integer;
  TmpDist: TRealType;
begin
  CurrPoint := ViewPort.ScreenToViewport(Point2D(X, Y));
  if SSLeft in Shift then
  begin //Start drag
    if PointDistance2D(CurrPoint, fLastPoint)
      < ViewPort.GetAperture(Drag_Aperture) then Exit;
    TmpObj := Drawing.PickObject(fLastPoint,
      ViewPort.GetPixelAperture.X * PixelApertureCoefficient,
      ViewPort.VisualRect, TpX_Manager.PickFilter,
    {FirstFound} False, OnObjectIndex);

    if OnObjectIndex < PICK_INOBJECT
      then TmpObj := nil;
    if Assigned(TmpObj) then
    begin
      CurrentCtrlPt := TmpObj.OnMe(fLastPoint,
        ViewPort.GetAperture({fApertureSize} Drag_Aperture),
        TmpDist);
      if (CurrentCtrlPt >= 0) and
        (Drawing.SelectedObjects.Find(TmpObj.ID) <> nil) then
      begin
        MoveControlPointMode.PointIndex := CurrentCtrlPt;
        MoveControlPointMode.Shift := Shift;
        MoveControlPointMode.OriginalPrimitive := TmpObj as
          TPrimitive2D;
        PushMode(MoveControlPointMode);
        Exit;
      end;
    end;
    if not Assigned(TmpObj) then
    begin
      if not (SSShift in Shift) then
        if Drawing.SelectedObjects.Count > 0 then
        begin
          Drawing.SelectionClear;
          Drawing.RepaintViewports;
        end;
//        SelectObjectsInAreaMode.PressedBtn := Sender;
      PushMode(SelectObjectsInAreaMode);
      SelectObjectsInAreaMode.OnMouseDown(
        Sender, mbLeft, [], X, Y);
      Exit;
    end;
      //if Drawing.SelectedObjects.Count = 0 then
    if Drawing.SelectedObjects.Find(TmpObj.ID) = nil then
    begin
      Drawing.SelectionClear;
      Drawing.SelectionAdd(TmpObj);
      Drawing.RepaintViewports;
    end;
    if SSCtrl in Shift then // Ctrl pressed
    begin
      PushMode(DragCopyMode);
      DragCopyMode.StartPoint := fLastPoint;
      DragCopyMode.CurrTransf := IdentityTransf2D;
      DragCopyMode.DrawRubber;
      Exit;
    end;
    PushMode(DragMode);
    DragMode.StartPoint := fLastPoint;
    DragMode.CurrTransf := IdentityTransf2D;
    DragMode.DrawRubber;
    Exit;
  end;
end;

procedure TBaseMode.OnMouseUp(Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TmpObj: TObject2D;
  OnObjectIndex: Integer;
begin
  if Button = mbRight then
  begin
    OnLocalPopup(Sender, Shift, X, Y);
    Exit;
  end;
  TmpObj := Drawing.PickObject(fLastPoint,
    ViewPort.GetPixelAperture.X * PixelApertureCoefficient,
    ViewPort.VisualRect, TpX_Manager.PickFilter,
    {FirstFound} False, OnObjectIndex);
  if OnObjectIndex < PICK_INOBJECT
    then TmpObj := nil;
  // if (OnObjectIndex < PICK_INBBOX)  or not (TmpObj is SelectionFilter) then Exit;
  if SSCtrl in Shift then // Ctrl pressed
  begin
    TmpObj := Drawing.PickObject_PreferSelected(fLastPoint,
      ViewPort.GetPixelAperture.X * PixelApertureCoefficient,
      ViewPort.VisualRect, TpX_Manager.PickFilter,
    {FirstFound} False, OnObjectIndex);
    if (SSAlt in Shift) and
      ((OnObjectIndex = PICK_ONOBJECT)
      or (OnObjectIndex = PICK_ONINOBJECT)
      or (OnObjectIndex >= 0)) then
    begin
      if Drawing.SelectedObjects.Find(TmpObj.ID) = nil then Exit;
      if not (TmpObj is TPrimitive2D) then Exit;
      // Break path at a point
      BreakPath(Drawing, TmpObj as TPrimitive2D, fLastPoint,
        ViewPort.GetAperture({fApertureSize} Drag_Aperture),
        ViewPort.GetPixelAperture.X);
      Drawing.RepaintViewports;
      Exit;
    end;
    if (OnObjectIndex = PICK_ONOBJECT) or
      (OnObjectIndex = PICK_ONINOBJECT) then
    begin
      if Drawing.SelectedObjects.Find(TmpObj.ID) = nil then Exit;
      if not (TmpObj is TPrimitive2D) then Exit;
      // Add a point to the path
      (TmpObj as TPrimitive2D).InsertControlPoint(
        fLastPoint,
        ViewPort.GetAperture({fApertureSize} Drag_Aperture),
        ViewPort.GetPixelAperture.X);
      Drawing.RepaintViewports;
      Exit;
    end;
    if OnObjectIndex < 0 then Exit;
    if Drawing.SelectedObjects.Find(TmpObj.ID) = nil then Exit;
    if not (TmpObj is TPrimitive2D) then Exit;
    (TmpObj as TPrimitive2D).DeleteControlPoint(
      OnObjectIndex);
    Drawing.RepaintViewports;
    Exit;
  end;
  if not (SSShift in Shift) then // Shift not pressed
  begin // Select clicked object
    if not Assigned(TmpObj) then
    begin
      Drawing.SelectionClear;
      Drawing.RepaintViewports;
      Exit;
    end;
    if Drawing.SelectedObjects.Find(TmpObj.ID) <> nil then Exit;
    Drawing.SelectionClear;
    Drawing.SelectionAdd(TmpObj);
    Drawing.RepaintViewports;
    Exit;
  end;
  if not Assigned(TmpObj) then Exit;
          // Shift pressed: +/- object
  if not Drawing.SelectionRemove(TmpObj)
    then Drawing.SelectionAdd(TmpObj);
  Drawing.RepaintViewports;
end;

procedure TBaseMode.OnLocalPopup(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  TmpObj: TObject2D;
  OnObjectIndex: Integer;
  Pt: TPoint;
begin
  Pt := MainForm.LocalView.ClientToScreen(Point(X, Y));
  TmpObj := Drawing.PickObject_PreferSelected(fLastPoint,
    ViewPort.GetPixelAperture.X * PixelApertureCoefficient,
    ViewPort.VisualRect, TpX_Manager.PickFilter,
    {FirstFound} False, OnObjectIndex);
  if OnObjectIndex < PICK_INOBJECT
    then TmpObj := nil;
  if Assigned(TmpObj) then
    if Drawing.SelectedObjects.Find(TmpObj.ID) = nil then
    begin
      Drawing.SelectionClear;
      Drawing.SelectionAdd(TmpObj);
      Drawing.RepaintViewports;
    end
    else
    begin
      if TmpObj.ID <> Drawing.SelectionFirst.ID then
        Drawing.SelectedObjects.Move(TmpObj.ID,
          Drawing.SelectionFirst.ID);
    end;
  MainForm.FillLocalPopUp(Assigned(TmpObj),
    Drawing.SelectedObjects.Count > 0,
    ((OnObjectIndex = PICK_ONOBJECT)
    or (OnObjectIndex = PICK_ONINOBJECT)
    or (OnObjectIndex >= 0)),
    OnObjectIndex >= 0,
    Assigned(TmpObj) and (TmpObj is TPrimitive2D)
    and (TmpObj as TPrimitive2D).CanDeletePoints);
  MainForm.LocalPopUp.Popup(Pt.X, Pt.Y);
end;

{* --------- TOpenRecentMode --------- *}

procedure TOpenRecentMode.OnPush;
var
  FileName: string;
begin
  if (Index < 0) or (Index >= TpX_Manager.RecentFiles.Count) then
  begin
    PopSelf;
    Exit;
  end;
  FileName := TpX_Manager.RecentFiles[Index];
  if not FileExists(FileName) then
  begin
    TpX_Manager.RecentFiles.Remove(FileName);
    PopSelf;
    Exit;
  end;
  if AskSaveCurrentDrawing <> mrCancel
    then DoOpenDrawing(FileName);
  PopSelf;
end;

{* --------- TInterruptableMode --------- *}

constructor TInterruptableMode.Create;
begin
  inherited Create;
  StopMessages := StopMessages + [Msg_Escape];
  PressedBtn := nil;
end;

procedure TInterruptableMode.OnPush;
begin
  MainForm.PressModeButton(PressedBtn, True);
end;

procedure TInterruptableMode.OnPop;
begin
  MainForm.PressModeButton(PressedBtn, False);
end;

{* --------- TInsertGrObjMode --------- *}

constructor TInsertGrObjMode.Create;
begin
  inherited Create;
  Obj := nil;
end;

procedure TInsertGrObjMode.OnPush;
begin
  inherited OnPush;
  if Assigned(Obj) then Obj.ParentDrawing := Drawing;
end;

procedure TInsertGrObjMode.OnPop;
begin
  inherited OnPop;
  if Assigned(Obj) then FreeAndNil(Obj);
  Drawing.RepaintViewports;
end;

{* --------- TInsertSimpleGrObjMode --------- *}

constructor TInsertSimpleGrObjMode.Create;
begin
  inherited Create;
end;

procedure TInsertSimpleGrObjMode.OnPush;
begin
  inherited OnPush;
end;

procedure TInsertSimpleGrObjMode.OnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(Obj) then
  begin
    (Obj as TPrimitive2D).Points[0] :=
      ViewPort.GetSnappedPoint(
      ViewPort.ScreenToViewport(Point2D(X, Y)));
    Obj.ParentDrawing := nil;
    Drawing.AddObject(-1, Obj);
    Drawing.SelectionAdd(Obj);
    Obj := nil;
  end;
  PopSelf;
end;

procedure TInsertSimpleGrObjMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  CurrPoint: TPoint2D;
begin
  CurrPoint := ViewPort.GetSnappedPoint(
    ViewPort.ScreenToViewport(Point2D(X, Y)));
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
  Obj.MoveTo(CurrPoint, (Obj as TPrimitive2D).Points[0]
    {Obj.Box.FirstEdge});
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
end;

{* --------- TInsertUnsizedGrObjMode --------- *}

procedure TInsertUnsizedGrObjMode.OnPush;
begin
  inherited OnPush;
  (Obj as TPrimitive2D).FirstDrawPoint := 0;
end;

procedure TInsertUnsizedGrObjMode.OnDblClick(Sender: TObject;
  X, Y: Integer);
begin
  if Assigned(Obj) then
  begin
    (Obj as TPrimitive2D).Points.Delete(
      (Obj as TPrimitive2D).Points.Count - 1);
    Dec((Obj as TPrimitive2D).FirstDrawPoint);
    (Obj as TPrimitive2D).FinishFirstDraw;
    Obj.ParentDrawing := nil;
    Drawing.AddObject(-1, Obj);
    Drawing.SelectionAdd(Obj);
    Obj := nil;
  end;
  PopSelf;
end;

procedure TInsertUnsizedGrObjMode.OnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurrPoint: TPoint2D;
  Prim: TPrimitive2D;
begin
  Prim := Obj as TPrimitive2D;
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
  CurrPoint :=
    ViewPort.GetSnappedPoint(
    ViewPort.ScreenToViewport(Point2D(X, Y)));
  //SnapOriginPoint := CurrPoint;
  if Prim.FirstDrawPoint = 0 then
    Prim.Points.Add(CurrPoint)
  else
  begin
    if Drawing.UseAngularSnap then
      Make45_2D(Prim.Points[Prim.FirstDrawPoint - 1],
        CurrPoint);
    Prim.Points[Prim.FirstDrawPoint] := CurrPoint;
  end;
  if (Prim.FirstDrawPoint = 0) or
    not IsSamePoint2D(CurrPoint,
    Prim.Points[Prim.FirstDrawPoint - 1]) then
    Inc(Prim.FirstDrawPoint);
  Prim.Points[Prim.FirstDrawPoint] := CurrPoint;
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
end;

procedure TInsertUnsizedGrObjMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  CurrPoint: TPoint2D;
  Prim: TPrimitive2D;
begin
  Prim := Obj as TPrimitive2D;
  CurrPoint :=
    ViewPort.GetSnappedPoint(
    ViewPort.ScreenToViewport(Point2D(X, Y)));
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
  if Prim.FirstDrawPoint > 0 then
    if Drawing.UseAngularSnap then
      Make45_2D(Prim.Points[Prim.FirstDrawPoint - 1], CurrPoint);
  Prim.Points[Prim.FirstDrawPoint] := CurrPoint;
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
end;

{* --------- TInsertSizedGrObjMode --------- *}

procedure TInsertSizedGrObjMode.OnPush;
begin
  inherited OnPush;
  (Obj as TPrimitive2D).FirstDrawPoint := 0;
  //SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

procedure TInsertSizedGrObjMode.FinishDraw;
begin
  if Assigned(Obj) then
  begin
    (Obj as TPrimitive2D).FinishFirstDraw;
    Obj.ParentDrawing := nil;
    Drawing.AddObject(-1, Obj);
    Drawing.SelectionAdd(Obj);
    Obj := nil;
  end;
  PopSelf;
end;

procedure TInsertSizedGrObjMode.OnDblClick(Sender: TObject;
  X, Y: Integer);
begin
  FinishDraw;
end;

procedure TInsertSizedGrObjMode.PointSelected(P: TPoint2D);
var
  I: Integer;
  Prim: TPrimitive2D;
begin
  Prim := Obj as TPrimitive2D;
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
  if Prim.FirstDrawPoint = 0 then
  begin
    for I := 0 to NPoints - 1 do
      Prim.Points[I] := P;
    if Prim is TBox2D0 then
      Prim.Points[2] := Point2D(P.X, P.Y - 1);
  end
  else
  begin
    if Drawing.UseAngularSnap then
      Make45_2D(Prim.Points[Prim.FirstDrawPoint - 1], P);
    Prim.Points[Prim.FirstDrawPoint] := P;
  end;
  Inc(Prim.FirstDrawPoint);
  if Prim.FirstDrawPoint = NPoints then
    FinishDraw
  else
    Prim.Points[Prim.FirstDrawPoint] := P;
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
end;

procedure TInsertSizedGrObjMode.OnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  PointSelected(
    ViewPort.GetSnappedPoint(
    ViewPort.ScreenToViewport(Point2D(X, Y))));
end;

procedure TInsertSizedGrObjMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  CurrPoint: TPoint2D;
  Prim: TPrimitive2D;
begin
  if Obj = nil then Exit;
  Prim := Obj as TPrimitive2D;
  if Prim.FirstDrawPoint <= 0 then Exit;
  CurrPoint :=
    ViewPort.GetSnappedPoint(
    ViewPort.ScreenToViewport(Point2D(X, Y)));
  if Drawing.UseAngularSnap then
    Make45_2D(Prim.Points[Prim.FirstDrawPoint - 1], CurrPoint);
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
  Prim.Points[Prim.FirstDrawPoint] := CurrPoint;
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
end;

procedure TInsertSizedGrObjMode.OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  if (Obj as TPrimitive2D).FirstDrawPoint = 1 then
    PointSelected(
      ViewPort.GetSnappedPoint(
      ViewPort.ScreenToViewport(Point2D(X, Y))));
end;

{* --------- TFreehandPolylineMode --------- *}

procedure TFreehandPolylineMode.OnPush;
begin
  inherited OnPush;
  //ViewPort.Cursor := crPen;
  ViewPort.Cursor := crHandPoint;
end;

procedure TFreehandPolylineMode.OnPop;
begin
  ViewPort.Cursor := crDefault;
  inherited OnPop;
end;

procedure TFreehandPolylineMode.FinishDraw;
begin
  if Assigned(Obj) then
  begin
    (Obj as TPrimitive2D).FinishFirstDraw;
    Obj.ParentDrawing := nil;
    Drawing.AddObject(-1, Obj);
    Drawing.SelectionAdd(Obj);
    Obj := nil;
  end;
  PopSelf;
end;

procedure TFreehandPolylineMode.OnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  Obj := TPolyline2D.Create(-1);
  Obj.ParentDrawing := Drawing;
end;

procedure TFreehandPolylineMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  CurrPoint: TPoint2D;
  Prim: TPrimitive2D;
begin
  if Obj = nil then Exit;
  Prim := Obj as TPrimitive2D;
  CurrPoint :=
    ViewPort.ScreenToViewport(Point2D(X, Y));
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
  Prim.Points.Add(CurrPoint);
  SimplifyPolyInPlace(Prim.Points,
    ViewPort.GetPixelAperture.Y, False);
  ViewPort.DrawObject2DWithRubber(Obj, IdentityTransf2D);
end;

procedure TFreehandPolylineMode.OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FinishDraw;
end;

{* --------- TSelectBoxMode --------- *}

constructor TSelectBoxMode.Create;
begin
  inherited Create;
  Rect := TRectangle2D.Create(0);
  fIsSelecting := False;
end;

destructor TSelectBoxMode.Destroy;
begin
  Rect.Free;
  inherited Destroy;
end;

procedure TSelectBoxMode.OnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint2D;
begin
  P := ViewPort.ScreenToViewport(Point2D(X, Y));
  Rect.Points[0] := P;
  Rect.Points[1] := P;
  Rect.Points[2] := Point2D(P.X, P.Y - 1);
  fIsSelecting := True;
  //ViewPort.DrawObject2DWithRubber(Rect, False);
end;

procedure TSelectBoxMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not fIsSelecting then Exit;
  ViewPort.DrawObject2DWithRubber(Rect, IdentityTransf2D);
  Rect.Points[1] :=
    ViewPort.ScreenToViewport(Point2D(X, Y));
  ViewPort.DrawObject2DWithRubber(Rect, IdentityTransf2D);
end;

procedure TSelectBoxMode.OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Rect.Points[1] :=
    ViewPort.ScreenToViewport(Point2D(X, Y));
  fIsSelecting := False;
  PopSelf;
end;

{* --------- TSelectObjectsInAreaMode --------- *}

procedure TSelectObjectsInAreaMode.DoSelectObjectsInArea(Rect:
  TRect2D);
var
  AreaMode: TGroupMode;
  TempList: TGraphicObjList;
begin
  if AreaSelectInside = True then
    AreaMode := gmAllInside
  else
    AreaMode := gmCrossFrame;
  TempList := TGraphicObjList.Create;
  TempList.FreeOnClear := False;
  try
    ViewPort.StopRepaint;
    Drawing.GroupObjects(TempList, Rect, ViewPort.VisualRect,
      AreaMode, TpX_Manager.PickFilter, False);
    Drawing.SelectionAddList(TempList);
  finally
    TempList.Free;
  end;
  Drawing.RepaintViewports
end;

procedure TSelectObjectsInAreaMode.OnPush;
begin
  inherited OnPush;
  ViewPort.Cursor := crCross
end;


procedure TSelectObjectsInAreaMode.OnPop;
begin
  ViewPort.Cursor := crDefault;
  inherited OnPop;
end;

procedure TSelectObjectsInAreaMode.OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited OnMouseUp(Sender, Button, Shift, X, Y);
  DoSelectObjectsInArea(Rect.BoundingBox);
end;

{* --------- TZoomAreaMode --------- *}

procedure TZoomAreaMode.OnPush;
begin
  inherited OnPush;
  ViewPort.Cursor := crCross;
end;


procedure TZoomAreaMode.OnPop;
begin
  ViewPort.Cursor := crDefault;
  inherited OnPop;
end;

procedure TZoomAreaMode.OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited OnMouseUp(Sender, Button, Shift, X, Y);
  ViewPort.ZoomWindow(Rect.BoundingBox);
end;

{* --------- TPanningMode --------- *}

procedure TPanningMode.OnPush;
begin
  inherited OnPush;
{$IFDEF VER140}
  ViewPort.Cursor := crHand;
{$ELSE}
  ViewPort.Cursor := {crSizeAll crHand} crSizeAll;
{$ENDIF}
  fIsMoving := False;
end;

procedure TPanningMode.OnPop;
begin
  ViewPort.Cursor := crDefault;
  inherited OnPop;
end;

procedure TPanningMode.OnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fStartPoint := ViewPort.ScreenToViewport(Point2D(X, Y));
  fIsMoving := True;
end;

procedure TPanningMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint2D;
begin
  if not fIsMoving then Exit;
  P := ViewPort.ScreenToViewport(Point2D(X, Y));
  ViewPort.PanWindow(fStartPoint.X - P.X,
    fStartPoint.Y - P.Y);
end;

procedure TPanningMode.OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OnMouseMove(Sender, Shift, X, Y);
  inherited OnMouseUp(Sender, Button, Shift, X, Y);
  fIsMoving := False;
end;

{* --------- TTransformingMode --------- *}

procedure TTransformingMode.ConfirmTransform(X, Y: Integer);
begin
  MouseMove(X, Y);
  TransformSelected(CurrTransf, True);
  PopSelf;
end;

procedure TTransformingMode.OnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ConfirmTransform(X, Y);
end;

procedure TTransformingMode.MouseMove(X, Y: Integer);
var
  P: TPoint2D;
begin
  DrawRubber;
  P := ViewPort.ScreenToViewport(Point2D(X, Y));
  if (Self is TMoveMode) or (Self is TDragMode)
    or (Self is TDragCopyMode) then
    P := ViewPort.GetSnappedPoint(P);
  CurrTransf := GetTransform(P);
  DrawRubber;
end;

procedure TTransformingMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseMove(X, Y);
end;

procedure TTransformingMode.OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Self is TDragMode) or (Self is TDragCopyMode) then
    ConfirmTransform(X, Y);
end;

function TTransformingMode.GetTransform(P: TPoint2D): TTransf2D;
begin
  if Drawing.UseAngularSnap then
    Make45_2D(StartPoint, P);
  Result := Translate2D(P.X - StartPoint.X, P.Y - StartPoint.Y);
end;

procedure TTransformingMode.DrawRubber;
var
  Obj: TObject2D;
  Iter: TGraphicObjIterator;
begin
  Iter := Drawing.SelectedObjects.GetIterator;
  with ViewPort do
  try
    Obj := Iter.First as TObject2D;
    while Assigned(Obj) do
    begin
      //if not (Obj is TObject2D) then Continue;
      if not TObject2D(Obj).IsVisible(
        VisualRect) then Exit;
      with Obj as TObject2D do
        ViewPort.DrawObject2DWithRubber(Obj, CurrTransf);
      Obj := Iter.Next as TObject2D;
    end;
  finally
    Iter.Free;
  end;
end;

{* --------- TDragCopyMode --------- *}

procedure TDragCopyMode.ConfirmTransform(X, Y: Integer);
begin
  MouseMove(X, Y);
  DuplicateSelected(Drawing, V2D(0, 0));
  TransformSelected(CurrTransf, True);
  PopSelf;
end;

procedure TDragCopyMode.OnPush;
begin
  inherited OnPush;
  ViewPort.Cursor := crDrag;
  //ViewPort.Cursor := crPlus; //crMultiDrag;
end;

procedure TDragCopyMode.OnPop;
begin
  ViewPort.Cursor := crDefault;
  inherited OnPop;
end;

{* --------- TMoveMode --------- *}

procedure TMoveMode.OnPush;
begin
  StartPoint :=
    ViewPort.GetSnappedPoint(
    Drawing.GetSelectionCenter);
  CurrTransf := IdentityTransf2D;
  {CurrTransf := GetTransform(
    ViewPort.GetSnappedPoint(
    ViewPort.ScreenToViewport(Point2D(X, Y))));}
  DrawRubber;
end;

{* --------- TRotateMode --------- *}

procedure TRotateMode.OnPush;
begin
  StartPoint := Drawing.GetSelectionCenter;
  BaseAngle := ArcTan2(fLastPoint.Y - StartPoint.Y,
    fLastPoint.X - StartPoint.X);
  CurrTransf := GetTransform(fLastPoint);
  DrawRubber;
end;

function TRotateMode.GetTransform(P: TPoint2D): TTransf2D;
begin
  if Drawing.UseAngularSnap then
    Make45_2D(StartPoint, P);
  Result := RotateCenter2D(
    ArcTan2(P.Y - StartPoint.Y,
    P.X - StartPoint.X) - BaseAngle, StartPoint);
end;

{* --------- TMoveControlPointMode --------- *}

procedure TMoveControlPointMode.OnPush;
begin
  CurrentPrimitive :=
    TpXFindClassByName(OriginalPrimitive.ClassName).Create(0)
    as TPrimitive2D;
  CurrentPrimitive.Assign(OriginalPrimitive);
  CurrentPrimitive.ParentDrawing := Drawing;
  ViewPort.DrawObject2DWithRubber(CurrentPrimitive,
    IdentityTransf2D);
end;

procedure TMoveControlPointMode.OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint2D;
begin
  ViewPort.DrawObject2DWithRubber(CurrentPrimitive,
    IdentityTransf2D);
  P := ViewPort.GetSnappedPoint(
    ViewPort.ScreenToViewport(Point2D(X, Y)));
  if Drawing.UseAngularSnap then
    Make45_2D(OriginalPrimitive.Points[PointIndex], P);
  P := ViewPort.WorldToObject(CurrentPrimitive, P);
  CurrentPrimitive.MoveControlPoint0(PointIndex, P, Shift);
  ViewPort.DrawObject2DWithRubber(CurrentPrimitive,
    IdentityTransf2D);
end;

procedure TMoveControlPointMode.OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OriginalPrimitive.Assign(CurrentPrimitive);
  Drawing.NotifyChanged;
  if Drawing.SelectedObjects.Find(
    OriginalPrimitive.ID) = nil then
    Drawing.SelectionAdd(OriginalPrimitive);
  Drawing.RepaintViewports;
  PopSelf;
end;

initialization
  BaseMode := TBaseMode.Create;
  OpenRecentMode := TOpenRecentMode.Create;
  InsertSimpleGrObjMode := TInsertSimpleGrObjMode.Create;
  InsertUnsizedGrObjMode := TInsertUnsizedGrObjMode.Create;
  InsertSizedGrObjMode := TInsertSizedGrObjMode.Create;
  FreehandPolylineMode := TFreehandPolylineMode.Create;
  SelectObjectsInAreaMode := TSelectObjectsInAreaMode.Create;
  ZoomAreaMode := TZoomAreaMode.Create;
  PanningMode := TPanningMode.Create;
  DragMode := TDragMode.Create;
  DragCopyMode := TDragCopyMode.Create;
  MoveMode := TMoveMode.Create;
  RotateMode := TRotateMode.Create;
  MoveControlPointMode := TMoveControlPointMode.Create;
finalization
  BaseMode.Free;
  OpenRecentMode.Free;
  InsertSimpleGrObjMode.Free;
  InsertUnsizedGrObjMode.Free;
  InsertSizedGrObjMode.Free;
  FreehandPolylineMode.Free;
  SelectObjectsInAreaMode.Free;
  ZoomAreaMode.Create;
  PanningMode.Free;
  DragMode.Free;
  DragCopyMode.Free;
  MoveMode.Free;
  RotateMode.Free;
  MoveControlPointMode.Free;
end.

