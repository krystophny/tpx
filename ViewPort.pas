unit ViewPort;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Types, SysUtils, Classes, Messages, Graphics, Controls,
  DevCanvas, Geometry, ColorEtc,
{$IFDEF VER140}
  WinBasic
{$ELSE}
  LCLIntf, LMessages, LCLType, LazBasic
{$ENDIF}
  , GObjBase, Drawings, Devices;

type

{: This is the type of an event handler for the mouse button event.

   <I=Sender> is the component that has raised the event, <I=Button>
   is the mouse button that has caused the event, <I=Shift> was
   the key configuration when the event was raised.
   <I=WX> and <I=WY> are the X and Y mouse coordinate in the world
   coordinate system ans X, Y are the X and Y mouse coordinate in
   the screen coordinate system.
}
  TMouseEvent2D = procedure(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; WX, WY: TRealType; X, Y:
    Integer) of object;
{: This is the type of an event handler for the mouse move event.

   <I=Sender> is the component that has raised the event, <I=Shift> was
   the key configuration when the event was raised.
   <I=WX> and <I=WY> are the X and Y mouse coordinate in the world
   coordinate system ans X, Y are the X and Y mouse coordinate in
   the screen coordinate system.
}
  TMouseMoveEvent2D = procedure(Sender: TObject; Shift:
    TShiftState; WX, WY: TRealType; X, Y: Integer) of object;

  {: This class defines a viewport with which it is possible to render
     the contents of a drawing. This is a visual component.

     The viewport is a 2D window through which you can see a virtual world.
     The world is stored in a <See Class=TDrawing> control as a list (display list)
     of graphic objects. The world used here is dimension less, so this component
     is not directly usable but it defines only an abstract interface.

     Regardless the kind of world (2d, 3d or more) a viewport is always a window
     on which the world is projected. The view trasformation has two
     components:

     <LI=the first one project the world on the window plane. If the world
         is already 2D this is a simple identity transform (Projection) >
     <LI=the second one project the window plane onto a portion on the canvas
         of the control (Mapping).>

     This control defines and uses only the second component of the transformation
     (see <See Method=TViewport@BuildViewportTransform>). The other one is
     dimension dependent and so must be defined in derived classes.

     You manage the window position and size (that is modify the second component
     of the above transformations) using the property
     <See Property=TViewport@VisualRect>, that is the portion of the
     window plane that you see on the canvas of the control.
     You change the VisualRect using the zooming methods (see
     <See Method=TViewport@ZoomIn>, <See Method=TViewport@ZoomOut>,
     <See Method=TViewport@ZoomWindow>, <See Method=TViewport@ZoomToExtension>
     <See Method=TViewport@PanWindow> and <See Method=TViewport@MoveWindow>).
     All of these changes the mapping transformation (the second component explained
     above).

     A viewport use a back buffer to store the current rapresentation of the
     Drawing display list. When you start a <See Method=TViewport@Repaint>,
     this buffer is created and copied on the canvas of the control.
     The repaint can use a thread (painting thread) to allow the user to stop
     the operation at any time. However using a thread require some additional
     steps that require a bit of time, so it is advisable to use the thread
     (see <See Property=TViewport@UsePaintingThread>) only when you have
     a complex draw made up of thousand of objects.
     During the repaint process you can also copy the buffer content on the
     on screen canvas when a prestabilited number of objects (see
     <See Property=TViewport@CopingFrequency> property) is drawed. This
     gives a useful feedback to the user.

     See also <See Class=TViewport2D>.
}
  TViewport = class(TBaseViewport)
  private
    fViewGuard: TTpXCriticalSection;
    fDrawing: TDrawing;
    fVisualWindow: TRect2D;
      { The current viewport on the view plane. }
    fViewportToScreen, fScreenToViewport: TTransf2D;
    //TSY: added fControlPointsPenColor
    fBackGroundColor, fGridColor,
      fControlPointsColor, fControlPointsPenColor: TColor;
    { FOffScreenBitmap contain the off-screen bitmap used by the Viewport. The
      Viewport use FOffScreenBitmap to store the actual view of the draw. When
      the draw change the FOffScreenBitmap is redrawed. During a repaint the
      FOffScreenBitmap is put on the canvas. }
    fOffScreenBitmap: TBitmap;
    fOffScreenCanvas: TCanvas;
    fOnScreenCanvas: TCanvas;
    fOffScreenDevice: TCanvasDevice;
    fOnScreenDevice: TCanvasDevice;
    fRubberDevice: TRubberCanvasDevice;
    fCopingFrequency: Integer;
      { Indica ogni quanto copiare il buffbitmap sul canvas quando si usa il threading. }
    fShowControlPoints, fShowGrid, fInUpdate: Boolean;
    fControlPointsWidth: Byte;
    fGridStep, fSnapStep: TRealType;
    fGridOnTop: Boolean;
    fTransparent: Boolean;
    { Event handlers }
    fDisablePaintEvent, fDisableMouseEvents: Boolean;
    fOnPaint, fOnResize, fOnBeginRedraw, fOnEndRedraw:
    TNotifyEvent;
    fOnViewMappingChanged: TNotifyEvent;
    fOnMouseEnter, fOnMouseLeave: TNotifyEvent;

    { Multithread support. }
    fPaintingThread: TObject;
    fUseThread: Boolean;

    { Set method for the property. }
    procedure SetBackColor(const Cl: TColor);
    procedure SetTransparent(const B: Boolean);
    procedure SetGridColor(const Cl: TColor);
    procedure SetShowGrid(const B: Boolean);
    procedure ClearCanvas(Sender: TObject; Cnv: TCanvas; const
      ARect: TRect2D; const BackCol: TColor);
    procedure DoCopyCanvas(const GenEvent: Boolean);
    procedure DoCopyCanvasThreadSafe;
    procedure ChangeViewportTransform(ViewWin: TRect2D);

    { Per il thread }
    procedure StopPaintingThread;
    { Se sono in repainting lo blocco e ricomincio. }
    procedure UpdateViewport(const ARect: TRect2D);
      { repaint all the objects contained in ARect. }
    function GetInRepaint: Boolean;
    procedure DoResize;
    procedure CopyBitmapOnCanvas(const DestCnv: TCanvas; const
      BMP: TBitmap; IRect: TRect; IsTransparent: Boolean;
      TransparentColor: TColor);
    procedure Control_Point(const P: TPoint2D;
      const VT: TTransf2D; const ClipRect: TRect2D);
    procedure Control_Point2(const P: TPoint2D;
      const VT: TTransf2D; const ClipRect: TRect2D);
    procedure Control_Point3(const P: TPoint2D;
      const VT: TTransf2D; const ClipRect: TRect2D);
    procedure Control_Line(const P0, P1: TPoint2D;
      const VT: TTransf2D; const ClipRect: TRect2D);
    procedure Control_Box(const R: TRect2D;
      const VT: TTransf2D; const ClipRect: TRect2D);
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    {: This method is called when a painting thread (if used) is terminated.

       It sets the thread instance reference to nil (the thread is freed by
       itself). It also copy the backbuffer on the canvas of the control.

       See also <See Property=TViewport@UsePaintingThread>.
    }
    procedure OnThreadEnded(Sender: TObject); dynamic;
    {: This method copies the backbuffer image on the canvas of the control.

       <I=Rect> is the rectangle of the backbuffer to be copied on the same
       rectangle on the canvas of the control (remember that the two canvases
       have the same size); if <I=GenEvent> is <B=True> the an
       <See Property=TViewport@OnPaint> event will be fired after the copy.
    }
    procedure CopyBackBufferRectOnCanvas(const Rect: TRect; const
      GenEvent: Boolean); dynamic;
    {: This method sets the <See Class=TDrawing> control that contains the display list
       to be drawed.

       <I=Drawing> is the drawing to which the viewport will be linked.
       If it is <B=nil> the viewport will be removed from the current drawing
       to which it is linked.

       See also <See Property=TDrawing@Viewports> property.
    }
    procedure SetDrawing(ADrawing: TDrawing); virtual;
    {: This method handles the WM_PAINT message of Windows.

       It firstly check to see if the dimension of the component are
       different with the ones of the back buffer canvas. In this case
       the back buffer dimensions are changed accordingly and the
       display list is rendered on it.

       After that the back buffer image is simply copied on the control
       canvas and an <See Property=TViewport@OnPaint> event is fired.
    }
    procedure Paint; override; { Repaint all the objects. }
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message
      WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMMouseEnter(var Message: TMessage); message
      CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message
      cm_MOUSELEAVE;
    {: This method is called by the viewport whenever a changing in the
       mapping transform from window plane (view plane) and canvas
       rectangle is required.

       It must returns a 2d transform matrix that transforms the 2d points
       in the visual rect (see <See Property=TViewport@VisualRect>)
       in the 2d points in the client rectangle of the control Canvas.

       <I=ViewWin> is the visual rect (that is the portion of
       the world currently in view); <I=ScreenWin> is the portion of the
       control's canvas that must contains the drawing.

       The new ViewWin will became the <See Property=TViewport@VisualRect>
       of the viewport.

       You must redefine this method if you want to create a new viewport,
       for example you can also change any other projection transform when
       this method is called; by default it returns <See const=IdentityTransf2D>.

       <B=Note>: You may want to use the <See function=GetVisualTransform2D> function
       to obtain the mapping transform.
    }
    function BuildViewportTransform(var ViewWin: TRect2D; const
      ScreenWin: TRect): TTransf2D; virtual;
    {: This method draws a 2D rectangular grid on the viewport.

       <I=ARect> is the portion of the window plane that is currently viewed in
       the control; <I=Cnv> is the canvas on which draw the grid.

       By default it draws a grid that originates in (0, 0) and has an X step
       and Y step specified by the <See Property=TViewport@GridStep>
       property.
    }
    procedure DrawGrid(const ARect: TRect2D;
      const Cnv: TCanvas); virtual;
    procedure DrawObjectControlPoints(
      const Obj: TGraphicObject; const Dvc: TDevice;
      const ClipRect2D: TRect2D);
      virtual; abstract;
    {: This method returns the mapping transform matrix that maps the
       visual rect portion of the view plane in the client area of the
       control.

       You may want to use it to mimics viewport view transform. For
       instance with this transformation you can found where a point in
       viewplane coordinates will be transformed on the control's canvas.

       <B=Note>: This matrix models only the mapping part of the view
       transformation. You may also want to add the projection part, that
       is viewport dependent.
    }
    function GetViewportToScreen: TTransf2D; virtual;
    {: This method returns the invers of the mapping transform matrix that
       maps the client area of the control in the visual rect portion of
       the view plane.

       You may want to use it to mimics viewport view transform. For
       instance with this transformation you can found where a point in
       canvas of the control will be transformed in the view plane.

       <B=Note>: This matrix models only the mapping part of the view
       transformation. You may also want to add the projection part, that
       is viewport dependent.
    }
    function GetScreenToViewport: TTransf2D; virtual;
    procedure Notification(AComponent: TComponent; Operation:
      TOperation); override;

    {: The back-buffer bitmap. }
    property OffScreenBitmap: TBitmap read fOffScreenBitmap;
  public
    { Public declarations }
    ShowRulers: Boolean;
    {: This is the constructor of the control.
       It creates a new viewport.
    }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {: This method draws an object on a canvas, by transforming it with the
       view transform.

       This method must be specilized to manages the objects of the correct type
       in the display list. This method then calls the appropriate drawing
       method of the specialized object. You have also to check for
       visibility in this method, to reduce the time of drawing.

       Before any drawing takes place the Canvas is modified with the
       object's layer properties.

       <I=Obj> is the object to be drawed and <I=Cnv> is the canvas on
       which to draw the object. <I=ClipRect2D> is the clipping rectangle
       in 2D coordinates (of the canvas).
    }
    procedure DrawObject(const Obj: TGraphicObject;
      const Dvc: TCanvasDevice;
      const ClipRect2D: TRect2D); virtual; abstract;
    {: This method forces to rebuild the mapping tranform.

       When the visual rect change this method will be called and the
       viewport repainted. If you want to force the mapping tranform
       to be updated call this method (this is useful when you derive a
       new viewport that has its own projection transform).
    }
    procedure UpdateViewportTransform;
    {: This method set the visual rect to a specified rectangle.

       <I=NewWindow> is the new visual rect. This correspond to
       zoom the viewport to a specified portion of the view plane.
       The rectangle is in view plane coordinates.
    }
    procedure ZoomWindow(const NewWindow: TRect2D);
    {: This method moves the current visual rect to a specified
       position.

       <I=NewStartX> is the new X position of the lower-left corner
       of visual rect; <I=NewStartY> is the new y position of the lower-left corner
       of visual rect.

       The two coordinates are referred to view plane coordinates.
    }
    procedure MoveWindow(const NewStartX, NewStartY: TRealType);
    {: This method enlarges the current visual rect.

       The actual visual rect is doubled on both X and Y directions,
       resulting in a magnification of the current view of the drawing.
    }
    procedure ZoomCenter(const C: TPoint2D; const F: TRealType);
    procedure ZoomFrac(const FX, FY: TRealType; const F:
      TRealType);
    procedure ZoomSelCenter(const F: TRealType);
    procedure ZoomViewCenter(const F: TRealType);
    procedure ZoomIn;
    {: This method reduces the current visual rect.

       The actual visual rect is halfed on both X and Y directions,
       resulting in a reduction of the current view of the drawing.
    }
    procedure ZoomOut;
    {: This method zooms the drawing so that all of it is visible.

       Because the extension of a drawing depends on the dimension of
       it (2d, 3d and so on), this is an abstract method that must be
       implemented in all derived components (if applicable).
    }
    procedure ZoomToExtension; virtual; abstract;
    {: This method pans the visual rect by a specified ammount in
       both X and Y directions.

       <I=DeltaX> is the ammount of pan in the X direction and
       <I=DeltaY> is the ammount of pan in the Y direction. Both
       of them are specified in view plane coordinates.
    }
    procedure PanWindow(const DeltaX, DeltaY: TRealType);
    procedure PanWindowFraction(const FX, FY: TRealType);
    {: This method start a group of updates so that only one repainting
       of the viewport take place at the end of the group.

       Normally the viewport is repainted as soon as the visual rect is
       changed. If you want to perform a group of such changes to obtain
       a special visual rect, the viewport is repainted a lot of times,
       reducing the performance of your application (specially if the
       drawing is complex). To resolve this problem you may want to call
       this method just before starting to changes to the visual rect, and
       then calling <See Method=TViewport@EndUpdate> to end the group.
       When you call <See Method=TViewport@EndUpdate> the visual rect is
       changed and the viewport is repainted. Only one
       <See Property=TViewport@OnPaint> event is fired.
    }
    procedure BeginUpdate;
    {: This method end a group of updates so that only one repainting
       of the viewport take place at the end of the group.

       Normally the viewport is repainted as soon as the visual rect is
       changed. If you want to perform a group of such changes to obtain
       a special visual rect, the viewport is repainted a lot of times,
       reducing the performance of your application (specially if the
       drawing is complex). To resolve this problem you may want to call
       <See Method=TViewport@BeginUpdate> just before starting to changes to
       the visual rect, and then calling this method to end the group.
       When you call this method the visual rect is
       changed and the viewport is repainted. Only one
       <See Property=TViewport@OnPaint> event is fired.
    }
    procedure EndUpdate;
    {: This method repaints the viewport contents.

       When you call this method the display list, of the <See Class=TDrawing>
       control associated to the viewport, is traversed and the objects contained
       in it are drawed on the viewport's off-screen buffer.

       At the end of the traversion a <See Property=TViewport@OnPaint> event
       is fired.

       The redrawing process cannot be interrupted if you don't use a
       painting thread. If you use it then you can interrupt the process
       by calling <See Method=TViewport@StopRepaint>. However if you
       use a painting thread the process takes a longer time.
    }
    procedure Repaint; override;
    procedure Invalidate; override;
    {: This method refreshes the viewport contents.

       The refresh consist of a copy of the off screen buffer on the
       canvas of the control. This refresh is very useful to remove
       spurious drawings of objects drawed directly on the canvas of
       the control.

       This process is faster than repainting the viewport but
       don't reflect changing in the objects already drawed by a repaint.
    }
    procedure Refresh;
    {: This method repaints a portion of the visual rect.

       <I=ARect> is the portion of the visual rect to be repainted.
       When you call this method the display list, of the <See Class=TDrawing>
       control associated to the viewport, is traversed and the objects contained
       , also partially, in the <I=ARect> portion of the visual rect
       are drawed on the viewport's off-screen buffer.

       At the end of the traversion a <See Property=TViewport@OnPaint> event
       is fired.

       The redrawing process cannot be interrupted if you don't use a
       painting thread. If you use it then you can interrupt the process
       by calling <See Method=TViewport@StopRepaint>. However if you
       use a painting thread the process takes a longer time.
    }
    procedure RepaintRect(const ARect: TRect2D);
    {: This method refreshes a portion of the viewport contents.

       The refresh consist of a copy of the off screen buffer on the
       canvas of the control. This refresh is very useful to remove
       spurious drawings of objects drawed directly on the canvas of
       the control. Only the portion specified by <I=ARect> is
       copied.

       This process is faster than repainting the viewport but
       don't reflect changing in the objects already drawed by a repaint.
    }
    procedure RefreshRect(const ARect: TRect);
    {: This method interrupts a repaint process.

       Only if you are using the painting threads this method is
       able to interrupt a repaint process. Otherwise it does nothing.
    }
    procedure StopRepaint;
    {: Wait for the painting thread to be finished.

       When you use the painting thread to repaint the viewport, you
       may want to wait for it to be finished. This method returns
       only when the active painting thread (if any) is finished.

       If you don't use the painting threads to repaint the viewport
       this method does nothing.
    }
    procedure WaitForRepaintEnd;
    {: This method returns the 2D point in view plane coordinates that
       correspond to the specified point in screen coordinates.

       <I=SPt> is the 2D point in screen coordinates to be
       transformed. It is of type <See Type=TPoint2D> but in fact it
       corresponds to a TPoint. Use <See function=PointToPoint2D>
       function to obtain this value.

       This method is useful to mimics the viewport mapping to the screen.
    }
    function ScreenToViewport(const SPt: TPoint2D): TPoint2D;
      virtual;
    {: This method returns the point in screen coordinates
       that correspond to a specified point in view plane coordinates.

       <I=WPt> is the 2D point in view plane coordinates to be
       transformed. The resulting point is of type <See Type=TPoint2D>
       but in fact it corresponds to a TPoint. Use <See function=Point2DToPoint>
       function to obtain the TPoint value from the result.
    }
    function ViewportToScreen(const WPt: TPoint2D): TPoint2D;
      virtual;
    {: This method returns the width of a square in pixel in view plane coordinates.
       This method is used
       by the <See Method=TViewport2D@PickObject> method.

       <I=L> is the width of the square in pixel that must be trasformed in
       view plane coordinates.
    }
    function GetPixelAperture: TVector2D; virtual;
    function GetAperture(const L: Word): TRealType;
    {: This property contains the <See Class=TDrawing> control that
       acts as the source for the drawing to be painted in the
       viewport.

       The Drawing contains the display list of the drawing that will
       be rendered in the canvas of the viewport control.

       You must assign it before using the viewport.
    }
    //TSY:
    //function GetPixelSizeX: TRealType;
    //function GetPixelSizeY: TRealType;
    property Drawing: TDrawing read fDrawing write SetDrawing;
    {: This property contains the off screen canvas used to
       store the drawing before copying it to the canvas of the
       control.

       By using an off screen buffer the flickering is greatly
       reduced.
    }
    property OffScreenCanvas: TCanvas read
      fOffScreenCanvas;
    {: This property contains the on-screen canvas used to
       draw directly on screen.
    }
    property OnScreenCanvas: TCanvas read
      fOnScreenCanvas;
    property OffScreenDevice: TCanvasDevice read fOffScreenDevice;
    property OnScreenDevice: TCanvasDevice read fOnScreenDevice;
    property RubberDevice: TRubberCanvasDevice read fRubberDevice;
    {: This property contains the mapping transform from the
       view plane coordinate system to the screen coordinate
       system.

       This transform matrix is computed in the
       <See Method=TViewport@BuildViewportTransform> method.
    }
    property ViewportToScreenTransform: TTransf2D read
      GetViewportToScreen;
    {: This property contains the mapping transform from the
       screen coordinate system to the view plane coordinate system.

       This transform matrix is computed as the inverse of the
       transform returned by the
       <See Method=TViewport@BuildViewportTransform> method.
    }
    property ScreenToViewportTransform: TTransf2D read
      GetScreenToViewport;
    {: This property contains the portion of the view plane that is
       rendered in the canvas of the control.

       Only the objects contained in this portion of the plane are
       drawed on the screen (by using clipping).
    }
    property VisualRect: TRect2D read fVisualWindow write
      ZoomWindow;
    {: This property is <B=True> when the viewport is inside an
       update block.

       See also <See Method=TViewport@BeginUpdate> and
       <See Method=TViewport@EndUpdate>.
    }
    property InUpdating: Boolean read fInUpdate;
    {: This property is <B=True> when the viewport is traversing the
       diplay list.

       When you call the <See Method=TViewport@Repaint> method this
       property becames <B=True>.
    }
    property InRepainting: Boolean read GetInRepaint;
    {: This property is used to inhibit the mouse events fired by the
       viewport.

       If it is <B=False> (the default) the mouse event are fired by
       the control, otherwise they are not.
    }
    property DisableMouseEvents: Boolean read fDisableMouseEvents
      write fDisableMouseEvents;
    {: This property is used to inhibit the repaint event fired by the
       viewport when it has repainted.

       If it is <B=False> (the default) the <See Property=TViewport@OnPaint>
       event is fired when the repaint process is finished, otherwise it is not.
    }
    property DisableRepaintEvents: Boolean read
      fDisablePaintEvent write fDisablePaintEvent;
  published
    { Published declarations }
    property Align;
    property Enabled;
    property Visible;
    property PopupMenu;
    property Height default 50;
    property Width default 50;
    {: This property contains the filling color of the control
       points.

       By default it is <B=clRed>.
    }
    property ControlPointsColor: TColor read fControlPointsColor
      write fControlPointsColor default clWhite;
    property ControlPointsPenColor: TColor read
      fControlPointsPenColor write fControlPointsPenColor default
      clNavy;

    {: This property contains the color of the background of the viewport.

       By default it is <B=clWhite>.
    }
    property BackGroundColor: TColor read fBackGroundColor write
      SetBackColor default clWhite;
    {: This property contains the color of reference grid of the viewport.

       By default it is <B=clMoneyGreen>.
    }
    property GridColor: TColor read fGridColor write SetGridColor
      default clMoneyGreen;
    {: This property specify the Z-order of the grid respect to
       the drawing. If it is false (the default) the grid will be
       drawed before any other shape and so will be covered by
       the drawing, otherwise it will be on top.
    }
    property GridOnTop: Boolean read fGridOnTop write fGridOnTop
      default False;
    {: This property contains the main step of the reference grid along
    }
    property GridStep: TRealType read fGridStep write fGridStep;
    {: An object can have control points. This property sets the
       dimension in pixels of these control points when they are
       drawed.
    }
    property ControlPointsWidth: Byte read fControlPointsWidth
      write fControlPointsWidth default 7;
    {: An object can have control points. If this property is <B=True> the
       control points are drawed with the object (if it has them).
    }
    property ShowControlPoints: Boolean read fShowControlPoints
      write fShowControlPoints default False;
    {: If this property is <B=True> the reference grid of the viewport
       will be drawed.
    }
    property ShowGrid: Boolean read fShowGrid write SetShowGrid
      default False;
    {: If this property is <B=True> a painting thread is used for
       the repaint process.

       The library can use a thread for the traversing of display list.
       Such a painting thread is a specilized process that runs parallel to
       the main thread of the application. A viewport can have only one
       running thread that can be interrupted at any moment by calling the
       <See Method=TViewport@StopRepaint>.

       Different viewports can have their own threads running
       concurrently, fasting the repainting process. They are also useful
       to allow the user to change the view parameters dinamically in
       real time.

       However when a painting thread is used, the repaint process take
       more time than if the thread use is disabled. For this the use
       of a painting thread is advisable only when there are more than
       an hundred of object to be drawed.

       By default it is <B=False>.
    }
    property UsePaintingThread: Boolean read fUseThread write
      fUseThread default False;
    {: If this property is <B=True> the the viewport will be transparent.

       By default it is <B=False>.

      <B=Note>: the transparency is avaiable only at run-time.
    }
    property IsTransparent: Boolean read fTransparent write
      SetTransparent default False;
    {: This property contains the number of objects that are drawed
       before the off screen buffer is copied onto the canvas of the
       control.

       By default its value is 100.

       Lower values make the repainting process slower.
    }
    property CopingFrequency: Integer read fCopingFrequency write
      fCopingFrequency default 0;
    {: This property may contain an event handler that is called
       just before a repaint operation is being started.
    }
    property OnBeginRedraw: TNotifyEvent read fOnBeginRedraw
      write fOnBeginRedraw;
    {: This property may contain an event handler that is called
       called after a repaint is finished (after the coping of the back buffer
       onto the on screen canvas).

       This event is fired before the <See Property=TViewport@OnPaint> event.
    }
    property OnEndRedraw: TNotifyEvent read fOnEndRedraw write
      fOnEndRedraw;
    {: This property may contain an event handler that is called
       after the back buffer is copied onto the on screen canvas.

       This event is fired after the <See Property=TViewport@OnEndRedraw>
       event.
    }
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;
    {: This property may contais an event handler that is called
       when the dimensions of the control are changed (and the
       off screen buffer is changed accordingly).

       The event is fired after the dimensions and the mapping
       trasforms are updated but before the repaint process take
       in place (and so before the
       <See Property=TViewport@OnPaint> event).
    }
    property OnResize: TNotifyEvent read fOnResize write
      fOnResize;
    {: This property may contains an event handler that is called when
       the view mapping transform is changed.

       This event is fired before the <See Property=TViewport@OnPaint> event.

       See also <See Method=TViewport@BuildViewportTransform>.
    }
    property OnViewMappingChanged: TNotifyEvent read
      fOnViewMappingChanged write fOnViewMappingChanged;
    property OnEnter;
    property OnExit;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write
      fOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write
      fOnMouseLeave;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

{: This type defines the possible orientations for a <See Class=TRuler>
   control.

   There are two types of orientations:

   <LI=<I=otHorizontal> the ruler is alligned horizontally.>
   <LI=<I=otVertical> the ruler is alligned vertically.>

   The horizontal ruler is drawed on the left side of the linked
   Viewport and the vertical ruler is drawed on the down side of
   the linked Viewport.
}
  TRulerOrientationType = (otHorizontal, otVertical);
  {: This class defines a ruler that can be linked to a Viewport to
     show the extension of its <See Property=TViewport@VisualRect>.

     The components isn't updated automatically, instead it defines
     methods that must be called from the application to keep it
     syncronized with the viewport.

     Normally the following events are used for updating the ruler:

     <LI=<I=OnEndRepaint> is used to redraw the ruler and to update
     its dimensions.>
     <LI=<I=OnMouseMove> is used to update the mark position on the
     ruler <See Method=TRuler@SetMark>.>
  }
  TRuler = class(TCustomControl)
  private
    fOwnerView: TViewport;
    fFontSize, FSize: Integer;
    fOrientation: TRulerOrientationType;
    fTicksColor: TColor;

    procedure SetTicksColor(C: TColor);
    procedure SetOwnerView(V: TViewport);
    procedure SetOrientation(O: TRulerOrientationType);
    procedure SetSize(S: Integer);
    procedure SetFontSize(S: Integer);
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message
      WM_ERASEBKGND;
  public
    {: This is the constructor of the control.

       It creates a vertical white ruler with size of 20 and
       a step division of 5.
    }
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    {: This method is used to move the position marker of the ruler.

       A ruler may have a position marker that is used to show the
       current viewport position on the ruler.

       <I=Value> is the position on the ruler of the mouse in view
       plane reference coordinate. The value to be passed depends
       on the orientation of the ruler. For example a vertical ruler
       may want to show the Y coordinates of the current mouse
       position on the view plane.
    }
    procedure SetMark(Value: TRealType);
  published
    property Align;
    {: This property contains the linked viewport used for the alignment
       of the ruler

       The ruler adjust its range by using the <See Property=TViewport@VisualRect>
       property.
    }
    property LinkedViewport: TViewport read fOwnerView write
      SetOwnerView;
    {: This property contains the background color used to paint the
       ruler background.

       By default its value is clWhite.
    }
    property Color default clWhite;
    {: This property contains the color of the thick mark used to
       show the step division of the ruler.

       By default it is clBlack.
    }
    property TicksColor: TColor read fTicksColor write
      SetTicksColor default clBlack;
    {: This property contains the orientation of the ruler.

       See <See Type=TRulerOrientationType> for details.

       By default it is <I=otVertical>.
    }
    property Orientation: TRulerOrientationType read fOrientation
      write SetOrientation default otVertical;
    {: This property contains the width (height) of the vertical
       (horizontal) ruler.

       If the value is small it will be difficulty to read the values
       of the divisions.

       By default it is 20.
    }
    property Size: Integer read FSize write SetSize default 20;
    {: This property contains the size of the font used to show
       the values of the divisions.

       By default it is 6.
    }
    property FontSize: Integer read fFontSize write SetFontSize
      default 6;
  end;

  {: This component derives from <See Class=TViewport> and
     specialize it to handle 2D objects.

     In this case the world is the view plane of TViewport
     and so the projection transform is simply an indentity tranform matrix.

     See <See Class=TViewport> for details.
  }
  TViewport2D = class(TViewport)
  private
      { consider only the objects with this type during the picking. }
    fDrawing2D: TDrawing2D;
    { Event handlers }
    fOnMouseDown2D, fOnMouseUp2D: TMouseEvent2D;
    fOnMouseMove2D: TMouseMoveEvent2D;
    FOnMouseWheel: TMouseWheelEvent;
    fImageList: TImageList;
    fBrushBitmap: TBitmap;
    fCursorColor: TColor;
    fLastCursorPos: TPoint2D;
    fLastMousePos: TPoint2D;

    procedure SetDrawing2D(ADrawing2D: TDrawing2D);
  protected
    procedure SetDrawing(ADrawing: TDrawing); override;
//TSY:
    procedure DrawObjectControlPoints(
      const Obj: TGraphicObject; const Dvc: TDevice;
      const ClipRect2D: TRect2D);
      override;
    function BuildViewportTransform(var ViewWin: TRect2D; const
      ScreenWin: TRect): TTransf2D; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseDown(Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta:
      Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Repaint; override;
    procedure DrawObject(const Obj: TGraphicObject;
      const Dvc: TCanvasDevice;
      const ClipRect2D: TRect2D); override;
    {: This method draws a 2D object on the viewport.

       <I=Obj> is that object to be drawed. If <I=CtrlPts> is <B=True>
       the control points of the object will also be drawed.

       The object is drawed by calling its <See Method=TObject2D@Draw>
       method if it is visible (that is it is contained in the
       <See Property=TViewport@VisualRect>)).
    }
    procedure DrawObject2D(const Obj: TObject2D; const CtrlPts:
      Boolean);
    procedure DrawObject2DWithRubber(
      const Obj: TGraphicObject; Transf: TTransf2D);
    procedure ZoomToExtension; override;
    {: This method returns the point <I=WPt> in world coordinate system
       transformed by the inverse of the object model trasform of <I=Obj>.

       This method simply obtain the point <I=WPt> referenced in
       the object coordinate system of <I=Obj>.
    }
    function WorldToObject(const Obj: TObject2D; WPt: TPoint2D):
      TPoint2D;
    {: This method returns the point <I=OPt> in object coordinate system
       transformed by the object model trasform of <I=Obj>.

       This method simply obtain the point <I=OPt> referenced in
       the world coordinate system.
    }
    function ObjectToWorld(const Obj: TObject2D; Opt: TPoint2D):
      TPoint2D;
    function GetSnappedPoint(P: TPoint2D): TPoint2D;
  published
    property ImageList: TImageList read fImageList write
      fImageList;
    {: This property contains the <See Class=TDrawing2D> control that
       acts as the source for the drawing to be painted in the
       viewport.

       The Drawing2D contains the display list of the drawing that will
       be rendered in the canvas of the viewport control.

       You must assign it before using the viewport.
    }
    property Drawing2D: TDrawing2D read fDrawing2D write
      SetDrawing2D;
    {: EVENTS}
    {: This property may contain an event-handler that will be
       called when the mouse in moved on the control.

       See <See Type=TMouseMoveEvent2D> for details on the event
       handler.
    }
    property OnMouseMove2D: TMouseMoveEvent2D read fOnMouseMove2D
      write fOnMouseMove2D;
    {: This property may contain an event-handler that will be
       called when the user presses a mouse button with the mouse pointer
       over the viewport.

       See <See Type=TMouseEvent2D> for details on the event
       handler.
    }
    property OnMouseDown2D: TMouseEvent2D read fOnMouseDown2D
      write fOnMouseDown2D;
    {: This property may contain an event-handler that will be
       called when the user releases a mouse button with the mouse pointer
       over the viewport.

       See <See Type=TMouseEvent2D> for details on the event
       handler.
    }
    property OnMouseUp2D: TMouseEvent2D read fOnMouseUp2D write
      fOnMouseUp2D;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel
      write FOnMouseWheel;
    procedure DrawCursorCross(P: TPoint2D; Visible: Boolean);
  end;

// Prepare canvas device for drawing
procedure SetupCanvasDevice(Drawing: TDrawing2D;
  Dvc: TCanvasDevice; const T: TTransf2D);

implementation

uses Math, GObjects, SysBasic;

procedure SetupCanvasDevice(Drawing: TDrawing2D;
  Dvc: TCanvasDevice; const T: TTransf2D);
begin
  Dvc.T := T;
  if Drawing.PicScale <= 0 then Drawing.PicScale := 1;
  Dvc.FactorMM := Dvc.TScale / Drawing.PicScale;
  if Drawing.FontName <> '' then
    Dvc.FaceName := Drawing.FontName
  else if FontName_Default <> '' then
    Dvc.FaceName := FontName_Default
  else
    Dvc.FaceName := 'Times New Roman';
  Dvc.LineWidthBase := Drawing.LineWidthBase;
  Dvc.MiterLimit := Drawing.MiterLimit;
  Dvc.HatchingLineWidth := Drawing.HatchingLineWidth;
  Dvc.HatchingStep := Drawing.HatchingStep;
end;

type
  TPaintingThread = class(TThread)
  private
    { Private declarations }
    fOwner: TViewport;
    fRect: TRect2D;
  public
    // Assign also the OnTerminate event.
    constructor Create(const Owner: TViewport; const ARect:
      TRect2D);
    destructor Destroy; override;

    procedure Execute; override;
  end;

// =====================================================================
// TPaintingThread
// =====================================================================

constructor TPaintingThread.Create(const Owner: TViewport;
  const ARect: TRect2D);
begin
  inherited Create(True); // Sempre disattivo alla partenza.

  FreeOnTerminate := True;
  fOwner := Owner;
  fRect := ARect;
end;

destructor TPaintingThread.Destroy;
begin
  DoTerminate;
  inherited;
end;

procedure TPaintingThread.Execute;
var
  Tmp: TGraphicObject;
  TmpIter: TGraphicObjIterator;
  I: Integer;
  TmpCanvas: TCanvas;
  TmpClipRect: TRect2D;
begin
  if not Assigned(fOwner) then
    Exit;
  with fOwner do
  try
    if fOwner is TViewport2D then
      with fOwner as TViewport2D do
      begin
        SetupCanvasDevice(fDrawing2D, fOffScreenDevice,
          fViewportToScreen);
        SetupCanvasDevice(fDrawing2D, fOnScreenDevice,
          fViewportToScreen);
        SetupCanvasDevice(fDrawing2D, fRubberDevice,
          fViewportToScreen);
      end;
    TmpCanvas := OffScreenCanvas;
    TmpClipRect := RectToRect2D(ClientRect);
    TmpCanvas.Lock;
    try
      if fShowGrid and not fGridOnTop then
        DrawGrid(fRect, TmpCanvas);
      if not Assigned(fDrawing) or fDrawing.IsBlocked then
        Exit
      else
        TmpIter := fDrawing.GetListOfObjects;
      try
        Tmp := TmpIter.First;
        I := 0;
        if fCopingFrequency > 0 then
          while Tmp <> nil do
          begin
            DrawObject(Tmp, OffScreenDevice, TmpClipRect);
            Inc(I);
            if I >= fCopingFrequency then
            begin
              Synchronize(DoCopyCanvasThreadSafe);
              I := 0;
            end;
            Tmp := TmpIter.Next;
            if Terminated then
              Break;
          end
        else
          while Tmp <> nil do
          begin
            DrawObject(Tmp, OffScreenDevice, TmpClipRect);
            Tmp := TmpIter.Next;
            if Terminated then
              Break;
          end;
      finally
        TmpIter.Free;
      end;
      if fShowGrid and fGridOnTop then
        DrawGrid(fRect, TmpCanvas);
    finally
      TmpCanvas.Unlock;
    end;
  except
  end;
  Synchronize(fOwner.DoCopyCanvasThreadSafe);
end;

// =====================================================================
// TViewport
// =====================================================================

procedure TViewport.CopyBitmapOnCanvas(const DestCnv:
  TCanvas; const BMP: TBitmap; IRect: TRect; IsTransparent:
  Boolean; TransparentColor: TColor);
begin
{$IFDEF LINUX}
  DestCnv.Pen.Mode := pmCopy;
  DestCnv.Copymode := 0; //??
{$ELSE}
{$ENDIF}
  if (IsTransparent and
    not (csDesigning in ComponentState)) then
  begin
    DestCnv.Brush.Style := bsClear;
{$IFDEF VER140}
    DestCnv.BrushCopy(IRect, BMP, IRect, TransparentColor);
{$ELSE}
    DestCnv.CopyRect(IRect, BMP.Canvas, IRect);
{$ENDIF}
  end
  else
  begin
    DestCnv.CopyRect(IRect, BMP.Canvas, IRect);
{$IFDEF LINUX}
    DestCnv.CopyRect(IRect, BMP.Canvas, IRect);
{$ELSE}
{$ENDIF}
  end;
end;

procedure TViewport.Control_Point(const P: TPoint2D;
  const VT: TTransf2D; const ClipRect: TRect2D);
begin
  CnvDrawControlPoint(fOffScreenCanvas,
    TransformPoint2D(P, VT),
    ControlPointsWidth);
end;

procedure TViewport.Control_Point2(const P: TPoint2D;
  const VT: TTransf2D; const ClipRect: TRect2D);
begin
  CnvDrawRoundControlPoint(fOffScreenCanvas,
    TransformPoint2D(P, VT),
    ControlPointsWidth);
end;

procedure TViewport.Control_Point3(const P: TPoint2D;
  const VT: TTransf2D; const ClipRect: TRect2D);
begin
  CnvDrawControlPoint3(fOffScreenCanvas,
    TransformPoint2D(P, VT),
    ControlPointsWidth);
end;

procedure TViewport.Control_Line(const P0, P1: TPoint2D;
  const VT: TTransf2D; const ClipRect: TRect2D);
begin
  CnvDrawLine(fOffScreenCanvas,
    TransformPoint2D(P0, VT),
    TransformPoint2D(P1, VT));
end;

procedure TViewport.Control_Box(const R: TRect2D;
  const VT: TTransf2D; const ClipRect: TRect2D);
begin
  DrawBoundingBox2D(fOffScreenCanvas, R, ClipRect, VT);
end;

procedure TViewport.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
  begin
    Style := Style and not ({CS_HREDRAW} DWORD(2) or {CS_VREDRAW}
      DWORD(1));
    if fTransparent and not (csDesigning in ComponentState) then
    begin
      Style := Style and not {WS_CLIPCHILDREN}  $2000000;
      Style := Style and not {WS_CLIPSIBLINGS}  $4000000;
      Params.ExStyle := Params.ExStyle or {WS_EX_TRANSPARENT}  $20;
    end;
  end;
end;

procedure TViewport.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if (not fTransparent) or
    (csDesigning in ComponentState) then
    inherited
  else
    Message.Result := 1;
end;

procedure TViewport.WMSize(var Message: TWMSize);
begin
  inherited;
  DoResize;
end;

procedure TViewport.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(fOnMouseEnter) then
    fOnMouseEnter(Self);
end;

procedure TViewport.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(fOnMouseLeave) then
    fOnMouseLeave(Self);
end;

procedure TViewport.SetBackColor(const Cl: TColor);
begin
  if fBackGroundColor <> Cl then
  begin
    StopRepaint;
    fBackGroundColor := Cl;
    Repaint;
  end;
end;

procedure TViewport.SetTransparent(const B: Boolean);
begin
  if (fTransparent <> B) then
  begin
    StopRepaint;
    fTransparent := B;
    Repaint;
  end;
end;

procedure TViewport.SetShowGrid(const B: Boolean);
begin
  if fShowGrid <> B then
  begin
    StopRepaint;
    fShowGrid := B;
    Repaint;
  end;
end;

procedure TViewport.SetGridColor(const Cl: TColor);
begin
  if fGridColor <> Cl then
  begin
    StopRepaint;
    fGridColor := Cl;
    Repaint;
  end;
end;

procedure TViewport.SetDrawing(ADrawing: TDrawing);
begin
  if ADrawing <> fDrawing then
  begin
    StopRepaint;
    if Assigned(ADrawing) and not (csDesigning in ComponentState)
      then
    begin
      if Assigned(fDrawing) then
        fDrawing.DelViewports(Self);
      ADrawing.AddViewports(Self);
    end;
    fDrawing := ADrawing;
  end;
end;

procedure TViewport.ClearCanvas(Sender: TObject; Cnv:
  TCanvas; const ARect: TRect2D; const BackCol: TColor);
begin
  with Cnv do
  begin
    Brush.Color := BackCol;
    Brush.Style := bsSolid;
    with TransformRect2D(ARect, fViewportToScreen) do
      FillRect(Rect(Round(Left), Round(Top), Round(Right) + 1,
        Round(Bottom) + 1));
  end;
end;

procedure TViewport.DoCopyCanvas(const GenEvent: Boolean);
begin
  CopyBackBufferRectOnCanvas(Rect(0, 0, Width, Height),
    GenEvent);
end;

procedure TViewport.DoCopyCanvasThreadSafe;
begin
  CopyBackBufferRectOnCanvas(Rect(0, 0, Width, Height), False);
end;

procedure TViewport.CopyBackBufferRectOnCanvas(const Rect:
  TRect; const GenEvent: Boolean);
begin
  if HandleAllocated then
  begin
    fOffScreenCanvas.Lock;
    try
      CopyBitmapOnCanvas(Canvas, fOffScreenBitmap, Rect,
        fTransparent, fBackGroundColor);
    finally
      fOffScreenCanvas.Unlock;
    end;
  end;
  if Assigned(fOnPaint) and GenEvent and (not fDisablePaintEvent)
    then
  try
    fDisablePaintEvent := True;
    fOnPaint(Self);
  finally
    fDisablePaintEvent := False;
  end;
end;

constructor TViewport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fViewGuard := TTpXCriticalSection.Create;
  ControlStyle := ControlStyle - [csOpaque];
  ControlStyle := ControlStyle + [csClickEvents, csSetCaption,
    csDoubleClicks {TSY}];
  fPaintingThread := nil;
  CopingFrequency := 0;
  Height := 50;
  Width := 50;
  fBackGroundColor := clWhite;
  fGridColor := clMoneyGreen;
  fShowGrid := False;
  fTransparent := False;
  fInUpdate := False;
  fGridStep := 0.0;
  fSnapStep := 10;
  fDisableMouseEvents := False;
  fDrawing := nil;
  fShowControlPoints := False;
  fControlPointsWidth := 7;
  fVisualWindow := Rect2D(0.0, 0.0, 100.0, 100.0);
  fScreenToViewport := IdentityTransf2D;
  fViewportToScreen := IdentityTransf2D;
  fUseThread := False;
  fGridOnTop := False;
  fOffScreenBitmap := TBitmap.Create;
  fOffScreenBitmap.Height := Height;
  fOffScreenBitmap.Width := Width;
{$IFDEF VER140}
  fOffScreenBitmap.PixelFormat := pf24bit;
{$ELSE}
  //fOffScreenBitmap.PixelFormat := pf4bit;
  //MessageBoxInfo(IntToStr(Ord(fOffScreenBitmap.PixelFormat)));
  //pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom
{$ENDIF}
  fOffScreenCanvas := fOffScreenBitmap.Canvas;
  fOnScreenCanvas := Canvas;
  fOffScreenDevice := TCanvasDevice.Create;
  fOffScreenDevice.Cnv := fOffScreenCanvas;
  fOnScreenDevice := TCanvasDevice.Create;
  fOnScreenDevice.Cnv := fOnScreenCanvas;
  fRubberDevice := TRubberCanvasDevice.Create;
  fRubberDevice.Cnv := fOnScreenCanvas;
  //TSY: added
  //fControlPointsColor := clSilver;
  fControlPointsColor := clWhite;
  ShowRulers := False;
end;

destructor TViewport.Destroy;
begin
  StopPaintingThread;
  if Assigned(fDrawing) then
    fDrawing.DelViewports(Self);
  fViewGuard.Free;
  fOffScreenBitmap.Free;
  fOffScreenDevice.Free;
  fOnScreenDevice.Free;
  fRubberDevice.Free;
  inherited Destroy;
end;

procedure TViewport.BeginUpdate;
begin
  StopRepaint;
  fInUpdate := True;
end;

procedure TViewport.EndUpdate;
begin
  fInUpdate := False;
  Repaint;
end;

procedure TViewport.Paint;
begin
  if fPaintingThread = nil then
    DoCopyCanvas(True);
end;

procedure TViewport.RefreshRect(const ARect: TRect);
var
  TmpRect: TRect;
begin
  if fPaintingThread <> nil then
    Exit;
  TmpRect := Rect(ARect.Left - 1, ARect.Top - 1, ARect.Right +
    1, ARect.Bottom + 1);
  CopyBackBufferRectOnCanvas(TmpRect, True);
end;

procedure TViewport.DoResize;
begin
  StopRepaint;
  fOffScreenBitmap.Height := Height;
  fOffScreenBitmap.Width := Width;
  ChangeViewportTransform(fVisualWindow);
  if Assigned(fOnResize) then
    fOnResize(Self);
end;

procedure TViewport.StopPaintingThread;
begin
  if Assigned(fPaintingThread) then
  try
    TPaintingThread(fPaintingThread).Terminate;
    if Assigned(fPaintingThread) then
      TPaintingThread(fPaintingThread).WaitFor;
    fPaintingThread := nil;
    if Assigned(fOnPaint) and (not fDisablePaintEvent) then
    try
      fDisablePaintEvent := True;
      fOnPaint(Self);
    finally
      fDisablePaintEvent := False;
    end;
  finally
    fPaintingThread := nil;
  end;
end;

procedure TViewport.OnThreadEnded(Sender: TObject);
begin
  fPaintingThread := nil;
  if Assigned(fOnPaint) and (not fDisablePaintEvent) then
  try
    fDisablePaintEvent := True;
    fOnPaint(Self);
  finally
    fDisablePaintEvent := False;
  end;
  if Assigned(fOnEndRedraw) then
    fOnEndRedraw(Self);
end;

procedure TViewport.UpdateViewport(const ARect: TRect2D);
var
  Tmp: TGraphicObject;
  TmpIter: TGraphicObjIterator;
  TmpCanvas: TCanvas;
  TmpClipRect: TRect2D;
  I: Integer;
begin
  if fInUpdate then
    Exit;
  StopRepaint;
  if Self is TViewport2D then
    with Self as TViewport2D do
    begin
      SetupCanvasDevice(fDrawing2D, fOffScreenDevice,
        fViewportToScreen);
      SetupCanvasDevice(fDrawing2D, fOnScreenDevice,
        fViewportToScreen);
      SetupCanvasDevice(fDrawing2D, fRubberDevice,
        fViewportToScreen);
    end;
  ClearCanvas(Self, fOffScreenCanvas, ARect,
    fBackGroundColor);
  if Assigned(fOnBeginRedraw) then
    fOnBeginRedraw(Self);
  if fUseThread then
  begin
    fPaintingThread := TPaintingThread.Create(Self, ARect);
    TPaintingThread(fPaintingThread).OnTerminate :=
      OnThreadEnded;
    TPaintingThread(fPaintingThread).Resume;
  end
  else
  try
    try
      TmpCanvas := fOffScreenCanvas;
      TmpClipRect := RectToRect2D(ClientRect);
      TmpCanvas.Lock;
      fDrawing.OnControlPoint := Control_Point;
      fDrawing.OnControlPoint2 := Control_Point2;
      fDrawing.OnControlPoint3 := Control_Point3;
      fDrawing.OnControlLine := Control_Line;
      fDrawing.OnControlBox := Control_Box;
      try
        if fShowGrid and not fGridOnTop then
          DrawGrid(ARect, TmpCanvas);
        if not Assigned(fDrawing)
          or fDrawing.IsBlocked then
          Exit
        else
          TmpIter := fDrawing.GetListOfObjects;
        try
          Tmp := TmpIter.First;
          I := 0;
          if fCopingFrequency > 0 then
            while Tmp <> nil do
            begin
              DrawObject(Tmp, fOffScreenDevice, TmpClipRect);
              Inc(I);
              if I = fCopingFrequency then
              begin
                DoCopyCanvas(False);
                I := 0;
              end;
              Tmp := TmpIter.Next;
            end
          else
            while Tmp <> nil do
            begin
              DrawObject(Tmp, fOffScreenDevice, TmpClipRect);
              Tmp := TmpIter.Next;
            end;
        //TSY:
          Tmp := TmpIter.First;
          while Tmp <> nil do
          begin
            if Tmp is TObject2D then
              if (Tmp as TObject2D).HasControlPoints then
                DrawObjectControlPoints(
                  Tmp, fOffScreenDevice, TmpClipRect);
            Tmp := TmpIter.Next;
          end;
        finally
          TmpIter.Free;
        end;
        if fShowGrid and fGridOnTop then
          DrawGrid(ARect, TmpCanvas);
      finally
        TmpCanvas.Unlock;
      end;
    except
    end;
  finally
    DoCopyCanvas(True);
    if Assigned(fOnEndRedraw) then
      fOnEndRedraw(Self);
  end;
end;

procedure TViewport.RepaintRect(const ARect: TRect2D);
begin
  UpdateViewport(ARect);
end;

function RoundGridStep(D: TRealType): TRealType;
begin
  if D = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Result := Power(10, Floor(Log10(D)));
  D := D / Result;
  if D > 7 then
    Result := 10 * Result
  else if D > 2 then
    Result := 5 * Result;
end;

function TViewport2D.GetSnappedPoint(P: TPoint2D): TPoint2D;
begin
  Result := P;
  if not Drawing2D.UseSnap then Exit;
  if fSnapStep <> 0 then
  begin
    Result.X := Round(Result.X / fSnapStep) * fSnapStep;
    Result.Y := Round(Result.Y / fSnapStep) * fSnapStep;
    Result.W := 1.0;
  end;
  //if Assigned(fSnapFilter) then    fSnapFilter(Self, CurrentState, fSnapOriginPoint, Result);
end;

procedure TViewport.DrawGrid(const ARect: TRect2D;
  const Cnv: TCanvas);
var
  P1, P2: TPoint2D;
  IX1, IX2, IY1, IY2: Integer;
  procedure DrawGridPoints(const Cnv: TCanvas;
    const D: TRealType);
  var
    IX, IY: Integer;
  begin
    IX1 := Ceil(ARect.Left / D);
    IX2 := Floor(ARect.Right / D);
    IY1 := Ceil(ARect.Bottom / D);
    IY2 := Floor(ARect.Top / D);
    for IX := IX1 to IX2 do
    begin
      for IY := IY1 to IY2 do
      begin
        P1 := ViewportToScreen(Point2D(IX * D, IY * D));
        Cnv.Pixels[Round(P1.X), Round(P1.Y)] := clMedGray;
      end;
    end;
  end;
  procedure DrawGridLines(const Cnv: TCanvas;
    const D: TRealType);
  var
    I: Integer;
  begin
    IX1 := Ceil(ARect.Left / D);
    IX2 := Floor(ARect.Right / D);
    IY1 := Ceil(ARect.Bottom / D);
    IY2 := Floor(ARect.Top / D);
    for I := IY1 to IY2 do
    begin
      P1 := ViewportToScreen(Point2D(ARect.Left, I * D));
      P2 := ViewportToScreen(Point2D(ARect.Right, I * D));
      CnvDrawLine(Cnv, P1, P2);
    end;
    for I := IX1 to IX2 do
    begin
      P1 := ViewportToScreen(Point2D(I * D, ARect.Top));
      P2 := ViewportToScreen(Point2D(I * D, ARect.Bottom));
      CnvDrawLine(Cnv, P1, P2);
    end;
  end;
begin
  if fGridStep <= 0 then
    fSnapStep := RoundGridStep(GetPixelAperture.X * 12)
  else
    fSnapStep := fGridStep;
  with Cnv do
  begin
{$IFDEF VER140}
    SetBkMode(Canvas, False);
{$ELSE}
    SetBkMode(Canvas.Handle, 0);
{$ENDIF}
    Pen.Color := fGridColor;
    Pen.Width := 1;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Brush.Color := fBackGroundColor;
    Brush.Style := bsSolid;
    // Draw the grid main divisions
    DrawGridLines(fOffScreenCanvas,
      RoundGridStep(GetPixelAperture.X * 75));
    // Draw the grid points
    DrawGridPoints(fOffScreenCanvas, fSnapStep);
    Pen.Style := psSolid;
    // Draw the main axes.
    Pen.Width := 2;
    P1 := ViewportToScreen(Point2D(ARect.Left, 0.0));
    P2 := ViewportToScreen(Point2D(ARect.Right, 0.0));
    CnvDrawLine(Cnv, P1, P2);
    P1 := ViewportToScreen(Point2D(0.0, ARect.Bottom));
    P2 := ViewportToScreen(Point2D(0.0, ARect.Top));
    CnvDrawLine(Cnv, P1, P2);
  end;
end;

function TViewport.GetInRepaint: Boolean;
begin
  Result := Assigned(fPaintingThread);
end;

procedure TViewport.Repaint;
begin
  if (csReadingState in ControlState) then
    Exit;
  if fTransparent then
    Parent.Repaint;
  RepaintRect(fVisualWindow);
end;

procedure TViewport.WaitForRepaintEnd;
begin
  if Assigned(fPaintingThread) then
  try
     // Devo aspettare.
    TPaintingThread(fPaintingThread).WaitFor;
  finally
    fPaintingThread := nil;
  end;
end;

procedure TViewport.StopRepaint;
begin
  StopPaintingThread;
end;

procedure TViewport.Refresh;
begin
  Invalidate;
end;

procedure TViewport.Invalidate;
begin
  if (csReadingState in ControlState) or
    (csLoading in ComponentState) then
  begin
    inherited;
    Exit;
  end;
  if (csDesigning in ComponentState) then
    inherited;
  if HandleAllocated then
    Paint;
end;

procedure TViewport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = Drawing) and (Operation = opRemove) then
  begin
    StopPaintingThread;
    Drawing := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TViewport.ChangeViewportTransform(ViewWin:
  TRect2D);
var
  OldView: TRect2D;
begin
  StopRepaint;
  ViewWin := ReorderRect2D(ViewWin);
  OldView := fVisualWindow;
  if (ViewWin.Bottom = ViewWin.Top) or (ViewWin.Right =
    ViewWin.Left) then
    Exit;
  try
    fViewGuard.Enter;
    try
      fVisualWindow := ViewWin;
      UpdateViewportTransform;
    finally
      fViewGuard.Leave;
    end;
  except
    on Exception do
    begin
      fVisualWindow := OldView;
      try
        UpdateViewportTransform;
      except
      end;
    end;
  end;
end;

procedure TViewport.UpdateViewportTransform;
var
  NewTransf: TTransf2D;
begin
  StopRepaint;
  try
    NewTransf := BuildViewportTransform(fVisualWindow, ClientRect);
    fScreenToViewport := InvertTransform2D(NewTransf);
    fViewportToScreen := NewTransf;
    if Assigned(fOnViewMappingChanged) then
      fOnViewMappingChanged(Self);
  except
  end;
  Repaint;
end;

function TViewport.BuildViewportTransform(var ViewWin:
  TRect2D; const ScreenWin: TRect): TTransf2D;
begin
  Result := IdentityTransf2D;
end;

procedure TViewport.ZoomWindow(const NewWindow: TRect2D);
begin
  ChangeViewportTransform(NewWindow);
end;

function TViewport.GetAperture(const L: Word): TRealType;
begin
  Result := GetPixelAperture.X * L;
end;

procedure TViewport.MoveWindow(const NewStartX, NewStartY:
  TRealType);
var
  TmpWin: TRect2D;
  Temp: TRealType;
begin
  StopRepaint;
  Temp := (fVisualWindow.Right - fVisualWindow.Left);
  TmpWin.Left := NewStartX;
  TmpWin.Right := NewStartX + Temp;
  TmpWin.W1 := 1.0;
  Temp := fVisualWindow.Top - fVisualWindow.Bottom;
  TmpWin.Bottom := NewStartY;
  TmpWin.Top := NewStartY + Temp;
  TmpWin.W2 := 1.0;
  ChangeViewportTransform(TmpWin);
end;

procedure TViewport.ZoomCenter(const C: TPoint2D; const F:
  TRealType);
var
  TmpWin: TRect2D;
begin
  StopRepaint;
  TmpWin.Left := fVisualWindow.Left * F + C.X * (1 - F);
  TmpWin.Right := fVisualWindow.Right * F + C.X * (1 - F);
  TmpWin.W1 := 1.0;
  TmpWin.Top := fVisualWindow.Top * F + C.Y * (1 - F);
  TmpWin.Bottom := fVisualWindow.Bottom * F + C.Y * (1 - F);
  TmpWin.W2 := 1.0;
  ChangeViewportTransform(TmpWin);
end;

procedure TViewport.ZoomFrac(const FX, FY: TRealType; const F:
  TRealType);
var
  C: TPoint2D;
begin
  C := Point2D(
    fVisualWindow.Left * (1 - FX) + fVisualWindow.Right * FX,
    fVisualWindow.Bottom * (1 - FY) + fVisualWindow.Top * FY);
  ZoomCenter(C, F);
end;

procedure TViewport.ZoomSelCenter(const F: TRealType);
var
  C: TPoint2D;
begin
  C := Drawing.GetSelectionCenter;
  ZoomCenter(C, F);
end;

procedure TViewport.ZoomViewCenter(const F: TRealType);
begin
  ZoomFrac(0.5, 0.5, F);
end;

procedure TViewport.ZoomIn;
begin
  ZoomSelCenter(1 / Sqrt(2));
end;

procedure TViewport.ZoomOut;
begin
  ZoomViewCenter(Sqrt(2));
end;

procedure TViewport.PanWindow(const DeltaX, DeltaY:
  TRealType);
var
  TmpWin: TRect2D;
begin
  StopRepaint;
  TmpWin.Left := fVisualWindow.Left + DeltaX;
  TmpWin.Right := fVisualWindow.Right + DeltaX;
  TmpWin.W1 := 1.0;
  TmpWin.Bottom := fVisualWindow.Bottom + DeltaY;
  TmpWin.Top := fVisualWindow.Top + DeltaY;
  TmpWin.W2 := 1.0;
  if Round(TmpWin.Right - TmpWin.Left) > 300 then
    TmpWin.W2 := 1.0;
  ChangeViewportTransform(TmpWin);
end;

procedure TViewport.PanWindowFraction(const FX, FY: TRealType);
begin
  PanWindow(
    FX * (fVisualWindow.Right - fVisualWindow.Left),
    FY * (fVisualWindow.Bottom - fVisualWindow.Top));
end;

function TViewport.ScreenToViewport(const SPt: TPoint2D):
  TPoint2D;
begin
  Result := TransformPoint2D(SPt, fScreenToViewport);
end;

function TViewport.ViewportToScreen(const WPt: TPoint2D):
  TPoint2D;
begin
  Result := CartesianPoint2D(TransformPoint2D(WPt,
    fViewportToScreen));
end;

function TViewport.GetViewportToScreen: TTransf2D;
begin
  Result := fViewportToScreen;
end;

function TViewport.GetScreenToViewport: TTransf2D;
begin
  Result := fScreenToViewport;
end;

function TViewport.GetPixelAperture: TVector2D;
begin
  Result.X := Abs(fScreenToViewport[1, 1]);
  Result.Y := Abs(fScreenToViewport[2, 2]);
end;

// =====================================================================
// TRuler
// =====================================================================

procedure TRuler.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TRuler.SetOwnerView(V: TViewport);
begin
  if V <> fOwnerView then
  begin
    fOwnerView := V;
    Visible := fOwnerView.ShowRulers;
    Invalidate;
  end;
end;

procedure TRuler.SetOrientation(O: TRulerOrientationType);
begin
  if fOrientation <> O then
  begin
    fOrientation := O;
    Invalidate;
  end;
end;

procedure TRuler.SetSize(S: Integer);
begin
  if S <> FSize then
  begin
    FSize := S;
    Invalidate;
  end;
end;

procedure TRuler.SetFontSize(S: Integer);
begin
  if S <> fFontSize then
  begin
    fFontSize := S;
    Invalidate;
  end;
end;

procedure TRuler.SetTicksColor(C: TColor);
begin
  if C <> fTicksColor then
  begin
    fTicksColor := C;
    Invalidate;
  end;
end;

constructor TRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fOwnerView := nil;
  FSize := 20;
  fFontSize := 6;
  fOrientation := otVertical;
  fTicksColor := clBlack;
  Color := clWhite;
//  Canvas.Font.Name := 'Verdana';
  Width := FSize;
  Height := FSize * 4;
end;

procedure TRuler.Paint;
var
  D: TRealType;
  P: TPoint;
  XShift, YShift: Integer;
  VisualRect: TRect2D;
  PixelSize: TRealType;
  V2S: TTransf2D;
  I1, I2: Integer;
  procedure PaintHorizontal;
  var
    I: Integer;
  begin
    Canvas.MoveTo(ClientRect.Left + XShift, ClientRect.Top);
    Canvas.LineTo(ClientRect.Right + XShift, ClientRect.Top);
    D := RoundGridStep(PixelSize * 100);
    I1 := Ceil(VisualRect.Left / D);
    I2 := Floor(VisualRect.Right / D);
    for I := I1 to I2 do
    begin
      P := Point2DToPoint(TransformPoint2D(
        Point2D(I * D, 0), V2S));
      Canvas.MoveTo(P.X + XShift, ClientRect.Top);
      Canvas.LineTo(P.X + XShift, ClientRect.Bottom);
      Canvas.TextOut(P.X + XShift + 2,
        ClientRect.Top + FSize div 2, Format('%.6g', [I * D]));
    end;
    // Subdivisions.
    D := RoundGridStep(PixelSize * 12);
    I1 := Ceil(VisualRect.Left / D);
    I2 := Floor(VisualRect.Right / D);
    for I := I1 to I2 do
    begin
      P := Point2DToPoint(TransformPoint2D(
        Point2D(I * D, 0), V2S));
      Canvas.MoveTo(P.X + XShift, ClientRect.Top);
      Canvas.LineTo(P.X + XShift,
        ClientRect.Top + FSize div 2);
    end;
  end;
  procedure PaintVertical;
  var
    I: Integer;
  begin
    Canvas.MoveTo(ClientRect.Right - 1,
      ClientRect.Top + YShift);
    Canvas.LineTo(ClientRect.Right - 1,
      ClientRect.Bottom + YShift);
    D := RoundGridStep(PixelSize * 100);
    I1 := Ceil(VisualRect.Bottom / D);
    I2 := Floor(VisualRect.Top / D);
    for I := I1 to I2 do
    begin
      P := Point2DToPoint(TransformPoint2D(
        Point2D(0, I * D), V2S));
      Canvas.MoveTo(ClientRect.Left, P.Y + YShift);
      Canvas.LineTo(ClientRect.Right, P.Y + YShift);
      Canvas.TextOut(ClientRect.Left + 1, P.Y + YShift,
        Format('%-.6g', [I * D]));
    end;
    // Subdivisions.
    D := RoundGridStep(PixelSize * 12);
    I1 := Ceil(VisualRect.Bottom / D);
    I2 := Floor(VisualRect.Top / D);
    for I := I1 to I2 do
    begin
      P := Point2DToPoint(TransformPoint2D(
        Point2D(0, I * D), V2S));
      Canvas.MoveTo(ClientRect.Right - FSize div 2,
        P.Y + YShift);
      Canvas.LineTo(ClientRect.Right, P.Y + YShift);
    end;
  end;
begin
  if fOwnerView = nil then
    Exit;
  Visible := fOwnerView.ShowRulers;
  if not Visible then Exit;
  Canvas.Font.Size := fFontSize;
  Canvas.Font.Color := fTicksColor;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := fTicksColor;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(ClientRect);
  //FillRect(ClientRect);
  //FrameRect(ClientRect);
{$IFDEF VER140}
  SetBkMode(Canvas, False);
{$ELSE}
  SetBkMode(Canvas.Handle, 0);
{$ENDIF}
  XShift := fOwnerView.Left - Left;
  YShift := fOwnerView.Top - Top;
  VisualRect := fOwnerView.VisualRect;
  PixelSize := fOwnerView.GetPixelAperture.X;
  V2S := fOwnerView.fViewportToScreen;
  case fOrientation of
    otHorizontal: PaintHorizontal;
    otVertical: PaintVertical;
  end;
end;

procedure TRuler.SetMark(Value: TRealType);
var
  TmpPt: TPoint;
begin
  Paint;
  if fOwnerView = nil then
    Exit;
  with Canvas do
  begin
    Pen.Color := fTicksColor;
    Pen.Width := 3;
    case fOrientation of
      otHorizontal:
        begin
          TmpPt :=
            Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(Value, 0)));
          MoveTo(TmpPt.X, ClientRect.Top);
          LineTo(TmpPt.X, ClientRect.Bottom);
        end;
      otVertical:
        begin
          TmpPt :=
            Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(0,
            Value)));
          MoveTo(ClientRect.Left, TmpPt.Y);
          LineTo(ClientRect.Right, TmpPt.Y);
        end;
    end;
  end;
end;

// =====================================================================
// TViewport2D
// =====================================================================

procedure TViewport2D.SetDrawing(ADrawing: TDrawing);
begin
  if (ADrawing <> fDrawing2D) then
  begin
    inherited SetDrawing(ADrawing);
    fDrawing2D := ADrawing as TDrawing2D;
  end;
end;

procedure TViewport2D.SetDrawing2D(ADrawing2D: TDrawing2D);
begin
  SetDrawing(ADrawing2D);
end;

constructor TViewport2D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fImageList := nil;
  fBrushBitmap := TBitmap.Create;
  fCursorColor := clBlue;
  fLastCursorPos := Point2D(MaxInt, MaxInt);
  fLastMousePos := Point2D(0.0, 0.0);
end;

destructor TViewport2D.Destroy;
begin
  fBrushBitmap.Free;
  inherited Destroy;
end;

procedure TViewport2D.Repaint;
begin
  DrawCursorCross(Point2D(0, 0), False);
  inherited Repaint;
  DrawCursorCross(fLastMousePos, True);
end;

procedure TViewport2D.DrawObject(const Obj: TGraphicObject;
  const Dvc: TCanvasDevice; const ClipRect2D: TRect2D);
begin
  if not (Obj as TObject2D).IsVisible(VisualRect) then Exit;
  (Obj as TObject2D).DeviceDraw(IdentityTransf2D, Dvc, ClipRect2D);
end;

procedure TViewport2D.DrawObjectControlPoints(
  const Obj: TGraphicObject; const Dvc: TDevice;
  const ClipRect2D: TRect2D);
var
  Cnv: TCanvas;
begin
  if not (Obj as TObject2D).IsVisible(VisualRect) then
    Exit;
  Cnv := (Dvc as TCanvasDevice).Cnv;
  with Obj as TObject2D do
  begin
    Cnv.Pen.Color := fControlPointsPenColor;
    Cnv.Pen.Width := 1;
    Cnv.Brush.Style := bsSolid;
    Cnv.Brush.Color := ControlPointsColor;
    DrawControlPoints(ViewportToScreenTransform,
      ClipRect2D, ControlPointsWidth);
  end;
end;

procedure TViewport2D.DrawObject2D(const Obj: TObject2D;
  const CtrlPts: Boolean);
var
  TmpFlag: Boolean;
begin
  if not Assigned(fDrawing2D) then Exit;
  TmpFlag := ShowControlPoints;
  ShowControlPoints := ShowControlPoints or CtrlPts;
  try
    DrawObject(Obj, OffScreenDevice,
      RectToRect2D(ClientRect));
    DrawObject(Obj, OnScreenDevice,
      RectToRect2D(ClientRect));
  finally
    ShowControlPoints := TmpFlag;
  end;
end;

procedure TViewport2D.DrawObject2DWithRubber(
  const Obj: TGraphicObject; Transf: TTransf2D);
begin
  if not Assigned(fDrawing2D) then Exit;
  if not Assigned(Obj) then Exit;
  if not (Obj as TObject2D).IsVisible(VisualRect) then
    Exit;
  (Obj as TObject2D).DeviceDraw(Transf, fRubberDevice,
    RectToRect2D(ClientRect));
end;

procedure TViewport2D.ZoomToExtension;
var
  NewWindow2D: TRect2D;
  Marg: TRealType;
begin
  if not Assigned(fDrawing2D) then
    Exit;
  StopRepaint;
  if Drawing2D.ObjectsCount = 0 then
  begin
    Repaint;
    Exit;
  end;
  NewWindow2D := fDrawing2D.DrawingExtension;
  Marg := Abs(NewWindow2D.Right - NewWindow2D.Left) / 20;
  Marg := Max(Marg, Abs(NewWindow2D.Top -
    NewWindow2D.Bottom) / 20);
  NewWindow2D.Left := NewWindow2D.Left - Marg;
  NewWindow2D.Right := NewWindow2D.Right + Marg;
  NewWindow2D.Bottom := NewWindow2D.Bottom - Marg;
  NewWindow2D.Top := NewWindow2D.Top + Marg;
  ZoomWindow(NewWindow2D);
end;

function TViewport2D.WorldToObject(const Obj: TObject2D;
  WPt:
  TPoint2D): TPoint2D;
begin
  Result := CartesianPoint2D(WPt);
end;

function TViewport2D.ObjectToWorld(const Obj: TObject2D;
  Opt:
  TPoint2D): TPoint2D;
begin
  Opt := CartesianPoint2D(Opt);
  Result := Opt;
end;

procedure TViewport2D.MouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  fLastMousePos := ScreenToViewport(Point2D(X, Y));
  DrawCursorCross(fLastMousePos, True);
  if (not DisableMouseEvents) and Assigned(fOnMouseMove2D)
    then
    fOnMouseMove2D(Self, Shift,
      fLastMousePos.X, fLastMousePos.Y, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TViewport2D.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (not DisableMouseEvents) and Assigned(fOnMouseDown2D)
    then
  begin
    fLastMousePos := ScreenToViewport(Point2D(X, Y));
    fOnMouseDown2D(Self, Button, Shift,
      fLastMousePos.X, fLastMousePos.Y, X, Y);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TViewport2D.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (not DisableMouseEvents) and Assigned(fOnMouseUp2D)
    then
  begin
    fLastMousePos := ScreenToViewport(Point2D(X, Y));
    fOnMouseUp2D(Self, Button, Shift,
      fLastMousePos.X, fLastMousePos.Y, X, Y);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TViewport2D.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
  if (not DisableMouseEvents) and Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
end;

function TViewport2D.BuildViewportTransform(var ViewWin:
  TRect2D; const ScreenWin: TRect): TTransf2D;
begin
  Result := GetVisualTransform2D(ViewWin, ScreenWin, 1);
end;

procedure TViewport2D.DrawCursorCross(P: TPoint2D; Visible:
  Boolean);
  procedure _DrawCursorCross2D(P: TPoint2D);
  var
    ScrPt: TPoint;
  begin
    ScrPt := Point2DToPoint(ViewportToScreen(P));
    Canvas.MoveTo(ScrPt.X, ClientRect.Top);
    Canvas.LineTo(ScrPt.X, ClientRect.Bottom);
    Canvas.MoveTo(ClientRect.Left, ScrPt.Y);
    Canvas.LineTo(ClientRect.Right, ScrPt.Y);
  end;
begin
  if (HandleAllocated = False) or InRepainting then Exit;
//    fLastCursorPos := Point2D(MaxInt, MaxInt);
  Canvas.Pen.Mode := pmXOr;
  Canvas.Pen.Color := fCursorColor xor BackGroundColor;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
  if fLastCursorPos.X <> MaxInt then
    _DrawCursorCross2D(fLastCursorPos);
  if not Visible then
  begin
    fLastCursorPos := Point2D(MaxInt, MaxInt);
    Exit;
  end;
  fLastCursorPos := GetSnappedPoint(P);
  _DrawCursorCross2D(fLastCursorPos);
end;

initialization
finalization
end.

