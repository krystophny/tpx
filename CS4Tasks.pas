{: This help file explain all the interaction task classes defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4Tasks unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types mentioned here.

   See also <See Class=TCADPrg><BR>

   The task classes defined here can be used in your program by
   adding a <See Class=TCADPrg> component of the desired type
   (2D or 3D) and using the following code to start a task:

   <CODE=
    ACADPrg.StartOperation(<<A task class>>, <<The required task parameter>>);
   >

   If you want to stop a task use the following code:

   <CODE=
    ACADPrg.SendUserEvent(CADPRG_CANCEL);
   >

   and to finish the current task use the code:

   <CODE=
    ACADPrg.SendUserEvent(CADPRG_ACCEPT);
   >

   You may also start another task by suspending the current
   one (if it can be suspended) with the code:

   <CODE=
    ACADPrg.SuspendOperation(<<A task class>>, <<The required task parameter>>);
   >

   <B=Note>: All the 3D tasks work on the active
   <See=working plane@WORKPLANE> of the <See Class=TCADPrg3D>.
}
unit CS4Tasks;

interface

uses SysUtils, Classes, Windows, Graphics, Dialogs, Controls,
  CADSys4, CS4BaseTypes, CS4Shapes;

const

  CADPRG_DeleteSelected = CADPRG_CANCEL + 1;
  CADPRG_SelectAll = CADPRG_DeleteSelected + 1;
  CADPRG_MoveUp = CADPRG_SelectAll + 1;
  CADPRG_MoveDown = CADPRG_MoveUp + 1;
  CADPRG_MoveLeft = CADPRG_MoveDown + 1;
  CADPRG_MoveRight = CADPRG_MoveLeft + 1;
  CADPRG_MoveUpPixel = CADPRG_MoveRight + 1;
  CADPRG_MoveDownPixel = CADPRG_MoveUpPixel + 1;
  CADPRG_MoveLeftPixel = CADPRG_MoveDownPixel + 1;
  CADPRG_MoveRightPixel = CADPRG_MoveLeftPixel + 1;
  CADPRG_SelNext = CADPRG_MoveRightPixel + 1;
  CADPRG_SelPrev = CADPRG_SelNext + 1;
  CADPRG_FlipV = CADPRG_SelPrev + 1;
  CADPRG_FlipH = CADPRG_FlipV + 1;
  CADPRG_RotateCounterclockW = CADPRG_FlipH + 1;
  CADPRG_RotateClockW = CADPRG_RotateCounterclockW + 1;
  CADPRG_RotateCounterclockWDegree = CADPRG_RotateClockW + 1;
  CADPRG_RotateClockWDegree
    = CADPRG_RotateCounterclockWDegree + 1;
  CADPRG_Grow10 = CADPRG_RotateClockWDegree + 1;
  CADPRG_Shrink10 = CADPRG_Grow10 + 1;
  CADPRG_Grow1 = CADPRG_Shrink10 + 1;
  CADPRG_Shrink1 = CADPRG_Grow1 + 1;
  CADPRG_MoveForward = CADPRG_Shrink1 + 1;
  CADPRG_MoveBackward = CADPRG_MoveForward + 1;
  CADPRG_MoveToFront = CADPRG_MoveBackward + 1;
  CADPRG_MoveToBack = CADPRG_MoveToFront + 1;
  CADPRG_StartRotate = CADPRG_MoveToBack + 1;
  CADPRG_StartMove = CADPRG_StartRotate + 1;
  CADPRG_DuplicateSelected = CADPRG_StartMove + 1;
  CADPRG_ClipboardCopy = CADPRG_DuplicateSelected + 1;
  CADPRG_ClipboardPaste = CADPRG_ClipboardCopy + 1;
  CADPRG_ClipboardCut = CADPRG_ClipboardPaste + 1;
  CADPRG_CustomTransform = CADPRG_ClipboardCut + 1;
  CADPRG_ConvertSelected = CADPRG_CustomTransform + 1;
  // = CADPRG_ConvertSelected + 50

type
  { ******************* Zooming states *********************** }

  {: This class rapresents the parameter for zooming tasks.

     All the zooming tasks may use an optional parameter that
     is useful only if you want to start an interaction task
     after the zooming task.
  }
  TCADPrgZoomParam = class(TCADPrgParam);

  {: This is the base class for all zooming and panning
     operations.

     Because it is an abstract class it cannot be used as an
     operation. It is only defined to give a
     common interface for zooming tasks.

     See also <See Class=TCADPrg>.
  }
  TCADPrgZoomState = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class can be used to perform a windowed zoom.

     The operation waits two points from the user that are
     the two window's corner of the area to be zoommed.

     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button
     is not used.

     To stop the operation you can use either the
     <See Method=TCADPrg@StopOperation> method or send the
     <I=CADPRG_CANCEL> message.

     The operation doesn't require any parameter and cannot be
     suspended.
  }
  TCADPrgZoomArea = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class can be used to perform a <I=continuos> zoom in/zoom out.

     The operation waits for the zooming center point. If the
     shift key is hold while pressing the mouse left button, a
     zoom out will be performed, otherwise a zoom in will be done.

     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button is
     not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message.

     The operation doesn't require any parameter and cannot
     be suspended.
  }
  TCADPrgZoomInOut = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
  end;

  {: This class can be used to perform a single pan operation.

     The operation waits two points from the user that are the
     start and ending point of the panning. After the selection
     of the two points, the current <I=visual rect> will be
     translated from the start point to the end point.

     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button
     is not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message.

     The operation doesn't require any parameter and cannot be
     suspended.
  }
  TCADPrgPan = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class can be used to perform a dynamic pan operation.

     The operation waits for the mouse to be pressed. Then,
     while holding the mouse left button, the current
     <I=visual rect> will follow the mouse position. If the
     draws is complex and the painting thread is not enabled the
     panning can be delayed with respect to the mouse movements.

     The right mouse button is not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message. The operation doesn't end
     by itself but you have to send a <I=CADPRG_ACCEPT> message.

     The operation doesn't require any parameter and cannot be
     suspended.
  }
  TCADPrgRealTimePan = class(TCADPrgZoomState)
  private
    fInPanning: Boolean;
    fLastPoint: TPoint2D;
    fOriginalRect: TRect2D;
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  { ******************* Useful states *********************** }

  {: This is the base class for the selection operations.

     Because it is an abstract class it cannot be used as an
     operation.

     See also <See Class=TCADPrg>.
  }
  TCADPrgSelectionState = class(TCADState);

  {: This class defines the parameter needed by the
     <See Class=TCADPrgSelectArea> task.

     The parameter is used to store the area selected by the
     user.
  }
  TCADPrgSelectAreaParam = class(TCADPrgParam)
  private
    fFrame: TRectangle2D;
    fCallerParam: TCADPrgParam;
    function GetArea: TRect2D;
  public
    {: This constructor creates the instance of the parameter
       to be passed to <See Method=TCADPrg@StartOperation> or
       <See Method=TCADPrg@SuspendOperation>.

       <I=AfterS> contains the starting state of the operation
       that can be started at the end of the selection. If it
       is <B=nil>, the CADPrg will returns into the default
       state.

       <I=CallerParam> may contains an optional parameter that
       can be used by the operation that will receive the selection.
       This parameter will be freed when the parameter will be
       deleted. If you need it after the deletion of the
       parameter set it to <B=nil> after you have retrieved it.
    }
    constructor Create(AfterS: TCADStateClass; CallerParam:
      TCADPrgParam);
    destructor Destroy; override;
    {: This property contains an optional parameter that can
       be used by the operation that will recive the selection made.

       This parameter will be freed when the parameter will be
       deleted. If you need it after the deletion of the
       parameter set it to <B=nil> after you have retrieved it.
    }
    property CallerParam: TCADPrgParam read fCallerParam write
      fCallerParam;
    {: It will contains the area selected by the user.
    }
    property Area: TRect2D read GetArea;
  end;

  {: This task class allows the user to select a 2D area defined
     in viewport coordinates.

     It can be used to obtain a rectangular parameter usable in
     selections or other operations.

     The operation waits for two points from the user that are
     the two area corner.
     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button is
     not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message.

     The operation requires a <See Class=TCADPrgSelectAreaParam>
     parameter.

     See also <See Class=TCADPrg>.
  }
  TCADPrgSelectArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This type is another name for <See Class=TCADPrgParam>.
  }
  TCAD2DCommonParam = class(TCADPrgParam);

  {: This class defines the parameter used by the
     <See Class=TCAD2DPositionObject> task.

     The class is used to store the 2D object that must be
     positioned on the CAD. If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TDrawing>,
     otherwise it will be added to it.
  }
  TCAD2DPositionObjectParam = class(TCAD2DCommonParam)
  private
    fObject: TObject2D;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=O> contains the object to be positioned with the
        <See Class=TCAD2DPositionObject> task.>

        <B=Note>: in the case <I=AfterS> is not <B=nil>, the
         object will not be added to the CAD.
    }
    constructor Create(AfterS: TCADStateClass; O: TObject2D);
    {: This property contains the object to be positioned
       (or already positioned when the task is finished) with the
       <See Class=TCAD2DPositionObject> task.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.

       If you have assigned a state to the
       <See Property=TCADPrgParam@AfterState> property, the
       object is not added to the linked <See Class=TDrawing>,
       otherwise it will be added to it.
    }
    property Obj: TObject2D read fObject;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD2DDrawUnSizedPrimitive> to construct a 2D
     primitive with a variable number of <I=control points>,
     like a polyline.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TDrawing>,
     otherwise it will be added to it.
  }
  TCAD2DDrawUnsizedPrimitiveParam = class(TCAD2DCommonParam)
  private
    fPrimObject: TPrimitive2D;
    fCurrPoint: Word;
    fOrtoIsUsable: Boolean;
  protected
    {: This method updates the on-screen informations during
       the task.

       The method draws the current primitive with the rubber
       band (xor pen mode) mode. You will see your primitive
       growing as new points are added to it.
    }
    procedure DrawOSD(Viewport: TCADViewport2D);
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=Primitive> is the 2D primitive to be constructed.>
       <LI=<I=StartPointIdx> is the first control points that
        will be added. For instance if this parameter is equal to
        3, when the user click on the viewport, the fourth control
        point will be added.>
       <LI=<I=OrtoIsU> indicate if the ortogonal constraint has
        any means with this primitive. If it is <B=True>, the
        orto constraint will be used, otherwise it will not used.>
    }
    constructor Create(AfterS: TCADStateClass; Primitive:
      TPrimitive2D; StartPointIdx: Integer; OrtoIsU: Boolean);
    {: This property contains the primitive being constructed.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.

       If you have assigned a state to the
       <See Property=TCADPrgParam@AfterState> property, the
       object is not added to the linked <See Class=TDrawing>,
       otherwise it will be added to it.
    }
    property Primitive: TPrimitive2D read fPrimObject;
    {: This property indicates if the ortogonal constraint has
       any means with the primitive being defined.

       If it is <B=True>, the orto constraint will be used,
       otherwise it will not used.
    }
    property OrtoIsUsable: Boolean read fOrtoIsUsable write
      fOrtoIsUsable;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD2DDrawSizedPrimitive> to construct a 2D
     primitive with a fixed number of points, like an ellipse.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TDrawing>,
     otherwise it will be added to it.
  }
  TCAD2DDrawSizedPrimitiveParam =
    class(TCAD2DDrawUnsizedPrimitiveParam)
  private
    fnPoints: Word;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=Primitive> is the 2D primitive to be constructed.>
       <LI=<I=StartPointIdx> is the first control points that
        will be added. For instance if this parameter is equal
        to 3 when the user click on the viewport, the fourth
        control point will be added.>
       <LI=<I=OrtoIsU> indicate if the ortogonal constraint has
        any meanse with this primitive. If it is <B=True>, the
        orto constraint will be used, otherwise it will not used.>
    }
    constructor Create(AfterS: TCADStateClass; Primitive:
      TPrimitive2D; StartPointIdx, NPoints: Integer; OrtoIsU:
      Boolean);
  end;

  {: This class implements the <I=object positioning task>.

     This task may be used to add an object to the linked CAD
     by firstly positioning it in the world.

     The operation wait for the user to do the following
     (in the order given here):

     <LI=move the object to the desired position using the
      mouse. You will see the object moving on the screen.>
     <LI=press the left mouse button on that point to lay down
      the object.>

     The object will be moved using its bottom-left bounding box
     corner as the base point. At the end of the task the object
     is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DPositionObjectParam> class. The task may
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DPositionObject = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=primitive constructing task>
     for primitives with a fixed number of control points.

     This task may be used to add an object to the linked CAD by
     firstly defining its control points.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the position for the first control
      point of the primitive (if <I=StartPointIdx> of the
      parameter is greater than zero the control point to be
      positioned is not the first control point).>
     <LI=press the left mouse button to set the control point on
      that point.>
     <LI=move the mouse on the desired position for the next
      control points. Continue with the second step until no
      control points are left.>

     During the second and third steps you will see the object
     on the screen.

     At the end of the task the object is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
     added to the CAD.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DDrawSizedPrimitiveParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DDrawSizedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=primitive constructing task>
     for primitives with a variable number of control points.

     This task may be used to add an object to the linked CAD
     by firstly defining its control points.

     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the mouse on the position for the first control
      point of the primitive (if <I=StartPointIdx> of the parameter
      is greater than zero the control point to be positioned is not
      the first control point).>
     <LI=press the left mouse button to set the control point on
      that point.>
     <LI=move the mouse on the desired position for the next
      control points. Continue with the second.>

     During the second and third steps you will see the object
     on the screen.

     At the end of the task the object is added to the CAD.
     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
      added to the CAD. Note that this is the only way to end
      the task.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DDrawUnSizedPrimitiveParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DDrawUnsizedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by the
     <See Class=TCAD2DDrawArcPrimitive> task to construct a
     2D arc segment.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TDrawing>,
     otherwise it will be added to it.
  }
  TCAD2DDrawArcPrimitiveParam = class(TCAD2DCommonParam)
  private
    fArcObject: TArc2D;
    fCurrPoint: Word;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state. Note that in case
        <I=AfterS> is not <B=nil>, the object will not be added to
        the CAD.>
       <LI=<I=Arc> contains the object to be constructed with
        the <See Class=TCAD2DDrawArcPrimitive> task.>
    }
    constructor Create(AfterS: TCADStateClass; Arc: TArc2D);
    {: This property contains the 2D arc that is being constructed.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.
    }
    property Arc: TArc2D read fArcObject;
  end;

  {: This class implements the <I=arc constructing task>.

     This task may be used to add an arc to the linked CAD by
     defining its control points.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the desired position for the first
      control point of the arc.>
     <LI=press the left mouse button to set the first control
      point on that point.>
     <LI=move the mouse on the desired position for the second
      control point. You will see an ellipse drawed on the
      viewport.>
     <LI=press the left mouse button to set the second control
      point.>
     <LI=move the mouse on the desired position for the third
      control point. You will modify the starting angle of the
      arc, and you will see the arc on the screen.>
     <LI=press the left mouse button to set the third control
      point.>
     <LI=move the mouse on the desired position for the fourth
      control point. You will modify the ending angle of the
      arc, and you will see the arc on the screen.>
     <LI=press the left mouse button to set the fourth control
      point.>

     At the end of the task the arc is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
      added to the CAD.>
     <LI=CADPRG_CANCEL>. The task is aborted and the object
     destroyed. The CADPrgreturns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DDrawArcPrimitiveParam> class. The task can
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DDrawArcPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This type is another name for <See Class=TCADPrgParam>.
  }
  TCAD2DSelectObjectsParam = class;

  {: This type defines the prototype for an event handler used
     to inform the application that an object was picked.

     See <See Class=TCAD2DSelectObject>,
     <See Class=TCAD2DSelectObjects> and
     <See Class=TCAD2DSelectObjectsInArea> for details.

     Parameters:

     <LI=<I=Sender> contains the instance of
      <See Class=TCAD2DSelectObjectsParam> (or
      <See Class=TCAD2DSelectObjectsInAreaParam>) of the task
      that had called the handler.>
     <LI=<I=Obj> contains the picked object, or <B=nil> if the
      selection task is <See Class=TCAD2DSelectObjectsInArea>.>
     <LI=<I=CtrlPt> contains the control point on which the
      mouse was picked. This is the same result of the
      <See Method=TCADViewport2D@PickObject> method. This
      parameter will be <I=PICK_NOOBJECT> in case the selection
      task is <See Class=TCAD2DSelectObjectsInArea>.>
     <LI=<I=Added> is <B=True> if Obj is added to the selected
      object list, or <B=False> if it is removed.>

     If a repaint event is raised, the handler is called for all
     the picked objects. In this case the <I=CtrlPt> is
     <I=PICK_NOOBJECT> and <I=Added> is <B=True> the selection
     task that fired the event is
     <See Class=TCAD2DSelectObjectsInArea>.
  }
  TSelection2DEvent = procedure(Sender:
    TCAD2DSelectObjectsParam; Obj: TObject2D; CtrlPt: Integer;
    Added: Boolean) of object;

  {: This class defines the parameter used by
     <See Class=TCAD2DSelectObject> and
     <See Class=TCAD2DSelectObjects> to pick objects
     interactively on the screen.
  }
  TCAD2DSelectObjectsParam = class(TCAD2DCommonParam)
  private
    fApertureSize: Word;
    fLastSelectedCtrlPoint: Integer;
    fLastPt: TPoint2D;
    fOnSelected: TSelection2DEvent;
  protected
    {: This method draws the picking frame used to show the
       aperture of the picking.

       The frame is drawed in xor pen mode and centered at <I=Pt>.

       Parameters:

       <LI=<I=Viewport> is the viewport on which draw the frame.>
       <LI=<I=Pt> is the point at which draw the frame.>
    }
    procedure DrawOSD(Viewport: TCADViewport2D; const PT:
      TPoint2D); virtual;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=ApertureSize> is the aperture used for the picking
        in pixels.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state.
    }
    constructor Create(ApertureSize: Word; const AfterS:
      TCADStateClass);
    {: This property contains the control point selected of the
       last picked object.
    }
    property LastSelectedCtrlPoint: Integer read
      fLastSelectedCtrlPoint;
    {: EVENTS}
    {: This property may contains an event handler that will be
      called when an object is picked (after it was added to the
      list).

      See Also <See Type=TSelection2DEvent>.
    }
    property OnObjectSelected: TSelection2DEvent read fOnSelected
      write fOnSelected;
  end;

  {: This class implements the <I=single object selection task>.

     This task may be used to select an object of the linked CAD.
     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object.>

     The object will be added to the
     <See Property=TCAD2DSelectObjectsParam@SelectedObjects> list
     of the task parameter. Normally you set <I=AfterS> of the
     task parameter to a state that will process the selected
     object by using the task parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of the
     <See Class=TCAD2DSelectObjectsParam> class. The task can
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DSelectObject = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=multiple object selection task>.

     This task may be used to select a set of object of the
     linked CAD.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object. If the object was already picked, it is removed
      from <See Property=TCAD2DSelectObjectsParam@SelectedObjects>
      list of the task parameter, otherwise it will be added.>
     <LI=continue with the first step.>

     Normally you set <I=AfterS> of the task parameter to a
     state that will process the selected objects by using the
     passed parameter.

     Note that no visual feedback is given to the user. If you
     want to show the selected objects, you can use the
     <See Property=TCAD2DSelectObjectsParam@OnObjectSelected>
     handler of the task parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>
     <LI=<I=CADPRG_ACCEPT>. The task is ended. The CADPrg
     returns in the default state or in the state specified by
     <I=AfterS>. Note that this is the only way to finish the
     task.>

     The operation needs an instance of the
     <See Class=TCAD2DSelectObjectsParam>. The task can be
     suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DSelectObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines a special kind of selection task that
     is the combination of the
     <See Class=TCAD2DSelectObject> and
     <See Class=TCAD2DSelectObjects> tasks.

     If the user holds down the Shift key, the task behaves
     like the <I=TCAD2DSelectObjects> task, otherwise it
     behaves like the <I=TCAD2DSelectObject> task.

     See also <See Class=TCADPrg>.
  }
  TCAD2DExtendedSelectObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD2DSelectObjectsInArea> task to select the
     objects contained in the specified window area.
  }
  TCAD2DSelectObjectsInAreaParam =
    class(TCAD2DSelectObjectsParam)
  private
    fAreaMode: TGroupMode;
    fArea: TRect2D;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AreaMode> specify the type of selection. If it is
        <I=gmAllInside> only the objects fully contained in the
        area are selected; if it is <I=gmCrossFrame> all the
        objects that are contained or cross the area are selected.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is nil, the
        CADPrg will return to the default state.>
    }
    constructor Create(AreaMode: TGroupMode; const AfterS:
      TCADStateClass);
  end;

  {: This class implements <I=the 'area selection task>.

     This task may be used to select a set of object of
     the linked CAD by specify a rectangle frame.

     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the mouse on the first corner of the area.>
     <LI=press the left mouse button to accept the point.>
     <LI=move the mouse on the second corner of the area. You
      will see the area being defined on the screen.>
     <LI=press the left mouse button to accept the point.>

     All the objects in the area are selected and stored in the
     <See Property=TCAD2DSelectObjectsParam@SelectedObjects>
     list of the task parameter.
     Normally you set <I=AfterS> of the task parameter to a
     state that will process the selected objects by using the
     passed parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of the
     <See Class=TCAD2DSelectObjectsInAreaParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DSelectObjectsInArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by the
     <I=object transformation task>.

     All the transformations of objects that may be described
     by a matrix transform that accept as parametesr a base
     point and a moving point can be modelled by this task.
     You only need to derive from this parameter and
     redefine the
     <See Method=TCAD2DTransformObjectsParam@GetTransform> method.
     Then you can pass the new parameter to
     <See Class=TCAD2DTransformObjects> task.
  }
  TCAD2DTransformObjectsParam = class(TCAD2DCommonParam)
  private
    fBasePt: TPoint2D;
    fNPoint: Integer;
    fBox: TRect2D;
    fObjs: TGraphicObjList;
    fUseFrame: Boolean;
    fCurrTransf: TTransf2D;

    procedure TransformObjs(CurrPt: TPoint2D; UseOrto: Boolean);
    procedure ConfirmTransform;
    procedure CancelTransform;
  protected
    {: This method draws the bounding box of the set of objects
       to be transformed.

       It is used by the <See Method=TCAD2DTransformObjectsParam@DrawOSD>
       method.

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawWithFrame(Viewport: TCADViewport2D); dynamic;
    {: This method draws the objects to be transformed in
       rubber band mode (xor pen mode).

       It is used by the <See Method=TCAD2DTransformObjectsParam@DrawOSD>
       method.

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawWithoutFrame(Viewport: TCADViewport2D);
      dynamic;
    {: This is the key method of the class.

       It must return the matrix transform that define the
       transformation of the objects. The returned matrix will
       override the current model transform for the selected
       objects.

       This method must be redefined in the derived classes
       for specific transformations.

       Parameters:

       <LI=<I=BasePt> is the base point for the transformation.
        For example to rotate an object you must give the center
        of rotation; to move an object you must give the first
        point of the translation.>
       <LI=<I=CurrPt> is the current point of the mouse. You
        may use this point to define the current transformation.
        For example to rotate an object you must give a second
        point, so you are able to find the angle of rotation
        with respect to the <I=BasePt>; to move an object this
        point is the second point of the translation.>
    }
    function GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D;
      virtual; abstract;
    {: This method draws the on screen informations that informs
       the user of the result of the transformation.

       There are two modes of visualization:

       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=True> only the bounding box of the objects is showed>
       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=False> the transformed objects are showed in xor
        pen mode>

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawOSD(Viewport: TCADViewport2D);
    {: This property contains the base point for the
       transformation.
    }
    property BasePoint: TPoint2D read fBasePt;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=Objs> is a list that contains the objects to be
        transformed. The list must have the
        <See Property=TGraphicObjList@FreeOnClear> property set
        to <B=False>.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state.>
    }
    constructor Create(Objs: TGraphicObjList; const AfterS:
      TCADStateClass);
    destructor Destroy; override;
    {: This property contains the list of the objects to be
       transformed.
    }
    property Objects: TGraphicObjList read fObjs;
    {: This property selects the visualization mode for the on
       screen informations:

       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=True> only the bounding box of the objects is showed>
       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=False> the transformed objects are showed in xor
        pen mode>

       When the parameter is constructed, this property is set
       to <B=False> if the passed list of objects has only one
       object, it is set to <B=True> otherwise.>
    }
    property UseFrame: Boolean read fUseFrame write fUseFrame;
  end;

  {: This class defines the method
     <See Method=TCAD2DTransformObjectsParam@GetTransform> for
     the <I=move object task>.
  }
  TCAD2DMoveObjectsParam = class(TCAD2DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D;
      override;
  end;

  {: This class defines the method
     <See Method=TCAD2DTransformObjectsParam@GetTransform> for
     the <I=rotate object task>.
  }
  TCAD2DRotateObjectsParam = class(TCAD2DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D;
      override;
  end;

  {: This class implements the <I=transform objects task>.

     This task may be used to apply a transformation to a set
     of objects by specifing the appropriate parameter (see below).

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the base point for the transformation.>
     <LI=press the left mouse button to accept the base point.>
     <LI=move the mouse on the second point. You will see the
      object transformed on the screen.>
     <LI=press the left mouse button to accept the current
      transformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of a class derived from
     <See Class=TCAD2DTransformObjectsParam>. The task can be
     suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DTransformObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  // TSY: simple drag variant of transform
  TCAD2DTransformObjectsSimple = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=move a selection task>.

     This task may be used to select and move a set of objects
     by specifing the start point and end point of the
     translation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the base point for the translation.>
     <LI=press the left mouse button to accept the base point.>
     <LI=move the mouse on the second point. You will see the
      objects moving on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD2DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DMoveSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=rotate a selection task>.

     This task may be used to select and rotate a set of
     objects by specifing the center of rotation and the angle
     of rotation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the center of rotation.>
     <LI=press the left mouse button to accept the center of
      rotation.>
     <LI=move the mouse on the second point. You will see the
      objects rotating on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD2DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DRotateSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=edit primitive task>.

     This task may be used to move the control points of a
     <See Class=TPrimitive2D> interactively on the screen.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on one control point of the primitive.
      The primitive is showed in with the rubber pen of
      <See Class=TCADViewport>.>
     <LI=press and hold the left mouse button to pick the
      control point.>
     <LI=move the mouse to move the control point. You will see
      the primitive changing its shape.>
     <LI=release the left mouse button to accept the new
      position of the control point.>
     <LI=continue from the first step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and accept the
      new setting for the control point.>
     <LI=CADPRG_CANCEL>. The task is aborted and the parameter
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the of the primitive
     incapsulated into the <See Property=TCADPrgParam@UserObject>
     property of a <See Class=TCADPrgParam> instance.

     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DEditPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  //TSY:
  TCAD2DMoveControlPoint = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=select and edit primitive task>.

     This task may be used to select a primitive and move its
     control points interactively on the screen.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object.>
     <LI=move the mouse on one control point of the primitive.
      The primitive is showed in with the rubber pen of
      <See Class=TCADViewport>.>
     <LI=press and hold the left mouse button to pick the
      control point.>
     <LI=move the mouse to move the control point. You will see
      the primitive changing its shape.>
     <LI=release the left mouse button to accept the
      new position of the control point.>
     <LI=continue from the third step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and accept the
      new setting for the control point.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of
     <See Class=TCAD2DSelectObjectsParam>.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DEditSelectedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  TInternalMode = (im_Normal, im_Rotate, im_Move, im_Drag);

  TCAD2D_BasicMode = class(TCADState)
  private
    fLastHot: TObject2D;
    fMode: TInternalMode;
    fBasePt: TPoint2D;
    fBaseAngle: TRealType;
    fCurrTransf: TTransf2D;
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    procedure TransformSelected(Transf: TTransf2D);
    procedure ShiftSelected(Shift: TPoint2D);
    procedure FlipSelected(DX, DY: TRealType);
    procedure RotateSelected(R: TRealType);
    procedure ScaleSelected(Scale: TRealType);
    procedure CustomTransform;
    procedure BackwardForward(Key: Integer);
    procedure DuplicateSelected;
    procedure ConvertSelected(DestClass: TPrimitive2DClass);
    procedure ChangePrimitiveProperties(Obj: TPrimitive2D);
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure DrawOSDTransform(Viewport: TCADViewport2D);
    function GetRotateTransform: TTransf2D;
    function GetMoveTransform: TTransf2D;
    procedure OnEventTransformMode(Event: TCADPrgEvent;
      MouseButton: TCS4MouseButton;
      Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass; Result: Boolean);
    procedure OnStop; override;
    procedure OnResume(const State: TClass; const SusParam:
      TCADPrgParam); override;
    procedure OnSuspend(const State: TClass; const
      SusParam: TCADPrgParam); override;
    property Mode: TInternalMode read fMode default im_Normal;
  end;

implementation

uses Math, Propert, TransForm;

type
// -----===== Starting Cs4CADPrgTasks.pas =====-----
  TCADPrgEndZoomArea = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  TCADPrgDragPan = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  TCADPrgDragSelectArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam:
      TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton:
      TCS4MouseButton; Shift: TShiftState; Key: Word;
      var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

// -----===== Starting Cs4CADPrgTasks2D.pas =====-----
  TCAD2DEditPrimitiveParam = class(TCAD2DCommonParam)
  private
    fCurrentPrimitive, fOriginalPrimitive: TPrimitive2D;
    fCurrentCtrlPt: Integer;
    fApertureSize: Word;
    fLastPt: TPoint2D;
  public
    constructor Create(Prim: TPrimitive2D; ApertureSize: Word);
    destructor Destroy; override;

    procedure SetCtrlPoint(Viewport: TCADViewport2D; PT:
      TPoint2D);
    procedure AddCtrlPoint(Viewport: TCADViewport2D; PT:
      TPoint2D);
    procedure UnSetCtrlPoint;
    procedure AcceptEdited;
    procedure MoveCtrlPoint(Viewport: TCADViewport2D; PT:
      TPoint2D);
    procedure DrawOSD(Viewport: TCADViewport2D; PT: TPoint2D;
      FirstTime: Boolean);
    procedure DrawModifiedPrim(Viewport: TCADViewport2D);

    property CurrentCtrlPt: Integer read fCurrentCtrlPt;
  end;

// -----===== Starting Cs4CADPrgTasks.pas =====-----

{ -------------- TCADPrgSelectAreaParam -------------- }

constructor TCADPrgSelectAreaParam.Create(AfterS:
  TCADStateClass; CallerParam: TCADPrgParam);
begin
  inherited Create(AfterS);
  fFrame := TRectangle2D.Create(0);
  fCallerParam := CallerParam;
end;

destructor TCADPrgSelectAreaParam.Destroy;
begin
  fFrame.Free;
  inherited Destroy;
end;

function TCADPrgSelectAreaParam.GetArea: TRect2D;
var
  PT: TPoint2D;
begin
  PT := fFrame.Box.FirstEdge;
  Result := fFrame.Box;
end;

{ ******************* Useful states *********************** }

{ ------------------ Select Area --------------------- }

constructor TCADPrgSelectArea.Create(const CADPrg: TCADPrg; const
  StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the first point of the area.'
end;

{ Need TCADPrgSelectAreaParam. }

function TCADPrgSelectArea.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton;
  Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with Param as TCADPrgSelectAreaParam, CADPrg do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          Param.Free;
          Param := nil;
          NextState := CADPrg.DefaultState;
          Result := True;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          CurrPoint := CurrentViewportPoint;
          fFrame.Points[0] := CurrPoint;
          fFrame.Points[1] := CurrPoint;
          fFrame.Points[2] := Point2D(CurrPoint.X,
            CurrPoint.Y - 1);
          if Viewport is TCADViewport2D then
            TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
          NextState := TCADPrgDragSelectArea;
          Result := True;
        end;
    end;
end;

procedure TCADPrgSelectArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCADPrgDragSelectArea.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the second point of the area.'
end;

function TCADPrgDragSelectArea.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton;
  Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with Param as TCADPrgSelectAreaParam, CADPrg do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseUp: if MouseButton = cmbLeft then
        begin
          if Assigned(AfterState) then
            NextState := AfterState
          else
            NextState := DefaultState;
          Result := True;
        end;
      ceMouseMove:
        begin
          if Viewport is TCADViewport2D then
            TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
          fFrame.Points[1] := CurrentViewportPoint;
          if Viewport is TCADViewport2D then
            TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
        end;
      cePaint:
        if Viewport is TCADViewport2D then
          TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
    end;
end;

procedure TCADPrgDragSelectArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

{ ******************* Zooming states *********************** }

constructor TCADPrgZoomState.Create(const CADPrg: TCADPrg; const
  StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  CanBeSuspended := False;
end;

{ ------------------ Zoom Area --------------------- }

{ No parameter. }

constructor TCADPrgZoomArea.Create(const CADPrg: TCADPrg; const
  StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Param := TCADPrgSelectAreaParam.Create(TCADPrgEndZoomArea,
    StateParam);
  NextState := TCADPrgSelectArea;
end;

constructor TCADPrgEndZoomArea.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if Assigned(Param) then
    with CADPrg as TCADPrg, Param as TCADPrgSelectAreaParam do
    begin
      if not IsSamePoint2D(Area.FirstEdge, Area.SecondEdge) then
        Viewport.ZoomWindow(Area);
      if (CallerParam is TCADPrgZoomParam) and
        Assigned(TCADPrgParam(CallerParam).AfterState) then
        NextState := TCADPrgParam(CallerParam).AfterState
      else
        NextState := CADPrg.DefaultState;
      Param.Free;
      Param := nil;
    end;
end;

{ ------------------ ZoomInOut --------------------- }

constructor TCADPrgZoomInOut.Create(const CADPrg: TCADPrg; const
  StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the center of the zoom. (Hold Shift key for zoom out)'
end;

{ No parameter. }

function TCADPrgZoomInOut.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  with CADPrg as TCADPrg do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          CurrPoint := CurrentViewportPoint;
          with Viewport do
            PanWindow(CurrPoint.X - (VisualRect.Right +
              VisualRect.Left) / 2.0,
              CurrPoint.Y - (VisualRect.Bottom + VisualRect.Top)
              / 2.0);
          if SSShift in Shift then
            Viewport.ZoomOut
          else
            Viewport.ZoomIn;
        end;
    end;
end;

{ ------------------ Pan --------------------- }

{ No parameter. }

constructor TCADPrgPan.Create(const CADPrg: TCADPrg; const
  StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  //Param := TCADPrgParam.Create(StateParam.AfterState);
  //tsy:
  if Assigned(StateParam) then
    Param := TCADPrgParam.Create(StateParam.AfterState)
  else
    Param := TCADPrgParam.Create(nil);
  Param.UserObject := TLine2D.Create(0, Point2D(0, 0),
    Point2D(0, 0));
  Description := 'Select the start point of the pan.'
end;

function TCADPrgPan.OnEvent(Event: TCADPrgEvent; MouseButton:
  TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with TCADPrgParam(Param), CADPrg as TCADPrg do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          TLine2D(UserObject).Free;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          CurrPoint := CurrentViewportPoint;
          TLine2D(UserObject).Points[0] := CurrPoint;
          TLine2D(UserObject).Points[1] := CurrPoint;
          if Viewport is TCADViewport2D then
            TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject),
              False);
          NextState := TCADPrgDragPan;
          Result := True;
        end;
    end;
end;

procedure TCADPrgPan.OnStop;
begin
  TCADPrgParam(Param).UserObject.Free;
  Param.Free;
  Param := nil;
end;

constructor TCADPrgDragPan.Create(const CADPrg: TCADPrg; const
  StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the end point of the pan.'
end;

function TCADPrgDragPan.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with TCADPrgParam(Param), CADPrg as TCADPrg do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          TLine2D(UserObject).Free;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
          with TLine2D(UserObject) do
          begin
            IgnoreEvents := True;
            try
              if not IsSamePoint2D(Box.FirstEdge, Box.SecondEdge)
                then
                Viewport.PanWindow(Points[0].X - Points[1].X,
                  Points[0].Y - Points[1].Y);
            finally
              IgnoreEvents := False;
            end;
            if Assigned(AfterState) then
              NextState := AfterState
            else
              NextState := DefaultState;
            Free;
            Param.Free;
            Param := nil;
            Result := True;
          end;
      ceMouseMove:
        begin
          if Viewport is TCADViewport2D then
            TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject),
              False);
          CurrPoint := CurrentViewportPoint;
          TLine2D(UserObject).Points[1] := CurrPoint;
          if Viewport is TCADViewport2D then
            TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject),
              False);
        end;
      cePaint:
        if Viewport is TCADViewport2D then
          TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject),
            False);
    end;
end;

procedure TCADPrgDragPan.OnStop;
begin
  TCADPrgParam(Param).UserObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------------ RealTimePan --------------------- }

{ No parameter. }

constructor TCADPrgRealTimePan.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Hold the mouse and move it to pan.';
  fInPanning := False;
  fOriginalRect := CADPrg.Viewport.VisualRect;
  (CADPrg as TCADPrg).Viewport.Cursor := {crSizeAll crHand} 1;
end;

function TCADPrgRealTimePan.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  ScrPt, CurrPoint: TPoint2D;
  TmpDist, RefDist: TRealType;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  with CADPrg as TCADPrg do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          NextState := DefaultState;
          Result := True;
          Viewport.ZoomWindow(fOriginalRect);
          Viewport.Cursor := crDefault;
        end
        else if Key = CADPRG_ACCEPT then
        begin
          if (Param is TCADPrgZoomParam) and
            Assigned(TCADPrgParam(Param).AfterState) then
            NextState := TCADPrgParam(Param).AfterState
          else
            NextState := DefaultState;
          RepaintAfterOperation;
          Viewport.Cursor := crDefault;
          Result := True;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          fInPanning := True;
          fLastPoint := CurrentViewportPoint;
          if MouseButton = cmbRight then
          begin
            NextState := DefaultState;
            Result := True;
          end;
        end;
      ceMouseDblClick,
        ceMouseUp: if MouseButton = cmbLeft then
        begin
          if Viewport.UsePaintingThread then
            Viewport.Repaint
          else
            Viewport.Refresh;
          fInPanning := False;
        end;
      ceMouseMove: if fInPanning then
        begin
          Viewport.StopRepaint;
          CurrPoint := CurrentViewportPoint;
          TmpDist := PointDistance2D(CurrPoint, fLastPoint);
          RefDist :=
            PointDistance2D(Viewport.VisualRect.FirstEdge,
            Viewport.VisualRect.SecondEdge);
          if (TmpDist < RefDist * 0.0001) or (TmpDist > RefDist
            * 2) then
          begin
            fLastPoint := CurrPoint;
            Exit;
          end;
          ScrPt := Viewport.ViewportToScreen(CurrPoint);
          Viewport.PanWindow(fLastPoint.X - CurrPoint.X,
            fLastPoint.Y - CurrPoint.Y);
          fLastPoint := Viewport.ScreenToViewport(ScrPt);
        end;
    end;
end;

procedure TCADPrgRealTimePan.OnStop;
begin
  with CADPrg as TCADPrg do
    Viewport.Cursor := crDefault;
end;

// -----===== Starting Cs4CADPrgTasks2D.pas =====-----

{ ******************* Drawing tasks *********************** }

constructor TCAD2DPositionObjectParam.Create(AfterS:
  TCADStateClass; O: TObject2D);
begin
  inherited Create(AfterS);
  fObject := O;
end;

constructor TCAD2DDrawUnsizedPrimitiveParam.Create(AfterS:
  TCADStateClass; Primitive: TPrimitive2D; StartPointIdx:
  Integer;
  OrtoIsU: Boolean);
begin
  inherited Create(AfterS);
  fPrimObject := Primitive;
  fCurrPoint := StartPointIdx;
  fOrtoIsUsable := OrtoIsU;
end;

procedure TCAD2DDrawUnsizedPrimitiveParam.DrawOSD(Viewport:
  TCADViewport2D);
begin
  Viewport.DrawObject2DWithRubber(fPrimObject, True);
end;

constructor TCAD2DDrawSizedPrimitiveParam.Create(AfterS:
  TCADStateClass; Primitive: TPrimitive2D; StartPointIdx,
  NPoints: Integer;
  OrtoIsU: Boolean);
begin
  inherited Create(AfterS, Primitive, StartPointIdx, OrtoIsU);
  if NPoints <= 0 then
    fnPoints := Primitive.Points.Count
  else fnPoints := NPoints;
end;

{ ------------- }

constructor TCAD2DPositionObject.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD2DPositionObjectParam) then
    raise
      ECADSysException.Create('TCAD2DPositionObject: Invalid param');
  Description :=
    'Press the mouse on the desired insertion point.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord,
    MaxCoord);
end;

function TCAD2DPositionObject.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DPositionObjectParam
    do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          fObject.Free;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
          IgnoreEvents := True;
          TmpBool := Viewport2D.Drawing2D.DrawOnAdd;
          Viewport2D.Drawing2D.DrawOnAdd := True;
          try
            Viewport2D.Drawing2D.AddObject(fObject.ID, fObject);
            Viewport.Drawing.SelectionAdd(fObject);
            Viewport.Repaint;
          finally
            Viewport2D.Drawing2D.DrawOnAdd := TmpBool;
            IgnoreEvents := False;
          end;
          fObject.UpdateExtension(Self);
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
          Exit;
        end;
      ceMouseMove:
        begin
          CurrPoint2D := CurrentViewportSnappedPoint;
          Viewport2D.DrawObject2DWithRubber(fObject, True);
          fObject.MoveTo(CurrPoint2D, fObject.Box.FirstEdge);
          Viewport2D.DrawObject2DWithRubber(fObject, True);
        end;
      cePaint:
        Viewport2D.DrawObject2DWithRubber(fObject, True);
    end;
end;

procedure TCAD2DPositionObject.OnStop;
begin
  TCAD2DPositionObjectParam(Param).fObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD2DDrawSizedPrimitive.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD2DDrawSizedPrimitiveParam) then
    raise
      ECADSysException.Create('TCAD2DDrawSizedPrimitive: Invalid param');
  Description := 'Press the mouse on the desired points.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord,
    MaxCoord);
end;

function TCAD2DDrawSizedPrimitive.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
  //TSY: Moved to procedure
  procedure PointSelected;
  var
    Cont: Integer;
  begin
    with CADPrg as TCADPrg2D, Param as
      TCAD2DDrawSizedPrimitiveParam do
    begin
      DrawOSD(Viewport2D);
      CurrPoint2D := CurrentViewportSnappedPoint;
      SnapOriginPoint := CurrentViewportSnappedPoint;
      if fCurrPoint = 0 then
      begin
        for Cont := 0 to fnPoints - 1 do
          fPrimObject.Points[Cont] := CurrPoint2D;
        if fPrimObject is TBox2D0 then
          fPrimObject.Points[2] :=
            Point2D(fPrimObject.Points[0].X,
            fPrimObject.Points[0].Y - 1);
      end
      else
      begin
        if fOrtoIsUsable and UseOrto then
          MakeOrto2D(fPrimObject.Points[fCurrPoint - 1],
            CurrPoint2D);
        fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      end;
      Inc(fCurrPoint);
      if fCurrPoint = fnPoints then
      begin
        if Assigned(AfterState) then
        begin
          NextState := AfterState;
          Result := True;
          Exit;
        end;
        IgnoreEvents := True;
        TmpBool := Viewport2D.Drawing2D.DrawOnAdd;
        Viewport2D.Drawing2D.DrawOnAdd := True;
        try
          Viewport2D.Drawing2D.AddObject(fPrimObject.ID,
            fPrimObject);
          Viewport.Drawing.SelectionAdd(fPrimObject);
          Viewport.Repaint;
        finally
          Viewport2D.Drawing2D.DrawOnAdd := TmpBool;
          IgnoreEvents := False;
        end;
        Param.Free;
        Param := nil;
        NextState := DefaultState;
        Result := True;
        Exit;
      end
      else
        fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      DrawOSD(Viewport2D);
    end;
  end;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as
    TCAD2DDrawSizedPrimitiveParam do
    case Event of
      ceUserDefined:
        if Key = CADPRG_ACCEPT then
        begin
          if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
          IgnoreEvents := True;
          TmpBool := Viewport2D.Drawing2D.DrawOnAdd;
          Viewport2D.Drawing2D.DrawOnAdd := True;
          try
            Viewport2D.Drawing2D.AddObject(fPrimObject.ID,
              fPrimObject);
            Viewport.Drawing.SelectionAdd(fPrimObject);
            Viewport.Repaint;
          finally
            Viewport2D.Drawing2D.DrawOnAdd := TmpBool;
            IgnoreEvents := False;
          end;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end
        else if Key = CADPRG_CANCEL then
        begin
          fPrimObject.Free;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          PointSelected;
        end;
      ceMouseUp: if MouseButton = cmbLeft then
        begin
          if fCurrPoint = 1 then PointSelected;
        end;
      ceMouseMove: if fCurrPoint > 0 then
        begin
          CurrPoint2D := CurrentViewportSnappedPoint;
          if fOrtoIsUsable and UseOrto then
            MakeOrto2D(fPrimObject.Points[fCurrPoint - 1],
              CurrPoint2D);
          DrawOSD(Viewport2D);
          fPrimObject.Points[fCurrPoint] := CurrPoint2D;
          DrawOSD(Viewport2D);
        end;
      cePaint: DrawOSD(Viewport2D);
    end;
end;

procedure TCAD2DDrawSizedPrimitive.OnStop;
begin
  TCAD2DDrawSizedPrimitiveParam(Param).fPrimObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD2DDrawUnsizedPrimitive.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD2DDrawUnsizedPrimitiveParam) then
    raise
      ECADSysException.Create('TCAD2DDrawUnSizedPrimitive: Invalid param');
  TCAD2DDrawUnsizedPrimitiveParam(StateParam).fPrimObject.Points.Clear;
  Description := 'Press the mouse on the desired points.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord,
    MaxCoord);
end;

function TCAD2DDrawUnsizedPrimitive.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
  //TSY: Moved
  procedure AcceptPrimitive;
  begin
    with CADPrg as TCADPrg2D, Param as
      TCAD2DDrawUnsizedPrimitiveParam do
    begin
      if Assigned(AfterState) then
      begin
        NextState := AfterState;
        Result := True;
        Exit;
      end;
      IgnoreEvents := True;
      TmpBool := Viewport2D.Drawing2D.DrawOnAdd;
      Viewport2D.Drawing2D.DrawOnAdd := True;
      try
        Viewport2D.Drawing2D.AddObject(fPrimObject.ID,
          fPrimObject);
        Viewport.Drawing.SelectionAdd(fPrimObject);
        Viewport.Repaint;
      finally
        Viewport2D.Drawing2D.DrawOnAdd := TmpBool;
        IgnoreEvents := False;
      end;
      Param.Free;
      Param := nil;
      NextState := DefaultState;
      Result := True;
    end;
  end;
  procedure AcceptPrimitive0;
  begin
    with Param as TCAD2DDrawUnsizedPrimitiveParam do
    begin
      //TSY: delete last point
      fPrimObject.Points.Delete(
        fPrimObject.Points.Count - 1);
      Dec(fCurrPoint);
    end;
    AcceptPrimitive;
  end;
begin
  {if not ((Event = ceMouseDown) and (MouseButton = cmbRight))
    then}
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as
    TCAD2DDrawUnsizedPrimitiveParam do
    case Event of
      ceUserDefined:
        if Key = CADPRG_ACCEPT then
          AcceptPrimitive0
        else if Key = CADPRG_CANCEL then
        begin
          fPrimObject.Free;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseDblClick:
        begin
          AcceptPrimitive0;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          DrawOSD(Viewport2D);
          CurrPoint2D := CurrentViewportSnappedPoint;
          SnapOriginPoint := CurrentViewportSnappedPoint;
          if fCurrPoint = 0 then
            fPrimObject.Points.Add(CurrPoint2D)
          else
          begin
            if fOrtoIsUsable and UseOrto then
              MakeOrto2D(fPrimObject.Points[fCurrPoint - 1],
                CurrPoint2D);
            fPrimObject.Points[fCurrPoint] := CurrPoint2D;
          end;
          if (fCurrPoint = 0) or not IsSamePoint2D(CurrPoint2D,
            fPrimObject.Points[fCurrPoint - 1]) then
            Inc(fCurrPoint);
          fPrimObject.Points[fCurrPoint] := CurrPoint2D;
          DrawOSD(Viewport2D);
        end
        else if MouseButton = cmbRight then
          AcceptPrimitive0;
      ceMouseMove: if fCurrPoint > 0 then
        begin
          CurrPoint2D := CurrentViewportSnappedPoint;
          if fOrtoIsUsable and UseOrto then
            MakeOrto2D(fPrimObject.Points[fCurrPoint - 1],
              CurrPoint2D);
          DrawOSD(Viewport2D);
          fPrimObject.Points[fCurrPoint] := CurrPoint2D;
          DrawOSD(Viewport2D);
        end;
      cePaint: DrawOSD(Viewport2D);
      ceKeyDown: if Key = VK_ESCAPE then
          AcceptPrimitive0;
    end;
end;

procedure TCAD2DDrawUnsizedPrimitive.OnStop;
begin
  TCAD2DDrawUnsizedPrimitiveParam(Param).fPrimObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD2DDrawArcPrimitiveParam.Create(AfterS:
  TCADStateClass; Arc: TArc2D);
begin
  inherited Create(AfterS);
  fArcObject := Arc;
end;

constructor TCAD2DDrawArcPrimitive.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DDrawArcPrimitiveParam) then
    raise
      ECADSysException.Create('TCAD2DDrawArcPrimitive: Invalid param');
  Description := 'Drag the ellipse which contain the arc.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord,
    MaxCoord);
end;

function TCAD2DDrawArcPrimitive.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DDrawArcPrimitiveParam
    do
    case Event of
      ceUserDefined:
        if Key = CADPRG_ACCEPT then
        begin
          if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
          IgnoreEvents := True;
          TmpBool := Viewport2D.Drawing2D.DrawOnAdd;
          Viewport2D.Drawing2D.DrawOnAdd := True;
          try
            Viewport2D.Drawing2D.AddObject(fArcObject.ID,
              fArcObject);
            Viewport.Drawing.SelectionAdd(fArcObject);
          finally
            Viewport2D.Drawing2D.DrawOnAdd := TmpBool;
            IgnoreEvents := False;
          end;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end
        else if Key = CADPRG_CANCEL then
        begin
          fArcObject.Free;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          Viewport2D.DrawObject2DWithRubber(fArcObject, True);
          CurrPoint2D := CurrentViewportSnappedPoint;
          SnapOriginPoint := CurrentViewportSnappedPoint;
          fArcObject.Points[fCurrPoint] := CurrPoint2D;
          Inc(fCurrPoint);
          if fCurrPoint = 4 then
          begin
            if Assigned(AfterState) then
            begin
              NextState := AfterState;
              Result := True;
              Exit;
            end;
            TmpBool := Viewport2D.Drawing2D.DrawOnAdd;
            Viewport2D.Drawing2D.DrawOnAdd := True;
            IgnoreEvents := True;
            try
              Viewport2D.Drawing2D.AddObject(fArcObject.ID,
                fArcObject);
              Viewport.Drawing.SelectionAdd(fArcObject);
            finally
              Viewport2D.Drawing2D.DrawOnAdd := TmpBool;
              IgnoreEvents := False;
            end;
            Param.Free;
            Param := nil;
            NextState := DefaultState;
            Result := True;
            Exit;
          end
          else if fCurrPoint = 0 then
          begin
            fArcObject.Points[0] := CurrPoint2D;
            fArcObject.Points[1] := CurrPoint2D;
            fArcObject.Points[2] := CurrPoint2D;
            fArcObject.Points[3] := CurrPoint2D;
          end
          else if fCurrPoint = 2 then
          begin
            fArcObject.StartAngle := 0;
            fArcObject.EndAngle := 0;
            Description := 'Select the start and end angle of the arc.'
          end;
          fArcObject.Points[fCurrPoint] := CurrPoint2D;
          Viewport2D.DrawObject2DWithRubber(fArcObject, True);
        end;
      ceMouseMove: if fCurrPoint > 0 then
        begin
          CurrPoint2D := CurrentViewportSnappedPoint;
          Viewport2D.DrawObject2DWithRubber(fArcObject, True);
          fArcObject.Points[fCurrPoint] := CurrPoint2D;
          Viewport2D.DrawObject2DWithRubber(fArcObject, True);
        end;
      cePaint: Viewport2D.DrawObject2DWithRubber(fArcObject,
          True);
    end;
end;

procedure TCAD2DDrawArcPrimitive.OnStop;
begin
  TCAD2DDrawArcPrimitiveParam(Param).fArcObject.Free;
  Param.Free;
  Param := nil;
end;

{ ******************* Editing tasks *********************** }

constructor TCAD2DSelectObjectsParam.Create(ApertureSize: Word;
  const AfterS: TCADStateClass);
begin
  inherited Create(AfterS);
  fApertureSize := ApertureSize;
end;

procedure TCAD2DSelectObjectsParam.DrawOSD(Viewport:
  TCADViewport2D; const PT: TPoint2D);
var
  ScrPt: TPoint;
begin
  with Viewport do
  begin
    ScrPt := Point2DToPoint(ViewportToScreen(PT));
    OnScreenCanvas.Canvas.Pen.Assign(Viewport.RubberPen);
    OnScreenCanvas.Canvas.Pen.Style := psSolid;
    OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X -
        fApertureSize, ScrPt.Y - fApertureSize),
      Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
        Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
        Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
        Point(ScrPt.X - fApertureSize, ScrPt.Y -
        fApertureSize)]);
  end;
end;

constructor TCAD2DSelectObject.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
    raise
      ECADSysException.Create('TCAD2DSelectObject: Invalid param');
  Description := 'Use the mouse to select an object.';
  with TCAD2DSelectObjectsParam(StateParam) do
    DrawOSD(TCADPrg2D(CADPrg).Viewport2D, fLastPt);
end;

function TCAD2DSelectObject.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;

        end;
      ceMouseUp: if MouseButton = cmbLeft then
        begin
          TmpObj := Viewport2D.PickObject(CurrentViewportPoint,
            fApertureSize, False, fLastSelectedCtrlPoint);
          if (fLastSelectedCtrlPoint > PICK_INBBOX) and
            Assigned(TmpObj) and (TmpObj is SelectionFilter)
            then
          begin
            Viewport.Drawing.SelectionAdd(TmpObj);
            IgnoreEvents := True;
            try
              if Assigned(fOnSelected) then
                fOnSelected(TCAD2DSelectObjectsParam(Param),
                  TmpObj, fLastSelectedCtrlPoint, True);
              Viewport2D.Refresh;
            finally
              IgnoreEvents := False;
            end;
            if Assigned(AfterState) then
              NextState := AfterState
            else
            begin
              Param.Free;
              Param := nil;
              NextState := DefaultState;
            end;
            Result := True;
          end;
        end;
      ceMouseMove:
        begin
          DrawOSD(Viewport2D, fLastPt);
          fLastPt := CurrentViewportPoint;
          DrawOSD(Viewport2D, fLastPt);
        end;
      cePaint: DrawOSD(Viewport2D, fLastPt);
    end;
end;

procedure TCAD2DSelectObject.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCAD2DSelectObjects.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
    raise
      ECADSysException.Create('TCAD2DSelectObjects: Invalid param');
  Description := 'Use the mouse to select objects.';
  with TCAD2DSelectObjectsParam(StateParam) do
    DrawOSD(TCADPrg2D(CADPrg).Viewport2D, fLastPt);
end;

function TCAD2DSelectObjects.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject2D;
  TmpExIter: TExclusiveGraphicObjIterator;
  TmpIter: TGraphicObjIterator;
  Removed: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end
        else if Key = CADPRG_ACCEPT then
        begin
          IgnoreEvents := True;
          try
            Viewport2D.Refresh;
          finally
            IgnoreEvents := False;
          end;
          if Assigned(AfterState) then
            NextState := AfterState
          else
          begin
            Param.Free;
            Param := nil;
          end;
          Result := True;
        end;
      ceMouseUp: if MouseButton = cmbLeft then
        begin
          DrawOSD(Viewport2D, fLastPt);
          TmpObj := Viewport2D.PickObject(CurrentViewportPoint,
            fApertureSize, False, fLastSelectedCtrlPoint);
          if (fLastSelectedCtrlPoint > PICK_INBBOX) and
            Assigned(TmpObj) and (TmpObj is SelectionFilter)
            and
            (fLastSelectedCtrlPoint > PICK_INBBOX) then
          begin
            Removed := False;
            TmpExIter := SelectedObjs.GetExclusiveIterator;
            try
              if TmpExIter.Search(TmpObj.ID) <> nil then
              begin
                TmpExIter.RemoveCurrent;
                Removed := True;
              end;
            finally
              TmpExIter.Free;
            end;
            if not Removed then
              Viewport.Drawing.SelectionAdd(TmpObj);
            IgnoreEvents := True;
            try
              if Assigned(fOnSelected) then
                fOnSelected(TCAD2DSelectObjectsParam(Param),
                  TmpObj, fLastSelectedCtrlPoint, not Removed);
            finally
              IgnoreEvents := False;
            end;
          end;
          DrawOSD(Viewport2D, fLastPt);
        end;
      ceMouseMove:
        begin
          DrawOSD(Viewport2D, fLastPt);
          fLastPt := CurrentViewportPoint;
          DrawOSD(Viewport2D, fLastPt);
        end;
      cePaint:
        begin
          DrawOSD(Viewport2D, fLastPt);
          if Assigned(fOnSelected) then
          begin
            IgnoreEvents := True;
            TmpIter := SelectedObjs.GetIterator;
            try
              TmpIter.First;
              while TmpIter.Current <> nil do
              begin
                fOnSelected(TCAD2DSelectObjectsParam(Param),
                  TObject2D(TmpIter.Current), PICK_NOOBJECT,
                  True);
                TmpIter.Next;
              end;
            finally
              TmpIter.Free;
              IgnoreEvents := False;
            end;
          end;
        end;
    end;
end;

procedure TCAD2DSelectObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;


constructor TCAD2DExtendedSelectObjects.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
    raise
      ECADSysException.Create('TCAD2DExtendedSelectObjects: Invalid param');
  Description :=
    'Use the mouse to select one object, hold shift key pressed to select more than one object.';
  with TCAD2DSelectObjectsParam(StateParam) do
    DrawOSD(TCADPrg2D(CADPrg).Viewport2D, fLastPt);
end;

function TCAD2DExtendedSelectObjects.OnEvent(Event:
  TCADPrgEvent; MouseButton: TCS4MouseButton; Shift:
  TShiftState;
  Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject2D;
  TmpExIter: TExclusiveGraphicObjIterator;
  TmpIter: TGraphicObjIterator;
  Removed: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end
        else if Key = CADPRG_ACCEPT then
        begin
          IgnoreEvents := True;
          try
            Viewport2D.Refresh;
          finally
            IgnoreEvents := False;
          end;
          if Assigned(AfterState) then
            NextState := AfterState
          else
          begin
            Param.Free;
            Param := nil;
          end;
          Result := True;
        end;
      ceMouseUp: if MouseButton = cmbLeft then
        begin
          DrawOSD(Viewport2D, fLastPt);
          TmpObj := Viewport2D.PickObject(CurrentViewportPoint,
            fApertureSize, False, fLastSelectedCtrlPoint);
          if (fLastSelectedCtrlPoint > PICK_INBBOX) and
            Assigned(TmpObj) and (TmpObj is SelectionFilter)
            and
            (fLastSelectedCtrlPoint > PICK_INBBOX) then
          begin
            Removed := False;
            TmpExIter := SelectedObjs.GetExclusiveIterator;
            try
              if TmpExIter.Search(TmpObj.ID) <> nil then
              begin
                TmpExIter.RemoveCurrent;
                Removed := True;
              end;
            finally
              TmpExIter.Free;
            end;
            if not Removed then
              Viewport.Drawing.SelectionAdd(TmpObj);
            IgnoreEvents := True;
            try
              if Assigned(fOnSelected) then
                fOnSelected(TCAD2DSelectObjectsParam(Param),
                  TmpObj, fLastSelectedCtrlPoint, not Removed);
            finally
              IgnoreEvents := False;
            end;
         // Controlla se il tasto del mouse è premuto.
            if Key <> VK_SHIFT then
            begin // No allora si comporta come selezione di un solo oggetto.
              if Removed then
                Viewport.Drawing.SelectionAdd(TmpObj);
              IgnoreEvents := True;
              try
                if Assigned(fOnSelected) then
                  fOnSelected(TCAD2DSelectObjectsParam(Param),
                    TmpObj, fLastSelectedCtrlPoint, True);
                Viewport2D.Refresh;
              finally
                IgnoreEvents := False;
              end;
              if Assigned(AfterState) then
                NextState := AfterState
              else
              begin
                Param.Free;
                Param := nil;
                NextState := DefaultState;
              end;
              Result := True;
              Exit;
            end;
          end;
          DrawOSD(Viewport2D, fLastPt);
        end;
      ceMouseMove:
        begin
          DrawOSD(Viewport2D, fLastPt);
          fLastPt := CurrentViewportPoint;
          DrawOSD(Viewport2D, fLastPt);
        end;
      cePaint:
        begin
          DrawOSD(Viewport2D, fLastPt);
          if Assigned(fOnSelected) then
          begin
            IgnoreEvents := True;
            TmpIter := SelectedObjs.GetExclusiveIterator;
            try
              TmpIter.First;
              while TmpIter.Current <> nil do
              begin
                fOnSelected(TCAD2DSelectObjectsParam(Param),
                  TObject2D(TmpIter.Current), PICK_NOOBJECT,
                  True);
                TmpIter.Next;
              end;
            finally
              TmpIter.Free;
              IgnoreEvents := False;
            end;
          end;
        end;
    end;
end;

procedure TCAD2DExtendedSelectObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;



constructor TCAD2DSelectObjectsInAreaParam.Create(AreaMode:
  TGroupMode; const AfterS: TCADStateClass);
begin
  inherited Create(0, AfterS);

  fAreaMode := AreaMode;
end;

constructor TCAD2DSelectObjectsInArea.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
var
  NewParam: TCADPrgParam;
  LastFilt: TObject2DClass;
  TempList: TGraphicObjList;
begin
  inherited;
  if StateParam is TCADPrgSelectAreaParam then
  begin // Return from Select area.
    NewParam := TCADPrgSelectAreaParam(Param).CallerParam;
    TCADPrgSelectAreaParam(Param).CallerParam := nil;
    TCAD2DSelectObjectsInAreaParam(NewParam).fArea :=
      TCADPrgSelectAreaParam(StateParam).Area;
    Param.Free;
    Param := NewParam; // Set the parameter back to the original.
    with CADPrg as TCADPrg2D, Param as
      TCAD2DSelectObjectsInAreaParam do
    begin
      LastFilt := Viewport2D.PickFilter;
      TempList := TGraphicObjList.Create;
      TempList.FreeOnClear := False;
      try
        Viewport2D.PickFilter := SelectionFilter;
        Viewport2D.GroupObjects(TempList, fArea,
          fAreaMode, False);
        Viewport.Drawing.SelectionAddList(TempList);
      finally
        Viewport2D.PickFilter := LastFilt;
        TempList.Free;
      end;
      if Assigned(fOnSelected) then
        fOnSelected(TCAD2DSelectObjectsParam(Param), nil,
          PICK_NOOBJECT, True);
      IgnoreEvents := True;
      try
        Viewport2D.Repaint;
      finally
        IgnoreEvents := False;
      end;
      if Assigned(AfterState) then
        NextState := AfterState
      else
      begin
        Param.Free;
        Param := nil;
        NextState := DefaultState;
      end;
    end;
  end
  else if not (StateParam is TCAD2DSelectObjectsInAreaParam)
    then
    raise
      ECADSysException.Create('TCAD2DSelectObjectsInArea: Invalid param')
  else
  begin
    NewParam :=
      TCADPrgSelectAreaParam.Create(TCAD2DSelectObjectsInArea,
      Param);
    Param := NewParam;
      // Set the parameter to the select area param.
    NextState := TCADPrgSelectArea;
  end;
end;

procedure TCAD2DSelectObjectsInArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

{ ---------------------------- }

procedure TCAD2DTransformObjectsParam.TransformObjs(CurrPt:
  TPoint2D; UseOrto: Boolean);
begin
  if UseOrto then
    MakeOrto2D(fBasePt, CurrPt);
  fCurrTransf := GetTransform(fBasePt, CurrPt);
end;

procedure TCAD2DTransformObjectsParam.DrawWithFrame(Viewport:
  TCADViewport2D);
begin
  with Viewport do
  begin
    OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
    DrawBoundingBox2D(OnScreenCanvas, fBox,
      RectToRect2D(OnScreenCanvas.Canvas.ClipRect),
      MultiplyTransform2D(fCurrTransf,
      ViewportToScreenTransform));
  end;
end;

procedure TCAD2DTransformObjectsParam.DrawWithoutFrame(Viewport:
  TCADViewport2D);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := fObjs.GetIterator;
  with Viewport do
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
    begin
      {TmpObj.ModelTransform := fCurrTransf;
      DrawObject2DWithRubber(TmpObj, False);!!}
      TmpObj := TmpIter.Next as TObject2D;
    end;
  finally
    TmpIter.Free;
  end;
end;

procedure TCAD2DTransformObjectsParam.DrawOSD(Viewport:
  TCADViewport2D);
begin
  Viewport.OnScreenCanvas.Canvas.Pen.Assign(Viewport.RubberPen);
  if fUseFrame then
    DrawWithFrame(Viewport)
  else
    DrawWithoutFrame(Viewport);
end;

procedure TCAD2DTransformObjectsParam.ConfirmTransform;
var
  TmpObj: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := fObjs.GetExclusiveIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
    begin
      {TmpObj.ModelTransform := fCurrTransf;
      TmpObj.ApplyTransform;}
      TmpObj := TmpIter.Next as TObject2D;
    end;
  finally
    TmpIter.Free;
  end;
end;

procedure TCAD2DTransformObjectsParam.CancelTransform;
var
  TmpObj: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := fObjs.GetExclusiveIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
    begin
      //TmpObj.ModelTransform := IdentityTransf2D;
      TmpObj := TmpIter.Next as TObject2D;
    end;
  finally
    TmpIter.Free;
  end;
end;

constructor TCAD2DTransformObjectsParam.Create(Objs:
  TGraphicObjList; const AfterS: TCADStateClass);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  inherited Create(AfterS);

  fObjs := TGraphicObjList.Create;
  fObjs.FreeOnClear := False;
  fCurrTransf := IdentityTransf2D;
  if Objs.Count = 0 then
    raise
      ECADSysException.Create('TCAD2DTransformObjectsParam: Invalid list');
  // Recupera il BBox a conferma la trasformazione Obj corrente.
  fUseFrame := Objs.Count > 1;
  TmpIter := Objs.GetIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    fBox := TmpObj.Box;
    while TmpObj <> nil do
    begin
      fObjs.Add(TmpObj);
      //TmpObj.ApplyTransform;
      fBox := BoxOutBox2D(fBox, TmpObj.Box);
      TmpObj := TmpIter.Next as TObject2D;
    end;
  finally
    TmpIter.Free;
  end;
end;

destructor TCAD2DTransformObjectsParam.Destroy;
begin
  fObjs.Free;
  inherited;
end;


function TCAD2DMoveObjectsParam.GetTransform(BasePt, CurrPt:
  TPoint2D): TTransf2D;
begin
  Result := Translate2D(CurrPt.X - BasePt.X, CurrPt.Y -
    BasePt.Y);
end;

function TCAD2DRotateObjectsParam.GetTransform(BasePt, CurrPt:
  TPoint2D): TTransf2D;
var
  A: TRealType;
begin
  A := ArcTan2(CurrPt.Y - BasePt.Y, CurrPt.X - BasePt.X);
  Result := Translate2D(-BasePt.X, -BasePt.Y);
  Result := MultiplyTransform2D(Result, Rotate2D(A));
  Result := MultiplyTransform2D(Result, Translate2D(BasePt.X,
    BasePt.Y));
end;


constructor TCAD2DTransformObjects.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DTransformObjectsParam) then
    raise
      ECADSysException.Create('TCAD2DTransformObjects: Invalid param');
  Description :=
    'Select the base point for the transformation.';
  with TCAD2DTransformObjectsParam(StateParam) do
    DrawWithFrame(TCADPrg2D(CADPrg).Viewport2D);
end;

function TCAD2DTransformObjects.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DTransformObjectsParam
    do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          CancelTransform;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end
        else if Key = CADPRG_ACCEPT then
        begin
          ConfirmTransform;
          IgnoreEvents := True;
          try
            Viewport2D.Repaint;
          finally
            IgnoreEvents := False;
          end;
          if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          CurrPoint2D := CurrentViewportSnappedPoint;
          if fNPoint = 0 then
          begin
            fBasePt := CurrPoint2D;
            Description :=
              'Move the mouse to modify the transformation and press the mouse to apply it.';
          end
          else
          begin
            ConfirmTransform;
            IgnoreEvents := True;
            try
              Viewport2D.Repaint;
            finally
              IgnoreEvents := False;
            end;
            if Assigned(AfterState) then
            begin
              NextState := AfterState;
              Result := True;
              Exit;
            end;
            Param.Free;
            Param := nil;
            Result := True;
            NextState := DefaultState;
            Exit;
          end;
          DrawOSD(Viewport2D);
          Inc(fNPoint);
        end;
      ceMouseMove: if fNPoint > 0 then
        begin
          DrawOSD(Viewport2D);
          CurrPoint2D := CurrentViewportSnappedPoint;
          TransformObjs(CurrPoint2D, UseOrto);
          DrawOSD(Viewport2D);
        end;
      cePaint:
        begin
          DrawOSD(Viewport2D);
        end;
    end;
end;

procedure TCAD2DTransformObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;


constructor TCAD2DTransformObjectsSimple.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DTransformObjectsParam) then
    raise
      ECADSysException.Create('TCAD2DTransformObjects: Invalid param');
  Description :=
    'Hold mouse and drag objects.';
  with CADPrg as TCADPrg2D,
    TCAD2DTransformObjectsParam(StateParam) do
  begin
    if fNPoint = 0 then
    begin
      fBasePt := CurrentViewportSnappedPoint;
      fNPoint := 1;
      DrawOSD(Viewport2D);
    end;
    DrawWithFrame(TCADPrg2D(CADPrg).Viewport2D);
  end;
end;

function TCAD2DTransformObjectsSimple.OnEvent(Event:
  TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DTransformObjectsParam
    do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          CancelTransform;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end
        else if Key = CADPRG_ACCEPT then
        begin
          ConfirmTransform;
          IgnoreEvents := True;
          try
            Viewport2D.Repaint;
          finally
            IgnoreEvents := False;
          end;
          if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end;
      ceMouseUp: if MouseButton = cmbLeft then
        begin
          ConfirmTransform;
          IgnoreEvents := True;
          try
            Viewport2D.Repaint;
          finally
            IgnoreEvents := False;
          end;
          if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
          Param.Free;
          Param := nil;
          Result := True;
          NextState := DefaultState;
          Exit;
        end;
      ceMouseMove: if fNPoint > 0 then
        begin
          DrawOSD(Viewport2D);
          CurrPoint2D := CurrentViewportSnappedPoint;
          TransformObjs(CurrPoint2D, UseOrto);
          DrawOSD(Viewport2D);
        end;
      cePaint:
        begin
          DrawOSD(Viewport2D);
        end;
    end;
end;

procedure TCAD2DTransformObjectsSimple.OnStop;
begin
  Param.Free;
  Param := nil;
end;


constructor TCAD2DMoveSelectedObjects.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
var
  NewParam: TCAD2DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
    raise
      ECADSysException.Create('TCAD2DMoveSelectedObjects: Invalid param');
  TmpList := CADPrg.CurrentState.SelectedObjs;
  if TmpList.Count = 0 then
  begin
    Param.Free;
    Param := nil;
    NextState := CADPrg.DefaultState;
    Exit;
  end;
  NewParam := TCAD2DMoveObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD2DMoveSelectedObjects;
  NextState := TCAD2DTransformObjects;
end;

constructor TCAD2DRotateSelectedObjects.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
var
  NewParam: TCAD2DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
    raise
      ECADSysException.Create('TCAD2DRotateSelectedObjects: Invalid param');
  TmpList := CADPrg.CurrentState.SelectedObjs;
  if TmpList.Count = 0 then
  begin
    Param.Free;
    Param := nil;
    NextState := CADPrg.DefaultState;
    Exit;
  end;
  NewParam := TCAD2DRotateObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD2DRotateSelectedObjects;
  NextState := TCAD2DTransformObjects;
end;


constructor TCAD2DEditPrimitiveParam.Create(Prim: TPrimitive2D;
  ApertureSize: Word);
begin
  inherited Create(nil);

  fOriginalPrimitive := Prim;
  fCurrentPrimitive :=
    CADSysFindClassByName(fOriginalPrimitive.ClassName).Create(0)
    as TPrimitive2D;
  if not Assigned(fCurrentPrimitive) then
    raise
      ECADSysException.Create('TCAD2DEditPrimitive: Only registered classes are allowed');
  fCurrentPrimitive.Assign(fOriginalPrimitive);
  fApertureSize := ApertureSize;
  fCurrentCtrlPt := -1;
end;

destructor TCAD2DEditPrimitiveParam.Destroy;
begin
  fCurrentPrimitive.Free;
  inherited;
end;

procedure TCAD2DEditPrimitiveParam.SetCtrlPoint(Viewport:
  TCADViewport2D; PT: TPoint2D);
var
  TmpDist: TRealType;
  TmpAp: TRealType;
begin
  with Viewport do
  begin
    TmpAp := GetAperture(fApertureSize);
    fCurrentCtrlPt := fCurrentPrimitive.OnMe(PT, TmpAp,
      TmpDist);
  end;
end;

procedure TCAD2DEditPrimitiveParam.AddCtrlPoint(Viewport:
  TCADViewport2D; PT: TPoint2D);
var
  TmpCPt: TPoint2D;
begin
  if fCurrentCtrlPt > -1 then
  begin
    DrawModifiedPrim(Viewport);
     // Porto il punto da coordinate mondo a coordinate oggetto
     // perche' i punti di controllo sono in quest'ultimo sistema.
    TmpCPt := Viewport.WorldToObject(fCurrentPrimitive, PT);
    fCurrentPrimitive.Points.Insert(fCurrentCtrlPt, TmpCPt);
    DrawModifiedPrim(Viewport);
  end;
end;

procedure TCAD2DEditPrimitiveParam.UnSetCtrlPoint;
begin
  fCurrentCtrlPt := -1;
end;

procedure TCAD2DEditPrimitiveParam.AcceptEdited;
begin
  fOriginalPrimitive.Assign(fCurrentPrimitive);
end;

procedure TCAD2DEditPrimitiveParam.MoveCtrlPoint(Viewport:
  TCADViewport2D; PT: TPoint2D);
var
  TmpCPt: TPoint2D;
begin
  if fCurrentCtrlPt > -1 then
  begin
    DrawModifiedPrim(Viewport);
     // Porto il punto da coordinate mondo a coordinate oggetto
     // perche' i punti di controllo sono in quest'ultimo sistema.
    TmpCPt := Viewport.WorldToObject(fCurrentPrimitive, PT);
    fCurrentPrimitive.Points[fCurrentCtrlPt] := TmpCPt;
    DrawModifiedPrim(Viewport);
  end;
end;

procedure TCAD2DEditPrimitiveParam.DrawOSD(Viewport:
  TCADViewport2D; PT: TPoint2D; FirstTime: Boolean);
var
  ScrPt: TPoint;
begin
  with Viewport do
  begin
    OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
    if not FirstTime then
    begin
      ScrPt := Point2DToPoint(ViewportToScreen(fLastPt));
      OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X -
          fApertureSize, ScrPt.Y - fApertureSize),
        Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
          Point(ScrPt.X + fApertureSize, ScrPt.Y +
          fApertureSize),
          Point(ScrPt.X - fApertureSize, ScrPt.Y +
          fApertureSize),
          Point(ScrPt.X - fApertureSize, ScrPt.Y -
          fApertureSize)]);
    end;
    fLastPt := PT;
    ScrPt := Point2DToPoint(ViewportToScreen(fLastPt));
    OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X -
        fApertureSize, ScrPt.Y - fApertureSize),
      Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
        Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
        Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
        Point(ScrPt.X - fApertureSize, ScrPt.Y -
        fApertureSize)]);
  end;
end;

procedure TCAD2DEditPrimitiveParam.DrawModifiedPrim(Viewport:
  TCADViewport2D);
begin
  with Viewport do
    DrawObject2DWithRubber(fCurrentPrimitive, True);
end;

constructor TCAD2DEditPrimitive.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
var
  NewParam: TCAD2DEditPrimitiveParam;
begin
  inherited;
  if not (StateParam is TCADPrgParam) or not
    (TCADPrgParam(StateParam).UserObject is TPrimitive2D) then
    raise
      ECADSysException.Create('TCAD2DEditPrimitive: Invalid param');
  Description := 'Select a Control point of the primitive';
  NewParam :=
    TCAD2DEditPrimitiveParam.Create(TPrimitive2D(TCADPrgParam(StateParam).UserObject), 5);
  with TCADPrg2D(CADPrg) do
  begin
    Viewport2D.Refresh;
    NewParam.DrawOSD(Viewport2D, Point2D(0, 0), True);
    NewParam.DrawModifiedPrim(Viewport2D);
  end;
  Param := NewParam;
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord,
    MaxCoord);
end;

function TCAD2DEditPrimitive.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  // TSY: moved
  procedure AcceptEdit;
  begin
    with CADPrg as TCADPrg2D, Param as TCAD2DEditPrimitiveParam
      do
    begin
      AcceptEdited;
      IgnoreEvents := True;
      try
        Viewport2D.Repaint;
      finally
        IgnoreEvents := False;
      end;
      Param.Free;
      Param := nil;
      NextState := DefaultState;
      Result := True;
    end;
  end;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DEditPrimitiveParam do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end
        else if Key = CADPRG_ACCEPT then
          AcceptEdit;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          SetCtrlPoint(Viewport2D, CurrentViewportPoint);
          if CurrentCtrlPt >= 0 then
            Description :=
              'Move the control point and release the mouse.'
          else AcceptEdit; //TSY: accept if clicked not current
        end
        else if MouseButton = cmbLeft then
          AcceptEdit;
      ceMouseDblClick: if (MouseButton = cmbLeft) and
        (fCurrentPrimitive.Points.GrowingEnabled) then
        begin
          SetCtrlPoint(Viewport2D, CurrentViewportPoint);
          if CurrentCtrlPt >= 0 then
          begin
            CurrPoint2D := CurrentViewportSnappedPoint;
            AddCtrlPoint(Viewport2D, CurrPoint2D);
          end;
        end;
      ceMouseUp: if MouseButton = cmbLeft then
        begin
          UnSetCtrlPoint;
          Description :=
            'Select a Control point of the primitive';
        end;
      ceMouseMove:
        begin
          CurrPoint2D := CurrentViewportSnappedPoint;
          DrawOSD(Viewport2D, CurrentViewportPoint, False);
          MoveCtrlPoint(Viewport2D, CurrPoint2D);
          DrawOSD(Viewport2D, CurrentViewportPoint, False);
        end;
      cePaint:
        begin
          DrawOSD(Viewport2D, CurrentViewportPoint, True);
          DrawModifiedPrim(Viewport2D);
        end;
    end;
end;

procedure TCAD2DEditPrimitive.OnStop;
begin
  Param.Free;
  Param := nil;
end;

type

  TCAD2DMoveControlPointParam = class(TCADPrgParam)
    ControlPoint: Integer;
    constructor Create(AfterS: TCADStateClass; ControlPoint0:
      Integer);
  end;

constructor TCAD2DMoveControlPointParam.Create(AfterS:
  TCADStateClass; ControlPoint0: Integer);
begin
  inherited Create(AfterS);
  ControlPoint := ControlPoint0;
end;


  //TSY:

constructor TCAD2DMoveControlPoint.Create(const CADPrg: TCADPrg;
  const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
var
  NewParam: TCAD2DEditPrimitiveParam;
begin
  inherited;
  if not (StateParam is TCADPrgParam) or not
    (TCADPrgParam(StateParam).UserObject is TPrimitive2D) then
    raise
      ECADSysException.Create('TCAD2DEditPrimitive: Invalid param');
  Description := 'Move the control point and release the mouse';
  NewParam :=
    TCAD2DEditPrimitiveParam.Create(TPrimitive2D(TCADPrgParam(StateParam).UserObject), 5);
  with TCADPrg2D(CADPrg) do
  begin
    NewParam.SetCtrlPoint(Viewport2D,
      TPrimitive2D(TCADPrgParam(StateParam).UserObject).Points[
      TCAD2DMoveControlPointParam(StateParam).ControlPoint]);
    NewParam.DrawOSD(Viewport2D, Point2D(0, 0), True);
    NewParam.DrawOSD(Viewport2D, CurrentViewportPoint, False);
    NewParam.DrawModifiedPrim(Viewport2D);
  end;
  Param := NewParam;
end;

function TCAD2DMoveControlPoint.OnEvent(Event: TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  OriginalPrimitive: TPrimitive2D;
  procedure AcceptEdit;
  begin
    with CADPrg as TCADPrg2D, Param as TCAD2DEditPrimitiveParam
      do
    begin
      AcceptEdited;
      OriginalPrimitive := (Param as
        TCAD2DEditPrimitiveParam).fOriginalPrimitive;
      Param.Free;
      Param := nil;
      CADPrg.Viewport.Drawing.NotifyChanged;
      if CADPrg.Viewport.Drawing.SelectedObjects.Find(
        OriginalPrimitive.ID) = nil then
        CADPrg.Viewport.Drawing.SelectionAdd(OriginalPrimitive);
      Viewport2D.Repaint;
      NextState := nil;
      Result := True;
    end;
  end;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DEditPrimitiveParam do
    case Event of
      ceMouseUp: if MouseButton = cmbLeft then
        begin
          AcceptEdit;
        end;
      ceMouseMove:
        begin
          CurrPoint2D := CurrentViewportSnappedPoint;
          DrawOSD(Viewport2D, CurrentViewportPoint, False);
          MoveCtrlPoint(Viewport2D, CurrPoint2D);
          DrawOSD(Viewport2D, CurrentViewportPoint, False);
        end;
      cePaint:
        begin
          DrawOSD(Viewport2D, CurrentViewportPoint, True);
          DrawModifiedPrim(Viewport2D);
        end;
    end;
end;

procedure TCAD2DMoveControlPoint.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCAD2DEditSelectedPrimitive.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
var
  NewParam: TCADPrgParam;
  NewObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
    raise
      ECADSysException.Create('TCAD2DEditSelectedObjects: Invalid param');
  with TCAD2DSelectObjectsParam(StateParam) do
  begin
    TmpIter := SelectedObjs.GetIterator;
    try
      NewObj := TmpIter.First as TObject2D;
    finally
      TmpIter.Free;
    end;
    if not (NewObj is TPrimitive2D) then
      raise
        ECADSysException.Create('TCAD2DEditSelectedObjects: Invalid param');
    NewParam := TCADPrgParam.Create(nil);
    NewParam.UserObject := NewObj;
  end;
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD2DEditSelectedPrimitive;
  NextState := TCAD2DEditPrimitive;
end;

 { ######################################################## }
     {*---- TCAD2D_BasicMode ----*}

constructor TCAD2D_BasicMode.Create(const CADPrg:
  TCADPrg; const StateParam: TCADPrgParam; var NextState:
  TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (Param is TCAD2DSelectObjectsParam) then
  begin
    Param.Free;
    Param := TCAD2DSelectObjectsParam.Create(
      4, TCAD2D_BasicMode);
  end;
    {raise
      ECADSysException.Create('TCAD2D_BasicMode: Invalid param');}
  fLastHot := nil;
  //CanBeSuspended := True;
  //(CADPrg as TCADPrg2D).Viewport2D.Repaint;
end;

procedure TCAD2D_BasicMode.TransformSelected(Transf: TTransf2D);
var
  ListOfObj: array of Longint;
  Iter: TGraphicObjIterator;
  Current: TGraphicObject;
  I: Integer;
begin
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
  begin
    //SelectedObjs.Transform(Transf);
    SetLength(ListOfObj, SelectedObjs.Count);
    Iter := SelectedObjs.GetIterator;
    try
      Current := Iter.First;
      I := 0;
      while Assigned(Current) do
      begin
        ListOfObj[I] := Current.ID;
        Current := Iter.Next;
        Inc(I);
      end;
    finally
      Iter.Free;
    end;
    Viewport2D.Drawing2D.TransformObjects(ListOfObj, Transf);
    //Viewport2D.Repaint;
  end;
end;

procedure TCAD2D_BasicMode.ShiftSelected(Shift: TPoint2D);
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
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
  begin
    if UseSnap then
      if (Viewport2D.GridDeltaX > 0) and (Viewport2D.GridDeltaY > 0) then
      begin
        Shift.X := RoundShift(Shift.X, Viewport2D.GridDeltaX);
        Shift.Y := RoundShift(Shift.Y, Viewport2D.GridDeltaY);
      end;
    if UseOrto then
      MakeOrto2D(Point2D(0, 0), Shift);
    TransformSelected(Translate2D(Shift.X, Shift.Y));
    //fGridDeltaX,       fGridDeltaY
  end;
end;

procedure TCAD2D_BasicMode.FlipSelected(DX, DY: TRealType);
var
  C: TPoint2D;
  A: TRealType;
begin
  C := CADPrg.Viewport.Drawing.GetSelectionCenter;
  A := (C.X * DX + C.Y * DY) / (Sqr(DX) + Sqr(DY));
  TransformSelected(Flip2D(A * DX, A * DY));
end;

procedure TCAD2D_BasicMode.RotateSelected(R: TRealType);
begin
  TransformSelected(RotateCenter2D(R,
    CADPrg.Viewport.Drawing.GetSelectionCenter));
end;

procedure TCAD2D_BasicMode.ScaleSelected(Scale: TRealType);
begin
  TransformSelected(ScaleCenter2D(Scale, Scale,
    CADPrg.Viewport.Drawing.GetSelectionCenter));
end;

procedure TCAD2D_BasicMode.CustomTransform;
var
  CP: TPoint2D;
  A: TRealType;
begin
  TransfForm.CP :=
    CADPrg.Viewport.Drawing.GetSelectionCenter;
  TransfForm.R :=
    CADPrg.Viewport.Drawing.GetSelectionExtension;
  if TransfForm.ShowModal = mrOK then
  begin
    TransformSelected(TransfForm.T);
  end;
  CADPrg.Viewport.Repaint;
end;

procedure TCAD2D_BasicMode.BackwardForward(Key: Integer);
var
  TmpIter: TGraphicObjIterator;
  TmpIter2: TGraphicObjIterator;
  Position: TGraphicObject;
  PositionID: Longint;
  Current: TGraphicObject;
  MoveMore: Boolean;
begin
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
  begin
    if SelectedObjs.Count = 0 then Exit;
    MoveMore := False;
    TmpIter := SelectedObjs.GetIterator;
    try
      TmpIter2 :=
        Viewport2D.Drawing2D.ObjectsIterator;
      try
        if Key = CADPRG_MoveForward then
        begin
          TmpIter2.Search(TmpIter.Last.ID);
          Position := TmpIter2.Next;
          if Position = nil then
            Position := TmpIter2.Last;
          MoveMore := True;
        end
        else if Key = CADPRG_MoveBackward then
        begin
          TmpIter2.Search(TmpIter.Last.ID);
          Position := TmpIter2.Prev;
          if Position = nil then
            Position := TmpIter2.First;
        end
        else if Key = CADPRG_MoveToFront then
        begin
          MoveMore := True;
          Position := TmpIter2.Last
        end
        else if Key = CADPRG_MoveToBack then
          Position := TmpIter2.First;
      finally
        TmpIter2.Free;
      end;
      Current := TmpIter.Last;
      while Current <> nil do
      begin
        if Current.ID <> Position.ID then
        begin
          Viewport2D.Drawing2D.MoveObject(Current.ID,
            Position.ID);
          if MoveMore then
            Viewport2D.Drawing2D.MoveObject(Position.ID,
              Current.ID);
        end;
        Current := TmpIter.Prev;
      end;
    finally
      TmpIter.Free;
    end;
    Viewport2D.Repaint;
  end;
end;

procedure TCAD2D_BasicMode.DuplicateSelected;
var
  ExIter: TExclusiveGraphicObjIterator;
  Current, NewObj: TGraphicObject;
  NewObjs: TGraphicObjList;
  T: TTransf2D;
begin
  with CADPrg as TCADPrg2D do
  begin
    NewObjs := TGraphicObjList.Create;
    NewObjs.FreeOnClear := False;
    try
      ExIter :=
        SelectedObjs.GetExclusiveIterator;
      try
        Current := ExIter.First;
        while Current <> nil do
        begin
          NewObj :=
            CADSysFindClassByName(Current.ClassName).CreateDupe(Current);
          NewObjs.Add(NewObj);
          Current := ExIter.Next;
        end;
      finally
        ExIter.Free;
      end;
      T := Translate2D(Viewport.GetPixelAperture.X * 5,
        Viewport.GetPixelAperture.Y * 5);
      ExIter := NewObjs.GetExclusiveIterator;
      try
        Current := ExIter.First;
        while Current <> nil do
        begin
          (Current as TObject2D).TransForm(T);
          Current := ExIter.Next;
        end;
      finally
        ExIter.Free;
      end;
      Viewport.Drawing.AddList(NewObjs);
      Viewport.Drawing.SelectionClear;
      Viewport.Drawing.SelectionAddList(NewObjs);
    finally
      NewObjs.Free;
      Viewport2D.Repaint;
    end;
  end;
end;

procedure TCAD2D_BasicMode.ConvertSelected(DestClass:
  TPrimitive2DClass);
var
  ExIter: TExclusiveGraphicObjIterator;
  Current, NewObj: TGraphicObject;
  NewObjs: TGraphicObjList;
  T: TTransf2D;
begin
  with CADPrg as TCADPrg2D do
  begin
    NewObjs := TGraphicObjList.Create;
    NewObjs.FreeOnClear := False;
    try
      ExIter :=
        SelectedObjs.GetExclusiveIterator;
      try
        Current := ExIter.First;
        while Current <> nil do
        begin
          NewObj :=
            CADSysFindClassByName(DestClass.ClassName).CreateDupe(Current);
          NewObjs.Add(NewObj);
          NewObj.ID := Current.ID;
          Current := ExIter.Next;
        end;
      finally
        ExIter.Free;
      end;
      T := Translate2D(Viewport.GetPixelAperture.X * 5,
        Viewport.GetPixelAperture.Y * 5);
      ExIter := NewObjs.GetExclusiveIterator;
      try
        Current := ExIter.First;
        {while Current <> nil do
        begin
          (Current as TObject2D).TransForm(T);
          Current := ExIter.Next;
        end;}
      finally
        ExIter.Free;
      end;
      Viewport.Drawing.DeleteSelected;
      Viewport.Drawing.AddList(NewObjs);
      //Viewport.Drawing.SelectionClear;
      Viewport.Drawing.SelectionAddList(NewObjs);
    finally
      NewObjs.Free;
      //Viewport2D.Repaint;
    end;
  end;
end;

procedure TCAD2D_BasicMode.ChangePrimitiveProperties(Obj:
  TPrimitive2D);
begin
  PropertiesForm.PPrimitive := Obj;
  if PropertiesForm.ShowModal = mrOK then
    CADPrg.Viewport.Drawing.NotifyChanged;
end;

function TCAD2D_BasicMode.OnEvent(Event:
  TCADPrgEvent; MouseButton: TCS4MouseButton;
  Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpObj: TObject2D;
  TmpDist: TRealType;
  CurrentCtrlPt: Integer;
  NewParam: TCADPrgParam;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key,
    NextState);
  if not Assigned(Param) then
    Exit;
  if (fMode = im_Rotate) or (fMode = im_Move) or
    (fMode = im_Drag) then
  begin
    OnEventTransformMode(Event, MouseButton, Shift,
      Key, NextState, Result);
    Exit;
  end;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
    case Event of
      ceUserDefined:
        if Key = CADPRG_CANCEL then
        begin
          Param.Free;
          Param := nil;
          NextState := DefaultState;
          Result := True;
        end
        else if Key = CADPRG_ACCEPT then
        begin
          IgnoreEvents := True;
          try
            Viewport2D.Refresh;
          finally
            IgnoreEvents := False;
          end;
          if Assigned(AfterState) then
            NextState := AfterState
          else
          begin
            Param.Free;
            Param := nil;
          end;
          Result := True;
        end
        else if Key = CADPRG_DuplicateSelected then
          DuplicateSelected
        else if Key = CADPRG_DeleteSelected then
        begin
          Viewport.Drawing.DeleteSelected;
          Viewport.Repaint;
        end
        else if Key = CADPRG_SelectAll then
        begin
          Viewport.Drawing.SelectAll;
          Viewport.Repaint;
        end
        else if (Key >= CADPRG_ConvertSelected)
          and (Key < CADPRG_ConvertSelected + 50) then
        begin
          //ConvertSelected(TPolyline2D);
          //ConvertSelected(TLine2D);
          ConvertSelected(TPrimitive2DClass(CADSysFindClassByIndex(
            Key - CADPRG_ConvertSelected + 3)));
          Viewport.Repaint;
        end
        else if Key = CADPRG_CustomTransform then
        begin
          CustomTransform;
        end
        else if Key = CADPRG_ClipboardCopy then
        begin
          Viewport2D.Drawing2D.CopySelectionToClipboard;
        end
        else if Key = CADPRG_ClipboardPaste then
        begin
          Viewport2D.Drawing2D.PasteFromClipboard;
          Viewport.Repaint;
        end
        else if Key = CADPRG_ClipboardCut then
        begin
          Viewport2D.Drawing2D.CopySelectionToClipboard;
          Viewport.Drawing.DeleteSelected;
          Viewport.Repaint;
        end
        else if Key = CADPRG_MoveUp then
          ShiftSelected(Point2D(0,
            Viewport.GetPixelAperture.Y * 20))
        else if Key = CADPRG_MoveDown then
          ShiftSelected(Point2D(0,
            -Viewport.GetPixelAperture.Y * 20))
        else if Key = CADPRG_MoveLeft then
          ShiftSelected(Point2D(-Viewport.GetPixelAperture.X *
            20, 0))
        else if Key = CADPRG_MoveRight then
          ShiftSelected(Point2D(Viewport.GetPixelAperture.X *
            20, 0))
        else if Key = CADPRG_MoveUpPixel then
          ShiftSelected(Point2D(0, Viewport.GetPixelAperture.Y))
        else if Key = CADPRG_MoveDownPixel then
          ShiftSelected(Point2D(0, -Viewport.GetPixelAperture.Y))
        else if Key = CADPRG_MoveLeftPixel then
          ShiftSelected(Point2D(-Viewport.GetPixelAperture.X, 0))
        else if Key = CADPRG_MoveRightPixel then
          ShiftSelected(Point2D(Viewport.GetPixelAperture.X, 0))
        else if Key = CADPRG_SelNext then
        begin
          Viewport.Drawing.SelectNext(1);
          Viewport.Repaint;
        end
        else if Key = CADPRG_SelPrev then
        begin
          Viewport.Drawing.SelectNext(-1);
          Viewport.Repaint;
        end
        else if Key = CADPRG_FlipV then
          FlipSelected(0, 1)
        else if Key = CADPRG_FlipH then
          FlipSelected(1, 0)
        else if Key = CADPRG_RotateCounterclockW then
          RotateSelected(Pi / 2)
        else if Key = CADPRG_RotateClockW then
          RotateSelected(-Pi / 2)
        else if Key = CADPRG_RotateCounterclockWDegree then
          RotateSelected(Pi / 180)
        else if Key = CADPRG_RotateClockWDegree then
          RotateSelected(-Pi / 180)
        else if Key = CADPRG_Grow10 then
          ScaleSelected(1.1)
        else if Key = CADPRG_Shrink10 then
          ScaleSelected(1 / 1.1)
        else if Key = CADPRG_Grow1 then
          ScaleSelected(1.01)
        else if Key = CADPRG_Shrink1 then
          ScaleSelected(1 / 1.01)
        else if (Key = CADPRG_MoveForward) or
          (Key = CADPRG_MoveBackward) or
          (Key = CADPRG_MoveToFront) or
          (Key = CADPRG_MoveToBack) then
          BackwardForward(Key)
        else if Key = CADPRG_StartRotate then
        begin
          fMode := im_Rotate;
          fBasePt := CADPrg.Viewport.Drawing.GetSelectionCenter;
          CurrPoint2D := CurrentViewportSnappedPoint;
          fBaseAngle := ArcTan2(CurrPoint2D.Y - fBasePt.Y,
            CurrPoint2D.X - fBasePt.X);
          fCurrTransf := GetRotateTransform;
          DrawOSDTransform(Viewport2D);
        end
        else if Key = CADPRG_StartMove then
        begin
          fMode := im_Move;
          fBasePt := CADPrg.Viewport.Drawing.GetSelectionCenter;
          fCurrTransf := GetMoveTransform;
          DrawOSDTransform(Viewport2D);
        end
        else if Key = CADPRG_DuplicateSelected then
          DuplicateSelected
            ;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          CurrPoint2D := CurrentViewportPoint;
          fLastPt := CurrPoint2D;
          TmpObj := Viewport2D.PickObject(CurrPoint2D,
            fApertureSize, False, fLastSelectedCtrlPoint);
          if fLastSelectedCtrlPoint < PICK_INOBJECT
            then TmpObj := nil;
          {if (fLastSelectedCtrlPoint < PICK_INBBOX)
            or not (TmpObj is SelectionFilter) then Exit;}
          if Key = VK_CONTROL then // Ctrl pressed
          begin
            if (fLastSelectedCtrlPoint = PICK_ONOBJECT) {or
              (fLastSelectedCtrlPoint = PICK_INOBJECT)}then
            begin
              if SelectedObjs.Find(TmpObj.ID) = nil then Exit;
              if not (TmpObj is TOutline2D) then Exit;
              fLastSelectedCtrlPoint :=
                (TmpObj as TOutline2D).OnProfile(CurrPoint2D,
                Viewport.GetAperture(fApertureSize));
              if fLastSelectedCtrlPoint < 0 then Exit;
              (TmpObj as TOutline2D).InsertControlPoint(
                fLastSelectedCtrlPoint + 1, CurrPoint2D);
              Viewport2D.Repaint;
              Exit;
            end;
            if fLastSelectedCtrlPoint < 0 then Exit;
            if SelectedObjs.Find(TmpObj.ID) = nil then Exit;
            if not (TmpObj is TPrimitive2D) then Exit;
            (TmpObj as TPrimitive2D).DeleteControlPoint(
              fLastSelectedCtrlPoint);
            Viewport2D.Repaint;
            Exit;
          end;
          if Key <> VK_SHIFT then // Shift not pressed
          begin // Select clicked object
            if not Assigned(TmpObj) then
            begin
              Viewport.Drawing.SelectionClear;
              Viewport2D.Repaint;
              Exit;
            end;
            if SelectedObjs.Find(TmpObj.ID) <> nil then Exit;
            Viewport.Drawing.SelectionClear;
            Viewport.Drawing.SelectionAdd(TmpObj);
            Viewport2D.Repaint;
            Exit;
          end;
          if not Assigned(TmpObj) then Exit;
          // Shift pressed: +/- object
          if not Viewport.Drawing.SelectionRemove(TmpObj)
            then Viewport.Drawing.SelectionAdd(TmpObj);
          Viewport2D.Repaint;
        end;
      ceMouseMove:
        begin
          if SSLeft in Shift then
          begin //Start drag
            if PointDistance2D(CurrentViewportPoint, fLastPt)
              < Viewport.GetAperture(3) then Exit;
            CurrPoint2D := fLastPt;
            TmpObj := Viewport2D.PickObject(CurrPoint2D,
              fApertureSize, False, fLastSelectedCtrlPoint);
            if fLastSelectedCtrlPoint < PICK_INOBJECT
              then TmpObj := nil;
            if Assigned(TmpObj) then
            begin
              CurrentCtrlPt := TmpObj.OnMe(CurrPoint2D,
                Viewport.GetAperture(fApertureSize), TmpDist);
              if (CurrentCtrlPt >= 0) and
                (SelectedObjs.Find(TmpObj.ID) <> nil) then
              begin
                NewParam :=
                  TCAD2DMoveControlPointParam.Create(nil,
                  CurrentCtrlPt);
                NewParam.UserObject := TmpObj;
                Param.Free;
                Param := nil;
                StartOperation(TCAD2DMoveControlPoint,
                  NewParam);
                Exit;
              end;
            end;
            if SelectedObjs.Count = 0 then
            begin
              if not Assigned(TmpObj) then Exit;
              Viewport.Drawing.SelectionAdd(TmpObj);
              Viewport2D.Repaint;
            end;
            fMode := im_Drag;
            fBasePt := CurrPoint2D;
            fCurrTransf := GetMoveTransform;
            DrawOSDTransform(Viewport2D);
          end;
        end;
      cePaint:
        begin
        end;
      ceMouseDblClick:
        begin
          TmpObj := Viewport2D.PickObject(CurrentViewportPoint,
            fApertureSize, False, fLastSelectedCtrlPoint);
          if fLastSelectedCtrlPoint < PICK_INOBJECT
            then TmpObj := nil;
          if TmpObj is TPrimitive2D then
          begin
            ChangePrimitiveProperties(TmpObj as TPrimitive2D);
            Viewport2D.Repaint;
          end;
        end;
    end;
end;

procedure TCAD2D_BasicMode.DrawOSDTransform(Viewport:
  TCADViewport2D);
var
  Obj: TGraphicObject;
  Iter: TGraphicObjIterator;
begin
  with CADPrg as TCADPrg2D,
    Param as TCAD2DSelectObjectsParam do
    with Viewport2D do
    begin
      OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
      Iter := SelectedObjs.GetIterator;
      try
        Obj := Iter.First;
        while Assigned(Obj) do
        begin
          if not (Obj is TObject2D) then Continue;
          //Obj.ModelTransform := fCurrTransf;
          //DrawObject2DWithRubber(TmpObj, False);
          if not TObject2D(Obj).IsVisible(VisualRect, DrawMode)
            then
            Exit;
          with Obj as TObject2D do
          begin
            OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
            OnScreenCanvas.Canvas.Brush.Color :=
              RubberPen.Color;
            OnScreenCanvas.Canvas.Brush.Style := bsHorizontal;
              //bsSolid;
            Draw(MultiplyTransform2D(fCurrTransf,
              ViewportToScreenTransform),
              OnScreenCanvas, RectToRect2D(ClientRect),
              DrawMode or DRAWMODE_OutlineOnly);
          end;
          Obj := Iter.Next as TObject2D;
        end;
      finally
        Iter.Free;
      end;
    //DrawWithFrame(Viewport)    //DrawWithoutFrame(Viewport);
    end;
end;

function TCAD2D_BasicMode.GetRotateTransform: TTransf2D;
var
  CurrPoint2D: TPoint2D;
begin
  with CADPrg as TCADPrg2D do
  begin
    CurrPoint2D := CurrentViewportSnappedPoint;
    if UseOrto then
      MakeOrto2D(fBasePt, CurrPoint2D);
    Result := RotateCenter2D(
      ArcTan2(CurrPoint2D.Y - fBasePt.Y,
      CurrPoint2D.X - fBasePt.X) - fBaseAngle, fBasePt);
  end;
end;

function TCAD2D_BasicMode.GetMoveTransform: TTransf2D;
var
  CurrPoint2D: TPoint2D;
begin
  with CADPrg as TCADPrg2D do
  begin
    CurrPoint2D := CurrentViewportSnappedPoint;
    if UseOrto then
      MakeOrto2D(fBasePt, CurrPoint2D);
    Result := Translate2D(CurrPoint2D.X - fBasePt.X,
      CurrPoint2D.Y - fBasePt.Y);
  end;
end;

procedure TCAD2D_BasicMode.OnEventTransformMode(Event:
  TCADPrgEvent;
  MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
  var NextState: TCADStateClass; Result: Boolean);
  procedure ConfirmTransform;
  begin
    fMode := im_Normal;
    with (CADPrg as TCADPrg2D) do
    begin
      TransformSelected(fCurrTransf);
      //Viewport2D.Repaint;
    end;
  end;
  procedure CancelTransform;
  begin
    fMode := im_Normal;
    with (CADPrg as TCADPrg2D) do
    begin
      Viewport2D.Refresh;
    end;
  end;
begin
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam
    do
    case Event of
      ceUserDefined:
        begin
        //if Key = CADPRG_CANCEL then
        end;
      ceKeyDown: if Key = VK_ESCAPE then
        begin
          CancelTransform;
        end;
      ceMouseDown: if MouseButton = cmbLeft then
        begin
          ConfirmTransform;
        end
        else if MouseButton = cmbRight then
        begin
          CancelTransform;
        end;
      ceMouseMove:
        begin
          if (fMode = im_Drag) and (Shift <> [SSLeft]) then
          begin
            CancelTransform;
            Exit;
          end;
          DrawOSDTransform(Viewport2D);
          case fMode of
            im_Rotate: fCurrTransf := GetRotateTransform;
            im_Move, im_Drag: fCurrTransf := GetMoveTransform;
          end;
          DrawOSDTransform(Viewport2D);
        end;
      ceMouseUp:
        if fMode = im_Drag then
        begin
          ConfirmTransform;
          Exit;
        end;
      cePaint:
        begin
          DrawOSDTransform(Viewport2D);
        end;
    end;
end;

procedure TCAD2D_BasicMode.OnStop;
begin
  Param.Free;
  Param := nil;
end;

procedure TCAD2D_BasicMode.OnResume(const State: TClass; const
  SusParam: TCADPrgParam);
begin
  (CADPrg as TCADPrg2D).Viewport2D.Repaint;
end;

procedure TCAD2D_BasicMode.OnSuspend(const State: TClass; const
  SusParam: TCADPrgParam);
begin
  //SelectionClear;
  (CADPrg as TCADPrg2D).Viewport2D.Repaint;
end;

end.

