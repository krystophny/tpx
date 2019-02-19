unit GObjBase;

{$MODE Delphi}

// A module for basic graphical objects, lists of objects and layers

interface

uses Classes, SysUtils,
{$IFDEF VER140}
  WinBasic,
{$ELSE}
  LCLIntf, LMessages, LCLType, {LazBasic,}
{$ENDIF}
  Geometry, Devices, SysBasic
  ;

const
  {:
  }
  MAX_REGISTERED_CLASSES = 512;

type

  TGraphicObjList = class;
  TGraphicObject = class;
  TGraphicObjectClass = class of TGraphicObject;
  TObject2D = class;
  TObject2DClass = class of TObject2D;
  TLayers = class;

{: This exception is raised when an attempt is made to use an
   unregistered shape class in a method that need a registered
   one.

   <B=Note>: When this exception is raised add the following code
   in the initialization section of one of your units:

   <Code=
    TpXRegisterClass(<<unused index>>, <<unregistered class type>>);
   >

   Where <I=unused index> must be an unused index above 150.
}
  ETpX_ObjClassNotFound = class(Exception);

// This exception is raised when you try to lock a list that is already
// locked or unlock a list which is already unlocked.
  ETpX_ListLocked = class(Exception);
{: This exception is raised when a specified object is not found
    in the list.
}
  ETpX_ListObjNotFound = class(ETpX_SysException);
  ETpX_NoLayers = class(ETpX_SysException);

{: This is the base class for all kinds of graphical objects.

   A graphical object is defined here as an object that:

   <LI=is identifiable by means of an ID number (<See Property=TGraphicObject@ID>)>
   <LI=is on a layer from which inherited a pen and brush for drawing (<See Property=TGraphicObject@Layer>)>
   <LI=may be visibility or not (<See Property=TGraphicObject@Visible>)>
   <LI=may be enabled or not for picking (<See Property=TGraphicObject@Enabled>)>
   <LI=may be saved or not to a stream (<See Property=TGraphicObject@ToBeSaved>)>

   A graphic object define also a common interface for management and storing
   that every object that must be drawed on a viewport must specialize.

   This class cannot be instantiated directly (and even derived from) but
   you must use instead the classes <See Class=TObject2D>
   and the classes derived from them. See the unit <See unit=GObjects>.
}
  TGraphicObject = class(TInterfacedObject)
  private
    fID, fTag: Integer;
    fLayer: Byte;
    fVisible, fEnabled, fToBeSaved: Boolean;
    fOnChange: TNotifyEvent;
    fOwnerDrawing: TObject;
    fParentDrawing: TObject;
  protected
{: This method is called when the extensions and shape of the object must
   be updated after some change to the object state was done.

   When you define a new shape you may want to redefine this method in
   order to keep the object in a coerent state. The library call this method
   whenever it has changed the object. Also when you manipulate object you
   must call the <See Method=TGraphicObject@UpdateExtension>, that in order
   call this method, after the object is changed.

   <B=Note>: Some of the objects already defined in the library define this
   method to compute the bounding box of the object and to set some object's
   specific properties, so don't forget to call the inherited method somewhere
   in you implementation.

   <B=Note>: In this method don't fire the <See Property=TGraphicObject@OnChange>
   event.
}
    procedure _UpdateExtension; virtual; abstract;
    function GetParentDrawing: TObject;
  public
{: This is the constructor of the graphic object.

   <I=ID> is the identifier of the the object (<See Property=TGraphicObject@ID>).

   The constructor also set:

   <LI=<See Property=TGraphicObject@Layer> to 0>
   <LI=<See Property=TGraphicObject@Visible> to <B=True> >
   <LI=<See Property=TGraphicObject@Enabled> to <B=True> >
   <LI=<See Property=TGraphicObject@ToBeSaved> to <B=True> >
}
    constructor Create(ID: Integer); virtual;
{: This constructor is used to create an instance of the class and initialize
   its state with the one saved to a stream with a previous call to
   <See Method=TGraphicObject@SaveToStream>.

   <I=Stream> is the stream that contains the saved instance's state.
   This stream must be positioned at the starting of the saved image. Note
   that the saved image must be made with the current library version.
   This method is automatically called by the library itself whenever a
   drawing is loaded.

   If the saved object doesn't correspond to the one that the method expect,
   the library may be left in a corrupted state and normally one or more
   <I=EAccessViolation> exceptions will be raised. To resolve this problem
   the library save an header at the start of a drawing to ensure
   the version correctness of the file.

   When you derive a new shape you have to override this method and
   give your own implementation. However remember to call the inherited
   method at the start of your implementation.

   Following the above rules you are ensured that your drawings are
   always readed by the library.

   See <See=Object's Persistance@PERSISTANCE> for details about the PERSISTANCE mecanism of
   the library.
}
{: <New topic=PERSISTANCE@TpX 4 - Drawing persistance>
   The library is able to save and retrive the current drawing in a
   <See Class=TDrawing> control into a stream or file.

   In order to keep the library customizable and easy to extend, this
   streaming facility is able to save user defined object as well as
   library ones. What you need to do is to simply define how to save
   the state and retrive it from a stream by defing the
   <See Method=TGraphicObject@CreateFromStream> and
   <See Method=TGraphicObject@SaveToStream>. After that you have to
   register your class when your application start by using the
   <See Function=TpXRegisterClass> procedure.
   With these steps your objects will be saved and retrived automatically
   by the library.

   Simply the library save a <I=class index> before saving the object,
   and then use this index to obtain the <I=class reference> to create
   the object when it must be loaded.

   Using this method the library is also able to automatically load and
   convert drawing created with old version of the library without
   need to create an explicit version converter.

   Most of the shapes in the library use this method extensively and
   you have only to save you own datas if you need by using simply
   <I=Read> and <I=Write> method of <I=TStream>.
   Also you don't need to know if your drawing is to be saved in a
   file, in a memory stream or also into a database record.
}
    constructor CreateFromStream(const Stream: TStream); virtual;
  //TSY:
    constructor CreateDupe(Obj: TGraphicObject); virtual;
{: This method save an image of the current state of the object to a stream.

   <I=Stream> is the stream on which save the image.

   When you derive a new shape you have to override this method and
   augument it with what you need to save the object state. This method
   will be called automatically by the library when the current drawing
   is being saved but you may want to call it directly to save a contained
   shape of another object. If the <See Property=TGraphicObject@ToBeSaved>
   property is set to <B=False> the object will not be saved by the library.

   See <See=Object's Persistance@PERSISTANCE> for details about the PERSISTANCE mecanism of
   the library.
}
    procedure SaveToStream(const Stream: TStream); virtual;
{: This method is used to make deep copy of the object by obtaining state
   informations from another.

   <I=Obj> is the object from which copy the needed informations. Note that
   this object doen't need to be of the same class of the object into which
   copy these informations. You have to check this class before accessing
   the informations. You have also to call the inherited method if you
   plan to override this method so the responsability chain is maintained.

   The parents classes of the objects from which this method is called
   are traversed to copy as much as possible from the given <I=Obj>.

   <B=Note>: If you use memory allocation remember to copy the memory
   and not simply copy the reference. This is also true with contained
   objects.
}
    procedure Assign(const Obj: TGraphicObject); virtual;
{: This method is called when the extensions and shape of the object must
   be updated after some change to the object state was done.

   When you define a new shape you may want to redefine this method in
   order to keep the object in a coerent state. The library call this method
   whenever it has changed the object. Also when you manipulate object you
   must call this method after the object is changed.
   You may want to override the <See Method=TGraphicObject@_UpdateExtension>
   to give special updates for your shape.

   This method fires a <See Property=TGraphicObject@OnChange> event.
}
    procedure UpdateExtension(Sender: TObject);
{: This property defines the ID of the object.

   Any graphic object is identified through th library by means of a
   <I=Integer> number that must univocally identify it. The library by
   itself doesn't check for uniqueness of these IDs so you have to ensure
   it by yourself.
}
    property ID: Integer read fID write fID;
{: This property contains the layer on which the object lies.

   Any graphic object is associated with a layer from which they get
   their drawing caracteristic such as color, line style, fill color
   and so on.

   An object also get the behavior of this layer such us visibility
   and enabling to picking that have priority on the ones of the
   object.

   If you want to have object specific color you have to change the
   color in the <I=Draw> method of the shape. You don't need to
   revert the object to the one of the layer after you have modified the
   current pen color, because the library will reset this color before
   the <I=Draw> method is called.

   By Dafault this property is set to <B=0>.
}
    property Layer: Byte read fLayer write fLayer;
{: This property specify the visibility of the object.

   If it is <B=True> then the object is drawed on the viewport otherwise
   it will not be drawed (the viewport on which it will be drawed depend
   on the <See Class=TDrawing> control into which the object is stored).

   The <See Property=TLayer@Visible> setting of the layer on which the
   object lies has the priority on this property.

   By Dafault this property is set to <B=True>.
}
    property Visible: Boolean read fVisible write fVisible;
{: This property specify the behaviour of the object when it is picked.

   If it is <B=False> then the object will not be considered by the picking
   methods, otherwise it may be picked.

   The <See Property=TLayer@Active> setting of the layer on which the
   object lies has the priority on this property.

   By Dafault this property is set to <B=True>.
}
    property Enabled: Boolean read fEnabled write fEnabled;
{: This property specify if the object is to be saved in a drawing.

   If it is <B=False> then the object will not be saved into a drawing stream,
   otherwise it will be.

   The <See Property=TLayer@Streamable> setting of the layer on which the
   object lies has the priority on this property.

   By Dafault this property is set to <B=True>.
}
    property ToBeSaved: Boolean read fToBeSaved write
      fToBeSaved;
{: This read-only property contains the drawing in which the object was added.
   Although you can add the same object to more than one drawing (and you can
   do that safely by removing all the references from the drawings without deleting
   the object) it isn't a good practice and it is discouraged because this
   property will not still have any meaning.
}
    property OwnerDrawing: TObject read fOwnerDrawing
      write fOwnerDrawing;
//TSY: A GraphicObject needs some drawing information to be rendered
// properly on a device. If GraphicObject is not part of a drawing
// (OwnerDrawing is nil) it can use ParentDrawing instead
    property ParentDrawing: TObject read GetParentDrawing write
      fParentDrawing;
{: This property may be used to store some user's defined data
   like object references.
   It is not used by the library.
}
    property Tag: Integer read fTag write fTag;
{: This is property can store an event-handler for the event <I=OnChange>
   that occour whenever an object is modified (actually the event is
   fired only when the user or the library call <See Method=TGraphicObject@UpdateExtension>
   method).

   You may want to use this event as described in the description of the
   <See Property=TGraphicObject@Tag> property.
}
    property OnChange: TNotifyEvent read fOnChange write
      fOnChange;
  end;

// List of objects

  POL_Item = ^TOL_Item;

  TOL_Item = record
    Obj: TObject; // An object.
    Next, Prev: POL_Item; // Linked list pointers
  end;

// A class for a doubly linked list of objects.

//   To traverse the list use FirstObj, NewxtObj.

//   A procedure which modifies the list (using Pop, Add, etc.)
//   can lock it to prevent ...

  TObjList = class(TObject)
  private
    fHead, fTail, fCurrent: POL_Item;
    fFreeOnDelete: Boolean;
    fCount: Integer;
    fLocked: Boolean;
    function _CreateItem(const Prev, Next: POL_Item;
      const Obj: TObject): POL_Item;
// Insert object before Item
    function InsertBefore(
      const Item: POL_Item; const Obj: TObject): POL_Item;
// Insert objects from another list before Item
    procedure InsertBefore_FromList(
      const Item: POL_Item; const Lst: TObjList);
    function RemoveItem(const Item: POL_Item): TObject; virtual;
    procedure DeleteItem(const Item: POL_Item); virtual;
    procedure _TrancateFromSelf(Item: POL_Item);
// Trancate the list after Item
    procedure TrancateAfter(const Item: POL_Item);
// Trancate the list starting from Item
    procedure TrancateAt(const Item: POL_Item);
  public
// Sets FreeOnDelete to True
    constructor Create;
//  If the FreeOnDelete is True then the objects in the list will also be freed
    destructor Destroy; override;
// Lock the list
    procedure Lock;
// Unlock the list
    procedure Unlock;
// Add an object to the end of list
    procedure Add(const Obj: TObject);
// Remove all objects from the list
// The objects will be freed if FreeOnDelete is True
    procedure Clear;
// Return last object of the list
    function Peek: TObject;
// Remove and return last object of the list
    function Pop: TObject;
// Start internal iterator from the head and return the first object
    function FirstObj: TObject;
// Return current object
    function CurrentObj: TObject;
// Return next object
    function NextObj: TObject;
// Start internal iterator from the tail and return the last object
    function LastObj: TObject;
// Return previous object
    function PrevObj: TObject;
// Specifies whether the list owns its objects
// (whether the objects in the list must be freed when deleted from it)
// This influences delete, truncate and clear methods
    property FreeOnDelete: Boolean read fFreeOnDelete write
      fFreeOnDelete;
// The number of objects in the list
    property Count: Integer read fCount;
  end;

//  List of graphical objects

{
   The objects are referenced by their IDs.
   The list doens't check for duplicated IDs so you have to ensure
   uniqueness yourself.

}
  TGraphicObjList = class(TObjList)
  private
    function RemoveItem(const Item: POL_Item): TObject; override;
  public
    constructor Create;
    destructor Destroy; override;
// Return last object of the list
    function Peek: TGraphicObject;
// Remove and return last object of the list
    function Pop: TGraphicObject;
// Start internal iterator from the head and return the first object
    function FirstObj: TGraphicObject;
// Return current object
    function CurrentObj: TGraphicObject;
// Return next object
    function NextObj: TGraphicObject;
// Start internal iterator from the tail and return the last object
    function LastObj: TGraphicObject;
// Return previous object
    function PrevObj: TGraphicObject;
// Find the first object with a given ID
    function FindObjByID(const ID: Integer): TGraphicObject;
// Delete current object from the list
    function DeleteCurrent: TGraphicObject;
// Remove current object from the list
    function RemoveCurrent: TGraphicObject;
// Add an object to the list
    procedure Add(const Obj: TGraphicObject);
// Add objects from another list
    procedure AddFromList(const Lst: TGraphicObjList);
// Insert an object into the list
//   The added object Obj will be inserted after the object in the
//   list with ID equal to IDInsertPoint. If no such object is present
//   an exception will be raised.
//   The list doesn't check for uniqueness of the inserted object.
    procedure Insert(const IDInsertPoint: Integer; const Obj:
      TGraphicObject);
{: This method inserts a list of objects into the list.

   The added objects will be inserted after the object in the
   list with ID equal to <I=IDInsertPoint>.
}
    procedure InsertFromList(const IDInsertPoint: Integer;
      const Lst: TGraphicObjList);
{: This method moves an object into the list.
   The object with ID equal to IDToMove will be moved before the
   object in the list with ID equal to IDInsertionPoint.
   This method is useful if you want to change the
   drawing order in the list of objects.
}
    procedure Move(const IDToMove, IDInsertPoint: Integer);
{: This method deletes an object from the list.
   The object with ID equal to ID will be deleted. If the list has
   the property FreeOnDelete set to True then the object will be
   deleted by calling its Free method.
}
    function Delete(const ID: Integer): Boolean;
// Same as FindObjByID, but locks the list
    function Find(const ID: Integer): TGraphicObject;
// Replace current object with another object
    procedure ReplaceDeleteCurrent(const Obj: TGraphicObject);
{: This method remove all the objectû from the list.
   The objects will be deleted if the property FreeOnDelete
   is set to True.
}
    procedure Clear;
    procedure Transform(const T: TTransf2D);
//TSY: reproduced from former viewport method with the same name
    {: This method returns an object that has the point <I=Pt> on it.

       The method traverses the list of objects to find an object
       that is at a distance from <I=Pt> less than <I=Aperture>. If
       that object is found then it will be returned by the method,
       otherwise the method will returns <B=nil>.

       <I=Pt> is the point of the picking; <I=Aperture> is the width
       of the picking region in <B=pixel>.

       <I=NPoint> will contains (if an object will be picked) one of
       the following values:

       <LI=<I=PICK_NOOBJECT> if no object is found at <I=Pt>.>
       <LI=<I=PICK_INBBOX> if an object is found that has <I=Pt> in its bounding box.>
       <LI=<I=PICK_ONOBJECT> if an object is found that has <I=Pt> on its outline.>
       <LI=<I=PICK_INOBJECT> if an object is found that has <I=Pt> in its interior.>
       <LI=a value equal or greater than zero if an object is found
         that has <I=Pt> on one of its control points.>

       The above value are ordered from the lowest priority to the
       greater priority. If more than one of the above condition is
       matched the value with the greater priority is returned.

       If there are more than one object at the point <I=Pt> the
       following rules are applied:

       <LI=The object with the greater priority value is returned>
       <LI=The last object added to the drawing is returned.>

       The display list is traversed until the last object is
       encountered and this may require a bit of time in the
       case of a complex object. If you are satisfied to found
       the first encountered object that is picked by the point
       <I=Pt> you may want to set <I=FirstFound> to <B=True>.

       <B=Note>: The method use the <See Method=TObject2D@OnMe> method
       to check for the picking. If you need a special picking function
       you may want to call that method.
    }
    function PickObject(const P: TPoint2D;
      Layers: TLayers; const Aperture: TRealType;
      const VisualRect: TRect2D; const PickFilter:
      TObject2DClass;
      const FirstFound: Boolean; var NPoint: Integer):
      TObject2D;
  end;


  {: A <I=2D graphic object> is a graphic object that is defined on
     a 2D plane.

     The 2D object is defined in a special coordinate system referred
     here as the <I=model coordinate system or model system>. When an object is used by the
     library, the world coordinate system (or world system) are
     needed instead. So the object in model system is transformed by an
     matrix transform to obtain the object in the world system.
     This transform is called the <See Property=TObject2D@ModelTransform>.
     So when an object is drawed, for example, its points are first
     transformed from model system to world system by using this matrix,
     then the view transform (that the concatenation of the
     projection matrix with the mapping matrix) tranform these points
     to obtain the points in the screen system that are easily displayed.

     The 2D object has a 2D bounding box, that is the smallest
     axis aligned rectangle that fully contains the object.
     This rectangle is always in the world coordinate system.
     The bounding box must be computed in the
     <See Property=TGraphicObject@_UpdateExtension> method that have
     to be redefined.

     The model matrix is split in two parts. The first one, called
     the <I=current model matrix>, can be removed at any time or
     can be post multiplied with a new one.

     The second one, called the <I=saved model matrix>, is stored
     permanently inside the object and can only be removed
     by using the <See Method=TObject2D@RemoveTransform> method
     or substituted with the actual model matrix with the
     <See Method=TObject2D@ApplyTransform> method.
     The actual model matrix is the concatenation of the
     <I=saved model matrix> with the <I=current model matrix> in
     this order (remember that the order of concatenation of transform matrix
     is important).

     The presence of the <I=current model matrix> is helpfull to
     create operations that transform an object interactilvely and
     that can be cancelled if they are not what the user want to do.

     <B=Note>: To save space the <I=current model matrix> and the
     <I=saved model matrix> are stored in the object only if they
     are not equal to the <See const=IdentityTransf2D> constant.
  }
  TObject2D = class(TGraphicObject)
  private
    fDrawBoundingBox: Boolean;
    //TSY:
    fHasControlPoints: Boolean;
    fBoundingBox: TRect2D;

  protected
    {: This field contains the bounding box of the object.

       A bounding box is the smallest axis aligned rectangle that fully contains the object.
       This rectangle is always in the world coordinate system and.
       The bounding box must be computed in the
       <See Property=TGraphicObject@_UpdateExtension> method that have
       to be redefined.

       Use this field directly to assign the computed bunding box.
    }
    property WritableBBox: TRect2D read fBoundingBox write
      fBoundingBox;
  public
    constructor Create(ID: Integer); override;
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream);
      override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method transform the object with a give transformation matrix.

       The <I=T> transform matrix will be post multiplied with the
       <I=current model matrix> and the result will take the place of
       the <I=current model matrix>.

       See the <See Class=TObject2D> class for details about the
       model matrix of a 2D object.
    }
    procedure Transform(const T: TTransf2D); dynamic;
    {: This method moves the object.

       <I=DragPt> is the base point of the movement and <I=ToPt>
       is the destination point.
    }
    procedure MoveTo(ToPt, DragPt: TPoint2D);
    {: This method renders the object on a device.
       A 2D object have to implement this method to draw itself
       appropriately.
    }
    procedure DeviceDraw(Transf: TTransf2D;
      const Dvc: TDevice; const ClipRect2D: TRect2D);
      virtual;
      abstract;
    {: This method returns <B=True> if the object is visible in the
       portion of the view plane given by the
       <See Property=TViewport@VisualRect> property of the viewport that
       called the method.

       This method is used to prune the object that must not be
       drawed to save time in the drawing process.

       <I=Clip> is the portion of the view plane that must be rendered, and

       By default this method returns <B=True> if the bounding box
       of the object is contained (also partially) in the <I=Clip>
       rectangle.
    }
    function IsVisible(const Clip: TRect2D): Boolean;
      virtual;
    {: This method draws only the control points of the object.

       An 2D object may have a set of points that are called
       <I=control points> and that control the shape of the
       object. For example a circle may have two control points;
       the center of the circle and a point on the circle that
       specify the radious of it.

       <I=VT> is the the mapping transform that can be obtained
       with the <See Property=TViewport@ViewportToScreenTransform> property.
       <I=Cnv> is the canvas on which draw the control points, and
       <I=Width> is the width in pixel of the control points (that are
       drawed as a square).
       <I=ClipRect> is the 2D pixel drawing area in 2D coordinates, you
       can use it in all of the drawing methods.

       This method uses the linked <See Class=TObject2DHandler> to
       draw the control points. If no handler is linked then no control point
       is drawed.
    }
    procedure DrawControlPoints(const VT: TTransf2D;
      const ClipRect2D: TRect2D; const Width: Integer);
      dynamic;
    {: This returns the part of the object that is picked by a point.

       <I=Pt> is the picking point, <I=Aperture> is the picking aperture
       and <I=Distance> will contains the distance from <I=Pt> to
       the object.

       The method returns one of these values (ordered from the highest
       priority to the lowest priority):

       <LI=<I=PICK_NOOBJECT> if <I=Pt> is not on the object. Distance
       will became <See const=MaxCoord>.>
       <LI=<I=PICK_INBBOX> if <I=Pt> is inside the bounding box of the
       object. Distance will be equal to Aperture.>
       <LI=<I=PICK_ONOBJECT> if <I=Pt> is on the outline of the object
       with a distance less than Aperture. Distance will be that
       distance.>
       <LI=<I=PICK_INOBJECT> if <I=Pt> is inside the outline of the
       object. Distance will be set to Aperture.>
       <LI=A value greater than or equal to zero if <I=Pt> is on any of
       the control points, in this case the resulting value is that
       control point. Distance will be the distance form the control
       point.

       You must use this convention in your implementations of
       the method. (You can use the inherited method if some of these
       condition is the same as in the base class).
    }
    function OnMe(P: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; dynamic;
    {: This property contains the bounding box of the object.

       The 2D object has a 2D bounding box, that is the smallest
       axis aligned rectangle that fully contains the object.
       This rectangle is always in the world coordinate system.
       The bounding box must be computed in the
       <See Property=TGraphicObject@_UpdateExtension> method that have
       to be redefined.
    }
    property BoundingBox: TRect2D read fBoundingBox;
    {: If this property is <B=True> then the bounding box is drawed
       in the <See Method=TObject2D@DrawControlPoints>.
    }
    property DrawBoundingBox: Boolean read fDrawBoundingBox
      write
      fDrawBoundingBox;
    {: This property contains the current handler object.

       See <See Class=TObject2DHandler@TObject2DHandler> for details.
    }
    //TSY:
    property HasControlPoints: Boolean read fHasControlPoints
      write
      fHasControlPoints;
  end;

  {: This class defines a group of 2D objects, that is a close set
     of objects

     A container has a special behaviour for the picking operation.
     Since a container embrace a group of objects, the picking take
     effect at group level returning the ID of the selected object
     instead of its control point.
  }
  TContainer2D = class(TObject2D)
  private
    { List of objects in the container. }
    fObjects: TGraphicObjList;
  protected
    procedure _UpdateExtension; override;
  public
    {: This is the constructor for the class.

       It creates an instance of the container. This constructor needs:

       <LI=the ID of the container. This identifier univocally identifies
       the object in the drawing. See also <See Method=TDrawing@AddObject>.>
       <LI=the array of objects that must be added to the set. After
       you have created the container it is possible to add and
       remove objects throught the <See Property=TContainer2D@Objects> property.>

       If you want to create a void container you have to supply an
       array with only one item set to <B=nil> (ie <B=[nil]>).

       The objects in the container are owned by the container
       itself and will be freed when the container is deleted. So it
       isn't possible to share objects between containers.
    }
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Objs: array of
      TObject2D);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream);
      override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Transform(const T: TTransf2D); override;
    procedure DeviceDraw(Transf: TTransf2D;
      const Dvc: TDevice; const ClipRect2D: TRect2D);
      override;
    procedure DrawControlPoints(const VT: TTransf2D;
      const ClipRect2D: TRect2D; const Width: Integer);
      override;
    function OnMe(P: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;
    {: This property contains the list that contians the object in the container.

       The objects in the container are owned by the container itself. Use
       this property to manage these objects.
    }
    property Objects: TGraphicObjList read fObjects;
  end;

{: This class defines a group of 2D objects

     It is used to group objects that must be moved or
     transformed as a whole. On the other hand, if you want to reuse
     a set of objects in different place on the drawing use the
     <See Class=TSourceBlock2D> class.}

  TGroup2D = class(TContainer2D)
  end;

{: This type define the general type for source block names.
}
  TSourceBlockName = array[0..12] of Char;

  {: This class defines a group of 2D objects that can be placed
     in different points of the drawing. This is the efficent
     way to compose a drawing from repeated part of it.

     This class defines a template that can be instantiated,
     and any instance share
     the same objects of the template. The instances of this
     template are obtained with <See Class=TBlock2D> class.

     This kind of object must be added to the drawing using the
     <See Method=TDrawing2D@AddSourceBlock> and
     <See Class=TDrawing2D@BlockObjects> methods.
  }
  TSourceBlock2D = class(TContainer2D)
  private
    fNReference: Word; { Number of reference }
    fName: TSourceBlockName;
    fLibraryBlock: Boolean;
  public
    {: This is the constructor of the class.

       A source block is referenced either by use of its ID or by use
       of its name.
       This constructor needs:

       <LI=the ID of the source block. This identifier univocally
       identify the source block in the drawing.>
       <LI=the name of the source block. The name is of type
       <See Type=TSourceBlockName>.>
       <LI=the array of objects that must be added to the source
       block. After you have created the source blocks it is possible
       to add and remove objects afterward throught the
       <See Property=TContainer2D@Objects> property.>

       The objects in the source block are owner by the source block
       itself and they will be freed when the source block is deleted.
       So it isn't possible to share objects between source block, but
       it is possible to share these objects using the <See Class=TBlock2D>
       class.
    }
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Name:
      TSourceBlockName; const Objs: array of TObject2D);
    constructor CreateFromStream(const Stream: TStream);
      override;
    destructor Destroy; override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method updates the references of the source blocks
       that are used in the container.

       This method is called automatically at the end of the loading
       process from a stream. Indeed when you save a container it
       may contains <See Class=TBlock2D> instances that references
       to <See Class=TSourceBlock2D> instances. When the application
       is closed these references are no longer valid, so the blocks
       must be relinked to the corrent source blocks.

       The method needs the source blocks list in order
       to relink the blocks. This list can be obtained from
       TDrawing@SourceBlocksList.??
    }
    procedure UpdateSourceReferences(
      const BlockList: TGraphicObjList);
    {: This property contains the name of the source block.

       See also <See Type=TSourceBlockName>.
    }
    property Name: TSourceBlockName read fName write fName;
    {: This property contains the number of instances of the
       source block, that is the number of <See Class=TBlock2D>
       objects that are linked to it.

       When a new block is created to reference a source block,
       this property is incremented by one. When the block is
       deleted this numeber is decremented by one.

       Only if this value is zero the source block can be deleted.
    }
    property NumOfReferences: Word read fNReference;
    {: This property tells if the source block is a library source
       block.

       A library source block is a source block that is shared among
       differert drawings, made up a library of symbols.

       When this property is <B=True>, the source block will not be
       saved in a drawing file, but can be stored in a library file
       by using the methods <See Method=TDrawing@SaveLibrary>
       and <See Method=TDrawing@LoadLibrary>.

       If this property is <B=False>, the source block will be
       saved in the drawing file.

       If <See Property=TGraphicObject@ToBeSaved> is <B=False> the
       source block will not be saved anyway.
    }
    property IsLibraryBlock: Boolean read fLibraryBlock write
      fLibraryBlock;
  end;

  {: This class defines an instance of a <See Class=TSourceBlock2D>.

     A block is an istance of a source block, namely a copy of the
     source block that share the
     same objects that define the source block.

     A block depends on the source block for its shape.
  }
  TBlock2D = class(TObject2D)
  private
    { Reference to the source block. }
    fSourceBlock: TSourceBlock2D;
    fSourceName: TSourceBlockName;
    { Origin of the block. }
    fOriginPoint: TPoint2D;

    procedure SetSourceBlock(const Source: TSourceBlock2D);
    procedure SetOriginPoint(P: TPoint2D);
  protected
    procedure _UpdateExtension; override;
  public
    {: This is the constructor of the class.

       <I=Source> is the source block that is used to define
       the instance.
    }
    constructor Create(ID: Integer); override;
    constructor CreateSpec(ID: Integer; const Source:
      TSourceBlock2D);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream);
      override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method updates the references of the block.

       This method is called automatically at the end of the loading
       process of the block from a stream. Indeed when you save a
       block the reference to the source block is no longer valid,
       so the block must be relinked to the correct source blocks
       instance.

       The method needs the list of source blocks of a drawing in
       order to relink the block's source block. This list can be
       obtained by TDrawing@SourceBlocksList.
    }
    procedure UpdateReference(const BlockList: TGraphicObjList);
    procedure DrawControlPoints(const VT: TTransf2D;
      const ClipRect2D: TRect2D; const Width: Integer);
      override;
    procedure DeviceDraw(Transf: TTransf2D;
      const Dvc: TDevice; const ClipRect2D: TRect2D);
      override;
    function OnMe(P: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;
    {: This property contains the base position of the block.

       The origin point is the position of the (0, 0) inside of
       the source block that is referenced by the block.

       The origin point is only a visual reference and doesn't
       alter the model transform (but it is altered from it).
    }
    property OriginPoint: TPoint2D read fOriginPoint write
      SetOriginPoint;
    {: This property contains the reference to the source block used by the block.

       See also <See Class=TSourceBlock2D>.
    }
    property SourceBlock: TSourceBlock2D read fSourceBlock
      write
      SetSourceBlock;
    {: This property contains the name of the source block
       referenced by the block.

       It is used to save the source block reference into a stream,
       and it is used to relink the source block reference when the
       block is loaded back.
    }
    property SourceName: TSourceBlockName read fSourceName;
  end;

{: This type defines the string used to name a layer.
}
  TLayerName = string[31];

{: This type defines the layer used by the library.

   Any graphic object is associated with a layer from which they get
   their drawing caracteristic such as color, line style, fill color
   and so on.

   An object also get the behavior of this layer such us visibility
   and enabling to picking that have priority on the ones of the
   object.
}
  TLayer = class(TObject)
  private
//    fPen: TPen;
//    fBrush: TBrush;
    fName: TLayerName;
    fActive: Boolean;
    fVisible: Boolean;
    fOpaque: Boolean;
    fStreamable: Boolean;
    fIdx: Byte;
    fTag: Integer;

    procedure SetName(NM: TLayerName);
  public
{: This is the constructor of the layer.

   <I=Idx> is the index number of the layer. The layers are indexed from
   0 to 255.

   The constructor sets:

   <LI=<See Property=TLayer@Name> to <I=Layer<<Idx>> > >
   <LI=<See Property=TLayer@Pen> to the pen <B=clBlack> and <B=psSolid> >.
   <LI=<See Property=TLayer@Brush> to the brush <B=clWhite> and <B=psSolid> >
   <LI=<See Property=TLayer@Active> to <B=True> >
   <LI=<See Property=TLayer@Visible> to <B=True> >
   <LI=<See Property=TLayer@Opaque> to <B=False> >
   <LI=<See Property=TLayer@Streamable> to <B=True> >
   <LI=<See Property=TLayer@Modified> to <B=False> >
}
    constructor Create(IDX: Byte);
{: This is the destructor of the layer, it destroys the pen and brush
   objects.
}
    destructor Destroy; override;
{: This method saves the layer settings to a stream.

   <I=Stream> is the stream onto which save the settings.
}
    procedure SaveToStream(const Stream: TStream); virtual;
{: This method retrieves the layer settings from a stream.

   <I=Cont> is the index of the layer, <I=Stream> is the stream that
   contains the settings and <I=Version> is the version of the library that
   has saved the layer.
}
    procedure LoadFromStream(const Stream: TStream); virtual;
{: This property contains the name of the layer.
}
    property Name: TLayerName read fName write SetName;
{: This property tells if the objects on the layer are considered for the
   picking operations.

   If you want to change the default brush of the layers use the
   <See Method=TDrawing@SetDefaultBrush>.
}
    property Active: Boolean read fActive write fActive;
{: This property tells if the objects on the layer are drawed or not.
}
    property Visible: Boolean read fVisible write fVisible;
{: This property tells if the objects on the layer are opaque (<B=True>) or
   transparent.

   If an object is transparent then the brush will not cover the background.
}
    property OPAQUE: Boolean read fOpaque write fOpaque;
{: This property is <B=True> if the objects on the layer must be saved into
   a drawing.

   If this property is <B=False> the objects on the layer are not saved. This
   is useful if you want to save some object into another stream (or a container
   object save it as part of its state).
}
    property Streamable: Boolean read fStreamable write
      fStreamable;
{: This property is index of the layer that ranges from 0 to 255.
}
    property LayerIndex: Byte read fIdx;
{: This property is not used by the library and so can be used to store
   user defined values like object references.
}
    property Tag: Integer read fTag write fTag;
  end;

{: This class defines a set of 256 layers (from 0 to 255).
   Every <See Class=TDrawing> control has an instance of this class.

   This class manage all the layers and stream them on a file.
}
  TLayers = class(TObject)
  private
    fLayers: TList;

    function GetLayer(Index: Byte): TLayer;
    function GetLayerByName(const NM: TLayerName): TLayer;
  public
{: This is the constructor of the class. It creates a new istance of the set of layers and initialize all
   the layers to the default settings.
}
    constructor Create;
{: This is the destructor of the class. It destroy the set and all of the
   contained layers.
}
    destructor Destroy; override;
{: This method saves only the modified layers into a drawing stream.
}
    procedure SaveToStream(const Stream: TStream);
{: This method retrieve the layers from a drawing stream.
}
    procedure LoadFromStream(const Stream: TStream);
{: This property contains the set of 256 layers.

   Use this property to change the setting of a layer for a <See Class=TDrawing>.
}
    property Layers[Index: Byte]: TLayer read GetLayer;
    default;
{: This property contains the set of 256 layers that can be accessed through
   their names.

   If <I=Nm> doesn't correspond to any of the layers it returns <B=nil>.
   Use this property to change the setting of a layer for a <See Class=TDrawing>.
}
    property LayerByName[const NM: TLayerName]: TLayer read
    GetLayerByName;
  end;

  { The below class utilities are for graphics object registration. }
  {: This procedure resets the list of graphic object registrations.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
procedure TpXInitClassRegister;
  {: This function search for the index of the registered
     graphic object. If no match is found it returns -1.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
function TpXFindClassIndex(const Name: string): Word;
  {: This function search for the index of the registered
     graphic object by using its class name. If no match is
     found it returns -1.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
function TpXFindClassByName(const Name: string):
  TGraphicObjectClass;
  {: This function returns the class reference of the graphic
     object which the specified registration index.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
function TpXFindClassByIndex(Index: Word):
  TGraphicObjectClass;
  {: This procedure registers a new graphic object class in the
     library PERSISTANCE system.

     This method must be used to register your owns graphical
     object classes so that they can be streamed to a drawing
     file automatically by the library.

     <I=Index> is the registration index you choose.
     If a class is already registered in that slot an exception
     will be raised. It is better to register your classe from
     the index 150 on.

     There are <I=MAX_REGISTERED_CLASSES> slots in the
     registration list.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
procedure TpXRegisterClass(Index: Word; const GraphClass:
  TGraphicObjectClass);
  {: This procedure unregisters a graphic object class in the
     library PERSISTANCE system.

     <I=Index> is the registration index to clear.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
procedure TpXUnregisterClass(Index: Word);
  {: This function converts a pascal string to a <I=source block>
     name type.

     See also <See Type=TSourceBlockName>
  }
function StringToBlockName(const Str: string): TSourceBlockName;

implementation

uses Drawings, GObjects;

type

  TGraphicClassRegistered = array[0..512] of
    TGraphicObjectClass;

var
  GraphicObjectsRegistered: TGraphicClassRegistered;

// =====================================================================
// TGraphicObject
// =====================================================================

function TGraphicObject.GetParentDrawing: TObject;
begin
  if Assigned(fParentDrawing) then Result := fParentDrawing
  else Result := fOwnerDrawing;
end;

constructor TGraphicObject.Create(ID: Integer);
begin
  inherited Create;

  fID := ID;
  fLayer := 0;
  fVisible := True;
  fEnabled := True;
  fToBeSaved := True;
  fTag := 0;
  fOnChange := nil;
  fParentDrawing := nil;
end;

constructor TGraphicObject.CreateFromStream(
  const Stream: TStream);
var
  BitMask: Byte;
begin
  inherited Create;
  with Stream do
  begin
    Read(fID, SizeOf(fID));
    Read(fLayer, SizeOf(fLayer));
    Read(BitMask, SizeOf(BitMask))
  end;
  fVisible := (BitMask and 1) = 1;
  fEnabled := (BitMask and 2) = 2;
  fToBeSaved := not ((BitMask and 4) = 4);
  fTag := 0;
  fOnChange := nil;
end;

constructor TGraphicObject.CreateDupe(Obj: TGraphicObject);
begin
  Create(-1);
  Assign(Obj);
end;

procedure TGraphicObject.SaveToStream(const Stream: TStream);
var
  BitMask: Byte;
begin
  BitMask := 0;
  if fVisible then BitMask := BitMask or 1;
  if fEnabled then BitMask := BitMask or 2;
  if not fToBeSaved then
    BitMask := BitMask or 4;
      // Uso il not per compatibilità con le versioni precedenti.
  with Stream do
  begin
    Write(fID, SizeOf(fID));
    Write(fLayer, SizeOf(fLayer));
    Write(BitMask, SizeOf(BitMask));
  end;
end;

procedure TGraphicObject.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  fVisible := Obj.Visible;
  fEnabled := Obj.Enabled;
  fToBeSaved := Obj.ToBeSaved;
//    fID, fTag: Integer;
//    fLayer: Byte;
//    fToBeSaved: Boolean;
//    fOnChange: TNotifyEvent;
  fOwnerDrawing := Obj.fOwnerDrawing;
  fParentDrawing := Obj.fParentDrawing;
end;

procedure TGraphicObject.UpdateExtension(Sender: TObject);
begin
  _UpdateExtension;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

// =====================================================================
// Registration functions
// =====================================================================

function TpXFindClassIndex(const Name: string): Word;
var
  Cont: Integer;
begin
  for Cont := 0 to MAX_REGISTERED_CLASSES - 1 do
    if Assigned(GraphicObjectsRegistered[Cont]) and
      (GraphicObjectsRegistered[Cont].ClassName = Name) then
    begin
      Result := Cont;
      Exit;
    end;
  raise ETpX_ObjClassNotFound.Create('TpXFindClassIndex: ' +
    Name + ' graphic class not found');
end;

function TpXFindClassByName(const Name: string):
  TGraphicObjectClass;
begin
  Result :=
    GraphicObjectsRegistered[TpXFindClassIndex(Name)];
end;

function TpXFindClassByIndex(Index: Word):
  TGraphicObjectClass;
begin
  if Index >= MAX_REGISTERED_CLASSES then
    raise
      Exception.Create(
      Format('TpXRegisterClass: Out of bound registration index %d',
      [Index]));
  if not Assigned(GraphicObjectsRegistered[Index]) then
    raise
      ETpX_ObjClassNotFound.Create(
      Format('TpXFindClassByIndex: Index not registered %d',
      [Index]));
  Result := GraphicObjectsRegistered[Index];
end;

procedure TpXRegisterClass(Index: Word; const GraphClass:
  TGraphicObjectClass);
begin
  if Index >= MAX_REGISTERED_CLASSES then
    raise
      Exception.Create('TpXRegisterClass: Out of bound registration index');
  if Assigned(GraphicObjectsRegistered[Index]) then
    raise
      ETpX_ObjClassNotFound.Create(Format('TpXRegisterClass: Index %d already allocated by %s', [Index, GraphicObjectsRegistered[Index].ClassName]));
  GraphicObjectsRegistered[Index] := GraphClass;
end;

procedure TpXUnregisterClass(Index: Word);
begin
  if Index >= MAX_REGISTERED_CLASSES then
    raise
      Exception.Create('TpXRegisterClass: Out of bound registration index');
  if Assigned(GraphicObjectsRegistered[Index]) then
    GraphicObjectsRegistered[Index] := nil;
end;

procedure TpXInitClassRegister;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_CLASSES do
    GraphicObjectsRegistered[Cont] := nil;
end;

// =====================================================================
// TObjList
// =====================================================================

constructor TObjList.Create;
begin
  inherited Create;
  fLocked := False;
  fHead := nil;
  fTail := nil;
  fCount := 0;
  fFreeOnDelete := True;
  fCurrent := nil;
end;

destructor TObjList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TObjList._CreateItem(const Prev, Next: POL_Item;
  const Obj: TObject): POL_Item;
begin
  GetMem(Result, SizeOf(TOL_Item));
  Result^.Prev := Prev;
  Result^.Next := Next;
  Result^.Obj := Obj;
  Inc(fCount);
end;

procedure TObjList.Add(const Obj: TObject);
begin
  InsertBefore(nil, Obj);
end;

function TObjList.InsertBefore(
  const Item: POL_Item; const Obj: TObject): POL_Item;
begin
  if Item = nil then
  begin
    Result := _CreateItem(fTail, nil, Obj);
    if fTail <> nil then fTail^.Next := Result
    else fHead := Result;
    fTail := Result;
    Exit;
  end;
  Result := _CreateItem(Item^.Prev, Item, Obj);
  if Item^.Prev <> nil then Item^.Prev^.Next := Result
  else fHead := Result;
  Item^.Prev := Result;
end;

procedure TObjList.InsertBefore_FromList(
  const Item: POL_Item; const Lst: TObjList);
var
  Prev, NewItem, CurrItem: POL_Item;
begin
  CurrItem := Lst.fHead;
  if Item <> nil then Prev := Item^.Prev else Prev := fTail;
  while CurrItem <> nil do
  begin
    NewItem := _CreateItem(Prev, Item, CurrItem^.Obj);
    if Prev <> nil then Prev^.Next := NewItem
    else fHead := NewItem;
    Prev := NewItem;
    CurrItem := CurrItem^.Next;
  end;
  if Item <> nil then Item^.Prev := NewItem else fTail := NewItem;
end;

function TObjList.RemoveItem(const Item: POL_Item): TObject;
begin
  Result := Item^.Obj;
  if Item^.Next <> nil then Item^.Next^.Prev := Item^.Prev
  else fTail := Item^.Prev;
  if Item^.Prev <> nil then Item^.Prev^.Next := Item^.Next
  else fHead := Item^.Next;
  FreeMem(Item, SizeOf(TOL_Item));
  Dec(fCount);
end;

procedure TObjList.DeleteItem(const Item: POL_Item);
var
  Obj: TObject;
begin
  Obj := RemoveItem(Item);
  if Assigned(Obj) and fFreeOnDelete then Obj.Free
end;

procedure TObjList._TrancateFromSelf(Item: POL_Item);
var
  Next: POL_Item;
begin
  while Item <> nil do
  begin
    Next := Item^.Next;
    if Assigned(Item^.Obj) and fFreeOnDelete then Item^.Obj.Free;
    FreeMem(Item, SizeOf(TOL_Item));
    Dec(fCount);
    Item := Next;
  end;
end;

procedure TObjList.TrancateAfter(const Item: POL_Item);
begin
  _TrancateFromSelf(Item^.Next);
  fTail := Item;
  Item^.Next := nil;
end;

procedure TObjList.TrancateAt(const Item: POL_Item);
var
  Prev: POL_Item;
begin
  Prev := Item^.Prev;
  _TrancateFromSelf(Item);
  if Prev = nil then fHead := nil else Prev^.Next := nil;
  fTail := Prev;
end;

procedure TObjList.Clear;
begin
  _TrancateFromSelf(fHead);
  fHead := nil;
  fTail := nil;
end;

procedure TObjList.Lock;
begin
  if fLocked then raise ETpX_ListLocked.Create(
      'TObjList.Lock: The list is alredy locked.');
  fLocked := True;
end;

procedure TObjList.Unlock;
begin
  if not fLocked then raise ETpX_ListLocked.Create(
      'TObjList.Unlock: The list is not locked. Can not unlock.');
  fLocked := False;
end;

function TObjList.Peek: TObject;
begin
  if fTail = nil then Result := nil
  else Result := fTail^.Obj;
end;

function TObjList.Pop: TObject;
var
  Prev: POL_Item;
begin
  if fTail = nil then Result := nil
  else
  begin
    Result := fTail^.Obj;
    Prev := fTail^.Prev;
    FreeMem(fTail, SizeOf(TOL_Item));
    Dec(fCount);
    if Prev = nil then fHead := nil else Prev^.Next := nil;
    fTail := Prev;
  end;
end;

function TObjList.FirstObj: TObject;
begin
  fCurrent := fHead;
  Result := CurrentObj;
end;

function TObjList.CurrentObj: TObject;
begin
  if fCurrent = nil then Result := nil
  else Result := fCurrent^.Obj;
end;

function TObjList.NextObj: TObject;
begin
  if fCurrent = nil then Result := nil
  else
  begin
    fCurrent := fCurrent^.Next;
    Result := CurrentObj;
  end;
end;

function TObjList.LastObj: TObject;
begin
  fCurrent := fTail;
  Result := CurrentObj;
end;

function TObjList.PrevObj: TObject;
begin
  if fCurrent = nil then Result := nil
  else
  begin
    fCurrent := fCurrent^.Prev;
    Result := CurrentObj;
  end;
end;

// =====================================================================
// TGraphicObjList
// =====================================================================

constructor TGraphicObjList.Create;
begin
  inherited Create;
end;

destructor TGraphicObjList.Destroy;
begin
  inherited Destroy;
end;

function TGraphicObjList.RemoveItem(const Item: POL_Item): TObject;
begin
  Result := inherited RemoveItem(Item);
end;

procedure TGraphicObjList.Add(const Obj: TGraphicObject);
begin
  inherited Add(Obj);
end;

function TGraphicObjList.Peek: TGraphicObject;
begin
  Result := inherited Peek as TGraphicObject;
end;

function TGraphicObjList.Pop: TGraphicObject;
begin
  Result := inherited Pop as TGraphicObject;
end;

function TGraphicObjList.FirstObj: TGraphicObject;
begin
  Result := inherited FirstObj as TGraphicObject;
end;

function TGraphicObjList.CurrentObj: TGraphicObject;
begin
  Result := inherited CurrentObj as TGraphicObject;
end;

function TGraphicObjList.NextObj: TGraphicObject;
begin
  Result := inherited NextObj as TGraphicObject;
end;

function TGraphicObjList.LastObj: TGraphicObject;
begin
  Result := inherited LastObj as TGraphicObject;
end;

function TGraphicObjList.PrevObj: TGraphicObject;
begin
  Result := inherited PrevObj as TGraphicObject;
end;

function TGraphicObjList.FindObjByID(
  const ID: Integer): TGraphicObject;
begin
  Result := FirstObj;
  while (Result <> nil) and (Result.fID <> ID) do
    Result := NextObj;
end;

function TGraphicObjList.DeleteCurrent: TGraphicObject;
var
  Item: POL_Item;
begin
  Result := nil;
  if fCurrent = nil then Exit;
  Item := fCurrent;
  fCurrent := fCurrent^.Next;
  DeleteItem(Item);
end;

function TGraphicObjList.RemoveCurrent: TGraphicObject;
var
  Item: POL_Item;
begin
  Result := nil;
  if fCurrent = nil then Exit;
  Item := fCurrent;
  fCurrent := fCurrent^.Next;
  RemoveItem(Item);
end;

procedure TGraphicObjList.Transform(const T: TTransf2D);
var
  Obj: TGraphicObject;
begin
  Lock;
  try
    Obj := FirstObj;
    while Obj <> nil do
    begin
      if Obj is TObject2D then (Obj as TObject2D).Transform(T);
      Obj := NextObj;
    end;
  finally
    Unlock;
  end;
end;

function TGraphicObjList.PickObject(const P: TPoint2D;
  Layers: TLayers; const Aperture: TRealType;
  const VisualRect: TRect2D; const PickFilter: TObject2DClass;
  const FirstFound: Boolean; var NPoint: Integer): TObject2D;
var
  TmpNPoint: Integer;
  Obj: TGraphicObject;
  MinDist, Distance: TRealType;
  //TSY:
  function NPointLevel(const NPoint: Integer): TRealType;
  begin
    Result := NPoint;
    if NPoint > -2 then Result := -2;
  end;
begin
  Result := nil;
  if Layers = nil then Exit;
  Lock;
  try
    MinDist := Aperture;
    NPoint := PICK_NOOBJECT;
    Obj := FirstObj;
    while Obj <> nil do
    begin
      if Obj is TObject2D then with (Layers[Obj.Layer]) do
          if Active and Visible and (Obj is PickFilter) and
            (Obj as TObject2D).IsVisible(VisualRect) then
          begin
            TmpNPoint := (Obj as TObject2D).OnMe(P, Aperture,
              Distance);
          //TSY:
          //if TmpNPoint = -2 then Distance := 0;
            if (NPointLevel(TmpNPoint) >= NPointLevel(NPoint))
              and (Distance <= MinDist) then
            begin
              Result := Obj as TObject2D;
              NPoint := TmpNPoint;
              MinDist := Distance;
              if FirstFound then
                Break;
            end;
          end;
      Obj := NextObj;
    end;
  finally
    Unlock;
  end;
end;

procedure TGraphicObjList.AddFromList(const Lst:
  TGraphicObjList);
var
  Obj: TGraphicObject;
  I: Integer;
begin
  if Lst.Count = 0 then Exit;
  Lst.Lock;
  Lock;
  try
    I := 0;
    Obj := Lst.FirstObj;
    while Obj <> nil do
    begin
      Add(Obj);
      Inc(I);
      if I mod 100 = 0 then ShowProgress(I / Lst.Count);
      Obj := Lst.NextObj;
    end;
  finally
    Lst.Unlock;
    Unlock;
  end;
end;

procedure TGraphicObjList.Insert(
  const IDInsertPoint: Integer; const Obj: TGraphicObject);
var
  NewItem: POL_Item;
  InsertPoint: POL_Item;
  TmpObj: TGraphicObject;
begin
  if fHead = nil then raise ETpX_SysException.Create(
      'TGraphicObjList.Insert: No objects in the list');
  Lock;
  try
    FindObjByID(IDInsertPoint);
    if fCurrent = nil then raise ETpX_ListObjNotFound.Create(
        'TGraphicObjList.Move: Insertion point not found');
    InsertPoint := fCurrent;
    // Allocate new item.
    GetMem(NewItem, SizeOf(TOL_Item));
    // Initialize the item.
    NewItem^.Prev := InsertPoint^.Prev;
    NewItem^.Next := InsertPoint;
    NewItem^.Obj := Obj;
    if InsertPoint^.Prev <> nil then
      InsertPoint^.Prev^.Next := NewItem
    else
      fHead := NewItem;
    InsertPoint^.Prev := NewItem;
    Inc(fCount);
  finally
    Unlock;
  end;
end;

procedure TGraphicObjList.InsertFromList(
  const IDInsertPoint: Integer; const Lst: TGraphicObjList);
var
  NewItem: POL_Item;
  InsertPoint: POL_Item;
begin
  if fHead = nil then raise ETpX_SysException.Create(
      'TGraphicObjList.Insert: No objects in the list');
  Lock;
  try
    FindObjByID(IDInsertPoint);
    if (fCurrent = nil) then raise ETpX_ListObjNotFound.Create(
        'TGraphicObjList.Move: Insertion point not found');
    InsertPoint := fCurrent;
    Lst.Lock;
    try
      Lst.FirstObj;
      while Lst.CurrentObj <> nil do
      begin
        // Allocate new item.
        GetMem(NewItem, SizeOf(TOL_Item));
        // Initialize the item. }
        NewItem^.Prev := InsertPoint^.Prev;
        NewItem^.Next := InsertPoint;
        NewItem^.Obj := Lst.CurrentObj;
        if InsertPoint^.Prev <> nil then
          InsertPoint^.Prev^.Next := NewItem
        else
          fHead := NewItem;
        InsertPoint^.Prev := NewItem;
        Inc(fCount);
        Lst.NextObj;
      end;
    finally
      Lst.Unlock;
    end;
  finally
    Unlock;
  end;
end;

procedure TGraphicObjList.Move(
  const IDToMove, IDInsertPoint: Integer);
var
  InsertPoint, ToMove: POL_Item;
begin
  if fHead = nil then raise ETpX_SysException.Create(
      'TGraphicObjList.Move: No objects in the list');
  // Check if the current and insert point are the same.
  if IDInsertPoint = IDToMove then raise ETpX_SysException.Create(
      'TGraphicObjList.Move: Bad object to move');
  Lock;
  try
    FindObjByID(IDInsertPoint);
    if fCurrent = nil then raise ETpX_ListObjNotFound.Create(
        'TGraphicObjList.Move: Insertion point not found');
    InsertPoint := fCurrent;
    FindObjByID(IDToMove);
    if fCurrent = nil then raise ETpX_ListObjNotFound.Create(
        'TGraphicObjList.Move: Object to move not found');
    ToMove := fCurrent;
    // Move object
    if ToMove^.Prev <> nil then
      ToMove^.Prev^.Next := ToMove^.Next
    else
      fHead := ToMove^.Next;
    if ToMove^.Next <> nil then
      ToMove^.Next^.Prev := ToMove^.Prev
    else
      fTail := ToMove^.Prev;
    // Set new link
    if InsertPoint^.Prev <> nil then
      InsertPoint^.Prev^.Next := ToMove
    else
      fHead := ToMove;
    ToMove^.Next := InsertPoint;
    ToMove^.Prev := InsertPoint^.Prev;
    InsertPoint^.Prev := ToMove;
  finally
    Unlock;
  end;
end;

function TGraphicObjList.Delete(const ID: Integer): Boolean;
begin
  Result := False;
  Lock;
  try
    FindObjByID(ID);
    if fCurrent = nil then raise ETpX_ListObjNotFound.Create(
        'TGraphicObjList.Delete: Object not found');
    DeleteItem(fCurrent);
    Result := True;
  finally
    Unlock;
  end;
end;

function TGraphicObjList.Find(const ID: Integer):
  TGraphicObject;
begin
  Lock;
  try
    Result := FindObjByID(ID);
  finally
    Unlock;
  end;
end;

procedure TGraphicObjList.ReplaceDeleteCurrent(
  const Obj: TGraphicObject);
var
  OwnerDrawing: TObject;
begin
  if fCurrent = nil then Exit;
  OwnerDrawing := nil;
  if Assigned(fCurrent^.Obj) and fFreeOnDelete then
  begin
    OwnerDrawing := (fCurrent^.Obj as
      TGraphicObject).OwnerDrawing;
    fCurrent^.Obj.Free;
  end;
  Obj.fOwnerDrawing := OwnerDrawing;
  fCurrent^.Obj := Obj;
end;

procedure TGraphicObjList.Clear;
begin
  Lock;
  try
    inherited Clear;
  finally
    Unlock;
  end;
end;

// =====================================================================
// TLayer
// =====================================================================

procedure TLayer.SetName(NM: TLayerName);
begin
  if fName <> NM then
  begin
    fName := NM;
  end;
end;

constructor TLayer.Create(IDX: Byte);
begin
  inherited Create;
  fIdx := IDX;
  fName := Format('Layer %d', [IDX]);
//  fPen := TPen.Create;
//  fPen.Color := clBlack;
//  fPen.Style := psSolid;
//  fBrush := TBrush.Create;
  //TSY:
//  fBrush.Color := clSilver;
  //fBrush.Color := clGray;
//  fBrush.Style := bsSolid;
  fActive := True;
  fVisible := True;
  fOpaque := False;
  fStreamable := True;
//  fPen.OnChange := Changed;
//  fBrush.OnChange := Changed;
  fTag := 0;
end;

destructor TLayer.Destroy;
begin
//  fPen.Free;
//  fBrush.Free;
  inherited Destroy;
end;

procedure TLayer.SaveToStream(const Stream: TStream);
//var
//  TmpColor: TColor;
//  TmpPenStyle: TPenStyle;
//  TmpPenMode: TPenMode;
//  TmpWidth: Integer;
//  TmpBrushStyle: TBrushStyle;
begin
//  TmpColor := fPen.Color;
//  Stream.Write(TmpColor, SizeOf(TmpColor));
//  TmpPenStyle := fPen.Style;
//  Stream.Write(TmpPenStyle, SizeOf(TmpPenStyle));
//  TmpPenMode := fPen.Mode;
//  Stream.Write(TmpPenMode, SizeOf(TmpPenMode));
//  TmpWidth := fPen.Width;
//  Stream.Write(TmpWidth, SizeOf(TmpWidth));

//  TmpColor := fBrush.Color;
//  Stream.Write(TmpColor, SizeOf(TmpColor));
//  TmpBrushStyle := fBrush.Style;
//  Stream.Write(TmpBrushStyle, SizeOf(TmpBrushStyle));

  Stream.Write(fActive, SizeOf(Boolean));
  Stream.Write(fVisible, SizeOf(Boolean));
  Stream.Write(fOpaque, SizeOf(Boolean));
  Stream.Write(fStreamable, SizeOf(Boolean));
  Stream.Write(fName, SizeOf(TLayerName));
end;

procedure TLayer.LoadFromStream(const Stream: TStream);
var
//  TmpColor: TColor;
//  TmpPenStyle: TPenStyle;
//  TmpPenMode: TPenMode;
//  TmpWidth: Integer;
//  TmpBrushStyle: TBrushStyle;
  TmpBool: Boolean;
begin
//  Stream.Read(TmpColor, SizeOf(TmpColor));
//  fPen.Color := TmpColor;
//  Stream.Read(TmpPenStyle, SizeOf(TmpPenStyle));
//  fPen.Style := TmpPenStyle;
//  Stream.Read(TmpPenMode, SizeOf(TmpPenMode));
//  fPen.Mode := TmpPenMode;
//  Stream.Read(TmpWidth, SizeOf(TmpWidth));
//  fPen.Width := TmpWidth;

//  Stream.Read(TmpColor, SizeOf(TmpColor));
//  fBrush.Color := TmpColor;
//  Stream.Read(TmpBrushStyle, SizeOf(TmpBrushStyle));
//  fBrush.Style := TmpBrushStyle;
  Stream.Read(fActive, SizeOf(Boolean));
  Stream.Read(fVisible, SizeOf(Boolean));
  Stream.Read(fOpaque, SizeOf(Boolean));
  Stream.Read(fStreamable, SizeOf(Boolean));
  Stream.Read(fName, SizeOf(TLayerName));
end;

function TLayers.GetLayerByName(const NM: TLayerName): TLayer;
var
  Cont: Integer;
begin
  Result := nil;
  for Cont := 0 to 255 do
    if NM = TLayer(fLayers.Items[Cont]).fName then
      Result := TLayer(fLayers.Items[Cont]);
end;

function TLayers.GetLayer(Index: Byte): TLayer;
begin
  Result := TLayer(fLayers.Items[Index]);
end;

constructor TLayers.Create;
var
  Cont: Byte;
  TmpLay: TLayer;
begin
  inherited Create;
  fLayers := TList.Create;
  for Cont := 0 to 255 do
  begin
    TmpLay := TLayer.Create(Cont);
    TmpLay.fName := Format('Layer %d', [Cont]);
    fLayers.Add(TmpLay);
  end;
end;

destructor TLayers.Destroy;
begin
  while fLayers.Count > 0 do
  begin
    TLayer(fLayers.Items[0]).Free;
    fLayers.Delete(0);
  end;
  fLayers.Free;
  inherited Destroy;
end;

procedure TLayers.SaveToStream(const Stream: TStream);
var
  Cont: Word;
begin
  Cont := 1;
  Stream.Write(Cont, SizeOf(Cont));
  for Cont := 0 to 255 do
    with Stream do
//      if TLayer(fLayers[Cont]).fModified then
    begin
      Write(Cont, SizeOf(Cont));
      TLayer(fLayers[Cont]).SaveToStream(Stream);
    end;
  Cont := 256;
  Stream.Write(Cont, SizeOf(Cont));
end;

procedure TLayers.LoadFromStream(const Stream: TStream);
var
  Cont: Word;
begin
  Stream.Read(Cont, SizeOf(Cont));
  if Cont <> 1 then
    raise
      ETpX_NoLayers.Create('TLayers.LoadFromStream: No layers found');
  while Stream.Position < Stream.Size do
    with Stream do
    begin
      Read(Cont, SizeOf(Cont));
      if Cont = 256 then Break;
      TLayer(fLayers[Cont]).LoadFromStream(Stream);
    end;
end;

// =====================================================================
// TObject2D
// =====================================================================

constructor TObject2D.Create(ID: Integer);
begin
  inherited Create(ID);
  fBoundingBox := Rect2D(0, 0, 0, 0);
  fDrawBoundingBox := False;
end;

destructor TObject2D.Destroy;
begin
  inherited Destroy;
end;

constructor TObject2D.CreateFromStream(const Stream: TStream);
var
  TmpTransf: TTransf2D;
begin
  inherited;
end;

procedure TObject2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
end;

procedure TObject2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited Assign(Obj);
  if Obj is TObject2D then
  begin
    fBoundingBox := TObject2D(Obj).fBoundingBox;
  end;
end;

procedure TObject2D.MoveTo(ToPt, DragPt: TPoint2D);
var
  TmpTransf: TTransf2D;
begin
  ToPt := CartesianPoint2D(ToPt);
  DragPt := CartesianPoint2D(DragPt);
  TmpTransf := Translate2D(ToPt.X - DragPt.X, ToPt.Y -
    DragPt.Y);
  Transform(TmpTransf);
end;

function TObject2D.OnMe(P: TPoint2D; Aperture: TRealType; var
  Distance: TRealType): Integer;
var
  TmpBox: TRect2D;
begin
  TmpBox := EnlargeBoxDelta2D(BoundingBox, Aperture);
  Distance := MaxRealType;
  Result := PICK_NOOBJECT;
  if not fEnabled then
    Exit;
  if IsPointInCartesianBox2D(P, TmpBox) then
  begin
    Distance := Aperture;
    Result := PICK_INBBOX;
  end;
end;

procedure TObject2D.DrawControlPoints(const VT: TTransf2D;
  const ClipRect2D: TRect2D; const Width: Integer);
begin
  if fDrawBoundingBox {and (Cnv.Pen.Mode <> pmXOr)}
    then (fOwnerDrawing as TDrawing2D).
    OnControlBox(BoundingBox, VT, ClipRect2D);
end;

procedure TObject2D.Transform(const T: TTransf2D);
begin
  //!!!
end;

function TObject2D.IsVisible(const Clip: TRect2D): Boolean;
begin
  Result := False;
  if not Visible then
    Exit;
  if fBoundingBox.Left > Clip.Right then
    Exit
  else if fBoundingBox.Right < Clip.Left then
    Exit;
  if fBoundingBox.Bottom > Clip.Top then
    Exit
  else if fBoundingBox.Top < Clip.Bottom then
    Exit;
  Result := True;
end;

// =====================================================================
// TContainer2D
// =====================================================================

function StringToBlockName(const Str: string): TSourceBlockName;
var
  Cont: Integer;
begin
  for Cont := 0 to 12 do
  begin
    if Cont < Length(Str) then
      Result[Cont] := Str[Cont + 1]
    else
      Result[Cont] := #0;
  end;
end;

constructor TContainer2D.Create(ID: Integer);
begin
  inherited Create(ID);

  fObjects := TGraphicObjList.Create;
  fObjects.FreeOnDelete := True;
  fDrawBoundingBox := True;
end;

constructor TContainer2D.CreateSpec(ID: Integer; const Objs: array
  of TObject2D);
var
  Cont: Word;
begin
  inherited Create(ID);

  fObjects := TGraphicObjList.Create;
  fObjects.FreeOnDelete := True;
  for Cont := Low(Objs) to High(Objs) do
    if Objs[Cont] <> nil then fObjects.Add(Objs[Cont]);
  UpdateExtension(Self);
  fDrawBoundingBox := True;
end;

procedure TContainer2D._UpdateExtension;
begin
  if fObjects.Count = 0 then
  begin
    fBoundingBox := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  fObjects.FirstObj._UpdateExtension;
  fBoundingBox := (fObjects.CurrentObj as TObject2D).BoundingBox;
  while fObjects.NextObj <> nil do
  begin
    fObjects.CurrentObj._UpdateExtension;
    fBoundingBox := BoxOutBox2D(fBoundingBox,
      (fObjects.CurrentObj as TObject2D).BoundingBox);
  end;
end;

destructor TContainer2D.Destroy;
begin
  fObjects.Free;
  inherited Destroy;
end;

procedure TContainer2D.SaveToStream(const Stream: TStream);
var
  Obj: TObject2D;
  N: Integer;
  Index: Word;
begin
  inherited SaveToStream(Stream);
// Write number of objects in the container
  N := fObjects.Count;
  Stream.Write(N, SizeOf(N));
// Write all contained objects
  Obj := fObjects.FirstObj as TObject2D;
  while Obj <> nil do
  begin
  // Write class index
    Index := TpXFindClassIndex(Obj.ClassName);
    Stream.Write(Index, SizeOf(Index));
  // Write object itself
    Obj.SaveToStream(Stream);
    Obj := fObjects.NextObj as TObject2D;
  end;
end;

constructor TContainer2D.CreateFromStream(const Stream: TStream);
var
  Obj: TGraphicObject;
  N: Integer;
  Index: Word;
begin
  inherited;
// Read number of objects
  Stream.Read(N, SizeOf(N));
  fObjects := TGraphicObjList.Create;
  fObjects.FreeOnDelete := True;
// Read all objects
  while N > 0 do
  begin
  // Read object type index
    Stream.Read(Index, SizeOf(Index));
  // Create object of the corresponding class
    Obj := TpXFindClassByIndex(Index).CreateFromStream(Stream);
//  Obj.UpdateExtension(Self);
    fObjects.Add(Obj);
    Dec(N);
  end;
  UpdateExtension(Self);
end;

procedure TContainer2D.Assign(const Obj: TGraphicObject);
var
  TmpObj, NewObj: TGraphicObject;
begin
  if (Obj = Self) then Exit;
  inherited Assign(Obj);
  if Obj is TContainer2D then
  begin
    // Copy contained objects
    fObjects.Clear;
    TmpObj := (Obj as TContainer2D).fObjects.FirstObj;
    while TmpObj <> nil do
    begin
      NewObj := TGraphicObjectClass(TmpObj.ClassType).Create(-1);
      NewObj.Assign(TmpObj);
      fObjects.Add(NewObj);
      TmpObj := (Obj as TContainer2D).fObjects.NextObj;
    end;
  end;
  UpdateExtension(Self);
end;

procedure TContainer2D.Transform(const T: TTransf2D);
begin
  fObjects.Transform(T);
  UpdateExtension(Self);
end;

procedure TContainer2D.DeviceDraw(Transf: TTransf2D;
  const Dvc: TDevice; const ClipRect2D: TRect2D);
var
  Obj: TGraphicObject;
begin
  Obj := fObjects.FirstObj;
  while Obj <> nil do
  begin
    if Obj is TObject2D then
      (Obj as TObject2D).DeviceDraw(Transf, Dvc, ClipRect2D);
    Obj := fObjects.NextObj;
  end;
end;

function TContainer2D.OnMe(P: TPoint2D; Aperture: TRealType;
  var Distance: TRealType): Integer;
var
  Obj: TObject2D;
  MinDist: TRealType;
begin
  Result := inherited OnMe(P, Aperture, Distance);
  if Result = PICK_INBBOX then
  begin
       { Check all the objects in the container. }
    Obj := fObjects.FirstObj as TObject2D;
    MinDist := 2.0 * Aperture;
    while Obj <> nil do
    begin
      if (Obj.OnMe(P, Aperture, Distance) >= PICK_INOBJECT)
        and
        (Distance < MinDist) then
      begin
        MinDist := Distance;
        Result := Obj.ID;
      end;
      Obj := fObjects.NextObj as TObject2D;
    end;
    Distance := MinDist;
  end;
end;

procedure TContainer2D.DrawControlPoints(const VT: TTransf2D;
  const ClipRect2D: TRect2D; const Width: Integer);
begin
  if fObjects.Count > 0 then
    inherited DrawControlPoints(VT, ClipRect2D, Width);
  (fOwnerDrawing as TDrawing2D).OnControlPoint3(
    BoundingBox.FirstEdge, VT, ClipRect2D);
  (fOwnerDrawing as TDrawing2D).OnControlPoint3(
    BoundingBox.SecondEdge, VT, ClipRect2D);
  (fOwnerDrawing as TDrawing2D).OnControlPoint3(
    Point2D(BoundingBox.Left, BoundingBox.Top), VT, ClipRect2D);
  (fOwnerDrawing as TDrawing2D).OnControlPoint3(
    Point2D(BoundingBox.Right, BoundingBox.Bottom), VT,
    ClipRect2D);
end;

// =====================================================================
// TSourceBlock2D
// =====================================================================

constructor TSourceBlock2D.Create(ID: Integer);
begin
  inherited Create(ID);

  fName := '';
  fLibraryBlock := False;
  fNReference := 0;
end;

constructor TSourceBlock2D.CreateSpec(ID: Integer; const Name:
  TSourceBlockName; const Objs: array of TObject2D);
begin
  inherited CreateSpec(ID, Objs);

  fName := Name;
  fLibraryBlock := False;
  fNReference := 0;
end;

destructor TSourceBlock2D.Destroy;
begin
  if fNReference > 0 then
    raise
      ETpX_SourceBlockIsReferenced.Create('TSourceBlock2D.Destroy: This source block is referenced and cannot be deleted');
  inherited Destroy;
end;

procedure TSourceBlock2D.UpdateSourceReferences(
  const BlockList: TGraphicObjList);
var
  Obj: TGraphicObject;
begin
  Obj := BlockList.FirstObj;
  while Obj <> nil do
  begin
    if Obj is TSourceBlock2D then
      (Obj as TSourceBlock2D).UpdateSourceReferences(BlockList)
    else if (Obj is TBlock2D) then
      (Obj as TBlock2D).UpdateReference(BlockList);
    Obj := BlockList.NextObj;
  end;
  UpdateExtension(Self);
end;

constructor TSourceBlock2D.CreateFromStream(
  const Stream: TStream);
begin
  inherited;
  Stream.Read(fToBeSaved, SizeOf(fToBeSaved));
  Stream.Read(fLibraryBlock, SizeOf(fLibraryBlock));
  Stream.Read(fName, SizeOf(fName));
  fNReference := 0;
end;

procedure TSourceBlock2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(fToBeSaved, SizeOf(fToBeSaved));
  Stream.Write(fLibraryBlock, SizeOf(fLibraryBlock));
  Stream.Write(fName, SizeOf(fName));
end;

procedure TSourceBlock2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited;
  if Obj is TSourceBlock2D then
    fToBeSaved := TSourceBlock2D(Obj).fToBeSaved;
end;

// =====================================================================
// TBlock2D
// =====================================================================

procedure TBlock2D.SetSourceBlock(const Source: TSourceBlock2D);
begin
  if not Assigned(Source) then
    raise
      Exception.Create('TBlock2D.SetSourceBlock: Invalid parameter');
  if (fSourceBlock <> Source) then
  begin
    if Assigned(fSourceBlock) and (fSourceBlock.fNReference > 0)
      then
      Dec(fSourceBlock.fNReference);
    fSourceBlock := Source;
    fSourceName := Source.Name;
    Inc(fSourceBlock.fNReference);
    UpdateExtension(Self);
  end;
end;

procedure TBlock2D.SetOriginPoint(P: TPoint2D);
begin
  fOriginPoint := P;
  UpdateExtension(Self);
end;

constructor TBlock2D.Create(ID: Integer);
begin
  inherited Create(ID);

  fOriginPoint := Point2D(0, 0);
  fSourceName := '';
  fSourceBlock := nil;
  fSourceName := '';
end;

constructor TBlock2D.CreateSpec(ID: Integer; const Source:
  TSourceBlock2D);
begin
  inherited Create(ID);

  if not Assigned(Source) then
    raise
      Exception.Create('TBlock2D.Create: Invalid parameter');
  fOriginPoint := Point2D(0, 0);
  fSourceName := '';
  fSourceBlock := Source;
  fSourceName := Source.Name;
  fBoundingBox := Source.BoundingBox;
  Inc(fSourceBlock.fNReference);
  UpdateExtension(Self);
end;

destructor TBlock2D.Destroy;
begin
  if Assigned(fSourceBlock) and (fSourceBlock.fNReference > 0)
    then
    Dec(fSourceBlock.fNReference);
  inherited Destroy;
end;

constructor TBlock2D.CreateFromStream(const Stream: TStream);
begin
  { Load the standard properties }
  inherited;
  with Stream do
  begin
     { TDrawing will use the value of FSourceName to find out the reference of the source block. }
    Read(fSourceName, SizeOf(fSourceName));
    Read(fOriginPoint, SizeOf(fOriginPoint));
  end;
  fSourceBlock := nil;
end;

procedure TBlock2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
  begin
     { Save the ID of the source block. }
    Write(fSourceName, SizeOf(fSourceName));
    Write(fOriginPoint, SizeOf(fOriginPoint));
  end;
end;

procedure TBlock2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
    Exit;
  inherited;
  if Obj is TBlock2D then
  begin
    if Assigned(fSourceBlock) and (fSourceBlock.fNReference > 0)
      then
      Dec(fSourceBlock.fNReference);
    fSourceBlock := TBlock2D(Obj).fSourceBlock;
    fSourceName := fSourceBlock.Name;
    fOriginPoint := TBlock2D(Obj).OriginPoint;
    Inc(fSourceBlock.fNReference);
    UpdateExtension(Self);
  end;
end;

procedure TBlock2D.UpdateReference(const BlockList:
  TGraphicObjList);
var
  TmpSource: TSourceBlock2D;
begin
  TmpSource := BlockList.FirstObj as TSourceBlock2D;
  while TmpSource <> nil do
  begin
    if TmpSource.Name = fSourceName then
    begin
      fSourceBlock := TmpSource;
      UpdateExtension(Self);
      Exit;
    end;
    TmpSource := BlockList.NextObj as TSourceBlock2D;
  end;
  raise
    ETpX_ListObjNotFound.Create('TBlock2D.UpdateReference: Source block not found');
end;

procedure TBlock2D._UpdateExtension;
begin
  if not Assigned(fSourceBlock) then Exit;
  fSourceBlock.UpdateExtension(Self);
  fBoundingBox := fSourceBlock.BoundingBox;
end;

procedure TBlock2D.DrawControlPoints(const VT: TTransf2D;
  const ClipRect2D: TRect2D; const Width: Integer);
begin
  inherited;
  (fOwnerDrawing as TDrawing2D).OnControlPoint(
    fOriginPoint, VT, ClipRect2D);
end;

procedure TBlock2D.DeviceDraw(Transf: TTransf2D;
  const Dvc: TDevice; const ClipRect2D: TRect2D);
begin
  if not Assigned(fSourceBlock) then Exit;
  fSourceBlock.DeviceDraw(Transf, Dvc, ClipRect2D);
end;

function TBlock2D.OnMe(P: TPoint2D; Aperture: TRealType; var
  Distance: TRealType): Integer;
var
  TmpPt: TPoint2D;
begin
  Result := PICK_NOOBJECT;
  if not Assigned(fSourceBlock) then
    Exit;
  Result := inherited OnMe(P, Aperture, Distance);
  if (Result = PICK_INBBOX) and NearPoint2D(P,
    fOriginPoint, Aperture, Distance) then
   { the origin of the block was picked. }
    Result := PICK_ONOBJECT
  else
  begin
     { Make Pt in Object coordinates }
    TmpPt := P;
    Result := fSourceBlock.OnMe(TmpPt, Aperture, Distance);
  end;
end;

initialization
  TpXInitClassRegister;

  TpXRegisterClass(0, TGroup2D);
  TpXRegisterClass(1, TSourceBlock2D);
  TpXRegisterClass(2, TBlock2D);

  TpXRegisterClass(3, TLine2D);
  TpXRegisterClass(4, TPolyline2D);
  TpXRegisterClass(5, TPolygon2D);
  TpXRegisterClass(6, TRectangle2D);
  TpXRegisterClass(7, TArc2D);
  TpXRegisterClass(8, TEllipse2D);
  TpXRegisterClass(9, TText2D);
  TpXRegisterClass(10, TBitmap2D);
  TpXRegisterClass(13, TCircle2D);
  TpXRegisterClass(14, TStar2D);
  TpXRegisterClass(15, TSector2D);
  TpXRegisterClass(16, TSegment2D);
  TpXRegisterClass(20, TSmoothPath2D);
  TpXRegisterClass(21, TClosedSmoothPath2D);
  TpXRegisterClass(22, TBezierPath2D);
  TpXRegisterClass(23, TClosedBezierPath2D);
  TpXRegisterClass(24, TSymbol2D);
  TpXRegisterClass(25, TBitmap2D);
  TpXRegisterClass(26, TCompound2D);

finalization
end.

