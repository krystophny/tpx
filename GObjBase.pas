unit GObjBase;

// A module for basic graphical objects, lists of objects and layers

interface

uses Classes, SysUtils,
{$IFDEF VER140}
  WinBasic,
{$ELSE}
  LCLIntf, LMessages, LCLType, LazBasic,
{$ENDIF}
  Geometry, Devices, SysBasic
  ;

const
  {:
  }
  MAX_REGISTERED_CLASSES = 512;

type

  TObject2D = class;
  TObject2DClass = class of TObject2D;
  TGraphicObjList = class;
  TGraphicObject = class;
  TGraphicObjectClass = class of TGraphicObject;
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
{: This exception is raised when you ask a
   <See Class=TExclusiveGraphicObjIterator> for a list that has already
   active iterators on it.
   Remove the other iterators before retry.

   <B=Note>: When a list has an active iterators that cannot
    be deleted (because you have lost the reference to it), you
    have to call <See Method=TGraphicObjList@RemoveAllIterators> to remove
    all the pending iterators (but not the memory used by them).
    However use this function with care expecially in a multi thread
    application.
}
  ETpX_ListBlocked = class(Exception);
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


  { Lists of graphical objects. }
{: This class defines an iterator for a list of graphical objects
   (<See class=TGraphicObjList>).

   The library can use a <I=thread> to paint the drawings and so
   a way is needed to ensure that a list is not changed when it is
   in use by another thread.

   To do so the concept of <I=iterator> is used. An iterator is
   an object that is used to traverse a list of objects. When you
   ask and obtain an iterator on a list, you can access the elements
   in the list through the iterator. You cannot modify a list if you
   doesn't have an <I=exclusive iterator> that let you to modify the
   list as well as iterate it (<See Class=TExclusiveGraphicObjIterator>).

   As soon as you obtain an iterator the list cannot be modified and
   so you are ensured that it will remain the same during the iteration.
   Of course if you doen't release an iterator you cannot modify the list
   until the end of the program, so attention is needed when using an
   iterator.

   For backward compatibility some operations on the list are avaiable
   with the list itself (and these are however implemented with temporary
   iterators so using them is the better approach).

   See also <See Class=TExclusiveGraphicObjIterator> and
   <See Class=TGraphicObjList>.

   <B=Note>: To release an iterator simply free it. If you lost some
   reference to iterators (and so the list is blocked) you can unblock
   the list with the method <See Method=TGraphicObjList@RemoveAllIterators>.

   <B=Note>: An iterator cannot be created directly but only with the
   use of the methods <See Method=TGraphicObjList@GetIterator>,
   <See Method=TGraphicObjList@GetExclusiveIterator> and
   <See Method=TGraphicObjList@GetPrivilegedIterator>.
}
  TGraphicObjIterator = class(TObject)
  private
    fCurrent: Pointer;
    fSourceList: TGraphicObjList;

    function GetCurrentObject: TGraphicObject;
    function SearchBlock(ID: Integer): Pointer;
    function GetCount: Integer;
    constructor Create(const Lst: TGraphicObjList); virtual;
  public
{: This is the descructor of the iterator.

   When an iterator is freed (with the use of <I=Free>) the iterator's
   count in the list from which the iterator was obtained will be decremented
   and the list will be unblocked if it was.

   <B=Remeber to free all the iterators you have when you finish with
   them>.
}
    destructor Destroy; override;
{: Return the object with the specified <I=ID> if found. If that object
   doen't exist in the list <B=nil> will be returned.

   If an object is found then the position in the list will be moved
   to it, if no it will not be moved.
   No exception is raised if no object is found.
}
    function Search(const ID: Integer): TGraphicObject;
{: Move the current position (<See Property=TGraphicObjIterator@Current>
   to the next object in the list, and return it.

   If the current position is at the end of the list the current position
   will be set to <B=nil> and <B=nil> will be returned.
}
    function Next: TGraphicObject;
{: Move the current position (<See Property=TGraphicObjIterator@Current>
   to the previous object in the list, and return it.

   If the current position is at the beginnig of the list the current position
   will be set to <B=nil> and <B=nil> will be returned.
}
    function Prev: TGraphicObject;
{: Move the current position (<See Property=TGraphicObjIterator@Current>
   to the first object in the list, and return it.

   If the list is empty current position
   will be set to <B=nil> and <B=nil> will be returned.
}
    function First: TGraphicObject;
{: Move the current position (<See Property=TGraphicObjIterator@Current>
   to the last object in the list, and return it.

   If the list is empty current position
   will be set to <B=nil> and <B=nil> will be returned.
}
    function Last: TGraphicObject;
    //For use in "while" cicles
    // Send nil as an argument to start from First
    function GetNext(var Obj: TGraphicObject): Boolean;
{: This property contains the number of items in the list.

   You may want to use it in combination with
   <See Property=TGraphicObjIterator@Items> to iterate the list
   in an array-like mode.
}
    property Count: Integer read GetCount;
{: This property contains the current object onto which the iterator
   is positioned. If the position is invalid it will be <B=nil>.
}
    property Current: TGraphicObject read GetCurrentObject;
{: This property contains the list linked to the iterator.
}
    property SourceList: TGraphicObjList read fSourceList;
{: This property contains the items in the list.

   You may want to use it in combination with
   <See Property=TGraphicObjIterator@Count> to iterate the list
   in an array-like mode.

   <I=ID> is zero based so the bounds of the array are from <I=0> to
   <See Property=TGraphicObjIterator@Count>.
}
    property Items[const ID: Integer]: TGraphicObject read
    Search; default;
  end;

{: This class defines an exclusive iterator for a list of graphical objects
   (<See class=TGraphicObjList>).

   The library can use a <I=thread> to paint the drawings and so
   a way is needed to ensure that a list is not changed when it is
   in use by another thread.

   To do so the concept of <I=iterator> is used. An iterator is
   an object that is used to traverse a list of objects. When you
   ask and obtain an iterator on a list, you can access the elements
   in the list through the iterator. You cannot modify a list if you
   doesn't have an <I=exclusive iterator> that let you to modify the
   list as well as iterate it (<See Class=TExclusiveGraphicObjIterator>).

   As soon as you obtain an iterator the list cannot be modified and
   so you are ensured that it will remain the same during the iteration.
   Of course if you doen't release an iterator you cannot modify the list
   until the end of the program, so attention is needed when using an
   iterator.

   For backward compatibility some operations on the list are avaiable
   with the list itself (and these are however implemented with temporary
   iterators so using them is the better approach).

   See also <See Class=TExclusiveGraphicObjIterator> and
   <See Class=TGraphicObjList>.

   <B=Note>: To release an iterator simply free it. If you lost some
   reference to iterators (and so the list is blocked) you can unblock
   the list with the method <See Method=TGraphicObjList@RemoveAllIterators>.

   <B=Note>: An iterator cannot be created directly but only with the
   use of the methods <See Method=TGraphicObjList@GetIterator>,
   <See Method=TGraphicObjList@GetExclusiveIterator> and
   <See Method=TGraphicObjList@GetPrivilegedIterator>.
}
  TExclusiveGraphicObjIterator = class(TGraphicObjIterator)
  private
    constructor Create(const Lst: TGraphicObjList); override;
  public
    destructor Destroy; override;
{: This method delete the current object from the source list.
   After the object is deleted the current position in the list
   will be set to the next object in it (if it is at the end
   of the list it will be set to the previous object).

   This method is affected by the state of the
   <See Property=TGraphicObjList@FreeOnClear> property.
}
    procedure DeleteCurrent;
{: This method remove the current object from the source list but
   doesn't free it.
   After the object is removed the current position in the list
   will be set to the next object in it (if it is at the end
   of the list it will be set to the previous object).

   This method is <B=NOT> affected by the state of the
   <See Property=TGraphicObjList@FreeOnClear> property.
}
    procedure RemoveCurrent;
    //TSY: deletes object from the list and puts another object to that place
    procedure ReplaceDeleteCurrent(const Obj: TGraphicObject);
  end;

{: This class defines a double linked list of graphic objects.

   The objects are referenced in the list through their
   identifier number (<See Property=TGraphicObject@ID>). The list doens't
   check for duplicated <I=ID> so you have to ensure uniqueness by yourself.

   To traverse the list you have to use instances of
   <See Class=TGraphicObjIterator> and <See Class=TExclusiveGraphicObjIterator>
   that can be obtained with the methods <See Method=TGraphicObjList@GetIterator>,
   <See Method=TGraphicObjList@GetExclusiveIterator> and <See Method=TGraphicObjList@GetPrivilegedIterator>.

   All of the methods that modify the list can be used only if no iterators
   are attached to the list. If this is not the case an
   <See Class=ETpX_ListBlocked> exception will be raised.

   The list can own the object it contains depending on the setting of
   the property <See Property=TGraphicObjList@FreeOnClear>.
}
  TGraphicObjList = class(TObject)
  private
    fHead, fTail: Pointer;
    fHasExclusive, fFreeOnClear: Boolean;
    fIterators: Integer; { Usato come semaforo. }
    fCount: Integer;
    fListGuard: TTpXCriticalSection;

    procedure DeleteBlock(ObjToDel: Pointer);
    procedure RemoveBlock(ObjToDel: Pointer);
    function GetHasIter: Boolean;
  public
{: This is the constructor of the list. It creates a new empty list.

  It sets <See Property=TGraphicObjList@FreeOnClear> to <B=True>.
}
    constructor Create;
{: This is the destructor of the list.

  If the property <See Property=TGraphicObjList@FreeOnClear> is <B=True>
  the the objects in the list will also be freed.
}
    destructor Destroy; override;
{: This method adds an object to the list.

   The added object <I=Obj> will be placed at the end of the list, after
   any other object already present in the list. The list doesn't check
   for uniqueness of the object's <See Property=TGraphicObject@ID>.

   If the list has some iterators active on it a
   <See Class=ETpX_ListBlocked> exception will be raised.
}
    procedure Add(const Obj: TGraphicObject);
{: This method adds all the object in another list <I=Lst> to the list.

   The added objects will be placed at the end of the list, after
   any other object already present in the list. The list doesn't check
   for uniqueness of the object's <See Property=TGraphicObject@ID>.

   If the list has some iterators active it a
   <See Class=ETpX_ListBlocked> exception will be raised.
}
    procedure AddFromList(const Lst: TGraphicObjList);
{: This method inserts an object into the list.

   The added object <I=Obj> will be inserted after the object in the
   list with ID equal to <I=IDInsertPoint>. If no such object is present
   a <See Class=ETpX_ListObjNotFound> exception will be raised.
   The list doesn't check for uniqueness of the inserted object.

   If the list has some iterators active on it a
   <See Class=ETpX_ListBlocked> exception will be raised.
}
    procedure Insert(const IDInsertPoint: Integer; const Obj:
      TGraphicObject);
{: This method moves an object into the list.

   The object with ID equal to <I=IDToMove> will be moved before the
   object in the list with ID equal to <I=IDInsertionPoint> .
   If no such object is present a <See Class=ETpX_ListObjNotFound>
   exception will be raised.

   If the list has some iterators active it a
   <See Class=ETpX_ListBlocked> exception will be raised.

   <B=Note>: This method is useful if you want to change the
   drawing order in the list of object of a <See Class=TDrawing>
   control.
}
    procedure Move(const IDToMove, IDInsertPoint: Integer);
{: This method deletes an object from the list.

   The object with ID equal to <I=ID> will be deleted.
   If no such object is present a <See Class=ETpX_ListObjNotFound>
   exception will be raised.

   If the list has the property <See Property=TGraphicObjList@FreeOnClear>
   set to <B=True> then the object will be deleted by calling its <I=Free>
   method.

   If the list has some iterators active on it a
   <See Class=ETpX_ListBlocked> exception will be raised.

   <B=Note>: If you want to delete more that one object from the
   list use an exclusive iterator for better performances.
}
    function Delete(const ID: Integer): Boolean;
{: This method removes an object from the list.

   The object with ID equal to <I=ID> will be removed.
   If no such object is present a <See Class=ETpX_ListObjNotFound>
   exception will be raised.

   The object will not also be deleted if the property <See Property=TGraphicObjList@FreeOnClear>
   is set to <B=True>.

   If the list has some iterators active on it a
   <See Class=ETpX_ListBlocked> exception will be raised.

   <B=Note>: If you want to remove more that one object from the
   list use an exclusive iterator for better performances.
}
    function Remove(const ID: Integer): Boolean;
{: This method returns the object with the gived Id.

   The object with ID equal to <I=ID> will be returned if found, or <B=nil>
   will be returned if no object is found.

   If the list has some exclusive iterators active on it a
   <See Class=ETpX_ListBlocked> exception will be raised.

   <B=Note>: If you have an iterator on the list don't use this method.
   Use it only if you want to find a single object.
}
    function Find(const ID: Integer): TGraphicObject;
{: This method remove all the object from the list.

   The objects will be deleted if the property <See Property=TGraphicObjList@FreeOnClear>
   is set to <B=True>.

   If the list has some iterators active on it a
   <See Class=ETpX_ListBlocked> exception will be raised.
}
    procedure Clear;
{: This method set a new iterator on the list.

   The new iterator will be returned and you can start to use it.
   After you have used it remember to <B=Free> it. It is better to
   protect the use of the iterator in a <B=try-finally> block.

   If the list has an exclusive iterator active on it a
   <See Class=ETpX_ListBlocked> exception will be raised.
}
    function GetIterator: TGraphicObjIterator;
{: This method set a new exclusive iterator on the list.

   The new iterator will be returned and you can start to use it.
   After you have used it remember to <B=Free> it. It is better to
   protect the use of the iterator in a <B=try-finally> block.

   An exclusive iterator prevent any other thread to modify the list,
   so grant you an exclusive use of the list. With this kind of
   iterator you can remove objects from the list.

   If the list has any iterators active on it a
   <See Class=ETpX_ListBlocked> exception will be raised.
}
    function GetExclusiveIterator: TExclusiveGraphicObjIterator;
{: This method set a new exclusive iterator on the list ignoring
   any other pending iterator already present.

   The new iterator will be returned and you can start to use it.
   After you have used it remember to <B=Free> it. It is better to
   protect the use of the iterator in a <B=try-finally> block.

   A priviliged iterator is somewhat dangerous and must be used only
   in case of error when an iterator was active on the list and cannot
   be freed (for instance it is useful when the application must be
   aborted in presence of errors but you want to save any other
   present in the drawing).
}
    function GetPrivilegedIterator:
      TExclusiveGraphicObjIterator;
{: This method removes any pending iterator active on the list.

   This method is somewhat dangerous and must be used only
   in case of error when an iterator was active on the list and cannot
   be freed (for instance it is useful when the application must be
   aborted in presence of errors but you want to save any other
   present in the drawing).
}
    procedure RemoveAllIterators;
    procedure TransForm(const T: TTransf2D);
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
      const VisualRect: TRect2D; const PickFilter: TObject2DClass;
      const FirstFound: Boolean; var NPoint: Integer): TObject2D;
{: This property returns the number of objects present in the list.

   It is useful when you want to iterate through the object in the
   list with an iterators.
}
    property Count: Integer read fCount;
{: This property returns <B=True> if the list has some iterator active on
   it.

   Check this property before asking for an iterator.
}
    property HasIterators: Boolean read GetHasIter;
{: This property returns <B=True> if the list has an exclusive iterator
   active on it.

   Check this property before asking for an exclusive iterator.
}
    property HasExclusiveIterators: Boolean read fHasExclusive;
{: This property tells the list if the objects in the list must
   be freed when removed from it.

   If it is set to <B=True> the objects in the list are deleted
   (by calling their <I=Free> method) when they are removed from
   the list by using the <See Method=TGraphicObjList@Delete>,
   <See Method=TGraphicObjList@Clear> methods or when the list
   is deleted.
}
    property FreeOnClear: Boolean read fFreeOnClear write
      fFreeOnClear;
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
    constructor CreateFromStream(const Stream: TStream); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method transform the object with a give transformation matrix.

       The <I=T> transform matrix will be post multiplied with the
       <I=current model matrix> and the result will take the place of
       the <I=current model matrix>.

       See the <See Class=TObject2D> class for details about the
       model matrix of a 2D object.
    }
    procedure TransForm(const T: TTransf2D); dynamic;
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
      const Dvc: TDevice; const ClipRect2D: TRect2D); virtual;
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
    function IsVisible(const Clip: TRect2D): Boolean; virtual;
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
      const ClipRect2D: TRect2D; const Width: Integer); dynamic;
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
    property DrawBoundingBox: Boolean read fDrawBoundingBox write
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

     A container can be used to group objects that must be moved or
     transformed as a whole. On the other hand, if you want to reuse
     a set of objects in different place on the drawing use the
     <See Class=TSourceBlock2D> class.
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
    constructor CreateFromStream(const Stream: TStream); override;
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

       The method needs the iterator of source blocks list in order
       to relink the blocks. This iterator can be obtained from
       <See Property=TDrawing@SourceBlocksIterator>.

       The method call the <See Method=TGraphicObject@UpdateExtension>.
    }
    procedure UpdateSourceReferences(const BlockList:
      TGraphicObjIterator);
    procedure TransForm(const T: TTransf2D); override;
    procedure DeviceDraw(Transf: TTransf2D;
      const Dvc: TDevice; const ClipRect2D: TRect2D); override;
    procedure DrawControlPoints(const VT: TTransf2D;
      const ClipRect2D: TRect2D; const Width: Integer); override;
    function OnMe(P: TPoint2D; Aperture: TRealType; var
      Distance: TRealType): Integer; override;
    {: This property contains the list that contians the object in the container.

       The objects in the container are owned by the container itself. Use
       this property to manage these objects.
    }
    property Objects: TGraphicObjList read fObjects;
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
    constructor CreateFromStream(const Stream: TStream); override;
    destructor Destroy; override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
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
    constructor CreateFromStream(const Stream: TStream); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method updates the references of the block.

       This method is called automatically at the end of the loading
       process of the block from a stream. Indeed when you save a
       block the reference to the source block is no longer valid,
       so the block must be relinked to the correct source blocks
       instance.

       The method needs the iterator of the source blocks of a drawing in
       order to relink the block's source block. This iterator can be
       obtained by <See Property=TDrawing@SourceBlocksIterator>
    }
    procedure UpdateReference(const BlockList:
      TGraphicObjIterator);
    procedure DrawControlPoints(const VT: TTransf2D;
      const ClipRect2D: TRect2D; const Width: Integer); override;
    procedure DeviceDraw(Transf: TTransf2D;
      const Dvc: TDevice; const ClipRect2D: TRect2D); override;
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
    property SourceBlock: TSourceBlock2D read fSourceBlock write
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
    fModified: Boolean;
    fStreamable: Boolean;
    fIdx: Byte;
    fTag: Integer;

    procedure SetName(NM: TLayerName);
    procedure Changed(Sender: TObject);
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
{: This property is <B=True> if the layer was modified and so it must be
   saved, otherwise it will not be saved to a drawing.

   If a layer is not saved it will get the default settings.
}
    property Modified: Boolean read fModified;
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
    property Layers[Index: Byte]: TLayer read GetLayer; default;
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

  PObjBlock = ^TObjBlock;

  TObjBlock = record
    Obj: TGraphicObject; { Graphic object. }
    Next, Prev: PObjBlock; { Linked list pointer. }
  end;

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
  inherited Create;
  fID := -1;
  fLayer := 0;
  fVisible := True;
  fEnabled := True;
  fToBeSaved := True;
  fTag := 0;
  fOnChange := nil;
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
// TGraphicObjList
// =====================================================================

constructor TGraphicObjList.Create;
begin
  inherited Create;

  fListGuard := TTpXCriticalSection.Create;
  fHead := nil;
  fTail := nil;
  fIterators := 0;
  fHasExclusive := False;
  fCount := 0;
  fFreeOnClear := True;
end;

destructor TGraphicObjList.Destroy;
var
  TmpBlock: PObjBlock;
begin
  while fHead <> nil do
  begin
    try
      if fFreeOnClear and Assigned(TObjBlock(fHead^).Obj) then
        TObjBlock(fHead^).Obj.Free;
    except
    end;
    TmpBlock := fHead;
    fHead := TObjBlock(fHead^).Next;
    FreeMem(TmpBlock, SizeOf(TObjBlock));
  end;
  fListGuard.Free;
  inherited;
end;

function TGraphicObjList.GetHasIter: Boolean;
begin
  Result := fIterators > 0;
end;

function TGraphicObjList.GetIterator: TGraphicObjIterator;
begin
  if fHasExclusive then
    raise
      ETpX_ListBlocked.Create('TGraphicObjList.GetIterator: The list has exclusive iterator.');
  Result := TGraphicObjIterator.Create(Self);
end;

function TGraphicObjList.GetExclusiveIterator:
  TExclusiveGraphicObjIterator;
begin
  if fIterators > 0 then
    raise
      ETpX_ListBlocked.Create('TGraphicObjList.GetExclusiveIterator: The list has active iterators.');
  Result := TExclusiveGraphicObjIterator.Create(Self);
end;

function TGraphicObjList.GetPrivilegedIterator:
  TExclusiveGraphicObjIterator;
begin
  Result := TExclusiveGraphicObjIterator.Create(Self);
end;

procedure TGraphicObjList.RemoveAllIterators;
begin
  fListGuard.Enter;
  try
    fIterators := 0;
    fHasExclusive := False;
  finally
    fListGuard.Leave;
  end;
end;

procedure TGraphicObjList.TransForm(const T: TTransf2D);
var
  ExIter: TExclusiveGraphicObjIterator;
  Current: TGraphicObject;
begin
  ExIter := GetExclusiveIterator;
  try
    Current := nil;
    while ExIter.GetNext(Current) do
      if Current is TObject2D then
        (Current as TObject2D).TransForm(T);
  finally
    ExIter.Free;
  end;
end;

function TGraphicObjList.PickObject(const P: TPoint2D;
  Layers: TLayers; const Aperture: TRealType;
  const VisualRect: TRect2D; const PickFilter: TObject2DClass;
  const FirstFound: Boolean; var NPoint: Integer): TObject2D;
var
  TmpNPoint: Integer;
  Tmp: TObject2D;
  MinDist, Distance: TRealType;
  TmpIter: TExclusiveGraphicObjIterator;
  //TSY:
  function NPointLevel(const NPoint: Integer): TRealType;
  begin
    Result := NPoint;
    if NPoint > -2 then Result := -2;
  end;
begin
  Result := nil;
  if Layers = nil then Exit;
  TmpIter := GetExclusiveIterator;
  try
    MinDist := Aperture;
    NPoint := PICK_NOOBJECT;
    Tmp := TmpIter.Current as TObject2D;
    while Tmp <> nil do
      with (Layers[Tmp.Layer]) do
      begin
        if Active and Visible and (Tmp is PickFilter) and
          Tmp.IsVisible(VisualRect) then
        begin
          TmpNPoint := Tmp.OnMe(P, Aperture, Distance);
          //TSY:
          //if TmpNPoint = -2 then Distance := 0;
          if (NPointLevel(TmpNPoint) >= NPointLevel(NPoint))
            and (Distance <= MinDist) then
          begin
            Result := Tmp;
            NPoint := TmpNPoint;
            MinDist := Distance;
            if FirstFound then
              Break;
          end;
        end;
        Tmp := TmpIter.Next as TObject2D;
      end;
  finally
    TmpIter.Free;
  end;
end;


procedure TGraphicObjList.Add(const Obj: TGraphicObject);
var
  NewBlock: PObjBlock;
begin
  if fIterators > 0 then
    raise
      ETpX_ListBlocked.Create('TGraphicObjList.Add: The list has active iterators.');
  fListGuard.Enter;
  try
    { Allocate new blocks. }
    GetMem(NewBlock, SizeOf(TObjBlock));
    { Initialize the block. }
    NewBlock^.Prev := fTail;
    NewBlock^.Next := nil;
    NewBlock^.Obj := Obj;
    if fHead = nil then
      fHead := NewBlock;
    if fTail <> nil then
      TObjBlock(fTail^).Next := NewBlock;
    fTail := NewBlock;
    Inc(fCount);
  finally
    fListGuard.Leave;
  end;
end;

procedure TGraphicObjList.AddFromList(const Lst:
  TGraphicObjList);
var
  TmpIter: TGraphicObjIterator;
  I: Integer;
begin
  if Lst.Count = 0 then
    Exit;
  if fIterators > 0 then
    raise
      ETpX_ListBlocked.Create('TGraphicObjList.AddFromList: The list has active iterators.');
  // Alloca un iterator locale
  fListGuard.Enter;
  try
    TmpIter := Lst.GetIterator;
    I := 0;
    try
      repeat
        Add(TmpIter.Current);
        Inc(I);
        if I mod 100 = 0 then ShowProgress(I / Lst.Count);
      until TmpIter.Next = nil;
    finally
      TmpIter.Free;
    end;
  finally
    fListGuard.Leave;
  end;
end;

procedure TGraphicObjList.Insert(const IDInsertPoint: Integer;
  const Obj: TGraphicObject);
var
  NewBlock: PObjBlock;
  InsertPoint: PObjBlock;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  { Safe guard. }
  if fHead = nil then
    raise
      ETpX_SysException.Create('TGraphicObjList.Insert: No objects in the list');
  if fIterators > 0 then
    raise
      ETpX_ListBlocked.Create('TGraphicObjList.Insert: The list has active iterators.');
  // Alloca un iterator locale
  TmpIter := GetExclusiveIterator;
  try
    InsertPoint := TmpIter.SearchBlock(IDInsertPoint);
    if (InsertPoint = nil) then
      raise
        ETpX_ListObjNotFound.Create('TGraphicObjList.Insert: Object not found');
    { Allocate new blocks. }
    GetMem(NewBlock, SizeOf(TObjBlock));
    { Initialize the block. }
    NewBlock^.Prev := InsertPoint^.Prev;
    NewBlock^.Next := InsertPoint;
    NewBlock^.Obj := Obj;
    if InsertPoint^.Prev <> nil then
      InsertPoint^.Prev^.Next := NewBlock
    else
      fHead := NewBlock;
    InsertPoint^.Prev := NewBlock;
    Inc(fCount);
  finally
    TmpIter.Free;
  end;
end;

procedure TGraphicObjList.Move(const IDToMove, IDInsertPoint:
  Integer);
var
  InsertPoint, ToMove: PObjBlock;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  { Safe guard. }
  if fHead = nil then
    raise
      ETpX_SysException.Create('TGraphicObjList.Move: No objects in the list');
  if fIterators > 0 then
    raise
      ETpX_ListBlocked.Create('TGraphicObjList.Move: The list has active iterators.');
  // Alloca un iterator locale
  TmpIter := GetExclusiveIterator;
  try
    { Check if the current and insert point are the same. }
    if IDInsertPoint = IDToMove then
      raise
        ETpX_SysException.Create('TGraphicObjList.Move: Bad object to move');
    InsertPoint := TmpIter.SearchBlock(IDInsertPoint);
    ToMove := TmpIter.SearchBlock(IDToMove);
    if (InsertPoint = nil) then
      raise
        ETpX_ListObjNotFound.Create('TGraphicObjList.Move: Insertion point not found');
    if (ToMove = nil) then
      raise
        ETpX_ListObjNotFound.Create('TGraphicObjList.Move: Object to move not found');
    { Now I have the block to move and the insertion point. }
    if ToMove^.Prev <> nil then
      ToMove^.Prev^.Next := ToMove^.Next
    else
      fHead := ToMove^.Next;
    if ToMove^.Next <> nil then
      ToMove^.Next^.Prev := ToMove^.Prev
    else
      fTail := ToMove^.Prev;
    { Set new link. }
    if InsertPoint^.Prev <> nil then
      InsertPoint^.Prev^.Next := ToMove
    else
      fHead := ToMove;
    ToMove^.Next := InsertPoint;
    ToMove^.Prev := InsertPoint^.Prev;
    InsertPoint^.Prev := ToMove;
  finally
    TmpIter.Free;
  end;
end;

procedure TGraphicObjList.DeleteBlock(ObjToDel: Pointer);
begin
  fListGuard.Enter;
  try
    // Free the first object. So if it cannot be deleted it will be later.
    if Assigned(TObjBlock(ObjToDel^).Obj) and fFreeOnClear then
      TObjBlock(ObjToDel^).Obj.Free;
    // First extract the block from all list.
    if TObjBlock(ObjToDel^).Next <> nil then
      TObjBlock(ObjToDel^).Next^.Prev :=
        TObjBlock(ObjToDel^).Prev
    else
     { I reached the Last block. }
      fTail := TObjBlock(ObjToDel^).Prev;
    if TObjBlock(ObjToDel^).Prev <> nil then
      TObjBlock(ObjToDel^).Prev^.Next :=
        TObjBlock(ObjToDel^).Next
    else
     { I reached the head. }
      fHead := TObjBlock(ObjToDel^).Next;
    FreeMem(ObjToDel, SizeOf(TObjBlock));
    Dec(fCount);
  finally
    fListGuard.Leave;
  end;
end;

function TGraphicObjList.Delete(const ID: Integer): Boolean;
var
  ObjToDel: PObjBlock;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  Result := False;
  if fIterators > 0 then
    raise
      ETpX_ListBlocked.Create('TGraphicObjList.Delete: The list has active iterators.');
  // Alloca un iterator locale
  TmpIter := GetExclusiveIterator;
  try
    ObjToDel := TmpIter.SearchBlock(ID);
    if ObjToDel = nil then
      raise
        ETpX_ListObjNotFound.Create('TGraphicObjList.Delete: Object not found');
    DeleteBlock(ObjToDel);
    Result := True;
  finally
    TmpIter.Free;
  end;
end;

function TGraphicObjList.Find(const ID: Integer):
  TGraphicObject;
var
  FoundObj: PObjBlock;
  TmpIter: TGraphicObjIterator;
begin
  Result := nil;
  // Alloca un iterator locale
  TmpIter := GetIterator;
  try
    FoundObj := TmpIter.SearchBlock(ID);
    if FoundObj = nil then
      Exit;
    Result := TObjBlock(FoundObj^).Obj;
  finally
    TmpIter.Free;
  end;
end;

procedure TGraphicObjList.RemoveBlock(ObjToDel: Pointer);
begin
  fListGuard.Enter;
  try
    // First extract the block from all list.
    if TObjBlock(ObjToDel^).Next <> nil then
      TObjBlock(ObjToDel^).Next^.Prev :=
        TObjBlock(ObjToDel^).Prev
    else
     { I reached the Last block. }
      fTail := TObjBlock(ObjToDel^).Prev;
    if TObjBlock(ObjToDel^).Prev <> nil then
      TObjBlock(ObjToDel^).Prev^.Next :=
        TObjBlock(ObjToDel^).Next
    else
     { I reached the head. }
      fHead := TObjBlock(ObjToDel^).Next;
    FreeMem(ObjToDel, SizeOf(TObjBlock));
    Dec(fCount);
  finally
    fListGuard.Leave;
  end;
end;

function TGraphicObjList.Remove(const ID: Integer): Boolean;
var
  ObjToDel: PObjBlock;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  Result := False;
  if fIterators > 0 then
    raise
      ETpX_ListBlocked.Create('TGraphicObjList.Remove: The list has active iterators.');
  // Alloca un iterator locale
  TmpIter := GetExclusiveIterator;
  try
    ObjToDel := TmpIter.SearchBlock(ID);
    if ObjToDel = nil then
      raise
        ETpX_ListObjNotFound.Create('TGraphicObjList.Remove: No object found');
    RemoveBlock(ObjToDel);
    Result := False;
  finally
    TmpIter.Free;
  end;
end;

procedure TGraphicObjList.Clear;
var
  TmpBlock: PObjBlock;
begin
  if fIterators > 0 then
    raise
      ETpX_ListBlocked.Create('TGraphicObjList.Clear: The list has active iterators.');
  fListGuard.Enter;
  try
    while fHead <> nil do
    begin
      try
        if fFreeOnClear and Assigned(TObjBlock(fHead^).Obj) then
          TObjBlock(fHead^).Obj.Free;
      except
      end;
      TmpBlock := fHead;
      fHead := TObjBlock(fHead^).Next;
      FreeMem(TmpBlock, SizeOf(TObjBlock));
    end;
    fHead := nil;
    fTail := nil;
    fCount := 0;
  finally
    fListGuard.Leave;
  end;
end;

// =====================================================================
// TGraphicObjIterator
// =====================================================================

{ Search the block corresponding to ID. }

function TGraphicObjIterator.SearchBlock(ID: Integer): Pointer;
begin
  Result := fSourceList.fHead;
  while (Result <> nil) and (TObjBlock(Result^).Obj.ID <> ID) do
  begin
    if TObjBlock(Result^).Prev = fSourceList.fTail then
    begin
      Result := nil;
      Break;
    end;
    Result := TObjBlock(Result^).Next;
  end;
end;

function TGraphicObjIterator.GetCurrentObject: TGraphicObject;
begin
  if fCurrent <> nil then
    Result := TObjBlock(fCurrent^).Obj
  else
    Result := nil;
end;

constructor TGraphicObjIterator.Create(const Lst:
  TGraphicObjList);
begin
  inherited Create;

  Lst.fListGuard.Enter;
  try
    fCurrent := nil;
    fSourceList := Lst;
    if fSourceList <> nil then
    begin
      Inc(fSourceList.fIterators);
      fCurrent := fSourceList.fHead;
    end;
  finally
    Lst.fListGuard.Leave;
  end;
end;

destructor TGraphicObjIterator.Destroy;
begin
  fSourceList.fListGuard.Enter;
  try
    if fSourceList <> nil then
    begin
      Dec(fSourceList.fIterators);
      if fSourceList.fIterators < 0 then
        fSourceList.fIterators := 0;
    end;
  finally
    fSourceList.fListGuard.Leave;
  end;
  inherited;
end;

function TGraphicObjIterator.GetCount: Integer;
begin
  if (fSourceList = nil) then
    Result := 0
  else
    Result := fSourceList.Count;
end;

function TGraphicObjIterator.Search(const ID: Integer):
  TGraphicObject;
var
  TmpBlock: PObjBlock;
begin
  Result := nil;
  if (fSourceList = nil) then
    Exit;
  TmpBlock := SearchBlock(ID);
  if TmpBlock = nil then
    Exit;
  Result := TmpBlock^.Obj;
  fCurrent := TmpBlock;
end;

function TGraphicObjIterator.Next: TGraphicObject;
begin
  Result := nil;
  if (fSourceList = nil) and (fCurrent = nil) then
    Exit;
  { Check if the End of the list is reached. }
  if fCurrent = Pointer(fSourceList.fTail) then
  begin
    fCurrent := nil;
    Exit;
  end;
  fCurrent := TObjBlock(fCurrent^).Next;
  Result := TObjBlock(fCurrent^).Obj;
end;

function TGraphicObjIterator.Prev: TGraphicObject;
begin
  Result := nil;
  if (fSourceList = nil) and (fCurrent = nil) then
    Exit;
  { Check if the start of the list is reached. }
  if fCurrent = Pointer(fSourceList.fHead) then
  begin
    fCurrent := nil;
    Exit;
  end;
  fCurrent := TObjBlock(fCurrent^).Prev;
  Result := TObjBlock(fCurrent^).Obj;
end;

function TGraphicObjIterator.First: TGraphicObject;
begin
  Result := nil;
  if fSourceList = nil then
    Exit;
  fCurrent := fSourceList.fHead;
  if fCurrent = nil then
    Exit;
  Result := TObjBlock(fCurrent^).Obj;
end;

function TGraphicObjIterator.Last: TGraphicObject;
begin
  Result := nil;
  if fSourceList = nil then
    Exit;
  fCurrent := fSourceList.fTail;
  if fCurrent = nil then
    Exit;
  Result := TObjBlock(fCurrent^).Obj;
end;

function TGraphicObjIterator.GetNext(var Obj: TGraphicObject):
  Boolean;
begin
  if Obj = nil then
    Obj := First
  else
    Obj := Next;
  Result := Obj <> nil;
end;

// =====================================================================
// TExclusiveGraphicObjIterator
// =====================================================================

constructor TExclusiveGraphicObjIterator.Create(const Lst:
  TGraphicObjList);
begin
  inherited Create(Lst);
  Lst.fListGuard.Enter;
  try
    if fSourceList <> nil then
      fSourceList.fHasExclusive := True;
  finally
    Lst.fListGuard.Leave;
  end;
end;

destructor TExclusiveGraphicObjIterator.Destroy;
begin
  fSourceList.fListGuard.Enter;
  try
    if fSourceList <> nil then
      fSourceList.fHasExclusive := False;
  finally
    fSourceList.fListGuard.Leave;
  end;
  inherited;
end;

procedure TExclusiveGraphicObjIterator.DeleteCurrent;
var
  TmpObj: Pointer;
begin
  if (fSourceList <> nil) and (fCurrent <> nil) then
  begin
    if TObjBlock(fCurrent^).Next <> nil then
      TmpObj := TObjBlock(fCurrent^).Next
    else
      TmpObj := TObjBlock(fCurrent^).Prev;
    fSourceList.DeleteBlock(fCurrent);
    fCurrent := TmpObj;
  end;
end;

procedure TExclusiveGraphicObjIterator.RemoveCurrent;
var
  TmpObj: Pointer;
begin
  if (fSourceList <> nil) and (fCurrent <> nil) then
  begin
    if TObjBlock(fCurrent^).Next <> nil then
      TmpObj := TObjBlock(fCurrent^).Next
    else
      TmpObj := TObjBlock(fCurrent^).Prev;
    fSourceList.RemoveBlock(fCurrent);
    fCurrent := TmpObj;
  end;
end;

procedure TExclusiveGraphicObjIterator.ReplaceDeleteCurrent(
  const Obj: TGraphicObject);
var
  OwnerDrawing: TObject;
begin
  if (fSourceList = nil) or (fCurrent = nil) then Exit;
  OwnerDrawing := nil;
  if Assigned(TObjBlock(fCurrent^).Obj) and
    fSourceList.fFreeOnClear then
  begin
    OwnerDrawing := TObjBlock(fCurrent^).Obj.OwnerDrawing;
    TObjBlock(fCurrent^).Obj.Free;
  end;
  Obj.fOwnerDrawing := OwnerDrawing;
  TObjBlock(fCurrent^).Obj := Obj;
end;

// =====================================================================
// TLayer
// =====================================================================

procedure TLayer.Changed(Sender: TObject);
begin
  fModified := True;
end;

procedure TLayer.SetName(NM: TLayerName);
begin
  if fName <> NM then
  begin
    fName := NM;
    fModified := True;
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
  fModified := False;
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
  fModified := True;
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
      if TLayer(fLayers[Cont]).fModified then
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
  TransForm(TmpTransf);
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

procedure TObject2D.TransForm(const T: TTransf2D);
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
  fObjects.FreeOnClear := True;
  fDrawBoundingBox := True;
end;

constructor TContainer2D.CreateSpec(ID: Integer; const Objs: array
  of TObject2D);
var
  Cont: Word;
begin
  inherited Create(ID);

  fObjects := TGraphicObjList.Create;
  fObjects.FreeOnClear := True;
  for Cont := Low(Objs) to High(Objs) do
    if Objs[Cont] <> nil then fObjects.Add(Objs[Cont]);
  UpdateExtension(Self);
  fDrawBoundingBox := True;
end;

procedure TContainer2D._UpdateExtension;
var
  TmpIter: TGraphicObjIterator;
begin
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  try
    if TmpIter.Count = 0 then
    begin
      fBoundingBox := Rect2D(0, 0, 0, 0);
      Exit;
    end;
    fBoundingBox := TObject2D(TmpIter.First).BoundingBox;
    while TmpIter.Next <> nil do
      fBoundingBox := BoxOutBox2D(fBoundingBox,
        TObject2D(TmpIter.Current).BoundingBox);
  finally // Libera l'iterator
    TmpIter.Free;
  end;
end;

destructor TContainer2D.Destroy;
begin
  fObjects.Free;
  inherited Destroy;
end;

procedure TContainer2D.SaveToStream(const Stream: TStream);
var
  TmpObj: TObject2D;
  TmpLong: Integer;
  TmpWord: Word;
  TmpIter: TGraphicObjIterator;
begin
  inherited SaveToStream(Stream);
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  with Stream do
  try
     { Write the number of objects in the container. }
    TmpLong := fObjects.Count;
    Write(TmpLong, SizeOf(TmpLong));
     { Now write the objects in the container. }
    TmpObj := TObject2D(TmpIter.First);
    while TmpObj <> nil do
    begin
      TmpWord := TpXFindClassIndex(TmpObj.ClassName);
        { Save the class index. }
      Write(TmpWord, SizeOf(TmpWord));
        { Save the object. }
      TmpObj.SaveToStream(Stream);
      TmpObj := TObject2D(TmpIter.Next);
    end;
  finally // Libera l'iterator
    TmpIter.Free;
  end;
end;

constructor TContainer2D.CreateFromStream(const Stream: TStream);
var
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
  TmpLong: Integer;
  TmpWord: Word;
begin
  inherited;
  with Stream do
  begin
     { Read the number of objects in the container. }
    Read(TmpLong, SizeOf(TmpLong));
    fObjects := TGraphicObjList.Create;
    fObjects.FreeOnClear := True;
     { Now read the objects for the container. }
    while TmpLong > 0 do
    begin
        { Read the type of object. }
      Read(TmpWord, SizeOf(TmpWord));
        { Retrive the class type from the registered classes. }
      TmpClass := TpXFindClassByIndex(TmpWord);
      TmpObj := TmpClass.CreateFromStream(Stream);
      TmpObj.UpdateExtension(Self);
      fObjects.Add(TmpObj);
      Dec(TmpLong);
    end;
  end;
  UpdateExtension(Self);
end;

procedure TContainer2D.Assign(const Obj: TGraphicObject);
var
  TmpIter: TGraphicObjIterator;
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
begin
  if (Obj = Self) then
    Exit;
  inherited;
  if Obj is TContainer2D then
  begin
    if fObjects = nil then
    begin
      fObjects := TGraphicObjList.Create;
      fObjects.FreeOnClear := True;
    end
    else
      fObjects.Clear;
     // Copia creando gli oggetti contenuti.
    if TContainer2D(Obj).fObjects.HasExclusiveIterators then
      raise
        ETpX_ListBlocked.Create('TContainer2D.Assign: The list has an exclusive iterator.');
     // Alloca un iterator locale
    TmpIter := TContainer2D(Obj).fObjects.GetIterator;
    try
      repeat
        TmpClass :=
          TGraphicObjectClass(TmpIter.Current.ClassType);
        TmpObj := TmpClass.Create(TmpIter.Current.ID);
        TmpObj.Assign(TmpIter.Current);
        fObjects.Add(TmpObj);
      until TmpIter.Next = nil;
    finally
      TmpIter.Free;
    end;
  end;
end;

procedure TContainer2D.UpdateSourceReferences(const BlockList:
  TGraphicObjIterator);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  try
    TmpObj := TObject2D(TmpIter.First);
    while TmpObj <> nil do
    begin
      if (TmpObj is TSourceBlock2D) then
        TSourceBlock2D(TmpObj).UpdateSourceReferences(BlockList)
      else if (TmpObj is TBlock2D) then
        TBlock2D(TmpObj).UpdateReference(BlockList);
      TmpObj := TObject2D(TmpIter.Next);
    end;
  finally // Libera l'iterator
    TmpIter.Free;
  end;
  UpdateExtension(Self);
end;

procedure TContainer2D.TransForm(const T: TTransf2D);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := fObjects.GetIterator;
  try
    TmpObj := TObject2D(TmpIter.First);
    while TmpObj <> nil do
    begin
      TmpObj.TransForm(T);
      TmpObj := TObject2D(TmpIter.Next);
    end;
  finally
    TmpIter.Free;
  end;
end;

procedure TContainer2D.DeviceDraw(Transf: TTransf2D;
  const Dvc: TDevice; const ClipRect2D: TRect2D);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := fObjects.GetIterator;
  try
    TmpObj := TObject2D(TmpIter.First);
    while TmpObj <> nil do
    begin
      TmpObj.DeviceDraw(Transf, Dvc, ClipRect2D);
      TmpObj := TObject2D(TmpIter.Next);
    end;
  finally
    TmpIter.Free;
  end;
end;

function TContainer2D.OnMe(P: TPoint2D; Aperture: TRealType;
  var Distance: TRealType): Integer;
var
  TmpObj: TObject2D;
  MinDist: TRealType;
  TmpIter: TGraphicObjIterator;
begin
  Result := inherited OnMe(P, Aperture, Distance);
  if Result = PICK_INBBOX then
  begin
     // Crea un iterator temporaneo.
    TmpIter := fObjects.GetIterator;
    try
       { Check all the objects in the container. }
      TmpObj := TObject2D(TmpIter.First);
      MinDist := 2.0 * Aperture;
      while TmpObj <> nil do
      begin
        if (TmpObj.OnMe(P, Aperture, Distance) >= PICK_INOBJECT)
          and
          (Distance < MinDist) then
        begin
          MinDist := Distance;
          Result := TmpObj.ID;
        end;
        TmpObj := TObject2D(TmpIter.Next);
      end;
      Distance := MinDist;
    finally
      TmpIter.Free;
    end;
  end;
end;

procedure TContainer2D.DrawControlPoints(const VT: TTransf2D;
  const ClipRect2D: TRect2D; const Width: Integer);
begin
  if fObjects.Count > 0 then
    inherited DrawControlPoints(VT, ClipRect2D, Width);
  _UpdateExtension; //??
  //OwnerDrawing.OnControlPoint3(BoxCenter(Box), VT, ClipRect2D);
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
  TGraphicObjIterator);
var
  TmpSource: TSourceBlock2D;
begin
  TmpSource := BlockList.First as TSourceBlock2D;
  while TmpSource <> nil do
  begin
    if TmpSource.Name = fSourceName then
    begin
      fSourceBlock := TmpSource;
      UpdateExtension(Self);
      Exit;
    end;
    TmpSource := BlockList.Next as TSourceBlock2D;
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

  TpXRegisterClass(0, TContainer2D);
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

finalization
end.

