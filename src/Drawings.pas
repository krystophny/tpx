unit Drawings;

// This unit defines classes for virtual drawings

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface

uses Types, SysUtils, Classes, Graphics, Controls,
  Contnrs, StrUtils, md5, Options0, Geometry, Pieces,
  Bitmaps,
{$IFDEF VER140}
  WinBasic,
{$ELSE}
  LMessages, LCLType, {LazBasic,}
{$ENDIF}
  Devices, GObjBase, SysBasic;

const
  Drawing_NewFileName = ': Unnamed drawing :';
{$I tpx.inc}

type
  TeXFormatKind = (tex_tex, tex_pgf, tex_pstricks, tex_eps,
    tex_png,
    tex_bmp, tex_metapost, tex_tikz, tex_emf, tex_none);
  PdfTeXFormatKind = (pdftex_tex, pdftex_pgf, pdftex_pdf,
    pdftex_png,
    pdftex_metapost, pdftex_tikz, pdftex_epstopdf, pdftex_none);
  ExportFormatKind = (export_SVG, export_EMF, export_EPS,
    export_PNG,
    export_BMP, export_PDF, export_metapost, export_mps,
    export_epstopdf, export_latexeps, export_latexpdf,
    export_latexcustom, export_latexsrc, export_pdflatexsrc);
  TeXFigureEnvKind = (fig_none, fig_figure, fig_floating,
    fig_wrap);

const
  TeXFormat_Choice =
    'tex;pgf;pstricks;eps;png;bmp;metapost;tikz;emf;none';
  PdfTeXFormat_Choice =
    'tex;pgf;pdf;png;metapost;tikz;epstopdf;none';
  ExportFormat_Choice =
    'svg;emf;eps;png;bmp;pdf;metapost;mps;epstopdf;'
    + 'latexeps;latexpdf;latexcustom;latexsrc;pdflatexsrc';
  ExportDefaultExt =
    'svg;emf;eps;png;bmp;pdf;mp;mps;pdf;eps;pdf;*;tex;tex';
  TeXFigure_Choice = 'none;figure;floatingfigure;wrapfigure';

var
  TeXFormat_Default: TeXFormatKind = tex_tikz;
  PdfTeXFormat_Default: PdfTeXFormatKind = pdftex_tikz;
  ArrowsSize_Default: TRealType = 0.7;
  StarsSize_Default: TRealType = 1;
  DefaultFontHeight_Default: TRealType = 5;
  //DefaultTeXFontHeight_Default: TRealType = 5;
  //PicWidth_Default: Integer = 75;//=140
  //PicHeight_Default: Integer = 60;//=100
  PicScale_Default: TRealType = 1;
  BitmapRes_Default: TRealType = 20000;
  HatchingStep_Default: TRealType = 2;
  HatchingLineWidth_Default: TRealType = 0.5;
  DottedSize_Default: TRealType = 0.5;
  DashSize_Default: TRealType = 1;
  FontName_Default: string = 'Times New Roman';
  TeXCenterFigure_Default: Boolean = False;
  TeXFigure_Default: TeXFigureEnvKind = fig_none;
  LineWidthBase_Default: TRealType = 0.3;
  Border_Default: TRealType = 2;
  PicMagnif_Default: TRealType = 1;
  FontSizeInTeX_Default: Boolean = True;
  MetaPostTeXText_Default: Boolean = True;
  IncludePath_Default: string = '';
  DefaultSymbolSize_Default: TRealType = 30;
  ApproximationPrecision_Default: TRealType = 0.01;
  New_LineStyle_Default: TLineStyle = liSolid;
  New_LineColor_Default: TColor = clDefault;
  New_LineWidth_Default: TRealType = 1;
  New_Hatching_Default: THatching = haNone;
  New_HatchColor_Default: TColor = clDefault;
  New_FillColor_Default: TColor = clDefault;
  New_Arr1_Default: Byte = 0;
  New_Arr2_Default: Byte = 0;
  New_ArrSizeFactor_Default: TRealType = 1;
  New_HAlignment_Default: Byte = 0;
  New_StarKind_Default: Byte = 0;
  New_StarSizeFactor_Default: TRealType = 1;

  // in=72.27pt  in=25.4mm  mm=2.845pt
  // in=72pt  in=25.4mm  mm=2.8346pt

  SmoothBezierNodes: Boolean = True;
  AreaSelectInside: Boolean = True;
  ScaleText: Boolean = True;
  RotateText: Boolean = True;
  RotateSymbols: Boolean = True;
  ScaleLineWidth: Boolean = False;
  UseSnap: Boolean = False;
  UseAngularSnap: Boolean = False;

type
{: This type defines the modes used to collect a set of objects using the
   <See Method=TViewport2D@CollectObjects> methods.

   These are the modes avaiable:

   <LI=<I=gmAllInside> all the objects wholy inside the area are collected.>
   <LI=<I=gmCrossFrame> all the objects wholy or partially inside the area are collected.>
}
  TCollectMode = (gmAllInside, gmCrossFrame);

{: This exception is raised when you try to delete an source block
   that has references (that has linked instances of <See Class=TBlock2D>).

   In the case of this error remove all the references to the
   source block before retry to delete it.
}
  ETpX_SourceBlockIsReferenced = class(ETpX_SysException);

  TDrawing = class;
  TDrawing2D = class;

  //TSY:
  TOnChangeDrawing = procedure(Drawing: TDrawing) of object;

{: This type defines the event that is fired when an object is added to
   a <See Class=TDrawing> control.

   <I=Sender> is the control in which the object is added;
   <I=Obj> is the object being added.

   It is useful when you want to set some object properties to a default
   value regardless of the procedure that added the object.
}
  TAddObjectEvent = procedure(Sender: TObject; Obj:
    TGraphicObject) of object;
{: This type defines the event that is fired when an object is loaded from
   a drawing.

   <I=Sender> is the control that is loading the file;
   <I=ReadPercent> is percentual of the drawing loaded so far.

   This event is useful when you want to inform the user of the progress
   of the loading (see <See Property=TDrawing@OnLoadProgress>).
}
  TOnLoadProgress = procedure(Sender: TObject; ReadPercent: Byte)
    of object;
{: This type defines the event that is fired when an object is saved to
   a drawing.

   <I=Sender> is the control that is loading the file;
   <I=SavePercent> is percentual of the drawing saved so far.

   This event is useful when you want to inform the user of the progress
   of the saving (see <See Property=TDrawing@OnSaveProgress>).
}
  TOnSaveProgress = procedure(Sender: TObject; SavedPercent:
    Byte) of object;

  // A basic class for TViewport visual control (see ViewPort unit)
  TBaseViewport = class(TCustomControl)
  end;

  // Types for control point drawing procedures
  TOnControlPoint = procedure(const P: TPoint2D;
    const VT: TTransf2D; const ClipRect: TRect2D) of object;
  TOnControlLine = procedure(const P0, P1: TPoint2D;
    const VT: TTransf2D; const ClipRect: TRect2D) of object;
  TOnControlBox = procedure(const R: TRect2D;
    const VT: TTransf2D; const ClipRect: TRect2D) of object;

{: This is a non visual control that is used to store and manage the list
   of graphic objects that defines a draw.

   TDrawing is an asbtract class use to define a common interface for
   drawing management. In fact the control is not useble by itself
   because it doesn't specify the kind of objects that can be stored,
   but only assest what kind of operation are necessary for storing
   general graphic objects.

   Basically this control is used to store a list of objects that must
   be drawed. Until you define a viewport from which see the objects
   these objects are only stored in a list that will be traversed to
   produce a draw as soon as you have defined such a viewport (see
   <See Class=TViewport>) by linking a viewport control to the
   TDrawing (see <See Property=TViewport@Drawing>).

   The first thing you must define is the kind of objects you want to
   use:

   <LI=If you want to create 2D drawings use the control
       <See Class=TDrawing2D> >

   these specific controls are derived from this one implementing
   the abstract method to handle 2D objects.

   This control handles also the PERSISTANCE of the draw, giving
   method to save and retrive the list of objects from a stream or
   a file (see <See=Object's Persistance@PERSISTANCE>). The use of polymorphism and
   class references made possible to add your own shape classes
   that will be considered as much the same as the native ones.

   Think about this control as a TDataSet for a Database application.

   <B=Note>: You don't need to derive from this control because the
   library has already the two controls you need to handle 2D.
}
  TDrawing = class(TComponent)
  private
    fListOfObjects, fListOfBlocks: TGraphicObjList;
      { There are two list. The one of the objects and the one of the blocks. }
    fNextID, fNextBlockID: Integer;
      { Contain the next ID to assign. }
    fListOfViewport: TList;
      { Contains all the Viewports linked. }
    fLayers: TLayers; { Layers of the component. }
    fCurrentLayer: Word;

    { event handlers. }
    //TSY:
    fOnChangeDrawing: TOnChangeDrawing;
    fOnAddObject: TAddObjectEvent;
    fOnLoadProgress: TOnLoadProgress;
    fOnSaveProgress: TOnSaveProgress;
    fSelectedObjs: TGraphicObjList;
    fSelectionFilter: TObject2DClass;

    procedure SetListOfObjects(Objects: TGraphicObjList);
    procedure SetListOfBlocks(Objects: TGraphicObjList);
    function GetObjectsCount: Integer;
    function GetSourceBlocksCount: Integer;
    function GetViewport(IDX: Integer): TBaseViewport;
    function GetViewportsCount: Integer;
  protected
    {: This method loads the blocks definitions (see <See Class=TSourceBlock2D>)
       from a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream that contains the blocks (and it must be
       positioned on the first block present).
       The blocks are created by reading the shape class registration index
       and creating the correct shape instance with the state present in the
       stream. Then this instance is saved in the list of objects of the
       source block.

       When a source block is loaded the objects that are contained in the
       source block are checked for recursive nesting of blocks. It is
       allowed to have a source block that use in its definition another
       source block, BUT this source block must be loaded before the
       source block that use it (this is automatically checked when you
       define a block). When the source block is loaded its
       <See Method=TSourceBlock2D@UpdateSourceReferences> is called to
       update the references.

       If there is a block that references to a not defined source block
       a message will be showed and the source block will not be loaded.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure LoadBlocksFromStream(const Stream: TStream); virtual;
      abstract;
    {: This method saves the blocks definitions (see <See Class=TSourceBlock2D>
       to a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream into which save the source blocks,
       <I=AsLibrary> tells if list of source blocks must be saved as
       a library. A Library will contains only the source blocks that
       have the <See Property=TGraphicObject@ToBeSaved> property set to
       <B=True> and the <See Property=TSourceBlock2D@IsLibraryBlock>
       property set to
       <B=True>. This is useful to create library of blocks definition
       to be shared beetwen drawing.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure SaveBlocksToStream(
      const Stream: TStream; const AsLibrary: Boolean);
      virtual; abstract;
    {: This method loads the objects saved in a drawing stream.

       <I=Stream> is the stream that contains the objects (and it must be
       positioned on the first object present).
       The objects are created by reading the shape class registration
       index and creating the correct shape instance with the state present
       in the stream. Then this instance is stored in the list of object of
       the control.

       When a block instance (see <See Class=TBlock2D>)
       is loaded the source block reference is updated with the current list
       of source block by calling the <See Method=TBlock2D@UpdateReference>
       method. If this source block
       is not present a message will be showed and the source object discarded.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure LoadObjectsFromStream(const Stream: TStream);
      virtual;
      abstract;
    {: This method saves the objects in the display list to a drawing stream.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream into which save the objects.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure SaveObjectsToStream(const Stream: TStream);
      virtual; abstract;
    {: This method adds a new source block.

       <I=ID> is the identifier number of the source block and Obj is the
       source block object. The library doesn't check for uniqueness of
       source block's ID. The given ID substitute the ID of Obj. A source
       block can also be referenced by its name and again the library
       doesn't check for its uniqueness.

       A source block is a collection of objects that are drawed at a whole
       on the viewport. A source block must be istantiated before the
       drawing occourse by define a block that references it. The instance
       has a position so you can share the same collection of objects and
       draw it in different places on the same drawing. This allows the
       efficent use of memory. If a source block is modified, all the
       instances are modified accordingly.

       The method returns the added block.

       See also <See Class=TSourceBlock2D>, <See Class=TBlock2D>.
    }
    function AddSourceBlock(ID: Integer; const Obj:
      TGraphicObject): TGraphicObject;
    {: This method returns the source block with the given name or
       <B=nil> if no such object is found.

       The returned reference is of type <See Class=TGraphicObject>, you must
       up-cast it to the appropriate class.

       See also <See Method=TDrawing@GetSourceBlock>.
    }
    function FindSourceBlock(const SrcName: TSourceBlockName):
      TGraphicObject;
    {: This method returns the source block with the given ID or
       <B=nil> if no such object is found.

       The returned reference is of type <See Class=TGraphicObject>, you must
       up-cast it to the appropriate class.

       See also <See Method=TDrawing@FindSourceBlock>.
    }
    function GetSourceBlock(ID: Integer): TGraphicObject;
    {: This method adds a new object.

       <I=ID> is the identifier number of the object to add and Obj is the
       object to add. The library doesn't check for uniqueness of
       object's ID. The given ID substitute the ID of Obj. The object
       will be placed on the current layer (see <See Property=TDrawing@CurrentLayer>).

       The object is added after all the objects in the control, and will be
       drawed in front of them. Use <See Method=TGraphicObjList@Move> method
       on <See Property=TDrawing@ObjectList> to change the this order; or use
       <See Method=TDrawing@InsertObject> to add the object in a specified
       position in the list.

       The method returns the added object.
    }
    function AddObject(ID: Integer; const Obj: TGraphicObject):
      TGraphicObject;
    {: This method insert a new object in a given position.

       <I=ID> is the identifier number of the object to insert and Obj is the
       object to insert. The library doesn't check for uniqueness of
       object's ID. The given ID substitute the ID of Obj. The object
       will be placed on the current layer (see <See Property=TDrawing@CurrentLayer>).
       <I=IDInsertPoint> is the object's ID of the object in front of which <I=Obj>
       will be inserted. So <I=Obj> is drawed before this object and appear under it.
       The objects are drawed in the order in which they appear in the list.

       If the object with <I=IDInsertPoint> doesn't exist in the list a
       <See Class=ETpX_ListObjNotFound> exception will be raised.

       The method returns the added object.
    }
    function InsertObject(ID, IDInsertPoint: Integer; const Obj:
      TGraphicObject): TGraphicObject;
    {: This method returns the object with the given ID or
       <B=nil> if no such object is found.

       The returned reference is of type <See Class=TGraphicObject>, you must
       up-cast it to the appropriate class before use.
    }
    function GetObject(ID: Integer): TGraphicObject;
  public
    // Procedures to show control points, lines, boxes, etc. on viewport
    OnControlPoint, OnControlPoint2, OnControlPoint3,
      OnControlPoint4:
    TOnControlPoint;
    OnControlLine: TOnControlLine;
    OnControlBox: TOnControlBox;
    {: This is the constructor of the control.

       It sets the control in the following state:

       <LI=<See property=TDrawing@CurrentLayer> set to 0>
       <LI=<See property=TDrawing@DefaultLayersColor> set to <B=clBlack> >
    }
    constructor Create(AOwner: TComponent); override;
    {: This is the destructor of the control.

       It destroy the control and all the objects and source blocks present
       in its display list. After that you are not allowed to reference any
       of its objects.
    }
    destructor Destroy; override;
    {: This property contains the list of objects of the control.

       Use this property if you want to change the display list through
       the methods of the list of graphic objects (see <See Class=TGraphicObjList>).

       This property is useful if you want to use an optimized list instead of the
       default one (it must be derived from <See Class=TGraphicObjList> class).
       In this case use this property just after the creation of the control and
       before adding objects to it.
    }
    property ObjectList: TGraphicObjList read fListOfObjects
      write SetListOfObjects;
    {: This property contains the list of source block of the control.

       Use this property if you want to change the display list through
       the methods of the list of graphic objects (see <See Class=TGraphicObjList>).

       This property is useful if you want to use an optimized list instead of the
       default one (it must be derived from <See Class=TGraphicObjList> class).
       In this case use this property just after the creation of the control and
       before adding any source blocks to it.
    }
    property BlockList: TGraphicObjList read fListOfBlocks write
      SetListOfBlocks;
    { Add the indicated Viewport. This function is automatically called by
      a Viewport so the user have no need to call it directly. }
    procedure AddViewports(const VP: TBaseViewport);
    { Delete the indicated Viewport. This function is automatically called
      by a Viewport so the user have no need to call it directly. }
    procedure DelViewports(const VP: TBaseViewport);
    //TSY:
    procedure AddList(const Lst: TGraphicObjList);
    //TSY:
    procedure InsertList(
      const IDInsertPoint: Integer; const Lst: TGraphicObjList);
    {: This method deletes the source block with the specified ID.

      <I=ID> is the identification number of the source block. If no such
      block is found a <See Class=ETpX_ListObjNotFound> exception will be raised.

      If the source block to be delete is in use (that is it is referenced
      by some <See Class=TBlock2D> object) a
      <See Class=ETpX_SourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.
    }
    procedure DeleteSourceBlockByID(const ID: Integer);
    {: This method deletes all the source block currently defined.

      If some of the source blocks is in use (that is it is referenced
      by some <See Class=TBlock2D> object) a
      <See Class=ETpX_SourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.

      This method is called when the Drawing is freed, so you may see an
      error before the application is closed that inform you that some
      source blocks are not deleted. You can simply ignore this error
      that indicate a problem in the construction of the drawing.
    }
    procedure DeleteAllSourceBlocks;
    {: This method deletes all the source blocks that were saved into
       a library stream (that is they have the property
       ToBeSaved set to true and they are not library blocks).

      Use it to delete all the source blocks that belong to a
      drawing keeping only the ones that are not saved to a stream.

      If some of the source blocks is in use (that is it is referenced
      by some <See Class=TBlock2D> object) a
      <See Class=ETpX_SourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.
    }
    procedure DeleteSavedSourceBlocks; virtual; abstract;
    {: This method deletes all the source blocks that are
       library stream (saved or not saved !).

      Use it to undload a library if you need this or before load
      another one.

      If some of the source blocks is in use (that is it is referenced
      by some <See Class=TBlock2D> object) a
      <See Class=ETpX_SourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.
    }
    procedure DeleteLibrarySourceBlocks; virtual; abstract;
    {: This method searches for an object with a given ID, and if
       found changes the layer on which it lies.

       <I=ID> is the identification number of the object;
       <I=NewLayer> is the new layer number on which the object must reside.

      If the object is not found a <See Class=ETpX_ListObjNotFound> exception
      will be raised.
    }
    procedure ChangeObjectLayer(const ID: Integer; const
      NewLayer: Byte);
    {: This method moves an object in the display list.

       <I=IDOrigin> is the identification number of the object to be moved;
       <I=IDDestination> is the identification number of the object before which
       the object will be moved.

       This method is useful to change the order of the object. After the
       moving the object with ID equal to <I=IDOrigin> will be under the
       one with ID equal to <I=IDDestination>.

      If the object is not found a <See Class=ETpX_ListObjNotFound> exception
      will be raised.
    }
    procedure MoveObject(const IDOrigin, IDDestination:
      Integer);
    {: This method deletes an object of the display list.

       <I=ID> is the identification number of the object to be deleted.

      If the object is not found a <See Class=ETpX_ListObjNotFound> exception
      will be raised.
    }
    procedure DeleteObject(const ID: Integer);
    {: This method removes all the objects of the display list.
    }
    procedure DeleteAllObjects;
    {: This method repaint all the linked viewports
       (see <See Property=TDrawing@Viewports>).

       This method is useful to regenerate all the viewports that display the
       objects.
    }
    procedure RepaintViewports; virtual;
    {: This method refresh all the linked viewports
       (see <See Property=TDrawing@Viewports>).

       This method is useful to update the viewports' images without
       redraw all the objects, but simply by copying the off-screen
       buffer on the canvases.
    }
    procedure RefreshViewports; virtual;
    function SelectionFirst: TGraphicObject;
    procedure SelectionClear;
    procedure SelectionAdd(const Obj: TGraphicObject);
    procedure SelectionAddList(const List: TGraphicObjList);
    function SelectionRemove(const Obj: TGraphicObject):
      Boolean;
    procedure SelectAll;
    procedure SelectNext(Direction: Integer);
    procedure DeleteSelected;
    function GetSelectionExtension: TRect2D;
    function GetSelectionCenter: TPoint2D;
    procedure NotifyChanged;
    property Layers: TLayers read fLayers;
    {: This property is the current layer on which any object added to the
       drawing resides.

       When you add on object to the display list of the control, its layer
       will be set to the value of this property (by default 0).
       You can modify the layer of the added object by using the reference
       to the object AFTER it is added to the control.
    }
    property CurrentLayer: Word read fCurrentLayer write
      fCurrentLayer;
    {: This property contains the number of objects present in the display list.
    }
    property ObjectsCount: Integer read GetObjectsCount;
    {: This property contains the number of source blocks present in the source blocks list.
    }
    property SourceBlocksCount: Integer read
      GetSourceBlocksCount;
    {: This property contains all the viewports that are linked to the
       control.

       A viewport is a visual components that display the world defined
       by the Drawing control. <I=Idx> is the index of the viewport from
       0 to <See Property=TDrawing@ViewportsCount> - 1.

       Use this property if you want to change some of the properties of
       the viewports that use the Drawing control or if you want to apply
       to all of them an operation.

       See also <See Class=TViewport>.
    }
    property Viewports[IDX: Integer]: TBaseViewport read
    GetViewport;
    {: This property contains the number of viewport that are linked
       to the control.

       See also <See Class=TDrawing@Viewports>.
    }
    property ViewportsCount: Integer read GetViewportsCount;
    property SelectedObjects: TGraphicObjList read
      fSelectedObjs;
    {: This property may contain a class reference type (deriving
       from <See Class=TObject2D>) used to filter the selection.

       If the picked object doesn't derive from that class, the
       object is ignored.

       By default it is <I=TObject2D>.
    }
    property SelectionFilter: TObject2DClass read
      fSelectionFilter write fSelectionFilter;
  published
    {: EVENTS}
    {: This property may contain an event handler that is
       called when an object is loaded into the control.

       See also <See Type=TOnLoadProgress>.
    }
    property OnLoadProgress: TOnLoadProgress read fOnLoadProgress
      write fOnLoadProgress;
    {: This property may contain an event handler that is
       called when an object is saved into a stream.

       See also <See Type=TOnSaveProgress>.
    }
    property OnSaveProgress: TOnSaveProgress read fOnSaveProgress
      write fOnSaveProgress;
    {: This property may contain an event handler that is
       called when an object is added to the control.

       See also <See Type=TAddObjectEvent>.
    }

    //TSY:
    property OnChangeDrawing: TOnChangeDrawing read
      fOnChangeDrawing write fOnChangeDrawing;

    property OnAddObject: TAddObjectEvent read fOnAddObject write
      fOnAddObject;
  end;

{$IFDEF VER140}
  TCheckSum = MD5Digest;
{$ELSE}
  TCheckSum = TMD5Digest;
{$ENDIF}

  TDrawHistory = class(TObjectList)
  private
    fDrawing: TDrawing2D;
    fPosition: Integer;
    fCheckSum, fSavedCheckSum: TCheckSum;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function GetIsChanged: Boolean;
    procedure RestorePosition(Pos: Integer);
  public
    constructor Create(ADrawing: TDrawing2D);
    procedure Truncate(Index: Integer);
    procedure Save;
    procedure Undo;
    procedure Redo;
    procedure Clear; override;
    procedure SaveCheckSum;
    procedure SetPropertiesChanged;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property IsChanged: Boolean read GetIsChanged;
  end;

  TOnPasteMetafileFromClipboard = procedure(Drawing: TDrawing2D) of
    object;

  {: This class defines a specialization of a <See Class=TDrawing>
     control (see it for details).

     This Drawing handles 2D objects and source blocks.
     This component must be used when you need to store 2D drawing.
  }
  TDrawing2D = class(TDrawing)
  private
    fArrowsSize: TRealType;
    fStarsSize: TRealType;
    fFileName: string;
    function GetExtension: TRect2D;
    procedure SetFileName(const NewFileName: string);
  public
    TeXFormat: TeXFormatKind;
    PdfTeXFormat: PdfTeXFormatKind;
    DefaultFontHeight: TRealType;
    Caption: string;
    FigLabel: string;
    Comment: string;
    {PicHeight: Integer;
    PicWidth: Integer;}
    PicScale: TRealType;
    BitmapRes: TRealType;
    HatchingStep: TRealType;
    HatchingLineWidth: TRealType;
    DottedSize: TRealType;
    DashSize: TRealType;
    FontName: string;
    TeXCenterFigure: Boolean;
    TeXFigure: TeXFigureEnvKind;
    TeXFigurePlacement: string;
    TeXFigurePrologue: string;
    TeXFigureEpilogue: string;
    TeXPicPrologue: string;
    TeXPicEpilogue: string;
    LineWidthBase: TRealType;
    MiterLimit: TRealType;
    //FactorMM: TRealType; // for line width
    Border: TRealType;
    PicMagnif: TRealType;
    FontSizeInTeX: Boolean;
    MetaPostTeXText: Boolean;
    IncludePath: string;
    DefaultSymbolSize: TRealType;
    ApproximationPrecision: TRealType;
    New_LineStyle: TLineStyle;
    New_LineColor: TColor;
    New_LineWidth: TRealType;
    New_Hatching: THatching;
    New_HatchColor: TColor;
    New_FillColor: TColor;
    New_Arr1: Byte;
    New_Arr2: Byte;
    New_ArrSizeFactor: TRealType;
    New_FontHeight: TRealType;
    New_HAlignment: Byte;
    New_StarKind: Byte;
    New_StarSizeFactor: TRealType;
    History: TDrawHistory;
    OptionsList: TOptionsList;
    OnPasteMetafileFromClipboard: TOnPasteMetafileFromClipboard;
    BitmapRegistry: TStringList;
    procedure LoadObjectsFromStream(const Stream: TStream);
      override;
    procedure SaveObjectsToStream0(const Stream: TStream;
      const ObjList: TGraphicObjList);
    procedure SaveObjectsToStream(const Stream: TStream);
      override;
    procedure SaveSelectionToStream(const Stream: TStream);
    procedure CopySelectionToClipboard;
    procedure PasteFromClipboard;
    procedure FillOptionsList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDefaults; virtual;
    procedure SetDefaultProperties;
    procedure Clear; virtual;
    // Fills all object pieces, updates drawing extension and repaints viewports
    procedure Update; virtual;
    {: This method loads the blocks definitions (see <See Class=TSourceBlock2D>) from a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream that contains the blocks (and it must be
       positioned on the first block present).
       The blocks are created by reading the shape class registration index
       and creating the correct shape instance with the state present in the
       stream. Then this instance is saved in the list of objects of the
       source block.

       When a source block is loaded the objects that are contained in the
       source block are checked for recursive nesting of blocks. It is
       allowed to have a source block that use in its definition another
       source block, BUT this source block must be loaded before the
       source block that use it (this is automatically checked when you
       define a block). When the source block is loaded its
       <See Method=TSourceBlock2D@UpdateSourceReferences> is called to
       update the references.

       If there is a block that references to a not defined source block
       a message will be showed and the source block will not be loaded.

       See also <See=oObject's Persistance@PERSISTANCE>.
    }
    procedure LoadBlocksFromStream(const Stream: TStream);
      override;
    {: This method saves the blocks definitions (see <See Class=TSourceBlock2D>) to a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream into which save the source blocks,
       <I=AsLibrary> tells if list of source blocks must be saved as
       a library. A Library will contains only the source blocks that
       have the <See Property=TGraphicObject@ToBeSaved> property set to
       <B=True> and the <See Property=TSourceBlock2D@IsLibraryBlock>
       property set to <B=True>. This is useful to create library of
       blocks definition to be shared beetwen drawing.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure SaveBlocksToStream(
      const Stream: TStream; const AsLibrary: Boolean); override;
    {: This method adds a new source block.

       <I=ID> is the identifier number of the source block and Obj is the
       source block object. The library doesn't check for uniqueness of
       source block's ID. The given ID substitute the ID of Obj. A source
       block can also be referenced by its name and again the library
       doesn't check for its uniqueness.

       A source block is a collection of objects that are drawed at a whole
       on the viewport. A source block must be istantiated before the
       drawing occourse by define a block that references it. The instance
       has a position so you can share the same collection of objects and
       draw it in different places on the same drawing. This allows the
       efficent use of memory. If a source block is modified, all the
       instances are modified accordingly.

       The method returns the added block.

       See also <See Class=TSourceBlock2D> and
       <See Class=TBlock2D>.
    }
    function AddSourceBlock(const Obj: TSourceBlock2D):
      TSourceBlock2D;
    {: This method deletes the source block with the specified Name.

      <I=ID> is the identification number of the source block. If no such
      block is found a <See Class=ETpX_ListObjNotFound> exception will be raised.

      If the source block to be delete is in use (that is it is referenced
      by some <See Class=TBlock2D> object) a
      <See Class=ETpX_SourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.
    }
    procedure DeleteSourceBlock(const SrcName:
      TSourceBlockName);
    function GetSourceBlock(const ID: Integer): TSourceBlock2D;
    function FindSourceBlock(const SrcName: TSourceBlockName):
      TSourceBlock2D;
    {: This method creates a source block (and add it to the drawing) by
       grouping a list of objects.
       ScrName will be the name of the source block; Objects is
       the list that contains the objects to be added.
       That list must have the FreeOnClear property set to False.
    }
    function BlockObjects(const SrcName: TSourceBlockName;
      const Objects: TGraphicObjList): TSourceBlock2D;
    procedure DeleteLibrarySourceBlocks; override;
    procedure DeleteSavedSourceBlocks; override;
    function AddObject(const ID: Integer; const Obj: TObject2D):
      TObject2D;
    function InsertObject(const ID, IDInsertPoint: Integer; const
      Obj: TObject2D): TObject2D;
    {: This method adds a new block (instance of a source block) into
       the drawing.

       <I=ID> will be the identifier of the new block,
       and <I=SrcName> is the name of the source block used to
       define the block.

       If the source block isn't in the drawing an exception will be
       raised. The method returns the reference of the added block.
    }
    function AddBlock(const ID: Integer; const SrcName:
      TSourceBlockName): TObject2D;
    function GetObject(const ID: Integer): TObject2D;
    procedure CollectObjects(const ResultLst: TGraphicObjList;
      Frm: TRect2D; const VisualRect: TRect2D;
      const Mode: TCollectMode; const PickFilter: TObject2DClass;
      const RemoveFromDrawing: Boolean);
    function PickObject(const P: TPoint2D;
      const Aperture: TRealType;
      const VisualRect: TRect2D; const PickFilter: TObject2DClass;
      const FirstFound: Boolean; var NPoint: Integer): TObject2D;
    function PickObject_PreferSelected(const P: TPoint2D;
      const Aperture: TRealType;
      const VisualRect: TRect2D; const PickFilter: TObject2DClass;
      const FirstFound: Boolean; var NPoint: Integer): TObject2D;
    {: This method fills a list with the object that are at a distance
       from <I=Pt> less than <I=Aperture>.

       The method perform the same operation of <See Method=PickObject>
       method but the objects that are picked by the point <I=Pt> are all
       added to the list <I=PickedObjects> in the same order as they
       are encountered.
    }
    function PickListOfObjects(
      const PickedObjects: TList; P: TPoint2D;
      const VisualRect: TRect2D; const PickFilter: TObject2DClass;
      Aperture: Word): Integer;
    procedure ClearBitmapRegistry;
    function RegisterBitmap(ImageLink: string): TBitmapEntry;
    procedure PickUpProperties(Obj: TGraphicObject);
    procedure ApplyProperties(Obj: TGraphicObject);
    {: This property contains the extension of the drawing.

       The drawing extension is the smallest axis aligned
       rectangle that bounds all the objects in the drawing.
       When you refer to the property the extension rectangle is
       computed and this may require a bit of time for a complex draw.

       If you want to use this value more than once in a series of
       computations, it is better to store it in a local variable
       instead of use this property in all the computations.
    }
    property DrawingExtension: TRect2D read GetExtension;
    property ArrowsSize: TRealType read fArrowsSize write
      fArrowsSize;
    property StarsSize: TRealType read fStarsSize write
      fStarsSize;
    property FileName: string read fFileName write SetFileName;
  end;

function GetExtension0(Drawing2D: TDrawing2D;
  Objects: TGraphicObjList): TRect2D;

implementation

uses GObjects,
{$IFDEF VER140}
  pngimage,
{$ENDIF}
  ClpbrdOp, ColorEtc, ViewPort;


function MD5Stream(const AStream: TStream): TCheckSum;
var
{$IFDEF VER140}
  Context: MD5Context;
{$ELSE}
  Context: TMD5Context;
{$ENDIF}
  Buffer: array[0..4095] of Byte;
  Len: Integer;
begin
  MD5Init(Context);
  AStream.Seek(0, soFromBeginning);
  repeat
    Len := AStream.Read(Buffer, 4096);
{$IFDEF VER140}
    if Len > 0 then MD5Update(Context, @Buffer, Len);
{$ELSE}
    if Len > 0 then MD5Update(Context, Buffer, Len);
{$ENDIF}
  until Len = 0;
  MD5Final(Context, Result);
end;

procedure MD5Stream_try(const St: string);
var
  AStream: TStringStream;
  CheckSum: TCheckSum;
begin
  MessageBoxInfo(MD5Print(MD5String(St)));
  //Exit;
  AStream := TStringStream.Create(St);
  CheckSum := MD5Stream(AStream);
  AStream.Free;
  MessageBoxError(MD5Print(CheckSum));
end;

// =====================================================================
// TDrawing
// =====================================================================

function TDrawing.GetObjectsCount: Integer;
begin
  Result := fListOfObjects.Count;
end;

function TDrawing.GetSourceBlocksCount: Integer;
begin
  Result := fListOfBlocks.Count;
end;

procedure TDrawing.SetListOfObjects(Objects: TGraphicObjList);
begin
  if not Assigned(Objects) then Exit;
  DeleteAllObjects;
  fListOfObjects.Free;
  fListOfObjects := Objects;
end;

procedure TDrawing.SetListOfBlocks(Objects: TGraphicObjList);
begin
  if not Assigned(Objects) then Exit;
  DeleteAllSourceBlocks;
  fListOfBlocks.Free;
  fListOfBlocks := Objects;
end;

function TDrawing.GetViewport(IDX: Integer): TBaseViewport;
begin
  Result := TViewport(fListOfViewport[IDX]);
end;

function TDrawing.GetViewportsCount: Integer;
begin
  Result := fListOfViewport.Count;
end;

constructor TDrawing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fListOfObjects := TGraphicObjList.Create;
  fListOfObjects.FreeOnDelete := True;
  fListOfBlocks := TGraphicObjList.Create;
  fListOfBlocks.FreeOnDelete := True;

  fListOfViewport := TList.Create;
  fLayers := TLayers.Create;
  fCurrentLayer := 0;
  fNextID := 0;
  fNextBlockID := 0;
  fSelectedObjs := TGraphicObjList.Create;
  fSelectedObjs.FreeOnDelete := False;
  fSelectionFilter := TObject2D;
end;

// Used because BlockList can contain circular references.

procedure TDrawing.DeleteAllSourceBlocks;
var
  Obj: TGraphicObject;
  Deleted: Integer;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  fListOfBlocks.Lock;
  OnChangeDrawing0 := fOnChangeDrawing;
  fOnChangeDrawing := nil;
  try
    repeat
      Deleted := 0;
      Obj := fListOfBlocks.FirstObj;
      while Assigned(Obj) do
      begin
        try
          fListOfBlocks.DeleteCurrent;
          Inc(Deleted);
          Obj := fListOfBlocks.CurrentObj;
        except
          Obj := fListOfBlocks.NextObj;
        end;
      end;
      if (Deleted = 0) and (fListOfBlocks.Count > 0) then
      begin
        raise
          ETpX_SourceBlockIsReferenced.Create('TDrawing.FreeSourceBlocks: TpX Severe error'#10#13'The drawing contains circular reference of source blocks. They will be not deleted !');
        Exit;
      end;
    until fListOfBlocks.Count = 0;
  finally
    fOnChangeDrawing := OnChangeDrawing0;
    fListOfBlocks.Unlock;
  end;
  NotifyChanged;
end;

procedure TDrawing.DeleteAllObjects;
var
  OnChangeDrawing0: TOnChangeDrawing;
begin
  SelectionClear;
  OnChangeDrawing0 := fOnChangeDrawing;
  fOnChangeDrawing := nil;
  try
    try
      fListOfObjects.Clear;
    except
    end;
  finally
    fOnChangeDrawing := OnChangeDrawing0;
  end;
  NotifyChanged;
  fNextID := 0;
end;

destructor TDrawing.Destroy;
var
  Cont: Integer;
begin
  fOnChangeDrawing := nil;
  DeleteAllObjects;
  fListOfObjects.Free;
  DeleteAllSourceBlocks;
  fListOfBlocks.Free;
  for Cont := 0 to fListOfViewport.Count - 1 do
    TViewport(fListOfViewport[Cont]).Drawing := nil;
  fListOfViewport.Free;
  fLayers.Free;
  fSelectedObjs.Free;
  inherited Destroy;
end;

function TDrawing.AddSourceBlock(ID: Integer; const Obj:
  TGraphicObject): TGraphicObject;
begin
  Result := Obj;
  if ID < 0 then
  begin
    ID := fNextBlockID;
    Inc(fNextBlockID);
  end
  else if ID >= fNextBlockID then
    fNextBlockID := ID + 1;
  Obj.OwnerDrawing := Self;
  Obj.Layer := fCurrentLayer;
  Obj.ID := ID;
  Obj.UpdateExtension(Self);
  fListOfBlocks.Add(Obj);
end;

function TDrawing.GetSourceBlock(ID: Integer): TGraphicObject;
begin
  Result := fListOfBlocks.Find(ID);
end;

function TDrawing.FindSourceBlock(const SrcName:
  TSourceBlockName): TGraphicObject;
begin
  Result := nil;
end;

procedure TDrawing.DeleteSourceBlockByID(const ID: Integer);
var
  Obj: TGraphicObject;
begin
  Obj := GetSourceBlock(ID);
  if Obj = nil then
    raise
      ETpX_ListObjNotFound.Create('TDrawing.DeleteSourceBlock: No source block found');
  try
    fListOfBlocks.Delete(ID);
  except
  end;
end;

function TDrawing.AddObject(ID: Integer; const Obj:
  TGraphicObject): TGraphicObject;
begin
  Result := Obj;
  if ID < 0 then
  begin
    ID := fNextID;
    Inc(fNextID);
  end
  else if ID >= fNextID then
    fNextID := ID + 1;
  Obj.OwnerDrawing := Self;
  Obj.Layer := fCurrentLayer;
  Obj.ID := ID;
  Obj.UpdateExtension(Self);
  fListOfObjects.Add(Obj);
  if Assigned(fOnAddObject) then
    fOnAddObject(Self, Obj);
  NotifyChanged;
end;

procedure TDrawing.AddList(const Lst: TGraphicObjList);
begin
  InsertList(-1, Lst);
end;

procedure TDrawing.InsertList(
  const IDInsertPoint: Integer; const Lst: TGraphicObjList);
var
  OnChangeDrawing0: TOnChangeDrawing;
  procedure ProcessObjects(const ALst: TGraphicObjList);
  var
    Obj: TGraphicObject;
    I: Integer;
  begin
    ALst.Lock;
    I := 0;
    try
      Obj := ALst.FirstObj;
      while Assigned(Obj) do
      begin
        if Obj is TGroup2D then
          ProcessObjects((Obj as TGroup2D).Objects);
        Obj.OwnerDrawing := Self;
        if ALst = Lst then
        begin
          Obj.Layer := fCurrentLayer;
          Obj.ID := fNextID;
          Inc(fNextID);
        end;
        Obj.UpdateExtension(Self);
        if ALst = Lst then
          if Assigned(fOnAddObject) then
            fOnAddObject(Self, Obj);
        Inc(I);
        if I mod 100 = 0 then ShowProgress(I / Lst.Count);
        Obj := ALst.NextObj;
      end;
    finally
      ALst.Unlock;
    end;
  end;
begin
  OnChangeDrawing0 := fOnChangeDrawing;
  fOnChangeDrawing := nil;
  try
    if IDInsertPoint = -1 then
      fListOfObjects.AddFromList(Lst)
    else
      fListOfObjects.InsertFromList(IDInsertPoint, Lst);
    ProcessObjects(Lst);
  finally
    fOnChangeDrawing := OnChangeDrawing0;
  end;
  NotifyChanged;
end;

function TDrawing.InsertObject(ID, IDInsertPoint: Integer; const
  Obj: TGraphicObject): TGraphicObject;
begin
  Result := Obj;
  fListOfObjects.Insert(IDInsertPoint, Obj);
  if ID < 0 then
  begin
    ID := fNextID;
    Inc(fNextID);
  end
  else if ID > fNextID then
    fNextID := ID;
  Obj.OwnerDrawing := Self;
  Obj.Layer := fCurrentLayer;
  Obj.ID := ID;
  Obj.UpdateExtension(Self);
  if Assigned(fOnAddObject) then
    fOnAddObject(Self, Obj);
  NotifyChanged;
end;

procedure TDrawing.MoveObject(const IDOrigin, IDDestination:
  Integer);
begin
  fListOfObjects.Move(IDOrigin, IDDestination);
  NotifyChanged;
end;

procedure TDrawing.DeleteObject(const ID: Integer);
var
  Obj: TGraphicObject;
begin
  Obj := GetObject(ID);
  if Obj = nil then
    raise
      ETpX_ListObjNotFound.Create('TDrawing.DeleteObject: No object found');
  fListOfObjects.Delete(ID);
  NotifyChanged;
end;

procedure TDrawing.ChangeObjectLayer(const ID: Integer; const
  NewLayer: Byte);
var
  Obj: TGraphicObject;
begin
  Obj := GetObject(ID);
  if Obj = nil then
    raise
      ETpX_ListObjNotFound.Create('TDrawing.ChangeObjectLayer: No object found');
  Obj.Layer := NewLayer;
end;

function TDrawing.GetObject(ID: Integer): TGraphicObject;
begin
  Result := fListOfObjects.Find(ID);
end;

procedure TDrawing.AddViewports(const VP: TBaseViewport);
begin
  if not Assigned(VP) then Exit;
  fListOfViewport.Add(VP);
end;

procedure TDrawing.DelViewports(const VP: TBaseViewport);
var
  Cont: Integer;
begin
  if not Assigned(VP) then Exit;
  Cont := fListOfViewport.IndexOf(VP);
  if Cont >= 0 then fListOfViewport.Delete(Cont);
end;

procedure TDrawing.RepaintViewports;
var
  Cont: Integer;
begin
  for Cont := 0 to fListOfViewport.Count - 1 do
    TViewport(fListOfViewport.Items[Cont]).Repaint;
end;

procedure TDrawing.RefreshViewports;
var
  Cont: Integer;
begin
  for Cont := 0 to fListOfViewport.Count - 1 do
    TViewport(fListOfViewport.Items[Cont]).Refresh;
end;

function TDrawing.SelectionFirst: TGraphicObject;
begin
  Result := SelectedObjects.FirstObj;
end;

procedure TDrawing.SelectionClear;
var
  Obj: TGraphicObject;
begin
  fSelectedObjs.Lock;
  try
    Obj := fSelectedObjs.FirstObj;
    while Assigned(Obj) do
    begin
      if Obj is TObject2D then
        (Obj as TObject2D).HasControlPoints := False;
      Obj := fSelectedObjs.NextObj;
    end;
  finally
    fSelectedObjs.Unlock;
  end;
  fSelectedObjs.Clear;
end;

procedure TDrawing.SelectionAdd(const Obj:
  TGraphicObject);
begin
  if Obj is TObject2D then
    (Obj as TObject2D).HasControlPoints := True;
  fSelectedObjs.Add(Obj);
end;

procedure TDrawing.SelectionAddList(const List:
  TGraphicObjList);
var
  Obj: TGraphicObject;
begin
  List.Lock;
  try
    Obj := List.FirstObj;
    while Assigned(Obj) do
    begin
      if fSelectedObjs.Find(Obj.ID) = nil then
      begin
        fSelectedObjs.Add(Obj);
        if Obj is TObject2D then
          (Obj as TObject2D).HasControlPoints := True;
      end;
      Obj := List.NextObj;
    end;
  finally
    List.Unlock;
  end;
end;

function TDrawing.SelectionRemove(const Obj:
  TGraphicObject): Boolean;
begin
  Result := False;
  if not Assigned(Obj) then Exit;
  if fSelectedObjs.Find(Obj.ID) <> nil then
  begin
    if Obj is TObject2D then
      (Obj as TObject2D).HasControlPoints := False;
    fSelectedObjs.RemoveCurrent;
    Result := True;
  end;
end;

procedure TDrawing.SelectAll;
var
  Obj: TGraphicObject;
begin
  SelectionClear;
  fListOfObjects.Lock;
  try
    Obj := fListOfObjects.FirstObj;
    while Assigned(Obj) do
    begin
      fSelectedObjs.Add(Obj);
      if Obj is TObject2D then
        (Obj as TObject2D).HasControlPoints := True;
      Obj := fListOfObjects.NextObj;
    end;
  finally
    fListOfObjects.Unlock;
  end;
end;

procedure TDrawing.SelectNext(Direction: Integer);
var
  Obj: TGraphicObject;
begin
  if Direction > 0 then
    Obj := fSelectedObjs.LastObj
  else
    Obj := fSelectedObjs.FirstObj;
  if Obj = nil then
    Obj := fListOfObjects.FirstObj
  else
  begin
    fListOfObjects.FindObjByID(Obj.ID);
    if Direction > 0 then
    begin
      Obj := fListOfObjects.NextObj;
      if Obj = nil then
        Obj := fListOfObjects.FirstObj;
    end
    else
    begin
      Obj := fListOfObjects.PrevObj;
      if Obj = nil then
        Obj := fListOfObjects.LastObj;
    end
  end;
  if Assigned(Obj) then
  begin
    SelectionClear;
    SelectionAdd(Obj);
  end;
end;

procedure TDrawing.DeleteSelected;
var
  Obj: TGraphicObject;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  OnChangeDrawing0 := fOnChangeDrawing;
  fOnChangeDrawing := nil;
  fSelectedObjs.Lock;
  try
    Obj := fSelectedObjs.FirstObj;
    while Assigned(Obj) do
    begin
      DeleteObject(Obj.ID);
      Obj := fSelectedObjs.NextObj;
    end;
  finally
    fSelectedObjs.Unlock;
    fOnChangeDrawing := OnChangeDrawing0;
  end;
  fSelectedObjs.Clear;
  NotifyChanged;
end;

function TDrawing.GetSelectionExtension: TRect2D;
begin
  if not (Self is TDrawing2D)
    or (fSelectedObjs.Count = 0) then
  begin
    if ViewportsCount > 0 then
      Result := (Viewports[0] as TViewport).VisualRect
      {TransformRect2D((Viewports[0] as TViewPort).VisualRect,
        (Viewports[0] as TViewPort).ViewportToScreenTransform)}
    else
      Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  Result := GetExtension0(Self as TDrawing2D, fSelectedObjs);
end;

function TDrawing.GetSelectionCenter: TPoint2D;
var
  E: TRect2D;
begin
  E := GetSelectionExtension;
  Result := MidPoint(E.FirstEdge, E.SecondEdge);
end;

procedure TDrawing.NotifyChanged;
begin
  if Assigned(fOnChangeDrawing) then
    fOnChangeDrawing(Self);
end;

// =====================================================================
// TDrawHistory
// =====================================================================

function TDrawHistory.GetCanUndo: Boolean;
begin
  Result := fPosition > 0;
end;

function TDrawHistory.GetCanRedo: Boolean;
begin
  Result := fPosition < Count - 1;
end;

function TDrawHistory.GetIsChanged: Boolean;
begin
  Result := not MD5Match(fCheckSum, fSavedCheckSum);
end;

constructor TDrawHistory.Create(ADrawing: TDrawing2D);
begin
  inherited Create;
  fDrawing := ADrawing;
  OwnsObjects := True;
  fPosition := 0;
  fCheckSum := MD5String('');
  fSavedCheckSum := fCheckSum;
end;

procedure TDrawHistory.Truncate(Index: Integer);
var
  I: Integer;
begin
  if Index < 0 then
  begin
    Clear;
    Exit;
  end;
  if (Count < 1) then Exit;
  for I := Count - 1 downto Index do
    Delete(I);
end;

procedure TDrawHistory.Save;
var
  AStream: TMemoryStream;
begin
  Truncate(fPosition + 1);
  AStream := TMemoryStream.Create;
  Add(AStream);
  fPosition := Count - 1;
  fDrawing.SaveObjectsToStream(AStream);
  fDrawing.OptionsList.SaveToStream(AStream);
  fCheckSum := MD5Stream(AStream);
end;

procedure TDrawHistory.RestorePosition(Pos: Integer);
var
  AStream: TMemoryStream;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  OnChangeDrawing0 := fDrawing.OnChangeDrawing;
  fDrawing.OnChangeDrawing := nil;
  try
    fDrawing.DeleteAllObjects;
    fDrawing.DeleteSavedSourceBlocks;
    AStream := Items[Pos] as TMemoryStream;
    AStream.Position := 0;
    fDrawing.LoadObjectsFromStream(AStream);
    fDrawing.OptionsList.LoadFromStream(AStream);
    fCheckSum := MD5Stream(AStream);
    fDrawing.SelectionClear;
  finally
    fDrawing.OnChangeDrawing := OnChangeDrawing0;
  end;
end;

procedure TDrawHistory.Undo;
begin
  if not GetCanUndo then Exit;
  Dec(fPosition);
  RestorePosition(fPosition);
end;

procedure TDrawHistory.Redo;
begin
  if not GetCanRedo then Exit;
  Inc(fPosition);
  RestorePosition(fPosition);
end;

procedure TDrawHistory.Clear;
begin
  inherited Clear;
  fPosition := 0;
  fCheckSum := MD5String('');
  fSavedCheckSum := fCheckSum;
end;

procedure TDrawHistory.SaveCheckSum;
begin
  fSavedCheckSum := fCheckSum;
end;

procedure TDrawHistory.SetPropertiesChanged;
begin
  Save;
  //fOptCheckSum := fDrawing.OptionsList.GetCheckSum;
end;

// =====================================================================
// TDrawing2D
// =====================================================================

constructor TDrawing2D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDefaults;
  History := nil;
  OnPasteMetafileFromClipboard := nil;
  OptionsList := TOptionsList.Create;
  BitmapRegistry := TStringList.Create;
  FillOptionsList;
end;

procedure TDrawing2D.FillOptionsList;
var
  Strings: TStringList;
begin
  OptionsList.AddString('Caption', @(Caption),
    'Text for LaTeX \caption');
  OptionsList.AddString('Comment', @(Comment),
    'Information about the picture');
  OptionsList.AddString('Label', @(FigLabel),
    'Id for LaTeX \label');
  OptionsList.AddRealType('PicScale', @PicScale,
    'Picture scale (mm per unit),'
    + ' physical size of the logical unit representing picture coordinates.'
    + ' (This unit is referred to as "sp" in hints for other picture properties).'
    + ' Use PicScale to scale picture without scaling'
    + ' line widths and other dimensions set in physical units.');
  OptionsList.AddRealType('Border', @Border,
    'Picture border (mm)');
  OptionsList.AddChoice('TeXFormat',
    @TeXFormat, TeXFormat_Choice,
    'Format for including picture in TeX'
    + ' (' + AnsiReplaceStr(TeXFormat_Choice, ';', ', ') + ')');
  OptionsList.AddChoice('PdfTeXFormat',
    @PdfTeXFormat, PdfTeXFormat_Choice,
    'Format for including picture in PdfTeX'
    + ' (' + AnsiReplaceStr(PdfTeXFormat_Choice, ';', ', ') + ')');
  OptionsList.AddRealType('BitmapRes',
    @BitmapRes, 'Bitmap resolution in pixels per meter.' +
    ' Use BitmapRes to set resolution of exported PNG and BMP images).' +
    ' Conversion between pixels per meter (PPM) and pixels per inch (PPI):' +
    EOL + '100 PPI = 3937 PPM' +
    EOL + '300 PPI = 11811 PPM' +
    EOL + '600 PPI = 23622 PPM');
  OptionsList.AddRealType('PicMagnif',
    @PicMagnif,
    'Picture physical size magnification factor.' +
    ' Use PicMagnif to change the meaning of mm for quick rescaling of the picture');
  OptionsList.AddString('IncludePath',
    @(IncludePath),
    'Path to add before \includegraphics file name (like mypictures/)');
  OptionsList.AddRealType('LineWidth', @LineWidthBase,
    'Basic line width (mm). Line widths for the drawing are set as fractions of this quantity');
  OptionsList.AddRealType('ArrowsSize',
    @(ArrowsSize), 'Arrows size (sp)');
  OptionsList.AddRealType('StarsSize',
    @(StarsSize), 'Stars size (sp)');
  OptionsList.AddRealType('HatchingStep',
    @HatchingStep, 'Hatching step (mm)');
  OptionsList.AddRealType('HatchingLineWidth',
    @HatchingLineWidth,
    'Hatching line width (fraction of LineWidth)');
  OptionsList.AddRealType('DottedSize', @DottedSize,
    'Dotted line step size (mm)');
  OptionsList.AddRealType('DashSize', @DashSize,
    'Dashed line step size (mm)');
  OptionsList.AddRealType('DefaultFontHeight',
    @(DefaultFontHeight), 'Default font height (sp)');
  OptionsList.AddFontName('FontName', @(FontName),
    'Font for text labels.' +
    ' Leave FontName empty to use FontName_Default');
  OptionsList.AddRealType('DefaultSymbolSize',
    @DefaultSymbolSize,
    'Default symbol size factor ("diameter", sp)');
  OptionsList.AddRealType('ApproximationPrecision',
    @ApproximationPrecision,
    'Precision of various approximations like linearization of Bezier curves (mm)');
  OptionsList.AddRealType('MiterLimit',
    @(MiterLimit),
    'Miter limit. Used to cut off too long spike miter join'
    + ' could have when the angle between two lines is sharp. If the ratio of miter length'
    + ' (distance between the outer corner and the inner corner of the miter) to'
    + ' line width is greater than miter limit, then bevel join'
    + ' is used instead of miter join. Default value of miter limit is 10.'
    +
    ' This option is not applicable to TeX-picture and PsTricks formats.');
  OptionsList.AddBoolean('TeXCenterFigure',
    @TeXCenterFigure,
    'Center TeX figure by adding \centering before picture/includegraphics');
  OptionsList.AddChoice('TeXFigure',
    @TeXFigure, TeXFigure_Choice,
    'TeX figure environment:' + EOL +
    'none - no figure, ' + EOL +
    'figure - standard {figure} environment, ' + EOL +
    'floatingfigure - {floatingfigure} from floatflt package, '
    + EOL +
    'wrapfigure - {wrapfigure} from wrapfig package');
  OptionsList.AddString('TeXFigurePlacement',
    @(TeXFigurePlacement),
    'The optional argument [placement] determines where LaTeX will try to place your figure. There are four places where LaTeX can possibly put a float:' + EOL
    + 'h (Here) - at the position in the text where the figure environment appears'
    + EOL
    + 't (Top) - at the top of a text page' + EOL
    + 'b (Bottom) - at the bottom of a text page' + EOL +
    'p (Page of floats) - on a separate float page, which is a page containing no text, only floats'
    + EOL +
    'Putting ! as the first argument in the square brackets will encourage LATEX to do what you say, even if the result''s sub-optimal.'
    + EOL +
    ' Example: htbp' + EOL + EOL +
    'For wrapfigure placement is one of  r, l, i, o, R, L, I, O,  for right, left,  inside, outside, (here / FLOAT)'
    + EOL + EOL +
    'The floatingfigure placement option may be either one of the following: r, l, p, or v. The options all overrule any present package option which may be in effect.  The options have the following functions:'
    + EOL +
    'r  Forces the current floating figure to be typeset to the right in a paragraph'
    + EOL +
    'l Forces the current floating figure to be typeset to the left in a paragraph'
    + EOL +
    'p Forces the current floating figure to be typeset to the right in a paragraph if the page number is odd, and to the left if even'
    + EOL +
    'v Applies the package option to the current figure, and if no package option is specified, it forces the current floating figure to be typeset to the right in a paragraph if the page number is odd, and to the left if even'
    );
  OptionsList.AddString('TeXFigurePrologue',
    @(TeXFigurePrologue), 'Text to put before float');
  OptionsList.AddString('TeXFigureEpilogue',
    @(TeXFigureEpilogue), 'Text to put after float');
  OptionsList.AddString('TeXPicPrologue',
    @(TeXPicPrologue),
    'Text to put before picture/includegraphics');
  OptionsList.AddString('TeXPicEpilogue',
    @(TeXPicEpilogue),
    'Text to put after picture/includegraphics');
  OptionsList.AddBoolean('FontSizeInTeX', @(FontSizeInTeX),
    'Put font size information into LaTeX/MetaPost output code.' +
    ' Set FontSizeInTeX to 0 to use the default font size of the parent LaTeX document' +
    ' (or default font size of MetaPost drawing)');
  OptionsList.AddBoolean('MetaPostTeXText',
    @(MetaPostTeXText), 'Use TeX text in MetaPost files');
  if not OutHelp then Exit;
  // Save drawing options hints for help
  Strings := TStringList.Create;
  try
    Strings.Text := OptionsList.HintsText;
    Strings.SaveToFile(ExtractFilePath(ParamStr(0))
      + '\Help\picture_properties.inc');
  finally
    Strings.Free;
  end;
end;

destructor TDrawing2D.Destroy;
begin
  History.Free;
  OptionsList.Free;
  ClearBitmapRegistry;
  BitmapRegistry.Free;
  inherited Destroy;
end;

procedure TDrawing2D.SetDefaults;
begin
  fFileName := Drawing_NewFileName;
  TeXFormat := TeXFormat_Default;
  PdfTeXFormat := PdfTeXFormat_Default;
  fArrowsSize := ArrowsSize_Default;
  fStarsSize := StarsSize_Default;
  DefaultFontHeight := DefaultFontHeight_Default;
  Caption := '';
  FigLabel := '';
  Comment := '';
  PicScale := PicScale_Default;
  Border := Border_Default;
  BitmapRes := BitmapRes_Default;
  PicMagnif := PicMagnif_Default;
  HatchingStep := HatchingStep_Default;
  HatchingLineWidth := HatchingLineWidth_Default;
  DottedSize := DottedSize_Default;
  DashSize := DashSize_Default;
  FontName := ''; //FontName_Default;
  TeXCenterFigure := TeXCenterFigure_Default;
  TeXFigure := TeXFigure_Default;
  TeXFigurePlacement := '';
  TeXFigurePrologue := '';
  TeXFigureEpilogue := '';
  TeXPicPrologue := '';
  TeXPicEpilogue := '';
  LineWidthBase := LineWidthBase_Default;
  MiterLimit := 10;
  FontSizeInTeX := FontSizeInTeX_Default;
  MetaPostTeXText := MetaPostTeXText_Default;
  IncludePath := IncludePath_Default;
  DefaultSymbolSize := DefaultSymbolSize_Default;
  ApproximationPrecision := ApproximationPrecision_Default;
  SetDefaultProperties;
end;

procedure TDrawing2D.Clear;
begin
  if Assigned(History) then History.Clear;
  DeleteAllObjects;
  DeleteSavedSourceBlocks;
  SetDefaults;
  ClearBitmapRegistry;
end;

procedure TDrawing2D.SetDefaultProperties;
begin
  New_LineStyle := New_LineStyle_Default;
  New_LineColor := New_LineColor_Default;
  New_LineWidth := New_LineWidth_Default;
  New_Hatching := New_Hatching_Default;
  New_HatchColor := New_HatchColor_Default;
  New_FillColor := New_FillColor_Default;
  New_Arr1 := New_Arr1_Default;
  New_Arr2 := New_Arr2_Default;
  New_ArrSizeFactor := New_ArrSizeFactor_Default;
  New_FontHeight := DefaultFontHeight;
  New_HAlignment := New_HAlignment_Default;
  New_StarKind := New_StarKind_Default;
  New_StarSizeFactor := New_StarSizeFactor_Default;
end;

procedure TDrawing2D.Update;
begin
  GetExtension;
  RepaintViewports;
end;

procedure TDrawing2D.SaveObjectsToStream0(const Stream: TStream;
  const ObjList: TGraphicObjList);
var
  Obj: TObject2D;
  TmpWord: Word;
  TmpLong, ObjPerc: Integer;
begin
  //with Stream do
     { Save the objects. }
  TmpLong := ObjList.Count;
  if TmpLong > 0 then
    ObjPerc := 100 div TmpLong
  else
    ObjPerc := 0;
  Stream.Write(TmpLong, SizeOf(TmpLong));
  Obj := ObjList.FirstObj as TObject2D;
  while Obj <> nil do
  begin
    if Layers[Obj.Layer].Streamable and
      Obj.ToBeSaved
      then
    begin
      TmpWord := TpXFindClassIndex(Obj.ClassName);
           { Save the class index. }
      Stream.Write(TmpWord, SizeOf(TmpWord));
      Obj.SaveToStream(Stream);
      if Assigned(OnSaveProgress) then
        OnSaveProgress(Self, 100 - ObjPerc * TmpLong);
      Dec(TmpLong);
    end;
    Obj := ObjList.NextObj as TObject2D;
  end;
     { End the list of objects if not all objects were saved. }
  if TmpLong > 0 then
  begin
    TmpWord := 65535;
    Stream.Write(TmpWord, SizeOf(TmpWord));
  end;
end;

procedure TDrawing2D.SaveObjectsToStream(const Stream:
  TStream);
begin
  SaveObjectsToStream0(Stream, fListOfObjects);
end;

procedure TDrawing2D.SaveSelectionToStream(const Stream:
  TStream);
begin
  SaveObjectsToStream0(Stream, fSelectedObjs);
end;

procedure TDrawing2D.CopySelectionToClipboard;
var
  MemStream: TMemoryStream;
begin
  if Self.fSelectedObjs.Count = 0 then Exit;
  MemStream := TMemoryStream.Create;
  try
    SaveSelectionToStream(MemStream);
    MemStream.Position := 0;
    PutStreamToClipboard(TpXClipboardFormat, True,
      MemStream, MemStream.Size);
  finally
    MemStream.Free;
  end;
end;

procedure TDrawing2D.PasteFromClipboard;
var
  Stream: TMemoryStream;
begin
  if not ClipboardHasTpX then
  begin
    if ClipboardHasMetafile then
    begin
      if Assigned(OnPasteMetafileFromClipboard) then
        OnPasteMetafileFromClipboard(Self);
    end;
    Exit;
  end;
  Stream := TMemoryStream.Create;
  try
    GetStreamFromClipboard(TpXClipboardFormat, Stream);
    Stream.Position := 0;
    LoadObjectsFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{$WARNINGS OFF}

procedure TDrawing2D.LoadObjectsFromStream(const Stream:
  TStream);
var
  TmpClass: TGraphicObjectClass;
  Obj: TGraphicObject;
  TmpLong, ObjPerc: Integer;
  TmpWord: Word;
  OnChangeDrawing0: TOnChangeDrawing;
begin
  OnChangeDrawing0 := fOnChangeDrawing;
  fOnChangeDrawing := nil;
  try
    fListOfBlocks.Lock;
    with Stream do
    try
      Read(TmpLong, SizeOf(TmpLong));
      if TmpLong > 0 then
        ObjPerc := 100 div TmpLong
      else
        ObjPerc := 0;
      if TmpLong > 0 then SelectionClear;
      while TmpLong > 0 do
      begin
        { Read the type of object and the length of the record. }
        Read(TmpWord, SizeOf(TmpWord));
        if TmpWord = 65535 then
         { End prematurely. }
          Break;
        Dec(TmpLong);
        { Retrive the class type from the registered classes. }
        try
          TmpClass := TpXFindClassByIndex(TmpWord);
        except
          on ETpX_ObjClassNotFound do
          begin
            MessageBoxError('Object class not found. Object not load');
            Break;
          end;
        end;
        Obj := TmpClass.CreateFromStream(Stream);
        if Assigned(OnLoadProgress) then
          OnLoadProgress(Self, ObjPerc);
        if not (Obj is TObject2D) then
        begin
          MessageBoxError('Not 2D Object. Object discarded.');
          Obj.Free;
          Continue;
        end;
        if Obj is TSourceBlock2D then
        try
          TSourceBlock2D(Obj).UpdateSourceReferences(fListOfBlocks);
        except
          on ETpX_ListObjNotFound do
          begin
            MessageBoxError('Source block not found. The block will not be loaded');
            Obj.Free;
            Continue;
          end;
        end
        else if Obj is TBlock2D then
        try
          TBlock2D(Obj).UpdateReference(fListOfBlocks);
        except
          on ETpX_ListObjNotFound do
          begin
            MessageBoxError('Source block not found. The block will not be loaded');
            Obj.Free;
            Continue;
          end;
        end;
        CurrentLayer := Obj.Layer;
        inherited AddObject(-1, TGraphicObject(Obj));
        SelectionAdd(Obj);
      end;
    finally
      fListOfBlocks.Unlock;
    end;
  finally
    fOnChangeDrawing := OnChangeDrawing0;
  end;
  NotifyChanged;
end;
{$WARNINGS ON}

procedure TDrawing2D.SaveBlocksToStream(
  const Stream: TStream; const AsLibrary: Boolean);
var
  Obj: TSourceBlock2D;
  TmpWord: Word;
  TmpPos, TmpLong: Integer;
begin
  //BlockList.;
//  with Stream do
  TmpLong := SourceBlocksCount;
  TmpPos := Stream.Position;
  Stream.Write(TmpLong, SizeOf(TmpLong));
  Obj := fListOfBlocks.FirstObj as TSourceBlock2D;
  TmpLong := 0;
  while Obj <> nil do
  begin
    if Obj.ToBeSaved and
      not (Obj.IsLibraryBlock xor AsLibrary) then
    begin
      TmpWord := TpXFindClassIndex(Obj.ClassName);
           { Save the class index. }
      Stream.Write(TmpWord, SizeOf(TmpWord));
      Obj.SaveToStream(Stream);
      Inc(TmpLong);
    end;
    Obj := fListOfBlocks.NextObj as TSourceBlock2D;
  end;
  Stream.Seek(TmpPos, soFromBeginning);
  Stream.Write(TmpLong, SizeOf(TmpLong));
  Stream.Seek(0, soFromEnd);
end;

{$WARNINGS OFF}

procedure TDrawing2D.LoadBlocksFromStream(const Stream:
  TStream);
var
  TmpClass: TGraphicObjectClass;
  Obj: TGraphicObject;
  TmpLong: Integer;
  TmpWord: Word;
begin
  with Stream do
  begin
     { Load the source blocks. }
    Read(TmpLong, SizeOf(TmpLong));
    while TmpLong > 0 do
    begin
        { Read the type of object and the length of the record. }
      Read(TmpWord, SizeOf(TmpWord));
      Dec(TmpLong);
        { Retrive the class type from the registered classes. }
      try
        TmpClass := TpXFindClassByIndex(TmpWord);
      except
        on ETpX_ObjClassNotFound do
        begin
          MessageBoxError('Object class not found. Object not load');
          Continue;
        end;
      end;
      Obj := TmpClass.CreateFromStream(Stream);
      try
        TSourceBlock2D(Obj).UpdateSourceReferences(fListOfBlocks);
      except
        on ETpX_ListObjNotFound do
        begin
          MessageBoxError('Source block not found. The block will not be loaded');
          Obj.Free;
          Continue;
        end;
      end;
      AddSourceBlock(TSourceBlock2D(Obj));
    end;
  end;
end;
{$WARNINGS ON}

function TDrawing2D.AddSourceBlock(const Obj:
  TSourceBlock2D):
  TSourceBlock2D;
begin
  Result := Obj;
  inherited AddSourceBlock(-1, Obj);
end;

procedure TDrawing2D.DeleteSourceBlock(const SrcName:
  TSourceBlockName);
var
  TmpSource: TSourceBlock2D;
begin
  TmpSource := TSourceBlock2D(FindSourceBlock(SrcName));
  if TmpSource.NumOfReferences > 0 then
    raise
      ETpX_SysException.Create('TDrawing2D.DeleteSourceBlock: Remove the references before the source');
  DeleteSourceBlockByID(TmpSource.ID);
end;

function TDrawing2D.GetSourceBlock(const ID: Integer):
  TSourceBlock2D;
begin
  Result := inherited GetSourceBlock(ID) as TSourceBlock2D;
end;

function TDrawing2D.FindSourceBlock(const SrcName:
  TSourceBlockName): TSourceBlock2D;
begin
  Result := fListOfBlocks.FirstObj as TSourceBlock2D;
  while Result <> nil do
  begin
    if SrcName = Result.Name then
      Exit;
    Result := fListOfBlocks.NextObj as TSourceBlock2D;
  end;
  raise
    ETpX_ListObjNotFound.Create(Format('TDrawing2D.FindSourceBlock: Source block %s not found', [SrcName]));
end;

function TDrawing2D.BlockObjects(
  const SrcName: TSourceBlockName;
  const Objects: TGraphicObjList): TSourceBlock2D;
var
  Obj: TObject2D;
begin
  Result := TSourceBlock2D.CreateSpec(0, SrcName, [nil]);
  try
    Obj := Objects.FirstObj as TObject2D;
    while Obj <> nil do
    begin
      Result.Objects.Add(Obj);
      Obj := Objects.NextObj as TObject2D;
    end;
    AddSourceBlock(Result);
  except
    Result.Free;
    Result := nil;
  end;
end;

procedure TDrawing2D.DeleteSavedSourceBlocks;
var
  Obj: TGraphicObject;
begin
  fListOfBlocks.Lock;
  try
    Obj := fListOfBlocks.FirstObj;
    while Assigned(Obj) do
      if TSourceBlock2D(Obj).ToBeSaved and not
        TSourceBlock2D(Obj).IsLibraryBlock then
      try
        fListOfBlocks.DeleteCurrent;
        Obj := fListOfBlocks.CurrentObj;
      except
        Obj := fListOfBlocks.NextObj;
      end
      else
        Obj := fListOfBlocks.NextObj;
  finally
    fListOfBlocks.Unlock;
  end;
end;

procedure TDrawing2D.DeleteLibrarySourceBlocks;
var
  Obj: TGraphicObject;
begin
  fListOfBlocks.Lock;
  try
    Obj := fListOfBlocks.FirstObj;
    while Assigned(Obj) do
      if TSourceBlock2D(Obj).IsLibraryBlock then
      try
        fListOfBlocks.DeleteCurrent;
        Obj := fListOfBlocks.CurrentObj;
      except
        Obj := fListOfBlocks.NextObj;
      end
      else
        Obj := fListOfBlocks.NextObj;
  finally
    fListOfBlocks.Unlock;
  end;
end;

function TDrawing2D.AddObject(const ID: Integer; const Obj:
  TObject2D): TObject2D;
begin
  Result := Obj;
  inherited AddObject(ID, TGraphicObject(Obj));
end;

function TDrawing2D.InsertObject(const ID, IDInsertPoint:
  Integer; const Obj: TObject2D): TObject2D;
begin
  Result := Obj;
  inherited InsertObject(ID, IDInsertPoint,
    TGraphicObject(Obj));
end;

function TDrawing2D.AddBlock(const ID: Integer; const
  SrcName:
  TSourceBlockName): TObject2D;
var
  Tmp: TBlock2D;
  TmpSource: TSourceBlock2D;
begin
  TmpSource := FindSourceBlock(SrcName);
  Tmp := TBlock2D.CreateSpec(ID, TmpSource);
  try
    AddObject(ID, Tmp);
    Result := Tmp;
  except
    Tmp.Free;
    Result := nil;
  end;
end;

function TDrawing2D.GetObject(const ID: Integer): TObject2D;
begin
  Result := inherited GetObject(ID) as TObject2D;
end;

procedure TDrawing2D.CollectObjects(const ResultLst:
  TGraphicObjList;
  Frm: TRect2D; const VisualRect: TRect2D;
  const Mode: TCollectMode; const PickFilter: TObject2DClass;
  const RemoveFromDrawing: Boolean);
var
  Obj: TObject2D;
begin
  Frm := ReorderRect2D(Frm);
  ResultLst.FreeOnDelete := RemoveFromDrawing;
  fListOfObjects.Lock;
  try
    Obj := fListOfObjects.FirstObj as TObject2D;
    while Obj <> nil do
      with (Layers[Obj.Layer]) do
      begin
        if Active and Visible and (Obj.Enabled)
          and (Obj is PickFilter)
          and Obj.IsVisible(VisualRect)
          then
        begin
          if (Mode = gmAllInside) and
            IsBoxAllInBox2D(Obj.BoundingBox, Frm) then
            { The object is in the frame. }
          begin
            ResultLst.Add(Obj);
            if RemoveFromDrawing then
            begin
              fListOfObjects.RemoveCurrent;
              Obj := TObject2D(fListOfObjects.CurrentObj);
              Continue;
            end;
          end
          else if (Mode = gmCrossFrame) and
            IsBoxInBox2D(Obj.BoundingBox, Frm) then
          begin
            ResultLst.Add(Obj);
            if RemoveFromDrawing then
            begin
              fListOfObjects.RemoveCurrent;
              Obj := TObject2D(fListOfObjects.CurrentObj);
              Continue;
            end;
          end;
        end;
        Obj := fListOfObjects.NextObj as TObject2D;
      end;
  finally
    fListOfObjects.Unlock;
  end;
end;

function TDrawing2D.PickObject(const P: TPoint2D;
  const Aperture: TRealType;
  const VisualRect: TRect2D; const PickFilter: TObject2DClass;
  const FirstFound: Boolean; var NPoint: Integer): TObject2D;
begin
  Result := fListOfObjects.PickObject(P, fLayers, Aperture,
    VisualRect, PickFilter, FirstFound, NPoint);
end;

function TDrawing2D.PickObject_PreferSelected(const P: TPoint2D;
  const Aperture: TRealType;
  const VisualRect: TRect2D; const PickFilter: TObject2DClass;
  const FirstFound: Boolean; var NPoint: Integer): TObject2D;
var
  Obj: TObject2D;
  TmpNPoint: Integer;
begin
  Result := fSelectedObjs.PickObject(P, fLayers, Aperture,
    VisualRect,
    PickFilter, FirstFound, NPoint);
  if NPoint >= 0 then Exit;
  Obj := fListOfObjects.PickObject(P, fLayers, Aperture,
    VisualRect,
    PickFilter, FirstFound, TmpNPoint);
  if Obj = nil then Exit;
  if (Result = nil) or (NPoint < TmpNPoint)
    //or ((NPoint = TmpNPoint) and (Result.ID < Obj.ID))
  then
  begin
    Result := Obj;
    NPoint := TmpNPoint;
  end;
end;

function TDrawing2D.PickListOfObjects(
  const PickedObjects: TList; P: TPoint2D;
  const VisualRect: TRect2D; const PickFilter: TObject2DClass;
  Aperture: Word): Integer;
var
  Tmp: TObject2D;
  Distance: TRealType;
begin
  Result := 0;
  fListOfObjects.Lock;
//  StopRepaint;
  try
    // Trasforma l'apertura in coordinate mondo.
//    WAperture := GetPixelAperture.X * Aperture;
    Tmp := fListOfObjects.CurrentObj as TObject2D;
    while Tmp <> nil do
      with (Layers[Tmp.Layer]) do
      begin
        if Active and Visible and (Tmp is PickFilter) and
          Tmp.IsVisible(VisualRect) then
        begin
          if Tmp.OnMe(P, Aperture, Distance) >
            PICK_NOOBJECT
            then
          begin
            PickedObjects.Add(Tmp);
            Inc(Result);
          end;
        end;
        Tmp := fListOfObjects.NextObj as TObject2D;
      end;
  finally
    fListOfObjects.Unlock;
  end;
end;

function GetExtension0(Drawing2D: TDrawing2D;
  Objects: TGraphicObjList): TRect2D;
var
  Obj: TObject2D;
begin
  Result := Rect2D(
    MaxRealType, MaxRealType, -MaxRealType, -MaxRealType);
  Obj := Objects.FirstObj as TObject2D;
  while Obj <> nil do
  begin
    if Drawing2D.Layers[Obj.Layer].Visible then
    begin
      try
        Obj.UpdateExtension(Drawing2D);
      finally
      end;
          { Set the new extension if necessary. }
      if Obj.BoundingBox.Left < Result.Left then
        Result.Left := Obj.BoundingBox.Left;
      if Obj.BoundingBox.Right > Result.Right then
        Result.Right := Obj.BoundingBox.Right;
      if Obj.BoundingBox.Bottom < Result.Bottom then
        Result.Bottom := Obj.BoundingBox.Bottom;
      if Obj.BoundingBox.Top > Result.Top then
        Result.Top := Obj.BoundingBox.Top;
    end;
    Obj := Objects.NextObj as TObject2D;
  end;
end;

function TDrawing2D.GetExtension: TRect2D;
begin
  if ObjectsCount = 0 then
  begin
    Result := Rect2D(0, 0, 0, 0);
    Exit;
  end;
  Result := GetExtension0(Self, fListOfObjects);
end;

procedure TDrawing2D.SetFileName(const NewFileName: string);
var
  I: Integer;
  BE: TBitmapEntry;
begin
  if NewFileName = fFileName then Exit;
  if fFileName = Drawing_NewFileName then fFileName := '';
  for I := 0 to BitmapRegistry.Count - 1 do
  begin
    BE := BitmapRegistry.Objects[I] as TBitmapEntry;
    BE.RefreshParentFileName(NewFileName);
  end;
  fFileName := NewFileName;
end;

procedure TDrawing2D.ClearBitmapRegistry;
var
  I: Integer;
begin
  for I := 0 to BitmapRegistry.Count - 1 do
    BitmapRegistry.Objects[I].Free;
  BitmapRegistry.Clear;
end;


function TDrawing2D.RegisterBitmap(ImageLink: string):
  TBitmapEntry;
var
  I: Integer;
begin
  if ExtractFileDrive(ImageLink) = '' then
    ImageLink := ExtractFilePath(FileName) + ImageLink;
  I := BitmapRegistry.IndexOf(ImageLink);
  if I < 0 then
  begin
    if fFileName <> Drawing_NewFileName then
      Result := TBitmapEntry.Create(ImageLink, fFileName)
    else
      Result := TBitmapEntry.Create(ImageLink, '');
    BitmapRegistry.AddObject(ImageLink, Result);
  end
  else Result := BitmapRegistry.Objects[I] as TBitmapEntry;
end;

procedure TDrawing2D.PickUpProperties(Obj: TGraphicObject);
var
  Prim: TPrimitive2D;
begin
  if not Assigned(Obj) then Exit;
  if Obj is TGroup2D then
    Obj := (Obj as TGroup2D).Objects.FirstObj;
  if not (Obj is TPrimitive2D) then Exit;
  Prim := Obj as TPrimitive2D;
  New_LineStyle := Prim.LineStyle;
  New_LineColor := Prim.LineColor;
  if New_LineColor = clNone then New_LineColor := clDefault;
  New_LineWidth := Prim.LineWidth;
  if New_LineWidth <= 0 then New_LineWidth := 1;
  New_Hatching := Prim.Hatching;
  New_HatchColor := Prim.HatchColor;
  if New_HatchColor = clNone then New_HatchColor := clDefault;
  New_FillColor := Prim.FillColor;
  if New_FillColor = clNone then New_FillColor := clDefault;
  New_Arr1 := Byte(Prim.BeginArrowKind);
  New_Arr2 := Byte(Prim.EndArrowKind);
  New_ArrSizeFactor := Prim.ArrowSizeFactor;
  if New_ArrSizeFactor <= 0 then New_ArrSizeFactor := 1;
  if Prim is TText2D then
  begin
    New_FontHeight := (Prim as TText2D).Height;
    if New_FontHeight <= 0 then
      New_FontHeight := DefaultFontHeight;
    New_HAlignment := Byte((Prim as TText2D).HAlignment);
  end
  else if Prim is TStar2D then
  begin
    New_StarKind := Byte((Prim as TStar2D).StarKind);
    New_StarSizeFactor := (Prim as TStar2D).StarSizeFactor;
  end;
end;

procedure TDrawing2D.ApplyProperties(Obj: TGraphicObject);
var
  Prim: TPrimitive2D;
begin
  if not Assigned(Obj) then Exit;
  Prim := Obj as TPrimitive2D;
  Prim.LineStyle := New_LineStyle;
  Prim.LineColor := New_LineColor;
  Prim.LineWidth := New_LineWidth;
  Prim.Hatching := New_Hatching;
  Prim.HatchColor := New_HatchColor;
  Prim.FillColor := New_FillColor;
  Prim.BeginArrowKind := TArrowKind(New_Arr1);
  Prim.EndArrowKind := TArrowKind(New_Arr2);
  Prim.ArrowSizeFactor := New_ArrSizeFactor;
  if Prim is TText2D then
  begin
    (Prim as TText2D).Height := New_FontHeight;
    (Prim as TText2D).HAlignment
      := THAlignment(New_HAlignment);
  end
  else if Prim is TStar2D then
  begin
    (Prim as TStar2D).StarKind := TStarKind(New_StarKind);
    (Prim as TStar2D).StarSizeFactor := New_StarSizeFactor;
  end;
end;

end.

