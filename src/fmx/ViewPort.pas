unit ViewPort;

interface

uses Drawings, Geometry;

type

TViewport = class(TBaseViewport)
  public
    Drawing : TDrawing;
    VisualRect : TRect2D;
  procedure Refresh;
  procedure Repaint;
end;

TViewport2D = class(TViewport)

end;

implementation

procedure TViewport.Refresh;
begin

end;

procedure TViewport.Repaint;
begin

end;

end.
