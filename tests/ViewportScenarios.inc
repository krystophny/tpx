type
  TProbeViewport = class(TViewport2D)
  public
    PaintCount: Integer;
    procedure Paint; override;
    procedure Leave;
  end;

procedure TProbeViewport.Paint;
begin
  Inc(PaintCount);
  inherited Paint;
end;

procedure TProbeViewport.Leave;
begin
  MouseLeave;
end;

procedure PumpEvents;
var
  UntilTime: QWord;
begin
  UntilTime := GetTickCount64 + 200;
  repeat
    Application.ProcessMessages;
    Sleep(1);
  until GetTickCount64 >= UntilTime;
end;

procedure TestViewport(const Scenario: string);
var
  Window: TForm;
  View: TProbeViewport;
  Drawing: TDrawing2D;
  Frame: TBitmap;
  PreviewLine: TLine2D;
  AfterRequest: Integer;
  function HasColor(X, Y: Integer; Color: TColor): Boolean;
  var
    DX, DY: Integer;
  begin
    Result := False;
    for DX := -1 to 1 do
      for DY := -1 to 1 do
        if ColorToRGB(Frame.Canvas.Pixels[X + DX, Y + DY]) = ColorToRGB(Color) then
          Result := True;
  end;
  procedure Capture;
  begin
    View.RenderToCanvas(Frame.Canvas);
  end;
begin
  UseSnap := False;
  UseShapeSnap := False;
  Window := TForm.CreateNew(nil);
  Drawing := TDrawing2D.Create(nil);
  Frame := TBitmap.Create;
  PreviewLine := nil;
  View := nil;
  try
    Window.SetBounds(50, 50, 240, 260);
    View := TProbeViewport.Create(Window);
    View.Parent := Window;
    View.SetBounds(10, 10, 200, 200);
    View.Drawing2D := Drawing;
    View.ShowGrid := False;
    View.ShowCrossHair := False;
    View.VisualRect := Rect2D(0, 0, 100, 100);
    Frame.SetSize(200, 200);
    Window.Show;
    PumpEvents;
    Drawing.AddObject(-1, TLine2D.CreateSpec(-1, Point2D(20, 20), Point2D(80, 20)));
    View.Repaint;
    AfterRequest := View.PaintCount;
    PumpEvents;
    Check(View.PaintCount > AfterRequest, 'Redraw did not schedule a native paint event');
    Capture;
    Check(HasColor(100, 160, clBlack), 'Committed line is absent from the frame');
    Check(HasColor(100, 20, clWhite), Format('Backbuffer copy corrupted the background: frame=%d source=%d size=%dx%d view=%dx%d',
      [ColorToRGB(Frame.Canvas.Pixels[100,20]), ColorToRGB(View.OffScreenBitmap.Canvas.Pixels[100,20]),
       View.OffScreenBitmap.Width, View.OffScreenBitmap.Height, View.ClientWidth, View.ClientHeight]));
    View.SetBounds(10, 10, 240, 240);
    PumpEvents;
    Frame.SetSize(240, 240);
    Capture;
    Check(HasColor(120, 192, clBlack), 'Resize lost committed geometry');
    Check(HasColor(220, 20, clWhite), 'Resize left an unpainted buffer region');
    View.SetBounds(10, 10, 200, 200);
    PumpEvents;
    Frame.SetSize(200, 200);
    if Scenario = 'viewport-crosshair' then begin
      View.ShowCrossHair := True;
      View.DrawCursorCrossHair(Point2D(20, 30), True);
      Capture;
      Check(HasColor(40, 20, clBlue), 'Vertical crosshair is absent');
      Check(HasColor(10, 140, clBlue), 'Horizontal crosshair is absent');
      View.Repaint;
      Capture;
      Check(HasColor(40, 20, clBlue), 'Repaint lost the crosshair');
      View.DrawCursorCrossHair(Point2D(40, 50), True);
      Capture;
      Check(not HasColor(40, 20, clBlue), 'Old vertical crosshair remained');
      Check(not HasColor(10, 140, clBlue), 'Old horizontal crosshair remained');
      Check(HasColor(80, 20, clBlue), 'Moved crosshair is absent');
      View.ShowCrossHair := False;
      Capture;
      Check(not HasColor(80, 20, clBlue), 'Disabled crosshair remained');
      View.ShowCrossHair := True;
      View.Leave;
      Capture;
      Check(not HasColor(80, 20, clBlue), 'Mouse leave retained the crosshair');
    end
    else if Scenario = 'viewport-preview' then begin
      PreviewLine := TLine2D.CreateSpec(-1, Point2D(20, 40), Point2D(80, 40));
      View.DrawObject2DWithRubber(PreviewLine, IdentityTransf2D);
      Capture;
      Check(HasColor(100, 120, clBlack), 'Live preview is absent');
      View.Repaint;
      Capture;
      Check(HasColor(100, 120, clBlack), 'Repaint lost the live preview');
      View.DrawObject2DWithRubber(PreviewLine, IdentityTransf2D);
      PreviewLine.Points[0] := Point2D(20, 60);
      PreviewLine.Points[1] := Point2D(80, 60);
      View.DrawObject2DWithRubber(PreviewLine, IdentityTransf2D);
      Capture;
      Check(HasColor(100, 80, clBlack), 'Moved live preview is absent');
      Check(not HasColor(100, 120, clBlack), 'Old live preview remained');
      View.ClearRubber;
      View.DrawObject2DWithRubber(PreviewLine, Translate2D(0, 10));
      Capture;
      Check(HasColor(100, 60, clBlack), 'Transformed live preview is absent');
      View.ClearRubber;
      FreeAndNil(PreviewLine);
      Capture;
      Check(not HasColor(100, 60, clBlack), 'Cancelled preview remained');
      Check(Drawing.ObjectsCount = 1, 'Preview changed committed geometry');
    end;
  finally
    if Assigned(View) then View.ClearRubber;
    PreviewLine.Free;
    Frame.Free;
    Window.Free;
    Drawing.Free;
  end;
end;
