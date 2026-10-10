unit TpXXmlSource;

{$mode delphi}{$H+}

interface

uses Classes;

{ Extracts the XML payload written as percent-comment lines by TpXSaver. }
function ExtractTpXXml(const Source: TStream): string;

implementation

uses SysUtils;

function SelfClosingRootEnd(const Line: string): Integer;
var
  I, StartAt: Integer;
  Quote: Char;
begin
  Result := 0;
  StartAt := Pos('<TpX', Line);
  if StartAt = 0 then Exit;
  I := StartAt + 4;
  if (I <= Length(Line)) and not (Line[I] in [#9, #10, #13, ' ', '/', '>']) then
    Exit;
  Quote := #0;
  while I <= Length(Line) do begin
    if Quote <> #0 then begin
      if Line[I] = Quote then Quote := #0;
    end else if Line[I] in ['"', ''''] then
      Quote := Line[I]
    else if Line[I] = '>' then begin
      if (I > StartAt + 4) and (Line[I - 1] = '/') then Result := I;
      Exit;
    end;
    Inc(I);
  end;
end;

function ExtractTpXXml(const Source: TStream): string;
var
  Lines: TStringList;
  I, RootEnd: Integer;
  Size: Int64;
  Contents, Line: string;
  Started: Boolean;
  Payload: TStringList;
begin
  if Source = nil then
    raise EArgumentNilException.Create('TpX source stream is nil');
  Result := '';
  Lines := TStringList.Create;
  Payload := TStringList.Create;
  try
    Size := Source.Size;
    if (Size < 0) or (Size > High(Integer)) then
      raise EStreamError.Create('TpX source stream is too large');
    SetLength(Contents, Integer(Size));
    Source.Position := 0;
    if Size > 0 then Source.ReadBuffer(Contents[1], Integer(Size));
    Lines.Text := Contents;
    Started := False;
    for I := 0 to Lines.Count - 1 do begin
      Line := Lines[I];
      if (Length(Line) = 0) or (Line[1] <> '%') then Continue;
      Delete(Line, 1, 1);
      RootEnd := 0;
      if not Started then begin
        if Pos('<TpX', Line) = 0 then Continue;
        RootEnd := SelfClosingRootEnd(Line);
        Started := True;
      end;
      if RootEnd > 0 then Line := Copy(Line, 1, RootEnd);
      Payload.Add(Line);
      if (Pos('</TpX>', Line) > 0) or (RootEnd > 0) then
        Break;
    end;
    Result := Payload.Text;
  finally
    Payload.Free;
    Lines.Free;
  end;
  if not Started then
    raise Exception.Create('No percent-commented TpX document found in source');
end;

end.
