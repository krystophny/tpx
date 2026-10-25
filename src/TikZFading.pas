unit TikZFading;

{$mode objfpc}{$H+}

{ The luminosity fading TpX declares in TikZ for a gradient whose two ends
  differ in opacity. The TikZ export, TikZ save and the TikZ import all
  build it here, so what is written and what is recognised stay the same.
  The name spells out the stop texts, so two declarations with the same
  name always have the same content. }

interface

{ An opacity as a stop text: clamped to [0, 1], four significant digits,
  '.' as decimal separator, no exponent. }
function TikZFadingStopText(Opacity: Double): string;
{ tpxfadeL<a>x<b> or tpxfadeR<a>x<b>, where <a> and <b> are the stop texts
  with 'p' for the decimal point. }
function TikZFadingName(Radial: Boolean; const A0, A1: string): string;
{ The shading and fading declarations of the fading Name, as they go
  between \pgfinterruptpicture and \endpgfinterruptpicture. }
function TikZFadingBody(const Name: string; Radial: Boolean;
  const A0, A1: string): string;
{ The complete declaration line (without its line break) and its name. }
function TikZFadingDeclaration(Radial: Boolean; const A0, A1: string;
  out Name: string): string;

implementation

uses SysUtils, Math;

function TikZFadingStopText(Opacity: Double): string;
var
  Settings: TFormatSettings;
begin
  if IsNan(Opacity) or (Opacity > 1) then Opacity := 1
  else if Opacity < 0 then Opacity := 0;
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  Result := FloatToStrF(Opacity, ffGeneral, 4, 0, Settings);
  { pgf reads no exponents; such tiny opacities are written in full. }
  if Pos('E', Result) > 0 then
  begin
    Result := FloatToStrF(Opacity, ffFixed, 15, 12, Settings);
    while Result[Length(Result)] = '0' do Delete(Result, Length(Result), 1);
  end;
end;

function NamePart(const Stop: string): string;
var
  I: Integer;
begin
  Result := Stop;
  for I := 1 to Length(Result) do
    if Result[I] = '.' then Result[I] := 'p';
end;

function TikZFadingName(Radial: Boolean; const A0, A1: string): string;
begin
  if Radial then Result := 'tpxfadeR' else Result := 'tpxfadeL';
  Result := Result + NamePart(A0) + 'x' + NamePart(A1);
end;

function TikZFadingBody(const Name: string; Radial: Boolean;
  const A0, A1: string): string;
begin
  if Radial then
    Result := '\pgfdeclareradialshading{' + Name + 's}{\pgfpointorigin}' +
      '{gray(0bp)=(' + A0 + ');gray(25bp)=(' + A1 + ');gray(50bp)=(' +
      A1 + ')}'
  else
    Result := '\pgfdeclarehorizontalshading{' + Name + 's}{100bp}' +
      '{gray(0bp)=(' + A0 + ');gray(25bp)=(' + A0 + ');gray(75bp)=(' +
      A1 + ');gray(100bp)=(' + A1 + ')}';
  Result := Result + '\pgfdeclarefading{' + Name + '}{\pgfuseshading{' +
    Name + 's}}';
end;

function TikZFadingDeclaration(Radial: Boolean; const A0, A1: string;
  out Name: string): string;
begin
  Name := TikZFadingName(Radial, A0, A1);
  Result := '\pgfinterruptpicture' + TikZFadingBody(Name, Radial, A0, A1) +
    '\endpgfinterruptpicture%';
end;

end.
