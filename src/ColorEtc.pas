unit ColorEtc; //Colors and miscellanea

interface

uses
{$IFNDEF USE_FMX}
Graphics, StdCtrls, Dialogs,
{$ELSE}
 FMX.StdCtrls, FMX.Dialogs, FMX.ListBox, System.UITypes,
 System.UIConsts,
{$ENDIF}
Types, Classes, SysUtils, StrUtils;

type
  T_PS_RGB = record R, G, B: Single;
  end;

function SwapColor(Color: TColor): TColor;
function GrayScale(Color: TColor): TColor;
function HSVToRGB(Hue, Sat, V: Double): TColor;
function MixColors(const C1, C2: TColor; const A: Double): TColor;
function HtmlToColor(ColorName: string): TColor;
  // HTML-color -> TColor
function ColorToHtml(Color: TColor): string; // TColor -> HTML-color
function PS_RGB(Color: TColor): T_PS_RGB; //  for PostScript, PGF

procedure MakeColorBox(ComboBox: TComboBox);
{$IFDEF FPC}
procedure ColorBoxDrawItem(ComboBox: TComboBox;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
procedure ColorBoxSelect(ComboBox: TComboBox);
procedure ColorBoxSet(ComboBox: TComboBox; Color: TColor);
function ColorBoxGet(ComboBox: TComboBox): TColor;
{$ENDIF}

//function LaBoiteACouleursLoadWindow: Integer; cdecl stdcall;
//function LaBoiteACouleursUnloadWindow: Integer; cdecl stdcall;
//function LaBoiteACouleursSelectColorStr(const Str: PChar): Integer; cdecl
//  stdcall;
//function LaBoiteACouleursSelectColor(var AColorRef: COLORREF): Integer; cdecl
//  stdcall;

type

  T_HTMLColor = class(TStringList)
  public
    constructor Create;
    function HtmlColor(ColorName: string): TColor;
    function ColorToHtml(Color: TColor): string;
  end;

var
  TheHTMLColor: T_HTMLColor;
  CustomColors: string;

const

{$IFNDEF FPC}
clDefault = TColorRec.SysDefault;
clNone = TColorRec.Null;
clBlack = TColorRec.Black;
clMaroon = TColorRec.Maroon;
clGreen = TColorRec.Green;
clOlive = TColorRec.Olive;
clNavy = TColorRec.Navy;
clPurple = TColorRec.Purple;
clTeal = TColorRec.Teal;
clGray = TColorRec.Gray;
clSilver = TColorRec.Silver;
clRed = TColorRec.Red;
clLime = TColorRec.Lime;
clYellow = TColorRec.Yellow;
clBlue = TColorRec.Blue;
clFuchsia = TColorRec.Fuchsia;
clAqua = TColorRec.Aqua;
clWhite = TColorRec.White;

{$ENDIF}
clMoneyGreen = TColor($C0DCC0);
clSkyBlue = TColor($F0CAA6);
clCream = TColor($F0FBFF);
clMedGray = TColor($A4A0A0);

  BasicColors: array[1..16] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal,
    clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua,
    clWhite);

  DelphiAddColors: array[1..4] of TColor = (
    clMoneyGreen, clSkyBlue, clCream, clMedGray);

implementation

{Black #000000
Silver #C0C0C0
Gray #808080
White #FFFFFF
Maroon #800000
Red #FF0000
Purple #800080
Fuchsia #FF00FF
Green #008000
Lime #00FF00
Olive #808000
Yellow #FFFF00
Navy #000080
Blue #0000FF
Teal #008080
Aqua #00FFFF}

function SwapColor(Color: TColor): TColor;
begin
  Result := Color and $FF0000 shr 16 +
    Color and $00FF00 + Color and $0000FF shl 16;
end;

function GrayScale(Color: TColor): TColor;
var
  Gray: Integer;
begin // Y=0.3RED+0.59GREEN+0.11Blue ?
//American NTSC (National Television Standards Committee) system:
  Gray := Round((Color and $FF) * 0.33
    + (Color shr 8 and $FF) * 0.56
    + (Color shr 16 and $FF) * 0.11);
  Result := Gray + Gray shl 8 + Gray shl 16;
end;

// Hue: 0-360

function HSVToRGB(Hue, Sat, V: Double): TColor;
var
  I: Integer;
  R, G, B: Integer;
  F, P, Q, T: Double;
begin
  Hue := (Hue / 60.0);
  I := Trunc(Hue);
  F := Hue - I;
  P := V * (1.0 - Sat);
  Q := V * (1.0 - (Sat * F));
  T := V * (1.0 - (Sat * (1.0 - F)));
  R := 0;
  G := 0;
  B := 0;
  case I of
    0:
      begin
        R := Trunc(V * 255);
        G := Trunc(T * 255);
        B := Trunc(P * 255);
      end;
    1:
      begin
        R := Trunc(Q * 255);
        G := Trunc(V * 255);
        B := Trunc(P * 255);
      end;
    2:
      begin
        R := Trunc(P * 255);
        G := Trunc(V * 255);
        B := Trunc(T * 255);
      end;
    3:
      begin
        R := Trunc(P * 255);
        G := Trunc(Q * 255);
        B := Trunc(V * 255);
      end;
    4:
      begin
        R := Trunc(T * 255);
        G := Trunc(P * 255);
        B := Trunc(V * 255);
      end;
    5:
      begin
        R := Trunc(V * 255);
        G := Trunc(P * 255);
        B := Trunc(Q * 255);
      end;
  end;
  Result := R shl 16 + G shl 8 + B; //?
 // Result := Windows.RGB(R, G, B);
end;

function MixColors(const C1, C2: TColor; const A: Double): TColor;
begin
  Result :=
    Round((C1 and $FF) * A +
    (C2 and $FF) * (1 - A)) +
    Round((C1 shr 8 and $FF) * A +
    (C2 shr 8 and $FF) * (1 - A)) shl 8 +
    Round((C1 shr 16 and $FF) * A +
    (C2 shr 16 and $FF) * (1 - A)) shl 16;
end;

function HtmlToColor(ColorName: string): TColor;
const
  Bytes: array[1..6] of Integer = (6, 7, 4, 5, 2, 3);
var
  St: string;
  Ch: Char;
  I: Integer;
begin
  ColorName := Trim(ColorName);
  Result := TheHTMLColor.HtmlColor(ColorName);
  if Result <> clNone then Exit;
  if Pos('#', ColorName) <> 1 then Exit;
  if Length(ColorName) <> 7 then Exit;
  St := '$';
  for I := 1 to 6 do
  begin
    Ch := ColorName[Bytes[I]];
    if not (Ch in ['0'..'9', 'a'..'f', 'A'..'F']) then Exit;
    St := St + Ch;
  end;
  Result := StringToColor(St);
end;

function ColorToHtml(Color: TColor): string;
begin
  Result := TheHTMLColor.ColorToHtml(Color);
end;

function T_HTMLColor.HtmlColor(ColorName: string): TColor;
var
  I: Integer;
begin
  I := IndexOf(ColorName);
  if I = -1 then
    Result := clNone
  else
    Result := SwapColor(Integer(Objects[I]));
end;

function T_HTMLColor.ColorToHtml(Color: TColor): string;
var
  I: Integer;
begin
  {$IFDEF FPC}Color := ColorToRGB(Color);{$ENDIF}
  I := IndexOfObject(TObject(SwapColor(Color)));
  if I = -1 then
    Result := '#' + IntToHex(Color and $FF, 2) +
      IntToHex(Color shr 8 and $FF, 2) +
      IntToHex(Color shr 16 and $FF, 2)
  else
    Result := Strings[I];
end;

constructor T_HTMLColor.Create;
begin
  CaseSensitive := False;
  AddObject('black', TObject($000000));
  AddObject('gray', TObject($808080));
  AddObject('silver', TObject($C0C0C0));
  AddObject('white', TObject($FFFFFF));
  AddObject('maroon', TObject($800000));
  AddObject('green', TObject($008000));
  AddObject('olive', TObject($808000));
  AddObject('navy', TObject($000080));
  AddObject('purple', TObject($800080));
  AddObject('teal', TObject($008080));
  AddObject('red', TObject($FF0000));
  AddObject('lime', TObject($00FF00));
  AddObject('yellow', TObject($FFFF00));
  AddObject('blue', TObject($0000FF));
  AddObject('magenta', TObject($FF00FF));
  AddObject('aqua', TObject($00FFFF));

  AddObject('black', TObject($000000));
  AddObject('darkslategray', TObject($2F4F4F));
  AddObject('darkslategrey', TObject($2F4F4F));
  AddObject('darkgreen', TObject($006400));
  AddObject('midnightblue', TObject($191970));
  AddObject('navy', TObject($000080));
  AddObject('green', TObject($008000));
  AddObject('darkblue', TObject($00008B));
  AddObject('darkolivegreen', TObject($556B2F));
  AddObject('teal', TObject($008080));
  AddObject('indigo', TObject($4B0082));
  AddObject('dimgray', TObject($696969));
  AddObject('dimgrey', TObject($696969));
  AddObject('maroon', TObject($800000));
  AddObject('darkcyan', TObject($008B8B));
  AddObject('forestgreen', TObject($228B22));
  AddObject('purple', TObject($800080));
  AddObject('darkslateblue', TObject($483D8B));
  AddObject('seagreen', TObject($2E8B57));
  AddObject('darkred', TObject($8B0000));
  AddObject('olive', TObject($808000));
  AddObject('darkmagenta', TObject($8B008B));
  AddObject('saddlebrown', TObject($8B4513));
  AddObject('gray', TObject($808080));
  AddObject('grey', TObject($808080));
  AddObject('olivedrab', TObject($6B8E23));
  AddObject('slategray', TObject($708090));
  AddObject('slategrey', TObject($708090));
  AddObject('mediumblue', TObject($0000CD));
  AddObject('lightslategray', TObject($778899));
  AddObject('lightslategrey', TObject($778899));
  AddObject('brown', TObject($A52A2A));
  AddObject('sienna', TObject($A0522D));
  AddObject('cadetblue', TObject($5F9EA0));
  AddObject('lightseagreen', TObject($20B2AA));
  AddObject('mediumseagreen', TObject($3CB371));
  AddObject('steelblue', TObject($4682B4));
  AddObject('firebrick', TObject($B22222));
  AddObject('limegreen', TObject($32CD32));
  AddObject('darkturquoise', TObject($00CED1));
  AddObject('darkgray', TObject($A9A9A9));
  AddObject('darkgrey', TObject($A9A9A9));
  AddObject('darkgoldenrod', TObject($B8860B));
  AddObject('slateblue', TObject($6A5ACD));
  AddObject('blue', TObject($0000FF));
  AddObject('darkviolet', TObject($9400D3));
  AddObject('darkseagreen', TObject($8FBC8F));
  AddObject('mediumvioletred', TObject($C71585));
  AddObject('darkorchid', TObject($9932CC));
  AddObject('royalblue', TObject($4169E1));
  AddObject('mediumturquoise', TObject($48D1CC));
  AddObject('rosybrown', TObject($BC8F8F));
  AddObject('mediumaquamarine', TObject($66CDAA));
  AddObject('darkkhaki', TObject($BDB76B));
  AddObject('yellowgreen', TObject($9ACD32));
  AddObject('indianred', TObject($CD5C5C));
  AddObject('lime', TObject($00FF00));
  AddObject('blueviolet', TObject($8A2BE2));
  AddObject('chocolate', TObject($D2691E));
  AddObject('turquoise', TObject($40E0D0));
  AddObject('peru', TObject($CD853F));
  AddObject('crimson', TObject($DC143C));
  AddObject('silver', TObject($C0C0C0));
  AddObject('mediumspringgreen', TObject($00FA9A));
  AddObject('mediumorchid', TObject($BA55D3));
  AddObject('mediumpurple', TObject($9370DB));
  AddObject('deepskyblue', TObject($00BFFF));
  AddObject('springgreen', TObject($00FF7F));
  AddObject('dodgerblue', TObject($1E90FF));
  AddObject('cornflowerblue', TObject($6495ED));
  AddObject('mediumslateblue', TObject($7B68EE));
  AddObject('aqua', TObject($00FFFF));
  AddObject('cyan', TObject($00FFFF));
  AddObject('goldenrod', TObject($DAA520));
  AddObject('tan', TObject($D2B48C));
  AddObject('palevioletred', TObject($DB7093));
  AddObject('orchid', TObject($DA70D6));
  AddObject('lightsteelblue', TObject($B0C4DE));
  AddObject('skyblue', TObject($87CEEB));
  AddObject('lightgray', TObject($D3D3D3));
  AddObject('lightgrey', TObject($D3D3D3));
  AddObject('lawngreen', TObject($7CFC00));
  AddObject('lightgreen', TObject($90EE90));
  AddObject('thistle', TObject($D8BFD8));
  AddObject('burlywood', TObject($DEB887));
  AddObject('plum', TObject($DDA0DD));
  AddObject('chartreuse', TObject($7FFF00));
  AddObject('red', TObject($FF0000));
  AddObject('lightblue', TObject($ADD8E6));
  AddObject('powderblue', TObject($B0E0E6));
  AddObject('darksalmon', TObject($E9967A));
  AddObject('gainsboro', TObject($DCDCDC));
  AddObject('lightskyblue', TObject($87CEFA));
  AddObject('orangered', TObject($FF4500));
  AddObject('lightcoral', TObject($F08080));
  AddObject('deeppink', TObject($FF1493));
  AddObject('paleturquoise', TObject($AFEEEE));
  AddObject('palegreen', TObject($98FB98));
  AddObject('aquamarine', TObject($7FFFD4));
  AddObject('violet', TObject($EE82EE));
  AddObject('greenyellow', TObject($ADFF2F));
  AddObject('sandybrown', TObject($F4A460));
  AddObject('fuchsia', TObject($FF00FF));
  AddObject('magenta', TObject($FF00FF));
  AddObject('tomato', TObject($FF6347));
  AddObject('darkorange', TObject($FF8C00));
  AddObject('salmon', TObject($FA8072));
  AddObject('orange', TObject($FFA500));
  AddObject('coral', TObject($FF7F50));
  AddObject('khaki', TObject($F0E68C));
  AddObject('palegoldenrod', TObject($EEE8AA));
  AddObject('hotpink', TObject($FF69B4));
  AddObject('gold', TObject($FFD700));
  AddObject('lightsalmon', TObject($FFA07A));
  AddObject('wheat', TObject($F5DEB3));
  AddObject('yellow', TObject($FFFF00));
  AddObject('beige', TObject($F5F5DC));
  AddObject('lavender', TObject($E6E6FA));
  AddObject('lightpink', TObject($FFB6C1));
  AddObject('whitesmoke', TObject($F5F5F5));
  AddObject('pink', TObject($FFC0CB));
  AddObject('antiquewhite', TObject($FAEBD7));
  AddObject('navajowhite', TObject($FFDEAD));
  AddObject('peachpuff', TObject($FFDAB9));
  AddObject('lightgoldenrodyellow', TObject($FAFAD2));
  AddObject('linen', TObject($FAF0E6));
  AddObject('moccasin', TObject($FFE4B5));
  AddObject('lightcyan', TObject($E0FFFF));
  AddObject('bisque', TObject($FFE4C4));
  AddObject('blanchedalmond', TObject($FFEBCD));
  AddObject('mistyrose', TObject($FFE4E1));
  AddObject('oldlace', TObject($FDF5E6));
  AddObject('honeydew', TObject($F0FFF0));
  AddObject('aliceblue', TObject($F0F8FF));
  AddObject('papayawhip', TObject($FFEFD5));
  AddObject('azure', TObject($F0FFFF));
  AddObject('lemonchiffon', TObject($FFFACD));
  AddObject('cornsilk', TObject($FFF8DC));
  AddObject('mintcream', TObject($F5FFFA));
  AddObject('ghostwhite', TObject($F8F8FF));
  AddObject('lavenderblush', TObject($FFF0F5));
  AddObject('seashell', TObject($FFF5EE));
  AddObject('lightyellow', TObject($FFFFE0));
  AddObject('floralwhite', TObject($FFFAF0));
  AddObject('ivory', TObject($FFFFF0));
  AddObject('snow', TObject($FFFAFA));
  AddObject('white', TObject($FFFFFF));
end;

function PS_RGB(Color: TColor): T_PS_RGB;
begin
  Result.R := (Color and $000000FF) / $000000FF;
  Result.G := (Color and $0000FF00) / $0000FF00;
  Result.B := (Color and $00FF0000) / $00FF0000;
end;

procedure MakeColorBox(ComboBox: TComboBox);
var
  I: Integer;
  ColorName: string;
begin
  ComboBox.Clear;
  ComboBox.Items.AddObject('Default', TObject(clDefault));
  ComboBox.Items.AddObject('Custom', TObject(clBlack));
  for I := 0 to TheHTMLColor.Count - 1 do
  begin
    ColorName := TheHTMLColor[I];
    if Pos('grey', ColorName) > 0 then Continue;
    if ColorName = 'cyan' then Continue;
    if ColorName = 'fuchsia' then Continue;
    ComboBox.Items.AddObject(ColorName,
      TObject(SwapColor(Integer(TheHTMLColor.Objects[I]))));
  end;
  ComboBox.ItemIndex := 0;
end;

{$IFDEF FPC}
procedure ColorBoxDrawItem(ComboBox: TComboBox;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  C0: TColor;
begin
  with ComboBox.Canvas do
  begin
    C0 := Brush.Color;
    FillRect(Rect);
    if (Index < 0) or (Index >= ComboBox.Items.Count - 1) then
      Exit;
    Brush.Color := Integer(ComboBox.Items.Objects[Index]);
    Rect.Left := Rect.Left + 1;
    Rect.Top := Rect.Top + 1;
    Rect.Bottom := Rect.Bottom - 1;
    Rect.Right := Rect.Left + ComboBox.Height - 7;
    if Brush.Color <> clDefault then
    begin
      FillRect(Rect);
      Brush.Color := C0;
    end
    else
    begin
      Brush.Color := C0;
      Pen.Color := clBlack;
      RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, 5,
        5);
    end;
    TextOut(Rect.Right + 3, Rect.Top, ComboBox.Items[Index]);
  end;
end;

procedure ColorBoxSelect(ComboBox: TComboBox);
var
  ColorDialog: TColorDialog;
  procedure Body;
  begin
    if ComboBox.Items.Count < 2 then Exit;
    ColorDialog.Color := Integer(ComboBox.Items.Objects[1]);
    if not ColorDialog.Execute then Exit;
    if ComboBox.Items.Count < 2 then Exit;
    ComboBox.Items.Objects[1] := TObject(ColorDialog.Color);
  end;
begin
  if ComboBox.ItemIndex <> 1 then Exit;
  ColorDialog := TColorDialog.Create(nil);
{$IFDEF VER140}
  if CustomColors <> '' then
    ColorDialog.CustomColors.Text := CustomColors;
{$ENDIF}
  Body;
{$IFDEF VER140}
  CustomColors := ColorDialog.CustomColors.Text;
{$ENDIF}
  ColorDialog.Free;
end;

procedure ColorBoxSet(ComboBox: TComboBox; Color: TColor);
var
  I: Integer;
begin
  if Color = clDefault then
  begin
    ComboBox.ItemIndex := 0;
    Exit;
  end;
  ComboBox.Items.Objects[1] := TObject(clBlack);
  if Color = clBlack then
  begin
    ComboBox.ItemIndex := 2;
    Exit;
  end;
  I := ComboBox.Items.IndexOfObject(TObject(Color));
  if I > 2 then
    ComboBox.ItemIndex := I
  else
  begin
    ComboBox.ItemIndex := 1;
  end;
  ComboBox.Items.Objects[1] := TObject(Color);
  ComboBox.Refresh;
end;

function ColorBoxGet(ComboBox: TComboBox): TColor;
begin
  Result := Integer(ComboBox.Items.Objects[ComboBox.ItemIndex])
end;
{$ENDIF}
//function LaBoiteACouleursLoadWindow: Integer; stdcall;
//  external 'C:\WRK\Delphi\TpX\-\LaBoiteACouleurs.dll'
//Name '_LoadWindow';

//function LaBoiteACouleursUnloadWindow: Integer; stdcall;
//  external 'C:\WRK\Delphi\TpX\-\LaBoiteACouleurs.dll'
//Name '_UnloadWindow';

//function LaBoiteACouleursSelectColorStr(const Str: PChar): Integer; stdcall;
//  external 'C:\WRK\Delphi\TpX\-\LaBoiteACouleurs.dll'
//Name '_SelectColorStr';

//function LaBoiteACouleursSelectColor(var AColorRef: COLORREF): Integer; stdcall;
//  external 'C:\WRK\Delphi\TpX\-\LaBoiteACouleurs.dll'
//Name '_SelectColor';

initialization
  TheHTMLColor := T_HTMLColor.Create;
  CustomColors := '';
finalization
  TheHTMLColor.Free;
end.

