unit GR32_ByteMaps;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
// $Id: GR32_ByteMaps.pas,v 1.3 2004/11/21 13:26:04 tpxproject Exp $

interface

{$I GR32.inc}

uses
  {$IFDEF CLX}
  Qt, Types, {$IFDEF LINUX}Libc, {$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  {$IFDEF CLX}
  QControls, QGraphics,
  {$ELSE}
  Controls, Graphics,
  {$ENDIF}
  GR32, GR32_Transforms;

type
  TConversionType = (ctRed, ctGreen, ctBlue, ctAlpha, ctUniformRGB, ctWeightedRGB);

  TByteMap = class(TCustomMap)
  private
    FBytes: TArrayOfByte;
    function GetValue(X, Y: Integer): Byte;
    function GetValPtr(X, Y: Integer): PByte;
    procedure SetValue(X, Y: Integer; Value: Byte);
  protected
    procedure AssignTo(Dst: TPersistent); override;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  Empty: Boolean; override;
    procedure Clear(FillValue: Byte);
    procedure ReadFrom(Source: TBitmap32; Conversion: TConversionType);
    procedure WriteTo(Dest: TBitmap32; Conversion: TConversionType); overload;
    procedure WriteTo(Dest: TBitmap32; const Palette: TPalette32); overload;
    property Bytes: TArrayOfByte read FBytes;
    property ValPtr[X, Y: Integer]: PByte read GetValPtr;
    property Value[X, Y: Integer]: Byte read GetValue write SetValue; default;
  end;

implementation

uses Math;

{ TByteMap }

procedure TByteMap.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TByteMap then
    begin
      inherited SetSize(TByteMap(Source).Width, TByteMap(Source).Height);
      FBytes := Copy(TByteMap(Source).Bytes, 0, Width * Height);
    end
    else if Source is TBitmap32 then
      ReadFrom(TBitmap32(Source), ctWeightedRGB)
    else
      inherited;
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TByteMap.AssignTo(Dst: TPersistent);
begin
  if Dst is TBitmap32 then WriteTo(TBitmap32(Dst), ctUniformRGB)
  else inherited;
end;

procedure TByteMap.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer);
begin
  SetLength(FBytes, NewWidth * NewHeight);
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TByteMap.Clear(FillValue: Byte);
begin
  FillChar(Bytes[0], Width * Height, FillValue);
  Changed;
end;

destructor TByteMap.Destroy;
begin
  FBytes := nil;
  inherited;
end;

function TByteMap.Empty: Boolean;
begin
  Result := not Assigned(Bytes);
end;

function TByteMap.GetValPtr(X, Y: Integer): PByte;
begin
  Result := @Bytes[X + Y * Width];
end;

function TByteMap.GetValue(X, Y: Integer): Byte;
begin
  Result := Bytes[X + Y * Width];
end;

procedure TByteMap.ReadFrom(Source: TBitmap32; Conversion: TConversionType);
var
  W, H, I, N: Integer;
  SrcC: PColor32;
  SrcB, DstB: PByte;
  Value: TColor32;
begin
  BeginUpdate;
  try
    SetSize(Source.Width, Source.Height);
    if Empty then Exit;

    W := Source.Width;
    H := Source.Height;
    N := W * H - 1;
    SrcC := Source.PixelPtr[0, 0];
    SrcB := Pointer(SrcC);
    DstB := @Bytes[0];
    case Conversion of

      ctRed:
        begin
          Inc(SrcB, 2);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;

      ctGreen:
        begin
          Inc(SrcB, 1);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;

      ctBlue:
        begin
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;

      ctAlpha:
        begin
          Inc(SrcB, 3);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;

      ctUniformRGB:
        begin
          for I := 0 to N do
          begin
            Value := SrcC^;
            Value := (Value and $00FF0000) shr 16 + (Value and $0000FF00) shr 8 +
              (Value and $000000FF);
            Value := Value div 3;
            DstB^ := Value;
            Inc(DstB);
            Inc(SrcC);
          end;
        end;

      ctWeightedRGB:
        begin
          for I := 0 to N do
          begin
            DstB^ := Intensity(SrcC^);
            Inc(DstB);
            Inc(SrcC);
          end;
        end;
    end;
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TByteMap.SetValue(X, Y: Integer; Value: Byte);
begin
  Bytes[X + Y * Width] := Value;
end;

procedure TByteMap.WriteTo(Dest: TBitmap32; Conversion: TConversionType);
var
  W, H, I, N: Integer;
  DstC: PColor32;
  DstB, SrcB: PByte;
  Resized: Boolean;
begin
  Dest.BeginUpdate;
  Resized := False;
  try
    Resized := Dest.SetSize(Width, Height);
    if Empty then Exit;

    W := Width;
    H := Height;
    N := W * H - 1;
    DstC := Dest.PixelPtr[0, 0];
    DstB := Pointer(DstC);
    SrcB := @Bytes[0];
    case Conversion of

      ctRed:
        begin
          Inc(DstB, 2);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;

      ctGreen:
        begin
          Inc(DstB, 1);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;

      ctBlue:
        begin
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;

      ctAlpha:
        begin
          Inc(DstB, 3);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;

      ctUniformRGB, ctWeightedRGB:
        begin
          for I := 0 to N do
          begin
            DstC^ := Gray32(SrcB^);
            Inc(DstC);
            Inc(SrcB);
          end;
        end;
    end;
  finally
    Dest.EndUpdate;
    Dest.Changed;
    if Resized then Dest.Resized;
  end;
end;

procedure TByteMap.WriteTo(Dest: TBitmap32; const Palette: TPalette32);
var
  W, H, I, N: Integer;
  DstC: PColor32;
  SrcB: PByte;
begin
  Dest.BeginUpdate;
  try
    Dest.SetSize(Width, Height);
    if Empty then Exit;

    W := Width;
    H := Height;
    N := W * H - 1;
    DstC := Dest.PixelPtr[0, 0];
    SrcB := @Bytes[0];

    for I := 0 to N do
    begin
      DstC^ := Palette[SrcB^];
      Inc(DstC);
      Inc(SrcB);
    end;
  finally
    Dest.EndUpdate;
    Dest.Changed;
  end;
end;

end.
