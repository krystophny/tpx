unit PropertyEditCore;

{$mode delphi}{$H+}

interface

uses SysUtils, Variants;

type
  TPropertyValueState = (pvsUnavailable, pvsUniform, pvsMixed);

  TPropertyValue = record
    State: TPropertyValueState;
    Value: Variant;
  end;

  TPropertyReadAccumulator = record
    SupportedCount: Integer;
    UnavailableCount: Integer;
    HasValue: Boolean;
    Value: Variant;
    Mixed: Boolean;
  end;

  TNumericEditRange = (nerFinite, nerNonnegative, nerPositive, nerUnit);

procedure AddPropertyRead(var Accumulator: TPropertyReadAccumulator;
  Supported: Boolean; const Value: Variant);
function FinishPropertyRead(
  const Accumulator: TPropertyReadAccumulator): TPropertyValue;
function TryParseNumericEdit(const RawText: string;
  Range: TNumericEditRange; out Value: Double;
  out ErrorText: string): Boolean;

implementation

uses Math;

procedure AddPropertyRead(var Accumulator: TPropertyReadAccumulator;
  Supported: Boolean; const Value: Variant);
begin
  if not Supported then
  begin
    Inc(Accumulator.UnavailableCount);
    Exit;
  end;
  Inc(Accumulator.SupportedCount);
  if not Accumulator.HasValue then
  begin
    Accumulator.Value := Value;
    Accumulator.HasValue := True;
  end
  else if not VarSameValue(Accumulator.Value, Value) then
    Accumulator.Mixed := True;
end;

function FinishPropertyRead(
  const Accumulator: TPropertyReadAccumulator): TPropertyValue;
begin
  Result.Value := Unassigned;
  if (Accumulator.SupportedCount = 0) or
    (Accumulator.UnavailableCount > 0) then
  begin
    Result.State := pvsUnavailable;
    Exit;
  end;
  Result.Value := Accumulator.Value;
  if Accumulator.Mixed then
    Result.State := pvsMixed
  else
    Result.State := pvsUniform;
end;

function TryParseNumericEdit(const RawText: string;
  Range: TNumericEditRange; out Value: Double;
  out ErrorText: string): Boolean;
var Settings: TFormatSettings;
begin
  Value := 0;
  ErrorText := '';
  Settings := DefaultFormatSettings;
  Result := TryStrToFloat(Trim(RawText), Value, Settings) and
    not IsNan(Value) and not IsInfinite(Value);
  if not Result then
  begin
    ErrorText := 'Enter a finite number';
    Exit;
  end;
  case Range of
    nerNonnegative:
      if Value < 0 then
      begin
        ErrorText := 'Enter a nonnegative number';
        Result := False;
      end;
    nerPositive:
      if Value <= 0 then
      begin
        ErrorText := 'Enter a positive number';
        Result := False;
      end;
    nerUnit:
      if (Value < 0) or (Value > 1) then
      begin
        ErrorText := 'Enter a number from 0 to 1';
        Result := False;
      end;
  end;
end;

end.
