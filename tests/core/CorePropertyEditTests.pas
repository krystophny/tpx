unit CorePropertyEditTests;

{$mode delphi}{$H+}

interface

implementation

uses SysUtils, PropertyEditCore, CoreTestSupport;

procedure TestPropertyReadStates;
var Acc: TPropertyReadAccumulator; Value: TPropertyValue;
begin
  Acc.SupportedCount := 0;
  Acc.UnavailableCount := 0;
  Acc.HasValue := False;
  Acc.Value := Unassigned;
  Acc.Mixed := False;
  AddPropertyRead(Acc, True, 3.25);
  AddPropertyRead(Acc, True, 3.25);
  Value := FinishPropertyRead(Acc);
  CheckCore((Value.State = pvsUniform) and (Value.Value = 3.25),
    'Equal supported values must read as uniform');

  Acc.SupportedCount := 0;
  Acc.UnavailableCount := 0;
  Acc.HasValue := False;
  Acc.Value := Unassigned;
  Acc.Mixed := False;
  AddPropertyRead(Acc, True, 1);
  AddPropertyRead(Acc, True, 2);
  Value := FinishPropertyRead(Acc);
  CheckCore(Value.State = pvsMixed,
    'Different supported values must read as mixed');

  Acc.SupportedCount := 0;
  Acc.UnavailableCount := 0;
  Acc.HasValue := False;
  Acc.Value := Unassigned;
  Acc.Mixed := False;
  AddPropertyRead(Acc, False, Unassigned);
  Value := FinishPropertyRead(Acc);
  CheckCore(Value.State = pvsUnavailable,
    'A field unsupported by all targets must be unavailable');

  Acc.SupportedCount := 0;
  Acc.UnavailableCount := 0;
  Acc.HasValue := False;
  Acc.Value := Unassigned;
  Acc.Mixed := False;
  AddPropertyRead(Acc, True, 'red');
  AddPropertyRead(Acc, False, Unassigned);
  Value := FinishPropertyRead(Acc);
  CheckCore(Value.State = pvsUnavailable,
    'A field unsupported by one target must be unavailable for the target set');
end;

procedure TestNumericEditValidation;
var Value: Double; ErrorText: string;
begin
  CheckCore(TryParseNumericEdit('0', nerNonnegative, Value, ErrorText) and
    (Value = 0), 'Zero must remain a valid nonnegative property value');
  CheckCore(not TryParseNumericEdit('0', nerPositive, Value, ErrorText),
    'Zero must not pass a positive text-height constraint');
  CheckCore(TryParseNumericEdit('-2.5', nerFinite, Value, ErrorText) and
    (Value = -2.5), 'Finite signed rotations must remain valid');
  CheckCore(not TryParseNumericEdit('-2.5', nerNonnegative, Value, ErrorText),
    'Negative dimensions must be rejected');
  CheckCore(not TryParseNumericEdit('1e', nerFinite, Value, ErrorText),
    'An incomplete exponent must remain an invalid raw draft');
  CheckCore(not TryParseNumericEdit('NaN', nerFinite, Value, ErrorText),
    'NaN must not pass finite-number validation');
  CheckCore(TryParseNumericEdit('0', nerUnit, Value, ErrorText) and
    TryParseNumericEdit('1', nerUnit, Value, ErrorText) and
    TryParseNumericEdit('0.35', nerUnit, Value, ErrorText) and
    (Abs(Value - 0.35) < 1E-12), 'Opacities from 0 to 1 must be valid');
  CheckCore(not TryParseNumericEdit('1.01', nerUnit, Value, ErrorText) and
    not TryParseNumericEdit('-0.1', nerUnit, Value, ErrorText),
    'Opacities outside 0 to 1 must be rejected');
end;

initialization
  RegisterCoreTest('property-edit-read-states', TestPropertyReadStates);
  RegisterCoreTest('property-edit-number-validation', TestNumericEditValidation);

end.
