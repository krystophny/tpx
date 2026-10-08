unit PlatformShortcuts;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
  Classes, Controls, Menus;

function StandardShortcut(Key: Word; Shift: TShiftState): TShortCut;

implementation

function StandardShortcut(Key: Word; Shift: TShiftState): TShortCut;
begin
{$IFDEF FPC}
  Include(Shift, ssModifier);
{$ELSE}
  Include(Shift, ssCtrl);
{$ENDIF}
  Result := Menus.ShortCut(Key, Shift);
end;

end.
