unit MiscUtils;

{$IFNDEF VER140}
{$MODE Delphi}
{$ENDIF}

interface
uses Classes, SysUtils, StrUtils, TypInfo, Variants, Graphics;

function CSV_Item(const CSV: string; const I: Integer): string;
function CSV_Find(const CSV: string; const ID: string): Integer;
procedure ParseCmdLine(const CmdLine: string; OptList:
  TStringList);
function XmlReplaceChars(const St: string): string;
function XmlUnReplaceChars(const St: string): string;
procedure StoreObjectProp(Obj: TPersistent; Data: TStrings);
procedure LoadObjectProp(Obj: TPersistent; Data: TStrings);

type

  TSetOfChar = set of Char;

  TSimpleParser = class
  private
    fPos, fLen: Integer;
    pSource: PString;
  public
    procedure SetFinished;
    function Finished: Boolean;
    function ViewCh: Char;
    function PreViewCh(const I: Integer): Char;
    function PreViewAnyOpt(const Chars: TSetOfChar;
      var I: Integer): Boolean;
    function PreViewAnyMult(const Chars: TSetOfChar;
      var I: Integer): Integer;
    function GetCh: Char;
    procedure Take(const N: Integer);
    function TakeAnyOpt(const Chars: TSetOfChar): Boolean;
    function GetAnyOpt(const Chars: TSetOfChar;
      const Default: Char): Char;
    function GetAnyMult(const Chars: TSetOfChar): string;
    function SkipAnyMult(const Chars: TSetOfChar): Integer;
    //function GetAnyPlus(const Chars: TSetOfChar): string;
    //function GetAnyMult(const Chars: TSetOfChar): string;
    function ViewPrev(const N: Integer): string;
    function Get(const N: Integer): string;
    function GetNumStr: string;
    function GetNum(const Default: Extended): Extended;
    procedure SetSource(const pSource: PString);
  end;

{$I tpx.inc}

implementation

uses SysBasic;

function CSV_Item(const CSV: string; const I: Integer): string;
var
  Len, Pos1, Pos2, Curr: Integer;
begin
  Result := '';
  Pos1 := 0;
  Curr := 1;
  Len := Length(CSV);
  for Pos2 := 1 to Len + 1 do
    if (Pos2 > Len) or (CSV[Pos2] in [',', ';']) then
    begin
      if Curr = I then
      begin
        Result := Copy(CSV, Pos1 + 1, Pos2 - Pos1 - 1);
        Exit;
      end;
      Inc(Curr);
      Pos1 := Pos2;
    end;
end;

function CSV_Find(const CSV: string; const ID: string): Integer;
var
  Len, Pos1, Pos2, Curr: Integer;
begin
  Result := 0;
  Pos1 := 0;
  Curr := 1;
  Len := Length(CSV);
  for Pos2 := 1 to Len + 1 do
    if (Pos2 > Len) or (CSV[Pos2] in [',', ';']) then
    begin
      if Copy(CSV, Pos1 + 1, Pos2 - Pos1 - 1) = ID then
      begin
        Result := Curr;
        Exit;
      end;
      Inc(Curr);
      Pos1 := Pos2;
    end;
end;

procedure ParseCmdLine(const CmdLine: string; OptList:
  TStringList);
var
  Pos, Len: Integer;
  Ch: Char;
  FileName, Key, Value: string;
  function ViewCh: Char;
  begin
    if Pos <= Len then
      Result := CmdLine[Pos]
    else
      Result := #0;
  end;
  function Take: Char;
  begin
    Result := CmdLine[Pos];
    if Pos <= Len then Inc(Pos);
  end;
  procedure TakeWsp;
  begin
    while ViewCh in [#32, #9] do
      Take;
  end;
  function GetKey: string;
  var
    Short: Boolean;
  begin
    Ch := ViewCh;
    Result := '';
    if Ch <> '-' then Exit;
    Take;
    Ch := ViewCh;
    Short := Ch <> '-';
    while Ch = '-' do
    begin
      Take;
      Ch := ViewCh;
    end;
    if Short then
      Result := Take
    else
    begin
      Result := Take;
      while ViewCh in ['a'..'z', 'A'..'Z'] do
        Result := Result + Take;
    end;
    Result := LowerCase(Result);
  end;
  function GetValue: string;
  begin
    Result := '';
    if ViewCh = '"' then
    begin
      Take;
      while not (ViewCh in ['"', #0]) do
        Result := Result + Take;
      Take;
    end
    else
      while not (ViewCh in [#0, #32, #9]) do
        Result := Result + Take;
  end;
begin
  Pos := 1;
  Len := Length(CmdLine);
  FileName := '';
  TakeWsp;
  OptList.Add('$ExeName=' + GetValue);
  while ViewCh <> #0 do
  begin
    TakeWsp;
    Key := GetKey;
    TakeWsp;
    if ViewCh in ['=', ':'] then Take;
    TakeWsp;
    Value := GetValue;
    if Key = '' then
      FileName := FileName + Value
    else
      OptList.Add(Key + '=' + Value);
  end;
  OptList.Add('$FileName=' + FileName);
end;

function ReplaceChars(const St: string; const Chars: string;
  const ReplArr: array of string): string;
var
  I, Prev, Pos, LenChars: Integer;
begin
  Result := '';
  LenChars := Length(Chars);
  Prev := 1;
  for Pos := 1 to Length(St) do
  begin
    for I := 1 to LenChars do if St[Pos] = Chars[I] then Break;
    if I <= LenChars then
    begin
      Result := Result
        + Copy(St, Prev, Pos - Prev) + ReplArr[I - 1];
      Prev := Pos + 1;
    end;
  end;
  Result := Result + Copy(St, Prev, Pos - Prev);
end;

function XmlReplaceChars(const St: string): string;
const
  ReplArr: array[1..6] of string =
  ('&amp;', '&lt;', '&gt;', '&quot;', '&apos;', '&#9;');
begin
  Result := ReplaceChars(St, '&<>"'''#9, ReplArr);
end;

function XmlUnReplaceChars(const St: string): string;
//?? Slow
begin
  Result := St;
  Result := AnsiReplaceStr(Result, '&lt;', '<');
  Result := AnsiReplaceStr(Result, '&gt;', '>');
  Result := AnsiReplaceStr(Result, '&amp;', '&');
  Result := AnsiReplaceStr(Result, '&quot;', '"');
  Result := AnsiReplaceStr(Result, '&apos;', '''');
  Result := AnsiReplaceStr(Result, '&#9;', #9);
  Result := AnsiReplaceStr(Result, '&#10;', #10);
  Result := AnsiReplaceStr(Result, '&#13;', #13);
end;

     {*---- TSimpleParser ----*}

procedure TSimpleParser.SetFinished;
begin
  fPos := fLen + 1;
end;

function TSimpleParser.Finished: Boolean;
begin
  Result := fPos > fLen;
end;

function TSimpleParser.ViewCh: Char;
begin
  if fPos <= fLen then
    Result := pSource^[fPos]
  else
    Result := #0;
  Result := Chr((Ord(Result) + 1) - 1);
end;

function TSimpleParser.PreViewCh(const I: Integer): Char;
begin
  if fPos + I <= fLen then
    Result := pSource^[fPos + I]
  else
    Result := #0;
  Result := Chr((Ord(Result) + 1) - 1);
end;

function TSimpleParser.PreViewAnyOpt(const Chars: TSetOfChar;
  var I: Integer): Boolean;
begin
  if PreViewCh(I) in Chars then
  begin
    Result := True;
    Inc(I);
  end
  else
    Result := False;
end;

function TSimpleParser.PreViewAnyMult(const Chars: TSetOfChar;
  var I: Integer): Integer;
begin
  Result := I;
  while PreViewCh(I) in Chars {[#32, ',', #13, #10, #9]} do
    Inc(I);
  Result := I - Result;
end;

function TSimpleParser.GetCh: Char;
begin
  if fPos <= fLen then
    Result := pSource^[fPos]
  else
    Result := #0;
  Inc(fPos);
end;

procedure TSimpleParser.Take(const N: Integer);
begin
  Inc(fPos, N);
end;

function TSimpleParser.TakeAnyOpt(const Chars: TSetOfChar):
  Boolean;
begin
  if ViewCh in Chars then
  begin
    GetCh;
    Result := True;
  end
  else
    Result := False;
end;

function TSimpleParser.GetAnyOpt(const Chars: TSetOfChar;
  const Default: Char): Char;
begin
  if ViewCh in Chars then
    Result := GetCh
  else
    Result := Default;
end;

function TSimpleParser.GetAnyMult(const Chars: TSetOfChar): string;
var
  I: Integer;
begin
  I := 0;
  while PreViewCh(I) in Chars do
    Inc(I);
  Result := Get(I);
end;

function TSimpleParser.SkipAnyMult(const Chars: TSetOfChar):
  Integer;
begin
  Result := 0;
  PreViewAnyMult(Chars, Result);
  Take(Result);
end;

function TSimpleParser.ViewPrev(const N: Integer): string;
begin
  Result := Copy(pSource^, fPos - N, N);
end;

function TSimpleParser.Get(const N: Integer): string;
begin
  Result := Copy(pSource^, fPos, N);
  Take(N);
end;

function TSimpleParser.GetNumStr: string;
var
  N: Integer;
begin
  N := 0;
  Result := '';
  PreViewAnyOpt(['-', '+'], N);
  PreViewAnyMult(['0'..'9'], N);
  if PreViewAnyOpt(['.'], N) then
  begin
    PreViewAnyMult(['0'..'9'], N);
    if N = 1 then Exit;
  end;
  if N = 0 then Exit;
  if PreViewAnyOpt(['e', 'E'], N) then
  begin
    PreViewAnyOpt(['-', '+'], N);
    if PreViewAnyMult(['0'..'9'], N) = 0 then Exit;
  end;
  Result := Get(N);
end;

function TSimpleParser.GetNum(const Default: Extended): Extended;
begin
  Result := StrToFloatDef(GetNumStr, Default);
end;

procedure TSimpleParser.SetSource(const pSource: PString);
begin
  Self.pSource := pSource;
  fPos := 1;
  fLen := Length(pSource^);
end;

procedure StoreObjectProp(Obj: TPersistent; Data: TStrings);
var
  I, NProp: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  TypeKind: TTypeKind;
begin
  Data.Clear;
  NProp := GetTypeData(Obj.ClassInfo)^.PropCount;
  if NProp > 0 then
  begin
    GetMem(PropList, NProp * SizeOf(Pointer));
    try
      GetPropInfos(Obj.ClassInfo, PropList);
      for I := 0 to NProp - 1 do
      begin
        PropInfo := PropList^[I];
        if (PropInfo <> nil) then
        begin
          TypeKind := PropInfo.PropType^.Kind;
          if (not (TypeKind = tkMethod))
            and (not (TypeKind = tkClass)) then
            Data.Values[PropInfo.Name]
              := VarToStr(GetPropValue(Obj, PropInfo.Name, False));
        end;
      end;
    finally
      FreeMem(PropList, NProp * SizeOf(Pointer));
    end;
  end;
end;

procedure LoadObjectProp(Obj: TPersistent; Data: TStrings);
var
  I: Integer;
  PropName: ShortString;
  PropData: Variant;
begin
  for I := 0 to Data.Count - 1 do
  begin
    PropName := Data.Names[I];
    if IsPublishedProp(Obj, PropName) then
    begin
      PropData := VarAsType(Data.Values[PropName],
        VarType(GetPropValue(Obj, PropName, False)));
      SetPropValue(Obj, PropName, PropData);
    end;
  end;
end;

procedure QQQ;
var
  Font: TFont;
  Data: TStringList;
begin
  Font := TFont.Create;
  Data := TStringList.Create;
  StoreObjectProp(Font, Data);
//  MessageBoxInfo(Trim(Data.Text));
  //Data.Values['Name'] := 'Arial';
  LoadObjectProp(Font, Data);
  StoreObjectProp(Font, Data);
  MessageBoxInfo(Trim(Data.Text));
  Data.Free;
  Font.Free;
end;


initialization
//  QQQ;
end.

