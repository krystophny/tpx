unit Options0;

interface
uses
{$IFDEF FPC}
Dialogs,
{$ENDIF}
{$IFNDEF NEXTGEN}
Contnrs,
{$ELSE}
System.Generics.Collections,
{$ENDIF}
SysUtils, Classes, StrUtils, md5;

{$I tpx.inc}

type

  THistoryList = class(TStringList)
  public
    HistoryCapacity: Integer;
    constructor Create;
    procedure Update(const S: string);
    procedure Remove(const S: string);
  end;

  TOptionData = class
    Key: string;
    Hint: string;
    PData: Pointer;
    constructor Create(Key0: string; PData0: Pointer; Hint0:
      string);
    procedure SetAsString(St: string); virtual;
    function GetAsString: string; virtual;
    function GetMask: string; virtual;
    property AsString: string read GetAsString write
      SetAsString;
    procedure SaveToStream(const AStream: TStream); virtual;
    procedure LoadFromStream(const AStream: TStream); virtual;
  end;

  TIntegerOption = class(TOptionData)
    MinValue, MaxValue: Integer;
    constructor Create(Key0: string; PData0: Pointer;
      MinValue0, MaxValue0: Integer;
      Hint0: string);
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
    function GetMask: string; override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream); override;
  end;

  TBooleanOption = class(TOptionData)
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream); override;
  end;

  TRealTypeOption = class(TOptionData)
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream); override;
  end;

  TStringOption0 = class(TOptionData)
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
  end;

  TStringOption = class(TStringOption0)
  end;

  TFilePathOption = class(TStringOption0)
    Filter: string;
    constructor Create(Key0: string; PData0: Pointer;
      Filter0: string; Hint0: string);
    procedure SetAsString(St: string); override;
  end;

  TFontNameOption = class(TStringOption0)
  end;

  TStringListOption = class(TStringOption0)
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream); override;
  end;

  TChoiceOption = class(TOptionData)
    Choices: TStringList;
    constructor Create(Key0: string; PData0: Pointer;
      const Choices0: string; Hint0: string);
    destructor Destroy; override;
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream); override;
  end;

{$IFDEF FPC}
  TCheckSum = TMD5Digest;
{$ELSE}
  TCheckSum = MD5Digest;
{$ENDIF}

{$IFNDEF NEXTGEN}
  TOptionsList = class(TObjectList)
{$ELSE}
  TOptionsList = class(TObjectList<TOptionData>)
{$ENDIF}
    constructor Create; overload;
    procedure AddInteger(Key0: string;
      PData0: Pointer; MinValue0, MaxValue0: Integer;
      Hint0: string);
    procedure AddRealType(Key0: string;
      PData0: Pointer; Hint0: string);
    procedure AddBoolean(Key0: string;
      PData0: Pointer; Hint0: string);
    procedure AddString(Key0: string;
      PData0: Pointer; Hint0: string);
    procedure AddFilePath(Key0: string;
      PData0: Pointer; Filter0: string; Hint0: string);
    procedure AddFontName(Key0: string;
      PData0: Pointer; Hint0: string);
    procedure AddStringList(Key0: string;
      List: TStrings; Hint0: string);
    procedure AddChoice(Key0: string;
      PData0: Pointer; const Choices0: string; Hint0: string);
    function GetCheckSum: TCheckSum;
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream);
    // HintsText is used to store hints information to a file
    // for subsequent use in TpX help
    function HintsText: string;
  end;

implementation

uses Geometry;

{ --================ THistoryList ==================-- }

constructor THistoryList.Create;
begin
  inherited Create;
  HistoryCapacity := 15;
  Delimiter := ';';
  QuoteChar := '"';
  CaseSensitive := False;
end;

procedure THistoryList.Update(const S: string);
begin
  Remove(S);
  while Count >= HistoryCapacity do Delete(HistoryCapacity - 1);
  Insert(0, S);
end;

procedure THistoryList.Remove(const S: string);
var
  J: Integer;
begin
  J := IndexOf(S);
  if J >= 0 then Delete(J);
end;

constructor TOptionData.Create(Key0: string;
  PData0: Pointer; Hint0: string);
begin
  inherited Create;
  Hint := Hint0;
  PData := PData0;
  Key := Key0;
end;

procedure TOptionData.SetAsString(St: string);
begin

end;

function TOptionData.GetAsString: string;
begin
  Result := '';
end;

function TOptionData.GetMask: string;
begin
  Result := '';
end;

procedure TOptionData.SaveToStream(const AStream: TStream);
var
  Len: Integer;
  Str: string;
begin
  Str := GetAsString;
  Len := Length(Str);
  AStream.Write(Len, SizeOf(Len));
  if Len > 0 then AStream.Write(Str[1], Len);
end;

procedure TOptionData.LoadFromStream(const AStream: TStream);
var
  Len: Integer;
  Str: string;
begin
  AStream.Read(Len, SizeOf(Len));
  if Len > 0 then
  begin
    SetLength(Str, Len);
    AStream.Read(Str[1], Len);
  end
  else Str := '';
  SetAsString(Str);
end;

constructor TIntegerOption.Create(Key0: string;
  PData0: Pointer; MinValue0, MaxValue0: Integer;
  Hint0: string);
begin
  inherited Create(Key0, PData0, Hint0);
  MinValue := MinValue0;
  MaxValue := MaxValue0;
end;

procedure TIntegerOption.SetAsString(St: string);
begin
  Integer(PData^) := StrToInt(St);
end;

function TIntegerOption.GetAsString: string;
begin
  Result := IntToStr(Integer(PData^));
end;

function TIntegerOption.GetMask: string;
begin
  Result := '';
  //Result := '99999999';
end;

procedure TIntegerOption.SaveToStream(const AStream: TStream);
begin
  AStream.Write(PData^, SizeOf(Integer));
end;

procedure TIntegerOption.LoadFromStream(const AStream: TStream);
begin
  AStream.Read(PData^, SizeOf(Integer));
end;

procedure TBooleanOption.SetAsString(St: string);
begin
  Boolean(PData^) := Trim(St) = '1';
end;

function TBooleanOption.GetAsString: string;
begin
  if Boolean(PData^) then Result := '1'
  else Result := '0';
end;

procedure TBooleanOption.SaveToStream(const AStream: TStream);
begin
  AStream.Write(PData^, SizeOf(Boolean));
end;

procedure TBooleanOption.LoadFromStream(const AStream: TStream);
begin
  AStream.Read(PData^, SizeOf(Boolean));
end;

procedure TRealTypeOption.SetAsString(St: string);
begin
  TRealType(PData^) := StrToRealType(St, 0);
end;

function TRealTypeOption.GetAsString: string;
begin
  Result := Format('%.5g', [TRealType(PData^)]);
end;

procedure TRealTypeOption.SaveToStream(const AStream: TStream);
begin
  AStream.Write(PData^, SizeOf(TRealType));
end;

procedure TRealTypeOption.LoadFromStream(const AStream: TStream);
begin
  AStream.Read(PData^, SizeOf(TRealType));
end;

procedure TStringOption0.SetAsString(St: string);
begin
  string(PData^) := St;
end;

function TStringOption0.GetAsString: string;
begin
  Result := string(PData^);
end;

constructor TFilePathOption.Create(Key0: string; PData0:
  Pointer; Filter0: string; Hint0: string);
begin
  inherited Create(Key0, PData0, Hint0);
  Filter := Filter0;
end;

procedure TFilePathOption.SetAsString(St: string);
begin
  inherited SetAsString(AnsiDequotedStr(Trim(St), '"'));
end;

procedure TStringListOption.SetAsString(St: string);
begin
  TStrings(PData).DelimitedText := St;
end;

function TStringListOption.GetAsString: string;
begin
  Result := TStrings(PData).DelimitedText;
end;

procedure TStringListOption.SaveToStream(const AStream: TStream);
begin
  TStrings(PData).SaveToStream(AStream);
end;

procedure TStringListOption.LoadFromStream(const AStream: TStream);
begin
  TStrings(PData).LoadFromStream(AStream);
end;

constructor TChoiceOption.Create(Key0: string; PData0: Pointer;
  const Choices0: string; Hint0: string);
begin
  inherited Create(Key0, PData0, Hint0);
  Choices := TStringList.Create;
  Choices.Delimiter := ';';
  Choices.DelimitedText := Choices0;
end;

destructor TChoiceOption.Destroy;
begin
  Choices.Free;
  inherited Destroy;
end;

procedure TChoiceOption.SetAsString(St: string);
begin
  Byte(PData^) := Choices.IndexOf(St);
end;

function TChoiceOption.GetAsString: string;
var
  I: Byte;
begin
  I := Byte(PData^);
  if I < Choices.Count then Result := Choices[Byte(PData^)]
  else Result := '';
end;

procedure TChoiceOption.SaveToStream(const AStream: TStream);
begin
  AStream.Write(PData^, SizeOf(Byte));
end;

procedure TChoiceOption.LoadFromStream(const AStream: TStream);
begin
  AStream.Read(PData^, SizeOf(Byte));
end;

constructor TOptionsList.Create;
begin
  inherited Create(True);
end;

procedure TOptionsList.AddInteger(Key0: string;
  PData0: Pointer; MinValue0, MaxValue0: Integer;
  Hint0: string);
begin
  Add(TIntegerOption.Create(Key0, PData0,
    MinValue0, MaxValue0, Hint0));
end;

procedure TOptionsList.AddRealType(Key0: string;
  PData0: Pointer; Hint0: string);
begin
  Add(TRealTypeOption.Create(Key0, PData0, Hint0));
end;

procedure TOptionsList.AddBoolean(Key0: string;
  PData0: Pointer; Hint0: string);
begin
  Add(TBooleanOption.Create(Key0, PData0, Hint0));
end;

procedure TOptionsList.AddString(Key0: string;
  PData0: Pointer; Hint0: string);
begin
  Add(TStringOption.Create(Key0, PData0, Hint0));
end;

procedure TOptionsList.AddFilePath(Key0: string;
  PData0: Pointer; Filter0: string; Hint0: string);
begin
{$IFDEF LINUX}
  Filter0 := AnsiReplaceStr(Filter0, '*.exe|*.exe', '*.*|*.*');
  Hint0 := AnsiReplaceStr(Hint0, '.exe', '');
{$ENDIF}
  Add(TFilePathOption.Create(Key0, PData0, Filter0, Hint0));
end;

procedure TOptionsList.AddFontName(Key0: string;
  PData0: Pointer; Hint0: string);
begin
  Add(TFontNameOption.Create(Key0, PData0, Hint0));
end;

procedure TOptionsList.AddStringList(Key0: string;
  List: TStrings; Hint0: string);
begin
  Add(TStringListOption.Create(Key0, List, Hint0));
end;

procedure TOptionsList.AddChoice(Key0: string;
  PData0: Pointer; const Choices0: string; Hint0: string);
begin
  Add(TChoiceOption.Create(Key0, PData0, Choices0, Hint0));
end;

function TOptionsList.GetCheckSum: TCheckSum;
var
  I: Integer;
{$IFNDEF FPC}
  Context: MD5Context;
{$ELSE}
  Context: TMD5Context;
{$ENDIF}
  St: string;
begin
  MD5Init(Context);
  for I := 0 to Count - 1 do
  begin
    St := (Items[I] as TOptionData).GetAsString;
{$IFNDEF FPC}
    MD5Update(Context, @St[1], Length(St));
{$ELSE}
    MD5Update(Context, St[1], Length(St));
{$ENDIF}
  end;
  MD5Final(Context, Result);
end;

procedure TOptionsList.SaveToStream(const AStream: TStream);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    (Items[I] as TOptionData).SaveToStream(AStream);
end;

procedure TOptionsList.LoadFromStream(const AStream: TStream);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    (Items[I] as TOptionData).LoadFromStream(AStream);
end;

function TOptionsList.HintsText: string;
var
  I: Integer;
  HintSt: string;
  procedure AddStr(St: string);
  begin
    Result := Result + St;
  end;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    AddStr('<li><b>');
    AddStr((Items[I] as TOptionData).Key);
    AddStr('</b>:');
    AddStr(EOL);
    HintSt := (Items[I] as TOptionData).Hint;
    HintSt := AnsiReplaceStr(HintSt, '<', '(<)');
    HintSt := AnsiReplaceStr(HintSt, '>', '(>)');
    HintSt := AnsiReplaceStr(HintSt, '"', '(")');
    HintSt := AnsiReplaceStr(HintSt, '&', '(&)');
    AddStr(HintSt);
    AddStr('</li>');
    AddStr(EOL);
    AddStr(EOL);
  end;
end;

end.

