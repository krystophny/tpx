unit Options0;

interface
uses SysUtils, Contnrs, Classes, StrUtils, Dialogs, md5;

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
  end;

  TIntegerOption = class(TOptionData)
    MinValue, MaxValue: Integer;
    constructor Create(Key0: string; PData0: Pointer;
      MinValue0, MaxValue0: Integer;
      Hint0: string);
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
    function GetMask: string; override;
  end;

  TBooleanOption = class(TOptionData)
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
  end;

  TRealTypeOption = class(TOptionData)
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
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
  end;

  TFontNameOption = class(TStringOption0)
  end;

  TStringListOption = class(TStringOption0)
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
  end;

  TChoiceOption = class(TOptionData)
    Choices: TStringList;
    constructor Create(Key0: string; PData0: Pointer;
      const Choices0: string; Hint0: string);
    destructor Destroy; override;
    procedure SetAsString(St: string); override;
    function GetAsString: string; override;
  end;

  TCheckSum = MD5Digest;

  TOptionsList = class(TObjectList)
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
//ShowMessage(S);
  J := IndexOf(S);
//ShowMessage(IntToStr(J));
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

procedure TBooleanOption.SetAsString(St: string);
begin
  Boolean(PData^) := Trim(St) = '1';
end;

function TBooleanOption.GetAsString: string;
begin
  if Boolean(PData^) then Result := '1'
  else Result := '0';
end;

procedure TRealTypeOption.SetAsString(St: string);
begin
  TRealType(PData^) := StrToRealType(St);
end;

function TRealTypeOption.GetAsString: string;
begin
  //Result := FloatToStr(TRealType(PData^));
  Result := Format('%.5g', [TRealType(PData^)]);
  {if DecimalSeparator <> '.' then
    Result := AnsiReplaceStr(Result,
      DecimalSeparator, '.');}
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

procedure TStringListOption.SetAsString(St: string);
begin
  TStrings(PData).DelimitedText := St;
end;

function TStringListOption.GetAsString: string;
begin
  Result := TStrings(PData).DelimitedText;
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
  Context: MD5Context;
  St: string;
begin
  MD5Init(Context);
  for I := 0 to Count - 1 do
  begin
    St := (Items[I] as TOptionData).GetAsString;
    MD5Update(Context, @St[1], Length(St));
  end;
  MD5Final(Context, Result);
end;

end.

