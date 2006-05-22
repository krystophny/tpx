unit Options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Grids, ValEdit, StdCtrls,
  ExtCtrls, ComCtrls, Options0;

type
  TOptionsForm = class(TForm)
    VLE: TValueListEditor;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    FontDialog1: TFontDialog;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose:
      Boolean);
    procedure VLESelectCell(Sender: TObject; ACol, ARow:
      Integer;
      var CanSelect: Boolean);
    procedure VLEGetPickList(Sender: TObject; const KeyName:
      string;
      Values: TStrings);
    procedure VLEEditButtonClick(Sender: TObject);
  private
    { Private declarations }
    fOptList: TOptionsList;
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses MainUnit, CADSys4;

{$R *.dfm}

procedure TOptionsForm.FormShow(Sender: TObject);
var
  Data: TOptionData;
  I: Integer;
begin
  VLE.Strings.Clear;
  fOptList := MainForm.TheDrawing.OptionsList;
  for I := 0 to fOptList.Count - 1 do
  begin
    Data := fOptList.Items[I] as TOptionData;
    VLE.InsertRow(Data.Key, Data.AsString, True);
    if Data.GetMask <> '' then
      VLE.ItemProps[I].EditMask := Data.GetMask;
    if Data is TFontNameOption then
      VLE.ItemProps[I].EditStyle := esEllipsis;
    if Data is TChoiceOption then
    begin
      VLE.ItemProps[I].EditStyle := esPickList;
      VLE.ItemProps[I].ReadOnly := True;
    end;
  end;
end;

procedure TOptionsForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  I: Integer;
  HasChanged: Boolean;
begin
  if ModalResult <> mrOK then Exit;
  //if VLE.RowCount-1 <> OptionsList.Count
  HasChanged := False;
  for I := 0 to fOptList.Count - 1 do
  try
    HasChanged := HasChanged or
      ((fOptList[I] as TOptionData).AsString <>
      VLE.Cells[1, I + 1]);
    (fOptList[I] as TOptionData).AsString :=
      VLE.Cells[1, I + 1];
  except
    VLE.SetFocus;
    VLE.Row := I + 1;
    CanClose := False;
    Exit;
  end;
  if HasChanged then
  begin
    MainForm.TheDrawing.History.SetPropertiesChanged;
    MainForm.SaveDoc.Enabled := MainForm.TheDrawing.History.IsChanged;
  end;
end;

procedure TOptionsForm.VLESelectCell(Sender: TObject; ACol,
  ARow: Integer;
  var CanSelect: Boolean);
begin
  Memo1.Text :=
    (fOptList[ARow - 1] as TOptionData).Hint;
  CanSelect := True;
end;

procedure TOptionsForm.VLEGetPickList(Sender: TObject;
  const KeyName: string; Values: TStrings);
begin
  if VLE.Row < 1 then Exit;
  if fOptList[VLE.Row - 1] is TChoiceOption then
  begin
    with fOptList[VLE.Row - 1] as TChoiceOption do
      Values.AddStrings(Choices);
  end;
end;

procedure TOptionsForm.VLEEditButtonClick(Sender: TObject);
begin
  if VLE.Row < 1 then Exit;
  if fOptList[VLE.Row - 1] is TFontNameOption then
    with fOptList[VLE.Row - 1] as TFontNameOption do
    begin
      FontDialog1.Font.Name := VLE.Cells[1, VLE.Row];
      if not FontDialog1.Execute then Exit;
      VLE.Cells[1, VLE.Row] := FontDialog1.Font.Name;
    end;
end;

end.

