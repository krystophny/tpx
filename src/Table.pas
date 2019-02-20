unit Table;

interface

uses
{$IFNDEF FPC}
  Windows, Variants,
{$ELSE}
  LCLIntf, LResources, Buttons,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Spin, GObjects, ExtCtrls, ActnList,
  Menus,
  Clipbrd, StrUtils, ComCtrls;

{$IFDEF VER140}
{$ELSE}
const
  VK_DELETE = 46;
{$ENDIF}

type
  TTableForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Button3: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ActionList1: TActionList;
    DeletePoints: TAction;
    CheckBox1: TCheckBox;
    AddPoints: TAction;
    PopupMenu1: TPopupMenu;
    Addpoints1: TMenuItem;
    Deletepoints1: TMenuItem;
    Copy: TAction;
    Paste: TAction;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    SelectAll: TAction;
    Selectall1: TMenuItem;
    Panel2: TPanel;
    Grid: TStringGrid;
    Panel3: TPanel;
    Edit1: TEdit;
    AddFromClipboard: TAction;
    AddFromClipboard1: TMenuItem;
    Edit2: TEdit;
    UpDown1: TUpDown;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GridRowMoved(Sender: TObject; FromIndex, ToIndex:
      Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure DeletePointsExecute(Sender: TObject);
    procedure AddPointsExecute(Sender: TObject);
    procedure CopyExecute(Sender: TObject);
    procedure PasteExecute(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure Edit1Change(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure Edit1Enter(Sender: TObject);
    procedure GridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GridClick(Sender: TObject);
    procedure AddFromClipboardExecute(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure GridKeyPress(Sender: TObject; var Key: Char);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    function AddPoints0(N: Integer): Integer;
  public
    { Public declarations }
    PPrimitive: TPrimitive2D;
  end;

var
  TableForm: TTableForm;

implementation

uses Drawings, Geometry, SysBasic;

{$IFDEF VER140}
{$R *.dfm}
{$ENDIF}

procedure TTableForm.FormCreate(Sender: TObject);
begin
  Grid.Cells[1, 0] := 'x';
  Grid.Cells[2, 0] := 'y';
end;

procedure TTableForm.FormShow(Sender: TObject);
var
  I: Integer;
begin
  Grid.RowCount := PPrimitive.Points.Count + 1;
  for I := 0 to PPrimitive.Points.Count - 1 do
  begin
    Grid.Cells[0, I + 1] := IntToStr(I + 1);
    Grid.Cells[1, I + 1] := RealTypeToStr(PPrimitive.Points[I].X);
    Grid.Cells[2, I + 1] := RealTypeToStr(PPrimitive.Points[I].Y);
  end;
  Panel1.Visible := PPrimitive.Points.GrowingEnabled;
  DeletePoints.Enabled := PPrimitive.Points.GrowingEnabled;
  AddPoints.Enabled := PPrimitive.Points.GrowingEnabled;
  AddFromClipboard.Enabled := PPrimitive.Points.GrowingEnabled;
  Grid.SetFocus;
end;

procedure TTableForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
var
  I: Integer;
  Pts: array of TPoint2D;
begin
  if ModalResult <> mrOK then Exit;
  SetLength(Pts, Grid.RowCount - 1);
  for I := 0 to Grid.RowCount - 2 do
    Pts[I] := Point2D(StrToRealType(Trim(Grid.Cells[1, I + 1]), 0),
      StrToRealType(Trim(Grid.Cells[2, I + 1]), 0));
  PPrimitive.Points.Clear;
  PPrimitive.Points.AddPoints(Pts);
end;

procedure TTableForm.GridRowMoved(Sender: TObject; FromIndex,
  ToIndex: Integer);
var
  I: Integer;
begin
  if FromIndex > ToIndex then
  begin
    I := ToIndex;
    ToIndex := FromIndex;
    FromIndex := I;
  end;
  for I := FromIndex to ToIndex do
    Grid.Cells[0, I] := IntToStr(I);
end;

procedure TTableForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    Grid.Options := Grid.Options + [goEditing]
  else
    Grid.Options := Grid.Options - [goEditing];
end;

procedure TTableForm.DeletePointsExecute(Sender: TObject);
var
  I, S: Integer;
begin
  if not Grid.Focused then Exit;
  S := Grid.Selection.Bottom + 1 - Grid.Selection.Top;
  for I := Grid.Selection.Bottom + 1 to Grid.RowCount - 1 do
  begin
    Grid.Cells[1, I - S] := Grid.Cells[1, I];
    Grid.Cells[2, I - S] := Grid.Cells[2, I];
  end;
  if S < Grid.RowCount - 1 then
    Grid.RowCount := Grid.RowCount - S
  else
  begin
    Grid.RowCount := 2;
    Grid.Cells[1, 1] := '0';
    Grid.Cells[2, 1] := '0';
  end;
  Grid.Col := 1;
end;

function TTableForm.AddPoints0(N: Integer): Integer;
var
  I, RowCount0: Integer;
begin
  RowCount0 := Grid.RowCount;
  if RadioButton1.Checked then
    Result := Grid.Selection.Top
  else
    Result := Grid.Selection.Bottom + 1;
  Grid.RowCount := Grid.RowCount + N;
  for I := RowCount0 - 1 downto Result - 1 do
  begin
    Grid.Cells[1, I + N] := Grid.Cells[1, I];
    Grid.Cells[2, I + N] := Grid.Cells[2, I];
  end;
  for I := Result to Result + N - 1 do
  begin
    Grid.Cells[1, I] := '0';
    Grid.Cells[2, I] := '0';
  end;
  for I := RowCount0 - 1 to RowCount0 - 1 + N do
    Grid.Cells[0, I] := IntToStr(I);
end;

procedure TTableForm.AddPointsExecute(Sender: TObject);
var
  Top: Integer;
begin
  Top := AddPoints0(UpDown1.Position);
  if not RadioButton1.Checked then Grid.Row := Top;
end;

procedure TTableForm.CopyExecute(Sender: TObject);
var
  Stream: TStringStream;
  I, J: Integer;
  procedure Write(const Text: string);
  begin
    Stream.WriteBuffer(Text[1], Length(Text));
  end;
begin
  Stream := TStringStream.Create('');
  try
    for I := Grid.Selection.Top to Grid.Selection.Bottom do
    begin
      for J := Grid.Selection.Left to Grid.Selection.Right do
        if J = Grid.Selection.Left then
          Write(Grid.Cells[J, I])
        else
          Write(#9 + Grid.Cells[J, I]);
      Write(#13#10);
    end;
    if Stream.DataString <> '' then
      Clipboard.AsText := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TTableForm.PasteExecute(Sender: TObject);
var
  Strings, Line: TStringList;
  I, RowCount0: Integer;
begin
  Strings := TStringList.Create;
  Line := TStringList.Create;
  Line.Delimiter := #9;
  try
    Strings.Text := AnsiReplaceStr(Clipboard.AsText, ',', '.');
    if PPrimitive.Points.GrowingEnabled then
      if Grid.Row + Strings.Count > Grid.RowCount then
      begin
        RowCount0 := Grid.RowCount;
        Grid.RowCount := Grid.Row + Strings.Count;
        for I := RowCount0 to Grid.RowCount - 1 do
        begin
          Grid.Cells[0, I] := IntToStr(I);
          Grid.Cells[1, I] := '0';
          Grid.Cells[2, I] := '0';
        end;
      end;
    for I := 0 to Strings.Count - 1 do
    begin
      if Grid.Row + I >= Grid.RowCount then Break;
      Line.DelimitedText := Strings[I];
      if Line.Count >= 1 then
        Grid.Cells[Grid.Selection.Left,
          Grid.Row + I] := Line[0];
      if (Grid.Selection.Left = 1) and (Line.Count >= 2) then
        Grid.Cells[2, Grid.Row + I] := Line[1];
    end;
  finally
    Strings.Free;
    Line.Free;
  end;
end;

procedure TTableForm.AddFromClipboardExecute(Sender: TObject);
var
  Strings, Line: TStringList;
  I, Top: Integer;
begin
  if not PPrimitive.Points.GrowingEnabled then Exit;
  Strings := TStringList.Create;
  Line := TStringList.Create;
  Line.Delimiter := #9;
  try
    Strings.Text := AnsiReplaceStr(Clipboard.AsText, ',', '.');
    Top := AddPoints0(Strings.Count);
    for I := 0 to Strings.Count - 1 do
    begin
      Line.DelimitedText := Strings[I];
      if Line.Count >= 1 then
        Grid.Cells[1, Top + I] := Line[0]
      else
        Grid.Cells[1, Top + I] := '0';
      if Line.Count >= 2 then
        Grid.Cells[2, Top + I] := Line[1]
      else
        Grid.Cells[2, Top + I] := '0';
    end;
  finally
    Strings.Free;
    Line.Free;
  end;
  if not RadioButton1.Checked then Grid.Row := Top;
end;

procedure TTableForm.SelectAllExecute(Sender: TObject);
var
  R: TGridRect;
begin
  R.Left := 1;
  R.Right := 2;
  R.Top := 1;
  R.Bottom := Grid.RowCount - 1;
  Grid.Selection := R;
end;

procedure TTableForm.GridSelectCell(Sender: TObject; ACol, ARow:
  Integer;
  var CanSelect: Boolean);
begin
  Edit1.OnChange := nil;
  Edit1.Text := Grid.Cells[ACol, ARow];
  Edit1.OnChange := Edit1Change;
end;

procedure TTableForm.Edit1Change(Sender: TObject);
var
  I, J: Integer;
begin
  if not (Sender = Edit1) then Exit;
  Edit1.Text := Trim(Edit1.Text);
  if Edit1.Text = '' then Exit;
  I := Grid.Row;
  if I < 1 then I := 1;
  if I >= Grid.RowCount then I := 1;
  J := Grid.Col;
  if J < 1 then J := 1;
  Grid.Cells[J, I] := Edit1.Text;
end;

procedure TTableForm.GridSetEditText(Sender: TObject; ACol, ARow:
  Integer;
  const Value: string);
begin
  Edit1.OnChange := nil;
  Edit1.Text := Value;
  Edit1.OnChange := Edit1Change;
end;

procedure TTableForm.Edit1Enter(Sender: TObject);
begin
  Edit1.OnChange := nil;
  Edit1.Text := Grid.Cells[Grid.Col, Grid.Row];
  Edit1.OnChange := Edit1Change;
end;

procedure TTableForm.GridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    DeletePoints.Execute;
    Exit;
  end;
  if not CheckBox1.Checked and (Key = VK_F2) then
  begin
    Edit1.SetFocus;
    Exit;
  end;
end;

procedure TTableForm.GridClick(Sender: TObject);
begin
  Edit1.OnChange := nil;
  Edit1.Text := Grid.Cells[Grid.Col, Grid.Row];
  Edit1.OnChange := Edit1Change;
end;

procedure TTableForm.Button4Click(Sender: TObject);
var
  I, J: Integer;
  TmpStr: string;
begin
  for J := 1 to 2 do
    for I := 0 to (Grid.RowCount - 3) div 2 do
    begin
      TmpStr := Grid.Cells[J, I + 1];
      Grid.Cells[J, I + 1] := Grid.Cells[J, Grid.RowCount - I - 1];
      Grid.Cells[J, Grid.RowCount - I - 1] := TmpStr;
    end;
end;

procedure TTableForm.GridKeyPress(Sender: TObject; var Key: Char);
begin
  if CheckBox1.Checked then Exit;
  if Key in ['-', '0'..'9', '.', 'e', 'E'] then
  begin
//   Edit1.OnChange := nil;
    Edit1.Text := Key;
//  Edit1.OnChange := Edit1Change;
    Edit1.SetFocus;
    Edit1.SelLength := 0;
    Edit1.SelStart := 1;
  end;
end;

procedure TTableForm.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Grid.SetFocus;
    Exit;
  end;
{  if (Key = VK_DOWN) or (Key = VK_Up) then
  begin
    Grid.SetFocus;
    Grid.OnKeyUp(Sender, Key, Shift);
    Exit;
  end;}
end;

initialization
{$IFDEF FPC}
{$I Table.lrs}
{$ELSE}
{$R *.dfm}
{$ENDIF}
end.

