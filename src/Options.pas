unit Options;

interface

uses
  SysUtils, Forms, ComCtrls, Options0, ImgList, Dialogs,
{$IFDEF FPC}
  LResources, Buttons,
{$ENDIF}
  StdCtrls, Controls, ExtCtrls, Classes
  ;

type
  TOptionsForm = class(TForm)
    FontDialog1: TFontDialog;
    ImageList1: TImageList;
    ComboBox1: TComboBox;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel3: TPanel;
    TreeView1: TTreeView;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose:
      Boolean);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Changing(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1DropDown(Sender: TObject);
  private
    { Private declarations }
    CurrNode: TTreeNode;
  public
    { Public declarations }
    POptList: TOptionsList;
    HasChanged: Boolean;
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses Drawings;

{$IFDEF VER140}
{$R *.dfm}
{$ENDIF}

procedure TOptionsForm.FormShow(Sender: TObject);
var
  Data: TOptionData;
  I: Integer;
  ANode: TTreeNode;
begin
  HasChanged := False;
  //pOptList := MainForm.TheDrawing.OptionsList;
  TreeView1.Items.Clear;
  CurrNode := nil;
  for I := 0 to POptList.Count - 1 do
  begin
    Data := POptList.Items[I] as TOptionData;
    ANode := TreeView1.Items.Add(nil, Data.Key + '=' +
      Data.AsString);
    ANode.Data := POptList.Items[I];
  end;
end;

procedure TOptionsForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  I, J: Integer;
  Value: string;
begin
  if ModalResult <> mrOK then Exit;
  for I := 0 to TreeView1.Items.Count - 1 do
  try
    Value := TreeView1.Items[I].Text;
    J := Pos('=', Value);
    Value := Copy(Value, J + 1, Length(Value));
    if ((POptList[I] as TOptionData).AsString <> Value) then
    begin
      HasChanged := True;
      (POptList[I] as TOptionData).AsString := Value;
    end;
  except
    TreeView1.SetFocus;
{$IFDEF VER140}
    TreeView1.Select(TreeView1.Items[I]);
{$ELSE}
    TreeView1.Selected := TreeView1.Items[I];
{$ENDIF}
    CanClose := False;
    Exit;
  end;
end;

procedure TOptionsForm.TreeView1Change(Sender: TObject; Node:
  TTreeNode);
var
  Data: TOptionData;
  I: Integer;
  Value: string;
begin
  if Node = nil then Exit;
  if Node.Data = nil then Exit;
  I := Pos('=', Node.Text);
  if I = 0 then Exit;
  Value := Copy(Node.Text, I + 1, Length(Node.Text));
  Data := TOptionData(Node.Data);
  Memo1.Text := Data.Hint;
  ComboBox1.Items.Clear;
  if Data is TChoiceOption then
  begin
    ComboBox1.Style := csDropDownList;
    ComboBox1.Items.AddStrings((Data as TChoiceOption).Choices);
    ComboBox1.ItemIndex := (Data as
      TChoiceOption).Choices.IndexOf(Value);
  end
  else
  begin
    ComboBox1.Style := csDropDown;
    ComboBox1.Text := Value;
  end;
  CurrNode := Node;
  //TOptionData(Node.Data).AsString;
end;

procedure TOptionsForm.TreeView1Changing(Sender: TObject; Node:
  TTreeNode;
  var AllowChange: Boolean);
begin
  if Node = nil then Exit;
  AllowChange := Node.Data <> nil;
end;

procedure TOptionsForm.ComboBox1Change(Sender: TObject);
var
  I: Integer;
begin
  if CurrNode = nil then Exit;
  I := Pos('=', CurrNode.Text);
  if I = 0 then I := Length(CurrNode.Text) + 1;
  CurrNode.Text := Copy(CurrNode.Text, 1, I - 1)
    + '=' + ComboBox1.Text;
end;

procedure TOptionsForm.ComboBox1DropDown(Sender: TObject);
var
  Data: TOptionData;
begin
  if CurrNode = nil then Exit;
  if CurrNode.Data = nil then Exit;
  Data := TOptionData(CurrNode.Data);
  if Data is TFontNameOption then
  begin
    FontDialog1.Font.Name := ComboBox1.Text;
    if not FontDialog1.Execute then Exit;
    ComboBox1.Text := FontDialog1.Font.Name;
    ComboBox1.Refresh;
    ComboBox1Change(Sender);
  end;
  if Data is TFilePathOption then
  begin
    OpenDialog1.FileName := AnsiDequotedStr(ComboBox1.Text, '"');
    OpenDialog1.Filter := (Data as TFilePathOption).Filter;
    if not OpenDialog1.Execute then Exit;
    if Pos(' ', OpenDialog1.FileName) > 0 then
      OpenDialog1.FileName := AnsiQuotedStr(OpenDialog1.FileName,
        '"');
    ComboBox1.Text := AnsiDequotedStr(OpenDialog1.FileName, '"');
    ComboBox1.Refresh;
    ComboBox1Change(Sender);
  end;
end;

initialization
{$IFDEF FPC}
{$I Propert.lrs}
{$ELSE}
{$R *.dfm}
{$ENDIF}
end.
