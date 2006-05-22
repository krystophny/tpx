unit Arrows;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, StdCtrls, ComCtrls;

type
  TArrowsFrame = class(TFrame)
    ImageList1: TImageList;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    procedure ComboBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TArrowsFrame.ComboBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(Rect);
    TextOut(Rect.Left + 50, Rect.Top, ComboBox1.Items[Index]);
    Rect.Right := Rect.Left + 46;
    Brush.Color := clWhite;
    FillRect(Rect);
    ImageList1.Draw((Control as TComboBox).Canvas, Rect.Left, Rect.Top + 2, Index);
  end;
end;

end.

