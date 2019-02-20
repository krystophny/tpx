unit InfoForm;

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
  {$ELSE}
  SysUtils, Classes, Graphics, Forms, Controls,
  StdCtrls, Buttons, ExtCtrls;
  {$ENDIF}
type
  TInfoBox = class(TForm)
    OKButton: TButton;
    InfoEdit: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InfoBox: TInfoBox;

implementation

{$R *.lfm}

end.

