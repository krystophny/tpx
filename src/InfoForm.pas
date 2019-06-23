unit InfoForm;

interface

uses
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls
  {$IFDEF FPC}
  , LCLIntf, LCLType, LMessages
  {$ENDIF}
  ;
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

