unit InfoForm;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Forms, Controls,
  StdCtrls, Buttons, ExtCtrls;

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

