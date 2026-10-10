unit PropertyImages;

{$mode delphi}{$H+}

interface

uses
  Classes, Forms, Controls, ImgList;

type
  TPropertyImages = class(TDataModule)
    ArrImageList: TImageList;
    StarsImageList: TImageList;
  end;

implementation

{$IFDEF FPC}
uses LResources;
{$ENDIF}

{$IFDEF FPC}
initialization
  {$I PropertyImages.lrs}
{$ENDIF}
end.
