unit FileWatchNative;

{$mode objfpc}{$H+}

interface

uses
  FileWatch;

implementation

{$IFDEF LINUX}
uses
  FileWatchLinux;
{$ENDIF}

end.
