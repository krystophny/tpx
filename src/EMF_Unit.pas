unit EMF_Unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
    Controls, Forms,
  Dialogs, ExtDlgs, ExtCtrls, StdCtrls, Clipbrd, CheckLst,
    ComCtrls,
  ActnList, Menus, ToolWin, Printers, Buttons, Spin, System.Actions;

type
  TFigPathKind = (fpk_Path, fpk_FileName, fpk_Dir_FileName);

  TEMF_Form = class(TForm)
    OpenPictureDialog1: TOpenPictureDialog;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    ActionList1: TActionList;
    Open: TAction;
    SaveAs: TAction;
    CloseWindow: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Saveas1: TMenuItem;
    Close1: TMenuItem;
    ClipboardCopy: TAction;
    ClipboardPaste: TAction;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    ToolBar1: TToolBar;
    N1: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    ToolButton6: TToolButton;
    Panel2: TPanel;
    ListView1: TListView;
    FitWindow: TAction;
    View1: TMenuItem;
    Fitwindow1: TMenuItem;
    ListEmfContents: TAction;
    EmfFromList: TAction;
    N2: TMenuItem;
    EMFfromlist1: TMenuItem;
    ListEMFcontents1: TMenuItem;
    ShowEmfPanel: TAction;
    ShowEMFpanel1: TMenuItem;
    PrintDialog1: TPrintDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    Panel3: TPanel;
    Button5: TButton;
    Button6: TButton;
    SpeedButton1: TSpeedButton;
    ProgressBar1: TProgressBar;
    SetPreferences: TAction;
    Preferences1: TMenuItem;
    SpinEdit1: TSpinEdit;
    ToolButton7: TToolButton;
    PrintAsEps: TAction;
    PrintasEPS1: TMenuItem;
    ToolButton8: TToolButton;
    procedure OpenExecute(Sender: TObject);
    procedure ClipboardCopyExecute(Sender: TObject);
    procedure ClipboardPasteExecute(Sender: TObject);
    procedure SaveAsExecute(Sender: TObject);
    procedure ListEmfContentsExecute(Sender: TObject);
    procedure FitWindowExecute(Sender: TObject);
    procedure EmfFromListExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CloseWindowExecute(Sender: TObject);
    procedure ShowEmfPanelExecute(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PrintAsEpsExecute(Sender: TObject);
    procedure SetPreferencesExecute(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Pic: TPicture;
    MF: TMetaFile;
    IsBitmap: Boolean;
    Resolution: Integer;
    FigPathKind: TFigPathKind;
    Scale: Double;
    MaxW, MaxH: Double;
    procedure ShowGraphic;
  end;

var
  EMF_Form: TEMF_Form;
  PostscriptPrinter: string = 'Acrobat Distiller';
  PostscriptPrinterUseOffset: Boolean = True;

implementation

uses MprtEMF, EMF_Add, MainUnit, Math, Imaging.pngimage, PrintEpsOpt;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

// Convert Graphic to EPS
// Graphic: Graphic (Metafile or bitmap) to be converted
// Resolution: printer resolution to be used
// FileName: fileName, where EPS is stored

procedure SetBoundingBox(const FileName: string;
  BB: TRect);
var
  F, FOut: TextFile;
  St: string;
begin
  if not FileExists(FileName) then Exit;
  AssignFile(F, FileName);
  AssignFile(FOut, ChangeFileExt(FileName, '.tmp'));
  FileMode := fmOpenReadWrite;
  Reset(F);
  Rewrite(FOut);
  while not EOF(F) do
  begin
    ReadLn(F, St);
    if Pos('%%BoundingBox:', St) > 0 then
      St := Format('%%%%BoundingBox: %d %d %d %d',
        [BB.Left, BB.Bottom, BB.Right, BB.Top]);
    WriteLn(FOut, St);
  end;
  CloseFile(FOut);
  CloseFile(F);
  DeleteFile(FileName);
  RenameFile(ChangeFileExt(FileName, '.tmp'), FileName);
end;

//Adapted from EmfToEps by Dirk Struve http://www.projectory.de/emftoeps/index.html

procedure GraphicToEps(const Graphic: TGraphic;
  const Resolution: Integer; const UseOffset: Boolean;
  const Scale, MaxW, MaxH: Double;
  const PostscriptPrinter: string; const FileName: string);
var
  PicWidth, picHeight: Integer;
  PhWidth, PhHeight: Integer;
  PhOffsetX, PhOffsetY: Integer;
  PxPerInch: Integer;
  Device,
    Driver,
    Port: PChar;
  deviceMode: THandle;
  pFileName: array[0..MAX_PATH] of Char;
  pPrinter: array[0..MAX_PATH] of Char;
  BB, Rect: TRect;
  DevMode: PDeviceMode;
  //in=72.27pt  in=25.4mm  mm=2.845pt
begin
  if FileExists(FileName) then
    if MessageDlg(Format('Overwrite %s?',
      [ExtractFileName(FileName)]),
      mtWarning, [mbOK, mbCancel], 0) <> mrOK then Exit;

  // set printer so that it prints to a file
  GetMem(Device, cchDeviceName);
  GetMem(Driver, MAX_PATH);
  GetMem(Port, MAX_PATH);
  Printer.GetPrinter(Device, Driver, Port, deviceMode);
  StrPCopy(pFileName, FileName);
  StrPCopy(pPrinter, PostscriptPrinter);

  PxPerInch := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  PhWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
    //Physical Width in device units (px)
  PhHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
    //Physical Height in device units (px)

  // recalculate picture dimension from 1/100mm to printer pixel units
  if Graphic is TMetaFile then
    if Resolution = 0 then
    begin //use printer resolution
      picHeight := MulDiv((Graphic as TMetaFile).MMHeight,
        PxPerInch, 2540);
      PicWidth := MulDiv((Graphic as TMetaFile).MMWidth, PxPerInch,
        2540);
    end
    else
    begin //use custom resolution
      picHeight := MulDiv((Graphic as TMetaFile).MMHeight,
        Resolution, 2540);
      PicWidth := MulDiv((Graphic as TMetaFile).MMWidth,
        Resolution, 2540);
    end
  else
  begin
    picHeight := Round(Graphic.Height * PxPerInch / 72.27);
    PicWidth := Round(Graphic.Width * PxPerInch / 72.27);
  end;
  picHeight := Round(picHeight * Scale);
  PicWidth := Round(PicWidth * Scale);

  if UseOffset then
  begin
    PhOffsetX := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
    PhOffsetY := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
  end
  else
  begin
    PhOffsetX := 0;
    PhOffsetY := 0;
  end;
    //BB is in pt
  BB.Left := Floor(PhOffsetX / PxPerInch * 72.27);
  BB.Bottom := Floor(PhOffsetY / PxPerInch * 72.27);

  BB.Right := Ceil((PhOffsetX + PicWidth) / PxPerInch * 72.27);
  BB.Top := Ceil((PhOffsetY + picHeight) / PxPerInch * 72.27);
    // in=72.27pt

  Printer.SetPrinter(pPrinter, Driver, pFileName, deviceMode);

  // print wmf to left lower corner of the page
  Printer.BeginDoc;
  Rect.Left := 0;
  Rect.Top := Printer.PageHeight - picHeight;
  Rect.Right := PicWidth;
  Rect.Bottom := Printer.PageHeight;
  if Graphic is TMetaFile then
  //if Scale <> 1 then
    Printer.Canvas.StretchDraw(Rect, Graphic)
  else Printer.Canvas.Draw(Rect.Left, Rect.Top, Graphic);
  {Printer.Canvas.MoveTo(Rect.Left, Rect.Top);
  Printer.Canvas.LineTo(Rect.Right, Rect.Bottom);}
  //PlayEnhMetaFile(Printer.Canvas.Handle, emf.Handle, Rect);
  Printer.EndDoc;
  SetBoundingBox(FileName, BB);
end;

procedure EMFtoPNG(const EMF: TMetaFile;
  const Resolution: Integer; const FileName: string);
var
  aBitmap: TBitmap;
  aPNG: TPNGObject;
begin
  aBitmap := TBitmap.Create;
  aBitmap.Width := MulDiv(EMF.MMWidth, Resolution, 2540);
  aBitmap.Height := MulDiv(EMF.MMHeight, Resolution, 2540);
  aBitmap.Canvas.StretchDraw(Rect(0, 0,
    aBitmap.Width, aBitmap.Height), EMF);
  //aBitmap.Canvas.Draw(0, 0, EMF);
  aPNG := TPNGObject.Create;
  try
    aPNG.Assign(aBitmap);
    aPNG.SaveToFile(ChangeFileExt(FileName, '.png'));
  finally
    aPNG.Free;
    aBitmap.Free;
  end;
end;

procedure TEMF_Form.FormCreate(Sender: TObject);
begin
  MF := TMetaFile.Create;
  Pic := TPicture.Create;
end;

procedure TEMF_Form.FormDestroy(Sender: TObject);
begin
  MF.Free;
  Pic.Free;
end;

procedure TEMF_Form.ShowGraphic;
var
  R: TRect;
begin
  if not IsBitmap then
  begin
    StatusBar1.Panels[0].Text := Format('Metafile: %g x %g (cm)',
      [Pic.MetaFile.MMWidth / 1000,
      Pic.MetaFile.MMHeight / 1000]);
  end
  else StatusBar1.Panels[0].Text := 'Bitmap: ';
  StatusBar1.Panels[0].Text := StatusBar1.Panels[0].Text +
    Format(' %d x %d (px)',
    [Pic.Width, Pic.Height]);
  Image1.Picture.MetaFile.Height := Round(Pic.Height *
    SpinEdit1.Value / 100);
  Image1.Picture.MetaFile.Width := Round(Pic.Width * SpinEdit1.Value
    / 100);
  with TMetaFileCanvas.Create(Image1.Picture.MetaFile, 0) do
  try
    StretchDraw(Rect(0, 0, Image1.Picture.MetaFile.Width - 1,
      Image1.Picture.MetaFile.Height - 1), Pic.Graphic);
  finally
    Free;
  end;
  //Image1.Picture.Assign(Pic);
  Scale := 1;
  MaxW := 500;
  MaxH := 500;
end;

procedure TEMF_Form.OpenExecute(Sender: TObject);
begin

  if not OpenPictureDialog1.Execute then Exit;
  IsBitmap := OpenPictureDialog1.FilterIndex = 2;
  Pic.LoadFromFile(OpenPictureDialog1.FileName);
  if not IsBitmap then
    Pic.MetaFile.Enhanced := True;
  ListView1.Clear;
  MF.Clear;
  ShowGraphic;
end;

procedure TEMF_Form.SaveAsExecute(Sender: TObject);
begin
  if Pic.Graphic = nil then Exit;
  if IsBitmap then
  begin
    SaveDialog1.DefaultExt := GraphicExtension(TBitmap);
    SaveDialog1.Filter := GraphicFilter(TBitmap);
  end
  else
  begin
    SaveDialog1.DefaultExt := GraphicExtension(TMetaFile);
    SaveDialog1.Filter := GraphicFilter(TMetaFile);
  end;
  if not SaveDialog1.Execute then Exit;
  Pic.SaveToFile(SaveDialog1.FileName);
end;

procedure TEMF_Form.ClipboardCopyExecute(Sender: TObject);
begin
  if Pic.Graphic <> nil then
    Clipboard.Assign(Pic.Graphic);
end;

procedure TEMF_Form.ClipboardPasteExecute(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_ENHMETAFILE) then
  begin
    Pic.MetaFile.Assign(Clipboard);
    IsBitmap := False;
  end
  else if Clipboard.HasFormat(CF_METAFILEPICT) then
  begin
    Pic.MetaFile.Assign(Clipboard);
    Pic.MetaFile.Enhanced := True;
    IsBitmap := False;
  end
  else if Clipboard.HasFormat(CF_BITMAP) or
    Clipboard.HasFormat(CF_DIB) then
  begin
    Pic.Bitmap.Assign(Clipboard);
    IsBitmap := True;
  end
  else Exit;
  ListView1.Clear;
  MF.Clear;
  ShowGraphic;
end;

procedure TEMF_Form.ListEmfContentsExecute(Sender: TObject);
var
  EMF_Struct: T_EMF_Structure;
  Stream: TMemoryStream;
  I: Integer;
  ARecord: TEMF_Record;
  ListItem: TListItem;
//  EMR_Info:TEMR_Info;
  iType: Byte;
begin
  Panel2.Visible := True;
  ShowEmfPanel.Checked := True;
  if IsBitmap then Exit;
  if Pic.Graphic = nil then Exit;
  if Pic.MetaFile = nil then Exit;
  MF.Assign(Pic.MetaFile);
  EMF_Struct := T_EMF_Structure.Create;
  Stream := TMemoryStream.Create;
  try
    Pic.MetaFile.SaveToStream(Stream);
    Stream.Seek(0, soFromBeginning);
    EMF_Struct.LoadFromStream(Stream);
    ListView1.Clear;
    for I := 0 to EMF_Struct.Count - 1 do
    begin
      ARecord := (EMF_Struct[I] as TEMF_Record);
      iType := ARecord.EMR_Info0.iType;
      ListItem := ListView1.Items.Add;
      ListItem.Caption := IntToStr(I) + ': ';
      if iType > 97
        then ListItem.Caption := ListItem.Caption +
        IntToStr(iType)
      else
        ListItem.Caption := ListItem.Caption
          + Copy(EMF_Struct[I].ClassName, 6,
          Length(EMF_Struct[I].ClassName));
      //if iType in [1, 14] then ListItem.Enabled := False;
      ListItem.Checked := True;
    end;
  finally
    EMF_Struct.Free;
    Stream.Free;
  end;
end;

procedure TEMF_Form.FitWindowExecute(Sender: TObject);
begin
  if FitWindow.Checked then
  begin
    Image1.Align := alClient;
    Image1.AutoSize := False;
    Image1.Proportional := True;
  end
  else
  begin
    Image1.Align := alNone;
    Image1.AutoSize := True;
    Image1.Proportional := False;
  end;
end;

procedure TEMF_Form.EmfFromListExecute(Sender: TObject);
var
  EMF_Struct: T_EMF_Structure;
  Stream, StreamOut: TMemoryStream;
  I: Integer;
  ARecord: TEMF_Record;
//  EMR_Info:TEMR_Info;
  nSize: Longword;
  OutHeader: T_EMFHeader;
begin
  if IsBitmap then Exit;
  if MF = nil then Exit;
  if ListView1.Items.Count = 0 then Exit;
  EMF_Struct := T_EMF_Structure.Create;
  Stream := TMemoryStream.Create;
  StreamOut := TMemoryStream.Create;
  try
    MF.SaveToStream(Stream);
    Stream.Seek(0, soFromBeginning);
    EMF_Struct.LoadFromStream(Stream);
    Stream.Seek(0, soFromBeginning);
    OutHeader := EMF_Struct.Header;
    OutHeader.Size := OutHeader.RecordSize;
    OutHeader.NumOfRecords := 1;
    for I := 0 to EMF_Struct.Count - 1 do
      if ListView1.Items[I].Checked then
      begin
        Inc(OutHeader.NumOfRecords);
        OutHeader.Size := OutHeader.Size +
          (EMF_Struct[I] as TEMF_Record).EMR_Info0.nSize;
      end;
    StreamOut.Write(OutHeader, OutHeader.RecordSize);
    Stream.Seek(OutHeader.RecordSize, soFromBeginning);
    OutHeader.Size := OutHeader.RecordSize;
    for I := 0 to EMF_Struct.Count - 1 do
    begin
      ARecord := (EMF_Struct[I] as TEMF_Record);
      nSize := ARecord.EMR_Info0.nSize;
      OutHeader.Size := OutHeader.Size + nSize;
      if ListView1.Items[I].Checked then
        StreamOut.CopyFrom(Stream, nSize)
      else Stream.Seek(nSize, soFromCurrent);
      MainForm.StatusBar1.Panels[0].Text
        := IntToStr(Stream.Position) + ' '
        + IntToStr(StreamOut.Position)
        + ' ' + IntToStr(OutHeader.Size);
    end;
    StreamOut.Seek(0, soFromBeginning);
    Pic.MetaFile.LoadFromStream(StreamOut);
  finally
    EMF_Struct.Free;
    Stream.Free;
    StreamOut.Free;
  end;
  ShowGraphic;
end;

procedure TEMF_Form.ListView1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
  Check: Boolean;
begin
  if Key = vk_Space then
  begin
    if ListView1.ItemIndex >= 0 then
      Check := not ListView1.Items[ListView1.ItemIndex].Checked
    else Check := False;
    for I := 0 to ListView1.Items.Count - 1 do
      if ListView1.Items[I].Selected
        {and not ListView1.Items[I].Focused }then
        ListView1.Items[I].Checked := Check;
  end;
end;

procedure TEMF_Form.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TEMF_Form.CloseWindowExecute(Sender: TObject);
begin
  Close;
end;

procedure TEMF_Form.ShowEmfPanelExecute(Sender: TObject);
begin
  Panel2.Visible := ShowEmfPanel.Checked;
end;

procedure TEMF_Form.SpeedButton1Click(Sender: TObject);
begin
  ShowEmfPanel.Checked := not ShowEmfPanel.Checked;
  ShowEmfPanelExecute(Self);
end;

procedure TEMF_Form.PrintAsEpsExecute(Sender: TObject);
var
  MF: TMetaFile;
  G: TGraphic;
begin
  if Pic.Graphic = nil then Exit;
  SaveDialog1.Filter := 'Encapsulated PostScript (EPS)|*.eps';
  SaveDialog1.DefaultExt := '*.eps';
  if not SaveDialog1.Execute then Exit;
  MF := TMetaFile.Create;
  try
    {if Pic.Graphic is TBitmap then
    begin
      MF.Width := Pic.Bitmap.Width;
      MF.Height := Pic.Bitmap.Height;
      with TMetaFileCanvas.Create(MF, 0) do
      try
        Draw(0, 0, Pic.Bitmap);
        G := MF;
      finally
        Free;
      end
    end
    else} G := Pic.Graphic;
    GraphicToEps(G, 0,
      PostscriptPrinterUseOffset,
      Scale, MaxW, MaxH, PostscriptPrinter,
      SaveDialog1.FileName);
  finally
    MF.Free;
  end;
end;

procedure TEMF_Form.SetPreferencesExecute(Sender: TObject);
begin
  PrintEpsOptForm.PrinterEdit.Text := PostscriptPrinter;
  //PrintEpsOptForm.ResolutionEdit.Text := IntToStr(Resolution);
  PrintEpsOptForm.UseOffsetBox.Checked :=
    PostscriptPrinterUseOffset;
  //PrintEpsOptForm.RadioGroup1.ItemIndex := Ord(FigPathKind);
  PrintEpsOptForm.SpinEdit1.Value := Round(Scale * 100);
  PrintEpsOptForm.Edit1.Text := Format('%.2f', [MaxW]);
  PrintEpsOptForm.Edit2.Text := Format('%.2f', [MaxH]);
  PrintEpsOptForm.ShowModal;

  if PrintEpsOptForm.ModalResult = mrOK then
  begin
    PostscriptPrinter := Trim(PrintEpsOptForm.PrinterEdit.Text);
    //Resolution := StrToInt(Trim(PrintEpsOptForm.ResolutionEdit.Text));
    PostscriptPrinterUseOffset :=
      PrintEpsOptForm.UseOffsetBox.Checked;
    //FigPathKind :=      TFigPathKind(PrintEpsOptForm.RadioGroup1.ItemIndex);
    Scale := PrintEpsOptForm.SpinEdit1.Value / 100;
    MaxW := StrToFloat(PrintEpsOptForm.Edit1.Text);
    MaxH := StrToFloat(PrintEpsOptForm.Edit2.Text);
  end;
end;

procedure TEMF_Form.SpinEdit1Change(Sender: TObject);
begin
  ShowGraphic;
end;

procedure TEMF_Form.FormShow(Sender: TObject);
begin
  ShowGraphic;
end;

end.
