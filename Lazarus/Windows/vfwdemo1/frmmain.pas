{
  MainForm Capturing and Processing Application

  Author: Bogdan Razvan Adrian
  The application itself can be used under Lazarus modified LGPL or MPL

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit frmmain;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$ifdef FPC}
  LResources,
  {$endif}
  {$ifdef MSWindows}Windows, VFW,{$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Clipbrd, Buttons, fpimage,
  ZXing.ReadResult,
  ZXing.BarCodeFormat,
  ZXing.ScanManager;

type

  { TMainForm }

  TMainForm = class(TForm)
    bSource: TButton;
    bFormat: TButton;
    bReconnect: TButton;
    bQuality: TButton;
    Memo1: TMemo;
    Scan: TButton;
    pControl: TPanel;
    pCapture: TPanel;
    stCapture: TStaticText;
    procedure ScanClick(Sender: TObject);
    procedure VideoCreate(Sender: TObject);
    procedure VideoDestroy(Sender: TObject);
    procedure bConnectClick(Sender: TObject);
    procedure bFormatClick(Sender: TObject);
    procedure bQualityClick(Sender: TObject);
    procedure bSourceClick(Sender: TObject);
    procedure pCaptureResize(Sender: TObject);
  public
    FScanManager : TScanManager;
    FCapHandle:  THandle;             // Capture window handle
    FConnected:  Boolean;             // Driver connected
    FDriverCaps: TCapDriverCaps;      // Driver capabilities
    FLiveVideo:  Boolean;             // Live MainForm enabled
    procedure CapCreate;              // Create capture window
    procedure CapConnect;             // Connect/Reconnect window + driver
    procedure CapEnableViewer;        // Start Live MainForm (Preview or Overlay)
    procedure CapDisableViewer;       // Stop Live MainForm
    procedure CapDisconnect;          // Disconnect driver
    procedure CapDestroy;             // Destroy capture window
    function CapCreated : Boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  UConvert,
  FPWriteBMP,
  dateutils;

function OnFrame(hCapWnd{%H-}: HWND; lpVHDR: PVideoHdr): DWord; stdcall;
var
  Bitmap: TBitmap;
  Bitmap24: TBitmap;
  BitmapInfo: TBitmapInfo;
  w, h: Integer;
  src, dest: Pointer;
  ReadResult: TReadResult;
begin
  Result:=1;
  FillChar(BitmapInfo{%H-}, SizeOf(BitmapInfo), 0);
  SendMessage(MainForm.FCapHandle, WM_CAP_GET_VIDEOFORMAT, SizeOf(BitmapInfo), Integer(@BitmapInfo));
  w:=bitmapinfo.bmiHeader.biWidth;
  h:=bitmapinfo.bmiHeader.biHeight;
  Bitmap:=TBitmap.Create;
  try
    Bitmap.PixelFormat:=pf32bit;
    Bitmap.SetSize(w,h);
    src:=lpVHdr^.lpData;
    dest:=Bitmap.ScanLine[0];
    with bitmapinfo.bmiHeader do CodecToARGB(src, dest, w, h, biBitCount, biCompression);
    Bitmap24:=TBitmap.Create;
    Bitmap24.PixelFormat:=pf24bit;
    Bitmap24.SetSize(w,h);
    src := dest;
    dest:=Bitmap24.ScanLine[0];
    Conv32To24(src, dest, w, h);
    ReadResult := nil;
    try
      try
        ReadResult := MainForm.FScanManager.Scan(Bitmap24);
      except
        on E: Exception do
        begin
          exit;
        end;
      end;
      if (ReadResult <> nil) then
      begin
        MainForm.Memo1.Lines.Append(ReadResult.Text);
      end;
    finally
      ReadResult.Free;
    end;
  finally
    Bitmap.free;
    Bitmap24.Free;
  end;
end;

procedure TMainForm.CapCreate;
begin
  CapDestroy;
  with pCapture do
    FCapHandle := capCreateCaptureWindow('Video Window',
      WS_CHILDWINDOW or WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS
      , 5, 5, Width-10, Height-10, Handle, 0);
  if Not CapCreated then
    stCapture.Caption := 'ERROR creating capture window !!!';
end;

procedure TMainForm.CapConnect;
begin
  if Not CapCreated then
    Exit;
   // Disconnect if necessary
  CapDisconnect;
  // Connect the Capture Driver
  FConnected:=capDriverConnect(FCapHandle, 0);
  if Not FConnected then
    stCapture.Caption := 'ERROR connecting capture driver !!!'
  else
    begin
    capDriverGetCaps(FCapHandle, @FDriverCaps, SizeOf(TCapDriverCaps));
    if FDriverCaps.fHasOverlay then
      stCapture.Caption := 'Driver connected, accepts overlay'
    else
      stCapture.Caption := 'Driver connected, software rendering';
    end
end;

procedure TMainForm.CapEnableViewer;

Var
  M : String;

begin
  FLiveVideo := False;
  if Not FConnected then
    Exit;
  capPreviewScale(FCapHandle, True);     // Allow stretching
  if FDriverCaps.fHasOverlay then        // Driver accepts overlay
    begin
    capPreviewRate(FCapHandle, 0);       // Overlay framerate is auto
    FLiveVideo:=capOverlay(FCapHandle,True);
    M:='Hardware';
    end
  else                                   // Driver doesn't accept overlay
    begin
    capPreviewRate(FCapHandle, 33);      // Preview framerate in ms/frame
    FLiveVideo:=capPreview(FCapHandle, True);
    M:='Software';
    end;
  if FLiveVideo then
    M:=Format('Video Capture - Preview (%s)',[M])
  else
    M:='ERROR configuring capture driver.';
  stCapture.Caption:=M;
end;

procedure TMainForm.CapDisableViewer;
begin
  if FLiveVideo then
    begin
    if FDriverCaps.fHasOverlay then
      capOverlay(FCapHandle, False)
    else
      capPreview(FCapHandle, False);
    FLiveVideo := False;
    end;
end;

procedure TMainForm.CapDisconnect;
begin
  if FConnected then
  begin
    capDriverDisconnect(FCapHandle);
    FConnected := False;
  end;
end;

procedure TMainForm.CapDestroy;
begin
  if CapCreated then
  begin
    DestroyWindow(FCapHandle);
    FCapHandle:=0;
  end;
end;

function TMainForm.CapCreated: Boolean;
begin
  Result:=(FCapHandle<>0);
end;

{ TMainForm }

procedure TMainForm.VideoCreate(Sender: TObject);
begin
  FScanManager:=TScanManager.Create(TBarcodeFormat.Auto, nil);
  CapCreate;
  CapConnect;
  CapEnableViewer;
end;

procedure TMainForm.ScanClick(Sender: TObject);
begin
  capSetCallbackOnFrame(FCapHandle,@OnFrame);
end;

procedure TMainForm.VideoDestroy(Sender: TObject);
begin
  CapDisableViewer;
  CapDisconnect;
  CapDestroy;
  FScanManager.Free;
end;

procedure TMainForm.bConnectClick(Sender: TObject);
begin
  CapConnect;
  CapEnableViewer;
end;

procedure TMainForm.bFormatClick(Sender: TObject);
begin
  capDlgVideoFormat(FCapHandle);
end;

procedure TMainForm.bQualityClick(Sender: TObject);
begin
  capDlgVideoCompression(FCapHandle);
end;

procedure TMainForm.bSourceClick(Sender: TObject);
begin
  capDlgVideoSource(FCapHandle);
end;

procedure TMainForm.pCaptureResize(Sender: TObject);
begin
  with MainForm.pCapture do
  if CapCreated then  // Adjust coordinates
    MoveWindow(FCapHandle, 5, 5, Width - 10, Height - 10, False);
end;

end.

