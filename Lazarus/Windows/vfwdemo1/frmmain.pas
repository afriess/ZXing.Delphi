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
  StdCtrls, ExtCtrls, Clipbrd, Buttons, Spin, fpimage,
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
    TScan: TTimer;
    pControl: TPanel;
    pCapture: TPanel;
    stCapture: TStaticText;
    procedure ScanClick(Sender: TObject);
    procedure TScanTimer(Sender: TObject);
    procedure VideoCreate(Sender: TObject);
    procedure VideoDestroy(Sender: TObject);
    procedure bConnectClick(Sender: TObject);
    procedure bFormatClick(Sender: TObject);
    procedure bQualityClick(Sender: TObject);
    procedure bSourceClick(Sender: TObject);
    procedure pCaptureResize(Sender: TObject);
  private
    procedure SaveTempFrame;
  public
    FScanManager : TScanManager;
    FScanInProgress : boolean;
    FFrameTake : byte;
    FLastSend : TDateTime;
    FLastImage : Array of Word;
    FTicks : Integer;
    FFrameFile : String;
    FBWFrameFile : String;
    FTempBMP   : TFPMemoryImage;
    FCapHandle:  THandle;             // Capture window handle
    FConnected:  Boolean;             // Driver connected
    FDriverCaps: TCapDriverCaps;      // Driver capabilities
    FLiveVideo:  Boolean;             // Live MainForm enabled
    FRecording:  Boolean;             // Recording MainForm
    FFileName:   string;              // AVI file name
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
  FPWriteBMP,
  dateutils;

(*
procedure MyBitmapLoadFromStream( bmp: Graphics.Tbitmap; stream: Tmemorystream );
var
  ScanlineBytes: integer;
begin
  stream.Seek(0, soFromBeginning);
  ScanlineBytes:=ABS( Integer( Bmp.Scanline[ 1 ] )-Integer( Bmp.Scanline[ 0 ] ) )*bmp.Height;
  stream.Read( bmp.Scanline[ bmp.Height - 1 ]^, ScanlineBytes );
end;

function VideoCap1FrameCallback(hWnd: HWND; lpVHdr: PVIDEOHDR): DWORD; stdcall;
var
  bmp: Tbitmap;
  stream : Tmemorystream;
  ReadResult: TReadResult;
begin
  bmp := Tbitmap.create;
  bmp.PixelFormat := pf24bit;
  bmp.width  := 640;
  bmp.height := 480;
  stream := Tmemorystream.create;
  try
    Stream.WriteBuffer(lpVhdr^.lpData^, lpVhdr^.dwBytesUsed );
    Stream.Position:=0;
    bmp.LoadFromStream(Stream);
    //MyBitmapLoadFromStream( bmp, stream );

    ReadResult := nil;
    try
      try
        ReadResult := MainForm.FScanManager.Scan(bmp);
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
   bmp.free;
   stream.free;
  end;
end;
*)

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
  FScanInProgress:=false;
  FFrameTake:=0;
  FLastSend:=0;
  FFrameFile:=ChangeFileExt(ParamStr(0),'.bmp');
  FBWFrameFile:=ChangeFileExt(ParamStr(0),'-bw.bmp');
  FTempBMP:=TFPMemoryImage.Create(0,0);
  CapCreate;
  CapConnect;
  CapEnableViewer;
  FFileName := 'Clip.avi';
end;


Procedure TMainForm.SaveTempFrame;

begin
  capGrabFrameNoStop(FCapHandle);
  capFileSaveDIB(FCapHandle,PChar(FFrameFile));
end;

procedure TMainForm.TScanTimer(Sender: TObject);
var
  scanBitmap: TBitmap;
  ReadResult: TReadResult;
  myBmpStream : TMemoryStream;
  myBmpWriter : TFPWriterBMP;
begin
  if (FScanInProgress) then exit;

  Inc(FTicks);

  //capSetCallbackOnFrame(FCapHandle,@VideoCap1FrameCallback);

  scanBitmap := TBitmap.Create;
  {
  capGrabFrameNoStop(FCapHandle);
  capEditCopy(FCapHandle);
  if Clipboard.HasFormat(CF_Bitmap) then
  begin
    scanBitmap.LoadFromClipboardFormat(CF_Bitmap);
  end;
  EmptyClipboard;

  if Clipboard.FindPictureFormatID = Windows.CF_BITMAP then
  begin
    OpenClipboard(FCapHandle); // Handle is my form's handle
    scanBitmap.Handle := HBITMAP(GetClipboardData(Windows.CF_BITMAP));
    CloseClipboard;
  end;
  }

  SaveTempFrame;
  FTempBMP.LoadFromFile(FFrameFile);

  myBmpStream := TMemoryStream.Create;
  myBmpWriter := TFPWriterBMP.Create;

  FTempBMP.SaveToStream(myBmpStream, myBmpWriter);
  myBmpStream.Position := 0;
  scanBitmap.LoadFromStream(myBmpStream, myBmpStream.Size);

  STCapture.Caption:=Format('Try %d - Scan:  w %d h %d',
                            [FTicks, scanBitmap.Width, scanBitmap.Height]);

  ReadResult := nil;

  try
    FScanInProgress := True;
    try
      ReadResult := FScanManager.Scan(scanBitmap);
    except
      on E: Exception do
      begin
        exit;
      end;
    end;

    if (ReadResult <> nil) then
    begin
      Memo1.Lines.Append(ReadResult.Text);
    end;

  finally
    ReadResult.Free;
    scanBitmap.Free;
    //myBmpStream.Free;
    //myBmpWriter.Free;
    FScanInProgress := false;
  end;

end;

procedure TMainForm.ScanClick(Sender: TObject);
begin
  With TScan do
    Enabled:=Not Enabled;
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

