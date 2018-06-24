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
  StdCtrls, ExtCtrls, Clipbrd, Buttons, Spin, fpimage;

type

  { TMainForm }

  TMainForm = class(TForm)
    bSource: TButton;
    bFormat: TButton;
    bReconnect: TButton;
    bQuality: TButton;
    bRecord: TButton;
    BMotion: TButton;
    Label1: TLabel;
    Trigger: TLabel;
    SETreshold: TFloatSpinEdit;
    SETrigger: TFloatSpinEdit;
    pControl: TPanel;
    pCapture: TPanel;
    stCapture: TStaticText;
    TMotion: TTimer;
    procedure BMotionClick(Sender: TObject);
    procedure TMotionTimer(Sender: TObject);
    procedure VideoCreate(Sender: TObject);
    procedure VideoDestroy(Sender: TObject);
    procedure bConnectClick(Sender: TObject);
    procedure bFormatClick(Sender: TObject);
    procedure bQualityClick(Sender: TObject);
    procedure bRecordClick(Sender: TObject);
    procedure bSourceClick(Sender: TObject);
    procedure pCaptureResize(Sender: TObject);
  private
    function CheckDifferent: boolean;
    procedure SaveTempFrame;
  public
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
    procedure CapRecord;              // Start recording
    procedure CapStop;                // Stop recording
    procedure CapDisconnect;          // Disconnect driver
    procedure CapDestroy;             // Destroy capture window
    function CapCreated : Boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  ZXing.ReadResult,
  ZXing.BarCodeFormat,
  ZXing.ScanManager,
  dateutils;

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
  stCapture.Caption :=M
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

procedure TMainForm.CapRecord;
begin
  CapStop;
  CapDisableViewer;
  FFileName:=ExtractFilePath(Application.ExeName) + FormatDateTime('"Clip "[dd mm yyyy hh mm ss]".avi"', Now);
  stCapture.Caption:='Recording '+FFileName;
  bRecord.Caption := 'S&top';
  capFileSetCaptureFile(FCapHandle, PChar(FFileName));
  capCaptureSequence(FCapHandle);
  capFileSaveAs(FCapHandle, PChar(FFileName));
  FRecording := True;
end;

procedure TMainForm.CapStop;
begin
  if Not FRecording then
    Exit;
  FRecording := False;
  capCaptureStop(FCapHandle);
  RenameFile(FFileName, ChangeFileExt(FFileName, FormatDateTime(' - [dd mm yyyy hh mm ss]".avi"', Now)));
  CapEnableViewer;
  stCapture.Caption := 'Recording stopped';
  bRecord.Caption := '&Record';
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

function TMainForm.CheckDifferent : boolean;

Const
  MaxColor = Cardinal($FFFF);

Var
  A : Array of Word;
  R,C,I,PD,DC,TH,TC : Integer;
  D,MD: Int64;
  G : Word;
  P : TFPColor;

begin
  FTempBMP.LoadFromFile(FFrameFile);
  TC:=FTempBMP.Height*FTempBMP.Width;
  TH:=Round(MaxColor/100*SETreshold.Value);
  MD:=TC*MaxColor;
  Result:=Length(FLastImage)<>0;
  SetLength(A,TC);
  I:=0;
  D:=0;
  dc:=0;
  For R:=0 to FTempBMP.Height-1 do
    For C:=0 to FTempBMP.Width-1 do
      begin
      P:=FTempBMP.Colors[C,R];
      G:=(P.blue+P.red+P.Green) div 3;
      P.Blue:=G;
      P.Red:=G;
      P.Green:=G;
      FTempBMP.Colors[C,R]:=P;
      A[i]:=G;
      if (I<Length(FLastImage)) then
        begin
        PD:=Abs(G-FLastImage[i]);
        If (PD>TH) then
          begin
          inc(DC);
          D:=D+Abs(PD);
          end;
        end;
      Inc(i);
      end;
  FLastImage:=A;
  STCapture.Caption:=Format('Try %d - Color:  %d (%f %%) Pixels: %d/%d (%f %%)',
                            [FTicks, D, D/MD*100, DC, TC, DC/TC*100]);
  if Result then
    begin
    Result:=(D/MD*100)>SETrigger.Value;
    if Result then
      FTempBMP.SaveToFile(FBWFrameFile);
    end;
end;


procedure TMainForm.TMotionTimer(Sender: TObject);

begin
  Inc(FTicks);
  SaveTempFrame;
  if CheckDifferent then
    begin
    If MinutesBetween(Now,FLastSend)>1 then
      begin
      FLastSend:=Now;
      end;
    end;
end;

procedure TMainForm.BMotionClick(Sender: TObject);

begin
  With TMotion do
    Enabled:=Not Enabled;
end;

procedure TMainForm.VideoDestroy(Sender: TObject);
begin
  CapDisableViewer;
  CapDisconnect;
  CapDestroy;
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

procedure TMainForm.bRecordClick(Sender: TObject);
begin
  if FRecording then
    CapStop
  else
    CapRecord;
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

