unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure GetPicture(OutputStream : TMemoryStream);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.GetPicture(OutputStream : TMemoryStream);
var
   BitmapImage : TBitmap;
   JPEGImage   : TJPEGImage;
   PictureFile : String;
begin
     if hWndC <> 0 then
     begin
          PictureFile := 'snapshot.bmp';
          // grab frame
          SendMessage( hWndC, WM_CAP_GRAB_FRAME, 0, 0);
          // save bmp
          SendMessage( hWndC,
                       WM_CAP_SAVEDIB,
                       0,
                       longint(pchar(PictureFile)));

     end
     else
     begin
          // a bitmap for a case you cannot grab a frame
          PictureFile := 'webcamtrouble.bmp';
     end;

     BitmapImage := TBitmap.Create;
     with BitmapImage, BitmapImage.Canvas do
     begin
          LoadFromFile(PictureFile);
          // date and time
          TextOut(10, 10, DateToStr(Now)
                          + '  '
                          + TimeToStr(Now)
                          + ' EMT');
          // a message
          TextOut(10, 30, ' ' + Edit1.Text + ' ');
          Edit1.Text := ''; // message sent
     end;

     { Assign bitmap to jpeg }
     JPEGImage := TJPEGImage.Create;
     with JPEGImage do
     begin
          Assign(BitmapImage);
          CompressionQuality := 50;
          SaveToStream(OutputStream);
          Free;
     end;

     BitmapImage.Free;
end;

end.

