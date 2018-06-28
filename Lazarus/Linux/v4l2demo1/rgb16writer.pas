{$mode objfpc}{$h+}
unit RGB16writer;

interface

uses FPImage, classes, sysutils, BMPComn;

type
  PFastBitmapPixelComponents16Bit = ^TFastBitmapPixelComponents16Bit;
  TFastBitmapPixelComponents16Bit = packed record
    Blue: 0..31; // 5 bits
    Green: 0..63; // 6 bits
    Red: 0..31; // 5 bits
  end;

  TFPWriterRGB16 = class (TFPCustomImageWriter)
  protected
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
  end;


implementation

constructor TFPWriterRGB16.create;
begin
  inherited create;
end;

procedure TFPWriterRGB16.InternalWrite (Stream:TStream; Img:TFPCustomImage);
var
  Row,Col,RowSize:Integer;
  aLine : PByte;
  tmpcol : TColorRGB;
begin
  RowSize:=Img.Width*2;
  GetMem(aLine,RowSize);
  try
    for Row:=0 to Img.Height-1 do
    begin
      for Col:=0 to img.Width-1 do
      begin
        with tmpcol,img.colors[Col,Row] do
        begin
          R:=(Red   and $FF00) shr 11;
          G:=(Green and $FF00) shr 10;
          B:=(Blue  and $FF00) shr 11;
        end;
        PWord(aline)[Col]:=( (tmpcol.R shl 11) or (tmpcol.G shl 5) or (tmpcol.B) );
      end;
      Stream.Write(aLine[0],RowSize);
     end;
  finally
    FreeMem(aLine);
  end;
end;

end.
