{*******************************************************************************
 Project       : "YUV" ?
 Date          : 2005-10-26
 Version       : 1.1
 Compiler Ver. : Delphi 7 Enterprise
 Author        : Denny Koberling
 E-Mail        : JasonVoorhees2k@arcor.com
 Copyright     : Copyright (c) 2005 Denny Koberling

*******************************************************************************}

unit UConvert;

interface

uses windows;

const
  YUY2 = $32595559;
  UYVY = $59565955;
  I420 = $30323449;

type
  PARGB = ^TARGB;
  TARGB = packed record
    case Byte of
      1: (B, G, R, A: Byte);
      2: (Color: LongWord);
  end;

  PARGBArray = ^TARGBArray;
  TARGBArray = array[0..0] of TARGB;

  PRGB = ^TRGB;
  TRGB = packed record
    B, G, R: Byte;
  end;

  PYUV = ^TYUV;
  TYUV = packed record
    Y, U, V: Byte;
  end;

  PYUY2 = ^TYUY2;
  TYUY2 = packed record
    Y1, U, Y2, V: Byte;
  end;

  PUYVY = ^TUYVY;
  TUYVY = packed record
    U, Y1, V, Y2: Byte;
  end;

  function CodecToARGB(Src, Dest: Pointer; Width, Height: Integer; BitCount: Word; Codec: LongWord): Boolean;
  function ARGBToCodec(Src, Dest: Pointer; Width, Height: Integer; BitCount: Word; Codec: LongWord): Boolean;

  function Conv15To32(Color: Word): LongWord; overload;
  function Conv16To32(Color: Word): LongWord; overload;
  function Conv32To15(Color: LongWord): Word; overload;
  function Conv32To16(Color: LongWord): Word; overload;
  procedure Conv15To32(Src: PWord; Dest: PLongWord; Width, Height: Integer); overload;
  procedure Conv16To32(Src: PWord; Dest: PLongWord; Width, Height: Integer); overload;
  procedure Conv24To32(Src: PRGB; Dest: PARGB; Width, Height: Integer);
  procedure Conv32To15(Src: PLongWord; Dest: PWord; Width, Height: Integer); overload;
  procedure Conv32To16(Src: PLongWord; Dest: PWord; Width, Height: Integer); overload;
  procedure Conv32To24(Src: PARGB; Dest: PRGB; Width, Height: Integer);

  function ARGBtoYUV(Color: LongWord): TYUV;
  function YUVtoARGB(Y, U, V: Byte): TARGB;
  procedure UYVYtoARGB(Src: PUYVY; Dest: PARGB; Width, Height: Integer);
  procedure YUY2toARGB(Src: PYUY2; Dest: PARGB; Width, Height: Integer);
  procedure I420toARGB(Src: PByte; Dest: PARGB; Width, Height: Integer);
  procedure ARGBtoUYVY(Src: PARGB; Dest: PUYVY; Width, Height: Integer);
  procedure ARGBtoYUY2(Src: PARGB; Dest: PYUY2; Width, Height: Integer);
  procedure ARGBtoI420(Src: PARGB; Dest: PByte; Width, Height: Integer);

implementation

function CodecToARGB(Src, Dest: Pointer; Width, Height: Integer; BitCount: Word; Codec: LongWord): Boolean;
begin
  Result:=True;
  case Codec of
    BI_RGB: case BitCount of
              16: Conv16To32(src, dest, Width, Height);
              24: Conv24To32(src, dest, Width, Height);
              32: Move(src^, dest^, Width*Height*4);
            end;
    UYVY: UYVYtoARGB(src, dest, Width, Height);
    YUY2: YUY2toARGB(src, dest, Width, Height);
    I420: I420toARGB(src, dest, Width, Height);
  else
    Result:=False;
  end;
end;

function ARGBToCodec(Src, Dest: Pointer; Width, Height: Integer; BitCount: Word; Codec: LongWord): Boolean;
begin
  Result:=True;
  case Codec of
    BI_RGB: case BitCount of
              16: Conv32To16(src, dest, Width, Height);
              24: Conv32To24(src, dest, Width, Height);
              32: Move(src^, dest^, Width*Height*4);
            end;
    UYVY: ARGBtoUYVY(src, dest, Width, Height);
    YUY2: ARGBtoYUY2(src, dest, Width, Height);
    I420: ARGBtoI420(src, dest, Width, Height);
  else
    Result:=False;
  end;
end;

function Conv15To32(Color: Word): LongWord; overload;
begin
  Result:=(Color and $001F) shl 3 or
          (Color and $03E0) shl 8 or
          (Color and $7C00) shl 9;
end;

function Conv16To32(Color: Word): LongWord; overload;
begin
  Result:=(Color and $001F) shl 3 or
          (Color and $07E0) shl 5 or
          (Color and $F800) shl 8;
end;

function Conv32To15(Color: LongWord): Word; overload;
begin
  Result:=(Color and $000000F8 shr 3) or
          (Color and $0000F800 shr 5) or
          (Color and $00F80000 shr 9);
end;

function Conv32To16(Color: LongWord): Word; overload;
begin
  Result:=(Color and $000000F8 shr 3) or
          (Color and $0000FC00 shr 5) or
          (Color and $00F80000 shr 8);
end;

procedure Conv15To32(Src: PWord; Dest: PLongWord; Width, Height: Integer); overload;
var x: Integer;
begin
  for x:=1 to Width*Height do begin
    Dest^:=Conv15to32(Src^);
    Inc(Dest);
    Inc(Src);
  end;
end;

procedure Conv16To32(Src: PWord; Dest: PLongWord; Width, Height: Integer); overload;
var x: Integer;
begin
  for x:=1 to Width*Height do begin
    Dest^:=Conv16to32(Src^);
    Inc(Dest);
    Inc(Src);
  end;
end;

procedure Conv24To32(Src: PRGB; Dest: PARGB; Width, Height: Integer);
var x: Integer;
begin
  for x:=1 to Width*Height do begin
    Dest^.R:=Src^.R;
    Dest^.G:=Src^.G;
    Dest^.B:=Src^.B;
    Dest^.A:=0;
    Inc(Dest);
    Inc(Src);
  end;
end;

procedure Conv32To15(Src: PLongWord; Dest: PWord; Width, Height: Integer); overload;
var x: Integer;
begin
  for x:=1 to Width*Height do begin
    Dest^:=Conv32to15(Src^);
    Inc(Dest);
    Inc(Src);
  end;
end;

procedure Conv32To16(Src: PLongWord; Dest: PWord; Width, Height: Integer); overload;
var x: Integer;
begin
  for x:=1 to Width*Height do begin
    Dest^:=Conv32to16(Src^);
    Inc(Dest);
    Inc(Src);
  end;
end;

procedure Conv32To24(Src: PARGB; Dest: PRGB; Width, Height: Integer);
var x: Integer;
begin
  for x:=1 to Width*Height do begin
    Dest^.R:=Src^.R;
    Dest^.G:=Src^.G;
    Dest^.B:=Src^.B;
    Inc(Dest);
    Inc(Src);
  end;
end;

function ARGBtoYUV(Color: LongWord): TYUV;
begin
  with TARGB(Color), Result do begin
    Y:=( 66*R + 129*G +  25*B + 128) shr 8 + 16;
    U:=(-38*R -  74*G + 112*B + 128) shr 8 + 128;
    V:=(112*R -  94*G -  18*B + 128) shr 8 + 128;
  end;
end;

function YUVtoARGB(Y, U, V: Byte): TARGB;
var ValueY, ValueU, ValueV, ValueR, ValueG, ValueB: Integer;
begin
  ValueY:=Y-16;
  ValueU:=U-128;
  ValueV:=V-128;

  ValueR:=(298*ValueY              + 409*ValueV + 128) div 256;
  ValueG:=(298*ValueY - 100*ValueU - 208*ValueV + 128) div 256;
  ValueB:=(298*ValueY + 516*ValueU              + 128) div 256;

  with Result do begin
    if ValueR>255 then R:=255 else if ValueR<0 then R:=0 else R:=ValueR;
    if ValueG>255 then G:=255 else if ValueG<0 then G:=0 else G:=ValueG;
    if ValueB>255 then B:=255 else if ValueB<0 then B:=0 else B:=ValueB;
    A:=0;
  end;
end;

procedure UYVYtoARGB(Src: PUYVY; Dest: PARGB; Width, Height: Integer);
var x: Integer;
begin
  for x:=1 to Width*Height div 2 do begin
    with Src^ do begin
      Dest^:=YUVtoARGB(Y1, U, V);
      Inc(Dest);
      Dest^:=YUVtoARGB(Y2, U, V);
      Inc(Dest);
    end;
    Inc(Src);
  end;
end;

procedure YUY2toARGB(Src: PYUY2; Dest: PARGB; Width, Height: Integer);
var x: Integer;
begin
  for x:=1 to Width*Height div 2 do begin
    with Src^ do begin
      Dest^:=YUVtoARGB(Y1, U, V);
      Inc(Dest);
      Dest^:=YUVtoARGB(Y2, U, V);
      Inc(Dest);
    end;
    Inc(Src);
  end;
end;

procedure I420toARGB(Src: PByte; Dest: PARGB; Width, Height: Integer);
var
  x, y, n: Integer;
  pY, pU, pV: PByte;
begin
  pY:=src;
  pU:=PByte(Integer(Src)+Width*Height);
  pV:=PByte(Integer(pU)+Width*Height div 4);
  for y:=1 to Height div 2 do
    for n:=1 to 2 do begin
      for x:=1 to Width div 2 do begin
        Dest^:=YUVtoARGB(pY^, pU^, pV^);
        Inc(Dest);
        Inc(pY);
        Dest^:=YUVtoARGB(pY^, pU^, pV^);
        Inc(Dest);
        Inc(pY);
        Inc(pU);
        Inc(pV);
      end;
      if n=1 then begin
        Dec(pU, Width div 2);
        Dec(pV, Width div 2);
      end;
    end;
end;

procedure ARGBtoUYVY(Src: PARGB; Dest: PUYVY; Width, Height: Integer);
var
  YUV1, YUV2: TYUV;
  x: Integer;
begin
  for x:=1 to Width*Height div 2 do begin
    YUV1:=ARGBtoYUV(Src^.Color);
    Inc(Src);
    YUV2:=ARGBtoYUV(Src^.Color);
    Inc(Src);
    Dest^.U:=(YUV1.U+YUV2.U) div 2;
    Dest^.Y1:=YUV1.Y;
    Dest^.V:=(YUV1.V+YUV2.V) div 2;
    Dest^.Y2:=YUV2.Y;
    Inc(Dest);
  end;
end;

procedure ARGBtoYUY2(Src: PARGB; Dest: PYUY2; Width, Height: Integer);
var
  YUV1, YUV2: TYUV;
  x: Integer;
begin
  for x:=1 to Width*Height div 2 do begin
    YUV1:=ARGBtoYUV(Src^.Color);
    Inc(Src);
    YUV2:=ARGBtoYUV(Src^.Color);
    Inc(Src);
    Dest^.Y1:=YUV1.Y;
    Dest^.U:=(YUV1.U+YUV2.U) div 2;
    Dest^.Y2:=YUV2.Y;
    Dest^.V:=(YUV1.V+YUV2.V) div 2;
    Inc(Dest);
  end;
end;

procedure ARGBtoI420(Src: PARGB; Dest: PByte; Width, Height: Integer);
var
  x, y: Integer;
  YUV1, YUV2, YUV3, YUV4: TYUV;
  pY1, pY2, pU, pV: PByte;
  pRGB1, pRGB2: PARGB;
begin
  pRGB1:=Src;
  pRGB2:=PARGB(Integer(Src)+Width*4);
  pY1:=Dest;
  pY2:=PByte(Integer(Dest)+Width);
  pU:=PByte(Integer(Dest)+Width*Height);
  pV:=PByte(Integer(pU)+Width*Height div 4);
  for y:=1 to Height div 2 do begin
    for x:=1 to Width div 2 do begin
      YUV1:=ARGBtoYUV(pRGB1^.Color); Inc(pRGB1);
      YUV2:=ARGBtoYUV(pRGB1^.Color); Inc(pRGB1);
      pY1^:=YUV1.Y; Inc(pY1);
      pY1^:=YUV2.Y; Inc(pY1);
      YUV3:=ARGBtoYUV(pRGB2^.Color); Inc(pRGB2);
      YUV4:=ARGBtoYUV(pRGB2^.Color); Inc(pRGB2);
      pY2^:=YUV3.Y; Inc(pY2);
      pY2^:=YUV4.Y; Inc(pY2);
      pU^:=(YUV1.U+YUV2.U+YUV3.U+YUV4.U) div 4; Inc(pU);
      pV^:=(YUV1.V+YUV2.V+YUV3.V+YUV4.V) div 4; Inc(pV);
    end;
    Inc(pRGB1, Width);
    Inc(pRGB2, Width);
    Inc(pY1, Width);
    Inc(pY2, Width);
  end;
end;

end.
