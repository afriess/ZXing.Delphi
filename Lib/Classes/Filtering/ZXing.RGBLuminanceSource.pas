{
  * Copyright 2012 ZXing authors
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *      http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.

  * Original Author: dswitkin@google.com (Daniel Switkin)
  * Delphi Implementation by E.Spelt and K. Gossens
}

unit ZXing.RGBLuminanceSource;

{$IFDEF FPC}
  {$mode delphi}{$H+}
  //{$mode advancedrecords}
{$ENDIF}

interface

uses
  {$ifndef FPC}System.{$endif}SysUtils,
  {$ifndef FPC}System.{$endif}UITypes,
  {$ifndef FPC}System.{$endif}TypInfo,
  {$ifndef FPC}
{$IFDEF USE_VCL_BITMAP}
  Winapi.Windows,
  VCL.Graphics,
{$ELSE}
  FMX.Graphics,
{$ENDIF}
  {$else}
  Graphics,
  {$endif}
  ZXing.LuminanceSource,
  ZXing.BaseLuminanceSource,
  ZXing.Common.Detector.MathUtils;

type
  TAlphaColor = cardinal;

  TAlphaColorRec = record
    const
      Alpha = TAlphaColor($FF000000);
      Aliceblue = Alpha or TAlphaColor($F0F8FF);
      Antiquewhite = Alpha or TAlphaColor($FAEBD7);
      Aqua = Alpha or TAlphaColor($00FFFF);
      Aquamarine = Alpha or TAlphaColor($7FFFD4);
      Azure = Alpha or TAlphaColor($F0FFFF);
      Beige = Alpha or TAlphaColor($F5F5DC);
      Bisque = Alpha or TAlphaColor($FFE4C4);
      Black = Alpha or TAlphaColor($000000);
      Blanchedalmond = Alpha or TAlphaColor($FFEBCD);
      Blue = Alpha or TAlphaColor($0000FF);
      Blueviolet = Alpha or TAlphaColor($8A2BE2);
      Brown = Alpha or TAlphaColor($A52A2A);
      Burlywood = Alpha or TAlphaColor($DEB887);
      Cadetblue = Alpha or TAlphaColor($5F9EA0);
      Chartreuse = Alpha or TAlphaColor($7FFF00);
      Chocolate = Alpha or TAlphaColor($D2691E);
      Coral = Alpha or TAlphaColor($FF7F50);
      Cornflowerblue = Alpha or TAlphaColor($6495ED);
      Cornsilk = Alpha or TAlphaColor($FFF8DC);
      Crimson = Alpha or TAlphaColor($DC143C);
      Cyan = Alpha or TAlphaColor($00FFFF);
      Darkblue = Alpha or TAlphaColor($00008B);
      Darkcyan = Alpha or TAlphaColor($008B8B);
      Darkgoldenrod = Alpha or TAlphaColor($B8860B);
      Darkgray = Alpha or TAlphaColor($A9A9A9);
      Darkgreen = Alpha or TAlphaColor($006400);
      Darkgrey = Alpha or TAlphaColor($A9A9A9);
      Darkkhaki = Alpha or TAlphaColor($BDB76B);
      Darkmagenta = Alpha or TAlphaColor($8B008B);
      Darkolivegreen = Alpha or TAlphaColor($556B2F);
      Darkorange = Alpha or TAlphaColor($FF8C00);
      Darkorchid = Alpha or TAlphaColor($9932CC);
      Darkred = Alpha or TAlphaColor($8B0000);
      Darksalmon = Alpha or TAlphaColor($E9967A);
      Darkseagreen = Alpha or TAlphaColor($8FBC8F);
      Darkslateblue = Alpha or TAlphaColor($483D8B);
      Darkslategray = Alpha or TAlphaColor($2F4F4F);
      Darkslategrey = Alpha or TAlphaColor($2F4F4F);
      Darkturquoise = Alpha or TAlphaColor($00CED1);
      Darkviolet = Alpha or TAlphaColor($9400D3);
      Deeppink = Alpha or TAlphaColor($FF1493);
      Deepskyblue = Alpha or TAlphaColor($00BFFF);
      Dimgray = Alpha or TAlphaColor($696969);
      Dimgrey = Alpha or TAlphaColor($696969);
      Dodgerblue = Alpha or TAlphaColor($1E90FF);
      Firebrick = Alpha or TAlphaColor($B22222);
      Floralwhite = Alpha or TAlphaColor($FFFAF0);
      Forestgreen = Alpha or TAlphaColor($228B22);
      Fuchsia = Alpha or TAlphaColor($FF00FF);
      Gainsboro = Alpha or TAlphaColor($DCDCDC);
      Ghostwhite = Alpha or TAlphaColor($F8F8FF);
      Gold = Alpha or TAlphaColor($FFD700);
      Goldenrod = Alpha or TAlphaColor($DAA520);
      Gray = Alpha or TAlphaColor($808080);
      Green = Alpha or TAlphaColor($008000);
      Greenyellow = Alpha or TAlphaColor($ADFF2F);
      Grey = Alpha or TAlphaColor($808080);
      Honeydew = Alpha or TAlphaColor($F0FFF0);
      Hotpink = Alpha or TAlphaColor($FF69B4);
      Indianred = Alpha or TAlphaColor($CD5C5C);
      Indigo = Alpha or TAlphaColor($4B0082);
      Ivory = Alpha or TAlphaColor($FFFFF0);
      Khaki = Alpha or TAlphaColor($F0E68C);
      Lavender = Alpha or TAlphaColor($E6E6FA);
      Lavenderblush = Alpha or TAlphaColor($FFF0F5);
      Lawngreen = Alpha or TAlphaColor($7CFC00);
      Lemonchiffon = Alpha or TAlphaColor($FFFACD);
      Lightblue = Alpha or TAlphaColor($ADD8E6);
      Lightcoral = Alpha or TAlphaColor($F08080);
      Lightcyan = Alpha or TAlphaColor($E0FFFF);
      Lightgoldenrodyellow = Alpha or TAlphaColor($FAFAD2);
      Lightgray = Alpha or TAlphaColor($D3D3D3);
      Lightgreen = Alpha or TAlphaColor($90EE90);
      Lightgrey = Alpha or TAlphaColor($D3D3D3);
      Lightpink = Alpha or TAlphaColor($FFB6C1);
      Lightsalmon = Alpha or TAlphaColor($FFA07A);
      Lightseagreen = Alpha or TAlphaColor($20B2AA);
      Lightskyblue = Alpha or TAlphaColor($87CEFA);
      Lightslategray = Alpha or TAlphaColor($778899);
      Lightslategrey = Alpha or TAlphaColor($778899);
      Lightsteelblue = Alpha or TAlphaColor($B0C4DE);
      Lightyellow = Alpha or TAlphaColor($FFFFE0);
      LtGray = Alpha or TAlphaColor($C0C0C0);
      MedGray = Alpha or TAlphaColor($A0A0A0);
      DkGray = Alpha or TAlphaColor($808080);
      MoneyGreen = Alpha or TAlphaColor($C0DCC0);
      LegacySkyBlue = Alpha or TAlphaColor($F0CAA6);
      Cream = Alpha or TAlphaColor($F0FBFF);
      Lime = Alpha or TAlphaColor($00FF00);
      Limegreen = Alpha or TAlphaColor($32CD32);
      Linen = Alpha or TAlphaColor($FAF0E6);
      Magenta = Alpha or TAlphaColor($FF00FF);
      Maroon = Alpha or TAlphaColor($800000);
      Mediumaquamarine = Alpha or TAlphaColor($66CDAA);
      Mediumblue = Alpha or TAlphaColor($0000CD);
      Mediumorchid = Alpha or TAlphaColor($BA55D3);
      Mediumpurple = Alpha or TAlphaColor($9370DB);
      Mediumseagreen = Alpha or TAlphaColor($3CB371);
      Mediumslateblue = Alpha or TAlphaColor($7B68EE);
      Mediumspringgreen = Alpha or TAlphaColor($00FA9A);
      Mediumturquoise = Alpha or TAlphaColor($48D1CC);
      Mediumvioletred = Alpha or TAlphaColor($C71585);
      Midnightblue = Alpha or TAlphaColor($191970);
      Mintcream = Alpha or TAlphaColor($F5FFFA);
      Mistyrose = Alpha or TAlphaColor($FFE4E1);
      Moccasin = Alpha or TAlphaColor($FFE4B5);
      Navajowhite = Alpha or TAlphaColor($FFDEAD);
      Navy = Alpha or TAlphaColor($000080);
      Oldlace = Alpha or TAlphaColor($FDF5E6);
      Olive = Alpha or TAlphaColor($808000);
      Olivedrab = Alpha or TAlphaColor($6B8E23);
      Orange = Alpha or TAlphaColor($FFA500);
      Orangered = Alpha or TAlphaColor($FF4500);
      Orchid = Alpha or TAlphaColor($DA70D6);
      Palegoldenrod = Alpha or TAlphaColor($EEE8AA);
      Palegreen = Alpha or TAlphaColor($98FB98);
      Paleturquoise = Alpha or TAlphaColor($AFEEEE);
      Palevioletred = Alpha or TAlphaColor($DB7093);
      Papayawhip = Alpha or TAlphaColor($FFEFD5);
      Peachpuff = Alpha or TAlphaColor($FFDAB9);
      Peru = Alpha or TAlphaColor($CD853F);
      Pink = Alpha or TAlphaColor($FFC0CB);
      Plum = Alpha or TAlphaColor($DDA0DD);
      Powderblue = Alpha or TAlphaColor($B0E0E6);
      Purple = Alpha or TAlphaColor($800080);
      Red = Alpha or TAlphaColor($FF0000);
      Rosybrown = Alpha or TAlphaColor($BC8F8F);
      Royalblue = Alpha or TAlphaColor($4169E1);
      Saddlebrown = Alpha or TAlphaColor($8B4513);
      Salmon = Alpha or TAlphaColor($FA8072);
      Sandybrown = Alpha or TAlphaColor($F4A460);
      Seagreen = Alpha or TAlphaColor($2E8B57);
      Seashell = Alpha or TAlphaColor($FFF5EE);
      Sienna = Alpha or TAlphaColor($A0522D);
      Silver = Alpha or TAlphaColor($C0C0C0);
      Skyblue = Alpha or TAlphaColor($87CEEB);
      Slateblue = Alpha or TAlphaColor($6A5ACD);
      Slategray = Alpha or TAlphaColor($708090);
      Slategrey = Alpha or TAlphaColor($708090);
      Snow = Alpha or TAlphaColor($FFFAFA);
      Springgreen = Alpha or TAlphaColor($00FF7F);
      Steelblue = Alpha or TAlphaColor($4682B4);
      Tan = Alpha or TAlphaColor($D2B48C);
      Teal = Alpha or TAlphaColor($008080);
      Thistle = Alpha or TAlphaColor($D8BFD8);
      Tomato = Alpha or TAlphaColor($FF6347);
      Turquoise = Alpha or TAlphaColor($40E0D0);
      Violet = Alpha or TAlphaColor($EE82EE);
      Wheat = Alpha or TAlphaColor($F5DEB3);
      White = Alpha or TAlphaColor($FFFFFF);
      Whitesmoke = Alpha or TAlphaColor($F5F5F5);
      Yellow = Alpha or TAlphaColor($FFFF00);
      Yellowgreen = Alpha or TAlphaColor($9ACD32);
      Null = TAlphaColor($00000000);
      constructor Create(const Color: TAlphaColor);
      class var ColorToRGB: function (Color: TAlphaColor): Longint;
      case Cardinal of
        0:
          (Color: TAlphaColor);
        2:
          (HiWord, LoWord: Word);
        3:
  {$IFDEF BIGENDIAN}
          (A, R, G, B: System.Byte);
  {$ELSE}
          (B, G, R, A: System.Byte);
  {$ENDIF}
    end;

  TBitmapData = record
    private
      FPixelFormat: TPixelFormat;
      FWidth: Integer;
      FHeight: Integer;
      function GetBytesPerPixel: Integer;
      function GetBytesPerLine: Integer;
    public
      Data: Pointer;
      Pitch: Integer;
      constructor Create(const AWidth, AHeight: Integer; const APixelFormat: TPixelFormat);
      function GetPixel(const X, Y: Integer): TAlphaColor;
      procedure SetPixel(const X, Y: Integer; const AColor: TAlphaColor);
      procedure Copy(const Source: TBitmapData);
      function GetScanline(const I: Integer): Pointer;
      function GetPixelAddr(const I, J: Integer): Pointer;
      property PixelFormat: TPixelFormat read FPixelFormat;
      property BytesPerPixel: Integer read GetBytesPerPixel;
      property BytesPerLine: Integer read GetBytesPerLine;
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
    end;

  /// <summary>
  /// enumeration of supported bitmap format which the RGBLuminanceSource can process
  /// </summary>
  TBitmapFormat = (
    /// <summary>
    /// format of the byte[] isn't known. RGBLuminanceSource tries to determine the best possible value
    /// </summary>
    Unknown,
    /// <summary>
    /// grayscale array, the byte array is a luminance array with 1 byte per pixel
    /// </summary>
    Gray8,
    /// <summary>
    /// 3 bytes per pixel with the channels red, green and blue
    /// </summary>
    RGB24,
    /// <summary>
    /// 4 bytes per pixel with the channels red, green and blue
    /// </summary>
    RGB32,
    /// <summary>
    /// 4 bytes per pixel with the channels alpha, red, green and blue
    /// </summary>
    ARGB32,
    /// <summary>
    /// 3 bytes per pixel with the channels blue, green and red
    /// </summary>
    BGR24,
    /// <summary>
    /// 4 bytes per pixel with the channels blue, green and red
    /// </summary>
    BGR32,
    /// <summary>
    /// 4 bytes per pixel with the channels blue, green, red and alpha
    /// </summary>
    BGRA32,
    /// <summary>
    /// 2 bytes per pixel, 5 bit red, 6 bits green and 5 bits blue
    /// </summary>
    RGB565,
    /// <summary>
    /// 4 bytes per pixel with the channels red, green, blue and alpha
    /// </summary>
    RGBA32); 

  /// <summary>
  /// Luminance source class which support different formats of images.
  /// </summary>
  TRGBLuminanceSource = class(TBaseLuminanceSource)
  private
    function DetermineBitmapFormat(const rgbRawBytes: TArray<Byte>;
      const width, height: Integer): TBitmapFormat;

    procedure CalculateLuminanceRGB24(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceRGB32(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceRGBA32(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceRGB565(const rgb565RawData: TArray<Byte>);

    procedure CalculateLuminanceBGR24(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceBGR32(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceBGRA32(const rgbRawBytes: TArray<Byte>);

    procedure CalculateLuminanceARGB32(const rgbRawBytes: TArray<Byte>);

    function GetBitmapFormat: TBitmapFormat;
  protected
    function CreateLuminanceSource(const newLuminances: TArray<Byte>;
      const width, height: Integer): TLuminanceSource; override;
    procedure CalculateLuminance(const rgbRawBytes: TArray<Byte>;
      bitmapFormat: TBitmapFormat);
  public
    constructor Create(const width, height: Integer); overload;
    constructor Create(const rgbRawBytes: TArray<Byte>;
      const width, height: Integer); overload;
    constructor Create(const rgbRawBytes: TArray<Byte>;
      const width, height: Integer; const bitmapFormat: TBitmapFormat); overload;
    constructor CreateFromBitmap(const sourceBitmap: TBitmap;
      const width, height: Integer);

    property BitmapFormat : TBitmapFormat read GetBitmapFormat;
  end;

implementation

uses ZXing.Helpers;

constructor TAlphaColorRec.Create(const Color: TAlphaColor);
begin
end;

function TBitmapData.GetBytesPerPixel: Integer;
begin
end;
function TBitmapData.GetBytesPerLine: Integer;
begin
end;
constructor TBitmapData.Create(const AWidth, AHeight: Integer; const APixelFormat: TPixelFormat);
begin
end;
function TBitmapData.GetPixel(const X, Y: Integer): TAlphaColor;
begin
end;
procedure TBitmapData.SetPixel(const X, Y: Integer; const AColor: TAlphaColor);
begin
end;
procedure TBitmapData.Copy(const Source: TBitmapData);
begin
end;
function TBitmapData.GetScanline(const I: Integer): Pointer;
begin
end;
function TBitmapData.GetPixelAddr(const I, J: Integer): Pointer;
begin
end;


/// <summary>
/// Initializes a new instance of the <see cref="RGBLuminanceSource"/> class.
/// </summary>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
constructor TRGBLuminanceSource.Create(const width, height: Integer);
begin
  inherited Create(width, height);
end;

/// <summary>
/// Initializes a new instance of the <see cref="RGBLuminanceSource"/> class.
/// It supports a byte array with 3 bytes per pixel (RGB24).
/// </summary>
/// <param name="rgbRawBytes">The RGB raw bytes.</param>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
constructor TRGBLuminanceSource.Create(const rgbRawBytes: TArray<Byte>;
  const width, height: Integer);
begin
  Create(rgbRawBytes, width, height, TBitmapFormat.RGB24);
end;

/// <summary>
/// Initializes a new instance of the <see cref="RGBLuminanceSource"/> class.
/// It supports a byte array with 3 bytes per pixel (RGB24).
/// </summary>
/// <param name="rgbRawBytes">The RGB raw bytes.</param>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
/// <param name="bitmapFormat">The bitmap format.</param>
constructor TRGBLuminanceSource.Create(const rgbRawBytes: TArray<Byte>;
  const width, height: Integer; const bitmapFormat: TBitmapFormat);
begin
  inherited Create(width, height);
  CalculateLuminance(rgbRawBytes, bitmapFormat);
end;


{$IFDEF USE_VCL_BITMAP}
// VCL TBitmap implementation
constructor TRGBLuminanceSource.CreateFromBitmap(const sourceBitmap: TBitmap; const width, height: Integer);
type
  TRGBTripleArray = ARRAY[Word] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray; // Use a PByteArray for pf8bit color.

var
  x, y,
  offset : Integer;
  r, g, b : Byte;
  P: pRGBTripleArray;

begin
  Self.Create(width, height);
  sourceBitmap.PixelFormat := pf24bit;
  for y := 0 to sourceBitmap.Height - 1 do
  begin
    offset := y * FWidth;
    P := sourceBitmap.ScanLine[y];
    for x := 0 to sourceBitmap.Width - 1 do
    begin
       r := P[x].rgbtRed;
       g := P[x].rgbtGreen;
       b := P[x].rgbtBlue;
       luminances[offset + x] := TMathUtils.Asr(3482*r + 11721*g + 1181*b, 14);
    end;
  end;
end;
{$ELSE}
// FMX TBitmap implementation
constructor TRGBLuminanceSource.CreateFromBitmap(const sourceBitmap: TBitmap;
   const width, height: Integer);
var
  x, y,
  offset : Integer;
  color : TAlphaColor;
  r, g, b : Byte;
  currentData: TBitmapData;
begin
  Self.Create(width, height);

  // In order to measure pure decoding speed, we convert the entire image to a greyscale array
  // up front, which is the same as the Y channel of the YUVLuminanceSource in the real app.

  //if (sourceBitmap.Map(TMapAccess.Read, currentData)) then
  begin
    try
      for y := 0 to FHeight - 1 do
      begin
        offset := y * FWidth;
        for x := 0 to FWidth - 1 do
        begin
          color := currentData.GetPixel(x, y);
          r := TAlphaColorRec(color).R;
          g := TAlphaColorRec(color).G;
          b := TAlphaColorRec(color).B;
          luminances[offset + x] := TMathUtils.Asr(3482*r + 11721*g + 1181*b, 14);
        end;
      end;
    finally
      //sourceBitmap.Unmap(currentData);
      //sourceBitmap.DisposeOf();
    end;
  end;
end;
{$ENDIF}

/// <summary>
/// Should create a new luminance source with the right class type.
/// The method is used in methods crop and rotate.
/// </summary>
/// <param name="newLuminances">The new luminances.</param>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
/// <returns></returns>
function TRGBLuminanceSource.CreateLuminanceSource(
  const newLuminances: TArray<Byte>;
  const width, height: Integer): TLuminanceSource;
begin
  Result := TRGBLuminanceSource.Create(width, height);
  luminances := newLuminances;
end;

function TRGBLuminanceSource.DetermineBitmapFormat(
  const rgbRawBytes: TArray<Byte>;
  const width, height: Integer): TBitmapFormat;
var
  square,
  byteperpixel : Integer;
begin
  square := (width * height);
  byteperpixel := (Length(rgbRawBytes) div square);

  case byteperpixel of
    1: Result := TBitmapFormat.Gray8;
    2: Result := TBitmapFormat.RGB565;
    3: Result := TBitmapFormat.RGB24;
    4: Result := TBitmapFormat.RGB32;
    else
       raise EArgumentException.Create('The bitmap format could not be determined. Please specify the correct value.');
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminance(const rgbRawBytes: TArray<Byte>;
  bitmapFormat: TBitmapFormat);
var
  len: Integer;
begin
  if (bitmapFormat = TBitmapFormat.Unknown)
  then
     bitmapFormat := DetermineBitmapFormat(rgbRawBytes, self.Width, self.Height);

  case bitmapFormat of
    TBitmapFormat.Gray8 :
      begin
        if (Length(rgbRawBytes) < Length(luminances))
        then
           len := Length(rgbRawBytes)
        else
           len := Length(luminances);

        //Copy(rgbRawBytes, 0, len);
      end;
    TBitmapFormat.RGB24 :
      begin
        CalculateLuminanceRGB24(rgbRawBytes);
      end;
    TBitmapFormat.RGB32 :
      begin
        CalculateLuminanceRGB32(rgbRawBytes);
      end;
    TBitmapFormat.ARGB32 :
      begin
        CalculateLuminanceARGB32(rgbRawBytes);
      end;
    TBitmapFormat.BGR24 :
      begin
        CalculateLuminanceBGR24(rgbRawBytes);
      end;
    TBitmapFormat.BGR32 :
      begin
        CalculateLuminanceBGR32(rgbRawBytes);
      end;
    TBitmapFormat.BGRA32 :
      begin
        CalculateLuminanceBGRA32(rgbRawBytes);
      end;
    TBitmapFormat.RGB565 :
      begin
        CalculateLuminanceRGB565(rgbRawBytes);
      end;
    TBitmapFormat.RGBA32 :
      begin
        CalculateLuminanceRGBA32(rgbRawBytes);
      end;
     else
        raise EArgumentException.Create('The bitmap format isn''t supported.' +
          GetEnumName(TypeInfo(TBitmapFormat), Ord(bitmapFormat)))
    end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceRGB565(
  const rgb565RawData: TArray<Byte>);
var
  luminanceIndex,
  index  : Integer;
  byte1,
  byte2,
  b5, g5, r5,
  r8, g8, b8 : Byte;
begin
  luminanceIndex := 0;
  index := 0;
  while (((index < Length(rgb565RawData)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    byte1 := rgb565RawData[index];
    byte2 := rgb565RawData[index + 1];

    b5 := (byte1 and $1F);
    g5 := (TMathUtils.Asr(byte1 and $E0 , 5) or ((byte2 and $03) shl 3)) and $1F;
    r5 := TMathUtils.Asr(byte2, 2) and $1F;
    r8 := TMathUtils.Asr(r5 * 527 + 23, 6);
    g8 := TMathUtils.Asr(g5 * 527 + 23, 6);
    b8 := TMathUtils.Asr(b5 * 527 + 23, 6);

    // cheap, not fully accurate conversion
    //pixel := (byte2 shl 8) or byte1;
    //b8 := (((pixel) and $001F) shl 3);
    //g8 := (((pixel) and $07E0) shr 2) and $FF;
    //r8 := (((pixel) and $F800) shr 8);

    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r8 + GChannelWeight * g8 +
                                    BChannelWeight * b8, ChannelWeight);
    Inc(index, 2);
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceRGB24(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  r, g, b : Integer;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                                    BChannelWeight * b,ChannelWeight);
    inc(luminanceIndex)
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceBGR24(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  b, g, r : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                                    BChannelWeight * b,ChannelWeight);
    Inc(luminanceIndex)
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceRGB32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  r, g, b : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex, 2);
    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                                    BChannelWeight * b, ChannelWeight);
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceBGR32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  b, g, r : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex, 2);
    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                                    BChannelWeight * b, ChannelWeight);
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceBGRA32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  b, g, r,
  alpha : Byte;
  luminance : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    alpha := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);

    luminance := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                   BChannelWeight * b, ChannelWeight);
    luminances[luminanceIndex] := (TMathUtils.Asr(luminance * alpha, 8) + TMathUtils.Asr(255 * (255 - alpha), 8));
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceRGBA32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  r, g, b,
  alpha : Byte;
  luminance : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    alpha := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);

    luminance := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                   BChannelWeight * b, ChannelWeight);
    luminances[luminanceIndex] := TMathUtils.Asr(luminance * alpha, 8) + TMathUtils.Asr(255 * (255 - alpha), 8);
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceARGB32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  r, g, b,
  alpha : Byte;
  luminance : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    alpha := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);

    luminance := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                   BChannelWeight * b, ChannelWeight);
    luminances[luminanceIndex] := TMathUtils.Asr(luminance * alpha,8) + TMathUtils.Asr(255 * (255 - alpha),8);
    Inc(luminanceIndex);
  end;
end;

function TRGBLuminanceSource.GetBitmapFormat: TBitmapFormat;
begin
  try
    Result := DetermineBitmapFormat(luminances, Width, Height);
  except
    Result := TBitmapFormat.Unknown;
  end;
end;

end.
