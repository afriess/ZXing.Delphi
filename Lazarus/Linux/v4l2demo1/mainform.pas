unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  VideoCapture, videodev2,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  FPReadJPEG,IntfGraphics,FPImage,FPWriteBMP,RGB16writer,
  gtk2, gdk2, gdk2x, glib
  ,ZXing.ReadResult
  ,ZXing.BarCodeFormat
  ,ZXing.ScanManager
  ;

type

  { TFormMain }

  TFormMain = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    ButtonDefaultControls: TButton;
    ButtonUpdateControls: TButton;
    CheckBoxCapture: TCheckBox;
    CheckBoxFast: TCheckBox;
    CheckBoxOpenClose: TCheckBox;
    Edit1: TEdit;
    EditBufferCount: TEdit;
    EditDevice: TEdit;
    EditFrameRate: TEdit;
    FrameRate: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BytesLabel: TLabel;
    BytesNumberLabel: TLabel;
    LabelError: TLabel;
    MemoInfo: TMemo;
    PageControl1: TPageControl;
    PaintBox: TPaintBox;
    PanelControls: TScrollBox;
    StatusBar: TStatusBar;
    PartyTabSheet: TTabSheet;
    CameraSettingsTabSheet: TTabSheet;
    GlobalSettingsTabSheet: TTabSheet;

    procedure ButtonDefaultControlsClick(Sender: TObject);
    procedure ButtonUpdateControlsClick(Sender: TObject);
    procedure CheckBoxOpenCloseChange(Sender: TObject);
    procedure CheckBoxCaptureChange(Sender: TObject);
    procedure EditBufferCountEditingDone(Sender: TObject);
    procedure EditFrameRateEditingDone(Sender: TObject);
    procedure EditDeviceEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LabelErrorClick(Sender: TObject);
    procedure LabelErrorDblClick(Sender: TObject);
    procedure VideoFrameSynchronized(Sender: TObject; Buffer: pointer;
      Size: integer; Error: boolean);
  private
    { private declarations }
    Video: TVideo4L2Device;
    BMP: TBitmap;
    prevTicks: QWord;
    UpdatingControls: boolean;
    TheAdmin: boolean;

    // for fast image showing
    FWindow, drawingarea: PGtkWidget;
    FImage: PGdkImage;

    // for image capturing
    aMS        : TMemoryStream;
    aImage     : TFPCompactImgRGB16Bit;
    aImgReader : TFPReaderJpeg;
    aImgWriter : TFPWriterRGB16;

    procedure ControlTrackBarChange(Sender: TObject);
    procedure ControlComboBoxChange(Sender: TObject);
    procedure ControlButtonClick(Sender: TObject);
    procedure ControlCheckBoxChange(Sender: TObject);
    procedure ClearPanelControls;
    procedure ShowPanelControls;
    procedure UpdatePanelControls;
    procedure UpdateParams;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

function CloseVideo(widget: pGtkWidget; event: pGdkEvent; data: gpointer): gint; cdecl;

implementation

{$R *.lfm}

uses
  YUV2RGB;

{ TFormMain }

function CloseVideo(widget: pGtkWidget; event: pGdkEvent; data: gpointer): gint; cdecl;
begin
  CloseVideo:= 0;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  ScaleDivisor:word;
begin

  TheAdmin:=False;

  Video:=TVideo4L2Device.Create(Self);

  //Video.PixelFormat := V4L2_PIX_FMT_MJPEG;

  Video.Width := 640;
  Video.Height := 480;
  //Video.Width := 960;
  //Video.Height := 720;
  //Video.Width := 320;
  //Video.Height := 240;
  Video.FrameRate := 10;
  Video.BufferCount := 4;

  Video.OnFrameSynchronized := @VideoFrameSynchronized;
  //Video.OnFrame := @VideoFrameSynchronized;

  // BMP for on-form preview
  BMP:=TBitmap.Create;
  BMP.Width:=PaintBox.Width;
  BMP.Height:=PaintBox.Height;
  BMP.PixelFormat:=pf16bit;

  aMS        := TMemoryStream.Create;
  aImgReader := TFPReaderJpeg.Create;
  aImgReader.Performance:=jpBestSpeed;
  aImgReader.Smoothing:=False;
  //aImgReader.Scale:=jsHalf;
  aImgWriter := TFPWriterRGB16.Create;

  case aImgReader.Scale of
    jsFullSize:ScaleDivisor:=1;
    jsHalf:ScaleDivisor:=2;
    jsQuarter:ScaleDivisor:=4;
    jsEighth:ScaleDivisor:=8;
  else
    ScaleDivisor:=1;
  end;

  aImage     := TFPCompactImgRGB16Bit.Create(Video.Width DIV ScaleDivisor, Video.Height DIV ScaleDivisor);
  aImage.UsePalette:=False;

  UpdateParams;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Video.Free; // free Video BEFORE drawable bitmap or Synchronize will raise AV
  if aMS<>nil then aMS.Free;
  if aImage<>nil then aImage.Free;
  if aImgReader<>nil then aImgReader.Free;
  if aImgWriter<>nil then aImgWriter.Free;
  if BMP<>nil then BMP.Free;
end;

procedure TFormMain.CheckBoxOpenCloseChange(Sender: TObject);
begin
  if UpdatingControls then exit;
  Video.Open:=CheckBoxOpenClose.Checked;
  if Video.Open then begin
    Video.SetControlValue(V4L2_CID_POWER_LINE_FREQUENCY,V4L2_CID_POWER_LINE_FREQUENCY_50HZ);
    Video.SetControlValue(V4L2_CID_AUTO_WHITE_BALANCE,V4L2_WHITE_BALANCE_AUTO);
    Video.SetControlValue(V4L2_CID_EXPOSURE_AUTO,V4L2_EXPOSURE_APERTURE_PRIORITY);
    ShowPanelControls;
    UpdatePanelControls;
  end else begin
    ClearPanelControls;
  end;
  UpdateParams;
end;

procedure TFormMain.CheckBoxCaptureChange(Sender: TObject);
var
  flag:boolean;
  vbox, bclose, frame, topbox: PGtkWidget;
begin
  if UpdatingControls then exit;
  flag:=CheckBoxCapture.Checked and not Video.Open;

  if CheckBoxCapture.Checked then
  begin
    FWindow:= gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW (FWindow), PChar('Preview'));
    gtk_window_set_resizable(GTK_WINDOW(FWindow), TRUE);

    topbox:= gtk_vbox_new(FALSE, 0);
    gtk_container_add(GTK_CONTAINER(FWindow), topbox);

    frame:= gtk_frame_new(nil);
    gtk_box_pack_start(GTK_BOX(topbox), frame, FALSE, TRUE, 0);
    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);

    drawingarea:= gtk_drawing_area_new();
    gtk_drawing_area_size(GTK_DRAWING_AREA(drawingarea), Video.Width, Video.Height);
    gtk_container_add(GTK_CONTAINER(frame), drawingarea);

    vbox:= gtk_hbox_new(FALSE, 5);

    bclose:= gtk_button_new_with_label('Close Preview');
    gtk_signal_connect(GTK_OBJECT(bclose), 'clicked', GTK_SIGNAL_FUNC(@CloseVideo), nil);
    gtk_box_pack_start(GTK_BOX(vbox), bclose, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(topbox), vbox, FALSE, TRUE, 0);

    {
    gtk_widget_show(bclose);
    gtk_widget_show(vbox);
    gtk_widget_show(drawingarea);
    gtk_widget_show(frame);
    gtk_widget_show(topbox);
    gtk_widget_show(FWindow);
    }

    gtk_widget_show_all(FWindow);

    {$ifdef LCLGTK2}
    //FImage:= gdk_image_new(GDK_IMAGE_NORMAL, gtk_widget_get_visual(FWindow), Video.Width, Video.Height);
    //FImage:= gdk_image_new(GDK_IMAGE_SHARED, gtk_widget_get_visual(FWindow), Video.Width, Video.Height);
    FImage:= gdk_image_new(GDK_IMAGE_FASTEST, gtk_widget_get_visual(FWindow), Video.Width, Video.Height);
    {$endif}
    {$ifdef LCLGTK}
    FImage := gdk_image_new(FFImageTypeConsts[FImageType], gtk_widget_get_visual(FWindow),FWidth,FHeight);
    {$endif}
  end;

  Video.Capture:=CheckBoxCapture.Checked;
  if flag then begin
    ShowPanelControls;
    UpdatePanelControls;
  end;

  if NOT CheckBoxCapture.Checked then
  begin
    if FImage<> nil then begin
       gdk_image_destroy(FImage);
       FImage:=nil;
    end;
    if FWindow<> nil then begin
       gtk_widget_hide(FWindow);
       gtk_widget_destroy(FWindow);
       FWindow:= nil;
    end;
  end;

  UpdateParams;

  if Video.Capture
     then MemoInfo.Lines.Add('Capturing started')
     else MemoInfo.Lines.Add('Capturing stopped');

end;

procedure TFormMain.EditBufferCountEditingDone(Sender: TObject);
begin
  Video.BufferCount:=StrToInt(EditBufferCount.Text);
end;

procedure TFormMain.EditFrameRateEditingDone(Sender: TObject);
begin
  Video.FrameRate:=StrToInt(EditFrameRate.Text);
  UpdateParams;
end;

procedure TFormMain.EditDeviceEditingDone(Sender: TObject);
begin
  Video.Device:=EditDevice.Text;
  UpdateParams;
end;

procedure TFormMain.UpdateParams;
begin
  UpdatingControls:=True;
  CheckBoxOpenClose.Checked:=Video.Open;
  CheckBoxCapture.Checked:=Video.Capture;
  EditBufferCount.Text:=IntToStr(Video.BufferCount);
  EditFrameRate.Text:=IntToStr(Video.FrameRate);
  UpdatingControls:=False;
end;

function ControlTypeToStr(t:longword):string;
begin
  case t of
    V4L2_CTRL_TYPE_INTEGER:      Result:='int';
    V4L2_CTRL_TYPE_BOOLEAN:      Result:='bool';
    V4L2_CTRL_TYPE_MENU:         Result:='menu';
    V4L2_CTRL_TYPE_INTEGER_MENU: Result:='int_menu';
    V4L2_CTRL_TYPE_BITMASK:      Result:='bitmask';
    V4L2_CTRL_TYPE_BUTTON:       Result:='button';
    V4L2_CTRL_TYPE_INTEGER64:    Result:='int64';
    V4L2_CTRL_TYPE_STRING:       Result:='string';
  else
    Result:='???';
  end;
end;

procedure TFormMain.ClearPanelControls;
var i: integer;
begin
  for i:=PanelControls.ControlCount-1 downto 0 do begin
    PanelControls.Controls[i].Free;
  end;
end;

procedure TFormMain.ShowPanelControls;
var i,j:integer;
  ctop: integer;
  LabelControl: TLabel;
  NewControl: TControl;
  TrackBar: TTrackBar;
  CheckBox: TCheckBox;
  ComboBox: TComboBox;
  Button: TButton;
begin
  MemoInfo.Clear;
  for i:=0 to Video.ControlsInfo.Count-1 do begin
    with Video.ControlsInfo[i] do begin
      MemoInfo.Lines.Add(Name+' ('+ControlTypeToStr(ControlType)+') ['+
        IntToStr(Minimum)+'..'+IntToStr(Maximum)+'] ('+IntToStr(DefaultValue)+')');
      if ControlType in [V4L2_CTRL_TYPE_MENU, V4L2_CTRL_TYPE_INTEGER_MENU] then begin
        for j:=0 to Menu.Count-1 do begin
          with Menu[j] do begin
            if TypeInteger then begin
              MemoInfo.Lines.Add('  '+IntToStr(Index)+': (int64) '+Name+' '+IntToHex(Value,8));
            end else begin
              MemoInfo.Lines.Add('  '+IntToStr(Index)+': '+Name)
            end;
          end;
        end;
      end;
    end;
  end;

  UpdatingControls:=True;
  ClearPanelControls;
  ctop:=0;
  for i:=0 to Video.ControlsInfo.Count-1 do begin
    with Video.ControlsInfo[i] do begin
      LabelControl:=TLabel.Create(Self);
      LabelControl.WordWrap:=True;
      LabelControl.AutoSize:=False;
      LabelControl.Font.Size:=8;
      LabelControl.Caption:=Name;
      LabelControl.Top:=14+ctop;
      LabelControl.Left:=4;
      LabelControl.Width:=100;
      LabelControl.Height:=26;
      LabelControl.WordWrap:=True;
      LabelControl.Parent:=PanelControls;
      case ControlType of
        V4L2_CTRL_TYPE_INTEGER: begin
          TrackBar:=TTrackBar.Create(Self);
          TrackBar.Min:=Minimum;
          TrackBar.Max:=Maximum;
          TrackBar.LineSize:=Step;
          TrackBar.PageSize:=Step;
          TrackBar.Position:=DefaultValue;
          TrackBar.Height:=38;
          TrackBar.OnChange:=@ControlTrackBarChange;
          NewControl:=TrackBar;
        end;
        V4L2_CTRL_TYPE_BOOLEAN: begin
          CheckBox:=TCheckBox.Create(Self);
          CheckBox.Checked:=Boolean(DefaultValue);
          CheckBox.OnChange:=@ControlCheckBoxChange;
          NewControl:=CheckBox;
        end;
        V4L2_CTRL_TYPE_MENU: begin
          ComboBox:=TComboBox.Create(Self);
          ComboBox.Style:=csDropDownList;
          for j:=0 to Menu.Count-1 do begin
            ComboBox.Items.Add(Menu[j].Name);
          end;
          ComboBox.ItemIndex:=Value; // DefaultValue is menu index, we need item index here, so using Value
          ComboBox.OnChange:=@ControlComboBoxChange;
          NewControl:=ComboBox;
        end;
        V4L2_CTRL_TYPE_INTEGER_MENU: begin
          ComboBox:=TComboBox.Create(Self);
          ComboBox.Style:=csDropDownList;
          for j:=0 to Menu.Count-1 do begin
            ComboBox.Items.Add(Menu[j].Name);
          end;
          ComboBox.ItemIndex:=Value; // DefaultValue is menu index, we need item index here, so using Value
          ComboBox.OnChange:=@ControlComboBoxChange;
          NewControl:=ComboBox;
        end;
        V4L2_CTRL_TYPE_BUTTON: begin
          Button:=TButton.Create(Self);
          Button.OnClick:=@ControlButtonClick;
          NewControl:=Button;
        end;
      else
        // V4L2_CTRL_TYPE_BITMASK:
        // V4L2_CTRL_TYPE_INTEGER64:
        // V4L2_CTRL_TYPE_STRING:
        LabelControl:=TLabel.Create(Self);
        LabelControl.Caption:='('+ControlTypeToStr(ControlType)+')';
        NewControl:=LabelControl;
      end;
      NewControl.Tag:=i;
      NewControl.Font.Size:=8;
      NewControl.Top:=4+ctop;
      NewControl.Left:=4+100;
      NewControl.Width:=170;
      NewControl.Parent:=PanelControls;
      ctop:=ctop+40;
    end;
  end;
  UpdatingControls:=False;
end;

procedure TFormMain.UpdatePanelControls;
var
  i:integer;
  Control:TControl;
begin
  UpdatingControls:=True;
  Video.GetControlValues;
  for i:=0 to Video.ControlsInfo.Count-1 do begin
    with Video.ControlsInfo[i] do begin
      Control:=PanelControls.Controls[i*2+1];
      case ControlType of
        V4L2_CTRL_TYPE_INTEGER: begin
          (Control as TTrackBar).Position:=Value;
        end;
        V4L2_CTRL_TYPE_BOOLEAN: begin
          (Control as TCheckBox).Checked:=Boolean(Value);
        end;
        V4L2_CTRL_TYPE_MENU: begin
          (Control as TComboBox).ItemIndex:=Value;
        end;
        V4L2_CTRL_TYPE_INTEGER_MENU: begin
          (Control as TComboBox).ItemIndex:=Value;
        end;
        V4L2_CTRL_TYPE_BUTTON: begin
          // no value
        end;
      else
        // V4L2_CTRL_TYPE_BITMASK:
        // V4L2_CTRL_TYPE_INTEGER64:
        // V4L2_CTRL_TYPE_STRING:
      end;
    end;
  end;
  UpdatingControls:=False;
end;

procedure TFormMain.ButtonUpdateControlsClick(Sender: TObject);
begin
  UpdatePanelControls;
end;

procedure TFormMain.ButtonDefaultControlsClick(Sender: TObject);
begin
  Video.SetDefaulControlValues;
  UpdatePanelControls;
end;

procedure TFormMain.ControlTrackBarChange(Sender: TObject);
begin
  if UpdatingControls then exit;
  with Sender as TTrackBar do begin
    Video.ControlsInfo[Tag].SetValue(Position);
  end;
end;

procedure TFormMain.ControlCheckBoxChange(Sender: TObject);
begin
  if UpdatingControls then exit;
  with Sender as TCheckBox do begin
    Video.ControlsInfo[Tag].SetValue(Integer(Checked));
  end;
end;

procedure TFormMain.ControlButtonClick(Sender: TObject);
begin
  if UpdatingControls then exit;
  with Sender as TButton do begin
    Video.ControlsInfo[Tag].SetValue(0); // any value is ok for button control
  end;
end;

procedure TFormMain.ControlComboBoxChange(Sender: TObject);
begin
  if UpdatingControls then exit;
  with Sender as TComboBox do begin
    Video.ControlsInfo[Tag].SetValue(ItemIndex); // item index will be recalculated to menu index
  end;
end;

procedure TFormMain.LabelErrorClick(Sender: TObject);
begin
  //LabelError.Caption:='';
end;

procedure TFormMain.LabelErrorDblClick(Sender: TObject);
begin
  TheAdmin:=NOT TheAdmin;

  if NOT TheAdmin then PageControl1.PageIndex:=0;

  GlobalSettingsTabSheet.TabVisible:=TheAdmin;
  CameraSettingsTabSheet.TabVisible:=TheAdmin;

  if TheAdmin then LabelError.Color:=clRed else LabelError.Color:=clBlack;

end;

procedure TFormMain.VideoFrameSynchronized(Sender: TObject; Buffer: pointer;
  Size: integer; Error: boolean);
var
  ticks       : QWord;
  PStart      : PByte;
begin

  BytesNumberLabel.Caption:=IntToStr(Size);

  if Error
     then LabelError.Caption:='Frame with recoverable error received';

  if Video.PixelFormat = V4L2_PIX_FMT_YUYV then
  begin
    if Size<>(FImage^.width*FImage^.height*2{YUYV}) then begin
    //if Size<>(PaintBox.Width*PaintBox.Height*2{YUYV}) then begin
       LabelError.Caption:='Invalid buffer length '+IntToStr(Size);
    end;

    if CheckBoxFast.Checked then
    begin
      if (FImage <> nil) AND (drawingarea <> nil) then
      begin
        YUYV_to_BGRA16(PLongWord(Buffer), FImage^.mem, Video.Width*Video.Height);
        {$ifdef LCLGTK2}
        gdk_draw_image(drawingarea^.window,drawingarea^.style^.white_gc,FImage,0,0,0,0,Video.Width,Video.Height);
        {$endif}
        {$ifdef LCLGTK}
        gdk_draw_image(drawingarea^.window,PGtkStyle(drawingarea^.thestyle)^.white_gc,FImage,0,0,0,0,Video.Width,Video.Height);
       {$endif}
      end;
    end
    else
    begin
      BMP.BeginUpdate;
      YUYV_to_BGRA16(PLongWord(Buffer), PWord(BMP.RawImage.Data), BMP.Width*BMP.Height);
      BMP.EndUpdate;
      PaintBox.Canvas.Draw(0,0,BMP);
    end;
  end;

  if Video.PixelFormat = V4L2_PIX_FMT_MJPEG then
  begin
    aMS.Clear;
    aMS.Position:=0;
    aMS.WriteBuffer(Buffer^,Size);
    aMS.Position:=0;
    aImage.LoadFromStream(aMS,aImgReader);
    aMS.Clear;
    aMS.Position:=0;
    aImage.SaveToStream(aMS,aImgWriter);
    aMS.Position:=0;
    if CheckBoxFast.Checked then
    begin
      if (FImage <> nil) AND (drawingarea <> nil) then
      begin
        Move(aMS.Memory^,FImage^.mem^,Video.Width*Video.Height*2);
        {$ifdef LCLGTK2}
        gdk_draw_image(drawingarea^.window,drawingarea^.style^.white_gc,FImage,0,0,0,0,Video.Width,Video.Height);
        {$endif}
        {$ifdef LCLGTK}
        gdk_draw_image(drawingarea^.window,PGtkStyle(drawingarea^.thestyle)^.white_gc,FImage,0,0,0,0,Video.Width,Video.Height);
        {$endif}
      end;
    end
    else
    begin
      PStart:=BMP.RawImage.Data;
      BMP.BeginUpdate;
      Move(aMS.Memory^,PStart^,Video.Width*Video.Height*2);
      BMP.EndUpdate;
      PaintBox.Canvas.Draw(0,0,BMP);
    end;
  end;

  ticks:=GetTickCount64;
  StatusBar.SimpleText:=IntToStr(round(1000.0/(ticks-prevTicks)))+' FPS';
  StatusBar.Refresh;
  prevTicks:=ticks;

end;

end.

