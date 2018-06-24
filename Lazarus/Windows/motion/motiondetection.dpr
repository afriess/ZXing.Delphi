program motiondetection;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

uses
  Interfaces,Forms, frmmain;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
