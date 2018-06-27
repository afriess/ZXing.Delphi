program vfwscanner;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  Forms, frmmain;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
