program Project1;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  uWidgetSubclass in 'uWidgetSubclass.pas',
  uWidget in 'uWidget.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, TWidgetForm);
  Application.Run;
end.
