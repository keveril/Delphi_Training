program Project1;

uses
  Vcl.Forms,
  uWidgetForm in 'uWidgetForm.pas' {WidgetForm},
  uWidgetSubclass in 'uWidgetSubclass.pas',
  uWidget in 'uWidget.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWidgetForm, WidgetForm);
  Application.Run;
end.
