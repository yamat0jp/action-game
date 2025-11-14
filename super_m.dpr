program super_m;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  update in 'update.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
