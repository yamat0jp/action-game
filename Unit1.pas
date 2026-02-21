unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
  private
    { private êÈåæ }
    procedure terminated(Sender: TObject);
  public
    { public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses Field;

var
  Field: TDataField;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Field := TDataField.Create(Self);
  Field.Parent := Self;
  Field.FieldSize := ClientHeight div TDataField.hei;
  Field.OnTerminate := terminated;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Field.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
var KeyChar: WideChar; Shift: TShiftState);
var
  player: TPlayer;
begin
  player := field.Players[0];
  if player.State = Sprite then
    Exit;
  case Key of
    VKLEFT:
      player.Kasoku_X := -field.kasoku;
    VKRIGHT:
      player.Kasoku_X := field.kasoku;
    VKUP:
      player.Jump;
  end;
  if (KeyChar = ' ') and player.Ground then
    player.Dash := true;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
var KeyChar: WideChar; Shift: TShiftState);
var
  player: TPlayer;
begin
  player := field.Players[0];
  if player.State = Sprite then
    Exit;
  case Key of
    VKLEFT, VKRIGHT:
      player.Kasoku_X := 0;
  end;
  if KeyChar = ' ' then
    player.Dash := false;
end;

procedure TForm1.terminated(Sender: TObject);
begin
  Close;
end;

end.
