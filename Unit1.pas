unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, System.Math,
  System.Character, Vcl.AppEvnts;

type
  TDataField = class
  private
    FField: array [0 .. 255, 0 .. 15] of AnsiChar;
    henkan: array [AnsiChar] of Char;
    function GetStrings(X, Y: integer): Char;
  public
    constructor Create(const str: AnsiString);
    property Strings[X, Y: integer]: Char read GetStrings; default;
  end;

  TPlayer = class
  const
    MAX_SPEED = 0.25;
  private
    FX, FY: Single;
    FSpeed_X, FSpeed_Y: Single;
    FKasoku_X, FKasoku_Y: Single;
    FJump: Boolean;
    FDash: Boolean;
    function limitPlus(X, delta, MAX: Single): Single;
    procedure SetJump(const Value: Boolean);
  protected
    procedure SetSpeed(X, Y: Single);
  public
    procedure move;
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property Kasoku_X: Single read FKasoku_X write FKasoku_X;
    property Kasoku_Y: Single read FKasoku_Y write FKasoku_Y;
    property Jump: Boolean write SetJump;
    property Dash: Boolean write FDash;
  end;

  TForm1 = class(TForm)
    UPDATE_INTERVAL: TTimer;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure UPDATE_INTERVALTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private 宣言 }
    Field: TDataField;
    Player: TPlayer;
    time: integer;
    size: integer;
  public
    { Public 宣言 }
    procedure Init;
  end;

const
  UPDATE_FPS = 60;
  kasoku = 0.5;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses update;

var
  FPSThread: TUpdate;

  { TDataField }

constructor TDataField.Create(const str: AnsiString);
var
  cnt: integer;
begin
  cnt := 1;
  for var j := 0 to 15 do
    for var i := 0 to 254 do
    begin
      FField[i, j] := str[cnt];
      inc(cnt);
    end;
  henkan['t'] := 'Y';
  henkan['m'] := 'へ';
  henkan['p'] := '□';
  henkan['b'] := '■';
  henkan['q'] := '？';
  henkan['c'] := '~';
  henkan[' '] := '　';
end;

function TDataField.GetStrings(X, Y: integer): Char;
begin
  if (X < 0) or (255 < X) or (Y < 0) or (15 < Y) then
    result := 'X'
  else
    result := henkan[FField[X, Y]];
end;

{ TPlayer }
const
  delta = 10;

var
  arr: array of Char = ['X', '□', '■', '？'];

function TPlayer.limitPlus(X, delta, MAX: Single): Single;
begin
  if FDash then
    MAX := MAX * 1.7;
  if X + delta > MAX then
    result := MAX
  else if X + delta < -MAX then
    result := -MAX
  else if (X * delta < 0) and (X * (X + delta) < 0) then
    result := 0
  else
    result := X + delta;
end;

procedure TPlayer.move;
var
  i: Single;
  n: integer;
begin
  n := 0;
  if FDash then
    SetSpeed(1.7 * Kasoku_X, 1.7 * FKasoku_Y)
  else
    SetSpeed(FKasoku_X, FKasoku_Y);
  if (Kasoku_X = 0) and (FSpeed_X <> 0) then
    FSpeed_X := 0.9 * FSpeed_X;
  FX := FX + FSpeed_X;
  FY := FY + FSpeed_Y;
  if FSpeed_X <> 0 then
  begin
    i := FY + 0.4;
    if FSpeed_X < 0 then
      n := Floor(FX)
    else if FSpeed_X > 0 then
      n := Floor(FX) + 1;
    if Form1.Field[n, Round(i)].IsInArray(arr) then
    begin
      FX := Round(FX);
      FSpeed_X := 0;
    end;
  end;
  i := FX + 0.4;
  if (FSpeed_Y < 0) and Form1.Field[Round(i), Ceil(FY)].IsInArray(arr) then
  // めり込んだ状態で判定開始
  begin
    FY := Ceil(FY);
    FSpeed_Y := 0;
  end;
  FJump := not Form1.Field[Round(i), Floor(FY + 1)].IsInArray(arr);
  if FJump then
    Kasoku_Y := 0.08 * kasoku
  else
  begin
    FY := Floor(FY);
    Kasoku_Y := 0;
    FSpeed_Y := 0;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  str: AnsiString; // 秀逸なマップ描画形式
begin
  str := '                                                                                                                                                                                                                                                               '
    + '                                                                                                                                                                                                                                                               '
    + '                                                                                                                                                                                                                                                               '
    + '                                                                                   cccc                                                                                                                                                                        '
    + '                   ccc              cccc                          ccc              cccc                 cccc                                                                                                                                                   '
    + '                   ccc     ccccc    cccc               ccc        ccc                                   cccc                                                                                                                                                   '
    + '                           ccccc                       ccc                     bbbbbbbb   bbbq                                                                                                                                                                 '
    + '                      q                                                                                                                                                                                                                                        '
    + '                                                                                                                                                                                                                                                               '
    + '                                                                                                                                                                                                                                                               '
    + '                                                                            bqb              b      bb                                                                                                                                                         '
    + '  m             q   bqbqb             pp     pp  m      pp                                       m                                                                                                                                                             '
    + ' mmm                        pp        pp     pp mmm     pp      m                               mmm                                                                                                                                                            '
    + 'mmmmm      tttttmmm    ttt  pp        pp ttttppmmmmm    pptttttmmm    ttt               tttt   mmmmm                                                                                                                                                           '
    + 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb  bbbbbbbbbbbbbbb   bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
    + 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb  bbbbbbbbbbbbbbb   bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb';
  Player := TPlayer.Create;
  Field := TDataField.Create(str);
  FPSThread := TUpdate.Create;
  Init;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Player.Free;
  Field.Free;
  FPSThread.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_LEFT:
      Player.Kasoku_X := -kasoku;
    VK_RIGHT:
      Player.Kasoku_X := kasoku;
    VK_DOWN:
      ;
    VK_UP:
      Player.Jump := true;
    VK_SPACE:
      Player.FDash := true;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE:
      Player.FDash := false;
  else
    Player.Kasoku_X := 0;
    Player.Kasoku_Y := 0;
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  for var j := 0 to 15 do
    for var i := 0 to 255 do
      if (i - Player.X) * size < ClientWidth then
        Canvas.TextOut(Round((i - Player.X + delta) * size), j * size,
          Field[i, j]);
end;

procedure TForm1.Init;
begin
  Player.X := 4;
  Player.Y := 13;
  size := ClientHeight div 20;
  Canvas.Font.Height := size;
  Shape1.Width := size;
  Shape1.Height := size;
  time := 5;
end;

procedure TForm1.UPDATE_INTERVALTimer(Sender: TObject);
var
  n: integer;
begin
  Player.move;
  dec(time);
  Refresh;
  n := MIN(delta * size, Round(Player.X * size));
  Shape1.Left := n;
  Shape1.Top := Round(Player.Y * size);
end;

procedure TPlayer.SetJump(const Value: Boolean);
begin
  if not FJump then
    Kasoku_Y := -kasoku;
end;

procedure TPlayer.SetSpeed(X, Y: Single);
begin
  FSpeed_X := limitPlus(FSpeed_X, X, MAX_SPEED);
  FSpeed_Y := limitPlus(FSpeed_Y, Y, 10 * MAX_SPEED);
end;

end.
