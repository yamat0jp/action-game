unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TCharState = (Run, afJump, bfChakuchi);

  TCheckRec = record
    top1, top2, side1, side2, under1, under2, center: TPointF;
  end;

  TPlayer = class
  private
    FMAX_SPEED: Single;
    FFull_Speed: Single;
    FX, FY: Single;
    FSpeed_X, FSpeed_Y: Single;
    FKasoku_X, FKasoku_Y: Single;
    FDash: Boolean;
    FState: TCharState;
    FGround: Boolean;
    function limitPlus(X, delta, MAX: Single): Single;
  protected
    procedure SetSpeed(X, Y: Single);
    property Ground: Boolean read FGround write FGround;
  public
    procedure Jump;
    procedure CheckPoints(out CheckRec: TCheckRec; size: integer);
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property Kasoku_X: Single read FKasoku_X write FKasoku_X;
    property Kasoku_Y: Single read FKasoku_Y write FKasoku_Y;
    property Dash: Boolean read FDash write FDash;
    property Speed_X: Single read FSpeed_X;
    property Speed_Y: Single read FSpeed_Y;
    property MAX_SPEED: Single read FMAX_SPEED write FMAX_SPEED;
    property Full_Speed: Single read FFull_Speed write FFull_Speed;
    property State: TCharState read FState write FState;
  end;

  TDataField = class
  const
    scroll = 10;
  private
    FPlayers: TArray<TPlayer>;
    FField: array [0 .. 255, 0 .. 15] of Char;
    henkan: array [Char] of Char;
    FSize: integer;
    FDelta: Single;
    function GetStrings(X, Y: integer): Char;
    function CheckHeadBlock(player: TPlayer): Boolean;
    function CheckJump(player: TPlayer): Boolean;
    function CheckState(player: TPlayer): TCharState;
    function CheckSideBlock(player: TPlayer): Boolean;
  protected
    function IsBlock(position: TPointF): Boolean;
  public
    constructor Create(const str: string; players: TArray<TPlayer>;
      const size: integer = 30);
    procedure Move;
    procedure GetImage(var Image: TBitmap);
    property Strings[X, Y: integer]: Char read GetStrings; default;
    property delta: Single read FDelta;
  end;

  TForm3 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
  private
    { private êÈåæ }
    field: TDataField;
    player: TPlayer;
    buff: TBitmap;
  public
    { public êÈåæ }
    procedure UPDATE_INTERVALTimer(Sender: TObject);
  end;

const
  UPDATE_FPS = 60;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses update, System.Math, System.Character, System.Threading;

var
  FPSThread: TUpdate;
  arr: array of Char = ['X', 'Å†', 'Å°', 'ÅH'];
  kasoku: Single;

  { TDataField }

function TDataField.CheckHeadBlock(player: TPlayer): Boolean;
var
  rec: TCheckRec;
begin
  player.CheckPoints(rec, FSize);
  if (player.Speed_Y < 0) and IsBlock(rec.top1) or IsBlock(rec.top2) then
  begin
    player.Y := Ceil(rec.top1.Y / FSize) * FSize;
    player.FSpeed_Y := 0;
    result := true;
  end
  else
    result := false;
end;

function TDataField.CheckJump(player: TPlayer): Boolean;
var
  rec: TCheckRec;
begin
  player.CheckPoints(rec, FSize);
  if (player.Speed_Y >= 0) and IsBlock(rec.under1) and IsBlock(rec.under2) and
    not IsBlock(rec.center) then
  begin
    player.Y := Floor(rec.under1.Y / FSize) * FSize - FSize;
    player.FSpeed_Y := 0;
    player.Ground := true;
    result := false;
  end
  else if not player.Ground and (player.Speed_Y >= 0) and IsBlock(rec.center)
  then
  begin
    player.Y := Floor(rec.center.Y / FSize) * FSize - FSize;
    player.FSpeed_Y := 0;
    player.Ground := true;
    result := false;
  end
  else
  begin
    player.Ground := false;
    result := true;
  end;
end;

function TDataField.CheckSideBlock(player: TPlayer): Boolean;
var
  rec: TCheckRec;
begin
  player.CheckPoints(rec, FSize);
  if ((player.Speed_X < 0) and IsBlock(rec.side1)) or
    ((player.Speed_X > 0) and IsBlock(rec.side2)) then
  begin
    if player.Speed_X < 0 then
      player.X := Ceil(rec.side1.X / FSize) * FSize
    else
      player.X := Floor(rec.side2.X / FSize) * FSize - FSize - 1;
    player.FSpeed_X := 0;
    result := true;
  end
  else
    result := false;
end;

function TDataField.CheckState(player: TPlayer): TCharState;
begin
  if not player.Ground and (player.Speed_Y >= 0) then
    result := bfChakuchi
  else if not player.Ground and (player.Speed_Y < 0) then
    result := afJump
  else
    result := Run;
end;

constructor TDataField.Create(const str: string; players: TArray<TPlayer>;
  const size: integer = 30);
var
  cnt: integer;
begin
  FSize := size;
  kasoku := 0.15 * size;
  cnt := 1;
  for var j := 0 to 15 do
    for var i := 0 to 254 do
    begin
      FField[i, j] := str[cnt];
      inc(cnt);
    end;
  henkan['t'] := 'Y';
  henkan['m'] := 'Ç÷';
  henkan['p'] := 'Å†';
  henkan['b'] := 'Å°';
  henkan['q'] := 'ÅH';
  henkan['c'] := '~';
  henkan[' '] := 'Å@';
  FPlayers := players;
  for var i := 0 to High(players) do
  begin
    players[i].X := i * FSize;
    players[i].Y := 13 * FSize;
    players[i].MAX_SPEED := size * 0.2;
    players[i].Kasoku_Y := kasoku;
    players[i].Full_Speed := 2.0;
  end;
end;

procedure TDataField.GetImage(var Image: TBitmap);
var
  a, b: Single;
  rect: TRectF;
  chrec: TCheckRec;
begin
  if Image.Canvas.BeginScene then
    try
      Image.Canvas.Fill.Color := TAlphaColors.Black;
      Image.Canvas.Font.size := FSize;
      Image.Canvas.FillRect(TRect.Create(0, 0, Image.Width, Image.Height), 1);
      Image.Canvas.Fill.Color := TAlphaColors.White;
      for var j := 0 to 15 do
        for var i := 0 to 255 do
        begin
          a := i * FSize;
          b := j * FSize;
          if (a - FDelta < 0) or (a - FDelta > Image.Width) then
            continue;
          rect := TRectF.Create(a - FDelta, b, a - FDelta + FSize, b + FSize);
          Image.Canvas.FillText(rect, Strings[i, j], false, 1.0, [],
            TTextAlign.center);
        end;
      for var boy in FPlayers do
      begin
        a := boy.X - FDelta;
        b := boy.Y;
        Image.Canvas.FillText(TRectF.Create(a, b, a + FSize, b + FSize), 'A',
          false, 1, [], TTextAlign.center);
        boy.CheckPoints(chrec, FSize);
        with chrec do
        begin
          top1.X := top1.X - FDelta;
          top2.X := top2.X - FDelta;
          side1.X := side1.X - FDelta;
          side2.X := side2.X - FDelta;
          under1.X := under1.X - FDelta;
          under2.X := under2.X - FDelta;
          center.X := center.X - FDelta;
        end;
        Image.Canvas.Stroke.Thickness := 5;
        Image.Canvas.Stroke.Color := TAlphaColors.Green;
        Image.Canvas.DrawLine(chrec.top1, chrec.top1, 1.0);
        Image.Canvas.DrawLine(chrec.top2, chrec.top2, 1.0);
        Image.Canvas.DrawLine(chrec.side1, chrec.side1, 1.0);
        Image.Canvas.DrawLine(chrec.side2, chrec.side2, 1.0);
        Image.Canvas.DrawLine(chrec.under1, chrec.under1, 1.0);
        Image.Canvas.DrawLine(chrec.under2, chrec.under2, 1.0);
        Image.Canvas.Stroke.Color := TAlphaColors.Red;
        Image.Canvas.DrawLine(chrec.center, chrec.center, 1.0);
      end;
    finally
      Image.Canvas.EndScene;
    end;
end;

function TDataField.GetStrings(X, Y: integer): Char;
begin
  if (X < 0) or (255 < X) or (Y < 0) or (15 < Y) then
    result := 'X'
  else
    result := henkan[FField[X, Y]];
end;

function TDataField.IsBlock(position: TPointF): Boolean;
var
  i, j: integer;
begin
  i := Floor(position.X / FSize);
  j := Floor(position.Y / FSize);
  result := Strings[i, j].IsInArray(arr);
end;

procedure TDataField.Move;
  procedure main(player: TPlayer);
  begin
    player.X := player.X + player.Speed_X;
    player.Y := player.Y + player.Speed_Y;
    if player.X > scroll * FSize then
      FDelta := player.X - scroll * FSize;
  end;

begin
  for var boy in FPlayers do
    case CheckState(boy) of
      Run:
        begin
          if boy.Kasoku_X = 0 then
            boy.FSpeed_X := 0.3 * boy.Speed_X;
          boy.SetSpeed(boy.Kasoku_X, 0);
          main(boy);
          CheckJump(boy);
          CheckSideBlock(boy);
        end;
      afJump:
        begin
          boy.SetSpeed(0, kasoku);
          main(boy);
          CheckHeadBlock(boy);
          CheckSideBlock(boy);
        end;
      bfChakuchi:
        begin
          boy.SetSpeed(boy.Kasoku_X, kasoku);
          main(boy);
          CheckJump(boy);
          CheckSideBlock(boy);
        end;
    end;
end;

{ TPlayer }

procedure TPlayer.CheckPoints(out CheckRec: TCheckRec; size: integer);
begin
  CheckRec.top1 := TPointF.Create(X + size / 4, Y);
  CheckRec.top2 := TPointF.Create(X + size - size / 4, Y);
  CheckRec.side1 := TPointF.Create(X, Y + size / 2);
  CheckRec.side2 := TPointF.Create(X + size, Y + size / 2);
  CheckRec.under1 := TPointF.Create(X + size / 4, Y + size);
  CheckRec.under2 := TPointF.Create(X + size - size / 4, Y + size);
  CheckRec.center := TPointF.Create(X + size / 2, Y + size / 5);
end;

procedure TPlayer.Jump;
begin
  if FGround then
  begin
    FGround := false;
    SetSpeed(0, -9 * kasoku);
  end;
end;

function TPlayer.limitPlus(X, delta, MAX: Single): Single;
begin
  if FDash then
    MAX := MAX * FFull_Speed;
  if X + delta > MAX then
    result := MAX
  else if X + delta < -MAX then
    result := -MAX
  else
    result := X + delta;
end;

procedure TPlayer.SetSpeed(X, Y: Single);
begin
  FSpeed_X := limitPlus(FSpeed_X, X, MAX_SPEED);
  FSpeed_Y := limitPlus(FSpeed_Y, Y, 5.5 * MAX_SPEED);
end;

{ TForm3 }

procedure TForm3.FormCreate(Sender: TObject);
var
  str: string;
  size: integer;
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
  size := ClientHeight div 16;
  player := TPlayer.Create;
  field := TDataField.Create(str, [player], size);
  FPSThread := TUpdate.Create;
  buff := TBitmap.Create(ClientWidth, ClientHeight);
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FPSThread.Free;
  player.Free;
  field.Free;
  buff.Free;
end;

procedure TForm3.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  case Key of
    VKLEFT:
      player.Kasoku_X := -kasoku;
    VKRIGHT:
      player.Kasoku_X := kasoku;
    VKUP:
      player.Jump;
  end;
  if (KeyChar = ' ') and player.Ground then
    player.Dash := true;
end;

procedure TForm3.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  case Key of
    VKLEFT, VKRIGHT:
      player.Kasoku_X := 0;
  end;
  if KeyChar = ' ' then
    player.Dash := false;
end;

procedure TForm3.UPDATE_INTERVALTimer(Sender: TObject);
begin
  field.Move;
  field.GetImage(buff);
  if Canvas.BeginScene then
    try
      Canvas.DrawBitmap(buff, ClientRect, ClientRect, 1, true);
    finally
      Canvas.EndScene;
    end;
end;

end.
