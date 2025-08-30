unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TCharState = (Run, afJump, bfChakuchi);

  TPlayer = class
  private
    FMAX_SPEED: Single;
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
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property Kasoku_X: Single read FKasoku_X write FKasoku_X;
    property Kasoku_Y: Single read FKasoku_Y write FKasoku_Y;
    property Dash: Boolean read FDash write FDash;
    property Speed_X: Single read FSpeed_X write FSpeed_X;
    property Speed_Y: Single read FSpeed_Y write FSpeed_Y;
    property MAX_SPEED: Single read FMAX_SPEED write FMAX_SPEED;
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
  public
    constructor Create(const str: string; players: TArray<TPlayer>;
      const size: integer = 30);
    destructor Destroy; override;
    procedure Move;
    procedure GetImage(var Image: TBitmap);
    property Strings[X, Y: integer]: Char read GetStrings; default;
    property size: integer read FSize;
    property delta: Single read FDelta;
  end;

  TForm3 = class(TForm)
    Shape1: TRectangle;
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
    procedure Init;
  end;

const
  UPDATE_FPS = 60;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses update, System.Math, System.Character;

var
  FPSThread: TUpdate;
  arr: array of Char = ['X', 'Å†', 'Å°', 'ÅH'];
  kasoku: Single;

  { TDataField }

function TDataField.CheckHeadBlock(player: TPlayer): Boolean;
var
  i, j: Single;
begin
  i := player.X / FSize;
  j := player.Y / FSize;
  if (player.Speed_Y < 0) and Strings[Round(i), Ceil(j)].IsInArray(arr) then
  begin
    player.Y := Ceil(j) * FSize;
    player.Speed_Y := 0;
    result := true;
  end
  else
    result := false;
end;

function TDataField.CheckJump(player: TPlayer): Boolean;
var
  i, j: Single;
begin
  i := player.X / FSize;
  j := player.Y / FSize;
  if not player.Ground then
  begin
    if (player.Speed_Y >= 0) and Strings[Round(i), Floor(j) + 1].IsInArray(arr)
    then
    begin
      player.Y := Floor(j) * FSize;
      player.Speed_Y := 0;
      player.Ground := true;
      result := false;
    end
    else
    begin
      player.Ground := false;
      result := true;
    end;
  end
  else
    result := false;
end;

function TDataField.CheckSideBlock(player: TPlayer): Boolean;
var
  n, m: integer;
begin
  result := false;
  if player.Speed_X <> 0 then
  begin
    m := Round(player.Y / FSize);
    if player.Speed_X < 0 then
      n := Floor(player.X / FSize)
    else
      n := Floor(player.X / FSize) + 1;
    if Strings[n, m].IsInArray(arr) then
    begin
      player.X := Round(player.X / FSize) * FSize;
      player.Speed_X := 0;
      result := true;
    end;
  end;
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
  for var boy in players do
  begin
    boy.MAX_SPEED := size * 0.2;
    boy.Kasoku_Y := kasoku;
  end;
end;

destructor TDataField.Destroy;
begin
  for var boy in FPlayers do
    boy.Free;
  Finalize(FPlayers);
  inherited;
end;

procedure TDataField.GetImage(var Image: TBitmap);
var
  a, b, c: Single;
begin
  if Image.Canvas.BeginScene then
    try
      Image.Canvas.Fill.Color := TAlphaColors.Black;
      Image.Canvas.FillRect(TRect.Create(0, 0, Image.Width, Image.Height), 1);
      Image.Canvas.Font.size := FSize;
      Image.Canvas.Fill.Color := TAlphaColors.White;
      for var j := 0 to 15 do
        for var i := 0 to 29 do
        begin
          a := i * FSize;
          b := j * FSize;
          c := FDelta / FSize;
          Image.Canvas.FillText(TRectF.Create(a - FDelta, b, a - FDelta + FSize,
            b + FSize), Strings[i + Round(c), j], false, 1.0, [],
            TTextAlign.Center);
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

procedure TDataField.Move;
  procedure main(player: TPlayer);
  begin
    player.X := player.X + player.Speed_X;
    player.Y := player.Y + player.Speed_Y;
    if player.X > scroll * FSize then
    begin
      FDelta := player.X - scroll * FSize;
      player.X := scroll * FSize;
    end;
  end;

begin
  for var boy in FPlayers do
    case CheckState(boy) of
      Run:
        begin
          if boy.Kasoku_X = 0 then
            boy.Speed_X := 0.9 * boy.Speed_X
          else if boy.Dash then
            boy.SetSpeed(1.7 * boy.Kasoku_X, 0)
          else
            boy.SetSpeed(boy.Kasoku_X, 0);
          main(boy);
          CheckSideBlock(boy);
          CheckJump(boy);
        end;
      afJump:
        begin
          boy.SetSpeed(boy.Kasoku_X, kasoku);
          main(boy);
          CheckHeadBlock(boy);
          CheckSideBlock(boy);
        end;
      bfChakuchi:
        begin
          boy.SetSpeed(0.01 * boy.Kasoku_X, kasoku);
          main(boy);
          CheckJump(boy);
        end;
    end;
end;

{ TPlayer }

procedure TPlayer.Jump;
begin
  if FGround then
  begin
    FGround := false;
    FSpeed_Y := -7 * kasoku;
  end;
end;

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

procedure TPlayer.SetSpeed(X, Y: Single);
begin
  FSpeed_X := limitPlus(FSpeed_X, X, MAX_SPEED);
  FSpeed_Y := limitPlus(FSpeed_Y, Y, 5 * MAX_SPEED);
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
  Init;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FPSThread.Free;
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
    VKDOWN:
      ;
    vkUP:
      player.Jump;
    VKSPACE:
      player.Dash := true;
  end;
end;

procedure TForm3.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  case Key of
    VKSPACE:
      player.FDash := false;
  else
    player.Kasoku_X := 0;
  end;
end;

procedure TForm3.Init;
begin
  player.X := 0;
  player.Y := 13 * field.size;
  Shape1.Width := field.size;
  Shape1.Height := field.size;
end;

procedure TForm3.UPDATE_INTERVALTimer(Sender: TObject);
var
  n: Single;
begin
  field.Move;
  field.GetImage(buff);
  if Canvas.BeginScene then
    try
      Canvas.DrawBitmap(buff, ClientRect, ClientRect, 1, true);
    finally
      Canvas.EndScene;
    end;
  n := MIN(player.X, player.X - field.delta);
  Shape1.Position.X := n;
  Shape1.Position.Y := player.Y;
end;

end.
