unit Field;

interface

uses System.Classes, System.Types, System.UITypes, System.SysUtils, FMX.Types,
  FMX.StdCtrls, FMX.Controls, FMX.Graphics, update;

type
  TCharState = (Run, afJump, bfChakuchi, Sprite);

  TCheckRec = record
    top1, top2, side1, side2, under1, under2, center: TPointF;
  end;

  TDataField = class;

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
    FVisible: Boolean;
    FLetter: Char;
    FOnBeginOut: TNotifyEvent;
    FParent: TDataField;
    function limitPlus(X, delta, MAX: Single): Single;
  protected
    procedure SetSpeed(X, Y: Single);
    property Letter: Char read FLetter write FLetter;
    property Parent: TDataField read FParent write FParent;
  public
    constructor Create(AOwner: TDataField);
    procedure Jump;
    procedure CheckPoints(out CheckRec: TCheckRec);
    procedure GameOver(Pop: Boolean);
    property Ground: Boolean read FGround write FGround;
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
    property Visible: Boolean read FVisible write FVisible;
    property OnBeginOut: TNotifyEvent read FOnBeginOut write FOnBeginOut;
  end;

  TDataField = class(TPanel)
  const
    hei = 16;
    wid = 256;
    scroll = 10;
  private
    FPlayers: TArray<TPlayer>;
    FField: array [0 .. wid - 1, 0 .. hei - 1] of Char;
    henkan: array [Char] of Char;
    FFieldSize: integer;
    FDelta: Single;
    FKasoku: Single;
    FFPSThread: TUpdate;
    FOnInterval: TNotifyEvent;
    function GetStrings(X, Y: integer): Char;
    function CheckHeadBlock(player: TPlayer): Boolean;
    function CheckJump(player: TPlayer): Boolean;
    function CheckState(player: TPlayer): TCharState;
    function CheckSideBlock(player: TPlayer): Boolean;
    function GetPlayers(index: integer): TPlayer;
    procedure SetFieldData(const Value: string);
    function GetTerminate: TNotifyEvent;
    procedure SetTerminate(const Value: TNotifyEvent);
  protected
    function IsBlock(position: TPointF): Boolean;
    function IsGameOver(player: TPlayer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PlayerMove; virtual;
    procedure DrawImage; virtual;
    procedure CreatePlayer;
    procedure UPDATE_INTERVALTimer(Sender: TObject); virtual;
    property Strings[X, Y: integer]: Char read GetStrings; default;
    property delta: Single read FDelta;
    property Players[index: integer]: TPlayer read GetPlayers;
    property kasoku: Single read FKasoku write FKasoku;
  published
    property FieldData: string write SetFieldData;
    property FieldSize: integer read FFieldSize write FFieldSize;
    property OnInterval: TNotifyEvent read FOnInterval write FOnInterval;
    property OnTerminate: TNotifyEvent read GetTerminate write SetTerminate;
  end;

const
  UPDATE_FPS = 60;

implementation

uses System.Math, System.Character, System.Threading;

var
  arr: array of Char = ['Å†', 'Å°', 'ÅH'];

  { TDataField }

function TDataField.CheckHeadBlock(player: TPlayer): Boolean;
var
  rec: TCheckRec;
begin
  player.CheckPoints(rec);
  if (player.Speed_Y < 0) and IsBlock(rec.top1) or IsBlock(rec.top2) then
  begin
    player.Y := Ceil(rec.top1.Y / FFieldSize) * FFieldSize;
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
  player.CheckPoints(rec);
  if (player.Speed_Y >= 0) and IsBlock(rec.under1) and IsBlock(rec.under2) and
    not IsBlock(rec.center) then
  begin
    player.Y := Floor(rec.under1.Y / FFieldSize) * FFieldSize - FFieldSize;
    player.FSpeed_Y := 0;
    player.Ground := true;
    result := false;
  end
  else if not player.Ground and (player.Speed_Y >= 0) and IsBlock(rec.center)
  then
  begin
    player.Y := Floor(rec.center.Y / FFieldSize) * FFieldSize - FFieldSize;
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
  player.CheckPoints(rec);
  if ((player.Speed_X < 0) and IsBlock(rec.side1)) or
    ((player.Speed_X > 0) and IsBlock(rec.side2)) then
  begin
    if player.Speed_X < 0 then
      player.X := Ceil(rec.side1.X / FFieldSize) * FFieldSize
    else
      player.X := Floor(rec.side2.X / FFieldSize) * FFieldSize - FFieldSize - 1;
    player.FSpeed_X := 0;
    result := true;
  end
  else
    result := false;
end;

function TDataField.CheckState(player: TPlayer): TCharState;
begin
  if player.State = Sprite then
    Exit(player.State);
  if not player.Ground and (player.Speed_Y >= 0) then
    result := bfChakuchi
  else if not player.Ground and (player.Speed_Y < 0) then
    result := afJump
  else
    result := Run;
end;

constructor TDataField.Create(AOwner: TComponent);
begin
  inherited;
  Align := TAlignLayout.Client;
  FFieldSize := 30;
  FieldData :=
    '                                                                                                                                                                                                                                                                '
    + '                                                                                                                                                                                                                                                                '
    + '                                                                                                                                                                                                                                                                '
    + '                                                                                   cccc                                                                                                                                                                         '
    + '                   ccc              cccc                          ccc              cccc                 cccc                                                                                                                                                    '
    + '                   ccc     ccccc    cccc               ccc        ccc                                   cccc                                                                                                                                                    '
    + '                           ccccc                       ccc                     bbbbbbbb   bbbq                                                                                                                                                                  '
    + '                      q                                                                                                                                                                                                                                         '
    + '                                                                                                                                                                                                                                                                '
    + '                                                                                                                                                                                                                                                                '
    + '                                                                            bqb              b      bb                                                                                                                                                          '
    + '  m             q   bqbqb             pp     pp  m      pp                                       m                                                                                                                                                              '
    + ' mmm                        pp        pp     pp mmm     pp      m                               mmm                                                                                                                                                             '
    + 'mmmmm      tttttmmm    ttt  pp        pp ttttppmmmmm    pptttttmmm    ttt               tttt   mmmmm                                                                                                                                                            '
    + 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb  bbbbbbbbbbbbbbb   bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
    + 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb  bbbbbbbbbbbbbbb   bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb';
  kasoku := 0.15 * FFieldSize;
  henkan['t'] := 'Y';
  henkan['m'] := 'Ç÷';
  henkan['p'] := 'Å†';
  henkan['b'] := 'Å°';
  henkan['q'] := 'ÅH';
  henkan['c'] := '~';
  henkan[' '] := 'Å@';
  henkan['X'] := 'X';
  CreatePlayer;
  for var i := 0 to High(FPlayers) do
  begin
    FPlayers[i].X := i * FFieldSize;
    FPlayers[i].Y := 13 * FFieldSize;
    FPlayers[i].MAX_SPEED := FFieldSize * 0.2;
  end;
  FFPSThread := TUpdate.Create(Self);
end;

procedure TDataField.CreatePlayer;
begin
  FPlayers := FPlayers + [TPlayer.Create(Self)];
end;

destructor TDataField.Destroy;
begin
  for var player in FPlayers do
    player.Free;
  FFPSThread.Free;
  inherited;
end;

function TDataField.IsGameOver(player: TPlayer): Boolean;
var
  X, Y: Single;
begin
  X := player.X + FFieldSize / 2;
  Y := player.Y + FFieldSize / 2;
  result := (Strings[Floor(X / FFieldSize), Floor(Y / FFieldSize)] = 'X') and
    (player.Letter <> 'X');
end;

procedure TDataField.DrawImage;
var
  a, b: Single;
  rect: TRectF;
  chrec: TCheckRec;
begin
  if Canvas.BeginScene then
    try
      Canvas.Fill.Color := TAlphaColors.Black;
      Canvas.Font.size := FFieldSize;
      Canvas.FillRect(TRect.Create(0, 0, Canvas.Width, Canvas.Height), 1);
      Canvas.Fill.Color := TAlphaColors.White;
      for var j := 0 to hei - 1 do
        for var i := 0 to wid - 1 do
        begin
          a := i * FFieldSize;
          b := j * FFieldSize;
          if (a - FDelta < 0) or (a - FDelta > Canvas.Width) then
            continue;
          rect := TRectF.Create(a - FDelta, b, a - FDelta + FFieldSize,
            b + FFieldSize);
          Canvas.FillText(rect, Strings[i, j], false, 1.0, [],
            TTextAlign.center);
        end;
      for var boy in FPlayers do
      begin
        if not boy.Visible then
          continue;
        a := boy.X - FDelta;
        b := boy.Y;
        Canvas.FillText(TRectF.Create(a, b, a + FFieldSize, b + FFieldSize),
          boy.Letter, false, 1, [], TTextAlign.center);
        boy.CheckPoints(chrec);
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
        Canvas.Stroke.Thickness := 5;
        Canvas.Stroke.Color := TAlphaColors.Green;
        Canvas.DrawLine(chrec.top1, chrec.top1, 1.0);
        Canvas.DrawLine(chrec.top2, chrec.top2, 1.0);
        Canvas.DrawLine(chrec.side1, chrec.side1, 1.0);
        Canvas.DrawLine(chrec.side2, chrec.side2, 1.0);
        Canvas.DrawLine(chrec.under1, chrec.under1, 1.0);
        Canvas.DrawLine(chrec.under2, chrec.under2, 1.0);
        Canvas.Stroke.Color := TAlphaColors.Red;
        Canvas.DrawLine(chrec.center, chrec.center, 1.0);
      end;
    finally
      Canvas.EndScene;
    end;
end;

function TDataField.GetPlayers(index: integer): TPlayer;
begin
  result := FPlayers[index];
end;

function TDataField.GetStrings(X, Y: integer): Char;
begin
  if (X < 0) or (wid - 1 < X) or (Y < 0) or (hei - 1 < Y) then
    result := 'X'
  else
    result := henkan[FField[X, Y]];
end;

function TDataField.GetTerminate: TNotifyEvent;
begin
  result := FFPSThread.OnTerminate;
end;

function TDataField.IsBlock(position: TPointF): Boolean;
var
  i, j: integer;
begin
  i := Floor(position.X / FFieldSize);
  j := Floor(position.Y / FFieldSize);
  result := Strings[i, j].IsInArray(arr);
end;

procedure TDataField.PlayerMove;
  procedure main(player: TPlayer);
  begin
    player.X := player.X + player.Speed_X;
    player.Y := player.Y + player.Speed_Y;
    if player.X > scroll * FFieldSize then
      FDelta := player.X - scroll * FFieldSize;
  end;

begin
  for var boy in FPlayers do
  begin
    if not boy.Visible then
      continue;
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
      Sprite:
        continue;
    end;
    if IsGameOver(boy) then
      boy.GameOver(boy.Y + FFieldSize > hei * FFieldSize);
  end;
end;

procedure TDataField.SetFieldData(const Value: string);
var
  cnt: integer;
begin
  cnt := 1;
  for var j := 0 to hei - 1 do
    for var i := 0 to wid - 1 do
    begin
      FField[i, j] := Value[cnt];
      inc(cnt);
    end;
end;

procedure TDataField.SetTerminate(const Value: TNotifyEvent);
begin
  FFPSThread.OnTerminate := Value;
end;

procedure TDataField.UPDATE_INTERVALTimer(Sender: TObject);
begin
  if Assigned(FOnInterval) then
    FOnInterval(Sender);
  for var player in FPlayers do
  begin
    if player.Visible then
      continue;
    FFPSThread.Terminate;
    break;
  end;
  PlayerMove;
  DrawImage;
end;

{ TPlayer }

procedure TPlayer.CheckPoints(out CheckRec: TCheckRec);
var
  size: integer;
begin
  size := Parent.FieldSize;
  CheckRec.top1 := TPointF.Create(X + size / 4, Y);
  CheckRec.top2 := TPointF.Create(X + size - size / 4, Y);
  CheckRec.side1 := TPointF.Create(X, Y + size / 2);
  CheckRec.side2 := TPointF.Create(X + size, Y + size / 2);
  CheckRec.under1 := TPointF.Create(X + size / 4, Y + size);
  CheckRec.under2 := TPointF.Create(X + size - size / 4, Y + size);
  CheckRec.center := TPointF.Create(X + size / 2, Y + size / 5);
end;

constructor TPlayer.Create(AOwner: TDataField);
begin
  inherited Create;
  FParent := AOwner;
  FKasoku_Y := AOwner.kasoku;
  FFull_Speed := 2.0;
  FLetter := 'A';
  FVisible := true;
end;

procedure TPlayer.GameOver(Pop: Boolean);
var
  kasoku: Single;
begin
  kasoku := Parent.kasoku;
  FState := Sprite;
  FLetter := 'X';
  FDash := false;
  FSpeed_X := 0;
  FKasoku_X := 0;
  FKasoku_Y := 0;
  TTask.Run(
    procedure
    begin
      if Assigned(FOnBeginOut) then
        FOnBeginOut(Self);
      if Pop then
      begin
        Sleep(50);
        FSpeed_Y := -3 * kasoku;
        for var i := 1 to 10 do
        begin
          FY := FY + FSpeed_Y;
          Sleep(10);
        end;
      end;
      Sleep(1000);
      FSpeed_X := kasoku;
      FSpeed_Y := -3 * kasoku;
      for var i := 1 to 50 do
      begin
        FX := FX + FSpeed_X;
        FY := FY + FSpeed_Y;
        FSpeed_Y := FSpeed_Y + kasoku;
        Sleep(50);
      end;
      FVisible := false;
    end);
end;

procedure TPlayer.Jump;
begin
  if FGround then
  begin
    FGround := false;
    SetSpeed(0, -9 * Parent.kasoku);
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

end.
