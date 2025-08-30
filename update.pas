unit update;

interface

uses
  System.Classes, Windows, System.SysUtils;

type
  TUpdate = class(TThread)
  private
    { Private 宣言 }
  protected
    procedure Execute; override;
  end;

implementation

{
  重要: ビジュアル コンポーネントにおけるオブジェクトのメソッドとプロパティは、Synchronize を使って
  呼び出されるメソッドでのみ使用できます。たとえば、次のようになります。

  Synchronize(UpdateCaption);

  UpdateCaption は、たとえば次のようなコードになります。

  procedure TUpdate.UpdateCaption;
  begin
  Form1.Caption := 'スレッドで更新されました';
  end;

  あるいは

  Synchronize(
  procedure
  begin
  Form1.Caption := '無名メソッドを通じてスレッドで更新されました'
  end
  )
  );

  ここでは、無名メソッドが渡されています。

  同様に、開発者は上記と同じようなパラメータで Queue メソッドを呼び出すことができます。
  ただし、別の TThread クラスを第 1 パラメータとして渡し、呼び出し元のスレッドを
  もう一方のスレッドでキューに入れます。

}

uses Unit3;

{ TUpdate }

procedure TUpdate.Execute;
var
  hTimer: THandle;
  DueTime: Int64;
begin
  { スレッドとして実行したいコードをここに記述してください }
  while not Terminated do
  begin
    hTimer := CreateWaitableTimer(nil, True, nil);
    DueTime := -Int64(1000 div Unit3.UPDATE_FPS) * 10000; // 負数で相対時間（100ns単位）
    SetWaitableTimer(hTimer, PLargeInteger(@DueTime)^, 0, nil, nil, False);
    WaitForSingleObject(hTimer, INFINITE);
    CloseHandle(hTimer);
    Synchronize(
      procedure
      begin
        Form3.UPDATE_INTERVALTimer(nil);
      end);
  end;
end;

end.
