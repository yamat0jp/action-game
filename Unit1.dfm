object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 734
  ClientWidth = 1111
  Color = clBackground
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnPaint = FormPaint
  TextHeight = 15
  object Shape1: TShape
    Left = 528
    Top = 352
    Width = 65
    Height = 65
  end
  object UPDATE_INTERVAL: TTimer
    Enabled = False
    Interval = 16
    OnTimer = UPDATE_INTERVALTimer
    Left = 256
    Top = 112
  end
end
