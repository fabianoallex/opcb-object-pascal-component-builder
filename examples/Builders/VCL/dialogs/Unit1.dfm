object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 137
    Height = 441
    Align = alLeft
    ParentBackground = False
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 24
      Width = 105
      Height = 25
      Caption = 'TListBox'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 55
      Width = 105
      Height = 25
      Caption = 'TMaskEdit'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 8
      Top = 86
      Width = 105
      Height = 25
      Caption = 'TColorBox'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 8
      Top = 117
      Width = 105
      Height = 25
      Caption = 'TCalendarView'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 8
      Top = 148
      Width = 105
      Height = 25
      Caption = 'TStringGrid'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 8
      Top = 179
      Width = 105
      Height = 25
      Caption = 'TTrackBar'
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 8
      Top = 210
      Width = 105
      Height = 25
      Caption = 'Custom'
      TabOrder = 6
      OnClick = Button7Click
    end
  end
end
