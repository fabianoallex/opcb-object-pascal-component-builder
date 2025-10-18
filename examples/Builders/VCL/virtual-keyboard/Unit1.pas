unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OPCB, Vcl.StdCtrls, Vcl.Buttons;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Vcl.ExtCtrls;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  ControlBuilder: TControlCreator;
  P: TPanel;
begin
  ControlBuilder := TControlCreator.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetSpace(2, 2)
      .SetTopLeft(20, 20)
      .SubLevel(TControlBuilder.Create(TPanel))
        .SetTopLeft(10, 10)
        .GridInit(4, 10)
          .GridSetCellWidthAndHeight(80, 80)
          .GridSetRowOffset(1, 22)
          .GridSetRowOffset(2, 65)
          .External(
            procedure(ABuilder: TControlCreator)
            const KeyRows: array[0..1] of string = ('QWERTYUIOPASDFGHJKL', 'ZXCVBNM');
            var Key: Char;
            begin
              for Key in KeyRows[0] do
                ABuilder.Add(TControlBuilder.Create(TSpeedButton).WithCaption(Key));
              ABuilder.Break;
              for Key in KeyRows[1] do
                ABuilder.Add(TControlBuilder.Create(TSpeedButton).WithCaption(Key));
              ABuilder.Break;
              ABuilder.GridSkipCells(2);
              ABuilder.GridColSpan(5);
              ABuilder.Add(TControlBuilder.Create(TSpeedButton).WithCaption('[ SPACE ]'));
            end)
        .GridFinish
        .IncLeft(20)
        .GridInit(4, 3)
          .GridSetCellWidthAndHeight(80, 80)
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('7'))
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('8'))
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('9'))
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('4'))
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('5'))
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('6'))
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('1'))
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('2'))
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('3'))
          .GridColSpan(2)
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption('0'))
          .Add(TControlBuilder.Create(TSpeedButton).WithCaption(','))
        .GridFinish
        .RecalcParentSize(10, 10)
      .SuperLevel
    ;
  finally
    ControlBuilder.Free;
  end;
end;

end.
