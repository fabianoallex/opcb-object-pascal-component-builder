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
  ControlBuilder: TControlBuilder;
  P: TPanel;
begin
  ControlBuilder := TControlBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetSpace(2, 2)
      .SetTopLeft(20, 20)
      .SubLevel(TControlInfo.Create(TPanel))
        .SetTopLeft(10, 10)
        .GridInit(4, 10)
          .GridSetCellWidthAndHeight(80, 80)
          .GridSetRowOffset(1, 22)
          .GridSetRowOffset(2, 65)
          .External(procedure (ABuilder: TControlBuilder)
            const KeyRows: array[0..1] of string = ('QWERTYUIOPASDFGHJKL', 'ZXCVBNM');
            var Key: Char;
            begin
              for Key in KeyRows[0] do
                ABuilder.AddControl(TControlInfo.Create(TSpeedButton).WithCaption(Key));
              ABuilder.Break;
              for Key in KeyRows[1] do
                ABuilder.AddControl(TControlInfo.Create(TSpeedButton).WithCaption(Key));
              ABuilder.Break;
              ABuilder.GridSkipCells(2);
              ABuilder.GridColSpan(5);
              ABuilder.AddControl(TControlInfo.Create(TSpeedButton).WithCaption('[ SPACE ]'));
            end
          )
        .GridFinish
        .IncLeft(20)
        .GridInit(4, 3)
          .GridSetCellWidthAndHeight(80, 80)
          .External(procedure(ABuilder: TControlBuilder)
            const Keys = '789456123';
            var Key: Char;
            begin
              for Key in Keys do
                ABuilder.AddControl(TControlInfo.Create(TSpeedButton).WithCaption(Key));
              ABuilder.GridColSpan(2);
              ABuilder.AddControl(TControlInfo.Create(TSpeedButton).WithCaption('0'));
              ABuilder.AddControl(TControlInfo.Create(TSpeedButton).WithCaption(','));
            end
          )
        .GridFinish
        .RecalcParentSize(10, 10)
      .SuperLevel
    ;
  finally
    ControlBuilder.Free;
  end;
end;

end.
