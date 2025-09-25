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
          .GridSetRowOffset(1, 15)
          .GridSetRowOffset(2, 50)
          .GridCellStrechAll
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('Q'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('W'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('E'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('R'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('T'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('Y'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('U'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('I'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('O'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('P'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('A'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('S'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('D'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('F'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('G'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('H'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('J'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('K'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('L'))
          .GridSkipCell
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('Z'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('X'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('C'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('V'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('B'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('N'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('M'))
          .GridSkipCells(5)
          .GridColSpan(5)
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('[ SPACE ]'))
        .GridFinish
        .IncLeft(20)
        .GridInit(4, 3)
          .GridSetCellWidthAndHeight(80, 80)
          .GridCellStrechAll
          .GridCellPosition(cpLeft)
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('7'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('8'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('9'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('4'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('5'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('6'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('1'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('2'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('3'))
          .GridColSpan(2)
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption('0'))
          .AddControl(TControlInfo.Create(TSpeedButton).WithCaption(','))
        .GridFinish
        .RecalcParentSize(10, 10)
      .SuperLevel
    ;

  finally
    ControlBuilder.Free;
  end;
end;

end.
