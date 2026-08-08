unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OPCB;

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
begin
  ControlBuilder := TControlCreator.Create;
  try
    ControlBuilder
      .SetOwnerAndParent(Self, Self)
      .SetTopLeft(20, 20)
      .SetSpace(5, 5)
      .GridInit(2, 2)
        .GridSetCellWidthAndHeight(70, 70)
        .Add(TControlBuilder.Create(TPanel).WithCaption('1'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('2'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('3'))
        .Add(TControlBuilder.Create(TPanel).WithCaption('4'))
      .GridFinish
      .Break
    ;
  finally
    ControlBuilder.Free;
  end;
end;

end.
