unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  OPCB;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  ControlBuilder: TControlsBuilder;
begin
  ControlBuilder := TControlsBuilder.Create;
  try
    ControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetTopLeft(20, 20)
      .SetSpace(5, 5)
      .SetDirection(cpdVertical)
      .GridInit(4, 6)
        .GridSetCellWidthAndHeight(70, 70)
        .GridSetColWidth(0, 200)
        .GridSetRowHeight(1, 150)
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('1'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('2'))
        .SubLevel(TControlBuilder.Create(TPanel).WithCaption('3'))
          .SetDirection(cpdHorizontal)
          .AddControl(TControlBuilder.Create(TButton).WithCaption('B1'))
          .AddControl(TControlBuilder.Create(TButton).WithCaption('B2'))
        .SuperLevel
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('4'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('5'))
        .SetDirection(cpdHorizontal)
        .GridColSpan(2)
        .GridRowSpan(2)
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('6'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('7'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('8'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('9'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('10'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('11'))
        .GridSkipCell
        .GridColSpan(1)
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('12'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('13'))
        .GridRowSpan(2)
        .GridColSpan(1)
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('14'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('15'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('16'))
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('17'))
        .GridSkipCell
        .AddControl(TControlBuilder.Create(TPanel).WithCaption('18'))
      .GridFinish
    ;
  finally
    ControlBuilder.Free;
  end;
end;

end.

