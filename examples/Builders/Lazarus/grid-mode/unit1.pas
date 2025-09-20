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
  ControlBuilder: TControlBuilder;
begin
  ControlBuilder := TControlBuilder.Create;
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
        .AddControl(TControlInfo.Create(TPanel).WithCaption('1'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('2'))
        .SubLevel(TControlInfo.Create(TPanel).WithCaption('3'))
          .SetDirection(cpdHorizontal)
          .AddControl(TControlInfo.Create(TButton).WithCaption('B1'))
          .AddControl(TControlInfo.Create(TButton).WithCaption('B2'))
        .SuperLevel
        .AddControl(TControlInfo.Create(TPanel).WithCaption('4'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('5'))
        .SetDirection(cpdHorizontal)
        .GridColSpan(2)
        .GridRowSpan(2)
        .AddControl(TControlInfo.Create(TPanel).WithCaption('6'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('7'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('8'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('9'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('10'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('11'))
        .GridSkipCell
        .GridColSpan(1)
        .AddControl(TControlInfo.Create(TPanel).WithCaption('12'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('13'))
        .GridRowSpan(2)
        .GridColSpan(1)
        .AddControl(TControlInfo.Create(TPanel).WithCaption('14'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('15'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('16'))
        .AddControl(TControlInfo.Create(TPanel).WithCaption('17'))
        .GridSkipCell
        .AddControl(TControlInfo.Create(TPanel).WithCaption('18'))
      .GridFinish
    ;
  finally
    ControlBuilder.Free;
  end;
end;

end.

