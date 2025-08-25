unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  OPCB, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm3 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.FormCreate(Sender: TObject);
var
  Builders: TOPCBBuilders;
begin
  Builders := TOPCBBuilders.Create(Self.Name);

  try
    Builders.AsComponentBuilder
      .WithOwner(Self)
    ;

    Builders.AsControlBuilder
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .AddControls([
        TControlInfo.Create(TButton, 'B1'), TControlInfo.Create(TButton, 'B2'),
        TControlInfo.Create(TButton, 'B3'), TControlInfo.Create(TButton, 'B4')
      ])
    ;
  finally
    Builders.Free;
  end;
end;

end.
