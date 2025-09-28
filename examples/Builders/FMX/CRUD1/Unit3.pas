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
    Builders.AsComponentsBuilder
      .WithOwner(Self)
    ;

    Builders.AsControlsBuilder
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .AddControls([
        TControlBuilder.Create(TButton, 'B1'), TControlBuilder.Create(TButton, 'B2'),
        TControlBuilder.Create(TButton, 'B3'), TControlBuilder.Create(TButton, 'B4')
      ])
    ;
  finally
    Builders.Free;
  end;
end;

end.
