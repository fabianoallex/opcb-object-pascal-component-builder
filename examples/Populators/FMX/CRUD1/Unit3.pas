unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Populators, FMX.Controls.Presentation, FMX.StdCtrls;

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
  Populators: TPopulators;
begin
  Populators := TPopulators.Create(Self.Name);

  try
    Populators.AsComponentPopulator
      .WithOwner(Self)
    ;

    Populators.AsControlPopulator
      .WithOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .AddControls([
        TControlInfo.Create(TButton, 'B1'), TControlInfo.Create(TButton, 'B2'),
        TControlInfo.Create(TButton, 'B3'), TControlInfo.Create(TButton, 'B4')
      ])
    ;
  finally
    Populators.Free;
  end;
end;

end.
