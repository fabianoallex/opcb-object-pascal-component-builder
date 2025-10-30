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
  Creators: TOPCBCreators;
begin
  Creators := TOPCBCreators.Create(Self.Name);

  try
    Creators.AsComponentCreator
      .SetOwner(Self)
    ;

    Creators.AsControlCreator
      .SetOwnerAndParent(Self, Self)
      .SetSpace(5, 5)
      .Add(TControlBuilder.Create(TButton, 'B1'))
      .Add(TControlBuilder.Create(TButton, 'B2'))
      .Add(TControlBuilder.Create(TButton, 'B3'))
      .Add(TControlBuilder.Create(TButton, 'B4'))
    ;
  finally
    Creators.Free;
  end;
end;

end.
