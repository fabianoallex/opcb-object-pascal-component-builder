unit UGridLayoutFillerFactory;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, ULayout, UGridLayoutFill;

type
  TFillerType = (ftRowFirst, ftColumnFirst);

  TGridFillClass = class of TGridFillBase;

  { TGridLayoutFillerFactory }

  TGridLayoutFillerFactory = class
  public
    class function CreateFiller(AType: TFillerType; AGrid: TGridLayout): IGridFill;
  end;

implementation

const
  FillerTypeClasses: array[TFillerType] of TGridFillClass = (
    TGridFillRowFirst,
    TGridFillColumnFirst
  );

{ TGridLayoutFillerFactory }

class function TGridLayoutFillerFactory.CreateFiller(
  AType: TFillerType; AGrid: TGridLayout): IGridFill;
begin
  Result := FillerTypeClasses[AType].Create(AGrid) as IGridFill;
end;

end.
