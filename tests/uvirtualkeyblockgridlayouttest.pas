unit UVirtualKeyBlockGridLayoutTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, UVirtualKeyBlockLayouts;

type

  { TVirtualKeyBlockGridLayoutTest }

  TVirtualKeyBlockGridLayoutTest = class(TTestCase)
  private
    FLayout: TVirtualKeyBlockGridLayout;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateWithNegativeRows;
    procedure TestCreateWithZeroRows;
    procedure TestCreateWithNegativeColumns;
    procedure TestCreateWithZeroColumns;
    procedure TestCreateWithNegativeVerticalSpacing;
    procedure TestCreateWithZeroVerticalSpacing;
  end;

implementation

procedure TVirtualKeyBlockGridLayoutTest.SetUp;
begin

end;

procedure TVirtualKeyBlockGridLayoutTest.TearDown;
begin

end;

procedure TVirtualKeyBlockGridLayoutTest.TestCreateWithNegativeRows;
begin
  FLayout := TVirtualKeyBlockGridLayout.Create(-1, 1, 10, 10);
  AssertEquals(1, FLayout.Rows);
end;

procedure TVirtualKeyBlockGridLayoutTest.TestCreateWithZeroRows;
begin
  FLayout := TVirtualKeyBlockGridLayout.Create(0, 1, 10, 10);
  AssertEquals(1, FLayout.Rows);
end;

procedure TVirtualKeyBlockGridLayoutTest.TestCreateWithNegativeColumns;
begin
  FLayout := TVirtualKeyBlockGridLayout.Create(1, -1, 10, 10);
  AssertEquals(1, FLayout.Columns);
end;

procedure TVirtualKeyBlockGridLayoutTest.TestCreateWithZeroColumns;
begin
  FLayout := TVirtualKeyBlockGridLayout.Create(1, 0, 10, 10);
  AssertEquals(1, FLayout.Columns);
end;

procedure TVirtualKeyBlockGridLayoutTest.TestCreateWithNegativeVerticalSpacing;
begin
  FLayout := TVirtualKeyBlockGridLayout.Create(1, 1, -1, 10);
  AssertEquals(1, FLayout.VerticalSpacing);
end;

procedure TVirtualKeyBlockGridLayoutTest.TestCreateWithZeroVerticalSpacing;
begin
  FLayout := TVirtualKeyBlockGridLayout.Create(1, 1, 0, 10);
  AssertEquals(1, FLayout.VerticalSpacing);
end;

initialization

  RegisterTest(TVirtualKeyBlockGridLayoutTest);
end.

