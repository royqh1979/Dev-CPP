unit TestsDCRBTContainer;

interface

uses
  TestFrameWork, U_DCRBTContainer;

type
  TDCRBTContainerTests = class(TTestCase)
  private
    TestObj : TDCRBTContainer;
  published
    procedure CreateAndFreeWithHash;
    procedure CreateAndFreeWithoutHash;
  end;

implementation

uses U_DCManagerList, U_DCHashBase;

{ TDCRBTContainerTests }

procedure TDCRBTContainerTests.CreateAndFreeWithHash;
begin
  TestObj:=TDCRBTContainer.Create(TDCManagerList.Create, TDCHashBase.Create);
  TestObj.Free;
end;

procedure TDCRBTContainerTests.CreateAndFreeWithoutHash;
begin
  TestObj:=TDCRBTContainer.Create(TDCManagerList.Create);
  TestObj.Free;
end;

initialization
  TestFramework.RegisterTest('DCRBTContainer', TDCRBTContainerTests.Suite);

end.
 