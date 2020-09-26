unit TestsDCSetInt;

interface

uses
  TestFrameWork, U_DCSetInt;

type
  TDCSetIntTests = class(TTestCase)
  private
    TestObj : TDCSetInt;
    procedure SimpleFillTestObj;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CreatedEmpty;

    procedure AddKey;
    procedure AddTwiceTheSameKey;

    procedure ClearLeavesEmpty;

    procedure ExistsKey;
    procedure ExistsKeyInEmptySet;

    procedure RemoveKey;
    procedure RemoveNonExistingElement;
    procedure RemoveNonExistingElementRaiseException;

    procedure IndexOf;
    procedure IndexOfNonExistingElement;
    procedure IndexOfDeletedElement;
  end;

implementation

{ TDCSetIntTests }

uses
  SysUtils, U_DCManagerList, U_DCExceptions, U_DCTreeKeyValue;

procedure TDCSetIntTests.SetUp;
begin
  TestObj:=TDCSetInt.Create(TDCManagerList.Create);
end;

procedure TDCSetIntTests.TearDown;
begin
  TestObj.Free;
end;

procedure TDCSetIntTests.CreatedEmpty;
begin
  CheckTrue(TestObj.IsEmpty);
end;

procedure TDCSetIntTests.AddKey;
const
  CNT = 5;
var
  i : integer;
begin
  for i:=1 to CNT do
    Checktrue(TestObj.Add(i));
  CheckEquals(CNT, TestObj.Count);
  for i:=1 to CNT do
    CheckTrue(TestObj.Exists(i));
end;

procedure TDCSetIntTests.AddTwiceTheSameKey;
begin
  CheckEquals(0, TestObj.Count);
  CheckTrue(TestObj.Add(1));
  CheckFalse(TestObj.Add(1));
  CheckEquals(1, TestObj.Count);
end;

procedure TDCSetIntTests.ExistsKey;
begin
  TestObj.Add(1);
  CheckTrue(TestObj.Exists(1));
  CheckFalse(TestObj.Exists(555));
end;

procedure TDCSetIntTests.ExistsKeyInEmptySet;
begin
  CheckFalse(TestObj.Exists(555));
end;

procedure TDCSetIntTests.RemoveKey;
begin
  TestObj.Add(1);
  CheckEquals(1, TestObj.Count);
  TestObj.Remove(1);
  CheckEquals(0, TestObj.Count);
end;

procedure TDCSetIntTests.RemoveNonExistingElement;
begin
  TestObj.Add(1);
  TestObj.Remove(3);
end;

procedure TDCSetIntTests.RemoveNonExistingElementRaiseException;
begin
  TestObj.Add(1);
  ExpectedException:=EDCKeyNotFound;
  TestObj.Remove(3, true);
end;

procedure TDCSetIntTests.ClearLeavesEmpty;
begin
  TestObj.Add(1);
  TestObj.Add(2);
  CheckEquals(2, TestObj.Count);
  CheckTrue(TestObj.Exists(1));
  CheckTrue(TestObj.Exists(2));
  TestObj.Clear;
  CheckEquals(0, TestObj.Count);
  CheckFalse(TestObj.Exists(1));
  CheckFalse(TestObj.Exists(2));
end;

procedure TDCSetIntTests.SimpleFillTestObj;
begin
  TestObj.Add(1);
  TestObj.Add(2);
  TestObj.Add(3);
  TestObj.Add(4);
  CheckEquals(4, TestObj.Count);
end;

procedure TDCSetIntTests.IndexOf;
var
  i : integer;
begin
  SimpleFillTestObj;
  for i:=0 to TestObj.Count - 1 do
    CheckEquals(i, TestObj.IndexOf(TestObj[i]));
end;

procedure TDCSetIntTests.IndexOfNonExistingElement;
begin
  SimpleFillTestObj;
  CheckEquals(-1, TestObj.IndexOf(nil));
end;

procedure TDCSetIntTests.IndexOfDeletedElement;
var
  ptr : PDCTreeKeyValue;
  i : integer;
begin
  SimpleFillTestObj;
  ptr:=TestObj[1];
  TestObj.Remove(2);
  CheckEquals(-1, TestObj.IndexOf(ptr));
  for i:=0 to TestObj.Count - 1 do
    CheckEquals(i, TestObj.IndexOf(TestObj[i]));
end;

initialization
  TestFramework.RegisterTest('DCSet', TDCSetIntTests.Suite);

end.

