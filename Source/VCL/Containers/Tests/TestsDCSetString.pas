unit TestsDCSetString;

interface

uses
  TestFrameWork, U_DCSetString;

type
  TDCSetStringTests = class(TTestCase)
  private
    TestObj : TDCSetString;
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

{ TDCSetStringTests }

uses
  SysUtils, U_DCManagerList, U_DCHashBJL3, U_DCExceptions,
  U_DCTreeKeyValue;

procedure TDCSetStringTests.SetUp;
begin
  TestObj:=TDCSetString.Create(TDCManagerList.Create, TDCHashBJL3.Create);
end;

procedure TDCSetStringTests.TearDown;
begin
  TestObj.Free;
end;

procedure TDCSetStringTests.CreatedEmpty;
begin
  CheckTrue(TestObj.IsEmpty);
end;

procedure TDCSetStringTests.AddKey;
const
  CNT = 5;
var
  i : integer;
begin
  for i:=1 to CNT do
    CheckTrue(TestObj.Add(IntToStr(i)));
  CheckEquals(CNT, TestObj.Count);
  for i:=1 to CNT do
    CheckTrue(TestObj.Exists(IntToStr(i)));
end;

procedure TDCSetStringTests.AddTwiceTheSameKey;
begin
  CheckEquals(0, TestObj.Count);
  CheckTrue(TestObj.Add('1'));
  CheckFalse(TestObj.Add('1'));
  CheckEquals(1, TestObj.Count);
end;

procedure TDCSetStringTests.ExistsKey;
begin
  TestObj.Add('1');
  CheckTrue(TestObj.Exists('1'));
  CheckFalse(TestObj.Exists('555'));
end;

procedure TDCSetStringTests.ExistsKeyInEmptySet;
begin
  CheckFalse(TestObj.Exists('555'));
end;

procedure TDCSetStringTests.RemoveKey;
begin
  TestObj.Add('1');
  CheckEquals(1, TestObj.Count);
  TestObj.Remove('1');
  CheckEquals(0, TestObj.Count);
end;

procedure TDCSetStringTests.RemoveNonExistingElement;
begin
  TestObj.Add('1');
  TestObj.Remove('3');
end;

procedure TDCSetStringTests.RemoveNonExistingElementRaiseException;
begin
  TestObj.Add('1');
  ExpectedException:=EDCKeyNotFound;
  TestObj.Remove('3', true);
end;

procedure TDCSetStringTests.ClearLeavesEmpty;
begin
  TestObj.Add('1');
  TestObj.Add('2');
  CheckEquals(2, TestObj.Count);
  CheckTrue(TestObj.Exists('1'));
  CheckTrue(TestObj.Exists('2'));
  TestObj.Clear;
  CheckEquals(0, TestObj.Count);
  CheckFalse(TestObj.Exists('1'));
  CheckFalse(TestObj.Exists('2'));
end;

procedure TDCSetStringTests.SimpleFillTestObj;
begin
  TestObj.Add('1');
  TestObj.Add('2');
  TestObj.Add('3');
  TestObj.Add('4');
  CheckEquals(4, TestObj.Count);
end;

procedure TDCSetStringTests.IndexOf;
var
  i : integer;
begin
  SimpleFillTestObj;
  for i:=0 to TestObj.Count - 1 do
    CheckEquals(i, TestObj.IndexOf(TestObj[i]));
end;

procedure TDCSetStringTests.IndexOfNonExistingElement;
begin
  SimpleFillTestObj;
  CheckEquals(-1, TestObj.IndexOf(nil));
end;

procedure TDCSetStringTests.IndexOfDeletedElement;
var
  ptr : PDCTreeKeyValue;
  i : integer;
begin
  SimpleFillTestObj;
  ptr:=TestObj[1];
  TestObj.Remove('2');
  CheckEquals(-1, TestObj.IndexOf(ptr));
  for i:=0 to TestObj.Count - 1 do
    CheckEquals(i, TestObj.IndexOf(TestObj[i]));
end;

initialization
  TestFramework.RegisterTest('DCSet', TDCSetStringTests.Suite);

end.

