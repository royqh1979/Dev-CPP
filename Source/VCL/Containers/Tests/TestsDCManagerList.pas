unit TestsDCManagerList;

interface

uses
  TestFrameWork, U_DCManagerList, U_DCValue;

type
  TTestClass = class
  public
    s : string;
  end;

  TDCManagerListTests = class(TTestCase)
  private
    TestObj : TDCManagerList;

    procedure CheckAllHaveEqualTypes(AType : TClass);
    procedure SimpleFillTestObj;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CreatedEmpty;

    procedure CreateObjectInt;   
    procedure CreateObjectString;
    procedure CreateObjectClass;
    procedure CreateObjectNil;

    procedure CreateObjectMixed;

    procedure DeleteObject;

    procedure IndexOf;
    procedure IndexOfNonExisting;
    procedure IndexOfDeleted;
  end;

implementation

{ TDSetTests }

uses
  SysUtils, U_DCTreeKeyValue, U_DCValueInteger, U_DCValueObject,
  U_DCValueString;

procedure TDCManagerListTests.SetUp;
begin
  TestObj:=TDCManagerList.Create;
end;

procedure TDCManagerListTests.TearDown;
begin
  TestObj.Free;
end;

procedure TDCManagerListTests.CheckAllHaveEqualTypes(AType : TClass);
var
  i : integer;
begin
  for i:=0 to TestObj.Count - 1 do
    CheckTrue(TestObj[i]^.Value is AType, Format('For %d type should be %s but is %s', [i, AType.ClassName, TestObj[i]^.Value.ClassName]));
end;

procedure TDCManagerListTests.CreatedEmpty;
begin
  CheckTrue(TestObj.IsEmpty);
  CheckEquals(0, TestObj.Count);
end;

procedure TDCManagerListTests.CreateObjectInt;
var
  kv : PDCTreeKeyValue;
begin
  kv:=TestObj.CreateObject(1, 1);
  CheckTrue(kv^.Value is TDCValueInteger);
  CheckEquals(1, kv^.Value.AsInteger);
  CheckEquals(1, TestObj.Count);
  CheckAllHaveEqualTypes(TDCValueInteger);

  kv:=TestObj.CreateObject(2, 100);
  CheckTrue(kv^.Value is TDCValueInteger);
  CheckEquals(100, kv^.Value.AsInteger);
  CheckEquals(2, TestObj.Count);  
  CheckAllHaveEqualTypes(TDCValueInteger);

  kv:=TestObj.CreateObject(2, 100);
  CheckTrue(kv^.Value is TDCValueInteger);
  CheckEquals(100, kv^.Value.AsInteger);
  CheckEquals(3, TestObj.Count);
  CheckAllHaveEqualTypes(TDCValueInteger);
end;

procedure TDCManagerListTests.CreateObjectNil;
const
  CNT = 5;
var
  i : integer;
  kv : PDCTreeKeyValue;
begin
  for i:=1 to CNT do
  begin
    kv:=TestObj.CreateObject(i, nil);
    CheckNull(kv^.Value);
  end;
  CheckEquals(5, TestObj.Count);
end;

procedure TDCManagerListTests.CreateObjectClass;
const
  CNT = 10;
var
  testClass : TTestClass;
  i : integer;
  kv : PDCTreeKeyValue;
begin
  for i:=1 to CNT do
  begin
    testClass:=TTestClass.Create;
    try
      testClass.s:='TestStringValue';
      kv:=TestObj.CreateObject(i, testClass);
      CheckTrue(kv^.Value is TDCValueObject);
      CheckTrue(kv^.Value.AsObject = testClass);
      CheckAllHaveEqualTypes(TDCValueObject);
    finally
      testClass.Free;
    end;
  end;
  CheckEquals(CNT, TestObj.Count);
end;

procedure TDCManagerListTests.CreateObjectString;
var
  kv : PDCTreeKeyValue;
begin
  kv:=TestObj.CreateObject(1, '1');
  CheckTrue(kv^.Value is TDCValueString);
  CheckEqualsString('1', kv^.Value.AsString);
  CheckAllHaveEqualTypes(TDCValueString);

  kv:=TestObj.CreateObject(2, '100');
  CheckTrue(kv^.Value is TDCValueString);
  CheckEqualsString('100', kv^.Value.AsString);
  CheckAllHaveEqualTypes(TDCValueString);

  CheckEquals(2, TestObj.Count);
end;

procedure TDCManagerListTests.CreateObjectMixed;
const
  CNT = 10;
var
  testClass : TTestClass;
  i : integer;
  kv, kv2 : PDCTreeKeyValue;
begin
  for i:=1 to CNT do
  begin
    testClass:=TTestClass.Create;
    try
      testClass.s:='TestStringValue';
      kv:=TestObj.CreateObject(i, testClass);
      CheckTrue(kv^.Value is TDCValueObject);
      CheckTrue(kv^.Value.AsObject = testClass);
      kv2:=TestObj[TestObj.Count - 1];
      CheckTrue(kv^.Value.ClassName = kv2^.Value.ClassName);
      CheckTrue(kv^.Value.AsObject = kv2^.Value.AsObject);

      kv:=TestObj.CreateObject(i, i);
      CheckTrue(kv^.Value is TDCValueInteger);
      CheckEquals(i, kv^.Value.AsInteger);
      kv2:=TestObj[TestObj.Count - 1];
      CheckTrue(kv^.Value.ClassName = kv2^.Value.ClassName);
      CheckTrue(kv^.Value.AsInteger = kv2^.Value.AsInteger);

      kv:=TestObj.CreateObject(i, IntToStr(i));
      CheckTrue(kv^.Value is TDCValueString);
      CheckEqualsString(IntToStr(i), kv^.Value.AsString);
      kv2:=TestObj[TestObj.Count - 1];
      CheckTrue(kv^.Value.ClassName = kv2^.Value.ClassName);
      CheckTrue(kv^.Value.AsString = kv2^.Value.AsString);
    finally
      testClass.Free;
    end;
  end;
  CheckEquals(CNT * 3, TestObj.Count);
end;

procedure TDCManagerListTests.SimpleFillTestObj;
begin
  TestObj.CreateObject(1, 1);
  TestObj.CreateObject(2, 2);
  TestObj.CreateObject(3, 3);
  CheckEquals(3, TestObj.Count);
end;

procedure TDCManagerListTests.DeleteObject;
begin
  SimpleFillTestObj;
  TestObj.DeleteObject(TestObj[1]);
  CheckEquals(2, TestObj.Count);
  CheckEquals(1, TestObj[0]^.Key);
  CheckEquals(3, TestObj[1]^.Key);
end;

procedure TDCManagerListTests.IndexOf;
var
  i : integer;
begin
  SimpleFillTestObj;
  for i:=0 to TestObj.Count - 1 do
    CheckEquals(i, TestObj.IndexOf(TestObj[i]));
end;

procedure TDCManagerListTests.IndexOfNonExisting;
var
  kv : TDCTreeKeyValue;
begin
  SimpleFillTestObj;
  CheckEquals(-1, TestObj.IndexOf(@kv));
end;

procedure TDCManagerListTests.IndexOfDeleted;
var
  ptr : PDCTreeKeyValue;
  i : integer;
begin
  SimpleFillTestObj;
  ptr:=TestObj[1];
  TestObj.DeleteObject(ptr);
  CheckEquals(-1, TestObj.IndexOf(ptr));
  for i:=0 to TestObj.Count - 1 do
    CheckEquals(i, TestObj.IndexOf(TestObj[i]));
end;

initialization
  TestFramework.RegisterTest('DCManager', TDCManagerListTests.Suite);

end.
