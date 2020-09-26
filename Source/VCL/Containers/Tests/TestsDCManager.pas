unit TestsDCManager;

interface

uses
  TestFrameWork, U_DCManagerList;

type
  TTestClass = class
  public
    s : string;
  end;

  TDCManagerListTests = class(TTestCase)
  private
    TestObj : TDCManagerList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure IsEmpty;

    procedure CreateObjectInt;   
    procedure CreateObjectString;
    procedure CreateObjectClass;
    procedure CreateObjectNil;
  end;

implementation

{ TDSetTests }

uses
  SysUtils, U_DCKeyValue, cArrays, U_DCValue;

procedure TDCManagerListTests.SetUp;
begin
  TestObj:=TDCManagerList.Create;
end;

procedure TDCManagerListTests.TearDown;
begin
  TestObj.Free;
end;

procedure TDCManagerListTests.IsEmpty;
begin
  CheckTrue(TestObj.IsEmpty);   
  CheckEquals(0, TestObj.Count);
end;

procedure TDCManagerListTests.CreateObjectInt;
var
  kv : PTDCKeyValue;
begin
  kv:=TestObj.CreateObject(1, 1);
  CheckTrue(kv^.Value.ValueType = dcInteger);
  CheckEquals(1, kv^.Value.AsInteger);

  kv:=TestObj.CreateObject(2, 100);
  CheckTrue(kv^.Value.ValueType = dcInteger);
  CheckEquals(100, kv^.Value.AsInteger);

  kv:=TestObj.CreateObject(2, 100);
  CheckTrue(kv^.Value.ValueType = dcInteger);
  CheckEquals(100, kv^.Value.AsInteger);

  CheckEquals(3, TestObj.Count);
end;

procedure TDCManagerListTests.CreateObjectNil;
const
  CNT = 5;
var    
  i : integer;
  kv : PTDCKeyValue;
begin
  for i:=1 to CNT do
  begin
    kv:=TestObj.CreateObject(i, nil);
    CheckTrue(kv^.Value.ValueType = dcObject);
    CheckNull(kv^.Value.AsObject);
  end;
  CheckEquals(5, TestObj.Count);
end;

procedure TDCManagerListTests.CreateObjectClass;
const
  CNT = 1000;
var
  testClass : TTestClass;
  i : integer;
  kv : PTDCKeyValue;
begin
  for i:=1 to CNT do
  begin
    testClass:=TTestClass.Create;
    try
      testClass.s:='TestStringValue';
      kv:=TestObj.CreateObject(i, testClass);
      CheckTrue(kv^.Value.ValueType = dcObject);
      CheckTrue(kv^.Value.AsObject = testClass);
    finally
      testClass.Free;
    end;
  end;
  CheckEquals(CNT, TestObj.Count);
end;

procedure TDCManagerListTests.CreateObjectString;
var
  kv : PTDCKeyValue;
begin
  kv:=TestObj.CreateObject(1, '1');
  CheckTrue(kv^.Value.ValueType = dcString);
  CheckEqualsString('1', kv^.Value.AsString);

  kv:=TestObj.CreateObject(2, '100');
  CheckTrue(kv^.Value.ValueType = dcString);
  CheckEqualsString('100', kv^.Value.AsString);

  CheckEquals(2, TestObj.Count);
end;

initialization
  TestFramework.RegisterTest('DCManager', TDCManagerListTests.Suite);

end.
