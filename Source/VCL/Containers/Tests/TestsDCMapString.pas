unit TestsDCMapString;

interface

uses
  TestFrameWork, TestsDCMapBase, U_DCMapString, U_DCTreeKeyValue;

type
  TDCMapStringTests = class(TDCMapBaseTests)
  private
    TestObj : TDCMapString;
    procedure SimpleFillTestObj;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CreatedEmpty;

    procedure AddKeyIntegerValue;
    procedure AddKeyStringValue;
    procedure AddKeyObjectValue;
    procedure AddTwiceTheSameKey;

    procedure ClearLeavesEmpty;
    procedure ClearFreeObjectsDoesntRaise;

    procedure ReplaceKeyIntegerValue;
    procedure ReplaceKeyStringValue;
    procedure ReplaceKeyObjectValue;
    procedure ReplaceNonExistingElement;

    procedure FindKey;
    procedure FindNonExistingElement;
    procedure FindInEmptyMap;

    procedure RemoveKey;
    procedure RemoveKeyGetObject;
    procedure RemoveKeyGetObjectReturnsNilForNonObject;
    procedure RemoveNonExistingElement;
    procedure RemoveNonExistingElementRaiseException;
    procedure RemoveNonExistingElementGetObjectReturnsNil;
    procedure RemoveNonExistingElementGetObjectRaiseException;

    procedure IndexOf;
    procedure IndexOfNonExistingElement;
    procedure IndexOfDeletedElement;
  end;

implementation

{ TDSetTests }

uses
  SysUtils, U_DCExceptions, U_DCValueInteger, U_DCValueObject, U_DCValueString,
  U_DCHashBJL3, U_DCManagerList;

procedure TDCMapStringTests.SetUp;
begin
  TestObj:=TDCMapString.Create(TDCManagerList.Create, TDCHashBJL3.Create);
end;

procedure TDCMapStringTests.TearDown;
begin
  TestObj.Free;
end;

procedure TDCMapStringTests.CreatedEmpty;
begin
  CheckTrue(TestObj.IsEmpty);
end;

procedure TDCMapStringTests.AddKeyIntegerValue;
const
  CNT = 5;
var
  i : integer;
begin
  for i:=1 to CNT do
    CheckTrue(TestObj.Add(IntToStr(i), i));
  CheckEquals(CNT, TestObj.Count);
  for i:=1 to CNT do
    CheckFoundIntegerValue(TestObj.Find(IntToStr(i)), i);
end;

procedure TDCMapStringTests.AddKeyStringValue;
const
  CNT = 10;
var
  i : integer;
begin
  for i:=1 to CNT do
    CheckTrue(TestObj.Add(IntToStr(i), IntToStr(i)));
  CheckEquals(CNT, TestObj.Count);
  for i:=1 to CNT do
    CheckFoundStringValue(TestObj.Find(IntToStr(i)), IntToStr(i));
end;

procedure TDCMapStringTests.AddKeyObjectValue;
const
  CNT = 3;
var
  testClass : array [1..CNT] of TTestClass;
  i : integer;
begin
  for i:=1 to CNT do
  begin
    testClass[i]:=TTestClass.Create;
    testClass[i].s:='Test'+IntToStr(i);
    CheckTrue(TestObj.Add(IntToStr(i), testClass[i]));
  end;
  try
    CheckEquals(CNT, TestObj.Count);
    for i:=1 to CNT do
      CheckFoundObjectValue(TestObj.Find(IntToStr(i)), testClass[i]);
  finally
    for i:=1 to CNT do
      FreeAndNil(testClass[i]);
  end;
end;

procedure TDCMapStringTests.AddTwiceTheSameKey;
var
  o1, o2 : TTestClass;
begin
  CheckEquals(0, TestObj.Count);
  CheckTrue(TestObj.Add('1', 2));
  CheckFalse(TestObj.Add('1', 20));
  CheckEquals(1, TestObj.Count);
  CheckFoundIntegerValue(TestObj.Find('1'), 2);

  TestObj.Clear;
  CheckEquals(0, TestObj.Count);
  CheckTrue(TestObj.Add('1', '2'));
  CheckFalse(TestObj.Add('1', '20'));
  CheckEquals(1, TestObj.Count);
  CheckFoundStringValue(TestObj.Find('1'), '2');

  TestObj.Clear;
  CheckEquals(0, TestObj.Count);
  o1:=TTestClass.Create; o1.s:='2';
  o2:=TTestClass.Create; o2.s:='20';
  try
    CheckTrue(TestObj.Add('1', o1));
    CheckFalse(TestObj.Add('1', o2));
    CheckEquals(1, TestObj.Count);
    CheckFoundObjectValue(TestObj.Find('1'), o1);
    TestObj.Clear;
  finally
    o2.Free;
    o1.Free;
  end;
end;

procedure TDCMapStringTests.ReplaceKeyIntegerValue;
const
  OLD_VALUE = 2;
  NEW_VALUE = 5;
var
  testClass : TTestClass;
  i : integer;
begin
  testClass:=TTestClass.Create;
  try
    testClass.s:=IntToStr(OLD_VALUE);
    TestObj.Add('1', OLD_VALUE);
    TestObj.Add('2', IntToStr(OLD_VALUE));
    TestObj.Add('3', testClass);

    for i:=1 to TestObj.Count do
    begin
      TestObj.Replace(IntToStr(i), NEW_VALUE);
      CheckFoundIntegerValue(TestObj.Find(IntToStr(i)), NEW_VALUE);
    end;
  finally
    testClass.Free;
  end;
end;

procedure TDCMapStringTests.ReplaceKeyStringValue;
const
  OLD_VALUE = 2;
  NEW_VALUE = '5';
var
  testClass : TTestClass;
  i : integer;
begin
  testClass:=TTestClass.Create;
  try
    testClass.s:=IntToStr(OLD_VALUE);
    TestObj.Add('1', OLD_VALUE);
    TestObj.Add('2', IntToStr(OLD_VALUE));
    TestObj.Add('3', testClass);

    for i:=1 to TestObj.Count do
    begin
      TestObj.Replace(IntToStr(i), NEW_VALUE);
      CheckFoundStringValue(TestObj.Find(IntToStr(i)), NEW_VALUE);
    end;
  finally
    testClass.Free;
  end;
end;

procedure TDCMapStringTests.ReplaceKeyObjectValue;
const
  OLD_VALUE = 2;
  NEW_VALUE = '5';
var
  testClass, newTestClass : TTestClass;
  i : integer;
begin
  testClass:=TTestClass.Create;
  newTestClass:=TTestClass.Create;
  try
    testClass.s:=IntToStr(OLD_VALUE);
    newTestClass.s:=NEW_VALUE;
    TestObj.Add('1', OLD_VALUE);
    TestObj.Add('2', IntToStr(OLD_VALUE));
    TestObj.Add('3', testClass);

    for i:=1 to TestObj.Count do
    begin
      TestObj.Replace(IntToStr(i), newTestClass);
      CheckFoundObjectValue(TestObj.Find(IntToStr(i)), newTestClass);
    end;
  finally
    newTestClass.Free;
    testClass.Free;
  end;
end;

procedure TDCMapStringTests.ReplaceNonExistingElement;
begin
  TestObj.Add('1', 2);
  ExpectedException:=EDCKeyNotFound;
  TestObj.Replace('2', 2);
end;

procedure TDCMapStringTests.FindKey;
begin
  TestObj.Add('1', 2);
  CheckFoundIntegerValue(TestObj.Find('1'), 2);
end;

procedure TDCMapStringTests.FindNonExistingElement;
begin
  TestObj.Add('1', 2);
  CheckTrue(TestObj.Find('3') = nil);
end;

procedure TDCMapStringTests.FindInEmptyMap;
begin
  CheckTrue(TestObj.Find('3') = nil);
end;

procedure TDCMapStringTests.RemoveKey;
begin
  TestObj.Add('1', 2);
  CheckEquals(1, TestObj.Count);
  TestObj.Remove('1');
  CheckEquals(0, TestObj.Count);
end;

procedure TDCMapStringTests.RemoveKeyGetObject;
var
  o, ores : TTestClass;
begin
  o:=TTestClass.Create;
  try
    TestObj.Add('1', o);
    CheckEquals(1, TestObj.Count);
    TestObj.Remove('1', TObject(ores));
    CheckEquals(0, TestObj.Count);
    CheckTrue(o = ores);
  finally
    o.Free;
  end;
end;

procedure TDCMapStringTests.RemoveKeyGetObjectReturnsNilForNonObject;
var
  ores : TTestClass;
begin
  TestObj.Add('1', 123);
  CheckEquals(1, TestObj.Count);
  TestObj.Remove('1', TObject(ores));
  CheckEquals(0, TestObj.Count);
  CheckTrue(ores = nil);
end;

procedure TDCMapStringTests.RemoveNonExistingElement;
begin
  TestObj.Add('1', 2);
  TestObj.Remove('3');
end;

procedure TDCMapStringTests.RemoveNonExistingElementRaiseException;
begin
  TestObj.Add('1', 2);
  ExpectedException:=EDCKeyNotFound;
  TestObj.Remove('3', true);
end;

procedure TDCMapStringTests.RemoveNonExistingElementGetObjectReturnsNil;
var
  ores : TTestClass;
begin
  TestObj.Add('1', 2);
  TestObj.Remove('3', TObject(ores));
  CheckTrue(ores = nil);
end;

procedure TDCMapStringTests.RemoveNonExistingElementGetObjectRaiseException;
var
  ores : TTestClass;
begin
  TestObj.Add('1', 2);
  ExpectedException:=EDCKeyNotFound;
  TestObj.Remove('3', TObject(ores), true);
end;

procedure TDCMapStringTests.SimpleFillTestObj;
begin
  TestObj.Add('1', 1);
  TestObj.Add('2', 'abcd');
  TestObj.Add('3', TTestClass.Create);
  TestObj.Add('4', nil);
  CheckEquals(4, TestObj.Count);
end;

procedure TDCMapStringTests.ClearLeavesEmpty;
begin
  TestObj.Add('1', 1);
  TestObj.Add('2', 2);
  CheckEquals(2, TestObj.Count);
  CheckTrue(TestObj.Find('1') <> nil);
  CheckTrue(TestObj.Find('2') <> nil);
  TestObj.Clear;
  CheckEquals(0, TestObj.Count);
  CheckTrue(TestObj.Find('1') = nil);
  CheckTrue(TestObj.Find('2') = nil);
end;

procedure TDCMapStringTests.ClearFreeObjectsDoesntRaise;
begin
  SimpleFillTestObj;
  TestObj.Clear(true);
end;

procedure TDCMapStringTests.IndexOf;
var
  i : integer;
begin
  SimpleFillTestObj;
  try
    for i:=0 to TestObj.Count - 1 do
      CheckEquals(i, TestObj.IndexOf(TestObj[i]));
  finally
    TestObj.Clear(true);
  end;
end;

procedure TDCMapStringTests.IndexOfNonExistingElement;
begin
  SimpleFillTestObj;
  try
    CheckEquals(-1, TestObj.IndexOf(nil));
  finally
    TestObj.Clear(true);
  end;
end;

procedure TDCMapStringTests.IndexOfDeletedElement;
var
  ptr : PDCTreeKeyValue;
  i : integer;
begin
  SimpleFillTestObj;
  try
    ptr:=TestObj.Find('2');
    TestObj.Remove('2');
    CheckEquals(-1, TestObj.IndexOf(ptr));
    for i:=0 to TestObj.Count - 1 do
      CheckEquals(i, TestObj.IndexOf(TestObj[i]));
  finally
    TestObj.Clear(true);
  end;
end;

initialization
  TestFramework.RegisterTest('DCMap', TDCMapStringTests.Suite);

end.
