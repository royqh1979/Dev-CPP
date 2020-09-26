unit TestsDCTreeKeyValue;

interface

uses
  TestFrameWork;

type
  TDCTreeKeyValueTests = class(TTestCase)
  protected
  published
    procedure CompareDCKeyValueTest;
    procedure CompareDCKeyValueBorderRangeTest;
    procedure CompareDCKeyValueMaxIntTest;

    procedure CreateAndFreeDCTKVWithValue;
    procedure CreateAndFreeDCTKVWithoutValue;
  end;

implementation

{ TDCTreeKeyValueTests }

uses
  SysUtils, U_DCTreeKeyValue, U_DCValue;

procedure TDCTreeKeyValueTests.CompareDCKeyValueTest;
var
  kv1, kv2 : PDCTreeKeyValue;
begin
  New(kv1);
  New(kv2);
  try
    kv1^.Key:=30;
    kv2^.Key:=10;
    CheckTrue(CompareDCTreeKeyValue(kv1, kv2) > 0, '> 0');
    CheckTrue(CompareDCTreeKeyValue(kv2, kv1) < 0, '< 0');
    kv1^.Key:=10;
    CheckTrue(CompareDCTreeKeyValue(kv1, kv2) = 0, '= 0');
  finally
    Dispose(kv1);
    Dispose(kv2);
  end;
end;

procedure TDCTreeKeyValueTests.CompareDCKeyValueBorderRangeTest;
var
  kv1, kv2 : PDCTreeKeyValue;
begin
  New(kv1);
  New(kv2);
  try
    kv1^.Key:=1128307827;
    kv2^.Key:=-1249881816;
    CheckTrue(CompareDCTreeKeyValue(kv1, kv2) > 0, '> 0');
    CheckTrue(CompareDCTreeKeyValue(kv2, kv1) < 0, '< 0');
  finally
    Dispose(kv1);
    Dispose(kv2);
  end;
end;

procedure TDCTreeKeyValueTests.CompareDCKeyValueMaxIntTest;
var
  kv1, kv2 : PDCTreeKeyValue;
begin
  New(kv1);
  New(kv2);
  try
    kv1^.Key:=MaxInt;
    kv2^.Key:=Low(Integer);
    CheckTrue(CompareDCTreeKeyValue(kv1, kv2) > 0, '> 0');
    CheckTrue(CompareDCTreeKeyValue(kv2, kv1) < 0, '< 0');
    kv2^.Key:=MaxInt;
    CheckTrue(CompareDCTreeKeyValue(kv1, kv2) = 0, '= 0');

    kv1^.Key:=MaxInt div 2;
    kv2^.Key:=Low(Integer) div 2;
    CheckTrue(CompareDCTreeKeyValue(kv1, kv2) > 0, '> 0');
    CheckTrue(CompareDCTreeKeyValue(kv2, kv1) < 0, '< 0');
  finally
    Dispose(kv1);
    Dispose(kv2);
  end;
end;

procedure TDCTreeKeyValueTests.CreateAndFreeDCTKVWithValue;
const
  C_KEY = 1;
var
  ptr : PDCTreeKeyValue;
  val : TDCValue;
begin
  val:=TDCValue.Create;
  ptr:=CreateDCTreeKeyValue(C_KEY, val);
  try
    CheckEquals(C_KEY, ptr^.Key);
    CheckTrue(ptr^.Value = val);
  finally
    FreeDCTreeKeyValue(ptr);
  end;
end;

procedure TDCTreeKeyValueTests.CreateAndFreeDCTKVWithoutValue;
const
  C_KEY = 1;
var
  ptr : PDCTreeKeyValue;
begin
  ptr:=CreateDCTreeKeyValue(C_KEY, nil);
  try
    CheckEquals(C_KEY, ptr^.Key);
    CheckTrue(ptr^.Value = nil);
  finally
    FreeDCTreeKeyValue(ptr);
  end;
end;

initialization
  TestFramework.RegisterTest('DCTreeKeyValue', TDCTreeKeyValueTests.Suite);

end.
