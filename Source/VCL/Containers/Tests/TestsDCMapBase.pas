unit TestsDCMapBase;

interface

uses
  TestFrameWork, U_DCTreeKeyValue;

type
  TTestClass = class
  public
    s : string;
  end;

  TDCMapBaseTests = class(TTestCase)
  protected
    procedure CheckFoundIntegerValue(KVPtr : PDCTreeKeyValue; AExpected : integer);
    procedure CheckFoundStringValue(KVPtr : PDCTreeKeyValue; const AExpected : string);
    procedure CheckFoundObjectValue(KVPtr : PDCTreeKeyValue; AExpected : TTestClass);
  end;

implementation

uses U_DCValueInteger, U_DCValueObject, U_DCValueString;

{ TDCMapStringTests }

procedure TDCMapBaseTests.CheckFoundIntegerValue(KVPtr: PDCTreeKeyValue;
  AExpected: integer);
begin
  CheckTrue(KVPtr <> nil, 'not found');
  CheckTrue(KVPtr^.Value is TDCValueInteger, 'Value is not TDCValueInteger');
  CheckEquals(AExpected, KVPtr^.Value.AsInteger);
end;

procedure TDCMapBaseTests.CheckFoundStringValue(KVPtr: PDCTreeKeyValue;
  const AExpected: string);
begin
  CheckTrue(KVPtr <> nil, 'not found');
  CheckTrue(KVPtr^.Value is TDCValueString, 'Value is not TDCValueString');
  CheckEqualsString(AExpected, KVPtr^.Value.AsString);
end;

procedure TDCMapBaseTests.CheckFoundObjectValue(KVPtr: PDCTreeKeyValue;
  AExpected: TTestClass);
begin
  CheckTrue(KVPtr <> nil, 'not found');
  CheckTrue(KVPtr^.Value is TDCValueObject, 'Value is not TDCValueObject');
  CheckTrue(KVPtr^.Value.AsObject is TTestClass, 'object is not TTestClass');
  CheckTrue(AExpected = KVPtr^.Value.AsObject, 'different object');
  CheckEqualsString(AExpected.s, TTestClass(KVPtr^.Value.AsObject).s, 'different object value');
end;

end.
