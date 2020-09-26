unit TestsDCValue;

interface

uses
  TestFrameWork, U_DCValue;

type
  TTestClass = class
  public
    s : string;
  end;

  TDCValueTests = class(TTestCase)
  protected
//    procedure SetUp; override;
//    procedure TearDown; override;
  published
    procedure CreateInteger;
    procedure CreateString;
    procedure CreateObject;
  end;

implementation

uses
  SysUtils, U_DCValueInteger, U_DCValueObject, U_DCValueString,
  U_DCExceptions;

const
  C_INTEGER = 20;
  C_STRING = 'TestString';
  C_UPDATE_INTEGER = 45;
  C_UPDATE_STRING = 'NewString';

{ TDSetTests }

procedure TDCValueTests.CreateInteger;
var
  TestObj : TDCValue;
begin
  TestObj:=TDCValueInteger.Create(C_INTEGER);
  try
    CheckEquals(C_INTEGER, TestObj.AsInteger);
    CheckEquals(inttostr(C_INTEGER), TestObj.AsString);
    ExpectedException:=EDCValueIncorrectFormat;
    CheckNull(TestObj.AsObject);
  finally
    TestObj.Free;
  end;
end;

procedure TDCValueTests.CreateString;
var
  TestObj : TDCValue;
begin
  TestObj:=TDCValueString.Create(C_STRING);
  try
    CheckEquals(C_STRING, TestObj.AsString);
    try
      CheckEquals(0, TestObj.AsInteger);
    except
      on e : EDCValueIncorrectFormat do ;
      on e : Exception do Fail('Expected EDCValueIncorrectFormat exception');
    end;
    try
      CheckNull(TestObj.AsObject);
    except
      on e : EDCValueIncorrectFormat do ;
      on e : Exception do Fail('Expected EDCValueIncorrectFormat eception');
    end;
  finally
    TestObj.Free;
  end;
end;

procedure TDCValueTests.CreateObject;
var
  TestObj : TDCValue;
  obj : TTestClass;
begin
  obj:=TTestClass.Create;
  obj.s:=C_STRING;
  TestObj:=TDCValueObject.Create(obj);
  try
    CheckEqualsString(C_STRING, TTestClass(TestObj.AsObject).s);
    try
      CheckEquals('', TestObj.AsString);
    except
      on e : EDCValueIncorrectFormat do ;
      on e : Exception do Fail('Expected EDCValueIncorrectFormat exception');
    end;
    try
      CheckEquals(0, TestObj.AsInteger);
    except
      on e : EDCValueIncorrectFormat do ;
      on e : Exception do Fail('Expected EDCValueIncorrectFormat exception');
    end;
  finally
    TestObj.Free;
    obj.Free;
  end;
end;

initialization
  TestFramework.RegisterTest('DCValue', TDCValueTests.Suite);

end.
