unit TestsDCHashBJL3;

interface

uses
  TestFramework, TestsDCHashBase;

type
  TDCHashBJL3Tests = class(TDCHashBaseTests)
  protected
    procedure SetUp; override;
  end;

implementation

uses
  U_DCHashBJL3;

{ TDCHashBJL3Tests }

procedure TDCHashBJL3Tests.SetUp;
const
  C_HASHED_STRING = 1743254674;
  C_HASHED_LONG_STRING = 565424085;
  C_HASHED_INTEGER = -450698039;
begin
  ExpectedHashString:=C_HASHED_STRING;
  ExpectedHashLongString:=C_HASHED_LONG_STRING;
  ExpectedHashInt:=C_HASHED_INTEGER;
  TestObj:=TDCHashBJL3.Create;
end;

initialization
  TestFramework.RegisterTest('DCHash', TDCHashBJL3Tests.Suite);

end.
