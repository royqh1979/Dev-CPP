unit U_DCHashBJL3;

interface

uses
  U_DCHashBase;

type
  {
    Bob Jenkins' 32-bit hash lookup3 / hashlittle
    using crc hash library by Wolfgang Ehrhardt
  }
  TDCHashBJL3 = class(TDCHashBase)
  private
  public
    function Hash(const s : string) : integer; override;
    function Hash(const i : integer) : integer; override;
  end;

implementation

uses
  bjl3;

{ TDCHashBJL3 }

function TDCHashBJL3.Hash(const s: string): integer;
begin
  result:=hashlittle(@s[1], length(s), 0);
end;

function TDCHashBJL3.Hash(const i: integer): integer;
begin
  result:=hashlittle(@i, sizeof(i), 0);
end;

end.
