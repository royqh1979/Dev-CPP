unit U_DCSetString;

interface

uses
  U_DCRBTContainer;

type
  {
    Set with string key.
  }
  TDCSetString = class(TDCRBTStringContainer)
  public
    function Add(const AKey : string) : boolean;

    function Exists(const AKey : string) : boolean;

    procedure Remove(const AKey : string; ARaiseIfNotFound : boolean = false);
  end;

implementation

uses U_DCExceptions;

{ TDCSetString }

function TDCSetString.Add(const AKey : string) : boolean;
begin
  result:=IntPut(FHash.Hash(AKey), nil);
end;

function TDCSetString.Exists(const AKey : string) : boolean;
begin
  result:=(IntGet(FHash.Hash(AKey)) <> nil);
end;

procedure TDCSetString.Remove(const AKey : string; ARaiseIfNotFound : boolean);
begin
  if not IntRemove(FHash.Hash(AKey)) then
    if ARaiseIfNotFound then
      raise EDCKeyNotFound.Create(AKey);
end;

end.

