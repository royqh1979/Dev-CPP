unit U_DCSetString;

interface

uses
  U_DCRBTContainer;

type
  {
    Set with string key.
  }
  TDCSetString = class(TDCRBTContainer)
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
  result:=IntAdd(FHash.Hash(AKey), nil);
end;

function TDCSetString.Exists(const AKey : string) : boolean;
begin
  result:=(IntFind(FHash.Hash(AKey)) <> nil);
end;

procedure TDCSetString.Remove(const AKey : string; ARaiseIfNotFound : boolean);
begin
  if not IntRemove(FHash.Hash(AKey)) then
    if ARaiseIfNotFound then
      raise EDCKeyNotFound.Create(AKey);
end;

end.

