unit U_DCSetInt;

interface

uses
  U_DCRBTContainer;

type
  {
    Set with integer key.
    Does not use a hashing object - AKey values are used as hashes
  }
  TDCSetInt = class(TDCRBTContainer)
  public
    function Add(AKey : integer) : boolean;

    function Exists(AKey : integer) : boolean;

    procedure Remove(AKey : integer; ARaiseIfNotFound : boolean = false);
  end;

implementation

uses U_DCExceptions;

{ TDCMapInt }

function TDCSetInt.Add(AKey : integer) : boolean;
begin
  result:=IntAdd(AKey, nil);
end;

function TDCSetInt.Exists(AKey : integer) : boolean;
begin
  result:=(IntFind(AKey) <> nil);
end;

procedure TDCSetInt.Remove(AKey : integer; ARaiseIfNotFound : boolean);
begin
  if not IntRemove(AKey) then
    if ARaiseIfNotFound then
      raise EDCKeyNotFound.Create(AKey);
end;

end.

