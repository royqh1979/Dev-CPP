unit U_DCExceptions;

interface

uses
  SysUtils;

type
  EDCKeyNotFound = class(Exception)
  public
    constructor Create(const AKey : string); overload;
    constructor Create(const AKey : integer); overload;
  end;

  EDCValueIncorrectFormat = class(Exception)
  public
    constructor Create(const AValueType, ARequestedType : string);
  end;

implementation

{ EDCKeyNotFound }

constructor EDCKeyNotFound.Create(const AKey: string);
const
  EX_MSG = 'Object not found for key "%s"';
begin
  Message:=format(EX_MSG, [AKey]);
end;

constructor EDCKeyNotFound.Create(const AKey: integer);
const
  EX_MSG = 'Object not found for key "%d"';
begin
  Message:=format(EX_MSG, [AKey]);
end;

{ EDCValueIncorrectFormat }

constructor EDCValueIncorrectFormat.Create(const AValueType,
  ARequestedType: string);
const
  EX_MSG = 'Can''t get value of type %s for %s';
begin
  Message:=format(EX_MSG, [ARequestedType, AValueType]);
end;

end.
