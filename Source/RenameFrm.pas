unit RenameFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TRenameForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    lbNewName: TLabel;
    txtVarName: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RenameForm: TRenameForm;

implementation


uses devCFG,MultiLangSupport,;
{$R *.dfm}


procedure TRenameForm.FormCreate(Sender: TObject);
begin
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;
  
  Caption := Lang[ID_RNSYM];
  lbNewName.Caption := Lang[ID_RNSYM_NEWNAME];
end;

end.
