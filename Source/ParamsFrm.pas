{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit ParamsFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls;

type
  TParamsForm = class(TForm)
    grpParameters: TGroupBox;
    ParamEdit: TEdit;
    grpHost: TGroupBox;
    HostEdit: TEdit;
    LoadBtn: TSpeedButton;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    cbUseParams: TCheckBox;
    cbUseInputFile: TCheckBox;
    grpInputFile: TGroupBox;
    btnChooseInputFile: TSpeedButton;
    txtInputFile: TEdit;
    procedure LoadBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnChooseInputFileClick(Sender: TObject);
    procedure cbUseParamsClick(Sender: TObject);
    procedure cbUseInputFileClick(Sender: TObject);
  private
    procedure LoadText;
    { Private declarations }
  public
    procedure DisableHost;
    { Public declarations }
  end;

implementation

uses
  MultiLangSupport, devcfg, utils,version;

{$R *.dfm}

procedure TParamsForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_PARAM_CAPTION];
  grpParameters.Caption := Lang[ID_PARAM_PARAMS];
  grpHost.Caption := Lang[ID_PARAM_HOST];
  OkBtn.Caption := Lang[ID_BTN_OK];
  CancelBtn.Caption := Lang[ID_BTN_CANCEL];
  cbUseInputFile.Caption := Lang[ID_PARAM_REDIRECT_INPUT];
  grpInputFile.Caption := Lang[ID_PARAM_GRP_INPUTFILE];
  cbUseParams.Caption := Lang[ID_PARAM_USE_PARAMS];

end;

procedure TParamsForm.LoadBtnClick(Sender: TObject);
begin
	with TOpenDialog.Create(self) do try
    Filter := BuildFilter([FLT_EXECUTABLES]);
		if Execute then
			HostEdit.Text := FileName;
	finally
		Free;
	end;
end;

procedure TParamsForm.DisableHost;
begin
	HostEdit.Enabled := false;
	LoadBtn.Enabled := false;
end;



procedure TParamsForm.FormCreate(Sender: TObject);
begin
	LoadText;
  ParamEdit.Enabled := cbUseParams.Checked;
  txtInputFile.Enabled := cbUseInputFile.Checked;
  btnChooseInputFile.Enabled := cbUseInputFile.Checked;
end;

procedure TParamsForm.btnChooseInputFileClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do try
//    Filter := BuildFilter([FLT_PROJECTS, FLT_CS, FLT_CPPS, FLT_RES, FLT_HEADS]);
//    Title := Lang[ID_NV_OPENFILE];
//    Options := Options + [ofAllowMultiSelect];
    Filter := BuildFilter([FLT_TEXTS,FLT_ALLFILES]);
    // Open all provided files
    if Execute then
      txtInputFile.Text := FileName;
  finally
    Free;
  end;
end;

procedure TParamsForm.cbUseParamsClick(Sender: TObject);
begin
  ParamEdit.Enabled := cbUseParams.Checked;
end;

procedure TParamsForm.cbUseInputFileClick(Sender: TObject);
begin
  txtInputFile.Enabled := cbUseInputFile.Checked;
  btnChooseInputFile.Enabled := cbUseInputFile.Checked;
end;

end.
