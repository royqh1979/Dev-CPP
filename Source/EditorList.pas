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

unit EditorList;

interface

uses
  Windows, SysUtils, Dialogs, StdCtrls, Controls, ComCtrls, Forms, Editor, ExtCtrls,
  devrun, version, project, utils, ProjectTypes, Classes, Graphics, Math, Messages;

type
  TLayoutShowType = (lstNone, lstLeft, lstRight, lstBoth);
  TEditorList = class
  private
    fLayout: TLayoutShowType;
    fLeftPageControl: ComCtrls.TPageControl;
    fRightPageControl: ComCtrls.TPageControl;
    fSplitter: TSplitter;
    fPanel: TPanel; // ui component that is the base layer for all page controls
    fUpdateCount: integer;
    function GetForEachEditor(index: integer): TEditor;
    function GetPageCount: integer;
    function GetFocusedPageControl: ComCtrls.TPageControl;
    function GetNewEditorPageControl: ComCtrls.TPageControl;
    procedure ShowLayout(Layout: TLayoutShowType);
  public
    function NewEditor(const Filename: AnsiString;Encoding:TFileEncodingType; InProject, NewFile: boolean; PageControl: ComCtrls.TPageControl = nil):
      TEditor;
    function FileIsOpen(const FileName: AnsiString; ProjectOnly: boolean = FALSE): TEditor;
    function GetEditor(PageIndex: integer = -1; PageControl: ComCtrls.TPageControl = nil): TEditor;
    function GetEditorFromFileName(const FileName: AnsiString): TEditor;
    function GetEditorFromTag(tag: integer): TEditor;
    function IsFileOpened(const FileName: AnsiString):boolean;
    function GetPreviousEditor(Editor: TEditor): TEditor;
    procedure ForceCloseEditor(Editor: TEditor); // close, no questions asked
    function CloseEditor(Editor: TEditor; transferFocus:boolean = True; force:boolean = False): Boolean;
    function CloseAll(force:boolean = False): boolean;
    function CloseAllButThis: boolean;
    function SwapEditor(Editor: TEditor): boolean;
    procedure OnPanelResize(Sender: TObject);
    procedure SelectNextPage;
    procedure SelectPrevPage;
    procedure BeginUpdate; // stops redraws
    procedure EndUpdate; // ends redraws
    procedure UpdateLayout; // reconfigures layout
    procedure GetVisibleEditors(var Left: TEditor; var Right: TEditor);
    procedure SetPreferences(TabPosition: TTabPosition; MultiLine: boolean);
    procedure GetStreamFromOpenedEditor(Sender: TObject; const FileName: String; var Stream: TMemoryStream);
    property LeftPageControl: ComCtrls.TPageControl read fLeftPageControl write fLeftPageControl;
    property RightPageControl: ComCtrls.TPageControl read fRightPageControl write fRightPageControl;
    property Splitter: TSplitter read fSplitter write fSplitter;
    property Panel: TPanel read fPanel write fPanel;
    property PageCount: integer read GetPageCount;
    property Editors[Index: integer]: TEditor read GetForEachEditor; default;
    property FocusedPageControl: ComCtrls.TPageControl read GetFocusedPageControl;
    property Layout: TLayoutShowType read fLayout;
  end;

implementation

uses
  main, MultiLangSupport, DataFrm, devCFG;

function TEditorList.GetPageCount: integer;
begin
  Result := fLeftPageControl.PageCount + fRightPageControl.PageCount;
end;

procedure TEditorList.BeginUpdate;
begin
  Inc(fUpdateCount);
  if fUpdateCount = 1 then
    SendMessage(fPanel.Handle, WM_SETREDRAW, 0, 0); // stop drawing, it's slow
end;

procedure TEditorList.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then begin
    SendMessage(fPanel.Handle, WM_SETREDRAW, 1, 0); // allow drawing again
    RedrawWindow(fPanel.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN); // draw once
  end;
end;

function TEditorList.GetFocusedPageControl: ComCtrls.TPageControl;
var
  ActivePage: TTabSheet;
begin
  case fLayout of
    lstLeft: begin
        Result := fLeftPageControl
      end;
    lstRight: begin
        Result := fRightPageControl
      end;
    lstBoth: begin
        // Check if right is focused, otherwise assume left one is focused
        ActivePage := TTabSheet(fRightPageControl.ActivePage);
        if TEditor(ActivePage.Tag).Text.Focused then
          Result := fRightPageControl
        else
          Result := fLeftPageControl; // no focus -> left one
      end;
    lstNone: begin
        Result := nil;
      end;
  else
    Result := nil;
  end;
end;

function TEditorList.GetForEachEditor(index: integer): TEditor;
begin
  // Is it within range of the first one?
  if (index >= 0) and (index < fLeftPageControl.PageCount) then begin
    result := TEditor(fLeftPageControl.Pages[index].Tag);
    Exit;
  end;

  // Nope? Check second one
  Dec(index, fLeftPageControl.PageCount);
  if (index >= 0) and (index < fRightPageControl.PageCount) then begin
    result := TEditor(fRightPageControl.Pages[index].Tag);
    Exit;
  end;

  Result := nil;
end;

function TEditorList.GetNewEditorPageControl;
begin
  case fLayout of
    lstNone: begin
        Result := fLeftPageControl; // first editor should be shown in the leftmost control
      end;
    lstLeft: begin
        Result := fLeftPageControl;
      end;
    lstRight: begin
        Result := fRightPageControl;
      end;
    lstBoth: begin
        Result := GetFocusedPageControl; // depends on the current keyboard focus
      end;
  else
    Result := nil;
  end;
end;

function TEditorList.NewEditor(const Filename: AnsiString;Encoding:TFileEncodingType; InProject, NewFile: boolean; PageControl: ComCtrls.TPageControl =
  nil):
  TEditor;
var
  ParentPageControl: ComCtrls.TPageControl;
begin
  BeginUpdate;
  try
    if PageControl = nil then
      ParentPageControl := GetNewEditorPageControl
    else
      ParentPageControl := PageControl;
    Result := TEditor.Create(FileName, Encoding,InProject, NewFile, ParentPageControl);

    // Force layout update when creating, destroying or moving editors
    UpdateLayout;
  finally
    EndUpdate; // redraw once
  end;
end;

function TEditorList.FileIsOpen(const FileName: AnsiString; ProjectOnly: boolean = FALSE): TEditor;
var
  e: TEditor;
  I: integer;
begin
  // Check first page control
  for I := 0 to fLeftPageControl.PageCount - 1 do begin
    e := GetEditor(I, fLeftPageControl);
    if SameFileName(e.FileName, FileName) then

      // Accept the file if it's in a project OR if it doesn't have to be in a project
      if (not ProjectOnly) or e.InProject then begin
        Result := e;
        Exit;
      end;
  end;

  // Same for the right page control
  for I := 0 to fRightPageControl.PageCount - 1 do begin
    e := GetEditor(I, fRightPageControl);
    if SameFileName(e.FileName, FileName) then

      // Accept the file if it's in a project OR if it doesn't have to be in a project
      if (not ProjectOnly) or e.InProject then begin
        Result := e;
        Exit;
      end;
  end;

  Result := nil;
end;

function TEditorList.GetEditor(PageIndex: integer = -1; PageControl: ComCtrls.TPageControl = nil): TEditor;
var
  SelectedPageControl: ComCtrls.TPageControl;
  TabSheet: TTabSheet;
begin
  Result := nil;

  // Select page control
  if PageControl = nil then
    SelectedPageControl := GetFocusedPageControl
  else
    SelectedPageControl := PageControl;
  if not Assigned(SelectedPageControl) then
    Exit;

  // Select tab in selected pagecontrol
  case PageIndex of
    -1: TabSheet := TTabSheet(SelectedPageControl.ActivePage);
  else
    TabSheet := TTabSheet(SelectedPageControl.Pages[PageIndex]);
  end;
  if not Assigned(TabSheet) then
    Exit;

  Result := TEditor(TabSheet.Tag);
end;

function TEditorList.GetPreviousEditor(Editor: TEditor): TEditor;
var
  I: integer;
  EditorPageControl: ComCtrls.TPageControl;
  PrevNaturalPage: TTabSheet;
  e: TEditor;
begin
  result := nil;
  if not Assigned(Editor) then
    Exit;

  // Determine what to view next
  EditorPageControl := Editor.PageControl;
  with EditorPageControl do begin

    // If we are closing the active tab, open the tab that was opened when this tab was created
    if ActivePage = Editor.TabSheet then begin // this is the current page...

      // Find the first tab in the history list that is still open
      for I := Editor.PreviousEditors.Count - 1 downto 0 do begin
        e := GetEditorFromTag(Integer(Editor.PreviousEditors[i]));
        if Assigned(e) then begin
          Result := e;
          Exit;
        end;
      end;

      // All history items are gone or this was the first tab to open which has no history
      // Select the editor that would appear naturally when closing this one
      PrevNaturalPage := TTabSheet(FindNextPage(Editor.TabSheet, False, True));
      if Assigned(PrevNaturalPage) and (PrevNaturalPage <> Editor.TabSheet) then begin
        Result := GetEditor(PrevNaturalPage.TabIndex, EditorPageControl);
        Exit;
      end;

      // All editors in the current page control are gone. Try the other page control
      if fLayout = lstBoth then begin
        if EditorPageControl = LeftPageControl then begin
          Result := GetEditor(-1, RightPageControl);
          Exit;
        end else begin
          Result := GetEditor(-1, LeftPageControl);
          Exit;
        end;
      end;

      // If we are not closing the active tab, don't change focus
    end else begin
      Result := GetEditor(-1, EditorPageControl);
    end;
  end;
end;

procedure TEditorList.ForceCloseEditor(Editor: TEditor);
begin
  BeginUpdate;
  try
    FreeAndNil(Editor);

    // Force layout update when creating, destroying or moving editors
    UpdateLayout;
  finally
    EndUpdate; // redraw once
  end;
end;

function TEditorList.CloseEditor(Editor: TEditor; transferFocus:boolean; force:boolean): Boolean;
var
  projindex: integer;
  PrevEditor: TEditor;
begin
  Result := False;
  if not Assigned(Editor) then
    Exit;

  if force then
    editor.save(true,false)
  else if Editor.Text.Modified and not Editor.Text.IsEmpty then begin
    // Ask user if he wants to save
    case MessageDlg(Format(Lang[ID_MSG_ASKSAVECLOSE], [Editor.FileName]), mtConfirmation, mbYesNoCancel, 0) of
      mrYes:
        if not Editor.Save(false, false) then
          Exit;
      mrCancel:
        Exit; // stop closing
    end;
  end;

  PrevEditor := nil;
  // Select editor to open when this one closes
  if transferFocus and (Editor.PageControl.activePage = Editor.TabSheet)then
    PrevEditor := GetPreviousEditor(Editor);

  BeginUpdate;
  try
    // We're allowed to close...
    Result := True;
    {
    while Editor.Parsing do
      Sleep(100);
    }
    // Show new editor after forcing a layout update
    if Assigned(PrevEditor) then begin
      PrevEditor.Activate;
    end;    

    if Editor.InProject and Assigned(MainForm.Project) then begin
      projindex := MainForm.Project.Units.IndexOf(Editor);
      if projindex <> -1 then
        MainForm.Project.CloseUnit(projindex); // calls ForceCloseEditor
    end else begin
      dmMain.AddtoHistory(Editor.FileName);
      FreeAndNil(Editor);
      // Force layout update when creating, destroying or moving editors
      UpdateLayout;
    end;
  finally
    EndUpdate; // redraw once
  end;
end;

function TEditorList.CloseAll(force:boolean = False): boolean;
begin
  Result := False;

  // Redraw once after the whole ordeal
  BeginUpdate;
  try
    // Keep closing the first one to prevent redrawing
    //e:=GetEditor(-1,fLeftPageControl);
    while fLeftPageControl.PageCount > 0 do
      if not CloseEditor(GetEditor(0, fLeftPageControl),False,force) then
        Exit;

    // Same for the right page control
    while fRightPageControl.PageCount > 0 do
      if not CloseEditor(GetEditor(0, fRightPageControl),False,force) then
        Exit;
  finally
    EndUpdate;
  end;

  Result := True;
end;

function TEditorList.CloseAllButThis: boolean;
var
  I: integer;
  ActiveEditor, Editor: TEditor;
begin
  Result := False;

  // Redraw once after the whole ordeal
  BeginUpdate;
  try
    // Keep closing the first one to prevent redrawing
    ActiveEditor := GetEditor(-1, fLeftPageControl);
    for I := fLeftPageControl.PageCount - 1 downto 0 do begin
      Editor := GetEditor(I, fLeftPageControl);
      if Assigned(Editor) and (Editor <> ActiveEditor) then
        if not CloseEditor(Editor,False) then
          Exit;
    end;

    // Keep closing the first one to prevent redrawing
    ActiveEditor := GetEditor(-1, fRightPageControl);
    for I := fRightPageControl.PageCount - 1 downto 0 do begin
      Editor := GetEditor(I, fRightPageControl);
      if Assigned(Editor) and (Editor <> ActiveEditor) then
        if not CloseEditor(Editor,False) then
          Exit;
    end;
  finally
    EndUpdate;
  end;

  Result := True;
end;

function TEditorList.IsFileOpened(const FileName: AnsiString): boolean;
var
  FullFileName: AnsiString;
  I: integer;
  e: TEditor;
begin
  Result := False;

  // ExpandFileName reduces all the "\..\" in the path
  if ContainsStr('..\',FileName) or ContainsStr('\..',FileName) then
    FullFileName := ExpandFileName(FileName)
  else
    FullFileName := FileName;

  // First, check wether the file is already open
  for I := 0 to fLeftPageControl.PageCount - 1 do begin
    e := GetEditor(I, fLeftPageControl);
    if Assigned(e) then begin
      if SameFileName(e.FileName, FullFileName) then begin
        Result := True;
        Exit;
      end;
    end;
  end;

  // Same for the right page control
  for I := 0 to fRightPageControl.PageCount - 1 do begin
    e := GetEditor(I, fRightPageControl);
    if Assigned(e) then begin
      if SameFileName(e.FileName, FullFileName) then begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TEditorList.GetEditorFromFileName(const FileName: AnsiString): TEditor;
var
  FullFileName: AnsiString;
  I: integer;
  e: TEditor;
begin
  Result := nil;

  // ExpandFileName reduces all the "\..\" in the path
  if ContainsStr('..\',FileName) or ContainsStr('\..',FileName) then
    FullFileName := ExpandFileName(FileName)
  else
    FullFileName := FileName;

  // First, check wether the file is already open
  for I := 0 to fLeftPageControl.PageCount - 1 do begin
    e := GetEditor(I, fLeftPageControl);
    if Assigned(e) then begin
      if SameFileName(e.FileName, FullFileName) then begin
        Result := e;
        Exit;
      end;
    end;
  end;

  // Same for the right page control
  for I := 0 to fRightPageControl.PageCount - 1 do begin
    e := GetEditor(I, fRightPageControl);
    if Assigned(e) then begin
      if SameFileName(e.FileName, FullFileName) then begin
        Result := e;
        Exit;
      end;
    end;
  end;

  // Then check the project...
  if Assigned(MainForm.Project) then begin
    I := MainForm.Project.GetUnitFromString(FullFileName);
    if I <> -1 then begin
      result := MainForm.Project.OpenUnit(I);
      Exit;
    end;
  end;

  // Else, just open from disk
  if FileExists(FullFileName) then begin
    Result := NewEditor(FullFileName, etAuto, False, False)
  end;
end;

function TEditorList.GetEditorFromTag(tag: integer): TEditor;
var
  I: integer;
begin
  result := nil;

  // First, check wether the file is already open
  for I := 0 to fLeftPageControl.PageCount - 1 do begin
    if fLeftPageControl.Pages[i].Tag = tag then begin
      result := TEditor(fLeftPageControl.Pages[i].Tag);
      break;
    end;
  end;

  // Same for the right page control
  for I := 0 to fRightPageControl.PageCount - 1 do begin
    if fRightPageControl.Pages[i].Tag = tag then begin
      result := TEditor(fRightPageControl.Pages[i].Tag);
      break;
    end;
  end;
end;

function TEditorList.SwapEditor(Editor: TEditor): boolean;
var
  FromPageControl: ComCtrls.TPageControl;
  FromPageControlPrevTab: TTabSheet;
begin
  Result := True;

  // Redraw once after the whole ordeal
  BeginUpdate;
  try
    // Remember old index
    FromPageControl := Editor.PageControl;
    FromPageControlPrevTab := TTabSheet(Editor.PageControl.FindNextPage(Editor.TabSheet, False, True));

    // Determine how to swap
    if Editor.PageControl = fLeftPageControl then
      Editor.PageControl := fRightPageControl
    else
      Editor.PageControl := fLeftPageControl;

    // Switch to previous editor in the other one
    FromPageControl.ActivePage := FromPageControlPrevTab;

    // Force layout update when creating, destroying or moving editors
    UpdateLayout;

    // Move editor focus too
    Editor.Activate;
  finally
    EndUpdate;
  end;
end;

procedure TEditorList.UpdateLayout;
begin
  if (fLeftPageControl.PageCount = 0) and (fRightPageControl.PageCount = 0) then
    ShowLayout(lstNone)
  else if (fLeftPageControl.PageCount > 0) and (fRightPageControl.PageCount = 0) then
    ShowLayout(lstLeft)
  else if (fLeftPageControl.PageCount = 0) and (fRightPageControl.PageCount > 0) then
    ShowLayout(lstRight)
  else if (fLeftPageControl.PageCount > 0) and (fRightPageControl.PageCount > 0) then
    ShowLayout(lstBoth);
end;

procedure TEditorList.ShowLayout(Layout: TLayoutShowType);
begin
  if Layout = fLayout then
    Exit;
  fLayout := Layout;

  // Apply widths if layout does not change
  case fLayout of
    lstLeft: begin
        fLeftPageControl.Align := alClient;
        fLeftPageControl.Visible := True;

        // Hide other components
        fRightPageControl.Visible := False;
        fRightPageControl.Width := 0;
        fSplitter.Visible := False;
        fSplitter.Width := 0;
      end;
    lstRight: begin
        fRightPageControl.Align := alClient;
        fRightPageControl.Visible := True;

        // Hide other components
        fLeftPageControl.Visible := False;
        fLeftPageControl.Width := 0;
        fSplitter.Visible := False;
        fSplitter.Width := 0;
      end;
    lstBoth: begin
        // Align to the left, assign 50% area
        fLeftPageControl.Align := alClient;
        fLeftPageControl.Visible := True;
        fLeftPageControl.Width := (fPanel.Width - 3) div 2;
        fLeftPageControl.Left := 0;

        // Put splitter in between
        fSplitter.Align := alRight;
        fSplitter.Visible := True;
        fSplitter.Width := 3;
        fSplitter.Left := fLeftPageControl.Width;

        // Align other one to the right
        fRightPageControl.Align := alRight;
        fRightPageControl.Visible := True;
        fRightPageControl.Width := (fPanel.Width - 3) div 2;
        fRightPageControl.Left := fLeftPageControl.Width + 3;
      end;
    lstNone: begin
        fLeftPageControl.Visible := False;
        fLeftPageControl.Width := 0;
        fRightPageControl.Visible := False;
        fRightPageControl.Width := 0;
        fSplitter.Visible := False;
        fSplitter.Width := 0;

        // Normally, change events are triggered by editor focus, but since there's no one left, fake it
        // Notify of change AFTER we change the official layout
        fLeftPageControl.OnChange(fLeftPageControl);
      end;
  end;
end;

procedure TEditorList.SelectNextPage;
var
  PageControl: ComCtrls.TPageControl;
begin
  PageControl := GetFocusedPageControl;
  if Assigned(PageControl) then
    PageControl.SelectNextPage(True);
end;

procedure TEditorList.SelectPrevPage;
var
  PageControl: ComCtrls.TPageControl;
begin
  PageControl := GetFocusedPageControl;
  if Assigned(PageControl) then
    PageControl.SelectNextPage(False);
end;

procedure TEditorList.GetVisibleEditors(var Left: TEditor; var Right: TEditor);
begin
  case fLayout of
    lstNone: begin
        Left := nil;
        Right := nil;
      end;
    lstLeft: begin
        Left := GetEditor(-1, fLeftPageControl);
        Right := nil;
      end;
    lstRight: begin
        Left := nil;
        Right := GetEditor(-1, fRightPageControl);
      end;
    lstBoth: begin
        Left := GetEditor(-1, fLeftPageControl);
        Right := GetEditor(-1, fLeftPageControl);
      end;
  end;
end;

procedure TEditorList.SetPreferences(TabPosition: TTabPosition; MultiLine: boolean);
begin
  LeftPageControl.TabPosition := TabPosition;
  LeftPageControl.MultiLine := MultiLine;
  RightPageControl.TabPosition := TabPosition;
  RightPageControl.MultiLine := MultiLine;
end;

procedure TEditorList.OnPanelResize(Sender: TObject);
//var
//  LeftPageWidthPercent : integer;
begin
  if fLayout = lstBoth then begin
    // Force 50% layout
    // TODO: better option would be to remember pagecontrol width percentages of parent panel
    fLayout := lstNone;
    ShowLayout(lstBoth);
  end;
end;

procedure TEditorList.GetStreamFromOpenedEditor(Sender: TObject; const FileName: String; var Stream: TMemoryStream);
var
  editor: TEditor;
begin
  if IsFileOpened(FileName) then begin
    editor := self.GetEditorFromFileName(FileName);
    Stream := TMemoryStream.Create;
    editor.Text.Lines.SaveToStream(Stream);
  end else
    Stream := nil;
end;

end.

