unit DBBrowser;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  NppDockingForms,
  NppPlugin;

type
  TfmDBBrowser = class(TNppDockingForm)
  private
    FMenuItemCheck: TMenuItemCheck;
    procedure SetMenuItemCheck(const Value: TMenuItemCheck);
  public
    procedure SelItem;
    property MenuItemCheck: TMenuItemCheck read FMenuItemCheck write SetMenuItemCheck;
  end;

implementation

{$R *.dfm}

{ TfmDBBrowser }

procedure TfmDBBrowser.SelItem;
begin
  if MenuItemCheck  = miShown then
  begin
    Hide;
    MenuItemCheck := miHidden;
  end
  else
  begin
    Show;
    MenuItemCheck := miShown;
  end;
end;

procedure TfmDBBrowser.SetMenuItemCheck(const Value: TMenuItemCheck);
begin
  if Value <> FMenuItemCheck then
  begin
    SendMessage(Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, CmdId, LPARAM(Value));
    FMenuItemCheck := Value;
  end;
end;

end.
