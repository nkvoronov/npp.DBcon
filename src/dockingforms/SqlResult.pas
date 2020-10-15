unit SqlResult;

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
  Data.DB,
  Vcl.Grids,
  Vcl.DBGrids,
  ZAbstractRODataset,
  ZAbstractDataset,
  ZDataset,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  ZAbstractConnection,
  ZConnection,
  NppDockingForms,
  NppPlugin, ZSqlMonitor;

type
  TfmSQLResult = class(TNppDockingForm)
    pcSQLResult: TPageControl;
    tsMessages: TTabSheet;
    tsResult: TTabSheet;
    mmMessages: TMemo;
    dsResult: TDataSource;
    qResult: TZQuery;
    dbgResult: TDBGrid;
    Connection: TZConnection;
    Monitor: TZSQLMonitor;
    procedure FormCreate(Sender: TObject);
    procedure MonitorLogTrace(Sender: TObject; Event: TZLoggingEvent);
    procedure FormDestroy(Sender: TObject);
  private
    FSQLText: string;
    FMenuItemCheck: TMenuItemCheck;
    procedure SetMenuItemCheck(const Value: TMenuItemCheck);
  public
    procedure SelItem;
    procedure DoSql(const SqlText: string);
    procedure DoConnect;
    property MenuItemCheck: TMenuItemCheck read FMenuItemCheck write SetMenuItemCheck;
  end;

implementation

{$R *.dfm}

{ TfmSQLResult }

procedure TfmSQLResult.DoConnect;
begin
  //
end;

procedure TfmSQLResult.DoSql(const SqlText: string);
begin
  FSQLText := SqlText;
  mmMessages.Lines.Clear;
  qResult.Close;
  qResult.SQL.Text := FSQLText;
  qResult.Open;
  pcSQLResult.ActivePageIndex := 0;
end;

procedure TfmSQLResult.FormCreate(Sender: TObject);
begin
  FSQLText := '';
  pcSQLResult.ActivePageIndex := 0;
  try
    Connection.Connect;
    Monitor.Active := true;
  finally
    //
  end;
end;

procedure TfmSQLResult.FormDestroy(Sender: TObject);
begin
  try
    if Connection.Connected then
    begin
      qResult.Close;
      Connection.Disconnect;
    end;
  finally
    //
  end;

end;

procedure TfmSQLResult.MonitorLogTrace(Sender: TObject; Event: TZLoggingEvent);
begin
  mmMessages.Lines.Add(Event.Message);
end;

procedure TfmSQLResult.SelItem;
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

procedure TfmSQLResult.SetMenuItemCheck(const Value: TMenuItemCheck);
begin
  if Value <> FMenuItemCheck then
  begin
    SendMessage(Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, CmdId, LPARAM(Value));
    FMenuItemCheck := Value;
  end;
end;

end.
