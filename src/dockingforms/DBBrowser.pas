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
  ZAbstractConnection,
  ZConnection,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Data.DB,
  ZAbstractRODataset,
  ZSqlMetadata,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Grids,
  Vcl.DBGrids,
  JvExDBGrids,
  JvDBGrid,
  JvDBUltimGrid,
  NppPlugin;

type
  TTypeItem = (tiNone, tiSchema, tiSchemaItem, tiTables, tiTablesItem, tiTablesColumn, tiTablesPrimaryKeys,
  tiTablesForeginKeys, tiTablesChecks, tiTablesIndex, tiTablesTriggers, tiTablesColumnItem,
  tiTablesPrimaryKeysItem, tiTablesForeginKeysItem, tiTablesChecksItem, tiTablesIndexItem,
  tiTablesTriggersItem, tiProcedures, tiProceduresItem, tiProceduresColumn,
  tiProceduresColumnItem, tiSequences, tiSequencesItem);

  TTreeItem = class
    TypeItem : TTypeItem;
    TypeMD : TZMetadataType;
    Catalog : String;
    Schema : String;
    DBName : String;
    Filter : String;
    Sort : String;
    constructor Create;
  end;

  TfmDBBrowser = class(TNppDockingForm)
    Connection: TZConnection;
    pnTop: TPanel;
    pnBottom: TPanel;
    btConnect: TButton;
    cbConnection: TComboBox;
    MetaData: TZSQLMetadata;
    ilDBBrowser: TImageList;
    dsMetaData: TDataSource;
    PageControl: TPageControl;
    tsTree: TTabSheet;
    tsTest: TTabSheet;
    lbListMataDataType: TListBox;
    dbgMetadataInfo: TJvDBUltimGrid;
    tvDBBrowser: TTreeView;
    spBottom: TSplitter;
    dbgInfo: TJvDBUltimGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure lbListMataDataTypeClick(Sender: TObject);
    procedure tvDBBrowserChange(Sender: TObject; Node: TTreeNode);
    procedure tvDBBrowserExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    FShowSelect: Boolean;
    FStartDefCon: Boolean;
    FCatalog: String;
    FMenuItemCheck: TMenuItemCheck;
    procedure SetMenuItemCheck(const Value: TMenuItemCheck);
    procedure LoadSettings;
    procedure SetMetaDataList;
    function GetMetaData(AType : TZMetadataType; ACatalog : String = ''; ASchema : String = ''; ADBName : String = ''; AFilter : String = ''; AUnique :Boolean = false; ASort : String = ''): TZSQLMetadata;
    function GetCountMetaData(AType : TZMetadataType; ACatalog : String = ''; ASchema : String = ''; ADBName : String = ''; AFilter : String = ''; AUnique :Boolean = false): Integer;
    procedure CreateTMPNode(AParent: TTreeNode);
    procedure ClearTreeNodeData;
    procedure CreateTree;
    procedure SetCatalogs;
    procedure SetSchemas;
    procedure SetTypeTables(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetProcedures(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetSequences(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesItem(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetProceduresItem(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetSequencesItem(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesContent(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetTablesColumn(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetProceduresContent(AParent: TTreeNode; ATi: TTreeItem);
    procedure SetProceduresColumn(AParent: TTreeNode; ATi: TTreeItem);
    function SetTreeList(Node: TTreeNode): Boolean;
    procedure SetTableInfo(Node: TTreeNode);
  public
    procedure SelItem;
    property MenuItemCheck: TMenuItemCheck read FMenuItemCheck write SetMenuItemCheck;
  end;

implementation

{$R *.dfm}

uses
  common;

{ TfmDBBrowser }

procedure TfmDBBrowser.btConnectClick(Sender: TObject);
begin
  DoConnect(Connection, GetConnection(cbConnection.Items, false, cbConnection.ItemIndex));
  lbListMataDataType.ItemIndex := 0;
  lbListMataDataTypeClick(Sender);
  CreateTree;
end;

procedure TfmDBBrowser.SetCatalogs;
var
  md : TZSQLMetadata;
  count : Integer;
  Catalogs : TStringList;
begin
  Catalogs := TStringList.Create;
  try
    Connection.GetCatalogNames(Catalogs);
    if Catalogs.Count > 0 then
      FCatalog := GetConnection(cbConnection.Items, false, cbConnection.ItemIndex).DataBase
    else FCatalog := '';
  finally
    Catalogs.Free;
  end;
end;

procedure TfmDBBrowser.SetSchemas;
var
 md : TZSQLMetadata;
 ASchema, Filter : String;
 Node, Parent : TTreeNode;
 count : Integer;
 ti : TTreeItem;
begin
  md := GetMetaData(mdSchemas, '', '', '', '', true, 'TABLE_SCHEM');
  try
    md.Open;
    count := md.RecordCount;
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiSchema;
      ti.TypeMD := mdSchemas;
      ti.Catalog := FCatalog;
      Parent := tvDBBrowser.Items.AddChildObject(nil, 'schema (' + IntToStr(count) + ')', Pointer(ti));
      md.First;
      while not md.Eof do
      begin
        ASchema := md.FieldByName('TABLE_SCHEM').AsString;
        ti := TTreeItem.Create;
        ti.TypeItem := tiSchemaItem;
        ti.TypeMD := mdSchemas;
        ti.Schema := ASchema;
        ti.Catalog := FCatalog;
        Node := tvDBBrowser.Items.AddChildObject(Parent, LowerCase(ASchema), Pointer(ti));
        SetTypeTables(Node, ti);
        SetProcedures(Node, ti);
        SetSequences(Node, ti);
        Node.Expand(false);
        md.Next;
      end;
      Parent.Expand(false);
    end else
    begin
      SetTypeTables(nil, nil);
      SetProcedures(nil, nil);
      SetSequences(nil, nil);
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTypeTables(AParent: TTreeNode; ATi: TTreeItem);
var
 md : TZSQLMetadata;
 ATableType, AFilter, ASchema : String;
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
begin
  if ATi <> nil then ASchema := ATi.Schema
  else ASchema := '';

  md := GetMetaData(mdTableTypes, '', '', '', '', true, 'TABLE_TYPE');
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      ATableType := md.FieldByName('TABLE_TYPE').AsString;
      AFilter := 'TABLE_TYPE = ''' + ATableType + '''';
      count := GetCountMetaData(mdTables, FCatalog, ASchema, ATableType, AFilter, true);
      if count > 0 then
      begin
        ti := TTreeItem.Create;
        ti.TypeItem := tiTables;
        ti.TypeMD := mdTables;
        ti.Catalog := FCatalog;
        ti.Schema := ASchema;
        ti.DBName := ATableType;
        ti.Filter := AFilter;
        ti.Sort := 'TABLE_NAME';
        Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(ATableType) + ' (' + IntToStr(count) + ')', Pointer(ti));
        CreateTMPNode(Node);
      end;
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTablesItem(AParent: TTreeNode; ATi: TTreeItem);
var
 md : TZSQLMetadata;
 ATableName, AFilter : String;
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter, true, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      ATableName := md.FieldByName('TABLE_NAME').AsString;
      AFilter := '';
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATableName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(ATableName), Pointer(ti));
      CreateTMPNode(Node);
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetTablesContent(AParent: TTreeNode; ATi: TTreeItem);
var
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
 Filter :String;
begin
  // Columns
  try
    count := GetCountMetaData(mdColumns, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter, true);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesColumn;
      ti.TypeMD := mdColumns;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := ATi.Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := 'ORDINAL_POSITION';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'column (' + IntToStr(count) + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  // Primary Keys
  try
    count := GetCountMetaData(mdPrimaryKeys, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter, true);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesPrimaryKeys;
      ti.TypeMD := mdPrimaryKeys;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := ATi.Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := '';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'primary key (' + IntToStr(count) + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  // Foregin Keys
  try
    Filter := '';
    count := GetCountMetaData(mdImportedKeys, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter, true);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesForeginKeys;
      ti.TypeMD := mdImportedKeys;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := '';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'foreign key (' + IntToStr(count) + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  // Checks
  try
    count := GetCountMetaData(mdExportedKeys, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter, true);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesChecks;
      ti.TypeMD := mdExportedKeys;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := ATi.Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := '';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'check (' + IntToStr(count) + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  //  Index
  try
    count := GetCountMetaData(mdIndexInfo, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter, true);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesIndex;
      ti.TypeMD := mdIndexInfo;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := ATi.Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := '';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'index (' + IntToStr(count) + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
  // Triggers
  try
    Filter := '';
    count := GetCountMetaData(mdTriggers, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter, true);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesTriggers;
      ti.TypeMD := mdTriggers;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.Filter := Filter;
      ti.DBName := ATi.DBName;
      ti.Sort := '';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'trigger (' + IntToStr(count) + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
end;

procedure TfmDBBrowser.SetTablesColumn(AParent: TTreeNode; ATi: TTreeItem);
var
 md : TZSQLMetadata;
 AColumnName, AFilter : String;
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, ATi.DBName, ATi.Filter, true, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      AColumnName := md.FieldByName('COLUMN_NAME').AsString;
      AFilter := '';
      ti := TTreeItem.Create;
      ti.TypeItem := tiTablesColumnItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ATi.DBName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(AColumnName), Pointer(ti));
      md.Next;
    end;
  finally
    md.Free;
  end;
end;


procedure TfmDBBrowser.SetProcedures(AParent: TTreeNode; ATi: TTreeItem);
var
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
 ASchema : String;
begin
  if ATi <> nil then ASchema := ATi.Schema
  else ASchema := '';

  count := GetCountMetaData(mdProcedures, FCatalog, ASchema, '', '', true);
  if count > 0 then
  begin
    ti := TTreeItem.Create;
    ti.TypeItem := tiProcedures;
    ti.TypeMD := mdProcedures;
    ti.Catalog := FCatalog;
    ti.Schema := ASchema;
    ti.Sort := 'PROCEDURE_NAME';
    Node := tvDBBrowser.Items.AddChildObject(AParent, 'procedure (' + IntToStr(count) + ')', Pointer(ti));
    CreateTMPNode(Node);
  end;
end;

procedure TfmDBBrowser.SetProceduresItem(AParent: TTreeNode; ATi: TTreeItem);
var
 md : TZSQLMetadata;
 AProcedureName, AFilter : String;
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, Ati.DBName, ATi.Filter, true, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      AProcedureName := md.FieldByName('PROCEDURE_NAME').AsString;
      AFilter := '';
      ti := TTreeItem.Create;
      ti.TypeItem := tiProceduresItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := AProcedureName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(AProcedureName), Pointer(ti));
      CreateTMPNode(Node);
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetProceduresContent(AParent: TTreeNode; ATi: TTreeItem);
var
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
begin
  // Columns
  try
    count := GetCountMetaData(mdProcedureColumns, ATi.Catalog, Ati.Schema, Ati.DBName, ATi.Filter, true);
    if count > 0 then
    begin
      ti := TTreeItem.Create;
      ti.TypeItem := tiProceduresColumn;
      ti.TypeMD := mdProcedureColumns;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := Ati.DBName;
      ti.Filter := ATi.Filter;
      ti.Sort := 'COLUMN_TYPE';
      Node := tvDBBrowser.Items.AddChildObject(AParent, 'column (' + IntToStr(count) + ')', Pointer(ti));
      CreateTMPNode(Node);
    end;
  except
    //
  end;
end;

procedure TfmDBBrowser.SetProceduresColumn(AParent: TTreeNode; ATi: TTreeItem);
var
 md : TZSQLMetadata;
 AColumnName, AFilter : String;
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, Ati.DBName, ATi.Filter, true, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      AColumnName := md.FieldByName('COLUMN_NAME').AsString;
      AFilter := '';
      ti := TTreeItem.Create;
      ti.TypeItem := tiProceduresColumnItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := Ati.DBName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(AColumnName), Pointer(ti));
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.SetSequences(AParent: TTreeNode; ATi: TTreeItem);
var
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
 ASchema : String;
begin
  if ATi <> nil then ASchema := ATi.Schema
  else ASchema := '';

  count := GetCountMetaData(mdSequences, FCatalog, ASchema, '', '', true);
  if count > 0 then
  begin
    ti := TTreeItem.Create;
    ti.TypeItem := tiSequences;
    ti.TypeMD := mdSequences;
    ti.Catalog := FCatalog;
    ti.Schema := ASchema;
    ti.Sort := 'SEQUENCE_NAME';
    Node := tvDBBrowser.Items.AddChildObject(AParent, 'sequence (' + IntToStr(count) + ')', Pointer(ti));
    CreateTMPNode(Node);
  end;
end;

procedure TfmDBBrowser.SetSequencesItem(AParent: TTreeNode; ATi: TTreeItem);
var
 md : TZSQLMetadata;
 ASequenceName, AFilter : String;
 Node : TTreeNode;
 count : Integer;
 ti : TTreeItem;
begin
  md := GetMetaData(ATi.TypeMD, ATi.Catalog, ATi.Schema, Ati.DBName, ATi.Filter, true, ATi.Sort);
  try
    md.Open;
    md.First;
    while not md.Eof do
    begin
      ASequenceName := md.FieldByName('SEQUENCE_NAME').AsString;
      AFilter := '';
      ti := TTreeItem.Create;
      ti.TypeItem := tiSequencesItem;
      ti.TypeMD := ATi.TypeMD;
      ti.Catalog := ATi.Catalog;
      ti.Schema := ATi.Schema;
      ti.DBName := ASequenceName;
      ti.Filter := AFilter;
      Node := tvDBBrowser.Items.AddChildObject(AParent, LowerCase(ASequenceName), Pointer(ti));
      md.Next;
    end;
  finally
    md.Free;
  end;
end;

procedure TfmDBBrowser.tvDBBrowserExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  AllowExpansion := SetTreeList(Node);
end;

function TfmDBBrowser.SetTreeList(Node: TTreeNode): Boolean;
var
  FirstNode : TTreeNode;
  ti : TTreeItem;
begin
  Result := True;
  FirstNode := Node.GetFirstChild;
  if (FirstNode <> nil) and (FirstNode.Text = 'TMP') then
  begin
    ti := TTreeItem(Node.Data);
    tvDBBrowser.Items.BeginUpdate;
    try
      if ti <> nil then
      begin
        if ti.TypeItem in [tiTables, tiTablesItem, tiTablesColumn, tiProcedures,
        tiProceduresItem, tiProceduresColumn, tiSequences] then tvDBBrowser.Items.Delete(FirstNode);
        case ti.TypeItem of
          tiTables: SetTablesItem(Node, ti);
          tiTablesItem: SetTablesContent(Node, ti);
          tiTablesColumn: SetTablesColumn(Node, ti);
          tiTablesPrimaryKeys: Result := False;
          tiTablesForeginKeys: Result := False;
          tiTablesChecks: Result := False;
          tiTablesIndex: Result := False;
          tiTablesTriggers: Result := False;
          tiTablesColumnItem: Result := False;
          tiTablesPrimaryKeysItem: Result := False;
          tiTablesForeginKeysItem: Result := False;
          tiTablesChecksItem: Result := False;
          tiTablesIndexItem: Result := False;
          tiTablesTriggersItem: Result := False;
          tiProcedures: SetProceduresItem(Node, ti);
          tiProceduresItem: SetProceduresContent(Node, ti);
          tiProceduresColumn: SetProceduresColumn(Node, ti);
          tiProceduresColumnItem: Result := False;
          tiSequences: SetSequencesItem(Node, ti);
          tiSequencesItem: Result := False;
        end;
      end;
    finally
      tvDBBrowser.Items.EndUpdate;
    end;
  end;
end;

procedure TfmDBBrowser.tvDBBrowserChange(Sender: TObject; Node: TTreeNode);
begin
  SetTableInfo(Node);
end;

procedure TfmDBBrowser.SetTableInfo(Node: TTreeNode);
var
  ti : TTreeItem;
begin
  ti := TTreeItem(Node.Data);
  if ti <> nil then
  case ti.TypeItem of
    tiSchema: ;
    tiSchemaItem: ;
    tiTables: ;
    tiTablesItem: ;
    tiTablesColumn: ;
    tiTablesPrimaryKeys: ;
    tiTablesForeginKeys: ;
    tiTablesChecks: ;
    tiTablesIndex: ;
    tiTablesTriggers: ;
    tiTablesColumnItem: ;
    tiTablesPrimaryKeysItem: ;
    tiTablesForeginKeysItem: ;
    tiTablesChecksItem: ;
    tiTablesIndexItem: ;
    tiTablesTriggersItem: ;
    tiProcedures: ;
    tiProceduresItem: ;
    tiProceduresColumn: ;
    tiProceduresColumnItem: ;
    tiSequences: ;
    tiSequencesItem: ;
  end;
end;

procedure TfmDBBrowser.ClearTreeNodeData;
var
  i : Integer;
  ti : TTreeItem;
begin
  for i := 0 to tvDBBrowser.Items.Count - 1 do
  begin
    ti := TTreeItem(tvDBBrowser.Items.Item[i].Data);
    if ti <> nil then
      ti.Free;
  end;
  tvDBBrowser.Items.Clear;
end;

procedure TfmDBBrowser.CreateTMPNode(AParent: TTreeNode);
begin
  tvDBBrowser.Items.AddChildObject(AParent, 'TMP', nil);
end;

procedure TfmDBBrowser.CreateTree;
var
  OldCursor : TCursor;
begin
  tvDBBrowser.Items.BeginUpdate;
  Connection.SQLHourGlass := false;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crSQLWait;
  ClearTreeNodeData;
  try
    SetCatalogs;
    SetSchemas;
  finally
    Screen.Cursor := OldCursor;
    Connection.SQLHourGlass := true;
    tvDBBrowser.Items.EndUpdate;
  end;
end;

procedure TfmDBBrowser.FormCreate(Sender: TObject);
begin
  FCatalog := '';
  FShowSelect := false;
  FStartDefCon := false;
  SetMetaDataList;
  LoadSettings;
  if cbConnection.Items.Count > 0 then
    cbConnection.ItemIndex := 0;
  if FStartDefCon then
    DoConnect(Connection, GetConnection(cbConnection.Items, true, 0));
end;

procedure TfmDBBrowser.FormDestroy(Sender: TObject);
begin
  ClearListConnection(cbConnection.Items);
  ClearTreeNodeData;
  DoDisconnect(Connection);
end;

function TfmDBBrowser.GetCountMetaData(AType : TZMetadataType; ACatalog : String; ASchema : String; ADBName : String; AFilter : String; AUnique :Boolean): Integer;
var
  md : TZSQLMetadata;
begin
  Result := 0;
  md := GetMetaData(AType,ACatalog, ASchema, ADBName, AFilter, true);
  try
    md.Open;
    Result := md.RecordCount;
  finally
    md.Free
  end;
end;

function TfmDBBrowser.GetMetaData(AType : TZMetadataType; ACatalog : String; ASchema : String; ADBName : String; AFilter : String; AUnique :Boolean; ASort : String): TZSQLMetadata;
begin
  Result := TZSQLMetadata.Create(self);
  try
    Result.Connection := Connection;
    Result.MetadataType := AType;
    if ADBName <> '' then
    case AType of
      mdProcedures : Result.ProcedureName := ADBName;
      mdProcedureColumns : Result.ProcedureName := ADBName;
      mdTables : ;
      mdSchemas : ;
      mdCatalogs : ;
      mdTableTypes : ;
      mdColumns : Result.TableName := ADBName;
      mdColumnPrivileges : Result.TableName := ADBName;
      mdTablePrivileges : Result.TableName := ADBName;
      mdBestRowIdentifier :;
      mdVersionColumns :;
      mdPrimaryKeys : Result.TableName := ADBName;
      mdImportedKeys : Result.TableName := ADBName;
      mdExportedKeys : Result.TableName := ADBName;
      mdCrossReference : Result.TableName := ADBName;
      mdTypeInfo : ;
      mdTriggers : Result.TableName := ADBName;
      mdIndexInfo : Result.TableName := ADBName;
      mdSequences : Result.SequenceName := ADBName;
      mdUserDefinedTypes : ;
    end;
    if ASchema <> '' then
    begin
      Result.Schema := ASchema;
      Result.Catalog := '';
    end else Result.Catalog := ACatalog;
    if ASort <> '' then
      Result.SortedFields := ASort;
    if AFilter <> '' then
    begin
      Result.Filter := AFilter;
      Result.Filtered := true;
    end;
    Result.Unique := AUnique;
  except
    Result := nil;
  end;
end;

procedure TfmDBBrowser.lbListMataDataTypeClick(Sender: TObject);
var
  index : Integer;
begin
  index := lbListMataDataType.ItemIndex;
  MetaData.Close;
  MetaData.MetadataType := TZMetadataType(index);
  MetaData.Open;
end;

procedure TfmDBBrowser.LoadSettings;
begin
  FShowSelect := INI.ReadBool('Main', 'ShowListConnections', false);
  FStartDefCon := INI.ReadBool('Main', 'StartDefaultConnection', false);
  cbConnection.Clear;
  cbConnection.Items.AddStrings(LoadConnection);
end;

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

procedure TfmDBBrowser.SetMetaDataList;
begin
  lbListMataDataType.Clear;
  with lbListMataDataType.Items do
  begin
    Add('Procedures');
    Add('Procedure Columns');
    Add('Tables');
    Add('Schemas');
    Add('Catalogs');
    Add('Table Types');
    Add('Columns');
    Add('Column Privileges');
    Add('Table Privileges');
    Add('Best Row Identifier');
    Add('Version Columns');
    Add('Primary Keys');
    Add('Imported Keys');
    Add('Exported Keys');
    Add('Cross Reference');
    Add('Type Info');
    Add('Triggers');
    Add('Index Info');
    Add('Sequences');
    Add('User Defined Types');
  end;
end;

constructor TTreeItem.Create;
begin
  TypeItem := tiNone;
  TypeMD := mdVersionColumns;
  Catalog := '';
  Schema := '';
  DBName := '';
  Filter := '';
  Sort := '';
end;

end.

//function TfmDBBrowser.SetTableColumns(AParent: TTreeNode; ATableName: String; ASchem: String = ''): Integer;
//var
// md : TZSQLMetadata;
// AColumnName, AColumnType, AInfoColumn : String;
// Node : TTreeNode;
//begin
//  Result := 0;
//  md := TZSQLMetadata.Create(nil);
//  try
//    md.MetadataType := mdColumns;
//    md.Connection := Connection;
//    if ASchem <> '' then
//    begin
//      md.Schema := ASchem;
//      md.Catalog := '';
//    end else md.Catalog := FCatalog;
//    md.SortedFields := 'ORDINAL_POSITION';
//    md.Filter := 'TABLE_NAME=''' + ATableName + '''';
//    md.Filtered := true;
//    md.Open;
//    Result := md.RecordCount;
//    if Result > 0 then
//    begin
//      md.First;
//      while not md.Eof do
//      begin
//        AColumnName := md.FieldByName('COLUMN_NAME').AsString;
//        AColumnType := UpperCase(md.FieldByName('TYPE_NAME').AsString);
//        //if AColumnType = 'TEXT' then AColumnType := 'CHAR';
//        AInfoColumn := AColumnType;
//        if (AColumnType = 'VARCHAR') or (AColumnType = 'CHAR')
//          then AInfoColumn := AInfoColumn + '(' + md.FieldByName('COLUMN_SIZE').AsString + ')';
//        if (AColumnType = 'NUMERIC') or (AColumnType = 'DECIMAL')
//          then AInfoColumn := AInfoColumn + '(' + md.FieldByName('COLUMN_SIZE').AsString + ',' + md.FieldByName('DECIMAL_DIGITS').AsString + ')';
//        if not md.FieldByName('COLUMN_DEF').IsNull then AInfoColumn := AInfoColumn + ' DEFAULT ' + md.FieldByName('COLUMN_DEF').AsString;
//        if md.FieldByName('NULLABLE').AsInteger = 1 then AInfoColumn := AInfoColumn + ' NULL'
//        else AInfoColumn := AInfoColumn + ' NOT NULL';
//        Node := tvDBBrowser.Items.AddChild(AParent, LowerCase(AColumnName) + ': ' + AInfoColumn);
//        md.Next;
//      end;
//    end;
//  finally
//    md.Free;
//  end;
//end;

