unit About;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Variants,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Grids,
  NppForms;


type
  TfmAbout = class(TNppForm)
    btOk: TButton;
    sgInfo: TStringGrid;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

Const
  NoSelection : TGridRect = (Left:-1; Top:-1; Right:-1; Bottom:-1 );

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  inherited;
  sgInfo.Cells[0,0] := 'Версия';
  sgInfo.Cells[1,0] := '1.0';
  sgInfo.Cells[0,1] := 'Автор';
  sgInfo.Cells[1,1] := 'Воронов Николай';
  sgInfo.Cells[0,2] := 'Сайт';
  sgInfo.Cells[1,2] := '';
  sgInfo.Selection  := NoSelection;
end;

end.
