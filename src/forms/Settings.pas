unit Settings;

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
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  NppForms;

type
  TfmSettings = class(TNppForm)
    pnButtons: TPanel;
    pcSettings: TPageControl;
    tsMain: TTabSheet;
    btCancel: TButton;
    btOk: TButton;
    tsConnection: TTabSheet;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
