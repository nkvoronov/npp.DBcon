object fmSettings: TfmSettings
  Left = 0
  Top = 0
  ActiveControl = btCancel
  BorderStyle = bsDialog
  Caption = 'Settings npp.DBcon'
  ClientHeight = 222
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnButtons: TPanel
    Left = 0
    Top = 192
    Width = 351
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btCancel: TButton
      AlignWithMargins = True
      Left = 273
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
    end
    object btOk: TButton
      AlignWithMargins = True
      Left = 192
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
  end
  object pcSettings: TPageControl
    Left = 0
    Top = 0
    Width = 351
    Height = 192
    ActivePage = tsMain
    Align = alClient
    TabOrder = 0
    object tsMain: TTabSheet
      Caption = 'Main'
    end
    object tsConnection: TTabSheet
      Caption = 'Connection'
      ImageIndex = 1
    end
  end
end
