inherited fmAbout: TfmAbout
  ActiveControl = btOk
  Caption = #1054' '#1055#1083#1072#1075#1080#1085#1077' npp.DBcon'
  ClientHeight = 104
  ClientWidth = 208
  OnCreate = FormCreate
  ExplicitWidth = 214
  ExplicitHeight = 133
  PixelsPerInch = 96
  TextHeight = 13
  object btOk: TButton
    AlignWithMargins = True
    Left = 3
    Top = 76
    Width = 202
    Height = 25
    Align = alBottom
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 63
    ExplicitTop = 77
    ExplicitWidth = 75
  end
  object sgInfo: TStringGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 202
    Height = 67
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    ColCount = 2
    Ctl3D = False
    DefaultColWidth = 100
    DefaultRowHeight = 21
    DoubleBuffered = False
    DrawingStyle = gdsClassic
    Enabled = False
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    Options = []
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ScrollBars = ssNone
    TabOrder = 0
    ExplicitWidth = 193
  end
end