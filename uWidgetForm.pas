unit uWidgetForm;

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
  Generics.Collections,
  uWidget,
  uWidgetSubclass;

type


  // Form1 definition
  TWidgetForm = class(TForm)
    ResultListBox: TListBox;
    Label1: TLabel;
    DirectoryTxtBox: TEdit;
    BrowseBtn: TButton;
    WidgetComboBox: TComboBox;
    Label2: TLabel;
    LoadBtn: TButton;
    procedure BrowseBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
  private
    WidgetList: TList<IWidget>;
    procedure ReadFromFile(const ASelectedFile: string; const AWidgetClass: TWidgetClass);
    procedure WriteFromFile;
    procedure PopulateList(Widget: TWidgetClass; Information: String);

  public
    constructor Create(AOwner: TComponent); override;

  end;

var
  WidgetForm: TWidgetForm;

implementation

{$R *.dfm}

const
  // Define word separator types that we will recognise
  TAB = #9;
  BLANK = #32;

procedure TWidgetForm.BrowseBtnClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
OpenDialog := TOpenDialog.Create(self);

  try
    if OpenDialog.Execute then
    begin
      DirectoryTxtBox.Text := OpenDialog.FileName;
    end;
  finally
  OpenDialog.Free;
  end;
end;

constructor TWidgetForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetComboBox.ItemIndex := 0;
end;

procedure TWidgetForm.LoadBtnClick(Sender: TObject);
var WidgetClass: TWidgetClass;
begin
  ResultListBox.Clear();

  if DirectoryTxtBox.Text = '' then
  begin
    ResultListBox.Items.Add('No File Selected');
    ShowMessage('Please select a file using the browse button.');
  end
  else if not FileExists(DirectoryTxtBox.Text) then
  begin
    ResultListBox.Items.Add('Wrong File Selected');
    ShowMessage('File is not exist, will abort the current process.');
  end
  else if WidgetComboBox.ItemIndex < 0 then
  begin
    ShowMessage('select widget type');
  end
  else
  begin
    if WidgetComboBox.ItemIndex = 0 then
    begin
      WidgetClass:= TBlueWidget;
    end
    else
    begin
      WidgetClass:= TRedWidget;
    end;

    try
      WidgetList := TList<IWidget>.Create;
      ReadFromFile(DirectoryTxtBox.Text, WidgetClass);
      WriteFromFile;
    finally
      FreeAndNil(WidgetList);
    end;
  end;
end;

procedure TWidgetForm.ReadFromFile(Const ASelectedFile: string; const AWidgetClass: TWidgetClass);
var
  SReader: TStreamReader;
  StringLine: string;
begin
  // Check the existense of the file
    // Open and read the file
    SReader := TStreamReader.Create(ASelectedFile);
    try
      while not(SReader.EndOfStream) do
      begin
        StringLine := SReader.ReadLine;
        PopulateList(AWidgetClass, StringLine);
      end;

    finally
      SReader.Close();
      SReader.Free();
    end;

end;

procedure TWidgetForm.WriteFromFile;
var
  Widget: IWidget;
begin
  if(WidgetList.Count > 0) then
    ResultListBox.Items.Add('ID Description Color Size');

  for Widget in WidgetList do
    ResultListBox.Items.Add(Widget.GetAsString);
end;

procedure TWidgetForm.PopulateList(Widget: TWidgetClass; Information: String);
var
  SlRow: TStringList;
  Id: Integer;
  Description: String;
begin
  SlRow := TStringList.Create;
  try
    SlRow.StrictDelimiter := true;
    SlRow.Delimiter := TAB;
    SlRow.DelimitedText:= Information;

    Id:= StrToInt(SlRow[0]);
    Description:= SlRow[1];

    WidgetList.Add(Widget.Create(Id, Description));
  finally
    SlRow.Free;
    SlRow := nil;
    SlRow.Free;
    FreeAndNil(SlRow);
    FreeAndNil(SlRow);
  end;
end;

end.
