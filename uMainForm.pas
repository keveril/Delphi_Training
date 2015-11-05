unit uMainForm;

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

  TWidgetClass = class of TWidget;

  // Form1 definition
  TForm1 = class(TForm)
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
    { Private declarations }
  public
    { Public declarations }
    GenericList: TList<IWidget>;
    // Read the file and store the information
    procedure ReadFromFile(const ASelectedFile: string);
    // Write the information from given file to the text box
    procedure WriteFromFile;
    // Populate the Generic List
    procedure PopulateList(Widget: TWidgetClass; Information: String);

  end;

var
  TWidgetForm: TForm1;

implementation

{$R *.dfm}

// Browse button click event handler
procedure TForm1.BrowseBtnClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
OpenDialog := TOpenDialog.Create(self);

  try
    if OpenDialog.Execute then
    begin
      DirectoryTxtBox.Text := OpenDialog.FileName;
      WidgetComboBox.ItemIndex := 0;
    end;
  finally
   // Clear the listbox and process the file
    ResultListBox.Clear();
  // Free the resources
  OpenDialog.Free;
  end;
end;

 // Load File button click event handler
procedure TForm1.LoadBtnClick(Sender: TObject);
begin
  // Clear the listbox and process the file
  ResultListBox.Clear();
  try
    // Create the list container
    GenericList := TList<IWidget>.Create;

    if Length(DirectoryTxtBox.Text) > 0 then
    begin
      ReadFromFile(DirectoryTxtBox.Text);
      WriteFromFile;
    end
    else
    begin
      ResultListBox.Items.Add('No File Selected');
      ShowMessage('Please select a file using the browse button.');
    end;
  finally
   // Free the resources
    { if we haven't added it to the list,
        we have to free it. }
    FreeAndNil(GenericList);
  end;
end;

procedure TForm1.ReadFromFile(Const ASelectedFile: string);
const
  // Define word separator types that we will recognise
  TAB = #9;
  BLANK = #32;
var
  SReader: TStreamReader;
  StringLine: string;
begin
  // Check the existense of the file
  if  FileExists(ASelectedFile) then
  begin
    try
      // Open and read the file
      SReader := TStreamReader.Create(ASelectedFile);

      while not(SReader.EndOfStream) do
      begin
        StringLine := SReader.ReadLine;
        if WidgetComboBox.ItemIndex = 0 then
        begin
          PopulateList(TBlueWidget, StringLine);
        end
        else
        begin
          PopulateList(TRedWidget, StringLine);
        end;
      end;

    finally
      // Free the resources
      SReader.Close();
      SReader.BaseStream.Free;
      SReader.Free();
    end;
  end
  else
  begin
    ResultListBox.Items.Add('Wrong File Selected');
    ShowMessage('File is not exist, will abort the current process.');
  end;

end;

procedure TForm1.WriteFromFile;
var
  Item: IWidget;
    I: Integer;
begin
  if(GenericList.Count > 0) then
  ResultListBox.Items.Add('ID Description Color Size');
  try
    // Loop and display the Widget class into the list
    for I := 0 to (GenericList.Count - 1) do
    begin
    Item := GenericList[I];
    ResultListBox.Items.Add(Item.GetAsString);
    end;

  finally
    // Free the resources
    Item:= NIL;
  end;
end;

procedure TForm1.PopulateList(Widget: TWidgetClass; Information: String);
const
  // Define word separator types that we will recognise
  TAB = #9;
  BLANK = #32;
var
  SlRow: TStringList;
  Id: Integer;
  Description: String;
begin
  try
    SlRow := TStringList.Create;
    SlRow.StrictDelimiter := true;
    SlRow.Delimiter := TAB;
    SlRow.DelimitedText:= Information;

    Id:= StrToInt(SlRow[0]);
    Description:= SlRow[1];

    GenericList.Add(Widget.Create(Id, Description));
  finally
    FreeAndNil(SlRow);
  end;
end;

end.
