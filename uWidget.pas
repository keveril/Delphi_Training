unit uWidget;

interface

uses
  Generics.Collections,
  System.SysUtils;

type
  // An interface definition
  IWidget = Interface
  function GetAsString(): String;
  end;

  // Define our  class
  TWidget = class abstract(TInterfacedObject, IWidget)
  private
    FId: Integer;
    FSize: Integer;
    FDescription: String;
    function GetAsString(): String;

    procedure SetSize(Value: Integer);
    procedure SetDescription(Value: String);

  protected
    function GetColor(): String; Virtual; Abstract;

  public
    // Widget properties
    property ID: Integer read FId;
    property Description: String read FDescription write SetDescription;
    property AsString: String read GetAsString;
    property Color: string read GetColor;
    property Size: Integer read FSize write SetSize;

    // TWidget constructor
    constructor Create(parseID: Integer; parseDescription: String);  virtual;
  end;

  //TMyClassClass = class of TWidget;

implementation


// Create a TWidget object - parameterised version
constructor TWidget.Create(parseID: Integer; parseDescription: string);
begin
  // Set the default variables
  Size := 0;
  Description := parseDescription;
  FId := parseID;
end;

// Return AsString modified message
function TWidget.GetAsString(): String;
begin
  Result := IntToStr(ID)
   + #32 + Description
    + #32 + Color
     + #32 + IntToStr(Size);
end;

procedure TWidget.SetSize(Value: Integer);
begin
  if Value <> FSize then
  begin
  FSize:=Value;
  end;
end;

procedure TWidget.SetDescription(Value: String);
begin
  if Value <> FDescription then
  begin
  FDescription:=Value;
  end;
end;

end.
