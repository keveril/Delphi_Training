unit uWidget;

interface

uses
  Generics.Collections,
  System.SysUtils;

type
  // An interface definition
  IWidget = interface
  function GetId: Integer;
  function GetDescription: string;
  procedure SetDescription(Value: String);
  property ID: Integer read GetId;
  property Description: string read GetDescription write SetDescription;

  function GetAsString(): string;
  end;

  TWidget = class abstract(TInterfacedObject, IWidget)
  private
    FId: Integer;
    FSize: Integer;
    FDescription: string;
    function GetAsString(): string;
    function GetId: Integer;
    function GetDescription: string;

    procedure SetSize(Value: Integer);
    procedure SetDescription(Value: string);

  protected
    function GetColor(): string; virtual; abstract;

  public
    // Widget properties
    property ID: Integer read GetId;
    property Description: string read GetDescription write SetDescription;
    property AsString: string read GetAsString;
    property Color: string read GetColor;
    property Size: Integer read FSize write SetSize;

    constructor Create(parseID: Integer; parseDescription: string);  virtual;
  end;

  TWidgetClass = class of TWidget;
  //TMyClassClass = class of TWidget;

implementation


// Create a TWidget object - parameterised version
constructor TWidget.Create(parseID: Integer; parseDescription: string);
begin
  inherited Create;
  Size := 0;
  Description := parseDescription;
  FId := parseID;
end;

// Return AsString modified message
function TWidget.GetAsString(): string;
begin
  Result := IntToStr(ID)
   + ' ' + Description
    + #32 + Color
     + #32 + IntToStr(Size);
end;

function TWidget.GetDescription: string;
begin
    Result:= FDescription;
end;

function TWidget.GetId: Integer;
begin
    Result:= FId;
end;

procedure TWidget.SetSize(Value: Integer);
begin
  FSize:=Value;
end;

procedure TWidget.SetDescription(Value: string);
begin
  FDescription:=Value;
end;

end.
