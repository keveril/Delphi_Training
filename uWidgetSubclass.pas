unit uWidgetSubclass;

interface

uses
   uWidget,
   System.SysUtils;

type
  TBlueWidget = class(TWidget)
  protected
    function GetColor(): String; override;
  public
    { Public declarations }
    constructor Create(parseID: Integer; parseDescription: String);  override;
  end;

  TRedWidget = class(TWidget)
  protected
    function GetColor(): String; override;
  public
    { Public declarations }
    constructor Create(parseID: Integer; parseDescription: String);  override;
  end;

implementation


// Create a TBlueWidget object - parameterised version
constructor TBlueWidget.Create(parseID: Integer; parseDescription: string);
begin
  // Call the parent constructor first
  Inherited Create(parseID, parseDescription);
  // Set the default variables
  Size := 2;
end;

function TBlueWidget.GetColor;
begin
 Result:= 'Blue';
end;

// Create a TRedWidget object - parameterised version
constructor TRedWidget.Create(parseID: Integer; parseDescription: string);
begin
  // Call the parent constructor first
  Inherited Create(parseID, parseDescription);
  // Set the default variables
  Size := 0;
end;

function TRedWidget.GetColor;
begin
 Result:= 'Red';
end;

end.
