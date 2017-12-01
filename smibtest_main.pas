unit smibtest_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox1MeasureItem(Control: TWinControl; Index: Integer;
      var AHeight: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses usmireader;

var
  ts : TTextStyle;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  sf: TSMIFile;
  i: Integer;
  sx : TStringStream;
  sz: TSRTFile;
begin
  OpenDialog1.FilterIndex:=1;
  if OpenDialog1.Execute then begin
    ListBox1.Clear;
    Memo1.Clear;
    sf:=TSMIFile.Create(OpenDialog1.FileName);
    try
      sf.Load;
      Memo1.Lines.Add(sf.Header.Text);
      //Memo1.Lines.BeginUpdate;
      ListBox1.Items.BeginUpdate;
      for i:=0 to sf.Body.Count-1 do begin
        {
        Memo1.Lines.Add(rawbytestring(sf.Body.TimeToStrEx[i]
                        +' : '+sf.Body.Lang[i]+' : '+sf.Body.Strings[i]));
        }
        ListBox1.AddItem(rawbytestring(IntToStr(i)+' '+ sf.Body.TimeToStrEx[i]
                        +' ('+sf.Body.Lang[i]+')'+LineEnding+sf.Body.Strings[i]),nil);
      end;
      ListBox1.Items.EndUpdate;
      //Memo1.Lines.EndUpdate;
      Memo1.Lines.Add(sf.Tail.Text);
      Memo1.Lines.Add('class : '+sf.Body.LangItem[10]);
      sx := TStringStream.Create('');
      try
        sz:=TSRTFile.Create;
        try
          sz.IsUTF8:=sf.IsUTF8;
          sz.Body.AddStrings(sf.Body);
          sz.SaveTo(sx);
          Memo1.Lines.Add('class : '+sz.Body.LangItem[10]);
        finally
          sz.Free;
        end;
        //Memo1.Lines.Add(sx.DataString);
        sx.SaveToFile(UTF8Decode(ChangeFileExt(OpenDialog1.FileName,'.srt')));
      finally
        sx.Free;
      end;
    finally
      sf.Free;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  sf: TSRTFile;
  sz: TSMIFile;
  i: Integer;
  sx : TStringStream;
begin
  OpenDialog1.FilterIndex:=2;
  if OpenDialog1.Execute then begin
    ListBox1.Clear;
    Memo1.Clear;
    sf:=TSRTFile.Create(OpenDialog1.FileName);
    try
      sf.Load;
      //Memo1.Lines.BeginUpdate;
      ListBox1.Items.BeginUpdate;
      for i:=0 to sf.Body.Count-1 do begin
        {
        Memo1.Lines.Add(rawbytestring(sf.Body.TimeToStrEx[i]
                        +' : '+sf.Body.Strings[i]));
        }
        ListBox1.AddItem(rawbytestring(IntToStr(i)+' '+sf.Body.TimeToStrEx[i]
                        +LineEnding+sf.Body.Strings[i]),nil);
      end;
      ListBox1.Items.EndUpdate;
      //Memo1.Lines.EndUpdate;
      sx := TStringStream.Create('');
      try
        sz:=TSMIFile.Create;
        try
          sz.IsUTF8:=sf.IsUTF8;
          sz.Body.AddStrings(sf.Body);
          sz.SaveTo(sx);
        finally
          sz.Free;
        end;
        //Memo1.Lines.Add(sx.DataString);
        sx.SaveToFile(UTF8Decode(ChangeFileExt(OpenDialog1.FileName,'.smi')));
      finally
        sx.Free;
      end;
    finally
      sf.Free;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ts.Clipping:=True;
  ts.Wordbreak:=False;
  ts.ShowPrefix:=False;
  ts.SingleLine:=False;
  //DefaultSystemCodePage:=CP_UTF8;
  SetDefaultCP;
end;

procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  s:string;
begin
  with TListBox(Control) do begin
    if (Index and 1)=0 then
      Canvas.Brush.Color:=$00eeeeee
      else
        Canvas.Brush.Color:=$00dddddd;
    Canvas.FillRect(ARect);
    Canvas.Font.Color:=clBlack;
    s := ListBox1.Items[Index];
    Canvas.TextRect(ARect,ARect.Left+2,ARect.Top+2,s,ts);
  end;
end;

function CheckLineBreak(const s:string):Integer;
var
  i, l : Integer;
  ch, lch: char;
begin
  Result:=0;
  l:=Length(s);
  i:=1;
  while i<=l do begin
    ch:=s[i];
    if (ch=#$0d) or (ch=#$0a) then begin
      Inc(Result);
      if i+1<=l then begin
        lch:=s[i+1];
        if ((lch=#$0d) or (lch=#$0a)) and (lch<>ch) then
          Inc(i);
      end;
    end;
    Inc(i);
  end;
end;

procedure TForm1.ListBox1MeasureItem(Control: TWinControl; Index: Integer;
  var AHeight: Integer);
begin
  with TListBox(Control) do begin
    AHeight:=Canvas.TextHeight('Qj')*(1+CheckLineBreak(Items[Index]))+8;
  end;
end;

end.

