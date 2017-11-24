unit uLineReader;

{ uLineReader

  Copyright (c) 2015 rasberryrabbit

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LConvEncoding;


type

  { TLineReader }

  TLineReader = class
    private
      fBuffer : array[0..2048] of char;
      fLastread, fbufpos: Integer;
      fEof:Boolean;
      fUTF8:Boolean;
      function ReadChar(Keep:Boolean=False):char;
      function PeekChar:char;
      function CheckUTF8:Boolean;
    public
      Stream : TStream;
      constructor Create;
      destructor Destroy; override;

      function ReadBlock(out tag,para:string):Integer;
      function ReadLine(out str: string): Integer;
      function Eof:Boolean;
      function ToUTF8(const s:rawbytestring):rawbytestring;

      property IsUTF8:Boolean read fUTF8;
  end;

var
  CPToUTF8: function(const s:string):string = @CP949ToUTF8;
  UTF8ToCP_func: function(const s:string; SetTargetCodePage: boolean): RawByteString = @UTF8ToCP949;

implementation


{ TLineReader }

function TLineReader.ReadChar(Keep: Boolean): char;
var
  DoCheckUTF8:Boolean;
begin
  if (fLastread=-1) or (fbufpos>=fLastread) then begin
    DoCheckUTF8:=fLastread=-1;
    fLastread:=Stream.Read(fBuffer[0], sizeof(fBuffer));
    if DoCheckUTF8 then
      fUTF8:=CheckUTF8;
    fbufpos:=0;
    fEof:=fbufpos=fLastread;
  end;
  if fLastread>0 then begin
    Result:=fBuffer[fbufpos];
    if not Keep then
      Inc(fbufpos);
  end else begin
    Result:=#0;
    fEof:=True;
  end;
end;

function TLineReader.PeekChar: char;
begin
  Result:=ReadChar(True);
end;

function TLineReader.CheckUTF8: Boolean;
var
  i, j, hitCount:Integer;
  BitMsk, BitCount:Byte;
begin
  Result:=False;
  i:=0;
  hitCount:=0;
  while i<fLastread do begin
    // skip 0~127
    BitMsk:=0;
    while i<fLastread do begin
      if fBuffer[i]>#$7f then begin
        BitMsk:=Byte(fBuffer[i]);
        break;
      end;
      Inc(i);
    end;
    if BitMsk<>0 then begin
      // get byte counts
      BitCount:=0;
      j:=0;
      BitMsk:=BitMsk shl 1;
      repeat
        if (BitMsk and $80)=0 then
          break
          else
            Inc(BitCount);
        Inc(j);
        BitMsk:=BitMsk shl 1;
      until j>6;
      if (BitCount>0) and (BitCount<7) then begin
        Inc(i);
        while i<fLastread do begin
          if BitCount>0 then begin
            // 10xxxxxx
            if Byte(fBuffer[i]) and $C0<>$80 then
              break
              else
                Dec(BitCount);
          end else
            break;
          Inc(i);
        end;
        if (BitCount=0) then begin
          if (i<fLastread) and (fBuffer[i]<#$7f) then begin
            Inc(hitCount);
            if hitCount>1 then begin
              Result:=True;
              break;
            end;
          end;
        end else
          break;
      end else
        break;
    end;
  end;

end;

function TLineReader.ReadBlock(out tag, para: string): Integer;
var
  ch : char;
  i : integer;
begin
  Result:=0;
  ch := PeekChar;
  if ch = '<' then begin
    while PeekChar<>#0 do begin
      ch:=ReadChar;
      para:=para+ch;
      if ch='>' then
        break;
    end;
    if Length(para)>1 then begin
      i:=2;
      while i<=Length(para) do begin
        ch:=para[i];
        if (ch<=#32) or (ch='>') then
          break;
        tag:=tag+ch;
        Inc(i);
      end;
      while i<=Length(para) do begin
        ch:=para[i];
        if ch>#32 then begin
          Delete(para,1,i-1);
          break;
        end;
        Inc(i);
      end;
      if Length(para)>0 then begin
        i:=Pos('>',para);
        if i>0 then
          Para:=Copy(para,1,i-1);
      end;
    end;
  end else begin
    while PeekChar<>#0 do begin
      if PeekChar='<' then
        break;
      ch:=ReadChar;
      para:=para+ch;
    end;
    para:=trim(para);
  end;
  Result:=Length(para);
  while (PeekChar<=#32) and (PeekChar<>#0) do
    ch:=ReadChar;
end;

function TLineReader.ReadLine(out str:string): Integer;
var
  ch : char;
  i : Integer;
begin
  Result:=0;
  ch:=ReadChar;
  while (ch<>#$0d) and (ch<>#$0a) and (ch<>#$0) do begin
    str:=str+ch;
    ch:=ReadChar;
  end;
  // skip line break
  if (ch=#$0d) or (ch=#$0a) then begin
    if (PeekChar=#$0d) or (PeekChar=#$0a) then begin
      if PeekChar<>ch then
        ch:=ReadChar;
    end;
  end;
  Result:=Length(str);
end;

function TLineReader.Eof: Boolean;
begin
  if fLastread=-1 then
    Result:=False
    else
      Result:=fEof;
end;

function TLineReader.ToUTF8(const s: rawbytestring): rawbytestring;
begin
  if not FUTF8 then begin
    // for linux
    if DefaultSystemCodePage<>CP_UTF8 then
      Result:=AnsiToUtf8(s)
      else
        Result:=CPToUTF8(s);
  end else
      Result:=s;
end;

constructor TLineReader.Create;
begin
  fLastread:=-1;
  fbufpos:=0;
  fEof:=True;
  fUTF8:=False;
  Stream:=nil;
end;

destructor TLineReader.Destroy;
begin
  inherited Destroy;
end;


end.

