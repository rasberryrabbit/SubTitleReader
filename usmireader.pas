unit usmireader;


{ SMI/SRT Subtitle Reader

  Copyright (C) 2015 rasberryrabbit http://github.com/rasberryrabbit

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

{$define USE_STRADDCAST}


interface

uses
  Classes, SysUtils;

type

  pSubtitleInfo = ^TSubtitleInfo;
  TSubtitleInfo = class
    STime : TTimeStamp;
    lang : string;
    Source : string;
  end;

  { TSUBCaptionList }

  TSUBCaptionList = class(TStringList)
    private
      function GetLangName(Idx: Integer): string;
      function GetSubInfo(Idx:Integer):TSubtitleInfo;
      function GetTimestampString(Idx: Integer): string;
      function GetTimeStart(Idx:Integer):TTimeStamp;
      function GetSource(Idx:Integer):string;
      function GetLang(Idx:Integer):string;
      function GetTimeString(Idx: Integer): string;
    protected
    public
      LangList:string;

      constructor Create;
      destructor Destroy; override;

      function Add(const S: string): Integer; override;
      procedure AddStrings(TheStrings: TStrings); overload; override;
      procedure Clear; override;
      procedure AddLang(const s:string);

      property SubInfo[Idx:Integer]:TSubtitleInfo read GetSubInfo;
      property TimeStart[Idx:Integer]:TTimeStamp read GetTimeStart;
      property Source[Idx:Integer]:string read GetSource;
      property Lang[Idx:Integer]:string read GetLang;
      property TimeToStr[Idx:Integer]:string read GetTimeString;
      property TimeToStrEx[Idx:Integer]:string read GetTimestampString;
      property LangItem[Idx:Integer]:string read GetLangName;
  end;

  { TSubTitleParser }

  TSubTitleParser = class
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

  { TSMIFile }

  TSMIFile = class
    private
      FHeader: TStringList;
      FBody:TSUBCaptionList;
      FTail:TStringList;
      FFile:TFileStream;
      FFileName:string;
      FUTF8:Boolean;
    protected
    public
      KeepEncoding:Boolean;

      constructor Create;
      constructor Create(const FileName: string);
      destructor Destroy; override;

      procedure Load;
      procedure SaveTo(Stream:TStream);
      function ToANSI(const s:string):string;

      property FileName:string read FFileName write FFileName;
      property Header:TStringList read FHeader;
      property Body:TSUBCaptionList read FBody;
      property Tail:TStringList read FTail;

      property IsUTF8:Boolean read FUTF8;
  end;

  { TSRTFile }

  TSRTFile = class
    private
      FBody:TSUBCaptionList;
      FFile:TFileStream;
      FFileName:string;
      FUTF8:Boolean;
    protected
    public
      KeepEncoding:Boolean;

      constructor Create;
      constructor Create(const FileName: string);
      destructor Destroy; override;

      procedure Load;
      procedure SaveTo(stream : TStream; skiplang:string = '');
      function ToANSI(const s:string):string;

      property FileName:string read FFileName write FFileName;
      property Body:TSUBCaptionList read FBody;

      property IsUTF8:Boolean read FUTF8;
  end;


implementation

const
  char_space = '&nbsp;';
  SRT_Between = ' --> ';

{ TSRTFile }

constructor TSRTFile.Create;
begin
  FBody:=TSUBCaptionList.Create;
  FUTF8:=True;
  KeepEncoding:=False;
end;

constructor TSRTFile.Create(const FileName: string);
begin
  FBody:=TSUBCaptionList.Create;
  FUTF8:=True;
  FFileName:=FileName;
end;

destructor TSRTFile.Destroy;
begin
  if Assigned(FFile) then
    FreeAndNil(FFile);
  FBody.Free;
  inherited Destroy;
end;

function ReadTime(const s:string; Idx:Integer; out strtime:string):Integer;
var
  l : Integer;
  lastcolon, lastcomma : Integer;
  ch : char;
begin
  Result:=Idx;
  l:=Length(s);
  lastcolon:=0;
  lastcomma:=0;
  while Result<=l do begin
    ch:=s[Result];
    if ch=':' then
      lastcolon:=Result
    else if ch=',' then
      lastcomma:=Result
    else if (ch<'0') or (ch>'9') then
      break;
    strtime:=strtime+ch;
    Inc(Result);
  end;
end;

function ReadBetween(const s:string; Idx:Integer; out strbet:string):Integer;
var
  l : Integer;
  ch: char;
begin
  Result:=Idx;
  l:=Length(s);
  repeat
    ch:=s[Result];
    if (ch>#32) or (ch=#$0d) or (ch=#$0a) then
      break;
    Inc(Result);
  until Result>l;
  // read between
  repeat
    ch:=s[Result];
    if ch<=#32 then
      break;
    Inc(Result);
  until Result>l;
  // skip space
  repeat
    ch:=s[Result];
    if (ch>#32) or (ch=#$0d) or (ch=#$0a) then
      break;
    Inc(Result);
  until Result>l;
end;

function StrTimeToTimeStamp(const s:string):TTimeStamp;
var
  hh, mm, ss, ms : Integer;
  newtime : TTime;
  temp : string;
  i, tp : Integer;
begin
  temp:='';
  i:=Length(s);
  tp:=3;
  while i>0 do begin
    if (s[i]=':') or (s[i]=',') then begin
      case tp of
      3: if s[i]=',' then begin
           TryStrToInt(temp,ms);
         end else begin
           ms:=0;
           TryStrToInt(temp,ss);
           Dec(tp);
         end;
      2: TryStrToInt(temp,ss);
      1: TryStrToInt(temp,mm);
      end;
      Dec(tp);
      Dec(i);
      temp:='';
    end;
    temp:=s[i]+temp;
    Dec(i);
  end;
  TryStrToInt(temp,hh);
  {
  newtime:=EncodeTime(hh,mm,ss,ms);
  Result:=DateTimeToTimeStamp(newtime);
  }
  Result.Time:=((hh*MinsPerHour+mm)*SecsPerMin+ss)*MSecsPerSec+ms;
  Result.Date:=0;
end;

procedure TSRTFile.Load;
var
  SRTParser:TSubTitleParser;
  Parselvl:Integer;
  sl, temp, stime, dummy:string;
  sidx, ipos, ioff, eidx:Integer;
begin
  FBody.Clear;
  FFile:=TFileStream.Create(FFileName,fmOpenRead);
  try
    Parselvl:=0;
    sidx:=-1;
    ioff:=0;
    SRTParser:=TSubTitleParser.Create;
    try
      SRTParser.Stream:=FFile;
      while not SRTParser.Eof do begin
        SRTParser.ReadLine(sl);
        if (Parselvl>2) and (sl='') then begin
          Parselvl:=0;
          if sidx<>-1 then
            FBody.Strings[sidx]:=temp;
          sidx:=-1;
          temp:='';
        end else
            Inc(Parselvl);
        case Parselvl of
        0: Inc(ioff);
        // index number
        1: ;
        // time
        2: begin
             ipos:=1;
             // start time
             ipos:=ReadTime(sl,ipos,stime);
             sidx:=FBody.Add('');
             FBody.GetSubInfo(sidx).STime:=StrTimeToTimeStamp(stime);
             ipos:=ReadBetween(sl,ipos,dummy);
             // end time
             ipos:=ReadTime(sl,ipos,stime);
             eidx:=FBody.Add(char_space);
             FBody.GetSubInfo(eidx).STime:=StrTimeToTimeStamp(stime);
           end
        // subtitle
        else
           begin
             if temp='' then
               temp:=pchar(SRTParser.ToUTF8(sl))
               else
                 {$ifdef USE_STRADDCAST}
                 temp:=rawbytestring(temp+LineEnding+SRTParser.ToUTF8(sl));
                 {$else}
                 temp:=temp+LineEnding+pchar(SRTParser.ToUTF8(sl));
                 {$endif}
           end;
        end;
      end;
      FUTF8:=SRTParser.IsUTF8;
    finally
      SRTParser.Free;
    end;
  finally
    FreeAndNil(FFile);
  end;
end;

procedure TSRTFile.SaveTo(stream: TStream; skiplang: string);
var
  i, Idx : Integer;
  temp, temp2:string;
  stime, ltime: TTimeStamp;
  idname, classn : string;
begin
  i:=0;
  Idx:=1;
  ltime.Time:=MSecsPerDay;
  temp:='';
  temp2:='';
  while i<FBody.Count do begin
    stime:=FBody.GetSubInfo(i).STime;
    if stime.Time=ltime.Time then begin
      // same time
      classn:=FBody.GetSubInfo(i).lang;
      // skip selected language
      if (skiplang='') or (skiplang<>classn) then begin
        idname:=FBody.GetSubInfo(i).Source;
        if idname<>'' then
          temp2:=temp2+idname+': ';
        {$ifdef USE_STRADDCAST}
        temp2:=rawbytestring(temp2+ToANSI(FBody.Strings[i]));
        {$else}
        temp2:=temp2+pchar(ToANSI(FBody.Strings[i]));
        {$endif}
      end;
    end else begin
      if temp<>'' then begin
        // end time
        temp:=temp+FBody.TimeToStrEx[i]+LineEnding;
        stream.Write(temp[1],Length(temp));
        {$ifdef USE_STRADDCAST}
        temp2:=rawbytestring(temp2+LineEnding);
        {$else}
        temp2:=pchar(temp2)+LineEnding;
        {$endif}
        stream.Write(temp2[1],Length(temp2));
        temp2:=LineEnding;
        stream.Write(temp2[1],Length(temp2));
        temp:='';
        temp2:='';
        Inc(Idx);
      end;
      if LowerCase(pchar(FBody.Strings[i]))=char_space then begin
        // nothing
      end else begin
        // start time
        temp:=IntToStr(Idx)+LineEnding+FBody.TimeToStrEx[i]+SRT_Between;
        classn:=FBody.GetSubInfo(i).lang;
        // only selected language
        if (skiplang='') or (skiplang<>classn) then begin
          idname:=FBody.GetSubInfo(i).Source;
          if idname<>'' then
            {$ifdef USE_STRADDCAST}
            temp2:=rawbytestring(temp2+idname+': ');
            {$else}
            temp2:=pchar(temp2)+idname+': ';
            {$endif}
          {$ifdef USE_STRADDCAST}
          temp2:=rawbytestring(temp2+ToANSI(FBody.Strings[i]));
          {$else}
          temp2:=temp2+pchar(ToANSI(FBody.Strings[i]));
          {$endif}
        end;
      end;
    end;
    ltime:=stime;
    Inc(i);
  end;
end;

function TSRTFile.ToANSI(const s: string): string;
begin
  if KeepEncoding and (not FUTF8) then
    Result:=Utf8ToAnsi(s)
    else
      Result:=s;
end;

{ TSUBCaptionList }

function TSUBCaptionList.GetSubInfo(Idx: Integer): TSubtitleInfo;
begin
  Result:=TSubtitleInfo(Objects[Idx]);
end;

function TSUBCaptionList.GetLangName(Idx: Integer): string;
var
  ss:TStringList;
begin
  ss:=TStringList.Create;
  try
    ss.Delimiter:='|';
    ss.DelimitedText:=LangList;
    if Idx<ss.Count then
      Result:=ss.Strings[idx]
      else begin
        if ss.Count>0 then
          Result:=ss.Strings[ss.Count-1]
          else
            Result:='';
      end;
  finally
    ss.Free;
  end;
end;

function TSUBCaptionList.GetTimestampString(Idx: Integer): string;
var
  subTime:TTime;
  hh, mm, ss, ms : word;
begin
  subTime:=TimeStampToDateTime(TSubtitleInfo(Objects[Idx]).STime);
  DecodeTime(subTime,hh,mm,ss,ms);
  Result:=Format('%.2d:%.2d:%.2d,%.3d',[hh,mm,ss,ms]);
end;

function TSUBCaptionList.GetTimeStart(Idx: Integer): TTimeStamp;
begin
  Result:=TSubtitleInfo(Objects[Idx]).STime;
end;

function TSUBCaptionList.GetSource(Idx: Integer): string;
begin
  Result:=TSubtitleInfo(Objects[Idx]).Source;
end;

function TSUBCaptionList.GetLang(Idx: Integer): string;
begin
  Result:=TSubtitleInfo(Objects[Idx]).lang;
end;

function TSUBCaptionList.GetTimeString(Idx: Integer): string;
begin
  Result:=IntToStr(TSubtitleInfo(Objects[Idx]).STime.Time);
end;

constructor TSUBCaptionList.Create;
begin
  inherited Create;
  OwnsObjects:=True;
  LangList:='';
end;

destructor TSUBCaptionList.Destroy;
begin
  inherited Destroy;
end;

function TSUBCaptionList.Add(const S: string): Integer;
var
  obj : TSubtitleInfo;
begin
  Result:=inherited Add(S);
  obj:=TSubtitleInfo.Create;
  Objects[Result]:=obj;
end;

procedure TSUBCaptionList.AddStrings(TheStrings: TStrings);
var
  i:Integer;
  obj:TSubtitleInfo;
  IsSUBCaption:Boolean;
begin
  IsSUBCaption:=TheStrings is TSUBCaptionList;
  if IsSUBCaption then
    LangList:=TSUBCaptionList(TheStrings).LangList;
  BeginUpdate;
  try
    for i:=0 to TheStrings.Count-1 do begin
      obj:=TSubtitleInfo.Create;
      try
        if IsSUBCaption then begin
          obj.lang:=TSubtitleInfo(TheStrings.Objects[i]).lang;
          obj.Source:=TSubtitleInfo(TheStrings.Objects[i]).Source;
          obj.STime:=TSubtitleInfo(TheStrings.Objects[i]).STime;
        end;
        AddObject(TheStrings.Strings[i],obj);
      except
        obj.Free;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSUBCaptionList.Clear;
begin
  inherited Clear;
  LangList:='';
end;

procedure TSUBCaptionList.AddLang(const s: string);
var
  ss:TStringList;
begin
  ss:=TStringList.Create;
  try
    ss.Delimiter:='|';
    ss.DelimitedText:=LangList;
    if -1=ss.IndexOf(s) then begin
      ss.Add(s);
      LangList:=ss.DelimitedText;
    end;
  finally
    ss.Free;
  end;
end;


{ TSubTitleParser }

function TSubTitleParser.ReadChar(Keep: Boolean): char;
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

function TSubTitleParser.PeekChar: char;
begin
  Result:=ReadChar(True);
end;

function TSubTitleParser.CheckUTF8: Boolean;
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

function TSubTitleParser.ReadBlock(out tag, para: string): Integer;
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

function TSubTitleParser.ReadLine(out str:string): Integer;
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

function TSubTitleParser.Eof: Boolean;
begin
  if fLastread=-1 then
    Result:=False
    else
      Result:=fEof;
end;

function TSubTitleParser.ToUTF8(const s: rawbytestring): rawbytestring;
begin
  if not FUTF8 then
    Result:=AnsiToUtf8(s)
    else
      Result:=s;
end;

constructor TSubTitleParser.Create;
begin
  fLastread:=-1;
  fbufpos:=0;
  fEof:=True;
  fUTF8:=False;
end;

destructor TSubTitleParser.Destroy;
begin
  inherited Destroy;
end;


{ TSMIFile }

constructor TSMIFile.Create;
begin
  FHeader:=TStringList.Create;
  FBody:=TSUBCaptionList.Create;
  FTail:=TStringList.Create;
  FUTF8:=False;
  KeepEncoding:=False;
end;

constructor TSMIFile.Create(const FileName: string);
begin
  FHeader:=TStringList.Create;
  FBody:=TSUBCaptionList.Create;
  FTail:=TStringList.Create;
  FFileName:=FileName;
end;

destructor TSMIFile.Destroy;
begin
  if assigned(FFile) then
    FreeAndNil(FFile);
  FTail.Free;
  FBody.Free;
  FHeader.Free;
  inherited Destroy;
end;

function ReadIdentfier(const s:string; Idx:Integer; out Identfier:string):Integer;
var
  ix, l, spos:Integer;
begin
  ix:=Idx;
  l:=Length(s);
  while ix<=l do begin
    if s[ix]>#32 then
      break;
    Inc(ix);
  end;
  spos:=ix;
  if (ix<=l) and (s[ix]<>'=') then begin
    Inc(ix);
    while ix<=l do begin
      if (s[ix]<=#32) or (s[ix]='=') then
        break;
      Inc(ix);
    end;
  end else
    Inc(ix);
  Result:=ix;
  Identfier:=copy(s,spos,Result-spos);
end;

procedure TSMIFile.Load;
var
  SMIParser:TSubTitleParser;
  tag, str, temp, idstr, attr: string;
  PartId, ipos, attrlvl, ix: Integer;
  InSync: Boolean;
  subTime:TTimeStamp;
  langclass, source : string;
begin
  FHeader.Clear;
  FTail.Clear;
  FBody.Clear;
  FFile:=TFileStream.Create(FFileName,fmOpenRead);
  try
    PartId:=0;
    InSync:=False;
    SMIParser:=TSubTitleParser.Create;
    try
       SMIParser.Stream:=FFile;
       temp:='';
       langclass:='';
       source:='';
       while not SMIParser.Eof do begin
         SMIParser.ReadBlock(tag,str);
         if tag='' then begin
           {$ifdef USE_STRADDCAST}
           temp:=rawbytestring(temp+SMIParser.ToUtf8(str));
           {$else}
           temp:=temp+pchar(SMIParser.ToUtf8(str));
           {$endif}
         end else begin
           tag:=UpperCase(tag);
           if tag='BODY' then begin
             Inc(PartId);
             temp:='';
           end
           else if tag='/BODY' then begin
             Inc(PartId);
             InSync:=False;
             if temp<>'' then begin
               ix:=FBody.Add(temp);
               FBody.GetSubInfo(ix).STime:=subTime;
               FBody.GetSubInfo(ix).lang:=langclass;
               FBody.GetSubInfo(ix).Source:=source;
               temp:='';
               langclass:='';
               source:='';
             end;
           end
           else if (PartId=1) and (tag='/SYNC') then begin
             if temp<>'' then begin
               ix:=FBody.Add(temp);
               FBody.GetSubInfo(ix).STime:=subTime;
               FBody.GetSubInfo(ix).lang:=langclass;
               FBody.GetSubInfo(ix).Source:=source;
               temp:='';
               langclass:='';
               source:='';
             end;
             InSync:=False;
           end
           else if (PartId=1) and (tag='P') then begin
             ipos:=1;
             attrlvl:=0;
             attr:='';
             while ipos<=Length(str) do begin
               ipos:=ReadIdentfier(str,ipos,idstr);
               if (attrlvl=0) then begin
                 Inc(attrlvl);
                 attr:=idstr;
               end else if attrlvl=1 then begin
                 if idstr='=' then
                   Inc(attrlvl)
                   else begin
                     attrlvl:=0;
                     attr:=idstr;
                   end;
               end
               else if attrlvl=2 then begin
                 attrlvl:=0;
                 // class
                 if UpperCase(attr)='CLASS' then begin
                   langclass:=idstr;
                   FBody.AddLang(langclass);
                   // id = source
                   end else if UpperCase(attr)='ID' then
                     source:=idstr;
                 attr:='';
               end;
             end;
           end
           else if (PartId=1) and (tag='/P') then begin
             if temp<>'' then begin
               ix:=FBody.Add(temp);
               FBody.GetSubInfo(ix).STime:=subTime;
               FBody.GetSubInfo(ix).lang:=langclass;
               FBody.GetSubInfo(ix).Source:=source;
               temp:='';
               langclass:='';
               source:='';
             end;
           end
           else if (PartId=1) and (tag='SYNC') then begin
             if temp<>'' then begin
               ix:=FBody.Add(temp);
               FBody.GetSubInfo(ix).STime:=subTime;
               FBody.GetSubInfo(ix).lang:=langclass;
               FBody.GetSubInfo(ix).Source:=source;
               temp:='';
               langclass:='';
               source:='';
             end;
             InSync:=True;
             temp:='';
             // get sync time
             ipos:=1;
             attrlvl:=0;
             attr:='';
             while ipos<=Length(str) do begin
               ipos:=ReadIdentfier(str,ipos,idstr);
               if (attrlvl=0) then begin
                 Inc(attrlvl);
                 attr:=idstr;
               end else if attrlvl=1 then begin
                 if idstr='=' then
                   Inc(attrlvl)
                   else begin
                     attrlvl:=0;
                     attr:=idstr;
                   end;
               end
               else if attrlvl=2 then begin
                 attrlvl:=0;
                 // time
                 if UpperCase(attr)='START' then begin
                   {
                   subTime:=MSecsToTimeStamp(StrToInt(idstr));
                   }
                   subTime.Time:=StrToInt(idstr);
                   subTime.Date:=0;
                 end;
                 attr:='';
               end;
             end;
           end else begin
             if InSync then begin
               if UpperCase(tag)='BR' then
                 {$ifdef USE_STRADDCAST}
                 temp:=RawByteString(temp+LineEnding)
                 {$else}
                 temp:=pchar(temp)+LineEnding
                 {$endif}
               else
               if str='' then
                 {$ifdef USE_STRADDCAST}
                 temp:=rawbytestring(temp+'<'+tag+'>')
                 {$else}
                 temp:=pchar(temp)+'<'+tag+'>'
                 {$endif}
                 else
                   {$ifdef USE_STRADDCAST}
                   temp:=rawbytestring(temp+'<'+tag+' '+SMIParser.ToUtf8(str)+'>');
                   {$else}
                   temp:=pchar(temp)+'<'+tag+' '+pchar(SMIParser.ToUtf8(str))+'>';
                   {$endif}
             end else begin
               if str='' then
                 temp:='<'+tag+'>'
                 else begin
                   if (PartId=1) and (tag='!--') then
                     temp:=''
                     else
                       {$ifdef USE_STRADDCAST}
                       temp:=rawbytestring('<'+tag+' '+SMIParser.ToUtf8(str)+'>')
                       {$else}
                       temp:='<'+tag+' '+pchar(SMIParser.ToUtf8(str))+'>'
                       {$endif}
                 end;
             end;
           end;
         end;
         case PartId of
         0 : FHeader.Add(temp);
         2 : FTail.Add(temp);
         end;
       end;
       FUTF8:=SMIParser.IsUTF8;
    finally
      SMIParser.Free;
    end;
  finally
    FreeAndNil(FFile);
  end;
end;

function LineEndingToBR(const s:string):string;
var
  i,l:Integer;
  ch, lch: char;
begin
  Result:='';
  i:=1;
  l:=Length(s);
  while i<=l do begin
    ch:=s[i];
    if (ch=#$0d) or (ch=#$0a) then begin
      Result:=Result+'<BR>';
      if i+1<=l then begin
        lch:=s[i+1];
        if (lch=#$0d) or (lch=#$0a) and (lch<>ch) then
          Inc(i);
      end;
    end else
      Result:=Result+ch;
    Inc(i);
  end;

end;

procedure TSMIFile.SaveTo(Stream: TStream);
var
  temp, classn, idname, lclassn, lidname : string;
  i : Integer;
  ltime, ctime : TTimeStamp;
begin
  if FHeader.Text='' then
    FHeader.Text:='<SAMI>'+LineEnding+'<HEAD></HEAD>';
  if FTail.Text='' then
    FTail.Text:='</SAMI>';
  temp:=FHeader.Text;
  Stream.Write(temp[1],Length(temp));
  // BODY
  temp:='<BODY>'+LineEnding;
  Stream.Write(temp[1],Length(temp));
  ltime.Time:=MSecsPerDay;
  for i:=0 to FBody.Count-1 do begin
    // SYNC
    ctime:=FBody.GetSubInfo(i).STime;
    if ltime.Time<>ctime.Time then begin
      temp:='<SYNC Start='+FBody.TimeToStr[i]+'>'+LineEnding;
      Stream.Write(temp[1],Length(temp));
    end;
    // P
    temp:='<P';
    classn:=FBody.GetSubInfo(i).lang;
    idname:=FBody.GetSubInfo(i).Source;
    {$ifdef USE_STRADDCAST}
    if classn<>'' then
      temp:=rawbytestring(temp+' Class='+classn);
    if idname<>'' then
      temp:=rawbytestring(temp+' ID='+idname);
    temp:=rawbytestring(temp+'>'+ToANSI(LineEndingToBR(FBody.Strings[i])));
    if (lclassn<>classn)  or (idname<>lidname) then
      temp:=rawbytestring(temp+'</P>');
    temp:=rawbytestring(temp+LineEnding);
    {$else}
    if classn<>'' then
      temp:=pchar(temp)+' Class='+classn;
    if idname<>'' then
      temp:=pchar(temp)+' ID='+idname;
    temp:=pchar(temp)+'>'+ToANSI(LineEndingToBR(FBody.Strings[i]));
    if (lclassn<>classn)  or (idname<>lidname) then
      temp:=pchar(temp)+'</P>';
    temp:=pchar(temp)+LineEnding;
    {$endif}
    Stream.Write(temp[1],Length(temp));
    ltime:=ctime;
    lidname:=idname;
    lclassn:=classn;
  end;
  temp:='</BODY>'+LineEnding;
  Stream.Write(temp[1],Length(temp));
  temp:=FTail.Text;
  Stream.Write(temp[1],Length(temp));
end;

function TSMIFile.ToANSI(const s: string): string;
begin
  if KeepEncoding and (not FUTF8) then
    Result:=Utf8ToAnsi(s)
    else
      Result:=s;
end;


end.
