{*******************************************************}
{                                                       }
{       Danfoss TripleLynx Pro WR Datenauswertung       }
{       Commandline LINUX to create HTML protocol       }
{       Copyright (c) 2011/2012 Helmut Elsner           }
{                                                       }
{       Compiler: FPC 2.6.1   /    Lazarus 1.1          }
{                                                       }
{*******************************************************}

program cpvausw;

{ Code von PV_ausw V0.4 vom 19.11.2011 übernommen 
  Update auf V1.1 am        21.12.2011
  Update auf V3.1 am        08.07.2012 (+ Archivierung!)
  Update auf V3.2 am        13.07.2012 (+ min_day.js)
  Update auf V4.3 am        26.05.2019 (Update HTML Generierung)   }

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, DateUtils, FileUtil;

type

{ TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   WriteHelp; virtual;
  end;

{ TMyApplication }
const
  Version='V0.8  05/2019 based on PV_Ausw 4.3';
  inidat='PV_Ausw.xml';

function SDToTime(s: string): TDateTime;        {Datum YYYY-MM-DD in TDateTime}
begin
  result:=0;
  try
    case length(s) of
      10: result:=EncodeDate(StrToInt(copy(s,1,4)),
                             StrToInt(copy(s,6,2)),
                             StrToInt(copy(s,9,2)));     {Tag}
       7: result:=EncodeDate(StrToInt(copy(s,1,4)),
                             StrToInt(copy(s,6,2)),1);   {Monat}
       4: result:=EncodeDate(StrToInt(copy(s,1,4)),1,1); {Jahr}
    end;
  except
    result:=0;
  end;
end;

Function TToTT(s: string): TDateTime;           {Zeitstempel hh:mm in TDateTime}
begin
  result:=0;
  try
    if length(s)>4 then result:=EncodeTime(StrToInt(copy(s,1,2)),
                                           StrToInt(copy(s,4,2)), 0, 0);
  except
    result:=0;
  end;
end;

function getval(w, s: string): string;  {sucht ID w in string s und gibt Wert}
var x, p: integer;                      {zwischen ".." zurück}
    start: boolean;
begin
  result:='';
  start:=false;
  p:=pos(w, s);
  if p>0 then for x:=p to length(s) do begin
    if start then begin
      if s[x]='"' then break;
      result:=result+s[x];
    end;
    if s[x]='"' then start:=true;
  end;
end;

function KorrFloat(w: extended): string;  {Ausgabe immer mit Komma,
                  3 Stellen hinter dem Komma und ohne Tausendertrennzeichen}
begin
  result:=IntToStr(round(w*1000));
  while length(result)<4 do result:='0'+result;            {führende Nullen}
  insert(',', result, length(result)-2);
end;

procedure TMyApplication.DoRun;
const
  datbeg='[wr]';
  datend='[wr_ende]';
  infbeg='[wr_def_start]';
  infend='[wr_def_end]';
  headerID='INTERVAL';
  archID  ='arch_';           {archivierte Datawarehousedateien}
  archfn  ='days_hist.csv';   {Dateiname für Archivierte Daten}
  sep=';';                    {Datenseperator für csv}
  intgt='<td style="color:#009900;">';       {grün: im Soll}
  outgt='<td style="color:#990000;">';       {rot: Soll (noch) nicht erreicht}

{siehe http://wiki.sonnenertrag.eu/datenimport:voraussetzung:voraussetzung

 base_vars.js Hier vergleichen wir die Anlagenkonfiguration und die
              Anlagenleistung. Nur mit dieser Datei ist der Datenimport ab
              01.04.2011 möglich. Ausgenommen sind Portale wie Sunny Portal,
              die diese Überprüfung bereits intern durchführen.
 WRInfo[0]=new Array("WR Kurzname","???",Angeschlossene AC Leistung,0,
           "WR Langname",Anzahl Strings,null,null,0,null,1,0,0,1000,null)

 days_hist.js In dieser Datei sind die Tagesdaten hinterlegt.
da[dx++]="Datum|Ertrag;Maximalleistung"

 months.js    Diese Datei benötigen wir für den Monat.
mo[mx++]="Datum|Ertrag"

 Anleitung Anmeldung: http://wiki.sonnenertrag.eu/datenimport:selfmade}

   basejs='base_vars.js';
   daysjs='days_hist.js';
   monsjs='months.js';
   histjs='min_day.js';     {Intervallmesswerte}

var
  ErrorMsg: String;
  outlist, ProtHTM, DayList: TStringList;
  FileList, InList, SplitList, MonList: TStringList;
  x, y, offs: integer;
  infoon, dataon, json: boolean;
  rs, ffilter, suchpfad, wrnummer, wrname, s: string;
  spezsoll, htmlfile, numstr, ltag, farbe: string;
  pla, maxlei, maxert, w, maxld: integer;
  ter, mer, lastmer, ger, anzt, anztm, anzts, ertr, korre: integer;

begin

// quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

// parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

// Create lists, set initial data
  OutList:=TStringlist.Create;
  DayList:=TStringlist.Create;
  FileList:=TStringList.Create;  {speichert gefundene Dateinamen zur Auswertung}
  InList:=TStringList.Create;    {speichert den Inhalt einer Datei}
  SplitList:=TStringList.Create; {Hilfsliste zum Aufsplitten separierter Daten}
  SplitList.Delimiter:=sep;
  ProtHTM:=TStringList.Create;
  MonList:=TStringList.Create;
  
  infoon:=false;
  dataon:=false;
  korre:=0;    {Korrekturwert bei nicht zurückgesetztem Ertrag am nächsten Tag}

  json:=false;
  maxert:=0;
  maxlei:=0;
  maxld:=0;
  ter:=0;    {Tagesertrag}
  mer:=0;    {Monatsertrag}
  lastmer:=0;
  ger:=0;    {gesamtertrag}
  anzt:=1;   {Anzahl Tage gesamt}
  anztm:=1;  {Anzahl Tage im Monat}
  ltag:='';  {letzter Tag}

// Daten aus dem XML file lesen
  ErrorMsg:='No proper integer value in XML file';
  try
    FileList.LoadFromFile(ExtractFilePath(paramstr(0))+inidat);
    for x:=0 to FileList.Count-1 do if FileList[x][1]<>'#' then begin
      rs:=getval('DatawarehouseRoot', FileList[x]);
      if rs>'' then suchpfad:=rs;
      rs:=getval('FileFilter', FileList[x]);
      if rs>'' then ffilter:=rs;
      rs:=getval('ProtHTMDat', FileList[x]);
      if rs>'' then htmlfile:=rs;
      rs:=getval('SpezJErtragSoll', FileList[x]);
      if rs>'' then spezsoll:=rs;
      rs:=getval('JSDateienAnlegen', FileList[x]);
      if rs>'' then json:=rs='1';
      rs:=getval('NumStrings', FileList[x]);  {Anzahl PV-Arrays (Strings)}
      if rs>'' then numstr:=rs;  {Anzahl PV-Arrays (Strings)}
      try
        rs:=getval('ProtHTMZeiNr', FileList[x]);
        if rs>'' then offs:=StrToInt(rs);
        rs:=getval('PeakLeistungAnlage', FileList[x]);
        if rs>'' then pla:=StrToInt(rs);
      except
        ShowException(Exception.Create(ErrorMsg));
        terminate;   {nichts zum Laden gefunden}
        exit;
      end;
    end;
  finally
    filelist.Clear;
  end;

// Create File list from datawarehouse
  ErrorMsg:='Error during create File List';
  try
    FileList:=FindAllFiles(suchpfad, ffilter, false);
    if FileList.Count>0 then begin
      FileList.Sort;
	  
// Read data from archived days if available
      suchpfad:=IncludeTrailingPathDelimiter(suchpfad);
      if FileExists(suchpfad+archID+archfn) then
        DayList.LoadFromFile(suchpfad+archID+archfn);
      anzt:=DayList.Count;                   {Anzahl Tage gesamt}

// Fill OutList from DW files; Change this for other inverters (i.e. SMAspot)
      ErrorMsg:='Error during fill OutList';
      for x:=0 to FileList.Count-1 do begin  {alle gefunden Dateien bearbeiten}
        InList.LoadFromFile(FileList[x]);
        for y:=0 to InList.Count-1 do begin  {eine Datei auswerten}
          if InList[y]=infend then infoon:=false;
          if InList[y]=datend then dataon:=false;
          if infoon then begin  {Sektion "wr_def" einlesen (eine Zeile)}
            SplitList.DelimitedText:=InList[y];
            if SplitList.Count>4 then begin
              WRName:=SplitList[3];    {Name}
              WRNummer:=SplitList[4];  {WR Nummer}
            end;
          end;
          if Dataon then begin  {Sektion "wr" einlesen (viele Zeilen)}
            SplitList.DelimitedText:=InList[y];
            {nur Datenzeilen auslesen, ohne Überschrift}
            if (SplitList.Count>30) and (SplitList[0]<>headerID) then begin
  {Korrektur Danfossfehler: 1. Ertrag vom Vortag, später erst zurückgesetzt,
                               aber Spannung AC=0
                            2. Ertrag nicht zurückgesetzt, sondern wird weiter
                               aufaddiert}
              rs:=StringReplace(SplitList[5], '.', '', [rfReplaceAll]);
              ertr:=StrToInt(rs);
              if ltag<>SplitList[1] then begin         {neuer Tag}
                korre:=0;
                if (ltag>'') and (ertr>0) and
                   (InList.Count>10) then korre:=ertr; {Korrekturwert erstellen}
                ltag:=SplitList[1];
              end;
              if (ertr>0) and (ertr<korre) then korre:=0;    {wegen 1.}
              if ertr>=korre then ertr:=ertr-korre;          {1. und 2.}
              if length(SplitList[12])>3 then begin    {Daten reduzieren}
                rs:=SplitList[1]+sep+                  {Datum}
                    copy(SplitList[2], 1, 5)+sep+      {Zeit}
                    SplitList[4]+sep+                  {Leistung}
                    IntToStr(ertr)+sep+                {Energieertrag}
                    SplitList[6]+sep+     {Temperatur Wechselrichter}
                    SplitList[13]+sep+    {Spannung Strang 1}
                    SplitList[14]+sep+    {Strom Strang 1}
                    SplitList[15]+sep+    {Spannung Strang 2}
                    SplitList[16]+sep+    {Strom Strang 2}
                    SplitList[17]+sep+    {Spannung Strang 3}
                    SplitList[18]+sep+    {Strom Strang 3}
                    SplitList[7]+sep+     {Spannung AC}
                    SplitList[12]+sep+    {Frequenz}
                    SplitList[0];         {Interval}
                rs:=StringReplace(rs, sep+'0.', sep, [rfReplaceAll]);
                rs:=StringReplace(rs, '.', '', [rfReplaceAll]);
                rs:=StringReplace(rs, sep+'000', sep+'0', [rfReplaceAll]);
                OutList.Add(rs);          {rawdata anlegen}
              end;
            end;  {ende if (SplitList.Count>30)}
          end;
          if InList[y]=datbeg then dataon:=true;
          if InList[y]=infbeg then infoon:=true;
        end;    {Ende eine Datei auswerten}
      end;  {Ende alle Dateien durchsuchen}

// Check daylist + outlist and do statistics
      if OutList.Count>1 then begin
	  
// Create definition file base_vars.js for sonnenertrag.eu (see wiki)
        ErrorMsg:='Error during create def file';
        ltag:=ExtractFilePath(htmlfile)+basejs;
        if json and (htmlfile<>'') and not(FileExists(ltag)) then begin
          ProtHTM.Add('var AnlagenKWP='+IntToStr(pla));
          ProtHTM.Add('var time_start=new Array(7,7,6,6,5,4,4,5,6,7,7,8)');
          ProtHTM.Add('var time_end=new Array(17,18,20,21,21,22,22,21,20,19,17,16)');
//        ProtHTM.Add('var sollMonth=new Array(2,6,9,11,11,13,13,12,10,6,4,3)');
          ProtHTM.Add('var SollYearKWP='+IntToStr(Round(StrToFloat(spezsoll))));
          ProtHTM.Add('var AnzahlWR=1');
          ProtHTM.Add('var MaxWRP=new Array(1)');
          ProtHTM.Add('MaxWRP[0]=new Array('+IntToStr((pla*120) div 100)+','+
                                             IntToStr((pla*750) div 100)+','+
                                             IntToStr(round(pla/100)*16000)+','+
                                             IntToStr(round(pla/100)*130000)+')');
          ProtHTM.Add('var WRInfo=new Array(1)');
          farbe:='WRInfo[0]=new Array("Danfoss TLX Pro","'+
                WRNummer+'",'+IntToStr(pla)+',0,"'+WRName+'",'+
                numstr+',null,null,0,null,1,0,1,1000,null)';
          ProtHTM.Add(farbe);
          farbe:='WRInfo[0][6]=new Array(';
          case StrToInt(numstr) of             {new array}
            1: farbe:=farbe+'"String 1")';
            2: farbe:=farbe+'"String 1","String 2")';
            3: farbe:=farbe+'"String 1","String 2","String 3")';
          end;
          ProtHTM.Add(farbe);
          farbe:='WRInfo[0][7]=new Array(';
          case StrToInt(numstr) of             {new array}
            1: farbe:=farbe+'1)';
            2: farbe:=farbe+'1,1)';
            3: farbe:=farbe+'1,1,1)';
          end;
          ProtHTM.Add(farbe);
          ProtHTM.Add('var StatusCodes=new Array(1)');
          ProtHTM.Add('var FehlerCodes=new Array(1)');
          ProtHTM.Add('StatusCodes[0]="----"');
          ProtHTM.Add('FehlerCodes[0]="----"');
          ProtHTM.Add('var isTemp=true');
          ProtHTM.Add('var DATALOGGER_NAME="cpvausw"');
          ProtHTM.Add('var DATALOGGER_VERSION="'+version+'"');
          try
            ProtHTM.SaveToFile(ltag);
          except
            ErrorMsg:='Could not save '+basejs;
          end;
          ProtHTM.Clear;
        end;

// Tagesdaten und Monatserträge aus dem Archiv
        ErrorMsg:='Error during check of DayList';
        ltag:='';  {letzter Tag}
        if DayList.Count>0 then for x:=0 to DayList.Count-1 do begin
          SplitList.DelimitedText:=DayList[x];
          if (ltag>'') and
             (copy(DayList[x],6,2)<>copy(ltag,6,2)) then begin  {neuer Monat}
            MonList.Insert(0, 'mo[mx++]="'+
                              FormatDateTime('dd.mm.yy', SDToTime(ltag))+
                              '|'+IntToStr(mer)+'"');
            mer:=0;
          end; {Ende Monat}
          maxld:=StrToInt(SplitList[1]);
          if maxld>maxlei then maxlei:=maxld;    {Peakleistung gesamt}
          ter:=StrToInt(SplitList[2]);
          if ter>maxert then maxert:=ter;
          ger:=ger+ter;                          {Gesamtertrag}
          mer:=mer+ter;                          {Monatsertrag}
          ltag:=copy(DayList[x],1,10);
        end;
        maxld:=0;
        ter:=0;
        if ltag='' then anzt:=1;

// Check the whole rawdata file and compute reported values
        ErrorMsg:='Error during check of OutList';
        for x:=0 to OutList.Count-1 do begin   {Gesamtliste für Statistik durchsuchen}
          SplitList.DelimitedText:=OutList[x];
          w:=StrToInt(SplitList[2]);
          if w>maxlei then maxlei:=w;  {Peakleistung gesamt}
          if w>maxld then maxld:=w;    {Peakleistung für den Tag}
          w:=StrToInt(SplitList[3]);   {Ertragswert in W}
          if copy(OutList[x],1,10)<>ltag then begin   {neuer Tag}
            if ltag<>'' then begin
              mer:=mer+ter;   {Monatsertrag}
              ger:=ger+ter;   {Gesamtertrag}
              if ter>1 then ProtHTM.Insert(0, 'da[dx++]="'+
                                   FormatDateTime('dd.mm.yy', SDToTime(ltag))+
                                   '|'+IntToStr(ter)+';'+IntToStr(maxld)+'"');
              if copy(OutList[x],6,2)<>copy(ltag,6,2) then begin  {neuer Monat}
                if anztm=DaysInMonth(SDToTime(ltag)) then lastmer:=mer;
                MonList.Insert(0, 'mo[mx++]="'+FormatDateTime('dd.mm.yy', SDToTime(ltag))+
                                  '|'+IntToStr(mer)+'"');
                mer:=0;
                anztm:=0;
              end;    {Monat}
              inc(anzt);
              inc(anztm);
              ter:=0;
              maxld:=0;   {Peakleitung am Tag}
            end;
            ltag:=copy(OutList[x],1,10);
          end;  {Tag}
          if w>maxert then maxert:=w;
          if w>ter then ter:=w;        {Tagesertrag}
        end;
        anzts:=DaysInYear(SDToTime(ltag)); {Solltage Jahr für spezifischen Jahresertrag}
        mer:=mer+ter;   {Monatsertrag}
        ger:=ger+ter;   {Gesamtertrag}
        ProtHTM.Insert(0, 'da[dx++]="'+FormatDateTime('dd.mm.yy',SDToTime(ltag))+
                          '|'+IntToStr(ter)+';'+IntToStr(maxld)+'"');
        MonList.Insert(0, 'mo[mx++]="'+FormatDateTime('dd.mm.yy',SDToTime(ltag))+
                          '|'+IntToStr(mer)+'"');
						  
// save data files for sonnenertrag.eu (see wiki)
        if json and (htmlfile<>'') then begin  {für sonnenertrag.eu}
          try
            MonList.SaveToFile(ExtractFilePath(htmlfile)+monsjs);
            MonList.Clear;    {Anzahl Tage in daysjs reduzieren}
            if ProtHTM.Count>31 then
                 for x:=0 to 30 do MonList.Add(ProtHTM[x])    {letzte 31 Tage}
                                else MonList.Assign(ProtHTM); {alles}
            MonList.SaveToFile(ExtractFilePath(htmlfile)+daysjs);
          except
            ErrorMsg:='Could not save '+monsjs;
          end;
          ProtHTM.Clear;
        end;
{min_day.js: Intervallmesswerte
m[mi++]="Datum Uhrzeit|AC Leistung;
DC Leistung String 1;DC Leistung String 2;DC Leistung String 3;AC Tagesertrag;
DC Spannung String 1;DC Spannung String 2;DC Spannung String 3;WR Temperatur"

- Leistungen in ganzen W
- DC Spannungen in ganzen V --> Round(spg/10)
- Ertrag in ganzen Wh
- Temperatur in ganzen °C

 m[mi++]="15.02.09 17:40:00|0;6;5200;202;21"}
        ErrorMsg:='Error during create min_day.js.';
        for x:=OutList.Count-1 downto 0 do begin
          SplitList.DelimitedText:=OutList[x];
          if SplitList[0]<>FormatDateTime('yyyy-mm-dd', now) then break;
          ltag:=SplitList[1];
          if (ltag[5]='0') or (ltag[5]='5') then begin
            farbe:='m[mi++]="'+
                  FormatDateTime('dd.mm.yy', SDToTime(SplitList[0]))+' '+
                  FormatDateTime('hh:nn:ss', TToTT(ltag))+'|'+
                  SplitList[2]+';';              {AC Leistung}
            case StrToInt(numstr) of             {DC Leistungen Strings}
              1: farbe:=farbe+IntToStr(Round(StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000))+';';
              2: farbe:=farbe+IntToStr(Round(StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000))+';'+
                            IntToStr(Round(StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000))+';';
              3: farbe:=farbe+IntToStr(Round(StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000))+';'+
                            IntToStr(Round(StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000))+';'+
                            IntToStr(Round(StrToInt(SplitList[9])*StrToInt(SplitList[10])/10000))+';';
            end;
            farbe:=farbe+SplitList[3]+';';         {AC Ertrag}
            case StrToInt(numstr) of             {DC Spannungen Strings}
              1: farbe:=farbe+IntToStr(Round(StrToInt(SplitList[5])/10))+';';
              2: farbe:=farbe+IntToStr(Round(StrToInt(SplitList[5])/10))+';'+
                            IntToStr(Round(StrToInt(SplitList[7])/10))+';';
              3: farbe:=farbe+IntToStr(Round(StrToInt(SplitList[5])/10))+';'+
                            IntToStr(Round(StrToInt(SplitList[7])/10))+';'+
                            IntToStr(Round(StrToInt(SplitList[9])/10))+';';
            end;
            ProtHTM.Add(farbe+SplitList[4]+'"');  {und noch Temperatur}
          end;
        end;
        if json and (htmlfile<>'') and (ProtHTM.Count>0) then begin
          try        {min_day.js speichern}
            ProtHTM.SaveToFile(ExtractFilePath(htmlfile)+histjs);
          except
            ErrorMsg:='Could not save '+histjs;
          end;
        end;
        ProtHTM.Clear;

// Read HTML file, write table into it and write back
        if offs>0 then begin    {HTML Protokoll schreiben}
          ErrorMsg:='Load HTML file not successful';
          try
            ProtHTM.LoadFromFile(htmlfile);
          except
            ShowException(Exception.Create(ErrorMsg));
            terminate;   {nichts zum Laden gefunden}
            exit;
          end;
        end;
        if ProtHTM.Count>(offs+33) then begin  {Tabelle mit Defaultwerten schreiben}
          s:=' <table style="border-width:0;width:90%;margin-left:auto;margin-right:auto;">';
          ProtHTM[offs+ 0]:=s+'  <!-- cpvausw'+version+'-->';
          ProtHTM[offs+ 1]:=' <colgroup><col style="width:45%;"><col style="width:10%;"><col style="width:45%;"></colgroup>';
          ProtHTM[offs+ 2]:='   <tr><td style="font-size:1.5em;">Wechselrichter</td><td></td>';
          ProtHTM[offs+ 3]:='       <td style="font-size:1.5em;">'+WRNummer+
                            '</td></tr>';
          ProtHTM[offs+ 4]:='   <tr><td style="height:15px;"><hr></td><td></td>';
          ProtHTM[offs+ 5]:='       <td style="height:15px;"><hr></td></tr>';
          ProtHTM[offs+ 6]:='   <tr><td>Projektierte Peakleistung:</td><td></td>';
          ProtHTM[offs+ 7]:='       <td>'+KorrFloat(pla/1000)+
                            ' kW<sub>p</sub></td></tr>';
          if maxlei<pla then farbe:=outgt else farbe:=intgt; {Farben einstellen}
          ProtHTM[offs+ 8]:='   <tr><td>Erreichte Peakleistung:</td><td></td>';
          ProtHTM[offs+ 9]:='       '+farbe+KorrFloat(maxlei/1000)+
                            ' kW<sub>p</sub></td></tr>';
          ProtHTM[offs+10]:='   <tr><td>Projektierter Spezifischer Jahresertrag:</td><td></td>';
          ProtHTM[offs+11]:='       <td>'+KorrFloat(StrToFloat(spezsoll))+
                            ' kWh/kW<sub>p</sub></td></tr>';
          if (ger/pla/anzt*anzts)<StrToFloat(spezsoll) then farbe:=outgt
                                                       else farbe:=intgt;
          ProtHTM[offs+12]:='   <tr><td>Erreichter Spezifischer Jahresertrag:</td><td></td>';
          ProtHTM[offs+13]:='       '+farbe+
                            KorrFloat(ger/pla/anzt*anzts)+
                            ' kWh/kW<sub>p</sub></td></tr>';
          ProtHTM[offs+14]:='   <tr><td>Gesamtertrag bisher:</td><td></td>';
          ProtHTM[offs+15]:='       <td><b>'+KorrFloat(ger/1000)+
                            ' kWh</b></td></tr>';
          ProtHTM[offs+16]:='   <tr><td>Ertrag im letzten Monat:</td><td></td>';
          ProtHTM[offs+17]:='       <td>'+KorrFloat(lastmer/1000)+
                            ' kWh ('+KorrFloat(lastmer/pla)+
                            ' kWh/kW<sub>p</sub>)</td></tr>';
          ProtHTM[offs+18]:='   <tr><td>Ertrag im aktuellen Monat:</td><td></td>';
          ProtHTM[offs+19]:='       <td><b>'+KorrFloat(mer/1000)+
                            ' kWh</b></td></tr>';
          ProtHTM[offs+20]:='   <tr><td>Bisher h&ouml;chster Tagesertrag:</td><td></td>';
          ProtHTM[offs+21]:='       <td>'+KorrFloat(maxert/1000)+
                            ' kWh ('+KorrFloat(maxert/pla)+
                            ' kWh/kW<sub>p</sub>)</td></tr>';
          ProtHTM[offs+22]:='   <tr><td>Durchschnittlicher Tagesertrag:</td><td></td>';
          ProtHTM[offs+23]:='       <td>'+KorrFloat(ger/anzt/1000)+
                            ' kWh ('+KorrFloat(ger/anzt/pla)+
                            ' kWh/kW<sub>p</sub>)</td></tr>';
          if ter<(ger/anzt) then farbe:=outgt else farbe:=intgt;
          ProtHTM[offs+24]:='   <tr><td>Tagesertrag vom '+
                            FormatDateTime('dd.mm.yyyy (hh:nn',
                            SDToTime(copy(OutList[OutList.Count-1],1,10))+
                            TToTT(copy(OutList[OutList.Count-1],12,5)))+
                            'h):</td><td></td>';
          ProtHTM[offs+25]:='       '+farbe+'<b>'+
                            KorrFloat(ter/1000)+
                            ' kWh</b></td></tr>';
          ProtHTM[offs+26]:=' </table>';
          ProtHTM.SaveToFile(htmlfile);
        end;
      end;     {nichts tun, wenn keine Daten gefunden}
    end;
  finally                {aufräumen}
    FileList.Free;
    InList.Free;
    SplitList.Free;
    OutList.Free;
    DayList.Free;
    ProtHTM.Free;
    MonList.Free;
  end;

// stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;   {add help code and version here}
begin
  Writeln(ExeName,' ', version);
  writeln(inidat+' with correct configuration data has to be available at the same location');
  writeln('Usage:             ',ExeName,'{ -h}');
  writeln('Usage as CRON job: ',ExeName);
end;

var
  Application: TMyApplication;

{$R *.res}

begin
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

