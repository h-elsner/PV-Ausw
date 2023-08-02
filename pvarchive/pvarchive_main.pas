{*******************************************************}
{                                                       }
{       Danfoss TripleLynx Pro WR Datenauswertung       }
{                                                       }
{       2020-02-29              Helmut Elsner           }
{                                                       }
{       Compiler: FPC 3.0.4     /    Lazarus 2.0.6      }
{                                                       }
{*******************************************************}


{Um die mittlerweile sehr vielen Einzeldatein vom Wechselrichter ohne
 Datenverlust zusammenzufassen, wurde dieses kleine Tool entwickelt.
 Es liest alle Einzeldateien ein und schreibt die Daten in eine CSV-Datei,
 die den ganzen Monat enthält.

 Die so archivierten Daten enthalten immer noch alle Einzeldatensätze,
 können aber nun schneller eingelesen werden und übersichtlicher ist es auch.

Historie:
 2020-02-29 Version 1.0 entwickelt und getestet.
 2021-01-03 Test und Update LINUX
 2023-03-11 Code review, english GUI, Konsistenzprüfungen eingeführt

}



unit PVarchive_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  XMLPropStorage, Buttons, FileUtil, lclintf, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnArch: TBitBtn;
    btnProt: TBitBtn;
    btnClose: TBitBtn;
    deArchDir: TDirectoryEdit;
    ebMask: TEditButton;
    ImageList1: TImageList;
    memProt: TMemo;
    SaveDialog1: TSaveDialog;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnArchClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnProtClick(Sender: TObject);
    procedure deArchDirDblClick(Sender: TObject);
    procedure ebMaskButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure memProtMouseWheelDown(Sender: TObject; Shift: TShiftState;
                                    MousePos: TPoint; var Handled: Boolean);
    procedure memProtMouseWheelUp(Sender: TObject; Shift: TShiftState;
                                    MousePos: TPoint; var Handled: Boolean);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{$I PVarchive_dt.inc}
{ $I PVarchive_en.inc}

const
  arcdir='arc';
  fn_prarc='PVarchive_';
  extcsv='.csv';
  tab1=' ';
  firstDay='01';
  wldc='*';

  wrdefstart='[wr_def_start]';
  wrdefend='[wr_def_end]';
  wrstart='[wr]';
  wrend='[wr_ende]';
  hdID='INT';                                          {ID für Spaltenüberschrift}
  fnsep='-';
  frmYM='YYYYMM';

procedure TForm1.FormCreate(Sender: TObject);          {Initialisieren}
begin
  Caption:=capForm;
  btnArch.Caption:=capArch;
  btnArch.Hint:=hntArch;
  btnProt.Caption:=capProt;
  btnProt.Hint:=hntProt;
  btnClose.Caption:=capClose;
  btnClose.Hint:=hntClose;
  deArchDir.Hint:=hntArchDir;
  ebMask.Hint:=hntMask;
  memProt.Lines.Clear;                                {Protokoll löschen}
end;

procedure TForm1.memProtMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    memProt.Font.Size:=memProt.Font.Size-1;
end;

procedure TForm1.memProtMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    memProt.Font.Size:=memProt.Font.Size+1;
end;

procedure TForm1.ebMaskButtonClick(Sender: TObject);   {Dateien suchen zum Test, Klick auf Mask ditor}
var filelist: TStringList;
begin
  if (deArchDir.Directory<>'') and
     (ebMask.Text<>'') and
      DirectoryExists(deArchDir.Directory) then begin
    filelist:=FindAllFiles(deArchDir.Directory, ebMask.Text+wldc, false);
    try
      memProt.Lines.Clear;
      memProt.Lines.Add(Format('%d'+resFilesFound, [filelist.Count]));
    finally
      filelist.Free;
    end;
  end else begin
    memProt.Lines.Add(errSettings);
    memProt.Lines.Add(errFileName);
  end;
  memProt.Lines.Add('');
end;

procedure TForm1.deArchDirDblClick(Sender: TObject);   {Goto archive dir}
begin
  OpenDocument(deArchDir.Directory);
end;

procedure TForm1.btnProtClick(Sender: TObject);        {Protokoll speichern}
begin
  if memProt.Lines.Count>0 then begin
    SaveDialog1.FileName:=IncludeTrailingPathDelimiter(deArchDir.Directory)+
                          Caption+fn_proto;
    if SaveDialog1.Execute then
       memProt.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;

function GetMonFile(fn: string): string;               {Monat aus dem Dateinamen in Form YYYYMM}
var
  f: string;

begin
  result:='';                                          {Empty for not useable}
  f:=ExtractFileName(fn);
  if f<>'' then begin
    f:=f.Split([fnsep])[1];
    if length(f)>5 then
       result:='20'+copy(f, 1, 4);                     {Century added}
    if result=FormatDateTime(frmYM, now) then
       result:='';                                     {nicht den aktuellen Monat auswerten}
  end;
end;

function GetDayFile(fn: string): string;               {Tagesnummer aus Dateinamen in Form DD}
var
  f: string;

begin
  result:='';                                          {Empty for not useable}
  f:=ExtractFileName(fn);
  if f<>'' then begin
    f:=f.Split([fnsep])[1];
    if length(f)>5 then
      result:=copy(f, 5, 2);
  end;
end;

function TestGap(fn, last: string): boolean;          {Prüft, ob Tage aufeinander folgen}
var
  d1, d2: integer;

begin
  result:=false;
  d1:=StrToIntDef(last, 0);
  d2:=StrToIntDef(GetDayFile(fn), 0);
  if (d1=0) or (d2=0) or ((d2-d1)>1) then
    result:=true;                                      {eine Lücke entdeckt}
end;

procedure Header(lst: TStringList; m: string);         {Metadaten und Überschrift für Danfoss WR}
begin
  lst.Add(m+'01;000000;webserver;WWS4a;+0100');        {Jahr/Monat einsetzen}
  lst.Add(wrdefstart);
  lst.Add('018202G221;Inverter Type;WWS4a;018202G221');
  lst.Add(wrdefend);
  lst.Add(wrstart);
  lst.Add('INTERVAL;TIMESTAMP;SERIAL;P_AC;E_DAY;T_WR;U_AC;U_AC_1;U_AC_2;U_AC_3;I_AC;F_AC;U_DC_1;I_DC_1;U_DC_2;I_DC_2;U_DC_3;I_DC_3;S;E_WR;M_WR;I_AC_1;I_AC_2;I_AC_3;P_AC_1;P_AC_2;P_AC_3;F_AC_1;F_AC_2;F_AC_3;R_DC;PC;PCS;PCS_LL;COS_PHI;COS_PHI_LL;S_COS_PHI;Current_Day_Energy;current_Day_Offset;ccEnergyOfDay_WithoutOffset');
end;

procedure TForm1.btnArchClick(Sender: TObject);        {Archivieren ausführen}
var
  i, k, dsges, fges: integer;
  filelist, inlist, outlist: TStringList;
  ad, mon, mon1, day, oldday: string;
  lck: boolean;                                        {Monat enthält Lücken}

  procedure CloseSave(m: string);
  var
    s: string;

  begin
    if day=firstDay then begin                         {Nur wenn der Monat mit dem 01. beginnt}
      if m<>'' then begin
        s:=ad+fn_prarc+m+extcsv;                       {Name der aktuellen Archivdatei}
        Outlist.Add(wrend);                            {Datei schließen...}
        outlist.SaveToFile(s);                         {Vorherigen Monat abschließen und speichern}
        inc(fges);                                     {Archivdateien zählen}
        dsges:=dsges+outlist.Count-7;                  {Datensätze zählen}
        s:=ExtractFileName(s);
        memProt.Lines.Add(s+resWith+IntToStr(outlist.Count-7)+resDatasets+resSaved);
        Application.ProcessMessages;
        if lck then begin
          memProt.Lines.Add(s+errLck);
          memProt.Lines.Add('');
        end;
      end;
    end else begin
      memProt.Lines.Add(mon+errFirstday);
      memProt.Lines.Add('');
    end;
  end;

begin
  memProt.Lines.Clear;                                 {Protokoll löschen}
  mon:='';
  dsges:=0;
  fges:=0;
  lck:=false;
  ad:=deArchDir.Directory+IncludeTrailingPathDelimiter(arcdir);   {Name Archivverzeichnis}
  if not DirectoryExists(ad) then
    if CreateDir(ad) then                              {wenn noch nicht vorhanden Archvierungsverzeichnis anlegen}
      memProt.Lines.Add(ad+resCreated);

  if DirectoryExists(ad) then begin
    Screen.Cursor:=crHourGlass;
    inlist:=TStringList.Create;
    outlist:=TStringList.Create;
    try
      filelist:=FindAllFiles(deArchDir.Directory, ebMask.Text+wldc, false);  {LINUX +'*'}
      memProt.Lines.Add(IntToStr(filelist.Count)+resFilesFound);
      memProt.Lines.Add('');

      if filelist.Count>0 then begin
        filelist.Sort;
        mon:=GetMonFile(filelist[0]);                  {Jahr/Monat aus 1. Datei}
        day:=getDayFile(filelist[0]);                  {1. Tag in den Dateien}
        oldday:=day;

        if mon<>'' then begin
          Header(outlist, mon);                        {Metadaten und Header für 1. Archivdatei}
          for i:=0 to filelist.Count-1 do begin
            mon1:=GetMonFile(filelist[i]);
            if mon1<>mon then begin                    {Neuer Monat}
              CloseSave(mon);
              outlist.Clear;
              mon:=mon1;
              lck:=false;
              Header(outlist, mon);                    {Metadaten und Header für nächste Archivdatei}
              day:=getDayFile(filelist[i]);            {Tagesnummer 1. Tag für den neuen Monat}
            end;

            if TestGap(filelist[i], oldday) then
              lck:=true;
            oldday:=GetDayFile(filelist[i]);

            inlist.LoadFromFile(filelist[i]);          {Liste füllen}
            for k:=0 to inlist.Count-1 do begin
              if (length(inlist[k])>100) and
                 (pos(hdID, inlist[k])<1) then
                outlist.Add(inlist[k]);
            end;
          end;
          CloseSave(mon1);
          memProt.Lines.Add('');
          memProt.Lines.Add(IntToStr(fges)+resFiles+resWith+
                            IntToStr(dsges)+resDatasets+resIn+
                            ad+tab1+resCreated);
        end else begin
          memProt.Lines.Add(errFileName);
        end;

      end;
    finally
      filelist.Free;
      inlist.Free;
      outlist.Free;
      Screen.Cursor:=crDefault;
    end;
  end else
    memProt.Lines.Add(errArchDir);
end;

procedure TForm1.btnCloseClick(Sender: TObject);       {Programm beenden}
begin
  Close;
end;

end.

