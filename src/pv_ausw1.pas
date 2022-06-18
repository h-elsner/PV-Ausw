          {********************************************************}
          {                                                        }
          {       Danfoss TripleLynx Pro WR Datenauswertung        }
          {                                                        }
          {       Copyright (c) 2011-2022 Helmut Elsner            }
          {                                                        }
          {       Compiler: FPC 3.2.2   /    Lazarus 2.2.0         }
          {                                                        }
          { Pascal programmers tend to plan ahead, they think      }
          { before they type. We type a lot because of Pascal      }
          { verboseness, but usually our code is right from the    }
          { start. We end up typing less because we fix less bugs. }
          {           [Jorge Aldo G. de F. Junior]                 }
          {********************************************************}


unit PV_Ausw1;

{Der TripleLynx Pro Wechselrichter von Danfoss zeichnet die Daten der PV-Anlage
 auf und stellt diese über FTP-Push auf einem als 'Datawarehouse' bezeichneten
 Server ab.
 Der Dateiname beginnt mit FTP-Userkennung+'-'+YYMMDD, dann folgt eine 
 Nummer (mit Zeitstempel) und er hat keine Extension.
 
 Das Programm dient dazu, diese Dateien auszuwerten und grafisch darzustellen,
 ähnlich wie es die Oberfläche des Webservers bietet, nur besser.
 ------------------------------------------------------------------------------
 Zusätzliche Packages werden benötigt:
  - Indy 10 lazarus:       http://www.indyproject.org/Sockets/fpc/index.de.aspx
 ------------------------------------------------------------------------------
  FTP RFC's (959 Updated by: 2228, 2640, 2773, 3659, 5797):
    http://tools.ietf.org/html/rfc959  FILE TRANSFER PROTOCOL (FTP)

    http://www.delphi-treff.de/tutorials/netzwerk-und-internet/indy/ftp-tidftp/
    http://tutorialsto.com/index.php/software/delphi/tidftp-using-indy-and-39-s-ftp-agreement-to-achieve-control.html
    http://www.kumanov.com/docs/prog/indy/
 ------------------------------------------------------------------------------
 Danfoss FW-Updates:
 http://www.danfoss.com/BusinessAreas/Solar+Energy/Downloads/TLX+Software.htm

 Rohdatenausgabeformat (rawdata.csv) und Backup file:

  Feld	Inhalt        Maßeinheit
  -----+-------------+-------------
   0	Datum         [JJJJ-MM-TT]
   1	Zeit          [hh:mm]
   2	Leistung      [W]
   3	Energie       [Wh]
   4    Temperatur WR [°C]
   5	Spannung 1    [/10 --> V]
   6	Strom 1       [mA]
   7	Spannung 2    [/10 --> V]
   8	Strom 2       [mA]
   9	Spannung 3    [/10 --> V]
  10	Strom 3       [mA]
  11    Netzspannung  [V]
  12    Netzfrequenz  [/100 --> Hz]
  13    Meßintervall  [sec]
  14    Isolationswiderstand [kOhm]
  ------------------------------------------------------------------------------

  Archiveausgabeformat (arch_days_hist.csv):

  Feld	Inhalt        Maßeinheit
  -----+-------------+-------------
   0	Datum         [JJJJ-MM-TT]
   1	Peak-Leistung [W]
   2	Ertrag/Tag    [Wh]
   3    Beginn        [hh:mm]
   4    Ende          [hh:mm]
  ------------------------------------------------------------------------------

  Historie:
  2011-10-20 V0.1 Erste lauffähige Version. Grundfuktionen verfügbar.
                  Beschriftung x-Achse fehlt oder falsch. Statistik-
                  funktionen nur halb realisiert.
  2011-11-04 V0.2 Strom/Spannungs-Auswertung, Beginn-/Endezeiten Statistik
  2011-11-13 V0.3 HTML-Protokollausgabe und Basisdatenseite
  2011-11-18 V0.4 Facelift, Histogramme beschleunigt (QuickSort)
  2011-11-21 V1.0 Erste Freigabe Version
  2011-12-19 V1.1 Inputdateien für sonnenertrag.eu anlegen (Solarlog Standard)
  2011-12-29 V1.2 Leistung per String anzeigen,
                  Start/Ende: 12:00h statt 00:00h anzeigen, ist logischer
  2012-01-02 V2.0 FTP-Client LNET zum Schreiben der HTML-Ausgabe und der
                  sonnenertrag-Dateien auf die Homepage. Download bricht ab.
  2012-01-16 V2.1 Restorefunktion für Tagesdateien aus dem Datawarehouse
  2012-01-28 V2.2 LNET entfernt; FTP-Download mit INDY 10 Komponente
  2012-01-29 V2.3 String-Erkennung verbessert. Parameter eingeführt:
                  -s  --stop       Programm beendet sich nach Auswertung
                  -n  --noftp      Programmlauf ohne FTP Zugriff
                  -b  --basisdaten Programmstart mit Basisdateneinstellung
                  -f  --ftpzugang  Programmstart mit FTP-Zugangsdateneinstellung.
                  Verzeichnis für FTP korrigiert. Bei Test Exception angezeigt.
  2012-01-31 V2.4 Sollwerte für Monat und Jahr. Größe des Programms merken.
  2012-03-09 V2.5 Ertrag und Leistung mit Gesamt + Strings
  2012-04-15 V2.6 Facelift, Tools, --ftpzugang entfernt und --basisdaten
                  durch --optionen ersetzt.
  2012-04-22 V2.7 Analyse 70%-Regelung unter Tools. Dazu Meßintervall in
                  Rawdata und Backup aufgenommen. Alte Backups funktionieren
                  nicht mehr für die Analyse und Wiederherstellen eines Tages.
  2012-05-04 V2.8 SynEdit für Protokoll mit highlight + Bugfixes + besseres
                  Verbindungsprotokoll.
                  FTP passiv mode eingeführt, hilft bei Routerproblemen.
                  Monatsertrag normiert, wie Jahresertrag.
  2012-05-21 V2.9 Archivierungsfunktion für Tagesdaten zu Laufzeitreduzierung
                  Glättung der Leistungsgesamtkurve, Codeoptimierung
  2012-06-28 V3.0 Tagesdiagramm für Temperatur, Click, um vom Balken zur
                  entsprechenden Auswertung zu springen.
  2012-07-03 V3.1 Spielwiese für spezielle Auswertungen.
  2012-07-13 V3.2 Intervallmesswerte min_day.js, Update base_vars.js,
                  Korrekturwert für Ertrag, ältere Daten schneller darstellen
  2012-08-18 V3.3 Spielwiese: Man kann in der Tagesansicht zwei unterschiedliche
                  Tage vergleichen.
                  Tagesansicht: Um Abweichungen bei unterschiedlich belegten
                  Strings erfassen zu können, kann man Leistung per String in
                  Prozent zur jeweiligen Maximalleistung des Strings darstellen
                  (Strings %).
                  Statistik: Spez. JE - Spezifischer Jahresertrag zum Vergleich,
                  um langfristig Degradation feststellen zu können.
                  Fadenkreuz per Kontextmenü (rechte Maustaste) einschaltbar -->
                  Anzeige der Meßpunktwerte in Fußzeile.
                  Monats- und Jahresausw.: Farbe der Ist-Linie abh. vom Soll.
  2012-09-06 V3.4 Spielwiese: P_DCx* per String eingeführt.
                  Beim Nachladen nur neue Dateien nehmen.
  2012-09-26 V3.5 Isolationswiderstand in Rohdaten und in der Spielwiese
                  verfügbar (R_DC). Wechselrichter-Fehler werden ins Protokoll
                  geschrieben. Dateien werden nicht mehr mehrmals im
                  Verbindungsprotokoll aufgelistet.
		          HTML-Protokoll überarbeitet (Ertrag aktueller Monat,
		          spez. Monatsertrag)
  2012-10-16 V3.6 Index Liste für Tage (Position in OutList) zwecks schnellerer
                  Suche.
                  'var sollMonth' in 'base_vars.js' aus Sollertrag Monate
                  berechnet (auf 100% hochgerechnet).
                  Parameter  -a  --sendall
                  Alle Tagesertragsdaten versenden statt der letzten 31 Tage.
  2012-11-28 V3.7 Importfilter für Kaco Powador
                  R_DC in Backup eingefügt. FTP Read Timeout admistrierbar.
				  Filelist ausblendbar.
  2013-04-23 V3.8 60% Regel zusätzlich.
  2013-05-06 V3.9 SMA-Spot Daten auswerten. Timer für automatisches Neuladen
                  einstellbar. Copy Chart to Clipboard eingefügt.
  2014-04-02 V3.0 Sommerzeit eleminieren auf der Spielwiese zum Vergleichen.
                  Einstellung aller Farben in den Diagrammen.
  2016-05-02 V4.1 Datenmigration von Danfoss Datawarehouse per download der Daten
  2017-06-01 V4.2 Dateneinlesen härten gegen WR-Abstürze mit sinnlosen Daten,
                  Quelltext überarbeitet
  2018-11-25 V4.3 Zoomfunktion und Pan geändert (Mausrad/linke Maustaste),
                  Klick auf Balken verbessert mit Chart-Tools.
  2019-05-10      Update HTML-Ausgabe Ertrag, Bilder besser skalierbar.
  2019-07-16 V4.4 Korrektur Ertragswerte als Summe der Strings.
  2019-09-03      Balken Klicken reaktiviert
  2020-02-28      Fehler bei Jahresstatistik Balken anklichen, wenn 3 Monate vom
                  Vorjahr noch angezeigt werden

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

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads, 
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  Buttons, StdCtrls, ComCtrls, EditBtn, XMLPropStorage, ExtCtrls, Spin, TAGraph,
  TASeries, TAChartAxis, types, TACustomSeries, TATransformations, TASources,
  TAIntervalSources, SynMemo, SynHighlighterAny, IdFTP, IdFTPCommon, dateutils,
  LCLIntf, LCLType, Grids, IdComponent, TAChartUtils, TATools, LazFileUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnEinlesen: TBitBtn;    {Daten einlesen}
    btnArch: TBitBtn;
    BitBtn11: TBitBtn;
    btnColorsReset: TBitBtn;
    btnColorsLoad: TBitBtn;
    btnColorsSave: TBitBtn;
    btnBackup: TBitBtn;
    btnClose: TBitBtn;    {Beenden}
    btnSave: TBitBtn;    {Stand speichern}
    btnLoad: TBitBtn;    {Stand laden}
    btnTag: TBitBtn;
    btnAbout: TBitBtn;
    btnSimu: TBitBtn;
    btnSaveProt: TBitBtn;
    btnTestFTP: TButton;
    btnTestHTML: TButton;
    Chart1:  TChart;
    Chart1BarSeries1:    TBarSeries;
    Chart1BarSeries2:    TBarSeries;
    Chart1ConstantLine1: TConstantLine;
    Chart1ConstantLine2: TConstantLine;
    Chart1LineSeries1:   TLineSeries;
    Chart1LineSeries2:   TLineSeries;
    Chart2: TChart;
    Chart2BarSeries1:    TBarSeries;
    Chart2ConstantLine1: TConstantLine;
    Chart2ConstantLine2: TConstantLine;
    Chart3: TChart;
    Chart3BarSeries1:    TBarSeries;
    Chart3BarSeries2:    TBarSeries;
    Chart3ConstantLine1: TConstantLine;
    Chart3ConstantLine2: TConstantLine;
    Chart4: TChart;
    Chart4LineSeries1:   TLineSeries;
    Chart4LineSeries2:   TLineSeries;
    Chart4LineSeries3:   TLineSeries;
    Chart4LineSeries4:   TLineSeries;
    Chart5: TChart;
    Chart5LineSeries1:   TLineSeries;
    Chart5LineSeries2:   TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations2: TChartAxisTransformations;
    ChartAxisTransformations2AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    ChartToolset2: TChartToolset;
    ChartToolset2DataPointClickTool1: TDataPointClickTool;
    ChartToolset3: TChartToolset;
    ChartToolset3DataPointClickTool1: TDataPointClickTool;
    CheckBox1:  TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox2:  TCheckBox;
    CheckBox3:  TCheckBox;
    CheckBox4:  TCheckBox;
    CheckBox5:  TCheckBox;
    CheckBox6:  TCheckBox;
    CheckBox7:  TCheckBox;
    CheckBox8:  TCheckBox;
    CheckBox9:  TCheckBox;
    ColorButton1: TColorButton;
    ColorButton10: TColorButton;
    ColorButton11: TColorButton;
    ColorButton12: TColorButton;
    ColorButton13: TColorButton;
    ColorButton14: TColorButton;
    ColorButton15: TColorButton;
    ColorButton16: TColorButton;
    ColorButton17: TColorButton;
    ColorButton18: TColorButton;
    ColorButton19: TColorButton;
    ColorButton2: TColorButton;
    ColorButton20: TColorButton;
    ColorButton3: TColorButton;
    ColorButton4: TColorButton;
    ColorButton5: TColorButton;
    ColorButton6: TColorButton;
    ColorButton7: TColorButton;
    ColorButton8: TColorButton;
    ColorButton9: TColorButton;
    ColorDialog1: TColorDialog;
    ComboBox1:  TComboBox;
    ComboBox2:  TComboBox;
    ComboBox3:  TComboBox;
    ComboBox4:  TComboBox;
    ComboBox5:  TComboBox;
    DateEdit1:  TDateEdit;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    DateTimeIntervalChartSource2: TDateTimeIntervalChartSource;
    DateTimeIntervalChartSource3: TDateTimeIntervalChartSource;
    deLocal: TDirectoryEdit;
    deServer: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    GroupBox12: TGroupBox;
    GroupBox13: TGroupBox;
    GroupBox14: TGroupBox;
    GroupBox15: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    IdFTP1:  TIdFTP;
    Label1:  TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2:  TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3:  TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4:  TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5:  TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label6:  TLabel;
    Label7:  TLabel;
    Label8:  TLabel;
    Label9:  TLabel;
    edFilter: TLabeledEdit;
    edHTMLserver: TLabeledEdit;
    edHTMLuser: TLabeledEdit;
    edHTMLpasw: TLabeledEdit;
    edHTMLfolder: TLabeledEdit;
    edHTMLfile: TLabeledEdit;
    edEuro: TLabeledEdit;
    edKorrFak: TLabeledEdit;
    LabeledEdit17: TLabeledEdit;
    LabeledEdit18: TLabeledEdit;
    LabeledEdit19: TLabeledEdit;
    edPeak:  TLabeledEdit;
    edSpezJE:  TLabeledEdit;
    edVerg:  TLabeledEdit;
    edMinPower:  TLabeledEdit;
    edServerName:  TLabeledEdit;
    edUser:  TLabeledEdit;
    edPasw:  TLabeledEdit;
    edFolder:  TLabeledEdit;
    ListChartSource1: TListChartSource;
    ListChartSource2: TListChartSource;
    MenuItem1:    TMenuItem;
    MenuItem2:    TMenuItem;
    MenuItem3:    TMenuItem;
    MenuItem4:    TMenuItem;
    MenuItem6:    TMenuItem;
    MenuItem7:    TMenuItem;
    MenuItem8:    TMenuItem;
    OpenDialog1:  TOpenDialog;
    pcMain: TPageControl;
    pcSettings: TPageControl;
    PopupMenu1:   TPopupMenu;
    PopupMenu2:   TPopupMenu;
    PopupMenu3:   TPopupMenu;
    ProgressBar1: TProgressBar;
    RadioGroup1:  TRadioGroup;
    RadioGroup2:  TRadioGroup;
    RadioGroup3:  TRadioGroup;
    RadioGroup4:  TRadioGroup;
    RadioGroup5:  TRadioGroup;
    RadioGroup6:  TRadioGroup;
    RadioGroup7:  TRadioGroup;
    SaveDialog1:  TSaveDialog;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    edHTMLzeile:    TSpinEdit;
    speArchPeriod:    TSpinEdit;
    spePeriod:    TSpinEdit;
    SpinEdit4:    TSpinEdit;
    StatusBar1:   TStatusBar;
    StringGrid1:  TStringGrid;
    SynAnySyn1: TSynAnySyn;
    SynMemo1:   TSynMemo;
    tsTag:  TTabSheet;
    tsSpiel: TTabSheet;
    tsColors: TTabSheet;
    tsMonat:  TTabSheet;
    tsJahr:  TTabSheet;
    tsStatistik:  TTabSheet;
    tsProt:  TTabSheet;
    tsSetOpt:  TTabSheet;
    tsBasics:  TTabSheet;
    tsOptions:  TTabSheet;
    tsFTP:  TTabSheet;
    Timer1:     TTimer;
    TrackBar1:  TTrackBar;
    TrackBar2:  TTrackBar;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnArchClick(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure btnColorsResetClick(Sender: TObject);
    procedure btnColorsLoadClick(Sender: TObject);
    procedure btnColorsSaveClick(Sender: TObject);
    procedure btnEinlesenClick(Sender: TObject);  {Daten neu einlesen}
    procedure btnBackupClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnTagClick(Sender: TObject);
    procedure btnSimuClick(Sender: TObject);
    procedure btnSaveProtClick(Sender: TObject);
    procedure btnTestFTPClick(Sender: TObject);
    procedure btnTestHTMLClick(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart2DblClick(Sender: TObject);
    procedure Chart3DblClick(Sender: TObject);
    procedure Chart4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChartToolset2DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure ChartToolset3DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure CheckBox10Change(Sender: TObject);
    procedure CheckBox11Change(Sender: TObject);
    procedure CheckBox12Change(Sender: TObject);
    procedure CheckBox13Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure CheckBox8Change(Sender: TObject);
    procedure CheckBox9Change(Sender: TObject);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure DateEdit1AcceptDate(Sender: TObject; var ADate: TDateTime;
      var AcceptDate: Boolean);
    procedure deLocalChange(Sender: TObject);   {Pfad zum Datawarehouse}
    procedure deServerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);    {Anwendung initialisieren und
                                               Standardwerte einlesen}
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure GroupBox1Click(Sender: TObject);
    procedure IdFTP1Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure Label51Click(Sender: TObject);
    procedure Label51MouseEnter(Sender: TObject);
    procedure Label51MouseLeave(Sender: TObject);
    procedure Label52Click(Sender: TObject);
    procedure Label52MouseEnter(Sender: TObject);
    procedure Label52MouseLeave(Sender: TObject);
    procedure Label53Click(Sender: TObject);
    procedure Label53MouseEnter(Sender: TObject);
    procedure Label53MouseLeave(Sender: TObject);
    procedure edHTMLserverChange(Sender: TObject);
    procedure edHTMLuserChange(Sender: TObject);
    procedure edHTMLpaswChange(Sender: TObject);
    procedure edHTMLfolderChange(Sender: TObject);
    procedure edHTMLfileChange(Sender: TObject);
    procedure edEuroChange(Sender: TObject);
    procedure edKorrFakChange(Sender: TObject);
    procedure LabeledEdit17Change(Sender: TObject);
    procedure edFilterChange(Sender: TObject);
    procedure edPeakChange(Sender: TObject);
    procedure edSpezJEChange(Sender: TObject);
    procedure edVergChange(Sender: TObject);
    procedure edMinPowerChange(Sender: TObject);
    procedure edServerNameChange(Sender: TObject);
    procedure edUserChange(Sender: TObject);
    procedure edPaswChange(Sender: TObject);
    procedure edFolderChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pcSettingsChange(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure edHTMLzeileChange(Sender: TObject);
    procedure speArchPeriodChange(Sender: TObject);
    procedure spePeriodChange(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);

  private
    OutList, DayList, PrevFileList, IDXList:  TStringList;

    procedure OutListLesen(crf: boolean);
    procedure Auswerten;
    procedure TagStat;
    procedure MonStat;
    procedure JahrStat;
    procedure Statistik;
    procedure ChangeTab;       {Anzeige aktualisieren, wenn TabSheet geändert}
    procedure FTPAnzeige(farbe: TColor);
    procedure FTPloginHP;
    procedure FTPloginDW;
    procedure FTPlogout;
    procedure FTPdownload;
    procedure ARCHrename;                          {lokal und FTP}
    function  SumGrid: integer;
    procedure StandSpeichern;
    procedure BackupSpeichern;
    procedure SpezAnalyse;
    function  GetOutListIDX(s: string): integer; {Zeilennummer ermitteln}
    function  ColorMatrix: string;                 {Farben speichern}
    procedure SetDColor(s: string);
  public
    {hier public einfügen}
  end;

const
  Version ='V4.4  07/2019';
  homepage='http://h-elsner.mooo.com';             {meine Homepage}
  meinname='Helmut Elsner';
  email   ='helmut.elsner@live.com';               {meine e-mail Adresse}
  datbeg  ='[wr]';
  datend  ='[wr_ende]';
  infbeg  ='[wr_def_start]';
  infend  ='[wr_def_end]';
  headerID='INTERVAL';
  sep     =';';                                    {Datenseperator für csv}
  datsep  ='-';                                    {Datenseperator Datum}
  dtr     =':  ';                                  {Datentrenner für die Optik}
  ziff    =['0'..'9'];
  nix     ='0'+sep;
  rawdat  ='rawdata.csv';
  frsw    =4960;     {Schwellwert für Frequenzbestimmung, kleinere
                      Werte werden ignoriert}
  spsw    =214;      {Schwellwert für Netzspannung, dito}
  Bar3d   =5;
  archID  ='arch_';  {archivierte Datawarehousedateien}
  archfn  ='days_hist.csv';  {Dateiname für Archivierte Daten}
  epg     =5;                {TabSheet active Page für Einstellungen}
  mwswt=20000000;            {Schwellwert für Anzeige in MWh}
  defint=300;        {5 min = 300 sec als default Meßintervall für Kaco und SMA}
  isodate ='YYYYMMDD';       {Datumsformat kurz}
  isoday  ='yyyy-mm-dd';     {Datumsformat ISO 8601}

{siehe http://wiki.sonnenertrag.eu/datenimport:voraussetzung:voraussetzung
 (SolarLog Standard)

 base_vars.js Hier vergleichen wir die Anlagenkonfiguration und die
              Anlagenleistung. Nur mit dieser Datei ist der Datenimport ab
              01.04.2011 möglich. Ausgenommen sind Portale wie Sunny Portal,
              die diese Überprüfung bereits intern durchführen.
WRInfo[0]=new Array("WR Kurzname","???",Angeschlossene AC Leistung,0,"WR Langname",Anzahl Strings,null,null,0,null,1,0,0,1000,null)

 days_hist.js In dieser Datei sind die Tagesdaten hinterlegt.
da[dx++]="Datum|Ertrag;Maximalleistung"
 Das Datum wird im Format TT.MM.JJ angegeben, der absolute Ertrag als
 ganze Zahl in Wh, die Maximalleistung als ganze Zahl in W.

 months.js    Diese Datei benötigen wir für den Monat.
mo[mx++]="Datum|Ertrag"
 Das Datum wird im Format TT.MM.JJ angegeben. Der absolute Ertrag in Wh,
 nicht in kWh!

 min_day.js bzw minJJMMTT.js
 http://wiki.sonnenertrag.eu/datenimport:voraussetzung:solarlog_min_day.js
 http://photonensammler.homedns.org/wiki/doku.php?id=solarlog_datenformat
 }

 basejs='base_vars.js';
 daysjs='days_hist.js';
 monsjs='months.js';
 histjs='min_day.js';     {Intervallmesswerte}
 
var
  Form1: TForm1;

{$I PV_Ausw_dt.inc}

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);   {Anwendung intialisieren}
var x, p: integer;
    SplitList: TStringList;
    s: string;
begin
  Self.DoubleBuffered:=True;
  SynMemo1.Lines.Clear;
  SynMemo1.Lines.Add(paramstr(0)+'  '+version);
  for x:=1 to ParamCount do SynMemo1.Lines.Add(paramstr(x));
  SynMemo1.Lines.Add('Start  : '+FormatDateTime(isoday+' hh:nn:ss', now));
  SynMemo1.Lines.Add('');
  ComboBox1.Text:='Day';
  ComboBox2.Text:='Month';
  ComboBox3.Text:='Year';
  DateEdit1.Enabled:=false;
  RadioGroup2.Tag:=0;               {zählt die FTP Downloads}
{Ressourcen zuweisen}
  btnEinlesen.Caption:=capDatLes;
  btnEinlesen.Hint   :=hntDatLes;
  btnClose.Caption:=capBeenden;
  btnClose.Hint   :=hntBeenden;
  btnSave.Caption:=capStandSp;
  btnSave.Hint   :=hntStandSp;
  btnLoad.Caption:=capStandLd;
  btnLoad.Hint   :=hntStandLd;
  btnTag.Caption:=capTagRest;
  btnAbout.Caption:=capInfo;
  btnAbout.Hint   :=hntInfo;
  btnSimu.Caption:=cap70Proz;
  btnSimu.Hint   :=hnt70Proz;
  btnSaveProt.Caption:=capFTPsv;
  btnSaveProt.Hint   :=capFTPprot+' '+capFTPsv;
  btnArch.Caption:=capArchiv;
  btnArch.Hint  :=hntArchiv;
  BitBtn11.Caption:=capSpAna;
  BitBtn11.Hint  :=hntSpAna;
  btnColorsReset.Caption:=capResetCl;
  btnColorsReset.Hint  :=hntResetCl;
  btnColorsLoad.Caption:=capBB13;
  btnColorsSave.Caption:=capBB14;
  btnTestFTP.Caption:=capTestDW;
  btnTestFTP.Hint   :=hntTestDW;
  btnTestHTML.Caption:=capTestHP;
  btnTestHTML.Hint   :=hntTestHP;
  SpeedButton1.Hint:=hntDBack;
  SpeedButton2.Hint:=hntDForw;
  SpeedButton3.Hint:=hntMForw;
  SpeedButton4.Hint:=hntMBack;
  SpeedButton5.Hint:=hntJForw;
  SpeedButton6.Hint:=hntJBack;
  Shape4.Hint    :=hntFTPStat;
  edFilter.Hint:=hntFilter;
  edPeak.Hint:=hntPeakLei;
  edSpezJE.Hint:=hntSJEPlan;
  edMinPower.EditLabel.Caption:=capPwrThrs;
  edServerName.EditLabel.Caption:=capFTPHost;
  edUser.EditLabel.Caption:=capAccount;
  edPasw.EditLabel.Caption:=capPasswd;
  edFolder.EditLabel.Caption:=capFTPVerz;
  edHTMLserver.EditLabel.Caption:=capFTPHost;
  edHTMLuser.EditLabel.Caption:=capAccount;
  edHTMLpasw.EditLabel.Caption:=capPasswd;
  edHTMLfolder.EditLabel.Caption:=capFTPVerz;
  edHTMLfile.EditLabel.Caption:=capHTMLdat;
  edEuro.EditLabel.Caption:=capCurrency;
  edKorrFak.EditLabel.Caption:=capLE16;
  edKorrFak.EditLabel.Hint:=hntLE16;
  tsTag.Caption:=rsTagesaw;
  tsMonat.Caption:=rsMonatsaw;
  tsJahr.Caption:=rsJahresaw;
  tsStatistik.Caption:=rsStat;
  tsSetOpt.Caption:=rsEinst;
  tsOptions.Caption:=rsOptionen;
  tsBasics.Caption:=rsBasisdat;
  tsFTP.Caption:=rsFTPac;
  tsSpiel.Caption:=capSheet10;
  tsColors.Caption:=capColor;
  Label1.Caption:=capLocDir;
  Label3.Hint :=hntMaxLei;
  Label9.Hint :=hntMaxLei;
  Label15.Hint:=hntMaxLei;
  Label37.Hint:=hntMaxLei;
  Label23.Hint:=hntHochrech;
  Label24.Hint:=hntHochrech;
  Label30.Hint:=hntHochrech;
  Label31.Hint:=hntHochrech;
  Label6.Hint :=hntErtragoE;
  Label7.Hint :=hntErtragoE;
  Label12.Hint:=hntErtragoE;
  Label13.Hint:=hntErtragoE;
  Label18.Hint:=hntErtragoE;
  Label19.Hint:=hntErtragoE;
  Label32.Hint:=hntErtragoE;
  Label33.Hint:=hntErtragoE;
  Label5.Hint :=hntAbsProd;
  Label11.Hint:=hntAbsProd;
  Label17.Hint:=hntAbsProd;
  Label35.Hint:=hntAbsProd;
  Label20.Hint:=hntNormiert;
  Label21.Hint:=hntNormiert;
  Label22.Hint:=hntNormiert;
  Label34.Hint:=hntNormiert;
  Label24.Hint:=hntSpezHoch;
  Label2.Hint :=hntPeakL;
  Label3.Hint :=hntPeakL;
  Label8.Hint :=hntPeakL;
  Label9.Hint :=hntPeakL;
  Label14.Hint:=hntPeakL;
  Label15.Hint:=hntPeakL;
  Label37.Hint:=hntPeakL;
  Label38.Hint:=hntPeakL;
  Label25.Caption:=capHTMLfile;
  Label26.Caption:=capLineNo;
  Label27.Caption:=capExact;
  Label28.Caption:=capFaster;
  Label29.Caption:=capTrack;
  Label29.Hint   :=hntTrack;
  TrackBar1.Hint :=hntTrack;
  Label40.Caption:=capDistri;
  Label43.Caption:=capDayArch;
  Label43.Hint   :=hntDayArch;
  Label44.Caption:=capSmooth;
  Label44.Hint   :=hntSmooth;
  TrackBar2.Hint :=hntSmooth;
  Label45.Caption:=capOhne;
  Label46.Caption:=capMax;
  Label49.Caption:=capLabel49;
  Label50.Caption:=capFTPrto;            {FTP Read Timeout}
  Label50.Hint   :=hntFTPrto;
  Label51.Caption:=capManual;
  edHTMLzeile.Hint :=hntZeiNr;
  speArchPeriod.Hint :=hntDayArch;
  spePeriod.Hint :=hntUpdInt;
  SpinEdit4.Hint :=hntFTPrto;
  GroupBox1.Caption:=capHead;
  GroupBox2.Caption:=capData;
  GroupBox3.Caption:=capProjData;
  GroupBox4.Caption:=capTuning;
  GroupBox5.Caption:=capFTPWR;
  GroupBox7.Caption:=rsTools;
  GroupBox10.Caption:=rsAllgemein;
  GroupBox11.Caption:=rsTagesaw;
  GroupBox12.Caption:=rsMonatsaw;
  GroupBox13.Caption:=rsJahresaw;
  GroupBox14.Caption:=rsStat;
  GroupBox15.Caption:=capSheet10;
  ColorButton1.Caption:=capCB1;          {Spielwiese: links}
  ColorButton2.Caption:=capCB2;          {Spielwiese: rechts}
  ColorButton3.Caption:=rsString+' 1';   {Tagesauswertung}
  ColorButton4.Caption:=rsString+' 2';
  ColorButton5.Caption:=rsString+' 3';
  ColorButton6.Caption:=rsGesamt;
  ColorButton7.Caption:=capCB7;
  ColorButton8.Caption:=capCB8;
  ColorButton9.Caption:=capCB9;
  ColorButton10.Caption:=capCB10;
  ColorButton11.Caption:=capCB11;
  ColorButton12.Caption:=capCB12;
  ColorButton13.Caption:=capCB13;
  ColorButton14.Caption:=capCB14;
  ColorButton15.Caption:=rsLei;
  ColorButton16.Caption:=rsErtrag;
  ColorButton17.Caption:=capCB13;
  ColorButton18.Caption:=rsHisto;
  ColorButton19.Caption:=capCB19;
  ColorButton20.Caption:=capCB20;
  StringGrid1.Hint:=hntStrg1;
  MenuItem3.Caption:=capECorr;
  edFilter.EditLabel.Caption:=capFilter;
  edPeak.EditLabel.Caption:=rsPeakL+ ' [W]';
  edSpezJE.EditLabel.Caption:=rsSpezJE+' [kWh/kWp]';
  edMinPower.Hint:=hntLblEdit5;
  Label2.Caption:=rsPeakL+':';
  Label8.Caption:=rsPeakL+':';
  Label14.Caption:=rsPeakL+':';
  Label38.Caption:=rsPeakL+':';
  Label4.Caption:=capEnergy;
  Label10.Caption:=capEnergy;
  Label16.Caption:=capEnergy;
  Label36.Caption:=capEnergy;
  Label23.Caption:=capSpezJE;
  Label31.Caption:=capSpezJE;
  Label54.Caption:=capUpdInt;
  CheckBox1.Caption:=capExtAusw;
  CheckBox1.Hint:=hntExtAusw;
  CheckBox2.Caption:=capFTPakt;
  CheckBox2.Hint:=hntFTPaktDW;
  CheckBox3.Caption:=capFTPcop;
  CheckBox4.Caption:=capFTPakt;
  CheckBox4.Hint:=hntFTPaktHP;
  CheckBox5.Caption:=capShowPW;
  CheckBox6.Caption:=capShowPW;
  CheckBox7.Caption:=capNormiert;
  CheckBox7.Hint:=hntNormiert;
  CheckBox10.Caption:=capNormiert;
  CheckBox10.Hint:=hntNormiert;
  CheckBox12.Caption:=capFList;
  CheckBox12.Hint:=hntFList;
  CheckBox13.Caption:=capNoReduc;
  CheckBox13.Hint:=hntNoReduc;
  CheckBox11.Caption:=capNoDST;
  CheckBox11.Hint:=hntNoDST;
  CheckBox8.Caption:=capShowStr;
  CheckBox8.Hint:=hntShowStr;
  CheckBox9.Caption:=capFTPpasv;
  CheckBox9.Hint:=hntFTPpasv;
  deLocal.Hint:=hntDiredit;
  deServer.Hint:=hntHTMLfile;
  Label51.Hint:=hntManual;
  Label52.Hint:=Homepage;
  Label53.Hint:=dupdate;

  RadioGroup1.Caption:=capAuswtg;
  RadioGroup1.Hint:=rsStatistikerz;
  RadioGroup1.Items.Clear;
  RadioGroup1.Items.Add(rsPeakL);
  RadioGroup1.Items.Add(rsStartE);
  RadioGroup1.Items.Add(rsEdauer);
  RadioGroup1.Items.Add(rsErtrag);
  RadioGroup1.Items.Add(rsSpYP);
  RadioGroup1.Items.Add(rsRelErtr);
  RadioGroup1.Items.Add(rsUAC);
  RadioGroup1.Items.Add(rsFrequ);
  RadioGroup1.Items.Add(rsWRTemp1);

  RadioGroup2.Caption:=capAnzeige;
  RadioGroup2.Hint:=hntAnzeige;
  RadioGroup2.Items.Clear;
  RadioGroup2.Items.Add(rsErtrag);
  RadioGroup2.Items.Add(rsLei);
  RadioGroup2.Items.Add('Strings %');
  RadioGroup2.Items.Add(rsSpg);
  RadioGroup2.Items.Add(rsStrom);
  RadioGroup2.Items.Add('Sweep');

  RadioGroup3.Items.Clear;
  RadioGroup3.Items.Add(rsTag);
  RadioGroup3.Items.Add(rsMonat);
  RadioGroup3.Items.Add(rsJahr);
  RadioGroup3.Items.Add(rsAlles);

  RadioGroup4.Caption:=capSpYl;
  RadioGroup5.Caption:=capSpYr;
  RadioGroup6.Caption:=capWRdata;
  RadioGroup6.Hint:=hntWRdata;
  RadioGroup7.Caption:=cap60prz;
  RadioGroup7.Hint:=hnt60prz;
  RadioGroup7.ItemIndex:=2;            {Default 70% Regel}
  GroupBox9.Caption:=capSpY;
  GroupBox8.Caption:=capSpX;
  Label47.Caption:=capSpSel;
  OutList:=TStringList.Create;
  DayList:=TStringList.Create;
  PrevFileList:=TStringList.Create;
  IDXList:=TStringList.Create;         {Position 1. Datenzeile in OutList}
  btnTag.Tag:=0;                      {Default: keine Wiederherstellung}
  ComboBox1.Color:=clDefault;
  ComboBox5.Visible:=false;
  ComboBox5.Hint:=hntSnddate;
  tsProt.Caption:=capFTPProt;
{Einstellungsdaten aus gespeicherten Basiswerten holen}
  deLocal.Directory:=XMLPropStorage1.StoredValue['DatawarehouseRoot'];
  deServer.FileName:=XMLPropStorage1.StoredValue['ProtHTMDat'];
  edHTMLzeile.Value:=StrToInt(XMLPropStorage1.StoredValue['ProtHTMZeiNr']);
  edFilter.Text:=XMLPropStorage1.StoredValue['FileFilter'];
  edPeak.Text:=XMLPropStorage1.StoredValue['PeakLeistungAnlage'];
  edSpezJE.Text:=XMLPropStorage1.StoredValue['SpezJErtragSoll'];
  edVerg.Text:=XMLPropStorage1.StoredValue['EEG_Verguetung'];
  edMinPower.Text:=XMLPropStorage1.StoredValue['SchwellwertLeistung'];
  edServerName.Text:=XMLPropStorage1.StoredValue['FTP_DWserver'];
  edUser.Text:=XMLPropStorage1.StoredValue['FTP_DWaccount'];
  edPasw.Text:=XMLPropStorage1.StoredValue['FTP_DWpsw'];
  edFolder.Text:=XMLPropStorage1.StoredValue['FTP_DWpath'];
  edHTMLserver.Text:=XMLPropStorage1.StoredValue['FTP_HPserver'];
  edHTMLuser.Text:=XMLPropStorage1.StoredValue['FTP_HPaccount'];
  edHTMLpasw.Text:=XMLPropStorage1.StoredValue['FTP_HPpsw'];
  edHTMLfolder.Text:=XMLPropStorage1.StoredValue['FTP_HPpath'];
  edHTMLfile.Text:=XMLPropStorage1.StoredValue['FTP_HTMLfile'];
  edEuro.Text:=XMLPropStorage1.StoredValue['CurrStr'];
  edKorrFak.Text:=XMLPropStorage1.StoredValue['KorrE'];
  CheckBox1.Checked:=StrToBool(XMLPropStorage1.StoredValue['JSDateienAnlegen']);
  CheckBox2.Checked:=StrToBool(XMLPropStorage1.StoredValue['FTP_DWakt']);
  CheckBox4.Checked:=StrToBool(XMLPropStorage1.StoredValue['FTP_HPakt']);
  CheckBox3.Checked:=StrToBool(XMLPropStorage1.StoredValue['FTP_single']);
  CheckBox7.Checked:=StrToBool(XMLPropStorage1.StoredValue['JNormiert']);
  CheckBox10.Checked:=StrToBool(XMLPropStorage1.StoredValue['MNormiert']);
  CheckBox12.Checked:=StrToBool(XMLPropStorage1.StoredValue['NoFileList']);
  CheckBox13.Checked:=StrToBool(XMLPropStorage1.StoredValue['NoReduction']);
  CheckBox11.Checked:=StrToBool(XMLPropStorage1.StoredValue['NoDST']);
  CheckBox8.Checked:=StrToBool(XMLPropStorage1.StoredValue['ShowStrings']);
  CheckBox9.Checked:=StrToBool(XMLPropStorage1.StoredValue['FTPpasv']);
  speArchPeriod.Value:=StrToInt(XMLPropStorage1.StoredValue['ToArchive']);
  spePeriod.Value:=StrToInt(XMLPropStorage1.StoredValue['AutoReload']);
  RadioGroup4.ItemIndex:=StrToInt(XMLPropStorage1.StoredValue['PGlY']);
  RadioGroup5.ItemIndex:=StrToInt(XMLPropStorage1.StoredValue['PGrY']);
  RadioGroup6.ItemIndex:=StrToInt(XMLPropStorage1.StoredValue['InverterType']);
  SpinEdit4.Value:=StrToInt(XMLPropStorage1.StoredValue['FTPReadTimeout']);
  btnTag.Visible:=(RadioGroup6.ItemIndex=0); {Tag wiederherstellen - nur Danfoss}
  IdFTP1.Passive:=CheckBox9.Checked;
  IdFTP1.ReadTimeout:=SpinEdit4.Value*1000;
  IdFTP1.ListenTimeout:=IdFTP1.ReadTimeout;
  edVerg.EditLabel.Caption:=rsEEGV+  ' ['+edEuro.Text+'/kWh]';
  edVerg.Hint:=hntEEGV;
  Label6.Caption :=Format(capEuro, [edEuro.Text]);
  Label12.Caption:=Format(capEuro, [edEuro.Text]);
  Label18.Caption:=Format(capEuro, [edEuro.Text]);
  Label33.Caption:=Format(capEuro, [edEuro.Text]);
  RadioGroup2.ItemIndex:=StrToInt(XMLPropStorage1.StoredValue['Tagesanzeige']);
  for x:=1 to 12 do begin                       {Monatsstrings füllen}
    s:=FormatDateTime('mmm', EncodeDate(2012, x, 1));
    StringGrid1.Columns[x-1].Title.Caption:=s;
    SynAnySyn1.Constants.Add(UpCase(s));        {Datum highlight}
  end;
  RadioGroup1.Tag:=0;                           {keine 70%-Analyse}
  SplitList:=TStringList.Create;
  SplitList.Delimiter:=sep;
  p:=0;
  try
    SplitList.DelimitedText:=XMLPropStorage1.StoredValue['Sollertrag'];
    if Splitlist.Count>11 then for x:=0 to 11 do StringGrid1.Cells[x,1]:= SplitList[x];
    SplitList.DelimitedText:=XMLPropStorage1.StoredValue['PeakString'];
    LabeledEdit17.Text:=SplitList[0];
    LabeledEdit18.Text:=SplitList[1];
    LabeledEdit19.Text:=SplitList[2];
    SetDColor(XMLPropStorage1.StoredValue['clMatrix']);  {Farben aus XML laden}
  finally
    FreeAndNil(SplitList);
  end;
  TrackBar1.Position:=StrToInt(XMLPropStorage1.StoredValue['Histogrammerzeugung']);
  TrackBar2.Position:=StrToInt(XMLPropStorage1.StoredValue['Smooth']);
  p:=StrToInt(XMLPropStorage1.StoredValue['LastEval']);
  if (Trackbar1.Position>3) or (p<5) then RadioGroup1.ItemIndex:=p
                                     else RadioGroup1.ItemIndex:=3; {Ertrag=default};
  StatusBar1.Panels[0].Text:=XMLPropStorage1.StoredValue['InverterName'];
  StatusBar1.Panels[1].Text:=XMLPropStorage1.StoredValue['InverterID'];
  StatusBar1.Panels[2].Text:=deLocal.Directory;
  Form1.Caption:=capForm+StatusBar1.Panels[1].Text;
  if CheckBox3.Checked then begin
    CheckBox6.Enabled:=false;
    edHTMLserver.Enabled:=false;
    edHTMLuser.Enabled:=false;
    edHTMLpasw.Enabled:=false;
  end else begin
    CheckBox6.Enabled:=true;
    edHTMLserver.Enabled:=true;
    edHTMLuser.Enabled:=true;
    edHTMLpasw.Enabled:=true;
  end;
  btnEinlesen.Enabled:=DirectoryExists(deLocal.Directory);
  btnSave.Enabled:=false;                       {Stand Speichern}
  btnBackup.Enabled:=false;                       {Backup}
  btnSimu.Enabled:=false;                       {70% Regel sperren}
  btnArch.Enabled:=false;                      {Archivieren sperren}
  BitBtn11.Enabled:=false;                      {Spez. Analyse sperren}
  RadioGroup3.Enabled:=false;
  btnLoad.Enabled:=FileExists((ExtractFilePath(Application.ExeName)+rawdat));
  pcSettings.ActivePageIndex:=StrToInt(XMLPropStorage1.StoredValue['LastOptSheet']);
  p:=StrToInt(XMLPropStorage1.StoredValue['LastTabSheet']);
  if Application.HasOption('o', 'options') then p:=epg;
  if Application.HasOption('m', 'month') then p:=1;
  pcMain.Tag:=p;                          {TabSheet merken}
  if deLocal.Directory='' then begin     {erst Einstellungen ausfüllen!}
    p:=epg;
    pcSettings.ActivePageIndex:=0;
  end else SaveDialog1.InitialDir:=deLocal.Directory;
  pcMain.ActivePageIndex:=p;
  ChangeTab;
  if p<>epg then begin                          {Auswertung automatisch starten}
    Timer1.Interval:=50;                        {relativ schnell starten, 50ms}
    Timer1.Enabled:=true;
  end;
end;

function TForm1.ColorMatrix: string;            {Farben in string speichern}
begin
  result:=
    ColorToString(ColorButton1.ButtonColor)+sep+
    ColorToString(ColorButton2.ButtonColor)+sep+
    ColorToString(ColorButton3.ButtonColor)+sep+
    ColorToString(ColorButton4.ButtonColor)+sep+
    ColorToString(ColorButton5.ButtonColor)+sep+
    ColorToString(ColorButton6.ButtonColor)+sep+
    ColorToString(ColorButton7.ButtonColor)+sep+
    ColorToString(ColorButton8.ButtonColor)+sep+
    ColorToString(ColorButton9.ButtonColor)+sep+
    ColorToString(ColorButton10.ButtonColor)+sep+
    ColorToString(ColorButton11.ButtonColor)+sep+
    ColorToString(ColorButton12.ButtonColor)+sep+
    ColorToString(ColorButton13.ButtonColor)+sep+
    ColorToString(ColorButton14.ButtonColor)+sep+
    ColorToString(ColorButton15.ButtonColor)+sep+
    ColorToString(ColorButton16.ButtonColor)+sep+
    ColorToString(ColorButton17.ButtonColor)+sep+
    ColorToString(ColorButton18.ButtonColor)+sep+
    ColorToString(ColorButton19.ButtonColor)+sep+
    ColorToString(ColorButton20.ButtonColor)+sep;
end;

procedure TForm1.SetDColor(s: string);           {Farbschema aus string laden}
var SplitList: TStringList;
begin
  SplitList:=TStringList.Create;
  SplitList.Delimiter:=sep;
  try
    SplitList.DelimitedText:=s;
    if Splitlist.Count>19 then begin  {verhindert Fehler, wenn noch keine Farbe geändert}
      ColorButton1.ButtonColor:=StringToColor(SplitList[0]); {Spielwiese}
      ColorButton2.ButtonColor:=StringToColor(SplitList[1]);
      ColorButton3.ButtonColor:=StringToColor(SplitList[2]); {Tagesauswertung}
      ColorButton4.ButtonColor:=StringToColor(SplitList[3]);
      ColorButton5.ButtonColor:=StringToColor(SplitList[4]);
      ColorButton6.ButtonColor:=StringToColor(SplitList[5]);
      ColorButton7.ButtonColor:=StringToColor(SplitList[6]);
      ColorButton8.ButtonColor:=StringToColor(SplitList[7]); {Monatsauswertung}
      ColorButton9.ButtonColor:=StringToColor(SplitList[8]); {Allgemein}
      ColorButton10.ButtonColor:=StringToColor(SplitList[9]);
      ColorButton11.ButtonColor:=StringToColor(SplitList[10]);
      ColorButton12.ButtonColor:=StringToColor(SplitList[11]);
      ColorButton13.ButtonColor:=StringToColor(SplitList[12]);
      ColorButton14.ButtonColor:=StringToColor(SplitList[13]);
      ColorButton15.ButtonColor:=StringToColor(SplitList[14]);
      ColorButton16.ButtonColor:=StringToColor(SplitList[15]);
      ColorButton17.ButtonColor:=StringToColor(SplitList[16]);
      ColorButton18.ButtonColor:=StringToColor(SplitList[17]);
      ColorButton19.ButtonColor:=StringToColor(SplitList[18]);
      ColorButton20.ButtonColor:=StringToColor(SplitList[19]);
    end;
  finally
    SplitList.Free;
  end;
end;

function MonToInt(s: string): integer; inline;   {Monatsnamen wieder in Zahlen}
var x: integer;
begin
  result:=0;
  for x:=1 to 12 do
    if Form1.StringGrid1.Columns[x-1].Title.Caption=s then begin
      result:=x;
      break;
    end;
end;

{Format Datenpunkt von ListChartSource1.DataPoints[Index]:
1|38.470374999999997|$FF0000|Jan
2|23.077000000000002|$F0CAA6|02   }

function NameOfDP(s: string): string;     {Label des Datenpunktes}
var SplitList: TStringList;
begin
  SplitList:=TStringList.Create;
  SplitList.Delimiter:='|';
  try
    SplitList.DelimitedText:=s;
    result:=SplitList[3];
  finally
    SplitList.Free;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);  {Timerevent starten Auswertung}
begin
  Timer1.Enabled:=false;
  if spePeriod.Value>0 then begin               {startet automatischen Download}
    SynMemo1.Clear;                             {verhindert Zumüllen}
    Timer1.Interval:=spePeriod.Value*60000;     {SpinEdit in Minuten}
    Timer1.Enabled:=true;
  end;
  FTPdownload;
  Auswerten;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);   {Histogrammerzeugung tunen}
begin
  XMLPropStorage1.StoredValue['Histogrammerzeugung']:=IntToStr(TrackBar1.Position);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);   {Glättungsfaktor}
begin
  XMLPropStorage1.StoredValue['Smooth']:=IntToStr(TrackBar2.Position);
end;

procedure TForm1.edFilterChange(Sender: TObject); {Dateifilter}
begin
  XMLPropStorage1.StoredValue['FileFilter']:=edFilter.Text;
end;

procedure TForm1.edPeakChange(Sender: TObject); {Peakleistung}
begin
  XMLPropStorage1.StoredValue['PeakLeistungAnlage']:=edPeak.Text;
end;

procedure TForm1.edSpezJEChange(Sender: TObject); {spezifischer Jahresertrag}
begin
  XMLPropStorage1.StoredValue['SpezJErtragSoll']:=edSpezJE.Text;
end;

procedure TForm1.edVergChange(Sender: TObject); {Einspeisevergütung}
begin
  XMLPropStorage1.StoredValue['EEG_Verguetung']:=edVerg.Text;
end;

procedure TForm1.edMinPowerChange(Sender: TObject); {Leistungsschwelle merken}
begin
  XMLPropStorage1.StoredValue['SchwellwertLeistung']:=edMinPower.Text;
end;

procedure TForm1.edServerNameChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['FTP_DWserver']:=edServerName.Text;
end;

procedure TForm1.edUserChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['FTP_DWaccount']:=edUser.Text;
end;

procedure TForm1.edPaswChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['FTP_DWpsw']:=edPasw.Text;
end;

procedure TForm1.edFolderChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['FTP_DWpath']:=edFolder.Text;
end;

{also   Chart1.SaveToBitmapFile(fn); aber dann wirklich Bitmap}

procedure TForm1.MenuItem1Click(Sender: TObject); {Als Bild speichern}
begin
  if OutList.Count>0 then begin               {nur wenn was angezeigt wird}
    SaveDialog1.Title:=titSave1;
    SaveDialog1.FilterIndex:=1;
    if RadioGroup1.Tag=1
      then SaveDialog1.FileName:=rsAnalyse+
               copy(RadioGroup7.Items[RadioGroup7.ItemIndex], 2, 2)+'_1.png'
      else SaveDialog1.FileName:='chart_'+FormatDateTime(isodate, now)+'_'+
                             IntToStr(pcMain.ActivePageIndex+1)+'.png';
    if SaveDialog1.Execute then begin
      Application.ProcessMessages;
      case pcMain.ActivePageIndex of
        0: Chart4.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
        1: Chart2.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
        2: Chart3.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
        3: Chart1.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);  {Statistik}
        4: Chart5.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);  {Spielwiese}
      end;
    end;
  end;
end;

{Manchmal stimmt der Ertrag nicht mit der Summer der Strings überein.
 Der Gesamtertrag bleibt auf einem Wert einfach hängen und wird nicht weiter
 aufaddiert. Dies ist zu erkennen, wenn der Monatsertrag hier von der Übersicht
 im WR abweicht. Dann wird im Tagesertrag irgendwo eine waagerechte Linie
 erscheinen. Dies wir hiermit korrigiert mit Summe Ertrag der Strings.
 E_DAY (4) = }

procedure TForm1.MenuItem3Click(Sender: TObject); {Ertragskorrektur Tag}
var i, k, l: integer;
    inlist, splitlist: TStringList;
    vstr, s: string;
    ertrag: double;
    b: char;
begin
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  b:=DefaultFormatSettings.DecimalSeparator;       {Dezimalseperator merken}
  try
    DefaultFormatSettings.DecimalSeparator:='.';   {Dezimalseperator neu setzen}
    splitlist.Delimiter:=sep;
    splitlist.StrictDelimiter:=true;               {Datum/Zeit komplett übernehmen}
    ertrag:=0;
    SynMemo1.Lines.Add('');
    SynMemo1.Lines.Add(rsKorrErtrag+ComboBox1.Text);
    vstr:=StringReplace(edFilter.Text, '*', '', [])+
          copy(StringReplace(ComboBox1.Text, '-', '', [rfReplaceAll]), 3, 6);
    if PrevFileList.Count>1 then begin             {überhaupt Dateien geladen}

      for i:=0 to PrevFileList.Count-1 do begin    {alle geladenen Dateien}
        if Pos(vstr, PrevFileList[i])>0 then begin {nur Dateien von dem Tag}
          inlist.LoadFromFile(PrevFileList[i]);    {Datei laden}
          for k:=0 to Inlist.Count-1 do begin
            splitlist.DelimitedText:=inlist[k];
            if (splitlist.Count>26) and            {nur Datenzeilen}
               (copy(splitlist[1], 1, 10)=ComboBox1.Text) then begin
              ertrag:=ertrag+                      {aus Leistung aufintegrieren}
                      (StrToInt(splitlist[3])*     {Leistung P_AC in Watt}
                       StrToInt(splitlist[0])/3600000);    {Intervall in sec}
              splitlist[4]:=FormatFloat('0.000', ertrag);  {Wert eintragen}
              s:=splitlist[0];
              for l:=1 to splitlist.Count-1 do     {Zeile wieder zusammenbauen}
                s:=s+sep+splitlist[l];
              inlist[k]:=s;                        {Zurückschreiben}
            end;
          end;
          inlist.SaveToFile(PrevFileList[i]);
          SynMemo1.Lines.Add(PrevFileList[i]+rsKorrDone);
        end;
      end;

    end;
  finally
    inlist.Free;
    splitlist.Free;
    DefaultFormatSettings.DecimalSeparator:=b;
  end;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);  {Copy chart to clipboard}
begin
  case pcMain.ActivePageIndex of
    0: Chart4.CopyToClipboardBitmap;
    1: Chart2.CopyToClipboardBitmap;
    2: Chart3.CopyToClipboardBitmap;
    3: Chart1.CopyToClipboardBitmap;               {Statistik}
    4: Chart5.CopyToClipboardBitmap;
  end;
end;

procedure TForm1.FTPAnzeige(farbe: TColor); {FTP Statusanzeige}
begin
  Shape4.Brush.Color:=farbe;
  Shape4.Refresh;
  Application.ProcessMessages;
end;

function CleanDir(s: string): string; inline;
                                 {Verzeichnis FTP-gerecht aufarbeiten}
begin
  result:=TrimFilename(s);                      {putzt die Pfadangabe}
  if s<>'' then begin
    result:=StringReplace(s, '\', '/', [rfReplaceAll]);
    if result[1]='/' then delete(result, 1, 1);
    if result[length(result)]<>'/' then
      result:=result+'/';
  end;
end;

{FTP Commands: https://en.wikipedia.org/wiki/List_of_FTP_commands}

procedure TForm1.FTPloginHP;                    {Homepage login}
begin
  IdFTP1.Host:=edHTMLserver.Text;
  IdFTP1.UserName:=edHTMLuser.Text;
  IdFTP1.Password:=edHTMLpasw.Text;
  IdFTP1.TransferType:=ftBinary;
  IdFTP1.Connect;
  if edHTMLfolder.Text<>'' then IdFTP1.ChangeDir(CleanDir(edHTMLfolder.Text));
end;

procedure TForm1.FTPloginDW;             {Datawarehouse login}
begin
  IdFTP1.Host:=edServerName.Text;
  IdFTP1.UserName:=edUser.Text;
  IdFTP1.Password:=edPasw.Text;
  IdFTP1.Connect;
  if edFolder.Text<>'' then IdFTP1.ChangeDir(CleanDir(edFolder.Text));
end;

procedure TForm1.FTPdownload;   {fehlende Dateien vom Datawarehouse downloaden}
var
  x: integer;
  FileList, TempList: TStringList;
  s: string;

begin
  if (CheckBox2.Checked)    and    {FTP download aktiviert}
     (edServerName.Text>'') and    {Host}
     (edUser.Text>'') and    {Account}
     (edPasw.Text>'') and    {Password}
     (deLocal.Directory>'') and
     (not Application.HasOption('n', 'noftp')) then begin
    SynMemo1.Lines.Add(edFilter.Text);     {FTP-Protokoll}
    SynMemo1.Lines.Add(deLocal.Directory);
    SynMemo1.Lines.Add(edServerName.Text);
    SynMemo1.Lines.Add(edUser.Text);
    SynMemo1.Lines.Add(edFolder.Text);
    SynMemo1.Lines.Add('');
    SynMemo1.Lines.Add('FTP download started at: '+FormatDateTime('hh:nn:ss', now));
    FTPAnzeige(clGreen);
    FileList:=TStringList.Create;
    TempList:=TStringList.Create;
    Screen.Cursor:=crHourGlass;
    try
      try
        FTPloginDW;
        IdFTP1.List(FileList, edFilter.Text, false); {false=ohne Details}
        SynMemo1.Lines.Add(IntToStr(FileList.Count)+' files on FTP server:');
        if (not CheckBox12.Checked) and
           (RadioGroup2.Tag=0) then begin  {verhindert, dass immer wieder
                                            alles aufgelistet wird}
          for x:=0 to FileList.Count-1 do SynMemo1.Lines.Add(FileList[x]);
          SynMemo1.Lines.Add('');
        end;
      except
        on e: Exception do begin
          StatusBar1.Panels[2].Text:='Exception from FTP command "list": '+e.Message;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          FTPAnzeige(clRed);
        end;
      end;
      try
        if FileList.Count>0 then begin   {nur, wenn überhaupt was gelistet wurde}
          TempList:=FindAllFiles(deLocal.Directory, edFilter.Text, false);
          TempList.Sort; {sortierte Liste der lokal vorhandenen Dateien für IndexOf}
          if (RadioGroup6.ItemIndex=3) and                        {nur für SMA-spot}
             (TempList.Count>0) then TempList.Delete(TempList.Count-1);
          SynMemo1.Lines.Add(IntToStr(TempList.Count)+' files in local directory:');
          if (not CheckBox12.Checked) and (RadioGroup2.Tag=0) then begin
            for x:=0 to TempList.Count-1 do SynMemo1.Lines.Add(TempList[x]);
            SynMemo1.Lines.Add('');
          end;
          for x:=0 to FileList.Count-1 do begin
            s:=IncludeTrailingPathDelimiter(deLocal.Directory)+FileList[x];
            Application.ProcessMessages;
            if TempList.IndexOf(s)<0 then begin  {fehlende Datei downloaden}
              IdFTP1.Get(FileList[x], s, false, IdFTP1.ResumeSupported);
              StatusBar1.Panels[2].Text:=Format(rsDownldd, [FileList[x]]);
              SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
            end;
          end;
        end;
        SynMemo1.Lines.Add('FTP download done at   : '+FormatDateTime('hh:nn:ss', now));
      except
        on e: Exception do begin
          StatusBar1.Panels[2].Text:='Exception from FTP command "get": '+
                                      e.Message+' at file '+s;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          FTPAnzeige(clRed);
        end;
      end;
      if RadioGroup2.Tag<100 then RadioGroup2.Tag:=RadioGroup2.Tag+1;
    finally
      FTPlogout;
      FileList.Free;
      TempList.Free;
      Screen.Cursor:=crDefault;
    end;
  end else
    SynMemo1.Lines.Add('FTP not allowed or missing parameter!');
end;

function TToTT(s: string): TDateTime; inline;  {Zeitstempel hh:mm in TDateTime}
begin
  result:=0;
  try
    if length(s)>4 then result:=EncodeTime(StrToInt(copy(s,1,2)),
                                           StrToInt(copy(s,4,2)), 0, 0);
  except
    result:=0;
  end;
end;

function SDToTime(s: string): TDateTime;     {Datum YYYY-MM-DD in TDateTime}
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

{Sommerzeit entfernen gilt nur für Europa und nur eingeschränkt von 1900-2099:
 Annahme: DaylightBias= 60 min, Umstellung um 02:00h bzw. um 03:00h
 http://delphiforfun.org/Programs/Math_Topics/DSTCalc.htm         For Europe :
 DST begins on the last Sunday in March (Day of month = 31 - (4 + 5*Y/4) mod 7)
 and ends on the last Sunday in October (Day of month = 31 - (1 + 5*Y/4) mod 7)}

function NixDST(tp: TDateTime): TDateTime;         {Sommerzeit entfernen}
var begDST, endDST: TDateTime;
    yy, dd: word;
begin
  result:=tp;
  DecodeDate(tp, yy, dd, dd);                      {Jahr ermitteln}
  dd:=31-(4+5*yy div 4) mod 7;
  begDST:=EncodeDateTime(yy, 03, dd, 2, 0, 0, 0);  {letzter Sonntag im März}
  dd:=31-(1+5*yy div 4) mod 7;
  endDST:=EncodeDateTime(yy, 10, dd, 3, 0, 0, 0);  {letzter Sonntag im Oktober}
  if (tp>begDST) and (tp<endDST) then result:=tp-(1/24);
end;

function MonToTxt(s: string): string; inline;      {Monat aus YYYY-MM in Namenskürzel umwandeln}
begin
  result:='';
  try
    if length(s)>6 then
      result:=FormatDateTime('mmm', SDToTime(s));
  except
  end;
end;

function KorrFloatStr(s: string): string;          {No matter if you use . or ,}
begin
  result:=StringReplace(s, '.', DefaultFormatSettings.DecimalSeparator, []);
  result:=StringReplace(result, ',', DefaultFormatSettings.DecimalSeparator, []);
end;

function GetKErt(er: integer): integer; inline;    {Korrekturwert für Ertrag in W}
begin
  try
    result:=er+round(er*StrToFloat(KorrFloatStr(Form1.edKorrFak.Text))/100);
  except
    result:=er;
  end;
end;

function GetEEVG: double; inline;                  {Vergütung}
begin
  try
    result:=StrToFloat(KorrFloatStr(Form1.edVerg.Text));
  except
    result:=0;
  end;
end;

function GetSJE: double; inline;                   {spezifischer Jahresertrag}
begin
  try
    result:=StrToFloat(KorrFloatStr(Form1.edSpezJE.Text));
  except
    result:=0;
  end;
end;

procedure TForm1.ARCHrename; {überflüssige Dateien im Datawarehouse umbenennen}
var x, y, ter, maxld, ertr, korre, ld: integer;
    FileList, TempList, SplitList, InList: TStringList;
    isftp, abbr, newf: boolean;
    dr, fn, lfn, ldat: string;
    beg, fbeg, fend: TDateTime;
begin
  if deLocal.Directory>'' then begin               {nur, wenn ein Verzeichnis angegeben}
    dr:=IncludeTrailingPathDelimiter(deLocal.Directory);
    isftp:=(CheckBox2.Checked)    and              {FTP download aktiviert}
           (edServerName.Text>'') and              {Host}
           (edUser.Text>'') and                    {Account}
           (edPasw.Text>'') and                    {Password}
           (not Application.HasOption('n', 'noftp'));   {FTP status ermitteln}
    fbeg:=1;                                       {Beginnzeit Produktion}
    fend:=0;                                       {Endezeit Produktion}
    ter:=0;                                        {Tagesertrag}
    maxld:=0;                                      {Peakleistung am Tag}
    korre:=0;                                      {Korrekturwert}
    ldat:='';                                      {letzter ausgewerteter Tag}
    lfn:='';                                       {letzter Dateiname}
    SynMemo1.Lines.Add('Archive up to '+
                       FormatDateTime(isoday, now-speArchPeriod.Value-1));
    if isftp then FTPAnzeige(clGreen);
    FileList :=TStringList.Create;                 {Dateilisten anlegen}
    TempList :=TStringList.Create;
    InList   :=TStringList.Create;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    Screen.Cursor:=crHourGlass;
    try
      try
        if isftp then begin     {einloggen und Dateiliste herunterladen}
          FTPloginDW;
          IdFTP1.List(FileList, edFilter.Text, false);
        end;
        FileList.Sort;
        TempList:=FindAllFiles(deLocal.Directory, edFilter.Text, false);
        TempList.Sort;          {sortierte Liste der lokal vorhandenen Dateien}
        DayList.Clear;          {archivierte Tagesdaten neu laden}
        PrevFileList.Clear;     {Merker rücksetzen}
        if FileExists(dr+archID+archfn) then
          DayList.LoadFromFile(dr+archID+archfn);
        for x:=0 to TempList.Count-1 do begin      {alle Dateien}
          Application.ProcessMessages;
          InList.LoadFromFile(TempList[x]);
          fn:=ExtractFileName(TempList[x]);
          newf:=true;                              {neue Datei geladen}
          abbr:=false;                             {(noch) nicht abbrechen}
          for y:=0 to InList.Count-1 do begin      {Datei auslesen}
            SplitList.DelimitedText:=InList[y];
            if (SplitList.Count>30) and (SplitList[0]<>headerID) then begin
              try
                ertr:=StrToInt(StringReplace(SplitList[5],'.','',[rfReplaceAll]));
                if ldat<>SplitList[1] then begin   {neuer Tag}
                  korre:=0;                        {Korrekturwert}
                  if ldat>'' then begin
                    if (ertr>0) and (InList.Count>10) then korre:=ertr;
                                                   {Korrekturwert erstellen}
                    if fend=0 then fend:=0.5;      {Zeitstempel schöner}
                    if fbeg=0 then fbeg:=0.5;
                    if fbeg=1 then fbeg:=fend;
                    DayList.Add(ldat+sep+IntToStr(maxld)+sep+IntToStr(ter)+sep+
                                FormatdateTime('hh:nn', fbeg)+sep+
                                FormatdateTime('hh:nn', fend));
                  end;
                  {Datum prüfen}
                  if (SDToTime(SplitList[1])>(now-speArchPeriod.Value-1)) and
                      newf then begin
                    abbr:=true;
                    break;                         {Schleife hier verlassen}
                  end;
                  ldat:=SplitList[1];
                  fbeg:=1;                               {Beginnzeit Produktion}
                  fend:=0;                               {Endezeit Produktion}
                  ter:=0;                                {Tagesertrag}
                  maxld:=0;                              {Peakleistung am Tag}
                  newf:=false;                           {mehrere Tage in Datei}
                end;
                if (ertr>0) and (ertr<korre) then korre:=0; {wegen 1. Danfossfehler}
                if ertr>=korre then ertr:=ertr-korre;    {1. und 2.}
                if ertr>ter then ter:=ertr;              {höchster Tagesertrag}
                ld:=StrToInt(SplitList[4]);              {Leistung}
                if ld>maxld then maxld:=ld;              {Peakleistung}
                if ld>StrToInt(edMinPower.Text) then begin
                  beg:=TToTT(copy(SplitList[2], 1, 5));  {Zeit}
                  if beg>fend then fend:=beg;
                  if beg<fbeg then fbeg:=beg;
                end;
              except              {Problem in Daten Zeile (Zeile verwerfen)}
                on e: Exception do begin
                  StatusBar1.Panels[2].Text:='Fehler bei Archivierung: '+e.Message;
                  SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
                end;
              end;
            end;                {Ende Datenzeile auswerten}
          end;                  {Ende alle Zeilen}
          if lfn>'' then begin  {Dateien umbenennen}
            if isftp and (FileList.IndexOf(lfn)>=0) then try
              IdFTP1.Rename(lfn, archID+lfn);
            except
              SynMemo1.Lines.Add('FTP Rename '+lfn+' not successful!');
            end;
            if RenameFile(dr+lfn, dr+archID+lfn) then begin  {und lokal}
              StatusBar1.Panels[2].Text:=lfn+' --> '+archID+lfn;
              SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
            end else SynMemo1.Lines.Add('Rename '+lfn+' not successful!');
          end;
          if abbr then break;   {keine weitere Datei bearbeiten}
          lfn:=fn;
        end;   {Ende alle Dateien durchsuchen}
        if DayList.Count>0 then begin
          DayList.SaveToFile(dr+archID+archfn); {Speichern}
          if isftp then begin
            FTPAnzeige(clBlue);
            try
              IdFTP1.Put(dr+archID+archfn, archID+archfn, false);
              SynMemo1.Lines.Add(archID+archfn+' uploaded');
              FTPlogout;
            except
              StatusBar1.Panels[2].Text:=rsFTPUploadnm;
              SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
              FTPAnzeige(clRed);
            end;
          end;
        end;
      except
        on e: Exception do begin
          StatusBar1.Panels[2].Text:='Exception during archive files: '+e.Message;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          if isftp then FTPAnzeige(clRed);
        end;
      end;
    finally
      if isftp then
        FTPlogout;
      FileList.Free;
      TempList.Free;
      InList.Free;
      SplitList.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.FTPlogout;                     {FTP ausloggen und beenden}
begin
  IdFTP1.Disconnect;
  FTPAnzeige(clDefault);
end;

procedure TForm1.pcMainChange(Sender: TObject);   {TabSheet wechseln}
begin
  ChangeTab;
end;

procedure TForm1.pcMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);                   {TabSheet merken}
begin
  XMLPropStorage1.StoredValue['LastTabSheet']:=IntToStr(pcMain.ActivePage.PageIndex);
  pcMain.Tag:=pcMain.ActivePage.PageIndex;  {TabSheet merken}
end;

procedure TForm1.pcSettingsChange(Sender: TObject);   {Einstellungen merken}
begin
  XMLPropStorage1.StoredValue['LastOptSheet']:=IntToStr(pcSettings.ActivePage.PageIndex);
end;

procedure TForm1.ChangeTab;                             {Hinweis anzeigen}
begin
  case pcMain.ActivePageIndex of
    0: StatusBar1.Panels[2].Text:=rsTagesaw;
    1: StatusBar1.Panels[2].Text:=rsMonatsaw;
    2: StatusBar1.Panels[2].Text:=rsJahresaw;
    3: StatusBar1.Panels[2].Text:=rsStatistikerz+'..';
    4: case pcSettings.ActivePageIndex of
         0: StatusBar1.Panels[2].Text:=rsUmgvar;
         1: StatusBar1.Panels[2].Text:=rsEinst+' / '+rsTools;
         2: StatusBar1.Panels[2].Text:=rsFTPZugein;
       end;
  end;
  StatusBar1.Panels[0].Text:=XMLPropStorage1.StoredValue['InverterName'];
  StatusBar1.Panels[1].Text:=XMLPropStorage1.StoredValue['InverterID'];
end;

procedure TForm1.deLocalChange(Sender: TObject); {Verzeichnis merken}
begin
  XMLPropStorage1.StoredValue['DatawarehouseRoot']:=deLocal.Directory;
  btnEinlesen.Enabled:=DirectoryExists(deLocal.Directory);
end;

procedure TForm1.deServerChange(Sender: TObject);  {Dateiname HTML Datei}
begin
  deServer.InitialDir:=deLocal.Directory;
  XMLPropStorage1.StoredValue['ProtHTMDat']:=deServer.FileName;
  edHTMLfile.Text:=ExtractFileName(deServer.FileName); {für FTP übernehmen}
  XMLPropStorage1.StoredValue['FTP_HTMLfile']:=edHTMLfile.Text;
end;

procedure TForm1.edHTMLzeileChange(Sender: TObject); {Zeilennummer merken}
begin
  XMLPropStorage1.StoredValue['ProtHTMZeiNr']:=IntToStr(edHTMLzeile.Value);
end;

procedure TForm1.speArchPeriodChange(Sender: TObject); {Tage bis zum Archivieren}
begin
  XMLPropStorage1.StoredValue['ToArchive']:=IntToStr(speArchPeriod.Value);
end;

procedure TForm1.spePeriodChange(Sender: TObject); {Timerintervall merken}
begin
  XMLPropStorage1.StoredValue['AutoReload']:=IntToStr(spePeriod.Value);
  if (spePeriod.Value>0) and (OutList.Count>0) then begin
    Timer1.Interval:=spePeriod.Value*60000;
    Timer1.Enabled:=true;
  end;
end;

procedure TForm1.SpinEdit4Change(Sender: TObject); {FTP Read Timeout}
begin
  IdFTP1.ReadTimeout:=SpinEdit4.Value*1000;
  IdFTP1.ListenTimeout:=IdFTP1.ReadTimeout;
  XMLPropStorage1.StoredValue['FTPReadTimeout']:=IntToStr(SpinEdit4.Value);
end;

function TForm1.SumGrid: integer;  {Summe aller Teiler berechnen für Quotienten}
var x: integer;
begin
  result:=0;                       {Teiler für 12 Monate aufsumnmieren}
  try
    for x:=0 to 11 do
      result:=result+StrToIntDef(StringGrid1.Cells[x,1], 0);
  except
    result:=0;
  end;
end;

procedure TForm1.StringGrid1Click(Sender: TObject); {Solldaten ändern}
var x: integer;
    s: string;
begin
  s:='';
  for x:=0 to 11 do
    s:=s+StringGrid1.Cells[x,1]+sep;
  XMLPropStorage1.StoredValue['Sollertrag']:=s;      {Solldaten speichern}
end;

procedure TForm1.CheckBox1Change(Sender: TObject);  {JS-Dateien anlegen merken}
begin
  XMLPropStorage1.StoredValue['JSDateienAnlegen']:=BoolToStr(Checkbox1.Checked);
  if Checkbox1.Checked and (deServer.FileName='') then begin  {Arbeitsverzeichnis benutzen}
    deServer.InitialDir:=deLocal.Directory;
    deServer.FileName:=deLocal.Directory;
  end;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);  {gleiche FTP-Daten für Homepage}
begin
  XMLPropStorage1.StoredValue['FTP_single']:=BoolToStr(CheckBox3.Checked);
  if CheckBox3.Checked then begin
    CheckBox6.Checked:=false;
    CheckBox6.Enabled:=false;
    edHTMLserver.Text:=edServerName.Text;
    edHTMLserver.Enabled:=false;
    edHTMLuser.Text:=edUser.Text;
    edHTMLuser.Enabled:=false;
    edHTMLpasw.Text:=edPasw.Text;
    edHTMLpasw.Enabled:=false;
  end else begin
    CheckBox6.Enabled:=true;
    edHTMLserver.Enabled:=true;
    edHTMLuser.Enabled:=true;
    edHTMLpasw.Enabled:=true;
  end;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);  {FTP Datawarehouse aktivieren}
begin
  XMLPropStorage1.StoredValue['FTP_DWakt']:=BoolToStr(CheckBox2.Checked);
end;

procedure TForm1.CheckBox4Change(Sender: TObject);  {FTP Homepage aktivieren}
begin
  XMLPropStorage1.StoredValue['FTP_HPakt']:=BoolToStr(CheckBox4.Checked);
end;

procedure TForm1.CheckBox5Change(Sender: TObject);  {Zeige Warehouse Passwort}
begin
  if CheckBox5.Checked then
    edPasw.PasswordChar:=#0
  else
    edPasw.PasswordChar:='*';
  edPasw.Refresh;
end;

procedure TForm1.CheckBox6Change(Sender: TObject);  {Zeige Homepage Passwort}
begin
  if CheckBox6.Checked then
    edHTMLpasw.PasswordChar:=#0
  else
    edHTMLpasw.PasswordChar:='*';
  edHTMLpasw.Refresh;
end;

procedure TForm1.CheckBox7Change(Sender: TObject);  {Jahresertrag normiert darstellen}
begin
  XMLPropStorage1.StoredValue['JNormiert']:=BoolToStr(CheckBox7.Checked);
  JahrStat;     {neu zeichnen}
end;

procedure TForm1.CheckBox10Change(Sender: TObject); {Monatsertrag normiert darstellen}
begin
  XMLPropStorage1.StoredValue['MNormiert']:=BoolToStr(CheckBox10.Checked);
  MonStat;     {neu zeichnen}
end;

procedure TForm1.CheckBox11Change(Sender: TObject); {keine Sommerzeit}
begin
  XMLPropStorage1.StoredValue['NoDST']:=BoolToStr(CheckBox11.Checked);
end;

procedure TForm1.CheckBox12Change(Sender: TObject); {Supress file listing}
begin
  XMLPropStorage1.StoredValue['NoFileList']:=BoolToStr(CheckBox12.Checked);
end;

procedure TForm1.CheckBox13Change(Sender: TObject); {No data reduction}
begin
  XMLPropStorage1.StoredValue['NoReduction']:=BoolToStr(CheckBox13.Checked);
end;

procedure TForm1.CheckBox8Change(Sender: TObject);  {Tagesanzeige mit Strings}
begin
  XMLPropStorage1.StoredValue['ShowStrings']:=BoolToStr(CheckBox8.Checked);
  if RadioGroup2.ItemIndex<3 then TagStat; {Ertrag oder Leistung neu zeichnen}
end;

procedure TForm1.CheckBox9Change(Sender: TObject);  {Passive FTP einstellen}
begin
  XMLPropStorage1.StoredValue['FTPpasv']:=BoolToStr(CheckBox9.Checked);
  if CheckBox9.Checked then begin
    IdFTP1.Passive:=true;
    SynMemo1.Lines.Add(capFTPpasv);                {im Log protokollieren}
  end else
    IdFTP1.Passive:=false;
end;

procedure TForm1.ColorButton1ColorChanged(Sender: TObject);   {Farben speichern}
begin
  XMLPropStorage1.StoredValue['clMatrix']:=ColorMatrix;
end;

procedure TForm1.btnCloseClick(Sender: TObject);     {Beenden}
begin
  Form1.Close;
end;

procedure TForm1.StandSpeichern;                    {Stand in Datei speichern}
var dr: string;
begin
  dr:=ExtractFilePath(Application.ExeName);
  try
    OutList.SaveToFile(dr+rawdat);
    if DayList.Count>0 then
      DayList.SaveToFile(dr+archID+archfn);
    StatusBar1.Panels[2].Text:=Format(rsSaveRaw,[dr+rawdat]);
  except
    StatusBar1.Panels[2].Text:=msgNoSave;
    SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
  end;
end;

procedure TForm1.btnSaveClick(Sender: TObject);     {Button Stand Speichern}
begin
  btnTag.Tag:=0;
  ComboBox1.Color:=clDefault;
  if OutList.Count>100 then
    StandSpeichern
  else
    StatusBar1.Panels[2].Text:=msgDatSp;
end;

procedure TForm1.btnLoadClick(Sender: TObject);      {Stand laden}
var dr: string;
begin
  Screen.Cursor:=crHourGlass;
  try
    PrevFileList.Clear;                              {Dateiliste zurücksetzen}
    dr:=ExtractFilePath(Application.ExeName);
    btnTag.Tag:=0;
    btnArch.Enabled:=false;                         {Archivieren sperren}
    ComboBox1.Color:=clDefault;
    try
      OutList.LoadFromFile(dr+rawdat);
      DayList.Clear;
      if FileExists(dr+archID+archfn) then
        DayList.LoadFromFile(dr+archID+archfn);
      OutListLesen(false);                           {ausführen ohne HTML prot}
      if pcMain.ActivePageIndex=epg then
        pcMain.ActivePageIndex:=2;
      btnBackup.Enabled:=true;
      btnSimu.Enabled:=true;
      BitBtn11.Enabled:=true;                        {Spez. Analyse entsperren}
      RadioGroup3.Enabled:=true;;
      ChangeTab;
    except
      StatusBar1.Panels[2].Text:=Format(rsNotFound, [rawdat]); {oder Daten falsch}
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.btnTagClick(Sender: TObject);     {Restore verlorenen Tag}
begin
  if btnTag.Tag=0 then begin
    OpenDialog1.InitialDir:=deLocal.Directory;
    OpenDialog1.FilterIndex:=1;
    OpenDialog1.Title:=titLdBackup;
    OpenDialog1.Filename:=rawdat;
    if OpenDialog1.Execute then begin
      OutList.LoadFromFile(OpenDialog1.FileName);
      OutListLesen(false);                          {ausführen ohne HTML prot}
      btnTag.Tag:=1;
      pcMain.ActivePageIndex:=0;
      ChangeTab;
    end;
  end else btnTag.Tag:=0;
  if btnTag.Tag=1 then
    ComboBox1.Color:=$000080FF
  else
    ComboBox1.Color:=clDefault;
end;

{ Shape4: Statusanzeige für FTP
          clDefault: Disconnect
          clBlue   : Upload
          clGreen  : Download
          clBlack  : Test
          clRed    : Error }

procedure TForm1.btnTestFTPClick(Sender: TObject);    {Test DW-FPT}
begin
  SynMemo1.Lines.Add('');
  SynMemo1.Lines.Add('Test FTP datawarehouse');
  StatusBar1.Panels[2].Text:=msgNetzwerk;
  if edPasw.Text <> '' then begin            {Pflichtfelder prüfen}
    if edServerName.Text <> '' then begin
      SynMemo1.Lines.Add(edServerName.Text);
      if edUser.Text <> '' then begin
        SynMemo1.Lines.Add(edUser.Text);
        FTPAnzeige(clBlack);
        try
          FTPloginDW;
          StatusBar1.Panels[2].Text:=IdFTP1.RetrieveCurrentDir;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          FTPlogout;
        except
          on e: Exception do begin
            StatusBar1.Panels[2].Text:=e.Message;
            SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
            FTPlogout;
            FTPAnzeige(clRed);
          end;
        end;
      end else
        StatusBar1.Panels[2].Text:=msgAccountMissing;
    end else
      StatusBar1.Panels[2].Text:=msgHostMissing;
  end else
    StatusBar1.Panels[2].Text:=msgPswMissing;
end;

procedure TForm1.btnTestHTMLClick(Sender: TObject);    {Test HP-FTP}
begin
  SynMemo1.Lines.Add('');
  SynMemo1.Lines.Add('Test FTP homepage');
  StatusBar1.Panels[2].Text:=msgNetzwerk;
  if edHTMLpasw.Text <> '' then begin
    if edHTMLserver.Text <> '' then begin
      SynMemo1.Lines.Add(edHTMLserver.Text);
      if edHTMLuser.Text <> '' then begin
        SynMemo1.Lines.Add(edHTMLuser.Text);
        FTPAnzeige(clBlack);
        try
          FTPloginHP;
          StatusBar1.Panels[2].Text:=IdFTP1.RetrieveCurrentDir;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          FTPlogout;
        except
          on e: Exception do begin
            StatusBar1.Panels[2].Text:=e.Message;
            SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
            FTPlogout;
            FTPAnzeige(clRed);
          end;
        end;
      end else
        StatusBar1.Panels[2].Text:=msgAccountMissing;
    end else
      StatusBar1.Panels[2].Text:=msgHostMissing;
  end else
    StatusBar1.Panels[2].Text:=msgPswMissing;
end;

procedure TForm1.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                      {Klicken mit mittlerer Taste}
    Chart1.ZoomFull;
end;

procedure TForm1.Chart2DblClick(Sender: TObject);  {Monat label ein-/ausblenden}
begin
  Chart2BarSeries1.Marks.Visible:=not Chart2BarSeries1.Marks.Visible;
end;

procedure TForm1.Chart3DblClick(Sender: TObject);  {Jahr label ein-/ausblenden}
begin
  Chart3BarSeries1.Marks.Visible:=not Chart3BarSeries1.Marks.Visible;
end;

procedure TForm1.Chart4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Zoomen rückgängig machen}
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                      {Klicken mit mittlerer Taste}
    Chart4.ZoomFull;
end;

procedure TForm1.Chart5MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Spielwiese}
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                      {Klicken mit mittlerer Taste}
    Chart5.ZoomFull;                               {Zoomen beenden}
end;

procedure TForm1.ChartToolset2DataPointClickTool1PointClick(ATool: TChartTool;
  APoint: TPoint);                                 {einen Tages-Balken anklicken}
var NewDay: string;
    p: integer;
begin
  if (ChartToolset2DataPointClickTool1.Series is TBarSeries) then begin
    Newday:=ComboBox2.Text+datsep+
            Format('%.2d', [ChartToolset2DataPointClickTool1.PointIndex+1]);
    p:=ComboBox1.Items.IndexOf(NewDay);
    if p>=0 then begin
      ComboBox1.Text:=ComboBox1.Items[p];          {Datum yyyy-mm-dd}
      ComboBox4.Text:=ComboBox1.Text;              {Selektion 1 Spielwiese}
      ComboBox5.Text:=ComboBox1.Text;              {Selektion 2 Spielwiese}
      pcMain.ActivePageIndex:=0;
      pcMain.Tag:=0;                         {TabSheet merken}
      if RadioGroup2.ItemIndex=5 then
        RadioGroup2.ItemIndex:=1; {nicht Sweep}
      TagStat;
    end else
      StatusBar1.Panels[2].Text:=rsDayArch;
  end;
end;

procedure TForm1.ChartToolset3DataPointClickTool1PointClick(ATool: TChartTool;
  APoint: TPoint);                                 {einen Monats-Balken anklicken}
var idx, year: integer;
begin
  if (ChartToolset3DataPointClickTool1.Series is TBarSeries) then begin
    idx:=ChartToolset3DataPointClickTool1.PointIndex+1;
    year:=StrToInt(ComboBox3.Text);
    if (YearOf(now)=year) and (MonthOf(now)<4) then begin
      idx:=idx-3;
      if idx<1 then begin                          {Subtract 3 month}
        year:=year-1;
        idx:=idx+12;
      end;
    end;
    ComboBox2.Text:=IntToStr(year)+datsep+Format('%.2d', [idx]);
    pcMain.ActivePageIndex:=1;
    pcMain.Tag:=1;                           {TabSheet merken}
    MonStat;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);     {Abschluß und aufräumen}
begin
  try
    if IdFTP1.Connected then
      IdFTP1.Disconnect;                           {FTP auslösen}
  finally
    FreeAndNil(OutList);                           {Listen vom Speicher nehmen}
    FreeAndNil(DayList);
    FreeAndNil(PrevFileList);
    FreeAndNil(IDXList);
    Screen.Cursor:=crDefault;                      {Cursor auf default}
  end;
end;

function GetFTPAccount(s: string): string;   {get FTP account from File name}
var x: integer;
    s1: string;
begin
  result:='';
  s1:=ExtractFileName(s);
  for x:=1 to length(s1) do begin
    if (s1[x]='-') or (s1[x]='.') then break;      {Danfoss specific: '-'}
    result:=result+s1[x];
  end;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var fld: string;                       {Objekt aus das geworfen wurde}
begin
  fld:=FindControlAtPosition(Mouse.CursorPos,true).Name;
  if (fld='GroupBox2') or (fld='DirectoryEdit1') then begin
    if DirectoryExists(FileNames[0]) then
      deLocal.Directory:=FileNames[0]
    else
      deLocal.Directory:=ExtractFilePath(FileNames[0]);
  end;
  if (fld='FileNameEdit1') and FileExists(FileNames[0]) then
      deServer.FileName:=FileNames[0];        {HTML file eintragen}
  if (fld='LabeledEdit1') and FileExists(FileNames[0]) then begin
    fld:=GetFTPAccount(FileNames[0]);   {FTP account und Filter eintragen}
    edFilter.Text:=fld+'-*';
    edUser.Text:=fld;
  end;
  Application.BringToFront;
end;

procedure TForm1.GroupBox1Click(Sender: TObject);  {Infobox}
begin
  btnEinlesen.Enabled:=DirectoryExists(deLocal.Directory); {Gültigkeit Buttons prüfen}
  btnLoad.Enabled:=FileExists((ExtractFilePath(Application.ExeName)+rawdat));
  if MessageDlg(ExtractFileName(paramstr(0))+'  '+
                version+sLineBreak+sLineBreak+
                meinname+sLineBreak+homepage+sLineBreak+email+sLineBreak+sLineBreak+
                rsWR+' ID: '+XMLPropStorage1.StoredValue['InverterID']+sLineBreak+
                rsWR+' '+rsName+': '+XMLPropStorage1.StoredValue['InverterName']+sLineBreak+
                rsPeakL+': '+edPeak.Text+'W'+sLineBreak+
                rsEEGV+'/kWh: '+edVerg.Text+edEuro.Text,
                mtInformation,[mbHelp, mbOK],0)=0 then
    OpenUrl(Infofile);                            {0: mrHelp nicht def}
end;

procedure TForm1.IdFTP1Status(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  StatusBar1.Panels[2].Text:=AStatusText;
  SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
end;

procedure TForm1.Label51Click(Sender: TObject);        {Link Manual}
begin
  if OpenUrl(Infofile) then Label51.Font.Color:=clPurple;
  Form1.Refresh;
end;

procedure TForm1.Label51MouseEnter(Sender: TObject);   {Link animieren}
begin
  with Label51 do begin
    Cursor:=crHandPoint;
    Font.Style:=Font.Style+[fsBold];
  end;
end;

procedure TForm1.Label51MouseLeave(Sender: TObject);   {Link animieren}
begin
  Label51.Font.Style:=Font.Style-[fsBold];
end;

procedure TForm1.Label52Click(Sender: TObject);        {Homepage}
begin
  if OpenUrl(Homepage) then
    Label52.Font.Color:=clPurple;
end;

procedure TForm1.Label52MouseEnter(Sender: TObject);
begin
  Label52.Cursor:=crHandPoint;
  Label52.Font.Style:=Font.Style+[fsBold];
end;

procedure TForm1.Label52MouseLeave(Sender: TObject);
begin
  Label52.Font.Style:=Font.Style-[fsBold];
end;

procedure TForm1.Label53Click(Sender: TObject);        {Download Update}
begin
  if OpenUrl(dupdate) then Label53.Font.Color:=clPurple;
end;

procedure TForm1.Label53MouseEnter(Sender: TObject);
begin
  Label53.Cursor:=crHandPoint;
  Label53.Font.Style:=Font.Style+[fsBold];
end;

procedure TForm1.Label53MouseLeave(Sender: TObject);
begin
  Label53.Font.Style:=Font.Style-[fsBold];
end;

procedure TForm1.edHTMLserverChange(Sender: TObject); {Basisdaten speichern}
begin
  XMLPropStorage1.StoredValue['FTP_HPserver']:=edHTMLserver.Text;
end;

procedure TForm1.edHTMLuserChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['FTP_HPaccount']:=edHTMLuser.Text;
end;

procedure TForm1.edHTMLpaswChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['FTP_HPpsw']:=edHTMLpasw.Text;
end;

procedure TForm1.edHTMLfolderChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['FTP_HPpath']:=edHTMLfolder.Text;
end;

procedure TForm1.edHTMLfileChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['FTP_HTMLfile']:=edHTMLfile.Text;
end;

procedure TForm1.edEuroChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['CurrStr']:=edEuro.Text;
  edVerg.EditLabel.Caption:=rsEEGV+  ' ['+edEuro.Text+'/kWh]';
end;

procedure TForm1.edKorrFakChange(Sender: TObject);
begin
  XMLPropStorage1.StoredValue['KorrE']:=edKorrFak.Text;
end;

procedure TForm1.LabeledEdit17Change(Sender: TObject); {Stringpower merken}
begin
  XMLPropStorage1.StoredValue['PeakString']:=LabeledEdit17.Text+sep+
                                             LabeledEdit18.Text+sep+
                                             LabeledEdit19.Text;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);   {Ein Tag rückwärts}
begin
  If ComboBox1.ItemIndex>0 then begin
    ComboBox1.ItemIndex:=ComboBox1.ItemIndex-1;
    if RadioGroup3.ItemIndex=0 then begin
      ComboBox4.Text:=ComboBox1.Text;
      ComboBox5.Text:=ComboBox1.Text;
    end;
    TagStat;
  end;
  SpeedButton1.Enabled:=not (ComboBox1.ItemIndex=0);
  SpeedButton2.Enabled:=not (ComboBox1.ItemIndex=ComboBox1.Items.Count-1);
  DateEdit1.Date:=SDToTime(ComboBox1.Text);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);   {Ein Tag vorwärts}
begin
  if ComboBox1.ItemIndex<(ComboBox1.Items.Count-1) then begin
    ComboBox1.ItemIndex:=ComboBox1.ItemIndex+1;
    if RadioGroup3.ItemIndex=0 then begin
      ComboBox4.Text:=ComboBox1.Text;
      ComboBox5.Text:=ComboBox1.Text;
    end;
    TagStat;
  end;
  SpeedButton1.Enabled:=not (ComboBox1.ItemIndex=0);
  SpeedButton2.Enabled:=not (ComboBox1.ItemIndex=ComboBox1.Items.Count-1);
  DateEdit1.Date:=SDToTime(ComboBox1.Text);
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);   {Monat vor}
begin
  if ComboBox2.ItemIndex<(ComboBox2.Items.Count-1) then begin
    ComboBox2.ItemIndex:=ComboBox2.ItemIndex+1;
    MonStat;
  end;
  SpeedButton4.Enabled:=not (ComboBox2.ItemIndex=0);
  SpeedButton3.Enabled:=not (ComboBox2.ItemIndex=ComboBox2.Items.Count-1);
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);   {Monat zurück}
begin
  If ComboBox2.ItemIndex>0 then begin
    ComboBox2.ItemIndex:=ComboBox2.ItemIndex-1;
    MonStat;
  end;
  SpeedButton4.Enabled:=not (ComboBox2.ItemIndex=0);
  SpeedButton3.Enabled:=not (ComboBox2.ItemIndex=ComboBox2.Items.Count-1);
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);   {Jahr vor}
begin
  if ComboBox3.ItemIndex<(ComboBox3.Items.Count-1) then begin
    ComboBox3.ItemIndex:=ComboBox3.ItemIndex+1;
    JahrStat;
  end;
  SpeedButton6.Enabled:=not (ComboBox3.ItemIndex=0);
  SpeedButton5.Enabled:=not (ComboBox3.ItemIndex=ComboBox3.Items.Count-1);
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);   {Jahr zurück}
begin
  If ComboBox3.ItemIndex>0 then begin
    ComboBox3.ItemIndex:=ComboBox3.ItemIndex-1;
    JahrStat;
  end;
  SpeedButton6.Enabled:=not (ComboBox3.ItemIndex=0);
  SpeedButton5.Enabled:=not (ComboBox3.ItemIndex=ComboBox3.Items.Count-1);
end;

procedure TForm1.btnEinlesenClick(Sender: TObject);        {Auswertung starten}
begin
  btnTag.Tag:=0;                                      {Restore rücksetzen}
  Timer1.Enabled:=false;
  ComboBox1.Color:=clDefault;
  FTPDownload;
  Auswerten;                                           {lokale Daten einlesen}
  if (spePeriod.Value>0) and (OutList.Count>0) then begin
    Timer1.Interval:=spePeriod.Value*60000;
    Timer1.Enabled:=true;                              {Timer neu starten}
  end;
end;

procedure TForm1.btnArchClick(Sender: TObject);       {Archivieren}
begin
  SynMemo1.Lines.Add('');
  SynMemo1.Lines.Add('Archive started at: '+FormatDateTime('hh:nn:ss', now));
  btnTag.Tag:=0;                                      {Restore rücksetzen}
  FTPDownload;                                         {Dateien sychronisieren}
  BackupSpeichern;                                     {erstmal speichern}
  ArchRename;                                          {alte Daten archivieren}
  Auswerten;                                           {Neuladen}
  SynMemo1.Lines.Add('Archive done at   : '+FormatDateTime('hh:nn:ss', now));
end;

procedure TForm1.BitBtn11Click(Sender: TObject);       {spez. Analyse ausführen}
begin
  SpezAnalyse;
end;

procedure TForm1.btnColorsResetClick(Sender: TObject);       {Reset Colors to default}
begin
  ColorButton1.ButtonColor:=clRed;                     {Spielwiese}
  ColorButton2.ButtonColor:=clBlue;
  ColorButton3.ButtonColor:=clBlue;                    {Tagesauswertung}
  ColorButton4.ButtonColor:=clLime;
  ColorButton5.ButtonColor:=clFuchsia;
  ColorButton6.ButtonColor:=clRed;
  ColorButton7.ButtonColor:=clSkyBlue;                 {Monatsauswertung}
  ColorButton8.ButtonColor:=clGreen;                   {Allgemein}
  ColorButton9.ButtonColor:=clBlue;
  ColorButton10.ButtonColor:=clRed;
  ColorButton11.ButtonColor:=clGreen;
  ColorButton12.ButtonColor:=clBlue;                   {Jahresauswertung}
  ColorButton13.ButtonColor:=clSkyBlue;
  ColorButton14.ButtonColor:=clMoneyGreen;
  ColorButton15.ButtonColor:=clBlue;                   {Statistik}
  ColorButton16.ButtonColor:=clGreen;
  ColorButton17.ButtonColor:=clMoneyGreen;
  ColorButton18.ButtonColor:=clSkyBlue;
  ColorButton19.ButtonColor:=clGreen;
  ColorButton20.ButtonColor:=clRed;
  XMLPropStorage1.StoredValue['clMatrix']:=ColorMatrix;                                         {speichern}
end;

procedure TForm1.btnColorsLoadClick(Sender: TObject);       {Farbschema laden}
var slist: TStringList;
    x: integer;
    s: string;
begin
  slist:=TStringList.Create;
  try
    OpenDialog1.Title:=capBB13+'...';
    OpenDialog1.FilterIndex:=2;
    OpenDialog1.InitialDir:=ExtractFilePath(Application.ExeName);
    if OpenDialog1.Execute then slist.LoadFromFile(OpenDialog1.FileName);
    for x:=0 to slist.Count-1 do begin
      s:=slist[x];
      if (pos(sep, s)>2) and (length(s)>80) then begin
        SetDColor(s);    {Farben übernehmen}
        break;
      end;
    end;
  finally
    FreeAndNil(slist);
  end;
end;

procedure TForm1.btnColorsSaveClick(Sender: TObject);       {Farbschema speichern}
var slist: TStringList;
begin
  slist:=TStringList.Create;
  try
    SaveDialog1.Title:=capBB14+'...';
    SaveDialog1.FilterIndex:=2;
    SaveDialog1.InitialDir:=ExtractFilePath(Application.ExeName);
    SaveDialog1.FileName:='ColorMatrix_'+FormatDateTime('mmddhh', now)+'.clm';
    sList.Add(ExtractFileName(Application.ExeName)+' ('+version+') - '+capBB14+': '+
              FormatDateTime('yyyy-mm-dd hh:nn:ss', now));
    sList.Add('');
    sList.Add(ColorMatrix);
    if SaveDialog1.Execute then slist.SaveToFile(SaveDialog1.FileName);
  finally
    FreeAndNil(slist);
  end;
end;

procedure TForm1.SpezAnalyse;                      {spez. Analyse ausführen}
var x, p, z: integer;
    w1, w2, swt1, swt2: double;
    SplitList: TStringList;
    dpt: TDateTime;
begin
  XMLPropStorage1.StoredValue['PGlY']:=IntToStr(RadioGroup4.ItemIndex);
  XMLPropStorage1.StoredValue['PGrY']:=IntToStr(RadioGroup5.ItemIndex);
  SplitList:=TStringList.Create;
  SplitList.Delimiter:=sep;
  Screen.Cursor:=crHourGlass;
  Chart5LineSeries1.Clear;
  Chart5LineSeries2.Clear;
  Chart5.ZoomFull;                                 {Zoomen beenden}
  ChartAxisTransformations1AutoScaleAxisTransform1.MaxValue:=1;
  ChartAxisTransformations2AutoScaleAxisTransform1.MaxValue:=1;
  ChartAxisTransformations1AutoScaleAxisTransform1.Enabled:=true;
  ChartAxisTransformations2AutoScaleAxisTransform1.Enabled:=true;
  Chart5LineSeries1.SeriesColor:=ColorButton1.ButtonColor;
  Chart5.AxisList[0].Title.LabelFont.Color:=ColorButton1.ButtonColor;
  Chart5LineSeries2.SeriesColor:=ColorButton2.ButtonColor;
  Chart5.AxisList[2].Title.LabelFont.Color:=ColorButton2.ButtonColor;
  if RadioGroup4.ItemIndex=RadioGroup5.ItemIndex then begin  {Skalierung fix}
    ChartAxisTransformations1AutoScaleAxisTransform1.Enabled:=false;
    ChartAxisTransformations2AutoScaleAxisTransform1.Enabled:=false;
  end;
  case RadioGroup4.ItemIndex of  {Beschriftung und Schwellwert festlegen links}
      0: begin
           swt1:=StrToInt(edMinPower.Text)/1000; {AC Leistung}
           Chart5.AxisList[0].Title.Caption:='AC '+rsLei+' [kW]';
         end;
      1: begin
           swt1:=StrToInt(edMinPower.Text)/1000; {DC Leistung}
           Chart5.AxisList[0].Title.Caption:='DC '+rsLei+' [kW]';
         end;
      2: begin
           swt1:=0;                                {Delta}
           Chart5.AxisList[0].Title.Caption:='Delta DC-AC [W]';
         end;
      3: begin
           swt1:=-1;                               {Energie Ertrag}
           Chart5.AxisList[0].Title.Caption:=rsErtrag+' [kWh]';
         end;
      4: begin
           swt1:=10;                               {Netzspannung}
           Chart5.AxisList[0].Title.Caption:=rsUAC+' [V]';
         end;
      5: begin
           swt1:=40;                               {Netzfrequenz}
           Chart5.AxisList[0].Title.Caption:=rsFrequ+' [Hz]';
         end;
      6: begin
           swt1:=-10;                              {Temperatur}
           Chart5.AxisList[0].Title.Caption:=rsWRTemp+' [°C]';
         end;
      7: begin
           swt1:=10;                               {Spannung 1}
           Chart5.AxisList[0].Title.Caption:='DC '+rsSpg+rsString+'1 [V]';
         end;
      8: begin
           swt1:=0;                                {Strom 1}
           Chart5.AxisList[0].Title.Caption:='DC '+rsStrom+rsString+'1 [A]';
         end;
      9: begin
           swt1:=10;                               {Spannung 2}
           Chart5.AxisList[0].Title.Caption:='DC '+rsSpg+rsString+'2 [V]';
         end;
     10: begin
           swt1:=0;                                {Strom 2}
           Chart5.AxisList[0].Title.Caption:='DC '+rsStrom+rsString+'2 [A]';
         end;
     11: begin
           swt1:=10;                               {Spannung 3}
           Chart5.AxisList[0].Title.Caption:='DC '+rsSpg+rsString+'3 [V]';
         end;
     12: begin
           swt1:=0;                                {Strom 3}
           Chart5.AxisList[0].Title.Caption:='DC '+rsStrom+rsString+'3 [A]';
         end;
     13: begin
           swt1:=StrToInt(edMinPower.Text)/1000; {DC Leistung String 1}
           Chart5.AxisList[0].Title.Caption:='DC '+rsLei+rsString+'1 [kW]';
         end;
     14: begin
           swt1:=StrToInt(edMinPower.Text)/1000; {DC Leistung String 2}
           Chart5.AxisList[0].Title.Caption:='DC '+rsLei+rsString+'2 [kW]';
         end;
     15: begin
           swt1:=StrToInt(edMinPower.Text)/1000; {DC Leistung String 3}
           Chart5.AxisList[0].Title.Caption:='DC '+rsLei+rsString+'3 [kW]';
         end;
     16: begin
           swt1:=0;                                {Isolationswiderstand}
           Chart5.AxisList[0].Title.Caption:=rsIsoWd+' [kOhm]';
         end;
  end;
  case RadioGroup5.ItemIndex of  {Beschriftung und Schwellwert festlegen rechts}
      0: begin
           swt2:=StrToInt(edMinPower.Text)/1000; {AC Leistung}
           Chart5.AxisList[2].Title.Caption:='AC '+rsLei+' [kW]';
         end;
      1: begin
           swt2:=StrToInt(edMinPower.Text)/1000; {DC Leistung}
           Chart5.AxisList[2].Title.Caption:='DC '+rsLei+' [kW]';
         end;
      2: begin
           swt2:=0;                                {Delta}
           Chart5.AxisList[2].Title.Caption:='Delta DC-AC [W]';
         end;
      3: begin
           swt2:=-1;                               {Energie Ertrag}
           Chart5.AxisList[2].Title.Caption:=rsErtrag+' [kWh]';
         end;
      4: begin
           swt2:=10;                               {Netzspannung}
           Chart5.AxisList[2].Title.Caption:=rsUAC+' [V]';
         end;
      5: begin
           swt2:=40;                               {Netzfrequenz}
           Chart5.AxisList[2].Title.Caption:=rsFrequ+' [Hz]';
         end;
      6: begin
           swt2:=-10;                              {Temperatur}
           Chart5.AxisList[2].Title.Caption:=rsWRTemp+' [°C]';
         end;
      7: begin
           swt2:=10;                               {Spannung 1}
           Chart5.AxisList[2].Title.Caption:='DC '+rsSpg+rsString+'1 [V]';
         end;
      8: begin
           swt2:=0;                                {Strom 1}
           Chart5.AxisList[2].Title.Caption:='DC '+rsStrom+rsString+'1 [A]';
         end;
      9: begin
           swt2:=10;                               {Spannung 2}
           Chart5.AxisList[2].Title.Caption:='DC '+rsSpg+rsString+'2 [V]';
         end;
     10: begin
           swt2:=0;                                {Strom 2}
           Chart5.AxisList[2].Title.Caption:='DC '+rsStrom+rsString+'2 [A]';
         end;
     11: begin
           swt2:=10;                               {Spannung 3}
           Chart5.AxisList[2].Title.Caption:='DC '+rsSpg+rsString+'3 [V]';
         end;
     12: begin
           swt2:=0;                                {Strom 3}
           Chart5.AxisList[2].Title.Caption:='DC '+rsStrom+rsString+'3 [A]';
         end;
     13: begin
           swt2:=StrToInt(edMinPower.Text)/1000; {DC Leistung String 1}
           Chart5.AxisList[2].Title.Caption:='DC '+rsLei+rsString+'1 [kW]';
         end;
     14: begin
           swt2:=StrToInt(edMinPower.Text)/1000; {DC Leistung String 2}
           Chart5.AxisList[2].Title.Caption:='DC '+rsLei+rsString+'2 [kW]';
         end;
     15: begin
           swt2:=StrToInt(edMinPower.Text)/1000; {DC Leistung String 3}
           Chart5.AxisList[2].Title.Caption:='DC '+rsLei+rsString+'3 [kW]';
         end;
     16: begin
           swt2:=0;                                {Isolationswiderstand}
           Chart5.AxisList[2].Title.Caption:=rsIsoWd+' [kOhm]';
         end;
  end;
  case RadioGroup3.ItemIndex of
    0: StatusBar1.Panels[2].Text:=Chart5.AxisList[0].Title.Caption+rsFor+
                                  ComboBox4.Text+' / '+
                                  Chart5.AxisList[2].Title.Caption+rsFor+
                                  ComboBox5.Text;
    3: StatusBar1.Panels[2].Text:=Chart5.AxisList[0].Title.Caption+' / '+
                                  Chart5.AxisList[2].Title.Caption;
    else StatusBar1.Panels[2].Text:=Chart5.AxisList[0].Title.Caption+' / '+
                                  Chart5.AxisList[2].Title.Caption+rsFor+
                                  ComboBox4.Text;
  end;
  try
    case RadioGroup3.ItemIndex of               {Länge Vergleichsstring}
      1: p:=7;
      2: p:=4;
      else p:=10;
    end;                                        {OutList auslesen}
    if ComboBox4.Text>ComboBox5.Text then z:=GetOutListIDX(ComboBox5.Text)
                                     else z:=GetOutListIDX(ComboBox4.Text);
    if RadioGroup3.ItemIndex=3 then z:=0;
    for x:=z to OutList.Count-1 do begin
      try
        SplitList.DelimitedText:=OutList[x];
        dpt:=SDToTime(SplitList[0])+TToTT(SplitList[1]);
        if CheckBox11.Checked then dpt:=NixDST(dpt);
        if RadioGroup3.ItemIndex=0 then dpt:=frac(dpt);
        if (RadioGroup3.ItemIndex=3) or
           (copy(OutList[x], 1, p)=ComboBox4.Text) then begin
          case RadioGroup4.ItemIndex of
             0: w1:=StrToInt(SplitList[2])/1000;    {Leistung}
             1: w1:=StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000000+
                             StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000000+
                             StrToInt(SplitList[9])*StrToInt(SplitList[10])/10000000;
             2: w1:=StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000+
                             StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000+
                             StrToInt(SplitList[9])*StrToInt(SplitList[10])/10000-
                             StrToInt(SplitList[2]);{Delta in Watt}
             3: w1:=StrToInt(SplitList[3])/1000;    {Energieertrag}
             4: w1:=StrToInt(SplitList[11]);        {Netzspannung}
             5: w1:=StrToInt(SplitList[12])/100;    {Netzfrequenz}
             6: w1:=StrToInt(SplitList[4]);         {Temperatur}
             7: w1:=StrToInt(SplitList[5])/10;      {U_DC1}
             8: w1:=StrToInt(SplitList[6])/1000;    {I_DC1}
             9: w1:=StrToInt(SplitList[7])/10;      {U_DC2}
            10: w1:=StrToInt(SplitList[8])/1000;    {I_DC2}
            11: w1:=StrToInt(SplitList[9])/10;      {U_DC3}
            12: w1:=StrToInt(SplitList[10])/1000;   {I_DC3}
            13: w1:=StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000000;  {P_DC1}
            14: w1:=StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000000;  {P_DC2}
            15: w1:=StrToInt(SplitList[9])*StrToInt(SplitList[10])/10000000; {P_DC3}
            16: w1:=StrToInt(SplitList[14]);        {R_DC}
          end;
          if w1>swt1 then Chart5LineSeries1.AddXY(dpt, w1);
        end;
        if (RadioGroup3.ItemIndex=3) or
           (copy(OutList[x], 1, p)=ComboBox5.Text) then begin
          case RadioGroup5.ItemIndex of
             0: w2:=StrToInt(SplitList[2])/1000;    {Leistung}
             1: w2:=StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000000+
                             StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000000+
                             StrToInt(SplitList[9])*StrToInt(SplitList[10])/10000000;
             2: w2:=StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000+
                             StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000+
                             StrToInt(SplitList[9])*StrToInt(SplitList[10])/10000-
                             StrToInt(SplitList[2]);{Delta in Watt}
             3: w2:=StrToInt(SplitList[3])/1000;    {Energieertrag}
             4: w2:=StrToInt(SplitList[11]);        {Netzspannung}
             5: w2:=StrToInt(SplitList[12])/100;    {Netzfrequenz}
             6: w2:=StrToInt(SplitList[4]);         {Temperatur}
             7: w2:=StrToInt(SplitList[5])/10;      {U_DC1}
             8: w2:=StrToInt(SplitList[6])/1000;    {I_DC1}
             9: w2:=StrToInt(SplitList[7])/10;      {U_DC2}
            10: w2:=StrToInt(SplitList[8])/1000;    {I_DC2}
            11: w2:=StrToInt(SplitList[9])/10;      {U_DC3}
            12: w2:=StrToInt(SplitList[10])/1000;   {I_DC3}
            13: w2:=StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000000;  {P_DC1}
            14: w2:=StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000000;  {P_DC2}
            15: w2:=StrToInt(SplitList[9])*StrToInt(SplitList[10])/10000000; {P_DC3}
            16: w2:=StrToInt(SplitList[14]);        {R_DC}
          end;
          if w2>swt2 then Chart5LineSeries2.AddXY(dpt, w2);
        end;
      except
        on e: exception do begin
          StatusBar1.Panels[2].Text:='Fehler in SpezAnalysis: '+e.Message;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
        end;
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
    SplitList.Free;
  end;
end;

procedure TForm1.BackupSpeichern;                      {Backup in Datei}
var s: string;
begin
  if OutList.Count>100 then begin
    s:=IncludeTrailingPathDelimiter(deLocal.Directory)+
       FormatDateTime(isodate, now-1)+'_'+rawdat;   {Datumsstempel}
    try
      if DayList.Count>0 then        {Archivierte Tage auch speichern}
        DayList.SaveToFile(IncludeTrailingPathDelimiter(deLocal.Directory)+
                           archID+FormatDateTime(isodate, now-1)+'_'+archfn);
      OutList.SaveToFile(s);
      StatusBar1.Panels[2].Text:=Format(msgBackupSp, [s]);
    except
      StatusBar1.Panels[2].Text:=msgNoSave;
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
    end;
  end else StatusBar1.Panels[2].Text:=msgDatSp;
end;

procedure TForm1.btnBackupClick(Sender: TObject);        {Backup anlegen}
begin
  btnTag.Tag:=0;
  ComboBox1.Color:=clDefault;
  BackupSpeichern;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);    {Statistiken}
begin
  Chart1.ZoomFull;
  if RadioGroup1.Tag=0 then begin
    XMLPropStorage1.StoredValue['LastEval']:=IntToStr(RadioGroup1.ItemIndex);
    Statistik;
  end else
    RadioGroup1.Tag:=0;                                {keine 70%-Analyse}
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);    {Tagesauswertung}
begin
  XMLPropStorage1.StoredValue['Tagesanzeige']:=IntToStr(RadioGroup2.ItemIndex);
  TagStat;    {Tagesstatistik neu zeichnen}
  MenuItem3.Enabled:=(RadioGroup2.ItemIndex=0);
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);    {Zeitachse Skalierung}
var SortList: TStringList;

  procedure FillCB(b: integer);                        {Selektionsliste füllen}
  var x: integer;
  begin
    SortList.Clear;
    for x:=0 to OutList.Count-1 do Sortlist.Add(copy(OutList[x], 1, b));
    ComboBox4.Items.Assign(SortList);
    ComboBox5.Items.Assign(SortList);
    ComboBox4.Text:=ComboBox4.Items[ComboBox4.Items.Count-1]; {letzten Wert anzeigen}
    ComboBox5.text:=ComboBox4.Text;
  end;

begin
  if OutList.Count>0 then begin
    SortList:=TStringList.Create;                      {sortierte Hilfsliste}
    SortList.Sorted:=true;
    SortList.Duplicates:=dupIgnore;                    {Entfernen der Duplikate}
    try
      ComboBox4.Enabled:=true;
      ComboBox4.Font.Color:=clDefault;
      ComboBox5.Visible:=false;
      case RadioGroup3.ItemIndex of
        0: begin
             FillCB(10);                               {Tag}
             ComboBox5.Visible:=true;
             ComboBox4.Font.Color:=ColorButton1.ButtonColor;
             ComboBox5.Font.Color:=ColorButton2.ButtonColor;
           end;
        1: FillCB(7);                                  {Monat}
        2: FillCB(4);                                  {Jahr}
        3: ComboBox4.Enabled:=false;                   {nix zu wählen}
      end;
    finally
      SortList.Free;
    end;
  end;
end;

procedure TForm1.RadioGroup6Click(Sender: TObject);    {Wechselrichter Type}
begin
  btnTag.Tag:=0;                                      {Restore rücksetzen}
  btnTag.Visible:=(RadioGroup6.ItemIndex=0);
  Timer1.Enabled:=false;
  ComboBox1.Color:=clDefault;
  XMLPropStorage1.StoredValue['InverterType']:=IntToStr(RadioGroup6.ItemIndex);
  OutList.Clear;
  DayList.Clear;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);     {Tagesauswertung}
begin
  DateEdit1.Date:=SDToTime(ComboBox1.Text);
  if RadioGroup3.ItemIndex=0 then begin
    ComboBox4.Text:=ComboBox1.Text;
    ComboBox5.Text:=ComboBox1.Text;
  end;
  TagStat;    {Tagesstatistik neu zeichnen}
end;

procedure TForm1.ComboBox2Change(Sender: TObject);     {Monatsauswertung}
begin
  MonStat;
end;

procedure TForm1.ComboBox3Change(Sender: TObject);     {Jahreserträge}
begin
  JahrStat;
end;

procedure TForm1.ComboBox4Change(Sender: TObject);     {Selektion ändern}
begin
  ComboBox5.Text:=ComboBox4.Text;
  if RadioGroup3.ItemIndex>0 then SpezAnalyse;
end;

procedure TForm1.ComboBox5Change(Sender: TObject);     {2. Tag ändern}
begin
  SpezAnalyse;
end;

procedure TForm1.DateEdit1AcceptDate(Sender: TObject; var ADate: TDateTime;
  var AcceptDate: Boolean);
var s: string;
    p: integer;
begin
  if AcceptDate then begin
    s:=FormatDateTime(isoday, ADate);
    p:=ComboBox1.Items.IndexOf(s);
    if p>=0 then ComboBox1.Text:=ComboBox1.Items[p] else begin
      if s>ComboBox1.Items[ComboBox1.Items.Count-1] then
        ComboBox1.Text:=ComboBox1.Items[ComboBox1.Items.Count-1];
      if s<ComboBox1.Items[0] then ComboBox1.Text:=ComboBox1.Items[0];
    end;
    if RadioGroup3.ItemIndex=0 then begin
      ComboBox4.Text:=ComboBox1.Text;
      ComboBox5.Text:=ComboBox1.Text;
    end;
    TagStat;        {Tagesstatistik neu zeichnen}
  end;
end;

{Zum Umwandeln der eingelesenen Daten in ein Ausgabeformat (auch String):
 Aus einem Integer String eine Kommazahl machen, auch als string:
 s: Inputstring, p: Anzahl Stellen nach dem Komma, k: Kommazeichen}
function MakeFloatStr(s: string; const p: integer; const k: char): string;
begin
  result:=s;
  while length(result)<(p+1) do result:='0'+result;  {führende Nullen einfügen}
  insert(k, result, length(result)-p+1);
end;

{Indexierung, um Tagesdaten schneller in der OutList zu finden. 
Geht aber auch mit Monat oder Jahr.
Format IDXList (zzzzzz - Zeilennummer in OutList, Position: 11, default: 0):
YYYY-MM-DDzzzzzz
2012-05-110
2012-05-12954
...
2012-05-2311632
...}
function TForm1.GetOutListIDX(s: string): integer; {Zeilennummer ermitteln}
var x: integer;
begin
  result:=0;                                         {default}
  try
    for x:=0 to IDXList.Count-1 do
      if copy(IDXList[x], 1, length(s))=s then begin
        result:=StrToInt(copy(IDXList[x], 11, length(IDXList[x])-10));
        break;
      end;
  except
    result:=0;
  end;
end;

procedure QuickSort(var A: array of Integer; iLo, iHi: Integer);
var
  Lo, Hi, Pivot, Tmp: Integer;
begin
  Lo:=iLo;
  Hi:=iHi;
  Pivot:=A[(Lo + Hi) div 2];
  repeat
    while A[Lo]<Pivot do Inc(Lo);
    while A[Hi]>Pivot do Dec(Hi);
    if Lo <= Hi then begin
      Tmp:=A[Lo];                                  {temporär zwischenspeichern}
      A[Lo]:=A[Hi];
      A[Hi]:=Tmp;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo>Hi;
  if Hi>iLo then QuickSort(A, iLo, Hi);
  if Lo<iHi then QuickSort(A, Lo, iHi);
end;

{Tabelle mit Werten in eine HTML-Datei schreiben. Es müssen dort >30 Leerzeilen
 reserviert werden, die dann überschrieben werden können. Die Zeile, ab der
 überschrieben wird, ist administrierbar. Damit ist es möglich, beliebige
 HTML-Seiten (Layout auch mit CSS) mit der Wertetabelle zu versorgen.
 Außerdem werden Inputdateien für sonnenertrag.eu erzeugt, wenn eingestellt}

procedure TForm1.OutListLesen(crf: boolean);
const intgt='<td style="color:#009900;">';       {grün: im Soll}
      outgt='<td style="color:#990000;">';       {rot: Soll (noch) nicht erreicht}

var SplitList, SortList, ProtHTM, MonList, FileList: TStringList;
    x, pla, maxlei, maxld, maxert, w, l: integer;
    ter, mer, lastmer, ger, anzt, anztm, anzts: integer;
    ltag, hstr, s: string;
    soll: double;

begin
  Application.ProcessMessages;
  maxert:=0;
  maxlei:=0;
  maxld:=0;
  ter:=0;    {Tagesertrag}
  mer:=0;    {Monatsertrag}
  lastmer:=0;
  ger:=0;    {gesamtertrag}
  anzt:=DayList.Count;   {Anzahl Tage gesamt}
  anztm:=1;  {Anzahl Tage im Monat}
  if OutList.Count>1 then begin
    SplitList:=TStringList.Create; {Hilfsliste zum Aufsplitten separierter Daten}
    SplitList.Delimiter:=sep;
    SortList:=TStringList.Create;  {sortierte Hilfsliste zum Entfernen der Duplikate}
    SortList.Sorted:=true;
    SortList.Duplicates:=dupIgnore;
    ProtHTM:=TStringList.Create;
    MonList:=TStringList.Create;
    FileList:=TStringList.Create;
    try
      pla:=StrToInt(edPeak.Text);
{Daten für Sonnenertrag.eu bereitstellen}
      edPeak.Tag:=0;                     {0 Strings (PV-Arrays) annehmen}
      Shape1.Brush.Color:=clDefault;
      Shape2.Brush.Color:=clDefault;
      Shape3.Brush.Color:=clDefault;
      for x:=OutList.Count-1 downto 0 do begin {ermitteln, wv. Strings vorhanden sind}
        try
          SplitList.DelimitedText:=OutList[x];
          if StrToInt(SplitList[2])>100 then begin     {> 100W zur Sicherheit}
            if SplitList[5]>'0' then begin
              edPeak.Tag:=edPeak.Tag+1;
              Shape1.Brush.Color:=ColorButton3.ButtonColor;        {String 1}
            end else LabeledEdit17.Text:='0';
            if SplitList[7]>'0' then begin
              edPeak.Tag:=edPeak.Tag+1;
              Shape2.Brush.Color:=ColorButton4.ButtonColor;        {String 2}
            end else LabeledEdit18.Text:='0';
            if SplitList[9]>'0' then begin
              edPeak.Tag:=edPeak.Tag+1;
              Shape3.Brush.Color:=ColorButton5.ButtonColor;        {String 3}
            end else LabeledEdit19.Text:='0';
            break;   {Abbruch, wenn Zeile mit gewisser Leistung gefunden}
          end;
        except
          StatusBar1.Panels[2].Text:='Datei defekt';
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
        end;
      end;
      Label39.Caption:=rsStrings+': '+IntToStr(edPeak.Tag);
      XMLPropStorage1.StoredValue['NumStrings']:=IntToStr(edPeak.Tag);
{base_vars anlegen, wenn noch nicht vorhanden}
      ltag:=ExtractFilePath(deServer.FileName)+basejs; {Dateinamen festlegen}
      if crf and CheckBox1.Checked and (not(FileExists(ltag)))
             and (deServer.FileName<>'') then begin
        ProtHTM.Add('var AnlagenKWP='+edPeak.Text);
                                             {Beginn und Endezeiten per Monat}
        ProtHTM.Add('var time_start=new Array(7,7,6,6,5,4,4,5,6,7,7,8)');
        ProtHTM.Add('var time_end=new Array(17,18,20,21,21,22,22,21,20,19,17,16)');
        s:='';                               {String für Monatssoll anlegen}
        for x:=0 to 10 do begin
          ter:=Round(StrToIntDef(StringGrid1.Cells[x,1], 0)*100/SumGrid);
          maxert:=maxert+ter;                {11 Monate aufsummieren}
          s:=s+IntToStr(ter)+',';
        end;
        s:=s+IntToStr(100-maxert);           {Rest zu 100 im Dezember}
        maxert:=0;                           {Aufräumen}
        ter:=0;
        ProtHTM.Add('var sollMonth=new Array('+s+')');
        ProtHTM.Add('var SollYearKWP='+IntToStr(Round(GetSJE)));
        ProtHTM.Add('var AnzahlWR=1');
        ProtHTM.Add('var MaxWRP=new Array(1)');
        ProtHTM.Add('MaxWRP[0]=new Array('+IntToStr((pla*120) div 100)+','+
                                           IntToStr((pla*750) div 100)+','+
                                           IntToStr(round(pla/100)*16000)+','+
                                           IntToStr(round(pla/100)*130000)+')');
        ProtHTM.Add('var WRInfo=new Array(1)');
        hstr:=RadioGroup6.Items[RadioGroup6.ItemIndex];  {Inverter Type}
        hstr:='WRInfo[0]=new Array("'+hstr+'","'+
        StatusBar1.Panels[1].Text+'",'+IntToStr(pla)+',0,"'+
        StatusBar1.Panels[0].Text+'",'+
        IntToStr(edPeak.Tag)+',null,null,0,null,1,0,1,1000,null)';
        ProtHTM.Add(hstr);
        hstr:='WRInfo[0][6]=new Array(';
        case edPeak.Tag of             {new array}
          1: hstr:=hstr+'"String 1")';
          2: hstr:=hstr+'"String 1","String 2")';
          3: hstr:=hstr+'"String 1","String 2","String 3")';
        end;
        ProtHTM.Add(hstr);
        hstr:='WRInfo[0][7]=new Array(';
        case edPeak.Tag of             {new array}
          1: hstr:=hstr+'1)';
          2: hstr:=hstr+'1,1)';
          3: hstr:=hstr+'1,1,1)';
        end;
        ProtHTM.Add(hstr);
        ProtHTM.Add('var StatusCodes=new Array(1)');
        ProtHTM.Add('var FehlerCodes=new Array(1)');
        ProtHTM.Add('StatusCodes[0]="----"');
        ProtHTM.Add('FehlerCodes[0]="----"');
        case RadioGroup6.ItemIndex of       {Temperature available in rawdata?}
          0, 1: hstr:='true';
          2, 3: hstr:='false';
        end;
        ProtHTM.Add('var isTemp='+hstr);
        ProtHTM.Add('var DATALOGGER_NAME="PV_Ausw"');
        ProtHTM.Add('var DATALOGGER_VERSION="'+version+'"');
        try
          ProtHTM.SaveToFile(ltag);
          if CheckBox4.Checked then FileList.Add(ltag);
        except
          StatusBar1.Panels[2].Text:=ExtractFileName(ltag)+' '+msgNoSave;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
        end;
        ProtHTM.Clear;
      end;
{Tagesdaten und Monatserträge aus dem Archiv}
      ltag:='';  {letzter Tag}
      if RadioGroup2.Tag<2 then SynMemo1.Lines.Add('');
      soll:=GetSJE*pla*
            StrToIntDef(StringGrid1.Cells[StrToInt(copy(OutList[0],6,2))-1, 1], 0)/
            SumGrid/1000;
      if DayList.Count>0 then begin
        soll:=GetSJE*pla*
              StrToIntDef(StringGrid1.Cells[StrToInt(copy(DayList[0],6,2))-1, 1], 0)/
              SumGrid/1000;
        for x:=0 to DayList.Count-1 do begin
          try
            SplitList.DelimitedText:=DayList[x];
            if (ltag>'') and
               (copy(DayList[x],6,2)<>copy(ltag,6,2)) then begin  {neuer Monat}
              MonList.Insert(0, 'mo[mx++]="'+
                                FormatDateTime('dd.mm.yy', SDToTime(ltag))+
                                '|'+IntToStr(mer)+'"');
              soll:=GetSJE*pla*
                    StrToIntDef(StringGrid1.Cells[StrToInt(copy(DayList[x],6,2))-1, 1], 0)/
                    SumGrid/1000;
              if RadioGroup2.Tag<2 then SynMemo1.Lines.Add(rsSoll+' '+
              {nur am Anfang}           copy(DayList[x],1,7)+': '+
                                        FloatToStrF(soll, ffFixed, 12, 3)+'kWh');
              mer:=0;
            end; {Ende Monat}
            maxld:=StrToInt(SplitList[1]);
            if maxld>maxlei then maxlei:=maxld;    {Peakleistung gesamt}
            ter:=GetKErt(StrToInt(SplitList[2]));
            if ter>maxert then maxert:=ter;
            ger:=ger+ter;                          {Gesamtertrag}
            mer:=mer+ter;                          {Monatsertrag}
            ltag:=copy(DayList[x],1,10);
          except
            StatusBar1.Panels[2].Text:='(1) Eingabedatei defekt, Zeile '+IntToStr(x+1);
            SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          end;
        end;
      end;
      maxld:=0;
      ter:=0;
      if ltag='' then anzt:=1;
{Archive Ende, nahtlos weiter mit OutList auslesen}
      IDXList.Clear;                         {alten Index löschen}
      for x:=0 to OutList.Count-1 do begin   {Gesamtliste für Statistik durchsuchen}
        try
          SortList.Add(copy(OutList[x],1,10)); {Datum Auslesen}
          SplitList.DelimitedText:=OutList[x];
          l:=StrToInt(SplitList[2]);
          if l>maxlei then maxlei:=l;          {Peakleistung gesamt}
          if l>maxld then maxld:=l;            {Peakleistung für den Tag}
          w:=StrToInt(SplitList[3]);           {Ertragswert in W}
          if copy(OutList[x],1,10)<>ltag then begin   {neuer Tag}
            if ltag<>'' then begin
              ter:=GetKErt(ter);
              mer:=mer+ter;                    {Monatsertrag}
              ger:=ger+ter;                    {Gesamtertrag}
              if ter>1 then ProtHTM.Insert(0, 'da[dx++]="'+
                                FormatDateTime('dd.mm.yy', SDToTime(ltag))+
                                '|'+IntToStr(ter)+';'+IntToStr(maxld)+'"');
              if (ltag>'') and
                 (copy(OutList[x],6,2)<>copy(ltag,6,2)) then begin  {neuer Monat}
                if anztm=DaysInMonth(SDToTime(ltag)) then lastmer:=mer;
                MonList.Insert(0, 'mo[mx++]="'+
                                  FormatDateTime('dd.mm.yy', SDToTime(ltag))+
                                  '|'+IntToStr(mer)+'"');
                soll:=GetSJE*pla*
                      StrToIntDef(StringGrid1.Cells[StrToInt(copy(OutList[x],6,2))-1, 1], 0)/
                      SumGrid/1000;
                if RadioGroup2.Tag<2 then SynMemo1.Lines.Add(rsSoll+' '+
                                          copy(OutList[x],1,7)+': '+
                                          FloatToStrF(soll, ffFixed, 12, 3)+'kWh');
                mer:=0;
                anztm:=0;
              end;                             {Monat}
              inc(anzt);
              inc(anztm);
              ter:=0;
              maxld:=0;                        {Peakleitung am Tag}
            end;
            IDXList.Add(copy(OutList[x],1,10)+IntToStr(x));
            ltag:=copy(OutList[x],1,10);
          end;                                 {Tag}
          if w>maxert then maxert:=w;
          if w>ter then ter:=w;                {Tagesertrag}
        except
          StatusBar1.Panels[2].Text:='(2) Eingabedatei defekt, Zeile '+IntToStr(x+1);
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
        end;
      end;
      if RadioGroup2.Tag<2 then SynMemo1.Lines.Add('');
      anzts:=DaysInYear(SDToTime(ltag));     {Solltage letztes Jahr für
                                              spezifischen Jahresertrag}
      ter:=GetKErt(ter);
      mer:=mer+ter;                          {Monatsertrag}
      ger:=ger+ter;                          {Gesamtertrag}
      ProtHTM.Insert(0, 'da[dx++]="'+FormatDateTime('dd.mm.yy',SDToTime(ltag))+
                        '|'+IntToStr(ter)+';'+IntToStr(maxld)+'"');
      MonList.Insert(0, 'mo[mx++]="'+FormatDateTime('dd.mm.yy',SDToTime(ltag))+
                        '|'+IntToStr(mer)+'"');
      if crf and CheckBox1.Checked and
         (deServer.FileName<>'') then begin  {sonnenertrag.eu}
        hstr:=ExtractFilePath(deServer.FileName)+monsjs;
        try
          MonList.SaveToFile(hstr);
          if CheckBox4.Checked then FileList.Add(hstr);
        except
          StatusBar1.Panels[2].Text:=ExtractFileName(hstr)+' '+msgNoSave;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
        end;
        MonList.Clear;    {Anzahl Tage in daysjs reduzieren}
        if (not Application.HasOption('a', 'sendall')) and {nur letzte 31 Tage}
           (ProtHTM.Count>31) then for x:=0 to 30 do MonList.Add(ProtHTM[x])
                              else MonList.Assign(ProtHTM);     {alles}
        hstr:=ExtractFilePath(deServer.FileName)+daysjs;
        try
          MonList.SaveToFile(hstr);
          if CheckBox4.Checked then FileList.Add(hstr);
        except
          StatusBar1.Panels[2].Text:=ExtractFileName(hstr)+' '+msgNoSave;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
        end;
        ProtHTM.Clear;

{min_day.js: Intervallmesswerte
m[mi++]="Datum Uhrzeit|AC Leistung;
DC Leistung String 1;DC Leistung String 2;DC Leistung String 3;AC Tagesertrag;
DC Spannung String 1;DC Spannung String 2;DC Spannung String 3;WR Temperatur"

- Leistungen in ganzen W
- DC Spannungen in ganzen V --> Round(spg/10)
- Ertrag in ganzen Wh
- Temperatur in ganzen °C

 m[mi++]="15.02.09 17:40:00|0;6;5200;202;21"}
        for x:=OutList.Count-1 downto 0 do begin
          try
            SplitList.DelimitedText:=OutList[x];
            if SplitList[0]<>FormatDateTime(isoday, now) then break;
            ltag:=SplitList[1];
            if (ltag[5]='0') or (ltag[5]='5') then begin
              hstr:='m[mi++]="'+
                    FormatDateTime('dd.mm.yy', SDToTime(SplitList[0]))+' '+
                    FormatDateTime('hh:nn:ss', TToTT(ltag))+'|'+
                    SplitList[2]+';';              {AC Leistung}
              case edPeak.Tag of             {DC Leistungen Strings}
                1: hstr:=hstr+IntToStr(Round(StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000))+';';
                2: hstr:=hstr+IntToStr(Round(StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000))+';'+
                              IntToStr(Round(StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000))+';';
                3: hstr:=hstr+IntToStr(Round(StrToInt(SplitList[5])*StrToInt(SplitList[6])/10000))+';'+
                              IntToStr(Round(StrToInt(SplitList[7])*StrToInt(SplitList[8])/10000))+';'+
                              IntToStr(Round(StrToInt(SplitList[9])*StrToInt(SplitList[10])/10000))+';';
              end;
              hstr:=hstr+SplitList[3]+';';         {AC Ertrag}
              case edPeak.Tag of             {DC Spannungen Strings}
                1: hstr:=hstr+IntToStr(Round(StrToInt(SplitList[5])/10))+';';
                2: hstr:=hstr+IntToStr(Round(StrToInt(SplitList[5])/10))+';'+
                              IntToStr(Round(StrToInt(SplitList[7])/10))+';';
                3: hstr:=hstr+IntToStr(Round(StrToInt(SplitList[5])/10))+';'+
                              IntToStr(Round(StrToInt(SplitList[7])/10))+';'+
                              IntToStr(Round(StrToInt(SplitList[9])/10))+';';
              end;
              ProtHTM.Add(hstr+SplitList[4]+'"');  {und noch Temperatur}
            end;
          except
            StatusBar1.Panels[2].Text:='(3) Eingabedatei defekt, Zeile '+IntToStr(x+1);
            SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          end;
        end;
        if ProtHTM.Count>0 then begin            {min_day.js speichern}
          hstr:=ExtractFilePath(deServer.FileName)+histjs;
          try
            ProtHTM.SaveToFile(hstr);
            if CheckBox4.Checked then FileList.Add(hstr);
          except
            StatusBar1.Panels[2].Text:=ExtractFileName(hstr)+' '+msgNoSave;
            SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          end;
        end;
        ProtHTM.Clear;
      end;
      RadioGroup3.ItemIndex:=0;
      ComboBox1.Items.Assign(SortList);             {ComboBox1: Tage füllen}
      ComboBox4.Items.Assign(SortList);
      ComboBox5.Items.Assign(SortList);
      ComboBox1.Text:=ComboBox1.Items[ComboBox1.Items.Count-1]; {letzten Wert anzeigen}
      ComboBox4.Text:=ComboBox4.Items[ComboBox4.Items.Count-1]; {letzten Wert anzeigen}
      ComboBox5.Text:=ComboBox4.Text;
      DateEdit1.Enabled:=(ComboBox1.Items.Count>0);
      DateEdit1.Date:=SDToTime(ComboBox1.Text);
      SortList.Clear;
      if DayList.Count>0 then
        for x:=0 to DayList.Count-1 do Sortlist.Add(copy(DayList[x], 1, 7));
      for x:=0 to ComboBox1.Items.Count-1 do        {ComboBox2: Monate auflisten}
        Sortlist.Add(copy(ComboBox1.Items[x], 1, 7));
      ComboBox2.Items.Assign(SortList);
      ComboBox2.Text:=ComboBox2.Items[ComboBox2.Items.Count-1]; {letzten Wert anzeigen}
      SortList.Clear;
      if DayList.Count>0 then
        for x:=0 to DayList.Count-1 do Sortlist.Add(copy(DayList[x], 1, 4));
      for x:=0 to ComboBox1.Items.Count-1 do        {ComboBox3: Jahre auflisten}
        Sortlist.Add(copy(ComboBox1.Items[x], 1, 4));
      ComboBox3.Items.Assign(SortList);
      ComboBox3.Text:=ComboBox3.Items[ComboBox3.Items.Count-1]; {letzten Wert anzeigen}
      XMLPropStorage1.StoredValue['InverterName']:=StatusBar1.Panels[0].Text;
      XMLPropStorage1.StoredValue['InverterID']:=StatusBar1.Panels[1].Text;
      Application.ProcessMessages;
      TagStat;    {Alle Diagramme füllen}
      MonStat;
      JahrStat;
      Statistik;  {Default Statistik ausführen}
      Label35.Caption:=FloatToStr(ger/1000)+'kWh';
      Label37.Caption:=FloatToStr(maxlei/1000)+' kWp'; {Peakleistung über alles}
      Label34.Caption:=FloatToStrF(ger/pla, ffFixed, 12, 3)+'kWh/kWp';
      Label32.Caption:=FloatToStrF(ger/1000*Geteevg, ffFixed, 12, 2)+edEuro.Text;
      Label30.Caption:=FloatToStrF(ger/pla/anzt*anzts, ffFixed, 12, 3)+'kWh/kWp';
      SynMemo1.Lines.Add(IntToStr(anzt)+' Tage aufgezeichnet durch '+
                         StatusBar1.Panels[0].Text);
      SynMemo1.Lines.Add('');
{HTML Protokoll schreiben: 31 Zeilen in die vorhandene Datei ab Zeile xx (einstellbar)}
      if crf and (edHTMLzeile.Value>0) then begin      {eingestellte Zeile=0 --> nix tun}
        if FileExists(deServer.FileName) then begin
          try
            ProtHTM.LoadFromFile(deServer.FileName);
          except
            ProtHTM.Clear;   {nichts zum Laden gefunden}
          end;

(* Dieser Part (HTML Generierung) in cpvausw for CRON job update:

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

Muss ggf. mit geändert werden!
*)

          if ProtHTM.Count>(edHTMLzeile.Value+33) then begin  {Tabelle mit Defaultwerten schreiben}
            s:=' <table style="border-width:0;width:90%;margin-left:auto;margin-right:auto;">';
            ProtHTM[edHTMLzeile.Value+ 0]:=s+'  <!-- '+ExtractFileName(paramstr(0))+
                                         '  '+version+' -->';
            ProtHTM[edHTMLzeile.Value+ 1]:=' <colgroup><col style="width:45%;"><col style="width:10%;"><col style="width:45%;"></colgroup>';
            ProtHTM[edHTMLzeile.Value+ 2]:='   <tr><td style="font-size:1.5em;">'+rsWR+'</td><td></td>';
            ProtHTM[edHTMLzeile.Value+ 3]:='       <td style="font-size:1.5em;">'+
                              StatusBar1.Panels[1].Text+'</td></tr>';
            ProtHTM[edHTMLzeile.Value+ 4]:='   <tr><td style="height:15px;"><hr></td><td></td>';
            ProtHTM[edHTMLzeile.Value+ 5]:='       <td style="height:15px;"><hr></td></tr>';
            ProtHTM[edHTMLzeile.Value+ 6]:='   <tr><td>'+rsProjekt1+rsPeakL+':</td><td></td>';
            ProtHTM[edHTMLzeile.Value+ 7]:='       <td>'+
                              FloatToStrF(pla/1000, ffFixed, 12, 3)+
                              ' kW<sub>p</sub></td></tr>';
            if maxlei<pla then hstr:=outgt else hstr:=intgt; {Farben einstellen}
            ProtHTM[edHTMLzeile.Value+ 8]:='   <tr><td>'+rsErreich1+rsPeakL+':</td><td></td>';
            ProtHTM[edHTMLzeile.Value+ 9]:='       '+hstr+FloatToStr(maxlei/1000)+
                              ' kW<sub>p</sub></td></tr>';
            ProtHTM[edHTMLzeile.Value+10]:='   <tr><td>'+rsProjekt2+rsSpezJE+':</td><td></td>';
            ProtHTM[edHTMLzeile.Value+11]:='       <td>'+edSpezJE.Text+
                                         ' kWh/kW<sub>p</sub></td></tr>';
            if (ger/pla/anzt*anzts)<GetSJE then hstr:=outgt else hstr:=intgt;
            ProtHTM[edHTMLzeile.Value+12]:='   <tr><td>'+rsErreich2+rsSpezJE+':</td><td></td>';
            ProtHTM[edHTMLzeile.Value+13]:='       '+hstr+
                              FloatToStrF(ger/pla/anzt*anzts, ffFixed, 12, 3)+
                              ' kWh/kW<sub>p</sub></td></tr>';
            if ger>mwswt then begin   {Gesamtertrag in kWh oder MWh}
              hstr:=' MWh';
              w:=1000000;
            end else begin
              hstr:=' kWh';
              w:=1000;
            end;
            ProtHTM[edHTMLzeile.Value+14]:='   <tr><td>Gesamtertrag bisher:</td><td></td>';
            ProtHTM[edHTMLzeile.Value+15]:='       <td><b>'+
                               FloatToStrF(ger/w, ffFixed, 12, 3)+
                               hstr+'</b></td></tr>';
            ProtHTM[edHTMLzeile.Value+16]:='   <tr><td>'+rsErtrag+' im letzten Monat:</td><td></td>';
            ProtHTM[edHTMLzeile.Value+17]:='       <td>'+FloatToStr(lastmer/1000)+
                              ' kWh ('+FloatToStrF(lastmer/pla, ffFixed, 12, 3)+
                              ' kWh/kW<sub>p</sub>)</td></tr>';
            if (mer/1000)<soll then hstr:=outgt else hstr:=intgt;
            ProtHTM[edHTMLzeile.Value+18]:='   <tr><td>'+rsErtrag+' im aktuellen Monat:</td><td></td>';
            ProtHTM[edHTMLzeile.Value+19]:='       '+hstr+'<b>'+
                              FloatToStr(mer/1000)+' kWh</b> ('+rsSoll+
                              ': '+FloatToStrF(soll, ffFixed, 12, 3)+
                              ' kWh)</td></tr>';
            ProtHTM[edHTMLzeile.Value+20]:='   <tr><td>Bisher h&ouml;chster '+rsTagErtr+':</td><td></td>';
            ProtHTM[edHTMLzeile.Value+21]:='       <td>'+FloatToStr(maxert/1000)+
                              ' kWh ('+FloatToStrF(maxert/pla, ffFixed, 12, 3)+
                              ' kWh/kW<sub>p</sub>)</td></tr>';
            ProtHTM[edHTMLzeile.Value+22]:='   <tr><td>Durchschnittlicher '+rsTagErtr+':</td><td></td>';
            ProtHTM[edHTMLzeile.Value+23]:='       <td>'+
                              FloatToStrF(ger/anzt/1000, ffFixed, 12, 3)+
                              ' kWh ('+FloatToStrF(ger/anzt/pla, ffFixed, 12, 3)+
                              ' kWh/kW<sub>p</sub>)</td></tr>';
            soll:=soll/DaysInAMonth(StrToInt(copy(OutList[OutList.Count-1],1,4)),
                                    StrToInt(copy(OutList[OutList.Count-1],6,2)));
            if (ter/1000)<soll then hstr:=outgt else hstr:=intgt;
            ProtHTM[edHTMLzeile.Value+24]:='   <tr><td>'+rsTagErtr+' '+
                              FormatDateTime('dddd, dd. mmm. yyyy (hh:nn',
                              SDToTime(copy(OutList[OutList.Count-1],1,10))+
                              TToTT(copy(OutList[OutList.Count-1],12,5)))+
                              'h):</td><td></td>';
            ProtHTM[edHTMLzeile.Value+25]:='       '+hstr+'<b>'+
                              FloatToStr(ter/1000)+' kWh</b> ('+rsSoll+
                              ': '+FloatToStrF(soll, ffFixed, 12, 3)+
                              ' kWh)</td></tr>';
            hstr:=ExtractFileName(ChangeFileExt(deServer.FileName, ''));
            ProtHTM[edHTMLzeile.Value+26]:=' </table>';
            ProtHTM[edHTMLzeile.Value+27]:=s;        {Table 90% in HTML}
            ProtHTM[edHTMLzeile.Value+28]:='   <tr><td><p><img src="'+hstr+
              '1.png" style="width:45%;float:left;" alt="'+rsTagErtr+'">';
            ProtHTM[edHTMLzeile.Value+29]:='   <img src="'+hstr+
              '2.png" style="width:45%;float:right;" alt="'+rsMonErtr+'"></p></td></tr>';
            ProtHTM[edHTMLzeile.Value+30]:=' </table>';
            try
              ProtHTM.SaveToFile(deServer.FileName);
              if CheckBox4.Checked then FileList.Add(deServer.FileName);
            except
              SynMemo1.Lines.Add('Could not save '+deServer.FileName);
            end;
            pcMain.ActivePageIndex:=0;
            hstr:=ChangeFileExt(deServer.FileName, '')+'1.png'; {Bilddateiname}
            Chart4.SaveToFile(TPortableNetworkGraphic, hstr);
            if CheckBox4.Checked then FileList.Add(hstr);
            pcMain.ActivePageIndex:=1;
            hstr:=ChangeFileExt(deServer.FileName, '')+'2.png'; {Bilddateiname}
            Chart2.SaveToFile(TPortableNetworkGraphic, hstr);
            if CheckBox4.Checked then FileList.Add(hstr);
            pcMain.ActivePageIndex:=pcMain.Tag;
          end;                                     {Ende HTML-Generierung}

        end else if CheckBox4.Checked and          {Datei lokal nicht vorhanden}
                    (not Application.HasOption('n', 'noftp'))  then begin
          FTPAnzeige(clGreen);                     {FTP Download anstoßen}
          try
            FTPloginHP;
            IdFTP1.Get(edHTMLfile.Text,deServer.FileName,
                       false, IdFTP1.ResumeSupported);
            FTPlogout;
            StatusBar1.Panels[2].Text:=edHTMLfile.Text+' downloaded';
            SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          except
            StatusBar1.Panels[2].Text:=msgHTMLdl;
            SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
            FTPAnzeige(clRed);
          end;
        end;
      end;   {HTML-Prot Ende}
      if (FileList.Count>0) and                    {FTP File Upload anstossen}
         (not Application.HasOption('n', 'noftp'))  then begin
        FTPAnzeige(clBlue);
        try
          FTPloginHP;
          for x:=0 to FileList.Count-1 do begin
            IdFTP1.Put(FileList[x], ExtractFileName(FileList[x]), false);
            StatusBar1.Panels[2].Text:=ExtractFileName(FileList[x])+' uploaded';
            SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          end;
          FTPlogout;
        except
          StatusBar1.Panels[2].Text:=rsFTPUploadnm;
          SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          FTPAnzeige(clRed);
        end;
      end;
      Form1.Refresh;
    finally
      SortList.Free;
      SplitList.Free;
      FileList.Free;
      ProtHTM.Free;
      MonList.Free;
    end;
  end;     {nichts tun, wenn keine Daten gefunden (OutList.Count<2 Zeilen)}
end;

procedure TForm1.Auswerten;   {Auswertung der Dateien vom WR -> in Rohdaten schreiben}
var
  FileList, InList, SplitList: TStringList;
  x, y: integer;
  dr: string;

  procedure AuswertenDanfoss; {Danfoss/IBC TLX Pro Daten in Rohdaten umwandlen}
  var x, y, korre, ertr: integer;
      infoon, dataon: boolean;
      rs, ldat: string;
  begin
    SplitList.StrictDelimiter:=false;
    korre:=0;    {Korrekturwert bei nicht zurückgesetztem Ertrag am nächsten Tag}
    infoon:=false;
    dataon:=false;
    ldat:='';
    for x:=0 to FileList.Count-1 do begin  {alle gefunden Dateien bearbeiten}
      StatusBar1.Panels[2].Text:=FileList[x];
      InList.LoadFromFile(FileList[x]);
      for y:=0 to InList.Count-1 do begin  {eine Datei auswerten}
        if InList[y]=infend then infoon:=false;
        if InList[y]=datend then begin
          dataon:=false;
          break;              {neu: Abbrechen, wenn Dataend --> testen}
        end;
        if infoon then begin  {Sektion "wr_def" einlesen (eine Zeile)}
          SplitList.DelimitedText:=InList[y];
          if SplitList.Count>4 then begin
            StatusBar1.Panels[0].Text:=SplitList[3]; {Name}
            StatusBar1.Panels[1].Text:=SplitList[4]; {WR Nummer}
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
            if length(rs)>7 then ertr:=0 else ertr:=StrToInt(rs);
            if ldat<>SplitList[1] then begin         {neuer Tag}
              korre:=0;
              if (ldat>'') and (ertr>0) and
                 (InList.Count>10) then korre:=ertr; {Korrekturwert erstellen}
              ldat:=SplitList[1];
            end;
            if (ertr>0) and (ertr<korre) then korre:=0;     {wegen 1.}
            if ertr>=korre then ertr:=ertr-korre;           {1. und 2.}
            if (length(SplitList[4])<8) and          {nicht wenn Ertrag defekt}
               ((length(SplitList[12])>3) or CheckBox13.Checked) then begin
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
                  SplitList[0]+sep+     {Intervall}
                  Splitlist[31];        {Isolationswiderstand}
              rs:=StringReplace(rs, sep+'0.', sep, [rfReplaceAll]);
              rs:=StringReplace(rs, '.', '', [rfReplaceAll]);
              rs:=StringReplace(rs, sep+'000', sep+'0', [rfReplaceAll]);
              OutList.Add(rs);          {rawdata anlegen}
            end;
            if SplitList[20]<>'0' then begin  {E_WR - Error Inverter}
              StatusBar1.Panels[2].Text:=SplitList[1]+' '+SplitList[2]+
                                         'h - '+rsErrorInv+': Code '+
                                         SplitList[20];
              SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
            end;
          end;  {ende if (SplitList.Count>30)}
        end;
        if InList[y]=datbeg then
          dataon:=true;
        if InList[y]=infbeg then
          infoon:=true;
      end;    {Ende eine Datei auswerten}
      ProgressBar1.Position:=x;
      Application.ProcessMessages;
    end;  {Ende alle Dateien durchsuchen}
  end;

  procedure AuswertenDanfossMigration; {Downloads vom Danfoss DW in Rohdaten umwandlen}
  var x, y, korre, ertr: integer;
      rs, ldat, tdat: string;
	
    function ClMig(s: string): string;  {Clean String from Migration}
    begin
      result:=trim(StringReplace(s, '"', ' ', [rfReplaceAll]));
    end;	
	  
  begin
    SplitList.Delimiter:=#09;            {Tabulator}
    SplitList.StrictDelimiter:=true;
    korre:=0;    {Korrekturwert bei nicht zurückgesetztem Ertrag am nächsten Tag}
    ldat:='';
    for x:=0 to FileList.Count-1 do begin  {alle gefunden Dateien bearbeiten}
      StatusBar1.Panels[2].Text:=FileList[x];
      InList.LoadFromFile(FileList[x]);
      SplitList.DelimitedText:=InList[2];
      StatusBar1.Panels[1].Text:=ClMig(SplitList[1]); {WR Nummer}
      StatusBar1.Panels[0].Text:=ClMig(SplitList[2]); {WR Name}
{Zeile 1  --> Überschriften - ignorieren}
      for y:=2 to InList.Count-1 do begin  {eine Datei auswerten}
        SplitList.DelimitedText:=InList[y];
        {nur Datenzeilen auslesen, ohne Überschrift}
        if SplitList.Count>30 then begin
{Korrektur Danfossfehler: 1. Ertrag vom Vortag, später erst zurückgesetzt,
                             aber Spannung AC=0
                          2. Ertrag nicht zurückgesetzt, sondern wird weiter
                             aufaddiert}
          tdat:=copy(ClMig(SplitList[0]), 1, 10);
          rs:=StringReplace(SplitList[6], ',', '', [rfReplaceAll]);
          ertr:=StrToInt(rs);
          if ldat<>tdat then begin         {neuer Tag}
            korre:=0;
            if (ldat>'') and (ertr>0) and
               (InList.Count>10) then korre:=ertr; {Korrekturwert erstellen}
            ldat:=tdat;
          end;
          if (ertr>0) and (ertr<korre) then korre:=0;     {wegen 1.}
          if ertr>=korre then ertr:=ertr-korre;           {1. und 2.}
          if (length(ClMig(SplitList[10]))>4) or CheckBox13.Checked then begin
            rs:=tdat+sep+                                 {Datum}
                copy(ClMig(SplitList[0]), 12, 5)+sep+     {Zeit}
                SplitList[5]+sep+                  {Leistung}
                IntToStr(ertr)+sep+                {Energieertrag}
                SplitList[7]+sep+                  {Temperatur Wechselrichter}
                SplitList[23]+sep+                 {Spannung Strang 1}
                SplitList[24]+sep+                 {Strom Strang 1}
                SplitList[25]+sep+                 {Spannung Strang 2}
                SplitList[26]+sep+                 {Strom Strang 2}
                SplitList[27]+sep+                 {Spannung Strang 3}
                SplitList[28]+sep+                 {Strom Strang 3}
                SplitList[8]+sep+                  {Spannung AC}
                SplitList[10]+sep+                 {Frequenz}
                SplitList[4]+sep+                  {Intervall}
                Splitlist[31];                     {Isolationswiderstand}
            rs:=StringReplace(rs, sep+'0,', sep, [rfReplaceAll]);
            rs:=StringReplace(rs, ',', '', [rfReplaceAll]);
            rs:=StringReplace(rs, sep+'000', sep+'0', [rfReplaceAll]);
            OutList.Add(rs);                       {rawdata anlegen}
          end;
          if SplitList[30]<>'0' then begin  {E_WR - Error Inverter}
            StatusBar1.Panels[2].Text:=tdat+' '+copy(ClMig(SplitList[0]), 12, 8)+
                                       'h - '+rsErrorInv+': Code '+
                                       SplitList[30];
            SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
          end;
        end;  {ende if (SplitList.Count>30)}
      end;    {Ende eine Datei auswerten}
      ProgressBar1.Position:=x;
      Application.ProcessMessages;
    end;  {Ende alle Dateien durchsuchen}
  end;
  
{Tool zum Visualisieren der CSV aus Kaco Powador 14,0 TL3 ?
 http://www.photovoltaikforum.com/wechselrichter-f3/
        tool-zum-visualisieren-der-csv-aus-kaco-powador-14-t85362.html}
  procedure AuswertenKaco;
  var x: integer;
      nam, mon: string;
      zp, zpa: TDateTime;
      EListM: array[1..31] of integer;   {Tageserträge, 31 Tage}

    procedure GetWRInfo(p: integer);     {Jahresdatei, nur WR-Metadaten holen}
    begin
      InList.LoadFromFile(FileList[p]);
      SplitList.DelimitedText:=InList[1];
      StatusBar1.Panels[0].Text:=SplitList[0];     {Name}
      StatusBar1.Panels[1].Text:=SplitList[1];     {WR Nummer}
    end;

    function GetErtrag(p: integer): string;  {Monatsdatei mit Tagesertrag}
    var x, d: integer;
        s: string;
    begin
      result:='';                                  {Fehler ID}
      for x:=low(EListM) to high(EListM) do 
	    EListM[x]:=0;                              {Ertrags-Array löschen}
      InList.LoadFromFile(FileList[p]);
      try
        for x:=0 to InList.Count-1 do begin
          s:=InList[x];
          if (s[3]='/') and (s[6]='/') then begin
            d:=StrToInt(copy(s, 1, 2));            {Tag merken}
            delete(s, 1, 11);
            EListM[d]:=StrToInt(s);
          end;
        end;
        result:=copy(nam, 1, 6);
      except
        SynMemo1.Lines.Add('Format error in file '+nam);
      end;
    end;

    procedure FillOutList(p: integer);
    var x, itv, d: integer;
        ee: double;
        rs: string;
    begin
      InList.LoadFromFile(FileList[p]);
      ee:=0;
      try
        d:=StrToInt(copy(nam, 7, 2));
        for x:=0 to InList.Count-1 do begin
          rs:=InList[x];
          if (rs[3]=':') and (rs[6]=':') then begin
            rs:=StringReplace(rs, '.', '', [rfReplaceAll]);
            SplitList.DelimitedText:=rs;
            zp:=StrToTime(SplitList[0]);
            itv:=SecondsBetween(zpa, zp);          {Intervall berechnen}
            if (itv<1) or (itv>800) then itv:=defint;
            ee:=ee+StrToInt(SplitList[14])*itv/3600; {Ertrag integrieren}
            if (EListM[d]>0) and (ee>EListM[d]) and  {Tagesertrag korrigieren}
               (mon=copy(nam, 1, 6)) then ee:=EListM[d];
            rs:=copy(nam, 1, 4)+'-'+copy(nam, 5, 2)+'-'+copy(nam, 7, 2)+sep+
                copy(SplitList[0], 1, 5)+sep+      {Zeit}
                SplitList[14]+sep+                 {Leistung}
                IntToStr(Round(ee))+sep+           {Ertrag oben berechnet}
                IntToStr(Round(StrToInt(SplitList[15])/10))+sep+  {Temperatur}
                SplitList[1]+sep+                                 {UDC 1}
                IntToStr(StrToInt((SplitList[2]+'0')))+sep+       {IDC 1}
                SplitList[4]+sep+                                 {UDC 2}
                IntToStr(StrToInt((SplitList[5]+'0')))+sep+       {IDC 2}
                nix+nix+                                          {UDC 3, IDC 3}
                IntToStr(Round((StrToInt(SplitList[7])+
                                StrToInt(SplitList[9])+
                                StrToInt(SplitList[11]))/30))+sep+ {Spannung AC}
                '5000'+sep+                        {Netzfrequenz}
                IntToStr(itv)+sep+                 {Meßintervall}
                '0';                               {Isolationswiderstand}
            zpa:=zp;                               {Zeitpunkt merken}
            OutList.Add(rs);                       {Rawdata anlegen}
          end;
        end;
        if mon=copy(nam, 1, 6) then begin          {Ertrag korrigieren}
          SplitList.DelimitedText:=OutList[OutList.Count-1];
          if StrToInt(SplitList[3])<EListM[d] then begin
            rs:='';
            for x:=0 to 2 do rs:=rs+SplitList[x]+sep;
            rs:=rs+IntToStr(EListM[d])+sep;
            for x:=4 to 13 do rs:=rs+SplitList[x]+sep;
            rs:=rs+SplitList[14];
            OutList[OutList.Count-1]:=rs;   {mit neuem Ertragswert aus Array}
          end;
        end;
      except
        SynMemo1.Lines.Add('Format error in file '+nam);
      end;
    end;

  begin                                            {Auswerten Kaco Main}
    SplitList.StrictDelimiter:=true;
    zpa:=0;
    for x:=low(EListM) to high(EListM) do 
	  EListM[x]:=0;                                {Ertrags-Array löschen}
    for x:=0 to FileList.Count-1 do begin  {alle gefunden Dateien bearbeiten}
      nam:=FileList[x];
      StatusBar1.Panels[2].Text:=nam;
      nam:=ChangeFileExt(ExtractFileName(nam), '');
      case length(nam) of                  {über Länge Dateinamen identifizieren}
        4: GetWRInfo(x);                           {Jahresdatei}
        6: mon:=GetErtrag(x);                      {Monatsdatei}
        8: FillOutList(x);                         {Tagesdaten}
      end;
      ProgressBar1.Position:=x;
      Application.ProcessMessages;
    end;                                   {Ende alle Dateien durchsuchen}
  end;

  procedure AuswertenSMA;                  {Daten vom SMA Sunny Boy}
  var x, y, z, ee, lei, itv: integer;
      zp, zpa: TDateTime;
      dat, rs: string;
  begin
    SplitList.StrictDelimiter:=false;
    ee:=0;
    dat:='';
    zpa:=0;
    for x:=0 to FileList.Count-1 do begin  {alle gefunden Dateien bearbeiten}
      StatusBar1.Panels[2].Text:=FileList[x];
      InList.LoadFromFile(FileList[x]);
      for y:=0 to InList.Count-1 do begin
        rs:=StringReplace(InList[y], ',', '', [rfReplaceAll]); {Kommas entfernen}
        SplitList.DelimitedText:=rs;
        if SplitList.Count>1 then begin
          if pos('SN: ', InList[y])>0 then begin
            rs:=InList[y+1];
            StatusBar1.Panels[0].Text:='';
            for z:=length(rs) downto 1 do if rs[z]<>sep then
              StatusBar1.Panels[0].Text:=rs[z]+StatusBar1.Panels[0].Text
                                                        else break;
            SplitList.DelimitedText:=InList[y+2];
            StatusBar1.Panels[1].Text:=SplitList[1];             {WR Nummer}
          end else if (length(rs)>24) and (rs[1] in ziff) then begin {Daten}
            try
              zp:=StrToTime(SplitList[1]);
              itv:=SecondsBetween(zpa, zp);     {Intervall berechnen}
              if (itv<1) or (itv>800) then itv:=defint;
              if SplitList[0]<>dat then begin   {neuer Tag}
                ee:=StrToInt(SplitList[2]);     {erster Ertrag des Tages}
                dat:=SplitList[0];
              end;
              lei:=StrToInt(SplitList[3]);         {Leistung}
              if (lei>0) or CheckBox13.Checked then
                OutList.Add(copy(SplitList[0], 7, 4)+'-'+
                copy(SplitList[0], 4, 2)+'-'+             {Datum}
                copy(SplitList[0], 1, 2)+sep+
                copy(SplitList[1], 1, 5)+sep+             {Zeit}
                IntToStr(lei)+sep+                        {Leistung}
                IntToStr(StrToInt(SplitList[2])-ee)+sep+  {Energie}
                '18'+sep+                                 {Temperatur}
                nix+nix+                                  {U/I String 1}
                nix+nix+                                  {U/I String 2}
                nix+nix+                                  {U/I String 3}
                '230'+sep+                                {Netzspannung}
                '5000'+sep+                               {Netzfrequenz}
                IntToStr(itv)+sep+                        {Intervall}
                '0');                                     {Isowiderstand}
              zpa:=zp;                                    {Zeitpunkt merken}
            except                                 {Fehler in Rohdaten SMA}
              SynMemo1.Lines.Add('Format error in file '+FileList[x]);
            end;
          end;
        end;
      end;
      ProgressBar1.Position:=x;
      Application.ProcessMessages;
    end;                                   {Ende alle Dateien durchsuchen}
  end;

{Voraussetzung:
 sep=;
 Version CSV1|Tool SMAspot2.0.5|Linebreaks CR/LF|
 Delimiter semicolon|Decimalpoint comma|Precision 3}

  procedure AuswertenSMAspot;              {CSV Daten vom SMA Spot}
  var x, y, z, lei, itv: integer;
      zp, zpa: TDateTime;
      rs: string;
  begin
    SplitList.StrictDelimiter:=false;
    zpa:=0;
    for x:=0 to FileList.Count-1 do begin  {alle gefunden Dateien bearbeiten}
      StatusBar1.Panels[2].Text:=FileList[x];
      InList.LoadFromFile(FileList[x]);
      for y:=0 to InList.Count-1 do begin
        rs:=StringReplace(InList[y], ',', '', [rfReplaceAll]); {Kommas entfernen}
        SplitList.DelimitedText:=rs;
        if SplitList.Count>1 then begin
          if pos('SN: ', InList[y])>0 then begin
            rs:=InList[y+1];
            StatusBar1.Panels[0].Text:='';
            for z:=length(rs) downto 1 do if rs[z]<>sep then
              StatusBar1.Panels[0].Text:=rs[z]+StatusBar1.Panels[0].Text
                                                        else break;
            SplitList.DelimitedText:=InList[y+2];
            StatusBar1.Panels[1].Text:=SplitList[1];              {WR Nummer}
          end else if (length(rs)>120) and (rs[1] in ziff) then begin {Daten}
            try
              zp:=StrToTime(SplitList[1]);
              itv:=SecondsBetween(zpa, zp);     {Intervall berechnen}
              if (itv<1) or (itv>800) then itv:=defint;
              lei:=StrToInt(SplitList[18]) div 1000;      {Leistung in W}
              if (lei>0) or CheckBox13.Checked then
                OutList.Add(copy(SplitList[0], 7, 4)+'-'+ {Datum}
                copy(SplitList[0], 4, 2)+'-'+
                copy(SplitList[0], 1, 2)+sep+
                copy(SplitList[1], 1, 5)+sep+             {Zeit}
                IntToStr(lei)+sep+                        {Leistung}
                SplitList[20]+sep+                        {Energie}
                '18'+sep+                                 {Temperatur, fix}
                IntToStr(StrToInt(SplitList[6]) div 100)+sep+
                SplitList[4]+sep+                         {U/I String 1}
                IntToStr(StrToInt(SplitList[7]) div 100)+sep+
                SplitList[5]+sep+                         {U/I String 2}
                nix+nix+                                  {U/I String 3}
                IntToStr(StrToInt(SplitList[14]) div 1000)+sep+  {Netzspannung}
                IntToStr(StrToInt(SplitList[22]) div 10)+sep+    {Netzfrequenz}
                IntToStr(itv)+sep+                        {Intervall}
                '0');                                     {Isowiderstand}
              zpa:=zp;                                    {Zeitpunkt merken}
            except                        {Fehler in Rohdaten SMA}
              SynMemo1.Lines.Add('Format error in file '+FileList[x]);
            end;
          end;
        end;
      end;
      ProgressBar1.Position:=x;
      Application.ProcessMessages;
    end;                                   {Ende alle Dateien durchsuchen}
  end;

begin
  screen.cursor:=crHourGlass;
  FileList:=TStringList.Create;  {speichert alle gefundenen Dateinamen zur Auswertung}
  InList:=TStringList.Create;    {speichert den Inhalt einer Datei}
  SplitList:=TStringList.Create; {Hilfsliste zum Aufsplitten separierter Daten}
  try
    FileList:=FindAllFiles(deLocal.Directory, edFilter.Text, false);
    if FileList.Count>0 then begin
      for x:=0 to FileList.Count-1 do
        FileList[x]:=FileList[x];
      FileList.Sort;      {schon heruntergeladene Dateien aufsteigend sortieren}
      if PrevFileList.Count>0 then           {erstmal Konsistenz prüfen}
        for x:=0 to PrevFileList.Count-1 do
          if FileList[x]<>PrevFileList[x] then begin   {Dateiabweichung}
            PrevFileList.Clear;              {alles neu laden erzwingen}
            break;
          end;

      if PrevFileList.Count>0 then begin     {es sind schon Dateien geladen}
        if FileList.Count>PrevFileList.Count then begin
          SplitList.Assign(FileList); {schon geladene Dateien zwischenspeichern}
          y:=PrevFileList.Count;
          PrevFileList.Assign(FileList);
          FileList.Clear;
          for x:=y to PrevFileList.Count-1 do FileList.Add(PrevFileList[x]);
          PrevFileList.Assign(SplitList);    {schon geladene Dateien merken}
          SplitList.Clear;
        end else FileList.Clear;             {nichts tun}
      end else begin                         {alles neu laden}
        OutList.Clear;
        DayList.Clear;
        dr:=IncludeTrailingPathDelimiter(deLocal.Directory);
  {hier wird Archive in Speicher (DayList) geladen}
        if FileExists(dr+archID+archfn) then DayList.LoadFromFile(dr+archID+archfn);
        PrevFileList.Assign(FileList);       {schon geladene Dateien merken}
      end;

      SplitList.Delimiter:=sep;          {default Delimiter}
      if FileList.Count>0 then begin     {nichts tun, wenn keine neuen Dateien}
        ProgressBar1.Min:=0;
        ProgressBar1.Max:=FileList.Count-1;
        case RadioGroup6.ItemIndex of
          0: AuswertenDanfoss;
          1: AuswertenKaco;
          2: AuswertenSMA;
          3: AuswertenSMAspot;
	  4: AuswertenDanfossMigration;
        end;
        ComboBox1.Items.Clear;                 {Datumsliste}
        DateEdit1.Enabled:=false;
        ComboBox2.Items.Clear;                 {Monatsliste}
        ComboBox3.Items.Clear;                 {Jahresliste}
        OutListLesen(true);
        if OutList.Count>0 then StatusBar1.Panels[2].Text:=
          'Stand: '+copy(OutList[OutList.Count-1],12,5)+rsUhr;
        btnSave.Enabled:=true;
        btnBackup.Enabled:=true;
        btnSimu.Enabled:=true;
        btnArch.Enabled:=true;
        BitBtn11.Enabled:=true;             {Spez. Analyse entsperren}
        RadioGroup3.Enabled:=true;
      end;
    end else begin
      StatusBar1.Panels[2].Text:=rsFChk+capLocDir+' / '+capFilter+'.';
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
    end;
  finally                                              {aufräumen}
    FileList.Free;
    InList.Free;
    SplitList.Free;
    screen.cursor:=crDefault;
    if Application.HasOption('s', 'stop') then Form1.Close;
  end;
end;

procedure TForm1.btnSimuClick(Sender: TObject);  {Analyse 70% Regelung}
var SplitList, ResList: TStringList;
    x, p: integer;
    red70, ee, eeges, jges, ee70, eeges70, jges70, dlta, maxdlta: double;
    t, m: string;
begin
  btnTag.Tag:=0;
  if OutList.Count>10 then begin
    Screen.Cursor:=crHourGlass;
    ResList:=TStringList.Create;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    try
      case RadioGroup7.ItemIndex of       {Leistungsgrenze einstellen}
        0: red70:=StrToInt(edPeak.Text)*0.5;   {50%}
        1: red70:=StrToInt(edPeak.Text)*0.6;   {60%}
        2: red70:=StrToInt(edPeak.Text)*0.7;   {70%}
        3: red70:=StrToInt(edPeak.Text)*0.8;   {80%}
      end;
      pcMain.ActivePageIndex:=3;    {zur Anzeige auf Statistik umschalten}
      RadioGroup1.ItemIndex:=-1;          {nichts auswählen}
      RadioGroup1.Tag:=1;                 {Indikator für diese Funktion}
      t:=RadioGroup7.Items[RadioGroup7.ItemIndex];
      ResList.Add(rsLimit+t+sep+rsReduct+
                  FloatToStrF(red70/1000, ffFixed, 12, 3)+'kW'+sep+sep+
                  FormatDateTime(isoday, now));
      SynMemo1.Lines.Add(rsLimit+t+' '+rsReduct+
                         FloatToStrF(red70/1000, ffFixed, 12, 3)+'kW'+'  '+
                         FormatDateTime(isoday, now));
      ResList.Add('');
      Reslist.Add('Monat'+sep+rsErtrag+' [kWh]'+sep+rsErtrag+t+' [kWh]'+sep+
                  'Verlust [%]');
    except
      SynMemo1.Lines.Add('InitError! Limit: '+
                         FloatToStrF(red70, ffFixed, 12, 0)+'W');
    end;
    try
      Chart1.AxisList[0].Inverted:=false;
      Chart1.AxisList[0].Title.Caption:=rsErtrag+' [kWh]';
      Chart1.AxisList[0].Marks.Source:=nil;
      Chart1.AxisList[0].Marks.Format:='%0:.9g';
      Chart1.AxisList[1].Title.Caption:=rsMonatsaw;
      Chart1.AxisList[1].Grid.Visible:=false;
      Chart1.AxisList[1].Intervals.MaxLength:=300;
      Chart1.AxisList[1].Marks.Format:='%0:.0g';
      Chart1.AxisList[1].Marks.AtDataOnly:=true;
      Chart1.Title.Text[0]:=rsLimit+t+rsReduct+
                            FloatToStrF(red70/1000, ffFixed, 12, 3)+'kW';
      Chart1ConstantLine1.Active:=false;
      Chart1ConstantLine2.Active:=false;
      Chart1BarSeries1.Clear;
      Chart1BarSeries1.BarWidthPercent:=60;
      Chart1BarSeries1.BarPen.Color:=clBlack;
      Chart1BarSeries1.Depth:=0;
      Chart1BarSeries2.Clear;
      Chart1BarSeries2.BarWidthPercent:=60;
      Chart1BarSeries2.BarPen.Color:=clBlack;
      Chart1BarSeries2.Depth:=0;
      Chart1LineSeries1.Clear;
      Chart1LineSeries2.Clear;
      ee:=0;                   {Energieertrag gesamt am Tag}
      eeges:=0;                {Energieertrag gesamt im Monat}
      jges:=0;                 {Energieertrag insgesamt}
      ee70:=0;                 {Energieertrag 70% gesamt am Tag}
      eeges70:=0;              {Energieertrag 70% gesamt im Monat}
      jges70:=0;               {Energieertrag 70% insgesamt}
      t:='';                   {Tag}
      m:='';
      maxdlta:=0;              {maximales Delta im Tagesertrag zw. normal und reduziert}
      for x:=0 to OutList.Count-1 do begin      {alle Daten durchsuchen}
        SplitList.DelimitedText:=OutList[x];
        if t<>copy(OutList[x],1,10) then begin  {neuer Tag}
          eeges:=eeges+ee;                      {Ertrag für Monat aufaddieren}
          jges:=jges+ee;
          eeges70:=eeges70+ee70;                {70% Ertrag für Monat aufaddieren}
          jges70:=jges70+ee70;
          if m<>copy(OutList[x],1,7) then begin {neuer Monat auch noch}
            if m<>'' then begin
              Chart1BarSeries1.Add(eeges/1000, m, ColorButton12.ButtonColor);
              Chart1BarSeries2.Add(eeges70/1000, m, ColorButton13.ButtonColor);
              ResList.Add(MonToTxt(m)+' '+copy(OutList[x],1,4)+sep+
                          FloatToStrF(eeges/1000, ffFixed, 12, 3)+sep+
                          FloatToStrF(eeges70/1000, ffFixed, 12, 3)+sep+
                          FloatToStrF((eeges-eeges70)/eeges*100, ffFixed, 5, 2));
            end;
            m:=copy(outList[x],1,7);
            eeges:=0;
            eeges70:=0;
          end;
          t:=Copy(OutList[x],1,10);
          ee:=0;
          ee70:=0;
        end;
        p:=StrToInt(SplitList[2]);
        ee:=ee+p*StrToInt(SplitList[13])/3600; {Ertrag integrieren}
        if p<red70 then ee70:=ee70+p*StrToInt(SplitList[13])/3600
                   else ee70:=ee70+red70*StrToInt(SplitList[13])/3600;
        dlta:=ee-ee70;
        if dlta>maxdlta then maxdlta:=dlta;
      end;
      eeges:=eeges+ee;                      {Ertrag für Monat aufaddieren}
      jges:=jges+ee;
      eeges70:=eeges70+ee70;                {70% Ertrag für Monat aufaddieren}
      jges70:=jges70+ee70;
      if (m<>'') and (eeges>0) then begin
        Chart1BarSeries1.Add(eeges/1000, m, ColorButton12.ButtonColor);
        Chart1BarSeries2.Add(eeges70/1000, m, ColorButton13.ButtonColor);
        ResList.Add(MonToTxt(m)+' '+copy(OutList[x],1,4)+sep+
                    FloatToStrF(eeges/1000, ffFixed, 12, 3)+sep+
                    FloatToStrF(eeges70/1000, ffFixed, 12, 3)+sep+
                    FloatToStrF((eeges-eeges70)/eeges*100, ffFixed, 5, 2));
      end;
      t:=RadioGroup7.Items[RadioGroup7.ItemIndex];
      StatusBar1.Panels[2].Text:='Ertrag bei'+t+'-Regel: '+
                                 FloatToStrF(jges70/1000, ffFixed, 12, 3)+
                                 'kWh statt '+
                                 FloatToStrF(jges/1000,ffFixed, 12, 3)+'kWh ('+
                                 FloatToStrF(jges70/jges*100,ffFixed, 5, 2)+'%)';
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
      ResList.Add('Total'+sep+FloatToStrF(jges/1000, ffFixed, 12, 3)+sep+
                              FloatToStrF(jges70/1000, ffFixed, 12, 3)+sep+
                              FloatToStrF((jges-jges70)/jges*100, ffFixed, 5, 2));
      ResList.Add('');
      ResList.Add(rsMaxDelta+FloatToStrF(maxdlta/1000,ffFixed, 12, 3)+'kWh');
      SynMemo1.Lines.Add(rsMaxDelta+FloatToStrF(maxdlta/1000,ffFixed, 12, 3)+'kWh');
      t:=IncludeTrailingPathDelimiter(deLocal.Directory)+
         FormatDateTime(isodate, now)+'_'+rsAnalyse+
         copy(RadioGroup7.Items[RadioGroup7.ItemIndex], 2, 2)+'.csv';
      try
        ResList.SaveToFile(t);
      except
        StatusBar1.Panels[2].Text:=msgNoSave;
        SynMemo1.Lines.Add(t+'  '+StatusBar1.Panels[2].Text);
      end;
    finally
      SynMemo1.Lines.Add('');
      SplitList.Free;
      ResList.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.btnSaveProtClick(Sender: TObject);  {FTP Verbindungsprotokoll speichern}
begin
  if SynMemo1.Lines.Count>0 then begin
    SaveDialog1.Title:=capFTPProt+' '+capFTPsv+'...';
    SaveDialog1.FilterIndex:=3;
    SaveDialog1.Filename:=IncludeTrailingPathDelimiter(deLocal.Directory)+
                          FormatdateTime(isodate, now)+'_'+capFTPProt+'.txt';
    if SaveDialog1.Execute then begin
      SynMemo1.Lines.SaveToFile(SaveDialog1.FileName);
      SynMemo1.Lines.Clear;
      SynMemo1.Lines.Add('FTP protocol saved to '+SaveDialog1.FileName);
    end;
  end;
end;

procedure TForm1.Statistik;   {Diverse Statistiken als Unterprozeduren}

  procedure StartEndeZeiten;  {Start und Endezeiten enspr. Schwellwert ermitteln}
  var SplitList: TStringList;
      x: Integer;
      t: string;
      beg, fa, fbeg, fend, zt: TDateTime;
      lesw, zhl: Integer;     {minimale Leistung für Anfang / Ende in W}

    function CorrSTime(s: string): TDateTime; inline;
    begin
      result:=TToTT(s);
      if CheckBox11.Checked then    {Sommerzeit eliminieren}
        result:=frac(NixDST(SDToTime(SplitList[0])+result));  {Datum addiert}
    end;

  begin
    Chart1.Title.Text[0]:='Tagesbeginn/-ende';
    Chart1.AxisList[0].Inverted:=true;             {Beginn oben}
    Chart1.AxisList[0].Title.Caption:='Tageszeit';
    Chart1.AxisList[0].Marks.Source:=DateTimeIntervalChartSource2;
    Chart1.AxisList[0].Marks.Format:='%2:s';
    Chart1.AxisList[1].Marks.AtDataOnly:=false;
    Chart1LineSeries1.SeriesColor:=ColorButton19.ButtonColor;
    Chart1LineSeries2.SeriesColor:=ColorButton20.ButtonColor;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    lesw:=StrToInt(edMinPower.Text);             {Schwellwert Leistung}
    beg:=0;
    fbeg:=1;
    fend:=0;
    fa:=0;
    zhl:=0;
    t:='';
    try
      if DayList.Count>0 then for x:=0 to DayList.Count-1 do begin
        SplitList.DelimitedText:=DayList[x];
        inc(zhl);
        beg:=CorrSTime(SplitList[4]);
        if beg>fend then fend:=beg;                {spätestes Ende}
        Chart1LineSeries2.AddXY(zhl, beg);
        beg:=CorrSTime(SplitList[3]);
        if beg<fbeg then fbeg:=beg;                {frühester Beginn}
        Chart1LineSeries1.AddXY(zhl, beg);
      end;
      for x:=0 to OutList.Count-1 do begin
        SplitList.DelimitedText:=OutList[x];
        if StrToInt(SplitList[2])>lesw then begin  {Leistungsschwellwert}
          zt:=CorrSTime(SplitList[1]);
          if beg=0 then beg:=zt;
          if beg<fbeg then fbeg:=beg;              {frühester Beginn}
          if zt>fa then fa:=zt;
          if fa>fend then fend:=fa;                {spätestes Ende}
        end;
        if copy(outList[x],1,10)<>t then begin
          if t<>'' then begin
            inc(zhl);                              {Tage einfach hochzählen}
            if fa=0 then fa:=0.5;                  {12:00h als "Mitte"}
            if beg=0 then beg:=0.5;
            Chart1LineSeries2.AddXY(zhl, fa);
            Chart1LineSeries1.AddXY(zhl, beg);
          end;
          beg:=0;
          fa:=0;
          t:=copy(outList[x],1,10);
        end;
      end;
      inc(zhl);
      Chart1LineSeries2.AddXY(zhl, fa);
      Chart1LineSeries1.AddXY(zhl, beg);
      StatusBar1.Panels[2].Text:='Frühester Beginn: '+
                                 FormatDateTime('hh:mm', fbeg)+
                                 rsUhr+' / Spätestes Ende: '+
                                 FormatDateTime('hh:mm', fend)+rsUhr;
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
    finally
      SplitList.Free;
    end;
  end;

  procedure StartEndeDauer;  {Ende-Start=Dauer enspr. Schwellwert ermitteln}
  var SplitList: TStringList;
      x: Integer;
      t: string;
      beg, fa, ld, ldt: TDateTime;
      lesw, zhl: Integer;    {minimale Leistung für Anfang / Ende in W}
  begin
    Chart1.Title.Text[0]:='Ertragsdauer je Tag';
    Chart1.AxisList[0].Title.Caption:='Tägliche Produktionsdauer';
    Chart1.AxisList[0].Marks.Source:=DateTimeIntervalChartSource2;
    Chart1.AxisList[0].Marks.Format:='%2:s';
    Chart1BarSeries1.SeriesColor:=ColorButton16.ButtonColor; {Farbe Ertrag}
    Chart1BarSeries1.BarPen.Color:=Chart1BarSeries1.SeriesColor;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    lesw:=StrToInt(edMinPower.Text);
    beg:=0;
    ld:=0;
    fa:=0;
    zhl:=0;
    t:='';
    try
      if DayList.Count>0 then for x:=0 to DayList.Count-1 do begin
        SplitList.DelimitedText:=DayList[x];
        inc(zhl);
        fa:=TToTT(SplitList[4]);                   {Ende Produktion}
        beg:=TToTT(SplitList[3]);                  {Beginn Produktion}
        if (fa-beg)>ld then begin           {längste Ertragsdauer feststellen}
          ld:=fa-beg;
          ldt:=SDToTime(copy(DayList[x],1,10))+fa; {Zeitpunkt: Datum+Ende}
        end;
        Chart1BarSeries1.AddXY(zhl, fa-beg);
      end;
      for x:=0 to OutList.Count-1 do begin
        SplitList.DelimitedText:=OutList[x];
        if StrToInt(SplitList[2])>lesw then begin  {Leistungsschwellwert}
          if beg=0 then beg:=TToTT(SplitList[1]);
          if TToTT(SplitList[1])>fa then fa:=TToTT(SplitList[1]);
          if (fa-beg)>ld then begin         {längste Ertragsdauer feststellen}
            ld:=fa-beg;
            ldt:=SDToTime(copy(OutList[x],1,10))+fa;  {Zeitpunkt: Datum+Ende}
          end;
        end;
        if copy(outList[x],1,10)<>t then begin
          if t<>'' then begin
            inc(zhl);  {Tage einfach hochzählen}
            Chart1BarSeries1.AddXY(zhl, fa-beg);
          end;
          beg:=0;
          fa:=0;
          t:=copy(outList[x],1,10);
        end;
      end;
      inc(zhl);
      Chart1BarSeries1.AddXY(zhl, fa-beg);
      StatusBar1.Panels[2].Text:='Längste Ertragsdauer: '+
                       FormatDateTime('hh:mm', ld)+'h'+rsAm+
                       FormatDateTime('dddd, dd. mmm. yyyy', ldt);
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
    finally
      SplitList.Free;
    end;
  end;

  procedure PeakLeist;         {Maximale Werte über Zeit: Leistung}
  var SplitList: TStringList;
      x, peak, maxpeak, zhl, sumpeak: Integer;
      t: string;
      maxt: TDateTime;
  begin
    Chart1.Title.Text[0]:='Maximale '+rsLei+' je Tag';
    Chart1.AxisList[0].Title.Caption:=rsPeakL+' [kWp]';
    Chart1BarSeries1.BarPen.Color:=ColorButton15.ButtonColor;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    peak:=0;
    maxpeak:=1000;  {Annahme, dass mehr als 1kW erzeugt wird}
    sumpeak:=0;
    t:='';
    maxt:=0;
    zhl:=1;         {Tageszähler für Durchschnittswert}
    try
      if DayList.Count>0 then for x:=0 to DayList.Count-1 do begin
        SplitList.DelimitedText:=DayList[x];
        peak:=StrToInt(SplitList[1]);
        Chart1BarSeries1.Add(peak/1000, copy(DayList[x],1,10), ColorButton15.ButtonColor);
        inc(zhl);
        sumpeak:=sumpeak+peak;
        if peak>maxpeak then begin
          maxpeak:=peak;
          maxt:=SDToTime(copy(DayList[x],1,10))+              {Datum}
                (TToTT(SplitList[3])+TToTT(SplitList[4]))/2;  {Zeit geschätzt}
        end;
      end;
      for x:=0 to OutList.Count-1 do begin
        SplitList.DelimitedText:=OutList[x];
        if StrToInt(SplitList[2])>peak then peak:=StrToInt(SplitList[2]);
        if StrToInt(SplitList[2])>maxpeak then begin
          maxpeak:=StrToInt(SplitList[2]);
          maxt:=SDToTime(copy(OutList[x],1,10))+              {Datum}
                TToTT(copy(OutList[x],12,5));                 {Zeit}
        end;
        if copy(OutList[x],1,10)<>t then begin                {neuer Tag}
          if t<>'' then begin
            Chart1BarSeries1.Add(peak/1000, t, ColorButton15.ButtonColor);
            inc(zhl);
            sumpeak:=sumpeak+peak;
          end;
          t:=copy(OutList[x],1,10);
          peak:=0;
        end;
      end;
      Chart1BarSeries1.Add(peak/1000, t, ColorButton15.ButtonColor);
      sumpeak:=sumpeak+peak;
      StatusBar1.Panels[2].Text:=FloatToStr(maxpeak/1000)+
                                 'kW '+rsPeakL+rsAm+
                                 FormatDateTime('dddd, dd. mmm. yyyy - hh:mm', maxt)+
                                 rsUhr;
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
      Chart1ConstantLine1.Position:=sumpeak/zhl/1000;         {Durchschnitt}
      Chart1ConstantLine1.Active:=true;
      Chart1ConstantLine2.Position:=StrToInt(edPeak.Text)/1000; {Soll}
      Chart1ConstantLine2.Active:=true;    {projektierte Peakleistung}
    finally
      SplitList.Free;
    end;
  end;

  procedure MaxErtrag;                             {Tagesertrag über Zeit}
  var SplitList: TStringList;
      x, ee, maxee, eeges, zhl: Integer;
      t: string;
      maxt: TDateTime;
  begin
    Chart1.Title.Text[0]:=rsErtrag+' je Tag';
    Chart1.AxisList[0].Title.Caption:=rsTagErtr+' [kWh]';
    Chart1BarSeries1.BarPen.Color:=ColorButton16.ButtonColor;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    maxee:=0;
    eeges:=0;
    ee:=0;
    zhl:=1;
    t:='';
    maxt:=0;
    try
      if DayList.Count>0 then for x:=0 to DayList.Count-1 do begin
        SplitList.DelimitedText:=DayList[x];
        ee:=GetKErt(StrToInt(SplitList[2]));       {Tagesertrag korrigieren}
        Chart1BarSeries1.Add(ee/1000, copy(DayList[x],1,10), ColorButton16.ButtonColor);
        inc(zhl);
        eeges:=eeges+ee;
        if ee>maxee then begin
          maxee:=ee;
          maxt:=SDToTime(copy(DayList[x],1,10));
        end;
      end;
      for x:=0 to OutList.Count-1 do begin
        SplitList.DelimitedText:=OutList[x];
        if StrToInt(SplitList[3])>ee then ee:=StrToInt(SplitList[3]);
        if copy(OutList[x],1,10)<>t then begin     {neuer Tag}
          if t<>'' then begin
            ee:=GetKErt(ee);                       {Tagesertrag korrigieren}
            Chart1BarSeries1.Add(ee/1000, t, ColorButton16.ButtonColor);
            inc(zhl);
            eeges:=eeges+ee;
            if ee>maxee then begin
              maxee:=ee;
              maxt:=SDToTime(t);
            end;
          end;
          t:=copy(OutList[x],1,10);
          ee:=0;
        end;
      end;
      ee:=GetKErt(ee);                             {Tagesertrag korrigieren}
      Chart1BarSeries1.Add(ee/1000, t, ColorButton16.ButtonColor);
      eeges:=eeges+ee;
      if ee>maxee then begin
        maxee:=ee;
        maxt:=SDToTime(t);
      end;
      StatusBar1.Panels[2].Text:='Höchster '+rsTagErtr+': '+
                                 FloatToStr(maxee/1000)+'kWh'+rsAm+
                                 FormatDateTime('dddd, dd. mmm. yyyy', maxt);
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
      Chart1ConstantLine1.Position:=eeges/zhl/1000;
      Chart1ConstantLine1.Active:=true;
    finally
      SplitList.Free;
    end;
  end;

  procedure SpezJE;                                {Spezifischer Jahresertrag}
  var SplitList: TStringList;
      x, ee, jee, mz, pla: Integer;
      t, maxt: string;
      sj, maxsj: double;
  begin
    pla:=StrToInt(edPeak.Text);              {Peakleistung der Anlage}
    Chart1.Title.Text[0]:=rsSpezJE;
    Chart1.AxisList[0].Title.Caption:=rsSpezJE+' [kWh/kWp]';
    Chart1.AxisList[1].Title.Caption:=rsJahresaw;
    Chart1.AxisList[1].Marks.Style:=smsLegend;
    Chart1.AxisList[1].Grid.Visible:=false;
    Chart1BarSeries1.BarPen.Color:=ColorButton16.ButtonColor;
    Chart1BarSeries1.BarWidthPercent:=80;
    Chart1BarSeries1.Marks.Visible:=true;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    maxsj:=0;
    ee:=0;
    jee:=0;                                        {Jahresertrag}
    t:='';
    mz:=0;                                         {Monatszähler}
    try
      if DayList.Count>0 then for x:=0 to DayList.Count-1 do begin
        if t<>'' then begin
          if copy(DayList[x], 1, 7)<>copy(t, 1, 7) then begin    {neuer Monat}
            inc(mz);
            if copy(DayList[x], 1, 4)<>copy(t, 1, 4) then begin  {neues Jahr}
              sj:=jee/pla;
              if mz=12 then Chart1BarSeries1.Add(sj, copy(t,1,4), ColorButton16.ButtonColor)
                       else Chart1BarSeries1.Add(sj, copy(t,1,4), ColorButton17.ButtonColor);
              if sj>maxsj then begin
                maxsj:=sj;
                maxt:=copy(t, 1, 4);
              end;
              jee:=0;
              mz:=0;
            end;
          end;
        end;
        SplitList.DelimitedText:=DayList[x];
        ee:=GetKErt(StrToInt(SplitList[2]));       {Tagesertrag korrigieren}
        t:=copy(DayList[x],1,10);
        jee:=jee+ee;
      end;
      ee:=0;
      for x:=0 to OutList.Count-1 do begin
        SplitList.DelimitedText:=OutList[x];
        if StrToInt(SplitList[3])>ee then ee:=StrToInt(SplitList[3]);
        if copy(OutList[x],1,10)<>t then begin     {neuer Tag}
          if t<>'' then begin
            ee:=GetKErt(ee);                       {Tagesertrag korrigieren}
            jee:=jee+ee;
            if copy(OutList[x], 1, 7)<>copy(t, 1, 7) then begin    {neuer Monat}
              inc(mz);
              if copy(OutList[x], 1, 4)<>copy(t, 1, 4) then begin  {neues Jahr}
                sj:=jee/pla;
                if mz=12 then Chart1BarSeries1.Add(sj, copy(t,1,4), ColorButton16.ButtonColor)
                         else Chart1BarSeries1.Add(sj, copy(t,1,4), ColorButton17.ButtonColor);
                if sj>maxsj then begin
                  maxsj:=sj;
                  maxt:=copy(t, 1, 4);
                end;
                jee:=0;
                mz:=0;
              end;
            end;
          end;
          t:=copy(OutList[x],1,10);
          ee:=0;
        end;
      end;
      ee:=GetKErt(ee);                             {Tagesertrag korrigieren}
      jee:=jee+ee;
      sj:=jee/pla;
      if mz=12 then Chart1BarSeries1.Add(sj, copy(t,1,4), ColorButton16.ButtonColor)
               else Chart1BarSeries1.Add(sj, copy(t,1,4), ColorButton17.ButtonColor);
      if sj>maxsj then begin
        maxsj:=sj;
        maxt:=copy(t, 1, 4);
      end;
      Chart1ConstantLine2.Position:=GetSJE;        {grüne Linie = Soll}
      Chart1ConstantLine2.Active:=true;
      StatusBar1.Panels[2].Text:='Höchster '+rsSpezJE+': '+
                                 FloatToStrF(maxsj, ffFixed, 12, 3)+
                                 'kWh/kWp in '+maxt;
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
    finally
      SplitList.Free;
    end;
  end;

  procedure ErtragRel;              {Histogramm Tagesertrag relativ zum Soll}
  var SplitList: TStringList;
      prozente: array of integer;
      x, y, ee, zhl, w, sum, pla: Integer;
      t: string;
      soll: double;
  begin
    Chart1BarSeries1.Depth:=Bar3D;
    Chart1.Title.Text[0]:=rsHistoTag;
    Chart1.AxisList[1].Title.Caption:=rsTagErtr+' [%]';
    Chart1.AxisList[1].Grid.Visible:=false;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    pla:=StrToInt(edPeak.Text);
    soll:=pla*GetSJE/365;
    ee:=0;
    y:=0;
    sum:=0;
    SetLength(prozente, y+1);
    t:='';
    ProgressBar1.Position:=1;
    Application.ProcessMessages;
    try
      if DayList.Count>0 then for x:=0 to DayList.Count-1 do begin
        SplitList.DelimitedText:=DayList[x];
        ee:=GetKErt(StrToInt(SplitList[2]));
        prozente[y]:=Round(ee/soll*10)*10;         {auf 10% runden}
        if ee/pla>=6 then inc(sum);                {Anzahl Tage mit >= 6kWh/kWp}
        Inc(y);
        SetLength(prozente, y+1);
        t:=copy(DayList[x],1,10);
      end;
      for x:=0 to OutList.Count-1 do begin
        SplitList.DelimitedText:=OutList[x];
        if StrToInt(SplitList[3])>ee then ee:=StrToInt(SplitList[3]);
        if copy(OutList[x],1,10)<>t then begin     {neuer Tag}
          if t<>'' then begin
            ee:=GetKErt(ee);                       {Tagesertrag korrigieren}
//          prozente[y]:=Round(ee/soll*20)*5;      {auf 5% runden}
            prozente[y]:=Round(ee/soll*10)*10;     {auf 10% runden}
            if ee/pla>=6 then inc(sum);            {Anzahl Tage mit >= 6kWh/kWp}
            Inc(y);
            SetLength(prozente, y+1);
            ee:=0;
          end;
          t:=copy(OutList[x],1,10);
        end;
      end;
      ee:=GetKErt(ee);                             {Tagesertrag korrigieren}
      prozente[y]:=Round(ee/soll*10)*10;
      if ee/pla>=6 then inc(sum);
      ProgressBar1.Position:=4;
      Application.ProcessMessages;
      QuickSort(prozente, Low(prozente), High(prozente));
      ProgressBar1.Position:=8;
      Application.ProcessMessages;
      zhl:=0;
      w:=0;
      for x:=0 to length(prozente)-1 do begin
        inc(zhl);                           {Anzahl gleicher Datenpunkte zählen}
        if prozente[x]<>w then begin        {neuer Wert gefunden}
          if w<>0 then Chart1BarSeries1.AddXY(w, zhl);
          zhl:=0;
          w:=prozente[x];
        end;
      end;
      if w<>0 then Chart1BarSeries1.AddXY(w, zhl);
      StatusBar1.Panels[2].Text:=IntToStr(sum)+' Tage mit >= 6kWh/kWp.';
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
      ProgressBar1.Position:=ProgressBar1.Max;
    finally
      SplitList.Free;
      SetLength(prozente, 0);
    end;
  end;

  procedure NetzSpg;                        {Spectrum Netzspannung anzeigen}
  var SplitList: TStringList;
      werte: array of Integer;
      x, zhl, w: Integer;
  begin
    Chart1.Title.Text[0]:=rsHisto+rsUAC;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    Chart1BarSeries1.Depth:=Bar3D;
    Chart1.AxisList[1].Title.Caption:=rsUAC+' [V]';
    Chart1.AxisList[1].Grid.Visible:=false;
    Chart1.AxisList[1].Intervals.MaxLength:=100;
    SetLength(werte, 0);                    {dynamisches Array initialisieren}
    ProgressBar1.Position:=1;
    Application.ProcessMessages;
    try
      for x:=0 to OutList.Count-1 do begin  {Spannungswerte ausfiltern}
        SplitList.DelimitedText:=OutList[x];
        w:=StrToInt(SplitList[11]);
        if ((x mod TrackBar1.Position)=0) and (w>spsw) then begin
          SetLength(werte, length(werte)+1);
          werte[high(werte)]:=w;
        end;
      end;
      ProgressBar1.Position:=3;
      Application.ProcessMessages;
      QuickSort(werte, Low(werte), High(werte));
      ProgressBar1.Position:=8;
      Application.ProcessMessages;
      w:=0;
      zhl:=0;
      for x:=0 to high(werte) do begin   {Alle Spannungs-Datenpunkte ansehen}
        inc(zhl);                        {Anzahl gleicher Datenpunkte zählen}
        if werte[x]<>w then begin        {neuer Wert gefunden}
          if w<>0 then begin
            Chart1BarSeries1.AddXY(w, zhl);
            zhl:=0;
          end;
          w:=werte[x];
        end;
      end;
      if w<>0 then Chart1BarSeries1.AddXY(w, zhl);
      ProgressBar1.Position:=ProgressBar1.Max;
      StatusBar1.Panels[2].Text:=rsHisto+rsUAC;
    finally
      SetLength(werte, 0);   {Speicher dyn . Array freigeben}
      SplitList.Free;
    end;
  end;

  procedure NetzFreq;  {Spectrum Netzfrequenz anzeigen}
  var SplitList: TStringList;
      werte: array of Integer;
      x, zhl, w: Integer;
  begin
    Chart1.Title.Text[0]:=rsHisto+rsFrequ;
    SetLength(werte, 0);      {dynamisches Array initialisieren}
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    Chart1BarSeries1.Depth:=Bar3D;
    Chart1.AxisList[1].Title.Caption:=rsFrequ+' [Hz]';
    Chart1.AxisList[1].Grid.Visible:=false;
    Chart1.AxisList[1].Intervals.MaxLength:=80;
    ProgressBar1.Position:=1;
    Application.ProcessMessages;
    try
      for x:=0 to OutList.Count-1 do begin  {Frequenzwerte ausfiltern}
        SplitList.DelimitedText:=OutList[x];
        w:=StrToInt(SplitList[12]);
        if ((x mod TrackBar1.Position)=0) and (w>frsw) then begin
          SetLength(werte, length(werte)+1);
          werte[high(werte)]:=w;
        end;
      end;
      ProgressBar1.Position:=3;
      Application.ProcessMessages;
      QuickSort(werte, Low(werte), High(werte));
      ProgressBar1.Position:=8;
      Application.ProcessMessages;
      w:=0;
      zhl:=0;
      for x:=0 to high(werte) do begin     {Alle Frequenz-Datenpunkte ansehen}
        inc(zhl);                          {Anzahl gleicher Datenpunkte zählen}
        if werte[x]<>w then begin          {neuer Wert gefunden}
          if w<>0 then begin
            Chart1BarSeries1.AddXY(w/100, zhl);
            zhl:=0;
          end;
          w:=werte[x];
        end;
      end;
      if w<>0 then Chart1BarSeries1.AddXY(w/100, zhl);
      ProgressBar1.Position:=ProgressBar1.Max;
      StatusBar1.Panels[2].Text:=rsHisto+rsFrequ;
    finally
      SetLength(werte, 0);   {Speicher dyn . Array freigeben}
      SplitList.Free;
    end;
  end;

  procedure Temperatur;    {Temperaturspectrum}
  var SplitList: TStringList;
      werte: array of Integer;
      x, zhl, w: Integer;
  begin
    Chart1.Title.Text[0]:=rsHisto+rsWRTemp;
    SetLength(werte, 0);      {dynamisches Array initialisieren}
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    Chart1BarSeries1.Depth:=Bar3D;
    Chart1.AxisList[1].Title.Caption:=rsWRTemp+' [°C]';
    Chart1.AxisList[1].Grid.Visible:=false;
    ProgressBar1.Position:=1;
    Application.ProcessMessages;
    try
      for x:=0 to OutList.Count-1 do begin  {Temperaturwerte ausfiltern}
        SplitList.DelimitedText:=OutList[x];
        if (x mod TrackBar1.Position)=0 then begin
          SetLength(werte, length(werte)+1);
          werte[high(werte)]:=StrToInt(SplitList[4]);
        end;
      end;
      ProgressBar1.Position:=3;
      Application.ProcessMessages;
      QuickSort(werte, Low(werte), High(werte));
      ProgressBar1.Position:=8;
      Application.ProcessMessages;
      w:=0;
      zhl:=0;
      for x:=0 to high(werte) do begin     {Alle Temperatur-Datenpunkte ansehen}
        inc(zhl);                          {Anzahl gleicher Datenpunkte zählen}
        if werte[x]<>w then begin          {neuer Wert gefunden}
          if w<>0 then begin
            Chart1BarSeries1.AddXY(w, zhl);
            zhl:=0;
          end;
          w:=werte[x];
        end;
      end;
      if w<>0 then Chart1BarSeries1.AddXY(w, zhl);
      ProgressBar1.Position:=ProgressBar1.Max;
      StatusBar1.Panels[2].Text:=rsHisto+rsWRTemp;
    finally
      SetLength(werte, 0);   {Speicher dyn . Array freigeben}
      SplitList.Free;
    end;
  end;

begin                        {allgemeine Statistiken}
  RadioGroup1.Tag:=0;        {keine 70%-Analyse}
  RadioGroup1.Refresh;
  if OutList.Count>1 then begin
    Screen.Cursor:=crHourGlass;
    Chart1ConstantLine1.Active:=false;
    Chart1ConstantLine2.Active:=false;
    Chart1ConstantLine1.SeriesColor:=ColorButton11.ButtonColor; {Durchschnitt}
    Chart1ConstantLine2.SeriesColor:=ColorButton8.ButtonColor;  {Soll}
    Chart1BarSeries1.Clear;
    Chart1BarSeries2.Clear;
    Chart1BarSeries1.BarWidthPercent:=100;
    Chart1BarSeries1.Marks.Visible:=false;
    Chart1BarSeries1.BarPen.Color:=clBlack;
    Chart1BarSeries1.SeriesColor:=ColorButton18.ButtonColor;    {Histogramm}
    Chart1BarSeries1.Depth:=0;
    Chart1LineSeries1.Clear;
    Chart1LineSeries2.Clear;
    Chart1.AxisList[0].Inverted:=false;
    Chart1.AxisList[0].Title.Caption:=rsHfkeit;
    Chart1.AxisList[0].Marks.Source:=nil;
    Chart1.AxisList[0].Marks.Format:='%0:.9g';
    Chart1.AxisList[1].Title.Caption:=rsGesZeit;
    Chart1.AxisList[1].Intervals.MaxLength:=50;
    Chart1.AxisList[1].Marks.Format:='%0:.3g';
    Chart1.AxisList[1].Marks.AtDataOnly:=true;
    Chart1.AxisList[1].Marks.Style:=smsValue;
    Chart1.AxisList[1].Grid.Visible:=true;
    ProgressBar1.Min:=0;
    ProgressBar1.Max:=10;
    ProgressBar1.Position:=0;
    try
      case RadioGroup1.ItemIndex of
        0: PeakLeist;
        1: StartEndeZeiten;
        2: StartEndeDauer;
        3: MaxErtrag;
        4: SpezJE;
        5: ErtragRel;
        6: NetzSpg;
        7: NetzFreq;
        8: Temperatur;
      end;
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.TagStat;                       {Tagesauswertung}
var SplitList, SweepList: TStringList;
    x, y, peak, ee, pla, umin, umax, w, itv, gw, gx, ts1, ts2, ts3: Integer;
    er1, er2, er3: double;
    s, fr: string;
    dpt: TDateTime;
    erl: boolean;
begin
  pla:=StrToInt(edPeak.Text);             {Peakleistung der Anlage}
  umax:=0;
  umin:=9999;
  er1:=0;                                       {Ertrag pro String}
  er2:=0;
  er3:=0;
  RadioGroup2.Refresh;
  if Shape1.Brush.Color<>clDefault then
     Shape1.Brush.Color:=ColorButton3.ButtonColor;        {String 1}
  if Shape2.Brush.Color<>clDefault then
     Shape2.Brush.Color:=ColorButton4.ButtonColor;        {String 2}
  if Shape3.Brush.Color<>clDefault then
     Shape3.Brush.Color:=ColorButton5.ButtonColor;        {String 3}
  if ComboBox1.Text[1] in ziff then begin                 {Datumsfeld}
    Chart4.AxisList[1].Marks.Source:=DateTimeIntervalChartSource1;
    Chart4.AxisList[1].Marks.Format:='%2:s';
    Chart4.AxisList[1].Title.Caption:='';       {default: nix anzeigen}
    Chart4.AxisList[1].Title.Visible:=false;
    Chart4.AxisList[0].Title.LabelFont.Color:=clDefault;
    Chart4LineSeries1.SeriesColor:=ColorButton3.ButtonColor;
    Chart4LineSeries1.LinePen.Width:=1;
    Chart4LineSeries2.SeriesColor:=ColorButton4.ButtonColor;
    Chart4LineSeries2.LinePen.Width:=1;
    Chart4LineSeries3.SeriesColor:=ColorButton5.ButtonColor;
    Chart4LineSeries3.AxisIndexY:=0;            {default: left axis}
    Chart4LineSeries3.LinePen.Width:=1;
    Screen.Cursor:=crHourGlass;
    Chart4.Title.Text[0]:=rsTagesaw+' '+
                FormatDateTime('dddd, dd. mmmm yyyy', SDToTime(ComboBox1.Text));
    Chart4LineSeries4.LinePen.Width:=2;         {dickere Gesamtlinie}
    Chart4LineSeries4.SeriesColor:=ColorButton6.ButtonColor; {Farbe für Gesamt}
    Chart4LineSeries1.Clear;          {Default, String 1}
    Chart4LineSeries2.Clear;          {String 2}
    Chart4LineSeries3.Clear;          {String 3}
    Chart4LineSeries4.Clear;          {Gesamt}
    Chart4.ZoomFull;                  {Zoom und Pan rücksetzen}
    case RadioGroup2.ItemIndex of     {Voreinstellungen entspr. Auswertung}
      0: begin                        {Ertrag}
           Chart4.AxisList[0].Title.Caption:=rsErtrag+' [kWh]';
         end;
      1: begin                        {Leistung}
           Chart4.AxisList[0].Title.Caption:=rsLei+' [kW]';
           Chart4.AxisList[0].Visible:=true;
         end;
      2: begin                        {Strings %}
           Chart4.AxisList[0].Title.Caption:=rsLei+' [%] per'+rsString;
           Chart4.AxisList[0].Visible:=true;
           try
             ts1:=StrToInt(LabeledEdit17.Text)*100; {Sollwerte per String}
             ts2:=StrToInt(LabeledEdit18.Text)*100;
             ts3:=StrToInt(LabeledEdit19.Text)*100;
           except
             ts1:=0;
             ts2:=0;
             ts3:=0;
           end;
         end;
      3: begin                        {Spannung}
           Chart4.AxisList[0].Title.Caption:=rsSpg+' [V]';
         end;
      4: begin                        {Strom}
           Chart4.AxisList[0].Title.Caption:=rsStrom+' [A]';
         end;
      5: begin                        {Sweep: X: Leistung über Y: Spannung }
           Chart4.AxisList[1].Marks.Source:=nil;
           Chart4.AxisList[1].Marks.Format:='%0:.9g';
           Chart4.AxisList[0].Title.Caption:=rsLei+' [W]';
           Chart4.AxisList[1].Title.Caption:=rsSpg+' [V]';
           Chart4.AxisList[1].Title.Visible:=true;
           ProgressBar1.Min:=0;
           ProgressBar1.Max:=11;
           ProgressBar1.Position:=0;
         end;
    end;
    SweepList:=TStringList.Create;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    peak:=0;
    ee:=0;
    gw:=0;
    gx:=0;
    erl:=false;
    try
      for x:=GetOutListIDX(ComboBox1.Text) to OutList.Count-1 do begin
        if ComboBox1.Text=copy(OutList[x], 1, 10) then begin
          erl:=true;
          SplitList.DelimitedText:=OutList[x];
          w:=StrToInt(SplitList[2]);
          if w>peak then peak:=w;
          w:=StrToInt(SplitList[3]);
          if w>ee then ee:=w;
          itv:=StrToInt(SplitList[13]);        {Meßintervall  60 oder 600}
          dpt:=TToTT(SplitList[1]);            {Zeitpunkt nur einmal machen}
          case RadioGroup2.ItemIndex of
            0: begin                                                {Ertrag}
                 if w>0 then begin
                   Chart4LineSeries4.AddXY(dpt, w/1000);
                   if CheckBox8.Checked then begin
                     er1:=er1+((StrToInt(SplitList[5])*
                                StrToInt(SplitList[6])/10000000)*(itv/3600));
                     er2:=er2+((StrToInt(SplitList[7])*
                                StrToInt(SplitList[8])/10000000)*(itv/3600));
                     er3:=er3+((StrToInt(SplitList[9])*
                                StrToInt(SplitList[10])/10000000)*(itv/3600));
                     if er1>0 then Chart4LineSeries1.AddXY(dpt, er1);
                     if er2>0 then Chart4LineSeries2.AddXY(dpt, er2);
                     if er3>0 then Chart4LineSeries3.AddXY(dpt, er3);
                    end;
                 end;
               end;
            1: begin                                                 {Leistung}
                 w:=StrToInt(SplitList[2]);      {Leistung in Watt (Einzelwert}
                 if TrackBar2.Position>1 then begin                  {Glättung}
                   gw:=gw+w;                     {Summe für Mittelwert}
                   inc(gx);                      {Glättungsfaktor}
                   if gx=TrackBar2.Position then begin       {Gesamt geglättet}
                     Chart4LineSeries4.AddXY(dpt, gw/TrackBar2.Position/1000);
                     gw:=0;
                     gx:=0;
                   end;
                 end else Chart4LineSeries4.AddXY(dpt, w/1000);      {Gesamt}
                 if CheckBox8.Checked then begin
                   w:=StrToInt(SplitList[5]);
                   if w>0 then Chart4LineSeries1.AddXY(dpt,
                                           w*StrToInt(SplitList[6])/10000000);
                   w:=StrToInt(SplitList[7]);
                   if w>0 then Chart4LineSeries2.AddXY(dpt,
                                           w*StrToInt(SplitList[8])/10000000);
                   w:=StrToInt(SplitList[9]);
                   if w>0 then Chart4LineSeries3.AddXY(dpt,
                                           w*StrToInt(SplitList[10])/10000000);
                 end;
               end;
            2: begin                                                 {String %}
                 w:=StrToInt(SplitList[5]);
                 if (w>0) and (ts1>0) then Chart4LineSeries1.AddXY(dpt,
                                           w*StrToInt(SplitList[6])/ts1);
                 w:=StrToInt(SplitList[7]);
                 if (w>0) and (ts2>0) then Chart4LineSeries2.AddXY(dpt,
                                           w*StrToInt(SplitList[8])/ts2);
                 w:=StrToInt(SplitList[9]);
                 if (w>0) and (ts3>0) then Chart4LineSeries3.AddXY(dpt,
                                           w*StrToInt(SplitList[10])/ts3);
               end;
            3: begin                                                 {Spannung}
                 w:=StrToInt(SplitList[5]);
                 if Shape1.Brush.Color<>clDefault then
                   Chart4LineSeries1.AddXY(dpt, w/10);
                 w:=StrToInt(SplitList[7]);
                 if Shape2.Brush.Color<>clDefault then
                   Chart4LineSeries2.AddXY(dpt, w/10);
                 w:=StrToInt(SplitList[9]);
                 if Shape3.Brush.Color<>clDefault then
                   Chart4LineSeries3.AddXY(dpt, w/10);
               end;
            4: begin                                                 {Strom}
                 w:=StrToInt(SplitList[6]);
                 if Shape1.Brush.Color<>clDefault then
                   Chart4LineSeries1.AddXY(dpt, w/1000);
                 w:=StrToInt(SplitList[8]);
                 if Shape2.Brush.Color<>clDefault then
                   Chart4LineSeries2.AddXY(dpt, w/1000);
                 w:=StrToInt(SplitList[10]);
                 if Shape3.Brush.Color<>clDefault then
                   Chart4LineSeries3.AddXY(dpt, w/1000);
               end;
            5: begin    {Sweep: erstmal Werte einsammeln und dann darstellen}
                 if StrToInt(SplitList[2])>StrToInt(edMinPower.Text) then begin
                   SweepList.Add(OutList[x]);      {Werte übernehmen}
                   w:=StrToInt(SplitList[5]);      {Spannung DC 1}
                   if w>umax then umax:=w;
                   if (w>2000) and (w<umin) then umin:=w;
                   w:=StrToInt(SplitList[7]);      {Spannung DC 2}
                   if w>umax then umax:=w;
                   if (w>2000) and (w<umin) then umin:=w;
                   w:=StrToInt(SplitList[9]);      {Spannung DC 3}
                   if w>umax then umax:=w;         {maximale Spannung}
                   if (w>2000) and (w<umin) then umin:=w;  {minimale Spannung}
                   w:=x*100 div OutList.Count;     {nur Anzeige}
                   if (w mod 10)=0 then ProgressBar1.Position:=w div 10;
                 end;
               end;
          end;
        end else if erl then break;                {Abbrechen, wenn Tag fertig}
      end;
      ee:=GetKErt(ee);
      s:=FloatToStr(ee/1000)+'kWh';
      Chart4.Title.Text[0]:=rsTagesaw+' '+
              FormatDateTime('dddd, dd. mmmm yyyy', SDToTime(ComboBox1.Text))+
              '    ['+s+']';
      StatusBar1.Panels[2].Text:='Maximale '+rsLei+rsAm+ComboBox1.Text+
                                 dtr+FloatToStr(peak/1000)+'kW';
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
      Label3.Caption:=FloatToStr(peak/1000)+'kW';
      Label5.Caption:=s;
      Label20.Caption:=FloatToStrF(ee/pla, ffFixed, 12, 3)+'kWh/kWp';
      Label7.Caption:=FloatToStrF(ee/1000*Geteevg, ffFixed, 12, 2)+edEuro.Text;
      SpeedButton1.Enabled:=not (ComboBox1.ItemIndex=0);
      SpeedButton2.Enabled:=not (ComboBox1.ItemIndex=ComboBox1.Items.Count-1);
      if RadioGroup2.ItemIndex=5 then begin  {Sweep Diagramm zeichnen}
        umax:=((umax div 100)+1)*100;        {Diagrammgrenzen festlegen}
        umin:=(umin div 100)*100;
        for x:=umin to umax do begin         {Anzeige erstellen}
          for y:=0 to SweepList.Count-1 do begin
            SplitList.DelimitedText:=SweepList[y];
            w:=StrToInt(SplitList[5]);       {String 1}
            if w=x then Chart4LineSeries1.AddXY(x/10,w*StrToInt(SplitList[6])/10000);
            w:=StrToInt(SplitList[7]);       {String 2}
            if w=x then Chart4LineSeries2.AddXY(x/10,w*StrToInt(SplitList[8])/10000);
            w:=StrToInt(SplitList[9]);       {String 3}
            if w=x then Chart4LineSeries3.AddXY(x/10,w*StrToInt(SplitList[10])/10000);
          end;
        end;
        ProgressBar1.Position:=ProgressBar1.Max;
      end;
      if btnTag.Tag=1 then begin            {Diesen Tag wieder herstellen}
        SweepList.Clear;
        SweepList.Add(FormatDateTime(isodate, SDToTime(ComboBox1.Text))+sep+
                      '000000'+sep+edUser.Text+sep+
                      StatusBar1.Panels[0].Text+sep+'+0100');
        SweepList.Add(infbeg);
        SweepList.Add(StatusBar1.Panels[1].Text+sep+'Inverter Type'+sep+
                      StatusBar1.Panels[0].Text+sep+StatusBar1.Panels[1].Text);
        SweepList.Add(infend);
        SweepList.Add(datbeg);
        SweepList.Add(headerID+';TIMESTAMP;SERIAL;P_AC;E_DAY;T_WR;U_AC;U_AC_1;'+
                      'U_AC_2;U_AC_3;I_AC;F_AC;U_DC_1;I_DC_1;U_DC_2;I_DC_2;'+
                      'U_DC_3;I_DC_3;S;E_WR;M_WR;I_AC_1;I_AC_2;I_AC_3;P_AC_1;'+
                      'P_AC_2;P_AC_3;F_AC_1;F_AC_2;F_AC_3;R_DC;PC;PCS;'+
                      'PCS_LL;COS_PHI;COS_PHI_LL;S_COS_PHI');
        for x:=0 to OutList.Count-1 do begin           {Rohdaten durchsuchen}
          if ComboBox1.Text=copy(OutList[x], 1, 10) then begin {Tag selektieren}
            SplitList.DelimitedText:=OutList[x];
            fr:=MakeFloatStr(SplitList[12], 2, '.')+sep;
            SweepList.Add(SplitList[13]+sep+ComboBox1.Text+' '+SplitList[1]+':00'+
                          sep+StatusBar1.Panels[1].Text+sep+SplitList[2]+sep+
                          MakeFloatStr(SplitList[3], 3, '.')+sep+
                          SplitList[4]+sep+SplitList[11]+sep+SplitList[11]+sep+
                          SplitList[11]+sep+SplitList[11]+sep+'0.000'+sep+fr+
                          MakeFloatStr(SplitList[5], 1, '.')+sep+    {U_DC_1}
                          MakeFloatStr(SplitList[6], 3, '.')+sep+
                          MakeFloatStr(SplitList[7], 1, '.')+sep+
                          MakeFloatStr(SplitList[8], 3, '.')+sep+
                          MakeFloatStr(SplitList[9], 1, '.')+sep+
                          MakeFloatStr(SplitList[10], 3, '.')+sep+   {I_DC_3}
                          nix+nix+nix+'0.000'+sep+'0.000'+sep+'0.000'+sep+
                          nix+nix+nix+fr+fr+fr+SplitList[14]+sep+    {R_DC}
                          '1000'+sep+'0.0'+sep+nix+'1.0000'+sep+nix+'0');
          end;
        end;
        SweepList.Add(datend);
        s:=IncludeTrailingPathDelimiter(deLocal.Directory)+
           copy(edFilter.Text,1,length(edFilter.Text)-1)+
           FormatDateTime('YYMMDD', SDToTime(ComboBox1.Text))+'000000';
        SweepList.SaveToFile(s);
        StatusBar1.Panels[2].Text:=ExtractFileName(s)+' wiederhergestellt';
        SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
      end;
    finally
      SplitList.Free;
      SweepList.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.MonStat;                            {Monatsauswertung}
var SplitList: TStringList;
    x, w, ee, eeges, peak, dz, pla: Integer;
    t: string;
    erl, gef: boolean;
begin
  Chart2ConstantLine1.Active:=false;      {Durchschnittlicher TE}
  Chart2ConstantLine1.SeriesColor:=ColorButton10.ButtonColor; {default: nicht erreicht}
  Chart2ConstantLine2.Active:=false;      {Soll}
  Chart2ConstantLine2.SeriesColor:=ColorButton8.ButtonColor;
  pla:=StrToInt(edPeak.Text);
  if CheckBox10.Checked then Chart2.AxisList[0].Title.Caption:=rsErtrag+' '+
                                                         rsNormiert+' [kWh/kWp]'
                        else Chart2.AxisList[0].Title.Caption:=rsErtrag+' [kWh]';
  if ComboBox2.Text[1] in ziff then begin
    Screen.Cursor:=crHourGlass;
    Chart2.Title.Text[0]:=rsErtrag+' '+
                          FormatDateTime('mmmm yyyy', SDToTime(ComboBox2.Text));
    Chart2BarSeries1.Clear;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    ee:=0;                   {Energieertrag}
    eeges:=0;
    peak:=0;                 {maximale Leistung}
    dz:=0;                   {Tageszähler}
    t:='';                   {Tag}
    erl:=false;
    gef:=false;
    try
{Monatserträge aus dem Archiv}
      if DayList.Count>0 then for x:=0 to DayList.Count-1 do begin
        if ComboBox2.Text=copy(DayList[x],1,7) then begin
          gef:=true;
          SplitList.DelimitedText:=DayList[x];
          w:=StrToInt(SplitList[1]);
          if w>peak then peak:=w;
          ee:=GetKErt(StrToInt(SplitList[2]));
          eeges:=eeges+ee;
          t:=Copy(DayList[x],9,2);
          if CheckBox10.Checked then Chart2BarSeries1.Add(ee/pla, t, ColorButton7.ButtonColor)
                                else Chart2BarSeries1.Add(ee/1000, t, ColorButton7.ButtonColor);
          inc(dz);
        end else if gef then begin
          erl:=true;
          break;
        end;
      end;
      ee:=0;
      t:='';
      if not erl then for x:=GetOutListIDX(ComboBox2.Text) to OutList.Count-1 do begin
        if ComboBox2.Text=copy(OutList[x],1,7) then begin
          erl:=true;
          SplitList.DelimitedText:=OutList[x];
          w:=StrToInt(SplitList[2]);
          if w>peak then peak:=w;
          w:=StrToInt(SplitList[3]);
          if w>ee then ee:=w;                     {größten Wert des Tages merken}
          if t<>Copy(OutList[x],9,2) then begin   {neuer Tag}
            if t<>'' then begin
              ee:=GetKErt(ee);
              if CheckBox10.Checked then Chart2BarSeries1.Add(ee/pla, t, ColorButton7.ButtonColor)
                                    else Chart2BarSeries1.Add(ee/1000, t, ColorButton7.ButtonColor);
              eeges:=eeges+ee;
              inc(dz);
            end;
            t:=Copy(OutList[x],9,2);
            ee:=0;
          end;
        end else if erl then break;
      end;
      ee:=GetKErt(ee);
      eeges:=eeges+ee;
      if (ee>0) and (t<>'') then begin
        if CheckBox10.Checked then Chart2BarSeries1.Add(ee/pla, t, ColorButton7.ButtonColor)
                              else Chart2BarSeries1.Add(ee/1000, t, ColorButton7.ButtonColor);
        inc(dz);
      end;
      t:=FloatToStr(eeges/1000)+'kWh';
      Chart2.Title.Text[0]:=rsErtrag+' '+
                         FormatDateTime('mmmm yyyy', SDToTime(ComboBox2.Text))+
                         '     ['+t+']';
      StatusBar1.Panels[2].Text:='Durchschnittlicher Tagesertrag für '+MonToTxt(ComboBox2.Text)+
                                 dtr+FloatToStrF(eeges/1000/dz, ffFixed,12, 3)+'kWh';
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
      Label9.Caption:=FloatToStr(peak/1000)+'kW';    {Peakleistung}
      Label42.Caption:=FloatToStrF(GetSJE*pla*       {Soll}
        StrToIntDef(StringGrid1.Cells[StrToInt(copy(ComboBox2.Text, 6, 2))-1, 1], 0)/
                                   SumGrid/1000, ffFixed, 12, 3)+'kWh';
      Label11.Caption:=t;
      Label21.Caption:=FloatToStrF(eeges/pla, ffFixed, 12, 3)+'kWh/kWp';
      Label13.Caption:=FloatToStrF(eeges/1000*Geteevg, ffFixed, 12, 2)+edEuro.Text;
      if (eeges/dz)>(GetSJE*pla*
          StrToIntDef(StringGrid1.Cells[StrToInt(copy(ComboBox2.Text, 6, 2))-1, 1], 0)/SumGrid/
          DaysInAMonth(StrToInt(copy(ComboBox2.Text,1,4)), StrToInt(copy(ComboBox2.Text,6,2))))
        then Chart2ConstantLine1.SeriesColor:=ColorButton9.ButtonColor;
      if CheckBox10.Checked then begin               {normiert}
        Chart2ConstantLine1.Position:=eeges/pla/dz;  {durchschn. Tagesertrag}
        Chart2ConstantLine2.Position:=GetSJE*
          StrToIntDef(StringGrid1.Cells[StrToInt(copy(ComboBox2.Text, 6, 2))-1, 1], 0)/SumGrid/
          DaysInAMonth(StrToInt(copy(ComboBox2.Text,1,4)), StrToInt(copy(ComboBox2.Text,6,2)));
      end else begin                                 {absolut}
        Chart2ConstantLine1.Position:=eeges/1000/dz; {durchschn. Tagesertrag}
        Chart2ConstantLine2.Position:=GetSJE*pla*
          StrToIntDef(StringGrid1.Cells[StrToInt(copy(ComboBox2.Text, 6, 2))-1, 1], 0)/SumGrid/
          1000/DaysInAMonth(StrToInt(copy(ComboBox2.Text,1,4)), StrToInt(copy(ComboBox2.Text,6,2)));
      end;
      if dz>1 then begin
        Chart2ConstantLine1.Active:=true;
        Chart2ConstantLine2.Active:=true;
      end;
      if dz<4 then for x:=1 to 4 do Chart2BarSeries1.Add(0, '', clBtnFace);
      SpeedButton4.Enabled:=not (ComboBox2.ItemIndex=0);
      SpeedButton3.Enabled:=not (ComboBox2.ItemIndex=ComboBox2.Items.Count-1);
    finally
      SplitList.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.JahrStat;                           {Jahreserträge}
var SplitList: TStringList;
    t, m, vj: string;
    x, ee, eeges, jges, peak, mz, pla, zhl, dz, gsum, w: integer;
    displ, sollw: double;
    erl, gef: boolean;
begin
  pla:=StrToInt(edPeak.Text);
  gsum:=SumGrid;
  if CheckBox7.Checked then Chart3.AxisList[0].Title.Caption:=rsErtrag+' '+
                                                        rsNormiert+' [kWh/kWp]'
                       else Chart3.AxisList[0].Title.Caption:=rsErtrag+' [kWh]';
  if ComboBox3.Text[1] in ziff then begin
    Screen.Cursor:=crHourGlass;
    Chart3ConstantLine1.Active:=false;      {Durchschnittlicher Monatsertrag}
    Chart3ConstantLine1.SeriesColor:=ColorButton10.ButtonColor;
    Chart3ConstantLine2.Active:=false;      {durchschnittlicher Sollertrag}
    Chart3ConstantLine2.SeriesColor:=ColorButton8.ButtonColor;
    Chart3BarSeries1.Clear;
    Chart3BarSeries2.Clear;
    SplitList:=TStringList.Create;
    SplitList.Delimiter:=sep;
    eeges:=0;                {Energieertrag gesamt im Monat}
    jges:=0;                 {Energieertrag gesamt im Jahr}
    peak:=0;                 {maximale Leistung}
    zhl:=0;                  {Tageszähler gesamt}
    dz:=0;                   {Tageszähler im Monat}
    t:='';                   {Tag}
    m:='';
    vj:='';                  {letzte 3 Monate des Vorjahres auch darstellen}
    erl:=false;
    gef:=false;
    if (copy(OutList[OutList.Count-1],6,2)<'04') and    {Letzten Tag prüfen}
       (copy(OutList[OutList.Count-1],1,4)=ComboBox3.Text)
      then vj:=IntToStr(StrToInt(copy(OutList[OutList.Count-1],1,4))-1);
    try
      if DayList.Count>0 then for x:=0 to DayList.Count-1 do begin
        if (ComboBox3.Text=copy(DayList[x],1,4)) or
           ((copy(DayList[x],1,4)=vj) and (copy(DayList[x],6,1)='1')) then begin
          SplitList.DelimitedText:=DayList[x];
          gef:=true;
          if m<>copy(DayList[x],1,7) then begin {neuer Monat auch noch}
            if m<>'' then begin
              if CheckBox7.Checked then begin
                displ:=eeges/pla;
                sollw:=GetSJE*
                  StrToIntDef(StringGrid1.Cells[StrToInt(copy(m,6,2))-1, 1], 0)/gsum;
              end else begin
                displ:=eeges/1000;
                sollw:=GetSJE*pla*
                  StrToIntDef(StringGrid1.Cells[StrToInt(copy(m,6,2))-1, 1], 0)/gsum/1000;
              end;
              if dz=DaysInMonth(SDToTime(m)) then begin
                Chart3BarSeries1.Add(displ, MonToTxt(m),ColorButton12.ButtonColor);
                Chart3BarSeries2.Add(sollw, '', ColorButton14.ButtonColor);   {Soll}
              end else begin
                Chart3BarSeries1.Add(displ, MonToTxt(m),ColorButton13.ButtonColor);
                Chart3BarSeries2.Add(sollw, '', ColorButton14.ButtonColor);
              end;
            end;
            m:=copy(DayList[x],1,7);
            eeges:=0;
            dz:=0;
          end;
          ee:=StrToInt(SplitList[1]);    {Leistung}
          if ee>peak then peak:=ee;
          ee:=GetKErt(StrToInt(SplitList[2]));    {korrigierter Tagesertrag}
          eeges:=eeges+ee;               {Ertrag für Monat aufaddieren}
          if ComboBox3.Text=copy(DayList[x],1,4) then begin
            jges:=jges+ee;               {Jahresertrag}
            inc(zhl);
          end;
          inc(dz);
        end else if gef then begin
          erl:=true;
          break;
        end;
      end;
      ee:=0;                   {Energieertrag gesamt am Tag}
      if vj='' then w:=GetOutListIDX(ComboBox3.Text)
               else w:=GetOutListIDX(vj+'-10-01');
      if not erl then for x:=w to OutList.Count-1 do begin
        if (ComboBox3.Text=copy(OutList[x],1,4)) or
           ((copy(OutList[x],1,4)=vj) and (copy(OutList[x],6,1)='1')) then begin
          erl:=true;
          SplitList.DelimitedText:=OutList[x];
          if t<>copy(OutList[x],1,10) then begin  {neuer Tag}
            ee:=GetKErt(ee);
            eeges:=eeges+ee;                      {Ertrag für Monat aufaddieren}
            if ComboBox3.Text=copy(t,1,4) then jges:=jges+ee;
            if m<>copy(OutList[x],1,7) then begin {neuer Monat auch noch}
              if m<>'' then begin
                if CheckBox7.Checked then begin
                  displ:=eeges/pla;
                  sollw:=GetSJE*
                    StrToIntDef(StringGrid1.Cells[StrToInt(copy(m,6,2))-1, 1], 0)/gsum;
                end else begin
                  displ:=eeges/1000;
                  sollw:=GetSJE*pla*
                    StrToIntDef(StringGrid1.Cells[StrToInt(copy(m,6,2))-1, 1], 0)/gsum/1000;
                end;
                if dz=DaysInMonth(SDToTime(m)) then begin
                  Chart3BarSeries1.Add(displ, MonToTxt(m),ColorButton12.ButtonColor);
                  Chart3BarSeries2.Add(sollw, '', ColorButton14.ButtonColor);   {Soll}
                end else begin
                  Chart3BarSeries1.Add(displ, MonToTxt(m),ColorButton13.ButtonColor);
                  Chart3BarSeries2.Add(sollw, '', ColorButton14.ButtonColor);
                end;
              end;
              m:=copy(outList[x],1,7);
              eeges:=0;
              dz:=0;
            end;
            t:=Copy(OutList[x],1,10);
            if ComboBox3.Text=copy(t,1,4) then inc(zhl);
            inc(dz);
            ee:=0;
          end;
          w:=StrToInt(SplitList[2]);
          if (ComboBox3.Text=copy(OutList[x],1,4)) and (w>peak) then peak:=w;
          w:=StrToInt(SplitList[3]);
          if w>ee then ee:=w;               {größten Wert des Tages merken}
        end else if erl then break;
      end;
      ee:=GetKErt(ee);
      eeges:=eeges+ee;                      {Ertrag für Monat aufaddieren}
      if ComboBox3.Text=copy(t,1,4) then jges:=jges+ee;
      if (m<>'') and (eeges>0) then begin
        if CheckBox7.Checked then begin     {normiert in kWh/kWp}
          displ:=eeges/pla;
          sollw:=GetSJE*
            StrToIntDef(StringGrid1.Cells[StrToInt(copy(m,6,2))-1, 1], 0)/gsum;
        end else begin                      {absolut}
          displ:=eeges/1000;
          sollw:=GetSJE*pla*
            StrToIntDef(StringGrid1.Cells[StrToInt(copy(m,6,2))-1, 1], 0)/gsum/1000;
        end;
        if dz=DaysInMonth(SDToTime(m)) then begin
          Chart3BarSeries1.Add(displ, MonToTxt(m), ColorButton12.ButtonColor);
          Chart3BarSeries2.Add(sollw, '', ColorButton14.ButtonColor);
        end else begin
          Chart3BarSeries1.Add(displ, MonToTxt(m), ColorButton13.ButtonColor);
          Chart3BarSeries2.Add(sollw, '', ColorButton14.ButtonColor);
        end;
      end;
      mz:=0;                  {Anzahl Monate im ausgewählten Jahr zählen}
      for x:=0 to ComboBox2.Items.Count-1 do
        if ComboBox3.Text=copy(ComboBox2.Items[x], 1, 4) then inc(mz);
      Chart3.Title.Text[0]:=rsYearPr+ComboBox3.Text+dtr+FloatToStr(jges/1000)+'kWh';
      StatusBar1.Panels[2].Text:=rsAvProdM+dtr+
                                 FloatToStrf(jges/1000/mz, ffFixed,12, 3)+'kWh';
      SynMemo1.Lines.Add(StatusBar1.Panels[2].Text);
      Label15.Caption:=FloatToStr(peak/1000)+'kW';
      Label17.Caption:=FloatToStr(jges/1000)+'kWh';
      Label22.Caption:=FloatToStrF(jges/pla, ffFixed, 12, 3)+'kWh/kWp';
      Label19.Caption:=FloatToStrF(jges/1000*Geteevg, ffFixed, 12, 2)+edEuro.Text;
{spezifischer Jahresertrag, hochgerechnet, wenn Jahr noch nicht voll}
      Label24.Caption:=FloatToStrF(jges/pla/zhl*DaysInAYear(StrToInt(ComboBox3.Text)),
                                   ffFixed, 12, 3)+'kWh/kWp';
{durchschn. Monatsertrag als Linie anzeigen}
      if (jges/mz)>(GetSJE*pla/12) then Chart3ConstantLine1.SeriesColor:=ColorButton9.ButtonColor;
      if CheckBox7.Checked then begin      {normiert}
        Chart3ConstantLine1.Position:=jges/pla/mz;
        Chart3ConstantLine2.Position:=GetSJE/12;
      end else begin                       {absolut}
        Chart3ConstantLine1.Position:=jges/1000/mz;
        Chart3ConstantLine2.Position:=GetSJE*pla/12000;
      end;
      Chart3ConstantLine1.Active:=true;  {Durchschnittlicher Monatsertrag}
      Chart3ConstantLine2.Active:=true;  {durchschnittlicher Sollertrag}
      SpeedButton6.Enabled:=not (ComboBox3.ItemIndex=0);
      SpeedButton5.Enabled:=not (ComboBox3.ItemIndex=ComboBox3.Items.Count-1);
    finally
      SplitList.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

end.

