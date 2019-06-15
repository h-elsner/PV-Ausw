# PV-Ausw
Photo voltaic statistics for Danfoss TLX series
Der TripleLynx Pro Wechselrichter von Danfoss® oder IBC Solar AG® (FLX Pro, TLX Pro oder TLX Pro +) zeichnet die Ertragsdaten der Photovoltaik-Anlage (kurz PV-Anlage) auf und stellt diese über FTP-Push auf einem „Datawarehouse“ bezeichneten Server ab. Der Dateiname besteht aus dem FTP-Benutzernamen, einem Bindestrich und einem Zeitstempel.
Siehe dazu die Anleitung im Benutzerhandbuch des Webservers vom Danfoss-Wechselrichter. Dokumentationen, Updates und neue Firmware gibt es online im Downloadbereich auf SMA bzw. bei jeweiligen Hersteller des Wechselrichters.

Die CSV-Dateien vom Wechselrichter Powador TL3-Serie mit neuer Firmware ab V1.16 von Kaco new energy GmbH® und von SMA® Sonny Boy können ebenfalls ausgewertet werden. Allerdings gibt es einige Einschränkungen:
- Bei Kaco wird die Netzfrequenz nicht geliefert ist fest auf 50,00Hz eingestellt,
- der Isolationswiderstand wird nicht geliefert wird zwar als ‚0‘ angezeigt, aber keine Angst, es gibt ihn,
- bei Kaco wird der momentane Ertrag nicht geliefert, sondern berechnet und nachträglich mit den Werten aus der Monatsdatei korrigiert, um Integrationsfehler in der Monats- und Jahresauswertung zu vermeiden,
- dafür wird zwar die DC-Leistung geliefert, vom Programm aber ignoriert und später  aus DC-Strom und DC-Spannung wieder berechnet (wegen Abwärtskompatibilität der Backupdaten zu älteren Versionen),
- bei SMA wird die Wechselrichtertemperatur nicht geliefert - fix auf 18°C eingestellt,
- Wiederherstellung einer Tagesdatei aus Backupdaten ist nicht möglich.

Das Programm „PV_Ausw“ dient nun dazu, diese Dateien auszuwerten und grafisch darzustellen, ähnlich wie es die Oberfläche des Webservers bei Danfoss/IBC bietet und etwas mehr. Die Auswertung ist nur für einen Wechselrichter (=Master) gemacht. Dies sollte ja auch der häufigste Fall für kleine Anlagen sein.
