# APPleTree

## Allgemeine Informationen
*APPleTree* ist eine R Shiny-Applikation, mit der du die Daten von **TreesCount! 2015**, einer Art Volkszählung von Straßenbäumen in New York City, herunterladen und in einer Tabelle ansehen kannst. Weiterhin gibt es verschiedene Möglichkeiten die Daten zu filtern, sowohl in der Sidebar mit Mehrfachauswahl als auch mit Einfach-Filtern direkt in der Tabelle. Um dir einen Überblick über die Daten zu verschaffen, kannst du eine Variable und eine Klasseneinteilung auswählen und dir als interaktives Balken- bzw. Violindiagramm anzeigen lassen. Die gefilterten und klassifizierten Daten werden ebenso in einer interaktiven Karte angezeigt. Falls dir mal eine Baumart nichts sagt, kannst du sie dir in der Baumfibel anschauen. Hier greift das Programm auf eine externe MySQL-Datenbank zu; so bleibt die App schön schlank und benötigt lediglich 180 kB Speicherplatz.

## Voraussetzungen
Neben einer base R Installation müssen folgende Pakete installiert sein:

```{undefined}
shiny
shinymaterial
shinyWidgets
DT
tidyverse
DBI
RMySQL
leaflet
leafgl
sf
RColorBrewer
ggplot2
plotly
```

Diese können, falls noch nicht vorhanden, einzeln...

```{undefined}
install.packages("Name des Pakets")
```

... oder kollektiv...

```{undefined}
install.packages(c("Name Paket 1","Name Paket 2",...))
```

... installiert werden.

## Starten der App
Zum Starten `app.R` in RStudio öffnen und entweder den `Run App`-Knopf rechts oben drücken oder den Befehl 

```{undefined}
runApp("~/app.R")
```

in der Konsole eingeben. Alternativ kann die App auch aus dem Terminal gestartet werden mit:

```{undefined}
R -e "shiny::runApp('~/app.R')"
```

In beiden Fällen muss '~/app.R' noch mit dem Pfad zum Arbeitsverzeichnis ergänzt werden, z. B. 'home/user/work/R-scripts/APPleTree/app.R'.

## Laden des Datensatzes
Nachdem die App im Browser erscheint, klicke einfach links oben unter dem Baum-Logo auf `Hole Datensatz vom Server`. Der Datensatz ist ca. 250 MB groß und kann daher etwas Zeit zum Laden in Anspruch nehmen. Einfach ~1 Minute warten, bis sich die Tabelle, Karte und Diagramm aufgebaut haben. Danach sollte alles flüssig gehen. Die Datei kannst du auch lokal auf deinem Rechner als CSV speichern... aber ACHTUNG, wenn Filter aktiv sind, wird auch nur der gefilterte Datensatz gespeichert.
<br>
<br>
Viel Spaß beim Ausprobieren!
<br>
<br>
<br>
