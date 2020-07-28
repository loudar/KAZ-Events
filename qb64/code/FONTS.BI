DIM SHARED r&
DIM SHARED h&
DIM SHARED b&
DIM SHARED fontheight
DIM SHARED fontwidth
fontfileh$ = netpath$ + "data\fonts\akira_expanded.otf" 'font-datei fur uberschrift
fontheight = 32
h& = _LOADFONT(fontfileh$, fontheight)
fontfiler$ = netpath$ + "data\fonts\consola.ttf" 'font-datei fur regular font
'fontfiler$ = "QD.otf"
fontheight = rfontheight
r& = _LOADFONT(fontfiler$, fontheight, "MONOSPACE")
fontfileb$ = netpath$ + "data\fonts\consolab.ttf" 'font-datei fur info feld rechts
b& = _LOADFONT(fontfileb$, fontheight, "MONOSPACE")
_FONT r&
fontwidth = _FONTWIDTH(r&)
DIM SHARED maxrows 'maximale anzahl an spalten
DIM SHARED maxlines 'maximale anzahl an zeilen
IF bigwindow = 0 THEN
    maxrows = INT((swidth / 1.5) / fontwidth)
    maxlines = INT((sheight / 2.2) / fontheight) - 4
ELSE
    maxrows = INT(swidth / fontwidth)
    maxlines = INT(sheight / fontheight) - 4
END IF
DIM SHARED firstline 'grund-spalte
DIM SHARED firstchar 'grund-zeile
firstline = INT((75 + fontheight) / fontheight)
firstchar = 6
