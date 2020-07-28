DIM SHARED inputcount 'anzahl von input-menuitems
DIM SHARED endparameter$ 'hat einen wert pro menu, wird genutzt, um zu schauen, was das menu beendet hat
DIM SHARED endparameterbf$
DIM SHARED endparameterbfbf$
DIM SHARED endparameterbfbfbf$

DIM SHARED menulimit
menulimit = 50 'maximale anzahl von gleichzeitig moglichen menuitems
DIM SHARED interactable
DIM SHARED allsel(menulimit)
DIM SHARED type$(menulimit) 'art des menuitems (toggle, text, input, menitem, function)
DIM SHARED text$(menulimit) 'angezeigter, unveranderbarer text von m
DIM SHARED yoffset(menulimit) 'zu platzierende zeile von m, pixel = zeile(m) / fontheight
DIM SHARED xoffset(menulimit) 'offset nach rechts, pixel = xoffset(m) / fontwidth
DIM SHARED basex(menulimit) 'linker rand von m
DIM SHARED basey(menulimit) 'oberer rand von m
DIM SHARED endx(menulimit) 'rechter rand von m
DIM SHARED endy(menulimit) 'unterer rand von m
DIM SHARED destination$(menulimit) 'ziel von m bei interaktion                        /
DIM SHARED setting$(menulimit) 'zu verandernde einstellung bei interaktion          /
DIM SHARED firstprint(menulimit) 'wenn 1: wurde mindestens einmal auf den bildschirm geschrieben, wenn 0: wird m bei nachster gelegenheit neu anzeigen
DIM SHARED state(menulimit) 'status von m, vor allem relevant fur toggle / function
DIM SHARED mode(menulimit) 'speicher fur selectoren den mode, in dem sie operieren sollen
DIM SHARED selected(menulimit) 'speichert die aktuell ausgewahlte instanz von selector m
DIM SHARED arraydata$(menulimit, 10000)
DIM SHARED maxad(menulimit)
DIM SHARED array(menulimit)
DIM SHARED minute(menulimit)
DIM SHARED hour(menulimit)
DIM SHARED day(menulimit)
DIM SHARED month(menulimit)
DIM SHARED year(menulimit)
DIM SHARED change(0 TO menulimit)
DIM SHARED changevalxl(menulimit)
DIM SHARED changevalxr(menulimit)
DIM SHARED increasevalyu(menulimit)
DIM SHARED increasevalyb(menulimit)
DIM SHARED decreasevalyu(menulimit)
DIM SHARED decreasevalyb(menulimit)
DIM SHARED melementxl(menulimit, 10000)
DIM SHARED melementxr(menulimit, 10000)
DIM SHARED number(menulimit)
DIM SHARED m 'aktuelles menuitem
DIM SHARED maxm
DIM SHARED clr&(menulimit)
DIM SHARED weight$(menulimit)
'slider
DIM SHARED maxval(menulimit) '                   maximum value of slider element
DIM SHARED minval(menulimit) '                   minimum value of slider element
DIM SHARED value(menulimit) '                    current value of slider element
'triangles :D
DIM SHARED triscale
triscale = 0.8
DIM SHARED p1x(menulimit)
DIM SHARED p1y(menulimit)
DIM SHARED p2x(menulimit)
DIM SHARED p2y(menulimit)
DIM SHARED p3x(menulimit)
DIM SHARED p3y(menulimit)
'start der input-variablen
DIM SHARED alch 'maximale zahl akzeptierter zeichen
DIM SHARED char$(menulimit, 500) 'speichert den buchstaben an position g pro element m
DIM SHARED zeichen$(500) 'das gleiche wie char$, aber exklusiv fur die suchfunktion...konnte man auch zusammenfuhren
DIM SHARED g(menulimit) 'aktuelle anzahl an zeichen des bearbeiteten textes
DIM SHARED gbf(menulimit) 'g vom vorigen loop-durchlauf
DIM SHARED ac$(500) 'akzeptierte input-zeichen
DIM SHARED UserInput$(menulimit) 'speichert den input von m
'Suche
DIM SHARED suchbegriff$
DIM SHARED lastsearchinput$ 'letzte sucheingabe, wird benutzt, wenn man aus der einzelansicht zuruckkehrt
DIM SHARED searched$(50)
DIM SHARED listnode(0 TO 500) 'speichert die interne ID des in einer liste anzuzeigendes objektes
DIM SHARED o
DIM SHARED maxo
DIM SHARED reprinto 'indiziert, ob die liste neu angezeigt werden soll
DIM SHARED listID$
DIM SHARED listID
DIM SHARED buttonstatus
DIM SHARED buttonlx
DIM SHARED buttonux
DIM SHARED buttonly
DIM SHARED buttonuy
'Graph
DIM SHARED pcount(15)
DIM SHARED pheight(15)
'Background
DIM SHARED l% 'dateihandle fur logo
l% = _LOADIMAGE(netpath$ + "data\logo.jpg", 32)
DIM SHARED titel$ 'uberschrift fur alles
'copy/paste
DIM SHARED maxcliparray
DIM SHARED cliparray$(menulimit)
'tempsave
DIM SHARED maxtemparray(0 TO 1000)
DIM SHARED temparray$(1000, menulimit)
'status
statuslimit = 20
DIM SHARED sttext$(statuslimit)
DIM SHARED color$(statuslimit)
DIM SHARED st
DIM SHARED maxst
DIM SHARED shading
shading = 1
DIM SHARED UMround 'used for rounded shapes
UMround = 4
DIM SHARED UMglow 'used for glow distance
UMglow = 5
DIM SHARED UMopacity 'used for glow max opacity
UMopacity = 150
DIM SHARED detail
detail = 2
maximportv = 500
DIM SHARED titel$(maximportv)
DIM SHARED datum$(maximportv)
DIM SHARED zeit$(maximportv)
DIM SHARED ivtext$(maximportv)
DIM SHARED closebuttonlx
DIM SHARED closebuttonux
DIM SHARED minbuttonlx
DIM SHARED minbuttonux
DIM SHARED buttonsly
DIM SHARED buttonsuy
