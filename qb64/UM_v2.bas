'Universal Menu v2

DIM SHARED inputcount 'amount of input elements
DIM SHARED endparameter$ 'used to know which element caused the menu to end, use to redirect elsewhere or do something

DIM SHARED menulimit
menulimit = 50 'maximum amount of menu items
DIM SHARED interactable '=1 if the menu contains an interactable item, otherwise =0
DIM SHARED allsel(menulimit) 'if written text in input element is selected (ctrl + a)
DIM SHARED type$(menulimit) 'type of menu item (toggle, text, input, menitem, button, selector, date, time, slider)
DIM SHARED text$(menulimit) 'unchangeable text of menu item
DIM SHARED yoffset(menulimit) 'row to place menu item m on, pixel = yoffset(m) / fontheight
DIM SHARED xoffset(menulimit) 'column to place menu item m on, pixel = xoffset(m) / fontwidth
DIM SHARED basex(menulimit) 'left border of menu item
DIM SHARED basey(menulimit) 'upper border of menu item
DIM SHARED endx(menulimit) 'right border of menu item
DIM SHARED endy(menulimit) 'lower border of menu item
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
DIM SHARED maxtemparray(0 TO 12)
DIM SHARED temparray$(12, menulimit)
'status
'statuslimit = 20
'DIM SHARED sttext$(statuslimit)
'DIM SHARED color$(statuslimit)
'DIM SHARED st
DIM SHARED maxst
DIM SHARED shading
shading = 1
DIM SHARED UMround 'used for rounded shapes
UMround = 4
DIM SHARED detail
detail = 2
