$EXEICON:'B.ico'
defChar

OPTION BASE 1 'definiert unteres arrayelement von 1 anstatt 0 aus
_ACCEPTFILEDROP OFF 'erlaubt drag&drop funktion

DIM SHARED restart
GOTO restart 'ueberspringt den errorhandler zu beginn

errorhandler:
_AUTODISPLAY
_DEST 0
COLOR colour&("fg"), colour&("bg")
CLS
LOCATE 5, 5
logThis "[ Fehler" + STR$(ERR) + "in Zeile" + STR$(_ERRORLINE) + " ]"
IF ERR <> 5 AND ERR <> 6 AND ERR <> 7 AND ERR <> 9 AND ERR <> 14 AND ERR <> 17 AND ERR <> 19 AND ERR <> 51 AND ERR <> 70 AND ERR <> 71 AND ERR <> 72 AND ERR <> 75 THEN
    PRINT "[ Fehler"; ERR; "in Zeile"; _ERRORLINE; ", wird ignoriert ]"
    _DELAY 0.2
    SLEEP
    RESUME NEXT
ELSEIF ERR = 61 THEN
    PRINT "[ Fehler"; ERR; "in Zeile"; _ERRORLINE; ", schliesse Programm ]"
    PRINT "[ Die Festplatte ist voll. Bitte loesche Daten. ]"
    SLEEP
    RESUME NEXT
ELSE
    PRINT "[ Fehler"; ERR; "in Zeile"; _ERRORLINE; ", schliesse Programm ]"
    PRINT "[ Bitte melde diesen Fehler an die IT-Abteilung. ]"
    SLEEP
    SYSTEM
END IF

restart:
restart = 0
CLOSE
CLEAR

ON ERROR GOTO errorhandler
DIM SHARED netpath$
DIM SHARED settingspath$
netpath$ = "\\NASBREMER\Software\_BREMER_Programm\"
settingspath$ = _DIR$("local application data") + "\BREMER Events + KAZ\" 'um manuell zu finden: windows-taste druecken und "%appdata%" eingeben, einen ordner hochgehen und "local" oeffnen
CHDIR netpath$ 'verlegt aktuelle arbeitsumgebung des programms in den richtigen ordner, damit es von ueberall startbar ist

'erzeugt eine log-datei pro sitzung
DIM SHARED logfile$
logfile$ = netpath$ + "data\logs\" + DATE$ + "_1.log"
IF _FILEEXISTS(logfile$) THEN
    logcount = 1: DO: logcount = logcount + 1
        logfile$ = netpath$ + "data\logs\" + DATE$ + "_" + LST$(logcount) + ".log"
    LOOP UNTIL _FILEEXISTS(logfile$) = 0
END IF
OPEN logfile$ FOR OUTPUT AS #999: CLOSE #999

DIM SHARED swidth 'screenbreite / desktopbreite
DIM SHARED sheight 'screenhohe / desktophohe
swidth = _DESKTOPWIDTH
sheight = _DESKTOPHEIGHT

'einstellungs-variablen
DIM SHARED darkmode
DIM SHARED bigwindow
DIM SHARED rfontheight
DIM SHARED searchbottomfixed
DIM SHARED transparency

PRINT "Lade Einstellungen..."
IF _DIREXISTS(settingspath$) = 0 THEN MKDIR settingspath$
IF _FILEEXISTS(settingspath$ + "settings.bremer") THEN
    OPEN settingspath$ + "settings.bremer" FOR INPUT AS #1
    INPUT #1, darkmode$
    darkmode = VAL(_INFLATE$(darkmode$))
    INPUT #1, bigwindow$
    bigwindow = VAL(_INFLATE$(bigwindow$))
    INPUT #1, scale$
    rfontheight = VAL(_INFLATE$(scale$))
    INPUT #1, searchbottom$
    searchbottomfixed = VAL(_INFLATE$(searchbottom$))
    INPUT #1, transparency$
    transparency = VAL(_INFLATE$(transparency$))
    CLOSE #1
ELSE
    OPEN settingspath$ + "settings.bremer" FOR OUTPUT AS #1
    WRITE #1, _DEFLATE$("1"): WRITE #1, _DEFLATE$("0"): WRITE #1, _DEFLATE$("16"): WRITE #1, _DEFLATE$("1"): WRITE #1, _DEFLATE$("1")
    CLOSE #1
    darkmode = 1
    bigwindow = 0
    rfontheight = 16
    searchbottomfixed = 1
    transparency = 1
END IF

'Windows API calls from Wiki / Forum
CONST SWP_NOSIZE = &H0001 'ignores cx and cy size parameters
CONST SWP_NOMOVE = &H0002 'ignores x and y position parameters
CONST SWP_NOZORDER = &H0004 'keeps z order and ignores hWndInsertAfter parameter
CONST SWP_NOREDRAW = &H0008 'does not redraw window changes
CONST SWP_NOACTIVATE = &H0010 'does not activate window
CONST SWP_FRAMECHANGED = &H0020
CONST SWP_SHOWWINDOW = &H0040
CONST SWP_HIDEWINDOW = &H0080
CONST SWP_NOCOPYBITS = &H0100
CONST SWP_NOOWNERZORDER = &H0200
CONST SWP_NOSENDCHANGING = &H0400
CONST SWP_DRAWFRAME = SWP_FRAMECHANGED
CONST SWP_NOREPOSITION = SWP_NOOWNERZORDER
CONST SWP_DEFERERASE = &H2000
CONST SWP_ASYNCWINDOWPOS = &H4000
CONST HWND_TOP = 0 'window at top of z order no focus
CONST HWND_BOTTOM = 1 'window at bottom of z order no focus
CONST HWND_TOPMOST = -1 'window above all others no focus unless active
CONST HWND_NOTOPMOST = -2 'window below active no focus
DECLARE DYNAMIC LIBRARY "user32"
    'sets a created window region
    FUNCTION SetWindowRgn& (BYVAL hwnd&, BYVAL hrgn&, BYVAL bredraw%)
    FUNCTION SetLayeredWindowAttributes& (BYVAL hwnd AS LONG, BYVAL crKey AS LONG, BYVAL bAlpha AS _UNSIGNED _BYTE, BYVAL dwFlags AS LONG)
    FUNCTION GetWindowLong& ALIAS "GetWindowLongA" (BYVAL hwnd AS LONG, BYVAL nIndex AS LONG)
    FUNCTION SetWindowLong& ALIAS "SetWindowLongA" (BYVAL hwnd AS LONG, BYVAL nIndex AS LONG, BYVAL dwNewLong AS LONG)
    FUNCTION FindWindowA%& (BYVAL lpClassName%&, BYVAL lpWindowName%&)
    FUNCTION SetWindowPos& (BYVAL hWnd%&, BYVAL hWndInsertAfter%&, BYVAL X&, BYVAL Y&, BYVAL cx&, BYVAL cy&, BYVAL uFlags~&)
    FUNCTION GetForegroundWindow%&
END DECLARE
DECLARE DYNAMIC LIBRARY "gdi32"
    'creates a rectangular region
    FUNCTION CreateRectRgn& (BYVAL x1&, BYVAL y1&, BYVAL x2&, BYVAL y2&)
    'creates an elliptical region
    FUNCTION CreateEllipticRgn& (BYVAL x1&, BYVAL y1&, BYVAL x2&, BYVAL y2&)
    'creates a rectangular region with rounded corners
    FUNCTION CreateRoundRectRgn& (BYVAL x1&, BYVAL y1&, BYVAL x2&, BYVAL y2&, BYVAL x3&, BYVAL y3&)
END DECLARE
DECLARE DYNAMIC LIBRARY "kernel32"
    FUNCTION GetLastError~& ()
END DECLARE

hwnd& = _WINDOWHANDLE 'need the windows handle to play with it
IF transparency = 1 THEN
    SetWindowOpacity hwnd&, 245
ELSE
    SetWindowOpacity hwnd&, 255
END IF
IF 0 = SetWindowPos(hwnd&, HWND_TOPMOST, 200, 200, 0, 0, SWP_NOSIZE OR SWP_NOACTIVATE) THEN
    PRINT "SetWindowPos failed. 0x" + LCASE$(HEX$(GetLastError))
END IF
x%& = GetForegroundWindow%& 'find currently focused process handle
IF hwnd& <> x%& THEN _SCREENCLICK 240, 240 'add 40 to x and y to focus on positioned window
DIM SHARED rounding: rounding = 20
DIM SHARED topbarheight: topbarheight = 54 'same as logo height
DIM SHARED maximized

DIM SHARED canvas&
DIM SHARED maxx 'fensterbreite
DIM SHARED maxy 'fensterhohe
DIM SHARED currentposx
DIM SHARED currentposy
IF bigwindow = 0 THEN
    maxx = _DESKTOPWIDTH / 1.5
    maxy = _DESKTOPHEIGHT / 2.2
    rgn& = CreateRoundRectRgn(4, 30, maxx, maxy, rounding, rounding)
    try& = SetWindowRgn(hwnd&, rgn&, 0)
    'Returns zero if failed...
    IF try& = 0 THEN
        END
    END IF
    SCREEN _NEWIMAGE(maxx, maxy, 32)
    canvas& = _NEWIMAGE(maxx, maxy, 32)
    DO: LOOP UNTIL _SCREENEXISTS
    _SCREENMOVE (_DESKTOPWIDTH / 2) - (_DESKTOPWIDTH / 1.5 / 2), (_DESKTOPHEIGHT / 2) - (_DESKTOPHEIGHT / 2. / 2)
    currentposx = (_DESKTOPWIDTH / 2) - (_DESKTOPWIDTH / 1.5 / 2)
    currentposy = (_DESKTOPHEIGHT / 2) - (_DESKTOPHEIGHT / 2. / 2)
ELSE
    maxx = swidth
    maxy = sheight
    rgn& = CreateRoundRectRgn(7, 30, maxx, maxy, rounding, rounding)
    'Set the created region...
    try& = SetWindowRgn(hwnd&, rgn&, 0)
    'Returns zero if failed...
    IF try& = 0 THEN
        END
    END IF
    SCREEN _NEWIMAGE(maxx, maxy, 32)
    canvas& = _NEWIMAGE(maxx, maxy, 32)
    DO: LOOP UNTIL _SCREENEXISTS
    _SCREENMOVE 0, 0
    currentposx = 0
    currentposy = 0
END IF
DIM SHARED framerate
framerate = 60
_TITLE "BREMER Events & KAZ"

DIM SHARED node 'aktuelle node/instanz
DIM SHARED recLEN
DIM SHARED timerdifference: DIM SHARED starttime$

DIM SHARED username$ 'benutzername des angemeldeten users
DIM SHARED login

REM $INCLUDE:'code/UM.BI'
REM $INCLUDE:'code/TYPES.BI'
REM $INCLUDE:'code/SaveImage.BI'

'import from site
maxtags = 100000
DIM SHARED tag$(maxtags)
DIM SHARED content$(maxtags)
'ExportToQuark
DIM SHARED printedd(60)
DIM SHARED printed(maxrubrik)
DIM SHARED printedo(maxort)
'Print
maxprinters = 50
DIM SHARED default$(maxprinters)
DIM SHARED printer$(maxprinters)

FOR i = 1 TO _COMMANDCOUNT
    SELECT CASE LEFT$(COMMAND$(i), INSTR(COMMAND$(i), "="))
        CASE "username=": username$ = MID$(COMMAND$(i), INSTR(COMMAND$(i), "=") + 1)
        CASE "login=": login = VAL(MID$(COMMAND$(i), INSTR(COMMAND$(i), "=") + 1))
        CASE "darkmode=": darkmode = VAL(MID$(COMMAND$(i), INSTR(COMMAND$(i), "=") + 1))
        CASE "bigwindow=": bigwindow = VAL(MID$(COMMAND$(i), INSTR(COMMAND$(i), "=") + 1))
        CASE "fontheight=": rfontheight = VAL(MID$(COMMAND$(i), INSTR(COMMAND$(i), "=") + 1))
        CASE "searchbottomfixed=": searchbottomfixed = VAL(MID$(COMMAND$(i), INSTR(COMMAND$(i), "=") + 1))
        CASE "transparency=": transparency = VAL(MID$(COMMAND$(i), INSTR(COMMAND$(i), "=") + 1))
    END SELECT
NEXT

REM $INCLUDE:'code/FONTS.BI'

COLOR colour&("fg"), colour&("bg")
CLS
loadall

loginmenu:
listID = 0
IF login <> 1 THEN
    NewInput 1, 0, "Benutzername:   ", "", 0
    NewInput 2, 0, "Passwort:       ", "", 0
    NewMenItem 4, 0, "Anmelden", "login"
    NewMenItem 4, 12, "Programm beenden", "system"
    RunMenu 1, 0, "LOGIN"
END IF

IF endparameter$ <> "system" THEN
    u = 0
    IF max(1) > 0 THEN
        DO
            u = u + 1
            IF UserInput$(1) <> "" AND UserInput$(2) <> "" AND UserInput$(1) = MID$(User(u).Name, 1, LEN(UserInput$(1))) AND UserInput$(2) = MID$(User(u).Passwort, 1, LEN(UserInput$(2))) THEN
                login = 1
                username$ = UserInput$(1)
                User(u).LetzterLogin = DATE$ + " @ " + TIME$
            END IF
        LOOP UNTIL u = max(1)
    END IF

    IF login = 1 THEN
        writeBinary "usr"
        IF readBinary("usr") = 2 THEN login = 0
    END IF

    Background 0, "LOGIN", 1
    IF login = 0 THEN
        COLOR colour&("red"), colour&("transparent")
        LOCATE firstline + 1, firstchar: PRINT "[ Anmeldung fehlgeschlagen. ]"
        _DELAY 2
        GOTO loginmenu
    ELSEIF username$ = "Alex" OR username$ = "Lothar" OR username$ = "Richard" THEN
        admin = 1
    ELSE
        admin = 0
    END IF
    COLOR colour&("green"), colour&("transparent")
    LOCATE firstline + 1, firstchar: PRINT "[ Angemeldet. ]"
    _DELAY 1
ELSE
    SYSTEM
END IF

'alle menus sind hier zusammen, als overview
endparameter$ = "start"
startmenu:
_ACCEPTFILEDROP ON 'enables drag/drop functionality
DO
    endparameterbfbfbf$ = endparameterbfbf$
    endparameterbfbf$ = endparameterbf$
    endparameterbf$ = endparameter$
    suchbegriff$ = ""
    SELECT CASE endparameter$ 'alle endparameter mussten hier zu finden sein
        CASE "start"
            NewMenItem 1, 0, "PK", "pk"
            NewMenItem 2, 0, "KAZ", "ka"
            NewMenItem 3, 0, "Benutzer", "usr"
            NewMenItem 4, 0, "Adressen", "aov"
            IF admin = 1 THEN NewMenItem 5, 0, "Ausgaben", "asg"
            NewMenItem 5 + admin, 0, "Einstellungen", "settings"
            NewMenItem 6 + admin, 0, "Programm beenden", "system"
            NewMenItem 1, maxrows - 16, "Abmelden", "logout"
            RunMenu 1, 0, "START"
        CASE "pk"
            NewMenItem 1, 0, "Veranstaltungen", "ver"
            NewMenItem 2, 0, "Veranstalter", "vea"
            NewMenItem 3, 0, "Rubriken", "rbk"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "PK"
        CASE "ka"
            NewMenItem 1, 0, "Kleinanzeigen", "kaz"
            NewMenItem 2, 0, "Kategorien", "kat"
            NewMenItem 3, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "KAZ"
        CASE "ver"
            NewMenItem 1, 0, "Suche", "search"
            NewMenItem 2, 0, "Neu", "new"
            NewMenItem 3, 0, "Exportieren", "export"
            IF admin = 1 THEN NewMenItem 4, 0, "Importieren", "import"
            NewMenItem 4 + admin, 0, "<- Abbrechen", "start"
            newStatus "Anzahl: " + LST$(max(11)), "yellow"
            IF max(11) > 0 THEN newStatus "Neueste: " + RTRIM$(Veranstaltung(max(11)).Titel), "yellow"
            RunMenu 1, 0, "VERANSTALTUNGEN"
        CASE "asg"
            NewMenItem 1, 0, "Suche", "search"
            NewMenItem 2, 0, "Neu", "new"
            NewMenItem 3, 0, "<- Abbrechen", "start"
            newStatus "Anzahl: " + LST$(max(3)), "yellow"
            IF max(3) > 0 THEN newStatus "Neueste: " + LST$(Ausgabe(max(3)).Monat), "yellow"
            RunMenu 1, 0, "AUSGABEN"
        CASE "kaz"
            NewMenItem 1, 0, "Suche", "search"
            NewMenItem 2, 0, "Neu", "new"
            NewMenItem 3, 0, "Exportieren", "export"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            newStatus "Anzahl: " + LST$(max(10)), "yellow"
            IF max(10) > 0 THEN newStatus "Neueste: " + RTRIM$(Kleinanzeige(max(10)).Titel), "yellow"
            RunMenu 1, 0, "KLEINANZEIGEN"
        CASE "vea"
            NewMenItem 1, 0, "Suche", "search"
            NewMenItem 2, 0, "Neu", "new"
            NewMenItem 3, 0, "Drucken", "print"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            newStatus "Anzahl: " + LST$(max(9)), "yellow"
            IF max(9) > 0 THEN newStatus "Neuester: " + RTRIM$(Veranstalter(max(9)).Name), "yellow"
            RunMenu 1, 0, "VERANSTALTER"
        CASE "usr"
            IF admin = 1 THEN NewMenItem 1, 0, "Suche", "search"
            IF admin = 1 THEN NewMenItem 2, 0, "Neu", "new"
            NewMenItem 1 + (admin * 2), 0, "Bearbeiten", "edit"
            NewMenItem 2 + (admin * 2), 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "BENUTZER"
            u = 0: DO: u = u + 1
                IF RTRIM$(User(u).Name) = username$ THEN node = u
            LOOP UNTIL u >= maxu(1)
        CASE "rbk"
            NewMenItem 1, 0, "Suche", "search"
            NewMenItem 2, 0, "Neu", "new"
            NewMenItem 3, 0, "Drucken", "print"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "RUBRIKEN"
        CASE "aov"
            NewMenItem 1, 0, "Postleitzahlen", "plz"
            NewMenItem 2, 0, "Orte", "ort"
            NewMenItem 3, 0, "Adressen", "adr"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, ""
        CASE "ort"
            NewMenItem 1, 0, "Suche", "search"
            NewMenItem 2, 0, "Neu", "new"
            NewMenItem 3, 0, "Drucken", "print"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "ADRESSEN"
        CASE "plz"
            NewMenItem 1, 0, "Suche", "search"
            NewMenItem 2, 0, "Neu", "new"
            NewMenItem 3, 0, "Drucken", "print"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "POSTLEITZAHLEN"
        CASE "adr"
            NewMenItem 1, 0, "Suche", "search"
            NewMenItem 2, 0, "Neu", "new"
            NewMenItem 3, 0, "Drucken", "print"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "ADRESSEN"
        CASE "kat"
            NewMenItem 1, 0, "Suche", "search"
            NewMenItem 2, 0, "Neu", "new"
            NewMenItem 3, 0, "Drucken", "print"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "KATEGORIEN"
        CASE "settings"
            NewToggle 1, 0, "Dunkles Design", "darkmode"
            NewToggle 2, 0, "Grosses Fenster", "size"
            NewToggle 3, 0, "Untere Suchkante fixiert", "searchbottom"
            NewToggle 4, 0, "Transparenz", "transparency"
            NewSlider 5, 0, 0, 100, "Skalierung", "scale", ((fontheight - 16) / (20 - 16)) * 100
            IF admin = 1 AND timerdifference < 0.2 THEN
                NewText 6, 0, "Startzeit: " + starttime$ + " Sekunden. SUPERFAST!!!", colour&("fg"), "r"
            ELSEIF admin = 1 THEN
                NewText 6, 0, "Startzeit: " + starttime$ + " Sekunden.", colour&("fg"), "r"
            END IF
            NewMenItem 6 + admin, 0, "<- Abbrechen", "start"
            NewMenItem 6 + admin, 16, "Speichern", "save"
            RunMenu 1, 0, "EINSTELLUNGEN"
            IF endparameter$ = "save" THEN
                rfontheight = 16 + ((20 - 16) * (value(3) / 100))
                OPEN settingspath$ + "settings.bremer" FOR OUTPUT AS #1
                WRITE #1, _DEFLATE$(LST$(darkmode))
                WRITE #1, _DEFLATE$(LST$(bigwindow))
                WRITE #1, _DEFLATE$(LST$(rfontheight))
                WRITE #1, _DEFLATE$(LST$(searchbottomfixed))
                WRITE #1, _DEFLATE$(LST$(transparency))
                CLOSE #1
                restartparameter$ = " username=" + username$ + " login=1 "
                SHELL _DONTWAIT CHR$(34) + COMMAND$(0) + CHR$(34) + restartparameter$
                SYSTEM
            END IF
        CASE "print"
            OPEN "toprinter.txt" FOR OUTPUT AS #6
            PRINT #6, "Gedruckt von: " + username$ + " @ " + DATE$ + " um " + TIME$
            PRINT #6, ""
            SELECT CASE endparameterbfbf$
                CASE "vea"
                    PRINT #6, "VERANSTALTER"
                    PRINT #6, ""
                    PRINT #6, "Kuerzel" + SPC(3) + "Name" + SPC(31) + "Adresse"
                    PRINT #6, longchar$("-", 79)
                    IF max(9) > 0 THEN
                        va = 2: DO: va = va + 1
                            PRINT #6, RTRIM$(Veranstalter(va).Kuerzel) + SPC(10 - LEN(RTRIM$(Veranstalter(va).Kuerzel))) + RTRIM$(Veranstalter(va).Name) + SPC(35 - LEN(RTRIM$(Veranstalter(va).Name)));
                            'IF Veranstalter(va).Telefon <> 0 THEN
                            '    PRINT #6, LST$((Veranstalter(va).Telefon)) + SPC(16 - LEN(LST$((Veranstalter(va).Telefon))));
                            'ELSE
                            '    PRINT #6, SPC(16);
                            'END IF
                            IF max(6) > 0 THEN
                                d = 0: DO: d = d + 1
                                    IF Adresse(d).ID = Veranstalter(va).Adresse THEN
                                        PRINT #6, LST$(Adresse(d).PLZ) + " " + _TRIM$(Adresse(d).Ort)
                                        d = max(6)
                                    END IF
                                LOOP UNTIL d = max(6)
                            END IF
                        LOOP UNTIL va = max(9)
                        PRINT #6, longchar$("-", 79)
                        PRINT #6, "Anzahl Veranstalter: " + LST$(max(9))
                    END IF
                CASE "rbk"
                    PRINT #6, "RUBRIKEN"
                    PRINT #6, ""
                    PRINT #6, "Kuerzel" + SPC(4) + "Objekt" + SPC(4) + "Name"
                    IF max(8) > 0 THEN
                        r = 0: DO: r = r + 1
                            PRINT #6, RTRIM$(Rubrik(r).Kuerzel) + SPC(10 - LEN(RTRIM$(Rubrik(r).Kuerzel))) + LST$(Rubrik(r).Objekt) + SPC(10 - LEN(LST$(Rubrik(r).Objekt))) + (Rubrik(r).Name)
                        LOOP UNTIL r = max(8)
                        PRINT #6, longchar$("-", 79)
                        PRINT #6, "Anzahl Rubriken: " + LST$(max(8))
                    END IF
                CASE "ort"
                    PRINT #6, "ORTE"
                    PRINT #6, ""
                    PRINT #6, "Kuerzel" + SPC(10) + "Name"
                    IF max(4) > 0 THEN
                        ot = 0: DO: ot = ot + 1
                            PRINT #6, RTRIM$(Ort(ot).Kuerzel) + SPC(16 - LEN(RTRIM$(Ort(ot).Kuerzel))) + RTRIM$(Ort(ot).Name)
                        LOOP UNTIL ot = max(4)
                        PRINT #6, longchar$("-", 79)
                        PRINT #6, "Anzahl Orte: " + LST$(max(4))
                    END IF
                CASE "plz"
                    PRINT #6, "POSTLEITZAHLEN"
                    PRINT #6, ""
                    PRINT #6, "Land" + SPC(16) + "Ort" + SPC(27) + "PLZ"
                    IF max(5) > 0 THEN
                        p = 0: DO: p = p + 1
                            PRINT #6, RTRIM$(PLZ(p).Land) + SPC(20 - LEN(RTRIM$(PLZ(p).Land))) + RTRIM$(PLZ(p).Ort) + SPC(30 - LEN(RTRIM$(PLZ(p).Ort))) + LST$(PLZ(p).PLZ)
                        LOOP UNTIL p = max(5)
                        PRINT #6, longchar$("-", 79)
                        PRINT #6, "Anzahl Postleitzahlen: " + LST$(max(5))
                    END IF
            END SELECT
            CLOSE #6
            printviaprinter
            NewText 1, 0, "Drucken erfolgreich.", colour&("green"), "r"
            RunMenu 1, 0, "DRUCKEN"
            _DELAY 2
            endparameter$ = "start"
        CASE "export"
            NewSelector 1, 0, "Ziel:        ", 1, "", 1
            arraydata$(1, 1) = "Quark": arraydata$(1, 2) = ".csv - The Events Calendar (Wordpress)": maxad(1) = 2
            NewSelector 2, 0, "Ausgabe:     ", 2, "asg", 1
            a = 0: DO: a = a + 1: arraydata$(2, a) = LST$(Ausgabe(a).Monat): LOOP UNTIL a = max(3): maxad(2) = max(3)
            NewMenItem 3, 0, "Exportieren", "export"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "EXPORT"
            IF endparameter$ = "export" THEN
                SELECT CASE selected(1)
                    CASE 1
                        exportToQuark endparameterbfbf$, LST$(Ausgabe(selected(2)).Monat), Ausgabe(selected(2)).Anfang, Ausgabe(selected(2)).Ende
                    CASE 2
                        formatting$ = "eventscalendar"
                        exportToCsv endparameterbfbf$, LST$(Ausgabe(selected(2)).Monat), formatting$
                END SELECT
            END IF
            endparameter$ = "start"
        CASE "import"
            NewSelector 1, 0, "Quelle:      ", 1, "", 1
            arraydata$(1, 1) = ".csv - TEC Format": arraydata$(1, 2) = ".doc - Format in Guide": arraydata$(1, 3) = ".txt - Format in Guide": maxad(1) = 3
            NewMenItem 3, 0, "Importieren", "import"
            NewMenItem 4, 0, "<- Abbrechen", "start"
            RunMenu 1, 0, "IMPORT"
            IF endparameter$ = "import" THEN
                SELECT CASE selected(1)
                    CASE 1
                        importEvent "csv"
                    CASE 2
                        importEvent "doc"
                    CASE 3
                        importEvent "txt"
                END SELECT
            END IF
            endparameter$ = "start"
        CASE "logout"
            GOTO restart
        CASE "search"
            node = search(endparameterbfbf$, "")
            SELECT CASE node
                CASE 0 'exited
                    endparameter$ = "start"
                CASE IS > 0 'display
                    display endparameterbfbf$, node
                    endparameterbf$ = endparameterbfbf$
                CASE -1 'edit
                    edit endparameterbfbf$, node
                    endparameterbf$ = endparameterbfbf$
                CASE -2 'close program
                    endparameter$ = "system"
            END SELECT
        CASE "display"
            display endparameterbfbf$, node
            endparameterbf$ = endparameterbfbf$
        CASE "edit"
            edit endparameterbfbf$, node
            endparameterbf$ = endparameterbfbf$
            IF endparameter$ = "save" THEN dontadd = 1
        CASE "delete"
            delete endparameterbfbf$, node
            endparameterbf$ = endparameterbfbf$
        CASE "new"
            SELECT CASE endparameterbfbf$
                CASE "ver"
                    NewSelector 1, 0, "Ausgabe:        ", 1, "asg", max(3)
                    a = 0: DO: a = a + 1: arraydata$(1, a) = LST$(Ausgabe(a).Monat): LOOP UNTIL a = max(3): maxad(1) = max(3)
                    NewDate 2, 0, "Datum:          ", DATE$
                    NewSelector 3, 0, "Ort:            ", 2, "ort", 1
                    ot = 0: DO: ot = ot + 1: arraydata$(2, ot) = RTRIM$(Ort(ot).Name): LOOP UNTIL ot = max(4): maxad(2) = max(4)
                    rejected = 0
                    IF max(9) > 0 THEN
                        NewSelector 4, 0, "Veranstalter:   ", 3, "vea", 1
                        va = 0: DO: va = va + 1
                            IF Veranstalter(va).Kuerzel <> longchar$(MID$(Veranstalter(va).Kuerzel, 1, 1), LEN(Veranstalter(va).Kuerzel)) AND Veranstalter(va).Name <> longchar$(MID$(Veranstalter(va).Name, 1, 1), LEN(Veranstalter(va).Name)) THEN
                                arraydata$(3, va) = _TRIM$(Veranstalter(va).Kuerzel) + ", " + LTRIM$(RTRIM$(Veranstalter(va).Name))
                            ELSE
                                arraydata$(3, va) = ""
                            END IF
                        LOOP UNTIL va = max(9): maxad(3) = max(9) ' - rejected
                    ELSE
                        NewMenItem 4, 0, "Neuen Veranstalter erstellen", "new"
                    END IF
                    NewSelector 5, 0, "Rubrik:         ", 4, "rbk", 1
                    r = 0: DO: r = r + 1: arraydata$(4, r) = RTRIM$(Rubrik(r).Name): LOOP UNTIL r = max(8): maxad(4) = max(8)
                    NewTime 6, 0, "Zeit 1:         ", "15:00"
                    NewTime 7, 0, "Zeit 2:         ", "00:00"
                    NewTime 8, 0, "Zeit 3:         ", "00:00"
                    NewInput 9, 0, "Zeitcode:      ", "", 0
                    NewInput 10, 0, "Titel:         ", "", 0
                    NewInput 11, 0, "Text:          ", "", 0
                    NewInput 12, 0, "Langer Text:   ", "", 0
                    NewMenItem 14, 0, "Speichern", "save"
                    NewMenItem 14, 13, "<- Abbrechen", "back"
                    NewMenItem 14, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2) + 1, 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 11
                    RunMenu 1, 0, "NEUE VERANSTALTUNG"
                CASE "kaz"
                    NewSelector 1, 0, "Kategorie 1:    ", 1, "kat", 1
                    kt = 0: DO: kt = kt + 1: arraydata$(1, kt) = RTRIM$(Kategorie(kt).Name): LOOP UNTIL kt = max(12): maxad(1) = max(12)
                    NewSelector 2, 0, "Kategorie 2:    ", 2, "kat", 1
                    kt = 0: DO: kt = kt + 1: arraydata$(2, kt) = RTRIM$(Kategorie(kt).Name): LOOP UNTIL kt = max(12): maxad(2) = max(12)
                    NewSelector 3, 0, "Kategorie 3:    ", 3, "kat", 1
                    kt = 0: DO: kt = kt + 1: arraydata$(3, kt) = RTRIM$(Kategorie(kt).Name): LOOP UNTIL kt = max(12): maxad(3) = max(12)
                    NewInput 4, 0, "Name:         ", "", 0
                    NewInput 5, 0, "Titel:        ", "", 0
                    NewInput 6, 0, "Text:         ", "", 0
                    NewSelector 7, 0, "Objekt:         ", 4, "obj", 1
                    o = 0: DO: o = o + 1: arraydata$(4, o) = RTRIM$(Objekt(o).Name): LOOP UNTIL o = max(2): maxad(4) = max(2)
                    NewSelector 8, 0, "Ausgabe:        ", 5, "asg", max(3)
                    a = 0: DO: a = a + 1: arraydata$(5, a) = LST$(Ausgabe(a).Monat): LOOP UNTIL a = max(3): maxad(5) = max(3)
                    NewInput 9, 0, "Telefon:      ", "", 1
                    NewInput 10, 0, "Chiffre:      ", "", 0
                    NewInput 11, 0, "Notiz:        ", "", 0
                    NewMenItem 13, 0, "Speichern", "save"
                    NewMenItem 13, 13, "<- Abbrechen", "back"
                    NewMenItem 13, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 10
                    RunMenu 1, 0, "NEUE KLEINANZEIGE"
                CASE "vea"
                    NewInput 1, 0, "K" + CHR$(129) + "rzel:         ", "", 0
                    NewInput 2, 0, "Name:           ", "", 0
                    IF max(6) > 0 THEN
                        NewSelector 3, 0, "Adresse:        ", 1, "adr", 1
                        d = 0: DO: d = d + 1: arraydata$(1, d) = RTRIM$(Adresse(d).Ort) + ", " + RTRIM$(Adresse(d).Strasse): LOOP UNTIL d = max(6): maxad(1) = max(6)
                    ELSE
                        NewMenItem 3, 0, "Neue Adresse erstellen", "new"
                    END IF
                    NewInput 4, 0, "Telefon:        ", "", 1
                    NewInput 5, 0, "Telefax:        ", "", 1
                    NewInput 6, 0, "Anrede:         ", "", 0
                    NewInput 7, 0, "Notiz:          ", "", 0
                    NewMenItem 9, 0, "Speichern", "save"
                    NewMenItem 9, 13, "<- Abbrechen", "back"
                    NewMenItem 9, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 9
                    RunMenu 1, 0, "NEUER VERANSTALTER"
                    IF endparameter$ = "new" THEN endparameterbf$ = "adr"
                CASE "adr"
                    NewSelector 1, 0, "Postleitzahl:   ", 1, "plz", 1
                    p = 0: DO: p = p + 1: arraydata$(1, p) = LST$(PLZ(p).PLZ): LOOP UNTIL p = max(5): maxad(1) = max(5)
                    NewSelector 2, 0, "Ort:            ", 2, "ort", 1
                    ot = 0: DO: ot = ot + 1: arraydata$(2, ot) = RTRIM$(Ort(ot).Name): LOOP UNTIL ot = max(4): maxad(2) = ot
                    NewInput 3, 0, "Land:           ", "Deutschland", 0
                    NewInput 4, 0, "Strasse:        ", "", 0
                    NewMenItem 6, 0, "Speichern", "save"
                    NewMenItem 6, 13, "<- Abbrechen", "back"
                    NewMenItem 6, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 6
                    RunMenu 1, 0, "NEUE ADRESSE"
                CASE "rbk"
                    NewInput 1, 0, "K" + CHR$(129) + "rzel:         ", "", 0
                    NewSelector 2, 0, "Objekt:         ", 1, "obj", 1
                    o = 0: DO: o = o + 1: arraydata$(1, o) = RTRIM$(Objekt(o).Name): LOOP UNTIL o = max(2): maxad(2) = max(2)
                    NewInput 3, 0, "Name:           ", "", 0
                    NewMenItem 5, 0, "Speichern", "save"
                    NewMenItem 5, 13, "<- Abbrechen", "back"
                    NewMenItem 5, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 8
                    RunMenu 1, 0, "NEUE RUBRIK"
                CASE "usr"
                    NewInput 1, 0, "Name:           ", "", 0
                    NewInput 2, 0, "Passwort:       ", "", 0
                    NewSelector 3, 0, "Zugang:         ", 1, "", 1: SetArrayData 1, 1
                    NewSelector 4, 0, "Abteilung:      ", 2, "", 1: SetArrayData 2, 2
                    NewInput 5, 0, "Telefon:        ", "", 1
                    NewMenItem 7, 0, "Speichern", "save"
                    NewMenItem 7, 13, "<- Abbrechen", "back"
                    NewMenItem 7, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 1
                    RunMenu 1, 0, "NEUER BENUTZER"
                CASE "ort"
                    NewInput 1, 0, "K" + CHR$(129) + "rzel:         ", "", 0
                    NewInput 2, 0, "Name:           ", "", 0
                    NewMenItem 4, 0, "Speichern", "save"
                    NewMenItem 4, 13, "<- Abbrechen", "back"
                    NewMenItem 4, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 4
                    RunMenu 1, 0, "NEUER ORT"
                CASE "plz"
                    NewInput 1, 0, "Postleitzahl:   ", "", 1
                    NewSelector 2, 0, "Ort:            ", 2, "ort", 1
                    ot = 0: DO: ot = ot + 1: arraydata$(2, ot) = RTRIM$(Ort(ot).Name): LOOP UNTIL ot = max(4): maxad(2) = max(4)
                    NewInput 3, 0, "Land:           ", "Deutschland", 0
                    NewMenItem 5, 0, "Speichern", "save"
                    NewMenItem 5, 13, "<- Abbrechen", "back"
                    NewMenItem 5, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 5
                    RunMenu 1, 0, "NEUE POSTLEITZAHL"
                CASE "asg"
                    NewSelector 1, 0, "Objekt:         ", 1, "obj", 1
                    o = 0: DO: o = o + 1: arraydata$(1, o) = RTRIM$(Objekt(o).Name): LOOP UNTIL o = max(2): maxad(2) = max(2)
                    NewInput 2, 0, "Monat:          ", MID$(DATE$, 7, 4) + MID$(DATE$, 1, 2), 1
                    NewDate 3, 0, "Anfangsdatum:   ", DATE$
                    NewDate 4, 0, "Enddatum:       ", DATE$
                    NewMenItem 6, 0, "Speichern", "save"
                    NewMenItem 6, 13, "<- Abbrechen", "back"
                    NewMenItem 6, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 3
                    RunMenu 1, 0, "NEUE AUSGABE"
                CASE "kat"
                    NewInput 1, 0, "K" + CHR$(129) + "rzel:         ", "", 0
                    NewSelector 2, 0, "Objekt:         ", 1, "obj", 1
                    o = 0: DO: o = o + 1: arraydata$(1, o) = RTRIM$(Objekt(o).Name): LOOP UNTIL o = max(2): maxad(2) = max(2)
                    NewInput 3, 0, "Name:           ", "", 0
                    NewMenItem 5, 0, "Speichern", "save"
                    NewMenItem 5, 13, "<- Abbrechen", "back"
                    NewMenItem 5, 29, "Einf" + CHR$(129) + "gen", "paste"
                    NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
                    listID = 12
                    RunMenu 1, 0, "NEUE KATEGORIE"
            END SELECT
        CASE "save"
            SELECT CASE endparameterbfbfbf$
                CASE "ver"
                    IF dontadd = 0 THEN
                        IF max(11) > 0 THEN
                            v = 0: DO: v = v + 1
                                IF Veranstaltung(v).Ausgabe = VAL(UserInput$(1)) AND Veranstaltung(v).Datum = UserInput$(2) AND Veranstaltung(v).Zeit1 = UserInput$(7) AND Veranstaltung(v).Zeit2 = UserInput$(8) AND Veranstaltung(v).Titel = UserInput$(10) THEN
                                    NewText 1, 0, "Diese Veranstaltung existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparamterbf$ = "ver"
                                END IF
                            LOOP UNTIL v = max(11)
                        END IF
                    END IF
                    IF endparameter$ = "save" THEN
                        IF dontadd = 0 THEN
                            max(11) = max(11) + 1
                            node = max(11)
                            IF node > 1 THEN
                                Veranstaltung(node).ID = Veranstaltung(node - 1).ID + 1
                            ELSE
                                Veranstaltung(node).ID = 1
                            END IF
                        END IF
                        Veranstaltung(node).Ausgabe = VAL(UserInput$(1))
                        Veranstaltung(node).Datum = UserInput$(2)
                        Veranstaltung(node).Ort = UserInput$(3)
                        Veranstaltung(node).Veranstalter = UserInput$(4)
                        Veranstaltung(node).Rubrik = UserInput$(5)
                        Veranstaltung(node).Zeit1 = UserInput$(6)
                        Veranstaltung(node).Zeit2 = UserInput$(7)
                        Veranstaltung(node).Zeit3 = UserInput$(8)
                        Veranstaltung(node).Zeitcode = UserInput$(9)
                        Veranstaltung(node).Titel = UserInput$(10)
                        Veranstaltung(node).Text = UserInput$(11)
                        Veranstaltung(node).TextLang = UserInput$(12)
                        writeBinary "ver"
                        IF readBinary("ver") = 1 THEN
                            NewText 1, 0, "Veranstaltung " + RTRIM$(Veranstaltung(node).Titel) + " erfolgreich gespeichert.", colour&("green"), "r"
                            RunMenu 1, 0, "SPEICHERN"
                        END IF
                        _DELAY 0.2: endparameter$ = "new": endparameterbf$ = "ver"
                    END IF
                CASE "kaz"
                    IF dontadd = 0 THEN
                        max(10) = max(10) + 1
                        node = max(10)
                        IF node > 1 THEN
                            Kleinanzeige(node).ID = Kleinanzeige(node - 1).ID + 1
                        ELSE
                            Kleinanzeige(node).ID = 1
                        END IF
                    END IF
                    Kleinanzeige(node).Kategorie1 = UserInput$(1)
                    Kleinanzeige(node).Kategorie2 = UserInput$(2)
                    Kleinanzeige(node).Kategorie3 = UserInput$(3)
                    Kleinanzeige(node).Name = UserInput$(4)
                    Kleinanzeige(node).Titel = UserInput$(5)
                    Kleinanzeige(node).Text = UserInput$(6)
                    Kleinanzeige(node).Objekt = selected(7)
                    Kleinanzeige(node).Ausgabe = Ausgabe(selected(8)).Monat
                    Kleinanzeige(node).Telefon = UserInput$(9)
                    Kleinanzeige(node).Chiffre = UserInput$(10)
                    Kleinanzeige(node).Notiz = UserInput$(11)
                    writeBinary "kaz"
                    IF readBinary("kaz") = 1 THEN
                        NewText 1, 0, "Kleinanzeige" + STR$(Kleinanzeige(node).ID) + " erfolgreich gespeichert.", colour&("green"), "r"
                        RunMenu 1, 0, "SPEICHERN"
                    END IF
                    _DELAY 1: endparameter$ = "new"
                    endparameterbf$ = "kaz"
                CASE "vea"
                    IF dontadd = 0 THEN
                        va = 0: DO: va = va + 1
                            IF Veranstalter(va).Kuerzel = UserInput$(1) THEN
                                NewText 1, 0, "Dieses K" + CHR$(129) + "rzel existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "vea"
                            END IF
                            IF Veranstalter(va).Name = UserInput$(2) THEN
                                NewText 1, 0, "Dieser Veranstalter existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "vea"
                            END IF
                        LOOP UNTIL va = max(9)
                    END IF
                    IF endparameter$ = "save" THEN
                        IF dontadd = 0 THEN
                            max(9) = max(9) + 1
                            node = max(9)
                        END IF
                        Veranstalter(node).Kuerzel = UserInput$(1)
                        Veranstalter(node).Name = UserInput$(2)
                        d = 0: DO: d = d + 1
                            IF RTRIM$(Adresse(d).Ort) + ", " + RTRIM$(Adresse(d).Strasse) = UserInput$(3) THEN
                                Veranstalter(node).Adresse = Adresse(d).ID
                                d = max(6)
                            END IF
                        LOOP UNTIL d = max(6)
                        Veranstalter(node).Telefon = VAL(UserInput$(4))
                        Veranstalter(node).Telefax = VAL(UserInput$(5))
                        Veranstalter(node).Anrede = UserInput$(6)
                        Veranstalter(node).Notiz = UserInput$(7)
                        writeBinary "vea"
                        IF readBinary("vea") = 1 THEN
                            NewText 1, 0, "Veranstalter " + RTRIM$(Veranstalter(node).Name) + " erfolgreich gespeichert.", colour&("green"), "r"
                            RunMenu 1, 0, "SPEICHERN"
                        END IF
                        _DELAY 1: endparameter$ = "new"
                        endparameterbf$ = "vea"
                    END IF
                CASE "adr"
                    IF dontadd = 0 THEN
                        d = 0: DO: d = d + 1
                            IF LST$(Adresse(d).PLZ) = UserInput$(1) AND RTRIM$(Adresse(d).Ort) = UserInput$(2) AND RTRIM$(Adresse(d).Land) = UserInput$(3) AND RTRIM$(Adresse(d).Strasse) = UserInput$(4) THEN
                                NewText 1, 0, "Diese Adresse existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "adr"
                            END IF
                        LOOP UNTIL d = max(6)
                    END IF
                    IF endparameter$ = "save" THEN
                        IF dontadd = 0 THEN
                            max(6) = max(6) + 1
                            node = max(6)
                            Adresse(node).ID = Adresse(node - 1).ID + 1
                        END IF
                        Adresse(node).PLZ = VAL(UserInput$(1))
                        Adresse(node).Ort = UserInput$(2)
                        Adresse(node).Land = UserInput$(3)
                        Adresse(node).Strasse = UserInput$(4)
                        writeBinary "adr"
                        IF readBinary("adr") = 1 THEN
                            NewText 1, 0, "Adresse " + LST$(Adresse(node).ID) + " erfolgreich gespeichert.", colour&("green"), "r"
                            RunMenu 1, 0, "SPEICHERN"
                        END IF
                    END IF
                    _DELAY 1: endparameter$ = "new"
                    endparameterbf$ = "adr"
                CASE "rbk"
                    IF dontadd = 0 THEN
                        r = 0: DO: r = r + 1
                            IF Rubrik(r).Kuerzel = UserInput$(1) THEN
                                NewText 1, 0, "Dieses K" + CHR$(129) + "rzel existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "rbk"
                            END IF
                            IF Rubrik(r).Name = UserInput$(1) THEN
                                NewText 1, 0, "Diese Rubrik existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "rbk"
                            END IF
                        LOOP UNTIL r = max(8)
                    END IF
                    IF endparameter$ = "save" THEN
                        IF dontadd = 0 THEN
                            max(8) = max(8) + 1
                            node = max(8)
                        END IF
                        Rubrik(node).Kuerzel = UserInput$(1)
                        Rubrik(node).Objekt = selected(2)
                        Rubrik(node).Name = UserInput$(3)
                        writeBinary "rbk"
                        IF readBinary("rbk") = 1 THEN
                            NewText 1, 0, "Rubrik " + RTRIM$(Rubrik(node).Name) + " erfolgreich gespeichert.", colour&("green"), "r"
                            RunMenu 1, 0, "SPEICHERN"
                        END IF
                        _DELAY 1: endparameter$ = "new"
                        endparameterbf$ = "rbk"
                    END IF
                CASE "usr"
                    IF dontadd = 0 THEN
                        u = 0: DO: u = u + 1
                            IF User(u).Name = UserInput$(1) THEN
                                NewText 1, 0, "Benutzername existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN"
                                _DELAY 2: endparameter$ = "start"
                            END IF
                        LOOP UNTIL u = max(1)
                    END IF
                    IF UserInput$(2) = "" THEN NewText 1, 0, "Passwort darf nicht leer sein.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "start"
                    IF endparameter$ = "save" THEN
                        IF dontadd = 0 THEN
                            max(1) = max(1) + 1
                            node = max(1)
                            pfp& = Robohash(User(node).Name)
                            Result = SaveImage(netpath$ + "data\pfp\" + User(node).Name + ".png", pfp&, 0, 0, _WIDTH(pfp&), _HEIGHT(pfp&))
                            IF Result = 1 THEN 'file already found on drive
                                KILL exportimage2$ 'delete the old file
                                Result = SaveImage(exportimage2$, 0, 0, 0, _WIDTH, _HEIGHT) 'save the new one again
                            END IF
                            _FREEIMAGE pfp&
                        END IF
                        User(node).Name = UserInput$(1)
                        IF dontadd = 0 THEN User(node).Profilbild = User(node).Name + ".png"
                        User(node).Passwort = UserInput$(2)
                        User(node).Zugang = selected(3)
                        User(node).Abteilung = UserInput$(4)
                        User(node).Telefon = VAL(UserInput$(5))
                        User(node).LetzterLogin = DATE$ + "@" + TIME$
                        writeBinary "usr"
                        IF readBinary("usr") = 1 THEN
                            NewText 1, 0, "Benutzer " + RTRIM$(User(node).Name) + " erfolgreich gespeichert.", colour&("green"), "r"
                            RunMenu 1, 0, "SPEICHERN"
                        END IF
                        _DELAY 2: endparameter$ = "start"
                        endparameterbf$ = "usr"
                    END IF
                CASE "ort"
                    IF dontadd = 0 THEN
                        ot = 0: DO: ot = ot + 1
                            IF Ort(ot).Name = UserInput$(2) THEN
                                NewText 1, 0, "Dieser Ort existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "ort"
                            END IF
                        LOOP UNTIL ot = max(4)
                    END IF
                    IF endparameter$ = "save" THEN
                        IF dontadd = 0 THEN
                            max(4) = max(4) + 1
                            node = max(4)
                        END IF
                        Ort(node).Kuerzel = UserInput$(1)
                        Ort(node).Name = UserInput$(2)
                        writeBinary "ort"
                        IF readBinary("ort") = 1 THEN
                            NewText 1, 0, "Ort " + RTRIM$(Ort(node).Name) + " erfolgreich gespeichert.", colour&("green"), "r"
                            RunMenu 1, 0, "SPEICHERN"
                        END IF
                        _DELAY 2: endparameter$ = "start"
                        endparameterbf$ = "ort"
                    END IF
                CASE "plz"
                    IF dontadd = 0 THEN
                        p = 0: DO: p = p + 1
                            IF PLZ(max(5)).PLZ = VAL(UserInput$(1)) THEN
                                NewText 1, 0, "Diese Postleitzahl existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "plz"
                            END IF
                        LOOP UNTIL p = max(5)
                    END IF
                    IF endparameter$ = "save" THEN
                        IF dontadd = 0 THEN
                            max(5) = max(5) + 1
                            node = max(5)
                        END IF
                        PLZ(node).PLZ = VAL(UserInput$(1))
                        PLZ(node).Ort = UserInput$(2)
                        PLZ(node).Land = UserInput$(3)
                        writeBinary "plz"
                        IF readBinary("plz") = 1 THEN
                            NewText 1, 0, "Postleitzahl " + LST$(PLZ(node).PLZ) + " erfolgreich gespeichert.", colour&("green"), "r"
                            RunMenu 1, 0, "SPEICHERN"
                        END IF
                        _DELAY 2: endparameter$ = "start"
                        endparameterbf$ = "plz"
                    END IF
                CASE "asg"
                    IF dontadd = 0 THEN
                        IF max(3) > 0 THEN
                            a = 0: DO: a = a + 1
                                IF Ausgabe(a).Monat = VAL(UserInput$(2)) THEN
                                    NewText 1, 0, "Dieser Monat existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "asg"
                                END IF
                            LOOP UNTIL a = max(3)
                        END IF
                    END IF
                    IF endparameter$ = "save" THEN
                        IF dontadd = 0 THEN
                            max(3) = max(3) + 1
                            node = max(3)
                            IF node > 0 THEN
                                Ausgabe(node).ID = Ausgabe(node - 1).ID + 1
                            ELSE
                                Ausgabe(node).ID = 1
                            END IF
                        END IF
                        Ausgabe(node).Objekt = selected(1)
                        Ausgabe(node).Monat = VAL(UserInput$(2))
                        Ausgabe(node).Anfang = UserInput$(3)
                        Ausgabe(node).Ende = UserInput$(4)
                        writeBinary "asg"
                        IF readBinary("asg") = 1 THEN
                            NewText 1, 0, "Ausgabe " + LST$(Ausgabe(node).Monat) + " erfolgreich gespeichert.", colour&("green"), "r"
                            RunMenu 1, 0, "SPEICHERN"
                        END IF
                        _DELAY 2: endparameter$ = "start"
                        endparameterbf$ = "asg"
                    END IF
                CASE "kat"
                    IF dontadd = 0 THEN
                        kt = 0: DO: kt = kt + 1
                            IF Kategorie(kt).Kuerzel = UserInput$(1) THEN
                                NewText 1, 0, "Dieses K" + CHR$(129) + "rzel existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "kat"
                            END IF
                            IF Kategorie(kt).Name = UserInput$(1) THEN
                                NewText 1, 0, "Diese Kategorie existiert bereits.", colour&("red"), "r": RunMenu 1, 0, "SPEICHERN": _DELAY 2: endparameter$ = "new": endparameterbf$ = "kat"
                            END IF
                        LOOP UNTIL kt = max(12)
                    END IF
                    IF endparameter$ = "save" THEN
                        IF dontadd = 0 THEN
                            max(12) = max(12) + 1
                            node = max(12)
                        END IF
                        Kategorie(node).Kuerzel = UserInput$(1)
                        Kategorie(node).Objekt = selected(2)
                        Kategorie(node).Name = UserInput$(3)
                        writeBinary "kat"
                        IF readBinary("kat") = 1 THEN
                            NewText 1, 0, "Kategorie " + RTRIM$(Kategorie(node).Name) + " erfolgreich gespeichert.", colour&("green"), "r"
                            RunMenu 1, 0, "SPEICHERN"
                        END IF
                        _DELAY 1: endparameter$ = "new"
                        endparameterbf$ = "kat"
                    END IF
            END SELECT
            dontadd = 0
        CASE "back"
            endparameter$ = endparameterbfbfbf$
        CASE "up"
            endparameter$ = "start"
        CASE "restart"
            restartparameter$ = " username=" + username$ + " login=1 "
            OPEN _STARTDIR$ + "\temp.temp" FOR OUTPUT AS #200
            PRINT #200, restartparameter$
            CLOSE #200
            GOTO restart
    END SELECT
LOOP UNTIL endparameter$ = "system"
SYSTEM

SUB EmptyMenu
    maxm = 0: m = 0: interactable = 0
END SUB

SUB display (listID$, node)
    SELECT CASE listID$
        CASE "ver"
            NewText 1, 0, "Ausgabe:      " + LST$(Veranstaltung(node).Ausgabe), colour&("fg"), "r"
            NewText 2, 0, "Datum:        " + RTRIM$(Veranstaltung(node).Datum), colour&("fg"), "r"
            NewText 3, 0, "Ort:          " + RTRIM$(Veranstaltung(node).Ort), colour&("fg"), "r"
            NewText 4, 0, "Veranstalter: " + RTRIM$(Veranstaltung(node).Veranstalter), colour&("fg"), "r"
            NewText 5, 0, "Rubriken:     " + RTRIM$(Veranstaltung(node).Rubrik), colour&("fg"), "r"
            IF RTRIM$(Veranstaltung(node).Zeitcode) = "" THEN
                NewText 6, 0, "Zeit:         " + RTRIM$(Veranstaltung(node).Zeit1), colour&("fg"), "r"
            ELSEIF RTRIM$(Veranstaltung(node).Zeit3) = "00:00" THEN
                NewText 6, 0, "Zeit:         " + RTRIM$(Veranstaltung(node).Zeit1) + RTRIM$(Veranstaltung(node).Zeitcode) + RTRIM$(Veranstaltung(node).Zeit2), colour&("fg"), "r"
            ELSE
                NewText 6, 0, "Zeit:         " + RTRIM$(Veranstaltung(node).Zeit1) + RTRIM$(Veranstaltung(node).Zeitcode) + RTRIM$(Veranstaltung(node).Zeit2) + RTRIM$(Veranstaltung(node).Zeitcode) + RTRIM$(Veranstaltung(node).Zeit3), colour&("fg"), "r"
            END IF
            NewText 7, 0, "Zeitcode:     " + RTRIM$(Veranstaltung(node).Zeitcode), colour&("fg"), "r"
            NewText 8, 0, "Titel:        " + RTRIM$(Veranstaltung(node).Titel), colour&("fg"), "r"
            NewText 9, 0, "Text:         " + RTRIM$(Veranstaltung(node).Text), colour&("fg"), "r"
            NewText 10, 0, "Langer Text: " + RTRIM$(Veranstaltung(node).TextLang), colour&("fg"), "r"
            NewMenItem 12, 0, "<- Abbrechen", "back"
            NewMenItem 12, 16, "Bearbeiten", "edit"
            NewMenItem 12, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 12, 41, "Kopieren", "copy"
            RunMenu 1, 0, "VERANSTALTUNG"
        CASE "kaz"
            NewText 1, 0, "Kategorien:   " + RTRIM$(Kleinanzeige(node).Kategorie1) + ", " + RTRIM$(Kleinanzeige(node).Kategorie2) + ", " + RTRIM$(Kleinanzeige(node).Kategorie3), colour&("fg"), "r"
            NewText 2, 0, "Name:         " + RTRIM$(Kleinanzeige(node).Name), colour&("fg"), "r"
            NewText 3, 0, "Titel:        " + RTRIM$(Kleinanzeige(node).Titel), colour&("fg"), "r"
            NewText 4, 0, "Text:         " + RTRIM$(Kleinanzeige(node).Text), colour&("fg"), "r"
            o = 0: DO: o = o + 1
                IF Objekt(o).ID = Kleinanzeige(node).Objekt THEN
                    NewText 5, 0, "Objekt:       " + RTRIM$(Objekt(o).Name), colour&("fg"), "r"
                END IF
            LOOP UNTIL o = max(2)
            NewText 6, 0, "Ausgabe:      " + LST$(Kleinanzeige(node).Ausgabe), colour&("fg"), "r"
            NewText 7, 0, "Telefon:      " + RTRIM$(Kleinanzeige(node).Telefon), colour&("fg"), "r"
            NewText 8, 0, "Chiffre:      " + RTRIM$(Kleinanzeige(node).Chiffre), colour&("fg"), "r"
            NewText 9, 0, "Notiz:        " + RTRIM$(Kleinanzeige(node).Notiz), colour&("fg"), "r"
            NewMenItem 11, 0, "<- Abbrechen", "back"
            NewMenItem 11, 16, "Bearbeiten", "edit"
            NewMenItem 11, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 11, 41, "Kopieren", "copy"
            RunMenu 1, 0, "KLEINANZEIGE"
        CASE "vea"
            NewText 1, 0, "K" + CHR$(129) + "rzel:       " + RTRIM$(Veranstalter(node).Kuerzel), colour&("fg"), "r"
            NewText 2, 0, "Name:         " + RTRIM$(Veranstalter(node).Name), colour&("fg"), "r"
            d = 0: DO: d = d + 1
                IF Veranstalter(node).Adresse = Adresse(d).ID THEN
                    NewText 3, 0, "Adresse:      " + RTRIM$(Adresse(d).Ort) + ", " + RTRIM$(Adresse(d).Strasse), colour&("fg"), "r"
                END IF
            LOOP UNTIL d = max(6)
            NewText 4, 0, "Telefon:      " + LST$(Veranstalter(node).Telefon), colour&("fg"), "r"
            NewText 5, 0, "Telefax:      " + LST$(Veranstalter(node).Telefax), colour&("fg"), "r"
            NewText 6, 0, "Anrede:       " + RTRIM$(Veranstalter(node).Anrede), colour&("fg"), "r"
            NewText 7, 0, "Notiz:        " + RTRIM$(Veranstalter(node).Notiz), colour&("fg"), "r"
            NewMenItem 9, 0, "<- Abbrechen", "back"
            NewMenItem 9, 16, "Bearbeiten", "edit"
            NewMenItem 9, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 9, 41, "Kopieren", "copy"
            RunMenu 1, 0, "VERANSTALTER"
        CASE "adr"
            NewText 1, 0, "ID:           " + LST$(Adresse(node).ID), colour&("fg"), "r"
            NewText 2, 0, "Postleitzahl: " + LST$(Adresse(node).PLZ), colour&("fg"), "r"
            NewText 3, 0, "Ort:          " + RTRIM$(Adresse(node).Ort), colour&("fg"), "r"
            NewText 4, 0, "Land:         " + RTRIM$(Adresse(node).Land), colour&("fg"), "r"
            NewText 5, 0, "Strasse:      " + RTRIM$(Adresse(node).Strasse), colour&("fg"), "r"
            NewMenItem 7, 0, "<- Abbrechen", "back"
            NewMenItem 7, 16, "Bearbeiten", "edit"
            NewMenItem 7, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 7, 41, "Kopieren", "copy"
            RunMenu 1, 0, "ADRESSE"
        CASE "rbk"
            NewText 1, 0, "K" + CHR$(129) + "rzel:       " + RTRIM$(Rubrik(node).Kuerzel), colour&("fg"), "r"
            NewText 2, 0, "Objekt:       " + RTRIM$(Objekt(Rubrik(node).Objekt).Name), colour&("fg"), "r"
            NewText 3, 0, "Name:         " + RTRIM$(Rubrik(node).Name), colour&("fg"), "r"
            NewMenItem 5, 0, "<- Abbrechen", "back"
            NewMenItem 5, 16, "Bearbeiten", "edit"
            NewMenItem 5, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 5, 41, "Kopieren", "copy"
            RunMenu 1, 0, "RUBRIK"
        CASE "usr"
            NewText 1, 0, "Benutzername:  " + RTRIM$(User(node).Name), colour&("fg"), "r"
            IF LEN(RTRIM$(User(node).Passwort)) > 0 THEN stars$ = longchar$("*", LEN(RTRIM$(User(node).Passwort)))
            NewText 2, 0, "Passwort:      " + stars$, colour&("fg"), "r"
            SetArrayData 30, 1
            NewText 3, 0, "Zugang:        " + arraydata$(User(node).Zugang, 1), colour&("fg"), "r"
            NewText 4, 0, "Abteilung:     " + RTRIM$(User(node).Abteilung), colour&("fg"), "r"
            NewText 5, 0, "Telefon:       " + STR$(User(node).Telefon), colour&("fg"), "r"
            NewText 6, 0, "Letzter Login: " + RTRIM$(User(node).LetzterLogin), colour&("fg"), "r"
            NewMenItem 8, 0, "<- Abbrechen", "back"
            NewMenItem 8, 16, "Bearbeiten", "edit"
            NewMenItem 8, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 8, 41, "Kopieren", "copy"
            RunMenu 1, 0, "BENUTZER"
        CASE "ort"
            NewText 1, 0, "K" + CHR$(129) + "rzel:       " + RTRIM$(Ort(node).Kuerzel), colour&("fg"), "r"
            NewText 2, 0, "Name:         " + RTRIM$(Ort(node).Name), colour&("fg"), "r"
            NewMenItem 4, 0, "<- Abbrechen", "back"
            NewMenItem 4, 16, "Bearbeiten", "edit"
            NewMenItem 4, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 4, 41, "Kopieren", "copy"
            RunMenu 1, 0, "ORT"
        CASE "plz"
            NewText 1, 0, "Postleitzahl: " + LST$(PLZ(node).PLZ), colour&("fg"), "r"
            NewText 2, 0, "Ort:          " + RTRIM$(PLZ(node).Ort), colour&("fg"), "r"
            NewText 3, 0, "Land:         " + RTRIM$(PLZ(node).Land), colour&("fg"), "r"
            NewMenItem 5, 0, "<- Abbrechen", "back"
            NewMenItem 5, 16, "Bearbeiten", "edit"
            NewMenItem 5, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 5, 41, "Kopieren", "copy"
            RunMenu 1, 0, "ORT"
        CASE "adr"
            NewText 1, 0, "ID:          " + STR$(Adresse(node).ID), colour&("fg"), "r"
            NewText 2, 0, "Postleitzahl:" + STR$(Adresse(node).PLZ), colour&("fg"), "r"
            NewText 3, 0, "Ort:         " + RTRIM$(Adresse(node).Ort), colour&("fg"), "r"
            NewText 4, 0, "Land:        " + RTRIM$(Adresse(node).Land), colour&("fg"), "r"
            NewText 5, 0, "Strasse:     " + RTRIM$(Adresse(node).Strasse), colour&("fg"), "r"
            NewMenItem 7, 0, "<- Abbrechen", "back"
            NewMenItem 7, 16, "Bearbeiten", "edit"
            NewMenItem 7, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 7, 41, "Kopieren", "copy"
            RunMenu 1, 0, "ADRESSE"
        CASE "plz"
            NewText 1, 0, "Postleitzahl:" + STR$(PLZ(node).PLZ), colour&("fg"), "r"
            NewText 2, 0, "Land:         " + RTRIM$(PLZ(node).Land), colour&("fg"), "r"
            NewText 3, 0, "Ort:          " + RTRIM$(PLZ(node).Ort), colour&("fg"), "r"
            NewMenItem 5, 0, "<- Abbrechen", "back"
            NewMenItem 5, 16, "Bearbeiten", "edit"
            NewMenItem 5, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 5, 41, "Kopieren", "copy"
            RunMenu 1, 0, "POSTLEITZAHL"
        CASE "asg"
            NewText 1, 0, "ID:          " + STR$(Ausgabe(node).ID), colour&("fg"), "r"
            NewText 2, 0, "Objekt:       " + RTRIM$(Objekt(Ausgabe(node).Objekt).Name), colour&("fg"), "r"
            NewText 3, 0, "Monat:       " + STR$(Ausgabe(node).Monat), colour&("fg"), "r"
            NewText 4, 0, "Anfangsdatum: " + Ausgabe(node).Anfang, colour&("fg"), "r"
            NewText 5, 0, "Enddatum:     " + Ausgabe(node).Ende, colour&("fg"), "r"
            NewMenItem 7, 0, "<- Abbrechen", "back"
            NewMenItem 7, 16, "Bearbeiten", "edit"
            NewMenItem 7, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 7, 41, "Kopieren", "copy"
            RunMenu 1, 0, "AUSGABE"
        CASE "kat"
            NewText 1, 0, "K" + CHR$(129) + "rzel:       " + RTRIM$(Kategorie(node).Kuerzel), colour&("fg"), "r"
            NewText 2, 0, "Objekt:       " + RTRIM$(Objekt(Kategorie(node).Objekt).Name), colour&("fg"), "r"
            NewText 3, 0, "Name:         " + RTRIM$(Kategorie(node).Name), colour&("fg"), "r"
            NewMenItem 5, 0, "<- Abbrechen", "back"
            NewMenItem 5, 16, "Bearbeiten", "edit"
            NewMenItem 5, 30, "L" + CHR$(148) + "schen", "delete"
            NewMenItem 5, 41, "Kopieren", "copy"
            RunMenu 1, 0, "KATEGORIE"
    END SELECT
END SUB

SUB edit (listID$, node)
    SELECT CASE listID$
        CASE "ver"
            a = 0: DO: a = a + 1
                arraydata$(1, a) = LST$(Ausgabe(a).Monat): IF Ausgabe(a).Monat = Veranstaltung(node).Ausgabe THEN standard = a
            LOOP UNTIL a = max(3): maxad(1) = max(3)
            NewSelector 1, 0, "Ausgabe:      ", 1, "asg", standard
            NewDate 2, 0, "Datum:        ", RTRIM$(Veranstaltung(node).Datum)
            ot = 0: DO: ot = ot + 1
                arraydata$(2, ot) = RTRIM$(Ort(ot).Name): IF RTRIM$(Ort(ot).Name) = RTRIM$(Veranstaltung(node).Ort) THEN standard = ot
            LOOP UNTIL ot = max(4): maxad(2) = max(4)
            NewSelector 3, 0, "Ort:          ", 2, "ort", standard
            IF max(9) > 0 THEN
                va = 0: DO: va = va + 1
                    arraydata$(3, va) = RTRIM$(Veranstalter(va).Kuerzel) + ", " + RTRIM$(Veranstalter(va).Name): IF RTRIM$(Veranstalter(va).Name) = RTRIM$(Veranstaltung(node).Veranstalter) THEN standard = va
                LOOP UNTIL max(9): maxad(3) = max(9)
                NewSelector 4, 0, "Veranstalter: ", 3, "vea", standard
            ELSE
                NewMenItem 4, 0, "Neuen Veranstalter erstellen", "new"
            END IF
            r = 0: DO: r = r + 1
                arraydata$(4, r) = RTRIM$(Rubrik(r).Name): IF RTRIM$(Rubrik(r).Name) = RTRIM$(Veranstaltung(node).Rubrik) THEN standard = r
            LOOP UNTIL max(8): maxad(4) = max(8)
            NewSelector 5, 0, "Rubrik:       ", 4, "rbk", standard
            NewTime 6, 0, "Zeit 1:       ", RTRIM$(Veranstaltung(node).Zeit1)
            NewTime 7, 0, "Zeit 2:       ", RTRIM$(Veranstaltung(node).Zeit2)
            NewTime 8, 0, "Zeit 3:       ", RTRIM$(Veranstaltung(node).Zeit3)
            NewInput 9, 0, "Zeitcode:     ", RTRIM$(Veranstaltung(node).Zeitcode), 0
            NewInput 10, 0, "Titel:        ", RTRIM$(Veranstaltung(node).Titel), 0
            NewInput 11, 0, "Text:         ", RTRIM$(Veranstaltung(node).Text), 0
            NewInput 12, 0, "Langer Text:  ", RTRIM$(Veranstaltung(node).TextLang), 0
            NewMenItem 14, 0, "Speichern", "save"
            NewMenItem 14, 13, "<- Abbrechen", "back"
            NewMenItem 14, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "VERANSTALTUNG BEARBEITEN"
        CASE "kaz"
            kt = 0: DO: kt = kt + 1
                arraydata$(1, kt) = RTRIM$(Kategorie(kt).Name): IF RTRIM$(Kategorie(kt).Name) = RTRIM$(Kleinanzeige(node).Kategorie1) THEN standard = kt
            LOOP UNTIL kt = max(12): maxad(1) = max(12)
            NewSelector 1, 0, "Kategorie 1:  ", 1, "rbk", standard
            kt = 0: DO: kt = kt + 1
                arraydata$(2, kt) = RTRIM$(Kategorie(kt).Name): IF RTRIM$(Kategorie(kt).Name) = RTRIM$(Kleinanzeige(node).Kategorie2) THEN standard = kt
            LOOP UNTIL kt = max(12): maxad(2) = max(12)
            NewSelector 2, 0, "Kategorie 2:  ", 2, "rbk", standard
            kt = 0: DO: kt = kt + 1
                arraydata$(3, kt) = RTRIM$(Kategorie(kt).Name): IF RTRIM$(Kategorie(kt).Name) = RTRIM$(Kleinanzeige(node).Kategorie3) THEN standard = kt
            LOOP UNTIL kt = max(12): maxad(3) = max(12)
            NewSelector 3, 0, "Kategorie 3:  ", 3, "rbk", standard
            NewInput 4, 0, "Name:         ", RTRIM$(Kleinanzeige(node).Name), 0
            NewInput 5, 0, "Titel:        ", RTRIM$(Kleinanzeige(node).Titel), 0
            NewInput 6, 0, "Text:         ", RTRIM$(Kleinanzeige(node).Text), 0
            o = 0: DO: o = o + 1
                arraydata$(4, o) = RTRIM$(Objekt(o).Name): IF Objekt(o).ID = Kleinanzeige(node).Objekt THEN standard = o
            LOOP UNTIL o = max(2): maxad(4) = max(2)
            NewSelector 7, 0, "Objekt:       ", 4, "obj", standard
            a = 0: DO: a = a + 1
                arraydata$(5, a) = LST$(Ausgabe(a).Monat): IF Ausgabe(a).Monat = Kleinanzeige(node).Ausgabe THEN standard = a
            LOOP UNTIL a = max(3): maxad(5) = max(3)
            NewSelector 8, 0, "Ausgabe:      ", 5, "asg", standard
            NewInput 9, 0, "Telefon:      ", RTRIM$(Kleinanzeige(node).Telefon), 1
            NewInput 10, 0, "Chiffre:      ", RTRIM$(Kleinanzeige(node).Chiffre), 0
            NewInput 11, 0, "Notiz:        ", RTRIM$(Kleinanzeige(node).Notiz), 0
            NewMenItem 13, 0, "Speichern", "save"
            NewMenItem 13, 13, "<- Abbrechen", "back"
            NewMenItem 13, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "KLEINANZEIGE BEARBEITEN"
        CASE "vea"
            NewInput 1, 0, "K" + CHR$(129) + "rzel:       ", RTRIM$(Veranstalter(node).Kuerzel), 0
            NewInput 2, 0, "Name:         ", RTRIM$(Veranstalter(node).Name), 0
            IF max(6) > 0 THEN
                d = 0: DO: d = d + 1
                    arraydata$(1, d) = RTRIM$(Adresse(d).Ort) + ", " + RTRIM$(Adresse(d).Strasse): IF Adresse(d).ID = Veranstalter(node).Adresse THEN standard = d
                LOOP UNTIL d = max(6): maxad(3) = max(6)
                NewSelector 3, 0, "Adresse:      ", 1, "adr", standard
            END IF
            NewInput 4, 0, "Telefon:      ", LST$(Veranstalter(node).Telefon), 1
            NewInput 5, 0, "Telefax:      ", LST$(Veranstalter(node).Telefax), 1
            NewInput 6, 0, "Anrede:       ", RTRIM$(Veranstalter(node).Anrede), 0
            NewInput 7, 0, "Notiz:        ", RTRIM$(Veranstalter(node).Notiz), 0
            NewMenItem 9, 0, "Speichern", "save"
            NewMenItem 9, 13, "<- Abbrechen", "back"
            NewMenItem 9, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "VERANSTALTER BEARBEITEN"
        CASE "adr"
            p = 0: DO
                p = p + 1: arraydata$(1, p) = LST$(PLZ(p).PLZ): IF PLZ(p).PLZ = Adresse(node).PLZ THEN standard = p
            LOOP UNTIL p = max(5): maxad(1) = max(5)
            NewSelector 1, 0, "Postleitzahl: ", 1, "adr", standard
            ot = 0: DO: ot = ot + 1
                arraydata$(2, ot) = RTRIM$(Ort(ot).Name): IF RTRIM$(Ort(ot).Name) = RTRIM$(Adresse(node).Ort) THEN standard = ot
            LOOP UNTIL ot = max(4): maxad(2) = max(4)
            NewSelector 2, 0, "Ort:          ", 2, "ort", standard
            NewInput 3, 0, "Land:         ", RTRIM$(Adresse(node).Land), 0
            NewInput 4, 0, "Strasse:      ", RTRIM$(Adresse(node).Strasse), 0
            NewMenItem 6, 0, "Speichern", "save"
            NewMenItem 6, 13, "<- Abbrechen", "back"
            NewMenItem 6, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "ADRESSE BEARBEITEN"
        CASE "rbk"
            NewInput 1, 0, "K" + CHR$(129) + "rzel:       ", RTRIM$(Rubrik(node).Kuerzel), 0
            o = 0: DO: o = o + 1
                arraydata$(1, o) = RTRIM$(Objekt(o).Name): IF Objekt(o).ID = Rubrik(node).Objekt THEN standard = o
            LOOP UNTIL o = max(2): maxad(2) = max(2)
            NewSelector 2, 0, "Objekt:       ", 1, "obj", standard
            NewInput 3, 0, "Name:         ", RTRIM$(Rubrik(node).Name), 0
            NewMenItem 5, 0, "Speichern", "save"
            NewMenItem 5, 13, "<- Abbrechen", "back"
            NewMenItem 5, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "RUBRIK BEARBEITEN"
        CASE "usr"
            IF node = 0 THEN
                DO: node = node + 1
                    IF RTRIM$(User(node).Name) = username$ THEN
                        GOTO nodeexit
                    END IF
                LOOP UNTIL node = max(1)
                nodeexit:
            END IF
            NewInput 1, 0, "Name:         ", RTRIM$(User(node).Name), 0
            NewInput 2, 0, "Passwort:     ", RTRIM$(User(node).Passwort), 0
            SetArrayData 1, 1: c = 0: DO: c = c + 1:
                IF c = User(node).Zugang THEN standard = c
            LOOP UNTIL c = maxad(1)
            NewSelector 3, 0, "Zugang:       ", 1, "", standard
            SetArrayData 2, 2: c = 0: DO: c = c + 1:
                IF arraydata$(2, c) = RTRIM$(User(node).Abteilung) THEN standard = c
            LOOP UNTIL c = maxad(2)
            NewSelector 4, 0, "Abteilung:    ", 2, "", standard
            NewInput 5, 0, "Telefon:      ", LST$(User(node).Telefon), 1
            NewMenItem 7, 0, "Speichern", "save"
            NewMenItem 7, 13, "<- Abbrechen", "back"
            NewMenItem 7, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "BENUTZER BEARBEITEN"
        CASE "ort"
            NewInput 1, 0, "K" + CHR$(129) + "rzel:       ", RTRIM$(Ort(node).Kuerzel), 0
            NewInput 2, 0, "Name:         ", RTRIM$(Ort(node).Name), 0
            NewMenItem 4, 0, "Speichern", "save"
            NewMenItem 4, 13, "<- Abbrechen", "back"
            NewMenItem 4, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "ORT BEARBEITEN"
        CASE "plz"
            NewInput 1, 0, "Postleitzahl: ", LST$(PLZ(node).PLZ), 1
            ot = 0: DO: ot = ot + 1
                arraydata$(1, ot) = RTRIM$(Ort(node).Name): IF arraydata$(1, ot) = RTRIM$(PLZ(node).Ort) THEN standard = ot
            LOOP UNTIL ot = max(4): maxad(1) = max(4)
            NewSelector 2, 0, "Ort:          ", 1, "ort", 1
            NewInput 3, 0, "Land:         ", RTRIM$(PLZ(node).Land), 0
            NewMenItem 5, 0, "Speichern", "save"
            NewMenItem 5, 13, "<- Abbrechen", "back"
            NewMenItem 5, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "POSTLEITZAHL BEARBEITEN"
        CASE "asg"
            o = 0: DO: o = o + 1
                arraydata$(1, o) = RTRIM$(Objekt(o).Name): IF Objekt(o).ID = Ausgabe(node).Objekt THEN standard = o
            LOOP UNTIL o = max(2): maxad(2) = max(2)
            NewSelector 1, 0, "Objekt:       ", 1, "obj", standard
            NewInput 2, 0, "Monat:        ", LST$(Ausgabe(node).Monat), 1
            NewMenItem 4, 0, "Speichern", "save"
            NewMenItem 4, 13, "<- Abbrechen", "back"
            NewMenItem 4, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "AUSGABE BEARBEITEN"
        CASE "kat"
            NewInput 1, 0, "K" + CHR$(129) + "rzel:       ", RTRIM$(Kategorie(node).Kuerzel), 0
            o = 0: DO: o = o + 1
                arraydata$(1, o) = RTRIM$(Objekt(o).Name): IF Objekt(o).ID = Kategorie(node).Objekt THEN standard = o
            LOOP UNTIL o = max(2): maxad(2) = max(2)
            NewSelector 2, 0, "Objekt:       ", 1, "obj", standard
            NewInput 3, 0, "Name:         ", RTRIM$(Kategorie(node).Name), 0
            NewMenItem 5, 0, "Speichern", "save"
            NewMenItem 5, 13, "<- Abbrechen", "back"
            NewMenItem 5, 29, "L" + CHR$(148) + "schen", "delete"
            NewText INT((maxlines - firstline) / 2), 0, "[STRG + ENTER], um eine Liste zu bearbeiten", colour&("offfocus"), "r"
            RunMenu 1, 0, "KATEGORIE BEARBEITEN"
    END SELECT
END SUB

FUNCTION search (listID$, placeholder$)
    reprintsearch:
    Background 0, "SUCHE:", 1
    COLOR colour&("transparent"), colour&("transparent")
    searchheader listID$
    LOCATE firstline + 3, firstchar
    PRINT "Keine Suchergebnisse."
    selectedo = 0

    searchxoffset = 14
    searchyoffset = 2

    LINE ((firstchar + 30 + searchxoffset - 1) * fontwidth, (searchyoffset + 1) * fontheight + 5)-((firstchar + 60 + searchxoffset) * fontwidth + (fontwidth / 2), ((searchyoffset + 1) * fontheight) + 9), colour&("fg"), BF

    endsearch = 0
    g = 0
    gbf = 0
    IF placeholder$ <> "" THEN
        LINE ((firstchar + 30 + searchxoffset + gbf - 1) * fontwidth + (fontwidth / 2), (1 + searchyoffset) * fontheight + 1)-((firstchar + 30 + searchxoffset + gbf - 1) * fontwidth + (fontwidth / 2) + 1, searchyoffset * fontheight - 1), colour&("bg"), BF
        LOCATE 1 + searchyoffset, firstchar + 30 + searchxoffset
        DO
            g = g + 1
            zeichen$(g) = MID$(placeholder$, g, 1)
            PRINT zeichen$(g);
        LOOP UNTIL g = LEN(placeholder$)
        gbf = g
        suchbegriff$ = placeholder$
        suchbegriffbf$ = suchbegriff$
    END IF
    DO
        Taste$ = INKEY$
        IF TIMER MOD 2 = 0 THEN
            LINE ((firstchar + 30 + searchxoffset + g - 1) * fontwidth + (fontwidth / 2), (1 + searchyoffset) * fontheight + 1)-((firstchar + 30 + searchxoffset + g - 1) * fontwidth + (fontwidth / 2) + 1, searchyoffset * fontheight - 1), colour&("fg"), BF
        ELSE
            LINE ((firstchar + 30 + searchxoffset + g - 1) * fontwidth + (fontwidth / 2), (1 + searchyoffset) * fontheight + 1)-((firstchar + 30 + searchxoffset + g - 1) * fontwidth + (fontwidth / 2) + 1, searchyoffset * fontheight - 1), colour&("bg"), BF
        END IF

        mouseinput = _MOUSEINPUT
        mousebutton = _MOUSEBUTTON(1)
        mousex = _MOUSEX
        mousey = _MOUSEY
        IF mouseinput = -1 THEN
            IF mousebutton = -1 THEN
                IF mousey >= buttonsly AND mousey <= buttonsuy THEN 'close button
                    IF mousex >= closebuttonlx AND mousex <= closebuttonux THEN
                        logThis "Programm durch X-Button beendet."
                        SYSTEM
                    END IF
                    IF mousex >= minbuttonlx AND mousex <= minbuttonux THEN 'minimize button
                        minimize
                        IF maximized = 1 THEN
                            GOTO reprintsearch
                        END IF
                    END IF
                END IF
                IF mousey < topbarheight THEN
                    DO
                        mouseinput = _MOUSEINPUT
                        mousebutton = _MOUSEBUTTON(1)
                    LOOP UNTIL mousebutton <> -1
                    _SCREENMOVE _DESKTOPWIDTH / 2 - (_WIDTH / 2) + (_MOUSEX - mousex), _DESKTOPHEIGHT / 2 - (_HEIGHT / 2) + (_MOUSEY - mousey)
                END IF
            END IF
        END IF
        IF mouseinput = -1 THEN
            mousex = _MOUSEX
            mousey = _MOUSEY
            'IF maxo > 0 THEN
            '    o2 = 0: DO: o2 = o2 + 1
            '        IF mousex > firstchar * fontwidth AND mousex < fontwidth * (maxrows - (firstchar * 2)) THEN
            '            IF mousey > fontheight * (firstline + o2 - selectedo + 2) AND mousey < fontheight * (firstline + o2 - selectedo + 3) THEN
            '                selectedo = o2: reprinto = 1
            '            END IF
            '        END IF
            '    LOOP UNTIL o2 >= maxo OR o2 - selectedo >= maxlines - firstline - 3
            'END IF

            IF mousex > buttonlx AND mousex < buttonux THEN
                IF mousey > buttonly AND mousey < buttonuy THEN
                    IF buttonstatus = 0 THEN buttonstatus = 1: searchbutton
                ELSE
                    IF buttonstatus = 1 THEN buttonstatus = 0: searchbutton
                END IF
            ELSE
                IF buttonstatus = 1 THEN buttonstatus = 0: searchbutton
            END IF

            IF _MOUSEBUTTON(1) = -1 AND buttonstatus = 1 THEN endsearch = 1: Taste$ = "Button"
            IF _MOUSEWHEEL = -1 THEN IF selectedo > 1 THEN selectedo = selectedo - 1: reprinto = 1 ELSE selectedo = maxo: reprinto = 1
            IF _MOUSEWHEEL = 1 THEN IF selectedo < maxo THEN selectedo = selectedo + 1: reprinto = 1 ELSE selectedo = 1: reprinto = 1
        END IF

        IF Taste$ <> "" THEN
            IF Taste$ = CHR$(8) AND g > 0 THEN
                g = g - 1
                zeichen$(g + 1) = ""
                suchbegriff$ = MID$(suchbegriff$, 1, LEN(suchbegriff$) - 1)
            ELSE
                IF Taste$ = CHR$(13) THEN endsearch = 1
                IF Taste$ = CHR$(27) THEN endsearch = 1
                IF Taste$ = CHR$(0) + CHR$(72) THEN IF selectedo > 1 THEN selectedo = selectedo - 1: reprinto = 1 ELSE selectedo = maxo: reprinto = 1
                IF Taste$ = CHR$(0) + CHR$(80) THEN IF selectedo < maxo THEN selectedo = selectedo + 1: reprinto = 1 ELSE selectedo = 1: reprinto = 1
                ac = 0
                DO
                    ac = ac + 1
                    IF ac$(ac) = Taste$ THEN
                        g = g + 1
                        zeichen$(g) = Taste$
                    END IF
                LOOP UNTIL ac = alch
                IF Taste$ = CHR$(228) THEN 'ae
                    g = g + 1
                    zeichen$(g) = CHR$(132)
                ELSEIF Taste$ = CHR$(252) THEN 'ue
                    g = g + 1
                    zeichen$(g) = CHR$(129)
                ELSEIF Taste$ = CHR$(249) THEN 'oe
                    g = g + 1
                    zeichen$(g) = CHR$(148)
                END IF
            END IF
        END IF

        IF gbf <> g THEN
            selectedo = 1
            LINE ((firstchar + 30 + searchxoffset + gbf - 1) * fontwidth + (fontwidth / 2), (1 + searchyoffset) * fontheight + 1)-((firstchar + 30 + searchxoffset + gbf - 1) * fontwidth + (fontwidth / 2) + 1, searchyoffset * fontheight - 1), colour&("bg"), BF
            gbf = g
            LOCATE 1 + searchyoffset, firstchar + 30 + searchxoffset
            COLOR colour&("fg"), colour&("bg")
            suchbegriff$ = ""
            IF g > 0 THEN
                g = 0
                DO
                    g = g + 1
                    PRINT zeichen$(g);
                    suchbegriff$ = suchbegriff$ + zeichen$(g)
                LOOP UNTIL g = gbf
                PRINT " "
            ELSE
                PRINT " "
            END IF
        END IF

        'LOCATE 1, 1: PRINT suchbegriff$, suchbegriffbf$, endparameter$, placeholder$ + "    "    'hier checken ob alles passt wenn die suche nicht klappt
        IF suchbegriffbf$ <> suchbegriff$ OR reprinto = 1 OR placeholder$ <> "" THEN
            placeholder$ = ""
            reprinto = 0
            IF suchbegriff$ = "" OR suchbegriff$ = " " THEN LINE (1, (firstline) * fontheight)-(maxx, maxy), colour&("bg")
            maxo = 0
            o = 0
            node = 0
            IF max(listID) > 0 THEN
                DO
                    node = node + 1
                    'Suchalgorithmus
                    SELECT CASE listID$
                        CASE "usr"
                            SetArrayData 30, 1
                            searched$(1) = User(node).Name: searched$(2) = arraydata$(30, User(node).Zugang): searched$(3) = User(node).Abteilung: searched$(4) = LST$(User(node).Telefon): searched$(5) = LST$(User(node).Zugang)
                            searchpart 5
                        CASE "obj"
                            searched$(1) = Objekt(node).Name: searched$(2) = Objekt(node).Waehrung:
                            searchpart 2
                        CASE "asg"
                            searched$(1) = Objekt(Ausgabe(node).Objekt).Name: searched$(2) = LST$(Ausgabe(node).Monat)
                            searchpart 2
                        CASE "ort"
                            searched$(1) = Ort(node).Kuerzel: searched$(2) = Ort(node).Name
                            searchpart 2
                        CASE "plz"
                            searched$(1) = LST$(PLZ(node).PLZ): searched$(2) = PLZ(node).Land: searched$(3) = PLZ(node).Ort
                            searchpart 3
                        CASE "adr"
                            searched$(1) = LST$(Adresse(node).PLZ): searched$(2) = Adresse(node).Ort: searched$(3) = Adresse(node).Land: searched$(4) = Adresse(node).Strasse
                            searchpart 4
                        CASE "knt"
                            searched$(1) = Konto(node).BIC: searched$(2) = Konto(node).IBAN: searched$(3) = Konto(node).Inhaber
                            searchpart 3
                        CASE "rbk"
                            searched$(1) = Rubrik(node).Kuerzel
                            searched$(2) = Objekt(Rubrik(node).Objekt).Name
                            searched$(3) = Rubrik(node).Name
                            searchpart 3
                        CASE "vea"
                            d = 0: DO: d = d + 1
                                IF Adresse(d).ID = Veranstalter(node).Adresse THEN
                                    searched$(8) = LST$(Adresse(d).PLZ)
                                    searched$(9) = Adresse(d).Ort
                                    searched$(10) = Adresse(d).Strasse
                                END IF
                            LOOP UNTIL d = max(6)
                            searched$(1) = Veranstalter(node).Kuerzel: searched$(2) = Veranstalter(node).Name: searched$(3) = LST$(Veranstalter(node).Telefon): searched$(4) = LST$(Veranstalter(node).Telefax): searched$(5) = Veranstalter(node).Sachbearbeiter: searched$(6) = Veranstalter(node).Anrede: searched$(7) = Veranstalter(node).Notiz
                            searchpart 10
                        CASE "ver"
                            searched$(1) = LST$(Veranstaltung(node).ID): searched$(2) = LST$(Veranstaltung(node).Ausgabe): searched$(3) = Veranstaltung(node).Datum: searched$(4) = Veranstaltung(node).Veranstalter: searched$(5) = Veranstaltung(node).Rubrik: searched$(6) = Veranstaltung(node).Zeit1: searched$(7) = Veranstaltung(node).Zeit2: searched$(8) = Veranstaltung(node).Titel: searched$(9) = Veranstaltung(node).Text
                            searchpart 9
                        CASE "kaz"
                            searched$(1) = Kleinanzeige(node).Kategorie1: searched$(2) = Kleinanzeige(node).Kategorie2: searched$(3) = Kleinanzeige(node).Kategorie3: searched$(4) = Kleinanzeige(node).Text: searched$(5) = Objekt(Kleinanzeige(node).Objekt).Name: searched$(6) = LST$(Kleinanzeige(node).Ausgabe): searched$(7) = Kleinanzeige(node).Telefon: searched$(8) = Kleinanzeige(node).Name: searched$(9) = Kleinanzeige(node).Chiffre: searched$(10) = Kleinanzeige(node).Notiz: searched$(11) = Kleinanzeige(node).Titel
                            searchpart 11
                        CASE "kat"
                            searched$(1) = Kategorie(node).Kuerzel: searched$(2) = Objekt(Kategorie(node).Objekt).Name: searched$(3) = Kategorie(node).Name
                            searchpart 3
                    END SELECT
                LOOP UNTIL node = max(listID)
                maxo = o
                IF maxo > 0 THEN
                    LINE (1, (firstline) * fontheight)-(maxx, maxy), colour&("bg"), BF
                    searchheader listID$
                    o = selectedo - 1
                    DO
                        o = o + 1
                        IF darkmode = 1 THEN
                            IF o = selectedo THEN COLOR colour&("fg"), colour&("dark grey") ELSE COLOR colour&("fg"), colour&("bg")
                        ELSE
                            IF o = selectedo THEN COLOR colour&("fg"), colour&("light grey") ELSE COLOR colour&("fg"), colour&("bg")
                        END IF
                        LOCATE firstline + o - selectedo + 3, firstchar
                        SELECT CASE listID 'auflistung der suchergebnisse
                            CASE 1
                                printsearchlist RTRIM$(User(listnode(o)).Name), 2
                                SetArrayData 30, 1
                                printsearchlist RTRIM$(arraydata$(30, User(listnode(o)).Zugang)), 3
                                printsearchlist RTRIM$(User(listnode(o)).LetzterLogin), 2
                            CASE 2
                                printsearchlist RTRIM$(LST$(Objekt(listnode(o)).ID)), 2
                                printsearchlist RTRIM$(Objekt(listnode(o)).Name), 2
                            CASE 3
                                printsearchlist RTRIM$(Objekt(Ausgabe(listnode(o)).Objekt).Name), 2
                                printsearchlist RTRIM$(LST$(Ausgabe(listnode(o)).Monat)), 2
                            CASE 4
                                printsearchlist RTRIM$(Ort(listnode(o)).Kuerzel), 2
                                printsearchlist RTRIM$(Ort(listnode(o)).Name), 2
                            CASE 5
                                printsearchlist RTRIM$(LST$(PLZ(listnode(o)).PLZ)), 1
                                printsearchlist RTRIM$(PLZ(listnode(o)).Land), 2
                                printsearchlist RTRIM$(PLZ(listnode(o)).Ort), 2
                            CASE 6
                                printsearchlist RTRIM$(LST$(Adresse(listnode(o)).ID)), 1
                                printsearchlist RTRIM$(LST$(Adresse(listnode(o)).PLZ)), 1
                                printsearchlist RTRIM$(Adresse(listnode(o)).Ort), 2
                                printsearchlist RTRIM$(Adresse(listnode(o)).Strasse), 2
                            CASE 7
                                printsearchlist RTRIM$(LST$(Konto(listnode(o)).ID)), 1
                                printsearchlist RTRIM$(Konto(listnode(o)).BIC), 2
                                printsearchlist RTRIM$(Konto(listnode(o)).IBAN), 2
                                printsearchlist RTRIM$(Konto(listnode(o)).Inhaber), 2
                            CASE 8
                                printsearchlist RTRIM$(Rubrik(listnode(o)).Kuerzel), 2
                                printsearchlist RTRIM$(Rubrik(listnode(o)).Name), 2
                            CASE 9
                                printsearchlist RTRIM$(Veranstalter(listnode(o)).Kuerzel), 2
                                printsearchlist RTRIM$(Veranstalter(listnode(o)).Name), 3
                                d = 0: DO: d = d + 1
                                    IF Veranstalter(listnode(o)).Adresse = Adresse(d).ID THEN
                                        printsearchlist RTRIM$(Adresse(d).Ort) + ", " + RTRIM$(Adresse(d).Strasse), 3
                                    END IF
                                LOOP UNTIL d = max(6)
                            CASE 10
                                printsearchlist RTRIM$(LST$(Kleinanzeige(listnode(o)).ID)), 1
                                printsearchlist RTRIM$(LST$(Kleinanzeige(listnode(o)).Ausgabe)), 1
                                printsearchlist RTRIM$(Kleinanzeige(listnode(o)).Name), 2
                                printsearchlist RTRIM$(Kleinanzeige(listnode(o)).Chiffre), 1
                                printsearchlist RTRIM$(Kleinanzeige(listnode(o)).Titel), 2
                            CASE 11
                                printsearchlist RTRIM$(LST$(Veranstaltung(listnode(o)).ID)), 1
                                printsearchlist RTRIM$(LST$(Veranstaltung(listnode(o)).Ausgabe)), 1
                                printsearchlist RTRIM$(Veranstaltung(listnode(o)).Titel), 2
                                printsearchlist RTRIM$(Veranstaltung(listnode(o)).Datum), 2
                            CASE 12
                                printsearchlist RTRIM$(Kategorie(listnode(o)).Kuerzel), 2
                                printsearchlist RTRIM$(Kategorie(listnode(o)).Name), 2
                        END SELECT
                    LOOP UNTIL o >= maxo OR o - selectedo >= maxlines - firstline - 3

                    'half- to nonvariable elements
                    LINE ((firstchar - 1.5) * fontwidth, (firstline + 1.5) * fontheight)-(fontwidth * (maxrows - (firstchar * 2)), (firstline + 1.5) * fontheight), colour&("fg"), BF 'top line
                    IF searchbottomfixed = 1 THEN
                        LINE ((firstchar - 1.5) * fontwidth, (maxlines - firstline + 5) * fontheight)-(fontwidth * (maxrows - (firstchar * 2)), (maxlines - firstline + 5) * fontheight), colour&("fg"), BF 'bottom line
                    ELSE
                        LINE ((firstchar - 1.5) * fontwidth, (firstline + o - selectedo + 3) * fontheight)-(fontwidth * (maxrows - (firstchar * 2)), (firstline + o - selectedo + 3) * fontheight), colour&("fg"), BF 'bottom line
                    END IF
                    LINE ((firstchar - 1.5) * fontwidth, (firstline + 1.5) * fontheight)-((firstchar - 1.5) * fontwidth, (maxlines - firstline + 5) * fontheight), colour&("fg"), BF 'left line
                    LINE (fontwidth * (maxrows - (firstchar * 2)), (firstline + 1.5) * fontheight)-(fontwidth * (maxrows - (firstchar * 2)), (maxlines - firstline + 5) * fontheight), colour&("fg"), BF 'search indicator line
                    factor = (selectedo - 1) / maxo
                    difference = ((maxlines - 3) * fontheight) - (firstline * fontheight)
                    LINE (fontwidth * (maxrows - (firstchar * 2)) - 10, ((firstline + 1.5) * fontheight) + (factor * difference))-(fontwidth * (maxrows - (firstchar * 2)), ((firstline + 1.5) * fontheight) + (factor * difference) + 30), colour&("fg"), BF 'search indicator
                ELSE
                    LINE (1, (firstline) * fontheight)-(maxx, maxy), colour&("bg"), BF
                    LOCATE firstline + 1, firstchar
                    PRINT "Keine Suchergebnisse."
                END IF
                searchbutton
                suchbegriffbf$ = suchbegriff$
                _DISPLAY
            END IF
        END IF
        '_LIMIT 30
    LOOP UNTIL endsearch = 1
    _AUTODISPLAY
    lastsearchinput$ = suchbegriff$
    IF Taste$ <> CHR$(27) AND Taste$ <> "Button" THEN
        search = listnode(selectedo)
    ELSE
        search = 0
    END IF
END FUNCTION

SUB searchbutton
    LOCATE maxlines + 2, firstchar
    buttonlx = (firstchar - 2) * fontwidth
    buttonux = (firstchar + LEN("<- Abbrechen") + 1.5) * fontwidth
    buttonly = ((maxlines + 1) * fontheight) - 7
    buttonuy = (maxlines + 2) * fontheight + 3
    IF buttonstatus = 0 THEN
        rectangle buttonlx, buttonly, buttonux, buttonuy, UMround, colour&("bg"), "BF"
        rectangle buttonlx, buttonly, buttonux, buttonuy, UMround, colour&("fg"), "B"
        COLOR colour&("fg"), colour&("transparent")
    ELSE
        rectangle buttonlx, buttonly, buttonux, buttonuy, UMround, colour&("fg"), "BF"
        COLOR colour&("bg"), colour&("transparent")
    END IF
    PRINT "<- Abbrechen",
    COLOR colour&("fg"), colour&("bg")
    PRINT "Suchergebnisse: " + LST$(maxo) + "    "
    _DISPLAY
END SUB

SUB searchheader (listID$)
    LOCATE firstline + 1, firstchar
    COLOR colour&("fg"), colour&("transparent")
    SELECT CASE listID$
        CASE "usr": listID = 1: PRINT "Benutzername", "Zugangsberechtigung", , "Letzter Login"
        CASE "obj": listID = 2: PRINT "ID", , "Name"
        CASE "asg": listID = 3: PRINT "Objekt", , "Monat"
        CASE "ort": listID = 4: PRINT "K" + CHR$(129) + "rzel", , "Ort"
        CASE "plz": listID = 5: PRINT "PLZ", "Land", , "Ort"
        CASE "adr": listID = 6: PRINT "ID", "PLZ", "Ort", , "Strasse"
        CASE "knt": listID = 7: PRINT "ID", "BIC", "IBAN", , "Inhaber"
        CASE "rbk": listID = 8: PRINT "K" + CHR$(129) + "rzel", , "Name"
        CASE "vea": listID = 9: PRINT "K" + CHR$(129) + "rzel", , "Name", , , "Adresse"
        CASE "kaz": listID = 10: PRINT "ID", "Ausgabe", "Name", , "Chiffre", "Titel"
        CASE "ver": listID = 11: PRINT "ID", "Ausgabe", "Titel", , "Datum"
        CASE "kat": listID = 12: PRINT "K" + CHR$(129) + "rzel", , "Name"
    END SELECT
END SUB

SUB searchpart (maxs)
    IF maxs > 0 THEN
        s = 0: DO: s = s + 1
            searched$(s) = RTRIM$(LTRIM$(searched$(s)))
            IF suchbegriff$ = searched$(s) AND listnode(o) <> node THEN
                addList node
            ELSE
                IF LEN(searched$(s)) > LEN(suchbegriff$) AND listnode(o) <> node THEN
                    po = 0: DO: po = po + 1
                        IF MID$(searched$(s), po, LEN(suchbegriff$)) = suchbegriff$ AND listnode(o) <> node THEN addList node 'normal casing
                        IF UCASE$(MID$(searched$(s), po, LEN(suchbegriff$))) = UCASE$(suchbegriff$) AND listnode(o) <> node THEN addList node 'uppercase
                    LOOP UNTIL po = LEN(searched$(s)) - LEN(suchbegriff$) + 1 OR listnode(o) = node
                END IF
            END IF
        LOOP UNTIL s = maxs
    END IF
END SUB

SUB addList (node)
    o = o + 1
    maxo = o
    listnode(o) = node
END SUB

SUB printsearchlist (tekst$, comcount) 'comcount seems to only be working if > 1
    'PRINT tekst$;
    length = LEN(tekst$)
    IF length < 14 THEN
        SELECT CASE comcount
            CASE 3
                PRINT tekst$, , ,
            CASE 2
                PRINT tekst$, ,
            CASE 1
                PRINT tekst$,
        END SELECT
    ELSE
        IF length < 28 THEN
            SELECT CASE comcount
                CASE 3
                    PRINT tekst$, ,
                CASE 2
                    PRINT tekst$,
                CASE 1
                    PRINT tekst$,
            END SELECT
        ELSE
            PRINT tekst$,
        END IF
    END IF
END SUB

SUB RunGraph (endparameter$)
    empty = 1
    SELECT CASE endparameter$
        CASE "kaz"
            grapha = -1: DO: grapha = grapha + 1
                IF max(10) > 0 THEN
                    ka = 0: DO: ka = ka + 1
                        IF Kleinanzeige(ka).Ausgabe = Ausgabe(max(3) - grapha).Monat THEN
                            pcount(12 - grapha) = pcount(12 - grapha) + 1
                            empty = 0
                        END IF
                    LOOP UNTIL ka = max(10)
                END IF
            LOOP UNTIL grapha = max(3) - 1 OR grapha = 11
        CASE "ver"
            grapha = -1: DO: grapha = grapha + 1
                IF max(11) > 0 THEN
                    va = 0: DO: va = va + 1
                        IF Veranstaltung(va).Ausgabe = Ausgabe(max(3) - grapha).Monat THEN
                            pcount(12 - grapha) = pcount(12 - grapha) + 1
                            empty = 0
                        END IF
                    LOOP UNTIL va = max(11)
                END IF
            LOOP UNTIL grapha = max(3) - 1 OR grapha = 11
    END SELECT
    IF empty = 99 THEN
        'calculate graph variables
        graphoffsety = -20
        maxpcount = 0
        average = 0
        grapha = 0: DO: grapha = grapha + 1
            IF maxpcount <= pcount(grapha) THEN maxpcount = pcount(grapha)
        LOOP UNTIL grapha = 12
        factor = maxy / 6 / maxpcount
        grapha = 0: DO: grapha = grapha + 1
            pheight(grapha) = factor * pcount(grapha)
            pcount(grapha) = 0
            average = average + pheight(grapha)
        LOOP UNTIL grapha = 12
        average = average / 12
        'output graph
        LINE (0, maxy - (firstchar * fontwidth) - (maxy / 6) + graphoffsety - 50)-(maxx, maxy), colour&("fg"), BF 'Background
        IF darkmode = 1 THEN
            LINE (firstchar * fontwidth, maxy - (firstchar * fontwidth) - average + graphoffsety)-(maxx - (firstchar * fontwidth * 2), maxy - (firstchar * fontwidth) - average + graphoffsety), colour&("light grey"), BF 'average
        ELSE
            LINE (firstchar * fontwidth, maxy - (firstchar * fontwidth) + graphoffsety)-(maxx - (firstchar * fontwidth * 2), maxy - (firstchar * fontwidth) - average + graphoffsety), colour&("dark grey"), BF 'average
        END IF
        LINE (firstchar * fontwidth, maxy - (firstchar * fontwidth) + graphoffsety)-(maxx - (firstchar * fontwidth * 2), maxy - (firstchar * fontwidth) + graphoffsety + 2), colour&("bg"), BF 'x-axis
        LINE ((firstchar - 0.75) * fontwidth, maxy - (firstchar * fontwidth) - (maxy / 6) + graphoffsety)-(firstchar * fontwidth, maxy - (firstchar * fontwidth) - (maxy / 6) + graphoffsety), colour&("bg"), BF 'top label line
        LINE (firstchar * fontwidth, maxy - (firstchar * fontwidth) + graphoffsety + 2)-(firstchar * fontwidth - 2, maxy - (firstchar * fontwidth) - (maxy / 6) + graphoffsety), colour&("bg"), BF 'y-axis
        'connecting dots
        grapha = 0: DO: grapha = grapha + 1
            LINE ((firstchar * fontwidth) + ((grapha - 1) * ((maxx - (firstchar * fontwidth * 2)) / 12)), maxy - (firstchar * fontwidth) - pheight(grapha - 1) + graphoffsety)-((firstchar * fontwidth) + ((grapha) * ((maxx - (firstchar * fontwidth * 2)) / 12)), maxy - (firstchar * fontwidth) - pheight(grapha + 1) + graphoffsety), colour&("red")
            LINE ((firstchar * fontwidth) + (grapha * ((maxx - (firstchar * fontwidth * 2)) / 12)) - 2, maxy - (firstchar * fontwidth) - pheight(grapha + 1) + graphoffsety - 2)-((firstchar * fontwidth) + (grapha * ((maxx - (firstchar * fontwidth * 2)) / 12)) + 2, maxy - (firstchar * fontwidth) - pheight(grapha + 1) + graphoffsety + 2), colour&("red"), BF
        LOOP UNTIL grapha = 11
        COLOR colour&("bg"), colour&("transparent")
        'x-axis labels
        grapha = -1: DO: grapha = grapha + 1
            LOCATE INT((maxy - (firstchar * fontwidth) + graphoffsety) / fontheight) + 2, INT(((firstchar * fontwidth) + (grapha * ((maxx - (firstchar * fontwidth * 2)) / 12))) / fontwidth) + 1
            PRINT MID$(LST$(Ausgabe(max(3) - 11 + grapha).Monat), 5, 2)
        LOOP UNTIL grapha = 11
        LOCATE INT((maxy - (firstchar * fontwidth) + graphoffsety) / fontheight) + 1, INT(((firstchar * fontwidth) + ((grapha + 0.5) * ((maxx - (firstchar * fontwidth * 2)) / 12))) / fontwidth) + 4
        PRINT "Monat"
        'y-axis labels
        LOCATE INT((maxy - (firstchar * fontwidth) + graphoffsety) / fontheight) + 1, firstchar - 2
        PRINT "0"
        LOCATE INT((maxy - (firstchar * fontwidth) - (maxy / 6) + graphoffsety) / fontheight), firstchar
        PRINT "Anzahl"
        LOCATE INT((maxy - (firstchar * fontwidth) - (maxy / 6) + graphoffsety) / fontheight) + 2, firstchar - LEN(STR$(maxpcount)) - 1
        PRINT maxpcount
    END IF
END SUB

SUB RunMenu (selectedm, layout, titel$)
    reprintmenu:
    _DEST canvas&
    IF m > 0 THEN
        Background layout, titel$, 1
        menID$ = endparameter$
        topass$ = menID$
        menID = SUM(topass$)
        endmenu = 0
        IF maximized = 0 THEN
            maxm = m
        ELSE
            maximized = 0
        END IF
        firstprint = 1
        IF maxtemparray(menID) > 0 AND menID <> 0 THEN
            IF menID$ = "edit" OR menID$ = "new" THEN
                m2 = 0: DO: m2 = m2 + 1
                    SELECT CASE type$(m2)
                        CASE "input"
                            IF LEN(temparray$(menID, m2)) > 0 THEN
                                p = 0: DO: p = p + 1
                                    char$(m2, p) = MID$(temparray$(menID, m2), p, 1)
                                LOOP UNTIL p = LEN(temparray$(menID, m2))
                                g(m2) = p
                                gbf(m2) = p
                            END IF
                        CASE "selector"
                            ad = 0: DO: ad = ad + 1
                                IF LEN(temparray$(menID, m2)) < LEN(arraydata$(array(m2), ad)) THEN
                                    p = 0: DO: p = p + 1
                                        IF MID$(arraydata$(array(m2), ad), p, LEN(temparray$(menID, m2))) = temparray$(menID, m2) THEN
                                            selected(m2) = ad: ad = maxad(array(m2))
                                        END IF
                                    LOOP UNTIL p >= LEN(arraydata$(array(m2), ad)) - LEN(temparray$(menID, m2)) + 1
                                ELSEIF temparray$(menID, m2) = arraydata$(array(m2), ad) THEN
                                    selected(m2) = ad: ad = maxad(array(m2)): change(m2) = 1
                                END IF
                            LOOP UNTIL ad = maxad(array(m2))
                        CASE "date"
                            IF LEN(temparray$(menID, m2)) >= 10 THEN
                                'year$ + "-" + month$ + "-" + day$
                                year(m2) = VAL(MID$(temparray$(menID, m2), 1, 4))
                                month(m2) = VAL(MID$(temparray$(menID, m2), 6, 2))
                                day(m2) = VAL(MID$(temparray$(menID, m2), 9, 2))
                            END IF
                        CASE "time"
                            IF LEN(temparray$(menID, m2)) >= 5 THEN
                                hour(m2) = VAL(MID$(temparray$(menID, m2), 1, 2))
                                minute(m2) = VAL(MID$(temparray$(menID, m2), 4, 2))
                            END IF
                    END SELECT
                LOOP UNTIL m2 = maxm OR m2 = maxtemparray(menID)
                maxtemparray(menID) = 0
                clearStatus
                newStatus "Daten aus vorheriger Sitzung eingef" + CHR$(129) + "gt.", "green"
            END IF
        END IF
        IF endparameter$ = "kaz" OR endparameter$ = "ver" THEN RunGraph endparameter$
        DO
            m = 0
            DO
                IF fps = 0 THEN
                    framestart = TIMER
                END IF
                m = m + 1
                prevm = selectedm

                'file drop check
                IF _TOTALDROPPEDFILES = 1 THEN
                    df$ = _DROPPEDFILE
                    IF _FILEEXISTS(df$) THEN
                        droptype$ = "file"
                    ELSE
                        IF _DIREXISTS(a$) THEN
                            droptype$ = "folder"
                        ELSE
                            droptype$ = "empty"
                        END IF
                    END IF
                    _FINISHDROP
                    SELECT CASE droptype$
                        CASE "file"
                            p = 0: DO: p = p + 1
                                IF MID$(df$, p, 1) = "\" THEN
                                    u = p
                                END IF
                            LOOP UNTIL p = LEN(df$)
                            IF MID$(df$, LEN(df$) - 6, 7) = ".bremer" THEN
                                endparameter$ = MID$(df$, u + 1, LEN(df$) - 7 - u)
                                endmenu = 1
                            END IF
                            IF MID$(df$, LEN(df$) - 5, 6) = ".event" THEN
                                'fill temparray with data from file, set the listID to the proper number
                                endmenu = 1
                            END IF
                    END SELECT
                END IF

                'Mouse selection
                mouseinput = _MOUSEINPUT
                mousebutton = _MOUSEBUTTON(1)
                mousex = _MOUSEX
                mousey = _MOUSEY
                IF mouseinput = -1 THEN
                    IF mousebutton = -1 THEN
                        IF mousey >= buttonsly AND mousey <= buttonsuy THEN 'close button
                            IF mousex >= closebuttonlx AND mousex <= closebuttonux THEN
                                logThis "Programm durch X-Button beendet."
                                SYSTEM
                            END IF
                            IF mousex >= minbuttonlx AND mousex <= minbuttonux THEN 'minimize button
                                minimize
                                IF maximized = 1 THEN
                                    change = 1
                                    GOTO reprintmenu
                                END IF
                            END IF
                        END IF
                        IF mousey < topbarheight THEN
                            DO
                                mouseinput = _MOUSEINPUT
                                mousebutton = _MOUSEBUTTON(1)
                            LOOP UNTIL mousebutton <> -1
                            _SCREENMOVE currentposx + (_MOUSEX - mousex), currentposy + (_MOUSEY - mousey)
                            currentposx = currentposx + (_MOUSEX - mousex)
                            currentposy = currentposy + (_MOUSEY - mousey)
                        END IF
                    END IF
                END IF
                IF mouseinput = -1 AND mousetrigger = 0 THEN
                    IF mousex > changevalxl(selectedm) AND mousex < changevalxr(selectedm) THEN
                        IF mousey > increasevalyu(selectedm) AND mousey < increasevalyb(selectedm) AND mousebutton = -1 THEN
                            mousetrigger = 1
                            GOTO increase
                        ELSEIF mousey > decreasevalyu(selectedm) AND mousey < decreasevalyb(selectedm) AND mousebutton = -1 THEN
                            mousetrigger = 1
                            GOTO decrease
                        END IF
                    END IF

                    IF mousex > (firstchsar + xoffset(m) - 1) * fontwidth - (fontwidth / 2) AND mousex < endx(m) THEN
                        IF mousey > basey(m) AND mousey < endy(m) THEN
                            change(selectedm) = 1: selectedm = m: change(selectedm) = 1
                            IF maxad(m) > 0 THEN
                                e = 0: DO
                                    e = e + 1
                                    IF mousex > melementxl(m, e) AND mousex < melementxr(m, e) THEN
                                        selected(m) = e
                                        change(m) = 1
                                    END IF
                                LOOP UNTIL e = maxad(m)
                            END IF
                            IF mousebutton = -1 THEN
                                GOTO engageonm
                            ELSE
                                m = selectedm
                                GOTO commitchange
                            END IF
                        END IF
                    END IF

                    mousewheel = _MOUSEWHEEL
                    IF mousewheel <> 0 THEN
                        IF mousex > melementxl(selectedm, selected(selectedm)) AND mousex < melementxr(selectedm, selected(selectedm)) THEN
                            IF mousewheel = -1 THEN
                                GOTO increase
                            ELSE
                                GOTO decrease
                            END IF
                        END IF
                    END IF
                ELSEIF mousebutton <> -1 THEN
                    mousetrigger = 0
                END IF

                'Key selection
                IF _KEYDOWN(100306) = -1 OR _KEYDOWN(100305) = -1 THEN 'ctrl
                    IF type$(selectedm) = "input" THEN showpassword = 1: change(selectedm) = 1
                    hitk = _KEYHIT
                    IF hitk = -49 OR hitk = -33 THEN selectedm = 1
                    IF (hitk = -50 OR hitk = -34) AND maxm > 1 THEN selectedm = 2
                    IF (hitk = -51 OR hitk = -21) AND maxm > 2 THEN selectedm = 3
                    IF (hitk = -52 OR hitk = -36) AND maxm > 3 THEN selectedm = 4
                    IF (hitk = -53 OR hitk = -37) AND maxm > 4 THEN selectedm = 5
                    IF (hitk = -54 OR hitk = -38) AND maxm > 5 THEN selectedm = 6
                    IF (hitk = -55 OR hitk = -47) AND maxm > 6 THEN selectedm = 7
                    IF (hitk = -56 OR hitk = -40) AND maxm > 7 THEN selectedm = 8
                    IF (hitk = -57 OR hitk = -41) AND maxm > 8 THEN selectedm = 9
                    IF hitk = 13 THEN 'ctrl + enter
                        IF type$(selectedm) = "selector" THEN
                            endparameter$ = destination$(selectedm): endmenu = 1
                        END IF
                    END IF
                    IF hitk = 83 OR hitk = 115 THEN
                        endparameter$ = "save"
                        endmenu = 1
                    END IF
                    IF hitk = 97 OR hitk = 65 THEN 'ctrl + a
                        IF type$(selectedm) = "input" THEN
                            DO: LOOP UNTIL _KEYDOWN(hitk) = 0 'prevents multiple triggers
                            IF allsel(selectedm) = 1 THEN allsel(selectedm) = 0 ELSE allsel(selectedm) = 1
                        END IF
                    END IF
                    IF hitk = 99 OR hitk = 67 THEN 'ctrl + c
                        maxcliparray = 0
                        SELECT CASE type$(selectedm)
                            CASE "text"
                                _CLIPBOARD$ = cutcontent$(text$(m2))
                            CASE "input"
                                UIn$ = ""
                                IF g(selectedm) > 0 THEN
                                    g = 0: DO: g = g + 1
                                        UIn$ = UIn$ + char$(selectedm, g)
                                    LOOP UNTIL g = g(selectedm)
                                END IF
                                _CLIPBOARD$ = UIn$
                        END SELECT
                        clearStatus
                        newStatus "Element Kopiert.", "green"
                    END IF
                    IF hitk = 118 OR hitk = 86 THEN 'ctrl + v
                        paste:
                        IF maxcliparray > 0 THEN
                            m2 = 0: DO: m2 = m2 + 1
                                SELECT CASE type$(m2)
                                    CASE "input"
                                        IF LEN(cliparray$(m2)) > 0 THEN
                                            p = 0: DO: p = p + 1
                                                char$(m2, p) = MID$(cliparray$(m2), p, 1)
                                            LOOP UNTIL p = LEN(cliparray$(m2))
                                            g(m2) = p
                                            gbf(m2) = p
                                        END IF
                                    CASE "selector"
                                        ad = 0: DO: ad = ad + 1
                                            IF LEN(cliparray$(m2)) < LEN(arraydata$(array(m2), ad)) THEN
                                                p = 0: DO: p = p + 1
                                                    IF MID$(arraydata$(array(m2), ad), p, LEN(cliparray$(m2))) = cliparray$(m2) THEN
                                                        selected(m2) = ad: ad = maxad(array(m2))
                                                    END IF
                                                LOOP UNTIL p >= LEN(arraydata$(array(m2), ad)) - LEN(cliparray$(m2)) + 1
                                            ELSEIF cliparray$(m2) = arraydata$(array(m2), ad) THEN
                                                selected(m2) = ad: ad = maxad(array(m2)): change(m2) = 1
                                            END IF
                                        LOOP UNTIL ad = maxad(array(m2))
                                    CASE "date"
                                        IF LEN(cliparray$(m2)) > 10 THEN
                                            year(m2) = VAL(MID$(cliparray$(m2), 1, 4))
                                            month(m2) = VAL(MID$(cliparray$(m2), 6, 2))
                                            day(m2) = VAL(MID$(cliparray$(m2), 9, 2))
                                        END IF
                                    CASE "time"
                                        IF LEN(cliparray$(m2)) >= 5 THEN
                                            hour(m2) = VAL(MID$(cliparray$(m2), 1, 2))
                                            minute(m2) = VAL(MID$(cliparray$(m2), 4, 2))
                                        END IF
                                END SELECT
                                change(m2) = 1
                            LOOP UNTIL m2 = maxm OR m2 = maxcliparray
                            maxcliparray = 0
                            clearStatus
                            newStatus "Eingef" + CHR$(129) + "gt.", "green"
                        END IF
                    END IF
                ELSE
                    IF type$(selectedm) = "input" THEN
                        showpassword = 0: change(selectedm) = 1
                    END IF
                END IF
                IF _KEYDOWN(100308) = -1 OR _KEYDOWN(100307) = -1 THEN 'alt
                    hitk = _KEYHIT
                    IF hitk = 97 OR hitk = 65 THEN 'alt + a
                        IF type$(selectedm) = "input" THEN
                            DO: LOOP UNTIL _KEYDOWN(hitk) = 0 'prevents multiple triggers
                            IF allsel(selectedm) = 1 THEN allsel(selectedm) = 0 ELSE allsel(selectedm) = 1
                        END IF
                    END IF
                    IF hitk = 67 OR hitk = 99 THEN 'alt + c
                        copy:
                        _CLIPBOARD$ = ""
                        m2 = 0: DO: m2 = m2 + 1
                            SELECT CASE type$(m2)
                                CASE "text"
                                    IF LEN(text$(m2)) > 0 THEN
                                        cliparray$(m2) = cutcontent$(text$(m2))
                                        IF LEN(cliparray$(m2)) = 50 THEN
                                            cliparray$(m2) = ""
                                        END IF
                                    ELSE
                                        cliparray$(m2) = ""
                                    END IF
                                CASE "input"
                                    UIn$ = ""
                                    IF g(m2) > 0 THEN
                                        g = 0: DO: g = g + 1
                                            UIn$ = UIn$ + char$(m2, g)
                                        LOOP UNTIL g = g(m2)
                                    END IF
                                    cliparray$(m2) = UIn$
                            END SELECT
                            _CLIPBOARD$ = _CLIPBOARD$ + CHR$(13) + cliparray$(m2)
                        LOOP UNTIL m2 = maxm
                        maxcliparray = maxm
                        clearStatus
                        newStatus "Alles Kopiert.", "green"
                    END IF
                END IF
                IF _KEYDOWN(100304) = -1 OR _KEYDOWN(100303) = -1 THEN 'shift
                    hitk = _KEYHIT
                    IF hitk = 9 THEN 'shift + tab
                        IF selectedm > 1 THEN selectedm = selectedm - 1 ELSE selectedm = maxm
                        shiftlog = 1
                    END IF
                END IF
                IF _KEYHIT = 15360 THEN
                    _CLIPBOARDIMAGE = canvas&
                    newStatus "Screenshot kopiert.", "green"
                END IF
                Taste$ = INKEY$
                IF Taste$ <> "" THEN
                    SELECT CASE Taste$
                        CASE CHR$(0) + CHR$(80) 'arrow down
                            decrease:
                            IF type$(selectedm) = "date" THEN
                                SELECT CASE selected(selectedm)
                                    CASE 1
                                        IF year(selectedm) > VAL(MID$(DATE$, 7, 4)) THEN
                                            year(selectedm) = year(selectedm) - 1
                                            IF day(selectedm) = maxday(year(selectedm) + 1, month(selectedm)) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        ELSE
                                            year(selectedm) = VAL(MID$(DATE$, 7, 4)) + 5
                                            IF day(selectedm) = maxday(VAL(MID$(DATE$, 7, 4)), month(selectedm)) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                    CASE 2
                                        IF month(selectedm) > 1 THEN
                                            month(selectedm) = month(selectedm) - 1
                                            IF day(selectedm) = maxday(year(selectedm), month(selectedm) + 1) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        ELSE
                                            month(selectedm) = 12
                                            IF day(selectedm) = maxday(year(selectedm), 1) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                    CASE 3
                                        IF day(selectedm) > 1 THEN
                                            day(selectedm) = day(selectedm) - 1
                                        ELSE
                                            day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                END SELECT
                                change(selectedm) = 1
                            ELSEIF type$(selectedm) = "time" THEN
                                SELECT CASE selected(selectedm)
                                    CASE 1
                                        IF hour(selectedm) > 0 THEN hour(selectedm) = hour(selectedm) - 1 ELSE hour(selectedm) = 23
                                    CASE 2
                                        IF minute(selectedm) > 0 THEN minute(selectedm) = minute(selectedm) - 5 ELSE minute(selectedm) = 55
                                END SELECT
                                change(selectedm) = 1
                            ELSE
                                IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                            END IF
                        CASE CHR$(0) + CHR$(72) 'arrow up
                            increase:
                            IF type$(selectedm) = "date" THEN
                                SELECT CASE selected(selectedm)
                                    CASE 1
                                        IF year(selectedm) < VAL(MID$(DATE$, 7, 4)) + 10 THEN
                                            year(selectedm) = year(selectedm) + 1
                                            IF day(selectedm) = maxday(year(selectedm) - 1, month(selectedm)) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        ELSE
                                            year(selectedm) = VAL(MID$(DATE$, 7, 4))
                                            IF day(selectedm) = maxday(VAL(MID$(DATE$, 7, 4)) + 10, month(selectedm)) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                    CASE 2
                                        IF month(selectedm) < 12 THEN
                                            month(selectedm) = month(selectedm) + 1
                                            IF day(selectedm) = maxday(year(selectedm), month(selectedm) - 1) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        ELSE
                                            month(selectedm) = 1
                                            IF day(selectedm) = maxday(year(selectedm), 12) THEN day(selectedm) = maxday(year(selectedm), month(selectedm))
                                        END IF
                                    CASE 3
                                        IF day(selectedm) < maxday(year(selectedm), month(selectedm)) THEN
                                            day(selectedm) = day(selectedm) + 1
                                        ELSE
                                            day(selectedm) = 1
                                        END IF
                                END SELECT
                                change(selectedm) = 1
                            ELSEIF type$(selectedm) = "time" THEN
                                SELECT CASE selected(selectedm)
                                    CASE 1
                                        IF hour(selectedm) < 23 THEN hour(selectedm) = hour(selectedm) + 1 ELSE hour(selectedm) = 0
                                    CASE 2
                                        IF minute(selectedm) < 54 THEN minute(selectedm) = minute(selectedm) + 5 ELSE minute(selectedm) = 0
                                END SELECT
                                change(selectedm) = 1
                            ELSE
                                IF selectedm > 1 THEN selectedm = selectedm - 1 ELSE selectedm = maxm
                            END IF
                        CASE CHR$(0) + CHR$(77) 'arrow right
                            IF type$(selectedm) = "selector" THEN
                                IF selected(selectedm) < maxad(array(selectedm)) THEN
                                    selected(selectedm) = selected(selectedm) + 1
                                    change(selectedm) = 1
                                    change(selectedm - 1) = 1
                                ELSE
                                    IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                END IF
                            ELSEIF type$(selectedm) = "date" THEN
                                IF selected(selectedm) < 3 THEN
                                    selected(selectedm) = selected(selectedm) + 1
                                    change(selectedm) = 1
                                    change(selectedm - 1) = 1
                                ELSE
                                    IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                END IF
                            ELSEIF type$(selectedm) = "time" THEN
                                IF selected(selectedm) < 2 THEN
                                    selected(selectedm) = selected(selectedm) + 1
                                    change(selectedm) = 1
                                    change(selectedm - 1) = 1
                                ELSE
                                    IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                END IF
                            ELSE
                                IF selectedm < maxm THEN selectedm = selectedm + 1: change(selectedm - 1) = 1 ELSE selectedm = 1
                                change(1) = 1
                            END IF
                        CASE CHR$(0) + CHR$(75) 'arrow left
                            IF type$(selectedm) = "selector" OR type$(selectedm) = "date" OR type$(selectedm) = "time" THEN
                                IF selected(selectedm) > 1 THEN
                                    selected(selectedm) = selected(selectedm) - 1
                                    change(selectedm) = 1
                                ELSE
                                    IF selectedm > 1 THEN selectedm = selectedm - 1 ELSE selectedm = maxm
                                END IF
                            ELSE
                                IF selectedm > 1 THEN selectedm = selectedm - 1 ELSE selectedm = maxm
                            END IF
                        CASE CHR$(9)
                            IF shiftlog = 0 THEN
                                IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                            ELSE
                                shiftlog = 0
                            END IF
                        CASE CHR$(13)
                            IF type$(selectedm) = "input" AND selectedm < maxm AND type$(selectedm + 1) = "input" THEN
                                LOCATE firstline + yoffset(selectedm) + overflow(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + gbf(selectedm) - (overflowlimit(selectedm) * overflow(selectedm)) + 2
                                PRINT "   "
                                selectedm = selectedm + 1
                                'GOTO commitchange
                            ELSEIF type$(selectedm) = "input" AND selectedm = inputcount THEN
                                LOCATE firstline + yoffset(selectedm) + overflow(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + gbf(selectedm) - (overflowlimit(selectedm) * overflow(selectedm)) + 2
                                PRINT "   "
                                selectedm = selectedm + 1
                                GOTO commitchange
                            END IF

                            engageonm: 'wird getriggert, wenn auf m geklickt wird oder enter gedruckt wird
                            SELECT CASE type$(selectedm)
                                CASE "slider"
                                    IF _MOUSEBUTTON(1) = -1 THEN
                                        DO
                                            IF _MOUSEINPUT = -1 AND _MOUSEX > basex(selectedm) AND _MOUSEX < endx(selectedm) THEN
                                                value(m) = _MOUSEX - (basex(selectedm))
                                                value(m) = INT(value(selectedm) / (maxx / 4) * 100)
                                                printslider:
                                                'cleanup
                                                LINE (basex(selectedm), (firstline + yoffset(selectedm) - 1) * fontheight + 1)-(endx(selectedm), (firstline + yoffset(selectedm)) * fontheight - 1), colour&("black"), BF

                                                'horizontal
                                                LINE (basex(selectedm), (firstline + yoffset(selectedm) - 1) * fontheight + (fontheight / 2))-(endx(selectedm), (firstline + yoffset(selectedm) - 1) * fontheight + (fontheight / 2) + 1), colour&("white"), BF
                                                'vertical @ value
                                                LINE (basex(selectedm) + (value(selectedm) / maxval(selectedm) * (maxx / 4)), (firstline + yoffset(selectedm) - 1) * fontheight + 1)-(basex(selectedm) + 4 + (value(selectedm) / maxval(selectedm) * (maxx / 4)), (firstline + yoffset(selectedm)) * fontheight - 1), colour&("white"), BF
                                                xvalue = firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + ((maxx / 4) / fontwidth) + 2
                                                LOCATE firstline + yoffset(selectedm), xvalue
                                                COLOR colour&("fg"), colour&("bg")
                                                PRINT value(selectedm); "   "
                                            ELSEIF _MOUSEINPUT = -1 AND _MOUSEX < basex(selectedm) OR _MOUSEX > basex(selectedm) + 4 + (maxx / 4) THEN
                                                IF _MOUSEX < basex(selectedm) THEN
                                                    value(selectedm) = minval(selectedm)
                                                ELSE
                                                    value(selectedm) = maxval(selectedm)
                                                END IF
                                                GOTO printslider
                                            END IF
                                        LOOP UNTIL _MOUSEBUTTON(1) = 0
                                    END IF
                                CASE "selector"
                                    IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                CASE "date"
                                    IF selected(selectedm) < maxad(array(selectedm)) THEN
                                        selected(selectedm) = selected(selectedm) + 1: change(selectedm) = 1
                                    ELSE
                                        IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                    END IF
                                CASE "time"
                                    IF selected(selectedm) < maxad(array(selectedm)) THEN
                                        selected(selectedm) = selected(selectedm) + 1: change(selectedm) = 1
                                    ELSE
                                        IF selectedm < maxm THEN selectedm = selectedm + 1 ELSE selectedm = 1
                                    END IF
                                CASE "menu"
                                    IF destination$(selectedm) = "system" THEN
                                        SYSTEM
                                    ELSE
                                        SELECT CASE destination$(selectedm)
                                            CASE "copy": change(selectedm) = 1: GOTO copy
                                            CASE "paste": change(selectedm) = 1: GOTO paste
                                        END SELECT
                                        IF destination$(selectedm) <> "" THEN
                                            IF destination$(selectedm) = "delete" THEN
                                                destination$(selectedm) = "confirm"
                                                text$(selectedm) = "Sicher?"
                                                change(selectedm) = 1
                                            ELSEIF destination$(selectedm) = "confirm" THEN
                                                destination$(selectedm) = "delete"
                                                endparameter$ = destination$(selectedm): endmenu = 1
                                            ELSE
                                                endparameter$ = destination$(selectedm): endmenu = 1
                                            END IF
                                        END IF
                                    END IF
                                CASE "toggle"
                                    endmenu = 0
                                    IF state(selectedm) = 1 THEN state(selectedm) = 0 ELSE state(selectedm) = 1
                                    SELECT CASE setting$(selectedm)
                                        CASE "darkmode"
                                            IF darkmode = 1 THEN darkmode = 0 ELSE darkmode = 1
                                        CASE "size"
                                            IF bigwindow = 1 THEN bigwindow = 0 ELSE bigwindow = 1
                                        CASE "searchbottom"
                                            IF searchbottomfixed = 1 THEN searchbottomfixed = 0 ELSE searchbottomfixed = 1
                                        CASE "transparency"
                                            IF transparency = 1 THEN transparency = 0 ELSE transparency = 1
                                    END SELECT: change(selectedm) = 1
                            END SELECT
                        CASE CHR$(27)
                            endmenu = 1: endparameter$ = "up"
                    END SELECT
                END IF
                commitchange:
                IF selectedm <> prevm THEN
                    change(selectedm) = 1: change(prevm) = 1 ': Background layout, titel$, 0
                END IF
                'if change is there, print the menu again
                IF change = 1 OR firstprint = 1 OR change(m) = 1 THEN
                    IF change(m) = 1 THEN
                        LINE (basex(m), basey(m))-(endx(m), endy(m)), colour&("bg"), BF
                    END IF
                    IF runm = m + 1 THEN
                        change = 0
                    END IF
                    SELECT CASE type$(m)
                        CASE "slider"
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            COLOR colour&("fg"), colour&("bg")
                            PRINT text$(m)
                            'horizontal
                            IF selectedm = m THEN
                                LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2))-(basex(m) + (maxx / 4), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2) + 1), colour&("fg"), BF
                            ELSE
                                LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2))-(basex(m) + (maxx / 4), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2) + 1), colour&("offfocus"), BF
                            END IF
                            'vertical
                            rectangle basex(m) + (value(m) / maxval(m) * (maxx / 4)), (firstline + yoffset(m) - 1) * fontheight + 1, basex(m) + (UMround * 2) + (value(m) / maxval(m) * (maxx / 4)), (firstline + yoffset(m)) * fontheight - 1, UMround, colour&("red"), "BF"
                            xvalue = firstchar + xoffset(m) + LEN(text$(m)) + ((maxx / 4) / fontwidth) + 2
                            LOCATE firstline + yoffset(m), xvalue
                            PRINT value(m); "   "
                        CASE "date"
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            COLOR colour&("fg"), colour&("transparent")
                            PRINT text$(m) + " ";
                            'counting elements
                            melementxl(m, 1) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)))
                            melementxr(m, 1) = fontwidth * (firstchar + xoffset(m) + LEN((text$(m)) + convyear$(year(m))))
                            melementxl(m, 2) = melementxr(m, 1) + fontwidth
                            melementxr(m, 2) = melementxl(m, 2) + (fontwidth * LEN(convmonth$(month(m))))
                            melementxl(m, 3) = melementxr(m, 2) + fontwidth
                            melementxr(m, 3) = melementxl(m, 3) + (fontwidth * LEN(convday$(day(m))))
                            LINE (melementxl(m, 1) - (fontwidth / 2), increasevalyu(m) - 4)-(melementxr(m, 3) + (fontwidth / 2), decreasevalyb(m) + 2), colour&("bg"), BF
                            IF selectedm = m THEN
                                COLOR colour&("fg"), colour&("transparent")
                                LINE (melementxl(m, selected(m)) - (fontwidth / 2), increasevalyu(m) - 4)-(melementxr(m, selected(m)) + (fontwidth / 2), decreasevalyb(m) + 2), colour&("red"), BF
                            ELSE
                                COLOR colour&("offfocus"), colour&("transparent")
                            END IF
                            PRINT convyear$(year(m)) + " "; convmonth$(month(m)) + " "; convday$(day(m)) + "   "
                            'triangles
                            drawTriangle changevalxl(m), increasevalyu(m), triscale, -1
                            drawTriangle changevalxl(m), decreasevalyu(m), triscale, 1
                        CASE "time"
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            COLOR colour&("fg"), colour&("transparent")
                            PRINT text$(m) + " ";
                            'counting elements
                            melementxl(m, 1) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)))
                            melementxr(m, 1) = fontwidth * (firstchar + xoffset(m) + LEN((text$(m)) + convhour$(hour(m))))
                            melementxl(m, 2) = melementxr(m, 1) + fontwidth
                            melementxr(m, 2) = melementxl(m, 2) + (fontwidth * LEN(convminute$(minute(m))))
                            LINE (melementxl(m, 1) - (fontwidth / 2), increasevalyu(m) - 4)-(melementxr(m, 2) + (fontwidth / 2), decreasevalyb(m) + 2), colour&("bg"), BF
                            IF selectedm = m THEN
                                COLOR colour&("fg"), colour&("transparent")
                                IF selectedm = m THEN LINE (melementxl(m, selected(m)) - (fontwidth / 2), increasevalyu(m) - 4)-(melementxr(m, selected(m)) + (fontwidth / 2), decreasevalyb(m) + 2), colour&("red"), BF
                            ELSE
                                COLOR colour&("offfocus"), colour&("transparent")
                            END IF
                            PRINT convhour$(hour(m)); ":"; convminute$(minute(m)); " Uhr   "
                            'triangles
                            drawTriangle changevalxl(m), increasevalyu(m), triscale, -1
                            drawTriangle changevalxl(m), decreasevalyu(m), triscale, 1
                        CASE "selector"
                            'zeigt nicht editierbaren text an
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            COLOR colour&("fg"), colour&("bg")
                            PRINT text$(m);
                            IF selectedm <> m THEN
                                COLOR colour&("offfocus"), colour&("bg")
                            ELSE
                                COLOR colour&("red"), colour&("bg")
                            END IF
                            PRINT " < "; _TRIM$(arraydata$(array(m), selected(m))); " >  ";
                            COLOR colour&("offfocus")
                            suche$ = ""
                            IF g(m) > 0 AND m = selectedm THEN
                                gbf(m) = 0: DO: gbf(m) = gbf(m) + 1: suche$ = suche$ + char$(m, gbf(m)): LOOP UNTIL gbf(m) = g(m)
                            END IF
                            PRINT "-" + suche$ + SPC(100 - LEN(arraydata$(array(m), selected(m))) - LEN(suche$))
                            rectangle (firstchar + xoffset(m) + LEN(text$(m)) - 0.5) * fontwidth, (firstline + yoffset(m) - 1) * fontheight - 6, (firstchar + xoffset(m) + LEN(arraydata$(array(m), selected(m))) + LEN(text$(m)) + 5) * fontwidth, (firstline + yoffset(m)) * fontheight + 4, UMround, colour&("fg"), "B"
                            endx(m) = basex(m) + (LEN(text$(m)) + LEN(arraydata$(array(m), selected(m))) + 11) * fontwidth + (fontwidth / 2)
                        CASE "toggle"
                            LINE (basex(m), basey(m))-(endx(m), endy(m)), colour&("bg"), BF
                            IF selectedm = m THEN
                                rectangle basex(m), basey(m), endx(m), endy(m), UMround, colour&("red"), "B"
                                COLOR colour&("red"), colour&("bg")
                            ELSE
                                rectangle basex(m), basey(m), endx(m), endy(m), UMround, colour&("offfocus"), "B"
                                COLOR colour&("fg"), colour&("bg")
                            END IF
                            IF state(m) = 0 THEN
                                IF selectedm = m THEN
                                    rectangle basex(m) + 2, basey(m) + 2, endx(m) - (endx(m) - basex(m)) / 2 - 2, endy(m) - 2, UMround, colour&("fg"), "BF"
                                ELSE
                                    rectangle basex(m) + 2, basey(m) + 2, endx(m) - (endx(m) - basex(m)) / 2 - 2, endy(m) - 2, UMround, colour&("offfocus"), "BF"
                                END IF
                            ELSE
                                rectangle basex(m) + (endx(m) - basex(m)) / 2 + 2, basey(m) + 2, endx(m) - 2, endy(m) - 2, UMround, colour&("red"), "BF"
                            END IF
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            PRINT text$(m)
                        CASE "input"
                            'zeigt nicht editierbaren text an
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            IF selectedm <> m THEN
                                COLOR colour&("fg"), colour&("transparent")
                            ELSE
                                COLOR colour&("red"), colour&("transparent")
                            END IF
                            PRINT text$(m)

                            'zeigt editierbaren text an
                            IF allsel(selectedm) = 0 THEN
                                IF selectedm = m THEN
                                    COLOR colour&("red"), colour&("bg")
                                ELSE
                                    COLOR colour&("offfocus"), colour&("bg")
                                END IF
                            ELSE
                                IF selectedm = m THEN
                                    IF darkmode = 1 THEN COLOR colour&("red"), colour&("dark grey"): ELSE COLOR colour&("red"), colour&("light grey")
                                ELSE
                                    IF darkmode = 1 THEN COLOR colour&("white"), colour&("dark grey"): ELSE COLOR colour&("black"), colour&("light grey")
                                END IF
                            END IF
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m) + LEN(text$(m)) + 2
                            IF g(m) > 0 THEN
                                gbf(m) = 0
                                UserInput$(m) = ""
                                DO
                                    gbf(m) = gbf(m) + 1
                                    o = 0: DO: o = o + 1
                                        IF gbf(m) = overflowlimit(m) * o THEN
                                            LINE ((firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2), (firstline + yoffset(m) + overflow(m)) * fontheight + 1)-(maxx, (firstline + yoffset(m) + overflow(m)) * fontheight + 4), colour&("bg"), BF
                                            overflow(m) = o
                                            LOCATE firstline + yoffset(m) + overflow(m), firstchar + xoffset(m) + LEN(text$(m)) + 2
                                        END IF
                                    LOOP UNTIL o = 10
                                    IF MID$(text$(m), 1, 8) = "Passwort" AND showpassword = 0 THEN
                                        PRINT "*";
                                    ELSE
                                        PRINT char$(m, gbf(m));
                                    END IF
                                LOOP UNTIL gbf(m) = g(m)
                            END IF
                            PRINT " "
                            endx(m) = basex(m) + (LEN(text$(m)) + g(m) - (overflowlimit(m) * overflow(m)) + 2.5) * fontwidth + (fontwidth / 2)
                        CASE "menu"
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            IF m <> selectedm THEN
                                rectangle basex(m), basey(m), endx(m), endy(m), UMround, colour&("fg"), "B"
                                COLOR colour&("fg"), colour&("transparent")
                            ELSE
                                rectangle basex(m), basey(m), endx(m), endy(m), UMround, colour&("red"), "BF"
                                COLOR colour&("white"), colour&("transparent")
                            END IF
                            PRINT text$(m)
                        CASE "text"
                            SELECT CASE weight$(m)
                                CASE "r"
                                    _FONT r&
                                CASE "b"
                                    _FONT b&
                            END SELECT
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            IF selectedm = m THEN
                                COLOR colour&("fg"), colour&("red")
                            ELSE
                                COLOR clr&(m), colour&("transparent")
                            END IF
                            PRINT text$(m)
                            _FONT r&
                    END SELECT
                    IF type$(m) = "selector" OR type$(m) = "date" OR type$(m) = "time" THEN 'thick line below elements
                        LINE ((firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2), (firstline + yoffset(m)) * fontheight + 1)-(maxx, (firstline + yoffset(m)) * fontheight + 4), colour&("bg"), BF
                        LINE ((firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2), (firstline + yoffset(m)) * fontheight + 1)-(endx(m) + (fontwidth / 2), (firstline + yoffset(m)) * fontheight + 4), colour&("fg"), BF
                    ELSEIF type$(m) = "input" THEN
                        LINE ((firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2), (firstline + yoffset(m) + overflow(m)) * fontheight + 1)-(maxx, (firstline + yoffset(m) + overflow(m)) * fontheight + 4), colour&("bg"), BF
                        LINE ((firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2), (firstline + yoffset(m) + overflow(m)) * fontheight + 1)-(endx(m) + (fontwidth / 2), (firstline + yoffset(m) + overflow(m)) * fontheight + 4), colour&("fg"), BF
                    END IF
                END IF
                change(m) = 0
                IF type$(selectedm) = "input" THEN
                    IF Taste$ <> "" THEN
                        IF Taste$ = CHR$(8) AND g(selectedm) > 0 AND allsel(selectedm) = 0 THEN
                            LINE ((firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2), (firstline + yoffset(m) + overflow(m)) * fontheight + 1)-(maxx, (firstline + yoffset(m) + overflow(m)) * fontheight + 4), colour&("bg"), BF
                            g(selectedm) = g(selectedm) - 1: char$(selectedm, g(selectedm) + 1) = ""
                            IF g(selectedm) < overflowlimit(selectedm) * overflow(selectedm) AND overflow(selectedm) > 0 THEN
                                LINE ((firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2), (firstline + yoffset(m) + overflow(m)) * fontheight + 1)-(maxx, (firstline + yoffset(m) + overflow(m)) * fontheight + 4), colour&("bg"), BF
                                overflow(selectedm) = overflow(selectedm) - 1
                            END IF
                            'reduce overflow here
                        ELSEIF Taste$ = CHR$(8) AND g(selectedm) > 0 AND allsel(selectedm) = 1 THEN
                            LINE ((firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2), (firstline + yoffset(m) + overflow(m)) * fontheight + 1)-(maxx, (firstline + yoffset(m) + overflow(m)) * fontheight + 4), colour&("bg"), BF
                            g(selectedm) = 0: DO: g(selectedm) = g(selectedm) + 1: char$(selectedm, g(selectedm)) = "": LOOP UNTIL g(selectedm) = gbf(selectedm): g(selectedm) = 0: PRINT " "
                            allsel(selectedm) = 0
                            overflow = -1: DO: overflow = overflow + 1
                                LOCATE firstline + yoffset(selectedm) + overflow, firstchar + xoffset(selectedm) + LEN(text$(selectedm))
                                DO: g(selectedm) = g(selectedm) + 1
                                    PRINT " ";
                                LOOP UNTIL g(selectedm) = gbf(selectedm) + 3
                            LOOP UNTIL overflow = overflow(selectedm)
                            g(selectedm) = 0
                        END IF
                        ac = 0
                        DO
                            ac = ac + 1
                            IF number(selectedm) = 1 THEN
                                IF ac$(ac) = Taste$ AND ((ASC(Taste$) > 47 AND ASC(Taste$) < 58) OR Taste$ = "/" OR Taste$ = " " OR Taste$ = "." OR Taste$ = ",") THEN
                                    g(selectedm) = g(selectedm) + 1
                                    char$(selectedm, g(selectedm)) = Taste$
                                END IF
                            ELSE
                                IF ac$(ac) = Taste$ THEN
                                    g(selectedm) = g(selectedm) + 1
                                    char$(selectedm, g(selectedm)) = Taste$
                                END IF
                            END IF
                        LOOP UNTIL ac = alch
                        IF replace$(Taste$) <> "" THEN
                            g(selectedm) = g(selectedm) + 1
                            char$(selectedm, g(selectedm)) = replace$(Taste$)
                        END IF
                        Taste$ = ""
                    END IF
                    IF gbf(selectedm) <> g(selectedm) THEN
                        LOCATE firstline + yoffset(selectedm) + overflow(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + gbf(selectedm) - (overflowlimit(selectedm) * overflow(selectedm)) + 2
                        PRINT "   "
                        change(selectedm) = 1
                        gbf(selectedm) = g(selectedm)
                    END IF
                    IF TIMER MOD 2 = 0 AND g(selectedm) = gbf(selectedm) THEN
                        LOCATE firstline + yoffset(selectedm) + overflow(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + gbf(selectedm) - (overflowlimit(selectedm) * overflow(selectedm)) + 2
                        COLOR colour&("red"), colour&("bg")
                        PRINT "|  "
                    ELSE
                        LOCATE firstline + yoffset(selectedm) + overflow(selectedm), firstchar + xoffset(selectedm) + LEN(text$(selectedm)) + gbf(selectedm) - (overflowlimit(selectedm) * overflow(selectedm)) + 2
                        COLOR colour&("bg"), colour&("bg")
                        PRINT "   "
                    END IF
                    IF change(m) = 1 AND selectedm <> m THEN
                        LOCATE firstline + yoffset(m) + overflow(m), firstchar + xoffset(m) + LEN(text$(m)) + g(m) - (overflowlimit(m) * overflow(m)) + 2
                        PRINT "   "
                    END IF
                ELSEIF type$(selectedm) = "selector" THEN
                    IF Taste$ <> "" THEN
                        IF Taste$ = CHR$(8) AND g(selectedm) > 0 THEN
                            g(selectedm) = g(selectedm) - 1: char$(selectedm, g(selectedm) + 1) = "": change(selectedm) = 1
                        END IF
                        ac = 0
                        DO
                            ac = ac + 1
                            IF ac$(ac) = Taste$ THEN
                                g(selectedm) = g(selectedm) + 1
                                char$(selectedm, g(selectedm)) = Taste$
                            END IF
                        LOOP UNTIL ac = alch
                        IF replace$(Taste$) <> "" THEN
                            g(selectedm) = g(selectedm) + 1
                            char$(selectedm, g(selectedm)) = replace$(Taste$)
                        END IF
                        IF gbf(selectedm) <> g(selectedm) AND g(selectedm) > 0 THEN
                            change(selectedm) = 1
                            suche$ = ""
                            gbf(selectedm) = 0: DO: gbf(selectedm) = gbf(selectedm) + 1: suche$ = suche$ + char$(selectedm, gbf(selectedm)): LOOP UNTIL gbf(selectedm) = g(selectedm)
                            ad = 0: DO: ad = ad + 1
                                IF LEN(suche$) < LEN(arraydata$(array(selectedm), ad)) THEN
                                    p = 0: DO: p = p + 1
                                        IF UCASE$(MID$(arraydata$(array(selectedm), ad), p, LEN(suche$))) = UCASE$(suche$) THEN
                                            selected(selectedm) = ad: ad = maxad(array(selectedm))
                                        END IF
                                    LOOP UNTIL p >= LEN(arraydata$(array(selectedm), ad)) - LEN(suche$) + 1
                                ELSEIF UCASE$(suche$) = UCASE$(arraydata$(array(selectedm), ad)) THEN
                                    selected(selectedm) = ad: ad = maxad(array(selectedm)): change(selectedm) = 1
                                END IF
                            LOOP UNTIL ad = maxad(array(selectedm))
                        ELSEIF g(selectedm) = 0 THEN
                            gbf(selectedm) = 0
                        END IF
                    END IF
                END IF
            LOOP UNTIL m = maxm
            firstprint = 0
            frameend = TIMER
            frametime = frameend - framestart
            fps = fps + 1
            IF frametime >= 1 THEN
                LOCATE maxlines - 1, maxrows - 4
                COLOR colour&("fg"), colour&("bg")
                PRINT fps
                IF fps < (framerate * 2) THEN 'removes rounding in case the framerate drops below half, potentially saves some time
                    UMround = 4
                    IF st > 0 THEN printStatus
                ELSE
                    UMround = 0
                END IF
                fps = 0
            END IF
            _DEST 0
            _PUTIMAGE (0, 0), canvas&
            _DISPLAY
            _DEST canvas&
            _LIMIT framerate
        LOOP UNTIL endmenu = 1 OR interactable = 0
        m = 0: DO: m = m + 1
            SELECT CASE type$(m)
                CASE "input"
                    UserInput$(m) = ""
                    g(m) = 0
                    IF gbf(m) > 0 THEN
                        DO
                            g(m) = g(m) + 1
                            UserInput$(m) = UserInput$(m) + char$(m, g(m))
                            char$(m, g(m)) = ""
                        LOOP UNTIL g(m) = gbf(m)
                    END IF
                CASE "selector"
                    UserInput$(m) = arraydata$(array(m), selected(m))
                    IF g(m) > 0 THEN
                        g = 0: DO: g = g + 1: char$(m, g) = "": LOOP UNTIL g = g(m)
                    END IF
                    g(m) = 0
                    gbf(m) = 0
                CASE "date"
                    year$ = convyear$(year(m))
                    month$ = convmonth$(month(m))
                    day$ = convday$(day(m))
                    UserInput$(m) = year$ + "-" + month$ + "-" + day$
                CASE "time"
                    hour$ = convhour$(hour(m))
                    minute$ = convminute$(minute(m))
                    UserInput$(m) = hour$ + ":" + minute$
            END SELECT
        LOOP UNTIL m = maxm

        'tempsave
        IF endparameter$ <> "save" AND menID$ <> "" THEN
            m2 = 0: DO: m2 = m2 + 1
                temparray$(menID, m2) = UserInput$(m2)
                IF LEN(temparray$(menID, m2)) = 50 THEN
                    temparray$(menID, m2) = ""
                END IF
            LOOP UNTIL m2 = maxm
            maxtemparray(menID) = maxm
        END IF
    END IF
    _AUTODISPLAY
    _DEST 0
    clearStatus
    EmptyMenu
END SUB

FUNCTION SUM (tosum$)
    SELECT CASE tosum$
        CASE "new"
            tosum$ = tosum$ + endparameterbfbf$
    END SELECT
    su = 0
    IF LEN(tosum$) > 0 THEN
        p = 0: DO: p = p + 1
            su = su + ASC(MID$(tosum$, p, 1))
        LOOP UNTIL p = LEN(tosum$)
    END IF
    SUM = su
END FUNCTION

FUNCTION replace$ (chor$)
    SELECT CASE chor$
        CASE CHR$(228) 'ae
            replace$ = CHR$(132)
        CASE CHR$(196) 'AE
            replace$ = CHR$(142)
        CASE CHR$(252) 'ue
            replace$ = CHR$(129)
        CASE CHR$(220) 'UE
            replace$ = CHR$(154)
        CASE CHR$(246) 'oe
            replace$ = CHR$(148)
        CASE CHR$(214) 'OE
            replace$ = CHR$(153)
        CASE CHR$(223) 'ss
            replace$ = CHR$(225)
    END SELECT
END FUNCTION

SUB printStatus
    maxst = st
    _FONT b&
    st = 0: DO: st = st + 1
        COLOR colour&(color$(st)), colour&("bg")
        LOCATE firstline + st, maxrows - LEN(sttext$(st)) - 10
        PRINT SPC(5) + sttext$(st)
    LOOP UNTIL st = maxst
    _FONT r&
END SUB

SUB newStatus (sttext$, color$)
    st = st + 1
    sttext$(st) = sttext$
    color$(st) = color$
END SUB

SUB clearStatus
    st = 0: maxst = 0
END SUB

FUNCTION convyear$ (year)
    IF year > 999 THEN
        convyear$ = LST$(year)
    ELSEIF year > 99 THEN
        convyear$ = "0" + LST$(year)
    ELSEIF year(m) > 9 THEN
        convyear$ = "00" + LST$(year)
    ELSEIF year > 0 THEN
        convyear$ = "000" + LST$(year)
    ELSE: convyear$ = "0000": END IF
END FUNCTION

FUNCTION convmonth$ (month)
    IF month > 9 THEN
        convmonth$ = LST$(month)
    ELSEIF month > 0 THEN
        convmonth$ = "0" + LST$(month)
    ELSE: convmonth$ = "00": END IF
END FUNCTION

FUNCTION convday$ (day)
    IF day > 9 THEN
        convday$ = LST$(day)
    ELSEIF day > 0 THEN
        convday$ = "0" + LST$(day)
    ELSE: convday$ = "00": END IF
END FUNCTION

FUNCTION convhour$ (hour)
    IF hour > 9 THEN
        convhour$ = LST$(hour)
    ELSEIF hour > 0 THEN
        convhour$ = "0" + LST$(hour)
    ELSE: convhour$ = "00": END IF
END FUNCTION

FUNCTION convminute$ (minute)
    IF minute > 9 THEN
        convminute$ = LST$(minute)
    ELSEIF minute > 0 THEN
        convminute$ = "0" + LST$(minute)
    ELSE: convminute$ = "00": END IF
END FUNCTION

SUB NewSlider (yoffset, xoffset, minval, maxval, text$, destination$, standard)
    m = m + 1
    interactable = 1
    type$(m) = "slider"
    yoffset(m) = yoffset * 2 - 1
    minval(m) = minval
    maxval(m) = maxval
    xoffset(m) = xoffset
    text$(m) = text$
    basex(m) = (firstchar + xoffset(m) + LEN(text$(m))) * fontwidth + (fontwidth / 2) 'change this to fit your setup
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 10) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
    value(m) = standard
    endx(m) = basex(m) + (maxx / 4) + 4
END SUB

SUB NewTime (yoffset, xoffset, text$, standard$)
    m = m + 1
    type$(m) = "time"
    selected(m) = 1
    hour(m) = VAL(MID$(standard$, 1, 2))
    minute(m) = VAL(MID$(standard$, 4, 2))
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    interactable = 1
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 12) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
    changevalxl(m) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)) + 10)
    changevalxr(m) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)) + 10 + triscale)
    increasevalyu(m) = fontheight * (firstline + yoffset(m) - 1)
    increasevalyb(m) = increasevalyu(m) + ((fontwidth * triscale) * 2 / 3)
    decreasevalyu(m) = fontheight * (firstline + yoffset(m) - 0.4)
    decreasevalyb(m) = decreasevalyu(m) + ((fontwidth * triscale) * 2 / 3)
END SUB

SUB NewDate (yoffset, xoffset, text$, standard$)
    m = m + 1
    type$(m) = "date"
    selected(m) = 1
    g(m) = 0: gbf(m) = 0
    month(m) = VAL(MID$(standard$, 1, 2))
    day(m) = VAL(MID$(standard$, 4, 2))
    year(m) = VAL(MID$(standard$, 7, 4))
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    interactable = 1
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 14) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
    changevalxl(m) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)) + 11)
    changevalxr(m) = fontwidth * (firstchar + xoffset(m) + LEN(text$(m)) + 11 + triscale)
    increasevalyu(m) = fontheight * (firstline + yoffset(m) - 1)
    increasevalyb(m) = increasevalyu(m) + ((fontwidth * triscale) * 2 / 3)
    decreasevalyu(m) = fontheight * (firstline + yoffset(m) - 0.4)
    decreasevalyb(m) = decreasevalyu(m) + ((fontwidth * triscale) * 2 / 3)
END SUB

SUB NewSelector (yoffset, xoffset, text$, array, destination$, standard)
    m = m + 1
    type$(m) = "selector"
    destination$(m) = destination$
    array(m) = array
    selected(m) = standard
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    interactable = 1
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 1) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
END SUB

SUB NewToggle (yoffset, xoffset, text$, setting$)
    m = m + 1
    type$(m) = "toggle"
    setting$(m) = setting$
    SELECT CASE setting$(m)
        CASE "darkmode": state(m) = darkmode
        CASE "size": state(m) = bigwindow
        CASE "searchbottom": state(m) = searchbottomfixed
        CASE "transparency": state(m) = transparency
    END SELECT
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    interactable = 1
    xoffset(m) = xoffset
    basex(m) = (firstchar + (LEN(text$(m))) + xoffset(m) + 1) * fontwidth
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + 2 * fontheight
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
END SUB

SUB NewText (yoffset, xoffset, text$, clr&, weight$)
    m = m + 1
    type$(m) = "text"
    text$(m) = text$
    weight$(m) = weight$
    clr&(m) = clr&
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$(m)) + 1) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
END SUB

SUB NewInput (yoffset, xoffset, text$, placeholder$, number)
    m = m + 1
    g(m) = 0
    IF LEN(placeholder$) > 0 THEN
        gbf(m) = 0
        DO
            g(m) = g(m) + 1
            char$(m, g(m)) = MID$(placeholder$, g(m), 1)
        LOOP UNTIL g(m) = LEN(placeholder$)
    END IF
    gbf(m) = g(m)
    number(m) = number
    inputcount = m
    type$(m) = "input"
    text$(m) = text$
    interactable = 1
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    basex(m) = (firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$(m)) + g(m) + 2.5) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
    allowoverflow(m) = 1
    overflowlimit(m) = 150
END SUB

SUB NewMenItem (yoffset, xoffset, text$, destination$)
    m = m + 1
    type$(m) = "menu"
    text$(m) = text$
    yoffset(m) = yoffset * 2 - 1
    xoffset(m) = xoffset
    interactable = 1
    basex(m) = (firstchar + xoffset(m) - 1.5) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - 7
    endx(m) = basex(m) + (LEN(text$) + 2.5) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + 3
    destination$(m) = destination$
END SUB

SUB Background (layout, titel$, maintrigger)
    IF maintrigger = 1 THEN
        COLOR colour&("fg"), colour&("bg")
        CLS
        IF darkmode = 1 THEN
            newgcolor 1, 1, 0, colour&("black")
            newgcolor 1, 2, 100, _RGBA(0, 0, 25, 255)
            newgradient 1, 2
            drawgradient 1, 0, maxx, 0, maxy, "v"
        ELSE
            newgcolor 1, 1, 0, colour&("white")
            newgcolor 1, 2, 100, _RGBA(240, 240, 255, 255)
            newgradient 1, 2
            drawgradient 1, 0, maxx, 0, maxy, "v"
        END IF
        'borderthickness = 5
        'rectangle 0, 0, maxx, borderthickness, 0, colour&("fg"), "BF"
        'rectangle 0, 0, borderthickness, maxy, 0, colour&("fg"), "BF"
        'rectangle 0, maxy, maxx, maxy - borderthickness, 0, colour&("fg"), "BF"
        'rectangle maxx, 0, maxx - borderthickness, maxy, 0, colour&("fg"), "BF"
        lheight = 54
        _PUTIMAGE (0, 0)-((lheight * 4.82), lheight), l%
        closebuttonlx = (maxrows - 4) * fontwidth - 10
        closebuttonux = maxx - 10
        minbuttonlx = (maxrows - 7) * fontwidth - 10
        minbuttonux = closebuttonlx
        buttonsly = 10
        buttonsuy = (fontheight * 1.5) + 10
        rectangle minbuttonlx, buttonsly, closebuttonux, buttonsuy, UMround, colour&("fg"), "B"
        LINE (minbuttonux, buttonsly)-(minbuttonux, buttonsuy), colour&("fg")
        factor = 3
        'close button
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor), buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor), buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) + 1, buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) + 1, buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 1, buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 1, buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 2, buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 2, buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 3, buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 3, buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor), buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor), buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) + 1, buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) + 1, buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 1, buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 1, buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 2, buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 2, buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
        LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 3, buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 3, buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
        'minimize button
        LINE (minbuttonlx + ((minbuttonux - minbuttonlx) / factor), buttonsuy - ((buttonsuy - buttonsly) / factor))-(minbuttonux - ((minbuttonux - minbuttonlx) / factor), buttonsuy - ((buttonsuy - buttonsly) / factor) - 3), colour&("fg"), BF
        IF username$ <> "" THEN
            'name
            IF bigwindow = 0 THEN
                maxrows = INT((swidth / 1.5) / fontwidth)
                maxlines = INT((sheight / 2.2) / fontheight) - 4
            ELSE
                maxrows = INT(swidth / fontwidth)
                maxlines = INT(sheight / fontheight) - 4
            END IF
            usernamepos = maxrows - LEN(_TRIM$(username$)) - 18
            LOCATE firstline - 1, usernamepos
            PRINT "Angemeldet als: " + _TRIM$(username$)

            'profile image
            IF _FILEEXISTS(netpath$ + "data\pfp\" + username$ + ".png") = 0 THEN
                pfp& = Robohash(username$)
                Result = SaveImage(netpath$ + "data\pfp\" + username$ + ".png", pfp&, 0, 0, _WIDTH(pfp&), _HEIGHT(pfp&))
                IF Result = 1 THEN 'file already found on drive
                    KILL exportimage2$ 'delete the old file
                    Result = SaveImage(exportimage2$, 0, 0, 0, _WIDTH, _HEIGHT) 'save the new one again
                END IF
                _FREEIMAGE pfp&
            END IF
            pfp& = _LOADIMAGE(netpath$ + "data\pfp\" + username$ + ".png", 32)
            pfpsize = 4
            _PUTIMAGE ((usernamepos - (2.5 + pfpsize)) * fontwidth, (firstline - pfpsize + 0.5) * fontheight)-((usernamepos - 2.5) * fontwidth, ((firstline - pfpsize + 0.5) * fontheight) + (pfpsize * fontwidth)), pfp&

            'thiccc line
            LINE (((usernamepos - 2 - pfpsize) * fontwidth) - (fontwidth / 2), (firstline - 1) * fontheight - 3)-(maxx, (firstline - 1) * fontheight), colour&("bg"), BF
            LINE (((usernamepos - 2 - pfpsize) * fontwidth) - (fontwidth / 2), (firstline - 1) * fontheight - 3)-(maxx, (firstline - 1) * fontheight), colour&("fg"), BF
        END IF
        SELECT CASE layout
            CASE 0
                ' :)
            CASE 1
                LINE ((maxx / 2) - maxx / 6, (firstline - 1) * fontheight)-((maxx / 2) + maxx / 7, (maxlines - 2) * fontheight), colour&("fg"), B
                LINE ((maxx / 2) + maxx / 6, (firstline - 1) * fontheight)-(maxx - 20, (maxlines / 2) * fontheight), colour&("fg"), B
                LINE ((maxx / 2) + maxx / 6, (maxlines / 2 + 1) * fontheight)-(maxx - 20, (maxlines - 2) * fontheight), colour&("fg"), B
        END SELECT
    END IF
    SetTitel titel$
END SUB

SUB minimize
    logThis "Programm maximiert."
    hwnd& = _WINDOWHANDLE 'need the windows handle to play with it
    IF username$ <> "" THEN
        loginstring$ = "Angemeldet als: " + username$
    ELSE
        loginstring$ = "Nicht angemeldet."
    END IF
    minwinsizex = (10 + LEN(loginstring$)) * fontwidth
    minwinsizey = 75
    rgn& = CreateRoundRectRgn(4, 30, minwinsizex, minwinsizey, UMround, UMround)
    '_DELAY 0.5
    try& = SetWindowRgn(hwnd&, rgn&, 0)
    '_DELAY 0.5
    CLS
    SCREEN _NEWIMAGE(minwinsizex, minwinsizey, 32)
    DO: LOOP UNTIL _SCREENEXISTS
    _SCREENMOVE -5, _DESKTOPHEIGHT - 40 - _HEIGHT
    currentposx = -5
    currentposy = _DESKTOPHEIGHT - 40 - _HEIGHT
    _DEST 0
    COLOR colour&("fg"), colour&("bg")
    CLS
    closebuttonlx = _WIDTH - (4 * fontwidth) - 10
    closebuttonux = _WIDTH - 10
    minbuttonlx = _WIDTH - (8 * fontwidth) - 10
    minbuttonux = closebuttonlx
    buttonsly = 10
    buttonsuy = (fontheight * 1.5) + 10
    factor = 3
    rectangle minbuttonlx, buttonsly, closebuttonux, buttonsuy, UMround, colour&("fg"), "B"
    LINE (closebuttonlx, buttonsly)-(closebuttonlx, buttonsuy), colour&("fg")
    'close button
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor), buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor), buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) + 1, buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) + 1, buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 1, buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 1, buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 2, buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 2, buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 3, buttonsly + ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 3, buttonsuy - ((buttonsuy - buttonsly) / factor)), colour&("fg")
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor), buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor), buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) + 1, buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) + 1, buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 1, buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 1, buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 2, buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 2, buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
    LINE (closebuttonlx + ((closebuttonux - closebuttonlx) / factor) - 3, buttonsuy - ((buttonsuy - buttonsly) / factor))-(closebuttonux - ((closebuttonux - closebuttonlx) / factor) - 3, buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg")
    'maximize button
    LINE (minbuttonlx + ((minbuttonux - minbuttonlx) / factor), buttonsuy - ((buttonsuy - buttonsly) / factor))-(minbuttonux - ((minbuttonux - minbuttonlx) / factor), buttonsly + ((buttonsuy - buttonsly) / factor)), colour&("fg"), B
    LINE (minbuttonlx + ((minbuttonux - minbuttonlx) / factor), buttonsuy - ((buttonsuy - buttonsly) / factor))-(minbuttonux - ((minbuttonux - minbuttonlx) / factor), buttonsly + ((buttonsuy - buttonsly) / factor) + 1), colour&("fg"), B
    LOCATE 2, 3
    PRINT loginstring$
    _DISPLAY
    DO
        mouseinput = _MOUSEINPUT
    LOOP UNTIL _MOUSEX < minbuttonlx OR _MOUSEY > buttonsuy
    maximized = 0
    DO
        mouseinput = _MOUSEINPUT
        mousebutton = _MOUSEBUTTON(1)
        mousex = _MOUSEX
        mousey = _MOUSEY
        IF mouseinput = -1 THEN
            IF mousebutton = -1 THEN
                IF mousey >= buttonsly AND mousey <= buttonsuy THEN 'close button
                    IF mousex >= closebuttonlx AND mousex <= closebuttonux THEN
                        SYSTEM
                    END IF
                    IF mousex >= minbuttonlx AND mousex <= minbuttonux THEN 'maximize button
                        maximized = 1
                    END IF
                END IF
                DO
                    mouseinput = _MOUSEINPUT
                    mousebutton = _MOUSEBUTTON(1)
                LOOP UNTIL mousebutton <> -1
                _SCREENMOVE currentposx + (_MOUSEX - mousex), currentposy + (_MOUSEY - mousey)
                currentposx = currentposx + (_MOUSEX - mousex)
                currentposy = currentposy + (_MOUSEY - mousey)
            END IF
        END IF
    LOOP UNTIL maximized = 1
    logThis "Programm maximiert."
    IF bigwindow = 0 THEN
        maxx = _DESKTOPWIDTH / 1.5
        maxy = _DESKTOPHEIGHT / 2.2
        rgn& = CreateRoundRectRgn(4, 30, maxx, maxy, rounding, rounding)
        try& = SetWindowRgn(hwnd&, rgn&, 0)
        'Returns zero if failed...
        IF try& = 0 THEN
            END
        END IF
        SCREEN _NEWIMAGE(maxx, maxy, 32)
        canvas& = _NEWIMAGE(maxx, maxy, 32)
        DO: LOOP UNTIL _SCREENEXISTS
        _SCREENMOVE (_DESKTOPWIDTH / 2) - (_DESKTOPWIDTH / 1.5 / 2), (_DESKTOPHEIGHT / 2) - (_DESKTOPHEIGHT / 2. / 2)
        currentposx = (_DESKTOPWIDTH / 2) - (_DESKTOPWIDTH / 1.5 / 2)
        currentposy = (_DESKTOPHEIGHT / 2) - (_DESKTOPHEIGHT / 2. / 2)
    ELSE
        maxx = swidth
        maxy = sheight
        rgn& = CreateRoundRectRgn(7, 30, maxx, maxy, rounding, rounding)
        'Set the created region...
        try& = SetWindowRgn(hwnd&, rgn&, 0)
        'Returns zero if failed...
        IF try& = 0 THEN
            END
        END IF
        SCREEN _NEWIMAGE(maxx, maxy, 32)
        canvas& = _NEWIMAGE(maxx, maxy, 32)
        DO: LOOP UNTIL _SCREENEXISTS
        _SCREENMOVE 0, 0
        currentposx = 0
        currentposy = 0
    END IF
    COLOR colour&("fg"), colour&("bg")
    CLS
END SUB

SUB SetTitel (titel$)
    _FONT h&
    LOCATE 2
    COLOR colour&("fg"), colour&("transparent")
    PRINT SPC(17) + titel$
    _FONT r&
END SUB

SUB loadall
    timerstart = TIMER(.001)
    quickprint maxlines / 2 + 2, maxrows / 2, "Laden der Verzeichnisse...", 1, 1, colour&("fg"), colour&("bg")
    'checkt, ob alle ordner und dateien da sind, wenn nicht, dann werden sie erstellt
    IF _DIREXISTS(netpath$ + "data") = 0 THEN MKDIR netpath$ + "data"

    maxloadprogress = 15
    load "usr", "User": progressBar 0, 1, maxloadprogress
    load "obj", "Objekte": progressBar 0, 2, maxloadprogress
    load "asg", "Ausgaben": progressBar 0, 3, maxloadprogress
    checkmonth
    load "ort", "Orte": progressBar 0, 4, maxloadprogress
    load "plz", "Postleitzahlen": progressBar 0, 5, maxloadprogress
    load "adr", "Adressen": progressBar 0, 6, maxloadprogress
    load "knt", "Konten": progressBar 0, 7, maxloadprogress
    load "rbk", "Rubriken": progressBar 0, 8, maxloadprogress
    load "vea", "Veranstalter": progressBar 0, 11, maxloadprogress
    load "kaz", "Kleinanzeigen": progressBar 0, 12, maxloadprogress
    load "ver", "Veranstaltungen": progressBar 0, 13, maxloadprogress
    load "kat", "Kategorien": progressBar 0, 14, maxloadprogress

    IF _FILEEXISTS("data\AC.tcc") = 0 THEN
        OPEN netpath$ + "data\AC.tcc" FOR OUTPUT AS #1
        ac = 31: DO: ac = ac + 1
            WRITE #1, _DEFLATE$(CHR$(ac))
        LOOP UNTIL ac = 122
        CLOSE #1
    END IF
    OPEN netpath$ + "data\AC.tcc" FOR INPUT AS #1
    ac = 0: DO: ac = ac + 1
        INPUT #1, ac$(ac)
        ac$(ac) = _INFLATE$(ac$(ac))
    LOOP UNTIL EOF(1) = -1
    alch = ac: CLOSE #1
    progressBar 0, maxloadprogress, maxloadprogress
    timerend = TIMER(.001)
    timerdifference = timerend - timerstart
    quickprint maxlines / 2 + 2, maxrows / 2, "Laden der Daten erfolgreich!", 1, 1, colour&("fg"), colour&("bg")
    starttime$ = LST$(timerdifference)
    quickprint maxlines / 2 + 3, maxrows / 2, "Ladezeit: " + starttime$ + " Sekunden", 1, 1, colour&("fg"), colour&("bg")
    CLS
END SUB

SUB progressBar (position, progress, maxprogress)
    SELECT CASE position
        CASE 0 'center
            LINE (maxx / 2 - 200, maxy / 2 - 50)-(maxx / 2 + 200, maxy / 2 - 45), colour&("red"), B
            LINE (maxx / 2 - 200, maxy / 2 - 50)-(maxx / 2 - 200 + ((progress / maxprogress) * 400), maxy / 2 - 45), colour&("red"), BF
    END SELECT
END SUB

SUB load (short$, plural$)
    'quickprint maxlines / 2 + 2, maxrows / 2, "Laden der " + plural$ + "...", 1, 1
    IF readBinary(short$) <> 1 THEN
        resetToStandard (short$)
        writeBinary (short$)
        IF readBinary(short$) = 2 THEN quickprint maxlines / 2 + 2, maxrows / 2, plural$ + ": Keine Daten gefunden.", 1, 1, colour&("fg"), colour&("bg")
    END IF
    'quickprint maxlines / 2 + 2, maxrows / 2, plural$ + " geladen.", 1, 1
END SUB

SUB quickprint (y, x, text$, snapping, logging, fg&, bg&)
    SELECT CASE snapping
        CASE 0 'left bound
            LOCATE y, x
        CASE 1 'centered
            LOCATE y, x - (LEN(text$) / 2) - 10
            PRINT "          ";
        CASE 2 'right bound
            LOCATE y, x - LEN(text$)
    END SELECT
    COLOR fg&, bg&
    PRINT text$ + "                 "
    IF logging = 1 THEN
        logThis text$
    END IF
END SUB

SUB rectangle (lx, ly, ux, uy, round, clr&, outline$)
    SELECT CASE outline$
        CASE "BF"
            rectangleoutline lx, ly, ux, uy, round, clr&
            PAINT (lx + ((ux - lx) / 2), ly + ((uy - ly) / 2)), clr&, clr&
            IF shading = 1 THEN
                IF clr& = colour&("white") THEN
                    shader& = _RGBA(0, 0, 0, 80)
                ELSE
                    IF colour&("bg") <> colour&("white") THEN
                        shader& = colour&("white")
                    ELSE
                        shader& = _RGBA(0, 0, 0, 80)
                    END IF
                END IF
                'top right
                x = -0.25 * _PI
                DO: x = x + ((0.25 * _PI) / round / detail)
                    PSET ((lx + round) + (SIN(x) * round), (ly + round) - (COS(x) * round)), shader&
                LOOP UNTIL x >= 0
                x = -0.5 * _PI
                DO: x = x + ((0.5 * _PI) / round / detail)
                    PSET ((ux - round) - (SIN(x) * round), (ly + round) - (COS(x) * round)), shader&
                LOOP UNTIL x >= 0
                x = -0.5 * _PI
                DO: x = x + ((0.25 * _PI) / round / detail)
                    PSET ((ux - round) - (SIN(x) * round), (uy - round) + (COS(x) * round)), shader&
                LOOP UNTIL x >= -0.25
                LINE (lx + round, ly)-(ux - round, ly), shader&
                LINE (ux, ly + round)-(ux, uy - round), shader&
            END IF
        CASE "B"
            rectangleoutline lx, ly, ux, uy, round, clr&
    END SELECT
END SUB

SUB rectangleoutline (lx, ly, ux, uy, round, clr&)
    IF round > 0 THEN
        '           corners:
        'lx-ly
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((lx + round) + (SIN(x) * round), (ly + round) - (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'lx-uy
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((lx + round) + (SIN(x) * round), (uy - round) + (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'ux-ly
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((ux - round) - (SIN(x) * round), (ly + round) - (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'ux-uy
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((ux - round) - (SIN(x) * round), (uy - round) + (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        '           lines:
        LINE (lx + round, ly)-(ux - round, ly), clr&
        LINE (lx + round, uy)-(ux - round, uy), clr&
        LINE (lx, ly + round)-(lx, uy - round), clr&
        LINE (ux, ly + round)-(ux, uy - round), clr&
    ELSE
        LINE (lx, ly)-(ux, uy), clr&, B
    END IF
END SUB

SUB publishPost (title$, content$, comman$)
    logThis "Veroeffentliche Post " + title$ + " auf Webseite..."
    SELECT CASE category$
        CASE ""
    END SELECT
    adminpw$ = CHR$(34) + "bremer_admin:Hcjk 3Enh JZXE 05zl vy20 QdMH" + CHR$(34)
    url$ = "http://bremer.de/wp-json/wp/v2/posts "
    'posttype$ = "--data-urlencode " + CHR$(34) + "post_type=event" + colCHR$(34) + " "
    format$ = "--data-urlencode " + CHR$(34) + "format=standard" + CHR$(34) + " "
    title$ = "--data-urlencode " + CHR$(34) + "title=" + title$ + CHR$(34) + " "
    content$ = "--data-urlencode " + CHR$(34) + "content=" + content$ + CHR$(34) + " "
    'comman$ = "--data-urlencode " + CHR$(34) + comman$ + CHR$(34) + " "
    cmd$ = "CMD /C curl --user " + adminpw$ + " -X POST " + url$ + format$ + title$ + content$ + comman$ + " > response.txt"
    SHELL _HIDE cmd$
    SHELL _HIDE "start Notepad response.txt"
    logThis "Post veroeffentlicht."
END SUB

SUB fetchEvents
    logThis "Importiere Events..."
    OPEN netpath$ + "import\links.txt" FOR INPUT AS #100
    IF LOF(100) > 0 THEN
        DO
            LINE INPUT #100, name$ 'reads name from file
            LINE INPUT #100, url$ 'reads url from file
            PRINT "Lade " + url$ + " herunter..."
            logThis "Lade " + url$ + " herunter..."
            SHELL "wget -l 2 -nd -r -k -A html " + url$ + " -P import/site"
            SELECT CASE name$ 'opens downloaded .html/.php or whatever file
                CASE "Kulturzentrum Lagerhaus"
                    OPEN "import/site/index.html" FOR INPUT AS #101
                CASE "Kulturzentrum Schlachthof"
                    OPEN "import/site/programm.html" FOR INPUT AS #101
            END SELECT
            PRINT "Lese Tags f" + CHR$(129) + "r " + name$ + "..."
            IF LOF(101) > 0 THEN
                t = 0
                DO
                    LINE INPUT #101, line$
                    SELECT CASE name$
                        CASE "Kulturzentrum Lagerhaus"
                            p = 0: DO: p = p + 1
                                IF MID$(line$, p, 1) = "<" THEN
                                    u = p
                                    DO: p = p + 1
                                        IF MID$(line$, p, 1) = ">" THEN
                                            t = t + 1
                                            newtag = 1
                                            tag$(t) = MID$(line$, u, p - u + 1)
                                        END IF
                                    LOOP UNTIL MID$(line$, p, 1) = ">" OR p >= LEN(line$) 'second case should never happen, but who knows?
                                    IF p < LEN(line$) AND MID$(line$, p + 1, 1) <> "<" THEN 'excludes closing tags and tags without content
                                        u = p + 1
                                        DO: p = p + 1
                                        LOOP UNTIL MID$(line$, p + 1, 1) = "<" OR p >= LEN(line$)
                                        content$(t) = MID$(line$, u, p - u + 1)
                                        IF content$(t) = SPC(LEN(content$(t))) OR content$(t) = longchar$(CHR$(9), LEN(content$(t))) THEN 'removes tag if content is empty
                                            t = t - 1
                                        END IF
                                    ELSEIF MID$(line$, p + 1, 1) = "<" THEN 'excludes tags without content
                                        t = t - 1
                                    ELSE
                                        IF newtag = 1 THEN
                                            t = t - 1 'removes closing tags and tags without content
                                        END IF
                                    END IF
                                    IF t > 2 THEN
                                        IF tag$(t) = "</span>" THEN
                                            IF tag$(t - 1) = "<span class=" + CHR$(34) + "caps" + CHR$(34) + ">" THEN
                                                IF tag$(t - 2) = "<p>" THEN
                                                    tag$(t - 2) = "<p>"
                                                    content$(t - 2) = content$(t - 2) + content$(t - 1) + content$(t)
                                                    t = t - 2
                                                END IF
                                            END IF
                                        END IF
                                    END IF
                                    newtag = 0
                                END IF
                            LOOP UNTIL p >= LEN(line$)
                    END SELECT
                LOOP UNTIL EOF(101) = -1
                maxt = t
                IF maxt > 0 THEN
                    OPEN "import\list2.txt" FOR OUTPUT AS #102
                    OPEN "import\list3.txt" FOR OUTPUT AS #103
                    skiptitle = 1
                    iv = 0
                    t = 0: DO: t = t + 1
                        'html replacer
                        runcode = 0: DO: runcode = runcode + 1
                            p = 0: DO: p = p + 1
                                SELECT CASE MID$(content$(t), p, 1)
                                    CASE "&" 'replaces html codes
                                        u = p
                                        DO: p = p + 1
                                        LOOP UNTIL MID$(content$(t), p, 1) = ";" OR p = LEN(content$(t))
                                        IF MID$(content$(t), p, 1) = ";" THEN
                                            htmlcode$ = MID$(content$(t), u, p - u + 1)
                                            content$(t) = MID$(content$(t), 1, u - 1) + htmlreplace$(htmlcode$) + MID$(content$(t), p + 1, LEN(content$(t)) - p)
                                            p = p - LEN(htmlcode$) + 1
                                        END IF
                                END SELECT
                                SELECT CASE MID$(content$(t), p, 2)
                                    CASE CHR$(195) + CHR$(164) 'ae
                                        content$(t) = MID$(content$(t), 1, p - 1) + CHR$(132) + MID$(content$(t), p + 2, LEN(content$(t)) - p)
                                    CASE CHR$(195) + CHR$(182) 'oe
                                        content$(t) = MID$(content$(t), 1, p - 1) + CHR$(148) + MID$(content$(t), p + 2, LEN(content$(t)) - p)
                                    CASE CHR$(195) + CHR$(188) 'ue
                                        content$(t) = MID$(content$(t), 1, p - 1) + CHR$(129) + MID$(content$(t), p + 2, LEN(content$(t)) - p)
                                    CASE CHR$(195) + CHR$(170) 'e
                                        content$(t) = MID$(content$(t), 1, p - 1) + CHR$(136) + MID$(content$(t), p + 2, LEN(content$(t)) - p)
                                END SELECT
                                SELECT CASE MID$(content$(t), p, 3) ' *sigh*
                                    CASE CHR$(226) + CHR$(128) + CHR$(158) '"-down
                                        content$(t) = MID$(content$(t), 1, p - 1) + CHR$(34) + MID$(content$(t), p + 3, LEN(content$(t)) - p)
                                    CASE CHR$(226) + CHR$(128) + CHR$(156) '"-up
                                        content$(t) = MID$(content$(t), 1, p - 1) + CHR$(34) + MID$(content$(t), p + 3, LEN(content$(t)) - p)
                                    CASE CHR$(226) + CHR$(128) + CHR$(153) 'apostrophe
                                        content$(t) = MID$(content$(t), 1, p - 1) + "'" + MID$(content$(t), p + 3, LEN(content$(t)) - p)
                                END SELECT
                            LOOP UNTIL p >= LEN(content$(t))
                        LOOP UNTIL runcode = 1
                        'print to file
                        PRINT #102, tag$(t)
                        PRINT #102, content$(t)
                    LOOP UNTIL t >= maxt
                    t = 0: DO: t = t + 1
                        SELECT CASE name$
                            CASE "Kulturzentrum Lagerhaus"
                                IF MID$(tag$(t), 1, 23) = "<a rel=" + CHR$(34) + "bookmark" + CHR$(34) + " href=" THEN 'starting tag for event (titel)
                                    IF content$(t) <> "Das Lagerhaus" AND content$(t) <> "Impressum" AND content$(t) <> "Kontakt" AND content$(t) <> "Datenschutz" AND content$(t) <> "Projekte" THEN 'exclude static cases
                                        iv = iv + 1
                                        titel$(iv) = content$(t)
                                        'PRINT "Veranstaltung gefunden: " + titel$(iv)
                                        t = t + 1
                                        IF tag$(t) = "<span id=" + CHR$(34) + "datum_global" + CHR$(34) + " class=" + CHR$(34) + "datum_lagerhaus" + CHR$(34) + ">" THEN 'datum-tag
                                            datum$(iv) = MID$(content$(t), LEN(content$(t)) - 9, 10)
                                        ELSE 'nothing, this should never happen
                                        END IF
                                        t = t + 1
                                        IF tag$(t) = "<span class=" + CHR$(34) + "zeit" + CHR$(34) + ">" THEN 'uhrzeit-tag
                                            zeit$(iv) = MID$(content$(t), LEN(content$(t)) - 4, 5)
                                        END IF
                                        DO: t = t + 1
                                        LOOP UNTIL tag$(t) = "<p>" OR tag$(t) = "</strong>" OR t >= maxt
                                        IF tag$(t) = "<p>" OR tag$(t) = "</strong>" THEN
                                            IF content$(t) <> "" THEN
                                                ivtext$(iv) = content$(t)
                                            ELSE
                                                iv = iv - 1
                                            END IF
                                        END IF
                                    END IF
                                END IF
                            CASE "Kulturzentrum Schlachthof"

                        END SELECT
                    LOOP UNTIL t = maxt
                    PRINT iv; " Veranstaltungen gefunden."
                END IF
            END IF
            CLOSE #101
            maxiv = iv
            runcode = 0: DO: runcode = runcode + 1
                iv = 0: DO: iv = iv + 2
                    coiv = iv - 1
                    IF titel$(iv) = titel$(coiv) AND datum$(iv) = datum$(coiv) AND zeit$(iv) = zeit$(coiv) THEN
                        IF LEN(ivtext$(iv)) > LEN(ivtext$(coiv)) THEN
                            printiv = iv
                        ELSE
                            printiv = coiv
                        END IF
                        PRINT #103, "Titel", titel$(printiv)
                        PRINT #103, "Datum", datum$(printiv)
                        PRINT #103, "Zeit", zeit$(printiv)
                        PRINT #103, "Text", ivtext$(printiv)
                    ELSE
                        PRINT #103, "Titel", titel$(coiv)
                        PRINT #103, "Datum", datum$(coiv)
                        PRINT #103, "Zeit", zeit$(coiv)
                        PRINT #103, "Text", ivtext$(coiv)
                        iv = iv - 1
                    END IF
                LOOP UNTIL iv >= maxiv
            LOOP UNTIL runcode = 2
            CLOSE #102
            CLOSE #103
            SLEEP
        LOOP UNTIL EOF(100) = -1
    END IF
    CLOSE #100
END SUB

FUNCTION htmlreplace$ (htmlcode$)
    SELECT CASE htmlcode$
        CASE "&#160;": htmlreplace$ = " "
        CASE "&amp;": htmlreplace$ = "&"
        CASE "&nbsp;": htmlreplace$ = " "
        CASE "&#8217;": htmlreplace$ = "'"
        CASE "&#8211;": htmlreplace$ = "-"
        CASE "&#8230;": htmlreplace$ = "..."
    END SELECT
    EXIT FUNCTION
END FUNCTION

SUB printviaprinter 'Code by SpriggsySpriggs: https://www.qb64.org/forum/index.php?topic=939.msg117741#msg117741
    IF _FILEEXISTS(netpath$ + "printdialog.ps1") = 0 THEN
        OPEN netpath$ + "printdialog.ps1" FOR OUTPUT AS #12
        PRINT #12, "$PSDefaultParametervalues['Out-File:Encoding'] = 'utf8'"
        PRINT #12, "Add-Type -AssemblyName System.Windows.Forms"
        PRINT #12, "$prnDlg=New-Object System.Windows.Forms.PrintDialog"
        PRINT #12, "$prnDlg.ShowDialog() > cancel.txt"
        PRINT #12, "$prnDlg.PrinterSettings.Copies > copies.txt"
        PRINT #12, "$prnDlg.PrinterSettings.PrinterName > printername.txt"
        CLOSE #12
        SHELL _HIDE _DONTWAIT "attrib +h " + netpath$ + "printdialog.ps1"
    END IF
    SHELL _HIDE "powershell -ExecutionPolicy Bypass -Command " + CHR$(34) + "&" + CHR$(34) + CHR$(34) + netpath$ + "\printdialog.ps1" + CHR$(34) + CHR$(34) + CHR$(34) 'This dialog returns whatever is selected, regardless of exiting without pressing PRINT
    _DELAY 0.2 'Makes program wait 0.2 seconds in order to ensure file has been deployed and ready for viewing
    DO: LOOP UNTIL _FILEEXISTS(netpath$ + "\printdialog.ps1")
    CLOSE #4
    CLOSE #5
    CLOSE #8
    OPEN "copies.txt" FOR BINARY AS #4 'Opens file to read in data from copies.txt
    OPEN "printername.txt" FOR BINARY AS #5 'Opens file to read in data from printername.txt
    OPEN "cancel.txt" FOR BINARY AS #8 'Opens file to read in data from cancel.txt
    LINE INPUT #4, copies$ 'Reads in data from copies.txt and stores it in the string copies$
    LINE INPUT #5, printername$ 'Reads in data from printername.txt and stores it in the string printername$
    LINE INPUT #8, cancel$ 'Reads in data from cancel.txt and stores it in the string cancel$
    LENGTHCOP = LEN(copies$) 'Stores the length of the copies$ string into an integer called LENGTHCOP
    LENGTHPN = LEN(printername$) 'Stores the length of the printername$ string into an integer called LENGTHPN
    LENGTHCAN = LEN(cancel$) 'Stores the length of the cancel$ string into an integer called LENGTHCAN
    copies$ = RIGHT$(copies$, LENGTHCOP - 3) 'Trims off the first three characters in the copies$ string
    printername$ = RIGHT$(printername$, LENGTHPN - 3) 'Trims off the first three characters in the printername$ string
    cancel$ = RIGHT$(cancel$, LENGTHCAN - 3) 'Trims off the first 3 characters in the cancel$ string
    CLOSE #4
    CLOSE #5
    CLOSE #8
    IF cancel$ = "Cancel" THEN
        copies = 0
    ELSE
        copies = VAL(copies$) 'Converts string value of copies$ to an integer value for counting
        DO UNTIL copies = 0 'Performs command below until copies = 0
            SHELL _HIDE _DONTWAIT "NOTEPAD /pt " + CHR$(34) + "toprinter" + CHR$(34) + " " + CHR$(34) + printername$ + CHR$(34) 'Opens NOTEPAD and uses the /pt switch to send file to printer
            copies = copies - 1 'Subtracts 1 from the copies integer
        LOOP
    END IF
    KILL "copies.txt": KILL "printername.txt": KILL "cancel.txt": KILL "printdialog.ps1"
    _DELAY 5
    KILL "toprinter.txt"
END SUB

SUB importEvent (source$)
    logThis "Importiere Event aus " + source$ + "-Datei..."
    SELECT CASE source$
        CASE "csv"

        CASE "doc"

        CASE "txt"

    END SELECT
END SUB

SUB exportToQuark (listID$, ausgabe$, start$, end$)
    logThis "Exportiere " + listID$ + " fuer + " + ausgabe$ + " von " + start$ + " bis " + end$ + "..."
    SELECT CASE listID$
        CASE "kaz"
            IF max(10) > 0 THEN
                FORMkaztitel$ = "@KAZ_Titel:<f$>"
                FORMkaztext$ = "@KAZ_Normal"
                FORMkazkat1$ = "@KAZ_Rubrik:<f$>"
                FORMkazkat2$ = "@KAZ_Rubrik2:<$>"
                OPEN netpath$ + "export\KAZ_" + ausgabe$ + "(" + username$ + ").XTG" FOR OUTPUT AS #5
                'header
                PRINT #5, "<v1.70><e10>"
                'variable content
                kt = 0: DO: kt = kt + 1
                    SELECT CASE RTRIM$(Kategorie(kt).Name)
                        CASE "Kurse/Workshops/Seminare": PRINT #5, FORMkazkat1$ + "Aktivit" + CHR$(228) + "ten"
                        CASE "Frau sucht Mann": PRINT #5, FORMkazkat1$ + "Kontakte"
                        CASE "Mietgesuche": PRINT #5, FORMkazkat1$ + "Vermietung"
                        CASE "Stellen- & Jobangebote": PRINT #5, FORMkazkat1$ + "Stellenmarkt"
                        CASE "Fahrzeug-Verleih": PRINT #5, FORMkazkat1$ + "Fahrzeuge"
                        CASE "Sonstiger Ankauf": PRINT #5, FORMkazkat1$ + "Handel"
                        CASE "Veranstaltungen": PRINT #5, FORMkazkat1$ + "Treffs"
                        CASE "Bands": PRINT #5, FORMkazkat1$ + "Musik"
                        CASE "Pauschal/Fl" + CHR$(129) + "ge": PRINT #5, FORMkazkat1$ + "Reisen"
                    END SELECT
                    printed(kt) = 0
                    ka = 0: DO: ka = ka + 1
                        progressBar 0, ka + (max(10) * (kt - 1)), (max(10) * max(12))
                        COLOR colour&("fg"), colour&("bg")
                        quickprint maxlines / 2 + 2, maxrows / 2, "Exportiere nach Quark... " + LST$(INT((ka + (max(10) * (kt - 1))) / (max(10) * max(12)) * 100)) + "%", 1, 1, colour&("fg"), colour&("bg")
                        IF Kleinanzeige(ka).Kategorie1 = Kategorie(kt).Name AND Kleinanzeige(ka).Ausgabe = VAL(ausgabe$) THEN
                            IF printed(kt) = 0 THEN
                                printed(kt) = 1
                                IF RTRIM$(Kategorie(kt).Name) <> "Verschiedenes" THEN
                                    PRINT #5, FORMkazkat2$ + RTRIM$(Kategorie(kt).Name)
                                ELSE
                                    PRINT #5, FORMkazkat1$ + RTRIM$(Kategorie(kt).Name)
                                END IF
                            END IF
                            PRINT #5, FORMkaztitel$ + RTRIM$(Kleinanzeige(ka).Titel) + FORMkaztext$ + RTRIM$(Kleinanzeige(ka).Text) + " [T]" + RTRIM$(Kleinanzeige(ka).Telefon)
                        END IF
                    LOOP UNTIL ka = max(10)
                LOOP UNTIL kt = max(12) - 1
                progressBar 0, max(10) * max(12), (max(10) * max(12))
                COLOR colour&("fg"), colour&("bg")
                quickprint maxlines / 2 + 2, maxrows / 2, "Exportiere nach Quark... 100%", 1, 1, colour&("fg"), colour&("bg")
                CLOSE #5
            ELSE

            END IF
        CASE "ver"
            IF max(11) > 0 THEN
                FORMverdatum$ = "@PK_Datum:<f$>"
                FORMvernormal$ = "@PK_Normal:<f$>"
                FORMverrubrik$ = "@PK_Rubrik:<f$>"
                FORMverort$ = "@PK_Ort:<f$>"
                OPEN netpath$ + "export\PK_" + ausgabe$ + "(" + username$ + ").XTG" FOR OUTPUT AS #5
                'header
                PRINT #5, "<v1.70><e1>"
                'variable content
                daystart = VAL(MID$(start$, 9, 2))
                dayend = VAL(MID$(end$, 9, 2))
                monthstart = VAL(MID$(start$, 6, 2))
                monthend = VAL(MID$(end$, 6, 2))
                yearstart = VAL(MID$(start$, 1, 4))
                yearend = VAL(MID$(end$, 1, 4))
                y = yearstart - 1: m = monthstart - 1: d = daystart - 1
                dif = maxday(yearstart, monthstart) * max(11) * max(4) * max(8)
                prog = 0
                DO: y = y + 1
                    DO: m = m + 1
                        DO: d = d + 1
                            printedd(d) = 0
                            'Datum$ = LST$(y) + "-" + LST$(m) + "-" + LST$(d)
                            Datum$ = LST$(y) + "-"
                            IF m < 10 THEN
                                Datum$ = Datum$ + "0" + LST$(m) + "-"
                            ELSE
                                Datum$ = Datum$ + LST$(m) + "-"
                            END IF
                            IF d < 10 THEN
                                Datum$ = Datum$ + "0" + LST$(d)
                            ELSE
                                Datum$ = Datum$ + LST$(d)
                            END IF
                            r = 0: DO: r = r + 1
                                printed(r) = 0
                                ot = 0: DO: ot = ot + 1
                                    printedo(ot) = 0
                                    v = 0: DO: v = v + 1
                                        prog = prog + 1
                                        progressBar 0, prog, dif
                                        quickprint maxlines / 2 + 2, maxrows / 2, "Exportiere nach Quark... " + LST$(INT(prog / dif * 100)) + "%", 1, 1, colour&("fg"), colour&("bg")
                                        IF (Veranstaltung(v).Rubrik = Rubrik(r).Name AND Veranstaltung(v).Ausgabe = VAL(ausgabe$) AND RTRIM$(Veranstaltung(v).Datum) = Datum$ AND OrtF$(v) = Ort(ot).Name) OR (Veranstaltung(v).Rubrik = Rubrik(r).Name AND Veranstaltung(v).Ausgabe = VAL(ausgabe$) AND RTRIM$(Veranstaltung(v).Datum) = Datum$ AND RTRIM$(Veranstaltung(v).Veranstalter) = "" AND RTRIM$(Veranstaltung(v).Ort) = Ort(ot).Name) THEN
                                            'headers
                                            IF printedd(d) = 0 THEN
                                                printedd(d) = 1
                                                PRINT #5, FORMverdatum$ + LST$(d) + ". " + LST$(m) + "."
                                            END IF
                                            IF printed(r) = 0 THEN
                                                printed(r) = 1
                                                PRINT #5, FORMverrubrik$ + RTRIM$(Rubrik(r).Name)
                                            END IF
                                            IF printedo(ot) = 0 THEN
                                                printedo(ot) = 1
                                                PRINT #5, FORMverort$ + RTRIM$(Ort(ot).Name)
                                            END IF

                                            'content-assembling
                                            IF RTRIM$(Veranstaltung(v).Zeitcode) <> "" THEN
                                                IF Veranstaltung(v).Zeit3 <> "00.00" THEN
                                                    vtime$ = RTRIM$(Veranstaltung(v).Zeit1) + RTRIM$(Veranstaltung(v).Zeitcode) + RTRIM$(Veranstaltung(v).Zeit2) + RTRIM$(Veranstaltung(v).Zeitcode) + RTRIM$(Veranstaltung(v).Zeit3) + " "
                                                ELSE
                                                    vtime$ = RTRIM$(Veranstaltung(v).Zeit1) + RTRIM$(Veranstaltung(v).Zeitcode) + RTRIM$(Veranstaltung(v).Zeit2) + " "
                                                END IF
                                            ELSE
                                                vtime$ = RTRIM$(Veranstaltung(v).Zeit1) + " "
                                            END IF
                                            IF RTRIM$(Veranstaltung(v).Veranstalter) <> "" THEN
                                                vort$ = "; " + RTRIM$(Veranstaltung(v).Veranstalter)
                                                va = 0: DO: va = va + 1
                                                    IF Veranstalter(va).Name = Veranstaltung(v).Veranstalter THEN
                                                        PRINT "1 yes"
                                                        _DELAY 0.1
                                                        dd = 0: DO: dd = dd + 1
                                                            IF Adresse(dd).ID = Veranstalter(va).Adresse THEN
                                                                PRINT "2 yes"
                                                                SLEEP
                                                                vort$ = vort$ + ", " + RTRIM$(Adresse(dd).Strasse)
                                                            END IF
                                                        LOOP UNTIL dd = max(6)
                                                    END IF
                                                LOOP UNTIL va = max(9)
                                            END IF
                                            'content-printing
                                            IF RTRIM$(Veranstaltung(v).TextLang) <> "" THEN
                                                PRINT #5, FORMvernormal$ + "<B>" + vtime$ + RTRIM$(Veranstaltung(v).Titel) + " <P>" + FORMvertext$ + RTRIM$(Veranstaltung(v).TextLang) + vort$ + vadr$
                                            ELSE
                                                PRINT #5, FORMvernormal$ + "<B>" + vtime$ + RTRIM$(Veranstaltung(v).Titel) + " <P>" + FORMvertext$ + RTRIM$(Veranstaltung(v).Text) + vort$ + vadr$
                                            END IF
                                            vort$ = "": vadr$ = ""
                                        END IF
                                    LOOP UNTIL v = max(11)
                                LOOP UNTIL ot = max(4)
                            LOOP UNTIL r = max(8)
                        LOOP UNTIL d = dayend OR d = maxday(y, m)
                        d = 0
                    LOOP UNTIL m = monthend
                    m = 0
                LOOP UNTIL y = yearend
                CLOSE #5
            ELSE

            END IF
    END SELECT
    logThis "Erfolgreich exportiert."
END SUB

SUB exportToCsv (listID$, ausgabe$, formatting$)
    logThis "Exportiere " + listID$ + " fuer + " + ausgabe$ + "..."
    SELECT CASE formatting$
        CASE "eventscalendar"
            SELECT CASE listID$
                CASE "ver"
                    OPEN netpath$ + "export\" + UCASE$(listID$) + "_" + ausgabe$ + ".csv" FOR OUTPUT AS #6
                    IF max(11) > 0 THEN
                        PRINT #6, "EVENT NAME,VENUE NAME,ORGANIZER NAME,START DATE,START TIME,END DATE,END TIME,ALL DAY EVENT?,CATEGORIES,EVENT COST,EVENT PHONE,EVENT WEBSITE,SHOW MAP LINK?,SHOW MAP?,EVENT DESCRIPTION"
                        v = 0: DO: v = v + 1
                            IF Veranstaltung(v).Ausgabe = VAL(ausgabe$) THEN
                                IF VAL(MID$(Veranstaltung(v).Zeit1, 1, 2)) > 11 THEN zeit1$ = RTRIM$(Veranstaltung(v).Zeit1) + " PM" ELSE zeit1$ = RTRIM$(Veranstaltung(v).Zeit1) + " AM"
                                IF VAL(MID$(Veranstaltung(v).Zeit2, 1, 2)) > 11 THEN zeit2$ = RTRIM$(Veranstaltung(v).Zeit2) + " PM" ELSE zeit2$ = RTRIM$(Veranstaltung(v).Zeit2) + " AM"
                                PRINT #6, CHR$(34) + RTRIM$(Veranstaltung(v).Titel) + CHR$(34) + "," + CHR$(34) + RTRIM$(Veranstaltung(v).Veranstalter) + CHR$(34) + ",," + RTRIM$(Veranstaltung(v).Datum) + "," + zeit1$ + "," + RTRIM$(Veranstaltung(v).Datum) + "," + zeit2$ + ",FALSE," + CHR$(34) + RTRIM$(Veranstaltung(v).Rubrik) + CHR$(34) + ",,,,TRUE,TRUE," + CHR$(34) + RTRIM$(Veranstaltung(v).TextLang) + CHR$(34)
                            END IF
                        LOOP UNTIL v = max(11)
                    END IF
                    CLOSE #6
            END SELECT
    END SELECT
    logThis "Erfolgreich exportiert."
END SUB

FUNCTION OrtF$ (v)
    vva = 0: DO: vva = vva + 1
        IF RTRIM$(Veranstalter(vva).Kuerzel) + ", " + RTRIM$(Veranstalter(vva).Name) = RTRIM$(Veranstaltung(v).Veranstalter) THEN
            ddd = 0: DO: ddd = ddd + 1
                IF Adresse(ddd).ID = Veranstalter(vva).Adresse THEN
                    vadr$ = ", " + RTRIM$(Adresse(ddd).Strasse)
                    IF RTRIM$(Adresse(ddd).Ort) <> "Bremen" THEN vadr$ = vadr$ + ", " + LST$(Adresse(ddd).PLZ) + " " + RTRIM$(Adresse(ddd).Ort)
                    OrtF$ = Adresse(ddd).Ort
                END IF
            LOOP UNTIL ddd = max(6)
        END IF
    LOOP UNTIL vva = max(9)
END SUB

SUB drawTriangle (x, y, trianglescale, direction)
    'variables for dropdown triangle (on the right side, you know?)
    p1x = x
    IF direction = 1 THEN p1y = y ELSE p1y = y + (((fontwidth * trianglescale) * 2 / 3))
    p2x = p1x + (fontwidth * trianglescale)
    p2y = p1y
    p3x = p1x + ((p2x - p1x) / 2)
    p3y = p1y + (((fontwidth * trianglescale) * 2 / 3) * direction)

    tricolor& = colour&("fg")
    'triangle
    LINE (p1x, p1y)-(p2x, p2y), tricolor&: LINE (p1x, p1y)-(p3x, p3y), tricolor&: LINE (p2x, p2y)-(p3x, p3y), tricolor&
    PAINT (p1x + ((p2x - p1x) / 2), p1y + ((p3y - p1y) / 2)), tricolor&, tricolor&
END SUB

SUB defChar
    RESTORE KB1031toCP437_Cp437
    RESTORE KB1031toCP437_Umlaut
    EXIT SUB
    KB1031toCP437_Umlaut:
    DATA 8,65,142,79,153,85,154,97,132,101,137,105,139,111,148,117,129
    KB1031toCP437_Cp437:
    DATA 199,252,233,226,228,224,229,231,234,235,232,239,238,236,196,197
    DATA 201,230,198,244,246,242,251,249,255,214,220,162,163,165,8359,402
    DATA 225,237,243,250,241,209,170,186,191,8976,172,189,188,161,171,187
    DATA 9617,9618,9619,9474,9508,9569,9570,9558,9557,9571,9553,9559,9565,9564,9563,9488
    DATA 9492,9524,9516,9500,9472,9532,9566,9567,9562,9556,9577,9574,9568,9552,9580,9575
    DATA 9576,9572,9573,9561,9560,9554,9555,9579,9578,9496,9484,9608,9604,9612,9616,9600
    DATA 945,223,915,960,931,963,181,964,934,920,937,948,8734,966,949,8745
    DATA 8801,177,8805,8804,8992,8993,247,8776,176,8729,183,8730,8319,178,9632,32
END SUB

FUNCTION longchar$ (char$, length) 'returns a string consisting of one character repeated (length) times
    temp$ = ""
    l = 0: DO: l = l + 1
        temp$ = temp$ + char$
    LOOP UNTIL l = length
    longchar$ = temp$
END FUNCTION

FUNCTION LST$ (number) 'converts number into string
    LST$ = LTRIM$(STR$(number))
END FUNCTION

FUNCTION monthstr$ (m)
    SELECT CASE m
        CASE 1: month$ = "Januar"
        CASE 2: month$ = "Februar"
        CASE 3: month$ = "M" + CHR$(228) + "rz"
        CASE 4: month$ = "April"
        CASE 5: month$ = "Mai"
        CASE 6: month$ = "Juni"
        CASE 7: month$ = "Juli"
        CASE 8: month$ = "August"
        CASE 9: month$ = "September"
        CASE 10: month$ = "Oktober"
        CASE 11: month$ = "November"
        CASE 12: month$ = "Dezember"
    END SELECT
END FUNCTION

FUNCTION isLeap (year)
    IF year MOD 4 <> 0 THEN
        isLeap = 0
    ELSEIF year MOD 100 <> 0 THEN
        isLeap = 1
    ELSEIF year MOD 400 <> 0 THEN
        isLeap = 0
    ELSE
        isLeap = 1
    END IF
END FUNCTION

FUNCTION maxday (year, month)
    SELECT CASE month
        CASE 2
            IF isLeap(year) = 1 THEN
                maxday = 29
            ELSE
                maxday = 28
            END IF
        CASE 1: maxday = 31
        CASE 3: maxday = 31
        CASE 5: maxday = 31
        CASE 7: maxday = 31
        CASE 8: maxday = 31
        CASE 10: maxday = 31
        CASE 12: maxday = 31
        CASE 4: maxday = 30
        CASE 6: maxday = 30
        CASE 9: maxday = 30
        CASE 11: maxday = 30
    END SELECT
END FUNCTION

SUB writeBinary (type$)
    logThis "Speichern von " + type$ + "..."
    detRecLen type$
    IF _FILEEXISTS(netpath$ + "data\" + type$ + ".bremer") THEN KILL netpath$ + "data\" + type$ + ".bremer"
    OPEN netpath$ + "data\" + type$ + ".bremer" FOR RANDOM AS #1 LEN = recLEN
    obscure type$
    SELECT CASE type$
        CASE "usr": IF max(1) > 0 THEN u = 0: DO: u = u + 1: PUT #1, u, User(u): LOOP UNTIL u = max(1)
        CASE "obj": IF max(2) > 0 THEN o = 0: DO: o = o + 1: PUT #1, o, Objekt(o): LOOP UNTIL o = max(2)
        CASE "asg": IF max(3) > 0 THEN a = 0: DO: a = a + 1: PUT #1, a, Ausgabe(a): LOOP UNTIL a = max(3)
        CASE "ort": IF max(4) > 0 THEN ot = 0: DO: ot = ot + 1: PUT #1, ot, Ort(ot): LOOP UNTIL ot = max(4)
        CASE "plz": IF max(5) > 0 THEN p = 0: DO: p = p + 1: PUT #1, p, PLZ(p): LOOP UNTIL p = max(5)
        CASE "adr": IF max(6) > 0 THEN d = 0: DO: d = d + 1: PUT #1, d, Adresse(d): LOOP UNTIL d = max(6)
        CASE "knt": IF max(7) > 0 THEN k = 0: DO: k = k + 1: PUT #1, k, Konto(k): LOOP UNTIL k = max(7)
        CASE "rbk": IF max(8) > 0 THEN r = 0: DO: r = r + 1: PUT #1, r, Rubrik(r): LOOP UNTIL r = max(8)
        CASE "vea": IF max(9) > 0 THEN va = 0: DO: va = va + 1: PUT #1, va, Veranstalter(va): LOOP UNTIL va = max(9)
        CASE "kaz": IF max(10) > 0 THEN ka = 0: DO: ka = ka + 1: PUT #1, ka, Kleinanzeige(ka): LOOP UNTIL ka = max(10)
        CASE "ver": IF max(11) > 0 THEN v = 0: DO: v = v + 1: PUT #1, v, Veranstaltung(v): LOOP UNTIL v = max(11)
        CASE "kat": IF max(12) > 0 THEN kt = 0: DO: kt = kt + 1: PUT #1, kt, Kategorie(kt): LOOP UNTIL kt = max(12)
    END SELECT
    CLOSE #1
    logThis "Erfolgreich gespeichert."
END SUB

SUB delete (type$, node)
    logThis "L" + CHR$(148) + "sche " + LST$(node) + " in " + type$ + "..."
    detRecLen type$
    IF _FILEEXISTS(netpath$ + "data\" + type$ + ".bremer") THEN KILL netpath$ + "data\" + type$ + ".bremer"
    OPEN netpath$ + "data\" + type$ + ".bremer" FOR RANDOM AS #1 LEN = recLEN
    obscure type$
    SELECT CASE type$
        CASE "usr"
            IF max(1) > 0 THEN
                u = 0: DO: u = u + 1
                    IF u <> node THEN PUT #1, u, User(u)
                LOOP UNTIL u = max(1)
                max(1) = max(1) - 1
            END IF
        CASE "obj"
            IF max(2) > 0 THEN
                o = 0: DO: o = o + 1
                    IF o <> node THEN PUT #1, o, Objekt(o)
                LOOP UNTIL o = max(2)
                max(2) = max(2) - 1
            END IF
        CASE "asg"
            IF max(3) > 0 THEN
                a = 0: DO: a = a + 1
                    IF a <> node THEN PUT #1, a, Ausgabe(a)
                LOOP UNTIL a = max(3)
                max(3) = max(3) - 1
            END IF
        CASE "ort"
            IF max(4) > 0 THEN
                ot = 0: DO: ot = ot + 1
                    IF ot <> node THEN PUT #1, ot, Ort(ot)
                LOOP UNTIL ot = max(4)
                max(4) = max(4) - 1
            END IF
        CASE "plz"
            IF max(5) > 0 THEN
                p = 0: DO: p = p + 1
                    IF p <> node THEN PUT #1, p, PLZ(p)
                LOOP UNTIL p = max(5)
                max(5) = max(5) - 1
            END IF
        CASE "adr"
            IF max(6) > 0 THEN
                d = 0: DO: d = d + 1
                    IF d <> node THEN PUT #1, d, Adresse(d)
                LOOP UNTIL d = max(6)
                max(6) = max(6) - 1
            END IF
        CASE "knt"
            IF max(7) > 0 THEN
                k = 0: DO: k = k + 1
                    IF k <> node THEN PUT #1, k, Konto(k)
                LOOP UNTIL k = max(7)
                max(7) = max(7) - 1
            END IF
        CASE "rbk"
            IF max(8) > 0 THEN
                r = 0: DO: r = r + 1
                    IF r <> node THEN PUT #1, r, Rubrik(r)
                LOOP UNTIL r = max(8)
                max(8) = max(8) - 1
            END IF
        CASE "vea"
            IF max(9) > 0 THEN
                va = 0: DO: va = va + 1
                    IF va <> node THEN PUT #1, va, Veranstalter(va)
                LOOP UNTIL va = max(9)
                max(9) = max(9) - 1
            END IF
        CASE "kaz"
            IF max(10) > 0 THEN
                ka = 0: DO: ka = ka + 1
                    IF ka <> node THEN PUT #1, ka, Kleinanzeige(ka)
                LOOP UNTIL ka = max(10)
                max(10) = max(10) - 1
            END IF
        CASE "ver"
            IF max(11) > 0 THEN
                v = 0: DO: v = v + 1
                    IF v <> node THEN PUT #1, v, Veranstaltung(v)
                LOOP UNTIL v = max(11)
                max(11) = max(11) - 1
            END IF
        CASE "kat"
            IF max(12) > 0 THEN
                kt = 0: DO: kt = kt + 1
                    IF kt <> node THEN PUT #1, kt, Kategorie(kt)
                LOOP UNTIL kt = max(12)
                max(12) = max(12) - 1
            END IF
    END SELECT
    CLOSE #1
    load type$, ""
    endparameter$ = type$
    logThis "Erfolgreich gel" + CHR$(148) + "scht."
END SUB

FUNCTION readBinary (type$)
    logThis "Lesen von " + type$ + "."
    detRecLen type$
    IF _FILEEXISTS(netpath$ + "data\" + type$ + ".bremer") = -1 THEN
        OPEN netpath$ + "data\" + type$ + ".bremer" FOR RANDOM AS #1 LEN = recLEN
    ELSE
        OPEN netpath$ + "data\" + type$ + ".bremer" FOR RANDOM AS #1: recLEN = 0
    END IF
    IF recLEN > 0 AND LOF(1) <> 0 THEN
        SELECT CASE type$
            CASE "usr": u = 0: DO: u = u + 1: GET #1, u, User(u): LOOP UNTIL u = LOF(1) \ recLEN: max(1) = u
            CASE "obj": o = 0: DO: o = o + 1: GET #1, o, Objekt(o): LOOP UNTIL o = LOF(1) \ recLEN: max(2) = o
            CASE "asg": a = 0: DO: a = a + 1: GET #1, a, Ausgabe(a): LOOP UNTIL a = LOF(1) \ recLEN: max(3) = a
            CASE "ort": ot = 0: DO: ot = ot + 1: GET #1, ot, Ort(ot): LOOP UNTIL ot = LOF(1) \ recLEN: max(4) = ot
            CASE "plz": p = 0: DO: p = p + 1: GET #1, p, PLZ(p): LOOP UNTIL p = LOF(1) \ recLEN: max(5) = p
            CASE "adr": d = 0: DO: d = d + 1: GET #1, d, Adresse(d): LOOP UNTIL d = LOF(1) \ recLEN: max(6) = d
            CASE "knt": k = 0: DO: k = k + 1: GET #1, k, Konto(k): LOOP UNTIL k = LOF(1) \ recLEN: max(7) = k
            CASE "rbk": r = 0: DO: r = r + 1: GET #1, r, Rubrik(r): LOOP UNTIL r = LOF(1) \ recLEN: max(8) = r
            CASE "vea": va = 0: DO: va = va + 1: GET #1, va, Veranstalter(va): LOOP UNTIL va = LOF(1) \ recLEN: max(9) = va
            CASE "kaz": ka = 0: DO: ka = ka + 1: GET #1, ka, Kleinanzeige(ka): LOOP UNTIL ka = LOF(1) \ recLEN: max(10) = ka
            CASE "ver": v = 0: DO: v = v + 1: GET #1, v, Veranstaltung(v): LOOP UNTIL v = LOF(1) \ recLEN: max(11) = v
            CASE "kat": kt = 0: DO: kt = kt + 1: GET #1, kt, Kategorie(kt): LOOP UNTIL kt = LOF(1) \ recLEN: max(12) = kt
        END SELECT: CLOSE #1: clearobs type$: readBinary = 1
    ELSE: CLOSE #1: readBinary = 2
    END IF
END SUB

SUB detRecLen (type$)
    SELECT CASE type$
        CASE "usr": recLEN = LEN(User(0))
        CASE "obj": recLEN = LEN(Objekt(0))
        CASE "asg": recLEN = LEN(Ausgabe(0))
        CASE "ort": recLEN = LEN(Ort(0))
        CASE "plz": recLEN = LEN(PLZ(0))
        CASE "adr": recLEN = LEN(Adresse(0))
        CASE "knt": recLEN = LEN(Konto(0))
        CASE "rbk": recLEN = LEN(Rubrik(0))
        CASE "vea": recLEN = LEN(Veranstalter(0))
        CASE "kaz": recLEN = LEN(Kleinanzeige(0))
        CASE "ver": recLEN = LEN(Veranstaltung(0))
        CASE "kat": recLEN = LEN(Kategorie(0))
    END SELECT
END SUB

SUB SetArrayData (currentm, hardcodelist)
    SELECT CASE hardcodelist
        CASE 1 'Zugang
            arraydata$(currentm, 1) = "Administrator"
            arraydata$(currentm, 2) = "Buchhalter"
            arraydata$(currentm, 3) = "Mitarbeiter"
            arraydata$(currentm, 4) = "Gast"
            maxad(currentm) = 4
        CASE 2 'Abteilungen
            arraydata$(currentm, 1) = "Redaktion"
            arraydata$(currentm, 2) = "Grafik"
            arraydata$(currentm, 3) = "Akquise"
            arraydata$(currentm, 4) = "Marketing"
            arraydata$(currentm, 5) = "Management"
            maxad(currentm) = 5
        CASE 3 'Waehrungen
            arraydata$(currentm, 1) = "EUR"
            arraydata$(currentm, 2) = "USD"
            arraydata$(currentm, 3) = "JPY"
            arraydata$(currentm, 4) = "GBP"
            arraydata$(currentm, 5) = "CHF"
            arraydata$(currentm, 6) = "AUD"
            arraydata$(currentm, 7) = "PLN"
            arraydata$(currentm, 8) = "DKK"
            maxad(currentm) = 8
    END SELECT
END SUB

SUB logThis (text$)
    OPEN logfile$ FOR APPEND AS #999
    PRINT #999, text$
    CLOSE #999
END SUB

SUB checkmonth
    dontaddm = 0
    dontaddnm = 0
    month$ = MID$(DATE$, 7, 4) + MID$(DATE$, 1, 2)
    IF VAL(MID$(DATE$, 1, 2)) < 10 THEN
        nextmonth$ = MID$(DATE$, 7, 4) + "0" + LST$(VAL(MID$(DATE$, 1, 2)) + 1)
    ELSEIF VAL(MID$(DATE$, 1, 2)) < 12 THEN
        nextmonth$ = MID$(DATE$, 7, 4) + LST$(VAL(MID$(DATE$, 1, 2)) + 1)
    ELSE
        nextmonth$ = LST$(VAL(MID$(DATE$, 7, 4)) + 1) + "01"
    END IF
    IF max(3) > 0 THEN
        a = 0: DO: a = a + 1
            IF VAL(month$) = Ausgabe(a).Monat THEN
                dontaddm = 1
            END IF
            IF VAL(nextmonth$) = Ausgabe(a).Monat THEN
                dontaddnm = 1
            END IF
        LOOP UNTIL a = max(3)
    END IF
    IF dontaddm = 0 THEN
        max(3) = max(3) + 1
        node = max(3)
        IF max(3) > 1 THEN
            Ausgabe(node).ID = Ausgabe(node - 1).ID + 1
        ELSE
            Ausgabe(node).ID = 1
        END IF
        Ausgabe(node).Objekt = 1
        Ausgabe(node).Monat = VAL(month$)
        Ausgabe(node).Anfang = MID$(month$, 1, 4) + "-" + MID$(month$, 5, 2) + "-01"
        Ausgabe(node).Ende = MID$(month$, 1, 4) + "-" + MID$(month$, 5, 2) + "-" + LST$(maxday(VAL(MID$(month$, 1, 4)), VAL(MID$(month$, 5, 2))))
        writeBinary "asg"
        IF readBinary("asg") = 1 THEN
        END IF
    END IF
    IF dontaddnm = 0 THEN
        max(3) = max(3) + 1
        node = max(3)
        IF node > 1 THEN
            Ausgabe(node).ID = Ausgabe(node - 1).ID + 1
        ELSE
            Ausgabe(node).ID = 1
        END IF
        Ausgabe(node).Objekt = 1
        Ausgabe(node).Monat = VAL(nextmonth$)
        Ausgabe(node).Anfang = MID$(nextmonth$, 1, 4) + "-" + MID$(nextmonth$, 5, 2) + "-01"
        Ausgabe(node).Ende = MID$(nextmonth$, 1, 4) + "-" + MID$(nextmonth$, 5, 2) + "-" + LST$(maxday(VAL(MID$(nextmonth$, 1, 4)), VAL(MID$(nextmonth$, 5, 2))))
        writeBinary "asg"
        IF readBinary("asg") = 1 THEN
        END IF
    END IF
END SUB

SUB obscure (type$) 'only obscures data, not make it entirely inaccessible :D
    SELECT CASE type$
        CASE "usr"
            IF max(1) > 0 THEN
                u = 0: DO: u = u + 1
                    User(u).Name = Ost$(User(u).Name)
                    User(u).Profilbild = Ost$(User(u).Profilbild)
                    User(u).Passwort = Ost$(User(u).Passwort)
                    User(u).Zugang = Ova(User(u).Zugang)
                    User(u).Abteilung = Ost$(User(u).Abteilung)
                    User(u).Telefon = Ova(User(u).Telefon)
                    User(u).LetzterLogin = Ost$(User(u).LetzterLogin)
                LOOP UNTIL u = max(1)
            END IF
        CASE "obj"
            IF max(2) > 0 THEN
                o = 0: DO: o = o + 1
                    Objekt(o).ID = Ova(Objekt(o).ID)
                    Objekt(o).Name = Ost$(Objekt(o).Name)
                    Objekt(o).Ausgabenfrequenz = Ova(Objekt(o).Ausgabenfrequenz)
                    Objekt(o).Waehrung = Ost$(Objekt(o).Waehrung)
                LOOP UNTIL o = max(2)
            END IF
        CASE "asg"
            IF max(3) > 0 THEN
                a = 0: DO: a = a + 1
                    Ausgabe(a).ID = Ova(Ausgabe(a).ID)
                    Ausgabe(a).Objekt = Ova(Ausgabe(a).Objekt)
                    Ausgabe(a).Monat = Ova(Ausgabe(a).Monat)
                    Ausgabe(a).Anfang = Ost$(Ausgabe(a).Anfang)
                    Ausgabe(a).Ende = Ost$(Ausgabe(a).Ende)
                LOOP UNTIL a = max(3)
            END IF
        CASE "ort"
            IF max(4) > 0 THEN
                ot = 0: DO: ot = ot + 1
                    Ort(ot).Kuerzel = Ost$(Ort(ot).Kuerzel)
                    Ort(ot).Name = Ost$(Ort(ot).Name)
                LOOP UNTIL ot = max(4)
            END IF
        CASE "plz"
            IF max(5) > 0 THEN
                p = 0: DO: p = p + 1
                    PLZ(p).PLZ = Ova(PLZ(p).PLZ)
                    PLZ(p).Land = Ost$(PLZ(p).Land)
                    PLZ(p).Ort = Ost$(PLZ(p).Ort)
                LOOP UNTIL p = max(5)
            END IF
        CASE "adr"
            IF max(6) > 0 THEN
                d = 0: DO: d = d + 1
                    Adresse(d).ID = Ova(Adresse(d).ID)
                    Adresse(d).PLZ = Ova(Adresse(d).PLZ)
                    Adresse(d).Ort = Ost$(Adresse(d).Ort)
                    Adresse(d).Land = Ost$(Adresse(d).Land)
                    Adresse(d).Strasse = Ost$(Adresse(d).Strasse)
                LOOP UNTIL d = max(6)
            END IF
        CASE "knt"
            IF max(7) > 0 THEN
                k = 0: DO: k = k + 1
                    Konto(k).ID = Ova(Konto(k).ID)
                    Konto(k).BIC = Ost$(Konto(k).BIC)
                    Konto(k).IBAN = Ost$(Konto(k).IBAN)
                    Konto(k).Inhaber = Ost$(Konto(k).Inhaber)
                    Konto(k).Adresse = Ova(Konto(k).Adresse)
                LOOP UNTIL k = max(7)
            END IF
        CASE "rbk"
            IF max(8) > 0 THEN
                r = 0: DO: r = r + 1
                    Rubrik(r).Kuerzel = Ost$(Rubrik(r).Kuerzel)
                    Rubrik(r).Objekt = Ova(Rubrik(r).Objekt)
                    Rubrik(r).Name = Ost$(Rubrik(r).Name)
                LOOP UNTIL r = max(8)
            END IF
        CASE "vea"
            IF max(9) > 0 THEN
                va = 0: DO: va = va + 1
                    Veranstalter(va).Kuerzel = Ost$(Veranstalter(va).Kuerzel)
                    Veranstalter(va).Name = Ost$(Veranstalter(va).Name)
                    Veranstalter(va).Adresse = Ova(Veranstalter(va).Adresse)
                    Veranstalter(va).Telefon = Ova(Veranstalter(va).Telefon)
                    Veranstalter(va).Telefax = Ova(Veranstalter(va).Telefax)
                    Veranstalter(va).Sachbearbeiter = Ost$(Veranstalter(va).Sachbearbeiter)
                    Veranstalter(va).Anrede = Ost$(Veranstalter(va).Anrede)
                    Veranstalter(va).Notiz = Ost$(Veranstalter(va).Notiz)
                LOOP UNTIL va = max(9)
            END IF
        CASE "kaz"
            IF max(10) > 0 THEN
                ka = 0: DO: ka = ka + 1
                    Kleinanzeige(ka).ID = Ova(Kleinanzeige(ka).ID)
                    Kleinanzeige(ka).Kategorie1 = Ost$(Kleinanzeige(ka).Kategorie1)
                    Kleinanzeige(ka).Kategorie2 = Ost$(Kleinanzeige(ka).Kategorie2)
                    Kleinanzeige(ka).Kategorie3 = Ost$(Kleinanzeige(ka).Kategorie3)
                    Kleinanzeige(ka).Text = Ost$(Kleinanzeige(ka).Text)
                    Kleinanzeige(ka).Titel = Ost$(Kleinanzeige(ka).Titel)
                    Kleinanzeige(ka).Objekt = Ova(Kleinanzeige(ka).Objekt)
                    Kleinanzeige(ka).Ausgabe = Ova(Kleinanzeige(ka).Ausgabe)
                    Kleinanzeige(ka).Telefon = Ost$(Kleinanzeige(ka).Telefon)
                    Kleinanzeige(ka).Name = Ost$(Kleinanzeige(ka).Name)
                    Kleinanzeige(ka).Chiffre = Ost$(Kleinanzeige(ka).Chiffre)
                    Kleinanzeige(ka).Notiz = Ost$(Kleinanzeige(ka).Notiz)
                LOOP UNTIL ka = max(10)
            END IF
        CASE "ver"
            IF max(11) > 0 THEN
                v = 0: DO: v = v + 1
                    Veranstaltung(v).ID = Ova(Veranstaltung(v).ID)
                    Veranstaltung(v).Ausgabe = Ova(Veranstaltung(v).Ausgabe)
                    Veranstaltung(v).Datum = Ost$(Veranstaltung(v).Datum)
                    Veranstaltung(v).Ort = Ost$(Veranstaltung(v).Ort)
                    Veranstaltung(v).Veranstalter = Ost$(Veranstaltung(v).Veranstalter)
                    Veranstaltung(v).Rubrik = Ost$(Veranstaltung(v).Rubrik)
                    Veranstaltung(v).Zeit1 = Ost$(Veranstaltung(v).Zeit1)
                    Veranstaltung(v).Zeit2 = Ost$(Veranstaltung(v).Zeit2)
                    Veranstaltung(v).Zeit3 = Ost$(Veranstaltung(v).Zeit3)
                    Veranstaltung(v).Zeitcode = Ost$(Veranstaltung(v).Zeitcode)
                    Veranstaltung(v).Titel = Ost$(Veranstaltung(v).Titel)
                    Veranstaltung(v).Text = Ost$(Veranstaltung(v).Text)
                    Veranstaltung(v).TextLang = Ost$(Veranstaltung(v).TextLang)
                LOOP UNTIL v = max(11)
            END IF
        CASE "kat"
            IF max(12) > 0 THEN
                kt = 0: DO: kt = kt + 1
                    Kategorie(kt).Kuerzel = Ost$(Kategorie(kt).Kuerzel)
                    Kategorie(kt).Objekt = Ova(Kategorie(kt).Objekt)
                    Kategorie(kt).Name = Ost$(Kategorie(kt).Name)
                LOOP UNTIL kt = max(12)
            END IF
    END SELECT
END SUB

FUNCTION Ost$ (content$)
    Ost$ = _DEFLATE$(content$)
END FUNCTION

FUNCTION Ova (value)
    Ova = value
END FUNCTION

SUB clearobs (type$)
    SELECT CASE type$
        CASE "usr"
            IF max(1) > 0 THEN
                u = 0: DO: u = u + 1
                    User(u).Name = Cst$(User(u).Name)
                    User(u).Profilbild = Cst$(User(u).Profilbild)
                    User(u).Passwort = Cst$(User(u).Passwort)
                    User(u).Zugang = Cva(User(u).Zugang)
                    User(u).Abteilung = Cst$(User(u).Abteilung)
                    User(u).Telefon = Cva(User(u).Telefon)
                    User(u).LetzterLogin = Cst$(User(u).LetzterLogin)
                LOOP UNTIL u = max(1)
            END IF
        CASE "obj"
            IF max(2) > 0 THEN
                o = 0: DO: o = o + 1
                    Objekt(o).ID = Cva(Objekt(o).ID)
                    Objekt(o).Name = Cst$(Objekt(o).Name)
                    Objekt(o).Ausgabenfrequenz = Cva(Objekt(o).Ausgabenfrequenz)
                    Objekt(o).Waehrung = Cst$(Objekt(o).Waehrung)
                LOOP UNTIL o = max(2)
            END IF
        CASE "asg"
            IF max(3) > 0 THEN
                a = 0: DO: a = a + 1
                    Ausgabe(a).ID = Cva(Ausgabe(a).ID)
                    Ausgabe(a).Objekt = Cva(Ausgabe(a).Objekt)
                    Ausgabe(a).Monat = Cva(Ausgabe(a).Monat)
                    Ausgabe(a).Anfang = Cst$(Ausgabe(a).Anfang)
                    Ausgabe(a).Ende = Cst$(Ausgabe(a).Ende)
                LOOP UNTIL a = max(3)
            END IF
        CASE "ort"
            IF max(4) > 0 THEN
                ot = 0: DO: ot = ot + 1
                    Ort(ot).Kuerzel = Cst$(Ort(ot).Kuerzel)
                    Ort(ot).Name = Cst$(Ort(ot).Name)
                LOOP UNTIL ot = max(4)
            END IF
        CASE "plz"
            IF max(5) > 0 THEN
                p = 0: DO: p = p + 1
                    PLZ(p).PLZ = Cva(PLZ(p).PLZ)
                    PLZ(p).Land = Cst$(PLZ(p).Land)
                    PLZ(p).Ort = Cst$(PLZ(p).Ort)
                LOOP UNTIL p = max(5)
            END IF
        CASE "adr"
            IF max(6) > 0 THEN
                d = 0: DO: d = d + 1
                    Adresse(d).ID = Cva(Adresse(d).ID)
                    Adresse(d).PLZ = Cva(Adresse(d).PLZ)
                    Adresse(d).Ort = Cst$(Adresse(d).Ort)
                    Adresse(d).Land = Cst$(Adresse(d).Land)
                    Adresse(d).Strasse = Cst$(Adresse(d).Strasse)
                LOOP UNTIL d = max(6)
            END IF
        CASE "knt"
            IF max(7) > 0 THEN
                k = 0: DO: k = k + 1
                    Konto(k).ID = Cva(Konto(k).ID)
                    Konto(k).BIC = Cst$(Konto(k).BIC)
                    Konto(k).IBAN = Cst$(Konto(k).IBAN)
                    Konto(k).Inhaber = Cst$(Konto(k).Inhaber)
                    Konto(k).Adresse = Cva(Konto(k).Adresse)
                LOOP UNTIL k = max(7)
            END IF
        CASE "rbk"
            IF max(8) > 0 THEN
                r = 0: DO: r = r + 1
                    Rubrik(r).Kuerzel = Cst$(Rubrik(r).Kuerzel)
                    Rubrik(r).Objekt = Cva(Rubrik(r).Objekt)
                    Rubrik(r).Name = Cst$(Rubrik(r).Name)
                LOOP UNTIL r = max(8)
            END IF
        CASE "vea"
            IF max(9) > 0 THEN
                va = 0: DO: va = va + 1
                    Veranstalter(va).Kuerzel = Cst$(Veranstalter(va).Kuerzel)
                    Veranstalter(va).Name = Cst$(Veranstalter(va).Name)
                    Veranstalter(va).Adresse = Cva(Veranstalter(va).Adresse)
                    Veranstalter(va).Telefon = Cva(Veranstalter(va).Telefon)
                    Veranstalter(va).Telefax = Cva(Veranstalter(va).Telefax)
                    Veranstalter(va).Sachbearbeiter = Cst$(Veranstalter(va).Sachbearbeiter)
                    Veranstalter(va).Anrede = Cst$(Veranstalter(va).Anrede)
                    Veranstalter(va).Notiz = Cst$(Veranstalter(va).Notiz)
                LOOP UNTIL va = max(9)
            END IF
        CASE "kaz"
            IF max(10) > 0 THEN
                ka = 0: DO: ka = ka + 1
                    Kleinanzeige(ka).ID = Cva(Kleinanzeige(ka).ID)
                    Kleinanzeige(ka).Kategorie1 = Cst$(Kleinanzeige(ka).Kategorie1)
                    Kleinanzeige(ka).Kategorie2 = Cst$(Kleinanzeige(ka).Kategorie2)
                    Kleinanzeige(ka).Kategorie3 = Cst$(Kleinanzeige(ka).Kategorie3)
                    Kleinanzeige(ka).Text = Cst$(Kleinanzeige(ka).Text)
                    Kleinanzeige(ka).Titel = Cst$(Kleinanzeige(ka).Titel)
                    Kleinanzeige(ka).Objekt = Cva(Kleinanzeige(ka).Objekt)
                    Kleinanzeige(ka).Ausgabe = Cva(Kleinanzeige(ka).Ausgabe)
                    Kleinanzeige(ka).Telefon = Cst$(Kleinanzeige(ka).Telefon)
                    Kleinanzeige(ka).Name = Cst$(Kleinanzeige(ka).Name)
                    Kleinanzeige(ka).Chiffre = Cst$(Kleinanzeige(ka).Chiffre)
                    Kleinanzeige(ka).Notiz = Cst$(Kleinanzeige(ka).Notiz)
                LOOP UNTIL ka = max(10)
            END IF
        CASE "ver"
            IF max(11) > 0 THEN
                v = 0: DO: v = v + 1
                    Veranstaltung(v).ID = Cva(Veranstaltung(v).ID)
                    Veranstaltung(v).Ausgabe = Cva(Veranstaltung(v).Ausgabe)
                    Veranstaltung(v).Datum = Cst$(Veranstaltung(v).Datum)
                    Veranstaltung(v).Ort = Cst$(Veranstaltung(v).Ort)
                    Veranstaltung(v).Veranstalter = Cst$(Veranstaltung(v).Veranstalter)
                    Veranstaltung(v).Rubrik = Cst$(Veranstaltung(v).Rubrik)
                    Veranstaltung(v).Zeit1 = Cst$(Veranstaltung(v).Zeit1)
                    Veranstaltung(v).Zeit2 = Cst$(Veranstaltung(v).Zeit2)
                    Veranstaltung(v).Zeit3 = Cst$(Veranstaltung(v).Zeit3)
                    Veranstaltung(v).Zeitcode = Cst$(Veranstaltung(v).Zeitcode)
                    Veranstaltung(v).Titel = Cst$(Veranstaltung(v).Titel)
                    Veranstaltung(v).Text = Cst$(Veranstaltung(v).Text)
                    Veranstaltung(v).TextLang = Cst$(Veranstaltung(v).TextLang)
                LOOP UNTIL v = max(11)
            END IF
        CASE "kat"
            IF max(12) > 0 THEN
                kt = 0: DO: kt = kt + 1
                    Kategorie(kt).Kuerzel = Cst$(Kategorie(kt).Kuerzel)
                    Kategorie(kt).Objekt = Cva(Kategorie(kt).Objekt)
                    Kategorie(kt).Name = Cst$(Kategorie(kt).Name)
                LOOP UNTIL kt = max(12)
            END IF
    END SELECT
END SUB

FUNCTION Cst$ (content$)
    Cst$ = _INFLATE$(content$)
END FUNCTION

FUNCTION Cva (value)
    Cva = value
END FUNCTION

FUNCTION cutcontent$ (text$)
    p = 0: DO: p = p + 1
        IF MID$(text$, p, 1) = ":" THEN
            cutcontent$ = LTRIM$(MID$(text$, p + 1, LEN(text$) - p + 1))
            EXIT FUNCTION
        END IF
    LOOP UNTIL p = LEN(text$)
    cutcontent$ = ""
END FUNCTION

'the following two SUBs are by bplus
'drwString needs sub RotoZoom2, intended for graphics screens using the default font.
'S$ is the string to display
'c is the color (will have a transparent background)
'midX and midY is the center of where you want to display the string
'xScale would multiply 8 pixel width of default font
'yScale would multiply the 16 pixel height of the default font
'Rotation is in Radian units, use _D2R to convert Degree units to Radian units
SUB drwString (S$, c AS _UNSIGNED LONG, midX, midY, xScale, yScale, Rotation)
    I& = _NEWIMAGE(8 * LEN(S$), 16, 32)
    _DEST I&
    COLOR c, _RGBA32(0, 0, 0, 0)
    _PRINTSTRING (0, 0), S$
    _DEST 0
    RotoZoom2 midX, midY, I&, xScale, yScale, Rotation
    _FREEIMAGE I&
END SUB

'This sub gives really nice control over displaying an Image.
SUB RotoZoom2 (centerX AS LONG, centerY AS LONG, Image AS LONG, xScale AS SINGLE, yScale AS SINGLE, Rotation AS SINGLE)
    DIM px(3) AS SINGLE: DIM py(3) AS SINGLE
    W& = _WIDTH(Image&): h& = _HEIGHT(Image&)
    px(0) = -W& / 2: py(0) = -h& / 2: px(1) = -W& / 2: py(1) = h& / 2
    px(2) = W& / 2: py(2) = h& / 2: px(3) = W& / 2: py(3) = -h& / 2
    sinr! = SIN(-Rotation): cosr! = COS(-Rotation)
    FOR i& = 0 TO 3
        x2& = (px(i&) * cosr! + sinr! * py(i&)) * xScale + centerX: y2& = (py(i&) * cosr! - px(i&) * sinr!) * yScale + centerY
        px(i&) = x2&: py(i&) = y2&
    NEXT
    _MAPTRIANGLE (0, 0)-(0, h& - 1)-(W& - 1, h& - 1), Image& TO(px(0), py(0))-(px(1), py(1))-(px(2), py(2))
    _MAPTRIANGLE (0, 0)-(W& - 1, 0)-(W& - 1, h& - 1), Image& TO(px(0), py(0))-(px(3), py(3))-(px(2), py(2))
END SUB

SUB SetWindowOpacity (hWnd AS LONG, Level)
    DIM Msg AS LONG
    CONST G = -20
    CONST LWA_ALPHA = &H2
    CONST WS_EX_LAYERED = &H80000
    Msg = GetWindowLong(hWnd, G)
    Msg = Msg OR WS_EX_LAYERED
    Crap = SetWindowLong(hWnd, G, Msg)
    Crap = SetLayeredWindowAttributes(hWnd, 0, Level, LWA_ALPHA)
END SUB

SUB resetToStandard (type$)
    logThis "Setze " + type$ + " auf Standardwerte zurueck..."
    SELECT CASE type$
        CASE "usr"
            u = 1: max(1) = 1
            User(1).Name = "Alex": User(1).Profilbild = "Alex.png": User(1).Passwort = "5642": User(1).Zugang = 1: User(1).Abteilung = "Management": User(1).Telefon = 4915162842083: User(1).LetzterLogin = "07-10-2470@00:50:00"
            pfp& = Robohash(User(1).Name)
            Result = SaveImage(netpath$ + "data\pfp\" + User(1).Name + ".png", pfp&, 0, 0, _WIDTH(pfp&), _HEIGHT(pfp&))
            IF Result = 1 THEN 'file already found on drive
                KILL exportimage2$ 'delete the old file
                Result = SaveImage(exportimage2$, 0, 0, 0, _WIDTH, _HEIGHT) 'save the new one again
            END IF
            _FREEIMAGE pfp&
        CASE "obj"
            o = 1: max(2) = 1
            Objekt(1).ID = 1: Objekt(1).Name = "BREMER": Objekt(1).Ausgabenfrequenz = 12: Objekt(1).Waehrung = "EUR"
        CASE "asg"
            a = 0: max(3) = a
        CASE "ort"
            ot = 29: max(4) = 29
            Ort(1).Kuerzel = "ACH": Ort(1).Name = "Achim"
            Ort(2).Kuerzel = "AUR": Ort(2).Name = "Aurich"
            Ort(3).Kuerzel = "BHV": Ort(3).Name = "Bremerhaven"
            Ort(4).Kuerzel = "CLP": Ort(4).Name = "Cloppenburg"
            Ort(5).Kuerzel = "EMD": Ort(5).Name = "Emden"
            Ort(6).Kuerzel = "GAN": Ort(6).Name = "Ganderkesee"
            Ort(7).Kuerzel = "GRA": Ort(7).Name = "Grasberg"
            Ort(8).Kuerzel = "H": Ort(8).Name = "Hannover"
            Ort(9).Kuerzel = "HB": Ort(9).Name = "Bremen"
            Ort(10).Kuerzel = "LEM": Ort(10).Name = "Lemwerder"
            Ort(11).Kuerzel = "LIL": Ort(11).Name = "Lilienthal"
            Ort(12).Kuerzel = "NDM": Ort(12).Name = "Nordenham"
            Ort(13).Kuerzel = "NI": Ort(13).Name = "Nienburg"
            Ort(14).Kuerzel = "OHZ": Ort(14).Name = "Osterholz-Scharmb."
            Ort(15).Kuerzel = "OL": Ort(15).Name = "Oldenburg"
            Ort(16).Kuerzel = "OS": Ort(16).Name = "Osnabruck"
            Ort(17).Kuerzel = "OYT": Ort(17).Name = "Oyten"
            Ort(18).Kuerzel = "SOR": Ort(18).Name = "Schortens"
            Ort(19).Kuerzel = "STU": Ort(19).Name = "Stuhr"
            Ort(20).Kuerzel = "SUL": Ort(20).Name = "Sulingen"
            Ort(21).Kuerzel = "VER": Ort(21).Name = "Verden"
            Ort(22).Kuerzel = "VIS": Ort(22).Name = "Visselhovede"
            Ort(23).Kuerzel = "WDH": Ort(23).Name = "Wildeshausen"
            Ort(24).Kuerzel = "WEY": Ort(24).Name = "Weyhe"
            Ort(25).Kuerzel = "WHV": Ort(25).Name = "Wilhelmshaven"
            Ort(26).Kuerzel = "WPS": Ort(26).Name = "Worpswede"
            Ort(27).Kuerzel = "WST": Ort(27).Name = "Westerstede"
            Ort(28).Kuerzel = "ZET": Ort(28).Name = "Zetel"
            Ort(29).Kuerzel = "ZEV": Ort(29).Name = "Zeven"
        CASE "plz"
            p = 36: max(5) = 36
            PLZ(1).PLZ = 28195: PLZ(1).Land = "Deutschland": PLZ(1).Ort = "Bremen"
            PLZ(2).PLZ = 28203: PLZ(2).Land = "Deutschland": PLZ(2).Ort = "Bremen"
            PLZ(3).PLZ = 28211: PLZ(3).Land = "Deutschland": PLZ(3).Ort = "Bremen"
            PLZ(4).PLZ = 28219: PLZ(4).Land = "Deutschland": PLZ(4).Ort = "Bremen"
            PLZ(5).PLZ = 28277: PLZ(5).Land = "Deutschland": PLZ(5).Ort = "Bremen"
            PLZ(6).PLZ = 28325: PLZ(6).Land = "Deutschland": PLZ(6).Ort = "Bremen"
            PLZ(7).PLZ = 28357: PLZ(7).Land = "Deutschland": PLZ(7).Ort = "Bremen"
            PLZ(8).PLZ = 28719: PLZ(8).Land = "Deutschland": PLZ(8).Ort = "Bremen"
            PLZ(9).PLZ = 28777: PLZ(9).Land = "Deutschland": PLZ(9).Ort = "Bremen"
            PLZ(10).PLZ = 28197: PLZ(10).Land = "Deutschland": PLZ(10).Ort = "Bremen"
            PLZ(11).PLZ = 28205: PLZ(11).Land = "Deutschland": PLZ(11).Ort = "Bremen"
            PLZ(12).PLZ = 28213: PLZ(12).Land = "Deutschland": PLZ(12).Ort = "Bremen"
            PLZ(13).PLZ = 28237: PLZ(13).Land = "Deutschland": PLZ(13).Ort = "Bremen"
            PLZ(14).PLZ = 28279: PLZ(14).Land = "Deutschland": PLZ(14).Ort = "Bremen"
            PLZ(15).PLZ = 28327: PLZ(15).Land = "Deutschland": PLZ(15).Ort = "Bremen"
            PLZ(16).PLZ = 28359: PLZ(16).Land = "Deutschland": PLZ(16).Ort = "Bremen"
            PLZ(17).PLZ = 28755: PLZ(17).Land = "Deutschland": PLZ(17).Ort = "Bremen"
            PLZ(18).PLZ = 28779: PLZ(18).Land = "Deutschland": PLZ(18).Ort = "Bremen"
            PLZ(19).PLZ = 28199: PLZ(19).Land = "Deutschland": PLZ(19).Ort = "Bremen"
            PLZ(20).PLZ = 28207: PLZ(20).Land = "Deutschland": PLZ(20).Ort = "Bremen"
            PLZ(21).PLZ = 28215: PLZ(21).Land = "Deutschland": PLZ(21).Ort = "Bremen"
            PLZ(22).PLZ = 28239: PLZ(22).Land = "Deutschland": PLZ(22).Ort = "Bremen"
            PLZ(23).PLZ = 28307: PLZ(23).Land = "Deutschland": PLZ(23).Ort = "Bremen"
            PLZ(24).PLZ = 28329: PLZ(24).Land = "Deutschland": PLZ(24).Ort = "Bremen"
            PLZ(25).PLZ = 28717: PLZ(25).Land = "Deutschland": PLZ(25).Ort = "Bremen"
            PLZ(26).PLZ = 28757: PLZ(26).Land = "Deutschland": PLZ(26).Ort = "Bremen"
            PLZ(27).PLZ = 28201: PLZ(27).Land = "Deutschland": PLZ(27).Ort = "Bremen"
            PLZ(28).PLZ = 28209: PLZ(28).Land = "Deutschland": PLZ(28).Ort = "Bremen"
            PLZ(29).PLZ = 28217: PLZ(29).Land = "Deutschland": PLZ(29).Ort = "Bremen"
            PLZ(30).PLZ = 28259: PLZ(30).Land = "Deutschland": PLZ(30).Ort = "Bremen"
            PLZ(31).PLZ = 28309: PLZ(31).Land = "Deutschland": PLZ(31).Ort = "Bremen"
            PLZ(32).PLZ = 28355: PLZ(32).Land = "Deutschland": PLZ(32).Ort = "Bremen"
            PLZ(33).PLZ = 28718: PLZ(33).Land = "Deutschland": PLZ(33).Ort = "Bremen"
            PLZ(34).PLZ = 28759: PLZ(34).Land = "Deutschland": PLZ(34).Ort = "Bremen"
            PLZ(35).PLZ = 28865: PLZ(35).Land = "Deutschland": PLZ(35).Ort = "Lilienthal"
            PLZ(36).PLZ = 27578: PLZ(36).Land = "Deutschland": PLZ(36).Ort = "Bremerhaven"
        CASE "adr"
            d = 7: max(6) = d
            Adresse(1).ID = 1: Adresse(1).PLZ = 28195: Adresse(1).Ort = "Bremen": Adresse(1).Land = "Deutschland": Adresse(1).Strasse = "Altenwall 9"
            Adresse(2).ID = 2: Adresse(2).PLZ = 28865: Adresse(2).Ort = "Lilienthal": Adresse(2).Land = "Deutschland": Adresse(2).Strasse = "Klosterstr. 21"
            Adresse(3).ID = 3: Adresse(3).PLZ = 28195: Adresse(3).Ort = "Bremen": Adresse(3).Land = "Deutschland": Adresse(3).Strasse = "Buergerstr. 1"
            Adresse(4).ID = 4: Adresse(4).PLZ = 28259: Adresse(4).Ort = "Bremen": Adresse(4).Land = "Deutschland": Adresse(4).Strasse = "Huchtinger Heerstr. 6"
            Adresse(5).ID = 5: Adresse(5).PLZ = 28203: Adresse(5).Ort = "Bremen": Adresse(5).Land = "Deutschland": Adresse(5).Strasse = "Sonnenstr. 8"
            Adresse(6).ID = 6: Adresse(6).PLZ = 27578: Adresse(6).Ort = "Bremerhaven": Adresse(6).Land = "Deutschland": Adresse(6).Strasse = "Mecklenburger Weg 180"
            Adresse(7).ID = 7: Adresse(7).PLZ = 27576: Adresse(7).Ort = "Bremerhaven": Adresse(7).Land = "Deutschland": Adresse(7).Strasse = "Eupener Str. 36"
        CASE "knt"
            k = 1: max(7) = k
            Konto(1).ID = 1: Konto(1).BIC = "SBRE DE22": Konto(1).IBAN = "DE88 2905 0101 0001 0676 85": Konto(1).Inhaber = "BREMER Blatt Verlags GmbH": Konto(1).Adresse = 1
        CASE "rbk"
            r = 16: max(8) = r
            Rubrik(1).Kuerzel = "MU": Rubrik(1).Objekt = 1: Rubrik(1).Name = "Musik"
            Rubrik(2).Kuerzel = "KL": Rubrik(2).Objekt = 1: Rubrik(2).Name = "Klassik"
            Rubrik(3).Kuerzel = "PT": Rubrik(3).Objekt = 1: Rubrik(3).Name = "Party/Tanz"
            Rubrik(4).Kuerzel = "GL": Rubrik(4).Objekt = 1: Rubrik(4).Name = "Gay/Lesbian"
            Rubrik(5).Kuerzel = "FI": Rubrik(5).Objekt = 1: Rubrik(5).Name = "Film"
            Rubrik(6).Kuerzel = "CK": Rubrik(6).Objekt = 1: Rubrik(6).Name = "Comedy/Kabarett"
            Rubrik(7).Kuerzel = "TH": Rubrik(7).Objekt = 1: Rubrik(7).Name = "Theater"
            Rubrik(8).Kuerzel = "SP": Rubrik(8).Objekt = 1: Rubrik(8).Name = "Sport"
            Rubrik(9).Kuerzel = "KU": Rubrik(9).Objekt = 1: Rubrik(9).Name = "Kunst"
            Rubrik(10).Kuerzel = "KI": Rubrik(10).Objekt = 1: Rubrik(10).Name = "Kinder"
            Rubrik(11).Kuerzel = "DI": Rubrik(11).Objekt = 1: Rubrik(11).Name = "Diverses"
            Rubrik(12).Kuerzel = "FU": Rubrik(12).Objekt = 1: Rubrik(12).Name = "F" + CHR$(129) + "hrungen"
            Rubrik(13).Kuerzel = "TR": Rubrik(13).Objekt = 1: Rubrik(13).Name = "Treffs"
            Rubrik(14).Kuerzel = "FL": Rubrik(14).Objekt = 1: Rubrik(14).Name = "Flohmarkt"
            Rubrik(15).Kuerzel = "LE": Rubrik(15).Objekt = 1: Rubrik(15).Name = "Lesung"
            Rubrik(16).Kuerzel = "VO": Rubrik(16).Objekt = 1: Rubrik(16).Name = "Vortrag"
        CASE "vea"
            va = 7: max(9) = va
            Veranstalter(1).Name = ""
            Veranstalter(2).Kuerzel = "XAA": Veranstalter(2).Name = "Altes Amtsgericht": Veranstalter(2).Adresse = 2: Veranstalter(1).Telefon = 4298929180
            Veranstalter(3).Kuerzel = "XAK": Veranstalter(3).Name = "Kultursaal Arbeitnehmerkammer": Veranstalter(3).Adresse = 3: Veranstalter(2).Telefon = 421363010
            Veranstalter(4).Kuerzel = "XAO": Veranstalter(4).Name = "Altes Ortsamt": Veranstalter(4).Adresse = 4
            Veranstalter(5).Kuerzel = "XBF": Veranstalter(5).Name = "Belladonna": Veranstalter(5).Adresse = 5
            Veranstalter(6).Kuerzel = "XBFO": Veranstalter(6).Name = "FZH-Folk-Treff": Veranstalter(6).Adresse = 6
            Veranstalter(7).Kuerzel = "XBL": Veranstalter(7).Name = "Lehe-Treff": Veranstalter(7).Adresse = 7
        CASE "kaz"
            ka = 0: max(10) = 0
        CASE "ver"
            v = 0: max(11) = 0
        CASE "kat"
            kt = 24: max(12) = kt
            Kategorie(1).Kuerzel = "KW": Kategorie(1).Objekt = 1: Kategorie(1).Name = "Kurse/Workshops/Seminare"
            Kategorie(2).Kuerzel = "UW": Kategorie(2).Objekt = 1: Kategorie(2).Name = "Unterricht/Weiterbildung"
            Kategorie(3).Kuerzel = "PG": Kategorie(3).Objekt = 1: Kategorie(3).Name = "Psyche & Gesundheit"
            Kategorie(4).Kuerzel = "SA": Kategorie(4).Objekt = 1: Kategorie(4).Name = "Sonstige Aktivitaeten"
            Kategorie(5).Kuerzel = "FM": Kategorie(5).Objekt = 1: Kategorie(5).Name = "Frau sucht Mann"
            Kategorie(6).Kuerzel = "MF": Kategorie(6).Objekt = 1: Kategorie(6).Name = "Mann sucht Frau"
            Kategorie(7).Kuerzel = "FF": Kategorie(7).Objekt = 1: Kategorie(7).Name = "Frau sucht Frau"
            Kategorie(8).Kuerzel = "MM": Kategorie(8).Objekt = 1: Kategorie(8).Name = "Mann sucht Mann"
            Kategorie(9).Kuerzel = "LG": Kategorie(9).Objekt = 1: Kategorie(9).Name = "Leute gesucht"
            Kategorie(10).Kuerzel = "LL": Kategorie(10).Objekt = 1: Kategorie(10).Name = "Lust und Laune"
            Kategorie(11).Kuerzel = "GR": Kategorie(11).Objekt = 1: Kategorie(11).Name = "Gruesse"
            Kategorie(12).Kuerzel = "SK": Kategorie(12).Objekt = 1: Kategorie(12).Name = "Sonstige Kontakte"
            Kategorie(13).Kuerzel = "MG": Kategorie(13).Objekt = 1: Kategorie(13).Name = "Mietgesuche"
            Kategorie(14).Kuerzel = "SJ": Kategorie(14).Objekt = 1: Kategorie(14).Name = "Stellen- & Jobangebote"
            Kategorie(15).Kuerzel = "SG": Kategorie(15).Objekt = 1: Kategorie(15).Name = "Stellen- & Jobgesuche"
            Kategorie(16).Kuerzel = "HA": Kategorie(16).Objekt = 1: Kategorie(16).Name = "Handwerkliches Arbeiten"
            Kategorie(17).Kuerzel = "FV": Kategorie(17).Objekt = 1: Kategorie(17).Name = "Fahrzeug-Verleih"
            Kategorie(18).Kuerzel = "SN": Kategorie(18).Objekt = 1: Kategorie(18).Name = "Sonstiger Ankauf"
            Kategorie(19).Kuerzel = "SV": Kategorie(19).Objekt = 1: Kategorie(19).Name = "Sonstiger Verkauf"
            Kategorie(20).Kuerzel = "va": Kategorie(20).Objekt = 1: Kategorie(20).Name = "Veranstaltungen"
            Kategorie(21).Kuerzel = "BA": Kategorie(21).Objekt = 1: Kategorie(21).Name = "Bands"
            Kategorie(22).Kuerzel = "PF": Kategorie(22).Objekt = 1: Kategorie(22).Name = "Pauschal/Fluege"
            Kategorie(23).Kuerzel = "FW": Kategorie(23).Objekt = 1: Kategorie(23).Name = "Ferienwohnungen"
            Kategorie(24).Kuerzel = "VS": Kategorie(24).Objekt = 1: Kategorie(24).Name = "Verschiedenes"
    END SELECT
    logThis type$ + " erfolgreich auf Standardwerte zurueckgesetzt."
END SUB

FUNCTION Robohash& (stringhash AS STRING)
    logThis "Generiere Profilbild fuer " + stringhash + "..."
    DIM URL AS STRING
    DIM URLFile AS STRING
    DIM a%
    URLFile = stringhash + ".png"
    URL = "https://robohash.org/" + stringhash
    a% = API_request(URL, URLFile)
    DIM U AS INTEGER
    U = FREEFILE
    OPEN URLFile FOR BINARY AS #U
    IF LOF(U) <> 0 THEN
        Robohash = _LOADIMAGE(URLFile, 32)
    ELSE
        CLOSE #U
        KILL URLFile
        Robohash = 0
    END IF
    CLOSE #U
    KILL URLFile
    logThis "Profilbild erfolgreich generiert."
END FUNCTION

DECLARE DYNAMIC LIBRARY "urlmon"
    FUNCTION URLDownloadToFileA (BYVAL pCaller AS LONG, szURL AS STRING, szFileName AS STRING, BYVAL dwReserved AS LONG, BYVAL lpfnCB AS LONG)
END DECLARE

FUNCTION API_request (URL AS STRING, File AS STRING)
    API_request = URLDownloadToFileA(0, URL + CHR$(0), File + CHR$(0), 0, 0)
END FUNCTION

REM $INCLUDE:'code/SaveImage.BM'

SUB newgcolor (gradient, gcolor, gpos, col&)
    IF gcolor > 0 AND gcolor <= maxcolors THEN
        gcol&(gradient, gcolor) = col&
        gpos(gradient, gcolor) = gpos
    END IF
END SUB

SUB newgradient (gradient, maxgrcolors)
    IF gradient > 0 AND gradient <= maxgradients AND maxgrcolors > 0 AND maxgrcolors <= maxcolors THEN
        maxgcol&(gradient) = maxgrcolors
    END IF
END SUB

FUNCTION gradientcolor& (gradient, grposition)
    IF maxgcol&(gradient) > 0 THEN
        grcolor = 0: DO: grcolor = grcolor + 1
            IF grposition = gpos(gradient, grcolor) THEN
                gradientcolor& = gcol&(gradient, grcolor)
                EXIT FUNCTION
            ELSE
                IF grcolor < maxgcol&(gradient) THEN
                    IF grposition > gpos(gradient, grcolor) AND grposition < gpos(gradient, grcolor + 1) THEN
                        r1 = _RED(gcol&(gradient, grcolor))
                        g1 = _GREEN(gcol&(gradient, grcolor))
                        b1 = _BLUE(gcol&(gradient, grcolor))
                        a1 = _ALPHA(gcol&(gradient, grcolor))
                        r2 = _RED(gcol&(gradient, grcolor + 1))
                        g2 = _GREEN(gcol&(gradient, grcolor + 1))
                        b2 = _BLUE(gcol&(gradient, grcolor + 1))
                        a2 = _ALPHA(gcol&(gradient, grcolor + 1))
                        p1 = gpos(gradient, grcolor)
                        p2 = gpos(gradient, grcolor + 1)
                        f = (grposition - p1) / (p2 - p1)
                        IF r1 > r2 THEN
                            r = r1 - ((r1 - r2) * f)
                        ELSEIF r1 = r2 THEN
                            r = r1
                        ELSE
                            r = r1 + ((r2 - r1) * f)
                        END IF
                        IF g1 > g2 THEN
                            g = g1 - ((g1 - g2) * f)
                        ELSEIF g1 = g2 THEN
                            g = g1
                        ELSE
                            g = g1 + ((g2 - g1) * f)
                        END IF
                        IF b1 > b2 THEN
                            b = b1 - ((b1 - b2) * f)
                        ELSEIF b1 = b2 THEN
                            b = b1
                        ELSE
                            b = b1 + ((b2 - b1) * f)
                        END IF
                        IF a1 > a2 THEN
                            a = a1 - ((a1 - a2) * f)
                        ELSEIF a1 = a2 THEN
                            a = a1
                        ELSE
                            a = a1 + ((a2 - a1) * f)
                        END IF
                        gradientcolor& = _RGBA(INT(r), INT(g), INT(b), INT(a))
                        EXIT FUNCTION
                    END IF
                END IF
            END IF
        LOOP UNTIL grcolor = maxgcol&(gradient)
    ELSE
        gradientcolor& = _RGBA(0, 0, 0, 0)
        EXIT FUNCTION
    END IF
END FUNCTION

SUB drawgradient (gradient, lx, ux, ly, uy, orientation$)
    SELECT CASE orientation$
        CASE "h"
            IF ux - lx > 0 THEN
                rx = 0: DO: rx = rx + 1
                    LINE (lx + rx, ly)-(lx + rx, uy), gradientcolor&(gradient, rx / (ux - lx) * 100)
                LOOP UNTIL rx >= ux - lx
            END IF
        CASE "v"
            IF uy - ly > 0 THEN
                ry = 0: DO: ry = ry + 1
                    LINE (lx, ly + ry)-(ux, ly + ry), gradientcolor&(gradient, ry / (uy - ly) * 100)
                LOOP UNTIL ry >= uy - ly
            END IF
    END SELECT
END SUB

FUNCTION colour& (color$)
    SELECT CASE color$
        CASE "bg"
            IF darkmode = 1 THEN colour& = colour&("black") ELSE colour& = colour&("white")
        CASE "fg"
            IF darkmode = 1 THEN colour& = colour&("white") ELSE colour& = colour&("black")
        CASE "offfocus"
            IF darkmode = 1 THEN colour& = colour&("light grey") ELSE colour& = colour&("dark grey")
        CASE "white"
            colour& = _RGBA(220, 220, 220, 255)
        CASE "black"
            colour& = _RGBA(15, 15, 15, 255)
        CASE "red"
            colour& = _RGBA(255, 30, 30, 255)
        CASE "yellow"
            colour& = _RGBA(249, 194, 0, 255)
        CASE "green"
            colour& = _RGBA(94, 233, 61, 255)
        CASE "dark grey"
            colour& = _RGBA(50, 50, 50, 255)
        CASE "light grey"
            colour& = _RGBA(170, 170, 170, 255)
        CASE "transparent"
            colour& = _RGBA(0, 0, 0, 0)
    END SELECT
END SUB
