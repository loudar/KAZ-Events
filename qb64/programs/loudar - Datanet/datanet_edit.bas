CLS
CLEAR
CLOSE

'variables
'screens
c% = _LOADIMAGE("data\bg\console.jpg", 32)
v% = _LOADIMAGE("data\bg\viewdata.jpg", 32)
n% = _LOADIMAGE("data\bg\newpoint.jpg", 32)
e% = _LOADIMAGE("data\bg\editpoint.jpg", 32)
DIM SHARED maxx
DIM SHARED maxy
maxx = (_DESKTOPWIDTH / 2) * 1.1
maxy = maxx / 16 * 9
logox = maxx / 3
logoy = ((365 / 1920) * logox) + 120 - (40 / 3)
SCREEN _NEWIMAGE(maxx, maxy, 32)
PAINT (maxx / 2, maxy / 2), _RGBA(237, 237, 237, 255)

DO: LOOP UNTIL _SCREENEXISTS
_TITLE "DATANET"
_SCREENMOVE (_DESKTOPWIDTH / 2) - (maxx / 2), (_DESKTOPHEIGHT / 2) - (maxy / 2)

'================================================================================================================ BASIC STUFF ======================================================================================

'base arrays
DIM SHARED instanceID$(20000)

'text
'text arrays
DIM SHARED text$(50)
DIM SHARED char$(50, 3000)
DIM SHARED editpos(50)
DIM SHARED g(50)
DIM SHARED gbf(50)
DIM SHARED UserInput$(50)
'loads allowed characters (ac) from file
DIM SHARED ac$(100)
DIM SHARED alch
'font
DIM SHARED fontheight
DIM SHARED fontwidth
DIM SHARED firstline
DIM SHARED xcharacter
DIM SHARED xoffset(100)
fontfiler$ = "data\bg\aldrich-r.ttf"
fontheight = 16
r& = _LOADFONT(fontfiler$, fontheight, "MONOSPACE")
_FONT r&
fontwidth = _FONTWIDTH(r&)
maxrows = INT(maxx / fontwidth)
maxlines = INT(maxy / fontheight) - 4
firstline = INT(120 / fontheight)
xcharacter = INT((maxx / 2.7) / fontwidth)
COLOR _RGBA(15, 15, 15, 255), _RGBA(237, 237, 237, 255)

'menu (alphabetically sorted)
DIM SHARED acceptinput(50)
DIM SHARED backspace(50)
DIM SHARED basex(50)
DIM SHARED categories(50)
DIM SHARED category$(50, 20)
DIM SHARED defaultstyle(50)
DIM SHARED destination$(50)
DIM SHARED endx(50)
DIM SHARED endy(50)
DIM SHARED expanded(50)
DIM SHARED file$(50)
DIM SHARED fileshort$(50)
DIM SHARED fill$(50)
DIM SHARED firstprint(50)
DIM SHARED kind$(50)
DIM SHARED linetbp(50)
DIM SHARED maxcatlength(50)
DIM SHARED maxg(50)
DIM SHARED maxval(50)
DIM SHARED menu$(50, 50)
DIM SHARED menufile$(50)
DIM SHARED minval(50)
DIM SHARED overlaptrigger(50)
DIM SHARED totalcat(50)
DIM SHARED trigger(50)
DIM SHARED type$(50)
DIM SHARED selected(50)
DIM SHARED status$(50)
DIM SHARED style(50)
DIM SHARED subcategory$(50, 20, 20)
DIM SHARED subcount(50, 20)
DIM SHARED value(50)
DIM SHARED xvalue(50)
'dropdown positioning
DIM SHARED p1x(50)
DIM SHARED p1y(50)
DIM SHARED p2x(50)
DIM SHARED p2y(50)
DIM SHARED p3x(50)
DIM SHARED p3y(50)
DIM SHARED borderoffsetx(50)
DIM SHARED borderoffsety(50)
DIM SHARED i 'fuckin shit dude

'=========================================================================================================== BASIC / SPECIAL STUFF =================================================================================

' Dialog flag constants (use + or OR to use more than 1 flag value)
CONST OFN_ALLOWMULTISELECT = &H200& '  Allows the user to select more than one file, not recommended!
CONST OFN_CREATEPROMPT = &H2000& '     Prompts if a file not found should be created(GetOpenFileName only).
CONST OFN_EXTENSIONDIFFERENT = &H400& 'Allows user to specify file extension other than default extension.
CONST OFN_FILEMUSTEXIST = &H1000& '    Chechs File name exists(GetOpenFileName only).
CONST OFN_HIDEREADONLY = &H4& '        Hides read-only checkbox(GetOpenFileName only)
CONST OFN_NOCHANGEDIR = &H8& '         Restores the current directory to original value if user changed
CONST OFN_NODEREFERENCELINKS = &H100000& 'Returns path and file name of selected(i)shortcut(.LNK) file instead of file referenced.
CONST OFN_NONETWORKBUTTON = &H20000& ' Hides and disables the Network button.
CONST OFN_NOREADONLYRETURN = &H8000& ' Prevents selection of read-only files, or files in read-only subdirectory.
CONST OFN_NOVALIDATE = &H100& '        Allows invalid file name characters.
CONST OFN_OVERWRITEPROMPT = &H2& '     Prompts if file already exists(GetSaveFileName only)
CONST OFN_PATHMUSTEXIST = &H800& '     Checks Path name exists (set with OFN_FILEMUSTEXIST).
CONST OFN_READONLY = &H1& '            Checks read-only checkbox. Returns if checkbox is checked
CONST OFN_SHAREAWARE = &H4000& '       Ignores sharing violations in networking
CONST OFN_SHOWHELP = &H10& '           Shows the help button (useless!)

'DEFINT A-Z
TYPE FILEDIALOGTYPE
    lStructSize AS LONG '        For the DLL call
    hwndOwner AS LONG '          Dialog will hide behind window when not set correctly
    hInstance AS LONG '          Handle to a module that contains a dialog box template.
    lpstrFilter AS _OFFSET '     Pointer of the string of file filters
    lpstrCustFilter AS _OFFSET
    nMaxCustFilter AS LONG
    nFilterIndex AS LONG '       One based starting filter index to use when dialog is called
    lpstrFile AS _OFFSET '       String full of 0's for the selected(i)file name
    nMaxFile AS LONG '           Maximum length of the string stuffed with 0's minus 1
    lpstrFileTitle AS _OFFSET '  Same as lpstrFile
    nMaxFileTitle AS LONG '      Same as nMaxFile
    lpstrInitialDir AS _OFFSET ' Starting directory
    lpstrTitle AS _OFFSET '      Dialog title
    flags AS LONG '              Dialog flags
    nFileOffset AS INTEGER '     Zero-based offset from path beginning to file name string pointed to by lpstrFile
    nFileExtension AS INTEGER '  Zero-based offset from path beginning to file extension string pointed to by lpstrFile.
    lpstrDefExt AS _OFFSET '     Default/selected(i)file extension
    lCustData AS LONG
    lpfnHook AS LONG
    lpTemplateName AS _OFFSET
END TYPE

DECLARE DYNAMIC LIBRARY "comdlg32" ' Library declarations using _OFFSET types
    FUNCTION GetOpenFileNameA& (DIALOGPARAMS AS FILEDIALOGTYPE) ' The Open file dialog
    FUNCTION GetSaveFileNameA& (DIALOGPARAMS AS FILEDIALOGTYPE) ' The Save file dialog
END DECLARE

DECLARE LIBRARY
    FUNCTION FindWindow& (BYVAL ClassName AS _OFFSET, WindowName$) ' To get hWnd handle
END DECLARE
'=============================================================================================================== SPECIAL STUFF =====================================================================================

loaddata '    does what it says ;)

console:
CLS
_PUTIMAGE (0, 120 - (40 / 3))-(logox, logoy), c%
InputSub 1, 0, "Command: ", ""
RunMenu
IF LEN(UserInput$(1)) > 4 THEN
    g = 0
    DO
        g = g + 1
    LOOP UNTIL MID$(UserInput$(1), g, 1) = " " OR g = maxg(1)
    cmd$ = MID$(UserInput$(1), 1, g - 1)
    instance$ = MID$(UserInput$(1), g + 1, LEN(UserInput$(1)) - g)
    ClearMenu
    SELECT CASE cmd$
        CASE IS = "new"
            GOTO newpoint
        CASE IS = "view"
            GOTO viewpoint
        CASE IS = "edit"
            GOTO editpoint
    END SELECT
END IF
LOCATE firstline + 2, xcharacter
PRINT "command not recognized."
SLEEP 3
GOTO console

newpoint:
CLS
_PUTIMAGE (0, 120 - (40 / 3))-(logox, logoy), n%
LOCATE firstline, xcharacter
InputSub 1, 0, "Data point name: ", instance$
DropdownSub 2, 0, "Category: ", "data\infrs\cat1.datnet"
Button 3, 0, "Next", 1, "redirect", "end menu"
RunMenu
instanceID = VAL(LTRIM$(instanceID$(maxinstances))) + 1
instanceID$ = LTRIM$(STR$(selected(i))) + LTRIM$(STR$(instanceID)) 'adds the number of selected(i)category in front of the actual ID to refer to proper folder
instanceNAME$ = UserInput$(1)
ClearMenu
CLS
_PUTIMAGE (0, 120 - (40 / 3))-(logox, logoy), n%
LOCATE firstline, xcharacter
NewText 1, 0, "new point -" + instanceNAME$ + "-"
DropdownSub 2, 0, "Subcategory: ", "data\infrs\scat" + LTRIM$(STR$(selected(2))) + ".datnet"
Button 3, 0, "Choose File", 0, "open file", "point"
Button 4, 0, "Create", 2, "redirect", ":console"
RunMenu
IF MID$(destination$(maxi), 1, 1) = ":" THEN
    SELECT CASE LTRIM$(destination$(i))
        CASE IS = "console"
            ClearMenu
            GOTO console
    END SELECT
END IF

viewpoint:
CLS
_PUTIMAGE (0, 120 - (40 / 3))-(logox, logoy), v%
GOTO console

editpoint:
CLS
_PUTIMAGE (0, 120 - (40 / 3))-(logox, logoy), e%
GOTO console

SUB loaddata 'not only loads, but also repairs the database
    'loads broad infrastructure
    IF _DIREXISTS("data") = 0 THEN MKDIR "data"
    IF _DIREXISTS("data\net") = 0 THEN MKDIR "data\net"

    'loads allowed characters
    IF _FILEEXISTS("data\AC.tcc") = 0 THEN
        OPEN "data\AC.tcc" FOR OUTPUT AS #1
        a = 31
        DO
            a = a + 1
            WRITE #1, CHR$(a)
        LOOP UNTIL a = 122
        CLOSE #1
    END IF
    OPEN "data\AC.tcc" FOR INPUT AS #1
    ac = 0
    DO
        ac = ac + 1
        INPUT #1, ac$(ac)
    LOOP UNTIL EOF(1) = -1
    alch = ac
    CLOSE #1

    'loads base categories
    IF _FILEEXISTS("data\infrs\cat1.datnet") = -1 THEN
        OPEN "data\infrs\cat1.datnet" FOR INPUT AS #1
    ELSE
        OPEN "data\infrs\cat1.datnet" FOR OUTPUT AS #1
        WRITE #1, "living", 0
        WRITE #1, "object", 0
        WRITE #1, "text", 0
        WRITE #1, "audio", 0
        WRITE #1, "video", 0
        WRITE #1, "construct", 0
        CLOSE #1
        OPEN "data\infrs\cat1.datnet" FOR INPUT AS #1
    END IF
    IF EOF(1) = 0 THEN
        DO
            INPUT #1, cat$
            IF LEN(cat$) > 2 THEN
                totalcat = totalcat + 1
            END IF
        LOOP UNTIL EOF(1) = -1
    END IF
    CLOSE #1

    'loads folders for categories
    c = 0
    DO
        c = c + 1
        IF _DIREXISTS("data\net\" + LTRIM$(STR$(c))) = 0 THEN
            MKDIR "data\net\" + LTRIM$(STR$(c))
        END IF
    LOOP UNTIL c = totalcat

    'loads index file for instances
    IF _FILEEXISTS("data\index.datnet") = -1 THEN
        OPEN "data\index.datnet" FOR INPUT AS #1
    ELSE
        OPEN "data\index.datnet" FOR OUTPUT AS #1
        CLOSE #1
        OPEN "data\index.datnet" FOR INPUT AS #1
    END IF
    IF EOF(1) = 0 THEN
        instance = 0
        DO
            instance = instance + 1
            INPUT #1, instanceID$(instance)
        LOOP UNTIL EOF(1) = -1
        maxinstances = instance
    ELSE
        maxinstances = 0
        instanceID$(0) = "00"
    END IF
    CLOSE #1
END SUB

SUB ClearMenu
    i = 0
    'DO
    '    i = i + 1
    '    type$(i) = ""
    '    UserInput$(i) = ""
    '    status$(i) = ""
    '    g(i) = 0
    '    gbf(i) = 0
    'LOOP UNTIL type$(i + 1) = ""
    ERASE g, gbf, char$, type$, UserInput$, status$
    maxi = 0
    activei = 1
END SUB

SUB RunMenu
    maxi = i
    i = 0
    DO
        i = i + 1
        firstprint(i) = 0
    LOOP UNTIL type$(i) = ""
    activei = 1
    DO
        i = 0
        DO
            i = i + 1
            Taste$ = INKEY$
            SELECT CASE Taste$
                CASE IS = CHR$(13)
                    IF activei < maxi THEN
                        activei = activei + 1
                    ELSE
                        status$(i) = "finished"
                    END IF
                CASE IS = CHR$(0) + CHR$(60)
                    IF activei < maxi THEN
                        activei = activei + 1
                    ELSE
                        activei = 1
                    END IF
                CASE IS = CHR$(0) + CHR$(59)
                    IF activei > 1 THEN
                        activei = activei - 1
                    ELSE
                        activei = maxi
                    END IF
            END SELECT
            SELECT CASE type$(i)
                CASE IS = "text"
                    IF activei = i AND activei < maxi THEN
                        activei = activei + 1
                    ELSE
                        activei = 1
                    END IF
                    checki = 0
                    DO
                        checki = checki + 1
                        IF overlaptrigger(checki) = 1 AND checki < i THEN
                            overlaptrigger(checki) = 0
                            firstprint(i) = 0
                        END IF
                    LOOP UNTIL checki = maxi
                    IF firstprint(i) = 0 THEN
                        firstprint(i) = 1
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                        PRINT text$(i)
                    END IF
                CASE IS = "switch"
                    'TBD
                CASE IS = "button"
                    IF activei = i AND activei < maxi THEN
                        activei = activei + 1
                    ELSE
                        activei = 1
                    END IF
                    checki = 0
                    DO
                        checki = checki + 1
                        IF overlaptrigger(checki) = 1 AND checki < i THEN
                            overlaptrigger(checki) = 0
                            firstprint(i) = 0
                        END IF
                    LOOP UNTIL checki = maxi
                    IF firstprint(i) = 0 THEN
                        firstprint(i) = 1
                        SELECT CASE style(i)
                            CASE IS = 0 'default
                                LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 2) + borderoffsety(i))-(endx(i), (firstline + linetbp(i)) * fontheight + (fontheight / 3) + borderoffsety(i)), _RGBA(237, 237, 237, 255), BF
                                LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 2) + borderoffsety(i))-(endx(i), (firstline + linetbp(i)) * fontheight + (fontheight / 3) + borderoffsety(i)), _RGBA(15, 15, 15, 255), B
                                COLOR _RGBA(15, 15, 15, 255), _RGBA(237, 237, 237, 255)
                            CASE IS = 1 'filled
                                LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 2) + borderoffsety(i))-(endx(i), (firstline + linetbp(i)) * fontheight + (fontheight / 3) + borderoffsety(i)), _RGBA(15, 15, 15, 255), BF
                                COLOR _RGBA(237, 237, 237, 255), _RGBA(15, 15, 15, 255)
                            CASE IS = 2 'colored
                                LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 2) + borderoffsety(i))-(endx(i), (firstline + linetbp(i)) * fontheight + (fontheight / 3) + borderoffsety(i)), _RGBA(28, 166, 28, 255), BF
                                COLOR _RGBA(237, 237, 237, 255), _RGBA(28, 166, 28, 255)
                            CASE IS = 3 'color border
                                LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 2) + borderoffsety(i))-(endx(i), (firstline + linetbp(i)) * fontheight + (fontheight / 3) + borderoffsety(i)), _RGBA(237, 237, 237, 255), B
                                LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 2) + borderoffsety(i))-(endx(i), (firstline + linetbp(i)) * fontheight + (fontheight / 3) + borderoffsety(i)), _RGBA(28, 166, 28, 255), B
                                COLOR _RGBA(28, 166, 28, 255), _RGBA(237, 237, 237, 255)
                        END SELECT
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                        PRINT text$(i)
                        COLOR _RGBA(15, 15, 15, 255), _RGBA(237, 237, 237, 255)
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i) + LEN(text$(i)) + 2
                        PRINT fileshort$(i)
                    END IF
                    IF _MOUSEINPUT = -1 THEN
                        IF _MOUSEBUTTON(1) = -1 THEN
                            IF _MOUSEX > basex(i) AND _MOUSEX < endx(i) THEN
                                IF _MOUSEY > (firstline + linetbp(i) - 1) * fontheight - (fontheight / 2) + borderoffsety(i) AND _MOUSEY < (firstline + linetbp(i)) * fontheight + (fontheight / 3) + borderoffsety(i) THEN
                                    SELECT CASE defaultstyle(i)
                                        CASE IS = 0
                                            style(i) = 1
                                        CASE IS = 1
                                            style(i) = 0
                                        CASE IS = 2
                                            style(i) = 3
                                        CASE IS = 3
                                            style(i) = 2
                                    END SELECT
                                    SELECT CASE kind$(i)
                                        CASE IS = "redirect" 'used for going to another marker, menu or similar
                                            SELECT CASE destination$(i)
                                                CASE IS = "end menu"
                                                    status$(maxi) = "finished"
                                            END SELECT
                                            IF MID$(destination$(i), 1, 1) = ":" THEN
                                                status$(maxi) = "finished"
                                            END IF
                                        CASE IS = "open file"
                                            IF acceptinput(i) = 1 THEN
                                                hWnd& = FindWindow(0, "DATANET" + CHR$(0)) 'get window handle using _TITLE string
                                                ' Do the Open File dialog call!
                                                Filter$ = "Text files (*.txt)|*.TXT|mp3 files (*.mp3)|*.MP3|wav files (*.wav|*.WAV|DATANET files (*.net)|*.NET|All files (*.*)|*.*" + CHR$(0)
                                                Flags& = OFN_FILEMUSTEXIST + OFN_NOCHANGEDIR + OFN_READONLY '    add flag constants here
                                                file$(i) = GetOpenFileName$("Select File", ".\", Filter$, 1, Flags&, hWnd&)
                                                acceptinput(i) = 0
                                                LASTTIMER = TIMER
                                            ELSE
                                                IF TIMER - LASTTIMER > 1 THEN
                                                    acceptinput(i) = 1
                                                END IF
                                            END IF
                                        CASE IS = "save file"
                                            IF acceptinput(i) = 1 THEN
                                                hWnd& = FindWindow(0, "DATANET" + CHR$(0)) 'get window handle using _TITLE string
                                                ' Do the Save File dialog call!
                                                Filter$ = "DATANET files (*.net)|*.NET" + CHR$(0)
                                                Flags& = OFN_FILEMUSTEXIST + OFN_NOCHANGEDIR + OFN_READONLY '    add flag constants here
                                                file$(i) = GetSaveFileName$("Save File", ".\", Filter$, 1, Flags&, hWnd&)
                                                acceptinput(i) = 0
                                            ELSE
                                                IF TIMER - LASTTIMER > 1 THEN
                                                    acceptinput(i) = 1
                                                END IF
                                            END IF
                                    END SELECT
                                    COLOR _RGBA(15, 15, 15, 255), _RGBA(237, 237, 237, 255)
                                    LOCATE firstline + linetbp(i), xcharacter + xoffset(i) + LEN(text$(i)) + 4
                                    IF file$(i) <> "" THEN
                                        p = LEN(file$(i))
                                        DO
                                            p = p - 1
                                            IF MID$(file$(i), 1, p) = "\" THEN
                                                fileshort$(i) = MID$(file$(i), LEN(file$(i)) - p, p + 1)
                                            END IF
                                        LOOP UNTIL p < 1 OR fileshort$(i) <> ""
                                        PRINT fileshort$(i)
                                    ELSE
                                        fileshort$(i) = ""
                                    END IF
                                END IF
                            END IF
                        ELSE
                            style(i) = defaultstyle(i)
                        END IF
                    END IF
                CASE IS = "slider"
                    IF activei = i AND activei < maxi THEN
                        activei = activei + 1
                    ELSE
                        activei = 1
                    END IF
                    checki = 0
                    DO
                        checki = checki + 1
                        IF overlaptrigger(checki) = 1 AND checki < i THEN
                            overlaptrigger(checki) = 0
                            firstprint(i) = 0
                        END IF
                    LOOP UNTIL checki = maxi
                    IF firstprint(i) = 0 THEN
                        firstprint(i) = 1
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                        COLOR _RGBA(15, 15, 15, 255), _RGBA(237, 237, 237, 255)
                        PRINT text$(i)
                        'horizontal
                        LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight + (fontheight / 2))-(basex(i) + (maxx / 4), (firstline + linetbp(i) - 1) * fontheight + (fontheight / 2) + 1), _RGBA(15, 15, 15, 255), BF
                        'vertical
                        LINE (basex(i) + (value(i) / maxval(i) * (maxx / 4)), (firstline + linetbp(i) - 1) * fontheight + 1)-(basex(i) + 4 + (value(i) / maxval(i) * (maxx / 4)), (firstline + linetbp(i)) * fontheight - 1), _RGBA(15, 15, 15, 255), BF
                        xvalue(i) = xcharacter + xoffset(i) + LEN(text$(i)) + ((maxx / 4) / fontwidth) + 2
                        LOCATE firstline + linetbp(i), xvalue(i)
                        PRINT value(i); "   "
                    END IF
                    IF _MOUSEINPUT = -1 THEN
                        IF _MOUSEBUTTON(1) = -1 THEN
                            IF _MOUSEY > (firstline + linetbp(i) - 1) * fontheight + 1 AND _MOUSEY < (firstline + linetbp(i)) * fontheight - 1 THEN
                                DO
                                    IF _MOUSEINPUT = -1 AND _MOUSEX > basex(i) AND _MOUSEX < endx(i) THEN
                                        value(i) = _MOUSEX - (basex(i))
                                        value(i) = INT(value(i) / (maxx / 4) * 100)
                                        printslider:
                                        'cleanup
                                        LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight + 1)-(endx(i), (firstline + linetbp(i)) * fontheight - 1), _RGBA(237, 237, 237, 255), BF

                                        'horizontal
                                        LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight + (fontheight / 2))-(endx(i), (firstline + linetbp(i) - 1) * fontheight + (fontheight / 2) + 1), _RGBA(15, 15, 15, 255), BF
                                        'vertical
                                        LINE (basex(i) + (value(i) / maxval(i) * (maxx / 4)), (firstline + linetbp(i) - 1) * fontheight + 1)-(basex(i) + 4 + (value(i) / maxval(i) * (maxx / 4)), (firstline + linetbp(i)) * fontheight - 1), _RGBA(15, 15, 15, 255), BF
                                        xvalue(i) = xcharacter + xoffset(i) + LEN(text$(i)) + ((maxx / 4) / fontwidth) + 2
                                        LOCATE firstline + linetbp(i), xvalue(i)
                                        PRINT value(i); "   "
                                    ELSEIF _MOUSEINPUT = -1 AND _MOUSEX < basex OR _MOUSEX > basex(i) + 4 + (maxx / 4) THEN
                                        IF _MOUSEX < basex THEN
                                            value(i) = minval(i)
                                        ELSE
                                            value(i) = maxval(i)
                                        END IF
                                        GOTO printslider
                                    END IF
                                LOOP UNTIL _MOUSEBUTTON(1) = 0
                            END IF
                        END IF
                    END IF
                CASE IS = "dropdown"
                    IF activei = i THEN 'only enables keys if dropdown menu is active
                        SELECT CASE Taste$
                            CASE IS = CHR$(9)
                                IF expanded(i) = 1 THEN
                                    expanded(i) = 0
                                    overlaptrigger(i) = 1
                                    GOTO reprint
                                ELSE
                                    expanded(i) = 1
                                    GOTO reprint
                                END IF
                            CASE IS = CHR$(0) + CHR$(72)
                                IF selected(i) > 1 THEN
                                    selected(i) = selected(i) - 1
                                ELSE
                                    selected(i) = totalcat(i)
                                END IF
                                GOTO reprint
                            CASE IS = CHR$(0) + CHR$(80)
                                IF selected(i) < totalcat(i) THEN
                                    selected(i) = selected(i) + 1
                                ELSE
                                    selected(i) = 1
                                END IF
                                GOTO reprint
                        END SELECT
                        IF Taste$ <> "" AND activei < maxi THEN
                            activei = activei + 1
                        ELSE
                            activei = 1
                        END IF
                    END IF
                    IF _MOUSEINPUT = -1 THEN
                        selline = INT(_MOUSEY / (fontheight)) + 1
                        IF selline > (firstline + linetbp(i) - 1) AND selline < (firstline + linetbp(i) + (totalcat(i) * 2) - 1) THEN
                            mouseselected = INT((selline - (firstline + linetbp(i) - 1)) / 2) + 1
                            IF mouseselected <> selected(i) AND expanded(i) = 1 THEN
                                selected(i) = mouseselected
                                GOTO reprint
                            END IF
                        END IF
                        IF _MOUSEBUTTON(1) = -1 THEN
                            IF _MOUSEX > basex(i) AND _MOUSEX < endx(i) THEN
                                IF expanded(i) = 1 THEN
                                    IF _MOUSEY > (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i) AND _MOUSEY < (firstline + linetbp(i) + (totalcat(i) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(i) THEN
                                        expanded(i) = 0
                                        overlaptrigger(i) = 1
                                        GOTO reprint
                                    END IF
                                ELSE
                                    IF _MOUSEY > (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i) AND _MOUSEY < (firstline + linetbp(i)) * fontheight + (fontheight / 2) + borderoffsety(i) THEN
                                        expanded(i) = 1
                                        GOTO reprint
                                    END IF
                                END IF
                            END IF
                        END IF
                    END IF
                    IF firstprint(i) = 0 THEN 'only prints menu once and sets variables, loads menu etc..
                        firstprint(i) = 1
                        OPEN menufile$(i) FOR INPUT AS #1
                        IF EOF(1) = -1 THEN
                            PRINT "menu file empty, can't display dropdown menu."
                            SLEEP 3
                            SYSTEM
                        END IF
                        selected(i) = 1
                        maxcatlength = 0
                        c = 0
                        m = 0
                        DO
                            c = c + 1
                            INPUT #1, category$(i, c), subcount(i, c)
                            m = m + 1
                            menu$(i, m) = category$(i, c)
                            IF LEN(category$(i, c)) > maxcatlength(i) THEN
                                maxcatlength(i) = LEN(category$(i, c)) + 1
                            END IF
                            IF subcount(i, c) > 0 THEN
                                sc = 0
                                DO
                                    sc = sc + 1
                                    INPUT #1, subcategory$(i, c, sc)
                                    m = m + 1
                                    menu$(i, m) = "  " + subcategory$(i, c, sc)
                                    IF LEN(subcategory$(i, c, sc)) > maxcatlength(i) THEN
                                        maxcatlength(i) = LEN(subcategory$(i, c, sc)) + 4
                                    END IF
                                LOOP UNTIL sc = subcount(i, c)
                            END IF
                        LOOP UNTIL EOF(1) = -1
                        categories(i) = c
                        totalcat(i) = m
                        endx(i) = (xcharacter + xoffset(i) + LEN(text$(i)) + maxcatlength(i)) * fontwidth + (fontwidth / 2) + borderoffsetx(i) 'right end of dropdown box
                        CLOSE #1

                        'prints text in front of dropdown
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                        COLOR _RGBA(15, 15, 15, 255), _RGBA(237, 237, 237, 255)
                        PRINT text$(i)

                        reprint: 'triggered when a change occurs
                        activei = i
                        IF expanded(i) = 0 THEN
                            LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i))-(endx(i), (firstline + linetbp(i) + (totalcat(i) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(i)), _RGBA(237, 237, 237, 255), BF
                            COLOR _RGBA(15, 15, 15, 255), _RGBA(237, 237, 237, 255)
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i) + LEN(text$(i)) + 1
                            PRINT menu$(i, selected)
                            LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i))-(endx(i), (firstline + linetbp(i)) * fontheight + (fontheight / 2) + borderoffsety(i)), _RGBA(15, 15, 15, 255), B
                        ELSE
                            LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i))-(endx(i), (firstline + linetbp(i) + (totalcat(i) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(i)), _RGBA(237, 237, 237, 255), BF
                            m = 0
                            DO
                                m = m + 1
                                IF m = selected(i) THEN
                                    COLOR _RGBA(237, 237, 237, 255), _RGBA(244, 188, 67, 255)
                                ELSE
                                    COLOR _RGBA(15, 15, 15, 255), _RGBA(237, 237, 237, 255)
                                END IF
                                LOCATE firstline + linetbp(i) + (m * 2) - 2, xcharacter + xoffset(i) + LEN(text$(i)) + 1
                                PRINT menu$(i, m)
                            LOOP UNTIL m = totalcat(i)
                            LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i))-(endx(i), (firstline + linetbp(i) + (totalcat(i) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(i)), _RGBA(15, 15, 15, 255), B
                        END IF

                        'variables for dropdown triangle (right side, you know?)
                        p1x(i) = (xcharacter + xoffset(i) + LEN(text$(i)) + maxcatlength(i) - 1) * fontwidth
                        p1y(i) = (firstline + linetbp(i) - 1) * fontheight + (fontheight / 3) + borderoffsety - 3
                        p2x(i) = p1x(i) + fontwidth
                        p2y(i) = p1y(i)
                        p3x(i) = p1x(i) + ((p2x(i) - p1x(i)) / 2)
                        p3y(i) = p1y(i) + SQR(((fontwidth / 2) * (fontwidth / 2)) + (fontwidth * fontwidth)) - (fontheight / 4)

                        'triangle
                        LINE (p1x(i), p1y(i))-(p2x(i), p2y(i)), _RGBA(15, 15, 15, 255)
                        LINE (p1x(i), p1y(i))-(p3x(i), p3y(i)), _RGBA(15, 15, 15, 255)
                        LINE (p2x(i), p2y(i))-(p3x(i), p3y(i)), _RGBA(15, 15, 15, 255)
                        PAINT (p1x(i) + ((p2x(i) - p1x(i)) / 2), p1y(i) + ((p3y(i) - p1y(i)) / 2)), _RGBA(15, 15, 15, 255), _RGBA(15, 15, 15, 255)
                    END IF
                CASE IS = "input"
                    checki = 0
                    DO
                        checki = checki + 1
                        IF overlaptrigger(checki) = 1 AND checki < i THEN
                            overlaptrigger(checki) = 0
                            firstprint(i) = 0
                        END IF
                    LOOP UNTIL checki = maxi
                    COLOR _RGBA(15, 15, 15, 255), _RGBA(237, 237, 237, 255)
                    IF firstprint(i) = 0 THEN
                        firstprint(i) = 1
                        IF fill$(i) <> "" AND g(i) = 0 THEN
                            DO
                                g(i) = g(i) + 1
                                char$(i, g(i)) = MID$(fill$(i), g(i), 1)
                            LOOP UNTIL g(i) = LEN(fill$(i))
                            gbf(i) = g(i)
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                            PRINT text$(i) + fill$(i);
                        END IF
                        printback:
                        IF backspace(i) = 1 AND g(i) > 0 THEN
                            LINE ((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3))-((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2) + 2, (firstline + linetbp(i)) * fontheight + (fontheight / 2)), _RGBA(237, 237, 237, 255), BF
                            backspace(i) = 0
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i) + LEN(text$(i)) + g(i) - 1
                            PRINT " ";
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                            PRINT text$(i);
                            gbf(i) = 0
                            trigger(i) = 0
                            DO
                                gbf(i) = gbf(i) + 1
                                IF gbf(i) <> editpos(i) THEN
                                    IF trigger(i) = 1 THEN
                                        char$(i, gbf(i)) = char$(i, gbf(i) + 1)
                                    END IF
                                    PRINT char$(i, gbf(i));
                                ELSE
                                    char$(i, gbf(i)) = char$(i, gbf(i) + 1)
                                    PRINT char$(i, gbf(i));
                                    trigger(i) = 1
                                END IF
                            LOOP UNTIL gbf(i) >= g(i) - 1
                            char$(i, g(i)) = ""
                            g(i) = g(i) - 1
                        ELSE
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                            PRINT text$(i);
                        END IF
                        Taste$ = ""
                    END IF
                    IF activei = i THEN 'only checks keys if input is active
                        loopback:
                        IF Taste$ <> "" THEN
                            ac = 0
                            DO
                                ac = ac + 1
                                IF ac$(ac) = Taste$ THEN
                                    g(i) = g(i) + 1
                                    char$(i, g(i)) = Taste$
                                END IF
                            LOOP UNTIL ac = alch
                        END IF
                        editpos(i) = g(i)

                        'prints the new character if g has changed
                        IF gbf(i) <> g(i) THEN
                            IF gbf(i) < g(i) THEN
                                LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                                PRINT text$(i);
                                gbf(i) = 0
                                DO
                                    gbf(i) = gbf(i) + 1
                                    PRINT char$(i, gbf(i));
                                LOOP UNTIL gbf(i) = g(i)
                            END IF
                        END IF
                        gbf(i) = g(i)

                        'blinking line after text
                        IF TIMER MOD 2 = 0 THEN
                            LINE ((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2), (firstline + linetbp(i) - 1) * fontheight + 1)-((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2) + 2, (firstline + linetbp(i)) * fontheight - 1), _RGBA(15, 15, 15, 255), BF
                        ELSE
                            LINE ((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2), (firstline + linetbp(i) - 1) * fontheight + 1)-((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2) + 2, (firstline + linetbp(i)) * fontheight - 1), _RGBA(237, 237, 237, 255), BF
                        END IF
                        SELECT CASE Taste$
                            CASE IS = CHR$(8)
                                backspace(i) = 1
                                GOTO printback
                            CASE IS = CHR$(3) 'copies the input data
                                _CLIPBOARD$ = ""
                                p = 0
                                DO
                                    p = p + 1
                                    _CLIPBOARD$ = _CLIPBOARD$ + char$(i, p)
                                LOOP UNTIL p = g(i)
                            CASE IS = CHR$(22) 'pastes data into input
                                IF _CLIPBOARD$ <> "" THEN
                                    p = 0
                                    DO
                                        p = p + 1
                                        g(i) = g(i) + 1
                                        char$(i, g(i)) = MID$(_CLIPBOARD$, p, 1)
                                    LOOP UNTIL p = LEN(_CLIPBOARD$)
                                END IF
                            CASE IS = CHR$(0) + CHR$(75)
                                IF editpos(i) > 1 THEN
                                    editpos(i) = editpos(i) - 1
                                END IF
                            CASE IS = CHR$(0) + CHR$(77)
                                IF editpos(i) < g(i) THEN
                                    editpos(i) = editpos(i) + 1
                                END IF
                            CASE IS = CHR$(13)
                                IF activei = maxi THEN
                                    status$(maxi) = "finished"
                                ELSE
                                    activei = acitvei + 1
                                END IF
                        END SELECT
                    ELSE
                        IF _MOUSEINPUT = -1 THEN
                            IF _MOUSEBUTTON(1) = -1 THEN
                                IF _MOUSEX > (xcharacter + xoffset(i)) * fontwidth + (fontwidth / 2) AND _MOUSEX < (xcharacter + xoffset(i) + LEN(text$(i)) + g(i)) * fontwidth + (fontwidth / 2) THEN
                                    IF _MOUSEY > (firstline + linetbp(i) - 1) * fontheight + 1 AND _MOUSEY < (firstline + linetbp(i)) * fontheight - 1 THEN
                                        activei = i
                                    END IF
                                END IF
                            END IF
                            'deletes blinking line after text
                            LINE ((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2), (firstline + linetbp(i) - 1) * fontheight + 1)-((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2) + 2, (firstline + linetbp(i)) * fontheight - 1), _RGBA(237, 237, 237, 255), BF
                        END IF
                    END IF
            END SELECT
        LOOP UNTIL i = maxi
    LOOP UNTIL status$(maxi) = "finished"
    i = 0
    DO
        i = i + 1
        SELECT CASE type$(i)
            CASE IS = "input"
                IF g(i) > 0 THEN
                    maxg(i) = g(i)
                    g(i) = 0
                    DO
                        g(i) = g(i) + 1
                        UserInput$(i) = UserInput$(i) + char$(i, g(i))
                    LOOP UNTIL g(i) = maxg(i)
                    g(i) = 0
                    f = 0
                    DO
                        f = f + 1
                        char$(i, f) = ""
                    LOOP UNTIL char$(i, f + 1) = ""
                ELSE
                    UserInput$(i) = ""
                END IF
        END SELECT
    LOOP UNTIL i = maxi
END SUB

SUB NewText (linetbp, xoffset, text$)
    i = i + 1
    type$(i) = "text"
    linetbp(i) = linetbp * 2
    text$(i) = text$
END SUB

SUB Slider (linetbp, xoffset, minval, maxval, text$)
    i = i + 1
    type$(i) = "slider"
    linetbp(i) = linetbp * 2
    minval(i) = minval
    maxval(i) = maxval
    xoffset(i) = xoffset
    text$(i) = text$
    basex(i) = (xcharacter + xoffset(i) + LEN(text$(i))) * fontwidth + (fontwidth / 2) 'change this to fit your setup
    value(i) = maxval(i)
    endx(i) = basex(i) + (maxx / 4) + 4
END SUB

SUB Button (linetbp, xoffset, text$, style, kind$, destination$)
    i = i + 1
    type$(i) = "button"
    linetbp(i) = linetbp * 2
    borderoffsetx(i) = 0
    borderoffsety(i) = 0
    text$(i) = text$
    xoffset(i) = xoffset
    basex(i) = (xcharacter + xoffset(i) - 1) * fontwidth - (fontwidth / 2) + borderoffsetx(i)
    endx(i) = basex(i) + (LEN(text$(i)) + 1) * fontwidth + (fontwidth / 2)
    kind$(i) = kind$
    destination$(i) = destination$
    style(i) = style
    defaultstyle(i) = style
END SUB

SUB DropdownSub (linetbp, xoffset, text$, menufile$)
    i = i + 1
    type$(i) = "dropdown"
    menufile$(i) = menufile$
    linetbp(i) = linetbp * 2
    borderoffsetx(i) = 0
    borderoffsety(i) = -2
    text$(i) = text$
    xoffset(i) = xoffset
    basex(i) = (xcharacter + xoffset(i) + LEN(text$(i)) - 1) * fontwidth - (fontwidth / 2) + borderoffsetx(i) 'left end of dropdown box
END SUB

SUB InputSub (linetbp, xoffset, text$, fill$)
    i = i + 1
    IF maxg(i) > 0 THEN
        g(i) = 0
        DO
            g(i) = g(i) + 1
            char$(i, g(i)) = ""
        LOOP UNTIL g(i) = maxg(i)
        g(i) = 0
        maxg(i) = 0
    END IF
    type$(i) = "input"
    linetbp(i) = linetbp * 2
    fill$(i) = fill$
    text$(i) = text$
    xoffset(i) = xoffset
END SUB

FUNCTION GetOpenFileName$ (Title$, InitialDir$, Filter$, FilterIndex, Flags&, hWnd&)
    '  Title$      - The dialog title.
    '  InitialDir$ - If this left blank, it will use the directory where the last opened file is
    '  located. Specify ".\" if you want to always use the current directory.
    '  Filter$     - File filters separated by pipes (|) in the same format as using VB6 common dialogs.
    '  FilterIndex - The initial file filter to use. Will be altered by user during the call.
    '  Flags&      - Dialog flags. Will be altered by the user during the call.
    '  hWnd&       - Your program's window handle that should be aquired by the FindWindow function.
    '
    ' Returns: Blank when cancel is clicked otherwise, the file name selected(i)by the user.
    ' FilterIndex and Flags& will be changed depending on the user's selections.

    DIM OpenCall AS FILEDIALOGTYPE ' Needed for dialog call

    fFilter$ = Filter$
    FOR R = 1 TO LEN(fFilter$) ' Replace the pipes with character zero
        IF MID$(fFilter$, R, 1) = "|" THEN MID$(fFilter$, R, 1) = CHR$(0)
    NEXT R
    fFilter$ = fFilter$ + CHR$(0)

    lpstrFile$ = STRING$(2048, 0) ' For the returned file name
    lpstrDefExt$ = STRING$(10, 0) ' Extension will not be added when this is not specified
    OpenCall.lStructSize = LEN(OpenCall)
    OpenCall.hwndOwner = hWnd&
    OpenCall.lpstrFilter = _OFFSET(fFilter$)
    OpenCall.nFilterIndex = FilterIndex
    OpenCall.lpstrFile = _OFFSET(lpstrFile$)
    OpenCall.nMaxFile = LEN(lpstrFile$) - 1
    OpenCall.lpstrFileTitle = OpenCall.lpstrFile
    OpenCall.nMaxFileTitle = OpenCall.nMaxFile
    OpenCall.lpstrInitialDir = _OFFSET(InitialDir$)
    OpenCall.lpstrTitle = _OFFSET(Title$)
    OpenCall.lpstrDefExt = _OFFSET(lpstrDefExt$)
    OpenCall.flags = Flags&

    Result = GetOpenFileNameA&(OpenCall) '            Do Open File dialog call!

    IF Result THEN ' Trim the remaining zeros
        GetOpenFileName$ = LEFT$(lpstrFile$, INSTR(lpstrFile$, CHR$(0)) - 1)
        Flags& = OpenCall.flags
        FilterIndex = OpenCall.nFilterIndex
    END IF
END FUNCTION

FUNCTION GetSaveFileName$ (Title$, InitialDir$, Filter$, FilterIndex, Flags&, hWnd&)
    '  Title$      - The dialog title.
    '  InitialDir$ - If this left blank, it will use the directory where the last opened file is
    '     located. Specify ".\" if you want to always use the current directory.
    '  Filter$     - File filters separated by pipes (|) in the same format as VB6 common dialogs.
    '  FilterIndex - The initial file filter to use. Will be altered by user during the call.
    '  Flags&      - Dialog flags. Will be altered by the user during the call.
    '  hWnd&       - Your program's window handle that should be aquired by the FindWindow function.

    ' Returns: Blank when cancel is clicked otherwise, the file name entered by the user.
    ' FilterIndex and Flags& will be changed depending on the user's selections.

    DIM SaveCall AS FILEDIALOGTYPE ' Needed for dialog call

    fFilter$ = Filter$
    FOR R = 1 TO LEN(fFilter$) ' Replace the pipes with zeros
        IF MID$(fFilter$, R, 1) = "|" THEN MID$(fFilter$, R, 1) = CHR$(0)
    NEXT R
    fFilter$ = fFilter$ + CHR$(0)

    lpstrFile$ = STRING$(2048, 0) ' For the returned file name
    lpstrDefExt$ = STRING$(10, 0) ' Extension will not be added when this is not specified
    SaveCall.lStructSize = LEN(SaveCall)
    SaveCall.hwndOwner = hWnd&
    SaveCall.lpstrFilter = _OFFSET(fFilter$)
    SaveCall.nFilterIndex = FilterIndex
    SaveCall.lpstrFile = _OFFSET(lpstrFile$)
    SaveCall.nMaxFile = LEN(lpstrFile$) - 1
    SaveCall.lpstrFileTitle = SaveCall.lpstrFile
    SaveCall.nMaxFileTitle = SaveCall.nMaxFile
    SaveCall.lpstrInitialDir = _OFFSET(InitialDir$)
    SaveCall.lpstrTitle = _OFFSET(Title$)
    SaveCall.lpstrDefExt = _OFFSET(lpstrDefExt$)
    SaveCall.flags = Flags&

    Result& = GetSaveFileNameA&(SaveCall) ' Do dialog call!

    IF Result& THEN ' Trim the remaining zeros
        GetSaveFileName$ = LEFT$(lpstrFile$, INSTR(lpstrFile$, CHR$(0)) - 1)
        Flags& = SaveCall.flags
        FilterIndex = SaveCall.nFilterIndex
    END IF
END FUNCTION
