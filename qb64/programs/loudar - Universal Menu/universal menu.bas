CLS: CLEAR: CLOSE
DIM SHARED maxx
DIM SHARED maxy
maxx = (_DESKTOPWIDTH / 2) * 1.1
maxy = maxx / 16 * 9
SCREEN _NEWIMAGE(maxx, maxy, 32)
DO: LOOP UNTIL _SCREENEXISTS
_SCREENMOVE (_DESKTOPWIDTH / 2) - (maxx / 2), (_DESKTOPHEIGHT / 2) - (maxy / 2)

'loads allowed characters (ac) from file
DIM SHARED ac$(200)
DIM SHARED alch 'amount of different allowed characters

'font
DIM SHARED maxrows 'maximum amount of rows
DIM SHARED maxlines 'maximum amount of columns
DIM SHARED fontheight
DIM SHARED fontwidth
DIM SHARED firstline 'standard 0-column
DIM SHARED firstchar 'standard 0-row
DIM SHARED r&
fontheight = 16
fontfiler$ = "C:\WINDOWS\FONTS\COUR.TTF"
r& = _LOADFONT(fontfiler$, fontheight, "MONOSPACE")
_FONT r&
fontwidth = _FONTWIDTH(r&)
maxrows = INT(maxx / fontwidth)
maxlines = INT(maxy / fontheight) - 4

'menu (alphabetically sorted)
maxmenuitems = 50
maxarraylen = 20
mafirstchars = 3000
DIM SHARED arraydata$(maxmenuitems, maxarraylen) '  used to store the contents of selectors and dropdown elements
DIM SHARED backspace(maxmenuitems) '                determines whether backspace was pressed during an active input
DIM SHARED basex(maxmenuitems) '                    represents the very left x value of a menu object
DIM SHARED basey(maxmenuitems) '                    represents the very top y value of a menu object
DIM SHARED borderoffsetx(maxmenuitems) '            change this to widen/narrow items with borders
DIM SHARED borderoffsety(maxmenuitems) '            change this to widen/narrow items with borders
DIM SHARED c(maxmenuitems) '                        amount of characters in input element
DIM SHARED cbf(maxmenuitems) '                      amount of characters in input element, but from one loop before
DIM SHARED char$(maxmenuitems, mafirstchars) '      single character within element at specific position
DIM SHARED defaultstyle(maxmenuitems) '             determines the default style of a menu object
DIM SHARED destination$(maxmenuitems) '             what should happen if the user interacts with this element
DIM SHARED editpos(maxmenuitems) '                  current editing position in input
DIM SHARED endparameter$ '                          this gives you info about what the user did in the menu, can be used to transfer data (could potentially convert the RunMenu SUB into a Function...)
DIM SHARED endx(maxmenuitems) '                     represents the very right x value of a menu object
DIM SHARED endy(maxmenuitems) '                     represents the very bottom y value of a menu object
DIM SHARED expanded(maxmenuitems) '                 if the dropdown menu is expanded
DIM SHARED fill$(maxmenuitems) '                    text to fill the editable part of an input with, good for stuff that is loaded, changed, then saved again
DIM SHARED firstprint(maxmenuitems) '               equals 0 if the element hasn't been written on screen before, can be used to reinitialize it
DIM SHARED kind$(maxmenuitems) '                    defines the button type
DIM SHARED inter(maxmenuitems) '                    if element is interactable with
DIM SHARED m '                                      could be used to get current element and use it in other SUB's/Functions
DIM SHARED maxad(maxmenuitems) '                    amount of data in array
DIM SHARED maxadlength(maxmenuitems) '              maximum length of array data for dropdown element
DIM SHARED maxc(maxmenuitems) '                     maximum amount of characters in input element
DIM SHARED maxmi '                                  highest interactable element
DIM SHARED maxval(maxmenuitems) '                   maximum value of slider element
DIM SHARED minval(maxmenuitems) '                   minimum value of slider element
DIM SHARED overlaptrigger(maxmenuitems) '           used to deal with overlapping issues of dropdown-elements
DIM SHARED selected(maxmenuitems) '                 selectd object within array of element
DIM SHARED setting$(maxmenuitems) '                 setting that the toggle element changes
DIM SHARED state(maxmenuitems) '                    state of toggle element
DIM SHARED status$(maxmenuitems) '                  used for interactive elements, "finished" if determined done (e.g. for filling out forms)
DIM SHARED style(maxmenuitems) '                    current style of an element
DIM SHARED text$(maxmenuitems) '                    non-editable text in front of input
DIM SHARED trigger(maxmenuitems) '                  it's used, but idk what it does anymore :'D
DIM SHARED TYPE$(maxmenuitems) '                    type of menu element
DIM SHARED UserInput$(maxmenuitems) '               full input text of input element, for usage after menu is run
DIM SHARED value(maxmenuitems) '                    current value of slider element
DIM SHARED xoffset(maxmenuitems) '                  positioning on x-axis
DIM SHARED yoffset(maxmenuitems) '                  positioning on y-axis
'dropdown triangles :D (ascii chars could work maybe?)
DIM SHARED p1x(maxmenuitems)
DIM SHARED p1y(maxmenuitems)
DIM SHARED p2x(maxmenuitems)
DIM SHARED p2y(maxmenuitems)
DIM SHARED p3x(maxmenuitems)
DIM SHARED p3y(maxmenuitems)

'color definitions
maxcolors = 5
DIM SHARED r(maxcolors): DIM SHARED g(maxcolors): DIM SHARED b(maxcolors): DIM SHARED a(maxcolors)
r(1) = 237: g(1) = r(1): b(1) = r(1): a(1) = 255 '_RGBA(237, 237, 237, 255)     - white
r(2) = 15: g(2) = r(2): b(2) = r(2): a(2) = 255 '_RGBA(15, 15, 15, 255)         - black
r(3) = 28: g(3) = 166: b(3) = 28: a(3) = 255 '_RGBA(28, 166, 28, 255)           - green
r(4) = 244: g(4) = 188: b(4) = 67: a(4) = 255 '_RGBA(244, 188, 67, 255)         - orange/yellow
r(5) = 216: g(5) = 33: b(5) = 17: a(5) = 255 ' _RGBA(216, 33, 17, 255)          - red
COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))

'load the allowed input characters, feel free to change these :)
OPEN "AC.txt" FOR INPUT AS #1
ac = 0
DO
    ac = ac + 1
    INPUT #1, ac$(ac)
    ac$(ac) = _INFLATE$(ac$(ac))
LOOP UNTIL EOF(1) = -1
alch = ac
CLOSE #1

'change these to your liking!
firstline = 2
firstchar = 5

'================================================= PROGRAM AREA =================================================

ClearMenu
'add your elements here
NewText 1, 0, "Test", 0

NewToggle 2, 0, "ToggleTest", "lmao"

NewSlider 3, 0, 0, 100, "Test Value"

NewButton 4, 0, "Button!", 0, "redirection", "nowhere"

NewSelector 5, 0, "Select"
arraydata$(5, 1) = "this"
arraydata$(5, 2) = "that"
maxad(5) = 2

NewDropdown 6, 0, "Select"
arraydata$(6, 1) = "dis"
arraydata$(6, 2) = "dat"
maxad(6) = 2

NewInput 7, 0, "Write here:", "actually, don't"

RunMenu
SLEEP
SYSTEM

'=================================================== SUB AREA ===================================================

SUB NewText (yoffset, xoffset, text$, style)
    m = m + 1
    inter(m) = 0
    TYPE$(m) = "text"
    yoffset(m) = yoffset * 2
    xoffset(m) = xoffset
    text$(m) = text$
    style(m) = style
    status$(m) = ""
END SUB

SUB NewToggle (yoffset, xoffset, text$, setting$)
    m = m + 1
    TYPE$(m) = "toggle"
    setting$(m) = setting$
    SELECT CASE setting$(m)
        CASE IS = "YOUR_SETTING_HERE"
            state(m) = SETTING_VALUE 'writes the state of the value used into the menu so it displays properly
    END SELECT
    text$(m) = text$
    yoffset(m) = yoffset * 2
    xoffset(m) = xoffset
    basex(m) = (firstchar + (LEN(text$)) + xoffset(m) + 1) * fontwidth
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + 2 * fontheight
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
END SUB

SUB NewSlider (yoffset, xoffset, minval, maxval, text$)
    m = m + 1
    inter(m) = 1
    maxmi = m
    TYPE$(m) = "slider"
    yoffset(m) = yoffset * 2
    minval(m) = minval
    maxval(m) = maxval
    xoffset(m) = xoffset
    text$(m) = text$
    basex(m) = (firstchar + xoffset(m) + LEN(text$(m))) * fontwidth + (fontwidth / 2) 'change this to fit your setup
    value(m) = maxval(m)
    endx(m) = basex(m) + (maxx / 4) + 4
    status$(m) = ""
END SUB

SUB NewButton (yoffset, xoffset, text$, style, kind$, destination$)
    m = m + 1
    maxmi = m
    inter(m) = 1
    TYPE$(m) = "button"
    yoffset(m) = yoffset * 2
    xoffset(m) = xoffset
    borderoffsetx(m) = 0
    borderoffsety(m) = 0
    text$(m) = text$
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2) + borderoffsetx(m)
    endx(m) = basex(m) + (LEN(text$(m)) + 1) * fontwidth + (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2) + borderoffsety(m)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3) + borderoffsety(m)
    kind$(m) = kind$
    destination$(m) = destination$
    style(m) = style
    defaultstyle(m) = style
    status$(m) = ""
END SUB

SUB NewSelector (yoffset, xoffset, text$)
    m = m + 1
    inter(m) = 1
    maxmi = m
    TYPE$(m) = "selector"
    selected(m) = 1
    text$(m) = text$
    yoffset(m) = yoffset * 2
    xoffset(m) = xoffset
    basex(m) = (firstchar + xoffset(m) - 1) * fontwidth - (fontwidth / 2)
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2)
    endx(m) = basex(m) + (LEN(text$) + 1) * fontwidth + (fontwidth / 2)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3)
END SUB

SUB NewDropdown (yoffset, xoffset, text$)
    m = m + 1
    inter(m) = 1
    maxmi = m
    TYPE$(m) = "dropdown"
    yoffset(m) = yoffset * 2
    xoffset(m) = xoffset
    borderoffsetx(m) = 0
    borderoffsety(m) = -2
    text$(m) = text$
    basex(m) = (firstchar + xoffset(m) + LEN(text$(m)) + 1) * fontwidth - (fontwidth / 2) + borderoffsetx(m) 'left end of dropdown box
    basey(m) = (firstline + yoffset(m) - 1) * fontheight - (fontheight / 2) + borderoffsety(m)
    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3) + borderoffsety(m)
    status$(m) = ""
END SUB

SUB NewInput (yoffset, xoffset, text$, fill$)
    m = m + 1
    inter(m) = 1
    maxmi = m
    IF maxc(m) > 0 THEN
        c(m) = 0
        DO
            c(m) = c(m) + 1
            char$(m, c(m)) = ""
        LOOP UNTIL c(m) = maxg(m)
        c(m) = 0
        maxc(m) = 0
    END IF
    TYPE$(m) = "input"
    yoffset(m) = yoffset * 2
    fill$(m) = fill$
    text$(m) = text$
    xoffset(m) = xoffset
    status$(m) = ""
END SUB

SUB ClearMenu
    m = 0
    'ERASE c, cbf, char$, firstprint, type$, UserInput$, status$, destination$, style, defaultstyle, basex, endx, basey, endy
    maxm = 0
    activem = 1
    maxmi = 0
END SUB

SUB RunMenu
    COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
    CLS
    'add more background elements here if you want! you can just add a parameter to RunMenu to get the info which BG should be used
    endparameter$ = ""
    maxm = m
    activem = 1
    DO
        '_LIMIT 120
        Taste$ = INKEY$
        SELECT CASE Taste$
            CASE IS = CHR$(13)
                IF activem < maxmm AND inter(activem) = 1 THEN
                    status$(activem) = "finished"
                    activem = activem + 1
                END IF
            CASE IS = CHR$(0) + CHR$(80) 'arrow down
                IF activem < maxm THEN
                    activem = activem + 1
                ELSE
                    activem = 1
                END IF
            CASE IS = CHR$(0) + CHR$(72) 'arrow up
                IF activem > 1 THEN
                    activem = activem - 1
                ELSE
                    activem = maxm
                END IF
            CASE IS = CHR$(0) + CHR$(60) 'F2
                IF activem < maxm THEN
                    activem = activem + 1
                ELSE
                    activem = 1
                END IF
            CASE IS = CHR$(0) + CHR$(59) 'F1
                IF activem > 1 THEN
                    activem = activem - 1
                ELSE
                    activem = maxm
                END IF
            CASE IS = CHR$(0) + CHR$(77) 'arrow right
                IF selected(activem) < maxad(activem) THEN
                    selected(activem) = selected(activem) + 1
                ELSE
                    selected(activem) = 1
                END IF
            CASE IS = CHR$(0) + CHR$(75) 'arrow left
                IF selected(activem) > 1 THEN
                    selected(activem) = selected(activem) - 1
                ELSE
                    selected(activem) = maxad(activem)
                END IF
        END SELECT
        m = 0
        DO
            m = m + 1
            SELECT CASE TYPE$(m)
                CASE IS = "text"
                    checkm = 0
                    DO
                        checkm = checkm + 1
                        IF overlaptrigger(checkm) = 1 AND checkm < m THEN
                            overlaptrigger(checkm) = 0
                            firstprint(m) = 0
                        END IF
                    LOOP UNTIL checkm = maxm
                    IF firstprint(m) = 0 THEN
                        firstprint(m) = 1
                        LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                        SELECT CASE style(m)
                            CASE IS = 0
                                COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                            CASE IS = 1
                                COLOR _RGBA(r(3), g(3), b(3), a(3)), _RGBA(r(1), g(1), b(1), a(1)) 'green
                            CASE IS = 2
                                COLOR _RGBA(r(5), g(5), b(5), a(5)), _RGBA(r(1), g(1), b(1), a(1)) 'red
                        END SELECT
                        PRINT text$(m)
                    END IF
                CASE IS = "toggle"
                    IF _MOUSEINPUT = -1 THEN
                        IF mouseclicked = 0 THEN
                            IF _MOUSEBUTTON(1) = -1 THEN
                                IF _MOUSEX > basex(m) AND _MOUSEX < endx(m) THEN
                                    IF _MOUSEY > basey(m) AND _MOUSEY < endy(m) THEN
                                        mouseclicked = 1
                                        IF state(m) = 1 THEN
                                            state(m) = 0
                                        ELSE
                                            state(m) = 1
                                        END IF
                                        GOTO printtoggle
                                    END IF
                                END IF
                            END IF
                        END IF
                    ELSE
                        mouseclicked = 0
                    END IF
                    IF firstprint(m) = 0 THEN
                        firstprint(m) = 1
                        printtoggle:
                        SELECT CASE state(m)
                            CASE IS = 0
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), _RGBA(r(1), g(1), b(1), a(1)), BF
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), _RGBA(r(2), g(2), b(2), a(2)), B
                                LINE (basex(m) + 2, basey(m) + 2)-(endx(m) - (endx(m) - basex(m)) / 2 - 2, endy(m) - 2), _RGBA(r(2), g(2), b(2), a(2)), BF
                            CASE IS = 1
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), _RGBA(r(2), g(2), b(2), a(2)), BF
                                LINE (basex(m) + (endx(m) - basex(m)) / 2 + 2, basey(m) + 2)-(endx(m) - 2, endy(m) - 2), _RGBA(r(4), g(4), b(4), a(4)), BF
                        END SELECT
                        COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                        LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                        PRINT text$(m)
                    END IF
                CASE IS = "button"
                    checkm = 0
                    DO
                        checkm = checkm + 1
                        IF overlaptrigger(checkm) = 1 AND checkm < m THEN
                            overlaptrigger(checkm) = 0
                            firstprint(m) = 0
                        END IF
                    LOOP UNTIL checkm = maxm
                    IF firstprint(m) = 0 THEN
                        firstprint(m) = 1
                        SELECT CASE style(m)
                            CASE IS = 0 'default
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), _RGBA(r(1), g(1), b(1), a(1)), BF
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), _RGBA(r(2), g(2), b(2), a(2)), B
                                COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                            CASE IS = 1 'filled
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), _RGBA(r(2), g(2), b(2), a(2)), BF
                                COLOR _RGBA(r(1), g(1), b(1), a(1)), _RGBA(r(2), g(2), b(2), a(2))
                            CASE IS = 2 'colored (color index 3)
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), _RGBA(r(3), g(3), b(3), a(3)), BF
                                COLOR _RGBA(r(1), g(1), b(1), a(1)), _RGBA(r(3), g(3), b(3), a(3))
                            CASE IS = 3 'color border (color index 3)
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), _RGBA(r(1), g(1), b(1), a(1)), B
                                LINE (basex(m), basey(m))-(endx(m), endy(m)), _RGBA(r(3), g(3), b(3), a(3)), B
                                COLOR _RGBA(r(3), g(3), b(3), a(3)), _RGBA(r(1), g(1), b(1), a(1))
                        END SELECT
                        LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                        PRINT text$(m)
                    END IF
                    IF _MOUSEINPUT = -1 THEN
                        IF _MOUSEX > basex(m) AND _MOUSEX < endx(m) THEN
                            IF _MOUSEY > basey(m) AND _MOUSEY < endy(m) THEN
                                SELECT CASE defaultstyle(m)
                                    CASE IS = 0
                                        style(m) = 1
                                    CASE IS = 1
                                        style(m) = 0
                                    CASE IS = 2
                                        style(m) = 3
                                    CASE IS = 3
                                        style(m) = 2
                                END SELECT
                                IF firstprint(m) = 1 THEN firstprint(m) = 0
                                IF _MOUSEBUTTON(1) = -1 THEN
                                    SELECT CASE kind$(m)
                                        CASE IS = "confirmation"
                                            endparameter$ = "true"
                                            status$(maxmi) = "finished"
                                        CASE IS = "redirect" 'used for going to another marker, menu or similar
                                            SELECT CASE destination$(m)
                                                CASE IS = "end menu"
                                                    status$(maxmi) = "finished"
                                            END SELECT
                                            IF MID$(destination$(m), LEN(destination$(m)), 1) = ":" THEN 'if destination$ has : at the end, it will end the menu because it interprets it as going to another place
                                                status$(maxmi) = "finished"
                                                endparameter$ = destination$(m)
                                            END IF
                                    END SELECT
                                END IF
                            ELSE
                                IF defaultstyle(m) <> style(m) THEN style(m) = defaultstyle(m): firstprint(m) = 0
                            END IF
                        ELSE
                            IF defaultstyle(m) <> style(m) THEN style(m) = defaultstyle(m): firstprint(m) = 0
                        END IF
                    END IF
                CASE IS = "slider"
                    checkm = 0
                    DO
                        checkm = checkm + 1
                        IF overlaptrigger(checkm) = 1 AND checkm < m THEN
                            overlaptrigger(checkm) = 0
                            firstprint(m) = 0
                        END IF
                    LOOP UNTIL checkm = maxm
                    IF firstprint(m) = 0 THEN
                        firstprint(m) = 1
                        LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                        COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                        PRINT text$(m)
                        'horizontal
                        LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2))-(basex(m) + (maxx / 4), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2) + 1), _RGBA(r(2), g(2), b(2), a(2)), BF
                        'vertical
                        LINE (basex(m) + (value(m) / maxval(m) * (maxx / 4)), (firstline + yoffset(m) - 1) * fontheight + 1)-(basex(m) + 4 + (value(m) / maxval(m) * (maxx / 4)), (firstline + yoffset(m)) * fontheight - 1), _RGBA(r(2), g(2), b(2), a(2)), BF
                        xvalue = firstchar + xoffset(m) + LEN(text$(m)) + ((maxx / 4) / fontwidth) + 2
                        LOCATE firstline + yoffset(m), xvalue
                        PRINT value(m); "   "
                    END IF
                    IF _MOUSEINPUT = -1 THEN
                        IF _MOUSEBUTTON(1) = -1 THEN
                            IF _MOUSEY > (firstline + yoffset(m) - 1) * fontheight + 1 AND _MOUSEY < (firstline + yoffset(m)) * fontheight - 1 THEN
                                DO
                                    IF _MOUSEINPUT = -1 AND _MOUSEX > basex(m) AND _MOUSEX < endx(m) THEN
                                        value(m) = _MOUSEX - (basex(m))
                                        value(m) = INT(value(m) / (maxx / 4) * 100)
                                        printslider:
                                        'cleanup
                                        LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight + 1)-(endx(m), (firstline + yoffset(m)) * fontheight - 1), _RGBA(r(1), g(1), b(1), a(1)), BF

                                        'horizontal
                                        LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2))-(endx(m), (firstline + yoffset(m) - 1) * fontheight + (fontheight / 2) + 1), _RGBA(r(2), g(2), b(2), a(2)), BF
                                        'vertical
                                        LINE (basex(m) + (value(m) / maxval(m) * (maxx / 4)), (firstline + yoffset(m) - 1) * fontheight + 1)-(basex(m) + 4 + (value(m) / maxval(m) * (maxx / 4)), (firstline + yoffset(m)) * fontheight - 1), _RGBA(r(2), g(2), b(2), a(2)), BF
                                        xvalue = firstchar + xoffset(m) + LEN(text$(m)) + ((maxx / 4) / fontwidth) + 2
                                        LOCATE firstline + yoffset(m), xvalue
                                        COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                                        PRINT value(m); "   "
                                    ELSEIF _MOUSEINPUT = -1 AND _MOUSEX < basex(m) OR _MOUSEX > basex(m) + 4 + (maxx / 4) THEN
                                        IF _MOUSEX < basex(m) THEN
                                            value(m) = minval(m)
                                        ELSE
                                            value(m) = maxval(m)
                                        END IF
                                        GOTO printslider
                                    END IF
                                LOOP UNTIL _MOUSEBUTTON(1) = 0
                            END IF
                        END IF
                    END IF
                CASE IS = "selector" 'simpler and in-line version of dropdown
                    IF _MOUSEINPUT = -1 THEN
                        IF _MOUSEBUTTON(1) = -1 THEN
                            IF _MOUSEX > (firstchar + xoffset(m)) * fontwidth + (fontwidth / 2) AND _MOUSEX < (firstchar + xoffset(m) + LEN(text$(m)) + LEN(arraydata$(m, selected(m))) + 5) * fontwidth + (fontwidth / 2) THEN
                                IF _MOUSEY > (firstline + yoffset(m) - 1) * fontheight AND _MOUSEY < (firstline + yoffset(m)) * fontheight THEN
                                    checkm = 0: dontgoactive = 0
                                    DO
                                        checkm = checkm + 1
                                        IF expanded(checkm) = 1 THEN dontgoactive = 1 ELSE dontgoactive = 0
                                    LOOP UNTIL checkm = maxm
                                    IF dontgoactive = 0 THEN activem = m
                                END IF
                            END IF
                        END IF
                    END IF
                    'shows non-editable text
                    COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                    LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                    PRINT text$(m);

                    'shows editable text
                    IF activem <> m THEN
                        COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                    ELSE
                        COLOR _RGBA(r(4), g(4), b(4), a(4)), _RGBA(r(1), g(1), b(1), a(1))
                    END IF
                    PRINT " < ";
                    PRINT arraydata$(m, selected(m));
                    PRINT " >   "
                CASE IS = "dropdown"
                    IF activem = m THEN 'only enables keys if dropdown menu is active
                        SELECT CASE Taste$
                            CASE IS = CHR$(9)
                                IF expanded(m) = 1 THEN
                                    expanded(m) = 0
                                    overlaptrigger(m) = 1
                                    GOTO reprint
                                ELSE
                                    expanded(m) = 1
                                    GOTO reprint
                                END IF
                            CASE IS = CHR$(0) + CHR$(77) 'arrow right
                                IF selected(m) > 1 THEN
                                    selected(m) = selected(m) - 1
                                ELSE
                                    selected(m) = maxad(m)
                                END IF
                                GOTO reprint
                            CASE IS = CHR$(0) + CHR$(75) 'arrow left
                                IF selected(m) < maxad(m) THEN
                                    selected(m) = selected(m) + 1
                                ELSE
                                    selected(m) = 1
                                END IF
                                GOTO reprint
                        END SELECT
                    END IF
                    IF firstprint(m) = 0 THEN 'only prints menu once and sets variables, loads menu etc..
                        firstprint(m) = 1
                        ad = 0
                        DO
                            ad = ad + 1
                            IF maxadlength(m) < LEN(arraydata$(m, ad)) THEN maxadlength(m) = LEN(arraydata$(m, ad))
                        LOOP UNTIL ad = maxad(m)
                        endx(m) = (firstchar + xoffset(m) + LEN(text$(m)) + maxadlength(m) + 3) * fontwidth + (fontwidth / 2) + borderoffsetx(m) 'right end of dropdown box
                        selected(m) = 1

                        reprint: 'triggered when a change occurs
                        'prints text in front of dropdown
                        LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                        COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                        PRINT text$(m)

                        'activem = m
                        IF expanded(m) = 0 THEN
                            LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight - (fontheight / 3) + borderoffsety(m))-(endx(m), (firstline + yoffset(m)) * fontheight + (fontheight / 2) + borderoffsety(m)), _RGBA(r(1), g(1), b(1), a(1)), BF
                            LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight - (fontheight / 3) + borderoffsety(m))-(endx(m), (firstline + yoffset(m)) * fontheight + (fontheight / 2) + borderoffsety(m)), _RGBA(r(2), g(2), b(2), a(2)), B
                            IF activem <> m THEN
                                COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                            ELSE
                                COLOR _RGBA(r(4), g(4), b(4), a(4)), _RGBA(r(1), g(1), b(1), a(1))
                            END IF
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m) + LEN(text$(m)) + 2
                            PRINT arraydata$(m, selected(m))
                        ELSE
                            LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight - (fontheight / 3) + borderoffsety(m))-(endx(m), (firstline + yoffset(m) + (maxad(m) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(m)), _RGBA(r(1), g(1), b(1), a(1)), BF
                            ad = 0
                            DO
                                ad = ad + 1
                                IF ad = selected(m) THEN
                                    COLOR _RGBA(r(1), g(1), b(1), a(1)), _RGBA(r(4), g(4), b(4), a(4))
                                ELSE
                                    COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                                END IF
                                LOCATE firstline + yoffset(m) + (ad * 2) - 2, firstchar + xoffset(m) + LEN(text$(m)) + 2
                                PRINT arraydata$(m, ad)
                            LOOP UNTIL ad = maxad(m)
                            LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight - (fontheight / 3) + borderoffsety(m))-(endx(m), (firstline + yoffset(m) + (maxad(m) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(m)), _RGBA(r(2), g(2), b(2), a(2)), B
                        END IF

                        'variables for dropdown triangle (right side, you know?)
                        p1x(m) = (firstchar + xoffset(m) + LEN(text$(m)) + maxadlength(m) + 2) * fontwidth
                        p1y(m) = (firstline + yoffset(m) - 1) * fontheight + (fontheight / 3) + borderoffsety - 3
                        p2x(m) = p1x(m) + fontwidth
                        p2y(m) = p1y(m)
                        p3x(m) = p1x(m) + ((p2x(m) - p1x(m)) / 2)
                        p3y(m) = p1y(m) + SQR(((fontwidth / 2) * (fontwidth / 2)) + (fontwidth * fontwidth)) - (fontheight / 4)

                        'triangle
                        LINE (p1x(m), p1y(m))-(p2x(m), p2y(m)), _RGBA(r(2), g(2), b(2), a(2))
                        LINE (p1x(m), p1y(m))-(p3x(m), p3y(m)), _RGBA(r(2), g(2), b(2), a(2))
                        LINE (p2x(m), p2y(m))-(p3x(m), p3y(m)), _RGBA(r(2), g(2), b(2), a(2))
                        PAINT (p1x(m) + ((p2x(m) - p1x(m)) / 2), p1y(m) + ((p3y(m) - p1y(m)) / 2)), _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(2), g(2), b(2), a(2))
                    END IF
                    IF _MOUSEINPUT = -1 THEN
                        selline = INT(_MOUSEY / (fontheight)) + 1
                        IF selline > (firstline + yoffset(m) - 1) AND selline < (firstline + yoffset(m) + (maxad(m) * 2) - 1) THEN
                            mouseselected = INT((selline - (firstline + yoffset(m) - 1)) / 2) + 1
                            IF mouseselected <> selected(m) AND expanded(m) = 1 THEN
                                selected(m) = mouseselected
                                GOTO reprint
                            END IF
                        END IF
                        IF _MOUSEBUTTON(1) = -1 AND mouseclicked = 0 THEN
                            IF _MOUSEX > basex(m) AND _MOUSEX < endx(m) THEN
                                mouseclicked = 1
                                IF expanded(m) = 1 THEN
                                    endy(m) = (firstline + yoffset(m) + maxad(m)) * fontheight + (fontheight / 3) + borderoffsety(m)
                                    IF _MOUSEY > basey(m) AND _MOUSEY < endy(m) THEN
                                        expanded(m) = 0
                                        LINE (basex(m), (firstline + yoffset(m) - 1) * fontheight - (fontheight / 3) + borderoffsety(m))-(endx(m), (firstline + yoffset(m) + (maxad(m) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(m)), _RGBA(r(1), g(1), b(1), a(1)), BF
                                        overlaptrigger(m) = 1
                                        GOTO reprint
                                    END IF
                                ELSE
                                    endy(m) = (firstline + yoffset(m)) * fontheight + (fontheight / 3) + borderoffsety(m)
                                    IF _MOUSEY > basey(m) AND _MOUSEY < endy(m) THEN
                                        expanded(m) = 1
                                        GOTO reprint
                                    END IF
                                END IF
                            END IF
                        ELSE
                            mouseclicked = 0
                        END IF
                    END IF
                CASE IS = "input"
                    checkm = 0
                    DO
                        checkm = checkm + 1
                        IF overlaptrigger(checkm) = 1 AND checkm < m THEN
                            overlaptrigger(checkm) = 0
                            firstprint(m) = 0
                            selectedall = 0
                        END IF
                    LOOP UNTIL checkm = maxm
                    COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                    IF firstprint(m) = 0 THEN
                        firstprint(m) = 1
                        IF fill$(m) <> "" AND c(m) = 0 THEN
                            DO
                                c(m) = c(m) + 1
                                char$(m, c(m)) = MID$(fill$(m), c(m), 1)
                            LOOP UNTIL c(m) = LEN(fill$(m))
                            cbf(m) = c(m)
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            PRINT text$(m) + " ";
                            IF selectedall = 0 THEN
                                COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                            ELSE
                                COLOR _RGBA(r(1), g(1), b(1), a(1)), _RGBA(r(2), g(2), b(2), a(2))
                            END IF
                            PRINT fill$(m);
                        END IF
                        printback:
                        IF backspace(m) = 1 AND c(m) > 0 AND selectedall = 0 THEN
                            IF TIMER MOD 2 = 0 THEN LINE ((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2), (firstline + yoffset(m) - 1) * fontheight + 1)-((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2) + 2, (firstline + yoffset(m)) * fontheight - 1), _RGBA(r(1), g(1), b(1), a(1)), BF
                            backspace(m) = 0
                            COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            PRINT text$(m) + " ";
                            cbf(m) = 0
                            trigger(m) = 0
                            DO
                                cbf(m) = cbf(m) + 1
                                IF cbf(m) <> editpos(m) THEN
                                    IF trigger(m) = 1 THEN
                                        char$(m, cbf(m)) = char$(m, cbf(m) + 1)
                                    END IF
                                    PRINT char$(m, cbf(m));
                                ELSE
                                    char$(m, cbf(m)) = char$(m, cbf(m) + 1)
                                    PRINT char$(m, cbf(m));
                                    trigger(m) = 1
                                END IF
                            LOOP UNTIL cbf(m) >= c(m) - 1
                            PRINT "  "
                            char$(m, c(m)) = ""
                            c(m) = c(m) - 1
                        ELSEIF backspace(m) = 1 AND c(m) > 0 AND selectedall = 1 THEN
                            IF TIMER MOD 2 = 0 THEN LINE ((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2), (firstline + yoffset(m) - 1) * fontheight + 1)-((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2) + 2, (firstline + yoffset(m)) * fontheight - 1), _RGBA(r(1), g(1), b(1), a(1)), BF
                            backspace(m) = 0
                            COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            PRINT text$(m) + " ";
                            selectedall = 0
                            cbf(m) = 0
                            DO
                                cbf(m) = cbf(m) + 1
                                char$(m, cbf(m)) = "" ' clears text array
                                PRINT " ";
                            LOOP UNTIL cbf(m) = c(m)
                            cbf(m) = c(m)
                            c(m) = 0
                            IF fill$(m) <> "" THEN
                                DO
                                    c(m) = c(m) + 1
                                    char$(m, c(m)) = MID$(fill$(m), c(m), 1)
                                LOOP UNTIL c(m) = LEN(fill$(m))
                                cbf(m) = c(m)
                                PRINT fill$(m);
                            END IF
                        ELSE
                            LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                            COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                            PRINT text$(m) + " ";
                            IF c(m) > 0 THEN
                                cbf(m) = 0
                                DO
                                    cbf(m) = cbf(m) + 1
                                    PRINT char$(m, cbf(m));
                                LOOP UNTIL cbf(m) = c(m)
                            END IF
                        END IF
                        Taste$ = ""
                    END IF
                    IF activem = m THEN 'only checks keys if input is active
                        loopback:
                        IF Taste$ <> "" THEN
                            ac = 0
                            DO
                                ac = ac + 1
                                IF ac$(ac) = Taste$ THEN
                                    c(m) = c(m) + 1
                                    char$(m, c(m)) = Taste$
                                END IF
                            LOOP UNTIL ac = alch
                        END IF
                        editpos(m) = c(m)

                        'prints the new character if g has changed
                        IF cbf(m) <> c(m) OR selectedall <> colorbf THEN
                            IF cbf(m) < c(m) OR selectedall <> colorbf THEN
                                LOCATE firstline + yoffset(m), firstchar + xoffset(m)
                                COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                                PRINT text$(m);
                                IF selectedall = 0 THEN
                                    COLOR _RGBA(r(2), g(2), b(2), a(2)), _RGBA(r(1), g(1), b(1), a(1))
                                ELSE
                                    COLOR _RGBA(r(1), g(1), b(1), a(1)), _RGBA(r(2), g(2), b(2), a(2))
                                END IF
                                IF c(m) > 0 THEN
                                    cbf(m) = 0
                                    DO
                                        cbf(m) = cbf(m) + 1
                                        PRINT char$(m, cbf(m));
                                    LOOP UNTIL cbf(m) = c(m)
                                END IF
                            END IF
                        END IF
                        cbf(m) = c(m)

                        'blinking line after text
                        IF TIMER MOD 2 = 0 THEN
                            LINE ((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2), (firstline + yoffset(m) - 1) * fontheight + 1)-((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2) + 2, (firstline + yoffset(m)) * fontheight - 1), _RGBA(r(2), g(2), b(2), a(2)), BF
                        ELSE
                            LINE ((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2), (firstline + yoffset(m) - 1) * fontheight + 1)-((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2) + 2, (firstline + yoffset(m)) * fontheight - 1), _RGBA(r(1), g(1), b(1), a(1)), BF
                        END IF

                        'key cases
                        SELECT CASE Taste$
                            CASE IS = CHR$(8)
                                backspace(m) = 1
                                GOTO printback
                            CASE IS = CHR$(3) 'ctrl + c input data
                                _CLIPBOARD$ = ""
                                p = 0
                                DO
                                    p = p + 1
                                    _CLIPBOARD$ = _CLIPBOARD$ + char$(m, p)
                                LOOP UNTIL p = c(m)
                            CASE IS = CHR$(22) 'ctrl + v into active input
                                IF _CLIPBOARD$ <> "" THEN
                                    p = 0
                                    DO
                                        p = p + 1
                                        c(m) = c(m) + 1
                                        char$(m, c(m)) = MID$(_CLIPBOARD$, p, 1)
                                    LOOP UNTIL p = LEN(_CLIPBOARD$)
                                END IF
                            CASE IS = CHR$(0) + CHR$(75)
                                IF editpos(m) > 1 THEN
                                    editpos(m) = editpos(m) - 1
                                END IF
                            CASE IS = CHR$(0) + CHR$(77)
                                IF editpos(m) < c(m) THEN
                                    editpos(m) = editpos(m) + 1
                                END IF
                            CASE IS = CHR$(13)
                                IF activem = maxm THEN
                                    status$(activem) = "finished"
                                ELSE
                                    activem = activem + 1
                                END IF
                            CASE IS = CHR$(0) + CHR$(59)
                                colorbf = selectedall
                                IF selectedall = 1 THEN
                                    selectedall = 0
                                ELSE
                                    selectedall = 1
                                END IF
                                firstprint(m) = 1
                        END SELECT
                    ELSE
                        IF _MOUSEINPUT = -1 THEN
                            IF _MOUSEBUTTON(1) = -1 THEN
                                IF _MOUSEX > (firstchar + xoffset(m)) * fontwidth + (fontwidth / 2) AND _MOUSEX < (firstchar + xoffset(m) + LEN(text$(m)) + c(m)) * fontwidth + (fontwidth / 2) THEN
                                    IF _MOUSEY > (firstline + yoffset(m) - 1) * fontheight + 1 AND _MOUSEY < (firstline + yoffset(m)) * fontheight - 1 THEN
                                        checkm = 0: dontgoactive = 0
                                        DO
                                            checkm = checkm + 1
                                            IF expanded(checkm) = 1 THEN dontgoactive = 1 ELSE dontgoactive = 0
                                        LOOP UNTIL checkm = maxm
                                        IF dontgoactive = 0 THEN activem = m
                                    END IF
                                END IF
                            END IF
                            'deletes blinking line after text
                            editpos(m) = c(m)
                            IF TIMER MOD 2 = 0 THEN LINE ((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2), (firstline + yoffset(m) - 1) * fontheight + 1)-((firstchar + xoffset(m) + LEN(text$(m)) + editpos(m)) * fontwidth + (fontwidth / 2) + 2, (firstline + yoffset(m)) * fontheight - 1), _RGBA(r(1), g(1), b(1), a(1)), BF

                        END IF
                    END IF
            END SELECT
        LOOP UNTIL m = maxm
    LOOP UNTIL status$(maxmi) = "finished" OR maxmi = 0 OR Taste$ = CHR$(27)
    IF Taste$ = CHR$(27) THEN endparameter$ = "aborted"
    ERASE status$
    m = 0
    DO
        m = m + 1
        SELECT CASE TYPE$(m)
            CASE IS = "input"
                IF c(m) > 0 THEN
                    maxg(m) = c(m)
                    c(m) = 0
                    DO
                        c(m) = c(m) + 1
                        UserInput$(m) = UserInput$(m) + char$(m, c(m))
                    LOOP UNTIL c(m) = maxg(m)
                    c(m) = 0
                    f = 0
                    DO
                        f = f + 1
                        char$(m, f) = ""
                    LOOP UNTIL char$(m, f + 1) = ""
                ELSE
                    UserInput$(m) = ""
                END IF
        END SELECT
    LOOP UNTIL m = maxm
END SUB
