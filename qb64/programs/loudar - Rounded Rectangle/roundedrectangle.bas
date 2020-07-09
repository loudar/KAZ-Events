CLS
CLEAR
CLOSE

SCREEN _NEWIMAGE(1920, 1080, 32)

lx = 100
ly = 100
ux = 500
uy = 300

rectangle lx, ly, ux, uy, 50, _RGBA(255, 255, 255, 255), "BF"
SLEEP
SYSTEM

SUB rectangle (lx, ly, ux, uy, round, clr&, outline$)
    SELECT CASE outline$
        CASE IS = "BF"
            rectangleoutline lx, ly, ux, uy, round, clr&
            PAINT (lx + ((ux - lx) / 2), ly + ((uy - ly) / 2)), clr&, clr&
        CASE IS = "B"
            rectangleoutline lx, ly, ux, uy, round, clr&
    END SELECT
END SUB

SUB rectangleoutline (lx, ly, ux, uy, round, clr&)
    IF round > 0 THEN
        '           corners:
        detail = 2
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
