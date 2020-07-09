OPEN "test.txt" FOR INPUT AS #1
IF EOF(1) = 0 THEN
    DO
        LINE INPUT #1, text$
        p = 0: DO: p = p + 1
            PRINT ASC(MID$(text$, p, 1)),
        LOOP UNTIL p = LEN(text$)
        PRINT ""
    LOOP UNTIL EOF(1) = -1
END IF
CLOSE #1
