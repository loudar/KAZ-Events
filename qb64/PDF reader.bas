'PDF Reader

prin = 0
OPEN "Test.pdf" FOR BINARY AS #1
IF EOF(1) = 0 THEN
    DO
        LINE INPUT #1, line$
        IF MID$(line$, LEN(line$) - 3, 4) = " obj" THEN
            DO
                LINE INPUT #1, line$
                IF line$ = "<</Filter/FlateDecode/Length 29044/Length1 97932>>" THEN
                    prin = 1
                END IF
                IF MID$(line$, 1, 2) = "<<" THEN
                    parstart = INSTR(3, line$, "/")
                    parend = INSTR(parstart + 1, line$, "/")
                    IF parend = 0 THEN parend = LEN(line$) - 1
                    parameter$ = MID$(line$, parstart + 1, parend - parstart - 1)
                    IF prin = 1 THEN PRINT parameter$,: SLEEP
                    SELECT CASE parameter$
                        CASE "Filter"
                            metend = INSTR(parend + 1, line$, "/")
                            IF metend = 0 THEN metend = LEN(line$) - 1
                            method$ = MID$(line$, parend + 1, metend - parend - 1)
                            IF prin = 1 THEN PRINT method$: SLEEP
                            SELECT CASE method$
                                CASE "FlateDecode"
                                    LINE INPUT #1, line$
                                    IF line$ = "stream" THEN
                                        flated$ = ""
                                        DO
                                            LINE INPUT #1, line$
                                            IF line$ <> "endstream" THEN
                                                flated$ = flated$ + line$
                                            END IF
                                        LOOP UNTIL line$ = "endstream"
                                        inflated$ = _INFLATE$(flated$)
                                        IF prin = 1 THEN PRINT inflated$, LEN(inflated$)
                                        SLEEP
                                        prin = 0
                                    END IF
                            END SELECT
                    END SELECT
                END IF
            LOOP UNTIL line$ = "endobj"
        END IF
    LOOP UNTIL EOF(1) = -1
END IF
CLOSE #1
