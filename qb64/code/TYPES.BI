DIM SHARED max(12)
maxuser = 100
maxobj = 5
maxausgaben = 12 * 100 'Monate * Jahre
maxort = 2000
maxplz = 2000
maxadresse = 5000
maxkonto = 5000
maxrubrik = 200
maxkategorie = 200
maxveranstalter = 2000
maxkleinanzeige = 1000
maxveranstaltung = 10000

'   LEVEL 1 (Basisdaten)
'---------------------------------------------------
TYPE User
    Name AS STRING * 50
    Profilbild AS STRING * 54
    Passwort AS STRING * 50
    Zugang AS _BYTE
    Abteilung AS STRING * 50
    Telefon AS INTEGER
    LetzterLogin AS STRING * 41
END TYPE
DIM SHARED User(maxuser) AS User
DIM SHARED pfp&

TYPE Objekt
    ID AS _BYTE
    Name AS STRING * 50
    Ausgabenfrequenz AS _BYTE
    Waehrung AS STRING * 50
END TYPE
DIM SHARED Objekt(maxobj) AS Objekt

TYPE Ausgabe
    ID AS INTEGER
    Objekt AS _BYTE
    Monat AS LONG
    Anfang AS STRING * 50
    Ende AS STRING * 50
END TYPE
DIM SHARED Ausgabe(maxausgaben) AS Ausgabe

TYPE Ort
    Kuerzel AS STRING * 50
    Name AS STRING * 50
END TYPE
DIM SHARED Ort(maxort) AS Ort

TYPE PLZ
    PLZ AS INTEGER
    Land AS STRING * 50
    Ort AS STRING * 50
END TYPE
DIM SHARED PLZ(maxplz) AS PLZ

TYPE Adresse
    ID AS INTEGER
    PLZ AS INTEGER
    Ort AS STRING * 50
    Land AS STRING * 50
    Strasse AS STRING * 50
END TYPE
DIM SHARED Adresse(maxadresse) AS Adresse

TYPE Konto
    ID AS INTEGER
    BIC AS STRING * 50
    IBAN AS STRING * 50
    Inhaber AS STRING * 50
    Adresse AS INTEGER
END TYPE
DIM SHARED Konto(maxkonto) AS Konto

'   LEVEL 2 (Semi-permanente Listen)
'---------------------------------------------------
TYPE Rubrik
    Kuerzel AS STRING * 50
    Objekt AS _BYTE
    Name AS STRING * 50
END TYPE
DIM SHARED Rubrik(maxrubrik) AS Rubrik

TYPE Kategorie
    Kuerzel AS STRING * 50
    Objekt AS _BYTE
    Name AS STRING * 50
END TYPE
DIM SHARED Kategorie(maxkategorie) AS Kategorie

TYPE Veranstalter
    Kuerzel AS STRING * 50
    Name AS STRING * 50
    Adresse AS INTEGER
    Telefon AS INTEGER
    Telefax AS INTEGER
    Sachbearbeiter AS STRING * 50
    Anrede AS STRING * 50
    Notiz AS STRING * 50
END TYPE
DIM SHARED Veranstalter(maxveranstalter) AS Veranstalter

'   LEVEL 3 (Einzelne Objekte)
'---------------------------------------------------
TYPE Kleinanzeige
    ID AS INTEGER
    Kategorie1 AS STRING * 50
    Kategorie2 AS STRING * 50
    Kategorie3 AS STRING * 50
    Text AS STRING * 500
    Titel AS STRING * 50
    Objekt AS _BYTE
    Ausgabe AS LONG
    Telefon AS STRING * 20
    Name AS STRING * 50
    Chiffre AS STRING * 50
    Notiz AS STRING * 50
END TYPE
DIM SHARED Kleinanzeige(maxkleinanzeige) AS Kleinanzeige

TYPE Veranstaltung
    ID AS INTEGER
    Ausgabe AS LONG
    Datum AS STRING * 50
    Ort AS STRING * 50
    Veranstalter AS STRING * 50
    Rubrik AS STRING * 50
    Zeit1 AS STRING * 20
    Zeit2 AS STRING * 20
    Zeit3 AS STRING * 20
    Zeitcode AS STRING * 20
    Titel AS STRING * 200
    Text AS STRING * 500
    TextLang AS STRING * 2000
END TYPE
DIM SHARED Veranstaltung(maxveranstaltung) AS Veranstaltung
DIM SHARED vadr$
