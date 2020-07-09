REM "3D Engine Test"

CLS
CLEAR
CLOSE

'ON ERROR GOTO errorhandler
GOTO start

errorhandler:

RESUME NEXT

start:
_TITLE "3D Engine Test"

DIM SHARED maxx
DIM SHARED maxy
DIM SHARED xmiddle
DIM SHARED ymiddle
DIM SHARED xdif
DIM SHARED ydif
maxx = 1920
maxy = 1080
xmiddle = maxx / 2
ymiddle = maxy / 2
SCREEN _NEWIMAGE(maxx, maxy, 32)
DO: LOOP UNTIL _SCREENEXISTS
_SCREENMOVE (_DESKTOPWIDTH / 2) - (maxx / 2), (_DESKTOPHEIGHT / 2) - (maxy / 2)

defineVariables:
TYPE viewDirection
    x AS LONG
    y AS LONG
    z AS LONG
END TYPE
DIM SHARED viewDirection AS viewDirection

TYPE viewPosition
    x AS LONG
    y AS LONG
    z AS LONG
END TYPE
DIM SHARED viewPosition AS viewPosition

maxobjects = 10
TYPE object
    type AS STRING
END TYPE
DIM SHARED object(maxobjects) AS object

TYPE cube
    posx AS LONG
    posy AS LONG
    posz AS LONG
    rotx AS LONG
    roty AS LONG
    rotz AS LONG
    texture AS STRING
END TYPE
DIM SHARED cube(maxobjects) AS cube

TYPE light
    posx AS LONG
    posy AS LONG
    posz AS LONG
    brightness AS INTEGER
END TYPE
DIM SHARED light(maxobjects) AS light

TYPE Vector2D
    x AS DOUBLE
    y AS DOUBLE
END TYPE

maxdots = 500
TYPE dot
    x AS LONG
    y AS LONG
    z AS LONG
    sx AS LONG
    sy AS LONG
    visible AS INTEGER
END TYPE
DIM SHARED dot(maxobjects, maxdots) AS dot
DIM SHARED gdot(maxdots) AS dot

maxconnections = 10
DIM SHARED dcon(maxobjects, maxdots, maxconnections) AS INTEGER

TYPE v
    x AS LONG
    y AS LONG
    z AS LONG
    length AS LONG
END TYPE
DIM SHARED v(3) AS v

DIM SHARED hiddens(maxobjects, maxdots)

DIM SHARED maxo
DIM SHARED o
DIM SHARED mousespeed: mousespeed = 1
DIM SHARED cmousex
DIM SHARED cmousey
DIM SHARED rotationspeed: rotationspeed = 0.02
DIM SHARED walkingspeed: walkingspeed = 3
DIM SHARED fov: fov = 120
DIM SHARED zscale: zscale = 100 / fov
DIM SHARED worldscale: worldscale = 10
DIM SHARED nhat(3): DIM SHARED uhat(3): DIM SHARED vhat(3)
DIM SHARED jumpheight: jumpheight = 10
DIM SHARED startjumptimer
DIM SHARED jumpactive
DIM SHARED change
DIM SHARED renderdistance: renderdistance = 50
DIM SHARED nearclipping: nearclipping = -5
DIM SHARED gridview: gridview = 1
DIM SHARED maxgdot
DIM SHARED worldbrightness: worldbrightness = 100
DIM SHARED rerun
DIM SHARED zoom: zoom = 1

RANDOMIZE TIMER

createElements:
o = 0: DO: o = o + 1
    object(o).type = ""
LOOP UNTIL o = maxobjects
o = 0
r = 0: DO: r = r + 1
    z = INT(RND * 40) + 10
    y = INT(RND * 30)
    x = INT(RND * 100) - 50
    S = INT(RND * 10) + 5
    rx = INT(RND * 360)
    ry = INT(RND * 360)
    rz = INT(RND * 360)
    newcube x, y, z, S, rx, ry, rz, "reflective"
LOOP UNTIL r = 5
run3d 15
IF rerun = 1 THEN GOTO createElements

SUB run3d (fps)
    maxo = o
    change = 1
    startframe = 1
    calcHat
    defgrid
    'lockMouse
    go.up
    '_MOUSEHIDE
    DO
        IF startframe = 1 THEN framestart = VAL(MID$(TIME$, 7, 2))
        'checkView
        Key$ = INKEY$
        SELECT CASE Key$
            CASE IS = CHR$(27)
                rerun = 0
                end3d = 1
            CASE IS = CHR$(0) + CHR$(77)
                rotate.right
            CASE IS = CHR$(0) + CHR$(75)
                rotate.left
            CASE IS = CHR$(0) + CHR$(80)
                rotate.down
            CASE IS = CHR$(0) + CHR$(72)
                rotate.up
            CASE IS = "w"
                go.forward
            CASE IS = "s"
                go.back
            CASE IS = "d"
                go.right
            CASE IS = "a"
                go.left
            CASE IS = " "
                jump
            CASE IS = "r"
                rerun = 1
                end3d = 1
            CASE IS = "+"
                fov = fov + 10
            CASE IS = "-"
                fov = fov - 10
        END SELECT
        IF jumpactive = 1 THEN
            IF TIMER - startjumptimer >= 0.5 THEN
                jumpactive = 0
                go.down
                change = 1
            END IF
        END IF
        'IF TIMER MOD 2 = 1 AND timerlock = 0 THEN
        '    rotate 1, 0, 2, 0: change = 1
        '    timerlock = 1
        'ELSE
        '    timerlock = 0
        'END IF
        IF Key$ <> "" THEN change = 1
        IF change = 1 THEN
            change = 0
            CLS
            'world grid
            IF gridview = 1 THEN
                d = 0: DO: d = d + 1
                    calcgrid d
                LOOP UNTIL d = maxgdot
                d = -1: DO: d = d + 2
                    IF gdot(d).z > 0 THEN
                        cline gdot(d).sx, gdot(d).sy, gdot(d + 1).sx, gdot(d + 1).sy, colour&("grid")
                    END IF
                LOOP UNTIL d = maxgdot - 1
            END IF
            IF maxo > 0 THEN

                'calculating position
                o = 0: DO: o = o + 1
                    SELECT CASE object(o).type
                        CASE IS = "cube"
                            d = 0: DO: d = d + 1
                                calcpos o, cube(o).posx, cube(o).posy, cube(o).posz, d
                            LOOP UNTIL d = 8
                    END SELECT
                LOOP UNTIL o = maxo
                'drawing
                o = 0: DO: o = o + 1
                    SELECT CASE object(o).type
                        CASE IS = "cube"
                            col = colour&(cube(o).texture)
                            IF facehidden(o, 1, 2, 4, 3) = 0 THEN cplane o, 1, 2, 4, 3, col
                            IF facehidden(o, 1, 2, 6, 5) = 0 THEN cplane o, 1, 2, 6, 5, col
                            IF facehidden(o, 3, 4, 8, 7) = 0 THEN cplane o, 3, 4, 8, 7, col
                            IF facehidden(o, 1, 3, 7, 5) = 0 THEN cplane o, 1, 3, 7, 5, col
                            IF facehidden(o, 5, 6, 4, 7) = 0 THEN cplane o, 5, 6, 8, 7, col
                            IF facehidden(o, 2, 4, 8, 6) = 0 THEN cplane o, 2, 4, 8, 6, col
                    END SELECT
                LOOP UNTIL o = maxo
            END IF
            IF VAL(MID$(TIME$, 7, 2)) > framestart OR VAL(MID$(TIME$, 7, 2)) = 0 THEN
                startframe = 1
                frames = frame
                frame = 0
            ELSE startframe = 0: frame = frame + 1
            END IF
            LOCATE 1, 1
            PRINT "fps: "; frames
            PRINT "fov: "; fov
            _DISPLAY
        END IF
        _LIMIT fps
    LOOP UNTIL end3d = 1
    _MOUSESHOW
    IF rerun = 0 THEN SYSTEM
END SUB

SUB cpset (x1, y1, col)
    PSET (maxx / 2 + x1, y1 + maxy / 2), col
END SUB

SUB cline (x1, y1, x2, y2, col)
    LINE (maxx / 2 + x1, y1 + maxy / 2)-(maxx / 2 + x2, y2 + maxy / 2), col
END SUB

SUB ccircle (x1 AS DOUBLE, y1 AS DOUBLE, rad AS DOUBLE, col AS _UNSIGNED LONG)
    CIRCLE (_WIDTH / 2 + x1, y1 + _HEIGHT / 2), rad, col
END SUB

SUB cplane (o, d1, d2, d3, d4, col)
    IF gridview = 1 THEN col2 = colour&("white") ELSE col2 = col
    length = dot(o, d3).sx - dot(o, d1).sx
    height = dot(o, d3).sy - dot(o, d1).sy
    middlex = dot(o, d1).sx + (length / 2)
    middley = dot(o, d1).sy + (height / 2)
    cline dot(o, d1).sx, dot(o, d1).sy, dot(o, d2).sx, dot(o, d2).sy, col2
    cline dot(o, d2).sx, dot(o, d2).sy, dot(o, d3).sx, dot(o, d3).sy, col2
    cline dot(o, d3).sx, dot(o, d3).sy, dot(o, d4).sx, dot(o, d4).sy, col2
    cline dot(o, d1).sx, dot(o, d1).sy, dot(o, d4).sx, dot(o, d4).sy, col2
    PAINT ((maxx / 2) + middlex, (maxy / 2) + middley), col, col2
END SUB

SUB defgrid
    leftx = -50
    rightx = 50
    gridheight = -1
    gridoffset = 0
    z = 5: d = -1: DO
        z = z + 5
        d = d + 2
        gdot(d).x = leftx
        gdot(d).y = gridheight
        gdot(d).z = z + gridoffset
        gdot(d + 1).x = rightx
        gdot(d + 1).y = gridheight
        gdot(d + 1).z = z + gridoffset
    LOOP UNTIL d = 9
    maxgdot = 10
END SUB

SUB calcgrid (d)
    vec3Ddotnhat = gdot(d).x * nhat(1) + gdot(d).y * nhat(2) + (gdot(d).z * zoom) * nhat(3)
    gdot(d).sx = (gdot(d).x * uhat(1) + gdot(d).y * uhat(2) + (gdot(d).z * zoom) * uhat(3)) * fov / vec3Ddotnhat
    gdot(d).sy = (gdot(d).x * vhat(1) + gdot(d).y * vhat(2) + (gdot(d).z * zoom) * vhat(3)) * fov / vec3Ddotnhat
    IF gdot(d).sx >= -(maxx / 2) AND gdot(d).sx <= maxx / 2 AND gdot(d).sy >= -(maxy / 2) AND gdot(d).sy <= maxy / 2 AND (gdot(d).z * zoom) < renderdistance AND (gdot(d).z * zoom) > nearclipping THEN
        gdot(d).visible = 1
    ELSE
        gdot(d).visible = 0
    END IF
END SUB

SUB calcpos (o, posx, posy, posz, d)
    x = posx + dot(o, d).x
    y = posy + dot(o, d).y
    z = (posz + dot(o, d).z)
    vec3Ddotnhat = (x * 1) * nhat(1) + (y * 1) * nhat(2) + (z * zoom) * nhat(3)
    dot(o, d).sx = ((x * 1) * uhat(1) + (y * 1) * uhat(2) + (z * zoom) * uhat(3)) * fov / vec3Ddotnhat
    dot(o, d).sy = ((x * 1) * vhat(1) + (y * 1) * vhat(2) + (z * zoom) * vhat(3)) * fov / vec3Ddotnhat
    IF dot(o, d).sx >= -(maxx / 2) AND dot(o, d).sx <= maxx / 2 AND dot(o, d).sy >= -(maxy / 2) AND dot(o, d).sy <= maxy / 2 AND (t * zoom) < renderdistance AND (z * zoom) > nearclipping THEN
        dot(o, d).visible = 1
    ELSE
        dot(o, d).visible = 0
    END IF
END SUB

FUNCTION lit (o, d1, d2, d3, d4)
    IF hiddens(o, d1) = 0 AND hiddens(o, d2) = 0 AND hiddens(o, d3) = 0 AND hiddens(o, d4) = 0 THEN

    END IF
END FUNCTION

FUNCTION facehidden (o, d1, d2, d3, d4)
    IF hidden(o, d1) = 0 AND hidden(o, d2) = 0 AND hidden(o, d3) = 0 AND hidden(o, d4) = 0 THEN facehidden = 0 ELSE facehidden = 1
END FUNCTION

FUNCTION hidden (o, d)
    IF dot(o, d).visible = 1 THEN
        o2 = 0: DO: o2 = o2 + 1
            SELECT CASE object(o2).type
                CASE IS = "cube"
                    d2 = 0: DO: d2 = d2 + 1
                        d3 = 0: DO: d3 = d3 + 1
                            d4 = 0: DO: d4 = d4 + 1
                                IF o2 = o THEN
                                    IF d4 <> d3 AND d4 <> d2 AND d3 <> d2 AND d4 <> d AND d3 <> d AND d2 <> d THEN
                                        IF obscured(o, o2, d, d2, d3, d4) = 1 THEN
                                            hidden = 1
                                            EXIT FUNCTION
                                        END IF
                                    END IF
                                ELSE
                                    IF obscured(o, o2, d, d2, d3, d4) = 1 THEN
                                        hidden = 1
                                        u1 = 1
                                        u2 = 1
                                        u1step = 1
                                        u2step = 1
                                        DIM v1 AS Vector2D
                                        DIM v2 AS Vector2D
                                        'v1.x = dot(o,
                                        DO
                                            'IF
                                        LOOP UNTIL dif < 0.1 OR dif = difbf
                                        LOCATE 4, 1
                                        PRINT u1, u2
                                        SLEEP
                                        EXIT FUNCTION
                                    END IF
                                END IF
                            LOOP UNTIL d4 = 8
                        LOOP UNTIL d3 = 8
                    LOOP UNTIL d2 = 8
            END SELECT
        LOOP UNTIL o2 = maxo
    ELSE
        hidden = 1
        EXIT FUNCTION
    END IF
    hidden = 0
END FUNCTION

FUNCTION obscured (o, o2, d, d2, d3, d4)
    DIM s12inside AS INTEGER
    DIM s23inside AS INTEGER
    DIM s31inside AS INTEGER
    DIM dz AS Vector2D
    DIM ta AS Vector2D
    ' side 12
    dz.x = dot(o, d).sx - dot(o2, d2).sx
    dz.y = dot(o, d).sy - dot(o2, d2).sy
    ta.x = dot(o2, d3).sx - dot(o2, d2).sx
    ta.y = dot(o2, d3).sy - dot(o2, d2).sy
    IF CrossProduct(ta.x, ta.y, dz.x, dz.y) > 0 THEN s12inside = 1 ELSE s12inside = 0
    ' side 23
    dz.x = dot(o, d).sx - dot(o2, d3).sx
    dz.y = dot(o, d).sy - dot(o2, d3).sy
    ta.x = dot(o2, d4).sx - dot(o2, d3).sx
    ta.y = dot(o2, d4).sy - dot(o2, d3).sy
    IF CrossProduct(ta.x, ta.y, dz.x, dz.y) > 0 THEN s23inside = 1 ELSE s23inside = 0
    ' side 31
    dz.x = dot(o, d).sx - dot(o2, d4).sx
    dz.y = dot(o, d).sy - dot(o2, d4).sy
    ta.x = dot(o2, d2).sx - dot(o2, d4).sx
    ta.y = dot(o2, d2).sy - dot(o2, d4).sy
    IF CrossProduct(ta.x, ta.y, dz.x, dz.y) > 0 THEN s31inside = 1 ELSE s31inside = 0
    IF s12inside = 1 AND s23inside = 1 AND s31inside = 1 THEN
        obscured = 1
    ELSE
        obscured = 0
    END IF
END FUNCTION

FUNCTION CrossProduct (a.x, a.y, b.x, b.y)
    CrossProduct = a.x * b.y - b.x * a.y
END FUNCTION

SUB jump
    IF jumpactive = 0 THEN
        jumpactive = 1
        go.up
        startjumptimer = TIMER
    END IF
END SUB

SUB go.up
    d = 0: DO: d = d + 1
        gdot(d).y = gdot(d).y - jumpheight
    LOOP UNTIL d = maxgdot
    o = 0: DO: o = o + 1
        SELECT CASE object(o).type
            CASE IS = "cube"
                d = 0: DO: d = d + 1
                    dot(o, d).y = dot(o, d).y - jumpheight
                LOOP UNTIL d = 8
        END SELECT
    LOOP UNTIL o = maxo
END SUB

SUB go.down
    d = 0: DO: d = d + 1
        gdot(d).y = gdot(d).y + jumpheight
    LOOP UNTIL d = maxgdot
    o = 0: DO: o = o + 1
        SELECT CASE object(o).type
            CASE IS = "cube"
                d = 0: DO: d = d + 1
                    dot(o, d).y = dot(o, d).y + jumpheight
                LOOP UNTIL d = 8
        END SELECT
    LOOP UNTIL o = maxo
END SUB

SUB go.forward
    d = 0: DO: d = d + 1
        gdot(d).z = gdot(d).z - walkingspeed
    LOOP UNTIL d = maxgdot
    o = 0: DO: o = o + 1
        SELECT CASE object(o).type
            CASE IS = "cube"
                d = 0: DO: d = d + 1
                    dot(o, d).z = dot(o, d).z - walkingspeed
                LOOP UNTIL d = 8
        END SELECT
    LOOP UNTIL o = maxo
END SUB

SUB go.back
    d = 0: DO: d = d + 1
        gdot(d).z = gdot(d).z + walkingspeed
    LOOP UNTIL d = maxgdot
    o = 0: DO: o = o + 1
        SELECT CASE object(o).type
            CASE IS = "cube"
                d = 0: DO: d = d + 1
                    dot(o, d).z = dot(o, d).z + walkingspeed
                LOOP UNTIL d = 8
        END SELECT
    LOOP UNTIL o = maxo
END SUB

SUB go.right
    d = 0: DO: d = d + 1
        gdot(d).x = gdot(d).x + walkingspeed
    LOOP UNTIL d = maxgdot
    o = 0: DO: o = o + 1
        SELECT CASE object(o).type
            CASE IS = "cube"
                d = 0: DO: d = d + 1
                    dot(o, d).x = dot(o, d).x + walkingspeed
                LOOP UNTIL d = 8
        END SELECT
    LOOP UNTIL o = maxo
END SUB

SUB go.left
    d = 0: DO: d = d + 1
        gdot(d).x = gdot(d).x - walkingspeed
    LOOP UNTIL d = maxgdot
    o = 0: DO: o = o + 1
        SELECT CASE object(o).type
            CASE IS = "cube"
                d = 0: DO: d = d + 1
                    dot(o, d).x = dot(o, d).x - walkingspeed
                LOOP UNTIL d = 8
        END SELECT
    LOOP UNTIL o = maxo
END SUB

SUB rotate.counterclockwise
    v1 = vhat(1)
    v2 = vhat(2)
    v3 = vhat(3)
    vhat(1) = vhat(1) + uhat(1) * rotationspeed
    vhat(2) = vhat(2) + uhat(2) * rotationspeed
    vhat(3) = vhat(3) + uhat(3) * rotationspeed
    uhat(1) = uhat(1) - v1 * rotationspeed
    uhat(2) = uhat(2) - v2 * rotationspeed
    uhat(3) = uhat(3) - v3 * rotationspeed
    normalize.screen.vectors
END SUB

SUB rotate.clockwise
    v1 = vhat(1)
    v2 = vhat(2)
    v3 = vhat(3)
    vhat(1) = vhat(1) - uhat(1) * rotationspeed
    vhat(2) = vhat(2) - uhat(2) * rotationspeed
    vhat(3) = vhat(3) - uhat(3) * rotationspeed
    uhat(1) = uhat(1) + v1 * rotationspeed
    uhat(2) = uhat(2) + v2 * rotationspeed
    uhat(3) = uhat(3) + v3 * rotationspeed
    normalize.screen.vectors
END SUB

SUB rotate.right
    uhat(1) = uhat(1) + nhat(1) * rotationspeed
    uhat(2) = uhat(2) + nhat(2) * rotationspeed
    uhat(3) = uhat(3) + nhat(3) * rotationspeed
    normalize.screen.vectors
END SUB

SUB rotate.left
    uhat(1) = uhat(1) - nhat(1) * rotationspeed
    uhat(2) = uhat(2) - nhat(2) * rotationspeed
    uhat(3) = uhat(3) - nhat(3) * rotationspeed
    normalize.screen.vectors
END SUB

SUB rotate.up
    vhat(1) = vhat(1) + nhat(1) * rotationspeed
    vhat(2) = vhat(2) + nhat(2) * rotationspeed
    vhat(3) = vhat(3) + nhat(3) * rotationspeed
    normalize.screen.vectors
END SUB

SUB rotate.down
    vhat(1) = vhat(1) - nhat(1) * rotationspeed
    vhat(2) = vhat(2) - nhat(2) * rotationspeed
    vhat(3) = vhat(3) - nhat(3) * rotationspeed
    normalize.screen.vectors
END SUB

SUB normalize.screen.vectors
    uhatmag = SQR(uhat(1) * uhat(1) + uhat(2) * uhat(2) + uhat(3) * uhat(3))
    uhat(1) = uhat(1) / uhatmag: uhat(2) = uhat(2) / uhatmag: uhat(3) = uhat(3) / uhatmag
    vhatmag = SQR(vhat(1) * vhat(1) + vhat(2) * vhat(2) + vhat(3) * vhat(3))
    vhat(1) = vhat(1) / vhatmag: vhat(2) = vhat(2) / vhatmag: vhat(3) = vhat(3) / vhatmag
    uhatdotvhat = uhat(1) * vhat(1) + uhat(2) * vhat(2) + uhat(3) * vhat(3)
    ' The normal vector points toward the eye.
    nhat(1) = uhat(2) * vhat(3) - uhat(3) * vhat(2)
    nhat(2) = uhat(3) * vhat(1) - uhat(1) * vhat(3)
    nhat(3) = uhat(1) * vhat(2) - uhat(2) * vhat(1)
    nhatmag = SQR(nhat(1) * nhat(1) + nhat(2) * nhat(2) + nhat(3) * nhat(3))
    nhat(1) = nhat(1) / nhatmag: nhat(2) = nhat(2) / nhatmag: nhat(3) = nhat(3) / nhatmag
END SUB

SUB calcHat
    nhat(1) = 0
    nhat(2) = 0
    nhat(3) = -1
    uhat(1) = 1
    uhat(2) = 0
    uhat(3) = 0
    vhat(1) = 0
    vhat(2) = 1
    vhat(3) = 0
    cmousex = xmiddle
    cmousey = ymiddle
END SUB

SUB lockMouse
    _MOUSEMOVE xmiddle, ymiddle
    cmousex = xmiddle
    cmousey = ymiddle
END SUB

SUB checkView
    lockMouse
    IF _MOUSEINPUT = -1 AND _MOUSEBUTTON(1) = -1 THEN
        cmousex = _MOUSEX
        cmousey = _MOUSEY
        IF cmousex > xmiddle + 2 THEN xdif = 1
        IF cmousex < xmiddle - 2 THEN xdif = -1
        IF cmousey > ymiddle + 2 THEN ydif = 1
        IF cmousey < ymiddle - 2 THEN ydif = -1
        IF xdif = 1 THEN
            lockMouse
            rotate.left
            change = 1
        END IF
        IF xdif = -1 THEN
            lockMouse
            rotate.right
            change = 1
        END IF
        IF ydif = 1 THEN
            lockMouse
            rotate.up
            change = 1
        END IF
        IF ydif = -1 THEN
            lockMouse
            rotate.down
            change = 1
        END IF
        xdif = 0
        ydif = 0
    END IF
END SUB

SUB newcube (posx, posy, posz, size, rotx, roty, rotz, texture$)
    o = o + 1
    object(o).type = "cube"
    cube(o).texture = texture$
    cube(o).posx = posx
    cube(o).posy = posy
    cube(o).posz = posz * zscale
    setdots o, size
    rotate o, rotx, roty, rotz
END SUB

SUB setdots (o, size)
    SELECT CASE object(o).type
        CASE IS = "cube"
            dot(o, 1).x = -(size / 2)
            dot(o, 1).y = (size / 2)
            dot(o, 1).z = (size / 2) * zscale
            dot(o, 2).x = (size / 2)
            dot(o, 2).y = (size / 2)
            dot(o, 2).z = (size / 2) * zscale
            dot(o, 3).x = -(size / 2)
            dot(o, 3).y = (size / 2)
            dot(o, 3).z = -(size / 2) * zscale
            dot(o, 4).x = (size / 2)
            dot(o, 4).y = (size / 2)
            dot(o, 4).z = -(size / 2) * zscale
            dot(o, 5).x = -(size / 2)
            dot(o, 5).y = -(size / 2)
            dot(o, 5).z = (size / 2) * zscale
            dot(o, 6).x = (size / 2)
            dot(o, 6).y = -(size / 2)
            dot(o, 6).z = (size / 2) * zscale
            dot(o, 7).x = -(size / 2)
            dot(o, 7).y = -(size / 2)
            dot(o, 7).z = -(size / 2) * zscale
            dot(o, 8).x = (size / 2)
            dot(o, 8).y = -(size / 2)
            dot(o, 8).z = -(size / 2) * zscale
            RESTORE cubeconnections
            d = 0: DO: d = d + 1
                READ dcon(o, d, 1), dcon(o, d, 2), dcon(o, d, 3)
            LOOP UNTIL d = 8
    END SELECT
END SUB

SUB rotate (o, rotx, roty, rotz)
    SELECT CASE object(o).type
        CASE IS = "cube"
            d = 0: DO: d = d + 1
                'x-axis rotation
                IF rotx > 0 THEN
                    sintheta = SIN(rotx)
                    costheta = COS(rotx)
                    y = dot(o, d).y
                    z = dot(o, d).z
                    dot(o, d).y = (y * costheta) - (z * sintheta)
                    dot(o, d).z = (z * costheta) + (y * sintheta)
                END IF
                'y-axis rotation
                IF roty > 0 THEN
                    sintheta = SIN(roty)
                    costheta = COS(roty)
                    x = dot(o, d).x
                    z = dot(o, d).z
                    dot(o, d).x = (x * costheta) - (z * sintheta)
                    dot(o, d).z = (z * costheta) + (x * sintheta)
                END IF
                'z-axis rotation
                IF rotz > 0 THEN
                    sintheta = SIN(roty)
                    costheta = COS(roty)
                    x = dot(o, d).x
                    y = dot(o, d).y
                    dot(o, d).x = (x * costheta) - (y * sintheta)
                    dot(o, d).y = (y * costheta) + (x * sintheta)
                END IF
            LOOP UNTIL d = 8
    END SELECT
END SUB

SUB newlight (posx, posy, posz, brightness)
    o = o + 1
    object(o).type = "light"
END SUB

FUNCTION colour& (name$)
    SELECT CASE name$
        CASE IS = "white"
            colour& = _RGBA(255, 255, 255, 255)
        CASE IS = "green"
            colour& = _RGBA(0, 238, 55, 255)
        CASE IS = "reflective"
            colour& = _RGBA(worldbrightness, worldbrightness, worldbrightness, 255)
        CASE IS = "grid"
            gridtransparency = 200
            colour& = _RGBA(gridtransparency, gridtransparency, gridtransparency, gridtransparency)
    END SELECT
END FUNCTION

cubeconnections:
DATA 2,3,5,1,4,6,1,4,7,2,3,8
DATA 1,6,7,5,2,8,5,8,3,4,6,7
