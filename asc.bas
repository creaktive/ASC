DEFINT A-Z

DECLARE SUB FontDraw (A$)
DECLARE SUB Back ()
DECLARE SUB Help ()
DECLARE SUB Sample ()
DECLARE SUB Colors ()
DECLARE SUB Clock ()
DECLARE SUB SendS (Code%)
DECLARE SUB Info (Code%)

DIM SHARED TX, TY, FC, BC, SampleStr$, MS, OldSec

REM $INCLUDE: 'hackqb.bi'

OldCY = CSRLIN
OldCX = POS(OldCY)

 FOR X! = 100 TO 0 STEP -1
  FadeSet X!
  Pause .002
 NEXT

ScreenGet 1, 1, MaxX, MaxY, Buff$

OldMode = ModeGet
 IF OldMode <> 3 THEN ModeSet 3
  WIDTH 80, 25

Blink 0
Back

TX = 14
TY = 3
FC = 15
BC = 7

EditStopCode$(0) = "LB"

LOCATE 1, 1
ChrString 32, 80, 14, 9

Frame TX, TY, TX + 34, TY + 17, FrameFont$(0), 7, 15, 8         'Main frame
Frame TX + 34, TY, TX + 51, TY + 17, FrameFont$(0), 7, 15, 8    'Chr display
Frame TX, TY + 17, TX + 54, TY + 20, FrameFont$(0), 7, 15, 8    'Info
Frame TX + 51, TY, TX + 54, TY + 17, FrameFont$(0), 7, 15, 8    'HEX code

COLOR 15, 7
LOCATE TY, TX + 34: PRINT "Â"
LOCATE TY, TX + 51: PRINT "Â"
LOCATE TY + 17, TX + 34: PRINT "Á"
LOCATE TY + 17, TX + 51: PRINT "Á"
LOCATE TY + 17, TX: PRINT "Ã"

COLOR 8, 7
LOCATE TY + 17, TX + 54: PRINT "´"
LOCATE TY + 17, TX + 51: PRINT "Á"
ChrSet 54, 1, 179, 8, 9

COLOR 31, 1
LOCATE 1, 3
PRINT "ASCII code viewer program. Press F1 for help."
COLOR 15, 7

LOCATE TY + 19, TX + 3
PRINT "Enter "; : COLOR 1: PRINT "s"; : COLOR 15, 7: PRINT "ample string:";

LOCATE TY + 18, TX + 36
COLOR 31, 8
PRINT "Change ";
COLOR 17
PRINT "c";
COLOR 31
PRINT "olours..."

COLOR 15, 7

'Draw Shadow:
LOCATE TY + 21, TX + 2
ChrString 176, 55, 7, 0

FOR X = 0 TO 20
 LOCATE TY + X + 1, TX + 55
 ChrString 176, 2, 7, 0
NEXT

FOR X = 0 TO 255
 A = X MOD 16
 IF A = 0 THEN B = B + 1
 ChrSet ((A * 2) + TX + 2), B + TY, X, 0, 7
NEXT X

 FOR X! = 0 TO 100 STEP 1
  FadeSet X!
  Pause .001
 NEXT

A = 0
B = 0
Xps = TX + 2
Yps = TY + 1
OldXps = Xps
OldYps = Yps
OldCode = 1

MS = MouseInit
IF MS THEN
 MouseRange 0, 0, 639, 196
 MousePut 1, 1
 MouseShow
END IF

ShiftSet 0

DO:

  Key$ = UCASE$(INKEY$)
 
  SELECT CASE Key$
   CASE CHR$(0) + "H"   'Up
    A = A - 1
   CASE CHR$(0) + "P"   'Down
    A = A + 1
   CASE CHR$(0) + "K"   'Left
    B = B - 1
   CASE CHR$(0) + "M"   'Right
    B = B + 1
   CASE CHR$(0) + ";"   'F1
    Help
   CASE CHR$(27)        'Esc
    GOTO Away
   CASE "S"             'Sample Text
    Sample
   CASE "C"             'Sample Text
    Colors
    OldA$ = ""
    Info Code
   CASE CHR$(13), " "   'Enter, Space
    SendS Code
  END SELECT

IF MS THEN MouseStatus MX, MY, Buttons

MX = MX \ 8 + 1
MY = MY \ 8 + 1

LB = BitGet(Buttons, 0)

RB = BitGet(Buttons, 1)
 IF RB AND OldRB THEN
  RB = 0
 ELSE
  OldRB = RB
 END IF

 IF (MX > TX) AND (MX < TX + 34) AND (MY > TY) AND (MY < TY + 17) THEN
  IF LB OR RB THEN
   A = MY - TY - 1
   B = MX \ 2 - TX + 6
   IF MX = TX + 1 THEN B = 0
  END IF

 ELSEIF (MX > TX) AND (MX < TX + 54) AND (MY = TY + 19) AND LB THEN
  Sample
 ELSEIF (MY = 1) AND LB THEN
  Help
 ELSEIF (MY = TY + 18) AND (MX < TX + 53) AND (MX > TX + 35) AND LB THEN
  Colors
  OldA$ = ""
  Info Code
 END IF

IF A < 0 THEN A = 15
IF B < 0 THEN B = 15
IF A > 15 THEN A = 0
IF B > 15 THEN B = 0

Code = (A * 16) + B

Xps = (B * 2) + TX + 2
Yps = (A + TY + 1)
 
  IF RB THEN
   SendS Code
  END IF

 IF OldCode <> Code THEN

MouseHide

COLOR , 7
LOCATE OldYps, OldXps - 1
PRINT "   "
ChrSet OldXps, OldYps, OldCode, 0, 7

COLOR , 3
LOCATE Yps, Xps - 1
PRINT "   "
ChrSet Xps, Yps, Code, 15, 3

MouseShow

Info Code

OldXps = Xps
OldYps = Yps
OldCode = Code

 END IF

A$ = ChrFontGet$(Code, 16)
 IF A$ <> OldA$ THEN FontDraw A$
OldA$ = A$

Clock

LOOP

Away:

 FOR X! = 100 TO 0 STEP -1
  FadeSet X!
  Pause .002
 NEXT

IF MS THEN MouseHide
 IF OldMode <> 3 THEN ModeSet OldMode
ScreenSet 1, 1, 0, Buff$
Blink 1
LOCATE OldCY, OldCX

 FOR X! = 0 TO 100 STEP 1
  FadeSet X!
  Pause .001
 NEXT

COLOR 7, 0

REM $DYNAMIC
SUB Back

CLS

FOR Y = 1 TO 25
 LOCATE Y, 1
 ChrString 176, 80, 1, 0
NEXT

END SUB

SUB Clock

TimeGet Hour, Min, Sec, CS
DateGet Day, Month, Year, WD

IF Sec <> OldSec THEN

Hour$ = Int2StrD$(Hour, "00")
Min$ = Int2StrD$(Min, "00")
Sec$ = Int2StrD$(Sec, "00")
Day$ = Int2StrD$(Day, "00")
Month$ = Int2StrD$(Month, "00")
Year$ = Int2StrD$(Year, "0000")

SELECT CASE WD
 CASE 0
WD$ = "Sun"
 CASE 1
WD$ = "Mon"
 CASE 2
WD$ = "Tue"
 CASE 3
WD$ = "Wed"
 CASE 4
WD$ = "Thu"
 CASE 5
WD$ = "Fri"
 CASE 6
WD$ = "Sat"
END SELECT

COLOR 30, 1
LOCATE 1, 56

IF MS THEN MouseStatus MX, MY, Buttons
 MX = MX \ 8 + 1
 MY = MY \ 8 + 1
  IF (MX > 55) AND (MY = 1) THEN F = 1

IF F THEN MouseHide
 PRINT USING "\ \, \\.\\.\  \, \\:\\:\\"; WD$; Day$; Month$; Year$; Hour$; Min$; Sec$
IF F THEN MouseShow

COLOR 15, 7

OldSec = Sec

END IF

END SUB

SUB Colors

Ap = TX + 7
Bp = TY + 7

MouseHide

ScreenGet Ap, Bp, Ap + 42, Bp + 6, Buff$
Frame Ap, Bp, Ap + 40, Bp + 5, FrameFont(0), 8, 15, 0

'Draw Shadow:
FOR X = 0 TO 40
 ChrGet Ap + X + 2, Bp + 6, OldCode, Attr, 255
  Attr = Attr AND &H7
 ChrSet Ap + X + 2, Bp + 6, OldCode, Attr, 255
NEXT

FOR X = 0 TO 4
FOR Y = 0 TO 1
 ChrGet Ap + 41 + Y, Bp + X + 1, OldCode, Attr, 255
  Attr = Attr AND &H7
 ChrSet Ap + 41 + Y, Bp + X + 1, OldCode, Attr, 255
NEXT
NEXT

LOCATE Bp, Ap + 8
COLOR 31, 0
PRINT "´ Change Preview Colors Ã"

COLOR 31
LOCATE Bp + 2, Ap + 6
PRINT "Foreground"

LOCATE Bp + 2, Ap + 25
COLOR 16
PRINT "Background"

LOCATE Bp + 4, Ap + 3
ChrString 176, 16, 15, 0

LOCATE Bp + 4, Ap + 22
ChrString 176, 16, 15, 0

ChrSet Ap + 2, Bp + 4, 27, 7, 0
ChrSet Ap + 21, Bp + 4, 27, 7, 0

ChrSet Ap + 19, Bp + 4, 26, 7, 0
ChrSet Ap + 38, Bp + 4, 26, 7, 0

FOR X = 0 TO 15
 ChrSet Ap + 3 + X, Bp + 3, 219, X, 0
NEXT

FOR X = 0 TO 15
 ChrSet Ap + 22 + X, Bp + 3, 0, 0, X
NEXT

ChrSet Ap + 3 + FC, Bp + 4, 24, 15, 0
ChrSet Ap + 22 + BC, Bp + 4, 24, 15, 0

MF = -1

OldFC = FC
OldBC = BC

MouseShow

DO:

SELECT CASE UCASE$(INKEY$)
 CASE CHR$(0) + "K"             'Left
 
  IF MF THEN
   OldFC = FC
   FC = FC - 1
    IF FC < 0 THEN FC = 0
  ELSE
   OldBC = BC
   BC = BC - 1
    IF BC < 0 THEN BC = 0
  END IF

 CASE CHR$(0) + "M"             'Right

  IF MF THEN
   OldFC = FC
   FC = FC + 1
    IF FC > 15 THEN FC = 15
  ELSE
   OldBC = BC
   BC = BC + 1
    IF BC > 15 THEN BC = 15
  END IF

 CASE CHR$(9)                   'Tab
  OldMF = MF
  MF = NOT MF
 CASE CHR$(27), CHR$(13)        'Esc or Enter
  EXIT DO
END SELECT

IF MS THEN MouseStatus MX, MY, Buttons

MX = MX \ 8 + 1
MY = MY \ 8 + 1

IF BitGet(Buttons, 0) THEN

 IF (MX > Ap + 2) AND (MX < Ap + 19) AND (MY > Bp + 1) AND (MY < Bp + 5) THEN
  MF = -1
  IF MY <> Bp + 2 THEN
   OldFC = FC
   FC = MX - Ap - 3
  END IF
 ELSEIF (MX > Ap + 21) AND (MX < Ap + 38) AND (MY > Bp + 1) AND (MY < Bp + 5) THEN
  MF = 0
  IF MY <> Bp + 2 THEN
   OldBC = BC
   BC = MX - Ap - 22
  END IF
 END IF

END IF

IF OldFC <> FC THEN
 MouseHide
 ChrSet Ap + 3 + FC, Bp + 4, 24, 15, 0
 ChrSet Ap + 3 + OldFC, Bp + 4, 176, 15, 0
 OldFC = FC
 MouseShow
END IF

IF OldBC <> BC THEN
 MouseHide
 ChrSet Ap + 22 + BC, Bp + 4, 24, 15, 0
 ChrSet Ap + 22 + OldBC, Bp + 4, 176, 15, 0
 OldBC = BC
 MouseShow
END IF

  IF MF <> OldMF THEN
   IF MF THEN
  
    MouseHide
    COLOR 31
    LOCATE Bp + 2, Ap + 6
    PRINT "Foreground"

    LOCATE Bp + 2, Ap + 25
    COLOR 16
    PRINT "Background"
    MouseShow
  
   ELSE
 
    MouseHide
    COLOR 16
    LOCATE Bp + 2, Ap + 6
    PRINT "Foreground"

    LOCATE Bp + 2, Ap + 25
    COLOR 31
    PRINT "Background"
    MouseShow

   END IF
  
   OldMF = MF
 
  END IF

LOOP

MouseHide
 ScreenSet Ap, Bp, 0, Buff$
MouseShow

END SUB

SUB FontDraw (A$)

Bp = TY
Ap = TX + 34

FOR X = 0 TO 15
  Byte = ASC(MID$(A$, X + 1, 1))

 FOR Y = 0 TO 7
 
  IF MS THEN
   MouseStatus MX, MY, Buttons
   MX = MX \ 8 + 1
   MY = MY \ 8 + 1
    IF (MX > TX + 34) AND (MX < TX + 54) AND (MY > TY) AND (MY < TY + 17) THEN
     F = 1
    END IF
  END IF

  IF F THEN MouseHide
 
  A = BitGet(Byte, Y)
  LOCATE Bp + X + 1, Ap + (15 - (Y * 2))
   IF A THEN
    ChrString 219, 2, FC, BC
   ELSE
    ChrString 32, 2, FC, BC
   END IF
 
  IF F THEN MouseShow

 NEXT
 
  LOCATE Bp + X + 1, Ap + 18
  COLOR 15, 7
   HEXByte$ = HEX$(Byte)
    IF LEN(HEXByte$) < 2 THEN HEXByte$ = "0" + HEXByte$
  PRINT HEXByte$

NEXT

COLOR 7, 0

END SUB

SUB Help

Ap = TX + 2
Bp = TY + 3

MouseHide

ScreenGet Ap, Bp, Ap + 52, Bp + 14, Buff$
Frame Ap, Bp, Ap + 50, Bp + 13, FrameFont(0), 8, 15, 0

'Draw Shadow:
FOR X = 0 TO 52
 ChrGet Ap + X + 2, Bp + 14, OldCode, Attr, 255
  Attr = Attr AND &H7
 ChrSet Ap + X + 2, Bp + 14, OldCode, Attr, 255
NEXT

FOR X = 0 TO 12
FOR Y = 0 TO 1
 ChrGet Ap + 51 + Y, Bp + X + 1, OldCode, Attr, 255
  Attr = Attr AND &H7
 ChrSet Ap + 51 + Y, Bp + X + 1, OldCode, Attr, 255
NEXT
NEXT

LOCATE Bp, Ap + 21
COLOR 31, 0
PRINT "´ Help Ã"

LOCATE Bp + 1, Ap + 18
COLOR 30
PRINT "ASC View v2.05"
LOCATE Bp + 2, Ap + 2
PRINT "(C)opyright by SysD Destructive Labs, 1997-1998"
LOCATE Bp + 3, Ap + 5
PRINT "Created by St. Y. Pusep, using QBasic 4.5"

LOCATE Bp + 5, Ap + 4
COLOR 31
PRINT "This program show to you all ASCII codes of"
LOCATE Bp + 6, Ap + 4
PRINT "system. Use arrows or mouse to view codes,"
LOCATE Bp + 7, Ap + 4
PRINT "and buttons "; : COLOR 17: PRINT "C"; : COLOR 31: PRINT " and "; : COLOR 17
PRINT "S"; : COLOR 31: PRINT " to change preview";
LOCATE Bp + 8, Ap + 4
PRINT "colors and enter sample text, respective."
LOCATE Bp + 10, Ap + 4
PRINT "Thanks for using this program! Send all your"
LOCATE Bp + 11, Ap + 4
PRINT "suggestions about this and other my programs"
LOCATE Bp + 12, Ap + 4
PRINT "by following adress: ";
COLOR 16
PRINT "stas@nettaxi.com"

MouseShow

DO:

IF MS THEN MouseStatus MX, MY, Buttons
 MX = MX \ 8 + 1
 MY = MY \ 8 + 1
  IF (MY > 1) AND (Buttons) THEN EXIT DO

LOOP UNTIL INKEY$ <> ""

MouseHide
 ScreenSet Ap, Bp, 0, Buff$
MouseShow

END SUB

SUB Info (Code%)

 HCode$ = Int2StrH$(Code, "00")
 DCode$ = Int2StrD$(Code, "000")

  FOR Z = 7 TO 0 STEP -1
   IF BitGet(Code, Z) THEN
    BCode$ = BCode$ + "1"
   ELSE
    BCode$ = BCode$ + "0"
   END IF
  NEXT

MouseHide

COLOR 15, 7
LOCATE TY + 18, TX + 3

PRINT "Code:   ";
PRINT USING "³ \ \d ³"; DCode$;
PRINT USING " \\h ³"; HCode$;
PRINT USING "\      \b³"; BCode$
ChrSet TX + 9, TY + 18, Code, FC, BC

BCode$ = ""

LOCATE TY + 19, TX + 23
ChrString 32, 30, FC, BC

FOR Z = 1 TO LEN(SampleStr$)
 ChrSet TX + 22 + Z, TY + 19, ASC(MID$(SampleStr$, Z, 1)), FC, BC
NEXT

MouseShow

END SUB

SUB Sample

DO:

MouseStatus MX, MY, Buttons
MY = MY \ 8 + 1

LOCATE TY + 19, TX + 23
Edit SampleStr$, 30, 0, "", FC, BC

ESC$ = EditEndCode$

IF MY = TY + 19 AND ESC$ = "LB" THEN ESC$ = ""

LOOP UNTIL ESC$ <> ""

END SUB

SUB SendS (Code%)

IF LEN(SampleStr$) < 30 THEN
 SampleStr$ = SampleStr$ + CHR$(Code)
 Info Code
END IF

END SUB

