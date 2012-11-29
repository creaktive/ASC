DEFINT A-Z

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                  QBasic 4.5 support for "Hacker's Tools"                  *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'
'- From QB.BI:

TYPE RegType
     AX    AS INTEGER
     BX    AS INTEGER
     CX    AS INTEGER
     DX    AS INTEGER
     BP    AS INTEGER
     SI    AS INTEGER
     DI    AS INTEGER
     Flags AS INTEGER
END TYPE

TYPE RegTypeX
     AX    AS INTEGER
     BX    AS INTEGER
     CX    AS INTEGER
     DX    AS INTEGER
     BP    AS INTEGER
     SI    AS INTEGER
     DI    AS INTEGER
     Flags AS INTEGER
     DS    AS INTEGER
     ES    AS INTEGER
END TYPE

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                 Machine code support:                   *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      DECLARE SUB INTERRUPT (IntNum AS INTEGER, InReg, OutReg)
      DECLARE SUB INTERRUPTX (IntNum AS INTEGER, InReg AS RegTypeX, OutReg AS RegTypeX)
      DECLARE SUB INT86OLD (IntNum AS INTEGER, InArray() AS INTEGER, OutArray() AS INTEGER)
      DECLARE SUB INT86XOLD (IntNum AS INTEGER, InArray() AS INTEGER, OutArray() AS INTEGER)
      DECLARE SUB BIOSInt (IntN%)
      DECLARE SUB IntVecSet (IntN%, Segm%, Offs%)
      DECLARE SUB IntVecGet (IntN%, Segm%, Offs%)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                  Data manipulators:                     *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 DECLARE FUNCTION LByte (Var%)
 DECLARE FUNCTION HByte (Var%)
      DECLARE SUB BitSet (Var%, BitN%, Value%)
 DECLARE FUNCTION BitGet (Var%, BitN%)
 DECLARE FUNCTION Int2StrD$ (Value%, Set$)
 DECLARE FUNCTION Int2StrH$ (Value%, Set$)
 DECLARE FUNCTION Int2Lng& (Value%)
 DECLARE FUNCTION Lng2Int (Value&)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                    System identify:                     *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 DECLARE FUNCTION BIOSVer$ ()
 DECLARE FUNCTION DOSVer$ ()
 DECLARE FUNCTION WinVer$ ()

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*               Video hardware control:                   *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 DECLARE FUNCTION ModeGet ()
      DECLARE SUB ModeSet (Mode%)
      DECLARE SUB PalGet (nColor%, Array%())
      DECLARE SUB PalSet (nColor%, Array%())
 DECLARE FUNCTION MaxX ()
 DECLARE FUNCTION MaxY ()
      DECLARE SUB Border (Clr%)
 DECLARE FUNCTION PageGet ()
      DECLARE SUB PageSet (Value%)
      DECLARE SUB TextSizeSet (X%, Y%)
      DECLARE SUB DisplaySet (X%, Y%)
      DECLARE SUB GrabPal ()
      DECLARE SUB FadeSet (Prcnt!)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                    Screen output:                       *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      DECLARE SUB Center (Lin%, Text$)
      DECLARE SUB ChrSet (XC%, YC%, Code%, FC%, BC%)
      DECLARE SUB ChrGet (XC%, YC%, Code%, FC%, BC%)
      DECLARE SUB ChrString (Code%, N%, FC%, BC%)
      DECLARE SUB Frame (HIP%, VIP%, HEP%, VEP%, Font$, CF%, CL%, CR%)
      DECLARE SUB ScreenGet (IX%, IY%, EX%, EY%, Buffer$)
      DECLARE SUB ScreenSet (IX%, IY%, Mode%, Buffer$)
      DECLARE SUB PrintScreen ()
      DECLARE SUB VidWait ()
      DECLARE SUB Blink (Mode%)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                ASCII character control:                 *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     
      DECLARE SUB ChrFontSet (AscN%, ChrH%, Font$)
 DECLARE FUNCTION ChrFontGet$ (AscN%, ChrH%)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                    Mouse routines:                      *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

'    !!!     NOTE:     !!!
'
'  Possible button values:
'
' ÉÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
' º Bit     ³ Bit    ³                          º
' º number: ³ value: ³ Description:             º
' ÇÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
' º       0 ³      1 ³  Left button             º
' º       1 ³      2 ³  Right button            º
' º       2 ³      4 ³  Middle button           º
' ÈÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼

 DECLARE FUNCTION MouseInit ()
      DECLARE SUB MouseStatus (XMouse%, YMouse%, Buttons%)
      DECLARE SUB MouseRange (X1%, Y1%, X2%, Y2%)
      DECLARE SUB MousePut (X%, Y%)
      DECLARE SUB MouseHide ()
      DECLARE SUB MouseDriver (AX%, BX%, CX%, DX%)
      DECLARE SUB MouseShow ()
      DECLARE SUB MouseCursor (Code%, FC%, BC%)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                 Mathematic routines:                    *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 DECLARE FUNCTION RAD# (DEGval#)
 DECLARE FUNCTION DEG# (RADval#)
      DECLARE SUB MatrixSort (SortVector$(), Low%, High%, Seq%)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                     Disk routines:                      *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  DECLARE FUNCTION HDriveList$ ()
  DECLARE FUNCTION FloppyList$ ()
       DECLARE SUB DiskDriver (Disk$, AX%, BX%, CX%, DX%)
  DECLARE FUNCTION DiskTSpace& (Disk$)
  DECLARE FUNCTION DiskFSpace& (Disk$)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                     File routines:                      *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

'    !!!     NOTE:     !!!
'
'  Possible file attributes:
'
' ÉÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
' º Bit     ³ Bit    ³                          º
' º number: ³ value: ³ Description:             º
' ÇÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
' º       0 ³      1 ³  Read only               º
' º       1 ³      2 ³  Hidden                  º
' º       2 ³      4 ³  System                  º
' º       3 ³      8 ³  Volume label            º
' º       4 ³     16 ³  Subdirectory name       º
' º       5 ³     32 ³  Archive                 º
' º       6 ³     64 ³  Not used                º
' º       7 ³    128 ³  Not used                º
' ÈÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼

  DECLARE FUNCTION EXEName$ ()
  DECLARE FUNCTION FindFirst$ (FileSpec$, Attr%)
  DECLARE FUNCTION FindNext$ ()
  DECLARE FUNCTION CPath$ (Disk$)
  DECLARE FUNCTION CDisk$ ()
  DECLARE FUNCTION FileName$ (Buffer$)
  DECLARE FUNCTION FileSize& (Buffer$)
       DECLARE SUB FileDate (Buffer$, Day%, Month%, Year%)
       DECLARE SUB FileTime (Buffer$, Hour%, Min%, Sec%)
  DECLARE FUNCTION FileAttrib (Buffer$)
       DECLARE SUB FileAttrSet (FileSpec$, Attr%)
  DECLARE FUNCTION InfoFPath$ (FileSpec$)
  DECLARE FUNCTION InfoFName$ (FileSpec$)
  DECLARE FUNCTION InfoFExtn$ (FileSpec$)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                  Keyboard routines:                     *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

'    !!!     NOTE:     !!!
'
'  Possible key values for ShiftGet/ShiftSet:
'
' ÉÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
' º Bit     ³ Bit    ³                          º
' º number: ³ value: ³ Description:             º
' ÇÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
' º       0 ³      1 ³  Right shift             º
' º       1 ³      2 ³  Left shift              º
' º       2 ³      4 ³  Control                 º
' º       3 ³      8 ³  Alt                     º
' º       4 ³     16 ³  Scroll Lock             º
' º       5 ³     32 ³  Num Lock                º
' º       6 ³     64 ³  Caps Lock               º
' º       7 ³    128 ³  Insert state            º
' ÈÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼

      DECLARE SUB ReBoot ()
      DECLARE SUB Edit (Text$, Length%, InitPos%, Param$, FC%, BC%)
      DECLARE SUB ClearKeyb ()
      DECLARE SUB ShiftSet (Value%)
 DECLARE FUNCTION ShiftGet ()

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                     Time routines:                      *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 DECLARE FUNCTION WeekDay (Day%, Month%, Year%)
      DECLARE SUB DateGet (Day%, Month%, Year%, WeekD%)
      DECLARE SUB DateSet (Day%, Month%, Year%)
      DECLARE SUB FileDateSet (FileSpec$, Day%, Month%, Year%, Hour%, Min%, Sec%)
      DECLARE SUB TimeGet (Hour%, Min%, Sec%, CSec%)
      DECLARE SUB TimeSet (Hour%, Min%, Sec%, CSec%)
      DECLARE SUB Pause (Time!)

'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                    Sound routines:                      *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     
      DECLARE SUB SoundFreq (Freq!)
      DECLARE SUB SoundOn ()
      DECLARE SUB SoundOff ()

DIM SHARED EditStopCode(-2 TO 35) AS STRING
DIM SHARED DefaultPal(0 TO 255, 0 TO 2) AS INTEGER
DIM SHARED FrameFont(0 TO 4) AS STRING

COMMON SHARED EditEndCode$, EditBgr$, EditStopCode() AS STRING  'For EDIT
COMMON SHARED PI AS DOUBLE
COMMON SHARED Page AS INTEGER
COMMON SHARED Reg AS RegTypeX
COMMON SHARED BlinkState AS INTEGER

REM $DYNAMIC

PI# = 3.141592653589793#

EditStopCode$(-2) = CHR$(27)
EditStopCode$(-1) = CHR$(13)
EditBgr$ = " "

' If you want to make EDIT routine stop, when
' mouse buttons are pressed, put "LB" (left button),
' "RB" (right button), or "MB" (middle button) in some
' place of EditEndCode$ array. Look this sample code:
'
'EditStopCode$(0) = "LB"
'EditStopCode$(1) = "RB"
'EditStopCode$(1) = "MB"
'
' It makes EDIT stop every time when some mouse button
' is pressed.

FrameFont$(0) = ""
FrameFont$(0) = FrameFont$(0) + "ÚÄ¿"
FrameFont$(0) = FrameFont$(0) + "³ ³"
FrameFont$(0) = FrameFont$(0) + "ÀÄÙ"

FrameFont$(1) = ""
FrameFont$(1) = FrameFont$(1) + "ÉÍ»"
FrameFont$(1) = FrameFont$(1) + "º º"
FrameFont$(1) = FrameFont$(1) + "ÈÍ¼"

FrameFont$(2) = ""
FrameFont$(2) = FrameFont$(2) + "ÖÄ·"
FrameFont$(2) = FrameFont$(2) + "º º"
FrameFont$(2) = FrameFont$(2) + "ÓÄ½"

FrameFont$(3) = ""
FrameFont$(3) = FrameFont$(3) + "ÕÍ¸"
FrameFont$(3) = FrameFont$(3) + "³ ³"
FrameFont$(3) = FrameFont$(3) + "ÔÍ¾"

FrameFont$(4) = ""
FrameFont$(4) = FrameFont$(4) + "ÛßÛ"
FrameFont$(4) = FrameFont$(4) + "Û Û"
FrameFont$(4) = FrameFont$(4) + "ÛÜÛ"

Page = 0
BlinkState = 1

Blink BlinkState

GrabPal

REM $STATIC
SUB BIOSInt (IntN%)

CALL INTERRUPTX(IntN%, Reg, Reg)

END SUB

FUNCTION BIOSVer$

 DEF SEG = &HF000
FOR X = 0 TO 7
Ver$ = Ver$ + CHR$(PEEK(&HFFF5 + X))
NEXT X
 DEF SEG
BIOSVer$ = Ver$

END FUNCTION

FUNCTION BitGet (Var%, BitN%)

Mask& = 2 ^ BitN
BitGet = (Var AND Mask&) / Mask&

END FUNCTION

SUB BitSet (Var%, BitN%, Value%)

ByteN = (BitN \ 8)
Desl = VARPTR(Var) + ByteN

Mask = 2 ^ (BitN - (8 * ByteN))

 IF Value = 1 THEN
POKE Desl, PEEK(Desl) OR Mask
 ELSEIF Value = 0 THEN
POKE Desl, PEEK(Desl) AND (Mask XOR &HFF)
 ELSE STOP
 END IF

END SUB

SUB Blink (Mode%)

Reg.AX = &H1003
Reg.BX = Mode
BIOSInt &H10
BlinkState = Mode

END SUB

SUB Border (Clr%)

Reg.AX = &H1001
Reg.BX = Clr * 256

BIOSInt &H10

END SUB

FUNCTION CDisk$

Reg.AX = &H1900
 BIOSInt &H21
CDisk$ = CHR$((Reg.AX AND &HFF) + 65) + ":"

END FUNCTION

SUB Center (Lin%, Text$)

ToJump = (MaxX / 2) - LEN(Text$) \ 2
LOCATE Lin, ToJump: PRINT Text$

END SUB

FUNCTION ChrFontGet$ (AscN%, ChrH%)

ChrOfs = 32 * AscN

ASM0$ = ""
ASM0$ = ASM0$ + CHR$(&HFA) + CHR$(&HBA) + CHR$(&HC4) + CHR$(&H3)
ASM0$ = ASM0$ + CHR$(&HB8) + CHR$(&H2) + CHR$(&H4) + CHR$(&HEF)
ASM0$ = ASM0$ + CHR$(&HB8) + CHR$(&H4) + CHR$(&H7) + CHR$(&HEF)
ASM0$ = ASM0$ + CHR$(&HBA) + CHR$(&HCE) + CHR$(&H3) + CHR$(&HB8)
ASM0$ = ASM0$ + CHR$(&H4) + CHR$(&H2) + CHR$(&HEF) + CHR$(&HB8)
ASM0$ = ASM0$ + CHR$(&H5) + CHR$(&H0) + CHR$(&HEF) + CHR$(&HB8)
ASM0$ = ASM0$ + CHR$(&H6) + CHR$(&H4) + CHR$(&HEF) + CHR$(&HCB)
ASM0$ = ASM0$ + CHR$(&H0) + CHR$(&H0) + CHR$(&H0) + CHR$(&H0)

ASM1$ = ""
ASM1$ = ASM1$ + CHR$(&HBA) + CHR$(&HC4) + CHR$(&H3) + CHR$(&HB8)
ASM1$ = ASM1$ + CHR$(&H2) + CHR$(&H3) + CHR$(&HEF) + CHR$(&HB8)
ASM1$ = ASM1$ + CHR$(&H4) + CHR$(&H3) + CHR$(&HEF) + CHR$(&HBA)
ASM1$ = ASM1$ + CHR$(&HCE) + CHR$(&H3) + CHR$(&HB8) + CHR$(&H4)
ASM1$ = ASM1$ + CHR$(&H0) + CHR$(&HEF) + CHR$(&HB8) + CHR$(&H5)
ASM1$ = ASM1$ + CHR$(&H10) + CHR$(&HEF) + CHR$(&HB8) + CHR$(&H6)
ASM1$ = ASM1$ + CHR$(&HE) + CHR$(&HEF) + CHR$(&HFB) + CHR$(&HCB)
ASM1$ = ASM1$ + CHR$(&H0) + CHR$(&H0) + CHR$(&H0) + CHR$(&H0)

DEF SEG = VARSEG(ASM0$)
 CALL ABSOLUTE(SADD(ASM0$))
DEF SEG

 DEF SEG = &HA000
  FOR X = 0 TO ChrH - 1
   Font$ = Font$ + CHR$(PEEK(ChrOfs + X))
  NEXT
 DEF SEG

DEF SEG = VARSEG(ASM1$)
 CALL ABSOLUTE(SADD(ASM1$))
DEF SEG

ChrFontGet$ = Font$

END FUNCTION

SUB ChrFontSet (AscN%, ChrH%, Font$)

 Reg.AX = (17 * 256)
 Reg.BX = (ChrH * 256)
 Reg.CX = 1
 Reg.DX = AscN
 Reg.ES = VARSEG(Font$)
 Reg.BP = SADD(Font$)
  BIOSInt &H10

END SUB

SUB ChrGet (XC%, YC%, Code%, FC%, BC%)
 
  IF ModeGet > 3 THEN

A = CSRLIN
B = POS(A)
LOCATE YC, XC, 0

Reg.AX = &H800
Reg.BX = Page * 256

 BIOSInt &H10
LOCATE A, B
 Code = Reg.AX
 FC = Reg.BX
 BC = 0

 ELSE
 Location = (MaxX * 2) * (YC - 1) + 2 * (XC - 1)

IF ModeGet < 2 THEN
 Steps = 128
ELSE
 Steps = 256
END IF

VidPage = &HB800 + (Page * Steps)

DEF SEG = VidPage
 Code = PEEK(Location)
 GetAttr = PEEK(Location + 1)
DEF SEG

 IF BitGet(GetAttr, 7) THEN
  FC = FC + 16
 END IF

IF BC < 255 THEN

  FMask = &HF   '1111
  BMask = &H7   '0111
   IF BlinkState = 0 THEN
    BitSet BMask, 3, 1
   END IF

  BC = (GetAttr AND (BMask * 16)) \ 16
  FC = (GetAttr AND FMask)
 
  IF BlinkState = 0 THEN BitSet FC, 4, 0

ELSE

 FC = GetAttr

END IF

  END IF

END SUB

SUB ChrSet (XC%, YC%, Code%, FC%, BC%)

 IF ModeGet > 3 THEN

A = CSRLIN
B = POS(A)

LOCATE YC, XC, 0

Reg.AX = &H900 + Code
Reg.BX = (Page * 256) + FC
Reg.CX = 1

 BIOSInt &H10
LOCATE A, B
 ELSE
 Location = (MaxX * 2) * (YC - 1) + 2 * (XC - 1)

IF ModeGet < 2 THEN
 Steps = 128
ELSE
 Steps = 256
END IF

VidPage = &HB800 + (Page * Steps)

 IF BC <> 255 THEN
 
  FMask = &HF   '1111
  BMask = &H7   '0111
   IF BlinkState THEN
    BitSet BMask, 3, 1
   END IF

  SetAttr = ((BC AND BMask) * 16) + (FC AND FMask)
 
  IF (BC > 7) AND (BlinkState = 0) THEN
   BitSet SetAttr, 7, 1
  ELSE
   BitSet SetAttr, 7, 0
  END IF

 ELSE

   SetAttr = FC

 END IF

DEF SEG = VidPage
 POKE Location, Code
 POKE Location + 1, SetAttr
DEF SEG

END IF

END SUB

SUB ChrString (Code%, N%, FC%, BC%)
 
    IF ModeGet% > 3 AND ModeGet% <> 7 THEN
   SetAttr% = FC%
    ELSE
  IF BitGet%(FC%, 4) THEN
  BitSet FC%, 4, 0
  BitSet SetAttr%, 7, 1
  END IF
   SetAttr% = SetAttr% + FC%
   SetAttr% = SetAttr% + (BC% * 16)
    END IF

Reg.AX = &H900 + Code%
Reg.BX = (Page% * 256) + SetAttr%
Reg.CX = N%

 BIOSInt &H10

END SUB

SUB ClearKeyb

DO:
 Reg.AX = &H100
 BIOSInt &H16
  IF BitGet(Reg.Flags, 6) THEN EXIT DO
  Reg.AX = 0
  BIOSInt &H16
LOOP

END SUB

FUNCTION CPath$ (Disk$)

Disk$ = UCASE$(Disk$)

Reg.AX = &H4700
Reg.DX = ASC(Disk$) - 64
BIOSInt &H21
DEF SEG = Reg.DS

DO:
NewChar$ = CHR$(PEEK(Reg.SI + I))
IF NewChar$ = CHR$(0) THEN EXIT DO
PathV$ = PathV$ + NewChar$
I = I + 1
LOOP

IF Disk$ = "@:" THEN Disk$ = CDisk$
CPath$ = Disk$ + "\" + PathV$

END FUNCTION

SUB DateGet (Day%, Month%, Year%, WeekD%)

Reg.AX = &H2A00
 BIOSInt &H21
  WeekD = LByte(Reg.AX)
  Year = Reg.CX
  Month = HByte(Reg.DX)
  Day = LByte(Reg.DX)

END SUB

SUB DateSet (Day%, Month%, Year%)

Reg.AX = &H2B00
Reg.CX = Year
Reg.DX = (256 * Month) + Day
 BIOSInt &H21

END SUB

DEFDBL A-Z
FUNCTION DEG# (RADval#)

 DEG = (180 * RADval) / PI

END FUNCTION

DEFINT A-Z
SUB DiskDriver (Disk$, AX%, BX%, CX%, DX%)

Reg.AX = &H3600
Reg.DX = ASC(UCASE$(Disk$)) - 64

BIOSInt &H21

AX = Reg.AX
BX = Reg.BX
CX = Reg.CX
DX = Reg.DX

END SUB

FUNCTION DiskFSpace& (Disk$)

DiskDriver Disk$, AX, BX, CX, DX
DiskFSpace& = Int2Lng&(BX) * AX * CX

END FUNCTION

FUNCTION DiskTSpace& (Disk$)

DiskDriver Disk$, AX, BX, CX, DX
DiskTSpace& = Int2Lng&(DX) * AX * CX

END FUNCTION

SUB DisplaySet (X%, Y%)

OUT &H3D4, &HC
OUT &H3D5, HByte((Y \ 16) * MaxX + X \ 9)

OUT &H3D4, &HD
OUT &H3D5, LByte((Y \ 16) * MaxX + X \ 9)

OUT &H3D4, &H8
OUT &H3D5, INP(&H3D5) AND &HE0 OR (Y AND 15)
OUT &H3C0, &H13 OR &H20
OUT &H3C0, (X + 8) MOD 9

END SUB

FUNCTION DOSVer$

Reg.AX = &H3000
BIOSInt &H21
MajVer = LByte(Reg.AX)
MinVer = HByte(Reg.AX)

MajVer$ = LTRIM$(RTRIM$(STR$(MajVer)))
MinVer$ = LTRIM$(RTRIM$(STR$(MinVer)))

 IF MinVer < 10 THEN MinVer$ = "0" + MinVer$

DOSVer$ = MajVer$ + "." + MinVer$

END FUNCTION

SUB Edit (Text$, Length%, InitPos%, Param$, FC%, BC%)

LeftArrow = 75
RightArrow = 77
Home = 71
EndKey = 79
Del = 83
Backspace = 8

IF ModeGet > 3 THEN
 GphFlag = 1
 TimeMark! = TIMER
 Time! = .1
END IF

 IF SGN(Length) = -1 THEN
ModFlag = 1
Length = ABS(Length)
 END IF

 ALoc = INSTR(Param$, "@")
 IF ALoc THEN
  Mask$ = MID$(Param$, ALoc + 1, 1)
Subst$ = LEFT$(Param$, ALoc - 1)
 ELSE
Subst$ = Param$
 END IF

 IF LEN(Text$) > Length THEN Text$ = MID$(Text$, 1, Length)

  'Initial values
  KeyPressed = 0
  InitPos = ABS(InitPos)
   IF InitPos AND InitPos < LEN(Text$) THEN Position = InitPos - 1 ELSE Position = LEN(Text$)
 IF LEN(Text$) >= Length AND ModFlag THEN Position = Length - 1
  Row = CSRLIN
  Column = POS(Row)

  'Main loop
 
  DO:
   
     MouseHide
      
       FOR X = 1 TO Length
       
	IF X <= LEN(Text$) THEN
	 A$ = MID$(Subst$, X, 1)
	  IF A$ = CHR$(0) OR X > LEN(Subst$) THEN
	   M$ = MID$(Text$, X, 1)
	  ELSE
	   M$ = MID$(Subst$, X, 1)
	  END IF
	ELSE
	 M$ = EditBgr$
	END IF

	ChrSet Column + X - 1, Row, ASC(M$), FC, BC
	IF X = Position + 1 THEN N$ = M$

       NEXT X

      IF GphFlag = 1 THEN
       ChrSet Column + Length, Row, 32, FC, BC
      END IF
    
     MouseShow
   
	Ins = BitGet(ShiftGet, 7)

X = ((Column + Position - 1) \ MaxX) + Row
Y = ((Column + Position - 1) MOD MaxX) + 1

    IF Ins = 0 THEN
      LOCATE X, Y, 1, 7, 8 'Line cursor
    ELSE
      LOCATE X, Y, 1, 8, 16 'Block cursor
    END IF

DO:

 Reg.AX = &H100
 BIOSInt &H16

  IF BitGet(Reg.Flags, 6) THEN
   Key$ = ""
  ELSE
   IF HByte(Reg.AX) = 0 THEN
    AltFlag = 1
   ELSE
    AltFlag = 0
   END IF

 IF LByte(Reg.AX) THEN
  Key$ = CHR$(LByte(Reg.AX))
 ELSE
  Key$ = CHR$(0) + CHR$(HByte(Reg.AX))
 END IF
  END IF

  IF GphFlag THEN
IF TimeMark! < TIMER THEN
 BlnFlag = NOT BlnFlag
 TimeMark! = TIMER + Time!
END IF

 IF BlnFlag THEN
  IF Ins THEN ChrSet Y, X, 219, FC, BC ELSE ChrSet Y, X, 220, FC, BC
 ELSE
  ChrSet Y, X, ASC(N$), FC, BC
 END IF
  END IF

 MouseStatus MX, MY, Buttons

LB = BitGet(Buttons, 0)
 IF LB AND OldLB THEN
  LB = 0
 ELSE
  OldLB = LB
 END IF

RB = BitGet(Buttons, 1)
 IF RB AND OldRB THEN
  RB = 0
 ELSE
  OldRB = RB
 END IF

MB = BitGet(Buttons, 2)
 IF MB AND OldMB THEN
  MB = 0
 ELSE
  OldMB = MB
 END IF

  IF LB THEN Key$ = "LB"
  IF RB THEN Key$ = "RB"
  IF MB THEN Key$ = "MB"

LOOP UNTIL Key$ <> ""

ClearKeyb
N$ = EditBgr$

      FOR Z = -2 TO 35
       IF Key$ = EditStopCode$(Z) AND AltFlag = 0 THEN EXIT DO
      NEXT Z
     IF Key$ = "LB" OR Key$ = "RB" OR Key$ = "MB" THEN Key$ = ""

    SELECT CASE LEFT$(Key$, 1)
      CASE CHR$(Backspace)
	IF AltFlag THEN GOTO AsciiCodes
	IF Position > 0 THEN
	  Text$ = LEFT$(Text$, Position - 1) + RIGHT$(Text$, LEN(Text$) - Position)
	  Position = Position - 1
	END IF
      CASE CHR$(0) 'Extended codes
	SELECT CASE ASC(RIGHT$(Key$, 1))
	 CASE Home
	    Position = 0
	  CASE LeftArrow
	    IF Position > 0 THEN Position = Position - 1
	  CASE RightArrow
	    IF Position < LEN(Text$) THEN Position = Position + 1
	  CASE EndKey
	    Position = LEN(Text$)
	  CASE Del
	    IF Position < LEN(Text$) THEN Text$ = LEFT$(Text$, Position) + RIGHT$(Text$, LEN(Text$) - Position - 1)
	END SELECT
     
      CASE ELSE
    
AsciiCodes:
      
       SELECT CASE UCASE$(Mask$)
       
	CASE "D"
	 SELECT CASE Key$
	  CASE "0" TO "9", ".", " "
	   ChrFlag = 1
	  CASE "-", "+"
	   IF Position = 0 THEN
	    ChrFlag = 1
	   ELSE
	    IF MID$(Text$, Position, 1) = " " THEN ChrFlag = 1
	   END IF
	 END SELECT

	CASE "H"
	 Key$ = UCASE$(Key$)
	 SELECT CASE Key$
	  CASE "0" TO "9", "A" TO "F", " "
	   ChrFlag = 1
	  END SELECT
	CASE ">"
	 IF Key$ >= CHR$(1) AND Key$ <= "ÿ" THEN
	  Key$ = UCASE$(Key$)
	 ChrFlag = 1
	 END IF
	CASE "<"
	 IF Key$ >= CHR$(1) AND Key$ <= "ÿ" THEN
	  Key$ = LCASE$(Key$)
	 ChrFlag = 1
	 END IF
	CASE ELSE
	 IF Key$ >= CHR$(1) AND Key$ <= "ÿ" THEN
	 ChrFlag = 1
	 END IF
      
       END SELECT

       IF ChrFlag THEN
	IF Ins = 0 AND LEN(Text$) < Length THEN
	  IF LEN(Text$) < Length THEN
	    Text$ = LEFT$(Text$, Position) + Key$ + RIGHT$(Text$, LEN(Text$) - Position)
	   Position = Position + 1
	  END IF
	ELSEIF Position < Length THEN
	  IF Position = LEN(Text$) THEN
	    Text$ = Text$ + Key$
	  ELSE
	    MID$(Text$, Position + 1, 1) = Key$
	  END IF
	  Position = Position + 1
	END IF
       END IF
    END SELECT

	IF Position = Length AND ModFlag THEN
	  Position = Length - 1
	END IF
 
   ChrFlag = 0

LOOP
 
  LOCATE Row, Column, 0
   PRINT ToPrint$;
   PRINT
  EditEndCode$ = Key$
  InitPos = Position + 1

END SUB

FUNCTION EXEName$
 
TMP$ = ""
Reg.AX = &H6200
BIOSInt &H21
 
DEF SEG = Reg.BX
DEF SEG = PEEK(&H2C) + PEEK(&H2D) * 256
 
Byte = 0

DO:
 IF PEEK(Byte) = 0 THEN
  IF PEEK(Byte + 1) = 0 THEN
   Byte = Byte + 2
   EXIT DO
  END IF
 END IF
   Byte = Byte + 1
LOOP

IF PEEK(Byte) = 1 THEN
 Byte = Byte + 2
 DO WHILE PEEK(Byte)
 TMP$ = TMP$ + CHR$(PEEK(Byte))
 Byte = Byte + 1
 LOOP
 EXEName$ = TMP$
END IF
 
END FUNCTION

SUB FadeSet (Prcnt!)

DIM Pal(0 TO 2)
Subtr = ((100 - Prcnt!) * 63) \ 100

FOR X = 0 TO 255

 Pal(0) = DefaultPal(X, 0) - Subtr
 Pal(1) = DefaultPal(X, 1) - Subtr
 Pal(2) = DefaultPal(X, 2) - Subtr

IF Pal(0) < 0 THEN Pal(0) = 0
IF Pal(1) < 0 THEN Pal(1) = 0
IF Pal(2) < 0 THEN Pal(2) = 0

IF Pal(0) > 63 THEN Pal(0) = 63
IF Pal(1) > 63 THEN Pal(1) = 63
IF Pal(2) > 63 THEN Pal(2) = 63

 PalSet X, Pal()

NEXT

END SUB

FUNCTION FileAttrib (Buffer$)

 FileAttrib = ASC(RIGHT$(Buffer$, 1))

END FUNCTION

SUB FileAttrSet (FileSpec$, Attr%)

NameFile$ = FileSpec$ + CHR$(0)

Reg.DS = VARSEG(NameFile$)
Reg.DX = SADD(NameFile$)
Reg.CX = Attr%
Reg.AX = &H4301

BIOSInt &H21

IF BitGet(Reg.Flags, 0) THEN Attr = 0

END SUB

SUB FileDate (Buffer$, Day%, Month%, Year%)

Date.$ = MID$(Buffer$, 19, 2) + STRING$(2, 0)
Date& = CVL(Date.$)

Year0 = Date& \ 512
Year = Year0 + 1980
Rest = Date& MOD 512
Month = Rest \ 32
Day = Rest MOD 32

END SUB

SUB FileDateSet (FileSpec$, Day%, Month%, Year%, Hour%, Min%, Sec%)

 IF FindFirst$(FileSpec$, 255) <> "" THEN

NameFile$ = FileSpec$ + CHR$(0)

Reg.DS = VARSEG(NameFile$)
Reg.DX = SADD(NameFile$)
Reg.AX = &H3D02

BIOSInt &H21

Handle = Reg.AX

Reg.AX = &H5701
Reg.BX = Handle
	Time& = Hour * 2048 + Min * 32 + Sec / 2
	Date& = (Year - 1980) * 512 + Month * 32 + Day

Reg.CX = Lng2Int(Time&)
Reg.DX = Lng2Int(Date&)

BIOSInt &H21

 ELSE
  FileSpec$ = ""
 END IF

END SUB

FUNCTION FileName$ (Buffer$)

FileName0$ = RTRIM$(MID$(Buffer$, 1, 12))
 IF FileAttrib(Buffer$) = 8 THEN
  P = INSTR(FileName0$, ".")
   IF P THEN
    FileName0$ = LEFT$(FileName0$, 8) + MID$(FileName0$, P + 1, LEN(FileName0$) - P)
   END IF
 END IF
FileName$ = FileName0$

END FUNCTION

FUNCTION FileSize& (Buffer$)

FileSize = CVL(MID$(Buffer$, 13, 4))

END FUNCTION

SUB FileTime (Buffer$, Hour%, Min%, Sec%)

Time.$ = MID$(Buffer$, 17, 2) + STRING$(2, 0)
Time& = CVL(Time.$)

Hour = Time& \ 2048
Rest = Time& MOD 2048
Min = Rest \ 32
Rest = Rest MOD 32
Sec = Rest * 2

END SUB

FUNCTION FindFirst$ (FileSpec$, Attr%)

DIM Name0 AS STRING * 12
Name0$ = STRING$(12, 32)

Reg.AX = &H2F00
 
BIOSInt &H21
 
DTASegment = Reg.ES
DTAOffset = Reg.BX
 
NameFile$ = FileSpec$ + CHR$(0)
 
Reg.DS = VARSEG(NameFile$)
Reg.DX = SADD(NameFile$)
Reg.CX = Attr%
Reg.AX = &H4E00
 
BIOSInt &H21

IF BitGet(Reg.Flags, 0) THEN
	FindFirst$ = ""
	EXIT FUNCTION
END IF
 
DEF SEG = DTASegment
MatchOffset = DTAOffset

'Getting File Name...
FOR I = 30 TO 43
NewChar$ = CHR$(PEEK(MatchOffset + I))
IF NewChar$ = CHR$(0) THEN EXIT FOR
Name$ = Name$ + NewChar$
NEXT I

'Getting File Date/Time...
FOR I = 22 TO 26
 Date.Time$ = Date.Time$ + CHR$(PEEK(MatchOffset + I))
NEXT I

'Getting File Attribute...
 attrib$ = CHR$(PEEK(MatchOffset + 21))

'Getting File Size...
FOR I = 26 TO 29
 Size$ = Size$ + CHR$(PEEK(MatchOffset + I))
NEXT I

DEF SEG

MID$(Name0$, 1, 12) = Name$

FindFirst$ = Name0$ + Size$ + Date.Time$ + attrib$

END FUNCTION

FUNCTION FindNext$

DIM Name0 AS STRING * 12
Name0$ = STRING$(12, 32)

Reg.AX = &H2F00
 
BIOSInt &H21

DTASegment = Reg.ES
DTAOffset% = Reg.BX
 
Reg.AX = &H4F00
BIOSInt &H21

IF BitGet(Reg.Flags, 0) THEN
	FindNext$ = ""
	EXIT FUNCTION
END IF

DEF SEG = DTASegment
MatchOffset = DTAOffset

'Getting File Name...
FOR I = 30 TO 43
NewChar$ = CHR$(PEEK(MatchOffset + I))
IF NewChar$ = CHR$(0) THEN EXIT FOR
Name$ = Name$ + NewChar$
NEXT I

'Getting File Date/Time...
FOR I = 22 TO 26
 Date.Time$ = Date.Time$ + CHR$(PEEK(MatchOffset + I))
NEXT I

'Getting File Attribute...
 attrib$ = CHR$(PEEK(MatchOffset + 21))

'Getting File Size...
FOR I = 26 TO 29
 Size$ = Size$ + CHR$(PEEK(MatchOffset + I))
NEXT I

DEF SEG

MID$(Name0$, 1, 12) = Name$

FindNext$ = Name0$ + Size$ + Date.Time$ + attrib$

END FUNCTION

FUNCTION FloppyList$
   
    DEF SEG = 0
    Floppies% = PEEK(&H410) \ 64 + 1   'How many floppy drives installed?
    DEF SEG                            'Back to DGROUP

    FOR N% = 1 TO Floppies%            'Place these letters into Floppies$
	Floppy$ = Floppy$ + CHR$(64 + N%)
    NEXT

FloppyList$ = Floppy$

END FUNCTION

SUB Frame (HIP, VIP, HEP, VEP, Font$, CF, CL, CR)

UL$ = MID$(Font$, 1, 1)
HUL$ = MID$(Font$, 2, 1)
UR$ = MID$(Font$, 3, 1)

VL$ = MID$(Font$, 4, 1)
FS$ = MID$(Font$, 5, 1)
VR$ = MID$(Font$, 6, 1)

DL$ = MID$(Font$, 7, 1)
HDL$ = MID$(Font$, 8, 1)
DR$ = MID$(Font$, 9, 1)

A = CSRLIN
B = POS(A)

 ChrSet HIP, VIP, ASC(UL$), CL, CF
 LOCATE VIP, HIP + 1
  ChrString ASC(HUL$), HEP - HIP - 1, CL, CF
 ChrSet HEP, VIP, ASC(UR$), CR, CF

    FOR ALine = VIP + 1 TO VEP - 1
  
   ChrSet HIP, ALine, ASC(VL$), CL, CF
     IF FS$ <> CHR$(0) THEN
    LOCATE ALine, HIP + 1
   ChrString ASC(FS$), HEP - HIP - 1, CL, CF
     END IF
   ChrSet HEP, ALine, ASC(VR$), CR, CF

    NEXT ALine

 ChrSet HIP, VEP, ASC(DL$), CL, CF
 LOCATE VEP, HIP + 1
  ChrString ASC(HDL$), HEP - HIP - 1, CR, CF
 ChrSet HEP, VEP, ASC(DR$), CR, CF

LOCATE A, B

END SUB

SUB GrabPal

DIM Pal(0 TO 2)

FOR X = 0 TO 255

 PalGet X, Pal()

 DefaultPal(X, 0) = Pal(0)
 DefaultPal(X, 1) = Pal(1)
 DefaultPal(X, 2) = Pal(2)

NEXT

END SUB

FUNCTION HByte (Var%)

DEF SEG = VARSEG(Var)
 HByte = PEEK(VARPTR(Var) + 1)
DEF SEG

END FUNCTION

FUNCTION HDriveList$

    'DOS function AH = 44h & AL = 09h is "Device Driver Control (IOCTL)" and
    'tests whether a drive is local or remote.  If the carry flag is set then
    'AX will return 0Fh when drive letter is invalid.  This routine simply
    'checks to see if the carry flag is set and ends if it is.

    FOR BL% = 3 TO 26          'Roll through possible hard drives.
	Reg.AX = &H4409                 'DOS function "Device Driver Control"
	Reg.BX = BL%                    'Drive letter in BL register.
	BIOSInt &H21                    'Call Mr DOS (INT 21h).
	IF BitGet(Reg.Flags, 0) = 0 THEN  'Check carry flag.
	HDList$ = HDList$ + CHR$(64 + Reg.BX)   'Add the letter
	END IF
    NEXT

    HDriveList$ = HDList$                'Set the function.

END FUNCTION

FUNCTION InfoFExtn$ (FileSpec$)

DO:
 LastN = N
 N = INSTR(N + 1, FileSpec$, ".")
LOOP UNTIL N = 0

IF LastN = 0 THEN
 InfoFExtn$ = ""
ELSE
 InfoFExtn$ = RIGHT$(FileSpec$, LEN(FileSpec$) - LastN)
END IF

END FUNCTION

FUNCTION InfoFName$ (FileSpec$)

DO:
 LastN = N
 N = INSTR(N + 1, FileSpec$, "\")
LOOP UNTIL N = 0

DO:
 LastO = O
 O = INSTR(O + 1, FileSpec$, ".")
LOOP UNTIL O = 0

IF LastO = 0 THEN LastO = LEN(FileSpec$) + 1

InfoFName$ = MID$(FileSpec$, LastN + 1, LastO - LastN - 1)

END FUNCTION

FUNCTION InfoFPath$ (FileSpec$)

DO:
 LastN = N
 N = INSTR(N + 1, FileSpec$, "\")
LOOP UNTIL N = 0

InfoFPath$ = LEFT$(FileSpec$, LastN)

END FUNCTION

FUNCTION Int2Lng& (Value%)

IF Value >= 0 THEN
	Int2Lng0& = Value
ELSE
	Int2Lng0& = Value + 65536
END IF

Int2Lng& = Int2Lng0&

END FUNCTION

FUNCTION Int2StrD$ (Value%, Set$)

Value$ = LTRIM$(RTRIM$(STR$(Value)))
Int2Str0$ = Set$
MID$(Int2Str0$, LEN(Set$) - LEN(Value$) + 1, LEN(Value$)) = Value$

Int2StrD$ = Int2Str0$

END FUNCTION

FUNCTION Int2StrH$ (Value%, Set$)

Value$ = HEX$(Value)
Int2Str0$ = Set$
MID$(Int2Str0$, LEN(Set$) - LEN(Value$) + 1, LEN(Value$)) = Value$

Int2StrH$ = Int2Str0$

END FUNCTION

SUB IntVecGet (IntN%, Segm%, Offs%)

Reg.AX = &H3500 + IntN
BIOSInt &H21
Segm = Reg.ES
Offs = Reg.BX

END SUB

SUB IntVecSet (IntN%, Segm%, Offs%)

Reg.AX = &H2500 + IntN
Reg.DX = Offs
Reg.DS = Segm

BIOSInt &H21

END SUB

FUNCTION LByte (Var%)

 LByte = (Var AND &HFF)

END FUNCTION

FUNCTION Lng2Int (Value&)

IF Value& >= 32767 THEN
 Lng2Int0 = Value& - 65536
ELSE
 Lng2Int0 = Value&
END IF

Lng2Int = Lng2Int0

END FUNCTION

SUB MatrixSort (SortVector$(), Low%, High%, Seq%)

ContNumber = High - Low + 1
Div = ContNumber \ 2

DO WHILE Div > 0
   FOR X = Div TO High - 1
     Y = X - Div + 1
	FOR Y = (X - Div + 1) TO Low STEP -Div
	 IF Seq = 0 THEN
	IF SortVector$(Y) <= SortVector$(Y + Div) THEN EXIT FOR
	 ELSE
	IF SortVector$(Y) >= SortVector$(Y + Div) THEN EXIT FOR
	 END IF
    SWAP SortVector$(Y), SortVector$(Y + Div)
	NEXT Y
   NEXT X

  Div = Div \ 2
LOOP

END SUB

FUNCTION MaxX

DEF SEG = 0
 MaxX = PEEK(&H44A)
DEF SEG

END FUNCTION

FUNCTION MaxY

DEF SEG = 0
 MaxY = PEEK(&H484) + 1
DEF SEG

END FUNCTION

DEFSNG A-Z
FUNCTION ModeGet%

DEF SEG = 0
 ModeGet% = PEEK(&H449)
DEF SEG

END FUNCTION

DEFINT A-Z
SUB ModeSet (Mode%)

 IF Mode >= 0 AND Mode <= 255 THEN
  Reg.AX = Mode%
  BIOSInt &H10
 END IF

END SUB

SUB MouseCursor (Code%, FC%, BC%)

IF FC > 15 OR FC < 0 THEN STOP
IF BC > 7 OR BC < 0 THEN STOP

 DX = (BC * 16 + FC) * 256 + Code
 MouseDriver 10, 0, 0, DX%

END SUB

SUB MouseDriver (AX%, BX%, CX%, DX%)

Reg.AX = AX
Reg.BX = BX
Reg.CX = CX
Reg.DX = DX
 BIOSInt 51
DX = Reg.DX
CX = Reg.CX
BX = Reg.BX
AX = Reg.AX

END SUB

SUB MouseHide

 AX = 2
 MouseDriver AX, 0, 0, 0

END SUB

FUNCTION MouseInit%
 
  AX = 0
  MouseDriver AX, 0, 0, 0
  MouseInit = ABS(AX)

END FUNCTION

SUB MousePut (X%, Y%)
 
  AX = 4
  CX = X
  DX = Y
  MouseDriver AX, 0, CX, DX

END SUB

SUB MouseRange (X1%, Y1%, X2%, Y2%)
 
  AX = 7
  CX = X1
  DX = X2
  MouseDriver AX, 0, CX, DX
  AX = 8
  CX = Y1
  DX = Y2
  MouseDriver AX, 0, CX, DX

END SUB

SUB MouseShow
 
 AX = 1
 MouseDriver AX, 0, 0, 0

END SUB

SUB MouseStatus (XMouse%, YMouse%, Buttons%)
 
  AX = 3
  MouseDriver AX, BX, CX, DX
  XMouse = CX
  YMouse = DX
  Buttons = BX

END SUB

FUNCTION PageGet

DEF SEG = 0
 Page = PEEK(&H462)
DEF SEG

END FUNCTION

SUB PageSet (Value%)

Reg.AX = (256 * 5) + Value
BIOSInt &H10

END SUB

SUB PalGet (nColor%, Array%())

OUT &H3C6, &HFF
OUT &H3C7, nColor
Array(0) = INP(&H3C9)
Array(1) = INP(&H3C9)
Array(2) = INP(&H3C9)

END SUB

SUB PalSet (nColor%, Array%())

OUT &H3C6, &HFF
OUT &H3C8, nColor
OUT &H3C9, Array(0)
OUT &H3C9, Array(1)
OUT &H3C9, Array(2)

END SUB

SUB Pause (Time!)

MSTime& = FIX(Time! * 1000000)
MSTimePos = VARPTR(MSTime&)

Reg.AX = &H8600

CX& = (PEEK(MSTimePos + 3) * 2 ^ 8) + (PEEK(MSTimePos + 2))
DX& = (PEEK(MSTimePos + 1) * 2 ^ 8) + (PEEK(MSTimePos + 0))

Reg.CX = Lng2Int(CX&)
Reg.DX = Lng2Int(DX&)

BIOSInt &H15

END SUB

SUB PrintScreen

 BIOSInt (5)

END SUB

DEFDBL A-Z
FUNCTION RAD# (DEGval#)

 RAD = (PI * DEGval) / 180

END FUNCTION

DEFSNG A-Z
SUB ReBoot

OUT &H64, &HFE

END SUB

DEFINT A-Z
SUB ScreenGet (IX%, IY%, EX%, EY%, Buffer$)

Buffer$ = ""
 FOR Y = IY TO EY
  FOR X = IX TO EX
   ChrGet X, Y, Code, Attr, 255
   Buffer$ = Buffer$ + CHR$(Code) + CHR$(Attr)
  NEXT
 NEXT
Buffer$ = Buffer$ + CHR$(EX - IX)

END SUB

SUB ScreenSet (IX%, IY%, Mode%, Buffer$)

Xs = ASC(RIGHT$(Buffer$, 1)) + 1
Ys = (LEN(Buffer$) - 1) / Xs / 2

 FOR Y = 1 TO Ys
  FOR X = 1 TO Xs
   Z = Z + 1

 Location = (Xs * 2) * (Y - 1) + 2 * (X - 1) + 1
  Code = ASC(MID$(Buffer$, Location, 1))
  Attr = ASC(MID$(Buffer$, Location + 1, 1))

 IF Mode = 0 THEN
  SetX = X + IX - 1
  SetY = Y + IY - 1
 ELSEIF Mode = 1 THEN
  SetX = (IX + Xs) - X
  SetY = Y + IY - 1
 ELSEIF Mode = 2 THEN
  SetX = X + IX - 1
  SetY = (IY + Ys) - Y
 ELSEIF Mode = 3 THEN
  SetX = (IX + Xs) - X
  SetY = (IY + Ys) - Y
 END IF

 ChrSet SetX, SetY, Code, Attr, 255

  NEXT
 NEXT

END SUB

FUNCTION ShiftGet

DEF SEG = 0
 ShiftGet = PEEK(&H417)
DEF SEG

END FUNCTION

SUB ShiftSet (Value%)

DEF SEG = 0
 POKE &H417, Value
DEF SEG

A$ = INKEY$

END SUB

SUB SoundFreq (Freq!)

Counter = 1193280 \ Freq!

OUT 67, 182
OUT 66, LByte(Counter)
OUT 66, HByte(Counter)

END SUB

SUB SoundOff

Port = INP(97)
 BitSet Port, 0, 0
 BitSet Port, 1, 0
  OUT 97, Port

END SUB

SUB SoundOn

Port = INP(97)
 BitSet Port, 0, 1
 BitSet Port, 1, 1
  OUT 97, Port

END SUB

SUB TextSizeSet (X%, Y%)
 
OUT &H3D4, &H13
OUT &H3D5, X \ 2

DEF SEG = 0
 POKE &H44A, X
 POKE &H484, Y - 1
 POKE &H44C, X * Y

END SUB

SUB TimeGet (Hour%, Min%, Sec%, CSec%)

Reg.AX = &H2C00
 BIOSInt &H21
  Hour = HByte(Reg.CX)
  Min = LByte(Reg.CX)
  Sec = HByte(Reg.DX)
  CSec = LByte(Reg.DX)

END SUB

SUB TimeSet (Hour%, Min%, Sec%, CSec%)

Reg.AX = &H2D00
Reg.CX = (256 * Hour) + Min
Reg.DX = (256 * Sec) + CSec
 BIOSInt &H21

END SUB

SUB VidWait

DO UNTIL BitGet(INP(&H3DA), 3) = 1: LOOP
DO UNTIL BitGet(INP(&H3DA), 3) = 0: LOOP

END SUB

FUNCTION WeekDay (Day%, Month%, Year%)

DateGet OldDay, OldMonth, OldYear, OldWD
DateSet Day, Month, Year
DateGet Day, Month, Year, NewWD
WeekDay = NewWD
DateSet OldDay, OldMonth, OldYear

END FUNCTION

FUNCTION WinVer$
 
  ASM$ = ""
  ASM$ = ASM$ + CHR$(&H55)                           'push bp
  ASM$ = ASM$ + CHR$(&H89) + CHR$(&HE5)              'mov bp, sp
  ASM$ = ASM$ + CHR$(&HB8) + CHR$(&HA) + CHR$(&H16)  'mov ax, 160a
  ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H2F)              'int 2f
  ASM$ = ASM$ + CHR$(&H3D) + CHR$(&H0) + CHR$(&H0)   'cmp ax, 0000
  ASM$ = ASM$ + CHR$(&H75) + CHR$(&H17)              'jnz 0124
  ASM$ = ASM$ + CHR$(&HBA) + CHR$(&H0) + CHR$(&H0)   'mov dx, 0000
  ASM$ = ASM$ + CHR$(&H89) + CHR$(&HD8)              'mov ax, bx
  ASM$ = ASM$ + CHR$(&H88) + CHR$(&HE2)              'mov dl, ah
  ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)  'mov bx, [bp+08]
  ASM$ = ASM$ + CHR$(&H89) + CHR$(&H17)              'mov [bx],dx
  ASM$ = ASM$ + CHR$(&H88) + CHR$(&HC2)              'mov dl, al
  ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)  'mov bx, [bp+06]
  ASM$ = ASM$ + CHR$(&H89) + CHR$(&H17)              'mov [bx], dx
  ASM$ = ASM$ + CHR$(&H5D)                           'pop bp
  ASM$ = ASM$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)   'retf
  ASM$ = ASM$ + CHR$(&HB8) + CHR$(&H0) + CHR$(&H16)  'mov ax, 1600
  ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H2F)              'int 2f
  ASM$ = ASM$ + CHR$(&H3C) + CHR$(&H1)               'cmp al, 01
  ASM$ = ASM$ + CHR$(&H74) + CHR$(&H19)              'jz 0146
  ASM$ = ASM$ + CHR$(&H3C) + CHR$(&H80)              'cmp al, 80
  ASM$ = ASM$ + CHR$(&H74) + CHR$(&H1A)              'jz 014b
  ASM$ = ASM$ + CHR$(&HBA) + CHR$(&H0) + CHR$(&H0)   'mov dx, 0000
  ASM$ = ASM$ + CHR$(&H88) + CHR$(&HC2)              'mov dl, al
  ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)  'mov bx, [bp+08]
  ASM$ = ASM$ + CHR$(&H89) + CHR$(&H17)              'mov [bx], dx
  ASM$ = ASM$ + CHR$(&H88) + CHR$(&HE2)              'mov dl, ah
  ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)  'mov bx, [bp+06]
  ASM$ = ASM$ + CHR$(&H89) + CHR$(&H17)              'mov [bx], dx
  ASM$ = ASM$ + CHR$(&H5D)                           'pop bp
  ASM$ = ASM$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)   'retf 0004
  ASM$ = ASM$ + CHR$(&HB8) + CHR$(&H2) + CHR$(&H0)   'mov ax, 0002
  ASM$ = ASM$ + CHR$(&HEB) + CHR$(&HE6)              'jmp 0131
  ASM$ = ASM$ + CHR$(&HB8) + CHR$(&H0) + CHR$(&H0)   'mov ax, 0000
  ASM$ = ASM$ + CHR$(&HEB) + CHR$(&HE1)              'jmp 0131

  ASMSeg = VARSEG(ASM$)
  ASMOff = SADD(ASM$)
 
  DEF SEG = ASMSeg
   CALL ABSOLUTE(MajVer, MinVer, ASMOff)
  DEF SEG
 
MajVer$ = LTRIM$(RTRIM$(STR$(MajVer)))
MinVer$ = LTRIM$(RTRIM$(STR$(MinVer)))
 
IF MinVer < 10 THEN MinVer$ = "0" + MinVer$
 
 IF MajVer = 0 THEN
  WinVer$ = ""
 ELSE
  WinVer$ = MajVer$ + "." + MinVer$
 END IF

END FUNCTION

