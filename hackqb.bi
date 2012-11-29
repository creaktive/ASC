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
' ���������������������������������������������ͻ
' � Bit     � Bit    �                          �
' � number: � value: � Description:             �
' ���������������������������������������������Ķ
' �       0 �      1 �  Left button             �
' �       1 �      2 �  Right button            �
' �       2 �      4 �  Middle button           �
' ���������������������������������������������ͼ

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
' ���������������������������������������������ͻ
' � Bit     � Bit    �                          �
' � number: � value: � Description:             �
' ���������������������������������������������Ķ
' �       0 �      1 �  Read only               �
' �       1 �      2 �  Hidden                  �
' �       2 �      4 �  System                  �
' �       3 �      8 �  Volume label            �
' �       4 �     16 �  Subdirectory name       �
' �       5 �     32 �  Archive                 �
' �       6 �     64 �  Not used                �
' �       7 �    128 �  Not used                �
' ���������������������������������������������ͼ

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
' ���������������������������������������������ͻ
' � Bit     � Bit    �                          �
' � number: � value: � Description:             �
' ���������������������������������������������Ķ
' �       0 �      1 �  Right shift             �
' �       1 �      2 �  Left shift              �
' �       2 �      4 �  Control                 �
' �       3 �      8 �  Alt                     �
' �       4 �     16 �  Scroll Lock             �
' �       5 �     32 �  Num Lock                �
' �       6 �     64 �  Caps Lock               �
' �       7 �    128 �  Insert state            �
' ���������������������������������������������ͼ

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
FrameFont$(0) = FrameFont$(0) + "�Ŀ"
FrameFont$(0) = FrameFont$(0) + "� �"
FrameFont$(0) = FrameFont$(0) + "���"

FrameFont$(1) = ""
FrameFont$(1) = FrameFont$(1) + "�ͻ"
FrameFont$(1) = FrameFont$(1) + "� �"
FrameFont$(1) = FrameFont$(1) + "�ͼ"

FrameFont$(2) = ""
FrameFont$(2) = FrameFont$(2) + "�ķ"
FrameFont$(2) = FrameFont$(2) + "� �"
FrameFont$(2) = FrameFont$(2) + "�Ľ"

FrameFont$(3) = ""
FrameFont$(3) = FrameFont$(3) + "�͸"
FrameFont$(3) = FrameFont$(3) + "� �"
FrameFont$(3) = FrameFont$(3) + "�;"

FrameFont$(4) = ""
FrameFont$(4) = FrameFont$(4) + "���"
FrameFont$(4) = FrameFont$(4) + "� �"
FrameFont$(4) = FrameFont$(4) + "���"

Page = 0
BlinkState = 1

Blink BlinkState

GrabPal