000100******************************************************************
000200*PROGRAM : Test of SCREEN SECTION. (Multiple disp/accepts)       *
000300*AUTHOR  : K. Pappas                                             *
000400*DATE    : 09/26/2012                                            *
000500*ABSTRACT: Uses a SCREEN SECTION entry for each field.           *
000600*          Allows enter key to navigate (non-auto fields).       *
000700******************************************************************
000800 IDENTIFICATION DIVISION.
000900 PROGRAM-ID. SCREEN-TEST-2-ENTER.
001000*-----------------------------------------------------------------
001100 ENVIRONMENT DIVISION.
001200*-----------------------------------------------------------------
001300 DATA DIVISION.
001400 WORKING-STORAGE SECTION.
001500 01  WS-REC.
001600     03  WS-NAME.
001700         05  WS-FNAME        PIC X(15).
001800         05  WS-LNAME        PIC X(15).
001900     03  WS-PHONE.
002000         05  WS-PH-AC        PIC XXX.
002100         05  WS-PH-3         PIC XXX.
002200         05  WS-PH-4         PIC XXXX.
002300     03  WS-EMAIL1           PIC X(40).
002400     03  WS-EMAIL2           PIC X(40).
002500
002600 01  WS-MISC.
002700     03  WS-ACTION           PIC X.
002800
002900*-----------------------------------------------------------------
003000 SCREEN SECTION.
003100 01  SCRN-TITLE.
003200     03  BLANK SCREEN.
003300     03  LINE 02 COL 34 VALUE "Screen Example 2".
003400
003500 01  SCRN-FNAME.
003600     03  LINE 05 COL 21 VALUE "First Name".
003700     03          COL 37 PIC X(15)   TO WS-FNAME.
003800 01  SCRN-LNAME.
003900     03  LINE 06 COL 21 VALUE "Last  Name".
004000     03          COL 37 PIC X(15)   TO WS-LNAME.
004100 01  SCRN-PHONE.
004200     03  LINE 07 COL 21 VALUE "Phone".
004300     03          COL 37 PIC XXX     TO WS-PH-AC  AUTO.
004400     03          COL 40 VALUE "-".
004500     03          COL 41 PIC XXX     TO WS-PH-3   AUTO.
004600     03          COL 44 VALUE "-".
004700     03          COL 45 PIC XXXX    TO WS-PH-4   AUTO.
004800 01  SCRN-EMAIL1.
004900     03  LINE 08 COL 21 VALUE "email 1".
005000     03          COL 37 PIC X(40)   TO WS-EMAIL1 AUTO.
005100 01  SCRN-EMAIL2.
005200     03  LINE 09 COL 21 VALUE "email 2".
005300     03          COL 37 PIC X(40)   TO WS-EMAIL2 AUTO.
005400     03  LINE 11 COL 31 VALUE "Enter key, items not grouped".
005500
005600 01  SCRN-ACTION.
005700         03  LINE 11 COL 30 PIC X ERASE EOL  TO WS-ACTION AUTO.
005800         03          COL 32 VALUE "(A)dd, (E)xit".
005900
006000 01  SCRN-PROG-TERM.
006100     03  BLANK SCREEN.
006200     03  LINE 12 COL 34 VALUE "PROGRAM ENDED".
006300
006400*-----------------------------------------------------------------
006500 PROCEDURE DIVISION.
006600 100-MAIN.
006700     DISPLAY SCRN-TITLE.
006800     DISPLAY SCRN-FNAME.
006900     DISPLAY SCRN-LNAME.
007000     DISPLAY SCRN-PHONE.
007100     DISPLAY SCRN-EMAIL1.
007200     DISPLAY SCRN-EMAIL2.
007300
007400     ACCEPT  SCRN-FNAME.
007500     ACCEPT  SCRN-LNAME.
007600     ACCEPT  SCRN-PHONE.
007700     ACCEPT  SCRN-EMAIL1.
007800     ACCEPT  SCRN-EMAIL2.
007900
008000     DISPLAY SCRN-ACTION.
008100     ACCEPT  SCRN-ACTION.
008200
008300     DISPLAY SCRN-PROG-TERM.
008400
008500     STOP RUN.
008600
