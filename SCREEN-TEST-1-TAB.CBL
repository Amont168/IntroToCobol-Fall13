000100******************************************************************
000200*PROGRAM : Test of SCREEN SECTION. (Single disp/accept for data) *
000300*AUTHOR  : K. Pappas                                             *
000400*DATE    : 09/26/2012                                            *
000500*ABSTRACT: Uses one SCREEN SECTION entry for all data fields.    *
000600*          Requires tab key to navigate (non-auto fields).       *
000700*          Use of enter key causes remaining fields to be        *
000800*          skipped and cursor advanced to next disp/accept.      *
000900******************************************************************
001000 IDENTIFICATION DIVISION.
001100 PROGRAM-ID. SCREEN-TEST-1-TAB.
001200*-----------------------------------------------------------------
001300 ENVIRONMENT DIVISION.
001400*-----------------------------------------------------------------
001500 DATA DIVISION.
001600 WORKING-STORAGE SECTION.
001700 01  WS-REC.
001800     03  WS-NAME.
001900         05  WS-FNAME        PIC X(15).
002000         05  WS-LNAME        PIC X(15).
002100     03  WS-PHONE.
002200         05  WS-PH-AC        PIC XXX.
002300         05  WS-PH-3         PIC XXX.
002400         05  WS-PH-4         PIC XXXX.
002500     03  WS-EMAIL1           PIC X(40).
002600     03  WS-EMAIL2           PIC X(40).
002700
002800 01  WS-MISC.
002900     03  WS-ACTION           PIC X.
003000
003100*-----------------------------------------------------------------
003200 SCREEN SECTION.
003300 01  SCRN-DATA.
003400     03  BLANK SCREEN.
003500     03  LINE 02 COL 34 VALUE "Screen Example 1".
003600     03  LINE 05 COL 21 VALUE "First Name".
003700     03          COL 37 PIC X(15)   TO WS-FNAME.
003800     03  LINE 06 COL 21 VALUE "Last  Name".
003900     03          COL 37 PIC X(15)   TO WS-LNAME.
004000     03  LINE 07 COL 21 VALUE "Phone".
004100     03          COL 37 PIC XXX     TO WS-PH-AC  AUTO.
004200     03          COL 40 VALUE "-".
004300     03          COL 41 PIC XXX     TO WS-PH-3   AUTO.
004400     03          COL 44 VALUE "-".
004500     03          COL 45 PIC XXXX    TO WS-PH-4   AUTO.
004600     03  LINE 08 COL 21 VALUE "email 1".
004700     03          COL 37 PIC X(40)   TO WS-EMAIL1 AUTO.
004800     03  LINE 09 COL 21 VALUE "email 2".
004900     03          COL 37 PIC X(40)   TO WS-EMAIL2 AUTO.
005000     03  LINE 11 COL 31 VALUE "Requires use of tab key".
005100
005200 01  SCRN-ACTION.
005300         03  LINE 11 COL 30 PIC X ERASE EOL  TO WS-ACTION AUTO.
005400         03          COL 32 VALUE "(A)dd, (E)xit".
005500
005600 01  SCRN-PROG-TERM.
005700     03  BLANK SCREEN.
005800     03  LINE 12 COL 34 VALUE "PROGRAM ENDED".
005900
006000*-----------------------------------------------------------------
006100 PROCEDURE DIVISION.
006200 100-MAIN.
006300     DISPLAY SCRN-DATA.
006400     ACCEPT  SCRN-DATA.
006500
006600     DISPLAY SCRN-ACTION.
006700     ACCEPT  SCRN-ACTION.
006800
006900     DISPLAY SCRN-PROG-TERM.
007000
007100     STOP RUN.
007200
