000100******************************************************************
000200*PROGRAM : Test of SCREEN SECTION. (Grouped screen entries)      *
000300*AUTHOR  : K. Pappas                                             *
000400*DATE    : 09/26/2012                                            *
000500*ABSTRACT: Uses a SCREEN SECTION entry for each field.           *
000600*          Allows enter key to navigate (non-auto fields).       *
000700*          Grouped fields allows fewer DISPLAY stmts.            *
000800******************************************************************
000900 IDENTIFICATION DIVISION.
001000 PROGRAM-ID. SCREEN-TEST-3-GROUP.
001100*-----------------------------------------------------------------
001200 ENVIRONMENT DIVISION.
001300*-----------------------------------------------------------------
001400 DATA DIVISION.
001500 WORKING-STORAGE SECTION.
001600 01  WS-REC.
001700     03  WS-NAME.
001800         05  WS-FNAME        PIC X(15).
001900         05  WS-LNAME        PIC X(15).
002000     03  WS-PHONE.
002100         05  WS-PH-AC        PIC XXX.
002200         05  WS-PH-3         PIC XXX.
002300         05  WS-PH-4         PIC XXXX.
002400     03  WS-EMAIL1           PIC X(40).
002500     03  WS-EMAIL2           PIC X(40).
002600
002700 01  WS-MISC.
002800     03  WS-ACTION           PIC X.
002900
003000*-----------------------------------------------------------------
003100 SCREEN SECTION.
003200 01  SCRN-TITLE.
003300     03  BLANK SCREEN.
003400     03  LINE 02 COLUMN 34 VALUE "Screen Example 3".
003500
003600 01  SCRN-DATA-FIELDS.
003700     03  SCRN-FNAME.
003800         05  LINE 05 COL 21 VALUE "First Name".
003900         05          COL 37 PIC X(15)   TO WS-FNAME.
004000     03  SCRN-LNAME.
004100         05  LINE 06 COL 21 VALUE "Last  Name".
004200         05          COL 37 PIC X(15)   TO WS-LNAME.
004300     03  SCRN-PHONE.
004400         05  LINE 07 COL 21 VALUE "Phone".
004500         05          COL 37 PIC XXX     TO WS-PH-AC  AUTO.
004600         05          COL 40 VALUE "-".
004700         05          COL 41 PIC XXX     TO WS-PH-3   AUTO.
004800         05          COL 44 VALUE "-".
004900         05          COL 45 PIC XXXX    TO WS-PH-4   AUTO.
005000     03  SCRN-EMAIL1.
005100         05  LINE 08 COL 21 VALUE "email 1".
005200         05          COL 37 PIC X(40)   TO WS-EMAIL1 AUTO.
005300     03  SCRN-EMAIL2.
005400         05  LINE 09 COL 21 VALUE "email 2".
005500         05          COL 37 PIC X(40)   TO WS-EMAIL2 AUTO.
005600     03  LINE 11 COL 31 VALUE "Enter key, items grouped".
005700
005800     01  SCRN-ACTION.
005900         03  LINE 11 COL 30 PIC X ERASE EOL  TO WS-ACTION AUTO.
006000         03          COL 32 VALUE "(A)dd, (E)xit".
006100
006200 01  SCRN-PROG-TERM.
006300     03  BLANK SCREEN.
006400     03  LINE 12 COL 34 VALUE "PROGRAM ENDED".
006500*-----------------------------------------------------------------
006600 PROCEDURE DIVISION.
006700 100-MAIN.
006800     DISPLAY SCRN-TITLE.
006900     DISPLAY SCRN-DATA-FIELDS.
007000
007100     ACCEPT  SCRN-FNAME.
007200     ACCEPT  SCRN-LNAME.
007300     ACCEPT  SCRN-PHONE.
007400     ACCEPT  SCRN-EMAIL1.
007500     ACCEPT  SCRN-EMAIL2.
007600
007700     DISPLAY SCRN-ACTION.
007800     ACCEPT  SCRN-ACTION.
007900
008000     DISPLAY SCRN-PROG-TERM.
008100
008200     STOP RUN.
008300
