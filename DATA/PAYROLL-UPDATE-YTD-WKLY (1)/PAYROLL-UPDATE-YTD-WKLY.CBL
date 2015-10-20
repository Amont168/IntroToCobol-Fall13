000100******************************************************************
000200* PROGRAM : PAYROLL-READ-DISP.CBL                                *
000300* AUTHOR  : K. PAPPAS                                            *
000400* DATE    : 09/10/2013                                           *
000500* ABSTRACT: DEMO OF READ AND DISPLAY A FILE.                     *
000600* CHANGE LOG                                                     *
000700* 09/10/13 K.PAPPAS     CREATED                                  *
000800******************************************************************
000900 IDENTIFICATION DIVISION.
001000 PROGRAM-ID. PAYROLL-READ-DISP.
001100*-----------------------------------------------------------------
001200 ENVIRONMENT DIVISION.
001300 INPUT-OUTPUT SECTION.
001400 FILE-CONTROL.
001500     SELECT EMP-MST-IN           ASSIGN TO "EMP-MST-OLD.TXT"
001600                                 ORGANIZATION IS LINE SEQUENTIAL.
001700*-----------------------------------------------------------------
001800 DATA DIVISION.
001900 FILE SECTION.
002000 FD  EMP-MST-IN.
002100 01  EMP-REC-IN.
002200     03  EMP-ID-IN           PIC X(5).
002300     03  EMP-FNAME-IN        PIC X(10).
002400     03  EMP-LNAME-IN        PIC X(10).
002500     03  EMP-PRATE-IN        PIC 9(7)V99.
002600     03  EMP-YTD-IN          PIC 9(7)V99.
002700
002800 WORKING-STORAGE SECTION.
002900 01  WS-REC-OUT.
003000     03  WS-ID               PIC X(5).
003100     03  FILLER              PIC X               VALUE SPACES.
003200     03  WS-FNAME            PIC X(10).
003300     03  WS-LNAME            PIC X(10).
003400     03  FILLER              PIC X               VALUE SPACES.
003500     03  WS-PRATE            PIC Z,ZZZ,ZZ9.99.
003600     03  FILLER              PIC X               VALUE SPACES.
003700     03  WS-YTD              PIC Z,ZZZ,ZZ9.99.
003800 01  WS-EOF-FLAG             PIC X               VALUE "N".
003900     88  WS-EOF                                  VALUE "Y".
004000 01  WS-RESP                 PIC X.
004100*-----------------------------------------------------------------
004200 PROCEDURE DIVISION.
004300 100-MAIN.
004400     OPEN INPUT  EMP-MST-IN.
004500     READ EMP-MST-IN AT END MOVE "Y" TO WS-EOF-FLAG.
004600     PERFORM 200-MAIN-LOOP UNTIL WS-EOF.
004700     DISPLAY "PROGRAM END".
004800     ACCEPT  WS-RESP.
004900     STOP RUN.
005000*-----------------------------------------------------------------
005100 200-MAIN-LOOP.
005200     MOVE    EMP-ID-IN    TO WS-ID.
005300     MOVE    EMP-FNAME-IN TO WS-FNAME.
005400     MOVE    EMP-LNAME-IN TO WS-LNAME.
005500     MOVE    EMP-PRATE-IN TO WS-PRATE.
005600     MOVE    EMP-YTD-IN   TO WS-YTD.
005700     DISPLAY WS-REC-OUT.
005800     READ    EMP-MST-IN    AT END MOVE "Y" TO WS-EOF-FLAG.
005900*-----------------------------------------------------------------
