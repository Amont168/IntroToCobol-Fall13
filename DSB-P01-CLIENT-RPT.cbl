      *******************************************************************
      * AUTHOR:  DUSTYNE BROWN                                          *
      * PROGRAM: PROJECT 01                                             *
      * PURPOSE: TO SOLVE THE PROBLEM PRESENTED IN EXCERSIZE 1 AT THE   *
      *            END OF CHAPTER 6 - READ IN A TEXT FILE, SPACE AND    *
      *            FORMAT IT FOR PRINTING.                              *
      * DATE:    10/3/13                                                *
      *******************************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID. DBROWNPROJ01.

      *-----------------------------------------------------------------*

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT MST-IN           ASSIGN "CH0601.TXT"
                                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MST-OUT          ASSIGN "DSB-client-report.txt"
                                   ORGANIZATION IS LINE SEQUENTIAL.

      *-----------------------------------------------------------------*

       DATA DIVISION.

       FILE SECTION.

       FD MST-IN.
       01 REC-IN.
           03  INITIAL1-IN         PIC X.
           03  INITIAL2-IN         PIC X.
           03  LNAME-IN            PIC X(10).
           03  MONTH-IN            PIC X(2).
           03  YEAR-IN             PIC X(4).
           03  TRANS-AMOUNT-IN     PIC 9(6).

       FD  MST-OUT.
       01  FILE-OUT                 PIC X(80).

       WORKING-STORAGE SECTION.

       01  HEADER.
           03  HDR-FLL1            PIC X(19).
           03  HDR                 PIC X(39)
                       VALUE "DEWEY, CHEATEM & HOWE, ATTORNEYS AT LAW".

       01  BLANK-LINE              PIC X.

       01  LABELS.
           03  LBL-FILL1           PIC X(16).
           03  NM-LBL              PIC X(6) VALUE "CLIENT".
           03  LBL-FILL2           PIC X(9).
           03  DATE-LBL            PIC X(17)
                                       VALUE "CONSULTATION DATE".
           03  LBL-FILL2           PIC X(1).
           03  TRANS-LBL           PIC X(13)
                                       VALUE "BILLED AMOUNT".
       01  DASHES.
           03  DSH-FILL1           PIC X(16).
           03  DSH1                PIC X(14)
                                       VALUE "--------------".
           03  DSH-FILL2           PIC X(1).
           03  DSH2                PIC X(17)
                                       VALUE "-----------------".
           03  DSH-FILL2           PIC X(1).
           03  DSH3                PIC X(13)
                                       VALUE "-------------".

       01 REC-OUT.
           03 FILLER1              PIC X(16).
           03 NAME-OUT.
               04  INITIAL1-OUT    PIC X.
               04  NAME-FILLER1    PIC X VALUE ".".
               04  INITIAL2-OUT    PIC X.
               04  NAME-FILLER2    PIC X VALUE ".".
               04  LNAME-OUT       PIC X(8).
           03 FILLER2              PIC X(8).
           03 DATE-OUT.
               04  MONTH-OUT       PIC X(2).
               04  DATE-DIVIDE     PIC X VALUE "/".
               04  YEAR-OUT        PIC X(4).
           03 FILLER3              PIC X(9).
           03 TRANS-AMOUNT-OUT     PIC $ZZZ,ZZ9.

       01  WS-EOF-FLAG             PIC X VALUE "N".
           88 WS-EOF                     VALUE "Y".
       01  WS-TOTAL                PIC 9(9) VALUE ZERO.
       01  WS-TOTAL-ROW.
           03 WS-SPACER            PIC X(49).
           03 WS-DISP-TOTAL        PIC $ZZ,ZZZ,ZZ9.
       01  WS-PROG-TERM.
           03  TERM-FILLER         PIC X(31).
           03  TERM-LBL            PIC X(18) VALUE "PROGRAM TERMINATED".
           03  TERM-FILLER2        PIC X(31).

      *-----------------------------------------------------------------*

       PROCEDURE DIVISION.

       100-MAIN.
           OPEN INPUT MST-IN.
           OPEN OUTPUT MST-OUT.
           MOVE HEADER TO FILE-OUT.
           WRITE FILE-OUT.
           MOVE BLANK-LINE TO FILE-OUT.
           WRITE FILE-OUT.
           MOVE LABELS TO FILE-OUT.
           WRITE FILE-OUT.
           MOVE DASHES TO FILE-OUT.
           WRITE FILE-OUT.
           READ MST-IN AT END MOVE "Y" TO WS-EOF-FLAG.
           PERFORM 200-READ-LOOP UNTIL WS-EOF.
           MOVE BLANK-LINE TO FILE-OUT.
           WRITE FILE-OUT
           MOVE WS-TOTAL TO WS-DISP-TOTAL.
           MOVE WS-TOTAL-ROW TO FILE-OUT.
           WRITE FILE-OUT.
           CLOSE MST-IN.
           CLOSE MST-OUT.
           DISPLAY WS-PROG-TERM.
           STOP RUN.

       200-READ-LOOP.
           MOVE INITIAL1-IN TO INITIAL1-OUT.
           MOVE INITIAL2-IN TO INITIAL2-OUT.
           MOVE LNAME-IN TO LNAME-OUT.
           MOVE MONTH-IN TO MONTH-OUT.
           MOVE YEAR-IN TO YEAR-OUT.
           ADD TRANS-AMOUNT-IN TO WS-TOTAL.
           MOVE TRANS-AMOUNT-IN TO TRANS-AMOUNT-OUT.
           MOVE REC-OUT TO FILE-OUT.
           WRITE FILE-OUT.
           READ MST-IN AT END MOVE "Y" TO WS-EOF-FLAG.
