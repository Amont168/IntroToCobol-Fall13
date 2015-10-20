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

       FILE SECTION.

           SELECT REC-IN           ASSIGN "CH0601.TXT"
                                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REC-OUT          ASSIGN "DBROWN-OUTPUT.TXT"
                                   ORGANIZATION IS LINE SEQUENTIAL.

      *-----------------------------------------------------------------*

       DATA DIVISION.

       INPUT-OUTPUT SECTION.

       FD REC-IN.
       01 REC-IN.
           03  INITIAL1-IN         PIC X.
           03  INITIAL2-IN         PIC X.
           03  LNAME-IN            PIC X(10).
           03  MONTH-IN            PIC X(2).
           03  YEAR-IN             PIC X(4).
           03  TRANS-AMOUNT-IN     PIC 9(6).

       FD REC-OUT.
       01 REC-OUT.
           03 FILLER1              PIC X(1).
           03 NAME-OUT.
               04  INITIAL1-OUT     PIC X.
               04  NAME-FILLER1     PIC X VALUE ".".
               04  INITIAL2-OUT     PIC X.
               04  NAME-FILLER2     PIC X VALUE ".".
               04  LNAME-OUT        PIC X(10).
           03 FILLER2               PIC X(14).
           03 DATE-OUT.
               04  MONTH-OUT        PIC X(2).
               04  DATE-DIVIDE      PIC X VALUE "/".
               04  YEAR-OUT         PIC X(4).
           03 FILLER3               PIC X(20).
           03 TRANS-AMOUNT-OUT      PIC $ZZZ,ZZ9.

       WORKING-STORAGE SECTION.
       01  LABELS.
           03  LBL-FILL1            PIC X(5).
           03  NM-LBL               PIC X(4) VALUE "NAME".
           03  LBL-FILL2            PIC X(14).
           03  DATE-LBL             PIC X(19)
                                       VALUE "DATE OF TRANSACTION".
           03  LBL-FILL2            PIC X(8).
           03  TRANS-LBL            PIC X(21)
                                       VALUE "AMOUNT OF TRANSACTION."
       01  DASHES.
           03  DSH-FILL1            PIC X(1).
           03  DSH1                 PIC X(14)
                                       VALUE "--------------"
           03  DSH-FILL2            PIC X(8).
           03  DSH2                 PIC X(19)
                                       VALUE "-------------------".
           03  DSH-FILL2            PIC X(8).
           03  DSH3                 PIC X(21)
                                       VALUE "---------------------".

       01  WS-EOF-FLAG              PIC X VALUE "N".
           WS-EOF                         VALUE "Y".
       01  WS-COUNTER               PIX 9(4) VALUE ZERO.

      *-----------------------------------------------------------------*

       PROCEDURE DIVISION.

       100-MAIN.
           OPEN REC-IN
                REC-OUT.
           DISPLAY BLANK.
           WRITE BLANK.
     **    READ REC-IN AT END MOVE "Y" TO WS-EOF-FLAG.
           CLOSE REC-IN
                 REC-OUT.
           STOP RUN.
