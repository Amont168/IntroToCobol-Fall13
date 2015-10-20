      *******************************************************************
      *    AUTHOR: DUSTYNE BROWN                                        *
      *    PROGRAM: MID-TERM-2                                          *
      *    DATE: 10/9/2013                                              *
      *******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. DB-MID-TERM-2.

      *******************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT MST-IN               ASSIGN "MT-MST-CURRENT-2.TXT"
                                       ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MST-OUT              ASSIGN "DSB-MST-NEW-2.TXT"
                                       ORGANIZATION IS LINE SEQUENTIAL.

      *******************************************************************

       DATA DIVISION.

       FILE SECTION.

       FD  MST-IN.
       01  REC-IN.
           03  ACC-NUM-IN              PIC X(5).
           03  FNAME-IN                PIC X(10).
           03  LNAME-IN                PIC X(10).
           03  LST-TRN-YR-IN           PIC 9(4).
           03  LST-TRN-MNTH-IN         PIC 9(2).
           03  LST-TRN-DY-IN           PIC 9(2).
           03  LST-TRN-AMNT-IN         PIC 9(6)V99.
           03  ACC-BLNC-IN             PIC 9(6)V99.

       FD  MST-OUT.
       01  REC-OUT.
           03  ACC-NUM-OUT             PIC X(5).
           03  FNAME-OUT               PIC X(10).
           03  LNAME-OUT               PIC X(10).
           03  LST-TRN-YR-OUT          PIC 9(4).
           03  LST-TRN-MNTH-OUT        PIC 9(2).
           03  LST-TRN-DY-OUT          PIC 9(2).
           03  LST-TRN-AMNT-OUT        PIC 9(6)V99.
           03  ACC-BLNC-OUT            PIC 9(6)V99.

      *******************************************************************

       WORKING-STORAGE SECTION.

       01  REC-DISP.
           03  ACC-NUM-DISP            PIC X(5).
           03  FILL1                   PIC X.
           03  FNAME-DISP              PIC X(10).
           03  FILL2                   PIC X.
           03  LNAME-DISP              PIC X(10).
           03  FILL3                   PIC X.
           03  LST-TRN-MNTH-DISP       PIC 9(2).
           03  FILL4                   PIC X VALUE "/".
           03  LST-TRN-DY-DISP         PIC 9(2).
           03  FILL5                   PIC X VALUE "/".
           03  LST-TRN-YR-DISP         PIC 9(4).
           03  FILL6                   PIC X.
           03  LST-TRN-AMNT-DISP       PIC ZZZ,ZZ9.99.
           03  FILL7                   PIC X.
           03  ACC-BLNC-DISP           PIC ZZZ,ZZ9.99.
           03  FILL8                   PIC X.
           03  ACC-BLNC-TOT-DISP       PIC ZZZ,ZZ9.99.

       01  WS-ACC-BLNC-TOT                PIC 9(6)V99.

       01  WS-EOF-FLAG                 PIC X VALUE "N".
           88 WS-EOF                         VALUE "Y".

       01  BLANK-LINE                  PIC X.

       01  CNTR                        PIC 9999 VALUE ZERO.

      *******************************************************************

       PROCEDURE DIVISION.

       100-MAIN.
           OPEN INPUT MST-IN.
           OPEN OUTPUT MST-OUT.
           READ MST-IN AT END MOVE "Y" TO WS-EOF-FLAG.
           PERFORM 200-READ-LOOP UNTIL WS-EOF.
           DISPLAY BLANK-LINE.
           DISPLAY "RECORDS PROCESSED = ", CNTR.
           DISPLAY "PROGRAM END".
           CLOSE MST-IN.
           CLOSE MST-OUT.
           STOP RUN.

      *******************************************************************

       200-READ-LOOP.
           MOVE ACC-NUM-IN TO ACC-NUM-DISP.
           MOVE FNAME-IN TO FNAME-DISP.
           MOVE LNAME-IN TO LNAME-DISP.
           MOVE LST-TRN-MNTH-IN TO LST-TRN-MNTH-DISP.
           MOVE LST-TRN-DY-IN TO LST-TRN-DY-DISP.
           MOVE LST-TRN-YR-IN TO LST-TRN-YR-DISP.
           MOVE LST-TRN-AMNT-IN TO LST-TRN-AMNT-DISP.
           MOVE ACC-BLNC-IN TO ACC-BLNC-DISP.
           ADD LST-TRN-AMNT-IN TO ACC-BLNC-IN GIVING ACC-BLNC-TOT-DISP.
           DISPLAY REC-DISP.

           MOVE ACC-NUM-DISP TO ACC-NUM-OUT.
           MOVE FNAME-DISP TO FNAME-OUT.
           MOVE LNAME-DISP TO LNAME-OUT.
           MOVE LST-TRN-MNTH-DISP TO LST-TRN-MNTH-OUT.
           MOVE LST-TRN-DY-DISP TO LST-TRN-DY-OUT.
           MOVE LST-TRN-YR-DISP TO LST-TRN-YR-OUT.
           MOVE ZERO TO LST-TRN-AMNT-OUT.
           MOVE ACC-BLNC-TOT-DISP TO ACC-BLNC-OUT.

           WRITE REC-OUT.

           ADD 1 TO CNTR.

           READ MST-IN AT END MOVE "Y" TO WS-EOF-FLAG.


