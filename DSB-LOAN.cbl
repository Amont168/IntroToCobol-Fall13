      *******************************************************************
      *    PROGRAM-ID: DSB_PROJ_3_LOAN                                  *
      *    AUTHOR: DUSTYNE BROWN                                        *
      *    DATE: 11/05/13 & 11/07/13                                    *
      *    PURPOSE: CREATE DISPLAY SCREEN THEN AMORT TABLE, WRITE IT TO *
      *        A FILE.                                                  *
      *******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. DSB-LOAN.

      *******************************************************************
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT MST-OUT              ASSIGN "DSB-PROJ-3-TBL.TXT"
                                   ORGANIZATION IS LINE SEQUENTIAL.

      *******************************************************************
       DATA DIVISION.

       FILE SECTION.

       FD  MST-OUT.
       01  REC-OUT                         PIC X(80).


      *-----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01  WS-TITLE-PRINT.
           03  WS-PRINTER-L1.
               05                      PIC X(50) VALUE
                   "--------------------------------------------------".
               05                      PIC X(30) VALUE
                                       "------------------------------".
           03  WS-PRINTER-L2.
               05                      PIC X(8)
                                       VALUE "DSB-LOAN".
               05                      PIC X(24).
               05                      PIC X(16)
                                       VALUE "ACME MORTAGE CO.".
               05                      PIC X(15).
               05  WS-HOUR-L2          PIC X(2).
               05                      PIC X VALUE ":".
               05  WS-MIN-L2           PIC X(2).
               05                      PIC X(2).
               05  WS-MONTH-L2         PIC X(2).
               05                      PIC X VALUE "/".
               05  WS-DAY-L2           PIC X(2).
               05                      PIC X VALUE "/".
               05  WS-YEAR-L2          PIC X(4).
           03  WS-PRINTER-L3.
               05                      PIC X(27).
               05                      PIC X(26)
                                     VALUE "LOAN AMORTIZATION SCHEDULE".
               05                      PIC X(27).
           03  WS-PRINTER-L4           PIC X(80).
           03  WS-PRINTER-L5.
               05                      PIC X(7)
                                        VALUE " PRIN: ".
               05  WS-PRINTER-PRINC    PIC Z,ZZZ,ZZ9.99.
           03  WS-PRINTER-L6.
               05                      PIC X(13)
                                       VALUE " INT :       ".
               05  WS-PRINTER-INT      PIC Z9.999.
           03  WS-PRINTER-L7.
               05                      PIC X(16)
                                       VALUE " TERM:          ".
               05  WS-PRINTER-TERM     PIC ZZ9.
           03  WS-PRINTER-L8           PIC X(80).
           03  WS-PRINTER-L9.
               05                      PIC X.
               05                      PIC X(3)
                                       VALUE "PMT".
               05                      PIC X.
               05                      PIC X(4)
                                       VALUE "YEAR".
               05                      PIC X.
               05                      PIC X(2)
                                       VALUE "MO".
               05                      PIC X(4).
               05                      PIC X(7)
                                       VALUE "BEG BAL".
               05                      PIC X(9).
               05                      PIC X(3)
                                       VALUE "PMT".
               05                      PIC X(9).
               05                      PIC X(8)
                                       VALUE "INT PAID".
               05                      PIC X(4).
               05                      PIC X(9)
                                       VALUE "PRIN APPL".
               05                      PIC X(4).
               05                      PIC X(7)
                                       VALUE "END BAL".
               05                      PIC X(4).
           03  WS-PRINTER-L10.
               05                      PIC X.
               05                      PIC X(3)
                                       VALUE "---".
               05                      PIC X.
               05                      PIC X(4)
                                       VALUE "----".
               05                      PIC X.
               05                      PIC X(2)
                                       VALUE "--".
               05                      PIC X(2).
               05                      PIC X(11)
                                       VALUE "-----------".
               05                      PIC X(4).
               05                      PIC X(9)
                                       VALUE "---------".
               05                      PIC X(6).
               05                      PIC X(8)
                                       VALUE "--------".
               05                      PIC X(4).
               05                      PIC X(9)
                                       VALUE "---------".
               05                      PIC X(2).
               05                      PIC X(11)
                                       VALUE "-----------".
               05                      PIC X(2).
           03  WS-PRINTER-L11.
               05                      PIC X.
               05  WS-PRINTER-PMT      PIC ZZ9.
               05                      PIC X.
               05  WS-PRINTER-YR       PIC 9(4).
               05                      PIC X.
               05  WS-PRINTER-MO       PIC 99.
               05                      PIC X.
               05  WS-PRINTER-BEG      PIC Z,ZZZ,ZZ9.99.
               05                      PIC X.
               05  WS-PRINTER-PYMNT    PIC Z,ZZZ,ZZ9.99.
               05                      PIC X(2).
               05  WS-PRINTER-INT-PD   PIC Z,ZZZ,ZZ9.99.
               05                      PIC X.
               05  WS-PRINTER-PRN-AP   PIC Z,ZZZ,ZZ9.99.
               05                      PIC X.
               05  WS-PRINTER-END      PIC Z,ZZZ,ZZ9.99.
           03  WS-PRINTER-L12.
               05                      PIC X(16).
               05                      PIC X(10)
                                       VALUE "YR TOTAL  ".
               05  WS-PRINTER-T-PMT    PIC Z,ZZZ,ZZ9.99.
               05                      PIC X.
               05  WS-PRINTER-T-INT    PIC Z,ZZZ,ZZ9.99.
               05                      PIC X.
               05  WS-PRINTER-T-PRN    PIC Z,ZZZ,ZZ9.99.
           03  WS-PRINTER-L13.
               05                      PIC X(29).
               05                      PIC X(9)
                                       VALUE "---------".
               05                      PIC X(6).
               05                      PIC X(8)
                                       VALUE "--------".
               05                      PIC X(4).
               05                      PIC X(9)
                                       VALUE "---------".
           03  WS-PRINTER-L14.
               05                      PIC X(16).
               05                      PIC X(10)
                                       VALUE "GR TOTAL  ".
               05  WS-PRINTER-GT-PMT   PIC Z,ZZZ,ZZ9.99.
               05                      PIC X.
               05  WS-PRINTER-GT-INT   PIC Z,ZZZ,ZZ9.99.
               05                      PIC X.
               05  WS-PRINTER-GT-PRN   PIC Z,ZZZ,ZZ9.99.

      *-----------------------------------------------------------------*

       01  WS-TIME-LOG.
           03  WS-DATE.
               05  WS-YEAR             PIC X(4).
               05  WS-MONTH            PIC X(2).
               05  WS-DAY              PIC X(2).
           03  WS-TIME.
               05  WS-HOUR             PIC X(2).
               05  WS-MIN              PIC X(2).

       01  WS-DATA.
           03  WS-USER-INPUT.
               05  WS-INP-PRINC        PIC 9(7)V99.
               05  WS-INP-INT          PIC 99V999.
               05  WS-INP-TRM          PIC 999.
               05  WS-INP-MNTH         PIC 99.
               05  WS-INP-YR           PIC 9999.
               05  WS-INP-PAYMENT      PIC 9(7)V99.
           03  WS-CALCULATED-DATA.
               05  WS-PAYMENT          PIC Z,ZZZ,ZZ9.99.
               05  WS-RATE             PIC V99999.
               05  WS-TERM             PIC 999.
               05  WS-RUNNING-PRINC    PIC 9999999V99.
               05  WS-DASH-FLAG        PIC 9 VALUE ZERO.
               05  WS-PMT-CNTR         PIC 999 VALUE ZERO.
               05  WS-INTEREST         PIC 9(7)V99.
               05  WS-PRIN-APPL        PIC 9(7)V99.
               05  WS-END-BAL          PIC 9(7)V99.
           03  WS-CALC-TEMP-DATA.
               05  WS-HOLDER1          PIC 99999999V999999.
               05  WS-HOLDER2          PIC 9999V9999999999.
               05  WS-HOLDER3          PIC 999V99999999999.
               05  WS-HOLDER4          PIC 9999V9999999999.
           03  WS-RUNNING-TOTALS.
               05  WS-T-PAYMENT        PIC 9(7)V99 VALUE ZERO.
               05  WS-T-INT-PAID       PIC 9(7)V99 VALUE ZERO.
               05  WS-T-PRIN-APPL      PIC 9(7)V99 VALUE ZERO.
           03  WS-GRAND-TOTALS.
               05  WS-GT-PAYMENT       PIC 9(7)V99 VALUE ZERO.
               05  WS-GT-INT-PAID      PIC 9(7)V99 VALUE ZERO.
               05  WS-GT-PRIN-APPL     PIC 9(7)V99 VALUE ZERO.

       01  WS-PRINT-FLAG               PIC X.

       01  WS-CONTINUE-FLAG            PIC X.

      *-----------------------------------------------------------------*
       SCREEN SECTION.

       01  SCRN-TITLE.
           03  SCRN-LINE-ONE.
               05  BLANK SCREEN.
               05  LINE 01 COL 01 VALUE "DSB-LOAN".
               05  LINE 01 COL 33 VALUE "ACME MORTGAGE CO.".
               05  LINE 01 COL 71 PIC X(2) FROM WS-MONTH.
               05  LINE 01 COL 73 VALUE "/".
               05  LINE 01 COL 74 PIC X(2) FROM WS-DAY.
               05  LINE 01 COL 76 VALUE "/".
               05  LINE 01 COL 77 PIC X(4) FROM WS-YEAR.
           03  SCRN-LINE-TWO.
               05  LINE 02 COL 76 PIC X(2) FROM WS-HOUR.
               05  LINE 02 COL 78 VALUE ":".
               05  LINE 02 COL 79 PIC X(2) FROM WS-MIN.
           03  SCRN-LINE-THREE.
               05  LINE 03 COL 32 VALUE "PAYMENT CALCULATION".

       01  SCRN-INPUT-FIELDS.
           03  SCRN-PRINCIPAL.
               05  LINE 07 COL 30 VALUE "PRINCIPAL".
               05  LINE 07 COL 40 PIC Z,ZZZ,ZZ9.99 TO WS-INP-PRINC.
           03  SCRN-ANNL-INT.
               05  LINE 08 COL 30 VALUE "ANNL INT".
               05  LINE 08 COL 46 PIC Z9.999 TO WS-INP-INT.
               05  LINE 08 COL 52 VALUE "%".
           03  SCRN-TERM.
               05  LINE 09 COL 30 VALUE "TERM (MO)".
               05  LINE 09 COL 49 PIC ZZZ TO WS-INP-TRM.
           03  SCRN-STRT-MONTH.
               05  LINE 10 COL 30 VALUE "BEG MO".
               05  LINE 10 COL 50 PIC ZZ TO WS-INP-MNTH REQUIRED FULL.
           03  SCRN-STRT-YEAR.
               05  LINE 11 COL 30 VALUE "BEG YR".
               05  LINE 11 COL 48 PIC Z(4) TO WS-INP-YR REQUIRED FULL.

       01  SCRN-BOTTOM.
           03  SCRN-PYMNT.
               05  LINE 13 COL 30 VALUE "PAYMENT".
               05  LINE 13 COL 40 PIC Z,ZZZ,ZZ9.99 FROM WS-PAYMENT.
           03  SCRN-SCHEDULE.
               05  LINE 16 COL 29 PIC X TO WS-PRINT-FLAG AUTO.
               05  LINE 16 COL 31 VALUE "PRINT SCHEDULE? (Y/N)".
           03  SCRN-PRINTED.
               05  LINE 16 COL 29 VALUE " PRINT FILE GENERATED  ".
           03  SCRN-NOT-PRINTED.
               05  LINE 16 COL 29 VALUE "                       ".
           03  SCRN-CONTINUE.
               05  LINE 18 COL 28 PIC X TO WS-CONTINUE-FLAG AUTO.
               05  LINE 18 COL 30 VALUE "CALCUALTE ANOTHER? (Y/N)".

       01  SCRN-PROG-TERM.
           03  BLANK SCREEN.
           03  LINE 12 COL 34 VALUE "PROGRAM TERMINATED".

       01  SCRN-BLANK.
           03  BLANK SCREEN.
      *******************************************************************
       PROCEDURE DIVISION.

       100-MAIN.
           OPEN OUTPUT MST-OUT.
           ACCEPT WS-DATE FROM DATE YYYYMMDD.
           ACCEPT WS-TIME FROM TIME.
           PERFORM 200-INPUT-LOOP UNTIL WS-CONTINUE-FLAG EQUALS 'N' OR
           'n'.
           DISPLAY SCRN-PROG-TERM.
           STOP RUN.

      *-----------------------------------------------------------------*

       200-INPUT-LOOP.
           DISPLAY SCRN-TITLE.
           DISPLAY SCRN-INPUT-FIELDS.
           DISPLAY SCRN-PYMNT.

           ACCEPT SCRN-PRINCIPAL.
           ACCEPT SCRN-ANNL-INT.
           ACCEPT SCRN-TERM.
           ACCEPT SCRN-STRT-MONTH.
           ACCEPT SCRN-STRT-YEAR.

           PERFORM 300-PAYMENT-CALC.

           DISPLAY SCRN-SCHEDULE.

           MOVE '0' TO WS-PRINT-FLAG.

           PERFORM UNTIL WS-PRINT-FLAG EQUALS 'Y' OR 'y' OR 'N' OR 'n'
               ACCEPT SCRN-SCHEDULE
           END-PERFORM.

           IF WS-PRINT-FLAG = 'y' OR 'Y'
               PERFORM 400-PRINT-LOOP UNTIL WS-PRINT-FLAG
                   EQUALS 'N' OR 'n'

               MOVE 0 TO WS-PMT-CNTR
               MOVE WS-T-PAYMENT TO WS-PRINTER-T-PMT
               MOVE WS-T-INT-PAID TO WS-PRINTER-T-INT
               MOVE WS-T-PRIN-APPL TO WS-PRINTER-T-PRN
               MOVE WS-GT-PAYMENT TO WS-PRINTER-GT-PMT
               MOVE WS-GT-INT-PAID TO WS-PRINTER-GT-INT
               MOVE WS-GT-PRIN-APPL TO WS-PRINTER-GT-PRN

               WRITE REC-OUT FROM WS-PRINTER-L13
               WRITE REC-OUT FROM WS-PRINTER-L12
               WRITE REC-OUT FROM WS-PRINTER-L4
               WRITE REC-OUT FROM WS-PRINTER-L14
               WRITE REC-OUT FROM WS-PRINTER-L1

               DISPLAY SCRN-PRINTED

           ELSE
               DISPLAY SCRN-NOT-PRINTED
           END-IF.

           DISPLAY SCRN-CONTINUE.

           MOVE '0' TO WS-CONTINUE-FLAG.

           PERFORM UNTIL WS-CONTINUE-FLAG EQUALS 'Y' OR 'y' OR 'N' OR
           'n'
               ACCEPT SCRN-CONTINUE
           END-PERFORM.

           MOVE '1' TO WS-DASH-FLAG.

      *-----------------------------------------------------------------*

       300-PAYMENT-CALC.
           COMPUTE WS-RATE ROUNDED = WS-INP-INT / 100.
           COMPUTE WS-HOLDER1 ROUNDED = WS-INP-PRINC * WS-RATE.
           COMPUTE WS-HOLDER2 ROUNDED = WS-RATE / 12.
           COMPUTE WS-HOLDER2 ROUNDED = WS-HOLDER2 + 1.
           COMPUTE WS-HOLDER3 ROUNDED = WS-HOLDER2 ** (WS-INP-TRM * -1).
           COMPUTE WS-HOLDER3 = 1 - WS-HOLDER3.
           COMPUTE WS-HOLDER3 ROUNDED = 12 * WS-HOLDER3.
           COMPUTE WS-INP-PAYMENT ROUNDED = WS-HOLDER1 / WS-HOLDER3.

           MOVE WS-INP-PAYMENT TO WS-PAYMENT.

           DISPLAY SCRN-PYMNT.

      *-----------------------------------------------------------------*

       400-PRINT-LOOP.
           IF WS-DASH-FLAG = 0
               MOVE WS-MONTH TO WS-MONTH-L2
               MOVE WS-DAY TO WS-DAY-L2
               MOVE WS-YEAR TO WS-YEAR-L2
               MOVE WS-MIN TO WS-MIN-L2
               MOVE WS-HOUR TO WS-HOUR-L2

               WRITE REC-OUT FROM WS-PRINTER-L1
               WRITE REC-OUT FROM WS-PRINTER-L2
               WRITE REC-OUT FROM WS-PRINTER-L3
               WRITE REC-OUT FROM WS-PRINTER-L4

               ADD 1 TO WS-DASH-FLAG
           END-IF.

           IF  WS-DASH-FLAG = 1
               MOVE WS-INP-PRINC TO WS-PRINTER-PRINC
               MOVE WS-INP-INT TO WS-PRINTER-INT
               MOVE WS-INP-TRM TO WS-PRINTER-TERM

               WRITE REC-OUT FROM WS-PRINTER-L5
               WRITE REC-OUT FROM WS-PRINTER-L6
               WRITE REC-OUT FROM WS-PRINTER-L7
               WRITE REC-OUT FROM WS-PRINTER-L8
               WRITE REC-OUT FROM WS-PRINTER-L9
               WRITE REC-OUT FROM WS-PRINTER-L10

               ADD 1 TO WS-DASH-FLAG

               MOVE WS-INP-PRINC TO WS-RUNNING-PRINC
           END-IF.


           IF WS-INP-MNTH > 12
               ADD 1 TO WS-INP-YR

               MOVE 1 TO WS-INP-MNTH
               MOVE WS-T-PAYMENT TO WS-PRINTER-T-PMT
               MOVE WS-T-INT-PAID TO WS-PRINTER-T-INT
               MOVE WS-T-PRIN-APPL TO WS-PRINTER-T-PRN

               WRITE REC-OUT FROM WS-PRINTER-L13
               WRITE REC-OUT FROM WS-PRINTER-L12
               WRITE REC-OUT FROM WS-PRINTER-L4

               COMPUTE WS-GT-PAYMENT = WS-GT-PAYMENT + WS-T-PAYMENT
               COMPUTE WS-GT-INT-PAID = WS-GT-INT-PAID + WS-T-INT-PAID
               COMPUTE WS-GT-PRIN-APPL = WS-GT-PRIN-APPL +
                                                         WS-T-PRIN-APPL

               MOVE 0 TO WS-T-PAYMENT
               MOVE 0 TO WS-T-INT-PAID
               MOVE 0 TO WS-T-PRIN-APPL

           ELSE
               COMPUTE WS-INTEREST ROUNDED = WS-RUNNING-PRINC *
                                           ((WS-INP-INT/100)/12)

               IF WS-RUNNING-PRINC + WS-INTEREST >= WS-INP-PAYMENT
                   ADD 1 TO WS-PMT-CNTR

                   COMPUTE WS-PRIN-APPL ROUNDED = WS-INP-PAYMENT -
                                                   WS-INTEREST
                   COMPUTE WS-END-BAL ROUNDED = WS-RUNNING-PRINC -
                                                      WS-PRIN-APPL
                   COMPUTE WS-T-PAYMENT = WS-INP-PAYMENT + WS-T-PAYMENT
                   COMPUTE WS-T-INT-PAID = WS-T-INT-PAID + WS-INTEREST
                   COMPUTE WS-T-PRIN-APPL = WS-T-PRIN-APPL +
                                            WS-PRIN-APPL

                   MOVE WS-PMT-CNTR TO WS-PRINTER-PMT
                   MOVE WS-INP-YR TO WS-PRINTER-YR
                   MOVE WS-INP-MNTH TO WS-PRINTER-MO
                   MOVE WS-RUNNING-PRINC TO WS-PRINTER-BEG
                   MOVE WS-INP-PAYMENT TO WS-PRINTER-PYMNT
                   MOVE WS-INTEREST TO WS-PRINTER-INT-PD
                   MOVE WS-PRIN-APPL TO WS-PRINTER-PRN-AP
                   MOVE WS-END-BAL TO WS-PRINTER-END
                   MOVE WS-END-BAL TO WS-RUNNING-PRINC

                   WRITE REC-OUT FROM WS-PRINTER-L11

                   ADD 1 TO WS-INP-MNTH
               ELSE

               COMPUTE WS-INP-PAYMENT = WS-RUNNING-PRINC + WS-INTEREST

               END-IF
           END-IF.

           IF WS-RUNNING-PRINC = 0
               MOVE 'N' TO WS-PRINT-FLAG
           END-IF.
