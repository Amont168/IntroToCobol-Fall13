      ******************************************************************
      *    AUTHOR: DUSTYNE BROWN
      *    DATE: 10/24/13
      *    PROGRAM: PROJECT 2 - ADD STUDENT
      *    PURPOSE: PROVIDE A PROGRAM THAT ALLOWS AN INDIVIDUAL TO
      *        ENTER STUDENT INFORMATION, SAVING IT TO A FILE WITH
      *        THE TIME THAT THE STUDENT WAS ADDED.
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. DSB-STU-ADD.

      ******************************************************************
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT MST-OUT          ASSIGN "DSB-STU-TRANSACTIONS.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.

      ******************************************************************
       DATA DIVISION.

       FILE SECTION.

       FD  MST-OUT.
       01  REC-OUT.
           03  ID-OUT                  PIC X(5).
           03  FNAME-OUT               PIC X(20).
           03  MNAME-OUT               PIC X(20).
           03  LNAME-OUT               PIC X(20).
           03  STR-LOC-OUT             PIC X(25).
           03  CTY-LOC-OUT             PIC X(20).
           03  ST-LOC-OUT              PIC X(2).
           03  ZIP-LOC-OUT             PIC X(5).
           03  STR-PRM-OUT             PIC X(25).
           03  CTY-PRM-OUT             PIC X(20).
           03  ST-PRM-OUT              PIC X(2).
           03  ZIP-PRM-OUT             PIC X(5).
           03  CELL-AREA-OUT           PIC X(3).
           03  CELL-EXCH-OUT           PIC X(3).
           03  CELL-NUMB-OUT           PIC X(4).
           03  HOM-AREA-OUT            PIC X(3).
           03  HOM-EXCH-OUT            PIC X(3).
           03  HOM-NUMB-OUT            PIC X(4).
           03  EMG-AREA-OUT            PIC X(3).
           03  EMG-EXCH-OUT            PIC X(3).
           03  EMG-NUMB-OUT            PIC X(4).
           03  GENDER-OUT              PIC X.
           03  RES-OUT                 PIC X.
           03  ACT-OUT                 PIC X(2).
           03  MJR-CD-OUT              PIC X(4).
           03  BRTH-YR-OUT             PIC X(4).
           03  BRTH-MNTH-OUT           PIC X(2).
           03  BRTH-DY-OUT             PIC X(2).
           03  ADMT-YR-OUT             PIC X(4).
           03  ADMT-MNTH-OUT           PIC X(2).
           03  ADMT-DY-OUT             PIC X(2).
           03  NOTE-OUT                PIC X(75).
           03  TRANS-YR-OUT            PIC X(4).
           03  TRANS-MNTH-OUT          PIC X(2).
           03  TRANS-DY-OUT            PIC X(2).
           03  TRANS-HR-OUT            PIC X(2).
           03  TRANS-MIN-OUT           PIC X(2).

      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-DATE.
           03  WS-YEAR                 PIC X(2).
           03  WS-MONTH                PIC X(2).
           03  WS-DAY                  PIC X(2).

       01  WS-CUR.
           03 WS-CUR-DATE.
               05  WS-CUR-YEAR         PIC X(4).
               05  WS-CUR-MONTH        PIC X(2).
               05  WS-CUR-DAY          PIC X(2).
           03  WS-CUR-TIME.
               05  WS-CUR-HOUR         PIC X(2).
               05  WS-CUR-MIN          PIC X(2).
               05  FILLER              PIC X(4).

       01  WS-SAVE                     PIC X VALUE "Y".

       01  WS-MORE-FLAG                PIC X VALUE "Y".
           88  WS-MORE                       VALUE "N".
      *-----------------------------------------------------------------*
       SCREEN SECTION.

       01  SCRN-TITLE.
           03  BLANK SCREEN.
           03  LINE 01 COL 01  VALUE   "DSB-STU-ADD".
           03  LINE 01 COL 31  VALUE   "WATSAMATA UNIVERSITY".
           03  LINE 01 COL 73  PIC X(2)    FROM    WS-MONTH.
           03  LINE 01 COL 75  VALUE   "/".
           03  LINE 01 COL 76  PIC X(2)    FROM    WS-DAY.
           03  LINE 01 COL 78  VALUE   "/".
           03  LINE 01 COL 79  PIC X(2)    FROM    WS-YEAR.
           03  LINE 02 COL 35  VALUE   "ADD STUDENT".

       01  SCRN-ID.
           03  LINE 04 COL 06  VALUE   "ID".
           03  LINE 04 COL 09  PIC X(5)    TO  ID-OUT  AUTO.

       01  SCRN-NAME.
           03  SCRN-FNAME.
               05  LINE 06 COL 11  VALUE   "NAME".
               05  LINE 07 COL 13  VALUE   "FIRST".
               05  LINE 07 COL 21  PIC X(20)   TO  FNAME-OUT.
           03  SCRN-MNAME.
               05  LINE 08 COL 13  VALUE   "MIDDLE".
               05  LINE 08 COL 21  PIC X(20)   TO  MNAME-OUT.
           03 SCRN-LNAME.
               05  LINE 09 COL 13  VALUE   "LAST".
               05  LINE 09 COL 21  PIC X(20)   TO  LNAME-OUT.

       01  SCRN-LOC.
           03  SCRN-STR-LOC.
               05  LINE 11 COL 11  VALUE   "ADDR LOCAL".
               05  LINE 12 COL 13  VALUE    "STREET".
               05  LINE 12 COL 21 PIC X(25)    TO  STR-LOC-OUT.
           03  SCRN-CTY-LOC.
               05  LINE 13 COL 13  VALUE   "CITY".
               05  LINE 13 COL 21  PIC X(20)   TO  CTY-LOC-OUT.
           03  SCRN-ZPST-LOC.
               05  LINE 14 COL 13  VALUE   "ST ZIP".
               05  LINE 14 COL 21  PIC X(2)    TO  ST-LOC-OUT  AUTO.
               05  LINE 14 COL 24  PIC X(5)    TO  ZIP-LOC-OUT AUTO.

       01  SCRN-PRM.
           03  SCRN-STR-PRM.
               05  LINE 16 COL 11  VALUE   "ADDR PERM".
               05  LINE 17 COL 13  VALUE   "STREET".
               05  LINE 17 COL 21  PIC X(25)   TO  STR-PRM-OUT.
           03  SCRN-CTY-PRM.
               05  LINE 18 COL 13  VALUE   "CITY".
               05  LINE 18 COL 21  PIC X(20)   TO  CTY-PRM-OUT.
           03  SCRN-ZPST-PRM.
               05  LINE 19 COL 13  VALUE   "ST ZIP".
               05  LINE 19 COL 21  PIC X(2)    TO  ST-PRM-OUT  AUTO.
               05  LINE 19 COL 24  PIC X(5)    TO  ZIP-PRM-OUT AUTO.

       01  SCRN-PHN.
           03  LINE 07 COL 50  VALUE   "CELL".
           03  CA  LINE 07 COL 57  PIC X(3)    TO  CELL-AREA-OUT   AUTO.
           03  LINE 07 COL 60  VALUE   "-".
           03  CE  LINE 07 COL 61  PIC X(3)    TO  CELL-EXCH-OUT   AUTO
                                                   REQUIRED.
           03  LINE 07 COL 64  VALUE   "-".
           03  CN  LINE 07 COL 65  PIC X(4)    TO  CELL-NUMB-OUT   AUTO
                                                   REQUIRED.
           03  LINE 08 COL 50  VALUE   "HOME".
           03  HA  LINE 08 COL 57  PIC X(3)    TO  HOM-AREA-OUT    AUTO.
           03  LINE 08 COL 60  VALUE   "-".
           03  HE  LINE 08 COL 61  PIC X(3)    TO  HOM-EXCH-OUT    AUTO
                                                   REQUIRED.
           03  LINE 08 COL 64  VALUE   "-".
           03  HN  LINE 08 COL 65  PIC X(4)    TO  HOM-NUMB-OUT    AUTO
                                                   REQUIRED.
           03  LINE 09 COL 50  VALUE   "EMER".
           03  EA  LINE 09 COL 57  PIC X(3)    TO  EMG-AREA-OUT    AUTO.
           03  LINE 09 COL 60  VALUE   "-".
           03  EE  LINE 09 COL 61  PIC X(3)    TO  EMG-EXCH-OUT    AUTO
                                                   REQUIRED.
           03  LINE 09 COL 64  VALUE   "-".
           03  EN  LINE 09 COL 65   PIC X(4)   TO  EMG-NUMB-OUT    AUTO
                                                   REQUIRED.

       01  SCRN-GEN-INFO.
           03  LINE 11 COL 50  VALUE   "GENDER".
           03  LINE 11 COL 57  PIC X   TO  GENDER-OUT  AUTO.
           03  LINE 11 COL 63  VALUE   "M/F".
           03  LINE 12 COL 50  VALUE   "RES".
           03  LINE 12 COL 57  PIC X   TO  RES-OUT     AUTO.
           03  LINE 12 COL 64  VALUE   "A/O/I".
           03  LINE 13 COL 50  VALUE   "ACT".
           03  LINE 13 COL 57  PIC X(2)    TO  ACT-OUT     AUTO.
           03  LINE 14 COL 50  VALUE   "MAJ".
           03  LINE 14 COL 57  PIC X(4)    TO  MJR-CD-OUT  AUTO.

       01  SCRN-DATES.
           03  LINE 16 COL 50  VALUE   "BIRTH".
           03  LINE 16 COL 57  PIC X(2)    TO  BRTH-MNTH-OUT   AUTO.
           03  LINE 16 COL 59  VALUE   "/".
           03  LINE 16 COL 60  PIC X(2)    TO  BRTH-DY-OUT AUTO.
           03  LINE 16 COL 62  VALUE   "/".
           03  LINE 16 COL 63  PIC X(4)    TO  BRTH-YR-OUT AUTO.
           03  LINE 17 COL 50  VALUE   "ADMIT".
           03  LINE 17 COL 57  PIC X(2)    TO  ADMT-MNTH-OUT   AUTO.
           03  LINE 17 COL 59  VALUE   "/".
           03  LINE 17 COL 60  PIC X(2)    TO  ADMT-DY-OUT AUTO.
           03  LINE 17 COL 62  VALUE   "/".
           03  LINE 17 COL 63  PIC X(4)    TO  ADMT-YR-OUT AUTO.

       01  SCRN-NOTE.
           03  LINE 21 COL 01  VALUE   "NOTE:".
           03  LINE 21 COL 06  PIC X(75)   TO  NOTE-OUT.

       01  SCRN-CNTRL.
           03  SCRN-SAVE.
               05  LINE 23 COL 35  PIC X   TO  WS-SAVE AUTO.
               05  LINE 23 COL 37  VALUE   "SAVE? (Y/N)".
           03 SCRN-SAVED LINE 23 COL 34    VALUE
                                           "RECORD ADDED  ".
           03 SCRN-NOT-SAVED LINE 23 COL 32    VALUE
                                               "RECORD NOT ADDED".
           03  SCRN-MORE.
               05  LINE 24 COL 30  PIC X   TO  WS-MORE-FLAG    AUTO.
               05  LINE 24 COL 32  VALUE   "ENTER ANOTHER? (Y/N)".


       01  SCRN-PROG-TERM.
           03  BLANK SCREEN.
           03  LINE 12 COL 34  VALUE   "PROGRAM ENDED".


      ******************************************************************
       PROCEDURE DIVISION.

       100-MAIN.
           ACCEPT WS-DATE FROM DATE.
           OPEN OUTPUT MST-OUT.
           PERFORM 200-DATA-ENTRY-LOOP UNTIL WS-MORE.
           CLOSE MST-OUT.
           DISPLAY SCRN-PROG-TERM.
           STOP RUN.
      *-----------------------------------------------------------------*
       200-DATA-ENTRY-LOOP.
           DISPLAY SCRN-TITLE.
           DISPLAY SCRN-ID.
           DISPLAY SCRN-NAME.
           DISPLAY SCRN-LOC.
           DISPLAY SCRN-PRM.
           DISPLAY SCRN-PHN.
           DISPLAY SCRN-GEN-INFO.
           DISPLAY SCRN-DATES.
           ACCEPT SCRN-ID.
           ACCEPT SCRN-FNAME.
           ACCEPT SCRN-MNAME.
           ACCEPT SCRN-LNAME.
           ACCEPT SCRN-STR-LOC.
           ACCEPT SCRN-CTY-LOC.
           ACCEPT SCRN-ZPST-LOC.
           ACCEPT SCRN-STR-PRM.
           ACCEPT SCRN-CTY-PRM.
           ACCEPT SCRN-ZPST-PRM.
           ACCEPT CA.
           ACCEPT CE.
           ACCEPT CN.
           ACCEPT HA.
           ACCEPT HE.
           ACCEPT HN.
           ACCEPT EA.
           ACCEPT EE.
           ACCEPT EN.
           ACCEPT SCRN-GEN-INFO.
           ACCEPT SCRN-DATES.
           DISPLAY SCRN-NOTE.
           ACCEPT SCRN-NOTE.
           DISPLAY SCRN-SAVE.
           ACCEPT SCRN-SAVE.
           IF WS-SAVE = "Y"
               ACCEPT WS-CUR-DATE FROM DATE YYYYMMDD
               ACCEPT WS-CUR-TIME FROM TIME
               MOVE WS-CUR-YEAR TO TRANS-YR-OUT
               MOVE WS-CUR-MONTH TO TRANS-MNTH-OUT
               MOVE WS-CUR-DAY TO TRANS-DY-OUT
               MOVE WS-CUR-HOUR TO TRANS-HR-OUT
               MOVE WS-CUR-MIN TO TRANS-MIN-OUT
               WRITE REC-OUT
               DISPLAY SCRN-SAVED
           ELSE
               DISPLAY SCRN-NOT-SAVED
           END-IF
           DISPLAY SCRN-MORE.
           ACCEPT SCRN-MORE.













           03  REC-LINE-ONE.
               05  PIC X(8) VALUE "DSB-LOAN".
               05  PIC X(24).
               05  PIC X(16) VALUE "ACME MORTAGE CO.".
               05  PIC X(24).
               05  REC-HOUR            PIC X(2).
               05  PIC X VALUE ":".
               05  REC-MIN             PIC X(2).
               05  PIC X(2).
               05  REC-MONTH           PIC X(2).
               05  PIC X VALUE "/".
               05  REC-DAY             PIC X(2).
               05  PIC X VALUE "/".
               05  REC-YEAR            PIC X(4).
           03  REC-LINE-TWO.
               05  PIC X(27).
               05  PIC X(26) VALUE "LOAN AMORTIZATION SCHEDULE".
               05  PIC X(27).

