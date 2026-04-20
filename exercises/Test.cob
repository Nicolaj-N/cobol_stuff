       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           ALPHABET EUROPEAN-EXTENDED IS STANDARD-1.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  START-BALANCE           PIC 99999999999V99.
       01  WS-FOUND-INDEX           PIC 999 VALUE 0.
       01  WS-I                     PIC 999 VALUE 0.
       01  IX                      PIC 9999 VALUE 1.
       01  IX2                     PIC 99999 VALUE 1.
       01  IX3                     PIC 99999 VALUE 1.
       01  BLANKSPACE              PIC X(40) VALUE SPACES.
       01  ANTAL-BANK              PIC 999 VALUE ZEROES.
       01  ANTAL-KUNDER            PIC 999 VALUE ZEROES.
       01  CURRENT-CHAR            PIC X(1).
       01  PREVIOUS-CHAR           PIC X(1) VALUE SPACE.
       01  CUR-REG                 PIC 9999 VALUE ZEROES.
       01  CUR-BALANCE             PIC S99999999999V99 VALUE 50000.
       01  CUR-VAL-DKK             PIC 99999999999V99.
       01  EOF-BANK                PIC X VALUE "N".
       01  EOF-MAIN                PIC X VALUE "N".
       01  WS-BALANCE-DISPLAY.
           03  BALANCE-DISPLAY PIC -ZZZ,ZZ9.99.
       01  WS-DATE-STRING          PIC X(10).
       01  WS-TIME-STRING          PIC X(8).
       
       PROCEDURE DIVISION.
           PERFORM MAIN-LOGIC.
           STOP RUN.
       MAIN-LOGIC.
           COMPUTE CUR-BALANCE =
               (CUR-BALANCE - 60000) * 2.35
           DISPLAY CUR-BALANCE
           STOP RUN.
