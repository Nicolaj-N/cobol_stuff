       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           ALPHABET EUROPEAN-EXTENDED IS STANDARD-1.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSAKTIONER ASSIGN TO "Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-TRANSAKTIONER ASSIGN TO "WRK.tmp".
           SELECT SORTED-TRANSAKTIONER ASSIGN TO "SortedTrans.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "Kontoudskrift.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSAKTIONER.
           01  TRANSAKTIONER-IN.
               COPY "TRANSAKTIONER.cpy".

       SD SORT-TRANSAKTIONER.
           01  SORT-REC.
           COPY "TRANSAKTIONER.cpy".

       FD  SORTED-TRANSAKTIONER.
           01 SORTED-REC.
           COPY "TRANSAKTIONER.cpy".

       FD  OUTPUT-FILE.
           01  OUTPUT-RECORD.
               02  NAVN-ADR            PIC X(300) VALUE SPACES.

       WORKING-STORAGE SECTION.
       01  PREVIOUS-REC.
           COPY "TRANSAKTIONER.cpy".
       01  TOP-3-ACCOUNTS OCCURS 3 TIMES.
           02 KUNDE-ID             PIC X(15) VALUE SPACES.
           02 KUNDE-NAVN           PIC X(30) VALUE SPACES.
           02 KUNDE-SALDO          PIC S99999999999V99 VALUE -9999999.
       01  MONTHLY-STATS OCCURS 12 TIMES.
           02 INDBETALINGDKK       PIC S99999999999V99 VALUE 0.
           02 UDBETALINGDKK        PIC S99999999999V99 VALUE 0.
       01  CUR-MONTH               PIC 99.             
       01  BUTIK-COUNTER.
           05 BUTIK-COUNT OCCURS 100 TIMES INDEXED BY IDX.
               10 BUTIK-KEY        PIC X(20).
               10 BUTIK-CNT        PIC 9(5) VALUE 0.
       01  CURRENT-CHAR            PIC X(1).
       01  PREVIOUS-CHAR           PIC X(1) VALUE SPACE.
       01  BELØB-NUM               PIC S99999999999V99.
       01  IX                      PIC S999999 VALUE 1.
       01  CUR-ID                  PIC X(15) VALUE SPACES.
       01  CUR-LINE-VAL-DKK        PIC S9(18)V99 VALUE ZEROES.
       01  CUR-ACC-SALDO           PIC S9(18)V99 VALUE ZEROES.
       01  CUR-BALANCE-DISPLAY     PIC +ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  CUR-VAL-DKK             PIC S9(18)V99 VALUE ZEROES.
       01  CUR-VAL-DKK-DISPLAY     PIC +ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  TOTAL-INDBETALT         PIC S9(18)V99 VALUE ZEROES.
       01  TOTAL-IND-DISPLAY       PIC +ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  TOTAL-UDBETALT          PIC S9(18)V99 VALUE ZEROES.
       01  TOTAL-UD-DISPLAY        PIC +ZZZ,ZZZ,ZZZ,ZZ9.99.
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
           SORT SORT-TRANSAKTIONER ON 
               ASCENDING KEY KONTO-ID OF SORT-REC
                           TIDSPUNKT OF SORT-REC
               USING TRANSAKTIONER GIVING SORTED-TRANSAKTIONER.
           
           OPEN INPUT SORTED-TRANSAKTIONER
           OPEN OUTPUT OUTPUT-FILE

           READ SORTED-TRANSAKTIONER
               AT END
                   MOVE "Y" TO EOF-MAIN
               NOT AT END
                   MOVE 50000 TO CUR-ACC-SALDO
                   PERFORM CONVERT-TO-DKK
                   ADD CUR-LINE-VAL-DKK TO CUR-ACC-SALDO
                   MOVE SORTED-REC TO PREVIOUS-REC
      *            PERFORM COUNT-BUTIK
           END-READ
           
           PERFORM UNTIL EOF-MAIN = "Y"
               READ SORTED-TRANSAKTIONER
               AT END
                   DISPLAY IX "    OCCURENCES OF    " 
                       KONTO-ID OF PREVIOUS-REC
                       " WITH A TOTAL BALANCE OF: "
                       CUR-ACC-SALDO
                   PERFORM HANDLE-TOP-3
                   DISPLAY TOP-3-ACCOUNTS(1)
                   DISPLAY TOP-3-ACCOUNTS(2)
                   DISPLAY TOP-3-ACCOUNTS(3)
                   MOVE "Y" TO EOF-MAIN
               NOT AT END
                   IF KONTO-ID OF SORTED-REC NOT
                           = KONTO-ID OF PREVIOUS-REC
                       DISPLAY IX "    OCCURENCES OF    " 
                           KONTO-ID OF PREVIOUS-REC 
                           " WITH A TOTAL BALANCE OF: "
                           CUR-ACC-SALDO
                       
                       PERFORM HANDLE-TOP-3

                       MOVE SORTED-REC TO PREVIOUS-REC
                       MOVE 50000 TO CUR-ACC-SALDO
                       DISPLAY "TEST BUH " CUR-ACC-SALDO
                       MOVE 1 TO IX
                   ELSE
                       ADD 1 TO IX
                   END-IF

                   PERFORM CONVERT-TO-DKK
                   PERFORM HANDLE-MONTHS
      *            PERFORM COUNT-BUTIK
                   ADD CUR-LINE-VAL-DKK TO CUR-ACC-SALDO
               END-READ
           END-PERFORM
           DISPLAY TIDSPUNKT OF SORTED-REC (6:2)
           DISPLAY CUR-MONTH
           DISPLAY "JAN " INDBETALINGDKK OF MONTHLY-STATS(1) "        "
               UDBETALINGDKK OF MONTHLY-STATS(1)
           DISPLAY "FEB " INDBETALINGDKK OF MONTHLY-STATS(2) "        "
               UDBETALINGDKK OF MONTHLY-STATS(2)
           DISPLAY "MAR " INDBETALINGDKK OF MONTHLY-STATS(3) "        "
               UDBETALINGDKK OF MONTHLY-STATS(3)
           DISPLAY "APR " INDBETALINGDKK OF MONTHLY-STATS(4) "        "
               UDBETALINGDKK OF MONTHLY-STATS(4)
           DISPLAY "MAJ " INDBETALINGDKK OF MONTHLY-STATS(5) "        "
               UDBETALINGDKK OF MONTHLY-STATS(5)
           DISPLAY "JUN " INDBETALINGDKK OF MONTHLY-STATS(6) "        "
               UDBETALINGDKK OF MONTHLY-STATS(6)
           DISPLAY "JUL " INDBETALINGDKK OF MONTHLY-STATS(7) "        "
               UDBETALINGDKK OF MONTHLY-STATS(7)
           DISPLAY "AUG " INDBETALINGDKK OF MONTHLY-STATS(8) "        "
               UDBETALINGDKK OF MONTHLY-STATS(8)
           DISPLAY "SEP " INDBETALINGDKK OF MONTHLY-STATS(9) "        "
               UDBETALINGDKK OF MONTHLY-STATS(9)
           DISPLAY "OKT " INDBETALINGDKK OF MONTHLY-STATS(10) "        "
               UDBETALINGDKK OF MONTHLY-STATS(10)
           DISPLAY "NOV " INDBETALINGDKK OF MONTHLY-STATS(11) "        "
               UDBETALINGDKK OF MONTHLY-STATS(11)
           DISPLAY "DEC " INDBETALINGDKK OF MONTHLY-STATS(12) "        "
               UDBETALINGDKK OF MONTHLY-STATS(12)
      *    PERFORM DISPLAY-BUTIK-COUNTS
           CLOSE SORTED-TRANSAKTIONER
           CLOSE OUTPUT-FILE
           STOP RUN.
       
       CONVERT-TO-DKK.
           MOVE FUNCTION
               NUMVAL(BELØB-TEXT OF SORTED-REC) TO BELØB-NUM
           IF VALUTA OF SORTED-REC = "USD"
               COMPUTE CUR-LINE-VAL-DKK =
               BELØB-NUM * 6.30
           ELSE IF VALUTA OF SORTED-REC = "EUR"
               COMPUTE CUR-LINE-VAL-DKK =
               BELØB-NUM * 7.50
           ELSE
               COMPUTE CUR-LINE-VAL-DKK =
               BELØB-NUM * 1.00
           END-IF.
       
       HANDLE-TOP-3.
           IF CUR-ACC-SALDO > KUNDE-SALDO OF TOP-3-ACCOUNTS(1)
               MOVE TOP-3-ACCOUNTS(2) TO TOP-3-ACCOUNTS(3)
               MOVE TOP-3-ACCOUNTS(1) TO TOP-3-ACCOUNTS(2)
               MOVE NAVN OF PREVIOUS-REC TO 
                   KUNDE-NAVN OF TOP-3-ACCOUNTS(1)
               MOVE KONTO-ID OF PREVIOUS-REC TO 
                   KUNDE-ID OF TOP-3-ACCOUNTS(1)
               MOVE CUR-ACC-SALDO TO 
                   KUNDE-SALDO OF TOP-3-ACCOUNTS(1)
           ELSE IF CUR-ACC-SALDO > KUNDE-SALDO OF TOP-3-ACCOUNTS(2)
               MOVE TOP-3-ACCOUNTS(2) TO TOP-3-ACCOUNTS(3)
               MOVE NAVN OF PREVIOUS-REC TO 
                   KUNDE-NAVN OF TOP-3-ACCOUNTS(2)
               MOVE KONTO-ID OF PREVIOUS-REC TO 
                   KUNDE-ID OF TOP-3-ACCOUNTS(2)
               MOVE CUR-ACC-SALDO TO 
                   KUNDE-SALDO OF TOP-3-ACCOUNTS(2)
           ELSE IF CUR-ACC-SALDO > KUNDE-SALDO OF TOP-3-ACCOUNTS(3)
               MOVE NAVN OF PREVIOUS-REC TO 
                   KUNDE-NAVN OF TOP-3-ACCOUNTS(3)
               MOVE KONTO-ID OF PREVIOUS-REC TO 
                   KUNDE-ID OF TOP-3-ACCOUNTS(3)
               MOVE CUR-ACC-SALDO TO KUNDE-SALDO OF TOP-3-ACCOUNTS(3)
           END-IF.
       
       HANDLE-MONTHS.
           MOVE TIDSPUNKT OF SORTED-REC (6:2) TO CUR-MONTH
           IF TRANSAKTIONSTYPE OF SORTED-REC = "Indbetaling"
               ADD CUR-LINE-VAL-DKK TO INDBETALINGDKK OF 
                   MONTHLY-STATS(CUR-MONTH)
           ELSE IF TRANSAKTIONSTYPE OF SORTED-REC = "Udbetaling"
               ADD CUR-LINE-VAL-DKK TO UDBETALINGDKK OF 
                   MONTHLY-STATS(CUR-MONTH)
           END-IF.

      *COUNT-BUTIK.
      *    MOVE BUTIK OF SORTED-REC TO BUTIK-KEY OF BUTIK-COUNT(1)
      *    PERFORM FIND-BUTIK
      *    ADD 1 TO BUTIK-COUNT(1).
      * 
      *FIND-BUTIK.
      *    SET IDX TO 1
      *    PERFORM UNTIL IDX > 100
      *        IF BUTIK-KEY OF BUTIK-COUNT(IDX) = BUTIK OF SORTED-REC
      *            ADD 1 TO BUTIK-CNT OF BUTIK-COUNT(IDX)
      *            EXIT PERFORM
      *        END-IF
      *        ADD 1 TO IDX
      *    END-PERFORM
      *    IF IDX > 100
      *        DISPLAY "Butik not found in BUTIK-COUNT table"
      *    END-IF.
      * 
      *DISPLAY-BUTIK-COUNTS.
      *    DISPLAY "BUTIK COUNTS:"
      *    SET IDX TO 1
      *    PERFORM UNTIL IDX > 100
      *        IF BUTIK-CNT OF BUTIK-COUNT(IDX) > 0
      *            DISPLAY "BUTIK: " BUTIK-KEY OF BUTIK-COUNT(IDX)
      *                    " COUNT: " BUTIK-CNT OF BUTIK-COUNT(IDX)
      *        END-IF
      *        ADD 1 TO IDX
      *    END-PERFORM.
        