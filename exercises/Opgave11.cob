       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           ALPHABET EUROPEAN-EXTENDED IS STANDARD-1.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANKOPLYSNINGER ASSIGN TO "Banker.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSAKTIONER ASSIGN TO "Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-TRANSAKTIONER ASSIGN TO "WRK.tmp".
           SELECT SORTED-TRANSAKTIONER ASSIGN TO "SortedTrans.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "Kontoudskrift.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       
       DATA DIVISION.
       FILE SECTION.
       FD  BANKOPLYSNINGER.
           01  BANKOPL-IN.
               COPY "BANKER.cpy".

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
       01  BANKOPL-AR OCCURS 100 TIMES.
           COPY "BANKER.cpy".
       01  PREV-REC.
           COPY "TRANSAKTIONER.cpy".
       01  TOP-3-ACCOUNTS OCCURS 3 TIMES.
           COPY "Transaktioner.cpy".
       01  TOP-1-BALANCE           PIC S9(18)V99 VALUE ZEROES.
       01  TOP-2-BALANCE           PIC S9(18)V99 VALUE ZEROES.
       01  TOP-3-BALANCE           PIC S9(18)V99 VALUE ZEROES.
       01  START-BALANCE           PIC 99999999999V99.
       01  IX                      PIC 9999 VALUE 1.
       01  IX2                     PIC 99999 VALUE 1.
       01  IX3                     PIC 99999 VALUE 1.
       01  BLANKSPACE              PIC X(40) VALUE SPACES.
       01  ANTAL-BANK              PIC 999 VALUE ZEROES.
       01  ANTAL-KUNDER            PIC 999 VALUE ZEROES.
       01  CURRENT-CHAR            PIC X(1).
       01  PREVIOUS-CHAR           PIC X(1) VALUE SPACE.
       01  BELØB-NUM               PIC S99999999999V99.
       01  CUR-REG                 PIC 9999 VALUE ZEROES.
       01  CUR-BALANCE             PIC S9(18)V99 VALUE ZEROES.
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
           
           OPEN INPUT BANKOPLYSNINGER
           OPEN INPUT SORTED-TRANSAKTIONER
           OPEN OUTPUT OUTPUT-FILE
           
           PERFORM UNTIL EOF-BANK = "Y"
               READ BANKOPLYSNINGER
               AT END
                   MOVE "Y" TO EOF-BANK
               NOT AT END
                   MOVE BANKOPL-IN TO BANKOPL-AR(IX)
                   ADD 1 TO IX
               END-READ
           END-PERFORM
           
           MOVE 1 TO IX

           READ SORTED-TRANSAKTIONER
               AT END
                   MOVE "Y" TO EOF-MAIN
               NOT AT END
                   MOVE SORTED-REC TO PREV-REC

                   MOVE 0 TO TOTAL-INDBETALT TOTAL-UDBETALT
                   MOVE 50000 TO CUR-BALANCE
                   MOVE REG-NR OF PREV-REC TO CUR-REG

                   PERFORM FORMAT-KUNDEINFO
                   PERFORM FORMAT-BANKINFO
                   PERFORM FORMAT-KOLONNE-NAVNE
           END-READ
           PERFORM UNTIL EOF-MAIN = "Y"
               READ SORTED-TRANSAKTIONER
               AT END
                   PERFORM PRINT-SALDO
                   MOVE "Y" TO EOF-MAIN
               NOT AT END
                   IF KONTO-ID OF SORTED-REC NOT = KONTO-ID OF
                           PREV-REC

                       PERFORM PRINT-SALDO
                       MOVE 0 TO TOTAL-INDBETALT 
                       MOVE 0 TO TOTAL-UDBETALT
                       MOVE 50000 TO CUR-BALANCE
                       MOVE SORTED-REC TO PREV-REC
                       MOVE REG-NR OF PREV-REC TO CUR-REG
                       
                       PERFORM FORMAT-KUNDEINFO
                       PERFORM FORMAT-BANKINFO
                       PERFORM FORMAT-KOLONNE-NAVNE
                   END-IF

                   IF CUR-BALANCE > TOP-1-BALANCE
                       MOVE CUR-BALANCE TO TOP-1-BALANCE
                       MOVE SORTED-REC TO TOP-3-ACCOUNTS(1)
                   ELSE IF CUR-BALANCE > TOP-2-BALANCE
                       MOVE CUR-BALANCE TO TOP-2-BALANCE
                       MOVE SORTED-REC TO TOP-3-ACCOUNTS(2)
                   ELSE IF CUR-BALANCE > TOP-3-BALANCE
                       MOVE CUR-BALANCE TO TOP-3-BALANCE
                       MOVE SORTED-REC TO TOP-3-ACCOUNTS(3)
                   END-IF
                       
                   MOVE FUNCTION 
                       NUMVAL(BELØB-TEXT OF SORTED-REC) TO BELØB-NUM
                   PERFORM FORMAT-VALUTATYPE
                   PERFORM FORMAT-SALDO
                   PERFORM FORMAT-TRANSAKTIONER
               END-READ
           END-PERFORM
           
           DISPLAY NAVN OF TOP-3-ACCOUNTS(1)
           DISPLAY TOP-1-BALANCE
           DISPLAY NAVN OF TOP-3-ACCOUNTS(2)
           DISPLAY TOP-2-BALANCE
           DISPLAY NAVN OF TOP-3-ACCOUNTS(3)
           DISPLAY TOP-3-BALANCE
           CLOSE BANKOPLYSNINGER
           CLOSE SORTED-TRANSAKTIONER
           CLOSE OUTPUT-FILE
           STOP RUN.
       
       FORMAT-KUNDEINFO.
               MOVE "--------------------------------------------"
                   TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
               MOVE SPACES TO OUTPUT-RECORD
               STRING "Kunde: " DELIMITED BY SIZE
                   NAVN OF SORTED-REC DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
               STRING "Adresse: " DELIMITED BY SIZE
                   ADRESSE OF SORTED-REC DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD.

       FORMAT-BANKINFO.
               MOVE SPACES TO OUTPUT-RECORD
               STRING BLANKSPACE DELIMITED BY SIZE
                   "Registreringsnummer: " DELIMITED BY SIZE
                   REG-NR OF BANKOPL-AR(CUR-REG) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           
               MOVE SPACES TO OUTPUT-RECORD
               STRING BLANKSPACE DELIMITED BY SIZE
                   "Bank: " DELIMITED BY SIZE
                   BANKNAVN OF BANKOPL-AR(CUR-REG) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           
               MOVE SPACES TO OUTPUT-RECORD
               STRING BLANKSPACE DELIMITED BY SIZE
                   "Bankadresse: " DELIMITED BY SIZE
                   BANKADRESSE OF BANKOPL-AR(CUR-REG) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           
               MOVE SPACES TO OUTPUT-RECORD
               STRING BLANKSPACE DELIMITED BY SIZE
                   "Telefon: " DELIMITED BY SIZE
                   TELEFON OF BANKOPL-AR(CUR-REG) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           
               MOVE SPACES TO OUTPUT-RECORD
               STRING BLANKSPACE DELIMITED BY SIZE
                   "E-mail: " DELIMITED BY SIZE
                   EMAIL OF BANKOPL-AR(CUR-REG) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD.

       FORMAT-KOLONNE-NAVNE.
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           STRING "Dato          "
               "Tidspunkt   "
               "Transaktionstype    "
               "Beloeb          "
               "Butik"
               INTO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
          
       FORMAT-TRANSAKTIONER.
               MOVE SPACES TO OUTPUT-RECORD
               UNSTRING TIDSPUNKT OF SORTED-REC
                   INTO WS-DATE-STRING, CURRENT-CHAR, WS-TIME-STRING
               
               STRING WS-DATE-STRING DELIMITED BY SIZE
                   "    " DELIMITED BY SIZE
                   WS-TIME-STRING DELIMITED BY SIZE
                   "    " DELIMITED BY SIZE 
                   TRANSAKTIONSTYPE OF SORTED-REC DELIMITED BY SIZE
                   CUR-VAL-DKK-DISPLAY DELIMITED BY SIZE
                   "DKK "
                   "("
                   VALUTA OF SORTED-REC DELIMITED BY SIZE
                   ")"
                   "     " DELIMITED BY SIZE
                   BUTIK OF SORTED-REC DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD.

       FORMAT-VALUTATYPE.
           DISPLAY VALUTA OF SORTED-REC
           IF VALUTA OF SORTED-REC = "USD"
               COMPUTE CUR-VAL-DKK = (BELØB-NUM * 630) / 100
           END-IF
           
           IF VALUTA OF SORTED-REC = "EUR"
               COMPUTE CUR-VAL-DKK = (BELØB-NUM * 750) / 100
           END-IF
           
           IF VALUTA OF SORTED-REC = "DKK"
               COMPUTE CUR-VAL-DKK = (BELØB-NUM * 100) / 100
           END-IF
           MOVE CUR-VAL-DKK TO CUR-VAL-DKK-DISPLAY.
       
       FORMAT-SALDO.
           IF CUR-VAL-DKK < 0
               ADD CUR-VAL-DKK TO TOTAL-UDBETALT
               ADD CUR-VAL-DKK TO CUR-BALANCE
           ELSE
               ADD CUR-VAL-DKK TO TOTAL-INDBETALT
               ADD CUR-VAL-DKK TO CUR-BALANCE
           END-IF.

       PRINT-SALDO.
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE TOTAL-INDBETALT TO TOTAL-IND-DISPLAY
           STRING "Totalt indbetalt" DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               "(DKK): " DELIMITED BY SIZE
               TOTAL-IND-DISPLAY DELIMITED BY SIZE
               INTO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           
           MOVE TOTAL-UDBETALT TO TOTAL-UD-DISPLAY
           STRING "Totalt udbetalt" DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               "(DKK): " DELIMITED BY SIZE
               TOTAL-UD-DISPLAY DELIMITED BY SIZE
               INTO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE CUR-BALANCE TO CUR-BALANCE-DISPLAY
           STRING "Saldo" DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               "(DKK): " DELIMITED BY SIZE
               CUR-BALANCE-DISPLAY DELIMITED BY SIZE
               INTO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.

            
-