       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SPECIAL-NAMES.
      *    ALPHABET EUROPEAN-EXTENDED IS STANDARD-1.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KUNDEOPLYSNINGER ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
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
       FD  KUNDEOPLYSNINGER.
           01  KUNDEOPL-IN.
               COPY "KUNDER_NEW.cpy".
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
               02  NAVN-ADR            PIC X(200).

       WORKING-STORAGE SECTION.
       01  BANKOPL-AR OCCURS 100 TIMES.
           COPY "BANKER.cpy".
       01  CURRENT-ACCOUNT.
           COPY "TRANSAKTIONER.cpy".
       01  START-BALANCE           PIC 99999999999V99.
       01 WS-FOUND-INDEX     PIC 999 VALUE 0.
       01 WS-I               PIC 999 VALUE 0.
       01  IX                      PIC 99999 VALUE 1.
       01  IX2                     PIC 99999 VALUE 1.
       01  IX3                     PIC 99999 VALUE 1.
       01  BLANKSPACE              PIC X(40) VALUE SPACES.
       01  ANTAL-BANK              PIC 999 VALUE ZEROES.
       01  ANTAL-KUNDER            PIC 999 VALUE ZEROES.
       01  CURRENT-CHAR            PIC X(1).
       01  PREVIOUS-CHAR           PIC X(1) VALUE SPACE.
       01  PREVIOUS-KONTO          PIC X(15) VALUE SPACES.
       01  EOF PIC X VALUE "N".
       01  EOF2 PIC X VALUE 'N'.
       01  WS-BALANCE-DISPLAY.
           03  BALANCE-DISPLAY PIC -ZZZ,ZZ9.99.
       
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
           
           MOVE "N" TO EOF
           
           PERFORM UNTIL EOF = "Y"
               READ BANKOPLYSNINGER
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   MOVE BANKOPL-IN TO BANKOPL-AR(IX)
                   DISPLAY BANKOPL-AR(IX)
                   ADD 1 TO IX
               END-READ
           END-PERFORM
           
           MOVE "N" TO EOF
           MOVE 1 TO IX
           PERFORM UNTIL EOF = "Y"
               READ SORTED-TRANSAKTIONER
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                       
                   IF IX < 10
                       DISPLAY "PREVIOUS IS: " PREVIOUS-KONTO
                       DISPLAY "CURRENT IS: " KONTO-ID OF SORTED-REC
                       IF KONTO-ID OF SORTED-REC IS NOT = KONTO-ID OF
                           CURRENT-ACCOUNT
                               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL 
                                   WS-I > ANTAL-BANK

                                   IF REG-NR OF BANKOPL-AR(WS-I)
                                       = REG-NR OF SORTED-REC
                                       DISPLAY WS-I
                                       MOVE WS-I TO WS-FOUND-INDEX
                                       EXIT PERFORM

                                   END-IF

                               END-PERFORM
                               MOVE SORTED-REC TO CURRENT-ACCOUNT
                               PERFORM FORMAT-KUNDEINFO
                               PERFORM FORMAT-BANKINFO
                               MOVE 50000 TO BELØB IN CURRENT-ACCOUNT

                       END-IF
                           
      *                    PERFORM FORMAT-TRANSAKTIONER
                   END-IF
                       ADD 1 TO IX
               END-READ
           END-PERFORM
           CLOSE BANKOPLYSNINGER
           CLOSE SORTED-TRANSAKTIONER
           CLOSE OUTPUT-FILE
           STOP RUN.
      *MAIN-LOGIC.
      *    OPEN INPUT KUNDEOPLYSNINGER
      *    OPEN INPUT BANKOPLYSNINGER
      *    OPEN OUTPUT SORTED-TRANSAKTIONER
      *    OPEN OUTPUT OUTPUT-FILE
      *    PERFORM UNTIL EOF = "Y"
      *        READ KUNDEOPLYSNINGER
      *        AT END
      *            MOVE "Y" TO EOF
      *        NOT AT END
      *            MOVE KUNDEOPL-IN TO KUNDEOPL-AR(IX)
      *            ADD 1 TO IX
      *        END-READ
      *    END-PERFORM
      *    SUBTRACT 1 FROM IX
      *    MOVE IX TO ANTAL-KUNDER
      *    PERFORM UNTIL EOF2 = "Y"
      *        READ BANKOPLYSNINGER
      *        AT END
      *            MOVE "Y" TO EOF2
      *        NOT AT END
      *            MOVE BANKOPL-IN TO BANKOPL-AR(IX2)
      *            ADD 1 TO IX2
      *        END-READ
      *    END-PERFORM
      *    
      *    SUBTRACT 1 FROM IX2
      *    MOVE IX2 TO ANTAL-BANK
      *    DISPLAY ANTAL-BANK
      *    MOVE 1 TO IX
      *    MOVE "N" TO EOF
      *    PERFORM UNTIL EOF = "Y"
      *        READ TRANSAKTIONER
      *        AT END
      *            MOVE "Y" TO EOF
      *        NOT AT END
      *            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > ANTAL-BANK
      *                IF REG-NR OF TRANSAKTIONER-IN = REG-NR OF 
      *                BANKOPL-AR(IX)
      *                    PERFORM FORMAT-TRANSAKTIONER
      *                END-IF
      *            END-PERFORM
      *        END-READ
      *    END-PERFORM
      *    DISPLAY "BEFORE"
      *    DISPLAY KUNDE-ID OF KUNDEOPL-AR(1)
      *    DISPLAY REG-NR OF BANKOPL-AR(1)
      *    SORT SORT-TRANSAKTIONER
      *        ON ASCENDING KEY CPR OF SORT-REC
      *                            KONTO-ID OF SORT-REC
      *                            TIDSPUNKT OF SORT-REC
      *        USING TRANSAKTIONER
      *        GIVING SORTED-TRANSAKTIONER
      *    SORT KUNDEOPL-AR ON ASCENDING KEY KUNDE-ID OF KUNDEOPL-AR
      *    SORT BANKOPL-AR ON ASCENDING KEY REG-NR OF BANKOPL-AR
      *    DISPLAY "AFTER"
      *    DISPLAY KUNDE-ID OF KUNDEOPL-AR(1)
      *    DISPLAY REG-NR OF BANKOPL-AR(1)
      *    CLOSE SORTED-TRANSAKTIONER
      *    OPEN INPUT SORTED-TRANSAKTIONER

      *    MOVE "N" TO EOF

      *    PERFORM UNTIL EOF = "Y"
      *        READ SORTED-TRANSAKTIONER
      *        AT END
      *            MOVE "Y" TO EOF
      *        NOT AT END
      *            DISPLAY CPR OF SORTED-RECS
      *    END-PERFORM
      *    PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 1 
      *        DISPLAY IX
      *        DISPLAY KUNDE-NAVN OF KUNDEOPL-AR(IX)
      *        DISPLAY KUNDE-ADRESSE OF KUNDEOPL-AR(IX)        
      *        MOVE SPACES TO NAVN-ADR
      *        PERFORM FORMAT-KUNDEINFO

      *        MOVE SPACES TO OUTPUT-RECORD
      *        WRITE OUTPUT-RECORD

      *        PERFORM FORMAT-BANKINFO
      *        PERFORM UNTIL EOF = "Y"
      *            READ TRANSAKTIONER
      *            AT END
      *                MOVE "Y" TO EOF
      *            NOT AT END
      *                IF 
      *    END-PERFORM

      *    CLOSE KUNDEOPLYSNINGER
      *    CLOSE BANKOPLYSNINGER
      *    CLOSE OUTPUT-FILE
      *    STOP RUN.
       
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
                   REG-NR OF BANKOPL-AR(WS-I) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           
               MOVE SPACES TO OUTPUT-RECORD
               STRING BLANKSPACE DELIMITED BY SIZE
                   "Bank: " DELIMITED BY SIZE
                   BANKNAVN OF BANKOPL-AR(WS-I) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           
               MOVE SPACES TO OUTPUT-RECORD
               STRING BLANKSPACE DELIMITED BY SIZE
                   "Bankadresse: " DELIMITED BY SIZE
                   BANKADRESSE OF BANKOPL-AR(WS-I) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           
               MOVE SPACES TO OUTPUT-RECORD
               STRING BLANKSPACE DELIMITED BY SIZE
                   "Telefon: " DELIMITED BY SIZE
                   TELEFON OF BANKOPL-AR(WS-I) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           
               MOVE SPACES TO OUTPUT-RECORD
               STRING BLANKSPACE DELIMITED BY SIZE
                   "E-mail: " DELIMITED BY SIZE
                   EMAIL OF BANKOPL-AR(WS-I) DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           STOP RUN.

       FORMAT-TRANSAKTIONER.
           MOVE SPACES TO NAVN-ADR


           STRING BANKNAVN OF BANKOPL-AR(IX) DELIMITED BY SIZE 
               " " DELIMITED BY SIZE 
               BANKADRESSE OF BANKOPL-AR(IX) DELIMITED BY SIZE
               INTO NAVN-ADR
           MOVE NAVN-ADR TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
           