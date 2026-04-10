       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KUNDEOPLYSNINGER ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BANKOPLYSNINGER ASSIGN TO "Banker.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSAKTIONER ASSIGN TO "Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "Kontoudskrift.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       
       DATA DIVISION.
       FILE SECTION.
       FD KUNDEOPLYSNINGER.
       01 KUNDEOPL-IN.
           COPY "KUNDER.cpy".
       FD BANKOPLYSNINGER.
       01 BANKOPL-IN.
           COPY "BANKER.cpy".
       FD TRANSAKTIONER.
       01  TRANSAKTIONER-IN.
           COPY "TRANSAKTIONER.cpy".
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           02  NAVN-ADR            PIC X(100).
       WORKING-STORAGE SECTION.
       01  KUNDEOPL-AR OCCURS 501 TIMES.
           COPY "KUNDER.cpy".
       01  BANKOPL-AR OCCURS 501 TIMES.
           COPY "BANKER.cpy".
       01  IX                      PIC 99999 VALUE 1.
       01  IX2                     PIC 99999 VALUE 1.
       01  IX3                     PIC 99999 VALUE 1.
       01  ANTAL-BANK              PIC 999.
       01  CURRENT-CHAR            PIC X(1).
       01  PREVIOUS-CHAR           PIC X(1) VALUE SPACE.
       01  EOF PIC X VALUE 'N'.
       01  EOF2 PIC X VALUE 'N'.
       01  WS-BALANCE-DISPLAY.
           03  BALANCE-DISPLAY PIC -ZZZ,ZZ9.99.
       
       PROCEDURE DIVISION.
           PERFORM MAIN-LOGIC.
           STOP RUN.
       
       MAIN-LOGIC.
           OPEN INPUT KUNDEOPLYSNINGER
           OPEN INPUT BANKOPLYSNINGER
           OPEN INPUT TRANSAKTIONER
           OPEN OUTPUT OUTPUT-FILE
           PERFORM UNTIL EOF = "Y"
               READ KUNDEOPLYSNINGER
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   MOVE KUNDEOPL-IN TO KUNDEOPL-AR(IX)
                   ADD 1 TO IX
               END-READ
           END-PERFORM
           
           MOVE "N" TO EOF
           PERFORM UNTIL EOF = "Y"
               READ BANKOPLYSNINGER
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   MOVE BANKOPL-IN TO BANKOPL-AR(IX2)
                   ADD 1 TO IX2
               END-READ
           END-PERFORM
           
           SUBTRACT 1 FROM IX2
           MOVE IX2 TO ANTAL-BANK
           MOVE 1 TO IX
           MOVE "N" TO EOF
           PERFORM UNTIL EOF = "Y"
               READ TRANSAKTIONER
               AT END
                   MOVE "Y" TO EOF
               NOT AT END
                   MOVE 1 TO IX
                   PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 100
                       IF REG-NR OF TRANSAKTIONER-IN = REG-NR OF 
                       BANKOPL-AR(IX)
                           PERFORM FORMAT-TRANSAKTIONER
                       END-IF
                   END-PERFORM
               END-READ
           END-PERFORM
           CLOSE KUNDEOPLYSNINGER
           CLOSE BANKOPLYSNINGER
           CLOSE TRANSAKTIONER
           CLOSE OUTPUT-FILE
           STOP RUN.

       FORMAT-TRANSAKTIONER.
           STRING BANKNAVN OF BANKOPL-AR(IX) DELIMITED BY SIZE " "
               DELIMITED BY SIZE REG-NR OF BANKOPL-AR(IX)
               INTO NAVN-ADR
           MOVE NAVN-ADR TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
           