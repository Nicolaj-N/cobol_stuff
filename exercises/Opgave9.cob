       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KUNDEOPLYSNINGER ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KONTOOPLYSNINGER ASSIGN TO "KontoOpt.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "KUNDEKONTOARRAY.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       
       DATA DIVISION.
       FILE SECTION.
       FD KUNDEOPLYSNINGER.
       01 KUNDEOPL-IN.
           COPY "KUNDER2.cpy".
       FD KONTOOPLYSNINGER.
       01 KONTOOPL-IN.
           COPY "KONTOOPL.cpy". 
      *    REPLACING ==KUNDE-ID== BY ==ACC-KUNDE-ID==.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           02  NAVN-ADR            PIC X(100).
       01 KUNDEKONTOAAA.
           02 OUTPUT-TEXT          PIC X(100).
       WORKING-STORAGE SECTION.
       01  KUNDEOPL-AR OCCURS 20 TIMES.
           COPY "KUNDER2.cpy".
       01  KONTOOPL-AR OCCURS 20 TIMES.
           COPY "KONTOOPL.cpy".
       01  FULDT-NAVN              PIC X(40).
       01  RENS-FULDT-NAVN         PIC X(40).
       01  IX                      PIC 9(2) VALUE 1.
       01  IX2                     PIC 9(2) VALUE 1.
       01  ARRSIZE                 PIC 9(2).
       01  CURRENT-CHAR            PIC X(1).
       01  PREVIOUS-CHAR           PIC X(1) VALUE SPACE.
       01  EOF1 PIC X VALUE 'N'.
       01  EOF2 PIC X VALUE 'N'.
       01  WS-BALANCE-DISPLAY.
           03  BALANCE-DISPLAY PIC -ZZZ,ZZ9.99.
       
       PROCEDURE DIVISION.
           PERFORM MAIN-LOGIC.
           STOP RUN.
       
       MAIN-LOGIC.
           OPEN INPUT KUNDEOPLYSNINGER
           OPEN INPUT KONTOOPLYSNINGER
           OPEN OUTPUT OUTPUT-FILE
           PERFORM UNTIL EOF1 = "Y" 
               READ KUNDEOPLYSNINGER
               AT END
                   MOVE "Y" TO EOF1
               NOT AT END
                   MOVE KUNDEOPL-IN TO KUNDEOPL-AR(IX)
                   ADD 1 TO IX
               END-READ
           END-PERFORM
           PERFORM UNTIL EOF2 = "Y" OR IX2 > IX
               READ KONTOOPLYSNINGER
               AT END
                   MOVE "Y" TO EOF2
               NOT AT END
                   MOVE KONTOOPL-IN TO KONTOOPL-AR(IX2)
                   ADD 1 TO IX2
               END-READ
           END-PERFORM
           IF IX2 > IX
               DISPLAY "FILES NEED TO HAVE EQUAL LENGTH"
           END-IF
           COMPUTE ARRSIZE =(IX - 1)
           MOVE 1 TO IX
           MOVE 1 TO IX2
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > ARRSIZE
               PERFORM VARYING IX2 FROM 1 BY 1 UNTIL IX2 > ARRSIZE
                   IF KUNDE-ID OF KUNDEOPL-AR(IX) NOT = SPACES
                       AND KUNDE-ID OF KONTOOPL-AR(IX2) NOT = SPACES
                       IF KUNDE-ID OF KUNDEOPL-AR(IX)
                           = KUNDE-ID OF KONTOOPL-AR(IX2)
                           MOVE KONTOOPL-AR(IX2) TO OUTPUT-RECORD
                           WRITE OUTPUT-RECORD

                       END-IF
                   END-IF

               END-PERFORM

               MOVE KUNDE-ID OF KUNDEOPL-AR(IX) TO NAVN-ADR
               MOVE NAVN-ADR TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD

               PERFORM FORMAT-NAVN
               PERFORM FORMAT-VEJ
               PERFORM FORMAT-BY

               MOVE TELEFON OF KUNDEOPL-AR(IX) TO NAVN-ADR
               MOVE NAVN-ADR TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD

               MOVE EMAIL OF KUNDEOPL-AR(IX) TO NAVN-ADR
               MOVE NAVN-ADR TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
               display ARRSIZE
               MOVE SPACES TO NAVN-ADR
               MOVE NAVN-ADR TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD

           END-PERFORM
   
           CLOSE KUNDEOPLYSNINGER
           CLOSE KONTOOPLYSNINGER
           CLOSE OUTPUT-FILE
           STOP RUN.
       
       FORMAT-NAVN.
           STRING FORNAVN OF KUNDEOPL-AR(IX) DELIMITED BY SIZE " "
               DELIMITED BY SIZE EFTERNAVN OF KUNDEOPL-AR(IX)
               DELIMITED BY SIZE
               INTO NAVN-ADR
           MOVE NAVN-ADR TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
       
       FORMAT-VEJ.
           STRING VEJNAVN OF KUNDEOPL-AR(IX) DELIMITED BY SIZE " "
               DELIMITED BY SIZE HUSNR OF KUNDEOPL-AR(IX)
               DELIMITED BY SIZE ETAGE OF KUNDEOPL-AR(IX)
               DELIMITED BY SIZE SIDE OF KUNDEOPL-AR(IX)
               INTO NAVN-ADR
           MOVE NAVN-ADR TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
       
       FORMAT-BY.
           STRING POSTNR OF KUNDEOPL-AR(IX) DELIMITED BY SIZE " "
               DELIMITED BY SIZE BYNAVN OF KUNDEOPL-AR(IX)
               INTO NAVN-ADR
           MOVE NAVN-ADR TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
           