       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE-1 ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INPUT-FILE-2 ASSIGN TO "KontoOpt.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "KUNDEKONTO.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE-1.
       01 INPUT-RECORD.
           COPY "KUNDER2.cpy".
       FD INPUT-FILE-2.
       01 KONTO-RECORD.
           COPY "KONTOOPL.cpy". 
      *    REPLACING ==KUNDE-ID== BY ==ACC-KUNDE-ID==.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           02  NAVN-ADR            PIC X(100).
       01 KUNDEKONTO.
           02 OUTPUT-TEXT          PIC X(100).
       WORKING-STORAGE SECTION.
       01  FULDT-NAVN              PIC X(40).
       01  RENS-FULDT-NAVN         PIC X(40).
       01  IX                      PIC 9(2).
       01  IX2                     PIC 9(2) VALUE 1.
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
           OPEN INPUT INPUT-FILE-1
           OPEN INPUT INPUT-FILE-2
           OPEN OUTPUT OUTPUT-FILE
           PERFORM UNTIL EOF1 = "Y"
               READ INPUT-FILE-1 INTO INPUT-RECORD
                   AT END
                       MOVE 'Y' TO EOF1
                   NOT AT END
                       MOVE "N" TO EOF2
                       CLOSE INPUT-FILE-2
                       OPEN INPUT INPUT-FILE-2
                       PERFORM UNTIL EOF2 = "Y"
                           READ INPUT-FILE-2 INTO KONTO-RECORD
                           AT END
                               MOVE "Y" TO EOF2
                           NOT AT END
                               IF KUNDE-ID OF INPUT-RECORD = KUNDE-ID
                                   OF KONTO-RECORD
                                   MOVE KONTO-RECORD TO OUTPUT-RECORD
                                   WRITE OUTPUT-RECORD
                               END-IF
                           END-READ
                       END-PERFORM

                       MOVE KUNDE-ID OF INPUT-RECORD TO NAVN-ADR
                       MOVE NAVN-ADR TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
                       PERFORM FORMAT-NAVN
                       PERFORM FORMAT-VEJ
                       PERFORM FORMAT-BY

                       MOVE TELEFON TO NAVN-ADR
                       MOVE NAVN-ADR TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD

                       MOVE EMAIL TO NAVN-ADR
                       MOVE NAVN-ADR TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
                       
                       MOVE SPACES TO NAVN-ADR
                       MOVE NAVN-ADR TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
      *                DISPLAY ACC-KUNDE-ID
               END-READ
           END-PERFORM
   
           CLOSE INPUT-FILE-1
           CLOSE INPUT-FILE-2
           CLOSE OUTPUT-FILE
           STOP RUN.
       
       FORMAT-NAVN.
           STRING FORNAVN DELIMITED BY SIZE " "
               DELIMITED BY SIZE EFTERNAVN
               DELIMITED BY SIZE
               INTO NAVN-ADR
           MOVE NAVN-ADR TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
       
       FORMAT-VEJ.
           STRING VEJNAVN DELIMITED BY SIZE " "
               DELIMITED BY SIZE HUSNR
               DELIMITED BY SIZE ETAGE
               DELIMITED BY SIZE SIDE
               INTO NAVN-ADR
           MOVE NAVN-ADR TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
       
       FORMAT-BY.
           STRING POSTNR DELIMITED BY SIZE " "
               DELIMITED BY SIZE BYNAVN
               INTO NAVN-ADR
           MOVE NAVN-ADR TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
           