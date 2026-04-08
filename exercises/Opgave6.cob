       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Kundeoplysninger.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           COPY "KUNDER2.cpy".
       WORKING-STORAGE SECTION.
       01  FULDT-NAVN              PIC X(40).
       01  RENS-FULDT-NAVN         PIC X(40).
       01  IX                      PIC 9(2).
       01  IX2                     PIC 9(2) VALUE 1.
       01  CURRENT-CHAR            PIC X(1).
       01  PREVIOUS-CHAR           PIC X(1) VALUE SPACE.
       01  EOF PIC X VALUE 'N'.
       01  WS-BALANCE-DISPLAY.
           03  BALANCE-DISPLAY PIC -ZZZ,ZZ9.99.
       
       PROCEDURE DIVISION.
           PERFORM MAIN-LOGIC.
           STOP RUN.
       
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
   
           PERFORM UNTIL EOF = "Y"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       PERFORM VIS-KUNDE
               END-READ
           END-PERFORM
   
           CLOSE INPUT-FILE
           STOP RUN.
       
       VIS-KUNDE.
           MOVE BALANCE TO BALANCE-DISPLAY
           DISPLAY "----------------------------------------".
           DISPLAY "Kunde-ID: " KUNDE-ID
           DISPLAY "Navn: " FORNAVN EFTERNAVN
           DISPLAY "Konto: " BALANCE-DISPLAY
           DISPLAY "Adresse: " ADDRESSE
           DISPLAY "Kontakt: " KONTAKTOPL
           DISPLAY "----------------------------------------".
