       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "out.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           COPY "KUNDER2.cpy".
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           02  NAVN-ADR            PIC X(100).    
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
           OPEN OUTPUT OUTPUT-FILE

           PERFORM UNTIL EOF = "Y"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE KUNDE-ID TO NAVN-ADR
                       MOVE NAVN-ADR TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
                       PERFORM FORMAT-NAVN
                       PERFORM FORMAT-VEJ
                       PERFORM FORMAT-BY

      *                STRING FORNAVN DELIMITED BY SIZE " "
      *                    DELIMITED BY SIZE EFTERNAVN
      *                    DELIMITED BY SIZE
      *                    INTO NAVN-ADR
      *                MOVE NAVN-ADR TO OUTPUT-RECORD
      *                WRITE OUTPUT-RECORD

      *                STRING VEJNAVN DELIMITED BY SIZE " "
      *                    DELIMITED BY SIZE HUSNR
      *                    DELIMITED BY SIZE ETAGE
      *                    DELIMITED BY SIZE SIDE
      *                    INTO NAVN-ADR
      *                MOVE NAVN-ADR TO OUTPUT-RECORD
      *                WRITE OUTPUT-RECORD
      *                
      *                STRING POSTNR DELIMITED BY SIZE " "
      *                    DELIMITED BY SIZE BYNAVN
      *                    INTO NAVN-ADR
      *                MOVE NAVN-ADR TO OUTPUT-RECORD
      *                WRITE OUTPUT-RECORD

                       MOVE TELEFON TO NAVN-ADR
                       MOVE NAVN-ADR TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD

                       MOVE EMAIL TO NAVN-ADR
                       MOVE NAVN-ADR TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
                       
                       MOVE SPACES TO NAVN-ADR
                       MOVE NAVN-ADR TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
               END-READ
           END-PERFORM
   
           CLOSE INPUT-FILE
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
               DELIMITED BY LINE
               INTO NAVN-ADR
           MOVE NAVN-ADR TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
