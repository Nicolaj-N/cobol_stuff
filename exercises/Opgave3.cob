       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  KUNDE-ID        PIC X(10).
       01  FORNAVN         PIC X(20).
       01  EFTERNAVN       PIC X(20).
       01  FULDT-NAVN      PIC X(40).
       01  RENS-FULDT-NAVN PIC X(40).
       01  KONTONUMMER     PIC X(20).
      * V9(2) ER DET SAMME SOM V99 
       01  BALANCE         PIC S9(7)V9(2).
       01  VALUTAKODE      PIC X(3).
       01  IX              PIC 9(2).
       01  IX2             PIC 9(2) VALUE 1.
       01  CURRENT-CHAR    PIC X(1).
       01  PREVIOUS-CHAR   PIC X(1) VALUE SPACE.
       PROCEDURE DIVISION.
       MOVE 1234567890 TO KUNDE-ID.
       MOVE "Lars" TO FORNAVN.
       MOVE "Hansen" TO EFTERNAVN.
       STRING FORNAVN DELIMITED BY SIZE " "
           DELIMITED BY SIZE EFTERNAVN
           DELIMITED BY SIZE
           INTO FULDT-NAVN
       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > LENGTH OF FULDT-NAVN
           MOVE FULDT-NAVN(IX:1) TO CURRENT-CHAR

           IF NOT (CURRENT-CHAR = SPACE AND PREVIOUS-CHAR = SPACE)
               MOVE CURRENT-CHAR TO RENS-FULDT-NAVN(IX2:1)
               ADD 1 TO IX2
           END-IF

           MOVE CURRENT-CHAR TO PREVIOUS-CHAR
       END-PERFORM

       MOVE "DK12345678912345" TO KONTONUMMER.
       MOVE 2500.75 TO BALANCE.
       MOVE "DKK" TO VALUTAKODE.
       DISPLAY "-----------------------------------"
       DISPLAY "Kunde-ID    : " KUNDE-ID
      * DISPLAY "Navn        : " FULDT-NAVN
       DISPLAY "Navn        : " RENS-FULDT-NAVN
       DISPLAY "Kontonummer : " KONTONUMMER
       DISPLAY "Balance     : " BALANCE " " VALUTAKODE
       DISPLAY "-----------------------------------"
       STOP RUN.
