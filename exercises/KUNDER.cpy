       02  KUNDE-ID            PIC X(10) VALUE SPACES.
       02  FORNAVN             PIC X(20) VALUE SPACES.
       02  EFTERNAVN           PIC X(20) VALUE SPACES.
       02  KONTOINFO.
           03  KONTONUMMER     PIC X(20) VALUE SPACES.
           03  BALANCE         PIC S9(7)V9(2) VALUE ZEROES.
           03  VALUTAKODE      PIC X(3) VALUE SPACES.
       02  ADRESSE.
           03    VEJNAVN       PIC X(30) VALUE SPACES.
           03    HUSNR         PIC X(5) VALUE SPACES.
           03    ETAGE         PIC X(5) VALUE SPACES.
           03    SIDE          PIC X(5) VALUE SPACES.
           03    BYNAVN        PIC X(20) VALUE SPACES.
           03    POSTNR        PIC X(4) VALUE SPACES.
           03    LANDE-KODE    PIC X(2) VALUE SPACES. 
       02  KONTAKTOPL.
           03 TELEFON          PIC X(8) VALUE SPACES.
           03 EMAIL            PIC X(50) VALUE SPACES.
