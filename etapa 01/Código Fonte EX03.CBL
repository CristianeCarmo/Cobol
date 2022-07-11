       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EM01EX03.                                               
       INSTALLATION.  HOME.
       DATE-WRITTEN.  10-03-2022.
       DATE-COMPILED.
       SECURITY.      APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.       LE OS REGISTROS DO ARQUIVO DE ENTRADAD CADALU E
      *               GRAVA NO ARQUIVO DE SAIDA CADATU COM O CALCULO
      *               DE MEDIA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER.  DELL-PC.
           OBJECT-COMPUTER.  DELL-PC.
           SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADATU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADALU.DAT".
       01 REG-ENT.
           02 COD-ENT    PIC 9(05).
           02 NOME       PIC X(20).
           02 NOTA1      PIC 99V99.
           02 NOTA2      PIC 99V99.
           02 NOTA3      PIC 99V99.

       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
       01 REG-SAI.
           02 COD-SAI    PIC 9(05).
           02 MEDIA      PIC 99V99.

       WORKING-STORAGE SECTION.
          77 FIM-ARQ     PIC X(03) VALUE "NAO".
          77 SOMA-NOTA   PIC 99V99 VALUE ZEROS.
          77 FMEDIA      PIC 99V99 VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-EM01EX03.
           PERFORM INICIO.
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
       OPEN INPUT CADALU
            OUTPUT CADATU.
       PERFORM LEITURA.

       LEITURA.
           READ CADALU AT END MOVE "SIM" TO FIM-ARQ.
                                                                                                                                                                                                                                
       PRINCIPAL.
           PERFORM CALMEDIA.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       CALMEDIA.
           ADD NOTA1
               NOTA2
               NOTA3 GIVING SOMA-NOTA.
           DIVIDE SOMA-NOTA BY 3 GIVING FMEDIA.

       GRAVACAO.
           MOVE COD-ENT TO COD-SAI.
           MOVE FMEDIA TO MEDIA.                               
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADATU.

