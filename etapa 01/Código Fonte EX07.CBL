
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EM01EX07.
       AUTHOR.        CRISTIANE CARMO.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  02-04-2022.
       DATE-COMPILED.
       SECURITY.      APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.       LE OS REGISTROS DO ARQUIVO DE ENTRADA CADALU E
      *               GRAVA NO ARQUIVO DE SAIDA CADATU A MEDIA DOS
      *               ALUNOS DO SEXO FEMININO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. DELL-PC.
           OBJECT-COMPUTER. DELL-PC.
           SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

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
           02 NUMERO-ENT  PIC 9(05).
           02 NOME-ENT    PIC X(20).
           02 NOTA1       PIC 99V99.
           02 NOTA2       PIC 99V99.
           02 NOTA3       PIC 99V99.
           02 NOTA4       PIC 99V99.
           02 SEXO-ENT    PIC X(01).

       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
       01 REG-SAI.
           02 NUMERO-SAI  PIC 9(05).
           02 NOME-SAI    PIC X(20).
           02 MEDIA       PIC 99V99.
           02 SEXO-SAI    PIC X(01).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ         PIC X(03) VALUE "NAO".
       77 SOMA-NOTA       PIC 99V99 VALUE ZEROS.
       77 MEDIAF          PIC 99V99 VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-EM01EX07.
           PERFORM INICIO.
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT  CADALU
                OUTPUT CADATU.
           PERFORM LEITURA.

       LEITURA.
           READ CADALU AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM CONDICAO.
           PERFORM LEITURA.

       CONDICAO.
           IF SEXO-ENT EQUAL "F" OR "f" THEN
           PERFORM CALMEDIA
           PERFORM GRAVACAO.

       CALMEDIA.
           ADD NOTA1,                         
               NOTA2,
               NOTA3,
               NOTA4 GIVING SOMA-NOTA.
           DIVIDE SOMA-NOTA BY 4 GIVING MEDIAF.

       GRAVACAO.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE MEDIAF TO MEDIA.
           MOVE SEXO-ENT TO SEXO-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADATU.




