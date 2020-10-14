      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION                           DIVISION.
       PROGRAM-ID. ELEICAO.
      *----------------------------------------------------------------*
       ENVIRONMENT                              DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                            SECTION.
      *----------------------------------------------------------------*
      *
       INPUT-OUTPUT                             SECTION.

       FILE-CONTROL.

       SELECT ARQAPU ASSIGN TO DISK
               ORGANIZATION  IS LINE SEQUENTIAL.

       SELECT ARQREL ASSIGN TO DISK
               ORGANIZATION  IS LINE SEQUENTIAL.

      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD ARQAPU
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "APURACAO.TXT".


       01  REG-ELEICAO.
           03 FD-TITELEITOR             PIC 9(03).
           03 FD-ELEITOR                PIC X(10).
           03 FD-VOTO                   PIC 9(03).

       FD ARQREL
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "RELELEICAO.PRN".
       01  REG-RELATORIO                PIC X(80).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  WS-EOF                       PIC X(01) VALUE ' '.
       77  WS-LINHABRANCO               PIC X(80) VALUE SPACES.
       77  WS-PONTILHADO                PIC X(80) VALUE ALL '-'.
       77  WS-TOTVOTO                   PIC 9(03) VALUE ZEROS.
       77  CONT-LINHA                   PIC 9(03) VALUE ZEROS.
       77  CONT-ELEITOR                 PIC S9(03) COMP VALUE ZEROS.
       77  WS-PAG-AUX                   PIC 9(03) VALUE ZEROS.

       01  WS-CABECALHO.
           03 WS-TIMESTAMP.
               05 WS-DATA.
                   07 WS-ANO               PIC 9(04).
                   07 WS-MES               PIC 9(02).
                   07 WS-DIA               PIC 9(02).
               05 WS-HORA.
                   07 WS-HH                PIC 9(02).
                   07 WS-MM                PIC 9(02).
                   07 WS-SS                PIC 9(02).
                   07 WS-MS                PIC 9(02).

           03 WS-LINHA.
               05 FILLER PIC X(06) VALUE 'DATA: '.
               05 WS-DATA-AUX PIC X(10).
               05 FILLER PIC X(30) VALUE SPACES.
               05 FILLER PIC X(06) VALUE 'HORA: '.
               05 WS-HORA-AUX PIC X(08).


           03 WS-LINHA1.
               05 FILLER PIC X(21) VALUE 'COBOL COURSE BY INDRA'.
               05 FILLER PIC X(50) VALUE SPACES.
               05 FILLER PIC X(05) VALUE 'PAGE'.
               05 WS-PAG PIC zzz9.



           03 WS-LINHA2.
               05 FILLER PIC X(26) VALUE SPACES.
               05 FILLER PIC X(26) VALUE 'LISTAGEM DE VOTOS APURADOS'.
               05 FILLER PIC X(28) VALUE SPACES.

           03 WS-LINHA3.
               05 FILLER PIC X(12) VALUE 'LEG. PARTIDO'.
               05 FILLER PIC X(04) VALUE SPACES.
               05 FILLER PIC X(17) VALUE 'NOME DO CANDIDATO'.
               05 FILLER PIC X(04) VALUE SPACES.
               05 FILLER PIC X(14) VALUE 'TOTAL DE VOTOS'.
               05 FILLER PIC X(29) VALUE SPACES.

       01  WS-AUXILIAR.
           03 WS-VOTO                     PIC 9(03) VALUE ZEROS.


       01  WS-EXIBICAO1.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 001.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(06) VALUE 'THULIO'.
           03 FILLER                      PIC X(26) VALUE SPACES.
           03 CONT-VOTO1                  PIC 9(03) VALUE ZEROS.

       01  WS-EXIBICAO2.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 002.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(03) VALUE 'ANA'.
           03 FILLER                      PIC X(29) VALUE SPACES.
           03 CONT-VOTO2                  PIC 9(03) VALUE ZEROS.

       01  WS-EXIBICAO3.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 003.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(06) VALUE 'AMOEDO'.
           03 FILLER                      PIC X(26) VALUE SPACES.
           03 CONT-VOTO3                  PIC 9(03) VALUE ZEROS.

       01  WS-EXIBICAO4.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 004.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(04) VALUE 'JOAO'.
           03 FILLER                      PIC X(28) VALUE SPACES.
           03 CONT-VOTO4                  PIC 9(03) VALUE ZEROS.

       01  WS-EXIBICAO5.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 005.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(04) VALUE 'DANI'.
           03 FILLER                      PIC X(28) VALUE SPACES.
           03 CONT-VOTO5                  PIC 9(03) VALUE ZEROS.





       01  WS-RODAPE1.
           03 FILLER             PIC X(20) VALUE 'TOTAL DE ELEITORES: '.
           03 WS-TOTELEITOR      PIC 9(05).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           MAIN-PROCEDURE.
               OPEN INPUT ARQAPU
               OPEN OUTPUT ARQREL

               MOVE 70 TO CONT-LINHA

               PERFORM 1000-LER-ARQUIVO


               PERFORM UNTIL WS-EOF = 'S'
                   IF CONT-LINHA >=60
                       PERFORM 2000-TRATA-CABECALHO
                   END-IF

                   PERFORM 3000-TRATA-EXIBICAO
                   PERFORM 1000-LER-ARQUIVO
               END-PERFORM


               PERFORM 4000-TRATA-RODAPE

               CLOSE ARQAPU      ARQREL
               STOP RUN.


      *----------------------------------------------------------------*
       1000-LER-ARQUIVO.
      *----------------------------------------------------------------*
           READ ARQAPU
               AT END
                   MOVE 'S' TO WS-EOF
               NOT AT END
                   CONTINUE
           END-READ
           .
           EXIT.
      *----------------------------------------------------------------*
       2000-TRATA-CABECALHO.
      *----------------------------------------------------------------*

           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP

           STRING WS-DIA'/'WS-MES'/'WS-ANO
           DELIMITED BY SIZE INTO WS-DATA-AUX
           END-STRING

           STRING WS-HH':'WS-MM':'WS-SS
           DELIMITED BY SIZE INTO WS-HORA-AUX
           END-STRING

           ADD 1 TO WS-PAG-AUX
           MOVE WS-PAG-AUX TO WS-PAG

           DISPLAY WS-LINHA1
           WRITE REG-RELATORIO FROM WS-LINHA1



           DISPLAY WS-LINHA
           WRITE REG-RELATORIO FROM WS-LINHA

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           DISPLAY WS-LINHA2
           WRITE REG-RELATORIO FROM WS-LINHA2

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           DISPLAY WS-LINHA3
           WRITE REG-RELATORIO FROM WS-LINHA3

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           MOVE 6 TO CONT-LINHA
           .
           EXIT.
      *----------------------------------------------------------------*
       3000-TRATA-EXIBICAO.
      *----------------------------------------------------------------*
           MOVE FD-VOTO TO WS-VOTO



           ADD 1 TO CONT-ELEITOR

           EVALUATE WS-VOTO
               WHEN 001
                   ADD 1 TO CONT-VOTO1
               WHEN 002
                   ADD 1 TO CONT-VOTO2
               WHEN 003
                   ADD 1 TO CONT-VOTO3
               WHEN 004
                   ADD 1 TO CONT-VOTO4
               WHEN 005
                   ADD 1 TO CONT-VOTO5
           END-EVALUATE.

      *----------------------------------------------------------------*
       4000-TRATA-RODAPE.
      *----------------------------------------------------------------*
           MOVE CONT-ELEITOR TO WS-TOTELEITOR

           DISPLAY WS-EXIBICAO1
           WRITE REG-RELATORIO FROM WS-EXIBICAO1

           DISPLAY WS-EXIBICAO2
           WRITE REG-RELATORIO FROM WS-EXIBICAO2

           DISPLAY WS-EXIBICAO3
           WRITE REG-RELATORIO FROM WS-EXIBICAO3

           DISPLAY WS-EXIBICAO4
           WRITE REG-RELATORIO FROM WS-EXIBICAO4

           DISPLAY WS-EXIBICAO5
           WRITE REG-RELATORIO FROM WS-EXIBICAO5

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           DISPLAY WS-RODAPE1
           WRITE REG-RELATORIO FROM WS-RODAPE1

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           EXIT.

       END PROGRAM ELEICAO.
