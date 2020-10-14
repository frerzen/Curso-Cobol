      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION                           DIVISION.
       PROGRAM-ID. ELEICAO2.
      *----------------------------------------------------------------*
       ENVIRONMENT                              DIVISION.
      *----------------------------------------------------------------*

      *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                            SECTION.

      * SPECIAL-NAMES.
      *     DECIMAL-POINT IS COMMA.
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
           VALUE OF FILE-ID IS "RELELEICAO.TXT".
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
       77  WS-RESTANTE                  PIC 9(03) VALUE ZEROS.
       77  WS-PERCT-AUX1                PIC 9(08)v999999 VALUE ZEROS.
       77  WS-PERCT-AUX2                PIC 9(08)v999999 VALUE ZEROS.
       77  WS-PERCT-AUX3                PIC 9(08)v999999 VALUE ZEROS.
       77  WS-PERCT-AUX4                PIC 9(08)v999999 VALUE ZEROS.
       77  WS-PERCT-AUX5                PIC 9(08)v999999 VALUE ZEROS.
       77  WS-TOTPERCT-AUX              PIC 9(09)v999999 VALUE ZEROS.

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




           03 WS-LINHA1.
               05 FILLER PIC X(15) VALUE 'COBOL COURSE - '.
               05 FILLER PIC X(06) VALUE 'DATA: '.
               05 WS-DATA-AUX PIC X(10).
               05 FILLER PIC X(02) VALUE ' -'.
               05 FILLER PIC X(07) VALUE ' HORA: '.
               05 WS-HORA-AUX PIC X(08).
               05 FILLER PIC X(23) VALUE SPACES.
               05 FILLER PIC X(05) VALUE 'PAG: '.
               05 WS-PAG PIC zzz9.



           03 WS-LINHA2.
               05 FILLER PIC X(26) VALUE SPACES.
               05 FILLER PIC X(26) VALUE 'LISTAGEM DE VOTOS APURADOS'.
               05 FILLER PIC X(28) VALUE SPACES.

           03 WS-LINHA3.
               05 FILLER PIC X(14) VALUE 'TITULO ELEITOR'.
               05 FILLER PIC X(10) VALUE SPACES.
               05 FILLER PIC X(12) VALUE 'NOME ELEITOR'.
               05 FILLER PIC X(36) VALUE SPACES.
               05 FILLER PIC X(4) VALUE 'VOTO'.

           03 WS-LINHA4.
               05 FILLER PIC X(12) VALUE 'LEG. PARTIDO'.
               05 FILLER PIC X(04) VALUE SPACES.
               05 FILLER PIC X(17) VALUE 'NOME DO CANDIDATO'.
               05 FILLER PIC X(04) VALUE SPACES.
               05 FILLER PIC X(14) VALUE 'TOTAL DE VOTOS'.
               05 FILLER PIC X(04) VALUE SPACES.
               05 FILLER PIC X(14) VALUE 'PERC. DE VOTOS'.

           03 WS-LINHA5.
               05 FILLER PIC X(26) VALUE SPACES.
               05 FILLER PIC X(26) VALUE 'RESULTADO FINAL DA VOTACAO'.
               05 FILLER PIC X(28) VALUE SPACES.

       01  WS-AUXILIAR.
           03 WS-VOTO                     PIC 9(03) VALUE ZEROS.


       01  WS-EXIBICAO1.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 001.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(06) VALUE 'THULIO'.
           03 FILLER                      PIC X(26) VALUE SPACES.
           03 CONT-VOTO1                  PIC 9(03) VALUE ZEROS.
           03 FILLER                      PIC X(11) VALUE SPACES.
           03 WS-PCRT1                    PIC Z99.99.
           03 FILLER                      PIC X(01) VALUE '%'.

       01  WS-EXIBICAO2.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 002.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(03) VALUE 'ANA'.
           03 FILLER                      PIC X(29) VALUE SPACES.
           03 CONT-VOTO2                  PIC 9(03) VALUE ZEROS.
           03 FILLER                      PIC X(11) VALUE SPACES.
           03 WS-PCRT2                    PIC Z99.99.
           03 FILLER                      PIC X(01) VALUE '%'.

       01  WS-EXIBICAO3.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 003.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(06) VALUE 'AMOEDO'.
           03 FILLER                      PIC X(26) VALUE SPACES.
           03 CONT-VOTO3                  PIC 9(03) VALUE ZEROS.
           03 FILLER                      PIC X(11) VALUE SPACES.
           03 WS-PCRT3                    PIC Z99.99.
           03 FILLER                      PIC X(01) VALUE '%'.

       01  WS-EXIBICAO4.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 004.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(04) VALUE 'JOAO'.
           03 FILLER                      PIC X(28) VALUE SPACES.
           03 CONT-VOTO4                  PIC 9(03) VALUE ZEROS.
           03 FILLER                      PIC X(11) VALUE SPACES.
           03 WS-PCRT4                    PIC Z99.99.
           03 FILLER                      PIC X(01) VALUE '%'.

       01  WS-EXIBICAO5.
           03 FILLER                      PIC X(01) VALUE SPACES.
           03 FILLER                      PIC 9(03) VALUE 005.
           03 FILLER                      PIC X(12) VALUE SPACES.
           03 FILLER                      PIC X(04) VALUE 'DANI'.
           03 FILLER                      PIC X(28) VALUE SPACES.
           03 CONT-VOTO5                  PIC 9(03) VALUE ZEROS.
           03 FILLER                      PIC X(11) VALUE SPACES.
           03 WS-PCRT5                    PIC Z99.99.
           03 FILLER                      PIC X(01) VALUE '%'.


       01  WS-EXIBICAO6.
           03 WS-TITULO                   PIC 9(03) VALUE ZEROS.
           03 FILLER                      PIC X(21) VALUE SPACES.
           03 WS-ELEITOR                  PIC X(10) VALUE ZEROS.
           03 FILLER                      PIC X(39) VALUE SPACES.
           03 WS-VOTO-AUX                 PIC 9(03) VALUE ZEROS.
           03 FILLER                      PIC X(10) VALUE SPACES.







       01  WS-RODAPE1.
           03 FILLER             PIC X(20) VALUE 'TOTAL DE ELEITORES: '.
           03 WS-TOTELEITOR      PIC 9(05).
           03 FILLER             PIC X(12) VALUE SPACES.
           03 FILLER      PIC X(25) VALUE 'TOTAL DE VOTOS APURADOS: '.
           03 WS-TOTPERCT        PIC Z999.99.
           03 FILLER             PIC X(01) VALUE '%'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           MAIN-PROCEDURE.
               OPEN INPUT ARQAPU
               OPEN OUTPUT ARQREL

               MOVE 70 TO CONT-LINHA

               PERFORM 1000-LER-ARQUIVO
               PERFORM 2000-TRATA-CABECALHO


               PERFORM UNTIL WS-EOF = 'S'


                   PERFORM 3000-TRATA-EXIBICAO
                   PERFORM 1000-LER-ARQUIVO

                   IF CONT-LINHA = 20
                       PERFORM 2000-TRATA-CABECALHO
                   END-IF

               END-PERFORM

               IF CONT-LINHA <= 9
                   DISPLAY WS-LINHABRANCO
                   WRITE REG-RELATORIO FROM WS-LINHABRANCO

                   PERFORM 5000-TRATA-RODAPE
               ELSE
                   IF CONT-LINHA > 9
                       ADD 1 TO WS-PAG-AUX
                       MOVE WS-PAG-AUX TO WS-PAG
                       DISPLAY WS-LINHABRANCO
                       WRITE REG-RELATORIO FROM WS-LINHABRANCO
                       DISPLAY WS-LINHA1
                       WRITE REG-RELATORIO FROM WS-LINHA1

                       DISPLAY WS-LINHABRANCO
                       WRITE REG-RELATORIO FROM WS-LINHABRANCO
                       PERFORM 5000-TRATA-RODAPE
                   END-IF
               END-IF


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



      *     DISPLAY WS-LINHA
      *     WRITE REG-RELATORIO FROM WS-LINHA

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
           MOVE FD-TITELEITOR TO WS-TITULO
           MOVE FD-ELEITOR    TO WS-ELEITOR
           MOVE FD-VOTO TO WS-VOTO-AUX



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

           DISPLAY WS-EXIBICAO6
           WRITE REG-RELATORIO FROM WS-EXIBICAO6
           ADD 1 TO CONT-LINHA
           .
           EXIT.


      *----------------------------------------------------------------*
       5000-TRATA-RODAPE.
      *----------------------------------------------------------------*
           MOVE CONT-ELEITOR TO WS-TOTELEITOR

           COMPUTE WS-PERCT-AUX1=(CONT-VOTO1 * 100)/CONT-ELEITOR.
           MOVE WS-PERCT-AUX1 TO WS-PCRT1.

           COMPUTE WS-PERCT-AUX2=(CONT-VOTO2 * 100)/CONT-ELEITOR.
           MOVE WS-PERCT-AUX2 TO WS-PCRT2.

           COMPUTE WS-PERCT-AUX3=(CONT-VOTO3 * 100)/CONT-ELEITOR.
           MOVE WS-PERCT-AUX3 TO WS-PCRT3.

           COMPUTE WS-PERCT-AUX4=(CONT-VOTO4 * 100)/CONT-ELEITOR.
           MOVE WS-PERCT-AUX4 TO WS-PCRT4.

           COMPUTE WS-PERCT-AUX5=(CONT-VOTO5 * 100)/CONT-ELEITOR.
           MOVE WS-PERCT-AUX5 TO WS-PCRT5.

           COMPUTE
               WS-TOTPERCT-AUX  = WS-PERCT-AUX1 + WS-PERCT-AUX2 +
                                 WS-PERCT-AUX3 + WS-PERCT-AUX4 +
                                 WS-PERCT-AUX5
           END-COMPUTE

           IF WS-TOTPERCT-AUX >= 99.98
               MOVE 100 TO WS-TOTPERCT-AUX
           END-IF



           MOVE WS-TOTPERCT-AUX TO WS-TOTPERCT.







           DISPLAY WS-LINHA5
           WRITE REG-RELATORIO FROM WS-LINHA5

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           DISPLAY WS-LINHA4
           WRITE REG-RELATORIO FROM WS-LINHA4

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

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


           .

           EXIT.

       END PROGRAM ELEICAO2.
