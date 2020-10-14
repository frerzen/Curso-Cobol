      ******************************************************************
      * Author: EMERSON
      * Date: 10-10-2020
      * Purpose: DESAFIO FINAL CURSO COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION                               DIVISION.
       PROGRAM-ID. ACADEMIA.
      *----------------------------------------------------------------*
       ENVIRONMENT                                  DIVISION.
      *----------------------------------------------------------------*

      *
      *----------------------------------------------------------------*
       CONFIGURATION                                SECTION.

      *     SPECIAL-NAMES.
      *             DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*

       INPUT-OUTPUT                                 SECTION.

       FILE-CONTROL.

       SELECT ARQACAD ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.

       SELECT ARQREL ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD ARQACAD
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ACADEMIA.TXT".

       01  REG-ACADEMIA.
               03 FD-CPF                  PIC 9(11).
               03 FD-NOME                 PIC X(30).
               03 FD-PESO                 PIC 9(03)V999.
               03 FD-SEXO                 PIC X(01).
               03 FD-ALTURA               PIC 9(03).


       FD ARQREL
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "RELACAD.TXT".
       01  REG-RELATORIO                  PIC X(80).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*

       77  WS-EOF                   PIC X(01) VALUE ' '.
       77  WS-PESO-IDEAL            PIC 9(06)V999 VALUE ZEROS.
       77  WS-CONT-ALUNO            PIC 9(03) VALUE ZEROS.
       77  WS-CONT-HOMEM            PIC 9(03) VALUE ZEROS.
       77  WS-CONT-MULHER           PIC 9(03) VALUE ZEROS.
       77  WS-CONT-LINHA            PIC 9(03) VALUE ZEROS.
       77  WS-LINHABRANCO           PIC X(80) VALUE SPACES.
       77  WS-PONTILHADO            PIC X(80) VALUE ALL '-'.
       77  WS-PAG-AUX               PIC 9(03) VALUE ZEROS.
       77  WS-PESO                  PIC 9(06)V999 VALUE ZEROS.
       77  WS-ALTURA                PIC 9(03) VALUE ZEROS.
       77  WS-SEXO                  PIC X(01) VALUE SPACES.


       01  WS-CPF.

           03 WS-CPF1               PIC 9(03).
           03 WS-CPF2               PIC 9(03).
           03 WS-CPF3               PIC 9(03).
           03 WS-DIG                PIC 9(02).


       01  WS-CABECALHO.
           03 WS-TIMESTAMP.
               05 WS-DATA.
                   07 WS-ANO        PIC 9(04).
                   07 WS-MES        PIC 9(02).
                   07 WS-DIA        PIC 9(02).

               05 WS-HORA.
                   07 WS-HH         PIC 9(02).
                   07 WS-MM         PIC 9(02).
                   07 WS-SS         PIC 9(02).
                   07 WS-MS         PIC 9(02).

           03 WS-LINHA1.
               05 FILLER PIC X(14) VALUE 'CURSO COBOL - '.
               05 FILLER PIC X(06)  VALUE 'DATA: '.
               05 WS-DATA-AUX PIC X(10).
               05 FILLER PIC X(02) VALUE ' -'.
               05 FILLER PIC X(07)   VALUE ' HORA: '.
               05 WS-HORA-AUX PIC X(08).
               05 FILLER PIC X(20) VALUE SPACES.
               05 FILLER PIC X(05) VALUE 'PAG: '.
               05 WS-PAG PIC ZZZ9.

           03 WS-LINHA2.
               05 FILLER PIC X(24) VALUE SPACES.
               05 FILLER PIC X(17) VALUE 'RELACAO DE ALUNOS'.
               05 FILLER PIC X(39) VALUE SPACES.

           03 WS-LINHA3.
               05 FILLER PIC X(03) VALUE 'CPF'.
               05 FILLER PIC X(18) VALUE SPACES.
               05 FILLER PIC X(13) VALUE 'NOME DO ALUNO'.
               05 FILLER PIC X(21) VALUE SPACES.
               05 FILLER PIC X(10) VALUE 'PESO ATUAL'.
               05 FILLER PIC X(05) VALUE SPACES.
               05 FILLER PIC X(10) VALUE 'PESO IDEAL'.

           03 WS-LINHA4.
               05 WS-CPF-AUX PIC X(14).
               05 FILLER PIC X(07) VALUE SPACES.
               05 WS-NOME PIC X(30).
               05 FILLER PIC X(04) VALUE SPACES.
               05 WS-PESO-AT PIC Z999.999.
               05 FILLER PIC X(02) VALUE 'kg'.
               05 FILLER PIC X(05) VALUE SPACES.
               05 WS-PESO-ID PIC Z999.999.
               05 FILLER PIC X(02) VALUE 'kg'.




           03  WS-RODAPE.
               05 FILLER     PIC X(15) VALUE'TOTAL DE ALUNOS'.
               05 FILLER     PIC X(40) VALUE SPACES.
               05 FILLER     PIC X(10) VALUE 'QUANTIDADE'.
               05 FILLER     PIC X(05) VALUE SPACES.

           03  WS-RODAPE-LINHA1.
               05 FILLER     PIC X(10) VALUE'MASCULINO:'.
               05 FILLER     PIC X(50) VALUE SPACES.
               05 WS-QTD-H   PIC 9(04) VALUE ZEROS.

           03  WS-RODAPE-LINHA2.
               05 FILLER     PIC X(09) VALUE'FEMININO:'.
               05 FILLER     PIC X(51) VALUE SPACES.
               05 WS-QTD-M   PIC 9(04) VALUE ZEROS.

           03  WS-RODAPE-LINHA3.
               05 FILLER     PIC X(12) VALUE'TOTAL GERAL:'.
               05 FILLER     PIC X(48) VALUE SPACES.
               05 WS-TOTAL   PIC 9(04) VALUE ZEROS.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           MAIN-PROCEDURE.
               OPEN INPUT ARQACAD
               OPEN OUTPUT ARQREL

               MOVE 70 TO WS-CONT-LINHA

               PERFORM 1000-LER-ARQUIVO
               PERFORM 2000-TRATA-CABECALHO

               PERFORM UNTIL WS-EOF = 'S'

                   PERFORM 3000-TRATA-EXIBICAO
                   PERFORM 1000-LER-ARQUIVO

                   IF WS-CONT-LINHA = 60
                       PERFORM 2000-TRATA-CABECALHO
                   END-IF

               END-PERFORM

               IF WS-CONT-LINHA <= 52
                   DISPLAY WS-LINHABRANCO
                   WRITE REG-RELATORIO FROM WS-LINHABRANCO

                   PERFORM 4000-TRATA-RODAPE
               ELSE
                  IF WS-CONT-LINHA > 52
                       ADD 1 TO WS-PAG-AUX
                       MOVE WS-PAG-AUX TO WS-PAG
                       DISPLAY WS-LINHABRANCO
                       WRITE REG-RELATORIO FROM WS-LINHABRANCO
                       DISPLAY WS-LINHA1
                       WRITE REG-RELATORIO FROM WS-LINHA1
                       PERFORM 4000-TRATA-RODAPE
                   END-IF
               END-IF


              CLOSE ARQACAD               ARQREL
              STOP RUN.



      *----------------------------------------------------------------*
       1000-LER-ARQUIVO.
      *----------------------------------------------------------------*

           READ ARQACAD
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

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           DISPLAY WS-LINHA2
           WRITE REG-RELATORIO FROM WS-LINHA2

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           DISPLAY WS-LINHA3
           WRITE REG-RELATORIO FROM WS-LINHA3

           MOVE 5 TO WS-CONT-LINHA
           .
           EXIT.
      *----------------------------------------------------------------*
       3000-TRATA-EXIBICAO.
      *----------------------------------------------------------------*
           MOVE FD-CPF TO WS-CPF

           MOVE FD-PESO TO WS-PESO

           MOVE FD-ALTURA TO WS-ALTURA

           MOVE FD-SEXO TO WS-SEXO

           MOVE FD-NOME TO WS-NOME

           STRING WS-CPF1'.'WS-CPF2'.'WS-CPF3'-'WS-DIG
           DELIMITED BY SIZE INTO WS-CPF-AUX
           END-STRING

           ADD 1 TO WS-CONT-ALUNO

           IF WS-SEXO = 'M'
               ADD 1 TO WS-CONT-HOMEM
               COMPUTE WS-PESO-IDEAL = (WS-ALTURA*0.95) - 95
               MOVE WS-PESO-IDEAL TO WS-PESO-ID
               MOVE WS-PESO TO WS-PESO-AT

               ELSE
                   ADD 1 TO WS-CONT-MULHER
                   COMPUTE WS-PESO-IDEAL = (WS-ALTURA*0.85) - 85
                   MOVE WS-PESO-IDEAL TO WS-PESO-ID
                   MOVE WS-PESO TO WS-PESO-AT

           END-IF






           DISPLAY WS-LINHA4
           WRITE REG-RELATORIO FROM WS-LINHA4
           ADD 1 TO WS-CONT-LINHA
           .
           EXIT.

      *----------------------------------------------------------------*
       4000-TRATA-RODAPE.
      *----------------------------------------------------------------*

           MOVE WS-CONT-MULHER TO WS-QTD-M
           MOVE WS-CONT-HOMEM TO WS-QTD-H
           MOVE WS-CONT-ALUNO TO WS-TOTAL


           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           DISPLAY WS-RODAPE
           WRITE REG-RELATORIO FROM WS-RODAPE

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           DISPLAY WS-RODAPE-LINHA1
           WRITE REG-RELATORIO FROM WS-RODAPE-LINHA1

           DISPLAY WS-RODAPE-LINHA2
           WRITE REG-RELATORIO FROM WS-RODAPE-LINHA2

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           DISPLAY WS-RODAPE-LINHA3
           WRITE REG-RELATORIO FROM WS-RODAPE-LINHA3

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           .

           EXIT.

       END PROGRAM ACADEMIA.
