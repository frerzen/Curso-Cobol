      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELATALU.
      *----------------------------------------------------------------*
       ENVIRONMENT                           DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                          SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT                            SECTION.

       FILE-CONTROL.

       SELECT ARQALU ASSIGN TO DISK
              ORGANIZATION      IS LINE SEQUENTIAL.
      *
       SELECT ARQREL ASSIGN TO DISK
              ORGANIZATION      IS LINE SEQUENTIAL.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD ARQALU
          LABEL RECORD STANDARD
          VALUE OF FILE-ID IS "ARQALUNOS.TXT".

       01  REG-ALUNO.
           03 FD-MATRICULA-ALU         PIC 9(03).
           03 FD-NOME-ALU              PIC X(10).
           03 FD-NOTA1-ALU             PIC 9(03)V99.
           03 FD-NOTA2-ALU             PIC 9(03)V99.
           03 FD-NOTA3-ALU             PIC 9(03)V99.
           03 FD-NOTA4-ALU             PIC 9(03)V99.

       FD  ARQREL
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "RELALUNOS.TXT".
       01  REG-RELATORIO               PIC X(80).
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  WS-EOF                      PIC X(01) VALUE ' '.
       77  WS-LINHABRANCO              PIC X(80) VALUE SPACES.
       77  WS-PONTILHADO               PIC X(80) VALUE ALL '-'.
       77  WS-TOTMEDIA-AUX             PIC 9(03)V99 VALUES ZEROS.
       77  CONT-ALUNO                  PIC 9(03) VALUE ZEROS.
       77  CONT-LINHA                  PIC 9(03) VALUE ZEROS.
       77  WS-PAG-AUX                  PIC 9(03) VALUE ZEROS.

       01  WS-CABECALHO.
           03 WS-LINHA1.
               05 FILLER PIC X(21) VALUE 'COBOL COURSE BY INDRA'.
               05 FILLER PIC X(50) VALUE SPACES.
               05 FILLER PIC X(05) VALUE 'PAGE:'.
               05 WS-PAG PIC zzz9.

           03 WS-LINHA2.
               05 FILLER PIC X(28) VALUE SPACES.
               05 FILLER PIC X(24) VALUE 'LIST OF STUDENTS'.
               05 FILLER PIC X(28) VALUE SPACES.

           03 WS-LINHA3.
               05 FILLER PIC X(18) VALUE 'STUDENT CODE'.
               05 FILLER PIC X(04) VALUE SPACES.
               05 FILLER PIC X(20) VALUE 'STUDENT NAME'.
               05 FILLER PIC X(05) VALUE SPACES.
               05 FILLER PIC X(19) VALUE 'STUDENT GRADE'.
               05 FILLER PIC X(02) VALUE SPACES.
               05 FILLER PIC X(15) VALUE 'FINAL GRADE'.

       01  WS-AUXILIAR.
           03  WS-MEDIA-AUX               PIC 9(03)V99.
           03  WS-MAIOR-AUX               PIC 9(03)V99 VALUE ZEROS.
           03  WS-MENOR-AUX               PIC 9(03)V99.
           03  WS-NOTA1-ALU               PIC 9(03)V99.
           03  WS-NOTA2-ALU               PIC 9(03)V99.
           03  WS-NOTA3-ALU               PIC 9(03)V99.
           03  WS-NOTA4-ALU               PIC 9(03)V99.
           03  WS-MEDIA-TURMA-AUX         PIC 9(03)V99.

       01  WS-DETALHE-EXIBICAO.
           03 FILLER                      PIC X(07) VALUE SPACES.
           03 WS-MAT-ALUNO                PIC 9(03) VALUE ZEROS.
           03 FILLER                      PIC X(15) VALUE SPACES.
           03 WS-NOME-ALUNO               PIC X(20) VALUE ZEROS.
           03 FILLER                      PIC X(5)  VALUE SPACES.
           03 WS-MEDIA-ALUNO              PIC zz9,99.
           03 FILLER                      PIC X(16) VALUE SPACES.
           03 WS-CONCEITO-ALUNO           PIC X(1).


       01  WS-RODAPE1.
           03 FILLER            PIC X(30) VALUE 'THE CLASS GRADE IS: '.
           03 WS-FINAL_GRADE    PIC ZZ9,99.

       01  WS-RODAPE2.
           03 FILLER           PIC X(30) VALUE 'THE BIGGEST GRADE IS: '.
           03 WS-MAIOR_GRADE   PIC ZZ9,99.

       01  WS-RODAPE3.
           03 FILLER          PIC X(30) VALUE 'THE SMALLEST GRADE IS: '.
           03 WS-MENOR_GRADE  PIC ZZ9,99.


      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
           MAIN-PROCEDURE.
               OPEN INPUT ARQALU
               OPEN OUTPUT ARQREL

               MOVE 70 TO CONT-LINHA

               PERFORM 1000-LER-ARQUIVO

               PERFORM UNTIL WS-EOF = 'S'
                   IF CONT-LINHA >= 60
                       PERFORM 2000-TRATA-CABECALHO
                   END-IF

                   PERFORM 3000-TRATA-DETALHE
                   PERFORM 1000-LER-ARQUIVO
               END-PERFORM

               PERFORM 5000-TRATA-RODAPE


               CLOSE ARQALU  ARQREL
               STOP RUN.
      *----------------------------------------------------------------*
       1000-LER-ARQUIVO.
      *----------------------------------------------------------------*
           READ ARQALU
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

           DISPLAY WS-PONTILHADO
           WRITE REG-RELATORIO FROM WS-PONTILHADO

           MOVE 6 TO CONT-LINHA
           .
           EXIT.

      *----------------------------------------------------------------*
       3000-TRATA-DETALHE.
      *----------------------------------------------------------------*
           MOVE FD-MATRICULA-ALU TO WS-MAT-ALUNO
           MOVE FD-NOME-ALU      TO WS-NOME-ALUNO
           MOVE FD-NOTA1-ALU     TO WS-NOTA1-ALU
           MOVE FD-NOTA2-ALU     TO WS-NOTA2-ALU
           MOVE FD-NOTA3-ALU     TO WS-NOTA3-ALU
           MOVE FD-NOTA4-ALU     TO WS-NOTA4-ALU
           MOVE 999              TO WS-MENOR-AUX

           DISPLAY WS-DETALHE-EXIBICAO
           WRITE REG-RELATORIO FROM WS-DETALHE-EXIBICAO

           ADD 1 TO CONT-ALUNO
           ADD 1 TO CONT-LINHA


           COMPUTE WS-MEDIA-AUX =
               (WS-NOTA1-ALU+WS-NOTA2-ALU+WS-NOTA3-ALU+WS-NOTA4-ALU)/4

           PERFORM 4000-CALCULA-CONCEITO-ALUNO

           ADD WS-MEDIA-AUX TO WS-TOTMEDIA-AUX

            IF WS-MEDIA-AUX > WS-MAIOR-AUX
                MOVE WS-MEDIA-AUX TO WS-MAIOR-AUX
            END-IF

            IF WS-MEDIA-AUX < WS-MENOR-AUX
                MOVE WS-MEDIA-AUX TO WS-MENOR-AUX



           MOVE WS-MEDIA-AUX TO WS-MEDIA-ALUNO
           DISPLAY WS-DETALHE-EXIBICAO
           WRITE REG-RELATORIO FROM WS-DETALHE-EXIBICAO

           ADD 1 TO CONT-LINHA

            IF CONT-LINHA = 57
                PERFORM 5000-TRATA-RODAPE
            END-IF
            .
            EXIT.

      *----------------------------------------------------------------*
       4000-CALCULA-CONCEITO-ALUNO.
      *----------------------------------------------------------------*
            IF WS-MEDIA-AUX > 49,9
                IF WS-MEDIA-AUX <= 69,9
                    MOVE 'C' TO WS-CONCEITO-ALUNO
                ELSE
                    IF WS-MEDIA-AUX <= 90,0
                        MOVE 'B' TO WS-CONCEITO-ALUNO
                    ELSE
                        MOVE 'A' TO WS-CONCEITO-ALUNO
                    END-IF
                END-IF
            ELSE
                MOVE 'D' TO WS-CONCEITO-ALUNO
            END-IF
           .
           EXIT.

      *----------------------------------------------------------------*
       5000-TRATA-RODAPE.
      *----------------------------------------------------------------*
           COMPUTE WS-MEDIA-TURMA-AUX = WS-TOTMEDIA-AUX / CONT-ALUNO

           MOVE WS-MEDIA-TURMA-AUX TO WS-FINAL_GRADE
           MOVE WS-MAIOR-AUX       TO WS-MAIOR_GRADE
           MOVE WS-MENOR-AUX       TO WS-MENOR_GRADE

           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-PONTILHADO

           DISPLAY WS-RODAPE1
           WRITE REG-RELATORIO FROM WS-RODAPE1

           DISPLAY WS-RODAPE2
           WRITE REG-RELATORIO FROM WS-RODAPE2

           DISPLAY WS-RODAPE3
           WRITE REG-RELATORIO FROM WS-RODAPE3

           WRITE REG-RELATORIO FROM WS-PONTILHADO
           DISPLAY WS-PONTILHADO

           ADD 3 TO CONT-LINHA

           INITIALIZE WS-TOTMEDIA-AUX WS-FINAL_GRADE
           .
           EXIT.

       END PROGRAM RELATALU.
