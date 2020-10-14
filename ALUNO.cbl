 ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION                           DIVISION.
       PROGRAM-ID. ALUNO.
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

       SELECT ARQALU ASSIGN TO DISK
               ORGANIZATION  IS LINE SEQUENTIAL.



      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.

       FD ARQALU
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "ALUNO.TXT".


       01  REG-ALUNO.
           03 FD-MATRICULA              PIC 9(03).
           03 FD-NOME                   PIC X(10).




      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
            EXEC SQL INCLUDE SQLCA  END-EXEC.
            EXEC SQL INCLUDE ALUNO  END-EXEC.



       77  WS-MEDIA              PIC 9(03)V99 VALUE ZEROS.
       77  WS-EOF                PIC X(01) VALUE ' '.


       01  DCLALUNO.
           03 MATRICULA PIC 9(03).
           03 NOTA1     PIC 9(03)V99.
           03 NOTA2     PIC 9(03)V99.
           03 NOTA3     PIC 9(03)V99.
           03 NOTA4     PIC 9(03)V99.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           MAIN-PROCEDURE.
               OPEN INPUT ARQALU

               PERFORM FUNC-LER-ARQUIVO

               PERFORM UNTIL WS-EOF = 'S'
                   MOVE FD-MATRICULA TO MATRICULA OF DCLALUNO

                       EXEC SQL
                       SELECT
                         NOTA1
                       , NOTA2
                       , NOTA3
                       , NOTA4

                   INTO
                        :DCLALUNO.NOTA1
                       ,:DCLALUNO.NOTA2
                       ,:DCLALUNO.NOTA3
                       ,:DCLALUNO.NOTA4
                       FROM ALUNO
                       WHERE
                           MATRICULA = :DCLALUNO.MATRICULA
                   END-EXEC


                       IF SQLCODE = 0
                           DISPLAY 'ALUNO 'FD-NOME' ESTA CADASTRADO'
                           COMPUTE WS-MEDIA =
                                   (NOTA1 OF DCALUNO+
                                    NOTA2 OF DCALUNO+
                                    NOTA3 OF DCALUNO+
                                    NOTA4 OF DCALUNO) / 4
                            END-COMPUTE
                           DISPLAY'A SUA MEDIA E: ' WS-MEDIA

                       ELSE
                           DISPLAY 'ALUNO NAO CADASTRADO'
                       END-IF
               END-PERFORM

               CLOSE ARQALU
               STOP RUN.




      *----------------------------------------------------------------*
       FUNC-LER-ARQUIVO
      *----------------------------------------------------------------*

           READ ARQALU
               AT END
                   MOVE 'S' TO WS-EOF
               NOT END
                   CONTINUE
           END-READ

           .

           EXIT.

       END PROGRAM ALUNO.
