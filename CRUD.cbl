  *+--------------------------------------------------------------+
      * MOTIVO     : APRESENTACAO                                     *
      * AUTOR      : THULIO MARCIO SILVA RIBEIRO                      *
      * DATA       : 21-08-2020                                       *
      * DESCRIPCION: PROGRAMA COBOL + DB2                             *
      *+--------------------------------------------------------------+
       IDENTIFICATION DIVISION.
      *-----------------------.
       PROGRAM-ID.   THULIO3.
       AUTHOR THULIO.
       DATE-WRITTEN. AGOST 2020.
      *+--------------------
       ENVIRONMENT DIVISION.
      *---------------------
      *---------------------                                            00003200
       CONFIGURATION SECTION.
      *---------------------                                            00003200
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
      *---------------------                                            00003200
       FILE-CONTROL.
      *---------------------                                            00003200
      *
           SELECT ALUNO    ASSIGN  TO  ARQJCL.
      *
      *+---------------------------------------------------------------+00003900
      *                      DATA   DIVISION                            00004000
      *                      ===============                            00004100
      *+---------------------------------------------------------------+00004200
       DATA DIVISION.                                                   00004400
      *--------------                                                   00004500
       FILE SECTION.                                                    00004700
      *
       FD  ALUNO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 33 CHARACTERS.
       01  REG-ENTRADA.
           03 FD-MATRICULA PIC 9(03).
           03 FD-NOME      PIC X(10).
           03 FD-NOTA1     PIC 9(03)V99.
           03 FD-NOTA2     PIC 9(03)V99.
           03 FD-NOTA3     PIC 9(03)V99.
           03 FD-NOTA4     PIC 9(03)V99.

      *
      *+-----------------------------------------------------------+
      *                   WORKING-STORAGE SECTION                  *
      *                  =========================                 *
      *+-----------------------------------------------------------+
       WORKING-STORAGE SECTION.

       01  FILLER                        PIC X(25)
                VALUE 'INICIO WORKING THULIO'.

      *+-----------------------------------------------------------+
      *  CONTADORES
      *+-----------------------------------------------------------+
       01  CONTADORES.
           03 CNT-LIN            PIC 9(03).

      *+-----------------------------------------------------------+
      *  VARIABLES DEL PROGRAMA
      *+-----------------------------------------------------------+

       01  WS-FIM-ARQUIVO        PIC X(01).

       01  WS-VARIAVEIS.
           03 WS-MEDIA           PIC 9(03)V99.
           03 WS-SQLCODE         PIC -999.
           03 WS-DATA-HORA       PIC X(30).
           03 WS-TIMESTAMP.
              05 WS-DATA.
                 07 WS-ANO               PIC 9(04).
                 07 WS-MES               PIC 9(02).
                 07 WS-DIA               PIC 9(02).
              05 WS-HORA.
                 07 WS-HH                PIC 9(02).
                 07 WS-MM                PIC 9(02).
                 07 WS-SS                PIC 9(02).
                 07 WS-MS                PIC 9(06).

      *----------------------------------------------------------------*
      *AREA DE DEFINICAO DE TABELAS DB2 - INCLUSÃO DE TABELAS          *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA  END-EXEC.
           EXEC SQL INCLUDE ALUNO  END-EXEC.
      *----------------------------------------------------------------*
      *01 DCLALUNO.
      *   03 MATRICULA PIC 9(03).
      *   03 NOTA1     PIC 9(03)V99.
      *   03 NOTA2     PIC 9(03)V99.
      *   03 NOTA3     PIC 9(03)V99.
      *   03 NOTA4     PIC 9(03)V99.
      *----------------------------------------------------------------*
       01  FILLER                        PIC X(25)
               VALUE 'FIN WORKING THULIO'.

      *+-----------------------------------------------------------+
      *                     PROCEDURE DIVISION                     *
      *                    ====================                    *
      *+-----------------------------------------------------------+
       PROCEDURE DIVISION.
      *------------------.

           DISPLAY ' INICIO DO PROGRAMA '
           PERFORM 10000-INICIO

           DISPLAY ' INICIO DO PROCESSAMENTO '
           PERFORM 20000-PROCESO

           DISPLAY ' FINALIZACAO DO PROGRAMA '
           PERFORM 30000-FIN
           .
      **************************************************************
      *                                                            *
      *                        10000-INICIO                        *
      *                                                            *
      **************************************************************
       10000-INICIO.
      *------------.

           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP

           STRING WS-DIA '/'
                  WS-MES '/'
                  WS-ANO ' - '
                  WS-HH  ':'
                  WS-MM  ':'
                  WS-SS
               DELIMITED BY SIZE INTO WS-DATA-HORA
           END-STRING

           DISPLAY 'DATA/HORA INICIO: ' WS-DATA-HORA

           INITIALIZE WS-VARIAVEIS

           OPEN INPUT  ALUNO
           .

      **************************************************************
      *                                                            *
      *                    15000-R-LEER-CONTROL                    *
      *                                                            *
      **************************************************************
      *                                                            *
      *                        20000-PROCESO                       *
      *                                                            *
      **************************************************************
       20000-PROCESO.
      *-------------.
           PERFORM LER-ARQUIVO

           PERFORM UNTIL WS-FIM-ARQUIVO = 'S'
              MOVE FD-MATRICULA TO MATRICULA OF DCLALUNO
              PERFORM 20001-VALIDA-MATRICULA
              IF WS-SQLCODE = 100
                 PERFORM 20002-CADASTRA-ALUNO
              END-IF
              PERFORM 20004-ATUALIZA-NOTAS
              PERFORM 20003-CALCULA-MEDIA
              IF WS-MEDIA < 5
                 PERFORM 20005-DELETA-ALUNO
              END-IF
              PERFORM LER-ARQUIVO
           END-PERFORM

           .
      *-------------------------
       20001-VALIDA-MATRICULA.
      *-------------------------

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

           MOVE SQLCODE TO WS-SQLCODE

           .
           EXIT.
      *-------------------------
       20002-CADASTRA-ALUNO.
      *-------------------------
           MOVE FD-MATRICULA       TO MATRICULA OF DCLALUNO
           MOVE FD-NOTA1           TO NOTA1     OF DCLALUNO
           MOVE FD-NOTA2           TO NOTA2     OF DCLALUNO
           MOVE FD-NOTA3           TO NOTA3     OF DCLALUNO
           MOVE FD-NOTA4           TO NOTA4     OF DCLALUNO
           EXEC SQL
              INSERT INTO ALUNOS
                ( MATRICULA
                , NOTA1
                , NOTA2
                , NOTA3
                , NOTA4 )
                VALUE
                (:DCLALUNO.MATRICULA
                ,:DCLALUNO.NOTA1
                ,:DCLALUNO.NOTA2
                ,:DCLALUNO.NOTA3
                ,:DCLALUNO.NOTA4 )
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           IF WS-SQLCODE = -803
              DISPLAY 'REGISTRO DUPLICADO'
           ELSE
              IF WS-SQLCODE = 0
                 DISPLAY 'ALUNO CADASTRADO COM SUCESSO'
              ELSE
                 DISPLAY 'ERRO DB2: ' WS-SQLCODE
                 GOBACK
              END-IF
           END-IF
           .
           EXIT.
      *-------------------------
       20004-ATUALIZA-NOTAS.
      *-------------------------
           MOVE FD-MATRICULA       TO MATRICULA OF DCLALUNO
           MOVE FD-NOTA1           TO NOTA1     OF DCLALUNO
           MOVE FD-NOTA2           TO NOTA2     OF DCLALUNO
           MOVE FD-NOTA3           TO NOTA3     OF DCLALUNO
           MOVE FD-NOTA4           TO NOTA4     OF DCLALUNO
           EXEC SQL
              UPDATE ALUNOS
                 SET  NOTA1 = :DCLALUNO.NOTA1
                    , NOTA2 = :DCLALUNO.NOTA2
                    , NOTA3 = :DCLALUNO.NOTA3
                    , NOTA4 = :DCLALUNO.NOTA4
               WHERE
                  MATRICULA = :DCLALUNO.MATRICULA
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           IF WS-SQLCODE = 100
              DISPLAY 'ALUNO NAO ATUALIZADO'
           ELSE
              IF WS-SQLCODE = 0
                 DISPLAY 'ALUNO ATUALIZADO COM SUCESSO'
              ELSE
                 DISPLAY 'ERRO DB2: ' WS-SQLCODE
                 GOBACK
              END-IF
           END-IF
           .
           EXIT.
      *-------------------------
       20005-DELETA-ALUNO.
      *-------------------------
           MOVE FD-MATRICULA       TO MATRICULA OF DCLALUNO

           EXEC SQL
              DELETE FROM ALUNOS
               WHERE
                  MATRICULA = :DCLALUNO.MATRICULA
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           IF WS-SQLCODE = 100
              DISPLAY 'ALUNO NAO DELETADO'
           ELSE
              IF WS-SQLCODE = 0
                 DISPLAY 'ALUNO EXCLUIDO COM SUCESSO'
              ELSE
                 DISPLAY 'ERRO DB2: ' WS-SQLCODE
                 GOBACK
              END-IF
           END-IF
           .
           EXIT.
      *-------------------------
       20003-CALCULA-MEDIA.
      *-------------------------
           COMPUTE WS-MEDIA =
                 ( NOTA1 OF DCLALUNO +
                   NOTA2 OF DCLALUNO +
                   NOTA3 OF DCLALUNO +
                   NOTA4 OF DCLALUNO ) / 4
           END-COMPUTE
           DISPLAY 'MEDIA DO ALUNO: ' FD-MATRICULA '-' WS-MEDIA
           .
           EXIT.
      **************************************************************
      * TRATA LEITURA DO ARQUIVO DE ENTRADA                        *
      **************************************************************
       LER-ARQUIVO.
      *----------------

           READ ALUNO
                 AT END
                    MOVE 'S' TO WS-FIM-ARQUIVO
                 NOT AT END
                    CONTINUE
           END-READ
           .
      **************************************************************
      * FINAL DO PROCESSAMENTO                                     *
      **************************************************************
       30000-FIN.
      *---------.

           CLOSE ALUNO

           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           STRING WS-DIA '/'
                  WS-MES '/'
                  WS-ANO ' - '
                  WS-HH  ':'
                  WS-MM  ':'
                  WS-SS
               DELIMITED BY SIZE INTO WS-DATA-HORA
           END-STRING

           DISPLAY 'DATA/HORA FIM   : ' WS-DATA-HORA
           GOBACK.
