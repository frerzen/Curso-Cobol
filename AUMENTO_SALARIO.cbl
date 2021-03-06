  *+--------------------------------------------------------------+
      * MOTIVO     : APRESENTACAO                                     *
      * AUTOR      : THULIO MARCIO SILVA RIBEIRO                      *
      * DATA       : 21-08-2020                                       *
      * DESCRIPCION: PROGRAMA COBOL + DB2                             *
      *+--------------------------------------------------------------+
       IDENTIFICATION DIVISION.
      *-----------------------.
       PROGRAM-ID.   SALARIO.
       AUTHOR EMERSON.
       DATE-WRITTEN. OCT 2020.
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
           SELECT SALARIO    ASSIGN  TO  ARQJCL.
      *
      *+---------------------------------------------------------------+00003900
      *                      DATA   DIVISION                            00004000
      *                      ===============                            00004100
      *+---------------------------------------------------------------+00004200
       DATA DIVISION.                                                   00004400
      *--------------                                                   00004500
       FILE SECTION.                                                    00004700
      *
       FD  FUNCIONARIO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "FUNCIONARIO.TXT".
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 33 CHARACTERS.
       01  REG-ENTRADA.
           03 FD-CODFUNCIONARIO PIC 9(03).
           03 FD-NOME           PIC X(10).
           03 FD-SALARIO        PIC 9(06)V99.


      *
      *+-----------------------------------------------------------+
      *                   WORKING-STORAGE SECTION                  *
      *                  =========================                 *
      *+-----------------------------------------------------------+
       WORKING-STORAGE SECTION.

       01  FILLER                        PIC X(25)
                VALUE 'INICIO WORKING EMERSON'.

      *+-----------------------------------------------------------+
      *  CONTADORES
      *+-----------------------------------------------------------+
       01  CONTADORES.
           03 CNT-LIN            PIC 9(03).

      *+-----------------------------------------------------------+
      *  VARIAVEIS DO PROGRAMA
      *+-----------------------------------------------------------+

       01  WS-EOF                PIC X(01).

       01  WS-VARIAVEIS.
           03WS-SALARIO-AUX           PIC 9(06)V99 VALUE ZEROS.
           03 WS-AUMENTO              PIC 9(06)V99 VALUE ZEROS.
           03 WS-CODFUNC-AUX

              05 WS-DATA.
                 07 WS-ANO               PIC 9(04).
                 07 WS-MES               PIC 9(02).
                 07 WS-DIA               PIC 9(02).
              05 WS-HORA.
                 07 WS-HH                PIC 9(02).
                 07 WS-MM                PIC 9(02).
                 07 WS-SS                PIC 9(02).
                 07 WS-MS                PIC 9(06).

       01  DCLFUNCIONARIO.
           03 IDFUNC                     PIC 9(03).
           03 NOME                       PIC X(10).
           03 SALARIO                    PIC 9(06)V99.

      *----------------------------------------------------------------*
      *AREA DE DEFINICAO DE TABELAS DB2 - INCLUS�O DE TABELAS          *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA  END-EXEC.
           EXEC SQL INCLUDE FUNCIONARIO  END-EXEC.
      *----------------------------------------------------------------*
      *01 DCLALUNO.
      *   03 MATRICULA PIC 9(03).
      *   03 NOTA1     PIC 9(03)V99.
      *   03 NOTA2     PIC 9(03)V99.
      *   03 NOTA3     PIC 9(03)V99.
      *   03 NOTA4     PIC 9(03)V99.
      *----------------------------------------------------------------*
       01  FILLER                        PIC X(25)
               VALUE 'FIM WORKING EMERSON'.

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
           STOP RUN.


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

           OPEN INPUT  ARQJCL
           .
           EXIT.

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

           PERFORM UNTIL WS-EOF = 'S'
              MOVE FD-CODFUNCIONARIO TO IDFUNC OF DCLFUNCIONARIO
              PERFORM 20001-VERIFICA-FUNCIONARIO
              IF WS-SQLCODE = 100
                 CONTINUE
              ELSE
                  IF WS-SQLCODE = 0

                       PERFORM 20003-CALCULA-AUMENTO
                       PERFORM 20004-ATUALIZA-SALARIO

                  END-IF
              END-IF
              PERFORM LER-ARQUIVO
           END-PERFORM

           .
      *-------------------------
       20001-VERIFICA-FUNCIONARIO.
      *-------------------------

           EXEC SQL
              SELECT
                  NOME
                , SALARIO
                INTO
                  DCLFUCNIIONARIO.NOME
                ,:DCLFUNCIONARIO.SALARIO

               FROM FUNCIONARIO
               WHERE
                IDFUNC = :DCLFUNCIONARIO.IDFUNC
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE

           .
           EXIT.

      *-------------------------
       20004-ATUALIZA-SALARIO.
      *-------------------------
           MOVE FD-SALARIO         TO SALARIO   OF DCLFUNCIONARIO
           MOVE FD-NOME            TO NOME      OF DCLFUCNIONARIO
           MOVE FD-CODFUNCIONARIO  TO IDFUNC    OF DCLFUNCIONARIO

           EXEC SQL
              UPDATE FUNCIONARIO
                 SET  SALARIO = :DCLFUNCIONARIO.SALARIO

                 WHERE
                  IDFUNC = :DCLFUNCIONARIO.IDFUNC
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE
           IF WS-SQLCODE = 100
              DISPLAY 'SALARIO NAO ATUALIZADO'
           ELSE
              IF WS-SQLCODE = 0
                 DISPLAY 'SALARIO ATUALIZADO COM SUCESSO'
                 DISPLAY 'O SALARIO ANTIGO ERA: R$' WS-SALARIO-AUX
                 DISPLAY 'O NOVO SALARIO E: R$' FD-SALARIO
              ELSE
                 DISPLAY 'ERRO DB2: ' WS-SQLCODE
                 GOBACK
              END-IF
           END-IF
           .
           EXIT.

      *-------------------------
       20003-CALCULA-AUMENTO.
      *-------------------------
           MOVE FD-SALARIO TO WS-SALARIO-AUX
           COMPUTE WS-AUMENTO =
                 ( SALARIO OF DCLFUNCIONARIO * 0,1 ) +
                   SALARIO OF DCLFUNCIONARIO
           END-COMPUTE
           MOVE WS-AUMENTO TO FD-SALARIO
           .
           EXIT.
      **************************************************************
      * TRATA LEITURA DO ARQUIVO DE ENTRADA                        *
      **************************************************************
       LER-ARQUIVO.
      *----------------

           READ ARQJCL
                 AT END
                    MOVE 'S' TO WS-EOF
                 NOT AT END
                    CONTINUE
           END-READ
           .
      **************************************************************
      * FINAL DO PROCESSAMENTO                                     *
      **************************************************************
       30000-FIN.
      *---------.

           CLOSE ARQJCL

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
