      ******************************************************************
      * Author: EMERSON
      * Date: 12-10-2020
      * Purpose: DESAFIO FINAL CURSO COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION                          DIVISION.
       PROGRAM-ID. HABITANTE2.
      *----------------------------------------------------------------*
       ENVIRONMENT                             DIVISION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                            SECTION.

           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT                             SECTION.

       FILE-CONTROL.

       SELECT ARQHAB ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.

      *----------------------------------------------------------------*
       DATA                                     DIVISION.
      *----------------------------------------------------------------*
       FILE                                     SECTION.

       FD ARQHAB
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "HABITANTES.TXT".

       01  REG-HABITANTE.
           03 FD-IDADE                     PIC 9(03).
           03 FD-SEXO                      PIC X(01).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  WS-EOF                          PIC X(01) VALUE ' '.
       77  CONT-CRIAN-F                    PIC 9(03) VALUE ZEROS.
       77  CONT-ADULTO                     PIC 9(03) VALUE ZEROS.
       77  CONT-ADS-H                      PIC 9(03) VALUE ZEROS.
       77  WS-SEXO                         PIC X(01) VALUE ' '.
       77  WS-IDADE                        PIC 9(03) VALUE ZEROS.
       77  CONT-PESSOA                     PIC 9(03) VALUE ZEROS.
       77  WS-PERC-ADS                  PIC 9(03)V99 VALUE ZEROS.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           MAIN-PROCEDURE.
            OPEN INPUT ARQHAB

            PERFORM 1000-LER-ARQUIVO

            PERFORM UNTIL WS-EOF = 'S'

               PERFORM 2000-TRATA-HABITANTE
               PERFORM 1000-LER-ARQUIVO

            END-PERFORM

            PERFORM 3000-RESULTADO

            CLOSE ARQHAB
            STOP RUN.


      *----------------------------------------------------------------*
       1000-LER-ARQUIVO.
      *----------------------------------------------------------------*
           READ ARQHAB
               AT END
                   MOVE 'S' TO WS-EOF
               NOT AT END
                   CONTINUE
           END-READ
           .
           EXIT.
      *----------------------------------------------------------------*
       2000-TRATA-HABITANTE.
      *----------------------------------------------------------------*
           MOVE FD-IDADE TO WS-IDADE
           MOVE FD-SEXO  TO WS-SEXO
           ADD 1 TO CONT-PESSOA

           EVALUATE WS-IDADE

               WHEN < 14
                   IF WS-SEXO = 'F'
                       ADD 1 TO CONT-CRIAN-F
               WHEN >= 15 AND <= 17

                       ADD 1 TO CONT-ADS-H
               WHEN >= 18
                   IF WS-SEXO = 'M'
                       ADD 1 TO CONT-ADULTO

           END-EVALUATE
           .
           EXIT.
      *----------------------------------------------------------------*
       3000-RESULTADO.
      *----------------------------------------------------------------*
           COMPUTE WS-PERC-ADS = (CONT-ADS-H * 100) / CONT-PESSOA

           DISPLAY 'A QTD DE CRIANCAS DO SEXO FEMININO: 'CONT-CRIAN-F

           DISPLAY 'A QTD DE ADULTOS MASCULINOS: 'CONT-ADS-H

           DISPLAY 'O PERCENTUAL DE ADOLESCENTES: 'WS-PERC-ADS'%'

           EXIT.

       END PROGRAM HABITANTE2.
