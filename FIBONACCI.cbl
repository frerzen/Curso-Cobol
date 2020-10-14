      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.
      *----------------------------------------------------------------*
       ENVIRONMENT   DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               CONSOLE IS N-INPUT.
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       77  WS-NTERMOS  PIC 9(03) VALUE ZERO.
       77  WS-SOMA     PIC 9(04) VALUE ZERO.
       77  WS-ANTERIOR      PIC 9(04) VALUE 1.
       77  WS-ANS     PIC 9(04) VALUE ZERO.
       77  CONT        PIC 9(04) VALUE 1.
       77  WS-PROX     PIC 9(04) VALUE 1.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY 'USUARIO DIGITE O NUMERO DE TERMOS A SER SOMADO: '
            ACCEPT WS-NTERMOS FROM N-INPUT


            IF WS-NTERMOS = 1
               COMPUTE WS-SOMA = 1
            ELSE
               IF WS-NTERMOS = 2
                   COMPUTE WS-SOMA = 2
               END-IF
            END-IF


               PERFORM UNTIL CONT > WS-NTERMOS
                   COMPUTE WS-SOMA = WS-ANTERIOR + WS-PROX
                   COMPUTE WS-ANS = WS-ANS + WS-SOMA
                   DISPLAY WS-ANS
                   MOVE WS-PROX TO WS-ANTERIOR
                   MOVE WS-SOMA TO WS-PROX

                   ADD 1 TO CONT
               END-PERFORM


            DISPLAY WS-ANS
            STOP RUN.
       END PROGRAM FIBONACCI.
