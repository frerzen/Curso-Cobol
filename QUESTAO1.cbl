      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUESTAO1.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES.
                   CONSOLE IS NUM1-INPUT.
                   CONSOLE IS NUM2-INPUT.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-NUM1  PIC 9(03).
       77  WS-NUM2  PIC 9(03).
       77  WS-MAIOR PIC 9(03) VALUE ZEROS.
       77  WS-MENOR PIC 9(03) VALUE ZEROS.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       MAIN-PROCEDURE.
           DISPLAY 'USUARIO DIGITE O PRIMEIRO NUMERO:'
           ACCEPT  WS-NUM1 FROM NUM1-INPUT

           DISPLAY 'USUARIO DIGITE O SEGUNDO NUMERO:'
           ACCEPT WS-NUM2 FROM NUM2-INPUT

           IF WS-NUM1 = WS-NUM2
               DISPLAY 'OS DOIS NUMEROS SAO IGUAIS'
           ELSE
               IF WS-NUM1 > WS-NUM2
                   COMPUTE WS-MAIOR = WS-NUM1
                   COMPUTE WS-MENOR = WS-NUM2

               ELSE
                   COMPUTE WS-MAIOR = WS-NUM2
                   COMPUTE WS-MENOR = WS-NUM1
               END-IF
           END-IF

           IF WS-MAIOR <> 0 AND WS-MENOR <> 0

               DISPLAY 'O MAIOR NUMERO E: 'WS-MAIOR
               DISPLAY 'O MENOR NUMERO E: 'WS-MENOR
           END-IF


            STOP RUN.
       END PROGRAM QUESTAO1.
