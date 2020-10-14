      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIFERENCA.
      *----------------------------------------------------------------*
       ENVIRONMENT  DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               CONSOLE IS NUM1-INPUT.
               CONSOLE IS NUM2-INPUT.
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       77  WS-NUM1  PIC 9(03) VALUE ZERO.
       77  WS-NUM2  PIC 9(03) VALUE ZERO.
       77  WS-DIF   PIC 9(03) VALUE ZERO.
       77  WS-FLAG  PIC X(01) VALUE ' '.
       PROCEDURE DIVISION.


       MAIN-PROCEDURE.

            PERFORM 1000-LER-ENTRADA

            PERFORM UNTIL WS-FLAG = 'S'
               DISPLAY 'OUCH'

               IF WS-NUM1 = WS-NUM2
                   DISPLAY 'USUARIO DIGITE DOIS NUMEROS DIFERENTES.'
                   PERFORM 1000-LER-ENTRADA
               END-IF
               END-PERFORM


                   IF WS-NUM1 > WS-NUM2
                       COMPUTE WS-DIF = WS-NUM1 - WS-NUM2
                       DISPLAY 'A DIFERENCA ENTRE OS NUMEROS EH: 'WS-DIF
                   ELSE
                       IF WS-NUM2 > WS-NUM1
                           COMPUTE WS-DIF = WS-NUM2 - WS-NUM2
                          DISPLAY 'A DIFERENCA ENTRE NUMEROS EH: 'WS-DIF
                       END-IF

                   END-IF



            STOP RUN.

      *----------------------------------------------------------------*
       1000-LER-ENTRADA.
      *----------------------------------------------------------------*
               DISPLAY'USUARIO DIGITE UM NUMERO: '
               ACCEPT WS-NUM1 FROM NUM1-INPUT

               DISPLAY'USUARIO DIGITE OUTRO NUMERO: '
               ACCEPT WS-NUM2 FROM NUM2-INPUT

               IF WS-NUM1 > WS-NUM2 OR WS-NUM2 > WS-NUM1
                  MOVE 'S' TO WS-FLAG
               END-IF


           EXIT.
       END PROGRAM DIFERENCA.
