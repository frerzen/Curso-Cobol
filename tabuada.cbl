********
      * Author: EMERSON
      * Date: 11-10-2020
      * Purpose: DESAFIO FINAL CURSO COBOL
      * Tectonics: cobc
      ********
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABUADA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  VARIAVEIS.
           03 WS-NUMA           PIC 9(02).
           03 WS-NUMB           PIC 9(02).
           03 WS-RESPOSTA       PIC 9(03).
           03 WS-LINHABRANCA2   PIC X(52) VALUE SPACES.

           03 WS-LINHA1.
               05 FILLER           PIC X(20) VALUE 'TABUADA DO NUMERO:'.
               05 WS-NUMAUX        PIC 9(02).



       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
           MOVE 1 TO WS-NUMA
           MOVE 0 TO WS-NUMB
           MOVE 1 TO WS-NUMAUX


           PERFORM UNTIL WS-NUMA > 10
                   MOVE WS-NUMA TO WS-NUMAUX
                   DISPLAY WS-LINHA1
                   DISPLAY WS-LINHABRANCA2


                   PERFORM UNTIL WS-NUMB > 10
                       COMPUTE WS-RESPOSTA = WS-NUMA * WS-NUMB


                       DISPLAY WS-NUMA ' X ' WS-NUMB  '= ' WS-RESPOSTA
                       ADD 1 TO WS-NUMB
                   END-PERFORM

               DISPLAY WS-LINHABRANCA2
               ADD 1 TO WS-NUMA
               MOVE 0 TO WS-NUMB
           END-PERFORM

           STOP RUN.
       END PROGRAM TABUADA.
