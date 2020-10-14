      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUESTAO2.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               CONSOLE IS LETRA-INPUT.
      *----------------------------------------------------------------*

       DATA DIVISION.
      *----------------------------------------------------------------*

       WORKING-STORAGE SECTION.

       77  WS-LETRA PIC X(01).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY 'USUARIO DIGITE UMA LETRA: '
            ACCEPT WS-LETRA FROM LETRA-INPUT

            IF WS-LETRA = 'A' OR WS-LETRA = 'E' OR WS-LETRA = 'I' OR
               WS-LETRA = 'O' OR WS-LETRA = 'U'
                   DISPLAY 'EH UMA VOGAL'
            ELSE
                   DISPLAY 'EH UMA CONSOANTE'
            STOP RUN.
       END PROGRAM QUESTAO2.
