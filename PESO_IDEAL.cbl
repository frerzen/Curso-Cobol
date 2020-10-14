      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PESO_IDEAL.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               CONSOLE IS SEXO-INPUT.
               CONSOLE IS ALT-INPUT.

      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       77  WS-SEXO       PIC X(01) VALUE ' '.
       77  WS-ALTURA     PIC 9(01)V99 VALUE ZERO.
       77  WS-PESO-IDEAL PIC 9(02)V99 VALUE ZERO.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY 'INFORME SEU SEXO "M" OU "F":  '
            ACCEPT WS-SEXO FROM SEXO-INPUT

            DISPLAY 'INFORME SUA ALTURA EM cm: '
            ACCEPT WS-ALTURA FROM ALT-INPUT

            IF WS-SEXO = 'M'
               COMPUTE WS-PESO-IDEAL = (WS-ALTURA * 0.95) - 95
               DISPLAY 'VOCE EH HOMEM SEU PESO IDEAL EH: 'WS-PESO-IDEAL

            ELSE
               COMPUTE WS-PESO-IDEAL = (WS-ALTURA * 0.85) - 85
               DISPLAY 'VOCE EH MULHER SEU PESO IDEAL EH: 'WS-PESO-IDEAL
            END-IF

            STOP RUN.
       END PROGRAM PESO_IDEAL.
