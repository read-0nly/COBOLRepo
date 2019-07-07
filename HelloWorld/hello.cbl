
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MainProgram.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 SharedItem     PIC X(40) IS GLOBAL.
       PROCEDURE DIVISION.
       Begin.
       CALL "HelloWorld"
       DISPLAY SharedItem
       STOP RUN.

           IDENTIFICATION DIVISION.
           PROGRAM-ID. HelloWorld.
           PROCEDURE DIVISION.
           Begin.
           MOVE "Hello World" TO SharedItem
           EXIT PROGRAM.
           END PROGRAM HelloWorld.

       END PROGRAM MainProgram.
