       IDENTIFICATION DIVISION.
       PROGRAM-ID. TICTACTOE.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01 GAMEBOARD PIC X(9) VALUES ".........".
           01 PLAYERS PIC A(2) VALUES "XO".
           01 EMPTYCELL PIC X(1) VALUES '.'.
           01 CURRENTPLAYER PIC 9(1) VALUES 0.
               88 ISPLAYER VALUES 1.
           01 LOOPCOUNTER PIC 9(1) VALUES 1.
           01 LOOPCOUNTER2 PIC 9(1).
           01 LOOPCOUNTER3 PIC 9(1).
           01 WINNER PIC S9(1) VALUES -1.
           01 PLAYROW PIC S9(1) VALUES -1.
           01 PLAYCOL PIC S9(1) VALUES -1.
           01 PLAYIND PIC S9(1) VALUES -1.
           01 POWERSHELL PIC X(255).

       PROCEDURE DIVISION.
           Abomination.
               MOVE 'powershell -c "get-command"' TO POWERSHELL
               CALL 'SYSTEM' USING POWERSHELL.
           ShowBoard.
               DISPLAY "-----"
               DISPLAY "|"GAMEBOARD(1:3)"|"
               DISPLAY "|"GAMEBOARD(4:3)"|"
               DISPLAY "|"GAMEBOARD(7:3)"|"
               DISPLAY "-----"
               DISPLAY " "
               GO TO CheckWin.

           CheckWin.
               MOVE 1 TO LOOPCOUNTER
               IF WINNER < 0
                   PERFORM ColumnCheck
               END-IF

               IF WINNER < 0
                   MOVE 1 TO LOOPCOUNTER
                   PERFORM RowCheck
               END-IF

               IF WINNER < 0
                   MOVE 1 TO LOOPCOUNTER
                   PERFORM DiagCheck
               END-IF

               IF WINNER < 0
                   PERFORM NextPlayer
                   GO TO PlayRound
               ELSE
                   DISPLAY "--------------------------"
                   DISPLAY "WINNER:"PLAYERS(WINNER:1)
                   DISPLAY "WINNING INDICES:"
                   DISPLAY LOOPCOUNTER" "LOOPCOUNTER2" "LOOPCOUNTER3
                   DISPLAY "--------------------------"
                   GO TO Endpoint
               END-IF.

           NextPlayer.
               MOVE -1 TO PLAYROW
               MOVE -1 TO PLAYCOL
               MOVE -1 TO PLAYIND
               ADD 1 TO CURRENTPLAYER
               IF CURRENTPLAYER > 2
                   MOVE 1 TO CURRENTPLAYER
               END-IF.

           PlayRound.
               DISPLAY " "
               DISPLAY "CURRENT PLAYER:"PLAYERS(CURRENTPLAYER:1)
               DISPLAY " "
               DISPLAY "PLEASE ENTER THE ROW(1-3)"
               ACCEPT PLAYROW
               IF PLAYROW < 4 AND PLAYROW > 0
                   DISPLAY "ENTERED ROW:"PLAYROW
                   DISPLAY " "
                   DISPLAY "PLEASE ENTER THE COLUMN (1-3)"
                   ACCEPT PLAYCOL

                   IF PLAYCOL < 4 AND PLAYCOL > 0
                       DISPLAY "ENTERED COLUMN:"PLAYCOL
                       DISPLAY " "
                       SUBTRACT 1 FROM PLAYROW GIVING PLAYIND
                       MULTIPLY PLAYIND BY 3 GIVING PLAYIND
                       ADD PLAYIND TO PLAYCOL GIVING PLAYIND
                       IF GAMEBOARD(PLAYIND:1) = EMPTYCELL(1:1)
                   MOVE PLAYERS(CURRENTPLAYER:1) TO GAMEBOARD(PLAYIND:1)
                           GO TO ShowBoard
                       END-IF
                   END-IF
               END-IF
           DISPLAY "INVALID ("PLAYROW":"PLAYCOL"). PLEASE TRY AGAIN."
              DISPLAY " "
               GO TO PlayRound.

           ColumnCheck.
               ADD LOOPCOUNTER TO 3 GIVING LOOPCOUNTER2
               ADD LOOPCOUNTER2 TO 3 GIVING LOOPCOUNTER3
               IF GAMEBOARD(LOOPCOUNTER:1) = GAMEBOARD(LOOPCOUNTER2:1)  -
               AND GAMEBOARD(LOOPCOUNTER2:1) = GAMEBOARD(LOOPCOUNTER3:1)-
               AND GAMEBOARD(LOOPCOUNTER:1) NOT = EMPTYCELL
                   MOVE CURRENTPLAYER TO WINNER
               ELSE
                   IF LOOPCOUNTER < 3
                       ADD 1 TO LOOPCOUNTER
                       GO TO ColumnCheck
                   END-IF
               END-IF.

           RowCheck.
               ADD LOOPCOUNTER TO 1 GIVING LOOPCOUNTER2
               ADD LOOPCOUNTER2 TO 1 GIVING LOOPCOUNTER3
               IF GAMEBOARD(LOOPCOUNTER:1) = GAMEBOARD(LOOPCOUNTER2:1)  -
               AND GAMEBOARD(LOOPCOUNTER2:1) = GAMEBOARD(LOOPCOUNTER3:1)-
               AND GAMEBOARD(LOOPCOUNTER:1) NOT = EMPTYCELL
                   MOVE CURRENTPLAYER TO WINNER
               ELSE
                   IF LOOPCOUNTER < 7
                       ADD 3 TO LOOPCOUNTER GIVING LOOPCOUNTER
                       GO TO RowCheck
                   END-IF
               END-IF.

           DiagCheck.
               ADD LOOPCOUNTER TO 4 GIVING LOOPCOUNTER2
               ADD LOOPCOUNTER2 TO 4 GIVING LOOPCOUNTER3
               IF GAMEBOARD(LOOPCOUNTER:1) = GAMEBOARD(LOOPCOUNTER2:1)  -
               AND GAMEBOARD(LOOPCOUNTER2:1) = GAMEBOARD(LOOPCOUNTER3:1)-
               AND GAMEBOARD(LOOPCOUNTER:1) NOT = EMPTYCELL
                   MOVE CURRENTPLAYER TO WINNER
               ELSE
                   MOVE 3 TO LOOPCOUNTER
                   ADD LOOPCOUNTER TO 2 GIVING LOOPCOUNTER2
                   ADD LOOPCOUNTER2 TO 2 GIVING LOOPCOUNTER3
                 IF GAMEBOARD(LOOPCOUNTER:1) = GAMEBOARD(LOOPCOUNTER2:1) -
               AND GAMEBOARD(LOOPCOUNTER2:1) = GAMEBOARD(LOOPCOUNTER3:1) -
               AND GAMEBOARD(LOOPCOUNTER:1) NOT = EMPTYCELL
                       MOVE CURRENTPLAYER TO WINNER
                   END-IF
               END-IF.


           EndPoint.
           STOP RUN.
       END PROGRAM TICTACTOE.
