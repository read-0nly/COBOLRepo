IDENTIFICATION DIVISION.
PROGRAM-ID. TICTACTOE.
DATA DIVISION.
LOCAL-STORAGE SECTION.
    *The 3x3 grid is encoded in a single line of text just to keep it simple - it does make some things harder though like figuring out the cell index
    01 GAMEBOARD PIC X(9) VALUES ".........".
    *Player chars
    01 PLAYERS PIC A(2) VALUES "XO".
    *Must match symbol used as empty cell in gameboard
    01 EMPTYCELL PIC X(1) VALUES '.'.
    *The current player - either 1 or 2 by default. First round adds 1 to 0 giving control to player 1
    01 CURRENTPLAYER PIC 9(1) VALUES 0.
        *useful for computer player scenario later
        88 ISPLAYER VALUES 1.
    *Loop counters to check win condition - one for the index of each cell checked in a possible win
    01 LOOPCOUNTER PIC 9(1) VALUES 1.
    01 LOOPCOUNTER2 PIC 9(1).
    01 LOOPCOUNTER3 PIC 9(1).
    *Holds the winner when someone wins, otherwise is -1
    01 WINNER PIC S9(1) VALUES -1.
    *Holds the entry of the user, otherwise -1
    01 PLAYROW PIC S9(1) VALUES -1.
    01 PLAYCOL PIC S9(1) VALUES -1.
    *calculates the 1d index of the 2d board (((playrow-1)*3)+playcol)
    01 PLAYIND PIC S9(1) VALUES -1.
    
PROCEDURE DIVISION.
*The overall flow here is: Display the board state > Check if the last play was a win > If not, play round (until a valid play is submitted). Otherwise end > Display the new board state
    ShowBoard.
    *Displays the current board state as a 3x3 grid
        DISPLAY "-----"
        DISPLAY "|"GAMEBOARD(1:3)"|"
        DISPLAY "|"GAMEBOARD(4:3)"|"
        DISPLAY "|"GAMEBOARD(7:3)"|"
        DISPLAY "-----"
        DISPLAY " ".
    CheckWin.
    *Checks if there's a win - vertically, horizontally, then diagonally.
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
        ELSE
            DISPLAY "--------------------------"
            DISPLAY "WINNER:"PLAYERS(WINNER:1)
            DISPLAY "WINNING INDICES:"
            DISPLAY LOOPCOUNTER" "LOOPCOUNTER2" "LOOPCOUNTER3
            DISPLAY "--------------------------"
            GO TO Endpoint
        END-IF.
    PlayRound.
    *Gets the player input for the next round, validates it and repeats till valid, translates it to an index, updates the board then loops back.
        MOVE -1 TO PLAYROW
        MOVE -1 TO PLAYCOL
        MOVE -1 TO PLAYIND
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
            
            IF PLAYROW < 4 AND PLAYROW > 0
                DISPLAY "ENTERED COLUMN:"PLAYCOL
                DISPLAY " "
                *This is the shuffle from row:col to index - the math of it is (row-1)*3+col.
                SUBTRACT 1 FROM PLAYROW GIVING PLAYIND
                MULTIPLY PLAYIND BY 3 GIVING PLAYIND
                ADD PLAYIND TO PLAYCOL GIVING PLAYIND
                IF GAMEBOARD(PLAYIND:1) = EMPTYCELL(1:1)
                    MOVE PLAYERS(CURRENTPLAYER:1) TO GAMEBOARD(PLAYIND:1)
                    GO TO ShowBoard
                END-IF
            END-IF
        END-IF
        DISPLAY "INVALID PLAY ("PLAYROW":"PLAYCOL"). PLEASE TRY AGAIN."
        DISPLAY " "
        GO TO PlayRound.
    
    NextPlayer.
    *switches between 1 and 2, keeping it within bounds
        ADD 1 TO CURRENTPLAYER
        IF CURRENTPLAYER > 2 OR CURRENTPLAYER < 1
            MOVE 1 TO CURRENTPLAYER
        END-IF.
        
    ColumnCheck.
    *For the first three characters, if they match the char offset+3 and offset+6 and also aren't an empty cell, declare a win
        ADD LOOPCOUNTER TO 3 GIVING LOOPCOUNTER2
        ADD LOOPCOUNTER2 TO 3 GIVING LOOPCOUNTER3
        IF GAMEBOARD(LOOPCOUNTER:1) = GAMEBOARD(LOOPCOUNTER2:1) AND GAMEBOARD(LOOPCOUNTER2:1) = GAMEBOARD(LOOPCOUNTER3:1) AND GAMEBOARD(LOOPCOUNTER:1) NOT = EMPTYCELL
            MOVE CURRENTPLAYER TO WINNER
        ELSE
            IF LOOPCOUNTER < 3
                *increment column
                ADD 1 TO LOOPCOUNTER
                *Do the next column
                GO TO ColumnCheck
            END-IF
        END-IF.
        
    RowCheck.
    *For the first character and every offset+3 after, if that character and offset+1 and offset+2 match and aren't empty, declare a win
        ADD LOOPCOUNTER TO 1 GIVING LOOPCOUNTER2
        ADD LOOPCOUNTER2 TO 1 GIVING LOOPCOUNTER3
        IF GAMEBOARD(LOOPCOUNTER:1) = GAMEBOARD(LOOPCOUNTER2:1) AND GAMEBOARD(LOOPCOUNTER2:1) = GAMEBOARD(LOOPCOUNTER3:1) AND GAMEBOARD(LOOPCOUNTER:1) NOT = EMPTYCELL
            MOVE CURRENTPLAYER TO WINNER
        ELSE
            IF LOOPCOUNTER < 7
                *increment row
                ADD 3 TO LOOPCOUNTER GIVING LOOPCOUNTER
                *Do the next row
                GO TO RowCheck
            END-IF
        END-IF.
    
    DiagCheck.
    *For the first cell offset+4 offset+8 or the third cell offset+2 offset+4, declare a win (the diagonals)
        ADD LOOPCOUNTER TO 4 GIVING LOOPCOUNTER2
        ADD LOOPCOUNTER2 TO 4 GIVING LOOPCOUNTER3
        IF GAMEBOARD(LOOPCOUNTER:1) = GAMEBOARD(LOOPCOUNTER2:1) AND GAMEBOARD(LOOPCOUNTER2:1) = GAMEBOARD(LOOPCOUNTER3:1) AND GAMEBOARD(LOOPCOUNTER:1) NOT = EMPTYCELL
            MOVE CURRENTPLAYER TO WINNER
        ELSE
            MOVE 3 TO LOOPCOUNTER
            ADD LOOPCOUNTER TO 2 GIVING LOOPCOUNTER2
            ADD LOOPCOUNTER2 TO 2 GIVING LOOPCOUNTER3 
            IF GAMEBOARD(LOOPCOUNTER:1) = GAMEBOARD(LOOPCOUNTER2:1) AND GAMEBOARD(LOOPCOUNTER2:1) = GAMEBOARD(LOOPCOUNTER3:1) AND GAMEBOARD(LOOPCOUNTER:1) NOT = EMPTYCELL
                MOVE CURRENTPLAYER TO WINNER
            END-IF
        END-IF.
        
    
    EndPoint.
    STOP RUN.
END PROGRAM TICTACTOE.
