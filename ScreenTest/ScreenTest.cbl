      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 black   constant as 0.
       01 blue    constant as 1.
       01 green   constant as 2.
       01 cyan    constant as 3.
       01 red     constant as 4.
       01 magenta constant as 5.
       01 brown   constant as 6.
       01 white   constant as 7.
       01 acpt-key         PIC X    VALUE SPACES.
       01 pntr usage pointer.
       01 player.
           05 playerX pic 9(2) value 3.
           05 playerY pic 9(2) value 3.
           05 playerHealth pic 9(3) value 100.
       01 width constant as 20.
       01 height constant as 10.

       01 godEye.
           05 godEyeX pic 9(3) value 1.
           05 godEyeY pic 9(3) value 1.
           05 godMaxX pic 9(3).
           05 godMaxY pic 9(3).

       01 glyphs.
           05 glyphWall pic x value "#".
           05 glyphFloor pic x value "+".
           05 glyphPlayer pic x value "U".
           05 glyphSlime pic x value "o".
           05 glyphVoid pic x value "@".
       01 firstRow OCCURS height TIMES.
           05 firstCol usage POINTER value null occurs width times.
       01 secondRow OCCURS height TIMES.
           05 secondCol  usage POINTER value null occurs width times.
       01 thirdRow OCCURS height TIMES.
           05 thirdCol usage POINTER value null occurs width times.
       01 gameboardRow OCCURS height TIMES.
           05 gameboardCol pic x value " " occurs width times.

       LINKAGE SECTION.
       01 godChar pic x(1) value null.
       01 RowCurrent.
           05 RowCurrentColumn pic x value " " occurs width times.
       01 ColCurrent pic x.
       SCREEN SECTION.
       01 gameMap.
          05 LINE 3 COL 25 PIC  X(20) FROM gameboardRow(1).
          05 LINE 4 COL 25 PIC  X(20) FROM gameboardRow(2).
          05 LINE 5 COL 25 PIC  X(20) FROM gameboardRow(3).
          05 LINE 6 COL 25 PIC  X(20) FROM gameboardRow(4).
          05 LINE 7 COL 25 PIC  X(20) FROM gameboardRow(5).
          05 LINE 8 COL 25 PIC  X(20) FROM gameboardRow(6).
          05 LINE 9 COL 25 PIC  X(20) FROM gameboardRow(7).
          05 LINE 10 COL 25 PIC  X(20) FROM gameboardRow(8).
          05 LINE 11 COL 25 PIC  X(20) FROM gameboardRow(9).
          05 LINE 12 COL 25 PIC  X(20) FROM gameboardRow(10).

       PROCEDURE DIVISION.
       initialization.
       add 1 to width giving godMaxX
       add 1 to height giving godMaxY
       PERFORM prepareMap
       .
       MAIN-PROCEDURE.
        PERFORM drawMap
        DISPLAY gameMap
      * set pntr to address of floorRow(playerX of player)
      * set address of RowCurrent to pntr
      * DISPLAY RowCurrent line 2
      * move glyphPlayer(1:1) to RowCurrentColumn(playerY)
      * DISPLAY RowCurrent line 3
        ACCEPT ACPT-KEY TIMEOUT AFTER 1 with auto

        GO TO MAIN-PROCEDURE.

       drawMap.
         move 1 to godEyeX
         move 1 to godEyeY
         PERFORM until godEyeX = height
          PERFORM until godEyeY = width
           if firstCol(godEyeX,godEyeY) = NULL then
            if secondCol(godEyeX,godEyeY) = NULL then
             if thirdCol(godEyeX,godEyeY) = NULL then
             ELSE
              SET ADDRESS OF godChar to thirdCol(godEyeX,godEyeY)
              move godChar to gameboardCol(godEyeX,godEyeY)
             END-IF
            ELSE
             SET ADDRESS OF godChar to secondCol(godEyeX,godEyeY)
             move godChar to gameboardCol(godEyeX,godEyeY)
            END-IF
           ELSE
             SET ADDRESS OF godChar to firstCol(godEyeX,godEyeY)
             move godChar to gameboardCol(godEyeX,godEyeY)
           end-if
           add 1 to godEyeY
          END-PERFORM
          move 1 to godEyeY
          add 1 to godEyeX
         END-PERFORM
         move 1 to godEyeX
         move 1 to godEyeY
         set address of godChar to NULL
       .
       prepareMap.
         move address of glyphWall to secondCol(1,1)
         move address of glyphPlayer to secondCol(3,3)
         move address of glyphSlime to secondCol(5,5)
         move address of glyphFloor to firstCol(6,6)
         move address of glyphFloor to firstCol(7,7)

       .
       ENN.
        STOP RUN.
        END PROGRAM YOUR-PROGRAM-NAME.
