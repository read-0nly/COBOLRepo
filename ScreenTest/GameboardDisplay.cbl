      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GameboardDisplay.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *initInfo
       01 black   constant as 0.
       01 blue    constant as 1.
       01 green   constant as 2.
       01 cyan    constant as 3.
       01 red     constant as 4.
       01 magenta constant as 5.
       01 brown   constant as 6.
       01 white   constant as 7.
       01 width constant as 20.
       01 height constant as 10.
       01 slimeFreq constant as 5.
       01 floater pic 999.9999 value ZEROES.
       01 inter pic 9(1) value 0.
      *inputKey
       01 acpt-key PIC X VALUE SPACES.
      *Entities
       01 player.
           05 playerX pic 9(3) value 3.
           05 playerY pic 9(3) value 3.
           05 playerPrevX pic 9(3) value 3.
           05 playerPrevY pic 9(3) value 3.
           05 playerHealth pic 9(3) value 100.
           05 playerHeading pic 9(1) value 2.
       01 swordStrike.
           05 swordX pic 9(3) value 999.
           05 swordY pic 9(3) value 999.
           05 swordHeading pic 9(1) value 0.
           05 swordDamage pic 9(3) value 10.
       01 slime occurs slimeFreq.
           05 slimeX pic 9(3) value 5.
           05 slimeY pic 9(3) value 6.
           05 slimePrevX pic 9(3) value 3.
           05 slimePrevY pic 9(3) value 3.
           05 slimeHealth pic 9(3) value 100.
       01 slimeIndex pic 9(1) value 1.
       01 slimeMax pic 9(1) value 6.
       01 godEye.
           05 godEyeX pic 9(3) value 1.
           05 godEyeY pic 9(3) value 1.
           05 godMaxX pic 9(3).
           05 godMaxY pic 9(3).
      *mapGlyphs
       01 glyphs.
        05 glyphPlayerN PIC X VALUE "A".
        05 glyphPlayerS PIC X VALUE "V".
        05 glyphPlayerW PIC X VALUE "<".
        05 glyphPlayerE PIC X VALUE ">".
        05 glyphPlayerShield PIC X VALUE "U".
        05 glyphWall PIC X VALUE "#".
        05 glyphFloor PIC X VALUE "_".
        05 glyphSlime PIC X VALUE "o".
        05 glyphSwordNS pic X value "|".
        05 glyphSwordEW pic X value "-".
        05 glyphSword pic x occurs 2 times.
        05 glyphPlayer pic X occurs 5 times.
      *mapLayers
       01 firstRow OCCURS height TIMES.
           05 firstCol usage POINTER value null occurs width times.
       01 secondRow OCCURS height TIMES.
           05 secondCol  usage POINTER value null occurs width times.
       01 thirdRow OCCURS height TIMES.
           05 thirdCol usage POINTER value null occurs width times.
       01 gameboardRow OCCURS height TIMES.
           05 gameboardCol pic x value " " occurs width times.
       01 acpt-num pic 9(4).
      *turncounter
       01 turnCounter pic 9(38) value 0.

       LINKAGE SECTION.
      *topmostCharacter
       01 godChar pic x value null.
       SCREEN SECTION.
       01 blnkScrn blank screen.
       01 gameMap.
          05 LINE 3 COL 3 PIC  X(width) FROM gameboardRow(1).
          05 LINE 4 COL 3 PIC  X(width) FROM gameboardRow(2).
          05 LINE 5 COL 3 PIC  X(width) FROM gameboardRow(3).
          05 LINE 6 COL 3 PIC  X(width) FROM gameboardRow(4).
          05 LINE 7 COL 3 PIC  X(width) FROM gameboardRow(5).
          05 LINE 8 COL 3 PIC  X(width) FROM gameboardRow(6).
          05 LINE 9 COL 3 PIC  X(width) FROM gameboardRow(7).
          05 LINE 10 COL 3 PIC  X(width) FROM gameboardRow(8).
          05 LINE 11 COL 3 PIC  X(width) FROM gameboardRow(9).
          05 LINE 12 COL 3 PIC  X(width) FROM gameboardRow(10).
          05 LINE 13 COL 3 PIC  X(width) FROM acpt-key.
          05 LINE 13 COL 5 PIC  X(width) FROM playerX.
          05 LINE 13 COL 8 PIC  X(width) FROM playerY.
          05 LINE 13 COL 12 PIC  X(width) FROM glyphPlayer(1).
          05 LINE 13 COL 13 PIC  X(width) FROM glyphPlayer(2).
          05 LINE 13 COL 14 PIC  X(width) FROM glyphPlayer(3).
          05 LINE 13 COL 15 PIC  X(width) FROM glyphPlayer(4).
          05 LINE 13 COL 16 PIC  X(width) FROM glyphPlayer(5).
          05 LINE 13 COL 20 PIC  X(width) FROM playerHeading.
          05 LINE 13 COL 22 PIC  X(width) FROM glyphPlayer(
           playerHeading).


       PROCEDURE DIVISION.
       initialization.
      * set bounds
        add 1 to height giving godMaxX
        add 1 to width giving godMaxY
      * set player glyphs
        move glyphPlayerShield to glyphPlayer(1)
        move glyphPlayerN to glyphPlayer(2)
        move glyphPlayerE to glyphPlayer(3)
        move glyphPlayerS to glyphPlayer(4)
        move glyphPlayerW to glyphPlayer(5)
        move glyphSwordNS to glyphSWord(1)
        move glyphSwordEW to glyphSWord(2)

      * random seed gets modified by last random result
        DISPLAY "Enter 4 digits to use as random seed" line 1
        ACCEPT ACPT-num TIMEOUT AFTER 10 with auto line 2
        display blnkScrn
      * Assign random position to slimes
        perform until slimeIndex = slimeMax
         move height to floater
         compute slimeX(slimeIndex) = (FUNCTION RANDOM(acpt-num) *
          (height - 2)) + 2
         move width to floater
         add acpt-num to acpt-num
         compute slimeY(slimeIndex) = (FUNCTION RANDOM(acpt-num) *
          (width - 2)) + 2
         add acpt-num to acpt-num
         add 1 to slimeIndex
        END-PERFORM
        move 1 to slimeIndex
      * Prepare map pointers for initial render
        PERFORM prepareMap
       .
       MAIN-PROCEDURE.
        PERFORM drawMap
        DISPLAY gameMap
        ACCEPT ACPT-KEY TIMEOUT AFTER 1 with auto
        if swordX < 999 and swordY < 999 then
         move null to secondCol(swordX,swordY)
         move 999 to swordX
         move 999 to swordY
        end-if
        add 1 to turncounter
        if acpt-key = "w" or
         acpt-key = "s" or
         acpt-key = "a" or
         acpt-key = "d" THEN
         perform playerMove
        else if acpt-key = "q" or
         acpt-key = "e" then
         perform playerAttack
        end-if.
       GO TO MAIN-PROCEDURE
       .
       drawMap.
        perform entityDraw
         move 1 to godEyeX
         move 1 to godEyeY
         PERFORM until godEyeX = godMaxX
          PERFORM until godEyeY = godMaxY
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
         move 1 to godEyeX
         move 1 to godEyeY
         PERFORM until godEyeX = godMaxX
          PERFORM until godEyeY = godMaxY
           move address of glyphFloor to thirdCol(godEyeX,godEyeY)
           if godEyeX = 1 or godEyeX = height then
            move address of glyphWall to secondCol(godEyeX,godEyeY)
           ELSE
            if godEyeY = 1 or godEyeY = width then
             move address of glyphWall to secondCol(godEyeX,godEyeY)
            ELSE
             if godEyeX = playerX and godEyeY = playerY
              move address of glyphPlayer(playerHeading) to secondCol(
               godEyeX,godEyeY)
             end-if
            END-IF
           END-IF
           perform until slimeIndex = slimeMax
            if slimeX(slimeIndex) = godEyeX and slimeY(slimeIndex) =
             godEyeY THEN
             move address of glyphSlime to secondCol(godEyeX,
              godEyeY)
            end-if
            add 1 to slimeIndex
           END-PERFORM
           move 1 to slimeIndex
           add 1 to godEyeY
          END-PERFORM
          move 1 to godEyeY
          add 1 to godEyeX
         END-PERFORM
         move 1 to godEyeX
         move 1 to godEyeY
         set address of godChar to NULL
       .
       entityDraw.
        move 1 to slimeIndex
        perform until slimeIndex = slimeMax
         move null to secondCol(slimex(slimeindex),
          slimey(slimeindex))
         if(slimeX(slimeIndex) = swordX
          and slimeY(slimeIndex) = swordY) THEN
          subtract swordDamage from slimeHealth(slimeIndex)
         END-IF
         if slimeHealth(slimeindex) > 1 THEN
          move address of glyphSlime to secondCol(slimex(slimeindex),
          slimey(slimeindex))
         end-IF
         add 1 to slimeIndex
        END-PERFORM
        if swordX < height and swordY < width
         move address of glyphSword(swordHeading) to secondCol(
          swordX,swordY)
        end-if
        move address of glyphPlayer(playerHeading) to secondCol(
         playerX,playerY)

       .
       playerMove.
        move playerX to playerPrevX
        move playerY to playerPrevY
        if acpt-key = "w" then
         subtract 1 from playerX
         move 2 to playerHeading
        else if acpt-key = "a" then
         subtract 1 from playerY
         move 5 to playerHeading
        else if acpt-key = "s" then
         add 1 to playerX
         move 4 to playerHeading
        else if acpt-key = "d" then
         add 1 to playerY
         move 3 to playerHeading
        end-if
        end-if
        end-if
        end-if

      *edge check
        if playerX < 1 or playerx > height THEN
          move playerPrevX to playerX

         else if  playerY < 1 or playerY > width THEN
          move playerPrevY to playerY
         END-IF
         END-IF
      *collision check
        if secondCol(playerX,playerY) = NULL then
         move null to secondCol(playerPrevX, playerPrevY)
        else
          move playerPrevX to playerX
          move playerPrevY to playerY
        end-if

       .
       playerAttack.
        move 0 to inter
        if acpt-key = "e" THEN
         if playerHeading = 2 THEN
           subtract 1 from playerX giving swordX
           move playerY to swordY
           subtract 1 from playerHeading GIVING swordHeading
           divide swordHeading by 2 giving inter remainder
            swordHeading
         end-if
         if playerHeading = 3 THEN
           add 1 to playerY giving swordY
           move playerX to swordX
           subtract 1 from playerHeading GIVING swordHeading
           divide swordHeading by 2 giving inter remainder
            swordHeading
         end-if
         if playerHeading = 4 THEN
           add 1 to playerX giving swordX
           move playerY to swordY
           subtract 1 from playerHeading GIVING swordHeading
           divide swordHeading by 2 giving inter remainder swordHeading
         end-if
         if playerHeading = 5 THEN
           subtract 1 from playerY giving swordY
           move playerX to swordX
           subtract 1 from playerHeading GIVING swordHeading
           divide swordHeading by 2 giving inter remainder swordHeading
         end-if
        END-IF
        move 0 to inter
        if acpt-key = "q" THEN
          move 1 to playerHeading
        END-IF
       .
       ENDGAME.
        STOP RUN.
        END PROGRAM GameboardDisplay.
