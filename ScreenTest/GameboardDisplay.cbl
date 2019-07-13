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
           05 CurrentX pic 9(3) value 3.
           05 CurrentY pic 9(3) value 3.
           05 OldX pic 9(3) value 3.
           05 OldY pic 9(3) value 3.
           05 Health pic 9(3) value 100.
           05 Nose pic 9(1) value 2.
           05 Glyph usage pointer value null.
           05 Recoil pic 9(1) value 3.
       01 swordStrike.
           05 CurrentX pic 9(3) value 999.
           05 CurrentY pic 9(3) value 999.
           05 OldX pic 9(3) value 3.
           05 OldY pic 9(3) value 3.
           05 Health pic 9(3) value 100.
           05 Nose pic 9(1) value 0.
           05 Damage pic 9(3) value 10.
           05 Glyph usage pointer value null.
           05 Recoil pic 9(1) value 2.
       01 slime occurs slimeFreq.
           05 CurrentX pic 9(3) value 5.
           05 CurrentY pic 9(3) value 6.
           05 OldX pic 9(3) value 3.
           05 OldY pic 9(3) value 3.
           05 Health pic 9(3) value 100.
           05 Damage pic 9(3) value 5.
           05 Glyph usage pointer value null.
           05 Recoil pic 9(1) value 1.
       01 slimeIndex pic 9(1) value 1.
       01 slimeMax pic 9(1) value 6.
       01 slimePntr usage pointer.
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
      *colliderVars
       01 cldrWS.
           03 cldrSbj.
           05 cldrSbjNewX pic 9(3) value 999.
           05 cldrSbjNewY pic 9(3) value 999.
           05 cldrSbjOldX pic 9(3) value 999.
           05 cldrSbjOldY pic 9(3) value 999.
           05 cldrSbjPntr usage POINTER.
           05 cldrTgtPntr usage POINTER.

       LINKAGE SECTION.
      *topmostCharacter
       01 godChar pic x value null.
      *Entity
       01 entity.
           05 CurrentX pic 9(3) value 3.
           05 CurrentY pic 9(3) value 3.
           05 OldX pic 9(3) value 3.
           05 OldY pic 9(3) value 3.
           05 Health pic 9(3) value 100.
           05 Nose pic 9(1) value 2.
           05 Glyph usage pointer value null.
           05 Recoil pic 9(1) value 3.
       01 cldrLS.
           03 cldrSbj.
            05 CurrentX pic 9(3) value 3.
            05 CurrentY pic 9(3) value 3.
            05 OldX pic 9(3) value 3.
            05 OldY pic 9(3) value 3.
            05 Health pic 9(3) value 100.
            05 Nose pic 9(1) value 2.
            05 Glyph usage pointer value null.
            05 Recoil pic 9(1) value 3.
           03 cldrTgt.
            05 CurrentX pic 9(3) value 3.
            05 CurrentY pic 9(3) value 3.
            05 OldX pic 9(3) value 3.
            05 OldY pic 9(3) value 3.
            05 Health pic 9(3) value 100.
            05 Nose pic 9(1) value 2.
            05 Glyph usage pointer value null.
            05 Recoil pic 9(1) value 3.
           05 cldrGlyph usage pointer value null.

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
          05 LINE 13 COL 5 PIC  X(width) FROM currentx of player.
          05 LINE 13 COL 8 PIC  X(width) FROM currenty of player.
          05 LINE 13 COL 12 PIC  X(width) FROM glyphPlayer(1).
          05 LINE 13 COL 13 PIC  X(width) FROM glyphPlayer(2).
          05 LINE 13 COL 14 PIC  X(width) FROM glyphPlayer(3).
          05 LINE 13 COL 15 PIC  X(width) FROM glyphPlayer(4).
          05 LINE 13 COL 16 PIC  X(width) FROM glyphPlayer(5).
          05 LINE 13 COL 20 PIC  X(width) FROM nose of player.
          05 LINE 13 COL 22 PIC  X(width) FROM glyphPlayer(
           nose of player).


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
         set address of entity to address of slime(slimeindex)
         compute currentX of entity = ((FUNCTION RANDOM(
          acpt-num) * (height - 2))+ 2)
         move width to floater
         add acpt-num to acpt-num
         compute currentY of entity = (FUNCTION RANDOM(acpt-num) *
          (width - 2)) + 2
         add acpt-num to acpt-num
         add 1 to slimeIndex
        END-PERFORM
        move 1 to slimeIndex
      * Prepare map pointers for initial render
        PERFORM prepareMap
        move 1 to currentX of swordStrike
       .
       MAIN-PROCEDURE.
        PERFORM drawMap
        DISPLAY gameMap
        ACCEPT ACPT-KEY TIMEOUT AFTER 1 with auto
        if currentX of swordStrike < 999 and currentY of swordStrike <
          999 then
         move null to secondCol(currentX of swordStrike ,
          currentY of swordStrike )
         move 999 to currentX of swordStrike
         move 999 to currentY of swordStrike
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
             if godEyeX = currentX of player and godEyeY = currentY
                 of player
              move address of glyphPlayer(nose of player) to
              secondCol(godEyeX,godEyeY)
             end-if
            END-IF
           END-IF
           perform until slimeIndex = slimeMax
            set address of entity to address of slime(slimeindex)
            if currentX of entity = godEyeX and currentX of entity =
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
         set address of entity to address of slime(slimeindex)

         move null to secondCol(currentx of entity,
          currenty of entity)
         if(currentx of entity = currentx of swordstrike
          and currenty of entity = currenty of swordstrike) THEN
          subtract damage of swordstrike from health of entity
         END-IF
         if health of entity > 1 THEN
          move address of glyph of entity to secondCol(
           currentx of entity, currenty of entity)
         end-IF
         add 1 to slimeIndex
        END-PERFORM
        if currentX of swordStrike < height and
            currentY of swordStrike < width
         move address of glyphSword(nose of swordStrike) to secondCol(
          currentX of swordStrike,currentY of swordStrike)
        end-if
        move address of glyphPlayer(nose of player) to secondCol(
         currentX of player,currentY of player).
       playerMove.
        move currentx of player to oldx of player
        move currenty of player to oldy of player
        if acpt-key = "w" then
         subtract 1 from currentx of player
         move 2 to nose of player
        else if acpt-key = "a" then
         subtract 1 from currenty of player
         move 5 to  nose of player
        else if acpt-key = "s" then
         add 1 to currentx of player
         move 4 to  nose of player
        else if acpt-key = "d" then
         add 1 to currenty of player
         move 3 to  nose of player
        end-if
        end-if
        end-if
        end-if

      *edge check
        if currentx of player < 1 or currentx of player > height THEN
          move oldx of player to currentx of player

         else
          if  currenty of player < 1 or currenty of player > width THEN
           move oldy of player to currenty of player
          END-IF
         END-IF
      *collision check
        if secondCol(currentx of player,currenty of player) = NULL then
         move null to secondCol(oldx of player, oldy of player)
        else
          move oldx of player to currentx of player
          move oldy of player to currenty of player
        end-if

       .
       playerAttack.
        move 0 to inter
        if acpt-key = "e" THEN
         if nose of player = 2 THEN
           subtract 1 from currentx of player giving currentx
            of swordStrike
           move currenty of player to currenty of swordStrike
           subtract 1 from nose of player GIVING nose of swordstrike
           divide nose of swordstrike by 2 giving inter remainder
            nose of swordstrike
         end-if
         if nose of player = 3 THEN
           add 1 to currenty of player giving currenty of swordStrike
           move currentx of player to currentx of swordstrike
           subtract 1 from nose of player GIVING nose of swordStrike
           divide nose of swordStrike by 2 giving inter remainder
            nose of swordStrike
         end-if
         if nose of player = 4 THEN
           add 1 to currentx of player giving currentx of swordStrike
           move currenty of player to currenty of swordStrike
           subtract 1 from nose of player GIVING nose of swordStrike
           divide nose of swordStrike by 2 giving inter remainder
            nose of swordStrike
         end-if
         if nose of player = 5 THEN
           subtract 1 from currenty of player giving currenty of
            swordStrike
           move currentx of player to currentx of swordstrike
           subtract 1 from nose of player GIVING nose of swordStrike
           divide nose of swordStrike by 2 giving inter remainder
            nose of swordStrike
         end-if
        END-IF
        move 0 to inter
        if acpt-key = "q" THEN
          move 1 to nose of player
        END-IF
       .
       ENDGAME.
        STOP RUN.
        END PROGRAM GameboardDisplay.
