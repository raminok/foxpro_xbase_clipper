 DO SETSAVE
 ON KEY
 SET SYSMENU ON
 SET STATUS OFF
 SET SAFETY OFF
 SET CONFIRM OFF
 SET TALK OFF
 SET ESCAPE OFF
 SET BLINK OFF
 RUN mode con:rate=32 delay=1
 SET COLOR TO N/W*, GR+/R, RB/N
 CLEAR
 WC = 0
 WR = 0
 COMMAND_T1 = SPACE(78)
 COMMAND_T2 = SPACE(78)
 COMMAND_T3 = SPACE(78)
 COMMAND_T4 = SPACE(78)
 COMMAND_T5 = SPACE(78)
 COMMAND_T = SPACE(78)
 COMMAND_TM = SPACE(78)
 COMMAND_TN = SPACE(78)
 COMMAND_TO = SPACE(78)
 PUBLIC _MSAVE_SCR
 _MSAVE_SCR = .F.
 DO WHILE .T.
      ON ERROR wait wind mess() nowait
      ON KEY LABEL F1 help
      ON KEY LABEL alt+F1 command=command_t5+spac(78-len(allt(command_t5)))
      ON KEY LABEL alt+F2 command=command_t4+spac(78-len(allt(command_t4)))
      ON KEY LABEL alt+F3 command=command_t3+spac(78-len(allt(command_t3)))
      ON KEY LABEL alt+F4 command=command_t2+spac(78-len(allt(command_t2)))
      ON KEY LABEL alt+F5 command=command_t1+spac(78-len(allt(command_t1)))
      ON KEY LABEL alt+F6 command=command_t +spac(78-len(allt(command_t)))
      ON KEY LABEL alt+F7 command=command_tm+spac(78-len(allt(command_tm)))
      ON KEY LABEL alt+F8 command=command_tn+spac(78-len(allt(command_tn)))
      ON KEY LABEL alt+F9 command=command_to+spac(78-len(allt(command_to)))
      ON KEY LABEL ctrl+z do undo
      ON KEY LABEL ctrl+F7 set disp to vga50
      ON KEY LABEL ctrl+F8 set disp to vga25
      ON KEY LABEL alt+x keyb 'EXIT'+'{enter}'
      ON KEY LABEL alt+f10 do prompt
      W_MQUIT = .F.
      DO _MCOLOR
      DEFINE WINDOW _MPROMPT FROM WR, WC TO WR, WC NONE COLOR W+/G 
      SHOW WINDOW _MPROMPT
      DO WHILE .T.
           COMMAND = SPACE(78)
           DEFINE WINDOW _MCOMMAND FROM SROWS()-1, 0 TO SROWS()-1, SCOLS()-1 NONE
           ACTIVATE WINDOW _MCOMMAND
            IF SET('blin')='ON'
                @ 0, 0 SAY '' COLOR W+/R 
                @ 0, 1 GET COMMAND SIZE 1, 79 COLOR ,W+/R 
            ELSE
                 @ 0, 0 SAY '' COLOR W+/R* 
                 @ 0, 1 GET COMMAND SIZE 1, 79 COLOR ,W+/R* 
            ENDIF
            READ
             IF READKEY()=12 .OR. EMPTY(ALLTRIM(COMMAND))
                 DEACTIVATE WINDOW _MPROMPT
                 EXIT
             ENDIF
             ACTIVATE SCREEN
             COMMANDTEM = ALLTRIM(COMMAND)
             COMMAND = UPPER(ALLTRIM(COMMAND))
             COMMAND_TO = COMMAND_TN
             COMMAND_TN = COMMAND_TM
             COMMAND_TM = COMMAND_T
             COMMAND_T = COMMAND_T1
             COMMAND_T1 = COMMAND_T2
             COMMAND_T2 = COMMAND_T3
             COMMAND_T3 = COMMAND_T4
             COMMAND_T4 = COMMAND_T5
             COMMAND_T5 = COMMAND
              DO CASE
                   CASE COMMAND=='HELP'
                            DO HLP
                            LOOP
      CASE COMMAND=='DOS'
              SET BELL TO 512, 4
              ?? CHR(7)
              RUN cls
              RUN echo            ............. Type EXIT to Return FoxDraw.........
              RUN command
              CLEAR
              LOOP
      CASE COMMAND='CLEAR WIND' .OR. COMMAND='CLEA WIND' .OR. 'CLEA WIND'$COMMAND
               DO _MERR
               LOOP
      CASE COMMAND=='NEW'
               IF _MSAVE_SCR
                   DEFINE WINDOW _MQUIT FROM 11, 18 TO 13, 53 SHADOW COLOR N/W 
                   ACTIVATE WINDOW _MQUIT
                   \ Save screen to a file? y/n
                   WKEY = INKEY(0)
                   DO WHILE UPPER(CHR(WKEY))<>'N'
                         IF UPPER(CHR(WKEY))='Y'
                             RELEASE WINDOW _MQUIT
                             DO _MSAVE
                             EXIT
                        ENDIF
                        WKEY = INKEY(0)
                   ENDDO
                   RELEASE WINDOW _MQUIT
                   _MSAVE_SCR = .F.
                    ACTIVATE SCREEN
                    CLEAR
                    LOOP
               ELSE
                     COMMANDTEM = 'CLEAR'
               ENDIF
       CASE COMMAND='HELP FOXDRAW' .OR. COMMAND=='/?' .OR. COMMAND=='?'
                DO HLP
                LOOP
        CASE 'RETU'$COMMAND
                 DO _MERR
                  LOOP
         CASE 'RELE ALL'$COMMAND .OR. 'RELEA ALL'$COMMAND .OR. 'RELEAS ALL'$COMMAND .OR. 'RELEASE ALL'$COMMAND
                  DO _MERR
                  LOOP
         CASE COMMAND='MODI COMM FOXDRAW'
                  DO _MERR
                  LOOP
         CASE COMMAND='CLEAR ALL' .OR. COMMAND='CLEA ALL'
                  DO _MERR
 LOOP
 CASE COMMAND=='QUIT'
 DO QUIT
 LOOP
 CASE 'SUSP'$COMMAND
 DO _MERR
 LOOP
 CASE 'CANC'$COMMAND
 DO _MERR
 LOOP
 CASE COMMAND=='MENU'
 KEYBOARD '{f10}'
 LOOP
 CASE COMMAND=='CLS'
 COMMANDTEM = 'CLEAR'
 _MSAVE_SCR = .F.
 CASE COMMAND=='VER' .OR. COMMAND=='AUTHOR' .OR. COMMAND=='ABOUT'
 DO PROMPT
 LOOP
 CASE COMMAND=='PAINT'
 DO PAINT
 LOOP
 CASE COMMAND=='EXIT'
 W_MQUIT = .T.
 EXIT
 CASE COMMAND=='COLOR'
 KEYBOARD '{ALT+C}'
 LOOP
 ENDCASE
 SET SAFETY OFF
 SAVE SCREEN TO _MSCR_UNDO
 SAVE TO rrrtemp.mem
 RELEASE WINDOW _MCOMMAND
 &commandtemp
 IF FILE('rrrtemp.mem')
 RESTORE FROM rrrtemp.mem
 ENDIF
 ENDDO
 W_MQUIT = IIF(TYPE('w_mquit')='U', .F., W_MQUIT)
 IF W_MQUIT
 EXIT
 ENDIF
 _MSAVE_SCR = .T.
 DEACTIVATE WINDOW _MCOMMAND
 @ WR, WC SAY ''
 _MCHAR = ' '
 WKEY = INKEY(0)
 WWWALLOW = .F.
 DO WHILE WKEY<>27
 DO CASE
 CASE WKEY=4
 WWWALLOW = .T.
 WC = IIF(WC>=SCOLS()-1, WC, WC+1)
 @ WR, WC SAY ''
 CASE WKEY=5
 WWWALLOW = .T.
 WR = IIF(WR<=0, WR, WR-1)
 @ WR, WC SAY ''
 CASE WKEY=19
 WWWALLOW = .T.
 WC = IIF(WC<=0, WC, WC-1)
 @ WR, WC SAY ''
 CASE WKEY=24
 WWWALLOW = .T.
 WR = IIF(WR>=SROWS()-1, WR, WR+1)
 @ WR, WC SAY ''
 CASE WKEY=127
 @ WR, WC SAY ' '
 WC = IIF(WC<=0, WC, WC-1)
 @ WR, WC SAY ''
 CASE (WKEY>=32 .AND. WKEY<127) .OR. (WKEY>127 .AND. WKEY<=255)
 IF WKEY=32
 _MCHAR = IIF(EMPTY(_MCHAR), '', _MCHAR)
 @ WR, WC SAY _MCHAR
 _MCOLO = SET('colo')
 @wr,wc to wr,wc fill colo &_mcolo
 ELSE
 _MCHAR = CHR(WKEY)
 @ WR, WC SAY CHR(WKEY)
 ENDIF
 WC = IIF(WC>=SCOLS()-1, WC, WC+1)
 @ WR, WC SAY ''
 CASE WKEY=26
 DO UNDO
 CASE WKEY=28
 DO RING
 DO PROMPT
 DO HLP
 CASE WKEY=18
 WWWALLOW = .T.
 WR = 0
 @ WR, WC SAY ''
 CASE WKEY=3
 WWWALLOW = .T.
 WR = SROWS()-1
 @ WR, WC SAY ''
 CASE WKEY=-9
 ACTIVATE POPUP _MCOLOR
 CASE WKEY=13
 WAIT WINDOW NOWAIT ' ROW='+STR(WR, 2)+'  COL='+STR(WC, 2)+' '
 CASE WKEY=9
 WWWALLOW = .T.
 WC = IIF(WC+6>=SCOLS()-1, WC, WC+6)
 @ WR, WC SAY ''
 CASE WKEY=15
 WWWALLOW = .T.
 WC = IIF(WC-6<=0, WC, WC-6)
 @ WR, WC SAY ''
 CASE WKEY=7
 SCROLL WR, WC, WR, 79, 0, -1
 CASE WKEY=1
 WWWALLOW = .T.
 WC = 0
 @ WR, WC SAY ''
 CASE WKEY=6
 WWWALLOW = .T.
 WC = SCOLS()-1
 @ WR, WC SAY ''
 ENDCASE
 IF WWWALLOW
 SAVE SCREEN TO _MSCR_UNDO
 ENDIF
 WWWALLOW = .F.
 WKEY = INKEY(0)
 ENDDO
 ENDDO
 IF _MSAVE_SCR
 DEFINE WINDOW _MQUIT FROM 11, 18 TO 13, 53 SHADOW COLOR N/W 
 ACTIVATE WINDOW _MQUIT
 \ Save screen to a file? y/n
 WKEY = INKEY(0)
 DO WHILE UPPER(CHR(WKEY))<>'N'
 IF UPPER(CHR(WKEY))='Y'
 RELEASE WINDOW _MQUIT
 DO _MSAVE
 RELEASE WINDOW _MSAVE
 EXIT
 ENDIF
 WKEY = INKEY(0)
 ENDDO
 RELEASE WINDOW _MQUIT
 ENDIF
 SET SYSMENU TO DEFAULT
 CLEAR WINDOW
 IF FILE('rrrtemp.mem')
 RUN del rrrtemp.mem
 ENDIF
 DO SETSET
 CLEAR
*
PROCEDURE UNDO
 IF TYPE('_mscr_undo')='S'
 ACTIVATE SCREEN
 RESTORE SCREEN FROM _MSCR_UNDO
 ENDIF
*
PROCEDURE RING
 SET BELL TO 600, 1
 ?? CHR(7)
 SET BELL TO 700, 1
 ?? CHR(7)
 SET BELL TO 800, 1
 ?? CHR(7)
*
PROCEDURE PROMPT
 = ZM(4,42)
 \            FoxDraw ver 1.1
 \         written by Ramin orak
 \ (c)copy right 1996 all rights reserved.
 DO RING
 = INKEY(0, 'h')
 RELEASE WINDOW ZM
*
PROCEDURE HLP
 = ZM(16,64)
 \                THANKS TO USE THIS PROGRAM.
 \
 \     This program is more useful to design and paint screen
 \  To use this program,type your command in the command line
 \  and press <Enter>. you can type every command as like as
 \  FoxPro command window.many command such as OS command and
 \  FoxPro command like as BROWSE,APPEND,EDIT,COPY,TOTAL,...
 \  you can paint a screen and color it by set color to command
 \  and save it to a variable or file and use more time.
 \  For example : SAVE SCREEN TO _mscreen
 \  save screen to variable _mscreen and you can restore screen
 \  from that . To use Draw screen press <Esc>.
 \  command:EXIT for exit this program and QUIT exit from FoxPro.
 DO RING
 IF INKEY(0, 'h')<>27
 RELEASE WINDOW ZM
 DEFINE WINDOW ZOOM FROM 11, 41 TO 12, 43 SHADOW NONE COLOR SCHEME 5
 ACTIVATE WINDOW ZOOM
 = ZOOM(0,0,22,40)
 \ ีอออออออออออออออออออออออออออออออออออออธ
 \ ณ    KEY AND COMMAND IN FOXDRAW       ณ
 \ ร----------------key------------------ด
 \ ณ F1      Help                        ณ
 \ ณ Alt+X   Exit                        ณ
 \ ณ Ctrl+Z  Undo                        ณ
 \ ณ Alt+F1  First  Command              ณ
 \ ณ       ...                           ณ
 \ ณ       ...                           ณ
 \ ณ Alt+F9  Nineth Command              ณ
 \ ณ F10     Active Menu (at draw screen)ณ
 \ ร--------------command----------------ด
 \ ณ ?       Show Foxdraw Help           ณ
 \ ณDOS      Return to Dos               ณ
 \ ณCLS      Clear the screen            ณ
 \ ณNEW      Begin new screen            ณ
 \ ณHELP     Activate help window        ณ
 \ ณQUIT     Exit from FoxPro            ณ
 \ ณEXIT     Exit from FoxDraw           ณ
 \ ณCOLOR    Active menu color           ณ
 \ ณVER      Version of program          ณ
 \ ณPAINT    Paint and color randomly    ณ
 \ ิอออออออออออออออออออออออออออออออออออออพ
 = INKEY(0, 'h')
 RELEASE WINDOW ZOOM
 ENDIF
 RELEASE WINDOW ZM
*
FUNCTION ZM
 PARAMETER _ROW1, _COL1, _SCHEM, _NAME
 _WR = _ROW1
 _WC = _COL1
 _MROW = INT(SROWS()/2)
 _MCOL = INT(SCOLS()/2)
 _ROW1 = INT((SROWS()-(ABS(_WR)))/2)
 _COL1 = INT((SCOLS()-(ABS(_WC)))/2)
 _ROW2 = _ROW1+(ABS(_WR))
 _COL2 = _COL1+(ABS(_WC))
 _TEMP_ID = IIF(TYPE('_name')='C', IIF(VAL(_NAME)>0, ALLTRIM(LOWER(_NAME)), 'd'+ALLTRIM(LOWER(_NAME))), 'zm')
 defi wind &_temp_id from _mrow,_mcol to _mrow+2,_mcol+2 doub shad colo sche 5
 acti wind &_temp_id
 zoom wind &_temp_id norm from _mrow-1,_mcol-2 to _mrow+2,_mcol+3
 zoom wind &_temp_id norm from _mrow-2,_mcol-4 to _mrow+3,_mcol+5
 zoom wind &_temp_id norm from _mrow-2,_mcol-5 to _mrow+3,_mcol+6
 = INKEY(0.001)
 zoom wind &_temp_id norm from _row1,_col1 to _row2,_col2
 RETURN ''
*
FUNCTION ZOOM
 PARAMETER _ROW1, _COL1, _ROW2, _COL2, _SCHEM, _NAME
 _WR = _ROW1
 _WC = _COL1
 _MROW = INT(SROWS()/2)
 _MCOL = INT(SCOLS()/2)
 _ROW1 = INT((SROWS()-(ABS(_ROW2-_WR)))/2)
 _COL1 = INT((SCOLS()-(ABS(_COL2-_WC)))/2)
 _ROW2 = _ROW1+(ABS(_ROW2-_WR))
 _COL2 = _COL1+(ABS(_COL2-_WC))
 _TEMP_ID = IIF(EMPTY(WOUTPUT()), 'zoom', WOUTPUT())
 IF EMPTY(WOUTPUT())
 IF TYPE('_schem')='C'
 _SCHEM = ALLTRIM(_SCHEM)
 DEFINE WINDOW &_temp_id FROM _mrow,_mcol TO _mrow+2,_mcol+2 COLOR &_schem
 ELSE
 IF TYPE('_schem')='N'
 DEFINE WINDOW &_temp_id FROM _mrow,_mcol TO _mrow+2,_mcol+2 DOUBLE SHADOW COLOR SCHEM _schem
 ELSE
 DEFINE WINDOW &_temp_id FROM _mrow,_mcol TO _mrow+2,_mcol+2 DOUBLE SHADOW COLOR SCHEM 5
 ENDIF
 ENDIF
 ENDIF
 ACTIVATE WINDOW &_temp_id
 ZOOM WINDOW &_temp_id NORMAL FROM _mrow-1,_mcol-2 TO _mrow+2,_mcol+3
 ZOOM WINDOW &_temp_id NORMAL FROM _mrow-2,_mcol-4 TO _mrow+3,_mcol+5
 ZOOM WINDOW &_temp_id NORMAL FROM _mrow-2,_mcol-5 TO _mrow+3,_mcol+6
 = INKEY(0.001)
 ZOOM WINDOW &_temp_id NORMAL FROM _row1,_col1 TO _row2,_col2
 RETURN ''
*
PROCEDURE _MCOLOR
 DEFINE PAD _MCOLOR OF _MSYSMENU PROMPT '\<Color' KEY alt+c
 DEFINE WINDOW _MSAVE FROM 4, 20 TO 8, 50 SHADOW SYSTEM COLOR SCHEME 8
 ON PAD _MCOLOR OF _MSYSMENU ACTIVATE POPUP _MCOLOR
 DEFINE POPUP _MCOLOR SHADOW COLOR SCHEME 4
 DEFINE BAR 1 OF _MCOLOR PROMPT '\<Save   ' KEY ctrl+S, '^S'
 DEFINE BAR 2 OF _MCOLOR PROMPT '\<Restore' KEY ctrl+R, '^R'
 ON SELECTION BAR 1 OF _MCOLOR do _msave
 ON SELECTION BAR 2 OF _MCOLOR do _mrest
 DEFINE PAD _MFI_EXIT OF _MFILE PROMPT 'E\<xit'
 RELEASE PAD _MPR_CANCL OF _MPROG
 RELEASE PAD _MED_UNDO OF _MEDIT
 RELEASE PAD _MFI_QUIT OF _MFILE
 DEFINE PAD _MED_UND OF _MEDIT PROMPT '\<Undo' KEY ctrl+z, '^Z'
 DEFINE PAD _MFIQUIT OF _MFILE PROMPT '\<QUIT'
 ON SELECTION PAD _MED_UND OF _MEDIT do undo
 ON SELECTION PAD _MFI_EXIT OF _MFILE keyb "exit"+"{enter}"
 ON SELECTION PAD _MFIQUIT OF _MFILE keyb "quit"+"{enter}"
*
PROCEDURE _MSAVE
 DO WHILE .T.
 DEFINE WINDOW _MSAVE FROM 4, 20 TO 8, 50 SHADOW TITLE 'save' SYSTEM COLOR SCHEME 8
 ACTIVATE WINDOW _MSAVE
 _MSAVE_SCR = .F.
 _MPATH = SPACE(8)
 @ 0, 0 SAY ' Save screen to :' COLOR W+/BG 
 @ 2, 0 SAY SYS(5)+SYS(2003)+'\' GET _MPATH DEFAULT 'untitled' COLOR W+/BG,W+/B 
 READ
 IF LASTKEY()=27
    EXIT
 ELSE
    _MPATH = ALLTRIM(_MPATH)
    _mapth=SYS(5)+SYS(2003)+'\'+_mpath+'.MEM'
    IF FILE('&_mapth')
       DO WHILE .T.
          WAIT WINDOW NOWAIT 'It is exist overwrite ? y/n '
          WKEY = INKEY(0)
          IF UPPER(CHR(WKEY))='Y'
             ACTIVATE SCREEN
             SAVE SCREEN TO &_mpath
             SAVE TO &_mpath all like &_mpath
             WQUIT_SAVE = .T.
             EXIT
          ENDIF
          IF UPPER(CHR(WKEY))='N'
             WAIT CLEAR
             WQUIT_SAVE = .F.
             EXIT
          ENDIF
          IF WKEY=27
             EXIT
          ENDIF
       ENDDO
    ELSE
       ACTIVATE SCREEN
       SAVE SCREEN TO &_mpath
       SAVE TO &_mpath all like &_mpath 
       WQUIT_SAVE = .T.
       EXIT
    ENDIF
 ENDIF
 IF WQUIT_SAVE
    EXIT
 ENDIF
 ENDDO
 RELEASE WINDOW _MSAVE
*
PROCEDURE _MREST
 ACTIVATE SCREEN
 DEFINE POPUP _MREST FROM 1, 30 TO 21, 50 PROMPT FILE LIKE *.MEM SHADOW COLOR SCHEME 4
 ON SELECTION POPUP _MREST do _mrest_m1 with bar()
 ACTIVATE POPUP _MREST
 DEACTIVATE POPUP _MREST
 RELEASE POPUP _MREST
 ACTIVATE WINDOW _MCOMMAND
*
PROCEDURE _MERR
 WAIT WINDOW NOWAIT ' statement not allowed in FoxDraw. '
*
PROCEDURE _MREST_M1
 PARAMETER BAR
 MREST_FILE = PRMBAR('_mrest', BAR)
 MREST_FILE = SUBSTR(MREST_FILE, 1, AT('.', MREST_FILE)-1)
 command_t5='RESTORE SCREEN FROM &mrest_file'
 **RAMIN
 RESTORE FROM &mrest_file..MEM ADDITIVE
 RESTORE SCREEN FROM &mrest_file
 DEACTIVATE POPUP _MREST
 RELEASE POPUP _MREST
*
PROCEDURE PAINT
      WCOLO_PAIN = 'rgbwngrbrbgrr+g+b+w+n+gr+br+bg+r*g*b*w*n*gr*bg*br*'
      WCOLO = SUBSTR(WCOLO_PAIN, INT(RAND()*50), 1)
      DO WHILE INKEY()=0
           @0,0 to srow(),scol() fill colo &wcolo/b
            IF INKEY(1)<>0
                EXIT
            ENDIF
            @0,0 to srow(),scol() fill colo &wcolo/g
             IF INKEY(1)<>0
                 EXIT
           ENDIF
           @0,0 to srow(),scol() fill colo &wcolo/r
            IF INKEY(1)<>0
                EXIT
 ENDIF
 @0,0 to srow(),scol() fill colo &wcolo/w
 IF INKEY(1)<>0
     EXIT
 ENDIF
 @0,0 to srow(),scol() fill colo &wcolo/bg
 IF INKEY(1)<>0
     EXIT
 ENDIF
 @0,0 to srow(),scol() fill colo &wcolo/br
 IF INKEY(1)<>0
     EXIT
 ENDIF
  @0,0 to srow(),scol() fill colo &wcolo/gr
   IF INKEY(1)<>0
       EXIT
   ENDIF
   @0,0 to srow(),scol() fill colo &wcolo/n
    IF INKEY(1)<>0
        EXIT
    ENDIF
     @0,0 to srow(),scol() fill colo &wcolo/w
      IF INKEY(1)<>0
          EXIT
      ENDIF
      WCOLO = SUBSTR(WCOLO_PAIN, INT(RAND()*37), 3)
 ENDDO
 _MSAVE_SCR = .T.
*
PROCEDURE QUIT
    DEFINE WINDOW _MQUIT FROM 11, 20 TO 13, 50 SHADOW COLOR N/W 
    ACTIVATE WINDOW _MQUIT
    DO WHILE .T.
          WAIT TO _MQUIT ' Are you sure to quit ? y/n '
           IF UPPER(_MQUIT)='Y'
               CLEAR ALL
               SET SYSMENU OFF
                QUIT
            ENDIF
            IF UPPER(_MQUIT)='N'
            RELEASE WINDOW _MQUIT
            EXIT
      ENDIF
       IF LASTKEY()=27
           RELEASE WINDOW _MQUIT
           EXIT
       ENDIF
 ENDDO
*
