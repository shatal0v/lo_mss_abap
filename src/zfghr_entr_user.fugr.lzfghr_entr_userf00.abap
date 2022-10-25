*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 17.03.2020 at 13:38:29
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVHR_ENTR_USER..................................*
FORM GET_DATA_ZVHR_ENTR_USER.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZTHR_ENTR_USER WHERE
(VIM_WHERETAB) .
    CLEAR ZVHR_ENTR_USER .
ZVHR_ENTR_USER-MANDT =
ZTHR_ENTR_USER-MANDT .
ZVHR_ENTR_USER-MAIN_USER =
ZTHR_ENTR_USER-MAIN_USER .
ZVHR_ENTR_USER-ENTRANCE_USER =
ZTHR_ENTR_USER-ENTRANCE_USER .
ZVHR_ENTR_USER-BEGDA =
ZTHR_ENTR_USER-BEGDA .
ZVHR_ENTR_USER-ENDDA =
ZTHR_ENTR_USER-ENDDA .
ZVHR_ENTR_USER-DEL =
ZTHR_ENTR_USER-DEL .
<VIM_TOTAL_STRUC> = ZVHR_ENTR_USER.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZVHR_ENTR_USER .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZVHR_ENTR_USER.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZVHR_ENTR_USER-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZTHR_ENTR_USER WHERE
  MAIN_USER = ZVHR_ENTR_USER-MAIN_USER AND
  ENTRANCE_USER = ZVHR_ENTR_USER-ENTRANCE_USER AND
  BEGDA = ZVHR_ENTR_USER-BEGDA AND
  ENDDA = ZVHR_ENTR_USER-ENDDA .
    IF SY-SUBRC = 0.
    DELETE ZTHR_ENTR_USER .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZTHR_ENTR_USER WHERE
  MAIN_USER = ZVHR_ENTR_USER-MAIN_USER AND
  ENTRANCE_USER = ZVHR_ENTR_USER-ENTRANCE_USER AND
  BEGDA = ZVHR_ENTR_USER-BEGDA AND
  ENDDA = ZVHR_ENTR_USER-ENDDA .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZTHR_ENTR_USER.
    ENDIF.
ZTHR_ENTR_USER-MANDT =
ZVHR_ENTR_USER-MANDT .
ZTHR_ENTR_USER-MAIN_USER =
ZVHR_ENTR_USER-MAIN_USER .
ZTHR_ENTR_USER-ENTRANCE_USER =
ZVHR_ENTR_USER-ENTRANCE_USER .
ZTHR_ENTR_USER-BEGDA =
ZVHR_ENTR_USER-BEGDA .
ZTHR_ENTR_USER-ENDDA =
ZVHR_ENTR_USER-ENDDA .
ZTHR_ENTR_USER-DEL =
ZVHR_ENTR_USER-DEL .
    IF SY-SUBRC = 0.
    UPDATE ZTHR_ENTR_USER .
    ELSE.
    INSERT ZTHR_ENTR_USER .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZVHR_ENTR_USER-UPD_FLAG,
STATUS_ZVHR_ENTR_USER-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZVHR_ENTR_USER.
  SELECT SINGLE * FROM ZTHR_ENTR_USER WHERE
MAIN_USER = ZVHR_ENTR_USER-MAIN_USER AND
ENTRANCE_USER = ZVHR_ENTR_USER-ENTRANCE_USER AND
BEGDA = ZVHR_ENTR_USER-BEGDA AND
ENDDA = ZVHR_ENTR_USER-ENDDA .
ZVHR_ENTR_USER-MANDT =
ZTHR_ENTR_USER-MANDT .
ZVHR_ENTR_USER-MAIN_USER =
ZTHR_ENTR_USER-MAIN_USER .
ZVHR_ENTR_USER-ENTRANCE_USER =
ZTHR_ENTR_USER-ENTRANCE_USER .
ZVHR_ENTR_USER-BEGDA =
ZTHR_ENTR_USER-BEGDA .
ZVHR_ENTR_USER-ENDDA =
ZTHR_ENTR_USER-ENDDA .
ZVHR_ENTR_USER-DEL =
ZTHR_ENTR_USER-DEL .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZVHR_ENTR_USER USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZVHR_ENTR_USER-MAIN_USER TO
ZTHR_ENTR_USER-MAIN_USER .
MOVE ZVHR_ENTR_USER-ENTRANCE_USER TO
ZTHR_ENTR_USER-ENTRANCE_USER .
MOVE ZVHR_ENTR_USER-BEGDA TO
ZTHR_ENTR_USER-BEGDA .
MOVE ZVHR_ENTR_USER-ENDDA TO
ZTHR_ENTR_USER-ENDDA .
MOVE ZVHR_ENTR_USER-MANDT TO
ZTHR_ENTR_USER-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZTHR_ENTR_USER'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZTHR_ENTR_USER TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZTHR_ENTR_USER'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*