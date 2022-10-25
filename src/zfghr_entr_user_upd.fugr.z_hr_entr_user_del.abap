FUNCTION z_hr_entr_user_del .
*"--------------------------------------------------------------------
*"*"Функц. модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IS_USER_ROW) TYPE
*"ZCL_ZHR_ZAMESTITEL_DEL_MPC=>TS_ENTR_USER
*"  EXCEPTIONS
*"      NO_BLOCK
*"--------------------------------------------------------------------
  DATA: ls_user TYPE zthr_entr_user.

  MOVE-CORRESPONDING is_user_row TO ls_user.


  CALL FUNCTION 'ENQUEUE_EZLO_ENTR_USER'
    EXPORTING
      mandt          = sy-mandt
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    RAISE no_block.
  ENDIF.

  ls_user-del = abap_true.

  DATA(lv_mai_user) = ls_user-main_user.
  ls_user-main_user = ls_user-entrance_user.
  ls_user-entrance_user = lv_mai_user.

  UPDATE zthr_entr_user
     FROM ls_user.

*  UPDATE zthr_entr_user SET  del = abap_true
*   WHERE main_user     = ls_user-main_user
*     AND entrance_user = ls_user-entrance_user
*     AND begda         = ls_user-begda
*     AND endda         = ls_user-endda.

  IF sy-subrc <> 0.

    MESSAGE text-m01 TYPE 'A'.
  ELSE.

    CALL FUNCTION 'DEQUEUE_EZLO_ENTR_USER'
      EXPORTING
        mandt = sy-mandt.

  ENDIF.
ENDFUNCTION.
