FUNCTION z_hr_entr_user_upd .
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IS_USER_ROW) TYPE
*"        ZCL_ZHR_ZAMESTITEL_DEL_MPC=>TS_ENTR_USER
*"  EXCEPTIONS
*"      NO_BLOCK
*"----------------------------------------------------------------------
  DATA: ls_user TYPE zthr_entr_user.

  MOVE-CORRESPONDING is_user_row TO ls_user.
  ls_user-mandt = sy-mandt.

  WRITE: is_user_row-begda TO ls_user-begda,
         is_user_row-endda TO ls_user-endda.

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

    MODIFY zthr_entr_user FROM ls_user.

  IF sy-subrc <> 0.

    MESSAGE text-m01 TYPE 'A'.
  ELSE.

    CALL FUNCTION 'DEQUEUE_EZLO_ENTR_USER'
      EXPORTING
        mandt = sy-mandt.

  ENDIF.
ENDFUNCTION.
