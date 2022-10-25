*----------------------------------------------------------------------*
***INCLUDE ZFGHR_ENTR_USER_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZZ_FILLFIO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zz_fillfio OUTPUT.
  DATA: entr_user TYPE zthr_entr_user
      .
  entr_user = extract.

  IF NOT lo_assistent IS BOUND.
    lo_assistent = zcl_mss_data_assistent=>get_instance( ).
  ENDIF.

  IF entr_user-main_user IS NOT INITIAL.
    zs_line-main_user_fio = lo_assistent->get_user_fio( iv_usrid = CONV string( entr_user-main_user ) ) .
  ENDIF.

  IF entr_user-entrance_user IS NOT INITIAL.
    zs_line-entrance_user_fio  = lo_assistent->get_user_fio( iv_usrid = CONV string( entr_user-entrance_user ) ) .
  ENDIF.
ENDMODULE.
