*----------------------------------------------------------------------*
***INCLUDE LZFGHR_MSS_ATTACHMOD.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST100'.
  CASE lv_ftype.
    WHEN 'IP'.
      SET TITLEBAR 'TITLE_0201'.
*        ls_layout-grid_title = 'ИПР'.
    WHEN 'LE'.
      SET TITLEBAR 'TITLE_0202'.
*        ls_layout-grid_title = 'Заявления на отсутствие'.
    WHEN OTHERS.
      SET TITLEBAR 'TITLE_0203'.
*        ls_layout-grid_title = 'График аттестации'.
  ENDCASE.


  PERFORM pbo_0200.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'ENTER'.
      LOOP AT lt_dele ASSIGNING FIELD-SYMBOL(<lt_dele>).
        lo_assistent->file_delete( iv_object = <lt_dele>-object
                                   iv_ftype  = <lt_dele>-ftype
                                   iv_seqnr  = <lt_dele>-seqnr ).
      ENDLOOP.
      LEAVE TO SCREEN 0.
    WHEN  'CANCEL' OR 'CLOSE'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
