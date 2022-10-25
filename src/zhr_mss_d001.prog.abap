REPORT zhr_mss_d001.

INCLUDE: zhr_mss_d001_top
       , zhr_mss_d001_lcl
       .

START-OF-SELECTION.
  DATA(lo_lcl) = NEW lcl_mss_d001( ).

  lo_lcl->get_data( ).

END-OF-SELECTION.

  lo_lcl->alv( ).
*&---------------------------------------------------------------------*
*&      Module  PBO100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0001 OUTPUT.
  SET PF-STATUS 'STATUS_0001'.
  set TITLEBAR 'TITLEBAR_0001'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0001 INPUT.
  DATA(lv_fcode) = fcode.
  CLEAR: fcode
       .
  CASE lv_fcode.
    WHEN `BACK`.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN OTHERS.
      fcode = lv_fcode.
  ENDCASE.
ENDMODULE.
