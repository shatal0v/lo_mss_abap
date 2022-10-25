

INCLUDE zhr_ess_docreq_top.
INCLUDE zhr_ess_docreq_scr.
INCLUDE zhr_ess_docreq_cl1.
INCLUDE zhr_ess_docreq_frm.
INCLUDE zhr_ess_docreq_p01.
INCLUDE zhr_ess_docreq_pa1.

INITIALIZATION.
  FREE: gt_att.
*  gv_usrid = sy-uname.
  var_date = sy-datum.

  var = var_date+0(4).
  var = var  - 1 .
  p_gjahr = var.

  SELECT SINGLE pernr FROM pa0105 INTO gv_pernr WHERE subty = '0001' AND usrid = sy-uname. "#EC CI_NOFIRST

  SELECT a~domvalue_l AS value b~ddtext AS dtext
    INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM dd07l AS a INNER JOIN dd07t AS b
    ON a~domname = b~domname
    AND a~as4local = b~as4local
    AND a~valpos = b~valpos
    AND a~as4vers = b~as4vers
    WHERE a~domname = 'ZDHR_IPRREQ_STATUS'
    AND b~ddlanguage = sy-langu.

*  CALL FUNCTION 'DD_DOMVALUES_GET'
*    EXPORTING
*      domname        = 'ZDHR_IPRREQ_STATUS'
*      text           = 'X'
*      langu          = sy-langu
*    TABLES
*      dd07v_tab      = gt_idd07v
*    EXCEPTIONS
*      wrong_textflag = 1
*      OTHERS         = 2.

START-OF-SELECTION.
  PERFORM select_data.

END-OF-SELECTION.
  CALL SCREEN 100.
