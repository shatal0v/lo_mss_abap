FUNCTION zfmhr_mss_attach_popup.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(IV_OBJECT) TYPE  ZTHR_MSS_AT_ATTR-OBJECT
*"     REFERENCE(IV_FTYPE) TYPE  ZTHR_MSS_AT_ATTR-OBJECT
*"----------------------------------------------------------------------
  lv_object = iv_object.
  lv_ftype  = iv_ftype.

  PERFORM fill_data.

  CREATE OBJECT lo_assistent.

  CHECK sy-batch IS INITIAL.

  CALL SCREEN 0200 STARTING AT 5  5
                     ENDING AT 95 20.
ENDFUNCTION.
