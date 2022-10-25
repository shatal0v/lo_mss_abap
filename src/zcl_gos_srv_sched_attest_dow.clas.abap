class ZCL_GOS_SRV_SCHED_ATTEST_DOW definition
  public
  inheriting from ZCL_GOS_SRV_SCHED_ATTEST
  create public .

public section.

  methods EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GOS_SRV_SCHED_ATTEST_DOW IMPLEMENTATION.


  METHOD execute.
    DATA(lv_object) = CONV zthr_mss_at_attr-object( gs_lporb-instid ).

    CALL FUNCTION 'ZFMHR_MSS_ATTACH_POPUP'
      EXPORTING
        iv_object = lv_object
        iv_ftype  = 'GA'.
  ENDMETHOD.
ENDCLASS.
