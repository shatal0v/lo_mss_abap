class ZCL_GOS_SRV_SCHED_ATTEST definition
  public
  inheriting from CL_GOS_SERVICE
  create public .

public section.

  methods EXECUTE
    redefinition .
protected section.

  methods CHECK_STATUS
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_GOS_SRV_SCHED_ATTEST IMPLEMENTATION.


  METHOD check_status.
    CASE is_lporb-typeid.
      WHEN 'PDOTYPE_O'.
        ep_status = mp_status_active.
      WHEN OTHERS.
        ep_status = MP_STATUS_INVISIBLE.
    ENDCASE.
  ENDMETHOD.


  METHOD execute.
  ENDMETHOD.
ENDCLASS.
