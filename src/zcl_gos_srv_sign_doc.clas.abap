class ZCL_GOS_SRV_SIGN_DOC definition
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



CLASS ZCL_GOS_SRV_SIGN_DOC IMPLEMENTATION.


  METHOD CHECK_STATUS.
    CASE is_lporb-typeid.
      WHEN 'BUS1065'.
        ep_status = mp_status_active.
      WHEN OTHERS.
        ep_status = MP_STATUS_INVISIBLE.
    ENDCASE.
  ENDMETHOD.


  METHOD EXECUTE.
  ENDMETHOD.
ENDCLASS.
