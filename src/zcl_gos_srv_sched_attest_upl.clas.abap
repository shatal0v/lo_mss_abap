class ZCL_GOS_SRV_SCHED_ATTEST_UPL definition
  public
  inheriting from ZCL_GOS_SRV_SCHED_ATTEST
  create public .

public section.

  methods EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GOS_SRV_SCHED_ATTEST_UPL IMPLEMENTATION.


  METHOD execute.
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    CASE lo_assistent->file_upload( iv_object = CONV #( gs_lporb-instid ) iv_ftype = 'GA' iv_unseq = abap_false ).
      WHEN 0.
        MESSAGE s003(zhr_mss).
      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
