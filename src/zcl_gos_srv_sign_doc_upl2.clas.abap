class ZCL_GOS_SRV_SIGN_DOC_UPL2 definition
  public
  inheriting from ZCL_GOS_SRV_SIGN_DOC
  create public .

public section.

  methods EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GOS_SRV_SIGN_DOC_UPL2 IMPLEMENTATION.


  METHOD EXECUTE.
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    CASE lo_assistent->file_upload( iv_object = CONV #( gs_lporb-instid ) iv_ftype = 'LE' iv_unseq = abap_false ).
      WHEN 0.
        MESSAGE s003(zhr_mss).
      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
