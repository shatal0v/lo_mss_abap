class ZCL_MSS_LIST_IPR_DPC_EXT definition
  public
  inheriting from ZCL_MSS_LIST_IPR_DPC
  create public .

public section.
protected section.

  methods LISTIPRSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_MSS_LIST_IPR_DPC_EXT IMPLEMENTATION.


METHOD listiprset_get_entityset.

  DATA: lo_mss        TYPE REF TO zcl_mss_data_assistent,
        ls_input_data TYPE zcl_mss_data_assistent=>gtys_input_data.

  lo_mss = NEW #( ).

  lo_mss->get_input_filter( EXPORTING io_tech_request_context  = io_tech_request_context
                                      it_filter_select_options = it_filter_select_options
                            IMPORTING es_input_data            = ls_input_data ).

  lo_mss->ws_get_list_ipr( EXPORTING is_input_data  = ls_input_data
                           IMPORTING et_output_data = DATA(lt_output)
                             EXCEPTIONS no_pernr       = 1
                                        no_sec_lev_ruk = 2
                                        pernr_inactive = 3
                                                   OTHERS         = 99 ).
  APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset0>).

  CASE sy-subrc.
    WHEN 1.

      <et_entityset0>-msgid        = 'E'.
      MESSAGE e015(zhr_mss) WITH ls_input_data-entrance_user INTO <et_entityset0>-message.
      EXIT.
    WHEN 2.

      <et_entityset0>-msgid        = 'E'.
      MESSAGE e017(zhr_mss) WITH ls_input_data-entrance_user INTO <et_entityset0>-message.
      EXIT.
    WHEN 3.

      <et_entityset0>-msgid        = 'E'.
      MESSAGE e020(zhr_mss) WITH ls_input_data-entrance_user INTO <et_entityset0>-message.
      EXIT.
    WHEN OTHERS.
      REFRESH et_entityset.
  ENDCASE.
  MOVE-CORRESPONDING lt_output TO et_entityset.

ENDMETHOD.
ENDCLASS.
