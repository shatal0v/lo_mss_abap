class ZCL_MSS_COMPETITION_DPC_EXT definition
  public
  inheriting from ZCL_MSS_COMPETITION_DPC
  create public .

public section.
protected section.

  methods REQUESTSET_CREATE_ENTITY
    redefinition .
  methods REQUESTSET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_MSS_COMPETITION_DPC_EXT IMPLEMENTATION.


METHOD requestset_create_entity.

  DATA: ls_input_data  TYPE zshr_mss_ws_competition,
        ls_output_data TYPE zshr_mss_ws_competition,
        lo_mss         TYPE REF TO zcl_mss_data_assistent.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_input_data ).

  lo_mss = NEW #( ).
  if ls_input_data-func eq 'CREATE'.
  lo_mss->ws_competition_request_create( EXPORTING is_input_data  = ls_input_data
                                         IMPORTING es_output_data = ls_output_data ).
  else.
   lo_mss->ws_competition_request_withdr( EXPORTING is_input_data  = ls_input_data
                                         IMPORTING es_output_data = ls_output_data ).

 endif.


  MOVE-CORRESPONDING ls_output_data TO er_entity.

ENDMETHOD.


METHOD requestset_update_entity.

  DATA: ls_input_data  TYPE zshr_mss_ws_competition,
        ls_output_data TYPE zshr_mss_ws_competition,
        lo_mss         TYPE REF TO zcl_mss_data_assistent.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_input_data ).

  lo_mss = NEW #( ).
  lo_mss->ws_competition_request_withdr( EXPORTING is_input_data  = ls_input_data
                                         IMPORTING es_output_data = ls_output_data ).

  MOVE-CORRESPONDING ls_output_data TO er_entity.

ENDMETHOD.
ENDCLASS.
