class ZCL_Z_MSS_F1GS_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_F1GS_DPC
  create public .

public section.

  constants C_MEMORY_ID type TEXT20 value 'Z_F1GS' ##NO_TEXT.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.
private section.

  methods GET_REPORT_XML
    importing
      !IT_KEY type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      !E_XSTRING type XSTRING
      !E_FILENAME type STRING .
ENDCLASS.



CLASS ZCL_Z_MSS_F1GS_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.
    DATA: ls_stream     TYPE ty_s_media_resource
           , ls_lheader    TYPE ihttpnvp
           .

    CASE iv_entity_name.
      WHEN 'Emp_F1gs'.
        ls_stream-mime_type = 'application/vnd.ms-excel'.

        get_report_xml(
          EXPORTING
            it_key    = it_key_tab
          IMPORTING
            e_xstring = ls_stream-value
            e_filename = DATA(lv_file) ).

        ls_lheader-name = 'Content-Disposition'.
        ls_lheader-value = 'inline; filename="' && lv_file && '";'.
        set_header( is_header = ls_lheader ).
    ENDCASE.
*
    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).
  ENDMETHOD.


  METHOD GET_REPORT_XML.
    DATA: BEGIN OF ls_input
          , main_user     TYPE string
          , entrance_user TYPE string
          , END OF ls_input

          , lr_user TYPE RANGE OF string
          .
    LOOP AT it_key ASSIGNING FIELD-SYMBOL(<fs_key>).
      CASE <fs_key>-name.
        WHEN 'ENTRANCE_USER'.
          ls_input-entrance_user = <fs_key>-value.
        WHEN 'MAIN_USER'.
          ls_input-main_user = <fs_key>-value.

      ENDCASE.
    ENDLOOP.
    IF ls_input-entrance_user IS INITIAL.
      ls_input-entrance_user = ls_input-main_user.
    ENDIF.
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    lo_assistent->ws_get_f1gs_xml(  EXPORTING  iv_euser       = ls_input-entrance_user
                                    IMPORTING  e_xstring       = e_xstring
                                    EXCEPTIONS no_pernr       = 1
                                               no_sec_lev_ruk = 2 ).
    e_filename = 'F1GS.xlsx'.

  ENDMETHOD.
ENDCLASS.
