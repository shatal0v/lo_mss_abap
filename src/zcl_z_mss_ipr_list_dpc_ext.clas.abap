class ZCL_Z_MSS_IPR_LIST_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_IPR_LIST_DPC
  create public .

public section.
protected section.

  methods IPR_LIST_SET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_IPR_LIST_DPC_EXT IMPLEMENTATION.


  METHOD ipr_list_set_get_entityset.

    DATA: BEGIN OF ls_input
              , main_user     TYPE string
              , entrance_user TYPE string
              , req_id TYPE string
              , END OF ls_input

              , lr_user TYPE RANGE OF string
              .

    TRY.
        DATA(lo_filter) = io_tech_request_context->get_filter( ).

        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<it_filter>).
          REFRESH lr_user.
          DATA(lv_property) = <it_filter>-property.
          TRANSLATE lv_property TO UPPER CASE.
          ASSIGN COMPONENT <it_filter>-property OF STRUCTURE ls_input TO FIELD-SYMBOL(<fs_input>).
          CHECK <fs_input> IS ASSIGNED AND sy-subrc = 0.

          lo_filter->convert_select_option( EXPORTING is_select_option = <it_filter>
                                            IMPORTING et_select_option = lr_user ).
          READ TABLE lr_user INTO DATA(ls_user) INDEX 1.
          <fs_input> = ls_user-low.
        ENDLOOP.


        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

        lo_assistent->ws_get_ipr_list( EXPORTING  iv_euser       = ls_input-entrance_user
                                                    iv_req_id      = ls_input-req_id
                                        IMPORTING   et_ipr_list       = DATA(lt_ipr_list)
                                        EXCEPTIONS no_pernr       = 1
                                                   no_sec_lev_ruk = 2 ).
        DATA: lt_return            TYPE bapiret2_t
            , lo_message_container TYPE REF TO /iwbep/if_message_container
         .
        APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).

        CASE sy-subrc.
          WHEN 1.
            MESSAGE e002(zhr_mss) WITH ls_input-entrance_user INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
          WHEN 2.
            DATA(lv_name) = lo_assistent->get_user_fio( iv_usrid = ls_input-entrance_user ).
            MESSAGE e001(zhr_mss) WITH lv_name INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
          WHEN OTHERS.
            REFRESH lt_return.
        ENDCASE.
        .
        IF NOT lt_return IS INITIAL.
          lo_message_container = mo_context->get_message_container( ).
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        LOOP AT lt_ipr_list ASSIGNING FIELD-SYMBOL(<lt_ipr_list>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
          MOVE-CORRESPONDING <lt_ipr_list> TO <et_entityset>.
        ENDLOOP.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
