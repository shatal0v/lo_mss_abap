class ZCL_Z_MSS_ENTRANCE_USE_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_ENTRANCE_USE_DPC
  create public .

public section.
protected section.

  methods ENTRANCE_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_ENTRANCE_USE_DPC_EXT IMPLEMENTATION.


  METHOD entrance_get_entityset.
    DATA: BEGIN OF ls_input
            , main_user     TYPE string
            , entrance_user TYPE string
            , END OF ls_input

            , lr_user TYPE RANGE OF string
            .
    TRY.
        DATA(lo_filter) = io_tech_request_context->get_filter( ).

        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<it_filter>).
          DATA(lv_property) = <it_filter>-property.
          TRANSLATE lv_property TO UPPER CASE.
          ASSIGN COMPONENT <it_filter>-property OF STRUCTURE ls_input TO FIELD-SYMBOL(<fs_input>).
          CHECK <fs_input> IS ASSIGNED AND sy-subrc = 0.

          lo_filter->convert_select_option( EXPORTING is_select_option = <it_filter>
                                            IMPORTING et_select_option = lr_user ).
          READ TABLE lr_user INTO DATA(ls_user) INDEX 1.
          <fs_input> = ls_user-low.
        ENDLOOP.
        IF  ls_input-main_user IS INITIAL .
          ls_input-main_user = ls_input-entrance_user.
        ENDIF.
        IF ls_input-main_user IS INITIAL .
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset0>).

          <et_entityset0>-msgid = 'E'.
          <et_entityset0>-message = 'MAIN USER missing'.

        ENDIF.
        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

        lo_assistent->ws_get_entrance_user( EXPORTING iv_muser    = ls_input-main_user
                                            IMPORTING et_entrance = DATA(lt_entrance) ).
        IF NOT sy-subrc = 0 OR lines( lt_entrance ) EQ 0.
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset1>).

          <et_entityset1>-msgid = 'E'.
          MESSAGE e010(zhr_mss) INTO  <et_entityset1>-message .

*          DATA: lt_return TYPE bapiret2_t
*              .
*          DATA(lo_message_container) = mo_context->get_message_container( ).
*
*          APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
*          MESSAGE e000(zhr_mss) INTO <lt_return>-message.
*
*          <lt_return>-type   = sy-msgty.
*          <lt_return>-id     = sy-msgid.
*          <lt_return>-number = sy-msgno.
*
*          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*
*          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*            EXPORTING
*              message_container = lo_message_container.
        ENDIF.

        LOOP AT lt_entrance ASSIGNING FIELD-SYMBOL(<lt_entrance>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
          MOVE-CORRESPONDING <lt_entrance> TO <et_entityset>.
          <et_entityset>-msgid = 'S'.
          MESSAGE s011(zhr_mss) INTO  <et_entityset>-message .

        ENDLOOP.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.
ENDCLASS.