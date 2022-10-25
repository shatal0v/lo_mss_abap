class ZCL_Z_MSS_ORG_STRUCTUR_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_ORG_STRUCTUR_DPC
  create public .

public section.
protected section.

  methods STRUCTURE_GET_ENTITYSET
    redefinition .
private section.

  methods _HAS_FILE
    importing
      !IS_OBJECT type HRROOTOB
    returning
      value(RV_HAS_FILE) type FLAG .
ENDCLASS.



CLASS ZCL_Z_MSS_ORG_STRUCTUR_DPC_EXT IMPLEMENTATION.


  METHOD structure_get_entityset.

    DATA: BEGIN OF ls_input
        , main_user     TYPE string
        , entrance_user TYPE string
        , xss           TYPE string
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

          REFRESH lr_user.
        ENDLOOP.

        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).


        "21/03/2020
        IF ls_input-entrance_user IS NOT INITIAL.
          lo_assistent->ws_get_structure( EXPORTING  iv_euser       = ls_input-entrance_user
                                                      iv_xss         = ls_input-xss
                                          IMPORTING  et_struc       = DATA(lt_struc)
                                          EXCEPTIONS no_pernr       = 1
                                                     no_sec_lev_ruk = 2
                                                     pernr_inactive = 3
                                                     OTHERS         = 99 ).
        ELSE.
          lo_assistent->ws_get_structure( EXPORTING  iv_euser       = ls_input-main_user
                                                      iv_xss         = ls_input-xss
                                           IMPORTING  et_struc       = lt_struc
                                           EXCEPTIONS no_pernr       = 1
                                                      no_sec_lev_ruk = 2
                                                      pernr_inactive = 3
                                                      OTHERS         = 99 ).
        ENDIF.

        DATA: lt_return            TYPE bapiret2_t
            , lo_message_container TYPE REF TO /iwbep/if_message_container
            .
        APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset0>).

        CASE sy-subrc.
          WHEN 1.
            IF ls_input-entrance_user IS NOT INITIAL.
              MESSAGE e002(zhr_mss) WITH ls_input-entrance_user INTO <lt_return>-message.
            ELSE.
              MESSAGE e002(zhr_mss) WITH ls_input-main_user INTO <lt_return>-message.
            ENDIF.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid        = 'E'.

            IF ls_input-entrance_user IS NOT INITIAL.
              MESSAGE e015(zhr_mss) WITH ls_input-entrance_user INTO <et_entityset0>-message.
            ELSE.
              MESSAGE e015(zhr_mss) WITH ls_input-main_user INTO <lt_return>-message.
            ENDIF.
          WHEN 2.
            IF ls_input-entrance_user IS NOT INITIAL.
              DATA(lv_name) = lo_assistent->get_user_fio( iv_usrid = ls_input-entrance_user ).
            ELSE.
              lv_name = lo_assistent->get_user_fio( iv_usrid = ls_input-main_user ).
            ENDIF.

            MESSAGE e001(zhr_mss) WITH lv_name INTO <lt_return>-message.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid        = 'E'.
            MESSAGE e017(zhr_mss) WITH lv_name INTO <et_entityset0>-message.

          WHEN 3.
            IF ls_input-entrance_user IS NOT INITIAL.
              MESSAGE e020(zhr_mss) WITH ls_input-entrance_user INTO <lt_return>-message.
            ELSE.
              MESSAGE e020(zhr_mss) WITH ls_input-main_user INTO <lt_return>-message.
            ENDIF.
            <lt_return>-type   = sy-msgty.
            <lt_return>-id     = sy-msgid.
            <lt_return>-number = sy-msgno.
            <et_entityset0>-msgid        = 'E'.
            IF ls_input-entrance_user IS NOT INITIAL.
              MESSAGE e020(zhr_mss) WITH ls_input-entrance_user INTO <et_entityset0>-message.
            ELSE.
              MESSAGE e020(zhr_mss) WITH ls_input-main_user INTO <lt_return>-message.
            ENDIF.
          WHEN OTHERS.
            REFRESH lt_return.
            REFRESH et_entityset.
        ENDCASE.
        .
*        IF NOT lt_return IS INITIAL.
*          lo_message_container = mo_context->get_message_container( ).
*          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*
*          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*            EXPORTING
*              message_container = lo_message_container.
*        ENDIF.

        LOOP AT lt_struc ASSIGNING FIELD-SYMBOL(<lt_struc>).
          APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
          <et_entityset>-hasfile      = _has_file( CORRESPONDING #( <lt_struc>-object ) ).
          <et_entityset>-otype        = <lt_struc>-object-otype.
          <et_entityset>-objid        = <lt_struc>-object-objid.
          <et_entityset>-otype_parent = <lt_struc>-parent-otype.
          <et_entityset>-objid_parent = <lt_struc>-parent-objid.
          <et_entityset>-desc         = <lt_struc>-descr.
          <et_entityset>-reg_date     = <lt_struc>-rdate.
          <et_entityset>-subst_date   = <lt_struc>-sdate.
          <et_entityset>-msgid        = 'S'.
          MESSAGE s012(zhr_mss)  INTO <et_entityset>-message.
        ENDLOOP.
*      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


  METHOD _has_file.
    DATA: ls_object TYPE sibflporb,
          lt_relation_options TYPE obl_t_relt,
          ls_relation_option  TYPE obl_s_relt.
    ls_object-instid = cl_hap_pmp_const=>plvar && is_object-objid.
    ls_object-catid = 'BO'.
    CASE is_object-otype.
      WHEN cl_hap_pmp_const=>otype_orgunit.
        ls_object-typeid = 'PDOTYPE_O'.
      WHEN cl_hap_pmp_const=>otype_position.
        ls_object-typeid = 'PDOTYPE_S'.
    ENDCASE.
    ls_relation_option-sign = 'I'.
    ls_relation_option-option = 'EQ'.
    ls_relation_option-low = 'ATTA'.
    APPEND ls_relation_option TO lt_relation_options.
    ls_relation_option-low = 'NOTE'.
    APPEND ls_relation_option TO lt_relation_options.
    ls_relation_option-low = 'URL'.
    APPEND ls_relation_option TO lt_relation_options.
    TRY.
        CALL METHOD cl_binary_relation=>read_links_of_binrels
          EXPORTING
            is_object    = ls_object
            ip_role      = 'GOSAPPLOBJ'
            it_relation_options = lt_relation_options
            ip_no_buffer = 'X'
          IMPORTING
            et_links     = DATA(lt_links).
      CATCH cx_obl_model_error .
      CATCH cx_obl_parameter_error.
      CATCH cx_obl_internal_error.
    ENDTRY.
    rv_has_file = boolc( lines( lt_links ) > 0 ).
  ENDMETHOD.
ENDCLASS.
