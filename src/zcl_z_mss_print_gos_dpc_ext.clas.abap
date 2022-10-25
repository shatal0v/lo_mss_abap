class ZCL_Z_MSS_PRINT_GOS_DPC_EXT definition
  public
  inheriting from ZCL_Z_MSS_PRINT_GOS_DPC
  create public .

public section.

  methods GET_GOS_XSTRING
    importing
      !IT_KEY type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      !E_XSTRING type XSTRING
      !E_FILENAME type STRING
      !E_MIMETYPE type STRING
      !RETURN type BAPIRET2
    raising
      CX_OBL_MODEL_ERROR
      CX_OBL_PARAMETER_ERROR .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_MSS_PRINT_GOS_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.
    DATA: ls_stream     TYPE ty_s_media_resource
           , ls_lheader    TYPE ihttpnvp
           .

    DATA: lt_return TYPE bapiret2_t
       , return TYPE  bapiret2
.
    IF 1 = 1 .

    ENDIF.
    CASE iv_entity_name.
      WHEN 'Print_Gos'.

        get_gos_xstring(
          EXPORTING
            it_key    = it_key_tab
          IMPORTING
            e_xstring = ls_stream-value
            e_mimetype =  ls_stream-mime_type
            e_filename = DATA(lv_file)
            return = return
            ).

        IF return IS NOT INITIAL.
          DATA(lo_message_container) = mo_context->get_message_container( ).
          APPEND return TO lt_return.
          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        lv_file = cl_http_utility=>if_http_utility~escape_url( lv_file ).
        ls_lheader-name = 'Content-Disposition'.
        ls_lheader-value = 'inline; filename="' && lv_file && '";'.
*        ls_lheader-value = 'inline; filename="' && lv_file && '";'.

        set_header( is_header = ls_lheader ).
*                ls_lheader-name = 'Content-Type'.
*        ls_lheader-value = 'charset=UTF-8'.
*
*        set_header( is_header = ls_lheader ).

    ENDCASE.
*
    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).
  ENDMETHOD.


  METHOD get_gos_xstring.
    DATA: l_fs TYPE i,   l_fl TYPE i, l_ll TYPE i.

    DATA: v_len TYPE so_obj_len.
    DATA: v_len_num TYPE i.
    DATA: lv_file_ext(10) TYPE c.

    DATA: ls_bor   TYPE  borident,
          lv_plvar TYPE plvar
          .
    DATA:
      input_length TYPE i,
      lin          TYPE i.
    DATA :
      doc_data1  TYPE sofolenti1,
      lv_doc_id  TYPE so_entryid,
      lt_objhead TYPE TABLE OF  solisti1,
      lt_hex     TYPE TABLE OF solix,
      lt_objcont TYPE TABLE OF solisti1.
    DATA:
      lt_relation_options TYPE obl_t_relt,
      ls_relation_option  TYPE obl_s_relt,
      ls_object           TYPE  sibflporb,
      lt_links            TYPE obl_t_link.
    DATA: BEGIN OF ls_input
              , main_user     TYPE string
              , entrance_user TYPE string
              , otype TYPE string
              , objid TYPE string
              , doctype TYPE string
              , END OF ls_input

              , lr_user TYPE RANGE OF string
              .

    DATA: content_info TYPE TABLE OF scms_acinf
        , content_char TYPE TABLE OF sdokcntasc
        , wa_content_info TYPE scms_acinf
        , content_bin  TYPE TABLE OF sdokcntbin
        .

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    LOOP AT it_key ASSIGNING FIELD-SYMBOL(<fs_key>).
      CASE <fs_key>-name.
        WHEN 'ENTRANCE_USER'.
          ls_input-entrance_user = <fs_key>-value.
        WHEN 'MAIN_USER'.
          ls_input-main_user = <fs_key>-value.
        WHEN 'OTYPE'.
          ls_input-otype = <fs_key>-value.
        WHEN 'OBJID'.
          ls_input-objid = <fs_key>-value.
        WHEN 'DOCTYPE'.
          ls_input-doctype = <fs_key>-value.
      ENDCASE.
    ENDLOOP.

    IF ls_input-entrance_user IS INITIAL.
      ls_input-entrance_user = ls_input-main_user.
    ENDIF.

    CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
      IMPORTING
        act_plvar       = lv_plvar
      EXCEPTIONS
        no_active_plvar = 1
        OTHERS          = 2.
    IF ls_input-doctype EQ '1'.
      lo_assistent->ws_get_entrance_user( EXPORTING iv_muser    = ls_input-entrance_user
                                           IMPORTING et_entrance = DATA(lt_entrance) ).
      IF sy-subrc = 0.
        LOOP AT lt_entrance ASSIGNING FIELD-SYMBOL(<lt_entrance>).
          ls_input-otype = 'O'.
          ls_input-objid = <lt_entrance>-orgeh.
          EXIT.
        ENDLOOP.
      ELSE.
        MESSAGE e017(zhr_mss) INTO return-message WITH ls_input-entrance_user.
        return-type   = sy-msgty.
        return-id     = sy-msgid.
        return-number = sy-msgno.
        EXIT.
      ENDIF.

      DATA(lv_object) = lv_plvar &&   ls_input-objid.

      SELECT ftype,
              object,
              seqnr
         FROM zthr_mss_at_attr
         INTO TABLE @DATA(lt_attr)
         WHERE ftype  = 'GA'
           AND object = @lv_object
         ORDER BY datum, uzeit.

      LOOP AT lt_attr ASSIGNING FIELD-SYMBOL(<lt_attr>).
      ENDLOOP.

      IF sy-subrc <> 0 OR <lt_attr> IS NOT ASSIGNED.
        MESSAGE e016(zhr_mss) INTO return-message.
        return-type   = sy-msgty.
        return-id     = sy-msgid.
        return-number = sy-msgno.
        EXIT.
      ENDIF.

      lo_assistent->file_download( EXPORTING iv_ftype   = <lt_attr>-ftype
                                 iv_object  = <lt_attr>-object
                                 iv_seqnr   = <lt_attr>-seqnr
                       IMPORTING es_content = e_xstring
                                 es_attr    = DATA(ls_attr) ).

      CHECK e_xstring IS NOT INITIAL.

      CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
        EXPORTING
          filename  = ls_attr-fname
          uppercase = 'X'
        IMPORTING
          extension = doc_data1-obj_type.

      SELECT SINGLE  mimetype INTO e_mimetype FROM toadd WHERE doc_type EQ  doc_data1-obj_type .

      e_filename =  ls_attr-fname.

    ELSE.

      ls_object-instid = lv_plvar &&   ls_input-objid .
      ls_object-catid = 'BO'.
      CASE  ls_input-otype.
        WHEN  'O'.
          ls_object-typeid = 'PDOTYPE_O'.
        WHEN  'S'.
          ls_object-typeid = 'PDOTYPE_S'. "ls_bor-objtype = 'PDOTYPE_SH'.
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
              is_object           = ls_object
              it_relation_options = lt_relation_options
              ip_role             = 'GOSAPPLOBJ'
              ip_no_buffer        = 'X'
            IMPORTING
              et_links            = lt_links.
        CATCH cx_obl_model_error .
        CATCH cx_obl_parameter_error.
        CATCH cx_obl_internal_error.
      ENDTRY.
      IF lines( lt_links ) > 0.

        SORT lt_links BY utctime DESCENDING.
        READ TABLE lt_links INTO DATA(ls_link) INDEX 1.
        lv_doc_id = ls_link-instid_b.
        CALL FUNCTION 'SO_DOCUMENT_READ_API1'
          EXPORTING
            document_id                = lv_doc_id
*           FILTER                     = 'X '
          IMPORTING
            document_data              = doc_data1
          TABLES
            object_header              = lt_objhead
            object_content             = lt_objcont
            contents_hex               = lt_hex
          EXCEPTIONS
            document_id_not_exist      = 1
            operation_no_authorization = 2
            x_error                    = 3
            OTHERS                     = 4.
        IF sy-subrc <> 0.
          MESSAGE e016(zhr_mss) INTO return-message.
          return-type   = sy-msgty.
          return-id     = sy-msgid.
          return-number = sy-msgno.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE e016(zhr_mss) INTO return-message.
        return-type   = sy-msgty.
        return-id     = sy-msgid.
        return-number = sy-msgno.
        EXIT.
      ENDIF.

      e_xstring = cl_bcs_convert=>solix_to_xstring(
          it_solix   = lt_hex
          iv_size    = CONV #( doc_data1-doc_size )
      ).
*      DESCRIBE TABLE lt_hex LINES lin.
*      input_length = lin * sy-tleng .
*      v_len_num = input_length.
**conversion to xstring from binary.
*      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*        EXPORTING
*          input_length = v_len_num
*        IMPORTING
*          buffer       = e_xstring
*        TABLES
*          binary_tab   = lt_hex.
      CLEAR lv_file_ext.
      READ TABLE lt_objhead INTO DATA(ls_objhead) INDEX 1.
      IF sy-subrc EQ 0.
        REPLACE ALL OCCURRENCES OF '&SO_FILENAME=' IN ls_objhead-line WITH ''.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
            EXPORTING
              filename  = ls_objhead-line
              uppercase = 'X'
            IMPORTING
              extension = lv_file_ext.
        ENDIF.
      ENDIF.


      IF lv_file_ext IS NOT INITIAL.

        SELECT SINGLE  mimetype INTO e_mimetype FROM toadd WHERE doc_type EQ  lv_file_ext.
        CONCATENATE  doc_data1-obj_descr  '.' lv_file_ext INTO e_filename.
      ELSE.
        SELECT SINGLE  mimetype INTO e_mimetype FROM toadd WHERE doc_type EQ  doc_data1-obj_type .
        CONCATENATE doc_data1-obj_descr '.' doc_data1-obj_type INTO e_filename.
      ENDIF.




    ENDIF.


  ENDMETHOD.
ENDCLASS.
