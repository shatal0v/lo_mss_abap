*&---------------------------------------------------------------------*
*&  Include           ZHR_MSS_D001_LCL
*&---------------------------------------------------------------------*
CLASS lcl_mss_d001 DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_uname TYPE syuname DEFAULT sy-uname
           , get_data
           , alv
           .
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_line
         , req_id               TYPE zhr_xss_iprreq-req_id

         , date_create         TYPE  zhr_xss_iprreq-date_create
         , time_create         TYPE  zhr_xss_iprreq-time_create
         , approver_number     TYPE  zhr_xss_iprreq-approver_number
         , data_change         TYPE  zhr_xss_iprreq-data_change
         , time_change         TYPE  zhr_xss_iprreq-time_change
         , uname               TYPE  zhr_xss_iprreq-uname

         , pernr_creator        TYPE zhr_xss_iprreq-pernr_creator
         , status               TYPE zhr_xss_iprreq-status
         , status_txt           TYPE dd07d-ddtext
         , edu_type             TYPE zhr_xss_iprreq-edu_type
         , edu_type_txt         TYPE t517t-zztext
         , edu_spec             TYPE zhr_xss_iprreq-edu_spec
         , edu_spec_txt         TYPE zthr_dlinet-stext
         , edu_spec_another     TYPE zhr_xss_iprreq-edu_spec_another
         , edu_frm              TYPE zhr_xss_iprreq-edu_frm
         , edu_frm_txt          TYPE t7ruokin-cname
         , edu_reas             TYPE zess_t_reas_txt-reason_id
         , edu_reas_txt         TYPE zess_t_reas_txt-text
         , edu_length           TYPE zhr_xss_iprreq-edu_length
         , edu_length_type      TYPE zhr_xss_iprreq-edu_length_type
         , edu_length_type_txt  TYPE t538t-etext
         , edu_year             TYPE zhr_xss_iprreq-edu_year
         , edu_prog             TYPE zthr_edu_prograt-id
         , edu_prog_txt         TYPE zthr_edu_prograt-name
         , edu_result_another   TYPE zhr_xss_iprreq-edu_result_another
         , edu_name             TYPE zhr_xss_iprreq-edu_name
         , field_style          TYPE lvc_t_styl
         , END OF ty_line
         .

    DATA: mo_assistent TYPE REF TO zcl_mss_data_assistent
        , BEGIN OF ms_param
        , pernr  TYPE pernr_d
        , status TYPE RANGE OF zhr_xss_iprreq-status
        , END OF ms_param

        , mt_line TYPE TABLE OF ty_line
        , mo_alv  TYPE REF TO cl_gui_alv_grid
        , not_enabled_fields TYPE TABLE OF lvc_fname
        .
    CONSTANTS: cs_stable TYPE lvc_s_stbl    VALUE '11'
             .
    METHODS: on_toolbar           FOR EVENT toolbar             OF cl_gui_alv_grid IMPORTING e_object
           , on_toolbar_save      FOR EVENT toolbar             OF cl_gui_alv_grid IMPORTING e_object
           , on_toolbar_edit      FOR EVENT toolbar             OF cl_gui_alv_grid IMPORTING e_object
           , handle_data_changed  FOR EVENT data_changed        OF cl_gui_alv_grid IMPORTING er_data_changed
           , on_f4                FOR EVENT onf4                OF cl_gui_alv_grid IMPORTING sender
                                                                                          e_fieldname
                                                                                          e_fieldvalue
                                                                                          es_row_no
                                                                                          er_event_data
                                                                                          et_bad_cells
                                                                                          e_display
    , user_command      FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm sender

    .
ENDCLASS.

CLASS lcl_mss_d001 IMPLEMENTATION.
  METHOD constructor.
    mo_assistent = NEW #( ).

    ms_param-pernr = mo_assistent->get_pernr( iv_usrid = CONV #( iv_uname ) iv_subty = '0001' ).

    APPEND VALUE #( sign = `I` option = `EQ` low = `8` ) TO ms_param-status.

**    APPEND 'EDU_TYPE' TO enabled_fields.
**    APPEND 'EDU_SPEC' TO enabled_fields.
**    APPEND 'EDU_FRM' TO enabled_fields.
**    APPEND 'EDU_GOAL' TO enabled_fields.
**    APPEND 'EDU_LENGTH' TO enabled_fields.
**    APPEND 'EDU_RESULT' TO enabled_fields.
**    APPEND 'EDU_LENGTH_TYPE' TO enabled_fields.

    APPEND 'req_id'  TO  not_enabled_fields.
    APPEND 'pernr_creator'  TO  not_enabled_fields.
    APPEND 'status'  TO  not_enabled_fields.
    APPEND 'status_txt'  TO  not_enabled_fields.
*      APPEND 'edu_type'  TO  not_enabled_fields.
*    APPEND 'edu_type_txt'  TO  not_enabled_fields.
*      APPEND 'edu_spec'  TO  not_enabled_fields.
*    APPEND 'edu_spec_txt'  TO  not_enabled_fields.
    APPEND 'edu_spec_another'  TO  not_enabled_fields.
*      APPEND 'edu_frm'  TO  not_enabled_fields.
*    APPEND ' edu_frm_txt'  TO  not_enabled_fields.
*      APPEND 'edu_goal'  TO  not_enabled_fields.
*    APPEND 'edu_goal_txt'  TO  not_enabled_fields.
*      APPEND 'edu_length'  TO  not_enabled_fields.
*      APPEND 'edu_length_type'  TO  not_enabled_fields.
*    APPEND 'edu_length_type_txt'  TO  not_enabled_fields.
*    APPEND 'edu_year'  TO  not_enabled_fields.
*      APPEND 'edu_result'  TO  not_enabled_fields.
*    APPEND 'edu_result_txt'  TO  not_enabled_fields.
    APPEND 'edu_result_another'  TO  not_enabled_fields.
*    APPEND 'edu_name'            TO  not_enabled_fields.
    APPEND 'field_style'  TO  not_enabled_fields.

    LOOP AT not_enabled_fields ASSIGNING FIELD-SYMBOL(<not_enabled_fields>).
      TRANSLATE <not_enabled_fields> TO UPPER CASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_data.

    DATA:
          lt_length_type TYPE TABLE OF t538t.


    DATA: ls_stylerow TYPE lvc_s_styl.
    SELECT zhr_xss_iprreq~req_id
         , zhr_xss_iprreq~date_create
         , zhr_xss_iprreq~time_create
         , zhr_xss_iprreq~approver_number
         , zhr_xss_iprreq~data_change
         , zhr_xss_iprreq~time_change
         , zhr_xss_iprreq~uname
         , zhr_xss_iprreq~pernr_creator
         , zhr_xss_iprreq~status
         , zhr_xss_iprreq~edu_type
         , zess_0023_t_aedu~addedu_text AS edu_type_txt
         , zhr_xss_iprreq~edu_spec
         , zthr_dlinet~stext AS edu_spec_txt
         , zhr_xss_iprreq~edu_spec_another
         , zhr_xss_iprreq~edu_frm
         , t7ruokin~cname AS edu_frm_txt
         , zhr_xss_iprreq~edu_reas
         , zess_t_reas_txt~text AS edu_reas_txt
         , zhr_xss_iprreq~edu_length
         , zhr_xss_iprreq~edu_length_type
         , t538t~etext AS edu_length_type_txt
         , zhr_xss_iprreq~edu_year
         , zhr_xss_iprreq~edu_prog
         , zthr_edu_prograt~name AS edu_prog_txt
         , zhr_xss_iprreq~edu_result_another
         , zhr_xss_iprreq~edu_name

           INTO CORRESPONDING FIELDS OF TABLE @mt_line
           FROM zhr_xss_iprreq

           LEFT OUTER JOIN zess_0023_t_aedu ON zess_0023_t_aedu~addeduid = zhr_xss_iprreq~edu_type

           LEFT OUTER JOIN zthr_dlinet ON zthr_dlinet~dline    = zhr_xss_iprreq~edu_spec
                                      AND zthr_dlinet~spras    = @sy-langu

           LEFT OUTER JOIN t7ruokin         ON t7ruokin~molga          = `33`
                                           AND t7ruokin~facet          = `33`
                                           AND t7ruokin~ccode          = zhr_xss_iprreq~edu_frm

           LEFT OUTER JOIN zess_t_reas_txt  ON zess_t_reas_txt~reason_id = zhr_xss_iprreq~edu_reas
                                           AND zess_t_reas_txt~lang      = @sy-langu

           LEFT OUTER JOIN t538t            ON t538t~sprsl             = @sy-langu
                                           AND t538t~zeinh             = zhr_xss_iprreq~edu_length_type

           LEFT OUTER JOIN zthr_edu_prograt ON zthr_edu_prograt~id   = zhr_xss_iprreq~edu_prog
                                           AND zthr_edu_prograt~lang = @sy-langu

           WHERE zhr_xss_iprreq~approver_number = @ms_param-pernr
             AND zhr_xss_iprreq~status         IN @ms_param-status.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_length_type
      FROM t538t
     WHERE sprsl EQ 'RU'.

    SELECT dd07t~domvalue_l
         , dd07t~ddtext
         FROM dd07t
         INTO TABLE @DATA(lt_dd07t) WHERE dd07t~domname    = `ZHR_XSS_IPRREQ_STATUS`
                                      AND dd07t~ddlanguage = @sy-langu.
    SORT lt_dd07t BY domvalue_l.

    LOOP AT mt_line ASSIGNING FIELD-SYMBOL(<mt_line>).
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_stylerow INTO TABLE <mt_line>-field_style.
      DATA(lv_status) = CONV dd07t-domvalue_l( <mt_line>-status ). CONDENSE lv_status.

      DATA(lv_edu_length_type) = '00' && <mt_line>-edu_length_type.

      READ TABLE lt_length_type
        INTO DATA(ls_length_type)
        WITH KEY zeinh = lv_edu_length_type.

      IF sy-subrc = 0.
        <mt_line>-edu_length_type_txt = ls_length_type-etext.
      ENDIF.

      READ TABLE lt_dd07t ASSIGNING FIELD-SYMBOL(<lt_dd07t>) WITH KEY domvalue_l = lv_status BINARY SEARCH.
      CHECK sy-subrc = 0 AND <lt_dd07t> IS ASSIGNED.
      <mt_line>-status_txt = <lt_dd07t>-ddtext.
    ENDLOOP.
  ENDMETHOD.
  METHOD alv.
    DATA: lt_fieldcat      TYPE lvc_t_fcat
        , lt_fieldcat_buff LIKE lt_fieldcat
        , ls_layout        TYPE lvc_s_layo
        , lo_struct        TYPE REF TO cl_abap_structdescr
        , ls_variant       TYPE disvariant
        , lo_cnt           TYPE REF TO cl_gui_custom_container
        , gt_f4            TYPE lvc_t_f4
        , gs_f4            TYPE lvc_s_f4
        .
    DO 10 TIMES.
      CASE  sy-index.
        WHEN 1. DATA(lv_tabname) = CONV dd02l-tabname( `zhr_xss_iprreq` ). DATA(lv_name_old) = CONV ltext( `` ). DATA(lv_name_new) = CONV ltext( `` ).
        WHEN 2. lv_tabname = `t517t`.            lv_name_old = `zztext`.    lv_name_new =  `edu_type_txt`.
        WHEN 3. lv_tabname = `zthr_dlinet`.      lv_name_old = `stext`.     lv_name_new =  `edu_spec_txt`.
        WHEN 4. lv_tabname = `t7ruokin`.         lv_name_old = `cname`.     lv_name_new =  `edu_frm_txt`.
        WHEN 5. lv_tabname = `ZESS_T_REAS_TXT`.  lv_name_old = `TEXT`.      lv_name_new =  `EDU_REAS_TXT`.
        WHEN 6. lv_tabname = `t538t`.            lv_name_old = `etext`.     lv_name_new =  `edu_length_type_txt`.
        WHEN 7. lv_tabname = `ZTHR_EDU_PROGRAT`. lv_name_old = `NAME`.      lv_name_new =  `edu_prog_txt`.
        WHEN 8. lv_tabname = `dd07t`.            lv_name_old = `ddtext`.    lv_name_new =  `status_txt`.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      REFRESH: lt_fieldcat_buff
             .
      TRANSLATE: lv_tabname  TO UPPER CASE
               , lv_name_old TO UPPER CASE
               , lv_name_new TO UPPER CASE
               .
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = lv_tabname
        CHANGING
          ct_fieldcat            = lt_fieldcat_buff
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.

      SORT lt_fieldcat_buff BY fieldname.

      DELETE lt_fieldcat_buff WHERE fieldname EQ 'DATE_CREATE' OR
                                    fieldname EQ 'TIME_CREATE' OR
                                    fieldname EQ 'APPROVER_NUMBER' OR
                                    fieldname EQ 'DATA_CHANGE' OR
                                    fieldname EQ 'TIME_CHANGE' OR
                                    fieldname EQ 'UNAME'.

      READ TABLE lt_fieldcat_buff ASSIGNING FIELD-SYMBOL(<lt_fieldcat_buff>) WITH KEY fieldname = lv_name_old BINARY SEARCH.
      IF sy-subrc = 0 AND <lt_fieldcat_buff> IS ASSIGNED.
        <lt_fieldcat_buff>-fieldname = lv_name_new.
      ENDIF.

      SORT lt_fieldcat_buff BY fieldname.

      DATA: ls_line LIKE LINE OF mt_line
          .

      lo_struct ?= cl_abap_tabledescr=>describe_by_data( ls_line ).

      LOOP AT lo_struct->components ASSIGNING FIELD-SYMBOL(<components>).
        DATA(lv_tabix) = sy-tabix.
        READ TABLE lt_fieldcat_buff ASSIGNING <lt_fieldcat_buff> WITH KEY fieldname = <components>-name BINARY SEARCH.
        CHECK sy-subrc = 0 AND <lt_fieldcat_buff> IS ASSIGNED.

        APPEND <lt_fieldcat_buff> TO lt_fieldcat ASSIGNING FIELD-SYMBOL(<lt_fieldcat>).
        <lt_fieldcat>-col_pos = lv_tabix.
        <lt_fieldcat>-no_out  = abap_false.
      ENDLOOP.
    ENDDO.

    SORT lt_fieldcat BY col_pos.

    LOOP AT lt_fieldcat ASSIGNING <lt_fieldcat>.
      <lt_fieldcat>-f4availabl = abap_true.
      LOOP AT not_enabled_fields ASSIGNING FIELD-SYMBOL(<not_enabled_fields>).
        CHECK <lt_fieldcat>-fieldname = <not_enabled_fields>.
        CLEAR <lt_fieldcat>-f4availabl.
      ENDLOOP.
      CASE <lt_fieldcat>-fieldname.
        WHEN `REQ_ID`.
          <lt_fieldcat>-no_out  = abap_true.
          CONTINUE.
        WHEN `EDU_LENGTH` OR 'EDU_YEAR' OR 'EDU_NAME'.
          CLEAR <lt_fieldcat>-f4availabl.
        WHEN OTHERS.

      ENDCASE.

      READ TABLE lt_fieldcat ASSIGNING FIELD-SYMBOL(<lt_fieldcat2>) INDEX sy-tabix + 1.
      CHECK <lt_fieldcat2> IS ASSIGNED AND sy-subrc = 0.
      DATA(lv_fieldname) = <lt_fieldcat>-fieldname && `_TXT`.
      CHECK <lt_fieldcat2>-fieldname = lv_fieldname.
      <lt_fieldcat>-no_out  = abap_true.
*      <lt_fieldcat2>-scrtext_l = <lt_fieldcat>-scrtext_l.
*      <lt_fieldcat2>-scrtext_m = <lt_fieldcat>-scrtext_m.
*      <lt_fieldcat2>-scrtext_s = <lt_fieldcat>-scrtext_s.
*      <lt_fieldcat2>-reptext   = <lt_fieldcat>-reptext.
    ENDLOOP.

    ls_layout-cwidth_opt = abap_true.

    CREATE OBJECT lo_cnt
      EXPORTING
        container_name              = 'GRID'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT mo_alv
      EXPORTING
        i_parent          = lo_cnt
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    gs_f4-register   = 'X'.
    gs_f4-getbefore  = space.
    gs_f4-chngeafter = space.
    gs_f4-fieldname  = 'EDU_TYPE'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_TYPE_TXT'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_SPEC'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_SPEC_TXT'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_FRM'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_FRM_TXT'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_GOAL'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_GOAL_TXT'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_LENGTH_TYPE'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_LENGTH_TYPE_TXT'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_RESULT'.
    INSERT gs_f4 INTO TABLE gt_f4.
    gs_f4-fieldname  = 'EDU_RESULT_TXT'.
    INSERT gs_f4 INTO TABLE gt_f4.

    CALL METHOD mo_alv->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4.

    CALL METHOD mo_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    mo_alv->set_ready_for_input( i_ready_for_input = 1 ).

    SET HANDLER me->on_toolbar   FOR mo_alv.
    SET HANDLER me->user_command FOR mo_alv.
    SET HANDLER me->handle_data_changed FOR mo_alv.
    SET HANDLER me->on_f4        FOR mo_alv.

    ls_layout-sel_mode   = 'A'.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-stylefname = 'FIELD_STYLE'.
    ls_layout-edit = 'X'.
    ls_layout-edit_mode = 'X'.
    ls_layout-sel_mode   = 'D'.
    ls_layout-cwidth_opt = 'X'.

    ls_variant-report = sy-repid.

    mo_alv->set_table_for_first_display(  EXPORTING is_layout                     = ls_layout
                                                    i_save                        = 'X'
                                                    is_variant                    = ls_variant
                                                    i_default                     = 'X'
                                          CHANGING  it_outtab                     = mt_line
                                                    it_fieldcatalog               = lt_fieldcat
                                        EXCEPTIONS  invalid_parameter_combination = 1
                                                    program_error                 = 2
                                                    too_many_lines                = 3
                                                    OTHERS                        = 4 ).

    CHECK sy-batch = abap_false.

    CALL SCREEN 0001.
  ENDMETHOD.
  METHOD on_toolbar.
    DELETE e_object->mt_toolbar WHERE function CP '&LOCAL&*'."редактирвоание

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<mt_toolbar>).

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZAPPR'.
    <mt_toolbar>-icon      = icon_set_state.
    <mt_toolbar>-text      = 'Согласовать'.
    <mt_toolbar>-quickinfo = 'Согласовать'(001).
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZREJE'.
    <mt_toolbar>-icon      = icon_reject.
    <mt_toolbar>-quickinfo = 'Отклонить'.
    <mt_toolbar>-text      = 'Отклонить'.
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZMODY'.
    <mt_toolbar>-icon      = icon_change.
    <mt_toolbar>-quickinfo = 'Отредактировать'.
    <mt_toolbar>-text      = 'Отредактировать'.
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZSAVE'.
    <mt_toolbar>-icon      = icon_change.
    <mt_toolbar>-quickinfo = 'Сохранить изменения'.
    <mt_toolbar>-text      = 'Сохранить изменения'.
    <mt_toolbar>-disabled  = abap_true.
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.



    MOVE 3 TO <mt_toolbar>-butn_type.
  ENDMETHOD.
  METHOD on_toolbar_save.
    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<mt_toolbar>) WHERE function CP 'Z*'.
      CASE <mt_toolbar>-function.
        WHEN 'ZSAVE'.
          <mt_toolbar>-disabled  = abap_false.
        WHEN OTHERS.
          <mt_toolbar>-disabled  = abap_true.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD on_toolbar_edit.
    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<mt_toolbar>) WHERE function CP 'Z*'.
      CASE <mt_toolbar>-function.
        WHEN 'ZSAVE'.
          <mt_toolbar>-disabled  = abap_true .
        WHEN OTHERS.
          <mt_toolbar>-disabled  = abap_false.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD handle_data_changed.
    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<change>).
      READ TABLE mt_line ASSIGNING FIELD-SYMBOL(<mt_line>) INDEX <change>-row_id.
      IF sy-subrc = 0.
        ASSIGN COMPONENT <change>-fieldname OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<change_field>).
        IF sy-subrc = 0.
          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = <change>-row_id
              i_fieldname = <change>-fieldname
            IMPORTING
              e_value     = <change_field>.
        ENDIF.
      ENDIF.
    ENDLOOP.
    mo_alv->refresh_table_display( is_stable = cs_stable ).
  ENDMETHOD.
  METHOD on_f4.
    DATA: lt_return       TYPE TABLE OF ddshretval
        , lv_edit_mode    TYPE flag
        .
    READ TABLE mt_line INDEX es_row_no-row_id ASSIGNING FIELD-SYMBOL(<mt_line>).
    CHECK <mt_line> IS ASSIGNED.
    lv_edit_mode = abap_true.
    READ TABLE <mt_line>-field_style ASSIGNING FIELD-SYMBOL(<field_style>) WITH KEY fieldname = e_fieldname.
    IF <field_style> IS ASSIGNED AND <field_style>-style = cl_gui_alv_grid=>mc_style_disabled .
      CLEAR: lv_edit_mode.
    ENDIF.
    READ TABLE <mt_line>-field_style ASSIGNING <field_style> WITH KEY fieldname = ''.
    IF <field_style> IS ASSIGNED AND <field_style>-style = cl_gui_alv_grid=>mc_style_disabled .
      CLEAR: lv_edit_mode.
    ENDIF.
    CASE e_fieldname.
      WHEN 'EDU_TYPE' OR 'EDU_TYPE_TXT'.
        SELECT *
          FROM zess_0023_t_aedu
          INTO TABLE @DATA(lt_edu_type).
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'ADDEDUID'
            value_org       = 'S'
          TABLES
            value_tab       = lt_edu_type
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
        IF sy-subrc = 0 AND lt_return IS NOT INITIAL AND lv_edit_mode = abap_true.
          READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<lt_return>) INDEX 1.
          READ TABLE lt_edu_type ASSIGNING FIELD-SYMBOL(<lt_edu_type>) WITH KEY addeduid = <lt_return>-fieldval.
          ASSIGN COMPONENT 'EDU_TYPE' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_type>).
          ASSIGN COMPONENT 'EDU_TYPE_TXT' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_type_txt>).
          IF <edu_type> IS ASSIGNED AND <edu_type_txt> IS ASSIGNED.
            <edu_type> = <lt_return>-fieldval.
            <edu_type_txt> = <lt_edu_type>-addedu_text.
          ENDIF.
        ENDIF.
      WHEN 'EDU_SPEC' OR 'EDU_SPEC_TXT'.
        SELECT *
          FROM zthr_dlinet
          INTO TABLE @DATA(lt_edu_spec)
          WHERE spras = 'R'.
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'DLINE'
            value_org       = 'S'
          TABLES
            value_tab       = lt_edu_spec
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
        IF sy-subrc = 0 AND lt_return IS NOT INITIAL AND lv_edit_mode = abap_true.
          READ TABLE lt_return ASSIGNING <lt_return> INDEX 1.
          READ TABLE lt_edu_spec ASSIGNING FIELD-SYMBOL(<lt_edu_spec>) WITH KEY dline = <lt_return>-fieldval.
          ASSIGN COMPONENT 'EDU_SPEC' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_spec>).
          ASSIGN COMPONENT 'EDU_SPEC_TXT' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_spec_txt>).
          IF <edu_spec> IS ASSIGNED AND <edu_spec_txt> IS ASSIGNED.
            <edu_spec> = <lt_return>-fieldval.
            <edu_spec_txt> = <lt_edu_spec>-stext.
          ENDIF.
        ENDIF.
      WHEN 'EDU_FRM' OR 'EDU_FRM_TXT'.
        SELECT *
          FROM t7ruokin
          INTO TABLE @DATA(lt_edu_frm)
          WHERE sprsl = 'R'
            AND molga = '33'
            AND facet = '33'
          .
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CCODE'
            value_org       = 'S'
          TABLES
            value_tab       = lt_edu_frm
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
        IF sy-subrc = 0 AND lt_return IS NOT INITIAL AND lv_edit_mode = abap_true.
          READ TABLE lt_return ASSIGNING <lt_return> INDEX 1.
          READ TABLE lt_edu_frm ASSIGNING FIELD-SYMBOL(<lt_edu_frm>) WITH KEY ccode = <lt_return>-fieldval.
          ASSIGN COMPONENT 'EDU_FRM' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_frm>).
          ASSIGN COMPONENT 'EDU_FRM_TXT' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_frm_txt>).
          IF <edu_frm> IS ASSIGNED AND <edu_frm_txt> IS ASSIGNED.
            <edu_frm> = <lt_return>-fieldval.
            <edu_frm_txt> = <lt_edu_frm>-cname.
          ENDIF.
        ENDIF.
      WHEN 'EDU_GOAL' OR 'EDU_GOAL_TXT'.
        SELECT *
          FROM zess_0023_t_goal
          INTO TABLE @DATA(lt_edu_goal)
          WHERE subty = @<mt_line>-edu_type
          .
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'GOALID'
            value_org       = 'S'
          TABLES
            value_tab       = lt_edu_goal
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
        IF sy-subrc = 0 AND lt_return IS NOT INITIAL AND lv_edit_mode = abap_true.
          READ TABLE lt_return ASSIGNING <lt_return> INDEX 1.
          READ TABLE lt_edu_goal ASSIGNING FIELD-SYMBOL(<lt_edu_goal>) WITH KEY goalid = <lt_return>-fieldval.
          ASSIGN COMPONENT 'EDU_GOAL' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_goal>).
          ASSIGN COMPONENT 'EDU_GOAL_TXT' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_goal_txt>).
          IF <edu_goal> IS ASSIGNED AND <edu_goal_txt> IS ASSIGNED.
            <edu_goal> = <lt_return>-fieldval.
            <edu_goal_txt> = <lt_edu_goal>-goal_text.
          ENDIF.
        ENDIF.
*      WHEN 'EDU_LENGTH*'.
      WHEN 'EDU_RESULT' OR 'EDU_RESULT_TXT'.
        SELECT *
          FROM zess_0023_t_efct
          INTO TABLE @DATA(lt_edu_result)
          .
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'EFCTID'
            value_org       = 'S'
          TABLES
            value_tab       = lt_edu_result
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
        IF sy-subrc = 0 AND lt_return IS NOT INITIAL AND lv_edit_mode = abap_true.
          READ TABLE lt_return ASSIGNING <lt_return> INDEX 1.
          READ TABLE lt_edu_result ASSIGNING FIELD-SYMBOL(<lt_edu_result>) WITH KEY efctid = <lt_return>-fieldval.
          ASSIGN COMPONENT 'EDU_RESULT' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_result>).
          ASSIGN COMPONENT 'EDU_RESULT_TXT' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_result_txt>).
          IF <edu_result> IS ASSIGNED AND <edu_result_txt> IS ASSIGNED.
            <edu_result> = <lt_return>-fieldval.
            <edu_result_txt> = <lt_edu_result>-efct_text.
          ENDIF.
        ENDIF.
      WHEN 'EDU_LENGTH_TYPE' OR 'EDU_LENGTH_TYPE_TXT'.
        SELECT zeinh, etext
          FROM t538t
          INTO TABLE @DATA(lt_edu_length_type)
          WHERE sprsl = 'R'
            AND zeinh = '001'
          .
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'ZEINH'
            value_org       = 'S'
          TABLES
            value_tab       = lt_edu_length_type
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
        IF sy-subrc = 0 AND lt_return IS NOT INITIAL AND lv_edit_mode = abap_true.
          READ TABLE lt_return ASSIGNING <lt_return> INDEX 1.
          READ TABLE lt_edu_length_type ASSIGNING FIELD-SYMBOL(<lt_edu_length_type>) WITH KEY zeinh = <lt_return>-fieldval.
          ASSIGN COMPONENT 'EDU_LENGTH_TYPE' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_length_type>).
          ASSIGN COMPONENT 'EDU_LENGTH_TYPE_TXT' OF STRUCTURE <mt_line> TO FIELD-SYMBOL(<edu_length_type_txt>).
          IF <edu_length_type> IS ASSIGNED AND <edu_length_type_txt> IS ASSIGNED.
            <edu_length_type> = <lt_return>-fieldval.
            <edu_length_type_txt> = <lt_edu_length_type>-etext.
          ENDIF.
        ENDIF.
    ENDCASE.
    er_event_data->m_event_handled = abap_true.
    mo_alv->refresh_table_display( is_stable = cs_stable
                                   i_soft_refresh = abap_true ).

  ENDMETHOD.
  METHOD user_command.
    DATA: lt_line LIKE mt_line
        , ls_stylerow TYPE lvc_s_styl
        , lt_stylerow TYPE lvc_t_styl
        .
    CASE e_ucomm.
      WHEN 'ZAPPR' OR 'ZREJE' OR 'ZMODY'.
        mo_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_index_rows) ).
        LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<row>).
          READ TABLE mt_line ASSIGNING FIELD-SYMBOL(<mt_line>) INDEX <row>-index.
          CHECK sy-subrc = 0.
          APPEND <mt_line> TO lt_line.
        ENDLOOP.

        IF lt_line IS INITIAL.
          MESSAGE 'Выберите Запись' TYPE 'S' DISPLAY LIKE 'W'.
        ELSE.
          SELECT * FROM zhr_xss_iprreq  INTO TABLE @DATA(lt_xss_iprreq) FOR ALL ENTRIES IN @lt_line WHERE req_id = @lt_line-req_id.
          SORT lt_xss_iprreq BY req_id.
        ENDIF.
      WHEN OTHERS.
        "
    ENDCASE.

    DATA: lt_xss_iprreq_upd LIKE lt_xss_iprreq
        .

    CASE e_ucomm.
      WHEN 'ZAPPR' OR 'ZREJE'.
        CASE e_ucomm.
          WHEN 'ZAPPR'.
            DATA(lv_status) = 5.
          WHEN 'ZREJE'.
            lv_status = 2.
        ENDCASE.
        LOOP AT lt_line ASSIGNING FIELD-SYMBOL(<lt_line>).
          READ TABLE lt_xss_iprreq ASSIGNING FIELD-SYMBOL(<lt_xss_iprreq>) WITH KEY req_id = <lt_line>-req_id BINARY SEARCH.
          CHECK sy-subrc = 0 AND <lt_xss_iprreq> IS ASSIGNED AND <lt_xss_iprreq>-status = <lt_line>-status.
          mo_assistent->get_approver_number( EXPORTING iv_pernr      = CONV #( <lt_xss_iprreq>-pernr_creator )
                                                       iv_status     = CONV #( lv_status )
                                                       iv_status_old = CONV #( <lt_xss_iprreq>-status )
                                             IMPORTING ev_approver   = <lt_xss_iprreq>-approver_number
                                                       ev_status     = <lt_xss_iprreq>-status
                                                       ev_status_log = DATA(lv_status_log) ).

          mo_assistent->write_ipr_sta( iv_req_id = <lt_xss_iprreq>-req_id
                                       iv_pernr  = CONV #( ms_param-pernr )
                                       iv_status = lv_status_log ).

          <lt_xss_iprreq>-data_change = sy-datum.
          <lt_xss_iprreq>-time_change = sy-uzeit.
          <lt_xss_iprreq>-uname       = sy-uname.
          APPEND <lt_xss_iprreq> TO lt_xss_iprreq_upd.
        ENDLOOP.

        IF lt_xss_iprreq_upd IS NOT INITIAL.
          MODIFY zhr_xss_iprreq FROM TABLE lt_xss_iprreq_upd.
          MESSAGE 'Данные изменены' TYPE 'S'.
        ELSE.
          MESSAGE 'Ошибка при изменении данных' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.
      WHEN 'ZMODY'.
        CHECK lt_line IS NOT INITIAL.
        SET HANDLER me->on_toolbar_save   FOR mo_alv.
        LOOP AT lt_index_rows ASSIGNING <row>.
          READ TABLE mt_line ASSIGNING <mt_line> INDEX <row>-index.
          CHECK sy-subrc = 0.
          LOOP AT not_enabled_fields ASSIGNING FIELD-SYMBOL(<enabled>).
            ls_stylerow-fieldname = <enabled> .
            ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT ls_stylerow INTO TABLE lt_stylerow.
          ENDLOOP.
          <mt_line>-field_style = lt_stylerow.
        ENDLOOP.
        mo_alv->refresh_table_display( is_stable = cs_stable ).
        EXIT.
      WHEN 'ZSAVE'.
        MOVE-CORRESPONDING mt_line TO lt_xss_iprreq_upd.
        LOOP AT lt_xss_iprreq_upd ASSIGNING <lt_xss_iprreq>.
          <lt_xss_iprreq>-data_change = sy-datum.
          <lt_xss_iprreq>-time_change = sy-uzeit.
          <lt_xss_iprreq>-uname       = sy-uname.
        ENDLOOP.
        IF lt_xss_iprreq_upd IS NOT INITIAL.
          MODIFY zhr_xss_iprreq FROM TABLE lt_xss_iprreq_upd.
          MESSAGE 'Данные изменены' TYPE 'S'.
        ELSE.
          MESSAGE 'Ошибка при изменении данных' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.
        SET HANDLER me->on_toolbar_edit   FOR mo_alv.
      WHEN OTHERS.

    ENDCASE.

    me->get_data( ).
    mo_alv->refresh_table_display( is_stable = cs_stable ).
  ENDMETHOD.
ENDCLASS.
