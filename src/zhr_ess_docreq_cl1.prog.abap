*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_EKSREQ_CL1
*&---------------------------------------------------------------------*
CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_toolbar   FOR EVENT toolbar             OF cl_gui_alv_grid IMPORTING e_object,
      user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm sender,
      double_click FOR EVENT double_click        OF cl_gui_alv_grid IMPORTING e_row,
      button_click FOR EVENT button_click        OF cl_gui_alv_grid IMPORTING es_col_id es_row_no.
ENDCLASS.
CLASS lcl_handler IMPLEMENTATION.
  METHOD on_toolbar.
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<mt_toolbar>).
*    MOVE 3 TO <mt_toolbar>-butn_type.
    CLEAR: e_object->mt_toolbar.

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZJOB'.
    <mt_toolbar>-icon      = icon_set_state.
    <mt_toolbar>-text      = 'Взять в работу'.
    <mt_toolbar>-quickinfo = 'Взять в работу'(001).
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    MOVE 3 TO <mt_toolbar>-butn_type.

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZREJ'.
    <mt_toolbar>-icon      = icon_reject.
    <mt_toolbar>-quickinfo = 'Отправить на корректировку '(002).
    <mt_toolbar>-text      = 'Отправить на корректировку '.
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    MOVE 3 TO <mt_toolbar>-butn_type.

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZDON'.
    <mt_toolbar>-icon      = icon_allow.
    <mt_toolbar>-text      = 'Согласовано'.
    <mt_toolbar>-quickinfo = 'Согласовано'(003).
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    MOVE 3 TO <mt_toolbar>-butn_type.


  ENDMETHOD.
  METHOD user_command.
    DATA: lt_update TYPE TABLE OF zthr_ess_docreq,
          lv_error  TYPE boole_d VALUE abap_false.

    "Сбор выбранных строк
    go_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_index_rows) ).
    LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<row>).
      READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX <row>-index.
      CHECK sy-subrc = 0 AND <alv> IS ASSIGNED.

      APPEND INITIAL LINE TO lt_update ASSIGNING FIELD-SYMBOL(<upd>).
      MOVE-CORRESPONDING <alv> TO <upd>.
    ENDLOOP.
    IF lt_update IS INITIAL.
      lv_error = abap_true.
      EXIT.
    ENDIF.

    CASE e_ucomm.
      WHEN 'ZJOB'.
        LOOP AT lt_update ASSIGNING <upd>.
          IF <upd>-status <> 0.
            lv_error = abap_true.
            EXIT.
          ENDIF.

          <upd>-status = 6.
          <upd>-date_change = sy-datum.
          <upd>-time_change = sy-timlo.
          <upd>-uname = sy-uname.
          <upd>-approver_number = gv_pernr.
        ENDLOOP.
      WHEN 'ZREJ'."отправить на корректировку
        LOOP AT lt_update ASSIGNING <upd>.
          IF <upd>-status <> 0 AND <upd>-status <> 6.
            lv_error = abap_true.
            EXIT.
          ENDIF.

          <upd>-status = 3.
          <upd>-date_change = sy-datum.
          <upd>-time_change = sy-timlo.
          <upd>-uname = sy-uname.
        ENDLOOP.
      WHEN 'ZDON'.
        LOOP AT lt_update ASSIGNING <upd>.
          IF <upd>-status <> 6.
            lv_error = abap_true.
            EXIT.
          ENDIF.

          " обновление статуса записи со Сведениями о доходе
          CALL FUNCTION 'ZHR_ESS_SVEDDF_AGREED' IN UPDATE TASK
            EXPORTING
              is_ess_docreq = <upd>
            EXCEPTIONS
              no_block      = 1
              no_file       = 2
              OTHERS        = 3.
          IF sy-subrc = 0.
            <upd>-status = 4.
            <upd>-date_change = sy-datum.
            <upd>-time_change = sy-timlo.
            <upd>-uname = sy-uname.
          ELSE.
            lv_error = abap_true.
            EXIT.
          ENDIF.

        ENDLOOP.
      WHEN 'ZREE'.
        IF lines( lt_update ) <> 1.
          lv_error = abap_true.
          EXIT.
        ENDIF.
        READ TABLE lt_update ASSIGNING <upd> INDEX 1.
        IF <upd>-status <> 0 AND <upd>-status <> 1.
          lv_error = abap_true.
          EXIT.
        ENDIF.
        <upd>-date_change = sy-datum.
        <upd>-time_change = sy-timlo.
        <upd>-uname = sy-uname.
        DATA: lv_sel_objec TYPE objec,
              lv_mode      TYPE hrf4param-srk_mode.

        CALL FUNCTION 'RH_OBJID_REQUEST_46A'
          EXPORTING
            plvar           = '01'
            otype           = 'P'
            seark           = '*'
            dynpro_repid    = sy-repid
            dynpro_dynnr    = '1000'
            set_mode        = 'X'
          IMPORTING
            sel_object      = lv_sel_objec
          EXCEPTIONS
            cancelled       = 1
            wrong_condition = 2
            nothing_found   = 3
            illegal_mode    = 4
            internal_error  = 5
            OTHERS          = 6.

        IF sy-subrc <> 0.
          lv_error = abap_true.
          EXIT.
        ENDIF.

        <upd>-approver_number = lv_sel_objec-objid.

    ENDCASE.

    IF lv_error = abap_true.
    ELSE.
      CALL FUNCTION 'ZHR_ESS_DOCREQ_UPD' IN UPDATE TASK
        EXPORTING
          it_data = lt_update.
      COMMIT WORK AND WAIT.
    ENDIF.

    PERFORM select_data.
    go_alv->refresh_table_display( is_stable = cs_stable ).
  ENDMETHOD.
  METHOD double_click.
  ENDMETHOD.
  METHOD button_click.
    DATA: lt_att      LIKE gt_att,
          lv_folder   TYPE string,
          lv_filename TYPE string,
          lv_ext      TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string,
          lt_data     TYPE solix_tab,
          lv_curpos   TYPE i VALUE 0,
          lv_line     TYPE i VALUE 255,
          ls_data     LIKE LINE OF lt_data,
          gs_selfield TYPE slis_selfield,
          g_exit(1)   TYPE c,
          lv_rc       TYPE i.

    READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX es_row_no-row_id.
    CHECK <alv> IS ASSIGNED AND sy-subrc = 0.

    CASE es_col_id.

      WHEN 'ATTACHEMENTS'.

        "По выбранной заявке - найдем файлы к ней
        PERFORM show_file USING <alv>.

      WHEN 'ZCOMBUT'.
*        IF <alv>-coment IS NOT INITIAL.
        gv_status = <alv>-status.
        gv_comment = <alv>-commentary.
        gv_hrcomment =  <alv>-hrcomment.
        gv_selrow = es_row_no-row_id.
        CALL SCREEN 9001 STARTING AT 100 1.
*        ENDIF.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
