*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_EKSREQ_FRM
*&---------------------------------------------------------------------*
FORM select_data.
  FREE: gt_alv, gt_att.

  "Получение данных из БД
  IF s_status-low EQ '5'.
    MESSAGE 'В статусе "В проекте(5)" заявки не выводятся' TYPE 'E'.

  ENDIF.
  SELECT * FROM zthr_ess_docreq
  INTO CORRESPONDING FIELDS OF TABLE gt_alv WHERE status IN s_status AND gjahr EQ p_gjahr AND status NE '5' AND
     pernr IN s_orgeh.
*  WHERE approver_pernr = gv_pernr.
  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<alv>).
    DATA: ls_pa0002 TYPE pa0002.

    CLEAR: ls_pa0002.
    SELECT SINGLE nachn vorna midnm FROM pa0002 INTO CORRESPONDING FIELDS OF ls_pa0002 WHERE pernr = <alv>-pernr.
    CONCATENATE ls_pa0002-nachn ls_pa0002-vorna ls_pa0002-midnm INTO <alv>-fio SEPARATED BY space.

    CLEAR: ls_pa0002.
    SELECT SINGLE nachn vorna midnm FROM pa0002 INTO CORRESPONDING FIELDS OF ls_pa0002 WHERE pernr = <alv>-approver_number.
    CONCATENATE ls_pa0002-nachn ls_pa0002-vorna ls_pa0002-midnm INTO <alv>-approver SEPARATED BY space.

*    SELECT SINGLE text FROM zthr_ess_ekstype INTO <alv>-reqtypename WHERE code = <alv>-req_type.

    READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<status>) WITH KEY value = <alv>-status.
    IF sy-subrc = 0.
      <alv>-status_text = <status>-dtext.
    ENDIF.

    SELECT req_id req_pos filename FROM zthr_ess_eksreqf APPENDING CORRESPONDING FIELDS OF TABLE gt_att WHERE req_id = <alv>-req_id.
    IF sy-subrc = 0.
      <alv>-attachements = icon_attachment.
    ENDIF.

    IF <alv>-commentary IS NOT INITIAL.
      <alv>-zcombut = icon_display_text.
    ENDIF.

  ENDLOOP.
ENDFORM.
FORM pbo100.
  DATA: lt_fc TYPE lvc_t_fcat,
        ls_lo TYPE lvc_s_layo,
        ls_vr TYPE disvariant.

  IF go_cnt IS INITIAL.
    CREATE OBJECT go_cnt
      EXPORTING
        container_name              = 'CNT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent          = go_cnt
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM alv_fc CHANGING lt_fc.

    DATA(lo_handler) = NEW lcl_handler( ).
    SET HANDLER lo_handler->on_toolbar   FOR go_alv.
    SET HANDLER lo_handler->user_command FOR go_alv.
    "SET HANDLER lo_handler->double_click FOR go_alv.
    SET HANDLER lo_handler->button_click FOR go_alv.

    ls_lo-sel_mode   = 'A'.
    ls_lo-cwidth_opt = 'X'.

    ls_vr-report = sy-repid.

    go_alv->set_table_for_first_display(  EXPORTING is_layout                     = ls_lo
                                                    i_save                        = 'X'
                                                    is_variant                    = ls_vr
                                                    i_default                     = 'X'
                                          CHANGING  it_outtab                     = gt_alv
                                                    it_fieldcatalog               = lt_fc
                                        EXCEPTIONS  invalid_parameter_combination = 1
                                                    program_error                 = 2
                                                    too_many_lines                = 3
                                                    OTHERS                        = 4 ).
  ELSE.
    go_alv->refresh_table_display( is_stable = cs_stable ).
  ENDIF.
ENDFORM.
FORM pai100.
  CASE ok_code.
    WHEN  'BACK' OR 'EXIT' OR 'QUIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.
FORM alv_fc CHANGING ct_fc TYPE lvc_t_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = cv_alv_structure
    CHANGING
      ct_fieldcat            = ct_fc
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  LOOP AT ct_fc ASSIGNING FIELD-SYMBOL(<fc>).
    CASE <fc>-fieldname.
      WHEN 'COMMENTARY'  OR 'TIME_CREATE' OR 'STATUS'.
        <fc>-no_out = 'X'.
      WHEN 'REQ_ID'.
        <fc>-tech = abap_true.
      WHEN 'ATTACHEMENTS'.
        <fc>-icon = abap_true.
        <fc>-style = cl_gui_alv_grid=>mc_style_button.
      WHEN 'ZCOMBUT'.
        <fc>-icon = abap_true.
        <fc>-style = cl_gui_alv_grid=>mc_style_button.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PAI9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pai9001 USING zcom_old  TYPE zehr_ess_eksreq_coment
                   zcom_new  TYPE zehr_ess_eksreq_coment.

  DATA: lt_update TYPE TABLE OF zthr_ess_docreq,
        lv_error  TYPE boole_d VALUE abap_false.
  CONSTANTS: line_length TYPE i VALUE 72.

  DATA: BEGIN OF stext_new OCCURS 200,
          line(line_length),
        END OF stext_new.

  CASE ok_code.
    WHEN  'OK'.
*

      CALL METHOD g_editor_new->get_text_as_r3table
        IMPORTING
          table = stext_new[].
      IF stext_new[] IS NOT INITIAL.
        CONCATENATE LINES OF stext_new[] INTO zcom_new SEPARATED BY space.
        IF zcom_new IS NOT INITIAL.
          READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX gv_selrow.
          CHECK sy-subrc = 0 AND <alv> IS ASSIGNED.
*
          APPEND INITIAL LINE TO lt_update ASSIGNING FIELD-SYMBOL(<upd>).
          MOVE-CORRESPONDING <alv> TO <upd>.
*
          IF lt_update IS INITIAL.
            lv_error = abap_true.
            CLEAR: zcom_new.
            LEAVE TO SCREEN 0.
          ENDIF.

          <upd>-hrcomment = zcom_new.

*        ENDIF.
*
          IF lv_error = abap_true.
          ELSE.
            IF gv_status EQ 6 OR
               gv_status EQ 3.

              CALL FUNCTION 'ZHR_ESS_DOCREQ_UPD' IN UPDATE TASK
                EXPORTING
                  it_data = lt_update.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.
*
          PERFORM select_data.
          go_alv->refresh_table_display( is_stable = cs_stable ).
        ENDIF.
      ENDIF.
*
      CLEAR: zcom_new, zcom_old.
      LEAVE TO SCREEN 0.
*
    WHEN 'CANCEL'.
      CLEAR: zcom_new, zcom_old.
      LEAVE TO SCREEN 0.
*
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_FILE
*&---------------------------------------------------------------------*
*       откытие файла со сведениями о доходе
*----------------------------------------------------------------------*
*      -->P_<ALV>  text
*----------------------------------------------------------------------*
FORM show_file  USING    is_alv TYPE zshr_ess_docreq_alv.
  DATA:  lv_folder   TYPE string,
         lv_fullpath TYPE string,
         ls_sveddf   TYPE zthr_ess_sveddf,
         lt_data     TYPE solix_tab,
         lv_curpos   TYPE i VALUE 0,
         lv_line     TYPE i VALUE 255,
         ls_data     LIKE LINE OF lt_data,
         gs_selfield TYPE slis_selfield,
         g_exit(1)   TYPE c,
         lv_rc       TYPE i.

  SELECT      seqnr
              filename
              filedata
    INTO (ls_sveddf-seqnr,
          ls_sveddf-filename,
          ls_sveddf-filedata)
     FROM zthr_ess_sveddf
    WHERE pernr = is_alv-pernr
      AND zyear = is_alv-gjahr
      AND seqnr IN (  SELECT MAX( seqnr ) FROM zthr_ess_sveddf WHERE pernr = is_alv-pernr
                                                                 AND zyear = is_alv-gjahr ).
  ENDSELECT.

  IF sy-subrc = 0.
    "Получение путей для сохранения файлы
    CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
      CHANGING
        sapworkdir            = lv_folder
      EXCEPTIONS
        get_sapworkdir_failed = 1
        cntl_error            = 2
        error_no_gui          = 3
        not_supported_by_gui  = 4
        OTHERS                = 5.
    CHECK sy-subrc = 0.
    cl_gui_cfw=>flush( ).

    lv_fullpath = lv_folder && '\' &&  ls_sveddf-filename .

    DATA(lv_length) = xstrlen( ls_sveddf-filedata ).
    TRY.
        CALL METHOD cl_bcs_convert=>xstring_to_xtab
          EXPORTING
            iv_xstring = ls_sveddf-filedata
          IMPORTING
            et_xtab    = lt_data[].
      CATCH cx_root.
    ENDTRY.
    "Сохранение файла
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = lv_fullpath
        filetype                = 'BIN'
        bin_filesize            = lv_length
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    CHECK sy-subrc = 0.
    cl_gui_cfw=>flush( ).

    "Открытие файла
    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document               = lv_fullpath
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10.
    cl_gui_cfw=>flush( ).
  ELSE.
    MESSAGE text-m01 TYPE 'S'.

  ENDIF.
ENDFORM.
