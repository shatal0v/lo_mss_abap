*----------------------------------------------------------------------*
***INCLUDE LZFGHR_MSS_ATTACHFOR.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PBO_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pbo_0200 .
  IF lo_container IS INITIAL AND lo_grid IS INITIAL.
    CREATE OBJECT lo_container
      EXPORTING
        container_name              = 'GRID1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT lo_grid
      EXPORTING
        i_parent          = lo_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    DATA: lt_fieldcat TYPE lvc_t_fcat
        .

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'SGOS_ATTA'
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    FIELD-SYMBOLS: <lt_fieldcat> LIKE LINE OF lt_fieldcat
                 .
    LOOP AT lt_fieldcat ASSIGNING <lt_fieldcat>.
      CASE <lt_fieldcat>-fieldname.
        WHEN 'BITM_TYPE' OR 'BITM_DESCR'.
          <lt_fieldcat>-tech = abap_true.
          CONTINUE.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

    ENDLOOP.

    DATA: ls_layout  TYPE lvc_s_layo
        , ls_variant TYPE disvariant
        .
    DATA(lo_handler) = NEW lcl_handler( ).

    SET HANDLER lo_handler->on_toolbar   FOR lo_grid.
    SET HANDLER lo_handler->user_command FOR lo_grid.
    SET HANDLER lo_handler->double_click FOR lo_grid.

    ls_layout-sel_mode   = 'A'.
    ls_layout-cwidth_opt = 'X'.
**    CASE lv_ftype.
**      WHEN 'IP'.
**        ls_layout-grid_title = 'ИПР'.
**      WHEN 'LE'.
**        ls_layout-grid_title = 'Заявления на отсутствие'.
**      WHEN OTHERS.
**        ls_layout-grid_title = 'График аттестации'.
**    ENDCASE.
    ls_layout-grid_title = 'Имеющиеся документы'.
    lo_grid->set_table_for_first_display( EXPORTING is_layout                     = ls_layout
                                                    i_save                        = 'X'
                                                    is_variant                    = ls_variant
                                                    i_default                     = 'X'
                                          CHANGING  it_outtab                     = lt_data
                                                    it_fieldcatalog               = lt_fieldcat
                                         EXCEPTIONS invalid_parameter_combination = 1
                                                    program_error                 = 2
                                                    too_many_lines                = 3
                                                    OTHERS                        = 4 ).
  ELSE.
    DATA: ls_stable TYPE lvc_s_stbl VALUE '11'
        .
    lo_grid->refresh_table_display( is_stable = ls_stable ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_ICON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LT_AT_ATTR>_DTYPE  text
*      <--P_<LT_DATA>_BITM_ICON  text
*----------------------------------------------------------------------*
FORM set_icon  USING    p_dtype
               CHANGING p_bitm_icon.

  DATA(lv_dtype) = CONV stext( p_dtype ).
  TRANSLATE lv_dtype TO UPPER CASE.
*ICON_2 ICON_PDF                       '@IT@'."  Adobe Acrobat document
*ICON_2 ICON_BMP                       '@IU@'."  Bitmap image
*ICON_2 ICON_FAX_DOC                   '@IV@'."  Fax document
*ICON_2 ICON_GIF                       '@IW@'."  GIF image
*ICON_2 ICON_HLP                       '@IX@'."  Help file
*ICON_2 ICON_HTT                       '@IY@'."  Hypertext template
*ICON_2 ICON_ITS                       '@IZ@'."  Internet document set
*ICON_2 ICON_JPG                       '@J0@'."  JPEG Image
*ICON_2 ICON_MSG                       '@J1@'."  E-mail message
*ICON_2 ICON_XLS                       '@J2@'."  MS Excel worksheet
*ICON_2 ICON_XLV                       '@J3@'."  MS Excel VBA module
*ICON_2 ICON_HTM                       '@J4@'."  HTML document
*ICON_2 ICON_PPT                       '@J5@'."  MS PowerPoint presentation
*ICON_2 ICON_DOT                       '@J6@'."  MS Word template
*ICON_2 ICON_DOC                       '@J7@'."  MS Word document
*ICON_2 ICON_EML                       '@J8@'."  MS Outlook express e-mail messa
*ICON_2 ICON_RTF                       '@J9@'."  Rich text format document
*ICON_2 ICON_TIF                       '@JA@'."  TIF image document
*ICON_2 ICON_WRI                       '@JB@'."  Notepad document
*ICON_2 ICON_LWP                       '@JC@'."  Lotus WordPro 97 document
*ICON_2 ICON_LOTUS                     '@JD@'."  Lotus 1-2-3 workbook
*ICON_2 ICON_VSD                       '@JE@'."  Visio document
*ICON_2 ICON_DEFAULT_WINDOWS           '@JF@'."  Default Windows icon






  CASE lv_dtype+0(3).
    WHEN 'PDF'. p_bitm_icon = icon_pdf.
    WHEN 'BMP'. p_bitm_icon = icon_bmp.
    WHEN 'GIF'. p_bitm_icon = icon_gif.
    WHEN 'HLP'. p_bitm_icon = icon_hlp.
    WHEN 'HTT'. p_bitm_icon = icon_htt.
    WHEN 'ITS'. p_bitm_icon = icon_its.
    WHEN 'JPG'. p_bitm_icon = icon_jpg.
    WHEN 'MSG'. p_bitm_icon = icon_msg.
    WHEN 'XLS'. p_bitm_icon = icon_xls.
    WHEN 'XLV'. p_bitm_icon = icon_xlv.
    WHEN 'HTM'. p_bitm_icon = icon_htm.
    WHEN 'PPT'. p_bitm_icon = icon_ppt.
    WHEN 'DOT'. p_bitm_icon = icon_dot.
    WHEN 'DOC'. p_bitm_icon = icon_doc.
    WHEN 'EML'. p_bitm_icon = icon_eml.
    WHEN 'RTF'. p_bitm_icon = icon_rtf.
    WHEN 'TIF'. p_bitm_icon = icon_tif.
    WHEN 'WRI'. p_bitm_icon = icon_wri.
    WHEN 'LWP'. p_bitm_icon = icon_lwp.
    WHEN 'VSD'. p_bitm_icon = icon_vsd.
    WHEN 'XML'. p_bitm_icon = icon_xml_doc.
    WHEN OTHERS. p_bitm_icon = icon_default_windows.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_data .
  REFRESH: lt_data
         , lt_dele
         .
  SELECT * FROM zthr_mss_at_attr INTO TABLE @DATA(lt_at_attr) WHERE object = @lv_object
                                                                AND ftype  = @lv_ftype
      ORDER BY datum, uzeit.

  LOOP AT lt_at_attr ASSIGNING FIELD-SYMBOL(<lt_at_attr>).
    APPEND INITIAL LINE TO lt_data ASSIGNING FIELD-SYMBOL(<lt_data>).
    <lt_data>-creator       = <lt_at_attr>-uname.
    <lt_data>-creadate      = <lt_at_attr>-datum.
    <lt_data>-creatime      = <lt_at_attr>-uzeit.
    <lt_data>-bitm_filename = <lt_at_attr>-fname.

    <lt_data>-ftype  = <lt_at_attr>-ftype.
    <lt_data>-object = <lt_at_attr>-object.
    <lt_data>-seqnr  = <lt_at_attr>-seqnr.

    PERFORM set_icon USING <lt_at_attr>-dtype
                     CHANGING <lt_data>-bitm_icon.
  ENDLOOP.
ENDFORM.
