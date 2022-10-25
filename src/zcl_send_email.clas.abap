class ZCL_SEND_EMAIL definition
  public
  final
  create public .

public section.

  data GO_SEND_REQUEST type ref to CL_BCS .
  data GO_DOCUMENT type ref to CL_DOCUMENT_BCS .
  data GO_SENDER type ref to CL_SAPUSER_BCS .
  data GO_RECIPIENT type ref to IF_RECIPIENT_BCS .
  data GT_ATTACHMENTS type RMPS_T_POST_CONTENT .
  class-data GT_TEXT type SOLI_TAB .
  data GT_SOLI_TAB type SOLI_TAB .
  data GT_SOLIX_TAB type SOLIX_TAB .
  data GV_SIZE type SOOD-OBJLEN .

  class-methods STRING_TO_SOLI_LINK
    importing
      !IV_STRING type STRING
      !IV_HTML type XFELD optional
    returning
      value(RTD_SOLI) type SOLI_TAB .
  methods ADD_ATTACHMENT
    importing
      value(I_ATTACHMENT_TYPE) type SOODK-OBJTP
      value(I_ATTACHMENT_SUBJECT) type SOOD-OBJDES
      value(I_ATTACHMENT_SIZE) type SOOD-OBJLEN optional
      value(I_ATTACHMENT_LANGUAGE) type SOOD-OBJLA default SPACE
      value(I_ATT_CONTENT_TEXT) type SOLI_TAB optional
      value(I_ATT_CONTENT_HEX) type SOLIX_TAB optional
      value(I_ATTACHMENT_HEADER) type SOLI_TAB optional .
  methods ADD_RECIPIENT_DLI
    importing
      !LIST_NAME type SOODD-OBJNAM .
  methods ADD_RECIPIENT_LIST
    importing
      !IM_RECIPIENT_LIST_NAME type STRING .
  methods ADD_RECIPIENT_MAIL
    importing
      value(IM_RECIPIENT_MAIL) type STRING .
  methods ADD_RECIPIENT_PERNR
    importing
      !I_PERNR type PERNR-PERNR
      !I_DATE type DATUM default SY-DATUM .
  methods ADD_RECIPIENT_UID
    importing
      value(IM_RECIPIENT_UID) type STRING
      !I_MAIL type FLAG default 'X'
      !I_EXPRESS type FLAG default ' ' .
  class-methods READ_TEXT_AS_STRING
    importing
      !IV_ID type THEAD-TDID default 'ST'
      !IV_LANGUAGE type THEAD-TDSPRAS default SY-LANGU
      !IV_NAME type THEAD-TDNAME
      !IV_OBJECT type THEAD-TDOBJECT default 'TEXT'
    returning
      value(RV_STRING) type STRING .
  methods ADD_SENDER
    importing
      !SENDER type STRING .
  methods ADD_SENDER_PERNR
    importing
      value(P_PERNR) type PERSNO optional
      value(P_UNAME) type UNAME optional
      !P_CHANGE_FROM_FIELD type FLAG default 'X' .
  methods CONSTRUCTOR
    importing
      value(IM_SUBJECT) type SO_OBJ_DES optional
      !P_HTML type FLAG optional
      !IM_TEXT type SOLI_TAB optional .
  class-methods DETERMINE_SENDER
    importing
      value(P_MAIL) type ADR6-SMTP_ADDR
      !P_CHANGE_FROM_FIELD type FLAG optional
    returning
      value(P_SENDER) type ref to IF_SENDER_BCS .
  methods SEND
    importing
      !I_WITH_ERROR_SCREEN type OS_BOOLEAN default SPACE
      !IV_IMMEDIATELY type OS_BOOLEAN default 'X'
      !I_COMMIT type OS_BOOLEAN optional
    returning
      value(RESULT) type OS_BOOLEAN .
  methods SET_LONG_SUBJECT
    importing
      !P_SUBJECT type STRING .
  class-methods STRING_TO_SOLI
    importing
      !IP_STRING type STRING
    returning
      value(RT_SOLI) type SOLI_TAB .
  class-methods XSTRING_TO_SOLIX
    importing
      !IP_XSTRING type XSTRING
    returning
      value(RT_SOLIX) type SOLIX_TAB .
  class-methods SEND_MAIL
    importing
      value(ITD_TO) type BCSY_SMTPA
      !IV_SUBJECT type SO_OBJ_DES optional
      !ITD_BODY type SOLI_TAB optional
      !ITD_FILE type ZTTBCS_FILE optional
      !IV_SENDER_UNAME type UNAME optional
      !IV_SENDER_EMAIL type ADR6-SMTP_ADDR optional
      !IV_HTML type XFELD optional
      !IV_TEXTNAME type THEAD-TDNAME optional
      !IV_TEXTID type THEAD-TDID default 'ST'
      !IV_TEXTSPRAS type THEAD-TDSPRAS default SY-LANGU
      !IV_TEXTOBJECT type THEAD-TDOBJECT default 'TEXT'
      !ITD_MAPPING type ZTTMAIL_MAPPING optional
      !IV_IMMEDIATELY type OS_BOOLEAN default 'X'
      !IV_COMMIT type XFELD default ABAP_FALSE
    exporting
      !ES_MESSAGE type BAPIRET2 .
protected section.
private section.
    Constants GC_TEXT_SUBJECT type CHAR10 value '_SUBJECT'. "#EC NOTEXT
  constants GC_TEXT_BODY type CHAR10 value '_BODY'. "#EC NOTEXT

  methods COUNT_DOC_SIZE
    importing
      !I_TEXT type SOLI_TAB optional
      !I_HEX type SOLIX_TAB optional
    returning
      value(RESULT) type SO_OBJ_LEN .
ENDCLASS.



CLASS ZCL_SEND_EMAIL IMPLEMENTATION.


method ADD_ATTACHMENT.

  go_document->add_attachment( EXPORTING I_ATTACHMENT_TYPE = I_ATTACHMENT_TYPE
                                         I_ATTACHMENT_SUBJECT = I_ATTACHMENT_SUBJECT
                                         I_ATTACHMENT_SIZE = I_ATTACHMENT_SIZE
                                         I_ATTACHMENT_LANGUAGE = I_ATTACHMENT_LANGUAGE
                                         I_ATT_CONTENT_TEXT = I_ATT_CONTENT_TEXT
                                         I_ATT_CONTENT_HEX = I_ATT_CONTENT_HEX
                                         I_ATTACHMENT_HEADER = I_ATTACHMENT_HEADER ).

endmethod.


method ADD_RECIPIENT_DLI.

DATA: lt_member TYPE TABLE OF sodm1,
      ls_member type sodm1,
      lt_objpara  TYPE TABLE OF selc,
      lt_objparb  TYPE TABLE OF soop1,
      lv_uname type string,
      lv_name type string.

  CALL FUNCTION 'SO_DLI_READ'
    EXPORTING
      distributionlist           = LIST_NAME
      system_dli                 = 'X'
    TABLES
      member                     = lt_member
      objpara                    = lt_objpara
      objparb                    = lt_objparb
    EXCEPTIONS
      active_user_not_exist      = 1
      communication_failure      = 2
      component_not_available    = 3
      dl_name_not_exist          = 4
      folder_not_exist           = 5
      folder_no_authorization    = 6
      forwarder_not_exist        = 7
      object_not_exist           = 8
      object_no_authorization    = 9
      operation_no_authorization = 10
      owner_not_exist            = 11
      parameter_error            = 12
      substitute_not_active      = 13
      substitute_not_defined     = 14
      system_failure             = 15
      user_not_exist             = 16
      x_error                    = 17
      OTHERS                     = 18.

  IF lt_member[] IS NOT INITIAL .
    LOOP AT lt_member INTO ls_member.
     CASE  ls_member-sndart.
      WHEN ''.

          lv_uname = ls_member-memnam.

          CALL METHOD me->add_recipient_uid
            EXPORTING
              im_recipient_uid = lv_uname
              i_mail           = 'X'
              .

      WHEN 'INT'.

        lv_name = ls_member-address.

        CALL METHOD me->add_recipient_mail
          EXPORTING
            im_recipient_mail = lv_name
            .

    ENDCASE.

    ENDLOOP.
  ENDIF.

endmethod.


method ADD_RECIPIENT_LIST.

*...
  data: ls_soos1 type SOOS1.
*
  try.
      check not im_recipient_list_name is initial.
      ls_soos1-RECESC = 'C'.   " Addr. list`
      ls_soos1-RECNAM = im_recipient_list_name.   " Like Z_LIST_1 from SAP Office
      me->go_send_request->send_request->add_recipient_from_soos1( i_soos1 = ls_soos1 ).
    catch cx_root.
      return.
  endtry.



endmethod.


method ADD_RECIPIENT_MAIL.

*...
  data: lv_smtp_addr type adr6-smtp_addr.
*
  try.
      check not im_recipient_mail is initial.
      TRANSLATE im_recipient_mail to UPPER CASE.
      lv_smtp_addr = im_recipient_mail.   " Like `IvanovVV@it-sk.ru`
      me->go_recipient = cl_cam_address_bcs=>create_internet_address( lv_smtp_addr ).
      me->go_send_request->add_recipient( me->go_recipient ).
    catch cx_root.
      return.
  endtry.


endmethod.


method ADD_RECIPIENT_PERNR.


  DATA: t_p0105 TYPE TABLE OF p0105.
  DATA: field   TYPE string.

  FIELD-SYMBOLS: <p0105> TYPE p0105.

  CALL FUNCTION 'HR_READ_SUBTYPE'
    EXPORTING
      pernr           = i_pernr
      infty           = '0105'
      subty           = '0010'
      begda           = i_date
      endda           = i_date
    TABLES
      infty_tab       = t_p0105
    EXCEPTIONS
      infty_not_found = 1
      invalid_input   = 2
      OTHERS          = 3.

  CHECK sy-subrc EQ 0.

  LOOP AT t_p0105 ASSIGNING <p0105>.
    field = <p0105>-usrid_long.
    me->add_recipient_mail( field ).
    EXIT.
  ENDLOOP.


endmethod.


method ADD_RECIPIENT_UID.

*...
  DATA: lv_uname  TYPE uname.
  data: ls_uname  type BAPIBNAME-BAPIBNAME.
  DATA: wa_adress TYPE bapiaddr3.
  DATA: wa_mail TYPE string.
  data: lt_BAPIRET2 type TABLE OF BAPIRET2.

*
  IF i_mail IS NOT INITIAL.

    TRY.
        CHECK im_recipient_uid IS NOT INITIAL.

        ls_uname = im_recipient_uid.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = ls_uname
          IMPORTING
            address  = wa_adress
          TABLES
            RETURN   = lt_BAPIRET2  .

        IF wa_adress-e_mail IS NOT INITIAL.
          TRANSLATE wa_adress-e_mail to UPPER CASE.
          MOVE wa_adress-e_mail TO wa_mail.

          me->add_recipient_mail( wa_mail ).

        ENDIF.

      CATCH cx_root.

    ENDTRY.
  ELSE.
    TRY.
        CHECK NOT im_recipient_uid IS INITIAL.
        lv_uname = im_recipient_uid.   " Like `IvanovVV` from sy-uname
        me->go_recipient = cl_sapuser_bcs=>create( lv_uname ).
        me->go_send_request->add_recipient(
            EXPORTING i_recipient = me->go_recipient
                      i_express = i_express ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDIF.




endmethod.


method ADD_SENDER.
  DATA: sender_mail    TYPE adr6-smtp_addr.
  DATA: lo_sender      TYPE REF TO if_sender_bcs VALUE IS INITIAL.

  sender_mail = sender.

  TRY.
* Set sender
      lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
      go_send_request->set_sender(
      EXPORTING
      i_sender = lo_sender ).
    CATCH cx_address_bcs.
      RETURN.
  ENDTRY.



endmethod.


method ADD_SENDER_PERNR.
  DATA:
        lo_sender       TYPE REF TO if_sender_bcs
      , sender_mail     TYPE adr6-smtp_addr
      .
  TRY.

      IF p_pernr IS NOT INITIAL.
*        sender_mail = zcl_pm_utils=>read_email_of_pernr( p_pernr ).
      ELSE.
        IF p_uname IS INITIAL.
          p_uname = sy-uname.
        ENDIF.
*        sender_mail = zcl_ess_utils=>get_mail_for_user( p_uname ).
      ENDIF.

      lo_sender = zcl_send_email=>determine_sender(
          p_mail              = sender_mail
          p_change_from_field = p_change_from_field ).

      go_send_request->set_sender( lo_sender ).
    CATCH cx_address_bcs.
      RETURN.
  ENDTRY.
endmethod.


method CONSTRUCTOR.
  DATA: lv_type TYPE so_obj_tp.

*--------------------------

* Create Persistent Send Request
  go_send_request = cl_bcs=>create_persistent( ).
*
*--------------------------
* Create document
  IF p_html EQ abap_true.
    lv_type = 'HTM'.
  ELSE.
    lv_type = 'RAW'.
  ENDIF.

  go_document = cl_document_bcs=>create_document( i_type    = lv_type
                                                  i_text    = im_text
                                                  i_subject = im_subject ).
*
*--------------------------
* Pass the document to send request
  go_send_request->set_document( go_document ).


endmethod.


method COUNT_DOC_SIZE.

  DATA: doc_lines  TYPE i,
          hex_line   TYPE solix-line,
          text_line  TYPE soli-line,
          line_size  TYPE i,
          last_line  TYPE i,
          doc_size   TYPE i,
          is_binary  TYPE os_boolean.


  DESCRIBE TABLE i_hex LINES doc_lines.
  IF doc_lines > 0.
    is_binary = 'X'.
  ENDIF.

  IF is_binary = 'X'.
* *--- binary document ---------------------------
    DESCRIBE TABLE i_hex LINES doc_lines.
    IF doc_lines = 0.
      doc_size = 0.
    ELSE.
      line_size = xstrlen( hex_line ).
      doc_size = line_size * doc_lines.
    ENDIF.
  ELSE.
* *--- text document ---------------------------
    DESCRIBE TABLE i_text LINES doc_lines.
    IF doc_lines = 0.
      doc_size = 0.
    ELSE.
      READ TABLE i_text INTO text_line
           INDEX doc_lines.
      last_line = strlen( text_line ).
      doc_size = 255 * ( doc_lines - 1 ) + last_line.
    ENDIF.
  ENDIF.

  result = doc_size.




endmethod.


method DETERMINE_SENDER.
  DATA: lv_res  TYPE i
      , lv_str  TYPE adr6-smtp_addr
      .

    TRY.
        p_sender = cl_sapuser_bcs=>create( sy-uname ).
        IF p_sender IS BOUND.
          RETURN.
        ENDIF.
      CATCH cx_address_bcs.
        RETURN.
    ENDTRY.

  TRANSLATE p_mail TO UPPER CASE.

  IF p_change_from_field EQ abap_true.
    lv_str = 'Служба рассылки SAP'(001).
  ELSE.

  ENDIF.

  p_sender = cl_cam_address_bcs=>create_internet_address( i_address_string = p_mail i_address_name = lv_str ).
endmethod.


METHOD READ_TEXT_AS_STRING.

  TYPES: BEGIN OF lty_stream,
           c(65535),
         END OF lty_stream.

  DATA: ltd_tline  TYPE STANDARD TABLE OF tline,
        ltd_stream TYPE STANDARD TABLE OF lty_stream.

  FIELD-SYMBOLS: <ls_stream> TYPE lty_stream.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id       = iv_id
      language = iv_language
      name     = iv_name
      object   = iv_object
    TABLES
      lines    = ltd_tline
    EXCEPTIONS
      OTHERS   = 0.

  IF ltd_tline IS NOT INITIAL.
    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      TABLES
        itf_text    = ltd_tline
        text_stream = ltd_stream.

    LOOP AT ltd_stream ASSIGNING <ls_stream>.
      CONCATENATE rv_string <ls_stream> INTO rv_string.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD send.

*...
  TRY.

*--------------------------
* Set the Send Immediately Flag
      go_send_request->set_send_immediately( iv_immediately ).
*
*--------------------------
* Send Document
      go_send_request->send( i_with_error_screen = 'X' ).
* { Goncharov Andrey 26.12.2012 14:31:34
      IF i_commit = 'X'.
        COMMIT WORK.
      ENDIF.
* } Goncharov Andrey 26.12.2012 14:31:34
      result = 'X'.

    CATCH cx_send_req_bcs.
      result = ''.
  ENDTRY.
*

ENDMETHOD.


METHOD send_mail.

  DATA: lo_mail     TYPE REF TO zcl_send_email,
        lv_subject  TYPE so_obj_des,
        lv_textname TYPE thead-tdname,
        ltd_body    TYPE soli_tab,
        lv_body     TYPE string.

  "--------------- Тема письма ----------------"
  lv_subject = iv_subject.
  IF iv_textname IS NOT INITIAL AND lv_subject IS INITIAL.
    lv_textname = iv_textname && gc_text_subject.
    lv_subject = zcl_send_email=>read_text_as_string( iv_id       = iv_textid
                                                      iv_language = iv_textspras
                                                      iv_name     = lv_textname
                                                      iv_object   = iv_textobject ).
    IF lv_subject IS INITIAL.
      MESSAGE e007(zhr_mss) WITH lv_textname iv_textid INTO es_message-message.
      "es_message = sy-msgty.
      RETURN.
    ENDIF.
  ENDIF.

  "--------------- Тело письма ----------------"
  ltd_body = itd_body.
  IF iv_textname IS NOT INITIAL AND ltd_body IS INITIAL.
    lv_textname = iv_textname && gc_text_body.
    lv_body = zcl_send_email=>read_text_as_string( iv_id       = iv_textid
                                                   iv_language = iv_textspras
                                                   iv_name     = lv_textname
                                                   iv_object   = iv_textobject ).
    IF lv_body IS INITIAL.
      MESSAGE e007(zhr_mss) WITH lv_textname iv_textid INTO es_message-message.
*      es_message-mess_type = sy-msgty.
      RETURN.
    ENDIF.

    "Мэпинг полей
    LOOP AT itd_mapping ASSIGNING FIELD-SYMBOL(<ls_mapping>).
      lv_body = replace( val  = lv_body
                         sub  = <ls_mapping>-name
                         with = <ls_mapping>-value
                         occ  = 0 ).
    ENDLOOP.
    "Преобразование строки в SOLI_TAB с учетом ссылок во избежение их "разбиения"
    ltd_body = string_to_soli_link( iv_string = lv_body
                                    iv_html   = iv_html ).
  ENDIF.

  "
  lo_mail = NEW #( "p_html     = abap_false
                   p_html     = iv_html
                   im_subject = lv_subject
                   im_text    = ltd_body ).

  "--------------- Вложения ----------------"
  LOOP AT itd_file ASSIGNING FIELD-SYMBOL(<ls_file>).
    lo_mail->add_attachment( i_attachment_type    = <ls_file>-extension
                             i_attachment_subject = <ls_file>-attachment_subject
                             i_att_content_hex    = <ls_file>-add_content_hex
                             i_att_content_text   = <ls_file>-add_content_text ).
  ENDLOOP.

  "--------------- Отправитель ----------------"
  IF iv_sender_email IS NOT INITIAL.
    lo_mail->add_sender( CONV string( iv_sender_email ) ).
*  ELSEIF iv_sender_uname IS SUPPLIED.
*    DATA(lr_sapuser_bcs) = cl_sapuser_bcs=>create( iv_sender_uname ).
*    lr_bcs->set_sender( lr_sapuser_bcs ).
  ENDIF.

  "--------------- Получатель ----------------"
  LOOP AT itd_to ASSIGNING FIELD-SYMBOL(<lv_to>).
    lo_mail->add_recipient_mail( CONV string( <lv_to> ) ).
  ENDLOOP.

  "Отправить
  lo_mail->send( i_commit = iv_commit
                 iv_immediately = iv_immediately ).

ENDMETHOD.


method SET_LONG_SUBJECT.
  TRY.
      go_send_request->set_message_subject( p_subject ).
    CATCH cx_send_req_bcs.
      RETURN.
  ENDTRY.
endmethod.


method STRING_TO_SOLI.

  data  lp_offset type i.
  data  lt_soli type soli_tab.
  data  ls_soli_line type soli.
  data  lp_string_len type i.
  data  lp_soli_rows type i.
  data  lp_last_row_length type i.
  data  lp_row_length type i.
  data  lp_doc_length type so_obj_len.

* * transform string to SOLI
  describe field ls_soli_line length lp_row_length in character mode.
  lp_offset = 0.

  lp_string_len = strlen( ip_string ).

  lp_soli_rows = lp_string_len div lp_row_length.
  lp_last_row_length = lp_string_len mod lp_row_length.
  do lp_soli_rows times.
    ls_soli_line-line =
           ip_string+lp_offset(lp_row_length).
    append ls_soli_line to rt_soli.
    add lp_row_length to lp_offset.
  enddo.
  if lp_last_row_length > 0.
    clear ls_soli_line-line.
    ls_soli_line-line = ip_string+lp_offset(lp_last_row_length).
    append ls_soli_line to rt_soli.
  endif.


endmethod.


METHOD string_to_soli_link.

  "CONSTANTS: lc_tag TYPE char3 VALUE `<a `.

  DATA: ltd_str1   TYPE STANDARD TABLE OF string,
        ltd_str2   TYPE STANDARD TABLE OF string,
        ltd_string TYPE STANDARD TABLE OF string,
        ltd_soli   TYPE soli_tab,
        lv_last    TYPE xfeld.

  FIELD-SYMBOLS: <lv_string> TYPE string,
                 <ls_soli>   TYPE LINE OF soli_tab.

  SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE ltd_string.

  LOOP AT ltd_string ASSIGNING <lv_string>.

    AT LAST.
      lv_last = abap_true.
    ENDAT.

    REFRESH ltd_soli.

    IF <lv_string> IS NOT INITIAL.
      ltd_soli = string_to_soli( <lv_string> ).

      IF iv_html = abap_true.

        LOOP AT ltd_soli ASSIGNING <ls_soli>.
          APPEND <ls_soli> TO rtd_soli.
          IF lv_last <> abap_true.
            "Реализуем переход на новую строку.
            <ls_soli>-line = '<br>'.
            APPEND <ls_soli> TO rtd_soli.

          ENDIF.
        ENDLOOP.

      ELSE.

        APPEND LINES OF ltd_soli TO rtd_soli.
      ENDIF.

    ELSE.
      APPEND INITIAL LINE TO rtd_soli ASSIGNING <ls_soli>.

      IF iv_html = abap_true.
        <ls_soli>-line = '<br>'.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


method XSTRING_TO_SOLIX.


  data  lp_offset type i.
  data  lt_solix type solix_tab.
  data  ls_solix_line type solix.
  data  ls_archive_params type toa_dara.
  data  lp_pdf_string_len type i.
  data  lp_solix_rows type i.
  data  lp_last_row_length type i.
  data  lp_row_length type i.
  data  lp_doc_length type so_obj_len.

* * transform xstring to SOLIX
  describe table lt_solix.
  lp_row_length = sy-tleng.
  lp_offset = 0.

  lp_pdf_string_len = xstrlen( ip_xstring ).

  lp_solix_rows = lp_pdf_string_len div lp_row_length.
  lp_last_row_length = lp_pdf_string_len mod lp_row_length.
  do lp_solix_rows times.
    ls_solix_line-line =
           ip_xstring+lp_offset(lp_row_length).
    append ls_solix_line to rt_solix.
    add lp_row_length to lp_offset.
  enddo.
  if lp_last_row_length > 0.
    clear ls_solix_line-line.
    ls_solix_line-line = ip_xstring+lp_offset(lp_last_row_length).
    append ls_solix_line to rt_solix.
  endif.

endmethod.
ENDCLASS.
