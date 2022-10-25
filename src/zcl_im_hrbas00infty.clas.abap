class ZCL_IM_HRBAS00INFTY definition
  public
  final
  create public .

public section.

  interfaces IF_EX_HRBAS00INFTY .
protected section.
private section.

  methods _PROCESS_COMPETITION
    importing
      !IV_OLD_INNNN type WPLOG
      !IV_NEW_INNNN type WPLOG
      !IV_PPPAR type PPPAR
      !IV_OK_CODE type SY-UCOMM .
ENDCLASS.



CLASS ZCL_IM_HRBAS00INFTY IMPLEMENTATION.


METHOD if_ex_hrbas00infty~after_input.

  CASE new_innnn-infty.
    WHEN '9103'.

      _process_competition( iv_old_innnn = old_innnn
                            iv_new_innnn = new_innnn
                            iv_pppar     = pppar
                            iv_ok_code   = ok_code  ).

    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.


  method IF_EX_HRBAS00INFTY~BEFORE_OUTPUT.
  endmethod.


  method IF_EX_HRBAS00INFTY~BEFORE_UPDATE.
  endmethod.


METHOD if_ex_hrbas00infty~in_update.

  DATA: ls_p9103 TYPE p9103.

  "Удаление ИТ9103
  LOOP AT plog_tab ASSIGNING FIELD-SYMBOL(<ls_pltab>) WHERE infty = '9103'
                                                        AND opera = 'D'.
    CLEAR ls_p9103.
    cl_hr_pnnnn_type_cast=>wplog_to_pnnnn( EXPORTING wplog = <ls_pltab>-pplog
                                           IMPORTING pnnnn = ls_p9103 ).

    CHECK ls_p9103-tabnr IS NOT INITIAL.
    read table  plog_tab ASSIGNING FIELD-SYMBOL(<ls_pltabi>) with key infty = '9103' opera = 'I'.
    if sy-subrc ne 0.
    DELETE FROM zhrt_9103_commt WHERE tabnr = ls_p9103-tabnr.
    endif.
  ENDLOOP.
ENDMETHOD.


METHOD _process_competition.

  FIELD-SYMBOLS: <ls_p9103> TYPE p9103.

  CASE iv_ok_code.
    WHEN 'UPD'."Сохранение
      ASSIGN ('(MP910300)P9103') TO <ls_p9103>.
      IF <ls_p9103> IS ASSIGNED.

        DO 1 TIMES.
          CHECK <ls_p9103>-status <> zcl_mss_data_assistent=>gc_kstatus_withdraw."Если не "Отозвано"

          IF ( <ls_p9103>-kondt IS NOT INITIAL AND <ls_p9103>-kondt <= sy-datum ) OR <ls_p9103>-knitg IS NOT INITIAL.
            <ls_p9103>-status = zcl_mss_data_assistent=>gc_kstatus_close. "«Закрыто»
          ELSEIF <ls_p9103>-bkper IS NOT INITIAL.
            <ls_p9103>-status = zcl_mss_data_assistent=>gc_kstatus_process. "«В работе»
          ELSE.
            <ls_p9103>-status = zcl_mss_data_assistent=>gc_kstatus_create. "«Создана»
          ENDIF.
        ENDDO.

      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.
ENDCLASS.
