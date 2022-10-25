*&---------------------------------------------------------------------*
*&  Include           ZHR_PA_D028_CLS
*&---------------------------------------------------------------------*

CLASS lcl DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
           , get_peras
           , print
           , export
           , f4_orgeh CHANGING p_value TYPE orgeh
           , get_group  IMPORTING iv_stell       TYPE p0001-stell
                                 iv_date        TYPE dats
                       EXPORTING ev_group_name  TYPE text255
                                 ev_group_objid TYPE hrobjid,
              get_categor IMPORTING iv_group_objid  TYPE hrobjid
                                  iv_date         TYPE dats
                        EXPORTING ev_categor_name TYPE text255.

    TYPES: BEGIN OF ty_head,
                field_01 TYPE string "Дата отчета
               ,field_02 TYPE string "Название ОИВ
       ,END OF ty_head.
    TYPES: BEGIN OF ty_main
              ,field_03 TYPE text250 "№
              ,field_04 TYPE text250 "Id шт. должности
              ,field_05 TYPE text250 "Код шт. должности
              ,field_06 TYPE text250 "Категория, группа
              ,field_07 TYPE text250 "Название штатной должности
              ,field_08 TYPE text250 "Наименование подразделения
              ,field_09 TYPE text250 "Фамилия, имя, отчество
              ,field_10 TYPE text250 "Сведения об образовании
              ,field_11 TYPE text250 "Сведения об имеющихся ученой степени и(или) ученом звании
              ,field_12 TYPE text250 "Дата зачисления в кадровый резерв
              ,field_13 TYPE text250 "Примечание
        , END OF ty_main.
    TYPES: BEGIN OF ty_pa9001
                 ,pernr TYPE pa9001-pernr
                 ,begda TYPE pa9001-begda
                 ,flag_pa(1) TYPE c
           , END OF ty_pa9001.
    DATA:  t_main TYPE TABLE OF ty_main.
    DATA:  t_head TYPE TABLE OF ty_head.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_edu,
             field_01 TYPE text250,
             field_02 TYPE i,
           END OF ty_edu.

    DATA:  s_main TYPE ty_main
          ,s_head TYPE  ty_head
          , lv_clas type PSPAR-TCLAS
          , t_edu TYPE TABLE OF ty_edu
          , ls_edu TYPE ty_edu
          , lt_pa9001 TYPE TABLE OF ty_pa9001
          , ls_pa9001 TYPE ty_pa9001
          , get_data TYPE REF TO zcl_hr_get_data
          .

    CONSTANTS: c_form TYPE wwwdatatab-objid VALUE 'ZHR_PA_D040'.
    DATA: mv_begda TYPE dats.
    DATA: mv_endda TYPE dats.
*    METHODS: field_07
*    ,field_10
    .

ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    get_data = NEW zcl_hr_get_data( ).

  ENDMETHOD.
  METHOD f4_orgeh.

    DATA:  zzselected_objid TYPE objec-realo.
    zzselected_objid = p_value.

    CALL FUNCTION 'RH_TYPE_STRUC_HELP'
      EXPORTING
        act_search_otype         = 'O'
*       ACT_SEARCH_WEGID         =
*       ACT_SEARCH_SVECT         = '1'
*       SET_MODE                 =
*       ACT_ROOT_OT              =
*       ACT_ROOT_ID              =
*       ACT_PLVAR                =
        act_search_begda         = '19000101'
        act_search_endda         = '19001231'
*       NO_SEARK                 = 'X'
*       ACT_LIST_TYPE            =
*       ACT_INT_WEGID            =
*       SELECTED_OBJ_APPEND      =
*       CHANGE_SEARCH_TYPE       =
*       RESTRICT_CALLBACK        =
      IMPORTING
*       SELECTED_PLVAR           = ''
*       SELECTED_OTYPE           =
        selected_objid           = zzselected_objid
*       CHANGED_FLAG             =
*       LAST_OK_CODE             =
*   TABLES
*       SELECTED_OBJECTS         =
      EXCEPTIONS
        no_active_plvar          = 1
        no_object_selected       = 2
        no_struc_search_possible = 3
        OTHERS                   = 4.

    IF sy-subrc = 0.
      p_value = zzselected_objid.
    ENDIF.

  ENDMETHOD.                    "AT_SELECTION_SCREEN_OUTPUT
  METHOD get_peras.
    DATA: ls_orgeh LIKE LINE OF p_orgeh.
    DATA: lt_tab TYPE STANDARD TABLE OF swhactor.
    DATA: ls_tab TYPE swhactor.
    DATA: lv_plans TYPE plans.
    data: V_SEPA type c value cl_abap_char_utilities=>NEWLINE .
    DATA: lv_plans_current TYPE plans.
    DATA  lv_date_from TYPE dats.
    DATA   cnt TYPE i.
    DATA:  lv_group_name  TYPE text255.
    DATA:  lv_group_objid  TYPE hrobjid.
    DATA:  lv_stell TYPE stell.
    DATA:  lv_categor_name  TYPE text255.
    DATA : lv_sobid TYPE hrp1001-sobid.
    DATA: l_initial TYPE sy-datum." pa9001-ZEXDATE .
    DATA: lt1001 TYPE STANDARD TABLE OF p1001,
          ls1001 TYPE p1001.
    DATA: lt1000 TYPE STANDARD TABLE OF p1000,
          ls1000 TYPE p1000.
    DATA: lt_p0022 TYPE TABLE OF p0022.

*    clear l_initial.
    mv_begda = p_date.
    mv_endda = p_date.
*    CREATE OBJECT l_lcl.
    cnt = 1.
    LOOP AT p_orgeh INTO ls_orgeh.

* -- Получаем все штатные должности
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = 'O'
          act_objid      = ls_orgeh-low
          act_wegid      = 'O-O-S'
          act_plvar      = '01'
          act_begda      = mv_begda
          act_endda      = mv_endda
        TABLES
          result_tab     = lt_tab
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.                               "#EC FB_RC

      DELETE lt_tab WHERE otype NE 'S'.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = mv_begda
          days      = 0
          months    = 0
          signum    = '-' " to calculate previous date
          years     = 3
        IMPORTING
          calc_date = lv_date_from.

      CALL METHOD zcl_calendar=>get_date_dd_month_yy
        EXPORTING
          i_date  = mv_begda
          i_short = space
        IMPORTING
          e_date  = s_head-field_01.

*        s_main-field_01 = mv_begda.
      s_head-field_02 =   get_data->get_objid_verbal( i_plvar = '01' i_otype = 'O' i_objid = ls_orgeh-low i_begda = mv_begda ).
      APPEND s_head TO t_head.
      LOOP AT lt_tab INTO ls_tab.
        CLEAR ms_data.
        CLEAR s_main.
        lv_plans = ls_tab-objid.


        "Далее среди них оставляем только те, по которым есть кадровый резерв:
        "•  PA9001-PLANS, при условии что PA9001-ZEXDAT =’ ‘ и PA9001-BEGDA + 3 года>= P_DATE

        REFRESH lt_pa9001.
        SELECT pernr begda INTO  TABLE lt_pa9001 FROM pa9001 WHERE begda <= mv_begda AND begda >= lv_date_from
          AND plans EQ lv_plans  AND   ( zexdate IS NULL OR zexdate EQ '00000000' ).
        IF sy-subrc EQ 0.
          LOOP AT lt_pa9001 ASSIGNING FIELD-SYMBOL(<fs_pa9001>).
            <fs_pa9001>-flag_pa = 'X'.
          ENDLOOP.
        ENDIF.
        "•  PB4002-OBJID, при условии что PB4002-ZZEXDAT=’ ‘ и PB4002-APSTV=3 и PB4002-BEGDA + 3 года>= P_DATE
        "VACID
        SELECT pernr begda APPENDING CORRESPONDING FIELDS OF TABLE lt_pa9001 FROM pb4002 WHERE objid EQ lv_plans AND apstv EQ '3' AND  begda <= mv_begda
          AND begda >= lv_date_from AND otype EQ 'S' AND (  zzexdat  IS NULL or zzexdat eq '00000000') .



*              ,field_01 TYPE text250 "Дата отчета
*              ,field_02 TYPE text250 "Название ОИВ
*              ,field_03 TYPE text250 "№
*              ,field_04 TYPE text250 "Id шт. должности
*              ,field_05 TYPE text250 "Код шт. должности
*              ,field_06 TYPE text250 "Категория, группа
*              ,field_07 TYPE text250 "Название штатной должности
*              ,field_08 TYPE text250 "Наименование подразделения
*              ,field_09 TYPE text250 "Фамилия, имя, отчество
*              ,field_10 TYPE text250 "Сведения об образовании
*              ,field_11 TYPE text250 "Сведения об имеющихся ученой степени и(или) ученом звании
*              ,field_12 TYPE text250 "Дата зачисления в кадровый резерв
*              ,field_13 TYPE text250 "Примечание
        CHECK lines( lt_pa9001 ) > 0.
*        CLEAR s_main.


        s_main-field_03 = cnt.  cnt = cnt + 1.
        s_main-field_04 = lv_plans.
        "Шифр   CONCATENATE waobjec-short+4(1) waobjec-short+6(2) INTO wa_node-short . " Род должности + (Категория+Группа)

        SELECT SINGLE short INTO @DATA(lv_short) FROM hrp1000 WHERE plvar EQ '01' AND objid EQ @lv_plans AND otype EQ 'S'.
        IF sy-subrc EQ 0.
          s_main-field_05 = lv_short+6(2).
        ENDIF.



*        BREAK-POINT.
        SELECT SINGLE sobid INTO lv_sobid FROM hrp1001 WHERE otype EQ 'S' AND plvar EQ '01'
          AND objid EQ lv_plans AND subty EQ 'B007' AND sclas EQ 'C' AND begda <= mv_begda AND endda >= mv_begda.
        IF sy-subrc EQ 0.
          clear : lv_group_name , lv_group_objid , lv_categor_name.
          lv_stell = lv_sobid.
          get_group(
              EXPORTING iv_stell        = lv_stell " ps_pernr-stell
                        iv_date         = sy-datum
              IMPORTING ev_group_name   = lv_group_name
                        ev_group_objid  = lv_group_objid ).

          IF lv_group_objid IS NOT INITIAL.
            get_categor( EXPORTING iv_group_objid = lv_group_objid iv_date = mv_begda
                         IMPORTING ev_categor_name  = lv_categor_name
                        ).
            if lv_categor_name is not initial and lv_group_name is not initial.
            CONCATENATE lv_group_name  ',' INTO lv_group_name.
            CONCATENATE lv_group_name lv_categor_name INTO s_main-field_06 SEPARATED BY space.
            else.
              CONCATENATE lv_group_name lv_categor_name INTO s_main-field_06 .
            endif.
          ENDIF.

        ENDIF.



* -- Имя должности
        DATA: lv_dname TYPE stext.
        CALL METHOD zcl_hr_data=>get_name_plans
          EXPORTING
            i_plans = lv_plans
            i_date  = mv_begda
          IMPORTING
            e_lname = s_main-field_07.  "lv_dname.

        "Выводим название всех орг.единиц исключая 1 и 2 уровень
        s_main-field_08 = zcl_hr_data_utils=>get_plans_and_ogreh_name_by_s(  i_objid = lv_plans
                                                                                       i_begda = mv_begda
                                                                                       i_endda = mv_begda
                                                                                       i_flag_levels = '4').

        "Сведения об образовании
        LOOP AT lt_pa9001 INTO ls_pa9001.
          APPEND INITIAL LINE TO t_main ASSIGNING FIELD-SYMBOL(<t_main>).
          MOVE-CORRESPONDING s_main TO <t_main>.
          IF ls_pa9001-flag_pa EQ 'X'.
            lv_clas = 'A'.
            <t_main>-field_09 = get_data->get_pernr_fio( i_pernr = ls_pa9001-pernr i_begda = mv_begda i_type = 1 ).
          ELSE.
            lv_clas = 'B'.
            <t_main>-field_09 = get_data->get_pernr_fio( i_pernr = ls_pa9001-pernr i_begda = mv_begda i_tclas = 'B' i_type = 1 ).
          ENDIF.
          REFRESH lt_p0022.

          CALL FUNCTION 'HR_READ_INFOTYPE'
            EXPORTING
              tclas           = lv_clas
              pernr           = ls_pa9001-pernr
              infty           = '0022'
              endda           = mv_begda
            TABLES
              infty_tab       = lt_p0022
            EXCEPTIONS
              infty_not_found = 1
              OTHERS          = 2.
          REFRESH t_edu.
          LOOP AT lt_p0022 ASSIGNING FIELD-SYMBOL(<lt_p0022>) WHERE subty NP '9*' AND subty <> '19'.
            APPEND INITIAL LINE TO t_edu ASSIGNING FIELD-SYMBOL(<t_edu>).
            <t_edu>-field_01 = zcl_hr_data_utils=>get_education( <lt_p0022> ) && ';'.
            CASE <lt_p0022>-subty."Приоритет вывода
              WHEN '18'.
                <t_edu>-field_02 = '1'.
              WHEN '15'.
                <t_edu>-field_02 = '3'.
              WHEN '11'.
                <t_edu>-field_02 = '4'.
              WHEN '10'.
                <t_edu>-field_02 = '5'.
              WHEN '07'.
                <t_edu>-field_02 = '6'.
              WHEN '03'.
                <t_edu>-field_02 = '7'.
              WHEN OTHERS.
                <t_edu>-field_02 = '8'.
            ENDCASE.

          ENDLOOP.
          SORT t_edu BY field_02.
          LOOP AT t_edu INTO ls_edu.
            IF sy-tabix EQ 1.
              <t_main>-field_10 = ls_edu-field_01.
            ELSE.
              CONCATENATE    <t_main>-field_10 ls_edu-field_01 INTO <t_main>-field_10 SEPARATED BY v_sepa.
            ENDIF.

          ENDLOOP.


          "Сведения об имеющихся ученой степени и(или) ученом звании ДЛЯ ПОДТИПОВ 19 и 90
          REFRESH t_edu.
          LOOP AT lt_p0022 ASSIGNING <lt_p0022> WHERE subty = '90' OR subty = '19'.
            APPEND INITIAL LINE TO t_edu ASSIGNING <t_edu>.
            <t_edu>-field_01 = zcl_hr_data_utils=>get_education( <lt_p0022> ) && ';'.

          ENDLOOP.
          LOOP AT t_edu INTO ls_edu.
            IF sy-tabix EQ 1.
              <t_main>-field_11 = ls_edu-field_01.
            ELSE.
              CONCATENATE   <t_main>-field_11 ls_edu-field_01 INTO <t_main>-field_11 SEPARATED BY v_sepa.
            ENDIF.
          ENDLOOP.
          "Дата зачисления в кадровый резерв
*            <t_main>-field_12 = ls_pa9001-begda.
          CONCATENATE ls_pa9001-begda+6(2) '.' ls_pa9001-begda+4(2) '.' ls_pa9001-begda(4) INTO <t_main>-field_12.
          "Примечание
          IF ls_pa9001-flag_pa NE 'X'.
            <t_main>-field_13 = 'Кандидат'.
          ELSE.
            "1. Если резервист из PA и его штатная должность (PA0001-PLANS) и Штатная должность, по которой формирован резерв (PA9001-PLANS) принадлежат к разным ОИВ (Орг. единица 2-го уровня) то выводим "Иной ОИВ";
            DATA: t_struc_cur TYPE TABLE OF struc.
            DATA: t_struc  TYPE TABLE OF struc.
            SELECT SINGLE plans INTO lv_plans_current FROM pa0001 WHERE pernr = ls_pa9001-pernr AND begda <= mv_begda AND endda >= mv_begda.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'RH_STRUC_GET'
                EXPORTING
                  act_otype      = 'S'
                  act_objid      = lv_plans_current
                  act_wegid      = 'S_UP'
*                 ACT_INT_FLAG   =
                  act_plvar      = '01'
                  act_begda      = mv_begda
                  act_endda      = mv_begda
*           IMPORTING
*                 ACT_PLVAR      =
                TABLES
*                 RESULT_TAB     =
*                 RESULT_OBJEC   = t_OBJEC
                  result_struc   = t_struc_cur
                EXCEPTIONS
                  no_plvar_found = 1
                  no_entry_found = 2
                  OTHERS         = 3.
              DATA:lv_level TYPE i.

              CALL FUNCTION 'RH_STRUC_GET'
                EXPORTING
                  act_otype      = 'S'
                  act_objid      = lv_plans
                  act_wegid      = 'S_UP'
*                 ACT_INT_FLAG   =
                  act_plvar      = '01'
                  act_begda      = mv_begda
                  act_endda      = mv_begda
*           IMPORTING
*                 ACT_PLVAR      =
                TABLES
*                 RESULT_TAB     =
*                 RESULT_OBJEC   = t_OBJEC
                  result_struc   = t_struc
                EXCEPTIONS
                  no_plvar_found = 1
                  no_entry_found = 2
                  OTHERS         = 3.
              READ TABLE t_struc_cur ASSIGNING FIELD-SYMBOL(<t_cur>) WITH KEY pdown = 0.
              READ TABLE t_struc ASSIGNING FIELD-SYMBOL(<t_str>) WITH KEY pdown = 0.
              IF <t_str> IS ASSIGNED AND <t_cur> IS ASSIGNED AND <t_str>-objid EQ <t_cur>-objid.

                READ TABLE t_struc_cur ASSIGNING   <t_cur> WITH KEY pdown = <t_cur>-level.
                READ TABLE t_struc ASSIGNING   <t_str> WITH KEY pdown = <t_str>-level.
                IF <t_str> IS ASSIGNED AND <t_cur> IS ASSIGNED AND <t_str>-objid NE <t_cur>-objid.
                  <t_main>-field_13 = 'Иной ОИВ'.
                ENDIF.
              ENDIF.





            ENDIF.

          ENDIF.

        ENDLOOP.


      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

*  METHOD field_07.
*    DATA lv_text TYPE text250.
*    SELECT SINGLE wdgtx
*      FROM t554d
*      INTO s_main-field_07
*      WHERE sprsl = 'RU'
*      AND wdgrd = p0081-wdgrd.
*
*    WRITE p0081-begda TO lv_text DD/MM/YYYY.
*
**    CONCATENATE s_main-field_07 'от' lv_text 'г.' INTO s_main-field_07 SEPARATED BY space.
*    CONCATENATE s_main-field_07 'от' lv_text INTO s_main-field_07 SEPARATED BY space.
*
*  ENDMETHOD.
*
*  METHOD field_10.
*    DATA: lv_pernr_ruk TYPE p0001-pernr.
*    lv_pernr_ruk = zcl_hr_data_utils=>get_tn_ruk( i_orgeh = p0001-orgeh i_begda = p0298-begda i_endda = p0298-endda ).
*
*    s_main-field_10 = zcl_hr_data_utils=>get_plans_and_ogreh_name(  i_pernr = lv_pernr_ruk
*                                                                        i_begda = p0298-begda
*                                                                        i_endda = p0298-endda
*                                                                        i_flag_levels = '3').
*
*    s_main-field_11 =  get_data->get_pernr_fio( i_pernr = lv_pernr_ruk i_type = 1 i_begda = p0298-begda ).
*  ENDMETHOD.

  METHOD print.
    DATA:  mo_www   TYPE REF TO zcl_www_openform
          ,form_name TYPE wwwdatatab-objid
          .
    form_name = c_form.

*    IF form_name IS INITIAL.
*      form_name = sy-repid && '_1'.
*    ENDIF.
    CREATE OBJECT mo_www
      EXPORTING
        i_nogetnameform = 'X'.
*
    IF t_main IS INITIAL.
      APPEND INITIAL LINE TO t_main.
    ENDIF.
    IF t_head IS INITIAL.
      APPEND INITIAL LINE TO t_head.
    ENDIF.
*

    CALL METHOD mo_www->maping_field
      EXPORTING
        name_mark   = 'Field'
        split_field = '_'
        struct      = s_main.
*
**** -- Добавляем таблицу
    CALL METHOD mo_www->add_table
      EXPORTING
        i_line  = 'TABLE1'
        i_table = t_main[].

    CALL METHOD mo_www->add_table
      EXPORTING
        i_line  = 'HEADER1'
        i_table = t_head[].

    CALL METHOD mo_www->add_macros
      EXPORTING
        var_name    = 'ZZZ1'
        name_macros = 'AUTOFIT1'.

**    " вызов шаблона
    CALL METHOD mo_www->openform
      EXPORTING
        form_name   = form_name
        printdialog = ''
        protect     = ''
        optimize    = 0.
  ENDMETHOD.
  METHOD export.

    DATA lv_str TYPE xstring .

    delete t_main where  field_13 = 'Иной ОИВ'.
    CALL TRANSFORMATION zhr_pa_d040_new SOURCE header = t_head[] table01 = t_main[] RESULT XML lv_str .
    IF sy-subrc EQ 0.
      EXPORT lv_xstring = lv_str   TO MEMORY ID zcl_z_mss_emp_reserv_dpc_ext=>c_memory_id.
    ENDIF.
  ENDMETHOD.
  METHOD get_categor.
    DATA: lt1001 TYPE STANDARD TABLE OF p1001,
          ls1001 TYPE p1001.
    DATA: lt1000 TYPE STANDARD TABLE OF p1000,
          ls1000 TYPE p1000.

    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        plvar            = '01'
        otype            = '2G'
        objid            = iv_group_objid
        subty            = 'B044'
        begda            = iv_date
        endda            = iv_date
      TABLES
        i1001            = lt1001
      EXCEPTIONS
        nothing_found    = 1
        wrong_condition  = 2
        wrong_parameters = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE lt1001 INTO ls1001 WITH KEY relat = '044' sclas = '2K'.
    IF sy-subrc = 0.
      CALL FUNCTION 'RH_READ_INFTY_1000'
        EXPORTING
          plvar            = ls1001-plvar
          otype            = CONV otype( ls1001-sclas )
          objid            = CONV hrobjid( ls1001-sobid )
          begda            = iv_date
          endda            = iv_date
        TABLES
          i1000            = lt1000
        EXCEPTIONS
          nothing_found    = 1
          wrong_condition  = 2
          wrong_parameters = 3
          OTHERS           = 4.
      LOOP AT lt1000 INTO ls1000.
        EXIT.
      ENDLOOP.
      ev_categor_name = ls1000-stext.
    ENDIF.

  ENDMETHOD.                    "get_categor

  METHOD get_group.
  DATA: lt1001 TYPE STANDARD TABLE OF p1001,
          ls1001 TYPE p1001.
    DATA: lt1000 TYPE STANDARD TABLE OF p1000,
          ls1000 TYPE p1000.

    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        plvar            = '01'
        otype            = 'C'
        objid            = iv_stell
        subty            = 'B007'
        begda            = iv_date
        endda            = iv_date
      TABLES
        i1001            = lt1001
      EXCEPTIONS
        nothing_found    = 1
        wrong_condition  = 2
        wrong_parameters = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE lt1001 INTO ls1001 WITH KEY relat = '007' sclas = '2G'.
    IF sy-subrc = 0.
      CALL FUNCTION 'RH_READ_INFTY_1000'
        EXPORTING
          plvar            = ls1001-plvar
          otype            = CONV otype( ls1001-sclas )
          objid            = CONV hrobjid( ls1001-sobid )
          begda            = iv_date
          endda            = iv_date
        TABLES
          i1000            = lt1000
        EXCEPTIONS
          nothing_found    = 1
          wrong_condition  = 2
          wrong_parameters = 3
          OTHERS           = 4.
      LOOP AT lt1000 INTO ls1000.
        EXIT.
      ENDLOOP.
      split ls1000-stext at space into ev_group_name data(lv_tail).

      ev_group_objid = ls1001-sobid.
    else." Для вывода категорий без групп
       READ TABLE lt1001 INTO ls1001 WITH KEY relat = '007' sclas = '2K'.
       if sy-subrc eq 0.
          CALL FUNCTION 'RH_READ_INFTY_1000'
        EXPORTING
          plvar            = ls1001-plvar
          otype            = CONV otype( ls1001-sclas )
          objid            = CONV hrobjid( ls1001-sobid )
          begda            = iv_date
          endda            = iv_date
        TABLES
          i1000            = lt1000
        EXCEPTIONS
          nothing_found    = 1
          wrong_condition  = 2
          wrong_parameters = 3
          OTHERS           = 4.
      LOOP AT lt1000 INTO ls1000.
        EXIT.
      ENDLOOP.
      ev_group_name  = ls1000-stext.
      ev_group_objid = ls1001-sobid.
     endif.
    ENDIF.
*    zcl_hr_data_utils=>get_stell_group(
*      EXPORTING
*        iv_stell       = iv_stell    " Должность
*        iv_date        = iv_date    " Дата
*      IMPORTING
*        ev_group_name  = ev_group_name    " Наименование группы должностей
*        ev_group_objid = ev_group_objid    " OBJID
*    ).

  ENDMETHOD.                    "get_group

ENDCLASS.
