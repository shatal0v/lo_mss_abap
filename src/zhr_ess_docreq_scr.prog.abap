*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_DOCREQ_SCR
*&---------------------------------------------------------------------*
DATA: gv_dats  TYPE dats.
DATA: gv_orgeh TYPE orgeh.
DATA: gv_status1 TYPE ZEHR_IPRREQ_STATUS.
data: lt_return type table of DDSHRETVAL.
data: ls_return like line of lt_return.

SELECTION-SCREEN BEGIN OF BLOCK b1 .
SELECT-OPTIONS: s_orgeh FOR gv_orgeh .
SELECT-OPTIONS: s_status for gv_status1   .
parameters: p_gjahr  type gjahr  OBLIGATORY .
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_orgeh-low.
  DATA:  zzselected_objid TYPE objec-realo.
    zzselected_objid = s_orgeh-low.

    CALL FUNCTION 'RH_TYPE_STRUC_HELP'
      EXPORTING
        act_search_otype         = 'P'
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
      s_orgeh-low = zzselected_objid.
    ENDIF.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_status-low .

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VALUE'
      value_org       = 'S'
      window_title    = 'Title'
    TABLES
      value_tab       = gt_data
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  if sy-subrc eq 0.
    read table lt_return into ls_return index 1.
    if sy-subrc eq 0.
      s_status-low = ls_return-FIELDVAL.
    endif.

  endif.
