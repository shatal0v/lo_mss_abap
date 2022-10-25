FUNCTION-POOL zfghr_mss_attach.             "MESSAGE-ID ..

* INCLUDE LZFGHR_MSS_ATTACHD...              " Local class definition

TYPE-POOLS: icon.

TYPES: BEGIN OF ty_data
     . INCLUDE TYPE sgos_atta
.
TYPES: ftype  TYPE zthr_mss_at_attr-ftype
     , object TYPE zthr_mss_at_attr-object
     , seqnr  TYPE zthr_mss_at_attr-seqnr
     , dele   type flag
     , END OF ty_data
     .

DATA: ok_code      TYPE sy-ucomm
    , lo_container TYPE REF TO cl_gui_custom_container
    , lo_grid      TYPE REF TO cl_gui_alv_grid
    , lt_data      TYPE TABLE OF ty_data
    , lt_dele      TYPE TABLE OF ty_data

    , lo_assistent TYPE REF TO zcl_mss_data_assistent

    , lv_object TYPE zthr_mss_at_attr-object
    , lv_ftype  TYPE zthr_mss_at_attr-ftype
    .
