* regenerated at 17.08.2017 18:02:30
FUNCTION-POOL zfghr_entr_user            MESSAGE-ID sv.

* INCLUDE LZFGHR_ENTR_USERD...               " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzfghr_entr_usert00                     . "view rel. data dcl.

DATA: BEGIN OF zs_line
, main_user_fio     TYPE text100
, entrance_user_fio TYPE text100
, END OF zs_line
.

DATA:lo_assistent TYPE REF TO zcl_mss_data_assistent.
