*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 17.03.2020 at 13:38:29
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVHR_ENTR_USER..................................*
TABLES: ZVHR_ENTR_USER, *ZVHR_ENTR_USER. "view work areas
CONTROLS: TCTRL_ZVHR_ENTR_USER
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVHR_ENTR_USER. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVHR_ENTR_USER.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVHR_ENTR_USER_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVHR_ENTR_USER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVHR_ENTR_USER_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVHR_ENTR_USER_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVHR_ENTR_USER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVHR_ENTR_USER_TOTAL.

*.........table declarations:.................................*
TABLES: ZTHR_ENTR_USER                 .
