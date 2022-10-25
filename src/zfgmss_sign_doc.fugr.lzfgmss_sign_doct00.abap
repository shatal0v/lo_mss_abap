*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 19.10.2017 at 10:56:16
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVMSS_SIGN_DOC..................................*
TABLES: ZVMSS_SIGN_DOC, *ZVMSS_SIGN_DOC. "view work areas
CONTROLS: TCTRL_ZVMSS_SIGN_DOC
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVMSS_SIGN_DOC. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVMSS_SIGN_DOC.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVMSS_SIGN_DOC_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVMSS_SIGN_DOC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVMSS_SIGN_DOC_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVMSS_SIGN_DOC_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVMSS_SIGN_DOC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVMSS_SIGN_DOC_TOTAL.

*.........table declarations:.................................*
TABLES: T554S                          .
TABLES: T554T                          .
TABLES: ZTMSS_SIGN_DOC                 .
