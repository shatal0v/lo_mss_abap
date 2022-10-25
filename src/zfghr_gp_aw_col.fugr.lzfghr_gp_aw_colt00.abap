*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.12.2019 at 12:55:43
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHRT_GP_AW_COLOR................................*
DATA:  BEGIN OF STATUS_ZHRT_GP_AW_COLOR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRT_GP_AW_COLOR              .
CONTROLS: TCTRL_ZHRT_GP_AW_COLOR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHRT_GP_AW_COLOR              .
TABLES: ZHRT_GP_AW_COLOR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
