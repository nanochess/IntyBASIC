	;
	; Prologue for IntyBASIC programs
	; by Oscar Toledo G.  http://nanochess.org/
	;
	; Revision: Jan/30/2014. Spacing adjustment and more comments.
	; Revision: Apr/01/2014. It now sets the starting screen pos. for PRINT
	; Revision: Aug/26/2014. Added PAL detection code.
	; Revision: Dec/12/2014. Added optimized constant multiplication routines.
	;                        by James Pujals.
	; Revision: Jan/25/2015. Added marker for automatic title replacement.
	;                        (option --title of IntyBASIC)
	; Revision: Aug/06/2015. Turns off ECS sound. Seed random generator using
	;                        trash in 16-bit RAM. Solved bugs and optimized
	;                        macro for constant multiplication.
	; Revision: Jan/12/2016. Solved bug in PAL detection.
	; Revision: May/03/2016. Changed in _mode_select initialization.
	; Revision: Jul/31/2016. Solved bug in multiplication by 126 and 127.
	; Revision: Sep/08/2016. Now CLRSCR initializes screen position for PRINT,
	;                        this solves bug when user programs goes directly
	;                        to PRINT.
	; Revision: Oct/21/2016. Accelerated MEMSET.
	; Revision: Jan/09/2018. Adjusted PAL/NTSC constant.
	; Revision: Feb/05/2018. Forces initialization of Intellivoice if included.
	;                        So VOICE INIT ceases to be dangerous.
	; Revision: Oct/30/2018. Redesigned PAL/NTSC detection using intvnut code,
	;                        also now compatible with Tutorvision. Reformatted.
	; Revision: Jan/10/2018. Added ECS detection.
	;

;;==========================================================================;;
;; IntyBASIC SDK Library: romseg-bs.mac                                     ;;
;;--------------------------------------------------------------------------;;
;;  This macro library is used by the IntyBASIC SDK to manage ROM address   ;;
;;  segments and generate statistics on program ROM usage.  It is an        ;;
;;  extension of the "romseg.mac" macro library with added support for      ;;
;;  bank-switching.                                                         ;;
;;                                                                          ;;
;;  The library is based on a similar module created for the P-Machinery    ;;
;;  programming framework, which itself was based on the "CART.MAC" macro   ;;
;;  library originally created by Joe Zbiciak and distributed as part of    ;;
;;  the SDK-1600 development kit.                                           ;;
;;--------------------------------------------------------------------------;;
;;      The file is placed into the public domain by its author.            ;;
;;      All copyrights are hereby relinquished on the routines and data in  ;;
;;      this file.  -- James Pujals (DZ-Jay), 2024                          ;;
;;==========================================================================;;

;; ======================================================================== ;;
;;  ROM MANAGEMENT STRUCTURES                                               ;;
;; ======================================================================== ;;

                ; Internal ROM information structure
_rom            STRUCT  0
@@null          QEQU    0
@@invalid       QEQU    -1

@@legacy        QEQU    0
@@static        QEQU    1
@@dynamic       QEQU    2

@@mapcnt        QEQU    8
@@pgsize        QEQU    4096

@@open          QSET    @@invalid
@@error         QSET    @@null

@@segcnt        QSET    0
@@segs          QSET    0
                ENDS

.ROM            STRUCT  0
@@CurrentSeg    QSET    _rom.invalid    ; No open segment

@@Size          QSET    0
@@Used          QSET    0
@@Available     QSET    0

                ; Initialize segment counters
@@Segments[_rom.legacy ]    QSET    0
@@Segments[_rom.static ]    QSET    0
@@Segments[_rom.dynamic]    QSET    0

                ENDS

_rom_stat       STRUCT  0
@@space         QEQU    "                                                                           " ; 75
@@single        QEQU    "---------------------------------------------------------------------------"
@@double        QEQU    "==========================================================================="
                ENDS


;; ======================================================================== ;;
;;  __rom_raise_error(err, desc)                                            ;;
;;  Generates an assembler error and sets the global error flag.            ;;
;;                                                                          ;;
;;  NOTE:   Both strings must be devoid of semi-colons and commas, or       ;;
;;          Bad Things(tm) may happen during pre-processing.                ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      err         The error message.                                      ;;
;;      desc        Optional error description, or _rom.null if none.       ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 (failed).                                            ;;
;; ======================================================================== ;;
MACRO   __rom_raise_error(err, desc)
;
    LISTING "off"

_rom.error      QSET    _rom.invalid

_rom.err_len    QSET    _rom.null
_rom.err_len    QSET    %desc%

        IF (_rom.err_len <> _rom.null)
            ERR  $(%err%, ": ", %desc%)
        ELSE
            ERR  $(%err%)
        ENDI

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  __rom_reset_error                                                       ;;
;;  Resets the global error flag.                                           ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      None.                                                               ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  0 (no error)                                            ;;
;; ======================================================================== ;;
MACRO   __rom_reset_error
;
_rom.error      QSET    _rom.null
ENDM

;; ======================================================================== ;;
;;  __rom_validate_map(map)                                                 ;;
;;  Validates the requested ROM map.                                        ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      map         The ROM map selected. Valid values are 0 to 7.          ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_validate_map(map)
;
_rom.max        QSET    (_rom.mapcnt - 1)

        IF (((%map%) < 0) OR ((%map%) > _rom.max))
            __rom_raise_error(["Invalid ROM map number (", $#(%map%), ")"], ["Valid maps are from 0 to ", $#(_rom.max), "."])
        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_validate_type(type)                                               ;;
;;  Validates the requested segment type symbol.                            ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      type        The segment type to validate.                           ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_validate_type(type)
;
        IF ((CLASSIFY(_rom.%type%) = -10000))
            __rom_raise_error("Invalid ROM segment type \"%type%\".", _rom.null)
        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_validate_segment(seg)                                             ;;
;;  Validates the requested segment number to ensure it is supported by the ;;
;;  active memory map.                                                      ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      seg         The segment number to validate.                         ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_validate_segment(seg)
;
        IF ((CLASSIFY(_rom.segidx[%seg%]) = -10000))
            __rom_raise_error(["Invalid ROM segment number #", $#(%seg%), " for selected memory map."], _rom.null)
        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_validate_seg_bank(seg, bank)                                      ;;
;;  Validates the requested dynamic segment and bank number to ensure it is ;;
;;  supported by the active memory map.                                     ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      seg         The segment number to validate, or -1 for the first one.;;
;;      bank        The dynamic segment bank number to validate.            ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_validate_seg_bank(seg, bank)
;
        ; First, check if the map supports dynamic segments at all
        IF (.ROM.Segments[_rom.dynamic] = 0)
                __rom_raise_error("The selected memory map does not support dynamic segments.", _rom.null)
        ENDI

        ; Validate the segment
        IF (_rom.error = _rom.null)
                __rom_validate_segment(%seg%)
        ENDI

        ; Validate the bank
        IF (_rom.error = _rom.null)

_rom.num    QSET    _rom.segidx[%seg%]
_rom.max    QSET    (_rom.bnkcnt[_rom.num] - 1)

            IF (((%bank%) < 0) OR ((%bank%) > _rom.max))
                __rom_raise_error(["Invalid bank number #", $#(%bank%), " for dynamic ROM segment #", $#(%seg%)], ["Must be between 0 and ", $#(_rom.max), "."])
            ENDI

        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_validate_segnum(segnum)                                           ;;
;;  Validates the requested internal segment number to ensure that it is    ;;
;;  valid within the active memory map.                                     ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      segnum      The internal memory map segment number to validate.     ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_validate_segnum(segnum)
;
        IF _EXPMAC ((CLASSIFY(_rom.t[%segnum%]) = -10000))
            __rom_raise_error(["Unknown internal ROM segment number #", $#(%segnum%), "."], _rom.null)
        ELSE

_rom.type   QSET    _rom.t[%segnum%]
_rom.max    QSET    (.ROM.Segments[_rom.type] - 1)

            IF _EXPMAC (((%segnum%) < 0) OR ((%segnum%) > _rom.max))
                __rom_raise_error(["Invalid internal segment number #", $#(%segnum%), " for selected memory map"], ["Must be a value between 0 and ", $#(_rom.max), "."])
            ENDI

        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_assert_setup(label)                                               ;;
;;  Ensures that ROM.Setup has been called.                                 ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      label       A quoted-string containing the label of the asserting   ;;
;;                  macro or function.                                      ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_assert_setup(label)
;
        IF ((CLASSIFY(_rom.init) = -10000))
            __rom_raise_error(["ROM", ".Setup directive must be used before calling ROM.", %label%, "."], _rom.null)
        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_assert_romseg_support(label)                                      ;;
;;  Prevents the invocation of a feature that is not supported by the       ;;
;;  legacy memory map when ROM map #0 is selected.                          ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      label       A quoted-string containing the label of the asserting   ;;
;;                  macro or function.                                      ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_assert_romseg_support(label)
;
        IF (_rom.map = 0)
            __rom_raise_error([%label%, " failed"], ["Legacy ROM map #", $#(_rom.map), " does not support it."])
        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_assert_def_order(type)                                            ;;
;;  Ensures that segments are defined in the proper order:  all legacy and  ;;
;;  static segments first, followed by all dynamic ones.                    ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      type        The segment type to check.                              ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_assert_def_order(type)
;
_rom.type   QSET    _rom.%type%

        ; Make sure all dynamic segments are defined last
        IF (((_rom.type = _rom.legacy) OR (_rom.type = _rom.static)) AND (.ROM.Segments[_rom.dynamic] > 0))
            __rom_raise_error("Invalid ROM segment definition order", "All static and legacy segments must be defined before any dynamic ones.")
        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_segmem_size(segnum)                                               ;;
;;  Computes the total size of a ROM segment.                               ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      segnum      The internal segment for which to compute the size.     ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      None.                                                               ;;
;; ======================================================================== ;;
MACRO   __rom_segmem_size(segnum)
;
.ROM.SegSize[%segnum%]  QSET (_rom.e[%segnum%] - _rom.b[%segnum%] + 1)
ENDM

;; ======================================================================== ;;
;;  __rom_segmem_used(segnum)                                               ;;
;;  Computes the usage of a ROM segment.                                    ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      segnum      The internal segment for which to compute the usage.    ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      None.                                                               ;;
;; ======================================================================== ;;
MACRO   __rom_segmem_used(segnum)
;
.ROM.SegUsed[%segnum%]  QSET (_rom.pos[%segnum%] - _rom.b[%segnum%])
ENDM

;; ======================================================================== ;;
;;  __rom_segmem_available(segnum)                                          ;;
;;  Computes the available space of a ROM segment.                          ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      segnum      The internal segment for which to compute the available ;;
;;                  space.                                                  ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      None.                                                               ;;
;; ======================================================================== ;;
MACRO   __rom_segmem_available(segnum)
;
.ROM.SegAvlb[%segnum%]  QSET (_rom.e[%segnum%] - _rom.pos[%segnum%] + 1)
ENDM

;; ======================================================================== ;;
;;  __rom_calculate_stats                                                   ;;
;;  Computes the total ROM size and usage statistics.                       ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      None.                                                               ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      None.                                                               ;;
;; ======================================================================== ;;
MACRO   __rom_calculate_stats
;
_rom.segnum     QSET    0

            REPEAT (_rom.segcnt)

.ROM.Size       SET     (.ROM.Size      + .ROM.SegSize[_rom.segnum])
.ROM.Used       SET     (.ROM.Used      + .ROM.SegUsed[_rom.segnum])
.ROM.Available  SET     (.ROM.Available + .ROM.SegAvlb[_rom.segnum])

_rom.segnum     QSET    (_rom.segnum + 1)

            ENDR
ENDM

;; ======================================================================== ;;
;;  __rom_init_segmem(seg, start, end, page, type)                          ;;
;;  Initializes and configures the requested memory map segment indicated   ;;
;;  by "seg," using the provided arguments.  All internal data structures   ;;
;;  for memory integrity and accounting are also initialized.               ;;
;;                                                                          ;;
;;                                                                          ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      seg         The number of the segment to initialize.                ;;
;;      start       The start address of the segment.                       ;;
;;      end         The end address of the segment.                         ;;
;;      page        An optional page number to switch the segment to.       ;;
;;      type        The type of segment:  "legacy," "static," or "dynamic". ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_init_segmem(seg, start, end, page, type)
;
                __rom_validate_type(%type%)
                __rom_assert_def_order(%type%)

        IF (_rom.error = _rom.null)

_rom.type           QSET    _rom.%type%
_rom.num            QSET    _rom.segcnt                 ; New internal segment number
_rom.segcnt         QSET    (_rom.segcnt + 1)           ; Total internal segments (so far)

            ; Keep track of the index for the given segment
            IF ((CLASSIFY(_rom.segidx[%seg%]) = -10000))
_rom.segs           QSET    (_rom.segs + 1)
_rom.segidx[%seg%]  QSET    _rom.num
_rom.bnkcnt[%seg%]  QSET    0
            ENDI

_rom.b   [_rom.num] QSET    %start%                     ; Start address
_rom.e   [_rom.num] QSET    %end%                       ; End address
_rom.p   [_rom.num] QSET    %page%                      ; Page number
_rom.t   [_rom.num] QSET    _rom.type                   ; Segment type

            IF ((%page%) <> _rom.invalid)

_rom.sbase          QSET    (_rom.b[_rom.num] AND $F000)
_rom.send           QSET    (_rom.e[_rom.num] OR  $0FFF)
_rom.spages         QSET    (((_rom.send - _rom.sbase) + 1) / _rom.pgsize)

_rom.pgs [_rom.num] QSET    _rom.spages                 ; Physical pages in segment

                ; For dynamic segments, keep track of
                ; the number of banks.
                IF (_rom.type = _rom.dynamic)
_rom.bnk [_rom.num] QSET    _rom.bnkcnt[%seg%]          ; Logical bank number
_rom.bnkcnt[%seg%]  QSET    (_rom.bnkcnt[%seg%] + 1)    ; Banks in segment
                ENDI

            ELSE

_rom.bnk [_rom.num] QSET    _rom.invalid
_rom.pgs [_rom.num] QSET    _rom.invalid

            ENDI

_rom.seg [_rom.num] QSET    %seg%
_rom.pos [_rom.num] QSET    %start%                     ; Starting position

.ROM.Segments[_rom.type] QSET   (.ROM.Segments[_rom.type] + 1)

            IF (_rom.type <> _rom.legacy)
                ; Initialize accounting statistics
                __rom_segmem_size     (_rom.num)
                __rom_segmem_used     (_rom.num)
                __rom_segmem_available(_rom.num)
            ENDI
        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_check_segmem_range(addr, segnum)                                  ;;
;;  Checks an address to make sure it falls within a given ROM segment.     ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      addr        The address to check.                                   ;;
;;      segnum      The internal segment for which to check the range.      ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;;      _rom.ovrflo The number of words in excess.                          ;;
;;      _rom.sidx   The logical segment number.                             ;;
;;      _rom.bidx   The logical bank number.                                ;;
;; ======================================================================== ;;
MACRO   __rom_check_segmem_range(addr, segnum)
;
        IF (_rom.open = %segnum%)
_rom.rbase      QSET    _rom.cb
_rom.rend       QSET    _rom.ce
        ELSE
_rom.rbase      QSET    _rom.b[%segnum%]
_rom.rend       QSET    _rom.e[%segnum%]
        ENDI


        IF _EXPMAC ((%addr%) < _rom.rbase) OR (((%addr%) - 1) > _rom.rend)

_rom.ovrflo     QSET    ((%addr%) - _rom.rend - 1)
_rom.sidx       QSET    _rom.seg[%segnum%]
_rom.bidx       QSET    _rom.bnk[%segnum%]

          ; NOTE: Overflows are significant, so we want to
          ;       display such errors in STDOUT as well.
          IF _EXPMAC (_rom.t[%segnum%] = _rom.dynamic)
            __rom_raise_error(["Dynamic ROM segment overflow in segment #", $#(_rom.sidx), ", bank #", $#(_rom.bidx)], ["Total ", $#(_rom.ovrflo), " words in excess."])

            SMSG $("ERROR: Overflow in dynamic ROM segment #", $#(_rom.sidx), ", bank #", $#(_rom.bidx), ": Total ", $#(_rom.ovrflo), " words in excess.")
          ELSE
            __rom_raise_error(["ROM segment overflow in segment #", $#(_rom.sidx)], ["Total ", $#(_rom.ovrflo), " words in excess."])

            SMSG $("ERROR: Overflow in ROM segment #", $#(_rom.sidx), ": Total ", $#(_rom.ovrflo), " words in excess.")
          ENDI
        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_set_pc_addrs(addr, page)                                          ;;
;;  Relocates the program counter to the given address, selecting a         ;;
;;  specific page if requested.                                             ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      addr        The new address to set the program counter.             ;;
;;      page        An optional page to select (or _rom.invalid if none).   ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      None.                                                               ;;
;; ======================================================================== ;;
MACRO   __rom_set_pc_addrs(addr, page)
;
        IF (%page% <> _rom.invalid)

            LISTING "on"
                ; Open segment page
                ORG     %addr%:%page%
            LISTING "prev"

        ELSE

            LISTING "on"
                ; Open segment
                ORG     %addr%
            LISTING "prev"

        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_open_seg(segnum)                                                  ;;
;;  Opens a ROM segment.  If the segment is already open, it checks the     ;;
;;  current program counter to ensure it is still within valid range.       ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      segnum      The internal memory map segment to open.                ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_open_seg(segnum)
;
                __rom_reset_error
                __rom_validate_segnum(%segnum%)

        IF _EXPMAC (_rom.error = _rom.null)

            IF _EXPMAC (_rom.open <> %segnum%)

_rom.cb         QSET    _rom.b  [%segnum%]      ; Current base address
_rom.ce         QSET    _rom.e  [%segnum%]      ; Current end address
_rom.cp         QSET    _rom.p  [%segnum%]      ; Current page

_rom.cpos       QSET    _rom.pos[%segnum%]

                __rom_set_pc_addrs(_rom.cpos, _rom.cp)

_rom.open       QSET    %segnum%
.ROM.CurrentSeg QSET    _rom.open

            ELSE

_rom.pc         QSET    $

                ; If the segment is already open, just
                ; verify we're still within in range.
                __rom_check_segmem_range(_rom.pc, %segnum%)

            ENDI

        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_close_seg(segnum)                                                 ;;
;;  Closes an open ROM segment.  It also checks that the current program    ;;
;;  counter falls within the valid range of the open segment.  Nothing will ;;
;;  be done if "segnum" is _rom.invalid.  An error is raised if the given   ;;
;;  segment is not open.                                                    ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      segnum      The internal memory map segment to close.               ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_close_seg(segnum)
;
                __rom_reset_error

        IF ((%segnum%) <> _rom.invalid)

                __rom_validate_segnum(%segnum%)

          IF (_rom.error = _rom.null)

            IF (_rom.open <> %segnum%)
                IF (_rom.t[%segnum%] = _rom.dynamic)
                  __rom_raise_error("Dynamic ROM segment closure failed", ["Bank #", $#(_rom.bnk[%segnum%]), " is not opened."])
                ELSE
                  __rom_raise_error("ROM segment closure failed", ["Segment #", $#(_rom.seg[%segnum%]), " is not opened."])
                ENDI
            ELSE

_rom.pc             QSET $

              ; Ignore legacy segments
              IF (_rom.t[%segnum%] <> _rom.legacy)

                ; Close segment
                __rom_check_segmem_range(_rom.pc, %segnum%)

                ; Keep track of current segment position
_rom.pos[%segnum%]  QSET _rom.pc

                ; Compute usage statistics
                __rom_segmem_used(%segnum%)
                __rom_segmem_available(%segnum%)

              ENDI

_rom.open           QSET _rom.invalid                   ; Close segment %segnum%
.ROM.CurrentSeg     QSET _rom.open

            ENDI

          ENDI

        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_try_open_seg(segnum, min)                                         ;;
;;  Opens a given ROM segment if it has a minimum of "min" words available. ;;
;;                                                                          ;;
;;  NOTE:   If a ROM segment is currently opened, this macro will not do    ;;
;;          anything.  This lets us chain calls to __rom_try_open_seg() for ;;
;;          all available segments, in order to attempt to find one with    ;;
;;          sufficient capacity.                                            ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      segnum      The internal memory map segment to test and open.       ;;
;;      min         The minimum size required, in 16-bit words.             ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_try_open_seg(segnum, min)
;
        IF _EXPMAC ((_rom.open = _rom.invalid) AND (%segnum% < _rom.segcnt) AND ((_rom.pos[%segnum%] + (%min%)) < _rom.e[%segnum%]))
                __rom_open_seg(%segnum%)
        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_select_segment(seg)                                               ;;
;;  Relocates the program counter to a static ROM segment.  Also closes the ;;
;;  currently open segment, keeping track of its usage.                     ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      seg         The static ROM segment to open.                         ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_select_segment(seg)
;
        IF (_rom.error = _rom.null)
                __rom_validate_segment(%seg%)
        ENDI

        IF (_rom.error = _rom.null)

_rom.segnum     QSET    _rom.segidx[%seg%]
_rom.type       QSET    _rom.t[_rom.segnum]

            ; Fail if the segment is dynamic
            IF (_rom.type = _rom.dynamic)
                __rom_raise_error(["Cannot select ROM segment #", $#(%seg%), " without a bank"], "Segment is dynamic.")
            ENDI

            ; Open static segment
            IF (_rom.type = _rom.static)
                __rom_close_seg(_rom.open)
                __rom_open_seg(_rom.segnum)
            ENDI

        ENDI
ENDM

;; ======================================================================== ;;
;;  __rom_switch_mem_page(base, page)                                       ;;
;;  Switches a range of memory addresses to a target page.                  ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      base        The base address of the range to switch.                ;;
;;      page        The target page number.                                 ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   __rom_switch_mem_page(base, page)
;
_rom.b_addr     QSET    ((%base%) AND $F000)
_rom.b_src      QSET    (_rom.b_addr OR $0A50 OR (%page%))
_rom.b_trg      QSET    (_rom.b_addr OR $0FFF)

    LISTING "on"

                MVII    #_rom.b_src, R0                 ; \_ Switch bank: [$s000 - $sFFF] to page
                MVO     R0,     _rom.b_trg              ; /

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  __rom_stats_scale(val, scale)                                           ;;
;;  Returns value "val" scaled by "scale." The formula used for scaling is: ;;
;;                                                                          ;;
;;              return = ceil(val / scale)                                  ;;
;;                     = [((val * base) / scale) + (base - 1)] / base       ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      val         The value to scale.                                     ;;
;;      scale       The scale to apply.                                     ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      ceil(val / scale).                                                  ;;
;; ======================================================================== ;;
MACRO   __rom_stats_scale(val, scale)
    (((((%val%) * 10) / (%scale%)) + 9) / 10)
ENDM

;; ======================================================================== ;;
;;  __rom_stats_draw_line(style, len)                                       ;;
;;  Outputs a horizontal line, useful for displaying tabular information.   ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      style       The line style to draw.  Available values are:          ;;
;;                      single      Single (thin) line.                     ;;
;;                      double      Double (thick) line.                    ;;
;;                                                                          ;;
;;      len         The length of the line to draw, in characters.          ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      A horizontal line in the given style.                               ;;
;; ======================================================================== ;;
MACRO   __rom_stats_draw_line(style, len)
        _rom_stat.%style%[0, ((%len%) - 1)]
ENDM

;; ======================================================================== ;;
;;  __rom_stats_pad_left(str, len)                                          ;;
;;  Outputs a string in a field of "len" characters, justified to the right ;;
;;  and padded on the left with blank spaces.                               ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      str         The string to output.                                   ;;
;;      len         The length of the field, in characters.                 ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      The string, left-padded in the field.                               ;;
;; ======================================================================== ;;
MACRO   __rom_stats_pad_left(str, len)
        $(_rom_stat.space[0, ((%len%) - STRLEN(%str%) - 1)], %str%)
ENDM

;; ======================================================================== ;;
;;  __rom_stats_pad_right(str, len)                                         ;;
;;  Outputs a string in a field of "len" characters, justified to the left  ;;
;;  and padded on the right with blank spaces.                              ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      str         The string to output.                                   ;;
;;      len         The length of the field, in characters.                 ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      The string, right-padded in the field.                              ;;
;; ======================================================================== ;;
MACRO   __rom_stats_pad_right(str, len)
        $(%str%, _rom_stat.space[0, ((%len%) - STRLEN(%str%) - 1)])
ENDM

;; ======================================================================== ;;
;;  ROM.Setup map                                                           ;;
;;  Configures and initializes the memory map indicated by "map."           ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      map         The memory map number to initialize.                    ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   ROM.Setup map
;
    LISTING "code"

                __rom_reset_error

        ; Make sure the directive is called only once!
        IF ((CLASSIFY(_rom.init) = -10000))
                __rom_validate_map(%map%)
        ELSE
                __rom_raise_error(["ROM", ".Setup directive must be called only once per program."], _rom.null)
        ENDI

        IF (_rom.error = _rom.null)

_rom.init       QEQU    1
_rom.map        QSET    %map%
_rom.ecs_off    QSET    _rom.null

            IF (DEFINED intybasic_ecs)
_rom.ecs_req    QSET    intybasic_ecs
            ELSE
_rom.ecs_req    QSET    _rom.null
            ENDI

            IF (DEFINED intybasic_jlp)
_rom.jlp_req    QSET    intybasic_jlp
            ELSE
_rom.jlp_req    QSET    _rom.null
            ENDI

            ; ---------------------------------------------------------
            ; Initialize ROM segments for active memory map.
            ;
            ; NOTE: Define below the segments available for each memory
            ;       map supported.  When defining a map, the following
            ;       rules must be observed:
            ;
            ;         - Map #0 must always be the "legacy" map.
            ;         - All static segments in a map must be defined
            ;           before any dynamic ones.
            ;         - Segment numbers must start at zero.
            ;         - Segment numbers should be defined in order.
            ;         - There must not be gaps in segment numbers.
            ; ---------------------------------------------------------

            ; MAP #0: Legacy memory map wit no ROM management
            IF (_rom.map = 0)
                __rom_init_segmem(0, $5000, $FFFF, _rom.invalid, legacy)
            ENDI

            ; MAP #1: Original Mattel 16K static memory map
            IF (_rom.map = 1)
                __rom_init_segmem(0, $5000, $6FFF, _rom.invalid, static)
                __rom_init_segmem(1, $D000, $DFFF, _rom.invalid, static)
                __rom_init_segmem(2, $F000, $FFFF, _rom.invalid, static)
            ENDI

            ; MAP #2: JLP 42K static memory map
            IF (_rom.map = 2)
_rom.ecs_off    QSET    1

                __rom_init_segmem(0, $5000, $6FFF, _rom.invalid, static)
                __rom_init_segmem(1, $A000, $BFFF, _rom.invalid, static)
                __rom_init_segmem(2, $C040, $FFFF, _rom.invalid, static)

              IF (_rom.jlp_req <> _rom.null)
                __rom_init_segmem(3, $2000, $2FFF, $F,           static)
                __rom_init_segmem(4, $7000, $7FFF, $F,           static)
              ELSE
                __rom_init_segmem(3, $2100, $2FFF, _rom.invalid, static)
                __rom_init_segmem(4, $7100, $7FFF, _rom.invalid, static)
              ENDI

                __rom_init_segmem(5, $4810, $4FFF, _rom.invalid, static)
            ENDI

            ; MAP #3: Dynamic bank-switching 98K memory map - 4K banks
            IF (_rom.map = 3)
_rom.ecs_off    QSET    1

                __rom_init_segmem(0, $5000, $6FFF, _rom.invalid, static)
                __rom_init_segmem(1, $A000, $BFFF, _rom.invalid, static)
                __rom_init_segmem(2, $C040, $FFFF, _rom.invalid, static)
                __rom_init_segmem(3, $2000, $2FFF, $F,           static)
                __rom_init_segmem(4, $4810, $4FFF, _rom.invalid, static)

                __rom_init_segmem(5, $7000, $7FFF, $1,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $2,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $3,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $4,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $5,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $6,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $7,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $8,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $9,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $A,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $B,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $C,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $D,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $E,           dynamic)
                __rom_init_segmem(5, $7000, $7FFF, $F,           dynamic)
            ENDI

            ; MAP #4: Dynamic bank-switching 154K memory map - 8K banks
            IF (_rom.map = 4)
_rom.ecs_off    QSET    1

                __rom_init_segmem(0, $5000, $6FFF, _rom.invalid, static)
                __rom_init_segmem(1, $A000, $BFFF, _rom.invalid, static)
                __rom_init_segmem(2, $C040, $DFFF, _rom.invalid, static)
                __rom_init_segmem(3, $2000, $2FFF, $F,           static)
                __rom_init_segmem(4, $7000, $7FFF, $F,           static)
                __rom_init_segmem(5, $4810, $4FFF, _rom.invalid, static)

                __rom_init_segmem(6, $E000, $FFFF, $0,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $2,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $3,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $4,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $5,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $6,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $7,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $8,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $9,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $A,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $B,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $C,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $D,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $E,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $F,           dynamic)
            ENDI

            ; MAP #5: Dynamic bank-switching 254K memory map - 16K banks
            IF (_rom.map = 5)
_rom.ecs_off    QSET    1

                __rom_init_segmem(0, $5000, $6FFF, _rom.invalid, static)
                __rom_init_segmem(1, $A000, $BFFF, _rom.invalid, static)
                __rom_init_segmem(2, $2000, $2FFF, $F,           static)
                __rom_init_segmem(3, $7000, $7FFF, $F,           static)
                __rom_init_segmem(4, $4810, $4FFF, _rom.invalid, static)

                __rom_init_segmem(5, $C040, $FFFF, $0,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $2,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $3,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $4,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $5,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $6,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $7,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $8,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $9,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $A,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $B,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $C,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $D,           dynamic)
                __rom_init_segmem(5, $C040, $FFFF, $E,           dynamic)
            ENDI

            ; MAP #6: Dynamic bank-switching 256K map -- 2 dynamic segments
            IF (_rom.map = 6)
_rom.ecs_off    QSET    1

                __rom_init_segmem(0, $5000, $6FFF, _rom.invalid, static)
                __rom_init_segmem(1, $C040, $DFFF, _rom.invalid, static)

                __rom_init_segmem(2, $A000, $BFFF, $0,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $1,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $2,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $3,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $4,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $5,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $6,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $7,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $8,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $9,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $A,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $B,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $C,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $D,           dynamic)
                __rom_init_segmem(2, $A000, $BFFF, $E,           dynamic)

                __rom_init_segmem(3, $E000, $FFFF, $0,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $2,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $3,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $4,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $5,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $6,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $7,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $8,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $9,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $A,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $B,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $C,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $D,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $E,           dynamic)
                __rom_init_segmem(3, $E000, $FFFF, $F,           dynamic)
            ENDI


            ; MAP #7: Dynamic bank-switching 238K map -- 4 dynamic segments
            IF (_rom.map = 7)
_rom.ecs_off    QSET    1

                __rom_init_segmem(0, $5000, $6FFF, _rom.invalid, static)
                __rom_init_segmem(1, $2000, $2FFF, $F,           static)
                __rom_init_segmem(2, $4810, $4FFF, _rom.invalid, static)

                __rom_init_segmem(3, $7000, $7FFF, $1,           dynamic)
                __rom_init_segmem(3, $7000, $7FFF, $2,           dynamic)
                __rom_init_segmem(3, $7000, $7FFF, $3,           dynamic)
                __rom_init_segmem(3, $7000, $7FFF, $4,           dynamic)
                __rom_init_segmem(3, $7000, $7FFF, $5,           dynamic)
                __rom_init_segmem(3, $7000, $7FFF, $6,           dynamic)
                __rom_init_segmem(3, $7000, $7FFF, $7,           dynamic)
                __rom_init_segmem(3, $7000, $7FFF, $8,           dynamic)

                __rom_init_segmem(4, $A000, $BFFF, $0,           dynamic)
                __rom_init_segmem(4, $A000, $BFFF, $1,           dynamic)
                __rom_init_segmem(4, $A000, $BFFF, $2,           dynamic)
                __rom_init_segmem(4, $A000, $BFFF, $3,           dynamic)
                __rom_init_segmem(4, $A000, $BFFF, $4,           dynamic)
                __rom_init_segmem(4, $A000, $BFFF, $5,           dynamic)
                __rom_init_segmem(4, $A000, $BFFF, $6,           dynamic)
                __rom_init_segmem(4, $A000, $BFFF, $7,           dynamic)

                __rom_init_segmem(5, $C040, $DFFF, $0,           dynamic)
                __rom_init_segmem(5, $C040, $DFFF, $1,           dynamic)
                __rom_init_segmem(5, $C040, $DFFF, $2,           dynamic)
                __rom_init_segmem(5, $C040, $DFFF, $3,           dynamic)
                __rom_init_segmem(5, $C040, $DFFF, $4,           dynamic)
                __rom_init_segmem(5, $C040, $DFFF, $5,           dynamic)
                __rom_init_segmem(5, $C040, $DFFF, $6,           dynamic)
                __rom_init_segmem(5, $C040, $DFFF, $7,           dynamic)

                __rom_init_segmem(6, $E000, $FFFF, $0,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $2,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $3,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $4,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $5,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $6,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $7,           dynamic)
                __rom_init_segmem(6, $E000, $FFFF, $8,           dynamic)
            ENDI

        ENDI

        IF (_rom.error = _rom.null)

                ; Disable ECS in advanced maps and when ECS is used.
            IF ((_rom.ecs_off <> _rom.null) OR (_rom.ecs_req))
                __rom_set_pc_addrs($4800, _rom.invalid) ; Set up bootstrap hook

                __rom_switch_mem_page($2000, $F)        ; \
                __rom_switch_mem_page($7000, $F)        ;  > Switch off ECS ROMs
                __rom_switch_mem_page($E000, $F)        ; /

                B       $1041                           ; resume boot
            ENDI

                ; Initialize ROM base to segment #0
                ;   ($5000 - $6FFF in all maps)
                __rom_open_seg(0)

                ; ------------------------------------------------
                ; Configure the ROM header (Universal Data Block)
                ; ------------------------------------------------
                BIDECLE _ZERO           ; MOB picture base
                BIDECLE _ZERO           ; Process table
                BIDECLE _MAIN           ; Program start
                BIDECLE _ZERO           ; Background base image
                BIDECLE _ONES           ; GRAM
                BIDECLE _TITLE          ; Cartridge title and date
                DECLE   $03C0           ; No ECS title, jump to code after title,
                                        ; ... no clicks

_ZERO:          DECLE   $0000           ; Border control
                DECLE   $0000           ; 0 = color stack, 1 = f/b mode

_ONES:          DECLE   $0001, $0001    ; Initial color stack 0 and 1: Blue
                DECLE   $0001, $0001    ; Initial color stack 2 and 3: Blue
                DECLE   $0001           ; Initial border color: Blue

        ENDI

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  ROM.SelectDefaultSegment                                                ;;
;;  Relocates the program counter to the default ROM segment (#0).  Also    ;;
;;  closes the currently open segment, keeping track of its usage.          ;;
;;                                                                          ;;
;;  The macro will do nothing when the legacy map (#0) is selected.         ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      None.                                                               ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   ROM.SelectDefaultSegment
;
    LISTING "code"

                __rom_reset_error
                __rom_assert_setup("SelectSegment")

        IF (_rom.error = _rom.null)

            ; Ignore when the legacy map is selected
            IF (_rom.map > 0)
                __rom_select_segment(0)
            ENDI

        ENDI

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  ROM.SelectSegment seg                                                   ;;
;;  Relocates the program counter to a static ROM segment.  Also closes the ;;
;;  currently open segment, keeping track of its usage.                     ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      seg         The static ROM segment to open.                         ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   ROM.SelectSegment seg
;
    LISTING "code"

                __rom_reset_error
                __rom_assert_romseg_support("Segment selection")
                __rom_assert_setup("SelectSegment")

        IF (_rom.error = _rom.null)
                __rom_select_segment(%seg%)
        ENDI

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  ROM.SelectBank seg, bank                                                ;;
;;  Relocates the program counter to a dynamic ROM segment bank.  Also      ;;
;;  closes the currently open segment, keeping track of its usage.          ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      seg         The dynamic ROM segment, or -1 for the first one.       ;;
;;      bank        The segment bank number to open.                        ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   ROM.SelectBank seg, bank
;
    LISTING "code"

                __rom_reset_error
                __rom_assert_romseg_support("Dynamic segment bank selection")
                __rom_assert_setup("SelectBank")

        IF (_rom.error = _rom.null)

            ; Determine the logical segment number
            IF ((%seg%) = _rom.invalid)
_rom.num        QSET    .ROM.Segments[_rom.static]
            ELSE
_rom.num        QSET    %seg%
            ENDI

_rom.type       QSET    _rom.t[_rom.num]

            ; Fail if the segment is not dynamic
            IF (_rom.type <> _rom.dynamic)
                __rom_raise_error(["Cannot select bank on ROM segment #", $#(_rom.num)], "Segment is not dynamic.")
            ENDI

            ; Validate the segment and bank
            IF (_rom.error = _rom.null)
                __rom_validate_seg_bank(_rom.num, %bank%)
            ENDI

            IF (_rom.error = _rom.null)

_rom.segnum     QSET    (_rom.segidx[_rom.num] + (%bank%))


              ; Open dynamic segment bank
              IF (_rom.error = _rom.null)
                __rom_close_seg(_rom.open)
                __rom_open_seg(_rom.segnum)
              ENDI

            ENDI

        ENDI

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  ROM.AutoSelectSegment min                                               ;;
;;  Finds a static ROM segment with the specified minimum available         ;;
;;  capacity, and relocates the program counter to it.  Also closes the     ;;
;;  currently open segment, keeping track of its usage statistics.          ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      min         The minimum capacity required, in 16-bit words.         ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   ROM.AutoSelectSegment min
;
    LISTING "code"

                __rom_reset_error
                __rom_assert_romseg_support("Automatic segment selection")
                __rom_assert_setup("AutoSelectSegment")

        IF (_rom.error = _rom.null)
                __rom_close_seg(_rom.open)
        ENDI

        IF (_rom.error = _rom.null)

_rom.segnum     QSET    0

            REPEAT (.ROM.Segments[_rom.static])
                __rom_try_open_seg(_rom.segnum, %min%)

_rom.segnum     QSET    (_rom.segnum + 1)
            ENDR

            ; Fail if no segment was found with enough space
            IF (_rom.open = _rom.invalid)
                __rom_raise_error("Automatic ROM segment selection failed", ["Could not find a suitable segment with ", $#(%min%), " words available."])
            ENDI

        ENDI

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  ROM.SwitchBank seg, bank                                                ;;
;;  Switches a dynamic ROM segment to the requested bank.                   ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      seg         The dynamic ROM segment, or -1 for the first one.       ;;
;;      bank        The dynamic ROM segment bank number to activate.        ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   ROM.SwitchBank seg, bank
;
    LISTING "code"

                __rom_reset_error
                __rom_assert_romseg_support("Automatic segment selection")
                __rom_assert_setup("SwitchBank")

        IF (_rom.error = _rom.null)

            ; Determine the logical segment number
            IF ((%seg%) = _rom.invalid)
_rom.num        QSET    .ROM.Segments[_rom.static]
            ELSE
_rom.num        QSET    %seg%
            ENDI

_rom.type       QSET    _rom.t[_rom.num]

            ; Fail if the segment is not dynamic
            IF (_rom.type <> _rom.dynamic)
                __rom_raise_error(["Cannot switch bank on ROM segment #", $#(_rom.num)], "Segment is not dynamic.")
            ENDI

            ; Validate the segment and bank
            IF (_rom.error = _rom.null)
                __rom_validate_seg_bank(_rom.num, %bank%)
            ENDI

            IF (_rom.error = _rom.null)

_rom.segnum     QSET    (_rom.segidx[_rom.num] + (%bank%))

                ; Initialize REPEAT loop symbols
_rom.r_pgs      QSET    _rom.pgs[_rom.segnum]
_rom.r_addr     QSET    _rom.b  [_rom.segnum]
_rom.r_page     QSET    _rom.p  [_rom.segnum]

                ; Switch the physical pages that comprise
                ; the logical segment bank.
                REPEAT (_rom.r_pgs)
                    __rom_switch_mem_page(_rom.r_addr, _rom.r_page)
_rom.r_addr         QSET    (_rom.r_addr + _rom.pgsize)
                ENDR

            ENDI

        ENDI

    LISTING "prev"
ENDM


;; ======================================================================== ;;
;;  ROM.End                                                                 ;;
;;  Closes any open ROM segment, reports usage statistics, and finalizes    ;;
;;  the program.                                                            ;;
;;                                                                          ;;
;;  This macro must be called at the very end of the program.               ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      None.                                                               ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      _rom.error  -1 on failure.                                          ;;
;; ======================================================================== ;;
MACRO   ROM.End
;
    LISTING "code"

                __rom_reset_error
                __rom_assert_setup("End")

        IF (_rom.error = _rom.null)
                __rom_close_seg(_rom.open)

            ; The legacy map does not support usage statistics
            IF (_rom.map > 0)
                __rom_calculate_stats
            ENDI
        ENDI

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  ROM.OutputRomStats                                                      ;;
;;  Outputs ROM usage statistics to STDOUT and to the listing file.         ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      None.                                                               ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      ROM usage statistics.                                               ;;
;; ======================================================================== ;;
MACRO   ROM.OutputRomStats
;
    LISTING "code"

_rom.hdr_len    QSET    55
_rom.fld_ttl    QSET    15
_rom.fld_mem    QSET    7
_rom.fld_bnk    QSET    3
_rom.fld_siz    QSET    4
_rom.fld_avl    QSET    8
_rom.scale      QSET    1024

      IF (_rom.map > 0)

                ; Draw header
                SMSG ""
                SMSG $("ROM USAGE (MAP #", $#(_rom.map), "):")
                SMSG    $("    ", __rom_stats_draw_line(double, _rom.hdr_len))
                SMSG    $("    ", "    Segment        Size       Used      Available")
                SMSG    $("    ", __rom_stats_draw_line(double, _rom.hdr_len))

_rom.idx        QSET    0
_rom.cnt        QSET    .ROM.Segments[_rom.static]

        ; Static segments
        REPEAT (_rom.cnt)
_rom.segnum     QSET    _rom.segidx[_rom.idx]

_rom.size       QSET    .ROM.SegSize[_rom.segnum]
_rom.used       QSET    .ROM.SegUsed[_rom.segnum]
_rom.avlb       QSET    .ROM.SegAvlb[_rom.segnum]

_rom.size       QSET    __rom_stats_scale(_rom.size, _rom.scale)    ; Scaled to 1K

                ; Static ROM segment stats
                SMSG    $("    ", "Static Seg #", $#(_rom.idx), "     ", __rom_stats_pad_left($#(_rom.size), _rom.fld_siz), "K     ", __rom_stats_pad_left($#(_rom.used), _rom.fld_mem), "  ", __rom_stats_pad_left($#(_rom.avlb), _rom.fld_avl), " words")

_rom.idx        QSET    (_rom.idx + 1)
        ENDR

_rom.cnt        QSET    (_rom.segs - _rom.idx)

        ; Dynamic segments
        REPEAT (_rom.cnt)
_rom.segnum     QSET    _rom.segidx[_rom.idx]

                ; Dynamic ROM segment header
                SMSG    $("    ", __rom_stats_draw_line(single, _rom.hdr_len))
                SMSG    $("    ", "Dynamic Seg #", $#(_rom.idx), ":")

_rom.bidx       QSET    0
_rom.bcnt       QSET    _rom.bnkcnt[_rom.idx]

            ; Dynamic segment banks
            REPEAT (_rom.bcnt)

_rom.segnum     QSET    (_rom.segidx[_rom.idx] + _rom.bidx)

_rom.size       QSET    .ROM.SegSize[_rom.segnum]
_rom.used       QSET    .ROM.SegUsed[_rom.segnum]
_rom.avlb       QSET    .ROM.SegAvlb[_rom.segnum]

_rom.size       QSET    __rom_stats_scale(_rom.size, _rom.scale)    ; Scaled to 1K

                SMSG    $("    ", "       Bank #", __rom_stats_pad_right($#(_rom.bidx), _rom.fld_bnk), "  ", __rom_stats_pad_left($#(_rom.size), _rom.fld_siz), "K     ", __rom_stats_pad_left($#(_rom.used), _rom.fld_mem), "  ", __rom_stats_pad_left($#(_rom.avlb), _rom.fld_avl), " words")

_rom.bidx       QSET    (_rom.bidx + 1)
            ENDR

_rom.idx        QSET    (_rom.idx + 1)
        ENDR

_rom.size       QSET    __rom_stats_scale(.ROM.Size, _rom.scale)    ; Scaled to 1K

                ; Draw footer
                SMSG    $("    ", __rom_stats_draw_line(double, _rom.hdr_len))
                SMSG    $("    ", __rom_stats_pad_left("TOTAL:", _rom.fld_ttl), "   ", __rom_stats_pad_left($#(_rom.size), _rom.fld_siz), "K     ", __rom_stats_pad_left($#(.ROM.Used), _rom.fld_mem), "  ", __rom_stats_pad_left($#(.ROM.Available), _rom.fld_avl), " words")
                SMSG    $("    ", __rom_stats_draw_line(double, _rom.hdr_len))
                SMSG ""

      ENDI

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  EOF: romseg-bs.mac                                                      ;;
;; ======================================================================== ;;

	ROM.Setup intybasic_map

	; This macro will 'eat' SRCFILE directives if the assembler doesn't support the directive.
	IF ( DEFINED __FEATURE.SRCFILE ) = 0
	    MACRO SRCFILE x, y
	    ; macro must be non-empty, but a comment works fine.
	    ENDM
	ENDI

CLRSCR:	MVII #$200,R4		; Used also for CLS
	MVO R4,_screen		; Set up starting screen position for PRINT
	MVII #$F0,R1
FILLZERO:
	CLRR R0
MEMSET:
	SARC R1,2
	BNOV $+4
	MVO@ R0,R4
	MVO@ R0,R4
	BNC $+3
	MVO@ R0,R4
	BEQ $+7
	MVO@ R0,R4
	MVO@ R0,R4
	MVO@ R0,R4
	MVO@ R0,R4
	DECR R1
	BNE $-5
	JR R5

	;
	; Title, Intellivision EXEC will jump over it and start
	; execution directly in _MAIN
	;
	; Note mark is for automatic replacement by IntyBASIC
_TITLE:
        BYTE 114, 'IntyBASIC program', 0 ;IntyBASIC MARK DON'T CHANGE
        
	;
	; Main program
	;
_MAIN:
	DIS			; Disable interrupts
	MVII #STACK,R6

	;
	; Clean memory
	;
	CALL CLRSCR		; Clean up screen, right here to avoid brief
				; screen display of title in Sears Intellivision.
	MVII #$00e,R1		; 14 of sound (ECS)
	MVII #$0f0,R4		; ECS PSG
	CALL FILLZERO
	MVII #$0fe,R1		; 240 words of 8 bits plus 14 of sound
	MVII #$100,R4		; 8-bit scratch RAM
	CALL FILLZERO

	; Seed random generator using 16 bit RAM (not cleared by EXEC)
	CLRR R0
	MVII #$02F0,R4
	MVII #$0110/4,R1	; Includes phantom memory for extra randomness
_MAIN4:				; This loop is courtesy of GroovyBee
	ADD@ R4,R0
	ADD@ R4,R0
	ADD@ R4,R0
	ADD@ R4,R0
	DECR R1
	BNE _MAIN4
	MVO R0,_rand

	MVII #$058,R1		; 88 words of 16 bits
	MVII #$308,R4		; 16-bit scratch RAM
	CALL FILLZERO

	MVII #$058,R1		; 88 words of 16 bits
	MVII #$308,R4		; 16-bit scratch RAM
	CALL FILLZERO

    IF intybasic_jlp
	MVII #$1F40,R1		; Words of 16 bits
	MVII #$8040,R4		; 16-bit scratch RAM
	CALL FILLZERO
    ENDI
    IF intybasic_cc3
	MVII #$1F40,R1		; Words of 16 bits
	MVII #intybasic_cc3*256+$40,R4	; 16-bit scratch RAM
	CALL FILLZERO
    ENDI

	; PAL/NTSC detect
	CALL _set_isr
	DECLE _pal1
	EIS
	DECR PC			; This is a kind of HALT instruction

	; First interrupt may come at a weird time on Tutorvision, or
	; if other startup timing changes.
_pal1:	SUBI #8,R6		; Drop interrupt stack.
	CALL _set_isr
	DECLE _pal2
	DECR PC

	; Second interrupt is safe for initializing MOBs.
	; We will know the screen is off after this one fires.
_pal2:	SUBI #8,R6		; Drop interrupt stack.
	CALL _set_isr
	DECLE _pal3
	; clear MOBs
	CLRR R0
	CLRR R4
	MVII #$18,R2
_pal2_lp:
	MVO@ R0,R4
	DECR R2
	BNE _pal2_lp
	MVO R0,$30		; Reset horizontal delay register
	MVO R0,$31		; Reset vertical delay register

	MVII #-1100,R2		; PAL/NTSC threshold
_pal2_cnt:
	INCR R2
	B _pal2_cnt

	; The final count in R2 will either be negative or positive.
	; If R2 is still -ve, NTSC; else PAL.
_pal3:	SUBI #8,R6		; Drop interrupt stack.
	RLC R2,1
	RLC R2,1
	ANDI #1,R2		; 1 = NTSC, 0 = PAL

	MVII #$55,R1
	MVO R1,$4040
	MVII #$AA,R1
	MVO R1,$4041
	MVI $4040,R1
	CMPI #$55,R1
	BNE _ecs1
	MVI $4041,R1
	CMPI #$AA,R1
	BNE _ecs1
	ADDI #2,R2		; ECS detected flag
_ecs1:
	MVO R2,_ntsc

	CALL _set_isr
	DECLE _int_vector

	CALL CLRSCR		; Because _screen was reset to zero
	CALL _wait
	CALL _init_music
	MVII #2,R0		; Color Stack mode
	MVO R0,_mode_select
	MVII #$038,R0
	MVO R0,$01F8		; Configures sound
	MVO R0,$00F8		; Configures sound (ECS)
	CALL IV_INIT_and_wait	; Setup Intellivoice

;* ======================================================================== *;
;*  These routines are placed into the public domain by their author.  All  *;
;*  copyright rights are hereby relinquished on the routines and data in    *;
;*  this file.  -- James Pujals (DZ-Jay), 2014                              *;
;* ======================================================================== *;

; Modified by Oscar Toledo G. (nanochess), Aug/06/2015
; * Tested all multiplications with automated test.
; * Accelerated multiplication by 7,14,15,28,31,60,62,63,112,120,124
; * Solved bug in multiplication by 23,39,46,47,55,71,78,79,87,92,93,94,95,103,110,111,119
; * Improved sequence of instructions to be more interruptible.

;; ======================================================================== ;;
;;  MULT reg, tmp, const                                                    ;;
;;  Multiplies "reg" by constant "const" and using "tmp" for temporary      ;;
;;  calculations.  The result is placed in "reg."  The multiplication is    ;;
;;  performed by an optimal combination of shifts, additions, and           ;;
;;  subtractions.                                                           ;;
;;                                                                          ;;
;;  NOTE:   The resulting contents of the "tmp" are undefined.              ;;
;;                                                                          ;;
;;  ARGUMENTS                                                               ;;
;;      reg         A register containing the multiplicand.                 ;;
;;      tmp         A register for temporary calculations.                  ;;
;;      const       The constant multiplier.                                ;;
;;                                                                          ;;
;;  OUTPUT                                                                  ;;
;;      reg         Output value.                                           ;;
;;      tmp         Trashed.                                                ;;
;;      .ERR.Failed True if operation failed.                               ;;
;; ======================================================================== ;;
MACRO   MULT reg, tmp, const
;
    LISTING "code"

_mul.const      QSET    %const%
_mul.done       QSET    0

        IF (%const% > $7F)
_mul.const      QSET    (_mul.const SHR 1)
                SLL     %reg%,  1
        ENDI

        ; Multiply by $00 (0)
        IF (_mul.const = $00)
_mul.done       QSET    -1
                CLRR    %reg%
        ENDI

        ; Multiply by $01 (1)
        IF (_mul.const = $01)
_mul.done       QSET    -1
                ; Nothing to do
        ENDI

        ; Multiply by $02 (2)
        IF (_mul.const = $02)
_mul.done       QSET    -1
                SLL     %reg%,  1
        ENDI

        ; Multiply by $03 (3)
        IF (_mul.const = $03)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $04 (4)
        IF (_mul.const = $04)
_mul.done       QSET    -1
                SLL     %reg%,  2
        ENDI

        ; Multiply by $05 (5)
        IF (_mul.const = $05)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $06 (6)
        IF (_mul.const = $06)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $07 (7)
        IF (_mul.const = $07)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $08 (8)
        IF (_mul.const = $08)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  1
        ENDI

        ; Multiply by $09 (9)
        IF (_mul.const = $09)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $0A (10)
        IF (_mul.const = $0A)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $0B (11)
        IF (_mul.const = $0B)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $0C (12)
        IF (_mul.const = $0C)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $0D (13)
        IF (_mul.const = $0D)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $0E (14)
        IF (_mul.const = $0E)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $0F (15)
        IF (_mul.const = $0F)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $10 (16)
        IF (_mul.const = $10)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  2
        ENDI

        ; Multiply by $11 (17)
        IF (_mul.const = $11)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $12 (18)
        IF (_mul.const = $12)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $13 (19)
        IF (_mul.const = $13)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $14 (20)
        IF (_mul.const = $14)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $15 (21)
        IF (_mul.const = $15)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $16 (22)
        IF (_mul.const = $16)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $17 (23)
        IF (_mul.const = $17)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  1
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $18 (24)
        IF (_mul.const = $18)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $19 (25)
        IF (_mul.const = $19)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $1A (26)
        IF (_mul.const = $1A)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $1B (27)
        IF (_mul.const = $1B)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $1C (28)
        IF (_mul.const = $1C)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $1D (29)
        IF (_mul.const = $1D)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $1E (30)
        IF (_mul.const = $1E)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $1F (31)
        IF (_mul.const = $1F)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
		ADDR	%reg%,	%reg%
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $20 (32)
        IF (_mul.const = $20)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  2
		ADDR	%reg%,	%reg%
        ENDI

        ; Multiply by $21 (33)
        IF (_mul.const = $21)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
		ADDR	%reg%,	%reg%
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $22 (34)
        IF (_mul.const = $22)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $23 (35)
        IF (_mul.const = $23)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $24 (36)
        IF (_mul.const = $24)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $25 (37)
        IF (_mul.const = $25)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $26 (38)
        IF (_mul.const = $26)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $27 (39)
        IF (_mul.const = $27)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $28 (40)
        IF (_mul.const = $28)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $29 (41)
        IF (_mul.const = $29)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $2A (42)
        IF (_mul.const = $2A)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $2B (43)
        IF (_mul.const = $2B)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $2C (44)
        IF (_mul.const = $2C)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $2D (45)
        IF (_mul.const = $2D)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $2E (46)
        IF (_mul.const = $2E)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  1
		SUBR	%tmp%,  %reg%
        ENDI

        ; Multiply by $2F (47)
        IF (_mul.const = $2F)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  1
		SUBR	%tmp%,  %reg%
        ENDI

        ; Multiply by $30 (48)
        IF (_mul.const = $30)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $31 (49)
        IF (_mul.const = $31)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $32 (50)
        IF (_mul.const = $32)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $33 (51)
        IF (_mul.const = $33)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $34 (52)
        IF (_mul.const = $34)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $35 (53)
        IF (_mul.const = $35)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $36 (54)
        IF (_mul.const = $36)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $37 (55)
        IF (_mul.const = $37)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
		SLL	%reg%,	1
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $38 (56)
        IF (_mul.const = $38)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $39 (57)
        IF (_mul.const = $39)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $3A (58)
        IF (_mul.const = $3A)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $3B (59)
        IF (_mul.const = $3B)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $3C (60)
        IF (_mul.const = $3C)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $3D (61)
        IF (_mul.const = $3D)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $3E (62)
        IF (_mul.const = $3E)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
		ADDR	%reg%,	%reg%
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $3F (63)
        IF (_mul.const = $3F)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $40 (64)
        IF (_mul.const = $40)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  2
                SLL     %reg%,  2
        ENDI

        ; Multiply by $41 (65)
        IF (_mul.const = $41)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $42 (66)
        IF (_mul.const = $42)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
		ADDR	%reg%,	%reg%
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $43 (67)
        IF (_mul.const = $43)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
		ADDR	%reg%,	%reg%
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $44 (68)
        IF (_mul.const = $44)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $45 (69)
        IF (_mul.const = $45)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $46 (70)
        IF (_mul.const = $46)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $47 (71)
        IF (_mul.const = $47)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $48 (72)
        IF (_mul.const = $48)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $49 (73)
        IF (_mul.const = $49)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $4A (74)
        IF (_mul.const = $4A)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $4B (75)
        IF (_mul.const = $4B)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $4C (76)
        IF (_mul.const = $4C)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $4D (77)
        IF (_mul.const = $4D)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $4E (78)
        IF (_mul.const = $4E)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $4F (79)
        IF (_mul.const = $4F)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $50 (80)
        IF (_mul.const = $50)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $51 (81)
        IF (_mul.const = $51)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $52 (82)
        IF (_mul.const = $52)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $53 (83)
        IF (_mul.const = $53)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $54 (84)
        IF (_mul.const = $54)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $55 (85)
        IF (_mul.const = $55)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $56 (86)
        IF (_mul.const = $56)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $57 (87)
        IF (_mul.const = $57)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  1
		SUBR    %reg%,	%tmp%
                SLL     %reg%,  2
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $58 (88)
        IF (_mul.const = $58)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $59 (89)
        IF (_mul.const = $59)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $5A (90)
        IF (_mul.const = $5A)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $5B (91)
        IF (_mul.const = $5B)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $5C (92)
        IF (_mul.const = $5C)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  1
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $5D (93)
        IF (_mul.const = $5D)
_mul.done       QSET    -1
		MOVR	%reg%,	%tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  1
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $5E (94)
        IF (_mul.const = $5E)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  1
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $5F (95)
        IF (_mul.const = $5F)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                ADDR	%reg%,	%reg%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  1
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $60 (96)
        IF (_mul.const = $60)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  2
		ADDR	%reg%,	%reg%
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $61 (97)
        IF (_mul.const = $61)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $62 (98)
        IF (_mul.const = $62)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $63 (99)
        IF (_mul.const = $63)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $64 (100)
        IF (_mul.const = $64)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $65 (101)
        IF (_mul.const = $65)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $66 (102)
        IF (_mul.const = $66)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $67 (103)
        IF (_mul.const = $67)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  1
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $68 (104)
        IF (_mul.const = $68)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $69 (105)
        IF (_mul.const = $69)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $6A (106)
        IF (_mul.const = $6A)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $6B (107)
        IF (_mul.const = $6B)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $6C (108)
        IF (_mul.const = $6C)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $6D (109)
        IF (_mul.const = $6D)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $6E (110)
        IF (_mul.const = $6E)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $6F (111)
        IF (_mul.const = $6F)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
		SUBR	%tmp%,	%reg%
        ENDI

        ; Multiply by $70 (112)
        IF (_mul.const = $70)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $71 (113)
        IF (_mul.const = $71)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $72 (114)
        IF (_mul.const = $72)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $73 (115)
        IF (_mul.const = $73)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $74 (116)
        IF (_mul.const = $74)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $75 (117)
        IF (_mul.const = $75)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $76 (118)
        IF (_mul.const = $76)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $77 (119)
        IF (_mul.const = $77)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $78 (120)
        IF (_mul.const = $78)
_mul.done       QSET    -1
                SLL     %reg%,  2
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $79 (121)
        IF (_mul.const = $79)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $7A (122)
        IF (_mul.const = $7A)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $7B (123)
        IF (_mul.const = $7B)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  1
                ADDR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $7C (124)
        IF (_mul.const = $7C)
_mul.done       QSET    -1
                SLL     %reg%,  2
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
		ADDR	%reg%,	%reg%
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $7D (125)
        IF (_mul.const = $7D)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SUBR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
		ADDR	%reg%,	%reg%
                ADDR    %tmp%,  %reg%
        ENDI

        ; Multiply by $7E (126)
        IF (_mul.const = $7E)
_mul.done       QSET    -1
                SLL     %reg%,  1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SLL     %reg%,  2
                SUBR    %tmp%,  %reg%
        ENDI

        ; Multiply by $7F (127)
        IF (_mul.const = $7F)
_mul.done       QSET    -1
                MOVR    %reg%,  %tmp%
                SLL     %reg%,  2
                SLL     %reg%,  2
                SLL     %reg%,  2
                SLL     %reg%,  1
                SUBR    %tmp%,  %reg%
        ENDI

        IF  (_mul.done = 0)
            ERR $("Invalid multiplication constant \'%const%\', must be between 0 and ", $#($7F), ".")
        ENDI

    LISTING "prev"
ENDM

;; ======================================================================== ;;
;;  EOF: pm:mac:lang:mult                                                   ;;
;; ======================================================================== ;;

