	ROMW 16
intybasic_map:	equ 0	; ROM map used
intybasic_jlp:	equ 1	; JLP is used
intybasic_cc3:	equ 0	; CC3 is used and where is RAM
	IF DEFINED __FEATURE.CFGVAR
		CFGVAR "jlp" = 1
	ENDI
intybasic_ecs:	equ 0	; Forces to include ECS startup
intybasic_voice:	equ 0	; Forces to include voice library
intybasic_flash:	equ 0	; Forces to include Flash memory library
intybasic_scroll:	equ 0	; Forces to include scroll library
intybasic_col:	equ 1	; Forces to include collision detection
intybasic_keypad:	equ 0	; Forces to include keypad library
intybasic_music:	equ 1	; Forces to include music library
intybasic_music_ecs:	equ 0	; Forces to include music library
intybasic_music_volume:	equ 1	; Forces to include music volume change
intybasic_stack:	equ 0	; Forces to include stack overflow checking
intybasic_numbers:	equ 1	; Forces to include numbers library
intybasic_fastmult:	equ 1	; Forces to include fast multiplication
intybasic_fastdiv:	equ 0	; Forces to include fast division/remainder
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
	BYTE 124,'IntyBASIC program',0
        
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

	; IntyBASIC compiler v1.5.0 Dec/10/2024

	MVII #SYSTEM2,R5
	MVII #_SYSTEM2-SYSTEM2-1,R1
	CLRR R0
	MVO@ R0,R5
	DECR R1
	BPL $-2

	;FILE samples/pumpkin_master.bas
	;[1] 	'
	SRCFILE "samples/pumpkin_master.bas",1
	;[2] 	' Pumpkin master
	SRCFILE "samples/pumpkin_master.bas",2
	;[3] 	'
	SRCFILE "samples/pumpkin_master.bas",3
	;[4] 	' Demo for IntyBASIC
	SRCFILE "samples/pumpkin_master.bas",4
	;[5] 	'
	SRCFILE "samples/pumpkin_master.bas",5
	;[6] 	' by Oscar Toledo G. (nanochess)
	SRCFILE "samples/pumpkin_master.bas",6
	;[7] 	' http://nanochess.org
	SRCFILE "samples/pumpkin_master.bas",7
	;[8] 	'
	SRCFILE "samples/pumpkin_master.bas",8
	;[9] 	' Creation date: Oct/28/2018.
	SRCFILE "samples/pumpkin_master.bas",9
	;[10] 	' Revision date: Oct/29/2018. Added the pumpkin master and story.
	SRCFILE "samples/pumpkin_master.bas",10
	;[11] 	' Revision date: Oct/31/2018. Story can be accelerated pressing button.
	SRCFILE "samples/pumpkin_master.bas",11
	;[12] 	'                             Added 3 more waves.
	SRCFILE "samples/pumpkin_master.bas",12
	;[13] 	'
	SRCFILE "samples/pumpkin_master.bas",13
	;[14] 
	SRCFILE "samples/pumpkin_master.bas",14
	;[15] 	ON FRAME GOSUB play_sound
	SRCFILE "samples/pumpkin_master.bas",15
	;[16] 
	SRCFILE "samples/pumpkin_master.bas",16
	;[17] 	' Number of pumpkins at same time, maximum 6 because SPRITE 6 is bullet
	SRCFILE "samples/pumpkin_master.bas",17
	;[18] 	' and SPRITE 7 is player
	SRCFILE "samples/pumpkin_master.bas",18
	;[19] 	CONST PUMPKINS = 6
	SRCFILE "samples/pumpkin_master.bas",19
const_PUMPKINS:	EQU 6
	;[20] 
	SRCFILE "samples/pumpkin_master.bas",20
	;[21] 	CONST VOLUME_TITLE = 12	' Volume of music during title
	SRCFILE "samples/pumpkin_master.bas",21
const_VOLUME_TITLE:	EQU 12
	;[22] 	CONST VOLUME_GAME = 10		' Volume of music inside the game
	SRCFILE "samples/pumpkin_master.bas",22
const_VOLUME_GAME:	EQU 10
	;[23] 	CONST VOLUME_BOSS = 14		' Volume of music during boss
	SRCFILE "samples/pumpkin_master.bas",23
const_VOLUME_BOSS:	EQU 14
	;[24] 
	SRCFILE "samples/pumpkin_master.bas",24
	;[25] 	UNSIGNED #score, #record
	SRCFILE "samples/pumpkin_master.bas",25
	;[26] 
	SRCFILE "samples/pumpkin_master.bas",26
	;[27] 	DIM x(PUMPKINS)		' X-coordinate of pumpkin or boss bullet
	SRCFILE "samples/pumpkin_master.bas",27
	;[28] 	DIM y(PUMPKINS)		' Y-coordinate of pumpkin or boss bullet
	SRCFILE "samples/pumpkin_master.bas",28
	;[29] 	DIM s(PUMPKINS)		' State of pumpkin (in current wave)
	SRCFILE "samples/pumpkin_master.bas",29
	;[30] 	DIM z(PUMPKINS)		' Timing of pumpkin explosion
	SRCFILE "samples/pumpkin_master.bas",30
	;[31] 	DIM b(PUMPKINS)		' Pumpkin bullet
	SRCFILE "samples/pumpkin_master.bas",31
	;[32] 
	SRCFILE "samples/pumpkin_master.bas",32
	;[33] 	#record = 25		' Setup a default record
	SRCFILE "samples/pumpkin_master.bas",33
	MVII #25,R0
	MVO R0,var_&RECORD
	;[34] 
	SRCFILE "samples/pumpkin_master.bas",34
	;[35] 	FOR c = 0 TO 60
	SRCFILE "samples/pumpkin_master.bas",35
	CLRR R0
	MVO R0,var_C
T1:
	;[36] 		WAIT
	SRCFILE "samples/pumpkin_master.bas",36
	CALL _wait
	;[37] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",37
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #60,R0
	BLE T1
	;[38] 
	SRCFILE "samples/pumpkin_master.bas",38
	;[39] 	'
	SRCFILE "samples/pumpkin_master.bas",39
	;[40] 	' For the good guys :)
	SRCFILE "samples/pumpkin_master.bas",40
	;[41] 	'
	SRCFILE "samples/pumpkin_master.bas",41
	;[42] 	PRINT AT 61 COLOR 5,"For all my friends"
	SRCFILE "samples/pumpkin_master.bas",42
	MVII #573,R0
	MVO R0,_screen
	MVII #5,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #304,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #840,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #656,R0
	MVO@ R0,R4
	XORI #520,R0
	MVO@ R0,R4
	XORI #104,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #608,R0
	MVO@ R0,R4
	XORI #616,R0
	MVO@ R0,R4
	XORI #160,R0
	MVO@ R0,R4
	XORI #712,R0
	MVO@ R0,R4
	XORI #560,R0
	MVO@ R0,R4
	XORI #160,R0
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #96,R0
	MVO@ R0,R4
	XORI #88,R0
	MVO@ R0,R4
	XORI #80,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[43] 	PRINT AT 81 COLOR 5,"   at Atariage!"
	SRCFILE "samples/pumpkin_master.bas",43
	MVII #593,R0
	MVO R0,_screen
	MVII #5,R0
	MVO R0,_color
	MVI _screen,R4
	MVO@ R0,R4
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #520,R0
	MVO@ R0,R4
	XORI #168,R0
	MVO@ R0,R4
	XORI #672,R0
	MVO@ R0,R4
	XORI #264,R0
	MVO@ R0,R4
	XORI #936,R0
	MVO@ R0,R4
	XORI #168,R0
	MVO@ R0,R4
	XORI #152,R0
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #64,R0
	MVO@ R0,R4
	XORI #48,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #544,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[44] 	GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",44
	CALL label_WAIT_AND_CLEAN
	;[45] 
	SRCFILE "samples/pumpkin_master.bas",45
	;[46] 	PRINT AT 102 COLOR 5,"Is it already"
	SRCFILE "samples/pumpkin_master.bas",46
	MVII #614,R0
	MVO R0,_screen
	MVII #5,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #328,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #976,R0
	MVO@ R0,R4
	XORI #664,R0
	MVO@ R0,R4
	XORI #584,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #672,R0
	MVO@ R0,R4
	XORI #520,R0
	MVO@ R0,R4
	XORI #104,R0
	MVO@ R0,R4
	XORI #240,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #32,R0
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[47] 	PRINT AT 122 COLOR 2,"Halloween 2018?"
	SRCFILE "samples/pumpkin_master.bas",47
	MVII #634,R0
	MVO R0,_screen
	MVII #2,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #320,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #840,R0
	MVO@ R0,R4
	XORI #104,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #24,R0
	MVO@ R0,R4
	XORI #192,R0
	MVO@ R0,R4
	XORI #144,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #88,R0
	MVO@ R0,R4
	XORI #624,R0
	MVO@ R0,R4
	XORI #144,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[48] 	PRINT AT 216 COLOR 3,";)"
	SRCFILE "samples/pumpkin_master.bas",48
	MVII #728,R0
	MVO R0,_screen
	MVII #3,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #216,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #144,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[49] 	GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",49
	CALL label_WAIT_AND_CLEAN
	;[50] 
	SRCFILE "samples/pumpkin_master.bas",50
	;[51] 	'
	SRCFILE "samples/pumpkin_master.bas",51
	;[52] 	' Show my logo
	SRCFILE "samples/pumpkin_master.bas",52
	;[53] 	'
	SRCFILE "samples/pumpkin_master.bas",53
	;[54] 	CLS
	SRCFILE "samples/pumpkin_master.bas",54
	CALL CLRSCR
	;[55] 	MODE 1
	SRCFILE "samples/pumpkin_master.bas",55
	MVII #3,R0
	MVO R0,_mode_select
	;[56] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",56
	CALL _wait
	;[57] 
	SRCFILE "samples/pumpkin_master.bas",57
	;[58] 	PLAY SIMPLE
	SRCFILE "samples/pumpkin_master.bas",58
	MVII #3,R3
	MVO R3,_music_mode
	;[59] 
	SRCFILE "samples/pumpkin_master.bas",59
	;[60] 	'
	SRCFILE "samples/pumpkin_master.bas",60
	;[61] 	' Title screen
	SRCFILE "samples/pumpkin_master.bas",61
	;[62] 	'
	SRCFILE "samples/pumpkin_master.bas",62
	;[63] title_screen:
	SRCFILE "samples/pumpkin_master.bas",63
	; TITLE_SCREEN
label_TITLE_SCREEN:	;[64] 	PLAY music_game
	SRCFILE "samples/pumpkin_master.bas",64
	MVII #label_MUSIC_GAME,R0
	CALL _play_music
	;[65] 	PLAY VOLUME VOLUME_TITLE
	SRCFILE "samples/pumpkin_master.bas",65
	MVII #12,R0
	MVO R0,_music_vol
	;[66] 
	SRCFILE "samples/pumpkin_master.bas",66
	;[67] 	CLS
	SRCFILE "samples/pumpkin_master.bas",67
	CALL CLRSCR
	;[68] 	MODE 0,0,1,0,0
	SRCFILE "samples/pumpkin_master.bas",68
	MVII #256,R0
	MVO R0,_color
	MVII #2,R0
	MVO R0,_mode_select
	;[69] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",69
	CALL _wait
	;[70] 	FOR c = 0 TO 7
	SRCFILE "samples/pumpkin_master.bas",70
	CLRR R0
	MVO R0,var_C
T2:
	;[71] 		SPRITE c, 0
	SRCFILE "samples/pumpkin_master.bas",71
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	CLRR R0
	MVO@ R0,R4
	;[72] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",72
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #7,R0
	BLE T2
	;[73] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",73
	CALL _wait
	;[74] 	DEFINE 0,16,pumpkin_bitmaps_0
	SRCFILE "samples/pumpkin_master.bas",74
	CLRR R0
	MVO R0,_gram_target
	MVII #16,R0
	MVO R0,_gram_total
	MVII #label_PUMPKIN_BITMAPS_0,R0
	MVO R0,_gram_bitmap
	;[75] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",75
	CALL _wait
	;[76] 	DEFINE 16,16,pumpkin_bitmaps_1
	SRCFILE "samples/pumpkin_master.bas",76
	MVII #16,R0
	MVO R0,_gram_target
	MVO R0,_gram_total
	MVII #label_PUMPKIN_BITMAPS_1,R0
	MVO R0,_gram_bitmap
	;[77] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",77
	CALL _wait
	;[78] 	DEFINE 32,16,pumpkin_bitmaps_2
	SRCFILE "samples/pumpkin_master.bas",78
	MVII #32,R0
	MVO R0,_gram_target
	MVII #16,R0
	MVO R0,_gram_total
	MVII #label_PUMPKIN_BITMAPS_2,R0
	MVO R0,_gram_bitmap
	;[79] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",79
	CALL _wait
	;[80] 	DEFINE 48,11,pumpkin_bitmaps_3
	SRCFILE "samples/pumpkin_master.bas",80
	MVII #48,R0
	MVO R0,_gram_target
	MVII #11,R0
	MVO R0,_gram_total
	MVII #label_PUMPKIN_BITMAPS_3,R0
	MVO R0,_gram_bitmap
	;[81] 	DEFINE ALTERNATE 60,4,game_bitmaps_0
	SRCFILE "samples/pumpkin_master.bas",81
	MVII #60,R0
	MVO R0,_gram2_target
	MVII #4,R0
	MVO R0,_gram2_total
	MVII #label_GAME_BITMAPS_0,R0
	MVO R0,_gram2_bitmap
	;[82] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",82
	CALL _wait
	;[83] 	' Show the Pumpkin Master and the title letters
	SRCFILE "samples/pumpkin_master.bas",83
	;[84] 	SCREEN pumpkin_cards,20,0,20,11
	SRCFILE "samples/pumpkin_master.bas",84
	MVII #label_PUMPKIN_CARDS+20,R0
	PSHR R0
	MVII #512,R0
	PSHR R0
	MVII #20,R0
	PSHR R0
	MVII #11,R0
	PULR R1
	PULR R2
	PULR R3
	CALL CPYBLK
	;[85] 
	SRCFILE "samples/pumpkin_master.bas",85
	;[86] 	' Add some houses
	SRCFILE "samples/pumpkin_master.bas",86
	;[87] 	SPRITE 0,$0308 + 8, $0088 + 64, $1801 + 62 * 8
	SRCFILE "samples/pumpkin_master.bas",87
	MVII #784,R0
	MVO R0,_mobs
	MVII #200,R0
	MVO R0,_mobs+8
	MVII #6641,R0
	MVO R0,_mobs+16
	;[88] 	SPRITE 1,$0308 + 24, $0088 + 64, $1801 + 62 * 8
	SRCFILE "samples/pumpkin_master.bas",88
	MVII #800,R0
	MVO R0,_mobs+1
	MVII #200,R0
	MVO R0,_mobs+9
	MVII #6641,R0
	MVO R0,_mobs+17
	;[89] 	SPRITE 2,$0308 + 40, $0088 + 64, $1801 + 62 * 8
	SRCFILE "samples/pumpkin_master.bas",89
	MVII #816,R0
	MVO R0,_mobs+2
	MVII #200,R0
	MVO R0,_mobs+10
	MVII #6641,R0
	MVO R0,_mobs+18
	;[90] 	SPRITE 3,$0308 + 104, $0088 + 64, $1801 + 62 * 8
	SRCFILE "samples/pumpkin_master.bas",90
	MVII #880,R0
	MVO R0,_mobs+3
	MVII #200,R0
	MVO R0,_mobs+11
	MVII #6641,R0
	MVO R0,_mobs+19
	;[91] 	SPRITE 4,$0308 + 120, $0088 + 64, $1801 + 62 * 8
	SRCFILE "samples/pumpkin_master.bas",91
	MVII #896,R0
	MVO R0,_mobs+4
	MVII #200,R0
	MVO R0,_mobs+12
	MVII #6641,R0
	MVO R0,_mobs+20
	;[92] 	SPRITE 5,$0308 + 136, $0088 + 64, $1801 + 62 * 8
	SRCFILE "samples/pumpkin_master.bas",92
	MVII #912,R0
	MVO R0,_mobs+5
	MVII #200,R0
	MVO R0,_mobs+13
	MVII #6641,R0
	MVO R0,_mobs+21
	;[93] 
	SRCFILE "samples/pumpkin_master.bas",93
	;[94] 	' Add a pair of pumpkins
	SRCFILE "samples/pumpkin_master.bas",94
	;[95] 	SPRITE 6,$0308 + 64, $0088 + 8, $1802 + 60 * 8
	SRCFILE "samples/pumpkin_master.bas",95
	MVII #840,R0
	MVO R0,_mobs+6
	MVII #144,R0
	MVO R0,_mobs+14
	MVII #6626,R0
	MVO R0,_mobs+22
	;[96] 	SPRITE 7,$0308 + 92, $0088 + 22, $1802 + 60 * 8
	SRCFILE "samples/pumpkin_master.bas",96
	MVII #868,R0
	MVO R0,_mobs+7
	MVII #158,R0
	MVO R0,_mobs+15
	MVII #6626,R0
	MVO R0,_mobs+23
	;[97] 
	SRCFILE "samples/pumpkin_master.bas",97
	;[98] 	' Show the record in a color bar (as ground)
	SRCFILE "samples/pumpkin_master.bas",98
	;[99] 	#backtab(180) = $2000
	SRCFILE "samples/pumpkin_master.bas",99
	MVII #8192,R0
	MVO R0,Q2+180
	;[100] 	PRINT AT 183 COLOR 5,"Record: ",<5>#record,"0"
	SRCFILE "samples/pumpkin_master.bas",100
	MVII #695,R0
	MVO R0,_screen
	MVII #5,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #400,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #952,R0
	MVO@ R0,R4
	XORI #48,R0
	MVO@ R0,R4
	XORI #96,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #176,R0
	MVO@ R0,R4
	XORI #752,R0
	MVO@ R0,R4
	XORI #208,R0
	MVO@ R0,R4
	MVO R4,_screen
	MVI var_&RECORD,R0
	MVII #5,R2
	MVI _color,R3
	MVI _screen,R4
	CALL PRNUM16.z
	MVO R4,_screen
	MVI _screen,R4
	MVII #128,R0
	XOR _color,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[101] 	#backtab(200) = $2000
	SRCFILE "samples/pumpkin_master.bas",101
	MVII #8192,R0
	MVO R0,Q2+200
	;[102] 
	SRCFILE "samples/pumpkin_master.bas",102
	;[103] 	PRINT AT 224 COLOR 6,"Press button"
	SRCFILE "samples/pumpkin_master.bas",103
	MVII #736,R0
	MVO R0,_screen
	MVII #6,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #384,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #784,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #176,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #664,R0
	MVO@ R0,R4
	XORI #528,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[104] 
	SRCFILE "samples/pumpkin_master.bas",104
	;[105] 	' Wait for controller to be "free"
	SRCFILE "samples/pumpkin_master.bas",105
	;[106] 	DO
	SRCFILE "samples/pumpkin_master.bas",106
T3:
	;[107] 		WAIT
	SRCFILE "samples/pumpkin_master.bas",107
	CALL _wait
	;[108] 		c = CONT
	SRCFILE "samples/pumpkin_master.bas",108
	MVI 510,R0
	XOR 511,R0
	MVO R0,var_C
	;[109] 	LOOP WHILE c
	SRCFILE "samples/pumpkin_master.bas",109
	MVI var_C,R0
	TSTR R0
	BEQ T5
	B T3
T5:
	;[110] 
	SRCFILE "samples/pumpkin_master.bas",110
	;[111] 	' Now wait for controller press
	SRCFILE "samples/pumpkin_master.bas",111
	;[112] 	DO
	SRCFILE "samples/pumpkin_master.bas",112
T6:
	;[113] 		WAIT
	SRCFILE "samples/pumpkin_master.bas",113
	CALL _wait
	;[114] 		c = CONT
	SRCFILE "samples/pumpkin_master.bas",114
	MVI 510,R0
	XOR 511,R0
	MVO R0,var_C
	;[115] 	LOOP WHILE c = 0
	SRCFILE "samples/pumpkin_master.bas",115
	MVI var_C,R0
	TSTR R0
	BNE T8
	B T6
T8:
	;[116] 
	SRCFILE "samples/pumpkin_master.bas",116
	;[117] 	'
	SRCFILE "samples/pumpkin_master.bas",117
	;[118] 	' Prepare for starting game
	SRCFILE "samples/pumpkin_master.bas",118
	;[119] 	'
	SRCFILE "samples/pumpkin_master.bas",119
	;[120] 	CLS
	SRCFILE "samples/pumpkin_master.bas",120
	CALL CLRSCR
	;[121] 	MODE 0,0,0,0,0
	SRCFILE "samples/pumpkin_master.bas",121
	CLRR R0
	MVO R0,_color
	MVII #2,R0
	MVO R0,_mode_select
	;[122] 	FOR c = 0 TO 7
	SRCFILE "samples/pumpkin_master.bas",122
	CLRR R0
	MVO R0,var_C
T9:
	;[123] 		SPRITE c, 0
	SRCFILE "samples/pumpkin_master.bas",123
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	CLRR R0
	MVO@ R0,R4
	;[124] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",124
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #7,R0
	BLE T9
	;[125] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",125
	CALL _wait
	;[126] 	DEFINE 0,16,game_bitmaps_0
	SRCFILE "samples/pumpkin_master.bas",126
	CLRR R0
	MVO R0,_gram_target
	MVII #16,R0
	MVO R0,_gram_total
	MVII #label_GAME_BITMAPS_0,R0
	MVO R0,_gram_bitmap
	;[127] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",127
	CALL _wait
	;[128] 
	SRCFILE "samples/pumpkin_master.bas",128
	;[129] 	first_time_ever = 1	' In order to show story
	SRCFILE "samples/pumpkin_master.bas",129
	MVII #1,R0
	MVO R0,var_FIRST_TIME_EVER
	;[130] 	lives = 4		' Default lives
	SRCFILE "samples/pumpkin_master.bas",130
	MVII #4,R0
	MVO R0,var_LIVES
	;[131] 	level = 1		' Start level
	SRCFILE "samples/pumpkin_master.bas",131
	MVII #1,R0
	MVO R0,var_LEVEL
	;[132] 	sublevel = 0		' Sublevel (or wave number)
	SRCFILE "samples/pumpkin_master.bas",132
	CLRR R0
	MVO R0,var_SUBLEVEL
	;[133] 	#score = 0		' Reset score
	SRCFILE "samples/pumpkin_master.bas",133
	MVO R0,var_&SCORE
	;[134] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",134
	NOP
	MVO R0,var_C
T10:
	;[135] 		x(c) = 0
	SRCFILE "samples/pumpkin_master.bas",135
	CLRR R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[136] 		y(c) = 0
	SRCFILE "samples/pumpkin_master.bas",136
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[137] 		s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",137
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[138] 		b(c) = 0
	SRCFILE "samples/pumpkin_master.bas",138
	ADDI #(array_B-array_S) AND $FFFF,R3
	MVO@ R0,R3
	;[139] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",139
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T10
	;[140] 	GOSUB start_wave	' Start a pumpkins wave
	SRCFILE "samples/pumpkin_master.bas",140
	CALL label_START_WAVE
	;[141] 
	SRCFILE "samples/pumpkin_master.bas",141
	;[142] 	'
	SRCFILE "samples/pumpkin_master.bas",142
	;[143] 	' Restart game after losing a life
	SRCFILE "samples/pumpkin_master.bas",143
	;[144] 	'
	SRCFILE "samples/pumpkin_master.bas",144
	;[145] restart_game:
	SRCFILE "samples/pumpkin_master.bas",145
	; RESTART_GAME
label_RESTART_GAME:	;[146] 	PLAY VOLUME VOLUME_GAME
	SRCFILE "samples/pumpkin_master.bas",146
	MVII #10,R0
	MVO R0,_music_vol
	;[147] 	by = 0			' No player bullet
	SRCFILE "samples/pumpkin_master.bas",147
	CLRR R0
	MVO R0,var_BY
	;[148] 	px = 84			' Setup player at bottom center
	SRCFILE "samples/pumpkin_master.bas",148
	MVII #84,R0
	MVO R0,var_PX
	;[149] 	py = 96
	SRCFILE "samples/pumpkin_master.bas",149
	MVII #96,R0
	MVO R0,var_PY
	;[150] 
	SRCFILE "samples/pumpkin_master.bas",150
	;[151] 	GOSUB update_score
	SRCFILE "samples/pumpkin_master.bas",151
	CALL label_UPDATE_SCORE
	;[152] 	GOSUB update_lives
	SRCFILE "samples/pumpkin_master.bas",152
	CALL label_UPDATE_LIVES
	;[153] 	GOSUB update_level
	SRCFILE "samples/pumpkin_master.bas",153
	CALL label_UPDATE_LEVEL
	;[154] 
	SRCFILE "samples/pumpkin_master.bas",154
	;[155] 	' If restarting inside boss level, go to boss game loop
	SRCFILE "samples/pumpkin_master.bas",155
	;[156] 	IF sublevel = 10 THEN PLAY VOLUME VOLUME_BOSS: GOTO boss_loop
	SRCFILE "samples/pumpkin_master.bas",156
	MVI var_SUBLEVEL,R0
	CMPI #10,R0
	BNE T11
	MVII #14,R0
	MVO R0,_music_vol
	B label_BOSS_LOOP
T11:
	;[157] 
	SRCFILE "samples/pumpkin_master.bas",157
	;[158] 	' Is it the first time the game restarts? show story.
	SRCFILE "samples/pumpkin_master.bas",158
	;[159] 	IF first_time_ever = 1 THEN
	SRCFILE "samples/pumpkin_master.bas",159
	MVI var_FIRST_TIME_EVER,R0
	CMPI #1,R0
	BNE T12
	;[160] 		first_time_ever = 0
	SRCFILE "samples/pumpkin_master.bas",160
	CLRR R0
	MVO R0,var_FIRST_TIME_EVER
	;[161] 
	SRCFILE "samples/pumpkin_master.bas",161
	;[162] 		' Re-use pumpkin arrays for tiny houses
	SRCFILE "samples/pumpkin_master.bas",162
	;[163] 		FOR c = 0 TO 2
	SRCFILE "samples/pumpkin_master.bas",163
	MVO R0,var_C
T13:
	;[164] 			x(c) = px - 12 * (c + 1)
	SRCFILE "samples/pumpkin_master.bas",164
	MVI var_PX,R0
	MVI var_C,R1
	INCR R1
	MULT R1,R4,12
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[165] 		NEXT c
	SRCFILE "samples/pumpkin_master.bas",165
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #2,R0
	BLE T13
	;[166] 		FOR c = 3 TO 5
	SRCFILE "samples/pumpkin_master.bas",166
	MVII #3,R0
	MVO R0,var_C
T14:
	;[167] 			x(c) = px + 12 * (c - 2)
	SRCFILE "samples/pumpkin_master.bas",167
	MVI var_C,R0
	SUBI #2,R0
	MULT R0,R4,12
	ADD var_PX,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[168] 		NEXT c 
	SRCFILE "samples/pumpkin_master.bas",168
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T14
	;[169] 		SPRITE 7, $0300 + px, $0080 + py, $1801 + 2 * 8
	SRCFILE "samples/pumpkin_master.bas",169
	MVI var_PX,R0
	ADDI #768,R0
	MVO R0,_mobs+7
	MVI var_PY,R0
	ADDI #128,R0
	MVO R0,_mobs+15
	MVII #6161,R0
	MVO R0,_mobs+23
	;[170] 		FOR c = 0 TO 6
	SRCFILE "samples/pumpkin_master.bas",170
	CLRR R0
	MVO R0,var_C
T15:
	;[171] 			SPRITE c, $0300 + x(c), $0080 + py, $1801 + 2 * 8
	SRCFILE "samples/pumpkin_master.bas",171
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #768,R0
	MVO@ R0,R4
	MVI var_PY,R0
	ADDI #128,R0
	ADDI #7,R4
	MVO@ R0,R4
	MVII #6161,R0
	ADDI #7,R4
	MVO@ R0,R4
	;[172] 		NEXT c
	SRCFILE "samples/pumpkin_master.bas",172
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #6,R0
	BLE T15
	;[173] 
	SRCFILE "samples/pumpkin_master.bas",173
	;[174] 		PRINT AT 65 COLOR 6,"Hey guys!"
	SRCFILE "samples/pumpkin_master.bas",174
	MVII #577,R0
	MVO R0,_screen
	MVII #6,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #320,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #872,R0
	MVO@ R0,R4
	XORI #224,R0
	MVO@ R0,R4
	XORI #712,R0
	MVO@ R0,R4
	XORI #568,R0
	MVO@ R0,R4
	XORI #144,R0
	MVO@ R0,R4
	XORI #96,R0
	MVO@ R0,R4
	XORI #80,R0
	MVO@ R0,R4
	XORI #656,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[175] 		PRINT AT 85,"Thanks for"
	SRCFILE "samples/pumpkin_master.bas",175
	MVII #597,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #416,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #992,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #120,R0
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	XORI #192,R0
	MVO@ R0,R4
	XORI #664,R0
	MVO@ R0,R4
	XORI #560,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[176] 		PRINT AT 105,"helping me."
	SRCFILE "samples/pumpkin_master.bas",176
	MVII #617,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #576,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #104,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #224,R0
	MVO@ R0,R4
	XORI #200,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #568,R0
	MVO@ R0,R4
	XORI #616,R0
	MVO@ R0,R4
	XORI #64,R0
	MVO@ R0,R4
	XORI #600,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[177] 		GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",177
	CALL label_WAIT_AND_CLEAN
	;[178] 
	SRCFILE "samples/pumpkin_master.bas",178
	;[179] 		PRINT AT 64 COLOR 2,"Of course, we"
	SRCFILE "samples/pumpkin_master.bas",179
	MVII #576,R0
	MVO R0,_screen
	MVII #2,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #376,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #840,R0
	MVO@ R0,R4
	XORI #560,R0
	MVO@ R0,R4
	XORI #536,R0
	MVO@ R0,R4
	XORI #96,R0
	MVO@ R0,R4
	XORI #208,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	XORI #176,R0
	MVO@ R0,R4
	XORI #584,R0
	MVO@ R0,R4
	XORI #96,R0
	MVO@ R0,R4
	XORI #696,R0
	MVO@ R0,R4
	XORI #144,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[180] 		PRINT AT 84,"like to eat"
	SRCFILE "samples/pumpkin_master.bas",180
	MVII #596,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #608,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #112,R0
	MVO@ R0,R4
	XORI #552,R0
	MVO@ R0,R4
	XORI #672,R0
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #632,R0
	MVO@ R0,R4
	XORI #552,R0
	MVO@ R0,R4
	XORI #32,R0
	MVO@ R0,R4
	XORI #168,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[181] 		PRINT AT 104,"pumpkin."
	SRCFILE "samples/pumpkin_master.bas",181
	MVII #616,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #640,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	XORI #192,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #512,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[182] 		GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",182
	CALL label_WAIT_AND_CLEAN
	;[183] 
	SRCFILE "samples/pumpkin_master.bas",183
	;[184] 		PRINT AT 62 COLOR 6,"Wait, we aren't"
	SRCFILE "samples/pumpkin_master.bas",184
	MVII #574,R0
	MVO R0,_screen
	MVII #6,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #440,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #944,R0
	MVO@ R0,R4
	XORI #64,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #704,R0
	MVO@ R0,R4
	XORI #96,R0
	MVO@ R0,R4
	XORI #696,R0
	MVO@ R0,R4
	XORI #144,R0
	MVO@ R0,R4
	XORI #552,R0
	MVO@ R0,R4
	XORI #520,R0
	MVO@ R0,R4
	XORI #152,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #88,R0
	MVO@ R0,R4
	XORI #584,R0
	MVO@ R0,R4
	XORI #664,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[185] 		PRINT AT 82,"eating pumpkin"
	SRCFILE "samples/pumpkin_master.bas",185
	MVII #594,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #552,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #32,R0
	MVO@ R0,R4
	XORI #168,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #568,R0
	MVO@ R0,R4
	XORI #640,R0
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	XORI #192,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[186] 		PRINT AT 102,"but killing them."
	SRCFILE "samples/pumpkin_master.bas",186
	MVII #614,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #528,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	XORI #672,R0
	MVO@ R0,R4
	XORI #600,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #568,R0
	MVO@ R0,R4
	XORI #672,R0
	MVO@ R0,R4
	XORI #224,R0
	MVO@ R0,R4
	XORI #104,R0
	MVO@ R0,R4
	XORI #64,R0
	MVO@ R0,R4
	XORI #536,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[187] 		GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",187
	CALL label_WAIT_AND_CLEAN
	;[188] 
	SRCFILE "samples/pumpkin_master.bas",188
	;[189] 		PRINT AT 65 COLOR 2,"Killing?"
	SRCFILE "samples/pumpkin_master.bas",189
	MVII #577,R0
	MVO R0,_screen
	MVII #2,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #344,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #784,R0
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #704,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[190] 		GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",190
	CALL label_WAIT_AND_CLEAN
	;[191] 
	SRCFILE "samples/pumpkin_master.bas",191
	;[192] 		PRINT AT 61 COLOR 6,"These are witched"
	SRCFILE "samples/pumpkin_master.bas",192
	MVII #573,R0
	MVO R0,_screen
	MVII #6,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #416,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #992,R0
	MVO@ R0,R4
	XORI #104,R0
	MVO@ R0,R4
	XORI #176,R0
	MVO@ R0,R4
	XORI #176,R0
	MVO@ R0,R4
	XORI #552,R0
	MVO@ R0,R4
	XORI #520,R0
	MVO@ R0,R4
	XORI #152,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #552,R0
	MVO@ R0,R4
	XORI #696,R0
	MVO@ R0,R4
	XORI #240,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #88,R0
	MVO@ R0,R4
	XORI #104,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[193] 		PRINT AT 81,"pumpkins!"
	SRCFILE "samples/pumpkin_master.bas",193
	MVII #593,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #640,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	XORI #192,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #656,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[194] 		GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",194
	CALL label_WAIT_AND_CLEAN
	;[195] 
	SRCFILE "samples/pumpkin_master.bas",195
	;[196] 		PRINT AT 65 COLOR 2,"Witched?"
	SRCFILE "samples/pumpkin_master.bas",196
	MVII #577,R0
	MVO R0,_screen
	MVII #2,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #440,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #1008,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #88,R0
	MVO@ R0,R4
	XORI #104,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	XORI #728,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[197] 		GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",197
	CALL label_WAIT_AND_CLEAN
	;[198] 
	SRCFILE "samples/pumpkin_master.bas",198
	;[199] 		PRINT AT 63 COLOR 2,"Err... we have"
	SRCFILE "samples/pumpkin_master.bas",199
	MVII #575,R0
	MVO R0,_screen
	MVII #2,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #296,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #952,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #736,R0
	MVO@ R0,R4
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #112,R0
	MVO@ R0,R4
	XORI #696,R0
	MVO@ R0,R4
	XORI #144,R0
	MVO@ R0,R4
	XORI #552,R0
	MVO@ R0,R4
	XORI #576,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #152,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[200] 		PRINT AT 83,"things to do..."
	SRCFILE "samples/pumpkin_master.bas",200
	MVII #595,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #672,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #224,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #72,R0
	MVO@ R0,R4
	XORI #160,R0
	MVO@ R0,R4
	XORI #664,R0
	MVO@ R0,R4
	XORI #672,R0
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #632,R0
	MVO@ R0,R4
	XORI #544,R0
	MVO@ R0,R4
	XORI #88,R0
	MVO@ R0,R4
	XORI #520,R0
	MVO@ R0,R4
	MVO@ R0,R4
	MVO@ R0,R4
	NOP
	MVO R4,_screen
	;[201] 		PRINT AT 103,"See you later!"
	SRCFILE "samples/pumpkin_master.bas",201
	MVII #615,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #408,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #944,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #552,R0
	MVO@ R0,R4
	XORI #712,R0
	MVO@ R0,R4
	XORI #176,R0
	MVO@ R0,R4
	XORI #208,R0
	MVO@ R0,R4
	XORI #680,R0
	MVO@ R0,R4
	XORI #608,R0
	MVO@ R0,R4
	XORI #104,R0
	MVO@ R0,R4
	XORI #168,R0
	MVO@ R0,R4
	XORI #136,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #664,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[202] 
	SRCFILE "samples/pumpkin_master.bas",202
	;[203] 		' Tiny houses run to the sides
	SRCFILE "samples/pumpkin_master.bas",203
	;[204] 		FOR d = 0 TO 120
	SRCFILE "samples/pumpkin_master.bas",204
	CLRR R0
	MVO R0,var_D
T16:
	;[205] 			' Houses at left side
	SRCFILE "samples/pumpkin_master.bas",205
	;[206] 			FOR c = 0 TO 2
	SRCFILE "samples/pumpkin_master.bas",206
	CLRR R0
	MVO R0,var_C
T17:
	;[207] 				IF x(c) THEN x(c) = x(c) - 1
	SRCFILE "samples/pumpkin_master.bas",207
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	TSTR R0
	BEQ T18
	MVI@ R3,R0
	DECR R0
	MVO@ R0,R3
T18:
	;[208] 			NEXT c
	SRCFILE "samples/pumpkin_master.bas",208
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #2,R0
	BLE T17
	;[209] 			' House at right side
	SRCFILE "samples/pumpkin_master.bas",209
	;[210] 			FOR c = 3 TO 5
	SRCFILE "samples/pumpkin_master.bas",210
	MVII #3,R0
	MVO R0,var_C
T19:
	;[211] 				IF x(c) < 168 THEN x(c) = x(c) + 1
	SRCFILE "samples/pumpkin_master.bas",211
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	CMPI #168,R0
	BGE T20
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
T20:
	;[212] 			NEXT c
	SRCFILE "samples/pumpkin_master.bas",212
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T19
	;[213] 			' Update sprites
	SRCFILE "samples/pumpkin_master.bas",213
	;[214] 			FOR c = 0 TO 6
	SRCFILE "samples/pumpkin_master.bas",214
	CLRR R0
	MVO R0,var_C
T21:
	;[215] 				SPRITE c, $0300 + x(c), $0080 + py, $1801 + 2 * 8
	SRCFILE "samples/pumpkin_master.bas",215
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #768,R0
	MVO@ R0,R4
	MVI var_PY,R0
	ADDI #128,R0
	ADDI #7,R4
	MVO@ R0,R4
	MVII #6161,R0
	ADDI #7,R4
	MVO@ R0,R4
	;[216] 			NEXT c
	SRCFILE "samples/pumpkin_master.bas",216
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #6,R0
	BLE T21
	;[217] 			WAIT
	SRCFILE "samples/pumpkin_master.bas",217
	CALL _wait
	;[218] 		NEXT d
	SRCFILE "samples/pumpkin_master.bas",218
	MVI var_D,R0
	INCR R0
	MVO R0,var_D
	CMPI #120,R0
	BLE T16
	;[219] 		GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",219
	CALL label_WAIT_AND_CLEAN
	;[220] 
	SRCFILE "samples/pumpkin_master.bas",220
	;[221] 		PRINT AT 63 COLOR 6,"Cowards! I'll"
	SRCFILE "samples/pumpkin_master.bas",221
	MVII #575,R0
	MVO R0,_screen
	MVII #6,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #280,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #864,R0
	MVO@ R0,R4
	XORI #192,R0
	MVO@ R0,R4
	XORI #176,R0
	MVO@ R0,R4
	XORI #152,R0
	MVO@ R0,R4
	XORI #176,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #656,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	XORI #328,R0
	MVO@ R0,R4
	XORI #368,R0
	MVO@ R0,R4
	XORI #600,R0
	MVO@ R0,R4
	MVO@ R0,R4
	NOP
	MVO R4,_screen
	;[222] 		PRINT AT 83,"use my homebrew"
	SRCFILE "samples/pumpkin_master.bas",222
	MVII #595,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #680,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #48,R0
	MVO@ R0,R4
	XORI #176,R0
	MVO@ R0,R4
	XORI #552,R0
	MVO@ R0,R4
	XORI #616,R0
	MVO@ R0,R4
	XORI #160,R0
	MVO@ R0,R4
	XORI #712,R0
	MVO@ R0,R4
	XORI #576,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #64,R0
	MVO@ R0,R4
	XORI #56,R0
	MVO@ R0,R4
	XORI #128,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	XORI #144,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[223] 		PRINT AT 103,"proton cannon."
	SRCFILE "samples/pumpkin_master.bas",223
	MVII #615,R0
	MVO R0,_screen
	MOVR R0,R4
	MVII #640,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #232,R0
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #216,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	XORI #624,R0
	MVO@ R0,R4
	XORI #536,R0
	MVO@ R0,R4
	XORI #16,R0
	MVO@ R0,R4
	XORI #120,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	XORI #8,R0
	MVO@ R0,R4
	XORI #512,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[224] 		GOSUB wait_and_clean
	SRCFILE "samples/pumpkin_master.bas",224
	CALL label_WAIT_AND_CLEAN
	;[225] 
	SRCFILE "samples/pumpkin_master.bas",225
	;[226] 		' Clean pumpkins array
	SRCFILE "samples/pumpkin_master.bas",226
	;[227] 		FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",227
	CLRR R0
	MVO R0,var_C
T22:
	;[228] 			x(c) = 0
	SRCFILE "samples/pumpkin_master.bas",228
	CLRR R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[229] 			y(c) = 0
	SRCFILE "samples/pumpkin_master.bas",229
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[230] 			s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",230
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[231] 		NEXT c
	SRCFILE "samples/pumpkin_master.bas",231
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T22
	;[232] 	END IF
	SRCFILE "samples/pumpkin_master.bas",232
T12:
	;[233] 
	SRCFILE "samples/pumpkin_master.bas",233
	;[234] 	'
	SRCFILE "samples/pumpkin_master.bas",234
	;[235] 	' Game loop for pumpkins waves
	SRCFILE "samples/pumpkin_master.bas",235
	;[236] 	'
	SRCFILE "samples/pumpkin_master.bas",236
	;[237] game_loop:
	SRCFILE "samples/pumpkin_master.bas",237
	; GAME_LOOP
label_GAME_LOOP:	;[238] 
	SRCFILE "samples/pumpkin_master.bas",238
	;[239] 	'
	SRCFILE "samples/pumpkin_master.bas",239
	;[240] 	' Update pumpkins and drop bullets
	SRCFILE "samples/pumpkin_master.bas",240
	;[241] 	'
	SRCFILE "samples/pumpkin_master.bas",241
	;[242] 	IF drop_bullet THEN	' Not yet time for a bullet?
	SRCFILE "samples/pumpkin_master.bas",242
	MVI var_DROP_BULLET,R0
	TSTR R0
	BEQ T23
	;[243] 		drop_bullet = drop_bullet - 1
	SRCFILE "samples/pumpkin_master.bas",243
	DECR R0
	MVO R0,var_DROP_BULLET
	;[244] 		d = PUMPKINS
	SRCFILE "samples/pumpkin_master.bas",244
	MVII #6,R0
	MVO R0,var_D
	;[245] 	ELSE
	SRCFILE "samples/pumpkin_master.bas",245
	B T24
T23:
	;[246] 		c = level
	SRCFILE "samples/pumpkin_master.bas",246
	MVI var_LEVEL,R0
	MVO R0,var_C
	;[247] 		IF c > 10 THEN c = 10
	SRCFILE "samples/pumpkin_master.bas",247
	MVI var_C,R0
	CMPI #10,R0
	BLE T25
	MVII #10,R0
	MVO R0,var_C
T25:
	;[248] 		drop_bullet = 15 - c + RAND(16)	' Time for next bullet
	SRCFILE "samples/pumpkin_master.bas",248
	MVII #15,R0
	SUB var_C,R0
	MVI _rand,R1
	ANDI #15,R1
	ADDR R1,R0
	MVO R0,var_DROP_BULLET
	;[249] 		d = RAND(PUMPKINS)	' Choose a random pumpkin that will shot
	SRCFILE "samples/pumpkin_master.bas",249
	MVII #6,R0
	MVI _rand,R1
	CALL qs_mpy8
	SWAP R0
	MVO R0,var_D
	;[250] 	END IF
	SRCFILE "samples/pumpkin_master.bas",250
T24:
	;[251] 
	SRCFILE "samples/pumpkin_master.bas",251
	;[252] 	'
	SRCFILE "samples/pumpkin_master.bas",252
	;[253] 	' Check also if no pumpkins are shown (in order to trigger another wave)
	SRCFILE "samples/pumpkin_master.bas",253
	;[254] 	'
	SRCFILE "samples/pumpkin_master.bas",254
	;[255] 	valid = 0
	SRCFILE "samples/pumpkin_master.bas",255
	CLRR R0
	MVO R0,var_VALID
	;[256] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",256
	MVO R0,var_C
T26:
	;[257] 		IF y(c) THEN
	SRCFILE "samples/pumpkin_master.bas",257
	MVII #array_Y,R3
	ADD var_C,R3
	MVI@ R3,R0
	TSTR R0
	BEQ T27
	;[258] 			IF s(c) THEN
	SRCFILE "samples/pumpkin_master.bas",258
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVI@ R3,R0
	TSTR R0
	BEQ T28
	;[259] 				SPRITE c, $0300 + x(c), $0080 + y(c), $1802 + 0 * 8
	SRCFILE "samples/pumpkin_master.bas",259
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	ADDI #(array_X-array_S) AND $FFFF,R3
	MVI@ R3,R0
	ADDI #768,R0
	MVO@ R0,R4
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	ADDI #128,R0
	ADDI #7,R4
	MVO@ R0,R4
	MVII #6146,R0
	ADDI #7,R4
	MVO@ R0,R4
	;[260] 			ELSE
	SRCFILE "samples/pumpkin_master.bas",260
	B T29
T28:
	;[261] 				SPRITE c, $0300 + x(c), $0080 + y(c), $1802 + 6 * 8
	SRCFILE "samples/pumpkin_master.bas",261
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #768,R0
	MVO@ R0,R4
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	ADDI #128,R0
	ADDI #7,R4
	MVO@ R0,R4
	MVII #6194,R0
	ADDI #7,R4
	MVO@ R0,R4
	;[262] 				z(c) = z(c) - 1
	SRCFILE "samples/pumpkin_master.bas",262
	ADDI #(array_Z-array_Y) AND $FFFF,R3
	MVI@ R3,R0
	DECR R0
	MVO@ R0,R3
	;[263] 				IF z(c) = 0 THEN y(c) = 0
	SRCFILE "samples/pumpkin_master.bas",263
	MVI@ R3,R0
	TSTR R0
	BNE T30
	ADDI #(array_Y-array_Z) AND $FFFF,R3
	MVO@ R0,R3
T30:
	;[264] 			END IF
	SRCFILE "samples/pumpkin_master.bas",264
T29:
	;[265] 			valid = 1
	SRCFILE "samples/pumpkin_master.bas",265
	MVII #1,R0
	MVO R0,var_VALID
	;[266] 			' Shot if possible
	SRCFILE "samples/pumpkin_master.bas",266
	;[267] 			IF c = d THEN	' Pumpkin can shot
	SRCFILE "samples/pumpkin_master.bas",267
	MVI var_C,R0
	CMP var_D,R0
	BNE T31
	;[268] 				IF b(c) = 0 THEN	' Free space for shot
	SRCFILE "samples/pumpkin_master.bas",268
	MVII #array_B,R3
	ADDR R0,R3
	MVI@ R3,R0
	TSTR R0
	BNE T32
	;[269] 					IF x(c) > 7 THEN	' Pumpkin inside visible screen
	SRCFILE "samples/pumpkin_master.bas",269
	ADDI #(array_X-array_B) AND $FFFF,R3
	MVI@ R3,R0
	CMPI #7,R0
	BLE T33
	;[270] 						IF x(c) < 168 THEN
	SRCFILE "samples/pumpkin_master.bas",270
	MVI@ R3,R0
	CMPI #168,R0
	BGE T34
	;[271] 							b(c) = x(c) / 8 + (y(c) / 8) * 20 - 21
	SRCFILE "samples/pumpkin_master.bas",271
	MVI@ R3,R0
	SLR R0,2
	SLR R0,1
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R1
	SLR R1,2
	SLR R1,1
	MULT R1,R4,20
	ADDR R1,R0
	SUBI #21,R0
	ADDI #(array_B-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[272] 							IF sound_effect < 3 THEN sound_effect = 2: sound_state = 0
	SRCFILE "samples/pumpkin_master.bas",272
	MVI var_SOUND_EFFECT,R0
	CMPI #3,R0
	BGE T35
	MVII #2,R0
	MVO R0,var_SOUND_EFFECT
	CLRR R0
	MVO R0,var_SOUND_STATE
T35:
	;[273] 						END IF
	SRCFILE "samples/pumpkin_master.bas",273
T34:
	;[274] 					END IF
	SRCFILE "samples/pumpkin_master.bas",274
T33:
	;[275] 				END IF
	SRCFILE "samples/pumpkin_master.bas",275
T32:
	;[276] 			END IF
	SRCFILE "samples/pumpkin_master.bas",276
T31:
	;[277] 		ELSE
	SRCFILE "samples/pumpkin_master.bas",277
	B T36
T27:
	;[278] 			SPRITE c, 0
	SRCFILE "samples/pumpkin_master.bas",278
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	CLRR R0
	MVO@ R0,R4
	;[279] 		END IF
	SRCFILE "samples/pumpkin_master.bas",279
T36:
	;[280] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",280
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T26
	;[281] 
	SRCFILE "samples/pumpkin_master.bas",281
	;[282] 	'
	SRCFILE "samples/pumpkin_master.bas",282
	;[283] 	' Update player house and bullet
	SRCFILE "samples/pumpkin_master.bas",283
	;[284] 	'
	SRCFILE "samples/pumpkin_master.bas",284
	;[285] 	GOSUB update_player
	SRCFILE "samples/pumpkin_master.bas",285
	CALL label_UPDATE_PLAYER
	;[286] 	MODE 0,0,0,0,0:BORDER 0
	SRCFILE "samples/pumpkin_master.bas",286
	CLRR R0
	MVO R0,_color
	MVII #2,R0
	MVO R0,_mode_select
	CLRR R0
	MVO R0,_border_color
	;[287] 
	SRCFILE "samples/pumpkin_master.bas",287
	;[288] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",288
	CALL _wait
	;[289] 	' Check if player is touched by background (bullet)
	SRCFILE "samples/pumpkin_master.bas",289
	;[290] 	IF COL7 AND $0100 THEN	GOTO player_touched
	SRCFILE "samples/pumpkin_master.bas",290
	MVI _col7,R0
	ANDI #256,R0
	BNE label_PLAYER_TOUCHED
	;[291] 	' Check if player bullet touches pumpkin
	SRCFILE "samples/pumpkin_master.bas",291
	;[292] 	IF COL6 AND $003F THEN
	SRCFILE "samples/pumpkin_master.bas",292
	MVI _col6,R0
	ANDI #63,R0
	BEQ T38
	;[293] 		' Hardware saves us of tedious collision checking
	SRCFILE "samples/pumpkin_master.bas",293
	;[294] 		c = 255
	SRCFILE "samples/pumpkin_master.bas",294
	MVII #255,R0
	MVO R0,var_C
	;[295] 		IF COL6 AND $0001 THEN c = 0
	SRCFILE "samples/pumpkin_master.bas",295
	MVI _col6,R0
	ANDI #1,R0
	BEQ T39
	CLRR R0
	MVO R0,var_C
T39:
	;[296] 		IF COL6 AND $0002 THEN c = 1
	SRCFILE "samples/pumpkin_master.bas",296
	MVI _col6,R0
	ANDI #2,R0
	BEQ T40
	MVII #1,R0
	MVO R0,var_C
T40:
	;[297] 		IF COL6 AND $0004 THEN c = 2
	SRCFILE "samples/pumpkin_master.bas",297
	MVI _col6,R0
	ANDI #4,R0
	BEQ T41
	MVII #2,R0
	MVO R0,var_C
T41:
	;[298] 		IF COL6 AND $0008 THEN c = 3
	SRCFILE "samples/pumpkin_master.bas",298
	MVI _col6,R0
	ANDI #8,R0
	BEQ T42
	MVII #3,R0
	MVO R0,var_C
T42:
	;[299] 		IF COL6 AND $0010 THEN c = 4
	SRCFILE "samples/pumpkin_master.bas",299
	MVI _col6,R0
	ANDI #16,R0
	BEQ T43
	MVII #4,R0
	MVO R0,var_C
T43:
	;[300] 		IF COL6 AND $0020 THEN c = 5
	SRCFILE "samples/pumpkin_master.bas",300
	MVI _col6,R0
	ANDI #32,R0
	BEQ T44
	MVII #5,R0
	MVO R0,var_C
T44:
	;[301] 		IF c < 6 THEN	' Pumpkin touched?
	SRCFILE "samples/pumpkin_master.bas",301
	MVI var_C,R0
	CMPI #6,R0
	BGE T45
	;[302] 			IF y(c) THEN	' Pumpkin alive?
	SRCFILE "samples/pumpkin_master.bas",302
	MVII #array_Y,R3
	ADDR R0,R3
	MVI@ R3,R0
	TSTR R0
	BEQ T46
	;[303] 				IF s(c) THEN	' Pumpkin moving?
	SRCFILE "samples/pumpkin_master.bas",303
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVI@ R3,R0
	TSTR R0
	BEQ T47
	;[304] 					s(c) = 0	' Cease movement
	SRCFILE "samples/pumpkin_master.bas",304
	CLRR R0
	MVO@ R0,R3
	;[305] 					z(c) = 8	' Start explosion timing
	SRCFILE "samples/pumpkin_master.bas",305
	MVII #8,R0
	ADDI #(array_Z-array_S) AND $FFFF,R3
	MVO@ R0,R3
	;[306] 					by = 0
	SRCFILE "samples/pumpkin_master.bas",306
	CLRR R0
	MVO R0,var_BY
	;[307] 					#score = #score + 1
	SRCFILE "samples/pumpkin_master.bas",307
	MVI var_&SCORE,R0
	INCR R0
	MVO R0,var_&SCORE
	;[308] 					GOSUB update_score
	SRCFILE "samples/pumpkin_master.bas",308
	CALL label_UPDATE_SCORE
	;[309] 					sound_effect = 3: sound_state = 0
	SRCFILE "samples/pumpkin_master.bas",309
	MVII #3,R0
	MVO R0,var_SOUND_EFFECT
	CLRR R0
	MVO R0,var_SOUND_STATE
	;[310] 				END IF
	SRCFILE "samples/pumpkin_master.bas",310
T47:
	;[311] 			END IF
	SRCFILE "samples/pumpkin_master.bas",311
T46:
	;[312] 		END IF
	SRCFILE "samples/pumpkin_master.bas",312
T45:
	;[313] 	END IF
	SRCFILE "samples/pumpkin_master.bas",313
T38:
	;[314] 
	SRCFILE "samples/pumpkin_master.bas",314
	;[315] 	'
	SRCFILE "samples/pumpkin_master.bas",315
	;[316] 	' Move pumpkins bullets in 4px steps.
	SRCFILE "samples/pumpkin_master.bas",316
	;[317] 	'
	SRCFILE "samples/pumpkin_master.bas",317
	;[318] 	next_bullet = (next_bullet + 1) AND 3
	SRCFILE "samples/pumpkin_master.bas",318
	MVI var_NEXT_BULLET,R0
	INCR R0
	ANDI #3,R0
	MVO R0,var_NEXT_BULLET
	;[319] 	IF next_bullet = 0 THEN
	SRCFILE "samples/pumpkin_master.bas",319
	MVI var_NEXT_BULLET,R0
	TSTR R0
	BNE T48
	;[320] 		FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",320
	MVO R0,var_C
T49:
	;[321] 			d = b(c)
	SRCFILE "samples/pumpkin_master.bas",321
	MVII #array_B,R3
	ADD var_C,R3
	MVI@ R3,R0
	MVO R0,var_D
	;[322] 			IF d THEN
	SRCFILE "samples/pumpkin_master.bas",322
	MVI var_D,R0
	TSTR R0
	BEQ T50
	;[323] 				#backtab(d) = $0802 + 5 * 8
	SRCFILE "samples/pumpkin_master.bas",323
	MVII #2090,R0
	MVII #Q2,R3
	ADD var_D,R3
	MVO@ R0,R3
	;[324] 				b(c) = d
	SRCFILE "samples/pumpkin_master.bas",324
	MVI var_D,R0
	MVII #array_B,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[325] 			END IF
	SRCFILE "samples/pumpkin_master.bas",325
T50:
	;[326] 		NEXT c
	SRCFILE "samples/pumpkin_master.bas",326
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T49
	;[327] 	ELSEIF next_bullet = 2 THEN
	SRCFILE "samples/pumpkin_master.bas",327
	B T51
T48:
	MVI var_NEXT_BULLET,R0
	CMPI #2,R0
	BNE T52
	;[328] 		FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",328
	CLRR R0
	MVO R0,var_C
T53:
	;[329] 			d = b(c)
	SRCFILE "samples/pumpkin_master.bas",329
	MVII #array_B,R3
	ADD var_C,R3
	MVI@ R3,R0
	MVO R0,var_D
	;[330] 			IF d THEN
	SRCFILE "samples/pumpkin_master.bas",330
	MVI var_D,R0
	TSTR R0
	BEQ T54
	;[331] 				#backtab(d) = 0
	SRCFILE "samples/pumpkin_master.bas",331
	CLRR R0
	MVII #Q2,R3
	ADD var_D,R3
	MVO@ R0,R3
	;[332] 				IF d >= 220 THEN
	SRCFILE "samples/pumpkin_master.bas",332
	MVI var_D,R0
	CMPI #220,R0
	BLT T55
	;[333] 					d = 0
	SRCFILE "samples/pumpkin_master.bas",333
	CLRR R0
	MVO R0,var_D
	;[334] 				ELSE
	SRCFILE "samples/pumpkin_master.bas",334
	B T56
T55:
	;[335] 					d = d + 20
	SRCFILE "samples/pumpkin_master.bas",335
	MVI var_D,R0
	ADDI #20,R0
	MVO R0,var_D
	;[336] 					#backtab(d) = $0802 + 4 * 8
	SRCFILE "samples/pumpkin_master.bas",336
	MVII #2082,R0
	MVII #Q2,R3
	ADD var_D,R3
	MVO@ R0,R3
	;[337] 				END IF
	SRCFILE "samples/pumpkin_master.bas",337
T56:
	;[338] 				b(c) = d
	SRCFILE "samples/pumpkin_master.bas",338
	MVI var_D,R0
	MVII #array_B,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[339] 			END IF
	SRCFILE "samples/pumpkin_master.bas",339
T54:
	;[340] 		NEXT c
	SRCFILE "samples/pumpkin_master.bas",340
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T53
	;[341] 	END IF
	SRCFILE "samples/pumpkin_master.bas",341
T51:
T52:
	;[342] 
	SRCFILE "samples/pumpkin_master.bas",342
	;[343] 	'
	SRCFILE "samples/pumpkin_master.bas",343
	;[344] 	' Start a new wave of pumpkins, pumpkin boss or move pumpkins
	SRCFILE "samples/pumpkin_master.bas",344
	;[345] 	'
	SRCFILE "samples/pumpkin_master.bas",345
	;[346] 	IF next_wave THEN	' Waiting for next wave
	SRCFILE "samples/pumpkin_master.bas",346
	MVI var_NEXT_WAVE,R0
	TSTR R0
	BEQ T57
	;[347] 		next_wave = next_wave - 1
	SRCFILE "samples/pumpkin_master.bas",347
	DECR R0
	MVO R0,var_NEXT_WAVE
	;[348] 		IF next_wave = 0 THEN	' Start it?
	SRCFILE "samples/pumpkin_master.bas",348
	MVI var_NEXT_WAVE,R0
	TSTR R0
	BNE T58
	;[349] 			ON wave GOSUB start_0, start_1, start_2, start_3, start_4, start_5, start_6, start_7, start_8, start_9
	SRCFILE "samples/pumpkin_master.bas",349
	MVI var_WAVE,R1
	CMPI #10,R1
	BC T60
	MVII #T60,R5
	ADDI #T59,R1
	MVI@ R1,PC
T59:
	DECLE label_START_0
	DECLE label_START_1
	DECLE label_START_2
	DECLE label_START_3
	DECLE label_START_4
	DECLE label_START_5
	DECLE label_START_6
	DECLE label_START_7
	DECLE label_START_8
	DECLE label_START_9
T60:
	;[350] 		END IF
	SRCFILE "samples/pumpkin_master.bas",350
T58:
	;[351] 	ELSEIF valid = 0 THEN	' No pumpkins alive?
	SRCFILE "samples/pumpkin_master.bas",351
	B T61
T57:
	MVI var_VALID,R0
	TSTR R0
	BNE T62
	;[352] 		IF sublevel = 9 THEN	' Last wave?
	SRCFILE "samples/pumpkin_master.bas",352
	MVI var_SUBLEVEL,R0
	CMPI #9,R0
	BNE T63
	;[353] 
	SRCFILE "samples/pumpkin_master.bas",353
	;[354] 			'
	SRCFILE "samples/pumpkin_master.bas",354
	;[355] 			' Start boss wave
	SRCFILE "samples/pumpkin_master.bas",355
	;[356] 			'
	SRCFILE "samples/pumpkin_master.bas",356
	;[357] 			sublevel = 10
	SRCFILE "samples/pumpkin_master.bas",357
	MVII #10,R0
	MVO R0,var_SUBLEVEL
	;[358] 
	SRCFILE "samples/pumpkin_master.bas",358
	;[359] 			' Remove pumpkin bullets
	SRCFILE "samples/pumpkin_master.bas",359
	;[360] 			FOR c = 0 TO PUMPKINS - 1	
	SRCFILE "samples/pumpkin_master.bas",360
	CLRR R0
	MVO R0,var_C
T64:
	;[361] 				b(c) = 0
	SRCFILE "samples/pumpkin_master.bas",361
	CLRR R0
	MVII #array_B,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[362] 			NEXT c
	SRCFILE "samples/pumpkin_master.bas",362
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T64
	;[363] 
	SRCFILE "samples/pumpkin_master.bas",363
	;[364] 			' Bring in the Pumpkin Master and clean screen
	SRCFILE "samples/pumpkin_master.bas",364
	;[365] 			SCREEN pumpkin_cards, 60, 20, 20, 7, 20
	SRCFILE "samples/pumpkin_master.bas",365
	MVII #label_PUMPKIN_CARDS+60,R0
	PSHR R0
	MVII #532,R0
	PSHR R0
	MVII #20,R0
	PSHR R0
	MVII #7,R0
	PSHR R0
	MVII #20,R0
	CALL CPYBLK2
	;[366] 			FOR c = 160 TO 239
	SRCFILE "samples/pumpkin_master.bas",366
	MVII #160,R0
	MVO R0,var_C
T65:
	;[367] 				#backtab(c) = 0
	SRCFILE "samples/pumpkin_master.bas",367
	CLRR R0
	MVII #Q2,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[368] 			NEXT c
	SRCFILE "samples/pumpkin_master.bas",368
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #239,R0
	BLE T65
	;[369] 
	SRCFILE "samples/pumpkin_master.bas",369
	;[370] 			' Remove all pumpkins
	SRCFILE "samples/pumpkin_master.bas",370
	;[371] 			FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",371
	CLRR R0
	MVO R0,var_C
T66:
	;[372] 				y(c) = 0
	SRCFILE "samples/pumpkin_master.bas",372
	CLRR R0
	MVII #array_Y,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[373] 				s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",373
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[374] 				SPRITE c, 0
	SRCFILE "samples/pumpkin_master.bas",374
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	CLRR R0
	MVO@ R0,R4
	;[375] 			NEXT c
	SRCFILE "samples/pumpkin_master.bas",375
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T66
	;[376] 
	SRCFILE "samples/pumpkin_master.bas",376
	;[377] 			' Count of boss blocks
	SRCFILE "samples/pumpkin_master.bas",377
	;[378] 			blocks = 7 * 7 - 7
	SRCFILE "samples/pumpkin_master.bas",378
	MVII #42,R0
	MVO R0,var_BLOCKS
	;[379] 
	SRCFILE "samples/pumpkin_master.bas",379
	;[380] 			' Start boss music
	SRCFILE "samples/pumpkin_master.bas",380
	;[381] 			PLAY VOLUME VOLUME_BOSS
	SRCFILE "samples/pumpkin_master.bas",381
	MVII #14,R0
	MVO R0,_music_vol
	;[382] 			PLAY music_beat
	SRCFILE "samples/pumpkin_master.bas",382
	MVII #label_MUSIC_BEAT,R0
	CALL _play_music
	;[383] 
	SRCFILE "samples/pumpkin_master.bas",383
	;[384] 			' Go to boss game loop
	SRCFILE "samples/pumpkin_master.bas",384
	;[385] 			GOTO boss_loop
	SRCFILE "samples/pumpkin_master.bas",385
	B label_BOSS_LOOP
	;[386] 		ELSE
	SRCFILE "samples/pumpkin_master.bas",386
	B T67
T63:
	;[387] 			GOSUB start_wave
	SRCFILE "samples/pumpkin_master.bas",387
	CALL label_START_WAVE
	;[388] 		END IF
	SRCFILE "samples/pumpkin_master.bas",388
T67:
	;[389] 	ELSE
	SRCFILE "samples/pumpkin_master.bas",389
	B T61
T62:
	;[390] 		ON wave GOSUB move_0, move_1, move_2, move_3, move_4, move_5, move_6, move_7, move_8, move_9
	SRCFILE "samples/pumpkin_master.bas",390
	MVI var_WAVE,R1
	CMPI #10,R1
	BC T69
	MVII #T69,R5
	ADDI #T68,R1
	MVI@ R1,PC
T68:
	DECLE label_MOVE_0
	DECLE label_MOVE_1
	DECLE label_MOVE_2
	DECLE label_MOVE_3
	DECLE label_MOVE_4
	DECLE label_MOVE_5
	DECLE label_MOVE_6
	DECLE label_MOVE_7
	DECLE label_MOVE_8
	DECLE label_MOVE_9
T69:
	;[391] 	END IF
	SRCFILE "samples/pumpkin_master.bas",391
T61:
	;[392] 
	SRCFILE "samples/pumpkin_master.bas",392
	;[393] 	GOSUB move_player
	SRCFILE "samples/pumpkin_master.bas",393
	CALL label_MOVE_PLAYER
	;[394] 	GOTO game_loop
	SRCFILE "samples/pumpkin_master.bas",394
	B label_GAME_LOOP
	;[395] 
	SRCFILE "samples/pumpkin_master.bas",395
	;[396] 	'
	SRCFILE "samples/pumpkin_master.bas",396
	;[397] 	' Boss loop
	SRCFILE "samples/pumpkin_master.bas",397
	;[398] 	'
	SRCFILE "samples/pumpkin_master.bas",398
	;[399] boss_loop:
	SRCFILE "samples/pumpkin_master.bas",399
	; BOSS_LOOP
label_BOSS_LOOP:	;[400] 
	SRCFILE "samples/pumpkin_master.bas",400
	;[401] 	'
	SRCFILE "samples/pumpkin_master.bas",401
	;[402] 	' Show boss' bullets and make them to descend
	SRCFILE "samples/pumpkin_master.bas",402
	;[403] 	'
	SRCFILE "samples/pumpkin_master.bas",403
	;[404] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",404
	CLRR R0
	MVO R0,var_C
T70:
	;[405] 		IF y(c) THEN
	SRCFILE "samples/pumpkin_master.bas",405
	MVII #array_Y,R3
	ADD var_C,R3
	MVI@ R3,R0
	TSTR R0
	BEQ T71
	;[406] 			SPRITE c, $0300 + x(c), $0000 + y(c), $0802 + 4 * 8
	SRCFILE "samples/pumpkin_master.bas",406
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	ADDI #(array_X-array_Y) AND $FFFF,R3
	MVI@ R3,R0
	ADDI #768,R0
	MVO@ R0,R4
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	ADDI #7,R4
	MVO@ R0,R4
	MVII #2082,R0
	ADDI #7,R4
	MVO@ R0,R4
	;[407] 			y(c) = y(c) + 1
	SRCFILE "samples/pumpkin_master.bas",407
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
	;[408] 			IF y(c) >= 104 THEN y(c) = 0
	SRCFILE "samples/pumpkin_master.bas",408
	MVI@ R3,R0
	CMPI #104,R0
	BLT T72
	CLRR R0
	MVO@ R0,R3
T72:
	;[409] 		ELSE
	SRCFILE "samples/pumpkin_master.bas",409
	B T73
T71:
	;[410] 			SPRITE c, 0
	SRCFILE "samples/pumpkin_master.bas",410
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	CLRR R0
	MVO@ R0,R4
	;[411] 			x(c) = RANDOM(152) + 8
	SRCFILE "samples/pumpkin_master.bas",411
	MVII #152,R1
	CALL _next_random
	CALL qs_mpy8
	SWAP R0
	ANDI #255,R0
	ADDI #8,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[412] 			y(c) = 8 + RANDOM(16)
	SRCFILE "samples/pumpkin_master.bas",412
	CALL _next_random
	ANDI #15,R0
	ADDI #8,R0
	MVII #array_Y,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[413] 		END IF
	SRCFILE "samples/pumpkin_master.bas",413
T73:
	;[414] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",414
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T70
	;[415] 
	SRCFILE "samples/pumpkin_master.bas",415
	;[416] 	'
	SRCFILE "samples/pumpkin_master.bas",416
	;[417] 	' Update player house and bullet
	SRCFILE "samples/pumpkin_master.bas",417
	;[418] 	'
	SRCFILE "samples/pumpkin_master.bas",418
	;[419] 	GOSUB update_player
	SRCFILE "samples/pumpkin_master.bas",419
	CALL label_UPDATE_PLAYER
	;[420] 
	SRCFILE "samples/pumpkin_master.bas",420
	;[421] 	'
	SRCFILE "samples/pumpkin_master.bas",421
	;[422] 	' If boss hit then flash screen
	SRCFILE "samples/pumpkin_master.bas",422
	;[423] 	'
	SRCFILE "samples/pumpkin_master.bas",423
	;[424] 	IF gronk THEN MODE 0,7,7,7,7:BORDER 7:gronk = 0 ELSE MODE 0,0,0,0,0:BORDER 0
	SRCFILE "samples/pumpkin_master.bas",424
	MVI var_GRONK,R0
	TSTR R0
	BEQ T74
	MVII #30583,R0
	MVO R0,_color
	MVII #2,R0
	MVO R0,_mode_select
	MVII #7,R0
	MVO R0,_border_color
	CLRR R0
	MVO R0,var_GRONK
	B T75
T74:
	CLRR R0
	MVO R0,_color
	MVII #2,R0
	MVO R0,_mode_select
	CLRR R0
	MVO R0,_border_color
T75:
	;[425] 	WAIT
	SRCFILE "samples/pumpkin_master.bas",425
	CALL _wait
	;[426] 
	SRCFILE "samples/pumpkin_master.bas",426
	;[427] 	' Player touched by bullet
	SRCFILE "samples/pumpkin_master.bas",427
	;[428] 	IF COL7 AND $003f THEN	GOTO player_touched
	SRCFILE "samples/pumpkin_master.bas",428
	MVI _col7,R0
	ANDI #63,R0
	BNE label_PLAYER_TOUCHED
	;[429] 
	SRCFILE "samples/pumpkin_master.bas",429
	;[430] 	' If bullet is moving check if touches boss
	SRCFILE "samples/pumpkin_master.bas",430
	;[431] 	IF by THEN
	SRCFILE "samples/pumpkin_master.bas",431
	MVI var_BY,R0
	TSTR R0
	BEQ T77
	;[432] 		c = (bx + 1) / 8 + (by / 8) * 20 - 21
	SRCFILE "samples/pumpkin_master.bas",432
	MVI var_BX,R0
	INCR R0
	SLR R0,2
	SLR R0,1
	MVI var_BY,R1
	SLR R1,2
	SLR R1,1
	MULT R1,R4,20
	ADDR R1,R0
	SUBI #21,R0
	MVO R0,var_C
	;[433] 		IF c >= 20 AND #backtab(c) <> 0 THEN	' Crashes against it?
	SRCFILE "samples/pumpkin_master.bas",433
	MVI var_C,R0
	CMPI #20,R0
	MVII #65535,R0
	BGE T79
	INCR R0
T79:
	MVII #Q2,R3
	ADD var_C,R3
	MVI@ R3,R1
	TSTR R1
	MVII #65535,R1
	BNE T80
	INCR R1
T80:
	ANDR R1,R0
	BEQ T78
	;[434] 			#backtab(c) = 0		' Remove block
	SRCFILE "samples/pumpkin_master.bas",434
	CLRR R0
	MVII #Q2,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[435] 			gronk = 1		' Signal flash requested
	SRCFILE "samples/pumpkin_master.bas",435
	MVII #1,R0
	MVO R0,var_GRONK
	;[436] 			by = 0			' Remove bullet
	SRCFILE "samples/pumpkin_master.bas",436
	CLRR R0
	MVO R0,var_BY
	;[437] 			#score = #score + 2
	SRCFILE "samples/pumpkin_master.bas",437
	MVI var_&SCORE,R0
	ADDI #2,R0
	MVO R0,var_&SCORE
	;[438] 			sound_effect = 3: sound_state = 0
	SRCFILE "samples/pumpkin_master.bas",438
	MVII #3,R0
	MVO R0,var_SOUND_EFFECT
	CLRR R0
	MVO R0,var_SOUND_STATE
	;[439] 			blocks = blocks - 1	' One block less
	SRCFILE "samples/pumpkin_master.bas",439
	MVI var_BLOCKS,R0
	DECR R0
	MVO R0,var_BLOCKS
	;[440] 			IF blocks = 0 THEN	' All blocks completed?
	SRCFILE "samples/pumpkin_master.bas",440
	MVI var_BLOCKS,R0
	TSTR R0
	BNE T81
	;[441] 				#score = #score + 50	' Bonus
	SRCFILE "samples/pumpkin_master.bas",441
	MVI var_&SCORE,R0
	ADDI #50,R0
	MVO R0,var_&SCORE
	;[442] 			END IF
	SRCFILE "samples/pumpkin_master.bas",442
T81:
	;[443] 			GOSUB update_score
	SRCFILE "samples/pumpkin_master.bas",443
	CALL label_UPDATE_SCORE
	;[444] 			IF blocks = 0 THEN	' All blocks completed?
	SRCFILE "samples/pumpkin_master.bas",444
	MVI var_BLOCKS,R0
	TSTR R0
	BNE T82
	;[445] 				'
	SRCFILE "samples/pumpkin_master.bas",445
	;[446] 				' Return to pumpkins waves
	SRCFILE "samples/pumpkin_master.bas",446
	;[447] 				'
	SRCFILE "samples/pumpkin_master.bas",447
	;[448] 				sound_effect = 4:sound_state = 0
	SRCFILE "samples/pumpkin_master.bas",448
	MVII #4,R0
	MVO R0,var_SOUND_EFFECT
	CLRR R0
	MVO R0,var_SOUND_STATE
	;[449] 				PLAY VOLUME VOLUME_GAME
	SRCFILE "samples/pumpkin_master.bas",449
	MVII #10,R0
	MVO R0,_music_vol
	;[450] 				PLAY music_game
	SRCFILE "samples/pumpkin_master.bas",450
	MVII #label_MUSIC_GAME,R0
	CALL _play_music
	;[451] 				FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",451
	CLRR R0
	MVO R0,var_C
T83:
	;[452] 					y(c) = 0
	SRCFILE "samples/pumpkin_master.bas",452
	CLRR R0
	MVII #array_Y,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[453] 					s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",453
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[454] 					SPRITE c, 0
	SRCFILE "samples/pumpkin_master.bas",454
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	CLRR R0
	MVO@ R0,R4
	;[455] 				NEXT c
	SRCFILE "samples/pumpkin_master.bas",455
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T83
	;[456] 				GOSUB start_wave
	SRCFILE "samples/pumpkin_master.bas",456
	CALL label_START_WAVE
	;[457] 				GOTO game_loop
	SRCFILE "samples/pumpkin_master.bas",457
	B label_GAME_LOOP
	;[458] 			END IF
	SRCFILE "samples/pumpkin_master.bas",458
T82:
	;[459] 		END IF
	SRCFILE "samples/pumpkin_master.bas",459
T78:
	;[460] 	END IF
	SRCFILE "samples/pumpkin_master.bas",460
T77:
	;[461] 
	SRCFILE "samples/pumpkin_master.bas",461
	;[462] 	GOSUB move_player
	SRCFILE "samples/pumpkin_master.bas",462
	CALL label_MOVE_PLAYER
	;[463] 	GOTO boss_loop
	SRCFILE "samples/pumpkin_master.bas",463
	B label_BOSS_LOOP
	;[464] 
	SRCFILE "samples/pumpkin_master.bas",464
	;[465] 	'
	SRCFILE "samples/pumpkin_master.bas",465
	;[466] 	' Player explosion
	SRCFILE "samples/pumpkin_master.bas",466
	;[467] 	'
	SRCFILE "samples/pumpkin_master.bas",467
	;[468] player_touched:
	SRCFILE "samples/pumpkin_master.bas",468
	; PLAYER_TOUCHED
label_PLAYER_TOUCHED:	;[469] 	PLAY VOLUME 0	
	SRCFILE "samples/pumpkin_master.bas",469
	CLRR R0
	MVO R0,_music_vol
	;[470] 	sound_effect = 4: sound_state = 0
	SRCFILE "samples/pumpkin_master.bas",470
	MVII #4,R0
	MVO R0,var_SOUND_EFFECT
	CLRR R0
	MVO R0,var_SOUND_STATE
	;[471] 
	SRCFILE "samples/pumpkin_master.bas",471
	;[472] 	'
	SRCFILE "samples/pumpkin_master.bas",472
	;[473] 	' Screen cleaning changes if it's pumpkins or boss wave.
	SRCFILE "samples/pumpkin_master.bas",473
	;[474] 	'
	SRCFILE "samples/pumpkin_master.bas",474
	;[475] 	IF sublevel = 10 THEN
	SRCFILE "samples/pumpkin_master.bas",475
	MVI var_SUBLEVEL,R0
	CMPI #10,R0
	BNE T84
	;[476] 		FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",476
	CLRR R0
	MVO R0,var_C
T85:
	;[477] 			SPRITE c, 0
	SRCFILE "samples/pumpkin_master.bas",477
	MVII #Q1,R0
	ADD var_C,R0
	MOVR R0,R4
	CLRR R0
	MVO@ R0,R4
	;[478] 			y(c) = 0
	SRCFILE "samples/pumpkin_master.bas",478
	MVII #array_Y,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[479] 			s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",479
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[480] 		NEXT c
	SRCFILE "samples/pumpkin_master.bas",480
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T85
	;[481] 	ELSE
	SRCFILE "samples/pumpkin_master.bas",481
	B T86
T84:
	;[482] 		FOR c = 20 TO 239
	SRCFILE "samples/pumpkin_master.bas",482
	MVII #20,R0
	MVO R0,var_C
T87:
	;[483] 			#backtab(c) = 0
	SRCFILE "samples/pumpkin_master.bas",483
	CLRR R0
	MVII #Q2,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[484] 		NEXT c
	SRCFILE "samples/pumpkin_master.bas",484
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #239,R0
	BLE T87
	;[485] 		FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",485
	CLRR R0
	MVO R0,var_C
T88:
	;[486] 			b(c) = 0
	SRCFILE "samples/pumpkin_master.bas",486
	CLRR R0
	MVII #array_B,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[487] 		NEXT c
	SRCFILE "samples/pumpkin_master.bas",487
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T88
	;[488] 	END IF
	SRCFILE "samples/pumpkin_master.bas",488
T86:
	;[489] 
	SRCFILE "samples/pumpkin_master.bas",489
	;[490] 	'
	SRCFILE "samples/pumpkin_master.bas",490
	;[491] 	' Player nuclear explosion
	SRCFILE "samples/pumpkin_master.bas",491
	;[492] 	' (not really, the pumpkin bullet hit the freezer and
	SRCFILE "samples/pumpkin_master.bas",492
	;[493] 	' it was filled with beer :P)
	SRCFILE "samples/pumpkin_master.bas",493
	;[494] 	'
	SRCFILE "samples/pumpkin_master.bas",494
	;[495] 	SPRITE 6,0
	SRCFILE "samples/pumpkin_master.bas",495
	CLRR R0
	MVO R0,_mobs+6
	;[496] 	FOR c = 0 TO 127
	SRCFILE "samples/pumpkin_master.bas",496
	MVO R0,var_C
T89:
	;[497] 		IF c = 0 THEN
	SRCFILE "samples/pumpkin_master.bas",497
	MVI var_C,R0
	TSTR R0
	BNE T90
	;[498] 			SPRITE 7, $0300 + px, $0080 + py
	SRCFILE "samples/pumpkin_master.bas",498
	MVI var_PX,R0
	ADDI #768,R0
	MVO R0,_mobs+7
	MVI var_PY,R0
	ADDI #128,R0
	MVO R0,_mobs+15
	;[499] 		END IF
	SRCFILE "samples/pumpkin_master.bas",499
T90:
	;[500] 		IF c = 32 THEN
	SRCFILE "samples/pumpkin_master.bas",500
	MVI var_C,R0
	CMPI #32,R0
	BNE T91
	;[501] 			SPRITE 7, $0300 + px, $0180 + py - 8
	SRCFILE "samples/pumpkin_master.bas",501
	MVI var_PX,R0
	ADDI #768,R0
	MVO R0,_mobs+7
	MVI var_PY,R0
	ADDI #376,R0
	MVO R0,_mobs+15
	;[502] 		END IF
	SRCFILE "samples/pumpkin_master.bas",502
T91:
	;[503] 		IF c = 64 THEN
	SRCFILE "samples/pumpkin_master.bas",503
	MVI var_C,R0
	CMPI #64,R0
	BNE T92
	;[504] 			SPRITE 7, $0700 + px - 4, $0180 + py - 8
	SRCFILE "samples/pumpkin_master.bas",504
	MVI var_PX,R0
	ADDI #1788,R0
	MVO R0,_mobs+7
	MVI var_PY,R0
	ADDI #376,R0
	MVO R0,_mobs+15
	;[505] 		END IF
	SRCFILE "samples/pumpkin_master.bas",505
T92:
	;[506] 		IF c = 96 THEN
	SRCFILE "samples/pumpkin_master.bas",506
	MVI var_C,R0
	CMPI #96,R0
	BNE T93
	;[507] 			SPRITE 7, $0700 + px - 4, $0280 + py - 24
	SRCFILE "samples/pumpkin_master.bas",507
	MVI var_PX,R0
	ADDI #1788,R0
	MVO R0,_mobs+7
	MVI var_PY,R0
	ADDI #616,R0
	MVO R0,_mobs+15
	;[508] 		END IF
	SRCFILE "samples/pumpkin_master.bas",508
T93:
	;[509] 		IF C AND 2 THEN
	SRCFILE "samples/pumpkin_master.bas",509
	MVI var_C,R0
	ANDI #2,R0
	BEQ T94
	;[510] 			SPRITE 7,,,$0807 + 10 * 8
	SRCFILE "samples/pumpkin_master.bas",510
	MVII #2135,R0
	MVO R0,_mobs+23
	;[511] 		ELSE
	SRCFILE "samples/pumpkin_master.bas",511
	B T95
T94:
	;[512] 			SPRITE 7,,,$0807 + 8 * 8
	SRCFILE "samples/pumpkin_master.bas",512
	MVII #2119,R0
	MVO R0,_mobs+23
	;[513] 		END IF
	SRCFILE "samples/pumpkin_master.bas",513
T95:
	;[514] 		d = c AND 15
	SRCFILE "samples/pumpkin_master.bas",514
	MVI var_C,R0
	ANDI #15,R0
	MVO R0,var_D
	;[515] 		MODE 0,d,d,d,d
	SRCFILE "samples/pumpkin_master.bas",515
	MVI var_D,R0
	SWAP R0
	ANDI #65280,R0
	ADD var_D,R0
	MVI var_D,R1
	SLL R1,2
	SLL R1,2
	SWAP R1
	ANDI #61440,R1
	ADDR R1,R0
	MVI var_D,R1
	SLL R1,2
	SLL R1,2
	ADDR R1,R0
	MVO R0,_color
	MVII #2,R0
	MVO R0,_mode_select
	;[516] 		BORDER d
	SRCFILE "samples/pumpkin_master.bas",516
	MVI var_D,R0
	MVO R0,_border_color
	;[517] 		WAIT
	SRCFILE "samples/pumpkin_master.bas",517
	CALL _wait
	;[518] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",518
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #127,R0
	BLE T89
	;[519] 	SPRITE 7,0
	SRCFILE "samples/pumpkin_master.bas",519
	CLRR R0
	MVO R0,_mobs+7
	;[520] 	MODE 0,0,0,0,0
	SRCFILE "samples/pumpkin_master.bas",520
	MVO R0,_color
	MVII #2,R0
	MVO R0,_mode_select
	;[521] 	BORDER 0
	SRCFILE "samples/pumpkin_master.bas",521
	CLRR R0
	MVO R0,_border_color
	;[522] 	FOR c = 0 TO 10
	SRCFILE "samples/pumpkin_master.bas",522
	MVO R0,var_C
T96:
	;[523] 		WAIT
	SRCFILE "samples/pumpkin_master.bas",523
	CALL _wait
	;[524] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",524
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #10,R0
	BLE T96
	;[525] 
	SRCFILE "samples/pumpkin_master.bas",525
	;[526] 	' No more lives = Game over
	SRCFILE "samples/pumpkin_master.bas",526
	;[527] 	IF lives = 0 THEN
	SRCFILE "samples/pumpkin_master.bas",527
	MVI var_LIVES,R0
	TSTR R0
	BNE T97
	;[528] 		FOR c = 0 TO 255
	SRCFILE "samples/pumpkin_master.bas",528
	MVO R0,var_C
T98:
	;[529] 			PRINT AT 105 COLOR C AND 7,"GAME  OVER"
	SRCFILE "samples/pumpkin_master.bas",529
	MVII #617,R0
	MVO R0,_screen
	MVI var_C,R0
	ANDI #7,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #312,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #48,R0
	MVO@ R0,R4
	XORI #96,R0
	MVO@ R0,R4
	XORI #64,R0
	MVO@ R0,R4
	XORI #296,R0
	MVO@ R0,R4
	MVO@ R0,R4
	XORI #376,R0
	MVO@ R0,R4
	XORI #200,R0
	MVO@ R0,R4
	XORI #152,R0
	MVO@ R0,R4
	XORI #184,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[530] 			WAIT
	SRCFILE "samples/pumpkin_master.bas",530
	CALL _wait
	;[531] 		NEXT c
	SRCFILE "samples/pumpkin_master.bas",531
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #255,R0
	BLE T98
	;[532] 		IF #score > #record THEN #record = #score
	SRCFILE "samples/pumpkin_master.bas",532
	MVI var_&SCORE,R0
	CMP var_&RECORD,R0
	BEQ T99
	BNC T99
	MVO R0,var_&RECORD
T99:
	;[533] 		GOTO title_screen
	SRCFILE "samples/pumpkin_master.bas",533
	B label_TITLE_SCREEN
	;[534] 	END IF
	SRCFILE "samples/pumpkin_master.bas",534
T97:
	;[535] 
	SRCFILE "samples/pumpkin_master.bas",535
	;[536] 	' One life less, restart game
	SRCFILE "samples/pumpkin_master.bas",536
	;[537] 	lives = lives - 1
	SRCFILE "samples/pumpkin_master.bas",537
	MVI var_LIVES,R0
	DECR R0
	MVO R0,var_LIVES
	;[538] 	GOTO restart_game
	SRCFILE "samples/pumpkin_master.bas",538
	B label_RESTART_GAME
	;[539] 
	SRCFILE "samples/pumpkin_master.bas",539
	;[540] 	'
	SRCFILE "samples/pumpkin_master.bas",540
	;[541] 	' Move player and bullet
	SRCFILE "samples/pumpkin_master.bas",541
	;[542] 	'
	SRCFILE "samples/pumpkin_master.bas",542
	;[543] move_player:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",543
	; MOVE_PLAYER
label_MOVE_PLAYER:	PROC
	BEGIN
	;[544] 	IF by THEN	' Active bullet?
	SRCFILE "samples/pumpkin_master.bas",544
	MVI var_BY,R0
	TSTR R0
	BEQ T100
	;[545] 		by = by - 4	' Move towards top
	SRCFILE "samples/pumpkin_master.bas",545
	SUBI #4,R0
	MVO R0,var_BY
	;[546] 		IF by < 16 THEN by = 0	' Disappears if touches score bar
	SRCFILE "samples/pumpkin_master.bas",546
	MVI var_BY,R0
	CMPI #16,R0
	BGE T101
	CLRR R0
	MVO R0,var_BY
T101:
	;[547] 	END IF	
	SRCFILE "samples/pumpkin_master.bas",547
T100:
	;[548] 
	SRCFILE "samples/pumpkin_master.bas",548
	;[549] 	c = CONT
	SRCFILE "samples/pumpkin_master.bas",549
	MVI 510,R0
	XOR 511,R0
	MVO R0,var_C
	;[550] 	d = c AND $E0
	SRCFILE "samples/pumpkin_master.bas",550
	MVI var_C,R0
	ANDI #224,R0
	MVO R0,var_D
	;[551] 	IF (d = $80) + (d = $40) + (d = $20) THEN	' Ignore keypad
	SRCFILE "samples/pumpkin_master.bas",551
	MVI var_D,R0
	CMPI #128,R0
	MVII #65535,R0
	BEQ T103
	INCR R0
T103:
	MVI var_D,R1
	CMPI #64,R1
	MVII #65535,R1
	BEQ T104
	INCR R1
T104:
	ADDR R1,R0
	MVI var_D,R1
	CMPI #32,R1
	MVII #65535,R1
	BEQ T105
	INCR R1
T105:
	ADDR R1,R0
	BEQ T102
	;[552] 	ELSE
	SRCFILE "samples/pumpkin_master.bas",552
	B T106
T102:
	;[553] 		IF (d = $a0) + (d = $c0) + (d = $60) THEN	' Side-button
	SRCFILE "samples/pumpkin_master.bas",553
	MVI var_D,R0
	CMPI #160,R0
	MVII #65535,R0
	BEQ T108
	INCR R0
T108:
	MVI var_D,R1
	CMPI #192,R1
	MVII #65535,R1
	BEQ T109
	INCR R1
T109:
	ADDR R1,R0
	MVI var_D,R1
	CMPI #96,R1
	MVII #65535,R1
	BEQ T110
	INCR R1
T110:
	ADDR R1,R0
	BEQ T107
	;[554] 			IF by = 0 THEN	' Only if no active bullet?
	SRCFILE "samples/pumpkin_master.bas",554
	MVI var_BY,R0
	TSTR R0
	BNE T111
	;[555] 				bx = px + 2	' Start a bullet
	SRCFILE "samples/pumpkin_master.bas",555
	MVI var_PX,R0
	ADDI #2,R0
	MVO R0,var_BX
	;[556] 				by = 96
	SRCFILE "samples/pumpkin_master.bas",556
	MVII #96,R0
	MVO R0,var_BY
	;[557] 				IF sound_effect < 3 THEN sound_effect = 1: sound_state = 0
	SRCFILE "samples/pumpkin_master.bas",557
	MVI var_SOUND_EFFECT,R0
	CMPI #3,R0
	BGE T112
	MVII #1,R0
	MVO R0,var_SOUND_EFFECT
	CLRR R0
	MVO R0,var_SOUND_STATE
T112:
	;[558] 			END IF
	SRCFILE "samples/pumpkin_master.bas",558
T111:
	;[559] 		END IF
	SRCFILE "samples/pumpkin_master.bas",559
T107:
	;[560] 		c = controller_direction(c AND $1F)
	SRCFILE "samples/pumpkin_master.bas",560
	MVII #label_CONTROLLER_DIRECTION,R1
	MVI var_C,R2
	ANDI #31,R2
	ADDR R2,R1
	MVI@ R1,R0
	MVO R0,var_C
	;[561] 		' Move to right
	SRCFILE "samples/pumpkin_master.bas",561
	;[562] 		IF c = 2 THEN IF px < 160 THEN px = px + 2
	SRCFILE "samples/pumpkin_master.bas",562
	MVI var_C,R0
	CMPI #2,R0
	BNE T113
	MVI var_PX,R0
	CMPI #160,R0
	BGE T114
	ADDI #2,R0
	MVO R0,var_PX
T114:
T113:
	;[563] 		' Move to left
	SRCFILE "samples/pumpkin_master.bas",563
	;[564] 		IF c = 4 THEN IF px > 8 THEN px = px - 2 
	SRCFILE "samples/pumpkin_master.bas",564
	MVI var_C,R0
	CMPI #4,R0
	BNE T115
	MVI var_PX,R0
	CMPI #8,R0
	BLE T116
	SUBI #2,R0
	MVO R0,var_PX
T116:
T115:
	;[565] 	END IF
	SRCFILE "samples/pumpkin_master.bas",565
T106:
	;[566] 	END
	SRCFILE "samples/pumpkin_master.bas",566
	RETURN
	ENDP
	;[567] 
	SRCFILE "samples/pumpkin_master.bas",567
	;[568] 	'
	SRCFILE "samples/pumpkin_master.bas",568
	;[569] 	' Update player house and bullet
	SRCFILE "samples/pumpkin_master.bas",569
	;[570] 	'
	SRCFILE "samples/pumpkin_master.bas",570
	;[571] update_player:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",571
	; UPDATE_PLAYER
label_UPDATE_PLAYER:	PROC
	BEGIN
	;[572] 	IF by THEN
	SRCFILE "samples/pumpkin_master.bas",572
	MVI var_BY,R0
	TSTR R0
	BEQ T117
	;[573] 		SPRITE 6, $0300 + bx, $0000 + by, $0805 + 12 * 8
	SRCFILE "samples/pumpkin_master.bas",573
	MVI var_BX,R0
	ADDI #768,R0
	MVO R0,_mobs+6
	MVI var_BY,R0
	MVO R0,_mobs+14
	MVII #2149,R0
	MVO R0,_mobs+22
	;[574] 	ELSE
	SRCFILE "samples/pumpkin_master.bas",574
	B T118
T117:
	;[575] 		SPRITE 6, 0
	SRCFILE "samples/pumpkin_master.bas",575
	CLRR R0
	MVO R0,_mobs+6
	;[576] 	END IF
	SRCFILE "samples/pumpkin_master.bas",576
T118:
	;[577] 	SPRITE 7, $0300 + px, $0080 + py, $1801 + 2 * 8
	SRCFILE "samples/pumpkin_master.bas",577
	MVI var_PX,R0
	ADDI #768,R0
	MVO R0,_mobs+7
	MVI var_PY,R0
	ADDI #128,R0
	MVO R0,_mobs+15
	MVII #6161,R0
	MVO R0,_mobs+23
	;[578] 	END
	SRCFILE "samples/pumpkin_master.bas",578
	RETURN
	ENDP
	;[579] 
	SRCFILE "samples/pumpkin_master.bas",579
	;[580] 	'
	SRCFILE "samples/pumpkin_master.bas",580
	;[581] 	' Update current score
	SRCFILE "samples/pumpkin_master.bas",581
	;[582] 	'
	SRCFILE "samples/pumpkin_master.bas",582
	;[583] update_score:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",583
	; UPDATE_SCORE
label_UPDATE_SCORE:	PROC
	BEGIN
	;[584] 	PRINT AT 0 COLOR 4,"1UP "
	SRCFILE "samples/pumpkin_master.bas",584
	MVII #512,R0
	MVO R0,_screen
	MVII #4,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #136,R0
	XOR _color,R0
	MVO@ R0,R4
	XORI #288,R0
	MVO@ R0,R4
	XORI #40,R0
	MVO@ R0,R4
	XORI #384,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[585] 	PRINT COLOR 5,<5>#score,"0"
	SRCFILE "samples/pumpkin_master.bas",585
	MVII #5,R0
	MVO R0,_color
	MVI var_&SCORE,R0
	MVII #5,R2
	MVI _color,R3
	MVI _screen,R4
	CALL PRNUM16.z
	MVO R4,_screen
	MVI _screen,R4
	MVII #128,R0
	XOR _color,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[586] 	END
	SRCFILE "samples/pumpkin_master.bas",586
	RETURN
	ENDP
	;[587] 
	SRCFILE "samples/pumpkin_master.bas",587
	;[588] 	'
	SRCFILE "samples/pumpkin_master.bas",588
	;[589] 	' Update level
	SRCFILE "samples/pumpkin_master.bas",589
	;[590] 	'
	SRCFILE "samples/pumpkin_master.bas",590
	;[591] update_level:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",591
	; UPDATE_LEVEL
label_UPDATE_LEVEL:	PROC
	BEGIN
	;[592] 	PRINT AT 12 COLOR 4,"L"
	SRCFILE "samples/pumpkin_master.bas",592
	MVII #524,R0
	MVO R0,_screen
	MVII #4,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #352,R0
	XOR _color,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[593] 	PRINT COLOR 5,<2>level
	SRCFILE "samples/pumpkin_master.bas",593
	MVII #5,R0
	MVO R0,_color
	MVI var_LEVEL,R0
	MVII #2,R2
	MVI _color,R3
	MVI _screen,R4
	CALL PRNUM16.z
	MVO R4,_screen
	;[594] 	END
	SRCFILE "samples/pumpkin_master.bas",594
	RETURN
	ENDP
	;[595] 
	SRCFILE "samples/pumpkin_master.bas",595
	;[596] 	'
	SRCFILE "samples/pumpkin_master.bas",596
	;[597] 	' Update number of lives
	SRCFILE "samples/pumpkin_master.bas",597
	;[598] 	'
	SRCFILE "samples/pumpkin_master.bas",598
	;[599] update_lives:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",599
	; UPDATE_LIVES
label_UPDATE_LIVES:	PROC
	BEGIN
	;[600] 	PRINT AT 18 COLOR 4,"H"
	SRCFILE "samples/pumpkin_master.bas",600
	MVII #530,R0
	MVO R0,_screen
	MVII #4,R0
	MVO R0,_color
	MVI _screen,R4
	MVII #320,R0
	XOR _color,R0
	MVO@ R0,R4
	MVO R4,_screen
	;[601] 	PRINT COLOR 5,<>lives
	SRCFILE "samples/pumpkin_master.bas",601
	MVII #5,R0
	MVO R0,_color
	MVI var_LIVES,R0
	MVI _color,R3
	MVI _screen,R4
	CALL PRNUM16.l
	MVO R4,_screen
	;[602] 	END
	SRCFILE "samples/pumpkin_master.bas",602
	RETURN
	ENDP
	;[603] 
	SRCFILE "samples/pumpkin_master.bas",603
	;[604] 	'
	SRCFILE "samples/pumpkin_master.bas",604
	;[605] 	' Wait and clean story
	SRCFILE "samples/pumpkin_master.bas",605
	;[606] 	'
	SRCFILE "samples/pumpkin_master.bas",606
	;[607] wait_and_clean:		PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",607
	; WAIT_AND_CLEAN
label_WAIT_AND_CLEAN:	PROC
	BEGIN
	;[608] 	FOR c = 0 TO 180
	SRCFILE "samples/pumpkin_master.bas",608
	CLRR R0
	MVO R0,var_C
T119:
	;[609] 		IF c >= 60 THEN d = CONT: IF d THEN EXIT FOR
	SRCFILE "samples/pumpkin_master.bas",609
	MVI var_C,R0
	CMPI #60,R0
	BLT T120
	MVI 510,R0
	XOR 511,R0
	MVO R0,var_D
	MVI var_D,R0
	TSTR R0
	BNE T122
T120:
	;[610] 		WAIT
	SRCFILE "samples/pumpkin_master.bas",610
	CALL _wait
	;[611] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",611
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #180,R0
	BLE T119
T122:
	;[612] 	FOR c = 60 TO 119
	SRCFILE "samples/pumpkin_master.bas",612
	MVII #60,R0
	MVO R0,var_C
T123:
	;[613] 		#backtab(c) = 0
	SRCFILE "samples/pumpkin_master.bas",613
	CLRR R0
	MVII #Q2,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[614] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",614
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #119,R0
	BLE T123
	;[615] 	END
	SRCFILE "samples/pumpkin_master.bas",615
	RETURN
	ENDP
	;[616] 
	SRCFILE "samples/pumpkin_master.bas",616
	;[617] 	'
	SRCFILE "samples/pumpkin_master.bas",617
	;[618] 	' Start a new attack wave
	SRCFILE "samples/pumpkin_master.bas",618
	;[619] 	'
	SRCFILE "samples/pumpkin_master.bas",619
	;[620] start_wave:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",620
	; START_WAVE
label_START_WAVE:	PROC
	BEGIN
	;[621] 	wave = cucu_wave
	SRCFILE "samples/pumpkin_master.bas",621
	MVI var_CUCU_WAVE,R0
	MVO R0,var_WAVE
	;[622] 	cucu_wave = cucu_wave + 1
	SRCFILE "samples/pumpkin_master.bas",622
	INCR R0
	MVO R0,var_CUCU_WAVE
	;[623] 	IF cucu_wave = 10 THEN cucu_wave = 0
	SRCFILE "samples/pumpkin_master.bas",623
	MVI var_CUCU_WAVE,R0
	CMPI #10,R0
	BNE T124
	CLRR R0
	MVO R0,var_CUCU_WAVE
T124:
	;[624] '	DO
	SRCFILE "samples/pumpkin_master.bas",624
	;[625] '		c = RAND(10)
	SRCFILE "samples/pumpkin_master.bas",625
	;[626] '	LOOP WHILE wave = c
	SRCFILE "samples/pumpkin_master.bas",626
	;[627] '	wave = c
	SRCFILE "samples/pumpkin_master.bas",627
	;[628] 	sublevel = sublevel + 1
	SRCFILE "samples/pumpkin_master.bas",628
	MVI var_SUBLEVEL,R0
	INCR R0
	MVO R0,var_SUBLEVEL
	;[629] 	next_wave = 30 + RAND(30)
	SRCFILE "samples/pumpkin_master.bas",629
	MVII #30,R0
	MVI _rand,R1
	CALL qs_mpy8
	SWAP R0
	ANDI #255,R0
	ADDI #30,R0
	MVO R0,var_NEXT_WAVE
	;[630] 	IF sublevel = 11 THEN
	SRCFILE "samples/pumpkin_master.bas",630
	MVI var_SUBLEVEL,R0
	CMPI #11,R0
	BNE T125
	;[631] 		sublevel = 0
	SRCFILE "samples/pumpkin_master.bas",631
	CLRR R0
	MVO R0,var_SUBLEVEL
	;[632] 		level = level + 1
	SRCFILE "samples/pumpkin_master.bas",632
	MVI var_LEVEL,R0
	INCR R0
	MVO R0,var_LEVEL
	;[633] 		GOSUB update_level
	SRCFILE "samples/pumpkin_master.bas",633
	CALL label_UPDATE_LEVEL
	;[634] 	END IF
	SRCFILE "samples/pumpkin_master.bas",634
T125:
	;[635] 	END
	SRCFILE "samples/pumpkin_master.bas",635
	RETURN
	ENDP
	;[636] 
	SRCFILE "samples/pumpkin_master.bas",636
	;[637] 	'
	SRCFILE "samples/pumpkin_master.bas",637
	;[638] 	' Start wave 0
	SRCFILE "samples/pumpkin_master.bas",638
	;[639] 	'
	SRCFILE "samples/pumpkin_master.bas",639
	;[640] start_0:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",640
	; START_0
label_START_0:	PROC
	BEGIN
	;[641] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",641
	CLRR R0
	MVO R0,var_C
T126:
	;[642] 		x(c) = 168 + c * 12
	SRCFILE "samples/pumpkin_master.bas",642
	MVI var_C,R0
	MULT R0,R4,12
	ADDI #168,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[643] 		y(c) = 48
	SRCFILE "samples/pumpkin_master.bas",643
	MVII #48,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[644] 		s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",644
	MVII #1,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[645] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",645
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T126
	;[646] 	END
	SRCFILE "samples/pumpkin_master.bas",646
	RETURN
	ENDP
	;[647] 
	SRCFILE "samples/pumpkin_master.bas",647
	;[648] 	'
	SRCFILE "samples/pumpkin_master.bas",648
	;[649] 	' Move wave 0
	SRCFILE "samples/pumpkin_master.bas",649
	;[650] 	'
	SRCFILE "samples/pumpkin_master.bas",650
	;[651] move_0:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",651
	; MOVE_0
label_MOVE_0:	PROC
	BEGIN
	;[652] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",652
	CLRR R0
	MVO R0,var_C
T127:
	;[653] 		ON s(c) GOTO move_00, move_01, move_02, move_03
	SRCFILE "samples/pumpkin_master.bas",653
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #4,R1
	BC T129
	ADDI #T128,R1
	MVI@ R1,PC
T128:
	DECLE label_MOVE_00
	DECLE label_MOVE_01
	DECLE label_MOVE_02
	DECLE label_MOVE_03
T129:
	;[654] 
	SRCFILE "samples/pumpkin_master.bas",654
	;[655] move_01:
	SRCFILE "samples/pumpkin_master.bas",655
	; MOVE_01
label_MOVE_01:	;[656] 		x(c) = x(c) - 2
	SRCFILE "samples/pumpkin_master.bas",656
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	SUBI #2,R0
	MVO@ R0,R3
	;[657] 		IF x(c) = 8 THEN s(c) = 2
	SRCFILE "samples/pumpkin_master.bas",657
	MVI@ R3,R0
	CMPI #8,R0
	BNE T130
	MVII #2,R0
	ADDI #(array_S-array_X) AND $FFFF,R3
	MVO@ R0,R3
T130:
	;[658] 		GOTO move_00
	SRCFILE "samples/pumpkin_master.bas",658
	B label_MOVE_00
	;[659] 
	SRCFILE "samples/pumpkin_master.bas",659
	;[660] move_02:
	SRCFILE "samples/pumpkin_master.bas",660
	; MOVE_02
label_MOVE_02:	;[661] 		x(c) = x(c) + 1
	SRCFILE "samples/pumpkin_master.bas",661
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
	;[662] 		y(c) = y(c) - 1
	SRCFILE "samples/pumpkin_master.bas",662
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	DECR R0
	MVO@ R0,R3
	;[663] 		IF y(c) = 24 THEN s(c) = 3
	SRCFILE "samples/pumpkin_master.bas",663
	MVI@ R3,R0
	CMPI #24,R0
	BNE T131
	MVII #3,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T131:
	;[664] 		GOTO move_00
	SRCFILE "samples/pumpkin_master.bas",664
	B label_MOVE_00
	;[665] 
	SRCFILE "samples/pumpkin_master.bas",665
	;[666] move_03:	x(c) = x(c) + 2
	SRCFILE "samples/pumpkin_master.bas",666
	; MOVE_03
label_MOVE_03:		MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[667] 		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",667
	MVI@ R3,R0
	CMPI #168,R0
	BNE T132
	CLRR R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T132:
	;[668] 		GOTO move_00
	SRCFILE "samples/pumpkin_master.bas",668
	B label_MOVE_00
	;[669] 
	SRCFILE "samples/pumpkin_master.bas",669
	;[670] move_00:
	SRCFILE "samples/pumpkin_master.bas",670
	; MOVE_00
label_MOVE_00:	;[671] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",671
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T127
	;[672] 	END
	SRCFILE "samples/pumpkin_master.bas",672
	RETURN
	ENDP
	;[673] 
	SRCFILE "samples/pumpkin_master.bas",673
	;[674] 	'
	SRCFILE "samples/pumpkin_master.bas",674
	;[675] 	' Start wave 1
	SRCFILE "samples/pumpkin_master.bas",675
	;[676] 	'
	SRCFILE "samples/pumpkin_master.bas",676
	;[677] start_1:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",677
	; START_1
label_START_1:	PROC
	BEGIN
	;[678] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",678
	CLRR R0
	MVO R0,var_C
T133:
	;[679] 		x(c) = 0 - c * 12
	SRCFILE "samples/pumpkin_master.bas",679
	CLRR R0
	MVI var_C,R1
	MULT R1,R4,12
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[680] 		y(c) = 48
	SRCFILE "samples/pumpkin_master.bas",680
	MVII #48,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[681] 		s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",681
	MVII #1,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[682] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",682
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T133
	;[683] 	END
	SRCFILE "samples/pumpkin_master.bas",683
	RETURN
	ENDP
	;[684] 
	SRCFILE "samples/pumpkin_master.bas",684
	;[685] 	'
	SRCFILE "samples/pumpkin_master.bas",685
	;[686] 	' Move wave 1
	SRCFILE "samples/pumpkin_master.bas",686
	;[687] 	'
	SRCFILE "samples/pumpkin_master.bas",687
	;[688] move_1:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",688
	; MOVE_1
label_MOVE_1:	PROC
	BEGIN
	;[689] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",689
	CLRR R0
	MVO R0,var_C
T134:
	;[690] 		ON s(c) GOTO move_010, move_011, move_012, move_013
	SRCFILE "samples/pumpkin_master.bas",690
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #4,R1
	BC T136
	ADDI #T135,R1
	MVI@ R1,PC
T135:
	DECLE label_MOVE_010
	DECLE label_MOVE_011
	DECLE label_MOVE_012
	DECLE label_MOVE_013
T136:
	;[691] 
	SRCFILE "samples/pumpkin_master.bas",691
	;[692] move_011:
	SRCFILE "samples/pumpkin_master.bas",692
	; MOVE_011
label_MOVE_011:	;[693] 		x(c) = x(c) + 2
	SRCFILE "samples/pumpkin_master.bas",693
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[694] 		IF x(c) = 160 THEN s(c) = 2
	SRCFILE "samples/pumpkin_master.bas",694
	MVI@ R3,R0
	CMPI #160,R0
	BNE T137
	MVII #2,R0
	ADDI #(array_S-array_X) AND $FFFF,R3
	MVO@ R0,R3
T137:
	;[695] 		GOTO move_010
	SRCFILE "samples/pumpkin_master.bas",695
	B label_MOVE_010
	;[696] 
	SRCFILE "samples/pumpkin_master.bas",696
	;[697] move_012:
	SRCFILE "samples/pumpkin_master.bas",697
	; MOVE_012
label_MOVE_012:	;[698] 		x(c) = x(c) - 1
	SRCFILE "samples/pumpkin_master.bas",698
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	DECR R0
	MVO@ R0,R3
	;[699] 		y(c) = y(c) - 1
	SRCFILE "samples/pumpkin_master.bas",699
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	DECR R0
	MVO@ R0,R3
	;[700] 		IF y(c) = 24 THEN s(c) = 3
	SRCFILE "samples/pumpkin_master.bas",700
	MVI@ R3,R0
	CMPI #24,R0
	BNE T138
	MVII #3,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T138:
	;[701] 		GOTO move_010
	SRCFILE "samples/pumpkin_master.bas",701
	B label_MOVE_010
	;[702] 
	SRCFILE "samples/pumpkin_master.bas",702
	;[703] move_013:	x(c) = x(c) - 2
	SRCFILE "samples/pumpkin_master.bas",703
	; MOVE_013
label_MOVE_013:		MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	SUBI #2,R0
	MVO@ R0,R3
	;[704] 		IF x(c) = 0 THEN y(c) = 0: s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",704
	MVI@ R3,R0
	TSTR R0
	BNE T139
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T139:
	;[705] 		GOTO move_010
	SRCFILE "samples/pumpkin_master.bas",705
	B label_MOVE_010
	;[706] 
	SRCFILE "samples/pumpkin_master.bas",706
	;[707] move_010:
	SRCFILE "samples/pumpkin_master.bas",707
	; MOVE_010
label_MOVE_010:	;[708] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",708
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T134
	;[709] 	END
	SRCFILE "samples/pumpkin_master.bas",709
	RETURN
	ENDP
	;[710] 
	SRCFILE "samples/pumpkin_master.bas",710
	;[711] 	'
	SRCFILE "samples/pumpkin_master.bas",711
	;[712] 	' Start wave 2
	SRCFILE "samples/pumpkin_master.bas",712
	;[713] 	'
	SRCFILE "samples/pumpkin_master.bas",713
	;[714] start_2:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",714
	; START_2
label_START_2:	PROC
	BEGIN
	;[715] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",715
	CLRR R0
	MVO R0,var_C
T140:
	;[716] 		x(c) = 168 + c * 12
	SRCFILE "samples/pumpkin_master.bas",716
	MVI var_C,R0
	MULT R0,R4,12
	ADDI #168,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[717] 		y(c) = 48
	SRCFILE "samples/pumpkin_master.bas",717
	MVII #48,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[718] 		s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",718
	MVII #1,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[719] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",719
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T140
	;[720] 	END
	SRCFILE "samples/pumpkin_master.bas",720
	RETURN
	ENDP
	;[721] 
	SRCFILE "samples/pumpkin_master.bas",721
	;[722] 	'
	SRCFILE "samples/pumpkin_master.bas",722
	;[723] 	' Move wave 2
	SRCFILE "samples/pumpkin_master.bas",723
	;[724] 	'
	SRCFILE "samples/pumpkin_master.bas",724
	;[725] move_2:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",725
	; MOVE_2
label_MOVE_2:	PROC
	BEGIN
	;[726] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",726
	CLRR R0
	MVO R0,var_C
T141:
	;[727] 		ON s(c) GOTO move_020, move_021
	SRCFILE "samples/pumpkin_master.bas",727
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #2,R1
	BC T143
	ADDI #T142,R1
	MVI@ R1,PC
T142:
	DECLE label_MOVE_020
	DECLE label_MOVE_021
T143:
	;[728] 
	SRCFILE "samples/pumpkin_master.bas",728
	;[729] move_021:
	SRCFILE "samples/pumpkin_master.bas",729
	; MOVE_021
label_MOVE_021:	;[730] 		x(c) = x(c) - 2
	SRCFILE "samples/pumpkin_master.bas",730
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	SUBI #2,R0
	MVO@ R0,R3
	;[731] 		y(c) = 24 + sin24((x(c) AND $3e) / 2)
	SRCFILE "samples/pumpkin_master.bas",731
	MVII #label_SIN24,R1
	MVI@ R3,R2
	ANDI #62,R2
	SLR R2,1
	ADDR R2,R1
	MVI@ R1,R0
	ADDI #24,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[732] 		IF x(c) = 0 THEN y(c) = 0: s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",732
	ADDI #(array_X-array_Y) AND $FFFF,R3
	MVI@ R3,R0
	TSTR R0
	BNE T144
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T144:
	;[733] 		GOTO move_020
	SRCFILE "samples/pumpkin_master.bas",733
	B label_MOVE_020
	;[734] 
	SRCFILE "samples/pumpkin_master.bas",734
	;[735] move_020:
	SRCFILE "samples/pumpkin_master.bas",735
	; MOVE_020
label_MOVE_020:	;[736] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",736
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T141
	;[737] 	END
	SRCFILE "samples/pumpkin_master.bas",737
	RETURN
	ENDP
	;[738] 
	SRCFILE "samples/pumpkin_master.bas",738
	;[739] 	'
	SRCFILE "samples/pumpkin_master.bas",739
	;[740] 	' Start wave 3
	SRCFILE "samples/pumpkin_master.bas",740
	;[741] 	'
	SRCFILE "samples/pumpkin_master.bas",741
	;[742] start_3:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",742
	; START_3
label_START_3:	PROC
	BEGIN
	;[743] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",743
	CLRR R0
	MVO R0,var_C
T145:
	;[744] 		x(c) = 0 - c * 12
	SRCFILE "samples/pumpkin_master.bas",744
	CLRR R0
	MVI var_C,R1
	MULT R1,R4,12
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[745] 		y(c) = 48
	SRCFILE "samples/pumpkin_master.bas",745
	MVII #48,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[746] 		s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",746
	MVII #1,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[747] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",747
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T145
	;[748] 	END
	SRCFILE "samples/pumpkin_master.bas",748
	RETURN
	ENDP
	;[749] 
	SRCFILE "samples/pumpkin_master.bas",749
	;[750] 	'
	SRCFILE "samples/pumpkin_master.bas",750
	;[751] 	' Move wave 3
	SRCFILE "samples/pumpkin_master.bas",751
	;[752] 	'
	SRCFILE "samples/pumpkin_master.bas",752
	;[753] move_3:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",753
	; MOVE_3
label_MOVE_3:	PROC
	BEGIN
	;[754] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",754
	CLRR R0
	MVO R0,var_C
T146:
	;[755] 		ON s(c) GOTO move_030, move_031
	SRCFILE "samples/pumpkin_master.bas",755
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #2,R1
	BC T148
	ADDI #T147,R1
	MVI@ R1,PC
T147:
	DECLE label_MOVE_030
	DECLE label_MOVE_031
T148:
	;[756] 
	SRCFILE "samples/pumpkin_master.bas",756
	;[757] move_031:
	SRCFILE "samples/pumpkin_master.bas",757
	; MOVE_031
label_MOVE_031:	;[758] 		x(c) = x(c) + 2
	SRCFILE "samples/pumpkin_master.bas",758
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[759] 		y(c) = 24 + sin24((x(c) AND $3e) / 2)
	SRCFILE "samples/pumpkin_master.bas",759
	MVII #label_SIN24,R1
	MVI@ R3,R2
	ANDI #62,R2
	SLR R2,1
	ADDR R2,R1
	MVI@ R1,R0
	ADDI #24,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[760] 		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",760
	ADDI #(array_X-array_Y) AND $FFFF,R3
	MVI@ R3,R0
	CMPI #168,R0
	BNE T149
	CLRR R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T149:
	;[761] 		GOTO move_030
	SRCFILE "samples/pumpkin_master.bas",761
	B label_MOVE_030
	;[762] 
	SRCFILE "samples/pumpkin_master.bas",762
	;[763] move_030:
	SRCFILE "samples/pumpkin_master.bas",763
	; MOVE_030
label_MOVE_030:	;[764] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",764
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T146
	;[765] 	END
	SRCFILE "samples/pumpkin_master.bas",765
	RETURN
	ENDP
	;[766] 
	SRCFILE "samples/pumpkin_master.bas",766
	;[767] 	'
	SRCFILE "samples/pumpkin_master.bas",767
	;[768] 	' Start wave 4
	SRCFILE "samples/pumpkin_master.bas",768
	;[769] 	'
	SRCFILE "samples/pumpkin_master.bas",769
	;[770] start_4:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",770
	; START_4
label_START_4:	PROC
	BEGIN
	;[771] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",771
	CLRR R0
	MVO R0,var_C
T150:
	;[772] 		x(c) = 0 - c * 12
	SRCFILE "samples/pumpkin_master.bas",772
	CLRR R0
	MVI var_C,R1
	MULT R1,R4,12
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[773] 		y(c) = 24
	SRCFILE "samples/pumpkin_master.bas",773
	MVII #24,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[774] 		s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",774
	MVII #1,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[775] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",775
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T150
	;[776] 	END
	SRCFILE "samples/pumpkin_master.bas",776
	RETURN
	ENDP
	;[777] 
	SRCFILE "samples/pumpkin_master.bas",777
	;[778] 	'
	SRCFILE "samples/pumpkin_master.bas",778
	;[779] 	' Move wave 4
	SRCFILE "samples/pumpkin_master.bas",779
	;[780] 	'
	SRCFILE "samples/pumpkin_master.bas",780
	;[781] move_4:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",781
	; MOVE_4
label_MOVE_4:	PROC
	BEGIN
	;[782] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",782
	CLRR R0
	MVO R0,var_C
T151:
	;[783] 		ON s(c) GOTO move_040, move_041, move_042, move_043
	SRCFILE "samples/pumpkin_master.bas",783
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #4,R1
	BC T153
	ADDI #T152,R1
	MVI@ R1,PC
T152:
	DECLE label_MOVE_040
	DECLE label_MOVE_041
	DECLE label_MOVE_042
	DECLE label_MOVE_043
T153:
	;[784] 
	SRCFILE "samples/pumpkin_master.bas",784
	;[785] move_041:
	SRCFILE "samples/pumpkin_master.bas",785
	; MOVE_041
label_MOVE_041:	;[786] 		x(c) = x(c) + 2
	SRCFILE "samples/pumpkin_master.bas",786
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[787] 		IF x(c) = 90 THEN s(c) = 2
	SRCFILE "samples/pumpkin_master.bas",787
	MVI@ R3,R0
	CMPI #90,R0
	BNE T154
	MVII #2,R0
	ADDI #(array_S-array_X) AND $FFFF,R3
	MVO@ R0,R3
T154:
	;[788] 		GOTO move_040
	SRCFILE "samples/pumpkin_master.bas",788
	B label_MOVE_040
	;[789] 
	SRCFILE "samples/pumpkin_master.bas",789
	;[790] move_042:
	SRCFILE "samples/pumpkin_master.bas",790
	; MOVE_042
label_MOVE_042:	;[791] 		y(c) = y(c) + 1
	SRCFILE "samples/pumpkin_master.bas",791
	MVII #array_Y,R3
	ADD var_C,R3
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
	;[792] 		x(c) = 72 + sin24(y(c) AND $1f)
	SRCFILE "samples/pumpkin_master.bas",792
	MVII #label_SIN24,R1
	MVI@ R3,R2
	ANDI #31,R2
	ADDR R2,R1
	MVI@ R1,R0
	ADDI #72,R0
	ADDI #(array_X-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[793] 		IF y(c) = 80 THEN s(c) = 3
	SRCFILE "samples/pumpkin_master.bas",793
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	CMPI #80,R0
	BNE T155
	MVII #3,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T155:
	;[794] 		GOTO move_040
	SRCFILE "samples/pumpkin_master.bas",794
	B label_MOVE_040
	;[795] 
	SRCFILE "samples/pumpkin_master.bas",795
	;[796] move_043:
	SRCFILE "samples/pumpkin_master.bas",796
	; MOVE_043
label_MOVE_043:	;[797] 		x(c) = (x(c) AND $fe) + 2
	SRCFILE "samples/pumpkin_master.bas",797
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ANDI #254,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[798] 		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",798
	MVI@ R3,R0
	CMPI #168,R0
	BNE T156
	CLRR R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T156:
	;[799] 		GOTO move_040
	SRCFILE "samples/pumpkin_master.bas",799
	B label_MOVE_040
	;[800] 
	SRCFILE "samples/pumpkin_master.bas",800
	;[801] move_040:
	SRCFILE "samples/pumpkin_master.bas",801
	; MOVE_040
label_MOVE_040:	;[802] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",802
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T151
	;[803] 	END
	SRCFILE "samples/pumpkin_master.bas",803
	RETURN
	ENDP
	;[804] 
	SRCFILE "samples/pumpkin_master.bas",804
	;[805] 	'
	SRCFILE "samples/pumpkin_master.bas",805
	;[806] 	' Start wave 5
	SRCFILE "samples/pumpkin_master.bas",806
	;[807] 	'
	SRCFILE "samples/pumpkin_master.bas",807
	;[808] start_5:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",808
	; START_5
label_START_5:	PROC
	BEGIN
	;[809] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",809
	CLRR R0
	MVO R0,var_C
T157:
	;[810] 		x(c) = 168 + RANDOM(32) * 2
	SRCFILE "samples/pumpkin_master.bas",810
	CALL _next_random
	ANDI #31,R0
	SLL R0,1
	ADDI #168,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[811] 		y(c) = 24 + c * 8
	SRCFILE "samples/pumpkin_master.bas",811
	MVI var_C,R0
	SLL R0,2
	ADDR R0,R0
	ADDI #24,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[812] 		s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",812
	MVII #1,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[813] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",813
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T157
	;[814] 	END
	SRCFILE "samples/pumpkin_master.bas",814
	RETURN
	ENDP
	;[815] 
	SRCFILE "samples/pumpkin_master.bas",815
	;[816] 	'
	SRCFILE "samples/pumpkin_master.bas",816
	;[817] 	' Move wave 5
	SRCFILE "samples/pumpkin_master.bas",817
	;[818] 	'
	SRCFILE "samples/pumpkin_master.bas",818
	;[819] move_5:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",819
	; MOVE_5
label_MOVE_5:	PROC
	BEGIN
	;[820] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",820
	CLRR R0
	MVO R0,var_C
T158:
	;[821] 		ON s(c) GOTO move_050, move_051
	SRCFILE "samples/pumpkin_master.bas",821
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #2,R1
	BC T160
	ADDI #T159,R1
	MVI@ R1,PC
T159:
	DECLE label_MOVE_050
	DECLE label_MOVE_051
T160:
	;[822] 
	SRCFILE "samples/pumpkin_master.bas",822
	;[823] move_051:
	SRCFILE "samples/pumpkin_master.bas",823
	; MOVE_051
label_MOVE_051:	;[824] 		x(c) = x(c) - 2
	SRCFILE "samples/pumpkin_master.bas",824
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	SUBI #2,R0
	MVO@ R0,R3
	;[825] 		IF x(c) = 0 THEN y(c) = 0: s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",825
	MVI@ R3,R0
	TSTR R0
	BNE T161
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T161:
	;[826] 		GOTO move_050
	SRCFILE "samples/pumpkin_master.bas",826
	B label_MOVE_050
	;[827] 
	SRCFILE "samples/pumpkin_master.bas",827
	;[828] move_050:
	SRCFILE "samples/pumpkin_master.bas",828
	; MOVE_050
label_MOVE_050:	;[829] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",829
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T158
	;[830] 	END
	SRCFILE "samples/pumpkin_master.bas",830
	RETURN
	ENDP
	;[831] 
	SRCFILE "samples/pumpkin_master.bas",831
	;[832] 	'
	SRCFILE "samples/pumpkin_master.bas",832
	;[833] 	' Start wave 6
	SRCFILE "samples/pumpkin_master.bas",833
	;[834] 	'
	SRCFILE "samples/pumpkin_master.bas",834
	;[835] start_6:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",835
	; START_6
label_START_6:	PROC
	BEGIN
	;[836] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",836
	CLRR R0
	MVO R0,var_C
T162:
	;[837] 		IF c AND 1 THEN
	SRCFILE "samples/pumpkin_master.bas",837
	MVI var_C,R0
	ANDI #1,R0
	BEQ T163
	;[838] 			x(c) = 168 + RANDOM(32) * 2
	SRCFILE "samples/pumpkin_master.bas",838
	CALL _next_random
	ANDI #31,R0
	SLL R0,1
	ADDI #168,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[839] 			s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",839
	MVII #1,R0
	ADDI #(array_S-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[840] 		ELSE
	SRCFILE "samples/pumpkin_master.bas",840
	B T164
T163:
	;[841] 			x(c) = 0 - RANDOM(32) * 2
	SRCFILE "samples/pumpkin_master.bas",841
	CALL _next_random
	ANDI #31,R0
	SLL R0,1
	NEGR R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[842] 			s(c) = 2
	SRCFILE "samples/pumpkin_master.bas",842
	MVII #2,R0
	ADDI #(array_S-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[843] 		END IF
	SRCFILE "samples/pumpkin_master.bas",843
T164:
	;[844] 		y(c) = 24 + c * 8
	SRCFILE "samples/pumpkin_master.bas",844
	MVI var_C,R0
	SLL R0,2
	ADDR R0,R0
	ADDI #24,R0
	MVII #array_Y,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[845] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",845
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T162
	;[846] 	END
	SRCFILE "samples/pumpkin_master.bas",846
	RETURN
	ENDP
	;[847] 
	SRCFILE "samples/pumpkin_master.bas",847
	;[848] 	'
	SRCFILE "samples/pumpkin_master.bas",848
	;[849] 	' Move wave 6
	SRCFILE "samples/pumpkin_master.bas",849
	;[850] 	'
	SRCFILE "samples/pumpkin_master.bas",850
	;[851] move_6:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",851
	; MOVE_6
label_MOVE_6:	PROC
	BEGIN
	;[852] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",852
	CLRR R0
	MVO R0,var_C
T165:
	;[853] 		ON s(c) GOTO move_060, move_061, move_062
	SRCFILE "samples/pumpkin_master.bas",853
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #3,R1
	BC T167
	ADDI #T166,R1
	MVI@ R1,PC
T166:
	DECLE label_MOVE_060
	DECLE label_MOVE_061
	DECLE label_MOVE_062
T167:
	;[854] 
	SRCFILE "samples/pumpkin_master.bas",854
	;[855] move_061:
	SRCFILE "samples/pumpkin_master.bas",855
	; MOVE_061
label_MOVE_061:	;[856] 		x(c) = x(c) - 2
	SRCFILE "samples/pumpkin_master.bas",856
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	SUBI #2,R0
	MVO@ R0,R3
	;[857] 		IF x(c) = 0 THEN y(c) = 0: s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",857
	MVI@ R3,R0
	TSTR R0
	BNE T168
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T168:
	;[858] 		GOTO move_060
	SRCFILE "samples/pumpkin_master.bas",858
	B label_MOVE_060
	;[859] 
	SRCFILE "samples/pumpkin_master.bas",859
	;[860] move_062:
	SRCFILE "samples/pumpkin_master.bas",860
	; MOVE_062
label_MOVE_062:	;[861] 		x(c) = x(c) + 2
	SRCFILE "samples/pumpkin_master.bas",861
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[862] 		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",862
	MVI@ R3,R0
	CMPI #168,R0
	BNE T169
	CLRR R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T169:
	;[863] 		GOTO move_060
	SRCFILE "samples/pumpkin_master.bas",863
	B label_MOVE_060
	;[864] 
	SRCFILE "samples/pumpkin_master.bas",864
	;[865] move_060:
	SRCFILE "samples/pumpkin_master.bas",865
	; MOVE_060
label_MOVE_060:	;[866] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",866
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T165
	;[867] 	END
	SRCFILE "samples/pumpkin_master.bas",867
	RETURN
	ENDP
	;[868] 
	SRCFILE "samples/pumpkin_master.bas",868
	;[869] 	'
	SRCFILE "samples/pumpkin_master.bas",869
	;[870] 	' Start wave 7
	SRCFILE "samples/pumpkin_master.bas",870
	;[871] 	'
	SRCFILE "samples/pumpkin_master.bas",871
	;[872] start_7:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",872
	; START_7
label_START_7:	PROC
	BEGIN
	;[873] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",873
	CLRR R0
	MVO R0,var_C
T170:
	;[874] 		d = c % 3
	SRCFILE "samples/pumpkin_master.bas",874
	MVI var_C,R0
	MVII #3,R4
	MVO R0,40842
	MVO R4,40843
	MVI 40847,R0
	MVO R0,var_D
	;[875] 		IF d = 0 THEN x(c) = 0 - c / 3 * 32:y(c) = 24
	SRCFILE "samples/pumpkin_master.bas",875
	MVI var_D,R0
	TSTR R0
	BNE T171
	MVI var_C,R1
	MVII #3,R4
	MVO R1,40842
	MVO R4,40843
	MVI 40846,R1
	SLL R1,2
	SLL R1,2
	ADDR R1,R1
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	MVII #24,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
T171:
	;[876] 		IF d = 1 THEN x(c) = 0 - c / 3 * 32:y(c) = 40
	SRCFILE "samples/pumpkin_master.bas",876
	MVI var_D,R0
	CMPI #1,R0
	BNE T172
	CLRR R0
	MVI var_C,R1
	MVII #3,R4
	MVO R1,40842
	MVO R4,40843
	MVI 40846,R1
	SLL R1,2
	SLL R1,2
	ADDR R1,R1
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	MVII #40,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
T172:
	;[877] 		IF d = 2 THEN x(c) = -16 - c / 3 * 32: y(c) = 32
	SRCFILE "samples/pumpkin_master.bas",877
	MVI var_D,R0
	CMPI #2,R0
	BNE T173
	MVII #65520,R0
	MVI var_C,R1
	MVII #3,R4
	MVO R1,40842
	MVO R4,40843
	MVI 40846,R1
	SLL R1,2
	SLL R1,2
	ADDR R1,R1
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	MVII #32,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
T173:
	;[878] 		s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",878
	MVII #1,R0
	MVII #array_S,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[879] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",879
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T170
	;[880] 	END
	SRCFILE "samples/pumpkin_master.bas",880
	RETURN
	ENDP
	;[881] 
	SRCFILE "samples/pumpkin_master.bas",881
	;[882] 	'
	SRCFILE "samples/pumpkin_master.bas",882
	;[883] 	' Move wave 7
	SRCFILE "samples/pumpkin_master.bas",883
	;[884] 	'
	SRCFILE "samples/pumpkin_master.bas",884
	;[885] move_7:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",885
	; MOVE_7
label_MOVE_7:	PROC
	BEGIN
	;[886] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",886
	CLRR R0
	MVO R0,var_C
T174:
	;[887] 		ON s(c) GOTO move_70, move_71
	SRCFILE "samples/pumpkin_master.bas",887
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #2,R1
	BC T176
	ADDI #T175,R1
	MVI@ R1,PC
T175:
	DECLE label_MOVE_70
	DECLE label_MOVE_71
T176:
	;[888] 
	SRCFILE "samples/pumpkin_master.bas",888
	;[889] move_71:
	SRCFILE "samples/pumpkin_master.bas",889
	; MOVE_71
label_MOVE_71:	;[890] 		x(c) = x(c) + 2
	SRCFILE "samples/pumpkin_master.bas",890
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[891] 		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
	SRCFILE "samples/pumpkin_master.bas",891
	MVI@ R3,R0
	CMPI #168,R0
	BNE T177
	CLRR R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T177:
	;[892] 		GOTO move_70
	SRCFILE "samples/pumpkin_master.bas",892
	B label_MOVE_70
	;[893] 
	SRCFILE "samples/pumpkin_master.bas",893
	;[894] move_70:
	SRCFILE "samples/pumpkin_master.bas",894
	; MOVE_70
label_MOVE_70:	;[895] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",895
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T174
	;[896] 	END
	SRCFILE "samples/pumpkin_master.bas",896
	RETURN
	ENDP
	;[897] 
	SRCFILE "samples/pumpkin_master.bas",897
	;[898] 	'
	SRCFILE "samples/pumpkin_master.bas",898
	;[899] 	' Start wave 8
	SRCFILE "samples/pumpkin_master.bas",899
	;[900] 	'
	SRCFILE "samples/pumpkin_master.bas",900
	;[901] start_8:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",901
	; START_8
label_START_8:	PROC
	BEGIN
	;[902] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",902
	CLRR R0
	MVO R0,var_C
T178:
	;[903] 		x(c) = 0 - c * 12
	SRCFILE "samples/pumpkin_master.bas",903
	CLRR R0
	MVI var_C,R1
	MULT R1,R4,12
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[904] 		y(c) = 24
	SRCFILE "samples/pumpkin_master.bas",904
	MVII #24,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[905] 		s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",905
	MVII #1,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	;[906] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",906
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T178
	;[907] 	END
	SRCFILE "samples/pumpkin_master.bas",907
	RETURN
	ENDP
	;[908] 
	SRCFILE "samples/pumpkin_master.bas",908
	;[909] 	'
	SRCFILE "samples/pumpkin_master.bas",909
	;[910] 	' Move wave 8
	SRCFILE "samples/pumpkin_master.bas",910
	;[911] 	'
	SRCFILE "samples/pumpkin_master.bas",911
	;[912] move_8:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",912
	; MOVE_8
label_MOVE_8:	PROC
	BEGIN
	;[913] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",913
	CLRR R0
	MVO R0,var_C
T179:
	;[914] 		ON s(c) GOTO move_080, move_081, move_082, move_083, move_084, move_085
	SRCFILE "samples/pumpkin_master.bas",914
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #6,R1
	BC T181
	ADDI #T180,R1
	MVI@ R1,PC
T180:
	DECLE label_MOVE_080
	DECLE label_MOVE_081
	DECLE label_MOVE_082
	DECLE label_MOVE_083
	DECLE label_MOVE_084
	DECLE label_MOVE_085
T181:
	;[915] 
	SRCFILE "samples/pumpkin_master.bas",915
	;[916] move_081:
	SRCFILE "samples/pumpkin_master.bas",916
	; MOVE_081
label_MOVE_081:	;[917] 		x(c) = x(c) + 2
	SRCFILE "samples/pumpkin_master.bas",917
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[918] 		IF x(c) = 160 THEN s(c) = 2
	SRCFILE "samples/pumpkin_master.bas",918
	MVI@ R3,R0
	CMPI #160,R0
	BNE T182
	MVII #2,R0
	ADDI #(array_S-array_X) AND $FFFF,R3
	MVO@ R0,R3
T182:
	;[919] 		GOTO move_080
	SRCFILE "samples/pumpkin_master.bas",919
	B label_MOVE_080
	;[920] 
	SRCFILE "samples/pumpkin_master.bas",920
	;[921] move_082:
	SRCFILE "samples/pumpkin_master.bas",921
	; MOVE_082
label_MOVE_082:	;[922] 		x(c) = x(c) - 1
	SRCFILE "samples/pumpkin_master.bas",922
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	DECR R0
	MVO@ R0,R3
	;[923] 		y(c) = y(c) + 1
	SRCFILE "samples/pumpkin_master.bas",923
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
	;[924] 		IF (y(c) AND 7) = 0 THEN IF y(c) = 80 THEN s(c) = 5 ELSE s(c) = 3
	SRCFILE "samples/pumpkin_master.bas",924
	MVI@ R3,R0
	ANDI #7,R0
	BNE T183
	MVI@ R3,R0
	CMPI #80,R0
	BNE T184
	MVII #5,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
	B T185
T184:
	MVII #3,R0
	MVII #array_S,R3
	ADD var_C,R3
	MVO@ R0,R3
T185:
T183:
	;[925] 		GOTO move_080
	SRCFILE "samples/pumpkin_master.bas",925
	B label_MOVE_080
	;[926] 
	SRCFILE "samples/pumpkin_master.bas",926
	;[927] move_083:	x(c) = x(c) - 2
	SRCFILE "samples/pumpkin_master.bas",927
	; MOVE_083
label_MOVE_083:		MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	SUBI #2,R0
	MVO@ R0,R3
	;[928] 		IF x(c) = 8 THEN s(c) = 4
	SRCFILE "samples/pumpkin_master.bas",928
	MVI@ R3,R0
	CMPI #8,R0
	BNE T186
	MVII #4,R0
	ADDI #(array_S-array_X) AND $FFFF,R3
	MVO@ R0,R3
T186:
	;[929] 		GOTO move_080
	SRCFILE "samples/pumpkin_master.bas",929
	B label_MOVE_080
	;[930] 
	SRCFILE "samples/pumpkin_master.bas",930
	;[931] move_084:
	SRCFILE "samples/pumpkin_master.bas",931
	; MOVE_084
label_MOVE_084:	;[932] 		x(c) = x(c) + 1
	SRCFILE "samples/pumpkin_master.bas",932
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
	;[933] 		y(c) = y(c) + 1
	SRCFILE "samples/pumpkin_master.bas",933
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
	;[934] 		IF (y(c) AND 7) = 0 THEN s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",934
	MVI@ R3,R0
	ANDI #7,R0
	BNE T187
	MVII #1,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T187:
	;[935] 		GOTO move_080
	SRCFILE "samples/pumpkin_master.bas",935
	B label_MOVE_080
	;[936] 
	SRCFILE "samples/pumpkin_master.bas",936
	;[937] move_085:
	SRCFILE "samples/pumpkin_master.bas",937
	; MOVE_085
label_MOVE_085:	;[938] 		x(c) = x(c) - 2
	SRCFILE "samples/pumpkin_master.bas",938
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	SUBI #2,R0
	MVO@ R0,R3
	;[939] 		IF x(c) = 0 THEN s(c) = 0: y(c) = 0
	SRCFILE "samples/pumpkin_master.bas",939
	MVI@ R3,R0
	TSTR R0
	BNE T188
	ADDI #(array_S-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_Y-array_S) AND $FFFF,R3
	MVO@ R0,R3
T188:
	;[940] 		GOTO move_080
	SRCFILE "samples/pumpkin_master.bas",940
	B label_MOVE_080
	;[941] 
	SRCFILE "samples/pumpkin_master.bas",941
	;[942] move_080:
	SRCFILE "samples/pumpkin_master.bas",942
	; MOVE_080
label_MOVE_080:	;[943] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",943
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T179
	;[944] 	END
	SRCFILE "samples/pumpkin_master.bas",944
	RETURN
	ENDP
	;[945] 
	SRCFILE "samples/pumpkin_master.bas",945
	;[946] 	'
	SRCFILE "samples/pumpkin_master.bas",946
	;[947] 	' Start wave 9
	SRCFILE "samples/pumpkin_master.bas",947
	;[948] 	'
	SRCFILE "samples/pumpkin_master.bas",948
	;[949] start_9:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",949
	; START_9
label_START_9:	PROC
	BEGIN
	;[950] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",950
	CLRR R0
	MVO R0,var_C
T189:
	;[951] 		IF c AND 1 THEN
	SRCFILE "samples/pumpkin_master.bas",951
	MVI var_C,R0
	ANDI #1,R0
	BEQ T190
	;[952] 			x(c) = 0 - c / 2 * 24
	SRCFILE "samples/pumpkin_master.bas",952
	CLRR R0
	MVI var_C,R1
	SLR R1,1
	MULT R1,R4,24
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[953] 			y(c) = 24
	SRCFILE "samples/pumpkin_master.bas",953
	MVII #24,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[954] 		ELSE
	SRCFILE "samples/pumpkin_master.bas",954
	B T191
T190:
	;[955] 			x(c) = 0 - c / 2 * 24
	SRCFILE "samples/pumpkin_master.bas",955
	CLRR R0
	MVI var_C,R1
	SLR R1,1
	MULT R1,R4,24
	SUBR R1,R0
	MVII #array_X,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[956] 			y(c) = 40
	SRCFILE "samples/pumpkin_master.bas",956
	MVII #40,R0
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVO@ R0,R3
	;[957] 		END IF
	SRCFILE "samples/pumpkin_master.bas",957
T191:
	;[958] 		s(c) = 1
	SRCFILE "samples/pumpkin_master.bas",958
	MVII #1,R0
	MVII #array_S,R3
	ADD var_C,R3
	MVO@ R0,R3
	;[959] 		z(c) = 54
	SRCFILE "samples/pumpkin_master.bas",959
	MVII #54,R0
	ADDI #(array_Z-array_S) AND $FFFF,R3
	MVO@ R0,R3
	;[960] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",960
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T189
	;[961] 	END
	SRCFILE "samples/pumpkin_master.bas",961
	RETURN
	ENDP
	;[962] 
	SRCFILE "samples/pumpkin_master.bas",962
	;[963] 	'
	SRCFILE "samples/pumpkin_master.bas",963
	;[964] 	' Move wave 9
	SRCFILE "samples/pumpkin_master.bas",964
	;[965] 	'
	SRCFILE "samples/pumpkin_master.bas",965
	;[966] move_9:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",966
	; MOVE_9
label_MOVE_9:	PROC
	BEGIN
	;[967] 	FOR c = 0 TO PUMPKINS - 1
	SRCFILE "samples/pumpkin_master.bas",967
	CLRR R0
	MVO R0,var_C
T192:
	;[968] 		ON s(c) GOTO move_090, move_091, move_092, move_093
	SRCFILE "samples/pumpkin_master.bas",968
	MVII #array_S,R3
	ADD var_C,R3
	MVI@ R3,R1
	CMPI #4,R1
	BC T194
	ADDI #T193,R1
	MVI@ R1,PC
T193:
	DECLE label_MOVE_090
	DECLE label_MOVE_091
	DECLE label_MOVE_092
	DECLE label_MOVE_093
T194:
	;[969] 
	SRCFILE "samples/pumpkin_master.bas",969
	;[970] move_091:
	SRCFILE "samples/pumpkin_master.bas",970
	; MOVE_091
label_MOVE_091:	;[971] 		x(c) = x(c) + 2
	SRCFILE "samples/pumpkin_master.bas",971
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[972] 		z(c) = z(c) - 1
	SRCFILE "samples/pumpkin_master.bas",972
	ADDI #(array_Z-array_X) AND $FFFF,R3
	MVI@ R3,R0
	DECR R0
	MVO@ R0,R3
	;[973] 		IF z(c) = 0 THEN s(c) = 2
	SRCFILE "samples/pumpkin_master.bas",973
	MVI@ R3,R0
	TSTR R0
	BNE T195
	MVII #2,R0
	ADDI #(array_S-array_Z) AND $FFFF,R3
	MVO@ R0,R3
T195:
	;[974] 		GOTO move_090
	SRCFILE "samples/pumpkin_master.bas",974
	B label_MOVE_090
	;[975] 
	SRCFILE "samples/pumpkin_master.bas",975
	;[976] move_092:
	SRCFILE "samples/pumpkin_master.bas",976
	; MOVE_092
label_MOVE_092:	;[977] 		IF y(c) AND 8 THEN
	SRCFILE "samples/pumpkin_master.bas",977
	MVII #array_Y,R3
	ADD var_C,R3
	MVI@ R3,R0
	ANDI #8,R0
	BEQ T196
	;[978] 			x(c) = x(c) - 1
	SRCFILE "samples/pumpkin_master.bas",978
	ADDI #(array_X-array_Y) AND $FFFF,R3
	MVI@ R3,R0
	DECR R0
	MVO@ R0,R3
	;[979] 			y(c) = y(c) + 1
	SRCFILE "samples/pumpkin_master.bas",979
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
	;[980] 		ELSE
	SRCFILE "samples/pumpkin_master.bas",980
	B T197
T196:
	;[981] 			x(c) = x(c) + 1
	SRCFILE "samples/pumpkin_master.bas",981
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
	;[982] 			y(c) = y(c) + 1
	SRCFILE "samples/pumpkin_master.bas",982
	ADDI #(array_Y-array_X) AND $FFFF,R3
	MVI@ R3,R0
	INCR R0
	MVO@ R0,R3
	;[983] 		END IF
	SRCFILE "samples/pumpkin_master.bas",983
T197:
	;[984] 		IF y(c) = 80 THEN s(c) = 3
	SRCFILE "samples/pumpkin_master.bas",984
	MVII #array_Y,R3
	ADD var_C,R3
	MVI@ R3,R0
	CMPI #80,R0
	BNE T198
	MVII #3,R0
	ADDI #(array_S-array_Y) AND $FFFF,R3
	MVO@ R0,R3
T198:
	;[985] 		GOTO move_090
	SRCFILE "samples/pumpkin_master.bas",985
	B label_MOVE_090
	;[986] 
	SRCFILE "samples/pumpkin_master.bas",986
	;[987] move_093:
	SRCFILE "samples/pumpkin_master.bas",987
	; MOVE_093
label_MOVE_093:	;[988] 		x(c) = x(c) + 2
	SRCFILE "samples/pumpkin_master.bas",988
	MVII #array_X,R3
	ADD var_C,R3
	MVI@ R3,R0
	ADDI #2,R0
	MVO@ R0,R3
	;[989] 		IF x(c) >= 168 THEN s(c) = 0: y(c) = 0
	SRCFILE "samples/pumpkin_master.bas",989
	MVI@ R3,R0
	CMPI #168,R0
	BLT T199
	CLRR R0
	ADDI #(array_S-array_X) AND $FFFF,R3
	MVO@ R0,R3
	ADDI #(array_Y-array_S) AND $FFFF,R3
	MVO@ R0,R3
T199:
	;[990] 		GOTO move_090
	SRCFILE "samples/pumpkin_master.bas",990
	B label_MOVE_090
	;[991] 
	SRCFILE "samples/pumpkin_master.bas",991
	;[992] move_090:
	SRCFILE "samples/pumpkin_master.bas",992
	; MOVE_090
label_MOVE_090:	;[993] 	NEXT c
	SRCFILE "samples/pumpkin_master.bas",993
	MVI var_C,R0
	INCR R0
	MVO R0,var_C
	CMPI #5,R0
	BLE T192
	;[994] 	END
	SRCFILE "samples/pumpkin_master.bas",994
	RETURN
	ENDP
	;[995] 
	SRCFILE "samples/pumpkin_master.bas",995
	;[996] 	'
	SRCFILE "samples/pumpkin_master.bas",996
	;[997] 	' Sine table for curvy movement
	SRCFILE "samples/pumpkin_master.bas",997
	;[998] 	'
	SRCFILE "samples/pumpkin_master.bas",998
	;[999] sin24:
	SRCFILE "samples/pumpkin_master.bas",999
	; SIN24
label_SIN24:	;[1000] 	DATA 0,2,5,7,9,11,13,15
	SRCFILE "samples/pumpkin_master.bas",1000
	DECLE 0
	DECLE 2
	DECLE 5
	DECLE 7
	DECLE 9
	DECLE 11
	DECLE 13
	DECLE 15
	;[1001] 	DATA 17,19,20,21,22,23,24,24
	SRCFILE "samples/pumpkin_master.bas",1001
	DECLE 17
	DECLE 19
	DECLE 20
	DECLE 21
	DECLE 22
	DECLE 23
	DECLE 24
	DECLE 24
	;[1002] 	DATA 24,24,24,23,22,21,20,19
	SRCFILE "samples/pumpkin_master.bas",1002
	DECLE 24
	DECLE 24
	DECLE 24
	DECLE 23
	DECLE 22
	DECLE 21
	DECLE 20
	DECLE 19
	;[1003] 	DATA 17,15,13,11,9,7,5,2
	SRCFILE "samples/pumpkin_master.bas",1003
	DECLE 17
	DECLE 15
	DECLE 13
	DECLE 11
	DECLE 9
	DECLE 7
	DECLE 5
	DECLE 2
	;[1004] 
	SRCFILE "samples/pumpkin_master.bas",1004
	;[1005] 	'
	SRCFILE "samples/pumpkin_master.bas",1005
	;[1006] 	' Table for converting disc direction to 4-way direction
	SRCFILE "samples/pumpkin_master.bas",1006
	;[1007] 	'
	SRCFILE "samples/pumpkin_master.bas",1007
	;[1008] controller_direction:
	SRCFILE "samples/pumpkin_master.bas",1008
	; CONTROLLER_DIRECTION
label_CONTROLLER_DIRECTION:	;[1009] 	DATA 0,3,2,3,1,0,2,0
	SRCFILE "samples/pumpkin_master.bas",1009
	DECLE 0
	DECLE 3
	DECLE 2
	DECLE 3
	DECLE 1
	DECLE 0
	DECLE 2
	DECLE 0
	;[1010] 	DATA 4,4,0,0,1,0,0,0
	SRCFILE "samples/pumpkin_master.bas",1010
	DECLE 4
	DECLE 4
	DECLE 0
	DECLE 0
	DECLE 1
	DECLE 0
	DECLE 0
	DECLE 0
	;[1011] 	DATA 0,3,2,0,0,0,0,0
	SRCFILE "samples/pumpkin_master.bas",1011
	DECLE 0
	DECLE 3
	DECLE 2
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1012] 	DATA 4,0,0,0,1,0,0,0
	SRCFILE "samples/pumpkin_master.bas",1012
	DECLE 4
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 1
	DECLE 0
	DECLE 0
	DECLE 0
	;[1013] 
	SRCFILE "samples/pumpkin_master.bas",1013
	;[1014] 	'
	SRCFILE "samples/pumpkin_master.bas",1014
	;[1015] 	' Bitmaps used for the game
	SRCFILE "samples/pumpkin_master.bas",1015
	;[1016] 	'
	SRCFILE "samples/pumpkin_master.bas",1016
	;[1017] game_bitmaps_0:
	SRCFILE "samples/pumpkin_master.bas",1017
	; GAME_BITMAPS_0
label_GAME_BITMAPS_0:	;[1018] 	BITMAP "........"	' 0 Pumpkin
	SRCFILE "samples/pumpkin_master.bas",1018
	;[1019] 	BITMAP "........"
	SRCFILE "samples/pumpkin_master.bas",1019
	DECLE 0
	;[1020] 	BITMAP "........"
	SRCFILE "samples/pumpkin_master.bas",1020
	;[1021] 	BITMAP ".XX....."
	SRCFILE "samples/pumpkin_master.bas",1021
	DECLE 24576
	;[1022] 	BITMAP "..X....."
	SRCFILE "samples/pumpkin_master.bas",1022
	;[1023] 	BITMAP "...X...."
	SRCFILE "samples/pumpkin_master.bas",1023
	DECLE 4128
	;[1024] 	BITMAP ".XX.XXX."
	SRCFILE "samples/pumpkin_master.bas",1024
	;[1025] 	BITMAP "XX.XX.XX"
	SRCFILE "samples/pumpkin_master.bas",1025
	DECLE 56174
	;[1026] 	BITMAP "X..XX..X"
	SRCFILE "samples/pumpkin_master.bas",1026
	;[1027] 	BITMAP "XXXX.XXX"
	SRCFILE "samples/pumpkin_master.bas",1027
	DECLE 63385
	;[1028] 	BITMAP "XXXXXXXX"
	SRCFILE "samples/pumpkin_master.bas",1028
	;[1029] 	BITMAP "X.XXXX.X"
	SRCFILE "samples/pumpkin_master.bas",1029
	DECLE 48639
	;[1030] 	BITMAP "X..X...X"
	SRCFILE "samples/pumpkin_master.bas",1030
	;[1031] 	BITMAP "XX...XXX"
	SRCFILE "samples/pumpkin_master.bas",1031
	DECLE 51089
	;[1032] 	BITMAP ".XXXXXX."
	SRCFILE "samples/pumpkin_master.bas",1032
	;[1033] 	BITMAP "..XXXX.."
	SRCFILE "samples/pumpkin_master.bas",1033
	DECLE 15486
	;[1034] 
	SRCFILE "samples/pumpkin_master.bas",1034
	;[1035] 	BITMAP "...XX..."	' 2 House
	SRCFILE "samples/pumpkin_master.bas",1035
	;[1036] 	BITMAP "...XX..."
	SRCFILE "samples/pumpkin_master.bas",1036
	DECLE 6168
	;[1037] 	BITMAP "...XX..."
	SRCFILE "samples/pumpkin_master.bas",1037
	;[1038] 	BITMAP "..XXX..."
	SRCFILE "samples/pumpkin_master.bas",1038
	DECLE 14360
	;[1039] 	BITMAP ".X..X..."
	SRCFILE "samples/pumpkin_master.bas",1039
	;[1040] 	BITMAP "X.XX.X.."
	SRCFILE "samples/pumpkin_master.bas",1040
	DECLE 46152
	;[1041] 	BITMAP ".X.XX.X."
	SRCFILE "samples/pumpkin_master.bas",1041
	;[1042] 	BITMAP "XX.XXX.X"
	SRCFILE "samples/pumpkin_master.bas",1042
	DECLE 56666
	;[1043] 	BITMAP "XXXXXXX."
	SRCFILE "samples/pumpkin_master.bas",1043
	;[1044] 	BITMAP "X.XXX.XX"
	SRCFILE "samples/pumpkin_master.bas",1044
	DECLE 48126
	;[1045] 	BITMAP "X.XXX.XX"
	SRCFILE "samples/pumpkin_master.bas",1045
	;[1046] 	BITMAP "XXXXXXXX"
	SRCFILE "samples/pumpkin_master.bas",1046
	DECLE 65467
	;[1047] 	BITMAP "X.XXX.XX"
	SRCFILE "samples/pumpkin_master.bas",1047
	;[1048] 	BITMAP "X.XXX.XX"
	SRCFILE "samples/pumpkin_master.bas",1048
	DECLE 48059
	;[1049] 	BITMAP "XXX.XXXX"
	SRCFILE "samples/pumpkin_master.bas",1049
	;[1050] 	BITMAP "XXX.XXXX"
	SRCFILE "samples/pumpkin_master.bas",1050
	DECLE 61423
	;[1051] 
	SRCFILE "samples/pumpkin_master.bas",1051
	;[1052] 	BITMAP "...X...."	' 4 Pumpkin bullet 1
	SRCFILE "samples/pumpkin_master.bas",1052
	;[1053] 	BITMAP "...XX..."
	SRCFILE "samples/pumpkin_master.bas",1053
	DECLE 6160
	;[1054] 	BITMAP "...XX..."
	SRCFILE "samples/pumpkin_master.bas",1054
	;[1055] 	BITMAP "...XX..."
	SRCFILE "samples/pumpkin_master.bas",1055
	DECLE 6168
	;[1056] 	BITMAP "....X..."
	SRCFILE "samples/pumpkin_master.bas",1056
	;[1057] 	BITMAP "........"
	SRCFILE "samples/pumpkin_master.bas",1057
	DECLE 8
	;[1058] 	BITMAP "........"
	SRCFILE "samples/pumpkin_master.bas",1058
	;[1059] 	BITMAP "........"
	SRCFILE "samples/pumpkin_master.bas",1059
	DECLE 0
	;[1060] 
	SRCFILE "samples/pumpkin_master.bas",1060
	;[1061] 	BITMAP "........"	' 5 Pumpkin bullet 2
	SRCFILE "samples/pumpkin_master.bas",1061
	;[1062] 	BITMAP "........"
	SRCFILE "samples/pumpkin_master.bas",1062
	DECLE 0
	;[1063] 	BITMAP "........"
	SRCFILE "samples/pumpkin_master.bas",1063
	;[1064] 	BITMAP "....X..."
	SRCFILE "samples/pumpkin_master.bas",1064
	DECLE 2048
	;[1065] 	BITMAP "...XX..."
	SRCFILE "samples/pumpkin_master.bas",1065
	;[1066] 	BITMAP "...XX..."
	SRCFILE "samples/pumpkin_master.bas",1066
	DECLE 6168
	;[1067] 	BITMAP "...XX..."
	SRCFILE "samples/pumpkin_master.bas",1067
	;[1068] 	BITMAP "...X...."
	SRCFILE "samples/pumpkin_master.bas",1068
	DECLE 4120
	;[1069] 
	SRCFILE "samples/pumpkin_master.bas",1069
	;[1070] 	BITMAP "X......."	' 6 Pumpkin explosion
	SRCFILE "samples/pumpkin_master.bas",1070
	;[1071] 	BITMAP "X......X"
	SRCFILE "samples/pumpkin_master.bas",1071
	DECLE 33152
	;[1072] 	BITMAP ".XX....."
	SRCFILE "samples/pumpkin_master.bas",1072
	;[1073] 	BITMAP "XX....XX"
	SRCFILE "samples/pumpkin_master.bas",1073
	DECLE 50016
	;[1074] 	BITMAP ".X....X."
	SRCFILE "samples/pumpkin_master.bas",1074
	;[1075] 	BITMAP ".X..X.X."
	SRCFILE "samples/pumpkin_master.bas",1075
	DECLE 19010
	;[1076] 	BITMAP ".X.X...."
	SRCFILE "samples/pumpkin_master.bas",1076
	;[1077] 	BITMAP "........"
	SRCFILE "samples/pumpkin_master.bas",1077
	DECLE 80
	;[1078] 	BITMAP "XX....XX"
	SRCFILE "samples/pumpkin_master.bas",1078
	;[1079] 	BITMAP ".X...X.."
	SRCFILE "samples/pumpkin_master.bas",1079
	DECLE 17603
	;[1080] 	BITMAP "........"
	SRCFILE "samples/pumpkin_master.bas",1080
	;[1081] 	BITMAP "..X..X.."
	SRCFILE "samples/pumpkin_master.bas",1081
	DECLE 9216
	;[1082] 	BITMAP ".XX..XX."
	SRCFILE "samples/pumpkin_master.bas",1082
	;[1083] 	BITMAP "X.....X."
	SRCFILE "samples/pumpkin_master.bas",1083
	DECLE 33382
	;[1084] 	BITMAP ".......X"
	SRCFILE "samples/pumpkin_master.bas",1084
	;[1085] 	BITMAP ".......X"
	SRCFILE "samples/pumpkin_master.bas",1085
	DECLE 257
	;[1086] 
	SRCFILE "samples/pumpkin_master.bas",1086
	;[1087] 	BITMAP "..XXXX.."	' 8 Nuclear mushroom 1
	SRCFILE "samples/pumpkin_master.bas",1087
	;[1088] 	BITMAP ".XXXXXX."
	SRCFILE "samples/pumpkin_master.bas",1088
	DECLE 32316
	;[1089] 	BITMAP "XXXXXXXX"
	SRCFILE "samples/pumpkin_master.bas",1089
	;[1090] 	BITMAP "XXXXXXXX"
	SRCFILE "samples/pumpkin_master.bas",1090
	DECLE 65535
	;[1091] 	BITMAP "X.X.XX.X"
	SRCFILE "samples/pumpkin_master.bas",1091
	;[1092] 	BITMAP "X.XXXX.X"
	SRCFILE "samples/pumpkin_master.bas",1092
	DECLE 48557
	;[1093] 	BITMAP "..XX.X.."
	SRCFILE "samples/pumpkin_master.bas",1093
	;[1094] 	BITMAP "..XXXX.."
	SRCFILE "samples/pumpkin_master.bas",1094
	DECLE 15412
	;[1095] 	BITMAP "..X.XX.."
	SRCFILE "samples/pumpkin_master.bas",1095
	;[1096] 	BITMAP "..XXXX.."
	SRCFILE "samples/pumpkin_master.bas",1096
	DECLE 15404
	;[1097] 	BITMAP "..XX.X.."
	SRCFILE "samples/pumpkin_master.bas",1097
	;[1098] 	BITMAP "..XXXX.."
	SRCFILE "samples/pumpkin_master.bas",1098
	DECLE 15412
	;[1099] 	BITMAP ".XXXXXX."
	SRCFILE "samples/pumpkin_master.bas",1099
	;[1100] 	BITMAP ".X.XXXX."
	SRCFILE "samples/pumpkin_master.bas",1100
	DECLE 24190
	;[1101] 	BITMAP "XXXXX.XX"
	SRCFILE "samples/pumpkin_master.bas",1101
	;[1102] 	BITMAP "XXXXXXXX"
	SRCFILE "samples/pumpkin_master.bas",1102
	DECLE 65531
	;[1103] 
	SRCFILE "samples/pumpkin_master.bas",1103
	;[1104] 	BITMAP "..XXXX.."	' 10 Nuclear mushroom 2
	SRCFILE "samples/pumpkin_master.bas",1104
	;[1105] 	BITMAP ".XXXXXX."
	SRCFILE "samples/pumpkin_master.bas",1105
	DECLE 32316
	;[1106] 	BITMAP "XXXXXXXX"
	SRCFILE "samples/pumpkin_master.bas",1106
	;[1107] 	BITMAP "XXXX.XXX"
	SRCFILE "samples/pumpkin_master.bas",1107
	DECLE 63487
	;[1108] 	BITMAP "X.XXXX.X"
	SRCFILE "samples/pumpkin_master.bas",1108
	;[1109] 	BITMAP "X.X.XX.X"
	SRCFILE "samples/pumpkin_master.bas",1109
	DECLE 44477
	;[1110] 	BITMAP "..XXXX.."
	SRCFILE "samples/pumpkin_master.bas",1110
	;[1111] 	BITMAP "..XX.X.."
	SRCFILE "samples/pumpkin_master.bas",1111
	DECLE 13372
	;[1112] 	BITMAP "..XXXX.."
	SRCFILE "samples/pumpkin_master.bas",1112
	;[1113] 	BITMAP "..X.XX.."
	SRCFILE "samples/pumpkin_master.bas",1113
	DECLE 11324
	;[1114] 	BITMAP "..XXXX.."
	SRCFILE "samples/pumpkin_master.bas",1114
	;[1115] 	BITMAP "..XXXX.."
	SRCFILE "samples/pumpkin_master.bas",1115
	DECLE 15420
	;[1116] 	BITMAP ".XXXX.X."
	SRCFILE "samples/pumpkin_master.bas",1116
	;[1117] 	BITMAP ".X.XXXX."
	SRCFILE "samples/pumpkin_master.bas",1117
	DECLE 24186
	;[1118] 	BITMAP "XXXX.XXX"
	SRCFILE "samples/pumpkin_master.bas",1118
	;[1119] 	BITMAP "XXXXXXXX"
	SRCFILE "samples/pumpkin_master.bas",1119
	DECLE 65527
	;[1120] 
	SRCFILE "samples/pumpkin_master.bas",1120
	;[1121] 	BITMAP ".X......"	' 12 Player bullet
	SRCFILE "samples/pumpkin_master.bas",1121
	;[1122] 	BITMAP "XX......"
	SRCFILE "samples/pumpkin_master.bas",1122
	DECLE 49216
	;[1123] 	BITMAP "XX......"
	SRCFILE "samples/pumpkin_master.bas",1123
	;[1124] 	BITMAP "XX......"
	SRCFILE "samples/pumpkin_master.bas",1124
	DECLE 49344
	;[1125] 	BITMAP "XXX....."
	SRCFILE "samples/pumpkin_master.bas",1125
	;[1126] 	BITMAP "XXX....."
	SRCFILE "samples/pumpkin_master.bas",1126
	DECLE 57568
	;[1127] 	BITMAP ".XX....."
	SRCFILE "samples/pumpkin_master.bas",1127
	;[1128] 	BITMAP ".X......"
	SRCFILE "samples/pumpkin_master.bas",1128
	DECLE 16480
	;[1129] 
	SRCFILE "samples/pumpkin_master.bas",1129
	;[1130] 	' Based on https://pixabay.com/es/pumpkin-helloween-witch-bruja-3726795/
	SRCFILE "samples/pumpkin_master.bas",1130
	;[1131] 	REM IntyColor v1.1.5 Jul/25/2017
	SRCFILE "samples/pumpkin_master.bas",1131
	;[1132] 	REM Command: ./intycolor -b -n -s0000 pumpkin.bmp pumpkin_title.bas pumpkin 
	SRCFILE "samples/pumpkin_master.bas",1132
	;[1133] 	REM Created: Sun Oct 28 21:28:30 2018
	SRCFILE "samples/pumpkin_master.bas",1133
	;[1134] 
	SRCFILE "samples/pumpkin_master.bas",1134
	;[1135] 	' 59 bitmaps
	SRCFILE "samples/pumpkin_master.bas",1135
	;[1136] pumpkin_bitmaps_0:
	SRCFILE "samples/pumpkin_master.bas",1136
	; PUMPKIN_BITMAPS_0
label_PUMPKIN_BITMAPS_0:	;[1137] 	DATA $0000,$0000,$0000,$0600
	SRCFILE "samples/pumpkin_master.bas",1137
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 1536
	;[1138] 	DATA $667C,$6666,$607C,$6060
	SRCFILE "samples/pumpkin_master.bas",1138
	DECLE 26236
	DECLE 26214
	DECLE 24700
	DECLE 24672
	;[1139] 	DATA $0000,$6666,$6666,$3E66
	SRCFILE "samples/pumpkin_master.bas",1139
	DECLE 0
	DECLE 26214
	DECLE 26214
	DECLE 15974
	;[1140] 	DATA $0000,$667D,$6666,$6666
	SRCFILE "samples/pumpkin_master.bas",1140
	DECLE 0
	DECLE 26237
	DECLE 26214
	DECLE 26214
	;[1141] 	DATA $0000,$66C7,$6666,$6766
	SRCFILE "samples/pumpkin_master.bas",1141
	DECLE 0
	DECLE 26311
	DECLE 26214
	DECLE 26470
	;[1142] 	DATA $0606,$66C6,$6767,$C666
	SRCFILE "samples/pumpkin_master.bas",1142
	DECLE 1542
	DECLE 26310
	DECLE 26471
	DECLE 50790
	;[1143] 	DATA $0006,$C666,$8686,$66C6
	SRCFILE "samples/pumpkin_master.bas",1143
	DECLE 6
	DECLE 50790
	DECLE 34438
	DECLE 26310
	;[1144] 	DATA $0000,$667C,$6666,$6666
	SRCFILE "samples/pumpkin_master.bas",1144
	DECLE 0
	DECLE 26236
	DECLE 26214
	DECLE 26214
	;[1145] 	DATA $0706,$0507,$0404,$0404
	SRCFILE "samples/pumpkin_master.bas",1145
	DECLE 1798
	DECLE 1287
	DECLE 1028
	DECLE 1028
	;[1146] 	DATA $1808,$D8B8,$1998,$1819
	SRCFILE "samples/pumpkin_master.bas",1146
	DECLE 6152
	DECLE 55480
	DECLE 6552
	DECLE 6169
	;[1147] 	DATA $0000,$19F0,$98F9,$F998
	SRCFILE "samples/pumpkin_master.bas",1147
	DECLE 0
	DECLE 6640
	DECLE 39161
	DECLE 63896
	;[1148] 	DATA $0303,$83F7,$F3E3,$E133
	SRCFILE "samples/pumpkin_master.bas",1148
	DECLE 771
	DECLE 33783
	DECLE 62435
	DECLE 57651
	;[1149] 	DATA $0000,$19CF,$181F,$CF18
	SRCFILE "samples/pumpkin_master.bas",1149
	DECLE 0
	DECLE 6607
	DECLE 6175
	DECLE 53016
	;[1150] 	DATA $0000,$9F1B,$1898,$1898
	SRCFILE "samples/pumpkin_master.bas",1150
	DECLE 0
	DECLE 40731
	DECLE 6296
	DECLE 6296
	;[1151] 	DATA $0606,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1151
	DECLE 1542
	DECLE 0
	DECLE 0
	DECLE 0
	;[1152] 	DATA $0100,$0003,$0300,$0101
	SRCFILE "samples/pumpkin_master.bas",1152
	DECLE 256
	DECLE 3
	DECLE 768
	DECLE 257
	;[1153] pumpkin_bitmaps_1:
	SRCFILE "samples/pumpkin_master.bas",1153
	; PUMPKIN_BITMAPS_1
label_PUMPKIN_BITMAPS_1:	;[1154] 	DATA $E000,$1810,$FC68,$B868
	SRCFILE "samples/pumpkin_master.bas",1154
	DECLE 57344
	DECLE 6160
	DECLE 64616
	DECLE 47208
	;[1155] 	DATA $60C0,$1830,$070C,$0303
	SRCFILE "samples/pumpkin_master.bas",1155
	DECLE 24768
	DECLE 6192
	DECLE 1804
	DECLE 771
	;[1156] 	DATA $0000,$1800,$F8F0,$CEFB
	SRCFILE "samples/pumpkin_master.bas",1156
	DECLE 0
	DECLE 6144
	DECLE 63728
	DECLE 52987
	;[1157] 	DATA $0000,$0000,$0000,$70F0
	SRCFILE "samples/pumpkin_master.bas",1157
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 28912
	;[1158] 	DATA $0100,$0301,$0101,$0000
	SRCFILE "samples/pumpkin_master.bas",1158
	DECLE 256
	DECLE 769
	DECLE 257
	DECLE 0
	;[1159] 	DATA $83FE,$B0E0,$F0D0,$3F6F
	SRCFILE "samples/pumpkin_master.bas",1159
	DECLE 33790
	DECLE 45280
	DECLE 61648
	DECLE 16239
	;[1160] 	DATA $8000,$3CE0,$0007,$9FE7
	SRCFILE "samples/pumpkin_master.bas",1160
	DECLE 32768
	DECLE 15584
	DECLE 7
	DECLE 40935
	;[1161] 	DATA $0706,$0F0F,$6100,$BFDE
	SRCFILE "samples/pumpkin_master.bas",1161
	DECLE 1798
	DECLE 3855
	DECLE 24832
	DECLE 49118
	;[1162] 	DATA $D18C,$6071,$C020,$7EF9
	SRCFILE "samples/pumpkin_master.bas",1162
	DECLE 53644
	DECLE 24689
	DECLE 49184
	DECLE 32505
	;[1163] 	DATA $F8F8,$00E0,$0000,$7CE0
	SRCFILE "samples/pumpkin_master.bas",1163
	DECLE 63736
	DECLE 224
	DECLE 0
	DECLE 31968
	;[1164] 	DATA $0100,$0303,$0707,$1F0F
	SRCFILE "samples/pumpkin_master.bas",1164
	DECLE 256
	DECLE 771
	DECLE 1799
	DECLE 7951
	;[1165] 	DATA $FEFF,$FBFD,$EFF7,$DFDF
	SRCFILE "samples/pumpkin_master.bas",1165
	DECLE 65279
	DECLE 64509
	DECLE 61431
	DECLE 57311
	;[1166] 	DATA $FE7F,$FEFE,$FDFD,$FBFD
	SRCFILE "samples/pumpkin_master.bas",1166
	DECLE 65151
	DECLE 65278
	DECLE 65021
	DECLE 64509
	;[1167] 	DATA $FF7F,$FFFF,$FFFF,$FFFF
	SRCFILE "samples/pumpkin_master.bas",1167
	DECLE 65407
	DECLE 65535
	DECLE 65535
	DECLE 65535
	;[1168] 	DATA $BFBF,$DFDF,$EFEF,$F7EF
	SRCFILE "samples/pumpkin_master.bas",1168
	DECLE 49087
	DECLE 57311
	DECLE 61423
	DECLE 63471
	;[1169] 	DATA $DFBF,$F7EF,$FDFB,$FEFE
	SRCFILE "samples/pumpkin_master.bas",1169
	DECLE 57279
	DECLE 63471
	DECLE 65019
	DECLE 65278
	;[1170] pumpkin_bitmaps_2:
	SRCFILE "samples/pumpkin_master.bas",1170
	; PUMPKIN_BITMAPS_2
label_PUMPKIN_BITMAPS_2:	;[1171] 	DATA $C000,$F0E0,$F8F0,$FCFC
	SRCFILE "samples/pumpkin_master.bas",1171
	DECLE 49152
	DECLE 61664
	DECLE 63728
	DECLE 64764
	;[1172] 	DATA $1F1F,$3F3F,$3F3F,$3E3E
	SRCFILE "samples/pumpkin_master.bas",1172
	DECLE 7967
	DECLE 16191
	DECLE 16191
	DECLE 15934
	;[1173] 	DATA $BFBF,$7F7F,$7F7F,$FFFF
	SRCFILE "samples/pumpkin_master.bas",1173
	DECLE 49087
	DECLE 32639
	DECLE 32639
	DECLE 65535
	;[1174] 	DATA $9BBB,$838B,$8081,$E0C0
	SRCFILE "samples/pumpkin_master.bas",1174
	DECLE 39867
	DECLE 33675
	DECLE 32897
	DECLE 57536
	;[1175] 	DATA $FFFF,$FFFF,$7FFF,$FF3F
	SRCFILE "samples/pumpkin_master.bas",1175
	DECLE 65535
	DECLE 65535
	DECLE 32767
	DECLE 65343
	;[1176] 	DATA $F6F7,$F0F4,$00C0,$C080
	SRCFILE "samples/pumpkin_master.bas",1176
	DECLE 63223
	DECLE 61684
	DECLE 192
	DECLE 49280
	;[1177] 	DATA $3FFF,$3F3F,$7F7F,$FF7F
	SRCFILE "samples/pumpkin_master.bas",1177
	DECLE 16383
	DECLE 16191
	DECLE 32639
	DECLE 65407
	;[1178] 	DATA $7E7E,$BEBE,$DEBE,$DEDE
	SRCFILE "samples/pumpkin_master.bas",1178
	DECLE 32382
	DECLE 48830
	DECLE 57022
	DECLE 57054
	;[1179] 	DATA $3E3E,$1E3E,$1E1E,$0F1E
	SRCFILE "samples/pumpkin_master.bas",1179
	DECLE 15934
	DECLE 7742
	DECLE 7710
	DECLE 3870
	;[1180] 	DATA $FFFF,$FFFF,$FFFF,$78F9
	SRCFILE "samples/pumpkin_master.bas",1180
	DECLE 65535
	DECLE 65535
	DECLE 65535
	DECLE 30969
	;[1181] 	DATA $F7F7,$F7F7,$F7F7,$F7F7
	SRCFILE "samples/pumpkin_master.bas",1181
	DECLE 63479
	DECLE 63479
	DECLE 63479
	DECLE 63479
	;[1182] 	DATA $FFFF,$F7F7,$E3E3,$FFFF
	SRCFILE "samples/pumpkin_master.bas",1182
	DECLE 65535
	DECLE 63479
	DECLE 58339
	DECLE 65535
	;[1183] 	DATA $FBFB,$FBFB,$FBFB,$FBFB
	SRCFILE "samples/pumpkin_master.bas",1183
	DECLE 64507
	DECLE 64507
	DECLE 64507
	DECLE 64507
	;[1184] 	DATA $FFFF,$FFFF,$FFFF,$CFEF
	SRCFILE "samples/pumpkin_master.bas",1184
	DECLE 65535
	DECLE 65535
	DECLE 65535
	DECLE 53231
	;[1185] 	DATA $DEDE,$DEDE,$DCDC,$BCDC
	SRCFILE "samples/pumpkin_master.bas",1185
	DECLE 57054
	DECLE 57054
	DECLE 56540
	DECLE 48348
	;[1186] 	DATA $070F,$0707,$0103,$0001
	SRCFILE "samples/pumpkin_master.bas",1186
	DECLE 1807
	DECLE 1799
	DECLE 259
	DECLE 1
	;[1187] pumpkin_bitmaps_3:
	SRCFILE "samples/pumpkin_master.bas",1187
	; PUMPKIN_BITMAPS_3
label_PUMPKIN_BITMAPS_3:	;[1188] 	DATA $7C7C,$BEBC,$DFDE,$F7EF
	SRCFILE "samples/pumpkin_master.bas",1188
	DECLE 31868
	DECLE 48828
	DECLE 57310
	DECLE 63471
	;[1189] 	DATA $3737,$3839,$0018,$E080
	SRCFILE "samples/pumpkin_master.bas",1189
	DECLE 14135
	DECLE 14393
	DECLE 24
	DECLE 57472
	;[1190] 	DATA $FFFF,$00FF,$0000,$1E00
	SRCFILE "samples/pumpkin_master.bas",1190
	DECLE 65535
	DECLE 255
	DECLE 0
	DECLE 7680
	;[1191] 	DATA $F8FB,$00C0,$0000,$6F28
	SRCFILE "samples/pumpkin_master.bas",1191
	DECLE 63739
	DECLE 192
	DECLE 0
	DECLE 28456
	;[1192] 	DATA $0F0F,$1F1F,$7E3E,$FBFD
	SRCFILE "samples/pumpkin_master.bas",1192
	DECLE 3855
	DECLE 7967
	DECLE 32318
	DECLE 64509
	;[1193] 	DATA $B8B8,$7070,$E0E0,$80C0
	SRCFILE "samples/pumpkin_master.bas",1193
	DECLE 47288
	DECLE 28784
	DECLE 57568
	DECLE 32960
	;[1194] 	DATA $3D7B,$0F1E,$0003,$0000
	SRCFILE "samples/pumpkin_master.bas",1194
	DECLE 15739
	DECLE 3870
	DECLE 3
	DECLE 0
	;[1195] 	DATA $FDFC,$7EFE,$E79F,$0118
	SRCFILE "samples/pumpkin_master.bas",1195
	DECLE 65020
	DECLE 32510
	DECLE 59295
	DECLE 280
	;[1196] 	DATA $9F1E,$FFFF,$BF7F,$00DE
	SRCFILE "samples/pumpkin_master.bas",1196
	DECLE 40734
	DECLE 65535
	DECLE 49023
	DECLE 222
	;[1197] 	DATA $DF6F,$DFDF,$79BE,$20C6
	SRCFILE "samples/pumpkin_master.bas",1197
	DECLE 57199
	DECLE 57311
	DECLE 31166
	DECLE 8390
	;[1198] 	DATA $EEF7,$B8DC,$8060,$0000
	SRCFILE "samples/pumpkin_master.bas",1198
	DECLE 61175
	DECLE 47324
	DECLE 32864
	DECLE 0
	;[1199] 
	SRCFILE "samples/pumpkin_master.bas",1199
	;[1200] 	REM 20x12 cards
	SRCFILE "samples/pumpkin_master.bas",1200
	;[1201] pumpkin_cards:
	SRCFILE "samples/pumpkin_master.bas",1201
	; PUMPKIN_CARDS
label_PUMPKIN_CARDS:	;[1202] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$1806,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1202
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 6150
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1203] 	DATA $0000,$0000,$0000,$180E,$1816,$181E,$1826,$182E,$1836,$183E,$1846,$184E,$1856,$185E,$1866,$186E,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1203
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 6158
	DECLE 6166
	DECLE 6174
	DECLE 6182
	DECLE 6190
	DECLE 6198
	DECLE 6206
	DECLE 6214
	DECLE 6222
	DECLE 6230
	DECLE 6238
	DECLE 6246
	DECLE 6254
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1204] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$1876,$0000,$087D,$0885,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1204
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 6262
	DECLE 0
	DECLE 2173
	DECLE 2181
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1205] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$088D,$0895,$089D,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1205
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 2189
	DECLE 2197
	DECLE 2205
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1206] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$08A5,$08AD,$08B5,$08BD,$08C5,$08CD,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1206
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 2213
	DECLE 2221
	DECLE 2229
	DECLE 2237
	DECLE 2245
	DECLE 2253
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1207] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$18D2,$18DA,$18E2,$18EA,$18F2,$18FA,$1902,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1207
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 6354
	DECLE 6362
	DECLE 6370
	DECLE 6378
	DECLE 6386
	DECLE 6394
	DECLE 6402
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1208] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$190A,$1912,$191A,$1922,$192A,$1932,$193A,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1208
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 6410
	DECLE 6418
	DECLE 6426
	DECLE 6434
	DECLE 6442
	DECLE 6450
	DECLE 6458
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1209] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$1942,$194A,$1952,$195A,$1962,$196A,$1972,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1209
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 6466
	DECLE 6474
	DECLE 6482
	DECLE 6490
	DECLE 6498
	DECLE 6506
	DECLE 6514
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1210] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$197A,$1982,$198A,$1992,$199A,$19A2,$19AA,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1210
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 6522
	DECLE 6530
	DECLE 6538
	DECLE 6546
	DECLE 6554
	DECLE 6562
	DECLE 6570
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1211] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$19B2,$19BA,$19C2,$19CA,$19D2,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1211
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 6578
	DECLE 6586
	DECLE 6594
	DECLE 6602
	DECLE 6610
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1212] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1212
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1213] 	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	SRCFILE "samples/pumpkin_master.bas",1213
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	DECLE 0
	;[1214] 
	SRCFILE "samples/pumpkin_master.bas",1214
	;[1215] 	'
	SRCFILE "samples/pumpkin_master.bas",1215
	;[1216] 	' Play sound effects
	SRCFILE "samples/pumpkin_master.bas",1216
	;[1217] 	'
	SRCFILE "samples/pumpkin_master.bas",1217
	;[1218] play_sound:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",1218
	; PLAY_SOUND
label_PLAY_SOUND:	PROC
	BEGIN
	;[1219] 	ON sound_effect GOSUB play_none, play_fire, play_drop, play_explosion_1, play_explosion_2
	SRCFILE "samples/pumpkin_master.bas",1219
	MVI var_SOUND_EFFECT,R1
	CMPI #5,R1
	BC T201
	MVII #T201,R5
	ADDI #T200,R1
	MVI@ R1,PC
T200:
	DECLE label_PLAY_NONE
	DECLE label_PLAY_FIRE
	DECLE label_PLAY_DROP
	DECLE label_PLAY_EXPLOSION_1
	DECLE label_PLAY_EXPLOSION_2
T201:
	;[1220] 	END
	SRCFILE "samples/pumpkin_master.bas",1220
	RETURN
	ENDP
	;[1221] 
	SRCFILE "samples/pumpkin_master.bas",1221
	;[1222] play_none:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",1222
	; PLAY_NONE
label_PLAY_NONE:	PROC
	BEGIN
	;[1223] 	SOUND 2,,0
	SRCFILE "samples/pumpkin_master.bas",1223
	CLRR R0
	MVO R0,509
	;[1224] 	SOUND 4,,$38
	SRCFILE "samples/pumpkin_master.bas",1224
	MVII #56,R0
	MVO R0,504
	;[1225] 	END
	SRCFILE "samples/pumpkin_master.bas",1225
	RETURN
	ENDP
	;[1226] 
	SRCFILE "samples/pumpkin_master.bas",1226
	;[1227] play_fire:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",1227
	; PLAY_FIRE
label_PLAY_FIRE:	PROC
	BEGIN
	;[1228] 	SOUND 2,200 - sound_state * sound_state,12
	SRCFILE "samples/pumpkin_master.bas",1228
	MVII #200,R0
	MVI var_SOUND_STATE,R1
	MOVR R1,R4
	MVO R1,40838
	MVO R4,40839
	MVI 40846,R1
	SUBR R1,R0
	MVO R0,498
	SWAP R0
	MVO R0,502
	MVII #12,R0
	MVO R0,509
	;[1229] 	SOUND 4,,$38
	SRCFILE "samples/pumpkin_master.bas",1229
	MVII #56,R0
	MVO R0,504
	;[1230] 	sound_state = sound_state + 1
	SRCFILE "samples/pumpkin_master.bas",1230
	MVI var_SOUND_STATE,R0
	INCR R0
	MVO R0,var_SOUND_STATE
	;[1231] 	IF sound_state = 10 THEN sound_effect = 0
	SRCFILE "samples/pumpkin_master.bas",1231
	MVI var_SOUND_STATE,R0
	CMPI #10,R0
	BNE T202
	CLRR R0
	MVO R0,var_SOUND_EFFECT
T202:
	;[1232] 	END
	SRCFILE "samples/pumpkin_master.bas",1232
	RETURN
	ENDP
	;[1233] 
	SRCFILE "samples/pumpkin_master.bas",1233
	;[1234] play_drop:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",1234
	; PLAY_DROP
label_PLAY_DROP:	PROC
	BEGIN
	;[1235] 	SOUND 2,100 + (RAND AND 1) + sound_state * sound_state,12
	SRCFILE "samples/pumpkin_master.bas",1235
	MVI _rand,R0
	ANDI #1,R0
	ADDI #100,R0
	MVI var_SOUND_STATE,R1
	MOVR R1,R4
	MVO R1,40838
	MVO R4,40839
	MVI 40846,R1
	ADDR R1,R0
	MVO R0,498
	SWAP R0
	MVO R0,502
	MVII #12,R0
	MVO R0,509
	;[1236] 	SOUND 4,,$38
	SRCFILE "samples/pumpkin_master.bas",1236
	MVII #56,R0
	MVO R0,504
	;[1237] 	sound_state = sound_state + 1
	SRCFILE "samples/pumpkin_master.bas",1237
	MVI var_SOUND_STATE,R0
	INCR R0
	MVO R0,var_SOUND_STATE
	;[1238] 	IF sound_state = 10 THEN sound_effect = 0
	SRCFILE "samples/pumpkin_master.bas",1238
	MVI var_SOUND_STATE,R0
	CMPI #10,R0
	BNE T203
	CLRR R0
	MVO R0,var_SOUND_EFFECT
T203:
	;[1239] 	END
	SRCFILE "samples/pumpkin_master.bas",1239
	RETURN
	ENDP
	;[1240] 
	SRCFILE "samples/pumpkin_master.bas",1240
	;[1241] play_explosion_1:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",1241
	; PLAY_EXPLOSION_1
label_PLAY_EXPLOSION_1:	PROC
	BEGIN
	;[1242] 	SOUND 2,1000+sound_state * 16,12 - sound_state / 4
	SRCFILE "samples/pumpkin_master.bas",1242
	MVI var_SOUND_STATE,R0
	SLL R0,2
	SLL R0,2
	ADDI #1000,R0
	MVO R0,498
	SWAP R0
	MVO R0,502
	MVII #12,R0
	MVI var_SOUND_STATE,R1
	SLR R1,2
	SUBR R1,R0
	MVO R0,509
	;[1243] 	SOUND 4,,$38
	SRCFILE "samples/pumpkin_master.bas",1243
	MVII #56,R0
	MVO R0,504
	;[1244] 	sound_state = sound_state + 1
	SRCFILE "samples/pumpkin_master.bas",1244
	MVI var_SOUND_STATE,R0
	INCR R0
	MVO R0,var_SOUND_STATE
	;[1245] 	IF sound_state = 24 THEN sound_effect = 0
	SRCFILE "samples/pumpkin_master.bas",1245
	MVI var_SOUND_STATE,R0
	CMPI #24,R0
	BNE T204
	CLRR R0
	MVO R0,var_SOUND_EFFECT
T204:
	;[1246] 	END
	SRCFILE "samples/pumpkin_master.bas",1246
	RETURN
	ENDP
	;[1247] 
	SRCFILE "samples/pumpkin_master.bas",1247
	;[1248] play_explosion_2:	PROCEDURE
	SRCFILE "samples/pumpkin_master.bas",1248
	; PLAY_EXPLOSION_2
label_PLAY_EXPLOSION_2:	PROC
	BEGIN
	;[1249] 	SOUND 2,2000-sound_state*16,12 - sound_state / 8
	SRCFILE "samples/pumpkin_master.bas",1249
	MVII #2000,R0
	MVI var_SOUND_STATE,R1
	SLL R1,2
	SLL R1,2
	SUBR R1,R0
	MVO R0,498
	SWAP R0
	MVO R0,502
	MVII #12,R0
	MVI var_SOUND_STATE,R1
	SLR R1,2
	SLR R1,1
	SUBR R1,R0
	MVO R0,509
	;[1250] 	SOUND 4,31-sound_state/4,$18
	SRCFILE "samples/pumpkin_master.bas",1250
	MVII #31,R0
	MVI var_SOUND_STATE,R1
	SLR R1,2
	SUBR R1,R0
	MVO R0,505
	MVII #24,R0
	MVO R0,504
	;[1251] 	sound_state = sound_state + 1
	SRCFILE "samples/pumpkin_master.bas",1251
	MVI var_SOUND_STATE,R0
	INCR R0
	MVO R0,var_SOUND_STATE
	;[1252] 	IF sound_state = 96 THEN sound_effect = 0
	SRCFILE "samples/pumpkin_master.bas",1252
	MVI var_SOUND_STATE,R0
	CMPI #96,R0
	BNE T205
	CLRR R0
	MVO R0,var_SOUND_EFFECT
T205:
	;[1253] 	END
	SRCFILE "samples/pumpkin_master.bas",1253
	RETURN
	ENDP
	;[1254] 	
	SRCFILE "samples/pumpkin_master.bas",1254
	;[1255] 	ASM ORG $F000
	SRCFILE "samples/pumpkin_master.bas",1255
 ORG $F000
	;[1256] 
	SRCFILE "samples/pumpkin_master.bas",1256
	;[1257] 	'
	SRCFILE "samples/pumpkin_master.bas",1257
	;[1258] 	' Pumpkin Boogie
	SRCFILE "samples/pumpkin_master.bas",1258
	;[1259] 	' 
	SRCFILE "samples/pumpkin_master.bas",1259
	;[1260] 	' Automagically generated by boogie.c srand(1540785469)
	SRCFILE "samples/pumpkin_master.bas",1260
	;[1261] music_game:
	SRCFILE "samples/pumpkin_master.bas",1261
	; MUSIC_GAME
label_MUSIC_GAME:	;[1262] 	DATA 6
	SRCFILE "samples/pumpkin_master.bas",1262
	DECLE 6
	;[1263] 	MUSIC C3W,C5W,-,-
	SRCFILE "samples/pumpkin_master.bas",1263
	DECLE 9485,0
	;[1264] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1264
	DECLE 6413,0
	;[1265] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1265
	DECLE 16191,0
	;[1266] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1266
	DECLE 8720,0
	;[1267] 	MUSIC E3,F4#,-,-
	SRCFILE "samples/pumpkin_master.bas",1267
	DECLE 7953,0
	;[1268] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1268
	DECLE 16191,0
	;[1269] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1269
	DECLE 8212,0
	;[1270] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1270
	DECLE 6413,0
	;[1271] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1271
	DECLE 16191,0
	;[1272] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1272
	DECLE 22,0
	;[1273] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1273
	DECLE 20,0
	;[1274] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1274
	DECLE 63,0
	;[1275] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1275
	DECLE 7698,0
	;[1276] 	MUSIC F3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1276
	DECLE 9490,0
	;[1277] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1277
	DECLE 16191,0
	;[1278] 	MUSIC G3#,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1278
	DECLE 10005,0
	;[1279] 	MUSIC A3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1279
	DECLE 9494,0
	;[1280] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1280
	DECLE 16191,0
	;[1281] 	MUSIC C4,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1281
	DECLE 7705,0
	;[1282] 	MUSIC F3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1282
	DECLE 18,0
	;[1283] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1283
	DECLE 63,0
	;[1284] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1284
	DECLE 27,0
	;[1285] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1285
	DECLE 25,0
	;[1286] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1286
	DECLE 63,0
	;[1287] 	MUSIC G3,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1287
	DECLE 12308,0
	;[1288] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1288
	DECLE 20,0
	;[1289] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1289
	DECLE 63,0
	;[1290] 	MUSIC A3#,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1290
	DECLE 12311,0
	;[1291] 	MUSIC B3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1291
	DECLE 24,0
	;[1292] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1292
	DECLE 63,0
	;[1293] 	MUSIC D4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1293
	DECLE 12315,0
	;[1294] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1294
	DECLE 20,0
	;[1295] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1295
	DECLE 63,0
	;[1296] 	MUSIC E4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1296
	DECLE 12317,0
	;[1297] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1297
	DECLE 27,0
	;[1298] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1298
	DECLE 63,0
	;[1299] 	MUSIC F3,F5,-,-
	SRCFILE "samples/pumpkin_master.bas",1299
	DECLE 10770,0
	;[1300] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1300
	DECLE 7698,0
	;[1301] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1301
	DECLE 16191,0
	;[1302] 	MUSIC G3#,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1302
	DECLE 10005,0
	;[1303] 	MUSIC A3,B4,-,-
	SRCFILE "samples/pumpkin_master.bas",1303
	DECLE 9238,0
	;[1304] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1304
	DECLE 16191,0
	;[1305] 	MUSIC C4,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1305
	DECLE 9497,0
	;[1306] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1306
	DECLE 7698,0
	;[1307] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1307
	DECLE 16191,0
	;[1308] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1308
	DECLE 27,0
	;[1309] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1309
	DECLE 25,0
	;[1310] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1310
	DECLE 63,0
	;[1311] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1311
	DECLE 9485,0
	;[1312] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1312
	DECLE 6413,0
	;[1313] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1313
	DECLE 16191,0
	;[1314] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1314
	DECLE 8720,0
	;[1315] 	MUSIC E3,F4#,-,-
	SRCFILE "samples/pumpkin_master.bas",1315
	DECLE 7953,0
	;[1316] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1316
	DECLE 16191,0
	;[1317] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1317
	DECLE 8212,0
	;[1318] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1318
	DECLE 6413,0
	;[1319] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1319
	DECLE 16191,0
	;[1320] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1320
	DECLE 22,0
	;[1321] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1321
	DECLE 20,0
	;[1322] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1322
	DECLE 63,0
	;[1323] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1323
	DECLE 13,0
	;[1324] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1324
	DECLE 13,0
	;[1325] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1325
	DECLE 63,0
	;[1326] 	MUSIC D3#,-,-
	SRCFILE "samples/pumpkin_master.bas",1326
	DECLE 16,0
	;[1327] 	MUSIC E3,-,-
	SRCFILE "samples/pumpkin_master.bas",1327
	DECLE 17,0
	;[1328] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1328
	DECLE 63,0
	;[1329] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1329
	DECLE 20,0
	;[1330] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1330
	DECLE 13,0
	;[1331] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1331
	DECLE 63,0
	;[1332] 	MUSIC A3,-,-
	SRCFILE "samples/pumpkin_master.bas",1332
	DECLE 22,0
	;[1333] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1333
	DECLE 20,0
	;[1334] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1334
	DECLE 63,0
	;[1335] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1335
	DECLE 10509,0
	;[1336] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1336
	DECLE 13,0
	;[1337] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1337
	DECLE 63,0
	;[1338] 	MUSIC D3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1338
	DECLE 10512,0
	;[1339] 	MUSIC E3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1339
	DECLE 17,0
	;[1340] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1340
	DECLE 63,0
	;[1341] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1341
	DECLE 10516,0
	;[1342] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1342
	DECLE 13,0
	;[1343] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1343
	DECLE 63,0
	;[1344] 	MUSIC A3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1344
	DECLE 10518,0
	;[1345] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1345
	DECLE 20,0
	;[1346] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1346
	DECLE 63,0
	;[1347] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1347
	DECLE 13,0
	;[1348] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1348
	DECLE 13,0
	;[1349] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1349
	DECLE 63,0
	;[1350] 	MUSIC D3#,-,-
	SRCFILE "samples/pumpkin_master.bas",1350
	DECLE 16,0
	;[1351] 	MUSIC E3,-,-
	SRCFILE "samples/pumpkin_master.bas",1351
	DECLE 17,0
	;[1352] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1352
	DECLE 63,0
	;[1353] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1353
	DECLE 20,0
	;[1354] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1354
	DECLE 13,0
	;[1355] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1355
	DECLE 63,0
	;[1356] 	MUSIC A3,-,-
	SRCFILE "samples/pumpkin_master.bas",1356
	DECLE 22,0
	;[1357] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1357
	DECLE 20,0
	;[1358] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1358
	DECLE 63,0
	;[1359] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1359
	DECLE 9485,0
	;[1360] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1360
	DECLE 6413,0
	;[1361] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1361
	DECLE 16191,0
	;[1362] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1362
	DECLE 8720,0
	;[1363] 	MUSIC E3,F4#,-,-
	SRCFILE "samples/pumpkin_master.bas",1363
	DECLE 7953,0
	;[1364] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1364
	DECLE 16191,0
	;[1365] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1365
	DECLE 8212,0
	;[1366] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1366
	DECLE 6413,0
	;[1367] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1367
	DECLE 16191,0
	;[1368] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1368
	DECLE 22,0
	;[1369] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1369
	DECLE 20,0
	;[1370] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1370
	DECLE 63,0
	;[1371] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1371
	DECLE 10509,0
	;[1372] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1372
	DECLE 13,0
	;[1373] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1373
	DECLE 63,0
	;[1374] 	MUSIC D3#,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1374
	DECLE 16,0
	;[1375] 	MUSIC E3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1375
	DECLE 17,0
	;[1376] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1376
	DECLE 63,0
	;[1377] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1377
	DECLE 10516,0
	;[1378] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1378
	DECLE 13,0
	;[1379] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1379
	DECLE 63,0
	;[1380] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1380
	DECLE 22,0
	;[1381] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1381
	DECLE 20,0
	;[1382] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1382
	DECLE 63,0
	;[1383] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1383
	DECLE 9485,0
	;[1384] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1384
	DECLE 6413,0
	;[1385] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1385
	DECLE 16191,0
	;[1386] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1386
	DECLE 8720,0
	;[1387] 	MUSIC E3,F4#,-,-
	SRCFILE "samples/pumpkin_master.bas",1387
	DECLE 7953,0
	;[1388] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1388
	DECLE 16191,0
	;[1389] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1389
	DECLE 8212,0
	;[1390] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1390
	DECLE 6413,0
	;[1391] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1391
	DECLE 16191,0
	;[1392] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1392
	DECLE 22,0
	;[1393] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1393
	DECLE 20,0
	;[1394] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1394
	DECLE 63,0
	;[1395] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1395
	DECLE 6413,0
	;[1396] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1396
	DECLE 8205,0
	;[1397] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1397
	DECLE 16191,0
	;[1398] 	MUSIC D3#,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1398
	DECLE 6416,0
	;[1399] 	MUSIC E3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1399
	DECLE 8721,0
	;[1400] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1400
	DECLE 16191,0
	;[1401] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1401
	DECLE 6420,0
	;[1402] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1402
	DECLE 9485,0
	;[1403] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1403
	DECLE 16191,0
	;[1404] 	MUSIC A3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1404
	DECLE 6422,0
	;[1405] 	MUSIC G3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1405
	DECLE 8724,0
	;[1406] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1406
	DECLE 16191,0
	;[1407] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1407
	DECLE 6413,0
	;[1408] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1408
	DECLE 8205,0
	;[1409] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1409
	DECLE 16191,0
	;[1410] 	MUSIC D3#,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1410
	DECLE 6416,0
	;[1411] 	MUSIC E3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1411
	DECLE 8721,0
	;[1412] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1412
	DECLE 16191,0
	;[1413] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1413
	DECLE 6420,0
	;[1414] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1414
	DECLE 9485,0
	;[1415] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1415
	DECLE 16191,0
	;[1416] 	MUSIC A3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1416
	DECLE 6422,0
	;[1417] 	MUSIC G3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1417
	DECLE 8724,0
	;[1418] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1418
	DECLE 16191,0
	;[1419] 	MUSIC F3,F5,-,-
	SRCFILE "samples/pumpkin_master.bas",1419
	DECLE 10770,0
	;[1420] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1420
	DECLE 7698,0
	;[1421] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1421
	DECLE 16191,0
	;[1422] 	MUSIC G3#,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1422
	DECLE 10005,0
	;[1423] 	MUSIC A3,B4,-,-
	SRCFILE "samples/pumpkin_master.bas",1423
	DECLE 9238,0
	;[1424] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1424
	DECLE 16191,0
	;[1425] 	MUSIC C4,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1425
	DECLE 9497,0
	;[1426] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1426
	DECLE 7698,0
	;[1427] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1427
	DECLE 16191,0
	;[1428] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1428
	DECLE 27,0
	;[1429] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1429
	DECLE 25,0
	;[1430] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1430
	DECLE 63,0
	;[1431] 	MUSIC G3,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1431
	DECLE 12308,0
	;[1432] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1432
	DECLE 20,0
	;[1433] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1433
	DECLE 63,0
	;[1434] 	MUSIC A3#,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1434
	DECLE 12311,0
	;[1435] 	MUSIC B3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1435
	DECLE 24,0
	;[1436] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1436
	DECLE 63,0
	;[1437] 	MUSIC D4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1437
	DECLE 12315,0
	;[1438] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1438
	DECLE 20,0
	;[1439] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1439
	DECLE 63,0
	;[1440] 	MUSIC E4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1440
	DECLE 12317,0
	;[1441] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1441
	DECLE 27,0
	;[1442] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1442
	DECLE 63,0
	;[1443] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1443
	DECLE 7698,0
	;[1444] 	MUSIC F3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1444
	DECLE 9490,0
	;[1445] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1445
	DECLE 16191,0
	;[1446] 	MUSIC G3#,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1446
	DECLE 10005,0
	;[1447] 	MUSIC A3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1447
	DECLE 9494,0
	;[1448] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1448
	DECLE 16191,0
	;[1449] 	MUSIC C4,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1449
	DECLE 7705,0
	;[1450] 	MUSIC F3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1450
	DECLE 18,0
	;[1451] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1451
	DECLE 63,0
	;[1452] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1452
	DECLE 27,0
	;[1453] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1453
	DECLE 25,0
	;[1454] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1454
	DECLE 63,0
	;[1455] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1455
	DECLE 6413,0
	;[1456] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1456
	DECLE 8205,0
	;[1457] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1457
	DECLE 16191,0
	;[1458] 	MUSIC D3#,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1458
	DECLE 6416,0
	;[1459] 	MUSIC E3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1459
	DECLE 8721,0
	;[1460] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1460
	DECLE 16191,0
	;[1461] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1461
	DECLE 6420,0
	;[1462] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1462
	DECLE 9485,0
	;[1463] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1463
	DECLE 16191,0
	;[1464] 	MUSIC A3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1464
	DECLE 6422,0
	;[1465] 	MUSIC G3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1465
	DECLE 8724,0
	;[1466] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1466
	DECLE 16191,0
	;[1467] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1467
	DECLE 10509,0
	;[1468] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1468
	DECLE 13,0
	;[1469] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1469
	DECLE 63,0
	;[1470] 	MUSIC D3#,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1470
	DECLE 16,0
	;[1471] 	MUSIC E3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1471
	DECLE 17,0
	;[1472] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1472
	DECLE 63,0
	;[1473] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1473
	DECLE 10516,0
	;[1474] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1474
	DECLE 13,0
	;[1475] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1475
	DECLE 63,0
	;[1476] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1476
	DECLE 22,0
	;[1477] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1477
	DECLE 20,0
	;[1478] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1478
	DECLE 63,0
	;[1479] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1479
	DECLE 6413,0
	;[1480] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1480
	DECLE 8205,0
	;[1481] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1481
	DECLE 16191,0
	;[1482] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1482
	DECLE 8720,0
	;[1483] 	MUSIC E3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1483
	DECLE 8209,0
	;[1484] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1484
	DECLE 16191,0
	;[1485] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1485
	DECLE 6420,0
	;[1486] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1486
	DECLE 13,0
	;[1487] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1487
	DECLE 63,0
	;[1488] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1488
	DECLE 22,0
	;[1489] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1489
	DECLE 20,0
	;[1490] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1490
	DECLE 63,0
	;[1491] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1491
	DECLE 6413,0
	;[1492] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1492
	DECLE 8205,0
	;[1493] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1493
	DECLE 16191,0
	;[1494] 	MUSIC D3#,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1494
	DECLE 6416,0
	;[1495] 	MUSIC E3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1495
	DECLE 8721,0
	;[1496] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1496
	DECLE 16191,0
	;[1497] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1497
	DECLE 6420,0
	;[1498] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1498
	DECLE 9485,0
	;[1499] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1499
	DECLE 16191,0
	;[1500] 	MUSIC A3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1500
	DECLE 6422,0
	;[1501] 	MUSIC G3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1501
	DECLE 8724,0
	;[1502] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1502
	DECLE 16191,0
	;[1503] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1503
	DECLE 9485,0
	;[1504] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1504
	DECLE 6413,0
	;[1505] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1505
	DECLE 16191,0
	;[1506] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1506
	DECLE 8720,0
	;[1507] 	MUSIC E3,F4#,-,-
	SRCFILE "samples/pumpkin_master.bas",1507
	DECLE 7953,0
	;[1508] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1508
	DECLE 16191,0
	;[1509] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1509
	DECLE 8212,0
	;[1510] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1510
	DECLE 6413,0
	;[1511] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1511
	DECLE 16191,0
	;[1512] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1512
	DECLE 22,0
	;[1513] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1513
	DECLE 20,0
	;[1514] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1514
	DECLE 63,0
	;[1515] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1515
	DECLE 9485,0
	;[1516] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1516
	DECLE 6413,0
	;[1517] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1517
	DECLE 16191,0
	;[1518] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1518
	DECLE 8720,0
	;[1519] 	MUSIC E3,F4#,-,-
	SRCFILE "samples/pumpkin_master.bas",1519
	DECLE 7953,0
	;[1520] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1520
	DECLE 16191,0
	;[1521] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1521
	DECLE 8212,0
	;[1522] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1522
	DECLE 6413,0
	;[1523] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1523
	DECLE 16191,0
	;[1524] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1524
	DECLE 22,0
	;[1525] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1525
	DECLE 20,0
	;[1526] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1526
	DECLE 63,0
	;[1527] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1527
	DECLE 6413,0
	;[1528] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1528
	DECLE 8205,0
	;[1529] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1529
	DECLE 16191,0
	;[1530] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1530
	DECLE 8720,0
	;[1531] 	MUSIC E3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1531
	DECLE 8209,0
	;[1532] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1532
	DECLE 16191,0
	;[1533] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1533
	DECLE 6420,0
	;[1534] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1534
	DECLE 13,0
	;[1535] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1535
	DECLE 63,0
	;[1536] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1536
	DECLE 22,0
	;[1537] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1537
	DECLE 20,0
	;[1538] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1538
	DECLE 63,0
	;[1539] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1539
	DECLE 9485,0
	;[1540] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1540
	DECLE 6413,0
	;[1541] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1541
	DECLE 16191,0
	;[1542] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1542
	DECLE 8720,0
	;[1543] 	MUSIC E3,F4#,-,-
	SRCFILE "samples/pumpkin_master.bas",1543
	DECLE 7953,0
	;[1544] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1544
	DECLE 16191,0
	;[1545] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1545
	DECLE 8212,0
	;[1546] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1546
	DECLE 6413,0
	;[1547] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1547
	DECLE 16191,0
	;[1548] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1548
	DECLE 22,0
	;[1549] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1549
	DECLE 20,0
	;[1550] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1550
	DECLE 63,0
	;[1551] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1551
	DECLE 10509,0
	;[1552] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1552
	DECLE 10509,0
	;[1553] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1553
	DECLE 16191,0
	;[1554] 	MUSIC D3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1554
	DECLE 10512,0
	;[1555] 	MUSIC E3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1555
	DECLE 10513,0
	;[1556] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1556
	DECLE 16191,0
	;[1557] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1557
	DECLE 10516,0
	;[1558] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1558
	DECLE 10509,0
	;[1559] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1559
	DECLE 16191,0
	;[1560] 	MUSIC A3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1560
	DECLE 10518,0
	;[1561] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1561
	DECLE 10516,0
	;[1562] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1562
	DECLE 16191,0
	;[1563] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1563
	DECLE 9485,0
	;[1564] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1564
	DECLE 6413,0
	;[1565] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1565
	DECLE 16191,0
	;[1566] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1566
	DECLE 8720,0
	;[1567] 	MUSIC E3,F4#,-,-
	SRCFILE "samples/pumpkin_master.bas",1567
	DECLE 7953,0
	;[1568] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1568
	DECLE 16191,0
	;[1569] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1569
	DECLE 8212,0
	;[1570] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1570
	DECLE 6413,0
	;[1571] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1571
	DECLE 16191,0
	;[1572] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1572
	DECLE 22,0
	;[1573] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1573
	DECLE 20,0
	;[1574] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1574
	DECLE 63,0
	;[1575] 	MUSIC F3,A5,-,-
	SRCFILE "samples/pumpkin_master.bas",1575
	DECLE 11794,0
	;[1576] 	MUSIC F3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1576
	DECLE 18,0
	;[1577] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1577
	DECLE 63,0
	;[1578] 	MUSIC G3#,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1578
	DECLE 21,0
	;[1579] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1579
	DECLE 22,0
	;[1580] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1580
	DECLE 63,0
	;[1581] 	MUSIC C4,A5,-,-
	SRCFILE "samples/pumpkin_master.bas",1581
	DECLE 11801,0
	;[1582] 	MUSIC F3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1582
	DECLE 18,0
	;[1583] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1583
	DECLE 63,0
	;[1584] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1584
	DECLE 27,0
	;[1585] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1585
	DECLE 25,0
	;[1586] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1586
	DECLE 63,0
	;[1587] 	MUSIC F3,F5,-,-
	SRCFILE "samples/pumpkin_master.bas",1587
	DECLE 10770,0
	;[1588] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1588
	DECLE 7698,0
	;[1589] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1589
	DECLE 16191,0
	;[1590] 	MUSIC G3#,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1590
	DECLE 10005,0
	;[1591] 	MUSIC A3,B4,-,-
	SRCFILE "samples/pumpkin_master.bas",1591
	DECLE 9238,0
	;[1592] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1592
	DECLE 16191,0
	;[1593] 	MUSIC C4,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1593
	DECLE 9497,0
	;[1594] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1594
	DECLE 7698,0
	;[1595] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1595
	DECLE 16191,0
	;[1596] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1596
	DECLE 27,0
	;[1597] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1597
	DECLE 25,0
	;[1598] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1598
	DECLE 63,0
	;[1599] 	MUSIC F3,A5,-,-
	SRCFILE "samples/pumpkin_master.bas",1599
	DECLE 11794,0
	;[1600] 	MUSIC F3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1600
	DECLE 18,0
	;[1601] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1601
	DECLE 63,0
	;[1602] 	MUSIC G3#,A5,-,-
	SRCFILE "samples/pumpkin_master.bas",1602
	DECLE 11797,0
	;[1603] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1603
	DECLE 22,0
	;[1604] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1604
	DECLE 63,0
	;[1605] 	MUSIC C4,A5,-,-
	SRCFILE "samples/pumpkin_master.bas",1605
	DECLE 11801,0
	;[1606] 	MUSIC F3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1606
	DECLE 18,0
	;[1607] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1607
	DECLE 63,0
	;[1608] 	MUSIC D4,A5,-,-
	SRCFILE "samples/pumpkin_master.bas",1608
	DECLE 11803,0
	;[1609] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1609
	DECLE 25,0
	;[1610] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1610
	DECLE 63,0
	;[1611] 	MUSIC F3,F5,-,-
	SRCFILE "samples/pumpkin_master.bas",1611
	DECLE 10770,0
	;[1612] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1612
	DECLE 7698,0
	;[1613] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1613
	DECLE 16191,0
	;[1614] 	MUSIC G3#,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1614
	DECLE 10005,0
	;[1615] 	MUSIC A3,B4,-,-
	SRCFILE "samples/pumpkin_master.bas",1615
	DECLE 9238,0
	;[1616] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1616
	DECLE 16191,0
	;[1617] 	MUSIC C4,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1617
	DECLE 9497,0
	;[1618] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1618
	DECLE 7698,0
	;[1619] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1619
	DECLE 16191,0
	;[1620] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1620
	DECLE 27,0
	;[1621] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1621
	DECLE 25,0
	;[1622] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1622
	DECLE 63,0
	;[1623] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1623
	DECLE 8212,0
	;[1624] 	MUSIC G3,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1624
	DECLE 10004,0
	;[1625] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1625
	DECLE 16191,0
	;[1626] 	MUSIC A3#,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1626
	DECLE 8215,0
	;[1627] 	MUSIC B3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1627
	DECLE 10520,0
	;[1628] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1628
	DECLE 16191,0
	;[1629] 	MUSIC D4,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1629
	DECLE 8219,0
	;[1630] 	MUSIC G3,G5,-,-
	SRCFILE "samples/pumpkin_master.bas",1630
	DECLE 11284,0
	;[1631] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1631
	DECLE 16191,0
	;[1632] 	MUSIC E4,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1632
	DECLE 8221,0
	;[1633] 	MUSIC D4,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1633
	DECLE 10523,0
	;[1634] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1634
	DECLE 16191,0
	;[1635] 	MUSIC G3,G5,-,-
	SRCFILE "samples/pumpkin_master.bas",1635
	DECLE 11284,0
	;[1636] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1636
	DECLE 8212,0
	;[1637] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1637
	DECLE 16191,0
	;[1638] 	MUSIC A3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1638
	DECLE 10519,0
	;[1639] 	MUSIC B3,C5#,-,-
	SRCFILE "samples/pumpkin_master.bas",1639
	DECLE 9752,0
	;[1640] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1640
	DECLE 16191,0
	;[1641] 	MUSIC D4,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1641
	DECLE 10011,0
	;[1642] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1642
	DECLE 8212,0
	;[1643] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1643
	DECLE 16191,0
	;[1644] 	MUSIC E4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1644
	DECLE 29,0
	;[1645] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1645
	DECLE 27,0
	;[1646] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1646
	DECLE 63,0
	;[1647] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1647
	DECLE 8212,0
	;[1648] 	MUSIC G3,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1648
	DECLE 10004,0
	;[1649] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1649
	DECLE 16191,0
	;[1650] 	MUSIC A3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1650
	DECLE 10519,0
	;[1651] 	MUSIC B3,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1651
	DECLE 10008,0
	;[1652] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1652
	DECLE 16191,0
	;[1653] 	MUSIC D4,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1653
	DECLE 8219,0
	;[1654] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1654
	DECLE 20,0
	;[1655] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1655
	DECLE 63,0
	;[1656] 	MUSIC E4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1656
	DECLE 29,0
	;[1657] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1657
	DECLE 27,0
	;[1658] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1658
	DECLE 63,0
	;[1659] 	MUSIC G3,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1659
	DECLE 12308,0
	;[1660] 	MUSIC G3,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1660
	DECLE 12308,0
	;[1661] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1661
	DECLE 16191,0
	;[1662] 	MUSIC A3#,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1662
	DECLE 12311,0
	;[1663] 	MUSIC B3,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1663
	DECLE 12312,0
	;[1664] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1664
	DECLE 16191,0
	;[1665] 	MUSIC D4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1665
	DECLE 12315,0
	;[1666] 	MUSIC G3,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1666
	DECLE 12308,0
	;[1667] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1667
	DECLE 16191,0
	;[1668] 	MUSIC E4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1668
	DECLE 12317,0
	;[1669] 	MUSIC D4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1669
	DECLE 12315,0
	;[1670] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1670
	DECLE 16191,0
	;[1671] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1671
	DECLE 6413,0
	;[1672] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1672
	DECLE 8205,0
	;[1673] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1673
	DECLE 16191,0
	;[1674] 	MUSIC D3#,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1674
	DECLE 6416,0
	;[1675] 	MUSIC E3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1675
	DECLE 8721,0
	;[1676] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1676
	DECLE 16191,0
	;[1677] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1677
	DECLE 6420,0
	;[1678] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1678
	DECLE 9485,0
	;[1679] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1679
	DECLE 16191,0
	;[1680] 	MUSIC A3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1680
	DECLE 6422,0
	;[1681] 	MUSIC G3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1681
	DECLE 8724,0
	;[1682] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1682
	DECLE 16191,0
	;[1683] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1683
	DECLE 13,0
	;[1684] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1684
	DECLE 13,0
	;[1685] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1685
	DECLE 63,0
	;[1686] 	MUSIC D3#,-,-
	SRCFILE "samples/pumpkin_master.bas",1686
	DECLE 16,0
	;[1687] 	MUSIC E3,-,-
	SRCFILE "samples/pumpkin_master.bas",1687
	DECLE 17,0
	;[1688] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1688
	DECLE 63,0
	;[1689] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1689
	DECLE 20,0
	;[1690] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1690
	DECLE 13,0
	;[1691] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1691
	DECLE 63,0
	;[1692] 	MUSIC A3,-,-
	SRCFILE "samples/pumpkin_master.bas",1692
	DECLE 22,0
	;[1693] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1693
	DECLE 20,0
	;[1694] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1694
	DECLE 63,0
	;[1695] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1695
	DECLE 10509,0
	;[1696] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1696
	DECLE 10509,0
	;[1697] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1697
	DECLE 16191,0
	;[1698] 	MUSIC D3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1698
	DECLE 10512,0
	;[1699] 	MUSIC E3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1699
	DECLE 10513,0
	;[1700] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1700
	DECLE 16191,0
	;[1701] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1701
	DECLE 10516,0
	;[1702] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1702
	DECLE 10509,0
	;[1703] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1703
	DECLE 16191,0
	;[1704] 	MUSIC A3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1704
	DECLE 10518,0
	;[1705] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1705
	DECLE 10516,0
	;[1706] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1706
	DECLE 16191,0
	;[1707] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1707
	DECLE 13,0
	;[1708] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1708
	DECLE 13,0
	;[1709] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1709
	DECLE 63,0
	;[1710] 	MUSIC D3#,-,-
	SRCFILE "samples/pumpkin_master.bas",1710
	DECLE 16,0
	;[1711] 	MUSIC E3,-,-
	SRCFILE "samples/pumpkin_master.bas",1711
	DECLE 17,0
	;[1712] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1712
	DECLE 63,0
	;[1713] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1713
	DECLE 20,0
	;[1714] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1714
	DECLE 13,0
	;[1715] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1715
	DECLE 63,0
	;[1716] 	MUSIC A3,-,-
	SRCFILE "samples/pumpkin_master.bas",1716
	DECLE 22,0
	;[1717] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1717
	DECLE 20,0
	;[1718] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1718
	DECLE 63,0
	;[1719] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1719
	DECLE 10509,0
	;[1720] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1720
	DECLE 13,0
	;[1721] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1721
	DECLE 63,0
	;[1722] 	MUSIC D3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1722
	DECLE 10512,0
	;[1723] 	MUSIC E3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1723
	DECLE 17,0
	;[1724] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1724
	DECLE 63,0
	;[1725] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1725
	DECLE 10516,0
	;[1726] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1726
	DECLE 13,0
	;[1727] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1727
	DECLE 63,0
	;[1728] 	MUSIC A3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1728
	DECLE 10518,0
	;[1729] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1729
	DECLE 20,0
	;[1730] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1730
	DECLE 63,0
	;[1731] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1731
	DECLE 13,0
	;[1732] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1732
	DECLE 13,0
	;[1733] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1733
	DECLE 63,0
	;[1734] 	MUSIC D3#,-,-
	SRCFILE "samples/pumpkin_master.bas",1734
	DECLE 16,0
	;[1735] 	MUSIC E3,-,-
	SRCFILE "samples/pumpkin_master.bas",1735
	DECLE 17,0
	;[1736] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1736
	DECLE 63,0
	;[1737] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1737
	DECLE 20,0
	;[1738] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1738
	DECLE 13,0
	;[1739] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1739
	DECLE 63,0
	;[1740] 	MUSIC A3,-,-
	SRCFILE "samples/pumpkin_master.bas",1740
	DECLE 22,0
	;[1741] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1741
	DECLE 20,0
	;[1742] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1742
	DECLE 63,0
	;[1743] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1743
	DECLE 10509,0
	;[1744] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1744
	DECLE 10509,0
	;[1745] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1745
	DECLE 16191,0
	;[1746] 	MUSIC D3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1746
	DECLE 10512,0
	;[1747] 	MUSIC E3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1747
	DECLE 10513,0
	;[1748] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1748
	DECLE 16191,0
	;[1749] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1749
	DECLE 10516,0
	;[1750] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1750
	DECLE 10509,0
	;[1751] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1751
	DECLE 16191,0
	;[1752] 	MUSIC A3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1752
	DECLE 10518,0
	;[1753] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1753
	DECLE 10516,0
	;[1754] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1754
	DECLE 16191,0
	;[1755] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1755
	DECLE 13,0
	;[1756] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1756
	DECLE 13,0
	;[1757] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1757
	DECLE 63,0
	;[1758] 	MUSIC D3#,-,-
	SRCFILE "samples/pumpkin_master.bas",1758
	DECLE 16,0
	;[1759] 	MUSIC E3,-,-
	SRCFILE "samples/pumpkin_master.bas",1759
	DECLE 17,0
	;[1760] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1760
	DECLE 63,0
	;[1761] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1761
	DECLE 20,0
	;[1762] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1762
	DECLE 13,0
	;[1763] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1763
	DECLE 63,0
	;[1764] 	MUSIC A3,-,-
	SRCFILE "samples/pumpkin_master.bas",1764
	DECLE 22,0
	;[1765] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1765
	DECLE 20,0
	;[1766] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1766
	DECLE 63,0
	;[1767] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1767
	DECLE 10509,0
	;[1768] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1768
	DECLE 10509,0
	;[1769] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1769
	DECLE 16191,0
	;[1770] 	MUSIC D3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1770
	DECLE 10512,0
	;[1771] 	MUSIC E3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1771
	DECLE 10513,0
	;[1772] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1772
	DECLE 16191,0
	;[1773] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1773
	DECLE 10516,0
	;[1774] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1774
	DECLE 10509,0
	;[1775] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1775
	DECLE 16191,0
	;[1776] 	MUSIC A3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1776
	DECLE 10518,0
	;[1777] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1777
	DECLE 10516,0
	;[1778] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1778
	DECLE 16191,0
	;[1779] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1779
	DECLE 6413,0
	;[1780] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1780
	DECLE 8205,0
	;[1781] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1781
	DECLE 16191,0
	;[1782] 	MUSIC D3#,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1782
	DECLE 6416,0
	;[1783] 	MUSIC E3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1783
	DECLE 8721,0
	;[1784] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1784
	DECLE 16191,0
	;[1785] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1785
	DECLE 6420,0
	;[1786] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1786
	DECLE 9485,0
	;[1787] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1787
	DECLE 16191,0
	;[1788] 	MUSIC A3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1788
	DECLE 6422,0
	;[1789] 	MUSIC G3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1789
	DECLE 8724,0
	;[1790] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1790
	DECLE 16191,0
	;[1791] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1791
	DECLE 8212,0
	;[1792] 	MUSIC G3,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1792
	DECLE 10004,0
	;[1793] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1793
	DECLE 16191,0
	;[1794] 	MUSIC A3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1794
	DECLE 10519,0
	;[1795] 	MUSIC B3,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1795
	DECLE 10008,0
	;[1796] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1796
	DECLE 16191,0
	;[1797] 	MUSIC D4,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1797
	DECLE 8219,0
	;[1798] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1798
	DECLE 20,0
	;[1799] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1799
	DECLE 63,0
	;[1800] 	MUSIC E4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1800
	DECLE 29,0
	;[1801] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1801
	DECLE 27,0
	;[1802] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1802
	DECLE 63,0
	;[1803] 	MUSIC G3,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1803
	DECLE 12308,0
	;[1804] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1804
	DECLE 20,0
	;[1805] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1805
	DECLE 63,0
	;[1806] 	MUSIC A3#,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1806
	DECLE 12311,0
	;[1807] 	MUSIC B3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1807
	DECLE 24,0
	;[1808] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1808
	DECLE 63,0
	;[1809] 	MUSIC D4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1809
	DECLE 12315,0
	;[1810] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1810
	DECLE 20,0
	;[1811] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1811
	DECLE 63,0
	;[1812] 	MUSIC E4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1812
	DECLE 12317,0
	;[1813] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1813
	DECLE 27,0
	;[1814] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1814
	DECLE 63,0
	;[1815] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1815
	DECLE 6413,0
	;[1816] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1816
	DECLE 8205,0
	;[1817] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1817
	DECLE 16191,0
	;[1818] 	MUSIC D3#,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1818
	DECLE 6416,0
	;[1819] 	MUSIC E3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1819
	DECLE 8721,0
	;[1820] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1820
	DECLE 16191,0
	;[1821] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1821
	DECLE 6420,0
	;[1822] 	MUSIC C3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1822
	DECLE 9485,0
	;[1823] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1823
	DECLE 16191,0
	;[1824] 	MUSIC A3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1824
	DECLE 6422,0
	;[1825] 	MUSIC G3,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1825
	DECLE 8724,0
	;[1826] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1826
	DECLE 16191,0
	;[1827] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1827
	DECLE 13,0
	;[1828] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1828
	DECLE 13,0
	;[1829] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1829
	DECLE 63,0
	;[1830] 	MUSIC D3#,-,-
	SRCFILE "samples/pumpkin_master.bas",1830
	DECLE 16,0
	;[1831] 	MUSIC E3,-,-
	SRCFILE "samples/pumpkin_master.bas",1831
	DECLE 17,0
	;[1832] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1832
	DECLE 63,0
	;[1833] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1833
	DECLE 20,0
	;[1834] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1834
	DECLE 13,0
	;[1835] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1835
	DECLE 63,0
	;[1836] 	MUSIC A3,-,-
	SRCFILE "samples/pumpkin_master.bas",1836
	DECLE 22,0
	;[1837] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1837
	DECLE 20,0
	;[1838] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1838
	DECLE 63,0
	;[1839] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1839
	DECLE 10509,0
	;[1840] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1840
	DECLE 13,0
	;[1841] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1841
	DECLE 63,0
	;[1842] 	MUSIC D3#,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1842
	DECLE 16,0
	;[1843] 	MUSIC E3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1843
	DECLE 17,0
	;[1844] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1844
	DECLE 63,0
	;[1845] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1845
	DECLE 10516,0
	;[1846] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1846
	DECLE 13,0
	;[1847] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1847
	DECLE 63,0
	;[1848] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1848
	DECLE 22,0
	;[1849] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1849
	DECLE 20,0
	;[1850] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1850
	DECLE 63,0
	;[1851] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1851
	DECLE 13,0
	;[1852] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1852
	DECLE 13,0
	;[1853] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1853
	DECLE 63,0
	;[1854] 	MUSIC D3#,-,-
	SRCFILE "samples/pumpkin_master.bas",1854
	DECLE 16,0
	;[1855] 	MUSIC E3,-,-
	SRCFILE "samples/pumpkin_master.bas",1855
	DECLE 17,0
	;[1856] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1856
	DECLE 63,0
	;[1857] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1857
	DECLE 20,0
	;[1858] 	MUSIC C3,-,-
	SRCFILE "samples/pumpkin_master.bas",1858
	DECLE 13,0
	;[1859] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1859
	DECLE 63,0
	;[1860] 	MUSIC A3,-,-
	SRCFILE "samples/pumpkin_master.bas",1860
	DECLE 22,0
	;[1861] 	MUSIC G3,-,-
	SRCFILE "samples/pumpkin_master.bas",1861
	DECLE 20,0
	;[1862] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1862
	DECLE 63,0
	;[1863] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1863
	DECLE 10509,0
	;[1864] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1864
	DECLE 10509,0
	;[1865] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1865
	DECLE 16191,0
	;[1866] 	MUSIC D3#,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1866
	DECLE 10512,0
	;[1867] 	MUSIC E3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1867
	DECLE 10513,0
	;[1868] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1868
	DECLE 16191,0
	;[1869] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1869
	DECLE 10516,0
	;[1870] 	MUSIC C3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1870
	DECLE 10509,0
	;[1871] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1871
	DECLE 16191,0
	;[1872] 	MUSIC A3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1872
	DECLE 10518,0
	;[1873] 	MUSIC G3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1873
	DECLE 10516,0
	;[1874] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1874
	DECLE 16191,0
	;[1875] 	MUSIC C3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1875
	DECLE 6413,0
	;[1876] 	MUSIC C3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1876
	DECLE 8205,0
	;[1877] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1877
	DECLE 16191,0
	;[1878] 	MUSIC D3#,A4,-,-
	SRCFILE "samples/pumpkin_master.bas",1878
	DECLE 8720,0
	;[1879] 	MUSIC E3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1879
	DECLE 8209,0
	;[1880] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1880
	DECLE 16191,0
	;[1881] 	MUSIC G3,C4,-,-
	SRCFILE "samples/pumpkin_master.bas",1881
	DECLE 6420,0
	;[1882] 	MUSIC C3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1882
	DECLE 13,0
	;[1883] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1883
	DECLE 63,0
	;[1884] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1884
	DECLE 22,0
	;[1885] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1885
	DECLE 20,0
	;[1886] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1886
	DECLE 63,0
	;[1887] 	MUSIC F3,F5,-,-
	SRCFILE "samples/pumpkin_master.bas",1887
	DECLE 10770,0
	;[1888] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1888
	DECLE 7698,0
	;[1889] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1889
	DECLE 16191,0
	;[1890] 	MUSIC G3#,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1890
	DECLE 10005,0
	;[1891] 	MUSIC A3,B4,-,-
	SRCFILE "samples/pumpkin_master.bas",1891
	DECLE 9238,0
	;[1892] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1892
	DECLE 16191,0
	;[1893] 	MUSIC C4,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1893
	DECLE 9497,0
	;[1894] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1894
	DECLE 7698,0
	;[1895] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1895
	DECLE 16191,0
	;[1896] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1896
	DECLE 27,0
	;[1897] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1897
	DECLE 25,0
	;[1898] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1898
	DECLE 63,0
	;[1899] 	MUSIC F3,F5,-,-
	SRCFILE "samples/pumpkin_master.bas",1899
	DECLE 10770,0
	;[1900] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1900
	DECLE 7698,0
	;[1901] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1901
	DECLE 16191,0
	;[1902] 	MUSIC G3#,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1902
	DECLE 10005,0
	;[1903] 	MUSIC A3,B4,-,-
	SRCFILE "samples/pumpkin_master.bas",1903
	DECLE 9238,0
	;[1904] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1904
	DECLE 16191,0
	;[1905] 	MUSIC C4,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1905
	DECLE 9497,0
	;[1906] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1906
	DECLE 7698,0
	;[1907] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1907
	DECLE 16191,0
	;[1908] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1908
	DECLE 27,0
	;[1909] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1909
	DECLE 25,0
	;[1910] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1910
	DECLE 63,0
	;[1911] 	MUSIC F3,A5,-,-
	SRCFILE "samples/pumpkin_master.bas",1911
	DECLE 11794,0
	;[1912] 	MUSIC F3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1912
	DECLE 18,0
	;[1913] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1913
	DECLE 63,0
	;[1914] 	MUSIC G3#,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1914
	DECLE 21,0
	;[1915] 	MUSIC A3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1915
	DECLE 22,0
	;[1916] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1916
	DECLE 63,0
	;[1917] 	MUSIC C4,A5,-,-
	SRCFILE "samples/pumpkin_master.bas",1917
	DECLE 11801,0
	;[1918] 	MUSIC F3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1918
	DECLE 18,0
	;[1919] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1919
	DECLE 63,0
	;[1920] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1920
	DECLE 27,0
	;[1921] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1921
	DECLE 25,0
	;[1922] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1922
	DECLE 63,0
	;[1923] 	MUSIC F3,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1923
	DECLE 7698,0
	;[1924] 	MUSIC F3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1924
	DECLE 9490,0
	;[1925] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1925
	DECLE 16191,0
	;[1926] 	MUSIC G3#,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1926
	DECLE 10005,0
	;[1927] 	MUSIC A3,C5,-,-
	SRCFILE "samples/pumpkin_master.bas",1927
	DECLE 9494,0
	;[1928] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1928
	DECLE 16191,0
	;[1929] 	MUSIC C4,F4,-,-
	SRCFILE "samples/pumpkin_master.bas",1929
	DECLE 7705,0
	;[1930] 	MUSIC F3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1930
	DECLE 18,0
	;[1931] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1931
	DECLE 63,0
	;[1932] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1932
	DECLE 27,0
	;[1933] 	MUSIC C4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1933
	DECLE 25,0
	;[1934] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1934
	DECLE 63,0
	;[1935] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1935
	DECLE 8212,0
	;[1936] 	MUSIC G3,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1936
	DECLE 10004,0
	;[1937] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1937
	DECLE 16191,0
	;[1938] 	MUSIC A3#,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1938
	DECLE 8215,0
	;[1939] 	MUSIC B3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1939
	DECLE 10520,0
	;[1940] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1940
	DECLE 16191,0
	;[1941] 	MUSIC D4,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1941
	DECLE 8219,0
	;[1942] 	MUSIC G3,G5,-,-
	SRCFILE "samples/pumpkin_master.bas",1942
	DECLE 11284,0
	;[1943] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1943
	DECLE 16191,0
	;[1944] 	MUSIC E4,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1944
	DECLE 8221,0
	;[1945] 	MUSIC D4,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1945
	DECLE 10523,0
	;[1946] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1946
	DECLE 16191,0
	;[1947] 	MUSIC G3,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1947
	DECLE 12308,0
	;[1948] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1948
	DECLE 20,0
	;[1949] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1949
	DECLE 63,0
	;[1950] 	MUSIC A3#,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1950
	DECLE 23,0
	;[1951] 	MUSIC B3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1951
	DECLE 24,0
	;[1952] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1952
	DECLE 63,0
	;[1953] 	MUSIC D4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1953
	DECLE 12315,0
	;[1954] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1954
	DECLE 20,0
	;[1955] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1955
	DECLE 63,0
	;[1956] 	MUSIC E4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1956
	DECLE 29,0
	;[1957] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1957
	DECLE 27,0
	;[1958] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1958
	DECLE 63,0
	;[1959] 	MUSIC G3,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1959
	DECLE 8212,0
	;[1960] 	MUSIC G3,D5,-,-
	SRCFILE "samples/pumpkin_master.bas",1960
	DECLE 10004,0
	;[1961] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1961
	DECLE 16191,0
	;[1962] 	MUSIC A3#,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1962
	DECLE 8215,0
	;[1963] 	MUSIC B3,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1963
	DECLE 10520,0
	;[1964] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1964
	DECLE 16191,0
	;[1965] 	MUSIC D4,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1965
	DECLE 8219,0
	;[1966] 	MUSIC G3,G5,-,-
	SRCFILE "samples/pumpkin_master.bas",1966
	DECLE 11284,0
	;[1967] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1967
	DECLE 16191,0
	;[1968] 	MUSIC E4,G4,-,-
	SRCFILE "samples/pumpkin_master.bas",1968
	DECLE 8221,0
	;[1969] 	MUSIC D4,E5,-,-
	SRCFILE "samples/pumpkin_master.bas",1969
	DECLE 10523,0
	;[1970] 	MUSIC S,S,-
	SRCFILE "samples/pumpkin_master.bas",1970
	DECLE 16191,0
	;[1971] 	MUSIC G3,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1971
	DECLE 12308,0
	;[1972] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1972
	DECLE 20,0
	;[1973] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1973
	DECLE 63,0
	;[1974] 	MUSIC A3#,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1974
	DECLE 12311,0
	;[1975] 	MUSIC B3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1975
	DECLE 24,0
	;[1976] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1976
	DECLE 63,0
	;[1977] 	MUSIC D4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1977
	DECLE 12315,0
	;[1978] 	MUSIC G3,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1978
	DECLE 20,0
	;[1979] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1979
	DECLE 63,0
	;[1980] 	MUSIC E4,B5,-,-
	SRCFILE "samples/pumpkin_master.bas",1980
	DECLE 12317,0
	;[1981] 	MUSIC D4,-,-,-
	SRCFILE "samples/pumpkin_master.bas",1981
	DECLE 27,0
	;[1982] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1982
	DECLE 63,0
	;[1983] 	MUSIC REPEAT
	SRCFILE "samples/pumpkin_master.bas",1983
	DECLE 0,64768
	;[1984] 
	SRCFILE "samples/pumpkin_master.bas",1984
	;[1985] 	'
	SRCFILE "samples/pumpkin_master.bas",1985
	;[1986] 	' Boss "music"
	SRCFILE "samples/pumpkin_master.bas",1986
	;[1987] 	'
	SRCFILE "samples/pumpkin_master.bas",1987
	;[1988] music_beat:
	SRCFILE "samples/pumpkin_master.bas",1988
	; MUSIC_BEAT
label_MUSIC_BEAT:	;[1989] 	DATA 6
	SRCFILE "samples/pumpkin_master.bas",1989
	DECLE 6
	;[1990] 	MUSIC C2W,-,-
	SRCFILE "samples/pumpkin_master.bas",1990
	DECLE 1,0
	;[1991] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1991
	DECLE 63,0
	;[1992] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1992
	DECLE 63,0
	;[1993] 	MUSIC -,-,-
	SRCFILE "samples/pumpkin_master.bas",1993
	DECLE 0,0
	;[1994] 	MUSIC C2#W,-,-
	SRCFILE "samples/pumpkin_master.bas",1994
	DECLE 2,0
	;[1995] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1995
	DECLE 63,0
	;[1996] 	MUSIC S,-,-
	SRCFILE "samples/pumpkin_master.bas",1996
	DECLE 63,0
	;[1997] 	MUSIC -,-,-
	SRCFILE "samples/pumpkin_master.bas",1997
	DECLE 0,0
	;[1998] 	MUSIC REPEAT
	SRCFILE "samples/pumpkin_master.bas",1998
	DECLE 0,64768
	;ENDFILE
	SRCFILE "",0
	;
	; Epilogue for IntyBASIC programs
	; by Oscar Toledo G.  http://nanochess.org/
	;
	; Revision: Jan/30/2014. Moved GRAM code below MOB updates.
	;                        Added comments.
	; Revision: Feb/26/2014. Optimized access to collision registers
	;                        per DZ-Jay suggestion. Added scrolling
	;                        routines with optimization per intvnut
	;                        suggestion. Added border/mask support.
	; Revision: Apr/02/2014. Added support to set MODE (color stack
	;                        or foreground/background), added support
	;                        for SCREEN statement.
	; Revision: Aug/19/2014. Solved bug in bottom scroll, moved an
	;                        extra unneeded line.
	; Revision: Aug/26/2014. Integrated music player and NTSC/PAL
	;                        detection.
	; Revision: Oct/24/2014. Adjust in some comments.
	; Revision: Nov/13/2014. Integrated Joseph Zbiciak's routines
	;                        for printing numbers.
	; Revision: Nov/17/2014. Redesigned MODE support to use a single
	;                        variable.
	; Revision: Nov/21/2014. Added Intellivoice support routines made
	;                        by Joseph Zbiciak.
	; Revision: Dec/11/2014. Optimized keypad decode routines.
	; Revision: Jan/25/2015. Added marker for insertion of ON FRAME GOSUB
	; Revision: Feb/17/2015. Allows to deactivate music player (PLAY NONE)
	; Revision: Apr/21/2015. Accelerates common case of keypad not pressed.
	;                        Added ECS ROM disable code.
	; Revision: Apr/22/2015. Added Joseph Zbiciak accelerated multiplication
	;                        routines.
	; Revision: Jun/04/2015. Optimized play_music (per GroovyBee suggestion)
	; Revision: Jul/25/2015. Added infinite loop at start to avoid crashing
	;                        with empty programs. Solved bug where _color
	;                        didn't started with white.
	; Revision: Aug/20/2015. Moved ECS mapper disable code so nothing gets
	;                        after it (GroovyBee 42K sample code)
	; Revision: Aug/21/2015. Added Joseph Zbiciak routines for JLP Flash
	;                        handling.
	; Revision: Aug/31/2015. Added CPYBLK2 for SCREEN fifth argument.
	; Revision: Sep/01/2015. Defined labels Q1 and Q2 as alias.
	; Revision: Jan/22/2016. Music player allows not to use noise channel
	;                        for drums. Allows setting music volume.
	; Revision: Jan/23/2016. Added jump inside of music (for MUSIC JUMP)
	; Revision: May/03/2016. Preserves current mode in bit 0 of _mode_select
	; Revision: Oct/21/2016. Added C7 in notes table, it was missing. (thanks
	;                        mmarrero)
	; Revision: Jan/09/2018. Initializes scroll offset registers (useful when
	;                        starting from $4800). Uses slightly less space.
	; Revision: Feb/05/2018. Added IV_HUSH.
	; Revision: Mar/01/2018. Added support for music tracker over ECS.
	; Revision: Sep/25/2018. Solved bug in mixer for ECS drums.
	; Revision: Oct/30/2018. Small optimization in music player.
	; Revision: Jan/09/2019. Solved bug where it would play always like
	;                        PLAY SIMPLE NO DRUMS.
	; Revision: May/18/2019. Solved bug where drums failed in ECS side.
	;

	;
	; Avoids empty programs to crash
	; 
stuck:	B stuck

	ROM.SelectDefaultSegment

	;
	; Copy screen helper for SCREEN wide statement
	;

CPYBLK2:	PROC
	MOVR R0,R3		; Offset
	MOVR R5,R2
	PULR R0
	PULR R1
	PULR R5
	PULR R4
	PSHR R2
	SUBR R1,R3

@@1:	PSHR R3
	MOVR R1,R3		; Init line copy
@@2:	MVI@ R4,R2		; Copy line
	MVO@ R2,R5
	DECR R3
	BNE @@2
	PULR R3		 ; Add offset to start in next line
	ADDR R3,R4
	SUBR R1,R5
	ADDI #20,R5
	DECR R0		 ; Count lines
	BNE @@1

	RETURN
	ENDP

	;
	; Copy screen helper for SCREEN statement
	;
CPYBLK:	PROC
	BEGIN
	MOVR R3,R4
	MOVR R2,R5

@@1:	MOVR R1,R3	      ; Init line copy
@@2:	MVI@ R4,R2	      ; Copy line
	MVO@ R2,R5
	DECR R3
	BNE @@2
	MVII #20,R3	     ; Add offset to start in next line
	SUBR R1,R3
	ADDR R3,R4
	ADDR R3,R5
	DECR R0		 ; Count lines
	BNE @@1
	RETURN
	ENDP

	;
	; Wait for interruption
	;
_wait:  PROC

    IF intybasic_keypad
	MVI $01FF,R0
	COMR R0
	ANDI #$FF,R0
	CMP _cnt1_p0,R0
	BNE @@2
	CMP _cnt1_p1,R0
	BNE @@2
	TSTR R0		; Accelerates common case of key not pressed
	MVII #_keypad_table+13,R4
	BEQ @@4
	MVII #_keypad_table,R4
    REPEAT 6
	CMP@ R4,R0
	BEQ @@4
	CMP@ R4,R0
	BEQ @@4
    ENDR
	INCR R4
@@4:    SUBI #_keypad_table+1,R4
	MVO R4,_cnt1_key

@@2:    MVI _cnt1_p1,R1
	MVO R1,_cnt1_p0
	MVO R0,_cnt1_p1

	MVI $01FE,R0
	COMR R0
	ANDI #$FF,R0
	CMP _cnt2_p0,R0
	BNE @@5
	CMP _cnt2_p1,R0
	BNE @@5
	TSTR R0		; Accelerates common case of key not pressed
	MVII #_keypad_table+13,R4
	BEQ @@7
	MVII #_keypad_table,R4
    REPEAT 6
	CMP@ R4,R0
	BEQ @@7
	CMP@ R4,R0
	BEQ @@7
    ENDR

	INCR R4
@@7:    SUBI #_keypad_table+1,R4
	MVO R4,_cnt2_key

@@5:    MVI _cnt2_p1,R1
	MVO R1,_cnt2_p0
	MVO R0,_cnt2_p1
    ENDI

	CLRR    R0
	MVO     R0,_int	 ; Clears waiting flag
@@1:	CMP     _int,  R0       ; Waits for change
	BEQ     @@1
	JR      R5	      ; Returns
	ENDP

	;
	; Keypad table
	;
_keypad_table:	  PROC
	DECLE $48,$81,$41,$21,$82,$42,$22,$84,$44,$24,$88,$28
	ENDP

_set_isr:	PROC
	MVI@ R5,R0
	MVO R0,ISRVEC
	SWAP R0
	MVO R0,ISRVEC+1
	JR R5
	ENDP

	;
	; Interruption routine
	;
_int_vector:     PROC

    IF intybasic_stack
	CMPI #$308,R6
	BNC @@vs
	MVO R0,$20	; Enables display
	MVI $21,R0	; Activates Color Stack mode
	CLRR R0
	MVO R0,$28
	MVO R0,$29
	MVO R0,$2A
	MVO R0,$2B
	MVII #@@vs1,R4
	MVII #$200,R5
	MVII #20,R1
@@vs2:	MVI@ R4,R0
	MVO@ R0,R5
	DECR R1
	BNE @@vs2
	RETURN

	; Stack Overflow message
@@vs1:	DECLE 0,0,0,$33*8+7,$54*8+7,$41*8+7,$43*8+7,$4B*8+7,$00*8+7
	DECLE $4F*8+7,$56*8+7,$45*8+7,$52*8+7,$46*8+7,$4C*8+7
	DECLE $4F*8+7,$57*8+7,0,0,0

@@vs:
    ENDI

	MVII #1,R1
	MVO R1,_int	; Indicates interrupt happened.

	MVI _mode_select,R0
	SARC R0,2
	BNE @@ds
	MVO R0,$20	; Enables display
@@ds:	BNC @@vi14
	MVO R0,$21	; Foreground/background mode
	BNOV @@vi0
	B @@vi15

@@vi14:	MVI $21,R0	; Color stack mode
	BNOV @@vi0
	CLRR R1
	MVI _color,R0
	MVO R0,$28
	SWAP R0
	MVO R0,$29
	SLR R0,2
	SLR R0,2
	MVO R0,$2A
	SWAP R0
	MVO R0,$2B
@@vi15:
	MVO R1,_mode_select
	MVII #7,R0
	MVO R0,_color	   ; Default color for PRINT "string"
@@vi0:

	BEGIN

	MVI _border_color,R0
	MVO     R0,     $2C     ; Border color
	MVI _border_mask,R0
	MVO     R0,     $32     ; Border mask
    IF intybasic_col
	;
	; Save collision registers for further use and clear them
	;
	MVII #$18,R4
	MVII #_col0,R5
	MVI@ R4,R0
	MVO@ R0,R5  ; _col0
	MVI@ R4,R0
	MVO@ R0,R5  ; _col1
	MVI@ R4,R0
	MVO@ R0,R5  ; _col2
	MVI@ R4,R0
	MVO@ R0,R5  ; _col3
	MVI@ R4,R0
	MVO@ R0,R5  ; _col4
	MVI@ R4,R0
	MVO@ R0,R5  ; _col5
	MVI@ R4,R0
	MVO@ R0,R5  ; _col6
	MVI@ R4,R0
	MVO@ R0,R5  ; _col7
    ENDI
	
    IF intybasic_scroll

	;
	; Scrolling things
	;
	MVI _scroll_x,R0
	MVO R0,$30
	MVI _scroll_y,R0
	MVO R0,$31
    ENDI

	;
	; Updates sprites (MOBs)
	;
	MVII #_mobs,R4
	CLRR R5		; X-coordinates
    REPEAT 8
	MVI@ R4,R0
	MVO@ R0,R5
	MVI@ R4,R0
	MVO@ R0,R5
	MVI@ R4,R0
	MVO@ R0,R5
    ENDR
    IF intybasic_col
	CLRR R0		; Erase collision bits (R5 = $18)
	MVO@ R0,R5
	MVO@ R0,R5
	MVO@ R0,R5
	MVO@ R0,R5
	MVO@ R0,R5
	MVO@ R0,R5
	MVO@ R0,R5
	MVO@ R0,R5
    ENDI

    IF intybasic_music
     	MVI _ntsc,R0
	RRC R0,1	 ; PAL?
	BNC @@vo97      ; Yes, always emit sound
	MVI _music_frame,R0
	INCR R0
	CMPI #6,R0
	BNE @@vo14
	CLRR R0
@@vo14:	MVO R0,_music_frame
	BEQ @@vo15
@@vo97:	CALL _emit_sound
    IF intybasic_music_ecs
	CALL _emit_sound_ecs
    ENDI
@@vo15:
    ENDI

	;
	; Detect GRAM definition
	;
	MVI _gram_bitmap,R4
	TSTR R4
	BEQ @@vi1
	MVI _gram_target,R1
	SLL R1,2
	SLL R1,1
	ADDI #$3800,R1
	MOVR R1,R5
	MVI _gram_total,R0
@@vi3:
	MVI@    R4,     R1
	MVO@    R1,     R5
	SWAP    R1
	MVO@    R1,     R5
	MVI@    R4,     R1
	MVO@    R1,     R5
	SWAP    R1
	MVO@    R1,     R5
	MVI@    R4,     R1
	MVO@    R1,     R5
	SWAP    R1
	MVO@    R1,     R5
	MVI@    R4,     R1
	MVO@    R1,     R5
	SWAP    R1
	MVO@    R1,     R5
	DECR R0
	BNE @@vi3
	MVO R0,_gram_bitmap
@@vi1:
	MVI _gram2_bitmap,R4
	TSTR R4
	BEQ @@vii1
	MVI _gram2_target,R1
	SLL R1,2
	SLL R1,1
	ADDI #$3800,R1
	MOVR R1,R5
	MVI _gram2_total,R0
@@vii3:
	MVI@    R4,     R1
	MVO@    R1,     R5
	SWAP    R1
	MVO@    R1,     R5
	MVI@    R4,     R1
	MVO@    R1,     R5
	SWAP    R1
	MVO@    R1,     R5
	MVI@    R4,     R1
	MVO@    R1,     R5
	SWAP    R1
	MVO@    R1,     R5
	MVI@    R4,     R1
	MVO@    R1,     R5
	SWAP    R1
	MVO@    R1,     R5
	DECR R0
	BNE @@vii3
	MVO R0,_gram2_bitmap
@@vii1:

    IF intybasic_scroll
	;
	; Frame scroll support
	;
	MVI _scroll_d,R0
	TSTR R0
	BEQ @@vi4
	CLRR R1
	MVO R1,_scroll_d
	DECR R0     ; Left
	BEQ @@vi5
	DECR R0     ; Right
	BEQ @@vi6
	DECR R0     ; Top
	BEQ @@vi7
	DECR R0     ; Bottom
	BEQ @@vi8
	B @@vi4

@@vi5:  MVII #$0200,R4
	MOVR R4,R5
	INCR R5
	MVII #12,R1
@@vi12: MVI@ R4,R2
	MVI@ R4,R3
	REPEAT 8
	MVO@ R2,R5
	MVI@ R4,R2
	MVO@ R3,R5
	MVI@ R4,R3
	ENDR
	MVO@ R2,R5
	MVI@ R4,R2
	MVO@ R3,R5
	MVO@ R2,R5
	INCR R4
	INCR R5
	DECR R1
	BNE @@vi12
	B @@vi4

@@vi6:  MVII #$0201,R4
	MVII #$0200,R5
	MVII #12,R1
@@vi11:
	REPEAT 19
	MVI@ R4,R0
	MVO@ R0,R5
	ENDR
	INCR R4
	INCR R5
	DECR R1
	BNE @@vi11
	B @@vi4
    
	;
	; Complex routine to be ahead of STIC display
	; Moves first the top 6 lines, saves intermediate line
	; Then moves the bottom 6 lines and restores intermediate line
	;
@@vi7:  MVII #$0264,R4
	MVII #5,R1
	MVII #_scroll_buffer,R5
	REPEAT 20
	MVI@ R4,R0
	MVO@ R0,R5
	ENDR
	SUBI #40,R4
	MOVR R4,R5
	ADDI #20,R5
@@vi10:
	REPEAT 20
	MVI@ R4,R0
	MVO@ R0,R5
	ENDR
	SUBI #40,R4
	SUBI #40,R5
	DECR R1
	BNE @@vi10
	MVII #$02C8,R4
	MVII #$02DC,R5
	MVII #5,R1
@@vi13:
	REPEAT 20
	MVI@ R4,R0
	MVO@ R0,R5
	ENDR
	SUBI #40,R4
	SUBI #40,R5
	DECR R1
	BNE @@vi13
	MVII #_scroll_buffer,R4
	REPEAT 20
	MVI@ R4,R0
	MVO@ R0,R5
	ENDR
	B @@vi4

@@vi8:  MVII #$0214,R4
	MVII #$0200,R5
	MVII #$DC/4,R1
@@vi9:  
	REPEAT 4
	MVI@ R4,R0
	MVO@ R0,R5
	ENDR
	DECR R1
	BNE @@vi9
	B @@vi4

@@vi4:
    ENDI

    IF intybasic_voice
	;
	; Intellivoice support
	;
	CALL IV_ISR
    ENDI

	;
	; Random number generator
	;
	CALL _next_random

    IF intybasic_music
	; Generate sound for next frame
       	MVI _ntsc,R0
	RRC R0,1	 ; PAL?
	BNC @@vo98      ; Yes, always generate sound
	MVI _music_frame,R0
	TSTR R0
	BEQ @@vo16
@@vo98: CALL _generate_music
@@vo16:
    ENDI

	; Increase frame number
	MVI _frame,R0
	INCR R0
	MVO R0,_frame

	; This mark is for ON FRAME GOSUB support
	CALL label_PLAY_SOUND

	RETURN
	ENDP

	;
	; Generates the next random number
	;
_next_random:	PROC

MACRO _ROR
	RRC R0,1
	MOVR R0,R2
	SLR R2,2
	SLR R2,2
	ANDI #$0800,R2
	SLR R2,2
	SLR R2,2
	ANDI #$007F,R0
	XORR R2,R0
ENDM
	MVI _rand,R0
	SETC
	_ROR
	XOR _frame,R0
	_ROR
	XOR _rand,R0
	_ROR
	XORI #9,R0
	MVO R0,_rand
	JR R5
	ENDP

    IF intybasic_music

	;
	; Music player, comes from my game Princess Quest for Intellivision
	; so it's a practical tracker used in a real game ;) and with enough
	; features.
	;

	; NTSC frequency for notes (based on 3.579545 mhz)
ntsc_note_table:    PROC
	; Silence - 0
	DECLE 0
	; Octave 2 - 1
	DECLE 1721,1621,1532,1434,1364,1286,1216,1141,1076,1017,956,909
	; Octave 3 - 13
	DECLE 854,805,761,717,678,639,605,571,538,508,480,453
	; Octave 4 - 25
	DECLE 427,404,380,360,339,321,302,285,270,254,240,226
	; Octave 5 - 37
	DECLE 214,202,191,180,170,160,151,143,135,127,120,113
	; Octave 6 - 49
	DECLE 107,101,95,90,85,80,76,71,67,64,60,57
	; Octave 7 - 61
	DECLE 54
	; Space for two notes more
	ENDP

	; PAL frequency for notes (based on 4 mhz)
pal_note_table:    PROC
	; Silence - 0
	DECLE 0
	; Octava 2 - 1
	DECLE 1923,1812,1712,1603,1524,1437,1359,1276,1202,1136,1068,1016
	; Octava 3 - 13
	DECLE 954,899,850,801,758,714,676,638,601,568,536,506
	; Octava 4 - 25
	DECLE 477,451,425,402,379,358,338,319,301,284,268,253
	; Octava 5 - 37
	DECLE 239,226,213,201,190,179,169,159,150,142,134,127
	; Octava 6 - 49
	DECLE 120,113,106,100,95,89,84,80,75,71,67,63
	; Octava 7 - 61
	DECLE 60
	; Space for two notes more
	ENDP
    ENDI

	;
	; Music tracker init
	;
_init_music:	PROC
    IF intybasic_music
	MVI _ntsc,R0
	RRC R0,1
	MVII #ntsc_note_table,R0
	BC @@0
	MVII #pal_note_table,R0
@@0:	MVO R0,_music_table
	MVII #$38,R0	; $B8 blocks controllers o.O!
	MVO R0,_music_mix
    IF intybasic_music_ecs
	MVO R0,_music2_mix
    ENDI
	CLRR R0
    ELSE
	JR R5		; Tracker disabled (no PLAY statement used)
    ENDI
	ENDP

    IF intybasic_music
	;
	; Start music
	; R0 = Pointer to music
	;
_play_music:	PROC
	MVII #1,R1
	MOVR R1,R3
	MOVR R0,R2
	BEQ @@1
	MVI@ R2,R3
	INCR R2
@@1:	MVO R2,_music_p
	MVO R2,_music_start
	SWAP R2
	MVO R2,_music_start+1
	MVO R3,_music_t
	MVO R1,_music_tc
	JR R5

	ENDP

	;
	; Generate music
	;
_generate_music:	PROC
	BEGIN
	MVI _music_mix,R0
	ANDI #$C0,R0
	XORI #$38,R0
	MVO R0,_music_mix
    IF intybasic_music_ecs
	MVI _music2_mix,R0
	ANDI #$C0,R0
	XORI #$38,R0
	MVO R0,_music2_mix
    ENDI
	CLRR R1			; Turn off volume for the three sound channels
	MVO R1,_music_vol1
	MVO R1,_music_vol2
	MVI _music_tc,R3
	MVO R1,_music_vol3
    IF intybasic_music_ecs
	MVO R1,_music2_vol1
	NOP
	MVO R1,_music2_vol2
	MVO R1,_music2_vol3
    ENDI
	DECR R3
	MVO R3,_music_tc
	BNE @@6
	; R3 is zero from here up to @@6
	MVI _music_p,R4
@@15:	TSTR R4		; Silence?
	BEQ @@43	; Keep quiet
@@41:	MVI@ R4,R0
	MVI@ R4,R1
	MVI _music_t,R2
	CMPI #$FA00,R1	; Volume?
	BNC @@42
    IF intybasic_music_volume
	BEQ @@40
    ENDI
	CMPI #$FF00,R1	; Speed?
	BEQ @@39
	CMPI #$FB00,R1	; Return?
	BEQ @@38
	CMPI #$FC00,R1	; Gosub?
	BEQ @@37
	CMPI #$FE00,R1	; The end?
	BEQ @@36       ; Keep quiet
;	CMPI #$FD00,R1	; Repeat?
;	BNE @@42
	MVI _music_start+1,R0
	SWAP R0
	ADD _music_start,R0
	MOVR R0,R4
	B @@15

    IF intybasic_music_volume
@@40:	
	MVO R0,_music_vol
	B @@41
    ENDI

@@39:	MVO R0,_music_t
	MOVR R0,R2
	B @@41

@@38:	MVI _music_gosub,R4
	B @@15

@@37:	MVO R4,_music_gosub
@@36:	MOVR R0,R4	; Jump, zero will make it quiet
	B @@15

@@43:	MVII #1,R0
	MVO R0,_music_tc
	B @@0
	
@@42: 	MVO R2,_music_tc    ; Restart note time
     	MVO R4,_music_p
     	
	MOVR R0,R2
	ANDI #$FF,R2
	CMPI #$3F,R2	; Sustain note?
	BEQ @@1
	MOVR R2,R4
	ANDI #$3F,R4
	MVO R4,_music_n1	; Note
	MVO R3,_music_s1	; Waveform
	ANDI #$C0,R2
	MVO R2,_music_i1	; Instrument
	
@@1:	SWAP R0
	ANDI #$FF,R0
	CMPI #$3F,R0	; Sustain note?
	BEQ @@2
	MOVR R0,R4
	ANDI #$3F,R4
	MVO R4,_music_n2	; Note
	MVO R3,_music_s2	; Waveform
	ANDI #$C0,R0
	MVO R0,_music_i2	; Instrument
	
@@2:	MOVR R1,R2
	ANDI #$FF,R2
	CMPI #$3F,R2	; Sustain note?
	BEQ @@3
	MOVR R2,R4
	ANDI #$3F,R4
	MVO R4,_music_n3	; Note
	MVO R3,_music_s3	; Waveform
	ANDI #$C0,R2
	MVO R2,_music_i3	; Instrument
	
@@3:	SWAP R1
	MVO R1,_music_n4
	MVO R3,_music_s4
	
    IF intybasic_music_ecs
	MVI _music_p,R4
	MVI@ R4,R0
	MVI@ R4,R1
	MVO R4,_music_p

	MOVR R0,R2
	ANDI #$FF,R2
	CMPI #$3F,R2	; Sustain note?
	BEQ @@33
	MOVR R2,R4
	ANDI #$3F,R4
	MVO R4,_music_n5	; Note
	MVO R3,_music_s5	; Waveform
	ANDI #$C0,R2
	MVO R2,_music_i5	; Instrument
	
@@33:	SWAP R0
	ANDI #$FF,R0
	CMPI #$3F,R0	; Sustain note?
	BEQ @@34
	MOVR R0,R4
	ANDI #$3F,R4
	MVO R4,_music_n6	; Note
	MVO R3,_music_s6	; Waveform
	ANDI #$C0,R0
	MVO R0,_music_i6	; Instrument
	
@@34:	MOVR R1,R2
	ANDI #$FF,R2
	CMPI #$3F,R2	; Sustain note?
	BEQ @@35
	MOVR R2,R4
	ANDI #$3F,R4
	MVO R4,_music_n7	; Note
	MVO R3,_music_s7	; Waveform
	ANDI #$C0,R2
	MVO R2,_music_i7	; Instrument
	
@@35:	MOVR R1,R2
	SWAP R2
	MVO R2,_music_n8
	MVO R3,_music_s8
	
    ENDI

	;
	; Construct main voice
	;
@@6:	MVI _music_n1,R3	; Read note
	TSTR R3		; There is note?
	BEQ @@7		; No, jump
	MVI _music_s1,R1
	MVI _music_i1,R2
	MOVR R1,R0
	CALL _note2freq
	MVO R3,_music_freq10	; Note in voice A
	SWAP R3
	MVO R3,_music_freq11
	MVO R1,_music_vol1
	; Increase time for instrument waveform
	INCR R0
	CMPI #$18,R0
	BNE @@20
	SUBI #$08,R0
@@20:	MVO R0,_music_s1

@@7:	MVI _music_n2,R3	; Read note
	TSTR R3		; There is note?
	BEQ @@8		; No, jump
	MVI _music_s2,R1
	MVI _music_i2,R2
	MOVR R1,R0
	CALL _note2freq
	MVO R3,_music_freq20	; Note in voice B
	SWAP R3
	MVO R3,_music_freq21
	MVO R1,_music_vol2
	; Increase time for instrument waveform
	INCR R0
	CMPI #$18,R0
	BNE @@21
	SUBI #$08,R0
@@21:	MVO R0,_music_s2

@@8:	MVI _music_n3,R3	; Read note
	TSTR R3		; There is note?
	BEQ @@9		; No, jump
	MVI _music_s3,R1
	MVI _music_i3,R2
	MOVR R1,R0
	CALL _note2freq
	MVO R3,_music_freq30	; Note in voice C
	SWAP R3
	MVO R3,_music_freq31
	MVO R1,_music_vol3
	; Increase time for instrument waveform
	INCR R0
	CMPI #$18,R0
	BNE @@22
	SUBI #$08,R0
@@22:	MVO R0,_music_s3

@@9:	MVI _music_n4,R0	; Read drum
	DECR R0		; There is drum?
	BMI @@4		; No, jump
	MVI _music_s4,R1
	       		; 1 - Strong
	BNE @@5
	CMPI #3,R1
	BGE @@12
@@10:	MVII #5,R0
	MVO R0,_music_noise
	CALL _activate_drum
	B @@12

@@5:	DECR R0		;2 - Short
	BNE @@11
	TSTR R1
	BNE @@12
	MVII #8,R0
	MVO R0,_music_noise
	CALL _activate_drum
	B @@12

@@11:	;DECR R0	; 3 - Rolling
	;BNE @@12
	CMPI #2,R1
	BLT @@10
	MVI _music_t,R0
	SLR R0,1
	CMPR R0,R1
	BLT @@12
	ADDI #2,R0
	CMPR R0,R1
	BLT @@10
	; Increase time for drum waveform
@@12:   INCR R1
	MVO R1,_music_s4

@@4:
    IF intybasic_music_ecs
	;
	; Construct main voice
	;
	MVI _music_n5,R3	; Read note
	TSTR R3		; There is note?
	BEQ @@23	; No, jump
	MVI _music_s5,R1
	MVI _music_i5,R2
	MOVR R1,R0
	CALL _note2freq
	MVO R3,_music2_freq10	; Note in voice A
	SWAP R3
	MVO R3,_music2_freq11
	MVO R1,_music2_vol1
	; Increase time for instrument waveform
	INCR R0
	CMPI #$18,R0
	BNE @@24
	SUBI #$08,R0
@@24:	MVO R0,_music_s5

@@23:	MVI _music_n6,R3	; Read note
	TSTR R3		; There is note?
	BEQ @@25		; No, jump
	MVI _music_s6,R1
	MVI _music_i6,R2
	MOVR R1,R0
	CALL _note2freq
	MVO R3,_music2_freq20	; Note in voice B
	SWAP R3
	MVO R3,_music2_freq21
	MVO R1,_music2_vol2
	; Increase time for instrument waveform
	INCR R0
	CMPI #$18,R0
	BNE @@26
	SUBI #$08,R0
@@26:	MVO R0,_music_s6

@@25:	MVI _music_n7,R3	; Read note
	TSTR R3		; There is note?
	BEQ @@27		; No, jump
	MVI _music_s7,R1
	MVI _music_i7,R2
	MOVR R1,R0
	CALL _note2freq
	MVO R3,_music2_freq30	; Note in voice C
	SWAP R3
	MVO R3,_music2_freq31
	MVO R1,_music2_vol3
	; Increase time for instrument waveform
	INCR R0
	CMPI #$18,R0
	BNE @@28
	SUBI #$08,R0
@@28:	MVO R0,_music_s7

@@27:	MVI _music_n8,R0	; Read drum
	DECR R0		; There is drum?
	BMI @@0		; No, jump
	MVI _music_s8,R1
	       		; 1 - Strong
	BNE @@29
	CMPI #3,R1
	BGE @@31
@@32:	MVII #5,R0
	MVO R0,_music2_noise
	CALL _activate_drum_ecs
	B @@31

@@29:	DECR R0		;2 - Short
	BNE @@30
	TSTR R1
	BNE @@31
	MVII #8,R0
	MVO R0,_music2_noise
	CALL _activate_drum_ecs
	B @@31

@@30:	;DECR R0	; 3 - Rolling
	;BNE @@31
	CMPI #2,R1
	BLT @@32
	MVI _music_t,R0
	SLR R0,1
	CMPR R0,R1
	BLT @@31
	ADDI #2,R0
	CMPR R0,R1
	BLT @@32
	; Increase time for drum waveform
@@31:	INCR R1
	MVO R1,_music_s8

    ENDI
@@0:	RETURN
	ENDP

	;
	; Translates note number to frequency
	; R3 = Note
	; R1 = Position in waveform for instrument
	; R2 = Instrument
	;
_note2freq:	PROC
	ADD _music_table,R3
	MVI@ R3,R3
	SWAP R2
	BEQ _piano_instrument
	RLC R2,1
	BNC _clarinet_instrument
	BPL _flute_instrument
;	BMI _bass_instrument
	ENDP

	;
	; Generates a bass
	;
_bass_instrument:	PROC
	SLL R3,2	; Lower 2 octaves
	ADDI #_bass_volume,R1
	MVI@ R1,R1	; Bass effect
    IF intybasic_music_volume
	B _global_volume
    ELSE
	JR R5
    ENDI
	ENDP

_bass_volume:	PROC
	DECLE 12,13,14,14,13,12,12,12
	DECLE 11,11,12,12,11,11,12,12
	DECLE 11,11,12,12,11,11,12,12
	ENDP

	;
	; Generates a piano
	; R3 = Frequency
	; R1 = Waveform position
	;
	; Output:
	; R3 = Frequency.
	; R1 = Volume.
	;
_piano_instrument:	PROC
	ADDI #_piano_volume,R1
	MVI@ R1,R1
    IF intybasic_music_volume
	B _global_volume
    ELSE
	JR R5
    ENDI
	ENDP

_piano_volume:	PROC
	DECLE 14,13,13,12,12,11,11,10
	DECLE 10,9,9,8,8,7,7,6
	DECLE 6,6,7,7,6,6,5,5
	ENDP

	;
	; Generate a clarinet
	; R3 = Frequency
	; R1 = Waveform position
	;
	; Output:
	; R3 = Frequency
	; R1 = Volume
	;
_clarinet_instrument:	PROC
	ADDI #_clarinet_vibrato,R1
	ADD@ R1,R3
	CLRC
	RRC R3,1	; Duplicates frequency
	ADCR R3
	ADDI #_clarinet_volume-_clarinet_vibrato,R1
	MVI@ R1,R1
    IF intybasic_music_volume
	B _global_volume
    ELSE
	JR R5
    ENDI
	ENDP

_clarinet_vibrato:	PROC
	DECLE 0,0,0,0
	DECLE -2,-4,-2,0
	DECLE 2,4,2,0
	DECLE -2,-4,-2,0
	DECLE 2,4,2,0
	DECLE -2,-4,-2,0
	ENDP

_clarinet_volume:	PROC
	DECLE 13,14,14,13,13,12,12,12
	DECLE 11,11,11,11,12,12,12,12
	DECLE 11,11,11,11,12,12,12,12
	ENDP

	;
	; Generates a flute
	; R3 = Frequency
	; R1 = Waveform position
	;
	; Output:
	; R3 = Frequency
	; R1 = Volume
	;
_flute_instrument:	PROC
	ADDI #_flute_vibrato,R1
	ADD@ R1,R3
	ADDI #_flute_volume-_flute_vibrato,R1
	MVI@ R1,R1
    IF intybasic_music_volume
	B _global_volume
    ELSE
	JR R5
    ENDI
	ENDP

_flute_vibrato:	PROC
	DECLE 0,0,0,0
	DECLE 0,1,2,1
	DECLE 0,1,2,1
	DECLE 0,1,2,1
	DECLE 0,1,2,1
	DECLE 0,1,2,1
	ENDP
		 
_flute_volume:	PROC
	DECLE 10,12,13,13,12,12,12,12
	DECLE 11,11,11,11,10,10,10,10
	DECLE 11,11,11,11,10,10,10,10
	ENDP

    IF intybasic_music_volume

_global_volume:	PROC
	MVI _music_vol,R2
	ANDI #$0F,R2
	SLL R2,2
	SLL R2,2
	ADDR R1,R2
	ADDI #@@table,R2
	MVI@ R2,R1
	JR R5

@@table:
	DECLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DECLE 0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1
	DECLE 0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2
	DECLE 0,0,0,1,1,1,1,1,2,2,2,2,2,3,3,3
	DECLE 0,0,1,1,1,1,2,2,2,2,3,3,3,4,4,4
	DECLE 0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5
	DECLE 0,0,1,1,2,2,2,3,3,4,4,4,5,5,6,6
	DECLE 0,1,1,1,2,2,3,3,4,4,5,5,6,6,7,7
	DECLE 0,1,1,2,2,3,3,4,4,5,5,6,6,7,8,8
	DECLE 0,1,1,2,2,3,4,4,5,5,6,7,7,8,8,9
	DECLE 0,1,1,2,3,3,4,5,5,6,7,7,8,9,9,10
	DECLE 0,1,2,2,3,4,4,5,6,7,7,8,9,10,10,11
	DECLE 0,1,2,2,3,4,5,6,6,7,8,9,10,10,11,12
	DECLE 0,1,2,3,4,4,5,6,7,8,9,10,10,11,12,13
	DECLE 0,1,2,3,4,5,6,7,8,8,9,10,11,12,13,14
	DECLE 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15

	ENDP

    ENDI

    IF intybasic_music_ecs
	;
	; Emits sound for ECS
	;
_emit_sound_ecs:	PROC
	MOVR R5,R1
	MVI _music_mode,R2
	SARC R2,1
	BEQ @@6
	MVII #_music2_freq10,R4
	MVII #$00F0,R5
	B _emit_sound.0

@@6:	JR R1

	ENDP

    ENDI

	;
	; Emits sound
	;
_emit_sound:	PROC
	MOVR R5,R1
	MVI _music_mode,R2
	SARC R2,1
	BEQ @@6
	MVII #_music_freq10,R4
	MVII #$01F0,R5
@@0:
	MVI@ R4,R0
	MVO@ R0,R5	; $01F0 - Channel A Period (Low 8 bits of 12)
	MVI@ R4,R0
	MVO@ R0,R5	; $01F1 - Channel B Period (Low 8 bits of 12)
	DECR R2
	BEQ @@1
	MVI@ R4,R0	
	MVO@ R0,R5	; $01F2 - Channel C Period (Low 8 bits of 12)
	INCR R5		; Avoid $01F3 - Enveloped Period (Low 8 bits of 16)
	MVI@ R4,R0
	MVO@ R0,R5	; $01F4 - Channel A Period (High 4 bits of 12)
	MVI@ R4,R0
	MVO@ R0,R5	; $01F5 - Channel B Period (High 4 bits of 12)
	MVI@ R4,R0
	MVO@ R0,R5	; $01F6 - Channel C Period (High 4 bits of 12)
	INCR R5		; Avoid $01F7 - Envelope Period (High 8 bits of 16)
	BC @@2		; Jump if playing with drums
	ADDI #2,R4
	ADDI #3,R5
	B @@3

@@2:	MVI@ R4,R0
	MVO@ R0,R5	; $01F8 - Enable Noise/Tone (bits 3-5 Noise : 0-2 Tone)
	MVI@ R4,R0	
	MVO@ R0,R5	; $01F9 - Noise Period (5 bits)
	INCR R5		; Avoid $01FA - Envelope Type (4 bits)
@@3:	MVI@ R4,R0
	MVO@ R0,R5	; $01FB - Channel A Volume
	MVI@ R4,R0
	MVO@ R0,R5	; $01FC - Channel B Volume
	MVI@ R4,R0
	MVO@ R0,R5	; $01FD - Channel C Volume
	JR R1

@@1:	INCR R4		
	INCR R5		; Avoid $01F2 and $01F3
	INCR R5		; Cannot use ADDI
	MVI@ R4,R0
	MVO@ R0,R5	; $01F4 - Channel A Period (High 4 bits of 12)
	MVI@ R4,R0
	MVO@ R0,R5	; $01F5 - Channel B Period (High 4 bits of 12)
	INCR R4
	INCR R5		; Avoid $01F6 and $01F7
	INCR R5		; Cannot use ADDI
	BC @@4		; Jump if playing with drums
	ADDI #2,R4
	ADDI #3,R5
	B @@5

@@4:	MVI@ R4,R0
	MVO@ R0,R5	; $01F8 - Enable Noise/Tone (bits 3-5 Noise : 0-2 Tone)
	MVI@ R4,R0
	MVO@ R0,R5	; $01F9 - Noise Period (5 bits)
	INCR R5		; Avoid $01FA - Envelope Type (4 bits)
@@5:	MVI@ R4,R0
	MVO@ R0,R5	; $01FB - Channel A Volume
	MVI@ R4,R0
	MVO@ R0,R5	; $01FC - Channel B Volume
@@6:	JR R1
	ENDP

	;
	; Activates drum
	;
_activate_drum:	PROC
    IF intybasic_music_volume
	BEGIN
    ENDI
	MVI _music_mode,R2
	SARC R2,1	; PLAY NO DRUMS?
	BNC @@0		; Yes, jump
	MVI _music_vol1,R0
	TSTR R0
	BNE @@1
	MVII #11,R1
    IF intybasic_music_volume
	CALL _global_volume
    ENDI
	MVO R1,_music_vol1
	MVI _music_mix,R0
	ANDI #$F6,R0
	XORI #$01,R0
	MVO R0,_music_mix
    IF intybasic_music_volume
	RETURN
    ELSE
	JR R5
    ENDI

@@1:    MVI _music_vol2,R0
	TSTR R0
	BNE @@2
	MVII #11,R1
    IF intybasic_music_volume
	CALL _global_volume
    ENDI
	MVO R1,_music_vol2
	MVI _music_mix,R0
	ANDI #$ED,R0
	XORI #$02,R0
	MVO R0,_music_mix
    IF intybasic_music_volume
	RETURN
    ELSE
	JR R5
    ENDI

@@2:    DECR R2		; PLAY SIMPLE?
	BEQ @@3		; Yes, jump
	MVI _music_vol3,R0
	TSTR R0
	BNE @@3
	MVII #11,R1
    IF intybasic_music_volume
	CALL _global_volume
    ENDI
	MVO R1,_music_vol3
	MVI _music_mix,R0
	ANDI #$DB,R0
	XORI #$04,R0
	MVO R0,_music_mix
    IF intybasic_music_volume
	RETURN
    ELSE
	JR R5
    ENDI

@@3:    MVI _music_mix,R0
	ANDI #$EF,R0
	MVO R0,_music_mix
@@0:	
    IF intybasic_music_volume
	RETURN
    ELSE
	JR R5
    ENDI

	ENDP

    IF intybasic_music_ecs
	;
	; Activates drum
	;
_activate_drum_ecs:	PROC
    IF intybasic_music_volume
	BEGIN
    ENDI
	MVI _music_mode,R2
	SARC R2,1	; PLAY NO DRUMS?
	BNC @@0		; Yes, jump
	MVI _music2_vol1,R0
	TSTR R0
	BNE @@1
	MVII #11,R1
    IF intybasic_music_volume
	CALL _global_volume
    ENDI
	MVO R1,_music2_vol1
	MVI _music2_mix,R0
	ANDI #$F6,R0
	XORI #$01,R0
	MVO R0,_music2_mix
    IF intybasic_music_volume
	RETURN
    ELSE
	JR R5
    ENDI

@@1:    MVI _music2_vol2,R0
	TSTR R0
	BNE @@2
	MVII #11,R1
    IF intybasic_music_volume
	CALL _global_volume
    ENDI
	MVO R1,_music2_vol2
	MVI _music2_mix,R0
	ANDI #$ED,R0
	XORI #$02,R0
	MVO R0,_music2_mix
    IF intybasic_music_volume
	RETURN
    ELSE
	JR R5
    ENDI

@@2:    DECR R2		; PLAY SIMPLE?
	BEQ @@3		; Yes, jump
	MVI _music2_vol3,R0
	TSTR R0
	BNE @@3
	MVII #11,R1
    IF intybasic_music_volume
	CALL _global_volume
    ENDI
	MVO R1,_music2_vol3
	MVI _music2_mix,R0
	ANDI #$DB,R0
	XORI #$04,R0
	MVO R0,_music2_mix
    IF intybasic_music_volume
	RETURN
    ELSE
	JR R5
    ENDI

@@3:    MVI _music2_mix,R0
	ANDI #$EF,R0
	MVO R0,_music2_mix
@@0:	
    IF intybasic_music_volume
	RETURN
    ELSE
	JR R5
    ENDI

	ENDP

    ENDI

    ENDI
    
    IF intybasic_numbers

	;
	; Following code from as1600 libraries, prnum16.asm
	; Public domain by Joseph Zbiciak
	;

;* ======================================================================== *;
;*  These routines are placed into the public domain by their author.  All  *;
;*  copyright rights are hereby relinquished on the routines and data in    *;
;*  this file.  -- Joseph Zbiciak, 2008				     *;
;* ======================================================================== *;

;; ======================================================================== ;;
;;  _PW10								   ;;
;;      Lookup table holding the first 5 powers of 10 (1 thru 10000) as     ;;
;;      16-bit numbers.						     ;;
;; ======================================================================== ;;
_PW10   PROC    ; 0 thru 10000
	DECLE   10000, 1000, 100, 10, 1, 0
	ENDP

;; ======================================================================== ;;
;;  PRNUM16.l     -- Print an unsigned 16-bit number left-justified.	;;
;;  PRNUM16.b     -- Print an unsigned 16-bit number with leading blanks.   ;;
;;  PRNUM16.z     -- Print an unsigned 16-bit number with leading zeros.    ;;
;;									  ;;
;;  AUTHOR								  ;;
;;      Joseph Zbiciak  <im14u2c AT globalcrossing DOT net>		 ;;
;;									  ;;
;;  REVISION HISTORY							;;
;;      30-Mar-2003 Initial complete revision			       ;;
;;									  ;;
;;  INPUTS for all variants						 ;;
;;      R0  Number to print.						;;
;;      R2  Width of field.  Ignored by PRNUM16.l.			  ;;
;;      R3  Format word, added to digits to set the color.		  ;;
;;	  Note:  Bit 15 MUST be cleared when building with PRNUM32.       ;;
;;      R4  Pointer to location on screen to print number		   ;;
;;									  ;;
;;  OUTPUTS								 ;;
;;      R0  Zeroed							  ;;
;;      R1  Unmodified						      ;;
;;      R2  Unmodified						      ;;
;;      R3  Unmodified						      ;;
;;      R4  Points to first character after field.			  ;;
;;									  ;;
;;  DESCRIPTION							     ;;
;;      These routines print unsigned 16-bit numbers in a field up to 5     ;;
;;      positions wide.  The number is printed either in left-justified     ;;
;;      or right-justified format.  Right-justified numbers are padded      ;;
;;      with leading blanks or leading zeros.  Left-justified numbers       ;;
;;      are not padded on the right.					;;
;;									  ;;
;;      This code handles fields wider than 5 characters, padding with      ;;
;;      zeros or blanks as necessary.				       ;;
;;									  ;;
;;	      Routine      Value(hex)     Field	Output	     ;;
;;	      ----------   ----------   ----------   ----------	   ;;
;;	      PRNUM16.l      $0045	 n/a	"69"		;;
;;	      PRNUM16.b      $0045	  4	 "  69"	      ;;
;;	      PRNUM16.b      $0045	  6	 "    69"	    ;;
;;	      PRNUM16.z      $0045	  4	 "0069"	      ;;
;;	      PRNUM16.z      $0045	  6	 "000069"	    ;;
;;									  ;;
;;  TECHNIQUES							      ;;
;;      This routine uses repeated subtraction to divide the number	 ;;
;;      to display by various powers of 10.  This is cheaper than a	 ;;
;;      full divide, at least when the input number is large.  It's	 ;;
;;      also easier to get right.  :-)				      ;;
;;									  ;;
;;      The printing routine first pads out fields wider than 5 spaces      ;;
;;      with zeros or blanks as requested.  It then scans the power-of-10   ;;
;;      table looking for the first power of 10 that is <= the number to    ;;
;;      display.  While scanning for this power of 10, it outputs leading   ;;
;;      blanks or zeros, if requested.  This eliminates "leading digit"     ;;
;;      logic from the main digit loop.				     ;;
;;									  ;;
;;      Once in the main digit loop, we discover the value of each digit    ;;
;;      by repeated subtraction.  We build up our digit value while	 ;;
;;      subtracting the power-of-10 repeatedly.  We iterate until we go     ;;
;;      a step too far, and then we add back on power-of-10 to restore      ;;
;;      the remainder.						      ;;
;;									  ;;
;;  NOTES								   ;;
;;      The left-justified variant ignores field width.		     ;;
;;									  ;;
;;      The code is fully reentrant.					;;
;;									  ;;
;;      This code does not handle numbers which are too large to be	 ;;
;;      displayed in the provided field.  If the number is too large,       ;;
;;      non-digit characters will be displayed in the initial digit	 ;;
;;      position.  Also, the run time of this routine may get excessively   ;;
;;      large, depending on the magnitude of the overflow.		  ;;
;;									  ;;
;;      When using with PRNUM32, one must either include PRNUM32 before     ;;
;;      this function, or define the symbol _WITH_PRNUM32.  PRNUM32	 ;;
;;      needs a tiny bit of support from PRNUM16 to handle numbers in       ;;
;;      the range 65536...99999 correctly.				  ;;
;;									  ;;
;;  CODESIZE								;;
;;      73 words, including power-of-10 table			       ;;
;;      80 words, if compiled with PRNUM32.				 ;;
;;									  ;;
;;      To save code size, you can define the following symbols to omit     ;;
;;      some variants:						      ;;
;;									  ;;
;;	  _NO_PRNUM16.l:   Disables PRNUM16.l.  Saves 10 words	    ;;
;;	  _NO_PRNUM16.b:   Disables PRNUM16.b.  Saves 3 words.	    ;;
;;									  ;;
;;      Defining both symbols saves 17 words total, because it omits	;;
;;      some code shared by both routines.				  ;;
;;									  ;;
;;  STACK USAGE							     ;;
;;      This function uses up to 4 words of stack space.		    ;;
;; ======================================================================== ;;

PRNUM16 PROC

    
	;; ---------------------------------------------------------------- ;;
	;;  PRNUM16.l:  Print unsigned, left-justified.		     ;;
	;; ---------------------------------------------------------------- ;;
@@l:    PSHR    R5	      ; save return address
@@l1:   MVII    #$1,    R5      ; set R5 to 1 to counteract screen ptr update
				; in the 'find initial power of 10' loop
	PSHR    R2
	MVII    #5,     R2      ; force effective field width to 5.
	B       @@z2

	;; ---------------------------------------------------------------- ;;
	;;  PRNUM16.b:  Print unsigned with leading blanks.		 ;;
	;; ---------------------------------------------------------------- ;;
@@b:    PSHR    R5
@@b1:   CLRR    R5	      ; let the blank loop do its thing
	INCR    PC	      ; skip the PSHR R5

	;; ---------------------------------------------------------------- ;;
	;;  PRNUM16.z:  Print unsigned with leading zeros.		  ;;
	;; ---------------------------------------------------------------- ;;
@@z:    PSHR    R5
@@z1:   PSHR    R2
@@z2:   PSHR    R1

	;; ---------------------------------------------------------------- ;;
	;;  Find the initial power of 10 to use for display.		;;
	;;  Note:  For fields wider than 5, fill the extra spots above 5    ;;
	;;  with blanks or zeros as needed.				 ;;
	;; ---------------------------------------------------------------- ;;
	MVII    #_PW10+5,R1     ; Point to end of power-of-10 table
	SUBR    R2,     R1      ; Subtract the field width to get right power
	PSHR    R3	      ; save format word

	CMPI    #2,     R5      ; are we leading with zeros?
	BNC     @@lblnk	 ; no:  then do the loop w/ blanks

	CLRR    R5	      ; force R5==0
	ADDI    #$80,   R3      ; yes: do the loop with zeros
	B       @@lblnk
    

@@llp   MVO@    R3,     R4      ; print a blank/zero

	SUBR    R5,     R4      ; rewind pointer if needed.

	INCR    R1	      ; get next power of 10
@@lblnk DECR    R2	      ; decrement available digits
	BEQ     @@ldone
	CMPI    #5,     R2      ; field too wide?
	BGE     @@llp	   ; just force blanks/zeros 'till we're narrower.
	CMP@    R1,     R0      ; Is this power of 10 too big?
	BNC     @@llp	   ; Yes:  Put a blank and go to next

@@ldone PULR    R3	      ; restore format word

	;; ---------------------------------------------------------------- ;;
	;;  The digit loop prints at least one digit.  It discovers digits  ;;
	;;  by repeated subtraction.					;;
	;; ---------------------------------------------------------------- ;;
@@digit TSTR    R0	      ; If the number is zero, print zero and leave
	BNEQ    @@dig1	  ; no: print the number

	MOVR    R3,     R5      ;\    
	ADDI    #$80,   R5      ; |-- print a 0 there.
	MVO@    R5,     R4      ;/    
	B       @@done

@@dig1:
    
@@nxdig MOVR    R3,     R5      ; save display format word
@@cont: ADDI    #$80-8, R5      ; start our digit as one just before '0'
@@spcl:
 
	;; ---------------------------------------------------------------- ;;
	;;  Divide by repeated subtraction.  This divide is constructed     ;;
	;;  to go "one step too far" and then back up.		      ;;
	;; ---------------------------------------------------------------- ;;
@@div:  ADDI    #8,     R5      ; increment our digit
	SUB@    R1,     R0      ; subtract power of 10
	BC      @@div	   ; loop until we go too far
	ADD@    R1,     R0      ; add back the extra power of 10.

	MVO@    R5,     R4      ; display the digit.

	INCR    R1	      ; point to next power of 10
	DECR    R2	      ; any room left in field?
	BPL     @@nxdig	 ; keep going until R2 < 0.

@@done: PULR    R1	      ; restore R1
	PULR    R2	      ; restore R2
	PULR    PC	      ; return

	ENDP
	
    ENDI

    IF intybasic_voice
;;==========================================================================;;
;;  SP0256-AL2 Allophones						   ;;
;;									  ;;
;;  This file contains the allophone set that was obtained from an	  ;;
;;  SP0256-AL2.  It is being provided for your convenience.		 ;;
;;									  ;;
;;  The directory "al2" contains a series of assembly files, each one       ;;
;;  containing a single allophone.  This series of files may be useful in   ;;
;;  situations where space is at a premium.				 ;;
;;									  ;;
;;  Consult the Archer SP0256-AL2 documentation (under doc/programming)     ;;
;;  for more information about SP0256-AL2's allophone library.	      ;;
;;									  ;;
;; ------------------------------------------------------------------------ ;;
;;									  ;;
;;  Copyright information:						  ;;
;;									  ;;
;;  The allophone data below was extracted from the SP0256-AL2 ROM image.   ;;
;;  The SP0256-AL2 allophones are NOT in the public domain, nor are they    ;;
;;  placed under the GNU General Public License.  This program is	   ;;
;;  distributed in the hope that it will be useful, but WITHOUT ANY	 ;;
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or       ;;
;;  FITNESS FOR A PARTICULAR PURPOSE.				       ;;
;;									  ;;
;;  Microchip, Inc. retains the copyright to the data and algorithms	;;
;;  contained in the SP0256-AL2.  This speech data is distributed with      ;;
;;  explicit permission from Microchip, Inc.  All such redistributions      ;;
;;  must retain this notice of copyright.				   ;;
;;									  ;;
;;  No copyright claims are made on this data by the author(s) of SDK1600.  ;;
;;  Please see http://spatula-city.org/~im14u2c/sp0256-al2/ for details.    ;;
;;									  ;;
;;==========================================================================;;

;; ------------------------------------------------------------------------ ;;
_AA:
    DECLE   _AA.end - _AA - 1
    DECLE   $0318, $014C, $016F, $02CE, $03AF, $015F, $01B1, $008E
    DECLE   $0088, $0392, $01EA, $024B, $03AA, $039B, $000F, $0000
_AA.end:  ; 16 decles
;; ------------------------------------------------------------------------ ;;
_AE1:
    DECLE   _AE1.end - _AE1 - 1
    DECLE   $0118, $038E, $016E, $01FC, $0149, $0043, $026F, $036E
    DECLE   $01CC, $0005, $0000
_AE1.end:  ; 11 decles
;; ------------------------------------------------------------------------ ;;
_AO:
    DECLE   _AO.end - _AO - 1
    DECLE   $0018, $010E, $016F, $0225, $00C6, $02C4, $030F, $0160
    DECLE   $024B, $0005, $0000
_AO.end:  ; 11 decles
;; ------------------------------------------------------------------------ ;;
_AR:
    DECLE   _AR.end - _AR - 1
    DECLE   $0218, $010C, $016E, $001E, $000B, $0091, $032F, $00DE
    DECLE   $018B, $0095, $0003, $0238, $0027, $01E0, $03E8, $0090
    DECLE   $0003, $01C7, $0020, $03DE, $0100, $0190, $01CA, $02AB
    DECLE   $00B7, $004A, $0386, $0100, $0144, $02B6, $0024, $0320
    DECLE   $0011, $0041, $01DF, $0316, $014C, $016E, $001E, $00C4
    DECLE   $02B2, $031E, $0264, $02AA, $019D, $01BE, $000B, $00F0
    DECLE   $006A, $01CE, $00D6, $015B, $03B5, $03E4, $0000, $0380
    DECLE   $0007, $0312, $03E8, $030C, $016D, $02EE, $0085, $03C2
    DECLE   $03EC, $0283, $024A, $0005, $0000
_AR.end:  ; 69 decles
;; ------------------------------------------------------------------------ ;;
_AW:
    DECLE   _AW.end - _AW - 1
    DECLE   $0010, $01CE, $016E, $02BE, $0375, $034F, $0220, $0290
    DECLE   $008A, $026D, $013F, $01D5, $0316, $029F, $02E2, $018A
    DECLE   $0170, $0035, $00BD, $0000, $0000
_AW.end:  ; 21 decles
;; ------------------------------------------------------------------------ ;;
_AX:
    DECLE   _AX.end - _AX - 1
    DECLE   $0218, $02CD, $016F, $02F5, $0386, $00C2, $00CD, $0094
    DECLE   $010C, $0005, $0000
_AX.end:  ; 11 decles
;; ------------------------------------------------------------------------ ;;
_AY:
    DECLE   _AY.end - _AY - 1
    DECLE   $0110, $038C, $016E, $03B7, $03B3, $02AF, $0221, $009E
    DECLE   $01AA, $01B3, $00BF, $02E7, $025B, $0354, $00DA, $017F
    DECLE   $018A, $03F3, $00AF, $02D5, $0356, $027F, $017A, $01FB
    DECLE   $011E, $01B9, $03E5, $029F, $025A, $0076, $0148, $0124
    DECLE   $003D, $0000
_AY.end:  ; 34 decles
;; ------------------------------------------------------------------------ ;;
_BB1:
    DECLE   _BB1.end - _BB1 - 1
    DECLE   $0318, $004C, $016C, $00FB, $00C7, $0144, $002E, $030C
    DECLE   $010E, $018C, $01DC, $00AB, $00C9, $0268, $01F7, $021D
    DECLE   $01B3, $0098, $0000
_BB1.end:  ; 19 decles
;; ------------------------------------------------------------------------ ;;
_BB2:
    DECLE   _BB2.end - _BB2 - 1
    DECLE   $00F4, $0046, $0062, $0200, $0221, $03E4, $0087, $016F
    DECLE   $02A6, $02B7, $0212, $0326, $0368, $01BF, $0338, $0196
    DECLE   $0002
_BB2.end:  ; 17 decles
;; ------------------------------------------------------------------------ ;;
_CH:
    DECLE   _CH.end - _CH - 1
    DECLE   $00F5, $0146, $0052, $0000, $032A, $0049, $0032, $02F2
    DECLE   $02A5, $0000, $026D, $0119, $0124, $00F6, $0000
_CH.end:  ; 15 decles
;; ------------------------------------------------------------------------ ;;
_DD1:
    DECLE   _DD1.end - _DD1 - 1
    DECLE   $0318, $034C, $016E, $0397, $01B9, $0020, $02B1, $008E
    DECLE   $0349, $0291, $01D8, $0072, $0000
_DD1.end:  ; 13 decles
;; ------------------------------------------------------------------------ ;;
_DD2:
    DECLE   _DD2.end - _DD2 - 1
    DECLE   $00F4, $00C6, $00F2, $0000, $0129, $00A6, $0246, $01F3
    DECLE   $02C6, $02B7, $028E, $0064, $0362, $01CF, $0379, $01D5
    DECLE   $0002
_DD2.end:  ; 17 decles
;; ------------------------------------------------------------------------ ;;
_DH1:
    DECLE   _DH1.end - _DH1 - 1
    DECLE   $0018, $034F, $016D, $030B, $0306, $0363, $017E, $006A
    DECLE   $0164, $019E, $01DA, $00CB, $00E8, $027A, $03E8, $01D7
    DECLE   $0173, $00A1, $0000
_DH1.end:  ; 19 decles
;; ------------------------------------------------------------------------ ;;
_DH2:
    DECLE   _DH2.end - _DH2 - 1
    DECLE   $0119, $034C, $016D, $030B, $0306, $0363, $017E, $006A
    DECLE   $0164, $019E, $01DA, $00CB, $00E8, $027A, $03E8, $01D7
    DECLE   $0173, $00A1, $0000
_DH2.end:  ; 19 decles
;; ------------------------------------------------------------------------ ;;
_EH:
    DECLE   _EH.end - _EH - 1
    DECLE   $0218, $02CD, $016F, $0105, $014B, $0224, $02CF, $0274
    DECLE   $014C, $0005, $0000
_EH.end:  ; 11 decles
;; ------------------------------------------------------------------------ ;;
_EL:
    DECLE   _EL.end - _EL - 1
    DECLE   $0118, $038D, $016E, $011C, $008B, $03D2, $030F, $0262
    DECLE   $006C, $019D, $01CC, $022B, $0170, $0078, $03FE, $0018
    DECLE   $0183, $03A3, $010D, $016E, $012E, $00C6, $00C3, $0300
    DECLE   $0060, $000D, $0005, $0000
_EL.end:  ; 28 decles
;; ------------------------------------------------------------------------ ;;
_ER1:
    DECLE   _ER1.end - _ER1 - 1
    DECLE   $0118, $034C, $016E, $001C, $0089, $01C3, $034E, $03E6
    DECLE   $00AB, $0095, $0001, $0000, $03FC, $0381, $0000, $0188
    DECLE   $01DA, $00CB, $00E7, $0048, $03A6, $0244, $016C, $01A8
    DECLE   $03E4, $0000, $0002, $0001, $00FC, $01DA, $02E4, $0000
    DECLE   $0002, $0008, $0200, $0217, $0164, $0000, $000E, $0038
    DECLE   $0014, $01EA, $0264, $0000, $0002, $0048, $01EC, $02F1
    DECLE   $03CC, $016D, $021E, $0048, $00C2, $034E, $036A, $000D
    DECLE   $008D, $000B, $0200, $0047, $0022, $03A8, $0000, $0000
_ER1.end:  ; 64 decles
;; ------------------------------------------------------------------------ ;;
_ER2:
    DECLE   _ER2.end - _ER2 - 1
    DECLE   $0218, $034C, $016E, $001C, $0089, $01C3, $034E, $03E6
    DECLE   $00AB, $0095, $0001, $0000, $03FC, $0381, $0000, $0190
    DECLE   $01D8, $00CB, $00E7, $0058, $01A6, $0244, $0164, $02A9
    DECLE   $0024, $0000, $0000, $0007, $0201, $02F8, $02E4, $0000
    DECLE   $0002, $0001, $00FC, $02DA, $0024, $0000, $0002, $0008
    DECLE   $0200, $0217, $0024, $0000, $000E, $0038, $0014, $03EA
    DECLE   $03A4, $0000, $0002, $0048, $01EC, $03F1, $038C, $016D
    DECLE   $021E, $0048, $00C2, $034E, $036A, $000D, $009D, $0003
    DECLE   $0200, $0047, $0022, $03A8, $0000, $0000
_ER2.end:  ; 70 decles
;; ------------------------------------------------------------------------ ;;
_EY:
    DECLE   _EY.end - _EY - 1
    DECLE   $0310, $038C, $016E, $02A7, $00BB, $0160, $0290, $0094
    DECLE   $01CA, $03A9, $00C1, $02D7, $015B, $01D4, $03CE, $02FF
    DECLE   $00EA, $03E7, $0041, $0277, $025B, $0355, $03C9, $0103
    DECLE   $02EA, $03E4, $003F, $0000
_EY.end:  ; 28 decles
;; ------------------------------------------------------------------------ ;;
_FF:
    DECLE   _FF.end - _FF - 1
    DECLE   $0119, $03C8, $0000, $00A7, $0094, $0138, $01C6, $0000
_FF.end:  ; 8 decles
;; ------------------------------------------------------------------------ ;;
_GG1:
    DECLE   _GG1.end - _GG1 - 1
    DECLE   $00F4, $00C6, $00C2, $0200, $0015, $03FE, $0283, $01FD
    DECLE   $01E6, $00B7, $030A, $0364, $0331, $017F, $033D, $0215
    DECLE   $0002
_GG1.end:  ; 17 decles
;; ------------------------------------------------------------------------ ;;
_GG2:
    DECLE   _GG2.end - _GG2 - 1
    DECLE   $00F4, $0106, $0072, $0300, $0021, $0308, $0039, $0173
    DECLE   $00C6, $00B7, $037E, $03A3, $0319, $0177, $0036, $0217
    DECLE   $0002
_GG2.end:  ; 17 decles
;; ------------------------------------------------------------------------ ;;
_GG3:
    DECLE   _GG3.end - _GG3 - 1
    DECLE   $00F8, $0146, $00F2, $0100, $0132, $03A8, $0055, $01F5
    DECLE   $00A6, $02B7, $0291, $0326, $0368, $0167, $023A, $01C6
    DECLE   $0002
_GG3.end:  ; 17 decles
;; ------------------------------------------------------------------------ ;;
_HH1:
    DECLE   _HH1.end - _HH1 - 1
    DECLE   $0218, $01C9, $0000, $0095, $0127, $0060, $01D6, $0213
    DECLE   $0002, $01AE, $033E, $01A0, $03C4, $0122, $0001, $0218
    DECLE   $01E4, $03FD, $0019, $0000
_HH1.end:  ; 20 decles
;; ------------------------------------------------------------------------ ;;
_HH2:
    DECLE   _HH2.end - _HH2 - 1
    DECLE   $0218, $00CB, $0000, $0086, $000F, $0240, $0182, $031A
    DECLE   $02DB, $0008, $0293, $0067, $00BD, $01E0, $0092, $000C
    DECLE   $0000
_HH2.end:  ; 17 decles
;; ------------------------------------------------------------------------ ;;
_IH:
    DECLE   _IH.end - _IH - 1
    DECLE   $0118, $02CD, $016F, $0205, $0144, $02C3, $00FE, $031A
    DECLE   $000D, $0005, $0000
_IH.end:  ; 11 decles
;; ------------------------------------------------------------------------ ;;
_IY:
    DECLE   _IY.end - _IY - 1
    DECLE   $0318, $02CC, $016F, $0008, $030B, $01C3, $0330, $0178
    DECLE   $002B, $019D, $01F6, $018B, $01E1, $0010, $020D, $0358
    DECLE   $015F, $02A4, $02CC, $016F, $0109, $030B, $0193, $0320
    DECLE   $017A, $034C, $009C, $0017, $0001, $0200, $03C1, $0020
    DECLE   $00A7, $001D, $0001, $0104, $003D, $0040, $01A7, $01CA
    DECLE   $018B, $0160, $0078, $01F6, $0343, $01C7, $0090, $0000
_IY.end:  ; 48 decles
;; ------------------------------------------------------------------------ ;;
_JH:
    DECLE   _JH.end - _JH - 1
    DECLE   $0018, $0149, $0001, $00A4, $0321, $0180, $01F4, $039A
    DECLE   $02DC, $023C, $011A, $0047, $0200, $0001, $018E, $034E
    DECLE   $0394, $0356, $02C1, $010C, $03FD, $0129, $00B7, $01BA
    DECLE   $0000
_JH.end:  ; 25 decles
;; ------------------------------------------------------------------------ ;;
_KK1:
    DECLE   _KK1.end - _KK1 - 1
    DECLE   $00F4, $00C6, $00D2, $0000, $023A, $03E0, $02D1, $02E5
    DECLE   $0184, $0200, $0041, $0210, $0188, $00C5, $0000
_KK1.end:  ; 15 decles
;; ------------------------------------------------------------------------ ;;
_KK2:
    DECLE   _KK2.end - _KK2 - 1
    DECLE   $021D, $023C, $0211, $003C, $0180, $024D, $0008, $032B
    DECLE   $025B, $002D, $01DC, $01E3, $007A, $0000
_KK2.end:  ; 14 decles
;; ------------------------------------------------------------------------ ;;
_KK3:
    DECLE   _KK3.end - _KK3 - 1
    DECLE   $00F7, $0046, $01D2, $0300, $0131, $006C, $006E, $00F1
    DECLE   $00E4, $0000, $025A, $010D, $0110, $01F9, $014A, $0001
    DECLE   $00B5, $01A2, $00D8, $01CE, $0000
_KK3.end:  ; 21 decles
;; ------------------------------------------------------------------------ ;;
_LL:
    DECLE   _LL.end - _LL - 1
    DECLE   $0318, $038C, $016D, $029E, $0333, $0260, $0221, $0294
    DECLE   $01C4, $0299, $025A, $00E6, $014C, $012C, $0031, $0000
_LL.end:  ; 16 decles
;; ------------------------------------------------------------------------ ;;
_MM:
    DECLE   _MM.end - _MM - 1
    DECLE   $0210, $034D, $016D, $03F5, $00B0, $002E, $0220, $0290
    DECLE   $03CE, $02B6, $03AA, $00F3, $00CF, $015D, $016E, $0000
_MM.end:  ; 16 decles
;; ------------------------------------------------------------------------ ;;
_NG1:
    DECLE   _NG1.end - _NG1 - 1
    DECLE   $0118, $03CD, $016E, $00DC, $032F, $01BF, $01E0, $0116
    DECLE   $02AB, $029A, $0358, $01DB, $015B, $01A7, $02FD, $02B1
    DECLE   $03D2, $0356, $0000
_NG1.end:  ; 19 decles
;; ------------------------------------------------------------------------ ;;
_NN1:
    DECLE   _NN1.end - _NN1 - 1
    DECLE   $0318, $03CD, $016C, $0203, $0306, $03C3, $015F, $0270
    DECLE   $002A, $009D, $000D, $0248, $01B4, $0120, $01E1, $00C8
    DECLE   $0003, $0040, $0000, $0080, $015F, $0006, $0000
_NN1.end:  ; 23 decles
;; ------------------------------------------------------------------------ ;;
_NN2:
    DECLE   _NN2.end - _NN2 - 1
    DECLE   $0018, $034D, $016D, $0203, $0306, $03C3, $015F, $0270
    DECLE   $002A, $0095, $0003, $0248, $01B4, $0120, $01E1, $0090
    DECLE   $000B, $0040, $0000, $0080, $015F, $019E, $01F6, $028B
    DECLE   $00E0, $0266, $03F6, $01D8, $0143, $01A8, $0024, $00C0
    DECLE   $0080, $0000, $01E6, $0321, $0024, $0260, $000A, $0008
    DECLE   $03FE, $0000, $0000
_NN2.end:  ; 43 decles
;; ------------------------------------------------------------------------ ;;
_OR2:
    DECLE   _OR2.end - _OR2 - 1
    DECLE   $0218, $018C, $016D, $02A6, $03AB, $004F, $0301, $0390
    DECLE   $02EA, $0289, $0228, $0356, $01CF, $02D5, $0135, $007D
    DECLE   $02B5, $02AF, $024A, $02E2, $0153, $0167, $0333, $02A9
    DECLE   $02B3, $039A, $0351, $0147, $03CD, $0339, $02DA, $0000
_OR2.end:  ; 32 decles
;; ------------------------------------------------------------------------ ;;
_OW:
    DECLE   _OW.end - _OW - 1
    DECLE   $0310, $034C, $016E, $02AE, $03B1, $00CF, $0304, $0192
    DECLE   $018A, $022B, $0041, $0277, $015B, $0395, $03D1, $0082
    DECLE   $03CE, $00B6, $03BB, $02DA, $0000
_OW.end:  ; 21 decles
;; ------------------------------------------------------------------------ ;;
_OY:
    DECLE   _OY.end - _OY - 1
    DECLE   $0310, $014C, $016E, $02A6, $03AF, $00CF, $0304, $0192
    DECLE   $03CA, $01A8, $007F, $0155, $02B4, $027F, $00E2, $036A
    DECLE   $031F, $035D, $0116, $01D5, $02F4, $025F, $033A, $038A
    DECLE   $014F, $01B5, $03D5, $0297, $02DA, $03F2, $0167, $0124
    DECLE   $03FB, $0001
_OY.end:  ; 34 decles
;; ------------------------------------------------------------------------ ;;
_PA1:
    DECLE   _PA1.end - _PA1 - 1
    DECLE   $00F1, $0000
_PA1.end:  ; 2 decles
;; ------------------------------------------------------------------------ ;;
_PA2:
    DECLE   _PA2.end - _PA2 - 1
    DECLE   $00F4, $0000
_PA2.end:  ; 2 decles
;; ------------------------------------------------------------------------ ;;
_PA3:
    DECLE   _PA3.end - _PA3 - 1
    DECLE   $00F7, $0000
_PA3.end:  ; 2 decles
;; ------------------------------------------------------------------------ ;;
_PA4:
    DECLE   _PA4.end - _PA4 - 1
    DECLE   $00FF, $0000
_PA4.end:  ; 2 decles
;; ------------------------------------------------------------------------ ;;
_PA5:
    DECLE   _PA5.end - _PA5 - 1
    DECLE   $031D, $003F, $0000
_PA5.end:  ; 3 decles
;; ------------------------------------------------------------------------ ;;
_PP:
    DECLE   _PP.end - _PP - 1
    DECLE   $00FD, $0106, $0052, $0000, $022A, $03A5, $0277, $035F
    DECLE   $0184, $0000, $0055, $0391, $00EB, $00CF, $0000
_PP.end:  ; 15 decles
;; ------------------------------------------------------------------------ ;;
_RR1:
    DECLE   _RR1.end - _RR1 - 1
    DECLE   $0118, $01CD, $016C, $029E, $0171, $038E, $01E0, $0190
    DECLE   $0245, $0299, $01AA, $02E2, $01C7, $02DE, $0125, $00B5
    DECLE   $02C5, $028F, $024E, $035E, $01CB, $02EC, $0005, $0000
_RR1.end:  ; 24 decles
;; ------------------------------------------------------------------------ ;;
_RR2:
    DECLE   _RR2.end - _RR2 - 1
    DECLE   $0218, $03CC, $016C, $030C, $02C8, $0393, $02CD, $025E
    DECLE   $008A, $019D, $01AC, $02CB, $00BE, $0046, $017E, $01C2
    DECLE   $0174, $00A1, $01E5, $00E0, $010E, $0007, $0313, $0017
    DECLE   $0000
_RR2.end:  ; 25 decles
;; ------------------------------------------------------------------------ ;;
_SH:
    DECLE   _SH.end - _SH - 1
    DECLE   $0218, $0109, $0000, $007A, $0187, $02E0, $03F6, $0311
    DECLE   $0002, $0126, $0242, $0161, $03E9, $0219, $016C, $0300
    DECLE   $0013, $0045, $0124, $0005, $024C, $005C, $0182, $03C2
    DECLE   $0001
_SH.end:  ; 25 decles
;; ------------------------------------------------------------------------ ;;
_SS:
    DECLE   _SS.end - _SS - 1
    DECLE   $0218, $01CA, $0001, $0128, $001C, $0149, $01C6, $0000
_SS.end:  ; 8 decles
;; ------------------------------------------------------------------------ ;;
_TH:
    DECLE   _TH.end - _TH - 1
    DECLE   $0019, $0349, $0000, $00C6, $0212, $01D8, $01CA, $0000
_TH.end:  ; 8 decles
;; ------------------------------------------------------------------------ ;;
_TT1:
    DECLE   _TT1.end - _TT1 - 1
    DECLE   $00F6, $0046, $0142, $0100, $0042, $0088, $027E, $02EF
    DECLE   $01A4, $0200, $0049, $0290, $00FC, $00E8, $0000
_TT1.end:  ; 15 decles
;; ------------------------------------------------------------------------ ;;
_TT2:
    DECLE   _TT2.end - _TT2 - 1
    DECLE   $00F5, $00C6, $01D2, $0100, $0335, $00E9, $0042, $027A
    DECLE   $02A4, $0000, $0062, $01D1, $014C, $03EA, $02EC, $01E0
    DECLE   $0007, $03A7, $0000
_TT2.end:  ; 19 decles
;; ------------------------------------------------------------------------ ;;
_UH:
    DECLE   _UH.end - _UH - 1
    DECLE   $0018, $034E, $016E, $01FF, $0349, $00D2, $003C, $030C
    DECLE   $008B, $0005, $0000
_UH.end:  ; 11 decles
;; ------------------------------------------------------------------------ ;;
_UW1:
    DECLE   _UW1.end - _UW1 - 1
    DECLE   $0318, $014C, $016F, $029E, $03BD, $03BD, $0271, $0212
    DECLE   $0325, $0291, $016A, $027B, $014A, $03B4, $0133, $0001
_UW1.end:  ; 16 decles
;; ------------------------------------------------------------------------ ;;
_UW2:
    DECLE   _UW2.end - _UW2 - 1
    DECLE   $0018, $034E, $016E, $02F6, $0107, $02C2, $006D, $0090
    DECLE   $03AC, $01A4, $01DC, $03AB, $0128, $0076, $03E6, $0119
    DECLE   $014F, $03A6, $03A5, $0020, $0090, $0001, $02EE, $00BB
    DECLE   $0000
_UW2.end:  ; 25 decles
;; ------------------------------------------------------------------------ ;;
_VV:
    DECLE   _VV.end - _VV - 1
    DECLE   $0218, $030D, $016C, $010B, $010B, $0095, $034F, $03E4
    DECLE   $0108, $01B5, $01BE, $028B, $0160, $00AA, $03E4, $0106
    DECLE   $00EB, $02DE, $014C, $016E, $00F6, $0107, $00D2, $00CD
    DECLE   $0296, $00E4, $0006, $0000
_VV.end:  ; 28 decles
;; ------------------------------------------------------------------------ ;;
_WH:
    DECLE   _WH.end - _WH - 1
    DECLE   $0218, $00C9, $0000, $0084, $038E, $0147, $03A4, $0195
    DECLE   $0000, $012E, $0118, $0150, $02D1, $0232, $01B7, $03F1
    DECLE   $0237, $01C8, $03B1, $0227, $01AE, $0254, $0329, $032D
    DECLE   $01BF, $0169, $019A, $0307, $0181, $028D, $0000
_WH.end:  ; 31 decles
;; ------------------------------------------------------------------------ ;;
_WW:
    DECLE   _WW.end - _WW - 1
    DECLE   $0118, $034D, $016C, $00FA, $02C7, $0072, $03CC, $0109
    DECLE   $000B, $01AD, $019E, $016B, $0130, $0278, $01F8, $0314
    DECLE   $017E, $029E, $014D, $016D, $0205, $0147, $02E2, $001A
    DECLE   $010A, $026E, $0004, $0000
_WW.end:  ; 28 decles
;; ------------------------------------------------------------------------ ;;
_XR2:
    DECLE   _XR2.end - _XR2 - 1
    DECLE   $0318, $034C, $016E, $02A6, $03BB, $002F, $0290, $008E
    DECLE   $004B, $0392, $01DA, $024B, $013A, $01DA, $012F, $00B5
    DECLE   $02E5, $0297, $02DC, $0372, $014B, $016D, $0377, $00E7
    DECLE   $0376, $038A, $01CE, $026B, $02FA, $01AA, $011E, $0071
    DECLE   $00D5, $0297, $02BC, $02EA, $01C7, $02D7, $0135, $0155
    DECLE   $01DD, $0007, $0000
_XR2.end:  ; 43 decles
;; ------------------------------------------------------------------------ ;;
_YR:
    DECLE   _YR.end - _YR - 1
    DECLE   $0318, $03CC, $016E, $0197, $00FD, $0130, $0270, $0094
    DECLE   $0328, $0291, $0168, $007E, $01CC, $02F5, $0125, $02B5
    DECLE   $00F4, $0298, $01DA, $03F6, $0153, $0126, $03B9, $00AB
    DECLE   $0293, $03DB, $0175, $01B9, $0001
_YR.end:  ; 29 decles
;; ------------------------------------------------------------------------ ;;
_YY1:
    DECLE   _YY1.end - _YY1 - 1
    DECLE   $0318, $01CC, $016E, $0015, $00CB, $0263, $0320, $0078
    DECLE   $01CE, $0094, $001F, $0040, $0320, $03BF, $0230, $00A7
    DECLE   $000F, $01FE, $03FC, $01E2, $00D0, $0089, $000F, $0248
    DECLE   $032B, $03FD, $01CF, $0001, $0000
_YY1.end:  ; 29 decles
;; ------------------------------------------------------------------------ ;;
_YY2:
    DECLE   _YY2.end - _YY2 - 1
    DECLE   $0318, $01CC, $016E, $0015, $00CB, $0263, $0320, $0078
    DECLE   $01CE, $0094, $001F, $0040, $0320, $03BF, $0230, $00A7
    DECLE   $000F, $01FE, $03FC, $01E2, $00D0, $0089, $000F, $0248
    DECLE   $032B, $03FD, $01CF, $0199, $01EE, $008B, $0161, $0232
    DECLE   $0004, $0318, $01A7, $0198, $0124, $03E0, $0001, $0001
    DECLE   $030F, $0027, $0000
_YY2.end:  ; 43 decles
;; ------------------------------------------------------------------------ ;;
_ZH:
    DECLE   _ZH.end - _ZH - 1
    DECLE   $0310, $014D, $016E, $00C3, $03B9, $01BF, $0241, $0012
    DECLE   $0163, $00E1, $0000, $0080, $0084, $023F, $003F, $0000
_ZH.end:  ; 16 decles
;; ------------------------------------------------------------------------ ;;
_ZZ:
    DECLE   _ZZ.end - _ZZ - 1
    DECLE   $0218, $010D, $016F, $0225, $0351, $00B5, $02A0, $02EE
    DECLE   $00E9, $014D, $002C, $0360, $0008, $00EC, $004C, $0342
    DECLE   $03D4, $0156, $0052, $0131, $0008, $03B0, $01BE, $0172
    DECLE   $0000
_ZZ.end:  ; 25 decles

;;==========================================================================;;
;;									  ;;
;;  Copyright information:						  ;;
;;									  ;;
;;  The above allophone data was extracted from the SP0256-AL2 ROM image.   ;;
;;  The SP0256-AL2 allophones are NOT in the public domain, nor are they    ;;
;;  placed under the GNU General Public License.  This program is	   ;;
;;  distributed in the hope that it will be useful, but WITHOUT ANY	 ;;
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or       ;;
;;  FITNESS FOR A PARTICULAR PURPOSE.				       ;;
;;									  ;;
;;  Microchip, Inc. retains the copyright to the data and algorithms	;;
;;  contained in the SP0256-AL2.  This speech data is distributed with      ;;
;;  explicit permission from Microchip, Inc.  All such redistributions      ;;
;;  must retain this notice of copyright.				   ;;
;;									  ;;
;;  No copyright claims are made on this data by the author(s) of SDK1600.  ;;
;;  Please see http://spatula-city.org/~im14u2c/sp0256-al2/ for details.    ;;
;;									  ;;
;;==========================================================================;;

;* ======================================================================== *;
;*  These routines are placed into the public domain by their author.  All  *;
;*  copyright rights are hereby relinquished on the routines and data in    *;
;*  this file.  -- Joseph Zbiciak, 2008				     *;
;* ======================================================================== *;

;; ======================================================================== ;;
;;  INTELLIVOICE DRIVER ROUTINES					    ;;
;;  Written in 2002 by Joe Zbiciak <intvnut AT gmail.com>		   ;;
;;  http://spatula-city.org/~im14u2c/intv/				  ;;
;; ======================================================================== ;;

;; ======================================================================== ;;
;;  GLOBAL VARIABLES USED BY THESE ROUTINES				 ;;
;;									  ;;
;;  Note that some of these routines may use one or more global variables.  ;;
;;  If you use these routines, you will need to allocate the appropriate    ;;
;;  space in either 16-bit or 8-bit memory as appropriate.  Each global     ;;
;;  variable is listed with the routines which use it and the required      ;;
;;  memory width.							   ;;
;;									  ;;
;;  Example declarations for these routines are shown below, commented out. ;;
;;  You should uncomment these and add them to your program to make use of  ;;
;;  the routine that needs them.  Make sure to assign these variables to    ;;
;;  locations that aren't used for anything else.			   ;;
;; ======================================================================== ;;

			; Used by       Req'd Width     Description
			;-----------------------------------------------------
;IV.QH      EQU $110    ; IV_xxx	8-bit	   Voice queue head
;IV.QT      EQU $111    ; IV_xxx	8-bit	   Voice queue tail
;IV.Q       EQU $112    ; IV_xxx	8-bit	   Voice queue  (8 bytes)
;IV.FLEN    EQU $11A    ; IV_xxx	8-bit	   Length of FIFO data
;IV.FPTR    EQU $320    ; IV_xxx	16-bit	  Current FIFO ptr.
;IV.PPTR    EQU $321    ; IV_xxx	16-bit	  Current Phrase ptr.

;; ======================================================================== ;;
;;  MEMORY USAGE							    ;;
;;									  ;;
;;  These routines implement a queue of "pending phrases" that will be      ;;
;;  played by the Intellivoice.  The user calls IV_PLAY to enqueue a	;;
;;  phrase number.  Phrase numbers indicate either a RESROM sample or       ;;
;;  a compiled in phrase to be spoken.				      ;;
;;									  ;;
;;  The user must compose an "IV_PHRASE_TBL", which is composed of	  ;;
;;  pointers to phrases to be spoken.  Phrases are strings of pointers      ;;
;;  and RESROM triggers, terminated by a NUL.			       ;;
;;									  ;;
;;  Phrase numbers 1 through 42 are RESROM samples.  Phrase numbers	 ;;
;;  43 through 255 index into the IV_PHRASE_TBL.			    ;;
;;									  ;;
;;  SPECIAL NOTES							   ;;
;;									  ;;
;;  Bit 7 of IV.QH and IV.QT is used to denote whether the Intellivoice     ;;
;;  is present.  If Intellivoice is present, this bit is clear.	     ;;
;;									  ;;
;;  Bit 6 of IV.QT is used to denote that we still need to do an ALD $00    ;;
;;  for FIFO'd voice data.						  ;;
;; ======================================================================== ;;
	    

;; ======================================================================== ;;
;;  NAME								    ;;
;;      IV_INIT     Initialize the Intellivoice			     ;;
;;									  ;;
;;  AUTHOR								  ;;
;;      Joseph Zbiciak <intvnut AT gmail.com>			       ;;
;;									  ;;
;;  REVISION HISTORY							;;
;;      15-Sep-2002 Initial revision . . . . . . . . . . .  J. Zbiciak      ;;
;;									  ;;
;;  INPUTS for IV_INIT						      ;;
;;      R5      Return address					      ;;
;;									  ;;
;;  OUTPUTS								 ;;
;;      R0      0 if Intellivoice found, -1 if not.			 ;;
;;									  ;;
;;  DESCRIPTION							     ;;
;;      Resets Intellivoice, determines if it is actually there, and	;;
;;      then initializes the IV structure.				  ;;
;; ------------------------------------------------------------------------ ;;
;;		   Copyright (c) 2002, Joseph Zbiciak		     ;;
;; ======================================================================== ;;

IV_INIT     PROC
	    MVII    #$0400, R0	  ;
	    MVO     R0,     $0081       ; Reset the Intellivoice

	    MVI     $0081,  R0	  ; \
	    RLC     R0,     2	   ;  |-- See if we detect Intellivoice
	    BOV     @@no_ivoice	 ; /    once we've reset it.

	    CLRR    R0		  ; 
	    MVO     R0,     IV.FPTR     ; No data for FIFO
	    MVO     R0,     IV.PPTR     ; No phrase being spoken
	    MVO     R0,     IV.QH       ; Clear our queue
	    MVO     R0,     IV.QT       ; Clear our queue
	    JR      R5		  ; Done!

@@no_ivoice:
	    CLRR    R0
	    MVO     R0,     IV.FPTR     ; No data for FIFO
	    MVO     R0,     IV.PPTR     ; No phrase being spoken
	    DECR    R0
	    MVO     R0,     IV.QH       ; Set queue to -1 ("No Intellivoice")
	    MVO     R0,     IV.QT       ; Set queue to -1 ("No Intellivoice")
;	    JR      R5		 ; Done!
	    B       _wait	       ; Special for IntyBASIC!
	    ENDP

;; ======================================================================== ;;
;;  NAME								    ;;
;;      IV_ISR      Interrupt service routine to feed Intellivoice	  ;;
;;									  ;;
;;  AUTHOR								  ;;
;;      Joseph Zbiciak <intvnut AT gmail.com>			       ;;
;;									  ;;
;;  REVISION HISTORY							;;
;;      15-Sep-2002 Initial revision . . . . . . . . . . .  J. Zbiciak      ;;
;;									  ;;
;;  INPUTS for IV_ISR						       ;;
;;      R5      Return address					      ;;
;;									  ;;
;;  OUTPUTS								 ;;
;;      R0, R1, R4 trashed.						 ;;
;;									  ;;
;;  NOTES								   ;;
;;      Call this from your main interrupt service routine.		 ;;
;; ------------------------------------------------------------------------ ;;
;;		   Copyright (c) 2002, Joseph Zbiciak		     ;;
;; ======================================================================== ;;
IV_ISR      PROC
	    ;; ------------------------------------------------------------ ;;
	    ;;  Check for Intellivoice.  Leave if none present.	     ;;
	    ;; ------------------------------------------------------------ ;;
	    MVI     IV.QT,  R1	  ; Get queue tail
	    SWAP    R1,     2
	    BPL     @@ok		; Bit 7 set? If yes: No Intellivoice
@@ald_busy:
@@leave     JR      R5		  ; Exit if no Intellivoice.

     
	    ;; ------------------------------------------------------------ ;;
	    ;;  Check to see if we pump samples into the FIFO.
	    ;; ------------------------------------------------------------ ;;
@@ok:       MVI     IV.FPTR, R4	 ; Get FIFO data pointer
	    TSTR    R4		  ; is it zero?
	    BEQ     @@no_fifodata       ; Yes:  No data for FIFO.
@@fifo_fill:
	    MVI     $0081,  R0	  ; Read speech FIFO ready bit
	    SLLC    R0,     1	   ; 
	    BC      @@fifo_busy     

	    MVI@    R4,     R0	  ; Get next word
	    MVO     R0,     $0081       ; write it to the FIFO

	    MVI     IV.FLEN, R0	 ;\
	    DECR    R0		  ; |-- Decrement our FIFO'd data length
	    MVO     R0,     IV.FLEN     ;/
	    BEQ     @@last_fifo	 ; If zero, we're done w/ FIFO
	    MVO     R4,     IV.FPTR     ; Otherwise, save new pointer
	    B       @@fifo_fill	 ; ...and keep trying to load FIFO

@@last_fifo MVO     R0,     IV.FPTR     ; done with FIFO loading.
					; fall into ALD processing.


	    ;; ------------------------------------------------------------ ;;
	    ;;  Try to do an Address Load.  We do this in two settings:     ;;
	    ;;   -- We have no FIFO data to load.			   ;;
	    ;;   -- We've loaded as much FIFO data as we can, but we	;;
	    ;;      might have an address load command to send for it.      ;;
	    ;; ------------------------------------------------------------ ;;
@@fifo_busy:
@@no_fifodata:
	    MVI     $0080,  R0	  ; Read LRQ bit from ALD register
	    SLLC    R0,     1
	    BNC     @@ald_busy	  ; LRQ is low, meaning we can't ALD.
					; So, leave.

	    ;; ------------------------------------------------------------ ;;
	    ;;  We can do an address load (ALD) on the SP0256.  Give FIFO   ;;
	    ;;  driven ALDs priority, since we already started the FIFO     ;;
	    ;;  load.  The "need ALD" bit is stored in bit 6 of IV.QT.      ;;
	    ;; ------------------------------------------------------------ ;;
	    ANDI    #$40,   R1	  ; Is "Need FIFO ALD" bit set?
	    BEQ     @@no_fifo_ald
	    XOR     IV.QT,  R1	  ;\__ Clear the "Need FIFO ALD" bit.
	    MVO     R1,     IV.QT       ;/
	    CLRR    R1
	    MVO     R1,     $80	 ; Load a 0 into ALD (trigger FIFO rd.)
	    JR      R5		  ; done!

	    ;; ------------------------------------------------------------ ;;
	    ;;  We don't need to ALD on behalf of the FIFO.  So, we grab    ;;
	    ;;  the next thing off our phrase list.			 ;;
	    ;; ------------------------------------------------------------ ;;
@@no_fifo_ald:
	    MVI     IV.PPTR, R4	 ; Get phrase pointer.
	    TSTR    R4		  ; Is it zero?
	    BEQ     @@next_phrase       ; Yes:  Get next phrase from queue.

	    MVI@    R4,     R0
	    TSTR    R0		  ; Is it end of phrase?
	    BNEQ    @@process_phrase    ; !=0:  Go do it.

	    MVO     R0,     IV.PPTR     ; 
@@next_phrase:
	    MVI     IV.QT,  R1	  ; reload queue tail (was trashed above)
	    MOVR    R1,     R0	  ; copy QT to R0 so we can increment it
	    ANDI    #$7,    R1	  ; Mask away flags in queue head
	    CMP     IV.QH,  R1	  ; Is it same as queue tail?
	    BEQ     @@leave	     ; Yes:  No more speech for now.

	    INCR    R0
	    ANDI    #$F7,   R0	  ; mask away the possible 'carry'
	    MVO     R0,     IV.QT       ; save updated queue tail

	    ADDI    #IV.Q,  R1	  ; Index into queue
	    MVI@    R1,     R4	  ; get next value from queue
	    CMPI    #43,    R4	  ; Is it a RESROM or Phrase?
	    BNC     @@play_resrom_r4
@@new_phrase:
;	    ADDI    #IV_PHRASE_TBL - 43, R4 ; Index into phrase table
;	    MVI@    R4,     R4	  ; Read from phrase table
	    MVO     R4,     IV.PPTR
	    JR      R5		  ; we'll get to this phrase next time.

@@play_resrom_r4:
	    MVO     R4,     $0080       ; Just ALD it
	    JR      R5		  ; and leave.

	    ;; ------------------------------------------------------------ ;;
	    ;;  We're in the middle of a phrase, so continue interpreting.  ;;
	    ;; ------------------------------------------------------------ ;;
@@process_phrase:
	    
	    MVO     R4,     IV.PPTR     ; save new phrase pointer
	    CMPI    #43,    R0	  ; Is it a RESROM cue?
	    BC      @@play_fifo	 ; Just ALD it and leave.
@@play_resrom_r0
	    MVO     R0,     $0080       ; Just ALD it
	    JR      R5		  ; and leave.
@@play_fifo:
	    MVI     IV.FPTR,R1	  ; Make sure not to stomp existing FIFO
	    TSTR    R1		  ; data.
	    BEQ     @@new_fifo_ok
	    DECR    R4		  ; Oops, FIFO data still playing,
	    MVO     R4,     IV.PPTR     ; so rewind.
	    JR      R5		  ; and leave.

@@new_fifo_ok:
	    MOVR    R0,     R4	  ;
	    MVI@    R4,     R0	  ; Get chunk length
	    MVO     R0,     IV.FLEN     ; Init FIFO chunk length
	    MVO     R4,     IV.FPTR     ; Init FIFO pointer
	    MVI     IV.QT,  R0	  ;\
	    XORI    #$40,   R0	  ; |- Set "Need ALD" bit in QT
	    MVO     R0,     IV.QT       ;/

  IF 1      ; debug code		;\
	    ANDI    #$40,   R0	  ; |   Debug code:  We should only
	    BNEQ    @@qtok	      ; |-- be here if "Need FIFO ALD" 
	    HLT     ;BUG!!	      ; |   was already clear.	 
@@qtok				  ;/    
  ENDI
	    JR      R5		  ; leave.

	    ENDP


;; ======================================================================== ;;
;;  NAME								    ;;
;;      IV_PLAY     Play a voice sample sequence.			   ;;
;;									  ;;
;;  AUTHOR								  ;;
;;      Joseph Zbiciak <intvnut AT gmail.com>			       ;;
;;									  ;;
;;  REVISION HISTORY							;;
;;      15-Sep-2002 Initial revision . . . . . . . . . . .  J. Zbiciak      ;;
;;									  ;;
;;  INPUTS for IV_PLAY						      ;;
;;      R5      Invocation record, followed by return address.	      ;;
;;		  1 DECLE    Phrase number to play.		       ;;
;;									  ;;
;;  INPUTS for IV_PLAY.1						    ;;
;;      R0      Address of phrase to play.				  ;;
;;      R5      Return address					      ;;
;;									  ;;
;;  OUTPUTS								 ;;
;;      R0, R1  trashed						     ;;
;;      Z==0    if item not successfully queued.			    ;;
;;      Z==1    if successfully queued.				     ;;
;;									  ;;
;;  NOTES								   ;;
;;      This code will drop phrases if the queue is full.		   ;;
;;      Phrase numbers 1..42 are RESROM samples.  43..255 will index	;;
;;      into the user-supplied IV_PHRASE_TBL.  43 will refer to the	 ;;
;;      first entry, 44 to the second, and so on.  Phrase 0 is undefined.   ;;
;;									  ;;
;; ------------------------------------------------------------------------ ;;
;;		   Copyright (c) 2002, Joseph Zbiciak		     ;;
;; ======================================================================== ;;
IV_PLAY     PROC
	    MVI@    R5,     R0

@@1:	; alternate entry point
	    MVI     IV.QT,  R1	  ; Get queue tail
	    SWAP    R1,     2	   ;\___ Leave if "no Intellivoice"
	    BMI     @@leave	     ;/    bit it set.
@@ok:       
	    DECR    R1		  ;\
	    ANDI    #$7,    R1	  ; |-- See if we still have room
	    CMP     IV.QH,  R1	  ;/
	    BEQ     @@leave	     ; Leave if we're full

@@2:	MVI     IV.QH,  R1	  ; Get our queue head pointer
	    PSHR    R1		  ;\
	    INCR    R1		  ; |
	    ANDI    #$F7,   R1	  ; |-- Increment it, removing
	    MVO     R1,     IV.QH       ; |   carry but preserving flags.
	    PULR    R1		  ;/

	    ADDI    #IV.Q,  R1	  ;\__ Store phrase to queue
	    MVO@    R0,     R1	  ;/

@@leave:    JR      R5		  ; Leave.
	    ENDP

;; ======================================================================== ;;
;;  NAME								    ;;
;;      IV_PLAYW    Play a voice sample sequence.  Wait for queue room.     ;;
;;									  ;;
;;  AUTHOR								  ;;
;;      Joseph Zbiciak <intvnut AT gmail.com>			       ;;
;;									  ;;
;;  REVISION HISTORY							;;
;;      15-Sep-2002 Initial revision . . . . . . . . . . .  J. Zbiciak      ;;
;;									  ;;
;;  INPUTS for IV_PLAY						      ;;
;;      R5      Invocation record, followed by return address.	      ;;
;;		  1 DECLE    Phrase number to play.		       ;;
;;									  ;;
;;  INPUTS for IV_PLAY.1						    ;;
;;      R0      Address of phrase to play.				  ;;
;;      R5      Return address					      ;;
;;									  ;;
;;  OUTPUTS								 ;;
;;      R0, R1  trashed						     ;;
;;									  ;;
;;  NOTES								   ;;
;;      This code will wait for a queue slot to open if queue is full.      ;;
;;      Phrase numbers 1..42 are RESROM samples.  43..255 will index	;;
;;      into the user-supplied IV_PHRASE_TBL.  43 will refer to the	 ;;
;;      first entry, 44 to the second, and so on.  Phrase 0 is undefined.   ;;
;;									  ;;
;; ------------------------------------------------------------------------ ;;
;;		   Copyright (c) 2002, Joseph Zbiciak		     ;;
;; ======================================================================== ;;
IV_PLAYW    PROC
	    MVI@    R5,     R0

@@1:	; alternate entry point
	    MVI     IV.QT,  R1	  ; Get queue tail
	    SWAP    R1,     2	   ;\___ Leave if "no Intellivoice"
	    BMI     IV_PLAY.leave       ;/    bit it set.
@@ok:       
	    DECR    R1		  ;\
	    ANDI    #$7,    R1	  ; |-- See if we still have room
	    CMP     IV.QH,  R1	  ;/
	    BEQ     @@1		 ; wait for room
	    B       IV_PLAY.2

	    ENDP

;; ======================================================================== ;;
;;  NAME								    ;;
;;      IV_HUSH     Flush the speech queue, and hush the Intellivoice.      ;;
;;									  ;;
;;  AUTHOR								  ;;
;;      Joseph Zbiciak <intvnut AT gmail.com>			       ;;
;;									  ;;
;;  REVISION HISTORY							;;
;;      02-Feb-2018 Initial revision . . . . . . . . . . .  J. Zbiciak      ;;
;;									  ;;
;;  INPUTS for IV_HUSH						      ;;
;;      None.							       ;;
;;									  ;;
;;  OUTPUTS								 ;;
;;      R0 trashed.							 ;;
;;									  ;;
;;  NOTES								   ;;
;;      Returns via IV_WAIT.						;;
;;									  ;;
;; ======================================================================== ;;
IV_HUSH:    PROC
	    MVI     IV.QH,  R0
	    SWAP    R0,     2
	    BMI     IV_WAIT.leave

	    DIS
	    ;; We can't stop a phrase segment that's being FIFOed down.
	    ;; We need to remember if we've committed to pushing ALD.
	    ;; We _can_ stop new phrase segments from going down, and _can_
	    ;; stop new phrases from being started.

	    ;; Set head pointer to indicate we've inserted one item.
	    MVI     IV.QH,  R0  ; Re-read, as an interrupt may have occurred
	    ANDI    #$F0,   R0
	    INCR    R0
	    MVO     R0,     IV.QH

	    ;; Reset tail pointer, keeping "need ALD" bit and other flags.
	    MVI     IV.QT,  R0
	    ANDI    #$F0,   R0
	    MVO     R0,     IV.QT

	    ;; Reset the phrase pointer, to stop a long phrase.
	    CLRR    R0
	    MVO     R0,     IV.PPTR

	    ;; Queue a PA1 in the queue.  Since we're can't guarantee the user
	    ;; has included resrom.asm, let's just use the raw number (5).
	    MVII    #5,     R0
	    MVO     R0,     IV.Q

	    ;; Re-enable interrupts and wait for Intellivoice to shut up.
	    ;;
	    ;; We can't just jump to IV_WAIT.q_loop, as we need to reload
	    ;; IV.QH into R0, and I'm really committed to only using R0.
;	   JE      IV_WAIT
	    EIS
	    ; fallthrough into IV_WAIT
	    ENDP

;; ======================================================================== ;;
;;  NAME								    ;;
;;      IV_WAIT     Wait for voice queue to empty.			  ;;
;;									  ;;
;;  AUTHOR								  ;;
;;      Joseph Zbiciak <intvnut AT gmail.com>			       ;;
;;									  ;;
;;  REVISION HISTORY							;;
;;      15-Sep-2002 Initial revision . . . . . . . . . . .  J. Zbiciak      ;;
;;									  ;;
;;  INPUTS for IV_WAIT						      ;;
;;      R5      Return address					      ;;
;;									  ;;
;;  OUTPUTS								 ;;
;;      R0      trashed.						    ;;
;;									  ;;
;;  NOTES								   ;;
;;      This waits until the Intellivoice is nearly completely quiescent.   ;;
;;      Some voice data may still be spoken from the last triggered	 ;;
;;      phrase.  To truly wait for *that* to be spoken, speak a 'pause'     ;;
;;      (eg. RESROM.pa1) and then call IV_WAIT.			     ;;
;; ------------------------------------------------------------------------ ;;
;;		   Copyright (c) 2002, Joseph Zbiciak		     ;;
;; ======================================================================== ;;
IV_WAIT     PROC
	    MVI     IV.QH,  R0
	    CMPI    #$80, R0	    ; test bit 7, leave if set.
	    BC      @@leave

	    ; Wait for queue to drain.
@@q_loop:   CMP     IV.QT,  R0
	    BNEQ    @@q_loop

	    ; Wait for FIFO and LRQ to say ready.
@@s_loop:   MVI     $81,    R0	  ; Read FIFO status.  0 == ready.
	    COMR    R0
	    AND     $80,    R0	  ; Merge w/ ALD status.  1 == ready
	    TSTR    R0
	    BPL     @@s_loop	    ; if bit 15 == 0, not ready.
	    
@@leave:    JR      R5
	    ENDP

;; ======================================================================== ;;
;;  End of File:  ivoice.asm						;;
;; ======================================================================== ;;

;* ======================================================================== *;
;*  These routines are placed into the public domain by their author.  All  *;
;*  copyright rights are hereby relinquished on the routines and data in    *;
;*  this file.  -- Joseph Zbiciak, 2008				     *;
;* ======================================================================== *;

;; ======================================================================== ;;
;;  NAME								    ;;
;;      IV_SAYNUM16 Say a 16-bit unsigned number using RESROM digits	;;
;;									  ;;
;;  AUTHOR								  ;;
;;      Joseph Zbiciak <intvnut AT gmail.com>			       ;;
;;									  ;;
;;  REVISION HISTORY							;;
;;      16-Sep-2002 Initial revision . . . . . . . . . . .  J. Zbiciak      ;;
;;									  ;;
;;  INPUTS for IV_SAYNUM16						  ;;
;;      R0      Number to "speak"					   ;;
;;      R5      Return address					      ;;
;;									  ;;
;;  OUTPUTS								 ;;
;;									  ;;
;;  DESCRIPTION							     ;;
;;      "Says" a 16-bit number using IV_PLAYW to queue up the phrase.       ;;
;;      Because the number may be built from several segments, it could     ;;
;;      easily eat up the queue.  I believe the longest number will take    ;;
;;      7 queue entries -- that is, fill the queue.  Thus, this code	;;
;;      could block, waiting for slots in the queue.			;;
;; ======================================================================== ;;

IV_SAYNUM16 PROC
	    PSHR    R5

	    TSTR    R0
	    BEQ     @@zero	  ; Special case:  Just say "zero"

	    ;; ------------------------------------------------------------ ;;
	    ;;  First, try to pull off 'thousands'.  We call ourselves      ;;
	    ;;  recursively to play the the number of thousands.	    ;;
	    ;; ------------------------------------------------------------ ;;
	    CLRR    R1
@@thloop:   INCR    R1
	    SUBI    #1000,  R0
	    BC      @@thloop

	    ADDI    #1000,  R0
	    PSHR    R0
	    DECR    R1
	    BEQ     @@no_thousand

	    CALL    IV_SAYNUM16.recurse

	    CALL    IV_PLAYW
	    DECLE   36  ; THOUSAND
	    
@@no_thousand
	    PULR    R1

	    ;; ------------------------------------------------------------ ;;
	    ;;  Now try to play hundreds.				   ;;
	    ;; ------------------------------------------------------------ ;;
	    MVII    #7-1, R0    ; ZERO
	    CMPI    #100,   R1
	    BNC     @@no_hundred

@@hloop:    INCR    R0
	    SUBI    #100,   R1
	    BC      @@hloop
	    ADDI    #100,   R1

	    PSHR    R1

	    CALL    IV_PLAYW.1

	    CALL    IV_PLAYW
	    DECLE   35  ; HUNDRED

	    PULR    R1
	    B       @@notrecurse    ; skip "PSHR R5"
@@recurse:  PSHR    R5	      ; recursive entry point for 'thousand'

@@no_hundred:
@@notrecurse:
	    MOVR    R1,     R0
	    BEQ     @@leave

	    SUBI    #20,    R1
	    BNC     @@teens

	    MVII    #27-1, R0   ; TWENTY
@@tyloop    INCR    R0
	    SUBI    #10,    R1
	    BC      @@tyloop
	    ADDI    #10,    R1

	    PSHR    R1
	    CALL    IV_PLAYW.1

	    PULR    R0
	    TSTR    R0
	    BEQ     @@leave

@@teens:
@@zero:     ADDI    #7, R0  ; ZERO

	    CALL    IV_PLAYW.1

@@leave     PULR    PC
	    ENDP

;; ======================================================================== ;;
;;  End of File:  saynum16.asm					      ;;
;; ======================================================================== ;;

IV_INIT_and_wait:     EQU IV_INIT

    ELSE

IV_INIT_and_wait:     EQU _wait	; No voice init; just WAIT.

    ENDI

	IF intybasic_flash

;; ======================================================================== ;;
;;  JLP "Save Game" support						 ;;
;; ======================================================================== ;;
JF.first    EQU     $8023
JF.last     EQU     $8024
JF.addr     EQU     $8025
JF.row      EQU     $8026
		   
JF.wrcmd    EQU     $802D
JF.rdcmd    EQU     $802E
JF.ercmd    EQU     $802F
JF.wrkey    EQU     $C0DE
JF.rdkey    EQU     $DEC0
JF.erkey    EQU     $BEEF

JF.write:   DECLE   JF.wrcmd,   JF.wrkey    ; Copy JLP RAM to flash row  
JF.read:    DECLE   JF.rdcmd,   JF.rdkey    ; Copy flash row to JLP RAM  
JF.erase:   DECLE   JF.ercmd,   JF.erkey    ; Erase flash sector 

;; ======================================================================== ;;
;;  JF.INIT	 Copy JLP save-game support routine to System RAM	;;
;; ======================================================================== ;;
JF.INIT     PROC
	    PSHR    R5	    
	    MVII    #@@__code,  R5
	    MVII    #JF.SYSRAM, R4
	    REPEAT  5       
	    MVI@    R5,	 R0      ; \_ Copy code fragment to System RAM
	    MVO@    R0,	 R4      ; /
	    ENDR
	    PULR    PC

	    ;; === start of code that will run from RAM
@@__code:   MVO@    R0,	 R1      ; JF.SYSRAM + 0: initiate command
	    ADD@    R1,	 PC      ; JF.SYSRAM + 1: Wait for JLP to return
	    JR      R5		  ; JF.SYSRAM + 2:
	    MVO@    R2,	 R2      ; JF.SYSRAM + 3: \__ simple ISR
	    JR      R5		  ; JF.SYSRAM + 4: /
	    ;; === end of code that will run from RAM
	    ENDP

;; ======================================================================== ;;
;;  JF.CMD	  Issue a JLP Flash command			       ;;
;;									  ;;
;;  INPUT								   ;;
;;      R0  Slot number to operate on				       ;;
;;      R1  Address to copy to/from in JLP RAM			      ;;
;;      @R5 Command to invoke:					      ;;
;;									  ;;
;;	      JF.write -- Copy JLP RAM to Flash			   ;;
;;	      JF.read  -- Copy Flash to JLP RAM			   ;;
;;	      JF.erase -- Erase flash sector			      ;;
;;									  ;;
;;  OUTPUT								  ;;
;;      R0 - R4 not modified.  (Saved and restored across call)	     ;;
;;      JLP command executed						;;
;;									  ;;
;;  NOTES								   ;;
;;      This code requires two short routines in the console's System RAM.  ;;
;;      It also requires that the system stack reside in System RAM.	;;
;;      Because an interrupt may occur during the code's execution, there   ;;
;;      must be sufficient stack space to service the interrupt (8 words).  ;;
;;									  ;;
;;      The code also relies on the fact that the EXEC ISR dispatch does    ;;
;;      not modify R2.  This allows us to initialize R2 for the ISR ahead   ;;
;;      of time, rather than in the ISR.				    ;;
;; ======================================================================== ;;
JF.CMD      PROC

	    MVO     R4,	 JF.SV.R4    ; \
	    MVII    #JF.SV.R0,  R4	  ;  |
	    MVO@    R0,	 R4	  ;  |- Save registers, but not on
	    MVO@    R1,	 R4	  ;  |  the stack.  (limit stack use)
	    MVO@    R2,	 R4	  ; /

	    MVI@    R5,	 R4	  ; Get command to invoke

	    MVO     R5,	 JF.SV.R5    ; save return address

	    DIS
	    MVO     R1,	 JF.addr     ; \_ Save SG arguments in JLP
	    MVO     R0,	 JF.row      ; /
					  
	    MVI@    R4,	 R1	  ; Get command address
	    MVI@    R4,	 R0	  ; Get unlock word
					  
	    MVII    #$100,      R4	  ; \
	    SDBD			    ;  |_ Save old ISR in save area
	    MVI@    R4,	 R2	  ;  |
	    MVO     R2,	 JF.SV.ISR   ; /
					  
	    MVII    #JF.SYSRAM + 3, R2      ; \
	    MVO     R2,	 $100	;  |_ Set up new ISR in RAM
	    SWAP    R2		      ;  |
	    MVO     R2,	 $101	; / 
					  
	    MVII    #$20,       R2	  ; Address of STIC handshake
	    JSRE    R5,  JF.SYSRAM	  ; Invoke the command
					  
	    MVI     JF.SV.ISR,  R2	  ; \
	    MVO     R2,	 $100	;  |_ Restore old ISR 
	    SWAP    R2		      ;  |
	    MVO     R2,	 $101	; /
					  
	    MVII    #JF.SV.R0,  R5	  ; \
	    MVI@    R5,	 R0	  ;  |
	    MVI@    R5,	 R1	  ;  |- Restore registers
	    MVI@    R5,	 R2	  ;  |
	    MVI@    R5,	 R4	  ; /
	    MVI@    R5,	 PC	  ; Return

	    ENDP


	ENDI

	IF intybasic_fastmult

; Quarter Square Multiplication
; Assembly code by Joe Zbiciak, 2015
; Released to public domain.

QSQR8_TBL:  PROC
	    DECLE   $3F80, $3F01, $3E82, $3E04, $3D86, $3D09, $3C8C, $3C10
	    DECLE   $3B94, $3B19, $3A9E, $3A24, $39AA, $3931, $38B8, $3840
	    DECLE   $37C8, $3751, $36DA, $3664, $35EE, $3579, $3504, $3490
	    DECLE   $341C, $33A9, $3336, $32C4, $3252, $31E1, $3170, $3100
	    DECLE   $3090, $3021, $2FB2, $2F44, $2ED6, $2E69, $2DFC, $2D90
	    DECLE   $2D24, $2CB9, $2C4E, $2BE4, $2B7A, $2B11, $2AA8, $2A40
	    DECLE   $29D8, $2971, $290A, $28A4, $283E, $27D9, $2774, $2710
	    DECLE   $26AC, $2649, $25E6, $2584, $2522, $24C1, $2460, $2400
	    DECLE   $23A0, $2341, $22E2, $2284, $2226, $21C9, $216C, $2110
	    DECLE   $20B4, $2059, $1FFE, $1FA4, $1F4A, $1EF1, $1E98, $1E40
	    DECLE   $1DE8, $1D91, $1D3A, $1CE4, $1C8E, $1C39, $1BE4, $1B90
	    DECLE   $1B3C, $1AE9, $1A96, $1A44, $19F2, $19A1, $1950, $1900
	    DECLE   $18B0, $1861, $1812, $17C4, $1776, $1729, $16DC, $1690
	    DECLE   $1644, $15F9, $15AE, $1564, $151A, $14D1, $1488, $1440
	    DECLE   $13F8, $13B1, $136A, $1324, $12DE, $1299, $1254, $1210
	    DECLE   $11CC, $1189, $1146, $1104, $10C2, $1081, $1040, $1000
	    DECLE   $0FC0, $0F81, $0F42, $0F04, $0EC6, $0E89, $0E4C, $0E10
	    DECLE   $0DD4, $0D99, $0D5E, $0D24, $0CEA, $0CB1, $0C78, $0C40
	    DECLE   $0C08, $0BD1, $0B9A, $0B64, $0B2E, $0AF9, $0AC4, $0A90
	    DECLE   $0A5C, $0A29, $09F6, $09C4, $0992, $0961, $0930, $0900
	    DECLE   $08D0, $08A1, $0872, $0844, $0816, $07E9, $07BC, $0790
	    DECLE   $0764, $0739, $070E, $06E4, $06BA, $0691, $0668, $0640
	    DECLE   $0618, $05F1, $05CA, $05A4, $057E, $0559, $0534, $0510
	    DECLE   $04EC, $04C9, $04A6, $0484, $0462, $0441, $0420, $0400
	    DECLE   $03E0, $03C1, $03A2, $0384, $0366, $0349, $032C, $0310
	    DECLE   $02F4, $02D9, $02BE, $02A4, $028A, $0271, $0258, $0240
	    DECLE   $0228, $0211, $01FA, $01E4, $01CE, $01B9, $01A4, $0190
	    DECLE   $017C, $0169, $0156, $0144, $0132, $0121, $0110, $0100
	    DECLE   $00F0, $00E1, $00D2, $00C4, $00B6, $00A9, $009C, $0090
	    DECLE   $0084, $0079, $006E, $0064, $005A, $0051, $0048, $0040
	    DECLE   $0038, $0031, $002A, $0024, $001E, $0019, $0014, $0010
	    DECLE   $000C, $0009, $0006, $0004, $0002, $0001, $0000
@@mid:
	    DECLE   $0000, $0000, $0001, $0002, $0004, $0006, $0009, $000C
	    DECLE   $0010, $0014, $0019, $001E, $0024, $002A, $0031, $0038
	    DECLE   $0040, $0048, $0051, $005A, $0064, $006E, $0079, $0084
	    DECLE   $0090, $009C, $00A9, $00B6, $00C4, $00D2, $00E1, $00F0
	    DECLE   $0100, $0110, $0121, $0132, $0144, $0156, $0169, $017C
	    DECLE   $0190, $01A4, $01B9, $01CE, $01E4, $01FA, $0211, $0228
	    DECLE   $0240, $0258, $0271, $028A, $02A4, $02BE, $02D9, $02F4
	    DECLE   $0310, $032C, $0349, $0366, $0384, $03A2, $03C1, $03E0
	    DECLE   $0400, $0420, $0441, $0462, $0484, $04A6, $04C9, $04EC
	    DECLE   $0510, $0534, $0559, $057E, $05A4, $05CA, $05F1, $0618
	    DECLE   $0640, $0668, $0691, $06BA, $06E4, $070E, $0739, $0764
	    DECLE   $0790, $07BC, $07E9, $0816, $0844, $0872, $08A1, $08D0
	    DECLE   $0900, $0930, $0961, $0992, $09C4, $09F6, $0A29, $0A5C
	    DECLE   $0A90, $0AC4, $0AF9, $0B2E, $0B64, $0B9A, $0BD1, $0C08
	    DECLE   $0C40, $0C78, $0CB1, $0CEA, $0D24, $0D5E, $0D99, $0DD4
	    DECLE   $0E10, $0E4C, $0E89, $0EC6, $0F04, $0F42, $0F81, $0FC0
	    DECLE   $1000, $1040, $1081, $10C2, $1104, $1146, $1189, $11CC
	    DECLE   $1210, $1254, $1299, $12DE, $1324, $136A, $13B1, $13F8
	    DECLE   $1440, $1488, $14D1, $151A, $1564, $15AE, $15F9, $1644
	    DECLE   $1690, $16DC, $1729, $1776, $17C4, $1812, $1861, $18B0
	    DECLE   $1900, $1950, $19A1, $19F2, $1A44, $1A96, $1AE9, $1B3C
	    DECLE   $1B90, $1BE4, $1C39, $1C8E, $1CE4, $1D3A, $1D91, $1DE8
	    DECLE   $1E40, $1E98, $1EF1, $1F4A, $1FA4, $1FFE, $2059, $20B4
	    DECLE   $2110, $216C, $21C9, $2226, $2284, $22E2, $2341, $23A0
	    DECLE   $2400, $2460, $24C1, $2522, $2584, $25E6, $2649, $26AC
	    DECLE   $2710, $2774, $27D9, $283E, $28A4, $290A, $2971, $29D8
	    DECLE   $2A40, $2AA8, $2B11, $2B7A, $2BE4, $2C4E, $2CB9, $2D24
	    DECLE   $2D90, $2DFC, $2E69, $2ED6, $2F44, $2FB2, $3021, $3090
	    DECLE   $3100, $3170, $31E1, $3252, $32C4, $3336, $33A9, $341C
	    DECLE   $3490, $3504, $3579, $35EE, $3664, $36DA, $3751, $37C8
	    DECLE   $3840, $38B8, $3931, $39AA, $3A24, $3A9E, $3B19, $3B94
	    DECLE   $3C10, $3C8C, $3D09, $3D86, $3E04, $3E82, $3F01, $3F80
	    DECLE   $4000, $4080, $4101, $4182, $4204, $4286, $4309, $438C
	    DECLE   $4410, $4494, $4519, $459E, $4624, $46AA, $4731, $47B8
	    DECLE   $4840, $48C8, $4951, $49DA, $4A64, $4AEE, $4B79, $4C04
	    DECLE   $4C90, $4D1C, $4DA9, $4E36, $4EC4, $4F52, $4FE1, $5070
	    DECLE   $5100, $5190, $5221, $52B2, $5344, $53D6, $5469, $54FC
	    DECLE   $5590, $5624, $56B9, $574E, $57E4, $587A, $5911, $59A8
	    DECLE   $5A40, $5AD8, $5B71, $5C0A, $5CA4, $5D3E, $5DD9, $5E74
	    DECLE   $5F10, $5FAC, $6049, $60E6, $6184, $6222, $62C1, $6360
	    DECLE   $6400, $64A0, $6541, $65E2, $6684, $6726, $67C9, $686C
	    DECLE   $6910, $69B4, $6A59, $6AFE, $6BA4, $6C4A, $6CF1, $6D98
	    DECLE   $6E40, $6EE8, $6F91, $703A, $70E4, $718E, $7239, $72E4
	    DECLE   $7390, $743C, $74E9, $7596, $7644, $76F2, $77A1, $7850
	    DECLE   $7900, $79B0, $7A61, $7B12, $7BC4, $7C76, $7D29, $7DDC
	    DECLE   $7E90, $7F44, $7FF9, $80AE, $8164, $821A, $82D1, $8388
	    DECLE   $8440, $84F8, $85B1, $866A, $8724, $87DE, $8899, $8954
	    DECLE   $8A10, $8ACC, $8B89, $8C46, $8D04, $8DC2, $8E81, $8F40
	    DECLE   $9000, $90C0, $9181, $9242, $9304, $93C6, $9489, $954C
	    DECLE   $9610, $96D4, $9799, $985E, $9924, $99EA, $9AB1, $9B78
	    DECLE   $9C40, $9D08, $9DD1, $9E9A, $9F64, $A02E, $A0F9, $A1C4
	    DECLE   $A290, $A35C, $A429, $A4F6, $A5C4, $A692, $A761, $A830
	    DECLE   $A900, $A9D0, $AAA1, $AB72, $AC44, $AD16, $ADE9, $AEBC
	    DECLE   $AF90, $B064, $B139, $B20E, $B2E4, $B3BA, $B491, $B568
	    DECLE   $B640, $B718, $B7F1, $B8CA, $B9A4, $BA7E, $BB59, $BC34
	    DECLE   $BD10, $BDEC, $BEC9, $BFA6, $C084, $C162, $C241, $C320
	    DECLE   $C400, $C4E0, $C5C1, $C6A2, $C784, $C866, $C949, $CA2C
	    DECLE   $CB10, $CBF4, $CCD9, $CDBE, $CEA4, $CF8A, $D071, $D158
	    DECLE   $D240, $D328, $D411, $D4FA, $D5E4, $D6CE, $D7B9, $D8A4
	    DECLE   $D990, $DA7C, $DB69, $DC56, $DD44, $DE32, $DF21, $E010
	    DECLE   $E100, $E1F0, $E2E1, $E3D2, $E4C4, $E5B6, $E6A9, $E79C
	    DECLE   $E890, $E984, $EA79, $EB6E, $EC64, $ED5A, $EE51, $EF48
	    DECLE   $F040, $F138, $F231, $F32A, $F424, $F51E, $F619, $F714
	    DECLE   $F810, $F90C, $FA09, $FB06, $FC04, $FD02, $FE01
	    ENDP

; R0 = R0 * R1, where R0 and R1 are unsigned 8-bit values
; Destroys R1, R4
qs_mpy8:    PROC
	    MOVR    R0,	     R4      ;   6
	    ADDI    #QSQR8_TBL.mid, R1      ;   8
	    ADDR    R1,	     R4      ;   6   a + b
	    SUBR    R0,	     R1      ;   6   a - b
@@ok:       MVI@    R4,	     R0      ;   8
	    SUB@    R1,	     R0      ;   8
	    JR      R5		      ;   7
					    ;----
					    ;  49
	    ENDP
	    

; R1 = R0 * R1, where R0 and R1 are 16-bit values
; destroys R0, R2, R3, R4, R5
qs_mpy16:   PROC
	    PSHR    R5		  ;   9
				   
	    ; Unpack lo/hi
	    MOVR    R0,	 R2      ;   6   
	    ANDI    #$FF,       R0      ;   8   R0 is lo(a)
	    XORR    R0,	 R2      ;   6   
	    SWAP    R2		  ;   6   R2 is hi(a)

	    MOVR    R1,	 R3      ;   6   R3 is orig 16-bit b
	    ANDI    #$FF,       R1      ;   8   R1 is lo(b)
	    MOVR    R1,	 R5      ;   6   R5 is lo(b)
	    XORR    R1,	 R3      ;   6   
	    SWAP    R3		  ;   6   R3 is hi(b)
					;----
					;  67
					
	    ; lo * lo		   
	    MOVR    R0,	 R4      ;   6   R4 is lo(a)
	    ADDI    #QSQR8_TBL.mid, R1  ;   8
	    ADDR    R1,	 R4      ;   6   R4 = lo(a) + lo(b)
	    SUBR    R0,	 R1      ;   6   R1 = lo(a) - lo(b)
					
@@pos_ll:   MVI@    R4,	 R4      ;   8   R4 = qstbl[lo(a)+lo(b)]
	    SUB@    R1,	 R4      ;   8   R4 = lo(a)*lo(b)
					;----
					;  42
					;  67 (carried forward)
					;----
					; 109
				       
	    ; lo * hi		  
	    MOVR    R0,	 R1      ;   6   R0 = R1 = lo(a)
	    ADDI    #QSQR8_TBL.mid, R3  ;   8
	    ADDR    R3,	 R1      ;   6   R1 = hi(b) + lo(a)
	    SUBR    R0,	 R3      ;   6   R3 = hi(b) - lo(a)
				       
@@pos_lh:   MVI@    R1,	 R1      ;   8   R1 = qstbl[hi(b)-lo(a)]
	    SUB@    R3,	 R1      ;   8   R1 = lo(a)*hi(b)
					;----
					;  42
					; 109 (carried forward)
					;----
					; 151
				       
	    ; hi * lo		  
	    MOVR    R5,	 R0      ;   6   R5 = R0 = lo(b)
	    ADDI    #QSQR8_TBL.mid, R2  ;   8
	    ADDR    R2,	 R5      ;   6   R3 = hi(a) + lo(b)
	    SUBR    R0,	 R2      ;   6   R2 = hi(a) - lo(b)
				       
@@pos_hl:   ADD@    R5,	 R1      ;   8   \_ R1 = lo(a)*hi(b)+hi(a)*lo(b)
	    SUB@    R2,	 R1      ;   8   /
					;----
					;  42
					; 151 (carried forward)
					;----
					; 193
				       
	    SWAP    R1		  ;   6   \_ shift upper product left 8
	    ANDI    #$FF00,     R1      ;   8   /
	    ADDR    R4,	 R1      ;   6   final product
	    PULR    PC		  ;  12
					;----
					;  32
					; 193 (carried forward)
					;----
					; 225
	    ENDP

	ENDI

	IF intybasic_fastdiv

; Fast unsigned division/remainder
; Assembly code by Oscar Toledo G. Jul/10/2015
; Released to public domain.

	; Ultrafast unsigned division/remainder operation
	; Entry: R0 = Dividend
	;	R1 = Divisor
	; Output: R0 = Quotient
	;	 R2 = Remainder
	; Worst case: 6 + 6 + 9 + 496 = 517 cycles
	; Best case: 6 + (6 + 7) * 16 = 214 cycles

uf_udiv16:	PROC
	CLRR R2		; 6
	SLLC R0,1	; 6
	BC @@1		; 7/9
	SLLC R0,1	; 6
	BC @@2		; 7/9
	SLLC R0,1	; 6
	BC @@3		; 7/9
	SLLC R0,1	; 6
	BC @@4		; 7/9
	SLLC R0,1	; 6
	BC @@5		; 7/9
	SLLC R0,1	; 6
	BC @@6		; 7/9
	SLLC R0,1	; 6
	BC @@7		; 7/9
	SLLC R0,1	; 6
	BC @@8		; 7/9
	SLLC R0,1	; 6
	BC @@9		; 7/9
	SLLC R0,1	; 6
	BC @@10		; 7/9
	SLLC R0,1	; 6
	BC @@11		; 7/9
	SLLC R0,1	; 6
	BC @@12		; 7/9
	SLLC R0,1	; 6
	BC @@13		; 7/9
	SLLC R0,1	; 6
	BC @@14		; 7/9
	SLLC R0,1	; 6
	BC @@15		; 7/9
	SLLC R0,1	; 6
	BC @@16		; 7/9
	JR R5

@@1:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@2:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@3:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@4:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@5:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@6:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@7:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@8:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@9:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@10:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@11:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@12:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@13:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@14:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@15:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
@@16:	RLC R2,1	; 6
	CMPR R1,R2	; 6
	BNC $+3		; 7/9
	SUBR R1,R2	; 6
	RLC R0,1	; 6
	JR R5
	
	ENDP

	ENDI

	ROM.End

	ROM.OutputRomStats

	ORG $200,$200,"-RWB"

Q2:	; Reserved label for #BACKTAB

	;
	; 16-bits variables
	; Note IntyBASIC variables grow up starting in $308.
	;

BASE_16BIT_SYSTEM_VARS: QSET $33f
    IF intybasic_voice
BASE_16BIT_SYSTEM_VARS: QSET BASE_16BIT_SYSTEM_VARS-10
    ENDI
    IF intybasic_col
BASE_16BIT_SYSTEM_VARS: QSET BASE_16BIT_SYSTEM_VARS-8
    ENDI
    IF intybasic_scroll
BASE_16BIT_SYSTEM_VARS: QSET BASE_16BIT_SYSTEM_VARS-20
    ENDI

	ORG BASE_16BIT_SYSTEM_VARS,BASE_16BIT_SYSTEM_VARS,"-RWB"

    IF intybasic_voice
IV.Q:      RMB 8    ; IV_xxx	16-bit	  Voice queue  (8 words)
IV.FPTR:   RMB 1    ; IV_xxx	16-bit	  Current FIFO ptr.
IV.PPTR:   RMB 1    ; IV_xxx	16-bit	  Current Phrase ptr.
    ENDI
    IF intybasic_col
_col0:      RMB 1       ; Collision status for MOB0
_col1:      RMB 1       ; Collision status for MOB1
_col2:      RMB 1       ; Collision status for MOB2
_col3:      RMB 1       ; Collision status for MOB3
_col4:      RMB 1       ; Collision status for MOB4
_col5:      RMB 1       ; Collision status for MOB5
_col6:      RMB 1       ; Collision status for MOB6
_col7:      RMB 1       ; Collision status for MOB7
    ENDI
    IF intybasic_scroll
_scroll_buffer: RMB 20  ; Sometimes this is unused
    ENDI
_music_gosub:	RMB 1	; GOSUB pointer
_music_table:	RMB 1	; Note table
_music_p:	RMB 1	; Pointer to music
_frame:		RMB 1   ; Current frame
_read:		RMB 1   ; Pointer to DATA
_gram_bitmap:   RMB 1   ; Bitmap for definition
_gram2_bitmap:  RMB 1   ; Secondary bitmap for definition
_screen:	RMB 1	; Pointer to current screen position
_color:		RMB 1	; Current color

Q1:			; Reserved label for #MOBSHADOW
_mobs:      RMB 3*8     ; MOB buffer

SCRATCH:    ORG $100,$100,"-RWBN"
	;
	; 8-bits variables
	;
ISRVEC:     RMB 2       ; Pointer to ISR vector (required by Intellivision ROM)
_int:       RMB 1       ; Signals interrupt received
_ntsc:      RMB 1       ; bit 0 = 1=NTSC, 0=PAL. Bit 1 = 1=ECS detected.
_rand:      RMB 1       ; Pseudo-random value
_gram_target:   RMB 1   ; Contains GRAM card number
_gram_total:    RMB 1   ; Contains total GRAM cards for definition
_gram2_target:  RMB 1   ; Contains GRAM card number
_gram2_total:   RMB 1   ; Contains total GRAM cards for definition
_mode_select:   RMB 1   ; Graphics mode selection
_border_color:  RMB 1   ; Border color
_border_mask:   RMB 1   ; Border mask
    IF intybasic_keypad
_cnt1_p0:   RMB 1       ; Debouncing 1
_cnt1_p1:   RMB 1       ; Debouncing 2
_cnt1_key:  RMB 1       ; Currently pressed key
_cnt2_p0:   RMB 1       ; Debouncing 1
_cnt2_p1:   RMB 1       ; Debouncing 2
_cnt2_key:  RMB 1       ; Currently pressed key
    ENDI
    IF intybasic_scroll
_scroll_x:  RMB 1       ; Scroll X offset
_scroll_y:  RMB 1       ; Scroll Y offset
_scroll_d:  RMB 1       ; Scroll direction
    ENDI
    IF intybasic_music
_music_start:	RMB 2	; Start of music

_music_mode: RMB 1      ; Music mode (0= Not using PSG, 2= Simple, 4= Full, add 1 if using noise channel for drums)
_music_frame: RMB 1     ; Music frame (for 50 hz fixed)
_music_tc:  RMB 1       ; Time counter
_music_t:   RMB 1       ; Time base
_music_i1:  RMB 1       ; Instrument 1 
_music_s1:  RMB 1       ; Sample pointer 1
_music_n1:  RMB 1       ; Note 1
_music_i2:  RMB 1       ; Instrument 2
_music_s2:  RMB 1       ; Sample pointer 2
_music_n2:  RMB 1       ; Note 2
_music_i3:  RMB 1       ; Instrument 3
_music_s3:  RMB 1       ; Sample pointer 3
_music_n3:  RMB 1       ; Note 3
_music_s4:  RMB 1       ; Sample pointer 4
_music_n4:  RMB 1       ; Note 4 (really it's drum)

_music_freq10:	RMB 1   ; Low byte frequency A
_music_freq20:	RMB 1   ; Low byte frequency B
_music_freq30:	RMB 1   ; Low byte frequency C
_music_freq11:	RMB 1   ; High byte frequency A
_music_freq21:	RMB 1   ; High byte frequency B
_music_freq31:	RMB 1   ; High byte frequency C
_music_mix:	RMB 1   ; Mixer
_music_noise:	RMB 1   ; Noise
_music_vol1:	RMB 1   ; Volume A
_music_vol2:	RMB 1   ; Volume B
_music_vol3:	RMB 1   ; Volume C
    ENDI
    IF intybasic_music_ecs
_music_i5:  RMB 1       ; Instrument 5
_music_s5:  RMB 1       ; Sample pointer 5
_music_n5:  RMB 1       ; Note 5
_music_i6:  RMB 1       ; Instrument 6
_music_s6:  RMB 1       ; Sample pointer 6
_music_n6:  RMB 1       ; Note 6
_music_i7:  RMB 1       ; Instrument 7
_music_s7:  RMB 1       ; Sample pointer 7
_music_n7:  RMB 1       ; Note 7
_music_s8:  RMB 1       ; Sample pointer 8
_music_n8:  RMB 1       ; Note 8 (really it's drum)

_music2_freq10:	RMB 1   ; Low byte frequency A
_music2_freq20:	RMB 1   ; Low byte frequency B
_music2_freq30:	RMB 1   ; Low byte frequency C
_music2_freq11:	RMB 1   ; High byte frequency A
_music2_freq21:	RMB 1   ; High byte frequency B
_music2_freq31:	RMB 1   ; High byte frequency C
_music2_mix:	RMB 1   ; Mixer
_music2_noise:	RMB 1   ; Noise
_music2_vol1:	RMB 1   ; Volume A
_music2_vol2:	RMB 1   ; Volume B
_music2_vol3:	RMB 1   ; Volume C
    ENDI
    IF intybasic_music_volume
_music_vol:	RMB 1	; Global music volume
    ENDI
    IF intybasic_voice
IV.QH:     RMB 1    ; IV_xxx	8-bit	   Voice queue head
IV.QT:     RMB 1    ; IV_xxx	8-bit	   Voice queue tail
IV.FLEN:   RMB 1    ; IV_xxx	8-bit	   Length of FIFO data
    ENDI

var_BLOCKS:	RMB 1	; BLOCKS
var_BX:	RMB 1	; BX
var_BY:	RMB 1	; BY
var_C:	RMB 1	; C
var_CUCU_WAVE:	RMB 1	; CUCU_WAVE
var_D:	RMB 1	; D
var_DROP_BULLET:	RMB 1	; DROP_BULLET
var_FIRST_TIME_EVER:	RMB 1	; FIRST_TIME_EVER
var_GRONK:	RMB 1	; GRONK
var_LEVEL:	RMB 1	; LEVEL
var_LIVES:	RMB 1	; LIVES
var_NEXT_BULLET:	RMB 1	; NEXT_BULLET
var_NEXT_WAVE:	RMB 1	; NEXT_WAVE
var_PX:	RMB 1	; PX
var_PY:	RMB 1	; PY
var_SOUND_EFFECT:	RMB 1	; SOUND_EFFECT
var_SOUND_STATE:	RMB 1	; SOUND_STATE
var_SUBLEVEL:	RMB 1	; SUBLEVEL
var_VALID:	RMB 1	; VALID
var_WAVE:	RMB 1	; WAVE
array_B:	RMB 6	; B
array_S:	RMB 6	; S
array_X:	RMB 6	; X
array_Y:	RMB 6	; Y
array_Z:	RMB 6	; Z
_SCRATCH:	EQU $

SYSTEM:	ORG $2F0, $2F0, "-RWBN"
STACK:	RMB 24
_SYSTEM:	EQU $

SYSTEM2:	ORG $8040, $8040, "-RWBN"
var_&RECORD:	RMB 1	; #RECORD
var_&SCORE:	RMB 1	; #SCORE
_SYSTEM2:	EQU $
