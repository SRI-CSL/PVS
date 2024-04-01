PVS patches
===========

This directory is for PVS patches.  These are normally lisp files of the
form `patch-YYYYMMDD.lisp`, where `YYYY` is the year, etc.  PVS loads the
earliest patches first, but skips those that are earlier than the latest
build date, which can be found in the `PVS Welcome` buffer.  The idea is
that such patches should already have been incorporated into the later
builds.

More can be added after the date, if desired, e.g.,
`patch-20180606-dday.lisp`.

If the modification is not really a patch, but a preference setting, it
should go into `~/.pvs.lisp` or `~/.pvsemacs`, rather than creating a
patch file.
