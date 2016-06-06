#include <libguile.h>
#include <fribidi.h>

#include "bidi.h"

SCM bidi_LTR;
SCM bidi_RTL;
SCM bidi_ON;
SCM bidi_WLTR;
SCM bidi_WRTL;

SCM
bidi_string_logical_to_visual (SCM str, SCM alignment)
{
  SCM_ASSERT (scm_is_string (str), str, SCM_ARG1, "%string-logical->visual");
  FriBidiParType c_alignment = (FriBidiParType) scm_to_int (alignment);
  size_t c_len_in;
  uint32_t *c_u32str_in = (uint32_t *) scm_to_utf32_stringn (str, &c_len_in);
  uint32_t *c_u32str_out = (uint32_t *) malloc ((c_len_in + 1) * sizeof (uint32_t));
  fribidi_log2vis(c_u32str_in, c_len_in, &c_alignment, c_u32str_out, 0, 0, 0);

  return scm_from_utf32_stringn (c_u32str_out, c_len_in);
  
}

SCM
bidi_get_par_direction (SCM str)
{
  FriBidiCharType *types;
  size_t str_len;
  FriBidiStrIndex len;
  FriBidiChar *c_str;
  
  c_str = (FriBidiChar *) scm_to_utf32_stringn (str, &str_len);
  len = str_len;
  types = (FriBidiCharType *) malloc (len * sizeof (FriBidiCharType));

  fribidi_get_bidi_types (c_str, len, types);
  
  FriBidiParType ptype = fribidi_get_par_direction (types, len);
  return scm_from_int (ptype);
}

void
bidi_init ()
{
  scm_c_define_gsubr ("%string-logical->visual", 2, 0, 0, bidi_string_logical_to_visual);
  scm_c_define_gsubr ("%string-par-direction", 1, 0, 0, bidi_get_par_direction);
  bidi_LTR = scm_permanent_object (scm_c_define ("LTR", scm_from_int (FRIBIDI_PAR_LTR)));
  bidi_RTL = scm_permanent_object (scm_c_define ("RTL", scm_from_int (FRIBIDI_PAR_RTL)));
  bidi_WLTR = scm_permanent_object (scm_c_define ("WLTR", scm_from_int (FRIBIDI_PAR_WLTR)));
  bidi_WRTL = scm_permanent_object (scm_c_define ("WRTL", scm_from_int (FRIBIDI_PAR_WRTL)));
  bidi_ON = scm_permanent_object (scm_c_define ("ON", scm_from_int (FRIBIDI_PAR_ON)));
}
