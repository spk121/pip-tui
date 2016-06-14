#include <libguile.h>
#include <math.h>
#include "colors.h"

/* For some reason, these color transforms are
   unusably slow in Guile.  So here is a C version. */


static inline double transform1 (double X)
{
  if (X > 0.04045)
    return (100.0 * pow((X + 0.055) / 1.055, 2.4));
  return 100.0 * (X / 12.92);
}

static inline double transform2 (double a)
{
  if (a > 0.008856)
    return cbrt(a);

  return a * 7.787 + 0.00886;
}

#define REFY 100.0
#define REFZ 108.883
#define REFX 95.047

SCM pip_color_distance (SCM cR, SCM cG, SCM cB, SCM cR2, SCM cG2, SCM cB2)
{
  double R = scm_to_double (cR);
  double G = scm_to_double (cG);
  double B = scm_to_double (cB);
  double R2 = scm_to_double (cR2);
  double G2 = scm_to_double (cG2);
  double B2 = scm_to_double (cB2);

  R /= 255.0;
  G /= 255.0;
  B /= 255.0;
  R2 /= 255.0;
  G2 /= 255.0;
  B2 /= 255.0;

  R = transform1 (R);
  G = transform1 (G);
  B = transform1 (B);
  R2 = transform1 (R2);
  G2 = transform1 (G2);
  B2 = transform1 (B2);

  double X = R * 0.4124 + G * 0.3576 + B * 0.1805;
  double Y = R * 0.2126 + G * 0.7152 + B * 0.0722;
  double Z = R * 0.0193 + G * 0.1192 + B * 0.9505;
  double X2 = R2 * 0.4124 + G2 * 0.3576 + B2 * 0.1805;
  double Y2 = R2 * 0.2126 + G2 * 0.7152 + B2 * 0.0722;
  double Z2 = R2 * 0.0193 + G2 * 0.1192 + B2 * 0.9505;

  Y /= REFY;
  Z /= REFZ;
  X /= REFX;
  Y2 /= REFY;
  Z2 /= REFZ;
  X2 /= REFX;

  double LL = 116.0 * transform2(Y) - 16.0;
  double AA = 500.0 * (transform2(X) - transform2 (Y));
  double BB = 200.0 * (transform2(Y) - transform2 (Z));
  double LL2 = 116.0 * transform2(Y2) - 16.0;
  double AA2 = 500.0 * (transform2(X2) - transform2 (Y2));
  double BB2 = 200.0 * (transform2(Y2) - transform2 (Z2));
  double dL = LL - LL2;
  double dA = AA - AA2;
  double dB = BB - BB2;
  
  return scm_from_double (sqrt (dL * dL + dA * dA + dB * dB));
}

void
pip_colors_init ()
{
  scm_c_define_gsubr ("%color-distance", 6, 0, 0, pip_color_distance);
}
