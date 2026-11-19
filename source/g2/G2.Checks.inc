{ G2's code indexes variable-length records past their declared bounds (array[0..0] of TValue), relies on
  wrapping integer arithmetic and has its own native stack guard: the range, overflow, assertion and stack
  checks of a development build stay off for it, so it compiles and runs the same inside the apps' builds }
{$R-}{$Q-}{$C-}{$S-}
