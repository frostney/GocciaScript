#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /tmp/regexprtest.iU2h3Q
OFS=$IFS
IFS="
"
/usr/bin/ld          -order_file symbol_order.fpc -multiply_defined suppress -L. -o /tmp/regexprtest.iU2h3Q `cat link9932.res` -filelist linkfiles9932.res
if [ $? != 0 ]; then DoExitLink /tmp/regexprtest.iU2h3Q; fi
IFS=$OFS
