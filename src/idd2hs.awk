#!/usr/bin/awk -f
#===============================================================================
#
#          File:  idd2hs.awk
#
#   Description:
#
#   VIM Version:  7.0+
#        Author:  Mitchell Paulus,
#  Organization:
#       Version:  1.0
#       Created:  05/24/2020 12:24:22 PM
#      Revision:  ---
#       License:  Copyright (c) 2020, Mitchell Paulus
#===============================================================================

BEGIN {
    unique     = -1
    min_fields = -1
    default    = ""
    memo       = ""
}

# Begin object
/^[A-Za-z0-9:]+,/ {

    printf ""

}






