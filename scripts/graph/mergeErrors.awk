#!/usr/bin/awk -f

BEGIN {
    FS=","
}

FNR >= 2 {
    error_kind=$1
    operation_kind=$2

    # Remove the quotes from the last field, it never contains a string
    gsub("\"", "", $3)
    n=$3

    # We concatenate the kind of error and operation together so we can
    # add the errors across files
    kind=sprintf("%s,%s", error_kind, operation_kind)

    if (kind in matrix) {
        matrix[kind] += n
    } else {
        matrix[kind] = n
    }
}

END {
    print "\"error\",\"operation\",\"count\""
    for (row in matrix) {
        printf("%s,%s\n", row, matrix[row])
    }
    print ""
}
