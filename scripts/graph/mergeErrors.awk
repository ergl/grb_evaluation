#!/usr/bin/awk -f

BEGIN {
    FS=","
}

FNR >= 2 {
    kind_a=$(NF-3)
    kind_b=$(NF-2)
    kind_c=$(NF-1)

    # Squash the fields into a single string, so we can print it later
    kind=sprintf("%s,%s,%s", kind_a, kind_b, kind_c)

    # Remove the quotes from the last field, it never contains a string
    gsub("\"", "", $NF)
    n=$(NF)

    if (kind in matrix) {
        matrix[kind] += n
    } else {
        matrix[kind] = n
    }
}

END {
    print "\"error\",\"operation\",count"
    for (row in matrix) {
        printf("%s,%s\n", row, matrix[row])
    }
    print ""
}
