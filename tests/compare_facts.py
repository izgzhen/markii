import csv
import sys
import os
import difflib

def is_int(s):
    try:
        int(s)
        return True
    except ValueError:
        return False

def assert_eq_set(s1, s2):
    assert s1 == s2, s1.difference(s2)

def read_non_int_rows(filepath: str):
    rows = []
    with open(filepath, "r") as fd:
        r1 = csv.reader(fd, delimiter='\t', quoting=csv.QUOTE_NONE)
        for row in r1:
            rows.append("\t".join([c for c in row if not is_int(c)]))
    return rows

if __name__ == "__main__":
    facts_dir_1 = sys.argv[1]
    facts_dir_2 = sys.argv[2]

    assert_eq_set(set(os.listdir(facts_dir_1)), set(os.listdir(facts_dir_2)))

    for filename in os.listdir(facts_dir_1):
        f1 = os.path.join(facts_dir_1, filename)
        f2 = os.path.join(facts_dir_2, filename)
        rows_1 = sorted(read_non_int_rows(f1))
        rows_2 = sorted(read_non_int_rows(f2))
        assert rows_1 == rows_2, "Different %s (without int values):\n%s" % (filename, "\n".join(difflib.unified_diff(rows_1, rows_2)))