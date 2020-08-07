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
    assert s1 == s2, "s1 diff s2: %s, s2 diff s1 %s" %(s1.difference(s2), s2.difference(s1))

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
        if 'methodLineNumber' in filename:
            assert len(rows_1)==len(rows_2)
            for i in range(len(rows_1)):
                r_1 = rows_1[i].split('\t')
                r_2 = rows_2[i].split('\t')
                for j in range(len(r_1)):
                    if 'res//' in r_1[j]:
                        r_1[j] = r_1[j][r_1[j].find('res//') + 5:]
                        r_2[j] = r_2[j][r_2[j].find('res//') + 5:]
                    assert r_1[j] == r_2[j], "Different %s (without int values):\n%s" % (filename, "\n".join(difflib.unified_diff(rows_1, rows_2)))
        else:
            assert rows_1 == rows_2, "Different %s (without int values):\n%s" % (filename, "\n".join(difflib.unified_diff(rows_1, rows_2)))