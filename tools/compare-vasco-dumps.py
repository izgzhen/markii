import json
import sys
from typing import Any, List, Mapping, Tuple
from pydantic import BaseModel
from pydantic.tools import parse_obj_as

class MethodAbs(BaseModel):
    localNodeMap: Mapping[str, List[Tuple[str, str]]]
    globalNodeMap: Mapping[str, List[Tuple[str, str]]]

with open(sys.argv[1], "r") as f:
    dump1: Mapping[str, List[Tuple[str, Any]]] = json.load(f)
with open(sys.argv[2], "r") as f:
    dump2: Mapping[str, List[Tuple[str, Any]]] = json.load(f)

def print_dump_info(name: str, dump):
    print(len(dump))

def get_sizes(abss: List[Tuple[str, MethodAbs]]):
    return [{
        "local_node_map_size": len(method_abs.localNodeMap),
        "global_node_map_size": len(method_abs.globalNodeMap),
    } for unit, method_abs in abss]

print_dump_info(sys.argv[1], dump1)
print_dump_info(sys.argv[2], dump2)

print("Common method diff:")

for m in set(dump1.keys()).intersection(set(dump2.keys())):
    abs1 = dump1[m]
    abs2 = dump2[m]
    parsed1 = parse_obj_as(List[Tuple[str, MethodAbs]], abs1)
    parsed2 = parse_obj_as(List[Tuple[str, MethodAbs]], abs2)
    sizes1 = get_sizes(parsed1)
    sizes2 = get_sizes(parsed2)
    assert len(sizes1) == len(sizes2)
    if sizes1 != sizes2:
        print("Difference abstraction sizes:")
        for s1, s2 in zip(sizes1, sizes2):
            for k in s1.keys():
                if s1[k] != s2[k]:
                    print("- %s: %s != %s" % (k, s1[k], s2[k]))
        print()