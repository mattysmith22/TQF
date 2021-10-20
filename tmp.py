import bpy
import re

regEx = re.compile(r"c([lr]_mpdObject\d\d_[a-zA-Z0-9]+)")

v_groups = bpy.context.active_object.vertex_groups
for n in v_groups:
    match = regEx.match(n.name);
    if match:
        n.name = f"p{match[1]}";