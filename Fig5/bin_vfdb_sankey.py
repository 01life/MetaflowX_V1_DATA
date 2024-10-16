#!/usr/bin/env python

import sys
import os
import plotly.graph_objects as go


# Function to convert HEX to RGBA
def hex_to_rgba(hex_color, alpha=1.0):
    # Remove the '#' symbol if present
    hex_color = hex_color.lstrip('#')
    # Convert HEX to RGB
    r, g, b = int(hex_color[0:2], 16), int(hex_color[2:4], 16), int(hex_color[4:6], 16)
    # Return RGBA format
    return f'rgba({r}, {g}, {b}, {alpha})'


vf_category_color = {}
with open("vf_category.color",'r') as category_color_F:
    for c in category_color_F:
        vf_catagory, color = c.strip().split("\t")
        vf_category_color[vf_catagory] = color


bin_name = {}
with open("bin.name.txt",'r') as binnameF:
    for b in binnameF:
        binid , binname = b.strip().split("\t")
        bin_name[binid] = binname


#VFDB
bin_list = []
bin_vfdb = {}
bin_vf_category_dir = {}
vf_category_dir={}
vf_category_vf_dir = {}
vf_set = set()
vf_info_file = [os.path.join("./vfdb/", f) for f in os.listdir("./vfdb/") if f.endswith("VF_info.summary") ]

for vf in vf_info_file:
    bin_num  = os.path.basename(vf).rstrip(".VF_info.summary")

    binID = bin_name[bin_num]

    bin_list.append(binID)

    with open(vf,'r')  as vf_infoF:
        header = vf_infoF.readline()
        for a in vf_infoF:
            al = a.strip().split("\t")
            vf_name = al[0].split("|")[1].split("(")[0].strip()
            vf_category = al[4]
            vf_category_dir[vf_name] = vf_category
            vf_category_vf_dir.setdefault(vf_category,[]).append(vf_name)
            # bin_vfdb.setdefault(binID,[]).append(vf_name)
            bin_vf_category_dir.setdefault(binID,[]).append(vf_category)
            vf_set.add(vf_name)



#creat node index 
node_index = {}
all_node = bin_list + list(vf_category_color.keys()) + list(vf_set)
node_color_list = []

n = 0
for i in all_node:
    node_index[i] = n
    n += 1
    if i in vf_set: #vf
        node_color = vf_category_color[vf_category_dir[i]]
    elif i in vf_category_color: #vf category
        node_color = vf_category_color[i]
    elif i in  bin_list: #bin
        node_color = "#d5bdaf"

    node_color_list.append(node_color)



source =  []
target =  []
link_value = []
link_color = []

#VFDB

#bin id -- vfdb category
for binid, vf_category_list in bin_vf_category_dir.items():
    for vf in vf_category_list:
        source.append(node_index[binid])
        target.append(node_index[vf])
        link_value.append(1)

        hex_color = vf_category_color[vf]
        link_color.append(hex_to_rgba(hex_color,0.5))


        #count_of_3 = my_list.count(3)

# vfdb category -- vf
for vf_category, vf_list in vf_category_vf_dir.items():
    for vf in vf_list:
        source.append(node_index[vf_category])
        target.append(node_index[vf])
        link_value.append(1)
        link_color.append("#f0f0f0")
        # link_color.append(vf_category_color[vf_category])


fig = go.Figure(data=[go.Sankey(
    valueformat = ".0f",
    # valuesuffix = "TWh",
    # Define nodes
    node = dict(
      pad = 15,
      thickness = 15,
      line = dict(color = "black", width = 0.5),
      label =  all_node,
      color = node_color_list
    ),
    # Add links
    link = dict(
      source =  source,
      target =  target,
      value =  link_value,
      color =  link_color


))])

fig.update_layout(title_text="",
                    font=dict(
                        family="Arial",     # 设置字体为 Arial
                        size=10             # 设置字体大小为 10
                    ),

                    width=600,   # 设置图的宽度
                    height=800,  # 设置图的高度
                    margin=dict(
                        l=50,  # 左侧留白
                        r=50,  # 右侧留白
                        t=50,  # 上方留白
                        b=50   # 下方留白
                    )
                )
fig.show()

# 保存图表为 PDF 文件
fig.write_image("bin_VFDB_sankey.pdf", format="pdf")     
