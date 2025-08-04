import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
from matplotlib.ticker import FuncFormatter

# 准备数据
data = {
    'group': ['168K', '346K', '2.4M', '4.4M'],
    'chunked': [17.23, 18.52, 67.52, 70.81],
    'single': [28.98, 44.55, 306.78, 876.85],
    'protein_set_size': [168368, 346786, 2446820, 4486950]
}
df = pd.DataFrame(data)

# 图形设置
fig, ax1 = plt.subplots(figsize=(4,3))

# 字体设置
label_font = {"fontsize": 8, "fontweight": "bold"}
tick_font = {"labelsize": 8, "width": 1.5}

# 条形图位置与宽度
x = range(len(df))
bar_width = 0.35

# 绘制柱状图
bars1 = ax1.bar([p - bar_width/2 for p in x], df['chunked'], width=bar_width,
                label='MetaflowX (Chunked)', color='#24B064', alpha=0.8)
bars2 = ax1.bar([p + bar_width/2 for p in x], df['single'], width=bar_width,
                label='Single-run', color='#636363', alpha=0.8)

# 设置 X 轴
ax1.set_xticks(x)
ax1.set_xticklabels(df['group'], fontweight='bold')
ax1.set_xlabel("Protein Set Size", **label_font)

# 设置左 Y 轴（运行时间）
ax1.set_ylabel("eggNOG Runtime (min)", **label_font, color='black')
ax1.tick_params(axis='y', colors='black', **tick_font)
ax1.set_ylim(0, max(df['single']) * 1.13)
ax1.yaxis.set_major_formatter(FuncFormatter(lambda y, _: f"{y:.0f}"))

# 图例设置
bar_legend = [
    Patch(facecolor='#24B064', edgecolor='black', label='MetaflowX (Chunked)'),
    Patch(facecolor='#636363', edgecolor='black', label='Single-run')
]
ax1.legend(handles=bar_legend, loc='upper left', fontsize=8, frameon=False)

# 第二坐标轴设置（蛋白集合大小）
highlight_color = "#bf0603"
ax2 = ax1.twinx()
ax2.plot(x, df['protein_set_size'], color=highlight_color, marker='o',
         linewidth=2, markersize=5, label='Protein Numbers')
ax2.set_ylabel('Protein Numbers', color=highlight_color, **label_font)
ax2.tick_params(axis='y', colors=highlight_color, **tick_font)
ax2.set_ylim(0, max(df['protein_set_size']) * 1.13)
ax2.yaxis.set_major_formatter(FuncFormatter(lambda y, _: f"{int(y):,}"))

# 添加蛋白数量数值标签
for i, val in enumerate(df['protein_set_size']):
    ax2.text(i, val + 0.05 * val, f'{val:,}', ha='center', va='bottom',
             fontsize=7, fontweight='bold', color="black",rotation=10)

# 布局与保存
plt.tight_layout(pad=0.5)
plt.savefig("Fig2e_eggNOG-mapper_runtime.pdf", format='pdf', bbox_inches='tight')
plt.show()
