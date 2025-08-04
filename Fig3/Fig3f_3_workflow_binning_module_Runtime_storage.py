import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter

# 数据（指定顺序）
workflows = ["MetaflowX", "nf-mag", "MetaWRAP"]
runtime = [3.94,6.03,136.19]      # 左轴：运行时间（h）
storage = [320,1843.2,714]             # 右轴：存储消耗（GB）

# 配色
bar_colors = ["#24B064", "#118ab2", "#f07167"]
line_color = "#333333"

# 图像尺寸：宽度 4 英寸，高度 3 英寸（适合拼图）
fig, ax1 = plt.subplots(figsize=(3, 3))

# 字体参数
label_font = {"fontsize": 8, "fontweight": "bold"}
tick_font = {"labelsize": 8, "width": 1.5}

# 左轴：柱状图（运行时间）
bars = ax1.bar(workflows, runtime, color=bar_colors, width=0.6, alpha=0.8)
ax1.set_ylabel("Runtime (hours)", **label_font, color="#333333")
ax1.tick_params(axis='y', colors="#333333", **tick_font)
ax1.tick_params(axis='x', **tick_font)
ax1.set_ylim(0, max(runtime) * 1.25)  # 留白防止溢出

# 设置 X 轴标签加粗
ax1.set_xticks(range(len(workflows)))
ax1.set_xticklabels(workflows, fontweight='bold')

# 设置左 Y 轴标签格式和加粗
ax1.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:.0f}"))
for label in ax1.get_yticklabels():
    label.set_fontweight('bold')

# 添加运行时间标签
for i, r in enumerate(runtime):
    ax1.text(i, r + 2, f"{r:.1f}", ha='center', va='bottom',
             fontsize=8, fontweight='bold')

# 右轴：折线图（存储消耗）
ax2 = ax1.twinx()
ax2.plot(workflows, storage, color=line_color, marker='o',
         linewidth=2.5, markersize=7)
ax2.set_ylabel("Storage Usage (GB)", **label_font, color=line_color)
ax2.tick_params(axis='y', colors=line_color, **tick_font)
ax2.set_ylim(0, max(storage) * 1.25)

# 设置右 Y 轴标签格式和加粗
ax2.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{int(x)}"))
for label in ax2.get_yticklabels():
    label.set_fontweight('bold')

# 添加存储消耗标签
for i, s in enumerate(storage):
    ax2.text(i, s + 20, f"{s}", ha='center', va='bottom',
             fontsize=8, fontweight='bold', color=line_color, rotation=-30)

# 输出高质量 PDF 图像
plt.tight_layout()
plt.savefig("Fig3f_3workflow_binning.pdf",
            format='pdf', bbox_inches='tight')
plt.show()
