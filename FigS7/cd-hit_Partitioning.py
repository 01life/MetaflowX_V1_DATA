import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# 创建数据
data = {
    'chunk_data_size': ['0.1M', '0.4M', '0.6M', '0.8M', '1M', '2M', '4M', '>8M'],
    'total_realtime': [638.75, 541.9, 707.7666667, 950.4, 991.5833333, 1792.2, 1709.35, 5989.15],
    'total_task_number': [386, 101, 71, 53, 44, 26, 21, 1],
    'strategy': ['MetaflowX_multi_CD_HIT'] * 7 + ['cd-hit-para']
}

# 创建DataFrame
df = pd.DataFrame(data)

# 创建图表
fig, ax1 = plt.subplots(figsize=(10, 6))

# 为每个策略创建不同的颜色
colors = {'MetaflowX_multi_CD_HIT': 'tab:green', 'cd-hit-para': 'tab:blue'}

# 绘制total_realtime条形图
bar_colors = [colors[strategy] for strategy in df['strategy']]
bars = ax1.bar(df['chunk_data_size'], df['total_realtime'], color=bar_colors, alpha=0.3)
ax1.set_ylabel('Total Realtime(minute)', color='tab:green',fontsize=10, )
ax1.set_xlabel('Chunk Data Size (in million genes)', fontsize=10)
ax1.tick_params(axis='y', labelcolor='tab:green',labelsize=10)



# 在条形上添加数值标签
for bar in bars:
    height = bar.get_height()
    ax1.text(bar.get_x() + bar.get_width()/2., height,
             f'{height:.2f}',
             ha='center', va='bottom', rotation=45)

# 绘制total_realtime折线图
line1 = ax1.plot(df['chunk_data_size'], df['total_realtime'], color='tab:green', marker='o', label='Total Realtime')

# 创建第二个Y轴用于total_task_number
ax2 = ax1.twinx()
ax2.set_ylabel('Total Task Number', color='tab:red',fontsize=10)
line2 = ax2.plot(df['chunk_data_size'], df['total_task_number'], color='tab:red', marker='s', linestyle='--', label='Total Task Number')
ax2.tick_params(axis='y', labelcolor='tab:red',labelsize=10)

# 添加标题和图例
plt.title(" MetaflowX's CD-HIT Partitioning vs Single cd-hit-para")
lines = line1 + line2
labels = [l.get_label() for l in lines]
ax1.legend(lines, labels, loc='upper left')

# 添加策略变化的注释
change_index = df[df['strategy'] == 'cd-hit-para'].index[0]
plt.annotate('Strategy changes to cd-hit-para', 
             xy=(change_index, df['total_realtime'].iloc[change_index]), 
             xytext=(change_index-1, df['total_realtime'].iloc[change_index] + 1000),
             arrowprops=dict(facecolor='black', shrink=0.05))

# 调整x轴标签
plt.xticks(rotation=45)

# 添加颜色图例来表示不同的策略
from matplotlib.patches import Patch
legend_elements = [Patch(facecolor=colors['MetaflowX_multi_CD_HIT'], edgecolor='black', label='MetaflowX_multi_CD_HIT'),
                   Patch(facecolor=colors['cd-hit-para'], edgecolor='black', label='cd-hit-para')]
ax1.add_artist(ax1.legend(handles=legend_elements, loc='upper center', title="Strategy"))

# 调整布局并显示图表
plt.tight_layout()
plt.show()


plt.savefig("FigS7.pdf", format='pdf')
# 显示图表
plt.show()
