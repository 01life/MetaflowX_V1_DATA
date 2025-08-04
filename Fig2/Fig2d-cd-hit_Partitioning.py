import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
from matplotlib.ticker import FuncFormatter

# Prepare data
data = {
    'chunk_data_size': ['0.1M', '0.4M', '0.6M', '0.8M', '1M', '2M', '4M', '>8M'],
    'total_realtime': [638.75, 541.9, 707.77, 950.4, 991.58, 1792.2, 1709.35, 5989.15],
    'total_task_number': [386, 101, 71, 53, 44, 26, 21, 1],
    'strategy': ['MetaflowX_multi_CD_HIT'] * 7 + ['cd-hit-para']
}
df = pd.DataFrame(data)

# Define color mapping
colors = {
    'MetaflowX_multi_CD_HIT': '#24B064',
    'cd-hit-para': '#636363'
}

# Create figure
fig, ax1 = plt.subplots(figsize=(4, 3))

# Font config
label_font = {"fontsize": 8, "fontweight": "bold"}
tick_font = {"labelsize": 8, "width": 1.5}

# Draw bar chart
bar_colors = [colors[s] for s in df['strategy']]
bars = ax1.bar(df['chunk_data_size'], df['total_realtime'], color=bar_colors, width=0.6, alpha=0.8)

# Draw line on primary y-axis
ax1.plot(df['chunk_data_size'], df['total_realtime'], color='#24B064',
         marker='o', linewidth=2, markersize=5)

# Primary y-axis config
ax1.set_ylabel('CD-HIT Runtime (min)', **label_font, color='black')
ax1.set_xlabel('Chunk Size (M genes)', **label_font)
ax1.tick_params(axis='y', colors='black', **tick_font)
ax1.tick_params(axis='x', **tick_font)
ax1.set_ylim(0, max(df['total_realtime']) * 1.25)

# 加粗坐标轴标签字体
ax1.set_xticks(range(len(df['chunk_data_size'])))
ax1.set_xticklabels(df['chunk_data_size'], fontweight='bold', rotation=45)
ax1.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:.0f}"))
for label in ax1.get_yticklabels():
    label.set_fontweight('bold')

# Add bar value labels
for bar in bars:
    height = bar.get_height()
    ax1.text(bar.get_x() + bar.get_width() / 2., height + 100,
             f'{height:.0f}', ha='center', va='bottom',
             fontsize=8, fontweight='bold', rotation=45)

# Secondary y-axis
highlight_color = "#bf0603"
ax2 = ax1.twinx()
ax2.plot(df['chunk_data_size'], df['total_task_number'], color=highlight_color,
         marker='s', linestyle='--', linewidth=1.5, markersize=4, label='Task Number')
ax2.set_ylabel('Task Number', color=highlight_color, **label_font)
ax2.tick_params(axis='y', colors=highlight_color, **tick_font)
ax2.set_ylim(0, max(df['total_task_number']) * 1.5)
ax2.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{int(x)}"))
for label in ax2.get_yticklabels():
    label.set_fontweight('bold')

# Annotate strategy change
change_index = df[df['strategy'] == 'cd-hit-para'].index[0]
ax1.annotate('cd-hit-para',
             xy=(change_index, df['total_realtime'].iloc[change_index]),
             xytext=(change_index - 1.2, df['total_realtime'].iloc[change_index] + 1200),
             fontsize=8, fontweight='bold',
             arrowprops=dict(facecolor='black', arrowstyle='->'),
             ha='right')

# Add strategy legend
strategy_legend = [
    Patch(facecolor=colors['MetaflowX_multi_CD_HIT'], edgecolor='black', label='MetaflowX_multi_CD_HIT'),
    Patch(facecolor=colors['cd-hit-para'], edgecolor='black', label='cd-hit-para')
]
ax1.legend(handles=strategy_legend, loc='center', fontsize=8, frameon=False, ncol=1)

# Export
plt.tight_layout(pad=0.5)
plt.savefig("Fig2d_CD-hit_runtime.pdf", format='pdf', bbox_inches='tight')
plt.show()
