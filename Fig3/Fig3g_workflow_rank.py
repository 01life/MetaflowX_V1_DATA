# Python 脚本：workflow 多维度评分与雷达图绘制
# 功能：根据 MQ/HQ bin、genus、species（每个样本下多个 workflow 比较）
#       以及整体的效率指标（时间+存储）评估 workflow 综合表现，并绘制雷达图

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from math import pi

# 输入数据文件（请根据实际路径替换）
MQ_bin_df = pd.read_csv("bin_quality_MQ.tsv", sep="\t")
HQ_bin_df = pd.read_csv("bin_quality_HQ.tsv", sep="\t")
genus_df = pd.read_csv("genus.tsv", sep="\t")
species_df = pd.read_csv("species.tsv", sep="\t")
efficiency_df = pd.read_csv("efficiency.tsv", sep="\t")

# 排名函数：对每个 sample_id 内 workflow 排名并归一化

def rank_score_per_sample(df, value_col):
    df = df.copy()
    df['rank'] = df.groupby('sample_id')[value_col].rank(ascending=False, method='min')
    df['rank_score'] = df.groupby('sample_id')['rank'].transform(lambda x: 1 - (x - 1) / (x.max() - 1))
    return df.groupby('workflow', as_index=False)['rank_score'].mean().rename(columns={'rank_score': f"{value_col}_score"})

# 处理数据
MQ_bin_df_s = MQ_bin_df[['sample_id', 'workflow', 'Bins_Count']]
HQ_bin_df_s = HQ_bin_df[['sample_id', 'workflow', 'Bins_Count']]
genus_df_s = genus_df[['sample_id', 'workflow', 'level_count']]
species_df_s = species_df[['sample_id', 'workflow', 'level_count']]

# 计算分数
MQ_score = rank_score_per_sample(MQ_bin_df_s, 'Bins_Count')
HQ_score = rank_score_per_sample(HQ_bin_df_s, 'Bins_Count')
genus_score = rank_score_per_sample(genus_df_s, 'level_count')
species_score = rank_score_per_sample(species_df_s, 'level_count')

# 效率分数（反向归一）
eff = efficiency_df.copy()
eff['time_score'] = (eff['efficiency_time'].max() - eff['efficiency_time']) / (eff['efficiency_time'].max() - eff['efficiency_time'].min())
eff['storage_score'] = (eff['efficiency_storage'].max() - eff['efficiency_storage']) / (eff['efficiency_storage'].max() - eff['efficiency_storage'].min())

# 合并得分表（包含 time_score 与 storage_score 单独输出）
rank_table = MQ_score.merge(HQ_score, on='workflow', suffixes=('_MQ', '_HQ')) \
    .merge(genus_score, on='workflow') \
    .merge(species_score, on='workflow', suffixes=('', '_species')) \
    .merge(eff[['workflow', 'time_score', 'storage_score']], on='workflow')

rank_table = rank_table.rename(columns={
    'Bins_Count_score_MQ': 'MQ_score',
    'Bins_Count_score_HQ': 'HQ_score',
    'level_count_score': 'Genus_score',
    'level_count_score_species': 'Species_score'
})

# 计算综合得分（可选）
rank_table['rank_score'] = rank_table[[
    'HQ_score', 'MQ_score', 'Genus_score', 'Species_score', 'time_score', 'storage_score'
]].mean(axis=1)

# 保存 rank_table 为 CSV
rank_table.to_csv("workflow_rank_table.csv", index=False)

# 准备雷达图，按指定顺序
categories = ['time_score', 'storage_score', 'Genus_score', 'Species_score', 'HQ_score', 'MQ_score']
labels = ["MetaflowX", "nf-mag", "MetaWRAP"]
data = rank_table.set_index('workflow').loc[labels][categories].values

# 加上最大值与最小值行（用于标准化坐标轴）
data = np.vstack([np.ones(len(categories)), np.zeros(len(categories)), data])

# 绘制雷达图
angles = [n / float(len(categories)) * 2 * pi for n in range(len(categories))]
angles += angles[:1]  # 闭合

fig, ax = plt.subplots(figsize=(3, 3), subplot_kw=dict(polar=True))
colors = ["#24B064", "#118ab2", "#f07167"]

for i in range(2, data.shape[0]):
    values = data[i].tolist()
    values += values[:1]
    ax.plot(angles, values, linewidth=2, linestyle='solid', label=labels[i-2], color=colors[i-2])
    ax.fill(angles, values, alpha=0.1, color=colors[i-2])
    # 添加数值标签
    # for j, v in enumerate(values[:-1]):
    #     ax.text(angles[j], v + 0.04, f"{v:.2f}", color=colors[i-2], fontsize=8, ha='center', va='center')

ax.set_theta_offset(pi / 2)
ax.set_theta_direction(-1)
ax.set_xticks(angles[:-1])
ax.set_xticklabels(['Time', 'Storage', 'Genus', 'Species', 'HQ', 'MQ'])
ax.set_yticks(np.linspace(0, 1, 6))
ax.set_yticklabels(["0.0", "0.2", "0.4", "0.6", "0.8", "1.0"])
ax.yaxis.grid(True)
plt.title("Rank Scores")
plt.tight_layout()
plt.savefig("Fig3g.workflow_rank_radar.pdf", format='pdf', bbox_inches='tight')
plt.show()
