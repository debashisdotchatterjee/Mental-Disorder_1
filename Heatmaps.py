import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Reload the dataset
xlsx_path = "Mental disorder symptoms.xlsx"
df_full = pd.read_excel(xlsx_path)

# Standardize column names
df_full.columns = [c.strip().lower().replace(' ', '_') for c in df_full.columns]

# Select binary symptom columns only
symptom_cols = [c for c in df_full.columns if c not in ('age', 'disorder')]
df = df_full[symptom_cols].copy()

# Convert Y/N or text fields to 0/1
for col in df.columns:
    if df[col].dtype == object:
        df[col] = df[col].astype(str).str.lower().map({'yes': 1, 'no': 0}).fillna(0).astype(int)

# -------------------
# 1. Factor Analysis
# -------------------
from sklearn.decomposition import FactorAnalysis

fa = FactorAnalysis(n_components=5, random_state=0)
fa.fit(df)

loadings = pd.DataFrame(fa.components_.T,
                        index=df.columns,
                        columns=[f"Factor_{i+1}" for i in range(5)])
loadings.to_csv("/mnt/data/factor_loadings.csv")

# Heatmap of factor loadings
plt.figure(figsize=(10, 6))
sns.heatmap(loadings, annot=True, fmt=".2f", cmap="RdBu_r", center=0,
            cbar_kws={"label": "Loading"})
plt.title("Factor Loadings Heatmap (5 Components, Unrotated)")
plt.tight_layout()
plt.savefig("Figure_EFA_Heatmap.png", dpi=300)
plt.savefig("Figure_EFA_Heatmap.pdf")
plt.close()

# -------------------------------
# 2. Tetrachoric Approx (Pearson)
# -------------------------------
tcorr = np.corrcoef(df.T)
tcorr_df = pd.DataFrame(tcorr, index=df.columns, columns=df.columns)
tcorr_df.to_csv("/mnt/data/tetra_corr.csv")

# Heatmap of tetrachoric correlation (approx)
plt.figure(figsize=(11, 9))
sns.heatmap(tcorr_df, cmap="coolwarm", center=0, vmin=-1, vmax=1,
            cbar_kws={"label": "Tetrachoric Correlation (Approx.)"})
plt.title("Tetrachoric Correlation Matrix Heatmap")
plt.tight_layout()
plt.savefig("/mnt/data/Figure_TCM_Heatmap.png", dpi=300)
plt.savefig("/mnt/data/Figure_TCM_Heatmap.pdf")
plt.close()

"/mnt/data/Figure_EFA_Heatmap.[pdf|png], /mnt/data/Figure_TCM_Heatmap.[pdf|png] generated."
