# Import required packages
import scanpy as sc
import pandas as pd
import numpy as np

# Load example brain dataset
adata = sc.datasets.pbmc68k_reduced()

# Basic preprocessing steps
sc.pp.filter_cells(adata, min_genes=200)
sc.pp.filter_genes(adata, min_cells=3)

# Normalize data
sc.pp.normalize_total(adata, target_sum=1e4)
sc.pp.log1p(adata)

# Convert to pandas DataFrame
expression_df = pd.DataFrame(adata.X.toarray(),
                           columns=adata.var_names,
                           index=adata.obs_names)

# Add cell type labels as a column
expression_df['cell_type'] = adata.obs['louvain']

# Save basic info
print("Dataset dimensions:", expression_df.shape)
print("\nNumber of cell types:", len(expression_df['cell_type'].unique()))
print("\nCell type distribution:\n", expression_df['cell_type'].value_counts())

# Save to CSV
expression_df.to_csv('brain_sc_expression.csv', index=True)

# Optional: Save a smaller subset if the file is too large
# Select top 2000 most variable genes
gene_vars = expression_df.iloc[:, :-1].var()  # exclude cell_type column
top_genes = gene_vars.nlargest(2000).index

subset_df = expression_df[list(top_genes) + ['cell_type']]
subset_df.to_csv('brain_sc_expression_subset.csv', index=True)
