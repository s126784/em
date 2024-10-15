# Mathematical Description of CDPCA

## Overview

The Clustering and Disjoint Principal Components Analysis (CDPCA) aims to simultaneously cluster rows and columns of a data matrix and extract principal components from the clustered data. Given a data matrix \( X \in \mathbb{R}^{I \times J} \), the goal is to partition it into \( P \) clusters of rows (objects) and \( Q \) clusters of columns (variables) while extracting \( Q \) disjoint principal components.

## Mathematical Steps

### Step 1: Data Normalization

First, normalize the data matrix \( X \):
```math
\[
X_s = \frac{X - \mu_X}{\sigma_X} \cdot \sqrt{\frac{I}{I-1}}
\]
```
where \( \mu_X \) and \( \sigma_X \) are the mean and standard deviation of the columns of \( X \).

### Step 2: Initialize Matrices

Initialize random binary and row-stochastic matrices:
```math
\[
U \in \{0,1\}^{I \times P}, \quad V \in \{0,1\}^{J \times Q}
\]
```
Each row of \( U \) and \( V \) contains exactly one non-zero entry.

### Step 3: Iterative Optimization

Iteratively update matrices \( U \), \( V \), and \( A \) (the component loading matrix).

#### Update \( A \)

For each cluster \( j \) in \( V \), find the dominant eigenvector using the power method:

1. Compute the within-cluster scatter matrix:
```math
   \[
   S_{\text{group}} = X_{\text{group}, J_{xx}}^\top X_{\text{group}, J_{xx}}
   \]
```
2. Compute or update the loading vectors:
```math
   \[
   A_{J_{xx}, j} = \text{dominant eigenvector of } S_{\text{group}}
   \]
```
#### Update \( U \) and \( V \)

Find new partitions by minimizing Euclidean distances:

- **Update \( U \):** Assign each object to the nearest centroid in the reduced space.

- **Update \( V \):** For each variable, determine the cluster by evaluating the increase in objective function \( \mathcal{F} \):
```math
  \[
  \mathcal{F} = \text{trace}\left((U Y_{\text{bar}})^\top (U Y_{\text{bar}})\right)
  \]
```
### Step 4: Converged Solution

Upon convergence determined by \( \text{tolerance} = \epsilon \):

- Normalize and reorder \( A \) according to the explained variances of the columns of \( X_s A \).

## Output

The algorithm outputs the sorted loading matrix \( A_{\text{order}} \), where columns are ordered by the variance they explain in the reduced space.

---

This README provides a mathematical overview and step-by-step guide to implementing the CDPCA algorithm using LaTeX and MathJax for documentation and publication.
