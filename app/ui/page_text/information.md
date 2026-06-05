---
editor_options: 
  markdown: 
    wrap: sentence
---

### Technical Information & Methodology

This page provides a detailed explanation of the application's tools, the mathematical frameworks used to quantify semantic similarity and visualize high-dimensional sensorimotor data.

------------------------------------------------------------------------

#### Full Datasets 
This page acts as a comprehensive repository for all 15 available sensorimotor norms (Human and LLM-generated).
* **Searching:** Users can perform global searches to explore the entire conceptual vocabulary.
* **Dataset Export:** Each dataset can be downloaded in its entirety as a `.csv` file.

------------------------------------------------------------------------

#### Sensorimotor profiles 
Inspired by the **Lancaster Sensorimotor Norms** (Lynott et al., [2020](https://doi.org/10.3758/s13428-019-01316-z)), these radial plots show the raw "signature" of a concept.
* **Structure:** Each of the 11 dimensions is a "slice" of the circle.
The length of the slice represents the mean rating strength (0–5).
* **Overlay Analysis:** By overlaying a grey "shadow" (Dataset 2) on top of the colored profile (Dataset 1), you can immediately see which dimensions are over-represented or under-represented by a specific model or language.

------------------------------------------------------------------------

#### Calculate distances 
Calculate distances betwen concepts adapted from **Sensorimotor Distance Calculator** (Wingfield & Connell, [2023](https://doi.org/10.3758/s13428-022-01965-7)). 

**One-to-one**: Calculate distances between pairs of concepts' vector representations.

**One-to-many**: Calculate distances between the vector representation of a concept and several other concepts.

**Many-to-many**: Compute distances within a single list of concepts or with between two lists of concepts.

Distance metrics allow us to quantify the semantic similarity between two concepts based on their 11-dimensional sensorimotor vectors.

* **Euclidean Distance:** The most common metric. It represents the "straight-line" distance between two points in the 11D space. It is sensitive to the absolute magnitude of the ratings.  
  **Interpretation:** Values close to 0 indicate very similar concepts, while larger values indicate greater dissimilarity (no fixed upper bound).  
  The $L_2$ norm, representing the straight-line distance, is:  
  $$d(\mathbf{x}, \mathbf{y}) = \left( \sum_{i=1}^{11} (x_i - y_i)^2 \right)^{1/2}$$

* **Minkowski-3 Distance:** A generalization of the Euclidean distance.  
  By using a power of $p=3$, it places a greater weight on larger differences between specific dimensions, making it more sensitive to "peaks" in sensorimotor strength.  
  **Interpretation:** Values close to 0 indicate high similarity; larger values emphasize strong differences in specific dimensions (no fixed upper bound).  
  $$d(\mathbf{x}, \mathbf{y}) = \left( \sum_{i=1}^{11} |x_i - y_i|^3 \right)^{1/3}$$

* **Cosine Distance:** Instead of measuring the distance between points, it measures the **angle** between two vectors.  
  It is highly effective for semantic similarity because it focuses on the *pattern* or *proportion* of sensorimotor dimensions rather than their absolute intensity.  
  **Interpretation:** 0 indicates identical orientation (very similar patterns), values near 1 indicate unrelated vectors, and values closer to 2 indicate opposite patterns.  
  $$d_{\text{cos}}(\mathbf{x}, \mathbf{y}) = 1 - \frac{\mathbf{x} \cdot \mathbf{y}}{\|\mathbf{x}\| \|\mathbf{y}\|}$$

* **Correlation Distance:** Measures the linear relationship between two profiles ($1 - r$).  
  It captures how well the "shape" of one sensorimotor signature predicts the other, regardless of the scale of the ratings.  
  **Interpretation:** 0 indicates perfectly correlated profiles (same shape), values near 1 indicate no correlation, and values closer to 2 indicate opposite profiles.  
  $$d_{\text{corr}}(\mathbf{x}, \mathbf{y}) = 1 - \frac{\sum (x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum (x_i - \bar{x})^2 \sum (y_i - \bar{y})^2}}$$

* **Mahalanobis Distance:** This is a "direction-sensitive" metric.  
  It accounts for the correlations between the 11 dimensions (e.g., how Visual and Haptic dimensions often move together).  
  It uses the **covariance matrix** of the dataset to normalize the space, effectively measuring distances in terms of standard deviations. Accounts for the variance and correlations between sensorimotor dimensions using the covariance matrix $S$.  
  **Interpretation:** Values close to 0 indicate very similar concepts relative to the structure of the dataset; larger values indicate increasingly unusual or distant points in the space (no fixed upper bound).  
  $$d_{\text{mah}}(\mathbf{x}, \mathbf{y}) = \sqrt{(\mathbf{x} - \mathbf{y})^T S^{-1} (\mathbf{x} - \mathbf{y})}$$

------------------------------------------------------------------------

#### Nearest Neighbours 
The **Nearest Neighbours** tool identifies the most similar concepts to a target word within a specific dataset.
* **Mechanism:** The app computes the distance from your target word to *every other word* in the dataset using your chosen distance metric. For a target word $w$, the algorithm computes $d(w, v)$ for all $v \in V$.
* **Ranking:** Results are ranked from the smallest distance (most similar) to the largest. Results are sorted such that $d(w, v_1) \leq d(w, v_2) \leq ... \leq d(w, v_k)$.
* **Radius:** Users can limit the search to a maximum distance $\epsilon$ to ensure that only truly similar concepts $v$ where $d(w, v) \leq \epsilon$ are displayed.

------------------------------------------------------------------------

#### Visualise Concepts 
**Multidimensional Scaling (MDS)** is a technique used to project the complex 11-dimensional space into a low-dimensional space so that the distances between them reflect their original similarities or dissimilarities. In other words, it projects the 11D space into 2D by minimizing a "stress" function that represents the difference between the 11D distances and the 2D distances.
* **Objective:** To maintain the relative distances between concepts.
If two words are close in the 11D space, MDS attempts to keep them close in the 2D plot.
* **Cross-Dataset View:** When comparing two datasets (e.g., Human vs. LLM), the app plots both versions of the word.
Connecting lines highlight the **semantic shift**, a longer line indicates that a word in the **Dataset 1** is represented differently in the **Dataset 2**.

------------------------------------------------------------------------

#### Sensorimotor space 
**t-Distributed Stochastic Neighbor Embedding (t-SNE)** is a non-linear dimensionality reduction tool used for exploring clusters. t-SNE visualizes high-dimensional data by assigning each point a location in a two- or three-dimensional map while preserving meaningful structure, keeping similar points close and dissimilar points apart (Van der Maaten & Hinton, [2008](https://www.jmlr.org/papers/volume9/vandermaaten08a/vandermaaten08a.pdf)).
* **Clustering:** It is excellent at finding groups of concepts that share similar sensorimotor dominance.
* **Mathematical Approach:** It minimizes the Kullback-Leibler divergence between the joint probabilities of the high-dimensional vectors and the low-dimensional embedding.
* **Adaptive Perplexity:** t-SNE results depend on a parameter called *perplexity*, which can be interpreted as a smooth measure of the effective number of neighbors. The script dynamically adjusts perplexity based on the dataset size $N$ to satisfy $3 \times \text{Perplexity} < N - 1$, ensuring stable embeddings for smaller word lists.
