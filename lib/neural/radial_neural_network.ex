# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Neural.RadialNeuralNetwork do
  @moduledoc """
  Radial Basis Function (RBF) Neural Network for Hypatia.

  A three-layer network using Gaussian radial basis functions for
  interpolation and classification of findings. Particularly effective
  for the "is this finding similar to known patterns?" problem.

  Architecture:
  - Input layer: finding feature vector
  - Hidden layer: RBF neurons with Gaussian activation (centers + widths)
  - Output layer: linear combination of RBF activations

  Key properties:
  - Universal approximation with fewer neurons than MLP
  - Centers learned via k-means clustering on training data
  - Widths set by average inter-center distance
  - Output weights trained via least squares (fast, no backprop needed)
  - Naturally provides confidence measure (distance to nearest center)

  This enables:
  - Finding classification (which category does this finding belong to?)
  - Novelty detection (findings far from all centers are novel)
  - Confidence estimation (closeness to centers = higher confidence)
  - Pattern similarity search (find centers closest to a query)
  """

  require Logger

  @default_num_centers 20
  @kmeans_max_iterations 50
  @width_scale 1.5  # Scale factor for RBF widths

  defstruct [
    centers: [],
    widths: [],
    output_weights: [],
    num_inputs: 0,
    num_centers: 0,
    num_outputs: 1,
    trained: false,
    training_stats: %{}
  ]

  # --- Public API ---

  @doc "Initialize an RBF network with specified architecture"
  def init(opts \\ []) do
    num_centers = Keyword.get(opts, :num_centers, @default_num_centers)

    %__MODULE__{
      num_centers: num_centers,
      num_outputs: Keyword.get(opts, :num_outputs, 1)
    }
  end

  @doc "Train the RBF network on labeled data"
  def train(%__MODULE__{} = rbf, data, targets) when length(data) >= 5 do
    num_inputs = length(List.first(data, []))
    num_centers = min(rbf.num_centers, length(data))

    # Step 1: K-means clustering to find centers
    centers = kmeans(data, num_centers)

    # Step 2: Compute widths (average distance to nearest center)
    widths = compute_widths(centers)

    # Step 3: Compute hidden layer activations for all training data
    activations = Enum.map(data, fn input ->
      Enum.map(Enum.zip(centers, widths), fn {center, width} ->
        gaussian(input, center, width)
      end)
    end)

    # Step 4: Solve for output weights (least squares)
    output_weights = solve_least_squares(activations, targets)

    # Compute training error
    predictions = Enum.map(activations, fn acts ->
      Enum.zip(acts, output_weights) |> Enum.reduce(0.0, fn {a, w}, acc -> acc + a * w end)
    end)

    mse = Enum.zip(predictions, targets)
    |> Enum.reduce(0.0, fn {p, t}, acc -> acc + (p - t) * (p - t) end)
    |> Kernel./(length(targets))

    Logger.info("RBF trained: #{num_centers} centers, #{num_inputs} inputs, MSE=#{Float.round(mse, 6)}")

    %{rbf |
      centers: centers,
      widths: widths,
      output_weights: output_weights,
      num_inputs: num_inputs,
      num_centers: num_centers,
      trained: true,
      training_stats: %{mse: mse, samples: length(data), centers: num_centers}
    }
  end

  def train(%__MODULE__{} = rbf, _data, _targets) do
    Logger.warning("RBF: insufficient training data (need >= 5 samples)")
    rbf
  end

  @doc "Predict output for an input vector"
  def predict(%__MODULE__{trained: false}, _input), do: {0.5, 0.0}

  def predict(%__MODULE__{} = rbf, input) do
    activations = Enum.map(Enum.zip(rbf.centers, rbf.widths), fn {center, width} ->
      gaussian(input, center, width)
    end)

    output = Enum.zip(activations, rbf.output_weights)
    |> Enum.reduce(0.0, fn {a, w}, acc -> acc + a * w end)

    # Confidence = max activation (how close to a known center)
    confidence = Enum.max(activations, fn -> 0.0 end)

    {sigmoid(output), confidence}
  end

  @doc "Find the nearest center to an input (for classification / similarity)"
  def nearest_center(%__MODULE__{trained: false}, _input), do: nil

  def nearest_center(%__MODULE__{centers: centers}, input) do
    centers
    |> Enum.with_index()
    |> Enum.min_by(fn {center, _idx} -> euclidean_distance(input, center) end)
    |> then(fn {center, idx} -> %{index: idx, center: center, distance: euclidean_distance(input, center)} end)
  end

  @doc "Detect novel findings (far from all centers)"
  def detect_novelty(%__MODULE__{trained: false}, _input), do: :untrained

  def detect_novelty(%__MODULE__{} = rbf, input) do
    distances = Enum.map(rbf.centers, fn center -> euclidean_distance(input, center) end)
    min_distance = Enum.min(distances, fn -> 999.0 end)

    # Threshold based on average width
    avg_width = Enum.sum(rbf.widths) / max(length(rbf.widths), 1)
    threshold = avg_width * 3.0

    if min_distance > threshold do
      {:novel, min_distance}
    else
      {:known, min_distance}
    end
  end

  @doc "Extract features from a finding for RBF input"
  def finding_to_vector(finding) do
    [
      severity_score(Map.get(finding, "severity", "medium")),
      Map.get(finding, "confidence", 0.5),
      Map.get(finding, "complexity", 0.5),
      Map.get(finding, "frequency", 0.5),
      Map.get(finding, "fix_rate", 0.5),
      category_score(Map.get(finding, "category", "")),
      Map.get(finding, "age_days", 30) / 365.0,  # Normalize to ~[0,1]
      Map.get(finding, "affected_files", 1) / 100.0  # Normalize
    ]
  end

  # --- K-Means Clustering ---

  defp kmeans(data, k) do
    # Initialize centers randomly from data points
    initial_centers = data |> Enum.shuffle() |> Enum.take(k)

    Enum.reduce_while(1..@kmeans_max_iterations, initial_centers, fn _iter, centers ->
      # Assign each point to nearest center
      clusters = Enum.group_by(data, fn point ->
        centers
        |> Enum.with_index()
        |> Enum.min_by(fn {center, _} -> euclidean_distance(point, center) end)
        |> elem(1)
      end)

      # Recompute centers
      new_centers = Enum.map(0..(k-1), fn i ->
        cluster = Map.get(clusters, i, [])
        if cluster == [] do
          Enum.at(centers, i)
        else
          centroid(cluster)
        end
      end)

      # Check convergence
      max_shift = Enum.zip(centers, new_centers)
      |> Enum.map(fn {old, new} -> euclidean_distance(old, new) end)
      |> Enum.max(fn -> 0.0 end)

      if max_shift < 0.001 do
        {:halt, new_centers}
      else
        {:cont, new_centers}
      end
    end)
  end

  defp centroid(points) do
    n = length(points)
    dim = length(List.first(points, []))

    sums = Enum.reduce(points, List.duplicate(0.0, dim), fn point, acc ->
      Enum.zip(acc, point) |> Enum.map(fn {a, p} -> a + p end)
    end)

    Enum.map(sums, fn s -> s / n end)
  end

  defp compute_widths(centers) do
    Enum.map(centers, fn center ->
      distances = Enum.map(centers, fn other ->
        if other == center, do: 999.0, else: euclidean_distance(center, other)
      end)

      nearest = Enum.min(distances, fn -> 1.0 end)
      max(nearest * @width_scale, 0.01)
    end)
  end

  # --- RBF Kernel ---

  defp gaussian(input, center, width) do
    dist_sq = Enum.zip(input, center)
    |> Enum.reduce(0.0, fn {i, c}, acc -> acc + (i - c) * (i - c) end)

    :math.exp(-dist_sq / (2.0 * width * width))
  end

  # --- Linear Algebra ---

  defp euclidean_distance(a, b) do
    Enum.zip(a, b)
    |> Enum.reduce(0.0, fn {ai, bi}, acc -> acc + (ai - bi) * (ai - bi) end)
    |> :math.sqrt()
  end

  defp solve_least_squares(activations, targets) do
    # Gradient descent for output weights
    size = length(List.first(activations, []))
    weights = List.duplicate(0.0, size)
    lr = 0.01
    epochs = 300

    Enum.reduce(1..epochs, weights, fn _epoch, w ->
      Enum.zip(activations, targets)
      |> Enum.reduce(w, fn {acts, target}, w_acc ->
        pred = Enum.zip(acts, w_acc) |> Enum.reduce(0.0, fn {a, wi}, acc -> acc + a * wi end)
        error = target - pred
        Enum.zip(w_acc, acts) |> Enum.map(fn {wi, ai} -> wi + lr * error * ai end)
      end)
    end)
  end

  defp sigmoid(x), do: 1.0 / (1.0 + :math.exp(-max(min(x, 500), -500)))

  # Match severity strings case-insensitively.
  # The verisimdb-data registry uses title-case ("Medium", "High") while
  # other callers may use lowercase.
  defp severity_score(sev) when is_binary(sev) do
    case String.downcase(sev) do
      "critical" -> 1.0
      "high" -> 0.8
      "medium" -> 0.5
      "low" -> 0.3
      "info" -> 0.1
      _ -> 0.5
    end
  end

  defp severity_score(_), do: 0.5

  defp category_score(cat) do
    cat = String.downcase(cat)
    cond do
      String.contains?(cat, "security") -> 0.9
      String.contains?(cat, "quality") -> 0.7
      String.contains?(cat, "license") -> 0.5
      String.contains?(cat, "doc") -> 0.3
      String.contains?(cat, "style") -> 0.1
      true -> 0.5
    end
  end
end
