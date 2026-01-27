# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.Finding do
  @moduledoc """
  Represents a security finding from the scanner.
  """

  use TypedStruct

  typedstruct do
    field :severity, :critical | :high | :medium | :low, enforce: true
    field :type, String.t(), enforce: true
    field :pattern, String.t(), enforce: true
    field :file, String.t(), enforce: true
    field :line, integer(), enforce: true
    field :code, String.t(), enforce: true
    field :cwe, String.t(), enforce: true
    field :fix, String.t(), enforce: true
  end

  @doc """
  Convert finding to JSON-encodable map.
  """
  def to_json(%__MODULE__{} = finding) do
    %{
      severity: Atom.to_string(finding.severity),
      type: finding.type,
      pattern: finding.pattern,
      file: finding.file,
      line: finding.line,
      code: finding.code,
      cwe: finding.cwe,
      fix: finding.fix
    }
  end

  @doc """
  Create a new finding.
  """
  def new(attrs) do
    struct!(__MODULE__, attrs)
  end
end
