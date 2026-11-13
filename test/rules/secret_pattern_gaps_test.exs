# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.SecretPatternGapsTest do
  @moduledoc """
  Two secret shapes the scanner missed (estate bot findings, 2026-11):
  an untyped PKCS#8 private-key header, which is the GitHub App key format,
  and a literal value assigned to a `*_TOKEN` name.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Rules.SecurityErrors

  # Headers are built at runtime so this file holds no literal key header.
  defp header(kind), do: "-----BEGIN " <> kind <> "-----"

  describe "private key headers" do
    for kind <- [
          "PRIVATE KEY",
          "ENCRYPTED PRIVATE KEY",
          "RSA PRIVATE KEY",
          "EC PRIVATE KEY",
          "OPENSSH PRIVATE KEY",
          "PGP PRIVATE KEY BLOCK"
        ] do
      test "#{kind} is a Private Key" do
        assert "Private Key" in SecurityErrors.detect_secrets(header(unquote(kind)))
      end
    end

    test "a public key or certificate is not" do
      refute "Private Key" in SecurityErrors.detect_secrets(header("PUBLIC KEY"))
      refute "Private Key" in SecurityErrors.detect_secrets(header("CERTIFICATE"))
    end
  end

  describe "*_TOKEN assignments" do
    # Values are assembled so the file never holds a token-shaped literal.
    @value "dummy" <> String.duplicate("a1B2", 6)

    for line <- [
          "export DEPLOY_TOKEN=#{"dummy" <> String.duplicate("a1B2", 6)}",
          "SONAR_TOKEN: \"#{"dummy" <> String.duplicate("a1B2", 6)}\"",
          "CODECOV_TOKEN='#{"dummy" <> String.duplicate("a1B2", 6)}'"
        ] do
      test "literal value is reported: #{String.slice(line, 0, 20)}" do
        assert "Token assignment" in SecurityErrors.detect_secrets(unquote(line))
      end
    end

    test "references and placeholders are not tokens" do
      for line <- [
            "GH_TOKEN: ${{ secrets.HYPATIA_DISPATCH_PAT }}",
            "GITHUB_TOKEN=$GITHUB_TOKEN",
            "NPM_TOKEN=\"${NPM_TOKEN}\"",
            "API_TOKEN=<your-token-here-from-the-dashboard>",
            "FALLBACK_TOKEN=SOME_OTHER_ENVIRONMENT_VARIABLE_NAME",
            "SHORT_TOKEN=#{String.slice(@value, 0, 10)}"
          ] do
        refute "Token assignment" in SecurityErrors.detect_secrets(line), line
      end
    end

    test "dotted references are not literal tokens" do
      for line <- [
            "const DEPLOY_TOKEN = process.env.DEPLOY_TOKEN_V2_FOR_PROD",
            "api_TOKEN = 1; NPM_TOKEN = var.npm_token_value_2026_prod",
            "SONAR_TOKEN: secrets.SONAR_TOKEN_FOR_ESTATE_2026",
            "GH_TOKEN = os.environ.get_token_value_2026_x"
          ] do
        refute "Token assignment" in SecurityErrors.detect_secrets(line), line
      end
    end

    test "a quoted key (YAML or JSON) is still an assignment" do
      line = ~s("SONAR_TOKEN": "#{@value}")
      assert "Token assignment" in SecurityErrors.detect_secrets(line)
    end

    test "a value may start with base64 punctuation" do
      for lead <- ["/", "+", "-", "_"] do
        line = "DEPLOY_TOKEN=" <> lead <> @value
        assert "Token assignment" in SecurityErrors.detect_secrets(line), lead
      end
    end

    test "a specific credential match is not reported a second time as a token assignment" do
      pat = "ghp_" <> String.duplicate("a1B2c3D4e5", 3) <> "a1B2c3"
      labels = SecurityErrors.detect_secrets("GITHUB_TOKEN=" <> pat)
      assert "GitHub PAT" in labels
      refute "Token assignment" in labels
    end

    test "a PAT on one line does not hide a distinct literal token on another" do
      pat = "ghp_" <> String.duplicate("a1B2c3D4e5", 3) <> "a1B2c3"
      file = "GITHUB_TOKEN=" <> pat <> "\nDEPLOY_TOKEN=" <> @value <> "\n"
      labels = SecurityErrors.detect_secrets(file)
      assert "GitHub PAT" in labels
      assert "Token assignment" in labels
    end

    test "a commented-out example is documentation, not a finding" do
      line = "# DEPLOY_TOKEN=" <> @value
      assert Hypatia.ScannerSuppression.comment_masked_secret_label?("Token assignment", line, 5)
    end

    test "a lowercase variable is not in scope (shell convention for secrets is UPPER)" do
      refute "Token assignment" in SecurityErrors.detect_secrets("my_token=#{@value}")
    end
  end
end
