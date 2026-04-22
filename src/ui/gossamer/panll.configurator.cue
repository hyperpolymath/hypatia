// SPDX-License-Identifier: PMPL-1.0-or-later
// PanLL Configurator input — Hypatia runtime config.
//
// Conforms to the #Config schema in hyperpolymath/panll
// schemas/config.cue. The constraint is inlined below so this file
// type-checks standalone; when panll exposes the schema as an
// importable package, switch to `import "hyperpolymath.panll/config"`
// and drop the local copy.

package hypatia

#Config: {
	name:     string
	replicas: int & >=1
	port:     int & >=1 & <=65535
	env:      string
}

config: #Config & {
	name:     "hypatia"
	replicas: 1
	port:     9090
	env:      "production"
}
