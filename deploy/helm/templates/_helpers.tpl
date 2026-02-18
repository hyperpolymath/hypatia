{{/*
SPDX-License-Identifier: PMPL-1.0-or-later
Helm template helpers for cicd-hyper-a
*/}}

{{/*
Expand the name of the chart.
*/}}
{{- define "cicd-hyper-a.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
*/}}
{{- define "cicd-hyper-a.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "cicd-hyper-a.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "cicd-hyper-a.labels" -}}
helm.sh/chart: {{ include "cicd-hyper-a.chart" . }}
{{ include "cicd-hyper-a.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
app.kubernetes.io/part-of: cicd-hyper-a
{{- end }}

{{/*
Selector labels
*/}}
{{- define "cicd-hyper-a.selectorLabels" -}}
app.kubernetes.io/name: {{ include "cicd-hyper-a.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "cicd-hyper-a.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "cicd-hyper-a.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{/*
API component labels
*/}}
{{- define "cicd-hyper-a.api.labels" -}}
{{ include "cicd-hyper-a.labels" . }}
app.kubernetes.io/component: api
{{- end }}

{{- define "cicd-hyper-a.api.selectorLabels" -}}
{{ include "cicd-hyper-a.selectorLabels" . }}
app.kubernetes.io/component: api
{{- end }}

{{/*
Registry component labels
*/}}
{{- define "cicd-hyper-a.registry.labels" -}}
{{ include "cicd-hyper-a.labels" . }}
app.kubernetes.io/component: registry
{{- end }}

{{- define "cicd-hyper-a.registry.selectorLabels" -}}
{{ include "cicd-hyper-a.selectorLabels" . }}
app.kubernetes.io/component: registry
{{- end }}

{{/*
Engine component labels
*/}}
{{- define "cicd-hyper-a.engine.labels" -}}
{{ include "cicd-hyper-a.labels" . }}
app.kubernetes.io/component: engine
{{- end }}

{{- define "cicd-hyper-a.engine.selectorLabels" -}}
{{ include "cicd-hyper-a.selectorLabels" . }}
app.kubernetes.io/component: engine
{{- end }}

{{/*
ArangoDB component labels
*/}}
{{- define "cicd-hyper-a.arangodb.labels" -}}
{{ include "cicd-hyper-a.labels" . }}
app.kubernetes.io/component: database
{{- end }}

{{- define "cicd-hyper-a.arangodb.selectorLabels" -}}
{{ include "cicd-hyper-a.selectorLabels" . }}
app.kubernetes.io/component: database
{{- end }}

{{/*
Dragonfly component labels
*/}}
{{- define "cicd-hyper-a.dragonfly.labels" -}}
{{ include "cicd-hyper-a.labels" . }}
app.kubernetes.io/component: cache
{{- end }}

{{- define "cicd-hyper-a.dragonfly.selectorLabels" -}}
{{ include "cicd-hyper-a.selectorLabels" . }}
app.kubernetes.io/component: cache
{{- end }}

{{/*
Get the secrets name
*/}}
{{- define "cicd-hyper-a.secretsName" -}}
{{- if .Values.secrets.existingSecret }}
{{- .Values.secrets.existingSecret }}
{{- else }}
{{- include "cicd-hyper-a.fullname" . }}-secrets
{{- end }}
{{- end }}

{{/*
ArangoDB URL
*/}}
{{- define "cicd-hyper-a.arangodbUrl" -}}
{{- if .Values.arangodb.external }}
{{- .Values.arangodb.externalUrl }}
{{- else }}
http://{{ include "cicd-hyper-a.fullname" . }}-arangodb-headless:8529
{{- end }}
{{- end }}

{{/*
Dragonfly URL
*/}}
{{- define "cicd-hyper-a.dragonflyUrl" -}}
{{- if .Values.dragonfly.external }}
{{- .Values.dragonfly.externalUrl }}
{{- else }}
redis://{{ include "cicd-hyper-a.fullname" . }}-dragonfly:6379
{{- end }}
{{- end }}

{{/*
Image tag default
*/}}
{{- define "cicd-hyper-a.imageTag" -}}
{{- .Values.api.image.tag | default .Chart.AppVersion }}
{{- end }}

{{/*
Namespace
*/}}
{{- define "cicd-hyper-a.namespace" -}}
{{- default .Release.Namespace .Values.namespace.name }}
{{- end }}
