# CICD-Hyper-A Monitoring Stack

This directory contains the complete monitoring and observability configuration for cicd-hyper-a, including Prometheus, Grafana, Alertmanager, and Node Exporter.

## Components

| Component | Port | Description |
|-----------|------|-------------|
| Prometheus | 9090 | Metrics collection and storage |
| Grafana | 3000 | Visualization and dashboards |
| Alertmanager | 9093 | Alert routing and management |
| Node Exporter | 9100 | Host system metrics |
| cAdvisor | 8088 | Container metrics (optional) |

## Quick Start

### Prerequisites

- Docker and Docker Compose installed
- cicd-hyper-a main stack running (creates the `cicd-network`)

### Start Monitoring Stack

```bash
# From the deploy/monitoring directory
cd /var/mnt/eclipse/repos/cicd-hyper-a/deploy/monitoring

# Start core monitoring services
docker compose -f docker-compose.monitoring.yml up -d

# Start with container metrics (cAdvisor)
docker compose -f docker-compose.monitoring.yml --profile full up -d
```

### Access Services

- **Grafana**: http://localhost:3000 (default: admin/admin)
- **Prometheus**: http://localhost:9090
- **Alertmanager**: http://localhost:9093

## Directory Structure

```
monitoring/
├── prometheus/
│   ├── prometheus.yml      # Main Prometheus configuration
│   ├── alerts.yml          # Alert rules
│   └── recording-rules.yml # Pre-computed metrics
├── grafana/
│   ├── datasources.yml     # Prometheus datasource config
│   ├── dashboards.yml      # Dashboard provisioning config
│   └── dashboards/
│       ├── overview.json   # System overview
│       ├── bots.json       # Bot execution metrics
│       ├── rules.json      # Rule performance
│       ├── findings.json   # Finding trends
│       └── api.json        # API and cache metrics
├── alertmanager/
│   └── alertmanager.yml    # Alert routing config
├── docker-compose.monitoring.yml
└── README.md
```

## Dashboards

### Overview Dashboard
High-level view of all services including:
- Service health status
- Request rates (rules, bots, API)
- Success rates
- Latency percentiles
- Findings summary

### Bot Execution Dashboard
Bot-specific metrics:
- Active bots count
- Execution rate by bot
- Success rate by bot
- Queue length and wait times
- Bot status table

### Rule Performance Dashboard
Rule execution metrics:
- Execution rate by status/type
- Latency percentiles
- Success rate over time
- Failures by rule type

### Finding Trends Dashboard
Finding analysis:
- Findings by severity
- Findings by category
- Processing backlog
- Auto-resolution rate
- Critical/High trends

### API & Cache Dashboard
API and cache performance:
- Request rate by service
- Status code distribution
- Latency percentiles
- Error rates
- Cache hit rate
- Cache memory usage

## Metrics Reference

### Rule Execution Metrics
| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `cicd_rule_execution_total` | Counter | status, rule_type | Total rule executions |
| `cicd_rule_execution_duration_seconds` | Histogram | rule_type | Rule execution duration |
| `cicd_rules_in_progress` | Gauge | - | Currently executing rules |

### Bot Execution Metrics
| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `cicd_bot_execution_total` | Counter | bot_name, status | Total bot executions |
| `cicd_bot_execution_duration_seconds` | Histogram | bot_name | Bot execution duration |
| `cicd_bot_queue_length` | Gauge | - | Pending bot executions |
| `cicd_bot_queue_wait_seconds` | Histogram | - | Queue wait time |
| `cicd_bot_execution_timeout_total` | Counter | bot_name | Bot timeouts |

### Finding Metrics
| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `cicd_findings_total` | Counter | severity, category, resolution | Total findings |
| `cicd_findings_pending_processing` | Gauge | - | Pending findings |
| `cicd_findings_processing_duration_seconds` | Histogram | - | Processing time |

### API Metrics
| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `http_request_total` | Counter | job, handler, method, status_code | HTTP requests |
| `http_request_duration_seconds` | Histogram | job, handler, method | Request duration |
| `http_rate_limit_exceeded_total` | Counter | job | Rate limit hits |

### Cache Metrics
| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `cicd_cache_hits_total` | Counter | - | Cache hits |
| `cicd_cache_misses_total` | Counter | - | Cache misses |
| `cicd_cache_operations_total` | Counter | operation | Cache operations |
| `cicd_cache_operation_duration_seconds` | Histogram | operation | Operation duration |

## Alert Configuration

### Severity Levels

| Severity | Response Time | Notification |
|----------|---------------|--------------|
| critical | Immediate | PagerDuty/SMS |
| warning | 5 minutes | Slack/Email |
| info | 24 hours | Logging only |

### Alert Categories

1. **Service Availability** - Service up/down status
2. **Rule Execution** - Latency, failure rates
3. **Bot Execution** - Success rates, timeouts, queue backlog
4. **Findings** - Severity spikes, processing backlog
5. **API Performance** - Latency, error rates
6. **Cache Performance** - Hit rate, memory usage
7. **Infrastructure** - CPU, memory, disk

### Configuring Notifications

Edit `alertmanager/alertmanager.yml` to configure receivers:

```yaml
receivers:
  - name: 'critical-receiver'
    pagerduty_configs:
      - service_key: 'YOUR_PAGERDUTY_KEY'

    slack_configs:
      - channel: '#cicd-critical-alerts'
        api_url: 'YOUR_SLACK_WEBHOOK'
```

## Recording Rules

Pre-computed metrics for dashboard performance:

| Rule | Expression | Purpose |
|------|------------|---------|
| `cicd:rule_execution_rate_total:5m` | sum(rate(cicd_rule_execution_total[5m])) | Total execution rate |
| `cicd:rule_execution_success_rate:5m` | success/total | Success percentage |
| `cicd:rule_execution_latency_p95:5m` | histogram_quantile(0.95, ...) | 95th percentile latency |
| `cicd:cache_hit_rate:5m` | hits/(hits+misses) | Cache efficiency |

## Extending the Stack

### Adding Custom Dashboards

1. Create a JSON dashboard file in `grafana/dashboards/`
2. Tag with `cicd-hyper-a` for linking
3. Use `prometheus` as datasource UID

### Adding Custom Alerts

1. Add rules to `prometheus/alerts.yml`
2. Reload Prometheus: `curl -X POST http://localhost:9090/-/reload`

### Adding Custom Recording Rules

1. Add rules to `prometheus/recording-rules.yml`
2. Use `cicd:` prefix for naming convention
3. Reload Prometheus

## Troubleshooting

### Prometheus Not Scraping Targets

```bash
# Check target status
curl http://localhost:9090/api/v1/targets

# Verify network connectivity
docker network inspect cicd-hyper-a_cicd-network
```

### Grafana Dashboards Not Loading

```bash
# Check provisioning logs
docker logs cicd-hyper-a-grafana

# Verify dashboard files
docker exec cicd-hyper-a-grafana ls -la /var/lib/grafana/dashboards/
```

### Alertmanager Not Sending Alerts

```bash
# Check alertmanager config
docker exec cicd-hyper-a-alertmanager amtool config show

# Test webhook
curl -X POST http://localhost:9093/api/v1/alerts -d '[{"labels":{"alertname":"test"}}]'
```

## Production Considerations

1. **Security**: Change default Grafana credentials
2. **Persistence**: Mount volumes to persistent storage
3. **Retention**: Adjust Prometheus retention based on storage
4. **High Availability**: Consider Prometheus federation or Thanos
5. **Backup**: Regular backup of Grafana dashboards and Prometheus data
6. **Network**: Restrict access to monitoring endpoints

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `GRAFANA_USER` | admin | Grafana admin username |
| `GRAFANA_PASSWORD` | admin | Grafana admin password |
| `GRAFANA_ROOT_URL` | http://localhost:3000 | External Grafana URL |
| `GRAFANA_ANONYMOUS_ENABLED` | false | Allow anonymous access |

## License

SPDX-License-Identifier: PMPL-1.0-or-later
