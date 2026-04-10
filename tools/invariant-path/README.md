# Invariant Path Integration (Hypatia)

Hypatia wrapper script:

```bash
./scripts/invariant-path.sh scan --file ./README.adoc --artifact-uri repo://README.adoc --write
```

Via `just` from repo root:

```bash
just invariant-path scan --file ./README.adoc --artifact-uri repo://README.adoc --write
```

Default profile: `hypatia`

Focus:
- data -> policy
- probability -> certainty
- benchmark -> capability

Store path defaults to `.invariant-path/` in the current working directory.

Desktop/start-menu launcher for shared tooling:
- `/var/mnt/eclipse/repos/.desktop-tools/invariant-path-launcher.sh`
