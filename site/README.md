# Hypatia Project Site

Built with [casket-ssg](https://github.com/hyperpolymath/polystack/tree/main/poly-ssg/casket-ssg) — a pure functional Haskell static site generator.

## Structure

```
site/
  content/     # Markdown pages with YAML frontmatter
    index.md   # Landing page
  templates/   # Mustache-style HTML templates
    default.html
  assets/      # Static assets (CSS, images)
    style.css
```

## Build

```bash
casket-ssg build site/content site/_output
```

## Deploy

The site is deployed to GitHub Pages via the `jekyll-gh-pages.yml` workflow
(casket-ssg output is static HTML, compatible with any static hosting).
