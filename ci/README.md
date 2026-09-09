# CI — pendiente de habilitar

`check.yaml` define el _quality gate_ para GitHub Actions
(`Rscript scripts/check.R` en cada push y PR a `main`).

No se subió en `.github/workflows/` en el commit inicial porque el token de
`gh` no tenía el scope `workflow`. Para activarlo:

```bash
gh auth refresh -h github.com -s workflow
mkdir -p .github/workflows
git mv ci/check.yaml .github/workflows/check.yaml
git commit -m "ci: add quality gate workflow"
git push
```
