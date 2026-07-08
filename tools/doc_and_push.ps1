$ErrorActionPreference = "Stop"
$logPath = "C:\Users\svdijkman.DESKTOP-4OG10M4\Desktop\AD\doc_push_log.txt"
"=== doc push $(Get-Date -Format o) ===" | Out-File $logPath -Encoding utf8

function Process-Package {
    param([string]$Path, [string]$Name)
    "`n========== $Name ==========" | Add-Content $logPath
    Push-Location $Path
    try {
        if (-not (git config user.email)) {
            git config user.email "svdijkman@users.noreply.github.com"
        }
        if (-not (git config user.name)) {
            git config user.name "Sven C. van Dijkman"
        }
        $p = ($Path -replace '\\', '/')
        & Rscript -e "if (!requireNamespace('roxygen2', quietly=TRUE)) install.packages('roxygen2', repos='https://cloud.r-project.org'); roxygen2::roxygenise('$p')" 2>&1 | Add-Content $logPath
        $manDir = Join-Path $Path "man"
        $manCount = 0
        if (Test-Path $manDir) {
            $manCount = (Get-ChildItem $manDir -File | Measure-Object).Count
        }
        "man count: $manCount" | Add-Content $logPath
        git add -A
        $st = git status --porcelain
        if ($st) {
            git commit -m "Release 0.4.1: update manuals, docs, and vignettes" 2>&1 | Add-Content $logPath
            "commit: $(git rev-parse HEAD)" | Add-Content $logPath
        } else {
            "no commit needed" | Add-Content $logPath
        }
        git push origin main 2>&1 | Add-Content $logPath
        "push exit: $LASTEXITCODE" | Add-Content $logPath
    } finally {
        Pop-Location
    }
}

$root = "C:\Users\svdijkman.DESKTOP-4OG10M4\Desktop\AD"
Process-Package "$root\LibeRtAD" "LibeRtAD"
Process-Package "$root\LibeRation" "LibeRation"
Process-Package "$root\LibeRties" "LibeRties"
"done $(Get-Date -Format o)" | Add-Content $logPath
