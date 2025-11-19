
param(
    [string]$pane,
    [string]$cmd,
    [string]$zoom
)

if ($zoom -eq "zoom") {
    wezterm cli zoom-pane
}

# 使用 yazi chooser
$paths = yazi --chooser-file=CON | ForEach-Object {
    "`"$_`""
} |  -join " "

if ($paths -ne "") {
    $text = ":$cmd $paths`r"
    wezterm cli send-text --no-paste --pane-id $pane -- $text
}
