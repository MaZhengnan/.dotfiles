#!/bin/sh

# 最简单的脚本，只做最基本的检测

# 检查 fcitx5 是否在运行
if ! pgrep fcitx5 >/dev/null; then
    echo ""
    exit 0
fi

# 直接检测，不使用复杂逻辑
if fcitx5-remote >/dev/null 2>&1; then
    if [ "$(fcitx5-remote)" = "2" ]; then
        # 检查是否是 rime
        CURRENT=$(fcitx5-remote -n 2>/dev/null)
        case "$CURRENT" in
            *[Rr]ime*|*rime*|*中州*)
                echo "中"
                ;;
            *)
                echo "EN"
                ;;
        esac
    else
        echo ""
    fi
else
    echo ""
fi
