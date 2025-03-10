# Created by newuser for 5.9
# File operation
alias ll="ls -la"  # 列出所有文件和目录（包括隐藏文件）
alias la="ls -A"   # 列出所有文件和目录（不包括 `.` 和 `..`）
alias l="ls -CF"   # 列出文件和目录，以列格式显示
alias rm="rm -i"   # 删除前提示确认
alias cp="cp -i"   # 复制前提示确认
alias mv="mv -i"   # 移动前提示确认

# File operation
alias ..="cd .."           # 返回上一级目录
alias ...="cd ../.."       # 返回上两级目录
alias ....="cd ../../.."   # 返回上三级目录
alias ~="cd ~"             # 返回家目录
alias dotfiles="cd ~/.dotfiles"  # 快速进入 dotfiles 目录

# Git
alias gs="git status"      # 查看 Git 状态
alias ga="git add"         # 添加文件到暂存区
alias gc="git commit -m"   # 提交更改
alias gp="git push"        # 推送更改
alias gl="git log --oneline"  # 查看简洁的提交日志

# Others
alias cls="clear"          # 清屏
alias h="history"          # 查看命令历史
alias grep="grep --color=auto"  # 高亮显示 grep 结果
alias mkdir="mkdir -p"     # 创建目录时自动创建父目录


# zinit install
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

# zinit plugins install
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-completions

# zinit install omz theme
# 加载 Oh My Zsh
#zinit snippet OMZ::lib/git.zsh  # 加载 Oh My Zsh 的 Git 库
#zinit snippet OMZ::plugins/git/git.plugin.zsh  # 加载 Git 插件
#zinit snippet OMZ::themes/robbyrussell.zsh-theme  # 加载 robbyrussell 主题
#ZSH_THEME="robbyrussell"
