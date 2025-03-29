# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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

zinit ice depth=1; zinit light romkatv/powerlevel10k

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/mzn/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/mzn/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/mzn/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/mzn/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

