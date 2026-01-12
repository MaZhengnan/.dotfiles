if status is-interactive
    # Commands to run in interactive sessions can go here
end

# starship
starship init fish | source

# make Alt + l to accept current suggestion like using right arrow
bind \el forward-char

# Aliases
if [ -f $HOME/.config/fish/alias.fish ]
    source $HOME/.config/fish/alias.fish
end
if command -v fish_add_path >/dev/null
    fish_add_path ~/.local/bin
else
    # 兼容旧版本
    set -gx PATH ~/.local/bin $PATH
end
function fish_greeting
    random choice "Hi MZN!"
end

# Android Studio
set -x ANDROID_HOME $HOME/Android/Sdk
set -x PATH $PATH $ANDROID_HOME/emulator
set -x PATH $PATH $ANDROID_HOME/platform-tools

# do not use slow default handle when not the command we typed not found
function fish_command_not_found
    __fish_default_command_not_found_handler $argv
end
# ~/.config/fish/config.fish
function emacs
    # 检查是否使用 "emacs=配置名" 格式
    if string match -q '*=*' $argv[1]
        # 提取配置名
        set profile_name (string split = -- $argv[1])[2]
        set -e argv[1]
        
        # 检查配置文件是否存在
        if test -f "$HOME/.emacs-profiles.el"
            command emacs --with-profile="$profile_name" $argv
        else
            echo "错误: Chemacs2配置文件未找到 ~/.emacs-profiles.el"
            return 1
        end
    else
        command emacs $argv
    end
end

# 快捷别名
alias emacs-doom='emacs --with-profile=doom'
alias emacs-space='emacs --with-profile=spacemacs'
alias emacs='emacs --with-profile=default'
alias emacs-efs='emacs --with-profile=efs'
