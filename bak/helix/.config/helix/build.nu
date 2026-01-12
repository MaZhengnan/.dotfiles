#! /usr/bin/env nu
# CMake 项目管理脚本

def main [action: string = "build", ...rest: string] {
    let project_root = $env.PWD
    let build_dir = "build"
    let exec_name = (detect-executable-name)
    
    match $action {
        "config" | "configure" => {
            configure-project $build_dir $rest
        }
        "build" | "compile" => {
            build-project $build_dir
        }
        "run" => {
            run-project $build_dir $exec_name
        }
        "clean" => {
            clean-project $build_dir
        }
        "rebuild" => {
            clean-project $build_dir
            configure-project $build_dir $rest
            build-project $build_dir
        }
        "all" => {
            configure-project $build_dir $rest
            build-project $build_dir
            run-project $build_dir $exec_name
        }
        "debug" => {
            configure-project $"($build_dir)-debug" ["-DCMAKE_BUILD_TYPE=Debug", ...$rest]
            build-project $"($build_dir)-debug"
        }
        "release" => {
            configure-project $"($build_dir)-release" ["-DCMAKE_BUILD_TYPE=Release", ...$rest]
            build-project $"($build_dir)-release"
        }
        "help" | "--help" | "-h" => {
            print-help
        }
        _ => {
            print $"未知操作: ($action)"
            print-help
        }
    }
}

def detect-executable-name [] {
    let cmake_file = "CMakeLists.txt"
    if ($cmake_file | path exists) {
        let project_line = (open $cmake_file | lines | find --regex 'project\s*\(' | get 0? | default "")
        if ($project_line | str length) > 0 {
            let name = ($project_line | parse --regex 'project\s*\(\s*(\w+)' | get capture0.0? | default "my_program")
            return $name
        }
    }
    "my_program"
}

def configure-project [build_dir: string, extra_args: list] {
    print $"配置项目到 ($build_dir)..."
    
    mkdir $build_dir
    cd $build_dir
    
    let base_args = [
        "..",
        "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
        "-DCMAKE_C_COMPILER=gcc",
        "-DCMAKE_CXX_COMPILER=g++"
    ]
    
    let all_args = ([$base_args, $extra_args] | flatten)
    
    let result = (do -i { cmake ...$all_args } | complete)
    
    if $result.exit_code == 0 {
        print "配置成功!"
        # 创建编译命令符号链接，用于 LSP
        if ("compile_commands.json" | path exists) {
            cd ..
            if ("compile_commands.json" | path exists) {
                rm "compile_commands.json"
            }
            ln -s $"($build_dir)/compile_commands.json" "compile_commands.json"
        }
    } else {
        print "配置失败!"
        print $result.stderr
    }
}

def build-project [build_dir: string] {
    print $"构建项目..."
    
    if not ($build_dir | path exists) {
        print $"构建目录 ($build_dir) 不存在，先执行配置..."
        configure-project $build_dir []
    }
    
    cd $build_dir
    
    let result = (do -i { make -j(nproc) } | complete)
    
    if $result.exit_code == 0 {
        print "构建成功!"
    } else {
        print "构建失败!"
        print $result.stderr
    }
}

def run-project [build_dir: string, exec_name: string] {
    let exec_path = $"($build_dir)/($exec_name)"
    
    if not ($exec_path | path exists) {
        print $"可执行文件 ($exec_path) 不存在，先执行构建..."
        build-project $build_dir
    }
    
    print $"运行程序: ($exec_name)"
    cd $build_dir
    ./$exec_name
}

def clean-project [build_dir: string] {
    print $"清理构建目录: ($build_dir)"
    if ($build_dir | path exists) {
        rm -rf $build_dir
        print "清理完成!"
    } else {
        print "构建目录不存在，无需清理"
    }
    
    # 清理编译命令文件
    if ("compile_commands.json" | path exists) {
        rm "compile_commands.json"
    }
}

def print-help [] {
    print "CMake 项目管理脚本"
    print ""
    print "使用方法: build.nu <操作> [额外参数]"
    print ""
    print "操作:"
    print "  config, configure   配置项目"
    print "  build, compile      构建项目" 
    print "  run                 运行程序"
    print "  clean               清理构建"
    print "  rebuild             重新构建"
    print "  all                 配置+构建+运行"
    print "  debug               调试构建"
    print "  release             发布构建"
    print "  help                显示帮助"
    print ""
    print "示例:"
    print "  build.nu config -G Ninja"
    print "  build.nu debug"
    print "  build.nu all"
    print "  build.nu run"
}
