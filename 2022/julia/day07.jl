using Match

struct FS
    size_or_subdirs::Dict{String,Union{Int,FS}}
end

function day07()
    fs = read_fs()

    ans1 = 0
    for_each_dir_size(fs, dir_size ->
        if dir_size <= 100000
            ans1 += dir_size
        end)
    println(ans1)

    root_size = for_each_dir_size(fs, identity)
    at_least_free = 30_000_000 - (70_000_000 - root_size)
    ans2 = root_size
    for_each_dir_size(fs, dir_size ->
        if dir_size >= at_least_free
            ans2 = min(ans2, dir_size)
        end)
    println(ans2)
end

function read_fs()
    lines = readlines()

    pwd::Vector{String} = ["/"]
    fs::FS = FS(Dict("/" => FS(Dict())))
    for line in lines
        @match split(line) begin
            ["\$", "cd", "/"] => (pwd = ["/"])
            ["\$", "cd", ".."] => pop!(pwd)
            ["\$", "cd", dir] => push!(pwd, dir)
            ["\$", "ls"] => ()
            ["dir", dir] => insert(fs, 1, pwd, dir, FS(Dict()))
            [size, file] => insert(fs, 1, pwd, file, parse(Int, size))
        end
    end
    fs
end

function insert(fs, i, pwd, name, val)
    if i == length(pwd) + 1
        fs.size_or_subdirs[name] = val
    else
        insert(fs.size_or_subdirs[pwd[i]], i + 1, pwd, name, val)
    end
end

function for_each_dir_size(size::Int, fn)
    size
end

function for_each_dir_size(fs::FS, fn)
    size = 0
    for p in fs.size_or_subdirs
        val = p[2]
        size += for_each_dir_size(val, fn)
    end
    fn(size)
    size
end

@time day07()
