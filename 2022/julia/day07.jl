using Match

struct FS
    name2data::Dict{String,Union{Int,FS}}
end

function day07()
    fs = read_fs()

    dir_sizes::Vector{Int} = []
    for_each_dir_size(fs, s -> push!(dir_sizes, s))

    println(sum(filter(s -> s <= 100_000, dir_sizes)))

    root = maximum(dir_sizes)
    at_least_free = 30_000_000 - (70_000_000 - root)
    println(minimum(filter(s -> s >= at_least_free, dir_sizes)))
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
        fs.name2data[name] = val
    else
        insert(fs.name2data[pwd[i]], i + 1, pwd, name, val)
    end
end

function for_each_dir_size(size::Int, fn)
    size
end

function for_each_dir_size(fs::FS, fn)
    size = 0
    for p in fs.name2data
        val = p[2]
        size += for_each_dir_size(val, fn)
    end
    fn(size)
    size
end

@time day07()
