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
    fs = Dict("/" => Dict())
    for line in lines
        if line[1] == '$'
            if line[3] == 'c' # cd
                dir = line[6:end]
                if dir == ".."
                    pop!(pwd)
                elseif dir == "/"
                    pwd = ["/"]
                else
                    push!(pwd, dir)
                end
            end
        elseif line[1] == 'd' # dir
            insert(fs, 1, pwd, line[5:end], Dict())
        else
            ws = split(line)
            size = parse(Int, ws[1])
            name = ws[2]
            insert(fs, 1, pwd, name, size)
        end
    end
    fs
end

function insert(fs, i, pwd, name, val)
    if i == length(pwd) + 1
        fs[name] = val
    else
        insert(fs[pwd[i]], i + 1, pwd, name, val)
    end
end

for_each_dir_size(fs::Int, fn) = fs

function for_each_dir_size(fs, fn)
    size = 0
    for p in fs
        val = p[2]
        size += for_each_dir_size(val, fn)
    end
    fn(size)
    size
end

@time day07()
