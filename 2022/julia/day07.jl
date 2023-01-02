using Match

function read_fs()
    lines = readlines()

    pwd::Vector{String} = ["/"]
    dirs::Dict{Vector{String},Int} = Dict()
    for line in lines
        @match split(line) begin
            ["\$", "cd", "/"] => (pwd = ["/"])
            ["\$", "cd", ".."] => pop!(pwd)
            ["\$", "cd", dir] => push!(pwd, dir)
            ["\$", "ls"] => ()
            ["dir", dir] => ()
            [size, file] => begin
                for i in 1:length(pwd)
                    prefix = pwd[1:i]
                    get!(dirs, prefix, 0)
                    dirs[prefix] += parse(Int, size)
                end
            end
        end
    end
    dirs
end

function day07()
    dirs = read_fs()

    dir_sizes::Vector{Int} = collect(values(dirs))

    println(sum(filter(s -> s <= 100_000, dir_sizes)))

    root = maximum(dir_sizes)
    at_least_free = 30_000_000 - (70_000_000 - root)
    println(minimum(filter(s -> s >= at_least_free, dir_sizes)))
end

@time day07()

