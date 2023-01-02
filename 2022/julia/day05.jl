function rearrange(stacks, procedure, transformer)
    for p in procedure
        k, from, to = p
        moved = transformer(stacks[from][end-k+1:end])
        stacks[from] = stacks[from][1:end-k]
        append!(stacks[to], moved)
    end

    join(map(last, stacks))
end

function day05()
    lines = readlines()

    m = findfirst(==(""), lines)
    n = length(lines[1])

    stacks::Vector{Vector{Char}} = fill([], (n + 1) ÷ 4)
    for i in m-2:-1:1
        line = lines[i]
        for j in 2:4:n
            if isuppercase(line[j])
                push!(stacks[j÷4+1], line[j])
            end
        end
    end

    procs::Vector{NTuple{3,Int}} = []
    for i in m+1:length(lines)
        words = split(lines[i])
        k, from, to = parse.(Int, (words[2], words[4], words[6]))
        push!(procs, (k, from, to))
    end

    println(rearrange(deepcopy(stacks), procs, reverse))
    println(rearrange(deepcopy(stacks), procs, identity))
end

@time day05()
